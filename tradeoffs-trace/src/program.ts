// Phase 4: the program scheduler's effects. The decisions are in
// core/program.ts; this file only observes the runs it launched and starts
// the nodes `nextStarts` names, each as an ordinary single-phase run (the
// same conductor, review loop and owner rules as a run started by hand).
//
// Layout (under <root>/programs/<id>/):
//   program.json    the program as started (never edited)
//   events.jsonl    NODE_STARTED / NODE_STATUS / PROGRAM_STOPPED, one per line;
//                   the program state is always rebuilt by folding it
//   scheduler.pid   the running scheduler, if any
//   scheduler.log   its stdout/stderr

import * as fs from "node:fs";
import * as path from "node:path";
import { randomUUID } from "node:crypto";

import { createRun, rebuildState, runPaths } from "./conductor.ts";
import { execFileSync } from "node:child_process";

import {
  expandProgram,
  nodeBases,
  nodeBranch,
  initialProgramState,
  nextProgramDirectiveId,
  nextStarts,
  nodePlan,
  programDirectivesInForce,
  programOutcome,
  reduceProgram,
  type NodeStatus,
  type ProgramDirective,
  type ProgramEvent,
  type ProgramFile,
  type ProgramNode,
  type ProgramOutcome,
  type ProgramState,
} from "./core/program.ts";

export function programsRoot(root: string): string {
  return path.join(root, "programs");
}

export function programPaths(dir: string) {
  return {
    program: path.join(dir, "program.json"),
    events: path.join(dir, "events.jsonl"),
    pid: path.join(dir, "scheduler.pid"),
    log: path.join(dir, "scheduler.log"),
    /** Plan 01i: the program's own owner-input inbox (a `C-u` directive from
     * a node run is forwarded here; the Emacs program buffer writes here). */
    inbox: path.join(dir, "inbox"),
    inboxApplied: path.join(dir, "inbox", "applied"),
  };
}

/** Validates the program (throws on a bad graph) and creates its directory. */
export function createProgram(root: string, program: ProgramFile, id = randomUUID().slice(0, 8)): string {
  expandProgram(program);
  const dir = path.join(programsRoot(root), id);
  fs.mkdirSync(dir, { recursive: true });
  fs.mkdirSync(programPaths(dir).inbox, { recursive: true });
  fs.mkdirSync(programPaths(dir).inboxApplied, { recursive: true });
  fs.writeFileSync(programPaths(dir).program, JSON.stringify(program, null, 2));
  fs.writeFileSync(programPaths(dir).events, "");
  return dir;
}

export function readProgram(dir: string): ProgramFile {
  return JSON.parse(fs.readFileSync(programPaths(dir).program, "utf8")) as ProgramFile;
}

export function foldProgram(dir: string): { program: ProgramFile; nodes: ProgramNode[]; state: ProgramState } {
  const program = readProgram(dir);
  const nodes = expandProgram(program);
  let state = initialProgramState(nodes);
  const text = fs.existsSync(programPaths(dir).events) ? fs.readFileSync(programPaths(dir).events, "utf8") : "";
  for (const line of text.split("\n")) {
    if (!line.trim()) continue;
    try {
      state = reduceProgram(state, (JSON.parse(line) as { event: ProgramEvent }).event);
    } catch {
      // a torn last line from a crash; the next append rewrites nothing
    }
  }
  return { program, nodes, state };
}

export function appendProgramEvent(dir: string, event: ProgramEvent): void {
  fs.appendFileSync(programPaths(dir).events, `${JSON.stringify({ ts: new Date().toISOString(), event })}\n`);
}

// -- plan 01i: program-wide owner directives (D5) --------------------------

/** Plan 01i: reads the program's own inbox and records each directive (or
 * withdrawal) as a program event, then moves the file to applied/. A node's
 * `C-u` directive is forwarded here by its conductor; the Emacs program
 * buffer writes here directly. */
export function scanProgramInbox(dir: string): void {
  const p = programPaths(dir);
  fs.mkdirSync(p.inbox, { recursive: true });
  fs.mkdirSync(p.inboxApplied, { recursive: true });
  let names: string[];
  try {
    names = fs.readdirSync(p.inbox);
  } catch {
    return;
  }
  for (const name of names.sort()) {
    if (!name.endsWith(".json")) continue;
    const file = path.join(p.inbox, name);
    let raw: unknown;
    try {
      raw = JSON.parse(fs.readFileSync(file, "utf8"));
    } catch {
      // A file still being written, or malformed: leave it for the next scan.
      continue;
    }
    const r = (raw && typeof raw === "object" ? raw : {}) as Record<string, unknown>;
    const kind = typeof r.type === "string" ? r.type : typeof r.kind === "string" ? r.kind : undefined;
    const text = typeof r.text === "string" ? r.text : "";
    const { state } = foldProgram(dir);
    // A node that forwarded the same command twice (a crash between the
    // forward and its own file move) must not create a second directive.
    const key = typeof r.key === "string" ? r.key : undefined;
    if (key && (state.directives ?? []).some((d) => d.key === key)) {
      // already recorded
    } else if (kind === "withdraw") {
      const id = typeof r.directiveId === "string" ? r.directiveId : undefined;
      if (id) appendProgramEvent(dir, { type: "DIRECTIVE_WITHDRAWN", directiveId: id });
    } else if (text.trim().length > 0) {
      const directive: ProgramDirective = {
        id: nextProgramDirectiveId(state),
        text: text.trim(),
        at: new Date().toISOString(),
        ...(typeof r.origin === "string" ? { origin: r.origin } : {}),
        ...(key ? { key } : {}),
      };
      appendProgramEvent(dir, { type: "DIRECTIVE_ADDED", directive });
    }
    try {
      fs.renameSync(file, path.join(p.inboxApplied, name));
    } catch {
      // already moved
    }
  }
}

/** Plan 01i: the directive ids a node run was started with (its plan
 * snapshot's `ownerDirectives`), so the scheduler does not push the same
 * directive into a run that already carries it. */
function seededDirectiveIds(runDir: string): Set<string> {
  try {
    const plan = JSON.parse(fs.readFileSync(path.join(runPaths(runDir).plan, "v1.json"), "utf8")) as {
      ownerDirectives?: Array<{ id?: string }>;
    };
    return new Set((plan.ownerDirectives ?? []).map((d) => d.id ?? "").filter((id) => id.length > 0));
  } catch {
    return new Set();
  }
}

function writeNodeInbox(runDir: string, name: string, command: unknown): void {
  const inbox = runPaths(runDir).inbox;
  try {
    fs.mkdirSync(inbox, { recursive: true });
  } catch {
    return;
  }
  const file = path.join(inbox, name);
  const tmp = `${file}.tmp`;
  try {
    fs.writeFileSync(tmp, JSON.stringify(command));
    fs.renameSync(tmp, file);
  } catch {
    // best effort: the node may be gone; the next tick retries.
  }
}

/** Plan 01i: pushes every in-force program-wide directive into each active
 * node's inbox (deterministic file name, so a re-push is applied at most
 * once by the node's own command-id dedup), and the withdrawal notice for
 * each withdrawn one. Future nodes get the directives through `nodePlan`. */
export function deliverProgramDirectives(runRoot: string, state: ProgramState): void {
  const inForce = programDirectivesInForce(state);
  const withdrawn = (state.directives ?? []).filter((d) => d.withdrawn);
  if (inForce.length === 0 && withdrawn.length === 0) return;
  for (const [, s] of Object.entries(state.nodes)) {
    if (!s.runId) continue;
    if (!["running", "needs-you", "stopped"].includes(s.status)) continue;
    const runDir = path.join(runRoot, s.runId);
    const seeded = seededDirectiveIds(runDir);
    for (const d of inForce) {
      if (seeded.has(d.id)) continue;
      // Pushed to every active node, the origin run included: the origin only
      // *forwarded* the ruling, so this push is what gives it the program's
      // own `ODP-n` record (no node ever mints or renumbers a program id).
      writeNodeInbox(runDir, `cmd-prog-${d.id}.json`, {
        type: "directive",
        text: d.text,
        scope: "program",
        pushed: true,
        programId: d.id,
      });
    }
    for (const d of withdrawn) {
      // Every active node is told, the origin node included. A node that
      // never had the directive treats an unknown-id withdrawal as a no-op —
      // never a refusal, or the same file would be rejected again on every
      // tick.
      writeNodeInbox(runDir, `cmd-prog-withdraw-${d.id}.json`, {
        type: "withdraw-directive",
        directiveId: d.id,
        text: `withdraw ${d.id}`,
        pushed: true,
      });
    }
  }
}

/** Plan 01i: record one program-wide directive (a `C-u` input or the program
 * buffer), deliver it to every running node now and seed every later node. */
export function addProgramDirective(root: string, dir: string, text: string, origin?: string): ProgramDirective {
  const { state } = foldProgram(dir);
  const directive: ProgramDirective = {
    id: nextProgramDirectiveId(state),
    text,
    at: new Date().toISOString(),
    ...(origin ? { origin } : {}),
  };
  appendProgramEvent(dir, { type: "DIRECTIVE_ADDED", directive });
  deliverProgramDirectives(root, foldProgram(dir).state);
  return directive;
}

/** Plan 01i: withdraw one program-wide directive: every running node's inbox
 * is told it no longer applies, and every node started later is started
 * without it. */
export function withdrawProgramDirective(root: string, dir: string, directiveId: string): boolean {
  const { state } = foldProgram(dir);
  const directive = (state.directives ?? []).find((d) => d.id === directiveId);
  if (!directive || directive.withdrawn) return false;
  appendProgramEvent(dir, { type: "DIRECTIVE_WITHDRAWN", directiveId });
  deliverProgramDirectives(root, foldProgram(dir).state);
  return true;
}

function pidAlive(file: string): boolean {
  try {
    const pid = Number(fs.readFileSync(file, "utf8"));
    if (!(pid > 0)) return false;
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

/** A launched node's status, observed from its run directory. "crashed":
 * the conductor died without a clean stop (no `stopped` marker). */
export function observeRun(runDir: string): Exclude<NodeStatus, "waiting"> | "crashed" {
  let phase: string | undefined;
  try {
    const plan = JSON.parse(fs.readFileSync(path.join(runPaths(runDir).plan, "v1.json"), "utf8"));
    phase = rebuildState(runDir, plan, { lenient: true }).phase.phase;
  } catch {
    phase = undefined;
  }
  if (phase === "DONE") return "done";
  if (phase === "BLOCKED") return "blocked";
  const alive = pidAlive(path.join(runDir, "conductor.pid"));
  if (phase === "AWAITING_OWNER") return "needs-you";
  // A run just launched may not have written its pid yet.
  if (!alive && fs.existsSync(path.join(runDir, "conductor.pid"))) {
    return fs.existsSync(path.join(runDir, "stopped")) ? "stopped" : "crashed";
  }
  return "running";
}

/** How often the scheduler restarts a node whose conductor crashed. */
export const MAX_CRASH_RESUMES = 3;

export interface SchedulerOptions {
  /** Where node runs are created (the usual run root). */
  runRoot: string;
  /** Launches a run's conductor (detached); the CLI passes its launcher. */
  launch: (runDir: string) => void;
  pollMs?: number;
  /** Test hook: stop the loop after this many ticks. */
  maxTicks?: number;
}

/** One scheduler step: observe active nodes, start ready ones. Returns the
 * program outcome after the step. */
export function schedulerTick(dir: string, opts: SchedulerOptions): ProgramOutcome {
  // Plan 01i: owner directives the program buffer (or a node's `C-u` input)
  // left in the program's inbox become logged program events first, so they
  // are part of the program state this tick observes and delivers.
  scanProgramInbox(dir);
  let { program, nodes, state } = foldProgram(dir);
  deliverProgramDirectives(opts.runRoot, state);
  const record = (event: ProgramEvent) => {
    appendProgramEvent(dir, event);
    state = reduceProgram(state, event);
  };
  for (const n of nodes) {
    const s = state.nodes[n.id];
    if (!s.runId || s.status === "done" || s.status === "blocked") continue;
    const runDir = path.join(opts.runRoot, s.runId);
    const seen = observeRun(runDir);
    if (seen === "crashed") {
      // The run recovers from its own control log (tt resume); an owner
      // stop leaves a marker and is not restarted here.
      if (!state.stopped && (s.resumes ?? 0) < MAX_CRASH_RESUMES) {
        record({ type: "NODE_RESUMED", node: n.id, reason: "crashed" });
        opts.launch(runDir);
      } else if (s.status !== "stopped") {
        record({ type: "NODE_STATUS", node: n.id, status: "stopped" });
      }
      continue;
    }
    if (seen !== s.status) record({ type: "NODE_STATUS", node: n.id, status: seen });
  }
  for (const id of nextStarts(nodes, state, program.maxParallel)) {
    const node = nodes.find((n) => n.id === id)!;
    const plan = nodePlan(program, node, programDirectivesInForce(state));
    const prepared = prepareBranch(plan.repo, plan.integrationBranch, nodeBases(program, nodes, node), id);
    if (!prepared.ok) {
      record({ type: "NODE_BLOCKED", node: id, reason: prepared.reason });
      continue;
    }
    let runDir: string;
    try {
      runDir = createRun(opts.runRoot, plan);
    } catch (err) {
      // e.g. a shallow clone: the node cannot start, and says why.
      record({ type: "NODE_BLOCKED", node: id, reason: String((err as Error).message ?? err) });
      continue;
    }
    fs.writeFileSync(
      path.join(runDir, "program.json"),
      JSON.stringify({ programId: path.basename(dir), node: id }),
    );
    record({
      type: "NODE_STARTED",
      node: id,
      runId: path.basename(runDir),
      branch: plan.integrationBranch,
      base: nodeBases(program, nodes, node).join(" + "),
    });
    opts.launch(runDir);
  }
  return programOutcome(nodes, state);
}

function git(repo: string, args: string[]): string {
  return execFileSync("git", ["-C", repo, ...args], { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();
}

/** Makes `branch` exist before the node's run starts (the run branches its
 * worktree from it and publishes onto it). An existing branch is kept (a
 * resumed program must not reset published work). One base: the branch
 * starts at it. Several (a join after parallel nodes): a merge commit of all
 * of them; a conflict blocks the node with the files named. */
export function prepareBranch(
  repo: string,
  branch: string,
  bases: string[],
  node: string,
): { ok: true } | { ok: false; reason: string } {
  try {
    git(repo, ["rev-parse", "--verify", "--quiet", `refs/heads/${branch}`]);
    return { ok: true };
  } catch {
    // not there yet
  }
  try {
    const shas = bases.map((b) => git(repo, ["rev-parse", "--verify", `${b}^{commit}`]));
    let head = shas[0];
    for (let i = 1; i < shas.length; i++) {
      let contained = false;
      try {
        git(repo, ["merge-base", "--is-ancestor", shas[i], head]);
        contained = true;
      } catch {
        contained = false;
      }
      if (contained) continue;
      let tree: string;
      try {
        tree = git(repo, ["merge-tree", "--write-tree", head, shas[i]]).split("\n")[0];
      } catch (err) {
        const out = String((err as { stdout?: string }).stdout ?? "");
        const files = out.split("\n").slice(1).filter((l) => l.trim()).join(", ");
        return { ok: false, reason: `merging ${bases[0]} with ${bases[i]} for ${node} conflicts${files ? `: ${files}` : ""}` };
      }
      head = git(repo, [
        "-c", "user.name=tradeoffs-trace", "-c", "user.email=tradeoffs-trace@local",
        "commit-tree", tree, "-p", head, "-p", shas[i], "-m", `tradeoffs-trace program: merge ${bases[i]} for ${node}`,
      ]);
    }
    git(repo, ["branch", branch, head]);
    return { ok: true };
  } catch (err) {
    return { ok: false, reason: `could not create ${branch} from ${bases.join(" + ")}: ${String((err as Error).message).split("\n")[0]}` };
  }
}

/** The scheduler process: tick until the program is done, stuck or stopped. */
export async function runScheduler(dir: string, opts: SchedulerOptions): Promise<ProgramOutcome> {
  fs.writeFileSync(programPaths(dir).pid, String(process.pid));
  let ticks = 0;
  for (;;) {
    const outcome = schedulerTick(dir, opts);
    ticks += 1;
    if (outcome !== "running") {
      fs.appendFileSync(programPaths(dir).log, `${new Date().toISOString()} program ${outcome}\n`);
      return outcome;
    }
    if (opts.maxTicks !== undefined && ticks >= opts.maxTicks) return outcome;
    await new Promise((r) => setTimeout(r, opts.pollMs ?? 5_000));
  }
}

/** Human-readable program status (the CLI and Emacs render this). */
export function programStatusLines(dir: string): string[] {
  const { program, nodes, state } = foldProgram(dir);
  const outcome = programOutcome(nodes, state);
  const alive = pidAlive(programPaths(dir).pid);
  const lines = [
    `${program.title}`,
    `program ${path.basename(dir)} · scheduler ${alive ? "running" : "stopped"} · ${outcome} · max ${program.maxParallel} in parallel`,
    "",
  ];
  const mark: Record<NodeStatus, string> = {
    waiting: "·",
    running: "▶",
    "needs-you": "⚑",
    stopped: "○",
    done: "✓",
    blocked: "✗",
  };
  for (const n of nodes) {
    const s = state.nodes[n.id];
    const deps = n.deps.length > 0 ? `  after ${n.deps.join(", ")}` : "";
    lines.push(`${mark[s.status]} ${n.id.padEnd(22)} ${s.status.padEnd(9)} ${s.runId ?? ""}${deps}`);
    if (s.branch) lines.push(`    branch ${s.branch}  (PR base: ${s.base ?? "?"})`);
    if (s.reason) lines.push(`    ${s.reason}`);
  }
  const directives = state.directives ?? [];
  if (directives.length > 0) {
    lines.push("", "Owner directives (whole program):");
    for (const d of directives) {
      lines.push(`  - ${d.id}${d.withdrawn ? " [withdrawn]" : " [in force]"}: ${d.text}`);
    }
  }
  return lines;
}

export function programPidAlive(dir: string): boolean {
  return pidAlive(programPaths(dir).pid);
}
