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

import { createRun, rebuildState, runPaths, type RunPlanFile } from "./conductor.ts";
import { execFileSync } from "node:child_process";

import { formatDuration } from "./view.ts";
import { notify, oneLine, waitReason } from "./notify.ts";

import {
  expandProgram,
  nodeBases,
  nodeBranch,
  initialProgramState,
  nextStarts,
  nodePlan,
  programOutcome,
  reduceProgram,
  type NodeStatus,
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
  };
}

/** Validates the program (throws on a bad graph) and creates its directory. */
export function createProgram(root: string, program: ProgramFile, id = randomUUID().slice(0, 8)): string {
  expandProgram(program);
  const dir = path.join(programsRoot(root), id);
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(programPaths(dir).program, JSON.stringify(program, null, 2));
  fs.writeFileSync(programPaths(dir).events, "");
  return dir;
}

export function readProgram(dir: string): ProgramFile {
  return JSON.parse(fs.readFileSync(programPaths(dir).program, "utf8")) as ProgramFile;
}

export function foldProgram(dir: string): {
  program: ProgramFile;
  nodes: ProgramNode[];
  state: ProgramState;
  /** Plan 01b: the time each node last changed status (from its own event's
   * `ts`), for the wait duration `tt program status` and the mode-line show. */
  at: Record<string, string>;
} {
  const program = readProgram(dir);
  const nodes = expandProgram(program);
  let state = initialProgramState(nodes);
  const at: Record<string, string> = {};
  const text = fs.existsSync(programPaths(dir).events) ? fs.readFileSync(programPaths(dir).events, "utf8") : "";
  for (const line of text.split("\n")) {
    if (!line.trim()) continue;
    try {
      const parsed = JSON.parse(line) as { ts?: string; event: ProgramEvent };
      state = reduceProgram(state, parsed.event);
      const node = (parsed.event as { node?: unknown }).node;
      if (typeof node === "string" && typeof parsed.ts === "string") at[node] = parsed.ts;
    } catch {
      // a torn last line from a crash; the next append rewrites nothing
    }
  }
  return { program, nodes, state, at };
}

export function appendProgramEvent(dir: string, event: ProgramEvent): void {
  fs.appendFileSync(programPaths(dir).events, `${JSON.stringify({ ts: new Date().toISOString(), event })}\n`);
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

/** Plan 01b: the one-line reason a node's run is waiting, read from the run
 * itself (its plan snapshot is redacted, and its owner-request reasons were
 * redacted when the conductor logged them). `undefined` when the run is not
 * (yet) parked or cannot be read. */
export function runWaitReason(runDir: string): string | undefined {
  try {
    const plan = JSON.parse(fs.readFileSync(path.join(runPaths(runDir).plan, "v1.json"), "utf8")) as RunPlanFile;
    const state = rebuildState(runDir, plan, { lenient: true });
    if (state.phase.phase === "AWAITING_OWNER" || state.phase.phase === "BLOCKED") return waitReason(state.phase);
  } catch {
    // a run whose plan or log cannot be read yet
  }
  return undefined;
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
  let { program, nodes, state } = foldProgram(dir);
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
    if (seen !== s.status) {
      record({ type: "NODE_STATUS", node: n.id, status: seen, ...(seen === "needs-you" ? reasonField(runWaitReason(runDir)) : {}) });
    }
  }
  for (const id of nextStarts(nodes, state, program.maxParallel)) {
    const node = nodes.find((n) => n.id === id)!;
    const plan = nodePlan(program, node);
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

/** `{ reason }` only when there is one, so an event payload stays exactly
 * what it was for a node with no known reason. */
function reasonField(reason: string | undefined): { reason?: string } {
  return reason ? { reason } : {};
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
      // Plan 01b: a program that ends done or stuck is the owner's business.
      // The scheduler is the component that observes this, so it notifies.
      if (outcome === "done" || outcome === "stuck") notifyProgramOutcome(dir, outcome);
      return outcome;
    }
    if (opts.maxTicks !== undefined && ticks >= opts.maxTicks) return outcome;
    await new Promise((r) => setTimeout(r, opts.pollMs ?? 5_000));
  }
}

/** Plan 01b: one notification when a program ends done or stuck (never when
 * the owner stopped it on purpose). `notify` dedups by `waitKey`, so a
 * scheduler restarted against an already-finished program does not re-notify,
 * and a failure of the notifier is logged into `scheduler.log` and ignored. */
export function notifyProgramOutcome(dir: string, outcome: "done" | "stuck"): void {
  const id = path.basename(dir);
  const log = (message: string) => {
    try {
      fs.appendFileSync(programPaths(dir).log, `${new Date().toISOString()} ${message}\n`);
    } catch {
      // the log directory may be gone; a failed log is not a reason to stop
    }
  };
  try {
    const program = readProgram(dir);
    const { nodes, state } = foldProgram(dir);
    const blocked = nodes.find((n) => state.nodes[n.id].status === "blocked");
    const reason =
      outcome === "done"
        ? "program done"
        : `program stuck${blocked ? `: ${oneLine(state.nodes[blocked.id].reason ?? `${blocked.id} blocked`)}` : ""}`;
    notify(
      { id, kind: "program", title: program.title, reason, waitKey: `program:${id}:${outcome}` },
      { root: path.dirname(path.dirname(dir)), onError: (message) => log(`notify: ${message}`) },
    );
  } catch (err) {
    // Announcing a finished program must never be what stops the scheduler.
    log(`notify: ${String((err as Error)?.message ?? err)}`);
  }
}

/** Human-readable program status (the CLI and Emacs render this). Waiting
 * (needs-you) nodes come first, oldest wait first, each with how long it has
 * been waiting and the one-line reason. `now` is injectable for tests. */
export function programStatusLines(dir: string, now: Date = new Date()): string[] {
  const { program, nodes, state, at } = foldProgram(dir);
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
  const order = (id: string) => {
    const t = at[id];
    return t ? Date.parse(t) : Number.MAX_SAFE_INTEGER;
  };
  const waiting = nodes
    .filter((n) => state.nodes[n.id].status === "needs-you")
    .sort((a, b) => order(a.id) - order(b.id));
  const rest = nodes.filter((n) => state.nodes[n.id].status !== "needs-you");
  for (const n of [...waiting, ...rest]) {
    const s = state.nodes[n.id];
    const deps = n.deps.length > 0 ? `  after ${n.deps.join(", ")}` : "";
    if (s.status === "needs-you") {
      const since = at[n.id];
      const duration = since ? formatDuration(now.getTime() - Date.parse(since)) : "?";
      lines.push(`${mark[s.status]} ${n.id.padEnd(22)} ${`waiting ${duration}`.padEnd(9)} ${s.runId ?? ""}${deps}`.trimEnd());
      lines.push(`    ${s.reason ?? "needs you"}`);
    } else {
      lines.push(`${mark[s.status]} ${n.id.padEnd(22)} ${s.status.padEnd(9)} ${s.runId ?? ""}${deps}`.trimEnd());
    }
    if (s.branch) lines.push(`    branch ${s.branch}  (PR base: ${s.base ?? "?"})`);
    if (s.status !== "needs-you" && s.reason) lines.push(`    ${s.reason}`);
  }
  return lines;
}

/** Plan 01b: every node currently waiting for the owner, oldest wait first —
 * what `tt program list --json` gives Emacs for the mode-line indicator. */
export interface WaitingNode {
  node: string;
  runId?: string;
  since?: string;
  duration: string;
  reason: string;
}

export function programWaitingNodes(dir: string, now: Date = new Date()): WaitingNode[] {
  const { nodes, state, at } = foldProgram(dir);
  const order = (id: string) => {
    const t = at[id];
    return t ? Date.parse(t) : Number.MAX_SAFE_INTEGER;
  };
  return nodes
    .filter((n) => state.nodes[n.id].status === "needs-you")
    .sort((a, b) => order(a.id) - order(b.id))
    .map((n) => {
      const s = state.nodes[n.id];
      const since = at[n.id];
      return {
        node: n.id,
        runId: s.runId,
        since,
        duration: since ? formatDuration(now.getTime() - Date.parse(since)) : "?",
        reason: s.reason ?? "needs you",
      };
    });
}

export function programPidAlive(dir: string): boolean {
  return pidAlive(programPaths(dir).pid);
}
