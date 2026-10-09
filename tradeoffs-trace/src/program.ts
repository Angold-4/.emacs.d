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

import { createRun, runPaths } from "./conductor.ts";
import { triageRecordFor } from "./core/triage.ts";
import type { RequirementItem } from "./core/items.ts";
import { readRunState } from "./effects/runner.ts";
import { processAlive } from "./effects/sweep.ts";
import { envBlockedLine } from "./core/env-preflight.ts";
import { execFileSync } from "node:child_process";
import { acquireLock, type Lock } from "./effects/lock.ts";

import { buildView, formatDuration, updateLiveWaiting } from "./view.ts";
import { notify, oneLine, waitReason, NOTIFY_REMINDER_MS } from "./notify.ts";
import { renderProgramChart } from "./charts.ts";
import { projectEntries, renderProgramEntryReview } from "./core/entries.ts";
import { renderGlossaryOrg } from "./core/briefs.ts";
import { runReviewLint } from "./core/review-lint.ts";
import { candidateAnchorFreshness } from "./render.ts";
// 06a finding #24: the program status names its models check's result.
import { modelsCheckStatusText, parseModelsCheck } from "./core/models-check.ts";

import {
  expandProgram,
  nodeBases,
  nodeBranch,
  initialProgramState,
  nextProgramDirectiveId,
  nextStarts,
  nodePlan,
  nodeReadableIds,
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
    /** Plan 06d (A3): the exclusive scheduler lock. One scheduler per
     * program; a second exits at once, and a lock left by a dead pid is
     * taken over because the OS releases the flock when its holder dies. */
    lock: path.join(dir, "scheduler.lock"),
    log: path.join(dir, "scheduler.log"),
    /** Plan 03c: the Org program file this program was started from. */
    source: path.join(dir, "source.json"),
    /** Plan 03c: the rendered dependency graph Emacs/the owner read. */
    programView: path.join(dir, "views", "program.txt"),
    /** Plan 05j: the program-wide entry review (`C-c m D`). */
    reviewView: path.join(dir, "views", "review.org"),
    /** Plan 01i: the program's own owner-input inbox (a `C-u` directive from
     * a node run is forwarded here; the Emacs program buffer writes here). */
    inbox: path.join(dir, "inbox"),
    inboxApplied: path.join(dir, "inbox", "applied"),
    inboxRejected: path.join(dir, "inbox", "rejected"),
  };
}

/** Validates the program (throws on a bad graph) and creates its directory. */
export function createProgram(root: string, program: ProgramFile, id = randomUUID().slice(0, 8)): string {
  const nodes = expandProgram(program);
  // Plan 03c: the readable ids are recorded in the program itself, once, at
  // start — `<program>-NN` by position in the program file.
  const recorded: ProgramFile = { ...program, readableIds: program.readableIds ?? nodeReadableIds(id, nodes) };
  const dir = path.join(programsRoot(root), id);
  fs.mkdirSync(dir, { recursive: true });
  fs.mkdirSync(programPaths(dir).inbox, { recursive: true });
  fs.mkdirSync(programPaths(dir).inboxApplied, { recursive: true });
  fs.mkdirSync(programPaths(dir).inboxRejected, { recursive: true });
  fs.writeFileSync(programPaths(dir).program, JSON.stringify(recorded, null, 2));
  fs.writeFileSync(programPaths(dir).events, "");
  return dir;
}

/** Plan 03c: record the Org program file this program was started from (Emacs
 * passes it), so the program buffer's header can name it. */
export function recordProgramSource(dir: string, source: string): void {
  fs.writeFileSync(programPaths(dir).source, JSON.stringify({ path: source }));
}

export function readProgramSource(dir: string): string | undefined {
  try {
    const raw = JSON.parse(fs.readFileSync(programPaths(dir).source, "utf8")) as { path?: string };
    return raw.path;
  } catch {
    return undefined;
  }
}

export function readProgram(dir: string): ProgramFile {
  return JSON.parse(fs.readFileSync(programPaths(dir).program, "utf8")) as ProgramFile;
}

export function foldProgram(dir: string): {
  program: ProgramFile;
  nodes: ProgramNode[];
  state: ProgramState;
  /** Plan 03c: every node's readable id (`<program>-NN`), from the program
   * file when it recorded them and by position otherwise. */
  readableIds: Record<string, string>;
  /** Plan 01b: the time each node last changed status (from its own event's
   * `ts`), for the wait duration `tt program status` and the mode-line show. */
  at: Record<string, string>;
} {
  const program = readProgram(dir);
  const nodes = expandProgram(program);
  const readableIds = program.readableIds ?? nodeReadableIds(path.basename(dir), nodes);
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
  return { program, nodes, state, readableIds, at };
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
  fs.mkdirSync(p.inboxRejected, { recursive: true });
  let names: string[];
  try {
    names = fs.readdirSync(p.inbox);
  } catch {
    return;
  }
  /** Moves a refused command out of the inbox and says why, beside it — the
   * same visible refusal a run's inbox gives, never a silent no-op. */
  const reject = (name: string, reason: string): void => {
    try {
      fs.writeFileSync(path.join(p.inboxRejected, `${name}.reason.txt`), `${reason}\n`);
      fs.renameSync(path.join(p.inbox, name), path.join(p.inboxRejected, name));
    } catch {
      // already moved
    }
  };
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
      const target = id ? (state.directives ?? []).find((d) => d.id === id) : undefined;
      if (!target) {
        reject(name, `no program directive ${id ?? "(none named)"} exists`);
        continue;
      }
      if (target.withdrawn) {
        reject(name, `program directive ${id} is already withdrawn`);
        continue;
      }
      appendProgramEvent(dir, { type: "DIRECTIVE_WITHDRAWN", directiveId: id! });
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

/** Plan 06i: the carried requirements a node's dependencies write into its
 * contract. Read from each dependency's accepted run: every `carriedItems`
 * id becomes a requirement named `CARRY-<id>` with the item's own text and a
 * VERIFY — `test "carried <id> is fixed"` when the item's triage impact is
 * wrong-output, `review` otherwise. */
export function carriedRequirementsFor(program: ProgramFile, node: ProgramNode, state: ProgramState, runRoot: string): RequirementItem[] {
  const out: RequirementItem[] = [];
  const seen = new Set<string>();
  // Plan 06i (A4): a carried item/deferral goes ONLY to the phase its
  // `--to` target names (`carriedTo[id]` / `d.toPhase`) — the runbook's
  // carry rule. A node is addressed by its node id or its phase id; an
  // absent target (the `accept_carried` fallback records `"next"`) reaches
  // the direct dependents, exactly as before.
  const phaseId = program.entries.find((e) => e.id === node.entry)!.plan.phases[node.phaseIndex].id;
  // A phase id is only an unambiguous target when it is unique across the
  // program's nodes; repeated phase ids (every entry using `p1`) must not let
  // one `--to p1` reach every child. The node id is always unique.
  const phaseIdCounts = new Map<string, number>();
  for (const e of program.entries) {
    for (const ph of e.plan.phases) phaseIdCounts.set(ph.id, (phaseIdCounts.get(ph.id) ?? 0) + 1);
  }
  const phaseIdIsUnique = (phaseIdCounts.get(phaseId) ?? 0) === 1;
  const targetedHere = (to: string | undefined): boolean =>
    to === undefined || to === "next" || to === node.id || (phaseIdIsUnique && to === phaseId);
  for (const dep of node.deps) {
    const s = state.nodes[dep];
    if (!s?.runId) continue;
    const read = readRunState(path.join(runRoot, s.runId), runRoot);
    if (!read) continue;
    const phase = read.state.phase;
    for (const id of phase.carriedItems ?? []) {
      if (seen.has(id)) continue;
      if (!targetedHere(phase.carriedTo?.[id])) continue;
      seen.add(id);
      const record = triageRecordFor(phase.triage, id);
      const finding = phase.findings.find((f) => f.id === id);
      const decision = phase.decisions.find((d) => d.id === id);
      const text = finding?.evidence ?? decision?.choice ?? `carried item ${id}`;
      const wrongOutput = record?.impact === "wrong-output";
      out.push({
        id: `CARRY-${id}`,
        title: `carried from ${dep}: ${id}`,
        text: `Carried from ${dep}: ${text}`,
        arch: [],
        verify: [wrongOutput ? `test "carried ${id} is fixed"` : "review"],
      });
    }
    // Plan 06i (A4): an open deferral is a requirement of the next node too,
    // with its guard as the VERIFY (a `test` for a cost test, `review` for an
    // owner ruling). A deferral never disappears between phases.
    for (const d of phase.deferrals ?? []) {
      if (d.status !== "open" || seen.has(d.itemId)) continue;
      if (!targetedHere(d.toPhase)) continue;
      seen.add(d.itemId);
      const record = triageRecordFor(phase.triage, d.itemId);
      const wrongOutput = record?.impact === "wrong-output";
      // Plan 06i (A4): a WRONG-OUTPUT item's next-node VERIFY is the FIX
      // verification, never the cost guard (which already passes today). The
      // cost test stays attached as context only. A judgement deferral is
      // judged by its guard (test or review).
      const costContext = d.test ? ` [cost guard: ${d.test}]` : "";
      out.push({
        id: `DEFER-${d.itemId}`,
        title: `deferred from ${dep}: ${d.itemId}`,
        text: `Deferred from ${dep}: ${d.text}${costContext}${d.ownerRuling ? ` (owner ruling ${d.ownerRuling})` : ""}`,
        arch: [],
        verify: [wrongOutput ? `test "carried ${d.itemId} is fixed"` : d.test ? `test "${d.test}"` : "review"],
      });
    }
  }
  return out;
}

function pidAlive(file: string): boolean {
  try {
    const pid = Number(fs.readFileSync(file, "utf8"));
    if (!(pid > 0)) return false;
    // A2/C3 (plan 06e): the sweep owns the signalling decision.
    return processAlive(pid);
  } catch {
    return false;
  }
}

/** A launched node's status, observed from its run directory. "crashed":
 * the conductor died without a clean stop (no `stopped` marker). */
export function observeRun(runDir: string, root: string = path.dirname(runDir)): Exclude<NodeStatus, "waiting"> | "crashed" {
  let phase: string | undefined;
  let envBlocked = false;
  // A4 (plan 06e): the scheduler reads a node's run through `runnerFor`, so a
  // run recorded under an installed older runner is read by that runner.
  const read = readRunState(runDir, root);
  if (read) {
    phase = read.state.phase.phase;
    // Plan 05i: an environment block is its own node status, distinct from a
    // code BLOCKED: it is recoverable by fixing the environment and resuming.
    envBlocked = read.state.run === "ENV_BLOCKED";
  }
  // A terminal phase wins over an environment block (finding M-7): a DONE or
  // BLOCKED run can never be re-blocked by a resume, so the program must not
  // report a finished node as env-blocked.
  if (phase === "DONE") return "done";
  if (phase === "BLOCKED") return "blocked";
  if (envBlocked) return "env-blocked";
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
export function runWaitReason(runDir: string, root: string = path.dirname(runDir)): string | undefined {
  const read = readRunState(runDir, root);
  if (!read) return undefined;
  const { state } = read;
  if (state.run === "ENV_BLOCKED") {
    return state.phase.env?.blocked ? envBlockedLine(state.phase.env.blocked) : "environment blocked";
  }
  if (state.phase.phase === "AWAITING_OWNER" || state.phase.phase === "BLOCKED") return waitReason(state.phase);
  return undefined;
}

/** Plan 06c (A3): the baseline-reuse decision, owned by the scheduler. When
 * a node has exactly one dependency, that dependency's run is DONE, and its
 * ACCEPTED candidate is exactly this node's base commit, the node reuses the
 * accepted candidate's passing check record instead of running a baseline.
 * The choice and its provenance are passed into the run; the conductor never
 * scans sibling runs itself. */
function acceptedCandidateBaseline(
  program: ProgramFile,
  nodes: ProgramNode[],
  node: ProgramNode,
  state: ProgramState,
  runRoot: string,
): { fromRunId: string; fromCandidateSha: string } | undefined {
  if (node.deps.length !== 1) return undefined;
  const dep = node.deps[0];
  const depState = state.nodes[dep];
  if (!depState?.runId || depState.status !== "done") return undefined;
  const depRunDir = path.join(runRoot, depState.runId);
  let accepted: string | undefined;
  const read = readRunState(depRunDir, runRoot);
  if (!read || read.state.phase.phase !== "DONE") return undefined;
  accepted = read.state.phase.candidate?.sha;
  if (!accepted) return undefined;
  const base = nodeBases(program, nodes, node)[0];
  let baseSha: string;
  try {
    baseSha = git(program.entries.find((e) => e.id === node.entry)!.plan.repo, ["rev-parse", "--verify", `${base}^{commit}`]);
  } catch {
    return undefined;
  }
  if (baseSha !== accepted) return undefined;
  return { fromRunId: depState.runId, fromCandidateSha: accepted };
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
  /** Plan 01b: how long a stuck program is watched before its one reminder. */
  notifyReminderMs?: number;
}

/** One scheduler step: observe active nodes, start ready ones. Returns the
 * program outcome after the step. */
export function schedulerTick(dir: string, opts: SchedulerOptions): ProgramOutcome {
  // Plan 01i: owner directives the program buffer (or a node's `C-u` input)
  // left in the program's inbox become logged program events first, so they
  // are part of the program state this tick observes and delivers.
  scanProgramInbox(dir);
  let { program, nodes, state, readableIds } = foldProgram(dir);
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
    if (seen !== s.status) {
      // Plan 05i: an env-blocked node carries the `env blocked · …` reason,
      // like a needs-you node, so the program buffer names the missing tool.
      const withReason = seen === "needs-you" || seen === "blocked" || seen === "env-blocked";
      record({ type: "NODE_STATUS", node: n.id, status: seen, ...(withReason ? reasonField(runWaitReason(runDir)) : {}) });
    }
  }
  for (const id of nextStarts(nodes, state, program.maxParallel)) {
    const node = nodes.find((n) => n.id === id)!;
    // Plan 06i: a dependency's carried items become real requirements of this
    // node's contract (an id, the item's text and a VERIFY). A wrong-output
    // item carries a `test` verify; every other carried item is judged by
    // review. Directive text alone never carries an item.
    const carried = carriedRequirementsFor(program, node, state, opts.runRoot);
    const plan = nodePlan(program, node, programDirectivesInForce(state), carried);
    if (carried.length > 0) {
      appendProgramEvent(dir, { type: "NODE_CARRIED_ITEMS", node: id, items: carried.map((c) => c.id) } as ProgramEvent);
    }
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
    // Plan 06c (A3): the scheduler is the only owner of the baseline-reuse
    // choice. A node with exactly one dependency whose accepted candidate IS
    // this node's base reuses that candidate's check record; the path is
    // passed into the run, and the conductor only consumes it.
    const baselineReuse = acceptedCandidateBaseline(program, nodes, node, state, opts.runRoot);
    fs.writeFileSync(
      path.join(runDir, "program.json"),
      JSON.stringify({ programId: path.basename(dir), node: id, readableId: readableIds[id], ...(baselineReuse ? { baselineReuse } : {}) }),
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
  // Plan 03c: keep `views/program.txt` current after every status change.
  writeProgramChart(dir);
  // Plan 06f (A2): the scheduler is `live.json`'s node writer, on the same
  // beat — the nodes still waiting to start are exactly what the mode line
  // must show while a run is not yet alive.
  writeProgramLive(dir, nodes, state);
  return programOutcome(nodes, state);
}

/** Plan 06f (A2): republish this program's waiting nodes in `<root>/live.json`.
 * Every other program's wait and every run row (written by the conductors) is
 * preserved. */
function writeProgramLive(dir: string, nodes: ProgramNode[], state: ProgramState): void {
  try {
    const root = path.dirname(path.dirname(dir));
    const programId = path.basename(dir);
    const waiting = nodes
      .filter((n) => state.nodes[n.id]?.status === "waiting")
      .map((n) => ({ program: programId, node: n.id }));
    updateLiveWaiting(root, programId, waiting);
  } catch {
    // a live file that cannot be written must never stop the scheduler
  }
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
        // Say how to unblock it: the scheduler keeps an existing branch, so a
        // hand-made merge commit at `branch` is used as-is on retry (plans
        // 14h and 01h were both unblocked this way).
        return {
          ok: false,
          reason: `merging ${bases[0]} with ${bases[i]} for ${node} conflicts${files ? `: ${files}` : ""}. To unblock: merge ${bases.join(" + ")} by hand into a new branch ${branch}, then \`tt program retry <program> ${node}\``,
        };
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

/** Plan 06d (A3): take the program's exclusive scheduler lock. A second
 * scheduler fails fast with "scheduler already running"; a lock whose pid is
 * dead is taken over, because the previous holder's flock died with it. The
 * returned Lock is released by `runScheduler` when it exits. */
export async function acquireSchedulerLock(dir: string): Promise<Lock> {
  return acquireLock(programPaths(dir).lock);
}

/** The scheduler process: tick until the program is done, stuck, stopped or
 * paused. Holds `programs/<id>/scheduler.lock` for its whole life. */
export async function runScheduler(dir: string, opts: SchedulerOptions): Promise<ProgramOutcome> {
  let lock: Lock;
  try {
    lock = await acquireSchedulerLock(dir);
  } catch (err) {
    const message = `scheduler already running for program ${path.basename(dir)}`;
    fs.appendFileSync(programPaths(dir).log, `${new Date().toISOString()} ${message}\n`);
    process.stderr.write(`${message}\n`);
    // Another scheduler holds the program; this process must not start any
    // node, so it exits at once. `running` reports what the program is doing.
    return "running";
  }
  try {
    // The lock file names THIS scheduler, not the perl helper that holds the
    // flock, so a second scheduler's refusal can name the real pid.
    fs.writeFileSync(programPaths(dir).lock, String(process.pid));
    fs.writeFileSync(programPaths(dir).pid, String(process.pid));
    return await schedulerLoop(dir, opts);
  } finally {
    // Plan 06f (A2): a scheduler that exits has no node waiting to start, so
    // its `live.json' wait list is cleared rather than left stale.
    try {
      updateLiveWaiting(path.dirname(path.dirname(dir)), path.basename(dir), []);
    } catch {
      // best effort: a live file that cannot be written must never matter here
    }
    await lock.release();
  }
}

async function schedulerLoop(dir: string, opts: SchedulerOptions): Promise<ProgramOutcome> {
  const reminderMs = opts.notifyReminderMs ?? NOTIFY_REMINDER_MS;
  let ticks = 0;
  let stuckNotifiedAt: number | undefined;
  for (;;) {
    const outcome = schedulerTick(dir, opts);
    ticks += 1;
    if (outcome === "running") {
      stuckNotifiedAt = undefined;
    } else if (outcome === "stuck") {
      // Plan 01b: a stuck program is exactly a wait for the owner, so the
      // scheduler stays alive to send the one 30-minute reminder (design
      // D4's "still waiting") before it exits — nothing else observes the
      // program once it is stuck. `notify` does the dedup, so the re-notify
      // is a reminder only after the window has passed; if a `tt program
      // resume` makes the program runnable again, the next tick sees
      // "running" and clears the wait.
      if (stuckNotifiedAt === undefined) {
        fs.appendFileSync(programPaths(dir).log, `${new Date().toISOString()} program stuck\n`);
        notifyProgramOutcome(dir, "stuck", { reminderMs });
        stuckNotifiedAt = Date.now();
      } else if (Date.now() - stuckNotifiedAt >= reminderMs) {
        notifyProgramOutcome(dir, "stuck", { reminderMs });
        return outcome;
      }
    } else {
      fs.appendFileSync(programPaths(dir).log, `${new Date().toISOString()} program ${outcome}\n`);
      if (outcome === "done") notifyProgramOutcome(dir, "done", { reminderMs });
      return outcome;
    }
    if (opts.maxTicks !== undefined && ticks >= opts.maxTicks) return outcome;
    await new Promise((r) => setTimeout(r, opts.pollMs ?? 5_000));
  }
}

/** Plan 01b: one notification when a program ends done or stuck (never when
 * the owner stopped it on purpose). `notify` dedups by `waitKey`, so a
 * scheduler restarted against an already-finished program does not re-notify,
 * and a failure of the notifier is logged into `scheduler.log` and ignored.
 * `reminderMs` matches the window `runScheduler` waits before re-notifying a
 * stuck program and the conductor's own default. */
export function notifyProgramOutcome(dir: string, outcome: "done" | "stuck", opts: { reminderMs?: number } = {}): void {
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
    // An env-blocked node is what makes the program `stuck` (findings A-8),
    // so the notification must name its `env blocked · …` reason, not omit
    // the tool.
    const stuck = nodes.find((n) => state.nodes[n.id].status === "blocked" || state.nodes[n.id].status === "env-blocked");
    const reason =
      outcome === "done"
        ? "program done"
        : `program stuck${stuck ? `: ${oneLine(state.nodes[stuck.id].reason ?? `${stuck.id} ${state.nodes[stuck.id].status}`)}` : ""}`;
    notify(
      { id, kind: "program", title: program.title, reason, waitKey: `program:${id}:${outcome}` },
      { root: path.dirname(path.dirname(dir)), ...(opts.reminderMs !== undefined ? { reminderMs: opts.reminderMs } : {}), onError: (message) => log(`notify: ${message}`) },
    );
  } catch (err) {
    // Announcing a finished program must never be what stops the scheduler.
    log(`notify: ${String((err as Error)?.message ?? err)}`);
  }
}

/** Plan 01h: one line per node under its own status line — its rounds, its
 * minutes, its owner wait and its single most important trade-off — built
 * from the node's run. Undefined when the run cannot be read (a node that
 * never started, or a hand-made fixture). */
function nodeCostLine(runDir: string): string | undefined {
  try {
    // A4 (plan 06e): the plan comes from the run's recorded runner.
    const read = readRunState(runDir, path.dirname(runDir));
    if (!read) return undefined;
    const view = buildView(runDir, read.plan, pidAlive(path.join(runDir, "conductor.pid")));
    const cost = view.cost;
    if (!cost) return undefined;
    const parts = [`${cost.rounds} round${cost.rounds === 1 ? "" : "s"}`, `${cost.totalMinutes}m`];
    if (cost.ownerWaitMinutes > 0) parts.push(`owner wait ${cost.ownerWaitMinutes}m`);
    const top = view.tradeoffs?.[0];
    if (top) parts.push(`top: ${top.text}`);
    return `    ${parts.join(" · ")}`;
  } catch {
    return undefined;
  }
}

/** Human-readable program status (the CLI and Emacs render this). Waiting
 * (needs-you) nodes come first, oldest wait first, each with how long it has
 * been waiting and the one-line reason. `now` is injectable for tests.
 *
 * Plan 01h: with `nodeDetail` (the default), each node with a readable run
 * gets one more indented line: its rounds, minutes, owner wait and its most
 * important trade-off. `tt program list` passes `nodeDetail: false` so it
 * never builds a view for every node. */
/** Plan 03c: the program's dependency graph as an ASCII chart, written to
 * `<program>/views/program.txt`. A running node shows its run's current phase
 * state (read from the run directory; a node that never started shows none). */
export function programChartText(dir: string): string {
  const { program, nodes, state, readableIds } = foldProgram(dir);
  const root = path.dirname(path.dirname(dir));
  const phaseOf: Record<string, string> = {};
  for (const n of nodes) {
    const s = state.nodes[n.id];
    if (!s.runId || s.status !== "running") continue;
    const read = readRunState(path.join(root, s.runId), root);
    if (read) phaseOf[n.id] = read.state.phase.phase;
    // else: the run is not readable (yet); the node still shows its node state
  }
  return renderProgramChart({ ...program, readableIds }, nodes, state, { programId: path.basename(dir), phaseOf });
}

/** Plan 03c: write the program chart beside the program's other files. */
export function writeProgramChart(dir: string): void {
  try {
    const file = programPaths(dir).programView;
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, programChartText(dir));
  } catch {
    // a chart that cannot be written must never stop the scheduler
  }
  writeProgramReview(dir);
}

/** Plan 05j: `programs/<id>/views/review.org` — every node run's entries,
 * grouped by the same three sections and tagged with the node's phase id.
 * Cross-phase links are only through shared anchors, so the same rule as the
 * phase view holds across the program. A node that never started or whose
 * run is unreadable contributes nothing (and is not hidden silently: it has
 * no entries to show). */
export function programReviewText(dir: string): string {
  const { nodes, state, readableIds } = foldProgram(dir);
  const root = path.dirname(path.dirname(dir));
  const phases: Array<{
    phaseId: string;
    readableId?: string;
    candidate?: { sha: string };
    runId?: string;
    contract?: { contractVersion?: { snapshot: number; sectionSha256: string } };
    messages?: unknown[];
    entries?: unknown[];
    decisions?: unknown[];
    overrides?: unknown[];
    briefs?: unknown[];
    ownerRequests?: unknown[];
  }> = [];
  // The lint runs per phase, on each phase's OWN entries and message ids
  // (every phase numbers E-1/T-1 from scratch, so a single combined
  // projection would collide ids and fire one-anchor on a topic that recurs
  // across phases — findings M-16/A-20). The program view itself folds those
  // cross-phase duplicates into one row.
  const violations: Array<{ detail: string }> = [];
  for (const n of nodes) {
    const s = state.nodes[n.id];
    if (!s.runId) continue;
    try {
      const runDir = path.join(root, s.runId);
      const read = readRunState(runDir, root);
      if (!read) continue;
      const phase = read.state.phase;
      const sha = phase.candidate?.sha;
      const projected = projectEntries({
        messages: phase.messages ?? [],
        entries: phase.entries ?? [],
        newestCandidateSha: sha,
        anchorFreshness: candidateAnchorFreshness(sha ? path.join(runDir, "candidates", sha) : undefined),
      });
      violations.push(...runReviewLint({ projected, newestCandidateSha: sha, messages: phase.messages ?? [] }).violations);
      phases.push({
        phaseId: phase.phaseId,
        readableId: readableIds[n.id],
        candidate: sha ? { sha } : undefined,
        messages: phase.messages ?? [],
        entries: phase.entries ?? [],
        decisions: phase.decisions ?? [],
        overrides: phase.overrides ?? [],
        runId: phase.runId,
        contract: phase.contract ? { contractVersion: phase.contract.contractVersion } : undefined,
        // Decision briefs: the program review shows each node's open owner
        // items as briefs too (renderProgramEntryReview).
        briefs: phase.briefs ?? [],
        ownerRequests: phase.ownerRequests ?? [],
      });
    } catch {
      // the run is not readable (yet): it contributes no entries
    }
  }
  const newestCandidateSha = [...phases].reverse().map((ph) => ph.candidate?.sha).find((sha) => typeof sha === "string" && sha.length > 0);
  return renderProgramEntryReview({
    program: { id: path.basename(dir), phases: phases as never },
    newestCandidateSha,
    lintError: violations.length > 0 ? violations[0].detail : undefined,
  });
}

/** Plan 05j: write the program review beside the program chart. */
export function writeProgramReview(dir: string): void {
  try {
    const file = programPaths(dir).reviewView;
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, programReviewText(dir));
    // Goal (4): the glossary a brief links to, beside the program review too.
    fs.writeFileSync(path.join(path.dirname(file), "glossary.org"), `* Owner glossary\n${renderGlossaryOrg()}\n`);
  } catch {
    // a review that cannot be written must never stop the scheduler
  }
}

/** Human-readable program status (the CLI and Emacs render this). Waiting
 * (needs-you) nodes come first, oldest wait first, each with how long it has
 * been waiting and the one-line reason. `now` is injectable for tests.
 *
 * Plan 01h: with `nodeDetail` (the default), each node with a readable run
 * gets one more indented line: its rounds, minutes, owner wait and its most
 * important trade-off. `tt program list` passes `nodeDetail: false` so it
 * never builds a view for every node. */
export function programStatusLines(dir: string, now: Date = new Date(), opts: { nodeDetail?: boolean } = {}): string[] {
  const { program, nodes, state, readableIds, at } = foldProgram(dir);
  const outcome = programOutcome(nodes, state);
  const alive = pidAlive(programPaths(dir).pid);
  // 06a finding #24: the models preflight's own line, when the program ran it.
  let modelsCheck: string | undefined;
  try {
    modelsCheck = modelsCheckStatusText(parseModelsCheck(JSON.parse(fs.readFileSync(path.join(dir, "models-check.json"), "utf8"))));
  } catch {
    modelsCheck = undefined;
  }
  const lines = [
    `${program.title}`,
    `program ${path.basename(dir)} · scheduler ${alive ? "running" : "stopped"} · ${outcome} · max ${program.maxParallel} in parallel`,
    ...(modelsCheck ? [`models    ${modelsCheck}`] : []),
    "",
  ];
  const mark: Record<NodeStatus, string> = {
    waiting: "·",
    running: "▶",
    "needs-you": "⚑",
    stopped: "○",
    done: "✓",
    blocked: "✗",
    "env-blocked": "E",
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
    const readable = (readableIds[n.id] ?? "").padEnd(11);
    if (s.status === "needs-you") {
      const since = at[n.id];
      const duration = since ? formatDuration(now.getTime() - Date.parse(since)) : "?";
      lines.push(`${mark[s.status]} ${n.id.padEnd(22)} ${`waiting ${duration}`.padEnd(9)} ${s.runId ?? ""} ${readable}${deps}`.trimEnd());
      lines.push(`    ${s.reason ?? "needs you"}`);
    } else {
      lines.push(`${mark[s.status]} ${n.id.padEnd(22)} ${s.status.padEnd(9)} ${s.runId ?? ""} ${readable}${deps}`.trimEnd());
    }
    if (s.branch) lines.push(`    branch ${s.branch}  (PR base: ${s.base ?? "?"})`);
    if (s.status !== "needs-you" && s.reason) lines.push(`    ${s.reason}`);
    if (opts.nodeDetail !== false && s.runId) {
      const detail = nodeCostLine(path.join(path.dirname(path.dirname(dir)), s.runId));
      if (detail) lines.push(detail);
    }
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
