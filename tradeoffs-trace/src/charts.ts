// Plan 03c: the ASCII charts the owner reads to know where things are. They
// are generated from the same tables and structures the runtime obeys — the
// phase state machine from `TRANSITIONS` (core/transitions.ts), the message
// state machine from `MESSAGE_TRANSITIONS` (core/messages.ts) and the program
// graph from `program.json` + its folded state — so a chart can never drift
// from the code. `test/contract/charts.test.ts` is the guard: it fails when a
// transition row is added and no edge draws it.
//
// Pure: no I/O. The conductor writes `views/loop.txt`; the scheduler writes
// `<program>/views/program.txt`; `tt`/Emacs only display those files.

import type { MessageTransitionRow } from "./core/messages.ts";
import { MESSAGE_TRANSITIONS } from "./core/messages.ts";
import type { ProgramNode, ProgramState } from "./core/program.ts";
import type { TransitionRow } from "./core/transitions.ts";
import { TRANSITIONS } from "./core/transitions.ts";

/** How often and how long each state was entered — what a chart shows beside
each state. `current` names the phase state the run is in now; `currentRun`
the run-axis state. Only the phase timeline records arrivals, so the run
section shows no counts (its rows are the run budget, not a visited loop). */
export interface ChartStats {
  current?: string;
  currentRun?: string;
  entries: Record<string, number>;
  timeMs: Record<string, number>;
}

/** The slice of a `Timeline` (src/conductor.ts) the chart needs. Structural,
 * so charts.ts stays free of a conductor import cycle. */
export interface TimelineLike {
  state: { run?: string; phase: { phase: string } };
  phases: Array<{ phase: string; at: string }>;
}

/** Entry counts and time spent per state, from the run's own timeline: every
 * entry in `timeline.phases` is one arrival, and each entry lasts until the
 * next one (the last one runs to `now`). */
export function statsFromTimeline(timeline: TimelineLike, now: Date): ChartStats {
  const entries: Record<string, number> = {};
  const timeMs: Record<string, number> = {};
  const phases = timeline.phases;
  for (let i = 0; i < phases.length; i++) {
    const { phase, at } = phases[i];
    entries[phase] = (entries[phase] ?? 0) + 1;
    const end = i + 1 < phases.length ? Date.parse(phases[i + 1].at) : now.getTime();
    timeMs[phase] = (timeMs[phase] ?? 0) + Math.max(0, end - Date.parse(at));
  }
  return { current: timeline.state.phase.phase, currentRun: timeline.state.run, entries, timeMs };
}

function formatMs(ms: number | undefined): string {
  if (ms === undefined) return "?";
  const s = Math.max(0, Math.round(ms / 1000));
  if (s < 60) return `${s}s`;
  const m = Math.floor(s / 60);
  if (m < 60) return `${m}m${String(s % 60).padStart(2, "0")}s`;
  return `${Math.floor(m / 60)}h${String(m % 60).padStart(2, "0")}m`;
}

/** The states that dispatch agents, the role(s) each one dispatches, and the
 * model role their model comes from. A state not in this map shows no role.
 * EVALUATING appears once the 04a/04b generator adds it to `TRANSITIONS`: the
 * chart draws the states the table has, never a hard-coded list. */
const DISPATCH_ROLES: Record<string, { roles: string; model: "worker" | "reviewer" | "evaluator" }> = {
  IMPLEMENTING: { roles: "worker", model: "worker" },
  REVIEWING: { roles: "M, A, B", model: "reviewer" },
  EVALUATING: { roles: "evaluator, panel", model: "evaluator" },
};

/** The models the chart may show, one per role. A role with no model (Pi's
 * settings unread, or a test) reads as `default`. */
export interface ChartModels {
  worker?: string;
  reviewer?: string;
  evaluator?: string;
}

/** Triggers whose edges are rare enough to fold out of the graph: a crash
 * recovery (interrupted) or a launch failure. Anything else is drawn. */
function isRare(row: TransitionRow): boolean {
  return row.trigger === "LAUNCH_FAILED" || row.trigger.endsWith("_INTERRUPTED");
}

const PHASE_ORDER = [
  "READY",
  "IMPLEMENTING",
  "REPAIRING",
  "FREEZING",
  "CHECKING",
  "PROBING",
  "REVIEWING",
  "RESOLVING",
  "GATING",
  "ACCEPTED",
  "PUBLISHING",
  "DONE",
  "AWAITING_OWNER",
  "BLOCKED",
];

const RUN_ORDER = ["RUN_ACTIVE", "RUN_PAUSED_BUDGET"];

function orderStates(rows: readonly TransitionRow[], fixed: readonly string[]): string[] {
  const present = new Set<string>();
  for (const r of rows) {
    present.add(r.from);
    present.add(r.to);
  }
  const out = fixed.filter((s) => present.has(s));
  for (const r of rows) for (const s of [r.from, r.to]) if (!out.includes(s)) out.push(s);
  return out;
}

function boxLines(name: string, inner: number, marker: string, annotation: string): string[] {
  const bar = `+${"-".repeat(inner)}+`;
  const body = `| ${name.padEnd(inner - 1)}|`;
  return [`${marker}${bar}`, `  ${body}${annotation ? `  ${annotation}` : ""}`, `  ${bar}`];
}

function edgeLine(row: TransitionRow, triggerWidth: number, toWidth: number, guardWhenShared: boolean): string {
  const label = guardWhenShared ? `${row.trigger} (${row.guardName})` : row.trigger;
  return `      +- ${label.padEnd(triggerWidth)} -> ${row.to.padEnd(toWidth)}  [${row.id}]`;
}

/** The phase state machine as an ASCII chart: each state a box (marked `>`
 * when it is the current state) carrying how often it was entered, how long
 * was spent in it and, for the states that dispatch agents, their role(s) and
 * model. Each box lists its outgoing edges, labelled with their trigger and
 * tagged with the `TRANSITIONS` row id that drew them. Rare edges (crash
 * recovery and launch failure) are folded under a footer so the graph stays
 * readable; the run-axis rows get their own small section. */
export function renderPhaseChart(
  rows: readonly TransitionRow[] = TRANSITIONS,
  opts: { stats?: ChartStats; model?: string; models?: ChartModels; now?: Date } = {},
): string {
  const stats = opts.stats ?? { entries: {}, timeMs: {} };
  const fallback = opts.model && opts.model.length > 0 ? opts.model : "default";
  const modelFor = (role: "worker" | "reviewer" | "evaluator") => opts.models?.[role] ?? fallback;
  const phaseRows = rows.filter((r) => r.axis === "phase");
  const runRows = rows.filter((r) => r.axis === "run");
  const lines: string[] = [];
  lines.push("tradeoffs-trace phase chart — generated from TRANSITIONS (src/core/transitions.ts); do not edit");
  lines.push(`current state: ${stats.current ?? "?"}${stats.current ? `   (entered ${stats.entries[stats.current] ?? 0}x - ${formatMs(stats.timeMs[stats.current])})` : ""}`);
  if (stats.currentRun) lines.push(`current run: ${stats.currentRun}`);
  lines.push("");

  const section = (
    title: string,
    sectionRows: readonly TransitionRow[],
    order: readonly string[],
    foldRare: boolean,
    axis: "phase" | "run",
  ) => {
    lines.push(`${title}:`);
    const states = orderStates(sectionRows, order);
    const drawn = foldRare ? sectionRows.filter((r) => !isRare(r)) : sectionRows;
    const rare = foldRare ? sectionRows.filter(isRare) : [];
    const inner = Math.max(...states.map((s) => s.length), 8) + 2;
    const triggerWidth = Math.max(0, ...drawn.map((r) => r.trigger.length));
    const toWidth = Math.max(0, ...drawn.map((r) => r.to.length));
    const currentName = axis === "phase" ? stats.current : stats.currentRun;
    const shareKey = new Map<string, number>();
    for (const r of drawn) shareKey.set(`${r.from}\0${r.trigger}`, (shareKey.get(`${r.from}\0${r.trigger}`) ?? 0) + 1);
    for (const state of states) {
      const marker = currentName === state ? "> " : "  ";
      const bits: string[] = [];
      if (axis === "phase") {
        const entered = stats.entries[state] ?? 0;
        if (entered === 0) bits.push(state === stats.current ? "current, not yet counted" : "not entered");
        else bits.push(`entered ${entered}x - ${formatMs(stats.timeMs[state])}`);
      } else {
        // The timeline records phase arrivals only, so the run budget rows are
        // never given counts that would read as "not entered" when the run is
        // plainly active (finding M-5).
        bits.push(state === currentName ? "current" : "not current");
      }
      const dispatch = DISPATCH_ROLES[state];
      if (dispatch) bits.push(`${dispatch.roles} - model ${modelFor(dispatch.model)}`);
      lines.push(...boxLines(state, inner, marker, bits.join("  -  ")));
      const outgoing = drawn.filter((r) => r.from === state);
      for (const r of outgoing) {
        lines.push(edgeLine(r, triggerWidth, toWidth, (shareKey.get(`${r.from}\0${r.trigger}`) ?? 0) > 1));
      }
      lines.push("");
    }
    if (foldRare && rare.length > 0) {
      lines.push("rare edges (crash recovery, launch failure):");
      const rw = Math.max(...rare.map((r) => r.trigger.length));
      const tw = Math.max(...rare.map((r) => r.to.length));
      for (const r of rare) lines.push(`  ${r.from} -- ${r.trigger.padEnd(rw)} -> ${r.to.padEnd(tw)}  [${r.id}]`);
      lines.push("");
    }
  };

  section("phase states", phaseRows, PHASE_ORDER, true, "phase");
  section("run states", runRows, RUN_ORDER, false, "run");
  return lines.join("\n");
}

const MESSAGE_ORDER = ["none", "raw", "published", "merged", "dropped", "accepted", "refused", "resolved", "superseded"];

/** The message state machine as an ASCII chart, from `MESSAGE_TRANSITIONS`.
 * Every row is an edge tagged with its row id, so the contract document and
 * the runbook can quote a chart that covers the whole table. */
export function renderMessageChart(rows: readonly MessageTransitionRow[] = MESSAGE_TRANSITIONS): string {
  const lines: string[] = [];
  lines.push("tradeoffs-trace message chart — generated from MESSAGE_TRANSITIONS (src/core/messages.ts); do not edit");
  lines.push("");
  const states = MESSAGE_ORDER.filter((s) => rows.some((r) => r.from === s || r.to === s));
  for (const r of rows) for (const s of [r.from, r.to]) if (!states.includes(s)) states.push(s);
  const inner = Math.max(...states.map((s) => s.length), 8) + 2;
  const triggerWidth = Math.max(...rows.map((r) => r.trigger.length));
  const toWidth = Math.max(...rows.map((r) => r.to.length));
  const shareKey = new Map<string, number>();
  for (const r of rows) shareKey.set(`${r.from}\0${r.trigger}`, (shareKey.get(`${r.from}\0${r.trigger}`) ?? 0) + 1);
  for (const state of states) {
    lines.push(...boxLines(state, inner, "  ", ""));
    for (const r of rows.filter((x) => x.from === state)) {
      const label = (shareKey.get(`${r.from}\0${r.trigger}`) ?? 0) > 1 ? `${r.trigger} (${r.guardName})` : r.trigger;
      lines.push(`      +- ${label.padEnd(triggerWidth + 12)} -> ${r.to.padEnd(toWidth)}  [${r.id}]`);
    }
    lines.push("");
  }
  return lines.join("\n");
}

const NODE_MARK: Record<string, string> = {
  waiting: ".",
  running: ">",
  "needs-you": "!",
  stopped: "o",
  done: "v",
  blocked: "x",
};

/** The program dependency graph as an ASCII chart: one box per node, carrying
 * its readable id (`<program>-NN`), its entry, its node state and, while it
 * runs, the phase state of its run. Under each box, the `after` edges name
 * the nodes it waits for — a join lists several. */
export function renderProgramChart(
  program: { title: string; maxParallel?: number; readableIds?: Record<string, string> },
  nodes: readonly ProgramNode[],
  state: ProgramState,
  opts: { programId: string; phaseOf?: Record<string, string> } = { programId: "" },
): string {
  const readable = (n: ProgramNode, i: number) =>
    program.readableIds?.[n.id] ?? `${opts.programId}-${String(i + 1).padStart(2, "0")}`;
  const lines: string[] = [];
  lines.push(`program ${opts.programId} — ${program.title}`);
  lines.push(`${nodes.length} node${nodes.length === 1 ? "" : "s"} · max ${program.maxParallel ?? 1} in parallel`);
  lines.push("");
  const inner = Math.max(...nodes.map((n, i) => `${readable(n, i)}  ${n.id}`.length), 16) + 2;
  const bar = `+${"-".repeat(inner)}+`;
  for (let i = 0; i < nodes.length; i++) {
    const n = nodes[i];
    const s = state.nodes[n.id] ?? { status: "waiting" as const };
    const label = `${readable(n, i)}  ${n.id}`;
    const bits = [s.status];
    const phase = s.status === "running" ? opts.phaseOf?.[n.id] : undefined;
    if (phase) bits.push(phase);
    const marker = s.status === "running" ? "> " : "  ";
    lines.push(`${marker}${bar}`);
    lines.push(`  | ${label.padEnd(inner - 1)}|  ${bits.join(" - ")}`);
    lines.push(`  ${bar}`);
    lines.push(`      after: ${n.deps.length > 0 ? n.deps.map((d) => readable(nodes.find((x) => x.id === d)!, nodes.findIndex((x) => x.id === d))).join(", ") : "(none)"}`);
    if (s.reason) lines.push(`      ${s.reason}`);
    if (s.runId) lines.push(`      run: ${s.runId}`);
    lines.push("");
  }
  return lines.join("\n");
}
