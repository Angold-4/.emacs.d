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
import { MAIN_PATH, TRANSITIONS } from "./core/transitions.ts";

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

/** The models the chart may show, one per role, plus the per-seat models
 * (#+TT_MODELS). A role or seat with no model (Pi's settings unread, or a
 * test) reads as `default`. A plan that only sets the four flat roles leaves
 * the seat maps empty, so the chart keeps its per-role line unchanged. */
export interface ChartModels {
  worker?: string;
  reviewer?: string;
  evaluator?: string;
  panel?: string;
  reviewerSeats?: Partial<Record<"M" | "A" | "B", string>>;
  panelSeats?: Partial<Record<string, string>>;
}
type ModelRole = "worker" | "reviewer" | "evaluator" | "panel";

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
  const modelFor = (role: ModelRole) => opts.models?.[role] ?? fallback;
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
      if (dispatch) {
        // Per-seat models (#+TT_MODELS): when M/A/B (or the panel seats) run
        // on different models, name each seat's own — that disagreement is
        // the point of three seats. When every seat resolves to the same
        // model (a plan that only sets the four flat roles), the box keeps
        // its single per-role line, exactly as before per-seat models.
        const reviewerSeat = (seat: "M" | "A" | "B") => opts.models?.reviewerSeats?.[seat] ?? modelFor("reviewer");
        const panelSeat = (seat: string) => opts.models?.panelSeats?.[seat] ?? modelFor("panel");
        if (state === "REVIEWING") {
          const seats = (["M", "A", "B"] as const).map((s) => [s, reviewerSeat(s)] as const);
          bits.push(
            seats.every(([, m]) => m === seats[0][1])
              ? `${dispatch.roles} - model ${seats[0][1]}`
              : seats.map(([s, m]) => `${s} ${m}`).join(" · "),
          );
        } else if (state === "EVALUATING") {
          const evaluatorModel = modelFor("evaluator");
          const panelSeats = (["1", "2", "3"] as const).map((s) => panelSeat(s));
          if (panelSeats.every((m) => m === panelSeats[0])) {
            const roles = `${dispatch.roles} - model ${evaluatorModel}`;
            // EVALUATING dispatches the evaluator and the panel; when the
            // panel has its own model, name it rather than showing the
            // evaluator's for a seat that ran on something else.
            bits.push(panelSeats[0] !== evaluatorModel ? `${roles} (panel model ${panelSeats[0]})` : roles);
          } else {
            bits.push(`evaluator ${evaluatorModel} · ${panelSeats.map((m, i) => `panel ${i + 1} ${m}`).join(" · ")}`);
          }
        } else {
          bits.push(`${dispatch.roles} - model ${modelFor(dispatch.model)}`);
        }
      }
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

// ---------------------------------------------------------------------------
// The live loop tape (plan 05h)
// ---------------------------------------------------------------------------

/** The phase state a tape reads. Structural, so charts.ts stays free of a
 * conductor import cycle. */
export interface LoopTapePhase {
  phase: string;
  attempt: { n: number; interrupted?: boolean };
  repairRoundsUsed: number;
  repairRoundsGranted: number;
  contract: { gate?: string };
  blockedReason?: string;
  ownerRequests?: Array<{ origin?: string; status?: string; linkedMessageId?: string }>;
}

/** Everything the tape needs: the phase's state timeline (one round starts at
 * the last new attempt), the current state, and the projection's own clock.
 * `endMs` is the last log timestamp, not a wall clock, so `tt contract
 * rebuild` reproduces the live file byte for byte. */
export interface LoopTapeInput {
  /** The phase's readable id and short title (`tapeLabel`), e.g. `05g seat models`. */
  label: string;
  /** The phase's current round (`view.round`). */
  round: number;
  phases: ReadonlyArray<{ phase: string; at: string }>;
  phase: LoopTapePhase;
  /** The run-axis state (`RUN_ACTIVE`/`RUN_PAUSED_BUDGET`). */
  run?: string;
  endMs: number;
  models?: ChartModels;
  /** Per-stage deadlines (`view.stageLimits`); the current row shows `x of y`. */
  limits?: Record<string, number>;
}

/** One drawn tape row. */
export interface LoopTapeRow {
  name: string;
  mark: string; // ✓ passed, ✗ failed, ▶ current, blank not reached
  time: string;
  annotation?: string;
  arrow?: string;
}

/** The row label each main-path state draws under. A state absent here keeps
 * its own name (BASELINE, DONE), so a state added to MAIN_PATH still gets a
 * row instead of being silently dropped. */
const TAPE_STEP_NAME: Record<string, string> = {
  IMPLEMENTING: "IMPLEMENT",
  FREEZING: "FREEZE",
  CHECKING: "CHECKS",
  PROBING: "PROBE",
  REVIEWING: "REVIEW",
  EVALUATING: "EVALUATE",
  RESOLVING: "RESOLVE",
  GATING: "GATE",
  ACCEPTED: "PUBLISH",
  PUBLISHING: "PUBLISH",
};

/** READY is the state before the first step; the tape has no row for it. */
const TAPE_SKIP_STATE = new Set(["READY"]);

/** The rows the tape draws, in order, DERIVED from the declared `MAIN_PATH`
 * (`src/core/transitions.ts`) — never a second hand-kept list. States that
 * fold to one row (ACCEPTED into PUBLISH) are merged, so the tape can only
 * ever draw the declared path. */
export const TAPE_STEPS: ReadonlyArray<{ name: string; states: readonly string[] }> = (() => {
  const out: Array<{ name: string; states: string[] }> = [];
  for (const state of MAIN_PATH) {
    if (TAPE_SKIP_STATE.has(state)) continue;
    const name = TAPE_STEP_NAME[state] ?? state;
    const last = out[out.length - 1];
    if (last && last.name === name) last.states.push(state);
    else out.push({ name, states: [state] });
  }
  return out;
})();

/** Which row a phase state belongs to, from the same derived list. REPAIRING,
 * AWAITING_OWNER, BLOCKED and the run-axis states are off the main path and
 * have none. */
const TAPE_STEP_OF_STATE: Record<string, string> = {};
for (const step of TAPE_STEPS) for (const state of step.states) TAPE_STEP_OF_STATE[state] = step.name;

const TAPE_STEP_LIMIT: Record<string, string> = {
  BASELINE: "baseline",
  IMPLEMENT: "implement",
  FREEZE: "freeze",
  CHECKS: "checks",
  PROBE: "probe",
  REVIEW: "review",
  EVALUATE: "evaluate",
  GATE: "gate",
};

const TAPE_OFF_PATH = new Set(["REPAIRING", "AWAITING_OWNER", "BLOCKED"]);

/** A new round begins at an IMPLEMENTING whose predecessor is NOT the front
 * of the same attempt (READY), the base baseline, or the attempt itself — so
 * a repair (REPAIRING -> IMPLEMENTING) or a criterion amendment
 * (RESOLVING -> IMPLEMENTING) starts a FRESH round, matching `view.round`,
 * which increments at each frozen candidate. FREEZING is included defensively:
 * a stray IMPLEMENTING after a freeze would not be a new attempt. */
const TAPE_ROUND_CONTINUES = new Set(["READY", "BASELINE", "IMPLEMENTING", "FREEZING"]);

/** `05h loop tape`: the readable id (the phase id without its `tt-` prefix and
 * with its first token kept whole) and the id's tail as the short title. */
export function tapeLabel(phaseId: string): string {
  const tail = phaseId.replace(/^tt-/, "");
  const parts = tail.split("-");
  return parts.length <= 1 ? tail : `${parts[0]} ${parts.slice(1).join(" ")}`;
}

/** Tape durations: whole minutes drop the seconds (`7m`), so the row stays
 * readable; the header's hour form is `1h04m`, as in the owner's layout. */
function formatTapeMs(ms: number): string {
  const s = Math.max(0, Math.round(ms / 1000));
  if (s < 60) return `${s}s`;
  const m = Math.floor(s / 60);
  if (m < 60) return s % 60 === 0 ? `${m}m` : `${m}m${String(s % 60).padStart(2, "0")}s`;
  return `${Math.floor(m / 60)}h${String(m % 60).padStart(2, "0")}m`;
}

function tapeModel(models: ChartModels | undefined, role: ModelRole): string {
  return models?.[role] ?? "default";
}

function tapeSeat(models: ChartModels | undefined, role: "reviewer" | "panel", seat: string): string {
  const own = role === "reviewer" ? models?.reviewerSeats?.[seat as "M" | "A" | "B"] : models?.panelSeats?.[seat];
  return own ?? tapeModel(models, role);
}

/** Who is on the current step, and on which model, matching the chart's own
 * per-seat resolution. Only the three dispatching steps name anyone. */
function tapeWorking(step: string, models: ChartModels | undefined): string | undefined {
  if (step === "IMPLEMENT") return `worker · ${tapeModel(models, "worker")}`;
  if (step === "REVIEW") {
    const seats = (["M", "A", "B"] as const).map((s) => [s, tapeSeat(models, "reviewer", s)] as const);
    return seats.every(([, m]) => m === seats[0][1])
      ? `M·A·B · ${seats[0][1]}`
      : seats.map(([s, m]) => `${s} ${m}`).join(" · ");
  }
  if (step === "EVALUATE") {
    const evaluator = tapeModel(models, "evaluator");
    const panel = tapeSeat(models, "panel", "1");
    return panel !== evaluator ? `evaluator · ${evaluator} · panel · ${panel}` : `evaluator · ${evaluator}`;
  }
  return undefined;
}

/** The plain-words reason the head carries when the round is off the main
 * path: a repair, the owner's desk, a block or a run-budget pause. */
function tapeOffPathReason(input: LoopTapeInput): string | undefined {
  if (input.run === "RUN_PAUSED_BUDGET") return "paused (run budget)";
  switch (input.phase.phase) {
    case "REPAIRING":
      return `repairing, attempt ${input.phase.attempt.n}/${input.phase.repairRoundsGranted}`;
    case "AWAITING_OWNER": {
      const escalated = (input.phase.ownerRequests ?? []).find(
        (r) => r.status === "open" && r.origin === "blocker_panel" && r.linkedMessageId,
      );
      return escalated ? `waiting for you · panel escalated ${escalated.linkedMessageId}` : "waiting for you";
    }
    case "BLOCKED":
      return input.phase.blockedReason ? `blocked · ${input.phase.blockedReason}` : "blocked";
    default:
      return undefined;
  }
}

/** Build the tape's header and rows from the phase timeline. Pure. */
export function buildLoopTape(input: LoopTapeInput): { header: string; rows: LoopTapeRow[]; head?: LoopTapeRow } {
  const { phases, phase } = input;
  // The current round starts at the last new attempt (an IMPLEMENTING after a
  // round-ending state), or at the top for round one, so BASELINE is included.
  let startIdx = 0;
  for (let i = 1; i < phases.length; i++) {
    if (phases[i].phase === "IMPLEMENTING" && !TAPE_ROUND_CONTINUES.has(phases[i - 1].phase)) startIdx = i;
  }
  const slice = phases.slice(startIdx);

  const times: Record<string, number> = {};
  const seen = new Set<string>();
  let lastMain: string | undefined;
  for (let i = 0; i < slice.length; i++) {
    const step = TAPE_STEP_OF_STATE[slice[i].phase];
    const end = i + 1 < slice.length ? Date.parse(slice[i + 1].at) : input.endMs;
    const ms = Math.max(0, end - Date.parse(slice[i].at));
    if (!step) continue;
    times[step] = (times[step] ?? 0) + ms;
    seen.add(step);
    lastMain = step;
  }

  const runPaused = input.run === "RUN_PAUSED_BUDGET";
  const offPath = TAPE_OFF_PATH.has(phase.phase) || runPaused;
  // Off the path, the head stays on the step the round left from; REPAIRING
  // and BLOCKED are a failure (`✗`), a pause or the owner's desk are not (`▶`).
  const currentStep = offPath ? lastMain : TAPE_STEP_OF_STATE[phase.phase];
  const headMark = offPath ? (phase.phase === "AWAITING_OWNER" || runPaused ? "▶" : "✗") : "▶";
  const wholeRunBaseline = phases.some((p) => p.phase === "BASELINE");
  const displayed = TAPE_STEPS.filter((s) =>
    s.name === "BASELINE" ? wholeRunBaseline : s.name === "GATE" ? Boolean(phase.contract.gate) : true,
  );
  const ci = currentStep ? displayed.findIndex((s) => s.name === currentStep) : -1;
  const arrow = offPath ? tapeOffPathReason(input) : undefined;

  const rows = displayed.map((s, k): LoopTapeRow => {
    const ms = times[s.name];
    let mark = " ";
    if (k === ci && ci >= 0) mark = headMark;
    else if (k < ci) mark = seen.has(s.name) || (s.name === "BASELINE" && wholeRunBaseline) ? "✓" : " ";
    let time = "";
    if (mark === "✓" || mark === "✗") time = ms === undefined ? "" : formatTapeMs(ms);
    else if (mark === "▶" && ms !== undefined) {
      const limitKey = TAPE_STEP_LIMIT[s.name];
      const limit = limitKey ? input.limits?.[limitKey] : undefined;
      time = limit === undefined ? formatTapeMs(ms) : `${formatTapeMs(ms)} of ${formatTapeMs(limit)}`;
    }
    return {
      name: s.name,
      mark,
      time,
      // Off the path nobody is on the step any more: the arrow names the
      // reason instead of a model.
      annotation: mark === "▶" && !offPath ? tapeWorking(s.name, input.models) : undefined,
      arrow: k === ci ? arrow : undefined,
    };
  });

  const firstAt = phases[0]?.at;
  const elapsed = firstAt ? formatTapeMs(Math.max(0, input.endMs - Date.parse(firstAt))) : "0s";
  const header = `${input.label} · round ${input.round} · attempt ${phase.attempt.n}/${phase.repairRoundsGranted} · ${elapsed}`;
  return { header, rows, head: ci >= 0 ? rows[ci] : undefined };
}

/** `views/tape.txt`: the current round as a vertical tape, one main-path step
 * per row, the head `▶` (or `✗`) on the step the phase is in, with its elapsed
 * time against the stage limit and who is on which model. Off the main path
 * the head carries `→` and the reason in plain words. */
export function renderLoopTape(input: LoopTapeInput): string {
  const { header, rows } = buildLoopTape(input);
  const lines = [header, ""];
  for (const r of rows) {
    let line = `  ${r.mark}  ${r.name.padEnd(12)}`;
    if (r.time) line += r.time;
    if (r.annotation) line += `   ${r.annotation}`;
    if (r.arrow) line += `  → ${r.arrow}`;
    lines.push(line.replace(/ +$/, ""));
  }
  return `${lines.join("\n")}\n`;
}

/** The current step's cell, one line: the status buffer's `loop` row and the
 * one thing the owner reads at a glance. Undefined before the first step. */
export function loopTapeHead(input: LoopTapeInput): string | undefined {
  const { head } = buildLoopTape(input);
  if (!head || head.mark === " ") return undefined;
  let line = `${head.mark} ${head.name}`;
  if (head.time) line += ` ${head.time}`;
  if (head.annotation) line += ` · ${head.annotation}`;
  if (head.arrow) line += ` → ${head.arrow}`;
  return line;
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
