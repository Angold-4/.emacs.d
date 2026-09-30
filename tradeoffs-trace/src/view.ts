// Plan 3b: the owner-facing view of a run — computed here once, rendered by
// Emacs (core/init-tradeoffs-trace.el) and `tt status`/`tt timing`/`tt list`
// as plain strings. Everything is derived from the run directory: the control
// log (via rebuildTimeline) and the agents' stream files. Nothing here moves
// run state.

import * as fs from "node:fs";
import * as path from "node:path";

import { DEFAULT_DEADLINES, rebuildTimelineWithEvents, runPaths, type RunPlanFile, type Timeline } from "./conductor.ts";
import { loopTapeHead, renderLoopTape, tapeLabel, type ChartModels } from "./charts.ts";
import { effectiveChecks } from "./core/checks.ts";
import { planModelSelector, type PlanModels, type RoleModel } from "./core/roles.ts";
// Plan 01f: the gate stage's own record (the conductor's live proof).
import { gateOutcomeText, parseGateRecord, type GateRecord } from "./core/gate.ts";
import { decisionStatus, isLiveDecision } from "./core/predicate.ts";
import { baselineCoversCommands, baselineStatusLine, parseBaseline, type Baseline } from "./core/test-failures.ts";
// Plan 05i: the environment block's one-line reason and the resolved tool rows.
import { envBlockedLine, envToolsLines } from "./core/env-preflight.ts";
import { notAcceptedReasons, reviewerOutcomes, tradeoffEntries, type ReviewerOutcome, type TradeoffEntry } from "./core/verdict.ts";
// Plan 05c: the status view's `review' row is counted from the same messages
// the review renderer shows, so the two views never disagree.
import { reviewSummary } from "./render.ts";
import { computeMetrics, flakesLine, metricsLine, metricsSummary, timelineEndMs, type PhaseMetrics } from "./metrics.ts";
import type { PhaseState } from "./core/types.ts";

// ---------------------------------------------------------------------------
// Pipeline
// ---------------------------------------------------------------------------

const STAGE_OF: Record<string, string> = {
  READY: "implement",
  // Plan 04a: the base baseline is its own stage, never counted as implement.
  BASELINE: "baseline",
  IMPLEMENTING: "implement",
  REPAIRING: "implement",
  FREEZING: "freeze",
  CHECKING: "checks",
  PROBING: "probe",
  REVIEWING: "review",
  EVALUATING: "evaluate",
  RESOLVING: "resolve",
  GATING: "gate",
  ACCEPTED: "publish",
  PUBLISHING: "publish",
  AWAITING_OWNER: "needs you",
  DONE: "DONE",
  BLOCKED: "BLOCKED",
};

/** The stages the conductor runs itself: no agent is expected to be active,
 * so "no agent activity" there is not idleness. */
export const CONDUCTOR_STAGES = new Set(["baseline", "freeze", "checks", "probe", "evaluate", "resolve", "gate", "publish"]);

function stageDeadlineMs(): Record<string, number> {
  return {
    baseline: DEFAULT_DEADLINES.checkMs,
    implement: DEFAULT_DEADLINES.workerAttemptMs,
    freeze: DEFAULT_DEADLINES.freezeMs,
    checks: DEFAULT_DEADLINES.checkMs,
    probe: DEFAULT_DEADLINES.probeMs,
    gate: DEFAULT_DEADLINES.gateMs,
    review: DEFAULT_DEADLINES.reviewMs,
    evaluate: DEFAULT_DEADLINES.evaluateMs,
  };
}

/** The stage limits the pipeline line counts down, with the plan's own
 * `#+TT_*_MINUTES` overrides applied (a plan that sets a 45-minute gate must
 * not be shown as "over by" while the conductor is running within it). */
export function stageLimits(plan: Pick<RunPlanFile, "deadlines">): Record<string, number> {
  const d = { ...DEFAULT_DEADLINES, ...(plan.deadlines ?? {}) };
  return {
    baseline: d.checkMs,
    implement: d.workerAttemptMs,
    freeze: d.freezeMs,
    checks: d.checkMs,
    probe: d.probeMs,
    gate: d.gateMs,
    review: d.reviewMs,
    evaluate: d.evaluateMs,
  };
}

export interface StageSpan {
  stage: string;
  startedAt: string;
  ms: number;
  failed: boolean; // the stage ended by sending the phase back to implement
  current: boolean;
}

/** `lastEventAt`: when the conductor is not running, the current stage
 * stops counting at the last logged event instead of running on. */
export function stageSpans(timeline: Timeline, now: Date, lastEventAt?: string): StageSpan[] {
  const spans: StageSpan[] = [];
  for (const p of timeline.phases) {
    const stage = STAGE_OF[p.phase] ?? p.phase.toLowerCase();
    const last = spans[spans.length - 1];
    if (last && last.stage === stage) continue;
    if (last) {
      last.ms = Date.parse(p.at) - Date.parse(last.startedAt);
      last.current = false;
      // A gate or review that hands the phase back to implement failed. A
      // baseline -> implement step is the normal path, not a failure.
      if (stage === "implement" && last.stage !== "implement" && last.stage !== "baseline") last.failed = true;
    }
    spans.push({ stage, startedAt: p.at, ms: 0, failed: false, current: true });
  }
  const last = spans[spans.length - 1];
  // The current stage counts from the last restart inside it: an interrupted
  // attempt that was re-dispatched has a fresh deadline.
  const restart = (timeline.restarts ?? []).filter((t) => last && Date.parse(t) >= Date.parse(last.startedAt)).pop();
  if (last && restart) last.startedAt = restart;
  if (last) {
    if (last.stage === "DONE" || last.stage === "BLOCKED") last.current = false;
    else {
      last.ms = (lastEventAt ? Date.parse(lastEventAt) : now.getTime()) - Date.parse(last.startedAt);
      if (lastEventAt) last.current = false;
    }
  }
  return spans;
}

/** #+TT_MODELS, in the order the roles appear in the pipeline, as
 * `role=provider:model` (provider only when the plan declared one). Empty
 * when the plan set no models, so a view can omit the line entirely.
 *
 * When the plan sets per-seat models, the reviewer and panel roles expand to
 * one entry per seat (`reviewer.M`, `panel.1`, …), each resolved with the
 * same selector the launch sites use — so the line names what each seat ran
 * on, not just the role's shared default. A plan that only sets the four flat
 * roles keeps the four-entry line unchanged. */
export function modelEntries(models: PlanModels | undefined): Array<{ role: string; value: string }> {
  if (!models) return [];
  const select = planModelSelector({ models });
  const out: Array<{ role: string; value: string }> = [];
  const push = (role: string, m: RoleModel | undefined): void => {
    if (!m) return;
    out.push({ role, value: m.provider ? `${m.provider}:${m.model ?? ""}` : m.model ?? "" });
  };
  push("worker", models.worker);
  const reviewerSeats = models.reviewerSeats ? Object.keys(models.reviewerSeats).length > 0 : false;
  if (reviewerSeats) {
    for (const seat of ["M", "A", "B"] as const) push(`reviewer.${seat}`, select("reviewer", seat));
  } else {
    push("reviewer", models.reviewer);
  }
  push("evaluator", models.evaluator);
  const panelSeats = models.panelFrom === "reviewers" || (models.panelSeats ? Object.keys(models.panelSeats).length > 0 : false);
  if (panelSeats) {
    for (const seat of ["1", "2", "3"] as const) push(`panel.${seat}`, select("panel", seat));
  } else {
    push("panel", models.panel);
  }
  return out;
}

/** The models row's value: one `role=provider:model` per role/seat, or
 * undefined when the plan set none (a run that uses Pi's `defaultModel` needs
 * no line). The status view pads the `models` label itself, so the value
 * carries no `models:` prefix. */
export function modelsLineText(models: PlanModels | undefined): string | undefined {
  const entries = modelEntries(models);
  return entries.length > 0 ? entries.map((e) => `${e.role}=${e.value}`).join(" ") : undefined;
}

/** The plan's own models in the shape `renderLoopTape` reads. Only what the
 * plan declares enters the tape, never Pi's settings, so the live file and a
 * `tt contract rebuild` agree; a role with no declared model reads `default`. */
export function chartModelsFromPlan(models: PlanModels | undefined): ChartModels | undefined {
  if (!models) return undefined;
  const select = planModelSelector({ models });
  const name = (m: RoleModel | undefined): string | undefined => m?.model;
  return {
    worker: name(select("worker")),
    reviewer: name(select("reviewer")),
    evaluator: name(select("evaluator")),
    panel: name(select("panel")),
    reviewerSeats: { M: name(select("reviewer", "M")), A: name(select("reviewer", "A")), B: name(select("reviewer", "B")) },
    panelSeats: { "1": name(select("panel", 1)), "2": name(select("panel", 2)), "3": name(select("panel", 3)) },
  };
}

export function formatDuration(ms: number): string {
  const s = Math.max(0, Math.round(ms / 1000));
  if (s < 60) return `${s}s`;
  const m = Math.floor(s / 60);
  if (m < 60) return `${m}m${String(s % 60).padStart(2, "0")}s`;
  return `${Math.floor(m / 60)}h${String(m % 60).padStart(2, "0")}m`;
}

/** "implement 30s → freeze 1s → checks ✗ 10m00s → implement 26m → … →
 * review 12s… (14m48s left)". Attempts after the first are numbered. */
export function pipelineLine(spans: StageSpan[], limits: Record<string, number> = stageDeadlineMs()): string {
  let attempt = 0;
  const parts = spans.map((s) => {
    let name = s.stage;
    if (s.stage === "implement") {
      attempt += 1;
      if (attempt > 1) name = `implement#${attempt}`;
    }
    if (s.stage === "DONE" || s.stage === "BLOCKED") return s.stage;
    const d = formatDuration(s.ms);
    if (s.current) {
      const limit = limits[s.stage];
      const left =
        limit === undefined ? "" : limit >= s.ms ? ` (${formatDuration(limit - s.ms)} left)` : ` (over by ${formatDuration(s.ms - limit)})`;
      return `${name} ${d}…${left}`;
    }
    return `${name}${s.failed ? " ✗" : ""} ${d}`;
  });
  // Long runs: keep the first stage and the most recent ones readable.
  if (parts.length > 9) return [parts[0], "…", ...parts.slice(-7)].join(" → ");
  return parts.join(" → ");
}

// ---------------------------------------------------------------------------
// Agent time (the reference behaviour is ~/.tradeoffs-trace/tools/tt-timing)
// ---------------------------------------------------------------------------

export type TimeCategory = "polling" | "full tests" | "narrow tests" | "file writes" | "reads/greps" | "other";

export interface ToolCall {
  name: string;
  arg: string;
  startedAt: string;
  ms: number;
  running: boolean;
  category: TimeCategory;
}

export interface AgentTime {
  agentId: string;
  startedAt: string;
  lastAt: string;
  elapsedMs: number;
  toolMs: number;
  modelMs: number;
  byCategory: Record<TimeCategory, { calls: number; ms: number }>;
  slowest: ToolCall[];
  running?: ToolCall;
}

export function categorize(name: string, arg: string): TimeCategory {
  if (name === "read" || name === "grep" || name === "ls" || name === "find") return "reads/greps";
  if (name === "edit" || name === "write") return "file writes";
  if (name !== "sh" && name !== "bash") return "other";
  if (/\bsleep\b|\btail -f\b/.test(arg)) return "polling";
  if (/make\b.*\b(check|test|crash|live)\b/.test(arg) || (/node --test/.test(arg) && /\*\*|crash/.test(arg))) return "full tests";
  if (/node --test|\bert-run-tests|npm (run )?test/.test(arg)) return "narrow tests";
  if (/cat >|tee |python3? -/.test(arg)) return "file writes";
  return "other";
}

function argOf(args: unknown): string {
  if (!args || typeof args !== "object") return "";
  const a = args as Record<string, unknown>;
  const v = a.command ?? a.path ?? a.pattern ?? "";
  return typeof v === "string" ? v : JSON.stringify(v);
}

/** Tool-call timing for one agent stream file. Reads line by line and skips
 * the (large, frequent) message_update records without parsing them. */
export function agentTime(file: string, now: Date, live: boolean): AgentTime | undefined {
  let text: string;
  try {
    text = fs.readFileSync(file, "utf8");
  } catch {
    return undefined;
  }
  let first: string | undefined;
  let last: string | undefined;
  const open = new Map<string, { name: string; arg: string; at: string }>();
  const calls: ToolCall[] = [];
  for (const line of text.split("\n")) {
    if (line.length === 0) continue;
    const tsMatch = line.match(/"ts":"([^"]+)"/);
    if (tsMatch) {
      first ??= tsMatch[1];
      last = tsMatch[1];
    }
    if (line.includes('"message_update"')) continue;
    if (!line.includes('"tool_execution_')) continue;
    let rec: { ts: string; event: { type: string; toolCallId: string; toolName?: string; args?: unknown } };
    try {
      rec = JSON.parse(line);
    } catch {
      continue;
    }
    const e = rec.event;
    if (e.type === "tool_execution_start") {
      open.set(e.toolCallId, { name: e.toolName ?? "?", arg: argOf(e.args), at: rec.ts });
    } else if (e.type === "tool_execution_end") {
      const s = open.get(e.toolCallId);
      if (!s) continue;
      open.delete(e.toolCallId);
      calls.push({
        name: s.name,
        arg: s.arg,
        startedAt: s.at,
        ms: Date.parse(rec.ts) - Date.parse(s.at),
        running: false,
        category: categorize(s.name, s.arg),
      });
    }
  }
  if (!first || !last) return undefined;
  const end = live ? now.getTime() : Date.parse(last);
  let running: ToolCall | undefined;
  for (const s of open.values()) {
    const call = {
      name: s.name,
      arg: s.arg,
      startedAt: s.at,
      ms: end - Date.parse(s.at),
      running: true,
      category: categorize(s.name, s.arg),
    };
    calls.push(call);
    if (live) running = call;
  }
  const byCategory = Object.fromEntries(
    (["polling", "full tests", "narrow tests", "file writes", "reads/greps", "other"] as TimeCategory[]).map((c) => [
      c,
      { calls: 0, ms: 0 },
    ]),
  ) as AgentTime["byCategory"];
  let toolMs = 0;
  for (const c of calls) {
    byCategory[c.category].calls += 1;
    byCategory[c.category].ms += c.ms;
    toolMs += c.ms;
  }
  const elapsedMs = Math.max(1, end - Date.parse(first));
  return {
    agentId: path.basename(file, ".jsonl"),
    startedAt: first,
    lastAt: last,
    elapsedMs,
    toolMs,
    modelMs: Math.max(0, elapsedMs - toolMs),
    byCategory,
    slowest: [...calls].sort((a, b) => b.ms - a.ms).slice(0, 5),
    running,
  };
}

const pct = (part: number, whole: number) => `${Math.round((100 * part) / Math.max(1, whole))}%`;

/** "worker-2: model 61% · polling 0% · full tests 12% · ⧗ $ node --test … 42s". */
export function timeLine(t: AgentTime): string {
  const role = t.agentId.replace(/-dispatch_.*$|-\d+$/, "");
  const parts = [
    `model ${pct(t.modelMs, t.elapsedMs)}`,
    `polling ${pct(t.byCategory.polling.ms, t.elapsedMs)}`,
    `full tests ${pct(t.byCategory["full tests"].ms, t.elapsedMs)}`,
  ];
  if (t.running) {
    const verb = t.running.name === "sh" ? "$" : t.running.name;
    const arg = t.running.arg.replace(/\s+/g, " ");
    parts.push(`⧗ ${verb} ${arg.length > 200 ? `${arg.slice(0, 199)}…` : arg} ${formatDuration(t.running.ms)}`);
  }
  const warn = t.byCategory.polling.ms > t.elapsedMs / 4 ? "  ⚠ polling over a quarter of the attempt" : "";
  return `${role}: ${parts.join(" · ")}${warn}`;
}

export function streamFiles(runDir: string): string[] {
  const dir = runPaths(runDir).stream;
  try {
    return fs
      .readdirSync(dir)
      .filter((f) => f.endsWith(".jsonl"))
      .map((f) => path.join(dir, f))
      .sort((a, b) => fs.statSync(a).mtimeMs - fs.statSync(b).mtimeMs);
  } catch {
    return [];
  }
}

// ---------------------------------------------------------------------------
// The view
// ---------------------------------------------------------------------------

export interface RunView {
  stage: string;
  stageElapsed: string;
  elapsed: string;
  pipeline: string;
  /** Checks/probe/reviews/gate for the current candidate only; while a repair
   * attempt implements they are "pending (round N)". A candidate whose checks
   * passed only because every failure was already on the base says so:
   * `checks ✓ (base has N failures)`. The gate is shown only for a phase
   * whose contract declares one (`gate ✓`, `gate ✓ (reused)`, `gate ✗`). */
  gates: string;
  /** Plan 01f: one line citing the conductor's gate record for the current
   * candidate (`tt status`/`tt summary`/the status buffer). Undefined when
   * the phase declares no gate or none has run yet. */
  gate?: string;
  /** Plan 01e: the base's own pre-existing check failures, when it has any
   * (`base fails: N tests: …`). Undefined when the base passes or no baseline
   * was taken. */
  baseline?: string;
  previousRound?: string;
  reviewers: ReviewerOutcome[];
  reviewLine: string;
  /** Why the phase did or did not accept, and what happens next. */
  verdict?: string;
  addressing: string[];
  rounds: Timeline["rounds"];
  round: number;
  time?: string;
  runningTool?: string;
  boundaryFilesChanged: number;
  liveDecisions: number;
  failedDecisions: number;
  /** Plan 01g: every amendment record of this phase, applied or reverted,
   * with its old → new wording. Undefined when there are none. */
  amendments?: string;
  /** Plan 01g: how many amendments are applied right now. */
  amendedCount: number;
  /** Reserved decisions on the current candidate: voted like any other,
   * flagged so the owner can look (and override through the input box). */
  flaggedDecisions: number;
  openFindings: number;
  /** Plan 05c: the status view's `review' row, in trade-off vocabulary
   * (`T 6 (6 raw) · F 0 · B 0 · C-c m d'), counted from the same messages
   * `views/review.org' renders. */
  review: string;
  needsYou: number;
  /** Plan 01h: the few trade-offs that matter while the run is live, most
   * important first, at most 6. Undefined when there is nothing to show. */
  tradeoffs?: TradeoffEntry[];
  /** Plan 01h: what the run costs so far (rounds, minutes, owner wait) and a
   * plain estimate for one more round from this phase's own history. */
  cost?: RunCost;
  /** Minutes since the active agent last produced an event (agent stages only). */
  idleMinutes?: number;
  attention?: string;
  /** Plan 04c: the balance metrics (review share, rounds, raw/published per
   * type, merge/drop rates, A/D, owner wait, blockers, unexposed proxy) and
   * the one status line that renders them. */
  metrics: PhaseMetrics;
  metricsLine: string;
  /** #+TT_MODELS: the `models` row's value (one `role=provider:model` per
   * role/seat), only when the plan set models (otherwise undefined — Pi's
   * defaultModel needs no line). */
  models?: string;
  /** Plan 05h: the tape's current-row cell (`▶ REVIEW 4m12s of 15m`), the
   * status buffer's one `loop` row. Undefined before the first step. */
  loop?: string;
  /** Plan 05i: the one `env blocked · …` line while the run's environment is
   * blocked (a missing tool, or a 126/127 at a check). Undefined otherwise. */
  envBlocked?: string;
  /** Plan 05i: `env  cargo /path` for every tool the preflight resolved at
   * start, so the owner can see which toolchain the run is using. */
  envTools: string[];
  /** Plan 05d: the `flakes` row (flakes per test, rounds saved, launch
   * retries). Undefined when there is nothing to show. */
  flakes?: string;
}

/** Plan 01h: what the run costs so far. `stageMinutes` excludes `needs you`,
 * which is reported separately as `ownerWaitMinutes`. */
export interface RunCost {
  /** Completed rounds plus the one in progress (the current candidate). */
  rounds: number;
  totalMinutes: number;
  stageMinutes: { stage: string; minutes: number }[];
  ownerWaitMinutes: number;
  /** Median minutes of this phase's completed rounds — the plain estimate
   * `next round ≈ N min`. Undefined when no round has completed yet. */
  nextRoundMinutes?: number;
  /** The one-line summary the status buffer and `tt program status` render. */
  text: string;
}

/** Round start times: every CHECKING phase entry that a FREEZING entry
 * immediately precedes is a candidate being reviewed (FREEZE_COMPLETED is
 * FREEZING → CHECKING). A CHECKING re-entry from AMEND/CRITERION_REVERTED is
 * preceded by some other stage and is not a new round. */
function roundStarts(timeline: Timeline): string[] {
  const starts: string[] = [];
  for (let i = 1; i < timeline.phases.length; i++) {
    if (timeline.phases[i].phase === "CHECKING" && timeline.phases[i - 1].phase === "FREEZING") {
      starts.push(timeline.phases[i].at);
    }
  }
  return starts;
}

function median(values: number[]): number {
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 1 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
}

/** Plan 01h: the cost meter. Rounds and per-stage minutes come from the
 * timeline's own stage spans; a completed round is the gap between one
 * candidate's freeze and the next (a round's checks, probe, review, resolve,
 * gate and repair implementation all count), and the phase's last round is
 * only complete once it ends DONE or BLOCKED. */
export function runCost(timeline: Timeline, spans: StageSpan[], now: Date): RunCost {
  const phase = timeline.state.phase;
  const round = timeline.rounds.length + (phase.candidate ? 1 : 0);
  const starts = roundStarts(timeline);
  const durations: number[] = [];
  for (let i = 0; i + 1 < starts.length; i++) durations.push(Date.parse(starts[i + 1]) - Date.parse(starts[i]));
  const ended = phase.phase === "DONE" || phase.phase === "BLOCKED";
  if (ended && starts.length > 0) {
    const last = timeline.phases[timeline.phases.length - 1]?.at;
    if (last) durations.push(Date.parse(last) - Date.parse(starts[starts.length - 1]));
  }
  const nextRoundMinutes = durations.length > 0 ? Math.round(median(durations) / 60_000) : undefined;

  const byStage = new Map<string, number>();
  for (const s of spans) byStage.set(s.stage, (byStage.get(s.stage) ?? 0) + s.ms);
  const ownerWaitMinutes = Math.round((byStage.get("needs you") ?? 0) / 60_000);
  byStage.delete("needs you");
  const stageMinutes = [...byStage.entries()]
    .map(([stage, ms]) => ({ stage, minutes: Math.round(ms / 60_000) }))
    .filter((s) => s.minutes > 0)
    .sort((a, b) => b.minutes - a.minutes);
  const totalMinutes = Math.round([...byStage.values()].reduce((a, b) => a + b, 0) / 60_000) + ownerWaitMinutes;

  const parts = [
    `${round} round${round === 1 ? "" : "s"}`,
    `${totalMinutes}m total`,
    ...stageMinutes.map((s) => `${s.stage} ${s.minutes}m`),
  ];
  if (ownerWaitMinutes > 0) parts.push(`owner wait ${ownerWaitMinutes}m`);
  if (nextRoundMinutes !== undefined) parts.push(`next round ≈ ${nextRoundMinutes} min`);
  return {
    rounds: round,
    totalMinutes,
    stageMinutes,
    ownerWaitMinutes,
    ...(nextRoundMinutes !== undefined ? { nextRoundMinutes } : {}),
    text: parts.join(" · "),
  };
}

export function buildView(
  runDir: string,
  plan: RunPlanFile,
  alive: boolean,
  now = new Date(),
): RunView & { timeline: Timeline; tape: string } {
  // One read of the control log gives both the timeline and the events the
  // balance metrics need, so nothing here parses the log twice.
  const { timeline, events } = rebuildTimelineWithEvents(runDir, plan);
  const phase = timeline.state.phase;
  const spans = stageSpans(timeline, now, alive ? undefined : lastEventAt(runDir));
  const current = spans[spans.length - 1];
  const stage = current?.stage ?? "implement";
  const repairing = (stage === "implement" && phase.attempt.n > 1) || stage === "needs you";
  const C = phase.candidate?.sha;
  const reviewers = reviewerOutcomes(phase);
  const reasons = notAcceptedReasons(phase);
  const round = timeline.rounds.length + (C ? 1 : 0);

  // Plan 01e: the base's own baseline, read from `<run>/checks/base/`; a
  // candidate's passing checks that leaned on it are shown as such, and the
  // base's failures get their own status line (D2). It is only trusted when it
  // covers the phase's *current* effective check list: after a contract
  // amendment changes a check, the gate (and the prompts) no longer use this
  // record, so the status must not claim a baseline-tolerated pass either.
  const record = readBaseline(runDir);
  const baseline = baselineCoversCommands(record, effectiveChecks(plan.checks, phase.contract.checks)) ? record : undefined;
  const gate = (r: { candidateSha: string; passed?: boolean } | undefined, reused = false) =>
    !r || r.candidateSha !== C ? "⧗" : r.passed === true ? `✓${reused ? " (reused)" : ""}` : r.passed === false ? "✗" : "⧗";
  /** ` (base has N failures)` only on the current candidate's passed checks,
   * when the base itself has pre-existing failures. */
  const baseNote = (r: { candidateSha: string; passed?: boolean } | undefined) =>
    C && r?.candidateSha === C && r.passed === true && baseline && baseline.failures.length > 0
      ? ` (base has ${baseline.failures.length} failures)`
      : "";
  // Plan 01f: the gate's own record for the current candidate, read from
  // disk like the baseline (the conductor writes it; nothing here moves run
  // state).
  const gateRecord = readGateRecord(runDir, C);
  const gateDeclared = Boolean(phase.contract.gate);
  const gateTag = !gateDeclared
    ? ""
    : !C || gateRecord?.candidateSha !== C
      ? " · gate ⧗"
      : ` · gate ${gateRecord.passed ? `✓${gateRecord.reused ? " (reused)" : ""}` : "✗"}`;
  let gates: string;
  let previousRound: string | undefined;
  if (repairing && stage === "implement") {
    gates = `pending (round ${round + 1})`;
    previousRound = C ? `round ${round} · ${C.slice(0, 7)} · ${reasons.length > 0 ? `not accepted: ${reasons.join("; ")}` : "not accepted"}` : undefined;
  } else {
    gates = C
      ? `checks ${gate(phase.checks)}${baseNote(phase.checks)} · probe ${gate(phase.probe, probeReused(runDir, C))}${gateTag}`
      : "no candidate yet";
  }

  let verdict: string | undefined;
  if (phase.phase === "DONE") verdict = "accepted and published";
  else if (phase.phase === "BLOCKED") verdict = `blocked: ${phase.blockedReason ?? "see the log"}`;
  else if (reasons.length > 0) {
    const next =
      phase.phase === "AWAITING_OWNER"
        ? "needs you (type a correction below)"
        : stage === "implement"
          ? `repair attempt ${phase.attempt.n}`
          : stage;
    verdict = `not accepted: ${reasons.join("; ")} → ${next}`;
  } else if (phase.phase === "AWAITING_OWNER") verdict = "needs you (type a correction below)";

  const files = streamFiles(runDir);
  const agentStage = stage === "implement" || stage === "review";
  const activeFile = files[files.length - 1];
  const t = activeFile && alive && agentStage ? agentTime(activeFile, now, true) : undefined;
  const idleMinutes = t ? (now.getTime() - Date.parse(t.lastAt)) / 60_000 : undefined;
  const needsYou = phase.ownerRequests.filter((r) => r.status === "open").length;
  const live = phase.decisions.filter((d) => isLiveDecision(d) && d.source !== "trigger");
  const failed = live.filter((d) => decisionStatus(d, phase).status === "failed").length;
  const flagged = live.filter((d) => d.class === "reserved" && d.boundCandidateSha === C).length;
  const openFindings = phase.findings.filter((f) => f.status === "open").length;
  // Plan 01g: every amendment (applied or reverted) is shown as
  // `⚑ AMENDED id: old → new`, so the reworded contract is visible in the
  // status, the decision view and `tt summary` alike.
  // Only live records: a sibling proposal superseded by another is not a
  // pending amendment and must not read PROPOSED (01g advisory B).
  const amendments = phase.decisions.filter((d) => d.amendment && isLiveDecision(d));
  // The arrow always names what the contract moved FROM → TO: an applied
  // amendment replaced the old criterion, a reverted one restored it, so the
  // rendering never claims replacement wording is in force after a revert
  // (finding A-14).
  const amendmentLine =
    amendments.length === 0
      ? undefined
      : amendments
          .map((d) => {
            const a = d.amendment!;
            const arrow = a.status === "reverted" ? `${a.proposedWording} → ${a.criterion}` : `${a.criterion} → ${a.proposedWording}`;
            const label = a.status === "applied" ? "AMENDED" : a.status.toUpperCase();
            return `⚑ ${label} ${a.id}: ${arrow}`;
          })
          .join("; ");
  const amendedCount = amendments.filter((d) => d.amendment!.status === "applied").length;

  // Plan 05i: what the environment block is, in one line (`env blocked · …`).
  const envBlocked = phase.env?.blocked ? envBlockedLine(phase.env.blocked) : undefined;
  const envTools = envToolsLines(phase.env?.tools);

  let attention: string | undefined;
  if (envBlocked) attention = envBlocked;
  else if (phase.phase === "BLOCKED") attention = "BLOCKED";
  else if (needsYou > 0 || phase.phase === "AWAITING_OWNER") attention = "needs you";
  else if (!alive && phase.phase !== "DONE") attention = "conductor stopped";
  else if (idleMinutes !== undefined && idleMinutes > 5) attention = `idle ${Math.round(idleMinutes)}m`;
  else if (t && t.byCategory.polling.ms > t.elapsedMs / 4) attention = "heavy polling";

  const firstAt = timeline.phases[0]?.at;
  const endAt = phase.phase === "DONE" || phase.phase === "BLOCKED" ? Date.parse(timeline.phases[timeline.phases.length - 1].at) : now.getTime();
  // Plan 01h: the live trade-offs panel and the cost meter. Both come from
  // this phase's own records (verdict.ts's tradeoffEntries and runCost).
  const tradeoffs = tradeoffEntries(phase);
  const cost = runCost(timeline, spans, now);
  // Plan 04c: the balance metrics are a deterministic projection of the same
  // timeline; the status view and `tt summary` render the one line / section.
  const metrics = computeMetrics(phase, timeline, events);
  // Plan 05h: the loop tape is a projection of the same log read as the
  // metrics — its durations end at `endMs`, the last logged timestamp, never a
  // wall clock, so `tt contract rebuild` reproduces the live file.
  const tapeInput = {
    label: tapeLabel(phase.phaseId),
    round,
    phases: timeline.phases,
    phase,
    run: timeline.state.run,
    endMs: timelineEndMs(timeline, events),
    models: chartModelsFromPlan(plan.models),
    limits: stageLimits(plan),
  };
  const tape = renderLoopTape(tapeInput);
  return {
    timeline,
    tape,
    metrics,
    metricsLine: metricsLine(metrics),
    models: modelsLineText(plan.models),
    loop: loopTapeHead(tapeInput),
    stage,
    stageElapsed: formatDuration(current?.ms ?? 0),
    elapsed: firstAt ? formatDuration(endAt - Date.parse(firstAt)) : "0s",
    pipeline: pipelineLine(spans, stageLimits(plan)),
    gates,
    gate: gateRecord && gateRecord.candidateSha === C ? gateSummaryLine(gateRecord, phase.integrationHead) : undefined,
    baseline: baselineStatusLine(baseline),
    previousRound,
    reviewers,
    reviewLine: reviewers.map((r) => r.label).join("   "),
    verdict,
    addressing: repairing ? reasons : [],
    rounds: timeline.rounds,
    round,
    time: t ? timeLine(t) : undefined,
    runningTool: t?.running ? `${t.running.name} ${t.running.arg}` : undefined,
    boundaryFilesChanged: phase.decisions.filter((d) => d.source === "trigger" && d.boundCandidateSha === C).length,
    liveDecisions: live.length,
    failedDecisions: failed,
    flaggedDecisions: flagged,
    amendments: amendmentLine,
    amendedCount,
    ...(tradeoffs.length > 0 ? { tradeoffs } : {}),
    cost,
    openFindings,
    review: reviewSummary(phase.messages),
    needsYou,
    idleMinutes,
    attention,
    envBlocked,
    envTools,
    ...(flakesLine(metrics) ? { flakes: flakesLine(metrics)! } : {}),
  };
}

function lastEventAt(runDir: string): string | undefined {
  try {
    const lines = fs.readFileSync(runPaths(runDir).events, "utf8").trimEnd().split("\n");
    return (JSON.parse(lines[lines.length - 1]) as { ts?: string }).ts;
  } catch {
    return undefined;
  }
}

/** Plan 01f: the gate record for `candidateSha`, or undefined when none has
 * been written (yet). A malformed record is no record. */
function readGateRecord(runDir: string, candidateSha: string | undefined): GateRecord | undefined {
  if (!candidateSha) return undefined;
  try {
    return parseGateRecord(JSON.parse(fs.readFileSync(path.join(runPaths(runDir).checks, candidateSha, "gate.json"), "utf8")));
  } catch {
    return undefined;
  }
}

/** Plan 01f: one line citing a gate record — what a passing gate proved, the
 * head its evidence was produced for, how long it took and where its log is.
 *
 * `acceptedAtBase` is the integration head the phase is accepting the
 * candidate against now. When it differs from the record's own head (a
 * re-acceptance after a stale publish reuses the candidate's own record
 * rather than rewriting it), both are named: the record is never rewritten,
 * so the citation is where a reader learns that the evidence was produced
 * against the older head. */
export function gateSummaryLine(record: GateRecord, acceptedAtBase?: string): string {
  const short = (sha: string) => sha.slice(0, 7);
  // One outcome phrase (core/gate.ts), shared with the prompts and the
  // failure evidence: a gate that never started is never rendered as an
  // exit-less failure.
  const how = record.reused
    ? `passed (reused candidate ${record.reusedFrom?.slice(0, 9) ?? "?"}'s record${record.reusedFromBaseSha ? `, gated against base ${short(record.reusedFromBaseSha)}` : ""})`
    : gateOutcomeText(record, { withElapsed: true });
  const accepted =
    acceptedAtBase !== undefined && acceptedAtBase !== record.baseSha
      ? ` (accepted against base ${short(acceptedAtBase)})`
      : "";
  // A gate that never started (the candidate no longer merges) has nothing to
  // clean up, and says so, not `exit ?` (01f advisories A, B and M).
  const cleanup = !record.cleanup
    ? ""
    : record.notStarted || record.cleanupSkipped
      ? "; cleanup skipped (the gate did not start)"
      : `; cleanup ${record.cleanupExitCode === 0 ? "ok" : record.cleanupTimedOut ? "timed out" : `exit ${record.cleanupExitCode ?? "?"}`}`;
  return `gate ${how} on candidate ${record.candidateSha.slice(0, 9)} against base ${short(record.baseSha)}${accepted}: ${record.command} (log sha256 ${record.logSha256.slice(0, 12)}…${cleanup})`;
}

function readBaseline(runDir: string): Baseline | undefined {
  try {
    return parseBaseline(JSON.parse(fs.readFileSync(path.join(runPaths(runDir).checks, "base", "baseline.json"), "utf8")));
  } catch {
    return undefined;
  }
}

function probeReused(runDir: string, C: string): boolean {
  try {
    const text = fs.readFileSync(runPaths(runDir).events, "utf8");
    return text.includes('"probe_checks_reused"') && text.includes(C);
  } catch {
    return false;
  }
}

/** `tt timing`: per agent, where the time went. */
export function timingReport(runDir: string, now = new Date()): AgentTime[] {
  return streamFiles(runDir)
    .map((f) => agentTime(f, now, false))
    .filter((t): t is AgentTime => t !== undefined);
}

export function timingText(times: AgentTime[]): string {
  const out: string[] = [];
  for (const t of times) {
    out.push(
      `${t.agentId}  ${t.startedAt.slice(11, 19)}–${t.lastAt.slice(11, 19)}  elapsed ${formatDuration(t.elapsedMs)} · tools ${formatDuration(t.toolMs)} · model ${formatDuration(t.modelMs)} (${pct(t.modelMs, t.elapsedMs)})`,
    );
    for (const [cat, v] of Object.entries(t.byCategory).sort((a, b) => b[1].ms - a[1].ms)) {
      if (v.calls === 0) continue;
      out.push(`  ${cat.padEnd(14)} ${String(v.calls).padStart(3)} calls  ${formatDuration(v.ms).padStart(7)}  ${pct(v.ms, t.elapsedMs).padStart(4)}`);
    }
    for (const c of t.slowest.slice(0, 3)) {
      out.push(`    ${formatDuration(c.ms).padStart(7)}  ${c.name}: ${c.arg.replace(/\s+/g, " ").slice(0, 90)}`);
    }
  }
  return out.join("\n");
}

/** Pure helper for PhaseState-only callers (tests). */
export function reviewLineFor(phase: PhaseState): string {
  return reviewerOutcomes(phase)
    .map((r) => r.label)
    .join("   ");
}

// ---------------------------------------------------------------------------
// PR summary (skill fix 3): what a reviewer of the PR needs, including the
// advisory findings the loop accepted without fixing — run 9120dca7 (12c)
// finished with 9 of them, and nothing carried them past the run.
// ---------------------------------------------------------------------------

export function prSummary(runDir: string, plan: RunPlanFile, extra: { removedTests?: string[] } = {}): string {
  const v = buildView(runDir, plan, false);
  const phase = v.timeline.state.phase;
  const C = phase.candidate?.sha;
  const goal = phase.contract.goal.length > 600 ? `${phase.contract.goal.slice(0, 599)}…` : phase.contract.goal;
  const live = phase.decisions.filter((d) => isLiveDecision(d) && d.source !== "trigger" && d.boundCandidateSha === C);
  const flagged = live.filter((d) => d.class === "reserved");
  const advisories = phase.findings.filter((f) => f.status === "open" && f.severity === "advisory");
  const fixed = phase.findings.filter((f) => f.severity === "blocking" && f.status === "repaired");
  // Plan 01g: every amendment the reviewers passed for this phase, with the
  // wording it replaced — the PR body must not hide a reworded criterion.
  // Only live records: a sibling proposal superseded by another is not a
  // pending amendment and must not read PROPOSED (01g advisory B).
  const amendments = phase.decisions.filter((d) => d.amendment && isLiveDecision(d));
  // Plan 01i: the owner's rulings in force are part of the PR body — binding
  // on reviewers, and the reason a `⚑` decision reads the way it does.
  const directives = (phase.ownerDirectives ?? []).filter((d) => d.status === "in-force");
  // Contract v1: a message the owner refused after DONE is a recorded
  // follow-up, not a blocker. The PR body must not hide it.
  const followUps = (phase.messages ?? []).filter((m) => m.followUp);
  // #+TT_MODELS: the models the run actually used, one line per role.
  const models = modelEntries(plan.models);
  const lines = [
    `## ${phase.phaseId}`,
    "",
    goal,
    "",
    ...(directives.length > 0
      ? [
          "### Owner directives (binding)",
          "",
          ...directives.map((d) => `- **${d.id}**${d.scope === "program" ? " (whole program)" : ""}: ${d.text}`),
          "",
        ]
      : []),
    "### Review (tradeoffs-trace)",
    "",
    `- Pipeline: ${v.pipeline}`,
    // Plan 01f: the conductor's own live proof, cited from its record.
    ...(phase.contract.gate ? [`- Gate: ${v.gate ?? "no record for the accepted candidate"}`] : []),
    `- Reviews on the accepted candidate ${C ? C.slice(0, 9) : "?"}: ${v.reviewLine}`,
    `- ${v.round} review round(s); ${fixed.length} blocking finding(s) raised and fixed before acceptance`,
    `- ${live.length} trade-off(s), ${flagged.length} flagged for the owner`,
    "",
    ...(models.length > 0
      ? ["### Models per role", "", ...models.map((m) => `- ${m.role}: ${m.value}`), ""]
      : []),
    ...metricsSummary(v.metrics),
  ];
  // Plan 01c: the owner's own checklist (from the plan's `Owner checklist:`
  // list). It is not a worker/reviewer acceptance criterion, so it is not
  // judged in the loop; the PR body carries it as an open checklist for the
  // owner to tick off.
  const ownerChecklist = plan.phases[0]?.ownerChecklist ?? [];
  if (ownerChecklist.length > 0) {
    lines.push("", "### Owner checklist", "");
    for (const item of ownerChecklist) lines.push(`- [ ] ${item}`);
  }
  if (amendments.length > 0) {
    lines.push("", "### Amended acceptance criteria", "");
    for (const d of amendments) {
      const a = d.amendment!;
      // A reverted amendment's arrow points back to the restored wording, so
      // the PR body never reads as if the replacement were in force (A-14).
      const arrow = a.status === "reverted" ? `${a.proposedWording} → ${a.criterion}` : `${a.criterion} → ${a.proposedWording}`;
      lines.push(
        `- ${a.status === "applied" ? "**⚑ AMENDED**" : `**${a.status.toUpperCase()}**`} **${a.id}** (raised by ${a.raisedBy}): ${arrow}`,
      );
    }
  }
  if (fixed.length > 0) {
    lines.push("", "### Blocking findings fixed during review", "");
    for (const f of fixed) lines.push(`- **${f.raisedBy}**: ${oneLine(f.evidence, 300)}`);
  }
  if (flagged.length > 0) {
    lines.push("", "### Flagged trade-offs (reserved: worth an owner's look)", "");
    for (const d of flagged) lines.push(`- ${d.choice} — ${decisionStatus(d, phase).status}`);
  }
  if (advisories.length > 0) {
    lines.push("", `### Open advisory findings (${advisories.length}) — accepted, not fixed`, "");
    for (const f of advisories) lines.push(`- **${f.raisedBy}**: ${oneLine(f.evidence, 400)}`);
  }
  if (followUps.length > 0) {
    lines.push("", `### Follow-ups (refused after DONE — recorded, not blocking)`, "");
    for (const m of followUps) {
      const reason = m.settlement?.reason ? ` — ${oneLine(m.settlement.reason, 200)}` : "";
      lines.push(`- **${m.id}** (${m.type}): ${oneLine(m.title, 200)}${reason}`);
    }
  }
  if (extra.removedTests && extra.removedTests.length > 0) {
    lines.push("", `### Tests removed from files that still exist (${extra.removedTests.length})`, "");
    for (const t of extra.removedTests) lines.push(`- ${t}`);
  }
  return `${lines.join("\n")}\n`;
}

function oneLine(text: string, max: number): string {
  const t = text.replace(/\s+/g, " ").trim();
  return t.length > max ? `${t.slice(0, max - 1)}…` : t;
}
