// Plan 04c: the balance metrics. Reviewers exist to help the owner review, but
// too many messages (or too few) make the balance wrong, and nothing measured
// it. This module turns one phase's own state and control log into a small
// machine view (`views/metrics.json`) plus a one-line summary the status buffer
// and `tt summary` render.
//
// Pure: no I/O. `computeMetrics` reads only the phase state, the phase
// timeline (the state arrivals with their timestamps) and the reduced event
// list, so `tt contract rebuild`/`check` and the live conductor compute the
// same bytes from the same log. The metrics are deliberately a deterministic
// projection of the log — a wall-clock `now` never enters them.

import { readLog } from "./effects/log.ts";
import type { Message, MessageType, PhaseState } from "./core/types.ts";

/** The slice of `Timeline` (src/conductor.ts) the metrics need. Structural,
 * so metrics.ts stays free of a conductor import cycle. */
export interface MetricsTimeline {
  phases: Array<{ phase: string; at: string }>;
}

/** One reduced event, as `events.jsonl` carries it. Only the fields the
 * metrics read are typed. */
export interface MetricEvent {
  type?: string;
  ts?: string;
  verdict?: string;
  outcome?: string;
}

/** Per type: how many messages were raised, and where they ended up. `raw`
 * counts messages still awaiting an evaluator; `published` counts the ones the
 * evaluator (or its timeout) published for the owner. */
export interface MessageCounts {
  raised: number;
  raw: number;
  published: number;
  merged: number;
  dropped: number;
  accepted: number;
  refused: number;
  resolved: number;
  superseded: number;
}

export interface PhaseMetrics {
  phaseId: string;
  /** Candidates frozen so far (`phase.round`), i.e. review rounds. */
  rounds: number;
  /** Total wall time from the first state arrival to the last logged event. */
  wallMs: number;
  /** Time in REVIEWING plus EVALUATING — the loop's review share. */
  reviewMs: number;
  /** `reviewMs / wallMs`, 0 when there is no time. */
  reviewShare: number;
  /** Time in AWAITING_OWNER: how long the owner's desk held the run. */
  ownerWaitMs: number;
  /** Raw vs published and every terminal state, per message type. */
  messages: Record<MessageType, MessageCounts>;
  /** `merged / raised` across all types. */
  mergeRate: number;
  /** `dropped / raised` across all types. */
  dropRate: number;
  /** The owner's verdicts: A and D counts and the D rate. */
  ownerVerdicts: { accept: number; refuse: number };
  /** `refuse / (accept + refuse)`. */
  refuseRate: number;
  /** Raw blockers the panel voted to escalate, downgrade or could not decide. */
  blockers: { escalated: number; downgraded: number; incomplete: number };
  /** The unexposed-decision proxy: trade-offs a reviewer raised that the
   * worker did not raise itself (a `reviewer-discovered` decision). */
  unexposedTradeoffs: number;
}

const EMPTY_COUNTS = (): MessageCounts => ({
  raised: 0,
  raw: 0,
  published: 0,
  merged: 0,
  dropped: 0,
  accepted: 0,
  refused: 0,
  resolved: 0,
  superseded: 0,
});

function ratio(n: number, d: number): number {
  return d === 0 ? 0 : Number((n / d).toFixed(4));
}

/** Reduce-phase time per state, every arrival lasting until the next (the
 * last until `endMs`). Mirrors `statsFromTimeline` so the metrics and the
 * phase chart read the same timeline the same way. */
function timeByState(timeline: MetricsTimeline, endMs: number): Record<string, number> {
  const timeMs: Record<string, number> = {};
  for (let i = 0; i < timeline.phases.length; i++) {
    const { phase, at } = timeline.phases[i];
    const start = Date.parse(at);
    const stop = i + 1 < timeline.phases.length ? Date.parse(timeline.phases[i + 1].at) : endMs;
    timeMs[phase] = (timeMs[phase] ?? 0) + Math.max(0, stop - start);
  }
  return timeMs;
}

/** Whether the worker or a reviewer raised a trade-off. `raisedBy` is set by
 * the conductor; a fixture without it falls back to the decision it came
 * from (`reviewer-discovered` records). */
function reviewerRaisedTradeoff(message: Message, phase: PhaseState): boolean {
  if (message.type !== "tradeoff") return false;
  if (message.raisedBy) return message.raisedBy !== "worker";
  if (!message.sourceRecordId) return false;
  return phase.decisions.find((d) => d.id === message.sourceRecordId)?.source === "reviewer-discovered";
}

/** The balance metrics for one phase, from its state, the state timeline and
 * the reduced events. Deterministic: every duration ends at the last event
 * timestamp in the log, never at a caller's clock. */
export function computeMetrics(phase: PhaseState, timeline: MetricsTimeline, events: readonly MetricEvent[] = []): PhaseMetrics {
  const phaseTimes = timeline.phases.map((p) => Date.parse(p.at)).filter((t) => Number.isFinite(t));
  const eventTimes = events.map((e) => Date.parse(e.ts ?? "")).filter((t) => Number.isFinite(t));
  const endMs = Math.max(0, ...phaseTimes, ...eventTimes);
  const wallMs = timeline.phases.length === 0 ? 0 : Math.max(0, endMs - Date.parse(timeline.phases[0].at));
  const timeMs = timeByState(timeline, endMs);
  const reviewMs = (timeMs.REVIEWING ?? 0) + (timeMs.EVALUATING ?? 0);
  const ownerWaitMs = timeMs.AWAITING_OWNER ?? 0;

  const messages: Record<MessageType, MessageCounts> = {
    tradeoff: EMPTY_COUNTS(),
    finding: EMPTY_COUNTS(),
    blocker: EMPTY_COUNTS(),
  };
  let unexposedTradeoffs = 0;
  for (const message of phase.messages ?? []) {
    const counts = messages[message.type];
    if (!counts) continue;
    counts.raised += 1;
    counts[message.state] += 1;
    if (reviewerRaisedTradeoff(message, phase)) unexposedTradeoffs += 1;
  }
  const raised = messages.tradeoff.raised + messages.finding.raised + messages.blocker.raised;
  const merged = messages.tradeoff.merged + messages.finding.merged + messages.blocker.merged;
  const dropped = messages.tradeoff.dropped + messages.finding.dropped + messages.blocker.dropped;

  // The owner's verdicts come from the log, not from the surviving message
  // states: a message later superseded or carried must not erase a D the
  // owner cast. A fixture with no events falls back to the settlements.
  let accept = 0;
  let refuse = 0;
  if (events.some((e) => e.type === "OWNER_VERDICT")) {
    for (const e of events) {
      if (e.type !== "OWNER_VERDICT") continue;
      if (e.verdict === "accept") accept += 1;
      else if (e.verdict === "refuse") refuse += 1;
    }
  } else {
    for (const message of phase.messages ?? []) {
      if (message.settlement?.settledBy !== "owner") continue;
      if (message.settlement.state === "accepted") accept += 1;
      else if (message.settlement.state === "refused") refuse += 1;
    }
  }

  const blockers = { escalated: 0, downgraded: 0, incomplete: 0 };
  const panelEvents = events.filter((e) => e.type === "PANEL_DECIDED");
  if (panelEvents.length > 0) {
    for (const e of panelEvents) {
      if (e.outcome === "escalate") blockers.escalated += 1;
      else if (e.outcome === "downgrade") blockers.downgraded += 1;
      else if (e.outcome === "incomplete") blockers.incomplete += 1;
    }
  } else {
    for (const state of Object.values(phase.panel?.blockers ?? {})) {
      const outcome = state.decided?.outcome;
      if (outcome) blockers[outcome === "escalate" ? "escalated" : outcome === "downgrade" ? "downgraded" : "incomplete"] += 1;
    }
  }

  return {
    phaseId: phase.phaseId,
    rounds: phase.round ?? timelineRoundCount(phase),
    wallMs,
    reviewMs,
    reviewShare: ratio(reviewMs, wallMs),
    ownerWaitMs,
    messages,
    mergeRate: ratio(merged, raised),
    dropRate: ratio(dropped, raised),
    ownerVerdicts: { accept, refuse },
    refuseRate: ratio(refuse, accept + refuse),
    blockers,
    unexposedTradeoffs,
  };
}

/** The number of candidates frozen so far, when `phase.round` is absent (a
 * fixture): one per raised round in the state history. */
function timelineRoundCount(phase: PhaseState): number {
  return phase.candidate ? 1 : 0;
}

/** `views/metrics.json` exactly as `tt contract rebuild`/`check` writes and
 * compares it. A trailing newline keeps it a text file like its siblings. */
export function projectMetrics(metrics: PhaseMetrics): string {
  return `${JSON.stringify(metrics, null, 2)}\n`;
}

function pct(n: number): string {
  return `${Math.round(n * 100)}%`;
}

function duration(ms: number): string {
  const s = Math.max(0, Math.round(ms / 1000));
  if (s < 60) return `${s}s`;
  const m = Math.floor(s / 60);
  return `${m}m${String(s % 60).padStart(2, "0")}s`;
}

/** The one status line: every balance number in a single readable row. */
export function metricsLine(m: PhaseMetrics): string {
  const t = m.messages.tradeoff;
  const f = m.messages.finding;
  const b = m.messages.blocker;
  const parts = [
    `${m.rounds} round${m.rounds === 1 ? "" : "s"}`,
    `review ${pct(m.reviewShare)} (${duration(m.reviewMs)} of ${duration(m.wallMs)})`,
    `T ${t.raw}/${t.published} F ${f.raw}/${f.published} B ${b.raw}/${b.published} raw/published`,
    `merge ${pct(m.mergeRate)} drop ${pct(m.dropRate)}`,
    `A ${m.ownerVerdicts.accept} D ${m.ownerVerdicts.refuse} (D ${pct(m.refuseRate)})`,
    `owner wait ${duration(m.ownerWaitMs)}`,
    `blockers ${m.blockers.escalated} escalated, ${m.blockers.downgraded} downgraded`,
    `unexposed ${m.unexposedTradeoffs}`,
  ];
  return `metrics   ${parts.join(" · ")}`;
}

/** The same numbers as a small Markdown section for the `tt summary` PR body. */
export function metricsSummary(m: PhaseMetrics): string[] {
  const t = m.messages.tradeoff;
  const f = m.messages.finding;
  const b = m.messages.blocker;
  return [
    "### Balance metrics",
    "",
    `- Rounds: ${m.rounds}`,
    `- Review share of wall time: ${pct(m.reviewShare)} (${duration(m.reviewMs)} of ${duration(m.wallMs)})`,
    `- Raw/published per type: trade-offs ${t.raw}/${t.published} · findings ${f.raw}/${f.published} · blockers ${b.raw}/${b.published}`,
    `- Merge rate: ${pct(m.mergeRate)}; drop rate: ${pct(m.dropRate)}`,
    `- Owner verdicts: A ${m.ownerVerdicts.accept}, D ${m.ownerVerdicts.refuse} (D rate ${pct(m.refuseRate)})`,
    `- Owner wait: ${duration(m.ownerWaitMs)}`,
    `- Blockers: ${m.blockers.escalated} escalated, ${m.blockers.downgraded} downgraded, ${m.blockers.incomplete} incomplete`,
    `- Unexposed-decision proxy (reviewer-raised trade-offs the worker did not raise): ${m.unexposedTradeoffs}`,
  ];
}

/** One log record, structurally (so metrics.ts never imports the log module's
 * record type). */
export interface LogLike {
  kind: string;
  ts: string;
  event: unknown;
}

/** The reduced events, as metrics read them: one per `kind:"event"` record,
 * carrying its log timestamp. Exported so a caller that already read the log
 * (the conductor's timeline rebuild) can reuse the same snapshot. */
export function metricEvents(records: readonly LogLike[]): MetricEvent[] {
  return records.filter((r) => r.kind === "event").map((r) => ({ ...(r.event as MetricEvent), ts: r.ts }));
}

/** Read a run's control log and compute its metrics from the given state and
 * timeline. `events` may be supplied when the caller already read the log, so
 * the projection cannot diverge from the timeline it was built beside. */
export function metricsForRunDir(runDir: string, phase: PhaseState, timeline: MetricsTimeline, events?: readonly MetricEvent[]): PhaseMetrics {
  const evs = events ?? metricEvents(readLog(`${runDir}/events.jsonl`).records);
  return computeMetrics(phase, timeline, evs);
}
