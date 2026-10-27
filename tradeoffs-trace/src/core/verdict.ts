// Plan 3b: what the owner reads about a review round, computed from state
// alone (pure; no I/O). Front ends render these strings and never infer an
// outcome from individual ballots themselves (observed in run 4ec5e0f8: "M ✓
// A ✓ B ✓" read as all approved while M had vetoed and a blocking finding was
// open).

import { acceptanceBlockers, decisionStatus, isLiveDecision, itemIsSplit, pointHasProvingFact, pointNeedsEvaluation, shortId } from "./predicate.ts";
import { isStructured, phaseItemOutcomes, thinMetItems, workerAnchorsOf } from "./items.ts";
import { discoveredDecisions, openFindings, recordClassified } from "./triage.ts";
import { seatsOf } from "./seats.ts";
import { currentBallot, isValidBallot } from "./tally.ts";
import type { MessageType, PhaseState, Reviewer } from "./types.ts";

export { shortId } from "./predicate.ts";

/** The default seats, kept for callers that predate `#+TT_REVIEWERS`. */
export const REVIEWERS: readonly Reviewer[] = ["M", "A", "B"];

export interface ReviewerOutcome {
  reviewer: Reviewer;
  /** "pending": no review of the current candidate yet; "objects": a reject
   * ballot or a blocking finding; "approves": submitted with neither. */
  state: "pending" | "approves" | "objects";
  approvals: number;
  rejections: number;
  blocking: number;
  advisory: number;
  /** e.g. "M ✗ 2 reject · 1 blocking", "A ✓", "B ✓ 3 advisory", "M ⧗". */
  label: string;
}

/** Each reviewer's outcome on the current candidate. */
export function reviewerOutcomes(phase: PhaseState): ReviewerOutcome[] {
  const C = phase.candidate?.sha;
  return seatsOf(phase.contract).map((reviewer) => {
    const review = phase.reviews[reviewer]?.review;
    const submitted = Boolean(C && review && review.candidateSha === C);
    const ballots = phase.ballots.filter((b) => b.reviewer === reviewer && b.boundCandidateSha === C);
    const approvals = ballots.filter((b) => b.vote === "approve").length;
    const rejections = ballots.filter((b) => b.vote === "reject").length;
    const raised = phase.findings.filter(
      (f) => f.boundCandidateSha === C && (f.raisedBy === reviewer || (f.alsoRaisedBy ?? []).includes(reviewer)),
    );
    const blocking = raised.filter((f) => f.severity === "blocking").length;
    const advisory = raised.filter((f) => f.severity === "advisory").length;
    const state = !submitted ? "pending" : rejections > 0 || blocking > 0 ? "objects" : "approves";
    const parts = [
      rejections > 0 ? `${rejections} reject` : "",
      blocking > 0 ? `${blocking} blocking` : "",
      advisory > 0 ? `${advisory} advisory` : "",
    ].filter(Boolean);
    const mark = state === "pending" ? "⧗" : state === "objects" ? "✗" : "✓";
    return {
      reviewer,
      state,
      approvals,
      rejections,
      blocking,
      advisory,
      label: `${reviewer} ${mark}${parts.length > 0 ? ` ${parts.join(" · ")}` : ""}`,
    };
  });
}

/** Why the current candidate is not (or not yet) accepted, one short clause
 * each; empty when nothing stands in the way. Only failures that already
 * happened are listed; a gate still running is not a reason. */
export function notAcceptedReasons(phase: PhaseState): string[] {
  if (!phase.candidate?.sha) return [];
  // Plan 07f (C3): the SAME list `accept` reads. There is no second
  // derivation of "why not accepted" here or in the status view.
  return acceptanceBlockers(phase).map((b) => b.detail);
}

/** Plan 07f (F0): the records that may still change scope, behaviour or cost,
 * and therefore the only records an evaluator session is started for:
 *
 *   - a raw/owner-refused message awaiting evaluation;
 *   - a plan item the seats split on, or one whose majority is not met/fits
 *     (or is a thin unanimous met that owes an audit);
 *   - an open point with no proving doneWhen fact (a failing fact keeps it
 *     open, whatever an evaluator drop says);
 *   - an open finding or discovered decision whose impact is not classified.
 *
 * Empty ⇒ no evaluator starts; the mechanical verdict stands and an
 * EVALUATION_SKIPPED record is written. */
export type EvaluationExceptionKind = "message" | "item" | "point" | "decision";

export interface EvaluationException {
  kind: EvaluationExceptionKind;
  id: string;
  detail: string;
  messageType?: MessageType;
}

export function evaluationExceptions(phase: PhaseState): EvaluationException[] {
  const out: EvaluationException[] = [];
  for (const message of phase.messages ?? []) {
    if (message.state !== "raw" && message.state !== "refused") continue;
    out.push({
      kind: "message",
      id: message.id,
      detail: `${message.type} message ${shortId(message.id)} (${message.state}): ${message.title}`,
      messageType: message.type,
    });
  }
  if (isStructured(phase.contract)) {
    const anchors = workerAnchorsOf(phase.coverage);
    for (const outcome of phaseItemOutcomes(phase)) {
      const owed = outcome.outcome !== "met" && outcome.outcome !== "fits";
      const thin = thinMetItems([outcome], anchors).length > 0;
      const split = itemIsSplit(outcome);
      if (!owed && !thin && !split) continue;
      out.push({
        kind: "item",
        id: outcome.item.id,
        detail:
          `${outcome.item.id} ${outcome.item.title}: ${outcome.outcome}` +
          (split ? " (the seats split on it)" : thin ? " (thin unanimous met — audit it)" : " (a majority judged it unmet or deviating)"),
      });
    }
  }
  for (const finding of openFindings(phase)) {
    if (!finding.domain && !finding.doneWhen) {
      if (recordClassified(phase, "finding", finding.id)) continue;
      out.push({ kind: "point", id: finding.id, detail: `finding ${shortId(finding.id)} needs an impact classification` });
      continue;
    }
    if (!pointNeedsEvaluation(phase, finding)) continue;
    out.push({
      kind: "point",
      id: finding.id,
      detail: pointHasProvingFact(phase, finding)
        ? `point ${shortId(finding.id)}${finding.domain ? ` in ${finding.domain}` : ""} needs an impact classification`
        : `point ${shortId(finding.id)}${finding.domain ? ` in ${finding.domain}` : ""} has no proving doneWhen fact on this candidate`,
    });
  }
  for (const decision of discoveredDecisions(phase)) {
    if (recordClassified(phase, "decision", decision.id)) continue;
    out.push({
      kind: "decision",
      id: decision.id,
      detail: `discovered decision ${shortId(decision.id)} needs an impact classification`,
    });
  }
  return out;
}

// ---------------------------------------------------------------------------
// Plan 01h: the live trade-offs panel
// ---------------------------------------------------------------------------

/** The ordered kinds the owner sees while a run is live, most important
 * first (design 01_ref_design.md goal 5). */
export type TradeoffKind = "directive" | "amendment" | "flagged" | "veto" | "dissent" | "advisories";

/** One self-contained line of the Trade-offs panel. `recordId` names the
 * record the line is about (a decision id, or an advisory finding id for the
 * count line), so `RET` on the line can open the decision view at it. */
export interface TradeoffEntry {
  kind: TradeoffKind;
  text: string;
  recordId?: string;
  /** Plan 06m (A3/R4): a human title for the record the line is about (a
   * decision's choice, a finding's claim), so the status never shows a bare
   * id. */
  title?: string;
  /** Plan 06m (A3/R4): the seats and ballot state behind the line, e.g.
   * `M veto · A,B rejected` — shown beside the title. */
  seats?: string;
}

/** The most lines the panel may show (design 01h: at most 6, most important
 * first). */
export const MAX_TRADEOFFS = 6;

/** Plan 01h: the few trade-offs that matter, most important first, at most
 * `max` lines. Everything comes from the phase's own records — the tally
 * (decisionStatus), the ballots (M's veto reason, the dissenters), the
 * findings (the advisory count), the amendment records and the owner
 * directives with their recorded delivery state. Nothing is inferred, and a
 * decision appears at most once, under its most actionable heading. */
export function tradeoffEntries(phase: PhaseState, max = MAX_TRADEOFFS): TradeoffEntry[] {
  const C = phase.candidate?.sha;
  const K = phase.contract.contractVersion;
  const directives: TradeoffEntry[] = [];
  const amendments: TradeoffEntry[] = [];
  const flagged: TradeoffEntry[] = [];
  const vetoes: TradeoffEntry[] = [];
  const dissents: TradeoffEntry[] = [];
  const advisories: TradeoffEntry[] = [];

  // (0) owner directives in force (plan 01i) whose immediate steer to a
  // recorded live agent has not been acknowledged — a ruling still in the
  // air is what the owner must know first. A directive with NO recorded
  // targets is not shown: "no agent was live at the time", and for a
  // program-wide directive seeded into a node's plan (conductor.ts's
  // `seededDirective`) the empty list means it reached this run through its
  // prompts all along. Nothing recorded says an agent has not received it,
  // so nothing here claims one has not (finding A-1).
  for (const d of phase.ownerDirectives ?? []) {
    if (d.status !== "in-force") continue;
    if (d.targets.length === 0) continue;
    const pending = d.targets.filter((t) => (d.deliveries ?? {})[t] !== "delivered");
    if (pending.length === 0) continue;
    directives.push({
      kind: "directive",
      recordId: d.id,
      title: oneLine(d.text, 80),
      seats: `not delivered to ${pending.join(", ")}`,
      text: `directive ${d.id} not yet delivered to ${pending.join(", ")}: ${oneLine(d.text, 80)}`,
    });
  }

  // (1) disputed or amended criteria (plan 01g), old → new. A superseded
  // record is history — the decision view does not render it, so a line
  // pointing at it could not be followed (finding A-2).
  for (const d of phase.decisions) {
    const a = d.amendment;
    if (!a || !isLiveDecision(d)) continue;
    const arrow = a.status === "reverted" ? `${a.proposedWording} → ${a.criterion}` : `${a.criterion} → ${a.proposedWording}`;
    const status = decisionStatus(d, phase).status;
    const label =
      a.status === "applied"
        ? "⚑ AMENDED"
        : a.status === "reverted"
          ? "⚑ REVERTED"
          : status === "failed"
            ? "⚑ AMENDMENT REJECTED"
            : "⚑ AMENDMENT (pending)";
    amendments.push({ kind: "amendment", recordId: d.id, title: oneLine(a.proposedWording || a.criterion, 80), seats: status, text: `${label} ${a.id}: ${oneLine(arrow, 140)}` });
  }

  // (2)-(4) the current candidate's live decisions, each under exactly one
  // heading: a flagged (reserved) decision, an M veto, or a pass with dissent.
  const live = phase.decisions.filter((d) => isLiveDecision(d) && d.source !== "trigger" && d.boundCandidateSha === C && !d.amendment);
  for (const d of live) {
    const status = decisionStatus(d, phase);
    const choice = oneLine(d.choice, 70);
    const vetoMatch = /([A-Za-z0-9_]+) veto/.exec(status.reason ?? "");
    if (status.status === "failed" && vetoMatch) {
      const leader = vetoMatch[1];
      const m = C ? currentBallot(phase.ballots, d.id, leader, C, K, d.version) : undefined;
      const why = m && isValidBallot(m) ? oneLine(m.rationale, 80) : "no reason recorded";
      vetoes.push({ kind: "veto", recordId: d.id, title: choice, seats: `vetoed by ${leader}`, text: `vetoed by ${leader}: ${shortId(d.id)} ${choice} — ${why}` });
      continue;
    }
    const dissenters = C
      ? seatsOf(phase.contract).filter((who) => {
          const b = currentBallot(phase.ballots, d.id, who, C, K, d.version);
          return isValidBallot(b) && b.vote === "reject";
        })
      : [];
    if (d.class === "reserved") {
      const who = dissenters.length > 0 ? `; ${dissenters.join(", ")} rejected` : "";
      flagged.push({ kind: "flagged", recordId: d.id, title: choice, seats: `${status.status}${who}`, text: `⚑ flagged: ${shortId(d.id)} ${choice} — ${status.status}${who}` });
      continue;
    }
    if (status.status === "passed" && dissenters.length > 0) {
      dissents.push({ kind: "dissent", recordId: d.id, title: choice, seats: `${dissenters.join(", ")} rejected`, text: `passed with dissent (${dissenters.join(", ")} rejected): ${shortId(d.id)} ${choice}` });
    }
  }

  // (5) one line counting the advisories instead of listing them.
  const openAdvisories = phase.findings.filter((f) => f.severity === "advisory" && f.status === "open");
  if (openAdvisories.length > 0) {
    const fresh = C ? openAdvisories.filter((f) => f.boundCandidateSha === C) : [];
    const firstAdvisory = fresh[0] ?? openAdvisories[0];
    advisories.push({
      kind: "advisories",
      recordId: firstAdvisory.id,
      title: oneLine(firstAdvisory.evidence, 80),
      seats: `${fresh.length} new`,
      text: `${openAdvisories.length} advisories (${fresh.length} new) — C-c m d`,
    });
  }

  return [...directives, ...amendments, ...flagged, ...vetoes, ...dissents, ...advisories].slice(0, max);
}

/** One line, whitespace collapsed and truncated at `max` characters. */
function oneLine(text: string, max: number): string {
  const t = text.replace(/\s+/g, " ").trim();
  return t.length > max ? `${t.slice(0, max - 1)}…` : t;
}

