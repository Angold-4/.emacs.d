// design §6.3, transcribed exactly:
//
//   accept(C, K) ⇔
//         every CHECKS command passed on a fresh checkout of C
//     ∧   the integration probe of C onto the current head H passed, giving I
//     ∧   M, A and B each submitted a valid review bound to (C, K)
//           — required even when there are no decisions to vote on
//     ∧   no finding is open with severity blocking
//     ∧   every decision on C is detail, passed by vote bound to (C, K),
//           or resolved by the owner bound to (C, K)
//     ∧   no owner request is open
//     ∧   every open owner correction X satisfies addressed(X, C, K)
//
//   addressed(X, C, K) ⇔
//         X is bound to contract K
//     ∧   M, A and B each stated, in their review bound to (C, K),
//           that X is honored — none states it is not
//
//   done(phase) ⇔ accept(C, K) ∧ the integration branch points at the probed I
//
// Every input `accept` reads is evaluated *before* ACCEPTED: it never reads
// the phase's own current FSM state name or `publishedI` — those are
// produced by acceptance or later. test/unit/no-circularity.test.ts asserts
// this by instrumenting property access.
//
// `decisionSettled` and `resolvedCorrectionIdsFor` are the single source of
// truth for "is this decision resolved" / "which corrections does this
// candidate address". next.ts and transitions.ts call these (via `accept`
// itself, or directly) rather than re-deriving the same facts — round-1
// review item 5's last bullet: openItemsRemain must not reimplement this.

import { readFileSync } from "node:fs";

import { leaderOf, seatsOf } from "./seats.ts";
import { currentBallot, isValidBallot, tally } from "./tally.ts";
import type { Domain } from "./domains.ts";
// Plan 06b: the per-item acceptance half — every R and C met by majority with
// its test verifies passed, every A fitting by majority or its deviation
// accepted by the owner, and every evidence item recorded.
import { evidenceFileAnchors, evidenceHasAnchor, flatItems, isStructured, itemNeedsEvidence, itemsFromPhase, phaseItemOutcomes, testVerifyProblems } from "./items.ts";
import { factKeepsOpen } from "./fix-rounds.ts";
import {
  discoveredDecisions,
  findingDispositionIsTradeoff,
  openFindings,
  ownerRequestIsStale,
  recordClassified,
} from "./triage.ts";
import type { RoundPanelOutcome, RoundPanelState } from "./types.ts";
import type {
  ContractVersion,
  Correction,
  CriterionAmendment,
  Decision,
  MessageType,
  PanelOption,
  PanelOutcome,
  PanelSeatState,
  PanelState,
  PhaseState,
  Review,
} from "./types.ts";

export function sameVersion(a: ContractVersion, b: ContractVersion): boolean {
  return a.snapshot === b.snapshot && a.sectionSha256 === b.sectionSha256;
}

/** A single review's correction statements must not state a disposition for
 * the same correction more than once: §6.3's addressed speaks of "stated ...
 * that X is honored — none states it is not", so a doubly-stated correction
 * (contradictory or merely repeated) is malformed, not order-decided. This
 * is the shared, pure ingestion check used by reduce() for REVIEW_SUBMITTED
 * and by the extension's submit_review validation; returns a
 * human-readable reason, or undefined when the review is well-formed. */
export function reviewIngestionIssue(review: Review, domains?: readonly Domain[]): string | undefined {
  const statements = review?.correctionStatements;
  if (!Array.isArray(statements)) {
    return `review by ${String(review?.reviewer)} has no correctionStatements array`;
  }
  const seen = new Set<string>();
  for (const statement of statements) {
    const id = statement?.correctionId;
    if (seen.has(id)) {
      return `review by ${String(review?.reviewer)} states a disposition for correction ${String(id)} more than once; a correction must be stated at most once`;
    }
    seen.add(id);
  }
  // Plan 07a (D2): in domain mode a reviewer's finding and blocker are one
  // point: `domain`, `item` and `doneWhen` are required, and the domain must
  // be one the plan declared. Outside domain mode the fields are optional and
  // ignored, so a review without them is exactly as today.
  if (domains && domains.length > 0) {
    const names = new Set(domains.map((d) => d.name));
    const pointIssue = (kind: "finding" | "blocker", entry: { domain?: string; item?: string; doneWhen?: string }): string | undefined => {
      for (const field of ["domain", "item", "doneWhen"] as const) {
        const value = entry[field];
        if (typeof value !== "string" || value.trim().length === 0) {
          return `${kind} by ${String(review?.reviewer)} has no ${field}; in domain mode every ${kind} names domain, item and doneWhen`;
        }
      }
      if (!names.has(entry.domain!)) {
        return `${kind} by ${String(review?.reviewer)} names the undeclared domain ${entry.domain}; declare it in #+TT_DOMAINS`;
      }
      return undefined;
    };
    for (const finding of review.findings ?? []) {
      const issue = pointIssue("finding", finding);
      if (issue) return issue;
    }
    for (const blocker of review.blockers ?? []) {
      const issue = pointIssue("blocker", blocker);
      if (issue) return issue;
    }
  }
  return undefined;
}

/** addressed(X, C, K): true iff M, A and B each stated, in a review bound to
 * (C, K), that correction X is honored — and none stated it is not. Fails
 * closed on malformed or legacy data: a missing statement, more than one
 * statement for X (identical or contradictory, in either order), or any
 * status that is not exactly `"honored"` all make it false. */
export function addressed(correction: Correction, phase: PhaseState, C: string, K: ContractVersion): boolean {
  if (!sameVersion(correction.boundContractVersion, K)) return false;
  for (const who of seatsOf(phase.contract)) {
    const review = phase.reviews[who]?.review;
    if (!review) return false;
    if (review.candidateSha !== C || !sameVersion(review.contractVersion, K)) return false;
    const matching = (review.correctionStatements ?? []).filter((s) => s.correctionId === correction.id);
    if (matching.length !== 1) return false;
    if (matching[0].status !== "honored") return false;
  }
  return true;
}

/** Plan 04a: the message types, one evaluator each per round. */
export const EVALUATOR_TYPES = ["tradeoff", "finding", "blocker"] as const;

/** Plan 04a: the types this round's EVALUATING stage must evaluate: a type
 * with a raw message to check, or one with an owner-refused message awaiting
 * its "was it addressed" report (owner correction, item 3). */
/** Plan 07f (F0): true when a point's doneWhen fact on this candidate made
 * it pass, and the same command did NOT pass on the round's base — i.e. the
 * fact actually proves the point. A passing-but-also-on-base fact, a failing
 * one, a timeout, or no fact at all proves nothing. */
export function pointHasProvingFact(
  phase: PhaseState,
  finding: { id: string; doneWhen?: string; domain?: string },
): boolean {
  const C = phase.candidate?.sha;
  if (!C || (!finding.domain && !finding.doneWhen)) return false;
  return (phase.doneWhenFacts ?? []).some(
    (f) =>
      f.findingId === finding.id &&
      f.candidateSha === C &&
      !f.timedOut &&
      f.exitCode === 0 &&
      f.passesOnBase !== true,
  );
}

/** Plan 07f (F0): true when an open point/finding still needs the
 * evaluator: an unclassified record, or a point with no proving doneWhen fact
 * that no approved trade-off closes. This is the ONE rule both the evaluator
 * dispatch list and `evaluationExceptions` read. */
export function pointNeedsEvaluation(phase: PhaseState, finding: import("./types.ts").Finding): boolean {
  const classified = recordClassified(phase, "finding", finding.id);
  if (!finding.domain && !finding.doneWhen) return !classified;
  return !(classified && (pointHasProvingFact(phase, finding) || findingDispositionIsTradeoff(phase.triage, finding.id)));
}

export function typesNeedingEvaluation(phase: PhaseState): MessageType[] {
  const types = (EVALUATOR_TYPES as readonly MessageType[]).filter((t) =>
    (phase.messages ?? []).some((m) => m.type === t && (m.state === "raw" || m.state === "refused")),
  );
  // Plan 06b (OD-1 R3b): a majority `unmet`/`deviates` needs the evaluator's
  // substantive re-check against the candidate before it blocks. Force one
  // `finding` evaluator pass for it when no finding message already asks.
  if (itemsNeedingEvaluatorReverify(phase) && !types.includes("finding")) types.push("finding");
  // Plan 06i (A3): a finding or discovered decision that still owes its impact
  // classification needs its evaluator pass even with no raw message of that
  // type this round — otherwise the classification is owed but never asked
  // and the record escalates.
  if (openFindings(phase).some((f) => !recordClassified(phase, "finding", f.id)) && !types.includes("finding")) {
    types.push("finding");
  }
  // Plan 07f (F0): an open point with no proving fact is an acceptance
  // exception even when its message was published in an earlier round.
  if (openFindings(phase).some((f) => pointNeedsEvaluation(phase, f)) && !types.includes("finding")) {
    types.push("finding");
  }
  if (discoveredDecisions(phase).some((d) => !recordClassified(phase, "decision", d.id)) && !types.includes("tradeoff")) {
    types.push("tradeoff");
  }
  return types;
}

/** Plan 06b (OD-1 R3b): true when a structured phase has an item whose
 * majority verdict is `unmet` or `deviates`, so the evaluator must re-check
 * it against the candidate before it can block. */
export function itemsNeedingEvaluatorReverify(phase: PhaseState): boolean {
  if (!isStructured(phase.contract)) return false;
  // ODP-2: an item blocker is never raised without an evaluator item check,
  // so every outcome that is not met/fits owes one.
  const outcomes = phaseItemOutcomes(phase);
  if (outcomes.some((o) => o.outcome !== "met" && o.outcome !== "fits")) return true;
  // Plan 07f (F0): the seats split on an item — at least one says met/fits
  // and at least one does not. The mechanical majority still decides the
  // verdict, but the split is exactly what the evaluator exists to resolve.
  // A unanimous met/fits item with resolving anchors is mechanically decided:
  // thin evidence alone never starts an evaluator session (plan 07f A-2).
  return outcomes.some(itemIsSplit);
}

/** True when the seats do not unanimously agree on an item's required
 * verdict (met/fits) — one or more say it and one or more do not. */
export function itemIsSplit(outcome: { item: { kind: string }; seats: ReadonlyArray<{ verdict: string }> }): boolean {
  const required = outcome.item.kind === "architecture" ? "fits" : "met";
  const yes = outcome.seats.filter((s) => s.verdict === required).length;
  const no = outcome.seats.filter((s) => s.verdict !== required).length;
  return yes > 0 && no > 0;
}

/** Plan 04a: whether everything EVALUATING waits for has settled. In 04a
 * that is one fresh evaluator per message type that has work: the predicate
 * holds when every such type's evaluator has finished or timed out. Plan 04b
 * adds its panels to this same predicate. */
// ---------------------------------------------------------------------------
// Plan 04b: the blocker panel
// ---------------------------------------------------------------------------

/** The default number of seats every panel has, kept for callers that
 * predate `#+TT_REVIEWERS`. From 06h a panel has as many seats as there are
 * reviewers, so every function below takes the count. */
export const PANEL_SEATS = [1, 2, 3] as const;

/** Plan 06h (A2): the panel seat numbers for `count` reviewers, 1-based. */
export function panelSeatNumbers(count: number): number[] {
  const n = Number.isInteger(count) && count >= 1 ? count : 3;
  return Array.from({ length: n }, (_, i) => i + 1);
}

/** The strict majority of `count` seats: 2 of 3, 3 of 5, 4 of 7. */
export function panelMajority(count: number): number {
  const n = Number.isInteger(count) && count >= 1 ? count : 3;
  return Math.floor(n / 2) + 1;
}

/** The fallback pair of owner options: what an ordinary open-finding request
 * for the blocker offers, worded exactly as `applyOwnerRequestResolved` and
 * `isRepairForcingOption` treat them (`accept_risk` settles the blocker and
 * lets the candidate stand; every other id starts the repair that carries it
 * out). A block vote must offer two or three options with distinct ids, so a
 * well-formed escalation never needs it — but if one ever did, both options
 * now do exactly what they say (round-3 review, advisory B-2). */
export const DEFAULT_BLOCKER_OPTIONS: PanelOption[] = [
  { id: "accept_risk", label: "accept the risk and let the candidate stand" },
  { id: "repair", label: "repair it (grant 3 rounds)" },
];

/** A seat is settled once it has voted, or is unavailable after its one
 * retry (its second dispatch). A first loss marks it unavailable but keeps it
 * re-dispatchable: `dispatches` is the durable retry bookkeeping. */
export function panelSeatSettled(seat: PanelSeatState | undefined): boolean {
  if (!seat) return false;
  if (seat.vote !== undefined) return true;
  return seat.unavailable === true && seat.dispatches >= 2;
}

export function panelSeatsSettled(panel: PanelState | undefined, count = 3): boolean {
  return panelSeatNumbers(count).every((n) => panelSeatSettled(panel?.seats?.[String(n)]));
}

/** The panel's verdict from its recorded votes: a majority `block` escalates,
 * a majority `downgrade` downgrades, anything else (including a split or two
 * unavailable seats) is incomplete. Only the core computes this; the
 * conductor may not invent an outcome. */
export function panelOutcome(panel: PanelState, count = 3): PanelOutcome {
  const votes = Object.values(panel.seats ?? {})
    .map((s) => s.vote)
    .filter((v): v is "block" | "downgrade" => v === "block" || v === "downgrade");
  const needed = panelMajority(count);
  const blocks = votes.filter((v) => v === "block").length;
  const downgrades = votes.filter((v) => v === "downgrade").length;
  if (blocks >= needed) return "escalate";
  if (downgrades >= needed) return "downgrade";
  return "incomplete";
}

/** The options a `block` majority puts to the owner: every block vote's own
 * options, in seat order, deduped by id and capped at three (a `block` vote
 * proposes two or three). Falls back to DEFAULT_BLOCKER_OPTIONS so an
 * escalation always has options. */
export function panelOptionsFor(panel: PanelState, count = 3): PanelOption[] {
  const out: PanelOption[] = [];
  for (const n of panelSeatNumbers(count)) {
    const seat = panel.seats?.[String(n)];
    if (seat?.vote !== "block") continue;
    for (const option of seat.options ?? []) {
      if (out.some((o) => o.id === option.id)) continue;
      out.push(option);
      if (out.length >= 3) return out;
    }
  }
  return out.length >= 2 ? out : DEFAULT_BLOCKER_OPTIONS;
}

/** Every blocker id this round's panel covers, in stable (sorted) order. */
export function blockersNeedingPanel(phase: PhaseState): string[] {
  return Object.keys(phase.panel?.blockers ?? {}).sort();
}

/** True once every panel of the round has a recorded decision. */
export function panelsSettled(phase: PhaseState): boolean {
  return Object.values(phase.panel?.blockers ?? {}).every((p) => p.decided !== undefined);
}

/** The first blocker (sorted order) whose panel decided `outcome`, if any. */
export function blockerWithOutcome(phase: PhaseState, outcome: PanelOutcome): string | undefined {
  return blockersNeedingPanel(phase).find((id) => phase.panel?.blockers?.[id]?.decided?.outcome === outcome);
}

/** Plan 04a/04b: whether everything EVALUATING waits for has settled: every
 * dispatched type's evaluator finished or timed out, every raw blocker's
 * panel reached a verdict, and (plan 05e) the round panel decided. This is
 * the gate that keeps the phase EVALUATING until the last of them settles. */
export function evaluationSettled(phase: PhaseState): boolean {
  return (
    typesNeedingEvaluation(phase).every((t) => phase.evaluation?.types?.[t]?.settled === true) &&
    panelsSettled(phase) &&
    roundPanelSettled(phase)
  );
}

// ---------------------------------------------------------------------------
// Plan 05e: the round panel (trade-offs and blocking findings, one per round)
// ---------------------------------------------------------------------------

/** The three round-panel seats, same as every panel. */
export function roundPanelSeatSettled(seat: import("./types.ts").RoundPanelSeatState | undefined): boolean {
  if (!seat) return false;
  if (seat.votes !== undefined) return true;
  return seat.unavailable === true && seat.dispatches >= 2;
}

export function roundPanelSeatsSettled(round: RoundPanelState | undefined, count = 3): boolean {
  return panelSeatNumbers(count).every((n) => roundPanelSeatSettled(round?.seats?.[String(n)]));
}

/** The items this round's panel votes on: every published trade-off message
 * whose backing decision does not already carry valid ballots from M, A and
 * B, plus every published open blocking finding (never a blocker message —
 * its own panel handles it). Computed from live state after the evaluators
 * publish, so it is stable for the panel's own dispatch. */
export function roundPanelItemsNeedingVote(phase: PhaseState): string[] {
  const C = phase.candidate?.sha;
  const K = phase.contract.contractVersion;
  if (!C) return [];
  const out: string[] = [];
  for (const m of phase.messages ?? []) {
    if (m.state !== "published" || m.boundCandidateSha !== C) continue;
    if (m.type === "tradeoff") {
      const decision = m.sourceRecordId ? phase.decisions.find((d) => d.id === m.sourceRecordId) : undefined;
      if (decision) {
        const all = seatsOf(phase.contract).every((who) =>
          isValidBallot(currentBallot(phase.ballots, decision.id, who, C, K, decision.version)),
        );
        if (all) continue;
      }
      out.push(m.id);
      continue;
    }
    if (m.type === "finding" && m.raisedAsBlocker !== true) {
      const finding = m.sourceRecordId ? phase.findings.find((f) => f.id === m.sourceRecordId) : undefined;
      if (!finding || finding.severity !== "blocking" || finding.status !== "open") continue;
      out.push(m.id);
    }
  }
  return out.sort();
}

/** One round-panel item's outcome from the recorded votes. A `keep` majority
 * keeps it (a trade-off is published to the owner; a blocking finding keeps
 * blocking). Otherwise a trade-off is dropped and a finding becomes
 * advisory — a 2-of-3 majority is the only way a finding blocks. */
export function roundPanelOutcomeFor(
  round: RoundPanelState | undefined,
  messageId: string,
  kind: "tradeoff" | "finding",
  count = 3,
): RoundPanelOutcome {
  const votes = panelSeatNumbers(count)
    .map((n) => round?.seats?.[String(n)]?.votes?.find((v) => v.messageId === messageId))
    .filter((v): v is NonNullable<typeof v> => v !== undefined);
  const needed = panelMajority(count);
  const tally = (verdict: string) => votes.filter((v) => v.verdict === verdict).length;
  if (tally("keep") >= needed) return "keep";
  if (tally("drop") >= needed) return "drop";
  if (tally("downgrade") >= needed) return "downgrade";
  return kind === "finding" ? "downgrade" : "drop";
}

/** True when the round panel has nothing to do (no pending item) or has
 * decided. A round with pending items cannot leave EVALUATING until it has. */
export function roundPanelSettled(phase: PhaseState): boolean {
  if (roundPanelItemsNeedingVote(phase).length === 0) return true;
  return phase.panel?.round?.decided === true;
}

/** Words too common to carry a citation on their own. */
const STOPWORDS = new Set([
  "that", "this", "with", "from", "into", "when", "then", "than", "have", "has", "had", "does", "done", "must", "should", "would", "could", "will", "shall", "each", "every", "before", "after", "over", "under", "their", "there", "where", "which", "while", "then", "them", "they", "your", "yours", "still", "only", "also", "very", "more", "most", "less", "much", "many", "some", "such", "been", "being", "were", "was", "are", "not",
]);

/** Plan 05e: a blocking finding is only legitimate when it is a defect against
 * an acceptance item or a reserved rule (finding #32, atlas 15.3). A
 * preference, or a defect that cites neither, may not block; the evaluator
 * lowers it and records the reason.
 *
 * A citation is accepted in the forms a reviewer actually writes (round-2
 * review, M-1): the item's text verbatim, a distinctive run of the item's own
 * words (a paraphrase that keeps its phrasing), a numbered reference
 * (`acceptance item 3`, `criterion 3`), an owner directive id the review
 * prompt tells a reviewer to cite, or a `criterionDispute` (which names an
 * acceptance item by construction). */
export function findingCitesAcceptanceOrReserved(
  finding: Finding,
  contract: { acceptance: string[]; reserved: string[] },
  directiveIds: readonly string[] = [],
): boolean {
  // Plan (5) ties blocking to the GROUND a finding cites, not to its kind
  // label: an `integration` finding against an acceptance item may block too
  // (round-4 reviews disc-M-51, F-M-15). A preference cites nothing, so it
  // still may not.
  const evidence = `${finding.evidence} ${finding.criterionDisputed ?? ""}`.toLowerCase();
  const words = (s: string): string[] => s.toLowerCase().replace(/[^a-z0-9\s]+/g, " ").split(/\s+/).filter(Boolean);
  const runs = (s: string, len: number): Set<string> => {
    const w = words(s);
    const n = Math.min(len, w.length);
    const out = new Set<string>();
    for (let i = 0; i + n <= w.length; i += 1) out.add(w.slice(i, i + n).join(" "));
    return out;
  };
  const evidenceRuns = runs(evidence, 4);
  const evidenceWords = new Set(words(evidence));
  const significant = (ws: string[]): string[] => ws.filter((w) => w.length >= 4 && !STOPWORDS.has(w));
  const cites = (item: string): boolean => {
    const needle = item.trim().toLowerCase();
    if (needle.length > 0 && evidence.includes(needle)) return true;
    // A paraphrase: most of the item's significant words, or a distinctive
    // four-word run of its own phrasing (round-2 review M-1).
    const itemSignificant = significant(words(item));
    if (itemSignificant.length >= 3) {
      const matched = itemSignificant.filter((w) => evidenceWords.has(w)).length;
      if (matched >= 3 && matched / itemSignificant.length >= 0.6) return true;
    }
    const n = Math.min(4, words(item).length);
    if (n >= 2) for (const run of runs(item, n)) if (evidenceRuns.has(run)) return true;
    return false;
  };
  if (contract.acceptance.some(cites) || contract.reserved.some(cites)) return true;
  if (finding.criterionDisputed && finding.criterionDisputed.trim().length > 0 && contract.acceptance.includes(finding.criterionDisputed)) return true;
  // A numbered reference to an acceptance item (`acceptance item 3`).
  for (const m of evidence.matchAll(/(?:acceptance\s+(?:item|criteria|criterion)|criterion|item)\s*#?\s*(\d+)/g)) {
    const idx = Number(m[1]);
    if (Number.isInteger(idx) && idx >= 1 && idx <= contract.acceptance.length) return true;
  }
  // An owner directive id cited as the blocking ground.
  for (const id of directiveIds) {
    const needle = id.trim().toLowerCase();
    if (needle.length === 0) continue;
    const escaped = needle.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    if (new RegExp(`(^|[^a-z0-9])${escaped}([^a-z0-9]|$)`).test(evidence)) return true;
  }
  return false;
}

/** M, A and B each have a review bound to (C, K) already. Used to decide,
 * after a probe (design §6.4 step 3's stale-publish retry re-probes the
 * SAME candidate against a new head), whether REVIEWING needs to dispatch
 * anything at all or the phase can go straight to RESOLVING. */
export function reviewsComplete(phase: PhaseState, C: string, K: ContractVersion): boolean {
  return seatsOf(phase.contract).every((who) => {
    const review = phase.reviews[who]?.review;
    return Boolean(review) && review!.candidateSha === C && sameVersion(review!.contractVersion, K);
  });
}

/** Every open correction addressed by (C, K) — design §6.3 / §7.5 step 5:
 * "the ACCEPTED event records it as resolved". Computed once here so
 * next()'s `accept` action and reduce()'s independent verification of the
 * event payload can never drift apart (see reduce.ts's ACCEPTED case). */
export function resolvedCorrectionIdsFor(phase: PhaseState, C: string, K: ContractVersion): string[] {
  return phase.corrections
    .filter((c) => c.status === "open" && addressed(c, phase, C, K))
    .map((c) => c.id)
    .sort();
}

/** True for a resolved owner request, linked to `decisionId`, bound to
 * (C, K), whose chosen option (never its label — round-3 review item 1) is
 * `settlingOptionId`. Shared by the `delegated` (`failed_vote`) and
 * `reserved` (`reserved_decision`) branches of decisionSettled below. */
function settledByOwnerRequest(
  phase: PhaseState,
  origin: "failed_vote" | "reserved_decision",
  decisionId: string,
  settlingOptionId: string,
  C: string,
  K: ContractVersion,
): boolean {
  return phase.ownerRequests.some(
    (r) =>
      r.status === "resolved" &&
      r.origin === origin &&
      r.linkedDecisionId === decisionId &&
      r.resolution?.option === settlingOptionId &&
      r.resolvedBinding !== undefined &&
      r.resolvedBinding.candidateSha === C &&
      sameVersion(r.resolvedBinding.contractVersion, K),
  );
}

/** Is `decision` settled for (C, K)? `detail` decisions always are (never
 * voted). `delegated` decisions are settled by a passing vote (tally.ts), an
 * owner `override` approving them bound to the decision's *current* version
 * (design §7.4: "recorded beside the ballots"), or a resolved `failed_vote`
 * owner request whose chosen option was "accept the decision as
 * implemented" (round-3 review item 1; §5.2, §6.3). `reserved` decisions are
 * settled only by a resolved `reserved_decision` owner request bound to
 * (C, K) whose chosen option was "approve" — the other option ("reject and
 * repair") does not settle it. */
/** Plan 2c: a record that still describes the current candidate and can be
 * voted on. Superseded records (by a correction, by a newer candidate the
 * worker did not carry them to, by the worker's withdrawal, or by a
 * reviewer matching its discovery to another record) never block
 * acceptance and are never votable. */
export function isLiveDecision(decision: Decision): boolean {
  return !decision.supersededByCorrection && !decision.supersededBy;
}

/** Plan 01g: a proposed amendment that has passed the normal tally and is
 * ready to be applied — a live `reserved` decision bound to the current
 * (candidate, contract) whose `amendment.status` is `proposed` and whose
 * ballot tally is `pass`. This is the only amendment that may rewrite the
 * contract; an amendment that failed (or is still short of a ballot) leaves
 * the criterion unchanged and never blocks acceptance by itself. */
export function amendmentToApply(
  phase: PhaseState,
  C: string,
  K: ContractVersion,
): Decision | undefined {
  return phase.decisions.find(
    (d) =>
      d.amendment !== undefined &&
      isLiveDecision(d) &&
      d.class === "reserved" &&
      d.amendment.status === "proposed" &&
      d.boundCandidateSha === C &&
      sameVersion(d.boundContractVersion, K) &&
      // The disputed criterion must still be in the contract: after another
      // amendment (or an owner AMEND) replaced it, applying this one is
      // meaningless — next() must agree with the row's own guard, or the
      // conductor emits an event the reducer refuses and throws.
      phase.contract.acceptance.includes(d.amendment.criterion) &&
      tally(d, phase.ballots, phase.findings, C, K, seatsOf(phase.contract), leaderOf(phase.contract)) === "pass",
  );
}

/** Plan 06k1 (A2): true when the owner ruled this decision — the recorded
 * ruling matches its id and its (unchanged) choice. Exported so the live
 * ballot demand reads the same fact `decisionSettled` does, and never asks a
 * reviewer for a ballot the owner's ruling already settled. */
export function ownerRuledDecision(phase: PhaseState, decision: Decision): boolean {
  return (phase.ruledDecisions ?? []).some((r) => r.id === decision.id && r.choice === decision.choice);
}

export function decisionSettled(decision: Decision, phase: PhaseState, C: string, K: ContractVersion): boolean {
  if (decision.class === "detail") return true;

  // delegated and reserved alike: the reviewers' vote, an owner override,
  // or the owner accepting it after a failed vote. A reserved decision is
  // flagged for the owner (DecisionStatus.flagged), never held for them.
  // Plan 06k1 (A2): a decision the owner ruled on is settled with no
  // ballots, on every later candidate whose decision has the same id and
  // choice. Checked before the tally so the ruling is never re-tallied.
  if (ownerRuledDecision(phase, decision)) return true;
  if (tally(decision, phase.ballots, phase.findings, C, K, seatsOf(phase.contract), leaderOf(phase.contract)) === "pass") return true;
  const overridden = phase.overrides.some(
    (o) =>
      o.decisionId === decision.id &&
      o.vote === "approve" &&
      o.boundCandidateSha === C &&
      sameVersion(o.boundContractVersion, K) &&
      o.boundRecordVersion === decision.version,
  );
  if (overridden) return true;
  if (settledByOwnerRequest(phase, "failed_vote", decision.id, "accept_as_implemented", C, K)) return true;
  // A reserved_decision request resolved before owner-optional (older runs).
  return settledByOwnerRequest(phase, "reserved_decision", decision.id, "approve", C, K);
}

/** A plain grant/stop repair-budget gate request (no linked record) is moot once the
 * phase's current candidate would be accepted without it: the gate asked
 * "more rounds or stop?" because items were open, and the owner has since
 * settled every one of them through their own requests (exex 01a1: a failed
 * vote and an open finding resolved, all items met, yet the gate still
 * offered only grant/stop). Inlined rather than isBudgetGateRequest, which
 * lives in owner-requests.ts, a module that imports this one. */
export function budgetGateIsMoot(phase: PhaseState, r: PhaseState["ownerRequests"][number]): boolean {
  if (r.origin !== "repair_budget_exhausted" || r.linkedDecisionId || r.linkedFindingId || r.linkedCorrectionId) {
    return false;
  }
  // Plan 06g: the "accept with carried items" gate asks the owner about the
  // open advisories themselves, so it is never moot.
  if (r.options.some((o) => o.id === "accept_carried")) return false;
  if (!phase.candidate) return false;
  const without = { ...phase, ownerRequests: phase.ownerRequests.filter((o) => o.id !== r.id) };
  return accept(without, phase.candidate.sha, phase.contract.contractVersion);
}

/** Plan 07f (F1): one unmet acceptance condition, with the command that
 * clears it. This is the single mechanical description of WHY a candidate is
 * not accepted; `accept()` is exactly "this list is empty", `notAcceptedReasons`
 * and the status view render it, and no other function keeps a second list. */
export type AcceptanceBlockerKind =
  | "candidate"
  | "check"
  | "probe"
  | "test-verify"
  | "symbol-deviation"
  | "evidence"
  | "review"
  | "item"
  | "point"
  | "finding"
  | "decision"
  | "owner-request"
  | "carry";

export interface AcceptanceBlocker {
  kind: AcceptanceBlockerKind;
  /** One plain line the owner reads (it becomes `notAcceptedReasons`). */
  detail: string;
  /** The CLI command that clears this blocker. */
  command: string;
}

/** A record id without its phase and candidate prefix: "D-p2d-x-dbe24445-4"
 * → "D-4", "F-p2d-x-M-3" → "F-M-3". */
export function shortId(id: string): string {
  const m = id.match(/^([A-Z]+)-.*?-((?:[MAB]-)?(?:[a-z]+-)?\d+)$/);
  if (!m) return id;
  const tail = m[2].replace(/^(?:disc|trigger)-/, "");
  return `${m[1]}-${tail}`;
}

function shellQuote(value: string): string {
  return `'${value.replace(/'/g, `'\\''`)}'`;
}

/** A decision-view owner command through `tt input`'s ONE writer. `tt input`
 * reads its JSON from the file in its second positional or, when none is
 * given, from stdin — never inline in argv — so the clearing command pipes
 * the payload. */
function inputCommand(phase: PhaseState, payload: Record<string, unknown>): string {
  return `printf %s ${shellQuote(JSON.stringify(payload))} | tt input ${phase.runId}`;
}

function recordBinding(
  phase: PhaseState,
  candidateSha: string,
  contractVersion: ContractVersion,
  recordId: string,
  recordVersion: number,
) {
  return { runId: phase.runId, phaseId: phase.phaseId, recordId, candidateSha, recordVersion, contractVersion };
}

function resolveCommand(phase: PhaseState, C: string, K: ContractVersion, request: PhaseState["ownerRequests"][number]): string {
  const option =
    request.options.find((o) => o.id === "repair")?.id ?? request.options.find((o) => o.id === "accept_risk")?.id ?? request.options[0]?.id ?? "repair";
  return inputCommand(phase, {
    type: "resolve",
    option,
    binding: recordBinding(phase, C, K, request.id, request.version),
  });
}

function acceptFindingCommand(phase: PhaseState, C: string, K: ContractVersion, finding: { id: string; version: number }): string {
  return inputCommand(phase, {
    type: "accept-finding",
    scope: "trade-off",
    binding: recordBinding(phase, C, K, finding.id, finding.version),
  });
}

/** Plan 07f (A1/A3): the ONE owner command that settles a trade-off record:
 * a ruling. It is applied at once and never queues a correction. */
function rulingCommand(phase: PhaseState, detail: string, recordId: string): string {
  return inputCommand(phase, { type: "ruling", text: detail, recordIds: [recordId] });
}

/** A2/C1: how one seat's evidence is resolved against the candidate. The
 * conductor, which owns the candidate's tree, installs its own resolver here
 * (see `setSeatAnchorResolver`); the default is the best-effort check of the
 * process's own tree that a pure fixture or unit test has. */
export type SeatAnchorResolver = (evidence: string) => boolean;

let seatAnchorResolver: SeatAnchorResolver | undefined;

/** Plan 07f (A2): install the resolver `acceptanceBlockers` uses to decide
 * whether a seat's item evidence resolves in the candidate. Only the
 * conductor can see the candidate's own tree (a lane's candidate lives in
 * its worktree, not in the conductor's cwd), so it passes its resolution in
 * here; `undefined` restores the default. */
export function setSeatAnchorResolver(resolver: SeatAnchorResolver | undefined): void {
  seatAnchorResolver = resolver;
}

/** A2/C1: the review-lint anchor check applied to one seat's item evidence:
 * a file:line-range must resolve in the candidate (in range), while a test
 * name or a command the reviewer ran is an anchor by itself. A bare
 * file:line token that names no candidate file is not an anchor. */
function seatAnchorResolves(evidence: string): boolean {
  if (seatAnchorResolver) return seatAnchorResolver(evidence);
  if (!evidenceHasAnchor(evidence)) return false;
  const anchors = evidenceFileAnchors(evidence);
  if (anchors.length === 0) return true;
  return anchors.every((a) => {
    try {
      const lines = readFileSync(a.path, "utf8").split("\n").length;
      return a.start >= 1 && a.end <= lines;
    } catch {
      return false;
    }
  });
}

/** Plan 07f (F1): every condition `accept` requires, in one list. The
 * mechanical gates come first (they are facts, and an owner carry never
 * waives them); then, when there is no carry, the review items, the points
 * whose doneWhen fact does not prove them, the ledger's blocking records, the
 * decisions, the owner requests and the corrections. */
function acceptanceBlockersFor(phase: PhaseState, C: string, K: ContractVersion): AcceptanceBlocker[] {
  const blockers: AcceptanceBlocker[] = [];

  // Mechanical gates are facts (ODP-1, ODP-2): the checks must have passed
  // for C, and the probe must be for C onto the CURRENT integration head. They
  // are evaluated BEFORE the owner's accept-with-carried shortcut, so a carry
  // can never accept a failing or stale candidate.
  if (!(phase.checks && phase.checks.candidateSha === C && phase.checks.passed === true)) {
    const failed = phase.checks?.candidateSha === C && phase.checks.passed === false;
    blockers.push({
      kind: "check",
      detail: failed ? "checks failed" : "checks have not passed for the candidate",
      command: `tt recheck ${phase.runId} --reason ${JSON.stringify("the candidate checks must pass")}`,
    });
  }
  // The probe must be onto the CURRENT integration head: a head moved by a
  // stale publish (design §6.4 step 3) must not let a stale probe count.
  if (
    !(
      phase.probe &&
      phase.probe.candidateSha === C &&
      phase.probe.passed === true &&
      phase.probe.head === phase.integrationHead
    )
  ) {
    const failed = phase.probe?.candidateSha === C && phase.probe.passed === false;
    blockers.push({
      kind: "probe",
      detail: failed ? "integration probe failed" : "the integration probe has not passed for the candidate onto the current head",
      command: `tt resume ${phase.runId}`,
    });
  }

  // Plan 06b (OD-1): a STRUCTURED phase additionally requires its `test`
  // verifies passed, no unaccepted `:WHERE:` symbol deviation, and every
  // `evidence` item recorded. These are MECHANICAL item gates (ODP-3): they are
  // evaluated BEFORE the owner's carry shortcut, so a carry can never waive a
  // missing named test verify, a symbol deviation or unrecorded evidence. The
  // item REVIEW VERDICTS are review items the carry does waive. An old-format
  // phase declares no items and keeps today's rule.
  const itemsEnforced = isStructured(phase.contract);
  const items = itemsEnforced ? itemsFromPhase(phase.contract) : undefined;
  let acceptedDeviations: Set<string> | undefined;
  let evidenceRecorded: string[] | undefined;
  if (itemsEnforced) {
    for (const problem of testVerifyProblems(phase.checkResolution ?? [])) {
      blockers.push({
        kind: "test-verify",
        detail: `named test verify ${problem}`,
        command: `tt recheck ${phase.runId} --reason ${JSON.stringify(problem)}`,
      });
    }
    // An architecture deviation is accepted when the owner accepts the item's
    // own blocking finding as a trade-off (`accept_risk`), or when it is
    // recorded in `acceptedDeviations`.
    acceptedDeviations = new Set([
      ...(phase.acceptedDeviations ?? []),
      ...phase.findings.filter((f) => f.itemId && f.status === "accepted").map((f) => f.itemId!),
    ]);
    // A `:WHERE:` symbol the conductor could not find is a deviation the
    // owner must accept before acceptance, whatever the seats said.
    for (const id of phase.archSymbolDeviations ?? []) {
      if (acceptedDeviations.has(id)) continue;
      const deviationFinding = phase.findings.find((f) => f.itemId === id);
      blockers.push({
        kind: "symbol-deviation",
        detail: `architecture symbol deviation ${id} is not accepted`,
        command: deviationFinding
          ? acceptFindingCommand(phase, C, K, deviationFinding)
          : `tt resume ${phase.runId}`,
      });
    }
    evidenceRecorded = (phase.itemEvidence ?? []).map((e) => e.id);
    // Every `evidence` item is owner-recorded, never waived by a carry.
    for (const item of flatItems(items!)) {
      if (!itemNeedsEvidence(item) || evidenceRecorded.includes(item.id)) continue;
      blockers.push({
        kind: "evidence",
        detail: `evidence for ${item.id} has not been recorded`,
        command: `tt evidence ${phase.runId} ${item.id} ${shellQuote(`evidence for ${item.id}`)}`,
      });
    }
  }

  // Plan 06g (A6/ODP-2/ODP-3): the owner's "accept with carried items" decision
  // at the end of the round budget accepts the candidate the carry was given
  // for (`carriedCandidateSha`), waiving only the OPEN REVIEW ITEMS — the
  // review verdicts, open findings, decisions and corrections that are carried.
  // It never waives the mechanical gates above.
  if (phase.acceptedWithCarried) {
    if (phase.carriedCandidateSha !== C) {
      blockers.push({
        kind: "carry",
        detail: `the carried-items acceptance is for ${phase.carriedCandidateSha ?? "another candidate"}, not ${C}`,
        command: `tt resume ${phase.runId}`,
      });
    }
    return blockers;
  }

  for (const who of seatsOf(phase.contract)) {
    const review = phase.reviews[who]?.review;
    if (!review) {
      blockers.push({ kind: "review", detail: `review by ${who} is missing`, command: `tt resume ${phase.runId}` });
      continue;
    }
    if (review.candidateSha !== C || !sameVersion(review.contractVersion, K)) {
      blockers.push({
        kind: "review",
        detail: `review by ${who} is not bound to this candidate and contract`,
        command: `tt resume ${phase.runId}`,
      });
    }
  }

  if (itemsEnforced) {
    // Plan 07f (F0/F1): an item is met only by a strict seat majority whose
    // own anchors pass the review-lint anchor check. An evaluator's `unchecked`
    // entry is not a seat verdict, so it can never make or unmake this
    // majority; a `contradicted` entry is applied earlier, as an overturn.
    const needed = Math.floor(seatsOf(phase.contract).length / 2) + 1;
    const outcomes = phaseItemOutcomes(phase);
    for (const item of flatItems(items!)) {
      if (item.kind === "architecture" && acceptedDeviations!.has(item.id)) continue;
      if (itemNeedsEvidence(item) && !evidenceRecorded!.includes(item.id)) continue;
      const outcome = outcomes.find((o) => o.item.id === item.id);
      const required = item.kind === "architecture" ? "fits" : "met";
      const resolving = outcome
        ? outcome.seats.filter((s) => s.verdict === required && seatAnchorResolves(s.evidence)).length
        : 0;
      if (resolving >= needed) continue;
      const split = outcome ? itemIsSplit(outcome) : false;
      blockers.push({
        kind: "item",
        detail:
          `${item.id} is ${outcome?.outcome ?? "incomplete"}` +
          (split ? " (the seats split on it)" : "") +
          (outcome && outcome.seats.some((s) => s.verdict === required) ? " without a majority of resolving anchors" : ""),
        command: `tt resume ${phase.runId}`,
      });
    }
  }

  // Plan 07c/07f: every OPEN domain point must be closed by a doneWhen fact
  // that passes on this candidate and not on its base, or by the evaluator's
  // approved trade-off. A failing fact keeps it open whatever an evaluator
  // drop says. Points are grouped by domain so the owner reads one blocker per
  // domain, naming the point ids.
  const facts = phase.doneWhenFacts ?? [];
  const open = openFindings(phase);
  const pointIds = new Set<string>();
  const pointsByDomain = new Map<string, string[]>();
  for (const finding of open) {
    if (!finding.domain && !finding.doneWhen) continue;
    const proves = pointHasProvingFact(phase, finding);
    // Plan 07f (F0/A1): a point is closed by a proving fact (or a domain
    // review that the conductor records as a closed finding). An evaluator's
    // trade-off triage record alone is not a closure — the owner accepts the
    // finding, or the fact/domain review proves it.
    if (!factKeepsOpen(finding, facts, C) && proves) continue;
    pointIds.add(finding.id);
    const key = finding.domain ?? "ungrouped";
    pointsByDomain.set(key, [...(pointsByDomain.get(key) ?? []), finding.id]);
  }
  for (const [domain, ids] of pointsByDomain) {
    const names = ids.map(shortId).join(", ");
    blockers.push({
      kind: "point",
      detail: `open point${ids.length === 1 ? "" : "s"} in ${domain}: ${names} — no proving fact closes ${ids.length === 1 ? "it" : "them"}`,
      // Plan 07f (A1): a point is a record with a domain and its next fix loop,
      // not an owner decision. `tt why` prints the point with its domain and
      // the command that shows the next step; no owner command can close it.
      command: `tt why ${phase.runId}`,
    });
  }

  // Plan 07f (A2): triage dispositions are no longer acceptance gates of
  // their own (a `fix` on a finding, a decision or an amendment no longer
  // blocks acceptance by itself). What remains is a blocking finding that is
  // still OPEN — a point or a trade-off the owner closes with a ruling.
  const covered = new Set(pointIds);
  for (const finding of open) {
    if (finding.severity !== "blocking" || covered.has(finding.id)) continue;
    // Plan 07f (A2): a disposition `tradeoff` settles the finding; its old
    // blocking label no longer blocks acceptance. An untriaged finding still
    // blocks (fail safe).
    if (findingDispositionIsTradeoff(phase.triage, finding.id)) continue;
    blockers.push({
      kind: "finding",
      detail: `blocking finding ${shortId(finding.id)} open`,
      command: rulingCommand(phase, `accept ${shortId(finding.id)}`, finding.id),
    });
  }

  // Plan 07f (F1/A2): one blocker (and one ruling) per unsettled decision, so
  // every outstanding decision has its own visible clearing action. A record
  // bound to an older candidate or contract version was closed when the phase
  // moved (conductor records DECISION_CLOSED); it never blocks acceptance.
  for (const decision of phase.decisions) {
    if (!isLiveDecision(decision)) continue;
    // Plan 01g: an amendment record is never an acceptance blocker by itself.
    if (decision.amendment) continue;
    if (decisionSettled(decision, phase, C, K)) continue;
    if (decision.boundCandidateSha !== C || !sameVersion(decision.boundContractVersion, K)) continue;
    const why = decisionStatus(decision, phase).reason ?? "failed";
    const veto = /([A-Za-z0-9_]+) veto/.exec(why);
    const verb = veto ? `vetoed by ${veto[1]}` : `failed (${why})`;
    blockers.push({
      kind: "decision",
      detail: `${shortId(decision.id)} ${verb}`,
      command: rulingCommand(phase, `rule ${shortId(decision.id)} as recorded`, decision.id),
    });
  }

  // Plan 06i: a non-blocking owner request (a late discovery's escalation)
  // is visible but does not gate acceptance; an open request whose linked
  // item is already closed is moot and does not gate it either.
  for (const request of phase.ownerRequests) {
    if (request.status !== "open" || request.blocking === false) continue;
    if (ownerRequestIsStale(phase, request) || budgetGateIsMoot(phase, request)) continue;
    blockers.push({
      kind: "owner-request",
      detail: `owner request ${shortId(request.id)} is open: ${request.reason}`,
      command: resolveCommand(phase, C, K, request),
    });
  }

  // Plan 07f (A3/C2, design §3.3): a correction is a point for its domain's
  // next fix loop, never an acceptance obligation; a ruling applies at once.
  // Acceptance therefore never waits for a correction to reach a worker and
  // keeps no separate corrections/queued-corrections gates.

  return blockers;
}

/** Plan 07f (F1): every unmet acceptance condition for the phase's current
 * candidate, each with the command that clears it. Empty ⇒ accepted. */
export function acceptanceBlockers(phase: PhaseState): AcceptanceBlocker[] {
  const C = phase.candidate?.sha;
  if (!C) return [];
  return acceptanceBlockersFor(phase, C, phase.contract.contractVersion);
}

/** accept(C, K): the only way a phase may reach ACCEPTED. Mechanical by
 * default (plan 07f F0): acceptance IS "no blockers". */
export function accept(phase: PhaseState, C: string, K: ContractVersion): boolean {
  if (phase.candidate?.sha !== C) return false;
  return acceptanceBlockersFor(phase, C, K).length === 0;
}

/** Plan 06b: true when the ONLY thing keeping the phase from acceptance is an
 * unrecorded `evidence` item. Then the phase parks AWAITING_OWNER naming the
 * item instead of spending a repair round the worker cannot satisfy. */
/** The `evidence` items not yet recorded. */
export function pendingEvidenceItems(phase: PhaseState) {
  if (!isStructured(phase.contract)) return [];
  const items = itemsFromPhase(phase.contract);
  const recorded = new Set((phase.itemEvidence ?? []).map((e) => e.id));
  return flatItems(items).filter((i) => itemNeedsEvidence(i) && !recorded.has(i.id));
}

/** True when the phase has at least one `evidence` item and every one is
 * recorded. */
export function evidenceAllRecorded(phase: PhaseState): boolean {
  if (!isStructured(phase.contract)) return false;
  const ev = flatItems(itemsFromPhase(phase.contract)).filter((i) => itemNeedsEvidence(i));
  if (ev.length === 0) return false;
  const recorded = new Set((phase.itemEvidence ?? []).map((e) => e.id));
  return ev.every((i) => recorded.has(i.id));
}

export function evidenceOnlyPending(phase: PhaseState): boolean {
  if (!isStructured(phase.contract)) return false;
  if (!phase.candidate) return false;
  const pending = pendingEvidenceItems(phase);
  if (pending.length === 0) return false;
  // Everything else acceptable: pretend the evidence is recorded and ask
  // accept() whether only the evidence stood in the way.
  const withEvidence: PhaseState = {
    ...phase,
    itemEvidence: [...(phase.itemEvidence ?? []), ...pending.map((i) => ({ id: i.id, text: "pending" }))],
  };
  return accept(withEvidence, phase.candidate.sha, phase.contract.contractVersion);
}

/** done(phase) ⇔ accept(C, K) ∧ the integration branch points at the probed I. */
export function done(phase: PhaseState): boolean {
  if (!phase.candidate || !phase.probe?.probedI) return false;
  if (!accept(phase, phase.candidate.sha, phase.contract.contractVersion)) return false;
  return phase.publishedI === phase.probe.probedI;
}

/** Plan 2c: the state of one decision record for the current candidate, as
 * the tally and the owner rules see it — exposed through `tt state` so the
 * status and decision views never infer it from individual ballots. */
export interface DecisionStatus {
  status: "superseded" | "detail" | "passed" | "failed" | "suspended" | "pending" | "owner";
  reason?: string;
  /** A reserved decision: voted like any other, shown to the owner. */
  flagged?: boolean;
  /** Plan 01g: set on an amendment record so the status and the decision
   * view can show `⚑ AMENDED` with the old and new wording, without
   * inferring it from the decision's own choice. */
  amendment?: CriterionAmendment;
}

export function decisionStatus(decision: Decision, phase: PhaseState): DecisionStatus {
  if (decision.supersededByCorrection) return { status: "superseded", reason: `by correction ${decision.supersededByCorrection}` };
  if (decision.supersededBy) return { status: "superseded", reason: decision.supersededBy };
  if (decision.class === "detail") return { status: "detail" };
  const amendment = decision.amendment;
  // Plan 01g: an applied or reverted amendment is history — it is never
  // re-tallied against a later candidate (its ballots belonged to the
  // candidate it passed on) and never reads as a failed decision, which
  // would wrongly appear in the verdict.
  if (amendment && (amendment.status === "applied" || amendment.status === "reverted")) {
    return {
      status: "passed",
      reason: amendment.status === "applied" ? "amendment applied" : "reverted by the owner",
      flagged: true,
      amendment,
    };
  }
  const C = phase.candidate?.sha;
  const K = phase.contract.contractVersion;
  if (!C || C !== decision.boundCandidateSha) return { status: "pending", reason: "not bound to the current candidate", ...(amendment ? { amendment } : {}) };
  const flag = decision.class === "reserved" ? { flagged: true } : {};
  return { ...votedStatus(decision, phase, C, K), ...flag, ...(amendment ? { amendment } : {}) };
}

function votedStatus(decision: Decision, phase: PhaseState, C: string, K: ContractVersion): DecisionStatus {
  const seats = seatsOf(phase.contract);
  const leader = leaderOf(phase.contract);
  if (decisionSettled(decision, phase, C, K)) {
    const t = tally(decision, phase.ballots, phase.findings, C, K, seats, leader);
    return { status: "passed", reason: t === "pass" ? "vote passed" : "settled by the owner" };
  }
  const t = tally(decision, phase.ballots, phase.findings, C, K, seats, leader);
  if (t === "suspended") return { status: "suspended", reason: `linked finding ${decision.linkedFindingId} is open` };
  const vote = (who: string) => {
    const b = currentBallot(phase.ballots, decision.id, who, C, K, decision.version);
    return isValidBallot(b) ? b.vote : undefined;
  };
  const reviewsDone = seats.every((w) => phase.reviews[w]?.review?.candidateSha === C);
  const leaderVote = vote(leader);
  const others = seats.filter((s) => s !== leader);
  const missingOthers = others.filter((s) => vote(s) === undefined);
  if (!reviewsDone && (leaderVote === undefined || missingOthers.length === others.length)) return { status: "pending", reason: "votes not cast yet" };
  const reasons: string[] = [];
  if (leaderVote === undefined) reasons.push(`missing ballot from ${leader}`);
  else if (leaderVote === "reject") reasons.push(`${leader} veto`);
  const otherApprovals = others.filter((s) => vote(s) === "approve").length;
  if (seats.length === 3 && others.length === 2) {
    // The default `M A B` keeps the exact 06g wording.
    const a = vote(others[0]);
    const b = vote(others[1]);
    if (a !== "approve" && b !== "approve") {
      reasons.push(a === undefined && b === undefined ? `no ballot from ${others[0]} or ${others[1]}` : `neither ${others[0]} nor ${others[1]} approved`);
    }
  } else {
    const needed = Math.floor(seats.length / 2) + 1;
    const approvals = (leaderVote === "approve" ? 1 : 0) + otherApprovals;
    if (approvals < needed) reasons.push(`only ${approvals} of ${seats.length} seats approved`);
  }
  return { status: "failed", reason: reasons.join("; ") || "vote failed" };
}
