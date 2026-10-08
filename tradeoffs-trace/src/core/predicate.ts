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

import { currentBallot, isValidBallot, tally } from "./tally.ts";
// Plan 06b: the per-item acceptance half — every R and C met by majority with
// its test verifies passed, every A fitting by majority or its deviation
// accepted by the owner, and every evidence item recorded.
import { flatItems, isStructured, itemNeedsEvidence, itemsAccept, itemsFromPhase, phaseItemOutcomes, tallyItems, testVerifyProblems, thinMetItems, workerAnchorsOf } from "./items.ts";
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
export function reviewIngestionIssue(review: Review): string | undefined {
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
  return undefined;
}

/** addressed(X, C, K): true iff M, A and B each stated, in a review bound to
 * (C, K), that correction X is honored — and none stated it is not. Fails
 * closed on malformed or legacy data: a missing statement, more than one
 * statement for X (identical or contradictory, in either order), or any
 * status that is not exactly `"honored"` all make it false. */
export function addressed(correction: Correction, phase: PhaseState, C: string, K: ContractVersion): boolean {
  if (!sameVersion(correction.boundContractVersion, K)) return false;
  for (const who of ["M", "A", "B"] as const) {
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
export function typesNeedingEvaluation(phase: PhaseState): MessageType[] {
  const types = (EVALUATOR_TYPES as readonly MessageType[]).filter((t) =>
    (phase.messages ?? []).some((m) => m.type === t && (m.state === "raw" || m.state === "refused")),
  );
  // Plan 06b (OD-1 R3b): a majority `unmet`/`deviates` needs the evaluator's
  // substantive re-check against the candidate before it blocks. Force one
  // `finding` evaluator pass for it when no finding message already asks.
  if (itemsNeedingEvaluatorReverify(phase) && !types.includes("finding")) types.push("finding");
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
  // Plan 06c (R5): a unanimous thin met/fits is audited by the evaluator.
  return thinMetItems(outcomes, workerAnchorsOf(phase.coverage)).length > 0;
}

/** Plan 04a: whether everything EVALUATING waits for has settled. In 04a
 * that is one fresh evaluator per message type that has work: the predicate
 * holds when every such type's evaluator has finished or timed out. Plan 04b
 * adds its panels to this same predicate. */
// ---------------------------------------------------------------------------
// Plan 04b: the blocker panel
// ---------------------------------------------------------------------------

/** The three seats every panel has. */
export const PANEL_SEATS = [1, 2, 3] as const;

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

export function panelSeatsSettled(panel: PanelState | undefined): boolean {
  return PANEL_SEATS.every((n) => panelSeatSettled(panel?.seats?.[String(n)]));
}

/** The panel's verdict from its recorded votes: a majority `block` escalates,
 * a majority `downgrade` downgrades, anything else (including a split or two
 * unavailable seats) is incomplete. Only the core computes this; the
 * conductor may not invent an outcome. */
export function panelOutcome(panel: PanelState): PanelOutcome {
  const votes = Object.values(panel.seats ?? {})
    .map((s) => s.vote)
    .filter((v): v is "block" | "downgrade" => v === "block" || v === "downgrade");
  const blocks = votes.filter((v) => v === "block").length;
  const downgrades = votes.filter((v) => v === "downgrade").length;
  if (blocks >= 2) return "escalate";
  if (downgrades >= 2) return "downgrade";
  return "incomplete";
}

/** The options a `block` majority puts to the owner: every block vote's own
 * options, in seat order, deduped by id and capped at three (a `block` vote
 * proposes two or three). Falls back to DEFAULT_BLOCKER_OPTIONS so an
 * escalation always has options. */
export function panelOptionsFor(panel: PanelState): PanelOption[] {
  const out: PanelOption[] = [];
  for (const n of PANEL_SEATS) {
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

export function roundPanelSeatsSettled(round: RoundPanelState | undefined): boolean {
  return PANEL_SEATS.every((n) => roundPanelSeatSettled(round?.seats?.[String(n)]));
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
        const all = (["M", "A", "B"] as const).every((who) =>
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
export function roundPanelOutcomeFor(round: RoundPanelState | undefined, messageId: string, kind: "tradeoff" | "finding"): RoundPanelOutcome {
  const votes = PANEL_SEATS.map((n) => round?.seats?.[String(n)]?.votes?.find((v) => v.messageId === messageId)).filter(
    (v): v is NonNullable<typeof v> => v !== undefined,
  );
  const count = (verdict: string) => votes.filter((v) => v.verdict === verdict).length;
  if (count("keep") >= 2) return "keep";
  if (count("drop") >= 2) return "drop";
  if (count("downgrade") >= 2) return "downgrade";
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
  return (["M", "A", "B"] as const).every((who) => {
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
      tally(d, phase.ballots, phase.findings, C, K) === "pass",
  );
}

export function decisionSettled(decision: Decision, phase: PhaseState, C: string, K: ContractVersion): boolean {
  if (decision.class === "detail") return true;

  // delegated and reserved alike: the reviewers' vote, an owner override,
  // or the owner accepting it after a failed vote. A reserved decision is
  // flagged for the owner (DecisionStatus.flagged), never held for them.
  if (tally(decision, phase.ballots, phase.findings, C, K) === "pass") return true;
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

/** accept(C, K): the only way a phase may reach ACCEPTED. */
export function accept(phase: PhaseState, C: string, K: ContractVersion): boolean {
  // Mechanical gates are facts (ODP-1, ODP-2): the checks must have passed
  // for C, and the probe must be for C onto the CURRENT integration head. They
  // are evaluated BEFORE the owner's accept-with-carried shortcut, so a carry
  // can never accept a failing or stale candidate.
  if (!(phase.checks && phase.checks.candidateSha === C && phase.checks.passed === true)) {
    return false;
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
    return false;
  }

  // Plan 06b (OD-1): a STRUCTURED phase additionally requires its `test`
  // verifies passed, no unaccepted `:WHERE:` symbol deviation, and every
  // `evidence` item recorded. These are MECHANICAL item gates (ODP-3): they are
  // evaluated BEFORE the owner's carry shortcut, so a carry can never waive a
  // missing named test verify, a symbol deviation or unrecorded evidence. The
  // item REVIEW VERDICTS (the tally below) are review items the carry does
  // waive. An old-format phase declares no items and keeps today's rule.
  const itemsEnforced = isStructured(phase.contract);
  const items = itemsEnforced ? itemsFromPhase(phase.contract) : undefined;
  let acceptedDeviations: Set<string> | undefined;
  let evidenceRecorded: string[] | undefined;
  if (itemsEnforced) {
    if (testVerifyProblems(phase.checkResolution ?? []).length > 0) return false;
    // An architecture deviation is accepted when the owner accepts the item's
    // own blocking finding as a trade-off (`accept_risk`), or when it is
    // recorded in `acceptedDeviations`.
    acceptedDeviations = new Set([
      ...(phase.acceptedDeviations ?? []),
      ...phase.findings.filter((f) => f.itemId && f.status === "accepted").map((f) => f.itemId!),
    ]);
    // A `:WHERE:` symbol the conductor could not find is a deviation the
    // owner must accept before acceptance, whatever the seats said.
    if ((phase.archSymbolDeviations ?? []).some((id) => !acceptedDeviations!.has(id))) return false;
    evidenceRecorded = (phase.itemEvidence ?? []).map((e) => e.id);
    // Every `evidence` item is owner-recorded, never waived by a carry.
    if (flatItems(items!).some((i) => itemNeedsEvidence(i) && !evidenceRecorded!.includes(i.id))) return false;
  }

  // Plan 06g (A6/ODP-2/ODP-3): the owner's "accept with carried items" decision
  // at the end of the round budget accepts the candidate the carry was given
  // for (`carriedCandidateSha`), waiving only the OPEN REVIEW ITEMS — the
  // review verdicts, open findings, decisions and corrections that are carried.
  // It never waives the mechanical gates above (checks, probe, test verifies,
  // symbol deviations, evidence), the candidate binding, or another
  // candidate's gate/probe.
  if (phase.acceptedWithCarried) return phase.carriedCandidateSha === C;

  for (const who of ["M", "A", "B"] as const) {
    const review = phase.reviews[who]?.review;
    if (!review) return false;
    if (review.candidateSha !== C || !sameVersion(review.contractVersion, K)) return false;
  }

  if (itemsEnforced) {
    const reviews = (["M", "A", "B"] as const).map((seat) => {
      const r = phase.reviews[seat]?.review;
      return { seat, items: r ? { items: r.items ?? [], arch: r.arch ?? [] } : undefined };
    });
    const outcomes = tallyItems(items!, reviews, phase.overturns ?? []);
    if (
      !itemsAccept(items!, outcomes, {
        acceptedDeviations: [...acceptedDeviations!],
        evidenceRecorded: evidenceRecorded!,
      })
    ) {
      return false;
    }
  }

  if (phase.findings.some((f) => f.severity === "blocking" && f.status === "open")) {
    return false;
  }

  for (const decision of phase.decisions) {
    // A decision superseded by a correction (design §7.5 step 1) is
    // historical: the correction, not the original decision's vote or
    // owner resolution, is what acceptance now depends on (via the
    // corrections/addressed check below).
    if (!isLiveDecision(decision)) continue;
    // Plan 01g: an amendment record is never an acceptance blocker by
    // itself. A passing one is applied (next.ts's `apply_amendment`); a
    // failed one leaves the criterion unchanged, and the round is handled
    // exactly as today — a dispute must not, by itself, consume a repair
    // round or park the run on the owner.
    if (decision.amendment) continue;
    if (!decisionSettled(decision, phase, C, K)) return false;
  }

  if (phase.ownerRequests.some((r) => r.status === "open")) {
    return false;
  }

  for (const correction of phase.corrections) {
    if (correction.status !== "open") continue; // resolved/superseded already settled
    if (!addressed(correction, phase, C, K)) return false;
  }

  return true;
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
  if (!C || decision.boundCandidateSha !== C) return { status: "pending", reason: "not bound to the current candidate", ...(amendment ? { amendment } : {}) };
  const flag = decision.class === "reserved" ? { flagged: true } : {};
  return { ...votedStatus(decision, phase, C, K), ...flag, ...(amendment ? { amendment } : {}) };
}

function votedStatus(decision: Decision, phase: PhaseState, C: string, K: ContractVersion): DecisionStatus {
  if (decisionSettled(decision, phase, C, K)) {
    const t = tally(decision, phase.ballots, phase.findings, C, K);
    return { status: "passed", reason: t === "pass" ? "vote passed" : "settled by the owner" };
  }
  const t = tally(decision, phase.ballots, phase.findings, C, K);
  if (t === "suspended") return { status: "suspended", reason: `linked finding ${decision.linkedFindingId} is open` };
  const vote = (who: "M" | "A" | "B") => {
    const b = currentBallot(phase.ballots, decision.id, who, C, K, decision.version);
    return isValidBallot(b) ? b.vote : undefined;
  };
  const reviewsDone = (["M", "A", "B"] as const).every((w) => phase.reviews[w]?.review?.candidateSha === C);
  const m = vote("M");
  const a = vote("A");
  const b = vote("B");
  if (!reviewsDone && (m === undefined || (a === undefined && b === undefined))) return { status: "pending", reason: "votes not cast yet" };
  const reasons: string[] = [];
  if (m === undefined) reasons.push("missing ballot from M");
  else if (m === "reject") reasons.push("M veto");
  if (a !== "approve" && b !== "approve") {
    reasons.push(a === undefined && b === undefined ? "no ballot from A or B" : "neither A nor B approved");
  }
  return { status: "failed", reason: reasons.join("; ") || "vote failed" };
}
