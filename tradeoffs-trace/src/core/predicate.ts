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

import { tally } from "./tally.ts";
import type { ContractVersion, Correction, Decision, PhaseState, Review } from "./types.ts";

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
export function decisionSettled(decision: Decision, phase: PhaseState, C: string, K: ContractVersion): boolean {
  if (decision.class === "detail") return true;

  if (decision.class === "delegated") {
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
    return settledByOwnerRequest(phase, "failed_vote", decision.id, "accept_as_implemented", C, K);
  }

  // reserved: settled only by the "approve" option of a resolved
  // reserved_decision request bound to (C, K).
  return settledByOwnerRequest(phase, "reserved_decision", decision.id, "approve", C, K);
}

/** accept(C, K): the only way a phase may reach ACCEPTED. */
export function accept(phase: PhaseState, C: string, K: ContractVersion): boolean {
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

  for (const who of ["M", "A", "B"] as const) {
    const review = phase.reviews[who]?.review;
    if (!review) return false;
    if (review.candidateSha !== C || !sameVersion(review.contractVersion, K)) return false;
  }

  if (phase.findings.some((f) => f.severity === "blocking" && f.status === "open")) {
    return false;
  }

  for (const decision of phase.decisions) {
    // A decision superseded by a correction (design §7.5 step 1) is
    // historical: the correction, not the original decision's vote or
    // owner resolution, is what acceptance now depends on (via the
    // corrections/addressed check below).
    if (decision.supersededByCorrection) continue;
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

/** done(phase) ⇔ accept(C, K) ∧ the integration branch points at the probed I. */
export function done(phase: PhaseState): boolean {
  if (!phase.candidate || !phase.probe?.probedI) return false;
  if (!accept(phase, phase.candidate.sha, phase.contract.contractVersion)) return false;
  return phase.publishedI === phase.probe.probedI;
}
