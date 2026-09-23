// design §4.2 ("a finding still open when the repair budget runs out
// becomes an owner request"), §5.2 ("a decision that keeps failing...
// becomes an owner request carrying every ballot"), §8.1 ("open items
// become owner requests") and §7.5 (an exhausted correction allowance ->
// owner request): every transition into AWAITING_OWNER must leave the
// owner something to resolve. This is the single place that decides what
// those requests are, so every entry point (transitions.ts's
// budget-exhausted rows) produces them the same way.
//
// Every option offered has a defined effect (round-3 review item 1) and is
// named by a stable id, never by its label — see owner-commands.ts for what
// choosing each one does.

import { addressed, decisionSettled, isLiveDecision, reviewsComplete } from "./predicate.ts";
import type { OwnerRequest, OwnerRequestOption, PhaseState } from "./types.ts";

function hasOpenRequestLinkedTo(phase: PhaseState, matches: (r: OwnerRequest) => boolean): boolean {
  return phase.ownerRequests.some((r) => r.status === "open" && matches(r));
}

export const BUDGET_GATE_OPTIONS: OwnerRequestOption[] = [
  { id: "grant", label: "grant 3 more repair rounds" },
  { id: "stop", label: "stop the phase" },
];

export const FAILED_VOTE_OPTIONS: OwnerRequestOption[] = [
  { id: "accept_as_implemented", label: "accept the decision as implemented" },
  { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" },
];

export const RESERVED_DECISION_OPTIONS: OwnerRequestOption[] = [
  { id: "approve", label: "approve" },
  { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" },
];

export const UNADDRESSED_CORRECTION_OPTIONS: OwnerRequestOption[] = [
  { id: "grant_correction", label: "grant 3 more rounds for this correction" },
  { id: "withdraw", label: "withdraw the correction" },
];

/** design §4.2/§4.3: a `contract` finding's disposition is amending the
 * contract, not accepting the risk — so "accept the risk" is not offered
 * for one; only "repair" is. Every other finding kind offers both. */
export function openFindingOptions(kind: "defect" | "contract" | "integration"): OwnerRequestOption[] {
  if (kind === "contract") {
    return [{ id: "repair", label: "repair (grant 3 rounds)" }];
  }
  return [
    { id: "accept_risk", label: "accept the risk" },
    { id: "repair", label: "repair (grant 3 rounds)" },
  ];
}

/** The options that grant a fresh repair allowance without settling the
 * item they're attached to — resolving one of these always routes directly
 * to REPAIRING (never RESOLVING), design §5.2/§4.2/§7.5. */
export function isRepairForcingOption(origin: OwnerRequest["origin"], optionId: string): boolean {
  return (
    (origin === "failed_vote" && optionId === "reject_and_repair") ||
    (origin === "reserved_decision" && optionId === "reject_and_repair") ||
    (origin === "open_finding" && optionId === "repair") ||
    (origin === "unaddressed_correction" && optionId === "grant_correction")
  );
}

/** True for the plain gate-failure request (no record it is linked to) —
 * the one whose options are exactly BUDGET_GATE_OPTIONS (design §8.1). */
export function isBudgetGateRequest(r: OwnerRequest): boolean {
  return r.origin === "repair_budget_exhausted" && !r.linkedDecisionId && !r.linkedFindingId && !r.linkedCorrectionId;
}

/** The owner requests a phase needs when it is about to enter
 * AWAITING_OWNER, given its state right before that transition. `cause` is
 * used only for the fallback "gate failed with no record-level item"
 * request (e.g. every worker attempt timed out before a candidate ever
 * existed). Requests already open for the same record are not duplicated,
 * and an item already settled some other way is not requested again. */
export function openItemOwnerRequestsFor(phase: PhaseState, cause: string): OwnerRequest[] {
  const created: OwnerRequest[] = [];
  const C = phase.candidate?.sha;
  const K = phase.contract.contractVersion;
  let n = phase.ownerRequests.length;
  const nextId = (tag: string) => {
    n += 1;
    return `OR-${phase.phaseId}-${tag}-${n}`;
  };

  // one per open blocking finding
  for (const f of phase.findings) {
    if (f.status !== "open" || f.severity !== "blocking") continue;
    if (hasOpenRequestLinkedTo(phase, (r) => r.linkedFindingId === f.id)) continue;
    if (created.some((r) => r.linkedFindingId === f.id)) continue;
    created.push({
      id: nextId("finding"),
      version: 1,
      phaseId: phase.phaseId,
      reason: `finding ${f.id} (${f.kind}) is still open: ${f.evidence}`,
      origin: "open_finding",
      linkedFindingId: f.id,
      boundCandidateSha: C,
      boundContractVersion: K,
      options: openFindingOptions(f.kind),
      status: "open",
    });
  }

  if (C) {
    // one per failing, unsettled delegated decision, carrying its ballots.
    // `decisionSettled` (not a raw tally check) so a decision already
    // settled via "accept the decision as implemented" or an override is
    // never asked about again (round-3 review item 1's closing test).
    // Only once the reviews of C are in: a decision nobody voted on (checks
    // or the probe failed before review) did not fail a vote, and asking the
    // owner to settle it misstates what happened (run 0c99b1ff: five "failed
    // its vote" requests after a checks failure, no review ever ran). The
    // budget gate below covers that case.
    const voted = reviewsComplete(phase, C, K);
    for (const d of phase.decisions) {
      if (!voted) break;
      if (!isLiveDecision(d) || (d.class !== "delegated" && d.class !== "reserved")) continue;
      if (decisionSettled(d, phase, C, K)) continue;
      if (hasOpenRequestLinkedTo(phase, (r) => r.linkedDecisionId === d.id)) continue;
      if (created.some((r) => r.linkedDecisionId === d.id)) continue;
      created.push({
        id: nextId("decision"),
        version: 1,
        phaseId: phase.phaseId,
        reason: `decision ${d.id} ("${d.choice}") failed its vote`,
        origin: "failed_vote",
        linkedDecisionId: d.id,
        relatedBallots: phase.ballots.filter((b) => b.decisionId === d.id),
        boundCandidateSha: C,
        boundContractVersion: K,
        options: FAILED_VOTE_OPTIONS,
        status: "open",
      });
    }

    // one per unaddressed open correction
    for (const c of phase.corrections) {
      if (c.status !== "open" || addressed(c, phase, C, K)) continue;
      if (hasOpenRequestLinkedTo(phase, (r) => r.linkedCorrectionId === c.id)) continue;
      if (created.some((r) => r.linkedCorrectionId === c.id)) continue;
      created.push({
        id: nextId("correction"),
        version: 1,
        phaseId: phase.phaseId,
        reason: `correction ${c.id} is not yet addressed`,
        origin: "unaddressed_correction",
        linkedCorrectionId: c.id,
        boundCandidateSha: C,
        boundContractVersion: K,
        options: UNADDRESSED_CORRECTION_OPTIONS,
        status: "open",
      });
    }

    // Reserved decisions get no owner request of their own (owner-optional):
    // they are voted like delegated ones above and only flagged for the owner.
  }

  if (created.length === 0) {
    // A gate failed with no record-level item to blame (design §8.1's own
    // escape): worker attempts, freezes, checks or probes kept failing.
    created.push({
      id: nextId("gate"),
      version: 1,
      phaseId: phase.phaseId,
      reason: cause,
      origin: "repair_budget_exhausted",
      boundCandidateSha: C,
      boundContractVersion: K,
      options: BUDGET_GATE_OPTIONS,
      status: "open",
    });
  }

  return created;
}
