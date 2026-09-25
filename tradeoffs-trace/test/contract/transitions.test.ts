// Transition-table check (phase-0 Goal 4, round-1 review item 1):
//
//  1. Every row in transitions.ts is exercised: a state is built that
//     satisfies the row's from-state + guard, reduce() with the row's
//     trigger produces the row's to-state, AND `next()` of the resulting
//     state equals that row's `actions` exactly — for every row, not just a
//     hand-picked subset, since next() is now a total function of state
//     (see next.ts's docstring for why the earlier AUTOMATIC/REACTIVE split
//     is gone).
//  2. Calling next() again after the matching ACTION_STARTED for a
//     dispatch-shaped action returns [] (no double dispatch).
//  3. A state rebuilt by folding the same event list from scratch gives the
//     same next() as the incrementally-reduced one (a recovery property).
//  4. Every edge in design §6.1's own transcription (quoted below, plus the
//     §8.1/§9.3/§7.3/§7.5/§6.4 additions the phase-0 brief lists) has a
//     matching row, so the document and the code cannot drift silently.

import assert from "node:assert/strict";
import { test } from "node:test";
import { next } from "../../src/core/next.ts";
import { reduce } from "../../src/core/reduce.ts";
import { TRANSITIONS } from "../../src/core/transitions.ts";
import type { Event, Finding, PhaseStateName, State } from "../../src/core/types.ts";
import { approvingReview, baseState, CV } from "../unit/helpers.ts";

const K = CV();
const C1 = { sha: "C1", contractVersion: K };

function openBlockingFinding(overrides: Partial<Finding> = {}): Finding {
  return {
    id: "F-open",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "x",
    raisedBy: "B",
    status: "open",
    boundCandidateSha: "C1",
    ...overrides,
  };
}

const acceptableReviews = {
  M: { review: approvingReview("M", "C1", K) },
  A: { review: approvingReview("A", "C1", K) },
  B: { review: approvingReview("B", "C1", K) },
};

/** Plan 01f: the same contract with a `:GATE:` command declared, which is
 * what inserts the GATING stage between RESOLVING and ACCEPTED. */
function gatedContract(gate = "deploy/atlas.sh --clean --build", gateCleanup?: string) {
  return { ...baseState().phase.contract, gate, ...(gateCleanup ? { gateCleanup } : {}) };
}

interface Fixture {
  state: State;
  event: Event;
}

const BUILD: Record<string, Fixture> = {
  "start-attempt": { state: baseState({ phase: "READY" }), event: { type: "ATTEMPT_STARTED" } },
  "submit-phase": { state: baseState({ phase: "IMPLEMENTING" }), event: { type: "SUBMIT_PHASE", disclosures: [] } },
  "attempt-timed-out-to-repairing": {
    state: baseState({ phase: "IMPLEMENTING" }),
    event: { type: "ATTEMPT_TIMED_OUT" },
  },
  "attempt-timed-out-budget-exhausted": {
    state: baseState({ phase: "IMPLEMENTING", repairRoundsUsed: 3, repairRoundsGranted: 3 }),
    event: { type: "ATTEMPT_TIMED_OUT" },
  },
  "attempt-no-submission-to-repairing": {
    state: baseState({ phase: "IMPLEMENTING" }),
    event: { type: "ATTEMPT_NO_SUBMISSION" },
  },
  "attempt-no-submission-budget-exhausted": {
    state: baseState({ phase: "IMPLEMENTING", repairRoundsUsed: 3, repairRoundsGranted: 3 }),
    event: { type: "ATTEMPT_NO_SUBMISSION" },
  },
  "attempt-interrupted": {
    state: baseState({ phase: "IMPLEMENTING", inFlight: { dispatch_worker: { actionId: "a1" } } }),
    event: { type: "ATTEMPT_INTERRUPTED" },
  },
  "freeze-completed": {
    state: baseState({ phase: "FREEZING", inFlight: { freeze: { actionId: "a1" } } }),
    event: { type: "FREEZE_COMPLETED", candidateSha: "C1", decisions: [] },
  },
  "freeze-timed-out-to-repairing": {
    state: baseState({ phase: "FREEZING" }),
    event: { type: "FREEZE_TIMED_OUT" },
  },
  "freeze-timed-out-budget-exhausted": {
    state: baseState({ phase: "FREEZING", repairRoundsUsed: 3, repairRoundsGranted: 3 }),
    event: { type: "FREEZE_TIMED_OUT" },
  },
  "freeze-interrupted": {
    state: baseState({ phase: "FREEZING", inFlight: { freeze: { actionId: "a1" } } }),
    event: { type: "FREEZE_INTERRUPTED" },
  },
  "checks-passed": {
    state: baseState({ phase: "CHECKING", candidate: C1, integrationHead: "H0", inFlight: { run_checks: { actionId: "a1" } } }),
    event: { type: "CHECKS_PASSED" },
  },
  "checks-failed-to-repairing": {
    state: baseState({ phase: "CHECKING", candidate: C1 }),
    event: { type: "CHECKS_FAILED" },
  },
  "checks-failed-budget-exhausted": {
    state: baseState({ phase: "CHECKING", candidate: C1, repairRoundsUsed: 3, repairRoundsGranted: 3 }),
    event: { type: "CHECKS_FAILED" },
  },
  "checks-interrupted": {
    state: baseState({ phase: "CHECKING", candidate: C1, inFlight: { run_checks: { actionId: "a1" } } }),
    event: { type: "CHECKS_INTERRUPTED" },
  },
  "probe-passed-needs-review": {
    state: baseState({ phase: "PROBING", candidate: C1, integrationHead: "H0", inFlight: { dispatch_probe: { actionId: "a1" } } }),
    event: { type: "PROBE_PASSED", probedI: "I1" },
  },
  "probe-passed-reviews-already-valid": {
    state: baseState({
      phase: "PROBING",
      candidate: C1,
      integrationHead: "H0",
      checks: { candidateSha: "C1", passed: true },
      reviews: acceptableReviews,
      inFlight: { dispatch_probe: { actionId: "a1" } },
    }),
    event: { type: "PROBE_PASSED", probedI: "I1" },
  },
  "probe-failed-to-repairing": {
    state: baseState({ phase: "PROBING", candidate: C1, integrationHead: "H0" }),
    event: { type: "PROBE_FAILED", evidence: "merge conflict" },
  },
  "probe-failed-budget-exhausted": {
    state: baseState({ phase: "PROBING", candidate: C1, integrationHead: "H0", repairRoundsUsed: 3, repairRoundsGranted: 3 }),
    event: { type: "PROBE_FAILED", evidence: "merge conflict" },
  },
  "probe-interrupted": {
    state: baseState({ phase: "PROBING", candidate: C1, integrationHead: "H0", inFlight: { dispatch_probe: { actionId: "a1" } } }),
    event: { type: "PROBE_INTERRUPTED" },
  },
  "review-submitted-incomplete": {
    state: baseState({ phase: "REVIEWING", candidate: C1, inFlight: { review_M: { actionId: "a1" } } }),
    event: { type: "REVIEW_SUBMITTED", review: approvingReview("M", "C1", K) },
  },
  "review-submitted-complete": {
    state: baseState({
      phase: "REVIEWING",
      candidate: C1,
      integrationHead: "H0",
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: { M: acceptableReviews.M, A: acceptableReviews.A },
      inFlight: { review_B: { actionId: "a1" } },
    }),
    event: { type: "REVIEW_SUBMITTED", review: approvingReview("B", "C1", K) },
  },
  "review-timed-out-first": {
    state: baseState({ phase: "REVIEWING", candidate: C1, inFlight: { review_A: { actionId: "a1" } } }),
    event: { type: "REVIEW_TIMED_OUT", reviewer: "A" },
  },
  "review-unavailable": {
    state: baseState({ phase: "REVIEWING", candidate: C1, reviews: { A: { timedOutOnce: true } } }),
    event: { type: "REVIEW_TIMED_OUT", reviewer: "A" },
  },
  "resolving-accept-holds": {
    state: baseState({
      phase: "RESOLVING",
      candidate: C1,
      integrationHead: "H0",
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
    }),
    event: { type: "ACCEPTED", resolvedCorrectionIds: [] },
  },
  // Plan 01f: an acceptable candidate whose contract declares a gate enters
  // GATING instead; the gate is the next action from there.
  "resolving-gate-required": {
    state: baseState({
      phase: "RESOLVING",
      candidate: C1,
      integrationHead: "H0",
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
      contract: gatedContract(),
    }),
    event: { type: "GATE_REQUIRED" },
  },
  "gate-accepted": {
    state: baseState({
      phase: "GATING",
      candidate: C1,
      integrationHead: "H0",
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
      inFlight: { run_gate: { actionId: "a1" } },
      contract: gatedContract(),
    }),
    event: { type: "ACCEPTED", resolvedCorrectionIds: [] },
  },
  "gate-failed-to-repairing": {
    state: baseState({
      phase: "GATING",
      candidate: C1,
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
      inFlight: { run_gate: { actionId: "a1" } },
      contract: gatedContract(),
    }),
    event: { type: "GATE_FAILED", evidence: "gate command exited 3: boom" },
  },
  "gate-failed-budget-exhausted": {
    state: baseState({
      phase: "GATING",
      candidate: C1,
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
      inFlight: { run_gate: { actionId: "a1" } },
      repairRoundsUsed: 3,
      repairRoundsGranted: 3,
      contract: gatedContract(),
    }),
    event: { type: "GATE_FAILED", evidence: "gate command exited 3: boom" },
  },
  "gate-interrupted": {
    state: baseState({
      phase: "GATING",
      candidate: C1,
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
      inFlight: { run_gate: { actionId: "a1" } },
      contract: gatedContract(),
    }),
    event: { type: "GATE_INTERRUPTED" },
  },
  // Plan 01g: a passing amendment (M and A approve it) replaces one
  // acceptance item, bumps the contract version and starts a fresh attempt
  // so the next candidate is judged against the new wording. The resulting
  // IMPLEMENTING state has no worker in flight, so next() dispatches one.
  "resolving-criterion-amended": {
    state: baseState({
      phase: "RESOLVING",
      candidate: C1,
      integrationHead: "H0",
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
      decisions: [
        {
          id: "D-am",
          version: 1,
          phaseId: "p1",
          source: "worker",
          class: "reserved",
          choice: "no fill after cancel is acknowledged",
          whyItMatters: "the letter of the wording cannot be met",
          alternatives: [{ option: "no fill after cancel is acknowledged", consequence: "no candidate can satisfy it" }],
          recommendation: { choice: "the guarantee holds within the tick", reason: "satisfiable" },
          boundCandidateSha: "C1",
          boundContractVersion: K,
          amendment: {
            id: "AM-p1",
            criterion: "no fill after cancel is acknowledged",
            proposedWording: "the guarantee holds within the tick",
            why: "the letter of the wording cannot be met",
            raisedBy: "worker",
            status: "proposed",
            previousContractVersion: K,
          },
        },
      ],
      ballots: [
        { reviewer: "M", decisionId: "D-am", vote: "approve", rationale: "satisfiable wording", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
        { reviewer: "A", decisionId: "D-am", vote: "approve", rationale: "satisfiable wording", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
      ],
    }),
    event: {
      type: "CRITERION_AMENDED",
      decisionId: "D-am",
      newAcceptance: ["the guarantee holds within the tick"],
      newContractVersion: CV(2),
    },
  },
  "resolving-incomplete-repair": {
    state: baseState({
      phase: "RESOLVING",
      candidate: C1,
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
      findings: [openBlockingFinding()],
    }),
    event: { type: "RESOLVING_INCOMPLETE" },
  },
  "resolving-incomplete-awaiting-owner": {
    state: baseState({
      phase: "RESOLVING",
      candidate: C1,
      checks: { candidateSha: "C1", passed: true },
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      reviews: acceptableReviews,
      findings: [openBlockingFinding()],
      repairRoundsUsed: 3,
      repairRoundsGranted: 3,
    }),
    event: { type: "RESOLVING_INCOMPLETE" },
  },
  "accepted-publish-intent": {
    state: baseState({ phase: "ACCEPTED", candidate: C1, integrationHead: "H0", probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true } }),
    event: { type: "PUBLISH_INTENT", expectedHead: "H0", candidateI: "I1" },
  },
  "publish-completed": {
    state: baseState({ phase: "PUBLISHING", candidate: C1, inFlight: { publish_cas: { actionId: "a1" } } }),
    event: { type: "PUBLISH_COMPLETED", newHead: "I1" },
  },
  "publish-stale": {
    state: baseState({
      phase: "PUBLISHING",
      candidate: C1,
      integrationHead: "H0",
      probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
      inFlight: { publish_cas: { actionId: "a1" } },
    }),
    event: { type: "PUBLISH_STALE", actualHead: "H1" },
  },
  "repair-attempt-started": {
    state: baseState({ phase: "REPAIRING", repairRoundsUsed: 0, repairRoundsGranted: 3 }),
    event: { type: "REPAIR_ATTEMPT_STARTED" },
  },
  "repairing-budget-exhausted": {
    state: baseState({ phase: "REPAIRING", repairRoundsUsed: 3, repairRoundsGranted: 3 }),
    event: { type: "REPAIR_BUDGET_EXHAUSTED" },
  },
  "run-budget-exceeded": {
    state: baseState({}, "RUN_ACTIVE"),
    event: { type: "RUN_BUDGET_EXCEEDED" },
  },
  "run-resumed": {
    state: baseState({ phase: "READY" }, "RUN_PAUSED_BUDGET"),
    event: { type: "RUN_RESUMED" },
  },
  "launch-failed-from-implementing": {
    state: baseState({ phase: "IMPLEMENTING", inFlight: { dispatch_worker: { actionId: "a1" } } }),
    event: { type: "LAUNCH_FAILED", role: "worker", expected: ["read", "edit"], missing: ["edit"], extra: ["submit_review"] },
  },
  "launch-failed-from-reviewing": {
    state: baseState({ phase: "REVIEWING", candidate: C1, inFlight: { review_A: { actionId: "a1" } } }),
    event: { type: "LAUNCH_FAILED", role: "reviewer", reviewer: "A", expected: ["read"], missing: [], extra: ["write"] },
  },
};

// amend-from-* and revise-from-* rows are generated over a list of states in
// transitions.ts; build fixtures for each the same way here.
const AMEND_FROM: PhaseStateName[] = ["CHECKING", "PROBING", "REVIEWING", "RESOLVING", "GATING", "ACCEPTED"];
for (const from of AMEND_FROM) {
  BUILD[`amend-from-${from.toLowerCase()}`] = {
    state: baseState({ phase: from, candidate: C1 }),
    event: { type: "AMEND", replacingContractVersion: K, newContractVersion: CV(2) },
  };
}

// criterion-reverted-from-* (plan 01g): the owner's correction naming an
// applied amendment restores the original wording, invalidates the evidence
// bound to the replaced version and returns to CHECKING.
const REVERT_FROM: PhaseStateName[] = ["CHECKING", "PROBING", "REVIEWING", "RESOLVING", "GATING", "ACCEPTED", "PUBLISHING", "AWAITING_OWNER"];
for (const from of REVERT_FROM) {
  BUILD[`criterion-reverted-from-${from.toLowerCase()}`] = {
    state: baseState({
      phase: from,
      candidate: C1,
      decisions: [
        {
          id: "D-am",
          version: 1,
          phaseId: "p1",
          source: "worker",
          class: "reserved",
          choice: "no fill after cancel is acknowledged",
          whyItMatters: "x",
          alternatives: [{ option: "the guarantee holds within the tick", consequence: "y" }],
          recommendation: { choice: "no fill after cancel is acknowledged", reason: "z" },
          boundCandidateSha: "C1",
          boundContractVersion: K,
          amendment: {
            id: "AM-p1",
            criterion: "the guarantee holds within the tick",
            proposedWording: "no fill after cancel is acknowledged",
            why: "x",
            raisedBy: "worker",
            status: "applied",
            previousContractVersion: K,
            appliedContractVersion: K,
          },
        },
      ],
    }),
    event: {
      type: "CRITERION_REVERTED",
      amendmentId: "AM-p1",
      newAcceptance: ["the guarantee holds within the tick"],
      newContractVersion: CV(2),
    },
  };
}

const REVISE_FROM: PhaseStateName[] = [
  "IMPLEMENTING",
  "FREEZING",
  "CHECKING",
  "PROBING",
  "REVIEWING",
  "RESOLVING",
  "GATING",
  "ACCEPTED",
  "BLOCKED",
];
for (const from of REVISE_FROM) {
  BUILD[`revise-from-${from.toLowerCase()}`] = {
    state: baseState({
      phase: from,
      candidate: C1,
      decisions: [
        {
          id: "D1",
          version: 1,
          phaseId: "p1",
          source: "worker",
          class: "delegated",
          choice: "x",
          whyItMatters: "y",
          alternatives: [{ option: "a", consequence: "b" }],
          recommendation: { choice: "a", reason: "b" },
          boundCandidateSha: "C1",
          boundContractVersion: K,
        },
      ],
    }),
    event: {
      type: "REVISE",
      correctionId: "C-revise-1",
      targetRecordId: "D1",
      correctionText: "do it this way instead",
      contractChange: false,
      boundCandidateSha: "C1",
      boundContractVersion: K,
      boundRecordVersion: 1,
    },
  };
}

// ---------------------------------------------------------------------------
// AWAITING_OWNER exits (round-2 review items 1 and 2): AMEND, REVISE,
// OWNER_REQUEST_RESOLVED, FINDING_ACCEPTED_BY_OWNER and OVERRIDE_CAST move
// the phase only once no owner request remains open after applying them.
// ---------------------------------------------------------------------------

function decisionFixture(overrides: Partial<import("../../src/core/types.ts").Decision> = {}) {
  return {
    id: "D1",
    version: 1,
    phaseId: "p1",
    source: "worker" as const,
    class: "delegated" as const,
    choice: "x",
    whyItMatters: "y",
    alternatives: [{ option: "a", consequence: "b" }],
    recommendation: { choice: "a", reason: "b" },
    boundCandidateSha: "C1",
    boundContractVersion: K,
    ...overrides,
  };
}

const resolvableAwaitingOwnerBase = {
  candidate: C1,
  integrationHead: "H0",
  checks: { candidateSha: "C1", passed: true },
  probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
  reviews: acceptableReviews,
};

// amend-from-awaiting_owner: round-3 review item 2 — AMEND works from
// AWAITING_OWNER even while a request is open (design §4.2/§4.3: for a
// `contract` finding, amending the contract IS the owner's disposition).
// It resolves the named contract finding and supersedes the open request
// bound to the contract version being replaced.
BUILD["amend-from-awaiting_owner"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    findings: [
      { id: "F-contract", version: 1, phaseId: "p1", kind: "contract", severity: "blocking", evidence: "cancel acknowledges before fills stop", raisedBy: "B", status: "open", boundCandidateSha: "C1" },
    ],
    ownerRequests: [
      { id: "OR-1", version: 1, phaseId: "p1", reason: "contract finding open", origin: "open_finding", linkedFindingId: "F-contract", boundCandidateSha: "C1", boundContractVersion: K, options: [{ id: "repair", label: "repair (grant 3 rounds)" }], status: "open" },
    ],
  }),
  event: { type: "AMEND", replacingContractVersion: K, newContractVersion: CV(2), resolvesFindingIds: ["F-contract"] },
};

// revise-from-awaiting-owner-clears: the ONLY open request is linked to the
// revised decision, so resolving it via REVISE clears AWAITING_OWNER; an
// open correction always sends it to REPAIRING (never RESOLVING) since the
// correction itself needs a round.
BUILD["revise-from-awaiting-owner-clears"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    decisions: [decisionFixture({ class: "reserved" })],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "REVISE",
    correctionId: "C-revise-1",
    targetRecordId: "D1",
    correctionText: "do it this way instead",
    contractChange: false,
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

// revise-from-awaiting-owner-stays: a SECOND, unrelated open request
// remains after the revise resolves only the one linked to D1.
BUILD["revise-from-awaiting-owner-stays"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    decisions: [decisionFixture({ id: "D1", class: "reserved" }), decisionFixture({ id: "D2", class: "reserved" })],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved D1",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
      {
        id: "OR-2",
        version: 1,
        phaseId: "p1",
        reason: "reserved D2",
        origin: "reserved_decision",
        linkedDecisionId: "D2",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "REVISE",
    correctionId: "C-revise-1",
    targetRecordId: "D1",
    correctionText: "do it this way instead",
    contractChange: false,
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

// awaiting-owner-request-resolved-grant / -stop: the plain repair-budget
// gate request (no linked record), routed by its own two options.
BUILD["awaiting-owner-request-resolved-grant"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
    ownerRequests: [
      {
        id: "OR-gate",
        version: 1,
        phaseId: "p1",
        reason: "worker attempts kept timing out",
        origin: "repair_budget_exhausted",
        options: [{ id: "grant", label: "grant 3 more repair rounds" }, { id: "stop", label: "stop the phase" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-gate",
    option: "grant",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

BUILD["awaiting-owner-request-resolved-stop"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    ownerRequests: [
      {
        id: "OR-gate",
        version: 1,
        phaseId: "p1",
        reason: "worker attempts kept timing out",
        origin: "repair_budget_exhausted",
        options: [{ id: "grant", label: "grant 3 more repair rounds" }, { id: "stop", label: "stop the phase" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-gate",
    option: "stop",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

// awaiting-owner-request-resolved-clears-to-resolving: a `reserved_decision`
// request — design §6.3's reserved-decision clause is satisfied by the mere
// resolution bound to (C, K), whatever option is chosen — so resolving it,
// with checks/probe/reviews already valid and nothing else open, lets
// next() go straight to `accept`.
BUILD["awaiting-owner-request-resolved-clears-to-resolving"] = {
  state: baseState({
    ...resolvableAwaitingOwnerBase,
    phase: "AWAITING_OWNER",
    decisions: [decisionFixture({ class: "reserved" })],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "approve",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

// awaiting-owner-request-resolved-clears-to-repairing: same, but an open
// correction from an earlier revise still needs its own round.
BUILD["awaiting-owner-request-resolved-clears-to-repairing"] = {
  state: baseState({
    ...resolvableAwaitingOwnerBase,
    phase: "AWAITING_OWNER",
    decisions: [decisionFixture({ class: "reserved" })],
    corrections: [
      {
        id: "C-1",
        version: 1,
        phaseId: "p1",
        targetRecordId: "D-other",
        correctionText: "x",
        contractChange: false,
        status: "open",
        boundContractVersion: K,
        grantedRounds: 3,
      },
    ],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "approve",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

// awaiting-owner-request-resolved-stays: a second, unrelated request stays open.
BUILD["awaiting-owner-request-resolved-stays"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    decisions: [decisionFixture({ id: "D1", class: "reserved" }), decisionFixture({ id: "D2", class: "reserved" })],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved D1",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
      {
        id: "OR-2",
        version: 1,
        phaseId: "p1",
        reason: "reserved D2",
        origin: "reserved_decision",
        linkedDecisionId: "D2",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "approve",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

// awaiting-owner-request-resolved-repair-clears/-stays: "reject and repair"
// never settles the decision — it always grants +3 rounds and, once
// nothing else is open, goes straight to REPAIRING (round-3 review item 1).
BUILD["awaiting-owner-request-resolved-repair-clears"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    decisions: [decisionFixture({ class: "reserved" })],
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "reject_and_repair",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

BUILD["awaiting-owner-request-resolved-repair-stays"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    decisions: [decisionFixture({ id: "D1", class: "reserved" }), decisionFixture({ id: "D2", class: "reserved" })],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved D1",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
      {
        id: "OR-2",
        version: 1,
        phaseId: "p1",
        reason: "reserved D2",
        origin: "reserved_decision",
        linkedDecisionId: "D2",
        options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "reject_and_repair",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

const findingFixture = (overrides: Partial<Finding> = {}): Finding => ({
  id: "F1",
  version: 1,
  phaseId: "p1",
  kind: "defect",
  severity: "blocking",
  evidence: "x",
  raisedBy: "B",
  status: "open",
  boundCandidateSha: "C1",
  ...overrides,
});

BUILD["awaiting-owner-finding-accepted-clears-to-resolving"] = {
  state: baseState({
    ...resolvableAwaitingOwnerBase,
    phase: "AWAITING_OWNER",
    findings: [findingFixture()],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "finding open",
        origin: "open_finding",
        linkedFindingId: "F1",
        options: [{ id: "accept_risk", label: "accept the risk" }, { id: "repair", label: "repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "known limitation for this phase",
    by: "owner",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

BUILD["awaiting-owner-finding-accepted-clears-to-repairing"] = {
  state: baseState({
    ...resolvableAwaitingOwnerBase,
    phase: "AWAITING_OWNER",
    findings: [findingFixture()],
    corrections: [
      {
        id: "C-1",
        version: 1,
        phaseId: "p1",
        targetRecordId: "D-other",
        correctionText: "x",
        contractChange: false,
        status: "open",
        boundContractVersion: K,
        grantedRounds: 3,
      },
    ],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "finding open",
        origin: "open_finding",
        linkedFindingId: "F1",
        options: [{ id: "accept_risk", label: "accept the risk" }, { id: "repair", label: "repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "known limitation for this phase",
    by: "owner",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

BUILD["awaiting-owner-finding-accepted-stays"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    findings: [findingFixture({ id: "F1" }), findingFixture({ id: "F2" })],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "finding F1 open",
        origin: "open_finding",
        linkedFindingId: "F1",
        options: [{ id: "accept_risk", label: "accept the risk" }, { id: "repair", label: "repair (grant 3 rounds)" }],
        status: "open",
      },
      {
        id: "OR-2",
        version: 1,
        phaseId: "p1",
        reason: "finding F2 open",
        origin: "open_finding",
        linkedFindingId: "F2",
        options: [{ id: "accept_risk", label: "accept the risk" }, { id: "repair", label: "repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "known limitation for this phase",
    by: "owner",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  },
};

function failingBallots() {
  return [
    { reviewer: "M" as const, decisionId: "D1", vote: "reject" as const, rationale: "x", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
    { reviewer: "A" as const, decisionId: "D1", vote: "reject" as const, rationale: "x", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
    { reviewer: "B" as const, decisionId: "D1", vote: "reject" as const, rationale: "x", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  ];
}

BUILD["awaiting-owner-override-clears-to-resolving"] = {
  state: baseState({
    ...resolvableAwaitingOwnerBase,
    phase: "AWAITING_OWNER",
    decisions: [decisionFixture()],
    ballots: failingBallots(),
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "failed vote",
        origin: "failed_vote",
        linkedDecisionId: "D1",
        options: [{ id: "accept_as_implemented", label: "accept the decision as implemented" }, { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OVERRIDE_CAST",
    override: { decisionId: "D1", vote: "approve", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  },
};

BUILD["awaiting-owner-override-clears-to-repairing"] = {
  state: baseState({
    ...resolvableAwaitingOwnerBase,
    phase: "AWAITING_OWNER",
    decisions: [decisionFixture()],
    ballots: failingBallots(),
    corrections: [
      {
        id: "C-1",
        version: 1,
        phaseId: "p1",
        targetRecordId: "D-other",
        correctionText: "x",
        contractChange: false,
        status: "open",
        boundContractVersion: K,
        grantedRounds: 3,
      },
    ],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "failed vote",
        origin: "failed_vote",
        linkedDecisionId: "D1",
        options: [{ id: "accept_as_implemented", label: "accept the decision as implemented" }, { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OVERRIDE_CAST",
    override: { decisionId: "D1", vote: "approve", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  },
};

BUILD["awaiting-owner-override-stays"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    decisions: [decisionFixture({ id: "D1" }), decisionFixture({ id: "D2" })],
    ballots: [...failingBallots(), ...failingBallots().map((b) => ({ ...b, decisionId: "D2" }))],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "failed vote D1",
        origin: "failed_vote",
        linkedDecisionId: "D1",
        options: [{ id: "accept_as_implemented", label: "accept the decision as implemented" }, { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" }],
        status: "open",
      },
      {
        id: "OR-2",
        version: 1,
        phaseId: "p1",
        reason: "failed vote D2",
        origin: "failed_vote",
        linkedDecisionId: "D2",
        options: [{ id: "accept_as_implemented", label: "accept the decision as implemented" }, { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" }],
        status: "open",
      },
    ],
  }),
  event: {
    type: "OVERRIDE_CAST",
    override: { decisionId: "D1", vote: "approve", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  },
};

BUILD["owner-correction-from-awaiting-owner"] = {
  state: baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "the repair budget ran out while items remained open",
        origin: "repair_budget_exhausted",
        boundCandidateSha: "C1",
        boundContractVersion: K,
        options: [{ id: "grant", label: "grant 3 more repair rounds" }, { id: "stop", label: "stop the phase" }],
        status: "open",
      },
    ],
  }),
  event: { type: "OWNER_CORRECTION", correctionId: "cmd-correction-1", text: "do it the other way" },
};

test("transition table: every row in transitions.ts has a covering fixture", () => {
  const missing = TRANSITIONS.filter((r) => !BUILD[r.id]).map((r) => r.id);
  assert.deepEqual(missing, [], `rows with no test fixture: ${missing.join(", ")}`);
});

for (const row of TRANSITIONS) {
  test(`transition row '${row.id}': ${row.from} --${row.trigger}[${row.guardName}]--> ${row.to}`, () => {
    const fixture = BUILD[row.id];
    assert.ok(fixture, `no fixture registered for row ${row.id}`);
    const { state, event } = fixture;

    const result = reduce(state, event);
    assert.equal(result.ok, true, `reduce rejected: ${!result.ok ? result.reason : ""}`);
    const to = row.axis === "phase" ? result.state.phase.phase : result.state.run;
    assert.equal(to, row.to, `row '${row.id}' expected to=${row.to}, got ${to}`);

    const actions = next(result.state);
    assert.deepEqual(actions, row.actions, `next() of row '${row.id}'s resulting state did not match its actions`);
  });
}

test("next(): no double dispatch — ACTION_STARTED for an outstanding action makes next() return [] for it", () => {
  // CHECKING with a fresh candidate: next() recommends run_checks once.
  let state = baseState({ phase: "CHECKING", candidate: C1 });
  assert.deepEqual(next(state), [{ type: "run_checks", candidateSha: "C1" }]);

  const started = reduce(state, { type: "ACTION_STARTED", action: "run_checks", actionId: "a1" });
  assert.equal(started.ok, true, !started.ok ? started.reason : "");
  state = started.state;
  assert.deepEqual(next(state), [], "next() must not re-recommend an action already marked in flight");

  // Starting the SAME action again is rejected: it is no longer outstanding.
  const again = reduce(state, { type: "ACTION_STARTED", action: "run_checks", actionId: "a2" });
  assert.equal(again.ok, false);
});

test("next(): REVIEWING dispatches only the reviewers still missing a valid review and not in flight", () => {
  const state = baseState({
    phase: "REVIEWING",
    candidate: C1,
    reviews: { M: acceptableReviews.M },
    inFlight: { review_A: { actionId: "a1" } },
  });
  // M has a valid review; A is in flight; only B is outstanding.
  assert.deepEqual(next(state), [{ type: "dispatch_review", reviewer: "B" }]);
});

test("next(): a state rebuilt by folding the same events from scratch gives the same next() (recovery property)", () => {
  const events: Event[] = [
    { type: "ATTEMPT_STARTED" },
    { type: "SUBMIT_PHASE", disclosures: [] },
    { type: "FREEZE_COMPLETED", candidateSha: "C1", decisions: [] },
  ];

  let incremental = baseState({ phase: "READY" });
  for (const ev of events) {
    const result = reduce(incremental, ev);
    assert.equal(result.ok, true, !result.ok ? result.reason : "");
    incremental = result.state;
  }

  // "Recovery" here is simulated by folding the identical event list again
  // from the identical starting state — a conductor restart replays
  // events.jsonl the same way.
  let rebuilt = baseState({ phase: "READY" });
  for (const ev of events) {
    const result = reduce(rebuilt, ev);
    assert.equal(result.ok, true, !result.ok ? result.reason : "");
    rebuilt = result.state;
  }

  assert.deepEqual(next(incremental), next(rebuilt));
  assert.deepEqual(incremental, rebuilt);
});

// ---------------------------------------------------------------------------
// Coverage against design §6.1's own transcription, plus the §8.1/§9.3/§7.3/
// §7.5/§6.4 additions the phase-0 brief names. Quoted quotes are from
// docs/tradeoffs-trace.md.
// ---------------------------------------------------------------------------

const DESIGN_EDGES: { from: PhaseStateName; to: PhaseStateName; cite: string }[] = [
  // design §6.1 diagram, verbatim structure:
  { from: "READY", to: "IMPLEMENTING", cite: "§6.1: READY │ start worker attempt n ▼ IMPLEMENTING" },
  { from: "IMPLEMENTING", to: "FREEZING", cite: "§6.1: IMPLEMENTING │ submit_phase ▼ FREEZING" },
  { from: "IMPLEMENTING", to: "REPAIRING", cite: "§6.1: IMPLEMENTING ──deadline / no_submission──▶ attempt failed ──▶ REPAIRING" },
  { from: "FREEZING", to: "CHECKING", cite: "§6.1: FREEZING ... ▼ CHECKING" },
  { from: "CHECKING", to: "PROBING", cite: "§6.1: CHECKING ... ▼ PROBING" },
  { from: "CHECKING", to: "REPAIRING", cite: "§6.1: CHECKING │ any failure ─...─▶ REPAIRING" },
  { from: "PROBING", to: "REVIEWING", cite: "§6.1: PROBING │ success → close open `integration` findings ▼ REVIEWING" },
  { from: "PROBING", to: "REPAIRING", cite: "§6.1: PROBING │ conflict or failure → raise `integration` finding ──▶ REPAIRING" },
  { from: "REVIEWING", to: "RESOLVING", cite: "§6.1: REVIEWING ... ▼ RESOLVING" },
  { from: "RESOLVING", to: "REPAIRING", cite: "§6.1: RESOLVING │ open items remain and budget remains ─...─▶ REPAIRING" },
  { from: "RESOLVING", to: "AWAITING_OWNER", cite: "§6.1: RESOLVING │ open items remain, budget exhausted ─...─▶ AWAITING_OWNER" },
  { from: "RESOLVING", to: "ACCEPTED", cite: "§6.1: RESOLVING │ accept(C, K) holds (§6.3) ▼ ACCEPTED(C)" },
  // Plan 01f: a declared `:GATE:` inserts the gate stage between them.
  { from: "RESOLVING", to: "GATING", cite: "plan 01f: accept(C, K) holds and the contract declares a gate ▼ GATING" },
  { from: "GATING", to: "ACCEPTED", cite: "plan 01f: a passing gate lets acceptance proceed ▼ ACCEPTED(C)" },
  { from: "GATING", to: "REPAIRING", cite: "plan 01f: a failing gate is a blocking integration finding ─▶ REPAIRING" },
  { from: "ACCEPTED", to: "PUBLISHING", cite: "§6.1: ACCEPTED(C) ... ▼ PUBLISHING" },
  { from: "PUBLISHING", to: "DONE", cite: "§6.1: PUBLISHING ... ▼ DONE(I)" },
  { from: "REPAIRING", to: "IMPLEMENTING", cite: "§6.1: REPAIRING the same worker session, a new attempt ... ──▶ IMPLEMENTING (consumes one repair round)" },

  // §8.1 outcomes that change phase state
  { from: "FREEZING", to: "REPAIRING", cite: "§8.1: freeze timeout → attempt failed; worktree tainted" },
  { from: "REVIEWING", to: "BLOCKED", cite: "§8.1: review timeout → cancel, re-dispatch once; then BLOCKED: reviewer unavailable" },
  { from: "REPAIRING", to: "AWAITING_OWNER", cite: "§8.1: repair rounds exhausted → open items become owner requests" },

  // §9.3 interrupted gates: interrupted, never passed — rerun
  { from: "CHECKING", to: "CHECKING", cite: "§9.3: check run → mark interrupted and rerun" },
  { from: "PROBING", to: "PROBING", cite: "§9.3: probe → mark the probe interrupted; probe again" },
  { from: "IMPLEMENTING", to: "IMPLEMENTING", cite: "§9.3: agent attempt interrupted → worker: new attempt on the same session" },
  { from: "FREEZING", to: "FREEZING", cite: "§9.3: freeze commit → otherwise redo the freeze" },

  // §6.4 step 3: publish CAS stale -> PROBING
  { from: "PUBLISHING", to: "PROBING", cite: "§6.4 step 3: still at H → probe is stale, return to PROBING" },

  // §7.3 amend: evidence invalidated, back to CHECKING under new K
  { from: "RESOLVING", to: "CHECKING", cite: "§7.3: the phase returns to checks and review under the new contract version" },

  // §7.5 revise: cancel in-flight, new 3-round allowance, -> REPAIRING
  { from: "ACCEPTED", to: "REPAIRING", cite: "§7.5: a running worker attempt or review is cancelled ... a new repair allowance is granted ... REPAIRING" },
];

for (const edge of DESIGN_EDGES) {
  test(`design §6.1 coverage: ${edge.from} -> ${edge.to} (${edge.cite})`, () => {
    const hasRow = TRANSITIONS.some((r) => r.from === edge.from && r.to === edge.to);
    assert.ok(hasRow, `no transition row covers ${edge.from} -> ${edge.to} (${edge.cite})`);
  });
}
