// Round-3 review item 1: every owner-request option has a defined effect —
// none is a no-op — named by a stable option id (never matched by label).
// Table-driven: for every origin × option, resolving it through reduce()
// either settles the item (accept's input changes), grants rounds, or
// blocks (design §5.2, §6.3, §4.2, §7.5). Also: a settling choice is never
// followed by a regenerated request for the same item on the next
// RESOLVING cycle.

import assert from "node:assert/strict";
import { test } from "node:test";
import { accept, decisionSettled } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { Correction, Decision, Finding, OwnerRequest, State } from "../../src/core/types.ts";
import { CV, baseState, makeDecision } from "./helpers.ts";

const K = CV();
const C1 = { sha: "C1", contractVersion: K };

function fullyReviewedBase(overrides: Partial<import("../../src/core/types.ts").PhaseState> = {}) {
  return baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: { reviewer: "M", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
      A: { review: { reviewer: "A", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
      B: { review: { reviewer: "B", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
    },
    ...overrides,
  });
}

interface Case {
  name: string;
  origin: OwnerRequest["origin"];
  option: string;
  note?: string;
  build: () => { state: State; requestId: string };
  assertEffect: (before: State, after: State) => void;
}

const CASES: Case[] = [
  {
    name: "failed_vote / accept_as_implemented settles the decision",
    origin: "failed_vote",
    option: "accept_as_implemented",
    build: () => {
      const decision: Decision = makeDecision({ id: "D1", class: "delegated" });
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "failed vote",
        origin: "failed_vote",
        linkedDecisionId: "D1",
        options: [
          { id: "accept_as_implemented", label: "accept the decision as implemented" },
          { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" },
        ],
        status: "open",
      };
      const state = fullyReviewedBase({ decisions: [decision], ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (before, after) => {
      assert.equal(decisionSettled(before.phase.decisions[0], before.phase, "C1", K), false);
      assert.equal(decisionSettled(after.phase.decisions[0], after.phase, "C1", K), true, "settles it");
      assert.equal(after.phase.repairRoundsGranted, before.phase.repairRoundsGranted, "does not grant rounds");
    },
  },
  {
    name: "failed_vote / reject_and_repair grants rounds without settling",
    origin: "failed_vote",
    option: "reject_and_repair",
    build: () => {
      const decision: Decision = makeDecision({ id: "D1", class: "delegated" });
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "failed vote",
        origin: "failed_vote",
        linkedDecisionId: "D1",
        options: [
          { id: "accept_as_implemented", label: "accept the decision as implemented" },
          { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" },
        ],
        status: "open",
      };
      const state = fullyReviewedBase({ decisions: [decision], ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (before, after) => {
      assert.equal(decisionSettled(after.phase.decisions[0], after.phase, "C1", K), false, "does not settle it");
      assert.equal(after.phase.repairRoundsGranted, before.phase.repairRoundsGranted + 3);
      assert.equal(after.phase.phase, "REPAIRING");
    },
  },
  {
    name: "reserved_decision / approve settles the decision",
    origin: "reserved_decision",
    option: "approve",
    build: () => {
      const decision: Decision = makeDecision({ id: "D1", class: "reserved" });
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [
          { id: "approve", label: "approve" },
          { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" },
        ],
        status: "open",
      };
      const state = fullyReviewedBase({ decisions: [decision], ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (before, after) => {
      assert.equal(decisionSettled(after.phase.decisions[0], after.phase, "C1", K), true);
      assert.equal(after.phase.phase, "RESOLVING");
    },
  },
  {
    name: "reserved_decision / reject_and_repair grants rounds without settling",
    origin: "reserved_decision",
    option: "reject_and_repair",
    build: () => {
      const decision: Decision = makeDecision({ id: "D1", class: "reserved" });
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [
          { id: "approve", label: "approve" },
          { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" },
        ],
        status: "open",
      };
      const state = fullyReviewedBase({ decisions: [decision], ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (before, after) => {
      assert.equal(decisionSettled(after.phase.decisions[0], after.phase, "C1", K), false);
      assert.equal(after.phase.repairRoundsGranted, before.phase.repairRoundsGranted + 3);
      assert.equal(after.phase.phase, "REPAIRING");
    },
  },
  {
    name: "open_finding (non-contract) / accept_risk settles the finding (requires a scope note)",
    origin: "open_finding",
    option: "accept_risk",
    note: "known limitation",
    build: () => {
      const finding: Finding = {
        id: "F1",
        version: 1,
        phaseId: "p1",
        kind: "defect",
        severity: "blocking",
        evidence: "x",
        raisedBy: "B",
        status: "open",
        boundCandidateSha: "C1",
      };
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "open finding",
        origin: "open_finding",
        linkedFindingId: "F1",
        options: [
          { id: "accept_risk", label: "accept the risk" },
          { id: "repair", label: "repair (grant 3 rounds)" },
        ],
        status: "open",
      };
      const state = fullyReviewedBase({ findings: [finding], ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (_before, after) => {
      assert.equal(after.phase.findings[0].status, "accepted");
      assert.equal(after.phase.findings[0].acceptedScope, "known limitation");
      assert.equal(after.phase.phase, "RESOLVING");
    },
  },
  {
    name: "open_finding (non-contract) / repair grants rounds, leaves the finding open",
    origin: "open_finding",
    option: "repair",
    build: () => {
      const finding: Finding = {
        id: "F1",
        version: 1,
        phaseId: "p1",
        kind: "defect",
        severity: "blocking",
        evidence: "x",
        raisedBy: "B",
        status: "open",
        boundCandidateSha: "C1",
      };
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "open finding",
        origin: "open_finding",
        linkedFindingId: "F1",
        options: [
          { id: "accept_risk", label: "accept the risk" },
          { id: "repair", label: "repair (grant 3 rounds)" },
        ],
        status: "open",
      };
      const state = fullyReviewedBase({ findings: [finding], ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (before, after) => {
      assert.equal(after.phase.findings[0].status, "open");
      assert.equal(after.phase.repairRoundsGranted, before.phase.repairRoundsGranted + 3);
      assert.equal(after.phase.phase, "REPAIRING");
    },
  },
  {
    name: "unaddressed_correction / grant_correction grants rounds, leaves the correction open",
    origin: "unaddressed_correction",
    option: "grant_correction",
    build: () => {
      const correction: Correction = {
        id: "C-1",
        version: 1,
        phaseId: "p1",
        targetRecordId: "D-x",
        correctionText: "x",
        contractChange: false,
        status: "open",
        boundContractVersion: K,
        grantedRounds: 3,
      };
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "unaddressed correction",
        origin: "unaddressed_correction",
        linkedCorrectionId: "C-1",
        options: [
          { id: "grant_correction", label: "grant 3 more rounds for this correction" },
          { id: "withdraw", label: "withdraw the correction" },
        ],
        status: "open",
      };
      const state = fullyReviewedBase({ corrections: [correction], ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (before, after) => {
      assert.equal(after.phase.corrections[0].status, "open");
      assert.equal(after.phase.repairRoundsGranted, before.phase.repairRoundsGranted + 3);
      assert.equal(after.phase.phase, "REPAIRING");
    },
  },
  {
    name: "unaddressed_correction / withdraw closes the correction (no longer gates accept)",
    origin: "unaddressed_correction",
    option: "withdraw",
    build: () => {
      const correction: Correction = {
        id: "C-1",
        version: 1,
        phaseId: "p1",
        targetRecordId: "D-x",
        correctionText: "x",
        contractChange: false,
        status: "open",
        boundContractVersion: K,
        grantedRounds: 3,
      };
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "unaddressed correction",
        origin: "unaddressed_correction",
        linkedCorrectionId: "C-1",
        options: [
          { id: "grant_correction", label: "grant 3 more rounds for this correction" },
          { id: "withdraw", label: "withdraw the correction" },
        ],
        status: "open",
      };
      const state = fullyReviewedBase({ corrections: [correction], ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (_before, after) => {
      assert.equal(after.phase.corrections[0].status, "withdrawn");
      assert.equal(accept(after.phase, "C1", K), true, "a withdrawn correction no longer gates accept()");
      assert.equal(after.phase.phase, "RESOLVING");
    },
  },
  {
    name: "repair_budget_exhausted / grant grants rounds, goes to REPAIRING",
    origin: "repair_budget_exhausted",
    option: "grant",
    build: () => {
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "attempts kept timing out",
        origin: "repair_budget_exhausted",
        options: [
          { id: "grant", label: "grant 3 more repair rounds" },
          { id: "stop", label: "stop the phase" },
        ],
        status: "open",
      };
      const state = baseState({
        phase: "AWAITING_OWNER",
        candidate: C1,
        repairRoundsUsed: 3,
        repairRoundsGranted: 3,
        ownerRequests: [request],
      });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (before, after) => {
      assert.equal(after.phase.repairRoundsGranted, before.phase.repairRoundsGranted + 3);
      assert.equal(after.phase.phase, "REPAIRING");
    },
  },
  {
    name: "repair_budget_exhausted / stop blocks the phase",
    origin: "repair_budget_exhausted",
    option: "stop",
    build: () => {
      const request: OwnerRequest = {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "attempts kept timing out",
        origin: "repair_budget_exhausted",
        options: [
          { id: "grant", label: "grant 3 more repair rounds" },
          { id: "stop", label: "stop the phase" },
        ],
        status: "open",
      };
      const state = baseState({ phase: "AWAITING_OWNER", candidate: C1, ownerRequests: [request] });
      return { state, requestId: "OR-1" };
    },
    assertEffect: (_before, after) => {
      assert.equal(after.phase.phase, "BLOCKED");
    },
  },
];

for (const c of CASES) {
  test(`owner-request-options: ${c.name}`, () => {
    const { state, requestId } = c.build();
    const before = state;
    const result = reduce(state, {
      type: "OWNER_REQUEST_RESOLVED",
      requestId,
      option: c.option,
      note: c.note,
      boundCandidateSha: "C1",
      boundContractVersion: K,
      boundRecordVersion: 1,
    });
    assert.equal(result.ok, true, !result.ok ? result.reason : "");
    c.assertEffect(before, result.state);
  });
}

test("owner-request-options: an unknown option id is rejected, never matched by label", () => {
  const decision: Decision = makeDecision({ id: "D1", class: "reserved" });
  const request: OwnerRequest = {
    id: "OR-1",
    version: 1,
    phaseId: "p1",
    reason: "reserved",
    origin: "reserved_decision",
    linkedDecisionId: "D1",
    options: [
      { id: "approve", label: "approve" },
      { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" },
    ],
    status: "open",
  };
  const state = fullyReviewedBase({ decisions: [decision], ownerRequests: [request] });

  const valid = reduce(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "approve", // a real id
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(valid.ok, true, !valid.ok ? valid.reason : "");

  // "reject and repair (grant 3 rounds)" is the OPTION'S LABEL, not its id
  // ("reject_and_repair") — matching by label must be rejected.
  const byLabel = reduce(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "reject and repair (grant 3 rounds)",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(byLabel.ok, false);
  assert.match((byLabel as { reason: string }).reason, /not an option/);
});

test("owner-request-options: a settling choice is never followed by a regenerated request for the same item on the next RESOLVING cycle", () => {
  const decision: Decision = makeDecision({ id: "D1", class: "reserved" });
  const request: OwnerRequest = {
    id: "OR-1",
    version: 1,
    phaseId: "p1",
    reason: "reserved",
    origin: "reserved_decision",
    linkedDecisionId: "D1",
    options: [
      { id: "approve", label: "approve" },
      { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" },
    ],
    status: "open",
  };
  const state = fullyReviewedBase({ decisions: [decision], ownerRequests: [request] });
  const settled = reduce(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "approve",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(settled.ok, true, !settled.ok ? settled.reason : "");
  assert.equal(settled.state.phase.phase, "RESOLVING");

  // If accept() doesn't hold for some OTHER reason (simulate one more open
  // item) and the phase bounces back through RESOLVING_INCOMPLETE, D1 must
  // not get a fresh request — it is already settled at (C1, K).
  const withAnotherBlocker = {
    ...settled.state,
    phase: {
      ...settled.state.phase,
      findings: [
        { id: "F-other", version: 1, phaseId: "p1", kind: "defect" as const, severity: "blocking" as const, evidence: "y", raisedBy: "A" as const, status: "open" as const, boundCandidateSha: "C1" },
      ],
      repairRoundsUsed: 3,
      repairRoundsGranted: 3, // exhausted, so RESOLVING_INCOMPLETE routes to AWAITING_OWNER
    },
  };
  const incomplete = reduce(withAnotherBlocker, { type: "RESOLVING_INCOMPLETE" });
  assert.equal(incomplete.ok, true, !incomplete.ok ? incomplete.reason : "");
  assert.equal(incomplete.state.phase.phase, "AWAITING_OWNER");
  const openLinkedToD1 = incomplete.state.phase.ownerRequests.filter((r) => r.status === "open" && r.linkedDecisionId === "D1");
  assert.equal(openLinkedToD1.length, 0, "D1 is already settled and must not get a new request");
  const openForOther = incomplete.state.phase.ownerRequests.filter((r) => r.status === "open" && r.linkedFindingId === "F-other");
  assert.equal(openForOther.length, 1, "the genuinely new open item still gets its own request");
});
