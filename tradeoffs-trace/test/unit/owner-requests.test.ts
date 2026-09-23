// Round-2 review item 1: every transition into AWAITING_OWNER records an
// owner request for whatever is open, so the owner always has something to
// resolve (design §4.2, §5.2, §7.5, §8.1). AWAITING_OWNER is never entered
// with zero open owner requests.

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import type { Ballot, Correction, Decision, Finding } from "../../src/core/types.ts";
import { CV, baseState, makeDecision } from "./helpers.ts";

const K = CV();

function openRequests(state: ReturnType<typeof baseState>) {
  return state.phase.ownerRequests.filter((r) => r.status === "open");
}

function assertWellFormedRequests(state: ReturnType<typeof baseState>) {
  const open = openRequests(state);
  assert.ok(open.length >= 1, "AWAITING_OWNER must never be entered with zero open owner requests");
  for (const r of open) {
    assert.equal(r.version, 1);
    // Every option has a defined effect (round-3 review item 1); a
    // `contract` finding's request legitimately offers only "repair" (its
    // other disposition is amending the contract, a separate command).
    assert.ok(r.options.length >= 1, `request ${r.id} must offer at least 1 option`);
    if (state.phase.candidate) {
      assert.equal(r.boundCandidateSha, state.phase.candidate.sha);
      assert.deepEqual(r.boundContractVersion, state.phase.contract.contractVersion);
    }
  }
}

test("owner-requests: a gate failure with no record-level item (attempts kept timing out) gets the plain budget request", () => {
  const state = baseState({ phase: "IMPLEMENTING", repairRoundsUsed: 3, repairRoundsGranted: 3 });
  const result = reduce(state, { type: "ATTEMPT_TIMED_OUT" });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.phase, "AWAITING_OWNER");
  assertWellFormedRequests(result.state);
  const open = openRequests(result.state);
  assert.equal(open.length, 1);
  assert.deepEqual(
    open[0].options.map((o) => o.id),
    ["grant", "stop"],
  );
  assert.equal(open[0].origin, "repair_budget_exhausted");
});

test("owner-requests: an open blocking finding (integration probe failure) gets a linked request, not the generic one", () => {
  const state = baseState({
    phase: "PROBING",
    candidate: { sha: "C1", contractVersion: K },
    integrationHead: "H0",
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
  });
  const result = reduce(state, { type: "PROBE_FAILED", evidence: "merge conflict" });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.phase, "AWAITING_OWNER");
  assertWellFormedRequests(result.state);
  const open = openRequests(result.state);
  assert.equal(open.length, 1);
  assert.equal(open[0].origin, "open_finding");
  const finding = result.state.phase.findings.find((f) => f.kind === "integration")!;
  assert.equal(open[0].linkedFindingId, finding.id);
});

test("owner-requests: RESOLVING_INCOMPLETE with budget exhausted records one request per open item", () => {
  const decision: Decision = makeDecision({ id: "D-fail", class: "delegated", boundCandidateSha: "C1" });
  const reserved: Decision = makeDecision({ id: "D-reserved", class: "reserved", boundCandidateSha: "C1" });
  const ballots: Ballot[] = [
    { reviewer: "M", decisionId: "D-fail", vote: "reject", rationale: "x", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
    { reviewer: "A", decisionId: "D-fail", vote: "reject", rationale: "x", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
    { reviewer: "B", decisionId: "D-fail", vote: "reject", rationale: "x", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  ];
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
  const correction: Correction = {
    id: "C-1",
    version: 1,
    phaseId: "p1",
    targetRecordId: "D-other",
    correctionText: "x",
    contractChange: false,
    status: "open",
    boundContractVersion: K,
    grantedRounds: 3,
  };
  const state = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: { reviewer: "M", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
      A: { review: { reviewer: "A", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
      B: { review: { reviewer: "B", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
    },
    decisions: [decision, reserved],
    ballots,
    findings: [finding],
    corrections: [correction],
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
  });

  const result = reduce(state, { type: "RESOLVING_INCOMPLETE" });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.phase, "AWAITING_OWNER");
  assertWellFormedRequests(result.state);

  const open = openRequests(result.state);
  const origins = open.map((r) => r.origin).sort();
  assert.deepEqual(origins, ["failed_vote", "open_finding", "reserved_decision", "unaddressed_correction"]);

  const failedVoteRequest = open.find((r) => r.origin === "failed_vote")!;
  assert.equal(failedVoteRequest.linkedDecisionId, "D-fail");
  assert.ok(failedVoteRequest.relatedBallots && failedVoteRequest.relatedBallots.length === 3, "carries every ballot");
});

test("owner-requests: a repeat entry does not duplicate an already-open request for the same record", () => {
  const decision: Decision = makeDecision({ id: "D-fail", class: "delegated", boundCandidateSha: "C1" });
  const ballots: Ballot[] = [
    { reviewer: "M", decisionId: "D-fail", vote: "reject", rationale: "x", evidence: ["e"], boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  ];
  const existingRequest = {
    id: "OR-existing",
    version: 1,
    phaseId: "p1",
    reason: "already open",
    origin: "failed_vote" as const,
    linkedDecisionId: "D-fail",
    options: [{ id: "accept_as_implemented", label: "accept the decision as implemented" }, { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" }],
    status: "open" as const,
  };
  const state = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: { reviewer: "M", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
      A: { review: { reviewer: "A", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
      B: { review: { reviewer: "B", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
    },
    decisions: [decision],
    ballots,
    ownerRequests: [existingRequest],
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
  });

  const result = reduce(state, { type: "RESOLVING_INCOMPLETE" });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  const open = openRequests(result.state).filter((r) => r.linkedDecisionId === "D-fail");
  assert.equal(open.length, 1, "must not duplicate the existing open request");
  assert.equal(open[0].id, "OR-existing");
});

test("owner-requests: checks failing with the budget spent asks for the budget, not for decisions nobody voted on (run 0c99b1ff)", () => {
  const state = baseState({
    phase: "CHECKING",
    candidate: { sha: "C1", contractVersion: K },
    decisions: [makeDecision({ id: "D1", class: "delegated" }), makeDecision({ id: "D2", class: "delegated" })],
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
  });
  const result = reduce(state, { type: "CHECKS_FAILED" });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.phase, "AWAITING_OWNER");
  assertWellFormedRequests(result.state);
  const open = openRequests(result.state);
  assert.equal(open.length, 1, "no per-decision request before any review ran");
  assert.equal(open[0].origin, "repair_budget_exhausted");
  assert.deepEqual(open[0].options.map((o) => o.id), ["grant", "stop"]);
});
