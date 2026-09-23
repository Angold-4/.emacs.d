// Round-2 review item 2: AWAITING_OWNER "leaves only through an owner
// command" (design §6.1), and only once no owner request remains open.
// This exercises the full sequence — stays after the first command,
// clears after the second — end to end through reduce(), not just one row
// at a time (see test/contract/transitions.test.ts for the per-row proof).

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import type { Event, State } from "../../src/core/types.ts";
import { CV, baseState, makeDecision } from "./helpers.ts";

const K = CV();

function step(state: State, event: Event): State {
  const result = reduce(state, event);
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  return result.state;
}

test("awaiting-owner-exits: resolving two independent open findings, one at a time, only leaves AWAITING_OWNER after the second", () => {
  let state = baseState({
    phase: "AWAITING_OWNER",
    candidate: { sha: "C1", contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: { reviewer: "M", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
      A: { review: { reviewer: "A", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
      B: { review: { reviewer: "B", phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
    },
    findings: [
      { id: "F1", version: 1, phaseId: "p1", kind: "defect", severity: "blocking", evidence: "x", raisedBy: "B", status: "open", boundCandidateSha: "C1" },
      { id: "F2", version: 1, phaseId: "p1", kind: "defect", severity: "blocking", evidence: "y", raisedBy: "A", status: "open", boundCandidateSha: "C1" },
    ],
    ownerRequests: [
      { id: "OR-1", version: 1, phaseId: "p1", reason: "F1 open", origin: "open_finding", linkedFindingId: "F1", options: [{ id: "accept_risk", label: "accept the risk" }, { id: "repair", label: "repair (grant 3 rounds)" }], status: "open" },
      { id: "OR-2", version: 1, phaseId: "p1", reason: "F2 open", origin: "open_finding", linkedFindingId: "F2", options: [{ id: "accept_risk", label: "accept the risk" }, { id: "repair", label: "repair (grant 3 rounds)" }], status: "open" },
    ],
  });

  state = step(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "known limitation",
    by: "owner",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(state.phase.phase, "AWAITING_OWNER", "F2's request is still open, so the phase must stay parked");
  assert.equal(state.phase.ownerRequests.find((r) => r.id === "OR-1")!.status, "resolved");
  assert.equal(state.phase.ownerRequests.find((r) => r.id === "OR-2")!.status, "open");

  state = step(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F2",
    scope: "known limitation",
    by: "owner",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(state.phase.phase, "RESOLVING", "no open request remains, and checks/probe/reviews are valid");

  const accepted = step(state, { type: "ACCEPTED", resolvedCorrectionIds: [] });
  assert.equal(accepted.phase.phase, "ACCEPTED");
});

test("awaiting-owner-exits: granting the budget request moves REPAIRING, consuming the grant", () => {
  const decision = makeDecision({ id: "D1", class: "reserved" });
  const state = baseState({
    phase: "AWAITING_OWNER",
    candidate: { sha: "C1", contractVersion: K },
    decisions: [decision],
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
    ownerRequests: [
      { id: "OR-gate", version: 1, phaseId: "p1", reason: "checks kept failing", origin: "repair_budget_exhausted", options: [{ id: "grant", label: "grant 3 more repair rounds" }, { id: "stop", label: "stop the phase" }], status: "open" },
    ],
  });
  const granted = step(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-gate",
    option: "grant",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(granted.phase.phase, "REPAIRING");
  assert.equal(granted.phase.repairRoundsGranted, 6);
});

test("awaiting-owner-exits: stopping the phase moves to BLOCKED citing the request", () => {
  const state = baseState({
    phase: "AWAITING_OWNER",
    candidate: { sha: "C1", contractVersion: K },
    ownerRequests: [
      { id: "OR-gate", version: 1, phaseId: "p1", reason: "checks kept failing", origin: "repair_budget_exhausted", options: [{ id: "grant", label: "grant 3 more repair rounds" }, { id: "stop", label: "stop the phase" }], status: "open" },
    ],
  });
  const stopped = step(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-gate",
    option: "stop",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(stopped.phase.phase, "BLOCKED");
  assert.match(stopped.phase.blockedReason ?? "", /OR-gate/);
});
