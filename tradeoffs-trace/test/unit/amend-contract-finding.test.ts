// Round-3 review item 2: design §4.3's worked example, run end to end. "The
// budget runs out, and the owner gets a request: keep the guarantee, or
// amend the contract to 'acknowledge means queued'." AMEND must work from
// AWAITING_OWNER while the request from B's contract finding is still
// open, resolving that finding and moving the phase to CHECKING under v2 —
// and after fresh checks, probe and reviews, it can accept.

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import type { Event, Finding, State } from "../../src/core/types.ts";
import { CV, baseState } from "./helpers.ts";

const K = CV(1);
const K2 = CV(2);

function step(state: State, event: Event): State {
  const result = reduce(state, event);
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  return result.state;
}

test("amend-contract-finding: design §4.3 — the budget exhausts on B's contract finding, the owner amends, and the phase accepts under v2", () => {
  // The repair budget is exhausted with B's contract finding still open —
  // exactly design §4.3's "three repair rounds did not settle it".
  const contractFinding: Finding = {
    id: "F-p2-02",
    version: 1,
    phaseId: "p1",
    kind: "contract",
    severity: "blocking",
    evidence: "cancel can acknowledge before fills stop (src/cancel.ts:42)",
    raisedBy: "B",
    status: "open",
    boundCandidateSha: "C1",
  };
  let state: State = baseState({
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
    findings: [contractFinding],
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
  });
  state = { ...state, phase: { ...state.phase, contract: { ...state.phase.contract, contractVersion: K } } };

  state = step(state, { type: "RESOLVING_INCOMPLETE" });
  assert.equal(state.phase.phase, "AWAITING_OWNER");
  const request = state.phase.ownerRequests.find((r) => r.status === "open" && r.linkedFindingId === "F-p2-02")!;
  assert.ok(request, "the contract finding gets an owner request");
  // §4.2/§4.3: a `contract` finding's disposition is amending the contract
  // — "accept the risk" is not one of its options.
  assert.equal(request.options.some((o) => o.id === "accept_risk"), false);

  // The owner amends: "acknowledge means queued" — resolving the finding
  // and moving the phase to CHECKING under v2, even with the request open.
  state = step(state, {
    type: "AMEND",
    replacingContractVersion: K,
    newContractVersion: K2,
    resolvesFindingIds: ["F-p2-02"],
  });
  assert.equal(state.phase.phase, "CHECKING");
  assert.deepEqual(state.phase.contract.contractVersion, K2);
  const resolvedFinding = state.phase.findings.find((f) => f.id === "F-p2-02")!;
  assert.equal(resolvedFinding.status, "accepted");
  assert.equal(resolvedFinding.acceptedScope, "amended to v2");
  const supersededRequest = state.phase.ownerRequests.find((r) => r.id === request.id)!;
  assert.equal(supersededRequest.status, "resolved");
  assert.equal(supersededRequest.resolution?.option, "superseded_by_amend");

  // Fresh checks, probe and reviews under v2 — and it can accept.
  state = step(state, { type: "CHECKS_PASSED" });
  assert.equal(state.phase.phase, "PROBING");
  state = step(state, { type: "PROBE_PASSED", probedI: "I2" });
  assert.equal(state.phase.phase, "REVIEWING");
  for (const reviewer of ["M", "A", "B"] as const) {
    state = step(state, {
      type: "REVIEW_SUBMITTED",
      review: { reviewer, phaseId: "p1", candidateSha: "C1", contractVersion: K2, correctionStatements: [], findingStatements: [] },
    });
  }
  assert.equal(state.phase.phase, "RESOLVING");
  state = step(state, { type: "ACCEPTED", resolvedCorrectionIds: [] });
  assert.equal(state.phase.phase, "ACCEPTED");
});
