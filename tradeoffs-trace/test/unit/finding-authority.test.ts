// finding-authority (design §4.2, §3.4): no vote outcome can close a
// finding; a non-raising reviewer cannot confirm/withdraw it; only the
// raising reviewer (repaired/disproved) or the owner (accepted) can; only
// the owner can lower severity/class.

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import type { Finding } from "../../src/core/types.ts";
import { CV, baseState, makeBallot, makeDecision } from "./helpers.ts";

const K = CV();

function withOpenFinding(raisedBy: "M" | "A" | "B" = "B") {
  const decision = makeDecision({ id: "D1", linkedFindingId: "F1" });
  const finding: Finding = {
    id: "F1",
    version: 1,
    phaseId: "p1",
    kind: "contract",
    severity: "blocking",
    evidence: "cancel acknowledges before fills stop",
    raisedBy,
    linkedDecisionId: "D1",
    status: "open",
    boundCandidateSha: "C1",
  };
  return baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    decisions: [decision],
    findings: [finding],
  });
}

/** A state where the finding was raised on C1, a new candidate C2 exists,
 * checks passed on C2, and `raisedBy`'s review of C2 confirms it — every
 * clause FINDING_CONFIRMED_REPAIRED now requires (design §4.2). */
function withRepairableFinding(raisedBy: "M" | "A" | "B") {
  const finding: Finding = {
    id: "F1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "race in cancel path",
    raisedBy,
    status: "open",
    boundCandidateSha: "C1",
  };
  return baseState({
    phase: "REVIEWING",
    candidate: { sha: "C2", contractVersion: K },
    checks: { candidateSha: "C2", passed: true },
    findings: [finding],
    reviews: {
      [raisedBy]: {
        review: {
          reviewer: raisedBy,
          phaseId: "p1",
          candidateSha: "C2",
          contractVersion: K,
          correctionStatements: [],
          findingStatements: [{ findingId: "F1", status: "confirm" }],
        },
      },
    },
  });
}

test("finding-authority: no tally result (pass or fail) changes a finding's status", () => {
  // Casting three approving ballots on the linked decision — which cannot
  // even be tallied while the finding is open (tally.ts: suspended) — must
  // not touch the finding itself; only reduce()'s explicit
  // FINDING_CONFIRMED_REPAIRED / FINDING_DISPROVED / FINDING_ACCEPTED_BY_OWNER
  // events do.
  let state = withOpenFinding("B");
  for (const reviewer of ["M", "A", "B"] as const) {
    const result = reduce(state, {
      type: "BALLOT_CAST",
      ballot: makeBallot({ reviewer, vote: "approve", boundRecordVersion: 1 }),
    });
    assert.equal(result.ok, true, !result.ok ? result.reason : "");
    state = result.state;
  }
  const finding = state.phase.findings.find((f) => f.id === "F1")!;
  assert.equal(finding.status, "open", "a vote outcome must never close a finding");
});

test("finding-authority: a non-raising reviewer cannot confirm a finding repaired", () => {
  const state = withRepairableFinding("B");
  const result = reduce(state, {
    type: "FINDING_CONFIRMED_REPAIRED",
    findingId: "F1",
    byReviewer: "A",
    candidateSha: "C2",
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /raising reviewer/);
  assert.equal(result.state.phase.findings[0].status, "open");
});

test("finding-authority: a non-raising reviewer cannot withdraw a finding", () => {
  const state = withOpenFinding("B");
  const result = reduce(state, {
    type: "FINDING_DISPROVED",
    findingId: "F1",
    byReviewer: "M",
    evidence: "counter-evidence",
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /raising reviewer/);
});

test("finding-authority: the raising reviewer can confirm a finding repaired only on a new, checked candidate whose review confirms it", () => {
  const state = withRepairableFinding("B");
  const result = reduce(state, {
    type: "FINDING_CONFIRMED_REPAIRED",
    findingId: "F1",
    byReviewer: "B",
    candidateSha: "C2",
  });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.findings[0].status, "repaired");
});

test("finding-authority: confirming repaired is rejected on the SAME candidate the finding was raised on", () => {
  const state = withRepairableFinding("B");
  // Report the current candidate as the one it was raised on (C1) instead of C2.
  const patched = { ...state, phase: { ...state.phase, candidate: { sha: "C1", contractVersion: K } } };
  const result = reduce(patched, {
    type: "FINDING_CONFIRMED_REPAIRED",
    findingId: "F1",
    byReviewer: "B",
    candidateSha: "C1",
  });
  assert.equal(result.ok, false);
});

test("finding-authority: confirming repaired is rejected if checks have not passed on the new candidate", () => {
  const state = withRepairableFinding("B");
  const noChecks = { ...state, phase: { ...state.phase, checks: undefined } };
  const result = reduce(noChecks, {
    type: "FINDING_CONFIRMED_REPAIRED",
    findingId: "F1",
    byReviewer: "B",
    candidateSha: "C2",
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /checks must have passed/);
});

test("finding-authority: confirming repaired is rejected without the raising reviewer's confirming review", () => {
  const state = withRepairableFinding("B");
  const noReview = { ...state, phase: { ...state.phase, reviews: {} } };
  const result = reduce(noReview, {
    type: "FINDING_CONFIRMED_REPAIRED",
    findingId: "F1",
    byReviewer: "B",
    candidateSha: "C2",
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /confirm it/);
});

test("finding-authority: the raising reviewer can withdraw (disprove) a finding", () => {
  const state = withOpenFinding("B");
  const result = reduce(state, {
    type: "FINDING_DISPROVED",
    findingId: "F1",
    byReviewer: "B",
    evidence: "the acknowledgement already implies the guarantee",
  });
  assert.equal(result.ok, true);
  assert.equal(result.state.phase.findings[0].status, "disproved");
});

test("finding-authority: FINDING_DISPROVED rejects empty counter-evidence", () => {
  const state = withOpenFinding("B");
  const result = reduce(state, {
    type: "FINDING_DISPROVED",
    findingId: "F1",
    byReviewer: "B",
    evidence: "   ",
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /counter-evidence/);
});

test("finding-authority: FINDING_RAISED rejects a finding with no evidence", () => {
  const state = baseState({ candidate: { sha: "C1", contractVersion: K } });
  const finding: Finding = {
    id: "F2",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "",
    raisedBy: "A",
    status: "open",
    boundCandidateSha: "C1",
  };
  const result = reduce(state, { type: "FINDING_RAISED", finding });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /evidence/);
});

test("finding-authority: only the owner can accept a finding", () => {
  // A `defect` finding, not `contract`: a contract finding's only
  // disposition is amending the contract (design §4.2), covered by
  // test/unit/r1-contract-authority.test.ts.
  const finding: Finding = {
    id: "F1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "race in cancel path",
    raisedBy: "B",
    status: "open",
    boundCandidateSha: "C1",
  };
  const state = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    findings: [finding],
  });
  const binding = { boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 };
  const notOwner = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "accepted for this phase only",
    by: "reviewer-B",
    ...binding,
  });
  assert.equal(notOwner.ok, false);

  const owner = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "accepted for this phase only",
    by: "owner",
    ...binding,
  });
  assert.equal(owner.ok, true, !owner.ok ? owner.reason : "");
  assert.equal(owner.state.phase.findings[0].status, "accepted");
});

test("finding-authority: only the owner can lower a finding's severity", () => {
  const state = withOpenFinding("B");
  const binding = { boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 };
  const notOwner = reduce(state, {
    type: "FINDING_SEVERITY_LOWERED",
    findingId: "F1",
    severity: "advisory",
    by: "reviewer-M",
    ...binding,
  });
  assert.equal(notOwner.ok, false);

  const owner = reduce(state, {
    type: "FINDING_SEVERITY_LOWERED",
    findingId: "F1",
    severity: "advisory",
    by: "owner",
    ...binding,
  });
  assert.equal(owner.ok, true, !owner.ok ? owner.reason : "");
  assert.equal(owner.state.phase.findings[0].severity, "advisory");
});

test("finding-authority: only the owner can lower a decision's class", () => {
  const decision = makeDecision({ id: "D1", class: "reserved" });
  const state = baseState({ candidate: { sha: "C1", contractVersion: K }, decisions: [decision] });
  const binding = { boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 };
  const notOwner = reduce(state, {
    type: "DECISION_CLASS_LOWERED",
    decisionId: "D1",
    class: "delegated",
    by: "worker",
    ...binding,
  });
  assert.equal(notOwner.ok, false);

  const owner = reduce(state, {
    type: "DECISION_CLASS_LOWERED",
    decisionId: "D1",
    class: "delegated",
    by: "owner",
    ...binding,
  });
  assert.equal(owner.ok, true, !owner.ok ? owner.reason : "");
  assert.equal(owner.state.phase.decisions[0].class, "delegated");
});
