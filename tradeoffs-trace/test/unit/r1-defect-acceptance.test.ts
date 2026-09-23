// R1 phase-0 repair gate — F01: a `defect` (non-contract) finding may be
// accepted by the owner, but only with owner authority, a current
// (candidate, contract, record) binding AND a nonblank scope.

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import type { Finding } from "../../src/core/types.ts";
import { CV, baseState } from "./helpers.ts";

const K = CV(1);

function defectFindingState() {
  const finding: Finding = {
    id: "F1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "race in cancel path (src/cancel.ts:42)",
    raisedBy: "B",
    status: "open",
    boundCandidateSha: "C1",
  };
  return baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    findings: [finding],
  });
}

const validBinding = { boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 };

test("R1.defect: a valid scoped owner acceptance succeeds and records the scope", () => {
  const state = defectFindingState();
  const result = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "accepted for this phase only",
    by: "owner",
    ...validBinding,
  });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  const finding = result.state.phase.findings[0];
  assert.equal(finding.status, "accepted");
  assert.equal(finding.acceptedScope, "accepted for this phase only");
});

test("R1.defect: a blank/whitespace-only scope is rejected", () => {
  for (const scope of ["", "   \t\n "]) {
    const state = defectFindingState();
    const result = reduce(state, {
      type: "FINDING_ACCEPTED_BY_OWNER",
      findingId: "F1",
      scope,
      by: "owner",
      ...validBinding,
    });
    assert.equal(result.ok, false, `scope ${JSON.stringify(scope)} must be rejected`);
    assert.match((result as { reason: string }).reason, /scope/i);
    assert.equal(result.state.phase.findings[0].status, "open");
  }
});

test("R1.defect: an actor other than the owner is rejected", () => {
  const state = defectFindingState();
  const result = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "accepted for this phase only",
    by: "reviewer-B",
    ...validBinding,
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /owner/);
});

test("R1.defect: a stale candidate sha is rejected", () => {
  const state = defectFindingState();
  const result = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "accepted for this phase only",
    by: "owner",
    ...validBinding,
    boundCandidateSha: "C9",
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /candidate/);
});

test("R1.defect: a stale contract version is rejected", () => {
  const state = defectFindingState();
  const result = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "accepted for this phase only",
    by: "owner",
    ...validBinding,
    boundContractVersion: CV(2),
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /contract/);
});

test("R1.defect: a stale finding record version is rejected", () => {
  const state = defectFindingState();
  const result = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "accepted for this phase only",
    by: "owner",
    ...validBinding,
    boundRecordVersion: 2,
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /changed v/i);
});
