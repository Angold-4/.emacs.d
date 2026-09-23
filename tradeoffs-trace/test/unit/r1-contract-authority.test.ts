// R1 phase-0 repair gate — F01: an open `contract` finding can never be
// closed by FINDING_ACCEPTED_BY_OWNER. Design §4.2: for `contract`,
// accepting "means amending the contract (§7.3)" — so the only disposition
// is an explicit AMEND, which must keep working.

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import type { Finding } from "../../src/core/types.ts";
import { CV, baseState } from "./helpers.ts";

const K = CV(1);
const K2 = CV(2);

function contractFindingState() {
  const finding: Finding = {
    id: "F1",
    version: 1,
    phaseId: "p1",
    kind: "contract",
    severity: "blocking",
    evidence: "cancel can acknowledge before fills stop (src/cancel.ts:42)",
    raisedBy: "B",
    status: "open",
    boundCandidateSha: "C1",
  };
  return baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    contract: {
      phaseId: "p1",
      contractVersion: K,
      goal: "make cancellation race-free",
      acceptance: ["no fill after cancel is acknowledged"],
      checks: ["npm test"],
      boundaries: [],
      reserved: [],
    },
    findings: [finding],
  });
}

test("R1.contract: direct FINDING_ACCEPTED_BY_OWNER of an open contract finding is rejected and changes nothing", () => {
  const state = contractFindingState();
  const result = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F1",
    scope: "accepted for this phase only",
    by: "owner",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, false, "contract findings must not be closed by acceptance");
  assert.match((result as { reason: string }).reason, /contract/i);
  // State and the contract version are unchanged.
  assert.deepEqual(result.state, state);
  assert.equal(result.state.phase.findings[0].status, "open");
  assert.equal(result.state.phase.contract.contractVersion.snapshot, K.snapshot);
});

test("R1.contract: the explicit AMEND path still resolves the contract finding and moves to CHECKING under the new version", () => {
  const state = contractFindingState();
  const result = reduce(state, {
    type: "AMEND",
    replacingContractVersion: K,
    newContractVersion: K2,
    resolvesFindingIds: ["F1"],
  });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.phase, "CHECKING");
  assert.deepEqual(result.state.phase.contract.contractVersion, K2);
  const finding = result.state.phase.findings.find((f) => f.id === "F1")!;
  assert.equal(finding.status, "accepted");
  assert.equal(finding.acceptedScope, "amended to v2");
  // Design §7.3 / the existing amend test: prior evidence is invalidated.
  assert.equal(result.state.phase.checks, undefined);
  assert.equal(result.state.phase.probe, undefined);
  assert.deepEqual(result.state.phase.reviews, {});
});
