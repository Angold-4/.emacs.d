// accept()'s owner-authority and binding rules (design §6.3, round-1 review
// item 5): a delegated decision may also be settled by an owner `override`
// bound to (C, K, decision version); a reserved decision needs its owner
// request resolved bound to (C, K); the probe must be onto the CURRENT
// integration head.

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import { accept } from "../../src/core/predicate.ts";
import { CV, baseState, makeBallot, makeDecision } from "./helpers.ts";

const K = CV();

function acceptableBase(overrides: Partial<import("../../src/core/types.ts").PhaseState> = {}) {
  return baseState({
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
    ...overrides,
  }).phase;
}

test("accept-authority: a delegated decision that failed its vote is settled by an owner override bound to (C, K, version)", () => {
  const decision = makeDecision({ id: "D1", class: "delegated", version: 1 });
  const ballots = [
    makeBallot({ reviewer: "M", vote: "reject", boundRecordVersion: 1 }),
    makeBallot({ reviewer: "A", vote: "reject", boundRecordVersion: 1 }),
    makeBallot({ reviewer: "B", vote: "reject", boundRecordVersion: 1 }),
  ];
  const withoutOverride = acceptableBase({ decisions: [decision], ballots });
  assert.equal(accept(withoutOverride, "C1", K), false, "a failed vote with no override must not accept");

  const state = { run: "RUN_ACTIVE" as const, phase: withoutOverride };
  const overridden = reduce(state, {
    type: "OVERRIDE_CAST",
    override: { decisionId: "D1", vote: "approve", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  });
  assert.equal(overridden.ok, true, !overridden.ok ? overridden.reason : "");
  assert.equal(accept(overridden.state.phase, "C1", K), true, "an owner override bound to (C, K, version) must settle it");
});

test("accept-authority: an override bound to a superseded decision version is rejected and does not settle it", () => {
  const decision = makeDecision({ id: "D1", class: "delegated", version: 2 });
  const ballots = [
    makeBallot({ reviewer: "M", vote: "reject", boundRecordVersion: 2 }),
    makeBallot({ reviewer: "A", vote: "reject", boundRecordVersion: 2 }),
    makeBallot({ reviewer: "B", vote: "reject", boundRecordVersion: 2 }),
  ];
  const phase = acceptableBase({ decisions: [decision], ballots });
  const state = { run: "RUN_ACTIVE" as const, phase };
  const result = reduce(state, {
    type: "OVERRIDE_CAST",
    override: { decisionId: "D1", vote: "approve", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  });
  assert.equal(result.ok, false);
});

test("accept-authority: a reserved decision needs its owner request resolved bound to (C, K), with the 'approve' option", () => {
  const decision = makeDecision({ id: "D1", class: "reserved" });
  const unresolved = acceptableBase({
    decisions: [decision],
    ownerRequests: [{ id: "OR-1", version: 1, phaseId: "p1", reason: "reserved", origin: "reserved_decision", linkedDecisionId: "D1", options: [{ id: "approve", label: "approve" }, { id: "reject_and_repair", label: "reject and repair (grant 3 rounds)" }], status: "open" }],
  });
  assert.equal(accept(unresolved, "C1", K), false);

  const resolvedElsewhere = acceptableBase({
    decisions: [decision],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [],
        status: "resolved",
        resolution: { option: "approve" },
        resolvedBinding: { candidateSha: "C0-other", contractVersion: K }, // bound to a DIFFERENT candidate
      },
    ],
  });
  assert.equal(accept(resolvedElsewhere, "C1", K), false, "resolution bound to a different candidate must not count");

  const resolvedWithRejectAndRepair = acceptableBase({
    decisions: [decision],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [],
        status: "resolved",
        resolution: { option: "reject_and_repair" }, // grants rounds; does NOT settle
        resolvedBinding: { candidateSha: "C1", contractVersion: K },
      },
    ],
  });
  assert.equal(accept(resolvedWithRejectAndRepair, "C1", K), false, "'reject_and_repair' must not settle the decision");

  const resolvedHere = acceptableBase({
    decisions: [decision],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "reserved",
        origin: "reserved_decision",
        linkedDecisionId: "D1",
        options: [],
        status: "resolved",
        resolution: { option: "approve" },
        resolvedBinding: { candidateSha: "C1", contractVersion: K },
      },
    ],
  });
  assert.equal(accept(resolvedHere, "C1", K), true);
});

test("accept-authority: the probe must be onto the CURRENT integration head — a stale probe does not accept", () => {
  const phase = acceptableBase({
    integrationHead: "H1", // the head moved (a stale publish happened)
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true }, // still against the OLD head
  });
  assert.equal(accept(phase, "C1", K), false);
});
