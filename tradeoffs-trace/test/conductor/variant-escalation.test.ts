// Plan 06k1 (A3): the variant limit. When the same requirement takes a new
// blocking finding of the same kind in N consecutive rounds, the Nth opens an
// owner request and no repair attempt starts. `#+TT_VARIANT_LIMIT` moves N.

import assert from "node:assert/strict";
import { test } from "node:test";

import { parseOrgPlan } from "../../src/core/org-plan.ts";
import { reduce } from "../../src/core/reduce.ts";
import { variantEscalation, variantLimitOf } from "../../src/core/triage.ts";
import { basePhase, baseState, CV } from "../unit/helpers.ts";
import type { Finding, PhaseState } from "../../src/core/types.ts";

const K = CV();
const C1 = { sha: "C1", contractVersion: K };

function finding(id: string, roundRaised: number, overrides: Partial<Finding> = {}): Finding {
  return {
    id,
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "src/x.ts:1 it works is unmet",
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
    itemId: "R1",
    roundRaised,
    ...overrides,
  };
}

function resolvingPhase(findings: Finding[], round: number, variantLimit?: number): PhaseState {
  return basePhase({
    phase: "RESOLVING",
    candidate: C1,
    integrationHead: "H0",
    round,
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: { reviewer: "M", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [], ballots: [] } as never },
      A: { review: { reviewer: "A", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [], ballots: [] } as never },
      B: { review: { reviewer: "B", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [], ballots: [] } as never },
    },
    findings,
    contract: { ...basePhase().contract, ...(variantLimit ? { variantLimit } : {}) },
  });
}

test("plan 06k1: a third consecutive round with a new blocking finding of the same kind on one requirement parks it for the owner", () => {
  // Rounds 1 and 2 repaired a blocking defect on R1; round 3 raises a third.
  const third = resolvingPhase(
    [finding("F-1", 1, { status: "repaired" }), finding("F-2", 2, { status: "repaired" }), finding("F-3", 3)],
    3,
  );
  assert.equal(variantLimitOf(third.contract), 3, "the default variant limit is 3");
  const escalation = variantEscalation(third);
  assert.ok(escalation, "the variant limit is reached");
  assert.equal(escalation!.itemId, "R1");
  assert.deepEqual(escalation!.rounds, [1, 2, 3]);

  // The phase parks for the owner instead of starting another repair.
  const result = reduce(baseState(third), { type: "RESOLVING_INCOMPLETE" });
  assert.ok(result.ok, "the event applies");
  assert.equal(result.state.phase.phase, "AWAITING_OWNER", "no repair attempt starts");
  const request = result.state.phase.ownerRequests.find((r) => r.status === "open");
  assert.ok(request, "an owner request is open");
  assert.equal(request!.linkedFindingId, "F-3", "the owner request is about the third variant");

  // A gap in the rounds means no escalation: the same kind was not raised in
  // three CONSECUTIVE rounds.
  const gap = resolvingPhase([finding("F-1", 1), finding("F-2", 2), finding("F-3", 4)], 4);
  assert.equal(variantEscalation(gap), undefined, "a gap breaks the streak");

  // A different kind, or a different requirement, does not count.
  const otherKind = resolvingPhase(
    [finding("F-1", 1), finding("F-2", 2, { kind: "integration" }), finding("F-3", 3)],
    3,
  );
  assert.equal(variantEscalation(otherKind), undefined, "the kind must match");
  const otherItem = resolvingPhase(
    [finding("F-1", 1), finding("F-2", 2, { itemId: "R2" }), finding("F-3", 3)],
    3,
  );
  assert.equal(variantEscalation(otherItem), undefined, "the requirement must match");

  // Finding D-M-60: only a REVIEWER's finding that is still open or was
  // repaired is a real variant. A conductor-raised finding (one per candidate)
  // is not a new variant, and a withdrawn/disproved one never establishes a
  // streak.
  const conductorRaised = resolvingPhase(
    [finding("F-1", 1), finding("F-2", 2, { raisedBy: "conductor" }), finding("F-3", 3)],
    3,
  );
  assert.equal(variantEscalation(conductorRaised), undefined, "a conductor-raised finding is not a variant");
  const withdrawn = resolvingPhase(
    [finding("F-1", 1), finding("F-2", 2, { status: "disproved" }), finding("F-3", 3)],
    3,
  );
  assert.equal(variantEscalation(withdrawn), undefined, "a withdrawn finding is not a variant");
  const superseded = resolvingPhase(
    [finding("F-1", 1), finding("F-2", 2, { status: "superseded" }), finding("F-3", 3)],
    3,
  );
  assert.equal(variantEscalation(superseded), undefined, "a superseded finding is not a variant");

  // `#+TT_VARIANT_LIMIT: 2` moves the escalation to the second round.
  const plan = parseOrgPlan(
    [
      "#+TITLE: t",
      "#+TT_REPO: /tmp/r",
      "#+TT_BRANCH: main",
      "#+TT_CHECKS: true",
      "#+TT_VARIANT_LIMIT: 2",
      "* Phase 1: p1",
      "  :PROPERTIES:",
      "  :ID: p1",
      "  :CHECKS: true",
      "  :END:",
      "  Goal: g",
      "  Acceptance:",
      "  - it works",
    ].join("\n") + "\n",
  );
  assert.equal(plan.variantLimit, 2, "the plan keyword is parsed");
  const two = resolvingPhase([finding("F-1", 1), finding("F-2", 2)], 2, plan.variantLimit);
  assert.equal(variantLimitOf(two.contract), 2);
  assert.ok(variantEscalation(two), "with limit 2 the second round escalates");
  const parked = reduce(baseState(two), { type: "RESOLVING_INCOMPLETE" });
  assert.equal(parked.state.phase.phase, "AWAITING_OWNER");

  // With limit 2 but only one round of findings, the phase still repairs.
  const one = resolvingPhase([finding("F-1", 1)], 1, 2);
  assert.equal(variantEscalation(one), undefined);
  const repaired = reduce(baseState(one), { type: "RESOLVING_INCOMPLETE" });
  assert.equal(repaired.state.phase.phase, "REPAIRING", "a single round still repairs");
});
