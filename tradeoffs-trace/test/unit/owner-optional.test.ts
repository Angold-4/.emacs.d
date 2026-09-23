// owner-optional: the owner is never a blocker. A reserved decision is voted
// by M, A and B like a delegated one and only flagged for the owner (run
// b46255dc: every reviewer approved, two reserved decisions were the only
// blockers, and the conductor started a repair the worker could not use).

import assert from "node:assert/strict";
import { test } from "node:test";

import { accept, decisionStatus } from "../../src/core/predicate.ts";
import { approvingReview, baseState, CV, makeBallot, makeDecision } from "./helpers.ts";

const K = CV();

function reviewedPhase(ballots: ReturnType<typeof makeBallot>[]) {
  const reserved1 = makeDecision({ id: "D-r1", class: "reserved" });
  const reserved2 = makeDecision({ id: "D-r2", class: "reserved" });
  return baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: approvingReview("M", "C1", K) },
      A: { review: approvingReview("A", "C1", K) },
      B: { review: approvingReview("B", "C1", K) },
    },
    decisions: [reserved1, reserved2],
    ballots,
  });
}

const allApprove = ["D-r1", "D-r2"].flatMap((id) =>
  (["M", "A", "B"] as const).map((reviewer) => makeBallot({ decisionId: id, reviewer, vote: "approve" })),
);

test("owner-optional: reserved decisions every reviewer approves are accepted with no owner input", () => {
  const s = reviewedPhase(allApprove);
  assert.equal(accept(s.phase, "C1", K), true);
  const status = decisionStatus(s.phase.decisions[0], s.phase);
  assert.equal(status.status, "passed");
  assert.equal(status.flagged, true, "still flagged for the owner to see");
});

test("owner-optional: a reserved decision M vetoes fails like a delegated one, never as an owner wait", () => {
  const ballots = allApprove.map((b) => (b.decisionId === "D-r1" && b.reviewer === "M" ? { ...b, vote: "reject" as const } : b));
  const s = reviewedPhase(ballots);
  assert.equal(accept(s.phase, "C1", K), false);
  const status = decisionStatus(s.phase.decisions[0], s.phase);
  assert.equal(status.status, "failed");
  assert.equal(status.reason, "M veto");
  assert.notEqual(status.status, "owner");
});
