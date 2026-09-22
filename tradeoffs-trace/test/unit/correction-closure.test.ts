// correction-closure (phase-0 exit gate): a correction raised on candidate
// C1 is addressed by C2 whose three reviews say "honored"; accept(C2)
// holds; the ACCEPTED event marks it resolved. Reverse: one review says
// "not honored" -> not accepted.

import assert from "node:assert/strict";
import { test } from "node:test";
import { accept, addressed } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { Correction, Review } from "../../src/core/types.ts";
import { CV, approvingReview, baseState } from "./helpers.ts";

const K = CV();

function resolvingStateWithCorrection(reviewStatuses: {
  M: "honored" | "not_honored";
  A: "honored" | "not_honored";
  B: "honored" | "not_honored";
}) {
  const correction: Correction = {
    id: "C-p2-01",
    version: 1,
    phaseId: "p1",
    targetRecordId: "D-p2-05",
    correctionText: "A lone cancellation must not wait for the next tick.",
    contractChange: false,
    status: "open",
    boundContractVersion: K,
    grantedRounds: 3,
  };

  const reviews: Record<"M" | "A" | "B", Review> = {
    M: approvingReview("M", "C2", K, { correctionStatements: [{ correctionId: correction.id, status: reviewStatuses.M }] }),
    A: approvingReview("A", "C2", K, { correctionStatements: [{ correctionId: correction.id, status: reviewStatuses.A }] }),
    B: approvingReview("B", "C2", K, { correctionStatements: [{ correctionId: correction.id, status: reviewStatuses.B }] }),
  };

  return baseState({
    phase: "RESOLVING",
    candidate: { sha: "C2", contractVersion: K },
    checks: { candidateSha: "C2", passed: true },
    probe: { candidateSha: "C2", head: "H0", probedI: "I2", passed: true },
    reviews: { M: { review: reviews.M }, A: { review: reviews.A }, B: { review: reviews.B } },
    corrections: [correction],
  });
}

test("correction-closure: addressed(X, C2, K) holds when all three reviews say honored", () => {
  const state = resolvingStateWithCorrection({ M: "honored", A: "honored", B: "honored" });
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), true);
});

test("correction-closure: accept(C2, K) holds once the correction is addressed", () => {
  const state = resolvingStateWithCorrection({ M: "honored", A: "honored", B: "honored" });
  assert.equal(accept(state.phase, "C2", K), true);
});

test("correction-closure: the ACCEPTED event marks the correction resolved, in the same append", () => {
  const state = resolvingStateWithCorrection({ M: "honored", A: "honored", B: "honored" });
  const result = reduce(state, { type: "ACCEPTED", resolvedCorrectionIds: ["C-p2-01"] });
  assert.equal(result.ok, true);
  assert.equal(result.state.phase.phase, "ACCEPTED");
  const correction = result.state.phase.corrections.find((c) => c.id === "C-p2-01")!;
  assert.equal(correction.status, "resolved");
});

test("correction-closure (reverse): a single 'not honored' review keeps the candidate from being accepted", () => {
  const state = resolvingStateWithCorrection({ M: "honored", A: "not_honored", B: "honored" });
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), false);
  assert.equal(accept(state.phase, "C2", K), false);

  const result = reduce(state, { type: "ACCEPTED", resolvedCorrectionIds: ["C-p2-01"] });
  assert.equal(result.ok, false);
  assert.equal(result.state.phase.phase, "RESOLVING");
});
