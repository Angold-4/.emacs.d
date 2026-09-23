// R1 phase-0 repair gate — F02 ingestion: a review that states a
// correction's disposition twice must be rejected by the real reducer
// before it can become state. A predicate unit test alone is not enough.

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import type { Review } from "../../src/core/types.ts";
import { CV, baseState } from "./helpers.ts";

const K = CV(1);

function reviewingState() {
  return baseState({
    phase: "REVIEWING",
    candidate: { sha: "C1", contractVersion: K },
  });
}

function review(statements: { correctionId: string; status: "honored" | "not_honored" }[]): Review {
  return {
    reviewer: "M",
    phaseId: "p1",
    candidateSha: "C1",
    contractVersion: K,
    correctionStatements: statements,
    findingStatements: [],
  };
}

test("R1.ingestion: a REVIEW_SUBMITTED repeating a correctionId is rejected with a reason and leaves state unchanged", () => {
  const state = reviewingState();
  const payload = review([
    { correctionId: "C-1", status: "honored" },
    { correctionId: "C-1", status: "not_honored" },
  ]);
  const result = reduce(state, { type: "REVIEW_SUBMITTED", review: payload });
  assert.equal(result.ok, false, "duplicate correctionId must be rejected at ingestion");
  assert.match((result as { reason: string }).reason, /C-1/);
  assert.match((result as { reason: string }).reason, /(more than once|twice|duplicate)/i);
  assert.deepEqual(result.state, state, "a rejected review must not change state");
  assert.deepEqual(result.state.phase.reviews, {});
});

test("R1.ingestion: a well-formed REVIEW_SUBMITTED for the same state is still accepted", () => {
  const state = reviewingState();
  const result = reduce(state, {
    type: "REVIEW_SUBMITTED",
    review: review([
      { correctionId: "C-1", status: "honored" },
      { correctionId: "C-2", status: "not_honored" },
    ]),
  });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.ok(result.state.phase.reviews.M?.review, "the well-formed review is recorded");
});
