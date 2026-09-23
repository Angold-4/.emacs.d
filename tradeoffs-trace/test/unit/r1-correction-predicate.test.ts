// R1 phase-0 repair gate — F02: the correction-addressed predicate must
// fail closed. `addressed(X, C, K)` is true only when M, A and B each
// stated, in their review bound to (C, K), EXACTLY ONE `honored` statement
// for X — a missing statement, a second statement for the same id
// (identical or contradictory, in either order), or any non-`honored`
// status makes it false. The reviews used below are schema-valid (checked
// with the real schemas/review.schema.json), matching the shape a reviewer
// may actually submit.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";
import { accept, addressed } from "../../src/core/predicate.ts";
import { validate, type JSONSchema } from "../../src/core/schema.ts";
import type { Correction, Review } from "../../src/core/types.ts";
import { CV, baseState } from "./helpers.ts";

const K = CV(1);
const K2 = CV(2);
const REVIEW_SCHEMA = JSON.parse(readFileSync(new URL("../../schemas/review.schema.json", import.meta.url), "utf8")) as JSONSchema;

const X = "C-p2-01";

function reviewFor(
  who: "M" | "A" | "B",
  statements: { correctionId: string; status: "honored" | "not_honored" }[],
  candidateSha = "C2",
  contractVersion = K,
): Review {
  return {
    reviewer: who,
    phaseId: "p1",
    candidateSha,
    contractVersion,
    correctionStatements: statements,
    findingStatements: [],
  };
}

function assertSchemaValid(review: Review) {
  const result = validate(REVIEW_SCHEMA, review);
  assert.equal(result.valid, true, result.errors.join("; "));
}

function stateWith(reviews: { M: Review; A: Review; B: Review }, opts: { skipSchemaCheck?: boolean } = {}) {
  const correction: Correction = {
    id: X,
    version: 1,
    phaseId: "p1",
    targetRecordId: "D-p2-05",
    correctionText: "A lone cancellation must not wait for the next tick.",
    contractChange: false,
    status: "open",
    boundContractVersion: K,
    grantedRounds: 3,
  };
  const state = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C2", contractVersion: K },
    checks: { candidateSha: "C2", passed: true },
    probe: { candidateSha: "C2", head: "H0", probedI: "I2", passed: true },
    reviews: { M: { review: reviews.M }, A: { review: reviews.A }, B: { review: reviews.B } },
    corrections: [correction],
  });
  if (!opts.skipSchemaCheck) {
    assertSchemaValid(reviews.M);
    assertSchemaValid(reviews.A);
    assertSchemaValid(reviews.B);
  }
  return state;
}

function allHonored(): { M: Review; A: Review; B: Review } {
  return {
    M: reviewFor("M", [{ correctionId: X, status: "honored" }]),
    A: reviewFor("A", [{ correctionId: X, status: "honored" }]),
    B: reviewFor("B", [{ correctionId: X, status: "honored" }]),
  };
}

test("R1.correction: exactly one honored statement from each of M/A/B -> addressed and accept are true", () => {
  const state = stateWith(allHonored());
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), true);
  assert.equal(accept(state.phase, "C2", K), true);
});

test("R1.correction: {X,honored} then {X,not_honored} in one review -> false", () => {
  const reviews = allHonored();
  reviews.A = reviewFor("A", [
    { correctionId: X, status: "honored" },
    { correctionId: X, status: "not_honored" },
  ]);
  const state = stateWith(reviews);
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), false);
  assert.equal(accept(state.phase, "C2", K), false);
});

test("R1.correction: {X,not_honored} then {X,honored} in one review -> false (order-independent)", () => {
  const reviews = allHonored();
  reviews.A = reviewFor("A", [
    { correctionId: X, status: "not_honored" },
    { correctionId: X, status: "honored" },
  ]);
  const state = stateWith(reviews);
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), false);
  assert.equal(accept(state.phase, "C2", K), false);
});

test("R1.correction: duplicate identical {X,honored},{X,honored} -> false", () => {
  const reviews = allHonored();
  reviews.B = reviewFor("B", [
    { correctionId: X, status: "honored" },
    { correctionId: X, status: "honored" },
  ]);
  const state = stateWith(reviews);
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), false);
  assert.equal(accept(state.phase, "C2", K), false);
});

test("R1.correction: a review missing any statement for X -> false", () => {
  const reviews = allHonored();
  reviews.M = reviewFor("M", [{ correctionId: "C-other", status: "honored" }]);
  const state = stateWith(reviews);
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), false);
  assert.equal(accept(state.phase, "C2", K), false);
});

test("R1.correction: an invalid/legacy status for X -> false", () => {
  const reviews = allHonored();
  reviews.B = reviewFor("B", [{ correctionId: X, status: "honored" }]);
  reviews.B.correctionStatements[0].status = "maybe" as unknown as "honored";
  // This review is deliberately not schema-valid (legacy data), so skip
  // the schema assertion the other cases make.
  const state = stateWith(reviews, { skipSchemaCheck: true });
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), false);
  assert.equal(accept(state.phase, "C2", K), false);
});

test("R1.correction: a review bound to a different candidate -> false", () => {
  const reviews = allHonored();
  reviews.M = reviewFor("M", [{ correctionId: X, status: "honored" }], "C9");
  const state = stateWith(reviews);
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), false);
  assert.equal(accept(state.phase, "C2", K), false);
});

test("R1.correction: a review bound to a different contract version -> false", () => {
  const reviews = allHonored();
  reviews.A = reviewFor("A", [{ correctionId: X, status: "honored" }], "C2", K2);
  const state = stateWith(reviews);
  const correction = state.phase.corrections[0];
  assert.equal(addressed(correction, state.phase, "C2", K), false);
  assert.equal(accept(state.phase, "C2", K), false);
});
