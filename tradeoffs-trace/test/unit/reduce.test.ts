// reduce.ts: a TOTAL function that never throws, and explicitly rejects
// unknown or out-of-order events (phase-0 Goal 3).

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import { baseState } from "./helpers.ts";

test("reduce: rejects an unknown event type explicitly", () => {
  const state = baseState({ phase: "READY" });
  const result = reduce(state, { type: "SOMETHING_MADE_UP" });
  assert.equal(result.ok, false);
  assert.match(result.reason, /unknown event type/);
  assert.equal(result.state, state);
});

test("reduce: rejects a malformed event (no 'type') explicitly, without throwing", () => {
  const state = baseState({ phase: "READY" });
  assert.doesNotThrow(() => reduce(state, { oops: true }));
  const result = reduce(state, { oops: true });
  assert.equal(result.ok, false);
  assert.match(result.reason, /malformed event/);
});

test("reduce: rejects null/undefined/non-object input explicitly, without throwing", () => {
  const state = baseState({ phase: "READY" });
  for (const bad of [null, undefined, 42, "SUBMIT_PHASE", []]) {
    assert.doesNotThrow(() => reduce(state, bad));
    const result = reduce(state, bad);
    assert.equal(result.ok, false);
  }
});

test("reduce: rejects an out-of-order event (SUBMIT_PHASE while READY)", () => {
  const state = baseState({ phase: "READY" });
  const result = reduce(state, { type: "SUBMIT_PHASE", decisions: [] });
  assert.equal(result.ok, false);
  assert.match(result.reason, /out of order|no rule/);
  assert.equal(result.state.phase.phase, "READY");
});

test("reduce: rejects an out-of-order event (CHECKS_PASSED while IMPLEMENTING)", () => {
  const state = baseState({ phase: "IMPLEMENTING" });
  const result = reduce(state, { type: "CHECKS_PASSED" });
  assert.equal(result.ok, false);
});

test("reduce: rejects ACCEPTED from RESOLVING when accept(C, K) does not hold", () => {
  const state = baseState({ phase: "RESOLVING" }); // no candidate, no checks, no reviews
  const result = reduce(state, { type: "ACCEPTED", resolvedCorrectionIds: [] });
  assert.equal(result.ok, false);
});

test("reduce: a valid transition is idempotent-safe — the input state object is never mutated", () => {
  const state = baseState({ phase: "READY" });
  const before = JSON.stringify(state);
  reduce(state, { type: "ATTEMPT_STARTED" });
  assert.equal(JSON.stringify(state), before);
});
