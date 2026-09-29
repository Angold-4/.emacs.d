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
  const result = reduce(state, { type: "SUBMIT_PHASE", disclosures: [] });
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

// Phase 1b work-packet item 3: INTEGRITY_VIOLATED is a logged fact (design
// §2.2), not an in-memory-only flag — a record-only event (no phase-state
// change) that reduce.ts's applyRecordEvent handles, like BALLOT_CAST.
test("reduce: INTEGRITY_VIOLATED sets phase.integrityViolated without changing the phase state name", () => {
  const state = baseState({ phase: "CHECKING" });
  assert.equal(state.phase.integrityViolated, undefined);
  const result = reduce(state, { type: "INTEGRITY_VIOLATED", stage: "checks" });
  assert.equal(result.ok, true);
  assert.equal(result.state.phase.integrityViolated, true);
  assert.equal(result.state.phase.phase, "CHECKING");
});

test("reduce: INTEGRITY_VIOLATED is accepted from any phase state (a record event, not a transition)", () => {
  for (const phase of ["IMPLEMENTING", "FREEZING", "CHECKING", "PROBING", "REVIEWING"] as const) {
    const state = baseState({ phase });
    const result = reduce(state, { type: "INTEGRITY_VIOLATED", stage: "checks" });
    assert.equal(result.ok, true, `expected INTEGRITY_VIOLATED to be accepted from ${phase}`);
    assert.equal(result.state.phase.integrityViolated, true);
  }
});

// Decision briefs: BRIEFS_RECORDED is a logged record event (no phase-state
// change), merged by requestId so a restart rebuilds the briefs the views
// render and the status `needs you` line shows.
const BRIEF = {
  requestId: "F-M-9",
  question: "Should a vendor excluded before a weekend stay excluded when its market reopens?",
  today: "Pyth's NVDA product reopens Sunday 20:00 ET. (example unverified)",
  impact: "No market stops publishing.",
  options: [{ id: "accept_risk", label: "Keep as is", effect: "rejoins at once", cost: "one stale quote" }],
  recommendation: { option: "accept_risk", why: "IC §5" },
  related: [],
  evidence: ["message: F-M-9"],
};

test("reduce: BRIEFS_RECORDED records and merges briefs by requestId, changing no phase state", () => {
  const state = baseState({ phase: "AWAITING_OWNER" });
  const first = reduce(state, { type: "BRIEFS_RECORDED", briefs: [BRIEF] });
  assert.equal(first.ok, true);
  assert.equal(first.state.phase.phase, "AWAITING_OWNER");
  assert.equal(first.state.phase.briefs?.length, 1);
  assert.equal(first.state.phase.briefs?.[0].requestId, "F-M-9");
  const second = reduce(first.state, { type: "BRIEFS_RECORDED", briefs: [{ ...BRIEF, question: "A reworded question?" }] });
  assert.equal(second.ok, true);
  assert.equal(second.state.phase.briefs?.length, 1, "a later brief for the same request replaces the earlier one");
  assert.equal(second.state.phase.briefs?.[0].question, "A reworded question?");
});
