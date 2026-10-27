// Plan 07b (design D3): FINDING_FACT is a known event, records one doneWhen
// result per finding/candidate, and a passing fact on the current candidate
// closes the finding exactly as a reviewer's confirmation does. Replaying the
// same log twice must give the same state (the reducer stays a pure function
// of the event log), and the unknown-event check must not reject the new
// type.

import assert from "node:assert/strict";
import { test } from "node:test";

import { reduce } from "../../src/core/reduce.ts";
import type { Event, Finding } from "../../src/core/types.ts";
import { CV, baseState } from "./helpers.ts";

const K = CV();

function openFinding(): Finding {
  return {
    id: "F1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "fixed-a is missing",
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
    domain: "booking",
    item: "R1",
    where: "src/core/rounds.ts:1 fn",
    expected: "fixed-a exists",
    doneWhen: "test -f fixed-a",
  };
}

/** The state a hand-off records facts into: the finding was raised on C1, a
 * new candidate C2 exists and its checks passed. */
function repairableState() {
  return baseState({
    phase: "REVIEWING",
    candidate: { sha: "C2", contractVersion: K },
    checks: { candidateSha: "C2", passed: true },
    findings: [openFinding()],
  });
}

const LOG: Event[] = [
  { type: "FINDING_FACT", findingId: "F1", candidateSha: "C2", exitCode: 0, timedOut: false, outputTail: "ok" },
  { type: "FINDING_CONFIRMED_REPAIRED", findingId: "F1", byReviewer: "M", candidateSha: "C2" },
];

function applyAll(events: Event[]) {
  let state = repairableState();
  for (const event of events) {
    const result = reduce(state, event);
    assert.equal(result.ok, true, !result.ok ? result.reason : "");
    state = result.state;
  }
  return state;
}

test("FINDING_FACT events replay to the same findings", () => {
  const once = applyAll(LOG);
  const twice = applyAll(LOG);
  assert.deepEqual(once, twice, "reducing the same log twice gives the same state");
  assert.equal(once.phase.findings[0].status, "repaired", "the passing fact closed the finding");
  assert.equal(once.phase.findings[0].repairedByCandidateSha, "C2");
  assert.deepEqual(once.phase.doneWhenFacts, [
    { findingId: "F1", candidateSha: "C2", exitCode: 0, timedOut: false, outputTail: "ok" },
  ]);
});

test("the unknown-event check does not reject FINDING_FACT", () => {
  const result = reduce(repairableState(), {
    type: "FINDING_FACT",
    findingId: "F1",
    candidateSha: "C2",
    exitCode: 1,
    timedOut: false,
    outputTail: "still missing",
  });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.doneWhenFacts?.length, 1);
  assert.equal(result.state.phase.findings[0].status, "open", "a non-zero fact repairs nothing");
});

test("a non-zero or timed-out fact never closes the finding", () => {
  for (const fact of [
    { type: "FINDING_FACT" as const, findingId: "F1", candidateSha: "C2", exitCode: 1, timedOut: false, outputTail: "no" },
    { type: "FINDING_FACT" as const, findingId: "F1", candidateSha: "C2", exitCode: null, timedOut: true, outputTail: "" },
  ]) {
    let state = repairableState();
    let result = reduce(state, fact);
    assert.equal(result.ok, true, !result.ok ? result.reason : "");
    state = result.state;
    result = reduce(state, { type: "FINDING_CONFIRMED_REPAIRED", findingId: "F1", byReviewer: "M", candidateSha: "C2" });
    assert.equal(result.ok, false, "without a passing fact and without M's confirming review, the finding cannot close");
    assert.equal(result.state.phase.findings[0].status, "open");
  }
});
