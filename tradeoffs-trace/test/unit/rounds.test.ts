// Plan 06g: the round's pure rules — pickWinner, blocksAcceptance and
// roundBudget. These are the only places that decide a round's winner,
// whether a finding blocks acceptance, and how many rounds a phase may
// spend (architecture A3 and A6); no model decides any of the three.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import { reduce } from "../../src/core/reduce.ts";
import { validate } from "../../src/core/schema.ts";
import { baseState, CV } from "./helpers.ts";

import {
  advisoryReason,
  blocksAcceptance,
  candidateLabel,
  DEFAULT_ROUND_BUDGET,
  laneIds,
  openAdvisories,
  passingCandidates,
  pickWinner,
  roundBudget,
} from "../../src/core/rounds.ts";
import type { Finding, RoundRecord } from "../../src/core/types.ts";

function round(overrides: Partial<RoundRecord> = {}): RoundRecord {
  return {
    round: 1,
    base: "B0",
    lanes: ["a", "b"],
    candidates: [
      { lane: "a", sha: "C1a", ok: true },
      { lane: "b", sha: "C1b", ok: true },
    ],
    votes: [],
    ...overrides,
  };
}

function finding(overrides: Partial<Finding> = {}): Finding {
  return {
    id: "F-1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "src/core/rounds.ts:12 the pick is wrong",
    raisedBy: "B",
    status: "open",
    boundCandidateSha: "C1a",
    ...overrides,
  };
}

test("plan 06g: pickWinner takes two of three votes, picks a single passing candidate without a vote, and repeats when none passes", () => {
  // Two passing candidates: a strict majority (2 of 3) wins.
  const twoPassing = round({
    votes: [
      { seat: "M", lane: "b", why: "b is simpler" },
      { seat: "A", lane: "b", why: "b keeps the lock short" },
      { seat: "B", lane: "a", why: "a names the edge path" },
    ],
  });
  assert.deepEqual(pickWinner(twoPassing, 3), { lane: "b", sha: "C1b", votes: 2 });

  // One passing candidate: it wins without any vote.
  const onePassing = round({
    candidates: [
      { lane: "a", sha: "C1a", ok: true },
      { lane: "b", note: "the lane crashed: no candidate" },
    ],
  });
  assert.deepEqual(pickWinner(onePassing, 3), { lane: "a", sha: "C1a", votes: 0 });

  // No passing candidate: no winner, so the round repeats from the same base.
  const nonePassing = round({
    candidates: [
      { lane: "a", sha: "C1a", ok: false },
      { lane: "b", sha: "C1b", ok: false },
    ],
  });
  assert.equal(pickWinner(nonePassing, 3), undefined);
});

test("plan 06g: with two passing candidates, two of three pick votes win and the winner is named with its vote count", () => {
  const r = round({
    votes: [
      { seat: "M", lane: "b", why: "b is simpler" },
      { seat: "A", lane: "b", why: "b has the smaller diff" },
      { seat: "B", lane: "a", why: "a names the edge path" },
    ],
  });
  assert.deepEqual(pickWinner(r, 3), { lane: "b", sha: "C1b", votes: 2 });
});

test("plan 06g: a single passing candidate wins without a vote", () => {
  const r = round({
    candidates: [
      { lane: "a", sha: "C1a", ok: true },
      { lane: "b", note: "the lane crashed: no candidate" },
    ],
  });
  assert.deepEqual(pickWinner(r, 3), { lane: "a", sha: "C1a", votes: 0 });
});

test("plan 06g: no passing candidate has no winner, so the round repeats", () => {
  const r = round({ candidates: [{ lane: "a", sha: "C1a", ok: false }, { lane: "b", sha: "C1b", ok: false }] });
  assert.equal(pickWinner(r, 3), undefined);
  assert.deepEqual(passingCandidates(r), []);
});

test("plan 06g: a split with no strict majority has no winner", () => {
  // A 1-1-1 split (three lanes, three seats) gives no lane a majority: the
  // pick is not a model's to break here.
  const r = round({
    lanes: ["a", "b", "c"],
    candidates: [
      { lane: "a", sha: "C1a", ok: true },
      { lane: "b", sha: "C1b", ok: true },
      { lane: "c", sha: "C1c", ok: true },
    ],
    votes: [
      { seat: "M", lane: "a", why: "x" },
      { seat: "A", lane: "b", why: "y" },
      { seat: "B", lane: "c", why: "z" },
    ],
  });
  assert.equal(pickWinner(r, 3), undefined);
});

test("plan 06g: a vote for a candidate that failed checks is not counted", () => {
  const r = round({
    candidates: [
      { lane: "a", sha: "C1a", ok: true },
      { lane: "b", sha: "C1b", ok: false },
    ],
    votes: [
      { seat: "M", lane: "b", why: "wrong lane" },
      { seat: "A", lane: "b", why: "wrong lane" },
    ],
  });
  // Only lane a passes, so it wins without a vote; the votes for the failed
  // candidate are ignored.
  assert.deepEqual(pickWinner(r, 3), { lane: "a", sha: "C1a", votes: 0 });
});

test("plan 06g: round 1's every blocking finding blocks, exactly as before", () => {
  const gate = { itemIds: ["R1"], regressions: [] };
  assert.equal(blocksAcceptance(finding({ evidence: "wording is unclear" }), 1, gate), true);
});

test("plan 06g: a round-2 finding naming an unmet item with its id and file:line blocks", () => {
  const gate = { itemIds: ["R3", "C1"], regressions: [] };
  const f = finding({ itemId: "R3", evidence: "R3 is unmet: the two lanes never pick, see src/core/rounds.ts:88" });
  assert.equal(blocksAcceptance(f, 2, gate), true);
  // The same item named without a file:line is an advisory.
  const vague = finding({ itemId: "R3", evidence: "R3 is still unmet in the pick path" });
  assert.equal(blocksAcceptance(vague, 2, gate), false);
  assert.match(advisoryReason(vague, 2, gate), /cites no file:line/);
});

test("plan 06g: a round-2 finding naming a test that passed at the round base and fails now blocks", () => {
  const gate = { itemIds: [], regressions: ["plan 06g: a lane's sweep never signals another lane's process"] };
  const f = finding({ evidence: "the test 'plan 06g: a lane's sweep never signals another lane's process' fails now" });
  assert.equal(blocksAcceptance(f, 2, gate), true);
});

test("plan 06g: a round-2 finding that names no contract item and no regression is an advisory, never blocking", () => {
  const gate = { itemIds: ["R1", "C1"], regressions: ["some other test"] };
  const f = finding({ evidence: "a further edge path: the third lane's sweep is not scoped (hardening)" });
  assert.equal(blocksAcceptance(f, 2, gate), false);
  assert.match(advisoryReason(f, 2, gate), /only an unmet named item .* or a regression blocks/);
  // An advisory-severity finding never blocks, in any round.
  assert.equal(blocksAcceptance(finding({ severity: "advisory", evidence: "R1 unmet src/x.ts:1" }), 2, gate), false);
});

test("plan 06g: an owner correction's id is a named item too", () => {
  const gate = { itemIds: ["OD-2", "R1"], regressions: [] };
  const f = finding({ evidence: "OD-2 is unmet at src/conductor.ts:42" });
  assert.equal(blocksAcceptance(f, 3, gate), true);
});

test("plan 06g: roundBudget is the only place that decides the budget — 3 by default, #+TT_ROUNDS overrides it", () => {
  assert.equal(roundBudget(undefined), DEFAULT_ROUND_BUDGET);
  assert.equal(roundBudget({}), DEFAULT_ROUND_BUDGET);
  assert.equal(roundBudget({ roundsAllowed: 5 }), 5);
  // A nonsense value falls back to the default rather than to an infinite
  // loop.
  assert.equal(roundBudget({ roundsAllowed: 0 }), DEFAULT_ROUND_BUDGET);
  assert.equal(roundBudget({ roundsAllowed: Number.NaN }), DEFAULT_ROUND_BUDGET);
});

test("plan 06g: candidates are named C<round>-<lane> and the lane ids are a, b, …", () => {
  assert.deepEqual(laneIds(2), ["a", "b"]);
  assert.deepEqual(laneIds(1), ["a"]);
  assert.equal(candidateLabel(2, "b"), "C2-b");
});

test("plan 06g: the five round events validate against schemas/event.schema.json and reduce into phase.rounds", () => {
  const schema = JSON.parse(readFileSync(new URL("../../schemas/event.schema.json", import.meta.url), "utf8"));
  const events = [
    { type: "ROUND_STARTED", round: 2, base: "B1", lanes: ["a", "b"] },
    { type: "CANDIDATE_SUBMITTED", round: 2, lane: "a", sha: "C2a" },
    { type: "CANDIDATE_CHECKED", round: 2, lane: "a", ok: true },
    { type: "CANDIDATE_SUBMITTED", round: 2, lane: "b", sha: "C2b" },
    { type: "CANDIDATE_CHECKED", round: 2, lane: "b", ok: true },
    { type: "PICK_VOTE", round: 2, seat: "M", lane: "b", why: "b is simpler" },
    { type: "PICK_VOTE", round: 2, seat: "A", lane: "b", why: "b keeps the lock short" },
    { type: "PICK_VOTE", round: 2, seat: "B", lane: "a", why: "a names the edge path" },
    { type: "CANDIDATE_PICKED", round: 2, lane: "b", sha: "C2b", votes: 2 },
  ];
  for (const event of events) {
    const result = validate(schema, event);
    assert.equal(result.valid, true, `${event.type}: ${result.errors.join("; ")}`);
  }

  // Record-only: the phase never moves, and the round is rebuilt from the log.
  let state = baseState({ phase: "REVIEWING", candidate: { sha: "C2b", contractVersion: CV() } });
  for (const event of events) {
    const result = reduce(state, event);
    assert.equal(result.ok, true, !result.ok ? result.reason : "");
    assert.equal(result.state.phase.phase, "REVIEWING", "a round event moves no phase state");
    state = result.state;
  }
  assert.deepEqual(state.phase.rounds, [
    {
      round: 2,
      base: "B1",
      lanes: ["a", "b"],
      candidates: [
        { lane: "a", sha: "C2a", ok: true },
        { lane: "b", sha: "C2b", ok: true },
      ],
      votes: [
        { seat: "M", lane: "b", why: "b is simpler" },
        { seat: "A", lane: "b", why: "b keeps the lock short" },
        { seat: "B", lane: "a", why: "a names the edge path" },
      ],
      picked: { lane: "b", sha: "C2b", votes: 2 },
    },
  ]);
  assert.deepEqual(pickWinner(state.phase.rounds![0], 3), { lane: "b", sha: "C2b", votes: 2 }, "two of three votes pick lane b");
});

test("plan 06g: a round event for a round that never started is rejected, so a stray event cannot invent a round", () => {
  const state = baseState({ phase: "REVIEWING" });
  const result = reduce(state, { type: "CANDIDATE_SUBMITTED", round: 1, lane: "a", sha: "C1" });
  assert.equal(result.ok, false);
  assert.match(result.ok ? "" : result.reason, /round 1, which has not started/);
  // A lane the round does not have is rejected too.
  const started = reduce(state, { type: "ROUND_STARTED", round: 1, base: "B0", lanes: ["a", "b"] });
  assert.equal(started.ok, true);
  const badLane = reduce(started.state, { type: "CANDIDATE_SUBMITTED", round: 1, lane: "c", sha: "C1" });
  assert.equal(badLane.ok, false);
  assert.match(badLane.ok ? "" : badLane.reason, /not a lane of round 1/);
});

test("plan 06g: openAdvisories lists exactly the carried items — open, advisory findings", () => {
  const findings = [
    finding({ id: "F-1", severity: "advisory" }),
    finding({ id: "F-2", severity: "blocking" }),
    finding({ id: "F-3", severity: "advisory", status: "repaired" }),
  ];
  assert.deepEqual(openAdvisories(findings).map((f) => f.id), ["F-1"]);
});
