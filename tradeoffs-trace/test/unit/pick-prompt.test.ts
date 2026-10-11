// Plan 06g (A4/R5): the pick turn's prompt builders.
//
// Each seat votes for one candidate in a round's pick turn. Every seat gets
// every candidate's diff and checked-out tree and must say it inspected each
// one (settlement 01b: a seat that saw only lane a voted for lane a by
// default). The leader seat (M) also carries the settled ledger and every
// earlier round's reviews and votes; its vote counts once, exactly like the
// others'. This is a unit test on the builder itself, so the words the
// conductor sends are the words checked here.


import assert from "node:assert/strict";
import { test } from "node:test";

import { buildPickPrompt, checkInspected } from "../../src/conductor.ts";


const candidates = [
  { lane: "a", sha: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", label: "C2-a" },
  { lane: "b", sha: "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", label: "C2-b" },
];

const base = {
  phaseId: "p1",
  goal: "keep the round to two lanes",
  round: 2,
  base: "B1",
  candidates,
  seats: ["M", "A", "B"],
};

const ledger = ["D1 kept: batch cancels per tick", "D2 changed: one lock per lane"];
const earlierRounds = ["round 1: C1-a picked 2-1 (M, B); A voted C1-b", "round 1 review A on C1-a: advisory finding about wording"];
const otherDiffs = { a: "diff --git a/src/core/rounds.ts b/src/core/rounds.ts\n+export function pickWinner() {}", b: "" };

const checkouts = { a: "/runs/r1/candidates/aaaa", b: "/runs/r1/candidates/bbbb" };

test("plan 06g: M's pick prompt carries the ledger, earlier rounds, both diffs and both checkouts", () => {
  const prompt = buildPickPrompt({ ...base, seat: "M", leader: true, ledger, earlierRounds, otherDiffs, checkouts });
  assert.match(prompt, /round 1: C1-a picked 2-1/);
  assert.match(prompt, /diff --git a\/src\/core\/rounds\.ts/);
  assert.match(prompt, /batch cancels per tick/);
  assert.match(prompt, /C2-a/);
  assert.match(prompt, /C2-b/);
  assert.match(prompt, /lane a: \/runs\/r1\/candidates\/aaaa/);
  assert.match(prompt, /lane b: \/runs\/r1\/candidates\/bbbb/);
  assert.match(prompt, /Your vote counts once/);
});

test("A's and B's pick prompt has every candidate's diff and checkout, but not the leader's ledger or earlier rounds", () => {
  for (const seat of ["A", "B"] as const) {
    const prompt = buildPickPrompt({ ...base, seat, leader: false, otherDiffs, checkouts });
    // Both lanes are visible to every seat.
    assert.match(prompt, /diff --git a\/src\/core\/rounds\.ts/);
    assert.match(prompt, /Candidate C2-b's diff \(lane b\):\n\(no diff\)/);
    assert.match(prompt, /lane a: \/runs\/r1\/candidates\/aaaa/);
    assert.match(prompt, /lane b: \/runs\/r1\/candidates\/bbbb/);
    assert.match(prompt, /Compare EVERY candidate before you vote/);
    assert.match(prompt, /inspected: \["a", "b"\]/);
    // The leader-only context stays with the leader.
    assert.doesNotMatch(prompt, /round 1: C1-a picked/);
    assert.doesNotMatch(prompt, /batch cancels per tick/);
    assert.match(prompt, /strict majority \(2 of 3\)/);
    assert.match(prompt, new RegExp(`You are reviewer ${seat}`));
  }
});

test("a pick vote must name every passing lane in inspected", () => {
  assert.equal(checkInspected(["a", "b"], ["a", "b"]), undefined);
  assert.equal(checkInspected(["b", "a", "c"], ["a", "b"]), undefined, "extra lanes are harmless");
  assert.match(checkInspected(undefined, ["a", "b"]) ?? "", /needs inspected/);
  assert.match(checkInspected(["a"], ["a", "b"]) ?? "", /misses lane b/);
  assert.equal(checkInspected(undefined, ["a"]), undefined, "a single passing candidate is not voted on");
});
