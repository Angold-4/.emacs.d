// Plan 06g (A4/R5): the pick turn's prompt builders.
//
// Each seat votes for one candidate in a round's pick turn. The leader seat
// (M) carries the full context — the settled ledger, every earlier round's
// reviews and votes, and the other candidates' diffs — and its vote counts
// once, exactly like the others'. A and B carry none of that: they see the
// round's candidates and vote. This is a unit test on the builder itself, so
// the words the conductor sends are the words checked here.

import assert from "node:assert/strict";
import { test } from "node:test";

import { buildPickPrompt } from "../../src/conductor.ts";

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

test("plan 06g: M's pick prompt contains the other candidate's diff and earlier rounds' votes, and A's and B's do not", () => {
  const prompt = buildPickPrompt({ ...base, seat: "M", leader: true, ledger, earlierRounds, otherDiffs });
  assert.match(prompt, /round 1: C1-a picked 2-1/);
  assert.match(prompt, /diff --git a\/src\/core\/rounds\.ts/);
  assert.match(prompt, /batch cancels per tick/);
  assert.match(prompt, /C2-a/);
  assert.match(prompt, /C2-b/);
  assert.match(prompt, /Your vote counts once/);
});

test("plan 06g: A's and B's pick prompt is the round's candidates and the vote rule, nothing else", () => {
  for (const seat of ["A", "B"] as const) {
    const prompt = buildPickPrompt({ ...base, seat, leader: false });
    assert.doesNotMatch(prompt, /diff --git/);
    assert.doesNotMatch(prompt, /round 1: C1-a picked/);
    assert.doesNotMatch(prompt, /batch cancels per tick/);
    // They still see the round's candidates and the vote rule.
    assert.match(prompt, /C2-a/);
    assert.match(prompt, /C2-b/);
    assert.match(prompt, /strict majority \(2 of 3\)/);
    assert.match(prompt, new RegExp(`You are reviewer ${seat}`));
  }
});
