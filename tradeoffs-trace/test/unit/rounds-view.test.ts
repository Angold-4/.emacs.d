// Plan 06g (A5): the views of a round — both lanes, the per-candidate
// grouping, the pick votes and the winner — in the status buffer, the tape,
// the review buffer and `tt summary`. These are the pure renderers behind
// them; the Emacs review buffer's own grouping is covered by an ERT test in
// test/tradeoffs-trace-test.el.

import assert from "node:assert/strict";
import { test } from "node:test";

import { renderRoundsOrg } from "../../src/render.ts";
import { renderLoopTape } from "../../src/charts.ts";
import { lanesView, roundsSection } from "../../src/view.ts";
import type { PhaseState, RoundRecord } from "../../src/core/types.ts";
import { basePhase, CV } from "./helpers.ts";

const K = CV();

function rounds(): RoundRecord[] {
  return [
    {
      round: 1,
      base: "B0",
      lanes: ["a", "b"],
      candidates: [
        { lane: "a", sha: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", ok: false },
        { lane: "b", note: "the lane crashed: no candidate" },
      ],
      votes: [],
    },
    {
      round: 2,
      base: "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      lanes: ["a", "b"],
      candidates: [
        { lane: "a", sha: "cccccccccccccccccccccccccccccccccccccccc", ok: true },
        { lane: "b", sha: "dddddddddddddddddddddddddddddddddddddddd", ok: true },
      ],
      votes: [
        { seat: "M", lane: "b", why: "b is the smaller diff" },
        { seat: "A", lane: "b", why: "b keeps the lock short" },
        { seat: "B", lane: "a", why: "a names the edge path" },
      ],
      picked: { lane: "b", sha: "dddddddddddddddddddddddddddddddddddddddd", votes: 2 },
    },
  ];
}

function phase(overrides: Partial<PhaseState> = {}): PhaseState {
  return basePhase({ rounds: rounds(), ...overrides });
}

test("plan 06g: the status shows one line per lane and the round's cost", () => {
  const lines = lanesView(phase());
  assert.deepEqual(lines[0], "a C2-a ccccccc checks ✓");
  assert.deepEqual(lines[1], "b C2-b ddddddd checks ✓ — picked (2 votes)");
  assert.match(lines[2], /round 2 from bbbbbbb: 2 worker run\(s\), 6 review\(s\)/);
  assert.match(lines[3], /votes: M→b · A→b · B→a/);
});

test("plan 06g: a lane with no candidate is shown with its failure, and a failed check is shown as one", () => {
  const first = lanesView(phase({ rounds: [rounds()[0]] }));
  assert.deepEqual(first[0], "a C1-a aaaaaaa checks ✗");
  assert.equal(first[1], "b (no candidate: the lane crashed: no candidate)");
  assert.match(first[2], /round 1 from B0: 2 worker run\(s\), 0 review\(s\)/);
  // A plan without `#+TT_WORKERS` records no round, so the view is unchanged.
  assert.deepEqual(lanesView(phase({ rounds: undefined })), []);
});

test("plan 06g: the loop tape shows both lanes and the pick", () => {
  const tape = renderLoopTape({
    label: "06g two lanes",
    round: 2,
    lanes: lanesView(phase()),
    phases: [
      { phase: "IMPLEMENTING", at: "2026-10-07T00:00:00.000Z" },
      { phase: "REVIEWING", at: "2026-10-07T00:01:00.000Z" },
    ],
    phase: {
      phase: "REVIEWING",
      attempt: { n: 2 },
      repairRoundsUsed: 1,
      repairRoundsGranted: 3,
      contract: {},
    },
    endMs: Date.parse("2026-10-07T00:02:00.000Z"),
  });
  assert.match(tape, /lanes {8}a C2-a ccccccc checks ✓/);
  assert.match(tape, /lanes {8}b C2-b ddddddd checks ✓ — picked \(2 votes\)/);
  assert.match(tape, /round 2 from bbbbbbb: 2 worker run\(s\), 6 review\(s\)/);
});

test("plan 06g: the review buffer groups reviews by candidate, with the votes and the winner", () => {
  const org = renderRoundsOrg(rounds());
  assert.match(org, /\* Rounds/);
  assert.match(org, /\*\* Round 2 from bbbbbbb/);
  assert.match(org, /\*\*\* C2-a \(lane a\) — checks passed/);
  assert.match(org, /\*\*\* C2-b \(lane b\) — checks passed, picked/);
  assert.match(org, /- M → C2-b: b is the smaller diff/);
  assert.match(org, /Winner: C2-b \(ddddddd, 2 votes\)/);
  // A lane that produced no candidate is named with why, not dropped.
  assert.match(org, /\*\*\* C1-b \(lane b\) — no candidate: the lane crashed/);
  assert.match(org, /Winner: none/);
});

test("plan 06g: status, loop chart, tape and tt summary show both lanes, per-candidate reviews, the votes and the winner", () => {
  // A round whose two passing candidates each drew three reviews: the views
  // show both lanes, each candidate's own reviews, the three votes and the
  // winner — in the status, the tape, the review buffer and `tt summary`.
  const withReviews = rounds();
  withReviews[1] = {
    ...withReviews[1],
    candidates: withReviews[1].candidates.map((c) => ({
      ...c,
      reviews: ["M", "A", "B"].map((seat) => ({
        seat,
        review: {
          reviewer: seat as "M",
          phaseId: "p1",
          candidateSha: c.sha!,
          contractVersion: K,
          correctionStatements: [],
          findingStatements: [],
        },
      })),
    })),
  };
  const p = phase({ rounds: withReviews });

  // Status: one line per lane, naming the seats that reviewed it.
  const lines = lanesView(p);
  assert.match(lines[0], /^a C2-a ccccccc checks ✓ — reviewed by A, B, M$/);
  assert.match(lines[1], /^b C2-b ddddddd checks ✓ — reviewed by A, B, M — picked \(2 votes\)$/);

  // Loop chart / tape: the same lane lines.
  const tape = renderLoopTape({
    label: "06g two lanes",
    round: 2,
    lanes: lanesView(p),
    phases: [{ phase: "IMPLEMENTING", at: "2026-10-07T00:00:00.000Z" }],
    phase: { phase: "REVIEWING", attempt: { n: 2 }, repairRoundsUsed: 1, repairRoundsGranted: 3, contract: {} },
    endMs: Date.parse("2026-10-07T00:02:00.000Z"),
  });
  assert.match(tape, /a C2-a ccccccc checks ✓ — reviewed by A, B, M/);
  assert.match(tape, /b C2-b ddddddd checks ✓ — reviewed by A, B, M — picked \(2 votes\)/);

  // Review buffer: grouped by candidate, with its reviews, the votes and the winner.
  const org = renderRoundsOrg(withReviews);
  assert.match(org, /\*\*\* C2-a \(lane a\) — checks passed/);
  assert.match(org, /    reviews: 3/);
  assert.match(org, /      - M: no findings/);
  assert.match(org, /- M → C2-b: b is the smaller diff/);
  assert.match(org, /Winner: C2-b \(ddddddd, 2 votes\)/);

  // tt summary: per-candidate reviews beside the votes and the winner.
  const section = roundsSection(p).join("\n");
  assert.match(section, /reviews — C2-a: A\/B\/M · C2-b: A\/B\/M/);
  assert.match(section, /votes M→b A→b B→a/);
  assert.match(section, /winner C2-b \(ddddddd, 2 votes\)/);
});

test("plan 06h: status, loop chart and tape list five seats and four lanes", () => {
  const seats = ["M", "A", "B", "C", "D"];
  const K5 = CV();
  const round: RoundRecord = {
    round: 1,
    base: "B0",
    lanes: ["a", "b", "c", "d"],
    candidates: ["a", "b", "c", "d"].map((lane, i) => ({
      lane,
      sha: String(i + 1).repeat(40),
      ok: true,
      reviews: seats.map((seat) => ({
        seat,
        review: { reviewer: seat, phaseId: "p1", candidateSha: String(i + 1).repeat(40), contractVersion: K5, correctionStatements: [], findingStatements: [] },
      })),
    })),
    votes: seats.map((seat, i) => ({ seat, lane: i < 3 ? "a" : "b", why: `${seat} picks` })),
    picked: { lane: "a", sha: "1".repeat(40), votes: 3 },
  };
  const p = basePhase({ rounds: [round], contract: { ...basePhase().contract, seats: [...seats], leader: "M" } });

  // Status: one line per lane (four), each naming all five seats, and the
  // round's cost is four candidates × five seats = 20 reviews.
  const lines = lanesView(p);
  assert.equal(lines.length, 6, "four lanes, the cost line and the votes line");
  assert.match(lines[0], /^a C1-a 1111111 checks ✓ — reviewed by A, B, C, D, M — picked \(3 votes\)$/);
  assert.match(lines[3], /^d C1-d 4444444 checks ✓ — reviewed by A, B, C, D, M$/);
  assert.match(lines[4], /round 1 from B0: 4 worker run\(s\), 20 review\(s\)/);
  assert.match(lines[5], /votes: M→a · A→a · B→a · C→b · D→b/);

  // Loop tape / chart: the same four lanes and the five seats' reviews.
  const tape = renderLoopTape({
    label: "06h four lanes",
    round: 1,
    lanes: lanesView(p),
    phases: [{ phase: "IMPLEMENTING", at: "2026-10-08T00:00:00.000Z" }],
    phase: { phase: "REVIEWING", attempt: { n: 1 }, repairRoundsUsed: 0, repairRoundsGranted: 3, contract: { seats } },
    endMs: Date.parse("2026-10-08T00:02:00.000Z"),
  });
  assert.match(tape, /a C1-a 1111111 checks ✓ — reviewed by A, B, C, D, M/);
  assert.match(tape, /d C1-d 4444444 checks ✓ — reviewed by A, B, C, D, M/);
  assert.match(tape, /4 worker run\(s\), 20 review\(s\)/);

  // Review buffer: each candidate shows all five seats' reviews.
  const org = renderRoundsOrg([round]);
  assert.match(org, /reviews: 5/);
  assert.match(org, /- D: no findings/);
  assert.match(org, /- M → C1-a: M picks/);
  assert.match(org, /Winner: C1-a \(1111111, 3 votes\)/);
});

test("plan 06g: tt summary lists each round's candidates, votes and winner", () => {
  const section = roundsSection(phase()).join("\n");
  assert.match(section, /### Rounds \(2\)/);
  assert.match(section, /Round 1 from B0: C1-a aaaaaaa checks ✗ · C1-b no candidate \(the lane crashed: no candidate\) — no vote — no winner/);
  assert.match(section, /Round 2 from bbbbbbb: C2-a ccccccc checks ✓ · C2-b ddddddd checks ✓ — votes M→b A→b B→a — winner C2-b \(ddddddd, 2 votes\)/);
  // A plan without lanes records no round: no section at all.
  assert.deepEqual(roundsSection(phase({ rounds: undefined })), []);
});
