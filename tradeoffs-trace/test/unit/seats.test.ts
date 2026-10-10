// Plan 06h (A1/A2): the seat list, its leader and the vote rule. These are
// the only places that decide who reviews and how a majority is counted
// (`seatsOf`/`leaderOf` in core/seats.ts, `tally` in core/tally.ts). The
// default `M A B`, led by `M`, is exactly 06g2's, so every 06g test passes
// unchanged (C1).

import assert from "node:assert/strict";
import { test } from "node:test";

import { buildContract } from "../../src/conductor.ts";
import { accept } from "../../src/core/predicate.ts";
import { seatsOf, leaderOf, seatsRecordOf } from "../../src/core/seats.ts";
import { tally } from "../../src/core/tally.ts";
import type { Ballot, PhaseState } from "../../src/core/types.ts";
import { approvingReview, baseState, CV, makeBallot, makeDecision } from "./helpers.ts";

const K = CV();
const C = "C1";

test("plan 06h: with TT_REVIEWERS and TT_LEADER absent the seats are M, A, B led by M and every 06g test passes unchanged", () => {
  // The defaults are exactly 06g2's fixed list and leader.
  assert.deepEqual(seatsOf(undefined), ["M", "A", "B"]);
  assert.deepEqual(seatsOf({}), ["M", "A", "B"]);
  assert.equal(leaderOf(undefined), "M");
  assert.equal(leaderOf({}), "M");

  // A contract built from a plan with no new keywords carries no seats, so
  // `seatsOf` reads the default and every 06g reader is unchanged.
  const contract = buildContract({
    id: "p1",
    goal: "g",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
  } as unknown as import("../../src/conductor.ts").RunPlanPhase);
  assert.equal(contract.seats, undefined);
  assert.equal(contract.leader, undefined);
  assert.deepEqual(seatsOf(contract), ["M", "A", "B"]);
  assert.equal(leaderOf(contract), "M");

  // The default tally rule is the 06g master-veto two-of-three rule.
  const decision = makeDecision();
  const ballots = [makeBallot({ reviewer: "M", vote: "approve" }), makeBallot({ reviewer: "A", vote: "approve" }), makeBallot({ reviewer: "B", vote: "reject" })];
  assert.equal(tally(decision, ballots, [], C, K), "pass");
  assert.equal(tally(decision, [makeBallot({ reviewer: "M", vote: "reject" }), makeBallot({ reviewer: "A", vote: "approve" }), makeBallot({ reviewer: "B", vote: "approve" })], [], C, K), "fail");
});

test("plan 06h: seatsOf reads #+TT_REVIEWERS and leaderOf reads #+TT_LEADER, falling back safely", () => {
  assert.deepEqual(seatsOf({ seats: ["M", "A", "B", "C", "D"] }), ["M", "A", "B", "C", "D"]);
  assert.equal(leaderOf({ seats: ["M", "A", "B", "C", "D"], leader: "C" }), "C");
  // An unknown leader falls back to the first seat, never a model's choice.
  assert.equal(leaderOf({ seats: ["M", "A", "B"], leader: "X" }), "M");
  // An empty or malformed list is the default.
  assert.deepEqual(seatsOf({ seats: [] }), ["M", "A", "B"]);
  assert.deepEqual(seatsOf({ seats: ["", "  "] }), ["M", "A", "B"]);
  assert.deepEqual(seatsRecordOf({ seats: ["M", "A", "B", "C", "D"], leader: "B", workers: 4 }), {
    seats: ["M", "A", "B", "C", "D"],
    leader: "B",
    workers: 4,
  });
});

test("plan 06h: a strict majority of five is three, and the leader's approval is still required", () => {
  const seats = ["M", "A", "B", "C", "D"];
  const decision = makeDecision();
  const ballot = (seat: string, vote: "approve" | "reject"): Ballot => makeBallot({ reviewer: seat, vote });
  // Leader plus two others (3 of 5) passes.
  assert.equal(
    tally(decision, [ballot("M", "approve"), ballot("A", "approve"), ballot("B", "approve"), ballot("C", "reject"), ballot("D", "reject")], [], C, K, seats),
    "pass",
  );
  // Two of five is not a majority.
  assert.equal(
    tally(decision, [ballot("M", "approve"), ballot("A", "approve"), ballot("B", "reject"), ballot("C", "reject"), ballot("D", "reject")], [], C, K, seats),
    "fail",
  );
  // Four non-leader approvals without the leader still fail: the leader's
  // approval (the veto) is required.
  assert.equal(
    tally(decision, [ballot("M", "reject"), ballot("A", "approve"), ballot("B", "approve"), ballot("C", "approve"), ballot("D", "approve")], [], C, K, seats),
    "fail",
  );
});

test("plan 06h: accept() reads every configured seat's review, not a fixed three", () => {
  const seats = ["M", "A", "B", "C", "D"];
  const phase = (): PhaseState => {
    const state = baseState({
      phase: "RESOLVING",
      candidate: { sha: C, contractVersion: K },
      integrationHead: "H0",
      checks: { candidateSha: C, passed: true },
      probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
      reviews: Object.fromEntries(seats.map((s) => [s, { review: approvingReview(s, C, K) }])),
      decisions: [],
    });
    // The contract is what carries the configured seats (`seatsOf`).
    state.phase.contract = { ...state.phase.contract, seats: [...seats], leader: "M" };
    return state.phase;
  };
  // With all five reviews, acceptance depends only on the (empty) decisions.
  assert.equal(accept(phase(), C, K), true);
  // Drop one seat's review: acceptance fails, because all five are required.
  const four = phase();
  delete (four.reviews as Record<string, unknown>).D;
  assert.equal(accept(four, C, K), false);
});
