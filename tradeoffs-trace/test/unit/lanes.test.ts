// Plan 06g2 (A1): `runRound` is the round's only orchestrator. These tests
// drive it with a fake `LaneHost`, so the sequence it owns — one base, K
// lanes, each lane's own build, the checks one after the other, every seat's
// review of every passing candidate, the pick turn, and `pickWinner` — is
// checked without spawning anything.

import assert from "node:assert/strict";
import { test } from "node:test";

import { reduce } from "../../src/core/reduce.ts";
import { planModelSelector } from "../../src/core/roles.ts";
import { isLaneRound, laneFailureLines, lanesOfContract, repeatLaneBases, runRound, type LaneHost } from "../../src/core/lanes.ts";
import type { Event, RoundRecord } from "../../src/core/types.ts";
import { baseState, CV } from "./helpers.ts";

/** A host that records every event into a real reduced phase, so
 * `rounds()`/`pickWinner` see exactly what a live round would. */
class FakeHost implements LaneHost {
  readonly phaseId = "p1";
  readonly goal = "build two candidates";
  readonly seats = ["M", "A", "B"];
  events: Event[] = [];
  checkLog: string[] = [];
  reviews: string[] = [];
  #state = baseState({ phase: "IMPLEMENTING" });
  /** Lane -> the candidate it freezes, or a note when it produces none. */
  readonly lanes: Record<string, string | { note: string }>;
  /** A seat whose review silently records nothing (a lost review). */
  silentReviewSeat?: string;
  /** A seat whose pick turn records no vote. */
  silentVoteSeat?: string;
  constructor(lanes: Record<string, string | { note: string }>) {
    this.lanes = lanes;
  }
  rounds(): readonly RoundRecord[] {
    return this.#state.phase.rounds ?? [];
  }
  ledger(): readonly string[] {
    return [];
  }
  earlierRounds(): readonly string[] {
    return [];
  }
  createWorktree(lane: string): string {
    return `/tmp/lane-${lane}`;
  }
  lanePrompt(round: number, base: string): string {
    return `round ${round} from ${base}`;
  }
  async buildLane(lane: string): Promise<{ lane: string; sha?: string; note?: string; disclosures?: never[] }> {
    const result = this.lanes[lane];
    return typeof result === "string" ? { lane, sha: result, disclosures: [] } : { lane, note: result.note };
  }
  async checkCandidate(_round: number, lane: string): Promise<{ ok: boolean; note?: string }> {
    // A real await: if the round ran two checks concurrently, the log would
    // interleave start-a/start-b.
    this.checkLog.push(`start-${lane}`);
    await new Promise((resolve) => setTimeout(resolve, 5));
    this.checkLog.push(`end-${lane}`);
    return this.lanes[lane] === "C1a" || this.lanes[lane] === "C1b" ? { ok: true } : { ok: false };
  }
  async reviewCandidate(round: number, lane: string, sha: string, seat: string): Promise<void> {
    if (seat === this.silentReviewSeat) return;
    this.reviews.push(`${lane}:${seat}`);
    this.emit({
      type: "ROUND_REVIEW_SUBMITTED",
      round,
      lane,
      seat,
      review: {
        reviewer: seat as "M",
        phaseId: this.phaseId,
        candidateSha: sha,
        contractVersion: CV(),
        correctionStatements: [],
        findingStatements: [],
      },
    });
  }
  async pickTurn(round: number, _base: string, seat: string): Promise<void> {
    if (seat === this.silentVoteSeat) return;
    this.emit({ type: "PICK_VOTE", round, seat, lane: seat === "B" ? "a" : "b", why: `${seat} prefers` });
  }
  emit(event: Event): void {
    this.events.push(event);
    const result = reduce(this.#state, event);
    if (result.ok) this.#state = result.state;
  }
}

const types = (host: FakeHost): string[] => host.events.map((e) => e.type);

test("plan 06l: runRound starts each lane's check as that lane submits and reviews both lanes at once, then picks the winner", async () => {
  const host = new FakeHost({ a: "C1a", b: "C1b" });
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });

  // The emitted record: one round, two candidates, two checks, six reviews,
  // three votes, one winner.
  assert.deepEqual(types(host).slice(0, 5), [
    "ROUND_STARTED",
    "CANDIDATE_SUBMITTED",
    "CANDIDATE_SUBMITTED",
    "CANDIDATE_CHECKED",
    "CANDIDATE_CHECKED",
  ]);
  assert.equal(types(host).filter((t) => t === "ROUND_REVIEW_SUBMITTED").length, 6);
  assert.equal(types(host).filter((t) => t === "PICK_VOTE").length, 3);
  assert.equal(types(host).filter((t) => t === "CANDIDATE_PICKED").length, 1);
  // The three seats review a candidate in parallel (the discovery barrier
  // needs all three in flight), so only the SET of reviews is ordered.
  assert.deepEqual(host.reviews.slice().sort(), ["a:A", "a:B", "a:M", "b:A", "b:B", "b:M"]);

  // Plan 06l (A1): each lane's check starts the moment that lane submits, so
  // both checks are in flight before either ends. The conductor's
  // `checkCandidate` takes the machine-wide lock, which is what keeps the two
  // actual commands from overlapping (the e2e checkIntervals test).
  assert.deepEqual(host.checkLog, ["start-a", "start-b", "end-a", "end-b"]);

  // pickWinner (code, never a model) decided lane b with 2 of 3 votes.
  assert.deepEqual(outcome.winner, { lane: "b", sha: "C1b", votes: 2 });
  assert.deepEqual(host.rounds()[0].picked, { lane: "b", sha: "C1b", votes: 2 });
});

test("plan 06g: runRound records no candidate for a crashed lane, reviews the other, and picks it without a vote", async () => {
  const host = new FakeHost({ a: "C1a", b: { note: "the lane's worker ended without submit_phase (exited)" } });
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });

  assert.equal(types(host).filter((t) => t === "CANDIDATE_SUBMITTED").length, 1, "only lane a froze a candidate");
  const round = host.rounds()[0];
  const b = round.candidates.find((c) => c.lane === "b")!;
  assert.equal(b.sha, undefined, "the crashed lane records no candidate");
  assert.match(b.note ?? "", /without submit_phase/);
  assert.equal(b.ok, false);
  // Only lane a is reviewed, and it wins without a vote.
  assert.deepEqual(host.reviews.slice().sort(), ["a:A", "a:B", "a:M"]);
  assert.equal(types(host).filter((t) => t === "PICK_VOTE").length, 0);
  assert.deepEqual(outcome.winner, { lane: "a", sha: "C1a", votes: 0 });
});

test("plan 06g: runRound repeats when no candidate passes — no reviews, no votes, no winner", async () => {
  const host = new FakeHost({ a: { note: "crashed" }, b: { note: "timed out" } });
  const outcome = await runRound(host, { round: 2, base: "B1", lanes: ["a", "b"] });
  assert.equal(outcome.winner, undefined);
  assert.equal(types(host).filter((t) => t === "ROUND_REVIEW_SUBMITTED").length, 0);
  assert.equal(types(host).filter((t) => t === "CANDIDATE_PICKED").length, 0);
  assert.equal(host.rounds()[0].base, "B1");
});

test("plan 06g: runRound refuses to pick when a seat's review is missing, and the round fails", async () => {
  const host = new FakeHost({ a: "C1a", b: "C1b" });
  host.silentReviewSeat = "B";
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.equal(outcome.winner, undefined);
  assert.match(outcome.failure ?? "", /C1-a was reviewed by 2 of 3 seats/);
  assert.equal(types(host).filter((t) => t === "PICK_VOTE").length, 0, "no pick turn ran");
  assert.equal(types(host).filter((t) => t === "CANDIDATE_PICKED").length, 0);
});

test("plan 06g: runRound refuses to pick when a seat's vote is missing and the votes cast are split, and the round fails", async () => {
  // M is silent; A votes b and B votes a: no lane has a majority of all
  // three seats, so a majority would have to be invented.
  const host = new FakeHost({ a: "C1a", b: "C1b" });
  host.silentVoteSeat = "M";
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.equal(outcome.winner, undefined);
  assert.match(outcome.failure ?? "", /missing a vote from M/);
  assert.equal(types(host).filter((t) => t === "CANDIDATE_PICKED").length, 0);
});

test("02k: a missing pick vote that cannot change the result does not discard the round", async () => {
  // B is silent; M and A both vote b — two of three seats, a majority no
  // missing vote could overturn. The round picks b and names B as missing.
  const host = new FakeHost({ a: "C1a", b: "C1b" });
  host.silentVoteSeat = "B";
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.equal(outcome.failure, undefined);
  assert.equal(outcome.winner?.lane, "b");
  assert.equal(outcome.winner?.votes, 2);
  assert.deepEqual(outcome.missingVotes, ["B"]);
  assert.equal(types(host).filter((t) => t === "CANDIDATE_PICKED").length, 1);
});

test("plan 06g: runRound reports a review that throws as the round's failure", async () => {
  const host = new FakeHost({ a: "C1a", b: "C1b" });
  const original = host.reviewCandidate.bind(host);
  host.reviewCandidate = async (round, lane, sha, seat) => {
    if (seat === "A" && lane === "a") throw new Error("the reviewer did not start");
    return original(round, lane, sha, seat);
  };
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.equal(outcome.winner, undefined);
  assert.match(outcome.failure ?? "", /A's review of C1-a failed: the reviewer did not start/);
});

test("plan 06g: lanesOfContract and isLaneRound read the frozen contract's lane count", () => {
  assert.deepEqual(lanesOfContract({}), ["a"]);
  assert.deepEqual(lanesOfContract({ workers: 1 }), ["a"]);
  assert.deepEqual(lanesOfContract({ workers: 2 }), ["a", "b"]);
  assert.equal(isLaneRound({}), false);
  assert.equal(isLaneRound({ workers: 2 }), true);
});

test("plan 06g: worker.1 and worker.2 give each lane its own worker model, each falling back to worker", () => {
  const select = planModelSelector({
    models: {
      worker: { model: "shared" },
      workerLanes: { "1": { model: "lane-one" }, "2": { model: "lane-two" } },
    },
  });
  assert.equal(select("worker", "1")?.model, "lane-one");
  assert.equal(select("worker", "2")?.model, "lane-two");
  assert.equal(select("worker", "3")?.model, "shared", "a lane with no own model keeps the role's");
  assert.equal(select("worker")?.model, "shared");
  // The pick turn is a reviewer seat's turn, so it takes that seat's model.
  const seats = planModelSelector({ models: { reviewerSeats: { M: { model: "opus" } }, worker: { model: "flash" } } });
  assert.equal(seats("picker", "M")?.model, "opus");
});

test("plan 06g: laneFailureLines names every lane's own failure, so a repeated round carries both", () => {
  const round: RoundRecord = {
    round: 1,
    base: "B0",
    lanes: ["a", "b"],
    candidates: [
      { lane: "a", sha: "C1a", ok: false, note: "the check `node --test` exited 1" },
      { lane: "b", note: "the lane's worker ended without submit_phase (exited)" },
    ],
    votes: [],
  };
  const lines = laneFailureLines(round);
  assert.equal(lines.length, 2);
  assert.match(lines[0], /C1-a \(lane a\) failed its checks: the check/);
  assert.match(lines[1], /C1-b \(lane b\) produced no candidate: the lane's worker ended/);
  assert.deepEqual(laneFailureLines(undefined), []);
});

// 02k (2026-10-10): rounds 1 and 2 were discarded because one seat's lane
// review timed out, and both lanes then restarted from the phase base,
// rebuilding 50 minutes of work. A lane whose candidate passed now restarts
// from it.
test("02k: a round that could not complete restarts each passing lane from its own candidate", async () => {
  const first = new FakeHost({ a: "C1a", b: "C1b" });
  first.silentReviewSeat = "A";
  const failed = await runRound(first, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.ok(failed.failure, "a missing review fails the round");
  assert.deepEqual(repeatLaneBases(first.rounds()[0]), { a: "C1a", b: "C1b" });

  // runRound builds each lane from its own base when the context names one.
  const bases: Record<string, string> = {};
  const second = new FakeHost({ a: "C1a", b: "C1b" });
  second.buildLane = async (lane: string, _round: number, base: string) => {
    bases[lane] = base;
    return { lane, sha: lane === "a" ? "C1a" : "C1b", disclosures: [] };
  };
  await runRound(second, { round: 2, base: "B0", lanes: ["a", "b"], laneBases: { a: "C1a" } });
  assert.deepEqual(bases, { a: "C1a", b: "B0" }, "a lane without a passing candidate keeps the round's base");
});

test("02k: a round with a winner, or with no passing candidate, keeps the round's base for every lane", async () => {
  const picked = new FakeHost({ a: "C1a", b: "C1b" });
  await runRound(picked, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.ok(picked.rounds()[0].picked, "a complete round picks");
  assert.equal(repeatLaneBases(picked.rounds()[0]), undefined);

  const nonePassed = new FakeHost({ a: "Xa", b: { note: "no candidate" } });
  await runRound(nonePassed, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.equal(repeatLaneBases(nonePassed.rounds()[0]), undefined, "failed checks repeat from the same base (plan 06g)");
  assert.equal(repeatLaneBases(undefined), undefined);
});
