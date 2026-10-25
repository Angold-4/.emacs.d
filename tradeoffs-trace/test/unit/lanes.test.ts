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
  fixLane?: LaneHost["fixLane"];
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
  /** 09-2: how long one review takes; 0 records it at once. */
  reviewDelayMs = 0;
  /** 09-2: every review dispatch, check start/end and cancel, in order. */
  timeline: string[] = [];
  /** 09-2: the `<lane>@<sha>` review sets the round cancelled. */
  cancels: string[] = [];
  /** 09-2: a cancelled review throws once its delay is over, like a
   * terminated review agent; set to undefined for a host that cannot cancel. */
  cancelReviews?: LaneHost["cancelReviews"] = (_round: number, lane: string, sha: string) => {
    this.timeline.push(`cancel-${lane}@${sha}`);
    this.cancels.push(`${lane}@${sha}`);
  };
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
    this.timeline.push(`check-start-${lane}`);
    await new Promise((resolve) => setTimeout(resolve, 5));
    this.checkLog.push(`end-${lane}`);
    this.timeline.push(`check-end-${lane}`);
    return this.lanes[lane] === "C1a" || this.lanes[lane] === "C1b" ? { ok: true } : { ok: false };
  }
  async reviewCandidate(round: number, lane: string, sha: string, seat: string): Promise<void> {
    this.timeline.push(`review-${lane}:${seat}@${sha}`);
    if (this.reviewDelayMs > 0) await new Promise((resolve) => setTimeout(resolve, this.reviewDelayMs));
    if (this.cancelReviews && this.cancels.includes(`${lane}@${sha}`)) throw new Error("the review was cancelled");
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
  // 09-2: the reviews land while the checks run, so only the non-review
  // events keep this order.
  assert.deepEqual(types(host).filter((t) => t !== "ROUND_REVIEW_SUBMITTED").slice(0, 5), [
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

  const nothing = new FakeHost({ a: { note: "crashed" }, b: { note: "no candidate" } });
  await runRound(nothing, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.equal(repeatLaneBases(nothing.rounds()[0]), undefined, "lanes with no candidate repeat from the same base");
  assert.equal(repeatLaneBases(undefined), undefined);
});

test("exex 01a2: a lane whose candidate is still red when its time runs out continues from that candidate next round", async () => {
  const host = new FakeHost({ a: "Xa", b: { note: "no candidate" } });
  await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.deepEqual(repeatLaneBases(host.rounds()[0]), { a: "Xa" }, "lane a keeps its own work; lane b, with none, starts from the base");
});

test("exex 01a2: a lane never hands in a red tree — a failed check goes back to the same lane, which fixes it and submits again", async () => {
  const host = new FakeHost({ a: "Xa", b: "C1b" });
  const fixes: string[] = [];
  host.fixLane = async (lane: string, _round: number, sha: string, failure: string) => {
    fixes.push(`${lane}:${sha}:${failure}`);
    host.lanes.a = "C1a"; // the fixed commit passes
    return { lane, sha: "C1a", disclosures: [] };
  };
  host.checkCandidate = async (_round: number, _lane: string, sha: string) =>
    sha.startsWith("C1") ? { ok: true } : { ok: false, note: "error[E0432]: unresolved import" };
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.deepEqual(fixes, ["a:Xa:error[E0432]: unresolved import"], "only the red lane is asked to fix, with its own failure");
  const a = host.rounds()[0].candidates.find((c) => c.lane === "a")!;
  assert.equal(a.sha, "C1a");
  assert.equal(a.ok, true, "the fixed commit is checked afresh; the red result never carries over");
  assert.equal(a.fixes, 1);
  // 09-2 (replaces "the red tree is never reviewed"): the red tree's reviews
  // started at its submission and were cancelled when its check failed; only
  // the fixed commit's reviews count.
  assert.deepEqual(host.cancels, ["a@Xa"]);
  assert.deepEqual(a.reviews?.map((r) => r.review.candidateSha), ["C1a", "C1a", "C1a"]);
  assert.ok(outcome.winner);
});

test("exex 01a2: a lane out of time keeps its red candidate, unreviewed, and the round goes on with the other lane", async () => {
  const host = new FakeHost({ a: "Xa", b: "C1b" });
  host.fixLane = async (lane: string) => ({ lane, note: "the lane's candidate is red and its attempt time is spent" });
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  const a = host.rounds()[0].candidates.find((c) => c.lane === "a")!;
  assert.equal(a.sha, "Xa");
  assert.equal(a.ok, false);
  // 09-2 (replaces "a red candidate is never reviewed"): its reviews were
  // cancelled at the failed check and none is recorded on it.
  assert.deepEqual(host.cancels, ["a@Xa"]);
  assert.equal(a.reviews, undefined, "a red candidate's reviews never count");
  assert.equal(outcome.winner?.lane, "b");
});

test("exex 01a2: a fix that hands back the same commit ends the loop instead of re-checking it forever", async () => {
  const host = new FakeHost({ a: "Xa", b: "C1b" });
  let calls = 0;
  host.fixLane = async (lane: string, _round: number, sha: string) => {
    calls += 1;
    return { lane, sha, disclosures: [] };
  };
  await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.equal(calls, 1);
});

// tt 09-2 (async review): reviews start at the candidate's submission, in
// parallel with its checks; a failed check cancels them.
test("09-2: a candidate's reviews start when its lane submits, before its check resolves", async () => {
  const host = new FakeHost({ a: "C1a", b: { note: "no candidate" } });
  let release!: () => void;
  const gate = new Promise<void>((resolve) => (release = resolve));
  host.checkCandidate = async (_round: number, lane: string) => {
    host.timeline.push(`check-start-${lane}`);
    await gate;
    host.timeline.push(`check-end-${lane}`);
    return { ok: true };
  };
  host.reviewDelayMs = 1;
  const running = runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  // The check is held open; every seat's review is already dispatched.
  await new Promise((resolve) => setTimeout(resolve, 20));
  assert.deepEqual(host.timeline.filter((t) => t.startsWith("review-")).sort(), ["review-a:A@C1a", "review-a:B@C1a", "review-a:M@C1a"]);
  assert.ok(!host.timeline.includes("check-end-a"), "the check has not finished");
  assert.equal(host.rounds()[0].candidates.find((c) => c.lane === "a")?.reviews?.length, 3, "the reviews landed while the check ran");
  release();
  const outcome = await running;
  assert.deepEqual(outcome.winner, { lane: "a", sha: "C1a", votes: 0 });
  assert.equal(outcome.failure, undefined);
});

test("09-2: a failed check cancels its reviews, they are not counted, and the fixed candidate gets fresh reviews", async () => {
  const host = new FakeHost({ a: "Xa", b: "C1b" });
  // Reviews outlast the 5 ms check, so the red candidate's are in flight
  // when its check fails.
  host.reviewDelayMs = 20;
  host.fixLane = async (lane: string) => ({ lane, sha: "C1a", disclosures: [] });
  host.checkCandidate = async (_round: number, lane: string, sha: string) => {
    host.timeline.push(`check-start-${lane}@${sha}`);
    await new Promise((resolve) => setTimeout(resolve, 5));
    host.timeline.push(`check-end-${lane}@${sha}`);
    return sha.startsWith("C1") ? { ok: true } : { ok: false, note: "error[E0432]: unresolved import" };
  };
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.deepEqual(host.cancels, ["a@Xa"], "only the red candidate's reviews are cancelled");
  const t = host.timeline;
  assert.ok(t.indexOf("review-a:M@Xa") < t.indexOf("check-end-a@Xa"), "the red candidate was under review while its check ran");
  assert.ok(t.indexOf("cancel-a@Xa") < t.indexOf("review-a:M@C1a"), "cancelled before the fixed candidate's reviews start");
  assert.ok(t.indexOf("review-a:M@C1a") < t.indexOf("check-end-a@C1a"), "the fixed candidate is reviewed from its own submission");
  assert.equal(host.events.filter((e) => e.type === "ROUND_REVIEW_SUBMITTED" && e.review.candidateSha === "Xa").length, 0);
  const a = host.rounds()[0].candidates.find((c) => c.lane === "a")!;
  assert.deepEqual(a.reviews?.map((r) => `${r.seat}@${r.review.candidateSha}`).sort(), ["A@C1a", "B@C1a", "M@C1a"]);
  assert.equal(outcome.failure, undefined);
  assert.ok(outcome.winner, "both lanes complete their K x N reviews and the round picks");
});

test("09-2: reviews a host could not cancel still never count — on the red candidate or on the lane's next one", async () => {
  for (const fixDelayMs of [40, 0]) {
    // 40: the stale reviews land while the lane is still red (the reducer
    // ignores them). 0: they land after the fixed candidate was submitted
    // (they name the old sha and never attach to the new one).
    const host = new FakeHost({ a: "Xa", b: "C1b" });
    host.cancelReviews = undefined;
    host.reviewDelayMs = 20;
    let fixedOnce = false;
    host.fixLane = async (lane: string) => {
      await new Promise((resolve) => setTimeout(resolve, fixDelayMs));
      fixedOnce = true;
      return { lane, sha: "C1a", disclosures: [] };
    };
    host.checkCandidate = async (_round: number, _lane: string, sha: string) => {
      await new Promise((resolve) => setTimeout(resolve, 5));
      return sha.startsWith("C1") ? { ok: true } : { ok: false, note: "red" };
    };
    const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
    assert.ok(fixedOnce);
    const a = host.rounds()[0].candidates.find((c) => c.lane === "a")!;
    assert.deepEqual(a.reviews?.map((r) => r.review.candidateSha), ["C1a", "C1a", "C1a"], `fix delay ${fixDelayMs}`);
    assert.ok(outcome.winner, `fix delay ${fixDelayMs}`);
  }
});

test("09-2: the pick waits until every passing candidate's reviews are complete", async () => {
  const host = new FakeHost({ a: "C1a", b: "C1b" });
  host.reviewDelayMs = 30;
  const outcome = await runRound(host, { round: 1, base: "B0", lanes: ["a", "b"] });
  const order = types(host);
  const lastReview = order.lastIndexOf("ROUND_REVIEW_SUBMITTED");
  assert.equal(order.filter((t) => t === "ROUND_REVIEW_SUBMITTED").length, 6);
  assert.ok(lastReview < order.indexOf("PICK_VOTE"), "no seat votes before all six reviews are in");
  assert.ok(order.lastIndexOf("CANDIDATE_CHECKED") < lastReview, "the reviews outlasted the checks and were still waited for");
  assert.equal(outcome.winner?.lane, "b");

  // A missing review still fails the round (today's K x N rule).
  const silent = new FakeHost({ a: "C1a", b: "C1b" });
  silent.reviewDelayMs = 30;
  silent.silentReviewSeat = "A";
  const failed = await runRound(silent, { round: 1, base: "B0", lanes: ["a", "b"] });
  assert.match(failed.failure ?? "", /reviewed by 2 of 3 seats/);
  assert.equal(failed.winner, undefined);
});

test("09-2: the reducer drops a red candidate's reviews and ignores one that lands after its check failed", () => {
  const review = (seat: string, sha: string) => ({
    reviewer: seat as "M",
    phaseId: "p1",
    candidateSha: sha,
    contractVersion: CV(),
    correctionStatements: [],
    findingStatements: [],
  });
  const apply = (state: ReturnType<typeof baseState>, event: Event) => {
    const r = reduce(state, event);
    assert.ok(r.ok, (r as { reason?: string }).reason);
    return r.state;
  };
  let state = baseState({ phase: "IMPLEMENTING" });
  state = apply(state, { type: "ROUND_STARTED", round: 1, base: "B0", lanes: ["a", "b"] });
  state = apply(state, { type: "CANDIDATE_SUBMITTED", round: 1, lane: "a", sha: "Xa" });
  // A review that lands while the check runs is recorded...
  state = apply(state, { type: "ROUND_REVIEW_SUBMITTED", round: 1, lane: "a", seat: "M", review: review("M", "Xa") });
  assert.equal(state.phase.rounds![0].candidates[0].reviews?.length, 1);
  // ...and dropped when the check fails.
  state = apply(state, { type: "CANDIDATE_CHECKED", round: 1, lane: "a", ok: false, note: "red" });
  assert.equal(state.phase.rounds![0].candidates[0].reviews, undefined);
  // A stale review that lands on the red candidate is ignored: accepted,
  // and the state is unchanged (so the log replays it as a no-op).
  const late = reduce(state, { type: "ROUND_REVIEW_SUBMITTED", round: 1, lane: "a", seat: "A", review: review("A", "Xa") });
  assert.ok(late.ok);
  assert.equal(late.state, state);
  // Once the lane submitted its fix, a review naming the old sha never
  // attaches to the new candidate.
  state = apply(state, { type: "CANDIDATE_SUBMITTED", round: 1, lane: "a", sha: "C1a" });
  const stale = reduce(state, { type: "ROUND_REVIEW_SUBMITTED", round: 1, lane: "a", seat: "B", review: review("B", "Xa") });
  assert.equal(stale.ok, false);
  assert.equal(stale.state.phase.rounds![0].candidates[0].reviews, undefined);
  // A passing check keeps the reviews that landed while it ran.
  state = apply(state, { type: "ROUND_REVIEW_SUBMITTED", round: 1, lane: "a", seat: "M", review: review("M", "C1a") });
  state = apply(state, { type: "CANDIDATE_CHECKED", round: 1, lane: "a", ok: true });
  assert.deepEqual(state.phase.rounds![0].candidates[0].reviews?.map((r) => r.seat), ["M"]);
});
