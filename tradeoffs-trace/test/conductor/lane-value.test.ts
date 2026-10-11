// Plan 06k1 (A1): the pick vote's `loserHad`, and the pick shape in the
// metrics. Two tests:
//   1. a two-lane round whose pick vote is first submitted without
//      `loserHad` is re-asked once, and a majority "yes" carries the losing
//      lane's anchors into the next round's repair prompt;
//   2. `tt summary`'s metrics report split vs unanimous picks and the rounds
//      where the loser had something, and an old log shows "not recorded".

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { computeMetrics, metricsLine, metricsSummary, type MetricEvent } from "../../src/metrics.ts";
import { loserAnchors, pickStats } from "../../src/core/rounds.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { basePhase } from "../unit/helpers.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 20_000,
  checkMs: 15_000,
  probeMs: 15_000,
  freezeMs: 15_000,
  evaluateMs: 8_000,
  reviewMs: 10_000,
  panelMs: 8_000,
};

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

function lanePhase(overrides: Partial<import("../../src/conductor.ts").RunPlanPhase> = {}) {
  return {
    id: "p1",
    goal: "build two candidates from one base",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers: 2,
    ...overrides,
  };
}

function laneWorker(lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane}\n' > lane-${lane}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

function laneReview(
  seat: Reviewer,
  contractVersion: unknown,
  opts: { findings?: unknown[]; findingStatements?: unknown[] } = {},
): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
          phaseId: "p1",
          candidateSha: "$TT_CANDIDATE_SHA",
          contractVersion,
          correctionStatements: [],
          findingStatements: opts.findingStatements ?? [],
          ballots: [],
          findings: opts.findings ?? [],
        },
      },
    ],
  };
}

function liveRound(state: State): number {
  return state.phase.rounds?.length ?? 1;
}

/** One seat's pick vote. `loserHad` is omitted for the first attempt when
 * `askAgain` is set, so the conductor must re-ask once. */
function pickVote(
  seat: Reviewer,
  round: number,
  lane: string,
  loserHad: { yes: boolean; anchors: string[]; note?: string } | undefined,
  askAgain = false,
): { hello: unknown; steps: FakePiStep[] } {
  const args: Record<string, unknown> = { round, seat, lane, why: `${seat} picks ${lane}` };
  if (loserHad) args.loserHad = loserHad;
  const steps: FakePiStep[] = [{ kind: "call-submit", tool: "submit_pick_vote", args }];
  if (askAgain) {
    // The first submission (no loserHad) is refused; the conductor re-asks
    // once, and the second submission carries it.
    steps.push({ kind: "wait-for-prompt" });
    steps.push({
      kind: "call-submit",
      tool: "submit_pick_vote",
      args: { round, seat, lane, why: `${seat} picks ${lane} after the re-ask`, loserHad: loserHad ?? { yes: false, anchors: [] } },
    });
  }
  return { hello: { role: "picker" as const, tools: ROLE_TOOLS.picker }, steps };
}

test("plan 06k1: a pick vote without loserHad is re-asked, and a majority yes carries the loser's anchors into the next repair prompt", async () => {
  const promptLog = fs.mkdtempSync("/tmp/tt-06k1-lv-prompts-");
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: path.join(promptLog, "prompts.txt") },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const K = state.phase.contract.contractVersion;
      const round = liveRound(state);
      if (round === 1) {
        // M raises one blocking finding naming the acceptance item, so round 1
        // is not accepted and round 2 runs from the winner.
        return seat === "M"
          ? laneReview("M", K, {
              findings: [
                { kind: "defect", severity: "blocking", evidence: "it works is unmet: src/lane-a.txt:1 the output is wrong" },
              ],
            })
          : laneReview(seat as Reviewer, K);
      }
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === "M");
      return seat === "M"
        ? laneReview("M", K, {
            findingStatements: open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })),
          })
        : laneReview(seat as Reviewer, K);
    },
    pickScriptFor: (seat, state) => {
      const round = liveRound(state);
      // Lane b wins 2-1: M and A pick b, B picks a. M's first vote omits
      // loserHad and is re-asked; M and A say the losing lane (a) had
      // something the winner lacked.
      if (seat === "B") return pickVote("B" as Reviewer, round, "a", { yes: false, anchors: [] });
      return pickVote(
        seat as Reviewer,
        round,
        "b",
        { yes: true, anchors: ["src/lane-a.txt:1", "lane a keeps the marker file"] },
        seat === "M",
      );
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "the phase ran two rounds");
    const first = phase.rounds![0];

    // The re-ask is recorded, and the final vote carries loserHad.
    const events = readEvents(setup.runDir).filter((r) => r.kind === "event");
    assert.ok(events.some((r) => (r.event as { type?: string }).type === "PICK_VOTE"), "the pick votes were recorded");
    const firstVotes = first.votes;
    assert.equal(firstVotes.length, 3, "all three seats voted in round 1");
    const m = firstVotes.find((v) => v.seat === "M");
    assert.ok(m?.loserHad, "M's final vote carries loserHad");
    assert.equal(m!.loserHad!.yes, true);
    assert.deepEqual(m!.loserHad!.anchors, ["src/lane-a.txt:1", "lane a keeps the marker file"]);
    assert.equal(first.picked?.lane, "b");

    // Round 2's repair prompt lists the losing lane's anchors.
    const prompts = fs.readFileSync(path.join(promptLog, "prompts.txt"), "utf8");
    const secondRound = prompts.split("=====").filter((p) => p.includes("Round 2 of this phase"));
    assert.equal(secondRound.length, 2, "both lanes were prompted in round 2");
    for (const prompt of secondRound) {
      assert.match(prompt, /had something the winner lacked/);
      assert.match(prompt, /src\/lane-a\.txt:1/);
      assert.match(prompt, /lane a keeps the marker file/);
    }
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
    cleanupDir(promptLog);
  }
});

test("plan 06k1: loser-had-something counts only yes-votes from seats that picked the winning lane, on the deciding votes", () => {
  const round = (overrides: Partial<import("../../src/core/types.ts").RoundRecord>): import("../../src/core/types.ts").RoundRecord => ({
    round: 1,
    base: "B0",
    lanes: ["a", "b"],
    candidates: [
      { lane: "a", sha: "C1a", ok: true },
      { lane: "b", sha: "C1b", ok: true },
    ],
    votes: [],
    ...overrides,
  });
  // A's reproduction: winner a; A and B voted b (the LOSER) and said yes about
  // a.txt:1 / a.txt:2 — those refer to the WINNER a, not the loser b. The old
  // count took them as a majority; the fix must not, so there are no anchors.
  const aPickedLoser = round({
    votes: [
      { seat: "M", lane: "a", why: "a", loserHad: { yes: false, anchors: [] } },
      { seat: "A", lane: "b", why: "b", loserHad: { yes: true, anchors: ["a.txt:1"] } },
      { seat: "B", lane: "b", why: "b", loserHad: { yes: true, anchors: ["a.txt:2"] } },
    ],
    picked: { lane: "a", sha: "C1a", votes: 1 },
  });
  assert.deepEqual(loserAnchors(aPickedLoser, ["M", "A", "B"]), [], "a yes from a loser-picking seat is about the winner");
  assert.equal(pickStats(aPickedLoser, ["M", "A", "B"]).loserHadSomething, false);

  // Only the winner-picking seats' yes-votes count.
  const winnerPickers = round({
    votes: [
      { seat: "M", lane: "b", why: "b", loserHad: { yes: true, anchors: ["a.txt:1"] } },
      { seat: "A", lane: "b", why: "b", loserHad: { yes: true, anchors: ["a.txt:2"] } },
      { seat: "B", lane: "a", why: "a", loserHad: { yes: true, anchors: ["b.txt:1"] } },
    ],
    picked: { lane: "b", sha: "C1b", votes: 2 },
  });
  assert.deepEqual([...loserAnchors(winnerPickers, ["M", "A", "B"])].sort(), ["a.txt:1", "a.txt:2"], "B's yes is about the winner b, not the loser a");
  assert.equal(pickStats(winnerPickers, ["M", "A", "B"]).loserHadSomething, true);

  // D-B-54: the DECIDING votes are the revote when one ran, so a first-turn
  // yes from a seat that is not in the deciding majority no longer carries.
  const revoted = round({
    votes: [
      { seat: "M", lane: "a", why: "a", loserHad: { yes: true, anchors: ["stale.txt:1"] } },
      { seat: "A", lane: "b", why: "b", loserHad: { yes: true, anchors: ["stale.txt:2"] } },
      { seat: "B", lane: "c", why: "c", loserHad: { yes: true, anchors: ["stale.txt:3"] } },
    ],
    lanes: ["a", "b", "c"],
    candidates: [
      { lane: "a", sha: "C1a", ok: true },
      { lane: "b", sha: "C1b", ok: true },
      { lane: "c", sha: "C1c", ok: true },
    ],
    revote: {
      lanes: ["a", "b"],
      votes: [
        { seat: "M", lane: "a", why: "a", loserHad: { yes: false, anchors: [] } },
        { seat: "A", lane: "a", why: "a", loserHad: { yes: false, anchors: [] } },
        { seat: "B", lane: "a", why: "a", loserHad: { yes: false, anchors: [] } },
      ],
    },
    picked: { lane: "a", sha: "C1a", votes: 2 },
  });
  assert.deepEqual(loserAnchors(revoted, ["M", "A", "B"]), [], "the first-turn stale anchors are not deciding");
  assert.equal(pickStats(revoted, ["M", "A", "B"]).loserHadSomething, false);
  // Finding A-11: the shape is the DECIDING vote's shape. The first turn was
  // a three-way split, but the revote was unanimous for a, so the round is
  // unanimous, not split.
  assert.equal(pickStats(revoted, ["M", "A", "B"]).split, false, "a unanimous revote is not a split");
  assert.equal(pickStats(revoted, ["M", "A", "B"]).unanimous, true, "the revote decided unanimously");
});

test("plan 06k1: tt summary reports split picks and rounds where the loser had something, and not recorded for old events", () => {
  // A round with a split pick where a majority said the loser had something,
  // and one round whose votes predate `loserHad`.
  const events: MetricEvent[] = [
    { type: "ROUND_STARTED", round: 1 },
    { type: "PICK_VOTE", round: 1, seat: "M", lane: "b", loserHad: { yes: true, anchors: ["src/x.ts:1"] } },
    { type: "PICK_VOTE", round: 1, seat: "A", lane: "b", loserHad: { yes: true, anchors: ["src/y.ts:2"] } },
    { type: "PICK_VOTE", round: 1, seat: "B", lane: "a", loserHad: { yes: false, anchors: [] } },
    { type: "ROUND_STARTED", round: 2 },
    { type: "PICK_VOTE", round: 2, seat: "M", lane: "a" },
    { type: "PICK_VOTE", round: 2, seat: "A", lane: "a" },
    { type: "PICK_VOTE", round: 2, seat: "B", lane: "a" },
  ];
  const phase = {
    ...basePhase(),
    round: 2,
    candidate: { sha: "C2" },
    contract: { ...basePhase().contract, workers: 2 },
    rounds: [
      {
        round: 1,
        base: "B0",
        lanes: ["a", "b"],
        candidates: [
          { lane: "a", sha: "C1a", ok: true },
          { lane: "b", sha: "C1b", ok: true },
        ],
        votes: [
          { seat: "M", lane: "b", why: "b", loserHad: { yes: true, anchors: ["src/x.ts:1"] } },
          { seat: "A", lane: "b", why: "b", loserHad: { yes: true, anchors: ["src/y.ts:2"] } },
          { seat: "B", lane: "a", why: "a", loserHad: { yes: false, anchors: [] } },
        ],
        picked: { lane: "b", sha: "C1b", votes: 2 },
      },
      {
        round: 2,
        base: "C1b",
        lanes: ["a", "b"],
        candidates: [
          { lane: "a", sha: "C2a", ok: true },
          { lane: "b", sha: "C2b", ok: true },
        ],
        votes: [
          { seat: "M", lane: "a", why: "a" },
          { seat: "A", lane: "a", why: "a" },
          { seat: "B", lane: "a", why: "a" },
        ],
        picked: { lane: "a", sha: "C2a", votes: 3 },
      },
    ],
  };
  const m = computeMetrics(phase, { phases: [{ phase: "IMPLEMENTING", at: "2026-10-10T00:00:00.000Z" }] }, events);
  assert.equal(m.picks.rounds, 2, "two rounds had a pick turn");
  assert.equal(m.picks.split, 1, "round 1 was split");
  assert.equal(m.picks.unanimous, 1, "round 2 was unanimous");
  assert.equal(m.picks.loserHadSomething, 1, "round 1's majority said the loser had something");
  assert.equal(m.picks.notRecorded, 1, "round 2's votes predate loserHad");

  const summary = metricsSummary(m).join("\n");
  assert.match(summary, /Picks: 2 rounds · 1 split · 1 unanimous · 1 loser-had-something · 1 not recorded/);
  // The status view renders `metricsLine`; it must carry the same pick shape
  // (rounds, split vs unanimous, loser-had-something, not recorded).
  const status = metricsLine(m);
  assert.match(status, /picks 2 rounds: 1 split, 1 unanimous, 1 loser-had-something, 1 not recorded/);
});
