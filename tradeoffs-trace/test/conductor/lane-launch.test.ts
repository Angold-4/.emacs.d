// Plan 06k2 (A1): a lane worker that fails to launch is retried once before
// the round continues with one lane; and the lane round's own review/pick
// sub-phase is visible in the loop view with the attempt clock stopped.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import { test } from "node:test";

import { runPaths } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

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
  // Plan 06k2 (A3): a loaded check run (this file beside seven others) must
  // still let a lane agent answer hello; a 300 ms limit flaked the two tests
  // here. The retry window is the same value, so the retry's own start has
  // room too.
  helloTimeoutMs: 2_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 8_000,
  reviewMs: 30_000,
  panelMs: 8_000,
};

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

function lanePhase() {
  return {
    id: "p1",
    goal: "build two candidates from one base",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers: 2,
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

function laneReview(seat: Reviewer, contractVersion: unknown, sleepMs = 0): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      ...(sleepMs > 0 ? [{ kind: "sleep", ms: sleepMs } as FakePiStep] : []),
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
          phaseId: "p1",
          candidateSha: "$TT_CANDIDATE_SHA",
          contractVersion,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  };
}

function pickVote(seat: Reviewer, round: number, lane: string, sleepMs = 0): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [
      ...(sleepMs > 0 ? [{ kind: "sleep", ms: sleepMs } as FakePiStep] : []),
      { kind: "call-submit", tool: "submit_pick_vote", args: { round, seat, lane, why: `${seat} picks ${lane}`, loserHad: { yes: false, anchors: [] } } },
    ],
  };
}

function liveRound(state: State): number {
  return state.phase.rounds?.length ?? 1;
}

test("plan 06k2: a lane whose worker fails to launch is retried once before the round runs one lane", async () => {
  const calls = new Map<string, number>();
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => {
      const n = (calls.get(lane) ?? 0) + 1;
      calls.set(lane, n);
      // Lane a's FIRST launch misses hello (its retry, a fresh agent id, gets
      // a script with no delay); lane b starts normally.
      if (lane === "a" && n === 1) return { hello: defaultWorkerHello(), helloDelayMs: 5_000, steps: [] };
      return laneWorker(lane);
    },
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    // The retry is logged and the round still has two candidates.
    const events = readEvents(setup.runDir);
    assert.equal(
      events.filter((r) => r.kind === "lane_launch_retried").length,
      1,
      "the lane launch was retried once and logged",
    );
    assert.ok(
      events.some((r) => r.kind === "event" && (r.event as { type?: string }).type === "LAUNCH_RETRIED"),
      "the retry is recorded as a LAUNCH_RETRIED event",
    );
    assert.equal(phase.rounds?.length, 1);
    const round = phase.rounds![0];
    assert.equal(round.candidates.length, 2, "the round ran both lanes");
    assert.ok(
      round.candidates.every((c) => typeof c.sha === "string" && c.sha.length > 0),
      "both lanes froze a candidate after the retry",
    );
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
  }
});

test("plan 06k2: during lane reviews the loop shows LANE REVIEW and the attempt clock is stopped", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    // A deliberately short implement budget: the lane reviews run past it, so
    // without the stopped clock the pipeline would read 'over by'. The budget
    // still leaves a loaded host room to write a lane file and submit.
    deadlines: { ...FAST, workerAttemptMs: 8_000 },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    // Turn 2 sleeps long enough for the test to read the loop view while the
    // three seats are still reviewing.
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion, 12_000),
    // One pick seat sleeps so the test can read the loop view during the pick;
    // the others vote at once, so the pick turn's own wait is short.
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", seat === "M" ? 12_000 : 0),
  });
  const statusPath = runPaths(setup.runDir).status;
  const readStatus = () => {
    try {
      return fs.readFileSync(statusPath, "utf8");
    } catch {
      return "";
    }
  };
  try {
    await setup.conductor.start();
    // LANE REVIEW: both lanes submitted and the reviews are running.
    await waitFor(() => readStatus().includes("LANE REVIEW"), 150_000, 50, setup.runDir);
    const reviewStatus = readStatus();
    assert.match(reviewStatus, /loop\s+▶ LANE REVIEW/, "the loop view reads LANE REVIEW during the lane reviews");
    assert.ok(!reviewStatus.includes("over by"), "the attempt clock is stopped, so nothing reads 'over by'");
    // PICK: the reviews finished and the pick turn is running.
    await waitFor(() => readStatus().includes("▶ PICK"), 150_000, 50, setup.runDir);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
  } finally {
    await teardown(setup);
  }
});
