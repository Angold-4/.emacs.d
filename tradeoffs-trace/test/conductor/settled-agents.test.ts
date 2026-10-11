// Plan 06l (A3): a seat late on turn 1 is retried at the turn-1 deadline, and
// an agent whose process exits or whose model call fails terminally (a 404, a
// context overflow) without submitting is detected and retried at once — never
// by waiting out a full review or attempt deadline.
//
// R4: a lane seat that hangs on its discovery turn fails at the discovery
//     deadline (a third of the per-stage review deadline), the retry runs, and
//     the other seats' turn 2 is not held for the full review deadline.
// R5: a worker that exits, or reports a 404 or context overflow, without
//     submitting goes straight to a fresh attempt within a minute, and the
//     recorded event names the cause.

import assert from "node:assert/strict";
import { test } from "node:test";

import { runPaths } from "../../src/conductor.ts";
import { readLog } from "../../src/effects/log.ts";
import { terminalModelError } from "../../src/effects/pi-rpc.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
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

function laneReview(seat: Reviewer, contractVersion: unknown): { hello: unknown; steps: FakePiStep[] } {
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
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  };
}

/** A review whose turn-1 discovery sleeps far past the discovery deadline. */
function hangingDiscovery(seat: Reviewer, contractVersion: unknown): { hello: unknown; steps: FakePiStep[] } {
  const review = laneReview(seat, contractVersion);
  review.steps.unshift({ kind: "sleep", ms: 30_000 });
  return review;
}

function pickVote(seat: Reviewer, round: number, lane: string, why: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { round, seat, lane, why, loserHad: { yes: false, anchors: [] } } }],
  };
}

function liveRound(state: State): number {
  return state.phase.rounds?.length ?? 1;
}

function submitPhaseStep(): FakePiStep {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

function reviewerFor() {
  return (reviewer: Reviewer, state: State) => ({
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer,
          phaseId: state.phase.phaseId,
          candidateSha: state.phase.candidate?.sha,
          contractVersion: state.phase.contract.contractVersion,
          correctionStatements: [],
          findingStatements: [],
        },
      },
    ],
  });
}

function records(runDir: string) {
  return readLog(runPaths(runDir).events).records;
}

test("plan 06l: a seat late on turn 1 is retried at the turn-1 deadline, not the review deadline", async () => {
  // reviewMs is 9 s, so turn 1's own deadline is 3 s. Seat M's discovery
  // sleeps 30 s on its first dispatch; the retry (a fresh agent) discovers at
  // once. The other seats wait at the barrier only ~3 s, not 9 s.
  const reviewMs = 9_000;
  let mDispatches = 0;
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, reviewMs },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      if (seat === "M") {
        mDispatches += 1;
        // Only M's very first dispatch hangs; its retry and its review of the
        // other candidate behave.
        if (mDispatches === 1) return hangingDiscovery(seat as Reviewer, state.phase.contract.contractVersion);
      }
      return laneReview(seat as Reviewer, state.phase.contract.contractVersion);
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only pick"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const log = records(setup.runDir);
    const retried = log.filter((r) => r.kind === "lane_review_retried" && (r.event as { seat?: string }).seat === "M");
    assert.equal(retried.length, 1, "seat M's turn-1 timeout was retried once");
    assert.match(String((retried[0].event as { reason?: string }).reason), /turn 1/, "the retry names the turn-1 deadline");
    // The first attempt failed at the turn-1 deadline, well before reviewMs.
    const intents = log.filter(
      (r) =>
        r.kind === "intent" &&
        (r.event as { seat?: string }).seat === "M" &&
        typeof (r.event as { candidateSha?: string }).candidateSha === "string",
    );
    const firstStart = Math.min(...intents.map((r) => Date.parse(r.ts)));
    assert.ok(
      Date.parse(retried[0].ts) - firstStart < reviewMs,
      `the turn-1 retry (${Date.parse(retried[0].ts) - firstStart}ms) must come before the full review deadline (${reviewMs}ms)`,
    );
    // Every seat finished every review; no other seat timed out.
    assert.equal(log.filter((r) => r.kind === "lane_review_submitted").length, 6, "all six reviews completed");
    const timedOut = log.filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "REVIEW_TIMED_OUT");
    assert.equal(timedOut.length, 0, "no seat hit the full review deadline");
    const phase = setup.conductor.state.phase;
    for (const c of phase.rounds![0].candidates) assert.equal((c.reviews ?? []).length, 3);
  } finally {
    await teardown(setup);
  }
});

test("plan 06l: an agent that exits or fails its model call without submitting is retried within one minute", async () => {
  // R5: three terminal ends without a submission. Each must go straight to a
  // fresh attempt (a repair attempt), within a minute, with the cause named.
  const scenarios: Array<{ name: string; steps: FakePiStep[]; cause: RegExp; event: string }> = [
    { name: "exit", steps: [{ kind: "crash", code: 9 }], cause: /exited \(exit code 9\)/i, event: "ATTEMPT_INTERRUPTED" },
    {
      name: "404",
      steps: [{ kind: "emit", event: { type: "agent_end", errorMessage: "404 model_not_found: deepseek/deepseek-v4.1-flash-fast" } }],
      cause: /404|model_not_found/i,
      event: "ATTEMPT_NO_SUBMISSION",
    },
    {
      name: "context overflow",
      steps: [{ kind: "emit", event: { type: "compaction_end", aborted: false, willRetry: false, errorMessage: "Context overflow recovery failed" } }],
      cause: /context overflow/i,
      event: "ATTEMPT_NO_SUBMISSION",
    },
  ];
  for (const scenario of scenarios) {
    const setup = await setupConductor({
      checks: ["true"],
      phaseChecks: ["true"],
      stubReviews: true,
      deadlines: FAST,
      workerScript: () => ({ hello: defaultWorkerHello(), steps: scenario.steps }),
      reviewerScriptFor: reviewerFor(),
    });
    try {
      await setup.conductor.start();
      // The terminal end and the immediate re-dispatch. (An `exit` is
      // ATTEMPT_INTERRUPTED and re-runs the SAME attempt, so the test stops as
      // soon as the retry is observed rather than waiting for a phase that
      // would crash forever on a static script.)
      // The retry is a fresh `dispatch_worker` intent after the terminal end
      // (an exit re-runs the same attempt; a model failure starts a repair).
      const retryIntent = (log: ReturnType<typeof records>, after: number) =>
        log.find((r) => r.kind === "intent" && (r.actionId ?? "").startsWith("dispatch_worker") && Date.parse(r.ts) > after);
      await waitFor(
        () => {
          const log = records(setup.runDir);
          const failure = log.find((r) => r.kind === "event" && (r.event as { type?: string }).type === scenario.event);
          if (!failure) return false;
          return retryIntent(log, Date.parse(failure.ts)) !== undefined;
        },
        60_000,
        25,
        setup.runDir,
      );
      const log = records(setup.runDir);
      const failure = log.find((r) => r.kind === "event" && (r.event as { type?: string }).type === scenario.event)!;
      assert.match(String((failure.event as { note?: string }).note), scenario.cause, `${scenario.name}: the event names the cause`);
      // A fresh dispatch started within a minute of the terminal end.
      const failureAt = Date.parse(failure.ts);
      const nextAttempt = retryIntent(log, failureAt);
      assert.ok(nextAttempt, `${scenario.name}: the attempt was retried`);
      assert.ok(
        Date.parse(nextAttempt!.ts) - failureAt < 60_000,
        `${scenario.name}: the retry came within a minute (${Date.parse(nextAttempt!.ts) - failureAt}ms)`,
      );
    } finally {
      await teardown(setup);
    }
  }
});

test("plan 06l: a lane reviewer's terminal model failure is retried at once with the cause", async () => {
  // A-8: seat M's first dispatch reports a 404 and stays alive without
  // settling. The turn must end at once (not at the discovery deadline) and
  // the seat's one retry must run, with the cause in the retry record.
  let mDispatches = 0;
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      if (seat === "M") {
        mDispatches += 1;
        if (mDispatches === 1) {
          return {
            hello: defaultReviewerHello(),
            steps: [
              { kind: "emit", event: { type: "error", errorMessage: "404 model_not_found: deepseek/deepseek-v4.1-flash-fast" } },
              // Stay alive without settling, so only the terminal-failure
              // signal can end the turn.
              { kind: "hang-until-abort" },
            ],
          };
        }
      }
      return laneReview(seat as Reviewer, state.phase.contract.contractVersion);
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only pick"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const retried = records(setup.runDir).filter((r) => r.kind === "lane_review_retried" && (r.event as { seat?: string }).seat === "M");
    assert.equal(retried.length, 1, "seat M's terminal model failure was retried once");
    assert.match(String((retried[0].event as { reason?: string }).reason), /404|model call failed terminally/, "the retry names the model failure");
    const phase = setup.conductor.state.phase;
    for (const c of phase.rounds![0].candidates) assert.equal((c.reviews ?? []).length, 3, "the retry completed the review");
  } finally {
    await teardown(setup);
  }
});

test("plan 06l: terminal model-error detection names only the terminal causes", () => {
  assert.equal(terminalModelError("404 model_not_found"), true);
  assert.equal(terminalModelError("Context overflow recovery failed"), true);
  assert.equal(terminalModelError("maximum context length exceeded"), true);
  assert.equal(terminalModelError("429 rate limited, retrying"), false);
  assert.equal(terminalModelError("503 service unavailable"), false);
});
