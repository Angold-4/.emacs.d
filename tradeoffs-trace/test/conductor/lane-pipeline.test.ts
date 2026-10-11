// Plan 06l (A4): a lane check that fails only on tests that pass when re-run
// alone passes, with one FLAKE_OBSERVED per test; a test that fails alone
// fails the lane check. This is the lane-pipeline counterpart of the
// single-candidate flakes.test.ts.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import { test } from "node:test";

import { runPaths } from "../../src/conductor.ts";
import { readLog } from "../../src/effects/log.ts";
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

function laneWorker(lane: string, marker: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf '${marker}\\n' > lane-${lane}.txt` },
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

function pickVote(seat: Reviewer, round: number, lane: string, why: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { round, seat, lane, why, loserHad: { yes: false, anchors: [] } } }],
  };
}

function liveRound(state: State): number {
  return state.phase.rounds?.length ?? 1;
}

function records(runDir: string) {
  return readLog(runPaths(runDir).events).records;
}

test("plan 06l: a check failure that passes alone is recorded as flaky and does not fail the check", async () => {
  // Scenario 1: lane a's check fails on one test that passes when re-run
  // alone; lane a's check must PASS and the flake must be recorded.
  const flakySetup = await setupConductor({
    phase: lanePhase(),
    // lane a's file makes the check fail; lane b's does not.
    checks: ["if grep -q tt-flake lane-a.txt; then printf '✖ %s\\n' 'a flaky test'; exit 1; fi"],
    phaseChecks: ["if grep -q tt-flake lane-a.txt; then printf '✖ %s\\n' 'a flaky test'; exit 1; fi"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a", "tt-flake"),
    laneWorkerScriptFor: (lane) => laneWorker(lane, lane === "a" ? "tt-flake" : "clean"),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only pick"),
  });
  flakySetup.plan.rerun = "printf '✔ %s\\n' {name}; exit 0";
  try {
    await flakySetup.conductor.start();
    await waitFor(() => flakySetup.conductor.state.phase.phase === "DONE", 180_000, 50, flakySetup.runDir);
    const log = records(flakySetup.runDir);
    const laneAFinished = log.find((r) => r.kind === "lane_check_finished" && (r.event as { lane?: string }).lane === "a");
    assert.ok(laneAFinished, "lane a's check finished");
    assert.equal((laneAFinished!.event as { passed?: boolean }).passed, true, "a load-only failure does not fail the check");
    const flakes = log.filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "FLAKE_OBSERVED");
    assert.equal(flakes.length, 1, "one flake observation");
    assert.equal((flakes[0].event as { name?: string }).name, "a flaky test");
    assert.equal((flakes[0].event as { savedRound?: boolean }).savedRound, true, "the check passed only because of it");
    assert.ok(
      log.some((r) => r.kind === "lane_check_failures_load_only"),
      "the lane records the load-only classification",
    );
    const phase = flakySetup.conductor.state.phase;
    assert.equal(phase.rounds![0].candidates.find((c) => c.lane === "a")!.ok, true);
  } finally {
    await teardown(flakySetup);
  }

  // Scenario 2: the same check also names a test that fails when re-run
  // alone; the lane check must FAIL on it.
  const realSetup = await setupConductor({
    phase: lanePhase(),
    checks: ["if grep -q tt-flake lane-a.txt; then printf '✖ %s\\n' 'a flaky test'; printf '✖ %s\\n' 'a real failure'; exit 1; fi"],
    phaseChecks: ["if grep -q tt-flake lane-a.txt; then printf '✖ %s\\n' 'a flaky test'; printf '✖ %s\\n' 'a real failure'; exit 1; fi"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a", "tt-flake"),
    laneWorkerScriptFor: (lane) => laneWorker(lane, lane === "a" ? "tt-flake" : "clean"),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "b", "lane b is clean"),
  });
  realSetup.plan.rerun = "case {name} in 'a flaky test') printf '✔ %s\\n' {name}; exit 0;; *) printf '✖ %s\\n' {name}; exit 1;; esac";
  try {
    await realSetup.conductor.start();
    // lane a's check fails, so the round repeats until the attempt budget is
    // spent or lane b alone passes; either way the failure must be recorded.
    await waitFor(
      () => records(realSetup.runDir).some((r) => r.kind === "lane_check_finished" && (r.event as { lane?: string }).lane === "a" && (r.event as { passed?: boolean }).passed === false),
      120_000,
      50,
      realSetup.runDir,
    );
    const finished = records(realSetup.runDir).find(
      (r) => r.kind === "lane_check_finished" && (r.event as { lane?: string }).lane === "a" && (r.event as { passed?: boolean }).passed === false,
    );
    assert.match(String((finished!.event as { note?: string }).note), /a real failure/, "the real failure fails the lane check");
    // The flake is still recorded, but never saves this check.
    const flakes = records(realSetup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "FLAKE_OBSERVED");
    assert.ok(flakes.every((f) => (f.event as { savedRound?: boolean }).savedRound === false));
  } finally {
    await teardown(realSetup);
  }

  // Scenario 3: a test that fails the FIRST isolated re-run and would pass a
  // second must still fail the lane check — A4 allows ONE isolated re-run.
  const counter = `/tmp/tt-lane-once-${process.pid}-${Date.now()}`;
  fs.rmSync(counter, { force: true });
  const onceSetup = await setupConductor({
    phase: lanePhase(),
    checks: ["if grep -q tt-flake lane-a.txt; then printf '✖ %s\\n' 'a one-shot test'; exit 1; fi"],
    phaseChecks: ["if grep -q tt-flake lane-a.txt; then printf '✖ %s\\n' 'a one-shot test'; exit 1; fi"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a", "tt-flake"),
    laneWorkerScriptFor: (lane) => laneWorker(lane, lane === "a" ? "tt-flake" : "clean"),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "b", "lane b is clean"),
  });
  onceSetup.plan.rerun = `n=$(cat ${counter} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${counter}; if [ $n -ge 2 ]; then printf '✔ %s\\n' {name}; exit 0; fi; printf '✖ %s\\n' {name}; exit 1`;
  try {
    await onceSetup.conductor.start();
    await waitFor(
      () => records(onceSetup.runDir).some((r) => r.kind === "lane_check_finished" && (r.event as { lane?: string }).lane === "a" && (r.event as { passed?: boolean }).passed === false),
      120_000,
      50,
      onceSetup.runDir,
    );
    assert.equal(fs.readFileSync(counter, "utf8").trim(), "1", "exactly one isolated re-run was made");
    const finished = records(onceSetup.runDir).find(
      (r) => r.kind === "lane_check_finished" && (r.event as { lane?: string }).lane === "a" && (r.event as { passed?: boolean }).passed === false,
    );
    assert.match(String((finished!.event as { note?: string }).note), /a one-shot test/, "a test that failed its one isolated re-run fails the check");
  } finally {
    await teardown(onceSetup);
    fs.rmSync(counter, { force: true });
  }
});
