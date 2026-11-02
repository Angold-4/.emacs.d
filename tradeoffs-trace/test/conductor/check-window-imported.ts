// Plan 06k1 (A4): the host-wide check window. While one run's check holds the
// machine, another run's worker is SIGSTOPped and its attempt clock does not
// advance; it resumes (SIGCONT) when the check ends.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync } from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  setupConductor,
  sleep,
  waitFor,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

/** A stub reviewer: one turn, an empty review. `stubReviews` still dispatches
 * the seats, so a script is needed (see happy-path.test.ts). */
function stubReviewer(seat: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
          phaseId: state.phase.phaseId,
          candidateSha: state.phase.candidate?.sha,
          contractVersion: state.phase.contract.contractVersion,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  };
}

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  checkMs: 30_000,
  probeMs: 15_000,
  freezeMs: 15_000,
  evaluateMs: 8_000,
  reviewMs: 10_000,
  panelMs: 8_000,
};

function submittingWorker(): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: "printf 'ok\\n' > marker.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** A worker that stays busy for `ms` before submitting — long enough that the
 * other run's check window must pause it, or its attempt times out first. */
function slowWorker(ms: number): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "sleep", ms },
      { kind: "call-sh", command: "printf 'ok\\n' > marker.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** The pid of run B's live worker, by its unique argv marker. */
function workerPids(marker: string): number[] {
  try {
    return execFileSync("pgrep", ["-f", marker], { encoding: "utf8" })
      .split("\n")
      .map((s) => Number(s.trim()))
      .filter((n) => Number.isInteger(n) && n > 0);
  } catch {
    return [];
  }
}

function processState(pid: number): string {
  try {
    return execFileSync("ps", ["-o", "stat=", "-p", String(pid)], { encoding: "utf8" }).trim();
  } catch {
    return "";
  }
}

async function teardown(...setups: TestConductorSetup[]): Promise<void> {
  for (const s of setups) {
    await s.conductor.stop();
    cleanupDir(s.runRoot);
    cleanupDir(s.scriptsDir);
  }
}

test("plan 06k1: a check in one run pauses the workers of another run and resumes them after", async () => {
  const markerA = "tt-06k1-checkwindow-A";
  const markerB = "tt-06k1-checkwindow-B";
  // The two runs share ONE machine-wide check lock, so A's check window is
  // visible to B. (Other tests use their own run-root lock, so they never
  // pause each other.)
  const checkLockPath = path.join(mkdtempSync("/tmp/tt-06k1-window-"), "check.lock");
  const setupA = await setupConductor({
    checks: ["sleep 4"],
    phaseChecks: ["sleep 4"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 20_000 },
    extraPiArgsPrefix: [markerA],
    checkLockPath,
    workerScript: () => submittingWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  const setupB = await setupConductor({
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    // Without the pause, B's worker would time out at 12 s, before its 14 s
    // of work is done.
    deadlines: { ...FAST, workerAttemptMs: 12_000 },
    extraPiArgsPrefix: [markerB],
    checkLockPath,
    // 07e (E5): the host-wide pause is opt-in now; this run asks for it.
    checkWindow: "exclusive",
    workerScript: () => slowWorker(14_000),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await Promise.all([setupA.conductor.start(), setupB.conductor.start()]);

    // While A's check runs, B's worker must be observed STOPPED (state T),
    // and the status written on the pause beat must name the pausing run.
    let sawStopped = false;
    let pausedStatus = "";
    const deadline = Date.now() + 20_000;
    while (Date.now() < deadline && pausedStatus === "") {
      if (!sawStopped && workerPids(markerB).some((pid) => processState(pid).startsWith("T"))) sawStopped = true;
      try {
        const text = readFileSync(`${setupB.runDir}/views/status.txt`, "utf8");
        if (/paused\s+paused by run .*'s check/.test(text)) pausedStatus = text;
      } catch {
        // the first status write may not have landed yet
      }
      if (pausedStatus === "") await sleep(100);
    }
    assert.ok(sawStopped, "B's worker was paused while A's check window was open");

    // A check that holds the window, and B's attempt clock paused with it:
    // B still submits and finishes, though 12 s is less than its 14 s of work.
    await waitFor(() => setupB.conductor.state.phase.phase === "DONE", 60_000, 100, setupB.runDir);
    await waitFor(() => setupA.conductor.state.phase.phase === "DONE", 60_000, 100, setupA.runDir);

    // The pause and resume are recorded, with the run that caused it.
    const bEvents = readFileSync(`${setupB.runDir}/events.jsonl`, "utf8");
    assert.match(bEvents, /worker_paused/);
    assert.match(bEvents, /worker_resumed/);
    assert.match(bEvents, /paused by run [^"\\]+'s check/);

    // 07e (A5): the status said who paused it, on the pause beat.
    assert.match(pausedStatus, /paused\s+paused by run [^\n]+'s check/);
  } finally {
    await teardown(setupA, setupB);
    try {
      execFileSync("rm", ["-rf", path.dirname(checkLockPath)]);
    } catch {
      // best effort
    }
  }
});

test("without TT_CHECK_WINDOW exclusive another run's open check window does not pause this run's workers", async () => {
  const markerA = "tt-07e-nowindow-A";
  const markerB = "tt-07e-nowindow-B";
  const checkLockPath = path.join(mkdtempSync("/tmp/tt-07e-window-"), "check.lock");
  const setupA = await setupConductor({
    checks: ["sleep 4"],
    phaseChecks: ["sleep 4"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 20_000 },
    extraPiArgsPrefix: [markerA],
    checkLockPath,
    workerScript: () => submittingWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  const setupB = await setupConductor({
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 30_000 },
    extraPiArgsPrefix: [markerB],
    checkLockPath,
    // No checkWindow: the default.
    workerScript: () => slowWorker(6_000),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await Promise.all([setupA.conductor.start(), setupB.conductor.start()]);
    // Over the whole overlap, no B worker is ever stopped and B never logs a
    // pause: the check window is another run's scheduling concern by default.
    let sawRunning = false;
    const deadline = Date.now() + 30_000;
    while (Date.now() < deadline) {
      for (const pid of workerPids(markerB)) {
        const state = processState(pid);
        if (state === "") continue;
        sawRunning = true;
        assert.ok(!state.startsWith("T"), `B's worker must not be paused by A's check window (state ${state})`);
      }
      if (setupA.conductor.state.phase.phase === "DONE" && setupB.conductor.state.phase.phase === "DONE") break;
      await sleep(100);
    }
    assert.ok(sawRunning, "B's worker was observed running");
    await waitFor(() => setupB.conductor.state.phase.phase === "DONE", 60_000, 100, setupB.runDir);
    const bEvents = readFileSync(`${setupB.runDir}/events.jsonl`, "utf8");
    assert.doesNotMatch(bEvents, /worker_paused/);
  } finally {
    await teardown(setupA, setupB);
    try {
      execFileSync("rm", ["-rf", path.dirname(checkLockPath)]);
    } catch {
      // best effort
    }
  }
});
