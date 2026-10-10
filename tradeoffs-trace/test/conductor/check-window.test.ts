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
    workerScript: () => slowWorker(14_000),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await Promise.all([setupA.conductor.start(), setupB.conductor.start()]);

    // While A's check runs, B's worker must be observed STOPPED (state T).
    let sawStopped = false;
    const deadline = Date.now() + 20_000;
    while (Date.now() < deadline && !sawStopped) {
      for (const pid of workerPids(markerB)) {
        if (processState(pid).startsWith("T")) sawStopped = true;
      }
      if (!sawStopped) await sleep(100);
    }
    assert.ok(sawStopped, "B's worker was paused while A's check window was open");

    // A check that holds the window, and B's attempt clock paused with it:
    // B still submits and finishes, though 12 s is less than its 14 s of work.
    await waitFor(() => setupB.conductor.state.phase.phase === "DONE", 60_000, 100, setupB.runDir);
    await waitFor(() => setupA.conductor.state.phase.phase === "DONE", 60_000, 100, setupA.runDir);

    // The pause and resume are recorded.
    const bEvents = readFileSync(`${setupB.runDir}/events.jsonl`, "utf8");
    assert.match(bEvents, /worker_paused/);
    assert.match(bEvents, /worker_resumed/);
  } finally {
    await teardown(setupA, setupB);
    try {
      execFileSync("rm", ["-rf", path.dirname(checkLockPath)]);
    } catch {
      // best effort
    }
  }
});
