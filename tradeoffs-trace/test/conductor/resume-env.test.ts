// Plan 06k1 (A6): a worker that settles after a failed compaction ends its
// attempt at once with ATTEMPT_NO_SUBMISSION, and `resume` after `stop`
// restores the phase worktree at the last frozen candidate and the check
// environment recorded at start.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { Conductor, contractVersionFor, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import { readEvents } from "./harness.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  FAKE_PI_PATH,
  setupConductor,
  waitFor,
  writeScript,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 5_000,
  workerAttemptMs: 60_000,
  freezeMs: 30_000,
  checkMs: 30_000,
  probeMs: 30_000,
  reviewMs: 30_000,
  panelMs: 8_000,
};

function eventsOfType(runDir: string, type: string): unknown[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === type)
    .map((r) => r.event);
}

async function teardown(...setups: TestConductorSetup[]): Promise<void> {
  for (const s of setups) {
    await s.conductor.stop();
    cleanupDir(s.runRoot);
    cleanupDir(s.scriptsDir);
  }
}

test("plan 06k1: a worker that settles after a failed compaction ends its attempt with no submission", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScriptForAttempt: (attempt): { hello: unknown; steps: FakePiStep[] } =>
      attempt === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "emit", event: { type: "compaction_start" } },
              {
                kind: "emit",
                event: {
                  type: "compaction_end",
                  aborted: false,
                  willRetry: false,
                  errorMessage: "Context overflow recovery failed: the compaction request was refused",
                },
              },
              // The worker never submits and never settles: only the failed
              // compaction ends the attempt.
              { kind: "hang-forever" },
            ],
          }
        : {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: "printf 'attempt 2\\n' > attempt-2.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
          },
    reviewerScriptFor: (reviewer: Reviewer, state: State) => ({
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
            ballots: [],
            findings: [],
          },
        },
      ],
    }),
  });
  try {
    const startedAt = Date.now();
    await setup.conductor.start();
    // The attempt ends at once, not after the 60 s attempt deadline. The
    // budget is 3, so the phase reaches DONE on attempt 2.
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 30_000, 50, setup.runDir);
    const elapsed = Date.now() - startedAt;
    assert.ok(elapsed < 20_000, `the failed compaction ended the attempt quickly (took ${elapsed}ms, deadline 60s)`);
    assert.equal(eventsOfType(setup.runDir, "ATTEMPT_NO_SUBMISSION").length, 1, "the attempt ended as ATTEMPT_NO_SUBMISSION");
    assert.equal(eventsOfType(setup.runDir, "ATTEMPT_TIMED_OUT").length, 0, "it never waited out the attempt deadline");
  } finally {
    await teardown(setup);
  }
});

test("plan 06k1: a lane worker that fails compaction ends its attempt at once, not at the deadline", async () => {
  const laneWorker = (lane: string): { hello: unknown; steps: FakePiStep[] } => ({
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane}\n' > lane-${lane}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  });
  const laneReview = (seat: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } => ({
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
          contractVersion: state.phase.contract.contractVersion,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  });
  const setup = await setupConductor({
    phase: { id: "p1", goal: "build two candidates from one base", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], workers: 2 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 30_000 },
    workerScript: () => laneWorker("b"),
    laneWorkerScriptFor: (lane) =>
      lane === "a"
        ? {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "emit", event: { type: "compaction_end", aborted: false, willRetry: false, errorMessage: "Context overflow recovery failed" } },
              { kind: "hang-until-abort" },
            ],
          }
        : laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state),
    pickScriptFor: (seat, state) => ({
      hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
      steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { round: state.phase.rounds?.length ?? 1, seat, lane: "b", why: "only b passed", loserHad: { yes: false, anchors: [] } } }],
    }),
  });
  try {
    const startedAt = Date.now();
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000, 50, setup.runDir);
    const elapsed = Date.now() - startedAt;
    assert.ok(elapsed < 20_000, `the lane attempt ended at once (took ${elapsed}ms, deadline 30s)`);
    const round = setup.conductor.state.phase.rounds![0];
    const laneA = round.candidates.find((c) => c.lane === "a")!;
    assert.equal(laneA.sha, undefined, "lane a produced no candidate");
    assert.match(laneA.note ?? "", /failed compaction/, "the lane note names the failed compaction");
    assert.equal(setup.conductor.state.phase.candidate?.sha, round.candidates.find((c) => c.lane === "b")?.sha);
  } finally {
    await teardown(setup);
  }
});

test("plan 06k1: resume after stop restores the frozen candidate and the recorded check environment", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k1-resume-");
  const scripts = fs.mkdtempSync("/tmp/tt-06k1-resume-scripts-");
  const attempt1 = writeScript(scripts, "attempt1", {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: "printf 'candidate 1\\n' > candidate-1.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  });
  const attempt2 = writeScript(scripts, "attempt2", {
    hello: defaultWorkerHello(),
    steps: [
      // A short pause so the test can read the worktree HEAD right after the
      // resume, before attempt 2 commits.
      { kind: "sleep", ms: 1500 },
      { kind: "call-sh", command: "printf 'candidate 2\\n' > second.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  });
  const oldJavaHome = process.env.JAVA_HOME;
  const oldPath = process.env.PATH;
  const recordedJavaHome = "/recorded-jdk";
  // The check needs JAVA_HOME (recorded at start) and a file only attempt 2
  // creates, so attempt 1 fails and the run parks in REPAIRING.
  const check = `test "$JAVA_HOME" = "${recordedJavaHome}" && test -f second.txt`;
  process.env.JAVA_HOME = recordedJavaHome;

  const setup = await setupConductor({
    checks: [check],
    phaseChecks: [check],
    stubReviews: true,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps:
        attempt === 1
          ? [
              { kind: "call-sh", command: "printf 'candidate 1\\n' > candidate-1.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ]
          : [
              // A pause so the test can stop before attempt 2 commits.
              { kind: "sleep", ms: 2_000 },
              { kind: "call-sh", command: "printf 'candidate 2\\n' > second.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
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
            ballots: [],
            findings: [],
          },
        },
      ],
    }),
  });
  // The resume conductor's own reviewer script needs the run's frozen
  // contract version, which is a hash of the phase.
  const reviewerPath = writeScript(scripts, "reviewer", {
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: "$TT_REVIEWER",
          phaseId: "p1",
          candidateSha: "$TT_CANDIDATE_SHA",
          contractVersion: contractVersionFor((setup.plan as RunPlanFile).phases[0]),
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  });

  let resumed: Conductor | undefined;
  try {
    await setup.conductor.start();
    // Attempt 1's check fails (second.txt is absent). Waiting on the check
    // RECORD, not the transient REPAIRING state (which is gone within a
    // poll), so the stop happens before attempt 2 submits.
    await waitFor(
      () => setup.conductor.state.phase.checks?.candidateSha !== undefined && setup.conductor.state.phase.checks?.passed === false,
      60_000,
      50,
      setup.runDir,
    );
    const candidateSha = setup.conductor.state.phase.candidate?.sha;
    assert.ok(candidateSha, "a candidate was frozen before the stop");

    // The environment the checks need was recorded at start.
    const envFile = path.join(setup.runDir, "check-env.json");
    const recorded = JSON.parse(fs.readFileSync(envFile, "utf8")) as Record<string, string>;
    assert.equal(recorded.JAVA_HOME, recordedJavaHome, "JAVA_HOME was recorded");
    assert.equal(recorded.PATH, oldPath, "PATH was recorded");

    await setup.conductor.stop();
    // The resuming shell has a different JAVA_HOME (and could lack it).
    process.env.JAVA_HOME = "/other-jdk";

    const plan = setup.plan as RunPlanFile;
    resumed = new Conductor({
      runDir: setup.runDir,
      plan,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      stubReviews: true,
      deadlines: FAST,
      checkLockPath: path.join(setup.runRoot, "check.lock"),
      piEnvFor: (role, agentId) => {
        if (role === "worker") {
          const attempt = Number(agentId.match(/^worker-(\d+)-/)?.[1] ?? "1");
          return { FAKE_PI_SCRIPT: attempt >= 2 ? attempt2 : attempt1 };
        }
        if (role === "reviewer") return { FAKE_PI_SCRIPT: reviewerPath };
        return undefined;
      },
    });
    await resumed.start();

    // The worktree is at the last frozen candidate, not the phase base.
    const worktree = runPaths(setup.runDir).worktree;
    const head = execFileSync("git", ["-C", worktree, "rev-parse", "HEAD"], { encoding: "utf8" }).trim();
    assert.equal(head, candidateSha, "the worktree is restored at the frozen candidate");

    // Attempt 2's check sees the recorded JAVA_HOME (not the resuming
    // shell's), so it passes and the phase finishes.
    await waitFor(() => resumed!.state.phase.phase === "DONE", 60_000, 50, setup.runDir);
    assert.equal(resumed.state.phase.phase, "DONE");
  } finally {
    if (resumed) await resumed.stop();
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
    fs.rmSync(scripts, { recursive: true, force: true });
    if (oldJavaHome === undefined) delete process.env.JAVA_HOME;
    else process.env.JAVA_HOME = oldJavaHome;
    process.env.PATH = oldPath;
  }
});

test("plan 06k1: a declared variable unset at start stays unset on resume", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k1-unset-");
  const scripts = fs.mkdtempSync("/tmp/tt-06k1-unset-scripts-");
  const observed = path.join(dir, "observed");
  const oldJavaHome = process.env.JAVA_HOME;
  delete process.env.JAVA_HOME;
  const attempt1 = writeScript(scripts, "attempt1", {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: "printf 'c1\n' > candidate-1.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  });
  const attempt2 = writeScript(scripts, "attempt2", {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "sleep", ms: 1_500 },
      { kind: "call-sh", command: "printf 'c2\n' > second.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  });
  // The check requires JAVA_HOME to be EMPTY (it was unset at start) and a
  // file only attempt 2 creates, then records what it saw.
  const check = `test -z "$JAVA_HOME" && test -f second.txt && printf '%s' "$JAVA_HOME" > ${observed}`;
  const setup = await setupConductor({
    checks: [check],
    phaseChecks: [check],
    stubReviews: true,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps:
        attempt === 1
          ? [
              { kind: "call-sh", command: "printf 'c1\n' > candidate-1.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ]
          : [
              { kind: "sleep", ms: 1_500 },
              { kind: "call-sh", command: "printf 'c2\n' > second.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
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
            ballots: [],
            findings: [],
          },
        },
      ],
    }),
  });
  const reviewerPath = writeScript(scripts, "reviewer", {
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: "$TT_REVIEWER",
          phaseId: "p1",
          candidateSha: "$TT_CANDIDATE_SHA",
          contractVersion: contractVersionFor((setup.plan as RunPlanFile).phases[0]),
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  });
  let resumed: Conductor | undefined;
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.checks?.passed === false, 60_000, 50, setup.runDir);
    const envFile = path.join(setup.runDir, "check-env.json");
    const recorded = JSON.parse(fs.readFileSync(envFile, "utf8")) as Record<string, string | null>;
    assert.equal(recorded.JAVA_HOME, null, "an unset declared variable is recorded as unset");
    await setup.conductor.stop();
    // The resuming shell supplies JAVA_HOME; the recorded unset must win.
    process.env.JAVA_HOME = "/other-jdk";
    resumed = new Conductor({
      runDir: setup.runDir,
      plan: setup.plan as RunPlanFile,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      stubReviews: true,
      deadlines: FAST,
      checkLockPath: path.join(setup.runRoot, "check.lock"),
      piEnvFor: (role, agentId) => {
        if (role === "worker") {
          const attempt = Number(agentId.match(/^worker-(\d+)-/)?.[1] ?? "1");
          return { FAKE_PI_SCRIPT: attempt >= 2 ? attempt2 : attempt1 };
        }
        if (role === "reviewer") return { FAKE_PI_SCRIPT: reviewerPath };
        return undefined;
      },
    });
    await resumed.start();
    await waitFor(() => resumed!.state.phase.phase === "DONE", 60_000, 50, setup.runDir);
    assert.equal(fs.readFileSync(observed, "utf8"), "", "the resumed check saw JAVA_HOME unset");
  } finally {
    if (resumed) await resumed.stop();
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
    fs.rmSync(scripts, { recursive: true, force: true });
    if (oldJavaHome === undefined) delete process.env.JAVA_HOME;
    else process.env.JAVA_HOME = oldJavaHome;
  }
});
