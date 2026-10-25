// ODP-3 (carried from 06c, owner-confirmed 2026-10-07): "on a stop during
// run_checks, conductor.ts returns before its kill loop, so a running check's
// process can be orphaned. In 06e, a stop signals the run's own check process
// group too, like every other stage's, with a fake-process test."
//
// The check command here is a REAL child process — a shell that, once a marker
// file exists, writes its own process-group id and then sleeps past the stop.
// The marker is written by the worker's own `sh` step, so the check is quick
// while the base baseline runs it and long-running when CHECKING runs it: the
// orphan this test is about is the CHECKING one. The test watches the real
// process table, and after `stop()` the check's group must be gone.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { Conductor, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { readLog } from "../../src/effects/log.ts";
import { processAlive } from "../../src/effects/sweep.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  FAKE_PI_PATH,
  readEvents,
  setupConductor,
  waitFor,
  writeScript,
  type FakePiStep,
} from "./harness.ts";

function pgidAlive(pgid: number): boolean {
  try {
    process.kill(-pgid, 0);
    return true;
  } catch {
    return false;
  }
}

function leaderCommand(pgid: number): string {
  try {
    return execFileSync("ps", ["-o", "command=", "-p", String(pgid)], { encoding: "utf8" }).trim();
  } catch {
    return "";
  }
}

const LANE_FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 60_000,
  checkMs: 120_000,
  probeMs: 15_000,
  freezeMs: 15_000,
  evaluateMs: 8_000,
  reviewMs: 20_000,
  panelMs: 8_000,
};

function laneWorkerScript(lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      ...(lane === "a" ? [{ kind: "sleep", ms: 2_000 } as FakePiStep] : []),
      { kind: "call-sh", command: lane === "a" ? "printf 'tt-slow\\n' > lane-a.txt" : "printf 'lane b\\n' > lane-b.txt" },
      {
        kind: "call-submit",
        tool: "submit_phase",
        args: {
          // Plan 06n: a payload the resumed round must still hand off (a
          // detail decision needs no ballot, so the phase can still finish).
          decisions: [
            {
              classProposal: "detail",
              choice: `lane ${lane} writes its own marker file`,
              whyItMatters: "the reviewers can tell the lanes apart",
              alternatives: [{ option: "one shared file", consequence: "the lanes would collide" }],
              recommendation: { choice: "one file per lane", reason: "lanes never touch each other's files" },
            },
          ],
          assumptions: [],
          deviations: [],
        },
      },
    ],
  };
}

function laneReviewScript(seat: Reviewer, contractVersion: unknown, delayMs = 0): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      ...(delayMs > 0 ? [{ kind: "sleep", ms: delayMs } as FakePiStep] : []),
      {
        kind: "call-submit",
        tool: "submit_review",
        args: { reviewer: seat, phaseId: "p1", candidateSha: "$TT_CANDIDATE_SHA", contractVersion, correctionStatements: [], findingStatements: [], ballots: [], findings: [] },
      },
    ],
  };
}

function pickVoteScript(seat: Reviewer, round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { inspected: ["a", "b", "c", "d"], round, seat, lane, why: `${seat} picks ${lane}`, loserHad: { yes: false, anchors: [] } } }],
  };
}

/** A second conductor on the stopped run, as `tt resume` starts one: its lane
 * reviews answer at once and every seat picks lane a. Lane workers get no
 * script, so a lane rebuilt after the resume would fail the test. */
function resumeConductor(setup: Awaited<ReturnType<typeof setupConductor>>): Conductor {
  const scripts = new Map<string, string>();
  const scriptFor = (name: string, script: { hello?: unknown; steps: FakePiStep[] }) => {
    if (!scripts.has(name)) scripts.set(name, writeScript(setup.scriptsDir, name, script));
    return scripts.get(name)!;
  };
  const resumed: Conductor = new Conductor({
    runDir: setup.runDir,
    plan: setup.plan as RunPlanFile,
    piCommand: process.execPath,
    piArgsPrefix: [FAKE_PI_PATH],
    stubReviews: true,
    deadlines: LANE_FAST,
    checkLockPath: path.join(setup.runRoot, "check.lock"),
    piEnvFor: (role, agentId) => {
      const laneReview = agentId.match(/^lane-review-(\d+)-([a-z])-([A-Za-z0-9_]+)-/);
      if (role === "reviewer" && laneReview) {
        const K = resumed.state.phase.contract.contractVersion;
        return { FAKE_PI_SCRIPT: scriptFor(`resume-lane-review-${laneReview[3]}`, laneReviewScript(laneReview[3] as Reviewer, K)) };
      }
      const pick = agentId.match(/^(?:pick|revote)-(\d+)-([A-Za-z0-9_]+)-/);
      if (role === "picker" && pick) {
        return { FAKE_PI_SCRIPT: scriptFor(`resume-pick-${pick[2]}`, pickVoteScript(pick[2] as Reviewer, Number(pick[1]), "a")) };
      }
      return undefined;
    },
  });
  return resumed;
}

/** What every clean stop + resume must show: no crash recovery, the same
 * round number, a pick, every review, and the winner's own payload. */
function assertResumedInPlace(setup: Awaited<ReturnType<typeof setupConductor>>, resumed: Conductor): void {
  const phase = resumed.state.phase;
  const records = readLog(runPaths(setup.runDir).events).records;
  assert.ok(
    !records.some((r) => r.kind === "sweep" && (r.event as { reason?: string }).reason === "crash-recovery"),
    "a clean stop records no crash-recovery sweep",
  );
  assert.equal(
    readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "ATTEMPT_INTERRUPTED").length,
    0,
    "a clean stop records no ATTEMPT_INTERRUPTED",
  );
  const stopTs = records.find((r) => r.kind === "stop")?.ts ?? "0";
  assert.deepEqual(
    records.filter((r) => r.kind === "lane_session" && Date.parse(r.ts) > Date.parse(stopTs)),
    [],
    "no lane worker is started again after the resume",
  );
  assert.equal(phase.rounds?.length, 1, "the round number did not advance");
  assert.equal(phase.rounds![0].picked?.lane, "a", "the resumed round picked");
  for (const c of phase.rounds![0].candidates) assert.equal((c.reviews ?? []).length, 3, `${c.lane} has three reviews`);
  // Plan 06n (finding 4): the kept winner's submit_phase payload reaches the
  // hand-off after the resume, as it would from a live build.
  assert.ok(
    phase.decisions.some((d) => d.source === "worker" && d.choice === "lane a writes its own marker file"),
    `the winner's disclosed decision survived the resume: ${phase.decisions.map((d) => d.choice).join(" | ")}`,
  );
  assert.equal(phase.phase, "DONE");
}

test("plan 06n: stop and resume after both lanes submitted keeps both candidates, checks the unchecked one and picks", async () => {
  const slept = `/tmp/tt-06n-wide-slept-${process.pid}`;
  const finalMarker = `/tmp/tt-06n-wide-final-${process.pid}`;
  fs.rmSync(slept, { force: true });
  fs.rmSync(finalMarker, { force: true });
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "two lanes",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      workers: 2,
      // Plan 06n (A5, finding 5): lane a touches a wide path, so its check
      // after the resume must still diff against its lane's start and run
      // the final checks.
      finalChecks: [`echo final >> ${finalMarker}`],
      widePaths: ["lane-a.txt"],
    },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: LANE_FAST,
    workerScript: () => laneWorkerScript("a"),
    laneWorkerScriptFor: (lane) => laneWorkerScript(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReviewScript(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVoteScript(seat as Reviewer, (state.phase.rounds ?? []).length || 1, "a"),
  });
  setup.plan.phases[0].checks = [`if grep -q tt-slow lane-a.txt && [ ! -f ${slept} ]; then touch ${slept}; sleep 60; fi; exit 0`];
  let resumed: Conductor | undefined;
  try {
    await setup.conductor.start();
    await waitFor(
      () =>
        readLog(runPaths(setup.runDir).events).records.some((r) => r.kind === "lane_check_finished" && (r.event as { lane?: string }).lane === "b") &&
        readLog(runPaths(setup.runDir).events).records.some((r) => r.kind === "lane_check_started" && (r.event as { lane?: string }).lane === "a"),
      120_000,
      50,
      setup.runDir,
    );
    await setup.conductor.stop();
    const stoppedRound = setup.conductor.state.phase.rounds![0];
    assert.equal(stoppedRound.picked, undefined, "the round had not picked");
    assert.equal(stoppedRound.candidates.find((c) => c.lane === "a")?.ok, undefined, "lane a's check had not finished");
    assert.equal(stoppedRound.candidates.find((c) => c.lane === "b")?.ok, true, "lane b was checked before the stop");
    const shaA = stoppedRound.candidates.find((c) => c.lane === "a")!.sha!;

    resumed = resumeConductor(setup);
    await resumed.start();
    await waitFor(() => resumed!.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const records = readLog(runPaths(setup.runDir).events).records;
    const stopTs = records.find((r) => r.kind === "stop")?.ts ?? "0";
    const afterStop = records.filter((r) => r.kind === "lane_check_started" && Date.parse(r.ts) > Date.parse(stopTs));
    assert.deepEqual(afterStop.map((r) => (r.event as { lane: string }).lane), ["a"], "only the unchecked lane is checked after resume");
    assertResumedInPlace(setup, resumed);
    // Lane a's start was restored, so its wide-path change still ran the
    // final checks in the resumed round.
    const recordA = JSON.parse(fs.readFileSync(path.join(runPaths(setup.runDir).checks, shaA, "record.json"), "utf8")) as { tier: string };
    assert.equal(recordA.tier, "final", "lane a's resumed check diffed against its own start and ran the final checks");
  } finally {
    if (resumed) await resumed.stop();
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(slept, { force: true });
    fs.rmSync(finalMarker, { force: true });
  }
});

test("plan 06n: a stop during the lane reviews resumes the same round with both kept candidates", async () => {
  const setup = await setupConductor({
    phase: { id: "p1", goal: "two lanes", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], workers: 2 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: LANE_FAST,
    workerScript: () => laneWorkerScript("b"),
    laneWorkerScriptFor: (lane) => laneWorkerScript(lane),
    // The first conductor's reviews are slow, so the stop lands while they run.
    laneReviewerScriptFor: (seat, _candidate, state) => laneReviewScript(seat as Reviewer, state.phase.contract.contractVersion, 30_000),
    pickScriptFor: (seat, state) => pickVoteScript(seat as Reviewer, (state.phase.rounds ?? []).length || 1, "a"),
  });
  let resumed: Conductor | undefined;
  try {
    await setup.conductor.start();
    // 09-2: reviews now start at submission, so a running reviewer no longer
    // implies both checks were recorded. Wait for the recorded check results
    // themselves (not the `lane_check_finished` log line, which precedes
    // CANDIDATE_CHECKED by the lock release), then for a review in flight.
    await waitFor(
      () => {
        const round = setup.conductor.state.phase.rounds?.[0];
        return round !== undefined && round.candidates.length === 2 && round.candidates.every((c) => c.ok === true);
      },
      120_000,
      50,
      setup.runDir,
    );
    await waitFor(() => setup.conductor.agentPgids.some((a) => a.role === "reviewer"), 30_000, 50, setup.runDir);
    await setup.conductor.stop();
    const stoppedRound = setup.conductor.state.phase.rounds![0];
    assert.equal(stoppedRound.picked, undefined, "the round had not picked");
    assert.ok(stoppedRound.candidates.every((c) => c.ok === true), "both lanes were checked before the stop");

    resumed = resumeConductor(setup);
    await resumed.start();
    await waitFor(() => resumed!.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const records = readLog(runPaths(setup.runDir).events).records;
    const stopTs = records.find((r) => r.kind === "stop")?.ts ?? "0";
    assert.deepEqual(
      records.filter((r) => r.kind === "lane_check_started" && Date.parse(r.ts) > Date.parse(stopTs)),
      [],
      "no checked candidate is checked again",
    );
    assertResumedInPlace(setup, resumed);
  } finally {
    if (resumed) await resumed.stop();
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 06g/ODP-3: a stop during the checks signals the run's own check process group", async () => {
  const markerDir = fs.mkdtempSync("/tmp/tt-check-stop-");
  const pgidFile = path.join(markerDir, "pgid");
  const startedFile = path.join(markerDir, "started");
  const armed = path.join(markerDir, "armed");
  // While `armed` is absent this check exits at once (the base baseline's
  // run); once the worker has created it, the check records its own process
  // group and sleeps well past the stop. `$$` is the shell's pid, which
  // runCommand makes the group leader, so `$$` IS the pgid.
  const check = `sh -c '[ -f ${armed} ] && { echo $$ > ${pgidFile}; touch ${startedFile}; sleep 120; }; exit 0'`;

  const setup = await setupConductor({
    // The check runs on its own, after the freeze: the worker has no other
    // job than arming it and submitting.
    checks: [check],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `touch ${armed}` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 20_000, checkMs: 120_000 },
  });

  await setup.conductor.start();
  let pgid: number | undefined;
  try {
    await waitFor(() => fs.existsSync(startedFile), 90_000, 20, setup.runDir);
    pgid = Number.parseInt(fs.readFileSync(pgidFile, "utf8").trim(), 10);
    assert.ok(Number.isFinite(pgid) && pgid > 0, "the check recorded its process group");
    assert.equal(pgidAlive(pgid), true, "the check is running before the stop");
    assert.match(leaderCommand(pgid), /sleep 120/, "the recorded group is the check's own");
    // It is genuinely long-running: it is still there a moment later, so the
    // assertion after `stop()` is about the stop, not about a command that
    // happened to finish.
    await new Promise((r) => setTimeout(r, 1500));
    assert.equal(pgidAlive(pgid), true, "the check is still running a second later");

    // A stop must signal the check's own group, like every other stage's.
    await setup.conductor.stop();
    // A short, bounded wait — not `waitFor`, whose load-tolerant floor is
    // 150 s and would let the orphan's own `sleep` end the test for us.
    const deadline = Date.now() + 5_000;
    while (pgidAlive(pgid) && Date.now() < deadline) await new Promise((r) => setTimeout(r, 50));
    assert.equal(pgidAlive(pgid!), false, "the check's process group is gone after the stop");
    assert.equal(processAlive(pgid!), false, "and so is its leader");
  } finally {
    // Best effort: never leave the sleeper behind for the next test.
    if (pgid !== undefined && pgidAlive(pgid)) {
      try {
        process.kill(-pgid, "SIGKILL");
      } catch {
        // already gone
      }
    }
    await setup.conductor.stop().catch(() => undefined);
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(markerDir, { recursive: true, force: true });
  }
});
