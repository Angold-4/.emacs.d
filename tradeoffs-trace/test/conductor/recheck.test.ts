// Plan 06j (A3): `tt recheck <run> --reason "<text>"` re-runs a frozen
// candidate's own checks after a failure the owner believes is the machine.
// It dispatches no worker, spends no repair round, and a recheck that fails
// again follows the ordinary check-failure path — it never waives a failure.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

/** Runs the real CLI asynchronously: the conductor runs in THIS process, so a
 * synchronous `execFileSync` would block the event loop and the inbox poll
 * that applies the command. */
function runCli(args: string[]): Promise<{ stdout: string; stderr: string; code: number }> {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, TT_NOTIFY_COMMAND: ":" } });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (d) => (stdout += String(d)));
    child.stderr.on("data", (d) => (stderr += String(d)));
    child.on("close", (code) => resolve({ stdout, stderr, code: code ?? 0 }));
  });
}

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  reviewMs: 20_000,
};

function submitPhaseStep() {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

/** A stub reviewer: the harness's `stubReviews` default applies it. */
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

function eventTypes(runDir: string): string[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => (r.event as { type: string }).type);
}

function eventsOfType(runDir: string, type: string): Array<Record<string, unknown>> {
  return readEvents(runDir)
    .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === type)
    .map((r) => r.event as Record<string, unknown>);
}

/** A check that fails the first time it sees the worker's marker and passes
 * afterwards. The baseline (no marker) passes; the candidate's first run
 * fails; the recheck passes. */
function failOnceCheck(counter: string): string {
  return `if grep -q TT_RECHECK_FAIL README.md; then n=$(cat ${counter} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${counter}; if [ "$n" -le 1 ]; then echo "load-only timing failure"; exit 1; fi; fi; exit 0`;
}

test("plan 06j: tt recheck re-runs the frozen candidate's checks without a worker or a round", async () => {
  const counter = `/tmp/tt-recheck-counter-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "do the thing",
      acceptance: ["it works"],
      checks: [failOnceCheck(counter)],
      boundaries: [],
      reserved: [],
      rounds: 1,
    },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "printf 'TT_RECHECK_FAIL\\n' >> README.md" }, submitPhaseStep()],
    }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    // With one round, the first check failure parks the phase on the owner
    // instead of dispatching a repair worker.
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 50, setup.runDir);
    const candidateSha = setup.conductor.state.phase.candidate!.sha;
    const usedBefore = setup.conductor.state.phase.repairRoundsUsed;
    assert.equal(eventTypes(setup.runDir).filter((t) => t === "REPAIR_ATTEMPT_STARTED").length, 0, "no repair round before the recheck");

    const out = await runCli(["recheck", setup.runDir, "--reason", "the check was load-only, the machine was busy"]);
    assert.match(out.stdout, /recheck requested/, out.stdout);

    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);

    const rechecks = eventsOfType(setup.runDir, "RECHECK_REQUESTED");
    assert.equal(rechecks.length, 1, "one RECHECK_REQUESTED is recorded");
    assert.equal(rechecks[0].candidateSha, candidateSha, "the recheck names the same sha");
    assert.match(String(rechecks[0].reason), /load-only/);
    assert.equal(setup.conductor.state.phase.candidate?.sha, candidateSha, "the same candidate is reviewed");
    assert.equal(eventTypes(setup.runDir).filter((t) => t === "REPAIR_ATTEMPT_STARTED").length, 0, "the recheck dispatches no worker");
    assert.equal(setup.conductor.state.phase.repairRoundsUsed, usedBefore, "the recheck spends no round");
    assert.ok(eventTypes(setup.runDir).includes("CHECKS_PASSED"), "the recheck's own pass is recorded");
    assert.ok(eventTypes(setup.runDir).includes("REVIEWING") || setup.conductor.state.phase.phase === "DONE", "the phase proceeds to review");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("plan 06j: tt recheck is refused unless the current candidate's checks just failed", async () => {
  // (1) checks passed: an evidence item parks the phase with green checks.
  const evidenceItems = {
    architecture: [],
    requirements: [
      { id: "R1", title: "R1 proves it", text: "R1 proves it", arch: [], verify: ['test "R1 proves it"'] },
      { id: "R2", title: "R2 owner run", text: "the owner live run is recorded", arch: [], verify: ["evidence"] },
    ],
    constraints: [],
  };
  const green = await setupConductor({
    items: evidenceItems,
    phaseChecks: ["printf 'ok 1 - R1 proves it\\n'"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "mkdir -p src/core && printf 'x\\n' > src/core/rounds.ts" },
        {
          kind: "call-submit",
          tool: "submit_coverage",
          args: {
            items: [
              { id: "R1", status: "done", where: ["src/core/rounds.ts:1"], tests: ["R1 proves it"] },
              { id: "R2", status: "done", where: [], tests: [] },
            ],
            arch: [],
          },
        },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
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
            items: [
              { id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1" },
              { id: "R2", verdict: "met", evidence: "src/core/rounds.ts:1" },
            ],
            arch: [],
          },
        },
      ],
    }),
  });
  try {
    await green.conductor.start();
    await waitFor(() => green.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 50, green.runDir);
    const out = await runCli(["recheck", green.runDir, "--reason", "try again"]);
    assert.match(out.stdout, /recheck refused: the current candidate's checks passed/, out.stdout);
    assert.equal(out.code, 1, "a refusal is a non-zero exit");
  } finally {
    await green.conductor.stop();
    cleanupDir(green.runRoot);
    cleanupDir(green.scriptsDir);
  }

  // (2) a worker attempt is running with no frozen candidate yet: the
  // initial attempt is held so the phase stays IMPLEMENTING. There is
  // nothing to recheck, so it is refused.
  const busy = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  try {
    await busy.conductor.start();
    await waitFor(() => busy.conductor.state.phase.phase === "IMPLEMENTING", 90_000, 50, busy.runDir);
    const out = await runCli(["recheck", busy.runDir, "--reason", "the machine was busy"]);
    assert.match(out.stdout, /recheck refused: the phase has no frozen candidate yet/, out.stdout);
    assert.equal(out.code, 1);
  } finally {
    await busy.conductor.stop();
    cleanupDir(busy.runRoot);
    cleanupDir(busy.scriptsDir);
  }

  // (3) a newer candidate exists: a hand-written command naming another sha
  // is rejected by the conductor, so the file path cannot bypass the CLI.
  const counter3 = `/tmp/tt-recheck-newer-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter3, { force: true });
  const newer = await setupConductor({
    phase: {
      id: "p1",
      goal: "do the thing",
      acceptance: ["it works"],
      checks: [failOnceCheck(counter3)],
      boundaries: [],
      reserved: [],
      rounds: 1,
    },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "printf 'TT_RECHECK_FAIL\\n' >> README.md" }, submitPhaseStep()],
    }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  try {
    await newer.conductor.start();
    await waitFor(() => newer.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 50, newer.runDir);
    const inbox = path.join(newer.runDir, "inbox");
    fs.mkdirSync(inbox, { recursive: true });
    fs.writeFileSync(
      path.join(inbox, "cmd-recheck-stale.json"),
      JSON.stringify({
        type: "recheck",
        reason: "stale",
        binding: { runId: newer.conductor.state.phase.runId, phaseId: "p1", candidateSha: "0000000000000000000000000000000000000000", contractVersion: newer.conductor.state.phase.contract.contractVersion },
      }),
    );
    await waitFor(() => readEvents(newer.runDir).some((r) => r.kind === "command_rejected"), 90_000, 50, newer.runDir);
    const rejected = readEvents(newer.runDir).find((r) => r.kind === "command_rejected")!;
    assert.match(String((rejected.event as { reason: string }).reason), /newer candidate exists/);
    assert.equal(eventTypes(newer.runDir).filter((t) => t === "RECHECK_REQUESTED").length, 0, "the stale recheck is never applied");
  } finally {
    await newer.conductor.stop();
    cleanupDir(newer.runRoot);
    cleanupDir(newer.scriptsDir);
    fs.rmSync(counter3, { force: true });
  }
});

test("plan 06j: a recheck that fails again leaves the phase where a failed check leaves it", async () => {
  const counter = `/tmp/tt-recheck-refail-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  // The check fails every time the marker is present: the recheck cannot
  // waive the failure.
  const alwaysFail = `if grep -q TT_RECHECK_FAIL README.md; then n=$(cat ${counter} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${counter}; exit 1; fi; exit 0`;
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "do the thing",
      acceptance: ["it works"],
      checks: [alwaysFail],
      boundaries: [],
      reserved: [],
      rounds: 1,
    },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "printf 'TT_RECHECK_FAIL\\n' >> README.md" }, submitPhaseStep()],
    }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 50, setup.runDir);
    const out = await runCli(["recheck", setup.runDir, "--reason", "it looked like the machine"]);
    assert.match(out.stdout, /recheck requested/, out.stdout);
    // The recheck fails again: a second CHECKS_FAILED, back to AWAITING_OWNER,
    // and never a review of the failing candidate.
    await waitFor(() => eventTypes(setup.runDir).filter((t) => t === "CHECKS_FAILED").length >= 2, 90_000, 50, setup.runDir);
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.checks?.passed, false, "the failure stands");
    assert.equal(eventTypes(setup.runDir).filter((t) => t === "REPAIR_ATTEMPT_STARTED").length, 0, "the exhausted budget dispatches no repair worker");
    assert.ok(!eventTypes(setup.runDir).includes("CHECKS_PASSED"), "a failing recheck never passes the checks");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("plan 06j: tt recheck stops a running repair attempt and re-runs the checks on the frozen candidate", async () => {
  // The case the command exists for: after CHECKS_FAILED the conductor starts
  // a repair attempt by itself, and the owner rechecks while that attempt has
  // not submitted. The worker is stopped, the round it charged is given back,
  // and the checks run again on the SAME frozen candidate.
  const counter = `/tmp/tt-recheck-impl-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "do the thing",
      acceptance: ["it works"],
      checks: [failOnceCheck(counter)],
      boundaries: [],
      reserved: [],
      rounds: 2,
    },
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'TT_RECHECK_FAIL\\n' >> README.md" }, submitPhaseStep()] }
        : { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] },
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => eventTypes(setup.runDir).includes("REPAIR_ATTEMPT_STARTED") && setup.conductor.state.phase.phase === "IMPLEMENTING",
      90_000,
      50,
      setup.runDir,
    );
    const candidateSha = setup.conductor.state.phase.candidate!.sha;
    assert.equal(setup.conductor.state.phase.repairRoundsUsed, 1, "the auto-repair charged one round");

    const out = await runCli(["recheck", setup.runDir, "--reason", "the check was killed by the machine"]);
    assert.match(out.stdout, /recheck requested/, out.stdout);

    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.candidate?.sha, candidateSha, "the same frozen candidate is reviewed");
    assert.equal(setup.conductor.state.phase.repairRoundsUsed, 0, "stopping the repair gives the round back");
    assert.equal(eventTypes(setup.runDir).filter((t) => t === "REPAIR_ATTEMPT_STARTED").length, 1, "no second repair attempt");
    assert.equal(eventsOfType(setup.runDir, "RECHECK_REQUESTED").length, 1, "the recheck is recorded");
    assert.ok(eventTypes(setup.runDir).includes("CHECKS_PASSED"), "the recheck's pass is recorded");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("plan 06j: a failed recheck from IMPLEMENTING still dispatches the next repair worker", async () => {
  // M-3: the stopped attempt's inFlight.dispatch_worker must not survive the
  // recheck, or the next repair's IMPLEMENTING state dispatches nothing.
  const counter = `/tmp/tt-recheck-stall-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  const alwaysFail = `if grep -q TT_RECHECK_FAIL README.md; then n=$(cat ${counter} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${counter}; exit 1; fi; exit 0`;
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "do the thing",
      acceptance: ["it works"],
      checks: [alwaysFail],
      boundaries: [],
      reserved: [],
      rounds: 3,
    },
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'TT_RECHECK_FAIL\\n' >> README.md" }, submitPhaseStep()] }
        : { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] },
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => eventTypes(setup.runDir).includes("REPAIR_ATTEMPT_STARTED") && setup.conductor.state.phase.phase === "IMPLEMENTING",
      90_000,
      50,
      setup.runDir,
    );
    const before = eventsOfType(setup.runDir, "ACTION_STARTED").filter((e) => e.action === "dispatch_worker").length;
    const out = await runCli(["recheck", setup.runDir, "--reason", "the machine was busy"]);
    assert.match(out.stdout, /recheck requested/, out.stdout);
    // The recheck fails again and the budget remains, so a fresh worker must
    // actually be dispatched; without the inFlight fix the phase stalls here.
    await waitFor(
      () =>
        setup.conductor.state.phase.phase === "IMPLEMENTING" &&
        eventsOfType(setup.runDir, "ACTION_STARTED").filter((e) => e.action === "dispatch_worker").length > before,
      90_000,
      50,
      setup.runDir,
    );
    assert.ok(eventTypes(setup.runDir).filter((t) => t === "REPAIR_ATTEMPT_STARTED").length >= 2, "the failed recheck leads to another repair");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("plan 06j: a recheck discards the stopped repair worker's tracked and untracked writes", async () => {
  // A-9: after the recheck stops the worker, the worktree is reset to the
  // frozen candidate, so the next repair never inherits cancelled writes.
  const counter = `/tmp/tt-recheck-clean-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  const failOnMarker = "if grep -q TT_RECHECK_FAIL README.md; then exit 1; fi; exit 0";
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "do the thing",
      acceptance: ["it works"],
      checks: [failOnMarker],
      boundaries: [],
      reserved: [],
      rounds: 3,
    },
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'TT_RECHECK_FAIL\\n' >> README.md" }, submitPhaseStep()] }
        : attempt === 2
          ? {
              hello: defaultWorkerHello(),
              steps: [
                { kind: "call-sh", command: "printf 'CANCELLED-WRITE\\n' >> README.md; printf 'x\\n' > untracked.txt" },
                { kind: "hang-until-abort" },
              ],
            }
          : { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] },
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  const worktree = runPaths(setup.runDir).worktree;
  try {
    await setup.conductor.start();
    await waitFor(
      () => eventTypes(setup.runDir).includes("REPAIR_ATTEMPT_STARTED") && setup.conductor.state.phase.phase === "IMPLEMENTING",
      90_000,
      50,
      setup.runDir,
    );
    await waitFor(() => fs.existsSync(path.join(worktree, "untracked.txt")), 90_000, 50, setup.runDir);
    const out = await runCli(["recheck", setup.runDir, "--reason", "the check was killed by the machine"]);
    assert.match(out.stdout, /recheck requested/, out.stdout);
    // The worktree is reset to the frozen candidate: neither write survives.
    await waitFor(
      () => !fs.existsSync(path.join(worktree, "untracked.txt")) && !fs.readFileSync(path.join(worktree, "README.md"), "utf8").includes("CANCELLED-WRITE"),
      90_000,
      50,
      setup.runDir,
    );
    // The recheck fails again (the marker is still present), so the next
    // repair starts from the clean frozen candidate.
    await waitFor(
      () => setup.conductor.state.phase.phase === "IMPLEMENTING" && eventTypes(setup.runDir).filter((t) => t === "REPAIR_ATTEMPT_STARTED").length >= 2,
      90_000,
      50,
      setup.runDir,
    );
    assert.ok(!fs.existsSync(path.join(worktree, "untracked.txt")), "the untracked write is gone");
    assert.ok(!fs.readFileSync(path.join(worktree, "README.md"), "utf8").includes("CANCELLED-WRITE"), "the tracked write is gone");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

/** Plan 06j (OD-5(3)): the lane helpers the two-lane recheck test needs. */
function laneWorker(lane: string, round: number): { hello: unknown; steps: Array<{ kind: string; [key: string]: unknown }> } {
  if (round >= 2) {
    return {
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'DIRTY-${lane}\\n' > dirty-${lane}.txt; printf 'DIRTY-TRACKED-${lane}\\n' >> README.md` },
        { kind: "hang-until-abort" },
      ],
    };
  }
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane}\\n' > lane-${lane}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

function laneReview(seat: Reviewer, contractVersion: unknown): { hello: unknown; steps: Array<{ kind: string; [key: string]: unknown }> } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      {
        kind: "call-submit",
        tool: "submit_review",
        args: { reviewer: seat, phaseId: "p1", candidateSha: "$TT_CANDIDATE_SHA", contractVersion, correctionStatements: [], findingStatements: [] },
      },
    ],
  };
}

function pickVote(seat: Reviewer, round: number, lane: string, why: string): { hello: unknown; steps: Array<{ kind: string; [key: string]: unknown }> } {
  return { hello: { role: "picker", tools: ROLE_TOOLS.picker }, steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { round, seat, lane, why } }] };
}

function liveRound(state: State): number {
  return state.phase.rounds?.length ?? 1;
}

test("plan 06j: a recheck whose worktree reset throws is refused, with the reason reported", async () => {
  // OD-5(2): a recheck verdict never rests on an unknown worktree.
  const counter = `/tmp/tt-recheck-refuse-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  const failOnMarker = "if grep -q TT_RECHECK_FAIL README.md; then exit 1; fi; exit 0";
  const setup = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: [failOnMarker], boundaries: [], reserved: [], rounds: 3 },
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'TT_RECHECK_FAIL\\n' >> README.md" }, submitPhaseStep()] }
        : { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] },
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  const worktree = runPaths(setup.runDir).worktree;
  try {
    await setup.conductor.start();
    await waitFor(
      () => eventTypes(setup.runDir).includes("REPAIR_ATTEMPT_STARTED") && setup.conductor.state.phase.phase === "IMPLEMENTING",
      90_000,
      50,
      setup.runDir,
    );
    // Make the worktree unremovable: a file inside a directory with no write
    // permission defeats both `git worktree remove` and the rmSync fallback.
    fs.writeFileSync(path.join(worktree, "blocker.txt"), "x");
    fs.chmodSync(worktree, 0o500);
    const out = await runCli(["recheck", setup.runDir, "--reason", "the machine was busy"]);
    assert.match(out.stdout, /recheck refused: could not reset the worker worktree/, out.stdout);
    assert.equal(out.code, 1, "a refusal is a non-zero exit");
    assert.equal(eventsOfType(setup.runDir, "RECHECK_REQUESTED").length, 0, "no RECHECK_REQUESTED is recorded");
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "recheck_reset_failed"), "the refusal is owner-visible in the log");
    assert.equal(setup.conductor.state.phase.phase, "IMPLEMENTING", "the run stays as it was");
  } finally {
    try {
      fs.chmodSync(worktree, 0o700);
    } catch {
      // best effort
    }
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("plan 06j: a recheck stops every lane worker and resets every lane worktree", async () => {
  // OD-5(3): with workers > 1, all live lane workers stop and all lane
  // worktrees reset before the checks re-run. The round checks pass and a
  // winner is handed off; the FINAL check then fails and starts a two-lane
  // repair round (the lane checks run only the round tier, so the final check
  // is what makes the phase repair). Both repair lane workers write dirty
  // files and hang.
  const setup = await setupConductor({
    phase: { id: "p1", goal: "build two candidates", acceptance: ["it works"], checks: ["true"], finalChecks: ["false"], boundaries: [], reserved: [], workers: 2 },
    workerScript: () => laneWorker("a", 1),
    laneWorkerScriptFor: (lane, round) => laneWorker(lane, round),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", `${seat} prefers`),
    deadlines: FAST,
  });
  const laneA = path.join(setup.runDir, "worktrees", "lane-a");
  const laneB = path.join(setup.runDir, "worktrees", "lane-b");
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.phase === "IMPLEMENTING" && fs.existsSync(path.join(laneA, "dirty-a.txt")) && fs.existsSync(path.join(laneB, "dirty-b.txt")),
      120_000,
      50,
      setup.runDir,
    );
    const out = await runCli(["recheck", setup.runDir, "--reason", "the final check was killed by the machine"]);
    assert.match(out.stdout, /recheck requested/, out.stdout);
    await waitFor(() => !fs.existsSync(path.join(laneA, "dirty-a.txt")) && !fs.existsSync(path.join(laneB, "dirty-b.txt")), 90_000, 50, setup.runDir);
    assert.ok(!fs.existsSync(path.join(laneA, "dirty-a.txt")), "lane a's untracked write is gone");
    assert.ok(!fs.existsSync(path.join(laneB, "dirty-b.txt")), "lane b's untracked write is gone");
    assert.ok(!fs.readFileSync(path.join(laneA, "README.md"), "utf8").includes("DIRTY-TRACKED-a"), "lane a's tracked write is gone");
    assert.ok(!fs.readFileSync(path.join(laneB, "README.md"), "utf8").includes("DIRTY-TRACKED-b"), "lane b's tracked write is gone");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
