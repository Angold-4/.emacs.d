// Plan 05i: the environment preflight and environment (126/127) failures.
//
// 1. A phase whose check names a command missing from the conductor's PATH
//    reaches ENV_BLOCKED before any baseline and before any agent; after the
//    command is made available, a fresh conductor (the `tt resume` path)
//    passes the preflight and the phase proceeds.
// 2. A check that exits 127 during CHECKING moves the run to ENV_BLOCKED with
//    no repair, no finding and no `checks failed`.
// 3. A baseline that exits 127 is not written; a shared baseline record with
//    exit 127 on disk is ignored and the baseline re-runs.
// 4. `tt program start`/`resume`/`retry` refuse, naming the missing tool, and
//    create no run.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomBytes, randomUUID } from "node:crypto";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { Conductor, contractVersionFor, createRun, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import { EventLog } from "../../src/effects/log.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { baselineHasEnvironmentFailure, baselineKey, parseBaseline } from "../../src/core/test-failures.ts";
import { appendProgramEvent, createProgram, observeRun } from "../../src/program.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import { buildView } from "../../src/view.ts";
import { renderStatusText } from "../../src/render.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  FAKE_PI_PATH,
  makeRepo,
  readEvents,
  setupConductor,
  waitFor,
} from "./harness.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

const FAST = {
  abortGraceMs: 200,
  termGraceMs: 200,
  helloTimeoutMs: 5_000,
  checkMs: 20_000,
  freezeMs: 15_000,
  workerAttemptMs: 30_000,
  reviewMs: 10_000,
};

function submitPhaseStep() {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

function eventTypes(runDir: string): string[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => (r.event as { type: string }).type);
}

/** Builds a fresh Conductor against an existing run (the `tt resume` path),
 * with the same fake-pi wiring the harness uses and stub reviews. */
function resumeConductor(runDir: string, plan: RunPlanFile, scriptsDir: string, preflightEnv: NodeJS.ProcessEnv): Conductor {
  const workerScript = path.join(scriptsDir, "worker.json");
  const reviewerScript = path.join(scriptsDir, "reviewer-resume.json");
  if (!fs.existsSync(reviewerScript)) {
    fs.writeFileSync(
      reviewerScript,
      JSON.stringify({
        hello: { role: "reviewer", tools: ROLE_TOOLS.reviewer },
        steps: [
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer: "$TT_REVIEWER",
              phaseId: "p1",
              candidateSha: "$TT_CANDIDATE_SHA",
              contractVersion: contractVersionFor(plan.phases[0]),
              correctionStatements: [],
              findingStatements: [],
            },
          },
        ],
      }),
    );
  }
  return new Conductor({
    runDir,
    plan,
    piCommand: process.execPath,
    piArgsPrefix: [FAKE_PI_PATH],
    piEnvFor: (role) => (role === "worker" ? { FAKE_PI_SCRIPT: workerScript } : { FAKE_PI_SCRIPT: reviewerScript }),
    deadlines: FAST,
    stubReviews: true,
    preflightEnv,
  });
}

test("env-preflight: a missing check tool blocks before the baseline and any agent, and resume proceeds once it is available", async () => {
  const toolDir = fs.mkdtempSync("/tmp/tt-envtool-");
  const originalPath = process.env.PATH;
  // A directory on PATH that does not (yet) hold the tool. The original PATH
  // is kept so git/node still resolve.
  process.env.PATH = `${toolDir}:${originalPath}`;
  const missing = "tt-env-probe-tool";
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      checks: [`${missing} --version`],
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
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
            },
          },
        ],
      }),
      deadlines: FAST,
    });
    await setup.conductor.start();
    const state = setup.conductor.state;
    assert.equal(state.run, "ENV_BLOCKED");
    assert.equal(state.phase.phase, "READY", "no baseline and no attempt started");
    assert.deepEqual(state.phase.env?.blocked?.missing, [missing]);
    assert.equal(state.phase.env?.blocked?.kind, "preflight");
    assert.match(state.phase.env?.blocked?.path ?? "", new RegExp(toolDir));
    assert.deepEqual(eventTypes(setup.runDir).filter((t) => t === "ATTEMPT_STARTED"), [], "no agent was dispatched");
    assert.equal(
      fs.existsSync(path.join(runPaths(setup.runDir).checks, "base", "baseline.json")),
      false,
      "no baseline was written",
    );
    const view = buildView(setup.runDir, setup.plan, false);
    assert.equal(
      view.envBlocked,
      `env blocked · ${missing} not found on PATH (${process.env.PATH})`,
    );
    const statusText = renderStatusText(setup.runDir, setup.conductor.state, view, { missing: [], tooShort: [] });
    assert.ok(statusText.includes(`env blocked · ${missing} not found on PATH`));
    assert.ok(statusText.includes(`env  ${missing} (not found)`));

    // Make the tool available exactly as fixing the environment would.
    const tool = path.join(toolDir, missing);
    fs.writeFileSync(tool, "#!/bin/sh\nexit 0\n");
    fs.chmodSync(tool, 0o755);

    // `tt resume`: a fresh conductor re-runs the preflight, unblocks and
    // proceeds to DONE.
    const resumed = resumeConductor(setup.runDir, setup.plan, setup.scriptsDir, process.env);
    try {
      await resumed.start();
      await waitFor(() => resumed.state.phase.phase === "DONE", 90_000, undefined, setup.runDir);
      assert.deepEqual(eventTypes(setup.runDir).filter((t) => t === "ENV_PREFLIGHT_FAILED").length, 1);
      assert.ok(eventTypes(setup.runDir).includes("RUN_RESUMED"));
      // The resolved path is recorded in the status at start.
      const doneText = renderStatusText(setup.runDir, resumed.state, buildView(setup.runDir, setup.plan, false), { missing: [], tooShort: [] });
      assert.ok(doneText.includes(`env  ${missing} ${tool}`));
    } finally {
      await resumed.stop();
    }
  } finally {
    process.env.PATH = originalPath;
    if (setup) {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
    cleanupDir(toolDir);
  }
});

test("env-checks: a check that exits 127 moves the run to ENV_BLOCKED with no repair, finding or checks-failed", async () => {
  const counter = `/tmp/tt-env-127-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  // Run 1 is the base baseline (exit 0); run 2, the candidate's check, exits
  // 127. The command's own executable (sh, echo, exit) is on PATH, so the
  // preflight passes and the failure happens at CHECKING.
  const command =
    `n=$(cat ${counter} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${counter};` +
    ` if [ $n -ge 2 ]; then exit 127; fi`;
  const setup = await setupConductor({
    checks: [command],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: () => ({ hello: defaultReviewerHello(), steps: [] }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.run === "ENV_BLOCKED", 60_000, undefined, setup.runDir);
    const state = setup.conductor.state;
    assert.equal(state.phase.phase, "CHECKING", "the phase is preserved so resume re-runs the checks");
    assert.equal(state.phase.env?.blocked?.kind, "check");
    assert.equal(state.phase.env?.blocked?.stage, "checks");
    assert.equal(state.phase.env?.blocked?.exitCode, 127);
    assert.match(state.phase.env?.blocked?.command ?? "", /cat/);
    // The environment failure never becomes a code failure.
    const types = eventTypes(setup.runDir);
    assert.ok(!types.includes("CHECKS_FAILED"), "no `checks failed` was recorded");
    assert.ok(!types.includes("FINDING_RAISED"), "no finding was raised");
    assert.ok(!types.includes("REPAIR_ATTEMPT_STARTED"), "no repair attempt started");
    assert.equal(state.phase.repairRoundsUsed, 0);
    assert.equal(state.phase.checks, undefined);
    assert.equal(state.phase.findings.length, 0);
    // The status displays the command and the exit.
    const view = buildView(setup.runDir, setup.plan, false);
    assert.match(view.envBlocked ?? "", /env blocked · .* exit 127/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("env-baseline: a baseline that exits 127 is not written, and a shared exit-127 record is ignored so the baseline re-runs", async () => {
  // Part A: the baseline's own check exits 127 — no record at all.
  const setup = await setupConductor({
    checks: ["sh -c 'exit 127'"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: () => ({ hello: defaultReviewerHello(), steps: [] }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.run === "ENV_BLOCKED", 60_000, undefined, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "BASELINE");
    assert.equal(setup.conductor.state.phase.env?.blocked?.stage, "baseline");
    assert.equal(
      fs.existsSync(path.join(runPaths(setup.runDir).checks, "base", "baseline.json")),
      false,
      "a 127 baseline must not be written",
    );
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }

  // Part B: a sibling program already published a baseline whose command
  // exited 127 (a record from before this change). It is ignored on read, so
  // this node re-runs the baseline rather than excusing its checks against it.
  const setup2 = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
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
          },
        },
      ],
    }),
    deadlines: FAST,
  });
  try {
    const programId = "prog-env-shared";
    fs.writeFileSync(path.join(setup2.runDir, "program.json"), JSON.stringify({ programId, node: "n1" }));
    const tree = execFileSync("git", ["-C", setup2.repo.dir, "rev-parse", "HEAD^{tree}"], { encoding: "utf8" }).trim();
    const baseSha = execFileSync("git", ["-C", setup2.repo.dir, "rev-parse", "HEAD"], { encoding: "utf8" }).trim();
    const key = baselineKey(tree, ["true"]);
    const shared = path.join(path.dirname(setup2.runDir), "programs", programId, "baselines", key);
    fs.mkdirSync(shared, { recursive: true });
    fs.writeFileSync(
      path.join(shared, "baseline.json"),
      JSON.stringify({
        baseSha,
        tree,
        key,
        at: new Date().toISOString(),
        commands: [{ command: "true", exitCode: 127, signal: null, timedOut: false, durationMs: 1, failures: [] }],
        failures: [],
      }),
    );
    await setup2.conductor.start();
    await waitFor(() => setup2.conductor.state.phase.phase === "DONE", 90_000, undefined, setup2.runDir);
    const local = parseBaseline(
      JSON.parse(fs.readFileSync(path.join(runPaths(setup2.runDir).checks, "base", "baseline.json"), "utf8")),
    );
    assert.ok(local, "the baseline re-ran and wrote its own record");
    assert.equal(local!.commands[0].exitCode, 0, "the exit-127 shared record was ignored and the baseline re-ran");
    assert.equal(baselineHasEnvironmentFailure(local), false);
  } finally {
    await setup2.conductor.stop();
    cleanupDir(setup2.runRoot);
    cleanupDir(setup2.scriptsDir);
  }
});

test("env-budget: a budget-paused run resumed without its tool reaches ENV_BLOCKED, not a crash", async () => {
  const toolDir = fs.mkdtempSync("/tmp/tt-budget-tool-");
  const originalPath = process.env.PATH;
  const tool = "tt-budget-tool";
  const toolPath = path.join(toolDir, tool);
  fs.writeFileSync(toolPath, "#!/bin/sh\nexit 0\n");
  fs.chmodSync(toolPath, 0o755);
  process.env.PATH = `${toolDir}:${originalPath}`;
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      checks: [`${tool} --check`],
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
      reviewerScriptFor: () => ({ hello: defaultReviewerHello(), steps: [] }),
      deadlines: FAST,
    });
    // Pause the run for budget deterministically in the log, before the
    // conductor starts: relying on the wall-clock budget timer was flaky
    // under load (the timer is re-armed on every event and never fires once
    // the remaining budget is consumed).
    const preLog = new EventLog(runPaths(setup.runDir).events);
    preLog.append("event", { type: "RUN_BUDGET_EXCEEDED" });
    preLog.close();
    await setup.conductor.start();
    assert.equal(setup.conductor.state.run, "RUN_PAUSED_BUDGET", "the run starts paused for budget");
    await setup.conductor.stop();

    // The tool disappears before the resume; `tt resume` must reach
    // ENV_BLOCKED with the visible reason, not throw a rejected event.
    fs.rmSync(toolPath, { force: true });
    const resumed = resumeConductor(setup.runDir, setup.plan, setup.scriptsDir, process.env);
    await resumed.start();
    assert.equal(resumed.state.run, "ENV_BLOCKED", "a budget-paused resume with a missing tool must not crash");
    assert.equal(resumed.state.phase.env?.blocked?.kind, "preflight");
    assert.deepEqual(resumed.state.phase.env?.blocked?.missing, [tool]);
    const view = buildView(setup.runDir, setup.plan, false);
    assert.match(view.envBlocked ?? "", new RegExp(`${tool} not found on PATH`));
    assert.ok(
      !readEvents(setup.runDir).some((r) => r.kind === "rejected"),
      "no event was rejected (the conductor did not crash on the missing row)",
    );

    // Finding M-6: once the tool is back, clearing the block restores the
    // budget pause it was entered from, never RUN_ACTIVE.
    fs.writeFileSync(toolPath, "#!/bin/sh\nexit 0\n");
    fs.chmodSync(toolPath, 0o755);
    const reResumed = resumeConductor(setup.runDir, setup.plan, setup.scriptsDir, process.env);
    await reResumed.start();
    assert.equal(reResumed.state.run, "RUN_PAUSED_BUDGET", "clearing the block must restore the budget pause");
    await reResumed.stop();
    await resumed.stop();
  } finally {
    process.env.PATH = originalPath;
    if (setup) {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
    cleanupDir(toolDir);
  }
});

test("env-terminal: resuming a DONE run with a missing tool records the tools but stays DONE", async () => {
  const toolDir = fs.mkdtempSync("/tmp/tt-terminal-tool-");
  const originalPath = process.env.PATH;
  const tool = "tt-terminal-tool";
  const toolPath = path.join(toolDir, tool);
  fs.writeFileSync(toolPath, "#!/bin/sh\nexit 0\n");
  fs.chmodSync(toolPath, 0o755);
  process.env.PATH = `${toolDir}:${originalPath}`;
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      checks: [`${tool} --check`],
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
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
            },
          },
        ],
      }),
      deadlines: FAST,
    });
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, undefined, setup.runDir);
    await setup.conductor.stop();

    // Findings M-7 / disc-A-26: the tool disappears and the finished run is
    // resumed. A terminal phase has no baseline or agent to guard, so it
    // must stay DONE — never flip to ENV_BLOCKED.
    fs.rmSync(toolPath, { force: true });
    const resumed = resumeConductor(setup.runDir, setup.plan, setup.scriptsDir, process.env);
    await resumed.start();
    assert.equal(resumed.state.phase.phase, "DONE");
    assert.notEqual(resumed.state.run, "ENV_BLOCKED", "a terminal phase must not be flipped to ENV_BLOCKED");
    assert.equal(observeRun(setup.runDir), "done", "the program still observes the node as done");
    await resumed.stop();
  } finally {
    process.env.PATH = originalPath;
    if (setup) {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
    cleanupDir(toolDir);
  }
});

// ---------------------------------------------------------------------------
// tt program start | resume | retry preflight
// ---------------------------------------------------------------------------

function tmpDir(prefix: string): string {
  return fs.mkdtempSync(path.join("/tmp", `${prefix}-${randomBytes(3).toString("hex")}-`));
}

function runCli(args: string[], env: NodeJS.ProcessEnv): Promise<{ code: number | null; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI, ...args], { env: { ...process.env, ...env } });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ code, stdout, stderr }));
  });
}

function missingToolPlan(repo: string, tool: string): RunPlanFile {
  return {
    title: "env preflight plan",
    repo,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: [`${tool} --check`], boundaries: [], reserved: [] }],
  };
}

test("env-preflight: tt program start refuses, naming the missing tool, and creates no run", async () => {
  const repo = makeRepo();
  const dir = tmpDir("tt-env-prog-start");
  const root = path.join(dir, "root");
  try {
    const program: ProgramFile = {
      title: "env",
      maxParallel: 1,
      entries: [{ id: "e1", after: [], plan: missingToolPlan(repo.dir, "tt-env-missing-start") }],
    };
    const programPath = path.join(dir, "program.json");
    fs.writeFileSync(programPath, JSON.stringify(program));
    const r = await runCli(["program", "start", programPath, "--root", root]);
    assert.notEqual(r.code, 0);
    assert.match(r.stderr, /tt-env-missing-start/);
    assert.match(r.stderr, /not on PATH/i);
    assert.equal(fs.existsSync(path.join(root, "programs")), false, "no program (and no run) was created");
  } finally {
    cleanupDir(root);
    cleanupDir(dir);
    cleanupDir(repo.dir);
  }
});

test("env-preflight: tt program resume refuses a node whose entry names a missing tool", async () => {
  const repo = makeRepo();
  const dir = tmpDir("tt-env-prog-resume");
  const root = path.join(dir, "root");
  try {
    fs.mkdirSync(root, { recursive: true });
    const program: ProgramFile = {
      title: "env",
      maxParallel: 1,
      entries: [{ id: "e1", after: [], plan: missingToolPlan(repo.dir, "tt-env-missing-resume") }],
    };
    const programDir = createProgram(root, program, "envresume1");
    const runDir = createRun(root, program.entries[0].plan);
    const runId = path.basename(runDir);
    appendProgramEvent(programDir, { type: "NODE_STARTED", node: "e1", runId });
    appendProgramEvent(programDir, { type: "NODE_STATUS", node: "e1", status: "stopped" });
    const r = await runCli(["program", "resume", path.basename(programDir), "--root", root]);
    assert.notEqual(r.code, 0);
    assert.match(r.stderr, /tt-env-missing-resume/);
    assert.equal(
      fs.existsSync(path.join(runDir, "conductor.pid")),
      false,
      "the env-blocked node was not restarted",
    );
  } finally {
    cleanupDir(root);
    cleanupDir(dir);
    cleanupDir(repo.dir);
  }
});

test("env-preflight: tt program retry refuses a blocked node whose entry names a missing tool", async () => {
  const repo = makeRepo();
  const dir = tmpDir("tt-env-prog-retry");
  const root = path.join(dir, "root");
  try {
    fs.mkdirSync(root, { recursive: true });
    const program: ProgramFile = {
      title: "env",
      maxParallel: 1,
      entries: [{ id: "e1", after: [], plan: missingToolPlan(repo.dir, "tt-env-missing-retry") }],
    };
    const programDir = createProgram(root, program, "envretry1");
    appendProgramEvent(programDir, { type: "NODE_BLOCKED", node: "e1", reason: "stuck" });
    const before = fs.readdirSync(root).filter((n) => !["programs", "notifications.jsonl"].includes(n));
    const r = await runCli(["program", "retry", path.basename(programDir), "e1", "--root", root]);
    assert.notEqual(r.code, 0);
    assert.match(r.stderr, /tt-env-missing-retry/);
    const after = fs.readdirSync(root).filter((n) => !["programs", "notifications.jsonl"].includes(n));
    assert.deepEqual(after, before, "no new run was created");
  } finally {
    cleanupDir(root);
    cleanupDir(dir);
    cleanupDir(repo.dir);
  }
});
