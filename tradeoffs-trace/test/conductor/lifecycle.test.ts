// 07 design A1 (§3.1): the lifecycle of a run's ADMIT. It runs before any
// event, on every start — `tt start`, `tt resume` and a program node's own
// start — and its environment check is re-resolved from the shell that runs
// the command now.
//
// exex 07f1, 2026-10-28: a run recorded a PATH at its first start; by the
// resume, brew had installed pi 1.4.0 into that PATH, and the resume adopted
// the record (`adoptRecordedPath`) instead of resolving the shell it ran from.
// The conductor then crashed at its next agent launch instead of naming the
// environment fault. So on resume the pinned pi must be resolved again: a shell
// whose pi is the pinned one starts the conductor even when the run's recorded
// PATH is wrong, and a shell whose pi is wrong refuses with X:ENV_FAULT naming
// the version and the path — even when the recorded PATH is right.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { AdmitRefusal, Conductor, contractVersionFor, createRun, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import { ADMIT_PINNED_PI } from "../../src/core/admit.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  FAKE_PI_PATH,
  makeRepo,
  makeRunRoot,
  readEvents,
  setupConductor,
  waitFor,
  writeScript,
} from "./harness.ts";
import type { Reviewer } from "../../src/core/types.ts";

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

/** A `pi` on a PATH: answers ADMIT's `--version` with `version`, and runs
 * fake-pi for every other invocation — so a conductor whose `piCommand` is
 * unset (the production shape) launches scripted agents through the PATH. */
function writePiShim(dir: string, version: string): string {
  const file = path.join(dir, "pi");
  fs.writeFileSync(
    file,
    "#!/bin/sh\n" +
      `if [ "$1" = "--version" ]; then printf '%s\\n' '${version}'; exit 0; fi\n` +
      `exec '${process.execPath}' '${FAKE_PI_PATH}' "$@"\n`,
  );
  fs.chmodSync(file, 0o755);
  return file;
}

/** A `pi` on a PATH that first records WHICH binary a spawn picked ($0 into
 * `marker`), then runs fake-pi — so a test can tell which pi an agent
 * launched, not merely that one did. */
function writeMarkingPiShim(dir: string, version: string, marker: string): string {
  const file = path.join(dir, "pi");
  fs.writeFileSync(
    file,
    "#!/bin/sh\n" +
      `if [ "$1" = "--version" ]; then printf '%s\\n' '${version}'; exit 0; fi\n` +
      `printf '%s\\n' "$0" >> '${marker}'\n` +
      `exec '${process.execPath}' '${FAKE_PI_PATH}' "$@"\n`,
  );
  fs.chmodSync(file, 0o755);
  return file;
}

function shortTmp(prefix: string): string {
  return fs.mkdtempSync(path.join("/tmp", `${prefix}-`));
}

function eventsOfType(runDir: string, type: string): unknown[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === type)
    .map((r) => r.event);
}

test("a resume re-resolves the pinned pi and refuses a wrong one with X:ENV_FAULT", async () => {
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const scripts = shortTmp("tt-07h1-scripts");
  const binRight = shortTmp("tt-07h1-pi-right");
  const binWrong = shortTmp("tt-07h1-pi-wrong");
  const shellPath = process.env.PATH ?? "";
  const rightPi = writePiShim(binRight, ADMIT_PINNED_PI);
  const wrongPi = writePiShim(binWrong, "1.4.0");

  const plan: RunPlanFile = {
    title: "admit lifecycle",
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
  };
  const runDir = createRun(runRoot, plan);
  const paths = runPaths(runDir);
  // One script for every worker attempt: it holds the run in BUILD, so the
  // run is genuinely mid-life (not DONE) when each resume happens.
  const worker = writeScript(scripts, "worker", { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] });
  const reviewer = writeScript(scripts, "reviewer", {
    hello: { role: "reviewer" as const, tools: ["read", "grep", "glob", "ls", "bash"] },
    steps: [],
  });
  const piEnvFor = (role: Reviewer | string, _agentId: string): NodeJS.ProcessEnv | undefined => {
    if (role === "worker") return { FAKE_PI_SCRIPT: worker };
    if (role === "reviewer") return { FAKE_PI_SCRIPT: reviewer };
    return undefined;
  };
  const makeConductor = (): Conductor =>
    new Conductor({
      runDir,
      plan,
      // No `piCommand`: this conductor resolves `pi` on the PATH, exactly as a
      // production run does (the whole point of the phase).
      piArgsPrefix: [],
      piEnvFor,
      stubReviews: true,
      deadlines: FAST,
      checkLockPath: path.join(runRoot, "check.lock"),
      gateLockPath: path.join(runRoot, "gate.lock"),
    });

  let conductor: Conductor | undefined;
  let resumed: Conductor | undefined;
  let refused: Conductor | undefined;
  try {
    // The first start resolves the pinned pi from the right PATH.
    process.env.PATH = `${binRight}:${shellPath}`;
    conductor = makeConductor();
    await conductor.start();
    await waitFor(() => conductor!.agentPgids.length > 0, 20_000, 25, runDir);
    await conductor.stop();
    conductor = undefined;

    // The run's recorded environment is now the stale one: the PATH it wrote
    // at its first start holds a pi that is NOT the pinned version. This is
    // 07f1's drift (a brew upgrade in place after the run started).
    const envFile = path.join(runDir, "check-env.json");
    const recorded = JSON.parse(fs.readFileSync(envFile, "utf8")) as Record<string, string | null>;
    assert.equal(recorded.PATH, `${binRight}:${shellPath}`, "the first start recorded the PATH it resolved");
    fs.writeFileSync(envFile, `${JSON.stringify({ ...recorded, PATH: `${binWrong}:${shellPath}` }, null, 2)}\n`);
    assert.match(fs.readFileSync(envFile, "utf8"), new RegExp(binWrong.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")));
    const eventsBefore = readEvents(runDir).length;
    const resumedBefore = eventsOfType(runDir, "RUN_RESUMED").length;

    // Resume 1: the shell's PATH has the pinned pi, the record's does not.
    // The conductor must start: the recorded environment is never the source.
    process.env.PATH = `${binRight}:${shellPath}`;
    resumed = makeConductor();
    await resumed.start();
    assert.equal(
      eventsOfType(runDir, "RUN_RESUMED").length,
      resumedBefore + 1,
      "the resumed conductor drove the run (a RUN_RESUMED was recorded), so it started",
    );
    assert.ok(readEvents(runDir).length > eventsBefore, "the resume appended its own events");
    // What ADMIT saw is recorded for the owner — and it is this start's pi.
    const admit = JSON.parse(fs.readFileSync(path.join(runDir, "admit.json"), "utf8")) as {
      path: string;
      pi: { path?: string; version?: string; pinned: string };
    };
    assert.equal(admit.pi.path, rightPi, "ADMIT resolved the pi on the resuming shell's PATH");
    assert.equal(admit.pi.version, ADMIT_PINNED_PI, "ADMIT recorded the version it saw");
    assert.equal(admit.pi.pinned, ADMIT_PINNED_PI, "ADMIT recorded the pin it asserts");
    await resumed.stop();
    resumed = undefined;

    // Resume 2: the shell's PATH has a pi that is NOT the pinned one, while
    // the RUN'S RECORDED PATH is now the right one — so a pass here could
    // only come from reading the record. It refuses, naming both.
    fs.writeFileSync(
      envFile,
      `${JSON.stringify({ ...JSON.parse(fs.readFileSync(envFile, "utf8")), PATH: `${binRight}:${shellPath}` }, null, 2)}\n`,
    );
    const eventsBeforeRefusal = readEvents(runDir).length;
    process.env.PATH = `${binWrong}:${shellPath}`;
    refused = makeConductor();
    await assert.rejects(
      () => refused!.start(),
      (err: unknown) => {
        assert.ok(err instanceof AdmitRefusal, `expected an AdmitRefusal, got ${String(err)}`);
        assert.equal(err.kind, "X:ENV_FAULT", "a wrong pi is an environment fault, not a plan defect");
        assert.match(err.message, /X:ENV_FAULT/);
        assert.match(err.reason, /1\.4\.0/, "the reason names the version the shell's pi reports");
        assert.match(err.reason, new RegExp(wrongPi.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")), "the reason names the pi it resolved");
        assert.match(err.reason, new RegExp(binWrong.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")), "the reason names the PATH it resolved on");
        return true;
      },
    );
    assert.equal(readEvents(runDir).length, eventsBeforeRefusal, "a refused resume appends no event");
    refused = undefined;
  } finally {
    if (refused) await refused.stop().catch(() => undefined);
    if (resumed) await resumed.stop().catch(() => undefined);
    if (conductor) await conductor.stop().catch(() => undefined);
    process.env.PATH = shellPath;
    cleanupDir(binRight);
    cleanupDir(binWrong);
    cleanupDir(scripts);
    cleanupDir(runRoot);
    cleanupDir(repo.dir);
  }
});

/** 07 design A1 (exex 07f1's own crash, one step later than ADMIT): ADMIT
 * resolved the pinned pi on the shell's PATH, but `adoptRecordedPath` prepends
 * the PATH the run recorded at its first start to this process's own — so a
 * bare `pi` on the next agent launch picks the RECORDED one up (a brew 1.4.0
 * in 07f1) and the agent crashes on a binary ADMIT never checked. Every agent
 * must launch the pi ADMIT resolved, whatever the process PATH leads to. */
test("a conductor launches the pi ADMIT resolved, not the one the recorded PATH leads to", async () => {
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const scripts = shortTmp("tt-07h1-scripts");
  const binRecorded = shortTmp("tt-07h1-pi-recorded");
  const binShell = shortTmp("tt-07h1-pi-shell");
  const shellPath = process.env.PATH ?? "";
  const recordedMarker = path.join(scripts, "recorded-pi-launched.txt");
  const shellMarker = path.join(scripts, "shell-pi-launched.txt");
  const recordedPi = writeMarkingPiShim(binRecorded, "1.4.0", recordedMarker);
  const shellPi = writeMarkingPiShim(binShell, ADMIT_PINNED_PI, shellMarker);

  const plan: RunPlanFile = {
    title: "admit launch",
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
  };
  const runDir = createRun(runRoot, plan);
  const worker = writeScript(scripts, "worker", { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] });
  const reviewer = writeScript(scripts, "reviewer", {
    hello: { role: "reviewer" as const, tools: ["read", "grep", "glob", "ls", "bash"] },
    steps: [],
  });
  const piEnvFor = (role: Reviewer | string): NodeJS.ProcessEnv | undefined => {
    if (role === "worker") return { FAKE_PI_SCRIPT: worker };
    if (role === "reviewer") return { FAKE_PI_SCRIPT: reviewer };
    return undefined;
  };
  let conductor: Conductor | undefined;
  try {
    // A resume's process PATH: `adoptRecordedPath` has already prepended the
    // PATH the run recorded — which here leads to a pi that is NOT the pinned
    // one. The shell this resumed from resolves the pinned pi, and that is the
    // PATH ADMIT is handed (`admitPath`).
    process.env.PATH = `${binRecorded}:${binShell}:${shellPath}`;
    conductor = new Conductor({
      runDir,
      plan,
      // No `piCommand`: this conductor launches `pi` as a production run does.
      piArgsPrefix: [],
      piEnvFor,
      stubReviews: true,
      deadlines: FAST,
      checkLockPath: path.join(runRoot, "check.lock"),
      gateLockPath: path.join(runRoot, "gate.lock"),
      admitPath: `${binShell}:${shellPath}`,
    });
    await conductor.start();
    await waitFor(() => conductor!.agentPgids.length > 0, 20_000, 25, runDir);

    // ADMIT resolved the shell's pi...
    const admit = JSON.parse(fs.readFileSync(path.join(runDir, "admit.json"), "utf8")) as {
      pi: { path?: string; version?: string };
    };
    assert.equal(admit.pi.path, shellPi, "ADMIT resolved the pinned pi the shell's PATH leads to");
    assert.equal(admit.pi.version, ADMIT_PINNED_PI);
    // ...and the agent process that just launched IS that binary: its own
    // shim recorded the launch, the recorded PATH's pi recorded nothing.
    assert.ok(fs.existsSync(shellMarker), "the launched agent ran the pi ADMIT resolved");
    assert.deepEqual(
      fs.readFileSync(shellMarker, "utf8").trim().split("\n"),
      [shellPi],
      "every launch so far is the pi ADMIT resolved on the shell's PATH",
    );
    assert.equal(fs.existsSync(recordedMarker), false, `no agent ran the recorded PATH's pi (${recordedPi})`);
    await conductor.stop();
    conductor = undefined;
  } finally {
    if (conductor) await conductor.stop().catch(() => undefined);
    process.env.PATH = shellPath;
    cleanupDir(binRecorded);
    cleanupDir(binShell);
    cleanupDir(scripts);
    cleanupDir(runRoot);
    cleanupDir(repo.dir);
  }
});

test("a conductor start refuses a check command that does not parse, before any event", async () => {
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const scripts = shortTmp("tt-07h1-scripts");
  const broken = 'AST_GREP="rg --json"; $AST_GREP --files | | grep -v node_modules';
  const plan: RunPlanFile = {
    title: "admit start",
    repo: repo.dir,
    integrationBranch: "main",
    checks: [broken],
    phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
  };
  const runDir = createRun(runRoot, plan);
  const worker = writeScript(scripts, "worker", {
    hello: defaultWorkerHello(),
    steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
  });
  let conductor: Conductor | undefined;
  try {
    conductor = new Conductor({
      runDir,
      plan,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      piEnvFor: (role) => (role === "worker" ? { FAKE_PI_SCRIPT: worker } : undefined),
      stubReviews: true,
      deadlines: FAST,
      checkLockPath: path.join(runRoot, "check.lock"),
      gateLockPath: path.join(runRoot, "gate.lock"),
    });
    await assert.rejects(
      () => conductor!.start(),
      (err: unknown) => {
        assert.ok(err instanceof AdmitRefusal, `expected an AdmitRefusal, got ${String(err)}`);
        assert.equal(err.kind, "X:PLAN_DEFECT", "a command that does not parse is a plan defect");
        assert.ok(err.reason.includes(broken), `the reason names the command:\n${err.reason}`);
        return true;
      },
    );
    // ADMIT runs before the log is even opened: no event, no check
    // environment, no baseline. The run's own directory exists (createRun)
    // but its life has not begun.
    assert.equal(fs.existsSync(runPaths(runDir).events), false, "no event file was created");
    assert.equal(fs.existsSync(path.join(runDir, "check-env.json")), false, "no check environment was recorded");
    assert.equal(readEvents(runDir).length, 0, "no event exists");
  } finally {
    if (conductor) await conductor.stop().catch(() => undefined);
    cleanupDir(scripts);
    cleanupDir(runRoot);
    cleanupDir(repo.dir);
  }
});

// ---------------------------------------------------------------------------
// Plan 07h2 (A3/R4, R11, R12): what a stopped-or-finished run leaves behind, and
// what a worker's shell command may never outlive.
// ---------------------------------------------------------------------------

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function pidFile(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

/** Pids whose command line contains TOKEN (the whole process table, so a
 * busy-loop child spawned by a script is found too). */
function pidsWith(token: string): number[] {
  const out = execFileSync("ps", ["-eo", "pid=,args="], { encoding: "utf8" });
  const pids: number[] = [];
  for (const line of out.split("\n")) {
    if (!line.includes(token)) continue;
    const pid = Number(line.trim().split(/\s+/)[0]);
    if (Number.isFinite(pid) && pid > 0 && pid !== process.pid) pids.push(pid);
  }
  return pids;
}

test("plan 07h2 (R4, run d170bb37): a conductor at DONE exits and removes the run's worktrees and build targets", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-07h2-root");
  const scriptsDir = shortTmp("tt-07h2-scripts");
  const plan: RunPlanFile = {
    title: "lifecycle prune",
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
  };
  fs.writeFileSync(path.join(scriptsDir, "plan.json"), JSON.stringify(plan));
  // The worker builds "134 GB of targets" the way the incident did: inside its
  // own worktree, before it submits.
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [
        { kind: "call-sh", command: "mkdir -p target node_modules/pkg && echo big > target/big.bin && echo dep > node_modules/pkg/index.js" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  );
  fs.writeFileSync(
    path.join(scriptsDir, "reviewer.json"),
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
  const env = {
    ...process.env,
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    TT_TEST_STUB_REVIEWS: "1",
    TT_TEST_DEADLINES: JSON.stringify({ abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 5_000, reviewMs: 20_000, freezeMs: 20_000 }),
    FAKE_PI_SCRIPT: scriptsDir,
  };
  let runId = "";
  let runDir = "";
  try {
    runId = execFileSync(process.execPath, [CLI_PATH, "start", path.join(scriptsDir, "plan.json"), "--root", root], { encoding: "utf8", env }).trim();
    runDir = path.join(root, runId);
    const paths = runPaths(runDir);
    // The worktree existed, with the targets inside it.
    await waitFor(() => fs.existsSync(path.join(paths.worktree, "target", "big.bin")), 30_000, 50, runDir);
    // The stage the incident never reached: the conductor process's own exit.
    const conductorPid = Number(fs.readFileSync(path.join(runDir, "conductor.pid"), "utf8"));
    await waitFor(() => !pidFile(conductorPid), 180_000, 200, runDir);
    assert.equal(pidFile(conductorPid), false, "no conductor process remains after DONE");
    const status = execFileSync(process.execPath, [CLI_PATH, "status", runId, "--root", root], { encoding: "utf8", env });
    assert.match(status, /phase: p1 — DONE/, `the run ended DONE:\n${status}`);

    // The worktrees and the build targets are gone; the run's evidence is not.
    assert.equal(fs.existsSync(paths.worktree), false, "the run's worktree is pruned");
    assert.equal(fs.existsSync(path.join(runDir, "worktrees")), false, "no lane worktrees remain");
    assert.equal(fs.existsSync(path.join(paths.candidates, "target")), false);
    assert.equal(fs.existsSync(paths.events), true, "the control log is kept");
    assert.ok(readEvents(runDir).length > 0);
    // Nothing named `target` survives anywhere in the run directory.
    const leftovers = execFileSync("find", [runDir, "-name", "target", "-type", "d"], { encoding: "utf8" }).trim();
    assert.equal(leftovers, "", `no target directory survives:\n${leftovers}`);
  } finally {
    try {
      if (runId) execFileSync(process.execPath, [CLI_PATH, "stop", runId, "--root", root], { env });
    } catch {
      // already gone
    }
    cleanupDir(root);
    cleanupDir(scriptsDir);
    cleanupDir(repo.dir);
  }
});

test("plan 07h2 (R11, exex 01c3b): an owner stop and resume keep a lane's uncommitted work, and the resumed worker is told its worktree is unchanged", async () => {
  const scripts = shortTmp("tt-07h2-lanes");
  const promptLog = path.join(scripts, "prompts.log");
  let conductor: Conductor | undefined;
  let resumed: Conductor | undefined;
  const setup = await setupConductor({
    checks: ["true"],
    phase: { id: "p1", goal: "two lanes", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], workers: 2 },
    deadlines: FAST,
    laneWorkerScriptFor: (lane) =>
      lane === "a"
        ? { hello: defaultWorkerHello(), steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }] }
        : {
            hello: defaultWorkerHello(),
            // The lane's uncommitted edit, then it waits for the owner's stop.
            steps: [{ kind: "call-sh", command: "printf 'kept work\\n' > uncommitted.txt" }, { kind: "hang-until-abort" }],
          },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "printf 'kept work\\n' > uncommitted.txt" }, { kind: "hang-until-abort" }],
    }),
    reviewerScriptFor: () => ({ hello: defaultReviewerHello(), steps: [] }),
  });
  const laneWorktree = path.join(setup.runDir, "worktrees", "lane-b");
  try {
    conductor = setup.conductor;
    await conductor.start();
    // Lane b has an UNCOMMITTED file in its own worktree...
    await waitFor(() => fs.existsSync(path.join(laneWorktree, "uncommitted.txt")), 60_000, 50, setup.runDir);
    const status = execFileSync("git", ["-C", laneWorktree, "status", "--porcelain"], { encoding: "utf8" });
    assert.match(status, /uncommitted\.txt/, "the edit is uncommitted before the stop");

    // ...the owner stops, and the edit survives the stop.
    await conductor.stop();
    conductor = undefined;
    assert.ok(fs.existsSync(path.join(laneWorktree, "uncommitted.txt")), "the lane's edits are still on disk after the stop");
    assert.equal(
      execFileSync("git", ["-C", laneWorktree, "status", "--porcelain"], { encoding: "utf8" }).includes("uncommitted.txt"),
      true,
      "and still uncommitted",
    );
    const keptRecord = readEvents(setup.runDir).find((r) => r.kind === "lane_worktree_kept");
    assert.ok(keptRecord, "the stop recorded the kept lane worktree (so a resume can find it)");

    // The resume: the same run, the same plan, a fresh prompt log.
    resumed = new Conductor({
      runDir: setup.runDir,
      plan: setup.plan,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      piEnvFor: (role, agentId) => {
        if (role !== "worker") return undefined;
        const script = writeScript(scripts, `resume-${agentId}`, {
          hello: defaultWorkerHello(),
          steps: [{ kind: "hang-until-abort" }],
        });
        return { FAKE_PI_SCRIPT: script, FAKE_PI_PROMPT_LOG: promptLog };
      },
      stubReviews: true,
      deadlines: FAST,
      checkLockPath: path.join(setup.runRoot, "check.lock"),
      gateLockPath: path.join(setup.runRoot, "gate.lock"),
    });
    await resumed.start();
    await waitFor(() => fs.existsSync(promptLog), 60_000, 50, setup.runDir);
    const prompts = fs.readFileSync(promptLog, "utf8");
    assert.match(prompts, /worktree of lane b is unchanged/, "the resumed worker is told its worktree is unchanged");
    assert.ok(fs.existsSync(path.join(laneWorktree, "uncommitted.txt")), "the resumed run did not reset the lane's worktree");
    assert.ok(
      readEvents(setup.runDir).some(
        (r) => r.kind === "lane_worktree_reused" && String(JSON.stringify(r.event)).includes("unchanged"),
      ),
      "the reuse is recorded, so the work is not silently recreated",
    );
  } finally {
    if (conductor) await conductor.stop().catch(() => undefined);
    if (resumed) await resumed.stop().catch(() => undefined);
    await setup.conductor.stop().catch(() => undefined);
    cleanupDir(scripts);
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(setup.repo.dir);
  }
});

test("plan 07h2 (R12, exex 01c3a): a worker's command and the child process it spawns are killed at the sh bound, and the worker is told the command, the bound and that it was killed", async () => {
  const scripts = shortTmp("tt-07h2-sh");
  const marker = "tt-07h2-busyloop";
  let conductor: Conductor | undefined;
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      checks: ["true"],
      // The bound the incident's plan set (TT_SH_MINUTES), shortened for the
      // test: the point is that EVERY command a worker runs is bounded by it.
      deadlines: { ...FAST, shCommandMs: 3_000 },
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          {
            kind: "call-sh",
            // A script the worker wrote itself: it starts a CHILD of its own
            // that busy-loops, then busy-loops itself. Both must die.
            command: `python3 -c 'import subprocess, sys
subprocess.Popen([sys.executable, "-c", "import time\\nwhile True: time.sleep(0.01)  # ${marker}-child"])
while True: pass  # ${marker}'`,
          },
          { kind: "hang-until-abort" },
        ],
      }),
      reviewerScriptFor: () => ({ hello: defaultReviewerHello(), steps: [] }),
    });
    conductor = setup.conductor;
    await conductor.start();
    // The command really ran (it is the one that is over the bound, not a
    // command that never started).
    await waitFor(() => pidsWith(marker).length > 0, 30_000, 50, setup.runDir);
    // Past the 3 s bound the WHOLE process group is killed — the python script
    // and the child it spawned.
    await waitFor(() => pidsWith(marker).length === 0, 30_000, 100, setup.runDir);
    assert.deepEqual(pidsWith(marker), [], "no process of the killed command survives, child included");
    // The worker got the note: its command, the bound, and that it was killed.
    const streamDir = path.join(setup.runDir, "stream");
    const streams = fs
      .readdirSync(streamDir)
      .filter((name) => name.startsWith("worker-"))
      .map((name) => fs.readFileSync(path.join(streamDir, name), "utf8"))
      .join("\n");
    assert.match(streams, /killed after 3 s/, "the note names the bound that killed it");
    assert.match(streams, /process group/, "the note says the whole process group was killed");
    assert.ok(streams.includes("python3 -c"), `the note names the command the worker ran:\n${streams.slice(-2000)}`);
    assert.match(streams, /tt-07h2-busyloop/, "the note quotes the worker's own command text");
  } finally {
    if (conductor) await conductor.stop().catch(() => undefined);
    for (const pid of pidsWith(marker)) {
      try {
        process.kill(pid, "SIGKILL");
      } catch {
        // already gone
      }
    }
    if (setup) {
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      cleanupDir(setup.repo.dir);
    }
    cleanupDir(scripts);
  }
});
