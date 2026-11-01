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
import { pruneRunTargets } from "../../src/effects/prune.ts";
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
//
// R4 (run d170bb37): a finished conductor exits and prunes — its worktrees and
// its build targets, including a package's nested target.
// R11 (exex 01c3b): a resumed run keeps each lane's uncommitted changes.
// R12 (exex 01c3a): a worker command that runs past TT_SH_MINUTES is killed and
// reported to the worker.
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

async function conductorAtDoneExitsAndPrunes(): Promise<void> {
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
  // own worktree, before it submits. One of them is nested inside a package
  // directory and committed, so the run's candidate checkout holds it too —
  // the shape a depth-limited walk of the checkout used to miss.
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [
        {
          kind: "call-sh",
          command:
            "mkdir -p target node_modules/pkg packages/web/target && echo big > target/big.bin && echo dep > node_modules/pkg/index.js && echo nested > packages/web/target/bundle.js && git add -f packages/web/target && git -c user.name=t -c user.email=t@t commit -q -m 'nested targets'",
        },
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
    // The nested package target the worker committed is in the candidate
    // checkout's tree (the shape a depth-limited walk missed): the run's repo
    // holds a commit that contains it.
    const committed = execFileSync("git", ["-C", repo.dir, "log", "--all", "--format=%H", "--", "packages/web/target/bundle.js"], { encoding: "utf8" }).trim();
    assert.notEqual(committed, "", "the nested target was committed, so the candidate checkout held it");
    const status = execFileSync(process.execPath, [CLI_PATH, "status", runId, "--root", root], { encoding: "utf8", env });
    assert.match(status, /phase: p1 — DONE/, `the run ended DONE:\n${status}`);

    // The worktrees and the build targets are gone; the run's evidence is not.
    const candidatesAfter = fs.readdirSync(paths.candidates);
    assert.equal(fs.existsSync(paths.worktree), false, "the run's worktree is pruned");
    assert.equal(fs.existsSync(path.join(runDir, "worktrees")), false, "no lane worktrees remain");
    assert.equal(fs.existsSync(path.join(paths.candidates, "target")), false);
    assert.ok(candidatesAfter.length > 0, "the run kept its candidate checkouts");
    for (const sha of candidatesAfter) {
      assert.equal(
        fs.existsSync(path.join(paths.candidates, sha, "packages", "web", "target")),
        true,
        `the tracked package target is kept in candidate ${sha}`,
      );
      assert.equal(fs.existsSync(path.join(paths.candidates, sha, "packages", "web", "target", "bundle.js")), true);
    }
    assert.equal(fs.existsSync(paths.events), true, "the control log is kept");
    // The conductor's own log says what it pruned (so an owner can tell a
    // finished run's clean exit from a crashed one).
    assert.match(fs.readFileSync(paths.log, "utf8"), /pruned \d+ director(y|ies) after DONE/, "the prune is recorded in the conductor's log");
    assert.ok(readEvents(runDir).length > 0);
    // Generated output is pruned, but a name alone cannot authorize deleting
    // tracked content from the saved evidence.
    assert.match(fs.readFileSync(paths.log, "utf8"), /kept .*packages\/web\/target: holds tracked files/);
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
}
test("plan 07h2 (R4, run d170bb37): a conductor at DONE exits and removes the run's worktrees and build targets", conductorAtDoneExitsAndPrunes);
test("a conductor at DONE exits and removes its worktrees and targets", conductorAtDoneExitsAndPrunes);

test("the prune reaches a build target nested in a package directory", () => {
  // A3/R4: run d170bb37 left 134 GB of targets behind. A repository nests its
  // build products (`packages/web/target`, `crates/api/target`), so the prune
  // that removes them cannot stop at the checkout's first level.
  const runDir = shortTmp("tt-07h2-prune");
  const worktree = shortTmp("tt-07h2-prune-wt");
  try {
    const candidate = path.join(runDir, "candidates", "abc123");
    const nested = path.join(candidate, "packages", "web");
    fs.mkdirSync(path.join(nested, "target"), { recursive: true });
    fs.writeFileSync(path.join(nested, "target", "bundle.js"), "built\n");
    fs.mkdirSync(path.join(candidate, "node_modules", "pkg"), { recursive: true });
    fs.writeFileSync(path.join(candidate, "node_modules", "pkg", "index.js"), "dep\n");
    fs.writeFileSync(path.join(candidate, "README.md"), "kept\n");
    // `.git` is never walked: a repository's own directory is not a build
    // target and may hold anything.
    execFileSync("git", ["init", "-q", candidate]);
    fs.mkdirSync(path.join(candidate, ".git", "target"), { recursive: true });
    const result = pruneRunTargets(runDir, { run: worktree });
    assert.equal(fs.existsSync(path.join(nested, "target")), false, "the nested package target is pruned");
    assert.equal(fs.existsSync(path.join(candidate, "node_modules")), false, "a nested node_modules is pruned");
    assert.equal(fs.existsSync(path.join(candidate, "README.md")), true, "a checked-in file is kept");
    assert.equal(fs.existsSync(path.join(candidate, ".git", "target")), true, ".git is not walked");
    assert.equal(fs.existsSync(worktree), false, "the worktree is pruned");
    assert.equal(fs.existsSync(path.join(runDir, "candidates")), true, "the candidate checkout itself is kept");
    assert.ok(result.removed.length >= 3, `the prune reports what it removed: ${JSON.stringify(result.removed)}`);
  } finally {
    cleanupDir(runDir);
    fs.rmSync(worktree, { recursive: true, force: true });
  }
});

async function resumedRunKeepsEachLanesChanges(): Promise<void> {
  const scripts = shortTmp("tt-07h2-lanes");
  const promptLog = path.join(scripts, "prompts.log");
  let conductor: Conductor | undefined;
  let resumed: Conductor | undefined;
  const setup = await setupConductor({
    checks: ["true"],
    phase: { id: "p1", goal: "two lanes", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], workers: 2 },
    deadlines: FAST,
    laneWorkerScriptFor: (lane) => ({
      hello: defaultWorkerHello(),
      // Each lane's uncommitted edit, then it waits for the owner's stop.
      steps: [{ kind: "call-sh", command: `printf 'kept ${lane}\\n' > uncommitted-${lane}.txt` }, { kind: "hang-until-abort" }],
    }),
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "printf 'kept work\\n' > uncommitted.txt" }, { kind: "hang-until-abort" }],
    }),
    reviewerScriptFor: () => ({ hello: defaultReviewerHello(), steps: [] }),
  });
  // The lanes' own worktrees, each with its own uncommitted file.
  const lanes = { a: path.join(setup.runDir, "worktrees", "lane-a"), b: path.join(setup.runDir, "worktrees", "lane-b") };
  const keptFiles = { a: path.join(lanes.a, "uncommitted-a.txt"), b: path.join(lanes.b, "uncommitted-b.txt") };
  try {
    conductor = setup.conductor;
    await conductor.start();
    for (const lane of ["a", "b"] as const) {
      // Each lane has an UNCOMMITTED file in its OWN worktree.
      await waitFor(() => fs.existsSync(keptFiles[lane]), 60_000, 50, setup.runDir);
      const status = execFileSync("git", ["-C", lanes[lane], "status", "--porcelain"], { encoding: "utf8" });
      assert.match(status, new RegExp(`uncommitted-${lane}\\.txt`), `lane ${lane}'s edit is uncommitted before the stop`);
    }

    // ...the owner stops, and each lane's edit survives the stop.
    await conductor.stop();
    conductor = undefined;
    for (const lane of ["a", "b"] as const) {
      assert.ok(fs.existsSync(keptFiles[lane]), `lane ${lane}'s edits are still on disk after the stop`);
      assert.equal(
        execFileSync("git", ["-C", lanes[lane], "status", "--porcelain"], { encoding: "utf8" }).includes(`uncommitted-${lane}.txt`),
        true,
        `lane ${lane}'s edit is still uncommitted`,
      );
    }
    const keptRecord = readEvents(setup.runDir).find((r) => r.kind === "lane_worktree_kept");
    assert.ok(keptRecord, "the stop recorded the kept lane worktrees (so a resume can find them)");

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
    await waitFor(() => fs.existsSync(promptLog) && ["a", "b"].every((lane) => fs.readFileSync(promptLog, "utf8").includes(`worktree of lane ${lane} is unchanged`)), 60_000, 50, setup.runDir);
    const prompts = fs.readFileSync(promptLog, "utf8");
    assert.match(prompts, /worktree of lane [a-z, ]+ is unchanged/, "each resumed worker is told its worktree is unchanged");
    for (const lane of ["a", "b"] as const) {
      assert.match(prompts, new RegExp(`worktree of lane [a-z, ]*${lane}`), `the note names lane ${lane}`);
      assert.ok(fs.existsSync(keptFiles[lane]), `the resumed run did not reset lane ${lane}'s worktree`);
    }
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
}
test(
  "plan 07h2 (R11, exex 01c3b): an owner stop and resume keep a lane's uncommitted work, and the resumed worker is told its worktree is unchanged",
  resumedRunKeepsEachLanesChanges,
);
test("a resumed run keeps each lane's uncommitted changes", resumedRunKeepsEachLanesChanges);

test("a resumed run keeps a lane's uncommitted changes when its base moved", async (t) => {
  for (const conflict of [false, true]) await t.test(conflict ? "conflicting base" : "clean new base", async () => {
    const scripts = shortTmp("tt-moved-base");
    const promptLog = path.join(scripts, "prompts.log");
    const setup = await setupConductor({
      checks: ["true"], deadlines: FAST,
      phase: { id: "p1", goal: "keep edits", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], workers: 2 },
      // Model the kept checkout's old base. The round now starts on a newer
      // integration commit; the lane still has edits against its earlier HEAD.
      workerScript: ({ repo }) => ({ hello: defaultWorkerHello(), steps: [
        { kind: "call-sh", command: `git checkout -q --detach ${repo.head}; printf 'worker edit\\n' > README.md; printf 'new file\\n' > untracked.txt` },
        { kind: "hang-until-abort" },
      ] }),
    });
    let resumed: Conductor | undefined;
    try {
      fs.writeFileSync(path.join(setup.repo.dir, conflict ? "README.md" : "new-base.txt"), "new base\n");
      execFileSync("git", ["-C", setup.repo.dir, "add", "-A"]);
      execFileSync("git", ["-C", setup.repo.dir, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "moved base"]);
      const base = execFileSync("git", ["-C", setup.repo.dir, "rev-parse", "HEAD"], { encoding: "utf8" }).trim();
      await setup.conductor.start();
      const lanes = ["a", "b"].map((lane) => path.join(setup.runDir, "worktrees", `lane-${lane}`));
      await waitFor(() => lanes.every((dir) => fs.existsSync(path.join(dir, "untracked.txt"))), 60_000, 50, setup.runDir);
      await setup.conductor.stop();
      const script = writeScript(scripts, "resume", { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] });
      resumed = new Conductor({
        runDir: setup.runDir, plan: setup.plan, piCommand: process.execPath, piArgsPrefix: [FAKE_PI_PATH],
        piEnvFor: () => ({ FAKE_PI_SCRIPT: script, FAKE_PI_PROMPT_LOG: promptLog }),
        stubReviews: true, deadlines: FAST,
        checkLockPath: path.join(setup.runRoot, "check.lock"), gateLockPath: path.join(setup.runRoot, "gate.lock"),
      });
      await resumed.start();
      await waitFor(() => fs.existsSync(promptLog) && fs.readFileSync(promptLog, "utf8").split("\n=====\n").length >= 3, 60_000, 50, setup.runDir);
      for (const dir of lanes) assert.match(fs.readFileSync(path.join(dir, "README.md"), "utf8"), /worker edit/, "the lane's dirty edit survived the base change");
      const prompts = fs.readFileSync(promptLog, "utf8");
      assert.doesNotMatch(prompts, /worktree of lane [ab] is unchanged/);
      assert.match(prompts, conflict ? /edits did not apply cleanly/ : /uncommitted edits were re-applied/);
      const refs = [...new Set(prompts.match(/refs\/tt\/[^\s"\\;]+\/kept\/[ab]\/[a-f0-9-]+/g) ?? [])];
      assert.equal(refs.length, 2, "each lane's exact durable ref is in its prompt");
      for (const ref of refs) {
        assert.equal(execFileSync("git", ["-C", setup.repo.dir, "show", `${ref}:untracked.txt`], { encoding: "utf8" }), "new file\n");
        assert.equal(execFileSync("git", ["-C", setup.repo.dir, "show", `${ref}:README.md`], { encoding: "utf8" }), "worker edit\n");
      }
      for (const dir of lanes) {
        assert.equal(execFileSync("git", ["-C", dir, "rev-parse", "HEAD"], { encoding: "utf8" }).trim(), base);
        const status = execFileSync("git", ["-C", dir, "status", "--porcelain"], { encoding: "utf8" });
        assert.match(status, /README.md/);
        if (!conflict) {
          assert.equal(fs.readFileSync(path.join(dir, "README.md"), "utf8"), "worker edit\n");
          assert.equal(fs.readFileSync(path.join(dir, "untracked.txt"), "utf8"), "new file\n");
        }
      }
    } finally {
      await resumed?.stop();
      await setup.conductor.stop();
      cleanupDir(scripts); cleanupDir(setup.runRoot); cleanupDir(setup.scriptsDir); cleanupDir(setup.repo.dir);
    }
  });
});

async function workerCommandPastShMinutesIsKilled(): Promise<void> {
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
    // The bound is a recorded event, scoped to the agent, not a silent kill.
    const bound = eventsOfType(setup.runDir, "BOUND_REACHED").find((e) => (e as { kind?: string }).kind === "sh") as
      | { scope?: string; limitMs?: number; action?: string }
      | undefined;
    assert.ok(bound, "reaching the sh bound is recorded as BOUND_REACHED");
    assert.equal(bound.limitMs, 3_000, "the record names the limit the plan set");
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
}
test(
  "plan 07h2 (R12, exex 01c3a): a worker's command and the child process it spawns are killed at the sh bound, and the worker is told the command, the bound and that it was killed",
  workerCommandPastShMinutesIsKilled,
);
test("a worker command that runs past TT_SH_MINUTES is killed and reported to the worker", workerCommandPastShMinutesIsKilled);
