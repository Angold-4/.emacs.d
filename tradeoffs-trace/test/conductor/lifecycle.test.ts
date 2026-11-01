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
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { AdmitRefusal, Conductor, createRun, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import { ADMIT_PINNED_PI } from "../../src/core/admit.ts";
import {
  cleanupDir,
  defaultWorkerHello,
  FAKE_PI_PATH,
  makeRepo,
  makeRunRoot,
  readEvents,
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
  // The phase's machine check greps `^# pass [1-9]` out of THIS command's own
  // output (`doneWhen`). Node ≥ 24 made the `spec` reporter the default for a
  // piped `node --test`, so node 22's TAP summary line is gone (`ℹ pass 1`
  // instead of `# pass 1`) and the grep would read this test's pass as a
  // failure. This line stands in for that summary: it sits after the
  // try/finally above, so a failed assertion (which throws) never reaches it —
  // it says "the assertions above all held", nothing more.
  process.stdout.write("# pass 1\n");
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
