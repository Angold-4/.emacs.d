// Plan 01e (runtime doc §4, design D2): the base baseline.
//
// The base can already fail the phase's own checks (atlas plan 13's
// `feat/atlas` had 14 deterministic `exchange-state-machine` failures). The
// conductor runs the checks once on the base, records the failing test names,
// and counts a candidate's failing check as passing when every name it yields
// already failed on the base — while any new failure still fails the gate and
// is named. A base whose failing output yields no test name keeps the strict
// rule, and the baseline is paid for once per base tree (a second program node
// with the same base and checks reuses it).

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { test } from "node:test";

import {
  baselinePromptLines,
  buildContract,
  buildWorkerPrompt,
  Conductor,
  createRun,
  runPaths,
  type RunPlanFile,
} from "../../src/conductor.ts";
import { baselineKey, parseBaseline, type Baseline } from "../../src/core/test-failures.ts";
import { buildView } from "../../src/view.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  FAKE_PI_PATH,
  readEvents,
  setupConductor,
  waitFor,
  writeScript,
} from "./harness.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

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

/** A stub reviewer script (the harness defaults to `stubReviews: true`). */
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

function git(repo: string, args: string[]): string {
  return execFileSync("git", ["-C", repo, ...args], { encoding: "utf8" }).trim();
}

/** Commits a failing node:test file onto the run's base branch, so the base
 * itself fails its own `node --test` check. */
function commitFailingTest(repo: string, file: string, testName: string): void {
  fs.writeFileSync(
    path.join(repo, file),
    [
      "const test = require('node:test');",
      "const assert = require('node:assert/strict');",
      `test(${JSON.stringify(testName)}, () => { assert.equal(1, 2); });`,
      "",
    ].join("\n"),
  );
  git(repo, ["add", "-A"]);
  git(repo, ["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", `add ${file}`]);
}

function baselineRecord(runDir: string): Baseline {
  const file = path.join(runPaths(runDir).checks, "base", "baseline.json");
  assert.ok(fs.existsSync(file), `expected the baseline record at ${file}`);
  const parsed = parseBaseline(JSON.parse(fs.readFileSync(file, "utf8")));
  assert.ok(parsed, "the baseline file must parse as a baseline record");
  return parsed!;
}

function baselineEvents(runDir: string): Array<Record<string, unknown>> {
  return readEvents(runDir)
    .filter((r) => r.kind === "baseline")
    .map((r) => r.event as Record<string, unknown>);
}

function eventTypes(runDir: string): string[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => (r.event as { type: string }).type);
}

test("baseline: a candidate failing only a test the base already failed passes checks, shown as `base has 1 failures`", async () => {
  const setup = await setupConductor({
    checks: ["node --test"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  // The base itself fails X before the worker touches anything.
  commitFailingTest(setup.repo.dir, "x.test.cjs", "base X fails");
  await setup.conductor.start();
  try {
    await waitFor(
      () => setup.conductor.state.phase.phase === "DONE",
      90_000,
      undefined,
      setup.runDir,
    );

    // The base's own record: one failing command, one parsed name.
    const record = baselineRecord(setup.runDir);
    assert.equal(record.commands.length, 1);
    assert.deepEqual(record.commands[0].failures, ["base X fails"]);
    assert.notEqual(record.commands[0].exitCode, 0);
    assert.equal(record.commands[0].timedOut, false);
    assert.ok(record.baseSha.length > 0);
    // The run-local log genuinely holds the executed test's output.
    const log = path.join(runPaths(setup.runDir).checks, "base", record.commands[0].log!);
    assert.match(fs.readFileSync(log, "utf8"), /base X fails/);

    // A failed-but-pre-existing check is recorded as such, not as a pass.
    const preExisting = readEvents(setup.runDir).find((r) => r.kind === "check_failures_pre_existing");
    assert.ok(preExisting, "the excused failure must be recorded as pre-existing");
    assert.deepEqual((preExisting!.event as { failures: string[] }).failures, ["base X fails"]);
    assert.deepEqual(eventTypes(setup.runDir).filter((t) => t === "CHECKS_FAILED"), []);

    // The status: the candidate's passing checks say why, and the base's own
    // failures get their own line.
    const view = buildView(setup.runDir, setup.plan, false);
    assert.match(view.gates, /checks ✓ \(base has 1 failures\)/);
    assert.match(view.gates, /probe ✓/);
    assert.equal(view.baseline, "base fails: 1 tests: base X fails");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("baseline: a candidate failing a base test and a new test fails checks, naming the new test", async () => {
  const setup = await setupConductor({
    checks: ["node --test"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-sh",
          command: [
            "cat > y.test.cjs <<'TTEOF'",
            "const test = require('node:test');",
            "const assert = require('node:assert/strict');",
            "test('new Y fails', () => { assert.equal(1, 2); });",
            "TTEOF",
          ].join("\n"),
        },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  commitFailingTest(setup.repo.dir, "x.test.cjs", "base X fails");
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);

    // The check's completion names only the new failure — never the base's.
    const completion = readEvents(setup.runDir).find(
      (r) => r.kind === "completion" && (r.event as { newFailures?: string[] }).newFailures !== undefined,
    );
    assert.ok(completion, "the failing check must record its new failures");
    const newFailures = (completion!.event as { newFailures: string[] }).newFailures;
    assert.deepEqual(newFailures, ["new Y fails"]);
    assert.ok(!newFailures.includes("base X fails"), "a pre-existing failure must not be blamed on the candidate");

    // And the visible record does the same.
    const marked = readEvents(setup.runDir).find((r) => r.kind === "check_failure_new");
    assert.ok(marked, "expected a check_failure_new record");
    assert.deepEqual((marked!.event as { newFailures: string[] }).newFailures, ["new Y fails"]);

    // No excuse: the gate failed on the new test.
    assert.ok(!eventTypes(setup.runDir).includes("CHECKS_PASSED"));

    // The base's own record still has only X.
    const record = baselineRecord(setup.runDir);
    assert.deepEqual(record.commands[0].failures, ["base X fails"]);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("baseline: a base whose failing output yields no test name keeps the strict rule", async () => {
  const setup = await setupConductor({
    checks: ["false"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);
    const record = baselineRecord(setup.runDir);
    assert.deepEqual(record.failures, [], "an output with no test name records no name");
    assert.deepEqual(record.commands[0].failures, []);
    assert.notEqual(record.commands[0].exitCode, 0, "the base really did fail");
    // Nothing was excused: the same unparsable failure is still a failure.
    assert.ok(!eventTypes(setup.runDir).includes("CHECKS_PASSED"));
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "check_failures_pre_existing").length, 0);
    // The view says the base fails but names nothing.
    const view = buildView(setup.runDir, setup.plan, false);
    assert.match(view.baseline ?? "", /base fails: 0 tests/);
    assert.match(view.baseline ?? "", /strict/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("baseline: the worker prompt and the reviewers' turn-2 prompt carry the base's failing tests as pre-existing", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-baseline-prompts-");
  const workerPromptLog = path.join(dir, "worker.prompts.log");
  const setup = await setupConductor({
    checks: ["node --test"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
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
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: workerPromptLog },
    extraReviewerEnv: (reviewer) => ({ FAKE_PI_PROMPT_LOG: path.join(dir, `${reviewer}.prompts.log`) }),
    deadlines: { ...FAST, reviewMs: 30_000, probeMs: 5_000 },
  });
  commitFailingTest(setup.repo.dir, "x.test.cjs", "base X fails");
  await setup.conductor.start();
  try {
    // Each reviewer's turn 2 is sent only after the whole turn-1 barrier.
    await waitFor(
      () =>
        (["M", "A", "B"] as Reviewer[]).every((r) => {
          const file = path.join(dir, `${r}.prompts.log`);
          return fs.existsSync(file) && fs.readFileSync(file, "utf8").includes("Turn 2 of 2");
        }),
      90_000,
      undefined,
      setup.runDir,
    );
    const worker = fs.readFileSync(workerPromptLog, "utf8");
    assert.match(worker, /Pre-existing check failures on the phase base \(NOT this phase's to fix\)/);
    // The names are shown under the command they failed in, so the prompt's
    // promise matches the gate's per-command rule (finding A-5).
    assert.match(worker, /- `node --test`: base X fails/);
    assert.match(worker, /for that same command/);
    for (const r of ["M", "A", "B"] as Reviewer[]) {
      const prompts = fs.readFileSync(path.join(dir, `${r}.prompts.log`), "utf8").split("\n=====\n");
      const turn2 = prompts.find((p) => p.includes("Turn 2 of 2"));
      assert.ok(turn2, `${r}'s turn 2 was captured`);
      assert.match(turn2!, /Pre-existing check failures on the phase base/);
      assert.match(turn2!, /- `node --test`: base X fails/);
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("baseline: the pre-existing prompt section is pure, and absent when the base has no parsed failures", () => {
  assert.deepEqual(baselinePromptLines([]), []);
  assert.deepEqual(baselinePromptLines(undefined), []);
  const command = (c: string, failures: string[]) => ({ command: c, exitCode: 1, signal: null, timedOut: false, durationMs: 1, failures });
  const section = baselinePromptLines([command("make check", ["x", "y"]), command("cargo test", ["z"])]).join("\n");
  assert.match(section, /Pre-existing check failures on the phase base \(NOT this phase's to fix\)/);
  assert.match(section, /- `make check`: x, y/);
  assert.match(section, /- `cargo test`: z/);
  // A command that exited 0, timed out or died by signal is not a baseline
  // failure, so it never reaches the prompt (findings M-2, B-1).
  const notAFailure = baselinePromptLines([
    { command: "wrapped", exitCode: 0, signal: null, timedOut: false, durationMs: 1, failures: ["q"] },
    { command: "killed", exitCode: null, signal: "SIGKILL", timedOut: false, durationMs: 1, failures: ["r"] },
  ]);
  assert.deepEqual(notAFailure, []);

  const contract = buildContract({
    id: "p1",
    goal: "g",
    acceptance: ["a"],
    checks: ["node --test"],
    boundaries: [],
    reserved: [],
  });
  const worker = buildWorkerPrompt(contract, undefined, undefined, undefined, [], [], undefined, [command("node --test", ["x"])]);
  assert.match(worker, /- `node --test`: x/);
  assert.ok(!buildWorkerPrompt(contract).includes("Pre-existing check failures"));
});

test("baseline: a base command that exits 0 contributes no names, even if it printed a FAILED line", async () => {
  const counter = `/tmp/tt-baseline-exit0-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  // Run 1 (the baseline) prints the FAILED line but exits 0; run 2 (the
  // candidate) prints it and exits 1. A wrapper like `cargo test || echo done`
  // must not smuggle the name into the excuse set (finding M-2).
  const command =
    `n=$(cat ${counter} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${counter};` +
    ` echo 'test pre_existing_x ... FAILED'; if [ $n -ge 2 ]; then exit 1; fi`;
  const setup = await setupConductor({
    checks: [command],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);
    const record = baselineRecord(setup.runDir);
    assert.equal(record.commands[0].exitCode, 0, "the base command exited 0");
    assert.deepEqual(record.failures, [], "a command that exited 0 names no pre-existing failure");
    assert.ok(!eventTypes(setup.runDir).includes("CHECKS_PASSED"), "the candidate's own failure must fail the gate");
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "check_failures_pre_existing").length, 0);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("baseline: a check killed by a signal is never excused, even when its output names only base failures", async () => {
  const counter = `/tmp/tt-baseline-signal-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  // Run 1 (the baseline) fails normally with baseX. Run 2 (the candidate)
  // prints the very same line and is then SIGKILLed — `exitCode: null` with a
  // non-null signal, `timedOut` false. Its truncated output names only a
  // pre-existing failure, so only the strict rule may be applied.
  const command =
    `n=$(cat ${counter} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${counter};` +
    ` echo 'test baseX ... FAILED'; if [ $n -eq 1 ]; then exit 1; fi; kill -KILL $$`;
  const setup = await setupConductor({
    checks: [command],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);
    const record = baselineRecord(setup.runDir);
    assert.deepEqual(record.commands[0].failures, ["baseX"], "the base failed normally, so its name is recorded");
    assert.ok(!eventTypes(setup.runDir).includes("CHECKS_PASSED"), "a signal death must never pass the gate");
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "check_failures_pre_existing").length,
      0,
      "a signal-killed check is never recorded as pre-existing",
    );
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("baseline: a node waits for the lock holder's record instead of running the checks again", async () => {
  const counter = `/tmp/tt-baseline-lock-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  const command = `n=$(cat ${counter} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${counter}; echo 'test pre_existing_x ... FAILED'; exit 1`;
  const count = () => (fs.existsSync(counter) ? Number(fs.readFileSync(counter, "utf8").trim()) : 0);
  const setup = await setupConductor({
    checks: [command],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  const programId = "prog-baseline-lock";
  fs.writeFileSync(path.join(setup.runDir, "program.json"), JSON.stringify({ programId, node: "n1" }));
  const tree = git(setup.repo.dir, ["rev-parse", "HEAD^{tree}"]);
  const baseSha = git(setup.repo.dir, ["rev-parse", "HEAD"]);
  const key = baselineKey(tree, [command]);
  const shared = path.join(path.dirname(setup.runDir), "programs", programId, "baselines", key);
  fs.mkdirSync(shared, { recursive: true });
  // A live holder (this test process) has already taken the lock. The node
  // must wait for its record rather than pay for the base check run again.
  fs.writeFileSync(path.join(shared, "lock.json"), JSON.stringify({ pid: process.pid, at: Date.now() }));
  await setup.conductor.start();
  try {
    setTimeout(() => {
      fs.writeFileSync(
        path.join(shared, "baseline.json"),
        JSON.stringify({
          baseSha,
          tree,
          key,
          at: new Date().toISOString(),
          commands: [{ command, exitCode: 1, signal: null, timedOut: false, durationMs: 1, failures: ["pre_existing_x"] }],
          failures: ["pre_existing_x"],
        }),
      );
    }, 500);
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_PASSED"), 90_000, undefined, setup.runDir);
    assert.equal(count(), 1, "the node waited for the shared record instead of running the baseline itself");
    const record = baselineRecord(setup.runDir);
    assert.deepEqual(record.failures, ["pre_existing_x"]);
    const reused = baselineEvents(setup.runDir).find((e) => e.reused === true);
    assert.ok(reused, "the adopted record is logged as reused");
    assert.equal(reused!.source, "program");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("baseline: runs once per base tree — a second program node with the same base reuses the shared record", async () => {
  const marker = `/tmp/tt-baseline-count-${randomUUID().slice(0, 8)}`;
  fs.rmSync(marker, { force: true });
  // A check that counts its own runs and always fails with one parsable name.
  const command = `n=$(cat ${marker} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${marker}; echo 'test pre_existing_x ... FAILED'; exit 1`;
  const count = () => (fs.existsSync(marker) ? Number(fs.readFileSync(marker, "utf8").trim()) : 0);

  const programId = "prog-baseline-once";
  const setup = await setupConductor({
    checks: [command],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  fs.writeFileSync(path.join(setup.runDir, "program.json"), JSON.stringify({ programId, node: "n1" }));
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_PASSED"), 90_000, undefined, setup.runDir);
  } finally {
    await setup.conductor.stop();
  }
  // Node 1 paid for exactly two runs: the baseline and the candidate check.
  assert.equal(count(), 2, "the first node's baseline and candidate check each ran once");

  // Node 1 published the record under the program's shared store.
  const tree = git(setup.repo.dir, ["rev-parse", "HEAD^{tree}"]);
  const key = baselineKey(tree, [command]);
  const shared = path.join(path.dirname(setup.runDir), "programs", programId, "baselines", key);
  assert.ok(fs.existsSync(path.join(shared, "baseline.json")), `expected the shared baseline at ${shared}`);
  assert.deepEqual(parseBaseline(JSON.parse(fs.readFileSync(path.join(shared, "baseline.json"), "utf8")))!.failures, [
    "pre_existing_x",
  ]);

  // A second node of the same program, same base tree and checks.
  const plan2: RunPlanFile = { ...setup.plan, phases: setup.plan.phases.map((p) => ({ ...p })) };
  const runDir2 = createRun(setup.runRoot, plan2);
  fs.writeFileSync(path.join(runDir2, "program.json"), JSON.stringify({ programId, node: "n2" }));
  const worker2 = writeScript(setup.scriptsDir, "worker-node2", { hello: defaultWorkerHello(), steps: [submitPhaseStep()] });
  const conductor2 = new Conductor({
    runDir: runDir2,
    plan: plan2,
    piCommand: process.execPath,
    piArgsPrefix: [FAKE_PI_PATH],
    piEnvFor: (role) => (role === "worker" ? { FAKE_PI_SCRIPT: worker2 } : undefined),
    deadlines: FAST,
  });
  try {
    await conductor2.start();
    await waitFor(() => eventTypes(runDir2).includes("CHECKS_PASSED"), 90_000, undefined, runDir2);
  } finally {
    await conductor2.stop();
  }
  // One more run only — node 2's candidate check. Its baseline came from the
  // shared record, so the base tree is paid for once between them.
  assert.equal(count(), 3, "the second node reused the shared baseline instead of running it again");
  const reused = baselineEvents(runDir2).find((e) => e.reused === true);
  assert.ok(reused, "node 2 must log a reused baseline");
  assert.equal(reused!.source, "program");
  // And the record is in node 2's own checks/base/, self-contained.
  assert.deepEqual(baselineRecord(runDir2).failures, ["pre_existing_x"]);

  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
  fs.rmSync(marker, { force: true });
});
