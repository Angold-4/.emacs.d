// Plan 05d (runtime findings #4, #10, #18, #25, #31, #33, #35): flaky tests
// and slow agent starts.
//
// A check that fails only on tests that pass when re-run alone must pass, with
// one FLAKE_OBSERVED per test; a check with a real failure among the new ones
// must fail and label each one for the worker's repair prompt. A base failure
// that passes alone is a `base flake`, never an excuse. A hello timeout retries
// the launch once and consumes no repair attempt; two in a row block the run as
// an environment problem.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { test } from "node:test";

import { checkFailureLines, checkFailurePromptLines, helloRetryTimeoutMs, runPaths } from "../../src/conductor.ts";
import { basePhase } from "../unit/helpers.ts";
import { parseBaseline, type Baseline } from "../../src/core/test-failures.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
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

function flakeEvents(runDir: string): Array<Record<string, unknown>> {
  return readEvents(runDir)
    .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "FLAKE_OBSERVED")
    .map((r) => r.event as Record<string, unknown>);
}

function baselineRecord(runDir: string): Baseline {
  const file = path.join(runPaths(runDir).checks, "base", "baseline.json");
  assert.ok(fs.existsSync(file), `expected the baseline record at ${file}`);
  const parsed = parseBaseline(JSON.parse(fs.readFileSync(file, "utf8")));
  assert.ok(parsed, "the baseline file must parse as a baseline record");
  return parsed!;
}

/** A check that always fails, printing one node-spec failing test per name. */
function failingCheck(names: readonly string[]): string {
  return `${names.map((n) => `printf '✖ %s\\n' '${n}'`).join("; ")}; exit 1`;
}

/** A `#+TT_RERUN:` template that exits 0 and prints a node pass line, so the
 * classification can see the named test really ran (finding M-1). `{name}` is
 * substituted as one already-quoted shell word, so the template must not quote
 * it itself. */
const PASS_TEMPLATE = "printf '✔ %s\\n' {name}; printf 'ℹ pass 1\\n'; exit 0";
/** A template that exits non-zero: the test reproduces alone. */
const FAIL_TEMPLATE = "printf '✖ %s\\n' {name}; exit 1";

test("launch retry: the retry limit scales with the session size and is capped at 60 s", () => {
  // No session: the retry keeps the configured limit.
  assert.equal(helloRetryTimeoutMs(10_000, 0), 10_000);
  // 1 MiB: 1024 KiB × 50 ms = 51.2 s, still under the cap.
  assert.equal(helloRetryTimeoutMs(10_000, 1024 * 1024), 51_200);
  // A huge session never waits longer than a minute.
  assert.equal(helloRetryTimeoutMs(10_000, 100 * 1024 * 1024), 60_000);
  // A base limit already above the scale is never lowered.
  assert.equal(helloRetryTimeoutMs(30_000, 1024), 30_000);
});

test("flake: the prompt labels each failing test, and an empty check failure adds nothing", () => {
  const phase = basePhase({
    runId: "r",
    phaseId: "p1",
    checks: {
      candidateSha: "C1",
      passed: false,
      failures: [
        { name: "real", reproducesAlone: true, loadOnly: false, failingExitCode: 1, rerunExitCodes: [1] },
        { name: "flaky", reproducesAlone: false, loadOnly: true, failingExitCode: 1, rerunExitCodes: [0] },
      ],
    },
  });
  assert.deepEqual(checkFailureLines(phase), [
    "`real`: reproduces alone (a real failure — fix it)",
    "`flaky`: load-only (passed when re-run alone; a flake — do not repair it)",
  ]);
  const section = checkFailurePromptLines(phase);
  assert.match(section.join("\n"), /Candidate check failures, each re-run alone/);
  // A passed check (or one whose output named no test) adds nothing.
  assert.deepEqual(checkFailurePromptLines(basePhase({ runId: "r", phaseId: "p1", checks: { candidateSha: "C1", passed: true } })), []);
  // Finding A-5: a failed check never reaches REVIEWING, so the reviewers of
  // the repaired candidate are shown the PREVIOUS candidate's split, kept
  // across the freeze in lastCheckFailures.
  const repaired = basePhase({
    runId: "r",
    phaseId: "p1",
    candidate: { sha: "C2", contractVersion: { snapshot: 1, sectionSha256: "x" } },
    checks: { candidateSha: "C2", passed: true },
    lastCheckFailures: {
      candidateSha: "C1",
      repairedBy: "C2",
      failures: [
        { name: "real", reproducesAlone: true, loadOnly: false, failingExitCode: 1, rerunExitCodes: [1] },
        { name: "flaky", reproducesAlone: false, loadOnly: true, failingExitCode: 1, rerunExitCodes: [0] },
      ],
    },
  });
  const repairedSection = checkFailurePromptLines(repaired).join("\n");
  assert.match(repairedSection, /previous candidate C1; every test was re-run alone/);
  assert.match(repairedSection, /`real`: reproduces alone \(a real failure\)/);
  assert.match(repairedSection, /`flaky`: load-only \(a flake; the check passed on it\)/);
  // The worker prompt helper never falls back to the previous candidate.
  assert.deepEqual(checkFailureLines(repaired), []);
  // Finding A-9: a LATER candidate (C3) whose freeze did not follow the check
  // failure is never shown the stale C1 split.
  const stale = checkFailurePromptLines({ ...repaired, candidate: { sha: "C3", contractVersion: { snapshot: 1, sectionSha256: "x" } } });
  assert.deepEqual(stale, []);
});

test("flake: a real `node --test` failure re-run by the built-in default passes the checks and records FLAKE_OBSERVED", async () => {
  // The 05j attempt-5 fixture shape: a real spec-reporter failure the parser
  // reads (with its `test at <file>` location), re-run by the built-in Node
  // default — no `#+TT_RERUN` template (finding M-4). The counter lives in
  // /tmp, never in the checkout: a file written inside the checkout would
  // trip `verifyIntegrity` and the check would fail with no classification.
  const counter = `/tmp/tt-flake-counter-${randomUUID().slice(0, 8)}`;
  fs.rmSync(counter, { force: true });
  const testFile = [
    "const test = require('node:test');",
    "const fs = require('node:fs');",
    `const counter = ${JSON.stringify(counter)};`,
    "test('no-unshown-ballots: a discovery after the barrier released', () => {",
    "  const n = fs.existsSync(counter) ? Number(fs.readFileSync(counter, 'utf8')) : 0;",
    "  fs.writeFileSync(counter, String(n + 1));",
    "  if (n >= 1) return;",
    "  throw new Error('the first run under load fails');",
    "});",
    "",
  ].join("\n");
  const setup = await setupConductor({
    checks: ["node --test flake.test.cjs"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `cat > flake.test.cjs <<'TTEOF'\n${testFile}TTEOF` },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(
      () => eventTypes(setup.runDir).includes("CHECKS_PASSED") && flakeEvents(setup.runDir).length > 0,
      90_000,
      undefined,
      setup.runDir,
    );

    assert.ok(!eventTypes(setup.runDir).includes("CHECKS_FAILED"));
    assert.ok(!eventTypes(setup.runDir).includes("REPAIR_ATTEMPT_STARTED"), "no repair round for a flake");

    const flakes = flakeEvents(setup.runDir);
    assert.equal(flakes.length, 1);
    assert.equal(flakes[0].name, "no-unshown-ballots: a discovery after the barrier released");
    assert.equal(flakes[0].failingExitCode, 1, "the failing run's exit status");
    assert.deepEqual(flakes[0].rerunExitCodes, [0], "the re-run's exit status");
    assert.equal(flakes[0].savedRound, true);
    assert.equal(typeof flakes[0].loadAverage, "number", "the load average at the failing run");
    // The re-run command is the built-in default, with the name escaped and
    // the file the reporter located.
    assert.equal(
      flakes[0].rerunCommand,
      "node --test --test-name-pattern 'no-unshown-ballots: a discovery after the barrier released' 'flake.test.cjs'",
    );
    // The excused check is recorded as load-only, not as pre-existing.
    const loadOnly = readEvents(setup.runDir).find((r) => r.kind === "check_failures_load_only");
    assert.ok(loadOnly, "the load-only classification is recorded");
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "check_failures_pre_existing").length, 0);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(counter, { force: true });
  }
});

test("flake: a re-run that exits 0 without running the test does not excuse a real failure", async () => {
  // Review finding M-1: `node --test --test-name-pattern` exits 0 when nothing
  // matches. A template that does the same must leave the failure standing.
  const setup = await setupConductor({
    checks: [`if grep -q tt-flake README.md; then ${failingCheck(["plan models route per seat"])}; fi`],
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'tt-flake\\n' >> README.md" }, submitPhaseStep()] }
        : { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] },
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  setup.plan.rerun = "printf 'nothing ran\\n'; exit 0";
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);
    const marked = readEvents(setup.runDir).find((r) => r.kind === "check_failure_new")!;
    const classifications = (marked.event as { classifications: Array<{ name: string; loadOnly: boolean }> }).classifications;
    assert.deepEqual(classifications.map((c) => [c.name, c.loadOnly]), [["plan models route per seat", false]]);
    assert.equal(flakeEvents(setup.runDir).length, 0);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("flake: a real failure reproduces alone, and a mixed check labels each test for the worker", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-flake-prompts-");
  const promptLog = path.join(dir, "worker.prompts.log");
  // The base passes; the candidate's own change (the marker line in README)
  // makes the check fail, so the failure is unambiguously this candidate's.
  const setup = await setupConductor({
    checks: [`if grep -q tt-flake README.md; then ${failingCheck(["plan models route per seat", "complete-ballots"])}; fi`],
    // Attempt 1 edits README and submits; the repair attempt is held so the
    // test can read its prompt and then stop.
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [{ kind: "call-sh", command: "printf 'tt-flake\\n' >> README.md" }, submitPhaseStep()],
          }
        : { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] },
    reviewerScriptFor: reviewerFor(),
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    deadlines: FAST,
  });
  // The first test reproduces (exit 1); the second passes alone (exit 0 with
  // the evidence that it ran).
  setup.plan.rerun = `case {name} in complete-ballots) ${PASS_TEMPLATE};; *) ${FAIL_TEMPLATE};; esac`;
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);
    await waitFor(() => fs.existsSync(promptLog) && fs.readFileSync(promptLog, "utf8").includes("REPAIR"), 90_000, undefined, setup.runDir);

    const prompt = fs.readFileSync(promptLog, "utf8");
    assert.match(prompt, /`plan models route per seat`: reproduces alone \(a real failure — fix it\)/);
    assert.match(prompt, /`complete-ballots`: load-only \(passed when re-run alone; a flake — do not repair it\)/);

    // The completion blames only the real failure, and the state records both.
    const completion = readEvents(setup.runDir).find(
      (r) => r.kind === "completion" && Array.isArray((r.event as { newFailures?: string[] }).newFailures),
    );
    assert.deepEqual((completion!.event as { newFailures: string[] }).newFailures, ["plan models route per seat"]);
    const marked = readEvents(setup.runDir).find((r) => r.kind === "check_failure_new");
    assert.ok(marked, "expected a check_failure_new record");
    const classifications = (marked!.event as { classifications: Array<{ name: string; loadOnly: boolean }> }).classifications;
    assert.deepEqual(
      classifications.map((c) => [c.name, c.loadOnly]),
      [["plan models route per seat", false], ["complete-ballots", true]],
    );
    // A check that fails does not emit a saved-round flake observation.
    assert.equal(flakeEvents(setup.runDir).filter((f) => f.savedRound === true).length, 0);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("flake: a check whose output names no test fails without any re-run", async () => {
  const marker = `/tmp/tt-flake-norerun-${randomUUID().slice(0, 8)}`;
  fs.rmSync(marker, { force: true });
  const setup = await setupConductor({
    // No parseable test name at all: the strict rule, no re-run.
    checks: ["echo 'error: build failed' >&2; exit 1"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  setup.plan.rerun = `printf '%s\\n' {name} >> ${marker}; exit 1`;
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);
    assert.ok(!fs.existsSync(marker), "an output that names no test must trigger no re-run");
    assert.equal(flakeEvents(setup.runDir).length, 0);
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "check_failure_new").length, 0);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(marker, { force: true });
  }
});

test("baseline: a base failure that passes alone is a `base flake` and never excuses the candidate", async () => {
  const marker = `/tmp/tt-base-flake-${randomUUID().slice(0, 8)}`;
  fs.rmSync(marker, { force: true });
  const setup = await setupConductor({
    checks: [failingCheck(["flaky base"])],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST,
  });
  // The base's single re-run (invocation 1) passes with evidence; the
  // candidate's re-run (invocation 2) fails, so it reproduces alone.
  setup.plan.rerun = `n=$(cat ${marker} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${marker}; if [ $n -eq 1 ]; then ${PASS_TEMPLATE}; fi; ${FAIL_TEMPLATE}`;
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);

    const record = baselineRecord(setup.runDir);
    assert.deepEqual(record.commands[0].failures, [], "a name that passes alone is not a base failure");
    assert.deepEqual(record.commands[0].flakes, ["flaky base"], "it is recorded as a base flake");
    assert.deepEqual(record.flakes, ["flaky base"]);
    const baseFlake = readEvents(setup.runDir).find((r) => r.kind === "baseline_flake");
    assert.ok(baseFlake, "the base flake is recorded in the log");

    // Never excused: the candidate's own failure of the same test is new.
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "check_failures_pre_existing").length, 0);
    const marked = readEvents(setup.runDir).find((r) => r.kind === "check_failure_new");
    assert.ok(marked, "the candidate's failure is recorded as new");
    assert.deepEqual((marked!.event as { newFailures: string[] }).newFailures, ["flaky base"]);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(marker, { force: true });
  }
});

test("launch retry: a worker that misses the first hello and answers the retry starts normally", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), helloDelayMs: 500, steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: { ...FAST, helloTimeoutMs: 200 },
  });
  // A large session being continued scales the retry limit far past the
  // delay, even when the machine is busy (50 ms per KiB, capped at 60 s).
  const sessionDir = path.join(runPaths(setup.runDir).sessions, "worker-p1");
  fs.mkdirSync(sessionDir, { recursive: true });
  fs.writeFileSync(path.join(sessionDir, "session.jsonl"), "x".repeat(1024 * 1024));
  await setup.conductor.start();
  try {
    // CHECKS_PASSED proves the worker created a candidate after the retry; the
    // review loop adds nothing to the assertions below.
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_PASSED"), 90_000, undefined, setup.runDir);
    const types = eventTypes(setup.runDir);
    assert.equal(types.filter((t) => t === "LAUNCH_RETRIED").length, 1, "the launch was retried once");
    assert.ok(!types.includes("ATTEMPT_TIMED_OUT"), "a hello timeout is not an attempt timeout");
    // The attempt count is unchanged: one attempt, no repair round.
    assert.equal(types.filter((t) => t === "ATTEMPT_STARTED").length, 1);
    assert.ok(!types.includes("REPAIR_ATTEMPT_STARTED"));
    assert.equal(setup.conductor.state.phase.repairRoundsUsed, 0);
    assert.ok(!types.includes("ENV_CHECK_FAILED"));
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("launch retry: two hello timeouts in a row block the run as an environment problem", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), helloDelayMs: 60_000, steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerFor(),
    deadlines: { ...FAST, helloTimeoutMs: 150 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.run === "ENV_BLOCKED", 90_000, undefined, setup.runDir);
    const types = eventTypes(setup.runDir);
    assert.equal(types.filter((t) => t === "LAUNCH_RETRIED").length, 1);
    assert.ok(types.includes("ENV_CHECK_FAILED"), "the second hello timeout is an environment problem");
    const envEvent = readEvents(setup.runDir).find(
      (r) => r.kind === "event" && (r.event as { type?: string }).type === "ENV_CHECK_FAILED",
    )!.event as Record<string, unknown>;
    assert.equal(envEvent.stage, "worker");
    // No repair attempt was consumed.
    assert.ok(!types.includes("REPAIR_ATTEMPT_STARTED"));
    assert.equal(setup.conductor.state.phase.repairRoundsUsed, 0);
    assert.equal(setup.conductor.state.phase.phase, "IMPLEMENTING", "the phase waits where it was");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("flake: a re-run that times out counts as `reproduces alone` and stays within the check deadline", async () => {
  const marker = `/tmp/tt-rerun-timeout-${randomUUID().slice(0, 8)}`;
  fs.rmSync(marker, { force: true });
  const setup = await setupConductor({
    checks: [`if grep -q tt-flake README.md; then ${failingCheck(["slow real failure"])}; fi`],
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [{ kind: "call-sh", command: "printf 'tt-flake\\n' >> README.md" }, submitPhaseStep()],
          }
        : { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] },
    reviewerScriptFor: reviewerFor(),
    // A short check deadline so the re-run's own deadline binds.
    deadlines: { ...FAST, checkMs: 1_500 },
  });
  setup.plan.rerun = `sleep 30; printf '%s\\n' {name} >> ${marker}`;
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 90_000, undefined, setup.runDir);
    const marked = readEvents(setup.runDir).find((r) => r.kind === "check_failure_new")!;
    const classifications = (marked.event as { classifications: Array<{ name: string; reproducesAlone: boolean; rerunExitCodes: Array<number | null> }> }).classifications;
    assert.equal(classifications[0].reproducesAlone, true, "a timed-out re-run proves nothing");
    assert.deepEqual(classifications[0].rerunExitCodes, [null], "the re-run was killed by the deadline");
    assert.ok(!fs.existsSync(marker), "the re-run was killed before it wrote its marker");
    assert.equal(flakeEvents(setup.runDir).length, 0);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(marker, { force: true });
  }
});
