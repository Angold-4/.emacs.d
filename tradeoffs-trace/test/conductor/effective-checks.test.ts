// R2 F04 regression gates: the executed check list must be the *effective*
// list — global `TT_CHECKS` followed by the phase contract's own `:CHECKS:`,
// deduped by exact command string — executed identically by `#runChecks`
// (candidate C) and `#runProbe` (probed integration I).
//
// Before the fix, both loops iterated `this.#plan.checks` and ignored
// `this.#state.phase.contract.checks`, so a phase-only check never ran.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { effectiveChecks, parseCheckRecord, rerunBudgetMs } from "../../src/core/checks.ts";
import { runPaths } from "../../src/conductor.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
} from "./harness.ts";
import type { State, Reviewer } from "../../src/core/types.ts";

function eventTypes(runDir: string): string[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => (r.event as { type: string }).type);
}

function submitOnlyWorker() {
  return {
    hello: defaultWorkerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_phase",
        args: { decisions: [], assumptions: [], deviations: [] },
      },
    ],
  };
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

const FAST_DEADLINES = { abortGraceMs: 200, termGraceMs: 200, helloTimeoutMs: 5_000, checkMs: 10_000, freezeMs: 15_000 };

test("R2.phase-fails: a failing phase-only check fails the gate and never accepts", async () => {
  const setup = await setupConductor({
    globalChecks: ["true"],
    phaseChecks: ["exit 1"],
    workerScript: submitOnlyWorker,
    deadlines: FAST_DEADLINES,
  });
  await setup.conductor.start();
  try {
    // The phase's own check must run at all: before the fix neither check
    // list was deduped/fused and `exit 1` was never executed, so the run
    // would have reached DONE via the global `true`. A failing check list is
    // fixed for the whole run, so `CHECKS_PASSED`/acceptance can never happen;
    // observing the first real `CHECKS_FAILED` is sufficient (and much faster
    // than waiting out all three repair rounds under parallel-suite load).
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 60_000);
    const types = eventTypes(setup.runDir);
    assert.ok(types.includes("CHECKS_FAILED"), `expected CHECKS_FAILED, got: ${types.join(",")}`);
    assert.ok(!types.includes("CHECKS_PASSED"), "a failing phase check must never also record CHECKS_PASSED");
    assert.ok(!types.includes("ACCEPTED"), "the phase must never be accepted");
    assert.ok(!types.includes("PUBLISH_COMPLETED"), "nothing may be published");
    assert.notEqual(setup.conductor.state.phase.phase, "DONE");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("R2.global-fails: a failing global check fails the gate, and the effective list dedupes exact duplicates once per gate", async () => {
  // (a) A failing global check, with the phase's own check passing, must
  // still fail — the global list is executed too.
  {
    const setup = await setupConductor({
      globalChecks: ["exit 1"],
      phaseChecks: ["true"],
      workerScript: submitOnlyWorker,
      deadlines: FAST_DEADLINES,
    });
    await setup.conductor.start();
    try {
      await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 60_000);
      const types = eventTypes(setup.runDir);
      assert.ok(types.includes("CHECKS_FAILED"), `expected CHECKS_FAILED, got: ${types.join(",")}`);
      assert.ok(!types.includes("ACCEPTED"), "the phase must never be accepted");
      assert.notEqual(setup.conductor.state.phase.phase, "DONE");
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
  }

  // (b) The dedup rule itself: global first, phase second, exact duplicates
  // dropped at their later occurrence.
  assert.deepEqual(effectiveChecks(["a", "b"], ["b", "c"]), ["a", "b", "c"]);
  assert.deepEqual(effectiveChecks([], []), []);

  // (c) And a real side effect: the *same* command in both lists runs
  // exactly once per gate, while the phase-only command also runs once per
  // gate. Two gates (checks on C, probe on I) means each marker file must
  // contain exactly two entries after a successful run. Without dedup the
  // duplicate would appear four times; if the phase list were ignored, the
  // phase-only marker would be absent.
  const marker = `/tmp/tt-dedup-${randomUUID().slice(0, 8)}`;
  const phaseMarker = `/tmp/tt-dedup-phase-${randomUUID().slice(0, 8)}`;
  const duplicateCommand = `printf x >> ${marker}`;
  const phaseOnlyCommand = `printf y >> ${phaseMarker}`;
  fs.rmSync(marker, { force: true });
  fs.rmSync(phaseMarker, { force: true });
  const setup = await setupConductor({
    globalChecks: ["true", duplicateCommand],
    phaseChecks: [duplicateCommand, phaseOnlyCommand],
    workerScript: submitOnlyWorker,
    reviewerScriptFor: reviewerFor(),
    // (c) asserts the checks run on *both* gates (C and the probed I), so it
    // must not take 2c's probe-reuse shortcut for an identical tree.
    probeReuse: false,
    deadlines: FAST_DEADLINES,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 30_000);
    // Plan 01e: the base baseline is a third execution of the effective list
    // (once on the base, before the first attempt), so each command now runs
    // once for the baseline, once for the C gate and once for the probe.
    // Without dedup the duplicate would appear six times; if the phase list
    // were ignored, the phase-only marker would be absent.
    assert.equal(fs.readFileSync(marker, "utf8"), "xxx", "the duplicate command must run exactly once per execution (baseline + checks + probe)");
    assert.equal(fs.readFileSync(phaseMarker, "utf8"), "yyy", "the phase-only command must run exactly once per execution (baseline + checks + probe)");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(marker, { force: true });
    fs.rmSync(phaseMarker, { force: true });
  }
});

test("R2.probe: a phase check passing on C but failing on the merged I yields PROBE_FAILED and blocks publication", async () => {
  const mark = `/tmp/tt-probe-${randomUUID().slice(0, 8)}`;
  fs.rmSync(mark, { force: true });
  // Plan 01e adds an execution before CHECKING: the base baseline runs the
  // same commands once on the base. So this counts runs: the first two — the
  // baseline and the C gate — pass, and the third (on the merged I, during
  // PROBING) fails.
  const command = `n=$(cat ${mark} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${mark}; test $n -lt 3`;
  const setup = await setupConductor({
    globalChecks: ["true"],
    phaseChecks: [command],
    workerScript: submitOnlyWorker,
    reviewerScriptFor: reviewerFor(),
    // The phase check must actually re-run on the probed I (and so fail
    // there), so this test cannot take 2c's probe-reuse shortcut.
    probeReuse: false,
    deadlines: FAST_DEADLINES,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("PROBE_FAILED"), 30_000);
    const types = eventTypes(setup.runDir);
    assert.ok(types.includes("CHECKS_PASSED"), "the effective list must have passed on candidate C first");
    assert.ok(types.includes("PROBE_FAILED"), "the probe must have run the effective list on its own checkout");
    assert.ok(!types.includes("PUBLISH_COMPLETED"), "a failed probe must block publication");
    assert.notEqual(setup.conductor.state.phase.phase, "DONE");
    // Evidence is recorded against the probed I as well as against C.
    const probeEvt = readEvents(setup.runDir).find(
      (r) => r.kind === "event" && (r.event as { type: string }).type === "PROBE_FAILED",
    );
    assert.match((probeEvt?.event as { evidence?: string }).evidence ?? "", /checks failed on probed integration/);
    const probeLogDir = path.join(runPaths(setup.runDir).checks, "probe");
    assert.ok(fs.existsSync(probeLogDir), "the probe's per-command records must live under a probe-specific directory");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(mark, { force: true });
  }

  // Control: the same run without that command in the list reaches DONE.
  const control = await setupConductor({
    globalChecks: ["true"],
    phaseChecks: ["true"],
    workerScript: submitOnlyWorker,
    reviewerScriptFor: reviewerFor(),
    deadlines: FAST_DEADLINES,
  });
  await control.conductor.start();
  try {
    await waitFor(() => control.conductor.state.phase.phase === "DONE", 30_000);
    assert.ok(eventTypes(control.runDir).includes("PUBLISH_COMPLETED"), "the control run must publish and reach DONE");
  } finally {
    await control.conductor.stop();
    cleanupDir(control.runRoot);
    cleanupDir(control.scriptsDir);
  }
});

// ---------------------------------------------------------------------------
// R2 F13: a check command that runs `node --test` must actually run the
// project's tests even when the conductor itself executes inside the Node
// test runner (as this very file does). Before the fix the child inherited
// `NODE_TEST_CONTEXT`/`NODE_TEST_WORKER_ID`, so its `node --test` printed
// `node:test run() is being called recursively within a test file. skipping
// running files.` and exited 0 — recording a real failure as a pass.
// ---------------------------------------------------------------------------

function freezeCandidateSha(runDir: string): string {
  const record = readEvents(runDir).find(
    (r) => r.kind === "event" && (r.event as { type: string }).type === "FREEZE_COMPLETED",
  );
  assert.ok(record, "expected a FREEZE_COMPLETED event");
  return (record!.event as { candidateSha: string }).candidateSha;
}

/** A worker that writes `file` into its worktree via `sh`, then submits —
 * so the frozen candidate actually contains the test project the check runs. */
function workerWriting(file: string, contents: string) {
  const write = `cat > ${file} <<'TTEOF'\n${contents}TTEOF`;
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: write },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

const NESTED_DEADLINES = { ...FAST_DEADLINES, checkMs: 20_000 };

test("R2.nested-negative: a nested failing `node --test` check is CHECKS_FAILED with real evidence, not a skipped pass", async () => {
  const setup = await setupConductor({
    checks: ["node --test"],
    workerScript: () =>
      workerWriting(
        "fail.test.js",
        [
          "const test = require('node:test');",
          "const assert = require('node:assert/strict');",
          "test('deliberately fails', () => { assert.equal(1, 2); });",
          "",
        ].join("\n"),
      ),
    deadlines: NESTED_DEADLINES,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 30_000);
    const types = eventTypes(setup.runDir);
    assert.ok(types.includes("CHECKS_FAILED"), `expected CHECKS_FAILED, got: ${types.join(",")}`);
    assert.ok(!types.includes("CHECKS_PASSED"), "a real failure must not be recorded as a pass");

    const logPath = path.join(runPaths(setup.runDir).checks, freezeCandidateSha(setup.runDir), "node_--test.log");
    assert.ok(fs.existsSync(logPath), `expected the check log at ${logPath}`);
    const log = fs.readFileSync(logPath, "utf8");
    assert.match(log, /deliberately fails/, "the log must show the real executed test name");
    assert.match(log, /fail 1/, "the log must show the real fail count");
    assert.match(log, /exit 1/, "the check must have exited nonzero");
    assert.ok(!log.includes("recursively within a test file"), "the recursion-skip warning must not appear");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("R2.nested-negative: a nested passing `node --test` check is CHECKS_PASSED with the known test name", async () => {
  const setup = await setupConductor({
    checks: ["node --test"],
    workerScript: () =>
      workerWriting(
        "pass.test.js",
        [
          "const test = require('node:test');",
          "const assert = require('node:assert/strict');",
          "test('known passing test', () => { assert.equal(2 + 2, 4); });",
          "",
        ].join("\n"),
      ),
    // This check passes, so the run proceeds past CHECKS_PASSED into the
    // review stage; give the reviewers a script so `stop()` can terminate
    // them cleanly (the sibling failing test stops before reviews start).
    reviewerScriptFor: reviewerFor(),
    deadlines: NESTED_DEADLINES,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_PASSED"), 30_000);
    const logPath = path.join(runPaths(setup.runDir).checks, freezeCandidateSha(setup.runDir), "node_--test.log");
    const log = fs.readFileSync(logPath, "utf8");
    assert.match(log, /known passing test/, "the log must show the known test name");
    assert.match(log, /pass 1/, "the log must show the real pass count");
    assert.ok(!log.includes("recursively within a test file"), "the recursion-skip warning must not appear");
    // In the merged stack the review loop runs after CHECKS_PASSED; let it
    // finish so `stop()` does not race a reviewer dispatch mid-teardown.
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 30_000);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

// Plan 05d: a re-run alone never overruns the failing check's own deadline.
test("a re-run's budget is what is left of the check's deadline, never negative", () => {
  assert.equal(rerunBudgetMs(10_000, 4_000), 6_000);
  assert.equal(rerunBudgetMs(10_000, 10_000), 0, "the deadline itself leaves no time");
  assert.equal(rerunBudgetMs(10_000, 12_000), 0, "a passed deadline is not negative");
});

// ---------------------------------------------------------------------------
// Plan 06c: the final check.
// ---------------------------------------------------------------------------

/** Run the REAL Emacs parser on an org plan and return its first phase. */
function parseOrgPhase(orgText: string): import("../../src/conductor.ts").RunPlanPhase {
  const dir = fs.mkdtempSync("/tmp/tt-06c-org-");
  const orgPath = path.join(dir, "PLAN.org");
  fs.writeFileSync(orgPath, orgText);
  const emacsLoad = fileURLToPath(new URL("../../../test/tradeoffs-trace-test.el", import.meta.url));
  const coreDir = fileURLToPath(new URL("../../../core", import.meta.url));
  const testDir = fileURLToPath(new URL("../../../test", import.meta.url));
  const out = execFileSync(
    "emacs",
    [
      "--batch",
      "-Q",
      "-L",
      coreDir,
      "-L",
      testDir,
      "-l",
      emacsLoad,
      "--eval",
      `(with-temp-buffer (insert-file-contents "${orgPath}") (org-mode) (setq buffer-file-name "${orgPath}") (princ (json-encode (plist-get (+tt-parse-plan) :plan))))`,
    ],
    { encoding: "utf8" },
  );
  fs.rmSync(dir, { recursive: true, force: true });
  const parsed = JSON.parse(out) as { phases: Array<import("../../src/conductor.ts").RunPlanPhase> };
  return parsed.phases[0];
}

function freezeShas(runDir: string): string[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event" && (r.event as { type: string }).type === "FREEZE_COMPLETED")
    .map((r) => (r.event as { candidateSha: string }).candidateSha);
}

function checkRecord(runDir: string, sha: string) {
  const file = path.join(runPaths(runDir).checks, sha, "record.json");
  assert.ok(fs.existsSync(file), `expected a check record at ${file}`);
  const raw = fs.readFileSync(file, "utf8");
  const record = parseCheckRecord(JSON.parse(raw));
  assert.ok(record, `the record must parse: ${raw}`);
  return record!;
}

const FINAL_DEADLINES = { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 10_000, workerAttemptMs: 30_000, checkMs: 20_000, probeMs: 20_000, freezeMs: 20_000, reviewMs: 20_000, evaluateMs: 10_000, panelMs: 10_000 };

/** A worker that writes a fresh file each attempt, so each repair freezes a
 * different tree. */
function writingWorker() {
  return (attempt: number) => ({
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh" as const, command: `printf '${attempt}\n' > attempt-${attempt}.txt` },
      { kind: "call-submit" as const, tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  });
}

/** A real two-turn reviewer that always passes (no ballots, no findings). */
function passingReviewer() {
  return (reviewer: Reviewer, state: State) => ({
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit" as const, tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" as const },
      {
        kind: "call-submit" as const,
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
  });
}

test("plan 06c: an org phase with TT_FINAL_CHECKS parsed by the real Emacs parser runs the final check only on the candidate about to be accepted", async () => {
  const finalMarker = `/tmp/tt-final-${randomUUID().slice(0, 8)}`;
  const checkMarker = `/tmp/tt-final-round-${randomUUID().slice(0, 8)}`;
  fs.rmSync(finalMarker, { force: true });
  fs.rmSync(checkMarker, { force: true });
  const finalCommand = `echo final >> ${finalMarker}`;
  // The baseline runs n=1 (pass), the first candidate n=2 (fail, so it is
  // repaired), the second n=3 (pass). The final command is separate.
  const roundCheck = `n=$(cat ${checkMarker} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${checkMarker}; test $n -ne 2`;
  const phase = parseOrgPhase(
    [
      "#+TITLE: 06c",
      "#+TT_REPO: /tmp/x",
      "#+TT_BRANCH: main",
      `#+TT_CHECKS: ${roundCheck}`,
      `#+TT_FINAL_CHECKS: ${finalCommand}`,
      "",
      "* Phase 1: p",
      "  :PROPERTIES:",
      "  :ID: p1",
      `  :CHECKS: ${roundCheck}`,
      "  :END:",
      "  Goal: g",
      "  Acceptance:",
      "  - it works",
    ].join("\n") + "\n",
  );
  assert.deepEqual(phase.finalChecks, [finalCommand], "the real parser wrote finalChecks into the phase");
  const setup = await setupConductor({
    phase,
    stubReviews: false,
    deadlines: FINAL_DEADLINES,
    workerScriptForAttempt: writingWorker(),
    reviewerScriptFor: passingReviewer(),
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const shas = freezeShas(setup.runDir);
    assert.equal(shas.length, 2, "one repair means two candidates");
    const [repairSha, acceptedSha] = shas;
    const repair = checkRecord(setup.runDir, repairSha);
    assert.equal(repair.tier, "round", "the repair candidate's record is a round run");
    assert.deepEqual(repair.finalCommands, [], "the repair candidate ran no final command");
    assert.ok(!repair.commands.some((c) => c.command === finalCommand), "the repair candidate did not run the final command");
    const accepted = checkRecord(setup.runDir, acceptedSha);
    assert.equal(accepted.tier, "final", "the accepted candidate's record is a final run");
    assert.deepEqual(accepted.finalCommands, [finalCommand], "the final command is named on the record");
    assert.ok(accepted.commands.some((c) => c.command === finalCommand), "the accepted candidate ran the final command");
    // Only one final run happened across the whole phase.
    assert.equal(fs.readFileSync(finalMarker, "utf8").trim().split("\n").length, 1, "the final command ran exactly once");
    assert.equal(eventTypes(setup.runDir).filter((t) => t === "FINAL_CHECKS_PASSED").length, 1);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(finalMarker, { force: true });
    fs.rmSync(checkMarker, { force: true });
  }
});

test("plan 06c: a failing final check sends the phase to repair and the next accepted candidate runs it again", async () => {
  const finalMarker = `/tmp/tt-final-fail-${randomUUID().slice(0, 8)}`;
  fs.rmSync(finalMarker, { force: true });
  // The final command fails the first time it runs (naming a test) and passes
  // the second; it only ever runs on the candidate about to be accepted.
  const finalCommand = `n=$(cat ${finalMarker} 2>/dev/null || echo 0); n=$((n+1)); echo $n > ${finalMarker}; if [ $n -lt 2 ]; then echo 'not ok 1 - final gate test'; exit 1; fi; echo 'ok 1 - final gate test'`;
  const phase = parseOrgPhase(
    [
      "#+TITLE: 06c",
      "#+TT_REPO: /tmp/x",
      "#+TT_BRANCH: main",
      "#+TT_CHECKS: true",
      `#+TT_FINAL_CHECKS: ${finalCommand}`,
      "",
      "* Phase 1: p",
      "  :PROPERTIES:",
      "  :ID: p1",
      "  :CHECKS: true",
      "  :END:",
      "  Goal: g",
      "  Acceptance:",
      "  - it works",
    ].join("\n") + "\n",
  );
  const promptLog = `/tmp/tt-final-prompt-${randomUUID().slice(0, 8)}.txt`;
  const setup = await setupConductor({
    phase,
    stubReviews: false,
    deadlines: FINAL_DEADLINES,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScriptForAttempt: writingWorker(),
    // Every round's reviews pass: only the final check can send the phase back.
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit" as const, tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" as const },
        {
          kind: "call-submit" as const,
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
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    assert.equal(fs.readFileSync(finalMarker, "utf8").trim(), "2", "the final check ran again for the next accepted candidate");
    const failed = readEvents(setup.runDir).find((r) => r.kind === "event" && (r.event as { type: string }).type === "FINAL_CHECKS_FAILED");
    assert.ok(failed, "a failing final check was recorded");
    assert.match((failed!.event as { evidence: string }).evidence, /final gate test/, "the failing final test is named");
    // The repair prompt names the failing final test.
    const prompts = fs.readFileSync(promptLog, "utf8");
    assert.match(prompts, /REPAIR/);
    assert.match(prompts, /final gate test/, "the repair prompt names the failing final test");
    const shas = freezeShas(setup.runDir);
    assert.equal(shas.length, 2);
    assert.equal(checkRecord(setup.runDir, shas[1]).tier, "final");
    const passed = eventTypes(setup.runDir).filter((t) => t === "FINAL_CHECKS_PASSED");
    assert.equal(passed.length, 1, "the accepted candidate ran the final check once");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(finalMarker, { force: true });
    fs.rmSync(promptLog, { force: true });
  }
});

test("plan 06c: a plan without TT_FINAL_CHECKS checks every candidate exactly as before", async () => {
  const marker = `/tmp/tt-nofinal-${randomUUID().slice(0, 8)}`;
  fs.rmSync(marker, { force: true });
  const command = `echo run >> ${marker}`;
  const setup = await setupConductor({
    checks: [command],
    probeReuse: false,
    deadlines: FINAL_DEADLINES,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\n' > sum.js" },
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
          },
        },
      ],
    }),
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    const types = eventTypes(setup.runDir);
    assert.ok(!types.includes("FINAL_CHECK_REQUIRED") && !types.includes("FINAL_CHECKS_PASSED") && !types.includes("FINAL_CHECKS_FAILED"), "no final-check event fires");
    // Same number of check runs as before: the base baseline, the candidate's
    // own gate and the probe (probeReuse is off).
    assert.equal(fs.readFileSync(marker, "utf8").trim().split("\n").length, 3, "baseline + candidate + probe, no extra run");
    const sha = setup.conductor.state.phase.candidate!.sha;
    assert.equal(checkRecord(setup.runDir, sha).tier, "round", "without a final check the record is a round run");
    assert.deepEqual(checkRecord(setup.runDir, sha).finalCommands, []);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(marker, { force: true });
  }
});

test("plan 06c: check records carry load average and free memory", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    deadlines: FINAL_DEADLINES,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\n' > sum.js" },
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
          },
        },
      ],
    }),
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    const record = checkRecord(setup.runDir, setup.conductor.state.phase.candidate!.sha);
    assert.equal(typeof record.load1, "number");
    assert.ok(record.load1 >= 0, `load1 is a load average, got ${record.load1}`);
    assert.equal(typeof record.freeMemMB, "number");
    assert.ok(record.freeMemMB > 0, `freeMemMB is in MiB, got ${record.freeMemMB}`);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
