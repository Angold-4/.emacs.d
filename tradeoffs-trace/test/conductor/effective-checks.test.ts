// R2 F04 regression gates: the executed check list must be the *effective*
// list — global `TT_CHECKS` followed by the phase contract's own `:CHECKS:`,
// deduped by exact command string — executed identically by `#runChecks`
// (candidate C) and `#runProbe` (probed integration I).
//
// Before the fix, both loops iterated `this.#plan.checks` and ignored
// `this.#state.phase.contract.checks`, so a phase-only check never ran.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { test } from "node:test";

import { effectiveChecks } from "../../src/core/checks.ts";
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
    deadlines: FAST_DEADLINES,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 30_000);
    assert.equal(fs.readFileSync(marker, "utf8"), "xx", "the duplicate command must run exactly once per gate (checks + probe)");
    assert.equal(fs.readFileSync(phaseMarker, "utf8"), "yy", "the phase-only command must run exactly once per gate");
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
  // Passes on C (marker absent -> touch it), fails on I (marker already there).
  const command = `test -f ${mark} && exit 1 || touch ${mark}`;
  const setup = await setupConductor({
    globalChecks: ["true"],
    phaseChecks: [command],
    workerScript: submitOnlyWorker,
    reviewerScriptFor: reviewerFor(),
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
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
