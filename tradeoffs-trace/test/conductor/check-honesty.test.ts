// Plan 07e (E4), contract item R10: a check's recorded outcome is honest.
//
// A check that exits non-zero with no output is recorded with failure class
// `silent` (never as an ordinary failure that hides the silence), and a check
// the owner's stop killed is recorded as `interrupted`, never as failed. The
// disk-full half of the same item is proved by `test/effects/log.test.ts`,
// where the ENOSPC failure lives.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor, type FakePiStep, type TestConductorSetup } from "./harness.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { buildView } from "../../src/view.ts";

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
};

/** A worker that submits at once (the minimal round). */
function quickWorker(): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: "printf 'ok\\n' > marker.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** An empty review, so `stubReviews` dispatch still settles. */
function stubReviewer(seat: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
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
  };
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

/** Every `checks/<sha>/record.json` under the run. */
function checkRecords(runDir: string): Array<Record<string, unknown>> {
  const root = path.join(runDir, "checks");
  if (!fs.existsSync(root)) return [];
  const out: Array<Record<string, unknown>> = [];
  for (const sha of fs.readdirSync(root)) {
    const file = path.join(root, sha, "record.json");
    if (!fs.existsSync(file)) continue;
    try {
      out.push(JSON.parse(fs.readFileSync(file, "utf8")) as Record<string, unknown>);
    } catch {
      // a torn write is retried by the caller's waitFor
    }
  }
  return out;
}

test("CHECKS_FAILED names the stopping command rather than a base-excused failure", async () => {
  const excused = "printf 'not ok 1 - base failure\\n'; exit 1";
  const stopping = "test ! -f marker.txt";
  const setup = await setupConductor({
    checks: [excused, stopping, "echo should-not-run"], phaseChecks: [], rounds: 1,
    stubReviews: true, deadlines: { ...FAST, checkMs: 10_000 },
    workerScript: () => quickWorker(), reviewerScriptFor: stubReviewer,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "event" &&
      (r.event as { type?: string }).type === "CHECKS_FAILED"), 30_000, 25, setup.runDir);
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "check_failures_pre_existing"), "the first failure was excused against the base");
    const failed = readEvents(setup.runDir).find((r) => r.kind === "event" &&
      (r.event as { type?: string }).type === "CHECKS_FAILED")!.event as { failingStep?: string };
    assert.equal(failed.failingStep, stopping, "the compatibility summary names the command that actually stopped the run");
    const record = checkRecords(setup.runDir).find((r) => r.tier === "round")!;
    assert.equal((record.commands as unknown[]).length, 2, "the runner still stops at the first non-excused failure");
  } finally {
    await teardown(setup);
  }
});

test("plan 07e: a silent check is class silent and a check killed by a stop is interrupted", async () => {
  // 1. A check that exits non-zero with nothing to say is `silent`.
  const silent = await setupConductor({
    checks: ["exit 3"],
    phaseChecks: ["exit 3"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 10_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await silent.conductor.start();
    await waitFor(() => checkRecords(silent.runDir).length > 0, 60_000, 100, silent.runDir);
    const record = checkRecords(silent.runDir)[0]!;
    const command = (record.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.class, "silent", "a silent check is class silent, not an ordinary failure");
    assert.equal(command.failureClass, "silent");
    assert.equal(command.passed, false, "silence never passes a check");
    assert.equal(command.note, "check exited 3 with no output");
    const logText = fs.readFileSync(path.join(silent.runDir, "checks", String(record.candidateSha), String(command.log)), "utf8");
    assert.match(logText, /check exited 3 with no output/, "the check's own log says why it failed");
  } finally {
    await teardown(silent);
  }

  // 2. A check the owner's stop killed is `interrupted`, never `failed`.
  const stopped = await setupConductor({
    checks: ["sleep 6"],
    phaseChecks: ["sleep 30"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 60_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await stopped.conductor.start();
    await waitFor(
      () => readEvents(stopped.runDir).some((r) => r.kind === "intent" && String(r.actionId ?? "").startsWith("check-sh-")),
      60_000,
      50,
      stopped.runDir,
    );
    await stopped.conductor.stop();
    await waitFor(() => checkRecords(stopped.runDir).length > 0, 10_000, 50, stopped.runDir);
    const command = (checkRecords(stopped.runDir)[0]!.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.class, "interrupted", "a stop is not a failure");
    assert.equal(command.failureClass, "interrupted");
    assert.equal(command.note, "check interrupted (the owner stopped the run)", "an owner's stop names the owner");
  } finally {
    cleanupDir(stopped.runRoot);
    cleanupDir(stopped.scriptsDir);
  }

  // 3. A stop the conductor asked for itself (a full disk, a terminal phase)
  // is interrupted too, but it never claims the owner did it.
  const selfStopped = await setupConductor({
    checks: ["sleep 6"],
    phaseChecks: ["sleep 30"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 60_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await selfStopped.conductor.start();
    await waitFor(
      () => readEvents(selfStopped.runDir).some((r) => r.kind === "intent" && String(r.actionId ?? "").startsWith("check-sh-")),
      60_000,
      50,
      selfStopped.runDir,
    );
    await selfStopped.conductor.stop("conductor");
    await waitFor(() => checkRecords(selfStopped.runDir).length > 0, 10_000, 50, selfStopped.runDir);
    const command = (checkRecords(selfStopped.runDir)[0]!.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.class, "interrupted", "a conductor stop is still not a failure");
    assert.equal(command.note, "check interrupted (the conductor stopped the run)", "the note names the conductor's own stop");
    assert.doesNotMatch(String(command.note), /owner/, "a conductor self-stop never claims the owner stopped the run");
  } finally {
    cleanupDir(selfStopped.runRoot);
    cleanupDir(selfStopped.scriptsDir);
  }
});

test("a silent failing check step is named with its output", async () => {
  // R9/A4: the check chain runs in one shell under a step trace. The last
  // simple command before the non-zero exit is the failing step; its own (here
  // empty) output is recorded too, and the check's log names both.
  const setup = await setupConductor({
    checks: ["true && exit 3"],
    phaseChecks: ["true"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 10_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => checkRecords(setup.runDir).length > 0, 60_000, 100, setup.runDir);
    const record = checkRecords(setup.runDir)[0]!;
    const command = (record.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.passed, false, "the chain failed");
    assert.equal(command.failingStep, "exit 3", "the failing step is named in the check record");
    assert.equal(command.failingStepSilent, true, "a step that printed nothing is marked silent");
    assert.equal(command.failingStepOutput, "", "the failing step's own output is recorded (empty here)");
    const logText = fs.readFileSync(path.join(setup.runDir, "checks", String(record.candidateSha), String(command.log)), "utf8");
    assert.match(logText, /failing step: exit 3 \(no output\)/, "the check's log names the silent failing step");
    // R9: the same failing step and its output are in the status line too, so
    // the owner reads which step failed without opening the record.
    await waitFor(() => setup.conductor.state.phase.checks?.passed === false, 60_000, 100, setup.runDir);
    const view = buildView(setup.runDir, setup.plan, false);
    const status = [view.verdict ?? "", ...(view.addressing ?? [])].join("\n");
    assert.match(status, /failing step: `exit 3`, silent/, `the status line names the failing step: ${status}`);
    assert.doesNotMatch(status, /output:/, `a silent step's output is empty, so the status marks it silent instead: ${status}`);
  } finally {
    await teardown(setup);
  }
});

test("two failing check commands each name their own failing step", async () => {
  const commands = ["true && test -z x && true", "true && sh -c 'echo second-boom >&2; exit 4'"];
  for (const [index, command] of commands.entries()) {
    const setup = await setupConductor({
      checks: [command, "echo MUST_NOT_RUN"],
      phaseChecks: ["true"],
      rounds: 1,
      stubReviews: true,
      deadlines: { ...FAST, checkMs: 10_000 },
      workerScript: () => quickWorker(),
      reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
    });
    try {
      await setup.conductor.start();
      await waitFor(() => setup.conductor.state.phase.checks?.passed === false, 60_000, 100, setup.runDir);
      await setup.conductor.stop();
      const record = checkRecords(setup.runDir).find((r) => r.candidateSha === setup.conductor.state.phase.checks?.candidateSha)!;
      const recorded = record.commands as Array<Record<string, unknown>>;
      assert.equal(recorded.length, 1, "stop at the first failure; later checks cost nothing");
      const failed = recorded[0]!;
      assert.equal(failed.command, command);
      assert.equal(failed.failingStep, index === 0 ? "test -z x" : "sh -c echo second-boom >&2; exit 4");
      if (index === 0) {
        assert.equal(failed.failingStepOutput, "");
        assert.equal(failed.failingStepSilent, true);
      } else {
        assert.match(String(failed.failingStepOutput), /second-boom/);
      }
      // The command record, not the optional phase-wide compatibility
      // summary, is authoritative. Replay also supports logs lacking that
      // summary (or carrying a stale first-step summary from older code).
      const log = path.join(setup.runDir, "events.jsonl");
      const rows = fs.readFileSync(log, "utf8").trim().split("\n").map((line) => JSON.parse(line));
      for (const row of rows) {
        if (row.kind === "event" && row.event?.type === "CHECKS_FAILED") {
          row.event.failingStep = "old unrelated step";
          row.event.failingStepSilent = true;
        }
      }
      fs.writeFileSync(log, rows.map((row) => JSON.stringify(row)).join("\n") + "\n");
      const view = buildView(setup.runDir, setup.plan, false);
      const status = [view.verdict ?? "", ...(view.addressing ?? [])].join("\n");
      assert.match(status, index === 0
        ? /failing step: `test -z x`, silent/
        : /failing step: `sh -c echo second-boom >&2; exit 4`, output: second-boom/);
      assert.doesNotMatch(status, /old unrelated step/);
    } finally {
      await teardown(setup);
    }
  }
});

test("a failing check step's own output is named in the status line", async () => {
  // R9: a step that prints its failure is named with that output too — the
  // record keeps it and the status line shows it.
  const setup = await setupConductor({
    checks: ["true && sh -c 'echo boom >&2; exit 4'"],
    phaseChecks: ["true"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 10_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => checkRecords(setup.runDir).length > 0, 60_000, 100, setup.runDir);
    const record = checkRecords(setup.runDir)[0]!;
    const command = (record.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.passed, false, "the chain failed");
    assert.match(String(command.failingStep), /exit 4/, "the failing step is named in the check record");
    assert.match(String(command.failingStepOutput), /boom/, "that step's own output is recorded");
    await waitFor(() => setup.conductor.state.phase.checks?.passed === false, 60_000, 100, setup.runDir);
    const view = buildView(setup.runDir, setup.plan, false);
    const status = [view.verdict ?? "", ...(view.addressing ?? [])].join("\n");
    assert.match(status, /failing step: `sh -c echo boom >&2; exit 4`/, `the status line names the failing step: ${status}`);
    assert.match(status, /output: boom/, `the status line carries that step's own output: ${status}`);
  } finally {
    await teardown(setup);
  }
});

test("plan 07e: a check stopped by a full disk is class disk-full", async () => {
  // The full disk's own stop path (`#onDiskFull` calls `stop("disk full")`).
  // A running check it cuts off is recorded as `disk-full`, never
  // `interrupted`, and never as an ordinary failure.
  const setup = await setupConductor({
    checks: ["sleep 6"],
    phaseChecks: ["sleep 30"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 60_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => readEvents(setup.runDir).some((r) => r.kind === "intent" && String(r.actionId ?? "").startsWith("check-sh-")),
      60_000,
      50,
      setup.runDir,
    );
    await setup.conductor.stop("disk full");
    await waitFor(() => checkRecords(setup.runDir).length > 0, 10_000, 50, setup.runDir);
    const command = (checkRecords(setup.runDir)[0]!.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.class, "disk-full", "a full disk is its own class, never an ordinary failure");
    assert.equal(command.failureClass, "disk-full");
    assert.equal(command.note, "disk full");
    assert.equal(command.passed, false, "a full disk never passes the check");
    assert.doesNotMatch(String(command.note), /interrupted/, "a full disk is not an interruption");
  } finally {
    await teardown(setup);
  }
});
