// Plan 01b exit-gate tests: owner-wait notifications (design D4). Runtime doc
// §3 measured 8 owner waits totalling 366 minutes, one of them open for more
// than 24 hours because nothing told the owner. The rule under test:
//   - a run that exhausts its repair budget writes exactly one record to
//     `<root>/notifications.jsonl` and runs TT_NOTIFY_COMMAND exactly once;
//   - with the clock advanced past the 30-minute window it writes exactly one
//     reminder (marked `reminder: true`) and runs the notifier exactly once
//     more, and never a third time for that wait;
//   - a TT_NOTIFY_COMMAND that exits non-zero is logged and neither the run
//     nor the scheduler stops.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
} from "./harness.ts";
import type { Deadlines, RunPlanFile } from "../../src/conductor.ts";
import { notificationsPath, readNotifications } from "../../src/notify.ts";
import { appendProgramEvent, createProgram, notifyProgramOutcome, programPaths } from "../../src/program.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

const FAST: Partial<Deadlines> = {
  inboxPollMs: 40,
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 5_000,
  workerAttemptMs: 20_000,
  freezeMs: 10_000,
  checkMs: 5_000,
  probeMs: 5_000,
  reviewMs: 10_000,
};

function submitPhaseStep() {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

function submitReviewStep(reviewer: Reviewer, state: State) {
  return {
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
  };
}

/** A worker that submits immediately; checks always fail, so the run reaches
 * AWAITING_OWNER after three repair rounds. */
function awaitingOwnerOptions(extra: { now?: () => number } = {}) {
  return {
    checks: ["false"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    deadlines: FAST,
    ...extra,
  };
}

function countLines(file: string): number {
  try {
    return fs.readFileSync(file, "utf8").trim().split("\n").filter(Boolean).length;
  } catch {
    return 0;
  }
}

function withEnv(name: string, value: string, fn: () => Promise<void>): Promise<void> {
  const previous = process.env[name];
  process.env[name] = value;
  return fn().finally(() => {
    if (previous === undefined) delete process.env[name];
    else process.env[name] = previous;
  });
}

test("notify: a run that exhausts its repair budget writes one record naming the run and the reason, then exactly one reminder", async () => {
  const callsDir = fs.mkdtempSync("/tmp/tt-notify-calls-");
  const calls = path.join(callsDir, "calls.log");
  let clock = Date.parse("2026-01-01T00:00:00.000Z");
  const setup = await setupConductor(awaitingOwnerOptions({ now: () => clock }));
  try {
    await withEnv("TT_NOTIFY_COMMAND", `printf 'notify\\n' >> ${calls}`, async () => {
      await setup.conductor.start();
      await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);

      await waitFor(() => readNotifications(setup.runRoot).length === 1, 15_000);
      const first = readNotifications(setup.runRoot)[0];
      assert.equal(first.kind, "run");
      assert.equal(first.id, path.basename(setup.runDir));
      assert.match(first.reason, /checks kept failing|repair budget/);
      assert.equal(first.reminder, undefined, "the first record is not a reminder");
      assert.equal(countLines(calls), 1, "TT_NOTIFY_COMMAND ran exactly once");
      assert.ok(fs.existsSync(notificationsPath(setup.runRoot)));

      // The 30-minute reminder: advance the injected clock; the inbox poll
      // notices on its next beat.
      clock += 31 * 60_000;
      await waitFor(() => readNotifications(setup.runRoot).length === 2, 15_000);
      const second = readNotifications(setup.runRoot)[1];
      assert.equal(second.reminder, true, "the second record is the reminder");
      assert.equal(second.waitKey, first.waitKey, "both records describe the same wait");
      assert.equal(countLines(calls), 2, "TT_NOTIFY_COMMAND ran once more");
      assert.equal(readNotifications(setup.runRoot).length, 2);
    });

    // Many more polls past the reminder window: still no third record.
    clock += 60 * 60_000;
    await new Promise((resolve) => setTimeout(resolve, 300));
    assert.equal(readNotifications(setup.runRoot).length, 2, "never a third notification for one wait");
    assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(callsDir);
  }
});

test("notify: a failing TT_NOTIFY_COMMAND is logged and the run keeps running", async () => {
  const setup = await setupConductor(awaitingOwnerOptions());
  try {
    await withEnv("TT_NOTIFY_COMMAND", "exit 1", async () => {
      await setup.conductor.start();
      await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
      await waitFor(
        () =>
          readEvents(setup.runDir).some(
            (r) => r.kind === "error" && (r.event as { where?: string }).where === "notify",
          ),
        15_000,
      );
      assert.equal(readNotifications(setup.runRoot).length, 1, "the record is written before the notifier runs");
      assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER", "the run is parked, never stopped by a bad notifier");
      assert.ok(setup.conductor.state, "the conductor is still alive");
    });
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("notify: a program ending stuck writes one record, and a failing notifier only logs", async () => {
  const root = fs.mkdtempSync("/tmp/tt-notify-prog-");
  const phase = { id: "p", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] };
  const plan = { title: "a", repo: "/tmp/repo", integrationBranch: "main", checks: ["true"], phases: [phase] } as RunPlanFile;
  const dir = createProgram(root, { title: "stuck program", maxParallel: 1, entries: [{ id: "a", after: [], plan }] } as ProgramFile);
  try {
    appendProgramEvent(dir, { type: "NODE_BLOCKED", node: "a", reason: "merge conflict in src/x" });
    await withEnv("TT_NOTIFY_COMMAND", "exit 7", async () => {
      notifyProgramOutcome(dir, "stuck");
    });
    const notes = readNotifications(root);
    assert.equal(notes.length, 1, "one record per finished program");
    assert.equal(notes[0].kind, "program");
    assert.match(notes[0].reason, /program stuck/);
    assert.match(notes[0].reason, /merge conflict/);
    assert.match(fs.readFileSync(programPaths(dir).log, "utf8"), /notifier failed/, "the scheduler logs the failure and carries on");
    notifyProgramOutcome(dir, "stuck");
    assert.equal(readNotifications(root).length, 1, "a restarted scheduler does not re-notify a finished program");
  } finally {
    cleanupDir(root);
  }
});
