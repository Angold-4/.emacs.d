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
import { runPaths } from "../../src/conductor.ts";
import { notifierCommand, notificationsPath, notify, readNotifications, type NotificationRecord } from "../../src/notify.ts";
import { appendProgramEvent, createProgram, notifyProgramOutcome, programPaths, runScheduler } from "../../src/program.ts";
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

test("notify: an unwritable notifications file is logged and never storms the notifier", () => {
  const root = fs.mkdtempSync("/tmp/tt-notify-unwritable-");
  const calls = path.join(root, "calls.log");
  // A directory where the JSONL file should be: appendFileSync throws EISDIR.
  fs.mkdirSync(notificationsPath(root));
  const errors: string[] = [];
  try {
    for (let i = 0; i < 3; i++) {
      notify(
        { id: "r1", kind: "run", title: "t", reason: "x", waitKey: "k" },
        { root, env: { TT_NOTIFY_COMMAND: `printf 'x\\n' >> ${calls}` }, onError: (m) => errors.push(m) },
      );
    }
    assert.equal(countLines(calls), 0, "the notifier never runs when no record could be written");
    assert.equal(errors.length, 3, "every failed write is logged, and only logged");
    assert.match(errors[0], /notifications\.jsonl/);
    assert.equal(readNotifications(root).length, 0);
  } finally {
    cleanupDir(root);
  }
});

test("notify: the macOS default is osascript argv, never a shell-quoted reason", () => {
  const record: NotificationRecord = {
    id: "r1",
    kind: "run",
    title: "13f vendor",
    node: "13f",
    // Model-written prose: the quote here is what broke the old shell form.
    reason: "the worker didn't add the blend_runs test: finding F-1 open",
    waitKey: "k",
    at: "2026-01-01T00:00:00.000Z",
  };
  // The override is the operator's command and runs through sh.
  assert.deepEqual(notifierCommand(record, { TT_NOTIFY_COMMAND: "true" }), { command: "/bin/sh", args: ["-c", "true"] });
  const spec = notifierCommand(record, {});
  if (process.platform !== "darwin") {
    assert.equal(spec, undefined, "no default notifier off macOS");
    return;
  }
  assert.equal(spec!.command, "osascript", "osascript runs directly, with no shell to close");
  assert.equal(spec!.args[0], "-e");
  assert.match(spec!.args[1]!, /display notification/);
  assert.ok(spec!.args[1]!.includes("didn't"), "a single quote survives inside the argv element");
});

test("notify: a stuck program keeps its scheduler watching, then reminds once and exits", async () => {
  const root = fs.mkdtempSync("/tmp/tt-notify-stuck-");
  const calls = path.join(root, "calls.log");
  const phase = { id: "p", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] };
  // A repo that cannot exist: prepareBranch fails and the node is BLOCKED,
  // which is exactly the program-stuck case the reminder covers.
  const plan = { title: "a", repo: path.join(root, "no-such-repo"), integrationBranch: "main", checks: ["true"], phases: [phase] } as RunPlanFile;
  const dir = createProgram(root, { title: "stuck program", maxParallel: 1, entries: [{ id: "a", after: [], plan }] } as ProgramFile);
  try {
    await withEnv("TT_NOTIFY_COMMAND", `printf 'notify\\n' >> ${calls}`, async () => {
      const outcome = await runScheduler(dir, { runRoot: root, launch: () => {}, pollMs: 20, notifyReminderMs: 120 });
      assert.equal(outcome, "stuck");
    });
    const notes = readNotifications(root);
    assert.equal(notes.length, 2, "the stuck program is announced once and reminded once");
    assert.equal(notes[0].kind, "program");
    assert.equal(notes[0].reminder, undefined);
    assert.match(notes[0].reason, /program stuck/);
    assert.equal(notes[1].reminder, true);
    assert.equal(notes[1].waitKey, notes[0].waitKey);
    assert.equal(countLines(calls), 2);
    assert.match(fs.readFileSync(programPaths(dir).log, "utf8"), /program stuck/);
  } finally {
    cleanupDir(root);
  }
});

test("notify: resolving one request of a park does not write a second record for the same park", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    // Real reviewers: two rejected decisions are what leave several open
    // owner requests in one park (the runtime doc's 13f/13j shape).
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: ["keep the retry loop", "cap the queue at 64"].map((choice, i) => ({
              choice,
              whyItMatters: `this is the phase's choice ${i + 1}`,
              alternatives: [{ option: "do it the other way", consequence: "a different trade-off" }],
              recommendation: { choice, reason: "measured acceptable" },
              classProposal: "delegated",
            })),
            assumptions: [],
            deviations: [],
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => {
      const decisions = state.phase.decisions.filter((d) => d.source === "worker");
      return {
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
              ballots: decisions.map((d) => ({
                decisionId: d.id,
                vote: "reject",
                rationale: "the trade-off is not the one the plan asked for",
                evidence: ["read the diff"],
              })),
              findings: [],
            },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000);
    const open = setup.conductor.state.phase.ownerRequests.filter((r) => r.status === "open");
    assert.ok(open.length >= 2, `expected several open requests in one park, got ${open.length}`);
    await waitFor(() => readNotifications(setup.runRoot).length === 1, 15_000);
    const first = readNotifications(setup.runRoot)[0];

    // Resolve the NEWEST request — the one the old (request-id) key used —
    // so this really discriminates: another request remains open, the phase
    // stays parked, and the park episode is unchanged.
    const phase = setup.conductor.state.phase;
    const target = open[open.length - 1];
    fs.writeFileSync(
      path.join(runPaths(setup.runDir).inbox, "cmd-resolve-one.json"),
      JSON.stringify({
        commandId: "cmd-resolve-one",
        type: "resolve",
        recordKind: "request",
        option: "accept_as_implemented",
        binding: {
          runId: phase.runId,
          phaseId: "p1",
          candidateSha: phase.candidate!.sha,
          contractVersion: phase.contract.contractVersion,
          recordId: target.id,
          recordVersion: target.version,
        },
      }),
    );
    await waitFor(
      () => setup.conductor.state.phase.ownerRequests.find((r) => r.id === target.id)?.status === "resolved",
      15_000,
    );
    assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER", "the remaining request keeps the phase parked");
    // Several polls elapse; the park is the same episode, so no second record.
    await new Promise((resolve) => setTimeout(resolve, 250));
    assert.equal(readNotifications(setup.runRoot).length, 1, "resolving within a park is not a new wait");
    assert.equal(readNotifications(setup.runRoot)[0].waitKey, first.waitKey);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
