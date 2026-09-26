// Phase 2b exit-gate tests: design §7.4/§9.3's inbox owner commands. The
// conductor reads `<run>/inbox/<id>.json` on start and while running, and
// applies each conductor-state command exactly once by appending its core
// event to `events.jsonl` (the log append *is* the effect), then moving the
// file to `inbox/applied/`. A stale binding is rejected visibly: a
// `command_rejected` log record and the file in `inbox/rejected/` with the
// reason beside it. Three properties, one test each:
//   - exactly-once application across a restart between the log append and
//     the file move (the command id is already in the log);
//   - a stale binding is rejected, never applied;
//   - a resolve command unparks AWAITING_OWNER and the run continues to DONE
//     with no further owner action.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  FAKE_PI_PATH,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";
import { Conductor, runPaths, type Deadlines } from "../../src/conductor.ts";
import type { OwnerRequest, Reviewer, State } from "../../src/core/types.ts";

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

function openRequests(state: State): OwnerRequest[] {
  return state.phase.ownerRequests.filter((r) => r.status === "open");
}

function inboxDir(setup: TestConductorSetup): string {
  return runPaths(setup.runDir).inbox;
}

function writeCommand(setup: TestConductorSetup, id: string, command: unknown): string {
  const file = path.join(inboxDir(setup), `${id}.json`);
  fs.writeFileSync(file, JSON.stringify(command));
  return file;
}

function eventsOfType(setup: TestConductorSetup, type: string): unknown[] {
  return readEvents(setup.runDir)
    .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === type)
    .map((r) => r.event);
}

/** Runs a phase whose checks always fail to the repair-budget gate, i.e.
 * AWAITING_OWNER with a plain `repair_budget_exhausted` owner request. */
async function setupAwaitingOwner(checks: string[]): Promise<TestConductorSetup> {
  const setup = await setupConductor({
    checks,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
  return setup;
}

test("owner-inbox: an id already in the log is only moved, never applied twice (restart between the log append and the file move)", async () => {
  const setup = await setupAwaitingOwner(["false"]);
  const noteText = "keep the lock hold under 50us";
  try {
    const commandId = "cmd-note-1";
    const inboxFile = writeCommand(setup, commandId, { kind: "note", phaseId: "p1", text: noteText });

    // The conductor applies the note (the log append is the effect) and
    // moves the file to applied/.
    await waitFor(() => eventsOfType(setup, "NOTE_ADDED").length === 1, 15_000);
    await waitFor(() => !fs.existsSync(inboxFile), 15_000);
    const applied = path.join(runPaths(setup.runDir).inboxApplied, `${commandId}.json`);
    assert.ok(fs.existsSync(applied), "the command file must have moved to inbox/applied/");
    assert.equal(setup.conductor.state.phase.ownerNotes?.length, 1, "the note must be queued once");

    // Simulate the crash design §9.3's boundary names: the event is in the
    // log but the file was never moved. Restart the conductor on the same
    // run directory; the log already carries the command id, so it must be
    // moved without a second append.
    await setup.conductor.stop();
    fs.copyFileSync(applied, inboxFile);

    const restarted = new Conductor({
      runDir: setup.runDir,
      plan: setup.plan,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      stubReviews: true,
      deadlines: { inboxPollMs: 40 },
    });
    await restarted.start();
    try {
      await waitFor(() => !fs.existsSync(inboxFile), 15_000);
      assert.ok(fs.existsSync(applied), "the replayed file must end up in inbox/applied/ again");
      assert.equal(eventsOfType(setup, "NOTE_ADDED").length, 1, "NOTE_ADDED must appear exactly once");
      assert.equal(restarted.state.phase.ownerNotes?.length, 1, "the note must not be queued twice");
      assert.equal(
        readEvents(setup.runDir).filter((r) => r.kind === "command_rejected").length,
        0,
        "an already-applied command must not be rejected",
      );
    } finally {
      await restarted.stop();
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("owner-inbox: a stale binding is rejected visibly, never applied", async () => {
  const setup = await setupAwaitingOwner(["false"]);
  try {
    const request = openRequests(setup.conductor.state)[0];
    assert.ok(request, "expected an open owner request at AWAITING_OWNER");
    const commandId = "cmd-stale-resolve";
    const inboxFile = writeCommand(setup, commandId, {
      kind: "resolve",
      requestId: request.id,
      // Stale: the request is still v1. The candidate/contract are current,
      // so the only thing wrong is the record version the sender saw.
      requestVersion: request.version + 1,
      option: "grant",
      boundCandidateSha: setup.conductor.state.phase.candidate!.sha,
      boundContractVersion: setup.conductor.state.phase.contract.contractVersion,
    });

    await waitFor(
      () => readEvents(setup.runDir).some((r) => r.kind === "command_rejected"),
      15_000,
    );
    await waitFor(() => !fs.existsSync(inboxFile), 15_000);

    const rejected = path.join(runPaths(setup.runDir).inboxRejected, `${commandId}.json`);
    assert.ok(fs.existsSync(rejected), "the stale command must move to inbox/rejected/");
    const reason = fs.readFileSync(path.join(runPaths(setup.runDir).inboxRejected, `${commandId}.reason.txt`), "utf8");
    assert.match(reason, /changed v\d+ → v\d+ since you viewed it/, "the rejection must name what changed");

    assert.equal(eventsOfType(setup, "OWNER_REQUEST_RESOLVED").length, 0, "a stale command must never apply");
    assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER", "the phase must stay parked");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("owner-inbox: a resolve command unparks AWAITING_OWNER, the queued note reaches the next attempt's prompt, and the run reaches DONE with no further owner action", async () => {
  const markerDir = fs.mkdtempSync("/tmp/tt-inbox-marker-");
  const marker = path.join(markerDir, "checks-pass");
  const promptLog = path.join(markerDir, "worker-prompts.log");
  const noteText = "NOTE-QUEUED-BEFORE-RESOLVE: keep the lock hold under 50us";
  const setup = await setupConductor({
    checks: [`test -f ${marker}`],
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
    const request = openRequests(setup.conductor.state)[0];
    assert.ok(request, "expected an open owner request at AWAITING_OWNER");
    const requestId = request.id;

    // These two use the decision view's own encoding (type + the full §7.1
    // binding tuple), exactly what core/init-tradeoffs-trace.el writes; the
    // note sorts before the resolve, so it applies first in the same scan.
    const runId = setup.conductor.state.phase.runId;
    writeCommand(setup, "cmd-aaa-note", {
      commandId: "cmd-aaa-note",
      type: "note",
      text: noteText,
      binding: { runId, phaseId: "p1" },
    });
    // Let the next candidate's checks (and probe) pass, then grant 3 more
    // repair rounds through the inbox.
    fs.writeFileSync(marker, "ok\n");
    writeCommand(setup, "cmd-resolve-grant", {
      commandId: "cmd-resolve-grant",
      type: "resolve",
      recordKind: "request",
      option: "grant",
      binding: {
        runId,
        phaseId: "p1",
        candidateSha: setup.conductor.state.phase.candidate!.sha,
        contractVersion: setup.conductor.state.phase.contract.contractVersion,
        recordId: requestId,
        recordVersion: request.version,
      },
    });

    await waitFor(() => setup.conductor.state.phase.phase !== "AWAITING_OWNER", 15_000);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000);

    assert.equal(setup.conductor.state.phase.phase, "DONE");
    const resolved = eventsOfType(setup, "OWNER_REQUEST_RESOLVED") as Array<{ requestId: string; option: string }>;
    assert.equal(resolved.length, 1);
    assert.equal(resolved[0].requestId, requestId);
    assert.equal(resolved[0].option, "grant");
    assert.ok(setup.conductor.state.phase.publishedI, "the run must publish and reach DONE");
    // The queued note was delivered verbatim in the repair attempt's prompt.
    assert.ok(fs.existsSync(promptLog), "expected the worker prompt to be captured");
    assert.match(fs.readFileSync(promptLog, "utf8"), /NOTE-QUEUED-BEFORE-RESOLVE: keep the lock hold under 50us/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(markerDir, { recursive: true, force: true });
  }
});

test("owner-inbox: an 'unneeded' command leaves the request open, so AWAITING_OWNER is never stranded and a later resolve still unparks it", async () => {
  const markerDir = fs.mkdtempSync("/tmp/tt-inbox-unneeded-");
  const marker = path.join(markerDir, "checks-pass");
  const setup = await setupConductor({
    checks: [`test -f ${marker}`],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
    const request = openRequests(setup.conductor.state)[0];
    assert.ok(request, "expected an open owner request at AWAITING_OWNER");
    const runId = setup.conductor.state.phase.runId;
    const candidateSha = setup.conductor.state.phase.candidate!.sha;
    const contractVersion = setup.conductor.state.phase.contract.contractVersion;

    // The owner's `u` key: the metric is recorded, the request stays open.
    writeCommand(setup, "cmd-aaa-unneeded", {
      commandId: "cmd-aaa-unneeded",
      type: "unneeded",
      recordKind: "request",
      binding: { runId, phaseId: "p1", candidateSha, contractVersion, recordId: request.id, recordVersion: request.version },
    });
    await waitFor(() => (setup.conductor.state.phase.unneededRequestIds ?? []).includes(request.id), 15_000);
    assert.equal(openRequests(setup.conductor.state).length, 1, "the request must stay open: 'unneeded' is a metric, not a resolution");
    assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER", "the phase stays parked until the request is actually answered");
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "command_rejected").length,
      0,
      "an unneeded command is applied, never rejected",
    );

    // The blocking finding's exact repro: the record-less fallback request
    // remains resolvable afterwards.
    fs.writeFileSync(marker, "ok\n");
    writeCommand(setup, "cmd-bbb-resolve", {
      commandId: "cmd-bbb-resolve",
      type: "resolve",
      recordKind: "request",
      option: "grant",
      binding: { runId, phaseId: "p1", candidateSha, contractVersion, recordId: request.id, recordVersion: request.version },
    });
    await waitFor(() => setup.conductor.state.phase.phase !== "AWAITING_OWNER", 15_000);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    const resolved = eventsOfType(setup, "OWNER_REQUEST_RESOLVED") as Array<{ requestId: string; option: string }>;
    assert.equal(resolved.length, 1);
    assert.equal(resolved[0].requestId, request.id);
    assert.equal(resolved[0].option, "grant");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(markerDir, { recursive: true, force: true });
  }
});

test("owner-inbox: a queued note is delivered in the next worker attempt (and, as an owner directive, in every later one)", async () => {
  const markerDir = fs.mkdtempSync("/tmp/tt-inbox-note-once-");
  const promptLog = path.join(markerDir, "worker-prompts.log");
  const noteText = "ONCE-ONLY-NOTE: keep the lock hold under 50us";
  const setup = await setupConductor({
    checks: ["false"],
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
    const request = openRequests(setup.conductor.state)[0];
    assert.ok(request, "expected an open owner request at AWAITING_OWNER");
    const runId = setup.conductor.state.phase.runId;
    const candidateSha = setup.conductor.state.phase.candidate!.sha;
    const contractVersion = setup.conductor.state.phase.contract.contractVersion;

    // Note first (sorts before the grant) so it is queued for the very next
    // attempt; grant adds 3 rounds so several more attempts follow.
    writeCommand(setup, "cmd-aaa-note", {
      commandId: "cmd-aaa-note",
      type: "note",
      text: noteText,
      binding: { runId, phaseId: "p1" },
    });
    writeCommand(setup, "cmd-bbb-grant", {
      commandId: "cmd-bbb-grant",
      type: "resolve",
      recordKind: "request",
      option: "grant",
      binding: { runId, phaseId: "p1", candidateSha, contractVersion, recordId: request.id, recordVersion: request.version },
    });

    // Let every granted repair attempt run (checks keep failing), so if the
    // note were re-sent it would show up again.
    await waitFor(
      () => (eventsOfType(setup, "REPAIR_ATTEMPT_STARTED").length >= 5) && setup.conductor.state.phase.phase === "AWAITING_OWNER",
      90_000,
    );
    assert.ok(fs.existsSync(promptLog), "expected worker prompts to be captured");
    const prompts = fs.readFileSync(promptLog, "utf8").split("\n=====\n").filter((p) => p.trim().length > 0);
    // The note's own queue entry is delivered to exactly the next attempt…
    const withNote = prompts.filter((p) => p.includes(`Owner notes: ${noteText}`));
    assert.equal(withNote.length, 1, "the note's own queue entry reaches exactly one worker attempt's prompt");
    assert.equal(setup.conductor.state.phase.deliveredNoteCount, 1, "exactly one note must be recorded as delivered");
    // …and, plan 01i, every input is an owner directive in force: every
    // later prompt quotes it verbatim, newest last.
    const firstWithNote = prompts.findIndex((p) => p.includes(`Owner notes: ${noteText}`));
    const later = prompts.slice(firstWithNote);
    assert.ok(later.length >= 2, "several later attempts ran");
    for (const p of later) assert.ok(p.includes(`OD-1: ${noteText}`), "every later prompt carries the directive");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(markerDir, { recursive: true, force: true });
  }
});

test("owner-inbox: an empty (still-being-written) file is retried, never rejected", async () => {
  const setup = await setupAwaitingOwner(["false"]);
  try {
    const commandId = "cmd-partial";
    const file = path.join(inboxDir(setup), `${commandId}.json`);
    fs.writeFileSync(file, "");
    // Several polls elapse at FAST.inboxPollMs (40ms).
    await new Promise((resolve) => setTimeout(resolve, 250));
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "command_rejected").length,
      0,
      "an empty mid-write file must not be rejected permanently",
    );
    assert.ok(fs.existsSync(file), "the empty file must be left for the next poll");

    // The writer finishes; the same file now applies normally.
    fs.writeFileSync(file, JSON.stringify({ kind: "note", phaseId: "p1", text: "late write" }));
    await waitFor(() => eventsOfType(setup, "NOTE_ADDED").length === 1, 15_000);
    assert.equal(eventsOfType(setup, "NOTE_ADDED").length, 1);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
