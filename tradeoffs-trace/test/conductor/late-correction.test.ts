// Plan 06k1 (A5): a correction that arrives while the phase is reviewing or
// final-checking is queued for the next attempt's prompt, and grants its own
// round even when the budget is spent. Nothing an owner sends is applied to a
// stage that can no longer use it, or dropped.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { reduce } from "../../src/core/reduce.ts";
import { accept } from "../../src/core/predicate.ts";
import { acceptInput } from "../../src/core/owner-inbox.ts";
import { runPaths } from "../../src/conductor.ts";
import { basePhase, baseState, CV } from "../unit/helpers.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

const FAST = {
  inboxPollMs: 40,
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 5_000,
  workerAttemptMs: 60_000,
  freezeMs: 30_000,
  checkMs: 30_000,
  probeMs: 30_000,
  reviewMs: 60_000,
  panelMs: 8_000,
};

function writeCommand(runDir: string, id: string, command: unknown): void {
  fs.writeFileSync(path.join(runPaths(runDir).inbox, `${id}.json`), JSON.stringify(command));
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

test("plan 06k1: a correction sent during review or final checks becomes the next attempt's correction", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k1-late-");
  const promptLog = path.join(dir, "worker-prompts.log");
  const correctionReview = "CORRECTION-DURING-REVIEW: cap the retry loop at 64";
  const correctionFinal = "CORRECTION-DURING-FINAL-CHECK: name the marker final-ok.txt";
  const phase = {
    id: "p1",
    goal: "do the thing",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    // The final check fails until attempt 3 writes the file it looks for, so
    // the phase reaches FINAL_CHECKING twice and DONE on the third candidate.
    finalChecks: ["sleep 2; test -f final-ok.txt"],
  };
  const setup = await setupConductor({
    phase,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-sh",
          command: attempt === 3 ? "printf 'ok\\n' > final-ok.txt" : `printf 'attempt ${attempt}\\n' > attempt-${attempt}.txt`,
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } => {
      const round = state.phase.round ?? 1;
      const open = state.phase.findings.filter((f) => f.status === "open");
      const findings =
        round === 1 && reviewer === "M"
          ? [{ kind: "defect", severity: "blocking", evidence: "README.md:1 the thing is not done" }]
          : [];
      const findingStatements = round >= 2 ? open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })) : [];
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
              phaseId: "p1",
              candidateSha: "$TT_CANDIDATE_SHA",
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements,
              findings,
            },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    // A correction written while the phase is REVIEWING.
    await waitFor(() => setup.conductor.state.phase.phase === "REVIEWING", 60_000, 50, setup.runDir);
    writeCommand(setup.runDir, "cmd-late-review", {
      type: "correction",
      text: correctionReview,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    await waitFor(
      () => (setup.conductor.state.phase.ownerInputs ?? []).some((i) => i.id === "cmd-late-review" && i.state === "queued"),
      30_000,
      50,
      setup.runDir,
    );

    // A correction written while the phase is FINAL_CHECKING.
    await waitFor(() => setup.conductor.state.phase.phase === "FINAL_CHECKING", 90_000, 50, setup.runDir);
    writeCommand(setup.runDir, "cmd-late-final", {
      type: "correction",
      text: correctionFinal,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    await waitFor(
      () => (setup.conductor.state.phase.ownerInputs ?? []).some((i) => i.id === "cmd-late-final" && i.state === "queued"),
      30_000,
      50,
      setup.runDir,
    );

    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);

    // Both corrections reached a worker attempt's prompt, verbatim.
    const prompts = fs.readFileSync(promptLog, "utf8");
    assert.match(prompts, new RegExp(correctionReview));
    assert.match(prompts, new RegExp(correctionFinal));
    // Neither was refused.
    assert.ok(
      (setup.conductor.state.phase.ownerInputs ?? []).every((i) => i.state !== "refused"),
      "a queued correction is never refused",
    );
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06k1: a queued correction blocks acceptance until NOTES_DELIVERED clears it", () => {
  const K = CV();
  const ready = basePhase({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: { reviewer: "M", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } as never },
      A: { review: { reviewer: "A", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } as never },
      B: { review: { reviewer: "B", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } as never },
    },
  });
  assert.equal(accept(ready, "C1", K), true, "the candidate is otherwise acceptable");
  const queued = { ...ready, queuedCorrections: [{ id: "C-1", text: "use UTC everywhere" }] };
  assert.equal(accept(queued, "C1", K), false, "a queued correction is an acceptance obligation");
  const delivered = reduce(
    baseState(queued),
    { type: "NOTES_DELIVERED", phaseId: "p1", count: 1 },
  );
  assert.ok(delivered.ok, "the delivery applies");
  assert.deepEqual(delivered.state.phase.queuedCorrections, [], "delivery clears the obligation");
  assert.equal(accept(delivered.state.phase, "C1", K), true, "once delivered, the candidate is acceptable again");
});

/** One clean reviewer, no findings: the phase would accept as soon as its
 * checks and probe pass, unless a queued correction obliges a repair. */
function cleanReviewer(reviewer: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } {
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
          phaseId: "p1",
          candidateSha: state.phase.candidate?.sha,
          contractVersion: state.phase.contract.contractVersion,
          correctionStatements: [],
          findingStatements: [],
          findings: [],
        },
      },
    ],
  };
}

test("plan 06k1: a correction queued during REVIEWING obliges a repair before DONE", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k1-review-");
  const promptLog = path.join(dir, "prompts.log");
  const correction = "CORRECTION-BEFORE-DONE-REVIEW: cap the retry loop at 64";
  const setup = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'attempt ${attempt}\n' > attempt-${attempt}.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => cleanReviewer(reviewer, state),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "REVIEWING", 60_000, 20, setup.runDir);
    writeCommand(setup.runDir, "cmd-review-correction", {
      type: "correction",
      text: correction,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    // The correction blocks acceptance, so a repair attempt runs and its
    // prompt carries the correction. Before the A-8 fix the phase accepted
    // here without the correction reaching any worker.
    await waitFor(() => (setup.conductor.state.phase.repairRoundsUsed ?? 0) >= 1, 90_000, 20, setup.runDir);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    assert.match(fs.readFileSync(promptLog, "utf8"), new RegExp(correction), "the repair prompt carried the correction");
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06k1: a correction queued during FINAL_CHECKING obliges a repair before DONE", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k1-final-");
  const promptLog = path.join(dir, "prompts.log");
  const correction = "CORRECTION-BEFORE-DONE-FINAL: name the marker final-ok.txt";
  const setup = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], finalChecks: ["sleep 2"] },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'attempt ${attempt}\n' > attempt-${attempt}.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => cleanReviewer(reviewer, state),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "FINAL_CHECKING", 90_000, 20, setup.runDir);
    writeCommand(setup.runDir, "cmd-final-correction", {
      type: "correction",
      text: correction,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    // The final check passes, but the queued correction obliges a repair.
    await waitFor(() => (setup.conductor.state.phase.repairRoundsUsed ?? 0) >= 1, 90_000, 20, setup.runDir);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    assert.match(fs.readFileSync(promptLog, "utf8"), new RegExp(correction), "the repair prompt carried the correction");
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06k1: a correction queued for a lane round stays queued when both lanes fail to start", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k1-lane-delivery-");
  const promptLog = path.join(dir, "prompts.log");
  const correction = "CORRECTION-LANE-DELIVERY: keep the marker file";
  const laneWorker = (lane: string): { hello: unknown; steps: FakePiStep[] } => ({
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane}\n' > lane-${lane}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  });
  const laneReview = (seat: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } => ({
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
          phaseId: "p1",
          candidateSha: "$TT_CANDIDATE_SHA",
          contractVersion: state.phase.contract.contractVersion,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  });
  const setup = await setupConductor({
    phase: { id: "p1", goal: "build two candidates from one base", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], workers: 2 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    deadlines: { ...FAST, helloTimeoutMs: 1_000, workerAttemptMs: 30_000 },
    workerScript: () => laneWorker("a"),
    // Round 1: BOTH lanes miss hello, so no lane worker ever receives the
    // prompt. Round 2: lane a starts and delivers it.
    laneWorkerScriptFor: (lane, round) =>
      round === 1
        ? { hello: defaultWorkerHello(), helloDelayMs: 30_000, steps: [] }
        : lane === "a"
          ? laneWorker("a")
          : { hello: defaultWorkerHello(), helloDelayMs: 30_000, steps: [] },
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state),
  });
  // The correction is queued BEFORE the run starts, so round 1's lane prompt
  // carries it. Both round-1 lanes then miss hello, so no worker ever
  // receives that prompt; the obligation must stay until round 2 actually
  // sends the prompt to a started lane worker.
  writeCommand(setup.runDir, "cmd-lane-correction", {
    type: "correction",
    text: correction,
    binding: { runId: path.basename(setup.runDir), phaseId: "p1" },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    // Round 1's prompt was never sent (both lanes missed hello), so the
    // correction reached a worker only in round 2; the prompt log proves it.
    assert.match(fs.readFileSync(promptLog, "utf8"), new RegExp(correction), "a started lane worker's prompt carried the correction");
    assert.ok(
      readEvents(setup.runDir).some((r) => r.kind === "event" && (r.event as { type?: string }).type === "NOTES_DELIVERED"),
      "delivery was recorded when a lane worker actually received the prompt",
    );
    assert.deepEqual(setup.conductor.state.phase.queuedCorrections ?? [], [], "the delivered correction is cleared");
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06k1: a correction queued with the budget spent grants its own round", () => {
  // REVIEWING and FINAL_CHECKING both queue a correction rather than applying
  // or refusing it.
  for (const name of ["REVIEWING", "FINAL_CHECKING", "EVALUATING"] as const) {
    const state = baseState({ phase: name });
    const outcome = acceptInput(state, { kind: "correction", text: "x", workerRunning: false });
    assert.equal(outcome.kind, "queued", `a correction during ${name} is queued`);
  }

  // With the budget spent (used == granted), the queued correction grants a
  // fresh round and queues its text for the next attempt.
  const spent = baseState({ phase: "FINAL_CHECKING", repairRoundsUsed: 3, repairRoundsGranted: 3 });
  const result = reduce(spent, {
    type: "OWNER_CORRECTION_QUEUED",
    phaseId: "p1",
    correctionId: "C-cmd-1",
    text: "keep the fix",
    grantedRounds: 3,
  });
  assert.ok(result.ok, "the queued correction applies");
  assert.equal(result.state.phase.repairRoundsGranted, 6, "the correction grants its round");
  assert.deepEqual(result.state.phase.ownerNotes, ["keep the fix"], "its text is queued for the next attempt");
});
