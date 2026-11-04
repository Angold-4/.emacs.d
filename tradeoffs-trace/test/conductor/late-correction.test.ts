// Plan 07f (A3/D-5/§3.3): an owner input that needs code work is a POINT in
// its domain's next fix loop — never a queued correction and never an
// acceptance obligation, in every state it arrives in (mid-state, before the
// first candidate, AWAITING_OWNER) and whatever the phase declares. A phase
// with no declared domain still admits the point: a ruling applies at once but
// performs no code work, and a withdraw only closes a record, so a correction
// is never refused for the shape of the phase (it is refused only when it omits
// the fields a point needs). A note stays a note: it is queued for the next
// attempt's prompt and marked delivered there.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { reduce } from "../../src/core/reduce.ts";
import { accept } from "../../src/core/predicate.ts";
import { acceptInput } from "../../src/core/owner-inbox.ts";
import { runPaths } from "../../src/conductor.ts";
import { basePhase, baseState, CV } from "../unit/helpers.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
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

test("plan 07f (A3): a correction during review or final checks is a point, never kept as a note", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-07f-late-correction-");
  const promptLog = path.join(dir, "worker-prompts.log");
  // Plan 07f (A3/§3.3): a structured correction is a point in the domain it
  // names, in every state — this phase declares no domain, which no longer
  // refuses it. Only an input that omits a point's own fields is refused.
  const correctionReview = [
    "CORRECTION-DURING-REVIEW: cap the retry loop at 64",
    "domain: conductor",
    "where: src/conductor.ts:4066",
    "expected: the retry loop stops at 64",
    `doneWhen: node --test test/conductor/late-correction.test.ts`,
  ].join("\n");
  const correctionFinal = [
    "CORRECTION-DURING-FINAL-CHECK: name the marker final-ok.txt",
    "domain: conductor",
    "where: src/conductor.ts:4066",
    "expected: the marker is named final-ok.txt",
    `doneWhen: node --test test/conductor/late-correction.test.ts`,
  ].join("\n");
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "do the thing",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      // The final check fails until a later attempt writes the marker, so the
      // phase reaches FINAL_CHECKING while the test still has a state to send
      // a correction to.
      finalChecks: ["sleep 2; test -f final-ok.txt"],
    },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-sh",
          command: attempt >= 2 ? "printf 'ok\\n' > final-ok.txt" : `printf 'attempt ${attempt}\\n' > attempt-${attempt}.txt`,
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => cleanReviewer(reviewer, state),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    const recorded = (id: string) => (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.id === id);

    await waitFor(() => setup.conductor.state.phase.phase === "REVIEWING", 60_000, 50, setup.runDir);
    writeCommand(setup.runDir, "cmd-late-review", {
      type: "correction",
      text: correctionReview,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    await waitFor(() => recorded("cmd-late-review")?.state === "noted", 30_000, 50, setup.runDir);
    const reviewPoint = (setup.conductor.state.phase.findings ?? []).find((f) => f.id.includes("owner-cmd-late-review"));
    assert.ok(reviewPoint, "the correction is a point in the record");
    assert.equal(reviewPoint!.status, "open", "and it stays open until its doneWhen fact passes");
    assert.equal(reviewPoint!.domain, "conductor", "the domain the input names owns it");

    // The point blocks acceptance until a fact proves it (that is the goal), so
    // the owner withdraws it here to let the run reach its final checks; the
    // withdraw closes the record and reaches every live session.
    writeCommand(setup.runDir, "cmd-withdraw-review-point", {
      type: "finding-verdict",
      findingId: reviewPoint!.id,
      verdict: "refuse",
      reason: "not the retry cap this phase needed",
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    await waitFor(
      () => setup.conductor.state.phase.findings.find((f) => f.id === reviewPoint!.id)?.status !== "open",
      30_000,
      50,
      setup.runDir,
    );

    await waitFor(() => setup.conductor.state.phase.phase === "FINAL_CHECKING", 90_000, 50, setup.runDir);
    writeCommand(setup.runDir, "cmd-late-final", {
      type: "correction",
      text: correctionFinal,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    await waitFor(() => recorded("cmd-late-final")?.state === "noted", 30_000, 50, setup.runDir);
    assert.ok(
      (setup.conductor.state.phase.findings ?? []).some((f) => f.id.includes("owner-cmd-late-final") && f.status === "open"),
      "a correction in FINAL_CHECKING is a point too",
    );

    // Neither correction is kept as a note and neither reaches a prompt as one:
    // a correction is the point in its domain, and only a note is queued text.
    const phase = setup.conductor.state.phase;
    assert.ok(!(phase.ownerNotes ?? []).some((n) => n.includes("CORRECTION-DURING")), "no correction is kept as a note");
    assert.equal((phase.queuedCorrections ?? []).length, 0, "and none is a queued correction");
    assert.ok(
      !fs.existsSync(promptLog) || !fs.readFileSync(promptLog, "utf8").split("\n")
        .some((line) => line.startsWith("Owner notes:") && line.includes("CORRECTION-DURING")),
      "no worker prompt carries a correction as queued note text",
    );
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06k1: a queued correction is not an acceptance obligation and delivery still clears it", () => {
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
  // Design draft 7 §3.3 (A3): acceptance never waits for a correction to reach
  // a worker. A stale record never written by this conductor still cannot gate
  // the candidate.
  assert.equal(accept(queued, "C1", K), true, "a queued correction is not an acceptance obligation");
  const delivered = reduce(
    baseState(queued),
    { type: "NOTES_DELIVERED", phaseId: "p1", count: 1 },
  );
  assert.ok(delivered.ok, "the delivery applies");
  assert.deepEqual(delivered.state.phase.queuedCorrections, [], "delivery clears the queue");
  assert.equal(accept(delivered.state.phase, "C1", K), true, "and the candidate stays acceptable");
});

test("plan 06k1: a correction queued with the budget spent grants its own round", () => {
  // REVIEWING and FINAL_CHECKING still queue a correction RECORD (a legacy
  // `OWNER_CORRECTION_QUEUED` in an old log); the conductor now records a point
  // instead, which gates nothing at acceptance but stays open until its fact
  // proves it.
  for (const name of ["REVIEWING", "FINAL_CHECKING", "EVALUATING"] as const) {
    const state = baseState({ phase: name });
    const outcome = acceptInput(state, { kind: "correction", text: "x", workerRunning: false });
    assert.equal(outcome.kind, "queued", `a correction during ${name} is queued by the input map`);
  }

  // With the budget spent (used == granted), the recorded queued correction
  // grants a fresh round and queues its text for the next attempt.
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

test("plan 06k2: a note queued after the lane prompt is built never enters that prompt and stays recorded", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k2-lane-late-");
  const promptLog = path.join(dir, "prompts.log");
  const before = "NOTE-BEFORE-LANE-PROMPT: keep the marker file";
  const late = "NOTE-AFTER-LANE-PROMPT: cap the retry loop at 64";
  const laneWorker = (lane: string): { hello: unknown; steps: FakePiStep[] } => ({
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `mkdir -p src && printf 'lane ${lane}\\n' > src/lane-${lane}.txt` },
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
    phase: { id: "p1", goal: "build two candidates from one base", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], workers: 2, rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    deadlines: { ...FAST, helloTimeoutMs: 10_000 },
    workerScript: () => laneWorker("a"),
    // Both lanes take a while to answer hello, so the test can queue the
    // second note AFTER the round's prompt was built.
    laneWorkerScriptFor: (lane) => ({ ...laneWorker(lane), helloDelayMs: 3_000 } as { hello: unknown; steps: FakePiStep[]; helloDelayMs: number }),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state),
    pickScriptFor: (seat, state) => ({
      hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_pick_vote",
          args: { inspected: ["a", "b", "c", "d"], round: state.phase.rounds?.length ?? 1, seat, lane: seat === "B" ? "a" : "b", why: `${seat} picks`, loserHad: { yes: false, anchors: [] } },
        },
      ],
    }),
  });
  // The first note is queued BEFORE the run: round 1's lane prompt carries it.
  // The second is queued during the hello wait, after that prompt was built,
  // so it must stay queued for the next attempt.
  writeCommand(setup.runDir, "cmd-lane-before", {
    type: "note",
    text: before,
    binding: { runId: path.basename(setup.runDir), phaseId: "p1" },
  });
  await setup.conductor.start();
  // Wait until the round's lane agents are dispatched: that proves the shared
  // lane prompt was already built, so the late note is not in it.
  await waitFor(
    () => readEvents(setup.runDir).some((r) => r.kind === "intent" && typeof r.actionId === "string" && r.actionId.startsWith("lane-agent-")),
    60_000,
    20,
    setup.runDir,
  );
  writeCommand(setup.runDir, "cmd-lane-late", {
    type: "note",
    text: late,
    binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
  });
  try {
    await waitFor(() => (setup.conductor.state.phase.ownerNotes ?? []).some((n) => n.includes(late)), 60_000, 20, setup.runDir);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    // The first NOTES_DELIVERED delivered only the pre-prompt note; the late
    // one was never injected into the prompt that was already built. It stays
    // queued (design draft 7 §3.3: acceptance never waits for it), so a later
    // session is the only place it may appear.
    const delivered = readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "NOTES_DELIVERED");
    assert.ok(delivered.length >= 1, "the round's prompt delivered its note");
    assert.equal((delivered[0].event as { count?: number }).count, 1, "the round-1 lane prompt delivered only the note it carried");
    assert.doesNotMatch(fs.readFileSync(promptLog, "utf8"), new RegExp(late), "the late note never entered the already-built prompt");
    const phase = setup.conductor.state.phase;
    assert.ok(
      (phase.ownerNotes ?? []).some((n) => n.includes(late)),
      "the late note is still recorded for a later session",
    );
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});
