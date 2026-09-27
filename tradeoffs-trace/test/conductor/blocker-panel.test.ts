// Plan 04b: the blocker panel. A reviewer's separate `blockers` list stops
// the work until the owner decides. From the moment it is raised it is two
// things — a raw `blocker` message and an open blocking finding, effective at
// once — and it is voted by three fresh panel agents dispatched in parallel,
// each with its own deadline. Their verdict is one of:
//
//   - escalate  (2 of 3 `block`):  AWAITING_OWNER, with the panel's options
//                for the owner; no repair round is spent, and only the owner's
//                choice resolves the blocker and its finding;
//   - downgrade (2 of 3 `downgrade`): one repair round, the blocking finding
//                in the next worker prompt, never parked;
//   - incomplete (no two seats agreed, e.g. two seats unavailable after their
//                retry): the blocking finding stays effective, and the phase
//                is not parked.
//
// EVALUATING leaves only through EVALUATION_COMPLETED, which waits for BOTH
// the evaluators and the panels, so the phase stays in EVALUATING until the
// last of the two settles and EVALUATION_COMPLETED is emitted exactly once.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { accept } from "../../src/core/predicate.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { runPaths } from "../../src/conductor.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";

// Small graces: a fake-pi agent replies to `abort` but never exits on its own,
// so every terminate pays the whole abort grace. These tests spend their time
// in that grace and in the pipeline itself, so keep both short (the suite's
// total wall time is what the phase's own check command is budgeted against).
const FAST = {
  abortGraceMs: 60,
  termGraceMs: 60,
  helloTimeoutMs: 5_000,
  checkMs: 20_000,
  freezeMs: 10_000,
  workerAttemptMs: 20_000,
  reviewMs: 15_000,
  probeMs: 4_000,
  evaluateMs: 15_000,
  panelMs: 15_000,
};

const BLOCKER_EVIDENCE = "src/cancel.ts:10 the cancel path can deadlock";
const BLOCK_OPTIONS = [
  { id: "repair_cancel", label: "repair the cancel path (grant 3 rounds)" },
  { id: "accept_risk", label: "accept the risk and document it" },
];

function submitPhaseStep() {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

/** A real two-turn reviewer; exactly one of them (B) raises a `blocker` on
 * the FIRST candidate only, so a later repair round does not raise it again. */
function blockerReviewer() {
  return (reviewer: Reviewer, state: State) => {
    const alreadyRaised = (state.phase.messages ?? []).some((m) => m.type === "blocker");
    const raise = reviewer === "B" && !alreadyRaised;
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
            ballots: [],
            findings: [],
            ...(raise ? { blockers: [{ kind: "defect", evidence: BLOCKER_EVIDENCE }] } : {}),
          },
        },
      ],
    };
  };
}

/** The blocker evaluator publishes the blocker message; no other type has
 * anything to do. */
function blockerEvaluator() {
  return (messageType: string) => ({
    hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
    steps:
      messageType === "blocker"
        ? [
            {
              kind: "call-submit",
              tool: "submit_evaluation",
              args: {
                evaluations: [
                  {
                    messageId: "B-1",
                    action: "publish",
                    title: "defect blocking: the cancel path can deadlock",
                    summary: "The reviewer says the cancel path can deadlock.",
                    context: BLOCKER_EVIDENCE,
                    evidence: [BLOCKER_EVIDENCE],
                  },
                ],
              },
            },
          ]
        : [],
  });
}

function events(setup: TestConductorSetup) {
  return readEvents(setup.runDir).filter((r) => r.kind === "event").map((r) => r.event as { type: string } & Record<string, unknown>);
}

function countType(setup: TestConductorSetup, type: string): number {
  return events(setup).filter((e) => e.type === type).length;
}

function panelVotes(setup: TestConductorSetup): Array<{ blockerId: string; seat: number; vote: string }> {
  return events(setup)
    .filter((e) => e.type === "PANEL_VOTE")
    .map((e) => e as { blockerId: string; seat: number; vote: string });
}

function writeResolve(setup: TestConductorSetup, id: string, requestId: string, option: string, recordVersion: number): void {
  const phase = setup.conductor.state.phase;
  fs.writeFileSync(
    path.join(runPaths(setup.runDir).inbox, `${id}.json`),
    JSON.stringify({
      commandId: id,
      type: "resolve",
      recordKind: "request",
      option,
      binding: {
        runId: phase.runId,
        phaseId: phase.phaseId,
        candidateSha: phase.candidate!.sha,
        contractVersion: phase.contract.contractVersion,
        recordId: requestId,
        recordVersion,
      },
    }),
  );
}

test("plan 04b: 2 of 3 block parks the phase with the panel's options and no repair round; the owner's choice resolves the blocker and its finding and the work continues", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-panel-resolve-");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: path.join(promptDir, "worker.log") },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: blockerReviewer(),
    evaluatorScriptFor: blockerEvaluator(),
    panelScriptFor: (blockerId, seat) => ({
      hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_panel_vote",
          args:
            seat <= 2
              ? { blockerId, seat, vote: "block", reason: `seat ${seat}: stop it`, options: BLOCK_OPTIONS }
              : { blockerId, seat, vote: "downgrade", reason: "seat 3: repairable" },
        },
      ],
    }),
    deadlines: { ...FAST, inboxPollMs: 100 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.repairRoundsUsed, 0, "escalating a blocker must spend no repair round");
    const request = phase.ownerRequests.find((r) => r.status === "open");
    assert.ok(request, "the escalation must leave the owner a request");
    assert.equal(request!.origin, "blocker_panel");
    assert.deepEqual(
      request!.options.map((o) => o.id),
      BLOCK_OPTIONS.map((o) => o.id),
      "the owner must be offered the panel's own options",
    );
    const blocker = (phase.messages ?? []).find((m) => m.type === "blocker")!;
    assert.equal(blocker.state, "published");
    const finding = phase.findings.find((f) => f.raisedBy === "B" && f.severity === "blocking")!;
    assert.ok(finding, "the blocker was raised as a blocking finding too");
    // A `block` majority is what carried, and the recorded votes are real.
    assert.equal(panelVotes(setup).filter((v) => v.vote === "block").length, 2);
    assert.equal(
      (events(setup).find((e) => e.type === "PANEL_DECIDED") as { outcome?: string } | undefined)?.outcome,
      "escalate",
    );

    // The owner picks one of the panel's options through the inbox.
    writeResolve(setup, "cmd-blocker-resolve", request!.id, "repair_cancel", request!.version);

    await waitFor(
      () => setup.conductor.state.phase.ownerRequests.every((r) => r.status !== "open") && setup.conductor.state.phase.phase !== "AWAITING_OWNER",
      60_000,
      20,
      setup.runDir,
    );
    const after = setup.conductor.state.phase;
    assert.equal(
      after.findings.find((f) => f.id === finding.id)?.status,
      "accepted",
      "the owner's choice resolves the blocker's finding",
    );
    assert.equal(
      (after.messages ?? []).find((m) => m.id === blocker.id)?.state,
      "resolved",
      "the owner's choice resolves the blocker message",
    );
    // The phase continues into the repair that carries the choice out.
    await waitFor(() => countType(setup, "REPAIR_ATTEMPT_STARTED") >= 1, 60_000, 20, setup.runDir);
    assert.notEqual(setup.conductor.state.phase.phase, "AWAITING_OWNER", "the phase resumes after the owner's choice");
    // The choice itself reaches the next worker attempt's prompt.
    await waitFor(
      () => fs.existsSync(path.join(promptDir, "worker.log")) && fs.readFileSync(path.join(promptDir, "worker.log"), "utf8").includes("repair the cancel path"),
      60_000,
      20,
      setup.runDir,
    );
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(promptDir, { recursive: true, force: true });
  }
});

test("plan 04b: 2 of 3 downgrade spends one repair round and puts the published blocking finding in the next worker prompt, never parked", async () => {
  const promptLog = fs.mkdtempSync("/tmp/tt-panel-prompt-");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: path.join(promptLog, "worker.log") },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: blockerReviewer(),
    evaluatorScriptFor: blockerEvaluator(),
    panelScriptFor: (blockerId, seat) => ({
      hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_panel_vote",
          args:
            seat <= 2
              ? { blockerId, seat, vote: "downgrade", reason: `seat ${seat}: it is repairable` }
              : { blockerId, seat, vote: "block", reason: "seat 3: stop it", options: BLOCK_OPTIONS },
        },
      ],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.repairRoundsUsed >= 1, 90_000, 20, setup.runDir);
    assert.notEqual(setup.conductor.state.phase.phase, "AWAITING_OWNER", "a downgrade must not park the phase");
    assert.equal(
      (events(setup).find((e) => e.type === "PANEL_DECIDED") as { outcome?: string } | undefined)?.outcome,
      "downgrade",
    );
    const finding = setup.conductor.state.phase.findings.find((f) => f.severity === "blocking" && f.status === "open");
    assert.ok(finding, "the downgraded blocker is still an open blocking finding");
    const blocker = (setup.conductor.state.phase.messages ?? []).find((m) => m.type === "blocker")!;
    assert.equal(blocker.state, "published", "the blocker message is published, not left raw");
    // The next worker prompt carries the blocking finding (the repair request).
    await waitFor(
      () => fs.existsSync(path.join(promptLog, "worker.log")) && fs.readFileSync(path.join(promptLog, "worker.log"), "utf8").includes(finding!.id),
      90_000,
      20,
      setup.runDir,
    );
    const prompt = fs.readFileSync(path.join(promptLog, "worker.log"), "utf8");
    assert.match(prompt, /Blocking \(must be fixed\):/);
    assert.match(prompt, /src\/cancel\.ts:10 the cancel path can deadlock/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(promptLog, { recursive: true, force: true });
  }
});

test("plan 04b: a seat that times out is retried once and its real vote decides; two seats unavailable after retry leave the panel incomplete and the blocking finding effective, not parked", async () => {
  // (a) One seat times out, its retry votes: the panel is decided on real
  // votes (2 block + 1 downgrade -> escalate).
  {
    const calls = new Map<number, number>();
    const setup = await setupConductor({
      checks: ["true"],
      stubReviews: false,
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
      reviewerScriptFor: blockerReviewer(),
      evaluatorScriptFor: blockerEvaluator(),
      panelScriptFor: (blockerId, seat) => {
        const n = (calls.get(seat) ?? 0) + 1;
        calls.set(seat, n);
        // Seat 1's first dispatch never votes (the deadline kills it); its
        // retry votes block.
        if (seat === 1 && n === 1) {
          return { hello: { role: "panel" as const, tools: ROLE_TOOLS.panel }, steps: [{ kind: "sleep", ms: 60_000 }] };
        }
        return {
          hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
          steps: [
            {
              kind: "call-submit",
              tool: "submit_panel_vote",
              args:
                seat === 1 || seat === 2
                  ? { blockerId, seat, vote: "block", reason: `seat ${seat}: stop it`, options: BLOCK_OPTIONS }
                  : { blockerId, seat, vote: "downgrade", reason: "seat 3: repairable" },
            },
          ],
        };
      },
      deadlines: { ...FAST, panelMs: 700 },
    });
    await setup.conductor.start();
    try {
      await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 20, setup.runDir);
      assert.equal(countType(setup, "PANEL_SEAT_UNAVAILABLE"), 1, "one seat was lost once");
      const votes = panelVotes(setup);
      assert.equal(votes.length, 3, "all three seats produced a real vote after the retry");
      assert.equal(votes.filter((v) => v.seat === 1).length, 1, "seat 1's retry vote is the recorded one");
      assert.equal(
        (events(setup).find((e) => e.type === "PANEL_DECIDED") as { outcome?: string } | undefined)?.outcome,
        "escalate",
      );
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
  }

  // (b) Two seats never vote, even on retry: the panel is incomplete, the
  // blocking finding stays effective, and the phase is not parked.
  {
    const setup = await setupConductor({
      checks: ["true"],
      stubReviews: false,
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
      reviewerScriptFor: blockerReviewer(),
      evaluatorScriptFor: blockerEvaluator(),
      panelScriptFor: (blockerId, seat) =>
        seat <= 2
          ? { hello: { role: "panel" as const, tools: ROLE_TOOLS.panel }, steps: [{ kind: "sleep", ms: 60_000 }] }
          : {
              hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
              steps: [
                {
                  kind: "call-submit",
                  tool: "submit_panel_vote",
                  args: { blockerId, seat, vote: "block", reason: "seat 3: one seat wants to stop", options: BLOCK_OPTIONS },
                },
              ],
            },
      deadlines: { ...FAST, panelMs: 700 },
    });
    await setup.conductor.start();
    try {
      // The panel's verdict is in as soon as it leaves EVALUATING; no need to
      // wait for the whole repair round on top.
      await waitFor(
        () => countType(setup, "PANEL_DECIDED") === 1 && setup.conductor.state.phase.phase !== "EVALUATING",
        120_000,
        20,
        setup.runDir,
      );
      assert.equal(countType(setup, "PANEL_SEAT_UNAVAILABLE"), 4, "two seats lost twice each");
      assert.equal(
        (events(setup).find((e) => e.type === "PANEL_DECIDED") as { outcome?: string } | undefined)?.outcome,
        "incomplete",
      );
      assert.notEqual(setup.conductor.state.phase.phase, "AWAITING_OWNER", "an incomplete panel must not park the phase");
      // The blocker's blocking finding is still effective: the candidate
      // cannot be accepted while it is open.
      const phase = setup.conductor.state.phase;
      const finding = phase.findings.find((f) => f.severity === "blocking" && f.status === "open");
      assert.ok(finding, "the blocking finding is still open and effective");
      if (phase.candidate) {
        assert.equal(accept(phase, phase.candidate.sha, phase.contract.contractVersion), false);
      }
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
  }
});

test("plan 04b: both completion orders — evaluator then panel, and panel then evaluator — leave the phase in EVALUATING until the last settles, with EVALUATION_COMPLETED exactly once", async () => {
  async function runOrder(order: "evaluator-first" | "panel-first", evaluatorDelayMs: number, seatDelayMs: number) {
    const setup = await setupConductor({
      checks: ["true"],
      stubReviews: false,
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
      reviewerScriptFor: blockerReviewer(),
      evaluatorScriptFor: () => ({
        hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
        steps: [
          { kind: "sleep", ms: evaluatorDelayMs },
          {
            kind: "call-submit",
            tool: "submit_evaluation",
            args: {
              evaluations: [
                {
                  messageId: "B-1",
                  action: "publish",
                  title: "defect blocking: the cancel path can deadlock",
                  summary: "The reviewer says the cancel path can deadlock.",
                  context: BLOCKER_EVIDENCE,
                  evidence: [BLOCKER_EVIDENCE],
                },
              ],
            },
          },
        ],
      }),
      panelScriptFor: (blockerId, seat) => ({
        hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
        steps: [
          { kind: "sleep", ms: seatDelayMs },
          {
            kind: "call-submit",
            tool: "submit_panel_vote",
            args:
              seat <= 2
                ? { blockerId, seat, vote: "block", reason: `seat ${seat}: stop it`, options: BLOCK_OPTIONS }
                : { blockerId, seat, vote: "downgrade", reason: "seat 3: repairable" },
          },
        ],
      }),
      deadlines: FAST,
    });
    await setup.conductor.start();
    try {
      const firstSettled = order === "evaluator-first" ? "EVALUATOR_FINISHED" : "PANEL_DECIDED";
      const lastSettled = order === "evaluator-first" ? "PANEL_DECIDED" : "EVALUATOR_FINISHED";
      await waitFor(() => countType(setup, firstSettled) >= 1, 90_000, 20, setup.runDir);
      // The phase must still be EVALUATING after the FIRST of the two, and
      // must not have completed.
      assert.equal(setup.conductor.state.phase.phase, "EVALUATING", `after ${firstSettled} the phase must still be EVALUATING`);
      assert.equal(countType(setup, "EVALUATION_COMPLETED"), 0, `EVALUATION_COMPLETED must not fire before ${lastSettled}`);
      await waitFor(() => countType(setup, lastSettled) >= 1, 90_000, 20, setup.runDir);
      await waitFor(() => countType(setup, "EVALUATION_COMPLETED") === 1, 60_000, 20, setup.runDir);
      assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER");
      assert.equal(countType(setup, "EVALUATION_COMPLETED"), 1, "EVALUATION_COMPLETED is emitted exactly once");
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
  }

  // Evaluator first: the panel seats are slow, so the evaluator settles while
  // the panel is still voting.
  await runOrder("evaluator-first", 0, 2500);
  // Panel first: the seats vote at once, the evaluator is slow.
  await runOrder("panel-first", 2500, 0);
});
