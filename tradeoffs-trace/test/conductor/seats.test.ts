// Plan 06h: the seat list and the K-lane round, end to end with fake-pi.
//
// R1: four lanes with five seats build four candidates, run 20 reviews and
//     five pick votes, and pick the strict-majority candidate.
// R2: three lanes with five seats splitting 2-2-1 go to one revote between
//     the top two; the leader's vote breaks the revote tie.
// R3: five seats with one worker need three approvals, and a blocker runs a
//     five-seat panel.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import { test } from "node:test";

import { lintPlan, type LintPlanInput } from "../../src/core/plan-lint.ts";
import { seatsOf } from "../../src/core/seats.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

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

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 20_000,
  checkMs: 15_000,
  probeMs: 15_000,
  freezeMs: 15_000,
  evaluateMs: 8_000,
  reviewMs: 10_000,
  panelMs: 8_000,
};

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

function lanePhase(overrides: Partial<import("../../src/conductor.ts").RunPlanPhase> = {}) {
  return {
    id: "p1",
    goal: "build candidates from one base",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    ...overrides,
  };
}

/** A lane worker: writes one lane-specific file and submits. */
function laneWorker(lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane}\n' > lane-${lane}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** One seat's two-turn review of one lane candidate. */
function laneReview(seat: string, contractVersion: unknown): { hello: unknown; steps: FakePiStep[] } {
  return {
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
          contractVersion,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  };
}

/** One seat's pick vote for a lane, in the round the conductor is running. */
function pickVote(seat: string, round: number, lane: string, why: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { round, seat, lane, why, loserHad: { yes: false, anchors: [] } } }],
  };
}

function liveRound(state: State): number {
  return state.phase.rounds?.length ?? 1;
}

const ROUND_EVENT = (r: { kind: string; event: unknown }): string | undefined =>
  r.kind === "event" ? (r.event as { type?: string }).type : undefined;

test("plan 06h: TT_WORKERS 4 with five seats builds four candidates, runs 20 reviews and five pick votes, and picks the strict-majority candidate", async () => {
  const seats = ["M", "A", "B", "C", "D"];
  const setup = await setupConductor({
    phase: lanePhase({ workers: 4, seats, leader: "M" }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat, state.phase.contract.contractVersion),
    // M, A and B pick lane a; C and D pick lane b: a strict majority (3 of 5)
    // for lane a, and no revote.
    pickScriptFor: (seat, state) => pickVote(seat, liveRound(state), seat === "C" || seat === "D" ? "b" : "a", `${seat} picks`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 1);
    const round = phase.rounds![0];
    assert.deepEqual(round.lanes, ["a", "b", "c", "d"], "four lanes");
    assert.equal(round.candidates.length, 4);
    assert.ok(round.candidates.every((c) => c.ok === true && typeof c.sha === "string"), "all four candidates passed");
    // 20 reviews: five seats on each of four candidates.
    for (const c of round.candidates) assert.deepEqual((c.reviews ?? []).map((r) => r.seat).sort(), [...seats].sort());
    assert.equal(round.votes.length, 5, "five pick votes");
    assert.deepEqual(round.picked, { lane: "a", sha: round.candidates.find((c) => c.lane === "a")!.sha, votes: 3 });
    assert.equal(round.revote, undefined, "a strict majority needs no revote");
    const events = readEvents(setup.runDir).filter((r) => r.kind === "event");
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "ROUND_REVIEW_SUBMITTED").length, 20, "20 reviews");
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "PICK_VOTE").length, 5, "five pick votes");
    assert.equal(phase.candidate?.sha, round.picked!.sha);
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
  }
});

test("plan 06h: three lanes with five seats splitting 2-2-1 go to one revote between the top two", async () => {
  const seats = ["M", "A", "B", "C", "D"];
  const setup = await setupConductor({
    phase: lanePhase({ workers: 3, seats, leader: "M" }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => {
      const round = state.phase.rounds?.[state.phase.rounds.length - 1];
      const rn = round?.round ?? 1;
      // First turn: 2-2-1 (M,A -> a; B,C -> b; D -> c).
      if (!round?.revote) {
        const lane = seat === "M" || seat === "A" ? "a" : seat === "D" ? "c" : "b";
        return pickVote(seat, rn, lane, `${seat} picks ${lane}`);
      }
      // Revote: the four non-leader seats split 2-2 (A,C -> a; B,D -> b); the
      // leader M votes b and breaks the tie.
      const lane = seat === "M" || seat === "B" || seat === "D" ? "b" : "a";
      return pickVote(seat, rn, lane, `${seat} revotes ${lane}`);
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 1);
    const round = phase.rounds![0];
    assert.deepEqual(round.lanes, ["a", "b", "c"], "three lanes");
    assert.deepEqual(round.revote?.lanes, ["a", "b"], "the top two went to one revote");
    assert.equal(round.revote?.votes.length, 5, "all five seats voted in the revote");
    assert.equal(round.votes.length, 5, "five first-turn votes");
    assert.equal(round.picked?.lane, "b", "the leader's revote vote broke the tie for lane b");
    assert.equal(round.picked?.votes, 2, "two non-leader seats backed lane b");
    const events = readEvents(setup.runDir).filter((r) => r.kind === "event");
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "REVOTE_STARTED").length, 1, "exactly one revote");
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "CANDIDATE_PICKED").length, 1);
    assert.equal(phase.candidate?.sha, round.picked!.sha);
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
  }
});

const BLOCKER_EVIDENCE = "src/cancel.ts:10 the cancel path can deadlock";

/** A real two-turn reviewer for five seats: M, A and B approve the one
 * delegated decision, C and D reject it (three approvals pass); B raises one
 * blocker so a five-seat panel runs. */
function fiveSeatReviewer() {
  return (reviewer: Reviewer, state: State) => {
    const C = state.phase.candidate?.sha;
    const decision = state.phase.decisions.find((d) => d.source === "worker" && d.boundCandidateSha === C);
    const approve = reviewer === "M" || reviewer === "A" || reviewer === "B";
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
            phaseId: "p1",
            candidateSha: state.phase.candidate?.sha,
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
            ballots: decision
              ? [{ decisionId: decision.id, vote: approve ? "approve" : "reject", rationale: `${reviewer} votes`, evidence: ["src/cancel.ts:1"] }]
              : [],
            findings: [],
            ...(raise ? { blockers: [{ kind: "defect", evidence: BLOCKER_EVIDENCE }] } : {}),
          },
        },
      ],
    };
  };
}

test("plan 06h: a phase-only seat override is recorded in the init event and replays", async () => {
  const seats = ["M", "A", "B", "C", "D"];
  // The seats live only on the phase, not the plan.
  const setup = await setupConductor({
    phase: lanePhase({ workers: 2, seats, leader: "M" }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const { readLog } = await import("../../src/effects/log.ts");
    const { rebuildState, runPaths } = await import("../../src/conductor.ts");
    const init = readLog(runPaths(setup.runDir).events).records.find((r) => r.kind === "init");
    const recorded = (init!.event as { seats?: { seats: string[] } }).seats;
    assert.deepEqual(recorded?.seats, seats, "the init event records the phase's effective five seats");
    const rebuilt = rebuildState(setup.runDir, setup.plan);
    assert.deepEqual(seatsOf(rebuilt.phase.contract), seats, "a replay keeps the phase's own seats");
  } finally {
    await teardown(setup);
  }
});

test("plan 06h: TT_ROUNDS 2 stops a still-blocked phase after two rounds, and tt lint refuses TT_ROUNDS 0 and 6", async () => {
  // `tt lint` refuses a round budget outside 1..5, at the keyword's line.
  const base: LintPlanInput = { phases: [{ id: "p", acceptance: ["it works"] }] };
  for (const n of [0, 6]) {
    const finding = lintPlan({ ...base, rounds: n, roundsLine: 4 }).find((f) => f.rule === "worker-count");
    assert.ok(finding, `#+TT_ROUNDS: ${n} must be refused`);
    assert.equal(finding!.line, 4);
    assert.match(finding!.problem, /from 1 to 5/);
  }
  assert.deepEqual(lintPlan({ ...base, rounds: 2, roundsLine: 4 }).filter((f) => f.rule === "worker-count"), []);

  // A still-blocked phase with a two-round budget stops after exactly two
  // rounds. Every candidate fails its checks, so no round ever picks a winner.
  const setup = await setupConductor({
    phase: lanePhase({ workers: 2, rounds: 2 }),
    checks: ["false"],
    phaseChecks: ["false"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => ["AWAITING_OWNER", "BLOCKED"].includes(setup.conductor.state.phase.phase), 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "exactly two rounds");
    assert.ok(phase.rounds!.every((r) => r.picked === undefined), "no round picked a winner");
    assert.equal(phase.phase, "AWAITING_OWNER", "the budget-spent phase parks on the owner");
  } finally {
    await teardown(setup);
  }
});

test("plan 06h: TT_REVIEWERS M A B C D with one worker needs three approvals and runs a five-seat blocker panel", async () => {
  const seats = ["M", "A", "B", "C", "D"];
  const setup = await setupConductor({
    phase: lanePhase({ seats, leader: "M" }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > cancel.ts" },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [
              {
                choice: "batch cancels per tick",
                whyItMatters: "fewer lock acquisitions under load",
                alternatives: [{ option: "one lock per request", consequence: "more contention" }],
                recommendation: { choice: "batch per tick", reason: "stays inside the latency budget" },
                classProposal: "delegated",
              },
            ],
            assumptions: [],
            deviations: [],
          },
        },
      ],
    }),
    reviewerScriptFor: fiveSeatReviewer(),
    defaultPanelVote: "downgrade",
  });
  try {
    await setup.conductor.start();
    // The blocker's downgrade is an ordinary blocking finding that forces one
    // repair (the existing 06g rule), so wait for the panel to decide and the
    // repair to start — never for DONE.
    await waitFor(
      () =>
        Object.values(setup.conductor.state.phase.panel?.blockers ?? {}).some((p) => p.decided !== undefined) &&
        setup.conductor.state.phase.repairRoundsUsed >= 1,
      120_000,
      50,
      setup.runDir,
    );
    const phase = setup.conductor.state.phase;
    // The one delegated decision drew five ballots, three approving: a strict
    // majority of five is three, so it passed.
    const decision = phase.decisions.find((d) => d.source === "worker");
    assert.ok(decision, "the worker's decision was assembled");
    const ballots = phase.ballots.filter((b) => b.decisionId === decision!.id);
    assert.equal(ballots.length, 5, "all five seats balloted it");
    assert.equal(ballots.filter((b) => b.vote === "approve").length, 3, "three approvals");
    const { decisionStatus } = await import("../../src/core/predicate.ts");
    assert.equal(decisionStatus(decision!, phase).status, "passed", "three of five approvals passed it");
    // A five-seat blocker panel ran and downgraded the blocker.
    const blockerId = Object.keys(phase.panel?.blockers ?? {})[0];
    assert.ok(blockerId, "a blocker panel was opened");
    assert.deepEqual(Object.keys(phase.panel!.blockers![blockerId].seats ?? {}).sort(), ["1", "2", "3", "4", "5"], "five panel seats");
    assert.equal(phase.panel!.blockers![blockerId].decided?.outcome, "downgrade");
    assert.notEqual(phase.phase, "AWAITING_OWNER", "a downgrade never parks the phase on the owner");
  } finally {
    await teardown(setup);
  }
});
