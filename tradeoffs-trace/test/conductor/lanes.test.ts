// Plan 06g2: the two-lane round, end to end. These tests drive the real
// conductor with fake-pi: two lanes from one base, their candidates checked
// one after the other under the machine-wide lock, M/A/B reviewing every
// passing candidate, the pick turn, and the winner handed to the
// single-candidate pipeline. A plan without `#+TT_WORKERS` is untouched
// (test/conductor/plan-items.test.ts's own C1 test).

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { rebuildState } from "../../src/conductor.ts";
import { readLog } from "../../src/effects/log.ts";
import { runPaths } from "../../src/conductor.ts";
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

// Kept short on purpose: `check-full` runs this file beside the whole
// conductor suite, so a lane test that waits out a 20 s review deadline is
// load other deadline-sensitive tests pay for.
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

/** A phase with `#+TT_WORKERS: 2` frozen into its contract. */
function lanePhase(overrides: Partial<import("../../src/conductor.ts").RunPlanPhase> = {}) {
  return {
    id: "p1",
    goal: "build two candidates from one base",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers: 2,
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

/** One seat's two-turn review of one lane candidate (design §3.3): turn 1
 * discovers, turn 2 reviews. `findings` raises blocking findings;
 * `findingStatements` withdraws/confirms earlier ones; `ballots` votes on the
 * lane's own decisions. */
function laneReview(
  seat: Reviewer,
  contractVersion: unknown,
  opts: { findings?: unknown[]; findingStatements?: unknown[]; ballots?: unknown[]; discoveries?: unknown[] } = {},
): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: opts.discoveries ?? [] } },
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
          findingStatements: opts.findingStatements ?? [],
          ballots: opts.ballots ?? [],
          findings: opts.findings ?? [],
        },
      },
    ],
  };
}

/** A lane worker that also discloses one delegated decision. */
function laneWorkerWithDecision(lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane}\n' > lane-${lane}.txt` },
      {
        kind: "call-submit",
        tool: "submit_phase",
        args: {
          decisions: [
            {
              classProposal: "delegated",
              choice: `lane ${lane} validates the input`,
              whyItMatters: "an unvalidated input is a silent wrong answer",
              alternatives: [{ option: "trust the caller", consequence: "a wrong answer with no error" }],
              recommendation: { choice: "validate", reason: "the acceptance item asks for defined behaviour" },
            },
          ],
          assumptions: [],
          deviations: [],
        },
      },
    ],
  };
}

/** One seat's pick vote for a lane, in the round the conductor is running. */
function pickVote(seat: Reviewer, round: number, lane: string, why: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { round, seat, lane, why, loserHad: { yes: false, anchors: [] } } }],
  };
}

/** The round the conductor is in, from live state (the round record exists
 * before the pick turn is dispatched). */
function liveRound(state: State): number {
  return state.phase.rounds?.length ?? 1;
}

/** Every `lane_check_started`/`lane_check_finished` pair in the log, as
 * [start, finish] wall-clock intervals — the evidence for C2. */
function checkIntervals(runDir: string): Array<{ lane: string; start: number; finish: number }> {
  const records = readLog(runPaths(runDir).events).records;
  const started = records.filter((r) => r.kind === "lane_check_started");
  const finished = records.filter((r) => r.kind === "lane_check_finished");
  const out: Array<{ lane: string; start: number; finish: number }> = [];
  for (const s of started) {
    const lane = (s.event as { lane: string }).lane;
    const sha = (s.event as { candidateSha: string }).candidateSha;
    const f = finished.find(
      (r) => (r.event as { lane: string }).lane === lane && (r.event as { candidateSha: string }).candidateSha === sha,
    );
    assert.ok(f, `check of lane ${lane} has no finish record`);
    out.push({ lane, start: Date.parse(s.ts), finish: Date.parse(f!.ts) });
  }
  return out;
}

const ROUND_EVENT = (r: { kind: string; event: unknown }): string | undefined =>
  r.kind === "event" ? (r.event as { type?: string }).type : undefined;

test("plan 06g: a two-lane plan parsed by the real Emacs parser builds two candidates from one base, checks them one after the other, runs six reviews and three pick votes, and b wins 2-1", async () => {
  // 1. The REAL Emacs parser, on an org plan with `#+TT_WORKERS: 2`.
  const dir = fs.mkdtempSync("/tmp/tt-06g2-");
  const repoDir = fs.mkdtempSync("/tmp/tt-06g2-repo-");
  execFileSync("git", ["init", "-q", "-b", "main"], { cwd: repoDir });
  fs.writeFileSync(path.join(repoDir, "README.md"), "base\n");
  execFileSync("git", ["add", "-A"], { cwd: repoDir });
  execFileSync("git", ["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], { cwd: repoDir });
  const orgPath = path.join(dir, "PLAN.org");
  fs.writeFileSync(
    orgPath,
    [
      "#+TITLE: two lanes",
      `#+TT_REPO: ${repoDir}`,
      "#+TT_BRANCH: main",
      "#+TT_WORKERS: 2",
      "",
      "* Phase 1: p1",
      "  :PROPERTIES:",
      "  :ID: p1",
      "  :CHECKS: true",
      "  :END:",
      "  Goal: build two candidates from one base",
      "  Acceptance:",
      "  - it works",
    ].join("\n") + "\n",
  );
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
  const parsed = JSON.parse(out) as { workers?: number; phases: Array<{ workers?: number }> };
  assert.equal(parsed.workers, 2, "the plan keyword reached the JSON");
  assert.equal(parsed.phases[0].workers, 2, "the phase inherited the plan's two lanes");
  const phase = parsed.phases[0] as import("../../src/conductor.ts").RunPlanPhase;

  // 2. Run it with fake-pi.
  const promptLog = path.join(dir, "lane-prompts.txt");
  const setup = await setupConductor({
    phase,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) =>
      pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", `${seat} prefers ${seat === "B" ? "a" : "b"}`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const phaseState = setup.conductor.state.phase;

    // One base, two candidates.
    assert.equal(phaseState.rounds?.length, 1);
    const round = phaseState.rounds![0];
    assert.equal(round.lanes.join(","), "a,b");
    assert.equal(round.candidates.length, 2);
    assert.ok(round.candidates.every((c) => typeof c.sha === "string" && c.sha.length > 0), "both lanes froze a candidate");
    assert.equal(round.base, setup.repo.head, "round 1 starts from the integration head");
    // Six reviews: M, A and B each reviewed both passing candidates.
    for (const c of round.candidates) {
      assert.equal(c.ok, true, "both candidates passed their checks");
      assert.deepEqual((c.reviews ?? []).map((r) => r.seat).sort(), ["A", "B", "M"]);
    }
    // Three pick votes and a 2-1 win for lane b.
    assert.equal(round.votes.length, 3);
    assert.deepEqual(round.picked, { lane: "b", sha: round.candidates.find((c) => c.lane === "b")!.sha, votes: 2 });

    // The record's own counts.
    const events = readEvents(setup.runDir).filter((r) => r.kind === "event");
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "ROUND_REVIEW_SUBMITTED").length, 6, "six reviews");
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "PICK_VOTE").length, 3, "three pick votes");

    // C2: the two candidates' checks never overlap in time.
    const intervals = checkIntervals(setup.runDir);
    assert.equal(intervals.length, 2, "one check record per lane");
    const [first, second] = intervals.sort((x, y) => x.start - y.start);
    assert.ok(first.finish <= second.start, `checks overlapped: ${JSON.stringify(intervals)}`);
    assert.notEqual(first.lane, second.lane);

    // The winner is the phase's candidate and the phase is DONE on its commit.
    assert.equal(phaseState.candidate?.sha, round.picked!.sha);
    assert.equal(phaseState.reviews.M?.review?.candidateSha, round.picked!.sha);
    assert.equal(phaseState.reviews.A?.review?.candidateSha, round.picked!.sha);
    assert.equal(phaseState.reviews.B?.review?.candidateSha, round.picked!.sha);
    assert.equal(phaseState.publishedI, phaseState.probe?.probedI);

    // The winner's own reviews are not re-taken: exactly the round's six.
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "REVIEW_SUBMITTED").length, 3, "only the winner's promotion");
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "lane_review_submitted").length, 6);

    // Both lanes' prompts are the same, and name the round's base.
    const prompts = fs.readFileSync(promptLog, "utf8").split("\n").filter((l) => l.includes("Round 1 of this phase"));
    assert.equal(prompts.length, 2, "both lanes were prompted");

    // A restart folds the same round back out of the log.
    const rebuilt = rebuildState(setup.runDir, setup.plan);
    assert.deepEqual(rebuilt.phase.rounds, phaseState.rounds);
    assert.equal(rebuilt.phase.candidate?.sha, phaseState.candidate?.sha);
  } finally {
    await teardown(setup);
    cleanupDir(dir);
    cleanupDir(repoDir);
  }
});

test("plan 06g: round 1's winner not accepted makes round 2 start both lanes from the winner's commit with its open findings", async () => {
  const promptLog = fs.mkdtempSync("/tmp/tt-06g2-r2-prompts-");
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: path.join(promptLog, "prompts.txt") },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const round = liveRound(state);
      const K = state.phase.contract.contractVersion;
      if (round === 1) {
        // M raises ONE blocking finding, naming the phase's acceptance item so
        // it is a legitimate blocker; round 1 is not accepted.
        return seat === "M"
          ? laneReview("M", K, {
              findings: [
                {
                  kind: "defect",
                  severity: "blocking",
                  evidence: "it works is unmet: src/lane-a.txt:1 the lane's output is wrong",
                },
              ],
            })
          : laneReview(seat as Reviewer, K);
      }
      // Round 2: M withdraws its round-1 finding (the new candidate fixed it).
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === "M");
      return seat === "M"
        ? laneReview("M", K, {
            findingStatements: open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })),
          })
        : laneReview(seat as Reviewer, K);
    },
    pickScriptFor: (seat, state) =>
      pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", `${seat} picks ${seat === "B" ? "a" : "b"}`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "the phase ran two rounds");
    const [first, second] = phase.rounds!;
    const firstWinner = first.picked!.sha;
    // Round 2 starts both lanes from round 1's winner, not from the phase base.
    assert.equal(second.base, firstWinner, "round 2's base is round 1's winner");
    assert.notEqual(second.base, first.base);
    // Both lanes of round 2 carry the winner's open finding in their prompt.
    const prompts = fs.readFileSync(path.join(promptLog, "prompts.txt"), "utf8");
    const secondRound = prompts.split("=====").filter((p) => p.includes("Round 2 of this phase"));
    assert.equal(secondRound.length, 2, "both lanes were prompted in round 2");
    for (const prompt of secondRound) {
      assert.match(prompt, /REPAIR \(round 2\)/);
      assert.match(prompt, /the lane's output is wrong/);
    }
    // Round 2's winner met the acceptance rule and ended the phase on its commit.
    assert.equal(phase.candidate?.sha, second.picked!.sha);
    assert.equal(phase.phase, "DONE");
    assert.equal(phase.publishedI, phase.probe?.probedI);
    assert.equal(phase.findings.filter((f) => f.severity === "blocking" && f.status === "open").length, 0);
  } finally {
    await teardown(setup);
    cleanupDir(promptLog);
  }
});

test("plan 06g: the winner's own disclosed decisions are balloted by the lane reviews, and its discoveries are raised on it", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorkerWithDecision("a"),
    laneWorkerScriptFor: (lane) => laneWorkerWithDecision(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const K = state.phase.contract.contractVersion;
      // Seat M discovers one choice of its own; every seat ballots both the
      // lane's worker decision and M's discovery (the ids the turn-2 prompt
      // listed: the worker's first, then the discoveries in seat order).
      const ballots = [
        {
          decisionId: "D-p1-$TT_CANDIDATE_SHA8-1",
          vote: "approve",
          rationale: "the choice is sound",
          evidence: ["src/lane-a.txt:1"],
        },
        {
          decisionId: "D-p1-$TT_CANDIDATE_SHA8-disc-M-2",
          vote: "approve",
          rationale: "the discovered choice is sound",
          evidence: ["src/lane-a.txt:1"],
        },
      ];
      return laneReview(seat as Reviewer, K, {
        ballots,
        ...(seat === "M"
          ? {
              discoveries: [
                {
                  classProposal: "delegated",
                  choice: "the lane keeps the marker file out of the commit",
                  whyItMatters: "a stray file changes what the candidate ships",
                  alternatives: [{ option: "commit it", consequence: "a file nobody asked for" }],
                  recommendation: { choice: "leave it out", reason: "the goal names no such file" },
                },
              ],
            }
          : {}),
      });
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    // The winner's own decision and the discovery are both live records on it.
    const workerDecision = phase.decisions.find((d) => d.source === "worker" && d.boundCandidateSha === phase.candidate?.sha);
    assert.ok(workerDecision, "the winner's disclosed decision was assembled");
    const discovery = phase.decisions.find((d) => d.source === "reviewer-discovered" && d.boundCandidateSha === phase.candidate?.sha);
    assert.ok(discovery, "the seat's discovery was raised on the winner");
    // All three seats balloted both, so the delegated decision passed.
    for (const decision of [workerDecision!, discovery!]) {
      const ballots = phase.ballots.filter((b) => b.decisionId === decision.id && b.boundCandidateSha === phase.candidate?.sha);
      assert.equal(ballots.length, 3, `${decision.id} has three ballots`);
      assert.ok(ballots.every((b) => b.vote === "approve"));
    }
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: round 2's lane reviews ballot the winner's own decisions and its discovery with the ids the hand-off creates", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorkerWithDecision("a"),
    laneWorkerScriptFor: (lane) => laneWorkerWithDecision(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const K = state.phase.contract.contractVersion;
      const round = liveRound(state);
      // Round 1: M raises one blocking finding so round 1 is not accepted;
      // every seat ballots the lane's worker decision.
      const ballots = [
        {
          decisionId: "D-p1-$TT_CANDIDATE_SHA8-1",
          vote: "approve",
          rationale: "the choice is sound",
          evidence: ["src/lane-a.txt:1"],
        },
      ];
      if (round === 1) {
        return seat === "M"
          ? laneReview("M", K, {
              ballots,
              findings: [
                { kind: "defect", severity: "blocking", evidence: "it works is unmet: src/lane-a.txt:1 the output is wrong" },
              ],
            })
          : laneReview(seat as Reviewer, K, { ballots });
      }
      // Round 2: the phase already holds round 1's records, so the lane's own
      // discovery is numbered after them and the lane's worker decision —
      // `D-p1-<sha8>-disc-M-3` — and every seat ballots both. M also withdraws
      // its round-1 finding.
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === "M");
      return laneReview(seat as Reviewer, K, {
        ballots: [
          ...ballots,
          {
            decisionId: "D-p1-$TT_CANDIDATE_SHA8-disc-M-3",
            vote: "approve",
            rationale: "the discovered choice is sound",
            evidence: ["src/lane-a.txt:1"],
          },
        ],
        ...(seat === "M"
          ? {
              discoveries: [
                {
                  classProposal: "delegated",
                  choice: "round 2 keeps the marker file out of the commit",
                  whyItMatters: "a stray file changes what the candidate ships",
                  alternatives: [{ option: "commit it", consequence: "a file nobody asked for" }],
                  recommendation: { choice: "leave it out", reason: "the goal names no such file" },
                },
              ],
              findingStatements: open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })),
            }
          : {}),
      });
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "round 1 was not accepted");
    // The discovery the round-2 prompt listed exists on the winner and drew
    // all three ballots — the ids agree across rounds.
    const discovery = phase.decisions.find(
      (d) => d.source === "reviewer-discovered" && d.boundCandidateSha === phase.candidate?.sha,
    );
    assert.ok(discovery, "the round-2 discovery was raised on the winner");
    assert.match(discovery!.id, /-disc-M-3$/);
    const ballots = phase.ballots.filter((b) => b.decisionId === discovery!.id && b.boundCandidateSha === phase.candidate?.sha);
    assert.equal(ballots.length, 3, `${discovery!.id} drew three ballots`);
    const workerDecision = phase.decisions.find((d) => d.source === "worker" && d.boundCandidateSha === phase.candidate?.sha);
    assert.ok(workerDecision);
    assert.equal(phase.ballots.filter((b) => b.decisionId === workerDecision!.id && b.boundCandidateSha === phase.candidate?.sha).length, 3);
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: a lane whose sweep found a survivor hands its winner off tainted", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => ({
      hello: defaultWorkerHello(),
      steps: [
        // A survivor in THIS lane's worktree: the lane's own sweep must find
        // and kill it, and the winner's freeze must record the taint.
        { kind: "call-sh", command: "sleep 30 >/dev/null 2>&1 & echo started" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.worktreeTainted, true, "the lane's sweep taint reached FREEZE_COMPLETED");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: a missing lane review fails the round, so no winner is picked on fewer than six reviews", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const K = state.phase.contract.contractVersion;
      // One seat of the FIRST round never submits its review; from round 2 on
      // it behaves, so the round repeats once and then completes.
      const round = liveRound(state);
      // A reviewer that dies at once: the round must notice the missing review
      // immediately, not wait out reviewMs.
      if (seat === "B" && round === 1) return { hello: defaultReviewerHello(), steps: [{ kind: "crash", code: 9 }] };
      return laneReview(seat as Reviewer, K);
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "the incomplete round repeated");
    assert.equal(phase.rounds![0].picked, undefined, "round 1 picked no winner with a review missing");
    assert.equal(phase.rounds![0].candidates.every((c) => (c.reviews ?? []).length < 3), true);
    // The repeat costs one repair attempt, and round 2 completed with six.
    assert.equal(phase.repairRoundsUsed, 1);
    for (const c of phase.rounds![1].candidates) assert.equal((c.reviews ?? []).length, 3);
    assert.equal(phase.phase, "DONE");
    // Only the completed round's six reviews are complete sets; the incomplete
    // round's candidate a holds at most the two seats that finished (B died).
    const events = readEvents(setup.runDir).filter((r) => r.kind === "event");
    assert.ok(events.filter((r) => ROUND_EVENT(r) === "ROUND_REVIEW_SUBMITTED").length >= 6);
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "CANDIDATE_PICKED").length, 1, "only the completed round picked a winner");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: a missing pick vote fails the round, so a majority is never invented", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => {
      // Seat B dies before voting in the first round; from round 2 on it votes.
      if (seat === "B" && liveRound(state) === 1) {
        return { hello: { role: "picker" as const, tools: ROLE_TOOLS.picker }, steps: [{ kind: "crash", code: 9 }] };
      }
      return pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", `${seat} picks`);
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "the round with a missing vote repeated");
    assert.equal(phase.rounds![0].picked, undefined, "no winner was picked without all three votes");
    assert.ok((phase.rounds![0].votes.length ?? 0) < 3);
    assert.equal(phase.rounds![1].votes.length, 3);
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: one lane crashing leaves the other candidate checked, reviewed and picked without a vote", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) =>
      lane === "b"
        ? { hello: defaultWorkerHello(), steps: [{ kind: "crash", code: 9 }] }
        : laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const round = setup.conductor.state.phase.rounds![0];
    const laneA = round.candidates.find((c) => c.lane === "a")!;
    const laneB = round.candidates.find((c) => c.lane === "b")!;
    assert.ok(laneA.sha, "lane a froze a candidate");
    assert.equal(laneA.ok, true);
    // The crashed lane records no candidate at all — only the note why.
    assert.equal(laneB.sha, undefined, "the crashed lane records no candidate");
    assert.ok((laneB.note ?? "").length > 0, "the crashed lane records why");
    // The single passing candidate wins without a vote.
    assert.equal(round.votes.length, 0);
    assert.deepEqual(round.picked, { lane: "a", sha: laneA.sha, votes: 0 });
    assert.equal(round.candidates.find((c) => c.lane === "a")!.reviews?.length, 3);
    // Its failure is shown in the status.
    const status = fs.readFileSync(runPaths(setup.runDir).status, "utf8");
    assert.match(status, /no candidate/);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: both candidates failing checks repeats the round from the same base with both lanes' failures", async () => {
  const promptLog = fs.mkdtempSync("/tmp/tt-06g2-prompts-");
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["test ! -f lane-marker.txt"],
    phaseChecks: ["test ! -f lane-marker.txt"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: path.join(promptLog, "prompts.txt") },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane, round) =>
      round === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: "printf 'broken\n' > lane-marker.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
          }
        : laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "the round repeated");
    const [first, second] = phase.rounds!;
    assert.deepEqual(first.candidates.map((c) => c.ok), [false, false], "round 1: both candidates failed their checks");
    assert.equal(second.base, first.base, "the repeat starts from the same base");
    // A round costs ONE attempt, whatever its lanes: round 1 failed, round 2 ran.
    assert.equal(phase.repairRoundsUsed, 1);
    // Both lanes' prompts of round 2 carry both failures of round 1.
    const prompts = fs.readFileSync(path.join(promptLog, "prompts.txt"), "utf8");
    const secondRound = prompts.split("=====").filter((p) => p.includes("Round 2 of this phase"));
    assert.equal(secondRound.length, 2, "both lanes were prompted in round 2");
    for (const prompt of secondRound) {
      assert.match(prompt, /C1-a \(lane a\) failed its checks/);
      assert.match(prompt, /C1-b \(lane b\) failed its checks/);
    }
  } finally {
    await teardown(setup);
    cleanupDir(promptLog);
  }
});
