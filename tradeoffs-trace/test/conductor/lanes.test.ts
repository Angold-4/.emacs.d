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

import { rebuildState, type RunPlanFile } from "../../src/conductor.ts";
import { renderStatusView, statusViewInput } from "../../src/render.ts";
import { buildView, roundsSection } from "../../src/view.ts";
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
  opts: { findings?: unknown[]; findingStatements?: unknown[]; ballots?: unknown[]; discoveries?: unknown[]; items?: unknown[]; read?: string } = {},
): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: opts.discoveries ?? [] } },
      { kind: "wait-for-prompt" },
      ...(opts.read ? [{ kind: "call-tool", tool: "read", args: { path: opts.read } } as FakePiStep] : []),
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
          ...(opts.items ? { items: opts.items, arch: [] } : {}),
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
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { inspected: ["a", "b", "c", "d"], round, seat, lane, why, loserHad: { yes: false, anchors: [] } } }],
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
    // One check slot: this test pins the serial-lock behaviour (C2).
    checkSlots: 1,
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

test("plan 06n: after lane a wins, lane b's next prompt names a's candidate as its base and lane b starts a fresh session", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06n-fresh-");
  const promptLog = path.join(dir, "prompts.txt");
  const argvDir = path.join(dir, "argv");
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog, FAKE_PI_ARGV_LOG: argvDir },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const round = liveRound(state);
      const K = state.phase.contract.contractVersion;
      if (round === 1) {
        // M raises one blocking finding so round 1 is not accepted and a
        // round 2 follows.
        return seat === "M"
          ? laneReview("M", K, {
              findings: [{ kind: "defect", severity: "blocking", evidence: "it works is unmet: src/lane-a.txt:1 the lane's output is wrong" }],
            })
          : laneReview(seat as Reviewer, K);
      }
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === "M");
      return seat === "M"
        ? laneReview("M", K, { findingStatements: open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })) })
        : laneReview(seat as Reviewer, K);
    },
    // Every seat picks lane a, so lane a wins round 1 and lane b loses.
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", `${seat} picks a`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "two rounds ran");
    const winnerSha = phase.rounds![0].picked!.sha;
    assert.equal(phase.rounds![0].picked!.lane, "a", "round 1 picked lane a");
    assert.equal(phase.rounds![1].base, winnerSha, "round 2 starts from lane a's candidate");

    // Lane b's round-2 prompt names lane a's candidate as its base and says
    // lane b's own previous candidate is not in the worktree.
    const prompts = fs.readFileSync(promptLog, "utf8").split("=====").filter((p) => p.includes("Round 2 of this phase"));
    assert.equal(prompts.length, 2, "both lanes were prompted in round 2");
    for (const prompt of prompts) {
      assert.match(prompt, new RegExp(`starts from ${winnerSha.slice(0, 7)}`), "the prompt names lane a's candidate as the base");
      assert.match(prompt, /picked lane a/, "the prompt names lane a as the round-1 winner");
      assert.match(prompt, /losing lane \(b\) has its own previous candidate no longer in the worktree/, "the losing lane is told its candidate is gone");
    }

    // The losing lane's worker starts a fresh session; the winner's continues.
    const sessions = readLog(runPaths(setup.runDir).events).records.filter((r) => r.kind === "lane_session");
    const r2 = sessions.filter((r) => (r.event as { round?: number }).round === 2);
    const sessionOf = (lane: string) => r2.find((r) => (r.event as { lane?: string }).lane === lane)?.event as { continueSession?: boolean } | undefined;
    assert.equal(sessionOf("a")?.continueSession, true, "the winning lane keeps its session");
    assert.equal(sessionOf("b")?.continueSession, false, "the losing lane starts a fresh session");
  } finally {
    await teardown(setup);
    cleanupDir(dir);
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
    // Plan 06k3 (A4): a lane restarted from its own round-1 candidate must
    // make a change; the round-aware worker writes a round-specific file.
    laneWorkerScriptFor: (lane, round) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'lane ${lane} r${round}\n' > lane-${lane}-r${round}.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
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
    // Plan 06k3 (A1): the repeat is free — no repair attempt is spent.
    assert.equal(phase.repairRoundsUsed, 0, "the missing-review repeat spent no repair attempt");
    for (const c of phase.rounds![1].candidates) assert.equal((c.reviews ?? []).length, 3);
    assert.equal(phase.phase, "DONE");
    // 02k: each lane restarted from its own round-1 candidate, not the base.
    for (const c of phase.rounds![1].candidates) {
      const own = phase.rounds![0].candidates.find((r) => r.lane === c.lane)!.sha as string;
      assert.doesNotThrow(
        () => execFileSync("git", ["merge-base", "--is-ancestor", own, c.sha as string], { cwd: setup.repo.dir }),
        `lane ${c.lane}'s round-2 candidate builds on its round-1 candidate`,
      );
    }
    // Only the completed round's six reviews are complete sets; the incomplete
    // round's candidate a holds at most the two seats that finished (B died).
    const events = readEvents(setup.runDir).filter((r) => r.kind === "event");
    assert.ok(events.filter((r) => ROUND_EVENT(r) === "ROUND_REVIEW_SUBMITTED").length >= 6);
    assert.equal(events.filter((r) => ROUND_EVENT(r) === "CANDIDATE_PICKED").length, 1, "only the completed round picked a winner");
  } finally {
    await teardown(setup);
  }
});

test("02k: a slow seat's discovery and another seat's slow turn 2 each get their own review deadline, so the round completes", async () => {
  // reviewMs is 10 s; plan 06l (A3) gives turn 1 its OWN shorter deadline
  // (reviewMs/3 ≈ 3.3 s). Seat M takes 2 s to discover (within that), so A and
  // B wait 2 s at the discovery barrier; seat A then takes 6 s on turn 2.
  // Under one timer across the whole review A ran out at 10 s (02k rounds 1
  // and 2, seat A "timeout (turn 2)") and the missing review discarded the
  // round.
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const review = laneReview(seat as Reviewer, state.phase.contract.contractVersion);
      if (seat === "M") review.steps.unshift({ kind: "sleep", ms: 2_000 });
      if (seat === "A") review.steps.splice(2, 0, { kind: "sleep", ms: 6_000 });
      return review;
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 1, "the round completed on its first run");
    assert.ok(phase.rounds![0].picked, "a winner was picked");
    for (const c of phase.rounds![0].candidates) assert.equal((c.reviews ?? []).length, 3);
    assert.equal(phase.repairRoundsUsed ?? 0, 0, "no repair attempt was spent");
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
    // Plan 06k3 (A4): round 2's lanes restart from their own round-1
    // candidates, so they must make a change.
    laneWorkerScriptFor: (lane, round) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'lane ${lane} r${round}\n' > lane-${lane}-r${round}.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => {
      // Seat B dies before voting in the first round; from round 2 on it votes.
      // Round 1's two votes are split (M a, A b), so no lane has a majority
      // of all three seats and none may be invented (02k: a majority the
      // missing vote cannot change is picked instead).
      if (seat === "B" && liveRound(state) === 1) {
        return { hello: { role: "picker" as const, tools: ROLE_TOOLS.picker }, steps: [{ kind: "crash", code: 9 }] };
      }
      const lane = liveRound(state) === 1 ? (seat === "M" ? "a" : "b") : seat === "B" ? "a" : "b";
      return pickVote(seat as Reviewer, liveRound(state), lane, `${seat} picks`);
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
    // Plan 06k3 (A1): the repeat is free — no repair attempt is spent.
    assert.equal(phase.repairRoundsUsed, 0, "the missing-vote repeat spent no repair attempt");
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

// exex 01a2 (replaces plan 06g's "repeats from the same base"): both lanes'
// round-1 candidates are red, and each lane's fix changes nothing, so the
// round ends with no passing candidate. Round 2 keeps the round's base but
// each lane continues from ITS OWN red candidate (never thrown away), carries
// both lanes' failures in its prompt, fixes the tree and passes.
test("exex 01a2: when both lanes are still red, the repeat continues each lane from its own candidate with both failures", async () => {
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
        : {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: `rm -f lane-marker.txt && printf 'lane ${lane}\n' > lane-${lane}.txt` },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
          },
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only candidate"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 2, "the round repeated");
    const [first, second] = phase.rounds!;
    assert.deepEqual(first.candidates.map((c) => c.ok), [false, false], "round 1: both candidates stayed red");
    const { records } = readLog(runPaths(setup.runDir).events);
    assert.equal(records.filter((r) => r.kind === "lane_fix_unchanged").length, 2, "each lane's fix changed nothing, so its loop ended");
    assert.equal(second.base, first.base, "the round's base is unchanged");
    const roundTwo = records.find((r) => r.kind === "intent" && (r.event as { round?: number }).round === 2 && (r.event as { laneBases?: unknown }).laneBases);
    assert.deepEqual(
      (roundTwo!.event as { laneBases: Record<string, string> }).laneBases,
      Object.fromEntries(first.candidates.map((c) => [c.lane, c.sha])),
      "each lane continues from its own red candidate",
    );
    assert.deepEqual(second.candidates.map((c) => c.ok), [true, true], "round 2 fixed both trees");
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

// Plan 06k2 (A7): a lane candidate's boundary triggers are computed BEFORE
// its lane reviews, so the lane reviewers ballot them and those ballots carry
// to the winner; and citations count every decision and finding of the phase
// (superseded ones included), so a path an earlier round's decision cited is
// not re-triggered.

test("plan 06k2: a two-lane winner's boundary-trigger decisions are balloted by its lane reviews and settle", async () => {
  // Scenario 1: the winner touches an uncited boundary path.
  const setup = await setupConductor({
    phase: { id: "p1", goal: "build two candidates", acceptance: ["it works"], checks: ["true"], boundaries: ["src/**"], reserved: [], workers: 2, rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `mkdir -p src && printf 'lane ${lane}\n' > src/lane-${lane}.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    laneReviewerScriptFor: (seat, _candidate, state) =>
      laneReview(seat as Reviewer, state.phase.contract.contractVersion, {
        ballots: [
          {
            decisionId: "D-p1-$TT_CANDIDATE_SHA8-trigger-1",
            vote: "approve",
            rationale: "the boundary path is covered",
            evidence: ["README.md:1"],
          },
        ],
      }),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", `${seat} picks`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    const winner = phase.rounds![0].picked!;
    const trigger = phase.decisions.find((d) => d.source === "trigger" && d.boundCandidateSha === winner.sha);
    assert.ok(trigger, "the winner's boundary trigger was created");
    const ballots = phase.ballots.filter((b) => b.decisionId === trigger!.id && b.boundCandidateSha === winner.sha);
    assert.equal(ballots.length, 3, "the winner's lane reviews balloted the trigger");
    assert.ok(ballots.every((b) => b.vote === "approve"));
    assert.equal(phase.phase, "DONE", "the balloted trigger settles and the phase reaches DONE");
  } finally {
    await teardown(setup);
  }

  // Scenario 2: a repair round whose own disclosure cites only its new path
  // raises no trigger for the paths an earlier round's decision cited.
  const setup2 = await setupConductor({
    phase: { id: "p1", goal: "build two candidates", acceptance: ["it works"], checks: ["true"], boundaries: ["src/**"], reserved: [], workers: 2, rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane, round) => {
      const decision = (choice: string) => ({
        classProposal: "delegated",
        choice,
        whyItMatters: "a boundary path must be reviewed",
        alternatives: [{ option: "leave it uncited", consequence: "an unreviewed boundary change" }],
        recommendation: { choice: "cite the path", reason: "the boundary rule requires a disposition" },
      });
      return round === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: "mkdir -p src && printf 'a\\n' > src/a.txt && printf 'b\\n' > src/b.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [decision("Files changed: src/a.txt and src/b.txt")], assumptions: [], deviations: [] } },
            ],
          }
        : {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: "mkdir -p src && printf 'c\\n' > src/c.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [decision("Files changed: src/c.txt")], assumptions: [], deviations: [] } },
            ],
          };
    },
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const round = liveRound(state);
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open");
      return laneReview(seat as Reviewer, state.phase.contract.contractVersion, {
        ballots: [
          {
            decisionId: "D-p1-$TT_CANDIDATE_SHA8-1",
            vote: "approve",
            rationale: "the path list is adequate",
            evidence: ["README.md:1"],
          },
        ],
        ...(round === 1 && seat === "M"
          ? { findings: [{ kind: "defect", severity: "blocking", evidence: "src/a.txt:1 the earlier change is wrong" }] }
          : {}),
        ...(round >= 2 ? { findingStatements: open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed" })) } : {}),
      });
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", `${seat} picks`),
  });
  try {
    await setup2.conductor.start();
    await waitFor(() => setup2.conductor.state.phase.phase === "DONE", 180_000, 50, setup2.runDir);
    const phase = setup2.conductor.state.phase;
    assert.ok((phase.rounds?.length ?? 0) >= 2, "the phase ran a repair round");
    const winner = phase.rounds![phase.rounds!.length - 1].picked!;
    const triggers = phase.decisions.filter((d) => d.source === "trigger" && d.boundCandidateSha === winner.sha);
    assert.deepEqual(triggers, [], "an earlier round's cited paths are not re-triggered");
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup2);
  }
});

// Plan 06l (A1): a lane's checks start when THAT lane submits, not after both
// have; and both lanes' reviews run at the same time, so the round's review
// span is the longer lane's, not their sum, while the pick still waits for
// every lane review.

/** One lane's worker: `sleepMs` before it writes its file and submits. */
function laneWorkerAfter(lane: string, sleepMs: number): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "sleep", ms: sleepMs },
      { kind: "call-sh", command: `printf 'lane ${lane}\n' > lane-${lane}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** A lane review whose turn 2 sleeps `sleepMs` before submit_review, so the
 * test can measure review concurrency. */
function laneReviewAfter(
  seat: Reviewer,
  contractVersion: unknown,
  sleepMs: number,
): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      { kind: "sleep", ms: sleepMs },
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

test("plan 06l: a lane's checks start when it submits, while the other lane is still working", async () => {
  // Lane a submits at once; lane b sleeps 6 s. Without A1, lane a's check
  // waited for lane b's build, so its `lane_check_started` came after lane b's
  // CANDIDATE_SUBMITTED.
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => (lane === "b" ? laneWorkerAfter("b", 6_000) : laneWorker("a")),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", "the only pick"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const records = readLog(runPaths(setup.runDir).events).records;
    const checkStartA = records.find((r) => r.kind === "lane_check_started" && (r.event as { lane?: string }).lane === "a");
    assert.ok(checkStartA, "lane a's check started");
    const submittedB = records.find(
      (r) => r.kind === "event" && (r.event as { type?: string; lane?: string }).type === "CANDIDATE_SUBMITTED" && (r.event as { lane?: string }).lane === "b",
    );
    assert.ok(submittedB, "lane b submitted a candidate");
    assert.ok(
      Date.parse(checkStartA!.ts) < Date.parse(submittedB!.ts),
      `lane a's check (${checkStartA!.ts}) must start before lane b submits (${submittedB!.ts})`,
    );
  } finally {
    await teardown(setup);
  }
});

test("plan 06l: both lanes' reviews run concurrently and the pick still waits for all of them", async () => {
  // Every lane review sleeps 6 s on turn 2. Sequentially the round's six
  // reviews take ~12 s; concurrently ~6 s. The pick must still wait for all.
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReviewAfter(seat as Reviewer, state.phase.contract.contractVersion, 6_000),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", `${seat} picks`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const records = readLog(runPaths(setup.runDir).events).records;
    // Six reviews, three seats on each of the two candidates.
    const submitted = records.filter((r) => r.kind === "lane_review_submitted");
    assert.equal(submitted.length, 6, "all six lane reviews completed");
    // The review span: the earliest review start to the latest review finish.
    const intents = records.filter((r) => r.kind === "intent" && typeof (r.event as { candidateSha?: string }).candidateSha === "string" && typeof (r.event as { seat?: string }).seat === "string");
    assert.equal(intents.length, 6, "one review start per seat and candidate");
    const firstStart = Math.min(...intents.map((r) => Date.parse(r.ts)));
    const lastFinish = Math.max(...submitted.map((r) => Date.parse(r.ts)));
    const span = lastFinish - firstStart;
    // Concurrent: ~6 s (the longer lane). Sequential: ~12 s (their sum). A
    // loose 16 s bound rides out host load while still failing a serial sum.
    assert.ok(span < 16_000, `the review span must be the longer lane's (~6 s), not their sum (~12 s): ${span}ms`);
    // The two lanes' review spans OVERLAP — the direct concurrency evidence.
    const spanOf = (lane: string) => {
      const starts = intents.filter((r) => (r.event as { lane?: string }).lane === lane).map((r) => Date.parse(r.ts));
      const ends = submitted.filter((r) => (r.event as { lane?: string }).lane === lane).map((r) => Date.parse(r.ts));
      return { start: Math.min(...starts), end: Math.max(...ends) };
    };
    const a = spanOf("a");
    const b = spanOf("b");
    assert.ok(Math.max(a.start, b.start) < Math.min(a.end, b.end), `lane a [${a.start},${a.end}] and lane b [${b.start},${b.end}] must overlap`);
    // The pick starts only after every lane review.
    const picks = records.filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "PICK_VOTE");
    assert.equal(picks.length, 3, "three pick votes");
    const firstPick = Math.min(...picks.map((r) => Date.parse(r.ts)));
    assert.ok(firstPick >= lastFinish, `the pick (${firstPick}) must start after the last review (${lastFinish})`);
    const phase = setup.conductor.state.phase;
    for (const c of phase.rounds![0].candidates) assert.equal((c.reviews ?? []).length, 3);
  } finally {
    await teardown(setup);
  }
});

/** A structured lane worker that submits coverage for R1 and R2. Each round
 * appends to its lane's file, so a later round's tree always differs; a lane
 * told to `fail` writes the marker its checks reject. */
function structuredLaneWorker(lane: string, round = 1, fail = false): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane} round ${round}${fail ? " fail" : ""}\n' >> lane-${lane}.txt` },
      {
        kind: "call-submit",
        tool: "submit_coverage",
        args: {
          items: [
            { id: "R1", status: "done", where: [`lane-${lane}.txt:1`], tests: [] },
            { id: "R2", status: "done", where: [`lane-${lane}.txt:1`], tests: [] },
          ],
          arch: [],
        },
      },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

test("plan 06n: a candidate that loses a met item in a file it changed is never adopted, and the next round restarts from the best candidate", async () => {
  // Plan 06n (A7/R9), end to end over three rounds:
  // - round 1: both lanes meet R1 and R2; lane a is picked and becomes the
  //   best candidate (M's blocking finding keeps the phase going);
  // - round 2: lane b fails its checks, so lane a is the only passing lane
  //   and is picked although its reviews mark R1 unmet with evidence in
  //   lane-a.txt, a file it changed: (c) ITEMS_REGRESSED [R1], round 3 starts
  //   from the best candidate and its repair prompt lists R1 first;
  // - round 3: every seat votes for lane a, which lost R2; lane b lost
  //   nothing, so (a) lane a cannot be picked and lane b wins.
  const dir = fs.mkdtempSync("/tmp/tt-06n-r9-");
  const promptLog = path.join(dir, "prompts.txt");
  const met = (lane: string) => [
    { id: "R1", verdict: "met", evidence: `lane-${lane}.txt:1` },
    { id: "R2", verdict: "met", evidence: `lane-${lane}.txt:1` },
  ];
  const setup = await setupConductor({
    phase: lanePhase({
      requirements: [
        { id: "R1", title: "R1 holds", text: "R1 holds", arch: [], verify: ["review"] },
        { id: "R2", title: "R2 holds", text: "R2 holds", arch: [], verify: ["review"] },
      ],
      constraints: [],
      checks: ["if grep -q fail lane-b.txt 2>/dev/null; then exit 1; fi; exit 0"],
    }),
    checks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => structuredLaneWorker("a"),
    laneWorkerScriptFor: (lane, round) => structuredLaneWorker(lane, round, lane === "b" && round === 2),
    laneReviewerScriptFor: (seat, _candidate, state, lane) => {
      const round = liveRound(state);
      const K = state.phase.contract.contractVersion;
      const read = `lane-${lane}.txt`;
      let items = met(lane!);
      if (round === 2 && lane === "a") items = [{ id: "R1", verdict: "unmet", evidence: "lane-a.txt:1 the R1 guard is gone" }, items[1]];
      if (round === 3 && lane === "a") items = [items[0], { id: "R2", verdict: "unmet", evidence: "lane-a.txt:1 R2 is lost" }];
      if (round === 1) {
        return seat === "M"
          ? laneReview("M", K, { items, read, findings: [{ kind: "defect", severity: "blocking", evidence: `lane-${lane}.txt:1 the output is wrong` }] })
          : laneReview(seat as Reviewer, K, { items, read });
      }
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === "M");
      return seat === "M"
        ? laneReview("M", K, { items, read, findingStatements: open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })) })
        : laneReview(seat as Reviewer, K, { items, read });
    },
    // Every seat always votes for lane a.
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", `${seat} picks a`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 240_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    const rounds = phase.rounds!;
    assert.equal(rounds.length, 3, "three rounds ran");
    const best = rounds[0].picked!;
    assert.equal(best.lane, "a", "round 1 picked lane a, the best candidate");
    const rejected = rounds[1].picked!;
    assert.equal(rejected.lane, "a", "round 2 picked lane a, the only passing lane");

    // (c) The regression is recorded, with the ids and the rejected sha.
    const regressed = readEvents(setup.runDir)
      .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "ITEMS_REGRESSED")
      .map((r) => r.event);
    assert.deepEqual(regressed, [{ type: "ITEMS_REGRESSED", round: 2, items: ["R1"], sha: rejected.sha }]);
    // The next round restarts from the best candidate, never the rejected one.
    assert.equal(rounds[2].base, best.sha, "round 3 starts from the best candidate");
    // Its repair prompt lists R1 first, with the rejected sha for reference.
    const r3 = fs.readFileSync(promptLog, "utf8").split("=====").filter((p) => p.includes("Round 3 of this phase"));
    assert.equal(r3.length, 2, "both lanes were prompted in round 3");
    for (const prompt of r3) {
      assert.match(prompt, new RegExp(`Regressed \\(fix these first\\): R1 — met by the best candidate ${best.sha.slice(0, 7)}, lost by candidate ${rejected.sha.slice(0, 7)}`));
      const blocking = prompt.slice(prompt.indexOf("Blocking (must be fixed):")).split("\n");
      assert.match(blocking[1] ?? "", /^- Regressed \(fix these first\): R1/, `R1's regression is the first blocking line: ${blocking.slice(0, 3).join(" | ")}`);
      assert.match(prompt, /its evaluation lost R1/, "the round prompt says the pick was not adopted");
    }
    // The status shows the regression while it is the latest pick.
    const view = buildView(setup.runDir, setup.plan as RunPlanFile, false);
    const atRegression = { ...setup.conductor.state, phase: { ...phase, rounds: rounds.slice(0, 2) } };
    const status = renderStatusView(statusViewInput({ runDir: setup.runDir, plan: setup.plan as RunPlanFile, state: atRegression, view, alive: false }));
    assert.match(status, /regressed: R1/, "the status names the regressed item");

    // Neither lane continues a session that remembers the rejected edits.
    const sessions = readLog(runPaths(setup.runDir).events).records.filter(
      (r) => r.kind === "lane_session" && (r.event as { round?: number }).round === 3,
    );
    assert.deepEqual(
      sessions.map((r) => (r.event as { continueSession?: boolean }).continueSession),
      [false, false],
      "both lanes start round 3 in a fresh session",
    );

    // (a) Round 3: lane a lost R2 against the best candidate; lane b lost
    // none, so lane b is picked although every seat voted for lane a.
    assert.equal(rounds[2].votes.filter((v) => v.lane === "a").length, 3, "every seat voted for lane a");
    assert.equal(rounds[2].picked?.lane, "b", "lane a cannot be picked: it lost an item the best candidate met");
    assert.equal(phase.candidate?.sha, rounds[2].picked?.sha, "the accepted candidate is round 3's pick");
  } finally {
    await teardown(setup);
    cleanupDir(dir);
  }
});

test("plan 06n (R9b): when both lanes lose a met item no lane is picked and both restart from the best candidate", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06n-r9b-");
  const promptLog = path.join(dir, "prompts.txt");
  const setup = await setupConductor({
    phase: lanePhase({
      requirements: [
        { id: "R1", title: "R1 holds", text: "R1 holds", arch: [], verify: ["review"] },
        { id: "R2", title: "R2 holds", text: "R2 holds", arch: [], verify: ["review"] },
      ],
      constraints: [],
    }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => structuredLaneWorker("a"),
    laneWorkerScriptFor: (lane, round) => structuredLaneWorker(lane, round),
    laneReviewerScriptFor: (seat, _candidate, state, lane) => {
      const round = liveRound(state);
      const K = state.phase.contract.contractVersion;
      const read = `lane-${lane}.txt`;
      const ev = `lane-${lane}.txt:1`;
      let items = [
        { id: "R1", verdict: "met", evidence: ev },
        { id: "R2", verdict: "met", evidence: ev },
      ];
      // Round 2: lane a loses R1, lane b loses R2.
      if (round === 2) items = items.map((i) => ((lane === "a" ? i.id === "R1" : i.id === "R2") ? { ...i, verdict: "unmet", evidence: `${ev} lost` } : i));
      if (round === 1) {
        return seat === "M"
          ? laneReview("M", K, { items, read, findings: [{ kind: "defect", severity: "blocking", evidence: `${ev} the output is wrong` }] })
          : laneReview(seat as Reviewer, K, { items, read });
      }
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === "M");
      return seat === "M"
        ? laneReview("M", K, { items, read, findingStatements: open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })) })
        : laneReview(seat as Reviewer, K, { items, read });
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", `${seat} picks a`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 240_000, 50, setup.runDir);
    const rounds = setup.conductor.state.phase.rounds!;
    assert.equal(rounds.length, 3, "three rounds ran");
    const best = rounds[0].picked!;
    assert.equal(rounds[1].votes.length, 3, "round 2's seats all voted");
    assert.equal(rounds[1].picked, undefined, "round 2 picked no lane: both lanes lost an item");
    // Round 3's lanes both start from the best candidate, never from their
    // own regressing round-2 candidates.
    const intents = readLog(runPaths(setup.runDir).events).records.filter(
      (r) => {
        const e = r.event as { round?: number; lane?: string; worktree?: string; base?: string };
        return r.kind === "intent" && e.round === 3 && typeof e.lane === "string" && typeof e.worktree === "string";
      },
    );
    assert.deepEqual(
      intents.map((r) => (r.event as { base?: string }).base).sort(),
      [best.sha, best.sha],
      "both lanes restart from the best candidate",
    );
    // Each lane is told which items it lost.
    const r3 = fs.readFileSync(promptLog, "utf8").split("=====").filter((p) => p.includes("Round 3 of this phase"));
    assert.equal(r3.length, 2);
    for (const prompt of r3) {
      assert.match(prompt, /Round 2 picked no lane: every lane lost a plan item/);
      assert.match(prompt, /lane a lost R1; lane b lost R2/);
      assert.doesNotMatch(prompt, /could not complete \(a review or pick vote was missing\)/);
    }
    assert.equal(rounds[2].picked?.lane, "a", "round 3 picks as usual");
  } finally {
    await teardown(setup);
    cleanupDir(dir);
  }
});

// exex 01a2: a lane never hands in a red tree. Lane a's first candidate fails
// the round check; the conductor gives the failure back to lane a's own
// worker (continuing its session, on its own candidate), which fixes it and
// submits again. Only the green commit is reviewed, and the round picks.
test("exex 01a2: a red lane candidate goes back to its own worker, which fixes it in the same round", async () => {
  const promptLog = fs.mkdtempSync(path.join(fs.realpathSync(path.dirname(fileURLToPath(import.meta.url))), ".lane-fix-"));
  const launches: Record<string, number> = {};
  const setup = await setupConductor({
    phase: lanePhase({ checks: ["test ! -f red.txt"] }),
    checks: ["test ! -f red.txt"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 60_000 },
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: path.join(promptLog, "prompts.txt") },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => {
      launches[lane] = (launches[lane] ?? 0) + 1;
      if (lane !== "a") return laneWorker(lane);
      return launches[lane] === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: "printf 'lane a\\n' > lane-a.txt && printf 'broken\\n' > red.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
          }
        : {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: "rm red.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
          };
    },
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", `${seat} prefers a`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const round = setup.conductor.state.phase.rounds![0];
    const a = round.candidates.find((c) => c.lane === "a")!;
    assert.equal(launches.a, 2, "lane a's worker was launched once to build and once to fix");
    assert.equal(a.fixes, 1);
    assert.equal(a.ok, true, "the fixed candidate passed its own check");
    assert.equal(round.picked?.lane, "a");
    const reviewed = (a.reviews ?? []).map((r) => r.review.candidateSha);
    assert.ok(reviewed.length > 0 && reviewed.every((sha) => sha === a.sha), "only the green commit was reviewed");
    const prompts = fs.readFileSync(path.join(promptLog, "prompts.txt"), "utf8");
    assert.match(prompts, /FAILED the round checks, so its reviews were cancelled/);
    const { records } = readLog(runPaths(setup.runDir).events);
    assert.ok(records.some((r) => r.kind === "lane_fix_started"), "the fix is recorded");
    // 09-2: the red candidate's reviews started at its submission and were
    // cancelled when its check failed; none was retried.
    const cancels = records.filter((r) => r.kind === "lane_reviews_cancelled").map((r) => r.event as { lane: string; candidateSha: string });
    assert.equal(cancels.length, 1, "only the red candidate's reviews were cancelled");
    assert.equal(cancels[0].lane, "a");
    assert.notEqual(cancels[0].candidateSha, a.sha);
    assert.equal(records.filter((r) => r.kind === "lane_review_retried").length, 0, "a cancelled review is never retried");
  } finally {
    await teardown(setup);
    cleanupDir(promptLog);
  }
});

// tt 09 (change 3): combine the lanes. Round 1's two lanes each meet a
// different plan item (lane a R1, lane b R2); the seats pick lane a and the
// phase does not accept it. A consolidator (fake-pi) starts from lane a's
// commit and takes lane b's file for R2; the checks pass, and round 2's lanes
// both start from the consolidated commit and are told what it carries.
function combineSetup(
  promptLog: string,
  consolidator: (round: number, laneB: string) => FakePiStep[],
  consolidatorRuns: number[],
): Promise<TestConductorSetup> {
  return setupConductor({
    phase: lanePhase({
      requirements: [
        { id: "R1", title: "R1 holds", text: "lane a's half: R1 holds", arch: [], verify: ["review"] },
        { id: "R2", title: "R2 holds", text: "lane b's half: R2 holds", arch: [], verify: ["review"] },
      ],
      constraints: [],
    }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 60_000 },
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => structuredLaneWorker("a"),
    laneWorkerScriptFor: (lane, round) => structuredLaneWorker(lane, round),
    laneReviewerScriptFor: (seat, _candidate, state, lane) => {
      const round = liveRound(state);
      const K = state.phase.contract.contractVersion;
      const read = `lane-${lane}.txt`;
      const ev = `lane-${lane}.txt:1`;
      // Round 1: each lane meets its own half only. Round 2 (from the
      // consolidated commit) meets both.
      const items =
        round === 1
          ? [
              { id: "R1", verdict: lane === "a" ? "met" : "unmet", evidence: `${ev} R1 ${lane === "a" ? "is done" : "is missing"}` },
              { id: "R2", verdict: lane === "b" ? "met" : "unmet", evidence: `${ev} R2 ${lane === "b" ? "is done" : "is missing"}` },
            ]
          : [
              { id: "R1", verdict: "met", evidence: `${ev} R1` },
              { id: "R2", verdict: "met", evidence: `${ev} R2` },
            ];
      if (round === 1) {
        return seat === "M"
          ? laneReview("M", K, { items, read, findings: [{ kind: "defect", severity: "blocking", evidence: `${ev} R2 is unmet on the main path` }] })
          : laneReview(seat as Reviewer, K, { items, read });
      }
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === "M");
      return seat === "M"
        ? laneReview("M", K, { items, read, findingStatements: open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })) })
        : laneReview(seat as Reviewer, K, { items, read });
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a", `${seat} picks a`),
    consolidatorScriptFor: (round, _n, state) => {
      consolidatorRuns.push(round);
      const b = state.phase.rounds!.find((r) => r.round === round)!.candidates.find((c) => c.lane === "b")!.sha!;
      return { hello: defaultWorkerHello(), steps: consolidator(round, b) };
    },
  });
}

test("tt 09: a consolidator combines the winner with the item the other lane met, and the next round's lanes start from it", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-09-combine-");
  const promptLog = path.join(dir, "prompts.txt");
  const consolidatorRuns: number[] = [];
  const setup = await combineSetup(
    promptLog,
    (_round, b) => [
      // Take lane b's work for R2: its file, from its commit.
      { kind: "call-sh", command: `git checkout ${b} -- lane-b.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
    consolidatorRuns,
  );
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 240_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    const rounds = phase.rounds!;
    assert.equal(rounds.length, 2, "two rounds ran");
    const [first, second] = rounds;
    assert.equal(first.picked?.lane, "a", "the seats picked lane a");
    const b = first.candidates.find((c) => c.lane === "b")!;
    assert.deepEqual(consolidatorRuns, [1], "one consolidator ran, after round 1");

    // The combine step is recorded with what it took, and it is green.
    const c = first.consolidated!;
    assert.equal(c.ok, true, `the consolidation passed: ${c.note ?? ""}`);
    assert.equal(c.from, first.picked!.sha);
    assert.deepEqual(c.took, [{ lane: "b", sha: b.sha, items: ["R2"] }]);
    // The consolidated commit sits on the winner and carries lane b's file.
    const git = (...args: string[]) => execFileSync("git", args, { cwd: setup.repo.dir, encoding: "utf8" }).trim();
    assert.equal(git("rev-parse", `${c.sha}^`), first.picked!.sha, "the consolidator started from the winner");
    assert.equal(git("show", `${c.sha}:lane-b.txt`), git("show", `${b.sha}:lane-b.txt`), "it took lane b's work");
    assert.equal(git("show", `${c.sha}:lane-a.txt`), git("show", `${first.picked!.sha}:lane-a.txt`), "it kept the winner's work");
    const { records } = readLog(runPaths(setup.runDir).events);
    assert.ok(records.some((r) => r.kind === "consolidation_check_finished" && (r.event as { passed?: boolean }).passed === true), "the round checks ran on it");

    // Round 2 starts both lanes from the consolidated commit.
    assert.equal(second.base, c.sha, "round 2's base is the consolidated commit");
    const intents = records.filter((r) => {
      const e = r.event as { round?: number; lane?: string; worktree?: string };
      return r.kind === "intent" && e.round === 2 && typeof e.lane === "string" && typeof e.worktree === "string";
    });
    assert.deepEqual(intents.map((r) => (r.event as { base?: string }).base), [c.sha, c.sha], "both lanes start from it");
    // The consolidator was told the exact item, its commits and the constraints.
    const prompts = fs.readFileSync(promptLog, "utf8").split("=====");
    const consolidator = prompts.find((p) => p.includes("You are the CONSOLIDATOR of round 1"));
    assert.ok(consolidator, "the consolidator was prompted");
    assert.match(consolidator!, new RegExp(`From lane b \\(C1-b, commit ${b.sha}\\)`));
    assert.match(consolidator!, /- R2 R2 holds: lane b's half: R2 holds/);
    assert.match(consolidator!, /the seats' evidence it is met: M: lane-b\.txt:1 R2 is done/);
    assert.doesNotMatch(consolidator!, /- R1 /, "it takes only what the winner lacks");
    assert.match(consolidator!, /Never rewrite the winner's work wholesale/);
    // Each round-2 lane is told its base is the consolidated commit and what it carries.
    const r2 = prompts.filter((p) => p.includes("Round 2 of this phase"));
    assert.equal(r2.length, 2, "both lanes were prompted in round 2");
    for (const prompt of r2) {
      assert.match(prompt, new RegExp(`consolidated commit C1-a\\+b ${c.sha!.slice(0, 7)}`));
      assert.match(prompt, /lane b's R2/);
    }
    // The views show it.
    const view = buildView(setup.runDir, setup.plan as RunPlanFile, false);
    const status = renderStatusView(statusViewInput({ runDir: setup.runDir, plan: setup.plan as RunPlanFile, state: setup.conductor.state, view, alive: false }));
    assert.match(status, new RegExp(`round 2 starts from consolidated C1-a\\+b ${c.sha!.slice(0, 7)} = C1-a \\+ lane b's items R2`));
    assert.match(roundsSection(phase).join("\n"), /consolidated C1-a\+b/, "tt summary's rounds section names it too");
    // A restart folds the same record back from the log.
    const rebuilt = rebuildState(setup.runDir, setup.plan);
    assert.deepEqual(rebuilt.phase.rounds![0].consolidated, c);
    assert.equal(phase.candidate?.sha, second.picked?.sha, "round 2's pick was accepted");
  } finally {
    await teardown(setup);
    cleanupDir(dir);
  }
});

// tt 09 (change 3): a consolidator that takes nothing is no base. The failure
// is recorded with its reason, round 2 starts from the winner exactly as
// before, and its lanes are told where the other lane's items are.
test("tt 09: a consolidation that changes nothing falls back to the winner and records why", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-09-combine-fail-");
  const promptLog = path.join(dir, "prompts.txt");
  const consolidatorRuns: number[] = [];
  const setup = await combineSetup(
    promptLog,
    () => [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    consolidatorRuns,
  );
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 240_000, 50, setup.runDir);
    const [first, second] = setup.conductor.state.phase.rounds!;
    const b = first.candidates.find((c) => c.lane === "b")!;
    assert.deepEqual(consolidatorRuns, [1]);
    assert.deepEqual(first.consolidated, {
      from: first.picked!.sha,
      lane: "a",
      took: [{ lane: "b", sha: b.sha, items: ["R2"] }],
      ok: false,
      note: "the consolidator changed nothing on the winner",
      fixes: 0,
    });
    assert.equal(second.base, first.picked!.sha, "round 2 starts from the winner, as before");
    const r2 = fs.readFileSync(promptLog, "utf8").split("=====").filter((p) => p.includes("Round 2 of this phase"));
    assert.equal(r2.length, 2);
    for (const prompt of r2) {
      assert.match(prompt, /Combining it with what the other lane met failed \(the consolidator changed nothing on the winner\)/);
      assert.match(prompt, new RegExp(`lane b ${b.sha!.slice(0, 7)}: R2`));
    }
  } finally {
    await teardown(setup);
    cleanupDir(dir);
  }
});

// tt 09-2 (async review): a candidate's reviews start when its lane submits,
// while its checks run. Each check sleeps 3 s (and, with one check slot, the
// machine-wide lock queues lane b's behind lane a's), so every lane's reviews are dispatched
// before its check finishes, the reviewers are told the checks are running,
// the two checks still never overlap, and the round completes.
test("09-2: a lane's reviews are dispatched before its check finishes, and the round completes", async () => {
  const promptDir = fs.mkdtempSync(path.join(fs.realpathSync(path.dirname(fileURLToPath(import.meta.url))), ".lane-async-"));
  const setup = await setupConductor({
    // One check slot: this test pins the serial-lock behaviour (C2).
    checkSlots: 1,
    phase: lanePhase({ checks: ["sleep 3 && true"] }),
    checks: ["sleep 3 && true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraReviewerEnv: (seat) => ({ FAKE_PI_PROMPT_LOG: path.join(promptDir, `reviewer-${seat}.txt`) }),
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b", `${seat} picks`),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const records = readLog(runPaths(setup.runDir).events).records;
    const round = setup.conductor.state.phase.rounds![0];
    for (const c of round.candidates) {
      // The lane's review dispatches (one intent per seat) come before its
      // check's finish record.
      const finished = records.find(
        (r) => r.kind === "lane_check_finished" && (r.event as { lane?: string }).lane === c.lane && (r.event as { candidateSha?: string }).candidateSha === c.sha,
      );
      assert.ok(finished, `lane ${c.lane}'s check finished`);
      const dispatched = records.filter(
        (r) => r.kind === "intent" && (r.event as { candidateSha?: string }).candidateSha === c.sha && typeof (r.event as { seat?: string }).seat === "string",
      );
      assert.equal(dispatched.length, 3, `lane ${c.lane}: one review per seat, dispatched once`);
      for (const d of dispatched) {
        assert.ok(Date.parse(d.ts) < Date.parse(finished!.ts), `lane ${c.lane}: review ${d.ts} dispatched before its check finished (${finished!.ts})`);
      }
      assert.equal(c.ok, true);
      assert.deepEqual((c.reviews ?? []).map((r) => r.seat).sort(), ["A", "B", "M"], `lane ${c.lane}: all three reviews count`);
    }
    // The pick waited for all six reviews, and the round picked.
    const submitted = records.filter((r) => r.kind === "lane_review_submitted").map((r) => Date.parse(r.ts));
    assert.equal(submitted.length, 6);
    const firstPick = Math.min(
      ...records.filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "PICK_VOTE").map((r) => Date.parse(r.ts)),
    );
    assert.ok(firstPick >= Math.max(...submitted), "no seat voted before every review was in");
    assert.equal(round.picked?.lane, "b");
    assert.equal(records.filter((r) => r.kind === "lane_reviews_cancelled").length, 0, "a passing candidate's reviews are never cancelled");
    // C2 still holds: the two checks never overlap.
    const [first, second] = checkIntervals(setup.runDir).sort((x, y) => x.start - y.start);
    assert.ok(first.finish <= second.start, "the two checks never overlap");
    // The reviewers were told the checks run in parallel with them.
    const prompts = ["M", "A", "B"].map((seat) => fs.readFileSync(path.join(promptDir, `reviewer-${seat}.txt`), "utf8")).join("\n");
    assert.match(prompts, /checks are running in parallel with your review; the conductor reports their result/);
  } finally {
    await teardown(setup);
    cleanupDir(promptDir);
  }
});
