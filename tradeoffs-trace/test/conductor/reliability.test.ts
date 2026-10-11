// Plan 06k3: a failure inside tt never costs a phase a round or a worker's
// work.
//
// - A lane review or pick turn that ends without its submission is retried
//   once with a FRESH seat agent before the round fails (A1/R1).
// - A round that fails only because a review or vote is still missing after
//   the retry is repeated without spending a repair attempt, at most twice per
//   phase (A1/R2).
// - At 75% of the attempt the worker is told how many minutes remain and to
//   submit (A2/R3).
// - At the deadline the worker's uncommitted work is committed to a ref, never
//   discarded; the next attempt of that lane starts from it and its prompt
//   names the ref (A2/R4).
// - submit_phase carries one answer per open blocking finding; a submission
//   that leaves one unanswered is refused, naming the ids (A3/R5).
// - Each open finding's reproduction command is re-run on the new candidate
//   and the table reaches every reviewer prompt (A3/R6).
// - An unchanged-resubmission permission's use is an event, not process
//   memory; a lane restarted from its own losing candidate that resubmits it
//   unchanged is refused naming that candidate (A4/R7).

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { Conductor, rebuildState, runPaths } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  FAKE_PI_PATH,
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
  // Kept short: the review/pick turns here are all scripted and answer at
  // once, so a long deadline only slows the free-repeat rounds (this file runs
  // beside three others under one check command).
  reviewMs: 6_000,
  panelMs: 8_000,
};

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

function lanePhase(overrides: Record<string, unknown> = {}) {
  return {
    id: "p1",
    goal: "build two candidates from one base",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers: 2,
    rounds: 10,
    ...overrides,
  };
}

function submitStep(extra: Record<string, unknown> = {}): FakePiStep {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [], ...extra } };
}

/** A lane worker that writes a round-specific file (so a lane restarted from
 * its own candidate is never refused as an unchanged resubmission) and
 * submits. */
function laneWorkerRound(lane: string, round: number): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane} r${round}\n' > lane-${lane}-r${round}.txt` },
      submitStep(),
    ],
  };
}

function reviewArgs(seat: Reviewer, contractVersion: unknown, extra: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    reviewer: seat,
    phaseId: "p1",
    candidateSha: "$TT_CANDIDATE_SHA",
    contractVersion,
    correctionStatements: [],
    findingStatements: [],
    ballots: [],
    findings: [],
    ...extra,
  };
}

/** One seat's two-turn lane review. `never` makes it end turn 2 without
 * submit_review (the retry path). */
function laneReview(seat: Reviewer, contractVersion: unknown, never = false): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      ...(never ? [] : [{ kind: "call-submit", tool: "submit_review", args: reviewArgs(seat, contractVersion) } as FakePiStep]),
    ],
  };
}

function pickVote(seat: Reviewer, round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { round, seat, lane, why: `${seat} picks ${lane}`, loserHad: { yes: false, anchors: [] } } }],
  };
}

function liveRound(state: State): number {
  return state.phase.rounds?.length ?? 1;
}

// --- A1/R1 ----------------------------------------------------------------

test("plan 06k3: a lane review that times out is retried once with a fresh seat agent and the round completes", async () => {
  const calls = new Map<string, number>();
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorkerRound("a", 1),
    laneWorkerScriptFor: (lane, round) => laneWorkerRound(lane, round),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const K = state.phase.contract.contractVersion;
      if (seat !== "A") return laneReview(seat as Reviewer, K);
      // Seat A's FIRST review of each candidate ends without submit_review;
      // its retry (a fresh agent, a new script) does. The two candidates are
      // reviewed one after the other, so A's calls alternate.
      const n = (calls.get(seat) ?? 0) + 1;
      calls.set(seat, n);
      return laneReview("A", K, n % 2 === 1);
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), seat === "B" ? "a" : "b"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.rounds?.length, 1, "the round completed on its first run");
    const round = phase.rounds![0];
    assert.ok(round.picked, "a winner was picked");
    for (const c of round.candidates) assert.equal((c.reviews ?? []).length, 3, `candidate ${c.lane} has all three reviews`);
    assert.equal(phase.repairRoundsUsed, 0, "the retry cost no repair attempt");
    // The retry is logged.
    const retried = readEvents(setup.runDir).filter((r) => r.kind === "lane_review_retried");
    assert.ok(retried.length >= 1, "the lane review retry is logged");
    assert.ok(
      retried.every((r) => (r.event as { seat?: string }).seat === "A"),
      "only seat A's review was retried",
    );
  } finally {
    await teardown(setup);
  }
});

test("plan 06k3: a pick seat that never starts is retried once with a fresh session and the round completes", async () => {
  const calls = new Map<string, number>();
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, helloTimeoutMs: 2_000 },
    workerScript: () => laneWorkerRound("a", 1),
    laneWorkerScriptFor: (lane, round) => laneWorkerRound(lane, round),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => {
      if (seat === "M") {
        const n = (calls.get(seat) ?? 0) + 1;
        calls.set(seat, n);
        // The FIRST picker for M never answers hello; its retry (a fresh
        // agent, a new script) votes. Before F-13 this returned normally and
        // the retry never ran.
        if (n === 1) return { hello: { role: "picker" as const, tools: ROLE_TOOLS.picker }, helloDelayMs: 5_000, steps: [] };
      }
      return pickVote(seat as Reviewer, liveRound(state), "a");
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    const round = setup.conductor.state.phase.rounds![0];
    assert.ok(round.picked, "a winner was picked after the retry");
    assert.equal(round.votes.length, 3, "all three seats voted");
    assert.equal(setup.conductor.state.phase.repairRoundsUsed, 0, "the pick retry cost no repair attempt");
    assert.ok(
      readEvents(setup.runDir).some((r) => r.kind === "pick_retried" && (r.event as { seat?: string }).seat === "M"),
      "the pick seat that never started was retried",
    );
  } finally {
    await teardown(setup);
  }
});

// --- A1/R2 ----------------------------------------------------------------

test("plan 06k3: a round dropped for a missing review after its retry is repeated without spending a repair attempt", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorkerRound("a", 1),
    laneWorkerScriptFor: (lane, round) => laneWorkerRound(lane, round),
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const K = state.phase.contract.contractVersion;
      const round = liveRound(state);
      // Seat B's review and its fresh-agent retry both end without
      // submit_review in rounds 1, 2 and 3; from round 4 on it behaves.
      if (seat === "B" && round <= 3) return laneReview("B", K, true);
      return laneReview(seat as Reviewer, K);
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    // Rounds 1 and 2 dropped for a missing review repeated for free; round 3's
    // drop is the third, so it spent one repair attempt and round 4 (attempt 2)
    // completed.
    assert.equal(phase.rounds?.length, 4, "the phase ran four rounds");
    assert.equal(phase.repairRoundsUsed, 1, "two free repeats, then the third drop counted");
    const repeats = readEvents(setup.runDir).filter((r) => r.kind === "round_repeated");
    assert.equal(repeats.length, 2, "exactly two free repeats were recorded");
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "round_incomplete").length, 1, "the third drop counted as an incomplete round");
    assert.ok(phase.rounds!.slice(0, 3).every((r) => r.picked === undefined), "no winner was picked in the dropped rounds");
    assert.ok(phase.rounds![3].picked, "the fourth round picked a winner");
  } finally {
    await teardown(setup);
  }
});

// --- A2/R3 ----------------------------------------------------------------

test("plan 06k3: a worker still running at 75% of its attempt is told how long it has left", async () => {
  // Single worker.
  const singleDir = fs.mkdtempSync("/tmp/tt-06k3-steer-single-");
  const singleLog = path.join(singleDir, "steers.log");
  const single = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: { ...FAST, workerAttemptMs: 12_000 },
    extraWorkerEnv: { FAKE_PI_STEER_LOG: singleLog },
    workerScriptForAttempt: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "sleep", ms: 10_500 }, { kind: "call-sh", command: "printf 'done\\n' > done.txt" }, submitStep()],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.contract.contractVersion) },
      ],
    }),
  });
  try {
    await single.conductor.start();
    await waitFor(() => single.conductor.state.phase.phase === "DONE", 150_000, 50, single.runDir);
    const steers = fs.readFileSync(singleLog, "utf8");
    assert.match(steers, /minute/, "the steer names the minutes left");
    assert.match(steers, /submit_phase/, "the steer says to submit");
  } finally {
    await teardown(single);
    fs.rmSync(singleDir, { recursive: true, force: true });
  }

  // Lane worker.
  const laneDir = fs.mkdtempSync("/tmp/tt-06k3-steer-lane-");
  const laneLog = path.join(laneDir, "steers.log");
  const lane = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 12_000 },
    extraWorkerEnv: { FAKE_PI_STEER_LOG: laneLog },
    workerScript: () => laneWorkerRound("a", 1),
    laneWorkerScriptFor: (l, round) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "sleep", ms: 10_500 },
        { kind: "call-sh", command: `printf 'lane ${l} r${round}\\n' > lane-${l}-r${round}.txt` },
        submitStep(),
      ],
    }),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a"),
  });
  try {
    await lane.conductor.start();
    await waitFor(() => lane.conductor.state.phase.phase === "DONE", 150_000, 50, lane.runDir);
    const steers = fs.readFileSync(laneLog, "utf8");
    assert.match(steers, /minute/, "the lane steer names the minutes left");
    assert.match(steers, /submit_phase/, "the lane steer says to submit");
  } finally {
    await teardown(lane);
    fs.rmSync(laneDir, { recursive: true, force: true });
  }
});

// --- A2/R4 ----------------------------------------------------------------

test("plan 06k3: a worker that misses its deadline has its work saved on a ref the next attempt starts from", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-06k3-ref-prompts-");
  const promptLog = path.join(promptDir, "prompts.log");
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 5_000 },
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorkerRound("a", 1),
    laneWorkerScriptFor: (lane, round) =>
      round === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: `printf 'saved ${lane}\\n' > lane-${lane}-r1.txt` },
              { kind: "sleep", ms: 8_000 },
            ],
          }
        : laneWorkerRound(lane, round),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    const saved = phase.unsubmittedWork ?? [];
    const runId = phase.runId;
    for (const lane of ["a", "b"]) {
      const entry = saved.find((w) => w.lane === lane);
      assert.ok(entry, `lane ${lane}'s unsubmitted work was saved`);
      assert.equal(entry!.ref, `refs/tt/${runId}/unsubmitted/1-${lane}`);
      // The ref holds the uncommitted file.
      const files = execFileSync("git", ["-C", setup.repo.dir, "ls-tree", "--name-only", entry!.ref], { encoding: "utf8" });
      assert.match(files, new RegExp(`lane-${lane}-r1\\.txt`), `the ref for lane ${lane} holds its round-1 file`);
    }
    // The next attempt's worktree (and candidate) contains them.
    assert.equal(phase.rounds?.length, 2, "round 2 ran");
    for (const lane of ["a", "b"]) {
      const candidate = phase.rounds![1].candidates.find((c) => c.lane === lane)!;
      const files = execFileSync("git", ["-C", setup.repo.dir, "ls-tree", "-r", "--name-only", candidate.sha as string], { encoding: "utf8" });
      assert.match(files, new RegExp(`lane-${lane}-r1\\.txt`), `lane ${lane}'s round-2 candidate still holds its saved work`);
    }
    // The round-2 prompt names the ref.
    const prompts = fs.readFileSync(promptLog, "utf8");
    for (const lane of ["a", "b"]) {
      assert.match(prompts, new RegExp(`refs/tt/${runId}/unsubmitted/1-${lane}`), `the round-2 prompt names lane ${lane}'s ref`);
    }
  } finally {
    await teardown(setup);
    fs.rmSync(promptDir, { recursive: true, force: true });
  }
});

test("plan 06k3: a stale ref lock does not lose the worker's unsubmitted work", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 6_000 },
    workerScript: () => laneWorkerRound("a", 1),
    laneWorkerScriptFor: (lane, round) =>
      round === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "call-sh", command: `printf 'saved ${lane}\\n' > lane-${lane}-r1.txt` },
              { kind: "sleep", ms: 9_000 },
            ],
          }
        : laneWorkerRound(lane, round),
    laneReviewerScriptFor: (seat, _candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "a"),
  });
  try {
    await setup.conductor.start();
    // Plant a stale lock on lane a's intended ref before the lane times out.
    const runId = setup.conductor.state.phase.runId;
    const lockDir = path.join(setup.repo.dir, ".git", "refs", "tt", runId, "unsubmitted");
    fs.mkdirSync(lockDir, { recursive: true });
    fs.writeFileSync(path.join(lockDir, "1-a.lock"), "stale");
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    const saved = (phase.unsubmittedWork ?? []).find((w) => w.lane === "a");
    assert.ok(saved, "lane a's work was saved despite the stale lock");
    // The ref itself resolves and holds the uncommitted file (A-13/T-29).
    const files = execFileSync("git", ["-C", setup.repo.dir, "ls-tree", "--name-only", saved!.ref], { encoding: "utf8" });
    assert.match(files, /lane-a-r1\.txt/, "the ref holds the uncommitted file");
  } finally {
    await teardown(setup);
  }
});

// --- A3/R5 ----------------------------------------------------------------

test("plan 06k3: a submission that leaves an open blocking finding unanswered is refused naming it", async () => {
  const setup = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    autoAnswerFindings: false,
    workerScriptForAttempt: (attempt, { state }) => {
      if (attempt === 1) {
        return { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'attempt 1\\n' > work.txt" }, submitStep()] };
      }
      // The finding id the repair must answer, read from live state.
      const finding = (state.phase.findings ?? []).find((f) => f.status === "open" && f.severity === "blocking")!;
      return {
        hello: defaultWorkerHello(),
        steps: [
          { kind: "sleep", ms: 1_000 },
          // 1st submit: no answer at all — refused, naming the finding.
          submitStep(),
          { kind: "call-sh", command: "printf 'fixed\\n' > work.txt" },
          { kind: "sleep", ms: 4_000 },
          // 2nd submit: fixed with the test that now covers it — accepted.
          submitStep({ findingAnswers: [{ findingId: finding.id, status: "fixed", test: "the work is done" }] }),
        ],
      };
    },
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open");
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: reviewArgs(reviewer, state.phase.contract.contractVersion, {
              findings:
                round === 1 && reviewer === "M"
                  ? [{ kind: "defect", severity: "blocking", evidence: "work.txt:1 the work is not done" }]
                  : [],
              findingStatements: round >= 2 ? open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })) : [],
            }),
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "finding_answer_refused"), 120_000, 50, setup.runDir);
    const refused = readEvents(setup.runDir).find((r) => r.kind === "finding_answer_refused")!;
    const findingId = (refused.event as { reason?: string }).reason?.match(/F-[A-Za-z0-9-]+/)?.[0];
    assert.ok(findingId, `the refusal names the missing finding id (${(refused.event as { reason?: string }).reason})`);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE", "the answered resubmission was accepted");
  } finally {
    await teardown(setup);
  }
});

// --- A3/R6 ----------------------------------------------------------------

test("plan 06k3: a finding's reproduction is re-run on the new candidate and its result reaches the reviewer prompt", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k3-repro-");
  const reviewerLog = path.join(dir, "reviewer-prompts.log");
  const setup = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    autoAnswerFindings: false,
    extraReviewerEnv: () => ({ FAKE_PI_PROMPT_LOG: reviewerLog }),
    workerScriptForAttempt: (attempt, { state }) => {
      if (attempt === 1) {
        return { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'attempt 1\\n' > work.txt" }, submitStep()] };
      }
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.severity === "blocking");
      return {
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-sh", command: "printf 'fixed\\n' > fixed.txt" },
          submitStep({
            findingAnswers: open.map((f) =>
              f.evidence.includes("never") ? { findingId: f.id, status: "disputed", reason: "the marker is intentionally absent" } : { findingId: f.id, status: "fixed", test: "fixed.txt exists" },
            ),
          }),
        ],
      };
    },
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open");
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: reviewArgs(reviewer, state.phase.contract.contractVersion, {
              findings:
                round === 1 && reviewer === "M"
                  ? [
                      // Exits 1 on the old candidate, 0 on the new one.
                      { kind: "defect", severity: "blocking", evidence: "the fixed marker is missing", reproduction: { command: "test -f fixed.txt" } },
                      // Still fails on the new candidate; prints its output.
                      { kind: "defect", severity: "blocking", evidence: "the never marker is never there", reproduction: { command: "echo STILL-BROKEN; test -f never.txt" } },
                    ]
                  : [],
              findingStatements: round >= 2 ? open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "checked" })) : [],
            }),
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    const prompts = fs.readFileSync(reviewerLog, "utf8");
    assert.match(prompts, /passes now/, "the finding whose reproduction now exits 0 shows 'passes now'");
    assert.match(prompts, /still fails \(exit 1\)/, "the finding whose reproduction still exits 1 shows 'still fails'");
    assert.match(prompts, /STILL-BROKEN/, "the still-failing row carries the re-run's last output lines");
    assert.match(prompts, /worker answer:/, "the table pairs each finding with the worker's answer");
    // The re-run is recorded per candidate.
    const reruns = readEvents(setup.runDir).filter((r) => r.kind === "finding_reproduction_rerun");
    assert.ok(reruns.length >= 2, "each finding's reproduction was re-run and logged");
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

// --- A4/R7 ----------------------------------------------------------------

test("plan 06k3: an unchanged-resubmission permission used before a conductor restart is still used after it", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06k3-restart-");
  const workerScript = path.join(dir, "worker-restart.json");
  const setup = await setupConductor({
    // A check that keeps failing: every frozen candidate goes straight to a
    // repair round, so a third attempt runs without needing a reviewer finding.
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["test -f fixed.txt"], boundaries: [], reserved: [], rounds: 10 },
    checks: ["test -f fixed.txt"],
    phaseChecks: ["test -f fixed.txt"],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) => {
      if (attempt === 1) {
        return { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'attempt 1\\n' > work.txt" }, submitStep()] };
      }
      if (attempt === 2) {
        // Two unchanged submissions: the first is refused; the owner's steer
        // lifts the second, consuming the permission.
        return { hello: defaultWorkerHello(), steps: [{ kind: "sleep", ms: 2_000 }, submitStep(), { kind: "sleep", ms: 8_000 }, submitStep()] };
      }
      // Attempt 3: sleep so the test can stop the conductor mid-attempt.
      return { hello: defaultWorkerHello(), steps: [{ kind: "sleep", ms: 15_000 }] };
    },
  });
  try {
    await setup.conductor.start();
    // Attempt 1's candidate fails its checks, so repair attempt 2 runs and its
    // unchanged submission is refused once.
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "resubmission_refused"), 120_000, 50, setup.runDir);
    // The owner asks for an unchanged resubmission; the next submit is taken,
    // consuming the permission (recorded as an event). A `note` (not a
    // `correction`) is one ask, so the permission is genuinely one-shot.
    fs.writeFileSync(
      path.join(runPaths(setup.runDir).inbox, "cmd-unchanged.json"),
      JSON.stringify({ type: "note", text: "resubmit unchanged", binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" } }),
    );
    await waitFor(
      () => (setup.conductor.state.phase.consumedUnchangedPermissions ?? []).length > 0,
      150_000,
      20,
      setup.runDir,
    );
    const usedKey = (setup.conductor.state.phase.consumedUnchangedPermissions ?? [])[0];
    assert.ok(usedKey, "the permission's use was recorded");
    // Wait until attempt 3 is running, then stop the conductor mid-attempt.
    await waitFor(
      () => setup.conductor.state.phase.phase === "IMPLEMENTING" && setup.conductor.state.phase.attempt.n >= 3,
      150_000,
      20,
      setup.runDir,
    );
    await setup.conductor.stop();

    // The permission's use is an EVENT, not process memory: a rebuild from the
    // log still knows it, so a restarted conductor cannot reopen it.
    const rebuilt = rebuildState(setup.runDir, setup.plan);
    assert.deepEqual(rebuilt.phase.consumedUnchangedPermissions, [usedKey], "the consumed permission survives a rebuild");
    const candidate = rebuilt.phase.candidate?.sha;
    assert.ok(candidate, "the accepted unchanged submission froze a candidate");
    const refusalsBefore = readEvents(setup.runDir).filter((r) => r.kind === "resubmission_refused").length;

    // Restart on the same run. Attempt 3 resubmits the candidate's own tree;
    // the permission is already spent, so the submission is refused before the
    // freeze, naming the earlier candidate.
    fs.writeFileSync(workerScript, JSON.stringify({ hello: defaultWorkerHello(), steps: [{ kind: "sleep", ms: 1_000 }, submitStep()] }));
    const restarted = new Conductor({
      runDir: setup.runDir,
      plan: setup.plan,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      stubReviews: false,
      deadlines: FAST,
      checkLockPath: path.join(setup.runRoot, "check.lock"),
      piEnvFor: (role) => (role === "worker" ? { FAKE_PI_SCRIPT: workerScript } : undefined),
    });
    await restarted.start();
    try {
      assert.deepEqual(
        restarted.state.phase.consumedUnchangedPermissions,
        [usedKey],
        "the restarted conductor still knows the permission was used",
      );
      await waitFor(
        () => readEvents(setup.runDir).filter((r) => r.kind === "resubmission_refused").length > refusalsBefore,
        150_000,
        50,
        setup.runDir,
      );
      const refusals = readEvents(setup.runDir).filter((r) => r.kind === "resubmission_refused");
      const last = refusals[refusals.length - 1].event as { candidateSha?: string; reason?: string };
      assert.equal(last.candidateSha, candidate, `the refusal after the restart names the earlier candidate (got ${last.candidateSha}, want ${candidate})`);
    } finally {
      await restarted.stop();
    }
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06k3: a lane restarted from its own losing candidate that resubmits it unchanged is refused naming that candidate", async () => {
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorkerRound("a", 1),
    laneWorkerScriptFor: (lane, round) => {
      if (round === 1) return laneWorkerRound(lane, round);
      // Round 2: lane a makes NO change (its worktree already holds its
      // round-1 candidate); lane b makes a change.
      return lane === "a"
        ? { hello: defaultWorkerHello(), steps: [submitStep()] }
        : laneWorkerRound(lane, round);
    },
    laneReviewerScriptFor: (seat, _candidate, state) => {
      const K = state.phase.contract.contractVersion;
      const round = liveRound(state);
      // Seat B's review and retry both end without submit_review in round 1,
      // so round 1 drops and repeats for free into round 2.
      if (seat === "B" && round === 1) return laneReview("B", K, true);
      return laneReview(seat as Reviewer, K);
    },
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, liveRound(state), "b"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "resubmission_refused"), 150_000, 50, setup.runDir);
    const refused = readEvents(setup.runDir).find((r) => r.kind === "resubmission_refused")!;
    const event = refused.event as { lane?: string; candidateSha?: string; reason?: string };
    assert.equal(event.lane, "a", "the refusal names lane a");
    // It names lane a's OWN round-1 candidate, not the round's base.
    const round1 = setup.conductor.state.phase.rounds!.find((r) => r.round === 1)!;
    const laneA = round1.candidates.find((c) => c.lane === "a")!.sha as string;
    assert.equal(event.candidateSha, laneA, "the refusal names the candidate lane a started from");
    assert.match(event.reason ?? "", new RegExp(laneA.slice(0, 7)), "the reason names that candidate");
  } finally {
    await teardown(setup);
  }
});
