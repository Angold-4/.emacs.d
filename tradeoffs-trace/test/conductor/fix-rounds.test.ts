// Plan 07c (design D4/D5): fix rounds, end to end. After a round whose pick
// the phase did not accept, the open findings triaged `fix` are grouped by
// domain and one fixer runs per domain, in parallel, from the same base; the
// conductor merges their commits with `git merge --no-ff` into one commit and
// the next round is a single-lane round on it. A conflict or a red fixer is
// skipped with a note. A plan without `#+TT_FIX_ROUNDS` is today's.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { buildContract, Conductor, rebuildState, runPaths, type RunPlanFile, type RunPlanPhase } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { readLog } from "../../src/effects/log.ts";
import { FIXER_DUTY } from "../../src/core/fix-rounds.ts";
import type { Domain } from "../../src/core/domains.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  FAKE_PI_PATH,
  readEvents,
  setupConductor,
  waitFor,
  writeScript,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 10_000,
  reviewMs: 20_000,
  panelMs: 10_000,
  reproductionMs: 20_000,
};

const DOMAINS: Domain[] = [
  { name: "booking", globs: ["booking/**"] },
  { name: "gates", globs: ["gates/**"] },
];

const BOOKING_DONE = "test -f booking-fixed.txt";
const GATES_DONE = "test -f gates-fixed.txt";

/** A two-lane, domain-mode phase with fix rounds on. */
function fixPhase(overrides: Partial<import("../../src/conductor.ts").RunPlanPhase> = {}) {
  return {
    id: "p1",
    goal: "fix the points",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers: 2,
    domains: DOMAINS,
    fixRounds: "domains",
    fixers: 4,
    fixChecks: "true",
    ...overrides,
  };
}

/** A lane worker: writes a lane marker and submits. */
function laneWorker(round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane} r${round}\\n' > lane-${lane}-r${round}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** One seat's two-turn review of one lane candidate. */
function laneReview(seat: Reviewer, contractVersion: unknown, findings: unknown[] = []): { hello: unknown; steps: FakePiStep[] } {
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
          findings,
        },
      },
    ],
  };
}

/** M raises the booking and gates points on round 1's candidates; every other
 * review is empty. Round 2 withdraws the open points (the fix candidate
 * repaired them). */
function reviewerWithPoints(bookingDone: string, gatesDone: string) {
  return (seat: Reviewer, _candidate: string, state: State): { hello: unknown; steps: FakePiStep[] } => {
    const K = state.phase.contract.contractVersion;
    const round = state.phase.rounds?.length ?? 1;
    if (round === 1 && seat === "M") {
      return laneReview("M", K, [
        {
          kind: "defect",
          severity: "blocking",
          evidence: "the booking path is wrong",
          domain: "booking",
          item: "R1",
          where: "booking/a.ts:1 fn",
          expected: "BOOKING_EXPECTED",
          doneWhen: bookingDone,
        },
        {
          kind: "defect",
          severity: "blocking",
          evidence: "the gates path is wrong",
          domain: "gates",
          item: "R2",
          where: "gates/b.ts:1 fn",
          expected: "GATES_EXPECTED",
          doneWhen: gatesDone,
        },
      ]);
    }
    return laneReview(seat as Reviewer, K);
  };
}

function pickVote(seat: Reviewer, round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { inspected: ["a", "b", "c", "d"], round, seat, lane, why: `${seat} picks ${lane}`, loserHad: { yes: false, anchors: [] } } }],
  };
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

function coreEvents(runDir: string): Array<Record<string, unknown>> {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => r.event as Record<string, unknown>);
}

test("TT_FIX_ROUNDS, TT_FIXERS and TT_FIX_CHECKS reach the phase contract through the Emacs parser", () => {
  const dir = fs.mkdtempSync("/tmp/tt-07c-parse-");
  const repoDir = fs.mkdtempSync("/tmp/tt-07c-parse-repo-");
  execFileSync("git", ["init", "-q", "-b", "main"], { cwd: repoDir });
  fs.writeFileSync(path.join(repoDir, "README.md"), "base\n");
  execFileSync("git", ["add", "-A"], { cwd: repoDir });
  execFileSync("git", ["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], { cwd: repoDir });
  const orgPath = path.join(dir, "PLAN.org");
  const body = (keywords: string[]) =>
    [
      "#+TITLE: fix rounds",
      `#+TT_REPO: ${repoDir}`,
      "#+TT_BRANCH: main",
      "#+TT_WORKERS: 2",
      "#+TT_DOMAINS: booking=booking/** gates=gates/**",
      ...keywords,
      "",
      "* Phase 1: p1",
      "  :PROPERTIES:",
      "  :ID: p1",
      "  :CHECKS: true",
      "  :END:",
      "  Goal: fix the points",
      "  Acceptance:",
      "  - it works",
    ].join("\n") + "\n";
  const emacsLoad = fileURLToPath(new URL("../../../test/tradeoffs-trace-test.el", import.meta.url));
  const coreDir = fileURLToPath(new URL("../../../core", import.meta.url));
  const testDir = fileURLToPath(new URL("../../../test", import.meta.url));
  const parse = (org: string) => {
    const p = path.join(dir, `P-${Math.random().toString(36).slice(2)}.org`);
    fs.writeFileSync(p, org);
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
        `(with-temp-buffer (insert-file-contents "${p}") (org-mode) (setq buffer-file-name "${p}") (princ (json-encode (plist-get (+tt-parse-plan) :plan))))`,
      ],
      { encoding: "utf8" },
    );
    return JSON.parse(out) as { fixRounds?: string; fixers?: number; fixChecks?: string; phases: Array<Record<string, unknown>> };
  };
  try {
    const on = parse(body(["#+TT_FIX_ROUNDS: domains", "#+TT_FIXERS: 3", "#+TT_FIX_CHECKS: cargo check"]));
    assert.equal(on.fixRounds, "domains", "the plan keyword reached the JSON");
    assert.equal(on.fixers, 3);
    assert.equal(on.fixChecks, "cargo check");
    const phase = on.phases[0] as RunPlanPhase;
    assert.equal(phase.fixRounds, "domains", "the phase inherited the plan's fix rounds");
    assert.equal(phase.fixers, 3);
    assert.equal(phase.fixChecks, "cargo check");
    const contract = buildContract(phase);
    assert.equal(contract.fixRounds, "domains", "the contract has fixRounds");
    assert.equal(contract.fixers, 3, "the contract has fixers");
    assert.equal(contract.fixChecks, "cargo check", "the contract has fixChecks");
    const off = parse(body([]));
    assert.equal(off.fixRounds, undefined, "without the keywords they are absent");
    assert.equal(off.fixers, undefined);
    assert.equal(off.fixChecks, undefined);
    const offContract = buildContract(off.phases[0] as RunPlanPhase);
    assert.equal("fixRounds" in offContract, false, "the contract has no fixRounds without the keyword");
    assert.equal("fixers" in offContract, false, "the contract has no fixers without the keyword");
    assert.equal("fixChecks" in offContract, false, "the contract has no fixChecks without the keyword");
  } finally {
    cleanupDir(dir);
    cleanupDir(repoDir);
  }
});

test("a fix round starts one fixer per domain from the same base and they overlap in time", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-07c-overlap-");
  const promptLog = path.join(promptDir, "prompts.txt");
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "sleep", ms: 1_500 },
        { kind: "call-sh", command: `printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    const record = fixed()!;
    assert.equal(record.tasks.length, 2, "one task per domain");
    assert.deepEqual(record.tasks.map((t) => t.domain).sort(), ["booking", "gates"]);
    const base = record.base;
    assert.equal(base, setup.conductor.state.phase.rounds![0].picked!.sha, "the fixers start from the pick");
    const { records } = readLog(runPaths(setup.runDir).events);
    const intents = records.filter((r) => r.kind === "intent" && (r.event as { fixer?: string }).fixer?.startsWith("fix-"));
    assert.equal(intents.length, 2, "two fixers started");
    for (const intent of intents) assert.equal((intent.event as { base?: string }).base, base, "both started at B");
    const checks = records.filter((r) => r.kind === "fixer_check_started").map((r) => ({ domain: (r.event as { domain: string }).domain, ts: Date.parse(r.ts) }));
    assert.equal(checks.length, 2, "both fixers' commits were checked");
    // They overlap: the second fixer's intent (its start) precedes the first
    // fixer's check (the end of its 1.5 s build).
    const firstIntent = Date.parse(intents[0].ts);
    const firstCheck = Math.min(...checks.map((c) => c.ts));
    assert.ok(firstIntent < firstCheck, "the first fixer's build had started before its check");
    assert.ok(Math.abs(Date.parse(intents[0].ts) - Date.parse(intents[1].ts)) < 1_000, "both fixers started together");
  } finally {
    await teardown(setup);
    cleanupDir(promptDir);
  }
});

test("a fixer's prompt has its own findings in full and only a one-line index of the other task", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-07c-prompt-");
  const promptLog = path.join(promptDir, "prompts.txt");
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    const record = fixed()!;
    const bookingTask = record.tasks.find((t) => t.domain === "booking")!;
    const gatesTask = record.tasks.find((t) => t.domain === "gates")!;
    await waitFor(() => {
      try {
        return fs.readFileSync(promptLog, "utf8").includes("You are fixer fix-booking");
      } catch {
        return false;
      }
    }, 60_000, 50, setup.runDir);
    const prompts = fs.readFileSync(promptLog, "utf8").split("\n=====\n");
    const booking = prompts.find((p) => p.includes("You are fixer fix-booking"));
    assert.ok(booking, "the booking fixer was prompted");
    assert.match(booking!, new RegExp(FIXER_DUTY.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")), "the duty paragraph is in the brief");
    for (const id of bookingTask.findingIds) {
      assert.ok(booking!.includes(id), `the booking prompt names its finding ${id}`);
    }
    assert.ok(booking!.includes("BOOKING_EXPECTED"), "its own expected text is in the brief");
    assert.ok(booking!.includes(BOOKING_DONE), "its own doneWhen is in the brief");
    assert.ok(booking!.includes(gatesTask.findingIds[0]), "the other task's finding id is in the index");
    assert.ok(!booking!.includes("GATES_EXPECTED"), "the other task's expected text is NOT in the brief");
  } finally {
    await teardown(setup);
    cleanupDir(promptDir);
  }
});

test("a fixer's submit_phase without item coverage or answers to other findings is accepted and its commit is merged", async () => {
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    // A bare submit_phase: no coverage, no findingAnswers. The consolidation
    // handle owes submit_phase only.
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    const record = fixed()!;
    assert.equal(record.merges.length, 2, "both bare submissions merged");
    assert.ok(record.tasks.every((t) => typeof t.sha === "string" && t.sha.length > 0), "each task recorded its commit");
    assert.ok(record.candidate, "the merged candidate was recorded");
  } finally {
    await teardown(setup);
  }
});

test("the fixers' commits are merged in task order into the next round's single candidate and ROUND_FIXED records it", async () => {
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    const record = fixed()!;
    const booking = record.tasks.find((t) => t.domain === "booking")!;
    const gates = record.tasks.find((t) => t.domain === "gates")!;
    assert.ok(booking.sha && gates.sha, "both tasks recorded their shas");
    assert.deepEqual(record.merges.map((m) => m.sha), [booking.sha, gates.sha], "both merges, in task order");
    const git = (...args: string[]) => execFileSync("git", args, { cwd: setup.repo.dir, encoding: "utf8" }).trim();
    assert.equal(git("show", `${record.candidate}:booking-fixed.txt`), "booking fixed", "the candidate carries booking's work");
    assert.equal(git("show", `${record.candidate}:gates-fixed.txt`), "gates fixed", "the candidate carries gates' work");
    // The next round is one lane on the candidate; its checks run on it and
    // the reviewers review it.
    await waitFor(() => (setup.conductor.state.phase.rounds?.length ?? 0) >= 2, 120_000, 50, setup.runDir);
    assert.equal(record.candidate, setup.conductor.state.phase.rounds![1].base, "the next round's base is the candidate");
    const second = () => setup.conductor.state.phase.rounds![1];
    assert.deepEqual(second().lanes, ["a"], "the next round has one lane");
    assert.equal(second().candidates[0].sha, record.candidate, "its candidate is the recorded merge");
    await waitFor(() => (second().candidates[0].reviews ?? []).length === 3, 120_000, 50, setup.runDir);
    const { records } = readLog(runPaths(setup.runDir).events);
    assert.ok(
      records.some((r) => r.kind === "lane_check_started" && (r.event as { candidateSha?: string }).candidateSha === record.candidate),
      "the round checks ran on the candidate",
    );
    assert.equal((second().candidates[0].reviews ?? []).length, 3, "the reviewers reviewed the candidate");
    const rebuilt = rebuildState(setup.runDir, setup.plan);
    assert.deepEqual(rebuilt.phase.rounds![0].fixed, record, "the record replays from the log");
  } finally {
    await teardown(setup);
  }
});

test("a fixer whose commit conflicts is skipped with a note and the other fixer's work is the candidate", async () => {
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    // Both fixers add the SAME file with different content: an add/add
    // conflict on the second merge.
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf '${domain}\\n' > shared.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    const record = fixed()!;
    assert.equal(record.merges.length, 1, "only the first merge landed");
    assert.equal(record.tasks.length, 2);
    const conflicted = record.tasks.find((t) => t.note?.includes("conflicted"));
    assert.ok(conflicted, `one task was skipped with a conflict note: ${JSON.stringify(record.tasks)}`);
    assert.ok(!record.merges.some((m) => m.sha === conflicted!.sha), "the conflicted fixer's commit is not a merge");
    // The candidate is the merge commit of the first fixer's work.
    const git = (...args: string[]) => execFileSync("git", args, { cwd: setup.repo.dir, encoding: "utf8" }).trim();
    assert.equal(git("show", `${record.candidate}:shared.txt`), "booking", "the candidate holds the first fixer's work");
    assert.equal(git("rev-parse", `${record.candidate}^2`), record.merges[0].sha, "the candidate merges the first fixer's commit");
    // Its findings stay open.
    const open = setup.conductor.state.phase.findings.filter((f) => f.status === "open" && f.domain === conflicted!.domain);
    assert.ok(open.length > 0, "the conflicted fixer's findings stay open");
  } finally {
    await teardown(setup);
  }
});

test("a fixer that stays red on TT_FIX_CHECKS is skipped and its findings stay open", async () => {
  const setup = await setupConductor({
    phase: fixPhase({ fixChecks: "test ! -f red-marker.txt" }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps:
        domain === "booking"
          ? [
              { kind: "call-sh", command: "printf 'red\\n' > red-marker.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ]
          : [
              { kind: "call-sh", command: "printf 'gates fixed\\n' > gates-fixed.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    const record = fixed()!;
    const booking = record.tasks.find((t) => t.domain === "booking")!;
    const gates = record.tasks.find((t) => t.domain === "gates")!;
    assert.ok(booking.note, `the red fixer is skipped with a note: ${JSON.stringify(record.tasks)}`);
    assert.equal(booking.sha, undefined, "a red fixer records no merged commit");
    assert.equal(record.merges.length, 1, "only the green fixer merged");
    assert.equal(record.merges[0].sha, gates.sha);
    const git = (...args: string[]) => execFileSync("git", args, { cwd: setup.repo.dir, encoding: "utf8" }).trim();
    assert.equal(git("show", `${record.candidate}:gates-fixed.txt`), "gates fixed", "the candidate holds the green fixer's work");
    assert.equal(git("rev-parse", `${record.candidate}^2`), gates.sha, "the candidate merges the green fixer's commit");
    const open = setup.conductor.state.phase.findings.filter((f) => f.status === "open" && f.domain === "booking");
    assert.ok(open.length > 0, "the red fixer's findings stay open");
  } finally {
    await teardown(setup);
  }
});

test("a phase with domain findings reaches acceptance through a fix round", async () => {
  const setup = await setupConductor({
    phase: fixPhase({ finalChecks: ["true"] }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 240_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.ok(phase.rounds?.[0]?.fixed?.candidate, "the fix round produced a candidate");
    const events = coreEvents(setup.runDir);
    const facts = events.filter((e) => e.type === "FINDING_FACT");
    assert.equal(facts.length, 2, `two facts on the merged candidate: ${JSON.stringify(facts)}`);
    assert.ok(facts.every((f) => f.exitCode === 0 && f.timedOut === false), "both facts pass");
    assert.ok(facts.every((f) => f.candidateSha === phase.rounds![0].fixed!.candidate), "the facts name the merged candidate");
    assert.equal(events.filter((e) => e.type === "FINDING_CONFIRMED_REPAIRED").length, 2, "both points are repaired");
    assert.ok(events.some((e) => e.type === "FINAL_CHECKS_PASSED"), "the final checks ran");
    assert.equal(phase.phase, "DONE");
  } finally {
    await teardown(setup);
  }
});

test("a fix round resumed after ROUND_FIXED starts the next round from the recorded candidate", async () => {
  const counter = `/tmp/tt-07c-fixer-counter-${process.pid}`;
  fs.rmSync(counter, { force: true });
  const promptDir = fs.mkdtempSync("/tmp/tt-07c-resume-");
  const promptLog = path.join(promptDir, "prompts.txt");
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `echo ${domain} >> ${counter}` },
        { kind: "call-sh", command: `printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  let resumed: Conductor | undefined;
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.rounds?.[0]?.fixed !== undefined, 180_000, 50, setup.runDir);
    const candidate = setup.conductor.state.phase.rounds![0].fixed!.candidate!;
    await setup.conductor.stop();
    const runs = () => fs.readFileSync(counter, "utf8").split("\n").filter((l) => l.length > 0);
    const before = runs().length;
    assert.equal(before, 2, "two fixers ran before the stop");

    resumed = resumeConductor(setup);
    await resumed.start();
    await waitFor(() => (resumed!.state.phase.rounds?.length ?? 0) >= 2, 180_000, 50, setup.runDir);
    assert.equal(runs().length, before, "no fixer started again after the resume");
    const second = resumed!.state.phase.rounds![1];
    assert.equal(second.base, candidate, "the next round's base is the recorded candidate");
    assert.equal(second.candidates[0].sha, candidate, "the next round's candidate is the recorded one");
  } finally {
    if (resumed) await resumed.stop();
    await teardown(setup);
    fs.rmSync(counter, { force: true });
    cleanupDir(promptDir);
  }
});

test("without TT_FIX_ROUNDS a round after a repair is today's two-lane round", async () => {
  const setup = await setupConductor({
    // Domain mode on, fix rounds OFF: the phase is today's.
    phase: fixPhase({ fixRounds: undefined, fixers: undefined, fixChecks: undefined }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => (setup.conductor.state.phase.rounds?.length ?? 0) >= 2, 180_000, 50, setup.runDir);
    const rounds = setup.conductor.state.phase.rounds!;
    assert.equal(rounds[0].fixed, undefined, "round 1 is never a fix round");
    assert.deepEqual(rounds[1].lanes, ["a", "b"], "the next round is today's two-lane round");
    const { records } = readLog(runPaths(setup.runDir).events);
    assert.equal(records.filter((r) => r.kind === "intent" && (r.event as { fixer?: string }).fixer?.startsWith("fix-")).length, 0, "no fixer started");
    assert.ok(!coreEvents(setup.runDir).some((e) => e.type === "ROUND_FIXED"), "no ROUND_FIXED was recorded");
  } finally {
    await teardown(setup);
  }
});

/** A second conductor on the stopped run, as `tt resume` starts one: its lane
 * reviews answer at once and every seat picks lane a. */
function resumeConductor(setup: TestConductorSetup): Conductor {
  const scripts = new Map<string, string>();
  const scriptFor = (name: string, script: { hello?: unknown; steps: FakePiStep[] }) => {
    if (!scripts.has(name)) scripts.set(name, writeScript(setup.scriptsDir, name, script));
    return scripts.get(name)!;
  };
  const K = setup.conductor.state.phase.contract.contractVersion;
  return new Conductor({
    runDir: setup.runDir,
    plan: setup.plan as RunPlanFile,
    piCommand: process.execPath,
    piArgsPrefix: [FAKE_PI_PATH],
    stubReviews: true,
    deadlines: FAST,
    checkLockPath: path.join(setup.runRoot, "check.lock"),
    piEnvFor: (role, agentId) => {
      const laneMatch = agentId.match(/^lane-review-(\d+)-([a-z])-([A-Za-z0-9_]+)-/);
      if (role === "reviewer" && laneMatch) {
        return { FAKE_PI_SCRIPT: scriptFor(`resume-lane-review-${laneMatch[3]}`, laneReview(laneMatch[3] as Reviewer, K)) };
      }
      const pick = agentId.match(/^(?:pick|revote)-(\d+)-([A-Za-z0-9_]+)-/);
      if (role === "picker" && pick) {
        return { FAKE_PI_SCRIPT: scriptFor(`resume-pick-${pick[2]}`, pickVote(pick[2] as Reviewer, Number(pick[1]), "a")) };
      }
      return undefined;
    },
  });
}
