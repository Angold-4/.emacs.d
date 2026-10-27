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
import type { PlanModels } from "../../src/core/roles.ts";
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

/** Plan 07c (C3): a decision a fixer reports instead of deciding it. */
function fixerDisclosure(domain: string) {
  return {
    choice: `FIXER_DISCLOSURE_${domain}`,
    whyItMatters: "the fixer met a decision outside its findings and reports it rather than deciding it",
    alternatives: [{ option: "decide it silently", consequence: "the owner never sees the choice" }],
    recommendation: { choice: "report it", reason: "a fixer decides no trade-off" },
    classProposal: "detail",
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
    // Plan 07c (C3): anything beyond its findings is reported as a disclosure.
    assert.ok(!booking!.includes("decisions []"), "the brief never asks the fixer for empty decisions");
  } finally {
    await teardown(setup);
    cleanupDir(promptDir);
  }
});

test("a fixer's submit_phase without item coverage or answers to other findings is accepted", async () => {
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

test("a fixer's out-of-task decision disclosure reaches the phase record", async () => {
  const disclosure = fixerDisclosure;
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
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [disclosure(domain)], assumptions: [], deviations: [`FIXER_DEVIATION_${domain}`] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 240_000, 50, setup.runDir);
    const fixed = setup.conductor.state.phase.rounds?.[0]?.fixed;
    assert.ok(fixed?.candidate, "the fix round merged a candidate");
    // ROUND_FIXED keeps every fixer's decisions, in task order (booking, gates).
    assert.deepEqual(
      (fixed!.disclosures ?? []).map((d) => d.choice),
      ["FIXER_DISCLOSURE_booking", "FIXER_DISCLOSURE_gates"],
      "the fixers' decisions are recorded on ROUND_FIXED in task order",
    );
    // The merged candidate's hand-off records them as the phase's own
    // decisions, bound to that candidate.
    const decisions = setup.conductor.state.phase.decisions ?? [];
    const choices = decisions.map((d) => d.choice);
    for (const domain of ["booking", "gates"]) {
      const decision = decisions.find((d) => d.choice === `FIXER_DISCLOSURE_${domain}`);
      assert.ok(decision, `the ${domain} fixer's disclosure reached the phase record: ${JSON.stringify(choices)}`);
      assert.equal(decision!.source, "worker", `the ${domain} fixer's disclosure is the phase's own (worker) decision`);
      assert.equal(decision!.boundCandidateSha, fixed!.candidate, `the ${domain} fixer's disclosure is bound to the merged candidate`);
    }
    assert.ok(
      choices.indexOf("FIXER_DISCLOSURE_booking") < choices.indexOf("FIXER_DISCLOSURE_gates"),
      `the phase records the fixers' decisions in task order: ${JSON.stringify(choices)}`,
    );
    const added = fixed!.added ?? [];
    assert.ok(added.includes("FIXER_DEVIATION_booking") && added.includes("FIXER_DEVIATION_gates"), `the fixers' notes are kept: ${JSON.stringify(added)}`);
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

test("a fixer whose commit conflicts is skipped with a note, the other fixer's work is the candidate, and only its disclosures reach the phase record", async () => {
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
    // Each fixer also reports a decision and a note (plan 07c C3).
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf '${domain}\\n' > shared.txt` },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: { decisions: [fixerDisclosure(domain)], assumptions: [], deviations: [`FIXER_DEVIATION_${domain}`] },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    const record = fixed()!;
    // Fix packet Z: only the merged fixer speaks for the candidate. The
    // skipped (conflicting) fixer's decision and note describe code the
    // candidate does not contain.
    assert.deepEqual((record.disclosures ?? []).map((d) => d.choice), ["FIXER_DISCLOSURE_booking"], "only the merged fixer's decision is on ROUND_FIXED");
    assert.deepEqual(record.added ?? [], ["FIXER_DEVIATION_booking"], "only the merged fixer's note is on ROUND_FIXED");
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
    // The merged candidate's hand-off records the merged fixer's decision and
    // never the skipped one's.
    await waitFor(() => setup.conductor.state.phase.candidate?.sha === record.candidate, 120_000, 50, setup.runDir);
    const choices = (setup.conductor.state.phase.decisions ?? []).map((d) => d.choice);
    assert.ok(choices.includes("FIXER_DISCLOSURE_booking"), `the merged fixer's decision reached the phase record: ${JSON.stringify(choices)}`);
    assert.ok(!choices.includes("FIXER_DISCLOSURE_gates"), `the skipped fixer's decision did not reach the phase record: ${JSON.stringify(choices)}`);
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
  // Plan 07c (A4): the next round's check of the merged candidate is held
  // until `release` exists, so the stop lands inside that unfinished round.
  // The candidate touches a wide path, so the resumed check must diff against
  // the recorded fix base B and run the final tier.
  const release = `/tmp/tt-07c-resume-release-${process.pid}`;
  fs.rmSync(release, { force: true });
  const finalMarker = `/tmp/tt-07c-resume-final-${process.pid}`;
  fs.rmSync(finalMarker, { force: true });
  const hold = `if [ -f booking-fixed.txt ] && [ ! -f ${release} ]; then sleep 30; fi; true`;
  const finalCheck = `echo final >> ${finalMarker}`;
  const promptDir = fs.mkdtempSync("/tmp/tt-07c-resume-");
  const promptLog = path.join(promptDir, "prompts.txt");
  const setup = await setupConductor({
    phase: fixPhase({ checks: [hold], finalChecks: [finalCheck], widePaths: ["booking-fixed.txt"] }),
    checks: ["true"],
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
  const roundTwoCheckStarts = () =>
    readLog(runPaths(setup.runDir).events).records.filter(
      (r) => r.kind === "lane_check_started" && (r.event as { round?: number }).round === 2,
    );
  let resumed: Conductor | undefined;
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.rounds?.[0]?.fixed !== undefined, 180_000, 50, setup.runDir);
    const fixed = setup.conductor.state.phase.rounds![0].fixed!;
    const candidate = fixed.candidate!;
    assert.notEqual(fixed.base, candidate, "the fix base B is not the merged candidate");
    // The next round has started and is checking the merged candidate (held).
    await waitFor(() => roundTwoCheckStarts().length > 0, 60_000, 50, setup.runDir);
    await setup.conductor.stop();
    const runs = () => fs.readFileSync(counter, "utf8").split("\n").filter((l) => l.length > 0);
    const before = runs().length;
    assert.equal(before, 2, "two fixers ran before the stop");
    const stoppedRound = setup.conductor.state.phase.rounds![1];
    assert.equal(stoppedRound.candidates[0]?.sha, candidate, "the stopped round's candidate is the merged one");
    assert.equal(stoppedRound.candidates[0]?.ok, undefined, "the stop left the next round unfinished (its candidate unchecked)");
    const startsBefore = roundTwoCheckStarts().length;

    fs.writeFileSync(release, "go\n");
    resumed = resumeConductor(setup);
    await resumed.start();
    await waitFor(() => resumed!.state.phase.rounds?.[1]?.candidates[0]?.ok !== undefined, 180_000, 50, setup.runDir);
    assert.equal(runs().length, before, "no fixer started again after the resume");
    const second = resumed!.state.phase.rounds![1];
    assert.equal(second.base, candidate, "the next round's base is the recorded candidate");
    assert.equal(second.candidates[0].sha, candidate, "the next round's candidate is the recorded one");
    // Plan 07c (A4): the resumed (unfinished) round's check of the synthetic
    // lane uses the recorded fix base B, exactly as the fresh check did.
    const starts = roundTwoCheckStarts();
    assert.ok(starts.length > startsBefore, "the resumed round checked the merged candidate again");
    for (const r of starts) {
      const e = r.event as { lane?: string; candidateSha?: string; base?: string };
      assert.equal(e.candidateSha, candidate, "round 2 checks the merged candidate");
      assert.equal(e.base, fixed.base, "the check of the merged candidate diffs against the fix base B");
    }
    // Its wide-path diff against B selects the final tier in the resumed check.
    const record = JSON.parse(fs.readFileSync(path.join(runPaths(setup.runDir).checks, candidate, "record.json"), "utf8")) as {
      commands: Array<{ command: string }>;
    };
    assert.ok(
      record.commands.some((c) => c.command === finalCheck),
      `the resumed check ran the final tier: ${JSON.stringify(record.commands.map((c) => c.command))}`,
    );
  } finally {
    if (resumed) await resumed.stop();
    await teardown(setup);
    fs.rmSync(counter, { force: true });
    fs.rmSync(release, { force: true });
    fs.rmSync(finalMarker, { force: true });
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

test("a fix round's red merge that its lane fix cannot repair is never replayed: the next round is today's two-lane round", async () => {
  // Packet X (B1): round 1 picks; the fixers' merge C carries booking-fixed.txt,
  // which the round checks refuse, so round 2 (one lane on C) is red; its
  // lane fix stays red and round 2 picks nothing. Round 3 must be today's
  // two-lane round from round 1's pick, never another synthetic round on C.
  const promptDir = fs.mkdtempSync("/tmp/tt-07c-replay-");
  const promptLog = path.join(promptDir, "prompts.txt");
  const setup = await setupConductor({
    phase: fixPhase({ checks: ["test ! -f booking-fixed.txt"] }),
    checks: ["true"],
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
    await waitFor(() => (setup.conductor.state.phase.rounds?.length ?? 0) >= 3, 240_000, 50, setup.runDir);
    const rounds = setup.conductor.state.phase.rounds!;
    const merged = rounds[0].fixed!.candidate!;
    assert.ok(merged, "round 1's fixers merged a candidate");
    assert.deepEqual(rounds[1].lanes, ["a"], "round 2 is the single lane on the merge");
    assert.equal(rounds[1].base, merged);
    assert.equal(rounds[1].picked, undefined, "round 2 picked nothing (its merge stayed red)");
    // The red merge's fix started a fresh session with a prompt naming the
    // fixers' merge, not lane a's own work.
    const { records } = readLog(runPaths(setup.runDir).events);
    const fixSessions = records.filter((r) => r.kind === "lane_session" && (r.event as { round?: number }).round === 2);
    assert.ok(fixSessions.length >= 1, "round 2's lane fix ran");
    assert.equal((fixSessions[0].event as { continueSession?: boolean }).continueSession, false, "the merge's fix starts a fresh session");
    const prompts = fs.readFileSync(promptLog, "utf8").split("\n=====\n");
    assert.ok(
      prompts.some((p) => p.includes(`Commit ${merged.slice(0, 7)} is the fixers' merge`)),
      "the merge's fix prompt names it as the fixers' merge",
    );
    // Round 3 is today's round: two lanes from round 1's pick, never C again.
    assert.deepEqual(rounds[2].lanes, ["a", "b"], "round 3 is today's two-lane round");
    assert.equal(rounds[2].base, rounds[0].picked!.sha, "round 3 starts from today's base (round 1's pick)");
    assert.notEqual(rounds[2].base, merged, "round 3 never replays the merge");
  } finally {
    await teardown(setup);
    cleanupDir(promptDir);
  }
});

test("a fix round removes its merge and fixer worktrees once ROUND_FIXED is recorded", async () => {
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
    // Round 2 has started, so the fix step (and its clean-up) is done.
    await waitFor(() => (setup.conductor.state.phase.rounds?.length ?? 0) >= 2, 180_000, 50, setup.runDir);
    const record = setup.conductor.state.phase.rounds![0].fixed!;
    assert.ok(record.candidate, "the fix round merged a candidate");
    const worktrees = path.join(setup.runDir, "worktrees");
    assert.equal(fs.existsSync(path.join(worktrees, "fix-merge-1")), false, "the merge worktree is gone");
    assert.equal(fs.existsSync(path.join(worktrees, "fix-booking")), false, "the booking fixer's worktree is gone");
    assert.equal(fs.existsSync(path.join(worktrees, "fix-gates")), false, "the gates fixer's worktree is gone");
    // The commits stay.
    const git = (...args: string[]) => execFileSync("git", args, { cwd: setup.repo.dir, encoding: "utf8" }).trim();
    for (const t of record.tasks) assert.equal(git("cat-file", "-t", t.sha!), "commit", `the ${t.domain} fixer's commit stays`);
    assert.equal(git("cat-file", "-t", record.candidate!), "commit", "the merged candidate stays");
  } finally {
    await teardown(setup);
  }
});

test("a red fixer's fix prompt carries its fix check's output", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-07c-redout-");
  const promptLog = path.join(promptDir, "prompts.txt");
  const setup = await setupConductor({
    phase: fixPhase({ fixChecks: "if [ -f red-marker.txt ]; then echo 'error: RED_MARKER_PRESENT'; exit 1; fi" }),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    // The booking fixer's build is red; its fix removes the marker.
    fixerScriptFor: (domain, _round, n) => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-sh",
          command: domain !== "booking" ? `printf '${domain} fixed\\n' > ${domain}-fixed.txt` : n === 1 ? "printf 'booking fixed\\n' > booking-fixed.txt && printf 'red\\n' > red-marker.txt" : "rm -f red-marker.txt",
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.rounds?.[0]?.fixed !== undefined, 180_000, 50, setup.runDir);
    const { records } = readLog(runPaths(setup.runDir).events);
    const red = records.find(
      (r) => r.kind === "fixer_check_finished" && (r.event as { domain?: string; passed?: boolean }).domain === "booking" && (r.event as { passed?: boolean }).passed === false,
    );
    assert.ok(red, "the booking fixer's first commit was red");
    const redSha = (red!.event as { sha: string }).sha;
    const logs = fs.readdirSync(path.join(runPaths(setup.runDir).checks, redSha)).filter((f) => f.endsWith(".log"));
    assert.ok(logs.length > 0, "the fixer check's output is kept under checks/<sha>/");
    assert.ok(
      logs.some((f) => fs.readFileSync(path.join(runPaths(setup.runDir).checks, redSha, f), "utf8").includes("error: RED_MARKER_PRESENT")),
      "the kept output is the check's own",
    );
    const prompts = fs.readFileSync(promptLog, "utf8").split("\n=====\n");
    const fixPrompt = prompts.find((p) => p.includes(`Your fixer commit ${redSha.slice(0, 7)} FAILED the fix check`));
    assert.ok(fixPrompt, "the red fixer was asked to fix it");
    assert.ok(fixPrompt!.includes("error: RED_MARKER_PRESENT"), "the fix prompt carries the failure output");
    const booking = setup.conductor.state.phase.rounds![0].fixed!.tasks.find((t) => t.domain === "booking")!;
    assert.ok(booking.sha && booking.sha !== redSha, "the fixed commit is the one merged");
  } finally {
    await teardown(setup);
    cleanupDir(promptDir);
  }
});

test("a fixers' merge whose tree is the base's records no candidate and the next round is today's", async () => {
  // Packet X: the gates fixer changes nothing; the booking fixer's red commit
  // adds a marker and its fix removes it again, so its green commit — and
  // the merge — has B's own tree.
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
    fixerScriptFor: (domain, _round, n) => ({
      hello: defaultWorkerHello(),
      steps: [
        ...(domain === "booking" ? [{ kind: "call-sh" as const, command: n === 1 ? "printf 'red\\n' > red-marker.txt" : "rm -f red-marker.txt" }] : []),
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.rounds?.[0]?.fixed !== undefined, 180_000, 50, setup.runDir);
    const record = setup.conductor.state.phase.rounds![0].fixed!;
    assert.equal(record.merges.length, 1, `the booking fixer's green commit merged: ${JSON.stringify(record)}`);
    assert.equal(record.candidate, undefined, "a merge with the base's tree is no candidate");
    const { records } = readLog(runPaths(setup.runDir).events);
    assert.ok(records.some((r) => r.kind === "fix_merge_unchanged"), "the unchanged merge is logged");
    await waitFor(() => (setup.conductor.state.phase.rounds?.length ?? 0) >= 2, 120_000, 50, setup.runDir);
    assert.deepEqual(setup.conductor.state.phase.rounds![1].lanes, ["a", "b"], "the next round is today's two-lane round");
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

// ---------------------------------------------------------------------------
// Fix packet Z: a voted-down or vetoed decision is fixer work
// ---------------------------------------------------------------------------

const VETO_REASON = "VETO_REASON: the booking cache hides a stale price";
const ALT_OPTION = "ALT_OPTION: read the price through without the cache";

/** A round-1 lane worker that also discloses one decision of `cls`; later
 * rounds disclose nothing. */
function laneWorkerWithDecision(round: number, lane: string, cls: "delegated" | "reserved"): { hello: unknown; steps: FakePiStep[] } {
  const decisions =
    round === 1
      ? [
          {
            choice: "cache the booking price per batch",
            whyItMatters: "fewer reads per batch",
            alternatives: [{ option: ALT_OPTION, consequence: "one more read per order" }],
            recommendation: { choice: "cache it", reason: "fewer reads" },
            classProposal: cls,
          },
        ]
      : [];
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane} r${round}\\n' > lane-${lane}-r${round}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions, assumptions: [], deviations: [] } },
    ],
  };
}

/** Round 1: M rejects the lane's decision (a veto, citing a booking path), A
 * and B approve it; no seat raises a finding. Later rounds: empty reviews. */
function reviewerVetoing(seat: Reviewer, _candidate: string, state: State): { hello: unknown; steps: FakePiStep[] } {
  const K = state.phase.contract.contractVersion;
  const round = state.phase.rounds?.length ?? 1;
  const review = laneReview(seat, K);
  if (round !== 1) return review;
  const submit = review.steps[2] as { args: Record<string, unknown> };
  submit.args.ballots = [
    seat === "M"
      ? { decisionId: "D-p1-$TT_CANDIDATE_SHA8-1", vote: "reject", rationale: VETO_REASON, evidence: ["booking/cache.ts:12 price"] }
      : { decisionId: "D-p1-$TT_CANDIDATE_SHA8-1", vote: "approve", rationale: `${seat} approves`, evidence: ["README.md:1"] },
  ];
  return review;
}

function fixerIntents(runDir: string) {
  return readLog(runPaths(runDir).events).records.filter((r) => r.kind === "intent" && (r.event as { fixer?: string }).fixer?.startsWith("fix-"));
}

test("a round blocked only by a vetoed decision is followed by a fix round with one fixer for its domain, briefed with the veto and the alternative", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-07z-veto-");
  const promptLog = path.join(promptDir, "prompts.txt");
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorkerWithDecision(1, "a", "delegated"),
    laneWorkerScriptFor: (lane, round) => laneWorkerWithDecision(round, lane, "delegated"),
    laneReviewerScriptFor: reviewerVetoing,
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
    const picked = setup.conductor.state.phase.rounds![0].picked!.sha;
    const decisionId = `D-p1-${picked.slice(0, 8)}-1`;
    // Round 1 had no finding: the vetoed decision alone blocked it.
    assert.equal(setup.conductor.state.phase.findings.filter((f) => f.boundCandidateSha === picked).length, 0, "round 1 raised no finding");
    assert.deepEqual(
      record.tasks.map((t) => ({ domain: t.domain, findingIds: t.findingIds })),
      [{ domain: "booking", findingIds: [decisionId] }],
      "one task: the vetoed decision, in the domain of M's dissent evidence",
    );
    assert.equal(fixerIntents(setup.runDir).length, 1, "one fixer started");
    await waitFor(() => {
      try {
        return fs.readFileSync(promptLog, "utf8").includes("You are fixer fix-booking");
      } catch {
        return false;
      }
    }, 60_000, 50, setup.runDir);
    const brief = fs.readFileSync(promptLog, "utf8").split("\n=====\n").find((p) => p.includes("You are fixer fix-booking"))!;
    assert.ok(brief.includes(decisionId), "the brief names the decision");
    assert.ok(brief.includes(VETO_REASON), "the brief carries M's veto reason");
    assert.ok(brief.includes(ALT_OPTION), "the brief carries the seats' alternative");
    assert.match(brief, /vetoed by M/);
    // The next round is the fix round's single lane on the merged candidate.
    await waitFor(() => (setup.conductor.state.phase.rounds?.length ?? 0) >= 2, 120_000, 50, setup.runDir);
    assert.deepEqual(setup.conductor.state.phase.rounds![1].lanes, ["a"], "the next round is one lane");
    assert.equal(setup.conductor.state.phase.rounds![1].base, record.candidate);
  } finally {
    await teardown(setup);
    cleanupDir(promptDir);
  }
});

test("a vetoed reserved (owner) decision is never a fixer task", async () => {
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorkerWithDecision(1, "a", "reserved"),
    laneWorkerScriptFor: (lane, round) => laneWorkerWithDecision(round, lane, "reserved"),
    laneReviewerScriptFor: reviewerVetoing,
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => (setup.conductor.state.phase.rounds?.length ?? 0) >= 2, 180_000, 50, setup.runDir);
    const rounds = setup.conductor.state.phase.rounds!;
    const picked = rounds[0].picked!.sha;
    const decision = setup.conductor.state.phase.decisions.find((d) => d.id === `D-p1-${picked.slice(0, 8)}-1`);
    assert.equal(decision?.class, "reserved", "the vetoed decision is reserved");
    assert.equal(rounds[0].fixed, undefined, "no fix round for a reserved decision");
    assert.deepEqual(rounds[1].lanes, ["a", "b"], "the next round is today's two-lane round");
    assert.equal(fixerIntents(setup.runDir).length, 0, "no fixer started");
  } finally {
    await teardown(setup);
  }
});

test("an unclassified domain-mode point with a doneWhen becomes a fixer task, not an owner request", async () => {
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
    // The evaluator confirms each open point but gives NO impact class.
    evaluatorScriptFor: (messageType, state) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations: [],
            itemChecks:
              messageType === "finding"
                ? state.phase.findings.filter((f) => f.status === "open").map((f) => ({ id: f.id, verdict: "confirmed", evidence: "README.md:1 evaluator: re-checked" }))
                : [],
          },
        },
      ],
    }),
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
    const phase = setup.conductor.state.phase;
    const points = phase.findings.filter((f) => f.domain === "booking" || f.domain === "gates");
    assert.equal(points.length, 2, "M raised two points");
    for (const point of points) {
      const check = (phase.itemChecks ?? []).find((c) => c.itemId === point.id);
      assert.equal(check?.impact, undefined, `the evaluator gave ${point.id} no impact`);
      const triage = (phase.triage ?? []).find((r) => r.itemId === point.id);
      assert.equal(triage?.disposition?.kind, "fix", `${point.id} is triaged fix: ${JSON.stringify(triage)}`);
      assert.equal(triage?.impact, "contract", `${point.id} has impact contract`);
      assert.ok(!phase.ownerRequests.some((r) => r.linkedFindingId === point.id), `${point.id} opened no owner request`);
    }
    assert.deepEqual(
      fixed()!.tasks.map((t) => t.domain),
      ["booking", "gates"],
      "each unclassified point is a fixer task in its domain",
    );
  } finally {
    await teardown(setup);
  }
});

// exex 01c1a (2026-10-27, run f4d01766): the fixers ran on the leader's model
// (consolidator mode). A fixer is a mapper: it launches on the worker model.
test("fixers launch on the worker model, not the consolidator's", async () => {
  const argvDir = fs.mkdtempSync("/tmp/tt-fix-model-");
  const models: PlanModels = { worker: { provider: "p-w", model: "m-w" }, reviewer: { provider: "p-r", model: "m-r" } };
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    models,
    extraEnv: { FAKE_PI_ARGV_LOG: argvDir },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(BOOKING_DONE, GATES_DONE)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    evaluatorScriptFor: (messageType, state) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations: [],
            itemChecks:
              messageType === "finding"
                ? state.phase.findings.filter((f) => f.status === "open").map((f) => ({ id: f.id, verdict: "confirmed", evidence: "README.md:1 evaluator: re-checked" }))
                : [],
          },
        },
      ],
    }),
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
    assert.deepEqual(fixed()!.tasks.map((t) => t.domain), ["booking", "gates"], "two fixers ran");
    const launches = fs
      .readFileSync(path.join(argvDir, "worker.jsonl"), "utf8")
      .split("\n")
      .filter(Boolean)
      .map((l) => (JSON.parse(l) as { argv: string[] }).argv);
    assert.ok(launches.length >= 4, `two lane workers and two fixers launched as workers: ${launches.length}`);
    for (const argv of launches) {
      const i = argv.indexOf("--model");
      assert.equal(i >= 0 ? argv[i + 1] : undefined, "m-w", `every mapper (lane worker or fixer) runs on the worker model: ${argv.join(" ")}`);
    }
  } finally {
    await teardown(setup);
    fs.rmSync(argvDir, { recursive: true, force: true });
  }
});
