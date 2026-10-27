// Lane a's integration coverage for R1-R5, R7, R10 and R11, kept alongside
// lane b's bounds.test.ts so neither lane's assertions are lost.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { DEFAULT_DEADLINES } from "../../src/conductor.ts";
import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor, type FakePiStep, type TestConductorSetup } from "./harness.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Domain } from "../../src/core/domains.ts";

const EMACS_LOAD = fileURLToPath(new URL("../../../test/tradeoffs-trace-test.el", import.meta.url));
const CORE_DIR = fileURLToPath(new URL("../../../core", import.meta.url));
const TEST_DIR = fileURLToPath(new URL("../../../test", import.meta.url));

function makeRepo(): string {
  const dir = fs.mkdtempSync("/tmp/tt-07e-repo-");
  execFileSync("git", ["init", "-q", "-b", "main"], { cwd: dir });
  fs.writeFileSync(path.join(dir, "README.md"), "base\n");
  execFileSync("git", ["add", "-A"], { cwd: dir });
  execFileSync("git", ["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], { cwd: dir });
  return dir;
}

function parsePlan(org: string): { deadlines?: Record<string, number>; checkWindow?: string } {
  const dir = fs.mkdtempSync("/tmp/tt-07e-parse-");
  try {
    const p = path.join(dir, "PLAN.org");
    fs.writeFileSync(p, org);
    const out = execFileSync(
      "emacs",
      [
        "--batch",
        "-Q",
        "-L",
        CORE_DIR,
        "-L",
        TEST_DIR,
        "-l",
        EMACS_LOAD,
        "--eval",
        `(with-temp-buffer (insert-file-contents "${p}") (org-mode) (setq buffer-file-name "${p}") (princ (json-encode (plist-get (+tt-parse-plan) :plan))))`,
      ],
      { encoding: "utf8" },
    );
    return JSON.parse(out);
  } finally {
    cleanupDir(dir);
  }
}

function orgBody(repoDir: string, keywords: string[]): string {
  return [
    "#+TITLE: bounds",
    `#+TT_REPO: ${repoDir}`,
    "#+TT_BRANCH: main",
    ...keywords,
    "",
    "* Phase 1: p1",
    "  :PROPERTIES:",
    "  :ID: p1",
    "  :CHECKS: true",
    "  :END:",
    "  Goal: the bounds reach the plan",
    "  Acceptance:",
    "  - it works",
  ].join("\n") + "\n";
}

test("plan 07e: TT_FACT_MINUTES reaches reproductionMs through the Emacs parser (120000 for 2)", () => {
  const repo = makeRepo();
  try {
    const plan = parsePlan(orgBody(repo, ["#+TT_FACT_MINUTES: 2"]));
    assert.equal(plan.deadlines?.reproductionMs, 120_000);
  } finally {
    cleanupDir(repo);
  }
});

test("plan 07e: the six new keywords all reach plan.deadlines, and without any every deadline is today's", () => {
  const repo = makeRepo();
  try {
    const plan = parsePlan(
      orgBody(repo, [
        "#+TT_EVALUATE_MINUTES: 3",
        "#+TT_FACT_MINUTES: 4",
        "#+TT_FIX_ATTEMPT_MINUTES: 6",
        "#+TT_MERGE_MINUTES: 7",
        "#+TT_FINAL_CHECK_MINUTES: 8",
        "#+TT_OWNER_WAIT_MINUTES: 9",
      ]),
    );
    assert.deepEqual(plan.deadlines, {
      evaluateMs: 180_000,
      reproductionMs: 240_000,
      fixerAttemptMs: 360_000,
      mergeMs: 420_000,
      finalCheckMs: 480_000,
      ownerWaitMs: 540_000,
    });
    const bare = parsePlan(orgBody(repo, []));
    assert.equal(bare.deadlines, undefined, "no keyword, no override");
    const effective = { ...DEFAULT_DEADLINES, ...(bare.deadlines ?? {}) };
    for (const [field, value] of Object.entries(DEFAULT_DEADLINES)) {
      assert.equal((effective as Record<string, unknown>)[field], value, `${field} stays today's value`);
    }
  } finally {
    cleanupDir(repo);
  }
});

test("plan 07e: #+TT_CHECK_WINDOW reaches the plan as written", () => {
  const repo = makeRepo();
  try {
    assert.equal(parsePlan(orgBody(repo, ["#+TT_CHECK_WINDOW: exclusive"])).checkWindow, "exclusive");
    assert.equal(parsePlan(orgBody(repo, [])).checkWindow, undefined);
  } finally {
    cleanupDir(repo);
  }
});

/** Core events of a run, in order. */
function coreEvents(runDir: string): Array<Record<string, unknown>> {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => r.event as Record<string, unknown>);
}

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
};

/** A worker that submits at once (the minimal round). */
function quickWorker(): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: "printf 'ok\\n' > marker.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** An empty review, so `stubReviews` dispatch still settles. */
function stubReviewer(seat: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
          phaseId: state.phase.phaseId,
          candidateSha: state.phase.candidate?.sha,
          contractVersion: state.phase.contract.contractVersion,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings: [],
        },
      },
    ],
  };
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

/** Every `checks/<sha>/record.json` under the run. */
function checkRecords(runDir: string): Array<Record<string, unknown>> {
  const root = path.join(runDir, "checks");
  if (!fs.existsSync(root)) return [];
  const out: Array<Record<string, unknown>> = [];
  for (const sha of fs.readdirSync(root)) {
    const file = path.join(root, sha, "record.json");
    if (!fs.existsSync(file)) continue;
    try {
      out.push(JSON.parse(fs.readFileSync(file, "utf8")) as Record<string, unknown>);
    } catch {
      // a torn write is retried by the caller's waitFor
    }
  }
  return out;
}

test("plan 07e: a check past TT_CHECK_MINUTES records BOUND_REACHED, the record fails, and its pgid is gone", async () => {
  const setup = await setupConductor({
    checks: ["sleep 5"],
    phaseChecks: ["sleep 5"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 1_500 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await setup.conductor.start();
    // Lane b also records the baseline's bound; wait for the candidate check.
    await waitFor(() => coreEvents(setup.runDir).some((e) => e.type === "CHECKS_FAILED"), 60_000, 100, setup.runDir);
    const bound = coreEvents(setup.runDir).find((e) => e.type === "BOUND_REACHED" && e.kind === "check" && String(e.scope).includes("sleep 5"))!;
    assert.equal(bound.limitMs, 1_500);
    assert.ok(typeof bound.elapsedMs === "number" && (bound.elapsedMs as number) >= 1_400, `elapsed ${String(bound.elapsedMs)}`);
    assert.match(String(bound.scope), /sleep 5/);
    assert.ok(coreEvents(setup.runDir).some((e) => e.type === "CHECKS_FAILED"), "the check failed");
    const record = checkRecords(setup.runDir)[0]!;
    const command = (record.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.timedOut, true);
    assert.equal(command.passed, false);
    // The killed check's process group is gone (design §8.1: kill its group).
    const intent = readEvents(setup.runDir).find((r) => r.kind === "intent" && String(r.actionId ?? "").startsWith("check-sh-") && (r.event as { pgid?: number }).pgid !== undefined);
    const pgid = (intent!.event as { pgid: number }).pgid;
    assert.throws(() => process.kill(-pgid, 0), /ESRCH/);
  } finally {
    await teardown(setup);
  }
});

test("plan 07e: a check that exits non-zero with no output is recorded as silent, not failed", async () => {
  const setup = await setupConductor({
    checks: ["exit 3"],
    phaseChecks: ["exit 3"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 10_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => checkRecords(setup.runDir).length > 0, 60_000, 100, setup.runDir);
    const record = checkRecords(setup.runDir)[0]!;
    const command = (record.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.class, "silent");
    assert.equal(command.note, "check exited 3 with no output");
    assert.match(fs.readFileSync(path.join(setup.runDir, "checks", String(record.candidateSha), String(command.log)), "utf8"), /check exited 3 with no output/);
  } finally {
    await teardown(setup);
  }
});

test("plan 07e: a check the owner stops is recorded as interrupted, not failed", async () => {
  const setup = await setupConductor({
    checks: ["sleep 6"],
    phaseChecks: ["sleep 30"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 60_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => readEvents(setup.runDir).some((r) => r.kind === "intent" && String(r.actionId ?? "").startsWith("check-sh-")),
      60_000,
      50,
      setup.runDir,
    );
    await setup.conductor.stop();
    await waitFor(() => checkRecords(setup.runDir).length > 0, 10_000, 50, setup.runDir);
    const command = (checkRecords(setup.runDir)[0]!.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.class, "interrupted");
    assert.match(String(command.note), /interrupted/);
  } finally {
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});


// ---------------------------------------------------------------------------
// The domain/fix-round harness (the same shape fix-rounds.test.ts uses): two
// lanes, M raises one point per domain on round 1, a fix round follows.
// ---------------------------------------------------------------------------

const DOMAINS: Domain[] = [
  { name: "booking", globs: ["booking/**"] },
  { name: "gates", globs: ["gates/**"] },
];

const BOOKING_DONE = "test -f booking-fixed.txt";

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

function laneWorker(round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane} r${round}\\n' > lane-${lane}-r${round}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

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

/** M raises one point per domain on round 1, with the given doneWhen. */
function reviewerWithPoints(doneWhen: string) {
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
          doneWhen,
        },
        {
          kind: "defect",
          severity: "blocking",
          evidence: "the gates path is wrong",
          domain: "gates",
          item: "R2",
          where: "gates/b.ts:1 fn",
          expected: "GATES_EXPECTED",
          doneWhen,
        },
      ]);
    }
    return laneReview(seat as Reviewer, K);
  };
}

function pickVote(seat: Reviewer, round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [
      {
        kind: "call-submit",
        tool: "submit_pick_vote",
        args: { inspected: ["a", "b", "c", "d"], round, seat, lane, why: `${seat} picks ${lane}`, loserHad: { yes: false, anchors: [] } },
      },
    ],
  };
}

/** A fixer that writes its domain's marker and submits. */
function fixedDomain(domain: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

async function setupFixRun(doneWhen: string, opts: { deadlines?: Record<string, number>; fixerScriptFor?: (domain: string) => { hello: unknown; steps: FakePiStep[] } } = {}) {
  return setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, workerAttemptMs: 30_000, checkMs: 20_000, probeMs: 20_000, freezeMs: 20_000, evaluateMs: 10_000, reviewMs: 20_000, panelMs: 10_000, ...(opts.deadlines ?? {}) },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints(doneWhen)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    fixerScriptFor: (domain) => (opts.fixerScriptFor ? opts.fixerScriptFor(domain) : fixedDomain(domain)),
  });
}

test("plan 07e: a doneWhen past TT_FACT_MINUTES records BOUND_REACHED, the fact is timed out, the point stays open", async () => {
  const setup = await setupFixRun("sleep 5", { deadlines: { reproductionMs: 1_500 } });
  try {
    await setup.conductor.start();
    await waitFor(() => coreEvents(setup.runDir).some((e) => e.type === "FINDING_FACT" && e.timedOut === true), 240_000, 100, setup.runDir);
    const fact = coreEvents(setup.runDir).find((e) => e.type === "FINDING_FACT" && e.timedOut === true)!;
    assert.ok(typeof fact.findingId === "string" && fact.findingId.length > 0);
    const bound = coreEvents(setup.runDir).find((e) => e.type === "BOUND_REACHED" && e.kind === "fact")!;
    assert.ok(bound, "the fact bound is recorded");
    assert.equal(bound.limitMs, 1_500);
    assert.match(String(bound.scope), /finding /);
    const open = setup.conductor.state.phase.findings.filter((f) => f.status === "open");
    assert.ok(open.length > 0, "the timed-out doneWhen repaired nothing: its point stays open");
    assert.ok(!open.some((f) => f.id === fact.findingId && f.status !== "open"));
  } finally {
    await teardown(setup);
  }
});

test("plan 07e: a fixer past its attempt bound is stopped and skipped with note bound: fixer-attempt, the other fixer merges", async () => {
  const setup = await setupFixRun(BOOKING_DONE, {
    deadlines: { workerAttemptMs: 2_000 },
    fixerScriptFor: (domain) =>
      domain === "booking"
        ? {
            hello: defaultWorkerHello(),
            steps: [
              { kind: "sleep", ms: 5_000 },
              { kind: "call-sh", command: "printf 'late\\n' > booking-fixed.txt" },
              { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
            ],
          }
        : fixedDomain(domain),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.rounds?.[0]?.fixed !== undefined, 240_000, 50, setup.runDir);
    const fixed = setup.conductor.state.phase.rounds![0].fixed!;
    const booking = fixed.tasks.find((t) => t.domain === "booking")!;
    const gates = fixed.tasks.find((t) => t.domain === "gates")!;
    assert.match(String(booking.note), /bound: fixer-attempt/, JSON.stringify(fixed.tasks));
    assert.equal(gates.note, undefined, "the other fixer merged normally");
    assert.ok(typeof gates.sha === "string" && gates.sha.length > 0);
    const bound = coreEvents(setup.runDir).find((e) => e.type === "BOUND_REACHED" && e.kind === "fixer-attempt")!;
    assert.ok(bound, "the fixer attempt bound is recorded");
    assert.equal(bound.limitMs, 2_000);
    assert.match(String(bound.scope), /booking/);
    assert.ok(fixed.candidate, "the fix round still has a candidate (the other fixer's work)");
  } finally {
    await teardown(setup);
  }
});

test("plan 07e: a merge past TT_MERGE_MINUTES is aborted, recorded, and the first fixer's merge is the candidate", async () => {
  const realGit = execFileSync("sh", ["-c", "command -v git"], { encoding: "utf8" }).trim();
  const shimDir = fs.mkdtempSync("/tmp/tt-07e-git-");
  const counter = path.join(shimDir, "count");
  fs.writeFileSync(counter, "0\n");
  fs.writeFileSync(
    path.join(shimDir, "git"),
    `#!/bin/sh
case "$*" in
  *"merge fixer commit"*)
    n=$(cat "${counter}")
    n=$((n + 1))
    echo "$n" > "${counter}"
    if [ "$n" -ge 2 ]; then sleep 6; fi
    ;;
esac
exec "${realGit}" "$@"
`,
    { mode: 0o755 },
  );
  const oldPath = process.env.PATH;
  process.env.PATH = `${shimDir}:${oldPath ?? ""}`;
  const setup = await setupFixRun(BOOKING_DONE, { deadlines: { mergeMs: 1_500 } });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.rounds?.[0]?.fixed !== undefined, 240_000, 50, setup.runDir);
    const fixed = setup.conductor.state.phase.rounds![0].fixed!;
    const bound = coreEvents(setup.runDir).find((e) => e.type === "BOUND_REACHED" && e.kind === "merge")!;
    assert.ok(bound, "the merge bound is recorded");
    assert.equal(bound.limitMs, 1_500);
    const timedOutTask = fixed.tasks.find((t) => /bound: merge/.test(String(t.note)));
    assert.ok(timedOutTask, `the timed-out merge is noted: ${JSON.stringify(fixed.tasks)}`);
    assert.match(String(bound.scope), new RegExp(`round 1 fixer ${timedOutTask!.domain}`));
    // The OTHER fixer's merge landed first and is the candidate.
    const mergedTask = fixed.tasks.find((t) => t.domain !== timedOutTask!.domain && typeof t.sha === "string" && t.sha.length > 0);
    assert.ok(mergedTask, `the first fixer merged: ${JSON.stringify(fixed.tasks)}`);
    assert.ok(fixed.candidate, "the first fixer's merge is the candidate");
    const files = execFileSync("git", ["-C", setup.plan.repo, "diff", "--name-only", `${fixed.base}..${fixed.candidate}`], { encoding: "utf8" });
    assert.match(files, new RegExp(`${mergedTask!.domain}-fixed\\.txt`));
    assert.doesNotMatch(files, new RegExp(`${timedOutTask!.domain}-fixed\\.txt`), "the timed-out fixer's work is not in the candidate");
  } finally {
    process.env.PATH = oldPath;
    await teardown(setup);
    cleanupDir(shimDir);
  }
});

/** A run whose checks always fail: every repair round is spent, then the
 * rounds bound parks the phase on the owner. */
async function setupFailingRun(deadlines: Record<string, number> = {}) {
  return setupConductor({
    checks: ["false"],
    phaseChecks: ["false"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 10_000, ...deadlines },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
}

test("plan 07e: an open owner request reminds once per TT_OWNER_WAIT_MINUTES and never answers itself", async () => {
  const setup = await setupFailingRun({ ownerWaitMs: 1_000, inboxPollMs: 100 });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 240_000, 50, setup.runDir);
    await waitFor(() => coreEvents(setup.runDir).filter((e) => e.type === "BOUND_REACHED" && e.kind === "owner-wait").length >= 2, 20_000, 100, setup.runDir);
    const bounds = coreEvents(setup.runDir).filter((e) => e.type === "BOUND_REACHED" && e.kind === "owner-wait");
    assert.ok(bounds.length >= 2, "one reminder per bound");
    assert.equal(bounds[0].limitMs, 1_000);
    const notifications = fs.readFileSync(path.join(setup.runRoot, "notifications.jsonl"), "utf8").trim().split("\n").map((l) => JSON.parse(l) as { reminder?: boolean });
    const reminders = notifications.filter((n) => n.reminder === true);
    assert.ok(reminders.length >= 2, `one notification per bound (got ${reminders.length})`);
    assert.equal(reminders.length, bounds.length, "the notification count follows the recorded bounds");
    // The bound never decides: the phase is still parked and the request open.
    assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER");
    assert.ok(setup.conductor.state.phase.ownerRequests.some((r) => r.status === "open"));
  } finally {
    await teardown(setup);
  }
});

