// 07g1 acceptance: the per-domain loop. These drive the real loop module
// with a host that records every effect, so the same-session order, the bound,
// the stateless brief and the read-only review contract are all observable
// without a model or git.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { Conductor, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import { domainReviewApproves, runDomainLoop } from "../../src/fix-loop.ts";
import { domainReviewBrief, type FixTask } from "../../src/core/fix-rounds.ts";
import type { Domain } from "../../src/core/domains.ts";
import type { DomainFacts, DomainLoopRecord, DomainReview, Reviewer, State } from "../../src/core/types.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { readLog } from "../../src/effects/log.ts";
import {
  cleanupDir,
  defaultDomainReviewHello,
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

function domainPhase() {
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

/** M raises the booking point on round 1; later reviews are empty. */
function reviewerWithBookingPoint(seat: Reviewer, _candidate: string, state: State): { hello: unknown; steps: FakePiStep[] } {
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
        doneWhen: BOOKING_DONE,
      },
    ]);
  }
  return laneReview(seat, K);
}

/** M raises one booking and one gates point on round 1. */
function reviewerWithTwoDomainPoints(seat: Reviewer, _candidate: string, state: State): { hello: unknown; steps: FakePiStep[] } {
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
        doneWhen: BOOKING_DONE,
      },
      {
        kind: "defect",
        severity: "blocking",
        evidence: "the gates path is wrong",
        domain: "gates",
        item: "R2",
        where: "gates/b.ts:1 fn",
        expected: "GATES_EXPECTED",
        doneWhen: "test -f gates-fixed.txt",
      },
    ]);
  }
  return laneReview(seat, K);
}

function pickVote(seat: Reviewer, round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { inspected: ["a", "b", "c", "d"], round, seat, lane, why: `${seat} picks ${lane}`, loserHad: { yes: false, anchors: [] } } }],
  };
}

function pointIdOf(state: State, domain: string, _round: number): string {
  const point = (state.phase.findings ?? []).find((f) => f.status === "open" && f.domain === domain);
  assert.ok(point, `an open ${domain} point is on the phase`);
  return point.id;
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

test("a domain review that marks a point not fixed sends it back to the same fixer", async () => {
  let domainReviewLaunches = 0;
  const setup = await setupConductor({
    phase: domainPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithBookingPoint(seat as Reviewer, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    domainReviewScriptFor: (domain, round, iteration, state) => {
      domainReviewLaunches += 1;
      const pointId = pointIdOf(state, domain, round);
      return {
        hello: defaultDomainReviewHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_domain_review",
            args: {
              points: [
                {
                  pointId,
                  fixed: iteration > 1,
                  why: iteration === 1 ? "STILL_WRONG booking/a.ts:1 the point is not fixed" : "README.md:1 the point is fixed",
                },
              ],
              quality: { ok: true },
            },
          },
        ],
      };
    },
    fixerScriptFor: (domain, round, n) => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-sh",
          command: n === 1 ? `printf 'fixed\\n' > ${domain}-fixed.txt` : `printf 'fixed again\\n' > second-attempt.txt`,
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    assert.ok(domainReviewLaunches >= 2, "the reviewer ran again after the rejected point");
    const events = coreEvents(setup.runDir);
    assert.equal(events.filter((e) => e.type === "DOMAIN_CHECKED").length, 2, "both fixer commits got facts");
    assert.equal(events.filter((e) => e.type === "DOMAIN_REVIEWED").length, 2, "both commits were reviewed");
    assert.equal(events.filter((e) => e.type === "DOMAIN_APPROVED").length, 1, "the second review approved the domain");
    const passes = readLog(runPaths(setup.runDir).events).records.filter((r) => r.kind === "domain_loop_pass").map((r) => r.event as DomainLoopRecord);
    assert.equal(passes.length, 2, "two loop passes were recorded");
    const firstSha = passes[0].sha;
    assert.ok(firstSha, "the first fixer commit is recorded");
    const intents = readLog(runPaths(setup.runDir).events).records.filter(
      (r) => r.kind === "intent" && (r.event as { fixer?: string }).fixer === "fix-booking",
    );
    assert.equal(intents.length, 2, "the same domain fixer was launched twice");
    assert.equal((intents[1].event as { fixOf?: string }).fixOf, firstSha, "the second launch continued the first commit's session");
    assert.ok(fs.existsSync(path.join(runPaths(setup.runDir).sessions, "fix-1-booking")), "both fixer turns use one session directory");
    const candidate = fixed()!.candidate!;
    const candidateDir = path.join(runPaths(setup.runDir).candidates, candidate);
    assert.ok(fs.existsSync(path.join(candidateDir, "second-attempt.txt")), "the second fixer attempt is in the merged candidate");
    const subjects = execFileSync("git", ["-C", setup.repo.dir, "log", "--format=%s", candidate], { encoding: "utf8" });
    assert.doesNotMatch(subjects, /domain review/i, "the review authored no commit");
  } finally {
    await teardown(setup);
  }
});

test("a domain review session has read-only tools and adds no commit", async () => {
  assert.deepEqual(ROLE_TOOLS["domain-review"], ["read", "grep", "find", "ls", "submit_domain_review"], "the domain review's tool set is read-only");
  let reviewLaunches = 0;
  const setup = await setupConductor({
    phase: domainPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithBookingPoint(seat as Reviewer, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    domainReviewScriptFor: (domain, round, iteration, state) => {
      reviewLaunches += 1;
      const pointId = pointIdOf(state, domain, round);
      return {
        hello: defaultDomainReviewHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_domain_review",
            args: { points: [{ pointId, fixed: true, why: "README.md:1 the point is fixed" }], quality: { ok: true } },
          },
        ],
      };
    },
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    assert.ok(reviewLaunches >= 1, "the stateless domain review script was launched");
    const approved = coreEvents(setup.runDir).find((e) => e.type === "DOMAIN_APPROVED");
    assert.ok(approved, "the domain was approved");
    const sha = String(approved.sha);
    const candidateDir = path.join(runPaths(setup.runDir).candidates, sha);
    assert.ok(fs.existsSync(candidateDir), "the reviewed commit has its candidate checkout");
    assert.equal(execFileSync("git", ["-C", candidateDir, "rev-parse", "HEAD"], { encoding: "utf8" }).trim(), sha, "the review did not move HEAD");
    assert.equal(execFileSync("git", ["-C", candidateDir, "status", "--porcelain"], { encoding: "utf8" }).trim(), "", "the review left the checkout clean");
  } finally {
    await teardown(setup);
  }
});

test("a domain loop at its bound is dropped while the approved domain merges", async () => {
  const setup = await setupConductor({
    phase: { ...domainPhase(), domainLoopMax: 1 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithTwoDomainPoints(seat as Reviewer, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    domainReviewScriptFor: (domain, round, _iteration, state) => {
      const pointId = pointIdOf(state, domain, round);
      return {
        hello: defaultDomainReviewHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_domain_review",
            args: {
              points: [
                {
                  pointId,
                  fixed: domain === "booking",
                  why: domain === "booking" ? "README.md:1 fixed" : "gates/b.ts:1 STILL_WRONG the gates point is not fixed",
                },
              ],
              quality: { ok: true },
            },
          },
        ],
      };
    },
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'fixed\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    const fixed = () => setup.conductor.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);
    const record = fixed()!;
    assert.deepEqual(record.tasks.map((t) => t.domain), ["booking", "gates"], "both domains ran");
    const events = coreEvents(setup.runDir);
    assert.ok(events.some((e) => e.type === "DOMAIN_APPROVED" && e.domain === "booking"), "the booking domain approved");
    const dropped = events.find((e) => e.type === "DOMAIN_DROPPED" && e.domain === "gates") as { reason?: string } | undefined;
    assert.ok(dropped, "the gates domain was dropped at its bound");
    assert.match(dropped.reason ?? "", /bound/);
    assert.equal(record.merges.length, 1, "only the approved domain merged");
    assert.ok(record.candidate, "the approved domain produced a merged candidate");
    const candidateDir = path.join(runPaths(setup.runDir).candidates, record.candidate!);
    assert.ok(fs.existsSync(path.join(candidateDir, "booking-fixed.txt")), "the approved domain's work is in the candidate");
    assert.ok(!fs.existsSync(path.join(candidateDir, "gates-fixed.txt")), "the dropped domain's work never merged");
    const gates = (setup.conductor.state.phase.findings ?? []).find((f) => f.domain === "gates" && f.raisedBy === "M");
    assert.equal(gates?.status, "open", "a dropped domain's points stay open");
  } finally {
    await teardown(setup);
  }
});

const task: FixTask = {
  domain: "x",
  findingIds: ["P"],
  findings: [
    {
      id: "P",
      version: 1,
      phaseId: "p1",
      kind: "defect",
      severity: "blocking",
      evidence: "src/x.ts:1 wrong",
      raisedBy: "M",
      status: "open",
      boundCandidateSha: "B",
      domain: "x",
      where: "src/x.ts:1 fn",
      expected: "right",
      doneWhen: "true",
    },
  ],
};

const clean: DomainFacts = { checkPassed: true, points: [{ id: "P", onBase: false, onHead: true }] };
const red: DomainFacts = { checkPassed: false, checkNote: "the fixer check failed: RED", points: [{ id: "P", onBase: false, onHead: false }] };
const approve: DomainReview = { points: [{ pointId: "P", fixed: true, why: "README.md:1 it is fixed" }], quality: { ok: true } };
const reject: DomainReview = { points: [{ pointId: "P", fixed: false, why: "src/x.ts:1 still wrong" }], quality: { ok: true } };

test("a red fix check goes back to the same fixer with no review session", async () => {
  const calls: string[] = [];
  const outcome = await runDomainLoop(task, {
    maxReviews: 3,
    build: async () => ({ sha: "a" }),
    facts: async (sha) => {
      calls.push(`facts:${sha}`);
      return sha === "a" ? red : clean;
    },
    review: async () => {
      calls.push("review");
      return approve;
    },
    fix: async (sha, failure) => {
      calls.push(`fix:${sha}:${failure.includes("RED")}`);
      return { sha: "b" };
    },
  });
  assert.equal(outcome.status, "approved");
  assert.deepEqual(calls, ["facts:a", "fix:a:true", "facts:b", "review"]);
});

test("a domain loop at its bound is dropped and the other domains merge", async () => {
  const good: FixTask = { ...task, domain: "good", findingIds: ["G"], findings: [{ ...task.findings[0], id: "G", domain: "good" }] };
  const bad: FixTask = { ...task, domain: "bad" };
  const goodOutcome = runDomainLoop(good, {
    maxReviews: 1,
    build: async () => ({ sha: "g1" }),
    facts: async () => ({ checkPassed: true, points: [{ id: "G", onBase: false, onHead: true }] }),
    review: async () => ({ points: [{ pointId: "G", fixed: true, why: "README.md:1 fixed" }], quality: { ok: true } }),
    fix: async () => ({ sha: "g2" }),
  });
  let badReviews = 0;
  const badOutcome = runDomainLoop(bad, {
    maxReviews: 2,
    build: async () => ({ sha: "d1" }),
    facts: async () => clean,
    review: async () => {
      badReviews += 1;
      return reject;
    },
    fix: async () => ({ sha: `d${badReviews + 1}` }),
  });
  const [a, b] = await Promise.all([goodOutcome, badOutcome]);
  assert.equal(a.status, "approved");
  assert.equal(b.status, "dropped", "the bound drops the domain");
  assert.equal(badReviews, 2);
  assert.match(b.reason!, /bound/);
});

test("a resumed domain loop counts the reviews its log already spent against the bound", async () => {
  // 07g1 (A1): the bound is the domain round's whole review budget, not a
  // fresh one per conductor process. Two reviews are on the log; the resumed
  // loop may take only the third and then drops.
  let reviews = 0;
  let fixes = 0;
  const outcome = await runDomainLoop(task, {
    maxReviews: 3,
    startIteration: 3,
    startReviews: 2,
    build: async () => ({ sha: "c" }),
    facts: async () => clean,
    review: async () => {
      reviews += 1;
      return reject;
    },
    fix: async () => {
      fixes += 1;
      return { sha: "d" };
    },
  });
  assert.equal(outcome.status, "dropped");
  assert.equal(reviews, 1, "only the one review left in the budget was taken");
  assert.equal(fixes, 0, "a spent budget sends no more work to the fixer");
  assert.equal(outcome.reviews, 3, "the outcome counts every review spent, including the log's");
  assert.match(outcome.reason!, /after 3 reviews/);
});

test("a quality problem goes back to the same fixer like an unfixed point", async () => {
  // 07g1 (A2): the domain review answers quality too. A point not fixed and a
  // quality problem that CITES a plan item or a design line both send the
  // verdicts back to the same fixer; only a review with every point fixed and
  // no citing quality problem approves the domain. 07g1b (A2): a quality
  // remark that cites neither is a note on the approval, not a fixer round.
  const itemIds = ["R1", "C1"];
  const remarks: DomainReview = {
    points: [{ pointId: "P", fixed: true, why: "README.md:1 it is fixed" }],
    quality: { ok: false, problems: ["R1: src/x.ts:9 the helper bypasses the design's gate"] },
    notes: ["the naming of P's helper could be better"],
  };
  assert.equal(domainReviewApproves(task, remarks, itemIds), false, "a quality problem that cites an item blocks a fully fixed domain");
  const uncited: DomainReview = {
    points: [{ pointId: "P", fixed: true, why: "README.md:1 it is fixed" }],
    quality: { ok: false, problems: ["the naming of P's helper could be better"] },
  };
  assert.equal(domainReviewApproves(task, uncited, itemIds), true, "an uncited quality remark never blocks");
  assert.equal(domainReviewApproves(task, uncited), true, "with no plan items in the loop an uncited remark never blocks either");
  const sent: string[] = [];
  let reviews = 0;
  const outcome = await runDomainLoop(task, {
    maxReviews: 3,
    itemIds,
    build: async () => ({ sha: "a" }),
    facts: async () => clean,
    review: async () => {
      reviews += 1;
      return reviews === 1 ? remarks : approve;
    },
    fix: async (_sha, failure) => {
      sent.push(failure);
      return { sha: "b" };
    },
  });
  assert.equal(outcome.status, "approved", "the second review approved the domain");
  assert.equal(reviews, 2, "the quality problem cost the same fixer a second round");
  assert.equal(outcome.fixes, 1, "the quality problem sent the work back");
  assert.equal(sent.length, 1, "the fixer is sent the review's verdict");
  assert.match(sent[0], /Quality: R1: src\/x\.ts:9 the helper bypasses the design's gate/, "the fixer sees the citing quality problem");
  const brief = domainReviewBrief(task, []);
  assert.match(brief, /a quality problem that names a plan item id or a design line/, "the brief tells the reviewer what a citing quality problem does");
  assert.doesNotMatch(brief, /never sends work back/);
  // The uncited remark alone: approved at the first review, kept as a note.
  const noted = await runDomainLoop(task, {
    maxReviews: 3,
    itemIds,
    build: async () => ({ sha: "a" }),
    facts: async () => clean,
    review: async () => uncited,
    fix: async () => ({ sha: "b" }),
  });
  assert.equal(noted.status, "approved", "an uncited quality remark does not hold the domain");
  assert.equal(noted.fixes, 0, "and costs no fixer round");
  assert.ok(
    noted.records.some((r) => (r.review?.notes ?? []).some((n) => /quality note/.test(n))),
    "the uncited remark is kept as a note of the approval",
  );
});

test("a resumed conductor rebuilds the domain brief from its log and continues the pass numbering", async () => {
  // Pass 1: the fixer commits, the facts are green and the review sends the
  // point back. The run is stopped while the fixer works on that verdict, so
  // the fix round is not recorded. A second conductor on the same run starts
  // with NO in-memory loop records: its next review's brief must be rebuilt
  // from the event log alone, and its next pass must continue the numbering
  // instead of overwriting pass 1.
  const setup = await setupConductor({
    phase: { ...domainPhase(), domainLoopMax: 3 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithBookingPoint(seat as Reviewer, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1, "a"),
    domainReviewScriptFor: (domain, round, iteration, state) => {
      const pointId = pointIdOf(state, domain, round);
      return {
        hello: defaultDomainReviewHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_domain_review",
            args: {
              points: [
                {
                  pointId,
                  fixed: iteration > 1,
                  why: iteration === 1 ? "STILL_WRONG booking/a.ts:1 the point is not fixed" : "README.md:1 the point is fixed",
                },
              ],
              quality: iteration === 1 ? { ok: false, problems: ["QUALITY_GAP booking/a.ts:9"] } : { ok: true },
            },
          },
        ],
      };
    },
    fixerScriptFor: (domain, round, n) => ({
      hello: defaultWorkerHello(),
      steps: [
        // The attempt the review's verdict starts sleeps, so the stop lands
        // inside it and pass 1 stays the last recorded pass.
        ...(n === 1 ? [] : [{ kind: "sleep", ms: 4_000 } as FakePiStep]),
        { kind: "call-sh", command: `printf 'fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  let resumed: Conductor | undefined;
  try {
    await setup.conductor.start();
    const reviewed = () =>
      readLog(runPaths(setup.runDir).events).records.some(
        (r) => r.kind === "event" && (r.event as { type?: string }).type === "DOMAIN_REVIEWED",
      );
    await waitFor(reviewed, 180_000, 25, setup.runDir);
    await setup.conductor.stop();
    assert.equal(setup.conductor.state.phase.rounds?.[0]?.fixed, undefined, "the stopped fix round is not recorded");
    const passes = readLog(runPaths(setup.runDir).events).records
      .filter((r) => r.kind === "domain_loop_pass")
      .map((r) => r.event as DomainLoopRecord);
    const first = passes.find((p) => p.iteration === 1 && p.review);
    assert.ok(first?.sha, "pass 1 (the reviewed commit and its verdict) is on the log");
    const pointId = pointIdOf(setup.conductor.state, "booking", 1);

    // The resumed conductor's own scripts: its domain review approves pass 2,
    // its fixer commits again, and the round-2 lane work stays scripted.
    const scripts = new Map<string, string>();
    const scriptFor = (name: string, script: { hello?: unknown; steps: FakePiStep[] }): string => {
      if (!scripts.has(name)) scripts.set(name, writeScript(setup.scriptsDir, name, script));
      return scripts.get(name)!;
    };
    resumed = new Conductor({
      runDir: setup.runDir,
      plan: setup.plan as RunPlanFile,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      stubReviews: true,
      deadlines: FAST,
      checkLockPath: path.join(setup.runRoot, "check.lock"),
      piEnvFor: (role, agentId) => {
        const domainReview = agentId.match(/^domain-review-(\d+)-(.+?)-(\d+)-/);
        if (role === "domain-review" && domainReview) {
          return {
            FAKE_PI_SCRIPT: scriptFor(`resume-domain-review-${agentId}`, {
              hello: defaultDomainReviewHello(),
              steps: [
                {
                  kind: "call-submit",
                  tool: "submit_domain_review",
                  args: { points: [{ pointId, fixed: true, why: `README.md:1 the point is fixed at pass ${domainReview[3]}` }], quality: { ok: true } },
                },
              ],
            }),
          };
        }
        const fixerAgent = agentId.match(/^fix-(\d+)-(.+?)-fixer-\d+-[0-9a-f]+$/);
        if (role === "worker" && fixerAgent) {
          const domain = fixerAgent[2];
          return {
            FAKE_PI_SCRIPT: scriptFor(`resume-fixer-${agentId}`, {
              hello: defaultWorkerHello(),
              steps: [
                { kind: "call-sh", command: `printf 'fixed\\n' > ${domain}-fixed.txt && printf 'resumed\\n' > resumed-fix.txt` },
                { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
              ],
            }),
          };
        }
        const lane = agentId.match(/^lane-(\d+)-([a-z])-/);
        if (role === "worker" && lane) {
          return { FAKE_PI_SCRIPT: scriptFor(`resume-lane-${agentId}`, laneWorker(Number(lane[1]), lane[2])) };
        }
        const laneReview = agentId.match(/^lane-review-(\d+)-([a-z])-([A-Za-z0-9_]+)-/);
        if (role === "reviewer" && laneReview) {
          const K = resumed!.state.phase.contract.contractVersion;
          return { FAKE_PI_SCRIPT: scriptFor(`resume-lane-review-${laneReview[3]}`, laneReview(laneReview[3] as Reviewer, K)) };
        }
        const pick = agentId.match(/^(?:pick|revote)-(\d+)-([A-Za-z0-9_]+)-/);
        if (role === "picker" && pick) {
          return { FAKE_PI_SCRIPT: scriptFor(`resume-pick-${pick[2]}`, pickVote(pick[2] as Reviewer, Number(pick[1]), "a")) };
        }
        return undefined;
      },
    });
    await resumed.start();
    const fixed = () => resumed!.state.phase.rounds?.[0]?.fixed;
    await waitFor(() => fixed() !== undefined, 180_000, 50, setup.runDir);

    const reviews = coreEvents(setup.runDir).filter(
      (e) => e.type === "DOMAIN_REVIEWED" && e.domain === "booking",
    ) as Array<{ iteration: number; brief: string; sha?: string }>;
    assert.equal(reviews.length, 2, "the resumed run took exactly the next review");
    assert.equal(reviews[1].iteration, 2, "the resumed loop continues at pass 2 instead of reusing pass 1");
    assert.match(reviews[1].brief, new RegExp(`- ${pointId} \\(item [^)]+\\): booking/a\\.ts:1 fn`), "the point and its file are in the brief");
    assert.match(reviews[1].brief, /doneWhen: test -f booking-fixed\.txt/, "the point's doneWhen is in the brief");
    assert.match(reviews[1].brief, /The loop so far \(oldest first\):/, "the resumed brief replays the log's history");
    assert.match(reviews[1].brief, new RegExp(`pass 1, commit ${String(first!.sha).slice(0, 9)}`), "pass 1 and its commit are in the resumed brief");
    assert.match(reviews[1].brief, new RegExp(`review: ${pointId} NOT fixed: STILL_WRONG`), "pass 1's verdict reaches the next review");
    assert.match(reviews[1].brief, /quality: QUALITY_GAP booking\/a\.ts:9/, "pass 1's quality problem reaches the next review");
    assert.ok(
      coreEvents(setup.runDir).some((e) => e.type === "DOMAIN_APPROVED" && e.domain === "booking"),
      "the resumed pass 2 approved the domain",
    );
    const candidateDir = path.join(runPaths(setup.runDir).candidates, fixed()!.candidate!);
    assert.ok(fs.existsSync(path.join(candidateDir, "resumed-fix.txt")), "the resumed fixer's commit is the merged candidate");
  } finally {
    await (resumed ?? setup.conductor).stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
