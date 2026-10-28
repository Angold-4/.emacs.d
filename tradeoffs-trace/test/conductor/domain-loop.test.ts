// 07g1 acceptance: the per-domain loop. These drive the real loop module
// with a host that records every effect, so the same-session order, the bound,
// the stateless brief and the read-only review contract are all observable
// without a model or git.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { runPaths } from "../../src/conductor.ts";
import { domainReviewBrief, type FixTask } from "../../src/core/fix-rounds.ts";
import type { Domain } from "../../src/core/domains.ts";
import type { DomainFacts, DomainLoopRecord, DomainReview, Reviewer, State } from "../../src/core/types.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { readLog } from "../../src/effects/log.ts";
import { runDomainLoop } from "../../src/fix-loop.ts";
import {
  cleanupDir,
  defaultDomainReviewHello,
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

test("a resumed domain review gets the same brief", () => {
  const record: DomainLoopRecord = { iteration: 1, sha: "a", facts: clean, review: reject, note: "sent back" };
  const before = domainReviewBrief(task, [record]);
  const resumed = domainReviewBrief(task, [record]);
  assert.equal(resumed, before);
  assert.match(before, /P/);
  assert.match(before, /still wrong/);
});
