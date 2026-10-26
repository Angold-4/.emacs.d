// Plan 07b (design D3/D6): done-when facts, end to end with fake pi. In
// domain mode, after a lane candidate's round checks pass, the conductor runs
// each open point's `doneWhen` in that check's own checkout with the check
// environment, bounded by the reproduction deadline. At the winner's hand-off
// it records one FINDING_FACT per point and repairs each point whose command
// exits 0; a fact on a losing lane repairs nothing and never reaches the log.
// The evaluator's prompt lists the picked candidate's facts.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { Conductor, type RunPlanFile } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import type { Domain } from "../../src/core/domains.ts";
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
  { name: "booking", globs: ["src/**"] },
  { name: "gates", globs: ["test/**"] },
];

/** A two-lane, domain-mode phase. `consolidate: "off"` keeps the next round
 * on the picked candidate (never a combined commit), so the fact is run in the
 * picked candidate's own checkout. */
function lanePhase(overrides: Partial<import("../../src/conductor.ts").RunPlanPhase> = {}) {
  return {
    id: "p1",
    goal: "fix the point",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers: 2,
    domains: DOMAINS,
    consolidate: "off",
    ...overrides,
  };
}

/** The same two-lane phase without `TT_DOMAINS`: point fields are ignored. */
function lanePhaseNoDomains() {
  return {
    id: "p1",
    goal: "fix the point",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers: 2,
    consolidate: "off",
  };
}

/** A lane worker: writes a round/lane-specific marker plus `extra`, submits. */
function laneWorker(round: number, lane: string, extra: string[] = []): { hello: unknown; steps: FakePiStep[] } {
  const files = [`lane-${lane}-r${round}.txt`, ...extra];
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: files.map((f) => `printf 'x\\n' > ${f}`).join(" && ") },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

function crashedLane(): { hello: unknown; steps: FakePiStep[] } {
  return { hello: defaultWorkerHello(), steps: [{ kind: "crash", code: 9 }] };
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

/** M raises ONE blocking domain-mode point in round 1; every other review is
 * empty. The point's `doneWhen` is the test's own probe. */
function reviewerWithPoint(doneWhen: string) {
  return (seat: Reviewer, _candidate: string, state: State): { hello: unknown; steps: FakePiStep[] } => {
    const K = state.phase.contract.contractVersion;
    const round = state.phase.rounds?.length ?? 1;
    if (round === 1 && seat === "M") {
      return laneReview("M", K, [
        {
          kind: "defect",
          severity: "blocking",
          evidence: "the point is not fixed",
          domain: "booking",
          item: "R1",
          where: "src/core/rounds.ts:1 fn",
          expected: "the point is fixed",
          doneWhen,
        },
      ]);
    }
    return laneReview(seat, K);
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

/** The log's core events, typed loosely for the assertions. */
function coreEvents(runDir: string): Array<Record<string, unknown>> {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => r.event as Record<string, unknown>);
}

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

test("after a passing check a finding whose doneWhen exits 0 on the picked candidate is repaired", async () => {
  const doneWhen = "test -f fixed-a";
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    // Lane b produces no candidate, so lane a is the round's only (and picked)
    // candidate and the fact runs exactly once per round.
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane, round === 1 ? [] : ["fixed-a"])),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoint(doneWhen)(seat, candidate, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.doneWhen === doneWhen && f.status === "repaired"),
      150_000,
      50,
      setup.runDir,
    );
    const finding = setup.conductor.state.phase.findings.find((f) => f.doneWhen === doneWhen)!;
    const events = coreEvents(setup.runDir);
    const facts = events.filter((e) => e.type === "FINDING_FACT" && e.findingId === finding.id);
    assert.equal(facts.length, 1, `one FINDING_FACT for the finding: ${JSON.stringify(facts)}`);
    assert.equal(facts[0].exitCode, 0);
    assert.equal(facts[0].timedOut, false);
    assert.equal(facts[0].candidateSha, setup.conductor.state.phase.candidate?.sha, "the fact names the picked candidate");
    assert.ok(
      events.some((e) => e.type === "FINDING_CONFIRMED_REPAIRED" && e.findingId === finding.id),
      "the passing fact records FINDING_CONFIRMED_REPAIRED",
    );
  } finally {
    await teardown(setup);
  }
});

test("a finding whose doneWhen exits non-zero on the picked candidate stays open with its fact", async () => {
  const doneWhen = "test -f fixed-b";
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane)),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoint(doneWhen)(seat, candidate, state),
  });
  try {
    await setup.conductor.start();
    const finding = () => setup.conductor.state.phase.findings.find((f) => f.doneWhen === doneWhen);
    await waitFor(() => coreEvents(setup.runDir).some((e) => e.type === "FINDING_FACT" && e.findingId === finding()?.id), 150_000, 50, setup.runDir);
    const events = coreEvents(setup.runDir);
    const fact = events.find((e) => e.type === "FINDING_FACT" && e.findingId === finding()!.id)!;
    assert.notEqual(fact.exitCode, 0, "a non-zero exit is recorded");
    assert.equal(fact.timedOut, false);
    assert.equal(finding()!.status, "open", "the finding stays open");
    assert.ok(!events.some((e) => e.type === "FINDING_CONFIRMED_REPAIRED" && e.findingId === finding()!.id), "nothing repairs it");
  } finally {
    await teardown(setup);
  }
});

test("a doneWhen that exceeds the reproduction deadline records a timed-out fact and leaves the finding open", async () => {
  const doneWhen = "sleep 30";
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: { ...FAST, reproductionMs: 2_000 },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane)),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoint(doneWhen)(seat, candidate, state),
  });
  try {
    const started = Date.now();
    await setup.conductor.start();
    const finding = () => setup.conductor.state.phase.findings.find((f) => f.doneWhen === doneWhen);
    await waitFor(() => coreEvents(setup.runDir).some((e) => e.type === "FINDING_FACT" && e.findingId === finding()?.id), 150_000, 50, setup.runDir);
    // The deadline is 2 s; the test must finish well inside its own limit, so
    // the 30 s sleep really was killed.
    assert.ok(Date.now() - started < 150_000, "the test finished well under its limit");
    const fact = coreEvents(setup.runDir).find((e) => e.type === "FINDING_FACT" && e.findingId === finding()!.id)!;
    assert.equal(fact.timedOut, true, "the fact records the timeout");
    assert.equal(finding()!.status, "open", "a timed-out fact repairs nothing");
    assert.ok(!coreEvents(setup.runDir).some((e) => e.type === "FINDING_CONFIRMED_REPAIRED"), "nothing repairs it");
  } finally {
    await teardown(setup);
  }
});

test("a doneWhen that passes only on the lane that is not picked repairs nothing", async () => {
  const doneWhen = "test -f fixed-a";
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    // Round 2: only lane b creates fixed-a; lane a is picked.
    laneWorkerScriptFor: (lane, round) =>
      round === 1 ? laneWorker(round, lane) : lane === "a" ? laneWorker(round, lane) : laneWorker(round, lane, ["fixed-a"]),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoint(doneWhen)(seat, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat, state.phase.rounds?.length ?? 1, "a"),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.rounds?.find((r) => r.round === 2)?.picked?.lane === "a",
      150_000,
      50,
      setup.runDir,
    );
    const round2 = setup.conductor.state.phase.rounds!.find((r) => r.round === 2)!;
    const laneASha = round2.candidates.find((c) => c.lane === "a")!.sha;
    const laneBSha = round2.candidates.find((c) => c.lane === "b")!.sha;
    await waitFor(() => coreEvents(setup.runDir).some((e) => e.type === "FINDING_FACT" && e.candidateSha === laneASha), 150_000, 50, setup.runDir);
    const events = coreEvents(setup.runDir);
    assert.ok(
      !events.some((e) => e.type === "FINDING_FACT" && e.candidateSha === laneBSha),
      "no FINDING_FACT names the losing lane's sha",
    );
    assert.ok(!events.some((e) => e.type === "FINDING_CONFIRMED_REPAIRED"), "a losing lane's passing fact repairs nothing");
    assert.equal(setup.conductor.state.phase.findings.find((f) => f.doneWhen === doneWhen)!.status, "open");
  } finally {
    await teardown(setup);
  }
});

test("a doneWhen runs in the checked checkout with the check environment", async () => {
  const doneWhen = 'test -f lane-a-r2.txt && test -n "$CARGO_TARGET_DIR"';
  const priorTarget = process.env.CARGO_TARGET_DIR;
  process.env.CARGO_TARGET_DIR = "/tmp/tt-done-when-target";
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane)),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoint(doneWhen)(seat, candidate, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.doneWhen === doneWhen && f.status === "repaired"),
      150_000,
      50,
      setup.runDir,
    );
    const finding = setup.conductor.state.phase.findings.find((f) => f.doneWhen === doneWhen)!;
    const fact = coreEvents(setup.runDir).find((e) => e.type === "FINDING_FACT" && e.findingId === finding.id)!;
    assert.equal(fact.exitCode, 0, "the probe of the checkout and the check env exits 0");
    assert.equal(fact.timedOut, false);
  } finally {
    await teardown(setup);
    if (priorTarget === undefined) delete process.env.CARGO_TARGET_DIR;
    else process.env.CARGO_TARGET_DIR = priorTarget;
  }
});

test("a round resumed after its check reads the persisted facts instead of running them again", async () => {
  const counter = `/tmp/tt-done-when-counter-${process.pid}`;
  fs.rmSync(counter, { force: true });
  const doneWhen = `echo run >> ${counter}`;
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane)),
    // The first conductor's reviews are slow, so the stop lands after round
    // 2's check (the fact has run) but while its reviews still run.
    laneReviewerScriptFor: (seat, candidate, state) => {
      const review = reviewerWithPoint(doneWhen)(seat, candidate, state);
      review.steps.splice(2, 0, { kind: "sleep", ms: 5_000 });
      return review;
    },
  });
  let resumed: Conductor | undefined;
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.rounds?.find((r) => r.round === 2)?.candidates.some((c) => c.ok === true) === true,
      150_000,
      50,
      setup.runDir,
    );
    await setup.conductor.stop();
    const counterLines = () => fs.readFileSync(counter, "utf8").split("\n").filter((l) => l.length > 0);
    assert.equal(counterLines().length, 1, "the doneWhen ran once before the stop");
    assert.ok(!coreEvents(setup.runDir).some((e) => e.type === "FINDING_FACT"), "no fact is recorded before the hand-off");

    resumed = resumeConductor(setup);
    await resumed.start();
    await waitFor(
      () => resumed!.state.phase.findings.some((f) => f.doneWhen === doneWhen && f.status === "repaired"),
      150_000,
      50,
      setup.runDir,
    );
    assert.equal(counterLines().length, 1, "the resumed round did not run the doneWhen again");
    const facts = coreEvents(setup.runDir).filter((e) => e.type === "FINDING_FACT");
    assert.equal(facts.length, 1, "the fact is recorded exactly once");
  } finally {
    if (resumed) await resumed.stop();
    await teardown(setup);
    fs.rmSync(counter, { force: true });
  }
});

test("the evaluator prompt lists the picked candidate's done-when facts", async () => {
  const doneWhen = "test -f fixed-a";
  const promptDir = fs.mkdtempSync("/tmp/tt-07b-prompt-");
  const promptLog = path.join(promptDir, "prompts.txt");
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane, round === 1 ? [] : ["fixed-a"])),
    // Round 1 raises the point the fact repairs. Round 2 raises a SECOND,
    // advisory point so a raw finding message exists on the candidate the
    // facts belong to: without a raw message the evaluator does not run and
    // there is no prompt to list the facts in.
    laneReviewerScriptFor: (seat, candidate, state) => {
      const K = state.phase.contract.contractVersion;
      const round = state.phase.rounds?.length ?? 1;
      if (round === 2 && seat === "M") {
        return laneReview("M", K, [
          {
            kind: "defect",
            severity: "advisory",
            evidence: "a second point",
            domain: "gates",
            item: "R2",
            where: "test/x.ts:1 fn",
            expected: "x",
            doneWhen: "true",
          },
        ]);
      }
      return reviewerWithPoint(doneWhen)(seat, candidate, state);
    },
  });
  // Every evaluator type carries the facts section, so match all of them.
  const evaluatorPrompts = (log: string) =>
    fs.readFileSync(log, "utf8").split("\n=====\n").filter((p) => /You are the \w+ evaluator for phase/.test(p));
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.doneWhen === doneWhen && f.status === "repaired"),
      150_000,
      50,
      setup.runDir,
    );
    const finding = setup.conductor.state.phase.findings.find((f) => f.doneWhen === doneWhen)!;
    // The evaluator of the round that recorded the fact runs after the
    // hand-off; wait for its prompt to carry the section.
    await waitFor(() => {
      try {
        return fs.readFileSync(promptLog, "utf8").includes("Done-when facts");
      } catch {
        return false;
      }
    }, 150_000, 50, setup.runDir);
    const prompts = evaluatorPrompts(promptLog);
    assert.ok(prompts.length > 0, "the evaluator was prompted");
    assert.ok(
      prompts.some((p) => p.includes("Done-when facts") && p.includes(finding.id) && p.includes(doneWhen) && p.includes("exit 0")),
      `the evaluator prompt lists the fact: ${prompts.map((p) => p.slice(-500)).join("\n---\n")}`,
    );
  } finally {
    await teardown(setup);
    fs.rmSync(promptDir, { recursive: true, force: true });
  }

  // A run without domains has no such section.
  const noDomainDir = fs.mkdtempSync("/tmp/tt-07b-nodomain-");
  const noDomainLog = path.join(noDomainDir, "prompts.txt");
  const noDomain = await setupConductor({
    phase: lanePhaseNoDomains(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraEnv: { FAKE_PI_PROMPT_LOG: noDomainLog },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane)),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoint(doneWhen)(seat, candidate, state),
  });
  try {
    await noDomain.conductor.start();
    await waitFor(() => {
      try {
        return /You are the \w+ evaluator for phase/.test(fs.readFileSync(noDomainLog, "utf8"));
      } catch {
        return false;
      }
    }, 150_000, 50, noDomain.runDir);
    const prompts = evaluatorPrompts(noDomainLog);
    assert.ok(prompts.length > 0, "the evaluator was prompted without domains");
    assert.ok(prompts.every((p) => !p.includes("Done-when facts")), "a run without domains has no facts section");
  } finally {
    await teardown(noDomain);
    fs.rmSync(noDomainDir, { recursive: true, force: true });
  }
});

test("a plan without TT_DOMAINS runs no doneWhen and records no FINDING_FACT", async () => {
  const doneWhen = "test -f fixed-a";
  const setup = await setupConductor({
    phase: lanePhaseNoDomains(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane, round === 1 ? [] : ["fixed-a"])),
    // The reviewer sends a doneWhen, but without TT_DOMAINS the field is
    // ignored: no finding carries it, so no fact can run.
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoint(doneWhen)(seat, candidate, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.rounds?.find((r) => r.round === 2)?.candidates.some((c) => c.ok === true) === true,
      150_000,
      50,
      setup.runDir,
    );
    const finding = setup.conductor.state.phase.findings.find((f) => f.evidence === "the point is not fixed");
    assert.ok(finding, "the reviewer's point was raised");
    assert.equal(finding!.doneWhen, undefined, "the doneWhen field is ignored outside domain mode");
    assert.ok(!coreEvents(setup.runDir).some((e) => e.type === "FINDING_FACT"), "no FINDING_FACT is recorded");
    assert.ok(!readEvents(setup.runDir).some((r) => r.kind === "done_when_fact"), "no doneWhen runs");
  } finally {
    await teardown(setup);
  }
});
