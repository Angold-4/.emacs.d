// Plan 07f (F0/F1): acceptance is mechanical by default. An evaluator session
// starts only for the records that may change scope, behaviour or cost:
// split items, points without a proving fact, and unclassified records.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer } from "../../src/core/types.ts";
import { acceptanceBlockers, shortId } from "../../src/core/predicate.ts";
import type { Domain } from "../../src/core/domains.ts";
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
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 15_000,
  reviewMs: 20_000,
  panelMs: 10_000,
  reproductionMs: 20_000,
};

const WRITE_ROUNDS = "mkdir -p src/core && printf 'interface Round { n: number }\\n' > src/core/rounds.ts";

const ITEM = (id: string) => ({ id, title: `${id} proves it`, text: `${id} proves it`, arch: [], verify: ["review"] });

const COVERAGE = (id: string) => ({
  items: [{ id, status: "done", where: ["src/core/rounds.ts:1"], tests: [] }],
  arch: [],
});

function workerFor(id: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: WRITE_ROUNDS },
      { kind: "call-submit", tool: "submit_coverage", args: COVERAGE(id) },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** Real two-turn review with the item verdict for `id`. The evidence cites
 * the candidate file AND README.md, so the met verdict is not thin. */
function reviewerFor(id: string, verdicts: Record<string, string> = {}) {
  return (reviewer: Reviewer, state: { phase: { candidate?: { sha: string }; contract: { contractVersion: unknown } } }) => ({
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
      { kind: "call-tool", tool: "read", args: { path: "README.md" } },
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
          items: [{ id, verdict: verdicts[reviewer] ?? "met", evidence: "src/core/rounds.ts:1 README.md:1" }],
          arch: [],
        },
      },
    ],
  });
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

test("a candidate with green checks passed tests fact-closed points and a unanimous seat majority is accepted with no evaluator session", async () => {
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")], finalChecks: ["sleep 2"] } as never,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => workerFor("R1"),
    reviewerScriptFor: reviewerFor("R1"),
  });
  try {
    await setup.conductor.start();
    // The final check sleeps, so FINAL_CHECKING is observable. Reaching it
    // means the phase left EVALUATING with no evaluator session.
    await waitFor(() => setup.conductor.state.phase.phase === "FINAL_CHECKING", 90_000, 20, setup.runDir);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const records = readEvents(setup.runDir);
    const events = records.filter((r) => r.kind === "event").map((r) => r.event as { type?: string });
    assert.ok(
      events.some((e) => e.type === "EVALUATION_SKIPPED"),
      "no exception means no evaluator, recorded as EVALUATION_SKIPPED",
    );
    assert.equal(
      records.filter((r) => r.kind === "intent" && (r.event as { agentId?: string }).agentId?.startsWith("evaluator-")).length,
      0,
      "no evaluator agent was started",
    );
    assert.equal(setup.conductor.state.phase.acceptedWithCarried, undefined, "the mechanical verdict accepted it");
  } finally {
    await teardown(setup);
  }
});

test("a split item starts the evaluator with only that item in its brief", async () => {
  const promptLog = path.join(fs.mkdtempSync("/tmp/tt-07f-prompt-"), "prompts.log");
  const setup = await setupConductor({
    items: { requirements: [ITEM("R2")] },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    extraEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => workerFor("R2"),
    reviewerScriptFor: reviewerFor("R2", { B: "unmet" }),
    evaluatorScriptFor: (messageType) =>
      messageType === "finding"
        ? {
            hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
            steps: [
              {
                kind: "call-submit",
                tool: "submit_evaluation",
                args: {
                  evaluations: [],
                  itemChecks: [{ id: "R2", verdict: "contradicted", evidence: "src/core/rounds.ts:1 contradicts R2" }],
                },
              },
            ],
          }
        : { hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator }, steps: [{ kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [] } }] },
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () =>
        readEvents(setup.runDir).some(
          (r) => r.kind === "intent" && (r.event as { agentId?: string }).agentId?.startsWith("evaluator-finding-"),
        ),
      90_000,
      20,
      setup.runDir,
    );
    await waitFor(
      () =>
        readEvents(setup.runDir).some(
          (r) =>
            r.kind === "event" &&
            (r.event as { type?: string; overturns?: Array<{ id: string }> }).type === "ITEM_STATE_UPDATED" &&
            ((r.event as { overturns?: Array<{ id: string }> }).overturns ?? []).some((o) => o.id === "R2"),
        ),
      90_000,
      20,
      setup.runDir,
    );
    const prompt = fs.existsSync(promptLog) ? fs.readFileSync(promptLog, "utf8") : "";
    assert.match(prompt, /Plan-item re-check:/, "the evaluator brief carries the item re-check");
    assert.match(prompt, /- R2 R2 proves it: met \(the seats split on it\)/, "it names the split item R2");
    assert.doesNotMatch(prompt, /\bR1\b/, "no other item is in the brief");
    assert.doesNotMatch(prompt, /\bF-\d+\b/, "no point is in the brief");
    assert.ok(
      readEvents(setup.runDir).some((r) => r.kind === "event" && (r.event as { type?: string }).type === "ITEM_STATE_UPDATED"),
      "the evaluator's ruling is recorded as the item's overturn",
    );
  } finally {
    await teardown(setup);
  }
});

// -- plan 07f (F1/R5): a failing doneWhen fact cannot be closed by a drop ----

const DOMAINS: Domain[] = [
  { name: "booking", globs: ["src/**"] },
  { name: "gates", globs: ["test/**"] },
];

function lanePhase() {
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
  };
}

function laneWorker(round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'x\\n' > lane-${lane}-r${round}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

function crashedLane(): { hello: unknown; steps: FakePiStep[] } {
  return { hello: defaultWorkerHello(), steps: [{ kind: "crash", code: 9 }] };
}

function laneReview(seat: Reviewer, contractVersion: unknown, findings: unknown[] = []) {
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

function reviewerWithPoint(doneWhen: string) {
  return (seat: Reviewer, _candidate: string, state: { phase: { contract: { contractVersion: unknown }; rounds?: unknown[] } }) => {
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

/** Round 1 publishes the point (so its message is not dropped before a
 * fact can exist). Round 2 classifies it as a judgement trade-off — the
 * evaluator's own approval — while its doneWhen fact still fails. */
function pointEvaluator(state: { phase: { round?: number; findings: Array<{ id: string; doneWhen?: string }> } }) {
  const round = state.phase.round ?? 1;
  const findingId = state.phase.findings.find((f) => f.doneWhen)?.id ?? "F-1";
  if (round >= 2) {
    return {
      hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
      steps: [{ kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [] } }],
    };
  }
  return {
    hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
    steps: [
      {
        kind: "call-submit",
        tool: "submit_evaluation",
        args: {
          evaluations: [
            {
              messageId: "F-1",
              action: "publish",
              title: "the point is not fixed",
              summary: "the point is not fixed",
              context: "the reviewer raised it against the lane candidate",
              evidence: ["src/core/rounds.ts:1"],
              importance: "high",
            },
          ],
          itemChecks: [
            {
              id: findingId,
              verdict: "confirmed",
              impact: "judgement",
              chosen: "accept the point as a trade-off",
              alternative: "fix it now",
              why: "the evaluator judged the remaining work a style call",
              evidence: "src/core/rounds.ts:1",
            },
          ],
        },
      },
    ],
  };
}

test("an evaluator ruling cannot close a point whose doneWhen fact fails on the candidate", async () => {
  const doneWhen = "test -f never-fixed";
  const setup = await setupConductor({
    phase: lanePhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane, round) => (lane === "b" ? crashedLane() : laneWorker(round, lane)),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoint(doneWhen)(seat, candidate, state),
    evaluatorScriptFor: (_messageType, state) => pointEvaluator(state),
  } as never);
  try {
    await setup.conductor.start();
    // Round 2's check runs the (still open) point's doneWhen, and it fails.
    await waitFor(
      () =>
        readEvents(setup.runDir).some(
          (r) =>
            r.kind === "event" &&
            (r.event as { type?: string; findingId?: string }).type === "FINDING_FACT" &&
            (r.event as { exitCode?: number | null }).exitCode !== 0,
        ),
      150_000,
      50,
      setup.runDir,
    );
    const finding = setup.conductor.state.phase.findings.find((f) => f.doneWhen === doneWhen)!;
    const fact = (setup.conductor.state.phase.doneWhenFacts ?? []).find((f) => f.findingId === finding.id)!;
    assert.notEqual(fact.exitCode, 0, "the fact on the candidate failed");
    assert.equal(finding.status, "open", "the failing fact keeps the point open despite the evaluator's trade-off ruling");
    const listed = acceptanceBlockers(setup.conductor.state.phase);
    assert.ok(
      listed.some((b) => b.kind === "point" && b.detail.includes(shortId(finding.id))),
      `acceptanceBlockers names the still-open point: ${JSON.stringify(listed)}`,
    );
    // The same point with its message recorded `dropped` (the evaluator's
    // literal drop entry) is still an acceptance blocker: a drop cannot
    // close a fact-failing point either.
    const dropped = {
      ...setup.conductor.state.phase,
      messages: (setup.conductor.state.phase.messages ?? []).map((m) =>
        m.sourceRecordId === finding.id ? { ...m, state: "dropped" as const } : m,
      ),
    };
    assert.ok(
      acceptanceBlockers(dropped).some((b) => b.kind === "point" && b.detail.includes(shortId(finding.id))),
      "a dropped message does not clear the point blocker",
    );
  } finally {
    await teardown(setup);
  }
});
