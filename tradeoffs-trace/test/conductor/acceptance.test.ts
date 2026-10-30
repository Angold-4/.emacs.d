// Plan 07f (F0/F1): acceptance is mechanical by default. An evaluator session
// starts only for the records that may change scope, behaviour or cost:
// split items, points without a proving fact, and unclassified records.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer } from "../../src/core/types.ts";
import { accept, acceptanceBlockers, shortId } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import { ownerCommandToEvent } from "../../src/core/owner-inbox.ts";
import { runPaths } from "../../src/conductor.ts";
import { CV, basePhase, baseState, makeDecision } from "../unit/helpers.ts";
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
          // 07g2 (R6): a seat that neither affirms nor denies the item votes
          // `partial`; that is the vote that leaves R2 without a strict
          // majority. (A seat may not omit the verdict: a review that omits an
          // item is refused.)
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

async function candidateWithoutExceptions(): Promise<void> {
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
}

// Keep the existing regression names as well as the phase's named verifies.
// Each registration runs the full scenario in its own conductor setup.
test("a candidate with green checks passed tests fact-closed points and a unanimous seat majority is accepted with no evaluator session", candidateWithoutExceptions);
test("a candidate with no exception is decided with no evaluator session", candidateWithoutExceptions);

async function itemWithoutSeatMajority(): Promise<void> {
  const promptLog = path.join(fs.mkdtempSync("/tmp/tt-07f-prompt-"), "prompts.log");
  const setup = await setupConductor({
    items: { requirements: [ITEM("R2")] },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    extraEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => workerFor("R2"),
    reviewerScriptFor: reviewerFor("R2", { A: "partial", B: "unmet" }),
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
    // 07g2 (R6): with M met, B unmet and A partial there is no strict
    // majority of the votes cast, so R2 is an exception; the brief re-checks it
    // as the split item and the evaluator's contradiction is recorded against
    // it. The old expectation that the ruling OVERTURNS a met majority is gone:
    // no evaluator ruling flips a verdict a majority gave.
    await waitFor(
      () => (setup.conductor.state.phase.itemChecks ?? []).some((c) => c.itemId === "R2" && c.verdict === "contradicted"),
      90_000,
      20,
      setup.runDir,
    );
    const prompt = fs.existsSync(promptLog) ? fs.readFileSync(promptLog, "utf8") : "";
    assert.match(prompt, /Plan-item re-check:/, "the evaluator brief carries the item re-check");
    assert.match(prompt, /- R2 R2 proves it: .*\(the seats split on it\)/, "it names the split item R2");
    assert.doesNotMatch(prompt, /\bR1\b/, "no other item is in the brief");
    assert.doesNotMatch(prompt, /\bF-\d+\b/, "no point is in the brief");
    assert.ok(
      (setup.conductor.state.phase.itemChecks ?? []).some((c) => c.itemId === "R2" && c.verdict === "contradicted"),
      "the evaluator's ruling is recorded as R2's item check",
    );
  } finally {
    await teardown(setup);
  }
}

test("a split item starts the evaluator with only that item in its brief", itemWithoutSeatMajority);
test("an item with no seat majority starts the evaluator with only that item in its brief", itemWithoutSeatMajority);

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

// -- R1: one blocker list; the ruling and resolve commands clear their two ---

test("every acceptance blocker carries its kind detail and next command", () => {
  const C = "C1";
  const K = CV();
  const contract = {
    ...basePhase().contract,
    acceptance: ["R1 works"],
    architecture: [],
    itemsSynthesized: false,
    requirements: [{ id: "R1", title: "R1", text: "R1 works", arch: [], verify: ['test "R1 works"'] }],
    constraints: [],
  };
  const review = (seat: string, verdict: string) => ({
    reviewer: seat,
    phaseId: "p1",
    candidateSha: C,
    contractVersion: K,
    correctionStatements: [],
    findingStatements: [],
    items: [{ id: "R1", verdict, evidence: "README.md:1 README.md:2" }],
    arch: [],
  });
  const point = (id: string) => ({
    id,
    version: 1,
    phaseId: "p1",
    kind: "defect" as const,
    severity: "advisory" as const,
    evidence: "README.md:1",
    raisedBy: "M" as const,
    status: "open" as const,
    boundCandidateSha: C,
    domain: "booking",
    doneWhen: "test -f done",
  });
  const phase = basePhase({
    contract,
    phase: "RESOLVING",
    candidate: { sha: C, contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: C, passed: false },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    checkResolution: [{ id: "R1", name: "R1 works", outcome: "missing" }],
    reviews: { M: { review: review("M", "met") }, A: { review: review("A", "met") }, B: { review: review("B", "met") } },
    decisions: [makeDecision({ id: "D-1", class: "reserved", boundCandidateSha: C, boundContractVersion: K })],
    findings: [point("F-1"), point("F-2")],
    doneWhenFacts: [
      { findingId: "F-1", candidateSha: C, exitCode: 1, timedOut: false, outputTail: "not done" },
      { findingId: "F-2", candidateSha: C, exitCode: 1, timedOut: false, outputTail: "not done" },
    ],
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "the booking point needs your ruling",
        origin: "open_finding" as const,
        linkedFindingId: "F-1",
        boundCandidateSha: C,
        boundContractVersion: K,
        options: [
          { id: "accept_risk", label: "accept the risk" },
          { id: "repair", label: "repair it" },
        ],
        status: "open" as const,
      },
    ],
  });

  const blockers = acceptanceBlockers(phase);
  assert.equal(blockers.length, 5, `one blocker for each unmet condition, got ${JSON.stringify(blockers, null, 2)}`);
  assert.deepEqual(blockers.map((b) => b.kind), ["check", "test-verify", "point", "decision", "owner-request"]);
  for (const blocker of blockers) {
    assert.ok(blocker.detail.length > 0, `${blocker.kind} carries its detail`);
    assert.ok(blocker.command.length > 0, `${blocker.kind} carries the command that clears it`);
  }
  assert.match(blockers.find((b) => b.kind === "decision")!.command, /"type":"ruling"/, "a decision is cleared by a tt input ruling");
  assert.match(blockers.find((b) => b.kind === "owner-request")!.command, /"type":"resolve"/, "a request is cleared by a tt input resolve");

  // The two owner commands clear exactly their own blockers: tt input maps a
  // `ruling` to RULING_RECORDED and a `resolve` to OWNER_REQUEST_RESOLVED.
  const ruling = ownerCommandToEvent({ kind: "ruling", text: "rule D-1 as recorded", recordIds: ["D-1"] }, "cmd-rule");
  assert.ok(ruling, "the ruling command maps to a conductor event");
  const ruled = reduce(baseState(phase), ruling!);
  assert.ok(ruled.ok, ruled.ok ? "" : ruled.reason);
  assert.ok(!acceptanceBlockers(ruled.state.phase).some((b) => b.kind === "decision"), "the ruling clears the decision blocker");

  const resolve = ownerCommandToEvent(
    { kind: "resolve", requestId: "OR-1", requestVersion: 1, option: "repair", boundCandidateSha: C, boundContractVersion: K },
    "cmd-resolve",
  );
  assert.ok(resolve, "the resolve command maps to a conductor event");
  const resolved = reduce(ruled.state, resolve!);
  assert.ok(resolved.ok, resolved.ok ? "" : resolved.reason);
  const remaining = acceptanceBlockers(resolved.state.phase);
  assert.ok(!remaining.some((b) => b.kind === "owner-request"), "the resolve clears the owner-request blocker");
  assert.deepEqual(remaining.map((b) => b.kind), ["check", "test-verify", "point"], "only the non-owner blockers remain");
});

// -- R2: the owner's ruling closes the open point, and the phase accepts ------

/** Writes an owner ruling to the live run's inbox. This is owner input, not a
 * hand-injected core event: the conductor polls the inbox and applies the
 * RULING_RECORDED itself. */
function sendRulingFile(setup: TestConductorSetup, recordIds: string[], text: string): void {
  const phase = setup.conductor.state.phase;
  const file = path.join(runPaths(setup.runDir).inbox, `test-ruling-${Date.now()}-${Math.random().toString(16).slice(2)}.json`);
  fs.writeFileSync(file, JSON.stringify({ type: "ruling", text, recordIds, binding: { runId: phase.runId, phaseId: phase.phaseId } }));
}

test("a ruling that names an open point closes it and the phase accepts", async () => {
  const evidence = "P-OPEN: the open point the owner rules on";
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")] } as never,
    domains: DOMAINS,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => workerFor("R1"),
    reviewerScriptFor: (reviewer: Reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        // M publishes the point first; A and B stay in review long enough for
        // the owner's ruling to land before triage. Every seat still meets
        // the phase's one item, so the majority is not in question.
        ...(reviewer === "M" ? [] : [{ kind: "sleep" as const, ms: 2_500 }]),
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
            items: [{ id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1 README.md:1" }],
            arch: [],
            findings:
              reviewer === "M"
                ? [
                    {
                      kind: "defect",
                      severity: "blocking",
                      evidence,
                      domain: "booking",
                      item: "R1",
                      where: "src/core/rounds.ts:1 fn",
                      expected: "the open point is settled",
                      doneWhen: "test -f never-settled",
                    },
                  ]
                : [],
          },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.evidence === evidence && f.status === "open"),
      90_000,
      20,
      setup.runDir,
    );
    const point = setup.conductor.state.phase.findings.find((f) => f.evidence === evidence)!;
    const beforeBlockers = acceptanceBlockers(setup.conductor.state.phase);
    assert.ok(
      beforeBlockers.some((b) => b.kind === "point" && b.detail.includes(shortId(point.id))),
      `the open point is an acceptance blocker before the ruling; point=${JSON.stringify(point)} phase=${setup.conductor.state.phase.phase} blockers=${JSON.stringify(beforeBlockers)}`,
    );

    const rulingText = `Rule ${point.id} settled: the point is accepted as ruled.`;
    sendRulingFile(setup, [point.id], rulingText);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 20, setup.runDir);

    const phase = setup.conductor.state.phase;
    const closed = phase.findings.find((f) => f.id === point.id)!;
    assert.notEqual(closed.status, "open", "the point the ruling names is no longer open");
    assert.equal(closed.status, "accepted", "a ruled point is settled, not left as a repair point");
    const ruling = phase.rulings?.find((r) => r.text === rulingText && r.recordIds.includes(point.id));
    assert.ok(ruling, "the ruling is on the record");
    assert.equal(closed.acceptedScope, ruling!.id, "the ruling id is the point's recorded reason");
    assert.equal(phase.round, 1, "the phase accepted in the round the ruling arrived, not after another one");
    assert.equal(phase.phase, "DONE");
    for (const seat of ["M", "A", "B"]) {
      const item = phase.reviews[seat]?.review?.items?.find((i) => i.id === "R1");
      assert.equal(item?.verdict, "met", `seat ${seat} meets R1; the only block was the ruled point`);
    }
    assert.equal(phase.findings.filter((f) => f.evidence === evidence).length, 1, "the fixture had exactly one open point P");
    assert.equal(accept(phase, phase.candidate!.sha, phase.contract.contractVersion), true, "every item is met and no blocker remains");
    assert.ok(!acceptanceBlockers(phase).some((b) => b.kind === "point"), "the ruled point is not an acceptance blocker");
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "REPAIR_ATTEMPT_STARTED").length,
      0,
      "no repair round was started for a ruled point",
    );
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "DECISION_CLOSED").length,
      0,
      "no DECISION_CLOSED event was needed or injected",
    );
  } finally {
    await teardown(setup);
  }
});

// -- R3: C1/v1's amendment is closed by the conductor before C3/v3 accepts ---

test("a decision bound to an older contract version never blocks acceptance", async () => {
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")] } as never,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt, { state }) => {
      const criterion = state.phase.contract.requirements?.find((r) => r.id === "R1")?.text ?? "R1 proves it";
      return {
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-sh", command: `printf 'attempt ${attempt}\\n' > attempt.txt` },
          { kind: "call-submit", tool: "submit_coverage", args: { items: [{ id: "R1", status: "done", where: ["attempt.txt:1"], tests: [] }], arch: [] } },
          {
            kind: "call-submit",
            tool: "submit_phase",
            args: {
              decisions: [],
              assumptions: [],
              deviations: [],
              ...(attempt <= 2
                ? {
                    criterionDispute: {
                      criterion,
                      why: `the wording under v${attempt} cannot be proven mechanically`,
                      proposedWording: `R1 proves it v${attempt + 1}`,
                    },
                  }
                : {}),
            },
          },
        ],
      };
    },
    reviewerScriptFor: (reviewer: Reviewer, state) => {
      const votable = state.phase.decisions.filter(
        (d) => d.amendment?.status === "proposed" && d.supersededBy === undefined,
      );
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "attempt.txt" } },
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
              ballots: votable.map((d) => ({
                decisionId: d.id,
                vote: "approve",
                rationale: "the new wording is satisfiable and still meaningful",
                evidence: ["attempt.txt:1"],
              })),
              items: [{ id: "R1", verdict: "met", evidence: "attempt.txt:1 README.md:1" }],
              arch: [],
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 20, setup.runDir);

    const phase = setup.conductor.state.phase;
    const events = readEvents(setup.runDir)
      .filter((r) => r.kind === "event")
      .map((r) => r.event as { type?: string; candidateSha?: string; decisionId?: string; reason?: string; decisions?: Array<{ id: string; boundCandidateSha: string; boundContractVersion: { snapshot: number }; amendment?: { status: string } }> });
    const freezes = events.filter((e) => e.type === "FREEZE_COMPLETED");
    assert.equal(freezes.length, 3, "C1/v1, C2/v2 and C3/v3 each freezed once");
    const first = freezes[0];
    const c1 = first.candidateSha!;
    const firstAmendment = first.decisions!.find((d) => d.amendment !== undefined)!;
    assert.equal(firstAmendment.boundCandidateSha, c1, "the amendment was raised on candidate C1");
    assert.equal(firstAmendment.boundContractVersion.snapshot, 1, "under contract v1");
    assert.notEqual(phase.candidate!.sha, c1, "the accepted candidate is a later one");
    assert.equal(phase.candidate!.contractVersion.snapshot, 3, "the accepted candidate is judged under v3");

    const amendment = phase.decisions.find((d) => d.id === firstAmendment.id)!;
    const reason = `the phase moved past candidate ${c1.slice(0, 8)}`;
    assert.equal(amendment.supersededBy, reason, "the conductor closed it with its reason");
    assert.ok(
      events.some((e) => e.type === "DECISION_CLOSED" && e.decisionId === firstAmendment.id && e.reason === reason),
      "the closure is a conductor-recorded DECISION_CLOSED event, not a hand-injected one",
    );
    for (const seat of ["M", "A", "B"]) {
      const item = phase.reviews[seat]?.review?.items?.find((i) => i.id === "R1");
      assert.equal(item?.verdict, "met", `seat ${seat} meets R1 under v3`);
    }
    assert.equal(phase.contract.requirements?.find((r) => r.id === "R1")?.text, "R1 proves it v3", "the accepted contract is the twice-amended v3 wording");
    assert.equal(accept(phase, phase.candidate!.sha, phase.contract.contractVersion), true, "C3/v3 is fully accepted");
    assert.ok(!acceptanceBlockers(phase).some((b) => b.kind === "decision"), "the closed amendment is not a blocker");
  } finally {
    await teardown(setup);
  }
});
