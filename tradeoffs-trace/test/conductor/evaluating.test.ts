// Plan 04a: BASELINE and EVALUATING are real states of the phase machine.
//
//  - the base baseline is its own stage (never implement), with no worker
//    dispatched while it runs, and the worker's attempt deadline starts at
//    its launch after it;
//  - an identical base tree whose baseline is already recorded skips it;
//  - acceptance cannot outrun evaluation: the phase stays EVALUATING until
//    EVALUATION_COMPLETED or EVALUATION_TIMED_OUT;
//  - an evaluation timeout publishes the raw messages unchanged, marked
//    `unevaluated`, and the phase still reaches RESOLVING;
//  - `raise_tradeoff` gives raw trade-offs with anchors, and the evaluator
//    merges/drops/publishes them into the ledger;
//  - the ledger and owner refusals reach the worker and reviewer prompts.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));
function tt(args: string[]): string {
  return execFileSync(process.execPath, [CLI, ...args], { encoding: "utf8" });
}

import {
  buildContract,
  buildWorkerPrompt,
  Conductor,
  contractVersionFor,
  createRun,
  ledgerPromptLines,
  refusedPromptLines,
  runPaths,
  type RunPlanFile,
} from "../../src/conductor.ts";
import { next } from "../../src/core/next.ts";
import { reduce } from "../../src/core/reduce.ts";
import { evaluationSettled, typesNeedingEvaluation } from "../../src/core/predicate.ts";
import { baseState, makeMessage } from "../unit/helpers.ts";
import { contentHashOf, projectLedger, projectMessages } from "../../src/core/messages.ts";
// The review view moved to src/render.ts in 03b; 04b wrote this test before the join.
import { projectReview } from "../../src/render.ts";
import { baselineKey } from "../../src/core/test-failures.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { buildView } from "../../src/view.ts";
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
} from "./harness.ts";

const FAST = {
  abortGraceMs: 200,
  termGraceMs: 200,
  helloTimeoutMs: 5_000,
  checkMs: 30_000,
  freezeMs: 15_000,
  workerAttemptMs: 30_000,
  reviewMs: 10_000,
};

function submitPhaseStep(decisions: unknown[] = []) {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions, assumptions: [], deviations: [] } };
}

function stubReviewer() {
  return (reviewer: Reviewer, state: State) => ({
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer,
          phaseId: state.phase.phaseId,
          candidateSha: state.phase.candidate?.sha,
          contractVersion: state.phase.contract.contractVersion,
          correctionStatements: [],
          findingStatements: [],
        },
      },
    ],
  });
}

function eventTypes(runDir: string): string[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => (r.event as { type: string }).type);
}

function git(repo: string, args: string[]): string {
  return execFileSync("git", ["-C", repo, ...args], { encoding: "utf8" }).trim();
}

test("plan 04a: BASELINE is its own stage, no worker runs during it, and the worker starts after it", async () => {
  const command = "sleep 3";
  const setup = await setupConductor({
    checks: [command],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: stubReviewer(),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "BASELINE", 30_000, 10, setup.runDir);
    assert.equal(
      setup.conductor.agentPgids.some((a) => a.role === "worker"),
      false,
      "no worker may be dispatched while the baseline runs",
    );
    const view = buildView(setup.runDir, setup.plan, true);
    assert.match(view.pipeline, /baseline \d+s…/, `the pipeline must count baseline as its own stage: ${view.pipeline}`);
    assert.ok(!/implement/.test(view.pipeline), "the baseline is never counted as implement");

    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const types = eventTypes(setup.runDir);
    assert.ok(types.includes("BASELINE_COMPLETED"), "the baseline completes before the worker runs");
    const baselineDone = readEvents(setup.runDir).find(
      (r) => r.kind === "event" && (r.event as { type: string }).type === "BASELINE_COMPLETED",
    )!;
    const firstWorkerAction = readEvents(setup.runDir).find(
      (r) => r.kind === "event" && (r.event as { action?: string }).action === "dispatch_worker",
    )!;
    assert.ok(
      Date.parse(baselineDone.ts) <= Date.parse(firstWorkerAction.ts),
      "the worker's dispatch must begin after BASELINE_COMPLETED",
    );
    // A-27: the worker's own attempt (its intent) also starts strictly after
    // the baseline, so its attempt deadline is measured from its launch.
    const workerIntent = readEvents(setup.runDir).find(
      (r) => r.kind === "intent" && String((r.event as { agentId?: string }).agentId ?? "").startsWith("worker-"),
    )!;
    assert.ok(
      Date.parse(baselineDone.ts) <= Date.parse(workerIntent.ts),
      "the worker attempt must start after the baseline, so its deadline runs from its launch",
    );
    assert.ok(!types.includes("BASELINE_TIMED_OUT"), "a 3s baseline against a 30s deadline must complete");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 04a: an identical base tree with a recorded baseline skips the BASELINE state", async () => {
  const command = "true";
  const setup = await setupConductor({
    checks: [command],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: stubReviewer(),
    deadlines: FAST,
  });
  // Record a baseline covering exactly this base tree and check list, as a
  // restart or a sibling would have left behind.
  const tree = git(setup.repo.dir, ["rev-parse", "HEAD^{tree}"]);
  const baseSha = git(setup.repo.dir, ["rev-parse", "HEAD"]);
  const key = baselineKey(tree, [command]);
  const dir = path.join(runPaths(setup.runDir).checks, "base");
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(
    path.join(dir, "baseline.json"),
    JSON.stringify({
      baseSha,
      tree,
      key,
      at: new Date().toISOString(),
      commands: [{ command, exitCode: 0, signal: null, timedOut: false, durationMs: 1, failures: [] }],
      failures: [],
    }),
  );
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000, 20, setup.runDir);
    assert.ok(!eventTypes(setup.runDir).includes("BASELINE_COMPLETED"), "the reuse rule must skip the state entirely");
    assert.ok(!eventTypes(setup.runDir).includes("BASELINE_TIMED_OUT"));
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

const DECISIONS = [
  {
    choice: "Use a simple loop rather than a library helper",
    whyItMatters: "Keeps the change dependency-free",
    alternatives: [{ option: "pull in a utility library", consequence: "adds a dependency" }],
    recommendation: { choice: "keep the loop", reason: "no dependency needed" },
    classProposal: "detail",
  },
];

test("plan 04a: acceptance cannot outrun evaluation — the phase stays EVALUATING until the evaluator settles", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep(DECISIONS)] }),
    reviewerScriptFor: stubReviewer(),
    // A slow evaluator: the phase must not accept while it runs.
    evaluatorScriptFor: () => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps: [
        { kind: "sleep", ms: 2000 },
        { kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [] } },
      ],
    }),
    // The empty submission above is refused (a raw message is uncovered), so
    // the trade-off type times out and publishes unevaluated; the phase still
    // may not accept before that.
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "EVALUATING", 60_000, 10, setup.runDir);
    // While the evaluator is running there must be no ACCEPTED.
    for (let i = 0; i < 5; i += 1) {
      assert.equal(setup.conductor.state.phase.phase, "EVALUATING");
      assert.ok(!eventTypes(setup.runDir).includes("ACCEPTED"), "acceptance must not outrun the evaluator");
      await new Promise((r) => setTimeout(r, 100));
    }
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000, 20, setup.runDir);
    const types = eventTypes(setup.runDir);
    assert.ok(
      types.indexOf("EVALUATION_COMPLETED") !== -1 &&
        types.indexOf("EVALUATION_COMPLETED") < types.indexOf("ACCEPTED"),
      "EVALUATION_COMPLETED must precede ACCEPTED",
    );
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 04a: an evaluation timeout publishes the raw messages unevaluated and the phase reaches RESOLVING", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep(DECISIONS)] }),
    reviewerScriptFor: stubReviewer(),
    evaluatorScriptFor: () => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps: [{ kind: "sleep", ms: 60_000 }],
    }),
    deadlines: { ...FAST, evaluateMs: 1200 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000, 20, setup.runDir);
    const types = eventTypes(setup.runDir);
    assert.ok(types.includes("EVALUATION_TIMED_OUT"), "the evaluator's deadline must take the timed-out path");
    const message = (setup.conductor.state.phase.messages ?? []).find((m) => m.type === "tradeoff")!;
    assert.equal(message.state, "published");
    assert.equal(message.unevaluated, true, "a message the evaluator never checked is published unevaluated");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 04a: raise_tradeoff anchors, the evaluator merges/drops/publishes, and all of it is in the ledger", async () => {
  const findings = [
    { kind: "defect", severity: "advisory", evidence: "src/a.ts:10 the loop can spin", linkedDecisionId: undefined },
    { kind: "defect", severity: "advisory", evidence: "src/a.ts:10 the loop can spin (same thing)" },
    { kind: "defect", severity: "advisory", evidence: "naming nit, trivial" },
  ].map(({ linkedDecisionId: _drop, ...f }) => f);
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "raise_tradeoff",
          args: {
            choice: "Batch cancels per tick",
            alternative: "a lock per request",
            why: "keeps the cancel path inside its latency budget",
            anchor: { path: "src/cancel.ts", lines: [10, 24] },
          },
        },
        {
          kind: "call-submit",
          tool: "raise_tradeoff",
          args: {
            choice: "Keep the existing file layout",
            alternative: "reorganize the module",
            why: "a smaller diff is easier to review",
            anchor: { path: "src/cancel.ts", lines: [1, 8] },
          },
        },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: {
            reviewer,
            phaseId: state.phase.phaseId,
            candidateSha: state.phase.candidate?.sha,
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
            ballots: [],
            findings: reviewer === "M" ? findings : [],
          },
        },
      ],
    }),
    // One evaluator per message type: the trade-off evaluator polishes the
    // two trade-offs, the finding evaluator merges/drops/publishes the three
    // findings.
    evaluatorScriptFor: (messageType) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations:
              messageType === "tradeoff"
                ? [
                    {
                      messageId: "T-1",
                      action: "publish",
                      title: "Batch cancels per tick to stay inside the latency budget",
                      summary: "The cancel path took one lock per request. Batching per tick keeps it inside budget.",
                      context: "src/cancel.ts:10-24 batches cancels instead of locking each one.",
                      evidence: ["src/cancel.ts:10-24"],
                      importance: "high",
                    },
                    {
                      messageId: "T-2",
                      action: "publish",
                      title: "Keep the existing file layout",
                      summary: "Reorganizing the module would only make the diff noisier.",
                      context: "src/cancel.ts:1-8 keeps the layout.",
                      evidence: ["src/cancel.ts:1-8"],
                      importance: "low",
                    },
                  ]
                : [
                    { messageId: "F-1", action: "publish", title: "The loop can spin on an empty input", summary: "A guard is missing.", context: "src/a.ts:10", evidence: ["src/a.ts:10"], importance: "medium" },
                    { messageId: "F-2", action: "merge", into: "F-1" },
                    { messageId: "F-3", action: "drop", reason: "a naming nit, not reviewable" },
                  ],
          },
        },
      ],
    }),
    deadlines: { ...FAST, reviewMs: 30_000, probeMs: 5_000 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const messages = setup.conductor.state.phase.messages ?? [];
    const t1 = messages.find((m) => m.id === "T-1")!;
    const t2 = messages.find((m) => m.id === "T-2")!;
    assert.equal(t1.state, "published");
    assert.deepEqual(t1.anchor, { path: "src/cancel.ts", lines: [10, 24] });
    assert.deepEqual(t2.anchor, { path: "src/cancel.ts", lines: [1, 8] });
    assert.ok(t1.title.length <= 80, `title must be at most 80 characters, got ${t1.title.length}`);
    assert.ok(t2.title.length <= 80);
    assert.equal(messages.find((m) => m.id === "F-1")!.state, "published");
    assert.equal(messages.find((m) => m.id === "F-2")!.state, "merged");
    assert.equal(messages.find((m) => m.id === "F-3")!.state, "dropped");

    // The settled ledger holds the evaluator's merges/drops; the published
    // trade-offs are live messages in messages.jsonl (they can still be
    // accepted or refused by the owner).
    const ledger = fs.readFileSync(runPaths(setup.runDir).ledger, "utf8");
    assert.match(ledger, /"messageId":"F-2","type":"finding","state":"merged"/);
    assert.match(ledger, /"messageId":"F-3","type":"finding","state":"dropped"/);
    const projected = fs.readFileSync(runPaths(setup.runDir).messages, "utf8");
    assert.match(projected, /"id":"T-1"/);
    assert.match(projected, /"id":"T-2"/);
    assert.match(projected, /"anchor":\{"path":"src\/cancel.ts","lines":\[10,24\]\}/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 04a: the ledger and owner refusals are in the prompt sections", () => {
  const message = {
    id: "T-1",
    phaseId: "p1",
    type: "tradeoff" as const,
    title: "Batch cancels per tick",
    summary: "why",
    context: "ctx",
    evidence: ["src/x.ts:1"],
    state: "accepted" as const,
    messageVersion: 1,
    boundCandidateSha: "C1",
    boundContractVersion: { snapshot: 1, sectionSha256: "a".repeat(64) },
    contentHash: "a".repeat(64),
    settlement: {
      state: "accepted" as const,
      settledBy: "owner" as const,
      candidateSha: "C1",
      contractVersion: { snapshot: 1, sectionSha256: "a".repeat(64) },
      messageVersion: 1,
      contentHash: "a".repeat(64),
    },
  };
  const ledger = ledgerPromptLines([message]).join("\n");
  assert.match(ledger, /Settled \(do not re-raise\)/);
  assert.match(ledger, /T-1 \[tradeoff\] accepted by owner/);

  const refused = { ...message, state: "refused" as const, settlement: { ...message.settlement, state: "refused" as const, reason: "not the trade-off the goal needed" } };
  const refusedSection = refusedPromptLines([refused]).join("\n");
  assert.match(refusedSection, /Owner-refused \(must address\)/);
  assert.match(refusedSection, /not the trade-off the goal needed/);

  const contract = buildContract({ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] });
  const prompt = buildWorkerPrompt(contract, undefined, undefined, undefined, [], [], undefined, [], [message]);
  assert.match(prompt, /Settled \(do not re-raise\)/);
});

test("plan 04a: the next reviewer turn-2 prompt carries the settled ledger", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-eval-ledger-");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep(DECISIONS)] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: {
            reviewer,
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
    }),
    extraReviewerEnv: (reviewer) => ({ FAKE_PI_PROMPT_LOG: path.join(dir, `${reviewer}.log`) }),
    deadlines: { ...FAST, reviewMs: 30_000, probeMs: 5_000 },
  });
  await setup.conductor.start();
  try {
    await waitFor(
      () =>
        (["M", "A", "B"] as Reviewer[]).every((r) => {
          const file = path.join(dir, `${r}.log`);
          return fs.existsSync(file) && fs.readFileSync(file, "utf8").includes("Turn 2 of 2");
        }),
      90_000,
      20,
      setup.runDir,
    );
    // The evaluator publishes the worker's decision as a trade-off only after
    // the reviews; the LEDGER is in every later prompt (a repair attempt or a
    // later round). Assert the section text is present in the turn-2 prompt's
    // own builder.
    const turn2 = fs.readFileSync(path.join(dir, "M.log"), "utf8");
    assert.ok(turn2.includes("Turn 2 of 2"));
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 04a: a refused-only round still dispatches that message type's evaluator", () => {
  const C1 = { sha: "C1", contractVersion: { snapshot: 1, sectionSha256: "a".repeat(64) } };
  const K = C1.contractVersion;
  const refused = makeMessage({
    state: "refused",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    settlement: { state: "refused", settledBy: "owner", reason: "not the trade-off the goal needed", candidateSha: "C1", contractVersion: K, messageVersion: 1, contentHash: "a".repeat(64) },
  });
  let state = baseState({
    phase: "EVALUATING",
    candidate: C1,
    messages: [refused],
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {},
  });
  assert.deepEqual(typesNeedingEvaluation(state.phase), ["tradeoff"]);
  assert.equal(evaluationSettled(state.phase), false);
  assert.deepEqual(next(state), [{ type: "dispatch_evaluation", messageType: "tradeoff" }]);
  const done = reduce(state, { type: "EVALUATOR_FINISHED", messageType: "tradeoff", evaluated: 0 });
  assert.equal(done.ok, true, !done.ok ? done.reason : "");
  state = done.state;
  assert.deepEqual(next(state), [{ type: "evaluation_complete" }]);
});

test("plan 04a: an addressed:false report is recorded, not silently dropped", () => {
  const K = { snapshot: 1, sectionSha256: "a".repeat(64) };
  const refused = makeMessage({
    state: "refused",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    settlement: { state: "refused", settledBy: "owner", reason: "still broken", candidateSha: "C1", contractVersion: K, messageVersion: 1, contentHash: "a".repeat(64) },
  });
  const state = baseState({ phase: "EVALUATING", candidate: { sha: "C1", contractVersion: K }, messages: [refused] });
  const result = reduce(state, {
    type: "MESSAGE_ADDRESS_REPORTED",
    messageId: "T-1",
    addressed: false,
    reason: "the candidate never touched the cancel path",
    at: "2026-01-01T00:00:00.000Z",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.messages?.[0].addressedReport?.addressed, false);
  // OD-2: reduce() copies the event's timestamp; it never reads a clock, so a
  // rebuild from events.jsonl is identical.
  assert.equal(result.state.phase.messages?.[0].addressedReport?.at, "2026-01-01T00:00:00.000Z");
  assert.equal(result.state.phase.messages?.[0].state, "refused", "the refusal still stands");
});

test("plan 04a: an owner refusal plus an addressed:false report rebuilds byte-identically (OD-2)", () => {
  // The exact proof OD-2 asks for, at the layer `tt contract check` uses:
  // fold the events live, project, fold the SAME events again from scratch and
  // project again. Byte-identical projections mean the check passes; a clock
  // (or randomness) inside reduce() would make them differ.
  const K = { snapshot: 1, sectionSha256: "a".repeat(64) };
  const content = { type: "tradeoff" as const, title: "A polished trade-off title", summary: "one sentence.", context: "src/cancel.ts:1", evidence: ["src/cancel.ts:1"] };
  const events = [
    { type: "MESSAGE_RAISED", message: makeMessage({ state: "raw", boundCandidateSha: "C1", boundContractVersion: K, ...content, contentHash: contentHashOf(content) }) },
    { type: "MESSAGE_PUBLISHED", messageId: "T-1", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1, content },
    { type: "OWNER_VERDICT", messageId: "T-1", verdict: "refuse", reason: "not the trade-off the goal needed", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
    { type: "MESSAGE_ADDRESS_REPORTED", messageId: "T-1", addressed: false, reason: "the candidate never touched the cancel path", at: "2026-01-02T00:00:00.000Z", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 },
  ];
  const fold = () => {
    let state = baseState({ phase: "EVALUATING", candidate: { sha: "C1", contractVersion: K } });
    for (const ev of events) {
      const r = reduce(state, ev);
      assert.equal(r.ok, true, !r.ok ? r.reason : "");
      state = r.state;
    }
    return state;
  };
  const live = fold();
  const rebuilt = fold();
  assert.equal(live.phase.messages?.[0].addressedReport?.at, "2026-01-02T00:00:00.000Z");
  assert.equal(projectMessages(live.phase), projectMessages(rebuilt.phase));
  assert.equal(projectLedger(live.phase), projectLedger(rebuilt.phase));
  assert.equal(projectReview(live.phase), projectReview(rebuilt.phase));
  assert.match(projectLedger(live.phase), /"addressed":false/);
});

test("plan 04a: a submit_evaluation naming one message twice is refused, never partly applied", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-submit", tool: "raise_tradeoff", args: { choice: "One choice", alternative: "the other", why: "matters", anchor: { path: "src/a.ts", lines: [1, 2] } } },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: stubReviewer(),
    evaluatorScriptFor: (messageType) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps:
        messageType === "tradeoff"
          ? [
              {
                kind: "call-submit",
                tool: "submit_evaluation",
                args: {
                  evaluations: [
                    { messageId: "T-1", action: "publish", title: "first" },
                    { messageId: "T-1", action: "drop", reason: "duplicate" },
                  ],
                },
              },
            ]
          : [],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000, 20, setup.runDir);
    assert.ok(
      !readEvents(setup.runDir).some((r) => r.kind === "error"),
      "a duplicate-id submission must be refused, not crash the conductor",
    );
    const streamDir = runPaths(setup.runDir).stream;
    const text = fs
      .readdirSync(streamDir)
      .filter((f) => f.startsWith("evaluator-tradeoff-"))
      .map((f) => fs.readFileSync(path.join(streamDir, f), "utf8"))
      .join("\n");
    assert.match(text, /named more than once/);
    const message = (setup.conductor.state.phase.messages ?? []).find((m) => m.type === "tradeoff")!;
    assert.equal(message.state, "published", "the refused round later publishes the message unevaluated");
    assert.equal(message.unevaluated, true);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 04a: a later round's new raw messages are evaluated afresh (no stale settled flag)", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "raise_tradeoff",
          args: { choice: `Round ${attempt} choice`, alternative: "the other way", why: "matters", anchor: { path: "src/a.ts", lines: [1, 2] } },
        },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const open = state.phase.findings.find((f) => f.raisedBy === "M" && f.status === "open");
      const repaired = open && open.boundCandidateSha !== state.phase.candidate?.sha;
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: reviewer === "M" && repaired ? [{ findingId: open!.id, status: "confirm" }] : [],
              ballots: [],
              findings: reviewer === "M" && !open ? [{ kind: "defect", severity: "blocking", evidence: "the loop can spin" }] : [],
            },
          },
        ],
      };
    },
    deadlines: { ...FAST, reviewMs: 30_000, probeMs: 5_000 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 90_000, 20, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    assert.ok(
      !(setup.conductor.state.phase.messages ?? []).some((m) => m.state === "raw"),
      "a later round's new raw message must not skip evaluation via a stale settled flag",
    );
    assert.ok(
      eventTypes(setup.runDir).filter((t) => t === "EVALUATION_COMPLETED").length >= 2,
      "each round must complete its own evaluation",
    );
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 04a: a conductor killed during EVALUATING re-dispatches the evaluation once on restart", async () => {
  const repo = fs.mkdtempSync("/tmp/tt-eval-repo-");
  const root = fs.mkdtempSync("/tmp/tt-eval-run-");
  try {
    execFileSync("git", ["init", "-q", "-b", "main"], { cwd: repo });
    fs.writeFileSync(path.join(repo, "README.md"), "base\n");
    execFileSync("git", ["add", "-A"], { cwd: repo });
    execFileSync("git", ["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], { cwd: repo });
    const plan: RunPlanFile = {
      title: "eval recovery",
      repo,
      integrationBranch: "main",
      checks: ["true"],
      phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    };
    const runDir = createRun(root, plan);
    const scriptsDir = fs.mkdtempSync("/tmp/tt-eval-scripts-");
    const worker = writeScript(scriptsDir, "worker", { hello: defaultWorkerHello(), steps: [submitPhaseStep(DECISIONS)] });
    const reviewer = (r: Reviewer) => writeScript(scriptsDir, `reviewer-${r}`, {
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-submit", tool: "submit_review", args: { reviewer: r, phaseId: "p1", candidateSha: "$TT_CANDIDATE_SHA", contractVersion: contractVersionFor(plan.phases[0]), correctionStatements: [], findingStatements: [], ballots: [], findings: [] } },
      ],
    });
    const evaluator = writeScript(scriptsDir, "evaluator", {
      hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
      steps: [{ kind: "sleep", ms: 8_000 }, { kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [] } }],
    });
    const deadlines = { abortGraceMs: 200, termGraceMs: 200, helloTimeoutMs: 5_000, checkMs: 20_000, freezeMs: 15_000, workerAttemptMs: 30_000, reviewMs: 30_000, probeMs: 5_000, evaluateMs: 30_000 };
    const make = () =>
      new Conductor({
        runDir,
        plan,
        piCommand: process.execPath,
        piArgsPrefix: [FAKE_PI_PATH],
        stubReviews: false,
        deadlines,
        piEnvFor: (role, agentId) =>
          role === "worker"
            ? { FAKE_PI_SCRIPT: worker }
            : role === "evaluator"
              ? { FAKE_PI_SCRIPT: evaluator }
              : { FAKE_PI_SCRIPT: reviewer((agentId.match(/^reviewer-([MAB])-/)?.[1] ?? "M") as Reviewer) },
      });

    const first = make();
    await first.start();
    await waitFor(() => first.state.phase.phase === "EVALUATING", 60_000, 20, runDir);
    await waitFor(
      () => readEvents(runDir).some((r) => r.kind === "intent" && (r.event as { agentId?: string }).agentId?.startsWith("evaluator-")),
      30_000,
      20,
      runDir,
    );
    await first.stop();

    const second = make();
    await second.start();
    try {
      await waitFor(
        () => readEvents(runDir).filter((r) => r.kind === "event" && (r.event as { type: string }).type === "EVALUATION_INTERRUPTED").length === 1,
        30_000,
        20,
        runDir,
      );
      await waitFor(() => second.state.phase.phase === "DONE", 60_000, 20, runDir);
    } finally {
      await second.stop();
    }
    cleanupDir(scriptsDir);
  } finally {
    cleanupDir(repo);
    cleanupDir(root);
  }
});