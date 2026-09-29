// Decision briefs: after evaluation, the conductor runs the brief-writing
// evaluator pass for every open owner item (and every flagged reserved
// decision). The evaluator's model writes a brief through `submit_brief`; the
// deterministic backstop fills anything it does not, and the recorded briefs
// are what the views and the status `needs you` line render.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { OwnerRequest, Reviewer, State } from "../../src/core/types.ts";

const FAST = {
  inboxPollMs: 40,
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 15_000,
  workerAttemptMs: 20_000,
  freezeMs: 10_000,
  checkMs: 5_000,
  probeMs: 5_000,
  reviewMs: 10_000,
  evaluateMs: 30_000,
};

function submitPhaseStep() {
  return { kind: "call-submit" as const, tool: "submit_phase" as const, args: { decisions: [], assumptions: [], deviations: [] } };
}

function submitReviewStep(reviewer: Reviewer, state: State) {
  return {
    kind: "call-submit" as const,
    tool: "submit_review" as const,
    args: {
      reviewer,
      phaseId: state.phase.phaseId,
      candidateSha: state.phase.candidate?.sha ?? "",
      contractVersion: state.phase.contract.contractVersion,
      correctionStatements: [],
      findingStatements: [],
    },
  };
}

/** Runs a phase whose checks always fail to the repair-budget gate:
 * AWAITING_OWNER with a plain `repair_budget_exhausted` owner request. */
async function awaitingOwner(opts: { briefScriptFor?: (state: State) => { hello?: unknown; steps: any[] } }): Promise<TestConductorSetup> {
  const setup = await setupConductor({
    checks: ["false"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({ hello: defaultReviewerHello(), steps: [submitReviewStep(reviewer, state)] }),
    briefs: true,
    ...(opts.briefScriptFor ? { briefScriptFor: opts.briefScriptFor } : {}),
    deadlines: FAST,
  });
  await setup.conductor.start();
  await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000);
  return setup;
}

function openRequest(setup: TestConductorSetup): OwnerRequest {
  const r = setup.conductor.state.phase.ownerRequests.find((x) => x.status === "open");
  assert.ok(r, "expected an open owner request");
  return r;
}

/** A valid brief for the budget-gate owner request (options grant/stop), with
 * no readable catalog in the harness so its example is explicitly unverified
 * and its impact is not asserted. */
function budgetBrief(request: OwnerRequest) {
  return {
    requestId: request.id,
    question: "Should the phase get more repair rounds or stop?",
    today: "No concrete example was recorded for this item. (example unverified)",
    impact: "No market stops publishing under either option; only the repair budget changes[2].",
    options: [
      { id: "grant", label: "Give more repair rounds", effect: "the worker tries again[2]", cost: "more time" },
      { id: "stop", label: "Stop the phase", effect: "the phase stops", cost: "the work is not finished" },
    ],
    recommendation: { option: "grant", why: "IC §5 says to try again before stopping" },
    related: [],
    evidence: ["message: the repair budget ran out", "code: src/core/blend.rs:88 the re-entry test reads the band only"],
  };
}

test("briefs: the evaluator's brief is recorded for the open owner request", async () => {
  const setup = await awaitingOwner({
    briefScriptFor: (state) => {
      const request = state.phase.ownerRequests.find((r) => r.status === "open");
      return {
        hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
        steps: [{ kind: "call-submit", tool: "submit_brief", args: request ? budgetBrief(request) : {} }],
      };
    },
  });
  try {
    const request = openRequest(setup);
    await waitFor(() => (setup.conductor.state.phase.briefs ?? []).some((b) => b.requestId === request.id), 60_000);
    const brief = (setup.conductor.state.phase.briefs ?? []).find((b) => b.requestId === request.id)!;
    assert.equal(brief.question, "Should the phase get more repair rounds or stop?");
    assert.deepEqual(brief.options.map((o) => o.id).sort(), request.options.map((o) => o.id).sort());
    // The brief is a logged fact, so a rebuild reproduces it.
    const types = readEvents(setup.runDir).map((r) => (r.event as { type?: string }).type);
    assert.ok(types.includes("BRIEFS_RECORDED"), "expected a BRIEFS_RECORDED event");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(setup.runRoot);
  }
});

test("briefs: a brief-agent tool mismatch falls back, with no LAUNCH_FAILED phase event", async () => {
  const setup = await awaitingOwner({
    briefScriptFor: () => ({ hello: { role: "evaluator", tools: ["read"] }, steps: [] }),
  });
  try {
    const request = openRequest(setup);
    await waitFor(() => (setup.conductor.state.phase.briefs ?? []).some((b) => b.requestId === request.id), 30_000);
    const types = readEvents(setup.runDir).map((r) => (r.event as { type?: string }).type);
    assert.ok(!types.includes("LAUNCH_FAILED"), "a brief mismatch runs only from AWAITING_OWNER, so it must not emit LAUNCH_FAILED");
    assert.ok(types.includes("BRIEFS_RECORDED"), "the deterministic backstop still records a brief");
    const brief = (setup.conductor.state.phase.briefs ?? []).find((b) => b.requestId === request.id)!;
    assert.match(brief.impact, /not established/i);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(setup.runRoot);
  }
});

/** A valid brief for a reserved decision (options approve/reject_and_repair),
 * with no readable catalog in the harness so its example is unverified and its
 * publishing answer is cited by a code path. */
function reservedBrief(decision: { id: string }) {
  return {
    requestId: decision.id,
    question: "Should this reserved choice stand or be rejected and repaired?",
    today: "No concrete example was recorded for this item. (example unverified)",
    impact: "No market stops publishing under either option; only the reserved choice changes[2].",
    options: [
      { id: "approve", label: "Approve it", effect: "the choice stands[2]", cost: "nothing further" },
      { id: "reject_and_repair", label: "Reject and repair", effect: "the worker tries again[2]", cost: "more time" },
    ],
    recommendation: { option: "approve", why: "IC §5 says the reviewed choice should stand" },
    related: [],
    evidence: ["message: the reserved choice", "code: src/core/blend.rs:88 the re-entry test reads the band only"],
  };
}

/** AWAITING_OWNER with two live reserved decisions and no readable catalogs.
 * The reserved decisions get briefs but never owner requests, so their only
 * briefs stay backstops until a model replaces them. */
async function awaitingOwnerWithReserved(
  briefScriptFor: (state: State) => { hello?: unknown; steps: any[] },
): Promise<TestConductorSetup> {
  const setup = await setupConductor({
    checks: ["false"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [1, 2].map((i) => ({
              choice: `keep reserved option ${i}`,
              whyItMatters: `reserved choice ${i} matters for the phase`,
              alternatives: [{ option: "the other way", consequence: "a different trade-off" }],
              recommendation: { choice: `keep reserved option ${i}`, reason: "measured acceptable" },
              classProposal: "reserved",
            })),
            assumptions: [],
            deviations: [],
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({ hello: defaultReviewerHello(), steps: [submitReviewStep(reviewer, state)] }),
    briefs: true,
    briefScriptFor,
    deadlines: FAST,
  });
  await setup.conductor.start();
  await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000);
  return setup;
}

/** The decision view's own AMEND encoding (type + binding); re-checking the
 * same tree leaves the candidate sha unchanged and starts a NEW park episode. */
function amendCommand(setup: TestConductorSetup, id: string): void {
  const phase = setup.conductor.state.phase;
  const K = phase.contract.contractVersion;
  fs.writeFileSync(
    path.join(runPaths(setup.runDir).inbox, `${id}.json`),
    JSON.stringify({
      commandId: id,
      type: "amend",
      binding: { runId: phase.runId, phaseId: phase.phaseId, contractVersion: K },
      newContractVersion: { snapshot: K.snapshot + 1, sectionSha256: "0".repeat(64) },
    }),
  );
}

const liveReserved = (state: State) =>
  state.phase.decisions.filter((d) => d.class === "reserved" && d.boundCandidateSha === state.phase.candidate?.sha);

const RESERVED_IDS = (setup: TestConductorSetup): string[] => liveReserved(setup.conductor.state).map((d) => d.id);

const hasModelBrief = (setup: TestConductorSetup, id: string, candidate: string): boolean =>
  (setup.conductor.state.phase.briefs ?? []).some(
    (b) => b.requestId === id && b.candidateSha === candidate && b.noRecommendationReason === undefined,
  );

test("briefs: a backstop is retried only on a later park, and one submit does not end the writer", async () => {
  let dispatches = 0;
  const setup = await awaitingOwnerWithReserved((state) => {
    dispatches += 1;
    if (dispatches === 1) {
      // Park 1: settle without submitting, so every item gets a backstop.
      return { hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator }, steps: [{ kind: "sleep", ms: 500 }] };
    }
    // Park 2 (after the AMEND re-check): submit the gate request's brief FIRST,
    // then each reserved decision. If the agent ended as soon as every in-flight
    // id had *a* brief, the first submit would look like full coverage and the
    // later two would never be recorded (finding M-41).
    const gate = state.phase.ownerRequests.find((r) => r.status === "open" && r.origin === "repair_budget_exhausted");
    const reserved = liveReserved(state);
    const steps: any[] = [];
    if (gate) steps.push({ kind: "call-submit", tool: "submit_brief", args: budgetBrief(gate) }, { kind: "sleep", ms: 300 });
    for (const d of reserved) steps.push({ kind: "call-submit", tool: "submit_brief", args: reservedBrief(d) }, { kind: "sleep", ms: 300 });
    return { hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator }, steps };
  });
  try {
    const reserved = RESERVED_IDS(setup);
    assert.equal(reserved.length, 2, "two live reserved decisions need briefs");
    const candidate = setup.conductor.state.phase.candidate!.sha;
    // Park 1: every item gets a backstop (the gate request plus both reserved
    // decisions), all from the one writer run.
    await waitFor(
      () =>
        reserved.every((id) =>
          (setup.conductor.state.phase.briefs ?? []).some((b) => b.requestId === id && b.noRecommendationReason !== undefined),
        ),
      60_000,
    );
    assert.equal(dispatches, 1, "one writer run covers every item of the first park");
    // OD-6: the SAME park must never retry. Several drive beats pass with no
    // second dispatch and no retry recorded.
    await new Promise((resolve) => setTimeout(resolve, 3_000));
    assert.equal(dispatches, 1, "a backstop is not retried on the next beat of the same park");
    assert.ok(
      !readEvents(setup.runDir).some((r) => r.kind === "event" && (r.event as { type?: string }).type === "BRIEF_RETRY_ATTEMPTED"),
      "no retry is recorded while the park is the same",
    );
    // A LATER park on the same candidate: AMEND re-checks the same tree.
    amendCommand(setup, "cmd-amend");
    await waitFor(() => setup.conductor.state.phase.phase !== "AWAITING_OWNER", 30_000);
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
    assert.equal(setup.conductor.state.phase.candidate?.sha, candidate, "the later park reviews the same candidate");
    // The retry runs once, and both reserved decisions end up with a MODEL
    // brief: the first submit did not end the writer (finding M-41).
    await waitFor(() => dispatches >= 2, 30_000);
    await waitFor(() => reserved.every((id) => hasModelBrief(setup, id, candidate)), 30_000);
    await new Promise((resolve) => setTimeout(resolve, 1_000));
    assert.equal(dispatches, 2, "exactly one retry dispatch on this candidate");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(setup.runRoot);
  }
});

test("briefs: a failed retry keeps the backstop and never dispatches again", async () => {
  let dispatches = 0;
  const setup = await awaitingOwnerWithReserved(() => {
    dispatches += 1;
    // Every writer run settles without submitting: the backstop covers the
    // items, and the one retry on the later park also fails.
    return { hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator }, steps: [{ kind: "sleep", ms: 500 }] };
  });
  try {
    const reserved = RESERVED_IDS(setup);
    const candidate = setup.conductor.state.phase.candidate!.sha;
    await waitFor(
      () =>
        reserved.every((id) =>
          (setup.conductor.state.phase.briefs ?? []).some((b) => b.requestId === id && b.noRecommendationReason !== undefined),
        ),
      60_000,
    );
    await new Promise((resolve) => setTimeout(resolve, 2_000));
    assert.equal(dispatches, 1, "no retry on the same park");
    amendCommand(setup, "cmd-amend");
    await waitFor(() => setup.conductor.state.phase.phase !== "AWAITING_OWNER", 30_000);
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
    // The single retry is dispatched once and fails; the backstop is kept and
    // the failure is recorded.
    await waitFor(() => (setup.conductor.state.phase.briefRetries ?? []).some((k) => k === `${candidate}::${reserved[0]}`), 30_000);
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "brief_retry_failed"), 30_000);
    assert.equal(dispatches, 2, "one retry dispatch for the backstop-only items");
    for (const id of reserved) {
      const held = (setup.conductor.state.phase.briefs ?? []).find((b) => b.requestId === id && b.candidateSha === candidate);
      assert.ok(held?.noRecommendationReason !== undefined, "the backstop stays shown until a model brief replaces it");
    }
    // A second later park must not spend another retry on the reserved items
    // (it may brief the new park's own new gate request).
    amendCommand(setup, "cmd-amend-2");
    await waitFor(() => setup.conductor.state.phase.phase !== "AWAITING_OWNER", 30_000);
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
    await new Promise((resolve) => setTimeout(resolve, 2_000));
    const retriedIds = readEvents(setup.runDir)
      .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "BRIEF_RETRY_ATTEMPTED")
      .flatMap((r) => ((r.event as { requestIds?: string[] }).requestIds ?? []));
    for (const id of reserved) {
      assert.equal(retriedIds.filter((x) => x === id).length, 1, "each reserved item is retried exactly once on this candidate");
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(setup.runRoot);
  }
});

test("briefs: the deterministic backstop fills in when the model submits none, without asserting an unchecked impact", async () => {
  const setup = await awaitingOwner({ briefScriptFor: () => ({ hello: { role: "evaluator", tools: [] }, steps: [] }) });
  try {
    const request = openRequest(setup);
    await waitFor(() => (setup.conductor.state.phase.briefs ?? []).some((b) => b.requestId === request.id), 60_000);
    const brief = (setup.conductor.state.phase.briefs ?? []).find((b) => b.requestId === request.id)!;
    assert.match(brief.impact, /not established/i);
    assert.deepEqual(brief.options.map((o) => o.id).sort(), request.options.map((o) => o.id).sort());
    assert.equal(brief.command, undefined);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(setup.runRoot);
  }
});
