// Decision briefs: after evaluation, the conductor runs the brief-writing
// evaluator pass for every open owner item (and every flagged reserved
// decision). The evaluator's model writes a brief through `submit_brief`; the
// deterministic backstop fills anything it does not, and the recorded briefs
// are what the views and the status `needs you` line render.

import assert from "node:assert/strict";
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
    impact: "Whether any market stops publishing is not established by this brief.",
    options: [
      { id: "grant", label: "Give three more rounds", effect: "the worker tries again", cost: "more time" },
      { id: "stop", label: "Stop the phase", effect: "the phase stops", cost: "the work is not finished" },
    ],
    recommendation: { option: "grant", why: "the plan's repair budget says to try again before stopping" },
    related: [],
    evidence: ["message: the repair budget ran out", "plan: the phase's own repair budget"],
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
