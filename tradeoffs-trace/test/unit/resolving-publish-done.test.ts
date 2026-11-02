import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { initialState, rebuildState, rebuildTimeline, type RunPlanFile } from "../../src/conductor.ts";
import { evidenceHasAnchor } from "../../src/core/items.ts";
import { next } from "../../src/core/next.ts";
import { accept, setSeatAnchorResolver } from "../../src/core/predicate.ts";
import { expandProgram, initialProgramState, nextStarts, reduceProgram } from "../../src/core/program.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { Event } from "../../src/core/types.ts";
import { observeRun } from "../../src/program.ts";
import { approvingReview, makeDecision } from "./helpers.ts";

test("a phase accepted from RESOLVING publishes and reaches DONE", () => {
  const root = fs.mkdtempSync("/tmp/tt-resolving-publish-");
  const runId = "r1";
  const runDir = path.join(root, runId);
  const plan: RunPlanFile = {
    title: "owner-resolved publish",
    repo: root,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{
      id: "p1",
      goal: "implement R1",
      acceptance: ["R1 works"],
      checks: ["true"],
      finalChecks: ["true"],
      boundaries: [],
      reserved: [],
      requirements: [{ id: "R1", title: "R1", text: "R1 works", arch: [], verify: ["review"] }],
    }],
  };
  let state = initialState(runId, plan.phases[0], "H0");
  const K = state.phase.contract.contractVersion;
  const records: Array<{ seq: number; ts: string; kind: string; event: unknown }> = [];
  const record = (kind: string, event: unknown) => records.push({ seq: records.length + 1, ts: "2026-11-01T00:00:00.000Z", kind, event });
  const step = (event: Event) => {
    const result = reduce(state, event);
    assert.ok(result.ok, `${event.type}: ${!result.ok ? result.reason : ""}`);
    state = result.state;
    record("event", event);
  };
  const persist = () => fs.writeFileSync(path.join(runDir, "events.jsonl"), records.map((r) => JSON.stringify(r)).join("\n") + "\n");

  try {
    fs.mkdirSync(path.join(runDir, "plan"), { recursive: true });
    fs.writeFileSync(path.join(runDir, "plan", "v1.json"), JSON.stringify(plan));
    record("init", { runId, integrationHead: "H0" });

    // The live conductor installs this rule after validating the review's
    // anchors against the candidate. Its source file need not exist in the
    // status reader's cwd (and the worktree may have been pruned after DONE).
    const evidence = "candidate-only-resolving-publish.ts:1";
    assert.equal(fs.existsSync("candidate-only-resolving-publish.ts"), false);
    setSeatAnchorResolver((anchor) => evidenceHasAnchor(anchor));
    step({ type: "ATTEMPT_STARTED" });
    step({ type: "SUBMIT_PHASE", disclosures: [] });
    step({ type: "FREEZE_COMPLETED", candidateSha: "C1", decisions: [makeDecision({ boundContractVersion: K })] });
    step({ type: "CHECKS_PASSED" });
    step({ type: "PROBE_PASSED", probedI: "I1" });
    for (const reviewer of ["M", "A", "B"]) {
      step({ type: "BALLOT_CAST", ballot: {
        reviewer, decisionId: "D1", vote: "reject", rationale: "owner must rule", evidence: [evidence],
        boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1,
      } });
      step({ type: "REVIEW_SUBMITTED", review: {
        ...approvingReview(reviewer, "C1", K), items: [{ id: "R1", verdict: "met", evidence }],
      } });
    }
    step({ type: "EVALUATION_COMPLETED" });
    step({ type: "OWNER_REQUEST_OPENED", request: {
      id: "OR-1", version: 1, phaseId: "p1", origin: "failed_vote", linkedDecisionId: "D1",
      reason: "the decision failed its vote", options: [{ id: "accept_as_implemented", label: "accept as implemented" }], status: "open",
    } });
    assert.equal(state.phase.phase, "RESOLVING");
    assert.equal(accept(state.phase, "C1", K), false);
    step({ type: "OWNER_REQUEST_RESOLVED", requestId: "OR-1", option: "accept_as_implemented",
      boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 });
    assert.equal(state.phase.phase, "RESOLVING");
    assert.equal(accept(state.phase, "C1", K), true);
    assert.deepEqual(next(state), [{ type: "final_check_required" }]);

    step({ type: "FINAL_CHECK_REQUIRED" });
    assert.equal(state.phase.phase, "FINAL_CHECKING");
    step({ type: "ACTION_STARTED", action: "run_final_checks", actionId: "final-1" });
    step({ type: "FINAL_CHECKS_PASSED", candidateSha: "C1" });
    assert.equal(state.phase.phase, "ACCEPTED");
    assert.deepEqual(next(state), [{ type: "publish_intent", expectedHead: "H0", candidateI: "I1" }]);
    step({ type: "PUBLISH_INTENT", expectedHead: "H0", candidateI: "I1" });
    assert.equal(state.phase.phase, "PUBLISHING");
    step({ type: "ACTION_STARTED", action: "publish_cas", actionId: "publish-1" });
    step({ type: "PUBLISH_COMPLETED", newHead: "I1" });
    assert.equal(state.phase.phase, "DONE");
    assert.equal(state.phase.publishedI, "I1");
    assert.deepEqual(next(state), []);
    record("stop", { reason: "conductor stopped", cause: "conductor" });
    persist();

    // A scheduler/status process has not constructed a live conductor or a
    // view. On the base, lenient replay rejects FINAL_CHECK_REQUIRED and
    // remains RESOLVING despite the recorded publish and clean stop.
    setSeatAnchorResolver(undefined);
    const rebuilt = rebuildState(runDir, plan, { lenient: true });
    assert.equal(rebuilt.phase.phase, "DONE", "status replay must retain the completed publish");
    assert.equal(rebuilt.phase.publishedI, "I1");
    setSeatAnchorResolver(undefined);
    assert.deepEqual(rebuildState(runDir, plan), state, "strict recovery must match the live reducer");
    setSeatAnchorResolver(undefined);
    const timeline = rebuildTimeline(runDir, plan);
    assert.deepEqual(timeline.state, state);
    assert.deepEqual(timeline.phases.slice(-5).map((p) => p.phase), ["RESOLVING", "FINAL_CHECKING", "ACCEPTED", "PUBLISHING", "DONE"]);

    const nodes = expandProgram({ title: "serial program", maxParallel: 1, entries: [
      { id: "first", after: [], plan },
      { id: "second", after: ["first"], plan },
    ] });
    let program = reduceProgram(initialProgramState(nodes), { type: "NODE_STARTED", node: "first", runId });
    assert.deepEqual(nextStarts(nodes, program, 1), []);
    setSeatAnchorResolver(undefined);
    const status = observeRun(runDir, root);
    assert.equal(status, "done", "the scheduler must observe the published run as done");
    program = reduceProgram(program, { type: "NODE_STATUS", node: "first", status });
    assert.equal(program.nodes.first.status, "done");
    assert.deepEqual(nextStarts(nodes, program, 1), ["second"]);
  } finally {
    setSeatAnchorResolver(undefined);
    fs.rmSync(root, { recursive: true, force: true });
  }
});
