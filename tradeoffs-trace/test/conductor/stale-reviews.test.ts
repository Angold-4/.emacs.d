// Program d7dcd255 (atlas plan 14): late reviewer results leaked into the
// next round. Run cc1992e2 and ff398f35 logged a late REVIEW_SUBMITTED and
// then its rejection, so every resume crashed; run 807d3e84 marked B timed
// out from a stray dispatch, so B was never dispatched again and the phase
// ended BLOCKED at the discovery barrier.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, makeRepo, makeRunRoot, readEvents, setupConductor, sleep, waitFor, type FakePiStep } from "./harness.ts";
import { createRun, rebuildState, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import { expandProgram, initialProgramState, nextStarts, reduceProgram, type ProgramFile } from "../../src/core/program.ts";

test("stale reviews: a log with an event followed by its rejection still rebuilds (poisoned logs recover)", () => {
  const repo = makeRepo();
  const root = makeRunRoot();
  try {
    const plan = {
      title: "t", repo: repo.dir, integrationBranch: "main", checks: ["true"],
      phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    } as RunPlanFile;
    const runDir = createRun(root, plan);
    const late = { type: "REVIEW_SUBMITTED", review: { reviewer: "B", phaseId: "p1", candidateSha: "C0", correctionStatements: [], findingStatements: [] } };
    const lines = [
      { seq: 1, ts: "2026-09-25T00:00:00.000Z", kind: "init", event: { runId: "r1", integrationHead: repo.head } },
      { seq: 2, ts: "2026-09-25T00:00:01.000Z", kind: "event", event: late },
      { seq: 3, ts: "2026-09-25T00:00:01.001Z", kind: "rejected", event: { event: late, reason: "no rule from phase=READY" } },
    ];
    fs.writeFileSync(runPaths(runDir).events, lines.map((l) => JSON.stringify(l)).join("\n") + "\n");
    const state = rebuildState(runDir, plan); // strict: used by resume
    assert.equal(state.phase.phase, "READY", "the rejected event is skipped, as the live conductor skipped it");
  } finally {
    cleanupDir(root);
    cleanupDir(repo.dir);
  }
});

test("program retry: a blocked node goes back to waiting and starts again; done and running nodes are left alone", () => {
  const plan = (t: string) => ({ title: t, repo: "/r", integrationBranch: "main", checks: [], phases: [{ id: "p", goal: "g", acceptance: ["a"], checks: [], boundaries: [], reserved: [] }] }) as unknown as RunPlanFile;
  const program: ProgramFile = { title: "t", maxParallel: 2, entries: [{ id: "a", after: [], plan: plan("a") }, { id: "b", after: ["a"], plan: plan("b") }] };
  const nodes = expandProgram(program);
  let s = initialProgramState(nodes);
  s = reduceProgram(s, { type: "NODE_STARTED", node: "a", runId: "r1", branch: "main--a", base: "main" });
  s = reduceProgram(s, { type: "NODE_STATUS", node: "a", status: "blocked" });
  assert.deepEqual(nextStarts(nodes, s, 2), [], "a blocked node never restarts by itself");
  s = reduceProgram(s, { type: "NODE_RETRY", node: "a" });
  assert.equal(s.nodes.a.status, "waiting");
  assert.equal(s.nodes.a.branch, "main--a", "its branch is kept");
  assert.equal(s.nodes.a.runId, undefined, "the next start is a fresh run");
  assert.deepEqual(nextStarts(nodes, s, 2), ["a"]);
  s = reduceProgram(s, { type: "NODE_STARTED", node: "a", runId: "r2" });
  assert.equal(reduceProgram(s, { type: "NODE_RETRY", node: "a" }).nodes.a.status, "running", "a running node is not retried");
});

test("stale reviews: a reviewer calling submit_review twice at once never crashes the conductor (run cc1992e2)", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    // helloTimeoutMs: generous, because the full suite runs four files at
    // once and a reviewer's process start can be slow under that load; a
    // hello that merely races a busy host must not read as a review timeout
    // (plan 04a adds an evaluator process per round, which adds to the load).
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 30_000, workerAttemptMs: 60_000, checkMs: 20_000, probeMs: 20_000, freezeMs: 20_000 },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > sum.js" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const review = (findings: unknown[]) => ({
        reviewer,
        phaseId: state.phase.phaseId,
        candidateSha: state.phase.candidate?.sha,
        contractVersion: state.phase.contract.contractVersion,
        correctionStatements: [],
        findingStatements: [],
        ballots: [],
        findings,
      });
      // The first submission waits on a slow reproduction; the second
      // arrives while it is being recorded.
      const slow = [{ kind: "defect", severity: "advisory", evidence: "check the loop — reproduction command: `sleep 2`", reproduction: { command: "sleep 2" } }];
      const steps: FakePiStep[] = [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
      ];
      if (reviewer === "B") steps.push({ kind: "call-submit", tool: "submit_review", args: review(slow), noWait: true }, { kind: "sleep", ms: 200 });
      steps.push({ kind: "call-submit", tool: "submit_review", args: review([]) });
      // A real agent's turn lasts until its tool calls return: wait for the
      // slow first submission itself, not a guessed time (a fixed 4 s sleep
      // was shorter than the reproduction under a loaded full-suite run).
      if (reviewer === "B") steps.push({ kind: "await-pending" });
      return { hello: defaultReviewerHello(), steps };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase), 90_000, 50, setup.runDir);
    const records = readEvents(setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    assert.ok(!records.some((r) => r.kind === "rejected"), "no event was ever rejected by reduce()");
    const submitted = records.filter((r) => r.kind === "event" && (r.event as { type: string }).type === "REVIEW_SUBMITTED");
    // Exactly one review per reviewer. WHICH of B's two submissions is
    // recorded, and whether B's first dispatch is re-dispatched, depends on
    // scheduling under load (program 03's 04c checks saw both orders on a
    // busy machine), so the test asserts only what it exists for: the
    // conductor never crashes and never records B twice.
    const byReviewer = new Map<string, number>();
    for (const r of submitted) {
      const who = (r.event as { review: { reviewer: string } }).review.reviewer;
      byReviewer.set(who, (byReviewer.get(who) ?? 0) + 1);
    }
    assert.deepEqual([...byReviewer.entries()].sort(), [["A", 1], ["B", 1], ["M", 1]], "one review per reviewer");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
