// Plan 07e (E2): every timeout branch records which bound it reached, and the
// owner still decides — a bound never accepts, rejects or rules.

// Keep lane a's additional integration cases in this targeted test entry point.
import "./bounds-imported.ts";

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
} from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";
import { parseCheckRecord } from "../../src/core/checks.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

const FAST = {
  inboxPollMs: 40,
  abortGraceMs: 200,
  termGraceMs: 200,
  helloTimeoutMs: 5_000,
  workerAttemptMs: 15_000,
  freezeMs: 10_000,
  checkMs: 5_000,
  probeMs: 5_000,
  reviewMs: 8_000,
  evaluateMs: 5_000,
};

function submitPhase() {
  return { kind: "call-submit" as const, tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

function stubReviewer(reviewer: Reviewer, state: State) {
  return {
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit" as const,
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
  };
}

function bounds(runDir: string): Array<{ kind: string; scope: string; limitMs: number; elapsedMs: number }> {
  return readEvents(runDir)
    .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "BOUND_REACHED")
    .map((r) => r.event as { kind: string; scope: string; limitMs: number; elapsedMs: number });
}

function eventTypes(runDir: string): string[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event")
    .map((r) => String((r.event as { type?: string }).type));
}

test("plan 07e: a worker attempt past its bound records BOUND_REACHED worker-attempt", async () => {
  const setup = await setupConductor({
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "hang-forever" }] }),
    deadlines: {
      ...FAST,
      abortGraceMs: 200,
      termGraceMs: 200,
      helloTimeoutMs: 5_000,
      workerAttemptMs: 500,
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => bounds(setup.runDir).some((b) => b.kind === "worker-attempt"), 15_000, 40, setup.runDir);
    const bound = bounds(setup.runDir).find((b) => b.kind === "worker-attempt")!;
    assert.equal(bound.scope, "p1");
    assert.equal(bound.limitMs, 500);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 07e: a check past its bound records BOUND_REACHED check", async () => {
  const setup = await setupConductor({
    checks: ["sleep 30"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhase()] }),
    deadlines: { ...FAST, checkMs: 500, probeMs: 500 },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => bounds(setup.runDir).some((b) => b.kind === "check"), 15_000, 40, setup.runDir);
    assert.equal(bounds(setup.runDir).find((b) => b.kind === "check")!.limitMs, 500);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 07e: a worker sh command past its bound records BOUND_REACHED sh", async () => {
  const setup = await setupConductor({
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "sleep 30" }, submitPhase()],
    }),
    deadlines: { ...FAST, workerAttemptMs: 20_000, shCommandMs: 400 },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => bounds(setup.runDir).some((b) => b.kind === "sh"), 15_000, 40, setup.runDir);
    assert.equal(bounds(setup.runDir).find((b) => b.kind === "sh")!.limitMs, 400);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 07e: an exhausted repair budget records BOUND_REACHED rounds and LEAD_CALLED, then parks exactly as before", async () => {
  const setup = await setupConductor({
    checks: ["false"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhase()] }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => stubReviewer(reviewer, state),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 30_000, 50, setup.runDir);
    const all = bounds(setup.runDir);
    assert.ok(
      all.some((b) => b.kind === "rounds"),
      `rounds bound recorded: ${JSON.stringify(all)}; events: ${JSON.stringify(readEvents(setup.runDir).slice(-16))}`,
    );
    const lead = readEvents(setup.runDir).find((r) => r.kind === "event" && (r.event as { type?: string }).type === "LEAD_CALLED");
    assert.ok(lead, "the lead is called when the rounds are exhausted");
    assert.ok(((lead!.event as { reasons?: string[] }).reasons ?? []).length > 0, "the lead is told why it is not accepted");
    assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 07e: an open owner park past its bound records BOUND_REACHED owner-wait and stays open", async () => {
  const setup = await setupConductor({
    checks: ["false"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhase()] }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => stubReviewer(reviewer, state),
    deadlines: { ...FAST, ownerWaitMs: 800 },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 30_000, 50, setup.runDir);
    await waitFor(() => bounds(setup.runDir).some((b) => b.kind === "owner-wait"), 10_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER", "the owner still decides");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 07e: a check that exits non-zero with no output is recorded as silent", async () => {
  const setup = await setupConductor({
    checks: ["exit 3"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhase()] }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => eventTypes(setup.runDir).includes("CHECKS_FAILED"), 20_000, 40, setup.runDir);
    const sha = setup.conductor.state.phase.candidate?.sha;
    assert.ok(sha, "a candidate exists");
    const file = path.join(runPaths(setup.runDir).checks, sha!, "record.json");
    await waitFor(() => fs.existsSync(file), 5_000, 40, setup.runDir);
    const record = parseCheckRecord(JSON.parse(fs.readFileSync(file, "utf8")));
    assert.ok(record, "the check record parses");
    assert.equal(record!.commands[0]?.failureClass, "silent");
    assert.equal(record!.commands[0]?.passed, false);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
