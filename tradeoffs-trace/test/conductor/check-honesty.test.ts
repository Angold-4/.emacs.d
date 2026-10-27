// Plan 07e (E4), contract item R10: a check's recorded outcome is honest.
//
// A check that exits non-zero with no output is recorded with failure class
// `silent` (never as an ordinary failure that hides the silence), and a check
// the owner's stop killed is recorded as `interrupted`, never as failed. The
// disk-full half of the same item is proved by `test/effects/log.test.ts`,
// where the ENOSPC failure lives.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor, type FakePiStep, type TestConductorSetup } from "./harness.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
};

/** A worker that submits at once (the minimal round). */
function quickWorker(): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: "printf 'ok\\n' > marker.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** An empty review, so `stubReviews` dispatch still settles. */
function stubReviewer(seat: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
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

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

/** Every `checks/<sha>/record.json` under the run. */
function checkRecords(runDir: string): Array<Record<string, unknown>> {
  const root = path.join(runDir, "checks");
  if (!fs.existsSync(root)) return [];
  const out: Array<Record<string, unknown>> = [];
  for (const sha of fs.readdirSync(root)) {
    const file = path.join(root, sha, "record.json");
    if (!fs.existsSync(file)) continue;
    try {
      out.push(JSON.parse(fs.readFileSync(file, "utf8")) as Record<string, unknown>);
    } catch {
      // a torn write is retried by the caller's waitFor
    }
  }
  return out;
}

test("plan 07e: a silent check is class silent and a check killed by a stop is interrupted", async () => {
  // 1. A check that exits non-zero with nothing to say is `silent`.
  const silent = await setupConductor({
    checks: ["exit 3"],
    phaseChecks: ["exit 3"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 10_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await silent.conductor.start();
    await waitFor(() => checkRecords(silent.runDir).length > 0, 60_000, 100, silent.runDir);
    const record = checkRecords(silent.runDir)[0]!;
    const command = (record.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.class, "silent", "a silent check is class silent, not an ordinary failure");
    assert.equal(command.failureClass, "silent");
    assert.equal(command.passed, false, "silence never passes a check");
    assert.equal(command.note, "check exited 3 with no output");
    const logText = fs.readFileSync(path.join(silent.runDir, "checks", String(record.candidateSha), String(command.log)), "utf8");
    assert.match(logText, /check exited 3 with no output/, "the check's own log says why it failed");
  } finally {
    await teardown(silent);
  }

  // 2. A check the owner's stop killed is `interrupted`, never `failed`.
  const stopped = await setupConductor({
    checks: ["sleep 6"],
    phaseChecks: ["sleep 30"],
    rounds: 1,
    stubReviews: true,
    deadlines: { ...FAST, checkMs: 60_000 },
    workerScript: () => quickWorker(),
    reviewerScriptFor: (seat, state) => stubReviewer(seat, state),
  });
  try {
    await stopped.conductor.start();
    await waitFor(
      () => readEvents(stopped.runDir).some((r) => r.kind === "intent" && String(r.actionId ?? "").startsWith("check-sh-")),
      60_000,
      50,
      stopped.runDir,
    );
    await stopped.conductor.stop();
    await waitFor(() => checkRecords(stopped.runDir).length > 0, 10_000, 50, stopped.runDir);
    const command = (checkRecords(stopped.runDir)[0]!.commands as Array<Record<string, unknown>>)[0]!;
    assert.equal(command.class, "interrupted", "a stop is not a failure");
    assert.equal(command.failureClass, "interrupted");
    assert.match(String(command.note), /interrupted/);
  } finally {
    cleanupDir(stopped.runRoot);
    cleanupDir(stopped.scriptsDir);
  }
});
