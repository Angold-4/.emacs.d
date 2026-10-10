// Plan 06k2 (A6): a submission whose tree equals the last reviewed candidate
// is refused before freeze (unless the owner asked for an unchanged
// resubmission), and a single huge tool-result line is truncated before it
// reaches the worker's context.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { runPaths } from "../../src/conductor.ts";
import { capToolResult, DEFAULT_TOOL_RESULT_CAP_BYTES } from "../../src/effects/rpc.ts";
import { asksForUnchangedResubmission, unchangedResubmissionRequesters } from "../../src/core/owner-commands.ts";

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
  workerAttemptMs: 60_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 8_000,
  reviewMs: 20_000,
  panelMs: 8_000,
  inboxPollMs: 40,
};

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

const submitStep = (): FakePiStep => ({
  kind: "call-submit",
  tool: "submit_phase",
  args: { decisions: [], assumptions: [], deviations: [] },
});

test("plan 06k2: a resubmission whose tree equals the last reviewed candidate is refused before freeze", async () => {
  const setup = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [{ kind: "call-sh", command: "printf 'attempt 1\\n' > work.txt" }, submitStep()],
          }
        : {
            // The repair worker makes NO change. Its first submit is refused;
            // the test then asks for an unchanged resubmission, and its second
            // (still unchanged) submit is accepted.
            hello: defaultWorkerHello(),
            steps: [{ kind: "sleep", ms: 2_000 }, submitStep(), { kind: "sleep", ms: 8_000 }, submitStep()],
          },
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const open = state.phase.findings.filter((f) => f.status === "open");
      const findings =
        round === 1 && reviewer === "M"
          ? [{ kind: "defect", severity: "blocking", evidence: "work.txt:1 the work is not done" }]
          : [];
      const findingStatements = round >= 2 ? open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })) : [];
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
              phaseId: "p1",
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements,
              findings,
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    // The unchanged repair submission is refused with a reason naming the
    // earlier candidate.
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "resubmission_refused"), 120_000, 50, setup.runDir);
    const refused = readEvents(setup.runDir).find((r) => r.kind === "resubmission_refused")!;
    const candidateSha = (refused.event as { candidateSha?: string }).candidateSha ?? "";
    assert.ok(candidateSha.length >= 7, "the refusal names the earlier candidate");
    assert.match((refused.event as { reason?: string }).reason ?? "", new RegExp(candidateSha.slice(0, 7)));

    // The owner asks for an unchanged resubmission; the next submit is taken.
    fs.writeFileSync(
      path.join(runPaths(setup.runDir).inbox, "cmd-unchanged.json"),
      JSON.stringify({ type: "correction", text: "resubmit unchanged", binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" } }),
    );
    await waitFor(() => (setup.conductor.state.phase.ownerNotes ?? []).some((n) => /unchanged/i.test(n)), 60_000, 20, setup.runDir);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 150_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    assert.ok(
      readEvents(setup.runDir).filter((r) => r.kind === "resubmission_refused").length >= 1,
      "the refusal happened before the owner's steer",
    );
  } finally {
    await teardown(setup);
  }
});

test("plan 06k2: a 4 MB tool output line is truncated with a marker before the worker sees it", () => {
  const line = "x".repeat(4 * 1024 * 1024);
  const capped = capToolResult(line);
  assert.ok(
    Buffer.byteLength(capped, "utf8") <= DEFAULT_TOOL_RESULT_CAP_BYTES + 200,
    `the result is at most the cap plus the marker (${Buffer.byteLength(capped, "utf8")} bytes)`,
  );
  assert.ok(capped.includes("truncated"), "the marker says it was truncated");
  assert.ok(capped.includes(String(line.length)), "the marker names the original length");
  // A result under the cap is byte-identical.
  assert.equal(capToolResult("small output"), "small output");
});

/** A repair round that changes nothing (the caller may stage-then-revert). */
function unchangedRepair(repairCommand: string): (attempt: number) => { hello: unknown; steps: FakePiStep[] } {
  return (attempt) =>
    attempt === 1
      ? { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "printf 'attempt 1\\n' > work.txt" }, submitStep()] }
      : { hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: repairCommand }, submitStep()] };
}

function raisingReviewer() {
  return (reviewer: string, state: { phase: { round?: number; findings: Array<{ id: string; status: string }> } }) => {
    const round = state.phase.round ?? 1;
    const open = state.phase.findings.filter((f) => f.status === "open");
    const findings =
      round === 1 && reviewer === "M"
        ? [{ kind: "defect", severity: "blocking", evidence: "work.txt:1 the work is not done" }]
        : [];
    const findingStatements = round >= 2 ? open.map((f) => ({ findingId: f.id, status: "withdraw", evidence: "fixed in this candidate" })) : [];
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
            phaseId: "p1",
            candidateSha: "$TT_CANDIDATE_SHA",
            contractVersion: (state.phase as unknown as { contract: { contractVersion: unknown } }).contract.contractVersion,
            correctionStatements: [],
            findingStatements,
            findings,
          },
        },
      ],
    };
  };
}

test("plan 06k2: a staged-then-reverted tree is refused as an unchanged resubmission", async () => {
  // The repair worker changes a tracked file, stages it, then restores the
  // working file to the candidate's bytes. `git status --porcelain` reads
  // `M ` (dirty), but the freeze's `git add -A` produces the candidate's
  // tree, so the frozen tree equals the last reviewed candidate.
  const setup = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: unchangedRepair("printf 'changed\\n' > work.txt && git add work.txt && printf 'attempt 1\\n' > work.txt"),
    reviewerScriptFor: raisingReviewer(),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "resubmission_refused"), 120_000, 50, setup.runDir);
    const refused = readEvents(setup.runDir).find((r) => r.kind === "resubmission_refused")!;
    assert.match((refused.event as { reason?: string }).reason ?? "", /identical to the last reviewed candidate/);
  } finally {
    await teardown(setup);
  }
});

test("plan 06k2: only an asking, in-force unchanged-resubmission steer lifts the refusal", () => {
  // A negation asks for the opposite, not permission.
  assert.equal(asksForUnchangedResubmission("resubmit unchanged"), true);
  assert.equal(asksForUnchangedResubmission("resubmit it unchanged please"), true);
  assert.equal(asksForUnchangedResubmission("do not resubmit unchanged"), false);
  assert.equal(asksForUnchangedResubmission("don't resubmit unchanged; fix the bug"), false);
  assert.equal(asksForUnchangedResubmission("never resubmit unchanged"), false);
  // A withdrawn directive asks for nothing; an in-force one does.
  assert.deepEqual(unchangedResubmissionRequesters([], [{ id: "OD-1", text: "resubmit unchanged", status: "withdrawn" }]), []);
  assert.deepEqual(unchangedResubmissionRequesters([], [{ id: "OD-1", text: "resubmit unchanged", status: "in-force" }]), ["directive:OD-1"]);
  assert.deepEqual(unchangedResubmissionRequesters(["resubmit unchanged"], []), ["note:0"]);
  assert.deepEqual(unchangedResubmissionRequesters(["do not resubmit unchanged"], []), []);
});
