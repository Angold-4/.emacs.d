// Plan 2c (tradeoffs-trace/plans/2c-review-loop-speed.org), end to end with
// fake-pi agents and real processes/git:
//
//  - discovery-barrier: no reviewer gets turn 2 before all three finished
//    turn 1, so M's turn-2 prompt lists a record A discovered late.
//  - no-unshown-ballots: a discovery submitted after the barrier released (a
//    reviewer re-dispatched after a turn-2 timeout) is logged as a late
//    observation, never a votable record, and the phase still reaches DONE.
//  - repair-context: a repair attempt's prompt carries the blocking finding,
//    the failed vote with the rejecting rationale, and the prior decisions to
//    mark kept/changed/withdrawn; the kept decision is rebound and passes.
//  - probe-reuse: a fast-forward probe reuses the candidate's passed checks
//    instead of running them a second time.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import type { State } from "../../src/core/types.ts";

type Setup = Awaited<ReturnType<typeof setupConductor>>;

const fastDeadlines = {
  abortGraceMs: 200,
  termGraceMs: 200,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
};

function tmpDir(name: string): string {
  return fs.mkdtempSync(path.join("/tmp", `tt-${name}-`));
}

function workerDecision(state: State) {
  return state.phase.decisions.find((d) => d.source === "worker" && !d.supersededBy);
}

function reviewArgs(reviewer: "M" | "A" | "B", state: State, extra: Record<string, unknown> = {}) {
  return {
    reviewer,
    phaseId: state.phase.phaseId,
    candidateSha: state.phase.candidate?.sha,
    contractVersion: state.phase.contract.contractVersion,
    correctionStatements: [],
    findingStatements: [],
    ...extra,
  };
}

function approveBallot(decisionId: string, rationale = "approve: consistent with the goal") {
  return { decisionId, vote: "approve", rationale, evidence: ["src/sum.js:1"] };
}

const DISCLOSED = {
  choice: "Reject non-numbers with a TypeError",
  whyItMatters: "Callers learn about bad input at once instead of getting NaN",
  alternatives: [{ option: "coerce with Number()", consequence: "silently turns '' into 0" }],
  recommendation: { choice: "throw", reason: "fails loudly" },
  classProposal: "delegated",
};

async function runToTerminal(setup: Setup, timeoutMs = 90_000): Promise<string> {
  await setup.conductor.start();
  await waitFor(
    () => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase),
    timeoutMs,
    50,
    setup.runDir,
  );
  return setup.conductor.state.phase.phase;
}

async function teardown(setup: Setup, ...dirs: string[]): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
  for (const d of dirs) cleanupDir(d);
}

test("discovery-barrier: M's turn 2 lists a record A discovered after M finished turn 1", async () => {
  const logs = tmpDir("barrier");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: fastDeadlines,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > sum.js" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [DISCLOSED], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const d = workerDecision(state)!;
      const discovery =
        reviewer === "A"
          ? [{ ...DISCLOSED, choice: "A-FOUND-LATE: logs every rejected input", classProposal: "detail" }]
          : [];
      return {
        hello: defaultReviewerHello(),
        steps: [
          // A discovers well after M and B have finished turn 1.
          ...(reviewer === "A" ? [{ kind: "sleep" as const, ms: 1500 }] : []),
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: discovery } },
          { kind: "wait-for-prompt" },
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, { ballots: [approveBallot(d.id)], findings: [] }) },
        ],
      };
    },
    extraReviewerEnv: (reviewer) => ({ FAKE_PI_PROMPT_LOG: path.join(logs, `${reviewer}.log`) }),
  });
  try {
    assert.equal(await runToTerminal(setup), "DONE");
    const mPrompts = fs.readFileSync(path.join(logs, "M.log"), "utf8");
    assert.match(mPrompts, /A-FOUND-LATE/, "M's turn-2 prompt must list A's discovery");

    const records = readEvents(setup.runDir);
    const released = records.findIndex((r) => r.kind === "discovery_barrier_released");
    const firstReview = records.findIndex((r) => r.kind === "event" && (r.event as { type: string }).type === "REVIEW_SUBMITTED");
    const discoveries = records.filter((r, i) => r.kind === "discovery_submitted" && i < released).length;
    assert.ok(released >= 0 && released < firstReview, "the barrier releases before any review is submitted");
    assert.equal(discoveries, 3, "all three discoveries precede the release");
  } finally {
    await teardown(setup, logs);
  }
});

test("no-unshown-ballots: a discovery after the barrier released is a late observation, and the phase still accepts", async () => {
  let bDispatches = 0;
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: { ...fastDeadlines, reviewMs: 4_000 },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > sum.js" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [DISCLOSED], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const d = workerDecision(state)!;
      const review = { kind: "call-submit" as const, tool: "submit_review", args: reviewArgs(reviewer, state, { ballots: [approveBallot(d.id)], findings: [] }) };
      if (reviewer === "B" && ++bDispatches === 1) {
        // First dispatch: turn 1 fine, then never submits turn 2 → timeout → re-dispatch.
        return {
          hello: defaultReviewerHello(),
          steps: [
            { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
            { kind: "wait-for-prompt" },
            { kind: "hang-forever" },
          ],
        };
      }
      const discoveries =
        reviewer === "B" ? [{ ...DISCLOSED, choice: "LATE-CHOICE: validates both arguments before adding" }] : [];
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries } },
          { kind: "wait-for-prompt" },
          review,
        ],
      };
    },
  });
  try {
    assert.equal(await runToTerminal(setup), "DONE");
    const records = readEvents(setup.runDir);
    const late = records.filter((r) => r.kind === "late_discovery");
    assert.equal(late.length, 1, "B's re-dispatched discovery is logged as late");
    assert.match(JSON.stringify(late[0].event), /LATE-CHOICE/);
    assert.ok(
      !setup.conductor.state.phase.decisions.some((d) => d.choice.startsWith("LATE-CHOICE")),
      "a late discovery never becomes a votable record",
    );
  } finally {
    await teardown(setup);
  }
});

test("repair-context: the repair prompt carries findings, the failed vote and prior decisions; the kept decision passes", async () => {
  const logs = tmpDir("repair");
  const promptLog = path.join(logs, "worker.log");
  let setupRef: Setup | undefined;
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: fastDeadlines,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScriptForAttempt: (attempt) => {
      if (attempt === 1) {
        return {
          hello: defaultWorkerHello(),
          steps: [
            { kind: "call-sh", command: "printf 'one\\n' > sum.js" },
            { kind: "call-submit", tool: "submit_phase", args: { decisions: [DISCLOSED], assumptions: [], deviations: [] } },
          ],
        };
      }
      const prior = workerDecision(setupRef!.conductor.state)!;
      return {
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-sh", command: "printf 'two\\n' > sum.js" },
          {
            kind: "call-submit",
            tool: "submit_phase",
            args: { decisions: [], assumptions: [], deviations: [], priorDecisions: [{ id: prior.id, status: "kept" }] },
          },
        ],
      };
    },
    reviewerScriptFor: (reviewer, state) => {
      const d = workerDecision(state)!;
      const round = state.phase.round ?? 1;
      let extra: Record<string, unknown>;
      if (round === 1) {
        extra = {
          ballots: [
            reviewer === "M"
              ? { decisionId: d.id, vote: "reject", rationale: "M-REJECT-RATIONALE: the error message leaks internals", evidence: ["sum.js:1"] }
              : approveBallot(d.id),
          ],
          findings:
            reviewer === "B"
              ? [{ kind: "defect", severity: "blocking", evidence: "B-BLOCKING-EVIDENCE: sum.js:1 returns NaN for null" }]
              : [],
        };
      } else {
        const blocking = state.phase.findings.find((f) => f.raisedBy === "B" && f.status === "open");
        extra = {
          ballots: [approveBallot(d.id, "approve: the repair answers the objection")],
          findings: [],
          findingStatements: reviewer === "B" && blocking ? [{ findingId: blocking.id, status: "confirm" }] : [],
        };
      }
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, extra) },
        ],
      };
    },
  });
  setupRef = setup;
  try {
    assert.equal(await runToTerminal(setup, 120_000), "DONE");
    const prompts = fs.readFileSync(promptLog, "utf8");
    const [first, second] = prompts.split(/\n=+\n?/).filter((p) => p.includes("Goal:"));
    assert.ok(first && second, "two worker prompts were captured");
    assert.match(first, /Do NOT run the full check suite/);
    assert.doesNotMatch(first, /REPAIR/);
    assert.match(second, /REPAIR/);
    assert.match(second, /B-BLOCKING-EVIDENCE/);
    assert.match(second, /M-REJECT-RATIONALE/);
    assert.match(second, /M veto/);
    assert.match(second, /priorDecisions/);

    const phase = setup.conductor.state.phase;
    const kept = phase.decisions.find((d) => d.source === "worker")!;
    assert.equal(kept.boundCandidateSha, phase.candidate!.sha, "the kept decision was rebound to the new candidate");
    assert.equal(kept.supersededBy, undefined);
    const finding = phase.findings.find((f) => f.raisedBy === "B")!;
    assert.equal(finding.status, "repaired", "B confirmed the repair (not a withdrawal)");
  } finally {
    await teardown(setup, logs);
  }
});

test("probe-reuse: a fast-forward probe reuses the candidate's passed checks", async () => {
  const dir = tmpDir("probe");
  const counter = path.join(dir, "checks.count");
  const setup = await setupConductor({
    checks: [`echo run >> ${counter}`],
    deadlines: fastDeadlines,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > sum.js" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    // Stub reviews (the harness default): one submit_review each.
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [{ kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state) }],
    }),
  });
  try {
    assert.equal(await runToTerminal(setup), "DONE");
    const runs = fs.readFileSync(counter, "utf8").trim().split("\n").length;
    // Plan 01e's base baseline is one execution, the candidate's gate another;
    // the identical probed tree may not add a third.
    assert.equal(runs, 2, "the checks ran on the base and the candidate, not again on the identical probed tree");
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "probe_checks_reused"));
  } finally {
    await teardown(setup, dir);
  }
});
