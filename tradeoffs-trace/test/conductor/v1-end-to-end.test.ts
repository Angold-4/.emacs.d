// Plan 04c: one end-to-end fake-pi run that drives a phase through every new
// state the joined v1 has — BASELINE, the real review loop, EVALUATING with
// the evaluator and the blocker panel, a repair round off a panel downgrade,
// and the owner's two D verdicts (one before acceptance, which forces the
// repair; one after DONE, recorded as a follow-up). At each step it asserts
// the phase state, the control log, the rendered review/loop views and the
// balance metrics, and at the end deletes the projections and rebuilds them
// from `events.jsonl` — they must come back byte-identical.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { rebuildState, rebuildTimeline, runPaths } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

// The stage deadlines are load-tolerant on purpose: under the full suite four
// test files run at once, and this phase dispatches worker, reviewers,
// evaluators and panel seats. The run itself is ~15 s when idle; these bounds
// exist only so a busy host cannot turn a normal stage into a timeout that
// parks the phase before the test's own assertions run.
const FAST = {
  abortGraceMs: 150,
  termGraceMs: 150,
  helloTimeoutMs: 30_000,
  checkMs: 60_000,
  freezeMs: 60_000,
  workerAttemptMs: 120_000,
  reviewMs: 120_000,
  probeMs: 60_000,
  evaluateMs: 120_000,
  panelMs: 120_000,
  inboxPollMs: 50,
};

const DECISIONS = [
  {
    choice: "Use a simple loop rather than a library helper",
    whyItMatters: "Keeps the change dependency-free, matching the plan's zero-dependency goal",
    alternatives: [{ option: "pull in a small utility library", consequence: "adds a dependency for one function" }],
    recommendation: { choice: "keep the loop", reason: "no dependency needed for something this small" },
    classProposal: "detail",
  },
];

function submitPhaseStep(decisions: unknown[] = []) {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions, assumptions: [], deviations: [] } };
}

/** The evaluator publishes that type's raw messages and reports the candidate
 * addressed any owner-refused ones. Keyed off live state, so it needs no
 * hard-coded ids. */
function e2eEvaluator(messageType: string, state: State) {
  const messages = state.phase.messages ?? [];
  const evaluations = [
    ...messages
      .filter((m) => m.type === messageType && m.state === "raw")
      .map((m) => ({
        messageId: m.id,
        action: "publish",
        title: `Evaluated: ${m.title}`,
        summary: m.summary,
        context: m.context,
        evidence: m.evidence,
        importance: "medium",
      })),
    ...messages
      .filter((m) => m.type === messageType && m.state === "refused")
      .map((m) => ({ messageId: m.id, addressed: true, reason: "the candidate addressed the owner's reason" })),
  ];
  return {
    hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
    steps: [{ kind: "call-submit", tool: "submit_evaluation", args: { evaluations } }],
  };
}

/** B raises a blocker and one reviewer-raised trade-off (the unexposed-decision
 * proxy); M raises an advisory finding. On the repair round B confirms its
 * blocking finding repaired, and nobody re-raises what already exists. */
function e2eReviewer(reviewer: Reviewer, state: State) {
  const hasReviewerTradeoff = state.phase.decisions.some((d) => d.source === "reviewer-discovered");
  const hasBlocker = (state.phase.messages ?? []).some((m) => m.type === "blocker");
  const hasMFinding = state.phase.findings.some((f) => f.raisedBy === "M");
  const priorBlocking = state.phase.findings.filter(
    (f) => f.raisedBy === reviewer && f.severity === "blocking" && f.status === "open" && f.boundCandidateSha !== state.phase.candidate?.sha,
  );
  return {
    hello: defaultReviewerHello(),
    steps: [
      {
        kind: "call-submit",
        tool: "submit_discovery",
        args:
          reviewer === "B" && !hasReviewerTradeoff
            ? {
                discoveries: [
                  {
                    choice: "Keep the retry inline in the loop",
                    whyItMatters: "The retry is a consequential choice the worker did not disclose",
                    alternatives: [{ option: "extract a helper", consequence: "an extra abstraction for one call site" }],
                    recommendation: { choice: "keep it inline", reason: "smallest reviewable change" },
                    classProposal: "detail",
                  },
                ],
              }
            : { discoveries: [] },
      },
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
          findingStatements: priorBlocking.map((f) => ({ findingId: f.id, status: "confirm" })),
          ballots: [],
          findings:
            reviewer === "M" && !hasMFinding
              ? [{ kind: "defect", severity: "advisory", evidence: "src/sum.ts:9 a slow path returns NaN to callers" }]
              : [],
          ...(reviewer === "B" && !hasBlocker
            ? { blockers: [{ kind: "defect", evidence: "src/cancel.ts:10 the cancel path can deadlock" }] }
            : {}),
        },
      },
    ],
  };
}

function tt(args: string[]): { status: number; stdout: string; stderr: string } {
  try {
    const stdout = execFileSync(process.execPath, [CLI, ...args], { encoding: "utf8" });
    return { status: 0, stdout, stderr: "" };
  } catch (err) {
    const e = err as { status?: number; stdout?: string; stderr?: string };
    return { status: e.status ?? 1, stdout: e.stdout ?? "", stderr: e.stderr ?? "" };
  }
}

function ttAsync(args: string[]): Promise<{ status: number; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI, ...args]);
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ status: code ?? 1, stdout, stderr }));
  });
}

function eventTypes(setup: TestConductorSetup): string[] {
  return readEvents(setup.runDir)
    .filter((r) => r.kind === "event")
    .map((r) => (r.event as { type: string }).type);
}

/** The renderer's projections exist and match state; `tt contract check` is
 * retried so a projection write racing this call is not a false failure.
 *
 * The retry is ASYNC (`ttAsync`, not the blocking `execFileSync` `tt`): the
 * conductor runs in this test's own process, so a synchronous check would
 * freeze its status beat and the log would keep moving under the check,
 * making the mismatch permanent under load (the v1-end-to-end flake).
 * Awaiting the subprocess lets the beat keep the projections current while
 * the check runs. */
async function assertProjections(setup: TestConductorSetup): Promise<void> {
  const p = runPaths(setup.runDir);
  assert.ok(existsSync(p.review), "views/review.org must exist");
  assert.ok(existsSync(p.loop), "views/loop.txt must exist");
  assert.ok(existsSync(p.metrics), "views/metrics.json must exist");
  // 250 ms: a check spawned every 50 ms would add many node processes to the
  // already-parallel suite; the retry is only to ride out a projection write
  // that has not caught up with the log yet.
  const deadline = Date.now() + 150_000;
  for (;;) {
    const r = await ttAsync(["contract", "check", setup.runDir]);
    if (r.status === 0) return;
    if (Date.now() > deadline) {
      throw new Error(`assertProjections: contract check never matched for ${setup.runDir}: ${r.stderr.trim()}`);
    }
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
}

test("plan 04c end to end: BASELINE, the review loop, EVALUATING and the panel, two rounds, and the owner's two D verdicts", async () => {
  // The repair attempt waits on a release file the test writes only after the
  // owner's D and its finding acceptance have landed on C1. Without this the
  // round-two freeze can beat the owner's commands under a loaded suite, which
  // is a test race, not a product behaviour.
  const releaseDir = mkdtempSync("/tmp/tt-e2e-release-");
  const releaseFile = path.join(releaseDir, "go");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? { hello: defaultWorkerHello(), steps: [submitPhaseStep(DECISIONS)] }
        : {
            hello: defaultWorkerHello(),
            steps: [
              {
                kind: "call-sh",
                // Held for as long as the test itself may run: a hold that gave up after
                // 60 s let round two finish before the owner's D on a slow CI suite.
                command: `i=0; while [ $i -lt 3000 ]; do if [ -f '${releaseFile}' ]; then exit 0; fi; i=$((i+1)); sleep 0.1; done; exit 1`,
              },
              {
                kind: "call-submit",
                tool: "raise_tradeoff",
                args: {
                  choice: "Round two keeps the retry inline",
                  alternative: "extract a helper",
                  why: "the smallest reviewable repair",
                  anchor: { path: "src/sum.ts", lines: [1, 2] },
                },
              },
              submitPhaseStep(),
            ],
          },
    reviewerScriptFor: e2eReviewer,
    evaluatorScriptFor: e2eEvaluator,
    deadlines: FAST,
  });

  try {
    await setup.conductor.start();

    // --- state: BASELINE, then the worker implements --------------------
    await waitFor(() => setup.conductor.state.phase.phase === "BASELINE", 60_000, 10, setup.runDir);
    assert.ok(eventTypes(setup).includes("ATTEMPT_STARTED"));
    await assertProjections(setup);

    // --- state: EVALUATING, raw messages become published ---------------
    await waitFor(() => setup.conductor.state.phase.phase === "EVALUATING", 90_000, 10, setup.runDir);
    await waitFor(
      () => (setup.conductor.state.phase.messages ?? []).some((m) => m.type === "tradeoff" && m.state === "published"),
      60_000,
      20,
      setup.runDir,
    );
    await assertProjections(setup);

    const tradeoffs = (setup.conductor.state.phase.messages ?? []).filter((m) => m.type === "tradeoff");
    assert.ok(tradeoffs.some((m) => m.raisedBy === "worker"), "the worker's trade-offs are messages");
    assert.ok(tradeoffs.some((m) => m.raisedBy && m.raisedBy !== "worker"), "the reviewer's discovery is a reviewer-raised trade-off");
    assert.ok((setup.conductor.state.phase.messages ?? []).some((m) => m.type === "blocker"), "the blocker is a message");
    assert.ok((setup.conductor.state.phase.findings ?? []).some((f) => f.raisedBy === "B" && f.severity === "blocking"), "the blocker is a blocking finding");

    // --- owner D before acceptance: a repair round must follow ----------
    const refusedTarget = tradeoffs.find((m) => m.raisedBy === "worker")!;
    writeFileSync(`${setup.runDir}/conductor.pid`, String(process.pid));
    const refused = await ttAsync(["verdict", setup.runDir, refusedTarget.id, "refuse", "--reason", "the choice is wrong for the goal"]);
    assert.equal(refused.status, 0, refused.stderr);
    assert.match(refused.stdout, /verdict applied: refuse recorded/);
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.raisedBy === "owner" && f.severity === "blocking"),
      30_000,
      20,
      setup.runDir,
    );
    const ownerFinding = setup.conductor.state.phase.findings.find((f) => f.raisedBy === "owner" && f.severity === "blocking")!;
    assert.equal(ownerFinding.status, "open", "a pre-DONE D raises an open blocking finding");

    // The owner accepts that finding through the inbox while still on C1; the
    // D's blocking force is carried out by the repair round, not left forever.
    const phaseBeforeRepair = setup.conductor.state.phase;
    const ownerFindingBinding = {
      runId: phaseBeforeRepair.runId,
      phaseId: phaseBeforeRepair.phaseId,
      candidateSha: phaseBeforeRepair.candidate!.sha,
      contractVersion: phaseBeforeRepair.contract.contractVersion,
      recordId: ownerFinding.id,
      recordVersion: ownerFinding.version,
    };
    mkdirSync(path.join(setup.runDir, "inbox"), { recursive: true });
    writeFileSync(
      path.join(setup.runDir, "inbox", "accept-owner-finding.json"),
      JSON.stringify({ type: "accept-finding", scope: "the repair round carries out the owner's reason", binding: ownerFindingBinding }),
    );
    await waitFor(() => setup.conductor.state.phase.findings.find((f) => f.id === ownerFinding.id)?.status === "accepted", 30_000, 20, setup.runDir);
    await assertProjections(setup);
    // Now let the repair attempt proceed: its freeze is C2.
    writeFileSync(releaseFile, "go");

    // --- state: the panel's downgrade starts round two, then DONE --------
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 30, setup.runDir);
    assert.equal(setup.conductor.state.phase.round, 2, "the phase must have frozen two candidates");
    const panelDecided = readEvents(setup.runDir).find(
      (r) => r.kind === "event" && (r.event as { type?: string }).type === "PANEL_DECIDED",
    )?.event as { outcome?: string } | undefined;
    assert.equal(panelDecided?.outcome, "downgrade", "the panel downgrades the blocker into a repair round");

    const timeline = rebuildTimeline(setup.runDir, setup.plan);
    const states = timeline.phases.map((p) => p.phase);
    for (const state of ["BASELINE", "IMPLEMENTING", "FREEZING", "CHECKING", "PROBING", "REVIEWING", "EVALUATING", "REPAIRING", "RESOLVING", "ACCEPTED", "PUBLISHING", "DONE"]) {
      assert.ok(states.includes(state), `the phase timeline must visit ${state}: ${states.join(" → ")}`);
    }
    const types = eventTypes(setup);
    for (const type of ["BASELINE_COMPLETED", "EVALUATION_COMPLETED", "PANEL_DECIDED", "MESSAGE_PUBLISHED", "REPAIR_ATTEMPT_STARTED", "OWNER_VERDICT", "ACCEPTED"]) {
      assert.ok(types.includes(type), `events.jsonl must record ${type}`);
    }

    // --- metrics for the finished run -----------------------------------
    const metrics = JSON.parse(readFileSync(runPaths(setup.runDir).metrics, "utf8"));
    assert.equal(metrics.rounds, 2);
    assert.equal(metrics.ownerVerdicts.refuse, 1, "the pre-DONE D is counted from the log");
    assert.equal(metrics.blockers.downgraded, 1);
    assert.equal(metrics.blockers.escalated, 0);
    assert.ok(metrics.unexposedTradeoffs >= 1, "the reviewer-raised discovery is the unexposed proxy");
    assert.ok(readFileSync(runPaths(setup.runDir).review, "utf8").includes("* Blockers"), "the review view renders the blocker");
    await assertProjections(setup);

    // --- stop, then the owner D after DONE is a recorded follow-up ------
    rmSync(`${setup.runDir}/conductor.pid`, { force: true });
    await setup.conductor.stop();
    const lateTarget = (setup.conductor.state.phase.messages ?? []).find((m) => m.type === "tradeoff" && m.state === "published" && m.boundCandidateSha === setup.conductor.state.phase.candidate?.sha);
    assert.ok(lateTarget, "the repair round's own trade-off is still published for the post-DONE verdict");
    const late = tt(["verdict", setup.runDir, lateTarget!.id, "refuse", "--reason", "revisit after the run"]);
    assert.equal(late.status, 0, late.stderr);
    assert.match(late.stdout, /recorded refuse/);
    const rebuilt = rebuildState(setup.runDir, setup.plan, { lenient: true });
    assert.equal(rebuilt.phase.phase, "DONE", "a post-DONE D must not reopen the phase");
    assert.equal((rebuilt.phase.messages ?? []).find((m) => m.id === lateTarget!.id)?.followUp, true);
    const finalMetrics = JSON.parse(readFileSync(runPaths(setup.runDir).metrics, "utf8"));
    assert.equal(finalMetrics.ownerVerdicts.refuse, 2, "both D verdicts are counted");
    assert.equal(finalMetrics.refuseRate, 1);

    // --- rebuilt from scratch, the projections must match ----------------
    const p = runPaths(setup.runDir);
    const before = {
      messages: readFileSync(p.messages, "utf8"),
      ledger: readFileSync(p.ledger, "utf8"),
      review: readFileSync(p.review, "utf8"),
      metrics: readFileSync(p.metrics, "utf8"),
    };
    rmSync(p.messages);
    rmSync(p.ledger);
    rmSync(p.review);
    rmSync(p.metrics);
    rmSync(p.messagesView, { recursive: true, force: true });
    const rebuiltOut = tt(["contract", "rebuild", setup.runDir]);
    assert.equal(rebuiltOut.status, 0, rebuiltOut.stderr);
    assert.equal(readFileSync(p.messages, "utf8"), before.messages);
    assert.equal(readFileSync(p.ledger, "utf8"), before.ledger);
    assert.equal(readFileSync(p.review, "utf8"), before.review);
    assert.equal(readFileSync(p.metrics, "utf8"), before.metrics);
    assert.match(tt(["contract", "check", setup.runDir]).stdout, /contract check ok/);
    // `tt summary` carries the same balance numbers.
    assert.match(tt(["summary", setup.runDir]).stdout, /### Balance metrics/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    rmSync(releaseDir, { recursive: true, force: true });
  }
});
