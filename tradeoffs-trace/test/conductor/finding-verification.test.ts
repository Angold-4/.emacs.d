// Plan 05e: finding verification. 3a facts first (the conductor compares a
// finding's claim with the check records it already holds, before any agent
// sees it), 3b run what can be run, and 3c the plan's severity rule.
//
// The 05b fixture (findings #19): reviewer M claimed "the ERT target of make
// check fails" on a candidate whose `make check` record had just passed. The
// claim is rejected with the record cited — before any evaluator is
// dispatched. A claim the record confirms is marked `confirmed by record`.

import assert from "node:assert/strict";
import { test } from "node:test";

import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { State } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";

const FAST = {
  abortGraceMs: 60,
  termGraceMs: 60,
  helloTimeoutMs: 5_000,
  checkMs: 20_000,
  freezeMs: 10_000,
  workerAttemptMs: 20_000,
  reviewMs: 15_000,
  probeMs: 4_000,
  evaluateMs: 15_000,
  panelMs: 15_000,
};

function submitPhaseStep() {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

function events(setup: TestConductorSetup) {
  return readEvents(setup.runDir).filter((r) => r.kind === "event").map((r) => r.event as { type: string } & Record<string, unknown>);
}

function reviewerRaising(findings: Array<Record<string, unknown>>) {
  return (reviewer: string, state: State) => ({
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
  });
}

test("plan 05e (3a): a finding claiming a named check fails on a candidate whose check record passed is rejected with the record cited, before any agent", async () => {
  const setup = await setupConductor({
    checks: ["make check"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "printf 'check:\\n\\ttrue\\n' > Makefile" }, submitPhaseStep()],
    }),
    reviewerScriptFor: reviewerRaising([
      // The 05b F-M-2 fixture: names `make check`, whose record passed.
      { kind: "defect", severity: "blocking", evidence: "the ERT target of make check fails" },
    ]),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const message = (phase.messages ?? []).find((m) => m.type === "finding")!;
    assert.ok(message, "the finding message was raised");
    assert.equal(message.state, "dropped", "a record-contradicted finding never reaches the owner");
    assert.match(message.settlement?.reason ?? "", /make check/);
    assert.match(message.settlement?.reason ?? "", /exit 0/);
    const finding = phase.findings.find((f) => f.raisedBy === "M")!;
    assert.equal(finding.status, "disproved");
    assert.match(finding.disprovedEvidence ?? "", /make check/);
    // No evaluator was ever dispatched: the finding was dropped before any
    // agent could see it.
    assert.equal(
      events(setup).filter((e) => e.type === "EVALUATOR_FINISHED").length,
      0,
      "no evaluator may run for a record-contradicted finding",
    );
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (3a): a finding naming a test the record lists as failing is marked confirmed by record", async () => {
  // The check exits 0 but its output names a failing test (a runner that
  // reports failures without a non-zero exit); the record therefore lists the
  // failing name, and a finding that names it is confirmed by the record.
  const setup = await setupConductor({
    checks: [`sh -c 'echo "not ok 1 - flaky_thing"; exit 0'`],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerRaising([{ kind: "defect", severity: "advisory", evidence: "flaky_thing fails on this candidate" }]),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.raisedBy === "M")!;
    assert.match(finding.verified ?? "", /confirmed by record/);
    assert.match(finding.verified ?? "", /flaky_thing/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (3c): a finding the evaluator cannot confirm is dropped with its reason; a confirmed one is published with its verified evidence", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerRaising([
      { kind: "defect", severity: "advisory", evidence: "src/a.ts:10 the loop can spin" },
      { kind: "defect", severity: "advisory", evidence: "src/b.ts:12 the guard is missing" },
    ]),
    evaluatorScriptFor: (messageType) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps:
        messageType === "finding"
          ? [
              {
                kind: "call-submit",
                tool: "submit_evaluation",
                args: {
                  evaluations: [
                    { messageId: "F-1", action: "drop", reason: "could not confirm the loop spins" },
                    { messageId: "F-2", action: "publish", title: "the guard is missing", summary: "no guard", context: "src/b.ts:12", evidence: ["src/b.ts:12"], verified: "src/b.ts:12" },
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
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const f1 = phase.messages.find((m) => m.id === "F-1")!;
    const f2 = phase.messages.find((m) => m.id === "F-2")!;
    assert.equal(f1.state, "dropped");
    assert.match(f1.settlement?.reason ?? "", /could not confirm/);
    assert.equal(phase.findings.find((f) => f.id === (f1.sourceRecordId ?? ""))?.status, "disproved");
    assert.equal(f2.state, "published");
    assert.match(phase.findings.find((f) => f.id === (f2.sourceRecordId ?? ""))?.verified ?? "", /evaluator: src\/b\.ts:12/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (3b): a finding's runnable test is re-run: a passing run drops it with command and exit, a failing run publishes it with them", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerRaising([
      { kind: "defect", severity: "advisory", evidence: "run_a fails", runnable: "exit 0" },
      { kind: "defect", severity: "advisory", evidence: "run_b fails", runnable: "exit 1" },
    ]),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const f1 = phase.messages.find((m) => m.id === "F-1")!;
    const f2 = phase.messages.find((m) => m.id === "F-2")!;
    assert.equal(f1.state, "dropped", "a runnable test that passes drops the finding");
    assert.match(f1.settlement?.reason ?? "", /exit 0/);
    assert.equal(f2.state, "published", "a runnable test that fails publishes the finding");
    const finding2 = phase.findings.find((f) => f.id === (f2.sourceRecordId ?? ""))!;
    assert.equal(finding2.verified, "run `exit 1` exit 1");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (5): the evaluator lowers a blocking finding that cites no acceptance item or reserved rule, and keeps one that cites an acceptance item", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    goal: "it works",
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerRaising([
      { kind: "defect", severity: "blocking", evidence: "the loop can spin on empty input" },
      { kind: "defect", severity: "blocking", evidence: "the acceptance item 'it works' does not hold: the value is not validated" },
    ]),
    evaluatorScriptFor: (messageType) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps:
        messageType === "finding"
          ? [
              {
                kind: "call-submit",
                tool: "submit_evaluation",
                args: {
                  evaluations: [
                    { messageId: "F-1", action: "publish", title: "the loop can spin", summary: "a guard is missing", context: "src/a.ts:10", evidence: ["src/a.ts:10"], verified: "src/a.ts:10" },
                    { messageId: "F-2", action: "publish", title: "the value is not validated", summary: "the criterion does not hold", context: "src/b.ts:12", evidence: ["src/b.ts:12"], verified: "src/b.ts:12" },
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
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => (f.verified ?? "").includes("src/a.ts:10")),
      90_000,
      20,
      setup.runDir,
    );
    const phase = setup.conductor.state.phase;
    const f1 = phase.findings.find((f) => (f.verified ?? "").includes("src/a.ts:10"))!;
    const f2 = phase.findings.find((f) => (f.verified ?? "").includes("src/b.ts:12"))!;
    assert.equal(f1.severity, "advisory", "a blocking finding that cites no acceptance item or reserved rule is lowered");
    assert.match(f1.severityReason ?? "", /acceptance item|reserved rule/);
    assert.equal(f2.severity, "blocking", "a blocking finding that cites an acceptance item stays blocking");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (3a): a finding that merely mentions a passing check is not auto-dropped (round-2 A-6)", async () => {
  const setup = await setupConductor({
    checks: ["make check"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "printf 'check:\\n\\ttrue\\n' > Makefile" }, submitPhaseStep()],
    }),
    reviewerScriptFor: reviewerRaising([
      { kind: "defect", severity: "advisory", evidence: "the cancel loop is unbounded; make check passes but does not cover it" },
    ]),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const finding = phase.findings.find((f) => f.raisedBy === "M")!;
    assert.equal(finding.status, "open", "mentioning a passing check is not a claim that it fails");
    const message = phase.messages.find((m) => m.type === "finding")!;
    assert.notEqual(message.state, "dropped");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (3b): a runnable command that times out does not publish the finding (round-2 A-7/M-3)", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerRaising([{ kind: "defect", severity: "advisory", evidence: "run_x hangs", runnable: "sleep 5" }]),
    deadlines: { ...FAST, checkMs: 1_000 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const message = phase.messages.find((m) => m.type === "finding")!;
    assert.equal(message.state, "dropped", "a timed-out run reproduces nothing");
    assert.match(message.settlement?.reason ?? "", /timed out/);
    assert.equal(phase.findings.find((f) => f.raisedBy === "M")!.status, "disproved");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (3a): a blocker claiming a named check fails is rejected before its panel too (round-3 disc-A-36)", async () => {
  const setup = await setupConductor({
    checks: ["make check"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "printf 'check:\\n\\ttrue\\n' > Makefile" }, submitPhaseStep()],
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
            blockers: reviewer === "B" ? [{ kind: "defect", evidence: "the ERT target of make check fails" }] : [],
          },
        },
      ],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const blocker = (phase.messages ?? []).find((m) => m.type === "blocker")!;
    assert.ok(blocker, "the blocker message was raised");
    assert.equal(blocker.state, "dropped", "a record-contradicted blocker never reaches a panel");
    assert.match(blocker.settlement?.reason ?? "", /make check/);
    assert.equal(
      events(setup).filter((e) => e.type === "ROUND_PANEL_VOTE" || e.type === "PANEL_VOTE").length,
      0,
      "no panel votes on a blocker the record already contradicted",
    );
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
