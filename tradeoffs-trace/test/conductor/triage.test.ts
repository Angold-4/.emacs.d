// Plan 06i: triage. Every finding and every discovered decision of the
// current candidate ends in exactly one disposition — fix (blocking until
// repaired), trade-off (accepted, with chosen/alternative/why) or escalate
// (an owner request). A wrong value, or a record that contradicts the golden
// source or the plan, must be fixed, whatever the reviewer's label and
// whatever round it was raised in; 06g's round-2 downgrade never applies to
// it. Nothing disappears. Fake-pi end to end.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { contractVersionFor, createRun, runPaths } from "../../src/conductor.ts";
import type { Decision, Reviewer, State } from "../../src/core/types.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import { createProgram, foldProgram, schedulerTick } from "../../src/program.ts";
import { prSummary } from "../../src/view.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  makeRepo,
  makeRunRoot,
  readEvents,
  setupConductor,
  waitFor,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

/** Runs the real CLI asynchronously: the conductor runs in THIS process, so a
 * synchronous execFileSync would block the event loop and the inbox poll that
 * applies the command. */
function runCli(args: string[]): Promise<{ stdout: string; code: number }> {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, TT_NOTIFY_COMMAND: ":" } });
    let stdout = "";
    child.stdout.on("data", (d) => (stdout += String(d)));
    child.on("close", (code) => resolve({ stdout, code: code ?? 0 }));
  });
}

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 10_000,
  reviewMs: 20_000,
  panelMs: 10_000,
};

const WRITE_SRC = "mkdir -p src && printf 'export const a = 1;\\n' > src/a.ts";

function submitPhaseStep() {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

function discoveryStep(discoveries: unknown[]) {
  return { kind: "call-submit", tool: "submit_discovery", args: { discoveries } };
}

function disclosure(choice: string, classProposal = "delegated") {
  return {
    choice,
    whyItMatters: "the plan left this choice open",
    alternatives: [{ option: "the other way", consequence: "would surprise callers" }],
    recommendation: { choice, reason: "keeps the promise" },
    classProposal,
  };
}

function reviewArgs(reviewer: Reviewer, state: State, extra: Record<string, unknown> = {}) {
  return {
    reviewer,
    phaseId: state.phase.phaseId,
    candidateSha: state.phase.candidate?.sha,
    contractVersion: state.phase.contract.contractVersion,
    correctionStatements: [],
    findingStatements: [],
    ballots: [],
    ...extra,
  };
}

/** An evaluator that publishes every raw message and, for the finding pass,
 * classifies each finding/discovered decision with the impact `impactFor`
 * returns. Evidence cites `src/a.ts:1`, a real file of the candidate. */
function triageEvaluator(impactFor: (id: string, source: "finding" | "decision") => "wrong-output" | "contract" | "judgement" | undefined) {
  return (messageType: string, state: State) => {
    const raw = (state.phase.messages ?? []).filter((m) => m.type === messageType && m.state === "raw");
    const checks: Array<{ id: string; verdict: string; impact?: string; evidence: string }> = [];
    if (messageType === "finding") {
      for (const f of state.phase.findings ?? []) {
        const impact = impactFor(f.id, "finding");
        if (impact)
          checks.push({
            id: f.id,
            verdict: "confirmed",
            impact,
            evidence: "src/a.ts:1 the evaluator re-checked the candidate",
            ...(impact === "judgement" ? { chosen: "accept it as an advisory", alternative: "repair it now", why: "the evaluator judged it a style or hardening call" } : {}),
          });
      }
    }
    // A discovered decision is published as a `tradeoff` message, so its
    // classification travels on whichever pass sees it; the finding pass
    // carries it too when one runs.
    for (const d of state.phase.decisions ?? []) {
      if (d.source !== "reviewer-discovered") continue;
      const impact = impactFor(d.id, "decision");
      if (impact)
        checks.push({
          id: d.id,
          verdict: "confirmed",
          impact,
          evidence: "src/a.ts:1 the evaluator re-checked the candidate",
          ...(impact === "judgement" ? { chosen: d.choice, alternative: d.alternatives?.[0]?.option, why: d.whyItMatters } : {}),
        });
    }
    return {
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations: raw.map((m) => ({
              messageId: m.id,
              action: "publish",
              title: "reviewed item",
              summary: m.summary,
              context: m.context,
              evidence: m.evidence,
            })),
            ...(checks.length > 0 ? { itemChecks: checks } : {}),
          },
        },
      ],
    };
  };
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

test("plan 06i: an advisory that the evaluator confirms as wrong output becomes blocking in round 2", async () => {
  // Round 1: M's grounded blocking finding forces a repair. Round 2: M files
  // the same point as an ADVISORY, but the evaluator confirms it is a wrong
  // value. Its impact is wrong-output, so its disposition is fix and it
  // blocks — 06g's round-2 downgrade never applies to a wrong-output record.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "triage a wrong value",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      provisional: false,
      rounds: 2,
    },
    workerScriptForAttempt: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const findings =
        reviewer === "M"
          ? round <= 1
            ? [{ kind: "defect", severity: "blocking", evidence: "R1 (it works) is unmet: src/a.ts:1 returns the wrong value" }]
            : [{ kind: "defect", severity: "advisory", evidence: "R1 (it works) is unmet: src/a.ts:1 still returns the wrong value" }]
          : [];
      return {
        hello: defaultReviewerHello(),
        steps: [
          discoveryStep([]),
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, { findings }) },
        ],
      };
    },
    evaluatorScriptFor: triageEvaluator((_id, source) => (source === "finding" ? "wrong-output" : undefined)),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.notEqual(phase.phase, "DONE", "a confirmed wrong value must not be accepted away");
    const advisory = phase.findings.find((f) => f.severity === "advisory" && f.status === "open");
    assert.ok(advisory, "the round-2 finding is an advisory");
    const record = (phase.triage ?? []).find((r) => r.itemId === advisory!.id);
    assert.ok(record, "the advisory has a triage record");
    assert.equal(record!.disposition?.kind, "fix", "a confirmed wrong-output advisory is a fix");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: after evaluation every finding and discovered decision of the candidate has exactly one disposition", async () => {
  // A blocking finding (fix) and a discovered decision (trade-off) — a mix.
  // After EVALUATION_COMPLETED each id has exactly one record with exactly one
  // disposition, and `tt summary` lists fixes, trade-offs and escalations in
  // separate sections.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "triage a mix",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      provisional: false,
      rounds: 1,
    },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep(reviewer === "M" ? [disclosure("the cap stays", "detail")] : []),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state, {
            findings:
              reviewer === "M"
                ? [{ kind: "defect", severity: "blocking", evidence: "R1 (it works) is unmet: src/a.ts:1 is broken" }]
                : [],
          }),
        },
      ],
    }),
    evaluatorScriptFor: triageEvaluator((_id, source) => (source === "finding" ? "wrong-output" : "judgement")),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const records = phase.triage ?? [];
    assert.ok(records.length >= 2, `expected a mix of findings and decisions, got ${records.length}`);
    for (const r of records) {
      assert.ok(r.disposition, `triage record ${r.itemId} has a disposition`);
      assert.equal(
        records.filter((x) => x.itemId === r.itemId).length,
        1,
        `exactly one record per id (${r.itemId})`,
      );
    }
    assert.ok(records.some((r) => r.disposition?.kind === "fix"), "the blocking finding is a fix");
    assert.ok(records.some((r) => r.disposition?.kind === "tradeoff"), "the discovered decision is a trade-off");
    const summary = prSummary(setup.runDir, setup.plan);
    assert.match(summary, /#### Fixes \(\d+\)/);
    assert.match(summary, /#### Trade-offs \(\d+\)/);
    assert.match(summary, /#### Escalations \(\d+\)/);
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a discovered decision that receives no ballots becomes an owner request and appears in the status", async () => {
  // M discovers a delegated decision; none of the three seats ballots it (each
  // review is accepted only after the incomplete-ballot re-prompts). It cannot
  // vanish: its triage disposition is escalate and an owner request names it,
  // so acceptance waits.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "escalate an unballoted decision",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      provisional: false,
      rounds: 1,
    },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => {
      const reviewStep = {
        kind: "call-submit",
        tool: "submit_review",
        args: reviewArgs(reviewer, state, { findings: [] }),
      };
      return {
        hello: defaultReviewerHello(),
        steps: [
          discoveryStep(reviewer === "M" ? [disclosure("a late prediction", "delegated")] : []),
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
          // The incomplete-ballot rule refuses twice; the third is accepted
          // with no ballots for the discovered decision.
          reviewStep,
          reviewStep,
          reviewStep,
        ],
      };
    },
    evaluatorScriptFor: triageEvaluator(() => undefined),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const decision = phase.decisions.find((d) => d.source === "reviewer-discovered")!;
    assert.ok(decision, "the discovered decision exists");
    assert.equal(phase.ballots.some((b) => b.decisionId === decision.id), false, "no seat balloted it");
    const record = (phase.triage ?? []).find((r) => r.itemId === decision.id);
    assert.ok(record, "the decision has a triage record");
    assert.equal(record!.disposition?.kind, "escalate", "a discovered decision with no ballots escalates");
    const request = phase.ownerRequests.find((r) => r.status === "open" && r.linkedDecisionId === decision.id);
    assert.ok(request, "an open owner request names the decision");
    assert.match(request!.reason, new RegExp(decision.id.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")));
    const summary = prSummary(setup.runDir, setup.plan);
    assert.match(summary, /#### Escalations \(\d+\)/);
    assert.match(summary, new RegExp(decision.id.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")));
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a run with only judgement trade-offs and no wrong output converges exactly as before", async () => {
  // C1: a phase whose only triage records are judgement trade-offs (an
  // advisory finding and a detail discovered decision) still reaches DONE.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep(reviewer === "M" ? [disclosure("a display nicety", "detail")] : []),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state, {
            findings: reviewer === "M" ? [{ kind: "defect", severity: "advisory", evidence: "a further edge path worth noting" }] : [],
          }),
        },
      ],
    }),
    evaluatorScriptFor: triageEvaluator(() => "judgement"),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 20, setup.runDir);
    const records = setup.conductor.state.phase.triage ?? [];
    assert.ok(records.length > 0, "the run was triaged");
    assert.ok(records.every((r) => r.disposition?.kind === "tradeoff"), "every disposition is a trade-off");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a deferral without a cost test or an owner ruling is refused, and open deferrals show in tt summary", async () => {
  // The owner's `tt defer` is an entry point: without a guard it is refused
  // with the reason and nothing is written; with a test guard it is recorded
  // and listed under "Open deferrals" in `tt summary` until resolved.
  // A REAL cost test: a requirement with a `test` verify and a check run
  // that emits it, so the item resolver resolves the name.
  const COST_TEST = "carried cap is fixed";
  const COST_CHECK = "printf 'ok 1 - carried cap is fixed\\n'";
  const setup = await setupConductor({
    checks: [COST_CHECK],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "defer an item with a guard",
      acceptance: ["R1 proves the cost"],
      checks: [COST_CHECK],
      boundaries: [],
      reserved: [],
      provisional: false,
      rounds: 1,
      requirements: [{ id: "R1", title: "R1 proves the cost", text: "R1 proves the cost", arch: [], verify: [`test "${COST_TEST}"`] }],
    },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_SRC },
        { kind: "call-submit", tool: "submit_coverage", args: { items: [{ id: "R1", status: "done", where: ["src/a.ts:1"], tests: [COST_TEST] }], arch: [] } },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const reviewStep = { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, { findings: [], items: [{ id: "R1", verdict: "met", evidence: "src/a.ts:1" }] }) };
      return {
        hello: defaultReviewerHello(),
        steps: [
          discoveryStep(reviewer === "M" ? [disclosure("a late prediction", "delegated")] : []),
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
          reviewStep,
          reviewStep,
          reviewStep,
        ],
      };
    },
    evaluatorScriptFor: triageEvaluator(() => undefined),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const decision = setup.conductor.state.phase.decisions.find((d) => d.source === "reviewer-discovered")!;
    assert.ok(decision, "the discovered decision exists");
    const refused = await runCli(["defer", setup.runDir, decision.id, "--root", setup.runRoot]);
    assert.equal(refused.code, 1, "a deferral with no guard is refused");
    assert.match(refused.stdout, /defer rejected:.*guard/);
    assert.equal((setup.conductor.state.phase.deferrals ?? []).length, 0, "nothing was recorded");

    // A test name that exists nowhere is not a substantive guard.
    const bogus = await runCli(["defer", setup.runDir, decision.id, "--test", "does-not-exist", "--root", setup.runRoot]);
    assert.equal(bogus.code, 1, "a test that does not exist is refused");
    assert.match(bogus.stdout, /defer rejected:.*does-not-exist/);

    const accepted = await runCli([
      "defer",
      setup.runDir,
      decision.id,
      "--test",
      COST_TEST,
      "--root",
      setup.runRoot,
    ]);
    assert.equal(accepted.code, 0, accepted.stdout);
    await waitFor(() => (setup.conductor.state.phase.deferrals ?? []).some((d) => d.itemId === decision.id), 30_000, 20, setup.runDir);
    const summary = prSummary(setup.runDir, setup.plan);
    assert.match(summary, /### Open deferrals \(1\)/);
    assert.match(summary, new RegExp(decision.id.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")), "the deferred item is named");
    assert.match(summary, new RegExp(`test: ${COST_TEST}`));
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: tt carry through a real inbox file adds the item as a requirement with VERIFY to the next node's contract", async () => {
  // The owner's carry is a REAL inbox file (written by `tt carry`) and is
  // applied by the conductor. The dependency's carried item then becomes a
  // REAL requirement of the next program node's contract (an id, its text
  // and a VERIFY): a wrong-output item carries a `test` verify, so that
  // node's reviewers must give it a verdict.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "carry a wrong-output finding",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      provisional: false,
      rounds: 1,
    },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep([]),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state, {
            findings: reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "R1 (it works) is unmet: src/a.ts:1 returns the wrong value" }] : [],
          }),
        },
      ],
    }),
    evaluatorScriptFor: triageEvaluator((_id, source) => (source === "finding" ? "wrong-output" : undefined)),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.status === "open" && f.severity === "blocking")!;
    assert.ok(finding, "the wrong-output finding is open");
    // `tt carry` writes a real command file into the run's inbox; the running
    // conductor applies it, which accepts the candidate with the item carried.
    const carried = await runCli(["carry", setup.runDir, finding.id, "--to", "b", "--root", setup.runRoot]);
    assert.equal(carried.code, 0, carried.stdout);
    assert.match(carried.stdout, new RegExp(`carried ${finding.id} to b`));
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    assert.ok(setup.conductor.state.phase.carriedItems?.includes(finding.id), "the item is carried in the run record");

    // A program whose node b depends on this run: the scheduler must write the
    // carried item into b's contract as a requirement.
    const repo = setup.repo;
    const phaseB = { id: "p1", goal: "b", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
    const planB = { title: "b", repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phaseB] };
    const program: ProgramFile = {
      title: "carry-program",
      maxParallel: 1,
      branches: "shared",
      entries: [
        // Node a is already DONE (its run is the conductor's); its plan is a
        // placeholder the scheduler never starts.
        { id: "a", after: [], plan: planB },
        { id: "b", after: ["a"], plan: planB },
      ],
    };
    const programDir = createProgram(setup.runRoot, program, "carryprog");
    fs.writeFileSync(
      path.join(programDir, "events.jsonl"),
      [
        { type: "NODE_STARTED", node: "a", runId: path.basename(setup.runDir), branch: "main", base: "main" },
        { type: "NODE_STATUS", node: "a", status: "done" },
      ]
        .map((event) => JSON.stringify({ ts: new Date().toISOString(), event }))
        .join("\n") + "\n",
    );
    schedulerTick(programDir, { runRoot: setup.runRoot, launch: () => undefined, maxTicks: 1 });
    const { state } = foldProgram(programDir);
    const runIdB = state.nodes["b"]?.runId;
    assert.ok(runIdB, "node b started");
    const plan = JSON.parse(fs.readFileSync(path.join(runPaths(path.join(setup.runRoot, runIdB!)).plan, "v1.json"), "utf8")) as {
      phases: Array<{ requirements?: Array<{ id: string; text: string; verify: string[] }> }>;
    };
    const requirement = plan.phases[0].requirements?.find((r) => r.id === `CARRY-${finding.id}`);
    assert.ok(requirement, "the carried item is a requirement of node b's contract");
    assert.match(requirement!.text, /returns the wrong value/);
    assert.deepEqual(requirement!.verify, [`test "carried ${finding.id} is fixed"`], "a wrong-output item carries a test verify");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a carried item goes only to the phase named by --to, not to every child", async () => {
  // a → {b, c}; the owner carries --to c. c gets the requirement; b does not.
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const phase = (goal: string) => ({ id: "p1", goal, acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] });
  const plan = (goal: string) => ({ title: goal, repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase(goal)] });
  const program: ProgramFile = {
    title: "targeted-carry",
    maxParallel: 2,
    branches: "shared",
    entries: [
      { id: "a", after: [], plan: plan("a") },
      { id: "b", after: ["a"], plan: plan("b") },
      { id: "c", after: ["a"], plan: plan("c") },
    ],
  };
  const programDir = createProgram(runRoot, program, "targetprog");
  try {
    const runDirA = createRun(runRoot, plan("a"));
    const finding = { id: "F-1", version: 1, phaseId: "p1", kind: "defect", severity: "blocking", evidence: "src/a.ts:1 returns the wrong value", raisedBy: "M", status: "open", boundCandidateSha: "" };
    const events = [
      { type: "FINDING_RAISED", finding },
      { type: "TRIAGE_RECORDED", record: { itemId: "F-1", source: "finding", impact: "wrong-output", disposition: { kind: "fix", reason: "a wrong value" } } },
      { type: "ITEM_CARRIED", recordId: "F-1", recordKind: "finding", toPhase: "c", boundCandidateSha: "", boundContractVersion: contractVersionFor(phase("a")), boundRecordVersion: 1 },
    ];
    fs.writeFileSync(
      runPaths(runDirA).events,
      events.map((event, i) => JSON.stringify({ seq: i + 1, ts: new Date().toISOString(), kind: "event", event })).join("\n") + "\n",
    );
    fs.writeFileSync(
      path.join(programDir, "events.jsonl"),
      [
        { type: "NODE_STARTED", node: "a", runId: path.basename(runDirA), branch: "main", base: "main" },
        { type: "NODE_STATUS", node: "a", status: "done" },
      ]
        .map((event) => JSON.stringify({ ts: new Date().toISOString(), event }))
        .join("\n") + "\n",
    );
    schedulerTick(programDir, { runRoot, launch: () => undefined, maxTicks: 1 });
    const { state } = foldProgram(programDir);
    const planOf = (id: string) =>
      JSON.parse(fs.readFileSync(path.join(runPaths(path.join(runRoot, state.nodes[id]!.runId!)).plan, "v1.json"), "utf8")) as {
        phases: Array<{ requirements?: Array<{ id: string }> }>;
      };
    assert.ok(!(planOf("b").phases[0].requirements ?? []).some((r) => r.id === "CARRY-F-1"), "node b must NOT get the item");
    const carried = (planOf("c").phases[0].requirements ?? []).find((r) => r.id === "CARRY-F-1");
    assert.ok(carried, "node c gets the carried requirement");
  } finally {
    cleanupDir(runRoot);
    cleanupDir(repo.dir);
  }
});

test("plan 06i: a carry and a deferral on different ids both import into the child", async () => {
  // OD-21(A): a carry and a deferral are different owner acts; when they name
  // DIFFERENT ids, both reach the child's contract.
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const phase = (goal: string) => ({ id: "p1", goal, acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] });
  const plan = (goal: string) => ({ title: goal, repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase(goal)] });
  const program: ProgramFile = {
    title: "carry-and-defer",
    maxParallel: 2,
    branches: "shared",
    entries: [
      { id: "a", after: [], plan: plan("a") },
      { id: "c", after: ["a"], plan: plan("c") },
    ],
  };
  const programDir = createProgram(runRoot, program, "bothprog");
  try {
    const runDirA = createRun(runRoot, plan("a"));
    const f1 = { id: "F-1", version: 1, phaseId: "p1", kind: "defect", severity: "blocking", evidence: "src/a.ts:1 wrong", raisedBy: "M", status: "open", boundCandidateSha: "" };
    const f2 = { id: "F-2", version: 1, phaseId: "p1", kind: "defect", severity: "blocking", evidence: "src/a.ts:2 wrong", raisedBy: "M", status: "open", boundCandidateSha: "" };
    const events = [
      { type: "FINDING_RAISED", finding: f1 },
      { type: "TRIAGE_RECORDED", record: { itemId: "F-1", source: "finding", impact: "wrong-output", disposition: { kind: "fix", reason: "wrong" } } },
      { type: "ITEM_CARRIED", recordId: "F-1", recordKind: "finding", toPhase: "c", boundCandidateSha: "", boundContractVersion: contractVersionFor(phase("a")), boundRecordVersion: 1 },
      { type: "FINDING_RAISED", finding: f2 },
      { type: "TRIAGE_RECORDED", record: { itemId: "F-2", source: "finding", impact: "wrong-output", disposition: { kind: "fix", reason: "wrong" } } },
      { type: "DEFERRAL_RECORDED", deferral: { id: "DEF-2", itemId: "F-2", text: "deferred", test: "the cost test", toPhase: "c", status: "open" } },
    ];
    fs.writeFileSync(
      runPaths(runDirA).events,
      events.map((event, i) => JSON.stringify({ seq: i + 1, ts: new Date().toISOString(), kind: "event", event })).join("\n") + "\n",
    );
    fs.writeFileSync(
      path.join(programDir, "events.jsonl"),
      [
        { type: "NODE_STARTED", node: "a", runId: path.basename(runDirA), branch: "main", base: "main" },
        { type: "NODE_STATUS", node: "a", status: "done" },
      ]
        .map((event) => JSON.stringify({ ts: new Date().toISOString(), event }))
        .join("\n") + "\n",
    );
    schedulerTick(programDir, { runRoot, launch: () => undefined, maxTicks: 1 });
    const { state } = foldProgram(programDir);
    const planC = JSON.parse(fs.readFileSync(path.join(runPaths(path.join(runRoot, state.nodes["c"]!.runId!)).plan, "v1.json"), "utf8")) as {
      phases: Array<{ requirements?: Array<{ id: string; verify: string[] }> }>;
    };
    const ids = (planC.phases[0].requirements ?? []).map((r) => r.id);
    assert.ok(ids.includes("CARRY-F-1"), "the carried id imported");
    assert.ok(ids.includes("DEFER-F-2"), "the deferred id imported too");
  } finally {
    cleanupDir(runRoot);
    cleanupDir(repo.dir);
  }
});

test("plan 06i: a deferred wrong-output item's next-node VERIFY is the fix test, not the cost test", async () => {
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const phase = (goal: string) => ({ id: "p1", goal, acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] });
  const plan = (goal: string) => ({ title: goal, repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase(goal)] });
  const program: ProgramFile = {
    title: "defer-target",
    maxParallel: 2,
    branches: "shared",
    entries: [
      { id: "a", after: [], plan: plan("a") },
      { id: "b", after: ["a"], plan: plan("b") },
      { id: "c", after: ["a"], plan: plan("c") },
    ],
  };
  const programDir = createProgram(runRoot, program, "deferprog");
  try {
    const runDirA = createRun(runRoot, plan("a"));
    const finding = { id: "F-1", version: 1, phaseId: "p1", kind: "defect", severity: "blocking", evidence: "src/a.ts:1 returns the wrong value", raisedBy: "M", status: "open", boundCandidateSha: "" };
    const events = [
      { type: "FINDING_RAISED", finding },
      { type: "TRIAGE_RECORDED", record: { itemId: "F-1", source: "finding", impact: "wrong-output", disposition: { kind: "fix", reason: "a wrong value" } } },
      { type: "DEFERRAL_RECORDED", deferral: { id: "DEF-1", itemId: "F-1", text: "deferred", test: "the cost test", toPhase: "c", status: "open" } },
    ];
    fs.writeFileSync(
      runPaths(runDirA).events,
      events.map((event, i) => JSON.stringify({ seq: i + 1, ts: new Date().toISOString(), kind: "event", event })).join("\n") + "\n",
    );
    fs.writeFileSync(
      path.join(programDir, "events.jsonl"),
      [
        { type: "NODE_STARTED", node: "a", runId: path.basename(runDirA), branch: "main", base: "main" },
        { type: "NODE_STATUS", node: "a", status: "done" },
      ]
        .map((event) => JSON.stringify({ ts: new Date().toISOString(), event }))
        .join("\n") + "\n",
    );
    schedulerTick(programDir, { runRoot, launch: () => undefined, maxTicks: 1 });
    const { state } = foldProgram(programDir);
    const planC = JSON.parse(fs.readFileSync(path.join(runPaths(path.join(runRoot, state.nodes["c"]!.runId!)).plan, "v1.json"), "utf8")) as {
      phases: Array<{ requirements?: Array<{ id: string; text: string; verify: string[] }> }>;
    };
    const req = (planC.phases[0].requirements ?? []).find((r) => r.id === "DEFER-F-1");
    assert.ok(req, "node c gets the deferred requirement");
    assert.deepEqual(req!.verify, ['test "carried F-1 is fixed"'], "a wrong-output deferral requires the FIX verification, not the cost test");
    assert.match(req!.text, /cost guard: the cost test/, "the cost test stays attached as context only");
  } finally {
    cleanupDir(runRoot);
    cleanupDir(repo.dir);
  }
});

test("plan 06i: a blocking finding from round 1 still open in round 2 gets a fresh classification", async () => {
  // A3: openFindings() includes every open finding whatever candidate it was
  // raised on, so the evaluator is asked again after the freeze cleared
  // itemChecks — it is not escalated without a re-prompt.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: { id: "p1", goal: "re-classify an earlier finding", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], provisional: false, rounds: 2 },
    workerScriptForAttempt: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep([]),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state, {
            findings: reviewer === "M" && (state.phase.round ?? 1) <= 1 ? [{ kind: "defect", severity: "blocking", evidence: "R1 (it works) is unmet: src/a.ts:1 broken" }] : [],
          }),
        },
      ],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const finding = phase.findings.find((f) => f.severity === "blocking" && f.status === "open");
    assert.ok(finding, "the round-1 blocking finding is still open in round 2");
    const record = (phase.triage ?? []).find((r) => r.itemId === finding!.id);
    assert.ok(record, "the earlier-candidate blocking finding has a triage record in round 2");
    assert.equal(record!.disposition?.kind, "fix", "it was re-classified, not escalated unclassified");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a majority-unmet item with no reviewer-filed finding has a disposition at EVALUATION_COMPLETED", async () => {
  // R5/C3: item outcomes run BEFORE triage, so the conductor-raised item
  // finding is dispositioned too.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "disposition an item finding",
      acceptance: ["R1 holds"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      provisional: false,
      rounds: 1,
      requirements: [{ id: "R1", title: "R1 holds", text: "R1 holds", arch: [], verify: ["review"] }],
    },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_SRC },
        { kind: "call-submit", tool: "submit_coverage", args: { items: [{ id: "R1", status: "done", where: ["src/a.ts:1"], tests: [] }], arch: [] } },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep([]),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, { findings: [], items: [{ id: "R1", verdict: "unmet", evidence: "src/a.ts:1 not done" }] }) },
      ],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const itemFinding = phase.findings.find((f) => f.itemId === "R1");
    assert.ok(itemFinding, "the unmet item raised a finding");
    const record = (phase.triage ?? []).find((r) => r.itemId === itemFinding!.id);
    assert.ok(record, "the item finding has a disposition at EVALUATION_COMPLETED");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a contradicted item check is re-prompted once and then escalates, never a silent classification", async () => {
  // OD-15(2): only a CONFIRMED check with an impact classifies a finding. A
  // contradicted verdict does not, so the owed check is re-prompted once and
  // then escalates.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: { id: "p1", goal: "contradicted check", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], provisional: false, rounds: 1 },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep([]),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state, {
            findings: reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "it works: src/a.ts:1 a defect" }] : [],
          }),
        },
      ],
    }),
    evaluatorScriptFor: (messageType, state) => {
      const raw = (state.phase.messages ?? []).filter((m) => m.type === messageType && m.state === "raw");
      const finding = (state.phase.findings ?? []).find((f) => f.status === "open");
      const step = {
        kind: "call-submit",
        tool: "submit_evaluation",
        args: {
          evaluations: raw.map((m) => ({ messageId: m.id, action: "publish", title: "reviewed item", summary: m.summary, context: m.context, evidence: m.evidence })),
          ...(messageType === "finding" && finding
            ? { itemChecks: [{ id: finding.id, verdict: "contradicted", impact: "judgement", evidence: "README.md:1 the evaluator disagreed" }] }
            : {}),
        },
      };
      return { hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator }, steps: [step, step] };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const finding = phase.findings.find((f) => f.raisedBy === "M")!;
    const record = (phase.triage ?? []).find((r) => r.itemId === finding.id);
    assert.equal(record?.disposition?.kind, "escalate", "a contradicted check does not classify, so it escalates");
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "item_check_rejected"), "the missing classification was re-prompted once");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a sameAs advisory downgrade with no evaluator impact is re-prompted and then escalates", async () => {
  // OD-15(1): a reviewer's severity label never classifies a record. With no
  // evaluator impact, the finding is unclassified: re-prompted once, then
  // escalated (never a stock trade-off).
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    // rounds: 2 so the round-2 sameAs re-raise actually runs (it needs a
    // repair round to exist); the downgrade path is what OD-15(1) requires.
    phase: { id: "p1", goal: "sameAs no impact", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], provisional: false, rounds: 2 },
    workerScriptForAttempt: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const finding = state.phase.findings.find((f) => f.raisedBy === "M");
      return {
        hello: defaultReviewerHello(),
        steps: [
          discoveryStep([]),
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: reviewArgs(reviewer, state, {
              findings:
                round <= 1 && reviewer === "M"
                  ? [{ kind: "defect", severity: "blocking", evidence: "it works: src/a.ts:1 a defect" }]
                  : round >= 2 && reviewer === "A" && finding
                    ? [{ kind: "defect", severity: "advisory", evidence: "a narrower point about the same code", sameAs: finding.id }]
                    : [],
            }),
          },
        ],
      };
    },
    // Provide a check for the finding id with NO impact: the harness will not
    // add its own (the id is covered), and the conductor skips it, so it is
    // owed and re-prompted.
    evaluatorScriptFor: (messageType, state) => {
      const raw = (state.phase.messages ?? []).filter((m) => m.type === messageType && m.state === "raw");
      const finding = (state.phase.findings ?? []).find((f) => f.status === "open");
      return {
        hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
        steps: [
          {
            kind: "call-submit",
            tool: "submit_evaluation",
            args: {
              evaluations: raw.map((m) => ({ messageId: m.id, action: "publish", title: "reviewed item", summary: m.summary, context: m.context, evidence: m.evidence })),
              ...(messageType === "finding" && finding ? { itemChecks: [{ id: finding.id, verdict: "confirmed", evidence: "README.md:1" }] } : {}),
            },
          },
          {
            kind: "call-submit",
            tool: "submit_evaluation",
            args: {
              evaluations: raw.map((m) => ({ messageId: m.id, action: "publish", title: "reviewed item", summary: m.summary, context: m.context, evidence: m.evidence })),
              ...(messageType === "finding" && finding ? { itemChecks: [{ id: finding.id, verdict: "confirmed", evidence: "README.md:1" }] } : {}),
            },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const finding = phase.findings.find((f) => f.raisedBy === "M")!;
    // The downgrade must have actually happened (the test fails if the
    // round-2 sameAs path was not exercised).
    assert.equal(finding.severity, "advisory", "the sameAs re-raise lowered it to advisory");
    assert.equal(finding.severityChangedBy, "reviewer", "the reviewer's sameAs is what lowered it");
    const record = (phase.triage ?? []).find((r) => r.itemId === finding.id);
    assert.equal(record?.disposition?.kind, "escalate", "an unclassified finding escalates, never a stock trade-off");
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "item_check_rejected"), "the missing impact was re-prompted once");
    assert.ok(
      phase.ownerRequests.some((r) => r.status === "open" && r.linkedFindingId === finding.id),
      "the escalation is an open owner request naming the finding",
    );
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a blocking-severity judgement finding is accepted as a trade-off and the phase converges", async () => {
  // A2: acceptance follows the disposition, never the reviewer's label. The
  // evaluator classifies M's blocking finding `judgement`, so its disposition
  // is a trade-off and the phase reaches DONE.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep([]),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state, {
            // Cite the acceptance item so the evaluator does not lower the
            // label: the test is about a BLOCKING finding accepted as a
            // trade-off, not about the severity-lowering rule.
            findings: reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "it works: src/a.ts:1 a style point the reviewer raised as blocking" }] : [],
          }),
        },
      ],
    }),
    // Override the harness default: classify the blocking finding `judgement`.
    evaluatorScriptFor: (messageType, state) => {
      const raw = (state.phase.messages ?? []).filter((m) => m.type === messageType && m.state === "raw");
      const finding = (state.phase.findings ?? []).find((f) => f.status === "open");
      return {
        hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
        steps: [
          {
            kind: "call-submit",
            tool: "submit_evaluation",
            args: {
              evaluations: raw.map((m) => ({ messageId: m.id, action: "publish", title: "reviewed item", summary: m.summary, context: m.context, evidence: m.evidence })),
              ...(messageType === "finding" && finding
                ? {
                    itemChecks: [
                      {
                        id: finding.id,
                        verdict: "confirmed",
                        impact: "judgement",
                        chosen: "accept the wording",
                        alternative: "repair it",
                        why: "it is a style point, not a wrong value",
                        evidence: "src/a.ts:1 the evaluator re-checked",
                      },
                    ],
                  }
                : {}),
            },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 20, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.raisedBy === "M")!;
    assert.equal(finding.severity, "blocking", "the reviewer's label is unchanged");
    const record = (setup.conductor.state.phase.triage ?? []).find((r) => r.itemId === finding.id);
    assert.equal(record?.disposition?.kind, "tradeoff", "the disposition is a trade-off");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a discovered decision justified only by a drafting decision that contradicts the golden source is a fix, not a trade-off", async () => {
  // The evaluator reads the golden note fixture and classifies the discovered
  // decision `contract` (it contradicts the golden source). Even after a 3-0
  // approve vote the disposition is fix and acceptance waits.
  // A fixture golden note. The plan's drafting decision (M's discovery) says
  // a settled offer may carry halt; the golden note says it never may.
  const GOLDEN = "A settled offer must never carry halt; a halted book settles as halt.";
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "fix a decision that contradicts the golden note",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      provisional: false,
      rounds: 1,
      golden: GOLDEN,
    },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => {
      // The discovered decision id is deterministic, so the static script can
      // name it with the candidate-sha token fake-pi substitutes at run time
      // (a seat's turn-1 script is written before M's discovery is recorded).
      const ballots = [
        {
          decisionId: "D-p1-$TT_CANDIDATE_SHA8-disc-M-1",
          vote: "approve" as const,
          rationale: "matches the plan's drafting decision D9",
          evidence: ["the plan's drafting decision D9"],
        },
      ];
      return {
        hello: defaultReviewerHello(),
        steps: [
          discoveryStep(reviewer === "M" ? [disclosure("a settled offer may carry halt", "delegated")] : []),
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, { ballots }) },
        ],
      };
    },
    // The evaluator re-checks the drafting decision against the golden note
    // and cites it; the conductor verifies the citation.
    evaluatorScriptFor: (messageType, state) => {
      const raw = (state.phase.messages ?? []).filter((m) => m.type === messageType && m.state === "raw");
      const decision = (state.phase.decisions ?? []).find((d) => d.source === "reviewer-discovered");
      return {
        hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
        steps: [
          {
            kind: "call-submit",
            tool: "submit_evaluation",
            args: {
              evaluations: raw.map((m) => ({ messageId: m.id, action: "publish", title: "reviewed item", summary: m.summary, context: m.context, evidence: m.evidence })),
              ...(messageType === "tradeoff" && decision
                ? {
                    itemChecks: [
                      {
                        id: decision.id,
                        verdict: "confirmed",
                        impact: "contract",
                        evidence: `the golden note says: ${GOLDEN}`,
                      },
                    ],
                  }
                : {}),
            },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.notEqual(phase.phase, "DONE", "the decision must not be accepted away");
    const decision = phase.decisions.find((d) => d.source === "reviewer-discovered")!;
    assert.ok(decision, "the discovered decision exists");
    const approvals = phase.ballots.filter((b) => b.decisionId === decision.id && b.vote === "approve");
    assert.equal(approvals.length, 3, "all three seats approved it");
    const record = (phase.triage ?? []).find((r) => r.itemId === decision.id);
    assert.ok(record, "the decision has a triage record");
    assert.equal(record!.disposition?.kind, "fix", "a contract contradiction is a fix, not a trade-off");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a valid classification survives an item-check re-prompt (F-A-99)", async () => {
  // A3: the first submission confirms X wrong-output with a valid anchor and
  // leaves Y unclassified. Persisting X before the re-prompt means the retry
  // owes only Y, and X keeps its fix disposition — it is not discarded and
  // escalated because the submission was incomplete.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: { id: "p1", goal: "keep a valid check", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], provisional: false, rounds: 1 },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep([]),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state, {
            findings:
              reviewer === "M"
                ? [
                    { kind: "defect", severity: "blocking", evidence: "it works: src/a.ts:1 defect one" },
                    { kind: "defect", severity: "blocking", evidence: "it works: src/a.ts:1 defect two" },
                  ]
                : [],
          }),
        },
      ],
    }),
    evaluatorScriptFor: (messageType, state) => {
      const raw = (state.phase.messages ?? []).filter((m) => m.type === messageType && m.state === "raw");
      const evals = raw.map((m) => ({ messageId: m.id, action: "publish", title: "reviewed item", summary: m.summary, context: m.context, evidence: m.evidence }));
      const findings = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.severity === "blocking");
      const [x, y] = findings;
      const check = (id: string, impact?: string) => ({ id, verdict: "confirmed", evidence: "src/a.ts:1 the evaluator re-checked the candidate", ...(impact ? { impact } : {}) });
      const step1 = {
        kind: "call-submit",
        tool: "submit_evaluation",
        args: { evaluations: evals, ...(messageType === "finding" && x && y ? { itemChecks: [check(x.id, "wrong-output"), check(y.id)] } : {}) },
      };
      const step2 = {
        kind: "call-submit",
        tool: "submit_evaluation",
        args: { evaluations: evals, ...(messageType === "finding" && x && y ? { itemChecks: [check(x.id), check(y.id, "judgement")] } : {}) },
      };
      return { hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator }, steps: [step1, step2] };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const findings = phase.findings.filter((f) => f.raisedBy === "M");
    assert.equal(findings.length, 2, "two blocking findings are open");
    const record = (phase.triage ?? []).find((r) => findings.some((f) => f.id === r.itemId) && r.disposition?.kind === "fix");
    assert.ok(record, "the first finding's confirmed wrong output survived the re-prompt as a fix");
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "item_check_rejected"), "the missing classification was re-prompted once");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: a title refusal keeps a validated item check (F-A-100)", async () => {
  // A3: the evaluator confirms X wrong-output with a valid anchor but
  // publishes an overlong title. The title-only retry must not erase the
  // classification: X keeps its fix disposition instead of escalating.
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    phase: { id: "p1", goal: "keep a check across a title refusal", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], provisional: false, rounds: 1 },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: WRITE_SRC }, submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        discoveryStep([]),
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/a.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state, {
            findings: reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "it works: src/a.ts:1 a defect" }] : [],
          }),
        },
      ],
    }),
    evaluatorScriptFor: (messageType, state) => {
      const raw = (state.phase.messages ?? []).filter((m) => m.type === messageType && m.state === "raw");
      const evals = (title: string) => raw.map((m) => ({ messageId: m.id, action: "publish", title, summary: m.summary, context: m.context, evidence: m.evidence }));
      const finding = (state.phase.findings ?? []).find((f) => f.status === "open");
      const check = (impact?: string) => ({ id: finding!.id, verdict: "confirmed", evidence: "src/a.ts:1 the evaluator re-checked the candidate", ...(impact ? { impact } : {}) });
      const overlong = "x".repeat(200);
      const step1 = {
        kind: "call-submit",
        tool: "submit_evaluation",
        args: { evaluations: evals(overlong), ...(messageType === "finding" && finding ? { itemChecks: [check("wrong-output")] } : {}) },
      };
      const step2 = {
        kind: "call-submit",
        tool: "submit_evaluation",
        args: { evaluations: evals("reviewed item"), ...(messageType === "finding" && finding ? { itemChecks: [check()] } : {}) },
      };
      return { hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator }, steps: [step1, step2, step2] };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const finding = phase.findings.find((f) => f.raisedBy === "M")!;
    const record = (phase.triage ?? []).find((r) => r.itemId === finding.id);
    assert.equal(record?.disposition?.kind, "fix", "the confirmed wrong output survived the title-only re-prompt");
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "evaluation_title_rejected"), "the overlong title was refused once");
  } finally {
    await teardown(setup);
  }
});

test("plan 06i: two late discoveries from one reviewer get distinct ids and both stay visible (F-A-98)", async () => {
  // A3: late discoveries never join `decisions`, so numbering from the
  // decision count reused an id and the second TRIAGE_RECORDED overwrote the
  // first (C3). Two late batches in one REVIEWING window must be two records.
  let bDispatches = 0;
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: { ...FAST, reviewMs: 30_000 },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > sum.js" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [disclosure("the only known choice")], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const d = state.phase.decisions.find((x) => x.source === "worker")!;
      if (reviewer === "B" && ++bDispatches === 1) {
        return {
          hello: defaultReviewerHello(),
          steps: [
            { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
            { kind: "wait-for-prompt" },
          ],
        };
      }
      const late = (choice: string) => ({ ...disclosure(choice), classProposal: "delegated" });
      const first = reviewer === "B" ? [late("LATE-ONE: validates both arguments before adding")] : [];
      const second = reviewer === "B" ? [late("LATE-TWO: rejects a negative operand")] : [];
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: first } },
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: second } },
          { kind: "wait-for-prompt" },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: reviewArgs(reviewer, state, {
              ballots: [{ decisionId: d.id, vote: "approve", rationale: "consistent with the goal", evidence: ["src/sum.js:1"] }],
              findings: [],
            }),
          },
        ],
      };
    },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const lateRecords = (phase.triage ?? []).filter((r) => r.disposition?.kind === "escalate" && /LATE-(ONE|TWO)/.test(r.disposition.reason));
    assert.equal(lateRecords.length, 2, "both late discoveries have their own triage record");
    assert.notEqual(lateRecords[0].itemId, lateRecords[1].itemId, "the two late ids are distinct");
    const requests = phase.ownerRequests.filter((r) => r.status === "open" && r.blocking === false && /LATE-(ONE|TWO)/.test(r.reason));
    assert.equal(requests.length, 2, "each late discovery has its own visible, non-blocking owner request");
    const summary = prSummary(setup.runDir, setup.plan);
    assert.match(summary, /LATE-ONE/);
    assert.match(summary, /LATE-TWO/);
  } finally {
    await teardown(setup);
  }
});
