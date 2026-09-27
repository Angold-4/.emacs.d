// #+TT_MODELS (design §2.1): a plan's per-role provider/model must reach each
// role's launch, and only that role's. The Conductor derives its
// `providerModelFor` from `plan.models` (the same one-liner `tt start` uses),
// and `launchArgs` adds `--provider`/`--model`. fake-pi records the argv it
// was actually launched with (`FAKE_PI_ARGV_LOG`), so this asserts the real
// command line, not the intent.
//
// The flow is blocker-panel.test.ts's: the worker submits, B raises a blocker,
// the evaluator publishes it and the panel votes — enough to launch all four
// roles in one short run.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { ROLE_TOOLS, type PlanModels, type Role } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";

// Short, but load-tolerant: the run only needs to reach the panel, not DONE.
const FAST = {
  abortGraceMs: 120,
  termGraceMs: 120,
  helloTimeoutMs: 30_000,
  checkMs: 60_000,
  freezeMs: 30_000,
  workerAttemptMs: 120_000,
  reviewMs: 120_000,
  probeMs: 30_000,
  evaluateMs: 120_000,
  panelMs: 120_000,
  inboxPollMs: 50,
};

const BLOCKER_EVIDENCE = "src/cancel.ts:10 the cancel path can deadlock";

function submitPhaseStep() {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

/** B raises a blocker on the first candidate; every reviewer submits turn 1
 * and turn 2 so the run reaches EVALUATING. */
function blockerReviewer() {
  return (reviewer: Reviewer, state: State) => {
    const raise = reviewer === "B" && !(state.phase.messages ?? []).some((m) => m.type === "blocker");
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
            phaseId: state.phase.phaseId,
            candidateSha: state.phase.candidate?.sha,
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
            ballots: [],
            findings: [],
            ...(raise ? { blockers: [{ kind: "defect", evidence: BLOCKER_EVIDENCE }] } : {}),
          },
        },
      ],
    };
  };
}

/** Publish the raw blocker so its panel is dispatched. */
function blockerEvaluator() {
  return (messageType: string) => ({
    hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
    steps:
      messageType === "blocker"
        ? [
            {
              kind: "call-submit",
              tool: "submit_evaluation",
              args: {
                evaluations: [
                  {
                    messageId: "B-1",
                    action: "publish",
                    title: "defect blocking: the cancel path can deadlock",
                    summary: "The reviewer says the cancel path can deadlock.",
                    context: BLOCKER_EVIDENCE,
                    evidence: [BLOCKER_EVIDENCE],
                  },
                ],
              },
            },
          ]
        : [],
  });
}

/** Every dispatch of ROLE, each one's recorded argv. */
function argvFor(setup: TestConductorSetup, role: Role, dir: string): string[][] {
  const file = path.join(dir, `${role}.jsonl`);
  if (!fs.existsSync(file)) return [];
  return fs
    .readFileSync(file, "utf8")
    .split("\n")
    .filter(Boolean)
    .map((line) => (JSON.parse(line) as { argv: string[] }).argv);
}

function flagsOf(argv: string[]): { provider?: string; model?: string } {
  const out: { provider?: string; model?: string } = {};
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--provider") out.provider = argv[++i];
    else if (argv[i] === "--model") out.model = argv[++i];
  }
  return out;
}

/** Assert ROLE was launched at least once and every launch carried exactly
 * EXPECTED (no provider/model at all when EXPECTED is undefined). */
function assertRoleFlags(setup: TestConductorSetup, role: Role, dir: string, expected: { provider?: string; model?: string } | undefined): void {
  const launches = argvFor(setup, role, dir);
  assert.ok(launches.length > 0, `${role} was never launched`);
  for (const argv of launches) {
    assert.deepEqual(flagsOf(argv), expected ?? {}, `${role} launch argv: ${argv.join(" ")}`);
  }
}

const ALL_MODELS: PlanModels = {
  worker: { provider: "p-w", model: "m-w" },
  reviewer: { provider: "p-r", model: "m-r" },
  evaluator: { provider: "p-e", model: "m-e" },
  panel: { provider: "p-p", model: "m-p" },
};

test("plan-models: a plan's models launch every role with its own --provider/--model", async () => {
  const argvDir = fs.mkdtempSync("/tmp/tt-plan-models-");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    models: ALL_MODELS,
    extraEnv: { FAKE_PI_ARGV_LOG: argvDir },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: blockerReviewer(),
    evaluatorScriptFor: blockerEvaluator(),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    // The panel is the last role to launch (the blocker is published and then
    // voted), so once its argv is recorded all four have run.
    await waitFor(() => argvFor(setup, "panel", argvDir).length > 0, 90_000, 25, setup.runDir);
    assertRoleFlags(setup, "worker", argvDir, { provider: "p-w", model: "m-w" });
    assertRoleFlags(setup, "reviewer", argvDir, { provider: "p-r", model: "m-r" });
    assertRoleFlags(setup, "evaluator", argvDir, { provider: "p-e", model: "m-e" });
    assertRoleFlags(setup, "panel", argvDir, { provider: "p-p", model: "m-p" });
    // What ran is visible: the phase chart names each dispatching role's
    // model, including the evaluator's (undefined before #+TT_MODELS).
    const loop = fs.readFileSync(path.join(setup.runDir, "views", "loop.txt"), "utf8");
    assert.match(loop, /IMPLEMENTING.*worker - model m-w/);
    assert.match(loop, /REVIEWING.*M, A, B - model m-r/);
    assert.match(loop, /EVALUATING.*evaluator, panel - model m-e/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(argvDir, { recursive: true, force: true });
  }
});

test("plan-models: setting only reviewer changes only the reviewers' arguments", async () => {
  const argvDir = fs.mkdtempSync("/tmp/tt-plan-models-");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    models: { reviewer: { provider: "p-r", model: "m-r" } },
    extraEnv: { FAKE_PI_ARGV_LOG: argvDir },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: blockerReviewer(),
    evaluatorScriptFor: blockerEvaluator(),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => argvFor(setup, "panel", argvDir).length > 0, 90_000, 25, setup.runDir);
    assertRoleFlags(setup, "worker", argvDir, undefined);
    assertRoleFlags(setup, "reviewer", argvDir, { provider: "p-r", model: "m-r" });
    assertRoleFlags(setup, "evaluator", argvDir, undefined);
    assertRoleFlags(setup, "panel", argvDir, undefined);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(argvDir, { recursive: true, force: true });
  }
});

test("plan-models: a plan without models launches every role with neither flag", async () => {
  const argvDir = fs.mkdtempSync("/tmp/tt-plan-models-");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    extraEnv: { FAKE_PI_ARGV_LOG: argvDir },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: blockerReviewer(),
    evaluatorScriptFor: blockerEvaluator(),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => argvFor(setup, "panel", argvDir).length > 0, 90_000, 25, setup.runDir);
    assertRoleFlags(setup, "worker", argvDir, undefined);
    assertRoleFlags(setup, "reviewer", argvDir, undefined);
    assertRoleFlags(setup, "evaluator", argvDir, undefined);
    assertRoleFlags(setup, "panel", argvDir, undefined);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(argvDir, { recursive: true, force: true });
  }
});
