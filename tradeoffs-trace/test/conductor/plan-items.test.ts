// Plan 06b: every point of a structured plan is a parsed, identified item the
// conductor carries mechanically — the worker's coverage, the check
// resolution, the reviewers' per-item verdicts and the acceptance decision
// (refs/06_ref_plan_format.md). Fake-pi end to end.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor, type FakePiStep, type TestConductorSetup } from "./harness.ts";
import { rebuildState, runPaths } from "../../src/conductor.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { readLog } from "../../src/effects/log.ts";
import { prSummary } from "../../src/view.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

/** Runs the real CLI asynchronously: the conductor runs in THIS process, so a
 * synchronous `execFileSync` would block the event loop and the inbox poll
 * that applies the command. Returns the CLI's stdout and exit code. */
function runCli(args: string[]): Promise<{ stdout: string; code: number }> {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, TT_NOTIFY_COMMAND: ":" } });
    let stdout = "";
    child.stdout.on("data", (d) => (stdout += String(d)));
    child.on("close", (code) => resolve({ stdout, code: code ?? 0 }));
  });
}

type Setup = Awaited<ReturnType<typeof setupConductor>>;

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

/** One architecture item (with a `:WHERE:` symbol), two requirements (one
 * test verify, one review) and one constraint. */
const ITEMS = {
  architecture: [{ id: "A1", title: "Round", text: "interface Round { n: number }", tags: ["data"], where: "src/core/rounds.ts" }],
  requirements: [
    { id: "R1", title: "R1 proves it", text: "R1 proves it", arch: ["A1"], verify: ['test "R1 proves it"'] },
    { id: "R2", title: "R2 reviewed", text: "R2 is judged by review", arch: [], verify: ["review"] },
  ],
  constraints: [{ id: "C1", title: "C1 holds", text: "C1 must not change", verify: ["review"] }],
};

const CHECK_OUTPUT = "printf 'ok 1 - R1 proves it\\n'";

const WRITE_ROUNDS = "mkdir -p src/core && printf 'interface Round { n: number }\\n' > src/core/rounds.ts";

function coverage(overrides: Record<string, unknown> = {}, archOverrides: Record<string, unknown> = {}) {
  return {
    items: [
      { id: "R1", status: "done", where: ["src/core/rounds.ts:1"], tests: ["R1 proves it"], ...(overrides.R1 as object) },
      { id: "R2", status: "done", where: [], tests: [], ...(overrides.R2 as object) },
      { id: "C1", status: "done", where: [], tests: [], ...(overrides.C1 as object) },
    ],
    arch: [{ id: "A1", fits: "yes", where: ["src/core/rounds.ts"], ...archOverrides }],
  };
}

/** A full item verdict set; `verdicts` overrides by id. */
function review(verdicts: Record<string, string>, evidence = "src/core/rounds.ts:1") {
  return {
    items: [
      { id: "R1", verdict: verdicts.R1 ?? "met", evidence },
      { id: "R2", verdict: verdicts.R2 ?? "met", evidence },
      { id: "C1", verdict: verdicts.C1 ?? "met", evidence },
    ],
    arch: [{ id: "A1", verdict: verdicts.A1 ?? "fits", evidence }],
  };
}

function reviewArgs(reviewer: Reviewer, candidateSha: string | undefined, contractVersion: unknown, items: unknown) {
  return {
    reviewer,
    phaseId: "p1",
    candidateSha,
    contractVersion,
    correctionStatements: [],
    findingStatements: [],
    ...(items as object),
  };
}

/** A reviewer that reads one candidate file (so a met verdict may cite it),
 * then submits one review with `items`. */
function reviewerScript(reviewer: Reviewer, candidateSha: string | undefined, contractVersion: unknown, items: unknown): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
      { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, candidateSha, contractVersion, items) },
    ],
  };
}

async function teardown(setup: Setup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

test("plan-items: the freeze is refused until submit_coverage covers every item; a partial note becomes a trade-off", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        // R1 omitted: the coverage is incomplete and submit_phase must be refused.
        { kind: "call-submit", tool: "submit_coverage", args: { items: [{ id: "R2", status: "done", where: [], tests: [] }, { id: "C1", status: "done", where: [], tests: [] }], arch: [{ id: "A1", fits: "yes", where: [] }] } },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
        // Complete, with a partial note on R2.
        { kind: "call-submit", tool: "submit_coverage", args: coverage({ R2: { status: "partial", note: "only the first half is implemented" } }) },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
        { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({})) },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase), 90_000, 50, setup.runDir);
    const events = readEvents(setup.runDir);
    const refused = events.filter((r) => r.kind === "coverage_refused");
    assert.ok(refused.length >= 1, "the incomplete coverage refused the freeze");
    assert.ok((refused[0].event as { issues: string[] }).issues.some((i) => i.includes("R1")));
    // The partial note is a trade-off message (coverage itself is reset at
    // each freeze, per OD-1 A2).
    const coverageMessage = (setup.conductor.state.phase.messages ?? []).find((m) => (m.sourceRecordId ?? "").endsWith("-R2") && (m.sourceRecordId ?? "").startsWith("coverage-"));
    assert.ok(coverageMessage, "the partial note became a trade-off message");
    assert.match(coverageMessage!.summary, /only the first half/);
    // OD-2 (disc-M-92): the note is keyed by the candidate too.
    assert.match(coverageMessage!.sourceRecordId ?? "", new RegExp(`^coverage-${setup.conductor.state.phase.candidate!.sha.slice(0, 8)}-R2$`));
  } finally {
    await teardown(setup);
  }
});

test("plan-items: a requirement whose test verify is missing from the check output is a blocking finding anchored to it", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({})),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "R1"), 90_000, 50, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.itemId === "R1")!;
    assert.equal(finding.severity, "blocking");
    assert.match(finding.evidence, /R1 proves it/);
    assert.match(finding.evidence, /missing/);
  } finally {
    await teardown(setup);
  }
});

test("plan-items: a review omitting one item's verdict is incomplete and re-asked", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const items = review({});
      const without = { ...items, items: (items.items as Array<{ id: string }>).filter((i) => i.id !== "R2") };
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          // The first omits R2; the conductor refuses it and the corrected one follows.
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, without) },
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, items) },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "incomplete_review_rejected"), 90_000, 50, setup.runDir);
    const rejected = readEvents(setup.runDir).filter((r) => r.kind === "incomplete_review_rejected");
    assert.ok(rejected.length >= 1);
    const itemIssues = (rejected[0].event as { itemIssues?: string[] }).itemIssues ?? [];
    assert.ok(itemIssues.some((i) => i.includes("omits a verdict for R2")), `R2 named: ${itemIssues.join("; ")}`);
  } finally {
    await teardown(setup);
  }
});

test("plan-items: two of three seats judging R2 unmet raises a blocking finding anchored to R2, and only R2 is a repair item", async () => {
  const promptLog = `/tmp/tt-items-prompt-${process.pid}-${Date.now()}.txt`;
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScriptForAttempt: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const verdicts = reviewer === "B" ? { R2: "met" } : { R2: "unmet" };
      return reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review(verdicts));
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "R2"), 90_000, 50, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.itemId === "R2")!;
    assert.equal(finding.severity, "blocking");
    assert.equal(finding.itemId, "R2");
    // The repair prompt lists R2, not R1.
    await waitFor(() => {
      try {
        return fs.readFileSync(promptLog, "utf8").includes("REPAIR");
      } catch {
        return false;
      }
    }, 60_000, 100, setup.runDir).catch(() => undefined);
    const prompt = fs.readFileSync(promptLog, "utf8");
    assert.match(prompt, /R2/);
    assert.doesNotMatch(prompt, /^\- R1 \(requirement\)/m);
  } finally {
    await teardown(setup);
    try {
      fs.rmSync(promptLog, { force: true });
    } catch {
      // best effort
    }
  }
});

test("plan-items: the evaluator overturns a majority unmet verdict the passing test contradicts, and counts it against the seat", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    // R1's own test verify passed, so two unmet verdicts are contradicted by the code.
    reviewerScriptFor: (reviewer, state) => reviewerScript(
      reviewer,
      state.phase.candidate?.sha,
      state.phase.contract.contractVersion,
      review(reviewer === "B" ? {} : { R1: "unmet" }),
    ),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase), 90_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE", "the overturned verdict does not block");
    const overturns = setup.conductor.state.phase.overturns ?? [];
    // Both contradicted seats are overturned and counted (finding M-3).
    assert.equal(overturns.length, 2);
    assert.deepEqual(overturns.map((o) => o.id), ["R1", "R1"]);
    assert.deepEqual(overturns.map((o) => o.seat).sort(), ["A", "M"]);
    assert.ok(overturns.every((o) => o.effect === "flip"));
    assert.ok(!setup.conductor.state.phase.findings.some((f) => f.itemId === "R1"), "no finding is raised for the overturned item");
    // The seat's overturn count shows in `tt summary`'s PR body.
    const { prSummary } = await import("../../src/view.ts");
    const md = prSummary(setup.runDir, setup.plan);
    assert.match(md, /Overturned verdicts \(counted against a seat\): .*\b1\b/);
  } finally {
    await teardown(setup);
  }
});

test("plan-items: an evidence requirement parks the phase AWAITING_OWNER until tt evidence records it, then it is accepted", async () => {
  const evidenceItems = {
    architecture: [],
    requirements: [
      { id: "R1", title: "R1 proves it", text: "R1 proves it", arch: [], verify: ['test "R1 proves it"'] },
      { id: "R2", title: "R2 owner run", text: "the owner live run is recorded", arch: [], verify: ["evidence"] },
    ],
    constraints: [],
  };
  const setup = await setupConductor({
    items: evidenceItems,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        {
          kind: "call-submit",
          tool: "submit_coverage",
          args: {
            items: [
              { id: "R1", status: "done", where: ["src/core/rounds.ts:1"], tests: ["R1 proves it"] },
              { id: "R2", status: "done", where: [], tests: [] },
            ],
            arch: [],
          },
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, {
            items: [
              { id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1" },
              { id: "R2", verdict: "met", evidence: "src/core/rounds.ts:1" },
            ],
            arch: [],
          }),
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 50, setup.runDir);
    // The fallback request names the item the owner must record.
    assert.ok(setup.conductor.state.phase.ownerRequests.some((r) => r.status === "open" && /R2/.test(r.reason)));
    // Record it with the real `tt evidence` command.
    const cli = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));
    const out = execFileSync(process.execPath, [cli, "evidence", setup.runDir, "R2", "the live run is in NOTES.md"], { encoding: "utf8", env: { ...process.env, TT_NOTIFY_COMMAND: ":" } });
    assert.match(out, /evidence (recorded for|.*for) R2/);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    assert.deepEqual(setup.conductor.state.phase.itemEvidence?.map((e) => e.id), ["R2"]);
  } finally {
    await teardown(setup);
  }
});

test("plan-items: a met verdict citing only the worker's anchors with no file the reviewer read is refused and re-asked", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        // No `read`: the met evidence cites only the worker's own anchor.
        { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({})) },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "incomplete_review_rejected"), 90_000, 50, setup.runDir);
    const rejected = readEvents(setup.runDir).filter((r) => r.kind === "incomplete_review_rejected");
    assert.ok(
      rejected.some((r) => ((r.event as { itemIssues?: string[] }).itemIssues ?? []).some((i) => /read yourself/.test(i))),
      `the reviewer was re-asked to cite a file it read: ${JSON.stringify(rejected[0]?.event)}`,
    );
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: the evaluator's substantive re-check overturns a review-only majority (R3b)", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    // R2 is review-only: all three seats judge it unmet, so only the
    // evaluator's own re-check can contradict the majority.
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({ R2: "unmet" })),
    evaluatorScriptFor: () => ({
      hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations: [],
            itemChecks: [
              { id: "R2", verdict: "contradicted", evidence: "src/core/rounds.ts:1 implements R2" },
              { id: "R1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
              { id: "C1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
              { id: "A1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
            ],
          },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase), 120_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE", "the contradicted majority did not block");
    assert.ok(!setup.conductor.state.phase.findings.some((f) => f.itemId === "R2"), "no finding is raised for the contradicted item");
    assert.ok((setup.conductor.state.phase.itemChecks ?? []).some((c) => c.itemId === "R2" && c.verdict === "contradicted"));
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: an old-format phase started through Emacs owes submit_phase only and its prompts never mention submit_coverage", async () => {
  // 1. Run the REAL Emacs parser on an old-format org file.
  const dir = fs.mkdtempSync("/tmp/tt-r1-");
  const orgPath = path.join(dir, "PLAN.org");
  fs.writeFileSync(
    orgPath,
    ["#+TITLE: old", "#+TT_REPO: /tmp/tt-r1-repo", "#+TT_BRANCH: main", "", "* Phase 1: p", "  :PROPERTIES:", "  :ID: p1", "  :CHECKS: true", "  :END:", "  Goal: g", "  Acceptance:", "  - it works"].join("\n") + "\n",
  );
  const emacsLoad = fileURLToPath(new URL("../../../test/tradeoffs-trace-test.el", import.meta.url));
  const coreDir = fileURLToPath(new URL("../../../core", import.meta.url));
  const testDir = fileURLToPath(new URL("../../../test", import.meta.url));
  const out = execFileSync(
    "emacs",
    [
      "--batch",
      "-Q",
      "-L",
      coreDir,
      "-L",
      testDir,
      "-l",
      emacsLoad,
      "--eval",
      `(with-temp-buffer (insert-file-contents "${orgPath}") (org-mode) (setq buffer-file-name "${orgPath}") (princ (json-encode (plist-get (+tt-parse-plan) :plan))))`,
    ],
    { encoding: "utf8" },
  );
  const parsed = JSON.parse(out) as { phases: Array<{ itemsSynthesized?: boolean; requirements?: unknown[] }> };
  const phase = parsed.phases[0] as import("../../src/conductor.ts").RunPlanPhase;
  assert.equal(phase.itemsSynthesized, true, "the Emacs parser marks the old format synthesized");
  assert.ok(Array.isArray(phase.requirements) && phase.requirements.length > 0, "the parser still emits R1..Rn");
  // 2. Start a run with that exact phase and capture the worker prompt.
  const promptLog = path.join(dir, "prompts.txt");
  const setup = await setupConductor({
    phase,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_review",
          args: { reviewer, phaseId: "p1", candidateSha: state.phase.candidate?.sha, contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [] },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    const prompt = fs.readFileSync(promptLog, "utf8");
    assert.doesNotMatch(prompt, /submit_coverage/, "the worker prompt never mentions submit_coverage");
    assert.doesNotMatch(prompt, /Architecture:|Requirements:/, "the worker prompt has no item checklist");
  } finally {
    await teardown(setup);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06b: a repair attempt that skips submit_coverage cannot freeze on the previous attempt's coverage", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        ...(attempt === 1 ? [{ kind: "call-submit", tool: "submit_coverage", args: coverage() }] : []),
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review((state.phase.round ?? 1) === 1 ? { R2: "unmet" } : {})),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => readEvents(setup.runDir).some((r) => r.kind === "coverage_refused" && (r.event as { at?: string }).at === "submit_phase"),
      120_000,
      50,
      setup.runDir,
    );
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: a review-only majority unmet item with no evaluator item check is re-prompted, then blocks as unchecked", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({ R2: "unmet" })),
    evaluatorScriptFor: () => ({
      hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
      steps: [
        { kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [] } },
        { kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "R2"), 120_000, 50, setup.runDir);
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "item_check_rejected"), "the evaluator was re-prompted");
    const check = (setup.conductor.state.phase.itemChecks ?? []).find((c) => c.itemId === "R2");
    assert.equal(check?.verdict, "unchecked");
    const finding = setup.conductor.state.phase.findings.find((f) => f.itemId === "R2")!;
    assert.match(finding.evidence, /unchecked/);
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: an evaluator item check with an invalid anchor never overturns a majority", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({ R2: "unmet" })),
    evaluatorScriptFor: () => ({
      hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations: [],
            itemChecks: [
              { id: "R2", verdict: "contradicted", evidence: "src/nonexistent.ts:1 proves it" },
              { id: "R1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
              { id: "C1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
              { id: "A1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
            ],
          },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "R2"), 120_000, 50, setup.runDir);
    // The contradicted check cited a file that does not exist, so the majority stands.
    assert.ok(setup.conductor.state.phase.findings.some((f) => f.itemId === "R2"));
  } finally {
    await teardown(setup);
  }
});

test("plan 06c: a confirmed evaluator item check with invalid anchors is recorded as unchecked by evaluator", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({ R2: "unmet" })),
    // A `confirmed` check whose only anchor does not exist in the candidate is
    // recorded `unchecked by evaluator`, never as a confirmation.
    evaluatorScriptFor: () => ({
      hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations: [],
            itemChecks: [
              { id: "R2", verdict: "confirmed", evidence: "src/nonexistent.ts:1 confirms it" },
              { id: "R1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
              { id: "C1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
              { id: "A1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
            ],
          },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => (setup.conductor.state.phase.itemChecks ?? []).some((c) => c.itemId === "R2"), 120_000, 50, setup.runDir);
    const check = (setup.conductor.state.phase.itemChecks ?? []).find((c) => c.itemId === "R2");
    assert.equal(check?.verdict, "unchecked", "a confirmed check with invalid anchors is unchecked");
    assert.match(check?.evidence ?? "", /unchecked by evaluator/);
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: the criterion-amended path's new attempt owes its own coverage", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        ...(attempt === 1 ? [{ kind: "call-submit", tool: "submit_coverage", args: coverage() }] : []),
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [],
            assumptions: [],
            deviations: [],
            ...(attempt === 1 ? { criterionDispute: { criterion: "R2 is judged by review", why: "no candidate can satisfy it", proposedWording: "R2 is judged by review v2" } } : {}),
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const amendment = state.phase.decisions.find((d) => d.amendment && d.amendment.status === "proposed");
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              ...reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({})),
              ...(amendment ? { ballots: [{ decisionId: amendment.id, vote: "approve", rationale: "the wording is unsatisfiable", evidence: ["src/core/rounds.ts:1"] }] } : {}),
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => readEvents(setup.runDir).some((r) => r.kind === "coverage_refused" && (r.event as { at?: string }).at === "submit_phase"),
      150_000,
      50,
      setup.runDir,
    );
    // The amendment applied and its new attempt refused a coverage-free freeze.
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "event" && (r.event as { type?: string }).type === "CRITERION_AMENDED"));
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: an evaluator that times out still records the item blocker as unchecked", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({ R2: "unmet" })),
    // The evaluator settles without ever submitting: its dispatch times out.
    evaluatorScriptFor: () => ({ hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator }, steps: [] }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "R2"), 150_000, 50, setup.runDir);
    const check = (setup.conductor.state.phase.itemChecks ?? []).find((c) => c.itemId === "R2");
    assert.equal(check?.verdict, "unchecked", "the timeout path records unchecked before the blocker");
    assert.match(setup.conductor.state.phase.findings.find((f) => f.itemId === "R2")!.evidence, /unchecked/);
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: an item blocker is never raised without an evaluator item check", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    // R2 is only partial: it blocks, and ODP-2 requires a check even then.
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({ R2: "partial" })),
    evaluatorScriptFor: () => ({
      hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
      steps: [
        { kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [] } },
        { kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "R2"), 120_000, 50, setup.runDir);
    assert.ok((setup.conductor.state.phase.itemChecks ?? []).some((c) => c.itemId === "R2"), "a check is recorded for the blocking item");
    const finding = setup.conductor.state.phase.findings.find((f) => f.itemId === "R2")!;
    assert.match(finding.evidence, /evaluator re-check:/);
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: tt evidence on an old-format phase is rejected", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_review", args: { reviewer, phaseId: "p1", candidateSha: state.phase.candidate?.sha, contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    fs.mkdirSync(`${setup.runDir}/inbox`, { recursive: true });
    fs.writeFileSync(`${setup.runDir}/inbox/evidence-1.json`, JSON.stringify({ type: "evidence", item: "R1", text: "x" }));
    await waitFor(() => fs.existsSync(`${setup.runDir}/inbox/rejected/evidence-1.reason.txt`), 60_000, 50, setup.runDir);
    const reason = fs.readFileSync(`${setup.runDir}/inbox/rejected/evidence-1.reason.txt`, "utf8");
    assert.match(reason, /no structured items/);
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: an old-format worker owes no submit_coverage", async () => {
  const setup = await setupConductor({
    // No `items`: the old format. The worker calls submit_phase only.
    checks: [CHECK_OUTPUT],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_review",
          args: { reviewer, phaseId: "p1", candidateSha: state.phase.candidate?.sha, contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [] },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    assert.ok(!readEvents(setup.runDir).some((r) => r.kind === "coverage_refused"), "no coverage was demanded or refused");
    assert.equal(setup.conductor.state.phase.contract.requirements, undefined, "the old format stays unstructured");
  } finally {
    await teardown(setup);
  }
});

test("plan-items: a verdict citing a command the reviewer did not run is refused, and a run command counts", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const cv = state.phase.contract.contractVersion;
      const cand = state.phase.candidate?.sha;
      const cmd = "./run-checks.sh";
      const withCommand = (evidence: string) => ({
        items: [
          { id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1" },
          { id: "R2", verdict: "unmet", evidence },
          { id: "C1", verdict: "met", evidence: "src/core/rounds.ts:1" },
        ],
        arch: [{ id: "A1", verdict: "fits", evidence: "src/core/rounds.ts:1" }],
      });
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          // A command the reviewer never ran is refused.
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, cand, cv, withCommand("`" + cmd + "` fails")) },
          // Now run it (any command, no tool whitelist), then cite it: accepted.
          { kind: "call-sh", command: cmd },
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, cand, cv, withCommand("`" + cmd + "` fails")) },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "incomplete_review_rejected"), 90_000, 50, setup.runDir);
    const rejected = readEvents(setup.runDir).filter((r) => r.kind === "incomplete_review_rejected");
    assert.ok(
      rejected.some((r) => ((r.event as { itemIssues?: string[] }).itemIssues ?? []).some((i) => /not one you ran/.test(i))),
      `the unrun command was refused: ${JSON.stringify(rejected[0]?.event)}`,
    );
  } finally {
    await teardown(setup);
  }
});

test("plan-items: an architecture :WHERE: file missing from the candidate is recorded deviates before review", async () => {
  const missingWhere = {
    architecture: [{ id: "A1", title: "Gone", text: "interface Gone { n: number }", tags: ["data"], where: "src/core/gone.ts" }],
    requirements: [{ id: "R1", title: "R1 proves it", text: "R1 proves it", arch: ["A1"], verify: ['test "R1 proves it"'] }],
    constraints: [],
  };
  const setup = await setupConductor({
    items: missingWhere,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        // The :WHERE: file is never created.
        { kind: "call-sh", command: "mkdir -p src/core && printf 'x\\n' > src/core/other.ts" },
        {
          kind: "call-submit",
          tool: "submit_coverage",
          args: { items: [{ id: "R1", status: "done", where: ["src/core/other.ts:1"], tests: ["R1 proves it"] }], arch: [{ id: "A1", fits: "yes", where: [] }] },
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/core/other.ts" } },
        { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, { items: [{ id: "R1", verdict: "met", evidence: "src/core/other.ts:1" }], arch: [{ id: "A1", verdict: "fits", evidence: "src/core/other.ts:1" }] }) },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => (setup.conductor.state.phase.archSymbolDeviations ?? []).includes("A1"), 90_000, 50, setup.runDir);
    assert.ok(setup.conductor.state.phase.findings.some((f) => f.itemId === "A1" && f.severity === "blocking"));
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: a met anchor on a file changed only in an earlier repair of this phase is accepted", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        // Attempt 1 adds src/core/rounds.ts; attempt 2 adds only other.ts.
        { kind: "call-sh", command: attempt === 1 ? WRITE_ROUNDS : "mkdir -p src/core && printf 'x\\n' > src/core/other.ts" },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const open = state.phase.findings.filter((f) => f.status === "open");
      const firstRound = (state.phase.round ?? 1) === 1;
      const items = {
        items: [
          { id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1" },
          { id: "R2", verdict: firstRound ? "unmet" : "met", evidence: "src/core/rounds.ts:1" },
          { id: "C1", verdict: "met", evidence: "src/core/rounds.ts:1" },
        ],
        arch: [{ id: "A1", verdict: "fits", evidence: "src/core/rounds.ts:1" }],
      };
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          { kind: "call-submit", tool: "submit_review", args: { ...reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, items), findingStatements: open.map((f) => ({ findingId: f.id, status: "confirm", evidence: "the repair fixes it" })) } },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    // The earlier repair's file is part of the phase diff, so its met anchor was accepted.
    assert.ok(!readEvents(setup.runDir).some((r) => r.kind === "incomplete_review_rejected"), "no met verdict was refused for touching an earlier repair's file");
  } finally {
    await teardown(setup);
  }
});

test("plan 06b: item state from the previous candidate never reaches the next", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        // Candidate 1 misses A1's `Round` symbol; candidate 2 has it.
        { kind: "call-sh", command: attempt === 1 ? "mkdir -p src/core && printf 'no symbol here\\n' > src/core/rounds.ts" : WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const open = state.phase.findings.filter((f) => f.status === "open");
      const items = {
        items: [
          { id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1" },
          { id: "R2", verdict: "met", evidence: "src/core/rounds.ts:1" },
          { id: "C1", verdict: "met", evidence: "src/core/rounds.ts:1" },
        ],
        arch: [{ id: "A1", verdict: "fits", evidence: "src/core/rounds.ts:1" }],
      };
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          { kind: "call-submit", tool: "submit_review", args: { ...reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, items), findingStatements: open.map((f) => ({ findingId: f.id, status: "confirm", evidence: "the symbol is present now" })) } },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    // Candidate 1 recorded the A1 deviation; candidate 2 cleared it.
    assert.deepEqual(setup.conductor.state.phase.archSymbolDeviations ?? [], []);
    // The candidate-1 deviation finding is closed (superseded when the new
    // candidate re-evaluated the item), never left open into candidate 2.
    assert.ok(setup.conductor.state.phase.findings.every((f) => f.itemId !== "A1" || f.status !== "open"));
  } finally {
    await teardown(setup);
  }
});

test("plan-items: two of three seats judging A1 deviates block acceptance until the owner accepts the deviation as a trade-off", async () => {
  // A1 has no :WHERE: symbol, so the code cannot contradict the deviating
  // verdicts (the evaluator's own symbol re-verification does not apply).
  const deviatingItems = {
    architecture: [{ id: "A1", title: "Round", text: "the round shape", tags: ["data"] }],
    requirements: ITEMS.requirements,
    constraints: ITEMS.constraints,
  };
  const setup = await setupConductor({
    items: deviatingItems,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    // Each repair changes the file, so every candidate differs from its
    // parent (a met verdict's anchor must touch the diff).
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `mkdir -p src/core && printf '// attempt ${attempt}\ninterface Round { n: number }\n' > src/core/rounds.ts` },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(
      reviewer,
      state.phase.candidate?.sha,
      state.phase.contract.contractVersion,
      review(reviewer === "B" ? {} : { A1: "deviates" }),
    ),
  });
  try {
    await setup.conductor.start();
    // The phase blocks: two deviating seats raise a blocking finding on A1.
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "A1" && f.severity === "blocking"), 90_000, 50, setup.runDir);
    // After the repair budget is spent it parks on the owner with A1 named.
    await waitFor(
      () => setup.conductor.state.phase.phase === "AWAITING_OWNER" && setup.conductor.state.phase.ownerRequests.some((r) => r.status === "open" && /A1/.test(r.reason)),
      120_000,
      50,
      setup.runDir,
    );
    assert.notEqual(setup.conductor.state.phase.phase, "DONE", "the deviation blocked acceptance");
    // The owner accepts the deviation as a trade-off.
    const req = setup.conductor.state.phase.ownerRequests.find((r) => r.status === "open" && /A1/.test(r.reason))!;
    assert.ok(req.options.some((o) => o.id === "accept_risk"), `accept_risk offered: ${req.options.map((o) => o.id).join(", ")}`);
    fs.mkdirSync(`${setup.runDir}/inbox`, { recursive: true });
    // The flat owner-command form the inbox accepts (schemas/owner-command).
    fs.writeFileSync(
      `${setup.runDir}/inbox/owner-1.json`,
      JSON.stringify({
        kind: "resolve",
        requestId: req.id,
        requestVersion: req.version,
        option: "accept_risk",
        note: "the deviation is fine this phase",
        boundCandidateSha: req.boundCandidateSha,
        boundContractVersion: req.boundContractVersion,
      }),
    );
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    assert.ok(setup.conductor.state.phase.findings.some((f) => f.itemId === "A1" && f.status === "accepted"));
  } finally {
    await teardown(setup);
  }
});

test("plan-items: a met verdict citing a line that does not exist is refused; an :WHERE: symbol missing from the candidate is deviates before review", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        // The file exists but does not name the architecture item's `Round` symbol.
        { kind: "call-sh", command: "mkdir -p src/core && printf 'export const something = 1\\n' > src/core/rounds.ts" },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const cv = state.phase.contract.contractVersion;
      const cand = state.phase.candidate?.sha;
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          // A line beyond the file: refused, then the valid review.
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, cand, cv, review({}, "src/core/rounds.ts:9999")) },
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, cand, cv, review({})) },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "incomplete_review_rejected"), 90_000, 50, setup.runDir);
    const rejected = readEvents(setup.runDir).filter((r) => r.kind === "incomplete_review_rejected");
    assert.ok(
      rejected.some((r) => ((r.event as { itemIssues?: string[] }).itemIssues ?? []).some((i) => /do not exist/.test(i))),
      "the nonexistent line was refused",
    );
    // The missing :WHERE: symbol was recorded deviates before the reviewers.
    await waitFor(() => (setup.conductor.state.phase.archSymbolDeviations ?? []).includes("A1"), 60_000, 50, setup.runDir);
    assert.ok(setup.conductor.state.phase.findings.some((f) => f.itemId === "A1" && f.severity === "blocking"));
  } finally {
    await teardown(setup);
  }
});

test("plan 06c: a thin unanimous met verdict is sent to the evaluator as an item check and never withdrawn by code", async () => {
  const promptLog = `/tmp/tt-06c-thin-${process.pid}-${Date.now()}.txt`;
  // Every seat judges every item met/fits with a single anchor (the review()
  // default), so each item is a thin unanimous met/fits.
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    extraEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({})),
    // The evaluator is asked to audit the thin met items; it contradicts R2
    // with valid anchors, which overturns the met verdict.
    evaluatorScriptFor: () => ({
      hello: { role: "evaluator", tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations: [],
            itemChecks: [
              { id: "R2", verdict: "contradicted", evidence: "src/core/rounds.ts:1 does not implement R2" },
              { id: "R1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
              { id: "C1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
              { id: "A1", verdict: "confirmed", evidence: "src/core/rounds.ts:1 re-checked" },
            ],
          },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    // The audit is listed in the evaluator's prompt.
    await waitFor(() => fs.existsSync(promptLog) && fs.readFileSync(promptLog, "utf8").includes("Plan-item re-check"), 90_000, 50, setup.runDir);
    const prompts = fs.readFileSync(promptLog, "utf8");
    assert.match(prompts, /Plan-item re-check/);
    assert.match(prompts, /- R2 .*thin unanimous evidence/, "the thin met R2 is an owed item check");
    // The evaluator's contradiction overturns the thin met verdict (it is the
    // evaluator's check, never the code, that withdrew it).
    await waitFor(() => (setup.conductor.state.phase.overturns ?? []).some((o) => o.id === "R2"), 90_000, 50, setup.runDir);
    const overturn = (setup.conductor.state.phase.overturns ?? []).find((o) => o.id === "R2")!;
    assert.equal(overturn.effect, "flip");
    assert.match(overturn.reason, /thin met verdict/);
    assert.ok((setup.conductor.state.phase.itemChecks ?? []).some((c) => c.itemId === "R2" && c.verdict === "contradicted"));
  } finally {
    await teardown(setup);
    fs.rmSync(promptLog, { force: true });
  }

  // Control: with no contradiction the thin met verdict is never withdrawn by
  // code — the phase reaches DONE with R2 still met.
  const control = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE_ROUNDS },
        { kind: "call-submit", tool: "submit_coverage", args: coverage() },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, review({})),
  });
  try {
    await control.conductor.start();
    await waitFor(() => control.conductor.state.phase.phase === "DONE", 120_000, 50, control.runDir);
    assert.deepEqual(control.conductor.state.phase.overturns ?? [], [], "code never withdraws a thin met verdict");
  } finally {
    await teardown(control);
  }
});

test("plan 06c: an amendment whose text equals the current criterion is never raised", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\n' > sum.js" },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [],
            assumptions: [],
            deviations: [],
            // The proposed wording IS the current criterion: a no-op.
            criterionDispute: { criterion: "it works", why: "no reason to change it", proposedWording: "it works" },
          },
        },
      ],
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
            findings: [],
          },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    // No amendment record, no message, no ballot for the no-op proposal.
    assert.ok(!setup.conductor.state.phase.decisions.some((d) => d.amendment), "no amendment decision was raised");
    assert.ok(!(setup.conductor.state.phase.messages ?? []).some((m) => (m.sourceRecordId ?? "").includes("amendment")), "no message was raised");
    assert.ok(!readEvents(setup.runDir).some((r) => r.kind === "ballot_demanded" && JSON.stringify(r.event).includes("amendment")), "no ballot was demanded for it");
    assert.ok(
      readEvents(setup.runDir).some((r) => r.kind === "dispute_ignored" && /equals the current criterion/.test(String((r.event as { reason?: string }).reason))),
      "the no-op amendment is logged as ignored",
    );
  } finally {
    await teardown(setup);
  }
});

// ---------------------------------------------------------------------------
// Plan 06g (A4/A5): the round budget's convergence rule and the owner's carry.
// These run only when a plan asks for a round budget; a plan without
// `#+TT_WORKERS` behaves exactly as today (C1).
// ---------------------------------------------------------------------------

/** Every event type this plan adds. None may appear in a run without
 * `#+TT_WORKERS`. */
const ROUND_EVENT_TYPES = new Set(["ROUND_STARTED", "CANDIDATE_SUBMITTED", "CANDIDATE_CHECKED", "PICK_VOTE", "CANDIDATE_PICKED"]);

function submitPhaseStep(): FakePiStep {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

/** M raises one blocking finding per CANDIDATE (so a repair round raises a
 * new one), and withdraws its previous open finding once a new candidate is
 * under review — the ordinary reviewer flow the record requires before a
 * reopened point may stop blocking. A and B approve. */
function reviewerRaisingOncePerRound(evidence: string, kind: "defect" | "contract" = "defect") {
  return (reviewer: Reviewer, state: State) => {
    const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === reviewer);
    const alreadyOnThisCandidate = open.some((f) => f.boundCandidateSha === state.phase.candidate?.sha);
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
            findingStatements: open
              .filter((f) => f.boundCandidateSha !== state.phase.candidate?.sha)
              .map((f) => ({ findingId: f.id, status: "withdraw", evidence: "withdrawn: the new candidate no longer carries it" })),
            ballots: [],
            findings: reviewer === "M" && !alreadyOnThisCandidate ? [{ kind, severity: "blocking", evidence }] : [],
          },
        },
      ],
    };
  };
}

/** A run that reaches DONE after a round-1 repair, with the given finding
 * evidence raised in each round. */
async function runTwoRounds(
  evidence: string,
  phase: Partial<import("../../src/conductor.ts").RunPlanPhase> = {},
  kind: "defect" | "contract" = "defect",
): Promise<TestConductorSetup> {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: reviewerRaisingOncePerRound(evidence, kind),
    deadlines: { ...FAST, inboxPollMs: 40 },
    ...(Object.keys(phase).length > 0
      ? {
          phase: {
            id: "p1",
            goal: "keep the loop short",
            acceptance: ["it works"],
            checks: ["true"],
            boundaries: [],
            reserved: [],
            provisional: false,
            ...phase,
          },
        }
      : {}),
  });
  await setup.conductor.start();
  return setup;
}

test("plan 06g: with TT_WORKERS absent no lane, pick or round event is recorded and old event logs replay unchanged", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
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
          },
        },
      ],
    }),
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
  });

  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000);
    const state = setup.conductor.state;

    // The plan declares no lanes, so the phase carries no round at all.
    assert.equal(state.phase.rounds, undefined);
    assert.equal(state.phase.contract.workers, undefined);
    assert.equal(state.phase.contract.roundsAllowed, undefined);
    // A4: `roundBudget` defaults to 3 ROUNDS — one candidate reviewed each,
    // so the repair allowance is roundBudget - 1 (the first candidate plus two
    // repairs).
    assert.equal(state.phase.repairRoundsGranted, 2);

    // No round event of any kind reached the log.
    const logEvents = readEvents(setup.runDir);
    const offenders = logEvents.filter((r) => r.kind === "event" && ROUND_EVENT_TYPES.has((r.event as { type?: string })?.type ?? ""));
    assert.deepEqual(offenders, [], "a run without TT_WORKERS must record no round event");
    const reviewSubmitted = logEvents.filter((r) => r.kind === "event" && (r.event as { type: string }).type === "REVIEW_SUBMITTED");
    assert.ok(reviewSubmitted.length >= 3, "the three reviews still run as before");
    for (const record of reviewSubmitted) {
      assert.equal((record.event as { candidate?: string }).candidate, undefined, "a single-lane review carries no candidate field");
    }

    // An old log replays unchanged: folding events.jsonl from scratch gives
    // the same state the live conductor holds, with no round record.
    const rebuilt = rebuildState(setup.runDir, setup.plan);
    assert.deepEqual(rebuilt.phase.rounds, undefined);
    assert.equal(rebuilt.phase.phase, state.phase.phase);
    assert.equal(rebuilt.phase.candidate?.sha, state.phase.candidate?.sha);
    assert.equal(rebuilt.phase.publishedI, state.phase.publishedI);
    const before = readLog(runPaths(setup.runDir).events).records.length;
    rebuildState(setup.runDir, setup.plan);
    assert.equal(readLog(runPaths(setup.runDir).events).records.length, before);
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: a round-2 blocking finding that names no contract item and no regression is carried as an advisory and the phase is accepted", async () => {
  const setup = await runTwoRounds("a further edge path in the sweep, not named by any item");
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.round, 2);
    const advisory = phase.findings.find((f) => f.severity === "advisory" && f.boundCandidateSha === phase.candidate?.sha)!;
    assert.ok(advisory, "the round-2 finding is carried as an advisory");
    assert.equal(advisory.status, "open");
    assert.match(advisory.severityReason ?? "", /A6 round 2/);
    assert.equal(phase.phase, "DONE");
    assert.equal(phase.repairRoundsUsed, 1, "the round rule saved the extra repair round");
    const log = readLog(runPaths(setup.runDir).events).records.filter((r) => r.kind === "round_advisory");
    assert.ok(log.length >= 1, "the downgrade is recorded with its reason");
    assert.match(String((log[0].event as { reason?: string }).reason ?? ""), /A6 round 2/);
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: a phase still blocked after three rounds stops AWAITING_OWNER and starts no fourth round until the owner grants one", async () => {
  // The checks never pass, so the phase can only repair: the default budget
  // (3 rounds = the first candidate plus two repairs) is spent after round 3,
  // the phase parks AWAITING_OWNER, and no fourth round starts by itself. The
  // owner lifts the park for exactly ONE more round through a real inbox file.
  const setup = await setupConductor({
    checks: ["false"],
    workerScriptForAttempt: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    deadlines: { ...FAST, inboxPollMs: 40 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const parked = setup.conductor.state.phase;
    assert.equal(parked.repairRoundsUsed, 2, "two repairs (three rounds) were spent");
    assert.equal(parked.repairRoundsGranted, 2, "the default budget of 3 rounds is spent");
    assert.equal(parked.attempt.n, 3, "three rounds ran, and no fourth worker attempt was launched");
    const request = parked.ownerRequests.find((r) => r.status === "open")!;
    assert.ok(request, "the owner is asked to lift the park");
    assert.ok(request.options.some((o) => o.id === "grant"), "the owner may grant one more round");

    // The owner grants exactly one more round through a real inbox file.
    const runId = parked.runId;
    const file = path.join(runPaths(setup.runDir).inbox, "cmd-resolve-grant.json");
    fs.writeFileSync(
      file,
      JSON.stringify({
        commandId: "cmd-resolve-grant",
        type: "resolve",
        recordKind: "request",
        option: "grant",
        binding: {
          runId,
          phaseId: parked.phaseId,
          candidateSha: parked.candidate!.sha,
          contractVersion: parked.contract.contractVersion,
          recordId: request.id,
          recordVersion: request.version,
        },
      }),
    );
    await waitFor(() => setup.conductor.state.phase.phase !== "AWAITING_OWNER", 30_000);
    await waitFor(() => setup.conductor.state.phase.attempt.n >= 4, 90_000, 20, setup.runDir);
    assert.equal(setup.conductor.state.phase.repairRoundsGranted, 3, "the grant added exactly one round");
    const resolved = readEvents(setup.runDir)
      .filter((r) => r.kind === "event")
      .map((r) => r.event as { type: string; option?: string })
      .filter((e) => e.type === "OWNER_REQUEST_RESOLVED");
    assert.equal(resolved.length, 1);
    assert.equal(resolved[0].option, "grant");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: tt carry accepts once no blocking item remains uncarried and lists each carried item with its target", async () => {
  // A structured phase whose R2 is an `evidence` item, RECORDED up front so
  // the carry never waives a mechanical gate (ODP-3). Rounds 1 and 2 raise a
  // GROUNDED blocking finding (withdrawn the next round); round 3 raises one
  // grounded blocking finding and one advisory. With the default budget of 3
  // rounds the phase parks on the owner; the owner carries each open review
  // item and the candidate is accepted, with both items reaching `tt summary`.
  const items = {
    architecture: [],
    requirements: [
      { id: "R1", title: "R1 proves it", text: "R1 proves it", arch: [], verify: ['test "R1 proves it"'] },
      { id: "R2", title: "R2 owner run", text: "the owner live run is recorded", arch: [], verify: ["evidence"] },
    ],
    constraints: [],
  };
  const setup = await setupConductor({
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "keep the loop short",
      acceptance: items.requirements.map((r) => r.text),
      checks: [CHECK_OUTPUT],
      boundaries: [],
      reserved: [],
      provisional: false,
      ...items,
    },
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `mkdir -p src/core && printf 'interface Round { n: number } // ${attempt}\\n' > src/core/rounds.ts` },
        {
          kind: "call-submit",
          tool: "submit_coverage",
          args: {
            items: [
              { id: "R1", status: "done", where: ["src/core/rounds.ts:1"], tests: ["R1 proves it"] },
              { id: "R2", status: "done", where: [], tests: [] },
            ],
            arch: [],
          },
        },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === reviewer);
      const findings =
        reviewer === "M"
          ? round <= 2
            ? [{ kind: "defect", severity: "blocking", evidence: "R1 (R1 proves it) is unmet: src/core/rounds.ts:1 still drops the second lane" }]
            : [
                { kind: "defect", severity: "blocking", evidence: "R1 (R1 proves it) is unmet: src/core/rounds.ts:1 still drops the second lane" },
                { kind: "defect", severity: "advisory", evidence: "a further edge path worth carrying into the next phase's plan" },
              ]
          : [];
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: open
                .filter((f) => f.boundCandidateSha !== state.phase.candidate?.sha)
                .map((f) => ({ findingId: f.id, status: "withdraw", evidence: "withdrawn: the new candidate no longer carries it" })),
              ballots: [],
              items: [
                { id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1" },
                { id: "R2", verdict: "met", evidence: "src/core/rounds.ts:1" },
              ],
              arch: [],
              findings,
            },
          },
        ],
      };
    },
    deadlines: { ...FAST, inboxPollMs: 40 },
  });
  await setup.conductor.start();
  try {
    // Record the evidence item up front, so the carry never has to waive a
    // mechanical gate (ODP-3): it accepts only once the gates hold and the
    // remaining open items are review items.
    await runCli(["evidence", setup.runDir, "R2", "the live run is in NOTES.md", "--root", setup.runRoot]);
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const parked = setup.conductor.state.phase;
    assert.equal(parked.repairRoundsUsed, 2, "two repairs (three rounds) were spent");
    assert.equal(parked.repairRoundsGranted, 2, "and the budget is spent, so no fourth round may start");
    assert.equal(parked.attempt.n, 3, "three rounds ran, and no fourth worker attempt was launched");
    assert.deepEqual(parked.itemEvidence?.map((e) => e.id), ["R2"], "the evidence gate is satisfied, not waived");
    // Round 3 leaves a grounded blocking finding and one advisory open.
    const blocking = parked.findings.find((f) => f.status === "open" && f.severity === "blocking")!;
    const advisory = parked.findings.find((f) => f.status === "open" && f.severity === "advisory")!;
    assert.ok(blocking && advisory, "the round-3 blocking finding and advisory are open");
    const request = parked.ownerRequests.find((r) => r.status === "open" && r.linkedFindingId === blocking.id)!;
    assert.ok(request, "the owner is asked about the blocking finding");

    // The owner carries each open review item through the real CLI. Carrying
    // the advisory alone does not accept (the blocking finding is uncarried);
    // carrying the blocking finding then accepts the candidate.
    const advisoryCarry = await runCli(["carry", setup.runDir, advisory.id, "--to", "06h", "--root", setup.runRoot]);
    assert.match(advisoryCarry.stdout, new RegExp(`carried ${advisory.id} to 06h`));
    assert.notEqual(setup.conductor.state.phase.phase, "DONE", "one uncarried blocking item still holds acceptance");
    const blockingCarry = await runCli(["carry", setup.runDir, blocking.id, "--to", "06h", "--root", setup.runRoot]);
    assert.match(blockingCarry.stdout, new RegExp(`carried ${blocking.id} to 06h`));
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const done = setup.conductor.state.phase;
    assert.equal(done.acceptedWithCarried, true);
    assert.ok(done.carriedItems?.includes(advisory.id), "the advisory is recorded");
    assert.ok(done.carriedItems?.includes(blocking.id), "the blocking item is recorded");
    assert.equal(done.carriedTo?.[advisory.id], "06h", "with the target phase the owner named");
    assert.equal(done.carriedTo?.[blocking.id], "06h", "with the target phase the owner named");
    const summary = prSummary(setup.runDir, setup.plan);
    assert.match(summary, /### Carried items \(2\)/);
    assert.match(summary, new RegExp(`${advisory.id}.* → 06h`));
    assert.match(summary, new RegExp(`${blocking.id}.* → 06h`));
    // The status view lists them too, with the target (A5).
    const status = fs.readFileSync(path.join(runPaths(setup.runDir).status), "utf8");
    assert.match(status, /Carried items \(2\)/);
    assert.match(status, new RegExp(`${advisory.id} → 06h`));
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: tt carry never waives a missing named test verify or unrecorded evidence", async () => {
  // ODP-3: the carry replaces only the review-verdict tally. A missing named
  // test verify or an unrecorded `evidence` item is a mechanical gate and must
  // keep holding, so the phase stays parked after the carry.
  const items = {
    architecture: [],
    requirements: [
      { id: "R1", title: "R1 proves it", text: "R1 proves it", arch: [], verify: ['test "R1 proves it"'] },
      { id: "R2", title: "R2 owner run", text: "the owner live run is recorded", arch: [], verify: ["evidence"] },
    ],
    constraints: [],
  };
  const setup = await setupConductor({
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "keep the loop short",
      acceptance: items.requirements.map((r) => r.text),
      checks: [CHECK_OUTPUT],
      boundaries: [],
      reserved: [],
      provisional: false,
      ...items,
    },
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `mkdir -p src/core && printf 'interface Round { n: number } // ${attempt}\\n' > src/core/rounds.ts` },
        {
          kind: "call-submit",
          tool: "submit_coverage",
          args: {
            items: [
              { id: "R1", status: "done", where: ["src/core/rounds.ts:1"], tests: ["R1 proves it"] },
              { id: "R2", status: "done", where: [], tests: [] },
            ],
            arch: [],
          },
        },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const open = (state.phase.findings ?? []).filter((f) => f.status === "open" && f.raisedBy === reviewer);
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: open
                .filter((f) => f.boundCandidateSha !== state.phase.candidate?.sha)
                .map((f) => ({ findingId: f.id, status: "withdraw", evidence: "withdrawn: the new candidate no longer carries it" })),
              ballots: [],
              items: [
                { id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1" },
                { id: "R2", verdict: "met", evidence: "src/core/rounds.ts:1" },
              ],
              arch: [],
              findings:
                reviewer === "M"
                  ? round <= 2
                    ? [{ kind: "defect", severity: "blocking", evidence: "R1 (R1 proves it) is unmet: src/core/rounds.ts:1 still drops the second lane" }]
                    : [{ kind: "defect", severity: "advisory", evidence: "a further edge path worth carrying into the next phase's plan" }]
                  : [],
            },
          },
        ],
      };
    },
    deadlines: { ...FAST, inboxPollMs: 40 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const parked = setup.conductor.state.phase;
    // R2 is unrecorded, so the park is the evidence gate; only the advisory is
    // open, and the owner is offered accept-with-carried.
    assert.deepEqual(parked.itemEvidence ?? [], [], "R2 is not recorded");
    const advisory = parked.findings.find((f) => f.status === "open" && f.severity === "advisory")!;
    assert.ok(advisory, "the advisory is open");
    assert.ok(!parked.findings.some((f) => f.status === "open" && f.severity === "blocking"), "no blocking finding is open");
    const request = parked.ownerRequests.find((r) => r.status === "open")!;
    assert.deepEqual(request.options.map((o) => o.id), ["accept_carried"], "the owner's single decision is accept with carried items");

    // The carry is recorded, but it must NOT accept: the evidence gate holds.
    const carry = await runCli(["carry", setup.runDir, advisory.id, "--to", "06h", "--root", setup.runRoot]);
    assert.match(carry.stdout, new RegExp(`carried ${advisory.id} to 06h`));
    assert.notEqual(setup.conductor.state.phase.phase, "DONE", "the carry never waives an unrecorded evidence item");
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 30_000, 20, setup.runDir);
    assert.deepEqual(setup.conductor.state.phase.itemEvidence ?? [], [], "the evidence is still unrecorded");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: a round-2 finding that names an unmet item (id and file:line) still blocks, so the phase repairs instead of accepting", async () => {
  // The negative half of the convergence rule (R4): a round-2 blocking finding
  // that DOES name an unmet item, with its id and a file:line, is not
  // downgraded — it keeps blocking acceptance.
  const setup = await runTwoRounds("R1 (it works) is unmet: src/core/rounds.ts:12 still drops the second lane");
  try {
    await waitFor(() => setup.conductor.state.phase.repairRoundsUsed >= 2, 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.notEqual(phase.phase, "DONE", "a grounded item finding must not be accepted away");
    const grounded = phase.findings.find((f) => f.severity === "blocking" && f.status === "open");
    assert.ok(grounded, "the grounded finding stays blocking and open");
    assert.match(grounded!.evidence, /src\/core\/rounds\.ts:12/);
    assert.equal(grounded!.severityReason, undefined, "a grounded finding is not downgraded");
  } finally {
    await teardown(setup);
  }
});

test("plan 06g: a test that passed at round 1's candidate and fails in round 2 is a regression and blocks", async () => {
  // R4's regression clause at the conductor level, with the round's own base
  // (not the phase base): test T passes at the phase base and at round 1's
  // candidate, and fails at round 2's. Comparing against the phase base would
  // (wrongly) be the same here; the point is that the round-2 failure is judged
  // against round 1's candidate, which passed it, so it is a regression.
  const marker = fs.mkdtempSync("/tmp/tt-regression-");
  const failMarker = path.join(marker, "fail");
  const check = `sh -c 'if [ -f ${failMarker} ]; then printf "not ok 1 - T\\n"; exit 1; else printf "ok 1 - T\\n"; exit 0; fi'`;
  const setup = await setupConductor({
    checks: [check],
    stubReviews: false,
    phase: {
      id: "p1",
      goal: "keep the loop short",
      acceptance: ["it works"],
      checks: [check],
      boundaries: [],
      reserved: [],
      provisional: false,
      rounds: 2,
    },
    // Round 2 arms the check's failure; round 1 does not.
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        ...(attempt >= 2 ? [{ kind: "call-sh", command: `touch ${failMarker}` }] : []),
        { kind: "call-sh", command: `printf 'round${attempt}' > candidate.txt` },
        submitPhaseStep(),
      ],
    }),
    reviewerScriptFor: reviewerRaisingOncePerRound("a blocking finding on round 1's candidate"),
    deadlines: { ...FAST, inboxPollMs: 40 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(phase.round, 2, "round 2's candidate is the one whose check failed");
    assert.notEqual(phase.phase, "DONE", "a regression blocks acceptance");
    const regressions = readLog(runPaths(setup.runDir).events).records.filter((r) => r.kind === "round_regression");
    assert.ok(regressions.length >= 1, "the regression against the round's base is recorded");
    assert.ok((regressions[0].event as { tests?: string[] }).tests?.includes("T"), "the test that passed at round 1 and fails now is named");
  } finally {
    await teardown(setup);
    cleanupDir(marker);
  }
});

test("plan 06g: tt carry on a contract finding at the end of the budget accepts the candidate and lists the item as carried to the named phase", async () => {
  // The 06c case (2026-10-07): one contract finding on a display detail, with
  // the budget spent. A contract finding cannot be answered by a correction
  // without granting three rounds and restarting the worker; `tt carry` is the
  // owner's disposition that accepts the candidate and carries the item.
  const setup = await runTwoRounds(
    "R1 (it works) is unmet: src/core/rounds.ts:12 the stage clock is wrong on the display",
    { rounds: 1 },
    "contract",
  );
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 120_000, 20, setup.runDir);
    const parked = setup.conductor.state.phase;
    // `#+TT_ROUNDS: 1` = one round (the first candidate), so round 1's finding
    // exhausts the budget and parks the phase.
    assert.equal(parked.repairRoundsUsed, 0, "the single round's candidate was the only one reviewed");
    assert.equal(parked.repairRoundsGranted, 0, "the budget of one round is spent");
    const contract = parked.findings.find((f) => f.status === "open" && f.kind === "contract")!;
    assert.ok(contract, "the open contract finding is what the owner must answer");
    // A contract finding's only offered option is repair, so the owner needs
    // the carry disposition to accept without another round.
    const request = parked.ownerRequests.find((r) => r.status === "open" && r.linkedFindingId === contract.id)!;
    assert.deepEqual(request.options.map((o) => o.id), ["repair"]);

    // The real CLI: `tt carry <run> <id> --to <phase-id>`.
    const { stdout: carryOut } = await runCli(["carry", setup.runDir, contract.id, "--to", "06h", "--root", setup.runRoot]);
    assert.match(carryOut, new RegExp(`carried ${contract.id} to 06h`));
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const done = setup.conductor.state.phase;
    assert.equal(done.acceptedWithCarried, true, "the owner's carry accepts the candidate");
    assert.ok(done.carriedItems?.includes(contract.id));
    assert.equal(done.carriedTo?.[contract.id], "06h", "the item is carried to the named phase");
    const summary = prSummary(setup.runDir, setup.plan);
    assert.match(summary, /### Carried items \(1\)/);
    assert.match(summary, new RegExp(`${contract.id}.* → 06h`));
    assert.match(summary, /the stage clock is wrong on the display/);
  } finally {
    await teardown(setup);
  }
});