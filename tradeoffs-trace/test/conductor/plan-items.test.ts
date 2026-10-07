// Plan 06b: every point of a structured plan is a parsed, identified item the
// conductor carries mechanically — the worker's coverage, the check
// resolution, the reviewers' per-item verdicts and the acceptance decision
// (refs/06_ref_plan_format.md). Fake-pi end to end.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor, type FakePiStep } from "./harness.ts";
import type { Reviewer } from "../../src/core/types.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";

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