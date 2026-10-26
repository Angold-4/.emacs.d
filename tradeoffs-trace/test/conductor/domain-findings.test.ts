// Plan 07a (design D1/D2/D6): domains and one-point findings, end to end with
// fake pi. A plan's `#+TT_DOMAINS` and each item's `:DOMAIN:` reach the plan
// JSON and the frozen contract; a domain-mode finding is one point (domain,
// item, where, expected, doneWhen) that is refused when incomplete, folded
// when a second seat names the same point, and that the conductor's own
// item findings carry from the item.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor, type FakePiStep } from "./harness.ts";
import { validate } from "../../src/core/schema.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import type { Domain } from "../../src/core/domains.ts";

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

const DOMAINS: Domain[] = [
  { name: "booking", globs: ["src/**"] },
  { name: "gates", globs: ["test/**"] },
];

const ITEMS = {
  architecture: [{ id: "A1", title: "Round", text: "interface Round { n: number }", tags: ["data"], where: "src/core/rounds.ts", domain: "booking" }],
  requirements: [
    { id: "R1", title: "R1 proves it", text: "R1 proves it", arch: ["A1"], verify: ['test "R1 proves it"'], domain: "booking" },
    { id: "R2", title: "R2 reviewed", text: "R2 is judged by review", arch: [], verify: ["review"], domain: "gates" },
  ],
  constraints: [{ id: "C1", title: "C1 holds", text: "C1 must not change", verify: ["review"], domain: "booking" }],
};

const CHECK_OUTPUT = "printf 'ok 1 - R1 proves it\\n'";
const WRITE_ROUNDS = "mkdir -p src/core && printf 'interface Round { n: number }\\n' > src/core/rounds.ts";

function coverage() {
  return {
    items: [
      { id: "R1", status: "done", where: ["src/core/rounds.ts:1"], tests: ["R1 proves it"] },
      { id: "R2", status: "done", where: [], tests: [] },
      { id: "C1", status: "done", where: [], tests: [] },
    ],
    arch: [{ id: "A1", fits: "yes", where: ["src/core/rounds.ts"] }],
  };
}

function itemVerdicts(r2: string = "met") {
  return {
    items: [
      { id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1" },
      { id: "R2", verdict: r2, evidence: "src/core/rounds.ts:1" },
      { id: "C1", verdict: "met", evidence: "src/core/rounds.ts:1" },
    ],
    arch: [{ id: "A1", verdict: "fits", evidence: "src/core/rounds.ts:1" }],
  };
}

function reviewArgs(reviewer: Reviewer, candidateSha: string | undefined, contractVersion: unknown, extra: Record<string, unknown> = {}) {
  return {
    reviewer,
    phaseId: "p1",
    candidateSha,
    contractVersion,
    correctionStatements: [],
    findingStatements: [],
    ...itemVerdicts((extra.r2 as string) ?? "met"),
    ...extra,
  };
}

/** A reviewer that reads the candidate file, then submits one review with
 * `extra` (findings/blockers). */
function reviewerScript(reviewer: Reviewer, state: State, extra: Record<string, unknown> = {}): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
      { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, extra) },
    ],
  };
}

function workerScript() {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: WRITE_ROUNDS },
      { kind: "call-submit", tool: "submit_coverage", args: coverage() },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

async function teardown(setup: Awaited<ReturnType<typeof setupConductor>>): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

test("a plan's TT_DOMAINS and item DOMAIN reach the plan JSON through the Emacs parser", () => {
  const dir = fs.mkdtempSync("/tmp/tt-07a-org-");
  const orgPath = path.join(dir, "PLAN.org");
  fs.writeFileSync(
    orgPath,
    [
      "#+TITLE: domains",
      "#+TT_REPO: /tmp/tt-07a-repo",
      "#+TT_BRANCH: main",
      "#+TT_CHECKS: true",
      "#+TT_DOMAINS: booking=src/b/*.ts gates=test/**",
      "",
      "* P",
      "  :PROPERTIES:",
      "  :ID: p1",
      "  :CHECKS: true",
      "  :END:",
      "** Goal",
      "   g",
      "** Architecture",
      "*** A1 shape",
      "    :PROPERTIES:",
      "    :ID: A1",
      "    :DOMAIN: booking",
      "    :END:",
      "    text",
      "** Requirements",
      "*** R1 thing",
      "    :PROPERTIES:",
      "    :ID: R1",
      "    :VERIFY: review",
      "    :DOMAIN: gates",
      "    :END:",
      "    text",
    ].join("\n") + "\n",
  );
  try {
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
    const parsed = JSON.parse(out) as { phases: Array<{ domains?: Domain[]; architecture?: Array<{ id: string; domain?: string }>; requirements?: Array<{ id: string; domain?: string }> }> };
    const phase = parsed.phases[0];
    assert.deepEqual(
      phase.domains,
      [
        { name: "booking", globs: ["src/b/*.ts"] },
        { name: "gates", globs: ["test/**"] },
      ],
      "names and globs in declaration order",
    );
    assert.equal(phase.architecture![0].domain, "booking");
    assert.equal(phase.requirements![0].domain, "gates");
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("a plan's domains reach the phase contract", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    domains: DOMAINS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: true,
    deadlines: FAST,
    workerScript,
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [{ kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion) }],
    }),
  });
  try {
    await setup.conductor.start();
    const contract = setup.conductor.state.phase.contract;
    assert.deepEqual(contract.domains, DOMAINS, "the contract's domains equal the plan's");
    assert.equal(contract.architecture![0].domain, "booking");
    assert.equal(contract.requirements!.find((r) => r.id === "R2")!.domain, "gates");
    const schema = JSON.parse(fs.readFileSync(new URL("../../schemas/phase-contract.schema.json", import.meta.url), "utf8"));
    const result = validate(schema, contract);
    assert.equal(result.valid, true, result.errors.join("; "));
  } finally {
    await teardown(setup);
  }
});

test("a reviewer whose domain-mode finding lacks doneWhen is told the field and resubmits", async () => {
  const point = { domain: "booking", item: "R1", where: "src/core/rounds.ts:1 fn", expected: "the point is fixed" };
  const setup = await setupConductor({
    items: ITEMS,
    domains: DOMAINS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript,
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
        // First: the point has no doneWhen, so the submit is refused.
        { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, { findings: [{ kind: "defect", severity: "advisory", evidence: "the point", ...point }] }) },
        // Second: with doneWhen, it is accepted.
        { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state.phase.candidate?.sha, state.phase.contract.contractVersion, { findings: [{ kind: "defect", severity: "advisory", evidence: "the point", ...point, doneWhen: "true" }] }) },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.item === "R1" && f.doneWhen === "true"), 90_000, 50, setup.runDir);
    const rejected = readEvents(setup.runDir).filter((r) => r.kind === "review_ingestion_rejected");
    assert.ok(rejected.length >= 1, "the incomplete point was refused");
    assert.ok(
      rejected.some((r) => String((r.event as { reason?: string }).reason ?? "").includes("doneWhen")),
      `the refusal names doneWhen: ${JSON.stringify(rejected.map((r) => r.event))}`,
    );
    const finding = setup.conductor.state.phase.findings.find((f) => f.item === "R1" && f.doneWhen === "true")!;
    assert.equal(finding.domain, "booking");
    assert.equal(finding.where, "src/core/rounds.ts:1 fn");
    assert.equal(finding.expected, "the point is fixed");
  } finally {
    await teardown(setup);
  }
});

test("two seats' findings with the same item, where and doneWhen become one finding raised by both", async () => {
  const point = { domain: "booking", item: "R1", where: "src/core/rounds.ts:1 fn", expected: "x", doneWhen: "true" };
  const setup = await setupConductor({
    items: ITEMS,
    domains: DOMAINS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript,
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state, { findings: [{ kind: "defect", severity: "advisory", evidence: "same point", ...point }] }),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.item === "R1" && (f.alsoRaisedBy ?? []).length >= 2),
      90_000,
      50,
      setup.runDir,
    );
    const matching = setup.conductor.state.phase.findings.filter((f) => f.item === "R1" && f.doneWhen === "true");
    assert.equal(matching.length, 1, "one open finding remains");
    assert.equal(matching[0].alsoRaisedBy!.length, 2, "the finding lists both other seats");
    assert.ok(readEvents(setup.runDir).some((r) => r.kind === "event" && (r.event as { type?: string }).type === "FINDING_ALSO_RAISED"));
  } finally {
    await teardown(setup);
  }
});

test("two findings with the same item and where but different doneWhen stay two findings", async () => {
  const base = { domain: "booking", item: "R1", where: "src/core/rounds.ts:1 fn", expected: "x" };
  const setup = await setupConductor({
    items: ITEMS,
    domains: DOMAINS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript,
    reviewerScriptFor: (reviewer, state) =>
      reviewerScript(reviewer, state, {
        findings: reviewer === "B" ? [] : [{ kind: "defect", severity: "advisory", evidence: "the point", ...base, doneWhen: reviewer === "M" ? "true" : "false" }],
      }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.filter((f) => f.item === "R1").length >= 2, 90_000, 50, setup.runDir);
    const matching = setup.conductor.state.phase.findings.filter((f) => f.item === "R1");
    assert.equal(matching.length, 2, "a different doneWhen stays a different finding");
    assert.deepEqual([...new Set(matching.map((f) => f.doneWhen))].sort(), ["false", "true"]);
    assert.ok(matching.every((f) => (f.alsoRaisedBy ?? []).length === 0), "neither folded into the other");
  } finally {
    await teardown(setup);
  }
});

test("a finding the conductor raises on an unmet item carries the item's domain", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    domains: DOMAINS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript,
    // R2 (review-only, domain gates) is marked unmet by two of three seats.
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state, { r2: reviewer === "B" ? "met" : "unmet" }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "R2" && f.severity === "blocking"), 90_000, 50, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.itemId === "R2")!;
    assert.equal(finding.raisedBy, "conductor");
    assert.equal(finding.domain, "gates");
    assert.equal(finding.doneWhen, undefined, "a conductor finding carries no doneWhen");
  } finally {
    await teardown(setup);
  }
});

test("a point with trailing whitespace still folds into the open one", async () => {
  const base = { domain: "booking", item: "R1", where: "src/core/rounds.ts:1 fn", expected: "x" };
  const setup = await setupConductor({
    items: ITEMS,
    domains: DOMAINS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript,
    reviewerScriptFor: (reviewer, state) =>
      reviewerScript(reviewer, state, {
        // One seat pads item/doneWhen; the stored values are trimmed, so the
        // fold must compare trimmed values too.
        findings: [{ kind: "defect", severity: "advisory", evidence: "padded point", ...base, item: reviewer === "M" ? "R1" : "R1 ", doneWhen: reviewer === "M" ? "true" : "true " }],
      }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.item === "R1" && (f.alsoRaisedBy ?? []).length >= 2), 90_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.findings.filter((f) => f.item === "R1").length, 1, "a padded point is the same point");
  } finally {
    await teardown(setup);
  }
});

test("a duplicate carrying a criterion dispute is not folded and its dispute survives", async () => {
  const point = { domain: "booking", item: "R1", where: "src/core/rounds.ts:1 fn", expected: "x", doneWhen: "true" };
  // Three distinct disputes on the SAME point. M raises R1 (wording p1) and
  // R2; A repeats M's R1 dispute verbatim (it is already on the open finding,
  // so folding loses nothing); B disputes the SAME criterion R1 but proposes
  // different wording — a different amendment that must not be folded away
  // (OD-6 / D-M-50, D-M-51; OD-7 / D-M-68).
  const dispute = (criterion: string, proposedWording: string) => ({ criterion, why: "the wording is wrong", proposedWording });
  const pointFinding = (criterion: string, proposedWording: string) => ({
    kind: "defect",
    severity: "advisory",
    evidence: "the point",
    ...point,
    criterionDispute: dispute(criterion, proposedWording),
  });
  const setup = await setupConductor({
    items: ITEMS,
    domains: DOMAINS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript,
    reviewerScriptFor: (reviewer, state) =>
      reviewerScript(reviewer, state, {
        findings:
          reviewer === "M"
            ? [pointFinding("R1 proves it", "R1 proves it, fixed"), pointFinding("R2 is judged by review", "R2 is judged by review, fixed")]
            : reviewer === "A"
              ? [pointFinding("R1 proves it", "R1 proves it, fixed")]
              : [pointFinding("R1 proves it", "R1 proves it, reworded")],
      }),
  });
  try {
    await setup.conductor.start();
    const matching = () => setup.conductor.state.phase.findings.filter((f) => f.item === "R1");
    await waitFor(
      () => matching().length === 3 && matching().some((f) => (f.alsoRaisedBy ?? []).length === 1),
      90_000,
      50,
      setup.runDir,
    );
    assert.equal(matching().length, 3, "each distinct dispute stays its own finding");
    assert.deepEqual(
      matching()
        .map((f) => `${f.criterionDisputed} :: ${f.criterionDispute?.proposedWording}`)
        .sort(),
      [
        "R1 proves it :: R1 proves it, fixed",
        "R1 proves it :: R1 proves it, reworded",
        "R2 is judged by review :: R2 is judged by review, fixed",
      ],
      "every dispute survives with its own proposal",
    );
    const identical = matching().filter((f) => f.criterionDispute?.proposedWording === "R1 proves it, fixed");
    assert.equal(identical.length, 1, "the identical duplicate folded into one finding");
    assert.equal(identical[0].alsoRaisedBy!.length, 1, "the identical duplicate folded into it");
    const reworded = matching().find((f) => f.criterionDispute?.proposedWording === "R1 proves it, reworded")!;
    assert.equal((reworded.alsoRaisedBy ?? []).length, 0, "the same criterion with different wording was not folded");
    assert.deepEqual(
      setup.conductor.state.phase.decisions
        .filter((d) => d.amendment?.criterion === "R1 proves it")
        .map((d) => d.amendment!.proposedWording)
        .sort(),
      ["R1 proves it, fixed", "R1 proves it, reworded"],
      "each R1 proposal is voted on its own",
    );
  } finally {
    await teardown(setup);
  }
});

test("a conductor finding without TT_DOMAINS carries no domain", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript,
    // R2 (review-only) unmet by two of three seats: the conductor raises the
    // blocking finding. Without domains it must be exactly as before.
    reviewerScriptFor: (reviewer, state) => reviewerScript(reviewer, state, { r2: reviewer === "B" ? "met" : "unmet" }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.itemId === "R2" && f.severity === "blocking"), 90_000, 50, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.itemId === "R2")!;
    assert.equal(finding.domain, undefined, "a conductor finding carries no domain outside domain mode");
    assert.equal(finding.item, undefined, "no point item outside domain mode");
    assert.equal(finding.doneWhen, undefined);
  } finally {
    await teardown(setup);
  }
});

test("a plan without TT_DOMAINS records no point fields and refuses nothing new", async () => {
  const setup = await setupConductor({
    items: ITEMS,
    phaseChecks: [CHECK_OUTPUT],
    stubReviews: false,
    deadlines: FAST,
    workerScript,
    // No domains: a finding with no point fields is accepted as before, and
    // point fields a reviewer happens to send are ignored.
    reviewerScriptFor: (reviewer, state) =>
      reviewerScript(reviewer, state, { findings: [{ kind: "defect", severity: "advisory", evidence: "an ordinary finding", domain: "booking", item: "R1", where: "src/core/rounds.ts:1 fn", expected: "x", doneWhen: "true" }] }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.evidence === "an ordinary finding"), 90_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.contract.domains, undefined, "no domains are frozen");
    assert.ok(!readEvents(setup.runDir).some((r) => r.kind === "review_ingestion_rejected"), "nothing new is refused");
    const finding = setup.conductor.state.phase.findings.find((f) => f.evidence === "an ordinary finding")!;
    assert.equal(finding.domain, undefined, "a point field is ignored outside domain mode");
    assert.equal(finding.item, undefined);
    assert.equal(finding.where, undefined);
    assert.equal(finding.expected, undefined);
    assert.equal(finding.doneWhen, undefined);
  } finally {
    await teardown(setup);
  }
});
