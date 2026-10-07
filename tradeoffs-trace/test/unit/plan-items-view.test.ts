// Plan 06b: the item-by-seat matrix in the views. `tt summary`'s PR body and
// the runtime's review.org both render it; the counts line shows the state,
// e.g. `R 7/8 met · A 3/3 fit · C 2/2`.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { buildContract, type RunPlanFile, type RunPlanPhase } from "../../src/conductor.ts";
import { validate } from "../../src/core/schema.ts";
import { countsLine, matrixMarkdown, phaseItemCounts, tallyItems, type ItemLoopState } from "../../src/core/items.ts";
import { prSummary } from "../../src/view.ts";
import { projectEntryReview, renderStatusView, statusViewInput } from "../../src/render.ts";

const items = {
  architecture: [{ id: "A1", title: "Round", text: "interface Round", tags: ["data"], where: "src/core/rounds.ts" }],
  requirements: [
    { id: "R1", title: "Two candidates", text: "two candidates from one base", arch: ["A1"], verify: ['test "x"'] },
    { id: "R2", title: "Owner run", text: "the owner run is recorded", arch: [], verify: ["evidence"] },
  ],
  constraints: [{ id: "C1", title: "K = 1", text: "K = 1 is today's loop", verify: ["review"] }],
};

function phase(): RunPlanPhase {
  return {
    id: "p1",
    goal: "carry every point",
    acceptance: items.requirements.map((r) => r.text),
    checks: ["true"],
    boundaries: [],
    reserved: [],
    provisional: false,
    architecture: items.architecture,
    requirements: items.requirements,
    constraints: items.constraints,
  };
}

function plan(): RunPlanFile {
  return { title: "items", repo: "/tmp/repo", integrationBranch: "main", checks: ["true"], phases: [phase()] };
}

function reviews() {
  const one = () => ({
    review: {
      items: [
        { id: "R1", verdict: "met" as const, evidence: "src/core/rounds.ts:1" },
        { id: "R2", verdict: "met" as const, evidence: "src/core/rounds.ts:1" },
        { id: "C1", verdict: "met" as const, evidence: "src/core/rounds.ts:1" },
      ],
      arch: [{ id: "A1", verdict: "fits" as const, evidence: "src/core/rounds.ts:1" }],
    },
  });
  return { M: one(), A: one(), B: one() };
}

test("plan-items view: the counts line is R x/y met · A a/b fit · C c/d", () => {
  const loopState: ItemLoopState = {
    contract: phase(),
    reviews: {
      M: { review: { items: [{ id: "R1", verdict: "met", evidence: "src/a.ts:1" }, { id: "R2", verdict: "unmet", evidence: "src/a.ts:2" }, { id: "C1", verdict: "met", evidence: "src/a.ts:3" }], arch: [{ id: "A1", verdict: "fits", evidence: "src/a.ts:1" }] } },
      A: { review: { items: [{ id: "R1", verdict: "met", evidence: "src/a.ts:1" }, { id: "R2", verdict: "unmet", evidence: "src/a.ts:2" }, { id: "C1", verdict: "met", evidence: "src/a.ts:3" }], arch: [{ id: "A1", verdict: "fits", evidence: "src/a.ts:1" }] } },
      B: { review: { items: [{ id: "R1", verdict: "met", evidence: "src/a.ts:1" }, { id: "R2", verdict: "met", evidence: "src/a.ts:2" }, { id: "C1", verdict: "met", evidence: "src/a.ts:3" }], arch: [{ id: "A1", verdict: "fits", evidence: "src/a.ts:1" }] } },
    },
    checkResolution: [{ id: "R1", name: "x", outcome: "passed" }],
  };
  assert.equal(countsLine(phaseItemCounts(loopState)), "R 1/2 met · A 1/1 fit · C 1/1");
  const matrix = matrixMarkdown(loopState);
  assert.equal(matrix[0], "| item | worker | check | M | A | B |");
  assert.match(matrix.find((l) => l.startsWith("| [R1 "))!, /\| \[—\]\(items\/R1\.org\) \| \[pass\]\(items\/R1\.org\) \| \[met\]\(items\/R1\.org\) \| \[met\]\(items\/R1\.org\) \| \[met\]\(items\/R1\.org\) \|/);
  assert.match(matrix.find((l) => l.startsWith("| [R2 "))!, /\[unmet\]\(items\/R2\.org\) \| \[unmet\]\(items\/R2\.org\) \| \[met\]\(items\/R2\.org\)/);
});

test("plan-items view: tt summary's PR body carries the matrix", () => {
  const runDir = fs.mkdtempSync(path.join("/tmp", "tt-items-view-"));
  try {
    const md = prSummary(runDir, plan());
    assert.match(md, /\| item \| worker \| check \| M \| A \| B \|/);
    // OD-1 R7: every matrix cell links to the item's evidence file.
    assert.match(md, /\[R1 Two candidates\]\(items\/R1\.org\)/);
    assert.match(md, /\[A1 Round\]\(items\/A1\.org\)/);
    assert.match(md, /\[C1 K = 1\]\(items\/C1\.org\)/);
  } finally {
    fs.rmSync(runDir, { recursive: true, force: true });
  }
});

test("plan-items view: review.org carries the matrix", () => {
  const rendered = projectEntryReview({
    phaseId: "p1",
    contract: buildContract(phase()),
    reviews: reviews(),
    messages: [],
    entries: [],
  });
  assert.match(rendered.text, /\* Plan items/);
  assert.match(rendered.text, /\| item \| worker \| check \| M \| A \| B \|/);
  assert.match(rendered.text, /R 2\/2 met · A 1\/1 fit · C 1\/1/);
  // Each row's first cell links to the item's evidence file.
  assert.match(rendered.text, /\[\[items\/R2\.org\]\[R2 Owner run\]\]/);
  assert.deepEqual(rendered.itemFiles.map((f) => f.id), ["A1", "R1", "R2", "C1"]);
  assert.match(rendered.itemFiles.find((f) => f.id === "R2")!.contents, /Verdict: met/);
});

test("plan-items view: the status view shows the counts line", () => {
  const contract = buildContract(phase());
  const view = {
    elapsed: "1m",
    loop: "REVIEWING",
    pipeline: "reviews in",
    time: "1m",
    gates: "",
    gate: "",
    baseline: "",
    amendments: "",
    round: 1,
    reviewLine: "M ⧗   A ⧗   B ⧗",
    liveDecisions: 0,
    failedDecisions: 0,
    flaggedDecisions: 0,
    openFindings: 0,
    boundaryFilesChanged: 0,
    envTools: [],
  };
  const input = statusViewInput({
    runDir: "/tmp/tt-items-status",
    plan: { title: "items" },
    state: { phase: { phaseId: "p1", phase: "REVIEWING", contract, reviews: reviews(), checkResolution: [{ id: "R1", name: "x", outcome: "passed" }], attempt: { n: 1 } } },
    view: view as never,
    alive: true,
  });
  const text = renderStatusView(input);
  assert.match(text, /items\s+R 2\/2 met · A 1\/1 fit · C 1\/1/);
});

test("plan 06b: a structured phase without acceptance validates against the schemas", () => {
  // Every field the Emacs parser emits, including the line metadata, must be
  // allowed (OD-1 R8); no null optional field.
  const phase = {
    id: "p1",
    goal: "g",
    checks: ["true"],
    boundaries: [],
    reserved: [],
    provisional: false,
    architecture: [{ id: "A1", title: "A", text: "t", tags: ["data"], where: "src/a.ts", whereLine: 10, rawText: "t", line: 9 }],
    requirements: [{ id: "R1", title: "R", text: "t", arch: ["A1"], verify: ['test "x"'], rawText: "t", line: 20, archLine: 22, verifyLine: 23 }],
    constraints: [{ id: "C1", title: "C", text: "t", verify: ["review"], rawText: "t", line: 30, verifyLine: 32 }],
  };
  const planJson = { title: "t", checks: ["true"], phases: [phase] };
  const planSchema = JSON.parse(fs.readFileSync(new URL("../../schemas/plan.schema.json", import.meta.url), "utf8"));
  const planResult = validate(planSchema, planJson);
  assert.equal(planResult.valid, true, planResult.errors.join("; "));
  const contract = buildContract(phase as unknown as RunPlanPhase);
  const contractSchema = JSON.parse(fs.readFileSync(new URL("../../schemas/phase-contract.schema.json", import.meta.url), "utf8"));
  const contractResult = validate(contractSchema, contract);
  assert.equal(contractResult.valid, true, contractResult.errors.join("; "));
  // The old format is synthesized and not declared, so the conductor keeps it
  // unstructured (R9).
  const oldContract = buildContract({ id: "p1", goal: "g", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], provisional: false } as unknown as RunPlanPhase);
  assert.equal(oldContract.itemsSynthesized, true);
});

test("plan 06b: an old-format plan declares no items and adds no matrix", () => {
  const plain: RunPlanPhase = { id: "p1", goal: "g", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], provisional: false };
  const contract = buildContract(plain);
  assert.equal(contract.requirements, undefined, "the old format is not made structured");
  assert.equal(contract.itemsSynthesized, true);
  const rendered = projectEntryReview({ phaseId: "p1", contract, messages: [], entries: [] });
  assert.doesNotMatch(rendered.text, /Plan items/);
});