// Plan 06b: the pure item machinery (src/core/items.ts). Every rule the loop
// carries mechanically — coverage completeness, test-verify resolution,
// verdict validation by code, the per-item majority, the matrix and the
// status counts — is exercised directly here, with no conductor.

import assert from "node:assert/strict";
import { test } from "node:test";

import {
  checklistLines,
  countsLine,
  coverageIssues,
  coverageNoteLines,
  evidenceFileAnchors,
  itemsAccept,
  itemsFromPhase,
  itemCounts,
  itemMatrix,
  parseVerify,
  repairItemLines,
  resolveTestVerifies,
  reviewItemsIssues,
  reverify,
  tallyItems,
  testOutcomeIn,
  verdictIssues,
  type Coverage,
  type PlanItems,
  type ReviewItems,
  type SeatItemVerdict,
} from "../../src/core/items.ts";

const structured: PlanItems = {
  goal: "make the loop carry every point",
  architecture: [
    { id: "A1", title: "Round", text: "interface Round { n: number }", tags: ["data"], where: "src/core/rounds.ts" },
    { id: "A2", title: "Pick tally", text: "Strict majority of seats.", tags: ["rule"] },
    { id: "A3", title: "Flow", text: "plan -> worker -> checks.", tags: ["flow"] },
  ],
  requirements: [
    { id: "R1", title: "Two candidates", text: "two candidates from one base", arch: ["A1"], verify: ['test "lanes: two candidates"'] },
    { id: "R2", title: "Owner run", text: "the owner run is recorded", arch: [], verify: ["evidence"] },
    { id: "R3", title: "Reviewed", text: "a reviewer judges it", arch: ["A2"], verify: ["review"] },
    { id: "R4", title: "Sub list", text: "keeps a sub-list", arch: ["A1"], verify: ["review"] },
  ],
  constraints: [
    { id: "C1", title: "K = 1", text: "K = 1 is today's loop", verify: ['test "lanes: one worker"'] },
    { id: "C2", title: "No API change", text: "public API stays", verify: ["review"] },
  ],
};

test("items: parseVerify reads test, review and evidence, with a file::name and several kinds", () => {
  assert.deepEqual(parseVerify(undefined), [{ kind: "review" }]);
  assert.deepEqual(parseVerify("review"), [{ kind: "review" }]);
  assert.deepEqual(parseVerify("evidence"), [{ kind: "evidence" }]);
  assert.deepEqual(parseVerify('test "a name"'), [{ kind: "test", name: "a name" }]);
  assert.deepEqual(parseVerify("test src/x.test.ts::works"), [{ kind: "test", file: "src/x.test.ts", name: "works" }]);
  assert.deepEqual(parseVerify('test "a" review'), [{ kind: "test", name: "a" }, { kind: "review" }]);
  // A bare `test` is a nameless test verify; lint rejects it.
  assert.deepEqual(parseVerify("test"), [{ kind: "test", name: "" }]);
});

test("items: the old format synthesizes R1..Rn and C1, evidence for an evidence: item", () => {
  const items = itemsFromPhase({
    goal: "g",
    acceptance: ["existing tests pass", "evidence: the owner live run is recorded", "no API change"],
    reserved: ["public API types", "persistence format"],
  });
  assert.deepEqual(items.requirements.map((r) => r.id), ["R1", "R2", "R3"]);
  assert.deepEqual(items.requirements.map((r) => r.verify), [["review"], ["evidence"], ["review"]]);
  assert.equal(items.constraints.length, 1);
  assert.equal(items.constraints[0].id, "C1");
  assert.equal(items.constraints[0].verify[0], "review");
  assert.deepEqual(items.architecture, []);
});

test("items: coverage must cover every R, C and A, and note a partial/not_done/deviates", () => {
  const partial: Coverage = {
    items: [
      { id: "R1", status: "done", where: ["src/a.ts:1"], tests: ["x"] },
      { id: "R3", status: "done", where: [], tests: [] },
      { id: "R4", status: "partial", where: [], tests: [] },
      { id: "C1", status: "done", where: [], tests: [] },
      { id: "C2", status: "done", where: [], tests: [] },
    ],
    arch: [
      { id: "A1", fits: "yes", where: [] },
      { id: "A2", fits: "yes", where: [] },
      { id: "A3", fits: "yes", where: [] },
    ],
  };
  const issues = coverageIssues(partial, structured);
  assert.ok(issues.some((i) => i.includes("R2")));
  assert.ok(issues.some((i) => i.includes("R4") && i.includes("no note")));

  const full: Coverage = {
    ...partial,
    items: [
      { id: "R1", status: "done", where: [], tests: [] },
      { id: "R2", status: "not_done", where: [], tests: [], note: "the owner has not run it yet" },
      { id: "R3", status: "done", where: [], tests: [] },
      { id: "R4", status: "partial", where: [], tests: [], note: "the sub-list is half done" },
      { id: "C1", status: "done", where: [], tests: [] },
      { id: "C2", status: "done", where: [], tests: [] },
    ],
  };
  assert.deepEqual(coverageIssues(full, structured), []);
  const notes = coverageNoteLines(full, structured);
  assert.equal(notes.length, 2);
  assert.match(notes[0].note, /^R2 not_done: the owner has not run it yet$/);
});

test("items: a deviating architecture item must carry a note", () => {
  const coverage: Coverage = {
    items: structured.requirements.concat(structured.constraints).map((i) => ({ id: i.id, status: "done" as const, where: [], tests: [] })),
    arch: [
      { id: "A1", fits: "yes", where: [] },
      { id: "A2", fits: "deviates", where: [] },
      { id: "A3", fits: "yes", where: [] },
    ],
  };
  assert.ok(coverageIssues(coverage, structured).some((i) => i.includes("A2") && i.includes("deviates")));
});

test("items: a test verify is resolved against the node check output by name", () => {
  const output = ["✔ lanes: two candidates (1.2ms)", "not ok 2 - lanes: one worker", "# fail 1"].join("\n");
  assert.equal(testOutcomeIn(output, "lanes: two candidates"), "passed");
  assert.equal(testOutcomeIn(output, "lanes: one worker"), "failed");
  assert.equal(testOutcomeIn(output, "never ran"), "missing");
  // The name is the reporter's own token, never a substring of a longer one
  // (finding M-14): `lanes: tally` must not be satisfied by `lanes: tally
  // extended`.
  assert.equal(testOutcomeIn("✔ lanes: tally extended", "lanes: tally"), "missing");
  assert.equal(testOutcomeIn("ok 3 - lanes: tally extended", "lanes: tally"), "missing");
  assert.equal(testOutcomeIn("✔ lanes: tally", "lanes: tally"), "passed");
  const resolved = resolveTestVerifies(structured, output);
  assert.deepEqual(resolved, [
    { id: "R1", name: "lanes: two candidates", outcome: "passed" },
    { id: "C1", name: "lanes: one worker", outcome: "failed" },
  ]);
});

test("items: a review that omits an id is incomplete", () => {
  const review: ReviewItems = {
    items: [
      { id: "R1", verdict: "met", evidence: "src/a.ts:1" },
      { id: "R3", verdict: "met", evidence: "src/a.ts:1" },
      { id: "R4", verdict: "met", evidence: "src/a.ts:1" },
      { id: "C1", verdict: "met", evidence: "src/a.ts:1" },
      { id: "C2", verdict: "met", evidence: "src/a.ts:1" },
    ],
    arch: [
      { id: "A1", verdict: "fits", evidence: "src/a.ts:1" },
      { id: "A2", verdict: "fits", evidence: "src/a.ts:1" },
      { id: "A3", verdict: "fits", evidence: "src/a.ts:1" },
    ],
  };
  const issues = reviewItemsIssues(review, structured);
  assert.ok(issues.some((i) => i.includes("omits a verdict for R2")));
});

test("items: a strict majority of seats decides each item", () => {
  const reviews = [
    { seat: "M" as const, items: { items: [
      { id: "R2", verdict: "unmet" as const, evidence: "src/a.ts:3 the call is missing" },
      { id: "R1", verdict: "met" as const, evidence: "src/a.ts:1" },
    ], arch: [] } },
    { seat: "A" as const, items: { items: [{ id: "R2", verdict: "unmet" as const, evidence: "src/a.ts:3" }], arch: [] } },
    { seat: "B" as const, items: { items: [{ id: "R2", verdict: "met" as const, evidence: "src/a.ts:3" }], arch: [] } },
  ];
  const outcomes = tallyItems(structured, reviews);
  const r2 = outcomes.find((o) => o.item.id === "R2")!;
  assert.equal(r2.outcome, "unmet");
  assert.equal(r2.evidence.length, 2);
  // Only M gave R1 a verdict, so no majority: incomplete, and it blocks.
  const r1 = outcomes.find((o) => o.item.id === "R1")!;
  assert.equal(r1.outcome, "incomplete");
});

test("items: verdict validation refuses a missing file, a missing line, a stale test and only-worker anchors", () => {
  const item = structured.requirements[0];
  const ctx = {
    lineCount: (p: string) => (p === "src/a.ts" ? 10 : undefined),
    diffFiles: ["src/a.ts"],
    where: undefined,
    testOutcomes: new Map([["lanes: two candidates", "passed" as const]]),
    reviewerReadFiles: ["src/a.ts"],
    workerAnchors: ["src/a.ts"],
  };
  // A line beyond the file: refused.
  assert.ok(verdictIssues(item, { id: "R1", verdict: "met", evidence: "src/a.ts:99" }, ctx).some((i) => i.includes("do not exist")));
  // A file the candidate does not have: refused.
  assert.ok(verdictIssues(item, { id: "R1", verdict: "met", evidence: "src/gone.ts:1" }, ctx).some((i) => i.includes("does not exist")));
  // A cited test that did not pass: refused.
  assert.ok(verdictIssues(item, { id: "R1", verdict: "met", evidence: 'test "other test"' }, ctx).some((i) => i.includes("not in this check run")));
  // A met verdict whose only anchor is the worker's and which the reviewer
  // never read: refused.
  assert.ok(
    verdictIssues(item, { id: "R1", verdict: "met", evidence: "src/a.ts:1" }, { ...ctx, reviewerReadFiles: [] }).some((i) => i.includes("read yourself")),
  );
  // A clean met verdict counts.
  assert.deepEqual(verdictIssues(item, { id: "R1", verdict: "met", evidence: "src/a.ts:1-3" }, ctx), []);
  // No anchor at all: refused.
  assert.ok(verdictIssues(item, { id: "R1", verdict: "unmet", evidence: "it is missing" }, ctx).some((i) => i.includes("no anchor")));
});

test("items: the evaluator overturns a majority unmet verdict the item's passing test contradicts", () => {
  const item = structured.requirements[0];
  const ctx = {
    lineCount: () => 10,
    diffFiles: ["src/a.ts"],
    testOutcomes: new Map([["lanes: two candidates", "passed" as const]]),
    reviewerReadFiles: ["src/a.ts"],
    workerAnchors: [],
  };
  const verdicts: SeatItemVerdict[] = [
    { seat: "M", verdict: "unmet", evidence: "src/a.ts:1" },
    { seat: "A", verdict: "unmet", evidence: "src/a.ts:1" },
    { seat: "B", verdict: "met", evidence: "src/a.ts:1" },
  ];
  const overturns = reverify(item, verdicts, ctx);
  // Every contradicted seat is overturned and counted (finding M-3).
  assert.equal(overturns.length, 2);
  assert.deepEqual(overturns.map((o) => o.seat).sort(), ["A", "M"]);
  assert.equal(overturns[0].id, "R1");
  assert.equal(overturns[0].effect, "flip");
});

test("items: a majority deviates on an architecture item whose :WHERE: symbols are present is overturned (B-18)", () => {
  const arch = structured.architecture[0];
  const item = { kind: "architecture" as const, id: arch.id, title: arch.title, text: arch.text, arch: [], verify: [], where: arch.where, tags: arch.tags };
  const verdicts: SeatItemVerdict[] = [
    { seat: "M", verdict: "deviates", evidence: "src/a.ts:1" },
    { seat: "A", verdict: "deviates", evidence: "src/a.ts:1" },
    { seat: "B", verdict: "fits", evidence: "src/a.ts:1" },
  ];
  const ctx = {
    lineCount: () => 10,
    diffFiles: ["src/a.ts"],
    testOutcomes: new Map(),
    reviewerReadFiles: ["src/a.ts"],
    workerAnchors: [],
    archSymbolsPresent: () => true,
  };
  const overturns = reverify(item, verdicts, ctx);
  assert.equal(overturns.length, 2);
  assert.ok(overturns.every((o) => o.effect === "flip"));
  // The flipped tally is a majority fits.
  assert.equal(tallyItems({ goal: "", architecture: [arch], requirements: [], constraints: [] }, [{ seat: "M", items: { items: [], arch: [{ id: arch.id, verdict: "deviates", evidence: "src/a.ts:1" }] } }, { seat: "A", items: { items: [], arch: [{ id: arch.id, verdict: "deviates", evidence: "src/a.ts:1" }] } }, { seat: "B", items: { items: [], arch: [{ id: arch.id, verdict: "fits", evidence: "src/a.ts:1" }] } }], overturns)[0].outcome, "fits");
});

test("items: the matrix and the counts line show every item by seat", () => {
  const reviews = [
    { seat: "M" as const, items: { items: structured.requirements.concat(structured.constraints).map((i) => ({ id: i.id, verdict: "met" as const, evidence: "src/a.ts:1" })), arch: structured.architecture.map((a) => ({ id: a.id, verdict: "fits" as const, evidence: "src/a.ts:1" })) } },
    { seat: "A" as const, items: { items: structured.requirements.concat(structured.constraints).map((i) => ({ id: i.id, verdict: "met" as const, evidence: "src/a.ts:1" })), arch: structured.architecture.map((a) => ({ id: a.id, verdict: "fits" as const, evidence: "src/a.ts:1" })) } },
    { seat: "B" as const, items: { items: structured.requirements.concat(structured.constraints).map((i) => ({ id: i.id, verdict: "met" as const, evidence: "src/a.ts:1" })), arch: structured.architecture.map((a) => ({ id: a.id, verdict: "fits" as const, evidence: "src/a.ts:1" })) } },
  ];
  const outcomes = tallyItems(structured, reviews);
  const counts = itemCounts(structured, outcomes);
  assert.deepEqual(counts, { requirementsMet: 4, requirementsTotal: 4, architectureFit: 3, architectureTotal: 3, constraintsMet: 2, constraintsTotal: 2 });
  assert.equal(countsLine(counts), "R 4/4 met · A 3/3 fit · C 2/2");
  const matrix = itemMatrix(structured, outcomes, undefined, resolveTestVerifies(structured, "✔ lanes: two candidates\nok 2 - lanes: one worker"));
  const row = matrix.find((r) => r.id === "R1")!;
  assert.deepEqual(row.cells.map((c) => c.by), ["worker", "check", "M", "A", "B"]);
  assert.equal(row.cells.find((c) => c.by === "check")!.text, "pass");
  assert.equal(row.cells.find((c) => c.by === "M")!.evidence, "src/a.ts:1");
});

test("items: acceptance requires every R/C met and every A fits or its deviation accepted", () => {
  const all = structured.requirements.concat(structured.constraints).map((i) => ({ id: i.id, verdict: "met" as const, evidence: "src/a.ts:1" }));
  const arch = structured.architecture.map((a) => ({ id: a.id, verdict: "fits" as const, evidence: "src/a.ts:1" }));
  const reviews = (["M", "A", "B"] as const).map((seat) => ({ seat, items: { items: all, arch } }));
  const outcomes = tallyItems(structured, reviews);
  assert.equal(itemsAccept(structured, outcomes, { evidenceRecorded: ["R2"] }), true);
  assert.equal(itemsAccept(structured, outcomes, {}), false, "an unrecorded evidence item blocks acceptance");
  // An A that deviates blocks until the owner accepts the deviation.
  const deviating = structured.architecture.map((a) => ({ id: a.id, verdict: (a.id === "A1" ? "deviates" : "fits") as "fits" | "deviates", evidence: "src/a.ts:1" }));
  const devOutcomes = tallyItems(structured, (["M", "A", "B"] as const).map((seat) => ({ seat, items: { items: all, arch: deviating } })));
  assert.equal(itemsAccept(structured, devOutcomes, { evidenceRecorded: ["R2"] }), false);
  assert.equal(itemsAccept(structured, devOutcomes, { evidenceRecorded: ["R2"], acceptedDeviations: ["A1"] }), true);
  assert.equal(repairItemLines(devOutcomes).some((l) => l.startsWith("A1")), true);
});

test("items: the checklist renders every item by id, with its verify kinds", () => {
  const lines = checklistLines(structured);
  assert.ok(lines.some((l) => l.startsWith("- A1") && l.includes("data") && l.includes("src/core/rounds.ts")));
  assert.ok(lines.some((l) => l.startsWith("- R1") && l.includes('test "lanes: two candidates"')));
  assert.ok(lines.some((l) => l.startsWith("- C1")));
});

test("items: evidenceFileAnchors reads a range", () => {
  assert.deepEqual(evidenceFileAnchors("src/a.ts:10-12 and src/b.ts:3"), [
    { path: "src/a.ts", start: 10, end: 12 },
    { path: "src/b.ts", start: 3, end: 3 },
  ]);
});
