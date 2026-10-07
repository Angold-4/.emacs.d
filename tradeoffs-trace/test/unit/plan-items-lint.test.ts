// Plan 06b: `tt lint` reads a phase subtree too, and the four structured-plan
// rules each report with a file and a line (refs/06_ref_plan_format.md).

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import { formatFindings, hasLintErrors, lintPlan, lostTextLines, type LintPlanInput } from "../../src/core/plan-lint.ts";
import { PLAN_TEMPLATE, parseOrgPlan } from "../../src/core/org-plan.ts";

const STRUCTURED = `#+TITLE: structured
#+TT_REPO: /tmp/repo
#+TT_BRANCH: main
#+TT_CHECKS: make check

* Stage 1: two lanes
  :PROPERTIES:
  :ID:          stage-1
  :CHECKS:      make check
  :BOUNDARIES:  src/**
  :END:
** Goal
   The outcome and why, in the owner's words.
** Architecture
*** A1 Round                                                       :data:
    :PROPERTIES:
    :ID:       A1
    :WHERE:    src/core/rounds.ts
    :END:
    #+begin_src typescript
    interface Round { n: number }
    #+end_src
    Who writes it, who reads it.
*** A2 Pick tally                                                   :rule:
    :PROPERTIES:
    :ID:       A2
    :END:
    Strict majority of seats.
*** A3 Flow                                                         :flow:
    :PROPERTIES:
    :ID:       A3
    :END:
    Plan to worker to checks.
** Requirements
*** R1 Two candidates from one base
    :PROPERTIES:
    :ID:       R1
    :ARCH:     A1
    :VERIFY:   test "lanes: two candidates"
    :END:
    One paragraph.
    - a sub-point
    - another sub-point
*** R2 Owner's live run
    :PROPERTIES:
    :ID:       R2
    :VERIFY:   evidence
    :END:
    The owner records it.
*** R3 Reviewed
    :PROPERTIES:
    :ID:       R3
    :VERIFY:   review
    :END:
    A reviewer judges it.
*** R4 Tally
    :PROPERTIES:
    :ID:       R4
    :ARCH:     A2
    :VERIFY:   test "lanes: tally"
    :END:
    The tally is by code.
** Constraints
*** C1 K = 1
    :PROPERTIES:
    :ID:       C1
    :VERIFY:   test "lanes: one worker"
    :END:
    Today's loop.
*** C2 Public API
    :PROPERTIES:
    :ID:       C2
    :VERIFY:   review
    :END:
    The public API does not change.
`;

test("org-plan: a structured fixture parses to the goal, 3 architecture, 4 requirements, 2 constraints", () => {
  const plan = parseOrgPlan(STRUCTURED, "/x/PLAN.org");
  const phase = plan.phases![0];
  assert.equal(phase.id, "stage-1");
  assert.match(phase.goal!, /The outcome and why/);
  assert.deepEqual(phase.architecture!.map((a) => a.id), ["A1", "A2", "A3"]);
  assert.deepEqual(phase.architecture!.map((a) => a.tags), [["data"], ["rule"], ["flow"]]);
  assert.equal(phase.architecture![0].where, "src/core/rounds.ts");
  assert.deepEqual(phase.requirements!.map((r) => r.id), ["R1", "R2", "R3", "R4"]);
  assert.deepEqual(phase.requirements!.map((r) => r.arch), [["A1"], [], [], ["A2"]]);
  assert.deepEqual(phase.requirements!.map((r) => r.verify), [['test "lanes: two candidates"'], ["evidence"], ["review"], ['test "lanes: tally"']]);
  assert.deepEqual(phase.constraints!.map((c) => c.id), ["C1", "C2"]);
  // The sub-list and the source block stay inside their item's text.
  assert.match(phase.requirements![0].text!, /a sub-point/);
  assert.match(phase.requirements![0].text!, /another sub-point/);
  assert.match(phase.architecture![0].text!, /interface Round/);
  assert.match(phase.architecture![0].text!, /#\+begin_src typescript/);
  // And the whole fixture is lint-clean.
  assert.deepEqual(lintPlan(plan), []);
});

test("org-plan: tt plan template is a skeleton tt lint accepts", () => {
  const plan = parseOrgPlan(PLAN_TEMPLATE, "/x/template.org");
  const findings = lintPlan(plan);
  assert.deepEqual(findings, [], formatFindings(findings, "/x/template.org"));
  assert.equal(hasLintErrors(findings), false);
});

test("plan-lint: a duplicate :ID: is an error with file and line", () => {
  const plan: LintPlanInput = {
    sourceFile: "/x/PLAN.org",
    phases: [
      {
        id: "p1",
        architecture: [{ id: "A1", title: "a", text: "a", tags: [], line: 10 }],
        requirements: [
          { id: "R1", title: "r", text: "r", arch: ["A1"], verify: ["review"], line: 14 },
          { id: "R1", title: "r again", text: "r again", arch: [], verify: ["review"], line: 20 },
        ],
        constraints: [],
      },
    ],
  };
  const finding = lintPlan(plan).find((f) => f.rule === "item-id");
  assert.ok(finding);
  assert.equal(finding!.severity, "error");
  assert.equal(finding!.line, 20);
  assert.match(formatFindings([finding!], "/x/PLAN.org"), /^\/x\/PLAN\.org:20: error:/);
});

test("plan-lint: a missing :ID: is an error with the item's line", () => {
  const plan: LintPlanInput = {
    sourceFile: "/x/PLAN.org",
    phases: [{ id: "p1", architecture: [{ title: "a", text: "a", tags: [], line: 9 }], requirements: [], constraints: [] }],
  };
  const finding = lintPlan(plan).find((f) => f.rule === "item-id");
  assert.ok(finding);
  assert.equal(finding!.line, 9);
  assert.match(finding!.problem, /no :ID:/);
});

test("plan-lint: an :ARCH: naming no architecture item is an error with its line", () => {
  const plan: LintPlanInput = {
    sourceFile: "/x/PLAN.org",
    phases: [
      {
        id: "p1",
        architecture: [{ id: "A1", title: "a", text: "a", tags: [], line: 9 }],
        requirements: [{ id: "R1", title: "r", text: "r", arch: ["A9"], verify: ["review"], line: 15, archLine: 17 }],
        constraints: [],
      },
    ],
  };
  const finding = lintPlan(plan).find((f) => f.rule === "item-arch");
  assert.ok(finding);
  assert.equal(finding!.line, 17);
  assert.match(finding!.problem, /A9/);
});

test("plan-lint: a test verify without a name is an error with its line", () => {
  const plan: LintPlanInput = {
    sourceFile: "/x/PLAN.org",
    phases: [{ id: "p1", architecture: [], requirements: [{ id: "R1", title: "r", text: "r", arch: [], verify: ["test"], line: 15, verifyLine: 16 }], constraints: [] }],
  };
  const finding = lintPlan(plan).find((f) => f.rule === "item-verify");
  assert.ok(finding);
  assert.equal(finding!.line, 16);
  assert.match(finding!.problem, /no name/);
});

test("plan-lint: a truncated item text is an error with file and line", () => {
  const plan: LintPlanInput = {
    sourceFile: "/x/PLAN.org",
    phases: [
      {
        id: "p1",
        architecture: [],
        requirements: [
          {
            id: "R1",
            title: "r",
            // The parsed text stops at the paragraph; the sub-list line is lost.
            text: "One paragraph.",
            rawText: "One paragraph.\n- a lost sub-point",
            arch: [],
            verify: ["review"],
            line: 15,
          },
        ],
        constraints: [],
      },
    ],
  };
  const finding = lintPlan(plan).find((f) => f.rule === "item-text-loss");
  assert.ok(finding);
  assert.equal(finding!.line, 15);
  assert.match(finding!.problem, /missing 1 source line/);
  assert.match(formatFindings([finding!], "/x/PLAN.org"), /^\/x\/PLAN\.org:15: error:/);
  assert.deepEqual(lostTextLines("a\n- b", "a\n- b"), []);
});

test("org-plan: a source block keeps its lines verbatim, and is not read as headings or keywords", () => {
  const org = [
    "#+TITLE: src",
    "#+TT_REPO: /tmp/x",
    "#+TT_BRANCH: main",
    "",
    "* P",
    "  :PROPERTIES:",
    "  :ID: p1",
    "  :CHECKS: true",
    "  :END:",
    "** Architecture",
    "*** A1 Shape",
    "    :PROPERTIES:",
    "    :ID: A1",
    "    :END:",
    "    #+begin_src org",
    "    * not a headline",
    "    #+name: not-a-plan-keyword",
    "    :PROPERTIES: not a drawer",
    "    #+end_src",
    "    after the block",
    "",
  ].join("\n");
  const plan = parseOrgPlan(org, "/x/PLAN.org");
  const a1 = plan.phases![0].architecture![0];
  assert.match(a1.text!, /\* not a headline/);
  assert.match(a1.text!, /#\+name: not-a-plan-keyword/);
  assert.match(a1.text!, /:PROPERTIES: not a drawer/);
  assert.match(a1.text!, /after the block/);
  // The lines inside the block are not parsed as keywords or a new headline.
  assert.deepEqual(plan.phases!.length, 1);
  assert.equal(plan.phases![0].architecture!.length, 1);
  // The raw text is the literal source, so a dropped line would be caught.
  assert.match(a1.rawText!, /#\+name: not-a-plan-keyword/);
  assert.deepEqual(lintPlan(plan), []);
});

test("plan-lint: the runbook's 'Writing a plan' section documents the format", () => {
  const runbook = readFileSync(new URL("../../../docs/tradeoffs-trace-runbook.md", import.meta.url), "utf8");
  assert.match(runbook, /^## Writing a plan$/m);
  // The one complete example carries all four headings and a source block.
  assert.match(runbook, /\*\* Architecture[\s\S]*\*\* Requirements[\s\S]*\*\* Constraints/);
  assert.match(runbook, /#\+begin_src typescript/);
  assert.match(runbook, /:VERIFY:\s+evidence/);
});

test("plan-lint: an old-format phase (no structured items) is unchanged", () => {
  const plan: LintPlanInput = {
    sourceFile: "/x/PLAN.org",
    phases: [{ id: "p1", acceptance: ["existing tests pass", "no API change"], acceptanceLines: [5, 6] }],
  };
  assert.deepEqual(lintPlan(plan), []);
});
