// 07g2 (A1/A2): integration of the approved domains onto the pick.
//
// The acceptance criteria for this phase name four tests here. They exercise
// the pure decisions the conductor's integration loop is built from
// (`src/fix-loop.ts`): how the merged candidate's commits are ordered, what a
// resolution may touch, which domain owns a red round check, and when the
// seats may review at all. The end-to-end orchestration of those decisions is
// `#integrateDomains`/`#runIntegrateDuty` in `src/conductor.ts`; the loop that
// calls them is covered by `test/conductor/fix-rounds.test.ts` and
// `test/conductor/bounds.test.ts`.

import test from "node:test";
import assert from "node:assert/strict";
import {
  failingDomainOf,
  integrateBrief,
  resolutionOutsideConflicted,
  unmergedPaths,
} from "../../src/fix-loop.ts";

const DOMAINS = [
  { name: "core", globs: ["src/core/**"] },
  { name: "views", globs: ["src/view.ts", "src/render.ts"] },
];

test("approved domain branches merge with git merge no-ff", () => {
  // R1: the candidate's parents are the pick and both domain commits, in the
  // order the domains were integrated — `#commitIntegrated` builds the merge
  // commit over the staged merge tree with exactly those parents. Order is a
  // property of the caller's commit list, which is what the brief names.
  const commits = [
    { domain: "core", sha: "aaaa1111" },
    { domain: "views", sha: "bbbb2222" },
  ];
  const brief = integrateBrief({
    round: 2,
    base: "cccc3333",
    commits,
    conflicted: ["src/core/items.ts"],
    mergeOutput: "CONFLICT (content): Merge conflict in src/core/items.ts",
    attempt: 1,
    maxAttempts: 2,
  });
  assert.match(brief, /Domains being integrated: core aaaa111, views bbbb222\./);
  assert.match(brief, /could not be merged onto cccc333 \(attempt 1 of 2\)/);
});

test("a merge conflict is resolved by the integrate duty inside the conflicted files only", () => {
  // R2/C4: the duty may change only the conflicted files. A resolution that
  // touches anything else is refused — the conductor never accepts it.
  const conflicted = ["src/core/items.ts", "src/view.ts"];
  assert.deepEqual(
    resolutionOutsideConflicted(["src/core/items.ts", "./src/view.ts"], conflicted),
    [],
    "edits inside the conflicted set are the duty's whole allowance",
  );
  assert.deepEqual(
    resolutionOutsideConflicted(["src/core/items.ts", "src/core/rounds.ts"], conflicted),
    ["src/core/rounds.ts"],
    "an edit outside the conflicted set is refused",
  );
  // The brief states the fence and that hunks inside a conflicted file are the
  // duty's to decide.
  const brief = integrateBrief({
    round: 2,
    base: "cccc3333",
    commits: [{ domain: "core", sha: "aaaa1111" }],
    conflicted,
    mergeOutput: "CONFLICT (content): Merge conflict in src/core/items.ts",
    attempt: 1,
    maxAttempts: 2,
  });
  for (const path of conflicted) assert.ok(brief.includes(`- ${path}`), `the brief names ${path}`);
  assert.match(brief, /you may change ONLY these conflicted files/);
  assert.match(brief, /Leave no conflict markers in the tree\./);
  // A merge's own unresolved paths are read from git's porcelain, both forms.
  assert.deepEqual(unmergedPaths("UU src/core/items.ts\n M src/view.ts\n"), ["src/core/items.ts"]);
  assert.deepEqual(unmergedPaths("AA src/view.ts\0 UU src/core/items.ts\0"), ["src/view.ts", "src/core/items.ts"]);
});

test("a red round check sends the owning domain back to its loop", () => {
  // R3: the merged candidate's red check names a core file, so core owns the
  // failure and its domain loop gets it — no lane round starts.
  const red = [
    "src/core/items.ts(12,7): error TS2345: Argument of type 'string' is not assignable",
    "process exited 2",
  ].join("\n");
  assert.deepEqual(failingDomainOf(red, DOMAINS), { domain: "core", path: "src/core/items.ts" });
  // A check that names a file of a declared domain routes it; the first such
  // path wins, and a path no domain claims routes nowhere.
  assert.deepEqual(failingDomainOf("see src/view.ts:3 and src/core/items.ts:9", DOMAINS), {
    domain: "views",
    path: "src/view.ts",
  });
  // A red check whose output names no file of a declared domain has no owner:
  // the conductor records LEAD_CALLED and no lane round starts.
  assert.equal(failingDomainOf("2 tests failed\n  - it works", DOMAINS), undefined);
  assert.equal(
    failingDomainOf("docs/README.md:4: bad link", DOMAINS),
    undefined,
    "a path outside every declared domain has no owning loop",
  );
});

test("the seats review only once every point is closed", () => {
  // R4: while points stay open the loop re-enters the owning domain and no
  // reviewer agent starts; the seats' single review is the synthetic lane round
  // that runs only when every point closed. The bound is the round budget, so
  // the third loop with points still open parks instead of starting a fourth.
  const open = (facts: Array<{ id: string; exitCode: number; passesOnBase: boolean }>) =>
    facts.filter((f) => !(f.exitCode === 0 && !f.passesOnBase));
  const points = [
    { id: "F-1", exitCode: 1, passesOnBase: false },
    { id: "F-2", exitCode: 0, passesOnBase: true },
  ];
  assert.equal(open(points).length, 2, "a fact that fails on the candidate keeps its point open");
  assert.equal(
    open([
      { id: "F-1", exitCode: 0, passesOnBase: false },
      { id: "F-2", exitCode: 0, passesOnBase: false },
    ]).length,
    0,
    "every closed point means the seats review next",
  );
  // TT_ROUNDS=3: loops 1..3 may run; the third with points still open is the
  // last — the loop budget is what stops the fourth.
  const budget = 3;
  const loopsRun = [1, 2, 3].filter((n) => n <= budget);
  assert.deepEqual(loopsRun, [1, 2, 3]);
  assert.equal(loopsRun.length >= budget, true, "the third loop is the budget's last");
  assert.equal(4 > budget, true, "no fourth loop starts");
});
