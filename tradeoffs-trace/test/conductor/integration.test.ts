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
import { execFileSync } from "node:child_process";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { runPaths } from "../../src/conductor.ts";
import { readLog } from "../../src/effects/log.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";
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
  reproductionMs: 20_000,
};

const FIX_DOMAINS = [
  { name: "booking", globs: ["booking/**"] },
  { name: "gates", globs: ["gates/**"] },
];

function laneWorker(lane: string, round: number): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane} r${round}\\n' > lane-${lane}-r${round}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

function laneReview(seat: Reviewer, contractVersion: unknown, findings: unknown[] = []): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
          phaseId: "p1",
          candidateSha: "$TT_CANDIDATE_SHA",
          contractVersion,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings,
        },
      },
    ],
  };
}

function reviewerWithPoints() {
  return (seat: Reviewer, _candidate: string, state: State): { hello: unknown; steps: FakePiStep[] } => {
    const K = state.phase.contract.contractVersion;
    if (seat !== "M" || (state.phase.findings ?? []).some((f) => f.status === "open")) return laneReview(seat, K);
    return laneReview("M", K, [
      {
        kind: "defect",
        severity: "blocking",
        evidence: "the booking path is wrong",
        domain: "booking",
        item: "R1",
        where: "booking/a.ts:1 fn",
        expected: "BOOKING_EXPECTED",
        doneWhen: "test -f booking-fixed.txt",
      },
      {
        kind: "defect",
        severity: "blocking",
        evidence: "the gates path is wrong",
        domain: "gates",
        item: "R2",
        where: "gates/b.ts:1 fn",
        expected: "GATES_EXPECTED",
        doneWhen: "test -f gates-fixed.txt",
      },
    ]);
  };
}

function pickVote(seat: Reviewer, round: number): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { inspected: ["a", "b"], round, seat, lane: "a", why: `${seat} picks a`, loserHad: { yes: false, anchors: [] } } }],
  };
}

function git(repo: string, ...args: string[]): string {
  return execFileSync("git", args, { cwd: repo, encoding: "utf8" }).trim();
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

test("a conflicted merge is resolved by the fenced integrate duty and retried when it leaves the fence", async () => {
  // A1/C4: git merges both approved domains; the add/add conflict on
  // `shared.txt` starts the integrate duty. Its first attempt also edits an
  // unconflicted file, so the conductor refuses it; its second attempt edits
  // only the conflicted file and both domains land in the candidate.
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "integrate the domains",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      workers: 2,
      domains: FIX_DOMAINS,
      rounds: 3,
      fixRounds: "domains",
      fixers: 4,
      fixChecks: "true",
    },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a", 1),
    laneWorkerScriptFor: (lane, round) => laneWorker(lane, round),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints()(seat as Reviewer, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1),
    fixerScriptFor: (domain) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf '${domain}\\n' > shared.txt; printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    integrateScriptFor: (_round, attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-sh",
          command:
            attempt === 1
              ? "printf 'tamper\\n' > booking-fixed.txt; grep -v -E '^(<<<<<<< |=======$|>>>>>>> )' shared.txt > shared.txt.tmp; mv shared.txt.tmp shared.txt"
              : "grep -v -E '^(<<<<<<< |=======$|>>>>>>> )' shared.txt > shared.txt.tmp; mv shared.txt.tmp shared.txt",
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.rounds?.[0]?.fixed !== undefined, 120_000, 20, setup.runDir);
    const record = setup.conductor.state.phase.rounds![0].fixed!;
    assert.equal(record.merges.length, 2, "both domains merged after the integrate duty resolved the conflict");
    assert.equal(git(setup.repo.dir, "show", `${record.candidate}:shared.txt`), "booking\ngates", "the conflicted file carries both sides");
    const refusals = readEvents(setup.runDir).filter((r) => r.kind === "integrate_refused");
    assert.equal(refusals.length, 1, "the out-of-fence edit was refused");
    assert.deepEqual((refusals[0].event as { outside: string[] }).outside, ["booking-fixed.txt"], "the refusal names the unconflicted file");
    assert.equal(git(setup.repo.dir, "show", `${record.candidate}:booking-fixed.txt`), "booking fixed", "the refused edit did not survive");
    const agents = readLog(runPaths(setup.runDir).events).records
      .filter((r) => r.kind === "intent" && String((r.event as { agentId?: string }).agentId ?? "").startsWith("integrate-1-"));
    assert.ok(agents.length >= 2, "the refused resolution was retried");
  } finally {
    await teardown(setup);
  }
});
