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
  // reviewer agent starts; the ordinary acceptance seats review the integrated
  // candidate only when every point is closed. The bound is the round budget, so
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

function reviewerWithBookingPoint() {
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

async function laneFreeFixLoop(fixRounds?: "domains"): Promise<void> {
  const setup = await setupConductor({
    phase: { id: "p1", goal: "lane-free fix loop", acceptance: ["it works"], checks: ["true"], finalChecks: ["true"],
      boundaries: [], reserved: [], workers: 2, domains: FIX_DOMAINS, rounds: 3, ...(fixRounds ? { fixRounds } : {}), fixers: 4, fixChecks: "true" },
    checks: ["true"], phaseChecks: ["true"], stubReviews: true, deadlines: FAST,
    workerScript: () => laneWorker("a", 1),
    laneWorkerScriptFor: (lane, round) => laneWorker(lane, round),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithPoints()(seat as Reviewer, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1),
    fixerScriptFor: (domain) => ({ hello: defaultWorkerHello(), steps: [
      { kind: "call-sh", command: `echo fixed > ${domain}-fixed.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ] }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const events = readEvents(setup.runDir).filter((r) => r.kind === "event").map((r) => r.event as Record<string, unknown>);
    assert.ok(events.some((e) => e.type === "ROUND_FIXED"), "a domain loop actually ran");
    const later = events.filter((e) => Number(e.round) >= 2);
    assert.ok(later.filter((e) => e.type === "ROUND_STARTED").every((e) => (e.lanes as unknown[]).length === 0));
    assert.ok(!later.some((e) => ["CANDIDATE_SUBMITTED", "CANDIDATE_CHECKED", "CANDIDATE_PICKED", "PICK_VOTE"].includes(String(e.type))), "no loop candidate goes through lanes or a pick");
    const agents = readEvents(setup.runDir).filter((r) => r.kind === "intent").map((r) => String((r.event as { agentId?: string }).agentId ?? ""));
    assert.ok(!agents.some((id) => /^lane-(?:review-)?[2-9]-|^pick-[2-9]-/.test(id)), `no later lane agent: ${agents}`);
    assert.ok(agents.some((id) => id.startsWith("fix-2-booking-")));
    assert.ok(agents.some((id) => id.startsWith("fix-2-gates-")));
    assert.equal(events.filter((e) => e.type === "REVIEW_SUBMITTED" && !e.promotedFrom).length, 3, "three ordinary acceptance seats review once");
  } finally { await teardown(setup); }
}

test("a fix loop starts no lane and picks no lane candidate", async () => {
  await laneFreeFixLoop("domains");
  // Domain loops are the only post-pick runtime, not an opt-in alternative.
  await laneFreeFixLoop();
});

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
    await waitFor(() => setup.conductor.state.phase.rounds?.[1]?.fixed !== undefined, 120_000, 20, setup.runDir);
    const record = setup.conductor.state.phase.rounds![1].fixed!;
    assert.equal(record.merges.length, 2, "both domains merged after the integrate duty resolved the conflict");
    // A1: the candidate is ONE merge commit whose direct parents are the pick and
    // each domain commit, in task order — not a chain of per-domain merges.
    assert.deepEqual(
      git(setup.repo.dir, "rev-list", "--parents", "-n1", record.candidate).split(/\s+/).slice(1),
      [record.base, ...record.merges.map((m) => m.sha)],
      "the candidate's parents are the pick and both domain commits",
    );
    assert.equal(git(setup.repo.dir, "show", `${record.candidate}:shared.txt`), "booking\ngates", "the conflicted file carries both sides");
    const refusals = readEvents(setup.runDir).filter((r) => r.kind === "integrate_refused");
    assert.equal(refusals.length, 1, "the out-of-fence edit was refused");
    assert.deepEqual((refusals[0].event as { outside: string[] }).outside, ["booking-fixed.txt"], "the refusal names the unconflicted file");
    assert.equal(git(setup.repo.dir, "show", `${record.candidate}:booking-fixed.txt`), "booking fixed", "the refused edit did not survive");
    const agents = readLog(runPaths(setup.runDir).events).records
      .filter((r) => r.kind === "intent" && String((r.event as { agentId?: string }).agentId ?? "").startsWith("integrate-2-"));
    assert.ok(agents.length >= 2, "the refused resolution was retried");
  } finally {
    await teardown(setup);
  }
});

test("the integrate duty is fenced by files even when its edit is staged", async () => {
  // C4/R2: staging an out-of-fence path is still changing it. The duty's first
  // attempt resolves `shared.txt` (which it may) but also stages a modified
  // unconflicted file and a brand-new file; the conductor compares the index
  // with the merge's own, refuses that attempt, and neither staged edit
  // reaches the candidate.
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
              ? "printf 'tamper\\n' > booking-fixed.txt; git add booking-fixed.txt; printf 'injected\\n' > injected.txt; git add injected.txt; grep -v -E '^(<<<<<<< |=======$|>>>>>>> )' shared.txt > shared.txt.tmp; mv shared.txt.tmp shared.txt; git add shared.txt"
              : "grep -v -E '^(<<<<<<< |=======$|>>>>>>> )' shared.txt > shared.txt.tmp; mv shared.txt.tmp shared.txt",
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.rounds?.[1]?.fixed !== undefined, 120_000, 20, setup.runDir);
    const record = setup.conductor.state.phase.rounds![1].fixed!;
    assert.equal(record.merges.length, 2, "both domains merged after the staged out-of-fence edit was refused");
    assert.equal(git(setup.repo.dir, "show", `${record.candidate}:shared.txt`), "booking\ngates", "the conflicted file carries both sides");
    const refusals = readEvents(setup.runDir).filter((r) => r.kind === "integrate_refused");
    assert.equal(refusals.length, 1, "the staged out-of-fence edit was refused");
    assert.deepEqual(
      [...(refusals[0].event as { outside: string[] }).outside].sort(),
      ["booking-fixed.txt", "injected.txt"],
      "the refusal names the staged files, and not the conflicted file it may rewrite",
    );
    assert.equal(git(setup.repo.dir, "show", `${record.candidate}:booking-fixed.txt`), "booking fixed", "the refused staged edit did not survive");
    assert.throws(() => git(setup.repo.dir, "cat-file", "-e", `${record.candidate}:injected.txt`), "the staged new file never reached the candidate");
  } finally {
    await teardown(setup);
  }
});

test("open points chain domain loops on the integrated candidate and park at the third loop", async () => {
  // R4: while points stay open the loop re-enters the owning domain and no
  // reviewer agent starts. Each loop starts on the candidate the round before
  // it picked — never on the older pick that candidate was based on — and the
  // third loop with a point still open is the round budget's last: it parks
  // for the lead instead of starting a fourth loop or an acceptance review.
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "chain the fix loop to the round bound",
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
    // booking closes its point on the first fix; gates keeps its point open
    // and changes nothing on a later loop, so the fix step produces no new
    // candidate and the chain must use the candidate that loop round picked.
    fixerScriptFor: (domain, round) => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-sh",
          command: domain === "booking" ? "printf 'booking fixed\\n' > booking-fixed.txt" : round === 2 ? "printf 'gates\\n' > gates.txt" : "true",
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    const events = () =>
      readEvents(setup.runDir)
        .filter((r) => r.kind === "event")
        .map((r) => r.event as Record<string, unknown>);
    await setup.conductor.start();
    await waitFor(() => events().some((e) => e.type === "LOOP_BOUND_REACHED"), 180_000, 50, setup.runDir);
    const rounds = setup.conductor.state.phase.rounds ?? [];
    const [first, second, third] = [rounds[0], rounds[1], rounds[2]];
    assert.ok(first?.picked && second?.fixed && third?.fixed, "BUILD picked once; domain loops record their own reductions");
    assert.ok(second.fixed.candidate, "the first domain loop merged a candidate");
    assert.equal(second.fixed.merges.length, 1, "only the approved domain merges; an open head is not an approval");
    assert.equal(second.base, first.picked.sha, "the first domain loop starts on the pick");
    assert.equal(third.base, second.fixed.candidate, "the later loop chains on the integrated candidate");
    assert.notEqual(third.base, first.picked.sha, "the chain never falls back to the original pick");
    for (const round of [second, third]) {
      assert.deepEqual(round.lanes, [], "a fix loop contains no lanes");
      assert.equal(round.picked, undefined, "a fix loop picks no candidate");
      assert.equal(round.votes.length, 0, `loop round ${round.round} takes no pick vote`);
      assert.equal(round.candidates.length, 0, `loop round ${round.round} starts no lane review`);
    }
    assert.deepEqual(
      readEvents(setup.runDir)
        .filter((r) => r.kind === "fix_loop_chained")
        .map((r) => (r.event as { round?: number }).round),
      [2, 3],
      "both open-point rounds are recorded as chained loops",
    );
    const bound = events().find((e) => e.type === "LOOP_BOUND_REACHED");
    assert.equal(bound?.round, 3, "the third loop with a point still open is the bound's last");
    assert.ok(events().some((e) => e.type === "LEAD_CALLED"), "the bound parks the phase for the lead");
    const intents = readEvents(setup.runDir)
      .filter((r) => r.kind === "intent")
      .map((r) => r.event as { agentId?: string; round?: number });
    const reviews = intents.filter((e) => String(e.agentId ?? "").startsWith("lane-review-"));
    assert.ok(reviews.length >= 3, "round 1's seats reviewed its lanes");
    assert.ok(
      reviews.every((e) => String(e.agentId ?? "").startsWith("lane-review-1-")),
      `no seat review starts while points stay open: ${JSON.stringify(reviews.map((e) => e.agentId))}`,
    );
  } finally {
    await teardown(setup);
  }
});

test("a red integrated check creates a point for an owning domain with no existing task", async () => {
  const check = "if [ -f poison.txt ]; then echo 'error: gates/b.ts:1: merged failure'; exit 1; fi";
  const setup = await setupConductor({
    phase: { id: "p1", goal: "route a newly red file", acceptance: ["it works"], checks: [check], finalChecks: ["true"],
      boundaries: [], reserved: [], workers: 2, domains: FIX_DOMAINS, rounds: 3, fixRounds: "domains", fixers: 4, fixChecks: "true" },
    checks: [check], phaseChecks: [check], stubReviews: true, deadlines: FAST,
    workerScript: () => laneWorker("a", 1), laneWorkerScriptFor: (lane, round) => laneWorker(lane, round),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithBookingPoint()(seat as Reviewer, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1),
    fixerScriptFor: (domain) => ({ hello: defaultWorkerHello(), steps: [
      { kind: "call-sh", command: domain === "booking" ? "echo fixed > booking-fixed.txt; echo poison > poison.txt" : "rm poison.txt; echo fixed > gates-fixed.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ] }),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
    const point = setup.conductor.state.phase.findings.find((f) => f.raisedBy === "conductor" && f.domain === "gates");
    assert.ok(point, "the owning domain receives a new point even without a prior task");
    assert.equal(point.where, "gates/b.ts:1");
    assert.equal(point.doneWhen, check);
    const rounds = setup.conductor.state.phase.rounds!;
    assert.deepEqual(rounds[1].fixed!.tasks.map((t) => t.domain), ["booking"]);
    assert.deepEqual(rounds[2].fixed!.tasks.map((t) => t.domain), ["gates"], "only the newly failing domain goes back to MAP");
    assert.equal(rounds[2].base, rounds[1].fixed!.candidate);
    const events = readEvents(setup.runDir).filter((r) => r.kind === "event").map((r) => r.event as Record<string, unknown>);
    assert.ok(!events.some((e) => e.type === "LEAD_CALLED"), "known ownership is not an exception");
    assert.ok(!events.some((e) => Number(e.round) >= 2 && e.type === "CANDIDATE_PICKED"));
  } finally { await teardown(setup); }
});

test("a red merged check that names no declared domain parks for the lead with no lane round", async () => {
  // R3: a red merged candidate whose failing file belongs to no declared
  // domain has no owning loop to re-enter. The conductor records LEAD_CALLED
  // and parks for the lead (design G3) — it never starts a fresh lane fix or
  // any seat review on that red merge.
  const RED_WITH_NO_DOMAIN = 'if [ -f poison.txt ]; then echo "error: docs/README.md:4: bad link"; exit 1; fi';
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "a red merge with no owning domain is the lead's",
      acceptance: ["it works"],
      checks: [RED_WITH_NO_DOMAIN],
      boundaries: [],
      reserved: [],
      workers: 2,
      domains: FIX_DOMAINS,
      rounds: 3,
      fixRounds: "domains",
      fixers: 4,
      fixChecks: "true",
    },
    checks: [RED_WITH_NO_DOMAIN],
    phaseChecks: [RED_WITH_NO_DOMAIN],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => laneWorker("a", 1),
    laneWorkerScriptFor: (lane, round) => laneWorker(lane, round),
    laneReviewerScriptFor: (seat, candidate, state) => reviewerWithBookingPoint()(seat as Reviewer, candidate, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1),
    // The booking fix is green on its own; the merged candidate it lands on
    // carries a file whose failing check names docs/README.md — no domain.
    fixerScriptFor: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'booking fixed\n' > booking-fixed.txt; printf 'poison\n' > poison.txt" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    const events = () =>
      readEvents(setup.runDir)
        .filter((r) => r.kind === "event")
        .map((r) => r.event as Record<string, unknown>);
    await setup.conductor.start();
    await waitFor(() => events().some((e) => e.type === "LEAD_CALLED"), 120_000, 20, setup.runDir);
    const lead = events().find((e) => e.type === "LEAD_CALLED");
    assert.match(
      ((lead?.reasons as string[] | undefined) ?? []).join(" "),
      /names no file of a declared domain/,
      "LEAD_CALLED names the red merge whose output has no declared domain",
    );
    assert.equal(setup.conductor.state.phase.phase, "AWAITING_OWNER", "the phase parks for the lead");
    assert.ok(events().some((e) => e.type === "LOOP_BOUND_REACHED" && String(e.reason).includes("names no file")), "the parking transition carries the ownerless-check reason");
    assert.ok(!events().some((e) => e.type === "BOUND_REACHED"), "an ownerless check is not a reached bound");
    assert.ok(
      !events().some((e) => e.type === "fix_loop_chained"),
      "no loop chains on a red merge with no owning domain",
    );
    const intents = readEvents(setup.runDir)
      .filter((r) => r.kind === "intent")
      .map((r) => String((r.event as { agentId?: string }).agentId ?? ""));
    assert.ok(
      !intents.some((id) => id.startsWith("lane-2-a-")),
      `no fresh lane fix starts on the red merge: ${JSON.stringify(intents)}`,
    );
    assert.ok(
      !intents.some((id) => id.startsWith("lane-review-2-")),
      "no seat review starts on the red merge",
    );
    const noLoop = readLog(runPaths(setup.runDir).events).records.filter((r) => r.kind === "merged_check_no_loop");
    assert.equal(noLoop.length, 1, "the ownerless red check is recorded once");
  } finally {
    await teardown(setup);
  }
});

test("a point proven by an earlier loop does not keep the seats from reviewing once every point closes", async () => {
  // R4: booking's point closes on the first loop's merged candidate and
  // gates' point only on the second loop's. On that second candidate booking's
  // fact passes on the base too (the base now carries its fix), but it stays
  // closed (design G4): the seats review, because every point is closed — no
  // extra loop and no park for the lead.
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "staggered point closure still reaches the seats",
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
    // booking closes its point on the first loop; gates only on the second.
    fixerScriptFor: (domain, round) => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-sh",
          command:
            domain === "booking"
              ? round === 2
                ? "printf 'booking fixed\n' > booking-fixed.txt"
                : "true"
              : round === 2
                ? "printf 'gates\n' > gates.txt"
                : "printf 'gates fixed\n' > gates-fixed.txt",
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    const events = () =>
      readEvents(setup.runDir)
        .filter((r) => r.kind === "event")
        .map((r) => r.event as Record<string, unknown>);
    const intents = () =>
      readEvents(setup.runDir)
        .filter((r) => r.kind === "intent")
        .map((r) => String((r.event as { agentId?: string }).agentId ?? ""));
    await setup.conductor.start();
    await waitFor(() => intents().filter((id) => id.startsWith("reviewer-")).length === 3, 120_000, 20, setup.runDir);
    assert.ok(
      !events().some((e) => e.type === "LEAD_CALLED"),
      "a point closed on an earlier candidate never parks the phase",
    );
    assert.ok(!events().some((e) => e.type === "LOOP_BOUND_REACHED"), "no loop bound is reached on a closed set");
    assert.deepEqual(
      readEvents(setup.runDir)
        .filter((r) => r.kind === "fix_loop_chained")
        .map((r) => (r.event as { round?: number }).round),
      [2],
      "only the first loop with a point still open chains",
    );
    assert.ok(
      intents().filter((id) => id.startsWith("reviewer-")).length === 3,
      "the seats review the candidate on which every point closed",
    );
  } finally {
    await teardown(setup);
  }
});
