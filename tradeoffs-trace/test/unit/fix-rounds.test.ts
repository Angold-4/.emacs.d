// Plan 07c (design D4): the fix-round task rule, pure. `fixTasks` is the ONE
// place that decides which open findings become a fixer task: an open finding
// triaged `fix` with a declared domain, grouped by domain in declaration
// order, at most `cap` tasks. `tt lint` refuses `#+TT_FIX_ROUNDS` without
// `#+TT_DOMAINS`.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import { domainReviewBrief, factKeepsOpen, fixTasks, testRemovalViolations, FIXER_DUTY, type FixFinding, type FixTask } from "../../src/core/fix-rounds.ts";
import { domainFactFailure } from "../../src/fix-loop.ts";
import { lintPlan, lintTestRemovals } from "../../src/core/plan-lint.ts";
import { reduce } from "../../src/core/reduce.ts";
import { validate } from "../../src/core/schema.ts";
import type { Domain } from "../../src/core/domains.ts";
import { disposition, evidenceForFinding, recordImpact, type Disposition } from "../../src/core/triage.ts";
import type { DomainLoopRecord, Finding } from "../../src/core/types.ts";
import { basePhase, baseState, makeBallot, makeDecision } from "./helpers.ts";

const DOMAINS: Domain[] = [
  { name: "booking", globs: ["booking/**"] },
  { name: "gates", globs: ["gates/**"] },
  { name: "matching", globs: ["matching/**"] },
];

const FIX: Disposition = { kind: "fix", reason: "a wrong output must be fixed" };

/** One open finding with a domain and a triage disposition. */
function finding(id: string, domain: string | undefined, disposition: Disposition | null = FIX, status: Finding["status"] = "open"): FixFinding {
  return {
    id,
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: `${id} is wrong`,
    raisedBy: "M",
    status,
    boundCandidateSha: "C1",
    ...(domain !== undefined ? { domain } : {}),
    item: "R1",
    where: `${domain ?? "x"}/a.ts:1 fn`,
    expected: `${id} expected`,
    doneWhen: `test ${id}`,
    ...(disposition ? { disposition } : {}),
  };
}

test("fixTasks groups open fix findings by domain in declaration order and caps them", () => {
  const findings = [
    finding("F-1", "booking"),
    finding("F-2", "booking"),
    finding("F-3", "gates"),
    finding("F-4", "matching"),
    finding("F-5", "matching"),
  ];
  const tasks = fixTasks(findings, DOMAINS, 2);
  assert.deepEqual(
    tasks.map((t) => ({ domain: t.domain, findingIds: t.findingIds })),
    [
      { domain: "booking", findingIds: ["F-1", "F-2"] },
      { domain: "gates", findingIds: ["F-3"] },
    ],
    "the first two declared domains that have findings, findings grouped by domain",
  );
  // The cap is the number of TASKS, not findings; a domain with no fix
  // finding is skipped, never a task.
  assert.deepEqual(fixTasks(findings, DOMAINS, 4).map((t) => t.domain), ["booking", "gates", "matching"]);
  assert.deepEqual(fixTasks(findings, DOMAINS, 1).map((t) => t.domain), ["booking"]);
  assert.deepEqual(fixTasks(findings, DOMAINS, 0), []);
  assert.deepEqual(fixTasks(findings, [], 4), []);
});

test("fixTasks leaves out trade-off, escalated, closed and domainless findings", () => {
  const findings = [
    finding("F-1", "booking"),
    finding("F-2", "booking", { kind: "tradeoff", chosen: "keep it", alternative: "fix it", why: "a style call" }),
    finding("F-3", "gates", { kind: "escalate", reason: "needs the owner", ownerRequestId: "OR-1" }),
    finding("F-4", "gates", FIX, "repaired"),
    finding("F-5", undefined),
    finding("F-6", "gates", null),
  ];
  const tasks = fixTasks(findings, DOMAINS, 4);
  assert.deepEqual(
    tasks.map((t) => ({ domain: t.domain, findingIds: t.findingIds })),
    [{ domain: "booking", findingIds: ["F-1"] }],
  );
});

test("the fixer duty paragraph says the fixer decides no trade-off and reports anything else", () => {
  assert.match(FIXER_DUTY, /fix exactly the points below/i);
  assert.match(FIXER_DUTY, /change only what they need/i);
  assert.match(FIXER_DUTY, /decide no trade-off/i);
  assert.match(FIXER_DUTY, /disclosure/i);
});

test("the plan schema accepts the fix-round keywords on a phase", () => {
  const planSchema = JSON.parse(readFileSync(new URL("../../schemas/plan.schema.json", import.meta.url), "utf8"));
  const plan = {
    title: "t",
    checks: ["true"],
    fixRounds: "domains",
    fixers: 4,
    fixChecks: "cargo check",
    domains: DOMAINS,
    phases: [
      {
        id: "p1",
        goal: "g",
        checks: ["true"],
        boundaries: [],
        reserved: [],
        provisional: false,
        domains: DOMAINS,
        fixRounds: "domains",
        fixers: 4,
        fixChecks: "cargo check",
      },
    ],
  };
  const result = validate(planSchema, plan);
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("ROUND_FIXED validates and makes the merged candidate the next round's single lane", () => {
  const schema = JSON.parse(readFileSync(new URL("../../schemas/event.schema.json", import.meta.url), "utf8"));
  const event = {
    type: "ROUND_FIXED",
    round: 2,
    base: "B",
    tasks: [
      { domain: "booking", findingIds: ["F-1"], sha: "b1" },
      { domain: "gates", findingIds: ["F-2"], note: "its commit conflicted and was skipped" },
    ],
    merges: [{ sha: "b1" }],
    candidate: "C",
  };
  assert.ok(validate(schema, event).valid, "the event validates");
  let state = baseState({ phase: "IMPLEMENTING" });
  for (const e of [
    { type: "ROUND_STARTED", round: 2, base: "B0", lanes: ["a", "b"] },
    { type: "CANDIDATE_SUBMITTED", round: 2, lane: "a", sha: "C2a" },
    { type: "CANDIDATE_CHECKED", round: 2, lane: "a", ok: true },
    { type: "CANDIDATE_PICKED", round: 2, lane: "a", sha: "C2a", votes: 3 },
  ] as const) {
    const r = reduce(state, e as never);
    assert.ok(r.ok, `${e.type}: ${!r.ok ? r.reason : ""}`);
    state = r.state;
  }
  const applied = reduce(state, event as never);
  assert.ok(applied.ok, !applied.ok ? applied.reason : "");
  assert.deepEqual(applied.state.phase.rounds![0].fixed, {
    base: "B",
    tasks: [
      { domain: "booking", findingIds: ["F-1"], sha: "b1" },
      { domain: "gates", findingIds: ["F-2"], note: "its commit conflicted and was skipped" },
    ],
    merges: [{ sha: "b1" }],
    candidate: "C",
  });
  assert.equal(applied.state.phase.rounds![0].fixed?.candidate, "C");
  assert.equal(applied.state.phase.rounds![0].fixed?.base, "B");
  // Legacy replay keeps an absent integrated candidate absent. A fix step
  // cannot attach to an unstarted lane round.
  const none = reduce(state, { ...event, candidate: undefined } as never);
  assert.ok(none.ok);
  assert.equal(none.state.phase.rounds![0].fixed?.candidate, undefined);
  const noPick = reduce(baseState({ phase: "IMPLEMENTING" }), event as never);
  assert.equal(noPick.ok, false, "a round with no pick cannot be fixed");
});

test("tt lint refuses TT_FIX_ROUNDS without TT_DOMAINS", () => {
  const findings = lintPlan({ fixRounds: "domains", workers: 2, phases: [{ id: "p1", fixRounds: "domains" }] }).filter((f) => f.rule === "fix-rounds");
  assert.equal(findings.length, 1);
  assert.equal(findings[0].severity, "error");
  assert.match(findings[0].problem, /needs #\+TT_DOMAINS/);
  // With domains declared it passes; without the keyword it is not checked.
  assert.deepEqual(lintPlan({ fixRounds: "domains", workers: 2, domains: DOMAINS, phases: [] }).filter((f) => f.rule === "fix-rounds"), []);
  assert.deepEqual(lintPlan({ phases: [] }).filter((f) => f.rule === "fix-rounds"), []);
  // The only value is `domains`.
  const bad = lintPlan({ fixRounds: "all", domains: DOMAINS, phases: [] }).filter((f) => f.rule === "fix-rounds");
  assert.equal(bad.length, 1);
  assert.match(bad[0].problem, /must be domains/);
  // #+TT_FIXERS is a positive whole number.
  const badFixers = lintPlan({ fixers: 0, phases: [] }).filter((f) => f.rule === "fix-rounds");
  assert.equal(badFixers.length, 1);
  assert.match(badFixers[0].problem, /whole number of 1 or more/);
});

test("tt lint refuses TT_FIX_ROUNDS with fewer than two workers", () => {
  const rule = (plan: Record<string, unknown>) =>
    lintPlan(plan as never).filter((f) => f.rule === "fix-rounds" && /lane rounds/.test(f.problem));
  // No #+TT_WORKERS is one worker: fix rounds never run.
  const absent = rule({ fixRounds: "domains", domains: DOMAINS, phases: [{ id: "p1", fixRounds: "domains" }] });
  assert.equal(absent.length, 1, "TT_FIX_ROUNDS without TT_WORKERS is an error");
  assert.equal(absent[0].severity, "error");
  assert.match(absent[0].problem, /fix rounds run only in lane rounds; set #\+TT_WORKERS: 2/);
  assert.equal(rule({ fixRounds: "domains", workers: 1, domains: DOMAINS, phases: [] }).length, 1, "one worker is an error");
  assert.deepEqual(rule({ fixRounds: "domains", workers: 2, domains: DOMAINS, phases: [{ id: "p1", fixRounds: "domains" }] }), []);
  // A phase's own two workers satisfy it; without the keyword nothing is checked.
  assert.deepEqual(rule({ fixRounds: "domains", domains: DOMAINS, phases: [{ id: "p1", fixRounds: "domains", workers: 2 }] }), []);
  assert.deepEqual(rule({ workers: 1, phases: [] }), []);
});

// ---------------------------------------------------------------------------
// 07g1 (R5/A3): a decision is never a fixer task
// ---------------------------------------------------------------------------

/** A decision-shaped object a caller might wrongly hand to `fixTasks`.
 * 07g1 (R5/A3): it carries everything a naive task rule would read — open,
 * triaged `fix`, a domain and a citing file — so only the source check keeps
 * it out of fixer work. */
function decisionPoint(id: string, domain: string | undefined, cls: "delegated" | "reserved" = "delegated"): Record<string, unknown> {
  return {
    source: "decision",
    id,
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: `${id} was vetoed`,
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
    disposition: FIX,
    class: cls,
    choice: `${id} choice`,
    outcome: "vetoed",
    ...(domain !== undefined ? { domain } : {}),
    where: `${domain ?? "src/x.ts"}:1 fn`,
    reasons: [`M: ${id} is wrong`],
    alternative: `${id} alternative`,
    expected: `${id} expected`,
  };
}

test("fixTasks groups mixed finding and decision points by domain in declaration order", () => {
  // 07g1 (R5): only findings are points. A mixed list keeps the findings and
  // never turns a decision into a task, whatever domain it carries.
  const mixed = [
    decisionPoint("D-1", "gates"),
    finding("F-1", "matching"),
    finding("F-2", "gates"),
    decisionPoint("D-2", "booking"),
    decisionPoint("D-3", undefined),
    finding("F-3", "booking"),
  ] as unknown as FixFinding[];
  const tasks = fixTasks(mixed, DOMAINS, 4);
  assert.deepEqual(
    tasks.map((t) => ({ domain: t.domain, ids: t.findingIds, findings: t.findings.map((f) => f.id) })),
    [
      { domain: "booking", ids: ["F-3"], findings: ["F-3"] },
      { domain: "gates", ids: ["F-2"], findings: ["F-2"] },
      { domain: "matching", ids: ["F-1"], findings: ["F-1"] },
    ],
    "domains in declaration order, findings in input order within a domain; a decision is in no task",
  );
  assert.deepEqual(fixTasks(mixed, DOMAINS, 1).map((t) => t.findingIds), [["F-3"]]);
});

test("a reserved (owner) decision is never a fixer task", async () => {
  // 07g1 (R5): a vetoed/triaged decision goes to the seats' vote or the
  // owner; fixTasks refuses every decision shape, reserved included.
  assert.deepEqual(fixTasks([decisionPoint("D-R", "booking", "reserved")] as unknown as FixFinding[], DOMAINS, 4), []);
  assert.equal((await import("../../src/core/fix-rounds.ts")).decisionFixPoint, undefined);
});

test("decisionFixPoint: a vetoed or failed delegated decision carries the dissent reasons, the alternative and a domain", async () => {
  // 07g1 (R5/A3): decisionFixPoint is removed. A decision-shaped object with a
  // domain and a doneWhen is still not fixer work; the seats' vote or the
  // owner decides it, so no task is built from it.
  const vetoed = { ...decisionPoint("D1", "gates"), where: "gates/g.ts:10 check", doneWhen: "test gate" } as unknown as FixFinding;
  assert.deepEqual(fixTasks([vetoed], DOMAINS, 4), []);
  assert.equal((await import("../../src/core/fix-rounds.ts")).decisionFixPoint, undefined);
});

test("the fixer duty tells a fixer to apply the seats' alternative and never to take an owner decision", () => {
  assert.match(FIXER_DUTY, /voted-down or vetoed decision is never your task/i);
  assert.match(FIXER_DUTY, /goes to the seats or the owner/i);
  assert.match(FIXER_DUTY, /decide no trade-off/i);
});

test("an unclassified domain-mode point with a doneWhen is triaged fix (contract), not escalated", () => {
  const point: Finding = {
    id: "F-P",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "the booking path is wrong",
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
    domain: "booking",
    item: "R1",
    where: "booking/a.ts:1 fn",
    doneWhen: "test -f booking-fixed.txt",
  };
  const contract = { ...basePhase().contract, domains: DOMAINS };
  const decide = (phase: ReturnType<typeof basePhase>) =>
    disposition({ itemId: point.id, source: "finding", impact: recordImpact(phase, point.id) }, evidenceForFinding(point, phase));
  const phase = basePhase({ contract, findings: [point] });
  assert.equal(recordImpact(phase, point.id), "contract");
  assert.equal(decide(phase).kind, "fix", "the unclassified point is a fix");
  // Classified judgement by the evaluator: the ordinary path (a trade-off with all three fields).
  const judged = basePhase({
    contract,
    findings: [point],
    itemChecks: [{ itemId: point.id, verdict: "confirmed", evidence: "booking/a.ts:1", impact: "judgement", chosen: "keep", alternative: "change", why: "style" }],
  });
  assert.equal(decide(judged).kind, "tradeoff");
  // Outside domain mode, or without a doneWhen, it escalates as before.
  assert.equal(decide(basePhase({ findings: [point] })).kind, "escalate");
  const noDone = { ...point, doneWhen: undefined };
  assert.equal(
    disposition({ itemId: point.id, source: "finding", impact: recordImpact(basePhase({ contract, findings: [noDone] }), point.id) }, evidenceForFinding(noDone, basePhase({ contract, findings: [noDone] }))).kind,
    "escalate",
  );
});

// exex 01c1a (2026-10-27, run f4d01766): the evaluator dropped confirmed domain
// points as "already tracked by E-n" and the drop disproved them, so no fixer
// got them. A point whose doneWhen fact fails on the candidate is kept open.
test("factKeepsOpen keeps a domain point open while its doneWhen fact fails on the candidate", () => {
  const point = {
    id: "F-1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "booking/a.ts:1 wrong",
    raisedBy: "A",
    status: "open",
    boundCandidateSha: "c1",
    domain: "booking",
    item: "R1",
    where: "booking/a.ts:1 f",
    expected: "right",
    doneWhen: "test -f booking-fixed.txt",
  } as Finding;
  const fact = (exitCode: number | null, timedOut = false, candidateSha = "c1") => ({ findingId: "F-1", candidateSha, exitCode, timedOut, outputTail: "" });
  assert.equal(factKeepsOpen(point, [fact(1)], "c1"), true, "a failing fact keeps it open");
  assert.equal(factKeepsOpen(point, [fact(null, true)], "c1"), true, "a timed-out fact keeps it open");
  assert.equal(factKeepsOpen(point, [fact(0)], "c1"), false, "a passing fact lets a drop close it");
  assert.equal(factKeepsOpen(point, [fact(1, false, "c0")], "c1"), false, "a fact on another candidate says nothing");
  assert.equal(factKeepsOpen(point, [], "c1"), false, "no fact: the drop applies as before");
  assert.equal(factKeepsOpen({ ...point, doneWhen: undefined }, [fact(1)], "c1"), false, "not a domain point: as before");
  assert.equal(factKeepsOpen({ ...point, domain: undefined }, [fact(1)], "c1"), false, "no domain: as before");
  assert.equal(factKeepsOpen(point, [fact(1)], undefined), false, "no candidate: as before");
});

test("a resumed domain review gets the same brief", () => {
  // The brief is built from the recorded loop history, so a review taken
  // after a stop/resume sees exactly the text the same log produced live.
  const task: FixTask = { domain: "booking", findingIds: ["P"], findings: [finding("P", "booking")] };
  const history: DomainLoopRecord[] = [
    {
      iteration: 1,
      sha: "a".repeat(40),
      facts: { checkPassed: true, points: [{ id: "P", onBase: false, onHead: true }] },
      review: { points: [{ pointId: "P", fixed: false, why: "booking/a.ts:1 STILL_WRONG" }], quality: { ok: false, problems: ["QUALITY_GAP booking/a.ts:9"] } },
      note: "domain review sent work back",
    },
    {
      iteration: 2,
      sha: "b".repeat(40),
      facts: { checkPassed: true, points: [{ id: "P", onBase: false, onHead: true }] },
      note: "domain review could not be taken",
    },
  ];
  const live = domainReviewBrief(task, history);
  const fromLog = domainReviewBrief(task, JSON.parse(JSON.stringify(history)) as DomainLoopRecord[]);
  assert.equal(fromLog, live, "the same history produces the same brief after a resume");
  assert.match(fromLog, /pass 1, commit aaaaaaaaa/);
  assert.match(fromLog, /review: P NOT fixed: booking\/a\.ts:1 STILL_WRONG/);
  assert.match(fromLog, /quality: QUALITY_GAP booking\/a\.ts:9/);
});

test("a vetoed decision makes no fixer task and an unmet item citing src/x.ts becomes a task of x's domain", () => {
  const xs: Domain[] = [{ name: "x", globs: ["src/x.ts"] }];
  const vetoed = decisionPoint("D-vetoed", "x");
  const discovered = { ...decisionPoint("D-discovered", "x"), outcome: "failed" };
  const r2: FixFinding = {
    ...finding("R2", undefined),
    item: "R2",
    where: "src/x.ts:10 fn",
    expected: "right",
    doneWhen: "true",
  };
  const tasks = fixTasks([vetoed, discovered, r2] as unknown as FixFinding[], xs, 4);
  assert.deepEqual(tasks.map((t) => ({ domain: t.domain, ids: t.findingIds })), [{ domain: "x", ids: ["R2"] }]);
  assert.equal(fixTasks([vetoed, discovered] as unknown as FixFinding[], xs, 4).length, 0, "a vetoed or discovered decision is never fixer work");
});

test("a candidate that deletes a test beyond the plan's list fails its round checks", () => {
  // exex 01c3a lane b adcc1b2 (2026-10-28): green by deleting T's and C's
  // test files. Both a deleted file and a removed #[test]/test( function are
  // red, and the check record's violation list names them.
  const violations = testRemovalViolations(
    [
      { path: "a.test.ts", base: "test('A', () => {});\ntest('B', () => {});\n", candidate: "test('A', () => {});\n" },
      { path: "b.test.ts", base: "test('C', () => {});\n", candidate: undefined },
      { path: "c.rs", base: "#[test]\nfn keeps_me() {}\n#[test]\nfn removed_me() {}\n", candidate: "#[test]\nfn keeps_me() {}\n" },
    ],
    [],
  );
  assert.deepEqual(violations, ["a.test.ts::B", "b.test.ts", "c.rs::removed_me"]);
  const failure = domainFactFailure({ checkPassed: true, points: [], testRemovals: violations });
  assert.ok(failure, "a test-removal violation makes the round's facts red");
  for (const removed of violations) assert.match(failure, new RegExp(removed.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")), `the check record names ${removed}`);
  assert.deepEqual(
    testRemovalViolations(
      [
        { path: "a.test.ts", base: "test('A', () => {});\ntest('B', () => {});\n", candidate: "test('A', () => {});\n" },
        { path: "b.test.ts", base: "test('C', () => {});\n", candidate: undefined },
        { path: "c.rs", base: "#[test]\nfn keeps_me() {}\n#[test]\nfn removed_me() {}\n", candidate: "#[test]\nfn keeps_me() {}\n" },
      ],
      ["a.test.ts::B", "b.test.ts::C", "c.rs::removed_me"],
    ),
    [],
    "the plan's allowance names exactly the removed tests",
  );
  // The allowance itself is linted against the base: an entry that names no
  // test there is refused, never silently ignored.
  const base = { namesIn: (file: string) => (file === "a.test.ts" ? ["A", "B"] : file === "b.test.ts" ? ["C"] : undefined) };
  const ok = { phases: [], testRemovals: ["a.test.ts::B", "b.test.ts::C"], repo: "/tmp/x" };
  assert.deepEqual(lintTestRemovals(ok, base), []);
  const bad = lintTestRemovals({ ...ok, testRemovals: ["a.test.ts::GONE", "b.test.ts::C"] }, base);
  assert.equal(bad.length, 1, "an entry naming no test on the base is an error");
  assert.equal(bad[0].rule, "test-removals");
  assert.match(bad[0].problem, /a\.test\.ts has no test named GONE/);
});

test("a candidate that deletes a test beyond the plan's list is red, with the deleted names", () => {
  const base = "test('A', () => {});\ntest('B', () => {});\n";
  assert.deepEqual(
    testRemovalViolations(
      [{ path: "t.test.ts", base, candidate: "test('A', () => {});\n" }],
      [],
    ),
    ["t.test.ts::B"],
  );
  assert.deepEqual(
    testRemovalViolations(
      [
        { path: "t.test.ts", base, candidate: "test('A', () => {});\n" },
        { path: "gone.test.ts", base: "test('C', () => {});\n", candidate: undefined },
      ],
      ["t.test.ts::B", "gone.test.ts::C"],
    ),
    [],
    "#+TT_TEST_REMOVALS names the deleted function and the deleted file's test",
  );
});
