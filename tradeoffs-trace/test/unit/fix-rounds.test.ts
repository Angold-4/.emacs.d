// Plan 07c (design D4): the fix-round task rule, pure. `fixTasks` is the ONE
// place that decides which open findings become a fixer task: an open finding
// triaged `fix` with a declared domain, grouped by domain in declaration
// order, at most `cap` tasks. `tt lint` refuses `#+TT_FIX_ROUNDS` without
// `#+TT_DOMAINS`.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import { decisionFixPoint, fixTasks, FIXER_DUTY, otherTaskIndexLines, type DecisionFixPoint, type FixFinding } from "../../src/core/fix-rounds.ts";
import { fixRoundCandidate } from "../../src/core/lanes.ts";
import { lintPlan } from "../../src/core/plan-lint.ts";
import { reduce } from "../../src/core/reduce.ts";
import { validate } from "../../src/core/schema.ts";
import type { Domain } from "../../src/core/domains.ts";
import { disposition, evidenceForFinding, recordImpact, type Disposition } from "../../src/core/triage.ts";
import type { Finding } from "../../src/core/types.ts";
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
  assert.match(FIXER_DUTY, /fix exactly the findings below/i);
  assert.match(FIXER_DUTY, /change only what they need/i);
  assert.match(FIXER_DUTY, /decide no trade-off/i);
  assert.match(FIXER_DUTY, /disclosure/i);
});

test("the other-task index names the other tasks' finding ids and nothing else", () => {
  const tasks = fixTasks([finding("F-1", "booking"), finding("F-2", "gates"), finding("F-3", "matching")], DOMAINS, 3);
  const lines = otherTaskIndexLines(tasks, tasks[0]);
  assert.deepEqual(lines, ["- fix-gates: F-2 (domain gates)", "- fix-matching: F-3 (domain matching)"]);
  assert.ok(lines.every((l) => !l.includes("expected")), "the index never carries the other findings' fields");
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
  assert.deepEqual(fixRoundCandidate(applied.state.phase.rounds, 3), { candidate: "C", base: "B" });
  // Without a candidate the next round is today's; a fix step for a round
  // with no pick is refused.
  const none = reduce(state, { ...event, candidate: undefined } as never);
  assert.ok(none.ok);
  assert.equal(fixRoundCandidate(none.state.phase.rounds, 3), undefined);
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

test("fixRoundCandidate serves only the round right after the fixed round, never a later one", () => {
  const picked = (round: number, sha: string) => ({ round, base: "B0", lanes: ["a", "b"], candidates: [], picked: { lane: "a", sha, votes: 3 } });
  const fixedRound = { ...picked(2, "P2"), fixed: { base: "B", tasks: [], merges: [{ sha: "f1" }], candidate: "C" } };
  const rounds = [fixedRound, { round: 3, base: "C", lanes: ["a"], candidates: [] }] as never;
  // Round 3 (right after the fixed round 2) runs on the merged candidate.
  assert.deepEqual(fixRoundCandidate(rounds, 3), { candidate: "C", base: "B" });
  // Round 3 ran on C and picked nothing: round 4 is today's round, not a
  // replay of C (which would throw away round 3's lane-fix commit).
  assert.equal(fixRoundCandidate(rounds, 4), undefined, "round 4 is not a synthetic round on C");
  assert.equal(fixRoundCandidate(rounds, 5), undefined);
  // A resumed, unfinished round 3 still reads its fix candidate.
  assert.deepEqual(fixRoundCandidate([fixedRound] as never, 3), { candidate: "C", base: "B" });
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
// Fix packet Z: voted-down and vetoed decisions are fix points
// ---------------------------------------------------------------------------

const SEATS = ["M", "A", "B"];

/** A decision point as `decisionFixPoint` builds it, for `fixTasks` tests. */
function decisionPoint(id: string, domain: string | undefined, cls: DecisionFixPoint["class"] = "delegated"): DecisionFixPoint {
  return {
    source: "decision",
    id,
    class: cls,
    choice: `${id} choice`,
    outcome: "vetoed",
    ...(domain !== undefined ? { domain } : {}),
    reasons: [`M: ${id} is wrong`],
    alternative: `${id} alternative`,
    expected: `${id} expected`,
  };
}

test("fixTasks groups mixed finding and decision points by domain in declaration order", () => {
  const points = [
    decisionPoint("D-1", "gates"),
    finding("F-1", "matching"),
    finding("F-2", "gates"),
    decisionPoint("D-2", "booking"),
    decisionPoint("D-3", undefined),
    finding("F-3", "booking"),
  ];
  const tasks = fixTasks(points, DOMAINS, 4);
  assert.deepEqual(
    tasks.map((t) => ({ domain: t.domain, ids: t.findingIds, findings: t.findings.map((f) => f.id), decisions: t.decisions.map((d) => d.id) })),
    [
      { domain: "booking", ids: ["D-2", "F-3"], findings: ["F-3"], decisions: ["D-2"] },
      { domain: "gates", ids: ["D-1", "F-2"], findings: ["F-2"], decisions: ["D-1"] },
      { domain: "matching", ids: ["F-1"], findings: ["F-1"], decisions: [] },
    ],
    "domains in declaration order, points in input order within a domain; a domainless decision is in no task",
  );
  // The cap still counts tasks, decisions included.
  assert.deepEqual(fixTasks(points, DOMAINS, 1).map((t) => t.findingIds), [["D-2", "F-3"]]);
});

test("a reserved (owner) decision is never a fixer task", () => {
  // fixTasks refuses a reserved decision point even when handed one ...
  assert.deepEqual(fixTasks([decisionPoint("D-R", "booking", "reserved")], DOMAINS, 4), []);
  // ... and decisionFixPoint never builds one: the owner rules on it.
  const reserved = makeDecision({ id: "D-R", class: "reserved" });
  const facts = {
    ballots: [makeBallot({ decisionId: "D-R", reviewer: "M", vote: "reject", evidence: ["booking/a.ts:3"] })],
    seats: SEATS,
    leader: "M",
    settled: false,
    domains: DOMAINS,
  };
  assert.equal(decisionFixPoint(reserved, facts), undefined);
  // A delegated decision with the same ballots is a point.
  assert.ok(decisionFixPoint(makeDecision({ id: "D-R" }), facts));
});

test("decisionFixPoint: a vetoed or failed delegated decision carries the dissent reasons, the alternative and a domain", () => {
  const decision = makeDecision({
    id: "D1",
    choice: "skip the overlay gate",
    alternatives: [{ option: "keep the overlay gate", consequence: "one more check per batch" }],
  });
  const veto = makeBallot({ reviewer: "M", vote: "reject", rationale: "VETO: the gate is required", evidence: ["the plan", "gates/g.ts:10 check"] });
  const approve = makeBallot({ reviewer: "A", vote: "approve", evidence: ["booking/b.ts:1"] });
  const point = decisionFixPoint(decision, { ballots: [approve, veto], seats: SEATS, leader: "M", settled: false, domains: DOMAINS });
  assert.ok(point);
  assert.equal(point!.outcome, "vetoed");
  assert.equal(point!.domain, "gates", "the first dissent evidence path a domain owns names the domain");
  assert.equal(point!.where, "gates/g.ts:10 check");
  assert.deepEqual(point!.reasons, ["M: VETO: the gate is required (evidence: the plan, gates/g.ts:10 check)"]);
  assert.match(point!.alternative, /keep the overlay gate/);
  assert.match(point!.expected, /VETO: the gate is required/);
  assert.match(point!.expected, /keep the overlay gate/);
  // The item's :DOMAIN: wins over the evidence path.
  const itemPoint = decisionFixPoint(makeDecision({ itemId: "R1" }), { ballots: [veto], seats: SEATS, leader: "M", settled: false, itemDomain: "booking", domains: DOMAINS });
  assert.equal(itemPoint!.domain, "booking");
  assert.equal(itemPoint!.item, "R1");
  // The evaluator's alternative, when it gave one, is the one the fixer applies.
  const evalPoint = decisionFixPoint(decision, { ballots: [veto], seats: SEATS, leader: "M", settled: false, domains: DOMAINS, evaluatorAlternative: "EVAL_ALT" });
  assert.equal(evalPoint!.alternative, "EVAL_ALT");
  // M approved, A and B rejected: a failed vote, not a veto.
  const failed = decisionFixPoint(decision, {
    ballots: [
      makeBallot({ reviewer: "M", vote: "approve" }),
      makeBallot({ reviewer: "A", vote: "reject", rationale: "A dissents", evidence: ["matching/m.ts:2"] }),
      makeBallot({ reviewer: "B", vote: "reject", rationale: "B dissents", evidence: ["booking/b.ts:2"] }),
    ],
    seats: SEATS,
    leader: "M",
    settled: false,
    domains: DOMAINS,
  });
  assert.equal(failed!.outcome, "failed");
  assert.equal(failed!.domain, "matching", "the first dissenting seat's path, in seat order");
  // Without a domain-owned path it is returned domainless and no task takes it.
  const noDomain = decisionFixPoint(decision, {
    ballots: [makeBallot({ reviewer: "M", vote: "reject", evidence: ["the plan says so"] })],
    seats: SEATS,
    leader: "M",
    settled: false,
    domains: DOMAINS,
  });
  assert.equal(noDomain!.domain, undefined);
  assert.deepEqual(fixTasks([noDomain!], DOMAINS, 4), []);
  // A settled, unballoted, detail or superseded decision is no point; a
  // ballot bound to another candidate does not count.
  assert.equal(decisionFixPoint(decision, { ballots: [veto], seats: SEATS, leader: "M", settled: true, domains: DOMAINS }), undefined);
  assert.equal(decisionFixPoint(decision, { ballots: [], seats: SEATS, leader: "M", settled: false, domains: DOMAINS }), undefined);
  assert.equal(decisionFixPoint(makeDecision({ class: "detail" }), { ballots: [veto], seats: SEATS, leader: "M", settled: false, domains: DOMAINS }), undefined);
  assert.equal(decisionFixPoint(makeDecision({ supersededBy: "x" }), { ballots: [veto], seats: SEATS, leader: "M", settled: false, domains: DOMAINS }), undefined);
  assert.equal(
    decisionFixPoint(decision, { ballots: [{ ...veto, boundCandidateSha: "OLD" }], seats: SEATS, leader: "M", settled: false, domains: DOMAINS }),
    undefined,
  );
});

test("the fixer duty tells a fixer to apply the seats' alternative and never to take an owner decision", () => {
  assert.match(FIXER_DUTY, /apply the alternative the seats chose/i);
  assert.match(FIXER_DUTY, /remove the vetoed change/i);
  assert.match(FIXER_DUTY, /reserved\) decision is never your task/i);
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
