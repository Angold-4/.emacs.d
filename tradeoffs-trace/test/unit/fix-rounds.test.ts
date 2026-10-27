// Plan 07c (design D4): the fix-round task rule, pure. `fixTasks` is the ONE
// place that decides which open findings become a fixer task: an open finding
// triaged `fix` with a declared domain, grouped by domain in declaration
// order, at most `cap` tasks. `tt lint` refuses `#+TT_FIX_ROUNDS` without
// `#+TT_DOMAINS`.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import { fixTasks, FIXER_DUTY, otherTaskIndexLines, type FixFinding } from "../../src/core/fix-rounds.ts";
import { fixRoundCandidate } from "../../src/core/lanes.ts";
import { lintPlan } from "../../src/core/plan-lint.ts";
import { reduce } from "../../src/core/reduce.ts";
import { validate } from "../../src/core/schema.ts";
import type { Domain } from "../../src/core/domains.ts";
import type { Disposition } from "../../src/core/triage.ts";
import type { Finding } from "../../src/core/types.ts";
import { baseState } from "./helpers.ts";

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
  const findings = lintPlan({ fixRounds: "domains", phases: [{ id: "p1", fixRounds: "domains" }] }).filter((f) => f.rule === "fix-rounds");
  assert.equal(findings.length, 1);
  assert.equal(findings[0].severity, "error");
  assert.match(findings[0].problem, /needs #\+TT_DOMAINS/);
  // With domains declared it passes; without the keyword it is not checked.
  assert.deepEqual(lintPlan({ fixRounds: "domains", domains: DOMAINS, phases: [] }).filter((f) => f.rule === "fix-rounds"), []);
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
