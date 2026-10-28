// Plan 07f (A3, §3.2/§3.3/C2): an owner correction that arrives mid-state in a
// domain-mode phase is a point in the domain the input names, with the proving
// facts a point needs (domain, where, expected, doneWhen) — or it is refused
// for clarification. Nothing is inferred from prose, and no unprovable point
// is admitted (X:UNPROVABLE). The repair a passing final check starts is
// bounded like every other one (C2).

import assert from "node:assert/strict";
import { test } from "node:test";

import { ownerPointForCorrection, parseOwnerPointFields } from "../../src/conductor.ts";
import { acceptanceBlockers } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import { next } from "../../src/core/next.ts";
import type { PhaseState, State } from "../../src/core/types.ts";
import { baseState, CV } from "../unit/helpers.ts";

const K = CV();
const C1 = { sha: "C1", contractVersion: K };

const DOMAINS = [
  { name: "conductor", paths: ["tradeoffs-trace/src/conductor.ts"] },
  { name: "core", paths: ["tradeoffs-trace/src/core/**"] },
];

function domainPhase(overrides: Partial<PhaseState> = {}): PhaseState {
  return baseState({
    phase: "REVIEWING",
    candidate: C1,
    integrationHead: "H0",
    contract: { ...baseState().phase.contract, domains: DOMAINS } as never,
    ...overrides,
  }).phase;
}

const STRUCTURED = [
  "the owner's point",
  "domain: conductor",
  "item: A3",
  "where: tradeoffs-trace/src/conductor.ts:4066",
  "expected: a correction mid-state becomes a point with a proving command",
  "doneWhen: node --test test/conductor/owner-point.test.ts",
].join("\n");

test("plan 07f (A3): a structured correction is a point with its domain, where, expected and doneWhen", () => {
  const decision = ownerPointForCorrection(domainPhase(), "cmd-1", STRUCTURED, ["A3", "C2"]);
  assert.equal(decision.kind, "point", JSON.stringify(decision));
  assert.ok(decision.kind === "point");
  const finding = (decision.event as Extract<typeof decision.event, { type: "FINDING_RAISED" }>).finding;
  assert.equal(finding.domain, "conductor", "the point belongs to the domain the input names");
  assert.equal(finding.where, "tradeoffs-trace/src/conductor.ts:4066");
  assert.equal(finding.doneWhen, "node --test test/conductor/owner-point.test.ts", "the point carries a proving command");
  assert.equal(finding.expected?.startsWith("a correction mid-state"), true);
  assert.equal(finding.item, "A3");
  assert.equal(finding.raisedBy, "owner");
  assert.equal(finding.severity, "blocking");
  assert.equal(finding.boundCandidateSha, "C1");

  // The record reduces, and the point is what acceptance asks to be closed by
  // a proving fact — it is accepted as a record, never as an unprovable one.
  const applied = reduce(baseState(domainPhase()), decision.event);
  assert.ok(applied.ok, applied.ok ? "" : applied.reason);
  const point = acceptanceBlockers(applied.state.phase).find((b) => b.kind === "point");
  assert.ok(point, "the open point is an acceptance blocker until a fact closes it");
  assert.match(point!.detail, /conductor/, "the blocker names the domain");
});

test("plan 07f (A3/D-5/D-6): a correction that does not name its own fields is refused, not admitted or noted", () => {
  const bare = "please make the retry window 30s in tradeoffs-trace/src/conductor.ts";
  const decision = ownerPointForCorrection(domainPhase(), "cmd-2", bare, ["A3"]);
  assert.equal(decision.kind, "refused");
  assert.ok(decision.kind === "refused");
  for (const field of ["domain", "where", "expected", "doneWhen"]) {
    assert.match(decision.reason, new RegExp(field), `the refusal names ${field}: ${decision.reason}`);
  }

  // A path in the prose routes nothing, even when the phase declares exactly
  // one domain: the input has to say which domain it is for.
  const single = domainPhase({ contract: { ...baseState().phase.contract, domains: [DOMAINS[0]] } as never });
  assert.equal(ownerPointForCorrection(single, "cmd-3", bare, ["A3"]).kind, "refused", "one declared domain is not routing");

  // A domain or item that the plan does not declare is refused with the ones
  // it does declare, so the owner can resend.
  const wrongDomain = ownerPointForCorrection(domainPhase(), "cmd-4", STRUCTURED.replace("domain: conductor", "domain: cli"), ["A3"]);
  assert.equal(wrongDomain.kind, "refused");
  assert.ok(wrongDomain.kind === "refused" && wrongDomain.reason.includes("conductor, core"));
  const wrongItem = ownerPointForCorrection(domainPhase(), "cmd-5", STRUCTURED.replace("item: A3", "item: Z9"), ["A3"]);
  assert.equal(wrongItem.kind, "refused");
  assert.ok(wrongItem.kind === "refused" && wrongItem.reason.includes("Z9"));
});

test("plan 07f (A3): outside domain mode a correction rides the note channel", () => {
  const noDomains = baseState({ phase: "REVIEWING", candidate: C1 }).phase;
  assert.equal(ownerPointForCorrection(noDomains, "cmd-6", STRUCTURED, ["A3"]).kind, "note");
  const noCandidate = domainPhase({ candidate: undefined });
  assert.equal(ownerPointForCorrection(noCandidate, "cmd-7", STRUCTURED, ["A3"]).kind, "note");
});

test("plan 07f (A3): the field parser reads only explicit fields, in any Key-Case", () => {
  const parsed = parseOwnerPointFields(
    ["Done when:  `node --test test/x.test.ts`", "DOMAIN: core", "where: src/core/x.ts:1", "expected: x", "note: prose is not a field"].join("\n"),
  );
  assert.deepEqual(parsed.missing, []);
  assert.equal(parsed.fields.doneWhen, "node --test test/x.test.ts");
  assert.equal(parsed.fields.domain, "core");
  assert.equal(parseOwnerPointFields("R7: keep the fix scoped").missing.length, 4, "prose with a colon names no field");
});

test("plan 07f (C2): a passing final check with blockers left is bounded by the repair budget", () => {
  const contract = { ...baseState().phase.contract, finalChecks: ["make check-final"] };
  const base = {
    phase: "FINAL_CHECKING" as const,
    candidate: C1,
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: false },
    contract,
    inFlight: { run_final_checks: { actionId: "a1" } },
  };

  const withBudget = reduce(baseState({ ...base, repairRoundsUsed: 0, repairRoundsGranted: 1 }), {
    type: "FINAL_CHECKS_PASSED",
    candidateSha: "C1",
  });
  assert.ok(withBudget.ok, withBudget.ok ? "" : withBudget.reason);
  assert.equal(withBudget.state.phase.phase, "REPAIRING", "an unmet blocker repairs while the budget lasts");

  const spent = reduce(baseState({ ...base, repairRoundsUsed: 3, repairRoundsGranted: 3 }), {
    type: "FINAL_CHECKS_PASSED",
    candidateSha: "C1",
  });
  assert.ok(spent.ok, spent.ok ? "" : spent.reason);
  assert.equal(spent.state.phase.phase, "AWAITING_OWNER", "a spent budget parks on the owner (X:BOUND), never an unbounded loop");
  assert.deepEqual(next(spent.state), [], "nothing else runs from the parked phase");
});

export type { State };
