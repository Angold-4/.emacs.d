// Plan 07f (F1): acceptance is mechanical by default. These two tests pin
// the single blocker list `accept()` reads and the one exception the
// evaluator may not override: a seat-majority item with resolving anchors is
// met, whatever the evaluator's `unchecked` entry says.

import assert from "node:assert/strict";
import test from "node:test";
import { accept, acceptanceBlockers } from "../../src/core/predicate.ts";
import { CV, basePhase, makeDecision } from "./helpers.ts";

const C = "C1";
const K = CV();

const CONTRACT = {
  ...basePhase().contract,
  acceptance: ["R1 works"],
  architecture: [],
  itemsSynthesized: false,
  requirements: [{ id: "R1", title: "R1", text: "R1 works", arch: [], verify: ['test "R1 works"'] }],
  constraints: [],
};

const REVIEW = (seat: string, verdict: string) => ({
  reviewer: seat,
  phaseId: "p1",
  candidateSha: C,
  contractVersion: K,
  correctionStatements: [],
  findingStatements: [],
  items: [{ id: "R1", verdict, evidence: "README.md:1 README.md:2" }],
  arch: [],
});

const POINT = (id: string) => ({
  id,
  version: 1,
  phaseId: "p1",
  kind: "defect" as const,
  severity: "advisory" as const,
  evidence: "README.md:1",
  raisedBy: "M" as const,
  status: "open" as const,
  boundCandidateSha: C,
  domain: "booking",
  doneWhen: "test -f done",
});

test("acceptanceBlockers lists failed checks missing test verifies open points flagged decisions and open owner requests each with its command", () => {
  const request = {
    id: "OR-1",
    version: 1,
    phaseId: "p1",
    reason: "the booking point needs your ruling",
    origin: "open_finding" as const,
    linkedFindingId: "F-1",
    boundCandidateSha: C,
    boundContractVersion: K,
    options: [
      { id: "accept_risk", label: "accept the risk" },
      { id: "repair", label: "repair it" },
    ],
    status: "open" as const,
  };
  const phase = basePhase({
    contract: CONTRACT,
    phase: "RESOLVING",
    candidate: { sha: C, contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: C, passed: false },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    checkResolution: [{ id: "R1", name: "R1 works", outcome: "missing" }],
    reviews: {
      M: { review: REVIEW("M", "met") },
      A: { review: REVIEW("A", "met") },
      B: { review: REVIEW("B", "met") },
    },
    decisions: [makeDecision({ id: "D-1", class: "reserved", boundCandidateSha: C, boundContractVersion: K })],
    findings: [POINT("F-1"), POINT("F-2")],
    doneWhenFacts: [
      { findingId: "F-1", candidateSha: C, exitCode: 1, timedOut: false, outputTail: "not done" },
      { findingId: "F-2", candidateSha: C, exitCode: 1, timedOut: false, outputTail: "not done" },
    ],
    ownerRequests: [request],
  });

  const blockers = acceptanceBlockers(phase);
  assert.equal(blockers.length, 5, `one blocker for each unmet condition, got ${JSON.stringify(blockers, null, 2)}`);
  assert.deepEqual(
    blockers.map((b) => b.kind),
    ["check", "test-verify", "point", "decision", "owner-request"],
  );
  const decision = blockers.find((b) => b.kind === "decision")!;
  assert.match(decision.command, /"type":"ruling"/, "the flagged reserved decision is cleared by an owner ruling");
  assert.match(decision.command, /tt input/, "the ruling is a `tt input` command");
  const owner = blockers.find((b) => b.kind === "owner-request")!;
  assert.match(owner.command, /resolve/, "the open owner request is cleared by resolving it");
  assert.match(blockers.find((b) => b.kind === "point")!.detail, /F-1, F-2/, "the two points are one per-domain blocker");
  assert.match(blockers.find((b) => b.kind === "point")!.command, /tt why r1/, "a point names `tt why`, not an owner command that cannot close it");
  assert.equal(accept(phase, C, K), false, "accept() is the blocker list being non-empty");

  const cleared = {
    ...phase,
    checks: { candidateSha: C, passed: true },
    checkResolution: [{ id: "R1", name: "R1 works", outcome: "passed" }],
    findings: [],
    doneWhenFacts: [],
    decisions: [],
    ownerRequests: [],
  };
  assert.deepEqual(acceptanceBlockers(cleared), []);
  assert.equal(accept(cleared, C, K), true, "with every condition met the same predicate accepts");
});

test("a red check blocker carries the failing check command", () => {
  const command = "make -C x check";
  const phase = basePhase({
    contract: { ...CONTRACT, checks: [command] },
    phase: "RESOLVING",
    candidate: { sha: C, contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: C, passed: false, tier: "round" },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    checkResolution: [{ id: "R1", name: "R1 works", outcome: "passed" }],
    reviews: {
      M: { review: REVIEW("M", "met") },
      A: { review: REVIEW("A", "met") },
      B: { review: REVIEW("B", "met") },
    },
  });

  for (const fixture of [phase, basePhase({
    contract: { ...basePhase().contract, checks: [command] },
    phase: "CHECKING",
    candidate: { sha: C, contractVersion: K },
    checks: { candidateSha: C, passed: false },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    integrationHead: "H0",
  })]) {
    // Plan 07f1b (A1): the caller that prints the blocker passes the commands
    // the candidate's check record names as failed.
    const blockers = acceptanceBlockers(fixture, [command]);
    const check = blockers.find((b) => b.kind === "check");
    assert.ok(check, `the failed check is a blocker: ${JSON.stringify(blockers)}`);
    assert.equal(check!.command, command, "the blocker names the failing check command tt ran, verbatim");
    assert.ok(blockers.every((b) => !b.command.includes("tt recheck")), "no blocker clears a red check by asking for a recheck");
  }

  // Plan 07f1b (A1): with several checks, the blocker names the command that
  // actually FAILED — never a command that passed, and never a synthesized
  // `a && b` expression the owner never ran. The failing set is the explicit
  // argument the caller read from the candidate's check record; no command is
  // carried in the log and the predicate installs no source of its own.
  const first = "make -C x check";
  const second = "make -C x check-e2e";
  const several = basePhase({
    contract: { ...CONTRACT, checks: [first, second] },
    phase: "CHECKING",
    candidate: { sha: C, contractVersion: K },
    checks: { candidateSha: C, passed: false },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    integrationHead: "H0",
  });
  const commands = acceptanceBlockers(several, [second]).filter((b) => b.kind === "check").map((b) => b.command);
  assert.deepEqual(commands, [second], "the red check names only the command that failed, not the one that passed");
  assert.ok(commands.every((c) => !c.includes("&&")), "no blocker carries a composite command");

  // A named-test verify problem is one blocker per problem, naming a single
  // command — never one blocker per declared check.
  const verifyProblem = basePhase({
    contract: { ...CONTRACT, checks: [first, second] },
    phase: "RESOLVING",
    candidate: { sha: C, contractVersion: K },
    checks: { candidateSha: C, passed: false },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    integrationHead: "H0",
    checkResolution: [{ id: "R1", name: "R1 works", outcome: "failed" }],
  });
  assert.deepEqual(
    acceptanceBlockers(verifyProblem, [second]).filter((b) => b.kind === "test-verify").map((b) => b.command),
    [second],
    "one test-verify blocker per problem, naming the failing command",
  );

  // Without a per-command result (an old log, a fixture, an interrupted run)
  // nothing knows which command failed: the blocker points at the run itself
  // rather than naming a declared command that may have passed.
  const legacy = basePhase({
    contract: { ...CONTRACT, checks: [first, second] },
    phase: "CHECKING",
    candidate: { sha: C, contractVersion: K },
    checks: { candidateSha: C, passed: false },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    integrationHead: "H0",
  });
  const declared = acceptanceBlockers(legacy).filter((b) => b.kind === "check").map((b) => b.command);
  assert.deepEqual(declared, [`tt status ${legacy.runId}`], "without a per-command result the blocker names the run to inspect");
  assert.ok(!declared.some((c) => c === first || c === second), "no blocker names a declared command that may have passed");
});

test("an item met by two of three seats with resolving anchors is met whatever the evaluator's item check", () => {
  const phase = basePhase({
    contract: CONTRACT,
    phase: "RESOLVING",
    candidate: { sha: C, contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: C, passed: true },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    checkResolution: [{ id: "R1", name: "R1 works", outcome: "passed" }],
    reviews: {
      M: { review: REVIEW("M", "met") },
      A: { review: REVIEW("A", "met") },
      B: { review: REVIEW("B", "unmet") },
    },
    // A split item is an evaluator exception; its `unchecked` entry is not a
    // seat verdict and must not become one.
    itemChecks: [{ itemId: "R1", verdict: "unchecked", evidence: "README.md:1" }],
  });

  assert.deepEqual(
    acceptanceBlockers(phase).filter((b) => b.kind === "item"),
    [],
    "R1's strict seat majority is met",
  );

  const unreadable = basePhase({
    ...phase,
    reviews: {
      M: { review: { ...REVIEW("M", "met"), items: [{ id: "R1", verdict: "met", evidence: "no-such-file.ts:3" }] } },
      A: { review: REVIEW("A", "met") },
      B: { review: REVIEW("B", "unmet") },
    },
  });
  assert.deepEqual(
    acceptanceBlockers(unreadable).filter((b) => b.kind === "item").map((b) => b.detail),
    ["R1 is met without a majority of resolving anchors"],
    // 07g2 (R6): two of three seats said met, so the seats did NOT split for
    // the evaluator — the dissenting seat is a follow-up note. R1 still blocks
    // because M's anchor does not resolve, so the resolving majority is A's
    // alone.
    "an anchor that does not resolve does not count, so M is not a second met seat",
  );
});
