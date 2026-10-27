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
  assert.match(decision.command, /override/, "the flagged reserved decision is cleared by an owner override");
  const owner = blockers.find((b) => b.kind === "owner-request")!;
  assert.match(owner.command, /resolve/, "the open owner request is cleared by resolving it");
  assert.match(blockers.find((b) => b.kind === "point")!.detail, /F-1, F-2/, "the two points are one per-domain blocker");
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
});
