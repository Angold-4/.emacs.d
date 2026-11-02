// Plan 07f (F0/A2): a unanimous item whose seats cite resolving anchors is
// decided by code. Thin evidence is not an exception; the evaluator is not
// started merely because one file:line was cited.

import assert from "node:assert/strict";
import test from "node:test";
import { accept } from "../../src/core/predicate.ts";
import { evaluationExceptions } from "../../src/core/verdict.ts";
import { CV, basePhase } from "./helpers.ts";

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

function review(seat: string, evidence: string) {
  return {
    reviewer: seat,
    phaseId: "p1",
    candidateSha: C,
    contractVersion: K,
    correctionStatements: [],
    findingStatements: [],
    items: [{ id: "R1", verdict: "met", evidence }],
    arch: [],
  };
}

function phaseWith(evidence: string) {
  return basePhase({
    contract: CONTRACT,
    phase: "RESOLVING",
    candidate: { sha: C, contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: C, passed: true },
    probe: { candidateSha: C, head: "H0", probedI: "I1", passed: true },
    checkResolution: [{ id: "R1", name: "R1 works", outcome: "passed" }],
    reviews: {
      M: { review: review("M", evidence) },
      A: { review: review("A", evidence) },
      B: { review: review("B", evidence) },
    },
  });
}

test("a unanimous item with resolving anchors is not an evaluator exception merely for thin evidence", () => {
  const phase = phaseWith("README.md:1");
  assert.deepEqual(evaluationExceptions(phase), [], "no exception: the seats agree and the anchor resolves");
  assert.equal(accept(phase, C, K), true, "the mechanical verdict accepts it");
});

test("a majority that cites only a test name uses the same anchor check review-lint applies", () => {
  const phase = phaseWith('test "R1 works"');
  assert.equal(accept(phase, C, K), true, "a test name is a resolving anchor, not a missing file:line");
});

test("a file:line that does not exist in the candidate does not resolve", () => {
  const phase = phaseWith("does/not/exist.ts:1");
  assert.equal(accept(phase, C, K), false, "a token that names no candidate file is not an anchor");
});
