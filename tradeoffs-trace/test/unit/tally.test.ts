// vote-table: design §5.1 — "a decision passes iff M approves and at least
// one of A, B approves". All eight M/A/B combinations, plus missing,
// malformed and evidence-free ballots each counting as reject, plus a
// superseded-candidate ballot discarded (which, since a missing ballot
// already counts as reject, has the same observable effect).

import assert from "node:assert/strict";
import { test } from "node:test";
import { tally } from "../../src/core/tally.ts";
import type { Ballot, Vote } from "../../src/core/types.ts";
import { CV, makeDecision } from "./helpers.ts";

const C = "C1";
const K = CV();

function ballot(reviewer: "M" | "A" | "B", vote: Vote, overrides: Partial<Ballot> = {}): Ballot {
  return {
    reviewer,
    decisionId: "D1",
    vote,
    rationale: "reasoned",
    evidence: ["src/cancel.ts:42"],
    boundCandidateSha: C,
    boundContractVersion: K,
    boundRecordVersion: 1, // matches makeDecision()'s default version
    ...overrides,
  };
}

const COMBINATIONS: { m: Vote; a: Vote; b: Vote; expected: "pass" | "fail" }[] = [
  { m: "approve", a: "approve", b: "approve", expected: "pass" },
  { m: "approve", a: "approve", b: "reject", expected: "pass" },
  { m: "approve", a: "reject", b: "approve", expected: "pass" },
  { m: "approve", a: "reject", b: "reject", expected: "fail" },
  { m: "reject", a: "approve", b: "approve", expected: "fail" },
  { m: "reject", a: "approve", b: "reject", expected: "fail" },
  { m: "reject", a: "reject", b: "approve", expected: "fail" },
  { m: "reject", a: "reject", b: "reject", expected: "fail" },
];

for (const { m, a, b, expected } of COMBINATIONS) {
  test(`vote-table: M=${m} A=${a} B=${b} -> ${expected}`, () => {
    const decision = makeDecision();
    const ballots = [ballot("M", m), ballot("A", a), ballot("B", b)];
    assert.equal(tally(decision, ballots, [], C, K), expected);
  });
}

test("vote-table: a missing ballot counts as reject", () => {
  const decision = makeDecision();
  // M approves, A approves; B never votes at all.
  const ballots = [ballot("M", "approve"), ballot("A", "reject")];
  assert.equal(tally(decision, ballots, [], C, K), "fail");
});

test("vote-table: a malformed ballot (no rationale) counts as reject", () => {
  const decision = makeDecision();
  const malformed = ballot("B", "approve", { rationale: "" });
  const ballots = [ballot("M", "approve"), ballot("A", "reject"), malformed];
  assert.equal(tally(decision, ballots, [], C, K), "fail");
});

test("vote-table: an evidence-free ballot counts as reject", () => {
  const decision = makeDecision();
  const evidenceFree = ballot("B", "approve", { evidence: [] });
  const ballots = [ballot("M", "approve"), ballot("A", "reject"), evidenceFree];
  assert.equal(tally(decision, ballots, [], C, K), "fail");
});

test("vote-table: a ballot bound to a superseded candidate is discarded, not counted", () => {
  const decision = makeDecision();
  const superseded = ballot("B", "approve", { boundCandidateSha: "C0-superseded" });
  const ballots = [ballot("M", "approve"), ballot("A", "reject"), superseded];
  // B's only ballot is bound to the superseded candidate, so it is
  // discarded exactly as if B had not voted — which already counts as
  // reject (tally.ts), giving the same (approve, reject, reject) => fail.
  assert.equal(tally(decision, ballots, [], C, K), "fail");
});

test("vote-table: a ballot bound to a superseded contract version is discarded, not counted", () => {
  const decision = makeDecision();
  const staleK = CV(0);
  const superseded = ballot("B", "approve", { boundContractVersion: staleK });
  const ballots = [ballot("M", "approve"), ballot("A", "reject"), superseded];
  assert.equal(tally(decision, ballots, [], C, K), "fail");
});

test("vote-table: a ballot bound to a superseded decision (record) version is discarded, not counted", () => {
  // design §7.1: a ballot cast on decision D v2 does not count once D is v3.
  const decision = makeDecision({ version: 3 });
  const superseded = ballot("B", "approve", { boundRecordVersion: 2 });
  const ballots = [ballot("M", "approve", { boundRecordVersion: 3 }), ballot("A", "reject", { boundRecordVersion: 3 }), superseded];
  assert.equal(tally(decision, ballots, [], C, K), "fail");
});

test("vote-table: `delegated` and `reserved` decisions are voted; `detail` is not", () => {
  const detail = makeDecision({ class: "detail" });
  const reserved = makeDecision({ class: "reserved" });
  const ballots = [ballot("M", "approve"), ballot("A", "approve"), ballot("B", "approve")];
  assert.equal(tally(detail, ballots, [], C, K), "not_votable");
  // owner-optional: a reserved decision is voted like any other (and only
  // flagged for the owner), so the reviewers can settle it.
  assert.equal(tally(reserved, ballots, [], C, K), "pass");
  assert.equal(tally(reserved, [ballot("M", "reject"), ballot("A", "approve"), ballot("B", "approve")], [], C, K), "fail");
});

test("vote-table: a decision with an open linked contract finding is suspended", () => {
  const decision = makeDecision({ linkedFindingId: "F1" });
  const findings = [
    {
      id: "F1",
      version: 1,
      phaseId: "p1",
      kind: "contract" as const,
      severity: "blocking" as const,
      evidence: "cancel acknowledges before fills stop",
      raisedBy: "B" as const,
      linkedDecisionId: decision.id,
      status: "open" as const,
      boundCandidateSha: C,
    },
  ];
  const ballots = [ballot("M", "approve"), ballot("A", "approve"), ballot("B", "approve")];
  assert.equal(tally(decision, ballots, findings, C, K), "suspended");
});

test("vote-table: once the linked finding closes, the decision votes normally again", () => {
  const decision = makeDecision({ linkedFindingId: "F1" });
  const findings = [
    {
      id: "F1",
      version: 1,
      phaseId: "p1",
      kind: "contract" as const,
      severity: "blocking" as const,
      evidence: "cancel acknowledges before fills stop",
      raisedBy: "B" as const,
      linkedDecisionId: decision.id,
      status: "repaired" as const,
      boundCandidateSha: C,
    },
  ];
  const ballots = [ballot("M", "approve"), ballot("A", "approve"), ballot("B", "approve")];
  assert.equal(tally(decision, ballots, findings, C, K), "pass");
});
