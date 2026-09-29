// Plan 05e (5): the plan-based severity rule. A blocking finding is legitimate
// only when it is a defect against an acceptance item or a reserved rule, and
// a citation is recognised in the forms a reviewer actually writes — the item
// verbatim, a paraphrase that keeps its phrasing, a numbered reference, an
// owner directive id — not only an exact, full-text quote (round-2 review
// M-1/A-4).

import assert from "node:assert/strict";
import { test } from "node:test";

import { findingCitesAcceptanceOrReserved } from "../../src/core/predicate.ts";
import type { Finding } from "../../src/core/types.ts";

function finding(overrides: Partial<Finding>): Finding {
  return {
    id: "F-1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "",
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
    ...overrides,
  };
}

const ACCEPTANCE = [
  "the conductor re-runs every newly failing test alone before a check may fail",
  "the review buffer shows one topic once",
];

test("plan 05e: a citation is the item verbatim, a phrasing-preserving paraphrase, a number, a reserved rule or a directive id", () => {
  // Verbatim.
  assert.equal(findingCitesAcceptanceOrReserved(finding({ evidence: "it breaks 'the review buffer shows one topic once'" }), { acceptance: ACCEPTANCE, reserved: [] }), true);
  // A paraphrase that keeps a distinctive run of the item's words.
  assert.equal(findingCitesAcceptanceOrReserved(finding({ evidence: "every newly failing test alone is rerun before the gate may fail" }), { acceptance: ACCEPTANCE, reserved: [] }), true);
  // A numbered reference.
  assert.equal(findingCitesAcceptanceOrReserved(finding({ evidence: "acceptance item 2 does not hold" }), { acceptance: ACCEPTANCE, reserved: [] }), true);
  assert.equal(findingCitesAcceptanceOrReserved(finding({ evidence: "criterion 3 does not hold" }), { acceptance: ACCEPTANCE, reserved: [] }), false, "a number beyond the list cites nothing");
  // A reserved rule.
  assert.equal(findingCitesAcceptanceOrReserved(finding({ evidence: "violates the reserved rule: never edit the lockfile by hand" }), { acceptance: ACCEPTANCE, reserved: ["never edit the lockfile by hand"] }), true);
  // An owner directive id.
  assert.equal(findingCitesAcceptanceOrReserved(finding({ evidence: "the candidate violates directive OD-1" }), { acceptance: ACCEPTANCE, reserved: [] }, ["OD-1"]), true);
});

test("plan 05e: a preference or an uncited defect may not block", () => {
  assert.equal(findingCitesAcceptanceOrReserved(finding({ evidence: "the helper's name is misleading" }), { acceptance: ACCEPTANCE, reserved: [] }), false);
  assert.equal(findingCitesAcceptanceOrReserved(finding({ evidence: "it breaks 'the review buffer shows one topic once'" }), { acceptance: ACCEPTANCE, reserved: [] }), true);
  // Plan (5) ties blocking to the ground cited, not the kind label: an
  // integration finding against an acceptance item may block (disc-M-51).
  assert.equal(findingCitesAcceptanceOrReserved(finding({ kind: "integration", evidence: "the review buffer shows one topic once" }), { acceptance: ACCEPTANCE, reserved: [] }), true);
  assert.equal(findingCitesAcceptanceOrReserved(finding({ kind: "integration", evidence: "the helper's name is misleading" }), { acceptance: ACCEPTANCE, reserved: [] }), false);
});
