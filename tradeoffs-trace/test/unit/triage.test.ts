// Plan 06i: `disposition(record, evidence)` is pure and is the ONLY place
// fix / trade-off / escalate is decided. One rule separates fix from
// trade-off: anything that gives a wrong value, offer or output on a
// reachable path, or contradicts the current golden source or the plan, must
// be fixed; only judgement calls (style, hardening, ergonomics) may be
// trade-offs; a record that cannot be classified escalates.

import assert from "node:assert/strict";
import { test } from "node:test";

import {
  disposition,
  dispositionReason,
  evidenceForFinding,
  ledgerRecords,
  type TriageRecord,
} from "../../src/core/triage.ts";
import { reduce } from "../../src/core/reduce.ts";
import { checkItemCarried } from "../../src/core/owner-commands.ts";
import { baseState } from "./helpers.ts";
import type { Decision, Finding, PhaseState } from "../../src/core/types.ts";

test("plan 06i: disposition gives a judgement call a trade-off only when chosen, alternative and why are all present", () => {
  const record: TriageRecord = { itemId: "F-1", source: "finding", impact: "judgement" };
  const full = disposition(record, {
    chosen: "keep the current label",
    alternative: "rename it",
    why: "the label is a display nicety",
  });
  assert.deepEqual(full, {
    kind: "tradeoff",
    chosen: "keep the current label",
    alternative: "rename it",
    why: "the label is a display nicety",
  });

  const missing = disposition(record, { chosen: "keep", alternative: "rename" });
  assert.equal(missing.kind, "escalate", "a judgement record missing a field escalates");

  const wrong: TriageRecord = { itemId: "F-2", source: "finding", impact: "wrong-output" };
  const fix = disposition(wrong, {});
  assert.equal(fix.kind, "fix", "a wrong-output record is a fix");

  const unclassified: TriageRecord = { itemId: "F-3", source: "finding", impact: undefined as never };
  assert.equal(disposition(unclassified, {}).kind, "escalate", "an unclassified record escalates");
});

test("plan 06i: a wrong value always wins over a judgement trade-off, whatever the reviewer's label", () => {
  // The evaluator re-checked a wrong-output claim and confirmed it: impact is
  // wrong-output, whatever the reviewer called the finding.
  const record: TriageRecord = { itemId: "F-9", source: "finding", impact: "wrong-output" };
  const d = disposition(record, {
    chosen: "accept the advisory",
    alternative: "fix it",
    why: "it was filed at advisory severity",
  });
  assert.equal(d.kind, "fix");
  assert.match(dispositionReason(d), /wrong output/);
});

test("plan 06i: a panel drop is not by itself a trade-off; the finding needs real chosen/alternative/why", () => {
  // OD-20: the panel's severity vote supplies neither the evaluator's
  // classification nor the three trade-off fields. A panel-lowered finding
  // goes through the same judgement path: real three fields -> trade-off;
  // a missing one -> escalate. The panel's reason is context only.
  const finding: Finding = {
    id: "F-panel",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "advisory",
    severityChangedBy: "panel",
    severityReason: "panel did not keep it blocking",
    evidence: "src/a.ts:1 a style point",
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
  };
  const base = {
    phaseId: "p1",
    candidate: { sha: "C1" },
    contract: { contractVersion: { snapshot: 1, sectionSha256: "x" } },
    findings: [finding],
    decisions: [],
    ballots: [],
  };
  const missingWhy = {
    ...base,
    itemChecks: [{ itemId: "F-panel", verdict: "confirmed", impact: "judgement", evidence: "README.md:1", chosen: "accept it", alternative: "repair it" }],
  } as unknown as PhaseState;
  const d1 = disposition({ itemId: "F-panel", source: "finding", impact: "judgement" }, evidenceForFinding(finding, missingWhy));
  assert.equal(d1.kind, "escalate", "a panel drop with a missing why escalates, not a stock trade-off");

  const withWhy = {
    ...base,
    itemChecks: [{ itemId: "F-panel", verdict: "confirmed", impact: "judgement", evidence: "README.md:1", chosen: "accept it", alternative: "repair it", why: "it is a style point" }],
  } as unknown as PhaseState;
  const d2 = disposition({ itemId: "F-panel", source: "finding", impact: "judgement" }, evidenceForFinding(finding, withWhy));
  assert.deepEqual(d2, { kind: "tradeoff", chosen: "accept it", alternative: "repair it", why: "it is a style point" });
});

test("plan 06i: a discovered decision with no ballots escalates to the owner, never a silent trade-off", () => {
  const record: TriageRecord = { itemId: "D-1", source: "decision", impact: "judgement" };
  const d = disposition(record, { noBallots: true, chosen: "c", alternative: "a", why: "w", ownerRequestId: "OR-1" });
  assert.equal(d.kind, "escalate");
  assert.equal(d.kind === "escalate" ? d.ownerRequestId : "", "OR-1");
});

test("plan 06i: a judgement check missing its why escalates, never borrowing the evidence field", () => {
  // The adapter must not supply `why` from `evidence`: a trade-off needs all
  // three fields the evaluator gave, or the record escalates (A2/R4).
  const finding: Finding = {
    id: "F-1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "advisory",
    evidence: "src/a.ts:1 a style point",
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
  };
  const phase = {
    phaseId: "p1",
    candidate: { sha: "C1" },
    contract: { contractVersion: { snapshot: 1, sectionSha256: "x" } },
    findings: [finding],
    decisions: [],
    ballots: [],
    itemChecks: [
      { itemId: "F-1", verdict: "confirmed", impact: "judgement", evidence: "README.md:1 the evaluator re-checked", chosen: "accept it", alternative: "repair it" },
    ],
  } as unknown as PhaseState;
  const d = disposition({ itemId: "F-1", source: "finding", impact: "judgement" }, evidenceForFinding(finding, phase));
  assert.equal(d.kind, "escalate", "a missing why must escalate");
});

test("plan 06i: a confirmed wrong output stays a fix even when the round panel dropped it", () => {
  // Wrong-output -> fix must come BEFORE the panelDropped trade-off, so a
  // panel vote can never downgrade a confirmed wrong value (A2/OD-12).
  const d = disposition(
    { itemId: "F-1", source: "finding", impact: "wrong-output" },
    { wrongOutputConfirmed: true, panelDropped: true, panelReason: "the round panel did not keep it blocking" },
  );
  assert.equal(d.kind, "fix", "a panel drop must not downgrade a confirmed wrong value");
});

test("plan 06i: an orphan duplicate escalates through disposition(), never a hand-built disposition", () => {
  const d = disposition(
    { itemId: "F-dup", source: "finding", impact: "judgement" },
    { orphanDuplicate: true, ownerRequestId: "OR-1", reason: "the original has no disposition" },
  );
  assert.equal(d.kind, "escalate");
  assert.equal(d.kind === "escalate" ? d.ownerRequestId : "", "OR-1");
});

test("plan 06i: a triage failure parks the phase on the owner, never a pass", () => {
  const result = reduce(baseState({ phase: "EVALUATING" }), { type: "TRIAGE_FAILED", reason: "triage blew up" });
  assert.ok(result.ok, result.ok ? "" : result.reason);
  assert.equal(result.state.phase.phase, "AWAITING_OWNER", "a triage failure must stop acceptance");
  assert.ok(
    result.state.phase.ownerRequests.some((r) => r.status === "open" && r.reason.includes("triage blew up")),
    "the owner is asked, with the reason",
  );
});

test("plan 06i: a carry and a defer on the same id are refused, each naming the existing act", () => {
  // OD addendum A: a second owner act on the same id is refused; neither is
  // silently dropped.
  const finding: Finding = { id: "F-1", version: 1, phaseId: "p1", kind: "defect", severity: "blocking", evidence: "e", raisedBy: "M", status: "open", boundCandidateSha: "" };
  const phase = { ...baseState({ findings: [finding] }).phase, deferrals: [{ id: "DEF-1", itemId: "F-1", text: "t", test: "x", status: "open" as const }] };
  const carried = checkItemCarried(phase, {
    type: "ITEM_CARRIED",
    recordId: "F-1",
    toPhase: "c",
    boundCandidateSha: phase.candidate?.sha ?? "",
    boundContractVersion: phase.contract.contractVersion,
    boundRecordVersion: 1,
  });
  assert.equal(carried.ok, false, "a carry on a deferred id is refused");
  assert.match(carried.reason ?? "", /DEF-1/, "the refusal names the existing deferral");

  const phase2 = { ...baseState({ findings: [finding] }).phase, carriedItems: ["F-1"] };
  const r = reduce(
    { ...baseState(), phase: phase2 },
    { type: "DEFERRAL_RECORDED", deferral: { id: "DEF-2", itemId: "F-1", text: "t", test: "x", status: "open" } },
  );
  assert.equal(r.ok, false, "a defer on a carried id is refused");
  assert.match(r.ok ? "" : r.reason, /carried/, "the refusal names the existing carry");
});

test("plan 06i: ledgerRecords covers every finding and every discovered decision of the ledger", () => {
  const finding: Finding = {
    id: "F-1",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "advisory",
    evidence: "src/a.ts:1 an edge path",
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
  };
  const decision: Decision = {
    id: "D-1",
    version: 1,
    phaseId: "p1",
    source: "reviewer-discovered",
    class: "delegated",
    choice: "keep the cap",
    whyItMatters: "the cap keeps the book bounded",
    alternatives: [{ option: "remove the cap", consequence: "unbounded memory" }],
    recommendation: { choice: "keep the cap", reason: "the book stays bounded" },
    boundCandidateSha: "C1",
    boundContractVersion: { snapshot: 1, sectionSha256: "x" },
  };
  const phase = {
    phaseId: "p1",
    candidate: { sha: "C1" },
    findings: [finding],
    decisions: [decision],
    ballots: [],
  } as unknown as PhaseState;
  const records = ledgerRecords(phase);
  assert.deepEqual(
    records.map((r) => r.id).sort(),
    ["D-1", "F-1"],
  );
});
