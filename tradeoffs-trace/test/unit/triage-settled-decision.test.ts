// A decision the owner ruled on, or an amendment already applied to the
// contract, is a settled trade-off in triage, never an unclassified record.
//
// The evaluator does not classify such records, so evidenceForDecision left
// them unclassified and disposition() escalated them: each new candidate
// reopened a blocking owner request for the same settled decision (mm 02a run
// 9617119b, 2026-10-15: amendment A-55 raised four times, three repair rounds
// spent on a phase whose items were all met).

import assert from "node:assert/strict";
import { test } from "node:test";

import { disposition, evidenceForDecision } from "../../src/core/triage.ts";
import type { Decision } from "../../src/core/types.ts";
import { basePhase, CV, makeBallot, makeDecision } from "./helpers.ts";

const amendment = (status: "proposed" | "applied" | "reverted"): Decision =>
  makeDecision({
    id: "D-amend",
    class: "reserved",
    choice: "Drive the strategy over the fixture books, with GOLD extended past the 50 bps edges",
    amendment: {
      id: "AM-1",
      criterion: "R5 as written",
      proposedWording: "R5 with the extended GOLD book",
      itemId: "R5",
      why: "the fixture book contradicts the spec",
      raisedBy: "worker",
      status,
    },
  });

function triage(decision: Decision, phaseOverrides = {}) {
  const phase = basePhase({
    candidate: { sha: "C1", contractVersion: CV() },
    decisions: [decision],
    ballots: [makeBallot({ decisionId: decision.id, reviewer: "A", vote: "reject" }), makeBallot({ decisionId: decision.id, reviewer: "B", vote: "reject" })],
    ...phaseOverrides,
  });
  const evidence = evidenceForDecision(decision, phase, true);
  return disposition({ itemId: decision.id, source: "decision" }, evidence);
}

test("an owner-ruled decision is an accepted trade-off, not an escalation, on every later candidate", () => {
  const decision = makeDecision({ id: "D-ruled", choice: "Keep the boundary stamp with the step-time book" });
  const d = triage(decision, { ruledDecisions: [{ id: "D-ruled", choice: decision.choice, by: "owner" }] });
  assert.equal(d.kind, "tradeoff", JSON.stringify(d));
});

test("an owner ruling on a different choice does not settle the reworded decision", () => {
  const decision = makeDecision({ id: "D-ruled", choice: "A reworded choice" });
  const d = triage(decision, { ruledDecisions: [{ id: "D-ruled", choice: "the original choice", by: "owner" }] });
  assert.notEqual(d.kind, "tradeoff");
});

test("an amendment already applied to the contract never reopens an owner request", () => {
  const d = triage(amendment("applied"));
  assert.equal(d.kind, "tradeoff", JSON.stringify(d));
});

test("a proposed amendment that failed its vote still escalates to the owner", () => {
  const d = triage(amendment("proposed"));
  assert.equal(d.kind, "escalate", JSON.stringify(d));
});
