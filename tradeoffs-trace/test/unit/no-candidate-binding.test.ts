// tt 09-5: an owner request on a phase with NO candidate can be resolved.
// On 2026-10-25 a repair-budget gate on a phase whose every attempt produced
// nothing could not be answered: the decision-view binding requires a
// candidateSha (schema minLength 1) while the phase's candidate was absent, so
// the binding check always said the phase had "changed since you viewed it".
// The binding now names the explicit NO_CANDIDATE ("none"), end to end: the
// review view writes it, the schema accepts it, the normalizer reads it back as
// the empty sha tt compares, and reduce() applies the owner's choice.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import { test } from "node:test";

import { NO_CANDIDATE, checkTupleBinding, wireCandidateSha } from "../../src/core/binding.ts";
import { briefCommandFor } from "../../src/core/briefs.ts";
import { normalizeDecisionViewCommand } from "../../src/core/owner-inbox.ts";
import { reduce } from "../../src/core/reduce.ts";
import { validate } from "../../src/core/schema.ts";
import type { DecisionBrief, OwnerRequest, State } from "../../src/core/types.ts";
import { projectEntryReview } from "../../src/render.ts";
import { CV, baseState } from "./helpers.ts";

const K = CV();

const gate: OwnerRequest = {
  id: "OR-1",
  version: 1,
  phaseId: "p1",
  reason: "every attempt timed out before a candidate existed",
  origin: "repair_budget_exhausted",
  options: [
    { id: "grant", label: "grant one more round" },
    { id: "stop", label: "stop the phase" },
  ],
  status: "open",
};

const brief: DecisionBrief = {
  requestId: "OR-1",
  question: "The phase used its rounds without a candidate. Grant one more?",
  today: "Every attempt timed out before it submitted.",
  impact: "No market stops publishing.",
  options: [
    { id: "grant", label: "Grant one more round", effect: "a fresh attempt starts", cost: "one more round" },
    { id: "stop", label: "Stop the phase", effect: "the phase is blocked", cost: "the phase's work" },
  ],
  related: [],
  evidence: [],
};

/** A phase parked on its budget gate with no candidate at all. */
function candidateLess(): State {
  return baseState({ phase: "AWAITING_OWNER", candidate: undefined, repairRoundsUsed: 3, repairRoundsGranted: 3, ownerRequests: [gate] });
}

/** The resolve command exactly as the review view's brief binding builds it. */
function resolveFromView(state: State, option: string): Record<string, unknown> {
  const p = state.phase;
  return briefCommandFor(brief, option, {
    runId: p.runId,
    phaseId: p.phaseId,
    candidateSha: wireCandidateSha(p.candidate?.sha),
    recordVersion: gate.version,
    contractVersion: p.contract.contractVersion,
  }, gate);
}

const ownerCommandSchema = JSON.parse(fs.readFileSync(new URL("../../schemas/owner-command.schema.json", import.meta.url), "utf8"));

for (const [option, expectPhase] of [["grant", "REPAIRING"], ["stop", "BLOCKED"]] as const) {
  test(`tt 09-5: a budget gate on a phase with no candidate resolves with ${option}`, () => {
    const before = candidateLess();
    const command = resolveFromView(before, option);
    assert.equal((command.binding as { candidateSha: string }).candidateSha, NO_CANDIDATE, "the view binds the explicit no-candidate value");
    const schema = validate(ownerCommandSchema, command);
    assert.equal(schema.valid, true, schema.errors.join("; "));
    const normalized = normalizeDecisionViewCommand(command, "cmd-1");
    assert.ok(normalized.ok, !normalized.ok ? normalized.reason : "");
    const result = reduce(before, normalized.event);
    assert.equal(result.ok, true, !result.ok ? result.reason : "");
    assert.equal(result.state.phase.phase, expectPhase);
    assert.equal(result.state.phase.ownerRequests[0].status, "resolved");
    if (option === "grant") assert.equal(result.state.phase.repairRoundsGranted, before.phase.repairRoundsGranted + 1);
  });
}

test("tt 09-5: a no-candidate binding is stale once the phase has a candidate, and says so plainly", () => {
  const withCandidate = baseState({ phase: "AWAITING_OWNER", candidate: { sha: "C1", contractVersion: K }, ownerRequests: [gate] });
  const stale = checkTupleBinding(
    { runId: "r1", phaseId: "p1", candidateSha: NO_CANDIDATE, contractVersion: K, recordId: "OR-1", recordVersion: 1 },
    withCandidate.phase,
  );
  assert.equal(stale.ok, false);
  assert.match(stale.reason ?? "", /bound to no candidate, but the phase is now at candidate C1 — it changed since you viewed it/);
  // And the other way round: a sha bound while the phase has none.
  const gone = checkTupleBinding(
    { runId: "r1", phaseId: "p1", candidateSha: "C1", contractVersion: K, recordId: "OR-1", recordVersion: 1 },
    candidateLess().phase,
  );
  assert.equal(gone.ok, false);
  assert.match(gone.reason ?? "", /bound to candidate C1, but the phase is now at no candidate/);
});

test("tt 09-5: an empty candidateSha is still a missing one", () => {
  const command = { ...resolveFromView(candidateLess(), "grant") };
  command.binding = { ...(command.binding as Record<string, unknown>), candidateSha: "" };
  const normalized = normalizeDecisionViewCommand(command, "cmd-1");
  assert.equal(normalized.ok, false);
  assert.match(!normalized.ok ? normalized.reason : "", /missing 'candidateSha'.*"none"/);
});

test("tt 09-5: the review view gives a candidate-less phase's brief a binding naming no candidate", () => {
  const p = candidateLess().phase;
  const { text } = projectEntryReview({
    phaseId: p.phaseId,
    runId: p.runId,
    briefs: [brief],
    ownerRequests: [gate],
    contract: { contractVersion: K },
  });
  assert.match(text, /^\s+:ID:\s+OR-1$/m);
  assert.match(text, /^\s+:CANDIDATE_SHA:\s+none$/m, "the Emacs front end reads this property into the binding");
  assert.match(text, /^\s+:RECORD_VERSION: 1$/m);
});
