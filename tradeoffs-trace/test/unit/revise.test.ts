// REVISE (design §7.5), round-1 review item 8: the repair allowance is
// fixed at 3 by the core (not taken from the event), and the original
// decision is preserved and marked `superseded by correction <id>`.

import assert from "node:assert/strict";
import { test } from "node:test";
import { accept } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import { CV, baseState, makeDecision } from "./helpers.ts";

const K = CV();

test("revise: the granted allowance is fixed at 3 by the core, independent of the event payload", () => {
  const decision = makeDecision({ id: "D1", version: 1 });
  const state = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    decisions: [decision],
    repairRoundsUsed: 3,
    repairRoundsGranted: 3, // budget already exhausted
  });

  const result = reduce(state, {
    type: "REVISE",
    correctionId: "C-1",
    targetRecordId: "D1",
    correctionText: "do it this way instead",
    contractChange: false,
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });

  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  assert.equal(result.state.phase.phase, "REPAIRING");
  // 3 (exhausted) + 3 (the fixed allowance) = 6 — independent of the
  // exhausted budget (design §7.5, §8.1).
  assert.equal(result.state.phase.repairRoundsGranted, 6);
  const correction = result.state.phase.corrections.find((c) => c.id === "C-1")!;
  assert.equal(correction.grantedRounds, 3);
});

test("revise: the original decision is preserved and marked superseded by the correction", () => {
  const decision = makeDecision({ id: "D-p2-05", version: 2, choice: "original choice text" });
  const state = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    decisions: [decision],
  });

  const result = reduce(state, {
    type: "REVISE",
    correctionId: "C-1",
    targetRecordId: "D-p2-05",
    correctionText: "keep immediate handling",
    contractChange: false,
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 2,
  });

  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  const superseded = result.state.phase.decisions.find((d) => d.id === "D-p2-05")!;
  assert.equal(superseded.supersededByCorrection, "C-1");
  // "the original decision is preserved" — its content is untouched.
  assert.equal(superseded.choice, "original choice text");
  assert.equal(superseded.version, 3); // bumped, since its content (linkage) changed
});

test("revise: a superseded decision no longer blocks accept(C, K) on its own — the correction does instead", () => {
  const decision = makeDecision({ id: "D1", class: "reserved", version: 1 }); // would otherwise require an owner-resolved request
  const state = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    decisions: [decision],
  });

  const revised = reduce(state, {
    type: "REVISE",
    correctionId: "C-1",
    targetRecordId: "D1",
    correctionText: "correction text",
    contractChange: false,
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(revised.ok, true, !revised.ok ? revised.reason : "");

  const phaseAfter = {
    ...revised.state.phase,
    phase: "RESOLVING" as const,
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: revised.state.phase.integrationHead, probedI: "I1", passed: true },
    reviews: {
      M: { review: { reviewer: "M" as const, phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [{ correctionId: "C-1", status: "honored" as const }], findingStatements: [] } },
      A: { review: { reviewer: "A" as const, phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [{ correctionId: "C-1", status: "honored" as const }], findingStatements: [] } },
      B: { review: { reviewer: "B" as const, phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [{ correctionId: "C-1", status: "honored" as const }], findingStatements: [] } },
    },
  };
  // accept() must hold: the superseded reserved decision is skipped, and
  // the correction it produced is addressed (all three reviews honored it).
  assert.equal(accept(phaseAfter, "C1", K), true);
});
