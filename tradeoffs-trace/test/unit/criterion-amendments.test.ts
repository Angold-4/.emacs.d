// Plan 01g: criterion disputes and amendments, in the pure core.
//
// "Unmet but clear" criteria are repaired; "unmeetable as written" ones are
// reworded. A dispute becomes a `reserved` amendment decision, voted on like
// any other. A passing normal tally rewrites one acceptance item for this
// phase, bumps the contract version and supersedes the contract findings that
// cited the old wording; the next candidate is judged against the new
// wording. A failed one changes nothing and never consumes a repair round by
// itself. The owner's correction naming the amendment id restores the
// original wording.

import assert from "node:assert/strict";
import { test } from "node:test";
import { next } from "../../src/core/next.ts";
import { reduce } from "../../src/core/reduce.ts";
import { amendmentToApply, decisionStatus } from "../../src/core/predicate.ts";
import type { Ballot, ContractVersion, Decision, Event, Finding, State } from "../../src/core/types.ts";
import { baseState, CV } from "./helpers.ts";

const K = CV(1);
const K2 = CV(2, "b".repeat(64));
const K3 = CV(3, "c".repeat(64));
const CRITERION = "no fill after cancel is acknowledged";
const NEW_WORDING = "the guarantee holds within the cancellation tick";

function step(state: State, event: Event): State {
  const result = reduce(state, event);
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  return result.state;
}

function amendmentDecision(overrides: Partial<Decision> = {}): Decision {
  return {
    id: "D-p1-C1-amendment",
    version: 1,
    phaseId: "p1",
    source: "worker",
    class: "reserved",
    choice: NEW_WORDING,
    whyItMatters: "every candidate misses the literal wording by one tick",
    alternatives: [{ option: CRITERION, consequence: "no candidate can satisfy it" }],
    recommendation: { choice: NEW_WORDING, reason: "satisfiable and still meaningful" },
    boundCandidateSha: "C1",
    boundContractVersion: K,
    amendment: {
      id: "AM-p1-C1",
      criterion: CRITERION,
      proposedWording: NEW_WORDING,
      why: "every candidate misses the literal wording by one tick",
      raisedBy: "worker",
      status: "proposed",
      previousContractVersion: K,
    },
    ...overrides,
  };
}

function ballot(reviewer: "M" | "A" | "B", vote: "approve" | "reject"): Ballot {
  return {
    reviewer,
    decisionId: "D-p1-C1-amendment",
    vote,
    rationale: "the wording cannot be met as written",
    evidence: ["src/cancel.ts:42"],
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  };
}

function contractFinding(): Finding {
  return {
    id: "F-p1-contract-1",
    version: 1,
    phaseId: "p1",
    kind: "contract",
    severity: "blocking",
    evidence: `the acceptance item "${CRITERION}" cannot be met on any candidate`,
    raisedBy: "B",
    status: "open",
    boundCandidateSha: "C1",
    criterionDisputed: CRITERION,
  };
}

function resolvingState(extra: { findings?: Finding[]; ballots?: Ballot[] } = {}): State {
  const reviews = {
    M: { review: { reviewer: "M" as const, phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
    A: { review: { reviewer: "A" as const, phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
    B: { review: { reviewer: "B" as const, phaseId: "p1", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [] } },
  };
  return baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews,
    decisions: [amendmentDecision()],
    findings: extra.findings ?? [],
    ballots: extra.ballots ?? [],
  });
}

test("criterion-amendments: a passing tally is the only thing that rewrites the criterion", () => {
  // M and A approve: the amendment is ready to apply before anything else.
  let state = resolvingState({ ballots: [ballot("M", "approve"), ballot("A", "approve"), ballot("B", "reject")] });
  assert.equal(amendmentToApply(state.phase, "C1", K)?.id, "D-p1-C1-amendment");

  // next() asks for the amendment before it asks to accept the candidate.
  assert.deepEqual(next(state), [{ type: "apply_amendment", decisionId: "D-p1-C1-amendment" }]);

  state = step(state, {
    type: "CRITERION_AMENDED",
    decisionId: "D-p1-C1-amendment",
    newAcceptance: [NEW_WORDING],
    newContractVersion: K2,
  });
  // The next candidate is judged against the new wording: the phase starts a
  // fresh attempt, and no repair round was consumed.
  assert.equal(state.phase.phase, "IMPLEMENTING");
  assert.deepEqual(state.phase.contract.acceptance, [NEW_WORDING]);
  assert.deepEqual(state.phase.contract.contractVersion, K2);
  assert.equal(state.phase.repairRoundsUsed, 0);
  assert.equal(state.phase.attempt.n, 2);
  assert.equal(state.phase.decisions[0].amendment?.status, "applied");
  assert.deepEqual(state.phase.decisions[0].amendment?.appliedContractVersion, K2);
  assert.deepEqual(next(state), [{ type: "dispatch_worker" }]);
});

test("criterion-amendments: contract findings citing the replaced wording are superseded, not left open", () => {
  let state = resolvingState({
    ballots: [ballot("M", "approve"), ballot("A", "approve")],
    findings: [contractFinding()],
  });
  assert.equal(state.phase.findings[0].status, "open", "precondition: the finding is open");
  state = step(state, {
    type: "CRITERION_AMENDED",
    decisionId: "D-p1-C1-amendment",
    newAcceptance: [NEW_WORDING],
    newContractVersion: K2,
  });
  const finding = state.phase.findings.find((f) => f.id === "F-p1-contract-1")!;
  assert.equal(finding.status, "superseded");
  assert.match(finding.supersededBy ?? "", /AM-p1-C1/);
  // A superseded finding no longer blocks acceptance or requests the owner.
  assert.equal(state.phase.findings.some((f) => f.status === "open"), false);
});

test("criterion-amendments: an M veto leaves the criterion unchanged and the round is handled as today", () => {
  // M vetoes, so the amendment fails its normal tally.
  const state = resolvingState({
    ballots: [ballot("M", "reject"), ballot("A", "approve")],
    findings: [
      {
        id: "F-p1-defect-1",
        version: 1,
        phaseId: "p1",
        kind: "defect",
        severity: "blocking",
        evidence: "src/cancel.ts:1",
        raisedBy: "A",
        status: "open",
        boundCandidateSha: "C1",
      },
    ],
  });
  assert.equal(amendmentToApply(state.phase, "C1", K), undefined);
  assert.deepEqual(state.phase.contract.acceptance, [CRITERION]);
  assert.equal(state.phase.decisions[0].amendment?.status, "proposed");
  assert.equal(decisionStatus(state.phase.decisions[0], state.phase).status, "failed");
  // The amendment does not block acceptance on its own, so the ordinary
  // blocking finding is what sends the phase to a repair round.
  assert.deepEqual(next(state), [{ type: "resolving_incomplete" }]);
});

test("criterion-amendments: the owner's correction naming the amendment id restores the original wording", () => {
  let state = resolvingState({ ballots: [ballot("M", "approve"), ballot("A", "approve")] });
  state = step(state, {
    type: "CRITERION_AMENDED",
    decisionId: "D-p1-C1-amendment",
    newAcceptance: [NEW_WORDING],
    newContractVersion: K2,
  });
  assert.deepEqual(state.phase.contract.acceptance, [NEW_WORDING]);

  // Record-only: it works from any non-terminal state, moves no phase state
  const before = state.phase.phase;
  state = step(state, {
    type: "CRITERION_REVERTED",
    amendmentId: "AM-p1-C1",
    newAcceptance: [CRITERION],
    newContractVersion: K3,
  });
  assert.equal(state.phase.phase, before, "a revert never moves the phase state name");
  assert.deepEqual(state.phase.contract.acceptance, [CRITERION]);
  assert.deepEqual(state.phase.contract.contractVersion, K3);
  assert.equal(state.phase.decisions[0].amendment?.status, "reverted");

  // A second revert of the same amendment is refused, not silently ignored.
  const again = reduce(state, {
    type: "CRITERION_REVERTED",
    amendmentId: "AM-p1-C1",
    newAcceptance: [CRITERION],
    newContractVersion: K3,
  });
  assert.equal(again.ok, false);
});

test("criterion-amendments: applying an unknown amendment, or one whose criterion the contract does not carry, is rejected", () => {
  const state = resolvingState({ ballots: [ballot("M", "approve"), ballot("A", "approve")] });
  const unknown = reduce(state, {
    type: "CRITERION_AMENDED",
    decisionId: "D-nope",
    newAcceptance: [NEW_WORDING],
    newContractVersion: K2,
  });
  assert.equal(unknown.ok, false);

  const offContract = baseState({
    ...state.phase,
    decisions: [amendmentDecision({ amendment: { ...amendmentDecision().amendment!, criterion: "a criterion the contract never had" } })],
  });
  const result = reduce(offContract, {
    type: "CRITERION_AMENDED",
    decisionId: "D-p1-C1-amendment",
    newAcceptance: [NEW_WORDING],
    newContractVersion: K2,
  });
  assert.equal(result.ok, false);
});
