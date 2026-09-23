// Shared fixture builders for unit tests. Not itself a test file.

import type { Ballot, ContractVersion, Decision, PhaseState, RunStateName, State } from "../../src/core/types.ts";

export function CV(snapshot = 1, sectionSha256 = "a".repeat(64)): ContractVersion {
  return { snapshot, sectionSha256 };
}

export function basePhase(overrides: Partial<PhaseState> = {}): PhaseState {
  return {
    runId: "r1",
    phaseId: "p1",
    contract: {
      phaseId: "p1",
      contractVersion: CV(),
      goal: "make cancellation race-free",
      acceptance: ["no fill after cancel is acknowledged"],
      checks: ["npm test"],
      boundaries: [],
      reserved: [],
    },
    phase: "READY",
    attempt: { n: 1 },
    integrationHead: "H0",
    reviews: {},
    decisions: [],
    findings: [],
    ownerRequests: [],
    corrections: [],
    ballots: [],
    overrides: [],
    inFlight: {},
    repairRoundsUsed: 0,
    repairRoundsGranted: 3,
    ...overrides,
  };
}

export function baseState(overrides: Partial<PhaseState> = {}, run: RunStateName = "RUN_ACTIVE"): State {
  return { run, phase: basePhase(overrides) };
}

export function makeDecision(overrides: Partial<Decision> = {}): Decision {
  return {
    id: "D1",
    version: 1,
    phaseId: "p1",
    source: "worker",
    class: "delegated",
    choice: "Batch cancels per tick instead of one lock per request",
    whyItMatters: "fewer lock acquisitions under load",
    alternatives: [{ option: "a lock per request", consequence: "more contention" }],
    recommendation: { choice: "batch per tick", reason: "stays inside the latency budget" },
    boundCandidateSha: "C1",
    boundContractVersion: CV(),
    ...overrides,
  };
}

export function makeBallot(overrides: Partial<Ballot> = {}): Ballot {
  return {
    reviewer: "M",
    decisionId: "D1",
    vote: "approve",
    rationale: "reasoned",
    evidence: ["src/cancel.ts:42"],
    boundCandidateSha: "C1",
    boundContractVersion: CV(),
    boundRecordVersion: 1,
    ...overrides,
  };
}

/** A fully-approving review bound to (C, K), with no corrections or
 * findings to speak to (design §6.3: required even when there are no
 * decisions to vote on). */
export function approvingReview(
  reviewer: "M" | "A" | "B",
  candidateSha: string,
  contractVersion: ContractVersion,
  extra: { correctionStatements?: { correctionId: string; status: "honored" | "not_honored" }[]; findingStatements?: { findingId: string; status: "confirm" | "withdraw" }[] } = {},
) {
  return {
    reviewer,
    phaseId: "p1",
    candidateSha,
    contractVersion,
    correctionStatements: extra.correctionStatements ?? [],
    findingStatements: extra.findingStatements ?? [],
  };
}
