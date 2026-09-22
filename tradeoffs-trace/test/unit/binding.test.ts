// stale-binding: design §7.1 — a ballot or command bound to a superseded
// candidate, contract version or record version is rejected with a
// visible, human-readable reason naming what changed, e.g. "decision D-7
// changed v2 → v3 since you viewed it". This is checked both directly
// against binding.ts's functions and, per round-1 review item 4, through
// reduce() itself for every owner-command event that carries a binding.

import assert from "node:assert/strict";
import { test } from "node:test";
import { checkBallotBinding, checkTupleBinding } from "../../src/core/binding.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { BindingTuple, Finding } from "../../src/core/types.ts";
import { CV, baseState, basePhase, makeBallot, makeDecision } from "./helpers.ts";

const K = CV();

test("stale-binding: a ballot bound to a superseded candidate is rejected with a visible reason", () => {
  const decision = makeDecision({ id: "D1" });
  const phase = basePhase({ candidate: { sha: "C2", contractVersion: K }, decisions: [decision] });
  const ballot = makeBallot({ decisionId: "D1", boundCandidateSha: "C1-stale", boundContractVersion: K, boundRecordVersion: 1 });
  const result = checkBallotBinding(ballot, phase);
  assert.equal(result.ok, false);
  assert.match(result.reason ?? "", /candidate C1-stale/);
  assert.match(result.reason ?? "", /now at candidate C2/);
  assert.match(result.reason ?? "", /changed since you viewed it/);
});

test("stale-binding: a ballot bound to a superseded contract version is rejected with a visible reason", () => {
  const decision = makeDecision({ id: "D1" });
  const phase = basePhase({
    candidate: { sha: "C1", contractVersion: CV(2) },
    contract: { ...basePhase().contract, contractVersion: CV(2) },
    decisions: [decision],
  });
  const ballot = makeBallot({ decisionId: "D1", boundCandidateSha: "C1", boundContractVersion: CV(1), boundRecordVersion: 1 });
  const result = checkBallotBinding(ballot, phase);
  assert.equal(result.ok, false);
  assert.match(result.reason ?? "", /contract v1/);
  assert.match(result.reason ?? "", /now at contract v2/);
});

test("stale-binding: a ballot bound to a superseded decision (record) version is rejected, naming the version change", () => {
  const decision = makeDecision({ id: "D-7", version: 3 });
  const phase = basePhase({ candidate: { sha: "C1", contractVersion: K }, decisions: [decision] });
  const ballot = makeBallot({ decisionId: "D-7", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 2 });
  const result = checkBallotBinding(ballot, phase);
  assert.equal(result.ok, false);
  assert.match(result.reason ?? "", /decision D-7 changed v2 → v3 since you viewed it/);
});

test("stale-binding: a command bound to a superseded decision version is rejected, naming the version change", () => {
  const decision = makeDecision({ id: "D-7", version: 3 });
  const phase = basePhase({ candidate: { sha: "C1", contractVersion: K }, decisions: [decision] });
  const tuple: BindingTuple = {
    runId: "r1",
    phaseId: "p1",
    candidateSha: "C1",
    contractVersion: K,
    recordId: "D-7",
    recordVersion: 2, // the owner viewed it at v2; it is now v3
  };
  const result = checkTupleBinding(tuple, phase);
  assert.equal(result.ok, false);
  assert.match(result.reason ?? "", /decision D-7 changed v2 → v3 since you viewed it/);
});

test("stale-binding: a command bound to the current version is accepted", () => {
  const decision = makeDecision({ id: "D-7", version: 2 });
  const phase = basePhase({ candidate: { sha: "C1", contractVersion: K }, decisions: [decision] });
  const tuple: BindingTuple = {
    runId: "r1",
    phaseId: "p1",
    candidateSha: "C1",
    contractVersion: K,
    recordId: "D-7",
    recordVersion: 2,
  };
  const result = checkTupleBinding(tuple, phase);
  assert.equal(result.ok, true);
});

// --- through reduce() itself (round-1 review item 4) -----------------------

test("stale-binding via reduce(): a stale BALLOT_CAST is rejected with a visible reason", () => {
  const decision = makeDecision({ id: "D1", version: 2 });
  const state = baseState({ candidate: { sha: "C1", contractVersion: K }, decisions: [decision] });
  const result = reduce(state, {
    type: "BALLOT_CAST",
    ballot: makeBallot({ decisionId: "D1", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 }),
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /decision D1 changed v1 → v2 since you viewed it/);
});

test("stale-binding via reduce(): a stale OWNER_REQUEST_RESOLVED is rejected with a visible reason", () => {
  const state = baseState({
    candidate: { sha: "C1", contractVersion: K },
    ownerRequests: [
      {
        id: "OR-1",
        version: 2,
        phaseId: "p1",
        reason: "x",
        origin: "open_finding",
        options: [
          { id: "accept_risk", label: "accept the risk" },
          { id: "repair", label: "repair (grant 3 rounds)" },
        ],
        status: "open",
      },
    ],
  });
  const result = reduce(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "repair",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /owner request OR-1 changed v1 → v2 since you viewed it/);
});

test("stale-binding via reduce(): resolving an already-resolved owner request is rejected", () => {
  const state = baseState({
    candidate: { sha: "C1", contractVersion: K },
    ownerRequests: [
      {
        id: "OR-1",
        version: 1,
        phaseId: "p1",
        reason: "x",
        origin: "open_finding",
        status: "resolved",
        resolution: { option: "1" },
        resolvedBinding: { candidateSha: "C1", contractVersion: K },
      },
    ],
  });
  const result = reduce(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "2",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /already resolved/);
});

test("stale-binding via reduce(): a stale FINDING_ACCEPTED_BY_OWNER is rejected with a visible reason", () => {
  const finding: Finding = {
    id: "F-1",
    version: 2,
    phaseId: "p1",
    kind: "defect",
    severity: "blocking",
    evidence: "x",
    raisedBy: "A",
    status: "open",
    boundCandidateSha: "C1",
  };
  const state = baseState({ candidate: { sha: "C1", contractVersion: K }, findings: [finding] });
  const result = reduce(state, {
    type: "FINDING_ACCEPTED_BY_OWNER",
    findingId: "F-1",
    scope: "known limitation",
    by: "owner",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /finding F-1 changed v1 → v2 since you viewed it/);
});

test("stale-binding via reduce(): a stale REVISE is rejected with a visible reason naming the decision and version change", () => {
  const decision = makeDecision({ id: "D-p2-05", version: 3 });
  const state = baseState({ candidate: { sha: "C1", contractVersion: K }, decisions: [decision] });
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
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /decision D-p2-05 changed v2 → v3 since you viewed it/);
});

test("stale-binding via reduce(): a stale AMEND is rejected, naming the contract version change", () => {
  const state = baseState({
    candidate: { sha: "C1", contractVersion: CV(2) },
    contract: { ...basePhase().contract, contractVersion: CV(2) },
  });
  const result = reduce(state, {
    type: "AMEND",
    replacingContractVersion: CV(1),
    newContractVersion: CV(3),
  });
  assert.equal(result.ok, false);
  assert.match((result as { reason: string }).reason, /contract changed v1 → v2 since you viewed it/);
});
