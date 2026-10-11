// An owner request id names exactly one request, and the park agrees with
// accept(). Valuation 02g (2026-10-10) parked for good: a record escalated
// again in a later round reused its triage request id, the resolve command
// found the resolved copy ("already resolved, not open"), the open copy kept
// the phase in AWAITING_OWNER, and even the owner's end-of-budget carry, which
// accept() honours, could not move it.

import assert from "node:assert/strict";
import { test } from "node:test";
import { awaitingOwnerTarget } from "../../src/core/owner-commands.ts";
import { reduce } from "../../src/core/reduce.ts";
import { escalationRequestId, findOwnerRequest, uniqueOwnerRequestId } from "../../src/core/triage.ts";
import type { Finding, OwnerRequest } from "../../src/core/types.ts";
import { CV, baseState, makeDecision } from "./helpers.ts";

const K = CV();
const C1 = { sha: "C1", contractVersion: K };

function triageRequest(overrides: Partial<OwnerRequest> = {}): OwnerRequest {
  return {
    id: "OR-p1-triage-D1",
    version: 1,
    phaseId: "p1",
    reason: "decision D1 received no ballots",
    origin: "failed_vote",
    linkedDecisionId: "D1",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    options: [
      { id: "accept_as_implemented", label: "accept as implemented" },
      { id: "reject_and_repair", label: "reject and repair" },
    ],
    status: "open",
    ...overrides,
  };
}

test("owner-request-ids: an item escalated again after its request was resolved gets a new request id", () => {
  const phase = baseState({ ownerRequests: [triageRequest({ status: "resolved", resolution: { option: "accept_as_implemented" } })] }).phase;
  assert.equal(escalationRequestId(phase, "D1"), "OR-p1-triage-D1-2");
  const twice = baseState({
    ownerRequests: [
      triageRequest({ status: "resolved", resolution: { option: "accept_as_implemented" } }),
      triageRequest({ id: "OR-p1-triage-D1-2", status: "resolved", resolution: { option: "accept_as_implemented" } }),
    ],
  }).phase;
  assert.equal(uniqueOwnerRequestId(twice, "OR-p1-triage-D1"), "OR-p1-triage-D1-3");
  assert.equal(uniqueOwnerRequestId(twice, "OR-p1-triage-D9"), "OR-p1-triage-D9", "an unused id is kept as it is");
});

test("owner-request-ids: in a log that already holds one id twice, a resolve answers the open request and leaves the earlier resolution alone", () => {
  const earlier = triageRequest({ status: "resolved", resolution: { option: "reject_and_repair" } });
  const open = triageRequest();
  const state = baseState({ phase: "AWAITING_OWNER", candidate: C1, decisions: [makeDecision({ id: "D1" })], ownerRequests: [earlier, open] });
  assert.equal(findOwnerRequest(state.phase, "OR-p1-triage-D1"), open);

  const result = reduce(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-p1-triage-D1",
    option: "accept_as_implemented",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  const [first, second] = result.state.phase.ownerRequests;
  assert.equal(first.resolution?.option, "reject_and_repair", "the earlier request keeps its own resolution");
  assert.equal(second.status, "resolved");
  assert.equal(second.resolution?.option, "accept_as_implemented");
});

test("owner-request-ids: the owner's carry for the current candidate un-parks the phase, as accept() honours it", () => {
  const parked = baseState({ phase: "AWAITING_OWNER", candidate: C1, ownerRequests: [triageRequest()] }).phase;
  assert.equal(awaitingOwnerTarget(parked), "AWAITING_OWNER");
  assert.equal(awaitingOwnerTarget({ ...parked, acceptedWithCarried: true, carriedCandidateSha: "C1" }), "RESOLVING");
  assert.equal(
    awaitingOwnerTarget({ ...parked, acceptedWithCarried: true, carriedCandidateSha: "C0" }),
    "AWAITING_OWNER",
    "a carry given for another candidate does not accept this one",
  );
});

test("owner-request-ids: a stale request (its finding is closed) does not keep the phase parked", () => {
  const finding = { id: "F-1", status: "repaired" } as unknown as Finding;
  const stale = triageRequest({ id: "OR-p1-triage-F-1", origin: "open_finding", linkedDecisionId: undefined, linkedFindingId: "F-1" });
  const phase = baseState({ phase: "AWAITING_OWNER", candidate: C1, findings: [finding], ownerRequests: [stale] }).phase;
  assert.equal(awaitingOwnerTarget(phase), "RESOLVING");
});
