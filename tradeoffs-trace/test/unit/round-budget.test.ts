// Plan 06g (A6): the round budget's owner decision. When the phase stops at
// the end of its round budget and the only open findings are advisories, the
// owner's SINGLE decision is "accept with carried items": taking it accepts
// the candidate as it stands and records the carried items, which `tt
// summary` lists for the next phase's plan. A blocking item is never carried
// — it gets its own request, so the owner sees only the blocking items.

import assert from "node:assert/strict";
import { test } from "node:test";

import { buildContract, type RunPlanPhase } from "../../src/conductor.ts";
import { applyItemCarried, applyOwnerRequestResolved } from "../../src/core/owner-commands.ts";
import { openItemOwnerRequestsFor } from "../../src/core/owner-requests.ts";
import { accept } from "../../src/core/predicate.ts";
import type { Finding, PhaseState } from "../../src/core/types.ts";
import { carriedItemsSection } from "../../src/view.ts";
import { approvingReview, baseState, CV } from "./helpers.ts";

const K = CV();
const C1 = { sha: "C1", contractVersion: K };

function finding(overrides: Partial<Finding> = {}): Finding {
  return {
    id: "F-adv",
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "advisory",
    evidence: "a further edge path in the pick path (hardening), src/core/rounds.ts:12",
    raisedBy: "B",
    status: "open",
    boundCandidateSha: "C1",
    ...overrides,
  };
}

/** A phase that has spent its whole round budget: checks, probe and reviews
 * are valid, and it is parked on the owner. */
function spent(overrides: Partial<PhaseState> = {}): PhaseState {
  return baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: approvingReview("M", "C1", K) },
      A: { review: approvingReview("A", "C1", K) },
      B: { review: approvingReview("B", "C1", K) },
    },
    repairRoundsUsed: 3,
    repairRoundsGranted: 3,
    ...overrides,
  }).phase;
}

test("plan 06g: with only advisories open, the budget-spent phase offers one decision — accept with carried items", () => {
  const phase = spent({ findings: [finding()] });
  const requests = openItemOwnerRequestsFor(phase, "the repair budget ran out while items remained open");
  assert.equal(requests.length, 1, "the owner gets exactly one decision");
  assert.deepEqual(requests[0].options.map((o) => o.id), ["accept_carried"]);
  assert.equal(requests[0].options[0].label, "accept with carried items");
  assert.match(requests[0].reason, /only advisories are open/);
  assert.match(requests[0].reason, /F-adv/);
});

test("plan 06g: with a blocking item open the owner sees only the blocking items", () => {
  const phase = spent({
    findings: [finding(), finding({ id: "F-block", severity: "blocking", evidence: "R3 unmet at src/core/rounds.ts:88" })],
  });
  const requests = openItemOwnerRequestsFor(phase, "the repair budget ran out while items remained open");
  assert.equal(requests.length, 1);
  assert.equal(requests[0].linkedFindingId, "F-block");
  assert.deepEqual(requests[0].options.map((o) => o.id), ["accept_risk", "repair"]);
  // No carried option anywhere.
  assert.ok(!requests.some((r) => r.options.some((o) => o.id === "accept_carried")));
});

test("plan 06g: a phase with no findings at all keeps the plain grant/stop budget request", () => {
  const phase = spent();
  const requests = openItemOwnerRequestsFor(phase, "the repair budget ran out");
  assert.deepEqual(requests[0].options.map((o) => o.id), ["grant", "stop"]);
});

test("plan 06g: taking accept with carried items accepts the candidate and records the carried items", () => {
  const phase = spent({ findings: [finding()] });
  const request = openItemOwnerRequestsFor(phase, "the repair budget ran out while items remained open")[0];
  const parked: PhaseState = { ...phase, ownerRequests: [{ ...request, status: "open" as const }] };
  // An advisory finding alone never blocks acceptance; the OPEN owner request
  // is what keeps the parked phase from accepting on its own.
  assert.equal(accept({ ...parked, ownerRequests: [] }, "C1", K), true, "an advisory alone never blocks acceptance");
  assert.equal(accept(parked, "C1", K), false, "an open owner request holds acceptance until the owner decides");

  const resolved = applyOwnerRequestResolved(parked, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: request.id,
    option: "accept_carried",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: request.version,
  });
  assert.equal(resolved.acceptedWithCarried, true);
  assert.deepEqual(resolved.carriedItems, ["F-adv"]);
  // A5: each carried item is listed with its target; this request has no
  // explicit `--to`, so the leftovers go to the next phase.
  assert.deepEqual(resolved.carriedTo, { "F-adv": "next" });
  assert.equal(accept(resolved, "C1", K), true);
});

test("plan 06g: tt carry never accepts a candidate whose checks failed", () => {
  // ODP-2 (owner-verified): the carry waives only the OPEN REVIEW ITEMS. The
  // mechanical gates — checks passed for C, the probe for C onto the current
  // head, and C being the candidate the carry was given for — are facts and
  // are evaluated BEFORE the carry shortcut.
  const phase = spent({ checks: { candidateSha: "C1", passed: false }, findings: [finding()] });
  const request = openItemOwnerRequestsFor(phase, "the repair budget ran out while items remained open")[0];
  const parked: PhaseState = { ...phase, ownerRequests: [{ ...request, status: "open" as const }] };
  const carried = applyItemCarried(parked, {
    type: "ITEM_CARRIED",
    recordId: "F-adv",
    recordKind: "finding",
    toPhase: "06h",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(carried.acceptedWithCarried, true, "the carry is recorded");
  assert.equal(carried.carriedCandidateSha, "C1", "with the candidate it was given for");
  assert.equal(accept(carried, "C1", K), false, "a failed check is never waived by the carry");

  // A stale carry (given for another candidate) is not acceptance either.
  const passingButStale = { ...carried, checks: { candidateSha: "C1", passed: true }, carriedCandidateSha: "C2" };
  assert.equal(accept(passingButStale, "C1", K), false, "a carry for another candidate never accepts C1");

  // With the mechanical gates met for the carried candidate, the owner's carry
  // accepts the candidate, waiving the open advisory and owner request.
  const gated = {
    ...carried,
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    integrationHead: "H0",
  };
  assert.equal(accept(gated, "C1", K), true, "with the mechanical gates met, the owner's carry accepts");
});

test("plan 06g: tt carry never waives a missing named test verify or unrecorded evidence", () => {
  // ODP-3: these are mechanical gates, evaluated before the carry shortcut.
  const contract = buildContract({
    id: "p1",
    goal: "g",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    provisional: false,
    requirements: [
      { id: "R1", title: "R1", text: "R1 proves it", arch: [], verify: ['test "R1 proves it"'] },
      { id: "R2", title: "R2", text: "the owner run is recorded", arch: [], verify: ["evidence"] },
    ],
  } as unknown as RunPlanPhase);
  const carried = spent({
    contract,
    acceptedWithCarried: true,
    carriedCandidateSha: "C1",
    itemEvidence: [{ id: "R2", text: "the live run is in NOTES.md" }],
  });
  // The named test verify passed and the evidence is recorded: the carry holds.
  assert.equal(
    accept({ ...carried, checkResolution: [{ id: "R1", name: "R1 proves it", outcome: "passed" }] }, "C1", K),
    true,
    "with every mechanical gate met, the carry accepts",
  );
  // A missing named test verify is never waived.
  assert.equal(
    accept({ ...carried, checkResolution: [{ id: "R1", name: "R1 proves it", outcome: "missing" }] }, "C1", K),
    false,
    "a missing named test verify is not waived",
  );
  // An unrecorded `evidence` item is never waived.
  assert.equal(
    accept({ ...carried, itemEvidence: [], checkResolution: [{ id: "R1", name: "R1 proves it", outcome: "passed" }] }, "C1", K),
    false,
    "unrecorded evidence is not waived",
  );
});

test("plan 06g: tt summary lists the carried items by id, with what each one says", () => {
  const phase = spent({ findings: [finding()], acceptedWithCarried: true, carriedItems: ["F-adv"] });
  const summary = carriedItemsSection(phase).join("\n");
  assert.match(summary, /### Carried items \(1\)/);
  assert.match(summary, /F-adv/);
  assert.match(summary, /a further edge path in the pick path/);
  // A phase the owner did not accept that way carries nothing.
  assert.deepEqual(carriedItemsSection(spent({ findings: [finding()] })), []);
});
