// Shared, single-source-of-truth logic for the three owner commands that
// close a record-level open item — OWNER_REQUEST_RESOLVED,
// FINDING_ACCEPTED_BY_OWNER and OVERRIDE_CAST — used both by reduce.ts's
// ordinary (phase-unchanged) handling and by transitions.ts's AWAITING_OWNER
// rows (design §6.1: "AWAITING_OWNER ... leaves only through an owner
// command"), so the binding check and the mutation are never duplicated.

import { checkBinding, checkTupleBinding } from "./binding.ts";
import { isBudgetGateRequest, isRepairForcingOption } from "./owner-requests.ts";
import type { BindingTuple, ContractVersion, Event, PhaseState } from "./types.ts";

export interface CommandCheck {
  ok: boolean;
  reason?: string;
}

function bindingTuple(phase: PhaseState, recordId: string, candidateSha: string, contractVersion: ContractVersion, recordVersion: number): BindingTuple {
  return { runId: phase.runId, phaseId: phase.phaseId, candidateSha, contractVersion, recordId, recordVersion };
}

/** Resolve any OPEN owner request linked to `recordId` (a decision,
 * finding or correction) as a side effect of the owner acting on that
 * record directly (override, accept-finding, or revise) — the record-level
 * action addresses the request about it. */
export function autoResolveLinkedRequest(
  phase: PhaseState,
  recordId: string,
  option: string,
  candidateSha: string,
  contractVersion: ContractVersion,
): PhaseState {
  const ownerRequests = phase.ownerRequests.map((r) =>
    r.status === "open" &&
    (r.linkedDecisionId === recordId || r.linkedFindingId === recordId || r.linkedCorrectionId === recordId)
      ? { ...r, status: "resolved" as const, resolution: { option }, resolvedBinding: { candidateSha, contractVersion } }
      : r,
  );
  return { ...phase, ownerRequests };
}

// --- OWNER_REQUEST_RESOLVED -------------------------------------------

type EvOwnerRequestResolved = Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;

export function checkOwnerRequestResolved(phase: PhaseState, event: EvOwnerRequestResolved): CommandCheck {
  const request = phase.ownerRequests.find((r) => r.id === event.requestId);
  if (!request) return { ok: false, reason: `unknown owner request ${event.requestId}` };
  if (request.status !== "open") {
    return { ok: false, reason: `owner request ${event.requestId} is already ${request.status}, not open` };
  }
  // Binding (design §7.1) first — is this even the version of the request
  // the sender saw? — before what they chose is judged at all.
  const tuple = bindingTuple(phase, event.requestId, event.boundCandidateSha, event.boundContractVersion, event.boundRecordVersion);
  const bindingCheck = checkTupleBinding(tuple, phase);
  if (!bindingCheck.ok) return bindingCheck;
  // design round-3 review item 1: the option is named by its stable id, and
  // an id the request does not offer is rejected — never matched by label.
  if (!request.options.some((o) => o.id === event.option)) {
    return {
      ok: false,
      reason: `'${event.option}' is not an option owner request ${event.requestId} offers (offered: ${request.options.map((o) => o.id).join(", ")})`,
    };
  }
  if (request.origin === "open_finding" && event.option === "accept_risk" && (!event.note || event.note.trim().length === 0)) {
    return { ok: false, reason: `accepting the risk for finding ${request.linkedFindingId} requires a non-empty scope note` };
  }
  return { ok: true };
}

export function applyOwnerRequestResolved(phase: PhaseState, event: EvOwnerRequestResolved): PhaseState {
  const request = phase.ownerRequests.find((r) => r.id === event.requestId)!;
  const ownerRequests = phase.ownerRequests.map((r) =>
    r.id === event.requestId
      ? {
          ...r,
          status: "resolved" as const,
          resolution: { option: event.option, note: event.note },
          resolvedBinding: { candidateSha: event.boundCandidateSha, contractVersion: event.boundContractVersion },
        }
      : r,
  );
  let next: PhaseState = { ...phase, ownerRequests };

  // Every option has a defined effect (round-3 review item 1) — none is a
  // no-op. "budget.granted by owner request <id>": the allowance is granted
  // exactly like a correction's, independent of the exhausted budget.
  if (isBudgetGateRequest(request) && event.option === "grant") {
    next = { ...next, repairRoundsGranted: next.repairRoundsGranted + 3 };
  }
  if (isRepairForcingOption(request.origin, event.option)) {
    next = { ...next, repairRoundsGranted: next.repairRoundsGranted + 3 };
  }
  if (request.origin === "open_finding" && event.option === "accept_risk" && request.linkedFindingId) {
    // design §4.2: equivalent to accept-finding, with the required scope note.
    next = {
      ...next,
      findings: next.findings.map((f) =>
        f.id === request.linkedFindingId ? { ...f, status: "accepted" as const, acceptedScope: event.note } : f,
      ),
    };
  }
  if (request.origin === "unaddressed_correction" && event.option === "withdraw" && request.linkedCorrectionId) {
    // design §7.5: withdrawn by the owner only — it no longer gates accept().
    next = {
      ...next,
      corrections: next.corrections.map((c) =>
        c.id === request.linkedCorrectionId ? { ...c, status: "withdrawn" as const } : c,
      ),
    };
  }
  // "accept_as_implemented" (failed_vote) and "approve" (reserved_decision)
  // need no further mutation here: decisionSettled (predicate.ts) reads the
  // resolved request's own `resolution.option` directly.
  return next;
}

// --- FINDING_ACCEPTED_BY_OWNER -----------------------------------------

type EvFindingAcceptedByOwner = Extract<Event, { type: "FINDING_ACCEPTED_BY_OWNER" }>;

export function checkFindingAcceptedByOwner(phase: PhaseState, event: EvFindingAcceptedByOwner): CommandCheck {
  if (event.by !== "owner") return { ok: false, reason: `only the owner may accept a finding, not '${String(event.by)}'` };
  const finding = phase.findings.find((f) => f.id === event.findingId);
  if (!finding) return { ok: false, reason: `unknown finding ${event.findingId}` };
  const tuple = bindingTuple(phase, event.findingId, event.boundCandidateSha, event.boundContractVersion, event.boundRecordVersion);
  return checkTupleBinding(tuple, phase);
}

export function applyFindingAcceptedByOwner(phase: PhaseState, event: EvFindingAcceptedByOwner): PhaseState {
  const findings = phase.findings.map((f) =>
    f.id === event.findingId ? { ...f, status: "accepted" as const, acceptedScope: event.scope } : f,
  );
  const withFinding = { ...phase, findings };
  return autoResolveLinkedRequest(withFinding, event.findingId, "accepted", event.boundCandidateSha, event.boundContractVersion);
}

// --- OVERRIDE_CAST -------------------------------------------------------

type EvOverrideCast = Extract<Event, { type: "OVERRIDE_CAST" }>;

export function checkOverrideCast(phase: PhaseState, event: EvOverrideCast): CommandCheck {
  const decision = phase.decisions.find((d) => d.id === event.override.decisionId);
  if (!decision) return { ok: false, reason: `decision ${event.override.decisionId} does not exist` };
  if (!phase.candidate) return { ok: false, reason: `phase ${phase.phaseId} has no frozen candidate yet` };
  return checkBinding(
    {
      candidateSha: event.override.boundCandidateSha,
      contractVersion: event.override.boundContractVersion,
      recordVersion: event.override.boundRecordVersion,
    },
    { candidateSha: phase.candidate.sha, contractVersion: phase.contract.contractVersion, recordVersion: decision.version },
    `decision ${decision.id}`,
  );
}

export function applyOverrideCast(phase: PhaseState, event: EvOverrideCast): PhaseState {
  const withOverride = { ...phase, overrides: [...phase.overrides, event.override] };
  return autoResolveLinkedRequest(
    withOverride,
    event.override.decisionId,
    "override",
    event.override.boundCandidateSha,
    event.override.boundContractVersion,
  );
}

// --- Routing out of AWAITING_OWNER --------------------------------------

/** What the phase should do once a command has just been applied while it
 * was AWAITING_OWNER: stay parked if any owner request is still open;
 * otherwise resume — REPAIRING if an open correction still needs a round,
 * RESOLVING if the candidate's checks/probe/reviews are already valid
 * (guaranteed once a candidate exists, since AWAITING_OWNER's record-level
 * requests only ever arise from RESOLVING), or AWAITING_OWNER defensively
 * if there is no candidate at all (should not happen for these three
 * commands, which all require a record that itself requires a candidate). */
export type AwaitingOwnerTarget = "AWAITING_OWNER" | "REPAIRING" | "RESOLVING";

export function awaitingOwnerTarget(phase: PhaseState): AwaitingOwnerTarget {
  if (phase.ownerRequests.some((r) => r.status === "open")) return "AWAITING_OWNER";
  if (phase.corrections.some((c) => c.status === "open")) return "REPAIRING";
  if (phase.candidate) return "RESOLVING";
  return "AWAITING_OWNER";
}
