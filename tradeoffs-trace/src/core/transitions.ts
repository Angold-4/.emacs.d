// design §6.1 transcribed as a DATA table, one row per transition, plus the
// §8.1 outcomes that change phase state, interrupted gates (§9.3), publish
// CAS (§6.4 step 3), amend (§7.3) and revise (§7.5). This table is the
// normative version of design §6.1 (plan D0-2): reduce.ts and next.ts only
// ever consult it, and test/contract/transitions.test.ts checks it against
// a hard-coded transcription of the design text, AND against next.ts (every
// row's `actions` must equal `next()` of the state the row produces), so
// the document, the reducer and the dispatcher cannot drift apart silently.
//
// Each row is data: `from`/`axis` select which rows are even candidates for
// a given state, `trigger` is the event type that fires the row, `guard`
// distinguishes rows that share a trigger, and `apply` computes the new
// state. `actions` is what `next()` of the resulting state must equal.

import {
  applyFindingAcceptedByOwner,
  applyOverrideCast,
  applyOwnerRequestResolved,
  autoResolveLinkedRequest,
  awaitingOwnerTarget,
  checkFindingAcceptedByOwner,
  checkOverrideCast,
  checkOwnerRequestResolved,
} from "./owner-commands.ts";
import { isBudgetGateRequest, isRepairForcingOption, openItemOwnerRequestsFor } from "./owner-requests.ts";
import { accept, resolvedCorrectionIdsFor, reviewsComplete, sameVersion } from "./predicate.ts";
import type {
  Correction,
  Decision,
  Event,
  Finding,
  InFlightKey,
  PhaseState,
  PhaseStateName,
  Review,
  RunStateName,
  State,
} from "./types.ts";

export type Axis = "phase" | "run";

export interface TransitionRow {
  id: string; // stable name, used by tests to check §6.1 coverage
  axis: Axis;
  from: PhaseStateName | RunStateName;
  trigger: Event["type"];
  guardName: string;
  guard: (state: State, event: Event) => boolean;
  to: PhaseStateName | RunStateName;
  actions: { type: string; [key: string]: unknown }[]; // must equal next() of the resulting state
  apply: (state: State, event: Event) => State;
}

function currentOf(state: State, axis: Axis): PhaseStateName | RunStateName {
  return axis === "phase" ? state.phase.phase : state.run;
}

function withPhase(state: State, patch: Partial<PhaseState>): State {
  return { ...state, phase: { ...state.phase, ...patch } };
}

/** Clear one or more in-flight entries (design §9.3: a completion event
 * clears the intent it completes; REVISE's cancel_in_flight clears all of
 * them at once — see the revise rows below). */
function clearInFlight(phase: PhaseState, ...keys: InFlightKey[]): PhaseState["inFlight"] {
  const next = { ...phase.inFlight };
  for (const k of keys) delete next[k];
  return next;
}

function budgetRemains(state: State): boolean {
  return state.phase.repairRoundsUsed < state.phase.repairRoundsGranted;
}

function budgetExhausted(state: State): boolean {
  return !budgetRemains(state);
}

function acceptHolds(s: State): boolean {
  return Boolean(s.phase.candidate) && accept(s.phase, s.phase.candidate!.sha, s.phase.contract.contractVersion);
}

function allThreeReviewsPresent(state: State, upcoming?: Review): boolean {
  const r = state.phase.reviews;
  const has = (slot: typeof r.M, who: "M" | "A" | "B") => Boolean(slot?.review) || upcoming?.reviewer === who;
  return has(r.M, "M") && has(r.A, "A") && has(r.B, "B");
}

function activePhaseStates(): PhaseStateName[] {
  return [
    "IMPLEMENTING",
    "FREEZING",
    "CHECKING",
    "PROBING",
    "REVIEWING",
    "RESOLVING",
    "ACCEPTED",
    "AWAITING_OWNER",
    "BLOCKED",
  ];
}

const rows: TransitionRow[] = [];

function addRow(row: TransitionRow) {
  rows.push(row);
}

// --- READY -> IMPLEMENTING ---------------------------------------------
addRow({
  id: "start-attempt",
  axis: "phase",
  from: "READY",
  trigger: "ATTEMPT_STARTED",
  guardName: "always",
  guard: () => true,
  to: "IMPLEMENTING",
  actions: [{ type: "dispatch_worker" }], // next() of the resulting IMPLEMENTING state
  apply: (s) => withPhase(s, { phase: "IMPLEMENTING" }),
});

// --- IMPLEMENTING --------------------------------------------------------
addRow({
  id: "submit-phase",
  axis: "phase",
  from: "IMPLEMENTING",
  trigger: "SUBMIT_PHASE",
  guardName: "always",
  guard: () => true,
  to: "FREEZING",
  actions: [{ type: "freeze" }],
  apply: (s) =>
    withPhase(s, {
      phase: "FREEZING",
      inFlight: clearInFlight(s.phase, "dispatch_worker"),
    }),
});

/** design §4.2/§5.2/§8.1: every entry into AWAITING_OWNER records the owner
 * requests for whatever is open (see owner-requests.ts); `cause` names the
 * gate that failed, for the fallback request when nothing record-level
 * caused it (e.g. every worker attempt just kept timing out). */
function enterAwaitingOwner(s: State, cause: string): State {
  const requests = openItemOwnerRequestsFor(s.phase, cause);
  return withPhase(s, { phase: "AWAITING_OWNER", ownerRequests: [...s.phase.ownerRequests, ...requests] });
}

function failureRows(
  idPrefix: string,
  from: PhaseStateName,
  trigger: Event["type"],
  repairActions: { type: string; [key: string]: unknown }[],
  cause: string,
  extraApply?: (s: State) => State,
) {
  addRow({
    id: `${idPrefix}-to-repairing`,
    axis: "phase",
    from,
    trigger,
    guardName: "budgetRemains",
    guard: (s) => budgetRemains(s),
    to: "REPAIRING",
    actions: repairActions,
    apply: (s) => {
      const base = withPhase(s, { phase: "REPAIRING" });
      return extraApply ? extraApply(base) : base;
    },
  });
  addRow({
    id: `${idPrefix}-budget-exhausted`,
    axis: "phase",
    from,
    trigger,
    guardName: "budgetExhausted",
    guard: (s) => budgetExhausted(s),
    to: "AWAITING_OWNER",
    actions: [],
    apply: (s) => {
      const base = extraApply ? extraApply(s) : s;
      return enterAwaitingOwner(base, cause);
    },
  });
}

const REPAIR_ATTEMPT_ACTIONS = [{ type: "repair_attempt_started" }];

failureRows(
  "attempt-timed-out",
  "IMPLEMENTING",
  "ATTEMPT_TIMED_OUT",
  REPAIR_ATTEMPT_ACTIONS,
  "worker attempts kept timing out",
  (s) => withPhase(s, { inFlight: clearInFlight(s.phase, "dispatch_worker") }),
);
failureRows(
  "attempt-no-submission",
  "IMPLEMENTING",
  "ATTEMPT_NO_SUBMISSION",
  REPAIR_ATTEMPT_ACTIONS,
  "the worker never called submit_phase",
  (s) => withPhase(s, { inFlight: clearInFlight(s.phase, "dispatch_worker") }),
);

addRow({
  id: "attempt-interrupted",
  axis: "phase",
  from: "IMPLEMENTING",
  trigger: "ATTEMPT_INTERRUPTED",
  guardName: "always",
  guard: () => true,
  to: "IMPLEMENTING",
  // §9.3: interrupted clears the intent but leaves the obligation, so
  // next() re-emits dispatch_worker ("new attempt on the same session").
  actions: [{ type: "dispatch_worker" }],
  apply: (s) =>
    withPhase(s, {
      phase: "IMPLEMENTING",
      attempt: { ...s.phase.attempt, interrupted: true },
      inFlight: clearInFlight(s.phase, "dispatch_worker"),
    }),
});

// --- FREEZING --------------------------------------------------------
addRow({
  id: "freeze-completed",
  axis: "phase",
  from: "FREEZING",
  trigger: "FREEZE_COMPLETED",
  guardName: "always",
  guard: () => true,
  to: "CHECKING",
  // Canonical candidateSha "C1" — the matching BUILD fixture in
  // test/contract/transitions.test.ts freezes exactly this candidate, so
  // next() of the resulting state reproduces this literally.
  actions: [{ type: "run_checks", candidateSha: "C1" }],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "FREEZE_COMPLETED" }>;
    return withPhase(s, {
      phase: "CHECKING",
      candidate: { sha: e.candidateSha, contractVersion: s.phase.contract.contractVersion },
      checks: undefined,
      probe: undefined,
      reviews: {},
      ballots: [],
      overrides: [],
      inFlight: clearInFlight(s.phase, "freeze"),
    });
  },
});

failureRows(
  "freeze-timed-out",
  "FREEZING",
  "FREEZE_TIMED_OUT",
  REPAIR_ATTEMPT_ACTIONS,
  "the freeze kept timing out",
  (s) => withPhase(s, { worktreeTainted: true, inFlight: clearInFlight(s.phase, "freeze") }),
);

addRow({
  id: "freeze-interrupted",
  axis: "phase",
  from: "FREEZING",
  trigger: "FREEZE_INTERRUPTED",
  guardName: "always",
  guard: () => true,
  to: "FREEZING",
  actions: [{ type: "freeze" }],
  apply: (s) => withPhase(s, { inFlight: clearInFlight(s.phase, "freeze") }),
});

// --- CHECKING --------------------------------------------------------
addRow({
  id: "checks-passed",
  axis: "phase",
  from: "CHECKING",
  trigger: "CHECKS_PASSED",
  guardName: "always",
  guard: () => true,
  to: "PROBING",
  // Canonical candidateSha "C1", integrationHead "H0" (see the row's fixture).
  actions: [{ type: "dispatch_probe", candidateSha: "C1", head: "H0" }],
  apply: (s) =>
    withPhase(s, {
      phase: "PROBING",
      checks: { candidateSha: s.phase.candidate!.sha, passed: true },
      inFlight: clearInFlight(s.phase, "run_checks"),
    }),
});

failureRows("checks-failed", "CHECKING", "CHECKS_FAILED", REPAIR_ATTEMPT_ACTIONS, "the checks kept failing", (s) =>
  withPhase(s, {
    checks: { candidateSha: s.phase.candidate!.sha, passed: false },
    inFlight: clearInFlight(s.phase, "run_checks"),
  }),
);

addRow({
  id: "checks-interrupted",
  axis: "phase",
  from: "CHECKING",
  trigger: "CHECKS_INTERRUPTED",
  guardName: "always",
  guard: () => true,
  to: "CHECKING",
  actions: [{ type: "run_checks", candidateSha: "C1" }],
  apply: (s) =>
    withPhase(s, {
      checks: { candidateSha: s.phase.candidate!.sha, interrupted: true },
      inFlight: clearInFlight(s.phase, "run_checks"),
    }),
});

// --- PROBING --------------------------------------------------------
// PROBE_PASSED normally moves to REVIEWING to collect the three reviews.
// But a probe can also succeed a SECOND time for the SAME candidate: design
// §6.4 step 3's stale-publish retry re-probes against a new head without
// touching checks or reviews (only the probe is redone). If M, A and B
// already each have a review bound to (C, K) from before, there is nothing
// left for REVIEWING to dispatch, so the phase goes straight to RESOLVING
// instead of visiting a REVIEWING state next() can do nothing more with —
// discovered by the no-circularity property test (round-1 review item 2).
function applyProbePassed(s: State, ev: Event, targetPhase: PhaseStateName): State {
  const e = ev as Extract<Event, { type: "PROBE_PASSED" }>;
  const findings = s.phase.findings.map((f: Finding) =>
    f.kind === "integration" && f.status === "open"
      ? { ...f, status: "repaired" as const, repairedByCandidateSha: s.phase.candidate!.sha }
      : f,
  );
  return withPhase(s, {
    phase: targetPhase,
    probe: { candidateSha: s.phase.candidate!.sha, head: s.phase.integrationHead, probedI: e.probedI, passed: true },
    findings,
    inFlight: clearInFlight(s.phase, "dispatch_probe"),
  });
}

addRow({
  id: "probe-passed-needs-review",
  axis: "phase",
  from: "PROBING",
  trigger: "PROBE_PASSED",
  guardName: "reviewsIncomplete",
  guard: (s) => !reviewsComplete(s.phase, s.phase.candidate!.sha, s.phase.contract.contractVersion),
  to: "REVIEWING",
  actions: [
    { type: "dispatch_review", reviewer: "M" },
    { type: "dispatch_review", reviewer: "A" },
    { type: "dispatch_review", reviewer: "B" },
  ],
  apply: (s, ev) => applyProbePassed(s, ev, "REVIEWING"),
});

addRow({
  id: "probe-passed-reviews-already-valid",
  axis: "phase",
  from: "PROBING",
  trigger: "PROBE_PASSED",
  guardName: "reviewsAlreadyComplete",
  guard: (s) => reviewsComplete(s.phase, s.phase.candidate!.sha, s.phase.contract.contractVersion),
  to: "RESOLVING",
  // Canonical fixture: checks/reviews already valid and no open items, so
  // accept(C, K) holds immediately.
  actions: [{ type: "accept", resolvedCorrectionIds: [] }],
  apply: (s, ev) => applyProbePassed(s, ev, "RESOLVING"),
});

failureRows("probe-failed", "PROBING", "PROBE_FAILED", REPAIR_ATTEMPT_ACTIONS, "the integration probe kept failing", (s) => {
  const finding: Finding = {
    id: `F-${s.phase.phaseId}-integration-${s.phase.findings.length + 1}`,
    version: 1,
    phaseId: s.phase.phaseId,
    kind: "integration",
    severity: "blocking",
    evidence: "integration probe failed",
    raisedBy: "conductor",
    status: "open",
    boundCandidateSha: s.phase.candidate!.sha,
  };
  return withPhase(s, {
    probe: { candidateSha: s.phase.candidate!.sha, head: s.phase.integrationHead, passed: false },
    findings: [...s.phase.findings, finding],
    inFlight: clearInFlight(s.phase, "dispatch_probe"),
  });
});

addRow({
  id: "probe-interrupted",
  axis: "phase",
  from: "PROBING",
  trigger: "PROBE_INTERRUPTED",
  guardName: "always",
  guard: () => true,
  to: "PROBING",
  actions: [{ type: "dispatch_probe", candidateSha: "C1", head: "H0" }],
  apply: (s) =>
    withPhase(s, {
      probe: { candidateSha: s.phase.candidate!.sha, head: s.phase.integrationHead, interrupted: true },
      inFlight: clearInFlight(s.phase, "dispatch_probe"),
    }),
});

// --- REVIEWING --------------------------------------------------------
addRow({
  id: "review-submitted-incomplete",
  axis: "phase",
  from: "REVIEWING",
  trigger: "REVIEW_SUBMITTED",
  guardName: "reviewIncomplete",
  guard: (s, ev) => {
    const e = ev as Extract<Event, { type: "REVIEW_SUBMITTED" }>;
    return !allThreeReviewsPresent(s, e.review);
  },
  to: "REVIEWING",
  // Fixture submits M; A and B are still missing and not in flight.
  actions: [
    { type: "dispatch_review", reviewer: "A" },
    { type: "dispatch_review", reviewer: "B" },
  ],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "REVIEW_SUBMITTED" }>;
    const key = `review_${e.review.reviewer}` as InFlightKey;
    return withPhase(s, {
      reviews: { ...s.phase.reviews, [e.review.reviewer]: { review: e.review } },
      inFlight: clearInFlight(s.phase, key),
    });
  },
});

addRow({
  id: "review-submitted-complete",
  axis: "phase",
  from: "REVIEWING",
  trigger: "REVIEW_SUBMITTED",
  guardName: "reviewComplete",
  guard: (s, ev) => {
    const e = ev as Extract<Event, { type: "REVIEW_SUBMITTED" }>;
    return allThreeReviewsPresent(s, e.review);
  },
  to: "RESOLVING",
  // Fixture also has checks/probe already passed and no open items, so
  // accept(C, K) holds once all three reviews are in.
  actions: [{ type: "accept", resolvedCorrectionIds: [] }],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "REVIEW_SUBMITTED" }>;
    const key = `review_${e.review.reviewer}` as InFlightKey;
    return withPhase(s, {
      phase: "RESOLVING",
      reviews: { ...s.phase.reviews, [e.review.reviewer]: { review: e.review } },
      inFlight: clearInFlight(s.phase, key),
    });
  },
});

addRow({
  id: "review-timed-out-first",
  axis: "phase",
  from: "REVIEWING",
  trigger: "REVIEW_TIMED_OUT",
  guardName: "firstTimeout",
  guard: (s, ev) => {
    const e = ev as Extract<Event, { type: "REVIEW_TIMED_OUT" }>;
    return !s.phase.reviews[e.reviewer]?.timedOutOnce;
  },
  to: "REVIEWING",
  // Fixture starts with all three reviews missing; after the timeout, all
  // three are still missing (the timed-out one has no review, just the
  // flag), so next() re-dispatches all three.
  actions: [
    { type: "dispatch_review", reviewer: "M" },
    { type: "dispatch_review", reviewer: "A" },
    { type: "dispatch_review", reviewer: "B" },
  ],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "REVIEW_TIMED_OUT" }>;
    const key = `review_${e.reviewer}` as InFlightKey;
    return withPhase(s, {
      reviews: {
        ...s.phase.reviews,
        [e.reviewer]: { ...s.phase.reviews[e.reviewer], timedOutOnce: true },
      },
      inFlight: clearInFlight(s.phase, key),
    });
  },
});

addRow({
  id: "review-unavailable",
  axis: "phase",
  from: "REVIEWING",
  // Same trigger as review-timed-out-first: reduce (not the caller) decides
  // "redispatch once" vs "unavailable" from the stored `timedOutOnce` flag,
  // so nothing outside the core chooses which event type to send.
  trigger: "REVIEW_TIMED_OUT",
  guardName: "alreadyTimedOut",
  guard: (s, ev) => {
    const e = ev as Extract<Event, { type: "REVIEW_TIMED_OUT" }>;
    return Boolean(s.phase.reviews[e.reviewer]?.timedOutOnce);
  },
  to: "BLOCKED",
  actions: [],
  apply: (s) => withPhase(s, { phase: "BLOCKED", blockedReason: "reviewer unavailable" }),
});

// --- RESOLVING --------------------------------------------------------
addRow({
  id: "resolving-accept-holds",
  axis: "phase",
  from: "RESOLVING",
  trigger: "ACCEPTED",
  guardName: "acceptHoldsAndCorrectionsMatch",
  guard: (s, ev) => {
    if (!acceptHolds(s)) return false;
    const e = ev as Extract<Event, { type: "ACCEPTED" }>;
    const C = s.phase.candidate!.sha;
    const K = s.phase.contract.contractVersion;
    const computed = resolvedCorrectionIdsFor(s.phase, C, K);
    const given = [...(e.resolvedCorrectionIds ?? [])].sort();
    return JSON.stringify(given) === JSON.stringify(computed);
  },
  to: "ACCEPTED",
  // Canonical integrationHead "H0", probedI "I1" (see the row's fixture).
  actions: [{ type: "publish_intent", expectedHead: "H0", candidateI: "I1" }],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "ACCEPTED" }>;
    const corrections = s.phase.corrections.map((c: Correction) =>
      e.resolvedCorrectionIds.includes(c.id) ? { ...c, status: "resolved" as const } : c,
    );
    return withPhase(s, { phase: "ACCEPTED", corrections });
  },
});

addRow({
  id: "resolving-incomplete-repair",
  axis: "phase",
  from: "RESOLVING",
  trigger: "RESOLVING_INCOMPLETE",
  guardName: "openItemsAndBudgetRemains",
  guard: (s) => !acceptHolds(s) && budgetRemains(s),
  to: "REPAIRING",
  actions: REPAIR_ATTEMPT_ACTIONS,
  apply: (s) => withPhase(s, { phase: "REPAIRING" }),
});

addRow({
  id: "resolving-incomplete-awaiting-owner",
  axis: "phase",
  from: "RESOLVING",
  trigger: "RESOLVING_INCOMPLETE",
  guardName: "openItemsBudgetExhausted",
  guard: (s) => !acceptHolds(s) && budgetExhausted(s),
  to: "AWAITING_OWNER",
  actions: [],
  apply: (s) => enterAwaitingOwner(s, "the repair budget ran out while items remained open"),
});

// --- ACCEPTED / PUBLISHING --------------------------------------------
addRow({
  id: "accepted-publish-intent",
  axis: "phase",
  from: "ACCEPTED",
  trigger: "PUBLISH_INTENT",
  guardName: "always",
  guard: () => true,
  to: "PUBLISHING",
  actions: [{ type: "publish_cas", expectedHead: "H0", candidateI: "I1" }],
  apply: (s) => withPhase(s, { phase: "PUBLISHING" }),
});

addRow({
  id: "publish-completed",
  axis: "phase",
  from: "PUBLISHING",
  trigger: "PUBLISH_COMPLETED",
  guardName: "casSucceeded",
  guard: () => true, // reduce only receives this event once the effect layer performed a successful CAS
  to: "DONE",
  actions: [],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "PUBLISH_COMPLETED" }>;
    return withPhase(s, { phase: "DONE", publishedI: e.newHead, inFlight: clearInFlight(s.phase, "publish_cas") });
  },
});

addRow({
  id: "publish-stale",
  axis: "phase",
  from: "PUBLISHING",
  trigger: "PUBLISH_STALE",
  guardName: "casFailed",
  guard: () => true, // reduce only receives this event once the effect layer observed a stale head
  to: "PROBING",
  // Canonical candidateSha "C1" (unchanged), new head "H1" (the fixture's actualHead).
  actions: [{ type: "dispatch_probe", candidateSha: "C1", head: "H1" }],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "PUBLISH_STALE" }>;
    return withPhase(s, {
      phase: "PROBING",
      integrationHead: e.actualHead,
      probe: undefined,
      inFlight: clearInFlight(s.phase, "publish_cas"),
    });
  },
});

// --- REPAIRING --------------------------------------------------------
addRow({
  id: "repair-attempt-started",
  axis: "phase",
  from: "REPAIRING",
  trigger: "REPAIR_ATTEMPT_STARTED",
  guardName: "budgetRemains",
  guard: (s) => budgetRemains(s),
  to: "IMPLEMENTING",
  actions: [{ type: "dispatch_worker" }],
  apply: (s) =>
    withPhase(s, {
      phase: "IMPLEMENTING",
      repairRoundsUsed: s.phase.repairRoundsUsed + 1,
      attempt: { n: s.phase.attempt.n + 1 },
    }),
});

addRow({
  id: "repairing-budget-exhausted",
  axis: "phase",
  from: "REPAIRING",
  trigger: "REPAIR_BUDGET_EXHAUSTED",
  guardName: "budgetExhausted",
  guard: (s) => budgetExhausted(s),
  to: "AWAITING_OWNER",
  actions: [],
  apply: (s) => enterAwaitingOwner(s, "the repair budget ran out"),
});

// --- amend (§7.3): evidence invalidated, back to CHECKING under new K ---
// Bound to the contract version it replaces (design §7.4's "Bound to"
// column) — a stale AMEND (someone else already amended) is rejected.
// Round-3 review item 2: for a `contract` finding, the owner's disposition
// IS amending the contract (design §4.2/§4.3), so AMEND must work from
// AWAITING_OWNER while requests are still open — it may carry
// `resolvesFindingIds` (validated in reduce.ts's pre-check), which are
// marked accepted, and every open request bound to the replaced contract
// version is closed as superseded, to be re-evaluated under the new K.
function applyAmend(s: State, ev: Event): State {
  const e = ev as Extract<Event, { type: "AMEND" }>;
  const resolves = new Set(e.resolvesFindingIds ?? []);
  const findings = s.phase.findings.map((f) =>
    resolves.has(f.id)
      ? { ...f, status: "accepted" as const, acceptedScope: `amended to v${e.newContractVersion.snapshot}` }
      : f,
  );
  const ownerRequests = s.phase.ownerRequests.map((r) =>
    r.status === "open" && r.boundContractVersion && sameVersion(r.boundContractVersion, e.replacingContractVersion)
      ? { ...r, status: "resolved" as const, resolution: { option: "superseded_by_amend" } }
      : r,
  );
  return withPhase(s, {
    phase: "CHECKING",
    contract: { ...s.phase.contract, contractVersion: e.newContractVersion },
    candidate: s.phase.candidate && {
      sha: s.phase.candidate.sha,
      contractVersion: e.newContractVersion,
    },
    findings,
    ownerRequests,
    checks: undefined,
    probe: undefined,
    reviews: {},
    ballots: [],
    overrides: [],
    inFlight: {},
  });
}

function hasCandidateAndCurrentContract(s: State, ev: Event): boolean {
  if (!s.phase.candidate) return false;
  const e = ev as Extract<Event, { type: "AMEND" }>;
  return sameVersion(e.replacingContractVersion, s.phase.contract.contractVersion);
}

for (const from of ["CHECKING", "PROBING", "REVIEWING", "RESOLVING", "ACCEPTED", "AWAITING_OWNER"] as PhaseStateName[]) {
  addRow({
    id: `amend-from-${from.toLowerCase()}`,
    axis: "phase",
    from,
    trigger: "AMEND",
    guardName: "hasCandidateAndCurrentContract",
    guard: hasCandidateAndCurrentContract,
    to: "CHECKING",
    actions: [{ type: "run_checks", candidateSha: "C1" }],
    apply: applyAmend,
  });
}

// --- revise (§7.5): cancel in-flight, new 3-round allowance
// Bound to the target record's version, candidate and contract (design
// §7.4's "Bound to" column: "record version, candidate, contract").
function targetRecordBindingCurrent(s: State, ev: Event): boolean {
  const e = ev as Extract<Event, { type: "REVISE" }>;
  const decision = s.phase.decisions.find((d) => d.id === e.targetRecordId);
  const finding = s.phase.findings.find((f) => f.id === e.targetRecordId);
  const recordVersion = decision?.version ?? finding?.version;
  if (recordVersion === undefined) return false; // unknown target record
  if (!s.phase.candidate) return false;
  return (
    e.boundCandidateSha === s.phase.candidate.sha &&
    sameVersion(e.boundContractVersion, s.phase.contract.contractVersion) &&
    e.boundRecordVersion === recordVersion
  );
}

/** The correction bookkeeping common to every REVISE, whatever state it
 * fires from: record the correction (allowance fixed at 3 by the core),
 * mark the target decision superseded, cancel in-flight work, and — since
 * the owner is acting directly on this record — resolve any owner request
 * that was open about it (design §6.1 item 2: a record-level command
 * addresses the request, it doesn't leave it dangling). `phase.phase` is
 * left untouched here; callers decide the destination. */
function applyReviseCorrection(s: State, ev: Event): State {
  const e = ev as Extract<Event, { type: "REVISE" }>;
  const correction: Correction = {
    id: e.correctionId,
    version: 1,
    phaseId: s.phase.phaseId,
    targetRecordId: e.targetRecordId,
    correctionText: e.correctionText,
    contractChange: e.contractChange,
    status: "open",
    boundContractVersion: s.phase.contract.contractVersion,
    grantedRounds: 3, // fixed by design (§7.5, §8.1), independent of any exhausted budget
  };
  // "The original decision is preserved and marked superseded by
  // correction C-…" (design §7.5 step 1) — only decisions carry a class
  // to supersede; findings are corrected without this marker.
  const decisions = s.phase.decisions.map((d: Decision) =>
    d.id === e.targetRecordId ? { ...d, supersededByCorrection: correction.id, version: d.version + 1 } : d,
  );
  const withCorrection = withPhase(s, {
    decisions,
    corrections: [...s.phase.corrections, correction],
    repairRoundsGranted: s.phase.repairRoundsGranted + correction.grantedRounds,
    inFlight: {}, // cancel_in_flight: a running attempt/review is cancelled (§7.5 step 2)
  });
  return {
    ...withCorrection,
    phase: autoResolveLinkedRequest(
      withCorrection.phase,
      e.targetRecordId,
      "revised",
      e.boundCandidateSha,
      e.boundContractVersion,
    ),
  };
}

for (const from of activePhaseStates().filter((s) => s !== "AWAITING_OWNER")) {
  addRow({
    id: `revise-from-${from.toLowerCase()}`,
    axis: "phase",
    from,
    trigger: "REVISE",
    guardName: "targetRecordBindingCurrent",
    guard: targetRecordBindingCurrent,
    to: "REPAIRING",
    actions: REPAIR_ATTEMPT_ACTIONS,
    apply: (s, ev) => withPhase(applyReviseCorrection(s, ev), { phase: "REPAIRING" }),
  });
}

// AWAITING_OWNER: REVISE only moves the phase once no owner request remains
// open after applying it (item 2) — the correction is still recorded
// either way, but if some OTHER open item is untouched by this revise, the
// phase stays parked until the owner clears it too.
addRow({
  id: "revise-from-awaiting-owner-clears",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "REVISE",
  guardName: "targetRecordBindingCurrentAndClears",
  guard: (s, ev) => targetRecordBindingCurrent(s, ev) && awaitingOwnerTarget(applyReviseCorrection(s, ev).phase) !== "AWAITING_OWNER",
  to: "REPAIRING",
  actions: REPAIR_ATTEMPT_ACTIONS,
  apply: (s, ev) => withPhase(applyReviseCorrection(s, ev), { phase: "REPAIRING" }),
});

addRow({
  id: "revise-from-awaiting-owner-stays",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "REVISE",
  guardName: "targetRecordBindingCurrentAndStays",
  guard: (s, ev) => targetRecordBindingCurrent(s, ev) && awaitingOwnerTarget(applyReviseCorrection(s, ev).phase) === "AWAITING_OWNER",
  to: "AWAITING_OWNER",
  actions: [],
  apply: (s, ev) => applyReviseCorrection(s, ev),
});

// --- AWAITING_OWNER: OWNER_REQUEST_RESOLVED, FINDING_ACCEPTED_BY_OWNER and
// OVERRIDE_CAST move the phase, but only once no owner request remains open
// after applying them (item 2, round 2). Otherwise the command is still
// recorded and the phase stays parked. ------------------------------------

function checkAndApply<E extends Event>(
  check: (phase: PhaseState, ev: E) => { ok: boolean },
  apply: (phase: PhaseState, ev: E) => PhaseState,
) {
  return {
    guardValid: (s: State, ev: Event) => check(s.phase, ev as E).ok,
    applyPhase: (s: State, ev: Event) => apply(s.phase, ev as E),
  };
}

const ownerRequestResolved = checkAndApply(checkOwnerRequestResolved, applyOwnerRequestResolved);
const findingAcceptedByOwner = checkAndApply(checkFindingAcceptedByOwner, applyFindingAcceptedByOwner);
const overrideCast = checkAndApply(checkOverrideCast, applyOverrideCast);

function resolvedRequestFor(s: State, ev: Event): PhaseState["ownerRequests"][number] | undefined {
  const e = ev as Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;
  return s.phase.ownerRequests.find((r) => r.id === e.requestId);
}

// OWNER_REQUEST_RESOLVED: the plain repair-budget gate request routes on
// its own two options; everything else routes generically via
// awaitingOwnerTarget once cleared.
addRow({
  id: "awaiting-owner-request-resolved-grant",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "OWNER_REQUEST_RESOLVED",
  guardName: "budgetGateGrant",
  guard: (s, ev) => {
    const request = resolvedRequestFor(s, ev);
    const e = ev as Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;
    if (!request || !isBudgetGateRequest(request) || e.option !== "grant") return false;
    if (!ownerRequestResolved.guardValid(s, ev)) return false;
    return awaitingOwnerTarget(ownerRequestResolved.applyPhase(s, ev)) !== "AWAITING_OWNER";
  },
  to: "REPAIRING",
  actions: REPAIR_ATTEMPT_ACTIONS,
  apply: (s, ev) => withPhase(s, { ...ownerRequestResolved.applyPhase(s, ev), phase: "REPAIRING" }),
});

addRow({
  id: "awaiting-owner-request-resolved-stop",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "OWNER_REQUEST_RESOLVED",
  guardName: "budgetGateStop",
  guard: (s, ev) => {
    const request = resolvedRequestFor(s, ev);
    const e = ev as Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;
    if (!request || !isBudgetGateRequest(request) || e.option !== "stop") return false;
    if (!ownerRequestResolved.guardValid(s, ev)) return false;
    return awaitingOwnerTarget(ownerRequestResolved.applyPhase(s, ev)) !== "AWAITING_OWNER";
  },
  to: "BLOCKED",
  actions: [],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;
    return withPhase(s, {
      ...ownerRequestResolved.applyPhase(s, ev),
      phase: "BLOCKED",
      blockedReason: `stopped by the owner: request ${e.requestId}`,
    });
  },
});

/** Builds the generic "clears / stays" three rows shared by
 * OWNER_REQUEST_RESOLVED (non-budget-gate), FINDING_ACCEPTED_BY_OWNER and
 * OVERRIDE_CAST: once applied, either an open item is still there (stays),
 * or none remain and the phase resumes into REPAIRING (an open correction
 * still needs a round) or RESOLVING (checks/probe/reviews already valid —
 * next() re-evaluates accept()). */
function addAwaitingOwnerRecordCommandRows(
  idPrefix: string,
  trigger: Event["type"],
  extraGuard: (s: State, ev: Event) => boolean,
  checker: { guardValid: (s: State, ev: Event) => boolean; applyPhase: (s: State, ev: Event) => PhaseState },
) {
  addRow({
    id: `${idPrefix}-clears-to-resolving`,
    axis: "phase",
    from: "AWAITING_OWNER",
    trigger,
    guardName: "clearsToResolving",
    guard: (s, ev) =>
      extraGuard(s, ev) && checker.guardValid(s, ev) && awaitingOwnerTarget(checker.applyPhase(s, ev)) === "RESOLVING",
    to: "RESOLVING",
    actions: [{ type: "accept", resolvedCorrectionIds: [] }],
    apply: (s, ev) => withPhase(s, { ...checker.applyPhase(s, ev), phase: "RESOLVING" }),
  });
  addRow({
    id: `${idPrefix}-clears-to-repairing`,
    axis: "phase",
    from: "AWAITING_OWNER",
    trigger,
    guardName: "clearsToRepairing",
    guard: (s, ev) =>
      extraGuard(s, ev) && checker.guardValid(s, ev) && awaitingOwnerTarget(checker.applyPhase(s, ev)) === "REPAIRING",
    to: "REPAIRING",
    actions: REPAIR_ATTEMPT_ACTIONS,
    apply: (s, ev) => withPhase(s, { ...checker.applyPhase(s, ev), phase: "REPAIRING" }),
  });
  addRow({
    id: `${idPrefix}-stays`,
    axis: "phase",
    from: "AWAITING_OWNER",
    trigger,
    guardName: "staysOpen",
    guard: (s, ev) =>
      extraGuard(s, ev) &&
      checker.guardValid(s, ev) &&
      awaitingOwnerTarget(checker.applyPhase(s, ev)) === "AWAITING_OWNER",
    to: "AWAITING_OWNER",
    actions: [],
    apply: (s, ev) => ({ ...s, phase: checker.applyPhase(s, ev) }),
  });
}

// Repair-forcing options ("reject it and repair", "repair", "grant [more
// rounds for this correction]" — round-3 review item 1) never settle the
// item: resolving one always grants +3 rounds and, once nothing else is
// open, goes straight to REPAIRING — never RESOLVING, since the item is
// still broken.
addRow({
  id: "awaiting-owner-request-resolved-repair-clears",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "OWNER_REQUEST_RESOLVED",
  guardName: "repairForcingClears",
  guard: (s, ev) => {
    const request = resolvedRequestFor(s, ev);
    const e = ev as Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;
    if (!request || isBudgetGateRequest(request) || !isRepairForcingOption(request.origin, e.option)) return false;
    if (!ownerRequestResolved.guardValid(s, ev)) return false;
    return !ownerRequestResolved.applyPhase(s, ev).ownerRequests.some((r) => r.status === "open");
  },
  to: "REPAIRING",
  actions: REPAIR_ATTEMPT_ACTIONS,
  apply: (s, ev) => withPhase(s, { ...ownerRequestResolved.applyPhase(s, ev), phase: "REPAIRING" }),
});

addRow({
  id: "awaiting-owner-request-resolved-repair-stays",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "OWNER_REQUEST_RESOLVED",
  guardName: "repairForcingStays",
  guard: (s, ev) => {
    const request = resolvedRequestFor(s, ev);
    const e = ev as Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;
    if (!request || isBudgetGateRequest(request) || !isRepairForcingOption(request.origin, e.option)) return false;
    if (!ownerRequestResolved.guardValid(s, ev)) return false;
    return ownerRequestResolved.applyPhase(s, ev).ownerRequests.some((r) => r.status === "open");
  },
  to: "AWAITING_OWNER",
  actions: [],
  apply: (s, ev) => ({ ...s, phase: ownerRequestResolved.applyPhase(s, ev) }),
});

addAwaitingOwnerRecordCommandRows(
  "awaiting-owner-request-resolved",
  "OWNER_REQUEST_RESOLVED",
  (s, ev) => {
    const request = resolvedRequestFor(s, ev);
    const e = ev as Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;
    return Boolean(request) && !isBudgetGateRequest(request!) && !isRepairForcingOption(request!.origin, e.option);
  },
  ownerRequestResolved,
);

addAwaitingOwnerRecordCommandRows("awaiting-owner-finding-accepted", "FINDING_ACCEPTED_BY_OWNER", () => true, findingAcceptedByOwner);

addAwaitingOwnerRecordCommandRows("awaiting-owner-override", "OVERRIDE_CAST", () => true, overrideCast);

// --- run execution budget (§8.1) --------------------------------------
addRow({
  id: "run-budget-exceeded",
  axis: "run",
  from: "RUN_ACTIVE",
  trigger: "RUN_BUDGET_EXCEEDED",
  guardName: "always",
  guard: () => true,
  to: "RUN_PAUSED_BUDGET",
  actions: [],
  apply: (s) => ({ ...s, run: "RUN_PAUSED_BUDGET" }),
});

addRow({
  id: "run-resumed",
  axis: "run",
  from: "RUN_PAUSED_BUDGET",
  trigger: "RUN_RESUMED",
  guardName: "always",
  guard: () => true,
  to: "RUN_ACTIVE",
  // The fixture resumes with the phase at READY, so next() recommends
  // start_attempt again once the run is unpaused.
  actions: [{ type: "start_attempt" }],
  apply: (s) => ({ ...s, run: "RUN_ACTIVE" }),
});

export const TRANSITIONS: readonly TransitionRow[] = rows;

export function rowsFor(state: State, trigger: Event["type"]): TransitionRow[] {
  return TRANSITIONS.filter((r) => currentOf(state, r.axis) === r.from && r.trigger === trigger);
}

export { budgetRemains, budgetExhausted, acceptHolds, allThreeReviewsPresent, clearInFlight };
