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

import { carryBallotsForward, carryDecisionsForward } from "./rounds.ts";
import { leaderOf, seatsOf } from "./seats.ts";
import { finalCheckOf } from "./checks.ts";
import { gateCommandOf } from "./gate.ts";
import {
  applyFindingAcceptedByOwner,
  applyItemCarried,
  applyOverrideCast,
  applyOwnerRequestResolved,
  autoResolveLinkedRequest,
  awaitingOwnerTarget,
  checkFindingAcceptedByOwner,
  checkItemCarried,
  checkOverrideCast,
  checkOwnerRequestResolved,
} from "./owner-commands.ts";
import { isBudgetGateRequest, isRepairForcingOption, openItemOwnerRequestsFor } from "./owner-requests.ts";
import {
  accept,
  blockerWithOutcome,
  blockersNeedingPanel,
  DEFAULT_BLOCKER_OPTIONS,
  evidenceAllRecorded,
  evidenceOnlyPending,
  evaluationSettled,
  isLiveDecision,
  pendingEvidenceItems,
  resolvedCorrectionIdsFor,
  reviewsComplete,
  sameVersion,
} from "./predicate.ts";
import type {
  Correction,
  Decision,
  Event,
  Finding,
  InFlightKey,
  Message,
  OwnerRequest,
  PanelState,
  RoundPanelState,
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

/** Plan 06j (A3): a recheck is available only for the current candidate whose
 * own checks just failed. A passed check, an interrupted run or a newer
 * candidate (the recorded check sha no longer matches) refuses it. */
function recheckAvailable(s: State): boolean {
  const candidate = s.phase.candidate;
  const checks = s.phase.checks;
  return Boolean(candidate && checks && checks.passed === false && checks.interrupted !== true && checks.candidateSha === candidate.sha);
}

/** Plan 06j (A3): a round recheck and a final recheck re-run the SAME tier the
 * failed run used. The two rows differ only in the state they enter, so the
 * conductor re-runs the phase's checks (`round`) or the phase's checks plus
 * the final check (`final`). */
function recheckAvailableRound(s: State): boolean {
  return recheckAvailable(s) && s.phase.checks?.tier !== "final";
}

function recheckAvailableFinal(s: State): boolean {
  return recheckAvailable(s) && s.phase.checks?.tier === "final";
}

/** Plan 06j (A3): the one mutation a recheck applies, whatever tier it enters:
 * keep the failed record (and its tier) for the re-run, answer the budget-gate
 * request the failure parked with (a recheck is a check run, not a grant), and
 * clear the stage's in-flight entries. */
function applyRecheck(s: State, ev: Event, to: "CHECKING" | "FINAL_CHECKING"): State {
  const e = ev as Extract<Event, { type: "RECHECK_REQUESTED" }>;
  return withPhase(s, {
    phase: to,
    checks: s.phase.checks,
    ownerRequests: s.phase.ownerRequests.map((r) =>
      r.status === "open" && isBudgetGateRequest(r)
        ? { ...r, status: "resolved" as const, resolution: { option: "recheck", note: e.reason } }
        : r,
    ),
    inFlight: clearInFlight(s.phase, "run_checks", "run_final_checks"),
  });
}

function allThreeReviewsPresent(state: State, upcoming?: Review): boolean {
  const r = state.phase.reviews;
  const has = (who: string) => Boolean(r[who]?.review) || upcoming?.reviewer === who;
  return seatsOf(state.phase.contract).every(has);
}

function activePhaseStates(): PhaseStateName[] {
  return [
    "IMPLEMENTING",
    "FREEZING",
    "CHECKING",
    "PROBING",
    "REVIEWING",
    "EVALUATING",
    "RESOLVING",
    "GATING",
    "ACCEPTED",
    "AWAITING_OWNER",
    "BLOCKED",
  ];
}

const rows: TransitionRow[] = [];

function addRow(row: TransitionRow) {
  rows.push(row);
}

// --- READY -> BASELINE | IMPLEMENTING -----------------------------------
// Plan 04a: the base baseline is a real state, not work hidden inside the
// first `dispatch_worker`. `baselineNeeded` is decided by the conductor (a
// baseline already recorded for this exact base tree and check list skips
// the stage — plan 01e's reuse rule, which reduce() cannot see).
function baselineNeeded(ev: Event): boolean {
  return (ev as Extract<Event, { type: "ATTEMPT_STARTED" }>).baselineNeeded === true;
}

addRow({
  id: "start-attempt",
  axis: "phase",
  from: "READY",
  trigger: "ATTEMPT_STARTED",
  guardName: "noBaselineNeeded",
  guard: (_s, ev) => !baselineNeeded(ev),
  to: "IMPLEMENTING",
  actions: [{ type: "dispatch_worker" }], // next() of the resulting IMPLEMENTING state
  // OD-2 A2: coverage is per candidate/attempt; a new attempt owes its own.
  apply: (s) => withPhase(s, { phase: "IMPLEMENTING", coverage: undefined, coverageAttempt: undefined }),
});

addRow({
  id: "start-baseline",
  axis: "phase",
  from: "READY",
  trigger: "ATTEMPT_STARTED",
  guardName: "baselineNeeded",
  guard: (_s, ev) => baselineNeeded(ev),
  to: "BASELINE",
  actions: [{ type: "run_baseline" }], // next() of the resulting BASELINE state
  apply: (s) => withPhase(s, { phase: "BASELINE" }),
});

// --- BASELINE (plan 04a) -------------------------------------------------
// Whatever the outcome, the worker's attempt begins after it: a baseline
// that could not be taken leaves the checks strict, never wedges the run.
// The worker's own attempt deadline starts at its launch, not here.
addRow({
  id: "baseline-completed",
  axis: "phase",
  from: "BASELINE",
  trigger: "BASELINE_COMPLETED",
  guardName: "always",
  guard: () => true,
  to: "IMPLEMENTING",
  actions: [{ type: "dispatch_worker" }],
  apply: (s) => withPhase(s, { phase: "IMPLEMENTING", coverage: undefined, coverageAttempt: undefined, inFlight: clearInFlight(s.phase, "run_baseline") }),
});

addRow({
  id: "baseline-timed-out",
  axis: "phase",
  from: "BASELINE",
  trigger: "BASELINE_TIMED_OUT",
  guardName: "always",
  guard: () => true,
  to: "IMPLEMENTING",
  actions: [{ type: "dispatch_worker" }],
  apply: (s) => withPhase(s, { phase: "IMPLEMENTING", coverage: undefined, coverageAttempt: undefined, inFlight: clearInFlight(s.phase, "run_baseline") }),
});

addRow({
  id: "baseline-interrupted",
  axis: "phase",
  from: "BASELINE",
  trigger: "BASELINE_INTERRUPTED",
  guardName: "firstInterruption",
  // Re-dispatched once; the conductor emits BASELINE_TIMED_OUT on a second
  // loss (it reads `baseline.interruptedOnce` before choosing the event).
  guard: () => true,
  to: "BASELINE",
  actions: [{ type: "run_baseline" }],
  apply: (s) =>
    withPhase(s, {
      inFlight: clearInFlight(s.phase, "run_baseline"),
      baseline: { ...(s.phase.baseline ?? {}), interruptedOnce: true },
    }),
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
  extraApply?: (s: State, ev: Event) => State,
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
    apply: (s, ev) => {
      const base = withPhase(s, { phase: "REPAIRING" });
      return extraApply ? extraApply(base, ev) : base;
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
    apply: (s, ev) => {
      const base = extraApply ? extraApply(s, ev) : s;
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
      // OD-2 A2: an interrupted attempt is re-dispatched as a new attempt, so
      // it owes its own coverage too.
      coverage: undefined,
      coverageAttempt: undefined,
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
    const carried = carryDecisionsForward(
      s.phase.decisions,
      s.phase.pendingPrior,
      e.candidateSha,
      s.phase.contract.contractVersion,
    );
    return withPhase(s, {
      phase: "CHECKING",
      candidate: { sha: e.candidateSha, contractVersion: s.phase.contract.contractVersion },
      // design §6.2/§7.1: the disclosures pending since SUBMIT_PHASE are now
      // fully assembled, bound Decision records (the conductor's job — see
      // conductor.ts's #runFreeze) and move into `decisions`; nothing stays
      // pending once a freeze completes.
      // Plan 2c: records from an earlier candidate carry forward only if the
      // worker kept or changed them (core/rounds.ts); the rest are superseded
      // and can no longer block acceptance.
      decisions: [...carried, ...e.decisions],
      // Plan 06b (OD-1 A2): a conductor-raised finding that was anchored to an
      // ITEM belongs to the candidate it was raised on. A new candidate
      // re-evaluates the item, so the old finding is superseded here; a still
      // unmet/deviating item gets a fresh one from #applyItemOutcomes.
      findings: s.phase.findings.map((f) =>
        f.status === "open" && f.raisedBy === "conductor" && f.itemId
          ? { ...f, status: "superseded" as const, supersededBy: `candidate ${e.candidateSha.slice(0, 8)} re-evaluated ${f.itemId}` }
          : f,
      ),
      round: (s.phase.round ?? 0) + 1,
      // Plan 06g (A4): the round's BASE, i.e. the candidate this one repairs
      // (round 1 keeps the phase baseline: absent here). A test that passed at
      // this base and fails in the new round is a regression; one the base
      // already failed is a persisting failure, not a regression.
      ...((s.phase.round ?? 0) >= 1
        ? { roundBaseFailures: (s.phase.checks?.failures ?? []).filter((f) => !f.loadOnly).map((f) => f.name) }
        : {}),
      checks: undefined,
      // Plan 05d: this candidate is the one that repairs the previous check
      // failure, so mark the kept split with its sha (finding A-9): the
      // reviewers of a LATER candidate are never shown a stale one.
      ...(s.phase.lastCheckFailures
        ? { lastCheckFailures: { ...s.phase.lastCheckFailures, repairedBy: e.candidateSha } }
        : {}),
      pendingDisclosures: undefined,
      pendingPrior: undefined,
      pendingDispute: undefined,
      probe: undefined,
      reviews: {},
      // Plan 06b / OD-1 A2: the previous candidate's item RESULTS are cleared
      // at each freeze (the check resolution, symbol deviations and overturns
      // belong to the candidate that just froze). The worker's COVERAGE is
      // this new candidate's own input — the reviewer prompt and the views
      // read it — so it is kept until the next worker attempt replaces it.
      checkResolution: undefined,
      archSymbolDeviations: undefined,
      overturns: undefined,
      itemChecks: undefined,
      // Plan 06i: the triage records belong to the candidate that just froze.
      // A disposition of `fix` for a finding the new candidate repaired must
      // not block acceptance for ever; the new candidate is triaged afresh.
      // (A deferral is an owner's recorded decision, not a candidate fact, so
      // it is kept.)
      triage: undefined,
      // Plan 06c: the final check's pass belongs to the candidate that just
      // froze, so a new candidate runs it again.
      finalChecksPassedFor: undefined,
      // Skill fix 5: kept decisions that passed keep their ballots.
      ballots: carryBallotsForward(
        s.phase.decisions,
        s.phase.ballots,
        s.phase.findings,
        s.phase.pendingPrior,
        s.phase.candidate?.sha,
        s.phase.contract.contractVersion,
        carried,
        e.candidateSha,
        seatsOf(s.phase.contract),
        leaderOf(s.phase.contract),
      ),
      overrides: [],
      // design §2.2: a clean freeze (no survivors this time) clears any
      // earlier taint; one that found survivors sets it, so the next
      // attempt starts from a clean checkout of this candidate.
      worktreeTainted: e.tainted ?? false,
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
      checks: { candidateSha: s.phase.candidate!.sha, passed: true, tier: "round" },
      inFlight: clearInFlight(s.phase, "run_checks"),
    }),
});

failureRows("checks-failed", "CHECKING", "CHECKS_FAILED", REPAIR_ATTEMPT_ACTIONS, "the checks kept failing", (s, ev) => {
  // Plan 05d: carry each new failing test's `reproduces alone` / `load-only`
  // classification into state, so the repair prompt and the reviewers' prompts
  // label a real regression and a flake apart (finding #35).
  const failures = (ev as Extract<Event, { type: "CHECKS_FAILED" }>).failures;
  const candidateSha = s.phase.candidate!.sha;
  return withPhase(s, {
    checks: { candidateSha, passed: false, tier: "round", ...(failures && failures.length > 0 ? { failures } : {}) },
    // Plan 05d: keep the split across the repair freeze too, so the reviewers
    // of the repaired candidate see which tests failed and how each was
    // classified (finding A-5).
    ...(failures && failures.length > 0 ? { lastCheckFailures: { candidateSha, failures } } : {}),
    inFlight: clearInFlight(s.phase, "run_checks"),
  });
});

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

// Plan 06j (A3): the owner re-runs the frozen candidate's checks because the
// failure looks like the machine. Accepted only when the current candidate's
// checks just failed (never a passed check, never a newer candidate) and the
// phase is parked with no worker running; it re-runs the SAME tier, answers
// the budget-gate request the failure parked with, and spends no round.
addRow({
  id: "recheck-requested",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "RECHECK_REQUESTED",
  guardName: "recheckAvailableRound",
  guard: (s) => recheckAvailableRound(s),
  to: "CHECKING",
  actions: [{ type: "run_checks", candidateSha: "C1" }],
  apply: (s, ev) => applyRecheck(s, ev, "CHECKING"),
});

// The same, for a candidate whose failed run was the final tier: the recheck
// enters FINAL_CHECKING so `#runChecks` runs the phase's checks plus the final
// check again.
addRow({
  id: "recheck-requested-final",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "RECHECK_REQUESTED",
  guardName: "recheckAvailableFinal",
  guard: (s) => recheckAvailableFinal(s),
  to: "FINAL_CHECKING",
  actions: [{ type: "run_final_checks", candidateSha: "C1" }],
  apply: (s, ev) => applyRecheck(s, ev, "FINAL_CHECKING"),
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
/** Plan 04b: the panel a round starts with — one entry per raw blocker
 * message raised through a reviewer's `blockers` list at the moment the
 * phase enters EVALUATING. Recording the set in STATE (not recomputing it
 * later) is what makes "which blockers need a panel" race-free: the blocker
 * evaluator may publish the message while the panel votes, and the panel
 * must still run. A blocking *finding* raised through the ordinary
 * `findings` list is not a blocker here — it keeps its pre-04b handling. */
function panelForMessages(messages: readonly Message[] | undefined): { blockers: Record<string, PanelState>; round: RoundPanelState } {
  const blockers: Record<string, PanelState> = {};
  for (const m of messages ?? []) {
    if (m.type === "blocker" && m.raisedAsBlocker === true && m.state === "raw") blockers[m.id] = {};
  }
  // Plan 05e: a fresh round panel, ready to record its seats' batched votes.
  return { blockers, round: { seats: {} } };
}

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
    // Entering EVALUATING starts a FRESH round: an earlier round's
    // `evaluation.settled` must never let the new round's raw messages skip
    // evaluation, and the panel starts from this round's own raw blockers.
    ...(targetPhase === "EVALUATING" ? { evaluation: undefined, panel: panelForMessages(s.phase.messages) } : {}),
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
  to: "EVALUATING",
  // Plan 04a: reviews were already valid, so this retry still evaluates the
  // round's raw messages first. The fixture carries none, so the predicate
  // already holds.
  actions: [{ type: "evaluation_complete" }],
  apply: (s, ev) => applyProbePassed(s, ev, "EVALUATING"),
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
  to: "EVALUATING",
  // Plan 04a: the last review no longer drives straight toward acceptance.
  // The phase evaluates the round's raw messages first; the fixture carries
  // none, so the predicate already holds and next() asks to complete.
  actions: [{ type: "evaluation_complete" }],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "REVIEW_SUBMITTED" }>;
    const key = `review_${e.review.reviewer}` as InFlightKey;
    return withPhase(s, {
      phase: "EVALUATING",
      reviews: { ...s.phase.reviews, [e.review.reviewer]: { review: e.review } },
      // A fresh round: never inherit a previous round's settled flag, and
      // start the panel from this round's own raw blockers (plan 04b).
      evaluation: undefined,
      panel: panelForMessages(s.phase.messages),
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

// --- EVALUATING (plan 04a/04b) ------------------------------------------
// One fresh evaluator per message type is dispatched from next(); each one's
// outcome is a RECORD event in reduce.ts (EVALUATOR_FINISHED, or
// EVALUATION_TIMED_OUT which publishes that type's raw messages unevaluated).
// Each raw blocker is ALSO voted by a panel of three fresh agents (also
// dispatched from next(), each seat its own logged action with a deadline);
// the panel's verdict is a RECORD event (PANEL_VOTE / PANEL_SEAT_UNAVAILABLE
// / PANEL_DECIDED).
//
// The ONLY transition out of EVALUATING is EVALUATION_COMPLETED, and only
// once `evaluationSettled` holds — every dispatched type's evaluator AND
// every raw blocker's panel. So the phase stays in EVALUATING until the LAST
// of the two settles, and EVALUATION_COMPLETED is emitted exactly once. The
// recorded panel outcome picks which of these rows that exit takes.

/** Clears every evaluation and panel dispatch left in flight at the exit.
 * (Both are normally already settled; this is defensive bookkeeping, exactly
 * as the pre-04b row cleared the evaluation keys.) */
function clearedAtEvaluationExit(phase: PhaseState): PhaseState["inFlight"] {
  const inFlight = { ...phase.inFlight };
  for (const key of Object.keys(inFlight)) {
    if (key.startsWith("dispatch_panel_")) delete inFlight[key as InFlightKey];
  }
  return clearInFlight(
    { ...phase, inFlight },
    "dispatch_evaluation_tradeoff",
    "dispatch_evaluation_finding",
    "dispatch_evaluation_blocker",
  );
}

/** Plan 04b: a `block` majority parks the phase on the owner, carrying the
 * panel's options. No repair round is spent (AWAITING_OWNER consumes
 * nothing); the owner's choice resolves the blocker message and its blocking
 * finding and resumes into REPAIRING. */
function applyPanelEscalation(s: State): State {
  const phase = s.phase;
  const K = phase.contract.contractVersion;
  const C = phase.candidate?.sha;
  const requests: OwnerRequest[] = [];
  let n = phase.ownerRequests.length;
  for (const id of blockersNeedingPanel(phase)) {
    if (phase.panel?.blockers?.[id]?.decided?.outcome !== "escalate") continue;
    if (phase.ownerRequests.some((r) => r.status === "open" && r.linkedMessageId === id)) continue;
    const decision = phase.panel.blockers![id].decided!;
    const message = (phase.messages ?? []).find((m) => m.id === id);
    n += 1;
    requests.push({
      id: `OR-${phase.phaseId}-blocker-${n}`,
      version: 1,
      phaseId: phase.phaseId,
      reason: `the blocker panel voted to stop: ${message?.title ?? id}${decision.reason ? ` — ${decision.reason}` : ""}`,
      origin: "blocker_panel",
      linkedFindingId: message?.sourceRecordId,
      linkedMessageId: id,
      boundCandidateSha: C,
      boundContractVersion: K,
      options: decision.options && decision.options.length > 0 ? decision.options : DEFAULT_BLOCKER_OPTIONS,
      status: "open",
    });
  }
  return withPhase(s, {
    phase: "AWAITING_OWNER",
    ownerRequests: [...phase.ownerRequests, ...requests],
    inFlight: clearedAtEvaluationExit(phase),
  });
}

// A majority `block`: stop for the owner, with the options to choose from.
// No repair round is used, and the blocking finding stays open until the
// owner's choice resolves it (so acceptance is blocked the whole time).
addRow({
  id: "panel-escalate",
  axis: "phase",
  from: "EVALUATING",
  trigger: "EVALUATION_COMPLETED",
  guardName: "evaluationSettledAndPanelEscalated",
  guard: (s) => evaluationSettled(s.phase) && blockerWithOutcome(s.phase, "escalate") !== undefined,
  to: "AWAITING_OWNER",
  // AWAITING_OWNER is terminal for next(): the owner must choose.
  actions: [],
  apply: applyPanelEscalation,
});

// A majority `downgrade`: the blocker becomes a blocking finding for the
// next worker attempt — one repair round, never parked. The blocking finding
// is still open and blocking, so it appears in the repair prompt, and accept()
// still refuses this candidate.
addRow({
  id: "panel-downgrade",
  axis: "phase",
  from: "EVALUATING",
  trigger: "EVALUATION_COMPLETED",
  guardName: "evaluationSettledAndPanelDowngraded",
  guard: (s) =>
    evaluationSettled(s.phase) &&
    blockerWithOutcome(s.phase, "escalate") === undefined &&
    blockerWithOutcome(s.phase, "downgrade") !== undefined,
  to: "REPAIRING",
  actions: REPAIR_ATTEMPT_ACTIONS,
  apply: (s) => withPhase(s, { phase: "REPAIRING", inFlight: clearedAtEvaluationExit(s.phase) }),
});

// The panel could not reach a verdict (a split, or two seats unavailable
// after their retry). Nothing is escalated to the owner and nothing is
// parked: the blocking finding stays effective, so RESOLVING's ordinary
// routing sends the candidate to a repair round.
addRow({
  id: "panel-incomplete",
  axis: "phase",
  from: "EVALUATING",
  trigger: "EVALUATION_COMPLETED",
  guardName: "evaluationSettledAndPanelIncomplete",
  guard: (s) =>
    evaluationSettled(s.phase) &&
    blockerWithOutcome(s.phase, "escalate") === undefined &&
    blockerWithOutcome(s.phase, "downgrade") === undefined &&
    blockerWithOutcome(s.phase, "incomplete") !== undefined,
  to: "RESOLVING",
  // Fixture has the blocker's blocking finding still open, so RESOLVING
  // routes to `resolving_incomplete` (a repair round if budget remains).
  actions: [{ type: "resolving_incomplete" }],
  apply: (s) => withPhase(s, { phase: "RESOLVING", inFlight: clearedAtEvaluationExit(s.phase) }),
});

// No blocker panel this round (no raw blocker): EVALUATION_COMPLETED means
// what it did before plan 04b.
addRow({
  id: "evaluation-completed",
  axis: "phase",
  from: "EVALUATING",
  trigger: "EVALUATION_COMPLETED",
  guardName: "evaluationSettledNoPanel",
  guard: (s) => evaluationSettled(s.phase) && blockersNeedingPanel(s.phase).length === 0,
  to: "RESOLVING",
  // Fixture is acceptable with nothing open, so next() of RESOLVING accepts.
  actions: [{ type: "accept", resolvedCorrectionIds: [] }],
  apply: (s) => withPhase(s, { phase: "RESOLVING", inFlight: clearedAtEvaluationExit(s.phase) }),
});

// Plan 06i (A1): the triage pass itself failed. A record without a
// disposition at the end of evaluation is a defect of the loop, never a pass,
// so the phase parks on the owner with the reason — it never continues to
// item outcomes or acceptance as if triage had passed.
addRow({
  id: "triage-failed",
  axis: "phase",
  from: "EVALUATING",
  trigger: "TRIAGE_FAILED",
  guardName: "always",
  guard: () => true,
  to: "AWAITING_OWNER",
  actions: [],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "TRIAGE_FAILED" }>;
    const base = withPhase(s, { inFlight: clearedAtEvaluationExit(s.phase) });
    return enterAwaitingOwner(base, `the triage pass failed: ${e.reason}`);
  },
});

// --- RESOLVING --------------------------------------------------------
/** Plan 01f: a phase whose contract declares a `:GATE:` command gates the
 * candidate before accepting it — `` accept(C, K) `` is what the *gate stage*
 * is entered on, and the ACCEPTED event is what a passing gate releases.
 * Two guards share the two events, so a gate-less phase keeps exactly the
 * pre-01f edge (RESOLVING --ACCEPTED--> ACCEPTED): */
function gateDeclared(s: State): boolean {
  return gateCommandOf(s.phase.contract) !== undefined;
}

/** The corrections the ACCEPTED event must name, checked against what
 * predicate.ts computes — the event may not invent or omit one (§6.3/§7.5
 * step 5: "the ACCEPTED event records it as resolved"). */
function acceptCorrectionsMatch(s: State, ev: Event): boolean {
  const e = ev as Extract<Event, { type: "ACCEPTED" }>;
  const C = s.phase.candidate!.sha;
  const K = s.phase.contract.contractVersion;
  const computed = resolvedCorrectionIdsFor(s.phase, C, K);
  const given = [...(e.resolvedCorrectionIds ?? [])].sort();
  return JSON.stringify(given) === JSON.stringify(computed);
}

function applyAccepted(s: State, ev: Event): State {
  const e = ev as Extract<Event, { type: "ACCEPTED" }>;
  const corrections = s.phase.corrections.map((c: Correction) =>
    e.resolvedCorrectionIds.includes(c.id) ? { ...c, status: "resolved" as const } : c,
  );
  return withPhase(s, { phase: "ACCEPTED", corrections, inFlight: clearInFlight(s.phase, "run_gate") });
}

// --- plan 01g: a passing amendment rewrites one acceptance item -------
// A `criterionDispute` becomes a `reserved` amendment decision the reviewers
// vote on like any other. A passing normal tally (M plus one of A/B) applies
// it: the wording is replaced for this phase only, the contract version
// bumps, contract findings citing the old wording are superseded, and the
// phase starts a fresh attempt so the NEXT candidate is judged against the
// new wording. The amendment itself consumes no repair round (the attempt is
// a fresh one, not a repair), and a failed amendment leaves the criterion
// unchanged and never blocks acceptance on its own.
function amendmentCitesCriterion(f: Finding, criterion: string, amendmentDecisionId: string): boolean {
  if (f.criterionDisputed === criterion) return true;
  if (f.linkedDecisionId === amendmentDecisionId) return true;
  // A `contract` finding may quote the criterion without carrying the
  // dispute marker; only a delimited verbatim quote counts, so a finding
  // that merely mentions the phrase while raising a different problem is
  // not silently retired (finding B-5).
  if (f.kind !== "contract") return false;
  const i = (f.evidence ?? "").indexOf(criterion);
  if (i === -1) return false;
  const before = i === 0 ? "" : f.evidence[i - 1];
  const after = i + criterion.length >= f.evidence.length ? "" : f.evidence[i + criterion.length];
  const delim = (c: string) => c === "" || /["'`“”‘’(\[<]/.test(c);
  return delim(before) && delim(after);
}

function criterionAmendmentReady(s: State, ev: Event): boolean {
  const e = ev as Extract<Event, { type: "CRITERION_AMENDED" }>;
  if (!s.phase.candidate) return false;
  const decision = s.phase.decisions.find((d) => d.id === e.decisionId);
  if (!decision || !decision.amendment || decision.amendment.status !== "proposed") return false;
  if (decision.boundCandidateSha !== s.phase.candidate.sha) return false;
  if (!sameVersion(decision.boundContractVersion, s.phase.contract.contractVersion)) return false;
  if (!Array.isArray(e.newAcceptance) || e.newAcceptance.length === 0 || e.newAcceptance.some((a) => typeof a !== "string" || a.length === 0)) {
    return false;
  }
  return s.phase.contract.acceptance.includes(decision.amendment.criterion);
}

function applyCriterionAmended(s: State, ev: Event): State {
  const e = ev as Extract<Event, { type: "CRITERION_AMENDED" }>;
  const decision = s.phase.decisions.find((d) => d.id === e.decisionId)!;
  const amendment = decision.amendment!;
  const decisions = s.phase.decisions.map((d) => {
    if (d.id === e.decisionId) {
      return {
        ...d,
        version: d.version + 1,
        // The applied amendment now describes the new contract version; a
        // later revert is bound to it.
        boundContractVersion: e.newContractVersion,
        amendment: { ...amendment, status: "applied" as const, appliedContractVersion: e.newContractVersion },
      };
    }
    // Another reviewer's still-proposed amendment for the SAME criterion is
    // moot once this one applies: supersede it so it is never voted or
    // applied again (finding A-1).
    if (
      d !== decision &&
      isLiveDecision(d) &&
      d.amendment?.status === "proposed" &&
      d.amendment.criterion === amendment.criterion
    ) {
      return { ...d, version: d.version + 1, supersededBy: `amendment ${amendment.id} replaced the wording` };
    }
    return d;
  });
  // Contract findings that cited the replaced wording are closed as
  // superseded — never left open to fail every later round.
  const findings = s.phase.findings.map((f) =>
    f.status === "open" && f.kind === "contract" && amendmentCitesCriterion(f, amendment.criterion, e.decisionId)
      ? { ...f, status: "superseded" as const, supersededBy: `amendment ${amendment.id} replaced the wording` }
      : f,
  );
  // Plan 06b (OD-1 R6, finding disc-B-37): a structured phase carries the
  // criterion in its requirement item too. Target the item the amendment NAMES
  // by id; fall back to matching its OLD wording. Never by list position,
  // which could retarget the wrong requirement.
  const requirements = s.phase.contract.requirements
    ? s.phase.contract.requirements.map((r) =>
        (amendment.itemId ? r.id === amendment.itemId : r.text === amendment.criterion || r.title === amendment.criterion)
          ? { ...r, title: amendment.proposedWording, text: amendment.proposedWording }
          : r,
      )
    : s.phase.contract.requirements;
  return withPhase(s, {
    phase: "IMPLEMENTING",
    contract: { ...s.phase.contract, acceptance: e.newAcceptance, ...(requirements ? { requirements } : {}), contractVersion: e.newContractVersion },
    candidate: s.phase.candidate && { sha: s.phase.candidate.sha, contractVersion: e.newContractVersion },
    decisions,
    findings,
    attempt: { n: s.phase.attempt.n + 1 },
    // OD-2 A2: the amended contract starts a NEW attempt, so it owes its own
    // coverage; the previous candidate's report must not satisfy its freeze.
    coverage: undefined,
    coverageAttempt: undefined,
    checks: undefined,
    probe: undefined,
    reviews: {},
    ballots: [],
    overrides: [],
    inFlight: {},
    pendingDispute: undefined,
    pendingDisclosures: undefined,
    pendingPrior: undefined,
  });
}

addRow({
  id: "resolving-criterion-amended",
  axis: "phase",
  from: "RESOLVING",
  trigger: "CRITERION_AMENDED",
  guardName: "criterionAmendmentReady",
  guard: criterionAmendmentReady,
  to: "IMPLEMENTING",
  // The resulting IMPLEMENTING state has no worker in flight, so next()
  // dispatches a fresh attempt under the new contract version.
  actions: [{ type: "dispatch_worker" }],
  apply: applyCriterionAmended,
});

// A gate-less phase: acceptance is immediate, exactly as before plan 01f.
addRow({
  id: "resolving-accept-holds",
  axis: "phase",
  from: "RESOLVING",
  trigger: "ACCEPTED",
  guardName: "acceptHoldsNoGateAndCorrectionsMatch",
  guard: (s, ev) => !gateDeclared(s) && acceptHolds(s) && acceptCorrectionsMatch(s, ev),
  to: "ACCEPTED",
  // Canonical integrationHead "H0", probedI "I1" (see the row's fixture).
  actions: [{ type: "publish_intent", expectedHead: "H0", candidateI: "I1" }],
  apply: applyAccepted,
});

/** Plan 01f: the candidate is acceptable, but the phase's contract declares a
 * gate — so the phase gates it first. The gate itself is dispatched from
 * GATING (the new stage), never from here. */
addRow({
  id: "resolving-gate-required",
  axis: "phase",
  from: "RESOLVING",
  trigger: "GATE_REQUIRED",
  guardName: "acceptHoldsAndGateDeclared",
  guard: (s) => acceptHolds(s) && gateDeclared(s),
  to: "GATING",
  actions: [{ type: "run_gate", candidateSha: "C1" }],
  apply: (s) => withPhase(s, { phase: "GATING" }),
});

// --- GATING (plan 01f): the conductor's own expensive, live gate ----------
// The gate command runs once for this candidate (a recorded pass for the same
// tree is reused instead of rerun: core/gate.ts's `reusableGate`, and the
// conductor's `#runGate`). Its outcome is one of these three events; nothing
// else leaves GATING except AMEND/REVISE (see their own rows).
addRow({
  id: "gate-accepted",
  axis: "phase",
  from: "GATING",
  trigger: "ACCEPTED",
  guardName: "acceptHoldsAndCorrectionsMatch",
  guard: (s, ev) => acceptHolds(s) && acceptCorrectionsMatch(s, ev),
  to: "ACCEPTED",
  actions: [{ type: "publish_intent", expectedHead: "H0", candidateI: "I1" }],
  apply: applyAccepted,
});

failureRows(
  "gate-failed",
  "GATING",
  "GATE_FAILED",
  REPAIR_ATTEMPT_ACTIONS,
  "the gate kept failing",
  (s, ev) => {
    // The gate's own evidence enters the record as a blocking `integration`
    // finding (runtime §4's fourth blocking-integration case), exactly like a
    // failed integration probe: the repair round carries the log's last lines
    // to the worker, and the owner sees the same finding if the budget runs
    // out.
    const e = ev as Extract<Event, { type: "GATE_FAILED" }>;
    const finding: Finding = {
      id: `F-${s.phase.phaseId}-gate-${s.phase.findings.length + 1}`,
      version: 1,
      phaseId: s.phase.phaseId,
      kind: "integration",
      severity: "blocking",
      evidence: e.evidence,
      raisedBy: "conductor",
      status: "open",
      boundCandidateSha: s.phase.candidate!.sha,
    };
    return withPhase(s, {
      findings: [...s.phase.findings, finding],
      inFlight: clearInFlight(s.phase, "run_gate"),
    });
  },
);

addRow({
  id: "gate-interrupted",
  axis: "phase",
  from: "GATING",
  trigger: "GATE_INTERRUPTED",
  guardName: "always",
  guard: () => true,
  to: "GATING",
  actions: [{ type: "run_gate", candidateSha: "C1" }],
  apply: (s) => withPhase(s, { inFlight: clearInFlight(s.phase, "run_gate") }),
});

// --- FINAL_CHECKING (plan 06c): the plan's final check ---------------------
// A phase whose contract declares a final check runs it once for the candidate
// about to be accepted, after the reviews have passed. A failure is an
// ordinary check failure: back to REPAIRING (or AWAITING_OWNER when the budget
// is spent), with the failing test named in a blocking finding.
function finalDeclared(s: State): boolean {
  return finalCheckOf(s.phase.contract) !== undefined;
}

function finalCheckAlreadyPassed(s: State): boolean {
  return s.phase.finalChecksPassedFor !== undefined && s.phase.finalChecksPassedFor === s.phase.candidate?.sha;
}

addRow({
  id: "resolving-final-check-required",
  axis: "phase",
  from: "RESOLVING",
  trigger: "FINAL_CHECK_REQUIRED",
  guardName: "acceptHoldsAndFinalDeclared",
  guard: (s) => acceptHolds(s) && finalDeclared(s) && !finalCheckAlreadyPassed(s),
  to: "FINAL_CHECKING",
  actions: [{ type: "run_final_checks", candidateSha: "C1" }],
  apply: (s) => withPhase(s, { phase: "FINAL_CHECKING" }),
});

addRow({
  id: "final-checks-passed-accepted",
  axis: "phase",
  from: "FINAL_CHECKING",
  trigger: "FINAL_CHECKS_PASSED",
  guardName: "acceptHoldsAndNoGate",
  guard: (s) => acceptHolds(s) && !gateDeclared(s),
  to: "ACCEPTED",
  actions: [{ type: "publish_intent", expectedHead: "H0", candidateI: "I1" }],
  apply: (s) => {
    const C = s.phase.candidate!.sha;
    const K = s.phase.contract.contractVersion;
    const resolved = resolvedCorrectionIdsFor(s.phase, C, K);
    const corrections = s.phase.corrections.map((c: Correction) =>
      resolved.includes(c.id) ? { ...c, status: "resolved" as const } : c,
    );
    return withPhase(s, {
      phase: "ACCEPTED",
      corrections,
      finalChecksPassedFor: C,
      inFlight: clearInFlight(s.phase, "run_final_checks"),
    });
  },
});

addRow({
  id: "final-checks-passed-gated",
  axis: "phase",
  from: "FINAL_CHECKING",
  trigger: "FINAL_CHECKS_PASSED",
  guardName: "acceptHoldsAndGateDeclared",
  guard: (s) => acceptHolds(s) && gateDeclared(s),
  to: "GATING",
  actions: [{ type: "run_gate", candidateSha: "C1" }],
  apply: (s) =>
    withPhase(s, {
      phase: "GATING",
      finalChecksPassedFor: s.phase.candidate?.sha ?? "",
      checks: { candidateSha: s.phase.candidate!.sha, passed: true, tier: "final" },
      inFlight: clearInFlight(s.phase, "run_final_checks"),
    }),
});

failureRows(
  "final-checks-failed",
  "FINAL_CHECKING",
  "FINAL_CHECKS_FAILED",
  REPAIR_ATTEMPT_ACTIONS,
  "the final check kept failing",
  (s, ev) => {
    const e = ev as Extract<Event, { type: "FINAL_CHECKS_FAILED" }>;
    // An ordinary check failure: the failing tests are kept on `checks` (so
    // the repair prompt names them) and no finding is raised — a finding
    // would stay open and block the next candidate's acceptance.
    return withPhase(s, {
      checks: {
        candidateSha: s.phase.candidate!.sha,
        passed: false,
        tier: "final",
        ...(e.failures && e.failures.length > 0 ? { failures: e.failures } : {}),
      },
      inFlight: clearInFlight(s.phase, "run_final_checks"),
    });
  },
);

addRow({
  id: "final-checks-interrupted",
  axis: "phase",
  from: "FINAL_CHECKING",
  trigger: "FINAL_CHECKS_INTERRUPTED",
  guardName: "always",
  guard: () => true,
  to: "FINAL_CHECKING",
  actions: [{ type: "run_final_checks", candidateSha: "C1" }],
  apply: (s) => withPhase(s, { inFlight: clearInFlight(s.phase, "run_final_checks") }),
});

addRow({
  id: "resolving-incomplete-repair",
  axis: "phase",
  from: "RESOLVING",
  trigger: "RESOLVING_INCOMPLETE",
  guardName: "openItemsAndBudgetRemains",
  // Plan 06b: an evidence-only phase parks on the owner, not a repair round.
  guard: (s) => !acceptHolds(s) && budgetRemains(s) && !evidenceOnlyPending(s.phase),
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
  guard: (s) => !acceptHolds(s) && (budgetExhausted(s) || evidenceOnlyPending(s.phase)),
  to: "AWAITING_OWNER",
  actions: [],
  apply: (s) =>
    enterAwaitingOwner(
      s,
      evidenceOnlyPending(s.phase)
        ? `the owner must record evidence for ${pendingEvidenceItems(s.phase).map((i) => i.id).join(", ")}`
        : "the repair budget ran out while items remained open",
    ),
});

// Plan 06b: every `evidence` item is recorded, so the owner has done the
// thing the phase parked for. The parking request closes and the phase
// resumes to RESOLVING, where next() re-evaluates accept().
addRow({
  id: "awaiting-owner-evidence-recorded",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "EVIDENCE_RECORDED",
  guardName: "evidenceAllRecorded",
  guard: (s) => evidenceAllRecorded(s.phase),
  to: "RESOLVING",
  // The gate-less fixture (base contract): accept now holds, so the row's
  // actions are exactly next(RESOLVING).
  actions: [{ type: "accept", resolvedCorrectionIds: [] }],
  // OD-1 (disc-M-56): resolve ONLY the request that parked the phase for
  // evidence — never an unrelated owner decision. The parking request is the
  // fallback whose reason names the evidence items.
  apply: (s) =>
    withPhase(s, {
      phase: "RESOLVING",
      ownerRequests: s.phase.ownerRequests.map((r) =>
        r.status === "open" && r.reason.startsWith("the owner must record evidence for")
          ? {
              ...r,
              status: "resolved" as const,
              resolution: { option: "evidence_recorded" },
              resolvedBinding: s.phase.candidate
                ? { candidateSha: s.phase.candidate.sha, contractVersion: s.phase.contract.contractVersion }
                : undefined,
            }
          : r,
      ),
    }),
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
      // OD-2 A2: a repair attempt owes its own coverage; the previous
      // attempt's report never satisfies the freeze.
      coverage: undefined,
      coverageAttempt: undefined,
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

for (const from of ["CHECKING", "PROBING", "REVIEWING", "EVALUATING", "RESOLVING", "GATING", "ACCEPTED", "AWAITING_OWNER"] as PhaseStateName[]) {
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

// --- plan 01g: the owner reverts an amendment (`revert AM-...`) ----------
// Like AMEND, a revert changes the contract version, so the evidence bound
// to the replaced version is invalidated and the phase re-evaluates from
// CHECKING. Without that reset the phase could not re-derive acceptance
// (finding M-6). It applies only from a correction naming an applied
// amendment (conductor/emacs enforce that); the core only cares that the
// target is a real, applied amendment of this phase.
function revertTarget(s: State, ev: Event): Decision | undefined {
  const e = ev as Extract<Event, { type: "CRITERION_REVERTED" }>;
  if (!s.phase.candidate) return undefined;
  const decision = s.phase.decisions.find((d) => d.amendment?.id === e.amendmentId);
  if (!decision || !decision.amendment || decision.amendment.status !== "applied") return undefined;
  if (!sameVersion(decision.boundContractVersion, s.phase.contract.contractVersion)) return undefined;
  if (!Array.isArray(e.newAcceptance) || e.newAcceptance.length === 0 || e.newAcceptance.some((a) => typeof a !== "string" || a.length === 0)) {
    return undefined;
  }
  if (!s.phase.contract.acceptance.includes(decision.amendment.proposedWording)) return undefined;
  return decision;
}

function applyCriterionReverted(s: State, ev: Event): State {
  const e = ev as Extract<Event, { type: "CRITERION_REVERTED" }>;
  const decisions = s.phase.decisions.map((d) =>
    d.amendment?.id === e.amendmentId
      ? {
          ...d,
          version: d.version + 1,
          boundContractVersion: e.newContractVersion,
          // OD-2 / B-30: the timestamp rides on the event; the transition
          // never reads a clock, so a rebuild is byte-identical.
          amendment: { ...d.amendment!, status: "reverted" as const, revertedAt: e.at },
        }
      : d,
  );
  // An open request bound to the wording just replaced is superseded; it is
  // re-evaluated under the restored contract version.
  const ownerRequests = s.phase.ownerRequests.map((r) =>
    r.status === "open" && r.boundContractVersion && sameVersion(r.boundContractVersion, s.phase.contract.contractVersion)
      ? { ...r, status: "resolved" as const, resolution: { option: "superseded_by_amendment_revert" } }
      : r,
  );
  return withPhase(s, {
    phase: "CHECKING",
    contract: { ...s.phase.contract, acceptance: e.newAcceptance, contractVersion: e.newContractVersion },
    candidate: s.phase.candidate && { sha: s.phase.candidate.sha, contractVersion: e.newContractVersion },
    decisions,
    ownerRequests,
    checks: undefined,
    probe: undefined,
    reviews: {},
    ballots: [],
    overrides: [],
    inFlight: {},
  });
}

for (const from of ["CHECKING", "PROBING", "REVIEWING", "EVALUATING", "RESOLVING", "GATING", "ACCEPTED", "PUBLISHING", "AWAITING_OWNER"] as PhaseStateName[]) {
  addRow({
    id: `criterion-reverted-from-${from.toLowerCase()}`,
    axis: "phase",
    from,
    trigger: "CRITERION_REVERTED",
    guardName: "revertTargetReady",
    guard: (s, ev) => revertTarget(s, ev) !== undefined,
    to: "CHECKING",
    actions: [{ type: "run_checks", candidateSha: "C1" }],
    apply: applyCriterionReverted,
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

/** Plan 2d (§7.5/§7.4): an owner correction typed into the input box while
 * the phase is parked on owner requests. Unlike REVISE it is not bound to a
 * single record, so it answers everything the phase is waiting on at once:
 * every open owner request is resolved, a fresh 3-round allowance is granted
 * (independent of any exhausted budget), and the owner's words are queued as
 * a note so the repair attempt's prompt carries them verbatim. */
function applyOwnerCorrection(s: State, ev: Event): State {
  const e = ev as Extract<Event, { type: "OWNER_CORRECTION" }>;
  const C = s.phase.candidate?.sha;
  const K = s.phase.contract.contractVersion;
  const ownerRequests = s.phase.ownerRequests.map((r) =>
    r.status === "open"
      ? {
          ...r,
          status: "resolved" as const,
          resolution: { option: "correction", note: e.text },
          resolvedBinding: C ? { candidateSha: C, contractVersion: K } : undefined,
        }
      : r,
  );
  return withPhase(s, {
    phase: "REPAIRING",
    ownerRequests,
    repairRoundsGranted: s.phase.repairRoundsGranted + 3,
    ownerNotes: [...(s.phase.ownerNotes ?? []), e.text],
    inFlight: {},
  });
}

addRow({
  id: "owner-correction-from-awaiting-owner",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "OWNER_CORRECTION",
  guardName: "always",
  guard: () => true,
  to: "REPAIRING",
  actions: REPAIR_ATTEMPT_ACTIONS,
  apply: applyOwnerCorrection,
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
const itemCarried = checkAndApply(checkItemCarried, applyItemCarried);

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

// Plan 06g (A6): "accept with carried items" — the single decision a phase
// whose budget is spent offers when only advisories are open. Taking it
// accepts the candidate as it stands (applyOwnerRequestResolved records
// `acceptedWithCarried` and the carried ids, and accept() honours them) and
// the phase resumes to RESOLVING, where next() accepts.
addRow({
  id: "awaiting-owner-request-resolved-accept-carried",
  axis: "phase",
  from: "AWAITING_OWNER",
  trigger: "OWNER_REQUEST_RESOLVED",
  guardName: "budgetGateAcceptCarried",
  guard: (s, ev) => {
    const request = resolvedRequestFor(s, ev);
    const e = ev as Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;
    if (!request || !isBudgetGateRequest(request) || e.option !== "accept_carried") return false;
    if (!ownerRequestResolved.guardValid(s, ev)) return false;
    return awaitingOwnerTarget(ownerRequestResolved.applyPhase(s, ev)) !== "AWAITING_OWNER";
  },
  to: "RESOLVING",
  // The gate-less form, exactly like the grant row's own fixture: a phase
  // whose contract declares a gate asks for `gate_required` first.
  actions: [{ type: "accept", resolvedCorrectionIds: [] }],
  apply: (s, ev) => withPhase(s, { ...ownerRequestResolved.applyPhase(s, ev), phase: "RESOLVING" }),
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
    // The gate-less form. A phase whose contract declares a gate never takes
    // the `accept` action from RESOLVING: next() asks for `gate_required`
    // first and the gate stage decides (plan 01f). The row's `actions` column
    // documents the fixture (a gate-less contract), exactly as before.
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

// Plan 06g (A5): the owner's `tt carry <run> <id> --to <phase-id>` — mark the
// item carried with its target, answer the request about it, and (once no
// blocking item remains uncarried) accept the candidate. It shares the same
// clears/stays routing as the other record-level owner commands.
addAwaitingOwnerRecordCommandRows("awaiting-owner-item-carried", "ITEM_CARRIED", () => true, itemCarried);

// --- launch failure (§2.1): a tool-set mismatch is a launch failure, not a
// warning — straight to BLOCKED, no repair round spent (phase-1b round of
// review item 4; a pure, additive core change — see EvLaunchFailed). -------
function launchFailedReason(e: Extract<Event, { type: "LAUNCH_FAILED" }>): string {
  const who = e.role === "reviewer" && e.reviewer ? `reviewer ${e.reviewer}` : e.role;
  return `launch failure: tool set mismatch (${who}) — expected [${e.expected.join(", ")}], missing [${e.missing.join(", ")}], extra [${e.extra.join(", ")}]`;
}

addRow({
  id: "launch-failed-from-implementing",
  axis: "phase",
  from: "IMPLEMENTING",
  trigger: "LAUNCH_FAILED",
  guardName: "always",
  guard: () => true,
  to: "BLOCKED",
  actions: [],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "LAUNCH_FAILED" }>;
    return withPhase(s, {
      phase: "BLOCKED",
      blockedReason: launchFailedReason(e),
      inFlight: clearInFlight(s.phase, "dispatch_worker"),
    });
  },
});

// Plan 04a / B-31: an evaluator whose tool set does not match is a launch
// failure too — straight to BLOCKED with evidence, never a silent timeout.
addRow({
  id: "launch-failed-from-evaluating",
  axis: "phase",
  from: "EVALUATING",
  trigger: "LAUNCH_FAILED",
  guardName: "always",
  guard: () => true,
  to: "BLOCKED",
  actions: [],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "LAUNCH_FAILED" }>;
    return withPhase(s, {
      phase: "BLOCKED",
      blockedReason: launchFailedReason(e),
      inFlight: clearInFlight(
        s.phase,
        "dispatch_evaluation_tradeoff",
        "dispatch_evaluation_finding",
        "dispatch_evaluation_blocker",
      ),
    });
  },
});

addRow({
  id: "launch-failed-from-reviewing",
  axis: "phase",
  from: "REVIEWING",
  trigger: "LAUNCH_FAILED",
  guardName: "always",
  guard: () => true,
  to: "BLOCKED",
  actions: [],
  apply: (s, ev) => {
    const e = ev as Extract<Event, { type: "LAUNCH_FAILED" }>;
    const key = e.reviewer ? (`review_${e.reviewer}` as InFlightKey) : undefined;
    return withPhase(s, {
      phase: "BLOCKED",
      blockedReason: launchFailedReason(e),
      inFlight: key ? clearInFlight(s.phase, key) : s.phase.inFlight,
    });
  },
});

// --- plan 05i: environment preflight / environment failures ---------------
// The environment is a run-axis concern: `ENV_BLOCKED` freezes dispatch the
// same way `RUN_PAUSED_BUDGET` does, while the phase state stays exactly
// where it was — a 127 in CHECKING leaves the phase in CHECKING, so a
// `tt resume` whose preflight now passes re-runs the checks instead of losing
// the candidate. `ENV_PREFLIGHT_FAILED` is emitted before the baseline and
// before any agent launch; `ENV_CHECK_FAILED` is emitted when any check,
// baseline, probe or gate command exits 126/127.

/** The in-flight entry an environment failure interrupts, per stage, so that
 * after a passing `tt resume` next() re-dispatches the stage instead of
 * waiting forever on an intent that will never complete. */
function envStageInFlight(stage: "baseline" | "checks" | "probe" | "gate" | "worker"): InFlightKey {
  switch (stage) {
    case "baseline": return "run_baseline";
    case "checks": return "run_checks";
    case "probe": return "dispatch_probe";
    case "gate": return "run_gate";
    // Plan 05d: a worker launch that missed hello twice is an environment
    // problem; clearing its in-flight entry lets a passing `tt resume`
    // re-dispatch the same attempt.
    case "worker": return "dispatch_worker";
  }
}

/** Set an environment block, remembering the run state to restore when it
 * clears (finding M-6): a run that was paused for budget must return to
 * `RUN_PAUSED_BUDGET`, not silently run past an exhausted budget. */
function applyEnvPreflightFailedState(s: State, e: Extract<Event, { type: "ENV_PREFLIGHT_FAILED" }>, from: RunStateName): State {
  return {
    ...s,
    run: "ENV_BLOCKED",
    phase: {
      ...s.phase,
      env: {
        ...(s.phase.env ?? {}),
        path: e.path,
        resumeRun: from,
        blocked: { kind: "preflight", missing: e.missing, path: e.path, at: e.at },
      },
    },
  };
}

addRow({
  id: "env-preflight-failed",
  axis: "run",
  from: "RUN_ACTIVE",
  trigger: "ENV_PREFLIGHT_FAILED",
  guardName: "always",
  guard: () => true,
  to: "ENV_BLOCKED",
  actions: [],
  apply: (s, ev) => applyEnvPreflightFailedState(s, ev as Extract<Event, { type: "ENV_PREFLIGHT_FAILED" }>, "RUN_ACTIVE"),
});

// Idempotent: a `tt resume` whose preflight still fails re-records the block
// rather than being rejected by reduce(), preserving the state to restore.
addRow({
  id: "env-preflight-failed-already-blocked",
  axis: "run",
  from: "ENV_BLOCKED",
  trigger: "ENV_PREFLIGHT_FAILED",
  guardName: "always",
  guard: () => true,
  to: "ENV_BLOCKED",
  actions: [],
  apply: (s, ev) =>
    applyEnvPreflightFailedState(
      s,
      ev as Extract<Event, { type: "ENV_PREFLIGHT_FAILED" }>,
      s.phase.env?.resumeRun ?? "RUN_ACTIVE",
    ),
});

// Plan 05i / findings M-1 and M-6: a conductor also starts (resumes) a run
// that is PAUSED for budget, where `#envPreflightGate` runs unconditionally
// too. A missing tool must block it rather than being rejected by reduce()
// and crashing the conductor; and clearing the block must return to the
// budget pause, never to RUN_ACTIVE.
addRow({
  id: "env-preflight-failed-from-budget",
  axis: "run",
  from: "RUN_PAUSED_BUDGET",
  trigger: "ENV_PREFLIGHT_FAILED",
  guardName: "always",
  guard: () => true,
  to: "ENV_BLOCKED",
  actions: [],
  apply: (s, ev) => applyEnvPreflightFailedState(s, ev as Extract<Event, { type: "ENV_PREFLIGHT_FAILED" }>, "RUN_PAUSED_BUDGET"),
});

function applyEnvCheckFailedState(s: State, e: Extract<Event, { type: "ENV_CHECK_FAILED" }>, from: RunStateName): State {
  return {
    ...s,
    run: "ENV_BLOCKED",
    phase: {
      ...s.phase,
      env: {
        ...(s.phase.env ?? {}),
        resumeRun: from,
        blocked: { kind: "check", stage: e.stage, command: e.command, exitCode: e.exitCode, tail: e.tail, at: e.at },
      },
      inFlight: clearInFlight(s.phase, envStageInFlight(e.stage)),
    },
  };
}

addRow({
  id: "env-check-failed",
  axis: "run",
  from: "RUN_ACTIVE",
  trigger: "ENV_CHECK_FAILED",
  guardName: "always",
  guard: () => true,
  to: "ENV_BLOCKED",
  actions: [],
  apply: (s, ev) => applyEnvCheckFailedState(s, ev as Extract<Event, { type: "ENV_CHECK_FAILED" }>, "RUN_ACTIVE"),
});

addRow({
  id: "env-check-failed-already-blocked",
  axis: "run",
  from: "ENV_BLOCKED",
  trigger: "ENV_CHECK_FAILED",
  guardName: "always",
  guard: () => true,
  to: "ENV_BLOCKED",
  actions: [],
  apply: (s, ev) =>
    applyEnvCheckFailedState(
      s,
      ev as Extract<Event, { type: "ENV_CHECK_FAILED" }>,
      s.phase.env?.resumeRun ?? "RUN_ACTIVE",
    ),
});

// Finding A-4: same missing row for the check-failure event, from the paused
// state (defensive: a check cannot start while paused, but every run state
// the gate can be applied in must have a row).
addRow({
  id: "env-check-failed-from-budget",
  axis: "run",
  from: "RUN_PAUSED_BUDGET",
  trigger: "ENV_CHECK_FAILED",
  guardName: "always",
  guard: () => true,
  to: "ENV_BLOCKED",
  actions: [],
  apply: (s, ev) => applyEnvCheckFailedState(s, ev as Extract<Event, { type: "ENV_CHECK_FAILED" }>, "RUN_PAUSED_BUDGET"),
});

// A passing `tt resume` preflight clears the block and the phase continues
// from wherever it was frozen. The run returns to the state it was blocked
// from (finding M-6): a budget pause is restored, not dropped.
addRow({
  id: "env-resumed",
  axis: "run",
  from: "ENV_BLOCKED",
  trigger: "RUN_RESUMED",
  guardName: "resumeRunWasNotBudget",
  guard: (s) => s.phase.env?.resumeRun !== "RUN_PAUSED_BUDGET",
  to: "RUN_ACTIVE",
  // The fixture resumes with the phase at READY and no prior pause, so next()
  // recommends start_attempt once the environment is unblocked.
  actions: [{ type: "start_attempt" }],
  apply: (s) => ({
    ...s,
    run: s.phase.env?.resumeRun ?? "RUN_ACTIVE",
    phase: { ...s.phase, env: { ...(s.phase.env ?? {}), blocked: undefined, resumeRun: undefined } },
  }),
});

// Finding M-6: the block was entered from a budget pause, so clearing it
// restores that pause instead of running past an exhausted budget.
addRow({
  id: "env-resumed-to-budget",
  axis: "run",
  from: "ENV_BLOCKED",
  trigger: "RUN_RESUMED",
  guardName: "resumeRunWasBudget",
  guard: (s) => s.phase.env?.resumeRun === "RUN_PAUSED_BUDGET",
  to: "RUN_PAUSED_BUDGET",
  // The run is paused again, so next() dispatches nothing.
  actions: [],
  apply: (s) => ({
    ...s,
    run: "RUN_PAUSED_BUDGET",
    phase: { ...s.phase, env: { ...(s.phase.env ?? {}), blocked: undefined, resumeRun: undefined } },
  }),
});

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

// Plan 06c (A5/OD-3): a conductor that starts on an existing, non-terminal
// run records RUN_RESUMED once at start-up, so the stage clock sees the resume
// as the start of a new segment. The run was already active, so this is a
// record-only row (the phase is preserved untouched).
addRow({
  id: "run-resumed-active",
  axis: "run",
  from: "RUN_ACTIVE",
  trigger: "RUN_RESUMED",
  guardName: "always",
  guard: () => true,
  to: "RUN_ACTIVE",
  // The fixture resumes with the phase at READY, so next() recommends
  // start_attempt once the resume is recorded.
  actions: [{ type: "start_attempt" }],
  apply: (s) => s,
});

export const TRANSITIONS: readonly TransitionRow[] = rows;

/** Plan 05h: the declared main path — the states a normal round passes
 * through, in order, from READY to DONE. `BASELINE` is on it (plan 04a takes
the base baseline before the first implement on a phase that needs one),
 * `EVALUATING` (plan 04a's evaluator) and `GATING` (plan 01f's gate) because
a round may pass through both before ACCEPTED. Off-path states (REPAIRING,
AWAITING_OWNER, BLOCKED, RUN_PAUSED_BUDGET) are deliberately not steps.
 *
 * This list is the tape's own path (`src/charts.ts` draws it) and its
 * integrity is checked, not assumed: every consecutive pair is a TRANSITIONS
 * edge and every name appears in the table (`test/contract/tape.test.ts`). */
export const MAIN_PATH: readonly PhaseStateName[] = [
  "READY",
  "BASELINE",
  "IMPLEMENTING",
  "FREEZING",
  "CHECKING",
  "PROBING",
  "REVIEWING",
  "EVALUATING",
  "RESOLVING",
  "GATING",
  "ACCEPTED",
  "PUBLISHING",
  "DONE",
];

export function rowsFor(state: State, trigger: Event["type"]): TransitionRow[] {
  return TRANSITIONS.filter((r) => currentOf(state, r.axis) === r.from && r.trigger === trigger);
}

export { budgetRemains, budgetExhausted, acceptHolds, allThreeReviewsPresent, clearInFlight };
