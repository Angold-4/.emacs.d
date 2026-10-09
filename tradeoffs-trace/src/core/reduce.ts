// reduce(state, event) -> ReduceResult
//
// A TOTAL function: it never throws, and it explicitly rejects unknown or
// out-of-order events rather than silently ignoring them or crashing. The
// only way it changes state is by applying a row from transitions.ts whose
// `from`/`axis` matches the current state, whose `trigger` matches the
// event's type, and whose `guard` accepts (state, event) — or, for events
// that only append a record without moving the phase's own FSM state
// (ballots, overrides, findings, owner requests, ACTION_STARTED), by
// `applyRecordEvent` below.
//
// Every owner-authored or reviewer-authored event that design §7.1 binds to
// a (candidate, contract, record version) is checked against that binding
// here — via binding.ts — with a visible, human-readable reason on
// mismatch, before anything else about the event is considered.

import { checkBallotBinding, checkBinding, checkTupleBinding, currentVersionsFor } from "./binding.ts";
import { applyEntryEvent, type EntryEvent } from "./entries.ts";
import { applyCarryWithContract, applyMessageEvent, checkMessageBinding } from "./messages.ts";
import { next as computeNext } from "./next.ts";
import {
  applyFindingAcceptedByOwner,
  applyItemCarried,
  applyOverrideCast,
  applyOwnerRequestResolved,
  checkFindingAcceptedByOwner,
  checkItemCarried,
  checkOverrideCast,
  checkOwnerRequestResolved,
} from "./owner-commands.ts";
import { isLiveDecision, panelOutcome, panelSeatNumbers, panelSeatSettled, panelSeatsSettled, reviewIngestionIssue, sameVersion } from "./predicate.ts";
import { seatsOf } from "./seats.ts";
import { rowsFor } from "./transitions.ts";
import type {
  BindingTuple,
  ContractVersion,
  Event,
  Finding,
  InFlightKey,
  MessageType,
  PhaseState,
  ReduceResult,
  State,
} from "./types.ts";

const KNOWN_EVENT_TYPES = new Set<string>([
  "ATTEMPT_STARTED",
  "BASELINE_COMPLETED",
  "BASELINE_TIMED_OUT",
  "BASELINE_INTERRUPTED",
  "EVALUATION_COMPLETED",
  "EVALUATION_TIMED_OUT",
  "EVALUATION_INTERRUPTED",
  "EVALUATOR_FINISHED",
  "PANEL_VOTE",
  "PANEL_SEAT_UNAVAILABLE",
  "PANEL_DECIDED",
  "SUBMIT_PHASE",
  "ATTEMPT_TIMED_OUT",
  "ATTEMPT_NO_SUBMISSION",
  "ATTEMPT_INTERRUPTED",
  "FREEZE_COMPLETED",
  "FREEZE_TIMED_OUT",
  "FREEZE_INTERRUPTED",
  "CHECKS_PASSED",
  "CHECKS_FAILED",
  "CHECKS_INTERRUPTED",
  "FINAL_CHECK_REQUIRED",
  "FINAL_CHECKS_PASSED",
  "FINAL_CHECKS_FAILED",
  "FINAL_CHECKS_INTERRUPTED",
  "ENV_CHECKED",
  "ENV_PREFLIGHT_FAILED",
  "ENV_CHECK_FAILED",
  "PROBE_PASSED",
  "PROBE_FAILED",
  "PROBE_INTERRUPTED",
  "REVIEW_SUBMITTED",
  "REVIEW_TIMED_OUT",
  "ACTION_STARTED",
  "BALLOT_CAST",
  "OVERRIDE_CAST",
  "FINDING_RAISED",
  "FINDING_CONFIRMED_REPAIRED",
  "FINDING_DISPROVED",
  "FINDING_ACCEPTED_BY_OWNER",
  "FINDING_SEVERITY_LOWERED",
  "FINDING_SEVERITY_CHANGED",
  "FINDING_VERIFIED",
  "FINDING_RESOLVED_BY_VOTE",
  "DECISION_CLASS_LOWERED",
  "ROUND_PANEL_VOTE",
  "ROUND_PANEL_SEAT_UNAVAILABLE",
  "ROUND_PANEL_DECIDED",
  "CANDIDATE_APPROVED",
  "OWNER_REQUEST_OPENED",
  "OWNER_REQUEST_RESOLVED",
  "ACCEPTED",
  "PUBLISH_INTENT",
  "PUBLISH_COMPLETED",
  "PUBLISH_STALE",
  "REPAIR_ATTEMPT_STARTED",
  "REPAIR_BUDGET_EXHAUSTED",
  "RESOLVING_INCOMPLETE",
  "GATE_REQUIRED",
  "GATE_FAILED",
  "GATE_INTERRUPTED",
  "REVISE",
  "AMEND",
  "CRITERION_AMENDED",
  "CRITERION_REVERTED",
  "RUN_BUDGET_EXCEEDED",
  "RUN_RESUMED",
  "LAUNCH_FAILED",
  "INTEGRITY_VIOLATED",
  "BRIEFS_RECORDED",
  "BRIEF_RETRY_ATTEMPTED",
  "ITEM_STATE_UPDATED",
  "EVIDENCE_RECORDED",
  "ITEM_CHECK_RECORDED",
  "FINDING_MERGED",
  "TRIAGE_RECORDED",
  "TRIAGE_FAILED",
  "DEFERRAL_RECORDED",
  "DEFERRAL_RESOLVED",
  "ROUND_STARTED",
  "CANDIDATE_SUBMITTED",
  "CANDIDATE_CHECKED",
  "PICK_VOTE",
  "REVOTE_STARTED",
  "CANDIDATE_PICKED",
  "ROUND_REVIEW_SUBMITTED",
  "ITEM_CARRIED",
  "DECISION_ADDED",
  "NOTE_ADDED",
  "OWNER_INPUT_RECORDED",
  "DIRECTIVE_ADDED",
  "DIRECTIVE_WITHDRAWN",
  "DIRECTIVE_DELIVERED",
  "OWNER_CORRECTION",
  "OWNER_REQUEST_MARKED_UNNEEDED",
  "MISS_RECORDED",
  "NOTES_DELIVERED",
  "FLAKE_OBSERVED",
  "LAUNCH_RETRIED",
  "DECISION_MATCHED",
  "FINDING_ALSO_RAISED",
  "MESSAGE_RAISED",
  "MESSAGE_PUBLISHED",
  "MESSAGE_MERGED",
  "MESSAGE_DROPPED",
  "OWNER_VERDICT",
  "MESSAGE_RESOLVED",
  "MESSAGE_ADDRESS_REPORTED",
  "MESSAGE_SUPERSEDED",
  "MESSAGE_CARRIED",
  "ENTRY_OPENED",
  "MESSAGE_LINKED",
  "ENTRY_RETITLED",
  "ENTRY_SPLIT",
  "ENTRY_STATE",
  "ENTRY_MERGED_BY_OWNER",
  "ENTRY_CURATED",
  "REVIEW_LINT_FAILED",
]);

function ok(state: State): ReduceResult {
  return { ok: true, state };
}

function rejected(state: State, reason: string): ReduceResult {
  return { ok: false, reason, state };
}

function bindingTupleForRecord(
  state: State,
  recordId: string,
  tuple: { candidateSha: string; contractVersion: ContractVersion; recordVersion: number },
): BindingTuple {
  return {
    runId: state.phase.runId,
    phaseId: state.phase.phaseId,
    candidateSha: tuple.candidateSha,
    contractVersion: tuple.contractVersion,
    recordId,
    recordVersion: tuple.recordVersion,
  };
}

function inFlightKeyFor(action: string, reviewer?: string, messageType?: string, blockerId?: string, seat?: number): string {
  if (action === "dispatch_review") return `review_${reviewer}`;
  // Plan 04a: one evaluator per message type, so each dispatch has its own
  // in-flight key (`dispatch_evaluation_tradeoff`, …).
  if (action === "dispatch_evaluation") return `dispatch_evaluation_${messageType}`;
  // Plan 04b: one panel seat per raw blocker, each its own dispatch.
  if (action === "dispatch_panel") return `dispatch_panel_${blockerId}_${seat}`;
  // Plan 05e: one round-panel seat per round, its own dispatch.
  if (action === "dispatch_round_panel") return `dispatch_round_panel_${seat}`;
  return action;
}

/** Plan 04b: merge one panel seat's state. */
function withPanelSeat(
  p: PhaseState,
  blockerId: string,
  seat: string,
  patch: Partial<import("./types.ts").PanelSeatState>,
): PhaseState["panel"] {
  const blockers = { ...(p.panel?.blockers ?? {}) };
  const panel = { ...(blockers[blockerId] ?? {}) };
  const seats = { ...(panel.seats ?? {}) };
  seats[seat] = { dispatches: 0, ...(seats[seat] ?? {}), ...patch };
  blockers[blockerId] = { ...panel, seats };
  return { blockers };
}

/** Plan 04a: merge one message type's evaluator outcome into `evaluation`. */
function withEvaluatorOutcome(p: PhaseState, type: MessageType, patch: { settled?: boolean; timedOut?: boolean; interruptedOnce?: boolean }): PhaseState["evaluation"] {
  const types = { ...(p.evaluation?.types ?? {}) };
  types[type] = { ...(types[type] ?? {}), ...patch };
  return { types };
}

function withoutEvaluationInFlight(p: PhaseState, type: MessageType): PhaseState["inFlight"] {
  const inFlight = { ...p.inFlight };
  delete inFlight[`dispatch_evaluation_${type}` as InFlightKey];
  return inFlight;
}

/** Plan 04b/06h: the panel seat keys a phase may have — one per reviewer. */
function panelSeatKeys(p: PhaseState): Set<string> {
  return new Set(panelSeatNumbers(seatsOf(p.contract).length).map(String));
}

function withoutPanelSeatInFlight(p: PhaseState, blockerId: string, seat: string): PhaseState["inFlight"] {
  const inFlight = { ...p.inFlight };
  delete inFlight[`dispatch_panel_${blockerId}_${seat}` as InFlightKey];
  return inFlight;
}

/** Plan 05e: merge one round-panel seat's state. */
function withRoundPanelSeat(p: PhaseState, seat: string, patch: Partial<import("./types.ts").RoundPanelSeatState>): PhaseState["panel"] {
  const round = { ...(p.panel?.round ?? {}) };
  const seats = { ...(round.seats ?? {}) };
  seats[seat] = { dispatches: 0, ...(seats[seat] ?? {}), ...patch };
  round.seats = seats;
  return { ...(p.panel ?? {}), round };
}

function withoutRoundPanelSeatInFlight(p: PhaseState, seat: string): PhaseState["inFlight"] {
  const inFlight = { ...p.inFlight };
  delete inFlight[`dispatch_round_panel_${seat}` as InFlightKey];
  return inFlight;
}

/** Events handled directly by reduce.ts, not by the transitions table: they
 * only ever append or amend a record without moving the phase's own FSM
 * state, so they have no row in transitions.ts (which is scoped to
 * phase-state-changing edges, per the plan's phase-0 brief). */
/** Plan 06g: replace (or insert) one lane's candidate record, preserving
 * every other lane's. */
function upsertLane(
  candidates: import("./types.ts").LaneCandidateRecord[],
  lane: string,
  patch: Partial<import("./types.ts").LaneCandidateRecord>,
): import("./types.ts").LaneCandidateRecord[] {
  const existing = candidates.find((c) => c.lane === lane);
  if (!existing) return [...candidates, { lane, ...patch }];
  return candidates.map((c) => (c.lane === lane ? { ...c, ...patch } : c));
}

/** Plan 06g: replace one round record in order. */
function replaceRound(
  rounds: import("./types.ts").RoundRecord[] | undefined,
  record: import("./types.ts").RoundRecord,
): import("./types.ts").RoundRecord[] {
  return (rounds ?? []).map((r) => (r.round === record.round ? record : r));
}

function applyRecordEvent(state: State, event: Event): ReduceResult | undefined {
  const p = state.phase;
  switch (event.type) {
    case "ACTION_STARTED": {
      // Only an action next() is actually recommending right now may be
      // marked in flight — this both prevents starting arbitrary work and
      // is exactly what makes a second next() call return [] (no double
      // dispatch).
      const outstanding = computeNext(state).some(
        (a) =>
          a.type === event.action &&
          (event.action !== "dispatch_review" || a.reviewer === event.reviewer) &&
          (event.action !== "dispatch_evaluation" || a.messageType === event.messageType) &&
          (event.action !== "dispatch_panel" ||
            (a.blockerId === event.blockerId && a.seat === event.seat)) &&
          (event.action !== "dispatch_round_panel" || a.seat === event.seat),
      );
      if (!outstanding) {
        return rejected(
          state,
          `action '${event.action}'${event.reviewer ? ` (${event.reviewer})` : ""}${event.messageType ? ` (${event.messageType})` : ""}${event.blockerId ? ` (${event.blockerId} seat ${event.seat})` : ""} is not currently outstanding in phase ${p.phase}`,
        );
      }
      const key = inFlightKeyFor(event.action, event.reviewer, event.messageType, event.blockerId, event.seat) as InFlightKey;
      // Plan 04b: a panel seat's dispatch count is the one-retry bookkeeping
      // (a second loss makes the seat unavailable), recorded here so it
      // survives a conductor restart.
      const panel =
        event.action === "dispatch_panel" && event.blockerId && event.seat !== undefined
          ? withPanelSeat(p, event.blockerId, String(event.seat), {
              dispatches: (p.panel?.blockers?.[event.blockerId]?.seats?.[String(event.seat)]?.dispatches ?? 0) + 1,
              // A re-dispatch clears the first loss's marker; the seat is
              // being tried again.
              unavailable: false,
            })
          : event.action === "dispatch_round_panel" && event.seat !== undefined
            ? withRoundPanelSeat(p, String(event.seat), {
                dispatches: (p.panel?.round?.seats?.[String(event.seat)]?.dispatches ?? 0) + 1,
                unavailable: false,
              })
            : p.panel;
      return ok({
        ...state,
        phase: { ...p, panel, inFlight: { ...p.inFlight, [key]: { actionId: event.actionId } } },
      });
    }

    case "BALLOT_CAST": {
      // design §7.1: bound to candidate, contract version AND the
      // decision's current record version.
      const check = checkBallotBinding(event.ballot, p);
      if (!check.ok) return rejected(state, check.reason!);
      const target = p.decisions.find((d) => d.id === event.ballot.decisionId);
      if (target && !isLiveDecision(target)) {
        return rejected(state, `decision ${target.id} is superseded (${target.supersededBy ?? "by correction"}) and is not votable`);
      }

      let next = { ...p, ballots: [...p.ballots, event.ballot] };
      if (event.ballot.contractObjection) {
        const decision = next.decisions.find((d) => d.id === event.ballot.decisionId);
        if (decision && !decision.linkedFindingId) {
          const finding = {
            id: `F-${next.phaseId}-contract-${next.findings.length + 1}`,
            version: 1,
            phaseId: next.phaseId,
            kind: "contract" as const,
            severity: "blocking" as const,
            evidence: event.ballot.evidence.join("; "),
            raisedBy: event.ballot.reviewer,
            status: "open" as const,
            linkedDecisionId: decision.id,
            boundCandidateSha: p.candidate!.sha,
          };
          next = {
            ...next,
            findings: [...next.findings, finding],
            decisions: next.decisions.map((d) =>
              d.id === decision.id ? { ...d, linkedFindingId: finding.id, version: d.version + 1 } : d,
            ),
          };
        }
      }
      return ok({ ...state, phase: next });
    }

    case "OVERRIDE_CAST": {
      // design §7.4: override is "recorded beside the ballots" — same
      // binding rule as a ballot (candidate, contract, decision version).
      const check = checkOverrideCast(p, event);
      if (!check.ok) return rejected(state, check.reason!);
      return ok({ ...state, phase: applyOverrideCast(p, event) });
    }

    case "ITEM_CARRIED": {
      // Plan 06g (A5): the owner's carry. The AWAITING_OWNER rows route it;
      // every other phase records it (mark carried, answer the request, and
      // accept once nothing blocking is uncarried) without moving the phase.
      const check = checkItemCarried(p, event);
      if (!check.ok) return rejected(state, check.reason!);
      return ok({ ...state, phase: applyItemCarried(p, event) });
    }

    case "FINDING_RAISED": {
      // design §4.1: "a finding needs evidence" — reject one with none.
      if (!event.finding.evidence || event.finding.evidence.trim().length === 0) {
        return rejected(state, "a finding must carry evidence; got none");
      }
      return ok({ ...state, phase: { ...p, findings: [...p.findings, event.finding] } });
    }

    case "ITEM_CHECK_RECORDED": {
      // Plan 06b (OD-1 R3b): the evaluator's re-check of an item's majority
      // verdict, merged by item id (the latest check wins). Plan 06i: the same
      // form carries the evaluator's impact class for a finding or discovered
      // decision, so one check is both the re-check and the classification.
      const others = (p.itemChecks ?? []).filter((c) => c.itemId !== event.itemId);
      return ok({
        ...state,
        phase: {
          ...p,
          itemChecks: [
            ...others,
            {
              itemId: event.itemId,
              verdict: event.verdict,
              evidence: event.evidence,
              ...(event.impact ? { impact: event.impact } : {}),
              ...(event.chosen ? { chosen: event.chosen } : {}),
              ...(event.alternative ? { alternative: event.alternative } : {}),
              ...(event.why ? { why: event.why } : {}),
            },
          ],
        },
      });
    }

    case "FINDING_MERGED": {
      // Plan 06i (C3): a duplicate finding is merged, not disproved, and
      // linked to the record it duplicates. Its own triage record copies the
      // original's disposition, so nothing leaves the ledger without one.
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      const findings = p.findings.map((f) =>
        f.id === event.findingId ? { ...f, status: "merged" as const, mergedInto: event.into, version: f.version + 1 } : f,
      );
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "TRIAGE_RECORDED": {
      // Plan 06i: one record per finding/discovered decision, merged by id
      // (the latest disposition wins). Record-only: the phase's own FSM state
      // never moves here; `accept()` and the views read the records.
      const others = (p.triage ?? []).filter((r) => r.itemId !== event.record.itemId);
      return ok({ ...state, phase: { ...p, triage: [...others, event.record] } });
    }

    case "DEFERRAL_RECORDED": {
      // Plan 06i: a deferral needs a guard — a test that shows its current
      // cost, or a recorded owner ruling. Without either it is refused with
      // the reason, never recorded silently. Recorded once per item id.
      const d = event.deferral;
      if (!(d.test && d.test.trim().length > 0) && !(d.ownerRuling && d.ownerRuling.trim().length > 0)) {
        return rejected(state, `deferring ${d.itemId} needs a guard: a test that shows its current cost, or a recorded owner ruling`);
      }
      const others = (p.deferrals ?? []).filter((x) => x.itemId !== d.itemId);
      return ok({ ...state, phase: { ...p, deferrals: [...others, d] } });
    }

    case "DEFERRAL_RESOLVED": {
      return ok({
        ...state,
        phase: {
          ...p,
          deferrals: (p.deferrals ?? []).map((d) => (d.id === event.deferralId ? { ...d, status: "resolved" as const } : d)),
        },
      });
    }

    case "ITEM_STATE_UPDATED": {
      // Plan 06b: the item loop's own state, merged record-only. Nothing
      // here moves the phase; the freeze, checks, reviews and acceptance
      // read it.
      return ok({
        ...state,
        phase: {
          ...p,
          ...(event.coverage ? { coverage: event.coverage } : {}),
          ...(event.coverageAttempt !== undefined ? { coverageAttempt: event.coverageAttempt } : {}),
          ...(event.checkResolution ? { checkResolution: event.checkResolution } : {}),
          ...(event.itemEvidence ? { itemEvidence: event.itemEvidence } : {}),
          ...(event.acceptedDeviations ? { acceptedDeviations: event.acceptedDeviations } : {}),
          ...(event.archSymbolDeviations ? { archSymbolDeviations: event.archSymbolDeviations } : {}),
          ...(event.overturns ? { overturns: event.overturns } : {}),
        },
      });
    }

    // Plan 06g: the round's record events. None of them moves the phase —
    // the conductor's own orchestration does that — but they are the log a
    // restart folds to know each round's base, lanes, candidates, votes and
    // winner, and the views render. Every one of them is rejected when its
    // round has not started, so a stray event cannot invent a round.
    case "ROUND_STARTED": {
      if (event.round < 1 || event.lanes.length === 0) {
        return rejected(state, "ROUND_STARTED needs a round number and at least one lane");
      }
      const others = (p.rounds ?? []).filter((r) => r.round !== event.round);
      return ok({
        ...state,
        phase: {
          ...p,
          rounds: [
            ...others,
            { round: event.round, base: event.base, lanes: [...event.lanes], candidates: [], votes: [] },
          ].sort((a, b) => a.round - b.round),
        },
      });
    }

    case "CANDIDATE_SUBMITTED": {
      const round = (p.rounds ?? []).find((r) => r.round === event.round);
      if (!round) return rejected(state, `CANDIDATE_SUBMITTED for round ${event.round}, which has not started`);
      if (!round.lanes.includes(event.lane)) {
        return rejected(state, `CANDIDATE_SUBMITTED names lane ${event.lane}, not a lane of round ${event.round}`);
      }
      const candidates = upsertLane(round.candidates, event.lane, { sha: event.sha });
      return ok({ ...state, phase: { ...p, rounds: replaceRound(p.rounds, { ...round, candidates }) } });
    }

    case "CANDIDATE_CHECKED": {
      const round = (p.rounds ?? []).find((r) => r.round === event.round);
      if (!round) return rejected(state, `CANDIDATE_CHECKED for round ${event.round}, which has not started`);
      if (!round.lanes.includes(event.lane)) {
        return rejected(state, `CANDIDATE_CHECKED names lane ${event.lane}, not a lane of round ${event.round}`);
      }
      const candidates = upsertLane(round.candidates, event.lane, { ok: event.ok, ...(event.note ? { note: event.note } : {}) });
      return ok({ ...state, phase: { ...p, rounds: replaceRound(p.rounds, { ...round, candidates }) } });
    }

    case "REVOTE_STARTED": {
      // Plan 06h (A3): the top two lanes go to one revote. Record-only.
      const round = (p.rounds ?? []).find((r) => r.round === event.round);
      if (!round) return rejected(state, `REVOTE_STARTED for round ${event.round}, which has not started`);
      if (event.lanes.length !== 2 || event.lanes.some((l) => !round.lanes.includes(l))) {
        return rejected(state, `REVOTE_STARTED names ${event.lanes.join(", ")}, not two lanes of round ${event.round}`);
      }
      return ok({
        ...state,
        phase: { ...p, rounds: replaceRound(p.rounds, { ...round, revote: { lanes: [...event.lanes], votes: [] } }) },
      });
    }

    case "PICK_VOTE": {
      const round = (p.rounds ?? []).find((r) => r.round === event.round);
      if (!round) return rejected(state, `PICK_VOTE for round ${event.round}, which has not started`);
      if (!round.lanes.includes(event.lane)) {
        return rejected(state, `PICK_VOTE names lane ${event.lane}, not a lane of round ${event.round}`);
      }
      // A seat votes once: a later vote from the same seat replaces the
      // earlier one, exactly like a re-cast ballot.
      const vote = { seat: event.seat, lane: event.lane, why: event.why };
      if (event.revote) {
        if (!round.revote || !round.revote.lanes.includes(event.lane)) {
          return rejected(state, `PICK_VOTE names lane ${event.lane}, not a revote lane of round ${event.round}`);
        }
        const votes = [...round.revote.votes.filter((v) => v.seat !== event.seat), vote];
        return ok({ ...state, phase: { ...p, rounds: replaceRound(p.rounds, { ...round, revote: { ...round.revote, votes } }) } });
      }
      const votes = [...round.votes.filter((v) => v.seat !== event.seat), vote];
      return ok({ ...state, phase: { ...p, rounds: replaceRound(p.rounds, { ...round, votes }) } });
    }

    case "ROUND_REVIEW_SUBMITTED": {
      // Plan 06g2: one seat's review of one lane candidate, recorded while
      // the round runs (no review slot exists yet). Record-only; the round's
      // per-candidate record, which the review buffer groups by candidate.
      const round = (p.rounds ?? []).find((r) => r.round === event.round);
      if (!round) return rejected(state, `ROUND_REVIEW_SUBMITTED for round ${event.round}, which has not started`);
      const candidate = round.candidates.find((c) => c.lane === event.lane);
      if (!candidate?.sha) {
        return rejected(state, `ROUND_REVIEW_SUBMITTED names lane ${event.lane}, which submitted no candidate of round ${event.round}`);
      }
      if (event.review.candidateSha !== candidate.sha) {
        return rejected(state, `ROUND_REVIEW_SUBMITTED for ${event.lane} names candidate ${event.review.candidateSha}, not ${candidate.sha}`);
      }
      const reviews = [...(candidate.reviews ?? []).filter((r) => r.seat !== event.seat), { seat: event.seat, review: event.review }];
      const candidates = upsertLane(round.candidates, event.lane, { reviews });
      return ok({ ...state, phase: { ...p, rounds: replaceRound(p.rounds, { ...round, candidates }) } });
    }

    case "CANDIDATE_PICKED": {
      const round = (p.rounds ?? []).find((r) => r.round === event.round);
      if (!round) return rejected(state, `CANDIDATE_PICKED for round ${event.round}, which has not started`);
      const candidate = round.candidates.find((c) => c.lane === event.lane);
      if (!candidate || candidate.sha !== event.sha) {
        return rejected(state, `CANDIDATE_PICKED names ${event.sha} in lane ${event.lane}, which that lane did not submit`);
      }
      return ok({
        ...state,
        phase: { ...p, rounds: replaceRound(p.rounds, { ...round, picked: { lane: event.lane, sha: event.sha, votes: event.votes } }) },
      });
    }

    case "DECISION_ADDED": {
      // Work packet 2a: a reviewer-discovered decision or a conductor
      // boundary trigger — the other two decision sources besides a
      // worker's disclosure (design §3.3). Bound to the phase's current
      // candidate/contract, same as any other record design §7.1 binds.
      const { decision } = event;
      if (p.decisions.some((d) => d.id === decision.id)) {
        return rejected(state, `decision ${decision.id} already exists`);
      }
      if (!p.candidate || decision.boundCandidateSha !== p.candidate.sha) {
        return rejected(state, `decision ${decision.id} must be bound to the current candidate`);
      }
      if (!sameVersion(decision.boundContractVersion, p.contract.contractVersion)) {
        return rejected(state, `decision ${decision.id} must be bound to the current contract version`);
      }
      return ok({ ...state, phase: { ...p, decisions: [...p.decisions, decision] } });
    }

    case "FINDING_CONFIRMED_REPAIRED": {
      // design §4.2: "a new candidate; checks pass on it; the raising
      // reviewer confirms, in its review of that candidate, that the
      // finding no longer holds". Every clause is checked, not assumed.
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      if (finding.raisedBy !== event.byReviewer) {
        return rejected(
          state,
          `only the raising reviewer (${finding.raisedBy}) may confirm finding ${event.findingId} repaired, not ${event.byReviewer}`,
        );
      }
      if (!p.candidate || event.candidateSha !== p.candidate.sha) {
        return rejected(state, `finding ${event.findingId} must be confirmed against the current candidate`);
      }
      if (event.candidateSha === finding.boundCandidateSha) {
        return rejected(
          state,
          `finding ${event.findingId} cannot be confirmed repaired on the same candidate it was raised on (${finding.boundCandidateSha})`,
        );
      }
      if (!p.checks || p.checks.candidateSha !== event.candidateSha || p.checks.passed !== true) {
        return rejected(state, `checks must have passed on candidate ${event.candidateSha} before finding ${event.findingId} can close`);
      }
      const review = p.reviews[event.byReviewer]?.review;
      const statement = review?.findingStatements.find((s) => s.findingId === event.findingId);
      if (!review || review.candidateSha !== event.candidateSha || !statement || statement.status !== "confirm") {
        return rejected(
          state,
          `finding ${event.findingId} repaired requires ${event.byReviewer}'s review of candidate ${event.candidateSha} to confirm it`,
        );
      }
      const findings = p.findings.map((f) =>
        f.id === event.findingId
          ? { ...f, status: "repaired" as const, repairedByCandidateSha: event.candidateSha }
          : f,
      );
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "FINDING_DISPROVED": {
      if (!event.evidence || event.evidence.trim().length === 0) {
        return rejected(state, `disproving finding ${event.findingId} requires counter-evidence; got none`);
      }
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      if (finding.raisedBy !== event.byReviewer) {
        return rejected(
          state,
          `only the raising reviewer (${finding.raisedBy}) may withdraw finding ${event.findingId}, not ${event.byReviewer}`,
        );
      }
      const findings = p.findings.map((f) =>
        f.id === event.findingId ? { ...f, status: "disproved" as const, disprovedEvidence: event.evidence } : f,
      );
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "FINDING_ACCEPTED_BY_OWNER": {
      // design §4.2: "accepted" requires the owner, and only the owner —
      // and, like every owner command, is bound to (C, K, record version).
      const check = checkFindingAcceptedByOwner(p, event);
      if (!check.ok) return rejected(state, check.reason!);
      return ok({ ...state, phase: applyFindingAcceptedByOwner(p, event) });
    }

    case "FINDING_SEVERITY_LOWERED": {
      // design §3.4: only the owner may lower a finding's severity.
      if (event.by !== "owner") {
        return rejected(state, `only the owner may lower a finding's severity, not '${String(event.by)}'`);
      }
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      const tuple = bindingTupleForRecord(state, event.findingId, {
        candidateSha: event.boundCandidateSha,
        contractVersion: event.boundContractVersion,
        recordVersion: event.boundRecordVersion,
      });
      const check = checkTupleBinding(tuple, p);
      if (!check.ok) return rejected(state, check.reason!);
      const findings = p.findings.map((f) => (f.id === event.findingId ? { ...f, severity: event.severity } : f));
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "FINDING_SEVERITY_CHANGED": {
      // Plan 05e: the evaluator's plan check, or the round panel's vote,
      // changes a finding's severity. Unlike FINDING_SEVERITY_LOWERED (the
      // owner's command) this carries its own reason and may raise severity
      // back only through the recorded round.
      if (event.by !== "evaluator" && event.by !== "panel" && event.by !== "reviewer") {
        return rejected(state, `FINDING_SEVERITY_CHANGED is only evaluator, panel or reviewer, not '${String(event.by)}'`);
      }
      if (!event.reason || event.reason.trim().length === 0) {
        return rejected(state, `a severity change on ${event.findingId} needs a reason`);
      }
      if (event.severity !== "blocking" && event.severity !== "advisory") {
        return rejected(state, `severity must be blocking or advisory, not '${String(event.severity)}'`);
      }
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      const findings = p.findings.map((f) =>
        f.id === event.findingId ? { ...f, severity: event.severity, severityReason: event.reason.trim(), severityChangedBy: event.by } : f,
      );
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "FINDING_VERIFIED": {
      // Plan 05e: record what validated a finding (the record, a run, a
      // citation, or the panel's vote).
      if (typeof event.verified !== "string" || event.verified.trim().length === 0) {
        return rejected(state, `verifying ${event.findingId} needs a non-empty verified marker`);
      }
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      const findings = p.findings.map((f) => (f.id === event.findingId ? { ...f, verified: event.verified.trim() } : f));
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "FINDING_RESOLVED_BY_VOTE": {
      // Plan 05e: a reviewer majority marked the finding's message resolved,
      // so the finding is repaired on the candidate the round reviewed.
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      const findings = p.findings.map((f) =>
        f.id === event.findingId && f.status === "open"
          ? { ...f, status: "repaired" as const, repairedByCandidateSha: event.candidateSha }
          : f,
      );
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "DECISION_CLASS_LOWERED": {
      // design §3.4: only the owner may lower a decision's class.
      if (event.by !== "owner") {
        return rejected(state, `only the owner may lower a decision's class, not '${String(event.by)}'`);
      }
      const decision = p.decisions.find((d) => d.id === event.decisionId);
      if (!decision) return rejected(state, `unknown decision ${event.decisionId}`);
      const tuple = bindingTupleForRecord(state, event.decisionId, {
        candidateSha: event.boundCandidateSha,
        contractVersion: event.boundContractVersion,
        recordVersion: event.boundRecordVersion,
      });
      const check = checkTupleBinding(tuple, p);
      if (!check.ok) return rejected(state, check.reason!);
      const decisions = p.decisions.map((d) =>
        d.id === event.decisionId ? { ...d, class: event.class, version: d.version + 1 } : d,
      );
      return ok({ ...state, phase: { ...p, decisions } });
    }

    case "OWNER_REQUEST_OPENED": {
      return ok({ ...state, phase: { ...p, ownerRequests: [...p.ownerRequests, event.request] } });
    }

    case "OWNER_REQUEST_RESOLVED": {
      const check = checkOwnerRequestResolved(p, event);
      if (!check.ok) return rejected(state, check.reason!);
      return ok({ ...state, phase: applyOwnerRequestResolved(p, event) });
    }

    case "BRIEF_RETRY_ATTEMPTED": {
      // Plan 05k (OD-6): remember that the backstop for each item was retried
      // once on this candidate, so a later park does not dispatch again.
      const retry = event as Extract<Event, { type: "BRIEF_RETRY_ATTEMPTED" }>;
      const keys = new Set(p.briefRetries ?? []);
      for (const id of retry.requestIds) keys.add(`${retry.candidateSha}::${id}`);
      return ok({ ...state, phase: { ...p, briefRetries: [...keys] } });
    }

    case "BRIEFS_RECORDED": {
      // Decision briefs: merge by requestId (a later round's brief for the
      // same item replaces the earlier one). A record-only event, so the
      // views and the status `needs you` line survive a conductor restart
      // via rebuildState folding the log.
      const incoming = (event as Extract<Event, { type: "BRIEFS_RECORDED" }>).briefs ?? [];
      const byId = new Map((p.briefs ?? []).map((b) => [b.requestId, b]));
      for (const brief of incoming) byId.set(brief.requestId, brief);
      return ok({ ...state, phase: { ...p, briefs: [...byId.values()] } });
    }

    case "INTEGRITY_VIOLATED": {
      // design §2.2: a checkout that no longer matches its candidate commit
      // invalidates that gate's result (never counted as passed) and marks
      // the run integrity-violated for the owner. Idempotent: once set, a
      // second occurrence (e.g. a repair round's own check) is still just
      // `true`.
      return ok({ ...state, phase: { ...p, integrityViolated: true } });
    }

    case "ENV_CHECKED": {
      // Plan 05i: the preflight resolved the run's tools at start. Record-only
      // — it stores the resolved paths for the status views and moves no
      // phase state. A later start updates them in place.
      const e = event as Extract<Event, { type: "ENV_CHECKED" }>;
      return ok({ ...state, phase: { ...p, env: { ...(p.env ?? {}), path: e.path, tools: e.tools } } });
    }

    case "SUBMIT_PHASE": {
      // The raw disclosures are stashed on the phase; the submit-phase row
      // itself moves IMPLEMENTING -> FREEZING. FREEZE_COMPLETED is what
      // assembles these into bound Decision records (design §6.2, §7.1).
      return undefined;
    }

    case "REVIEW_TIMED_OUT": {
      // Plan 04a: a review dispatch whose phase has already moved on (a
      // crash-recovery reconciliation after all three reviews landed and the
      // phase entered EVALUATING) is stale. It has no transition row there;
      // clear the lingering in-flight entry so it can never block a later
      // REVIEWING dispatch, and change nothing else.
      const key = `review_${event.reviewer}` as InFlightKey;
      if (!(key in p.inFlight)) return rejected(state, `no in-flight review for ${event.reviewer} to clear`);
      const inFlight = { ...p.inFlight };
      delete inFlight[key];
      return ok({ ...state, phase: { ...p, inFlight } });
    }

    case "EVALUATOR_FINISHED": {
      // Plan 04a: one type's evaluator outcome, a record event inside
      // EVALUATING. It settles that type without moving the phase; next()
      // asks for `evaluation_complete` once every dispatched type has
      // settled. Rejected outside EVALUATING, so a late evaluator cannot
      // settle a different stage.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `EVALUATOR_FINISHED is only valid in EVALUATING, not ${p.phase}`);
      }
      return ok({
        ...state,
        phase: {
          ...p,
          evaluation: withEvaluatorOutcome(p, event.messageType, { settled: true }),
          inFlight: withoutEvaluationInFlight(p, event.messageType),
        },
      });
    }

    case "EVALUATION_TIMED_OUT": {
      // Plan 04a: only THIS type's raw messages are published unchanged,
      // marked unevaluated; the type is settled so the phase can move on once
      // every type has. A record event (no phase change) — EVALUATION_COMPLETED
      // is the transition that leaves EVALUATING.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `EVALUATION_TIMED_OUT is only valid in EVALUATING, not ${p.phase}`);
      }
      const messages = (p.messages ?? []).map((m) =>
        m.type === event.messageType && m.state === "raw" ? { ...m, state: "published" as const, unevaluated: true } : m,
      );
      return ok({
        ...state,
        phase: {
          ...p,
          messages,
          evaluation: withEvaluatorOutcome(p, event.messageType, { settled: true, timedOut: true }),
          inFlight: withoutEvaluationInFlight(p, event.messageType),
        },
      });
    }

    case "EVALUATION_INTERRUPTED": {
      // Plan 04a: one type's evaluator was interrupted by a conductor crash;
      // re-dispatched once, tracked per type.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `EVALUATION_INTERRUPTED is only valid in EVALUATING, not ${p.phase}`);
      }
      return ok({
        ...state,
        phase: {
          ...p,
          evaluation: withEvaluatorOutcome(p, event.messageType, { interruptedOnce: true }),
          inFlight: withoutEvaluationInFlight(p, event.messageType),
        },
      });
    }

    case "PANEL_VOTE": {
      // Plan 04b: one seat's vote on one raw blocker. Record-only inside
      // EVALUATING: the phase leaves only through EVALUATION_COMPLETED once
      // every evaluator AND every panel has settled.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `PANEL_VOTE is only valid in EVALUATING, not ${p.phase}`);
      }
      const panel = p.panel?.blockers?.[event.blockerId];
      if (!panel) return rejected(state, `no panel is open for blocker ${event.blockerId}`);
      const seatKey = String(event.seat);
      if (!panelSeatKeys(p).has(seatKey)) return rejected(state, `panel seat must be 1..${panelSeatKeys(p).size}, not ${String(event.seat)}`);
      const seat = panel.seats?.[seatKey];
      if (seat?.vote !== undefined) return rejected(state, `panel seat ${seatKey} of blocker ${event.blockerId} already voted`);
      if (seat?.unavailable) return rejected(state, `panel seat ${seatKey} of blocker ${event.blockerId} is unavailable`);
      if (event.vote !== "block" && event.vote !== "downgrade") {
        return rejected(state, `a panel vote must be block or downgrade, not ${String(event.vote)}`);
      }
      if (typeof event.reason !== "string" || event.reason.trim().length === 0) {
        return rejected(state, `panel seat ${seatKey} must give a reason for its ${event.vote} vote`);
      }
      // A `block` vote proposes two or three options for the owner: the whole
      // point of an escalation is that the owner has something to choose.
      const options = event.options ?? [];
      if (event.vote === "block") {
        if (options.length < 2 || options.length > 3) {
          return rejected(state, `a block vote must propose two or three options for the owner, got ${options.length}`);
        }
        if (options.some((o) => !o || typeof o.id !== "string" || o.id.length === 0 || typeof o.label !== "string" || o.label.length === 0)) {
          return rejected(state, `a block vote's options each need a non-empty id and label`);
        }
        // Two or three DISTINCT options: a repeated id would collapse the
        // owner's choice to one (round-3 review, advisory B-2).
        const ids = new Set(options.map((o) => o.id));
        if (ids.size !== options.length) {
          return rejected(state, `a block vote's options must have distinct ids`);
        }
        if (new Set(options.map((o) => o.label)).size !== options.length) {
          return rejected(state, `a block vote's options must read differently`);
        }
      }
      return ok({
        ...state,
        phase: {
          ...p,
          panel: withPanelSeat(p, event.blockerId, seatKey, {
            vote: event.vote,
            reason: event.reason.trim(),
            ...(event.vote === "block" ? { options } : {}),
          }),
          inFlight: withoutPanelSeatInFlight(p, event.blockerId, seatKey),
        },
      });
    }

    case "PANEL_SEAT_UNAVAILABLE": {
      // Plan 04b: a seat that did not produce a vote after its own retry.
      // Record-only: the other seats can still decide, and a panel with two
      // unavailable seats becomes `incomplete`.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `PANEL_SEAT_UNAVAILABLE is only valid in EVALUATING, not ${p.phase}`);
      }
      const panel = p.panel?.blockers?.[event.blockerId];
      if (!panel) return rejected(state, `no panel is open for blocker ${event.blockerId}`);
      const seatKey = String(event.seat);
      if (!panelSeatKeys(p).has(seatKey)) return rejected(state, `panel seat must be 1..${panelSeatKeys(p).size}, not ${String(event.seat)}`);
      const seat = panel.seats?.[seatKey];
      if (seat?.vote !== undefined) return rejected(state, `panel seat ${seatKey} of blocker ${event.blockerId} already voted`);
      if (panelSeatSettled(seat)) return rejected(state, `panel seat ${seatKey} of blocker ${event.blockerId} is already unavailable`);
      return ok({
        ...state,
        phase: {
          ...p,
          panel: withPanelSeat(p, event.blockerId, seatKey, {
            unavailable: true,
            ...(event.reason ? { reason: event.reason } : {}),
          }),
          inFlight: withoutPanelSeatInFlight(p, event.blockerId, seatKey),
        },
      });
    }

    case "PANEL_DECIDED": {
      // Plan 04b: the panel's counted verdict. The recorded outcome must be
      // exactly what the votes imply — the conductor may not invent one —
      // and every seat must have settled first.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `PANEL_DECIDED is only valid in EVALUATING, not ${p.phase}`);
      }
      const panel = p.panel?.blockers?.[event.blockerId];
      if (!panel) return rejected(state, `no panel is open for blocker ${event.blockerId}`);
      if (panel.decided) return rejected(state, `blocker ${event.blockerId}'s panel has already decided`);
      if (!panelSeatsSettled(panel, panelSeatKeys(p).size)) {
        return rejected(state, `blocker ${event.blockerId}'s panel still has undecided seats`);
      }
      const computed = panelOutcome(panel, panelSeatKeys(p).size);
      if (event.outcome !== computed) {
        return rejected(
          state,
          `blocker ${event.blockerId}'s votes imply '${computed}', not '${String(event.outcome)}'`,
        );
      }
      if (event.outcome === "escalate") {
        const options = event.options ?? [];
        if (options.length < 2 || options.length > 3) {
          return rejected(state, `an escalated blocker needs two or three options for the owner, got ${options.length}`);
        }
      }
      const blockers = { ...(p.panel?.blockers ?? {}) };
      blockers[event.blockerId] = {
        ...panel,
        decided: {
          outcome: event.outcome,
          ...(event.reason ? { reason: event.reason } : {}),
          ...(event.outcome === "escalate" ? { options: event.options } : {}),
        },
      };
      // Plan 05e: keep the round panel beside the blocker panels.
      return ok({ ...state, phase: { ...p, panel: { blockers, ...(p.panel?.round ? { round: p.panel.round } : {}) } } });
    }

    case "ROUND_PANEL_VOTE": {
      // Plan 05e: one round-panel seat's batched votes on every pending
      // trade-off and blocking finding. Record-only inside EVALUATING.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `ROUND_PANEL_VOTE is only valid in EVALUATING, not ${p.phase}`);
      }
      const seatKey = String(event.seat);
      if (!panelSeatKeys(p).has(seatKey)) return rejected(state, `round panel seat must be 1..${panelSeatKeys(p).size}, not ${String(event.seat)}`);
      const seat = p.panel?.round?.seats?.[seatKey];
      if (seat?.votes !== undefined) return rejected(state, `round panel seat ${seatKey} already voted`);
      if (seat?.unavailable) return rejected(state, `round panel seat ${seatKey} is unavailable`);
      if (!Array.isArray(event.votes) || event.votes.length === 0) {
        return rejected(state, `round panel seat ${seatKey} must vote on at least one item`);
      }
      for (const vote of event.votes) {
        if (!vote || typeof vote.messageId !== "string" || vote.messageId.length === 0) {
          return rejected(state, `every round panel vote must name a messageId`);
        }
        if (vote.verdict !== "keep" && vote.verdict !== "drop" && vote.verdict !== "downgrade") {
          return rejected(state, `a round panel verdict must be keep, drop or downgrade, not ${String(vote.verdict)}`);
        }
        if (typeof vote.reason !== "string" || vote.reason.trim().length === 0) {
          return rejected(state, `round panel seat ${seatKey} must give a reason for ${vote.messageId}`);
        }
      }
      return ok({
        ...state,
        phase: { ...p, panel: withRoundPanelSeat(p, seatKey, { votes: event.votes }), inFlight: withoutRoundPanelSeatInFlight(p, seatKey) },
      });
    }

    case "ROUND_PANEL_SEAT_UNAVAILABLE": {
      // Plan 05e: the round panel's one-retry rule, exactly like a blocker
      // seat's. With two seats unavailable the remaining real votes decide.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `ROUND_PANEL_SEAT_UNAVAILABLE is only valid in EVALUATING, not ${p.phase}`);
      }
      const seatKey = String(event.seat);
      if (!panelSeatKeys(p).has(seatKey)) return rejected(state, `round panel seat must be 1..${panelSeatKeys(p).size}, not ${String(event.seat)}`);
      const seat = p.panel?.round?.seats?.[seatKey];
      if (seat?.votes !== undefined) return rejected(state, `round panel seat ${seatKey} already voted`);
      if (seat?.unavailable === true && (seat.dispatches ?? 0) >= 2) {
        return rejected(state, `round panel seat ${seatKey} is already unavailable`);
      }
      return ok({
        ...state,
        phase: {
          ...p,
          panel: withRoundPanelSeat(p, seatKey, { unavailable: true }),
          inFlight: withoutRoundPanelSeatInFlight(p, seatKey),
        },
      });
    }

    case "ROUND_PANEL_DECIDED": {
      // Plan 05e: the seats' batched votes are counted and stamped on each
      // item message so its file shows the three reasons after the round.
      if (p.phase !== "EVALUATING") {
        return rejected(state, `ROUND_PANEL_DECIDED is only valid in EVALUATING, not ${p.phase}`);
      }
      if (p.panel?.round?.decided) return rejected(state, "the round panel has already decided");
      if (!Array.isArray(event.decisions) || event.decisions.length === 0) {
        return rejected(state, "a round panel decision must cover at least one item");
      }
      const round = p.panel?.round ?? {};
      const messages = (p.messages ?? []).map((m) => {
        const decision = event.decisions.find((d) => d.messageId === m.id);
        if (!decision) return m;
        const panelVotes: Array<{ seat: number; verdict: string; reason: string }> = [];
        for (const seatKey of ["1", "2", "3"]) {
          const vote = round.seats?.[seatKey]?.votes?.find((v) => v.messageId === m.id);
          if (vote) panelVotes.push({ seat: Number(seatKey), verdict: vote.verdict, reason: vote.reason });
        }
        return { ...m, panelOutcome: decision.outcome, panelVotes };
      });
      return ok({ ...state, phase: { ...p, messages, panel: { ...(p.panel ?? {}), round: { ...round, decided: true } } } });
    }

    case "CANDIDATE_APPROVED": {
      // Plan 05e: the three reviewers approved this candidate with no open
      // blocking finding. Record-only; the tree is what an amendment-only
      // resubmission is compared against.
      if (typeof event.candidateSha !== "string" || event.candidateSha.length === 0 || typeof event.tree !== "string" || event.tree.length === 0) {
        return rejected(state, "CANDIDATE_APPROVED must carry the candidate sha and its tree");
      }
      const already = (p.approvedCandidates ?? []).some((a) => a.candidateSha === event.candidateSha);
      if (already) return ok(state);
      return ok({
        ...state,
        phase: { ...p, approvedCandidates: [...(p.approvedCandidates ?? []), { candidateSha: event.candidateSha, tree: event.tree }] },
      });
    }

    case "CRITERION_REVERTED": {
      // Plan 01g: the owner's correction naming an amendment id restores the
      // criterion's original wording. A phase with a candidate has its own
      // transition rows (transitions.ts) that also invalidate the evidence
      // bound to the replaced contract version and return to CHECKING. This
      // handler is the fallback for a phase with no candidate yet
      // (IMPLEMENTING/FREEZING/REPAIRING), where there is no evidence to
      // invalidate: it only restores the wording so the next freeze binds to
      // it. Both paths reject an unknown, not-yet-applied or already-reverted
      // amendment, and the row guard and this handler agree on validity so
      // next() can never emit an event the reducer refuses.
      const decision = p.decisions.find((d) => d.amendment?.id === event.amendmentId);
      if (!decision || !decision.amendment) {
        return rejected(state, `unknown amendment ${event.amendmentId}`);
      }
      if (decision.amendment.status === "reverted") {
        return rejected(state, `amendment ${event.amendmentId} is already reverted`);
      }
      if (decision.amendment.status !== "applied") {
        return rejected(state, `amendment ${event.amendmentId} has not been applied, so there is no wording to restore`);
      }
      if (!Array.isArray(event.newAcceptance) || event.newAcceptance.length === 0 || event.newAcceptance.some((a) => typeof a !== "string" || a.length === 0)) {
        return rejected(state, `reverting amendment ${event.amendmentId} needs the restored acceptance list`);
      }
      if (!p.contract.acceptance.includes(decision.amendment.proposedWording)) {
        return rejected(state, `amendment ${event.amendmentId}'s wording is not in the current contract, so there is nothing to restore`);
      }
      const decisions = p.decisions.map((d) =>
        d.id === decision.id
          ? {
              ...d,
              version: d.version + 1,
              boundContractVersion: event.newContractVersion,
              amendment: { ...d.amendment!, status: "reverted" as const, revertedAt: event.at },
            }
          : d,
      );
      return ok({
        ...state,
        phase: {
          ...p,
          contract: { ...p.contract, acceptance: event.newAcceptance, contractVersion: event.newContractVersion },
          candidate: p.candidate && { sha: p.candidate.sha, contractVersion: event.newContractVersion },
          decisions,
        },
      });
    }

    case "NOTE_ADDED": {
      // §7.4 `note` (conductor state): queued for the next worker attempt's
      // prompt. Record-only — it moves no phase.
      if (event.phaseId !== p.phaseId) {
        return rejected(state, `note is for phase ${event.phaseId}, but this run is on phase ${p.phaseId}`);
      }
      if (!event.text || event.text.trim().length === 0) {
        return rejected(state, "a note must carry non-empty text");
      }
      return ok({ ...state, phase: { ...p, ownerNotes: [...(p.ownerNotes ?? []), event.text] } });
    }

    case "DIRECTIVE_ADDED": {
      // Plan 01i: a numbered owner directive. Record-only (moves no phase
      // state name) but binding: every later prompt quotes it verbatim
      // until it is withdrawn. The conductor assigns the id/seq and the
      // delivery targets; reduce() only stores the record, exactly like
      // NOTE_ADDED's queue.
      const directive = event.directive;
      if (!directive || typeof directive.id !== "string" || directive.id.length === 0) {
        return rejected(state, "an owner directive must carry its id (OD-n)");
      }
      if (typeof directive.text !== "string" || directive.text.trim().length === 0) {
        return rejected(state, "an owner directive must carry the text the owner sent");
      }
      if (directive.scope !== "phase" && directive.scope !== "program") {
        return rejected(state, `owner directive ${directive.id} must have scope phase or program`);
      }
      const existing = p.ownerDirectives ?? [];
      if (existing.some((d) => d.id === directive.id)) {
        return rejected(state, `owner directive ${directive.id} already exists`);
      }
      const added = { ...directive, targets: directive.targets ?? [], deliveries: directive.deliveries ?? {} };
      // Kept in ascending id order, never in the order the inbox happened to
      // hand the files over: a lexicographic scan applies `cmd-prog-ODP-10`
      // before `cmd-prog-ODP-2`, and prompts and the status must still read
      // oldest → newest. `seq` is the number in the id (`OD-n` or `ODP-n`).
      const ownerDirectives = [...existing, added].sort((a, b) => a.seq - b.seq);
      return ok({ ...state, phase: { ...p, ownerDirectives } });
    }

    case "DIRECTIVE_WITHDRAWN": {
      // Plan 01i: `withdraw OD-n`. Record-only: the record stays and the
      // status shows it withdrawn; every later prompt omits it.
      const directive = (p.ownerDirectives ?? []).find((d) => d.id === event.directiveId);
      if (!directive) return rejected(state, `unknown owner directive ${event.directiveId}`);
      if (directive.status === "withdrawn") {
        return rejected(state, `owner directive ${event.directiveId} is already withdrawn`);
      }
      const ownerDirectives = (p.ownerDirectives ?? []).map((d) =>
        d.id === event.directiveId ? { ...d, status: "withdrawn" as const, withdrawnAt: event.at } : d,
      );
      return ok({ ...state, phase: { ...p, ownerDirectives } });
    }

    case "DIRECTIVE_DELIVERED": {
      // Plan 01i: the observed outcome of the immediate steer to one live
      // agent, keyed by target. Record-only; a later record for the same
      // target (a re-dispatched agent) is the truer one.
      const directive = (p.ownerDirectives ?? []).find((d) => d.id === event.directiveId);
      if (!directive) return rejected(state, `unknown owner directive ${event.directiveId}`);
      if (event.state !== "delivered" && event.state !== "delivery-uncertain") {
        return rejected(state, `owner directive ${event.directiveId} delivery must be delivered or delivery-uncertain`);
      }
      const ownerDirectives = (p.ownerDirectives ?? []).map((d) =>
        d.id === event.directiveId ? { ...d, deliveries: { ...d.deliveries, [event.target]: event.state } } : d,
      );
      return ok({ ...state, phase: { ...p, ownerDirectives } });
    }

    case "OWNER_INPUT_RECORDED": {
      // Plan 2d (§7.4/§9.3): the recorded effect of one input the owner
      // sent. Record-only (moves no phase state); keyed by the inbox
      // command id so a later, more definitive record (e.g. a recovered
      // steer's `delivered` after its `delivery-uncertain`) updates rather
      // than duplicates. Newest wins — the conductor only ever writes this
      // after observing the effect, so the later record is the truer one.
      const input = event.input;
      if (!input || typeof input.id !== "string" || input.id.length === 0) {
        return rejected(state, "an owner-input record must name its inbox command id");
      }
      if (typeof input.text !== "string" || input.text.trim().length === 0) {
        return rejected(state, "an owner-input record must carry the text the owner sent");
      }
      const existing = p.ownerInputs ?? [];
      const ownerInputs = existing.some((i) => i.id === input.id)
        ? existing.map((i) => (i.id === input.id ? { ...i, ...input } : i))
        : [...existing, input];
      return ok({ ...state, phase: { ...p, ownerInputs } });
    }

    case "OWNER_REQUEST_MARKED_UNNEEDED": {
      // §7.4 `unneeded` / §11.4's "unnecessary escalations" pilot metric:
      // recorded, not resolved (nothing was answered). Record-only, and the
      // request deliberately STAYS `open`: closing it would (a) reject a
      // follow-up OWNER_REQUEST_RESOLVED as "already unneeded" and (b) for
      // the record-less fallback request (no candidate/decision/finding)
      // leave AWAITING_OWNER with no open request and no command that can
      // unstick it — the blocking finding on this candidate. The metric
      // lives in `unneededRequestIds`, so the owner can still grant/stop/
      // repair the request afterwards.
      const request = p.ownerRequests.find((r) => r.id === event.requestId);
      if (!request) return rejected(state, `unknown owner request ${event.requestId}`);
      if (request.status !== "open") {
        return rejected(state, `owner request ${event.requestId} is already ${request.status}, not open`);
      }
      const already = p.unneededRequestIds ?? [];
      if (already.includes(event.requestId)) {
        return rejected(state, `owner request ${event.requestId} is already marked unneeded`);
      }
      return ok({ ...state, phase: { ...p, unneededRequestIds: [...already, event.requestId] } });
    }

    case "NOTES_DELIVERED": {
      // §7.4 `note`: the conductor delivered the first `count` queued notes
      // in a worker attempt's prompt; later attempts send only the rest, so
      // a note reaches the NEXT attempt and does not keep steering every
      // one (finding F-p2b-...-B-2). Record-only.
      if (event.phaseId !== p.phaseId) {
        return rejected(state, `notes are for phase ${event.phaseId}, but this run is on phase ${p.phaseId}`);
      }
      if (!Number.isInteger(event.count) || event.count <= 0) {
        return rejected(state, "a notes-delivered event must carry a positive integer count");
      }
      return ok({ ...state, phase: { ...p, deliveredNoteCount: (p.deliveredNoteCount ?? 0) + event.count } });
    }

    case "FLAKE_OBSERVED": {
      // Plan 05d: one new failing test that passed when re-run alone. A
      // record-only event: it names the test, the command, both exit statuses
      // and the machine's load average, so the classification is evidence the
      // status, `tt summary` and a restart all read.
      if (typeof event.name !== "string" || event.name.trim().length === 0) {
        return rejected(state, "a flake observation must name the test that flaked");
      }
      if (typeof event.command !== "string" || event.command.trim().length === 0) {
        return rejected(state, "a flake observation must name the check command that failed");
      }
      const flakes = p.flakes ?? [];
      return ok({
        ...state,
        phase: {
          ...p,
          flakes: [
            ...flakes,
            {
              name: event.name,
              command: event.command,
              ...(event.rerunCommand !== undefined ? { rerunCommand: event.rerunCommand } : {}),
              failingExitCode: event.failingExitCode ?? null,
              rerunExitCodes: Array.isArray(event.rerunExitCodes) ? event.rerunExitCodes : [],
              ...(event.loadAverage !== undefined ? { loadAverage: event.loadAverage } : {}),
              savedRound: event.savedRound === true,
              ...(event.candidateSha !== undefined ? { candidateSha: event.candidateSha } : {}),
            },
          ],
        },
      });
    }

    case "LAUNCH_RETRIED": {
      // Plan 05d / finding #33: a launch missed its hello and was retried
      // once with a longer limit. Record-only; the metrics count it.
      if (typeof event.role !== "string" || event.role.length === 0) {
        return rejected(state, "a launch retry must name the role it retried");
      }
      return ok(state);
    }

    case "DECISION_MATCHED": {
      // Plan 2c (design §3.3): a reviewer's own discovery is the same choice
      // as another listed record. The discovery stops being a separate,
      // votable record; the reviewer is recorded on the matched record.
      const discovery = p.decisions.find((d) => d.id === event.decisionId);
      const target = p.decisions.find((d) => d.id === event.sameAs);
      if (!discovery) return rejected(state, `unknown decision ${event.decisionId}`);
      if (!target) return rejected(state, `unknown decision ${event.sameAs}`);
      if (discovery.id === target.id) return rejected(state, "a decision cannot match itself");
      if (discovery.source !== "reviewer-discovered") {
        return rejected(state, `only a reviewer-discovered record can be matched, not ${discovery.source} ${discovery.id}`);
      }
      if (!isLiveDecision(discovery) || !isLiveDecision(target)) {
        return rejected(state, `cannot match superseded records (${discovery.id} → ${target.id})`);
      }
      const decisions = p.decisions.map((d) => {
        if (d.id === discovery.id) return { ...d, supersededBy: `same as ${target.id}`, version: d.version + 1 };
        if (d.id === target.id) {
          const seen = d.alsoSeenBy ?? [];
          return seen.includes(event.reviewer) ? d : { ...d, alsoSeenBy: [...seen, event.reviewer] };
        }
        return d;
      });
      return ok({ ...state, phase: { ...p, decisions } });
    }

    case "FINDING_ALSO_RAISED": {
      // Plan 2c: "same as F-…" — the reviewer agrees with an open finding
      // instead of filing a duplicate. Record-only.
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      if (finding.status !== "open") return rejected(state, `finding ${finding.id} is ${finding.status}, not open`);
      const also = finding.alsoRaisedBy ?? [];
      if (finding.raisedBy === event.reviewer || also.includes(event.reviewer)) return ok(state);
      const findings = p.findings.map((f) => (f.id === finding.id ? { ...f, alsoRaisedBy: [...also, event.reviewer] } : f));
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "MESSAGE_RAISED":
    case "MESSAGE_PUBLISHED":
    case "MESSAGE_MERGED":
    case "MESSAGE_DROPPED":
    case "MESSAGE_RESOLVED":
    case "MESSAGE_SUPERSEDED": {
      const result = applyMessageEvent(p.messages ?? [], event);
      if (!result.ok) return rejected(state, result.reason);
      return ok({ ...state, phase: { ...p, messages: result.messages } });
    }

    case "MESSAGE_ADDRESS_REPORTED": {
      // Plan 04a item 4: the evaluator's report on an owner-refused message.
      // Record-only (no state change): `addressed: true` is separately a
      // MESSAGE_RESOLVED; `false` is recorded here so the ledger can tell
      // "checked and not addressed" from "never checked" (findings M-20/A-21).
      const message = (p.messages ?? []).find((m) => m.id === event.messageId);
      if (!message) return rejected(state, `unknown message ${event.messageId}`);
      const binding = checkMessageBinding(message, event);
      if (!binding.ok) return rejected(state, binding.reason!);
      const messages = (p.messages ?? []).map((m) =>
        m.id === event.messageId
          ? { ...m, addressedReport: { addressed: event.addressed, ...(event.reason ? { reason: event.reason } : {}), at: event.at } }
          : m,
      );
      return ok({ ...state, phase: { ...p, messages } });
    }

    case "MESSAGE_CARRIED": {
      const message = (p.messages ?? []).find((m) => m.id === event.messageId);
      if (!message) return rejected(state, `unknown message ${event.messageId}`);
      const result = applyCarryWithContract(p.messages ?? [], message, event, p.contract.contractVersion);
      if (!result.ok) return rejected(state, result.reason);
      return ok({ ...state, phase: { ...p, messages: result.messages } });
    }

    // Plan 05j: the entry ledger. A refused link is not an error: the message
    // opens its own entry at projection time, and the projection logs the
    // refusal (the event already stays in the log).
    case "ENTRY_OPENED":
    case "MESSAGE_LINKED":
    case "ENTRY_RETITLED":
    case "ENTRY_SPLIT":
    case "ENTRY_STATE":
    case "ENTRY_MERGED_BY_OWNER": {
      const result = applyEntryEvent(p.entries ?? [], event as EntryEvent, p.messages ?? []);
      if (!result.ok) return rejected(state, result.reason);
      return ok({ ...state, phase: { ...p, entries: result.entries } });
    }

    // Plan 05j: the round's curator pass is done for this candidate, so the
    // evaluators may start. Record-only on the entries themselves.
    case "ENTRY_CURATED": {
      return ok({ ...state, phase: { ...p, curatedFor: event.candidateSha } });
    }

    // Plan 05j: the review lint's own record. Record-only: nothing about the
    // entries changes, the event is the log's copy of the view's first line.
    case "REVIEW_LINT_FAILED": {
      return ok(state);
    }

    case "OWNER_VERDICT": {
      const message = (p.messages ?? []).find((m) => m.id === event.messageId);
      if (!message) return rejected(state, `unknown message ${event.messageId}`);
      if (message.state === "raw") {
        return rejected(state, `message ${event.messageId} is not yet frozen; it must be published before a verdict`);
      }
      const result = applyMessageEvent(p.messages ?? [], event);
      if (!result.ok) return rejected(state, result.reason);
      let messages = result.messages;
      let findings = p.findings;
      if (event.verdict === "refuse") {
        if (p.phase === "DONE") {
          // After DONE a refusal is a recorded follow-up: it changes no phase
          // state and does not reopen the run (contract §1.3).
          messages = messages.map((m) => (m.id === event.messageId ? { ...m, followUp: true } : m));
        } else if (p.candidate) {
          // A refusal before DONE always raises an owner-authored blocking
          // finding, in every pre-DONE phase — REVIEWING included, but also
          // CHECKING/PROBING/ACCEPTED/... — so the refusal can never settle
          // silently and accept(C, K) cannot hold on this candidate
          // (predicate.ts's open-blocking-finding clause). A message only
          // exists once a candidate does, so a pre-DONE refusal always has
          // one to bind the finding to.
          const finding: Finding = {
            id: `F-${p.phaseId}-owner-${p.findings.length + 1}`,
            version: 1,
            phaseId: p.phaseId,
            kind: "defect",
            severity: "blocking",
            evidence: event.reason?.trim().length ? event.reason : `the owner refused message ${event.messageId}: ${message.title}`,
            raisedBy: "owner",
            status: "open",
            boundCandidateSha: p.candidate.sha,
          };
          findings = [...p.findings, finding];
        }
      }
      return ok({ ...state, phase: { ...p, messages, findings } });
    }

    case "MISS_RECORDED": {
      // §3.5/§10.4 `s`: the observed miss sample. Record-only.
      if (!event.recordId || event.recordId.trim().length === 0) {
        return rejected(state, "a miss must name the sampled record it refers to");
      }
      const misses = p.misses ?? [];
      if (misses.includes(event.recordId)) {
        return rejected(state, `record ${event.recordId} is already marked as a miss`);
      }
      return ok({ ...state, phase: { ...p, misses: [...misses, event.recordId] } });
    }

    default:
      return undefined;
  }
}

export function reduce(state: State, event: unknown): ReduceResult {
  try {
    if (!event || typeof event !== "object" || typeof (event as { type?: unknown }).type !== "string") {
      return rejected(state, "malformed event: missing string 'type'");
    }
    const ev = event as Event;
    if (!KNOWN_EVENT_TYPES.has(ev.type)) {
      return rejected(state, `unknown event type: ${String((event as { type: unknown }).type)}`);
    }

    // REVISE and AMEND get a dedicated, human-readable staleness check up
    // front (design §7.1) rather than a boolean guard failing silently into
    // the generic "no rule matched" message — see the stale-binding tests.
    if (ev.type === "REVISE") {
      const current = currentVersionsFor(state.phase, ev.targetRecordId);
      if (!current) {
        return rejected(state, `revise target ${ev.targetRecordId} does not exist in phase ${state.phase.phaseId}`);
      }
      const check = checkBinding(
        { candidateSha: ev.boundCandidateSha, contractVersion: ev.boundContractVersion, recordVersion: ev.boundRecordVersion },
        current,
        current.label,
      );
      if (!check.ok) return rejected(state, check.reason!);
    }
    if (ev.type === "AMEND") {
      if (!sameVersion(ev.replacingContractVersion, state.phase.contract.contractVersion)) {
        return rejected(
          state,
          `phase ${state.phase.phaseId}'s contract changed v${ev.replacingContractVersion.snapshot} → v${state.phase.contract.contractVersion.snapshot} since you viewed it`,
        );
      }
      // design §4.2/§4.3: an amend may resolve `contract` findings directly
      // — but only ones that exist, are the right kind, and are still open.
      for (const findingId of ev.resolvesFindingIds ?? []) {
        const finding = state.phase.findings.find((f) => f.id === findingId);
        if (!finding) return rejected(state, `amend cannot resolve unknown finding ${findingId}`);
        if (finding.kind !== "contract") {
          return rejected(state, `amend can only resolve 'contract' findings; ${findingId} is '${finding.kind}'`);
        }
        if (finding.status !== "open") {
          return rejected(state, `finding ${findingId} is already ${finding.status}, not open`);
        }
      }
    }

    // A review that states the same correction's disposition twice is
    // malformed at ingestion (F02): reject it before it can become state,
    // so `addressed` never has to choose between contradictory statements.
    if (ev.type === "REVIEW_SUBMITTED") {
      const issue = reviewIngestionIssue(ev.review);
      if (issue) return rejected(state, issue);
    }

    // The raw disclosures alongside SUBMIT_PHASE are stashed before the
    // phase-table row (which only moves phase -> FREEZING) runs. They stay
    // pending until FREEZE_COMPLETED assembles and binds them.
    let working = state;
    if (ev.type === "SUBMIT_PHASE") {
      working = {
        ...state,
        phase: { ...state.phase, pendingDisclosures: ev.disclosures, pendingPrior: ev.prior, pendingDispute: ev.dispute },
      };
    }

    const rows = rowsFor(working, ev.type);
    if (rows.length === 0) {
      const recordResult = applyRecordEvent(working, ev);
      if (recordResult) return recordResult;
      return rejected(
        state,
        `out of order or unknown transition: '${ev.type}' has no rule from phase=${working.phase.phase} run=${working.run}`,
      );
    }

    const matching = rows.find((r) => {
      try {
        return r.guard(working, ev);
      } catch {
        return false;
      }
    });

    if (!matching) {
      const recordResult = applyRecordEvent(working, ev);
      if (recordResult) return recordResult;
      return rejected(
        state,
        `event '${ev.type}' arrived in phase=${working.phase.phase} but no guard accepted it (tried: ${rows
          .map((r) => r.guardName)
          .join(", ")})`,
      );
    }

    const nextState = matching.apply(working, ev);
    return ok(nextState);
  } catch (err) {
    return rejected(state, `reduce threw and was recovered: ${String((err as Error)?.message ?? err)}`);
  }
}
