// next(state) -> actions[]: the ONLY place that decides what happens next.
// A pure function of state alone (no event needed) that, for every
// non-terminal resting state, returns the outstanding obligations that are
// not already in flight (design §9.3's "intent" bookkeeping, tracked in
// `phase.inFlight`). Terminal states (DONE, BLOCKED, AWAITING_OWNER) return
// [], and so does every state while the run is PAUSED for budget.
//
// This replaces the earlier AUTOMATIC/REACTIVE row split (round-1 review,
// item 1): there is no longer a class of row next() cannot decide, because
// `inFlight` distinguishes "just entered this state, dispatch the work" from
// "already dispatched, waiting on a completion event" — the ambiguity that
// previously made rows like `checks-passed` look like a trivial "always"
// guard next() could not act on.
//
// test/contract/transitions.test.ts makes the transition table's `actions`
// column normative against this function: for every row,
// `next(reduce(fromState, trigger).state)` must equal that row's actions
// exactly, and calling next() again after the matching ACTION_STARTED
// returns [] (no double dispatch) for the dispatch-shaped ones.

import { finalCheckOf } from "./checks.ts";
import { gateCommandOf } from "./gate.ts";
import {
  accept,
  amendmentToApply,
  blockersNeedingPanel,
  evaluationSettled,
  PANEL_SEATS,
  panelSeatSettled,
  panelSeatsSettled,
  resolvedCorrectionIdsFor,
  roundPanelItemsNeedingVote,
  roundPanelSeatSettled,
  roundPanelSeatsSettled,
  sameVersion,
  typesNeedingEvaluation,
} from "./predicate.ts";
import type { Action, InFlightKey, PhaseState, Reviewer, State } from "./types.ts";

function hasValidReview(phase: PhaseState, who: Reviewer): boolean {
  const review = phase.reviews[who]?.review;
  if (!review || !phase.candidate) return false;
  return (
    review.candidateSha === phase.candidate.sha && sameVersion(review.contractVersion, phase.contract.contractVersion)
  );
}

export function next(state: State): Action[] {
  // While the run is paused for budget or blocked on its environment,
  // nothing dispatches for any phase.
  if (state.run !== "RUN_ACTIVE") return [];

  const p = state.phase;

  switch (p.phase) {
    case "READY":
      // A one-shot decision (like REPAIRING's), not an in-flight dispatch:
      // it fires ATTEMPT_STARTED immediately and is gone. Distinct from
      // IMPLEMENTING's "dispatch_worker" (a real, trackable in-flight
      // dispatch waiting on a worker) even though both end up running a
      // worker, so ACTION_STARTED bookkeeping is never asked for a phase
      // that has no worktree yet.
      return [{ type: "start_attempt" }];

    // Plan 04a: the base baseline is its own stage, with its own deadline —
    // never hidden inside the worker's first dispatch. An identical base
    // tree with a recorded baseline skips it (the conductor routes READY
    // straight to IMPLEMENTING).
    case "BASELINE":
      return p.inFlight.run_baseline ? [] : [{ type: "run_baseline" }];

    case "IMPLEMENTING":
      return p.inFlight.dispatch_worker ? [] : [{ type: "dispatch_worker" }];

    case "FREEZING":
      return p.inFlight.freeze ? [] : [{ type: "freeze" }];

    case "CHECKING":
      if (!p.candidate) return [];
      return p.inFlight.run_checks ? [] : [{ type: "run_checks", candidateSha: p.candidate.sha }];

    case "PROBING":
      if (!p.candidate) return [];
      return p.inFlight.dispatch_probe
        ? []
        : [{ type: "dispatch_probe", candidateSha: p.candidate.sha, head: p.integrationHead }];

    case "REVIEWING": {
      if (!p.candidate) return [];
      const actions: Action[] = [];
      for (const who of ["M", "A", "B"] as const) {
        const key = `review_${who}` as const;
        if (!hasValidReview(p, who) && !p.inFlight[key]) {
          actions.push({ type: "dispatch_review", reviewer: who });
        }
      }
      return actions;
    }

    // Plan 04a: one fresh evaluator PER MESSAGE TYPE that has work this
    // round (batched per type, not one agent for all types — plan item 3 and
    // vision §8). The phase only asks for `evaluation_complete` once every
    // dispatched type has finished or timed out. Acceptance can never outrun
    // evaluation.
    case "EVALUATING": {
      const pending = typesNeedingEvaluation(p).filter((t) => p.evaluation?.types?.[t]?.settled !== true);
      const actions: Action[] = [];
      for (const t of pending) {
        if (!p.inFlight[`dispatch_evaluation_${t}`]) actions.push({ type: "dispatch_evaluation", messageType: t });
      }
      // Plan 04b: three fresh panel seats per raw blocker, dispatched in
      // parallel as their own logged actions, each with its own deadline.
      for (const blockerId of blockersNeedingPanel(p)) {
        const panel = p.panel!.blockers![blockerId];
        if (panel.decided) continue;
        for (const seat of PANEL_SEATS) {
          if (panelSeatSettled(panel.seats?.[String(seat)])) continue;
          const key = `dispatch_panel_${blockerId}_${seat}` as InFlightKey;
          if (!p.inFlight[key]) actions.push({ type: "dispatch_panel", blockerId, seat });
        }
      }
      // Plan 05e: the round panel votes on every published trade-off that no
      // three reviewers balloted and on every published blocking finding. It
      // is dispatched only once the evaluators have published (its items are
      // a fact of the published messages), batched as ONE panel per round.
      const evaluatorsSettled = pending.length === 0;
      const roundItems = evaluatorsSettled ? roundPanelItemsNeedingVote(p) : [];
      if (evaluatorsSettled && roundItems.length > 0 && !p.panel?.round?.decided) {
        for (const seat of PANEL_SEATS) {
          if (roundPanelSeatSettled(p.panel?.round?.seats?.[String(seat)])) continue;
          const key = `dispatch_round_panel_${seat}` as InFlightKey;
          if (!p.inFlight[key]) actions.push({ type: "dispatch_round_panel", seat });
        }
      }
      if (actions.length > 0) return actions;
      // Every seat has settled; the panel's own verdict is next(), not the
      // agent's — one action per undecided blocker, computed from the votes.
      for (const blockerId of blockersNeedingPanel(p)) {
        const panel = p.panel!.blockers![blockerId];
        if (!panel.decided && panelSeatsSettled(panel)) return [{ type: "panel_decide", blockerId }];
      }
      if (
        evaluatorsSettled &&
        roundItems.length > 0 &&
        !p.panel?.round?.decided &&
        roundPanelSeatsSettled(p.panel?.round)
      ) {
        return [{ type: "round_panel_decide" }];
      }
      return evaluationSettled(p) ? [{ type: "evaluation_complete" }] : [];
    }

    case "RESOLVING": {
      if (!p.candidate) return [];
      const C = p.candidate.sha;
      const K = p.contract.contractVersion;
      // Plan 01g: a passing amendment is applied before anything else — it
      // rewrites one acceptance item and returns the phase to a fresh
      // attempt under the new contract version, so the next candidate is
      // judged against the new wording. It never waits for the owner and
      // never consumes a repair round by itself.
      const amendment = amendmentToApply(p, C, K);
      if (amendment) return [{ type: "apply_amendment", decisionId: amendment.id }];
      if (accept(p, C, K)) {
        // Plan 06c: an acceptable candidate with a declared final check runs
        // it first, once; the FINAL_CHECKING stage asks for it.
        if (finalCheckOf(p.contract) && p.finalChecksPassedFor !== C) return [{ type: "final_check_required" }];
        // Plan 01f: an acceptable candidate with a declared gate is gated
        // first; the gate stage then asks for the gate command itself. A
        // gate-less phase accepts exactly as it did before plan 01f.
        if (gateCommandOf(p.contract)) return [{ type: "gate_required" }];
        return [{ type: "accept", resolvedCorrectionIds: resolvedCorrectionIdsFor(p, C, K) }];
      }
      return [{ type: "resolving_incomplete" }];
    }

    // Plan 06c: the plan's final check, run once for the candidate about to
    // be accepted.
    case "FINAL_CHECKING":
      if (!p.candidate) return [];
      return p.inFlight.run_final_checks ? [] : [{ type: "run_final_checks", candidateSha: p.candidate.sha }];

    // Plan 01f: the conductor's own gate command, run once for this
    // candidate (a recorded pass for an identical tree is reused instead,
    // and the action is still reported as dispatched — the reuse decision is
    // the effect layer's, not the FSM's).
    case "GATING":
      if (!p.candidate) return [];
      return p.inFlight.run_gate ? [] : [{ type: "run_gate", candidateSha: p.candidate.sha }];

    case "ACCEPTED":
      if (!p.candidate || !p.probe?.probedI) return [];
      return [{ type: "publish_intent", expectedHead: p.integrationHead, candidateI: p.probe.probedI }];

    case "PUBLISHING":
      if (!p.candidate || !p.probe?.probedI) return [];
      return p.inFlight.publish_cas
        ? []
        : [{ type: "publish_cas", expectedHead: p.integrationHead, candidateI: p.probe.probedI }];

    case "REPAIRING":
      return p.repairRoundsUsed < p.repairRoundsGranted
        ? [{ type: "repair_attempt_started" }]
        : [{ type: "repair_budget_exhausted" }];

    case "DONE":
    case "BLOCKED":
    case "AWAITING_OWNER":
    default:
      return [];
  }
}
