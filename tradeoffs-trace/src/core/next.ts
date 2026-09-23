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

import { accept, resolvedCorrectionIdsFor, sameVersion } from "./predicate.ts";
import type { Action, PhaseState, Reviewer, State } from "./types.ts";

function hasValidReview(phase: PhaseState, who: Reviewer): boolean {
  const review = phase.reviews[who]?.review;
  if (!review || !phase.candidate) return false;
  return (
    review.candidateSha === phase.candidate.sha && sameVersion(review.contractVersion, phase.contract.contractVersion)
  );
}

export function next(state: State): Action[] {
  // While the run is paused for budget, nothing dispatches for any phase.
  if (state.run === "RUN_PAUSED_BUDGET") return [];

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

    case "RESOLVING": {
      if (!p.candidate) return [];
      const C = p.candidate.sha;
      const K = p.contract.contractVersion;
      if (accept(p, C, K)) {
        return [{ type: "accept", resolvedCorrectionIds: resolvedCorrectionIdsFor(p, C, K) }];
      }
      return [{ type: "resolving_incomplete" }];
    }

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
