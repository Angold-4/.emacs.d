// Plan 06k2 (A1): the two-lane round's own sub-phase, as the loop view reads
// it. During a round the phase state stays IMPLEMENTING while the lane
// workers write; once both lanes have submitted, the conductor reviews each
// passing candidate (LANE REVIEW) and then runs the pick turn (PICK). The
// loop tape and the status `loop` row use this so the attempt clock is not
// shown as still implementing (and never "over by") while nobody is writing.
//
// Pure: no I/O, no clock. The conductor decides nothing about lanes beyond
// what the round records already say.

import { isLaneRound } from "./lanes.ts";
import { seatsOf } from "./seats.ts";
import type { PhaseState } from "./types.ts";

export type LaneRoundPhase = "LANE REVIEW" | "PICK";

/** The lane round's sub-phase label, or undefined when the phase is not in a
 * two-lane round with both candidates already submitted. */
export function laneRoundPhase(phase: PhaseState): LaneRoundPhase | undefined {
  if (phase.phase !== "IMPLEMENTING") return undefined;
  if (!isLaneRound(phase.contract)) return undefined;
  const rounds = phase.rounds ?? [];
  const round = rounds[rounds.length - 1];
  if (!round) return undefined;
  const submitted = round.candidates.filter((c) => typeof c.sha === "string" && c.sha.length > 0);
  // Still implementing: at least one lane has not submitted yet.
  if (submitted.length < round.lanes.length) return undefined;
  const passing = round.candidates.filter((c) => c.ok === true && typeof c.sha === "string");
  // No passing candidate: the round is about to repeat; no review or pick
  // turn runs, so there is no sub-phase to show.
  if (passing.length === 0) return undefined;
  const seats = seatsOf(phase.contract);
  const allReviewed = passing.every((c) => (c.reviews ?? []).length >= seats.length);
  return allReviewed ? "PICK" : "LANE REVIEW";
}
