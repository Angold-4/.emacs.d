// Plan 06h (A1/A2): the seat list, its leader and the lane count are the
// plan's to declare, never the code's to assume. `seatsOf` is the ONLY source
// of the seat list; every place that used to hard-code `M`, `A` and `B`
// reads it. The default stays `M`, `A` and `B`, led by `M`, with one lane,
// so a plan without the new keywords behaves exactly as 06g2 did.

import type { PhaseContract, Seats } from "./types.ts";

/** The seats of a plan that declares none: the fixed list 06g2 used. */
export const DEFAULT_SEATS: readonly string[] = ["M", "A", "B"];

/** The default leader: the first seat. */
export const DEFAULT_LEADER = "M";

/** The smallest shape `seatsOf`/`leaderOf` read. A `PhaseContract`, a
 * `PlanPhase`, a `RunPlanPhase` and a `RunPlanFile` are all assignable. */
export interface SeatSource {
  seats?: string[];
  leader?: string;
  workers?: number;
}

/** The reviewer seats of a contract (or plan). An absent, empty or malformed
 * list falls back to `M A B`, so an old contract and an old log replay
 * unchanged. The list is returned as declared; `tt lint` is what refuses an
 * even or duplicate list. */
export function seatsOf(source: SeatSource | PhaseContract | undefined): string[] {
  const declared = source?.seats;
  if (Array.isArray(declared)) {
    const clean = declared.filter((s): s is string => typeof s === "string" && s.trim().length > 0);
    if (clean.length > 0) return clean;
  }
  return [...DEFAULT_SEATS];
}

/** The leader seat: `#+TT_LEADER` when it names a seat, the first seat
 * otherwise. Never a model's choice. */
export function leaderOf(source: SeatSource | PhaseContract | undefined): string {
  const seats = seatsOf(source);
  const declared = source?.leader;
  if (typeof declared === "string" && seats.includes(declared)) return declared;
  return seats[0] ?? DEFAULT_LEADER;
}

/** The number of lanes one round runs. `#+TT_WORKERS` when the plan declares
 * it, 1 otherwise. A non-positive or malformed value is 1 here; `tt lint`
 * refuses it before a run starts. */
export function workerCountOf(source: SeatSource | undefined): number {
  const n = source?.workers;
  return typeof n === "number" && Number.isFinite(n) && n >= 1 ? Math.floor(n) : 1;
}

/** A plan's seat configuration as `Seats`, for the init event. */
export function seatsRecordOf(source: SeatSource): Seats {
  return { seats: seatsOf(source), leader: leaderOf(source), workers: workerCountOf(source) };
}
