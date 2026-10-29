import type { RoundRecord } from "./types.ts";

/** 07g1b (A4/R12): the lane candidate a ruling id names, by its label
 * (`C<round>-<lane>`) or its sha (exact, or a prefix of 7+ characters). */
export function namedCandidate(
  phase: { rounds?: RoundRecord[] },
  id: string,
): { round: number; lane: string; sha: string } | undefined {
  const wanted = id.trim().toLowerCase();
  if (wanted.length === 0) return undefined;
  for (const round of phase.rounds ?? []) {
    for (const candidate of round.candidates) {
      if (typeof candidate.sha !== "string" || candidate.sha.length === 0) continue;
      const label = `c${round.round}-${candidate.lane}`;
      if (label === wanted) return { round: round.round, lane: candidate.lane, sha: candidate.sha };
      if (candidate.sha === id || (wanted.length >= 7 && candidate.sha.toLowerCase().startsWith(wanted))) {
        return { round: round.round, lane: candidate.lane, sha: candidate.sha };
      }
    }
  }
  return undefined;
}
