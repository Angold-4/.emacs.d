// Plan 2c: decision records across review rounds.
//
// v1 invalidates all evidence when a new candidate is frozen (design §7.2):
// ballots, reviews, checks. Decision *records* used to survive unchanged, so
// a record describing the previous candidate stayed votable. Reviewers then
// correctly rejected it as "no longer true of the new code", and acceptance
// failed on records the worker could not change (dogfood run 4ec5e0f8,
// round 2). This module decides, at FREEZE_COMPLETED, which records carry
// forward to the new candidate.
//
// - A worker record the repairing worker marked `kept` is rebound to the new
//   candidate; `changed` is rebound with the new plain-language text. The
//   version bumps either way, so no ballot on the old record can count.
// - A worker record marked `withdrawn`, or not mentioned at all, is
//   superseded.
// - Reviewer-discovered and trigger records belong to the review of the old
//   candidate. They are superseded: reviewers rediscover (and match) against
//   the new candidate, and triggers are recomputed from the new diff.
//
// Superseded records are kept for history and never votable (predicate.ts's
// `isLiveDecision`).

import { isLiveDecision } from "./predicate.ts";
import { currentBallot, tally } from "./tally.ts";
import type { Ballot, ContractVersion, Decision, Finding, PriorDecisionStatement } from "./types.ts";

export function carryDecisionsForward(
  decisions: Decision[],
  prior: PriorDecisionStatement[] | undefined,
  newCandidateSha: string,
): Decision[] {
  const statements = new Map((prior ?? []).map((p) => [p.id, p]));
  const short = newCandidateSha.slice(0, 7);
  return decisions.map((d) => {
    if (!isLiveDecision(d) || d.boundCandidateSha === newCandidateSha) return d;
    const statement = d.source === "worker" ? statements.get(d.id) : undefined;
    if (statement?.status === "kept") {
      return { ...d, boundCandidateSha: newCandidateSha, version: d.version + 1 };
    }
    if (statement?.status === "changed") {
      return {
        ...d,
        choice: statement.choice ?? d.choice,
        whyItMatters: statement.whyItMatters ?? d.whyItMatters,
        alternatives: statement.alternatives ?? d.alternatives,
        recommendation: statement.recommendation ?? d.recommendation,
        boundCandidateSha: newCandidateSha,
        version: d.version + 1,
      };
    }
    const why =
      statement?.status === "withdrawn"
        ? `withdrawn by the worker at candidate ${short}`
        : d.source === "worker"
          ? `candidate ${short}: not carried forward by the worker`
          : `candidate ${short}: ${d.source} record from an earlier round`;
    return { ...d, supersededBy: why, version: d.version + 1 };
  });
}

/** Skill fix 5: ballots a new round inherits. When the worker keeps a
 * decision unchanged (`kept`) and it PASSED its vote on the previous
 * candidate, each reviewer's ballot carries over, rebound to the new
 * candidate and record version and marked `carriedFrom`. Reviewers see the
 * record as carried and vote again only if the new changes affect it; a
 * fresh ballot is appended after the carried one, so it wins (currentBallot
 * takes the latest). Run 0a35ae40 (12b) re-cast every ballot in each of its
 * three rounds, 47 in the last one alone.
 *
 * `prevDecisions`/`prevBallots`/`findings` are the phase before the freeze;
 * `carried` is carryDecisionsForward's result. */
export function carryBallotsForward(
  prevDecisions: Decision[],
  prevBallots: Ballot[],
  findings: Finding[],
  prior: PriorDecisionStatement[] | undefined,
  prevCandidateSha: string | undefined,
  contractVersion: ContractVersion,
  carried: Decision[],
  newCandidateSha: string,
): Ballot[] {
  if (!prevCandidateSha) return [];
  const kept = new Set((prior ?? []).filter((p) => p.status === "kept").map((p) => p.id));
  const out: Ballot[] = [];
  for (const d of carried) {
    if (!kept.has(d.id) || d.boundCandidateSha !== newCandidateSha) continue;
    const before = prevDecisions.find((x) => x.id === d.id);
    if (!before || before.boundCandidateSha !== prevCandidateSha) continue;
    if (tally(before, prevBallots, findings, prevCandidateSha, contractVersion) !== "pass") continue;
    for (const reviewer of ["M", "A", "B"] as const) {
      const b = currentBallot(prevBallots, d.id, reviewer, prevCandidateSha, contractVersion, before.version);
      if (!b) continue;
      out.push({ ...b, boundCandidateSha: newCandidateSha, boundRecordVersion: d.version, carriedFrom: prevCandidateSha });
    }
  }
  return out;
}
