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
import type { Decision, PriorDecisionStatement } from "./types.ts";

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
