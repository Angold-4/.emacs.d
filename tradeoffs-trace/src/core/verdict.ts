// Plan 3b: what the owner reads about a review round, computed from state
// alone (pure; no I/O). Front ends render these strings and never infer an
// outcome from individual ballots themselves (observed in run 4ec5e0f8: "M ✓
// A ✓ B ✓" read as all approved while M had vetoed and a blocking finding was
// open).

import { decisionStatus, isLiveDecision } from "./predicate.ts";
import type { PhaseState, Reviewer } from "./types.ts";

export const REVIEWERS: readonly Reviewer[] = ["M", "A", "B"];

export interface ReviewerOutcome {
  reviewer: Reviewer;
  /** "pending": no review of the current candidate yet; "objects": a reject
   * ballot or a blocking finding; "approves": submitted with neither. */
  state: "pending" | "approves" | "objects";
  approvals: number;
  rejections: number;
  blocking: number;
  advisory: number;
  /** e.g. "M ✗ 2 reject · 1 blocking", "A ✓", "B ✓ 3 advisory", "M ⧗". */
  label: string;
}

/** Each reviewer's outcome on the current candidate. */
export function reviewerOutcomes(phase: PhaseState): ReviewerOutcome[] {
  const C = phase.candidate?.sha;
  return REVIEWERS.map((reviewer) => {
    const review = phase.reviews[reviewer]?.review;
    const submitted = Boolean(C && review && review.candidateSha === C);
    const ballots = phase.ballots.filter((b) => b.reviewer === reviewer && b.boundCandidateSha === C);
    const approvals = ballots.filter((b) => b.vote === "approve").length;
    const rejections = ballots.filter((b) => b.vote === "reject").length;
    const raised = phase.findings.filter(
      (f) => f.boundCandidateSha === C && (f.raisedBy === reviewer || (f.alsoRaisedBy ?? []).includes(reviewer)),
    );
    const blocking = raised.filter((f) => f.severity === "blocking").length;
    const advisory = raised.filter((f) => f.severity === "advisory").length;
    const state = !submitted ? "pending" : rejections > 0 || blocking > 0 ? "objects" : "approves";
    const parts = [
      rejections > 0 ? `${rejections} reject` : "",
      blocking > 0 ? `${blocking} blocking` : "",
      advisory > 0 ? `${advisory} advisory` : "",
    ].filter(Boolean);
    const mark = state === "pending" ? "⧗" : state === "objects" ? "✗" : "✓";
    return {
      reviewer,
      state,
      approvals,
      rejections,
      blocking,
      advisory,
      label: `${reviewer} ${mark}${parts.length > 0 ? ` ${parts.join(" · ")}` : ""}`,
    };
  });
}

/** Why the current candidate is not (or not yet) accepted, one short clause
 * each; empty when nothing stands in the way. Only failures that already
 * happened are listed; a gate still running is not a reason. */
export function notAcceptedReasons(phase: PhaseState): string[] {
  const C = phase.candidate?.sha;
  if (!C) return [];
  const reasons: string[] = [];
  if (phase.checks?.candidateSha === C && phase.checks.passed === false) reasons.push("checks failed");
  if (phase.probe?.candidateSha === C && phase.probe.passed === false) reasons.push("integration probe failed");
  // Failed decisions, grouped by why: "D-3 vetoed by M", or "16 decisions
  // failed (missing ballot from M)" rather than sixteen clauses.
  const failedBy = new Map<string, string[]>();
  for (const d of phase.decisions) {
    if (!isLiveDecision(d) || d.boundCandidateSha !== C) continue;
    // Plan 01g: a failed amendment is not a reason the candidate was not
    // accepted — it leaves the criterion unchanged and blocks nothing.
    if (d.amendment) continue;
    const s = decisionStatus(d, phase);
    if (s.status !== "failed") continue;
    const why = s.reason ?? "failed";
    failedBy.set(why, [...(failedBy.get(why) ?? []), shortId(d.id)]);
  }
  for (const [why, ids] of failedBy) {
    const verb = why === "M veto" ? "vetoed by M" : `failed (${why})`;
    reasons.push(ids.length <= 2 ? `${ids.join(", ")} ${verb}` : `${ids.length} decisions ${verb}`);
  }
  for (const f of phase.findings) {
    if (f.status === "open" && f.severity === "blocking") reasons.push(`blocking finding ${shortId(f.id)} open`);
  }
  return reasons;
}

/** A record id without its phase and candidate prefix: "D-p2d-x-dbe24445-4"
 * → "D-4", "F-p2d-x-M-3" → "F-M-3". */
export function shortId(id: string): string {
  const m = id.match(/^([A-Z]+)-.*?-((?:[MAB]-)?(?:[a-z]+-)?\d+)$/);
  if (!m) return id;
  const tail = m[2].replace(/^(?:disc|trigger)-/, "");
  return `${m[1]}-${tail}`;
}
