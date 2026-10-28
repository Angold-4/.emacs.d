// 07g1: the per-domain loop. One domain with open points runs from a base,
// establishes its facts (its fix check, every point's doneWhen and the test
// removal rule), then a fresh stateless review judges every point. A red
// check or a review that does not fix every point goes back to the SAME
// fixer session; a review that fixes them all approves the domain. A domain
// that spends its review bound is dropped, never allowed to hold another
// domain.
//
// This module owns only the sequence. Every effect (building the fixer,
// running the facts, reviewing, fixing, emitting events) is a host callback,
// so the loop is unit-testable without a conductor, a model or git.

import type { DomainFacts, DomainLoopRecord, DomainReview } from "./core/types.ts";
import type { FixTask } from "./core/fix-rounds.ts";

export interface DomainBuild {
  sha?: string;
  note?: string;
  unchanged?: boolean;
}

export interface DomainLoopHost {
  /** How many domain REVIEWS this domain may spend. */
  readonly maxReviews: number;
  /** Start the domain from its base. */
  build(): Promise<DomainBuild>;
  /** Establish the check, doneWhen and test-removal facts on `sha`. */
  facts(sha: string, iteration: number): Promise<DomainFacts>;
  /** A fresh, stateless review of `sha`; undefined means it could not be taken. */
  review(sha: string, iteration: number, facts: DomainFacts): Promise<DomainReview | undefined>;
  /** Send a failure back to the SAME fixer session. */
  fix(sha: string, failure: string, iteration: number): Promise<DomainBuild>;
  stopRequested?(): boolean;
  /** Record one pass for the review brief and the log. */
  record?(record: DomainLoopRecord): void;
  approved?(sha: string, reviews: number): void;
  dropped?(reason: string, reviews: number): void;
}

export interface DomainLoopOutcome {
  status: "approved" | "dropped" | "stopped";
  sha?: string;
  reason?: string;
  reviews: number;
  fixes: number;
  records: DomainLoopRecord[];
}

/** The facts that make a pass red before any review may start. */
export function domainFactFailure(facts: DomainFacts): string | undefined {
  if (!facts.checkPassed) return facts.checkNote ?? "the fix check failed";
  if (facts.testRemovals && facts.testRemovals.length > 0) {
    return `tests were removed without a #+TT_TEST_REMOVALS allowance: ${facts.testRemovals.join(", ")}`;
  }
  const failing = facts.points.filter((p) => !p.onHead);
  if (failing.length > 0) {
    return failing
      .map((p) => `${p.id}${p.doneWhen ? ` (${p.doneWhen})` : ""}${p.outputTail ? `: ${p.outputTail.split("\n").slice(-6).join("\n")}` : " did not pass on the head"}`)
      .join("\n");
  }
  return undefined;
}

/** Whether a quality remark is a cited plan/design violation: only such a
 * remark may send a domain back. A naming preference or another uncited style
 * remark is a note on the approval, never a blocker. */
export function qualityProblemBlocks(problem: string): boolean {
  const text = problem.trim();
  if (text.length === 0) return false;
  const lower = text.toLowerCase();
  const citesPlanOrDesign = /\b(plan|design|conceptual integrity|requirement|constraint|architecture|contract)\b/.test(lower);
  if (citesPlanOrDesign) return true;
  // A bare item id is a citation only when the remark also says it is violated.
  const itemId = /\b(?:[ARCF]-?\d+)\b/i.test(text);
  const violation = /\b(violat(?:e|es|ed|ion)|contradict(?:s|ed|ion)?|breach(?:es|ed)?|deviate(?:s|d)?|unmet|does not meet|fails? to meet|breaks?)\b/.test(lower);
  return itemId && violation;
}

/** The quality remarks that may send work back: cited plan/design violations.
 * Uncited remarks remain notes on DOMAIN_APPROVED. */
export function blockingQualityProblems(review: DomainReview): string[] {
  if (review.quality.ok) return [];
  return (review.quality.problems ?? []).filter(qualityProblemBlocks);
}

/** Whether a review approved every point of the task. A missing point is not
 * fixed; quality blocks only for a cited plan/design violation. */
export function domainReviewApproves(task: FixTask, review: DomainReview): boolean {
  if (blockingQualityProblems(review).length > 0) return false;
  for (const id of task.findingIds) {
    const verdict = review.points.find((p) => p.pointId === id);
    if (!verdict || !verdict.fixed) return false;
  }
  return true;
}

/** The failure text sent back to the same fixer session after a review. */
export function domainReviewFailure(task: FixTask, review: DomainReview): string {
  const lines: string[] = [];
  const bad = review.points.filter((p) => !p.fixed);
  if (bad.length > 0) {
    lines.push(
      "The domain review did not accept these points:",
      ...bad.map((p) => `- ${p.pointId}: ${p.why}${p.anchors && p.anchors.length > 0 ? ` (${p.anchors.join(", ")})` : ""}`),
    );
  }
  const missing = task.findingIds.filter((id) => !review.points.some((p) => p.pointId === id));
  if (missing.length > 0) lines.push(`The review did not judge: ${missing.join(", ")}.`);
  const blocking = blockingQualityProblems(review);
  if (blocking.length > 0) lines.push(`Quality (a cited plan/design violation): ${blocking.join("; ")}`);
  if (review.notes && review.notes.length > 0) lines.push(`Notes: ${review.notes.join("; ")}`);
  return lines.join("\n") || "The domain review did not approve this commit.";
}

/** Run one domain's loop. See the module comment. */
export async function runDomainLoop(task: FixTask, host: DomainLoopHost): Promise<DomainLoopOutcome> {
  const records: DomainLoopRecord[] = [];
  const maxReviews = Number.isFinite(host.maxReviews) && host.maxReviews > 0 ? Math.floor(host.maxReviews) : 1;
  let fixes = 0;
  let reviews = 0;
  const stopped = (): boolean => host.stopRequested?.() === true;
  const finish = (outcome: DomainLoopOutcome): DomainLoopOutcome => {
    if (outcome.status === "approved" && outcome.sha) host.approved?.(outcome.sha, outcome.reviews);
    if (outcome.status === "dropped") host.dropped?.(outcome.reason ?? "the domain loop ended", outcome.reviews);
    return outcome;
  };
  const record = (r: DomainLoopRecord): void => {
    records.push(r);
    host.record?.(r);
  };

  let build: DomainBuild;
  try {
    build = await host.build();
  } catch (err) {
    const reason = `the fixer failed: ${String((err as Error)?.message ?? err)}`;
    record({ iteration: 1, note: reason });
    return finish({ status: "dropped", reason, reviews, fixes, records });
  }
  if (typeof build.sha !== "string" || build.sha.length === 0) {
    const reason = build.note ?? "the fixer produced no commit";
    record({ iteration: 1, note: reason });
    return finish({ status: "dropped", reason, reviews, fixes, records });
  }
  let sha = build.sha;
  let iteration = 1;

  for (;;) {
    if (stopped()) return finish({ status: "stopped", sha, reason: "the owner stopped the run", reviews, fixes, records });
    let facts: DomainFacts;
    try {
      facts = await host.facts(sha, iteration);
    } catch (err) {
      facts = { checkPassed: false, checkNote: `the facts could not be established: ${String((err as Error)?.message ?? err)}`, points: [] };
    }
    const failure = domainFactFailure(facts);
    if (failure !== undefined) {
      record({ iteration, sha, facts, note: `red: ${failure.split("\n")[0]}` });
      if (stopped()) return finish({ status: "stopped", sha, reason: "the owner stopped the run", reviews, fixes, records });
      let fixed: DomainBuild;
      try {
        fixed = await host.fix(sha, failure, iteration);
      } catch (err) {
        fixed = { note: `the fix failed: ${String((err as Error)?.message ?? err)}` };
      }
      fixes += 1;
      if (typeof fixed.sha !== "string" || fixed.sha.length === 0 || fixed.sha === sha) {
        const reason = fixed.note ?? `the fix of ${sha.slice(0, 9)} changed nothing`;
        record({ iteration, sha, facts, note: reason });
        return finish({ status: "dropped", sha, reason, reviews, fixes, records });
      }
      sha = fixed.sha;
      iteration += 1;
      continue;
    }

    // Facts are clean; only now may a fresh domain review start. It is
    // stateless: a new agent and a brief built from `records` alone.
    let review: DomainReview | undefined;
    try {
      review = await host.review(sha, iteration, facts);
    } catch (err) {
      review = undefined;
      record({ iteration, sha, facts, note: `the domain review failed: ${String((err as Error)?.message ?? err)}` });
    }
    if (!review) {
      const reason = records[records.length - 1]?.note ?? "the domain review could not be taken";
      if (!records.some((r) => r.iteration === iteration && r.note === reason)) record({ iteration, sha, facts, note: reason });
      return finish({ status: "dropped", sha, reason, reviews, fixes, records });
    }
    reviews += 1;
    const approved = domainReviewApproves(task, review);
    record({ iteration, sha, facts, review, note: approved ? "domain review approved" : "domain review sent work back" });
    if (reviewApproved(task, review)) return finish({ status: "approved", sha, reviews, fixes, records });
    if (reviews >= maxReviews) {
      const reason = `the domain loop bound was reached after ${reviews} reviews: ${domainReviewFailure(task, review).split("\n")[0]}`;
      return finish({ status: "dropped", sha, reason, reviews, fixes, records });
    }
    if (stopped()) return finish({ status: "stopped", sha, reason: "the owner stopped the run", reviews, fixes, records });
    let fixed: DomainBuild;
    try {
      fixed = await host.fix(sha, domainReviewFailure(task, review), iteration);
    } catch (err) {
      fixed = { note: `the fix failed: ${String((err as Error)?.message ?? err)}` };
    }
    fixes += 1;
    if (typeof fixed.sha !== "string" || fixed.sha.length === 0 || fixed.sha === sha) {
      const reason = fixed.note ?? `the fix of ${sha.slice(0, 9)} changed nothing`;
      record({ iteration, sha, facts, review, note: reason });
      return finish({ status: "dropped", sha, reason, reviews, fixes, records });
    }
    sha = fixed.sha;
    iteration += 1;
  }
}

/** Small wrapper so the approval rule is one function, not two copies. */
function reviewApproved(task: FixTask, review: DomainReview): boolean {
  return domainReviewApproves(task, review);
}
