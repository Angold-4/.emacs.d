// 07g1: the per-domain loop. One domain with open points runs from a base,
// establishes its facts (its fix check, every point's doneWhen and the test
// removal rule), then a fresh stateless review judges every point. A red
// check or a review that does not fix every point (or that reports a quality
// problem) goes back to the SAME fixer session; a review that fixes them all
// with quality ok approves the domain. A domain that spends its review bound
// is dropped, never allowed to hold another domain.
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
  /** How many domain REVIEWS this domain may spend in total. A resumed loop
   * counts the reviews its log already recorded against this bound. */
  readonly maxReviews: number;
  /** The pass number this loop starts at. A first run starts at 1; a resumed
   * run continues after the last pass the conductor's log recorded, so the
   * log-built brief carries every earlier pass instead of overwriting it. */
  readonly startIteration?: number;
  /** 07g1b (A2): the plan's item ids, so a quality problem may be checked
   * against the item it names. Absent: only a design-line citation blocks. */
  readonly itemIds?: readonly string[];
  /** The reviews this domain already spent before this process started. A
   * restart continues the same budget instead of handing the domain a fresh
   * one. */
  readonly startReviews?: number;
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

/** 07g1b (A2): whether one quality problem names something the seats and the
 * design can be held to — a plan item id (as its own token: `R2`, `C1`, `A4`,
 * never part of a longer word) or a design line (`§3.2`, `07_full.design.md:56`).
 * A remark that cites neither is a follow-up note, not work for the same
 * fixer: the fixer's task is the points, and an uncited quality wish would let
 * a review re-open settled scope (exex 01's r1 incident, 07e r4). */
export function qualityProblemCites(problem: string, itemIds: readonly string[] = []): boolean {
  const text = problem.trim();
  if (text.length === 0) return false;
  for (const id of itemIds) {
    const escaped = id.trim().replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    if (escaped.length === 0) continue;
    if (new RegExp(`(^|[^A-Za-z0-9_])${escaped}(?![A-Za-z0-9_])`).test(text)) return true;
  }
  return /§\s*\d/.test(text) || /[\w./-]+\.md:\d+/.test(text);
}

/** The quality problems of a review that cite a plan item or a design line —
 * the only ones that send the work back (A2). */
export function blockingQualityProblems(review: DomainReview, itemIds: readonly string[] = []): string[] {
  if (review.quality.ok) return [];
  return (review.quality.problems ?? []).filter((p) => qualityProblemCites(p, itemIds));
}

/** Whether a review approved every point of the task. 07g1 (A2): a domain
 * review blocks on its named points and on a quality problem — a point it
 * judged not fixed (or did not judge), or a quality problem that cites a plan
 * item or a design line, sends the work back to the same fixer. A quality
 * remark that cites neither is a note on the approval (07g1b A2). A failing
 * machine check is caught by the facts before any review starts. */
export function domainReviewApproves(task: FixTask, review: DomainReview, itemIds: readonly string[] = []): boolean {
  if (blockingQualityProblems(review, itemIds).length > 0) return false;
  for (const id of task.findingIds) {
    const verdict = review.points.find((p) => p.pointId === id);
    if (!verdict || !verdict.fixed) return false;
  }
  return true;
}

/** The failure text sent back to the same fixer session after a review. */
export function domainReviewFailure(task: FixTask, review: DomainReview, itemIds: readonly string[] = []): string {
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
  const quality = blockingQualityProblems(review, itemIds);
  if (quality.length > 0) lines.push(`Quality: ${quality.join("; ")}`);
  if (review.notes && review.notes.length > 0) lines.push(`Notes: ${review.notes.join("; ")}`);
  return lines.join("\n") || "The domain review did not approve this commit.";
}

/** Run one domain's loop. See the module comment. */
export async function runDomainLoop(task: FixTask, host: DomainLoopHost): Promise<DomainLoopOutcome> {
  const records: DomainLoopRecord[] = [];
  const maxReviews = Number.isFinite(host.maxReviews) && host.maxReviews > 0 ? Math.floor(host.maxReviews) : 1;
  let fixes = 0;
  let reviews = Number.isFinite(host.startReviews) && (host.startReviews as number) > 0 ? Math.floor(host.startReviews as number) : 0;
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
  // 07g1b (A2): a quality remark that cites neither a plan item nor a design
  // line never sends work back; it is kept as a note of the approval instead.
  const itemIds = host.itemIds ?? [];
  const qualityNote = (review: DomainReview): string[] =>
    (review.quality.ok ? [] : (review.quality.problems ?? []).filter((p) => !qualityProblemCites(p, itemIds)));
  const notedReview = (review: DomainReview): DomainReview => {
    const extra = qualityNote(review);
    if (extra.length === 0) return review;
    return { ...review, notes: [...(review.notes ?? []), ...extra.map((p) => `quality note (no plan item or design line cited): ${p}`)] };
  };

  let build: DomainBuild;
  const firstIteration = Number.isFinite(host.startIteration) && (host.startIteration as number) > 0 ? Math.floor(host.startIteration as number) : 1;
  if (reviews >= maxReviews) {
    const reason = `the domain loop bound was reached after ${reviews} reviews`;
    record({ iteration: firstIteration, note: reason });
    return finish({ status: "dropped", reason, reviews, fixes, records });
  }
  try {
    build = await host.build();
  } catch (err) {
    const reason = `the fixer failed: ${String((err as Error)?.message ?? err)}`;
    record({ iteration: firstIteration, note: reason });
    return finish({ status: "dropped", reason, reviews, fixes, records });
  }
  if (typeof build.sha !== "string" || build.sha.length === 0) {
    const reason = build.note ?? "the fixer produced no commit";
    record({ iteration: firstIteration, note: reason });
    return finish({ status: "dropped", reason, reviews, fixes, records });
  }
  let sha = build.sha;
  let iteration = firstIteration;

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
        // 07g1b (R14): a fixer that reports it changed nothing has nothing to
        // repair here, and its check is green. The domain is approved with its
        // head as the commit rather than dropped: dropping it threw away the
        // first commit's real work and every other domain still merged without
        // it (the 07g1a bounds regressions). Its points stay open — the facts
        // say so — and the next round's map (or acceptance) judges them.
        if (fixed.unchanged === true && facts.checkPassed && (facts.testRemovals?.length ?? 0) === 0) {
          record({
            iteration,
            sha,
            facts,
            note: "the fixer changed nothing and the fix check is green: the domain is approved with its point open",
          });
          return finish({ status: "approved", sha, reviews, fixes, records });
        }
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
    const approved = domainReviewApproves(task, review, itemIds);
    record({
      iteration,
      sha,
      facts,
      review: notedReview(review),
      note: approved ? "domain review approved" : "domain review sent work back",
    });
    if (reviewApproved(task, review, itemIds)) return finish({ status: "approved", sha, reviews, fixes, records });
    if (reviews >= maxReviews) {
      const reason = `the domain loop bound was reached after ${reviews} reviews: ${domainReviewFailure(task, review, itemIds).split("\n")[0]}`;
      return finish({ status: "dropped", sha, reason, reviews, fixes, records });
    }
    if (stopped()) return finish({ status: "stopped", sha, reason: "the owner stopped the run", reviews, fixes, records });
    let fixed: DomainBuild;
    try {
      fixed = await host.fix(sha, domainReviewFailure(task, review, itemIds), iteration);
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
function reviewApproved(task: FixTask, review: DomainReview, itemIds: readonly string[] = []): boolean {
  return domainReviewApproves(task, review, itemIds);
}
