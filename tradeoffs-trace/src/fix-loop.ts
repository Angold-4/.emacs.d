// 07g1: the per-domain loop. One domain with open points runs from a base,
// establishes its facts (its fix check, every point's doneWhen and the test
// removal rule), then a fresh stateless review judges every point. A red
// check or a review that does not fix every point (or that reports a quality
// problem) goes back to the SAME fixer session; a review that fixes them all
// with quality ok approves the domain. A domain that spends its review bound
// is dropped, never allowed to hold another domain. 07g1b (A2): a domain whose
// fixer has nothing left to change with its check green ends `open` — no
// review approved it, so its head cannot merge and its points stay open.
//
// This module owns only the sequence. Every effect (building the fixer,
// running the facts, reviewing, fixing, emitting events) is a host callback,
// so the loop is unit-testable without a conductor, a model or git.

import type { DomainFacts, DomainLoopRecord, DomainReview } from "./core/types.ts";
import type { FixTask } from "./core/fix-rounds.ts";
import { domainOf } from "./core/domains.ts";

export interface DomainBuild {
  sha?: string;
  note?: string;
  /** The head stays as it is: the fix changed nothing, or no fix could run
   * because the fixer's attempt time is spent (07g1b R14). */
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
  /** 07g1b (A2/R14): `open` is a loop that ended because its fixer had nothing
   * left to change while its fix check was green, with a point still open. It
   * is NOT an approval — no domain review ran, so its points stay open and
   * its head remains on the domain branch, never in the integrated candidate. */
  status: "approved" | "open" | "dropped" | "stopped";
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
    if (review.quality.ok || blockingQualityProblems(review, itemIds).length > 0) return review;
    // Those remarks have already been judged advisory by the domain gate.
    // Record its effective quality approval, retaining each remark as a note.
    return { ...review, quality: { ok: true }, notes: [...(review.notes ?? []), ...extra.map((p) => `quality note (no plan item or design line cited): ${p}`)] };
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
        const note = fixed.note ?? `the fix of ${sha.slice(0, 9)} changed nothing`;
        // An unchanged fixer with a green check and a red point ends open,
        // not approved. No review runs on a red fact, and the conductor may
        // merge only approved domains; these points wait for the next loop.
        if (fixed.unchanged === true && facts.checkPassed && (facts.testRemovals?.length ?? 0) === 0) {
          const reason = `${note}: the domain ends open, not approved, and its points stay open`;
          record({ iteration, sha, facts, note: reason });
          return finish({ status: "open", sha, reason, reviews, fixes, records });
        }
        record({ iteration, sha, facts, note });
        return finish({ status: "dropped", sha, reason: note, reviews, fixes, records });
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

// ---------------------------------------------------------------------------
// 07g2 (A1): integration of the approved domains onto the pick
// ---------------------------------------------------------------------------
// git does the merge (`merge --no-ff` of every approved domain commit); a
// conflict is the ONE place another writer runs — the integrate duty — and its
// resolution may change only the files git left unmerged (C4). The functions
// below are pure so the conductor's merge step, its fence and its brief are
// testable without git, a model or a run directory.

/** The two-character porcelain code of an unmerged entry: `UU`, `AA`, `DU`,
 * `UD`, `AU`, `UA` or `DD`. Any other code is a settled path. */
function isUnmergedCode(code: string): boolean {
  return code.length === 2 && (code[0] === "U" || code[1] === "U" || code === "AA" || code === "DD");
}

/** The paths `git status --porcelain` reports as unmerged, in git's order. Accepts
 * both the NUL-separated form (`-z`, no quoting of odd names) and the line
 * form. A rename entry's second name is never a conflict path. */
export function unmergedPaths(porcelain: string): string[] {
  const entries = porcelain.includes("\u0000") ? porcelain.split("\u0000") : porcelain.split("\n");
  const out: string[] = [];
  for (const entry of entries) {
    if (entry.length < 4) continue;
    const code = entry.slice(0, 2);
    if (!isUnmergedCode(code)) continue;
    const name = entry.slice(3).trim();
    if (name.length > 0) out.push(name);
  }
  return out;
}

/** 07g2 (C4): the paths the integrate duty changed that git did NOT leave
 * conflicted. A resolution may rewrite a conflicted file however it needs;
 * any other path is outside its fence and the conductor refuses it. */
export function resolutionOutsideConflicted(changed: readonly string[], conflicted: readonly string[]): string[] {
  const allowed = new Set(conflicted.map((p) => p.replace(/^\.\//, "")));
  return changed.map((p) => p.replace(/^\.\//, "")).filter((p) => !allowed.has(p));
}

/** The repository path `git merge`'s output or a red check names, if any: a
 * token with at least one `/` (or a bare `name.ext`), stripped of a trailing
 * `:line`, `,` or `)`. Quoted and word-boundary forms both count. */
function namedPaths(text: string): string[] {
  const out: string[] = [];
  const re = /(?:\.\/)?[A-Za-z0-9_@+][A-Za-z0-9_.@+-]*(?:\/[A-Za-z0-9_.@+-]+)+(?::\d+)?|[A-Za-z0-9_@+-]+\.(?:ts|tsx|js|mjs|cjs|el|json|org|md|css|html|py|go|rs)(?::\d+)?/g;
  for (const match of text.matchAll(re)) {
    const path = match[0].replace(/:\d+$/, "");
    if (!out.includes(path)) out.push(path);
  }
  return out;
}

/** 07g2 (A1/R3): the domain that owns the first file a failing check names, in
 * the output's own order. `undefined` when the output names no declared
 * domain's file — the conductor then records LEAD_CALLED instead of guessing. */
export function failingDomainOf(
  output: string,
  domains: readonly { name: string; globs: string[] }[],
): { domain: string; path: string } | undefined {
  if (!domains || domains.length === 0) return undefined;
  for (const path of namedPaths(output)) {
    const domain = domainOf(path, domains);
    if (domain) return { domain, path };
  }
  return undefined;
}

/** 07g2 (A1): the integrate duty's brief. It is fenced by FILES: the resolution
 * may change only the conflicted files, and hunks inside them are never judged
 * (C4). The merge's own output is quoted so the duty sees both sides. */
export function integrateBrief(opts: {
  round: number;
  base: string;
  commits: readonly { domain: string; sha: string }[];
  conflicted: readonly string[];
  mergeOutput: string;
  attempt: number;
  maxAttempts: number;
}): string {
  return [
    `The approved domain commits could not be merged onto ${opts.base.slice(0, 7)} (attempt ${opts.attempt} of ${opts.maxAttempts}).`,
    "",
    "Resolve the conflict in this worktree, which is mid-merge. This is the integrate duty: you may change ONLY these conflicted files:",
    ...opts.conflicted.map((p) => `- ${p}`),
    "",
    "Keep both domains' intent; do not drop one side's work, do not edit any other file, do not delete, skip or #[ignore] a test, and do not commit or run git merge/commit/abort — the conductor stages the conflicted files and commits the merge itself. Leave no conflict markers in the tree.",
    "",
    `Domains being integrated: ${opts.commits.map((c) => `${c.domain} ${c.sha.slice(0, 7)}`).join(", ")}.`,
    "",
    "git's merge output:",
    opts.mergeOutput.trim() || "(git printed nothing)",
  ].join("\n");
}
