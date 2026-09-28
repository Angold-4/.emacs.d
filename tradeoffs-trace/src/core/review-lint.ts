// Plan 05j: the review lint. The views are pure projections of the event log
// (the same events render byte-identical bytes), and this lint runs on every
// render so a broken projection can never reach the owner looking clean.
//
// Six rules (each fails on a crafted violation in test/contract/review-lint):
//
//   one-anchor         one live entry per anchor — two live entries never
//                      share a file line range, a decision id or a plan clause
//   live-only          only live entries are rendered — a merged, dropped or
//                      resolved message is never shown as its own entry
//   evidence           every entry has a type, an anchor and its validation
//                      evidence (`verified:` or a vote/settlement)
//   title              titles non-empty, at most 80 characters, not cut mid-word
//   newest-candidate   every state is computed against the newest candidate
//   accounting         the accounting reconciles to 0 unaccounted
//
// A violation is never silently repaired. `runReviewLint` only READS the
// entries; the caller renders the first violation as the view's first line in
// the error face and records a REVIEW_LINT_FAILED event.

import {
  accountingLine,
  anchorFromEvidence,
  anchorsOverlap,
  entryTypeOf,
  formatAnchor,
  type Accounting,
  type Entry,
  type EntryView,
  type ProjectedEntries,
} from "./entries.ts";
import type { Message } from "./types.ts";

export type ReviewLintRule = "one-anchor" | "live-only" | "evidence" | "title" | "newest-candidate" | "accounting";

export interface ReviewLintViolation {
  rule: ReviewLintRule;
  detail: string;
}

export interface ReviewLintInput {
  projected: ProjectedEntries;
  /** The newest candidate the view must reflect. */
  newestCandidateSha?: string;
  /** The messages, for the evidence rule. */
  messages?: readonly Message[];
}

export interface ReviewLintResult {
  ok: boolean;
  violations: ReviewLintViolation[];
  /** The first violation's one-line message, the view's first line. */
  firstLine?: string;
}

const MAX_TITLE = 80;

function cutMidWord(title: string): boolean {
  const trimmed = title.trim();
  if (trimmed.length === 0) return true;
  // A title ending in a hyphen or in the middle of a word (no terminal
  // punctuation and a final word that looks truncated) is suspicious. The one
  // deterministic signal the plan names is a title the evaluator cut at the
  // schema cap: exactly 80 characters with no terminal punctuation.
  if (trimmed.length === MAX_TITLE && /[a-z]$/i.test(trimmed)) return true;
  if (/-$/.test(trimmed)) return true;
  return false;
}

/** Runs every rule and returns all violations, never repairing anything. */
export function runReviewLint(input: ReviewLintInput): ReviewLintResult {
  const { projected } = input;
  const violations: ReviewLintViolation[] = [];
  const live = projected.views.filter((v) => v.live);

  // one-anchor: no two live entries share an anchor.
  for (let i = 0; i < live.length; i++) {
    for (let j = i + 1; j < live.length; j++) {
      const a = live[i];
      const b = live[j];
      if (anchorsOverlap(a.anchor, b.anchor)) {
        violations.push({
          rule: "one-anchor",
          detail: `2 entries share ${formatAnchor(a.anchor)} (${a.entry.id} and ${b.entry.id})`,
        });
      }
    }
  }

  // live-only: every rendered entry must have at least one live message; a
  // non-live message never forces an entry of its own.
  for (const view of live) {
    const own = view.messages.filter((m) => !isNonLive(m));
    if (own.length === 0) {
      violations.push({ rule: "live-only", detail: `entry ${view.entry.id} renders only non-live messages` });
    }
    for (const m of view.messages) {
      if (isNonLive(m) && view.messages.length === 1) {
        violations.push({
          rule: "live-only",
          detail: `entry ${view.entry.id} renders ${m.state} message ${m.id} as an entry`,
        });
      }
    }
  }

  // evidence: every live entry has a type, an anchor and validation evidence.
  for (const view of live) {
    if (!view.type || !["blocker", "finding", "tradeoff"].includes(view.type)) {
      violations.push({ rule: "evidence", detail: `entry ${view.entry.id} has no type` });
    }
    if (!view.anchor) {
      violations.push({ rule: "evidence", detail: `entry ${view.entry.id} has no anchor` });
    }
    for (const m of view.messages) {
      const evidence = (m.evidence ?? []).map((e) => e.trim()).filter((e) => e.length > 0);
      const hasVote = m.settlement !== undefined;
      // Plan 05j: evidence is a citation (file:line), a `verified:` marker or
      // a vote — not merely any non-empty string (finding M-4). A trade-off's
      // evidence is its choice/alternative by construction, so it passes on
      // its own text; a finding without a citation fails until it is voted.
      const validated = evidence.some((e) => /verified:/i.test(e) || anchorFromEvidence(e) !== undefined);
      if (evidence.length === 0 && !hasVote) {
        violations.push({
          rule: "evidence",
          detail: `entry ${view.entry.id}: message ${m.id} has neither evidence nor a vote`,
        });
      } else if (!validated && !hasVote && m.type !== "tradeoff") {
        violations.push({
          rule: "evidence",
          detail: `entry ${view.entry.id}: message ${m.id} has no citation, verified: marker or vote`,
        });
      }
    }
  }

  // title: non-empty, at most 80 characters, not cut mid-word.
  for (const view of live) {
    const title = view.entry.title ?? "";
    if (title.trim().length === 0) {
      violations.push({ rule: "title", detail: `entry ${view.entry.id} has an empty title` });
    } else if (title.trim().length > MAX_TITLE) {
      violations.push({ rule: "title", detail: `entry ${view.entry.id} title is longer than ${MAX_TITLE} characters` });
    } else if (cutMidWord(title)) {
      violations.push({ rule: "title", detail: `entry ${view.entry.id} title is cut mid-word: ${title.trim()}` });
    }
  }

  // newest-candidate: a live entry whose stored state or messages name an
  // older candidate is stale, and a stale entry must not render as current.
  if (input.newestCandidateSha) {
    for (const view of live) {
      if (view.entry.stateSha && view.entry.stateSha !== input.newestCandidateSha) {
        violations.push({
          rule: "newest-candidate",
          detail: `entry ${view.entry.id} state computed against ${view.entry.stateSha}, not newest ${input.newestCandidateSha}`,
        });
      } else if (view.staleState) {
        violations.push({
          rule: "newest-candidate",
          detail: `entry ${view.entry.id} state is not computed against newest candidate ${input.newestCandidateSha}`,
        });
      }
    }
  }

  // accounting: the footer must reconcile.
  const expected = expectedAccounting(projected.accounting);
  if (projected.accounting.unaccounted !== expected) {
    violations.push({
      rule: "accounting",
      detail: `${projected.accounting.unaccounted} unaccounted messages (${accountingLine(projected.accounting)})`,
    });
  }

  const firstLine = violations.length > 0 ? `${violations[0].detail}` : undefined;
  return { ok: violations.length === 0, violations, firstLine };
}

function isNonLive(m: Message): boolean {
  return m.state === "merged" || m.state === "dropped" || m.state === "resolved" || m.state === "superseded";
}

/** unaccounted — raw − (linked + dropped + merged + resolved) — is 0 exactly
 * when every raw message is accounted for. */
function expectedAccounting(a: Accounting): number {
  return a.raw - (a.linked + a.dropped + a.merged + a.resolved);
}

/** A REVIEW_LINT_FAILED event, one per violation, so the log records what the
 * view's first line says. The conductor/CLI appends these; the lint itself
 * never mutates state. */
export function reviewLintFailedEvents(result: ReviewLintResult, at?: string): Array<{ type: "REVIEW_LINT_FAILED"; rule: string; detail: string; at?: string }> {
  return result.violations.map((v) => ({ type: "REVIEW_LINT_FAILED" as const, rule: v.rule, detail: v.detail, ...(at ? { at } : {}) }));
}

/** Re-export so callers need only this module for the common case. */
export { entryTypeOf, type Entry, type EntryView };
