// Plan 07e (E1/A1): every bound the conductor enforces, in one named table.
//
// A bound is a limit, never a decision: when one is reached the conductor
// records BOUND_REACHED (and, for the rounds budget, LEAD_CALLED) and takes
// the action named here. The owner remains the judge of every candidate.
//
// This module is deliberately pure: it imports nothing, so lint, the plan
// parser, the conductor and the views can all read the same table without a
// cycle. The numeric defaults are in minutes (or a count for `rounds`); the
// conductor's own DEFAULT_DEADLINES uses these values.

export type BoundKind =
  | "worker-attempt"
  | "check"
  | "review"
  | "evaluate"
  | "gate"
  | "sh"
  | "fact"
  | "fixer-attempt"
  | "merge"
  | "final-check"
  | "owner-wait"
  | "rounds";

export interface BoundDefinition {
  kind: BoundKind;
  /** The Org keyword the plan may set. */
  keyword: string;
  /** The conductor deadline field (or contract count) it fills. */
  field: string;
  /** `minutes` for a duration, `count` for the rounds budget. */
  unit: "minutes" | "count";
  /** Today's default, in `unit`. */
  defaultValue: number;
  /** What the conductor does when the bound is reached. Never accepts,
   * rejects or rules on a candidate. */
  action: string;
}

/** E1/A1: the complete table. Every default is today's value. */
export const BOUNDS: readonly BoundDefinition[] = Object.freeze([
  {
    kind: "worker-attempt",
    keyword: "TT_ATTEMPT_MINUTES",
    field: "workerAttemptMs",
    unit: "minutes",
    defaultValue: 45,
    action: "end the attempt (ATTEMPT_TIMED_OUT); the work is saved and a repair round is spent",
  },
  {
    kind: "check",
    keyword: "TT_CHECK_MINUTES",
    field: "checkMs",
    unit: "minutes",
    defaultValue: 5,
    action: "kill the check's process group and record the check as failed",
  },
  {
    kind: "review",
    keyword: "TT_REVIEW_MINUTES",
    field: "reviewMs",
    unit: "minutes",
    defaultValue: 15,
    action: "record the seat's review as timed out; the seat is retried under the existing review rule",
  },
  {
    kind: "evaluate",
    keyword: "TT_EVALUATE_MINUTES",
    field: "evaluateMs",
    unit: "minutes",
    defaultValue: 10,
    action: "publish the raw messages unevaluated and move on",
  },
  {
    kind: "gate",
    keyword: "TT_GATE_MINUTES",
    field: "gateMs",
    unit: "minutes",
    defaultValue: 30,
    action: "kill the gate's process group and record the gate as failed",
  },
  {
    kind: "sh",
    keyword: "TT_SH_MINUTES",
    field: "shCommandMs",
    unit: "minutes",
    defaultValue: 3,
    action: "kill the command's process group and return its timeout to the agent",
  },
  {
    kind: "fact",
    keyword: "TT_FACT_MINUTES",
    field: "reproductionMs",
    unit: "minutes",
    defaultValue: 5,
    action: "record the doneWhen fact as timed out; it is never retried",
  },
  {
    kind: "fixer-attempt",
    keyword: "TT_FIX_ATTEMPT_MINUTES",
    field: "fixerAttemptMs",
    unit: "minutes",
    defaultValue: 45,
    action: "end the fixer attempt and skip its commit",
  },
  {
    kind: "merge",
    keyword: "TT_MERGE_MINUTES",
    field: "mergeMs",
    unit: "minutes",
    defaultValue: 10,
    // 07e E1: abort the merge and keep what merged before it; the fix round's
    // candidate is the merges done so far, never the timed-out one.
    action: "abort the merge; the fix round's candidate is the merges done so far",
  },
  {
    kind: "final-check",
    keyword: "TT_FINAL_CHECK_MINUTES",
    field: "finalCheckMs",
    unit: "minutes",
    // The final check follows the ordinary check unless set separately.
    defaultValue: 5,
    action: "kill the final check's process group and record the final checks as failed",
  },
  {
    kind: "owner-wait",
    keyword: "TT_OWNER_WAIT_MINUTES",
    field: "ownerWaitMs",
    unit: "minutes",
    defaultValue: 240,
    action: "record the wait and send a reminder; the request stays open and the owner still decides",
  },
  {
    kind: "rounds",
    keyword: "TT_ROUNDS",
    field: "rounds",
    unit: "count",
    defaultValue: 3,
    action: "record LEAD_CALLED and park in AWAITING_OWNER with today's not-accepted reasons",
  },
]);

const BY_KEYWORD = new Map(BOUNDS.map((b) => [b.keyword, b]));
const BY_KIND = new Map(BOUNDS.map((b) => [b.kind, b]));

export function boundDefinition(kind: BoundKind): BoundDefinition {
  return BY_KIND.get(kind)!;
}

export function boundByKeyword(keyword: string): BoundDefinition | undefined {
  return BY_KEYWORD.get(keyword);
}

export function boundKeywords(): string[] {
  return BOUNDS.map((b) => b.keyword);
}

/** The default in milliseconds for a duration bound (the conductor's own
 * defaults use this). `rounds` has no milliseconds. */
export function boundDefaultMs(kind: Exclude<BoundKind, "rounds">): number {
  return boundDefinition(kind).defaultValue * 60_000;
}

/** E3/A4: why a bound keyword's raw value is invalid, or undefined when it
 * is a positive whole number. Kept as one helper so Emacs's silent parse and
 * `tt lint` cannot disagree. */
export function boundIssue(raw: unknown): string | undefined {
  const text = typeof raw === "string" ? raw.trim() : typeof raw === "number" ? String(raw) : "";
  if (text.length === 0) return "is empty (a bound must be a positive whole number)";
  if (!/^[0-9]+$/.test(text)) return `must be a positive whole number, got "${text}"`;
  const n = Number(text);
  if (!Number.isSafeInteger(n) || n <= 0) return `must be a positive whole number, got "${text}"`;
  return undefined;
}

/** `check 2m/5m`: the one status spelling of a bound's elapsed and limit. */
export function boundText(kind: BoundKind, elapsedMs: number, limitMs: number): string {
  return `${kind} ${minutes(elapsedMs)}/${minutes(limitMs)}`;
}

function minutes(ms: number): string {
  return `${Math.max(0, Math.floor(ms / 60_000))}m`;
}
