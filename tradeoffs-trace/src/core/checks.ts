// The effective ordered list of check commands the conductor executes at a
// gate (design §6.2 "CHECKING", §6.3 "every CHECKS command passed", §6.4
// "Run CHECKS on a fresh checkout of I").
//
// Two sources carry CHECKS:
//   - the plan's global list (`#+TT_CHECKS:` -> `RunPlanFile.checks`), and
//   - the current phase contract's own list (`:CHECKS:` -> `PhaseContract.checks`).
//
// The design does not specify a separate inheritance policy for the two, so
// the conductor resolves a single effective list: the global commands first,
// then the phase's own commands, in their given order. An exact duplicate
// command string (byte-for-byte equal) is executed once, at its *first*
// occurrence — the duplicate rule is pure string equality: no splitting of
// shell expressions (`a && b` is one command, not two), no normalization, no
// reordering.
//
// This module is deliberately pure so the rule can be unit-tested directly;
// `Conductor#runChecks` (candidate C) and `Conductor#runProbe` (probed
// integration I) both call it, so both gates execute the same list.

/** Resolve the one effective, order-preserving, exact-duplicate-deduped list
 * of check commands for a gate. `globalChecks` comes first, then
 * `phaseChecks`; a command already seen (by exact string equality) is
 * dropped from its later occurrence. */
export function effectiveChecks(
  globalChecks: readonly string[],
  phaseChecks: readonly string[],
): string[] {
  const seen = new Set<string>();
  const effective: string[] = [];
  for (const command of [...globalChecks, ...phaseChecks]) {
    if (seen.has(command)) continue;
    seen.add(command);
    effective.push(command);
  }
  return effective;
}

// Plan 05d: re-running a newly failing test alone happens inside the failing
// check command's own deadline (design §8.1's per-command limit). The budget
// helper is pure so the boundary — no re-run may start once the deadline has
// passed — is unit-tested directly rather than inferred from a timing test.

/** Milliseconds left of a failing check command's deadline, never negative.
 * Zero means no time is left, so the caller runs no re-run and keeps the
 * strict rule (a re-run that cannot finish proves nothing). */
export function rerunBudgetMs(deadlineAt: number, now: number): number {
  return Math.max(0, deadlineAt - now);
}

// ---------------------------------------------------------------------------
// Plan 06c: the final check.
//
// A plan may name one final command (`#+TT_FINAL_CHECKS`, overridden by a
// phase's `:FINAL_CHECKS:`). Every candidate runs the phase's ordinary
// checks (`round`); only the candidate about to be accepted runs the phase's
// checks AND the final check (`final`), once. `checkTier` is the ONE place
// that decides the tier; the conductor derives the command list from it.
// ---------------------------------------------------------------------------

export type CheckTier = "round" | "final";

/** What `checkTier` reads about the candidate: whether review has passed with
 * no open blocker, so the candidate is the one about to be accepted. */
export interface CheckCandidateContext {
  sha: string;
  reviewed: boolean;
  openBlocker: boolean;
}

/** The one final-check command a phase declares, or undefined. An empty or
 * whitespace-only declaration is no final check (the plan behaves as before).
 */
export function finalCheckOf(phase: { finalChecks?: readonly string[] } | undefined): string | undefined {
  const first = (phase?.finalChecks ?? []).map((c) => c.trim()).find((c) => c.length > 0);
  return first;
}

/** `final` only for a candidate that has passed review with no open blocker
 * AND whose phase declares a final check; `round` otherwise. This is the
 * only place the tier is decided. */
export function checkTier(
  phase: { finalChecks?: readonly string[] } | undefined,
  candidate: CheckCandidateContext,
): CheckTier {
  if (!finalCheckOf(phase)) return "round";
  return candidate.reviewed && !candidate.openBlocker ? "final" : "round";
}

/** The ordered command list a candidate runs at `tier`: the phase's checks
 * (`round`), plus the phase's final check appended (`final`). Built from
 * `checkTier`'s decision, so no caller chooses a command itself. */
export function checkCommands(
  globalChecks: readonly string[],
  phaseChecks: readonly string[],
  finalChecks: readonly string[] | undefined,
  tier: CheckTier,
): string[] {
  const base = effectiveChecks(globalChecks, phaseChecks);
  if (tier !== "final") return base;
  return effectiveChecks(base, finalChecks ?? []);
}

// ---------------------------------------------------------------------------
// The check record
// ---------------------------------------------------------------------------

/** One executed check command's outcome, as the record holds it. */
export interface CheckRecordCommand {
  command: string;
  exitCode: number | null;
  signal?: string | null;
  timedOut: boolean;
  durationMs: number;
  passed: boolean;
  /** The log file name under the candidate's checks directory. */
  log?: string;
}

/** `checks/<sha>/record.json`: what one candidate's check run did, which tier
 * it ran at, the final command when there was one, and the machine's load at
 * the run. The record tells the truth about the tier, so a reader can tell a
 * repair candidate's `round` run from the accepted candidate's `final` one. */
export interface CheckRecord {
  candidateSha: string;
  baseSha?: string;
  tier: CheckTier;
  passed: boolean;
  commands: CheckRecordCommand[];
  /** The final command this run included (empty for a `round` record). */
  finalCommands: string[];
  /** The machine's 1-minute load average when the run started. */
  load1: number;
  /** Free system memory in MiB when the run started. */
  freeMemMB: number;
  at: string;
}

function isNumber(v: unknown): v is number {
  return typeof v === "number" && Number.isFinite(v);
}

/** Parse a `record.json`; undefined for anything malformed (a hand-edited or
 * half-written record is no record, never a passing one). */
export function parseCheckRecord(value: unknown): CheckRecord | undefined {
  if (!value || typeof value !== "object") return undefined;
  const r = value as Record<string, unknown>;
  if (typeof r.candidateSha !== "string" || r.candidateSha.length === 0) return undefined;
  if (r.tier !== "round" && r.tier !== "final") return undefined;
  if (typeof r.passed !== "boolean") return undefined;
  if (!isNumber(r.load1) || !isNumber(r.freeMemMB)) return undefined;
  if (!Array.isArray(r.commands)) return undefined;
  const commands: CheckRecordCommand[] = [];
  for (const c of r.commands) {
    if (!c || typeof c !== "object") return undefined;
    const e = c as Record<string, unknown>;
    if (typeof e.command !== "string") return undefined;
    if (!isNumber(e.durationMs) || typeof e.timedOut !== "boolean" || typeof e.passed !== "boolean") return undefined;
    commands.push({
      command: e.command,
      exitCode: e.exitCode === null || isNumber(e.exitCode) ? (e.exitCode as number | null) : null,
      signal: typeof e.signal === "string" || e.signal === null ? (e.signal as string | null) : undefined,
      timedOut: e.timedOut,
      durationMs: e.durationMs,
      passed: e.passed,
      ...(typeof e.log === "string" ? { log: e.log } : {}),
    });
  }
  return {
    candidateSha: r.candidateSha,
    ...(typeof r.baseSha === "string" ? { baseSha: r.baseSha } : {}),
    tier: r.tier,
    passed: r.passed,
    commands,
    finalCommands: Array.isArray(r.finalCommands) ? r.finalCommands.filter((c): c is string => typeof c === "string") : [],
    load1: r.load1,
    freeMemMB: r.freeMemMB,
    at: typeof r.at === "string" ? r.at : "",
  };
}
