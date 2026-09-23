// Fault injection for phase-1's crash-recovery suite (design §9.3).
//
// `TT_CRASH_AT=<boundary>` makes the conductor exit *abruptly* — no
// `stop()`, no lock release, no socket close — at a single named point.
// This is deliberate: design §9.3's whole reconciliation table exists
// because a real crash never gets to run cleanup code, and the run lock's
// flock-holding perl helper (src/effects/lock.ts) is only ever released by
// the OS noticing the holder process is gone, not by any code path here.
//
// `CRASH_BOUNDARIES` is every phase-1 boundary this packet's conductor can
// actually be asked to crash at: for each external effect in design §9.3's
// reconciliation table (create worktree, agent attempt, freeze commit,
// check run, probe, publish) both "before the effect (after its intent is
// logged)" and "after the effect but before its completion event" —
// `before_<effect>` / `after_<effect>`.
//
// `PHASE_2_CRASH_BOUNDARIES` names design §9.3's other two boundaries —
// "after an owner-command event but before its inbox file moves" and
// "between steer and its acknowledgement" — now, so the full boundary list
// this design section describes is documented in one place. Phase 1 has no
// inbox or steer mechanism at all (both are phase 2 work), so these are
// never fired by anything in this packet: `crashAt` only ever recognizes a
// `CrashBoundary`, and no phase-1 code path passes one of these two names to
// it. test/crash/crash-suite.test.ts asserts they are listed here and
// documented as not-yet-exercised, rather than faking a crash inside a
// mechanism that does not exist yet.
export const CRASH_BOUNDARIES = [
  "before_create_worktree",
  "after_create_worktree",
  "before_dispatch_worker",
  "after_dispatch_worker",
  "before_freeze",
  "after_freeze",
  "before_run_checks",
  "after_run_checks",
  "before_dispatch_probe",
  "after_dispatch_probe",
  "before_publish_cas",
  "after_publish_cas",
] as const;

export type CrashBoundary = (typeof CRASH_BOUNDARIES)[number];

export const PHASE_2_CRASH_BOUNDARIES = ["before_inbox_move", "before_steer_ack"] as const;

export type Phase2CrashBoundary = (typeof PHASE_2_CRASH_BOUNDARIES)[number];

/** A distinctive exit code (never one a normal `process.exit` in this
 * codebase uses) so a test can tell "the conductor reached this boundary and
 * crashed there" apart from any other reason the process might have ended. */
export const CRASH_EXIT_CODE = 87;

/** Exits the process immediately iff `TT_CRASH_AT` (read fresh every call,
 * never cached, so tests can rely on plain env-var inheritance) equals
 * `boundary`. A no-op whenever the env var is unset or names a different
 * boundary — which is always true outside this packet's own crash-suite, so
 * this is safe to leave wired into the conductor's normal code paths
 * unconditionally. */
export function crashAt(boundary: CrashBoundary): void {
  if (process.env.TT_CRASH_AT === boundary) {
    process.exit(CRASH_EXIT_CODE);
  }
}
