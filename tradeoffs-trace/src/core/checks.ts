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
