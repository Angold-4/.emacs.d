// Published-behavior verification (R2 F13 companion gate): prove that the
// *published* integration commit I actually implements the phase's feature,
// by running the feature's real API on a fresh checkout of I — not by
// searching the event log for a `CHECKS_PASSED` event.
//
// The live single-phase fixture's feature is `sum.js` gaining a
// `subtract(a, b)` export (with `sum` still working). This module is shared
// between `test/live/live-single-phase.test.ts` (which runs it on a fresh
// checkout of the published I before deleting its fixtures) and a
// normal-suite test (`test/conductor/published-behavior.test.ts`) that
// proves it accepts a correct candidate and rejects a no-op one. Keeping it
// out of the `.test.ts` glob keeps `make check` from treating it as a test.
//
// It shells out to the real `node` binary, so a broken/missing export is a
// genuine failure rather than a mocked environment object. `childEnv()`
// strips the Node test-runner recursion markers (F13) so this verifier
// itself works when run from inside `node --test`.

import { execFileSync } from "node:child_process";

import { childEnv } from "../../src/effects/shell.ts";

export interface BehaviorResult {
  ok: boolean;
  /** Human-readable evidence: the verifier's own output on success, or the
   * child's stderr/stdout (the assertion failure) on failure. */
  detail: string;
}

/** Runs in the checkout via `node -e`; prints `BEHAVIOR-OK` only if every
 * assertion holds. Each assertion names the case, so a failure detail says
 * exactly which input was wrong. */
const PROBE_SCRIPT = `
const assert = require("node:assert/strict");
const mod = require("./sum.js");
assert.equal(typeof mod.sum, "function", "sum must still be exported as a function");
assert.equal(typeof mod.subtract, "function", "subtract must be exported as a function");
assert.equal(mod.sum(2, 3), 5, "sum(2, 3) must still be 5");
assert.equal(mod.subtract(5, 3), 2, "subtract(5, 3) must be 2 (positive inputs)");
assert.equal(mod.subtract(3, 5), -2, "subtract(3, 5) must be -2 (negative result)");
assert.equal(mod.subtract(4, 4), 0, "subtract(4, 4) must be 0 (zero result)");
assert.equal(mod.subtract(0, 0), 0, "subtract(0, 0) must be 0 (zero inputs)");
console.log("BEHAVIOR-OK");
`;

/** Verifies the `subtract` feature on a fresh checkout directory `dir`
 * (which must contain the fixture's `sum.js`). Returns `ok: false` with the
 * child's own diagnostic when the feature is absent or wrong. */
export function verifySubtractFeature(dir: string): BehaviorResult {
  try {
    const output = execFileSync(process.execPath, ["-e", PROBE_SCRIPT], {
      cwd: dir,
      encoding: "utf8",
      env: childEnv(),
      stdio: ["ignore", "pipe", "pipe"],
    });
    return { ok: output.includes("BEHAVIOR-OK"), detail: output.trim() };
  } catch (err) {
    const e = err as { stdout?: string | Buffer; stderr?: string | Buffer; message?: string };
    const out = `${e.stdout?.toString() ?? ""}${e.stderr?.toString() ?? ""}`.trim();
    return { ok: false, detail: out.length > 0 ? out : String(e.message ?? err) };
  }
}
