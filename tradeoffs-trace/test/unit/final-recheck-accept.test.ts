// A final check that passes only on `tt recheck` is accepted.
//
// After a failed final check, applyRecheck keeps the failed record
// (`checks: {passed: false, tier: "final"}`) for the re-run. The
// FINAL_CHECKS_PASSED rows judged acceptance on that pre-event state, so the
// passing event matched no row, reduce() rejected it and the run sat in
// FINAL_CHECKING (index 20e run d3db1625, 2026-10-15).

import assert from "node:assert/strict";
import { test } from "node:test";
import { accept } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { Event, PhaseState, State } from "../../src/core/types.ts";
import { approvingReview, baseState, CV } from "./helpers.ts";

const K = CV();
const C1 = { sha: "C1", contractVersion: K };

/** A phase in FINAL_CHECKING whose final check failed once and was rechecked:
 * every other gate holds, only `checks` still carries the failed final run. */
function recheckedFinal(overrides: Partial<PhaseState> = {}): State {
  return baseState({
    phase: "FINAL_CHECKING",
    candidate: C1,
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: false, tier: "final" },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: approvingReview("M", "C1", K) },
      A: { review: approvingReview("A", "C1", K) },
      B: { review: approvingReview("B", "C1", K) },
    },
    inFlight: { run_final_checks: { actionId: "a1" } },
    contract: { ...baseState().phase.contract, finalChecks: ["make check-final"] },
    ...overrides,
  });
}

const PASSED: Event = { type: "FINAL_CHECKS_PASSED", candidateSha: "C1" };

test("a final check that passes on recheck accepts the candidate and records the passing checks", () => {
  const r = reduce(recheckedFinal(), PASSED);
  assert.ok(r.ok, `FINAL_CHECKS_PASSED was rejected: ${!r.ok ? r.reason : ""}`);
  assert.equal(r.state.phase.phase, "ACCEPTED");
  assert.deepEqual(r.state.phase.checks, { candidateSha: "C1", passed: true, tier: "final" });
  // done() and publish read accept() on the accepted state: it must hold there.
  assert.equal(accept(r.state.phase, "C1", K), true);
});

test("with a gate declared, a final check that passes on recheck moves on to GATING", () => {
  const gated = recheckedFinal({
    contract: { ...baseState().phase.contract, finalChecks: ["make check-final"], gate: "deploy/atlas.sh --clean --build" },
  });
  const r = reduce(gated, PASSED);
  assert.ok(r.ok, `FINAL_CHECKS_PASSED was rejected: ${!r.ok ? r.reason : ""}`);
  assert.equal(r.state.phase.phase, "GATING");
});

test("a queued correction still repairs after a final check that passes on recheck", () => {
  const queued = recheckedFinal({
    queuedCorrections: [{ id: "C-1", text: "use UTC everywhere" }],
    repairRoundsUsed: 0,
    repairRoundsGranted: 1,
  });
  const r = reduce(queued, PASSED);
  assert.ok(r.ok, `FINAL_CHECKS_PASSED was rejected: ${!r.ok ? r.reason : ""}`);
  assert.equal(r.state.phase.phase, "REPAIRING");
});
