// Phase 1b work-packet item 3's exit-gate test: design §2.2's "before and
// after every check ... the conductor verifies that the checkout still
// matches its candidate commit exactly ... a mismatch invalidates that
// gate's result and marks the run integrity-violated for the owner."
//
// A test-hook check command (the second entry in `plan.checks`) tampers
// with the disposable checkout mid-check by writing a new untracked file
// into it — exactly what a stray/escaped process could do. Asserts:
//   - that check's own result is invalidated: never counted `passed`;
//   - `phase.integrityViolated` becomes true — a logged fact (INTEGRITY_
//     VIOLATED), not an in-memory-only flag;
//   - it is STILL true after `rebuildState` rebuilds state purely from
//     `events.jsonl` (design §9.3 crash-safe recovery), i.e. it survives a
//     conductor restart.

import assert from "node:assert/strict";
import { test } from "node:test";

import { cleanupDir, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import { rebuildState } from "../../src/conductor.ts";

test("integrity-violated: a tampered check checkout invalidates that check and marks the run integrity-violated, surviving rebuildState", async () => {
  const setup = await setupConductor({
    // The first command passes cleanly; the second is the "test hook":
    // it mutates the checkout's on-disk content mid-check, simulating a
    // stray/escaped process editing files under a checkout the conductor
    // believes is inert.
    checks: ["true", "echo tampered > tampered-by-test-hook.txt"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
    deadlines: {
      abortGraceMs: 300,
      termGraceMs: 300,
      helloTimeoutMs: 5_000,
      workerAttemptMs: 20_000,
      checkMs: 20_000,
      freezeMs: 20_000,
    },
  });

  await setup.conductor.start();
  try {
    // Every attempt re-tampers via the same check command, so checks never
    // pass; the repair budget (3 rounds, default) runs out and the phase
    // parks in AWAITING_OWNER — a stable end state to wait for.
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);

    const state = setup.conductor.state;
    assert.equal(state.phase.integrityViolated, true, "integrityViolated must be set");
    assert.notEqual(state.phase.checks?.passed, true, "the tampered check must never count as passed");

    // It is a logged fact: an INTEGRITY_VIOLATED event actually appears in
    // events.jsonl, not just an in-memory field.
    const events = readEvents(setup.runDir);
    assert.ok(
      events.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "INTEGRITY_VIOLATED"),
      "expected a logged INTEGRITY_VIOLATED event",
    );

    await setup.conductor.stop();

    // Survives a conductor restart: rebuildState folds events.jsonl from
    // scratch (design §9.3) and must reproduce the same flag.
    const rebuilt = rebuildState(setup.runDir, setup.plan);
    assert.equal(rebuilt.phase.integrityViolated, true, "integrityViolated must survive rebuildState");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
