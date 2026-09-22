// integration-recovery (phase-0 exit gate): probe of C1 fails -> `integration`
// finding -> repair -> C2 probe passes -> finding repaired -> accept ->
// publish moves the branch from H to I only via compare-and-swap. A publish
// completion reporting the branch not at H returns the phase to PROBING
// against the new head (design §6.4 step 3).

import assert from "node:assert/strict";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import type { State } from "../../src/core/types.ts";
import { approvingReview, baseState, CV } from "./helpers.ts";

const K = CV();

/** `ctx.state` is mutated in place by `step`, so every caller sees the
 * latest value through the same object — unlike returning a plain
 * `{ state, step }` pair, which would capture `state` by value once, at
 * return time, and never see later updates. */
function driveToReviewing(): { ctx: { state: State }; step: (event: unknown) => ReturnType<typeof reduce> } {
  const ctx: { state: State } = { state: baseState({ phase: "READY", integrationHead: "H0" }) };

  const step = (event: unknown) => {
    const result = reduce(ctx.state, event);
    assert.equal(result.ok, true, `event ${JSON.stringify(event)} was rejected: ${(result as { reason?: string }).reason}`);
    ctx.state = result.state;
    return result;
  };

  step({ type: "ATTEMPT_STARTED" });
  assert.equal(ctx.state.phase.phase, "IMPLEMENTING");

  step({ type: "SUBMIT_PHASE", decisions: [] });
  assert.equal(ctx.state.phase.phase, "FREEZING");

  step({ type: "FREEZE_COMPLETED", candidateSha: "C1" });
  assert.equal(ctx.state.phase.phase, "CHECKING");
  assert.equal(ctx.state.phase.candidate?.sha, "C1");

  step({ type: "CHECKS_PASSED" });
  assert.equal(ctx.state.phase.phase, "PROBING");

  // The probe of C1 fails: an `integration` finding is raised and the
  // phase returns to REPAIRING (budget remains: 0 of 3 rounds used).
  step({ type: "PROBE_FAILED", evidence: "merge conflict against H0" });
  assert.equal(ctx.state.phase.phase, "REPAIRING");
  const finding = ctx.state.phase.findings.find((f) => f.kind === "integration");
  assert.ok(finding, "an integration finding must be raised");
  assert.equal(finding!.status, "open");
  assert.equal(finding!.raisedBy, "conductor");

  // Repair: a new attempt (consumes one repair round), a new submission,
  // a new freeze giving C2.
  step({ type: "REPAIR_ATTEMPT_STARTED" });
  assert.equal(ctx.state.phase.phase, "IMPLEMENTING");
  assert.equal(ctx.state.phase.repairRoundsUsed, 1);

  step({ type: "SUBMIT_PHASE", decisions: [] });
  step({ type: "FREEZE_COMPLETED", candidateSha: "C2" });
  assert.equal(ctx.state.phase.candidate?.sha, "C2");

  step({ type: "CHECKS_PASSED" });
  assert.equal(ctx.state.phase.phase, "PROBING");

  // C2's probe passes: the integration finding closes as repaired.
  step({ type: "PROBE_PASSED", probedI: "I2" });
  assert.equal(ctx.state.phase.phase, "REVIEWING");
  const closedFinding = ctx.state.phase.findings.find((f) => f.kind === "integration");
  assert.equal(closedFinding!.status, "repaired");
  assert.equal(closedFinding!.repairedByCandidateSha, "C2");

  for (const reviewer of ["M", "A", "B"] as const) {
    step({ type: "REVIEW_SUBMITTED", review: approvingReview(reviewer, "C2", K) });
  }
  assert.equal(ctx.state.phase.phase, "RESOLVING");

  return { ctx, step };
}

test("integration-recovery: probe failure raises a finding, repair produces C2, its probe passes and the finding repairs", () => {
  const { ctx } = driveToReviewing();
  assert.equal(ctx.state.phase.phase, "RESOLVING");
});

test("integration-recovery: accept holds once the integration finding is repaired, and publish uses compare-and-swap", () => {
  const { ctx, step } = driveToReviewing();

  const acceptedResult = step({ type: "ACCEPTED", resolvedCorrectionIds: [] });
  assert.equal(acceptedResult.state.phase.phase, "ACCEPTED");

  step({ type: "PUBLISH_INTENT", expectedHead: "H0", candidateI: "I2" });
  assert.equal(ctx.state.phase.phase, "PUBLISHING");

  // CAS success: the branch was still at H0, so it moves to I2.
  step({ type: "PUBLISH_COMPLETED", newHead: "I2" });
  assert.equal(ctx.state.phase.phase, "DONE");
  assert.equal(ctx.state.phase.publishedI, "I2");
});

test("integration-recovery: a publish completion reporting the branch not at H returns the phase to PROBING against the new head", () => {
  const { ctx, step } = driveToReviewing();

  step({ type: "ACCEPTED", resolvedCorrectionIds: [] });
  step({ type: "PUBLISH_INTENT", expectedHead: "H0", candidateI: "I2" });
  assert.equal(ctx.state.phase.phase, "PUBLISHING");

  // CAS failure: something else moved the branch to H1 first (cannot
  // happen with serial phases, but is checked anyway — design §6.4 step 3).
  step({ type: "PUBLISH_STALE", actualHead: "H1" });
  assert.equal(ctx.state.phase.phase, "PROBING");
  assert.equal(ctx.state.phase.integrationHead, "H1");
  assert.equal(ctx.state.phase.probe, undefined, "the stale probe result must be discarded");
});
