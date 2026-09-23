// Work packet 2a, design §3.3: a diff path matching the phase's BOUNDARIES
// globs, with no decision or finding citing it, becomes a conductor-
// computed `trigger` Decision record — no model involved in detecting it.
// A trigger starts at class `delegated` (design §3.4: worker proposes,
// reviewer may raise, only the owner lowers — a conductor trigger has no
// worker proposal at all, so it starts needing a real vote rather than the
// owner-mandatory `reserved`; see conductor.ts's
// `#recordBoundaryDataAndSample`), so a reviewer "classifies" it by voting
// on it like any other delegated decision — this test's own reviewers all
// approve it, so the run reaches DONE in one round.

import assert from "node:assert/strict";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";

test("boundary-trigger: a diff path matching BOUNDARIES with no citing record becomes a trigger decision", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    boundaries: ["config/**"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "mkdir -p config && printf 'x' > config/settings.json" },
        {
          kind: "call-submit",
          tool: "submit_phase",
          // No decision at all — nothing cites config/settings.json, so the
          // conductor's own boundary-trigger computation is the only thing
          // that puts a record on it.
          args: { decisions: [], assumptions: [], deviations: [] },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      // The trigger decision already exists by the time a reviewer is
      // dispatched (boundary computation runs synchronously right after
      // FREEZE_COMPLETED, well before REVIEWING's own reviewer dispatch).
      const trigger = () => state.phase.decisions.find((d) => d.source === "trigger");
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
              ballots: trigger()
                ? [
                    {
                      decisionId: trigger()!.id,
                      vote: "approve",
                      rationale: "config/settings.json's change is a trivial settings value, adequately covered by this review",
                      evidence: ["reviewed config/settings.json directly"],
                    },
                  ]
                : [],
              findings: [],
            },
          },
        ],
      };
    },
  });

  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED",
      90_000,
      50,
      setup.runDir,
    );
    assert.equal(
      setup.conductor.state.phase.phase,
      "DONE",
      `expected DONE (the trigger classified/approved by all three reviewers); got ${JSON.stringify(setup.conductor.state.phase)}`,
    );

    const trigger = setup.conductor.state.phase.decisions.find((d) => d.source === "trigger");
    assert.ok(trigger, `expected a trigger decision; got ${JSON.stringify(setup.conductor.state.phase.decisions)}`);
    assert.equal(trigger!.class, "delegated");
    assert.ok(trigger!.choice.includes("config/settings.json"), `trigger choice should name the path; got ${trigger!.choice}`);

    const records = readEvents(setup.runDir);
    const decisionAddedEvents = records
      .filter((r) => r.kind === "event" && (r.event as { type: string }).type === "DECISION_ADDED")
      .map((r) => (r.event as unknown as { decision: { source: string } }).decision);
    assert.ok(decisionAddedEvents.some((d) => d.source === "trigger"), "a DECISION_ADDED event with source 'trigger' should be logged");

    const ballotsOnTrigger = records.filter(
      (r) =>
        r.kind === "event" &&
        (r.event as { type: string }).type === "BALLOT_CAST" &&
        (r.event as unknown as { ballot: { decisionId: string } }).ballot.decisionId === trigger!.id,
    );
    assert.ok(ballotsOnTrigger.length >= 3, `all three reviewers should have voted on (classified) the trigger at least once; got ${ballotsOnTrigger.length}`);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
