// Plan 01g: criterion disputes, end to end through the conductor.
//
// The worker says a criterion cannot be met as written (not merely unmet) and
// proposes replacement wording. The conductor records it as a `reserved`
// amendment decision; M, A and B vote on it in turn 2 like any other record.
// A passing normal tally (M, plus one of A/B) replaces the acceptance item
// for this phase, bumps the contract version, logs the change as an event,
// and starts a fresh attempt so the NEXT candidate is judged against the new
// wording. The run reaches DONE without ever parking on the owner and without
// consuming a repair round for the dispute.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";
import type { Reviewer } from "../../src/core/types.ts";

test("criterion-disputes: a disputed criterion is amended and the next candidate is judged against the new wording to DONE", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        // A genuinely new tree each attempt, so the repair is a new candidate.
        { kind: "call-sh", command: `printf 'attempt ${attempt}\\n' > attempt.txt` },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [],
            assumptions: [],
            deviations: [],
            // Attempt 1 disputes the default harness criterion; attempt 2 is
            // judged under the amended wording and needs no dispute.
            ...(attempt === 1
              ? {
                  criterionDispute: {
                    criterion: "it works",
                    why: "the literal wording demands a certainty no implementation can give",
                    proposedWording: "the tests pass",
                  },
                }
              : {}),
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer: Reviewer, state) => {
      // Vote for every still-proposed votable record. The amendment is a
      // reserved decision, so it is demanded in turn 2 exactly like any
      // other record.
      const votable = state.phase.decisions.filter(
        (d) =>
          (d.class === "reserved" || d.class === "delegated") &&
          !d.supersededBy &&
          !d.supersededByCorrection &&
          (!d.amendment || d.amendment.status === "proposed"),
      );
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
              ballots: votable.map((d) => ({
                decisionId: d.id,
                vote: "approve",
                rationale: "the proposed wording is satisfiable and still meaningful",
                evidence: ["reviewed the candidate diff"],
              })),
            },
          },
        ],
      };
    },
  });

  try {
    await setup.conductor.start();
    // The amendment must be recorded as a reserved decision on the candidate.
    await waitFor(
      () => setup.conductor.state.phase.decisions.some((d) => d.amendment !== undefined),
      60_000,
      50,
      setup.runDir,
    );
    const proposed = setup.conductor.state.phase.decisions.find((d) => d.amendment)!;
    assert.equal(proposed.class, "reserved");
    assert.equal(proposed.amendment!.criterion, "it works");

    await waitFor(
      () => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase),
      180_000,
      50,
      setup.runDir,
    );
    const phase = setup.conductor.state.phase;
    assert.equal(phase.phase, "DONE", `expected DONE; got ${JSON.stringify({ phase: phase.phase, blockedReason: phase.blockedReason })}`);

    // The criterion was replaced for this phase, from the next candidate on.
    assert.ok(phase.contract.acceptance.includes("the tests pass"), "the amended wording must be in the contract");
    assert.ok(!phase.contract.acceptance.includes("it works"), "the old wording must be gone");
    assert.equal(phase.contract.contractVersion.snapshot, 2, "the contract version must bump");

    // The amendment is recorded and applied, and the dispute consumed no
    // repair round and never parked the run on the owner.
    const amendment = phase.decisions.find((d) => d.amendment?.id === proposed.amendment!.id)!;
    assert.equal(amendment.amendment?.status, "applied");
    assert.equal(phase.repairRoundsUsed, 0, "a dispute must not consume a repair round by itself");
    assert.equal(phase.ownerRequests.filter((r) => r.status === "open").length, 0, "no owner request should be open");

    const events = readEvents(setup.runDir);
    const types = events.filter((r) => r.kind === "event").map((r) => (r.event as { type: string }).type);
    assert.ok(types.includes("CRITERION_AMENDED"), "the wording change must be logged as an event");
    assert.ok(!types.includes("AWAITING_OWNER"), "the run must never wait on the owner for the dispute");
    assert.ok(!types.includes("REPAIR_ATTEMPT_STARTED"), "applying an amendment starts a fresh attempt, not a repair round");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("criterion-disputes: a correction naming the amendment id restores the original wording through the input box", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    // A quick inbox poll so the owner's correction lands while attempt 2 runs.
    deadlines: { inboxPollMs: 100 },
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        // Attempt 2 gives the owner a window to revert the amendment before
        // the next candidate is frozen.
        { kind: "call-sh", command: `sleep 5` },
        { kind: "call-sh", command: `printf 'attempt ${attempt}\\n' > attempt.txt` },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [],
            assumptions: [],
            deviations: [],
            ...(attempt === 1
              ? {
                  criterionDispute: {
                    criterion: "it works",
                    why: "the literal wording cannot be met",
                    proposedWording: "the tests pass",
                  },
                }
              : {}),
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer: Reviewer, state) => ({
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
            ballots: state.phase.decisions
              .filter(
                (d) =>
                  (d.class === "reserved" || d.class === "delegated") &&
                  !d.supersededBy &&
                  !d.supersededByCorrection &&
                  (!d.amendment || d.amendment.status === "proposed"),
              )
              .map((d) => ({ decisionId: d.id, vote: "approve", rationale: "satisfiable", evidence: ["reviewed"] })),
          },
        },
      ],
    }),
  });

  try {
    await setup.conductor.start();
    // Wait for the amendment to pass and be applied, then revert it while the
    // fresh attempt is running.
    await waitFor(
      () => setup.conductor.state.phase.decisions.some((d) => d.amendment?.status === "applied"),
      120_000,
      20,
      setup.runDir,
    );
    const applied = setup.conductor.state.phase.decisions.find((d) => d.amendment?.status === "applied")!;
    const inbox = runPaths(setup.runDir).inbox;
    fs.writeFileSync(
      path.join(inbox, "cmd-revert-1.json"),
      JSON.stringify({ type: "correction", text: `revert ${applied.amendment!.id} because the owner disagrees` }),
    );
    await waitFor(() => setup.conductor.state.phase.decisions.some((d) => d.amendment?.status === "reverted"), 30_000, 20, setup.runDir);

    await waitFor(
      () => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase),
      180_000,
      50,
      setup.runDir,
    );
    assert.equal(setup.conductor.state.phase.phase, "DONE", "the run must still reach DONE after the owner's revert");
    // The original wording is back: the amendment was restored to the letter.
    assert.deepEqual(setup.conductor.state.phase.contract.acceptance, ["it works"]);
    const appliedEvents = readEvents(setup.runDir).filter((r) => r.kind === "event");
    const types = appliedEvents.map((r) => (r.event as { type: string }).type);
    assert.ok(types.includes("CRITERION_REVERTED"), "the revert is logged as an event");
    const revertInput = (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.state === "reverted");
    assert.ok(revertInput, "the owner input is recorded as reverted");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
