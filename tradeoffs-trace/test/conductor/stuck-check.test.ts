import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import { readNotifications } from "../../src/notify.ts";

for (const recover of [true, false]) test(`compilation timeout ${recover ? "recovers on the same candidate" : "parks and notifies after one retry"}`, async () => {
  const setup = await setupConductor({ checks: ["true"], stubReviews: true,
    deadlines: { checkMs: 1_000, termGraceMs: 100, abortGraceMs: 100, helloTimeoutMs: 10_000 },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [
      { kind: "call-sh", command: "echo candidate > marker.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ] }),
    reviewerScriptFor: (reviewer, state) => ({ hello: defaultReviewerHello(), steps: [{ kind: "call-submit", tool: "submit_review", args: {
      reviewer, phaseId: state.phase.phaseId, candidateSha: state.phase.candidate?.sha,
      contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [], ballots: [], findings: [],
    } }] }),
  });
  const marker = path.join(setup.scriptsDir, "check-attempted");
  const command = `if test -f marker.txt; then ${recover ? `if test -f '${marker}'; then exit 0; fi; touch '${marker}'; ` : ""}echo 'Compiling crate v1.0'; sleep 10; fi`;
  setup.plan.checks = [command];
  setup.plan.phases[0].checks = [command];
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === (recover ? "DONE" : "AWAITING_OWNER"), 60_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.repairRoundsUsed, 0);
    const records = readEvents(setup.runDir);
    assert.equal(records.filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "FREEZE_COMPLETED").length, 1);
    assert.equal(records.filter((r) => r.kind === "check_infrastructure_retry").length, 1);
    assert.equal(records.filter((r) => r.kind === "check_infrastructure_retried").length, 1);
    assert.equal(records.filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "CHECKS_FAILED").length, 0);
    if (!recover) {
      const reason = setup.conductor.state.phase.ownerRequests.at(-1)!.reason;
      assert.match(reason, /infrastructure stall.*Compiling.*load1/s);
      assert.ok(readNotifications(setup.runRoot).some((n) => /infrastructure stall/.test(n.reason)));
      const sha = setup.conductor.state.phase.candidate!.sha;
      const record = JSON.parse(fs.readFileSync(path.join(setup.runDir, "checks", sha, "record.json"), "utf8"));
      assert.equal(record.commands[0].class, "infrastructure-stall");
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.repo.dir); cleanupDir(setup.runRoot); cleanupDir(setup.scriptsDir);
  }
});
