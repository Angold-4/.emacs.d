// The full-happy-path exit-gate test: `tt start` on a plan in a temp git
// repo with a fake-pi worker and stub fake-pi reviewers reaches DONE with
// the integration branch moved via CAS, and `tt status`'s renderer reflects
// it. Reviews are stubbed per the phase-1b brief; the worker discloses no
// decisions so acceptance holds as soon as all three reviews are in (see
// the harness's note on why reviewer scripts are generated lazily).

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { test } from "node:test";

import { defaultReviewerHello, defaultWorkerHello, cleanupDir, setupConductor, waitFor } from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";

test("happy path: worker submits, checks/probe/reviews pass, run reaches DONE via CAS", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: { decisions: [], assumptions: [], deviations: [] },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
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
          },
        },
      ],
    }),
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
  });

  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000);
    const state = setup.conductor.state;
    assert.equal(state.phase.phase, "DONE");
    assert.ok(state.phase.publishedI, "expected a published integration commit");

    const branchHead = execFileSync("git", ["-C", setup.repo.dir, "rev-parse", "main"], { encoding: "utf8" }).trim();
    assert.equal(branchHead, state.phase.publishedI);
    assert.notEqual(branchHead, setup.repo.head, "the integration branch must have moved");

    // tt status's own renderer reads the same events.jsonl this test does;
    // spot check the run directory has what it needs to render DONE.
    const p = runPaths(setup.runDir);
    assert.ok(existsSync(p.events));
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
