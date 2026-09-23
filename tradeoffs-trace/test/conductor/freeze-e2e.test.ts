// End-to-end freeze test (phase 1b work-packet item 4, design §6.2): a
// fake-pi worker calls submit_phase and then, via a scripted `call-sh` step
// that launches a background loop, keeps writing into the live worktree —
// exactly the scenario §6.2 exists to guard against ("the worker, or
// anything it started, could keep editing files while they are being
// checked"). Asserts:
//   - the freeze's sweep actually finds and kills that writer (a `sweep`
//     log record with a non-empty `killed`), and marks the worktree
//     tainted;
//   - the writer process is provably dead afterwards;
//   - the materialized read-only candidate checkout, and a fresh disposable
//     checkout of the same candidate (exactly what a check gets), are
//     immune to anything written to the live worktree from that point on —
//     they are independent git clones of a fixed commit, never the live
//     worktree.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";
import { disposableCheckout } from "../../src/effects/git.ts";

function pidAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

test("freeze-e2e: a worker that keeps writing after submit_phase is swept, killed and tainted, and every checkout is immune to it", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: ({ repo }) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
        // Launched AFTER submit_phase, exactly as the brief asks: a
        // background loop that never stops on its own (so the freeze's
        // sweep is the only thing that can end it) writing into the live
        // worktree. Its pid is recorded outside the worktree (the repo
        // dir's parent would also work; using the repo dir itself keeps
        // this test independent of TT_WORKTREE's exact path) so the test
        // can check it after the worktree may have been swept/reset.
        {
          kind: "call-sh",
          // Redirected to /dev/null (not left attached to this command's
          // own stdout pipe): an orphaned grandchild that still holds a
          // parent pipe's write end open is a classic way to hang an
          // otherwise-finished Node process, independent of whatever the
          // conductor's own sweep does — this is purely about not
          // confusing that unrelated pipe-lifetime issue with what this
          // test actually checks.
          command: `( i=0; while true; do i=$((i+1)); echo "survivor-$i" >> survivor.txt; sleep 0.05; done ) >/dev/null 2>&1 & echo $! > "${path.join(repo.dir, "survivor.pid")}"`,
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
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000, freezeMs: 20_000 },
  });

  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000);
    const state = setup.conductor.state;

    // The freeze's own sweep found and killed the survivor — recorded as a
    // sweep event with a non-empty `killed`, and the run's own worktreeTainted
    // fact set true (design §2.2's "a sweep that found survivors marks the
    // worktree tainted").
    const events = readEvents(setup.runDir);
    const sweepWithSurvivor = events.find(
      (e) => e.kind === "sweep" && Array.isArray((e.event as { killed?: unknown[] }).killed) && (e.event as { killed: unknown[] }).killed.length > 0,
    );
    assert.ok(sweepWithSurvivor, "expected a sweep record that found and killed a survivor");

    const freezeCompleted = events.find(
      (e) => e.kind === "event" && (e.event as { type?: string }).type === "FREEZE_COMPLETED",
    );
    assert.ok(freezeCompleted, "expected a FREEZE_COMPLETED event");
    assert.equal((freezeCompleted!.event as { tainted?: boolean }).tainted, true, "the freeze must report tainted: true");

    // The survivor process is provably dead.
    const pidFile = path.join(setup.repo.dir, "survivor.pid");
    await waitFor(() => fs.existsSync(pidFile), 15_000);
    const pid = Number(fs.readFileSync(pidFile, "utf8").trim());
    await waitFor(() => !pidAlive(pid), 30_000);

    // Now that the run is DONE, simulate a hypothetical further escaped
    // write landing in the (still on-disk) worktree — the exact kind of
    // write §6.2 says must never be visible to a checkout. Every materialized
    // or disposable checkout of the candidate must be completely unaffected:
    // they are independent git clones of a fixed commit, not views onto the
    // live worktree.
    const candidate = state.phase.candidate!;
    const materializedDir = path.join(runPaths(setup.runDir).candidates, candidate.sha);

    const lateMarker = "late-escaped-write-after-done";
    const worktreeDir = runPaths(setup.runDir).worktree;
    fs.writeFileSync(path.join(worktreeDir, "late.txt"), lateMarker);

    assert.ok(
      !fs.existsSync(path.join(materializedDir, "late.txt")),
      "the materialized candidate checkout must not see a later live-worktree write",
    );

    const fresh = disposableCheckout(setup.plan.repo, candidate.sha);
    try {
      assert.ok(!fs.existsSync(path.join(fresh.dir, "late.txt")), "a fresh disposable checkout (what a check gets) must not see the later live-worktree write either");
    } finally {
      fresh.dispose();
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
