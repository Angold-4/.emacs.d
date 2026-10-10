// Plan 06k2 (A4): a worktree create/remove that throws while starting a
// worker attempt is recorded as that attempt's failure, never left as an
// in-flight dispatch. A worker that cannot start fails the attempt and the
// next attempt begins.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { runPaths } from "../../src/conductor.ts";
import { materializeCandidate } from "../../src/effects/git.ts";
import { EventLog } from "../../src/effects/log.ts";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 8_000,
  reviewMs: 20_000,
  panelMs: 8_000,
};

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

test("plan 06k2: a worktree create that throws records the attempt as failed", async () => {
  const setup = await setupConductor({
    // A large round budget, so the assertion is about the failure and the
    // next dispatch, not the budget gate.
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'work\n' > work.txt" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  // Seed the run at a tainted candidate whose repair attempt must reset its
  // worktree before starting.
  const log = new EventLog(runPaths(setup.runDir).events, []);
  log.append("event", { type: "ATTEMPT_STARTED", baselineNeeded: false });
  log.append("event", { type: "SUBMIT_PHASE", disclosures: [], prior: [] });
  log.append("event", { type: "FREEZE_COMPLETED", candidateSha: setup.repo.head, decisions: [], tainted: true });
  log.append("event", { type: "CHECKS_FAILED" });
  log.append("event", { type: "REPAIR_ATTEMPT_STARTED" });
  log.close();
  const candidateDir = path.join(runPaths(setup.runDir).candidates, setup.repo.head);
  if (!fs.existsSync(candidateDir)) materializeCandidate(setup.repo.dir, setup.repo.head, candidateDir);
  const worktree = runPaths(setup.runDir).worktree;
  // A real worktree at the candidate, then made unremovable: both
  // `git worktree remove` and the rmSync fallback fail.
  execFileSync("git", ["-C", setup.repo.dir, "worktree", "add", "-q", worktree, setup.repo.head]);
  fs.writeFileSync(path.join(worktree, "blocker.txt"), "x");
  fs.chmodSync(worktree, 0o500);
  try {
    await setup.conductor.start();
    // The reset fails synchronously, so the whole repair budget can be spent
    // in one turn: the assertions below only need the failure and the next
    // dispatch, never a later success.
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "worktree_reset_failed"), 120_000, 50, setup.runDir);
    const events = readEvents(setup.runDir);
    assert.ok(events.some((r) => r.kind === "worktree_reset_failed"), "the worktree failure is recorded");
    assert.ok(
      events.some((r) => r.kind === "event" && (r.event as { type?: string }).type === "ATTEMPT_NO_SUBMISSION"),
      "the attempt ends as failed, never left in flight",
    );
    // The next attempt starts: at least two dispatch_worker ACTION_STARTED
    // records (the failed one and the one after it).
    const dispatches = events.filter(
      (r) => r.kind === "event" && (r.event as { type?: string }).type === "ACTION_STARTED" && (r.event as { action?: string }).action === "dispatch_worker",
    );
    assert.ok(dispatches.length >= 2, `the next attempt starts (saw ${dispatches.length} dispatches)`);
    // No dispatch stays in flight: the failure cleared the dispatch slot.
    assert.equal(setup.conductor.state.phase.inFlight.dispatch_worker, undefined, "no dispatch is left in flight");
  } finally {
    try {
      fs.chmodSync(worktree, 0o700);
    } catch {
      // best effort
    }
    await teardown(setup);
  }
});

test("plan 06k2: a lane worktree create that throws is recorded as that lane's failure", async () => {
  const setup = await setupConductor({
    phase: { id: "p1", goal: "build two candidates", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [], workers: 2, rounds: 10 },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'lane\n' > lane.txt" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    laneWorkerScriptFor: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'lane\n' > lane.txt" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    laneReviewerScriptFor: (seat, _candidate, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: {
            reviewer: seat,
            phaseId: "p1",
            candidateSha: "$TT_CANDIDATE_SHA",
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
            ballots: [],
            findings: [],
          },
        },
      ],
    }),
  });
  // Lane a's worktree path is made unremovable before the round starts: the
  // lane host's remove+create throws and must be recorded as that lane's
  // failure (finding M-1), not left to the round's generic catch.
  const laneA = path.join(setup.runDir, "worktrees", "lane-a");
  fs.mkdirSync(laneA, { recursive: true });
  fs.writeFileSync(path.join(laneA, "blocker.txt"), "x");
  fs.chmodSync(laneA, 0o500);
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "lane_worktree_failed"), 120_000, 50, setup.runDir);
    const failed = readEvents(setup.runDir).find((r) => r.kind === "lane_worktree_failed")!;
    assert.equal((failed.event as { lane?: string }).lane, "a");
    assert.match((failed.event as { error?: string }).error ?? "", /./, "the failure names its error");
    await waitFor(
      () => (setup.conductor.state.phase.rounds?.[0]?.candidates.find((c) => c.lane === "a")?.note ?? "").includes("worktree could not be created"),
      60_000,
      20,
      setup.runDir,
    );
  } finally {
    try {
      fs.chmodSync(laneA, 0o700);
    } catch {
      // best effort
    }
    await teardown(setup);
  }
});
