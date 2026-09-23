// The "force-kill-shell" exit-gate test (design §2.2, §6.1, §9.3): while a
// worker `sh` command is running, SIGKILL the worker's own Pi process group
// out from under the conductor (not the `sh` command's own group — a real
// crash/OOM-kill/force-stop hits the agent process, not necessarily its
// children first). Asserts:
//   - the conductor notices (the attempt ends, not left hanging);
//   - the recorded shell group is gone;
//   - the sweep finds nothing left under the worktree;
//   - the attempt is recorded as interrupted (§9.3's "agent attempt"
//     reconciliation: ATTEMPT_INTERRUPTED, still IMPLEMENTING — budget
//     remains, so §6.1's "new attempt on the same session" fires);
//   - a previously frozen candidate checkout elsewhere is unaffected.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";
import { freezeCommit, materializeCandidate, verifyIntegrity } from "../../src/effects/git.ts";

function groupAlive(pgid: number): boolean {
  try {
    process.kill(-pgid, 0);
    return true;
  } catch {
    return false;
  }
}

test("force-kill-shell: SIGKILLing the worker's process group mid-`sh` is noticed, cleaned up, and does not touch a previously frozen candidate", async () => {
  const markerDir = fs.mkdtempSync("/tmp/tt-marker-");
  const marker = path.join(markerDir, "marker");

  const setup = await setupConductor({
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: `touch ${marker} && sleep 300` }],
    }),
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 5_000, workerAttemptMs: 60_000 },
  });

  // Simulate a candidate frozen earlier in the run (e.g. by an attempt this
  // one is repairing) — a real commit on top of the repo's base, but never
  // touched by this test's worker attempt at all.
  const p = runPaths(setup.runDir);
  fs.writeFileSync(path.join(setup.repo.dir, "prior.txt"), "prior candidate content\n");
  execFileSync("git", ["-C", setup.repo.dir, "add", "-A"]);
  const priorSha = freezeCommit(setup.repo.dir, "prior-action", "a prior candidate");
  // freezeCommit leaves the repo's checkout at `priorSha`; reset it back to
  // the base commit so the run's own worktree (created next, by
  // conductor.start()) starts from the same head the test set up.
  execFileSync("git", ["-C", setup.repo.dir, "reset", "--hard", setup.repo.head]);
  const priorCandidateDir = path.join(p.candidates, priorSha);
  materializeCandidate(setup.repo.dir, priorSha, priorCandidateDir);
  assert.ok(verifyIntegrity(setup.repo.dir, priorCandidateDir, priorSha), "sanity: the prior candidate checkout starts intact");

  await setup.conductor.start();
  try {
    // Wait for the sh command to actually run (past the SIGSTOP handshake —
    // see shell.ts): the marker file only appears after SIGCONT.
    await waitFor(() => fs.existsSync(marker), 15_000);

    const before = setup.conductor.agentPgids.find((a) => a.role === "worker");
    assert.ok(before, "expected a live worker agent");
    const workerPgid = before!.pgid;
    assert.ok(groupAlive(workerPgid), "sanity: the worker's process group is alive before the kill");

    // The recorded shell intent (design §2.2: pgid logged before the
    // command's first side effect).
    const events = readEvents(setup.runDir);
    const shIntent = events.find((e) => e.kind === "intent" && (e.event as { pgid?: number }).pgid !== undefined);
    assert.ok(shIntent, "expected a recorded sh intent with a pgid");
    const shPgid = (shIntent!.event as { pgid: number }).pgid;
    assert.ok(groupAlive(shPgid), "sanity: the sh command's own group is alive before the kill");

    // SIGKILL the WORKER's own process group — not the sh command's group —
    // simulating the agent process itself being force-killed (a crash,
    // OOM-kill, or an operator's force-stop) while a command is running.
    process.kill(-workerPgid, "SIGKILL");

    // The conductor notices: the attempt ends (ATTEMPT_INTERRUPTED, still
    // IMPLEMENTING per §9.3/§6.1 — budget remains, so a new attempt on the
    // same session is dispatched) rather than hanging forever.
    await waitFor(() => {
      const evs = readEvents(setup.runDir);
      return evs.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "ATTEMPT_INTERRUPTED");
    }, 15_000);
    assert.equal(setup.conductor.state.phase.phase, "IMPLEMENTING", "budget remains — a new attempt is dispatched");
    assert.equal(setup.conductor.state.phase.attempt.interrupted, true);

    // The recorded shell group (the `sh` command's own, orphaned when its
    // parent agent was force-killed) is gone — the conductor's own
    // §2.2 reconciliation (killGroup over the recorded shGroups) got it,
    // not just the direct SIGKILL to the worker's own group.
    await waitFor(() => !groupAlive(shPgid), 5_000);
    assert.ok(!groupAlive(workerPgid), "the worker's own group is gone");

    // The sweep (run as part of that same reconciliation) finds nothing
    // left under the worktree.
    const sweepRecord = readEvents(setup.runDir).find((e) => e.kind === "sweep");
    assert.ok(sweepRecord, "expected a sweep record");
    const sweepResult = sweepRecord!.event as { killed: unknown[]; tainted: boolean };
    assert.deepEqual(sweepResult.killed, [], "the sweep should find nothing still running under the worktree");

    // A previously frozen candidate checkout elsewhere is completely
    // unaffected by this attempt's force-kill and cleanup.
    assert.ok(
      verifyIntegrity(setup.repo.dir, priorCandidateDir, priorSha),
      "the previously frozen candidate checkout must be unchanged",
    );
  } finally {
    await setup.conductor.stop();
    // The interrupted attempt is re-dispatched and runs the same `sleep 300`
    // again; stop() must kill that command too, or it outlives the test (and
    // keeps `node --test` from exiting for five minutes).
    await waitFor(() => !processMentions(marker), 5_000).catch(() => undefined);
    const leaked = processMentions(marker);
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(markerDir);
    assert.equal(leaked, false, "stop() left an agent's sh command running");
  }
});

function processMentions(text: string): boolean {
  try {
    execFileSync("pgrep", ["-f", text], { stdio: "ignore" });
    return true;
  } catch {
    return false;
  }
}
