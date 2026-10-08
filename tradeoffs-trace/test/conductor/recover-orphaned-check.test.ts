// A conductor that stops while a candidate check is running leaves the
// check's process group behind. The next conductor's recovery kills that
// recorded group (`check-sh-<actionId>-<pgid>`) before it re-runs the check.
// An earlier `run_checks` recovery branch returned before this kill loop, so
// the orphan survived (06c reviewer B, carried to 06e).
import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultWorkerHello, FAKE_PI_PATH, readEvents, setupConductor, waitFor } from "./harness.ts";
import { Conductor } from "../../src/conductor.ts";

function groupAlive(pgid: number): boolean {
  try {
    process.kill(-pgid, 0);
    return true;
  } catch {
    return false;
  }
}

test("recovery: a check left running by a stopped conductor is killed before the check re-runs", async () => {
  const markerDir = fs.mkdtempSync("/tmp/tt-orphan-check-");
  const started = path.join(markerDir, "started");
  // The base tree has no hang.txt, so the baseline returns at once; the
  // candidate adds it, so its check sleeps until something kills it.
  const check = `if [ -f hang.txt ]; then touch ${started} && sleep 300; fi`;
  const setup = await setupConductor({
    checks: [check],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "echo x > hang.txt" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 5_000, workerAttemptMs: 60_000 },
  });
  let pgid: number | undefined;
  await setup.conductor.start();
  try {
    await waitFor(() => fs.existsSync(started), 90_000, undefined, setup.runDir);
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "intent" && String(r.actionId).startsWith("check-sh-")), 15_000);
    const intent = readEvents(setup.runDir).find((r) => r.kind === "intent" && String(r.actionId).startsWith("check-sh-"))!;
    pgid = (intent.event as { pgid: number }).pgid;
    assert.ok(groupAlive(pgid), "sanity: the candidate check's group is running");
  } finally {
    await setup.conductor.stop();
  }

  const before = readEvents(setup.runDir).length;
  const restarted = new Conductor({
    runDir: setup.runDir,
    plan: setup.plan,
    piCommand: process.execPath,
    piArgsPrefix: [FAKE_PI_PATH],
    stubReviews: true,
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 5_000, workerAttemptMs: 60_000 },
  });
  await restarted.start();
  try {
    await waitFor(
      () => readEvents(setup.runDir).slice(before).some((r) => r.kind === "event" && (r.event as { type?: string }).type === "CHECKS_INTERRUPTED"),
      30_000,
      undefined,
      setup.runDir,
    );
    await waitFor(() => !groupAlive(pgid!), 10_000);
    assert.equal(groupAlive(pgid!), false, "recovery must kill the orphaned check group");
  } finally {
    await restarted.stop();
    if (pgid !== undefined && groupAlive(pgid)) process.kill(-pgid, "SIGKILL");
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(markerDir, { recursive: true, force: true });
  }
});
