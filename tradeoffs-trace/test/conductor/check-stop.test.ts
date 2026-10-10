// ODP-3 (carried from 06c, owner-confirmed 2026-10-07): "on a stop during
// run_checks, conductor.ts returns before its kill loop, so a running check's
// process can be orphaned. In 06e, a stop signals the run's own check process
// group too, like every other stage's, with a fake-process test."
//
// The check command here is a REAL child process — a shell that, once a marker
// file exists, writes its own process-group id and then sleeps past the stop.
// The marker is written by the worker's own `sh` step, so the check is quick
// while the base baseline runs it and long-running when CHECKING runs it: the
// orphan this test is about is the CHECKING one. The test watches the real
// process table, and after `stop()` the check's group must be gone.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { processAlive } from "../../src/effects/sweep.ts";
import { defaultWorkerHello, cleanupDir, setupConductor, waitFor } from "./harness.ts";

function pgidAlive(pgid: number): boolean {
  try {
    process.kill(-pgid, 0);
    return true;
  } catch {
    return false;
  }
}

function leaderCommand(pgid: number): string {
  try {
    return execFileSync("ps", ["-o", "command=", "-p", String(pgid)], { encoding: "utf8" }).trim();
  } catch {
    return "";
  }
}

test("plan 06g/ODP-3: a stop during the checks signals the run's own check process group", async () => {
  const markerDir = fs.mkdtempSync("/tmp/tt-check-stop-");
  const pgidFile = path.join(markerDir, "pgid");
  const startedFile = path.join(markerDir, "started");
  const armed = path.join(markerDir, "armed");
  // While `armed` is absent this check exits at once (the base baseline's
  // run); once the worker has created it, the check records its own process
  // group and sleeps well past the stop. `$$` is the shell's pid, which
  // runCommand makes the group leader, so `$$` IS the pgid.
  const check = `sh -c '[ -f ${armed} ] && { echo $$ > ${pgidFile}; touch ${startedFile}; sleep 120; }; exit 0'`;

  const setup = await setupConductor({
    // The check runs on its own, after the freeze: the worker has no other
    // job than arming it and submitting.
    checks: [check],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `touch ${armed}` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 20_000, checkMs: 120_000 },
  });

  await setup.conductor.start();
  let pgid: number | undefined;
  try {
    await waitFor(() => fs.existsSync(startedFile), 90_000, 20, setup.runDir);
    pgid = Number.parseInt(fs.readFileSync(pgidFile, "utf8").trim(), 10);
    assert.ok(Number.isFinite(pgid) && pgid > 0, "the check recorded its process group");
    assert.equal(pgidAlive(pgid), true, "the check is running before the stop");
    assert.match(leaderCommand(pgid), /sleep 120/, "the recorded group is the check's own");
    // It is genuinely long-running: it is still there a moment later, so the
    // assertion after `stop()` is about the stop, not about a command that
    // happened to finish.
    await new Promise((r) => setTimeout(r, 1500));
    assert.equal(pgidAlive(pgid), true, "the check is still running a second later");

    // A stop must signal the check's own group, like every other stage's.
    await setup.conductor.stop();
    // A short, bounded wait — not `waitFor`, whose load-tolerant floor is
    // 150 s and would let the orphan's own `sleep` end the test for us.
    const deadline = Date.now() + 5_000;
    while (pgidAlive(pgid) && Date.now() < deadline) await new Promise((r) => setTimeout(r, 50));
    assert.equal(pgidAlive(pgid!), false, "the check's process group is gone after the stop");
    assert.equal(processAlive(pgid!), false, "and so is its leader");
  } finally {
    // Best effort: never leave the sleeper behind for the next test.
    if (pgid !== undefined && pgidAlive(pgid)) {
      try {
        process.kill(-pgid, "SIGKILL");
      } catch {
        // already gone
      }
    }
    await setup.conductor.stop().catch(() => undefined);
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(markerDir, { recursive: true, force: true });
  }
});
