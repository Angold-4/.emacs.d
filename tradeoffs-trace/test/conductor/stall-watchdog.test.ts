// Plan 3b stall watchdog: an agent that is mid-turn, runs no command of its
// own and produces no event is steered once, then its attempt ends as timed
// out — long before workerAttemptMs. Observed before the watchdog: runs sat
// silent until a 10- or 45-minute deadline.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";

test("stall-watchdog: a silent worker is nudged once, then its attempt times out early", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-stall-");
  const steerLog = path.join(dir, "steers.log");
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] }),
    extraWorkerEnv: { FAKE_PI_STEER_LOG: steerLog },
    deadlines: { stallMs: 400, workerAttemptMs: 60_000, abortGraceMs: 500, termGraceMs: 500 },
  });
  const started = Date.now();
  await setup.conductor.start();
  try {
    await waitFor(
      () =>
        readEvents(setup.runDir).some(
          (e) => e.kind === "event" && (e.event as { type?: string }).type === "ATTEMPT_TIMED_OUT",
        ),
      20_000,
    );
    assert.ok(Date.now() - started < 20_000, "ended by the watchdog, not the 60 s attempt deadline");
    const kinds = readEvents(setup.runDir).map((e) => e.kind);
    const nudge = kinds.indexOf("stall_nudge");
    const stalled = kinds.indexOf("stalled");
    assert.ok(nudge >= 0 && stalled > nudge, `expected stall_nudge then stalled, got ${kinds.join(",")}`);
    assert.match(fs.readFileSync(steerLog, "utf8"), /no progress/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("stall-watchdog: a worker whose own sh command is running is not a stall", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-sh", command: "sleep 2" }, { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
    deadlines: { stallMs: 400, workerAttemptMs: 60_000, abortGraceMs: 500, termGraceMs: 500 },
  });
  await setup.conductor.start();
  try {
    await waitFor(
      () =>
        readEvents(setup.runDir).some((e) => e.kind === "event" && (e.event as { type?: string }).type === "SUBMIT_PHASE"),
      20_000,
    );
    const kinds = readEvents(setup.runDir).map((e) => e.kind);
    assert.ok(!kinds.includes("stall_nudge"), "a running sh command must not count as silence");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
