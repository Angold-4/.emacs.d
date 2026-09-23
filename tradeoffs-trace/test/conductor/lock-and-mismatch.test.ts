import assert from "node:assert/strict";
import { test } from "node:test";

import { Conductor } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { cleanupDir, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";

test("second conductor on the same run fails fast", async () => {
  const setup = await setupConductor({
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "hang-forever" }] }),
    deadlines: { abortGraceMs: 200, termGraceMs: 200 },
  });
  await setup.conductor.start();
  try {
    const second = new Conductor({ runDir: setup.runDir, plan: setup.plan, piCommand: process.execPath });
    await assert.rejects(() => second.start(), /could not acquire lock/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("tool-set mismatch refuses to prompt and goes straight to BLOCKED (design §2.1: a launch failure, not a warning)", async () => {
  const setup = await setupConductor({
    // A worker reporting the *reviewer* tool set is a mismatch: missing
    // edit/write/sh/submit_phase, extra submit_discovery/submit_review.
    workerScript: () => ({ hello: { role: "worker", tools: ROLE_TOOLS.reviewer }, steps: [{ kind: "hang-forever" }] }),
    deadlines: { helloTimeoutMs: 10_000, workerAttemptMs: 15_000, abortGraceMs: 500, termGraceMs: 500 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => {
      const events = readEvents(setup.runDir);
      return events.some((e) => e.kind === "launch_failure");
    }, 20_000);
    const events = readEvents(setup.runDir);
    const failure = events.find((e) => e.kind === "launch_failure");
    assert.ok(failure, "expected a launch_failure record");
    // design §2.1/round-of-review item 4: a tool-set mismatch is a launch
    // failure, not a warning — the phase goes straight to BLOCKED (no
    // repair round spent retrying it), and the daemon auto-stops there.
    await waitFor(() => setup.conductor.state.phase.phase === "BLOCKED", 5_000);
    assert.equal(setup.conductor.state.phase.phase, "BLOCKED");
    assert.match(setup.conductor.state.phase.blockedReason ?? "", /launch failure: tool set mismatch/);
    assert.match(setup.conductor.state.phase.blockedReason ?? "", /missing/);
    assert.match(setup.conductor.state.phase.blockedReason ?? "", /extra/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
