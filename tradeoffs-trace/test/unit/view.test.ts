// Plan 07e (E3/E4): status names the current bound and its used/limit pair,
// and a check-window pause is shown as a pause by a named run.

import assert from "node:assert/strict";
import { test } from "node:test";

import { renderStatusText } from "../../src/render.ts";
import { pipelineLine, stageBoundText } from "../../src/view.ts";

test("plan 07e: a bound reads as `check 2m/5m`", () => {
  assert.equal(stageBoundText("checks", 120_000, 300_000), "check 2m/5m");
  assert.equal(stageBoundText("implement", 45 * 60_000, 45 * 60_000), "worker-attempt 45m/45m");
  assert.equal(stageBoundText("mystery", 1_000, 2_000), undefined);
});

test("plan 07e: the pipeline line names the current stage's bound", () => {
  const line = pipelineLine(
    [{ stage: "checks", startedAt: new Date().toISOString(), ms: 120_000, failed: false, current: true }],
    { checks: 300_000 },
  );
  assert.match(line, /\[check 2m\/5m\]/);
});

test("plan 07e: status prints the bound and the pause cause naming the other run", () => {
  const view = {
    pausedBy: "run-a",
    bounds: "check 2m/5m",
    pipeline: "checks 2m00s… (3m00s left) [check 2m/5m]",
    gates: "no candidate yet",
    reviewLine: "none",
  };
  const text = renderStatusText(
    "/tmp/tt-07e-view",
    { run: "live", phase: { phaseId: "p1", phase: "IMPLEMENTING" } },
    view as never,
  );
  assert.match(text, /^bounds: check 2m\/5m$/m);
  assert.match(text, /^paused by run run-a's check$/m);
});
