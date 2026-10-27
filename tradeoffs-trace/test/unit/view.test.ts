// Plan 07e (E3/E4): status names the current bound and its used/limit pair,
// and a check-window pause is shown as a pause by a named run.

import assert from "node:assert/strict";
import { test } from "node:test";

import { renderStatusText } from "../../src/render.ts";
import { pipelineLine, stageBoundText, stageLimits, stageSpans } from "../../src/view.ts";
import type { Timeline } from "../../src/conductor.ts";

test("plan 07e: the final check is its own running unit with its own bound", () => {
  // A4: FINAL_CHECKING must not fall through to a lowercase stage name with
  // no limit, and must not borrow the ordinary check's limit: it has its own
  // `#+TT_FINAL_CHECK_MINUTES` bound. The pipeline shows it as a stage with
  // the used/limit pair and the bound's own kind.
  const timeline = {
    state: {} as Timeline["state"],
    phases: [
      { phase: "CHECKING", at: "2026-10-27T10:00:00.000Z" },
      { phase: "FINAL_CHECKING", at: "2026-10-27T10:05:00.000Z" },
    ],
    rounds: [],
  } as Timeline;
  const spans = stageSpans(timeline, new Date("2026-10-27T10:06:00.000Z"));
  assert.deepEqual(
    spans.map((s) => s.stage),
    ["checks", "final-check"],
    "the final check is named as its own stage, not folded into checks",
  );
  const limits = stageLimits({ deadlines: { finalCheckMs: 7 * 60_000 } });
  assert.equal(limits["final-check"], 7 * 60_000, "the plan's final-check bound reaches the stage limit");
  assert.equal(limits.checks, 5 * 60_000, "the ordinary check keeps its own limit");
  assert.equal(stageBoundText("final-check", 2 * 60_000, limits["final-check"]), "final-check 2m/7m");
  const line = pipelineLine([spans[1]], limits);
  assert.match(line, /final-check 1m00s… \(6m00s left\) \[final-check 1m\/7m\]/);
});

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

test("tt status shows elapsed over limit for a running check", () => {
  // A check still running past its plan limit: the status line names the
  // elapsed time, says the limit is reached (never a cumulative overrun), and
  // still carries the `<kind> <elapsed>/<limit>` bound pair.
  const line = pipelineLine(
    [{ stage: "checks", startedAt: new Date().toISOString(), ms: 6 * 60_000, failed: false, current: true }],
    { checks: 5 * 60_000 },
  );
  assert.match(line, /checks 6m00s… \(limit reached\) \[check 6m\/5m\]/);
  const text = renderStatusText(
    "/tmp/tt-07e-over",
    { run: "live", phase: { phaseId: "p1", phase: "CHECKING" } },
    { bounds: "check 6m/5m", pipeline: line, gates: "no candidate yet", reviewLine: "none" } as never,
  );
  assert.match(text, /^bounds: check 6m\/5m$/m);
  assert.match(text, /^pipeline: checks 6m00s… \(limit reached\) \[check 6m\/5m\]$/m);
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
