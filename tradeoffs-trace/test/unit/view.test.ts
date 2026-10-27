// Plan 07e (E3/E4): status names the current bound and its used/limit pair,
// and a check-window pause is shown as a pause by a named run.

import assert from "node:assert/strict";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import * as path from "node:path";
import { test } from "node:test";

import type { RunPlanFile } from "../../src/conductor.ts";
import { renderStatusText } from "../../src/render.ts";
import { buildView, pipelineLine, stageBoundText, stageLimits, stageSpans } from "../../src/view.ts";
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

test("plan 07e (E3/R9): an ordinary check derives its elapsed and limit from the timeline and the plan", () => {
  // The status pair is never a literal: the elapsed time comes from the phase
  // timeline through `stageSpans`, the limit from the plan's
  // `#+TT_CHECK_MINUTES` through `stageLimits`, and `pipelineLine` renders
  // both. Inside the bound the line counts down; past it, it says so.
  const startedAt = "2026-10-27T10:00:00.000Z";
  const timeline = {
    state: {} as Timeline["state"],
    phases: [{ phase: "CHECKING", at: startedAt }],
    rounds: [],
  } as unknown as Timeline;
  const plan = { deadlines: { checkMs: 5 * 60_000 } } as Pick<RunPlanFile, "deadlines">;
  const limits = stageLimits(plan);
  assert.equal(limits.checks, 5 * 60_000, "the plan's check keyword is the ordinary check's limit");

  const running = stageSpans(timeline, new Date("2026-10-27T10:02:00.000Z"));
  assert.equal(running.length, 1, "the running ordinary check is the one stage");
  assert.equal(running[0]!.stage, "checks");
  assert.equal(running[0]!.ms, 2 * 60_000, "the elapsed time is the check stage's own");
  assert.equal(stageBoundText("checks", running[0]!.ms, limits.checks), "check 2m/5m");
  assert.match(pipelineLine(running, limits), /checks 2m00s… \(3m00s left\) \[check 2m\/5m\]/);

  const over = stageSpans(timeline, new Date("2026-10-27T10:06:30.000Z"));
  assert.equal(over[0]!.ms, 6.5 * 60_000, "the elapsed time keeps counting past the limit");
  const line = pipelineLine(over, limits);
  assert.match(line, /checks 6m30s… \(limit reached\) \[check 6m\/5m\]/, "elapsed over limit is named, never a bare overrun");
  const text = renderStatusText(
    "/tmp/tt-07e-ordinary-check",
    { run: "live", phase: { phaseId: "p1", phase: "CHECKING" } },
    { bounds: stageBoundText("checks", over[0]!.ms, limits.checks), pipeline: line, gates: "no candidate yet", reviewLine: "none" } as never,
  );
  assert.match(text, /^bounds: check 6m\/5m$/m);
  assert.match(text, /^pipeline: checks 6m30s… \(limit reached\) \[check 6m\/5m\]$/m);
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

test("plan 07e: a check-window pause is shown only while the conductor is alive", () => {
  // Decision disc-B: an unmatched `worker_paused` record survives watcher
  // cleanup, so the pause may outrank `idle` but never a stopped conductor —
  // a dead run must still be reported as stopped.
  const plan = {
    title: "pause test",
    repo: "/tmp/tt-07e-pause",
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [] }],
  } as unknown as RunPlanFile;
  const dir = mkdtempSync(path.join(tmpdir(), "tt-07e-pause-"));
  try {
    const events = [
      { seq: 1, ts: "2026-10-27T10:00:00.000Z", kind: "init", event: { runId: "r-pause", integrationHead: "H0" } },
      { seq: 2, ts: "2026-10-27T10:00:00.000Z", kind: "event", event: { type: "ATTEMPT_STARTED" } },
      // No matching `worker_resumed`: the pause record outlives the watcher.
      { seq: 3, ts: "2026-10-27T10:00:30.000Z", kind: "worker_paused", event: { agentId: "w-a", by: "run-b" } },
    ];
    writeFileSync(path.join(dir, "events.jsonl"), `${events.map((e) => JSON.stringify(e)).join("\n")}\n`);
    const now = new Date("2026-10-27T10:01:00.000Z");
    assert.equal(buildView(dir, plan, true, now).attention, "paused by run run-b's check", "a live run shows the pause cause");
    assert.equal(buildView(dir, plan, false, now).attention, "conductor stopped", "a dead conductor is never shown as paused");
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 07e: the final check inherits the plan's check bound when it has no keyword of its own", () => {
  // A4: the conductor makes `finalCheckMs` follow `checkMs`; the status must
  // not show the bare default to a plan that raised `TT_CHECK_MINUTES`.
  const limits = stageLimits({ deadlines: { checkMs: 30 * 60_000 } });
  assert.equal(limits.checks, 30 * 60_000);
  assert.equal(limits["final-check"], 30 * 60_000, "the final check follows the ordinary check");
  const own = stageLimits({ deadlines: { checkMs: 30 * 60_000, finalCheckMs: 7 * 60_000 } });
  assert.equal(own["final-check"], 7 * 60_000, "its own keyword still wins");
  const bare = stageLimits({});
  assert.equal(bare["final-check"], bare.checks, "with no keyword both keep today's 5m");
});
