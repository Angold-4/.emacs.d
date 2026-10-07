// Program 14: after a stop and resume, 14g's pipeline read "implement 1h07m…
// (over by 22m00s)". It counted the whole stopped period, although the
// re-dispatched worker had a fresh attempt deadline.

import assert from "node:assert/strict";
import { test } from "node:test";

import { pipelineLine, stageSpans } from "../../src/view.ts";
import { restartInstants, type Timeline } from "../../src/conductor.ts";
import type { LogRecord } from "../../src/effects/log.ts";

test("stage spans: the current stage counts from the last attempt restart, not from before a stop", () => {
  const timeline = {
    state: {} as Timeline["state"],
    phases: [{ phase: "IMPLEMENTING", at: "2026-09-26T10:41:00.000Z" }],
    rounds: [],
    restarts: ["2026-09-26T11:48:24.000Z"],
  } as Timeline;
  const now = new Date("2026-09-26T11:50:24.000Z");
  const spans = stageSpans(timeline, now);
  assert.equal(spans.length, 1);
  assert.equal(spans[0].ms, 120_000, "two minutes since the restart, not 69 since the stage began");
  assert.match(pipelineLine(spans, { implement: 45 * 60_000 }), /implement 2m00s… \(43m00s left\)/);

  // A restart before the current stage began is ignored.
  const earlier = { ...timeline, restarts: ["2026-09-26T10:00:00.000Z"] } as Timeline;
  assert.equal(stageSpans(earlier, now)[0].ms, 69 * 60_000 + 24_000);
});

test("plan 06c: status after stop and resume shows the resumed stage's own time", () => {
  // The exact example from the steer: CHECKING at 16:00, a stop at 16:05, a
  // resume at 17:30, PROBING at 17:31. The completed checks span is the sum of
  // its running segments (6 minutes), never the 91-minute wall interval; the
  // current stage counts from its own resume.
  const records = [
    { kind: "event", ts: "2026-10-07T16:00:00.000Z", event: { type: "CHECKS_PASSED" } },
    { kind: "stop", ts: "2026-10-07T16:05:00.000Z", event: { reason: "conductor stopped" } },
    { kind: "event", ts: "2026-10-07T17:30:00.000Z", event: { type: "RUN_RESUMED" } },
  ] as LogRecord[];
  assert.deepEqual(restartInstants(records), ["2026-10-07T17:30:00.000Z"], "the resume starts the new segment");
  const timeline = {
    state: {} as Timeline["state"],
    phases: [
      { phase: "CHECKING", at: "2026-10-07T16:00:00.000Z" },
      { phase: "PROBING", at: "2026-10-07T17:31:00.000Z" },
    ],
    rounds: [],
    restarts: ["2026-10-07T17:30:00.000Z"],
    stops: ["2026-10-07T16:05:00.000Z"],
  } as Timeline;
  const now = new Date("2026-10-07T17:32:00.000Z");
  const spans = stageSpans(timeline, now);
  assert.equal(spans.length, 2);
  assert.equal(spans[0].stage, "checks");
  assert.equal(spans[0].ms, 6 * 60_000, "checks = 6 minutes of running segments, not 91");
  assert.equal(spans[1].stage, "probe");
  assert.equal(spans[1].ms, 60_000, "the current stage counts from its own start");

  // The same stage still running after the resume counts from the resume.
  const running = { ...timeline, phases: [{ phase: "CHECKING", at: "2026-10-07T16:00:00.000Z" }] } as Timeline;
  const runningSpans = stageSpans(running, now);
  assert.equal(runningSpans[0].ms, 2 * 60_000, "two minutes since the resume, not 92 since the stage began");
  assert.equal(runningSpans[0].startedAt, "2026-10-07T17:30:00.000Z", "the resumed stage shows its own start");
});
