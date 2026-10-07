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
  // The stage clock stops at the stop and restarts at the resume: the resumed
  // stage's span counts from its own restart, never the stopped interval. The
  // restart instants come from the production `restartInstants` scan of the
  // log records (a `stop` record then the first event after it, or a `resume`
  // record), so this is not a hand-built `restarts` list.
  const records = [
    { kind: "init", ts: "2026-10-07T15:59:00.000Z", event: {} },
    { kind: "event", ts: "2026-10-07T16:00:00.000Z", event: { type: "CHECKS_PASSED" } },
    // A clean stop at 16:05...
    { kind: "stop", ts: "2026-10-07T16:05:00.000Z", event: { reason: "conductor stopped" } },
    // ...and the conductor starts again at 17:30, writing a `resume` record.
    { kind: "resume", ts: "2026-10-07T17:30:00.000Z", event: {} },
    { kind: "event", ts: "2026-10-07T17:30:00.100Z", event: { type: "CHECKS_PASSED" } },
  ] as LogRecord[];
  const restarts = restartInstants(records);
  assert.deepEqual(restarts, ["2026-10-07T17:30:00.000Z"], "the resume starts the new segment");
  const timeline = {
    state: {} as Timeline["state"],
    phases: [{ phase: "CHECKING", at: "2026-10-07T16:00:00.000Z" }],
    rounds: [],
    restarts,
  } as Timeline;
  const now = new Date("2026-10-07T17:32:00.000Z");
  const spans = stageSpans(timeline, now);
  assert.equal(spans.length, 1);
  assert.equal(spans[0].stage, "checks");
  assert.equal(spans[0].ms, 2 * 60_000, "two minutes since the resume, not 92 since the stage began");
  assert.equal(spans[0].startedAt, "2026-10-07T17:30:00.000Z", "the resumed stage shows its own start");

  // A stop without a resume record (a crash/exit) also starts the next
  // segment at the first event after the stop.
  const crashed = [
    { kind: "event", ts: "2026-10-07T16:00:00.000Z", event: { type: "CHECKS_PASSED" } },
    { kind: "stop", ts: "2026-10-07T16:05:00.000Z", event: {} },
    { kind: "event", ts: "2026-10-07T17:30:00.000Z", event: { type: "CHECKS_PASSED" } },
  ] as LogRecord[];
  assert.deepEqual(restartInstants(crashed), ["2026-10-07T17:30:00.000Z"]);
});
