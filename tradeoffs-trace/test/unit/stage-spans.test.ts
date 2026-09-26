// Program 14: after a stop and resume, 14g's pipeline read "implement 1h07m…
// (over by 22m00s)". It counted the whole stopped period, although the
// re-dispatched worker had a fresh attempt deadline.

import assert from "node:assert/strict";
import { test } from "node:test";

import { pipelineLine, stageSpans } from "../../src/view.ts";
import type { Timeline } from "../../src/conductor.ts";

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
