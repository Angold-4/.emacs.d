import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { initialState, rebuildState, rebuildTimeline, rebuildTimelineWithEvents, type RunPlanFile } from "../../src/conductor.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { Event, State } from "../../src/core/types.ts";

const current = "0464fa82b9a08e8ca6ed1a1f09d33fe6f4f6aa09";
const legacy = "be6b76296741ff5132cc15ae9cffb31273ac4449";
function fixture(runnerRevision: string) {
  const runDir = fs.mkdtempSync("/tmp/tt-rejected-replay-");
  const plan: RunPlanFile = { title: "replay", repo: runDir, integrationBranch: "main", checks: [], phases: [
    { id: "p1", goal: "parse", acceptance: [], checks: [], boundaries: [], reserved: [] },
  ] };
  const records: Array<{ seq: number; ts: string; kind: string; event: unknown }> = [];
  const record = (kind: string, event: unknown) => records.push({ seq: records.length + 1, ts: `2026-11-01T00:00:0${records.length}.000Z`, kind, event });
  record("init", { runId: "r1", integrationHead: "H0", runnerRevision });
  let live = initialState("r1", plan.phases[0], "H0", [], undefined, plan);
  return { runDir, plan, records, record,
    get live() { return live; },
    apply(event: Event) { const result = reduce(live, event); assert.ok(result.ok); live = result.state; record("event", event); },
    reject(event: Event) { const result = reduce(live, event); assert.equal(result.ok, false); record("rejected", { event, reason: result.reason }); },
    verify(expected: State = live, projection: "all" | "recovery" | "timeline" = "all") {
      fs.writeFileSync(path.join(runDir, "events.jsonl"), records.map((r) => JSON.stringify(r)).join("\n") + "\n");
      if (projection !== "timeline") {
        assert.deepEqual(rebuildState(runDir, plan), expected, "strict recovery equals live");
        assert.deepEqual(rebuildState(runDir, plan, { lenient: true }), expected, "lenient recovery equals live");
      }
      if (projection !== "recovery") {
        assert.deepEqual(rebuildTimeline(runDir, plan).state, expected, "timeline equals live");
        assert.deepEqual(rebuildTimelineWithEvents(runDir, plan).timeline.state, expected);
      }
    },
    cleanup() { fs.rmSync(runDir, { recursive: true, force: true }); },
  };
}

test("current rejection of a different event retains the successful predecessor in every replay", () => {
  const f = fixture(current);
  try {
    f.apply({ type: "ATTEMPT_STARTED", attempt: 1 });
    f.reject({ type: "CHECKS_PASSED" });
    assert.equal(f.live.phase.phase, "IMPLEMENTING");
    f.verify();
  } finally { f.cleanup(); }
});

test("a current rejected duplicate does not erase its earlier successful application", () => {
  const f = fixture(current);
  try {
    const event: Event = { type: "ATTEMPT_STARTED", attempt: 1 };
    f.apply(event);
    f.reject(event);
    f.verify();
    assert.equal(rebuildTimelineWithEvents(f.runDir, f.plan).events.filter((e) => e.type === "ATTEMPT_STARTED").length, 1);
  } finally { f.cleanup(); }
});

test("historical poisoned logs suppress only the matching legacy attempt in recovery and timeline events", () => {
  for (const revision of [legacy, undefined]) {
    const f = fixture(revision as string);
    try {
      const late = { type: "REVIEW_SUBMITTED", review: { reviewer: "B", phaseId: "p1", candidateSha: "C0", correctionStatements: [], findingStatements: [] } };
      // The historical logger wrote this refused attempt as an event first.
      f.record("event", late);
      f.record("rejected", { event: late, reason: "no rule from phase=READY" });
      f.verify();
      assert.equal(rebuildTimelineWithEvents(f.runDir, f.plan).events.length, 0, "the refused legacy attempt is absent from the accepted timeline stream");
      // Even a legacy rejection cannot invalidate a different predecessor.
      f.apply({ type: "ATTEMPT_STARTED", attempt: 1 });
      f.reject({ type: "CHECKS_PASSED" });
      f.verify();
    } finally { f.cleanup(); }
  }
});

for (const projection of ["recovery", "timeline"] as const)
for (const resumeRevision of [undefined, current, "future-runner"]) test(`a legacy init followed by current resume (${resumeRevision ?? "unstamped"}) preserves an accepted duplicate in ${projection}`, () => {
  const f = fixture(legacy);
  try {
    const poisoned: Event = { type: "CHECKS_PASSED" };
    f.record("event", poisoned);
    f.record("rejected", { event: poisoned, reason: "legacy refused attempt" });
    f.apply({ type: "RUN_RESUMED", ...(resumeRevision ? { runnerRevision: resumeRevision } : {}) });
    const event: Event = { type: "ATTEMPT_STARTED", attempt: 1 };
    f.apply(event);
    f.reject(event);
    assert.equal(f.live.phase.phase, "IMPLEMENTING");
    f.verify(f.live, projection);
    const { timeline, events } = rebuildTimelineWithEvents(f.runDir, f.plan);
    assert.equal(events.filter((e) => e.type === "CHECKS_PASSED").length, 0, "legacy suppression stays in the legacy segment");
    assert.equal(events.filter((e) => e.type === "ATTEMPT_STARTED").length, 1, "current rejection never erases its accepted predecessor");
    assert.equal(timeline.restarts.length, 1, "the resume boundary remains visible");
  } finally { f.cleanup(); }
});
