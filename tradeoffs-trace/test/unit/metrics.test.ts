// Plan 04c: the balance metrics. The acceptance criterion is "from a fixture
// run, every metric above has the expected value": this builds a fixture run
// directory with its own control log and a fixture phase, and checks each
// number of `views/metrics.json` (computed through the same path `tt contract
// rebuild`/`check` uses).

import assert from "node:assert/strict";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import * as path from "node:path";
import { test } from "node:test";

import { computeMetrics, metricsForRunDir, metricsLine, metricsSummary, projectMetrics, type MetricsTimeline } from "../../src/metrics.ts";
import type { Entry } from "../../src/core/entries.ts";
import { basePhase, makeMessage } from "./helpers.ts";

/** READY 0s → implement 20s → review 20s → evaluate 10s → resolve 5s →
 * owner 5s → DONE, with the last event at 1m00s. */
const TIMELINE: MetricsTimeline = {
  phases: [
    { phase: "READY", at: "2026-09-27T09:00:00.000Z" },
    { phase: "IMPLEMENTING", at: "2026-09-27T09:00:00.000Z" },
    { phase: "REVIEWING", at: "2026-09-27T09:00:20.000Z" },
    { phase: "EVALUATING", at: "2026-09-27T09:00:40.000Z" },
    { phase: "RESOLVING", at: "2026-09-27T09:00:50.000Z" },
    { phase: "AWAITING_OWNER", at: "2026-09-27T09:00:55.000Z" },
    { phase: "DONE", at: "2026-09-27T09:01:00.000Z" },
  ],
};

const EVENTS = [
  { ts: "2026-09-27T09:00:58.000Z", event: { type: "OWNER_VERDICT", verdict: "accept" } },
  { ts: "2026-09-27T09:00:58.100Z", event: { type: "OWNER_VERDICT", verdict: "refuse" } },
  { ts: "2026-09-27T09:00:59.000Z", event: { type: "PANEL_DECIDED", outcome: "escalate" } },
  { ts: "2026-09-27T09:00:59.100Z", event: { type: "PANEL_DECIDED", outcome: "downgrade" } },
  { ts: "2026-09-27T09:00:59.200Z", event: { type: "PANEL_DECIDED", outcome: "incomplete" } },
];

/** A fixture run directory whose `events.jsonl` is the metrics' own log. */
function fixtureRun(): string {
  const dir = mkdtempSync(path.join(tmpdir(), "tt-metrics-"));
  const lines = EVENTS.map((e, i) => JSON.stringify({ seq: i + 1, ts: e.ts, kind: "event", event: e.event }));
  writeFileSync(path.join(dir, "events.jsonl"), `${lines.join("\n")}\n`);
  return dir;
}

function fixturePhase() {
  return basePhase({
    runId: "r-metrics",
    phaseId: "p1",
    round: 2,
    messages: [
      makeMessage({ id: "T-1", state: "raw" }),
      makeMessage({ id: "T-2", state: "published" }),
      makeMessage({ id: "T-3", state: "merged", raisedBy: "reviewer M" }),
      makeMessage({ id: "T-4", state: "dropped" }),
      makeMessage({ id: "T-5", state: "accepted" }),
      makeMessage({ id: "F-1", type: "finding", state: "published" }),
      makeMessage({ id: "F-2", type: "finding", state: "raw" }),
      makeMessage({ id: "F-3", type: "finding", state: "merged" }),
      makeMessage({ id: "B-1", type: "blocker", state: "published" }),
      makeMessage({ id: "B-2", type: "blocker", state: "dropped" }),
    ],
  });
}

test("every balance metric has its expected value from the fixture run", () => {
  const dir = fixtureRun();
  try {
    const metrics = metricsForRunDir(dir, fixturePhase(), TIMELINE);

    // Rounds and wall time.
    assert.equal(metrics.rounds, 2);
    assert.equal(metrics.wallMs, 60_000);
    assert.equal(metrics.reviewMs, 30_000, "REVIEWING 20s + EVALUATING 10s");
    assert.equal(metrics.reviewShare, 0.5);
    assert.equal(metrics.ownerWaitMs, 5_000);

    // Raw vs published per type.
    assert.deepEqual(metrics.messages.tradeoff, {
      raised: 5, raw: 1, published: 1, merged: 1, dropped: 1, accepted: 1, refused: 0, resolved: 0, superseded: 0,
    });
    assert.deepEqual(metrics.messages.finding, {
      raised: 3, raw: 1, published: 1, merged: 1, dropped: 0, accepted: 0, refused: 0, resolved: 0, superseded: 0,
    });
    assert.deepEqual(metrics.messages.blocker, {
      raised: 2, raw: 0, published: 1, merged: 0, dropped: 1, accepted: 0, refused: 0, resolved: 0, superseded: 0,
    });

    // Merge and drop rates.
    assert.equal(metrics.mergeRate, 0.2, "2 merged of 10 raised");
    assert.equal(metrics.dropRate, 0.2, "2 dropped of 10 raised");

    // The owner's A/D counts and D rate.
    assert.deepEqual(metrics.ownerVerdicts, { accept: 1, refuse: 1 });
    assert.equal(metrics.refuseRate, 0.5);

    // Blockers escalated vs downgraded (and incomplete).
    assert.deepEqual(metrics.blockers, { escalated: 1, downgraded: 1, incomplete: 1 });

    // The unexposed-decision proxy: the reviewer-raised trade-off.
    assert.equal(metrics.unexposedTradeoffs, 1);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test("the cleanness metrics join views/metrics.json, the status line and tt summary", () => {
  // 13 live entries (over the default budget of 12); two of them are
  // near-duplicates without a shared anchor (an open ≈ hint each).
  const messages = Array.from({ length: 13 }, (_, i) =>
    makeMessage({ id: `T-${i + 1}`, title: `topic number ${i + 1}`, evidence: [`src/f${i}.rs:1 x`], summary: `s${i}`, boundCandidateSha: "C1" }),
  );
  messages[11] = makeMessage({ id: "T-12", title: "tolerances raised on the fill path", evidence: ["src/f11.rs:1 x"], summary: "s", boundCandidateSha: "C1" });
  messages[12] = makeMessage({ id: "T-13", title: "tolerances raised on the fill path again", evidence: ["src/f12.rs:1 x"], summary: "s", boundCandidateSha: "C1" });
  const phase = basePhase({ runId: "r-metrics", phaseId: "p1", messages, entries: [] as Entry[] });
  const m = computeMetrics(phase, TIMELINE, EVENTS.map((e) => ({ ...e.event, ts: e.ts })));
  assert.equal(m.cleanness.liveEntries, 13);
  assert.equal(m.cleanness.entryBudget, 12);
  assert.equal(m.cleanness.entryBudgetWarning, true);
  assert.ok(m.cleanness.openHints >= 2, `expected the ≈ hint to be counted: ${m.cleanness.openHints}`);
  assert.ok(m.cleanness.entriesPerAnchor > 0);
  const json = JSON.parse(projectMetrics(m));
  assert.equal(json.cleanness.liveEntries, 13);
  assert.ok(metricsLine(m).includes("entries 13 (over budget 12)"));
  assert.ok(metricsLine(m).includes("lint 0"));
  assert.ok(metricsSummary(m).some((l) => l.includes("Cleanness") && l.includes("13 live entries")));
});

test("the metrics projection is deterministic and the status line carries every number", () => {
  const dir = fixtureRun();
  try {
    const phase = fixturePhase();
    const first = projectMetrics(computeMetrics(phase, TIMELINE, EVENTS.map((e) => ({ ...e.event, ts: e.ts }))));
    const second = projectMetrics(computeMetrics(phase, TIMELINE, EVENTS.map((e) => ({ ...e.event, ts: e.ts }))));
    assert.equal(first, second);
    const parsed = JSON.parse(first);
    assert.equal(parsed.reviewShare, 0.5);
    assert.equal(first.endsWith("\n"), true, "the projection is a text file with a trailing newline");

    const line = metricsLine(computeMetrics(phase, TIMELINE, EVENTS.map((e) => ({ ...e.event, ts: e.ts }))));
    for (const needle of ["2 round", "review 50%", "merge 20%", "drop 20%", "A 1 D 1", "owner wait", "1 escalated, 1 downgraded", "unexposed 1"]) {
      assert.ok(line.includes(needle), `the status line must carry "${needle}": ${line}`);
    }
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
