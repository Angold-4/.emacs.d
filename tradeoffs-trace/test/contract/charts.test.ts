// Plan 03c acceptance: the charts are generated from the same tables the
// runtime obeys. The coverage tests fail when a transition row is added to
// `TRANSITIONS` or `MESSAGE_TRANSITIONS` and no edge draws it; the two golden
// files pin the visible layout (a phase timeline with two rounds and a gate;
// a four-node program DAG with a join).

import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { renderMessageChart, renderPhaseChart, renderProgramChart, statsFromTimeline, type TimelineLike } from "../../src/charts.ts";
import { MESSAGE_TRANSITIONS } from "../../src/core/messages.ts";
import { TRANSITIONS } from "../../src/core/transitions.ts";
import { expandProgram, initialProgramState, reduceProgram, type ProgramFile } from "../../src/core/program.ts";
import type { RunPlanFile } from "../../src/conductor.ts";

const GOLDEN = fileURLToPath(new URL("../fixtures/charts", import.meta.url));

function golden(rel: string): string {
  return readFileSync(`${GOLDEN}/${rel}`, "utf8");
}

function assertGolden(rel: string, actual: string): void {
  if (process.env.TT_UPDATE_GOLDEN) {
    mkdirSync(`${GOLDEN}/${rel.split("/").slice(0, -1).join("/")}`, { recursive: true });
    writeFileSync(`${GOLDEN}/${rel}`, actual);
    return;
  }
  assert.ok(existsSync(`${GOLDEN}/${rel}`), `missing golden ${rel}; run with TT_UPDATE_GOLDEN=1 to write it`);
  assert.equal(actual, golden(rel));
}

test("every TRANSITIONS row appears as a drawn edge tagged with its row id", () => {
  const chart = renderPhaseChart(TRANSITIONS, { stats: { current: "CHECKING", entries: {}, timeMs: {} }, model: "sonnet" });
  for (const row of TRANSITIONS) {
    assert.ok(chart.includes(`[${row.id}]`), `TRANSITIONS row '${row.id}' is not drawn in the phase chart`);
  }
});

test("every MESSAGE_TRANSITIONS row appears as a drawn edge tagged with its row id", () => {
  const chart = renderMessageChart(MESSAGE_TRANSITIONS);
  for (const row of MESSAGE_TRANSITIONS) {
    assert.ok(chart.includes(`[${row.id}]`), `MESSAGE_TRANSITIONS row '${row.id}' is not drawn in the message chart`);
  }
});

/** The chart text between the doc's markers. The doc must exist: a moved or
 * renamed document must fail this guard, not silently stop being checked
 * (finding A-3). */
function docMessageChart(rel: string): string {
  const file = fileURLToPath(new URL(rel, import.meta.url));
  assert.ok(existsSync(file), `missing ${rel}; the message chart cannot be checked`);
  const m = readFileSync(file, "utf8").match(/<!-- BEGIN message-chart[^>]*-->\n```text\n([\s\S]*?)```\n<!-- END message-chart -->/);
  assert.ok(m, `no message-chart block in ${rel}`);
  return m![1];
}

test("the contract doc and the runbook quote the generated message chart", () => {
  const chart = renderMessageChart(MESSAGE_TRANSITIONS);
  for (const rel of ["../../../docs/tradeoffs-trace-contract.md", "../../../docs/tradeoffs-trace-runbook.md"]) {
    assert.equal(docMessageChart(rel), chart, `${rel}'s message chart is stale; regenerate it (renderMessageChart)`);
  }
});

/** A phase timeline with two full rounds and a gate: check → probe → review →
 * resolve → gate (fails) → repair, then a second round whose gate passes and
 * the phase publishes. */
const TIMELINE: TimelineLike = {
  state: { run: "RUN_ACTIVE", phase: { phase: "DONE" } },
  phases: [
    ["READY", "2026-09-27T09:00:00.000Z"],
    ["IMPLEMENTING", "2026-09-27T09:00:10.000Z"],
    ["FREEZING", "2026-09-27T09:26:10.000Z"],
    ["CHECKING", "2026-09-27T09:26:20.000Z"],
    ["PROBING", "2026-09-27T09:30:20.000Z"],
    ["REVIEWING", "2026-09-27T09:36:20.000Z"],
    ["RESOLVING", "2026-09-27T09:51:20.000Z"],
    ["GATING", "2026-09-27T09:51:30.000Z"],
    ["REPAIRING", "2026-09-27T10:06:30.000Z"],
    ["IMPLEMENTING", "2026-09-27T10:06:40.000Z"],
    ["FREEZING", "2026-09-27T10:21:40.000Z"],
    ["CHECKING", "2026-09-27T10:21:50.000Z"],
    ["PROBING", "2026-09-27T10:25:50.000Z"],
    ["REVIEWING", "2026-09-27T10:31:50.000Z"],
    ["RESOLVING", "2026-09-27T10:46:50.000Z"],
    ["GATING", "2026-09-27T10:47:00.000Z"],
    ["ACCEPTED", "2026-09-27T11:02:00.000Z"],
    ["PUBLISHING", "2026-09-27T11:02:10.000Z"],
    ["DONE", "2026-09-27T11:02:20.000Z"],
  ].map(([phase, at]) => ({ phase, at })),
};

test("loop.txt matches its golden file, with the current-state marker", () => {
  const stats = statsFromTimeline(TIMELINE, new Date("2026-09-27T11:05:00.000Z"));
  const chart = renderPhaseChart(TRANSITIONS, { stats, models: { worker: "opus", reviewer: "sonnet" } });
  assertGolden("loop.txt", chart);
  assert.match(chart, /^current state: DONE/m);
  assert.match(chart, /^current run: RUN_ACTIVE/m);
  assert.match(chart, /^> \+/m);
  assert.match(chart, /entered 2x/);
  // Per-role models: the worker and the reviewers each show their own (D-18).
  assert.match(chart, /IMPLEMENTING.*worker - model opus/);
  assert.match(chart, /REVIEWING.*M, A, B - model sonnet/);
  // The run axis carries its own current marker, not phase counts (M-5).
  assert.match(chart, /^> .*\n  \| RUN_ACTIVE\s+\|  current/m);
});

function phasePlan(title: string): RunPlanFile {
  return {
    title,
    repo: "/tmp/repo",
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
  } as unknown as RunPlanFile;
}

test("program.txt matches its golden file, with readable ids and a join", () => {
  const program: ProgramFile = {
    title: "plan 14",
    maxParallel: 2,
    entries: [
      { id: "a", after: [], plan: phasePlan("14a") },
      { id: "b", after: ["a"], plan: phasePlan("14b") },
      { id: "c", after: ["a"], plan: phasePlan("14c") },
      { id: "d", after: ["b", "c"], plan: phasePlan("14d") },
    ],
    readableIds: { a: "atlas-01", b: "atlas-02", c: "atlas-03", d: "atlas-04" },
  };
  const nodes = expandProgram(program);
  let state = initialProgramState(nodes);
  state = reduceProgram(state, { type: "NODE_STARTED", node: "a", runId: "r-a" });
  state = reduceProgram(state, { type: "NODE_STATUS", node: "a", status: "done" });
  state = reduceProgram(state, { type: "NODE_STARTED", node: "b", runId: "r-b" });
  state = reduceProgram(state, { type: "NODE_STARTED", node: "c", runId: "r-c" });
  state = reduceProgram(state, { type: "NODE_STATUS", node: "c", status: "needs-you", reason: "the repair budget ran out" });
  const chart = renderProgramChart(program, nodes, state, { programId: "atlas", phaseOf: { b: "REVIEWING" } });
  assertGolden("program.txt", chart);
  assert.match(chart, /\| atlas-01  a\s+\|  done/);
  assert.match(chart, /\| atlas-02  b\s+\|  running - REVIEWING/);
  assert.match(chart, /> \+/);
  assert.match(chart, /after: atlas-02, atlas-03/);
});
