// Plan 06h (A1/R5): an old event log replays under the new reducer. The
// 05-era fixture was recorded before the seats existed, so its init event
// records no seats: the run must replay with the default `M A B`, led by `M`,
// and reach the phase it recorded (DONE).

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { rebuildState, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import { leaderOf, seatsOf } from "../../src/core/seats.ts";
import { readLog } from "../../src/effects/log.ts";
import { cleanupDir } from "./harness.ts";

const FIXTURE_05 = fileURLToPath(new URL("../fixtures/runs/05-era", import.meta.url));

test("plan 06h: a fixture event log recorded with seats M, A, B replays to its recorded phase under the new reducer", () => {
  const runRoot = fs.mkdtempSync("/tmp/tt-06h-replay-");
  const runDir = path.join(runRoot, "05-era");
  fs.cpSync(FIXTURE_05, runDir, { recursive: true });
  try {
    // The init event records no seats: an old log is read as M, A, B.
    const records = readLog(runPaths(runDir).events).records;
    const init = records.find((r) => r.kind === "init");
    assert.ok(init, "the fixture has an init record");
    assert.equal((init!.event as { seats?: unknown }).seats, undefined, "the fixture's init event records no seats");

    const plan = JSON.parse(fs.readFileSync(path.join(runPaths(runDir).plan, "v1.json"), "utf8")) as RunPlanFile;
    const state = rebuildState(runDir, plan);
    // It replays to its recorded phase under the new reducer.
    assert.equal(state.phase.phase, "DONE");
    assert.deepEqual(seatsOf(state.phase.contract), ["M", "A", "B"], "the default seats are M, A and B");
    assert.equal(leaderOf(state.phase.contract), "M", "the default leader is the first seat");
  } finally {
    cleanupDir(runRoot);
  }
});
