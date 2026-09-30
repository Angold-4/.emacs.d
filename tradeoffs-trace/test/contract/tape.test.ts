// Plan 05h acceptance: the loop tape is a projection of a declared main path,
// never hand-drawn. The declared `MAIN_PATH` must be a real path through
// TRANSITIONS (every consecutive pair an edge, every state in the table), and
// the tape's own bytes are pinned by golden files built from fixture
// timelines: a first round in REVIEW, a round that failed CHECKS and is
// REPAIRING, AWAITING_OWNER after a panel escalation, and a phase with and
// without a gate.

import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { renderLoopTape, tapeLabel, TAPE_STEPS, type LoopTapeInput, type LoopTapePhase } from "../../src/charts.ts";
import { MAIN_PATH, TRANSITIONS } from "../../src/core/transitions.ts";

const GOLDEN = fileURLToPath(new URL("../fixtures/tape", import.meta.url));

function assertGolden(rel: string, actual: string): void {
  if (process.env.TT_UPDATE_GOLDEN) {
    mkdirSync(GOLDEN, { recursive: true });
    writeFileSync(`${GOLDEN}/${rel}`, actual);
    return;
  }
  assert.ok(existsSync(`${GOLDEN}/${rel}`), `missing golden ${rel}; run with TT_UPDATE_GOLDEN=1 to write it`);
  assert.equal(actual, readFileSync(`${GOLDEN}/${rel}`, "utf8"));
}

function phases(list: Array<[string, string]>): LoopTapeInput["phases"] {
  return list.map(([phase, at]) => ({ phase, at }));
}

function tapePhase(over: Partial<LoopTapePhase> = {}): LoopTapePhase {
  return { phase: "REVIEWING", attempt: { n: 1 }, repairRoundsUsed: 0, repairRoundsGranted: 3, contract: {}, ...over };
}

test("the declared main path is a real TRANSITIONS path", () => {
  const states = new Set<string>();
  for (const row of TRANSITIONS) {
    states.add(row.from);
    states.add(row.to);
  }
  for (const state of MAIN_PATH) {
    assert.ok(states.has(state), `main-path state ${state} is not in TRANSITIONS`);
  }
  for (let i = 0; i + 1 < MAIN_PATH.length; i++) {
    const from = MAIN_PATH[i];
    const to = MAIN_PATH[i + 1];
    assert.ok(
      TRANSITIONS.some((row) => row.axis === "phase" && row.from === from && row.to === to),
      `main path ${from} -> ${to} is not a TRANSITIONS edge`,
    );
  }
});

test("the tape's rows are derived from MAIN_PATH, not a second list", () => {
  // Every main-path state but READY (the pre-step) draws a row; ACCEPTED and
  // PUBLISHING fold into one PUBLISH row. Adding a state to MAIN_PATH without
  // a row would fail here instead of drifting.
  const drawn = new Set(TAPE_STEPS.flatMap((s) => s.states));
  for (const state of MAIN_PATH) {
    if (state === "READY") continue;
    assert.ok(drawn.has(state), `main-path state ${state} draws no tape row`);
  }
  assert.deepEqual(
    TAPE_STEPS.map((s) => s.name),
    ["BASELINE", "IMPLEMENT", "FREEZE", "CHECKS", "PROBE", "REVIEW", "EVALUATE", "RESOLVE", "GATE", "PUBLISH", "DONE"],
  );
});

test("tapeLabel is the readable id plus the id's tail", () => {
  assert.equal(tapeLabel("tt-05h-loop-tape"), "05h loop tape");
  assert.equal(tapeLabel("p1"), "p1");
});

const REVIEW: LoopTapeInput = {
  label: "05g per-seat models",
  round: 1,
  phases: phases([
    ["READY", "2026-09-27T09:00:00.000Z"],
    ["BASELINE", "2026-09-27T09:00:05.000Z"],
    ["IMPLEMENTING", "2026-09-27T09:00:15.000Z"],
    ["FREEZING", "2026-09-27T09:16:15.000Z"],
    ["CHECKING", "2026-09-27T09:16:25.000Z"],
    ["PROBING", "2026-09-27T09:23:25.000Z"],
    ["REVIEWING", "2026-09-27T09:23:26.000Z"],
  ]),
  phase: tapePhase({ phase: "REVIEWING" }),
  run: "RUN_ACTIVE",
  endMs: Date.parse("2026-09-27T09:27:38.000Z"),
  models: { reviewer: "deepseek-v4.1-flash" },
  limits: { review: 15 * 60_000 },
};

test("tape golden: a first round in REVIEW", () => {
  const tape = renderLoopTape(REVIEW);
  assertGolden("review.txt", tape);
  // The passed rows carry their own times up to PROBE, the head its time
  // against the stage limit, the reviewers and the model, and the steps the
  // round has not reached are blank.
  assert.match(tape, /^  ✓  PROBE       1s$/m);
  assert.match(tape, /^  ▶  REVIEW      4m12s of 15m   M·A·B · deepseek-v4\.1-flash$/m);
  assert.match(tape, /^     EVALUATE$/m);
  assert.match(tape, /^     DONE$/m);
  // A phase without a gate draws no GATE row.
  assert.doesNotMatch(tape, /^  .  GATE\b/m);
});

test("tape golden: a round that failed CHECKS and is REPAIRING", () => {
  const tape = renderLoopTape({
    label: "05g per-seat models",
    round: 1,
    phases: phases([
      ["READY", "2026-09-27T09:00:00.000Z"],
      ["IMPLEMENTING", "2026-09-27T09:00:10.000Z"],
      ["FREEZING", "2026-09-27T09:16:10.000Z"],
      ["CHECKING", "2026-09-27T09:16:20.000Z"],
      ["REPAIRING", "2026-09-27T09:23:20.000Z"],
    ]),
    phase: tapePhase({ phase: "REPAIRING", attempt: { n: 2 }, repairRoundsUsed: 1 }),
    run: "RUN_ACTIVE",
    endMs: Date.parse("2026-09-27T09:23:20.000Z"),
  });
  assertGolden("repairing.txt", tape);
  assert.match(tape, /^  ✗  CHECKS      7m  → repairing, attempt 2\/3$/m);
  // No BASELINE row: this phase never took one.
  assert.doesNotMatch(tape, /BASELINE/);
});

test("tape golden: AWAITING_OWNER after a panel escalation", () => {
  const tape = renderLoopTape({
    label: "05g per-seat models",
    round: 1,
    phases: phases([
      ["READY", "2026-09-27T09:00:00.000Z"],
      ["IMPLEMENTING", "2026-09-27T09:00:10.000Z"],
      ["FREEZING", "2026-09-27T09:16:10.000Z"],
      ["CHECKING", "2026-09-27T09:16:20.000Z"],
      ["PROBING", "2026-09-27T09:23:20.000Z"],
      ["REVIEWING", "2026-09-27T09:23:30.000Z"],
      ["EVALUATING", "2026-09-27T09:23:40.000Z"],
      ["AWAITING_OWNER", "2026-09-27T09:25:40.000Z"],
    ]),
    phase: tapePhase({
      phase: "AWAITING_OWNER",
      ownerRequests: [{ origin: "blocker_panel", status: "open", linkedMessageId: "B-2" }],
    }),
    run: "RUN_ACTIVE",
    endMs: Date.parse("2026-09-27T09:25:40.000Z"),
  });
  assertGolden("awaiting-owner.txt", tape);
  assert.match(tape, /^  ▶  EVALUATE    2m  → waiting for you · panel escalated B-2$/m);
});

test("a repair starts a fresh tape round, not a sum of rounds", () => {
  // Round 1 failed CHECKS (attempt 1); the repair is attempt 2, whose own
  // freeze/checks/review are round 2. The tape is view.round's round only: the
  // IMPLEMENT row is attempt 2's 15m, not 16m + 15m.
  const tape = renderLoopTape({
    label: "05g per-seat models",
    round: 2,
    phases: phases([
      ["READY", "2026-09-27T09:00:00.000Z"],
      ["IMPLEMENTING", "2026-09-27T09:00:10.000Z"],
      ["FREEZING", "2026-09-27T09:16:10.000Z"],
      ["CHECKING", "2026-09-27T09:16:20.000Z"],
      ["PROBING", "2026-09-27T09:23:20.000Z"],
      ["REVIEWING", "2026-09-27T09:23:30.000Z"],
      ["EVALUATING", "2026-09-27T09:30:00.000Z"],
      ["RESOLVING", "2026-09-27T09:40:00.000Z"],
      ["REPAIRING", "2026-09-27T09:45:00.000Z"],
      ["IMPLEMENTING", "2026-09-27T09:45:10.000Z"],
      ["FREEZING", "2026-09-27T10:00:10.000Z"],
      ["CHECKING", "2026-09-27T10:00:20.000Z"],
      ["PROBING", "2026-09-27T10:07:20.000Z"],
      ["REVIEWING", "2026-09-27T10:07:30.000Z"],
    ]),
    phase: tapePhase({ phase: "REVIEWING", attempt: { n: 2 }, repairRoundsUsed: 1 }),
    run: "RUN_ACTIVE",
    endMs: Date.parse("2026-09-27T10:11:42.000Z"),
    models: { reviewer: "deepseek-v4.1-flash" },
    limits: { review: 15 * 60_000 },
  });
  assertGolden("repair-round.txt", tape);
  assert.match(tape, /^05g per-seat models · round 2 · attempt 2\/3 · /m);
  assert.match(tape, /^  ✓  IMPLEMENT   15m$/m);
  assert.match(tape, /^  ✓  CHECKS      7m$/m);
  assert.match(tape, /^  ▶  REVIEW      4m12s of 15m/m);
  // Round 1's 16m IMPLEMENT time is not added in.
  assert.doesNotMatch(tape, /31m/);
});

test("a phase with a gate draws the GATE row; one without does not", () => {
  const withGate = renderLoopTape({ ...REVIEW, phase: tapePhase({ phase: "REVIEWING", contract: { gate: "make gate" } }) });
  assertGolden("gate.txt", withGate);
  assert.match(withGate, /^     GATE$/m);
  assert.doesNotMatch(renderLoopTape(REVIEW), /^  .  GATE\b/m);
});
