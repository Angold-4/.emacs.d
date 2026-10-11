// Plan 06l (A5): the per-seat table. For a recorded run with known events,
// every count equals the hand count; `tt seats` prints the same table for a
// run and the summed table for a program.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { computeSeatTable, mergeSeatTables, renderSeatTable } from "../../src/core/seat-table.ts";
import type { LogRecord } from "../../src/effects/log.ts";
import { cleanupDir } from "../conductor/harness.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function rec(seq: number, ts: string, kind: string, event: unknown, actionId?: string): LogRecord {
  return { seq, ts, kind, event, ...(actionId ? { actionId } : {}) };
}

const T0 = "2026-10-11T10:00:00.000Z";
const T60 = "2026-10-11T10:01:00.000Z";
const T120 = "2026-10-11T10:02:00.000Z";

/** A hand-built log with known counts for M, A and B. */
function knownRecords(): LogRecord[] {
  return [
    rec(1, T0, "init", { runId: "r1", seats: { seats: ["M", "A", "B"], leader: "M", workers: 2 } }),
    rec(2, T0, "event", { type: "FINDING_RAISED", finding: { id: "F1", raisedBy: "M", status: "open" } }),
    rec(3, T0, "event", { type: "FINDING_RAISED", finding: { id: "F2", raisedBy: "A", status: "open" } }),
    rec(4, T0, "event", { type: "FINDING_RAISED", finding: { id: "F3", raisedBy: "A", alsoRaisedBy: ["M"], status: "open" } }),
    rec(5, T60, "event", { type: "FINDING_DISPROVED", findingId: "F1" }),
    rec(6, T60, "event", { type: "FINDING_ACCEPTED_BY_OWNER", findingId: "F2" }),
    rec(7, T0, "event", { type: "ROUND_REVIEW_SUBMITTED", round: 1, lane: "a", seat: "M", review: { reviewer: "M" } }),
    rec(8, T60, "event", { type: "ROUND_REVIEW_SUBMITTED", round: 1, lane: "b", seat: "M", review: { reviewer: "M" } }),
    rec(9, T0, "event", { type: "ROUND_REVIEW_SUBMITTED", round: 1, lane: "a", seat: "A", review: { reviewer: "A" } }),
    rec(10, T60, "event", { type: "REVIEW_TIMED_OUT", reviewer: "B" }),
    rec(11, T60, "lane_review_retried", { round: 1, lane: "a", seat: "M", reason: "timeout (turn 1)" }),
    // The same retry failing is NOT a second retry.
    rec(20, T60, "lane_review_retry_failed", { round: 1, lane: "a", seat: "M", reason: "timeout (turn 1)" }),
    rec(12, T60, "review_reprompt", { reviewer: "A", agentId: "x", reason: "turn 2 settled" }),
    rec(13, T60, "incomplete_review_rejected", { reviewer: "B", lane: "a", missing: ["D-1"] }),
    // Plan 2c: a later FINDING_ALSO_RAISED makes F4 not unique for A.
    rec(18, T60, "event", { type: "FINDING_RAISED", finding: { id: "F4", raisedBy: "A", status: "open" } }),
    rec(19, T60, "event", { type: "FINDING_ALSO_RAISED", findingId: "F4", reviewer: "M" }),
    rec(14, T0, "intent", { seat: "M", candidateSha: "C1" }, "a1"),
    rec(15, T60, "completion", { seat: "M", ok: true }, "a1"),
    // The single-candidate review path logs `{ reviewer, ok }`, not `seat`.
    rec(16, T0, "intent", { reviewer: "A", agentId: "x", pgid: 1 }, "a2"),
    rec(17, T120, "completion", { reviewer: "A", ok: true }, "a2"),
  ];
}

test("plan 06l: the seat table counts raised, unique, accepted, dropped, timeouts, retries and minutes per seat", () => {
  const table = computeSeatTable(knownRecords(), ["M", "A", "B"]);
  const bySeat = Object.fromEntries(table.seats.map((s) => [s.seat, s]));
  assert.deepEqual(
    { raised: bySeat.M.findingsRaised, unique: bySeat.M.findingsUnique, accepted: bySeat.M.findingsAccepted, dropped: bySeat.M.findingsDropped, turns: bySeat.M.reviewTurns, timeouts: bySeat.M.timeouts, retries: bySeat.M.retries, reprompts: bySeat.M.reprompts, refused: bySeat.M.refusedSubmissions },
    { raised: 1, unique: 1, accepted: 0, dropped: 1, turns: 2, timeouts: 0, retries: 1, reprompts: 0, refused: 0 },
    "M's hand count",
  );
  assert.deepEqual(
    { raised: bySeat.A.findingsRaised, unique: bySeat.A.findingsUnique, accepted: bySeat.A.findingsAccepted, dropped: bySeat.A.findingsDropped, turns: bySeat.A.reviewTurns, timeouts: bySeat.A.timeouts, retries: bySeat.A.retries, reprompts: bySeat.A.reprompts, refused: bySeat.A.refusedSubmissions },
    { raised: 3, unique: 1, accepted: 1, dropped: 0, turns: 1, timeouts: 0, retries: 0, reprompts: 1, refused: 0 },
    "A's hand count (F4 is co-raised by M, so it is not unique)",
  );
  assert.deepEqual(
    { raised: bySeat.B.findingsRaised, unique: bySeat.B.findingsUnique, accepted: bySeat.B.findingsAccepted, dropped: bySeat.B.findingsDropped, turns: bySeat.B.reviewTurns, timeouts: bySeat.B.timeouts, retries: bySeat.B.retries, reprompts: bySeat.B.reprompts, refused: bySeat.B.refusedSubmissions },
    { raised: 0, unique: 0, accepted: 0, dropped: 0, turns: 0, timeouts: 1, retries: 0, reprompts: 0, refused: 1 },
    "B's hand count",
  );
  assert.deepEqual(bySeat.M.turnMinutes, { p50: 1, max: 1 }, "M's one-minute turn");
  assert.deepEqual(bySeat.A.turnMinutes, { p50: 2, max: 2 }, "A's two-minute turn");
  const text = renderSeatTable(table);
  assert.match(text, /^# seats$/m);
  assert.match(text, /M\s+-\s+1\s+1\s+0\s+1\s+2\s+0\s+1\s+0\s+0\s+1\.00\s+1\.00/);
  // A seat's model is shown when the plan names one, and the model does not
  // split a seat's own records across two rows.
  const withModels = computeSeatTable(knownRecords(), ["M", "A", "B"], { M: "opus", A: "flash" });
  assert.equal(withModels.seats.find((s) => s.seat === "M")!.model, "opus");
  assert.equal(withModels.seats.find((s) => s.seat === "A")!.model, "flash");
  assert.equal(withModels.seats.filter((s) => s.seat === "M").length, 1, "one row per seat and model");
  // A seat that ran on two models in two program nodes keeps two rows.
  const nodeX = computeSeatTable([rec(1, T0, "event", { type: "FINDING_RAISED", finding: { id: "H1", raisedBy: "A", status: "open" } })], ["A"], { A: "X" });
  const nodeY = computeSeatTable([rec(1, T0, "event", { type: "FINDING_RAISED", finding: { id: "H2", raisedBy: "A", status: "open" } })], ["A"], { A: "Y" });
  const merged = mergeSeatTables([nodeX, nodeY]);
  assert.deepEqual(
    merged.seats.map((s) => [s.seat, s.model, s.findingsRaised]).sort(),
    [["A", "X", 1], ["A", "Y", 1]],
    "a seat's two models are not pooled into one row",
  );
});

function writeRun(root: string, runId: string, records: LogRecord[], seats: string[]): string {
  const runDir = path.join(root, runId);
  fs.mkdirSync(path.join(runDir, "plan"), { recursive: true });
  fs.writeFileSync(path.join(runDir, "meta.json"), JSON.stringify({ runId }));
  fs.writeFileSync(
    path.join(runDir, "plan", "v1.json"),
    JSON.stringify({ title: "t", repo: root, integrationBranch: "main", checks: [], seats, phases: [] }),
  );
  fs.writeFileSync(path.join(runDir, "events.jsonl"), records.map((r) => JSON.stringify(r)).join("\n") + "\n");
  return runDir;
}

function tt(args: string[]): string {
  return execFileSync(process.execPath, [CLI, ...args], { encoding: "utf8" });
}

test("plan 06l: tt seats prints the same table for a run and summed for a program", () => {
  const root = fs.mkdtempSync("/tmp/tt-seats-");
  try {
    const seats = ["M", "A", "B"];
    const records1 = knownRecords();
    const records2: LogRecord[] = [
      rec(1, T0, "event", { type: "FINDING_RAISED", finding: { id: "G1", raisedBy: "A", status: "open" } }),
      rec(2, T0, "event", { type: "ROUND_REVIEW_SUBMITTED", round: 1, lane: "a", seat: "B", review: { reviewer: "B" } }),
    ];
    const run1 = writeRun(root, "run1", records1, seats);
    const run2 = writeRun(root, "run2", records2, seats);

    const expectedRun = renderSeatTable(computeSeatTable(records1, seats));
    assert.equal(tt(["seats", run1, "--root", root]), expectedRun, "tt seats <run> prints the run's table");

    // The program's table pools every node run's records.
    const programsDir = path.join(root, "programs", "prog");
    fs.mkdirSync(programsDir, { recursive: true });
    fs.writeFileSync(
      path.join(programsDir, "program.json"),
      JSON.stringify({
        title: "prog",
        maxParallel: 1,
        entries: ["n1", "n2"].map((id) => ({
          id,
          after: [],
          plan: { title: "p", repo: root, integrationBranch: "main", checks: [], phases: [{ id: "p1", goal: "g", acceptance: ["it works"], checks: [], boundaries: [], reserved: [] }] },
        })),
      }),
    );
    fs.writeFileSync(
      path.join(programsDir, "events.jsonl"),
      [
        JSON.stringify({ ts: T0, event: { type: "NODE_STARTED", node: "n1", runId: "run1" } }),
        JSON.stringify({ ts: T0, event: { type: "NODE_STARTED", node: "n2", runId: "run2" } }),
      ].join("\n") + "\n",
    );
    const expectedProgram = renderSeatTable(mergeSeatTables([computeSeatTable(records1, seats), computeSeatTable(records2, seats)]));
    assert.equal(tt(["seats", "prog", "--root", root]), expectedProgram, "tt seats <program> prints the summed table");

    // Spot-check the sum: A raised three findings in run 1 and one in run 2.
    const summed = computeSeatTable([...records1, ...records2], seats);
    assert.equal(summed.seats.find((s) => s.seat === "A")!.findingsRaised, 4);
  } finally {
    cleanupDir(root);
  }
});
