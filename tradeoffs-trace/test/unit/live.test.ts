// Plan 06f (A2): `<root>/live.json` is the one file the Emacs mode line reads.
// The conductor and the scheduler are its only writers; these unit tests pin
// the merge/removal rules directly, including the removal a closed conductor
// performs (a `run?.id` filter once left every row in place).

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { livePath, readLive, updateLiveRun, updateLiveWaiting } from "../../src/view.ts";

function root(): string {
  return fs.mkdtempSync("/tmp/tt-live-unit-");
}

test("plan 06f: updateLiveRun removes a closed run's row and keeps every other", () => {
  const dir = root();
  try {
    updateLiveRun(dir, "a", { title: "A", phase: "IMPLEMENTING", needsOwner: false });
    updateLiveRun(dir, "b", { title: "B", phase: "REVIEWING", needsOwner: false });
    assert.deepEqual(
      readLive(dir).runs.map((r) => r.id).sort(),
      ["a", "b"],
    );
    // The exact bug: a closed conductor passes no row, and the row must go.
    updateLiveRun(dir, "a", undefined);
    const after = readLive(dir);
    assert.deepEqual(after.runs.map((r) => r.id), ["b"], "the closed run's row is gone, the other stays");
    // A stopped-but-owner-waiting run is re-published with needsOwner true.
    updateLiveRun(dir, "b", { title: "B", phase: "AWAITING_OWNER", needsOwner: true });
    const waiting = readLive(dir).runs.find((r) => r.id === "b");
    assert.equal(waiting?.phase, "AWAITING_OWNER");
    assert.equal(waiting?.needsOwner, true);
    // Removing it again leaves an empty list.
    updateLiveRun(dir, "b", undefined);
    assert.deepEqual(readLive(dir).runs, []);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06f: updateLiveWaiting replaces only its program's nodes and keeps the runs", () => {
  const dir = root();
  try {
    updateLiveRun(dir, "run-a", { title: "A", phase: "IMPLEMENTING", needsOwner: false });
    updateLiveWaiting(dir, "prog1", [
      { program: "prog1", node: "b" },
      { program: "prog1", node: "c" },
    ]);
    updateLiveWaiting(dir, "prog2", [{ program: "prog2", node: "z" }]);
    // prog1 is replaced, prog2 kept, the run row untouched.
    updateLiveWaiting(dir, "prog1", [{ program: "prog1", node: "d" }]);
    const live = readLive(dir);
    assert.deepEqual(live.waitingNodes, [
      { program: "prog2", node: "z" },
      { program: "prog1", node: "d" },
    ]);
    assert.deepEqual(live.runs.map((r) => r.id), ["run-a"]);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06f: live.json is written atomically and a missing file reads empty", () => {
  const dir = root();
  try {
    assert.deepEqual(readLive(dir), { updatedAt: "", runs: [], waitingNodes: [] });
    updateLiveRun(dir, "a", { title: "A", phase: "DONE", needsOwner: false });
    assert.ok(fs.existsSync(livePath(dir)));
    // No temporary file is left behind by the tmp+rename write.
    assert.deepEqual(
      fs.readdirSync(dir).filter((f) => f.includes(".tmp")),
      [],
    );
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});
