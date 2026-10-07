// The note an agent reads when its command hits the per-command limit must
// say what to do instead of rerunning it (run b46255dc: a worker reran a test
// file that could not finish inside the limit, piped through tail, and got no
// output three times). Plan 06e (A3): the note also says how much memory the
// command used and how much the machine had left, from a sampler tests can
// replace.

import assert from "node:assert/strict";
import { test } from "node:test";

import { runCommand } from "../../src/effects/shell.ts";
import { shTimeoutNote } from "../../src/effects/socket.ts";

test("sh timeout note: names the limit, says a rerun is killed again, and how to narrow it", () => {
  const note = shTimeoutNote(180_000);
  assert.match(note, /killed after 180 s/);
  assert.match(note, /same command will be killed again/);
  assert.match(note, /--test-name-pattern/);
  assert.match(note, /tail or head is lost/);
});

test("plan 06e: a shell kill note includes peak RSS and free memory", async () => {
  // A fake sampler with fixed values: the note's numbers are deterministic.
  const result = await runCommand({
    command: "sleep 30",
    deadlineMs: 50,
    termGraceMs: 50,
    sampler: () => ({ rssMB: 123, freeMemMB: 456 }),
  }).result;
  assert.equal(result.timedOut, true, "the command was killed at its limit");
  assert.deepEqual(result.resourceSample, { peakRssMB: 123, freeMemMB: 456 });

  const note = shTimeoutNote(50, result.resourceSample);
  assert.match(note, /peak RSS/);
  assert.match(note, /123 MB/);
  assert.match(note, /free/);
  assert.match(note, /456 MB/);
});
