// The note an agent reads when its command hits the per-command limit must
// say what to do instead of rerunning it (run b46255dc: a worker reran a test
// file that could not finish inside the limit, piped through tail, and got no
// output three times).

import assert from "node:assert/strict";
import { test } from "node:test";

import { shTimeoutNote } from "../../src/effects/socket.ts";

test("sh timeout note: names the limit, says a rerun is killed again, and how to narrow it", () => {
  const note = shTimeoutNote(180_000);
  assert.match(note, /killed after 180 s/);
  assert.match(note, /same command will be killed again/);
  assert.match(note, /--test-name-pattern/);
  assert.match(note, /tail or head is lost/);
});
