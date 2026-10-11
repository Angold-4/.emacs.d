// 06k2 round 5 (2026-10-10): plan 06k1 made `loserHad` a required field of a
// pick vote, but the `submit_pick_vote` tool's parameter schema never declared
// it. A model cannot send an undeclared field as an object, so every vote was
// refused ("needs loserHad" / "must be an object") and pick turns settled
// without a vote — rounds were dropped for a schema gap, not for a model.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

const source = readFileSync(new URL("../../extension/tradeoffs-trace.ts", import.meta.url), "utf8");

function toolParameters(name: string): string {
  const at = source.indexOf(`name: "${name}"`);
  assert.ok(at >= 0, `${name} is registered`);
  const start = source.indexOf("parameters:", at);
  const end = source.indexOf("async execute", start);
  return source.slice(start, end);
}

test("submit_pick_vote's schema declares every field the conductor requires of a pick vote, loserHad as an object", () => {
  const params = toolParameters("submit_pick_vote");
  for (const field of ["round", "seat", "lane", "why"]) {
    assert.match(params, new RegExp(`\\b${field}:`), `the schema declares ${field}`);
  }
  assert.match(params, /loserHad:\s*Type\.Object\(/, "loserHad is declared as an object");
  assert.match(params, /yes:\s*Type\.Boolean\(\)/, "loserHad.yes is a boolean");
  assert.match(params, /anchors:\s*Type\.Array\(Type\.String\(\)\)/, "loserHad.anchors is a list of anchors");
});
