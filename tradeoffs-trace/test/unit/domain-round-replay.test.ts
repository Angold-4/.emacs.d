import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import * as fs from "node:fs";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import { validate } from "../../src/core/schema.ts";
import { baseState } from "./helpers.ts";

const laneEvents = [
  { type: "ROUND_STARTED", round: 1, base: "B", lanes: ["a", "b"] },
  { type: "CANDIDATE_SUBMITTED", round: 1, lane: "a", sha: "C" },
  { type: "CANDIDATE_CHECKED", round: 1, lane: "a", ok: true },
  { type: "CANDIDATE_PICKED", round: 1, lane: "a", sha: "C", votes: 3 },
  { type: "ROUND_FIXED", round: 1, base: "C", tasks: [{ domain: "core", findingIds: ["F1"] }], merges: [], candidate: "D" },
];

test("a lane-free ROUND_STARTED and the existing fixture log validate against the event schema", () => {
  const schema = JSON.parse(fs.readFileSync(new URL("../../schemas/event.schema.json", import.meta.url), "utf8"));
  const rows = fs.readFileSync(new URL("../fixtures/runs/05-era/events.jsonl", import.meta.url), "utf8").trim().split("\n").map(JSON.parse);
  const events = [
    ...rows.filter((r) => r.kind === "event").map((r) => r.event),
    ...laneEvents,
    { type: "ROUND_STARTED", round: 2, base: "C", lanes: [] },
  ];
  for (const event of events) {
    const result = validate(schema, event);
    assert.equal(result.valid, true, `${event.type}: ${result.errors.join("; ")}`);
  }
});

test("an existing fixture and legacy lane-only rounds replay to the pre-change state", () => {
  let state = baseState();
  const rows = fs.readFileSync(new URL("../fixtures/runs/05-era/events.jsonl", import.meta.url), "utf8").trim().split("\n").map(JSON.parse);
  for (const row of rows) if (row.kind === "event") state = reduce(state, row.event).state;
  for (const event of laneEvents) {
    const result = reduce(state, event);
    assert.equal(result.ok, true, result.reason);
    state = result.state;
  }
  // Full-state digest measured with b4c2f0a's reducer on this same existing
  // fixture and legacy lane-round sequence; no new empty-lane event enters it.
  assert.equal(createHash("sha256").update(JSON.stringify(state)).digest("hex"),
    "bf9df04eb11524b56a97a7ceccfde0381581bf5a038b4b21025baeb286dc22d1");
});

test("a lane-free loop checkpoint replays its base and integrated candidate without a pick", () => {
  let state = baseState({ phase: "IMPLEMENTING" });
  const events = [
    { type: "ROUND_STARTED", round: 2, base: "PICK", lanes: [] },
    { type: "ROUND_FIXED", round: 2, base: "PICK", tasks: [{ domain: "core", findingIds: ["F1"] }], merges: [{ sha: "DOMAIN" }], candidate: "INTEGRATED" },
  ];
  for (const event of events) {
    const result = reduce(state, event);
    assert.equal(result.ok, true, result.reason);
    state = result.state;
  }
  const replayed = events.reduce((s, e) => reduce(s, e).state, baseState({ phase: "IMPLEMENTING" }));
  assert.deepEqual(replayed, state);
  const checkpoint = replayed.phase.rounds![0];
  assert.equal(checkpoint.base, "PICK");
  assert.equal(checkpoint.fixed?.candidate, "INTEGRATED");
  assert.equal(checkpoint.picked, undefined);
  assert.deepEqual(checkpoint.lanes, []);
  assert.deepEqual(checkpoint.candidates, []);
});
