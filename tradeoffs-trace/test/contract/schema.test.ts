// Schema tests (phase-0 brief): each schema accepts a valid fixture and
// rejects at least a missing required field, an empty plain-language
// decision field, and a wrong enum value. Paths are resolved from this
// file's own URL, not process.cwd(), so these tests pass regardless of the
// caller's working directory.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";
import { validate } from "../../src/core/schema.ts";

function loadJSON(relPath: string): unknown {
  return JSON.parse(readFileSync(new URL(relPath, import.meta.url), "utf8"));
}

function schema(name: string) {
  return loadJSON(`../../schemas/${name}.schema.json`) as Record<string, unknown>;
}

function fixture(name: string) {
  return loadJSON(`../fixtures/${name}.json`);
}

test("schema: plan accepts a valid fixture", () => {
  const result = validate(schema("plan"), fixture("plan"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: plan rejects a missing required field (phases)", () => {
  const data = fixture("plan") as Record<string, unknown>;
  const { phases, ...rest } = data;
  const result = validate(schema("plan"), rest);
  assert.equal(result.valid, false);
});

test("schema: plan phase rejects a wrong enum-shaped value (provisional not boolean)", () => {
  const data = structuredClone(fixture("plan")) as { phases: { provisional: unknown }[] };
  data.phases[0].provisional = "yes";
  const result = validate(schema("plan"), data);
  assert.equal(result.valid, false);
});

test("schema: phase-contract accepts a valid fixture", () => {
  const result = validate(schema("phase-contract"), fixture("phase-contract"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: phase-contract rejects a missing required field (goal)", () => {
  const data = fixture("phase-contract") as Record<string, unknown>;
  const { goal, ...rest } = data;
  const result = validate(schema("phase-contract"), rest);
  assert.equal(result.valid, false);
});

test("schema: decision accepts a valid fixture", () => {
  const result = validate(schema("decision"), fixture("decision"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: decision rejects a missing required field (whyItMatters)", () => {
  const data = fixture("decision") as Record<string, unknown>;
  const { whyItMatters, ...rest } = data;
  const result = validate(schema("decision"), rest);
  assert.equal(result.valid, false);
});

test("schema: decision rejects an empty plain-language field (choice)", () => {
  const data = { ...(fixture("decision") as Record<string, unknown>), choice: "" };
  const result = validate(schema("decision"), data);
  assert.equal(result.valid, false);
});

test("schema: decision rejects a wrong enum value (class)", () => {
  const data = { ...(fixture("decision") as Record<string, unknown>), class: "irrelevant" };
  const result = validate(schema("decision"), data);
  assert.equal(result.valid, false);
});

test("schema: decision requires at least one alternative", () => {
  const data = { ...(fixture("decision") as Record<string, unknown>), alternatives: [] };
  const result = validate(schema("decision"), data);
  assert.equal(result.valid, false);
});

test("schema: finding accepts a valid fixture", () => {
  const result = validate(schema("finding"), fixture("finding"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: finding rejects a missing required field (evidence)", () => {
  const data = fixture("finding") as Record<string, unknown>;
  const { evidence, ...rest } = data;
  const result = validate(schema("finding"), rest);
  assert.equal(result.valid, false);
});

test("schema: finding rejects a wrong enum value (severity)", () => {
  const data = { ...(fixture("finding") as Record<string, unknown>), severity: "urgent" };
  const result = validate(schema("finding"), data);
  assert.equal(result.valid, false);
});

test("schema: owner-request accepts a valid fixture", () => {
  const result = validate(schema("owner-request"), fixture("owner-request"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: owner-request rejects a wrong enum value (status)", () => {
  const data = { ...(fixture("owner-request") as Record<string, unknown>), status: "ignored" };
  const result = validate(schema("owner-request"), data);
  assert.equal(result.valid, false);
});

test("schema: correction accepts a valid fixture", () => {
  const result = validate(schema("correction"), fixture("correction"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: correction rejects an empty plain-language field (correctionText)", () => {
  const data = { ...(fixture("correction") as Record<string, unknown>), correctionText: "" };
  const result = validate(schema("correction"), data);
  assert.equal(result.valid, false);
});

test("schema: ballot accepts a valid fixture", () => {
  const result = validate(schema("ballot"), fixture("ballot"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: ballot rejects a missing required field (evidence)", () => {
  const data = fixture("ballot") as Record<string, unknown>;
  const { evidence, ...rest } = data;
  const result = validate(schema("ballot"), rest);
  assert.equal(result.valid, false);
});

test("schema: ballot rejects evidence-free ballot (empty array)", () => {
  const data = { ...(fixture("ballot") as Record<string, unknown>), evidence: [] };
  const result = validate(schema("ballot"), data);
  assert.equal(result.valid, false);
});

test("schema: ballot rejects a wrong enum value (vote)", () => {
  const data = { ...(fixture("ballot") as Record<string, unknown>), vote: "maybe" };
  const result = validate(schema("ballot"), data);
  assert.equal(result.valid, false);
});

test("schema: review accepts a valid fixture", () => {
  const result = validate(schema("review"), fixture("review"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: review rejects a wrong enum value (correction status)", () => {
  const data = structuredClone(fixture("review")) as { correctionStatements: { status: unknown }[] };
  data.correctionStatements[0].status = "maybe";
  const result = validate(schema("review"), data);
  assert.equal(result.valid, false);
});

test("schema: owner-command accepts a valid steer fixture", () => {
  const result = validate(schema("owner-command"), fixture("owner-command-steer"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: owner-command rejects a missing required field (boundAttemptId)", () => {
  const data = fixture("owner-command-steer") as Record<string, unknown>;
  const { boundAttemptId, ...rest } = data;
  const result = validate(schema("owner-command"), rest);
  assert.equal(result.valid, false);
});

test("schema: owner-command rejects an unknown kind", () => {
  const data = { ...(fixture("owner-command-steer") as Record<string, unknown>), kind: "sabotage" };
  const result = validate(schema("owner-command"), data);
  assert.equal(result.valid, false);
});

test("schema: binding accepts a valid fixture", () => {
  const result = validate(schema("binding"), fixture("binding"));
  assert.equal(result.valid, true, result.errors.join("; "));
});

test("schema: binding rejects a missing required field (recordVersion)", () => {
  const data = fixture("binding") as Record<string, unknown>;
  const { recordVersion, ...rest } = data;
  const result = validate(schema("binding"), rest);
  assert.equal(result.valid, false);
});

test("schema: event accepts every known event type and rejects an unknown one", () => {
  const eventSchema = schema("event");
  const knownSamples: unknown[] = [
    { type: "ATTEMPT_STARTED" },
    { type: "SUBMIT_PHASE", disclosures: [] },
    { type: "FREEZE_COMPLETED", candidateSha: "C1", decisions: [] },
    { type: "REPAIR_ATTEMPT_STARTED" },
    { type: "RUN_RESUMED" },
    { type: "LAUNCH_FAILED", role: "worker", expected: ["read"], missing: [], extra: ["write"] },
  ];
  for (const sample of knownSamples) {
    const result = validate(eventSchema, sample);
    assert.equal(result.valid, true, `${JSON.stringify(sample)}: ${result.errors.join("; ")}`);
  }
  const unknown = validate(eventSchema, { type: "SOMETHING_ELSE" });
  assert.equal(unknown.valid, false);
});
