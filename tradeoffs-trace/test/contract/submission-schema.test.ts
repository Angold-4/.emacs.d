// Phase-0 review, round 1, item 1: a single source of truth for the
// submission tools' model-facing shapes. This test asserts that:
//
//  1. decision.schema.json's plain-language fields (choice, whyItMatters,
//     alternatives, recommendation — design §3.2) are, after resolving
//     $refs, structurally identical to submission.schema.json's
//     $defs.decisionDisclosure — the shape extension/tradeoffs-trace.ts
//     actually validates submit_phase/submit_discovery against.
//  2. Each submission tool's typebox parameter schema has the same
//     top-level required keys and property names as its JSON schema.
//
// typebox is only resolvable inside Pi's own extension loader (jiti) — a
// plain `node --test` run in this package cannot import it (there is no
// node_modules here; see README's zero-npm-deps note), so importing
// extension/tradeoffs-trace.ts directly would fail at module load. Confirmed
// empirically: `node -e "import('typebox')"` from this package's directory
// fails with ERR_MODULE_NOT_FOUND. So this test imports the plain-data field
// lists from extension/param-shapes.ts instead (no typebox import there at
// all) — tradeoffs-trace.ts builds each tool's real `Type.Object(...)` by
// mapping over those exact same arrays (see its comments), so checking the
// arrays here is checking the real parameter schema's key set, not a
// separately-maintained parallel description of it.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import {
  DECISION_DISCLOSURE_PARAMS,
  SUBMIT_DISCOVERY_PARAMS,
  SUBMIT_PHASE_PARAMS,
  SUBMIT_REVIEW_PARAMS,
} from "../../extension/param-shapes.ts";

function loadJSON(relPath: string): Record<string, unknown> {
  return JSON.parse(readFileSync(new URL(relPath, import.meta.url), "utf8")) as Record<string, unknown>;
}

const decisionSchema = loadJSON("../../schemas/decision.schema.json");
const submissionSchema = loadJSON("../../schemas/submission.schema.json");
const reviewSchema = loadJSON("../../schemas/review.schema.json");

/** Recursively resolves local `#/$defs/...` refs against `root`, so two
 * schemas that spell the same shape via differently-named (but
 * content-identical) $defs still compare equal. */
function resolve(node: unknown, root: Record<string, unknown>): unknown {
  if (Array.isArray(node)) return node.map((n) => resolve(n, root));
  if (node && typeof node === "object") {
    const obj = node as Record<string, unknown>;
    if (typeof obj.$ref === "string") {
      const prefix = "#/$defs/";
      assert.ok(obj.$ref.startsWith(prefix), `unsupported $ref: ${obj.$ref}`);
      const name = obj.$ref.slice(prefix.length);
      const defs = root.$defs as Record<string, unknown> | undefined;
      const target = defs?.[name];
      assert.ok(target, `$ref target not found: ${obj.$ref}`);
      return resolve(target, root);
    }
    const out: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(obj)) out[k] = resolve(v, root);
    return out;
  }
  return node;
}

// The design §3.2 plain-language fields — the ones a worker/reviewer can
// actually supply, as opposed to id/version/phaseId/source/class/
// boundCandidateSha/boundContractVersion, which the conductor assigns once
// a candidate exists (see README's phase-1 note).
const PLAIN_LANGUAGE_FIELDS = ["choice", "whyItMatters", "alternatives", "recommendation"];

test("submission-schema: decision.schema.json's plain-language fields are required", () => {
  const required = decisionSchema.required as string[];
  for (const field of PLAIN_LANGUAGE_FIELDS) {
    assert.ok(required.includes(field), `decision.schema.json must require '${field}'`);
  }
});

test("submission-schema: decisionDisclosure requires the plain-language fields, plus classProposal", () => {
  const decisionDisclosure = (submissionSchema.$defs as Record<string, { required: string[] }>).decisionDisclosure;
  for (const field of PLAIN_LANGUAGE_FIELDS) {
    assert.ok(decisionDisclosure.required.includes(field), `decisionDisclosure must require '${field}'`);
  }
  assert.ok(decisionDisclosure.required.includes("classProposal"));
  assert.deepEqual([...decisionDisclosure.required].sort(), [...PLAIN_LANGUAGE_FIELDS, "classProposal"].sort());
});

for (const field of PLAIN_LANGUAGE_FIELDS) {
  test(`submission-schema: decision.schema.json.properties.${field} equals submission.schema.json's decisionDisclosure.properties.${field} (refs resolved)`, () => {
    const decisionProp = (decisionSchema.properties as Record<string, unknown>)[field];
    const decisionDisclosure = (submissionSchema.$defs as Record<string, { properties: Record<string, unknown> }>).decisionDisclosure;
    const disclosureProp = decisionDisclosure.properties[field];
    assert.deepEqual(resolve(decisionProp, decisionSchema), resolve(disclosureProp, submissionSchema));
  });
}

test("submission-schema: decisionDisclosure's own Alternative/Recommendation $defs equal decision.schema.json's", () => {
  const decisionDefs = decisionSchema.$defs as Record<string, unknown>;
  const submissionDefs = submissionSchema.$defs as Record<string, unknown>;
  assert.deepEqual(resolve(decisionDefs.Alternative, decisionSchema), resolve(submissionDefs.Alternative, submissionSchema));
  assert.deepEqual(resolve(decisionDefs.Recommendation, decisionSchema), resolve(submissionDefs.Recommendation, submissionSchema));
});

// --- typebox parameter shape vs. JSON schema, per submission tool --------

function assertShapeMatchesSchema(name: string, shape: { properties: string[]; required: string[] }, schemaNode: { properties: Record<string, unknown>; required: string[] }): void {
  test(`submission-schema: ${name}'s typebox parameter shape matches its JSON schema exactly`, () => {
    assert.deepEqual([...shape.properties].sort(), Object.keys(schemaNode.properties).sort(), `${name}: property names must match`);
    assert.deepEqual([...shape.required].sort(), [...schemaNode.required].sort(), `${name}: required keys must match`);
  });
}

const submissionDefs = submissionSchema.$defs as Record<string, { properties: Record<string, unknown>; required: string[] }>;

assertShapeMatchesSchema("decisionDisclosure", DECISION_DISCLOSURE_PARAMS, submissionDefs.decisionDisclosure);
assertShapeMatchesSchema("submit_phase", SUBMIT_PHASE_PARAMS, submissionDefs.submitPhase);
assertShapeMatchesSchema("submit_discovery", SUBMIT_DISCOVERY_PARAMS, submissionDefs.submitDiscovery);
assertShapeMatchesSchema("submit_review", SUBMIT_REVIEW_PARAMS, reviewSchema as unknown as { properties: Record<string, unknown>; required: string[] });
