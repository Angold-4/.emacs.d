// 2026-10-08/10 runs: seat A (gpt-6.1-sol) sent "" for optional review fields
// (sameAs, linkedDecisionId, criterionDispute.*). The schema's minLength 1
// refused the whole review 186 times; six reviews repeated the same shape 9-13
// times until the seat's deadline, and five two-lane rounds were dropped.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import { dropEmptyStrings, validate, type JSONSchema } from "../../src/core/schema.ts";

const REVIEW = JSON.parse(
  readFileSync(new URL("../../schemas/review.schema.json", import.meta.url), "utf8"),
) as JSONSchema;

function review(finding: Record<string, unknown>) {
  return {
    reviewer: "A",
    phaseId: "p",
    candidateSha: "abc",
    contractVersion: { snapshot: 1, sectionSha256: "0".repeat(64) },
    correctionStatements: [],
    findingStatements: [],
    findings: [finding],
  };
}

test("empty optional review fields are treated as absent and the review validates", () => {
  const sent = review({
    kind: "defect",
    severity: "advisory",
    evidence: "src/a.ts:1 reads the wrong field",
    sameAs: "",
    linkedDecisionId: "",
    criterionDispute: { criterion: "", why: "", proposedWording: "" },
  });
  assert.equal(validate(REVIEW, sent).valid, false, "the raw shape is what the schema refused");
  const cleaned = dropEmptyStrings(sent);
  assert.deepEqual(validate(REVIEW, cleaned).errors, []);
  const finding = (cleaned as { findings: Record<string, unknown>[] }).findings[0];
  assert.deepEqual(Object.keys(finding).sort(), ["evidence", "kind", "severity"]);
});

test("an empty required field is still refused, as missing", () => {
  const cleaned = dropEmptyStrings(review({ kind: "defect", severity: "blocking", evidence: "" }));
  const result = validate(REVIEW, cleaned);
  assert.equal(result.valid, false);
  assert.match(result.errors.join("; "), /evidence/);
});

test("a partly filled optional object keeps its content and is still checked", () => {
  const cleaned = dropEmptyStrings(
    review({
      kind: "contract",
      severity: "blocking",
      evidence: "x",
      criterionDispute: { criterion: "R1", why: "", proposedWording: "" },
    }),
  );
  assert.equal(validate(REVIEW, cleaned).valid, false, "criterionDispute without why is not silently dropped");
});

test("every submit tool normalizes its arguments before validating or sending", () => {
  const source = readFileSync(new URL("../../extension/tradeoffs-trace.ts", import.meta.url), "utf8");
  const tools = [...source.matchAll(/name: "(submit_[a-z_]+)"/g)].map((m) => m[1]);
  assert.ok(tools.length >= 9);
  for (const tool of tools) {
    const at = source.indexOf(`name: "${tool}"`);
    const body = source.slice(source.indexOf("async execute", at), source.indexOf("pi.registerTool", at + 1));
    assert.match(body, /dropEmptyStrings\(params\)/, `${tool} normalizes its arguments`);
  }
});
