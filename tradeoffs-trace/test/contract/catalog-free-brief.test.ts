import assert from "node:assert/strict";
import { test } from "node:test";
import * as briefs from "../../src/core/briefs.ts";
import { validate } from "../../src/core/schema.ts";
import { readFileSync } from "node:fs";
import type { OwnerRequest } from "../../src/core/types.ts";

const request: OwnerRequest = {
  id: "OR-1", phaseId: "p1", version: 1, origin: "repair_budget_exhausted", status: "open",
  reason: "the string parser needs an owner ruling on invalid input",
  options: [{ id: "stop", label: "Stop the phase" }],
};
const schema = JSON.parse(readFileSync(new URL("../../schemas/decision-brief.schema.json", import.meta.url), "utf8"));
const trading = /\b(?:product|products|market|markets|vendor|vendors|calendar|calendars|publishing|reopen)\b|session.time|example unverified/i;

test("a brief for a plan without catalogs carries no product or market text", () => {
  const backstops = [
    briefs.fallbackBrief(request, { catalogs: null }),
    briefs.fallbackDecisionBrief({ id: "D-1", choice: "reject invalid strings" }, { catalogs: null }),
    briefs.fallbackEntryBrief({ id: "E-1", title: "reject invalid strings" }, { catalogs: null }),
  ];
  for (const brief of backstops) {
    assert.doesNotMatch(briefs.renderBriefOrg(brief), trading);
    assert.equal(briefs.briefIssue(brief, { catalogs: null, allowUnverifiedImpact: true }), undefined);
    const { noRecommendationReason, ...wire } = brief;
    const result = validate(schema, wire);
    assert.equal(result.valid, true, result.errors.join("; "));
  }
  assert.doesNotMatch(briefs.briefInstructions(null).join("\n"), trading);
  const model = {
    ...backstops[0], today: "The parser refuses invalid strings.", impact: "Callers receive a useful error.",
    recommendation: { option: "stop", why: "Plan §3 requires an owner ruling." },
  };
  assert.equal(briefs.briefIssue(model, { catalogs: null, requestOptions: ["stop"] }), undefined);
  // General brief requirements and evidence rules remain enforced.
  assert.ok(briefs.briefIssue({ ...model, today: "" }, { catalogs: null }));
  assert.ok(briefs.briefIssue({ ...model, impact: "" }, { catalogs: null }));
  assert.ok(briefs.briefIssue({ ...model, today: "The parser waits 10 s." }, { catalogs: null }));
  assert.ok(briefs.briefIssue({ ...model, recommendation: undefined }, { catalogs: null }));
});
