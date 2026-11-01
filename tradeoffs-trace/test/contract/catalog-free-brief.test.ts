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
    assert.match(brief.impact, /not established/i);
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


test("the registered submit_brief execute accepts a neutral brief and leaves catalog checks to the conductor", async () => {
  // Pi provides these UI/schema constructors at extension load time. Keep
  // their SDK surface local; execute still uses the production JSON schema,
  // brief validator and submission path, with no socket required.
  const { registerHooks } = await import("node:module");
  const sdk = {
    typebox: `export const Type = new Proxy({}, { get: (_, type) => (...args) => ({ type, args }) });`,
    "@earendil-works/pi-ai": `export const StringEnum = (...args) => ({ args });`,
    "@earendil-works/pi-coding-agent": `export const isToolCallEventType = () => false;`,
  };
  const hooks = registerHooks({
    resolve(specifier, context, next) {
      if (specifier in sdk) return { url: "data:text/javascript," + encodeURIComponent(sdk[specifier]), shortCircuit: true };
      return next(specifier, context);
    },
  });
  try {
    const { default: extension } = await import("../../extension/tradeoffs-trace.ts");
    const tools = new Map();
    extension({ registerTool: (tool) => tools.set(tool.name, tool), on() {} } as never);
    const tool = tools.get("submit_brief");
    const { noRecommendationReason, ...wire } = briefs.fallbackBrief(request, { catalogs: null });
    const model = { ...wire, today: "The parser refuses invalid strings.", impact: "Callers receive a useful error.",
      recommendation: { option: "stop", why: "Plan §3 requires an owner ruling." } };
    const result = await tool.execute("neutral-brief", model);
    assert.match(result.content[0].text, /cannot submit: no run socket/, "the brief reaches submitTool");
    assert.doesNotMatch(result.content[0].text, /invalid brief/);
    assert.match(tool.description, /when the plan supplies catalogs/i);
    const bad = await tool.execute("empty-impact", { ...model, impact: "" });
    assert.equal(bad.isError, true);
    assert.match(bad.content[0].text, /invalid/);
    // This is the same authoritative catalog-aware gate the conductor uses.
    assert.match(briefs.briefIssue(model, { catalogs: { products: [], calendars: [] } } as never) ?? "", /market stops publishing/);
  } finally { hooks.deregister(); }
});
