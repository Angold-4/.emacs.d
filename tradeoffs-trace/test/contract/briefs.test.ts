// Decision briefs: the golden from atlas 15d's three open owner items
// (F-M-9, D-A-80, D-B-82). Each renders as a brief whose question has no
// code identifier, whose `today` example names a real product and session
// time consistent with the fixture's calendars.yaml/products.yaml, whose
// `impact` says whether any market stops publishing, and whose options map
// one-to-one to the request's own option ids. The same briefs are checked by
// briefIssue (the evaluator's submit_brief gate).

import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import {
  briefIssue,
  checkTodayExample,
  fallbackBrief,
  fallbackDecisionBrief,
  hasCodeIdentifier,
  impactAnswersPublishing,
  parseCatalogs,
  relatedOpenItems,
  renderBriefsSection,
  briefCommandFor,
  briefResolveCommand,
  resolveCommandFor,
  type Catalogs,
} from "../../src/core/briefs.ts";
import { normalizeDecisionViewCommand } from "../../src/core/owner-inbox.ts";
import type { DecisionBrief, OwnerRequest } from "../../src/core/types.ts";

const FIXTURE = fileURLToPath(new URL("../fixtures/briefs/atlas-15d", import.meta.url));
const GOLDEN = fileURLToPath(new URL("../fixtures/briefs/atlas-15d/golden.org", import.meta.url));

interface Fixture {
  run: string;
  phase: string;
  candidateSha: string;
  contractVersion: { snapshot: number; sectionSha256: string };
  requests: OwnerRequest[];
  openItems: Array<{ id: string; question: string; files?: string[]; planRefs?: string[] }>;
  briefs: DecisionBrief[];
}

function fixture(): Fixture {
  return JSON.parse(readFileSync(`${FIXTURE}/atlas-15d.json`, "utf8")) as Fixture;
}

function catalogs(): Catalogs {
  return parseCatalogs(readFileSync(`${FIXTURE}/calendars.yaml`, "utf8"), readFileSync(`${FIXTURE}/products.yaml`, "utf8"));
}

function golden(rel: string, actual: string): void {
  const file = `${GOLDEN.slice(0, GOLDEN.lastIndexOf("/"))}/${rel}`;
  if (process.env.TT_UPDATE_GOLDEN === "1" || !existsSync(file)) {
    mkdirSync(file.slice(0, file.lastIndexOf("/")), { recursive: true });
    writeFileSync(file, actual);
    return;
  }
  assert.equal(actual, readFileSync(file, "utf8"));
}

test("atlas 15d: every brief passes the tool gate against its own catalogs and request options", () => {
  const f = fixture();
  const cats = catalogs();
  for (const brief of f.briefs) {
    const request = f.requests.find((r) => r.id === brief.requestId)!;
    assert.ok(request, `no request for ${brief.requestId}`);
    const issue = briefIssue(brief, { requestOptions: request.options.map((o) => o.id), catalogs: cats });
    assert.equal(issue, undefined, `${brief.requestId}: ${issue}`);
    assert.equal(hasCodeIdentifier(brief.question), false, `${brief.requestId} question names code`);
    assert.equal(impactAnswersPublishing(brief.impact), true, `${brief.requestId} impact does not answer publishing`);
    assert.deepEqual(
      brief.options.map((o) => o.id).sort(),
      request.options.map((o) => o.id).sort(),
      `${brief.requestId} option ids do not map one-to-one`,
    );
  }
});

test("atlas 15d: each today example names a real product and session time from the catalogs", () => {
  const f = fixture();
  const cats = catalogs();
  for (const brief of f.briefs) {
    const check = checkTodayExample(brief.today, cats);
    assert.equal(check.ok, true, `${brief.requestId}: ${check.reason}`);
    assert.ok(check.product, `${brief.requestId} names no product`);
    assert.ok(check.time, `${brief.requestId} names no time`);
    assert.ok(cats.calendars[check.calendar!], `${brief.requestId} names an unknown calendar`);
  }
});

test("atlas 15d: the three briefs render to the golden file", () => {
  const f = fixture();
  const section = renderBriefsSection(f.briefs, {
    requestFor: (id) => f.requests.find((r) => r.id === id),
    binding: {
      runId: f.run,
      phaseId: f.phase,
      candidateSha: f.candidateSha,
      recordVersion: 1,
      contractVersion: f.contractVersion,
    },
  });
  golden("golden.org", `${section.join("\n")}\n`);
});

test("related lists an open item on the same file or plan clause", () => {
  const f = fixture();
  const byId = (id: string) => f.openItems.find((i) => i.id === id)!;
  const related = relatedOpenItems(byId("F-M-9"), f.openItems);
  assert.deepEqual(related.map((r) => r.id).sort(), ["D-A-80", "T-54"]);
  // A different concern (publish.rs / IC §7) is not brought in.
  assert.equal(related.some((r) => r.id === "D-B-82"), false);
  // The bigger silence on the same clause is listed, so it is never hidden.
  assert.ok(related.some((r) => r.id === "T-54"));
});

test("resolving from a brief writes the identical resolve command as resolving the request", () => {
  const f = fixture();
  const brief = f.briefs.find((b) => b.requestId === "F-M-9")!;
  const request = f.requests.find((r) => r.id === "F-M-9")!;
  const binding = {
    runId: f.run,
    phaseId: f.phase,
    candidateSha: f.candidateSha,
    recordVersion: 1,
    contractVersion: f.contractVersion,
  };
  // The literal decision-view encoding the existing request resolve writes.
  const expected = {
    type: "resolve",
    option: "repair",
    binding: {
      runId: f.run,
      phaseId: f.phase,
      recordId: "F-M-9",
      candidateSha: f.candidateSha,
      recordVersion: 1,
      contractVersion: { snapshot: 4, sectionSha256: "a1b2c3d4" },
    },
  };
  assert.deepEqual(briefCommandFor(brief, "repair", binding), expected);
  assert.deepEqual(briefResolveCommand(brief, "repair", binding), expected);
  assert.deepEqual(resolveCommandFor(request, "repair", binding), expected);
  // Both paths normalize to the same core event.
  const fromBrief = normalizeDecisionViewCommand(briefCommandFor(brief, "repair", binding), "cmd-brief");
  const fromRequest = normalizeDecisionViewCommand(resolveCommandFor(request, "repair", binding), "cmd-brief");
  assert.equal(fromBrief.ok, true);
  assert.equal(fromRequest.ok, true);
  if (fromBrief.ok && fromRequest.ok) assert.deepEqual(fromBrief.event, fromRequest.event);
});

test("a reserved decision's override brief sends an override, not a resolve", () => {
  const f = fixture();
  const brief = fallbackDecisionBrief({ id: "D-A-80", choice: "widen the re-entry band to ten seconds", whyItMatters: "fewer stale rejoins" });
  const binding = { runId: f.run, phaseId: f.phase, candidateSha: f.candidateSha, recordVersion: 2, contractVersion: f.contractVersion };
  const command = briefCommandFor(brief, "reject_and_repair", binding);
  assert.equal(command.type, "override");
  assert.equal(command.vote, "reject");
  assert.equal((command.binding as { recordId: string }).recordId, "D-A-80");
  assert.equal((command.binding as { recordVersion: number }).recordVersion, 2);
  // `approve` maps to an approve override.
  assert.equal((briefCommandFor(brief, "approve", binding) as { vote: string }).vote, "approve");
  // The override brief passes the same gate with its own option ids.
  assert.equal(briefIssue(brief, { requestOptions: ["approve", "reject_and_repair"] }), undefined);
});

test("a today that names a market and time cannot excuse itself with the unverified marker", () => {
  const good = fixture().briefs[0];
  const wrong = { ...good, today: "Pyth's NVDA product reopens Monday 09:31 ET. (example unverified)" };
  const issue = briefIssue(wrong, { catalogs: catalogs() });
  assert.ok(issue, "a wrong time must be refused even with the marker");
  assert.match(issue!, /cannot be checked|not a .* session boundary/);
  const noExample = { ...good, today: "No concrete example was recorded. (example unverified)" };
  assert.equal(briefIssue(noExample, { catalogs: catalogs() }), undefined);
});

test("the deterministic backstop lists related items on the same concern and never asserts an unchecked impact", () => {
  const f = fixture();
  const concerns = f.openItems;
  const request = f.requests.find((r) => r.id === "F-M-9")!;
  const brief = fallbackBrief(request, { catalogs: null, allItems: concerns, files: ["src/core/blend.rs"], planRefs: ["IC §5"] });
  assert.equal(impactAnswersPublishing(brief.impact), true);
  assert.match(brief.impact, /not established/i);
  assert.deepEqual(brief.related.map((r) => r.id).sort(), ["D-A-80", "T-54"]);
  assert.equal(briefIssue(brief, { requestOptions: request.options.map((o) => o.id) }), undefined);
});

test("briefIssue rejects a code identifier in the question", () => {
  const good = fixture().briefs[0];
  for (const bad of ["Should within_band_active stay on?", "Should src/core/blend.rs change?", "Is `excluded_before` right?"]) {
    const issue = briefIssue({ ...good, question: bad });
    assert.ok(issue, `expected a rejection for ${bad}`);
    assert.match(issue!, /code|plain/i);
  }
});

test("briefIssue rejects a time, count or duration without an evidence citation", () => {
  const good = fixture().briefs[0];
  const noCitation = { ...good, evidence: ["message: F-M-9 reopened the finding"] };
  // Each of the three quantified forms in turn: with a config/code citation
  // the same text passes; without one it is refused.
  for (const field of [
    { today: "Pyth's NVDA product reopens Sunday 20:00 ET and the vendor waits 10 s." },
    { impact: "No market stops publishing; 3 markets keep their full vendor count." },
    { impact: "No market stops publishing; the wait lasts two minutes." },
  ]) {
    const bad = { ...noCitation, ...field };
    const issue = briefIssue(bad);
    assert.ok(issue, `expected a rejection for ${JSON.stringify(field)}`);
    assert.match(issue!, /citation|config|code/i);
    const withCitation = { ...bad, evidence: [...bad.evidence, "config: calendars.yaml us_equity"] };
    assert.equal(briefIssue(withCitation), undefined, `${JSON.stringify(field)} should pass with a citation`);
  }
});

test("briefIssue rejects an impact that does not answer whether a market stops publishing", () => {
  const good = fixture().briefs[0];
  const issue = briefIssue({ ...good, impact: "The price is formed from fewer vendors for a short while." });
  assert.ok(issue);
  assert.match(issue!, /stop[s]? publishing/i);
});

test("the deterministic backstop brief passes the same gate without inventing an example", () => {
  const f = fixture();
  for (const request of f.requests) {
    const brief = fallbackBrief(request, { catalogs: null });
    assert.equal(briefIssue(brief, { requestOptions: request.options.map((o) => o.id) }), undefined);
    assert.equal(briefQuestionHasNoCode(brief.question), true);
    assert.equal(impactAnswersPublishing(brief.impact), true);
    assert.match(brief.today, /example unverified/);
    assert.deepEqual(brief.options.map((o) => o.id).sort(), request.options.map((o) => o.id).sort());
  }
});

function briefQuestionHasNoCode(question: string): boolean {
  return !hasCodeIdentifier(question);
}

test("a brief whose example cannot be checked says so instead of inventing one", () => {
  const good = fixture().briefs[0];
  const invented = { ...good, today: "A market reopens at 12:34 on a day that is not in any calendar." };
  const issue = briefIssue(invented, { catalogs: catalogs() });
  assert.ok(issue);
  assert.match(issue!, /cannot be checked/i);
  const honest = { ...invented, today: "No example could be checked against the plan's calendars. (example unverified)" };
  assert.equal(briefIssue(honest, { catalogs: catalogs() }), undefined);
});
