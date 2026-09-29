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
  renderBriefOrg,
  enrichBriefRelated,
  fallbackBrief,
  fallbackDecisionBrief,
  fallbackEntryBrief,
  hasCodeIdentifier,
  impactAnswersPublishing,
  impactUnestablished,
  parseCatalogs,
  relatedOpenItems,
  renderBriefsSection,
  briefCommandFor,
  briefResolveCommand,
  resolveCommandFor,
  type Catalogs,
} from "../../src/core/briefs.ts";
import { normalizeDecisionViewCommand } from "../../src/core/owner-inbox.ts";
import { renderEntryReview } from "../../src/core/entries.ts";
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
  assert.equal(briefIssue(brief, { requestOptions: ["approve", "reject_and_repair"], allowUnverifiedImpact: true }), undefined);
});

test("a weekday reopen claim is checked against the calendar's weekly schedule", () => {
  const good = fixture().briefs[0];
  // 09:30 is a real us_equity session boundary, so only the weekday check can
  // reject the exact wrong example the goal cites (finding M-10).
  const monday = { ...good, today: "Pyth's NVDA product reopens Monday 09:30 ET (config: calendars.yaml us_equity opens Sun 20:00)." };
  const issue = briefIssue(monday, { catalogs: catalogs() });
  assert.ok(issue, "a Monday reopen must be refused when the calendar reopens Sunday");
  assert.match(issue!, /weekly reopen|cannot be checked/);
  const sunday = { ...good, today: "Pyth's NVDA product reopens Sunday 20:00 ET (config: calendars.yaml us_equity opens Sun 20:00)." };
  assert.equal(briefIssue(sunday, { catalogs: catalogs() }), undefined);
  // A close is not a reopen: Friday 17:00 must not be read as one.
  const closes = { ...good, today: "Kaiko's XAUUSD product closes Friday 17:00 ET (config: calendars.yaml metal_otc sessions 18:00-17:00)." };
  assert.equal(briefIssue(closes, { catalogs: catalogs() }), undefined);
});

test("the backstop omits a recommendation and does not recommend the risky first option", () => {
  const f = fixture();
  const request = f.requests.find((r) => r.id === "F-M-9")!;
  const brief = fallbackBrief(request, { catalogs: null });
  assert.equal(brief.recommendation, undefined);
  assert.equal(briefIssue(brief, { requestOptions: request.options.map((o) => o.id), allowUnverifiedImpact: true }), undefined);
  // The model briefs still carry one.
  assert.ok(f.briefs[0].recommendation);
});

test("a live entry's brief sends an entry verdict, and the conductor merges related", () => {
  const f = fixture();
  const entry = { id: "E-1", title: "widen the re-entry band", messages: [{ id: "T-9", title: "widen the band" }] };
  const brief = fallbackEntryBrief(entry, { allItems: f.openItems, files: ["src/core/blend.rs"], planRefs: ["IC §5"] });
  assert.equal(brief.command, "entry");
  assert.deepEqual(brief.options.map((o) => o.id), ["accept", "refuse"]);
  assert.equal(brief.recommendation, undefined);
  assert.equal(briefIssue(brief, { requestOptions: ["accept", "refuse"], allowUnverifiedImpact: true }), undefined);
  assert.ok(brief.related.some((r) => r.id === "T-54"));

  const request = f.requests.find((r) => r.id === "F-M-9")!;
  const concerns = f.openItems.map((c) => ({ ...c }));
  const model = { ...f.briefs[0], related: [{ id: "T-54", question: "held" }] };
  const merged = enrichBriefRelated(model, [...concerns, { id: request.id, question: request.reason, files: ["src/core/blend.rs"], planRefs: ["IC §5"] }]);
  assert.ok(merged.related.some((r) => r.id === "D-A-80"), "the conductor's same-file item is merged in");
});

test("renderBriefOrg renders a backstop brief whose recommendation is omitted", () => {
  const f = fixture();
  const request = f.requests.find((r) => r.id === "F-M-9")!;
  const brief = fallbackBrief(request, { catalogs: null });
  assert.equal(brief.recommendation, undefined);
  const org = renderBriefOrg(brief);
  assert.match(org, /\*\* Should this stay as it is/);
  assert.match(org, /Recommendation: none established/);
  assert.match(org, /\[accept_risk\]/);
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
  assert.equal(impactUnestablished(brief.impact), true);
  assert.equal(impactAnswersPublishing(brief.impact), false, "the strict model check refuses the backstop wording");
  assert.deepEqual(brief.related.map((r) => r.id).sort(), ["D-A-80", "T-54"]);
  assert.equal(briefIssue(brief, { requestOptions: request.options.map((o) => o.id), allowUnverifiedImpact: true }), undefined);
});

test("a model brief without a recommendation, or without a plan/IC citation, is refused", () => {
  const good = fixture().briefs[0];
  const without = { ...good, recommendation: undefined };
  const issue = briefIssue(without);
  assert.ok(issue, "a model brief must recommend one option");
  assert.match(issue!, /recommended option/i);
  // The backstop's mode allows the omission.
  assert.equal(briefIssue(without, { allowUnverifiedImpact: true }), undefined);
  const uncited = { ...good, recommendation: { option: "repair", why: "it seems better" } };
  const issue2 = briefIssue(uncited);
  assert.ok(issue2, "the recommendation must cite the plan or an IC section");
  assert.match(issue2!, /cite the plan or an IC/i);
});

test("the conductor forces a model brief's command to its item class", () => {
  // The briefIssue gate cannot see the item, so the conductor overwrites the
  // command: a reserved decision's brief must be an override, not a resolve.
  const good = fixture().briefs[0];
  const reserved = fallbackDecisionBrief({ id: "D-A-80", choice: "widen the band" });
  assert.equal(reserved.command, "override");
  assert.equal(good.command, undefined);
});

test("only the backstop may say the impact was not established", () => {
  const f = fixture();
  const model = { ...f.briefs[0], impact: "Whether any market stops publishing is not established." };
  const issue = briefIssue(model);
  assert.ok(issue, "a model brief must answer whether a market stops publishing");
  assert.match(issue!, /stops publishing/i);
  assert.equal(briefIssue(model, { allowUnverifiedImpact: true }), undefined);
});

test("the glossary terms T_in and T_out are allowed in a question", () => {
  const good = fixture().briefs[0];
  assert.equal(briefIssue({ ...good, question: "Should the band use T_in and T_out as edge values?" }), undefined);
});

test("a brief written for another candidate is not shown as current", () => {
  const f = fixture();
  const request = f.requests.find((r) => r.id === "F-M-9")!;
  const stale = { ...f.briefs[0], candidateSha: "old-sha" };
  const hidden = renderEntryReview({ messages: [], entries: [], ownerRequests: [request], briefs: [stale], newestCandidateSha: "new-sha" });
  assert.ok(!hidden.includes("Should a vendor excluded"), "a stale brief must not read as current");
  const current = renderEntryReview({
    messages: [],
    entries: [],
    ownerRequests: [request],
    briefs: [{ ...stale, candidateSha: "new-sha" }],
    newestCandidateSha: "new-sha",
  });
  assert.ok(current.includes("Should a vendor excluded"), "the current candidate's brief shows");
});

test("a claim's own citation is required, and plan: or a bare file name does not count", () => {
  const good = fixture().briefs[0];
  const uncited = { ...good, today: "Pyth's NVDA product reopens Sunday 20:00 ET." };
  const issue = briefIssue(uncited);
  assert.ok(issue, "the 20:00 claim has no inline citation");
  assert.match(issue!, /citation|config|code/i);
  const planOnly = { ...good, today: "Pyth's NVDA product reopens Sunday 20:00 ET (plan: IC §5)." };
  assert.ok(briefIssue(planOnly), "plan: is not the config or code read");
  const bare = { ...good, today: "Pyth's NVDA product reopens Sunday 20:00 ET (calendars.yaml)." };
  assert.ok(briefIssue(bare), "a bare file name is not a code citation");
  const cited = { ...good, today: "Pyth's NVDA product reopens Sunday 20:00 ET (config: calendars.yaml us_equity opens Sun 20:00)." };
  assert.equal(briefIssue(cited), undefined);
});

test("briefIssue rejects a code identifier in the question", () => {
  const good = fixture().briefs[0];
  for (const bad of ["Should within_band_active stay on?", "Should src/core/blend.rs change?", "Is `excluded_before` right?"]) {
    const issue = briefIssue({ ...good, question: bad });
    assert.ok(issue, `expected a rejection for ${bad}`);
    assert.match(issue!, /code|plain/i);
  }
});

test("each time, count or duration needs its own citation", () => {
  const good = fixture().briefs[0];
  // One inline citation does not cover a second, unrelated claim.
  const twoClaims = {
    ...good,
    today: "Pyth's NVDA product reopens Sunday 20:00 ET (config: calendars.yaml us_equity opens Sun 20:00). The vendor then waits 10 s.",
  };
  const issue = briefIssue(twoClaims);
  assert.ok(issue, "the 10 s claim is not covered by the 20:00 citation");
  assert.match(issue!, /10 s|citation/i);
  const bothCited = {
    ...good,
    today: "Pyth's NVDA product reopens Sunday 20:00 ET (config: calendars.yaml us_equity opens Sun 20:00). The vendor then waits 10 s (code: src/core/blend.rs:88).",
  };
  assert.equal(briefIssue(bothCited), undefined);
  // Each quantified form with its own inline citation passes; without it the
  // same claim is refused.
  for (const value of [
    "No market stops publishing; the wait lasts 3 minutes (code: src/core/blend.rs:41).",
    "No market stops publishing; 3 markets keep their vendor count (config: calendars.yaml us_equity).",
    "No market stops publishing; the wait lasts two minutes (code: src/core/blend.rs:41).",
  ]) {
    assert.equal(briefIssue({ ...good, impact: value }), undefined, `${value} should pass with its inline citation`);
    const uncited = value.replace(/\s*\((?:config|code):[^)]*\)/, "");
    assert.ok(briefIssue({ ...good, impact: uncited }), `${uncited} should be refused`);
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
    assert.equal(briefIssue(brief, { requestOptions: request.options.map((o) => o.id), allowUnverifiedImpact: true }), undefined);
    assert.equal(briefQuestionHasNoCode(brief.question), true);
    assert.equal(impactUnestablished(brief.impact), true);
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
