import assert from "node:assert/strict";
import { test } from "node:test";
import { readFileSync } from "node:fs";
import { validate } from "../../src/core/schema.ts";
import { applyEntryEvent } from "../../src/core/entries.ts";
import { repeatedItemStall } from "../../src/core/stuck.ts";
import { reduce } from "../../src/core/reduce.ts";
import { next } from "../../src/core/next.ts";
import { roundCeiling, repairBudgetRemains } from "../../src/core/rounds.ts";
import { nodePlan } from "../../src/core/program.ts";
import { lintPlan, lintProgram } from "../../src/core/plan-lint.ts";
import { parseOrgPlan } from "../../src/core/org-plan.ts";
import { initialState } from "../../src/conductor.ts";
import { answeringCommand, waitReason } from "../../src/notify.ts";
import { approvingReview, baseState, makeMessage } from "./helpers.ts";
import type { Event, PhaseState, Review } from "../../src/core/types.ts";

function history(verdicts: string[], duplicate = false): { phase: PhaseState; events: Event[] } {
  const phase = baseState({ phase: "RESOLVING", repairRoundsGranted: 47 }).phase;
  phase.contract.requirements = [{ id: "R1", title: "atomic", text: "atomic", arch: [], verify: ["review"] }];
  const events: Event[] = [];
  if (duplicate) {
    phase.messages = [makeMessage({ id: "F-original", state: "published" })];
    const created = applyEntryEvent([], { type: "ENTRY_OPENED", entryId: "E-115", phaseId: "p1", title: "R1 atomic write", messageId: "F-original" }, phase.messages);
    assert.ok(created.ok);
    phase.entries = created.entries;
  }
  verdicts.forEach((verdict, i) => {
    const sha = `C${i + 1}`;
    events.push({ type: "FREEZE_COMPLETED", candidateSha: sha, decisions: [] });
    const reviews = ["M", "A", "B"].map((seat): Review => ({
      ...approvingReview(seat, sha, phase.contract.contractVersion),
      items: [{ id: "R1", verdict: (verdict === "mixed" ? { M: "met", A: "unmet", B: "partial" }[seat] : verdict) as "unmet", evidence: `src/check.ts:${i + 1}` }],
    }));
    events.push(...reviews.map((review): Event => ({ type: "REVIEW_SUBMITTED", review })));
    if (duplicate) {
      events.push({ type: "MESSAGE_RAISED", message: makeMessage({ id: `F-${i + 1}`, type: "finding", title: "R1 atomic write still missing", boundCandidateSha: sha }) });
      events.push({ type: "MESSAGE_DROPPED", messageId: `F-${i + 1}`, boundCandidateSha: sha,
        boundContractVersion: phase.contract.contractVersion, boundRecordVersion: 1, by: "evaluator", reason: "Duplicates E-115 and earlier open finding" });
    }
    events.push({ type: "EVALUATION_COMPLETED" });
    phase.candidate = { sha, contractVersion: phase.contract.contractVersion };
    phase.reviews = Object.fromEntries(reviews.map((review) => [review.reviewer, { review }]));
  });
  return { phase, events };
}

test("three reviewed candidates with the same unmet verdict stop even when the evaluator dropped duplicates", () => {
  const { phase, events } = history(["unmet", "unmet", "unmet"], true);
  const reason = repeatedItemStall(phase, JSON.parse(JSON.stringify(events)))!;
  const schema = JSON.parse(readFileSync(new URL("../../schemas/event.schema.json", import.meta.url), "utf8"));
  assert.equal(validate(schema, { type: "PHASE_STUCK", reason }).valid, true);
  assert.match(reason, /^stuck: R1 unchanged for 3 rounds/);
  assert.match(reason, /Repeated findings: E-115/);
  const parked = reduce({ run: "RUN_ACTIVE", phase }, { type: "PHASE_STUCK", reason });
  assert.ok(parked.ok);
  assert.equal(parked.state.phase.phase, "AWAITING_OWNER");
  assert.deepEqual(next(parked.state), []);
  assert.match(waitReason(parked.state.phase), /stuck: R1/);
  assert.deepEqual(parked.state.phase.ownerRequests.at(-1)!.options.map((o) => o.id), ["grant", "stop"]);
  assert.match(answeringCommand("run", parked.state.phase)!, /stop/);
});

test("unchanged verdicts alone stop; progress, missing reviews and old contracts break a streak", () => {
  const { phase, events } = history(["unmet", "unmet", "unmet"]);
  assert.match(repeatedItemStall(phase, events)!, /stuck: R1/);
  for (const verdicts of [["unmet", "unmet"], ["unmet", "partial", "partial"], ["unmet", "met", "unmet"]]) {
    const h = history(verdicts);
    assert.equal(repeatedItemStall(h.phase, h.events), undefined);
  }
  assert.equal(repeatedItemStall(phase, events.filter((e) => e.type !== "REVIEW_SUBMITTED" || e.review.reviewer !== "B")), undefined);
  phase.contract = { ...phase.contract, contractVersion: { snapshot: 2, sectionSha256: "changed" } };
  assert.equal(repeatedItemStall(phase, events), undefined);
});

test("a recheck of the same candidate counts once; failed check rounds do not erase reviewed history", () => {
  const { phase, events } = history(["unmet", "unmet"]);
  assert.equal(repeatedItemStall(phase, [...events, { type: "EVALUATION_COMPLETED" }]), undefined);
  const h = history(["unmet", "unmet", "unmet"]);
  h.events.splice(5, 0, { type: "FREEZE_COMPLETED", candidateSha: "red", decisions: [] }, { type: "CHECKS_FAILED" });
  assert.match(repeatedItemStall(h.phase, h.events)!, /R1 unchanged/);
});

test("the same open finding dropped in three reviews also stops changing unmet verdicts", () => {
  const h = history(["unmet", "partial", "unmet"], true);
  assert.match(repeatedItemStall(h.phase, h.events)!, /R1 unchanged/);
  h.phase.messages![0].state = "resolved";
  assert.equal(repeatedItemStall(h.phase, h.events), undefined, "a resolved finding is progress");
});

test("all grants remain recorded but cannot buy a seventh phase round", () => {
  const state = baseState({ phase: "REPAIRING", repairRoundsGranted: 47, repairRoundsUsed: 5 });
  assert.equal(roundCeiling(state.phase.contract), 6);
  assert.equal(repairBudgetRemains(state.phase), false);
  assert.deepEqual(next(state), [{ type: "repair_budget_exhausted" }]);
  assert.equal(reduce(state, { type: "REPAIR_ATTEMPT_STARTED" }).ok, false);
  for (const lanes of [[], ["a", "b"]]) {
    assert.equal(reduce(state, { type: "ROUND_STARTED", round: 7, base: "B", lanes }).ok, false);
  }
  const parked = reduce(state, { type: "REPAIR_BUDGET_EXHAUSTED" });
  assert.ok(parked.ok);
  assert.match(parked.state.phase.ownerRequests.at(-1)!.reason, /stuck:.*6 total rounds/);
  assert.equal(parked.state.phase.repairRoundsGranted, 47);
  state.phase.contract.roundCeiling = 8;
  assert.equal(repairBudgetRemains(state.phase), true);
  assert.equal(repairBudgetRemains(state.phase, 9), false);
});

test("an ordinary exhausted allowance below the ceiling keeps its gate reason and grant/stop options", () => {
  const state = baseState({ phase: "CHECKING", repairRoundsGranted: 0 });
  state.phase.candidate = { sha: "C1", contractVersion: state.phase.contract.contractVersion };
  const parked = reduce(state, { type: "CHECKS_FAILED" });
  assert.ok(parked.ok);
  const request = parked.state.phase.ownerRequests[0];
  assert.equal(request.reason, "the checks kept failing");
  assert.deepEqual(request.options.map((o) => o.id), ["grant", "stop"]);
  assert.equal(parked.state.phase.phase, "AWAITING_OWNER");
});

for (const existing of [false, true]) test(`stuck parking preserves ${existing ? "existing" : "new"} finding resolutions and accepting risk clears the park`, () => {
  const state = baseState({ phase: "RESOLVING" });
  const K = state.phase.contract.contractVersion;
  state.phase.candidate = { sha: "C1", contractVersion: K };
  state.phase.checks = { candidateSha: "C1", passed: true };
  state.phase.probe = { candidateSha: "C1", head: "H0", probedI: "I1", passed: true };
  state.phase.reviews = Object.fromEntries(["M", "A", "B"].map((seat) => [seat, { review: approvingReview(seat, "C1", K) }]));
  state.phase.findings = [{ id: "F1", version: 1, phaseId: "p1", kind: "defect", severity: "blocking",
    evidence: "A1 deviates from the plan", raisedBy: "conductor", status: "open", boundCandidateSha: "C1" }];
  if (existing) state.phase.ownerRequests = [{ id: "OR-existing", version: 1, phaseId: "p1", origin: "open_finding",
    linkedFindingId: "F1", boundCandidateSha: "C1", boundContractVersion: K, status: "open", reason: "A1 deviates",
    options: [{ id: "accept_risk", label: "accept risk" }, { id: "repair", label: "repair" }] }];
  const parked = reduce(state, { type: "PHASE_STUCK", reason: "stuck: A1 unchanged for 3 rounds" });
  assert.ok(parked.ok);
  assert.equal(parked.state.phase.ownerRequests.length, 1, "no extra gate prevents settling the finding");
  const request = parked.state.phase.ownerRequests[0];
  assert.match(request.reason, /^stuck: A1 unchanged for 3 rounds\n.*A1/);
  assert.deepEqual(request.options.map((o) => o.id), ["accept_risk", "repair"]);
  const accepted = reduce(parked.state, { type: "OWNER_REQUEST_RESOLVED", requestId: request.id, option: "accept_risk",
    note: "the deviation is acceptable", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 1 });
  assert.ok(accepted.ok, accepted.ok ? "" : accepted.reason);
  assert.equal(accepted.state.phase.phase, "RESOLVING");
  assert.equal(accepted.state.phase.findings[0].status, "accepted");
  assert.equal(next(accepted.state)[0]?.type, "accept");
});

test("a ceiling gate still offers grant, but refuses it for the ceiling rather than an unknown option", () => {
  const state = baseState({ phase: "REPAIRING", repairRoundsGranted: 47, repairRoundsUsed: 5 });
  const K = state.phase.contract.contractVersion;
  state.phase.candidate = { sha: "C6", contractVersion: K };
  const parked = reduce(state, { type: "REPAIR_BUDGET_EXHAUSTED" });
  assert.ok(parked.ok);
  const request = parked.state.phase.ownerRequests[0];
  assert.deepEqual(request.options.map((o) => o.id), ["grant", "stop"]);
  const event = { type: "OWNER_REQUEST_RESOLVED" as const, requestId: request.id, option: "grant",
    boundCandidateSha: "C6", boundContractVersion: K, boundRecordVersion: 1 };
  const granted = reduce(parked.state, event);
  assert.equal(granted.ok, false);
  if (!granted.ok) assert.match(granted.reason, /hard ceiling of 6 total rounds.*'grant' cannot authorize another round/);
  assert.deepEqual(granted.state, parked.state, "refusal neither resolves the request nor changes the allowance");
  const stopped = reduce(parked.state, { ...event, option: "stop" });
  assert.ok(stopped.ok);
  assert.equal(stopped.state.phase.phase, "BLOCKED");
});

test("program ceiling reaches a node's frozen contract and invalid settings lint", () => {
  const plan = { title: "test", repo: ".", integrationBranch: "main", checks: [], phases: [{ id: "p1", goal: "test", acceptance: [], checks: [], boundaries: [], reserved: [] }] };
  const p = nodePlan({ title: "program", maxParallel: 1, roundCeiling: 4, entries: [{ id: "node", after: [], plan }] },
    { id: "node", entry: "node", phaseIndex: 0, deps: [] });
  assert.equal(initialState("r", p.phases[0], "H", [], undefined, p).phase.contract.roundCeiling, 4);
  assert.equal(parseOrgPlan("#+TT_ROUND_CEILING: 9\n* phase\nGoal: test").roundCeiling, 9);
  for (const roundCeiling of [0, -1, 1.5, Infinity, NaN]) {
    assert.ok(lintPlan({ phases: [], roundCeiling }).some((f) => /ROUND_CEILING/.test(f.problem)));
    assert.ok(lintProgram({ entries: [], roundCeiling }).some((f) => /ROUND_CEILING/.test(f.problem)));
  }
});

test("repeat detection honors evaluator overturns instead of raw reviewer verdicts", () => {
  const h = history(["unmet", "unmet", "unmet"]);
  const end = h.events.length - 1;
  const overturns = ["M", "A"].map((seat) => ({ id: "R1", seat, verdict: "unmet" as const, effect: "flip" as const, reason: "the candidate proves it" }));
  h.events.splice(end, 0, { type: "ITEM_STATE_UPDATED", overturns });
  h.phase.overturns = overturns;
  assert.equal(repeatedItemStall(h.phase, h.events), undefined);
});

test("stable disagreement also stalls, while absent item verdicts do not invent a review streak", () => {
  const h = history(["mixed", "mixed", "mixed"]);
  assert.match(repeatedItemStall(h.phase, h.events)!, /R1 unchanged for 3 rounds/);
  for (const event of h.events) {
    if (event.type === "REVIEW_SUBMITTED" && event.review.reviewer === "B") event.review.items = [];
  }
  assert.equal(repeatedItemStall(h.phase, h.events), undefined);
});

test("distinct worker rounds on an unchanged commit still count as three reviewed rounds", () => {
  const h = history(["unmet", "unmet", "unmet"]);
  const events: Event[] = [];
  let first = true;
  for (const event of h.events) {
    if (event.type === "FREEZE_COMPLETED") {
      if (!first) events.push({ type: "REPAIR_ATTEMPT_STARTED" });
      first = false;
      event.candidateSha = "same-commit";
    }
    if (event.type === "REVIEW_SUBMITTED") event.review.candidateSha = "same-commit";
    events.push(event);
  }
  h.phase.candidate!.sha = "same-commit";
  assert.match(repeatedItemStall(h.phase, events)!, /R1 unchanged for 3 rounds/);
});
