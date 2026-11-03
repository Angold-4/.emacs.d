import assert from "node:assert/strict";
import { test } from "node:test";
import { phaseItemOutcomes } from "../../src/core/items.ts";
import { accept, acceptanceBlockers, itemsNeedingEvaluatorReverify } from "../../src/core/predicate.ts";
import { blocksAcceptance } from "../../src/core/rounds.ts";
import * as fixes from "../../src/core/fix-rounds.ts";
import { buildReviewerPrompt, ownerRepairPointFor } from "../../src/conductor.ts";
import { runDomainLoop } from "../../src/fix-loop.ts";
import type { Finding, PhaseState, Review } from "../../src/core/types.ts";
import { basePhase, CV } from "./helpers.ts";

function point(id: string, item: string, domain: string): Finding {
  return { id, item, domain, version: 1, phaseId: "p1", kind: "defect", severity: "blocking",
    raisedBy: "M", status: "repaired", boundCandidateSha: "C1", evidence: `${item} was wrong`,
    where: `${domain}/a.ts:1`, expected: "the result is correct", doneWhen: `test-${id}` };
}
function phase(): PhaseState {
  const findings = [point("F1", "R1", "booking"), point("F2", "R2", "gates")];
  const p = basePhase({ phase: "RESOLVING", candidate: { sha: "C2", contractVersion: CV() }, round: 2,
    findings, checks: { candidateSha: "C2", passed: true },
    probe: { candidateSha: "C2", head: "H0", probedI: "I2", passed: true },
    rounds: [{ round: 2, base: "C1", lanes: [], candidates: [], votes: [], fixed: {
      base: "C1", candidate: "C2", tasks: findings.map((f) => ({ domain: f.domain!, findingIds: [f.id], sha: `head-${f.id}` })),
      merges: findings.map((f) => ({ sha: `head-${f.id}` })),
      domains: findings.map((f) => ({ iteration: 1, sha: `head-${f.id}`,
        facts: { checkPassed: true, points: [{ id: f.id, onBase: false, onHead: true, doneWhen: f.doneWhen }] },
        review: { points: [{ pointId: f.id, fixed: true, why: `${f.where} correct result`, anchors: [f.where!] }], quality: { ok: true } } })),
    } }],
  });
  p.contract = { ...p.contract, domains: [{ name: "booking", globs: ["booking/**"] }, { name: "gates", globs: ["gates/**"] }],
    requirements: ["R1", "R2", "R3"].map((id) => ({ id, title: id, text: `${id} works`, arch: [], verify: ["review"] })) };
  p.reviews = Object.fromEntries(["M", "A", "B"].map((seat) => [seat, { review: review(seat) }]));
  return p;
}
function review(reviewer = "M"): Review {
  return { reviewer, phaseId: "p1", candidateSha: "C2", contractVersion: CV(), correctionStatements: [], findingStatements: [] };
}

test("stage assumptions: two approved domains meet their items without seat votes; untouched items still need seats", () => {
  const p = phase();
  const outcomes = phaseItemOutcomes(p);
  assert.deepEqual(outcomes.map((o) => o.outcome), ["met", "met", "incomplete"]);
  assert.match(outcomes[0].evidence.join("\n"), /booking.*F1.*head-F1/);
  assert.match(outcomes[0].evidence.join("\n"), /correct result/);
  assert.deepEqual(acceptanceBlockers(p).filter((b) => b.kind === "item").map((b) => b.detail.split(" ")[0]), ["R3"]);
  const brief = buildReviewerPrompt(p, "M");
  assert.match(brief, /Assume.*resolved/i);
  assert.match(brief, /integration defects|across domains/i);
  assert.match(brief, /head-F1/);
  assert.doesNotMatch(brief, /- R1 \(requirement\): met \|/);
  assert.match(brief, /- R3 \(requirement\): met \|/);
  for (const seat of ["M", "A", "B"]) p.reviews[seat]!.review!.items = [{ id: "R3", verdict: "met", evidence: "src/core/items.ts:1 holds" }];
  assert.deepEqual(acceptanceBlockers(p), []);
  assert.equal(accept(p, "C2", CV()), true, "the seats owe only the untouched item");
});

test("stage assumptions: an unsupported partial is advisory; a new path:line defect blocks", () => {
  const p = phase();
  for (const seat of ["M", "A", "B"]) p.reviews[seat]!.review!.items = [{ id: "R3", verdict: "met", evidence: "src/core/items.ts:1 holds" }];
  const r = { ...review(), items: [...p.reviews.M!.review!.items!, { id: "R1", verdict: "partial" as const, evidence: "still partial" }] };
  p.reviews.M = { review: r };
  assert.equal(phaseItemOutcomes(p)[0].outcome, "met");
  assert.equal(acceptanceBlockers(p).some((b) => b.kind === "item" && b.detail.startsWith("R1 ")), false);
  assert.equal(accept(p, "C2", CV()), true);
  const advisory = fixes.acceptanceReviewAdvisories(p, r);
  assert.equal(advisory.length, 1);
  assert.equal(advisory[0].severity, "advisory");
  assert.match(advisory[0].evidence, /R1.*partial/);
  const newFinding = { kind: "defect" as const, severity: "blocking" as const, item: "R1", domain: "booking",
    where: "booking/a.ts:9", evidence: "R1 booking/a.ts:9 returns a duplicate when both domains retry", expected: "return once", doneWhen: "test-retry" };
  assert.deepEqual(fixes.acceptanceReviewAdvisories(p, { ...r, findings: [newFinding] }), []);
  assert.equal(blocksAcceptance({ severity: "blocking", itemId: "R1", evidence: newFinding.evidence }, 2,
    { itemIds: ["R1", "R2", "R3"], regressions: [] }), true);
  p.findings.push({ ...point("F3", "R1", "booking"), ...newFinding, status: "open", boundCandidateSha: "C2" });
  assert.ok(acceptanceBlockers(p).some((b) => b.kind === "point" && b.detail.includes("F3")));
  assert.equal(accept(p, "C2", CV()), false);
});

test("stage assumptions: a domain dropped after three reviews requests the owner and leaves approved work mergeable", async () => {
  const p = phase();
  const task = { domain: "gates", findingIds: ["F2"], findings: [{ ...p.findings[1], status: "open" as const }] };
  let n = 0;
  const outcome = await runDomainLoop(task, { maxReviews: 3,
    build: async () => ({ sha: "g1" }),
    facts: async () => ({ checkPassed: true, points: [{ id: "F2", onBase: false, onHead: true, doneWhen: "test-F2", outputTail: "fact passed" }] }),
    review: async () => { n++; return { points: [{ pointId: "F2", fixed: false, why: "gates/a.ts:1 still duplicates output" }], quality: { ok: true } }; },
    fix: async () => ({ sha: `g${n + 1}` }),
  });
  assert.equal(outcome.status, "dropped");
  assert.equal(outcome.reviews, 3);
  const request = fixes.domainFailureRequest(p, task, 2, outcome);
  assert.ok(request);
  assert.match(request.reason, /domain gates was not fixed in 3 loops/);
  assert.match(request.reason, /still duplicates output/);
  assert.match(request.reason, /fact passed/);
  assert.deepEqual(request.options.map((o) => o.id), ["repair", "rewrite_plan_point"]);
  assert.equal(request.origin, "open_finding");
  assert.equal(request.linkedFindingId, "F2");
  // The caller integrates only approved outcomes; an owner request is not
  // an instruction to discard the other domain's approved commit.
  const approved = await runDomainLoop({ domain: "booking", findingIds: ["F1"], findings: [p.findings[0]] }, {
    maxReviews: 3, build: async () => ({ sha: "head-F1" }),
    facts: async () => ({ checkPassed: true, points: [{ id: "F1", onBase: false, onHead: true }] }),
    review: async () => ({ points: [{ pointId: "F1", fixed: true, why: "booking/a.ts:1 correct output" }], quality: { ok: true } }),
    fix: async () => { throw Error("no retry is owed"); },
  });
  assert.deepEqual([approved, outcome].filter((o) => o.status === "approved").map((o) => o.sha), ["head-F1"]);
});

test("stage assumptions: unmerged approvals and unresolved companion points do not cover an item", () => {
  const p = phase();
  p.rounds![0].fixed!.merges = [{ sha: "head-F1" }];
  assert.deepEqual(phaseItemOutcomes(p).map((o) => o.outcome), ["met", "incomplete", "incomplete"]);
  p.findings.push({ ...point("F3", "R1", "booking"), status: "open" });
  assert.equal(phaseItemOutcomes(p)[0].outcome, "incomplete");
  p.findings.pop();
  p.doneWhenFacts = [{ findingId: "F1", candidateSha: "C2", exitCode: 1, timedOut: false, outputTail: "regressed" }];
  assert.equal(phaseItemOutcomes(p)[0].outcome, "incomplete", "a failing merged fact overrides approval");
});

test("stage assumptions: approvals accumulate along integrated bases and do not send settled item splits to the evaluator", () => {
  const p = phase();
  p.rounds!.push({ round: 3, base: "C2", lanes: [], candidates: [], votes: [], fixed: { base: "C2", candidate: "C3", tasks: [], merges: [] } });
  p.candidate!.sha = "C3";
  p.reviews.M!.review!.items = [{ id: "R1", verdict: "met", evidence: "booking/a.ts:1" }];
  p.reviews.A!.review!.items = [{ id: "R1", verdict: "partial", evidence: "still partial" }];
  p.reviews.B!.review!.items = [{ id: "R1", verdict: "unmet", evidence: "still unmet" }];
  assert.equal(phaseItemOutcomes(p)[0].outcome, "met");
  assert.equal(itemsNeedingEvaluatorReverify(p), false, "ACCEPT cannot re-open a domain item via an evaluator split");
  p.candidate!.sha = "unrelated";
  assert.equal(phaseItemOutcomes(p)[0].outcome, "incomplete", "an unrelated candidate inherits no domain approval");
});

test("stage assumptions: a path:line grade alone is advisory and round one keeps its original gate", () => {
  const ground = { severity: "blocking" as const, itemId: "R1", evidence: "R1 booking/a.ts:1 still partial" };
  const gate = { itemIds: ["R1"], regressions: [], domainResolvedItems: ["R1"] };
  assert.equal(blocksAcceptance(ground, 2, gate), false);
  assert.equal(blocksAcceptance(ground, 1, gate), true);
  assert.equal(blocksAcceptance({ ...ground, evidence: "retry_check failed" }, 2, { ...gate, regressions: ["retry_check"] }), true);
});

test("stage assumptions: the owner can rewrite a failed plan point through the existing repair route", () => {
  const p = phase();
  p.findings[1].status = "open";
  const request = fixes.domainFailureRequest(p, { domain: "gates", findingIds: ["F2"], findings: [p.findings[1]] }, 2,
    { status: "dropped", reviews: 3, fixes: 2, records: [] })!;
  request.status = "resolved";
  request.resolution = { option: "rewrite_plan_point", note: "domain: booking\nitem: R1\nwhere: booking/new.ts:7\nexpected: return exactly once\ndoneWhen: test-rewritten-point" };
  request.resolvedBinding = { candidateSha: "C2", contractVersion: CV() };
  p.ownerRequests.push(request);
  const result = ownerRepairPointFor(p, request, { checks: [] });
  assert.ok(result && "finding" in result);
  assert.equal(result.finding.domain, "booking");
  assert.equal(result.finding.item, "R1");
  assert.equal(result.finding.where, "booking/new.ts:7");
  assert.equal(result.finding.doneWhen, "test-rewritten-point");
  assert.match(result.finding.expected!, /return exactly once/);
  assert.equal(result.replaces, "F2");
});
