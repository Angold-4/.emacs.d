import assert from "node:assert/strict";
import { test } from "node:test";
import { runDomainLoop } from "../../src/fix-loop.ts";
import { domainFailureRequest, domainResolvedPoints, domainUnresolvedPoints, type FixTask } from "../../src/core/fix-rounds.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { DomainFacts, DomainReview, RoundFixed } from "../../src/core/types.ts";
import { baseState, CV } from "./helpers.ts";

const task: FixTask = { domain: "schedule", findingIds: [], findings: [] };
for (let n = 1; n <= 13; n++) {
  const id = `P${n}`;
  task.findingIds.push(id);
  task.findings.push({ id, version: 1, phaseId: "p1", kind: "defect", severity: "blocking", status: "open",
    raisedBy: "M", boundCandidateSha: "pick", domain: "schedule", where: "schedule/a.ts:1", evidence: `${id} broken`, doneWhen: `test-${id}` });
}
const facts: DomainFacts = { checkPassed: true, points: task.findingIds.map((id) => ({ id, onBase: false, onHead: true, doneWhen: `test-${id}` })) };
const review = (count = 5): DomainReview => ({ points: task.findingIds.map((pointId, n) => ({ pointId, fixed: n < count,
  why: n < count ? `${pointId} verified at schedule/a.ts:1` : `${pointId} still broken at schedule/a.ts:2` })),
  quality: { ok: false, problems: ["design.md:10 verifier rejects legal overshoot"] } });

function checkpoint(sha: string, records: RoundFixed["domains"]): RoundFixed {
  return { base: "pick", candidate: "merged", tasks: [{ domain: task.domain, findingIds: task.findingIds, sha }], merges: [{ sha }], domains: records };
}

test("point progress: five of thirteen approvals survive a fixer timeout, close at REDUCE and follow the merged base", async () => {
  const outcome = await runDomainLoop(task, { maxReviews: 3, build: async () => ({ sha: "reviewed" }), facts: async () => facts,
    review: async () => review(), fix: async () => { throw Error("bound: fixer-attempt"); } });
  assert.equal(outcome.status, "dropped");
  assert.equal(outcome.sha, "reviewed");
  const fixed = checkpoint(outcome.sha!, outcome.records);
  const state = baseState({ phase: "REPAIRING", candidate: { sha: "pick", contractVersion: CV() },
    findings: task.findings, rounds: [{ round: 2, base: "pick", lanes: [], candidates: [], votes: [] }] });
  const result = reduce(state, { type: "ROUND_FIXED", round: 2, ...fixed });
  assert.equal(result.ok, true);
  const p = result.state.phase;
  assert.deepEqual(p.findings.filter((f) => f.status === "repaired").map((f) => f.id), task.findingIds.slice(0, 5));
  assert.deepEqual(p.findings.filter((f) => f.status === "open" && task.findingIds.includes(f.id)).map((f) => f.id), task.findingIds.slice(5));
  assert.ok(p.findings.some((f) => f.status === "open" && /verifier rejects/.test(f.evidence)));
  assert.deepEqual(domainUnresolvedPoints(p, "merged"), new Set(task.findingIds.slice(5)), "green doneWhen facts cannot override rejected verdicts");
  assert.deepEqual(reduce(result.state, { type: "ROUND_FIXED", round: 2, ...fixed }).state.phase.findings, p.findings,
    "checkpoint replay neither duplicates quality findings nor reopens approvals");
  assert.match(domainResolvedPoints(p, "merged").get("P1")!, /reviewed.*pass 1.*P1 verified/);
  p.rounds!.push({ round: 3, base: "merged", lanes: [], candidates: [], votes: [], fixed: { base: "merged", candidate: "merged-next", tasks: [], merges: [] } });
  assert.equal(domainResolvedPoints(p, "merged-next").size, 5);
});

test("point progress: the review bound retains partial approval without requiring quality approval", async () => {
  const outcome = await runDomainLoop(task, { maxReviews: 1, build: async () => ({ sha: "bounded" }), facts: async () => facts,
    review: async () => review(), fix: async () => { throw Error("the budget is already spent"); } });
  assert.equal(outcome.status, "dropped");
  assert.equal(outcome.sha, "bounded");
  assert.equal(outcome.fixes, 0);
  const p = baseState({ rounds: [{ round: 2, base: "pick", lanes: [], candidates: [], votes: [], fixed: checkpoint(outcome.sha!, outcome.records) }] }).phase;
  assert.equal(domainResolvedPoints(p, "merged").size, 5);
});

test("point progress: a point's red doneWhen cannot inherit its review's fixed verdict", () => {
  const pointFacts = facts.points.map((p) => ({ ...p, onHead: p.id !== "P3" }));
  const pass = { iteration: 1, sha: "reviewed", facts: { ...facts, points: pointFacts }, review: review() };
  const p = baseState({ rounds: [{ round: 2, base: "pick", lanes: [], candidates: [], votes: [], fixed: checkpoint("reviewed", [pass]) }] }).phase;
  assert.deepEqual([...domainResolvedPoints(p, "merged").keys()], ["P1", "P2", "P4", "P5"]);
  p.rounds![0].fixed!.tasks[0].sha = "unreviewed";
  p.rounds![0].fixed!.merges = [{ sha: "unreviewed" }];
  assert.equal(domainResolvedPoints(p, "merged").size, 0, "approvals cannot be moved to a different head");
});

test("point progress: a resumed flat review stalls immediately using the logged previous pass", async () => {
  const pass = { iteration: 1, sha: "pass1", facts, review: review() };
  const outcome = await runDomainLoop(task, { maxReviews: 3, startReviews: 1, startIteration: 2, startRecords: [pass],
    build: async () => ({ sha: "pass2" }), facts: async () => facts, review: async () => review(),
    fix: async () => { throw Error("a stalled review must not launch another fixer"); } });
  assert.equal(outcome.status, "stalled");
  assert.equal(outcome.reviews, 2);
  assert.equal(outcome.fixes, 0);
  assert.equal(outcome.sha, "pass2");
});

test("point progress: a resumed fixer timeout keeps the previously logged reviewed head", async () => {
  const pass = { iteration: 1, sha: "logged", facts, review: review() };
  const outcome = await runDomainLoop(task, { maxReviews: 3, startReviews: 1, startIteration: 2, startRecords: [pass],
    build: async () => { throw Error("bound: fixer-attempt"); }, facts: async () => { throw Error("no new head"); },
    review: async () => { throw Error("no new head"); }, fix: async () => { throw Error("no new head"); } });
  assert.equal(outcome.status, "dropped");
  assert.equal(outcome.sha, "logged");
  assert.equal(domainResolvedPoints(baseState({ rounds: [{ round: 2, base: "pick", lanes: [], candidates: [], votes: [],
    fixed: checkpoint(outcome.sha!, outcome.records) }] }).phase, "merged").size, 5);
});

test("point progress: a failing integrated doneWhen reopens a checkpoint closure", () => {
  const fixed = checkpoint("reviewed", [{ iteration: 1, sha: "reviewed", facts, review: review() }]);
  const state = baseState({ findings: task.findings, rounds: [{ round: 2, base: "pick", lanes: [], candidates: [], votes: [] }] });
  const checkpointed = reduce(state, { type: "ROUND_FIXED", round: 2, ...fixed });
  assert.equal(checkpointed.state.phase.findings[0].status, "repaired");
  const regressed = reduce(checkpointed.state, { type: "FINDING_FACT", findingId: "P1", candidateSha: "merged",
    exitCode: 1, timedOut: false, outputTail: "integration changed the result" });
  assert.equal(regressed.ok, true);
  assert.equal(regressed.state.phase.findings[0].status, "open");
  assert.equal(domainResolvedPoints(regressed.state.phase, "merged").has("P1"), false);
  assert.equal(domainUnresolvedPoints(regressed.state.phase, "merged").has("P1"), true);
});

test("point progress: three identical review scripts stop at pass two as stalled and request the owner for stuck points", async () => {
  let reviews = 0;
  let fixes = 0;
  const outcome = await runDomainLoop(task, { maxReviews: 3, build: async () => ({ sha: "pass1" }), facts: async () => facts,
    review: async () => { reviews++; return review(); }, fix: async () => ({ sha: `pass${++fixes + 1}` }) });
  assert.equal(outcome.status, "stalled");
  assert.equal(reviews, 2);
  assert.equal(fixes, 1);
  const request = domainFailureRequest(baseState().phase, task, 2, outcome)!;
  assert.ok(request);
  assert.match(request.reason, /stalled.*P6/);
  assert.match(request.reason, /P13 still broken/);
  assert.equal(request.linkedFindingId, "P6");
});

test("point progress: after a reviewed green head, a dropped red head is never selected for merging", async () => {
  const outcome = await runDomainLoop(task, { maxReviews: 3, build: async () => ({ sha: "green" }),
    facts: async (sha) => ({ ...facts, checkPassed: sha === "green" }), review: async () => review(),
    fix: async (sha) => sha === "green" ? { sha: "red" } : { note: "bound: fixer-attempt" } });
  assert.equal(outcome.status, "dropped");
  assert.equal(outcome.sha, "green", "keep the reviewed green commit, never the later red head");
  const fixed = checkpoint("red", outcome.records);
  const p = baseState({ rounds: [{ round: 2, base: "pick", lanes: [], candidates: [], votes: [], fixed }] }).phase;
  assert.equal(domainResolvedPoints(p, "merged").size, 0, "red facts cannot close any point even in a malformed merge record");
});
