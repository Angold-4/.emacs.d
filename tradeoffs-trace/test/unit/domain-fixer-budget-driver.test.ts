import assert from "node:assert/strict";
import { test } from "node:test";
import { registerHooks } from "node:module";
import { reduce } from "../../src/core/reduce.ts";
import { baseState } from "./helpers.ts";
import type { FixTask } from "../../src/core/fix-rounds.ts";
import type { Event } from "../../src/core/types.ts";

const seams = ["runFixer", "fixerWorktree", "paths", "deadlines", "state", "planItems", "domainLoopReviewsSpent",
  "domainLoopResumeIteration", "domainLoopHistory", "buildFixerAgent", "domainFacts", "runDomainReview", "fixerPrompt",
  "fixerFixPrompt", "stopRequested", "recheckCancelledLaneRound", "log", "applyEvent"];
const url = new URL("../../src/conductor.ts?domain-fixer-budget-driver", import.meta.url).href;
const hooks = registerHooks({ load(target, context, load) {
  const result = load(target, context);
  if (target !== url) return result;
  let source = result.source.toString();
  const start = source.indexOf("  async #runFixer(");
  const end = source.indexOf("\n  /** 07g1: the failure text", start);
  assert.ok(start >= 0 && end > start);
  source = source.slice(0, start) + source.slice(start, end).replaceAll("Date.now()", "this.test_now()") + source.slice(end);
  return { ...result, source: source.replaceAll("treeOf(this.#plan.repo,", "this.test_treeOf(")
    .replace(new RegExp(`#(${seams.join("|")})\\b`, "g"), "test_$1") };
} });
const { Conductor } = await import(url);
hooks.deregister();

for (const ending of ["stalled", "timeout"] as const) test(`slow facts/reviews preserve the ${ending} outcome and five reviewed point closures`, async () => {
  const task: FixTask = { domain: "booking", findingIds: [], findings: [] };
  for (let i = 1; i <= 13; i++) {
    const id = `P${i}`;
    task.findingIds.push(id);
    task.findings.push({ id, version: 1, phaseId: "p1", kind: "defect", severity: "blocking", status: "open",
      raisedBy: "M", boundCandidateSha: "pick", domain: "booking", where: `booking/a.ts:${i}`,
      evidence: `${id} broken`, doneWhen: "test -f booking-fixed.txt" });
  }
  let now = 0;
  let builds = 0;
  let reviews = 0;
  const events: Event[] = [];
  const conductor = Object.create(Conductor.prototype);
  conductor.test_now = () => now;
  conductor.test_treeOf = (sha: string) => sha;
  conductor.test_fixerWorktree = () => "/tmp/unused-budget-worktree";
  conductor.test_paths = { sessions: "/tmp/unused-budget-sessions" };
  conductor.test_deadlines = { fixerAttemptMs: 6_000 };
  conductor.test_state = baseState({ phase: "IMPLEMENTING", findings: task.findings,
    rounds: [{ round: 2, base: "pick", lanes: [], candidates: [], votes: [] }] });
  conductor.test_planItems = () => ({ architecture: [], requirements: [], constraints: [] });
  conductor.test_domainLoopReviewsSpent = () => 0;
  conductor.test_domainLoopResumeIteration = () => 1;
  conductor.test_domainLoopHistory = (_task, _round, records) => records;
  conductor.test_fixerPrompt = conductor.test_fixerFixPrompt = () => "fix the points";
  conductor.test_stopRequested = conductor.test_recheckCancelledLaneRound = false;
  conductor.test_log = { append() {} };
  conductor.test_applyEvent = (event: Event) => events.push(event);
  conductor.test_buildFixerAgent = async (opts) => {
    builds++;
    if (builds === 1) { now += 1_000; return { sha: "reviewed-first" }; }
    assert.equal(opts.fix.timeoutMs, 5_000, "the second fix shares the original budget, excluding idle verification time");
    if (ending === "timeout") { now += opts.fix.timeoutMs; return { note: "bound: fixer-attempt" }; }
    now += 1_000;
    return { sha: "reviewed-second" };
  };
  conductor.test_domainFacts = async () => {
    now += 12_000;
    return { checkPassed: true, points: task.findingIds.map((id) => ({ id, onBase: false, onHead: true })) };
  };
  conductor.test_runDomainReview = async () => {
    now += 8_000;
    reviews++;
    return { points: task.findingIds.map((pointId, n) => ({ pointId, fixed: n < 5, why: n < 5 ? "verified" : "STUCK" })),
      quality: { ok: false, problems: ["design.md:10 verifier rejects legal overshoot"] } };
  };
  const outcome = await conductor.test_runFixer(task, 2, "pick");
  assert.equal(builds, 2, "verification must not spend the next fixer dispatch");
  assert.equal(reviews, ending === "stalled" ? 2 : 1);
  assert.equal(outcome.status, ending === "stalled" ? "stalled" : "dropped");
  assert.equal(outcome.ok, true, "the reviewed partial head still merges");
  const result = reduce(conductor.test_state, { type: "ROUND_FIXED", round: 2, base: "pick", candidate: "merged",
    tasks: [{ domain: task.domain, findingIds: task.findingIds, sha: outcome.sha }], merges: [{ sha: outcome.sha }], domains: outcome.domainRecords });
  assert.ok(result.ok, result.ok ? "" : result.reason);
  assert.equal(result.state.phase.findings.filter((f) => task.findingIds.includes(f.id) && f.status === "repaired").length, 5);
  assert.equal(result.state.phase.findings.filter((f) => task.findingIds.includes(f.id) && f.status === "open").length, 8);
  assert.ok(events.some((e) => e.type === "OWNER_REQUEST_OPENED"));
});
