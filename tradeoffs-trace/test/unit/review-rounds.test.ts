// Plan 2c (tradeoffs-trace/plans/2c-review-loop-speed.org): decision records
// across review rounds, discovery matching, repeated findings and the tally
// status exposed through `tt state`. Pure core, driven through reduce() and
// the predicate — the real entry points.

import assert from "node:assert/strict";
import { test } from "node:test";

import { accept, decisionStatus, isLiveDecision } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import { carryDecisionsForward } from "../../src/core/rounds.ts";
import { launchArgs } from "../../src/core/roles.ts";
import type { Decision, Event, PriorDecisionStatement, State } from "../../src/core/types.ts";
import { approvingReview, baseState, CV, makeBallot, makeDecision } from "./helpers.ts";

const K = CV();

function step(state: State, event: Event): State {
  const r = reduce(state, event);
  assert.equal(r.ok, true, `reduce rejected ${event.type}: ${r.ok ? "" : r.reason}`);
  return r.state;
}

/** A phase that reviewed candidate C1 (with the given decisions) and is now
 * freezing its repair candidate. */
function freezingRepair(decisions: Decision[], prior?: PriorDecisionStatement[]): State {
  const s = baseState({
    phase: "IMPLEMENTING",
    attempt: { n: 2 },
    candidate: { sha: "C1", contractVersion: K },
    decisions,
    round: 1,
  });
  return step(s, { type: "SUBMIT_PHASE", disclosures: [], ...(prior ? { prior } : {}) });
}

test("stale-decisions-not-votable: records of a superseded candidate never block acceptance", () => {
  const worker = makeDecision({ id: "D-w1", source: "worker" });
  const disc = makeDecision({ id: "D-disc-A-2", source: "reviewer-discovered" });
  const trig = makeDecision({ id: "D-trigger-3", source: "trigger" });
  let s = freezingRepair([worker, disc, trig]);
  s = step(s, { type: "FREEZE_COMPLETED", candidateSha: "C2", decisions: [] });

  assert.equal(s.phase.round, 2);
  for (const d of s.phase.decisions) {
    assert.equal(isLiveDecision(d), false, `${d.id} should be superseded`);
    assert.match(d.supersededBy ?? "", /C2|candidate/);
    assert.equal(decisionStatus(d, s.phase).status, "superseded");
  }

  // A ballot on a superseded record is refused by the core.
  const ballot = reduce({ ...s, phase: { ...s.phase, phase: "REVIEWING" } }, {
    type: "BALLOT_CAST",
    ballot: makeBallot({ decisionId: "D-w1", boundCandidateSha: "C2", boundRecordVersion: s.phase.decisions[0].version }),
  });
  assert.equal(ballot.ok, false);

  // With checks, probe and three reviews on C2 and nothing else open, the
  // stale records do not stop acceptance.
  const phase = {
    ...s.phase,
    phase: "RESOLVING" as const,
    checks: { candidateSha: "C2", passed: true },
    probe: { candidateSha: "C2", head: "H0", probedI: "C2", passed: true },
    reviews: {
      M: { review: approvingReview("M", "C2", K) },
      A: { review: approvingReview("A", "C2", K) },
      B: { review: approvingReview("B", "C2", K) },
    },
  };
  assert.equal(accept(phase, "C2", K), true);
});

test("stale-decisions-not-votable: kept and changed records are rebound to the new candidate, withdrawn ones superseded", () => {
  const kept = makeDecision({ id: "D-kept", source: "worker" });
  const changed = makeDecision({ id: "D-changed", source: "worker", choice: "old text" });
  const withdrawn = makeDecision({ id: "D-withdrawn", source: "worker" });
  let s = freezingRepair([kept, changed, withdrawn], [
    { id: "D-kept", status: "kept" },
    { id: "D-changed", status: "changed", choice: "new text" },
    { id: "D-withdrawn", status: "withdrawn" },
  ]);
  s = step(s, { type: "FREEZE_COMPLETED", candidateSha: "C2", decisions: [] });
  const byId = new Map(s.phase.decisions.map((d) => [d.id, d]));

  assert.equal(byId.get("D-kept")!.boundCandidateSha, "C2");
  assert.equal(byId.get("D-kept")!.version, 2, "the version bumps so no old ballot can count");
  assert.equal(isLiveDecision(byId.get("D-kept")!), true);

  assert.equal(byId.get("D-changed")!.choice, "new text");
  assert.equal(byId.get("D-changed")!.boundCandidateSha, "C2");

  assert.equal(isLiveDecision(byId.get("D-withdrawn")!), false);
  assert.match(byId.get("D-withdrawn")!.supersededBy!, /withdrawn/);

  // Carried records still need a vote on the new candidate.
  assert.equal(decisionStatus(byId.get("D-kept")!, s.phase).status, "pending");
  assert.equal(s.phase.pendingPrior, undefined, "consumed by the freeze");
});

test("carryDecisionsForward leaves records already on the new candidate alone", () => {
  const onC2 = makeDecision({ id: "D-new", boundCandidateSha: "C2" });
  assert.deepEqual(carryDecisionsForward([onC2], undefined, "C2"), [onC2]);
});

test("discovery matching: a reviewer's duplicate discovery is superseded and credited on the matched record", () => {
  const worker = makeDecision({ id: "D-w1", source: "worker" });
  const disc = makeDecision({ id: "D-disc-A-2", source: "reviewer-discovered" });
  let s = baseState({ phase: "REVIEWING", candidate: { sha: "C1", contractVersion: K }, decisions: [worker, disc] });
  s = step(s, { type: "DECISION_MATCHED", decisionId: "D-disc-A-2", sameAs: "D-w1", reviewer: "A" });
  const byId = new Map(s.phase.decisions.map((d) => [d.id, d]));
  assert.equal(isLiveDecision(byId.get("D-disc-A-2")!), false);
  assert.match(byId.get("D-disc-A-2")!.supersededBy!, /same as D-w1/);
  assert.deepEqual(byId.get("D-w1")!.alsoSeenBy, ["A"]);

  // Only a reviewer-discovered record can be matched away.
  const bad = reduce(s, { type: "DECISION_MATCHED", decisionId: "D-w1", sameAs: "D-disc-A-2", reviewer: "A" });
  assert.equal(bad.ok, false);
});

test("repeated findings: 'same as' records the reviewer on the open finding", () => {
  const finding = {
    id: "F1",
    version: 1,
    phaseId: "p1",
    kind: "defect" as const,
    severity: "blocking" as const,
    evidence: "src/a.ts:1 wedges the run",
    raisedBy: "M" as const,
    status: "open" as const,
    boundCandidateSha: "C1",
  };
  let s = baseState({ phase: "REVIEWING", candidate: { sha: "C1", contractVersion: K }, findings: [finding] });
  s = step(s, { type: "FINDING_ALSO_RAISED", findingId: "F1", reviewer: "B" });
  s = step(s, { type: "FINDING_ALSO_RAISED", findingId: "F1", reviewer: "B" });
  assert.deepEqual(s.phase.findings[0].alsoRaisedBy, ["B"], "recorded once");
  assert.equal(s.phase.findings.length, 1, "no duplicate finding");
});

test("decisionStatus names why a vote failed", () => {
  const d = makeDecision({ id: "D1" });
  const reviews = {
    M: { review: approvingReview("M", "C1", K) },
    A: { review: approvingReview("A", "C1", K) },
    B: { review: approvingReview("B", "C1", K) },
  };
  const phase = (ballots: ReturnType<typeof makeBallot>[]) =>
    baseState({ phase: "RESOLVING", candidate: { sha: "C1", contractVersion: K }, decisions: [d], ballots, reviews }).phase;

  assert.deepEqual(decisionStatus(d, phase([makeBallot({ reviewer: "A" })])), {
    status: "failed",
    reason: "missing ballot from M",
  });
  assert.equal(
    decisionStatus(d, phase([makeBallot({ reviewer: "M", vote: "reject" }), makeBallot({ reviewer: "A" })])).reason,
    "M veto",
  );
  assert.equal(
    decisionStatus(d, phase([makeBallot({ reviewer: "M" }), makeBallot({ reviewer: "A" })])).status,
    "passed",
  );
});

test("launchArgs continues the previous session only when asked (repair attempts, later rounds)", () => {
  assert.ok(launchArgs("worker", { sessionDir: "/s", continueSession: true }).includes("--continue"));
  assert.ok(!launchArgs("worker", { sessionDir: "/s" }).includes("--continue"));
  assert.ok(!launchArgs("worker", { noSession: true, continueSession: true }).includes("--continue"));
});
