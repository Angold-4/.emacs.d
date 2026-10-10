// Plan 06k1 (A2): a decision the owner ruled on is settled without ballots,
// and a reviewer amendment that proposes no change is a vote against, not an
// amendment of its own.

import assert from "node:assert/strict";
import { test } from "node:test";

import { isNoChangeAmendmentWording } from "../../src/conductor.ts";
import { accept, amendmentToApply, decisionSettled } from "../../src/core/predicate.ts";
import { applyOwnerRequestResolved, ruledDecisionIds } from "../../src/core/owner-commands.ts";
import { reduce } from "../../src/core/reduce.ts";
import { tally } from "../../src/core/tally.ts";
import { basePhase, baseState, CV, makeDecision } from "../unit/helpers.ts";
import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";
import * as fs from "node:fs";
import * as path from "node:path";
import type { Ballot, OwnerRequest, PhaseState, Reviewer, State } from "../../src/core/types.ts";

const K = CV();
const C1 = { sha: "C1", contractVersion: K };

function settledPhase(overrides: Partial<PhaseState> = {}): PhaseState {
  return basePhase({
    phase: "RESOLVING",
    candidate: C1,
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: { reviewer: "M", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [], ballots: [] } as never },
      A: { review: { reviewer: "A", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [], ballots: [] } as never },
      B: { review: { reviewer: "B", candidateSha: "C1", contractVersion: K, correctionStatements: [], findingStatements: [], ballots: [] } as never },
    },
    ...overrides,
  });
}

test("plan 06k1: a decision the owner ruled on is settled on the next candidate without ballots", () => {
  const decision = makeDecision({ id: "D-1", choice: "keep the cache warm", class: "delegated" });
  // The owner's correction named D-1 as ruled; the ruling is pinned to the
  // choice the owner saw.
  const ruled = [{ id: "D-1", choice: "keep the cache warm", by: "C-cmd-1" }];

  // An owner correction that names a decision id as ruled records the ruling.
  const before = baseState({
    phase: "AWAITING_OWNER",
    candidate: C1,
    decisions: [decision],
  });
  const applied = reduce(before, { type: "OWNER_CORRECTION", correctionId: "C-cmd-1", text: "D-1 is ruled: keep the cache warm." });
  assert.ok(applied.ok, "the correction applies");
  assert.deepEqual(applied.state.phase.ruledDecisions, ruled);

  // On the next candidate the decision has NO ballots and is still settled.
  const phase = settledPhase({ decisions: [decision], ballots: [], ruledDecisions: ruled });
  assert.equal(decisionSettled(decision, phase, "C1", K), true, "the ruling settles it without a ballot");
  assert.equal(phase.ballots.length, 0, "there are no ballots");
  assert.equal(accept(phase, "C1", K), true, "the phase accepts: every item met, no finding blocks");

  // A later candidate that CHANGES the choice is not settled by the ruling.
  const changed = { ...decision, choice: "drop the cache" };
  const changedPhase = settledPhase({ decisions: [changed], ballots: [], ruledDecisions: ruled });
  assert.equal(decisionSettled(changed, changedPhase, "C1", K), false, "a changed choice is not settled by the old ruling");
  assert.equal(accept(changedPhase, "C1", K), false, "so the phase does not accept on the changed decision");

  // Without the ruling, no ballots means not settled.
  const unruled = settledPhase({ decisions: [decision], ballots: [] });
  assert.equal(decisionSettled(decision, unruled, "C1", K), false);
  assert.equal(accept(unruled, "C1", K), false, "an unruled decision with no ballots still blocks acceptance");
});

test("plan 06k1: a ruling is parsed per decision id, so a negation or another id never settles one", () => {
  const decisions = [makeDecision({ id: "D-1", choice: "keep the cache warm" }), makeDecision({ id: "D-2", choice: "drop the cache" })];
  // A negated ruling rules nothing.
  assert.deepEqual(ruledDecisionIds("D-1 is not ruled; reviewers must vote on it", decisions), []);
  // A ruling attaches only to the id in its own clause.
  assert.deepEqual(ruledDecisionIds("D-1 is ruled; D-2 remains undecided", decisions), ["D-1"]);
  // The runbook's form still works, and a passing mention is not a ruling.
  assert.deepEqual(ruledDecisionIds("D-1 is ruled: keep the cache warm.", decisions), ["D-1"]);
  assert.deepEqual(ruledDecisionIds("D-1 is discussed above", decisions), []);
});

test("plan 06k1: a queued correction records the ruling it carries", () => {
  const decision = makeDecision({ id: "D-1", choice: "keep the cache warm" });
  const state = baseState({ phase: "REVIEWING", candidate: C1, decisions: [decision] });
  const result = reduce(state, {
    type: "OWNER_CORRECTION_QUEUED",
    phaseId: "p1",
    correctionId: "C-q1",
    text: "D-1 is ruled",
    grantedRounds: 3,
  });
  assert.ok(result.ok, !result.ok ? result.reason : "");
  assert.deepEqual(result.state.phase.ruledDecisions, [{ id: "D-1", choice: "keep the cache warm", by: "C-q1" }]);
});

test("plan 06k1: accept_as_implemented on a failed vote carries forward by id and unchanged choice", () => {
  const decision = makeDecision({ id: "D-1", choice: "keep the cache warm", class: "delegated" });
  const request: OwnerRequest = {
    id: "OR-p1-D-1",
    version: 1,
    phaseId: "p1",
    reason: "decision D-1 failed its vote",
    origin: "failed_vote",
    linkedDecisionId: "D-1",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    options: [
      { id: "accept_as_implemented", label: "accept the decision as implemented" },
      { id: "reject_and_repair", label: "reject it and repair (grant 3 rounds)" },
    ],
    status: "open",
  };
  const phase = basePhase({ phase: "AWAITING_OWNER", candidate: C1, decisions: [decision], ownerRequests: [request] });
  const resolved = applyOwnerRequestResolved(phase, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-p1-D-1",
    option: "accept_as_implemented",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.deepEqual(resolved.ruledDecisions, [{ id: "D-1", choice: "keep the cache warm", by: "owner" }]);
  // The ruling carries to a later candidate with the same id and choice and
  // no ballots; a changed choice is not covered.
  const later = settledPhase({ candidate: { sha: "C2", contractVersion: K }, decisions: [decision], ballots: [], ruledDecisions: resolved.ruledDecisions });
  assert.equal(decisionSettled(decision, later, "C2", K), true, "the failed-vote settlement carries forward");
  assert.equal(decisionSettled({ ...decision, choice: "drop the cache" }, later, "C2", K), false, "a changed choice is not settled");
});

test("plan 06k1: a ruled decision is not demanded in the live ballot review, exercised through the conductor", async () => {
  const setup = await setupConductor({
    phase: { id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 5_000, workerAttemptMs: 30_000, freezeMs: 20_000, checkMs: 20_000, probeMs: 20_000, reviewMs: 30_000 },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'work\n' > work.txt" },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [
              {
                classProposal: "delegated",
                choice: "keep the cache warm",
                whyItMatters: "a cold cache is a wrong answer",
                alternatives: [{ option: "drop it", consequence: "a wrong answer" }],
                recommendation: { choice: "keep", reason: "the goal names it" },
              },
            ],
            assumptions: [],
            deviations: [],
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        // Turn 1 holds the barrier for a moment so the test can send the
        // owner's ruling before turn 2 builds the live ballot demand.
        ...(reviewer === "M" ? [{ kind: "call-sh", command: "sleep 3" }] : []),
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: {
            reviewer,
            phaseId: "p1",
            candidateSha: state.phase.candidate?.sha,
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
            // NO ballot: the owner's ruling settles the decision, so the
            // live demand must not require one. Before the A-3 fix this
            // review was rejected as incomplete.
            ballots: [],
            findings: [],
          },
        },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    // Wait for the worker's decision, then send the owner's ruling as a note
    // (recorded as a directive) while the reviewers are still in turn 1.
    await waitFor(() => setup.conductor.state.phase.decisions.some((d) => d.source === "worker"), 60_000, 10, setup.runDir);
    const decision = setup.conductor.state.phase.decisions.find((d) => d.source === "worker")!;
    fs.writeFileSync(
      path.join(runPaths(setup.runDir).inbox, "cmd-rule.json"),
      JSON.stringify({ type: "note", text: `${decision.id} is ruled: keep the cache warm.` }),
    );
    await waitFor(() => (setup.conductor.state.phase.ruledDecisions ?? []).some((r) => r.id === decision.id), 60_000, 20, setup.runDir);
    await waitFor(() => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase), 120_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE", `expected DONE; got ${setup.conductor.state.phase.phase}`);
    // The live demand never listed the ruled decision.
    const demanded = readEvents(setup.runDir).filter((r) => r.kind === "ballot_demanded");
    assert.ok(demanded.length >= 3, "each seat's turn-2 prompt recorded its demand");
    for (const record of demanded) {
      const records = (record.event as { records?: Array<{ id: string }> }).records ?? [];
      assert.ok(!records.some((x) => x.id === decision.id), `the ruled decision must not be demanded: ${JSON.stringify(records)}`);
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 06k1: an amendment proposing the criterion unchanged is recorded as a vote against, not an amendment", () => {
  const criterion = "no fill after cancel is acknowledged";
  // The two no-change forms the acceptance names.
  assert.equal(isNoChangeAmendmentWording("retain the criterion unchanged", criterion), true);
  assert.equal(isNoChangeAmendmentWording(criterion, criterion), true, "the criterion's own text is no change");
  assert.equal(isNoChangeAmendmentWording("keep the criterion as written", criterion), true);
  assert.equal(isNoChangeAmendmentWording("no change", criterion), true);
  // Finding D-B-66: empty/whitespace wording states no position, so it is not
  // a retain proposal (and the conductor ignores it, casting no reject).
  assert.equal(isNoChangeAmendmentWording("", criterion), false, "empty wording is not a retain proposal");
  assert.equal(isNoChangeAmendmentWording("   ", criterion), false, "whitespace wording is not a retain proposal");
  // A real rewrite is not a no-change proposal.
  assert.equal(isNoChangeAmendmentWording("no fill after cancel is acknowledged within one tick", criterion), false);

  // The worker's amendment (a `reserved` decision) is voted on like any
  // other; a reviewer's "retain unchanged" counts as a reject, so the
  // amendment fails and nothing is applied.
  const amendment = makeDecision({
    id: "D-amend",
    class: "reserved",
    choice: "no fill after cancel is acknowledged within one tick",
    amendment: {
      id: "AM-1",
      criterion,
      proposedWording: "no fill after cancel is acknowledged within one tick",
      why: "the literal wording is stricter than the design",
      raisedBy: "worker",
      status: "proposed",
      previousContractVersion: K,
    },
  });
  const base = settledPhase({
    decisions: [amendment],
    contract: { ...basePhase().contract, acceptance: [criterion] },
  });
  const ballot = (reviewer: string, vote: "approve" | "reject"): Ballot => ({
    reviewer,
    decisionId: "D-amend",
    vote,
    rationale: vote === "reject" ? "retain the criterion unchanged" : "the proposed wording is satisfiable",
    evidence: ["reviewed the candidate diff"],
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });

  // The reviewer who proposed "retain unchanged" is counted against.
  const against = { ...base, ballots: [ballot("M", "reject"), ballot("A", "approve"), ballot("B", "approve")] };
  assert.notEqual(tally(amendment, against.ballots, against.findings, "C1", K, ["M", "A", "B"], "M"), "pass");
  assert.equal(amendmentToApply(against, "C1", K), undefined, "a rejected amendment is never applied");

  // With the reviewer's approval instead, the amendment passes and applies.
  const forIt = { ...base, ballots: [ballot("M", "approve"), ballot("A", "approve"), ballot("B", "approve")] };
  assert.equal(tally(amendment, forIt.ballots, forIt.findings, "C1", K, ["M", "A", "B"], "M"), "pass");
  assert.equal(amendmentToApply(forIt, "C1", K)?.id, "D-amend", "an approved amendment is applied");
});

test("plan 06k1: a reviewer's no-change dispute creates no amendment record and rejects the worker's", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 5_000, workerAttemptMs: 30_000, freezeMs: 20_000, checkMs: 20_000, probeMs: 20_000, reviewMs: 30_000 },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'work\n' > work.txt" },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [],
            assumptions: [],
            deviations: [],
            criterionDispute: { criterion: "it works", why: "the literal wording is stricter than the design", proposedWording: "the tests pass" },
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => {
      const workerAmendment = state.phase.decisions.find((d) => d.amendment?.raisedBy === "worker");
      const findings =
        reviewer === "M"
          ? [
              {
                kind: "defect",
                severity: "advisory",
                evidence: "README.md:1 the criterion should stand as written",
                criterionDispute: { criterion: "it works", why: "keep it", proposedWording: "retain the criterion unchanged" },
              },
            ]
          : [];
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: {
              reviewer,
              phaseId: "p1",
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
              findings,
              ballots: workerAmendment
                ? [
                    {
                      decisionId: workerAmendment.id,
                      vote: reviewer === "M" ? "reject" : "approve",
                      rationale: reviewer === "M" ? "retain the criterion unchanged" : "the proposed wording is satisfiable",
                      evidence: ["reviewed the candidate diff"],
                    },
                  ]
                : [],
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase), 120_000, 50, setup.runDir);
    const phase = setup.conductor.state.phase;
    // The reviewer's no-change wording created NO amendment record...
    assert.equal(
      phase.decisions.filter((d) => d.amendment?.raisedBy === "M").length,
      0,
      "a no-change dispute is never an amendment of its own",
    );
    // ... and it counted against the worker's amendment, so the criterion is
    // unchanged and the amendment was never applied.
    const workerAmendment = phase.decisions.find((d) => d.amendment?.raisedBy === "worker");
    assert.ok(workerAmendment, "the worker's amendment exists");
    assert.ok(phase.contract.acceptance.includes("it works"), "the criterion is unchanged");
    assert.ok(!phase.contract.acceptance.includes("the tests pass"), "the worker's wording was not applied");
    assert.equal(phase.phase, "DONE", `expected DONE; got ${phase.phase}`);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
