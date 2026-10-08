// Plan 05e: the round panel. After the evaluators publish, every published
// trade-off that no three reviewers balloted, and every published blocking
// finding, goes to ONE panel of three fresh agents per round. A `keep`
// majority publishes a trade-off to the owner (and keeps a finding blocking);
// otherwise the trade-off is dropped (its message file keeps the three
// reasons) and the finding becomes advisory.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { accept } from "../../src/core/predicate.ts";
import { roundPanelItemsNeedingVote } from "../../src/core/predicate.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { State } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";

const FAST = {
  abortGraceMs: 60,
  termGraceMs: 60,
  helloTimeoutMs: 5_000,
  checkMs: 20_000,
  freezeMs: 10_000,
  workerAttemptMs: 20_000,
  reviewMs: 15_000,
  probeMs: 4_000,
  evaluateMs: 15_000,
  panelMs: 15_000,
};

function workerDelegated() {
  return {
    kind: "call-submit",
    tool: "submit_phase",
    args: {
      decisions: [
        {
          choice: "the cancel path batches per tick",
          whyItMatters: "it keeps the cancel path inside its latency budget",
          alternatives: [{ option: "a lock per request", consequence: "the path exceeds its budget under load" }],
          recommendation: { choice: "batch per tick", reason: "the budget is the plan's goal" },
          classProposal: "delegated",
        },
      ],
      assumptions: [],
      deviations: [],
    },
  };
}

/** A reviewer that discovers one detail choice in turn 1 and ballots the
 * worker's record in turn 2. */
function discoveringReviewer() {
  return (reviewer: string, state: State) => {
    const worker = state.phase.decisions.find((d) => d.source === "worker");
    const C = state.phase.candidate?.sha;
    // A re-dispatched seat (a timeout under load) must not discover the same
    // choice twice: its own earlier discovery already exists on this
    // candidate. The script is keyed by agent id, so a retry re-runs it.
    const alreadyDiscovered = state.phase.decisions.some(
      (d) => d.source === "reviewer-discovered" && d.boundCandidateSha === C && d.id.includes(`-disc-${reviewer}-`),
    );
    return {
      hello: defaultReviewerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_discovery",
          args: {
            // Both M and A discover one detail choice: two pending items, so
            // the same single panel (three seats) must cover both at once.
            discoveries:
              reviewer === "B" || alreadyDiscovered
                ? []
                : [
                    {
                      choice: reviewer === "M" ? "the helper is private" : "the cache is per phase",
                      whyItMatters: "it fixes the module's surface",
                      alternatives: [{ option: "export it", consequence: "callers can couple to it" }],
                      recommendation: { choice: "keep it private", reason: "smaller surface" },
                      classProposal: "detail",
                    },
                  ],
          },
        },
        { kind: "wait-for-prompt" },
        {
          kind: "call-submit",
          tool: "submit_review",
          args: {
            reviewer,
            phaseId: state.phase.phaseId,
            candidateSha: state.phase.candidate?.sha,
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
            ballots: worker
              ? [{ decisionId: worker.id, vote: "approve", rationale: "the batch is right for the budget", evidence: ["src/cancel.ts:10"] }]
              : [],
            findings: [],
          },
        },
      ],
    };
  };
}

function evaluatorPublishingBoth() {
  return (messageType: string) => ({
    hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
    steps:
      messageType === "tradeoff"
        ? [
            {
              kind: "call-submit",
              tool: "submit_evaluation",
              args: {
                evaluations: [
                  { messageId: "T-1", action: "publish", title: "batch cancels per tick", summary: "keeps the path in budget", context: "src/cancel.ts:10", evidence: ["src/cancel.ts:10"] },
                  { messageId: "T-2", action: "publish", title: "the helper is private", summary: "smaller surface", context: "src/helper.ts:1", evidence: ["src/helper.ts:1"] },
                  { messageId: "T-3", action: "publish", title: "the cache is per phase", summary: "one cache per phase", context: "src/cache.ts:1", evidence: ["src/cache.ts:1"] },
                ],
              },
            },
          ]
        : [],
  });
}

function events(setup: TestConductorSetup) {
  return readEvents(setup.runDir).filter((r) => r.kind === "event").map((r) => r.event as { type: string } & Record<string, unknown>);
}

function roundPanelVotes(setup: TestConductorSetup): Array<{ seat: number; votes: Array<{ messageId: string; verdict: string; reason: string }> }> {
  return events(setup)
    .filter((e) => e.type === "ROUND_PANEL_VOTE")
    .map((e) => e as unknown as { seat: number; votes: Array<{ messageId: string; verdict: string; reason: string }> });
}

test("plan 05e: a reviewer-discovered trade-off is voted by a three-seat panel; with 2 keep it is published; a worker trade-off already balloted by M, A and B goes to no panel", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [workerDelegated()] }),
    reviewerScriptFor: discoveringReviewer(),
    evaluatorScriptFor: evaluatorPublishingBoth(),
    roundPanelScriptFor: (seat, state) => {
      const items = roundPanelItemsNeedingVote(state.phase);
      return {
        hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
        steps: [
          {
            kind: "call-submit",
            tool: "submit_round_panel_votes",
            args: {
              votes: items.map((messageId) => ({
                messageId,
                verdict: seat <= 2 ? "keep" : "drop",
                reason: `seat ${seat} on ${messageId}`,
              })),
            },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const t1 = phase.messages.find((m) => m.id === "T-1")!;
    const t2 = phase.messages.find((m) => m.id === "T-2")!;
    // The worker trade-off carried M, A and B's ballots, so it was never a
    // panel item; the reviewer-discovered detail trade-off was.
    assert.equal(t1.panelOutcome, undefined, "a fully-balloted worker trade-off goes to no panel");
    assert.equal(t2.state, "published");
    assert.equal(t2.panelOutcome, "keep");
    // Exactly one panel of three seats voted, and every seat voted on T-2.
    const t3 = phase.messages.find((m) => m.id === "T-3")!;
    assert.equal(t3.state, "published");
    const votes = roundPanelVotes(setup);
    assert.equal(votes.length, 3, "one panel (three seats) per round");
    for (const seat of votes) {
      assert.deepEqual(
        seat.votes.map((v) => v.messageId).sort(),
        ["T-2", "T-3"],
        "the one panel covers EVERY pending item and only the pending items",
      );
    }
    // Plan 05e criterion 2: one panel per round (three dispatch actions).
    assert.equal(events(setup).filter((e) => e.type === "ROUND_PANEL_VOTE").length, 3);
    const dispatchActions = readEvents(setup.runDir).filter(
      (r) => r.kind === "event" && (r.event as { type?: string; action?: string }).type === "ACTION_STARTED" && (r.event as { action?: string }).action === "dispatch_round_panel",
    );
    assert.equal(dispatchActions.length, 3, "one dispatch per seat, all three items batched");
    // The message file keeps every seat's reason.
    const file = fs.readFileSync(path.join(setup.runDir, "views", "messages", "T-2.org"), "utf8");
    assert.match(file, /seat 1/);
    assert.match(file, /seat 2/);
    assert.match(file, /seat 3/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e: a reviewer-discovered trade-off with 2 drop is dropped, and its message file shows all three reasons", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [workerDelegated()] }),
    reviewerScriptFor: discoveringReviewer(),
    evaluatorScriptFor: evaluatorPublishingBoth(),
    roundPanelScriptFor: (seat, state) => {
      const items = roundPanelItemsNeedingVote(state.phase);
      return {
        hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
        steps: [
          {
            kind: "call-submit",
            tool: "submit_round_panel_votes",
            args: {
              votes: items.map((messageId) => ({
                messageId,
                verdict: seat <= 2 ? "drop" : "keep",
                reason: `seat ${seat} reason on ${messageId}`,
              })),
            },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const t2 = setup.conductor.state.phase.messages.find((m) => m.id === "T-2")!;
    assert.equal(t2.state, "dropped");
    assert.equal(t2.panelOutcome, "drop");
    const file = fs.readFileSync(path.join(setup.runDir, "views", "messages", "T-2.org"), "utf8");
    assert.match(file, /seat 1 reason/);
    assert.match(file, /seat 2 reason/);
    assert.match(file, /seat 3 reason/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e: a blocking finding goes to the panel with the round's trade-offs (one dispatch); with 2 drop it becomes advisory and acceptance proceeds", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [workerDelegated()] }),
    reviewerScriptFor: (reviewer, state) => {
      const worker = state.phase.decisions.find((d) => d.source === "worker");
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
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
              ballots: worker ? [{ decisionId: worker.id, vote: "approve", rationale: "right for the budget", evidence: ["src/cancel.ts:10"] }] : [],
              findings: reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "the acceptance item 'it works' does not hold" }] : [],
            },
          },
        ],
      };
    },
    evaluatorScriptFor: (messageType) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps:
        messageType === "tradeoff"
          ? [{ kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [{ messageId: "T-1", action: "publish", title: "batch cancels", summary: "in budget", context: "src/cancel.ts:10", evidence: ["src/cancel.ts:10"] }] } }]
          : messageType === "finding"
            ? [{ kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [{ messageId: "F-1", action: "publish", title: "the criterion does not hold", summary: "not validated", context: "src/b.ts:1", evidence: ["src/b.ts:1"], verified: "src/b.ts:1" }] } }]
            : [],
    }),
    roundPanelScriptFor: (seat, state) => {
      const items = roundPanelItemsNeedingVote(state.phase);
      return {
        hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
        steps: [
          {
            kind: "call-submit",
            tool: "submit_round_panel_votes",
            args: { votes: items.map((messageId) => ({ messageId, verdict: seat <= 2 ? "downgrade" : "keep", reason: `seat ${seat} on ${messageId}` })) },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const finding = phase.findings.find((f) => f.raisedBy === "M")!;
    assert.equal(finding.severity, "advisory", "2 downgrade makes the blocking finding advisory (2 drop behaves the same)");
    const message = phase.messages.find((m) => m.id === "F-1")!;
    assert.equal(message.state, "published");
    assert.equal(message.panelOutcome, "downgrade");
    // The finding and the trade-off were handled by the SAME single panel.
    const votes = roundPanelVotes(setup);
    assert.equal(votes.length, 3);
    const votedIds = new Set(votes.flatMap((s) => s.votes.map((v) => v.messageId)));
    assert.ok(votedIds.has("F-1"), "the blocking finding was a panel item");
    assert.ok(!votedIds.has("T-1"), "the fully-balloted worker trade-off was not a panel item");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e: with 2 keep a blocking finding stays blocking and the candidate is not acceptable", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [workerDelegated()] }),
    reviewerScriptFor: (reviewer, state) => {
      const worker = state.phase.decisions.find((d) => d.source === "worker");
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
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
              ballots: worker ? [{ decisionId: worker.id, vote: "approve", rationale: "right for the budget", evidence: ["src/cancel.ts:10"] }] : [],
              findings: reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "the acceptance item 'it works' does not hold" }] : [],
            },
          },
        ],
      };
    },
    evaluatorScriptFor: (messageType) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps:
        messageType === "finding"
          ? [{ kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [{ messageId: "F-1", action: "publish", title: "the criterion does not hold", summary: "not validated", context: "src/b.ts:1", evidence: ["src/b.ts:1"], verified: "src/b.ts:1" }] } }]
          : [],
    }),
    roundPanelScriptFor: (seat, state) => {
      const items = roundPanelItemsNeedingVote(state.phase);
      return {
        hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
        steps: [
          {
            kind: "call-submit",
            tool: "submit_round_panel_votes",
            args: { votes: items.map((messageId) => ({ messageId, verdict: seat <= 2 ? "keep" : "drop", reason: `seat ${seat} on ${messageId}` })) },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.raisedBy === "M"), 90_000, 20, setup.runDir);
    await waitFor(() => (setup.conductor.state.phase.messages ?? []).some((m) => m.panelOutcome === "keep"), 90_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const finding = phase.findings.find((f) => f.raisedBy === "M")!;
    assert.equal(finding.severity, "blocking", "2 keep keeps the finding blocking");
    if (phase.candidate) {
      assert.equal(accept(phase, phase.candidate.sha, phase.contract.contractVersion), false, "an open blocking finding blocks acceptance");
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
