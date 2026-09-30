// Plan 05e: resolution and severity across rounds.
//
//  - every later round's reviewers list each earlier-round finding/blocker
//    and mark it resolved / open; a 2-of-3 `resolved` majority moves the
//    message to `resolved` (it leaves the owner's live view);
//  - a `sameAs` re-raise takes the re-raiser's severity (finding #32), so a
//    narrower advisory re-raise of a fixed blocking finding no longer blocks;
//  - a resubmission of bytes the reviewers already approved re-reviews only
//    the amended criterion; a new blocking point on the unchanged code is
//    advisory (finding #34).

import assert from "node:assert/strict";
import * as fs from "node:fs";
import { test } from "node:test";

import { accept } from "../../src/core/predicate.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { State } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
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

const DISPUTE = { criterion: "it works", why: "the criterion cannot hold as written", proposedWording: "it really works" };

/** A worker whose second attempt changes the tree (round 1 → repair). */
function workerAttempt(dispute: boolean) {
  return (attempt: number) => ({
    hello: defaultWorkerHello(),
    steps: [
      ...(attempt === 1 ? [{ kind: "call-sh" as const, command: "printf 'one\\n' > a.txt" }] : [{ kind: "call-sh" as const, command: "printf 'two\\n' > b.txt" }]),
      {
        kind: "call-submit" as const,
        tool: "submit_phase" as const,
        args: { decisions: [], assumptions: [], deviations: [], ...(attempt === 1 && dispute ? { criterionDispute: DISPUTE } : {}) },
      },
    ],
  });
}

/** Round 1 raises one blocking finding (citing an acceptance item, so it
 * stays blocking); the round-2 reviewers mark it resolved/open. */
function resolutionReviewer(opts: { resolved: boolean }) {
  return (reviewer: string, state: State) => {
    const round = state.phase.round ?? 1;
    const finding = state.phase.findings.find((f) => f.raisedBy === "M");
    const findingMessage = finding ? (state.phase.messages ?? []).find((m) => m.sourceRecordId === finding.id) : undefined;
    const disputed = state.phase.decisions;
    const votable = disputed.filter((d) => (d.class === "delegated" || d.class === "reserved") && !d.supersededBy && !d.supersededByCorrection);
    return {
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        ...(round === 2 && finding ? [{ kind: "sleep" as const, ms: 0 }] : []),
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
            ballots: round === 1 ? votable.map((d) => ({ decisionId: d.id, vote: "approve", rationale: "right for the goal", evidence: ["src/a.ts:1"] })) : [],
            findings: round === 1 && reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "the acceptance item 'it works' does not hold" }] : [],
            ...(round === 2 && finding
              ? {
                  resolutionStatements: [
                    {
                      messageId: findingMessage?.id ?? "F-1",
                      status: reviewer === "B" ? "open" : opts.resolved ? "resolved" : "open",
                      evidence: reviewer === "B" ? "still broken" : "fixed in b.txt",
                    },
                  ],
                }
              : {}),
          },
        },
      ],
    };
  };
}

function findingEvaluator() {
  return (messageType: string) => ({
    hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
    steps:
      messageType === "finding"
        ? [{ kind: "call-submit", tool: "submit_evaluation", args: { evaluations: [{ messageId: "F-1", action: "publish", title: "the criterion does not hold", summary: "not validated", context: "src/a.ts:1", evidence: ["src/a.ts:1"], verified: "src/a.ts:1" }] } }]
        : [],
  });
}

test("plan 05e: a finding marked resolved by 2 of 3 reviewers moves to resolved and leaves review.org's live entries", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-05e-resolve-prompts-");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: workerAttempt(false),
    reviewerScriptFor: resolutionReviewer({ resolved: true }),
    evaluatorScriptFor: findingEvaluator(),
    extraReviewerEnv: () => ({ FAKE_PI_PROMPT_LOG: `${promptDir}/reviewer.log` }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 120_000, 20, setup.runDir);
    const message = (setup.conductor.state.phase.messages ?? []).find((m) => m.id === "F-1")!;
    assert.equal(message.state, "resolved", "a 2-of-3 resolved majority closes the finding");
    assert.equal(message.settlement?.settledBy, "vote");
    const review = fs.readFileSync(`${setup.runDir}/views/review.org`, "utf8");
    assert.doesNotMatch(review, /\bF-1\b/, "a resolved message leaves the live view");
    // Round-3 reviews M-8/A-11/B-14: the turn-2 prompt must actually LIST the
    // earlier round's finding, even though MESSAGE_CARRIED rebound it to the
    // current candidate.
    const prompt = fs.readFileSync(`${promptDir}/reviewer.log`, "utf8");
    assert.match(prompt, /Earlier rounds' live findings and blockers/);
    assert.match(prompt, /- F-1 \[finding, published\]/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(promptDir, { recursive: true, force: true });
  }
});

test("plan 05e: a finding marked open by 2 of 3 reviewers stays live", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: workerAttempt(false),
    reviewerScriptFor: resolutionReviewer({ resolved: false }),
    evaluatorScriptFor: findingEvaluator(),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.round !== undefined && (setup.conductor.state.phase.round ?? 1) >= 2, 120_000, 20, setup.runDir);
    await waitFor(() => (setup.conductor.state.phase.messages ?? []).some((m) => m.id === "F-1" && m.state === "published"), 120_000, 20, setup.runDir);
    // Let the round-2 reviews settle.
    await waitFor(() => setup.conductor.state.phase.reviews.A?.review?.candidateSha === setup.conductor.state.phase.candidate?.sha, 120_000, 20, setup.runDir);
    const message = (setup.conductor.state.phase.messages ?? []).find((m) => m.id === "F-1")!;
    assert.notEqual(message.state, "resolved", "a 2-of-3 open majority leaves the finding live");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (finding #32): a narrower advisory sameAs re-raise of a blocking finding makes it advisory and no longer blocks", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: workerAttempt(false),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const finding = state.phase.findings.find((f) => f.raisedBy === "M");
      const votable = state.phase.decisions.filter((d) => (d.class === "delegated" || d.class === "reserved") && !d.supersededBy && !d.supersededByCorrection);
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
              ballots: round === 1 ? votable.map((d) => ({ decisionId: d.id, vote: "approve", rationale: "right for the goal", evidence: ["src/a.ts:1"] })) : [],
              findings:
                round === 1 && reviewer === "M"
                  ? [{ kind: "defect", severity: "blocking", evidence: "the acceptance item 'it works' does not hold" }]
                  : round === 2 && reviewer === "A" && finding
                    ? [{ kind: "defect", severity: "advisory", evidence: "a narrower point about the same code", sameAs: finding.id }]
                    : [],
            },
          },
        ],
      };
    },
    evaluatorScriptFor: findingEvaluator(),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 120_000, 20, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.raisedBy === "M")!;
    assert.equal(finding.severity, "advisory", "the sameAs re-raise takes the re-raiser's severity");
    assert.equal(setup.conductor.state.phase.phase, "DONE", "an advisory finding no longer blocks acceptance");
    // An advisory finding stays a live message in front of the owner.
    const message = (setup.conductor.state.phase.messages ?? []).find((m) => m.sourceRecordId === finding.id)!;
    assert.equal(message.state, "published");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (finding #34): an amendment-only resubmission of identical bytes re-reviews only the amended criterion; a new blocking point on unchanged code is advisory and acceptance proceeds", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-05e-prompts-");
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    // Round 1 writes a.txt and raises the criterion dispute; round 2 writes
    // nothing, so the resubmitted tree is byte-identical to C1.
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        ...(attempt === 1 ? [{ kind: "call-sh" as const, command: "printf 'one\\n' > a.txt" }] : []),
        {
          kind: "call-submit" as const,
          tool: "submit_phase" as const,
          args: { decisions: [], assumptions: [], deviations: [], ...(attempt === 1 ? { criterionDispute: DISPUTE } : {}) },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const votable = state.phase.decisions.filter((d) => (d.class === "delegated" || d.class === "reserved") && !d.supersededBy && !d.supersededByCorrection);
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
              // The amendment decision is reserved and demanded in round 1;
              // approving it lets it pass and forces the re-review round.
              ballots: votable
                .filter((d) => round === 1 || d.amendment?.status === "proposed")
                .map((d) => ({ decisionId: d.id, vote: "approve", rationale: "right for the goal", evidence: ["src/a.ts:1"] })),
              // Round 2: a NEW blocking point on the unchanged code, citing
              // no acceptance item or reserved rule.
              findings: round === 2 && reviewer === "A" ? [{ kind: "defect", severity: "blocking", evidence: "the helper's name is misleading" }] : [],
            },
          },
        ],
      };
    },
    extraReviewerEnv: () => ({ FAKE_PI_PROMPT_LOG: `${promptDir}/reviewer.log` }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 150_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    const finding = phase.findings.find((f) => f.raisedBy === "A");
    assert.ok(finding, "A raised the new point");
    assert.equal(finding!.severity, "advisory", "a new blocking point on unchanged approved bytes is advisory");
    assert.equal(phase.phase, "DONE", "acceptance proceeds");
    if (phase.candidate) assert.equal(accept(phase, phase.candidate.sha, phase.contract.contractVersion), true);
    const prompt = fs.readFileSync(`${promptDir}/reviewer.log`, "utf8");
    assert.match(prompt, /amendment-only resubmission/);
    assert.match(prompt, /Review ONLY the amended criterion/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(promptDir, { recursive: true, force: true });
  }
});

test("plan 05e (round-2 A-5): an advisory sameAs re-raise cannot raise a finding to blocking", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: workerAttempt(false),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const advisory = state.phase.findings.find((f) => f.raisedBy === "M" && f.severity === "advisory");
      const votable = state.phase.decisions.filter((d) => (d.class === "delegated" || d.class === "reserved") && !d.supersededBy && !d.supersededByCorrection);
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
              ballots: round === 1 ? votable.map((d) => ({ decisionId: d.id, vote: "approve", rationale: "right for the goal", evidence: ["src/a.ts:1"] })) : [],
              findings:
                round === 1 && reviewer === "M"
                  ? [
                      { kind: "defect", severity: "advisory", evidence: "the helper's name is misleading" },
                      { kind: "defect", severity: "blocking", evidence: "the acceptance item 'it works' does not hold" },
                    ]
                  : round === 2 && reviewer === "A" && advisory
                    ? [{ kind: "defect", severity: "blocking", evidence: "the same naming point, now claimed blocking", sameAs: advisory.id }]
                    : [],
            },
          },
        ],
      };
    },
    evaluatorScriptFor: (messageType) => ({
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps:
        messageType === "finding"
          ? [
              {
                kind: "call-submit",
                tool: "submit_evaluation",
                args: {
                  evaluations: [
                    { messageId: "F-1", action: "publish", title: "the helper's name is misleading", summary: "naming", context: "src/a.ts:1", evidence: ["src/a.ts:1"], verified: "src/a.ts:1" },
                    { messageId: "F-2", action: "publish", title: "the criterion does not hold", summary: "not validated", context: "src/b.ts:1", evidence: ["src/b.ts:1"], verified: "src/b.ts:1" },
                  ],
                },
              },
            ]
          : [],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.raisedBy === "M" && f.severity === "advisory" && (f.alsoRaisedBy ?? []).includes("A")),
      120_000,
      20,
      setup.runDir,
    );
    const advisory = setup.conductor.state.phase.findings.find((f) => f.raisedBy === "M" && f.severity === "advisory")!;
    assert.equal(advisory.severity, "advisory", "one reviewer's sameAs re-raise cannot raise a finding to blocking");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 05e (round-4 A-16): on an amendment-only round a blockers-list point is an advisory finding, not a blocker panel item", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        ...(attempt === 1 ? [{ kind: "call-sh" as const, command: "printf 'one\\n' > a.txt" }] : []),
        {
          kind: "call-submit" as const,
          tool: "submit_phase" as const,
          args: { decisions: [], assumptions: [], deviations: [], ...(attempt === 1 ? { criterionDispute: DISPUTE } : {}) },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const votable = state.phase.decisions.filter((d) => (d.class === "delegated" || d.class === "reserved") && !d.supersededBy && !d.supersededByCorrection);
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
              ballots: votable
                .filter((d) => round === 1 || d.amendment?.status === "proposed")
                .map((d) => ({ decisionId: d.id, vote: "approve", rationale: "right for the goal", evidence: ["src/a.ts:1"] })),
              // Round 2: a NEW point on the unchanged code filed through the
              // blockers list, citing no acceptance item.
              blockers: round === 2 && reviewer === "A" ? [{ kind: "defect", evidence: "the helper's name is misleading" }] : [],
            },
          },
        ],
      };
    },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 150_000, 20, setup.runDir);
    const phase = setup.conductor.state.phase;
    assert.equal(
      (phase.messages ?? []).filter((m) => m.type === "blocker").length,
      0,
      "no blocker message may be raised on unchanged approved code",
    );
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "PANEL_VOTE").length,
      0,
      "no blocker panel may run on unchanged approved code",
    );
    const finding = phase.findings.find((f) => f.raisedBy === "A")!;
    assert.equal(finding.severity, "advisory");
    assert.equal(phase.phase, "DONE");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
