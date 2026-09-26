// Plan 01d (01_ref_runtime_f8ecf5e3.md §5): nothing used to enforce a
// complete ballot, so a reviewer that skipped records its turn-2 prompt
// listed failed the tally as "missing ballot" and cost a whole repair round
// (≈ 40–60 min). The conductor now records, per reviewer dispatch, the
// votable records (delegated or reserved) that dispatch's turn-2 prompt
// listed — minus records marked carried — and rejects a `submit_review` that
// gives no ballot for one of them, naming every missing id and its one-line
// choice, so the reviewer resubmits within the same turn. At most two such
// rejections are allowed; after that the review is accepted as-is and logged
// as `incomplete_review`, so a stubborn model cannot wedge the turn. A record
// that appears after the prompt was built (a late discovery) is never
// demanded.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor, type FakePiStep } from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";
import type { Decision, Reviewer, State } from "../../src/core/types.ts";

type Setup = Awaited<ReturnType<typeof setupConductor>>;

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
};

function disclosure(choice: string) {
  return {
    choice,
    whyItMatters: "The plan left this choice open and callers depend on the answer",
    alternatives: [{ option: "the other obvious behaviour", consequence: "would surprise callers" }],
    recommendation: { choice, reason: "keeps the contract's own promise" },
    classProposal: "delegated",
  };
}

function ballot(d: Decision) {
  return { decisionId: d.id, vote: "approve" as const, rationale: "consistent with the goal", evidence: ["src/sum.js:1"] };
}

function reviewArgs(reviewer: Reviewer, state: State, extra: Record<string, unknown> = {}) {
  return {
    reviewer,
    phaseId: state.phase.phaseId,
    candidateSha: state.phase.candidate?.sha,
    contractVersion: state.phase.contract.contractVersion,
    correctionStatements: [],
    findingStatements: [],
    ...extra,
  };
}

async function runToTerminal(setup: Setup, timeoutMs = 90_000): Promise<string> {
  await setup.conductor.start();
  await waitFor(
    () => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase),
    timeoutMs,
    50,
    setup.runDir,
  );
  return setup.conductor.state.phase.phase;
}

async function teardown(setup: Setup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

test("complete-ballots: a submit omitting two listed records is rejected naming both ids, and the corrected resubmission is accepted", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > sum.js" },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [disclosure("first choice"), disclosure("second choice"), disclosure("third choice")],
            assumptions: [],
            deviations: [],
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const ds = state.phase.decisions.filter((d) => d.source === "worker");
      const steps: FakePiStep[] = [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
      ];
      if (reviewer === "M") {
        // M's first ballot covers only the first record; the other two are
        // missing, so the conductor must reject this submission.
        steps.push({ kind: "call-submit", tool: "submit_review", args: reviewArgs("M", state, { ballots: [ballot(ds[0])], findings: [] }) });
      }
      steps.push({ kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, { ballots: ds.map(ballot), findings: [] }) });
      return { hello: defaultReviewerHello(), steps };
    },
  });
  try {
    assert.equal(await runToTerminal(setup), "DONE", "the round passes without a repair once the ballot is complete");

    const decisions = setup.conductor.state.phase.decisions.filter((d) => d.source === "worker");
    assert.equal(decisions.length, 3);
    const records = readEvents(setup.runDir);
    const rejected = records.filter((r) => r.kind === "incomplete_review_rejected");
    assert.equal(rejected.length, 1, "M's one incomplete submit was rejected");
    const missing = (rejected[0].event as { missing: string[] }).missing;
    assert.deepEqual(new Set(missing), new Set([decisions[1].id, decisions[2].id]), "the rejection names both omitted records");
    assert.ok(!records.some((r) => r.kind === "incomplete_review"), "the corrected resubmission was complete");

    // The rejection the model actually saw names every missing id and the
    // record's one-line choice.
    const streamDir = runPaths(setup.runDir).stream;
    const mStream = fs.readdirSync(streamDir).find((f) => f.startsWith("reviewer-M-"));
    assert.ok(mStream, "the M reviewer's RPC stream was written");
    const streamText = fs.readFileSync(path.join(streamDir, mStream!), "utf8");
    for (const d of [decisions[1], decisions[2]]) {
      assert.ok(streamText.includes(d.id), `the rejection message names ${d.id}`);
      assert.ok(streamText.includes(d.choice), `the rejection message gives ${d.id}'s choice`);
    }
  } finally {
    await teardown(setup);
  }
});

test("complete-ballots: a reviewer that omits a record three times is accepted on the third submission with an incomplete_review log naming the id", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > sum.js" },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: { decisions: [disclosure("first choice"), disclosure("second choice")], assumptions: [], deviations: [] },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const ds = state.phase.decisions.filter((d) => d.source === "worker");
      const steps: FakePiStep[] = [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
      ];
      if (reviewer === "M") {
        // Three submissions, each omitting the second record: two rejections,
        // then the review is accepted as-is on the third.
        const incomplete: FakePiStep = { kind: "call-submit", tool: "submit_review", args: reviewArgs("M", state, { ballots: [ballot(ds[0])], findings: [] }) };
        steps.push(incomplete, { ...incomplete }, { ...incomplete });
      } else {
        steps.push({ kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, { ballots: ds.map(ballot), findings: [] }) });
      }
      return { hello: defaultReviewerHello(), steps };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "incomplete_review"), 90_000, 50, setup.runDir);
    const records = readEvents(setup.runDir);
    const rejected = records.filter(
      (r) => r.kind === "incomplete_review_rejected" && (r.event as { reviewer: string }).reviewer === "M",
    );
    assert.equal(rejected.length, 2, "two rejections, then the review is accepted as-is");
    const accepted = records.find((r) => r.kind === "incomplete_review")!;
    const missing = (accepted.event as { missing: string[] }).missing;
    assert.equal(missing.length, 1, "the accepted-as-is review names the one record it omitted");
    assert.equal((accepted.event as { reviewer: string }).reviewer, "M");
  } finally {
    await teardown(setup);
  }
});

test("complete-ballots: a carried record is not demanded — reviewers may rely on the carried ballots", async () => {
  let setupRef: Setup | undefined;
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt) => {
      if (attempt === 1) {
        return {
          hello: defaultWorkerHello(),
          steps: [
            { kind: "call-sh", command: "printf 'one\\n' > sum.js" },
            { kind: "call-submit", tool: "submit_phase", args: { decisions: [disclosure("the kept choice")], assumptions: [], deviations: [] } },
          ],
        };
      }
      const prior = setupRef!.conductor.state.phase.decisions.find((d) => d.source === "worker")!;
      return {
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-sh", command: "printf 'two\\n' > sum.js" },
          {
            kind: "call-submit",
            tool: "submit_phase",
            args: { decisions: [], assumptions: [], deviations: [], priorDecisions: [{ id: prior.id, status: "kept" }] },
          },
        ],
      };
    },
    reviewerScriptFor: (reviewer, state) => {
      const d = state.phase.decisions.find((x) => x.source === "worker")!;
      const round = state.phase.round ?? 1;
      // Round 1 approves the decision but M raises a blocking finding, so the
      // round repairs. Round 2 keeps the decision (its ballots carry forward)
      // and only M confirms the finding.
      const findings =
        round === 1 && reviewer === "M"
          ? [{ kind: "defect", severity: "blocking", evidence: "sum.js:1 — the value is not validated before use" }]
          : [];
      const findingStatements =
        round === 2 && reviewer === "M" && state.phase.findings.some((f) => f.raisedBy === "M")
          ? [{ findingId: state.phase.findings.find((f) => f.raisedBy === "M")!.id, status: "confirm" }]
          : [];
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          {
            kind: "call-submit",
            tool: "submit_review",
            args: reviewArgs(reviewer, state, {
              ballots: round === 1 ? [ballot(d)] : [],
              findings,
              findingStatements,
            }),
          },
        ],
      };
    },
  });
  setupRef = setup;
  try {
    assert.equal(await runToTerminal(setup, 120_000), "DONE");
    assert.ok(
      !readEvents(setup.runDir).some((r) => r.kind === "incomplete_review" || r.kind === "incomplete_review_rejected"),
      "a carried record is never demanded, so no review was rejected",
    );
    const kept = setup.conductor.state.phase.decisions.find((x) => x.source === "worker")!;
    assert.equal(kept.boundCandidateSha, setup.conductor.state.phase.candidate!.sha, "the kept decision is live on the new candidate");
    assert.ok(
      setup.conductor.state.phase.ballots.some((b) => b.decisionId === kept.id && b.carriedFrom),
      "its carried ballots stand in for a new vote",
    );
  } finally {
    await teardown(setup);
  }
});

test("complete-ballots: a discovery that arrives after the reviewer's turn-2 prompt is never demanded", async () => {
  let bDispatches = 0;
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: { ...FAST, reviewMs: 5_000 },
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: "printf 'x\\n' > sum.js" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [disclosure("the only known choice")], assumptions: [], deviations: [] } },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const d = state.phase.decisions.find((x) => x.source === "worker")!;
      if (reviewer === "B" && ++bDispatches === 1) {
        // B's first dispatch never submits turn 2, so it times out and is
        // re-dispatched — and by then the discovery barrier has released.
        return {
          hello: defaultReviewerHello(),
          steps: [
            { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
            { kind: "wait-for-prompt" },
            { kind: "hang-forever" },
          ],
        };
      }
      const discoveries =
        reviewer === "B" ? [{ ...disclosure("LATE-ONLY: validates both arguments before adding"), classProposal: "delegated" }] : [];
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries } },
          { kind: "wait-for-prompt" },
          // B ballots only the record its turn-2 prompt listed; the late
          // discovery is never demanded.
          { kind: "call-submit", tool: "submit_review", args: reviewArgs(reviewer, state, { ballots: [ballot(d)], findings: [] }) },
        ],
      };
    },
  });
  try {
    assert.equal(await runToTerminal(setup), "DONE");
    const records = readEvents(setup.runDir);
    const late = records.filter((r) => r.kind === "late_discovery");
    assert.equal(late.length, 1, "B's re-dispatched discovery is logged as a late observation");
    assert.ok(!setup.conductor.state.phase.decisions.some((x) => x.choice.startsWith("LATE-ONLY")), "a late discovery is never a votable record");
    // The prompt-time demand snapshot for B's re-dispatch is the only place a
    // ballot could be demanded from, and it holds just the one live record —
    // the late discovery is not in it.
    const demanded = records.filter((r) => r.kind === "ballot_demanded" && (r.event as { reviewer: string }).reviewer === "B");
    assert.equal(demanded.length, 2, "one demand snapshot per B dispatch");
    const live = setup.conductor.state.phase.decisions.find((x) => x.source === "worker")!;
    assert.deepEqual(
      (demanded[1].event as { records: Array<{ id: string }> }).records.map((r) => r.id),
      [live.id],
      "B's second demand snapshot lists only the record its turn-2 prompt listed",
    );
    assert.ok(
      !records.some((r) => r.kind === "incomplete_review" || r.kind === "incomplete_review_rejected"),
      "the late record is never demanded, so no review was rejected",
    );
  } finally {
    await teardown(setup);
  }
});
