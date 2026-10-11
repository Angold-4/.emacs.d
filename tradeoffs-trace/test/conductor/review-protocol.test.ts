// Work packet 2a: the real two-turn review protocol, fake-pi end to end.
// Named scenarios from the brief, all observable from ONE happy-path run
// (no repair round needed — see contract-objection.test.ts for that one):
//
//  - two-turn ordering: turn 2 content (the worker's disclosure) is never
//    sent before submit_discovery is accepted — asserted directly from the
//    event/log order (`discovery_submitted` before `REVIEW_SUBMITTED`, per
//    reviewer), not just inferred from the run finishing.
//  - reviewer-discovered record: each reviewer's submit_discovery produces
//    a `source: "reviewer-discovered"` Decision.
//  - unreferenced-hunks: a diff hunk no record cites appears in the sample.
//  - reproduction run: a finding's reproduction command is run by the
//    conductor and its result recorded.
//
// `boundary-trigger` (a `trigger` record) is its own test below: a
// `reserved`-class trigger blocks acceptance (design §3.4: only the owner
// may lower it, and owner commands are 2b's scope), so it cannot share this
// file's happy-path run to DONE.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import type { LogRecord } from "../../src/effects/log.ts";

function eventTypes(records: LogRecord[]): string[] {
  return records.filter((r) => r.kind === "event").map((r) => (r.event as { type: string }).type);
}

test("review-protocol: two-turn ordering, reviewer-discovered records, unreferenced hunks and a reproduction run", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        // A file no decision or finding will ever mention — its hunk must
        // show up in the sample as "unreferenced".
        { kind: "call-sh", command: "printf 'untouched\\n' > NOTES.txt" },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            decisions: [
              {
                choice: "Use a simple loop rather than a library helper",
                whyItMatters: "Keeps the change dependency-free, matching the plan's own zero-dependency goal",
                alternatives: [{ option: "pull in a small utility library", consequence: "adds a dependency for one function" }],
                recommendation: { choice: "keep the loop", reason: "no dependency needed for something this small" },
                classProposal: "delegated",
              },
            ],
            assumptions: [],
            deviations: [],
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const workerDecision = state.phase.decisions.find((d) => d.source === "worker");
      return {
        hello: defaultReviewerHello(),
        steps: [
          {
            kind: "call-submit",
            tool: "submit_discovery",
            args: {
              discoveries: [
                {
                  choice: `${reviewer} noticed the candidate keeps the existing file layout`,
                  whyItMatters: "A layout change would have made the diff harder to review",
                  alternatives: [{ option: "reorganize files", consequence: "a noisier diff for no behavioral gain" }],
                  recommendation: { choice: "keep the existing layout", reason: "smallest reviewable diff" },
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
              ballots: workerDecision
                ? [
                    {
                      decisionId: workerDecision.id,
                      vote: "approve",
                      rationale: "the loop is simple and correct",
                      evidence: ["reviewed the diff directly"],
                    },
                  ]
                : [],
              findings:
                reviewer === "M"
                  ? [
                      {
                        kind: "defect",
                        severity: "advisory",
                        evidence: "worth double-checking the loop terminates — reproduction command: `true`",
                        reproduction: { command: "true" },
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
    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 90_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE", `expected DONE; got ${JSON.stringify(setup.conductor.state.phase)}`);

    const records = readEvents(setup.runDir);
    const types = eventTypes(records);

    // two-turn ordering: for every reviewer, its own discovery_submitted
    // log record comes strictly before its REVIEW_SUBMITTED event.
    const discoveryIdx: Record<string, number> = {};
    const reviewIdx: Record<string, number> = {};
    records.forEach((r, i) => {
      if (r.kind === "discovery_submitted") {
        const reviewer = (r.event as unknown as { reviewer: string }).reviewer;
        discoveryIdx[reviewer] = i;
      }
      if (r.kind === "event" && (r.event as { type: string }).type === "REVIEW_SUBMITTED") {
        const reviewer = (r.event as unknown as { review: { reviewer: string } }).review.reviewer;
        reviewIdx[reviewer] = i;
      }
    });
    for (const reviewer of ["M", "A", "B"]) {
      assert.ok(discoveryIdx[reviewer] !== undefined, `${reviewer} should have a discovery_submitted record`);
      assert.ok(reviewIdx[reviewer] !== undefined, `${reviewer} should have a REVIEW_SUBMITTED event`);
      assert.ok(
        discoveryIdx[reviewer] < reviewIdx[reviewer],
        `${reviewer}'s submit_discovery must be logged before its submit_review (turn ordering)`,
      );
    }

    // reviewer-discovered record: one DECISION_ADDED per reviewer, source
    // reviewer-discovered.
    //
    // A reviewer whose first turn ended without an accepted review is
    // re-dispatched (REVIEW_TIMED_OUT), and its second turn 1 submits the same
    // discovery again — logged as a late observation and never a votable record
    // when the discovery barrier has already released, which
    // review-loop-speed.test.ts pins down as the intended behaviour. That is a
    // race this environment can produce under load (the phase's own `make
    // check` hit it), so the claim is: every reviewer has a discovery, and a
    // reviewer that was NOT re-dispatched has exactly one. A dropped discovery
    // still fails.
    const decisionAdded = records
      .filter((r) => r.kind === "event" && (r.event as { type: string }).type === "DECISION_ADDED")
      .map((r) => (r.event as unknown as { decision: { source: string } }).decision);
    const discovered = decisionAdded.filter((d) => d.source === "reviewer-discovered");
    const redispatched = new Set(
      records
        .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "REVIEW_TIMED_OUT")
        .map((r) => (r.event as unknown as { reviewer: string }).reviewer),
    );
    for (const reviewer of ["M", "A", "B"] as const) {
      const mine = discovered.filter((d) => d.id.includes(`-disc-${reviewer}-`));
      assert.ok(
        mine.length >= 1,
        `${reviewer} should have a reviewer-discovered decision; got ${JSON.stringify(discovered.map((d: { id: string }) => d.id))}`,
      );
      if (!redispatched.has(reviewer)) {
        assert.equal(
          mine.length,
          1,
          `${reviewer} was not re-dispatched, so exactly one reviewer-discovered decision; got ${JSON.stringify(mine.map((d: { id: string }) => d.id))}`,
        );
      }
    }
    assert.ok(
      discovered.length >= 3,
      `expected at least one reviewer-discovered decision per reviewer; got ${JSON.stringify(decisionAdded)}`,
    );

    // unreferenced hunks: NOTES.txt's hunk is not cited by any decision or
    // finding text, so it must appear in the logged sample.
    const sampling = records.filter((r) => r.kind === "sampling").map((r) => r.event as unknown as { unreferencedHunks: { file: string }[] });
    assert.ok(sampling.length > 0, "a sampling record should have been logged at freeze time");
    assert.ok(
      sampling.some((s) => s.unreferencedHunks.some((h) => h.file === "NOTES.txt")),
      `NOTES.txt's hunk should be in the unreferenced sample; got ${JSON.stringify(sampling)}`,
    );

    // reproduction run: M's finding carried a reproduction command (`true`,
    // exit 0) — the conductor must have run it and recorded "reproduced".
    const findingRaised = records
      .filter((r) => r.kind === "event" && (r.event as { type: string }).type === "FINDING_RAISED")
      .map((r) => (r.event as unknown as { finding: { reproduction?: { result: string } } }).finding);
    assert.ok(
      findingRaised.some((f) => f.reproduction?.result === "reproduced"),
      `expected a finding with reproduction.result === 'reproduced'; got ${JSON.stringify(findingRaised)}`,
    );
    assert.ok(!types.includes("LAUNCH_FAILED"), "no tool-set mismatch should occur");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 06c: the silent-review re-prompt names missing tools from the preflight", async () => {
  const originalPath = process.env.PATH;
  const dir = fs.mkdtempSync("/tmp/tt-06c-reprompt-");
  // A PATH without rg: the agent-tool line names it, including on the silent
  // reviewer's re-prompt (which goes through the shared #agentPrompt helper).
  process.env.PATH = "/usr/bin:/bin";
  const bLog = path.join(dir, `B-${randomUUID().slice(0, 8)}.log`);
  let setup: Awaited<ReturnType<typeof setupConductor>> | undefined;
  try {
    setup = await setupConductor({
      checks: ["true"],
      stubReviews: false,
      deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 10_000, reviewMs: 20_000 },
      extraReviewerEnv: (reviewer) => (reviewer === "B" ? { FAKE_PI_PROMPT_LOG: bLog } : undefined),
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-sh", command: "printf 'x\n' > sum.js" },
          { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
        ],
      }),
      reviewerScriptFor: (reviewer, state) => {
        const review = {
          kind: "call-submit" as const,
          tool: "submit_review" as const,
          args: {
            reviewer,
            phaseId: state.phase.phaseId,
            candidateSha: state.phase.candidate?.sha,
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
            ballots: [],
            findings: [],
          },
        };
        if (reviewer !== "B") {
          return { hello: defaultReviewerHello(), steps: [{ kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } }, { kind: "wait-for-prompt" }, review] };
        }
        return {
          hello: defaultReviewerHello(),
          steps: [
            { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
            { kind: "wait-for-prompt" },
            { kind: "wait-for-prompt" },
            review,
          ],
        };
      },
    });
    await setup.conductor.start();
    await waitFor(() => setup!.conductor.state.phase.phase === "DONE", 120_000, 50, setup!.runDir);
    const prompts = fs.readFileSync(bLog, "utf8");
    assert.match(prompts, /environment preflight did not find/, "the re-prompt carries the missing-tools line");
    assert.match(prompts, /\brg\b/, "rg is named on the re-prompt");
  } finally {
    process.env.PATH = originalPath;
    if (setup) {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06c: a reviewer whose turn settles without submit_review is re-prompted once and its second-turn submission counts", async () => {
  // First run: B's turn 2 settles silently, the conductor re-prompts once, and
  // B's submission in that second turn counts like any other review.
  {
    const setup = await setupConductor({
      checks: ["true"],
      stubReviews: false,
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-sh", command: "printf 'x\n' > sum.js" },
          { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
        ],
      }),
      reviewerScriptFor: (reviewer, state) => {
        const review = {
          kind: "call-submit" as const,
          tool: "submit_review" as const,
          args: {
            reviewer,
            phaseId: state.phase.phaseId,
            candidateSha: state.phase.candidate?.sha,
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
            ballots: [],
            findings: [],
          },
        };
        if (reviewer !== "B") {
          return {
            hello: defaultReviewerHello(),
            steps: [{ kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } }, { kind: "wait-for-prompt" }, review],
          };
        }
        return {
          hello: defaultReviewerHello(),
          steps: [
            { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
            // Turn 2 settles with no submit_review...
            { kind: "wait-for-prompt" },
            // ...the re-prompt's turn submits it.
            { kind: "wait-for-prompt" },
            review,
          ],
        };
      },
    });
    await setup.conductor.start();
    try {
      await waitFor(() => setup.conductor.state.phase.phase === "DONE", 120_000, 50, setup.runDir);
      const records = readEvents(setup.runDir);
      assert.ok(records.some((r) => r.kind === "review_reprompt" && (r.event as { reviewer?: string }).reviewer === "B"), "B was re-prompted once");
      const submitted = records.filter((r) => r.kind === "event" && (r.event as { type: string }).type === "REVIEW_SUBMITTED").map((r) => (r.event as unknown as { review: { reviewer: string } }).review.reviewer);
      assert.ok(submitted.includes("B"), "B's second-turn submission counts");
      assert.equal(setup.conductor.state.phase.reviews.B?.review?.reviewer, "B");
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
  }

  // Second run: B settles silently twice — only then does the seat time out,
  // exactly as before.
  {
    const setup = await setupConductor({
      checks: ["true"],
      stubReviews: false,
      deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 10_000, reviewMs: 20_000 },
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-sh", command: "printf 'x\n' > sum.js" },
          { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
        ],
      }),
      reviewerScriptFor: (reviewer, state) => {
        if (reviewer !== "B") {
          return {
            hello: defaultReviewerHello(),
            steps: [
              { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
              { kind: "wait-for-prompt" },
              {
                kind: "call-submit",
                tool: "submit_review",
                args: { reviewer, phaseId: state.phase.phaseId, candidateSha: state.phase.candidate?.sha, contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [], ballots: [], findings: [] },
              },
            ],
          };
        }
        return {
          hello: defaultReviewerHello(),
          steps: [
            { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
            // Turn 2 and the re-prompt both settle with no submit_review.
            { kind: "wait-for-prompt" },
            { kind: "wait-for-prompt" },
          ],
        };
      },
    });
    await setup.conductor.start();
    try {
      await waitFor(() => eventTypes(readEvents(setup.runDir)).includes("REVIEW_TIMED_OUT"), 120_000, 50, setup.runDir);
      const records = readEvents(setup.runDir);
      assert.ok(records.some((r) => r.kind === "review_reprompt" && (r.event as { reviewer?: string }).reviewer === "B"), "B was re-prompted once before timing out");
      const completion = records.find(
        (r) => r.kind === "completion" && (r.event as { reviewer?: string; reason?: string }).reviewer === "B" && (r.event as { reason?: string }).reason?.includes("re-prompt"),
      );
      assert.ok(completion, "the second silent settle is recorded as a timeout");
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
    }
  }
});
