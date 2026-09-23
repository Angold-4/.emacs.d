// Work packet 2a, design §4.3: M and A approve a delegated decision; B
// objects on contract (a ballot with `contractObjection: true`) — this
// opens a linked, blocking `contract` finding and suspends the vote (core
// already implements this; see reduce.ts's BALLOT_CAST case). The phase
// cannot accept until a new candidate exists AND B confirms, in its review
// of that new candidate, that the finding no longer holds
// (FINDING_CONFIRMED_REPAIRED). The owner-action branch (an owner
// resolving it directly) is work packet 2b's scope — this test only
// asserts the repair branch: RESOLVING_INCOMPLETE consumes a repair round,
// a second attempt produces a new candidate, and B's confirm on that
// candidate closes the finding and lets the vote pass.

import assert from "node:assert/strict";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";
import type { Reviewer } from "../../src/core/types.ts";

test("contract-objection: B's contract objection opens a finding and suspends the vote; a repaired candidate + B's confirm closes it", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    // Default deadlines (freezeMs 2min etc.) — this machine's own load
    // under concurrent test runs made a tighter override (this test's
    // original 20s freezeMs) fire spuriously, forcing an unnecessary extra
    // repair round.
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        // Every attempt changes the tree, so the repair produces a genuinely
        // new candidate (B may only confirm a repair on a different sha).
        { kind: "call-sh", command: `printf 'attempt ${attempt}\\n' > attempt.txt` },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: {
            // Only attempt 1 discloses the decision B will object to — the
            // repair attempt must not re-disclose a duplicate.
            decisions:
              attempt === 1
                ? [
                    {
                      choice: "Treat empty input as an error rather than returning a default",
                      whyItMatters: "Silently defaulting could hide caller bugs",
                      alternatives: [{ option: "return a default value", consequence: "caller bugs go unnoticed" }],
                      recommendation: { choice: "raise an error on empty input", reason: "fails loudly instead of silently" },
                      classProposal: "delegated",
                    },
                  ]
                : [],
            assumptions: [],
            deviations: [],
          },
        },
      ],
    }),
    reviewerScriptFor: (reviewer, state) => {
      const workerDecision = () => state.phase.decisions.find((d) => d.source === "worker")!;
      const contractFinding = () => state.phase.findings.find((f) => f.kind === "contract" && f.raisedBy === "B");
      if (reviewer === "B") {
        // B's script spans BOTH attempts' reviews (its dispatch is
        // redispatched fresh each time, but the script content is decided
        // once, lazily, at each dispatch — see reviewerScriptFor's own
        // per-dispatch call): on the first review of a candidate with no
        // open contract finding of its own yet, object; once one exists
        // and is still open, confirm it repaired now that a newer
        // candidate exists.
        const existing = contractFinding();
        if (existing && existing.status === "open" && existing.boundCandidateSha !== state.phase.candidate?.sha) {
          return {
            hello: defaultReviewerHello(),
            steps: [
              { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
              { kind: "wait-for-prompt" },
              {
                kind: "call-submit",
                tool: "submit_review",
                args: {
                  reviewer: "B",
                  phaseId: state.phase.phaseId,
                  candidateSha: state.phase.candidate?.sha,
                  contractVersion: state.phase.contract.contractVersion,
                  correctionStatements: [],
                  findingStatements: [{ findingId: existing.id, status: "confirm" }],
                  ballots: [
                    {
                      decisionId: workerDecision().id,
                      vote: "approve",
                      rationale: "the new candidate addresses my contract concern",
                      evidence: ["reviewed the repaired candidate"],
                    },
                  ],
                  findings: [],
                },
              },
            ],
          };
        }
        return {
          hello: defaultReviewerHello(),
          steps: [
            { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
            { kind: "wait-for-prompt" },
            {
              kind: "call-submit",
              tool: "submit_review",
              args: {
                reviewer: "B",
                phaseId: state.phase.phaseId,
                candidateSha: state.phase.candidate?.sha,
                contractVersion: state.phase.contract.contractVersion,
                correctionStatements: [],
                findingStatements: [],
                ballots: [
                  {
                    decisionId: workerDecision().id,
                    vote: "reject",
                    rationale: "the contract never said what to do on empty input — this needs an amendment first",
                    evidence: ["contract §acceptance has no clause for empty input"],
                    contractObjection: true,
                  },
                ],
                findings: [],
              },
            },
          ],
        };
      }
      // M and A always approve.
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
              ballots: [
                {
                  decisionId: workerDecision().id,
                  vote: "approve",
                  rationale: "raising on empty input is reasonable",
                  evidence: ["reviewed the diff"],
                },
              ],
              findings: [],
            },
          },
        ],
      };
    },
  });

  try {
    await setup.conductor.start();
    // First: the objection must open a finding and suspend the vote,
    // consuming a repair round (RESOLVING_INCOMPLETE) rather than
    // accepting.
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.kind === "contract"), 60_000, 50, setup.runDir);
    const openFinding = setup.conductor.state.phase.findings.find((f) => f.kind === "contract")!;
    assert.equal(openFinding.raisedBy, "B" as Reviewer);
    assert.equal(openFinding.severity, "blocking");
    assert.equal(openFinding.status, "open");

    await waitFor(
      () => ["DONE", "BLOCKED", "AWAITING_OWNER"].includes(setup.conductor.state.phase.phase),
      120_000,
      50,
      setup.runDir,
    );
    assert.equal(setup.conductor.state.phase.phase, "DONE", `expected DONE after the repair; got ${JSON.stringify(setup.conductor.state.phase)}`);

    const records = readEvents(setup.runDir);
    const types = records.filter((r) => r.kind === "event").map((r) => (r.event as { type: string }).type);
    assert.ok(types.includes("RESOLVING_INCOMPLETE"), "the objection should have forced a repair round, not immediate acceptance");
    assert.ok(types.includes("REPAIR_ATTEMPT_STARTED"), "a second attempt should have been started");
    assert.ok(types.includes("FINDING_CONFIRMED_REPAIRED"), "B's confirm on the new candidate should have closed the finding");

    const finalFinding = setup.conductor.state.phase.findings.find((f) => f.kind === "contract")!;
    assert.equal(finalFinding.status, "repaired");
    assert.notEqual(finalFinding.repairedByCandidateSha, finalFinding.boundCandidateSha);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
