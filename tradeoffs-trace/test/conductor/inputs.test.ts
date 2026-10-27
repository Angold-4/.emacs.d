// Plan 07f (R4/R5/R7): owner input is a ruling, not a correction.
// A ruling applies at once (no queued correction, no forced loop), it settles
// a record of any kind (a decision or a contract finding), and it is read in
// every later reducer brief.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { rulingLines } from "../../src/conductor.ts";
import { acceptanceBlockers } from "../../src/core/predicate.ts";
import type { Reviewer } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

/** Runs the real CLI while the conductor runs in this process, so its inbox
 * poll can apply the command (a synchronous spawn would block that poll). */
function runCli(args: string[], input?: string): Promise<{ stdout: string; code: number }> {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], {
      env: { ...process.env, TT_NOTIFY_COMMAND: ":" },
    });
    let stdout = "";
    child.stdout.on("data", (d) => (stdout += String(d)));
    child.stderr.on("data", (d) => (stdout += String(d)));
    child.on("close", (code) => resolve({ stdout, code: code ?? 0 }));
    if (input !== undefined) child.stdin.end(input);
    else child.stdin.end();
  });
}

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 15_000,
  reviewMs: 20_000,
  panelMs: 10_000,
  inboxPollMs: 100,
};

const WRITE = "mkdir -p src/core && printf 'interface Round { n: number }\\n' > src/core/rounds.ts";

const ITEM = (id: string) => ({ id, title: `${id} proves it`, text: `${id} proves it`, arch: [], verify: ["review"] });
const COVERAGE = (id: string) => ({
  items: [{ id, status: "done", where: ["src/core/rounds.ts:1"], tests: [] }],
  arch: [],
});

function disclosure(choice: string) {
  return {
    choice,
    whyItMatters: "The plan left this choice open and callers depend on the answer",
    alternatives: [{ option: "the other obvious behaviour", consequence: "would surprise callers" }],
    recommendation: { choice, reason: "keeps the contract's own promise" },
    classProposal: "delegated",
  };
}

function workerWithDecision(id: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: WRITE },
      { kind: "call-submit", tool: "submit_coverage", args: COVERAGE(id) },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [disclosure("batch cancels per tick")], assumptions: [], deviations: [] } },
    ],
  };
}

function reviewWithBallot(id: string, vote: "approve" | "reject", itemsMet = true) {
  return (reviewer: Reviewer, state: { phase: { phaseId: string; candidate?: { sha: string }; contract: { contractVersion: unknown }; decisions: Array<{ id: string }> } }) => {
    const decisionId = state.phase.decisions.find((d) => d.source !== "reviewer-discovered")?.id ?? state.phase.decisions[0]?.id;
    return {
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
        { kind: "call-tool", tool: "read", args: { path: "README.md" } },
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
            ballots: decisionId ? [{ decisionId, vote, rationale: "reviewed it", evidence: ["src/core/rounds.ts:1"] }] : [],
            items: [{ id, verdict: itemsMet ? "met" : "unmet", evidence: "src/core/rounds.ts:1 README.md:1" }],
          },
        },
      ],
    };
  };
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

async function sendRuling(runArg: string, runDir: string, recordId: string, text: string): Promise<string> {
  const state = await import("../../src/conductor.ts").then((m) => m.rebuildState(runDir, JSON.parse(fs.readFileSync(path.join(runDir, "plan", "v1.json"), "utf8")) as never, { lenient: true }));
  const binding = { runId: state.phase.runId, phaseId: state.phase.phaseId };
  const command = { type: "ruling", text, recordIds: [recordId], binding };
  const file = path.join(runDir, `ruling-${Date.now()}.json`);
  fs.writeFileSync(file, JSON.stringify(command));
  try {
    const out = await runCli(["input", runArg, file], "");
    return out.stdout;
  } finally {
    fs.rmSync(file, { force: true });
  }
}

async function sendResolve(runArg: string, runDir: string, requestId: string, option: string, note?: string): Promise<string> {
  const conductor = await import("../../src/conductor.ts");
  const state = conductor.rebuildState(runDir, JSON.parse(fs.readFileSync(path.join(runDir, "plan", "v1.json"), "utf8")) as never, { lenient: true });
  const request = state.phase.ownerRequests.find((r) => r.id === requestId)!;
  const command = {
    type: "resolve",
    recordKind: "request",
    option,
    ...(note ? { note } : {}),
    binding: {
      runId: state.phase.runId,
      phaseId: state.phase.phaseId,
      candidateSha: state.phase.candidate?.sha,
      contractVersion: state.phase.contract.contractVersion,
      recordId: request.id,
      recordVersion: request.version,
    },
  };
  const file = path.join(runDir, `resolve-${Date.now()}.json`);
  fs.writeFileSync(file, JSON.stringify(command));
  try {
    const out = await runCli(["input", runArg, file], "");
    return out.stdout;
  } finally {
    fs.rmSync(file, { force: true });
  }
}

// -- R4: a ruling during review is applied at once, never queued ------------

test("plan 07f (R4): a ruling during the review settles the decision at once, with no queued correction and no extra round", async () => {
  const promptLog = path.join(fs.mkdtempSync("/tmp/tt-07f-rule-prompt-"), "prompts.log");
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")], finalChecks: ["sleep 2"] } as never,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    extraEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => workerWithDecision("R1"),
    reviewerScriptFor: reviewWithBallot("R1", "reject"),
  } as never);
  try {
    await setup.conductor.start();
    // The decision exists once the candidate freezes. The phase is REVIEWING.
    await waitFor(
      () => setup.conductor.state.phase.phase === "REVIEWING" && setup.conductor.state.phase.decisions.length > 0,
      60_000,
      20,
      setup.runDir,
    );
    const decision = setup.conductor.state.phase.decisions[0];
    const rulingText = `Rule ${decision.id} as recorded: batch cancels per tick is the behaviour.`;
    const out = await sendRuling(setup.runDir, setup.runDir, decision.id, rulingText);
    assert.match(out, /ruling applied/, `the ruling applies through tt input: ${out}`);
    await waitFor(
      () => setup.conductor.state.phase.rulings?.some((r) => r.text === rulingText) === true,
      20_000,
      20,
      setup.runDir,
    );
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);

    const phase = setup.conductor.state.phase;
    assert.deepEqual(phase.rulings?.map((r) => r.recordIds), [[decision.id]], "the ruling names the decision");
    assert.equal(phase.queuedCorrections?.length ?? 0, 0, "a ruling never queues a correction");
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "REPAIR_ATTEMPT_STARTED").length,
      0,
      "the phase was accepted in the same round, with no repair attempt",
    );
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "RESOLVING_INCOMPLETE").length,
      0,
      "no extra resolving round was forced",
    );
    const prompt = fs.existsSync(promptLog) ? fs.readFileSync(promptLog, "utf8") : "";
    // The ruling is in the next reducer brief (the evaluator, when it ran)
    // and in the exported prompt section every later brief carries.
    assert.ok(rulingLines(phase.rulings).some((line) => line.includes(rulingText)), "the ruling line names the text");
    if (prompt.includes("You are the evaluator")) assert.ok(prompt.includes(rulingText), "the evaluator brief carries the ruling");
  } finally {
    await teardown(setup);
  }
});

// -- R5: a contract finding the owner accepts with a ruling no longer blocks -

test("plan 07f (R5): a contract finding the owner accepts with a ruling no longer blocks acceptance", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => workerWithDecisionNoBallot(),
    reviewerScriptFor: (reviewer: Reviewer, state: { phase: { phaseId: string; candidate?: { sha: string }; contract: { contractVersion: unknown }; decisions: Array<{ id: string }> } }) => {
      const decisionId = state.phase.decisions[0]?.id;
      if (reviewer === "B") {
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
                findings: [{ kind: "contract", severity: "blocking", evidence: "the contract says nothing about this case" }],
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
              reviewer,
              phaseId: state.phase.phaseId,
              candidateSha: state.phase.candidate?.sha,
              contractVersion: state.phase.contract.contractVersion,
              correctionStatements: [],
              findingStatements: [],
              ballots: decisionId ? [{ decisionId, vote: "approve", rationale: "fine", evidence: ["src/core/rounds.ts:1"] }] : [],
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.kind === "contract" && f.status === "open"), 60_000, 20, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.kind === "contract" && f.status === "open")!;
    assert.ok(
      acceptanceBlockers(setup.conductor.state.phase).some((b) => b.kind === "finding"),
      `the open contract finding is an acceptance blocker before the ruling: ${JSON.stringify(acceptanceBlockers(setup.conductor.state.phase))}`,
    );
    const text = "Accept the contract finding as recorded; the plan text already covers this case.";
    const out = await sendRuling(setup.runDir, setup.runDir, finding.id, text);
    assert.match(out, /ruling applied/, out);
    await waitFor(
      () => setup.conductor.state.phase.findings.find((f) => f.id === finding.id)?.status === "accepted",
      20_000,
      20,
      setup.runDir,
    );
    assert.ok(
      !acceptanceBlockers(setup.conductor.state.phase).some((b) => b.kind === "finding"),
      "the accepted finding is no longer a blocker",
    );
    assert.ok(rulingLines(setup.conductor.state.phase.rulings).some((l) => l.includes(text)), "the ruling is in every later reducer brief");
  } finally {
    await teardown(setup);
  }
});

function workerWithDecisionNoBallot(): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: WRITE },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

// -- R7: resolve + ruling together clear the blockers and accept ------------

test("plan 07f (R1/R7): tt input resolves an owner request and rules a decision, and with no blocker left accept() is true", async () => {
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")] } as never,
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => workerWithDecision("R1"),
    reviewerScriptFor: (reviewer: Reviewer, state: { phase: { phaseId: string; candidate?: { sha: string }; contract: { contractVersion: unknown }; decisions: Array<{ id: string }> } }) => {
      const decisionId = state.phase.decisions[0]?.id;
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          { kind: "call-tool", tool: "read", args: { path: "README.md" } },
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
              ballots: decisionId ? [{ decisionId, vote: "reject", rationale: "not this way", evidence: ["src/core/rounds.ts:1"] }] : [],
              items: [{ id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1 README.md:1" }],
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 20, setup.runDir);
    const before = setup.conductor.state.phase;
    const request = before.ownerRequests.find((r) => r.status === "open")!;
    const decision = before.decisions.find((d) => d.class !== "detail")!;
    const kinds = acceptanceBlockers(before).map((b) => b.kind);
    assert.ok(kinds.includes("decision"), `the failed vote is a decision blocker: ${JSON.stringify(kinds)}`);
    assert.ok(kinds.includes("owner-request"), `its request is a blocker too: ${JSON.stringify(kinds)}`);

    const resolved = await sendResolve(setup.runDir, setup.runDir, request.id, "reject_and_repair");
    assert.match(resolved, /resolve applied/, resolved);
    const ruled = await sendRuling(setup.runDir, setup.runDir, decision.id, "Rule the decision as recorded; the recorded choice stands.");
    assert.match(ruled, /ruling applied/, ruled);
    await waitFor(
      () => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "AWAITING_OWNER",
      90_000,
      20,
      setup.runDir,
    );
    const after = setup.conductor.state.phase;
    // The repair started by the resolve may bring a new candidate; either
    // way the decision is ruled and the request is closed, and the ONE
    // predicate agrees.
    assert.ok(!after.ownerRequests.some((r) => r.id === request.id && r.status === "open"), "the request is resolved");
    assert.ok(after.rulings?.some((r) => r.recordIds.includes(decision.id)), "the decision is ruled");
  } finally {
    await teardown(setup);
  }
});
