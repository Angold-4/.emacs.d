// Plan 07f (R3/R8): `tt why` is the lead's one read of a run — the exact
// blocker list the conductor uses, each blocker with the command that clears
// it, plus the open exceptions, the recorded runner and any bound reached.
// It reads the log and works on a stopped run.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import type { Reviewer } from "../../src/core/types.ts";
import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  setupConductor,
  waitFor,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function runCli(args: string[]): Promise<{ stdout: string; code: number }> {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, TT_NOTIFY_COMMAND: ":" } });
    let stdout = "";
    child.stdout.on("data", (d) => (stdout += String(d)));
    child.stderr.on("data", (d) => (stdout += String(d)));
    child.on("close", (code) => resolve({ stdout, code: code ?? 0 }));
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

const ITEM = (id: string) => ({ id, title: `${id} proves it`, text: `${id} proves it`, arch: [], verify: ["review"] });

function disclosure(choice: string) {
  return {
    choice,
    whyItMatters: "The plan left this choice open and callers depend on the answer",
    alternatives: [{ option: "the other obvious behaviour", consequence: "would surprise callers" }],
    recommendation: { choice, reason: "keeps the contract's own promise" },
    classProposal: "delegated",
  };
}

function worker(): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: "mkdir -p src/core && printf 'export const x = 1;\\n' > src/core/x.ts" },
      { kind: "call-submit", tool: "submit_coverage", args: { items: [{ id: "R1", status: "done", where: ["src/core/x.ts:1"], tests: [] }], arch: [] } },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [disclosure("batch cancels per tick")], assumptions: [], deviations: [] } },
    ],
  };
}

function reviewers(id: string) {
  return (reviewer: Reviewer, state: { phase: { phaseId: string; candidate?: { sha: string }; contract: { contractVersion: unknown }; decisions: Array<{ id: string }> } }) => {
    const decisionId = state.phase.decisions[0]?.id;
    return {
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        { kind: "call-tool", tool: "read", args: { path: "src/core/x.ts" } },
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
            ballots: decisionId ? [{ decisionId, vote: "reject", rationale: "not this way", evidence: ["src/core/x.ts:1"] }] : [],
            items: [{ id, verdict: "met", evidence: "src/core/x.ts:1" }],
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

test("plan 07f (R3/R8): tt why prints the blockers with commands from a stopped run, and --json is one object", async () => {
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")] } as never,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: worker,
    reviewerScriptFor: reviewers("R1"),
  } as never);
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 20, setup.runDir);
    const decision = setup.conductor.state.phase.decisions.find((d) => d.class !== "detail")!;
    const request = setup.conductor.state.phase.ownerRequests.find((r) => r.status === "open")!;
    assert.ok(decision && request);
    await setup.conductor.stop();

    const text = await runCli(["why", setup.runDir]);
    assert.equal(text.code, 0, text.stdout);
    assert.match(text.stdout, /accepted: no/);
    assert.match(text.stdout, /\[decision\]/);
    assert.match(text.stdout, /\[owner-request\]/);
    assert.match(text.stdout, /tt input/, "each blocker ends with the command that clears it");
    assert.match(text.stdout, /"type":"ruling"/, "the decision blocker's command is a ruling");
    assert.match(text.stdout, /"type":"resolve"/, "the request blocker's command is a resolve");

    const json = await runCli(["why", setup.runDir, "--json"]);
    assert.equal(json.code, 0, json.stdout);
    const parsed = JSON.parse(json.stdout.trim()) as {
      runId: string;
      phase: string;
      accepted: boolean;
      blockers: Array<{ kind: string; detail: string; command: string }>;
      exceptions: string[];
    };
    assert.equal(parsed.runId, path.basename(setup.runDir));
    assert.equal(parsed.phase, "AWAITING_OWNER");
    assert.equal(parsed.accepted, false);
    assert.ok(parsed.blockers.some((b) => b.kind === "decision" && b.command.includes("ruling")));
    assert.ok(parsed.blockers.some((b) => b.kind === "owner-request" && b.command.includes("resolve")));
    assert.ok(Array.isArray(parsed.exceptions));
  } finally {
    await cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
