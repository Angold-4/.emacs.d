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

/** Append one event to a stopped run's log, after the conductor has released
 * it. `tt why` must read this exactly like the conductor's own records. */
function appendEvent(runDir: string, event: Record<string, unknown>): void {
  const eventsPath = path.join(runDir, "events.jsonl");
  const lines = fs.readFileSync(eventsPath, "utf8").split("\n").filter((line) => line.length > 0);
  const last = lines.length > 0 ? (JSON.parse(lines[lines.length - 1]) as { seq?: number }) : { seq: 0 };
  fs.appendFileSync(
    eventsPath,
    `${JSON.stringify({ seq: (last.seq ?? 0) + 1, ts: new Date().toISOString(), kind: "event", event })}\n`,
  );
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

test("tt why prints the blockers with commands on a stopped run", async () => {
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

    // Make it a stopped run with the two owner gates as its blockers: the
    // decision and the request. The item R1 the single seat marked `met` is
    // NOT a blocker: its `src/core/x.ts:1` anchor lives in the candidate's own
    // tree, which this CLI process's cwd cannot see, so `tt why` must apply
    // the same review-lint anchor rule the conductor installs instead of
    // reading the anchor caller-relative. Also record a runner and a bound, so
    // the read must show both, not just the blocker list.
    const candidate = setup.conductor.state.phase.candidate!.sha as string;
    appendEvent(setup.runDir, {
      type: "BOUND_REACHED",
      kind: "check",
      scope: `runnable:${candidate}`,
      limitMs: 20_000,
      elapsedMs: 20_000,
    });
    const revision = "f".repeat(40);
    const metaPath = path.join(setup.runDir, "meta.json");
    const meta = JSON.parse(fs.readFileSync(metaPath, "utf8")) as Record<string, unknown>;
    fs.writeFileSync(metaPath, `${JSON.stringify({ ...meta, runnerRevision: revision }, null, 2)}\n`);

    const text = await runCli(["why", setup.runDir]);
    assert.equal(text.code, 0, text.stdout);
    assert.match(text.stdout, /accepted: no/);
    assert.match(text.stdout, /\[decision\]/);
    assert.match(text.stdout, /\[owner-request\]/);
    assert.doesNotMatch(text.stdout, /\[item\]/, "the seat's candidate anchor resolves, so R1 does not block");
    assert.match(text.stdout, /tt input/, "each blocker ends with the command that clears it");
    assert.match(text.stdout, /"type":"ruling"/, "the decision blocker's command is a ruling");
    assert.match(text.stdout, /"type":"resolve"/, "the request blocker's command is a resolve");
    assert.match(text.stdout, new RegExp(`runner revision: ${revision}`), "the text names the recorded runner");
    assert.match(text.stdout, /Bounds reached:/, "the text lists the bound reached");
    assert.match(text.stdout, /check \(runnable:/, "the bound's kind and scope are shown");
    assert.match(text.stdout, /20000ms of 20000ms/, "the bound's elapsed and limit are shown");

    const json = await runCli(["why", setup.runDir, "--json"]);
    assert.equal(json.code, 0, json.stdout);
    const parsed = JSON.parse(json.stdout.trim()) as {
      runId: string;
      phase: string;
      accepted: boolean;
      blockers: Array<{ kind: string; detail: string; command: string }>;
      exceptions: string[];
      runnerRevision?: string;
      bounds: Array<{ kind: string; scope: string; limitMs: number; elapsedMs: number }>;
    };
    assert.equal(parsed.runId, path.basename(setup.runDir));
    assert.equal(parsed.phase, "AWAITING_OWNER");
    assert.equal(parsed.accepted, false);
    assert.equal(parsed.blockers.length, 2, `exactly two blockers, got ${JSON.stringify(parsed.blockers, null, 2)}`);
    assert.ok(parsed.blockers.every((b) => b.detail.length > 0 && b.command.length > 0), "every blocker carries detail and command");
    assert.ok(parsed.blockers.some((b) => b.kind === "decision" && b.command.includes("ruling")));
    assert.ok(parsed.blockers.some((b) => b.kind === "owner-request" && b.command.includes("resolve")));
    assert.ok(!parsed.blockers.some((b) => b.kind === "item"), "the seat's candidate anchor resolves, so R1 does not block");
    assert.equal(parsed.runnerRevision, revision, "the JSON reports the recorded runner revision");
    assert.deepEqual(parsed.bounds, [{ kind: "check", scope: `runnable:${candidate}`, limitMs: 20_000, elapsedMs: 20_000 }]);
    assert.ok(Array.isArray(parsed.exceptions));
  } finally {
    await cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
