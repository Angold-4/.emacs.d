// Plan 01i exit gate (01_ref_design.md §8, D5): every text the owner sends
// through the input box is an owner directive — delivered at once to every
// live agent of the run, recorded, included verbatim in every later prompt
// (worker attempts and repairs, both reviewer turns), withdrawable, and
// survived across a conductor restart. The status view (`tt state`) carries
// the per-agent delivery. A program-wide directive reaches every running
// node now and every node started later.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  makeRepo,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";
import { contractVersionFor, runPaths, type Deadlines, type RunPlanFile } from "../../src/conductor.ts";
import { createProgram, foldProgram, schedulerTick } from "../../src/program.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import type { OwnerDirective, Reviewer, State } from "../../src/core/types.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));
const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));

const FAST: Partial<Deadlines> = {
  inboxPollMs: 40,
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 5_000,
  workerAttemptMs: 20_000,
  freezeMs: 10_000,
  checkMs: 5_000,
  probeMs: 5_000,
  reviewMs: 20_000,
};

function submitPhaseStep() {
  return { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } };
}

function submitReviewStep(reviewer: Reviewer, state: State) {
  return {
    kind: "call-submit",
    tool: "submit_review",
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
}

/** One input-box command for this run, bound to its live run/phase ids. */
function inputCommand(setup: TestConductorSetup, type: string, text: string, scope?: string) {
  return {
    type,
    text,
    ...(scope ? { scope } : {}),
    binding: { runId: setup.conductor.state.phase.runId, phaseId: setup.conductor.state.phase.phaseId },
  };
}

function writeCommand(runDir: string, id: string, command: unknown): string {
  const file = path.join(runPaths(runDir).inbox, `${id}.json`);
  fs.writeFileSync(file, JSON.stringify(command));
  return file;
}

function directives(setup: TestConductorSetup): OwnerDirective[] {
  return setup.conductor.state.phase.ownerDirectives ?? [];
}

function eventsOfType(runDir: string, type: string): unknown[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === type)
    .map((r) => r.event);
}

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function stateJson(runDir: string, root: string): State {
  return (
    JSON.parse(execFileSync(process.execPath, [CLI_PATH, "state", runDir, "--root", root], { encoding: "utf8" })) as {
      state: State;
    }
  ).state;
}

test("owner-directives: an input while the worker runs is steered to the worker, recorded as OD-1, and quoted in every later prompt", async () => {
  const dir = shortTmp("tt-dir-repair");
  const steerLog = path.join(dir, "worker-steers.log");
  const promptLog = path.join(dir, "worker-prompts.log");
  const reviewerPromptLogs = new Map<Reviewer, string>();
  for (const r of ["M", "A", "B"] as Reviewer[]) reviewerPromptLogs.set(r, path.join(dir, `reviewer-${r}-prompts.log`));
  const marker = path.join(dir, "round-two-passed");
  const text = "the 14 exchange-state-machine failures are pre-existing, not yours";
  const setup = await setupConductor({
    // Round 1's checks fail, so a repair attempt follows; the worker makes
    // them pass on round 2, which sends the candidate into review.
    checks: [`test -f ${marker}`],
    stubReviews: false,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps:
        attempt === 1
          ? // Stays up long enough for the owner to steer it.
            [{ kind: "sleep", ms: 4_000 }, submitPhaseStep()]
          : [{ kind: "call-sh", command: `touch ${marker}` }, submitPhaseStep()],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        submitReviewStep(reviewer, state),
      ],
    }),
    extraWorkerEnv: { FAKE_PI_STEER_LOG: steerLog, FAKE_PI_PROMPT_LOG: promptLog },
    extraReviewerEnv: (reviewer) => ({ FAKE_PI_PROMPT_LOG: reviewerPromptLogs.get(reviewer)! }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.agentPgids.some((a) => a.role === "worker"), 30_000);
    writeCommand(setup.runDir, "cmd-steer-1", inputCommand(setup, "steer", text));

    await waitFor(
      () => (directives(setup).find((d) => d.commandId === "cmd-steer-1")?.deliveries.worker ?? "") === "delivered",
      20_000,
    );
    const directive = directives(setup).find((d) => d.commandId === "cmd-steer-1")!;
    assert.equal(directive.id, "OD-1");
    assert.equal(directive.seq, 1);
    assert.equal(directive.status, "in-force");
    assert.equal(directive.scope, "phase");
    assert.deepEqual(directive.targets, ["worker"]);
    assert.equal(directive.text, text, "the text is recorded verbatim");

    // The steer actually reached the running worker.
    await waitFor(() => fs.existsSync(steerLog) && fs.readFileSync(steerLog, "utf8").includes(text), 20_000);

    // The NEXT worker attempt's prompt (the repair round) carries it verbatim.
    await waitFor(() => fs.existsSync(promptLog) && fs.readFileSync(promptLog, "utf8").includes(`OD-1: ${text}`), 90_000);
    assert.match(fs.readFileSync(promptLog, "utf8"), /Owner directives \(binding\):/);
    assert.equal(eventsOfType(setup.runDir, "DIRECTIVE_ADDED").length, 1, "exactly one directive event");

    // The status view's own payload carries the record and its delivery.
    const shown = stateJson(setup.runDir, setup.runRoot).phase.ownerDirectives?.[0];
    assert.equal(shown?.id, "OD-1");
    assert.equal(shown?.deliveries.worker, "delivered");

    // Both reviewer turns' prompts carry it too (the candidate reached review
    // only after that repair attempt).
    await waitFor(
      () =>
        (["M", "A", "B"] as Reviewer[]).every((r) => {
          const log = fs.existsSync(reviewerPromptLogs.get(r)!) ? fs.readFileSync(reviewerPromptLogs.get(r)!, "utf8") : "";
          return log.includes("Turn 1 of 2") && log.includes("Turn 2 of 2");
        }),
      90_000,
    );
    for (const r of ["M", "A", "B"] as Reviewer[]) {
      const prompts = fs.readFileSync(reviewerPromptLogs.get(r)!, "utf8").split("\n=====\n");
      const turn1 = prompts.find((p) => p.includes("Turn 1 of 2"))!;
      const turn2 = prompts.find((p) => p.includes("Turn 2 of 2"))!;
      assert.ok(turn1.includes(`OD-1: ${text}`), `${r}'s turn-1 prompt quotes OD-1 verbatim`);
      assert.ok(turn2.includes(`OD-1: ${text}`), `${r}'s turn-2 prompt quotes OD-1 verbatim`);
    }

    // `tt summary` lists the directives in force (the PR body is where a
    // reviewer of the PR learns why the candidate looks the way it does).
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000);
    const summary = execFileSync(process.execPath, [CLI_PATH, "summary", setup.runDir, "--root", setup.runRoot], {
      encoding: "utf8",
    });
    assert.match(summary, /### Owner directives \(binding\)/);
    assert.ok(summary.includes(`**OD-1**: ${text}`), "tt summary lists OD-1 verbatim");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("owner-directives: an input while the reviewers are mid-turn is steered to each live reviewer and shown per agent", async () => {
  const dir = shortTmp("tt-dir-reviewers");
  const steerLogs = new Map<Reviewer, string>();
  const promptLogs = new Map<Reviewer, string>();
  for (const r of ["M", "A", "B"] as Reviewer[]) {
    steerLogs.set(r, path.join(dir, `reviewer-${r}-steers.log`));
    promptLogs.set(r, path.join(dir, `reviewer-${r}-prompts.log`));
  }
  const text = "treat Stork's 501 ms as inside the contracted budget; the owner accepts the miss";
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "sleep", ms: 2_000 }, submitPhaseStep()],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        // Holds each reviewer mid-turn long enough for the owner to steer it.
        { kind: "sleep", ms: 10_000 },
        submitReviewStep(reviewer, state),
      ],
    }),
    extraReviewerEnv: (reviewer) => ({
      FAKE_PI_STEER_LOG: steerLogs.get(reviewer)!,
      FAKE_PI_PROMPT_LOG: promptLogs.get(reviewer)!,
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.agentPgids.filter((a) => a.role === "reviewer").length === 3, 90_000);
    writeCommand(setup.runDir, "cmd-note-rev", inputCommand(setup, "note", text));

    await waitFor(() => {
      const d = directives(setup).find((x) => x.commandId === "cmd-note-rev");
      return !!d && d.deliveries.M === "delivered" && d.deliveries.A === "delivered" && d.deliveries.B === "delivered";
    }, 30_000);
    const directive = directives(setup).find((d) => d.commandId === "cmd-note-rev")!;
    assert.deepEqual([...directive.targets].sort(), ["A", "B", "M"], "one target per live reviewer");
    // …and the status view's payload shows that per-agent delivery.
    const shown = stateJson(setup.runDir, setup.runRoot).phase.ownerDirectives?.find((d) => d.commandId === "cmd-note-rev");
    assert.deepEqual([...(shown?.targets ?? [])].sort(), ["A", "B", "M"]);
    assert.equal(shown?.deliveries.M, "delivered");
    assert.equal(shown?.deliveries.A, "delivered");
    assert.equal(shown?.deliveries.B, "delivered");
    for (const r of ["M", "A", "B"] as Reviewer[]) {
      assert.equal(directive.deliveries[r], "delivered", `${r} was steered`);
      const steered = fs.readFileSync(steerLogs.get(r)!, "utf8");
      assert.ok(steered.includes(`OD-${directive.seq}`), `${r}'s steer names the directive id`);
      assert.ok(steered.includes(text), `${r}'s steer carries the text verbatim`);
    }

    // Every later prompt quotes it: wait for each reviewer's turn 2 (which
    // is sent after the directive here) and check both turns' prompts.
    await waitFor(
      () =>
        (["M", "A", "B"] as Reviewer[]).every((r) => {
          const log = fs.existsSync(promptLogs.get(r)!) ? fs.readFileSync(promptLogs.get(r)!, "utf8") : "";
          return log.includes(`OD-${directive.seq}: ${text}`) && log.includes("Turn 2 of 2");
        }),
      90_000,
    );
    const prompts = fs.readFileSync(promptLogs.get("M")!, "utf8").split("\n=====\n");
    const turn1 = prompts.find((p) => p.includes("Turn 1 of 2"))!;
    const turn2 = prompts.find((p) => p.includes("Turn 2 of 2"))!;
    assert.match(turn1, new RegExp(`OD-${directive.seq}: ${text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}`));
    assert.match(turn2, /Owner directives \(binding\):/);
    assert.match(turn2, /cannot be faulted for doing so/);
    assert.match(turn2, /blocking contract finding/);

    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("owner-directives: a directive survives a restart, and a correction at AWAITING_OWNER is a directive too", async () => {
  const dir = shortTmp("tt-dir-restart");
  const promptLog = path.join(dir, "worker-prompts.log");
  const text = "hold the lock under 50us";
  const correction = "CORRECTION: a lone cancellation must not wait for the next tick";
  const setup = await setupConductor({
    checks: ["false"],
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: attempt === 1 ? [{ kind: "sleep", ms: 3_000 }, submitPhaseStep()] : [submitPhaseStep()],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.agentPgids.some((a) => a.role === "worker"), 30_000);
    writeCommand(setup.runDir, "cmd-keep", inputCommand(setup, "steer", text));
    await waitFor(() => directives(setup).some((d) => d.commandId === "cmd-keep"), 20_000);
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000);

    // `tt stop`, then `tt resume`: the detached conductor folds the log
    // back. The repair attempt the correction starts is launched by that
    // detached process, which has no per-attempt script map — one script
    // for every role, and the prompt log it writes is this run's.
    await setup.conductor.stop();
    const repairScript = path.join(setup.scriptsDir, "repair.json");
    fs.writeFileSync(repairScript, JSON.stringify({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }));
    const cliEnv: NodeJS.ProcessEnv = {
      ...process.env,
      TT_TEST_MODE: "1",
      TT_TEST_PI_COMMAND: process.execPath,
      TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
      TT_TEST_STUB_REVIEWS: "1",
      FAKE_PI_SCRIPT: repairScript,
      FAKE_PI_PROMPT_LOG: promptLog,
      TT_TEST_DEADLINES: JSON.stringify({ ...FAST, workerAttemptMs: 60_000 }),
    };
    execFileSync(process.execPath, [CLI_PATH, "resume", setup.runDir, "--root", setup.runRoot], { encoding: "utf8", env: cliEnv });
    await waitFor(
      () => stateJson(setup.runDir, setup.runRoot).phase.ownerDirectives?.some((d) => d.commandId === "cmd-keep"),
      90_000,
    );
    const folded = stateJson(setup.runDir, setup.runRoot).phase.ownerDirectives?.find((d) => d.commandId === "cmd-keep");
    assert.ok(folded, "the directive survives tt stop + tt resume");
    assert.equal(folded!.status, "in-force");
    assert.equal(folded!.text, text);

    const before = stateJson(setup.runDir, setup.runRoot).phase;
    assert.equal(before.phase, "AWAITING_OWNER", "the resumed run is parked on the owner");
    assert.ok(before.ownerRequests.some((r) => r.status === "open"), "AWAITING_OWNER has open owner requests");
    const grantedBefore = before.repairRoundsGranted;
    writeCommand(setup.runDir, "cmd-corr", {
      type: "correction",
      text: correction,
      binding: { runId: before.runId, phaseId: before.phaseId },
    });

    await waitFor(() => stateJson(setup.runDir, setup.runRoot).phase.phase !== "AWAITING_OWNER", 60_000);
    const after = stateJson(setup.runDir, setup.runRoot).phase;
    assert.equal(after.ownerRequests.filter((r) => r.status === "open").length, 0, "the correction resolves every request");
    assert.equal(after.repairRoundsGranted, grantedBefore + 3, "a correction grants 3 fresh rounds");
    const corrDirective = (after.ownerDirectives ?? []).find((d) => d.commandId === "cmd-corr");
    assert.ok(corrDirective, "the correction is also an owner directive");
    assert.equal(corrDirective!.id, "OD-2", "directives are numbered OD-1, OD-2, …");

    await waitFor(() => {
      const log = fs.existsSync(promptLog) ? fs.readFileSync(promptLog, "utf8") : "";
      return log.includes(correction) && log.includes(`OD-1: ${text}`) && log.includes(`OD-2: ${correction}`);
    }, 90_000);
    const finalLog = fs.readFileSync(promptLog, "utf8");
    assert.ok(finalLog.includes(`OD-1: ${text}`), "the resumed run still quotes OD-1");
    assert.ok(finalLog.includes(`OD-2: ${correction}`), "the correction is quoted as OD-2");
  } finally {
    try {
      execFileSync(process.execPath, [CLI_PATH, "stop", setup.runDir, "--root", setup.runRoot], {
        encoding: "utf8",
        stdio: ["ignore", "pipe", "ignore"],
      });
    } catch {
      // the in-process conductor was already stopped
    }
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

/** Waits for a `command_rejected` record whose reason matches RE. */
async function waitForRejection(runDir: string, re: RegExp): Promise<{ reason: string }> {
  await waitFor(
    () => readEvents(runDir).some((r) => r.kind === "command_rejected" && re.test((r.event as { reason?: string }).reason ?? "")),
    30_000,
  );
  return readEvents(runDir).find((r) => r.kind === "command_rejected" && re.test((r.event as { reason?: string }).reason ?? ""))!
    .event as { reason: string };
}

test("owner-directives: withdraw retracts exactly the named directive; a malformed or unknown one is refused, never inverted", async () => {
  const dir = shortTmp("tt-dir-withdraw");
  const steerLog = path.join(dir, "worker-steers.log");
  const promptLog = path.join(dir, "worker-prompts.log");
  const text = "fix the Stork live link in this phase; owner-directed exception";
  const second = "treat the Stork 501ms as inside the contract";
  const setup = await setupConductor({
    checks: ["false"],
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: attempt === 1 ? [{ kind: "sleep", ms: 8_000 }, submitPhaseStep()] : [submitPhaseStep()],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    extraWorkerEnv: { FAKE_PI_STEER_LOG: steerLog, FAKE_PI_PROMPT_LOG: promptLog },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.agentPgids.some((a) => a.role === "worker"), 30_000);
    writeCommand(setup.runDir, "cmd-d1", inputCommand(setup, "steer", text));
    await waitFor(() => directives(setup).some((d) => d.commandId === "cmd-d1"), 20_000);

    // An unknown id is refused with the reason, recorded for the status.
    writeCommand(setup.runDir, "cmd-bad", inputCommand(setup, "steer", "withdraw OD-99"));
    const unknown = await waitForRejection(setup.runDir, /no owner directive OD-99/);
    assert.equal(
      (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.id === "cmd-bad")?.state,
      "refused",
      `the refusal is recorded (${unknown.reason})`,
    );
    assert.equal(directives(setup).length, 1, "a refused withdraw adds no directive");

    // A withdrawal that names no id is refused too: it must never be recorded
    // as a *new* binding directive that leaves the intended one in force.
    writeCommand(setup.runDir, "cmd-mal", inputCommand(setup, "steer", "withdraw nonsense"));
    await waitForRejection(setup.runDir, /must name a directive id/);
    assert.equal(directives(setup).length, 1, "a malformed withdraw adds no directive");

    // A second ruling, retracted with the owner's natural phrasing.
    writeCommand(setup.runDir, "cmd-d2", inputCommand(setup, "steer", second));
    await waitFor(() => directives(setup).some((d) => d.commandId === "cmd-d2"), 20_000);
    writeCommand(setup.runDir, "cmd-w2", inputCommand(setup, "steer", "withdraw OD-2 because it is stale"));
    await waitFor(() => directives(setup).find((d) => d.id === "OD-2")?.status === "withdrawn", 20_000);
    assert.equal(directives(setup).length, 2, "trailing prose retracts OD-2 instead of adding OD-3");
    assert.equal(directives(setup).find((d) => d.id === "OD-1")?.status, "in-force", "OD-1 is untouched");

    // Withdraw OD-1 too, while the worker is still live: it is steered that it
    // no longer applies.
    writeCommand(setup.runDir, "cmd-w1", inputCommand(setup, "steer", "withdraw OD-1"));
    await waitFor(
      () => fs.existsSync(steerLog) && fs.readFileSync(steerLog, "utf8").includes("OD-1 is withdrawn; it no longer applies."),
      20_000,
    );
    const withdrawn = directives(setup).find((d) => d.id === "OD-1")!;
    assert.equal(withdrawn.status, "withdrawn", "the withdrawal is recorded before anything else is promised");
    assert.ok(withdrawn.withdrawnAt, "the withdrawal time is recorded");
    // Everything the phase sends from here on must omit them. (The attempt-1
    // prompt may have been built before the owner ruled at all — that one
    // legitimately quotes them.)
    const sizeAtWithdraw = fs.existsSync(promptLog) ? fs.statSync(promptLog).size : 0;

    await waitFor(() => fs.existsSync(promptLog) && fs.readFileSync(promptLog, "utf8").includes("REPAIR"), 90_000);
    const later = fs.readFileSync(promptLog, "utf8").slice(sizeAtWithdraw);
    assert.match(later, /REPAIR/, "a later attempt's prompt is in the sampled window");
    assert.ok(!later.includes("Owner directives (binding)"), "withdrawn directives are omitted from later prompts");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

// ---------------------------------------------------------------------------
// Plan 01i (D5): program-wide directives
// ---------------------------------------------------------------------------

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

interface ProgramHarness {
  runRoot: string;
  scriptsDir: string;
  programDir: string;
  tick: () => void;
  tickUntil: (check: () => boolean, ms: number, label: string) => Promise<void>;
  node: (id: string) => { status: string; runId?: string };
  waitForWorker: (runId: string) => Promise<void>;
  stateOf: (runId: string) => State;
  promptsOf: (runId: string) => string;
  steersOf: (runId: string) => string;
  programDirectives: () => Array<{ id: string; text: string; withdrawn?: boolean; origin?: string }>;
  nodeInbox: (runId: string) => string;
  cleanup: () => Promise<void>;
}

/** One program, its node runs launched as detached conductors (exactly what
 * the scheduler does), and the helpers to tick it and read its state. */
function startProgramHarness(opts: {
  entries: Array<{ id: string; after: string[] }>;
  maxParallel?: number;
  workerSleepMs?: number;
  checks?: string[];
}): ProgramHarness {
  const repo = makeRepo();
  const runRoot = shortTmp("tt-dir-prog-root");
  const scriptsDir = shortTmp("tt-dir-prog-scripts");
  const checks = opts.checks ?? ["true"];
  const phase = { id: "p1", goal: "add one file", acceptance: ["it works"], checks, boundaries: [], reserved: [] };
  const plan = (title: string): RunPlanFile => ({ title, repo: repo.dir, integrationBranch: "main", checks, phases: [phase] });
  const program: ProgramFile = {
    title: "owner-directive-program",
    maxParallel: opts.maxParallel ?? 1,
    entries: opts.entries.map((e) => ({ ...e, plan: plan(e.id) })),
  };
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      // Long enough for the owner's rulings to reach the node while it is
      // still running; every node then submits.
      steps: [
        { kind: "sleep", ms: opts.workerSleepMs ?? 15_000 },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  );
  fs.writeFileSync(
    path.join(scriptsDir, "reviewer.json"),
    JSON.stringify({
      hello: { role: "reviewer", tools: ROLE_TOOLS.reviewer },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_review",
          args: {
            reviewer: "$TT_REVIEWER",
            phaseId: "p1",
            candidateSha: "$TT_CANDIDATE_SHA",
            contractVersion: contractVersionFor(phase),
            correctionStatements: [],
            findingStatements: [],
            ballots: [],
            findings: [],
          },
        },
      ],
    }),
  );

  const programDir = createProgram(runRoot, program);
  const launch = (runDir: string): void => {
    const base = path.basename(runDir);
    const child = spawn(process.execPath, [CLI_PATH, "__run-conductor", runDir], {
      detached: true,
      stdio: "ignore",
      env: {
        ...process.env,
        TT_TEST_MODE: "1",
        TT_TEST_PI_COMMAND: process.execPath,
        TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
        TT_TEST_STUB_REVIEWS: "1",
        FAKE_PI_SCRIPT: scriptsDir,
        TT_TEST_DEADLINES: JSON.stringify({ ...FAST, workerAttemptMs: 60_000 }),
        FAKE_PI_STEER_LOG: path.join(scriptsDir, `${base}.steers.log`),
        FAKE_PI_PROMPT_LOG: path.join(scriptsDir, `${base}.prompts.log`),
      },
    });
    child.unref();
  };
  const tick = (): void => {
    schedulerTick(programDir, { runRoot, launch });
  };
  const tickUntil = async (check: () => boolean, ms: number, label: string): Promise<void> => {
    const start = Date.now();
    while (!check()) {
      if (Date.now() - start > ms) throw new Error(`tickUntil: timed out after ${ms}ms (${label})`);
      tick();
      await sleep(250);
    }
  };
  const node = (id: string) => foldProgram(programDir).state.nodes[id];
  const runDirOf = (runId: string) => path.join(runRoot, runId);
  return {
    runRoot,
    scriptsDir,
    programDir,
    tick,
    tickUntil,
    node,
    waitForWorker: async (runId: string) => {
      const stream = path.join(runDirOf(runId), "stream");
      await tickUntil(
        () => fs.existsSync(stream) && fs.readdirSync(stream).some((f) => f.startsWith("worker-")),
        30_000,
        `${runId}'s worker is up`,
      );
    },
    stateOf: (runId: string) => stateJson(runDirOf(runId), runRoot),
    promptsOf: (runId: string) => {
      const f = path.join(scriptsDir, `${runId}.prompts.log`);
      return fs.existsSync(f) ? fs.readFileSync(f, "utf8") : "";
    },
    steersOf: (runId: string) => {
      const f = path.join(scriptsDir, `${runId}.steers.log`);
      return fs.existsSync(f) ? fs.readFileSync(f, "utf8") : "";
    },
    programDirectives: () => foldProgram(programDir).state.directives ?? [],
    nodeInbox: (runId: string) => runPaths(runDirOf(runId)).inbox,
    cleanup: async () => {
      // Every node is a detached conductor; stop them hard before removing
      // the run root, or a still-writing process makes the cleanup fail first
      // and hides the test's real error.
      try {
        execFileSync("pkill", ["-9", "-f", runRoot]);
      } catch {
        // nothing running
      }
      try {
        execFileSync(process.execPath, [CLI_PATH, "program", "stop", programDir, "--root", runRoot], {
          encoding: "utf8",
          stdio: ["ignore", "pipe", "ignore"],
        });
      } catch {
        // already finished, or a node refused to stop; pkill above is enough
      }
      await sleep(300);
      try {
        cleanupDir(runRoot);
      } catch {
        // best effort
      }
      cleanupDir(scriptsDir);
      cleanupDir(repo.dir);
    },
  };
}

function writeNodeInput(h: ProgramHarness, runId: string, id: string, command: unknown): void {
  fs.writeFileSync(path.join(h.nodeInbox(runId), `${id}.json`), JSON.stringify(command));
}

test("owner-directives: a program-wide directive reaches a running node (steered) and the node started afterwards", async () => {
  const text = "PROGRAM-WIDE: no node may touch the vendor adapters after this ruling";
  const cuText = "C-U-WIDE: every node stops polling the vendor API";
  const h = startProgramHarness({ entries: [{ id: "n1", after: [] }, { id: "n2", after: ["n1"] }] });
  try {
    await h.tickUntil(() => h.node("n1").status === "running", 60_000, "n1 running");
    const n1 = h.node("n1").runId!;
    await h.waitForWorker(n1);

    // The owner rules program-wide: the directive is left in the program's
    // own inbox (what the Emacs program buffer writes), and the next
    // scheduler tick delivers it.
    fs.writeFileSync(path.join(h.programDir, "inbox", "cmd-prog-1.json"), JSON.stringify({ type: "directive", text, scope: "program" }));
    await h.tickUntil(() => h.steersOf(n1).includes(text), 60_000, "the running node's worker was steered");
    const n1Directive = h.stateOf(n1).phase.ownerDirectives?.find((d) => d.text === text);
    assert.ok(n1Directive, "the running node recorded the program-wide directive");
    assert.equal(n1Directive!.scope, "program");
    assert.equal(n1Directive!.deliveries.worker, "delivered", "the running node's worker was steered");

    // A `C-u` input in a *run's* own box: the node applies it and forwards it
    // to the program (D5), so the whole program carries it too.
    const n1Phase = h.stateOf(n1).phase;
    writeNodeInput(h, n1, "cmd-cu-1", {
      type: "steer",
      text: cuText,
      scope: "program",
      binding: { runId: n1Phase.runId, phaseId: n1Phase.phaseId },
    });
    await h.tickUntil(() => h.programDirectives().some((d) => d.text === cuText), 60_000, "the run's C-u input was forwarded to the program");

    // Node 1 finishes; node 2 starts WITH the directives (its plan carries
    // them), so its very first prompt quotes them.
    await h.tickUntil(() => h.node("n2").runId !== undefined, 120_000, "n2 started");
    const n2 = h.node("n2").runId!;
    await h.tickUntil(() => h.promptsOf(n2).includes(text), 90_000, "n2's first prompt quotes the directive");
    const seeded = h.stateOf(n2).phase.ownerDirectives?.find((d) => d.text === text);
    assert.ok(seeded, "the node started later carries the directive");
    assert.equal(seeded!.seeded, true, "…seeded from its plan, not pushed to its inbox");
    assert.equal(seeded!.id, "ODP-1", "a program-wide ruling keeps the program's ODP-n id");

    const programState = JSON.parse(
      execFileSync(process.execPath, [CLI_PATH, "program", "state", h.programDir, "--root", h.runRoot], { encoding: "utf8" }),
    ) as { state: { directives?: Array<{ id: string; text: string }> }; lines: string[] };
    assert.equal(programState.state.directives?.[0].text, text, "the program records the directive as a logged event");
    assert.ok(programState.lines.some((l) => l.includes(text)), "program status lists it");
  } finally {
    await h.cleanup();
  }
});

test("owner-directives: a program ruling keeps its ODP-n beside a node's own OD-n, and withdrawing it program-wide retires it on the node that issued it", async () => {
  const localText = "LOCAL-RULING: this phase keeps its own exception about the lock";
  const progText = "PROGRAM-RULING: no node may touch the vendor adapters";
  // Checks fail so a repair attempt follows, whose prompt must have dropped
  // the withdrawn program ruling but kept the phase's own.
  const h = startProgramHarness({ entries: [{ id: "n1", after: [] }], workerSleepMs: 14_000, checks: ["false"] });
  try {
    await h.tickUntil(() => h.node("n1").status === "running", 60_000, "n1 running");
    const n1 = h.node("n1").runId!;
    await h.waitForWorker(n1);
    const phase = h.stateOf(n1).phase;
    const binding = { runId: phase.runId, phaseId: phase.phaseId };

    // The phase's own ruling first, so the program ruling has to coexist with
    // a local OD-1 (the collision that used to force a renumber).
    writeNodeInput(h, n1, "cmd-local", { type: "note", text: localText, binding });
    await waitFor(() => h.stateOf(n1).phase.ownerDirectives?.some((d) => d.text === localText) ?? false, 60_000);
    // Then a `C-u` program ruling issued FROM this node (origin = n1).
    writeNodeInput(h, n1, "cmd-cu", { type: "steer", text: progText, scope: "program", binding });
    await h.tickUntil(() => h.programDirectives().some((d) => d.text === progText), 60_000, "the program recorded the C-u ruling");

    const beforeWithdraw = h.stateOf(n1).phase.ownerDirectives ?? [];
    assert.equal(beforeWithdraw.find((d) => d.text === localText)?.id, "OD-1", "the phase's own ruling is OD-1");
    assert.equal(beforeWithdraw.find((d) => d.text === progText)?.id, "ODP-1", "the program ruling keeps ODP-1, never renumbered");
    assert.equal(beforeWithdraw.find((d) => d.text === progText)?.scope, "program");

    // The program retracts it. The node that ISSUED it is still told (the
    // scheduler used to skip the origin), so its own copy is withdrawn too,
    // while its unrelated local OD-1 stays in force.
    const out = execFileSync(process.execPath, [CLI_PATH, "program", "withdraw", h.programDir, "ODP-1", "--root", h.runRoot], {
      encoding: "utf8",
    });
    assert.match(out, /withdrew ODP-1/);
    await waitFor(() => h.stateOf(n1).phase.ownerDirectives?.find((d) => d.text === progText)?.status === "withdrawn", 60_000);
    const after = h.stateOf(n1).phase.ownerDirectives ?? [];
    assert.equal(after.find((d) => d.text === localText)?.status, "in-force", "the unrelated local OD-1 is untouched");
    assert.equal(h.programDirectives().find((d) => d.text === progText)?.withdrawn, true, "the program record is withdrawn");

    // A later prompt quotes OD-1 and no longer quotes ODP-1.
    const atWithdraw = h.promptsOf(n1).length;
    await h.tickUntil(() => h.promptsOf(n1).slice(atWithdraw).includes("REPAIR"), 90_000, "a repair attempt followed");
    const later = h.promptsOf(n1).slice(atWithdraw);
    assert.ok(later.includes(`OD-1: ${localText}`), "the phase's own ruling is still quoted");
    assert.ok(!later.includes("ODP-1"), "the withdrawn program ruling is omitted");
  } finally {
    await h.cleanup();
  }
});

test("owner-directives: withdrawing a program ruling from a run's own box retires it on every node", async () => {
  const text = "C-U-WIDE: every node stops polling the vendor API";
  const h = startProgramHarness({
    entries: [{ id: "n1", after: [] }, { id: "n2", after: [] }],
    maxParallel: 2,
    workerSleepMs: 18_000,
  });
  try {
    await h.tickUntil(() => h.node("n1").status === "running" && h.node("n2").status === "running", 60_000, "both nodes running");
    const n1 = h.node("n1").runId!;
    const n2 = h.node("n2").runId!;
    await h.waitForWorker(n1);
    await h.waitForWorker(n2);

    const n1Phase = h.stateOf(n1).phase;
    writeNodeInput(h, n1, "cmd-cu", {
      type: "steer",
      text,
      scope: "program",
      binding: { runId: n1Phase.runId, phaseId: n1Phase.phaseId },
    });
    await h.tickUntil(() => h.programDirectives().some((d) => d.text === text), 60_000, "the C-u ruling reached the program");
    await waitFor(() => h.stateOf(n2).phase.ownerDirectives?.some((d) => d.text === text && d.status === "in-force") ?? false, 60_000);
    assert.equal(h.stateOf(n2).phase.ownerDirectives?.find((d) => d.text === text)?.id, "ODP-1");

    // The owner retracts it from n1's own input box, without `C-u`: a
    // program-wide ruling must be retracted program-wide from wherever it is
    // withdrawn.
    writeNodeInput(h, n1, "cmd-w", {
      type: "steer",
      text: "withdraw ODP-1",
      binding: { runId: n1Phase.runId, phaseId: n1Phase.phaseId },
    });
    await h.tickUntil(() => h.programDirectives().find((d) => d.text === text)?.withdrawn === true, 60_000, "the program recorded the withdrawal");
    await waitFor(() => h.stateOf(n2).phase.ownerDirectives?.find((d) => d.text === text)?.status === "withdrawn", 60_000);
    assert.equal(
      h.stateOf(n1).phase.ownerDirectives?.find((d) => d.text === text)?.status,
      "withdrawn",
      "the issuing node withdrew its own copy",
    );
  } finally {
    await h.cleanup();
  }
});
