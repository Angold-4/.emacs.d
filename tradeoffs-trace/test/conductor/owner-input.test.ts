// Plan 2d exit-gate tests: the owner intervenes only through the input box,
// and every intervention is transparent (design §7.4, §7.5, §9.3):
//   - input while a worker attempt runs is delivered as a Pi RPC steer, with
//     `deliver.intent` before sending and `deliver.done` on acknowledgement;
//   - a steer intent with no acknowledgement is shown `delivery uncertain`
//     and never resent;
//   - input while the phase is AWAITING_OWNER is a correction: it resolves
//     the open owner requests, grants 3 fresh repair rounds and starts a
//     repair attempt whose prompt contains the text verbatim;
//   - input after DONE is refused visibly;
//   - `tt stop` ends a run's conductor cleanly within 15 s and `tt resume`
//     continues it.

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
  FAKE_PI_PATH,
  makeRepo,
  readEvents,
  setupConductor,
  waitFor,
  type TestConductorSetup,
} from "./harness.ts";
import { Conductor, runPaths, type Deadlines, type RunPlanFile } from "../../src/conductor.ts";
import { readLog } from "../../src/effects/log.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { OwnerRequest, Reviewer, State } from "../../src/core/types.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

const FAST: Partial<Deadlines> = {
  inboxPollMs: 40,
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 5_000,
  workerAttemptMs: 20_000,
  freezeMs: 10_000,
  checkMs: 5_000,
  probeMs: 5_000,
  reviewMs: 10_000,
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
    },
  };
}

function inboxDir(runDir: string): string {
  return runPaths(runDir).inbox;
}

function writeCommand(runDir: string, id: string, command: unknown): string {
  const file = path.join(inboxDir(runDir), `${id}.json`);
  fs.writeFileSync(file, JSON.stringify(command));
  return file;
}

function eventsOfType(runDir: string, type: string): unknown[] {
  return readEvents(runDir)
    .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === type)
    .map((r) => r.event);
}

function openRequests(state: State): OwnerRequest[] {
  return state.phase.ownerRequests.filter((r) => r.status === "open");
}

/** Appends a `deliver-<id>` intent record with no completion, simulating a
 * conductor that crashed between logging the intent and Pi acknowledging. */
function appendSteerIntent(runDir: string, commandId: string, text: string, attemptId: string): void {
  const p = runPaths(runDir);
  const { records } = readLog(p.events);
  const seq = records.reduce((m, r) => Math.max(m, r.seq), 0) + 1;
  const record = {
    seq,
    ts: new Date().toISOString(),
    kind: "intent",
    actionId: `deliver-${commandId}`,
    event: { commandId, text, attemptId, agentId: "worker-1-crashed" },
  };
  fs.appendFileSync(p.events, `${JSON.stringify(record)}\n`);
}

/** Runs a phase whose checks always fail to the repair-budget gate, i.e.
 * AWAITING_OWNER with a plain `repair_budget_exhausted` owner request. */
async function setupAwaitingOwner(checks: string[]): Promise<TestConductorSetup> {
  const setup = await setupConductor({
    checks,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
  return setup;
}

/** Restarts a conductor on the same run directory (read/write recovery). */
async function restart(setup: TestConductorSetup): Promise<Conductor> {
  const conductor = new Conductor({
    runDir: setup.runDir,
    plan: setup.plan,
    piCommand: process.execPath,
    piArgsPrefix: [FAKE_PI_PATH],
    stubReviews: true,
    deadlines: { inboxPollMs: 40 },
  });
  await conductor.start();
  return conductor;
}

test("owner-input: input while a worker runs is delivered as a steer (deliver.intent before send, deliver.done on ack)", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-input-steer-");
  const steerLog = path.join(dir, "steers.log");
  const setup = await setupConductor({
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    extraWorkerEnv: { FAKE_PI_STEER_LOG: steerLog },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.agentPgids.some((a) => a.role === "worker"), 30_000);
    const runId = setup.conductor.state.phase.runId;
    const text = "STEER-NOW: hold the lock under 50us";
    writeCommand(setup.runDir, "cmd-steer-1", {
      type: "steer",
      text,
      binding: { runId, phaseId: "p1" },
    });

    await waitFor(
      () => (setup.conductor.state.phase.ownerInputs ?? []).some((i) => i.id === "cmd-steer-1" && i.state === "delivered"),
      15_000,
    );
    const input = (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.id === "cmd-steer-1")!;
    assert.equal(input.kind, "steer");
    assert.ok(input.attemptId, "the delivered record is bound to the worker attempt it reached");

    // deliver.intent before sending, deliver.done after acknowledgement.
    const records = readEvents(setup.runDir);
    const intent = records.find((r) => r.kind === "intent" && r.actionId === "deliver-cmd-steer-1");
    const done = records.find((r) => r.kind === "completion" && r.actionId === "deliver-cmd-steer-1");
    assert.ok(intent, "deliver.intent must be logged before the RPC steer");
    assert.ok(done, "deliver.done must be logged once Pi acknowledges");
    assert.equal((done!.event as { outcome: string }).outcome, "steer-acknowledged");

    // The text actually reached the worker.
    assert.match(fs.readFileSync(steerLog, "utf8"), /STEER-NOW: hold the lock under 50us/);
    assert.ok(!fs.existsSync(path.join(inboxDir(setup.runDir), "cmd-steer-1.json")), "the command file moves to applied/");

    // `tt state` carries the recorded owner input, so the status buffer can
    // show what actually happened.
    const stateJson = JSON.parse(
      execFileSync(process.execPath, [CLI_PATH, "state", setup.runDir, "--root", setup.runRoot], { encoding: "utf8" }),
    ) as { ownerInputs: Array<{ id: string; state: string }>; pendingOwnerInputs: unknown[] };
    assert.ok(stateJson.ownerInputs.some((i) => i.id === "cmd-steer-1" && i.state === "delivered"));
    assert.ok(Array.isArray(stateJson.pendingOwnerInputs));
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("owner-input: a steer intent with no acknowledgement is shown delivery uncertain and never resent (steer-uncertain)", async () => {
  const setup = await setupAwaitingOwner(["false"]);
  await setup.conductor.stop();
  try {
    const text = "MAYBE-DELIVERED: this may never have reached the worker";
    appendSteerIntent(setup.runDir, "cmd-uncertain", text, "worker-1-crashed");
    writeCommand(setup.runDir, "cmd-uncertain", {
      type: "steer",
      text,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });

    const restarted = await restart(setup);
    try {
      await waitFor(
        () => (restarted.state.phase.ownerInputs ?? []).some((i) => i.id === "cmd-uncertain" && i.state === "delivery-uncertain"),
        15_000,
      );
      const input = (restarted.state.phase.ownerInputs ?? []).find((i) => i.id === "cmd-uncertain")!;
      assert.equal(input.kind, "steer");
      assert.match(input.reason ?? "", /never resent/);

      // Exactly one intent, and never a completion: it is never resent.
      const records = readEvents(setup.runDir);
      const intents = records.filter((r) => r.kind === "intent" && r.actionId === "deliver-cmd-uncertain");
      const completions = records.filter((r) => r.kind === "completion" && r.actionId === "deliver-cmd-uncertain");
      assert.equal(intents.length, 1, "exactly one deliver.intent");
      assert.equal(completions.length, 0, "no acknowledgement may be invented");
      assert.ok(!fs.existsSync(path.join(inboxDir(setup.runDir), "cmd-uncertain.json")), "the file moves out of the inbox");
      assert.ok(fs.existsSync(path.join(runPaths(setup.runDir).inboxApplied, "cmd-uncertain.json")));
    } finally {
      await restarted.stop();
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("owner-input: input while AWAITING_OWNER is a correction that resolves the requests, grants 3 rounds and repairs with the text verbatim (correction-after-budget)", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-input-correction-");
  const promptLog = path.join(dir, "worker-prompts.log");
  const setup = await setupConductor({
    checks: ["false"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    deadlines: FAST,
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 60_000);
    const requests = openRequests(setup.conductor.state);
    assert.ok(requests.length > 0, "expected open owner requests at AWAITING_OWNER");
    const grantedBefore = setup.conductor.state.phase.repairRoundsGranted;
    const text = "CORRECTION: a lone cancellation must not wait for the next tick";
    writeCommand(setup.runDir, "cmd-correction-1", {
      type: "correction",
      text,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });

    await waitFor(() => setup.conductor.state.phase.phase !== "AWAITING_OWNER", 15_000);
    assert.equal(openRequests(setup.conductor.state).length, 0, "a correction resolves every open owner request");
    assert.equal(setup.conductor.state.phase.repairRoundsGranted, grantedBefore + 3, "a correction grants 3 fresh rounds");

    await waitFor(() => fs.existsSync(promptLog) && fs.readFileSync(promptLog, "utf8").includes(text), 20_000);
    assert.match(fs.readFileSync(promptLog, "utf8"), new RegExp(text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")));

    const recorded = (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.id === "cmd-correction-1");
    assert.ok(recorded, "the correction is recorded for the status view");
    assert.equal(recorded!.state, "correction-started");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("owner-input: input after DONE is refused with the reason recorded", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [submitReviewStep(reviewer, state)],
    }),
    deadlines: FAST,
  });
  await setup.conductor.start();
  await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000);
  await setup.conductor.stop();
  try {
    writeCommand(setup.runDir, "cmd-after-done", {
      type: "note",
      text: "too late",
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    const restarted = await restart(setup);
    try {
      await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "command_rejected"), 15_000);
      const rejected = readEvents(setup.runDir).find((r) => r.kind === "command_rejected")!;
      assert.match((rejected.event as { reason: string }).reason, /DONE/);
      assert.equal(restarted.state.phase.phase, "DONE");
      const file = path.join(runPaths(setup.runDir).inboxRejected, "cmd-after-done.json");
      assert.ok(fs.existsSync(file), "the refused command moves to inbox/rejected/");
      assert.ok(fs.existsSync(path.join(runPaths(setup.runDir).inboxRejected, "cmd-after-done.reason.txt")));
      assert.equal(eventsOfType(setup.runDir, "NOTE_ADDED").length, 0, "a refused input is never applied");
      const refused = (restarted.state.phase.ownerInputs ?? []).find((i) => i.id === "cmd-after-done");
      assert.ok(refused, "the refusal is recorded for the status view");
      assert.equal(refused!.state, "refused");
      assert.match(refused!.reason ?? "", /DONE/);
    } finally {
      await restarted.stop();
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function runCli(args: string[], env: NodeJS.ProcessEnv): Promise<{ code: number | null; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, ...env } });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ code, stdout, stderr }));
  });
}

test("owner-input: tt stop ends a run's conductor cleanly within 15s and tt resume continues it", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-stop-root");
  const scriptsDir = shortTmp("tt-stop-scripts");
  const plan: RunPlanFile = {
    title: "tt-stop",
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
  };
  const planPath = path.join(scriptsDir, "plan.json");
  fs.writeFileSync(planPath, JSON.stringify(plan));
  // A worker that stays in IMPLEMENTING (never submits, never exits on its
  // own) so the daemon is stable while `tt stop` is called.
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({ hello: { role: "worker", tools: ROLE_TOOLS.worker }, steps: [{ kind: "hang-until-abort" }] }),
  );
  fs.writeFileSync(
    path.join(scriptsDir, "reviewer.json"),
    JSON.stringify({ hello: { role: "reviewer", tools: ROLE_TOOLS.reviewer }, steps: [] }),
  );
  const testEnv: NodeJS.ProcessEnv = {
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    TT_TEST_STUB_REVIEWS: "1",
    FAKE_PI_SCRIPT: scriptsDir,
  };

  let runId = "";
  let daemonDir = "";
  try {
    const start = await runCli(["start", planPath, "--root", root], testEnv);
    assert.equal(start.code, 0, `tt start failed: ${start.stderr}`);
    runId = start.stdout.trim();
    daemonDir = path.join(root, runId);

    await waitFor(() => {
      const status = execFileSync(process.execPath, [CLI_PATH, "status", runId, "--root", root], { encoding: "utf8" });
      return /phase: p1 — IMPLEMENTING/.test(status);
    }, 20_000, 200);

    const t0 = Date.now();
    const stop = await runCli(["stop", runId, "--root", root], testEnv);
    const elapsed = Date.now() - t0;
    assert.equal(stop.code, 0, `tt stop failed: ${stop.stderr}`);
    assert.ok(elapsed < 15_000, `tt stop took ${elapsed}ms, expected < 15000ms`);
    assert.match(stop.stdout, /stopped/);

    // Agents terminated and the stop event logged.
    assert.throws(() => execFileSync("pgrep", ["-f", daemonDir], { encoding: "utf8" }), /.*/, "the daemon must be gone");
    assert.ok(
      readEvents(daemonDir).some((r) => r.kind === "stop"),
      "the stop event must be logged",
    );

    // The run can be resumed, and stops cleanly again.
    const resume = await runCli(["resume", runId, "--root", root], testEnv);
    assert.equal(resume.code, 0, `tt resume failed: ${resume.stderr}`);
    await waitFor(() => {
      try {
        return execFileSync("pgrep", ["-f", daemonDir], { encoding: "utf8" }).length > 0;
      } catch {
        return false;
      }
    }, 15_000);
    const stop2 = await runCli(["stop", runId, "--root", root], testEnv);
    assert.equal(stop2.code, 0, `second tt stop failed: ${stop2.stderr}`);
  } finally {
    try {
      if (daemonDir) execFileSync("pkill", ["-9", "-f", daemonDir]);
    } catch {
      // already gone
    }
    await new Promise((resolve) => setTimeout(resolve, 50));
    cleanupDir(root);
    cleanupDir(scriptsDir);
  }
});
