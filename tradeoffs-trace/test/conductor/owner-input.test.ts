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
import { Conductor, rebuildState, runPaths, type Deadlines, type RunPlanFile } from "../../src/conductor.ts";
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

test("plan 06d: a correction written as an inbox file during REVIEWING and a steer written during CHECKING are queued and reach the next worker attempt's prompt", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-06d-queue-");
  const promptLog = path.join(dir, "worker-prompts.log");
  const correctionText = "CORRECTION-QUEUED-06D: the retry loop must cap at 64";
  const steerText = "STEER-QUEUED-06D: keep the lock hold under 50us";
  // Round 1 and round 2 each raise a blocking finding, so round 3 is a real
  // worker attempt. The correction is written during round 1's REVIEWING and
  // the steer during round 2's CHECKING; both are queued, and round 3's prompt
  // must carry them.
  const setup = await setupConductor({
    checks: ["true"],
    // The real two-turn review protocol, like the other repair-driving tests:
    // a blocking finding raised in round 1 and round 2 forces a round 3 worker
    // attempt, which is where the queued correction and steer must arrive.
    stubReviews: false,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [submitPhaseStep()] }),
    reviewerScriptFor: (reviewer, state) => {
      const round = state.phase.round ?? 1;
      const open = state.phase.findings.filter((f) => f.status === "open");
      const findings =
        round <= 2 && reviewer === "M"
          ? [{ kind: "defect", severity: "blocking", evidence: "README.md:1 the thing is not done" }]
          : [];
      const findingStatements = round >= 3 ? open.map((f) => ({ findingId: f.id, status: "confirm" })) : [];
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
              findingStatements,
              findings,
            },
          },
        ],
      };
    },
    // Generous stage limits: the gate runs this file beside five other
    // conductor files, and the two-turn review needs both turns per round.
    deadlines: { ...FAST, workerAttemptMs: 60_000, reviewMs: 60_000, freezeMs: 30_000, checkMs: 30_000, probeMs: 30_000 },
  });
  await setup.conductor.start();
  try {
    await waitFor(() => setup.conductor.state.phase.phase === "REVIEWING", 60_000);
    writeCommand(setup.runDir, "cmd-06d-correction", {
      type: "correction",
      text: correctionText,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    await waitFor(
      () => (setup.conductor.state.phase.ownerInputs ?? []).some((i) => i.id === "cmd-06d-correction" && i.state === "queued"),
      30_000,
      50,
      setup.runDir,
    );

    // The repair attempt after round 1 carries the queued correction.
    await waitFor(() => setup.conductor.state.phase.phase === "CHECKING" && (setup.conductor.state.phase.round ?? 0) >= 2, 90_000, 50, setup.runDir);
    writeCommand(setup.runDir, "cmd-06d-steer", {
      type: "steer",
      text: steerText,
      binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" },
    });
    await waitFor(
      () => (setup.conductor.state.phase.ownerInputs ?? []).some((i) => i.id === "cmd-06d-steer" && i.state === "queued"),
      30_000,
    );

    // Both are shown queued in views/status.txt (Emacs reads this, not state).
    // The view is refreshed on its own beat, so wait for the file to catch up.
    await waitFor(
      () => {
        const file = runPaths(setup.runDir).status;
        const status = fs.existsSync(file) ? fs.readFileSync(file, "utf8") : "";
        return status.includes(correctionText) && status.includes(steerText) && (status.match(/ — queued /g) ?? []).length >= 2;
      },
      30_000,
      100,
      setup.runDir,
    );

    // The next worker attempt (round 3) carries both texts verbatim.
    await waitFor(() => {
      if (!fs.existsSync(promptLog)) return false;
      const prompts = fs.readFileSync(promptLog, "utf8");
      return prompts.includes(correctionText) && prompts.includes(steerText);
    }, 90_000);
    assert.ok(
      (setup.conductor.state.phase.ownerInputs ?? []).every((i) => i.state !== "refused"),
      "a queued correction or steer is never refused",
    );
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("plan 06d: corrections bound with the run directory id and with the readable id are accepted", async () => {
  const setup = await setupAwaitingOwner(["false"]);
  try {
    const runId = setup.conductor.state.phase.runId;
    const dirId = path.basename(setup.runDir);
    const readableId = "prog06d-01";
    // A program node's readable id, recorded where the scheduler records it.
    fs.writeFileSync(
      path.join(setup.runDir, "program.json"),
      JSON.stringify({ programId: "prog06d", node: "a", readableId }),
    );

    writeCommand(setup.runDir, "cmd-aaa-dir", {
      type: "correction",
      text: "DIR-ID-CORRECTION: applied now",
      binding: { runId: dirId, phaseId: "p1" },
    });
    writeCommand(setup.runDir, "cmd-bbb-readable", {
      type: "correction",
      text: "READABLE-ID-CORRECTION: accepted",
      binding: { runId: readableId, phaseId: "p1" },
    });
    writeCommand(setup.runDir, "cmd-ccc-unknown", {
      type: "correction",
      text: "UNKNOWN-ID-CORRECTION: refused",
      binding: { runId: "no-such-run", phaseId: "p1" },
    });

    await waitFor(
      () => (setup.conductor.state.phase.ownerInputs ?? []).some((i) => i.id === "cmd-aaa-dir" && i.state === "correction-started"),
      30_000,
    );
    await waitFor(
      () => (setup.conductor.state.phase.ownerInputs ?? []).some((i) => i.id === "cmd-bbb-readable" && i.state === "queued"),
      30_000,
    );
    await waitFor(
      () => readEvents(setup.runDir).some((r) => r.kind === "command_rejected" && (r.event as { commandId?: string }).commandId === "cmd-ccc-unknown"),
      30_000,
    );

    assert.equal(
      (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.id === "cmd-aaa-dir")!.state,
      "correction-started",
      "the run directory id bound the correction and it applied",
    );
    assert.equal(
      (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.id === "cmd-bbb-readable")!.state,
      "queued",
      "the readable id bound the correction and it was accepted (queued)",
    );
    const refused = (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.id === "cmd-ccc-unknown")!;
    assert.equal(refused.state, "refused");
    // The reason lists all three ids that would have bound.
    assert.match(refused.reason ?? "", new RegExp(runId));
    assert.match(refused.reason ?? "", new RegExp(dirId));
    assert.match(refused.reason ?? "", new RegExp(readableId));
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "command_rejected").length, 1, "only the unknown id is refused");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 06d: every inbox command ends applied, queued or refused with a recorded event", async () => {
  const setup = await setupAwaitingOwner(["false"]);
  try {
    const runId = setup.conductor.state.phase.runId;
    // Name-sorted so the steer (no worker, still AWAITING_OWNER) queues before
    // the correction applies now (AWAITING_OWNER); the mis-addressed note is
    // refused. One of each outcome.
    writeCommand(setup.runDir, "cmd-aaa-steer", { type: "steer", text: "queued steer", binding: { runId, phaseId: "p1" } });
    writeCommand(setup.runDir, "cmd-bbb-correction", { type: "correction", text: "applied now", binding: { runId, phaseId: "p1" } });
    writeCommand(setup.runDir, "cmd-ccc-unknown", { type: "note", text: "unknown id", binding: { runId: "no-such-run", phaseId: "p1" } });

    await waitFor(() => fs.readdirSync(inboxDir(setup.runDir)).filter((n) => n.endsWith(".json")).length === 0, 30_000);
    const inputs = setup.conductor.state.phase.ownerInputs ?? [];
    assert.equal(inputs.find((i) => i.id === "cmd-aaa-steer")!.state, "queued", "queued");
    assert.equal(inputs.find((i) => i.id === "cmd-bbb-correction")!.state, "correction-started", "applied");
    assert.equal(inputs.find((i) => i.id === "cmd-ccc-unknown")!.state, "refused", "refused");
    // Every outcome is an OWNER_INPUT_RECORDED event; the refusal additionally
    // has its command_rejected record.
    const recorded = eventsOfType(setup.runDir, "OWNER_INPUT_RECORDED") as Array<{ input?: { id?: string; state?: string } }>;
    for (const id of ["cmd-aaa-steer", "cmd-bbb-correction", "cmd-ccc-unknown"]) {
      assert.ok(recorded.some((r) => r.input?.id === id), `${id} has a recorded outcome event`);
    }
    assert.equal(readEvents(setup.runDir).filter((r) => r.kind === "command_rejected").length, 1);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("plan 06d: an event log recorded before queued input replays to its recorded phase", async () => {
  // A log that recorded a refusal (input after DONE) must replay to DONE with
  // the refusal intact: a refusal is never turned into a queued input after
  // the fact.
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
    writeCommand(setup.runDir, "cmd-after-done", { type: "note", text: "too late", binding: { runId: setup.conductor.state.phase.runId, phaseId: "p1" } });
    const restarted = await restart(setup);
    try {
      await waitFor(() => readEvents(setup.runDir).some((r) => r.kind === "command_rejected"), 15_000);
      const recorded = readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "OWNER_INPUT_RECORDED").map((r) => r.event as { input?: { id?: string; state?: string } });
      assert.equal(recorded.find((e) => e.input?.id === "cmd-after-done")!.input!.state, "refused");
      assert.equal(eventsOfType(setup.runDir, "NOTE_ADDED").length, 0, "a refusal is never turned into a queued note");
      assert.equal(restarted.state.phase.phase, "DONE");
      // Rebuilding the same log again gives the same recorded phase and refusal.
      const rebuilt = rebuildState(setup.runDir, setup.plan, { lenient: true });
      assert.equal(rebuilt.phase.phase, "DONE");
      assert.equal((rebuilt.phase.ownerInputs ?? []).find((i) => i.id === "cmd-after-done")!.state, "refused");
    } finally {
      await restarted.stop();
    }
  } finally {
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
