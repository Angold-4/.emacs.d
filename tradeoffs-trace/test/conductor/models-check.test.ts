// 06a finding #24: `tt program start` runs the models check before it
// launches the scheduler. A configured model the gateway refuses stops the
// whole program (naming the gateway's message); `--skip-models-check` starts
// it anyway. The unit-level classification lives in test/unit/models-check.test.ts;
// this is the CLI wiring that reads a program, writes the record and refuses.

import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, makeRepo, waitFor } from "./harness.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { contractVersionFor, type RunPlanFile } from "../../src/conductor.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import type { PlanModels } from "../../src/core/roles.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function plan(repo: string, models: PlanModels): RunPlanFile {
  const phase = { id: "p1", goal: "add one file", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
  return { title: "models node", repo, integrationBranch: "main", checks: ["true"], models, phases: [phase] };
}

function writeScripts(dir: string, print: { mode: "ok" | "refused"; text?: string }): void {
  const phase = { id: "p1", goal: "add one file", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
  fs.writeFileSync(
    path.join(dir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      print,
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
  );
  fs.writeFileSync(
    path.join(dir, "reviewer.json"),
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
          },
        },
      ],
    }),
  );
}

function startEnv(scriptsDir: string): NodeJS.ProcessEnv {
  return {
    ...process.env,
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    TT_TEST_STUB_REVIEWS: "1",
    FAKE_PI_SCRIPT: scriptsDir,
    TT_MODELS_CHECK_TIMEOUT_MS: "5000",
    TT_NOTIFY_COMMAND: ":",
    TT_TEST_DEADLINES: JSON.stringify({
      inboxPollMs: 100,
      abortGraceMs: 500,
      termGraceMs: 500,
      helloTimeoutMs: 5_000,
      workerAttemptMs: 20_000,
      freezeMs: 10_000,
      checkMs: 5_000,
      probeMs: 5_000,
      reviewMs: 10_000,
    }),
  };
}

/** The newest program directory under ROOT. */
function newestProgram(root: string): string | undefined {
  const dir = path.join(root, "programs");
  try {
    return fs
      .readdirSync(dir)
      .map((id) => path.join(dir, id))
      .sort((a, b) => fs.statSync(b).mtimeMs - fs.statSync(a).mtimeMs)[0];
  } catch {
    return undefined;
  }
}

test("models-check: tt program start refuses a refused model and records the refusal", () => {
  const repo = makeRepo();
  const root = shortTmp("tt-mc-prog-root");
  const scriptsDir = shortTmp("tt-mc-prog-scripts");
  const models: PlanModels = { worker: { provider: "vercel-ai-gateway", model: "spacexai/grok-4.7" } };
  const program: ProgramFile = { title: "refused", maxParallel: 1, entries: [{ id: "a", after: [], plan: plan(repo.dir, models) }] };
  const programPath = path.join(scriptsDir, "program.json");
  fs.writeFileSync(programPath, JSON.stringify(program));
  writeScripts(scriptsDir, { mode: "refused", text: "403 restricted" });
  try {
    const r = spawnSync(process.execPath, [CLI_PATH, "program", "start", programPath, "--root", root], { encoding: "utf8", env: startEnv(scriptsDir) });
    assert.notEqual(r.status, 0, "a refused model refuses the start");
    assert.match(`${r.stdout}${r.stderr}`, /refusing to start the program/);
    assert.match(`${r.stdout}${r.stderr}`, /grok-4\.7/);
    // The refusal is recorded in the program directory the check belongs to.
    const dir = newestProgram(root);
    assert.ok(dir, "the program directory was created");
    const record = JSON.parse(fs.readFileSync(path.join(dir!, "models-check.json"), "utf8")) as { probes: Array<{ status: string; message?: string }> };
    assert.equal(record.probes.length, 1);
    assert.equal(record.probes[0].status, "refused");
    assert.match(record.probes[0].message ?? "", /403 restricted/);
    // No scheduler was launched.
    assert.equal(fs.existsSync(path.join(dir!, "scheduler.pid")), false);
  } finally {
    cleanupDir(root);
    cleanupDir(scriptsDir);
    cleanupDir(repo.dir);
  }
});

test("models-check: tt program start --skip-models-check starts with a refused model", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-mc-skip-root");
  const scriptsDir = shortTmp("tt-mc-skip-scripts");
  const models: PlanModels = { worker: { provider: "vercel-ai-gateway", model: "spacexai/grok-4.7" } };
  const program: ProgramFile = { title: "skip", maxParallel: 1, entries: [{ id: "a", after: [], plan: plan(repo.dir, models) }] };
  const programPath = path.join(scriptsDir, "program.json");
  fs.writeFileSync(programPath, JSON.stringify(program));
  // The print probe still refuses; only `--skip-models-check` lets it start.
  writeScripts(scriptsDir, { mode: "refused", text: "403 restricted" });
  const env = startEnv(scriptsDir);
  const cli = (args: string[]) => execFileSync(process.execPath, [CLI_PATH, ...args, "--root", root], { encoding: "utf8", env });
  const nodeStatus = (id: string): string => {
    const state = JSON.parse(cli(["program", "state", id])) as { state: { nodes: Record<string, { status: string }> } };
    return state.state.nodes.a?.status ?? "?";
  };
  let programId = "";
  try {
    programId = cli(["program", "start", programPath, "--skip-models-check"]).trim();
    assert.ok(programId.length > 0, "tt program start --skip-models-check prints the program id");
    const dir = path.join(root, "programs", programId);
    const record = JSON.parse(fs.readFileSync(path.join(dir, "models-check.json"), "utf8")) as { probes: Array<{ status: string }> };
    assert.equal(record.probes[0].status, "refused");
    // It really started: the node runs to DONE with the fake agents, so the
    // scheduler exits on its own and nothing is left running.
    await waitFor(() => nodeStatus(programId) === "done", 60_000, 100);
  } finally {
    if (programId) {
      try {
        cli(["program", "stop", programId]);
      } catch {
        // already stopped / finished
      }
    }
    cleanupDir(root);
    cleanupDir(scriptsDir);
    cleanupDir(repo.dir);
  }
});
