// Plan 01c: the linter's CLI contract.
//
// `tt lint` prints the findings and exits non-zero on errors (so Emacs can
// branch on the exit status). `tt start` and `tt program start` lint every
// plan first: an error refuses the start with the message on stderr, warnings
// are printed and the start continues.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomBytes } from "node:crypto";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { cleanupDir, makeRepo } from "./harness.ts";
import type { RunPlanFile } from "../../src/conductor.ts";
import type { ProgramFile } from "../../src/core/program.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function tmpDir(prefix: string): string {
  return fs.mkdtempSync(path.join("/tmp", `${prefix}-${randomBytes(3).toString("hex")}-`));
}

function runCli(args: string[], env: NodeJS.ProcessEnv): Promise<{ code: number | null; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI, ...args], { env: { ...process.env, ...env } });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ code, stdout, stderr }));
  });
}

function planWith(acceptance: string[], acceptanceLines: number[], repo: string): RunPlanFile {
  return {
    title: "plan-lint-cli",
    sourceFile: "/tmp/PLAN.org",
    repo,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "g", acceptance, acceptanceLines, checks: ["true"], boundaries: [], reserved: [] }],
  };
}

function programWith(plan: RunPlanFile): ProgramFile {
  return { title: "program-lint-cli", maxParallel: 1, branches: "stack", entries: [{ id: "e1", after: [], plan }] };
}

/** Best-effort kill of a detached conductor/scheduler and its node runs. */
function killUnder(dir: string): void {
  try {
    execFileSync("pkill", ["-9", "-f", dir]);
  } catch {
    // nothing matched
  }
}

async function waitForPid(dir: string, pidFile: string, ms = 5000): Promise<void> {
  const start = Date.now();
  while (!fs.existsSync(path.join(dir, pidFile))) {
    if (Date.now() - start > ms) return;
    await new Promise((r) => setTimeout(r, 25));
  }
}

test("plan-lint CLI: `tt lint` reports an error with file and line and exits non-zero", async () => {
  const dir = tmpDir("tt-lint");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(planPath, JSON.stringify(planWith(["the owner records a live run"], [39], "/tmp/repo")));
    const r = await runCli(["lint", planPath], {});
    assert.notEqual(r.code, 0);
    assert.match(r.stdout, /\/tmp\/PLAN\.org:39: error: \[p1\]/);
    assert.match(r.stdout, /Owner checklist/);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt lint` prints warnings and exits zero", async () => {
  const dir = tmpDir("tt-lint");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(planPath, JSON.stringify(planWith(["the SHA is the current rebased tip"], [7], "/tmp/repo")));
    const r = await runCli(["lint", planPath], {});
    assert.equal(r.code, 0);
    assert.match(r.stdout, /warning: \[p1\]/);
    assert.doesNotMatch(r.stdout, /error:/);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt lint` on a clean plan prints nothing and exits zero", async () => {
  const dir = tmpDir("tt-lint");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(planPath, JSON.stringify(planWith(["all existing tests still pass"], [9], "/tmp/repo")));
    const r = await runCli(["lint", planPath], {});
    assert.equal(r.code, 0);
    assert.equal(r.stdout.trim(), "");
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt lint` reports an unknown #+TT_MODELS role with file and line", async () => {
  const dir = tmpDir("tt-lint-models");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(
      planPath,
      JSON.stringify({ ...planWith(["all existing tests still pass"], [9], "/tmp/repo"), models: { foo: { model: "x" } }, modelsLine: 4 }),
    );
    const r = await runCli(["lint", planPath], {});
    assert.notEqual(r.code, 0);
    assert.match(r.stdout, /\/tmp\/PLAN\.org:4: error: \[models\]/);
    assert.match(r.stdout, /unknown role foo/);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt lint` reports a role given twice", async () => {
  const dir = tmpDir("tt-lint-models");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(
      planPath,
      JSON.stringify({
        ...planWith(["all existing tests still pass"], [9], "/tmp/repo"),
        models: { worker: { model: "b" } },
        modelsRepeated: ["worker"],
        modelsLine: 5,
      }),
    );
    const r = await runCli(["lint", planPath], {});
    assert.notEqual(r.code, 0);
    assert.match(r.stdout, /\/tmp\/PLAN\.org:5: error: \[models\]/);
    assert.match(r.stdout, /worker more than once/);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt lint` reports an empty model", async () => {
  const dir = tmpDir("tt-lint-models");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(
      planPath,
      JSON.stringify({ ...planWith(["all existing tests still pass"], [9], "/tmp/repo"), models: { reviewer: {} }, modelsLine: 6 }),
    );
    const r = await runCli(["lint", planPath], {});
    assert.notEqual(r.code, 0);
    assert.match(r.stdout, /\/tmp\/PLAN\.org:6: error: \[models\]/);
    assert.match(r.stdout, /empty model/);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: a plan that uses #+TT_MODELS cleanly passes", async () => {
  const dir = tmpDir("tt-lint-models");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(
      planPath,
      JSON.stringify({
        ...planWith(["all existing tests still pass"], [9], "/tmp/repo"),
        models: { worker: { model: "deepseek/deepseek-v4.1-flash" }, reviewer: { provider: "vercel-ai-gateway", model: "anthropic/claude-sonnet-5" } },
        modelsLine: 3,
      }),
    );
    const r = await runCli(["lint", planPath], {});
    assert.equal(r.code, 0, r.stdout);
    assert.equal(r.stdout.trim(), "");
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: a program-level #+TT_MODELS is reported once, at the program file", async () => {
  const dir = tmpDir("tt-lint-prog-models");
  try {
    const programPath = path.join(dir, "program.json");
    const entry = planWith(["all existing tests still pass"], [9], "/tmp/repo");
    // The shape Emacs emits: the program's `foo` copied into every entry and
    // recorded in `modelsFromProgram`. It must be reported once, at the
    // program's own Org file, never once per entry or as the temp JSON path.
    fs.writeFileSync(
      programPath,
      JSON.stringify({
        title: "program-models",
        sourceFile: "/tmp/PROGRAM.org",
        maxParallel: 1,
        branches: "stack",
        models: { foo: { model: "x" } },
        modelsLine: 3,
        entries: [
          { id: "e1", after: [], plan: { ...entry, sourceFile: "/tmp/e1.org", models: { foo: { model: "x" } }, modelsLine: 3, modelsFromProgram: ["foo"] } },
          { id: "e2", after: [], plan: { ...entry, sourceFile: "/tmp/e2.org", models: { foo: { model: "x" } }, modelsLine: 3, modelsFromProgram: ["foo"] } },
        ],
      }),
    );
    const r = await runCli(["lint", programPath], {});
    assert.notEqual(r.code, 0);
    assert.equal((r.stdout.match(/error: \[models\]/g) ?? []).length, 1, `exactly one program-level finding:\n${r.stdout}`);
    assert.match(r.stdout, /\/tmp\/PROGRAM\.org:3: error: \[models\]/);
    assert.doesNotMatch(r.stdout, /\[e[12]\/models\]/);
    assert.doesNotMatch(r.stdout, /program\.json/);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt lint` on a program lints every entry", async () => {
  const dir = tmpDir("tt-lint");
  try {
    const programPath = path.join(dir, "program.json");
    fs.writeFileSync(programPath, JSON.stringify(programWith(planWith(["the owner records a live run"], [3], "/tmp/repo"))));
    const r = await runCli(["lint", programPath], {});
    assert.notEqual(r.code, 0);
    assert.match(r.stdout, /error: \[e1\/p1\]/);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt start` refuses an error before any run exists", async () => {
  const dir = tmpDir("tt-start-lint");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(planPath, JSON.stringify(planWith(["the owner records a live run"], [39], "/tmp/repo")));
    const r = await runCli(["start", planPath, "--root", path.join(dir, "root")], {});
    assert.notEqual(r.code, 0);
    assert.match(r.stderr, /error: \[p1\]/);
    // No run directory was created.
    assert.deepEqual(fs.existsSync(path.join(dir, "root")) ? fs.readdirSync(path.join(dir, "root")) : [], []);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt program start` refuses an error before any program exists", async () => {
  const dir = tmpDir("tt-prog-lint");
  try {
    const programPath = path.join(dir, "program.json");
    fs.writeFileSync(programPath, JSON.stringify(programWith(planWith(["someone approves the release"], [4], "/tmp/repo"))));
    const root = path.join(dir, "root");
    const r = await runCli(["program", "start", programPath, "--root", root], {});
    assert.notEqual(r.code, 0);
    assert.match(r.stderr, /error: \[e1\/p1\]/);
    assert.equal(fs.existsSync(path.join(root, "programs")), false);
  } finally {
    cleanupDir(dir);
  }
});

test("plan-lint CLI: `tt start` prints warnings and starts", async () => {
  const repo = makeRepo();
  const dir = tmpDir("tt-start-warn");
  const root = path.join(dir, "root");
  fs.mkdirSync(root, { recursive: true });
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(planPath, JSON.stringify(planWith(["the SHA is the current rebased tip"], [7], repo.dir)));
    const r = await runCli(["start", planPath, "--root", root], { TT_TEST_MODE: "1", TT_TEST_PI_COMMAND: "/bin/true" });
    assert.equal(r.code, 0, r.stderr);
    assert.match(r.stderr, /warning: \[p1\]/);
    const runId = r.stdout.trim();
    assert.ok(runId.length > 0, "expected a run id on stdout");
    await waitForPid(path.join(root, runId), "conductor.pid");
    killUnder(path.join(root, runId));
  } finally {
    killUnder(root);
    cleanupDir(root);
    cleanupDir(dir);
    cleanupDir(repo.dir);
  }
});

test("plan-lint CLI: `tt program start` prints warnings and starts", async () => {
  const repo = makeRepo();
  const dir = tmpDir("tt-prog-warn");
  const root = path.join(dir, "root");
  try {
    const programPath = path.join(dir, "program.json");
    fs.writeFileSync(programPath, JSON.stringify(programWith(planWith(["the SHA is the current rebased tip"], [7], repo.dir))));
    const r = await runCli(["program", "start", programPath, "--root", root], { TT_TEST_MODE: "1", TT_TEST_PI_COMMAND: "/bin/true" });
    assert.equal(r.code, 0, r.stderr);
    assert.match(r.stderr, /warning: \[e1\/p1\]/);
    const programId = r.stdout.trim();
    assert.ok(programId.length > 0, "expected a program id on stdout");
    assert.ok(fs.existsSync(path.join(root, "programs", programId, "program.json")));
    await waitForPid(path.join(root, "programs", programId), "scheduler.pid");
    killUnder(root);
  } finally {
    killUnder(root);
    cleanupDir(root);
    cleanupDir(dir);
    cleanupDir(repo.dir);
  }
});

test("plan-lint CLI: `tt lint` reports a bad seat and a panel=reviewers conflict with file and line", async () => {
  const dir = tmpDir("tt-lint-seats");
  try {
    const planPath = path.join(dir, "plan.json");
    fs.writeFileSync(
      planPath,
      JSON.stringify({
        ...planWith(["all existing tests still pass"], [9], "/tmp/repo"),
        models: { reviewerSeats: { X: { model: "x" } }, panelFrom: "reviewers", panelSeats: { "2": { model: "own" } } },
        modelsRepeated: ["reviewer.M"],
        modelsLine: 4,
      }),
    );
    const r = await runCli(["lint", planPath], {});
    assert.notEqual(r.code, 0);
    assert.match(r.stdout, /\/tmp\/PLAN\.org:4: error: \[models\]/);
    assert.match(r.stdout, /unknown reviewer seat reviewer\.X/);
    assert.match(r.stdout, /panel=reviewers together with an explicit panel seat/);
    assert.match(r.stdout, /reviewer\.M more than once/);
  } finally {
    cleanupDir(dir);
  }
});
