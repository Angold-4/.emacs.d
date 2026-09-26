// Plan 14: `tt program resume` from a shell without the vendor keys started
// every node keyless; the worker could not run the live gate and deferred it
// for hours. A start or resume that would launch a conductor now refuses when
// a declared secret is not set, naming it (never a value), unless
// TT_ALLOW_MISSING_SECRETS=1.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, makeRepo } from "./harness.ts";
import type { RunPlanFile } from "../../src/conductor.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function runCli(args: string[], env: NodeJS.ProcessEnv): Promise<{ code: number | null; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const e = { ...process.env, ...env };
    delete e.TT_ALLOW_MISSING_SECRETS;
    delete e.TT_MISSING_SECRET_TEST;
    Object.assign(e, env);
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: e });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ code, stdout, stderr }));
  });
}

test("missing secrets: `tt start` and `tt program start` refuse a plan whose declared secret is not set, naming it", async () => {
  const repo = makeRepo();
  const root = path.join("/tmp", `tt-missing-secret-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(root, { recursive: true });
  try {
    const plan: RunPlanFile = {
      title: "needs a key",
      repo: repo.dir,
      integrationBranch: "main",
      checks: ["true"],
      secrets: ["TT_MISSING_SECRET_TEST"],
      phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    } as RunPlanFile;
    const planFile = path.join(root, "plan.json");
    fs.writeFileSync(planFile, JSON.stringify(plan));
    const refused = await runCli(["start", planFile, "--root", root], {});
    assert.equal(refused.code, 1);
    assert.match(refused.stderr, /refusing to start the run: declared secret\(s\) not set in this environment: TT_MISSING_SECRET_TEST/);
    assert.deepEqual(fs.readdirSync(root).filter((f) => f !== "plan.json"), [], "no run was created");

    const programFile = path.join(root, "program.json");
    fs.writeFileSync(programFile, JSON.stringify({ title: "p", maxParallel: 1, entries: [{ id: "a", after: [], plan }] }));
    const programRefused = await runCli(["program", "start", programFile, "--root", root], {});
    assert.equal(programRefused.code, 1);
    assert.match(programRefused.stderr, /refusing to start the program: .*TT_MISSING_SECRET_TEST/);
    assert.ok(!fs.existsSync(path.join(root, "programs")) || fs.readdirSync(path.join(root, "programs")).length === 0, "no program was created");

    // The value itself is never printed, and the escape hatch is named.
    assert.match(refused.stderr, /TT_ALLOW_MISSING_SECRETS=1/);
  } finally {
    cleanupDir(root);
    cleanupDir(repo.dir);
  }
});
