// Per-plan time limits (plan `deadlines`, from TT_*_MINUTES): a detached
// conductor started by `tt start` applies the plan's own per-command limit
// (and tells the extension to wait that long), instead of the defaults.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, makeRepo, readEvents, waitFor } from "./harness.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { RunPlanFile } from "../../src/conductor.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

test("plan-deadlines: a plan's own sh limit governs the detached conductor", async () => {
  const repo = makeRepo();
  const root = path.join("/tmp", `tt-pd-root-${randomBytes(4).toString("hex")}`);
  const scripts = path.join("/tmp", `tt-pd-scripts-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(root, { recursive: true });
  fs.mkdirSync(scripts, { recursive: true });
  const plan: RunPlanFile = {
    title: "plan-deadlines",
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    deadlines: { shCommandMs: 2_000 },
  } as RunPlanFile;
  fs.writeFileSync(path.join(scripts, "plan.json"), JSON.stringify(plan));
  fs.writeFileSync(
    path.join(scripts, "worker.json"),
    JSON.stringify({ hello: { role: "worker", tools: ROLE_TOOLS.worker }, steps: [{ kind: "call-sh", command: "sleep 30" }, { kind: "hang-until-abort" }] }),
  );
  const env = {
    ...process.env,
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    FAKE_PI_SCRIPT: scripts,
  };
  let runId = "";
  try {
    runId = execFileSync(process.execPath, [CLI_PATH, "start", path.join(scripts, "plan.json"), "--root", root], { encoding: "utf8", env }).trim();
    const runDir = path.join(root, runId);
    const started = Date.now();
    // The sh intent is logged when the command starts; the group is gone
    // (killed by the 2 s plan limit) well before the 30 s sleep ends.
    await waitFor(() => readEvents(runDir).some((e) => e.kind === "intent" && String(e.actionId ?? "").startsWith("sh-")), 20_000);
    const pgid = (readEvents(runDir).find((e) => String(e.actionId ?? "").startsWith("sh-"))!.event as { pgid: number }).pgid;
    await waitFor(() => {
      try {
        process.kill(-pgid, 0);
        return false;
      } catch {
        return true;
      }
    }, 15_000);
    assert.ok(Date.now() - started < 15_000, "killed by the plan's 2 s limit, not the 3-minute default");
  } finally {
    if (runId) {
      try {
        execFileSync(process.execPath, [CLI_PATH, "stop", runId, "--root", root], { env });
      } catch {
        // already gone
      }
    }
    cleanupDir(root);
    cleanupDir(scripts);
    cleanupDir(repo.dir);
  }
});
