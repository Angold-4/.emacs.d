import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";
import { createRun, rebuildState, type RunPlanFile } from "../../src/conductor.ts";
import { normalizeDecisionViewCommand } from "../../src/core/owner-inbox.ts";
import { reduce } from "../../src/core/reduce.ts";
import { EventLog } from "../../src/effects/log.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function shell(command: string, cwd: string, env: NodeJS.ProcessEnv): Promise<{ code: number; output: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn("bash", ["-c", command], { cwd, env: { ...process.env, TT_NOTIFY_COMMAND: ":", ...env } });
    let output = "";
    child.stdout.on("data", (d) => output += d);
    child.stderr.on("data", (d) => output += d);
    child.on("error", reject);
    child.on("close", (code) => resolve({ code: code ?? 1, output }));
  });
}

test("a printed owner command applies when pasted", async () => {
  const cwd = fs.mkdtempSync("/tmp/tt-paste-");
  const root = path.join(cwd, "runs");
  fs.mkdirSync(root);
  const plan: RunPlanFile = { title: "parser", repo: "", integrationBranch: "main", phases: [
    { id: "p1", goal: "parse strings", acceptance: ["strings parse"], checks: [], boundaries: [], reserved: [] },
  ] };
  const runDir = createRun(root, plan, "directory-id");
  const log = new EventLog(path.join(runDir, "events.jsonl"));
  const runId = "init-id";
  log.append("init", { runId, integrationHead: "H0" });
  log.append("event", { type: "ATTEMPT_STARTED", attempt: 1 });
  log.append("event", { type: "SUBMIT_PHASE", decisions: [], assumptions: [], deviations: [] });
  log.append("event", { type: "FREEZE_COMPLETED", candidateSha: "C1", decisions: [] });
  log.append("event", { type: "OWNER_REQUEST_OPENED", request: {
    id: "OR-1", version: 1, phaseId: "p1", reason: "choose the parser policy", origin: "repair_budget_exhausted",
    status: "open", options: [{ id: "stop", label: "Stop the phase" }],
  } });
  // This test supplies only the inbox service: use the production normalizer,
  // reducer and event log, without starting unrelated workers or sockets.
  let serviceError: unknown;
  const poll = setInterval(() => {
    try {
      for (const name of fs.readdirSync(path.join(runDir, "inbox")).filter((n) => n.endsWith(".json"))) {
        const file = path.join(runDir, "inbox", name);
        const normalized = normalizeDecisionViewCommand(JSON.parse(fs.readFileSync(file, "utf8")), name.slice(0, -5));
        assert.ok(normalized.ok, !normalized.ok ? normalized.reason : "");
        const result = reduce(rebuildState(runDir, plan), normalized.event);
        assert.ok(result.ok, !result.ok ? result.reason : "");
        log.append("event", normalized.event);
        fs.renameSync(file, path.join(runDir, "inbox", "applied", name));
      }
    } catch (err) { serviceError = err; }
  }, 20);
  try {
    assert.notEqual(path.basename(runDir), runId);
    const bin = path.join(cwd, "bin");
    fs.mkdirSync(bin);
    fs.writeFileSync(path.join(bin, "tt"), '#!/bin/sh\nexec "$TT_TEST_NODE" "$TT_TEST_CLI" "$@" --root "$TT_TEST_ROOT"\n', { mode: 0o755 });
    const env = { PATH: `${bin}:${process.env.PATH}`, TT_TEST_NODE: process.execPath, TT_TEST_CLI: CLI, TT_TEST_ROOT: root };
    const why = await shell("tt why directory-id", cwd, env);
    assert.equal(why.code, 0, why.output);
    const printed = why.output.split("\n").map((line) => line.trim()).find((line) => line.startsWith("printf ") && line.includes(" | tt input "));
    assert.ok(printed, why.output);
    assert.ok(printed.includes(`tt input ${runId}`), printed);
    const pasted = await shell(printed, cwd, env);
    assert.ifError(serviceError);
    assert.equal(pasted.code, 0, pasted.output);
    assert.match(pasted.output, /resolve applied/);
    assert.equal(rebuildState(runDir, plan).phase.ownerRequests[0].status, "resolved");
    assert.equal(fs.existsSync(path.join(cwd, runId)), false);
  } finally {
    clearInterval(poll);
    log.close();
    fs.rmSync(cwd, { recursive: true, force: true });
  }
});
