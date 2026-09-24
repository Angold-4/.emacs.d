// Phase 4 recovery: a program survives being interrupted.
// 1. `tt program stop` then `tt program resume` finishes the program.
// 2. A crash (the scheduler and a node's conductor SIGKILLed mid-run) is
//    recovered: `tt program resume` relaunches the scheduler, which restarts
//    the crashed node's conductor from its control log.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, makeRepo, waitFor } from "./harness.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { contractVersionFor, type RunPlanFile } from "../../src/conductor.ts";
import type { ProgramFile } from "../../src/core/program.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function setup() {
  const repo = makeRepo();
  const root = path.join("/tmp", `tt-pr-root-${randomBytes(4).toString("hex")}`);
  const scripts = path.join("/tmp", `tt-pr-scripts-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(root, { recursive: true });
  fs.mkdirSync(scripts, { recursive: true });
  const phase = { id: "p1", goal: "add one file", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
  const plan = (title: string): RunPlanFile => ({ title, repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase] });
  const program: ProgramFile = {
    title: "program-resume",
    maxParallel: 1,
    entries: [
      { id: "a", after: [], plan: plan("a") },
      { id: "b", after: ["a"], plan: plan("b") },
    ],
  };
  fs.writeFileSync(path.join(scripts, "program.json"), JSON.stringify(program));
  // The worker's command takes a few seconds, so the test can interrupt it.
  fs.writeFileSync(
    path.join(scripts, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [
        { kind: "call-sh", command: 'sleep 3; echo "$TT_AGENT_ID" > "node-$(date +%s)-$$.txt"' },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  );
  fs.writeFileSync(
    path.join(scripts, "reviewer.json"),
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
  const env: NodeJS.ProcessEnv = {
    ...process.env,
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    TT_TEST_STUB_REVIEWS: "1",
    FAKE_PI_SCRIPT: scripts,
  };
  const cli = (args: string[]) => execFileSync(process.execPath, [CLI_PATH, ...args, "--root", root], { encoding: "utf8", env });
  const nodes = (id: string) =>
    (JSON.parse(cli(["program", "state", id])) as { state: { nodes: Record<string, { status: string; runId?: string }> } }).state.nodes;
  const cleanup = () => {
    cleanupDir(root);
    cleanupDir(scripts);
    cleanupDir(repo.dir);
  };
  return { root, scripts, cli, nodes, cleanup };
}

test("program-resume: stop, then resume, finishes the program", async () => {
  const s = setup();
  let id = "";
  try {
    id = s.cli(["program", "start", path.join(s.scripts, "program.json")]).trim();
    await waitFor(() => s.nodes(id).a.status === "running", 20_000, 200);
    s.cli(["program", "stop", id]);
    await waitFor(() => s.nodes(id).a.status === "stopped", 30_000, 300);
    s.cli(["program", "resume", id]);
    await waitFor(() => s.nodes(id).b.status === "done", 120_000, 300);
    assert.equal(s.nodes(id).a.status, "done");
  } finally {
    if (id) {
      try {
        s.cli(["program", "stop", id]);
      } catch {
        // finished
      }
    }
    s.cleanup();
  }
});

test("program-resume: a crashed scheduler and conductor are recovered by resume", async () => {
  const s = setup();
  let id = "";
  try {
    id = s.cli(["program", "start", path.join(s.scripts, "program.json")]).trim();
    await waitFor(() => s.nodes(id).a.status === "running", 20_000, 200);
    const runDir = path.join(s.root, s.nodes(id).a.runId!);
    await waitFor(() => fs.existsSync(path.join(runDir, "conductor.pid")), 20_000, 100);
    // Simulate a crash: SIGKILL the scheduler and the node's conductor.
    const kill = (file: string) => {
      try {
        process.kill(Number(fs.readFileSync(file, "utf8")), "SIGKILL");
      } catch {
        // already gone
      }
    };
    kill(path.join(s.root, "programs", id, "scheduler.pid"));
    kill(path.join(runDir, "conductor.pid"));
    s.cli(["program", "resume", id]);
    await waitFor(() => s.nodes(id).b.status === "done", 120_000, 300);
    assert.equal(s.nodes(id).a.status, "done", "the crashed node's run was restarted and finished");
    const events = fs.readFileSync(path.join(s.root, "programs", id, "events.jsonl"), "utf8");
    assert.match(events, /NODE_RESUMED/);
  } finally {
    if (id) {
      try {
        s.cli(["program", "stop", id]);
      } catch {
        // finished
      }
    }
    s.cleanup();
  }
});
