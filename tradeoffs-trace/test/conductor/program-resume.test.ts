// Phase 4 recovery: a program survives being interrupted.
// 1. `tt program stop` then `tt program resume` finishes the program.
// 2. A crash (the scheduler and a node's conductor SIGKILLed mid-run) is
//    recovered: `tt program resume` relaunches the scheduler, which restarts
//    the crashed node's conductor from its control log.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, makeRepo, waitFor } from "./harness.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { contractVersionFor, type RunPlanFile } from "../../src/conductor.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import { createProgram, programPaths, runScheduler } from "../../src/program.ts";

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
  return { root, scripts, cli, nodes, cleanup, env, program };
}

/** Count the NODE_STARTED events a node has in the program's own log. */
function nodeStartedCount(root: string, id: string, node: string): number {
  const file = path.join(root, "programs", id, "events.jsonl");
  if (!fs.existsSync(file)) return 0;
  return fs
    .readFileSync(file, "utf8")
    .split("\n")
    .filter(Boolean)
    .map((line) => JSON.parse(line) as { event?: { type?: string; node?: string } })
    .filter((r) => r.event?.type === "NODE_STARTED" && r.event.node === node).length;
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

test("plan 06d: tt program pause stops new node starts while a running node finishes, and resume starts the next node", async () => {
  const s = setup();
  let id = "";
  try {
    id = s.cli(["program", "start", path.join(s.scripts, "program.json")]).trim();
    await waitFor(() => s.nodes(id).a.status === "running", 20_000, 200);
    // Pause while `a` runs. Running nodes go on; no new node starts.
    s.cli(["program", "pause", id]);
    await waitFor(() => s.nodes(id).a.status === "done", 120_000, 300);
    assert.equal(s.nodes(id).b.status, "waiting", "no new node starts while paused");
    assert.equal(nodeStartedCount(s.root, id, "b"), 0, "b never started while paused");
    // Resume starts the next ready node.
    s.cli(["program", "resume", id]);
    await waitFor(() => s.nodes(id).b.status === "done", 120_000, 300);
    assert.equal(s.nodes(id).a.status, "done");
    assert.equal(nodeStartedCount(s.root, id, "b"), 1, "b starts exactly once, after resume");
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

test("plan 06d: a second scheduler for the same program exits with scheduler already running and the node starts once", async () => {
  const s = setup();
  let id = "";
  try {
    id = s.cli(["program", "start", path.join(s.scripts, "program.json")]).trim();
    await waitFor(() => s.nodes(id).a.status === "running", 20_000, 200);
    const programDir = path.join(s.root, "programs", id);
    // A second scheduler on the same program must exit at once, starting no
    // node: the first still holds `scheduler.lock`.
    const second = spawnSync(process.execPath, [CLI_PATH, "__run-program", programDir], {
      encoding: "utf8",
      env: s.env,
      timeout: 30_000,
    });
    assert.match(second.stderr, /scheduler already running/);
    assert.equal(nodeStartedCount(s.root, id, "a"), 1, "a started exactly once");
    // The first scheduler is untouched and finishes the program.
    await waitFor(() => s.nodes(id).b.status === "done", 120_000, 300);
    assert.equal(nodeStartedCount(s.root, id, "a"), 1);
    assert.equal(nodeStartedCount(s.root, id, "b"), 1);
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

test("plan 06d: a scheduler lock left by a dead pid is taken over", async () => {
  const root = path.join("/tmp", `tt-06d-stale-${randomBytes(4).toString("hex")}`);
  const repo = makeRepo();
  fs.mkdirSync(root, { recursive: true });
  try {
    const phase = { id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] };
    const program: ProgramFile = {
      title: "stale-lock",
      maxParallel: 1,
      entries: [
        { id: "a", after: [], plan: { title: "a", repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase] } },
      ],
    };
    const dir = createProgram(root, program, "stale06d");
    // A scheduler that died left this file behind: its pid is gone, so the
    // flock died with it and a new scheduler must take the lock over.
    fs.writeFileSync(programPaths(dir).lock, "999999");
    let launched = 0;
    const outcome = await runScheduler(dir, { runRoot: root, launch: () => { launched += 1; }, pollMs: 10, maxTicks: 3 });
    assert.ok(launched >= 1, "the node started despite the stale lock");
    assert.equal(outcome, "running");
    assert.equal(nodeStartedCount(root, "stale06d", "a"), 1, "the node starts exactly once");
  } finally {
    cleanupDir(root);
    cleanupDir(repo.dir);
  }
});
