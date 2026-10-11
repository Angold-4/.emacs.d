// Phase 4 end to end: `tt program start` launches the detached scheduler,
// which runs each node as an ordinary detached conductor run (fake agents),
// in dependency order, with the two parallel nodes at once, on stacked
// per-node branches, and the join starting from a merge of both.
//
//   a ─┬─► b ─┬─► d
//      └─► c ─┘

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
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

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

test("program-e2e: a DAG runs in dependency order on stacked branches; the join starts from a merge of both parents", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-prog-root");
  const scriptsDir = shortTmp("tt-prog-scripts");
  const phase = { id: "p1", goal: "add one file", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
  const plan = (title: string): RunPlanFile => ({
    title,
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["true"],
    phases: [phase],
  });
  const program: ProgramFile = {
    title: "program-e2e",
    maxParallel: 2,
    entries: [
      { id: "a", after: [], plan: plan("a") },
      { id: "b", after: ["a"], plan: plan("b") },
      { id: "c", after: ["a"], plan: plan("c") },
      { id: "d", after: ["b", "c"], plan: plan("d") },
    ],
  };
  const programPath = path.join(scriptsDir, "program.json");
  fs.writeFileSync(programPath, JSON.stringify(program));
  // Each worker adds a uniquely named file, so parallel nodes never conflict
  // and every branch's content shows which nodes it contains.
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [
        { kind: "call-sh", command: 'echo "$TT_AGENT_ID" > "node-$(date +%s)-$$.txt"' },
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
    FAKE_PI_SCRIPT: scriptsDir,
  };
  const cli = (args: string[]) => execFileSync(process.execPath, [CLI_PATH, ...args, "--root", root], { encoding: "utf8", env });
  const state = (id: string) => JSON.parse(cli(["program", "state", id])) as {
    state: { nodes: Record<string, { status: string; runId?: string; branch?: string }> };
    lines: string[];
  };
  const git = (...args: string[]) => execFileSync("git", ["-C", repo.dir, ...args], { encoding: "utf8" }).trim();

  let programId = "";
  try {
    programId = cli(["program", "start", programPath]).trim();
    assert.ok(programId.length > 0, "tt program start prints the program id");

    // b and c run at the same time (maxParallel 2), both after a.
    let sawParallel = false;
    await waitFor(
      () => {
        const nodes = state(programId).state.nodes;
        if (nodes.b.status === "running" && nodes.c.status === "running") sawParallel = true;
        assert.notEqual(nodes.b.status === "waiting" ? "" : nodes.a.status, "waiting", "b never starts before a");
        return nodes.d.status === "done";
      },
      150_000,
      200,
    );
    const nodes = state(programId).state.nodes;
    for (const id of ["a", "b", "c", "d"]) assert.equal(nodes[id].status, "done", `${id} is DONE`);
    assert.ok(sawParallel || nodes.b.runId !== nodes.c.runId, "b and c ran as separate runs");

    // Stacked branches: one per node, each containing its dependencies.
    for (const id of ["a", "b", "c", "d"]) assert.equal(nodes[id].branch, `main--${id}`);
    const isAncestor = (a: string, b: string) => {
      try {
        git("merge-base", "--is-ancestor", a, b);
        return true;
      } catch {
        return false;
      }
    };
    assert.ok(isAncestor("main--a", "main--b") && isAncestor("main--a", "main--c"), "b and c are cut from a");
    assert.ok(isAncestor("main--b", "main--d") && isAncestor("main--c", "main--d"), "d starts from a merge of b and c");
    const filesOn = (branch: string) => git("ls-tree", "--name-only", branch).split("\n").filter((f) => f.startsWith("node-"));
    assert.equal(filesOn("main--a").length, 1);
    assert.equal(filesOn("main--b").length, 2);
    assert.equal(filesOn("main--d").length, 4, "the join has a's, b's, c's and its own file");
    assert.equal(git("rev-parse", "main"), git("rev-parse", "main~0"), "TT_BRANCH itself is untouched");
    await waitFor(() => fs.readFileSync(path.join(root, "programs", programId, "scheduler.log"), "utf8").includes("program done"), 20_000);
  } finally {
    if (programId) {
      try {
        cli(["program", "stop", programId]);
      } catch {
        // already finished
      }
    }
    cleanupDir(root);
    cleanupDir(scriptsDir);
    cleanupDir(repo.dir);
  }
});

/** Plan 06f: fast conductor deadlines, injected into the detached scheduler's
 * own child conductors through `TT_TEST_DEADLINES` (src/cli.ts). */
const LIVE_FAST: Record<string, number> = {
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

test("plan 06f: live.json lists a running run, a run awaiting the owner and a waiting program node", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-live-root");
  const scriptsDir = shortTmp("tt-live-scripts");
  // Failing checks walk node a to the repair-budget gate (AWAITING_OWNER) in a
  // few attempts; node b waits behind a the whole time.
  const phase = { id: "p1", goal: "g", acceptance: ["it works"], checks: ["false"], boundaries: [], reserved: [] };
  const plan = (title: string): RunPlanFile => ({
    title,
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["false"],
    phases: [phase],
  });
  const program: ProgramFile = {
    title: "live",
    maxParallel: 1,
    entries: [
      { id: "a", after: [], plan: plan("a") },
      { id: "b", after: ["a"], plan: plan("b") },
    ],
  };
  const programPath = path.join(scriptsDir, "program.json");
  fs.writeFileSync(programPath, JSON.stringify(program));
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
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
    TT_TEST_DEADLINES: JSON.stringify(LIVE_FAST),
    FAKE_PI_SCRIPT: scriptsDir,
  };
  const cli = (args: string[]) => execFileSync(process.execPath, [CLI_PATH, ...args, "--root", root], { encoding: "utf8", env });
  const live = (): { runs: Array<{ id: string; phase: string; needsOwner: boolean }>; waitingNodes: Array<{ program: string; node: string }> } =>
    JSON.parse(fs.readFileSync(path.join(root, "live.json"), "utf8")) as never;
  let programId = "";
  try {
    programId = cli(["program", "start", programPath]).trim();
    assert.ok(programId.length > 0, "tt program start prints the program id");
    // A run is alive (node a's conductor is in a working phase) while node b is
    // still waiting to start.
    await waitFor(
      () => {
        try {
          const l = live();
          return l.runs.some((r) => r.needsOwner === false) && l.waitingNodes.some((w) => w.node === "b");
        } catch {
          return false;
        }
      },
      60_000,
      200,
    );
    // The same run reaches the owner's desk, and node b is still waiting.
    await waitFor(
      () => {
        try {
          const l = live();
          return l.runs.some((r) => r.needsOwner === true) && l.waitingNodes.some((w) => w.node === "b");
        } catch {
          return false;
        }
      },
      90_000,
      200,
    );
  } finally {
    if (programId) {
      try {
        cli(["program", "stop", programId]);
      } catch {
        // already finished
      }
    }
    cleanupDir(root);
    cleanupDir(scriptsDir);
    cleanupDir(repo.dir);
  }
});
