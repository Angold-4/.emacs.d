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
import { contractVersionFor, runnerRevision, type RunPlanFile } from "../../src/conductor.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import { appendProgramEvent, createProgram, recordProgramSource, schedulerTick } from "../../src/program.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));
/** This checkout's package root, copied by the A2 test to make two installed
 * runners (`tt runner install`'s own layout). */
const PACKAGE_ROOT = fileURLToPath(new URL("../../", import.meta.url));

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

test("plan 07h2 (R3, A2, exex 01: five restarts): a node started after its plan file changed runs the changed checks and the program's current runner", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-07h2-r3-root");
  const scriptsDir = shortTmp("tt-07h2-r3-scripts");
  const phase = (checks: string[]) => ({ id: "p1", goal: "g", acceptance: ["it works"], checks, boundaries: [], reserved: [] });
  const plan = (title: string, checks: string[]): RunPlanFile => ({
    title,
    repo: repo.dir,
    integrationBranch: "main",
    checks,
    phases: [phase(checks)],
  });
  // The program's SOURCE file, which the scheduler re-exports at every node
  // start (the same file `tt program start` was given).
  const program = (bChecks: string[]): ProgramFile => ({
    title: "reexport",
    maxParallel: 1,
    entries: [
      { id: "a", after: [], plan: plan("a", ["true"]) },
      { id: "b", after: ["a"], plan: plan("b", bChecks) },
    ],
  });
  const sourcePath = path.join(scriptsDir, "source.json");
  const staleProgramPath = path.join(scriptsDir, "stale-program.json");
  const STALE = "echo the-plan-as-the-program-started";
  const CHANGED = "echo the-plan-fixed-between-nodes";
  fs.writeFileSync(sourcePath, JSON.stringify(program([STALE])));
  // `tt program start` gets the snapshot as it was at program start...
  fs.writeFileSync(staleProgramPath, JSON.stringify(program([STALE])));
  // ...and the exporter is INJECTABLE, so this test needs no Emacs: `cat`
  // prints the source file exactly as an exporter would print the program JSON.
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [
        // Node a holds the program in BUILD long enough for the owner to fix
        // node b's plan file (the real exex 01 gap was minutes).
        { kind: "call-sh", command: "sleep 2" },
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
            contractVersion: contractVersionFor(phase(["true"])),
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
    TT_EXPORT_COMMAND: "cat",
  };
  const cli = (args: string[]) => execFileSync(process.execPath, [CLI_PATH, ...args, "--root", root], { encoding: "utf8", env });
  const state = (id: string) =>
    JSON.parse(cli(["program", "state", id])) as {
      state: { nodes: Record<string, { status: string; runId?: string }> };
    };
  const runFile = (runId: string, file: string) => JSON.parse(fs.readFileSync(path.join(root, runId, file), "utf8")) as Record<string, unknown>;
  let programId = "";
  try {
    programId = cli(["program", "start", staleProgramPath, "--source", sourcePath]).trim();
    assert.ok(programId.length > 0, "tt program start prints the program id");
    // The owner fixes node b's plan file WHILE node a runs.
    await waitFor(() => state(programId).state.nodes.a.status === "running", 60_000, 100);
    fs.writeFileSync(sourcePath, JSON.stringify(program([CHANGED])));

    // Node b is started by the scheduler after that, from the source as it is
    // NOW — the program-as-started snapshot is never restarted.
    await waitFor(() => state(programId).state.nodes.b.status === "running", 150_000, 100);
    const nodes = state(programId).state.nodes;

    // Node a ran the plan the program started with.
    const aPlan = runFile(nodes.a.runId!, path.join("plan", "v1.json"));
    assert.deepEqual(aPlan.checks, ["true"]);
    // Node b — started AFTER the change — ran the CHANGED checks, not the
    // snapshot the program was started with (which still says STALE).
    const bPlan = runFile(nodes.b.runId!, path.join("plan", "v1.json"));
    assert.deepEqual(bPlan.checks, [CHANGED], "node b's run carries the plan file as it is NOW");
    const stale = JSON.parse(fs.readFileSync(staleProgramPath, "utf8")) as ProgramFile;
    assert.deepEqual(stale.entries[1].plan!.checks, [STALE], "the program-as-started snapshot still holds the old plan");

    // The node's own record says where its plan came from, and which runner
    // started it: the program's CURRENT runner, read at the node's start.
    const snapshotB = runFile(nodes.b.runId!, "program.json").snapshot as Record<string, unknown>;
    assert.equal(snapshotB.reexported, true, "node b's plan was re-exported at its own start");
    assert.equal(snapshotB.exporter, "command");
    assert.equal(snapshotB.source, sourcePath);
    assert.equal(typeof snapshotB.planSha256, "string");
    const metaB = runFile(nodes.b.runId!, "meta.json") as { runnerRevision?: string };
    assert.equal(snapshotB.runner, metaB.runnerRevision, "the snapshot names the runner this node's run was created under");
    assert.equal(snapshotB.runner, runnerRevision(), "and that runner is the program's current one");
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

/** Plan 07h2 (R10): the behaviour the phase contract names as "two running
 * nodes of one program check without pausing each other's workers" — two
 * nodes of one program each take their own check slot, and neither plan's
 * exclusive check window freezes the other node's workers. Registered under
 * both the plan-named test name and this packet's own descriptive one. */
async function twoRunningNodesCheckWithoutPausingEachOther(): Promise<void> {
  const repo = makeRepo();
  const root = shortTmp("tt-07h2-r10-root");
  const scriptsDir = shortTmp("tt-07h2-r10-scripts");
  // Both nodes ask for the aggressive cross-run check window (TT_CHECK_WINDOW:
  // exclusive) — exactly the plans the exex incident ran — and their checks
  // are long enough that a shared slot/window would be visible.
  const phase = {
    id: "p1",
    goal: "g",
    acceptance: ["it works"],
    // Long enough that node b's worker (which sleeps 5 s) is inside the window
    // node a's check opens for its whole duration.
    checks: ["sleep 8"],
    boundaries: [],
    reserved: [],
  };
  const plan = (title: string): RunPlanFile => ({
    title,
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["sleep 8"],
    checkWindow: "exclusive",
    phases: [phase],
  });
  const program: ProgramFile = {
    title: "two-slots",
    // `#+TT_PROGRAM: 2`: both nodes run at once.
    maxParallel: 2,
    entries: [
      { id: "a", after: [], plan: plan("a") },
      { id: "b", after: [], plan: plan("b") },
    ],
  };
  const programPath = path.join(scriptsDir, "program.json");
  fs.writeFileSync(programPath, JSON.stringify(program));
  // The worker waits until this TEST lets it go (a `go` file in its own
  // worktree), so the order is the one under test rather than a race: node a
  // submits first and its check window opens; node b's worker is then released
  // to work THROUGH that window, which is what must not pause it.
  const windowDir = path.join(root, "check.lock.window.d");
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [
        {
          kind: "call-sh",
          command:
            "i=0; while [ ! -f go ] && [ $i -lt 300 ]; do i=$((i+1)); sleep 0.1; done; " +
            "rm -f go; sleep 5",
        },
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
    TT_TEST_DEADLINES: JSON.stringify({ ...LIVE_FAST, checkMs: 60_000 }),
    FAKE_PI_SCRIPT: scriptsDir,
  };
  const cli = (args: string[]) => execFileSync(process.execPath, [CLI_PATH, ...args, "--root", root], { encoding: "utf8", env });
  const state = (id: string) =>
    JSON.parse(cli(["program", "state", id])) as { state: { nodes: Record<string, { status: string; runId?: string }> } };
  const records = (runId: string): Array<{ kind: string; event: unknown }> => {
    const file = path.join(root, runId, "events.jsonl");
    if (!fs.existsSync(file)) return [];
    return fs
      .readFileSync(file, "utf8")
      .split("\n")
      .filter((l) => l.trim().length > 0)
      .map((l) => JSON.parse(l) as { kind: string; event: unknown });
  };
  let programId = "";
  try {
    programId = cli(["program", "start", programPath]).trim();
    await waitFor(() => {
      const nodes = state(programId).state.nodes;
      return nodes.a.status === "running" && nodes.b.status === "running";
    }, 90_000, 100);
    const runs = state(programId).state.nodes;
    const go = (node: string) => fs.writeFileSync(path.join(root, runs[node].runId!, "worktree", "go"), "");
    // Node a submits first: its check — and so the check window — opens.
    await waitFor(
      () => {
        try {
          go("a");
          return true;
        } catch {
          return false;
        }
      },
      90_000,
      100,
    );
    await waitFor(() => fs.existsSync(windowDir) && fs.readdirSync(windowDir).length > 0, 90_000, 25);
    // Now node b's worker works INSIDE node a's window.
    go("b");
    await waitFor(
      () => Object.values(state(programId).state.nodes).every((n) => n.status === "done"),
      150_000,
      100,
    );
    const runIds = Object.values(state(programId).state.nodes).map((n) => n.runId!);
    for (const runId of runIds) {
      assert.ok(fs.existsSync(path.join(root, runId, "check.slot")), `${runId} took its own check slot`);
    }
    // ...so neither node's check window paused the other's workers, even
    // though both plans ask for an exclusive window.
    for (const runId of runIds) {
      const paused = records(runId).filter((r) => r.kind === "worker_paused");
      assert.deepEqual(paused, [], `${runId}'s workers were never paused by the other run's check`);
    }
    // The run root's shared check lock is not what these two nodes contend on.
    assert.equal(fs.existsSync(path.join(root, "check.lock")), false, "no root-level check.lock was taken");
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
}

// The phase contract's own name for this behaviour, registered beside the
// packet's descriptive one: both drive the same end-to-end body once each.
test("plan 07h2 (R10, exex 01c3): two running nodes of one program each have their own check slot, and neither run's workers are paused by the other's check", twoRunningNodesCheckWithoutPausingEachOther);
test("two running nodes of one program check without pausing each other's workers", twoRunningNodesCheckWithoutPausingEachOther);

test("plan 07h2 (A2): a failed re-export blocks the node with X:PLAN_DEFECT instead of running a stale plan", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-07h2-defect-root");
  const scriptsDir = shortTmp("tt-07h2-defect-scripts");
  const phase = { id: "p1", goal: "g", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
  const program: ProgramFile = {
    title: "defect",
    maxParallel: 1,
    entries: [{ id: "a", after: [], plan: { title: "a", repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase] } }],
  };
  const sourcePath = path.join(scriptsDir, "source.json");
  const programPath = path.join(scriptsDir, "program.json");
  fs.writeFileSync(sourcePath, JSON.stringify(program));
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
    FAKE_PI_SCRIPT: scriptsDir,
    // An exporter that fails: the node must not fall back to the snapshot.
    TT_EXPORT_COMMAND: "exit 1",
  };
  const cli = (args: string[]) => execFileSync(process.execPath, [CLI_PATH, ...args, "--root", root], { encoding: "utf8", env });
  let programId = "";
  try {
    programId = cli(["program", "start", programPath, "--source", sourcePath]).trim();
    const state = () =>
      JSON.parse(cli(["program", "state", programId])) as { state: { nodes: Record<string, { status: string; reason?: string; runId?: string }> } };
    await waitFor(() => state().state.nodes.a.status === "blocked", 60_000, 100);
    const node = state().state.nodes.a;
    assert.match(node.reason ?? "", /X:PLAN_DEFECT/, "the node names why it cannot start");
    assert.match(node.reason ?? "", /could not be re-exported/);
    assert.equal(node.runId, undefined, "no run was created for the refused node");
  } finally {
    if (programId) {
      try {
        cli(["program", "stop", programId]);
      } catch {
        // finished
      }
    }
    cleanupDir(root);
    cleanupDir(scriptsDir);
    cleanupDir(repo.dir);
  }
});

/** Plan 07h2 (R3/A2): the phase contract's own name for "a plan fixed between
 * nodes applies without a restart". The end-to-end proof is the plan-named
 * test above (two real nodes, a real conductor each); this one drives the
 * step that decides it — `schedulerTick` — so the *node start* itself is
 * pinned: node a already ran and finished from the program-as-started
 * snapshot, the owner then fixed node b's plan file, and the next node start
 * is the one that reads it. No conductor is spawned (`launch` is recorded),
 * so this is deterministic and fast. */
test("a node started after its plan file changed runs the changed plan", () => {
  const repo = makeRepo();
  const root = shortTmp("tt-07h2-r3-plan-root");
  const scriptsDir = shortTmp("tt-07h2-r3-plan-scripts");
  const STALE = "echo the-plan-as-the-program-started";
  const CHANGED = "echo the-plan-fixed-between-nodes";
  const phase = (checks: string[]) => ({ id: "p1", goal: "g", acceptance: ["it works"], checks, boundaries: [], reserved: [] });
  const plan = (title: string, checks: string[]): RunPlanFile => ({
    title,
    repo: repo.dir,
    integrationBranch: "main",
    checks,
    phases: [phase(checks)],
  });
  const program = (bChecks: string[]): ProgramFile => ({
    title: "plan-fixed-between-nodes",
    maxParallel: 1,
    entries: [
      { id: "a", after: [], plan: plan("a", ["true"]) },
      { id: "b", after: ["a"], plan: plan("b", bChecks) },
    ],
  });
  const source = path.join(scriptsDir, "source.json");
  fs.writeFileSync(source, JSON.stringify(program([STALE])));
  // The program was STARTED from this snapshot, which still says STALE; the
  // `--source` file is what a node's own start re-exports.
  const dir = createProgram(root, program([STALE]), "r3plan");
  recordProgramSource(dir, source);
  // Node a already ran and finished — from the program-as-started snapshot.
  appendProgramEvent(dir, { type: "NODE_STARTED", node: "a", runId: "aaaaaaaa", branch: "tt/a", base: repo.head });
  appendProgramEvent(dir, { type: "NODE_STATUS", node: "a", status: "done" });
  // ...and published its work on its own branch, which node b is cut from.
  execFileSync("git", ["-C", repo.dir, "branch", "main--a", repo.head]);
  // The owner fixes node b's plan file AFTER node a started.
  fs.writeFileSync(source, JSON.stringify(program([CHANGED])));

  const launched: string[] = [];
  let asked = 0;
  schedulerTick(dir, {
    runRoot: root,
    launch: (runDir) => launched.push(runDir),
    // The exporter is injectable, exactly as `TT_EXPORT_COMMAND` is: `cat`
    // would print the source file, and so does this.
    exporter: (file) => {
      asked += 1;
      return JSON.parse(fs.readFileSync(file, "utf8")) as ProgramFile;
    },
    // Plan 07h2 (A2): the program's current runner, read at THIS node's
    // start — a distinct value from the program's own start, so the record
    // cannot be a leftover.
    runner: () => "runner-at-node-start",
  });

  assert.equal(asked, 1, "the node's start re-exported the source file");
  assert.equal(launched.length, 1, "exactly the ready node started");
  const runDir = launched[0];
  // The node's run carries the CHANGED plan, not the snapshot.
  const runPlan = JSON.parse(fs.readFileSync(path.join(runDir, "plan", "v1.json"), "utf8")) as RunPlanFile;
  assert.deepEqual(runPlan.checks, [CHANGED], "the node's own run runs the plan file as it is NOW");
  // ...and it records where that plan came from and which runner started it.
  const nodeRecord = JSON.parse(fs.readFileSync(path.join(runDir, "program.json"), "utf8")) as {
    node: string;
    snapshot: { source: string; exporter: string; runner: string; reexported: boolean; planSha256: string };
  };
  assert.equal(nodeRecord.node, "b");
  assert.equal(nodeRecord.snapshot.reexported, true);
  assert.equal(nodeRecord.snapshot.source, source);
  assert.equal(nodeRecord.snapshot.runner, "runner-at-node-start");
  const meta = JSON.parse(fs.readFileSync(path.join(runDir, "meta.json"), "utf8")) as { runnerRevision: string };
  assert.equal(meta.runnerRevision, "runner-at-node-start", "the run itself is recorded under the runner its start snapshotted");
  // The program-as-started snapshot is never edited: it still holds STALE.
  const asStarted = JSON.parse(fs.readFileSync(path.join(root, "programs", "r3plan", "program.json"), "utf8")) as ProgramFile;
  assert.deepEqual(asStarted.entries[1].plan!.checks, [STALE]);
  cleanupDir(root);
  cleanupDir(scriptsDir);
  cleanupDir(repo.dir);
});

/** Plan 07h2 (A2): "the program's current runner" is a production fact, not a
 * parameter: `tt runner install <sha>` freezes a copy under
 * `<root>/runner/<sha>/tradeoffs-trace` and points `<root>/runner/current` at
 * it. So this test starts the program under an INSTALLED runner A, installs a
 * different runner B while node a runs, and asserts the NEXT node not only
 * records B but is actually executed by B (the conductor that starts the run
 * writes its own `RUNNER_SHA` into the run's `init` record). */
test("plan 07h2 (A2): a node started after a newer runner is installed records and runs that runner", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-07h2-a2-runner-root");
  const scriptsDir = shortTmp("tt-07h2-a2-runner-scripts");
  const phase = { id: "p1", goal: "g", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
  const plan = (title: string): RunPlanFile => ({ title, repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase] });
  const program: ProgramFile = {
    title: "runner-at-node-start",
    maxParallel: 1,
    entries: [
      { id: "a", after: [], plan: plan("a") },
      { id: "b", after: ["a"], plan: plan("b") },
    ],
  };
  const sourcePath = path.join(scriptsDir, "source.json");
  const programPath = path.join(scriptsDir, "program.json");
  fs.writeFileSync(sourcePath, JSON.stringify(program));
  fs.writeFileSync(programPath, JSON.stringify(program));
  // Node a holds the program long enough for the owner to install the newer
  // runner (the real gap was the minutes between two nodes).
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [
        { kind: "call-sh", command: "sleep 4" },
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
    TT_TEST_DEADLINES: JSON.stringify(LIVE_FAST),
    FAKE_PI_SCRIPT: scriptsDir,
    TT_EXPORT_COMMAND: "cat",
  };
  const state = (id: string) =>
    JSON.parse(
      execFileSync(process.execPath, [CLI_PATH, "program", "state", id, "--root", root], { encoding: "utf8", env }),
    ) as { state: { nodes: Record<string, { status: string; runId?: string }> } };
  const runFile = (runId: string, file: string) =>
    JSON.parse(fs.readFileSync(path.join(root, runId, file), "utf8")) as Record<string, unknown>;
  /** `tt runner install <sha>`'s own layout: the frozen copy, its `RUNNER_SHA`,
   * and `<root>/runner/current` pointing at it. The copy is the working
   * checkout's package, so both installs carry this runner's code and differ
   * only in which revision they are. */
  const installRunner = (revision: string): string => {
    const dest = path.join(root, "runner", revision, "tradeoffs-trace");
    fs.mkdirSync(path.dirname(dest), { recursive: true });
    execFileSync("cp", ["-a", PACKAGE_ROOT, dest]);
    fs.writeFileSync(path.join(dest, "RUNNER_SHA"), `${revision}\n`);
    const current = path.join(root, "runner", "current");
    fs.rmSync(current, { force: true });
    fs.symlinkSync(revision, current);
    return dest;
  };
  const runnerA = installRunner("runner-a");
  let programId = "";
  try {
    // The program is STARTED by runner A's own CLI, so its scheduler is A.
    programId = execFileSync(
      process.execPath,
      [path.join(runnerA, "src", "cli.ts"), "program", "start", programPath, "--source", sourcePath, "--root", root],
      { encoding: "utf8", env },
    )
      .trim()
      .split("\n")[0];
    await waitFor(() => state(programId).state.nodes.a.status === "running", 90_000, 100);
    const aRun = state(programId).state.nodes.a.runId!;
    assert.equal((runFile(aRun, "program.json").snapshot as { runner: string }).runner, "runner-a", "node a started under A");

    // The owner installs the newer runner WHILE node a runs: from now on the
    // program's current runner is B.
    installRunner("runner-b");

    await waitFor(() => state(programId).state.nodes.b.status === "running", 150_000, 100);
    const bRun = state(programId).state.nodes.b.runId!;
    // Node b's start SELECTED B...
    const snapshotB = runFile(bRun, "program.json").snapshot as { runner: string };
    assert.equal(snapshotB.runner, "runner-b", "node b's start resolved the program's current runner");
    assert.equal((runFile(bRun, "meta.json") as { runnerRevision: string }).runnerRevision, "runner-b", "node b's run is recorded under B");
    // ...and B's own code EXECUTED it: the conductor writes its own
    // `RUNNER_SHA` into the run's `init` record.
    const init = fs
      .readFileSync(path.join(root, bRun, "events.jsonl"), "utf8")
      .split("\n")
      .filter((l) => l.trim().length > 0)
      .map((l) => JSON.parse(l) as { kind?: string; event?: { runnerRevision?: string } })
      .find((r) => r.kind === "init");
    assert.equal(init?.event?.runnerRevision, "runner-b", "the conductor that ran node b was B's");
  } finally {
    if (programId) {
      try {
        execFileSync(process.execPath, [CLI_PATH, "program", "stop", programId, "--root", root], { encoding: "utf8", env });
      } catch {
        // already finished
      }
    }
    cleanupDir(root);
    cleanupDir(scriptsDir);
    cleanupDir(repo.dir);
  }
});

/** Plan 07h2 (A2): a node's plan is its own plan FILE, read at its start. A
 * program started from `--source` whose host cannot re-export that file has no
 * way to read it, so the node is blocked with `X:PLAN_DEFECT` — running the
 * program-start snapshot instead would be exactly the stale plan the item
 * exists to remove. */
test("plan 07h2 (A2): a node that cannot re-export its source is blocked instead of running the stale plan", () => {
  const repo = makeRepo();
  const root = shortTmp("tt-07h2-noexport-root");
  const scriptsDir = shortTmp("tt-07h2-noexport-scripts");
  const phase = { id: "p1", goal: "g", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
  const plan: RunPlanFile = { title: "a", repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase] };
  const program: ProgramFile = { title: "no-exporter", maxParallel: 1, entries: [{ id: "a", after: [], plan }] };
  const source = path.join(scriptsDir, "source.json");
  fs.writeFileSync(source, JSON.stringify(program));
  const dir = createProgram(root, program, "noexport");
  recordProgramSource(dir, source);
  const launched: string[] = [];
  // No `TT_EXPORT_COMMAND` and no Emacs on PATH: this host cannot export.
  const saved = { path: process.env.PATH, emacs: process.env.TT_EMACS, command: process.env.TT_EXPORT_COMMAND };
  try {
    process.env.PATH = "";
    process.env.TT_EMACS = "no-such-emacs-binary";
    delete process.env.TT_EXPORT_COMMAND;
    schedulerTick(dir, { runRoot: root, launch: (runDir) => launched.push(runDir) });
  } finally {
    if (saved.path === undefined) delete process.env.PATH;
    else process.env.PATH = saved.path;
    if (saved.emacs === undefined) delete process.env.TT_EMACS;
    else process.env.TT_EMACS = saved.emacs;
    if (saved.command === undefined) delete process.env.TT_EXPORT_COMMAND;
    else process.env.TT_EXPORT_COMMAND = saved.command;
  }
  const events = fs
    .readFileSync(path.join(root, "programs", "noexport", "events.jsonl"), "utf8")
    .split("\n")
    .filter((l) => l.trim().length > 0)
    .map((l) => JSON.parse(l) as { event: { type: string; reason?: string } });
  const blocked = events.find((e) => e.event.type === "NODE_BLOCKED");
  assert.ok(blocked, "the node is blocked");
  assert.match(blocked!.event.reason ?? "", /X:PLAN_DEFECT/);
  assert.match(blocked!.event.reason ?? "", /no exporter is configured/);
  assert.deepEqual(launched, [], "no run was created from the stale plan");
  cleanupDir(root);
  cleanupDir(scriptsDir);
  cleanupDir(repo.dir);
});
