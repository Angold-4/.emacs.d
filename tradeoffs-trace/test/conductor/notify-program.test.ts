// Plan 01b: program-level notifications (design D4). A node entering
// needs-you is announced by that run's own conductor; a program ending done
// or stuck is announced by the scheduler, which is what observes it. This
// exercises the detach path end to end: `tt program start` → the scheduler
// launches the node's conductor → the conductor writes the run record and
// the scheduler writes the program record, both to
// `<root>/notifications.jsonl`, with TT_NOTIFY_COMMAND run once each.

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
import { readNotifications } from "../../src/notify.ts";
import type { ProgramFile } from "../../src/core/program.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function countLines(file: string): number {
  try {
    return fs.readFileSync(file, "utf8").trim().split("\n").filter(Boolean).length;
  } catch {
    return 0;
  }
}

test("notify-program: a node that needs you, then the program ending done, each write one notification", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-notify-prog-root");
  const scriptsDir = shortTmp("tt-notify-prog-scripts");
  const markerDir = shortTmp("tt-notify-prog-marker");
  const marker = path.join(markerDir, "checks-pass");
  const calls = path.join(markerDir, "calls.log");

  const phase = {
    id: "p1",
    goal: "add one file",
    acceptance: ["it works"],
    checks: [`test -f ${marker}`],
    boundaries: [],
    reserved: [],
  };
  const plan: RunPlanFile = {
    title: "node a",
    repo: repo.dir,
    integrationBranch: "main",
    checks: [`test -f ${marker}`],
    phases: [phase],
  };
  const program: ProgramFile = { title: "notify program", maxParallel: 1, entries: [{ id: "a", after: [], plan }] };
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
    FAKE_PI_SCRIPT: scriptsDir,
    TT_NOTIFY_COMMAND: `printf 'notify\\n' >> ${calls}`,
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
  const cli = (args: string[]) => execFileSync(process.execPath, [CLI_PATH, ...args, "--root", root], { encoding: "utf8", env });
  const state = (id: string) =>
    JSON.parse(cli(["program", "state", id])) as { state: { nodes: Record<string, { status: string; runId?: string }> } };

  let programId = "";
  try {
    programId = cli(["program", "start", programPath]).trim();
    assert.ok(programId.length > 0);

    // 1) The node exhausts its repair budget: its own conductor announces it.
    await waitFor(() => state(programId).state.nodes.a.status === "needs-you", 150_000, 200);
    const runId = state(programId).state.nodes.a.runId!;
    const runDir = path.join(root, runId);
    await waitFor(() => readNotifications(root).filter((r) => r.kind === "run").length === 1, 30_000, 200);
    const runNote = readNotifications(root).find((r) => r.kind === "run")!;
    assert.equal(runNote.id, runId);
    assert.equal(runNote.node, "a", "the record names the program node");
    assert.match(runNote.reason, /checks kept failing|repair budget/);

    // 2) Resolve the wait with three more rounds and let the checks pass.
    fs.writeFileSync(marker, "ok\n");
    const rs = JSON.parse(cli(["state", runDir])) as {
      state: {
        phase: {
          runId: string;
          candidate: { sha: string };
          contract: { contractVersion: unknown };
          ownerRequests: Array<{ id: string; version: number; status: string }>;
        };
      };
    };
    const request = rs.state.phase.ownerRequests.find((r) => r.status === "open")!;
    assert.ok(request, "expected an open owner request");
    fs.writeFileSync(
      path.join(runDir, "inbox", "cmd-resolve-grant.json"),
      JSON.stringify({
        commandId: "cmd-resolve-grant",
        type: "resolve",
        recordKind: "request",
        option: "grant",
        binding: {
          runId: rs.state.phase.runId,
          phaseId: "p1",
          candidateSha: rs.state.phase.candidate.sha,
          contractVersion: rs.state.phase.contract.contractVersion,
          recordId: request.id,
          recordVersion: request.version,
        },
      }),
    );

    await waitFor(() => state(programId).state.nodes.a.status === "done", 150_000, 200);
    await waitFor(() => readNotifications(root).some((r) => r.kind === "program"), 60_000, 200);
    const programNote = readNotifications(root).find((r) => r.kind === "program")!;
    assert.equal(programNote.id, programId);
    assert.match(programNote.reason, /program done/);

    assert.equal(readNotifications(root).filter((r) => r.kind === "run").length, 1, "the owner wait is announced exactly once");
    assert.equal(readNotifications(root).filter((r) => r.kind === "program").length, 1, "the program done is announced exactly once");
    await waitFor(
      () => fs.readFileSync(path.join(root, "programs", programId, "scheduler.log"), "utf8").includes("program done"),
      20_000,
    );
    assert.equal(countLines(calls), 2, "one notifier run per notification");
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
    cleanupDir(markerDir);
    cleanupDir(repo.dir);
  }
});
