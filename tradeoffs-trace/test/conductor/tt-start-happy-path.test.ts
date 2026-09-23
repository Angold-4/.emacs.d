// `tt-start-happy-path` (phase 1b work-packet item 6): `bin/tt start
// <plan.json> --root /tmp/tt-…` returns a run id immediately, the detached
// daemon it launches reaches DONE with the integration branch moved by CAS,
// the daemon process exits on its own, and `bin/tt status <run>` shows DONE
// with the candidate and I.
//
// Uses the documented test-only injection point (`TT_TEST_MODE=1` +
// `TT_TEST_PI_COMMAND`/`TT_TEST_PI_ARGS_PREFIX`, src/cli.ts's
// `testPiInjection`) so the CLI-launched, detached conductor runs fake-pi
// instead of a real Pi — refused unless `TT_TEST_MODE=1` is set, and never
// the default. Since a `tt start`-launched conductor has no `piEnvFor` hook,
// the worker and every reviewer share one `FAKE_PI_SCRIPT` *directory*
// (fake-pi.ts's own item-6 addition: `<dir>/<TT_ROLE>.json`), and the
// reviewer script reads which of M/A/B it is, and the live candidate sha,
// from `$TT_REVIEWER`/`$TT_CANDIDATE_SHA` env-var tokens (fake-pi.ts's other
// item-6 addition) rather than being static.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, makeRepo, sleep, waitFor } from "./harness.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { contractVersionFor, type RunPlanFile } from "../../src/conductor.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function runCli(args: string[], env: NodeJS.ProcessEnv): Promise<{ code: number | null; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, ...env } });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ code, stdout, stderr }));
  });
}

test("tt-start-happy-path: `tt start` returns a run id immediately, the detached daemon reaches DONE and exits on its own, and `tt status` shows it", async () => {
  const repo = makeRepo();
  const root = shortTmp("tt-cli-root");
  const scriptsDir = shortTmp("tt-cli-scripts");

  const plan: RunPlanFile = {
    title: "tt-start-happy-path",
    repo: repo.dir,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
  };
  const planPath = path.join(scriptsDir, "plan.json");
  fs.writeFileSync(planPath, JSON.stringify(plan));

  // One shared FAKE_PI_SCRIPT *directory*: fake-pi.ts picks <dir>/<TT_ROLE>.json.
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
            // Computed exactly the way the conductor itself derives it
            // (contractVersionFor(phase)), since a static script can't ask
            // the conductor for it the way a real reviewer would be told.
            contractVersion: contractVersionFor(plan.phases[0]),
            correctionStatements: [],
            findingStatements: [],
          },
        },
      ],
    }),
  );

  const testEnv: NodeJS.ProcessEnv = {
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    TT_TEST_STUB_REVIEWS: "1",
    FAKE_PI_SCRIPT: scriptsDir,
  };

  let runId = "";
  let daemonDir = "";
  try {
    const start = await runCli(["start", planPath, "--root", root], testEnv);
    assert.equal(start.code, 0, `tt start failed: ${start.stderr}`);
    runId = start.stdout.trim();
    assert.ok(runId.length > 0, "expected tt start to print a run id");
    daemonDir = path.join(root, runId);

    // The daemon process (spawned detached by `tt start`, argv names its
    // own run directory) is actually running.
    await waitFor(() => {
      try {
        execFileSync("pgrep", ["-f", daemonDir], { encoding: "utf8" });
        return true;
      } catch {
        return false;
      }
    }, 10_000);

    // Poll `tt status` (a separate, short-lived process each time — the
    // real CLI contract) until it reports DONE. `tt start` uses the
    // conductor's full production DEFAULT_DEADLINES (no CLI knob to
    // shorten them in this packet), so this genuinely takes over 30s: the
    // freeze's own quiesce step waits up to the default `abortGraceMs`
    // (30s) for fake-pi to exit on its own after `abort` — fake-pi (like
    // real Pi) does not exit just because it was asked to abort, so this
    // always spends that full 30s before escalating to SIGTERM.
    await waitFor(() => {
      const status = execFileSync(process.execPath, [CLI_PATH, "status", runId, "--root", root], { encoding: "utf8" });
      return /phase: p1 — DONE/.test(status);
    }, 90_000, 500);

    const finalStatus = execFileSync(process.execPath, [CLI_PATH, "status", runId, "--root", root], { encoding: "utf8" });
    assert.match(finalStatus, /run status: RUN_ACTIVE/);
    assert.match(finalStatus, /phase: p1 — DONE/);
    assert.match(finalStatus, /candidate: [0-9a-f]{7,40}/);
    assert.match(finalStatus, /published: [0-9a-f]{7,40}/);

    // The integration branch actually moved by CAS.
    const branchHead = execFileSync("git", ["-C", repo.dir, "rev-parse", "main"], { encoding: "utf8" }).trim();
    assert.notEqual(branchHead, repo.head, "the integration branch must have moved");

    // The daemon process exits on its own shortly after DONE — the literal
    // "the test process hangs" regression check, at the `tt start` level
    // rather than the in-process `process-exit.test.ts` one.
    await waitFor(() => {
      try {
        execFileSync("pgrep", ["-f", daemonDir], { encoding: "utf8" });
        return false;
      } catch {
        return true;
      }
    }, 30_000);
  } finally {
    // Best-effort: if anything above threw before the daemon exited on its
    // own, make sure it does not linger past this test.
    try {
      if (daemonDir) execFileSync("pkill", ["-9", "-f", daemonDir]);
    } catch {
      // already gone, or pkill found nothing — either is fine.
    }
    await sleep(50);
    cleanupDir(root);
    cleanupDir(scriptsDir);
  }
});
