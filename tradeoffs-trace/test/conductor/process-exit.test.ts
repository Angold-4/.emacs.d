// Round-of-review item 1's required exit-gate test: "spawns a conductor
// process for a fake-pi happy path and asserts the process exits by itself
// within a bound after DONE." This is deliberately a real OS process (not
// an in-process Conductor, which the other conductor tests already cover),
// because the bug this guards against — a leftover setTimeout/handle
// keeping the event loop alive — is invisible to an in-process assertion
// like `await conductor.stop()`; it only shows up as "the process never
// exits on its own". See src/conductor.ts's `#maybeAutoStop` and
// src/effects/pi-rpc.ts's memoized `terminate()` for the fixes this guards.
//
// The child script below builds its own Conductor (the same way
// harness.setupConductor does) and calls `start()` but deliberately never
// calls `stop()` itself — the only thing that can make the child process
// exit on its own is the conductor's own DONE/BLOCKED auto-stop.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir } from "./harness.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CONDUCTOR_PATH = fileURLToPath(new URL("../../src/conductor.ts", import.meta.url));
const ROLES_PATH = fileURLToPath(new URL("../../src/core/roles.ts", import.meta.url));

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

test("a spawned conductor process for a fake-pi happy path exits by itself shortly after reaching DONE", async () => {
  const repoDir = shortTmp("tt-repo");
  git(["init", "-q", "-b", "main"], repoDir);
  fs.writeFileSync(path.join(repoDir, "README.md"), "base\n");
  git(["add", "-A"], repoDir);
  git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repoDir);

  const runRoot = shortTmp("tt-run");
  const scriptsDir = shortTmp("tt-scripts");
  const childScript = path.join(scriptsDir, "child.ts");

  // Reviewer scripts must embed the live candidate sha, which only exists
  // once the conductor freezes it — so, like harness.ts's own
  // `reviewerScriptFor`, the child process writes each reviewer's script
  // lazily, from inside `piEnvFor`, using the conductor's own current state.
  fs.writeFileSync(
    childScript,
    `
    import { Conductor, createRun } from ${JSON.stringify(CONDUCTOR_PATH)};
    import { ROLE_TOOLS } from ${JSON.stringify(ROLES_PATH)};
    import * as fs from "node:fs";
    import * as path from "node:path";

    const repo = ${JSON.stringify(repoDir)};
    const scriptsDir = ${JSON.stringify(scriptsDir)};
    const fakePiPath = ${JSON.stringify(FAKE_PI_PATH)};

    const plan = {
      title: "process-exit test plan",
      repo,
      integrationBranch: "main",
      checks: ["true"],
      phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
    };

    function writeScript(name, script) {
      const file = path.join(scriptsDir, name + ".json");
      fs.writeFileSync(file, JSON.stringify(script));
      return file;
    }

    const workerScriptPath = writeScript("worker", {
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    });

    const runDir = createRun(${JSON.stringify(runRoot)}, plan);
    const reviewerScriptPaths = new Map();
    const conductor = new Conductor({
      runDir,
      plan,
      piCommand: process.execPath,
      piArgsPrefix: [fakePiPath],
      stubReviews: true,
      piEnvFor: (role, agentId) => {
        if (role === "worker") return { FAKE_PI_SCRIPT: workerScriptPath };
        const reviewer = (agentId.match(/^reviewer-([MAB])-/)?.[1]) ?? "M";
        if (!reviewerScriptPaths.has(agentId)) {
          const state = conductor.state;
          const script = {
            hello: { role: "reviewer", tools: ROLE_TOOLS.reviewer },
            steps: [
              {
                kind: "call-submit",
                tool: "submit_review",
                args: {
                  reviewer,
                  phaseId: state.phase.phaseId,
                  candidateSha: state.phase.candidate?.sha,
                  contractVersion: state.phase.contract.contractVersion,
                  correctionStatements: [],
                  findingStatements: [],
                },
              },
            ],
          };
          reviewerScriptPaths.set(agentId, writeScript(agentId, script));
        }
        return { FAKE_PI_SCRIPT: reviewerScriptPaths.get(agentId) };
      },
      deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5000, reviewMs: 10000 },
    });

    // Deliberately no stop() call here and no process.exit() call anywhere
    // in this script: the only thing that can end this process is the
    // conductor's own DONE auto-stop (src/conductor.ts's #maybeAutoStop).
    await conductor.start();
    `,
  );

  const start = Date.now();
  const child = spawn(process.execPath, [childScript], { stdio: ["ignore", "pipe", "pipe"] });
  let stderr = "";
  child.stderr.on("data", (c) => (stderr += c.toString()));
  const exit = await new Promise<{ code: number | null; signal: NodeJS.Signals | null }>((resolve) => {
    child.once("exit", (code, signal) => resolve({ code, signal }));
  });
  const elapsedMs = Date.now() - start;

  assert.equal(exit.signal, null, `child process was killed by a signal; stderr:\n${stderr}`);
  assert.equal(exit.code, 0, `child process exited non-zero; stderr:\n${stderr}`);
  // Generous bound: the happy path itself normally finishes in a few
  // seconds (see happy-path.test.ts); this only guards against the
  // "never exits" failure mode, not tight timing.
  assert.ok(elapsedMs < 30_000, `child process took ${elapsedMs}ms to exit on its own — expected well under 30s`);

  cleanupDir(runRoot);
  cleanupDir(scriptsDir);
  cleanupDir(repoDir);
});
