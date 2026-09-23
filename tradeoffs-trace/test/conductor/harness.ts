// Shared test harness for conductor tests: a disposable git repo, a short
// run root (macOS's 104-byte unix-socket path limit rules out the default
// `os.tmpdir()` on this machine — see socket.ts's module comment — so every
// helper here roots temp dirs at `/tmp/tt-*` explicitly, never
// `os.tmpdir()`), and small utilities for driving fake-pi through the
// conductor.

import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";

import { Conductor, createRun, runPaths, type ConductorOptions, type RunPlanFile } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { readLog, type LogRecord } from "../../src/effects/log.ts";

export const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

export interface TestRepo {
  dir: string;
  head: string;
}

/** A fresh git repo with one commit on `main`, under a short `/tmp` path. */
export function makeRepo(): TestRepo {
  const dir = shortTmp("tt-repo");
  git(["init", "-q", "-b", "main"], dir);
  fs.writeFileSync(path.join(dir, "README.md"), "base\n");
  git(["add", "-A"], dir);
  git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], dir);
  const head = git(["rev-parse", "HEAD"], dir);
  return { dir, head };
}

export function makeRunRoot(): string {
  return shortTmp("tt-run");
}

export function cleanupDir(dir: string): void {
  try {
    // Materialized candidates are read-only; make everything writable
    // first so rm can remove it.
    execFileSync("chmod", ["-R", "u+w", dir]);
  } catch {
    // best effort
  }
  fs.rmSync(dir, { recursive: true, force: true });
}

export interface FakePiStep {
  kind: string;
  [key: string]: unknown;
}

export function writeScript(dir: string, name: string, script: { hello?: unknown; steps: FakePiStep[] }): string {
  const file = path.join(dir, `${name}.json`);
  fs.writeFileSync(file, JSON.stringify(script));
  return file;
}

export function defaultWorkerHello() {
  return { role: "worker" as const, tools: ROLE_TOOLS.worker };
}

export function defaultReviewerHello() {
  return { role: "reviewer" as const, tools: ROLE_TOOLS.reviewer };
}

export interface TestConductorSetup {
  repo: TestRepo;
  runRoot: string;
  scriptsDir: string;
  plan: RunPlanFile;
  runDir: string;
  conductor: Conductor;
}

/** Starts a Conductor wired to fake-pi, with a worker script written up
 * front (`workerScript`) and reviewer scripts generated lazily via
 * `reviewerScriptFor` (called once per reviewer, at the moment the
 * conductor is about to spawn it, so the script can embed the *live*
 * candidate sha the conductor just froze — a real reviewer is simply told
 * this by the conductor; fake-pi has to be handed it via its script file
 * instead). */
export async function setupConductor(opts: {
  checks?: string[];
  workerScript: (setup: { repo: TestRepo }) => { hello?: unknown; steps: FakePiStep[] };
  reviewerScriptFor?: (reviewer: Reviewer, state: State) => { hello?: unknown; steps: FakePiStep[] };
  deadlines?: ConductorOptions["deadlines"];
  /** Extra argv tokens prepended before fake-pi.ts's own path — fake-pi
   * never parses argv, so these are inert except as a unique, greppable
   * marker in every agent process's command line (`ps`/`pgrep -f`), e.g.
   * for a test that must assert no orphan process remains afterwards. */
  extraPiArgsPrefix?: string[];
}): Promise<TestConductorSetup> {
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const scriptsDir = shortTmp("tt-scripts");

  const plan: RunPlanFile = {
    title: "test plan",
    repo: repo.dir,
    integrationBranch: "main",
    checks: opts.checks ?? ["true"],
    phases: [
      {
        id: "p1",
        goal: "do the thing",
        acceptance: ["it works"],
        checks: opts.checks ?? ["true"],
        boundaries: [],
        reserved: [],
      },
    ],
  };

  const runDir = createRun(runRoot, plan);
  const workerScriptPath = writeScript(scriptsDir, "worker", opts.workerScript({ repo }));

  const reviewerScriptPaths = new Map<string, string>();

  const conductor = new Conductor({
    runDir,
    plan,
    piCommand: process.execPath,
    // FAKE_PI_PATH must be the first token (it's the script `node` runs);
    // any extra marker tokens go after it, as fake-pi's own (ignored)
    // argv — before it, `node` would try to parse them as its own CLI
    // flags and refuse to start.
    piArgsPrefix: [FAKE_PI_PATH, ...(opts.extraPiArgsPrefix ?? [])],
    deadlines: opts.deadlines,
    piEnvFor: (role, agentId) => {
      if (role === "worker") return { FAKE_PI_SCRIPT: workerScriptPath };
      const reviewer = (agentId.match(/^reviewer-([MAB])-/)?.[1] ?? "M") as Reviewer;
      if (!reviewerScriptPaths.has(agentId) && opts.reviewerScriptFor) {
        const script = opts.reviewerScriptFor(reviewer, conductor.state);
        reviewerScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
      }
      const p = reviewerScriptPaths.get(agentId);
      return p ? { FAKE_PI_SCRIPT: p } : undefined;
    },
  });

  return { repo, runRoot, scriptsDir, plan, runDir, conductor };
}

export function readEvents(runDir: string): LogRecord[] {
  return readLog(runPaths(runDir).events).records;
}

export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/** Polls `check()` until it returns true or `timeoutMs` elapses. */
export async function waitFor(check: () => boolean, timeoutMs = 15_000, intervalMs = 50): Promise<void> {
  const start = Date.now();
  while (!check()) {
    if (Date.now() - start > timeoutMs) throw new Error("waitFor: timed out");
    await sleep(intervalMs);
  }
}
