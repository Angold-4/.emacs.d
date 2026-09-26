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

import { Conductor, createRun, runPaths, type ConductorOptions, type RunPlanFile } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { readLog, type LogRecord } from "../../src/effects/log.ts";

export const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));

function shortTmp(prefix: string): string {
  // `mkdtemp` guarantees the name is NEW (it retries on collision). A plain
  // random name written into with `mkdirSync(..., {recursive: true})` silently
  // reuses an existing directory when the name collides — and thousands of
  // `tt-*` directories accumulate in /tmp from earlier runs, so that is a real
  // draw. A reused repo directory is a confusing failure: the fresh
  // `git init` sees the old `base` commit, the identical README changes
  // nothing, and `git commit` fails with "nothing to commit, working tree
  // clean" (observed failing the phase's own `make check`).
  return fs.mkdtempSync(path.join("/tmp", `${prefix}-`));
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
  // Names the real cause if the commit above did not happen: `rev-parse HEAD`
  // fails with "Needed a single revision" instead of leaving a test to fail
  // later for reasons that look unrelated.
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
  /** Shorthand: sets both the global and phase check lists to this value
   * (the harness's historical behavior). Use `globalChecks`/`phaseChecks`
   * for a run whose two lists differ. */
  checks?: string[];
  /** The plan's global `TT_CHECKS` list. Defaults to `checks`, then
   * `["true"]`. */
  globalChecks?: string[];
  /** The phase contract's own `:CHECKS:` list. Defaults to `checks`, then
   * `["true"]`. */
  phaseChecks?: string[];
  workerScript: (setup: { repo: TestRepo }) => { hello?: unknown; steps: FakePiStep[] };
  /** Work packet 2a: an attempt-aware alternative to the static
   * `workerScript` — needed by tests where a repair round's second attempt
   * must submit different (or empty) decisions than the first (e.g.
   * contract-objection.test.ts, so the repair does not re-disclose a
   * duplicate decision). Takes precedence over `workerScript` when given. */
  workerScriptForAttempt?: (attempt: number, setup: { repo: TestRepo }) => { hello?: unknown; steps: FakePiStep[] };
  reviewerScriptFor?: (reviewer: Reviewer, state: State) => { hello?: unknown; steps: FakePiStep[] };
  deadlines?: ConductorOptions["deadlines"];
  /** Extra argv tokens prepended before fake-pi.ts's own path — fake-pi
   * never parses argv, so these are inert except as a unique, greppable
   * marker in every agent process's command line (`ps`/`pgrep -f`), e.g.
   * for a test that must assert no orphan process remains afterwards. */
  extraPiArgsPrefix?: string[];
  /** Work packet 2a: `Conductor`'s own default is real reviewers
   * (`stubReviews: false`); this harness defaults to `true` instead, so the
   * ~30 pre-existing conductor tests (whose reviewer scripts predate the
   * real two-turn discovery/review protocol) keep working unchanged. Tests
   * exercising the real protocol pass `stubReviews: false` explicitly. */
  stubReviews?: boolean;
  /** Plan 2c: pass `false` to make the integration probe rerun the checks
   * even when the probed tree equals the candidate's. */
  probeReuse?: boolean;
  /** Phase 2b: extra env vars merged into every fake-pi worker's
   * environment (e.g. `FAKE_PI_PROMPT_LOG`, to capture the prompt text the
   * conductor actually sent). */
  extraWorkerEnv?: NodeJS.ProcessEnv;
  /** Plan 2c: extra env vars per reviewer (e.g. a per-reviewer
   * `FAKE_PI_PROMPT_LOG`). */
  extraReviewerEnv?: (reviewer: Reviewer) => NodeJS.ProcessEnv | undefined;
  /** Work packet 2a: BOUNDARIES globs for the phase's contract, so a test
   * can exercise conductor-computed boundary triggers (design §3.3).
   * Defaults to `[]` (no boundaries), exactly as before this option
   * existed. */
  boundaries?: string[];
  /** Work packet 2a: path-shaped acceptance criteria, exercising the
   * "acceptance files" boundary-trigger input. Appended after the fixed
   * `"it works"` acceptance criterion. */
  acceptanceFiles?: string[];
  /** Plan 01a: the plan's `#+TT_SECRETS` names, as Emacs would put them in
   * the JSON plan. The values are read from the harness process's own
   * environment (set them with `process.env.NAME = …` before calling). */
  secrets?: string[];
  /** The phase's own goal text (default "do the thing"). A plan's prose is a
   * secret-value carrier too, so a test can quote one in it. */
  goal?: string;
  /** The plan's title (default "test plan") — plan prose like any other. */
  title?: string;
}): Promise<TestConductorSetup> {
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const scriptsDir = shortTmp("tt-scripts");

  const plan: RunPlanFile = {
    title: opts.title ?? "test plan",
    repo: repo.dir,
    integrationBranch: "main",
    checks: opts.globalChecks ?? opts.checks ?? ["true"],
    ...(opts.secrets ? { secrets: opts.secrets } : {}),
    phases: [
      {
        id: "p1",
        goal: opts.goal ?? "do the thing",
        acceptance: ["it works", ...(opts.acceptanceFiles ?? [])],
        checks: opts.phaseChecks ?? opts.checks ?? ["true"],
        boundaries: opts.boundaries ?? [],
        reserved: [],
      },
    ],
  };

  const runDir = createRun(runRoot, plan);
  const workerScriptPath = opts.workerScriptForAttempt ? undefined : writeScript(scriptsDir, "worker", opts.workerScript({ repo }));
  const workerScriptPaths = new Map<number, string>();

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
    stubReviews: opts.stubReviews ?? true,
    probeReuse: opts.probeReuse,
    piEnvFor: (role, agentId) => {
      if (role === "worker") {
        if (opts.workerScriptForAttempt) {
          const attempt = Number(agentId.match(/^worker-(\d+)-/)?.[1] ?? "1");
          if (!workerScriptPaths.has(attempt)) {
            workerScriptPaths.set(attempt, writeScript(scriptsDir, `worker-${attempt}`, opts.workerScriptForAttempt(attempt, { repo })));
          }
          return { FAKE_PI_SCRIPT: workerScriptPaths.get(attempt)!, ...(opts.extraWorkerEnv ?? {}) };
        }
        return { FAKE_PI_SCRIPT: workerScriptPath!, ...(opts.extraWorkerEnv ?? {}) };
      }
      const reviewer = (agentId.match(/^reviewer-([MAB])-/)?.[1] ?? "M") as Reviewer;
      if (!reviewerScriptPaths.has(agentId) && opts.reviewerScriptFor) {
        const script = opts.reviewerScriptFor(reviewer, conductor.state);
        reviewerScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
      }
      const p = reviewerScriptPaths.get(agentId);
      return p ? { FAKE_PI_SCRIPT: p, ...(opts.extraReviewerEnv?.(reviewer) ?? {}) } : undefined;
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

/** Dumps the tail of `<runDir>/events.jsonl` plus each agent's last few raw
 * RPC stream lines (`<runDir>/stream/*.jsonl`) to stderr — for a
 * `waitFor` timeout on a real (non-fake) reviewer flow, where the failure
 * otherwise gives no clue which side (conductor vs. the scripted agent)
 * stopped responding. */
function dumpDebugState(runDir: string): void {
  try {
    const eventsPath = runPaths(runDir).events;
    if (fs.existsSync(eventsPath)) {
      const tail = fs.readFileSync(eventsPath, "utf8").split("\n").filter(Boolean).slice(-30);
      console.error(`--- waitFor timeout: tail of ${eventsPath} ---`);
      for (const line of tail) console.error(line);
    }
    const streamDir = runPaths(runDir).stream;
    if (fs.existsSync(streamDir)) {
      for (const file of fs.readdirSync(streamDir)) {
        const lines = fs.readFileSync(path.join(streamDir, file), "utf8").split("\n").filter(Boolean);
        console.error(`--- waitFor timeout: last events of ${file} (${lines.length} total) ---`);
        for (const line of lines.slice(-8)) console.error(line);
      }
    }
  } catch (err) {
    console.error(`waitFor timeout debug dump failed: ${String((err as Error)?.message ?? err)}`);
  }
}

/** Plan 2d: the minimum budget every `waitFor` gets. `node --test` runs
 * test files in parallel, so under a full `make check` the host can be
 * several times slower than a single-file run; a correct run whose
 * transition takes 40 s instead of 3 s must not be reported as a failure
 * just because the host was busy. A genuinely stuck run still fails, just
 * after at least this many milliseconds. */
export const WAIT_FOR_FLOOR_MS = 90_000;

/** Polls `check()` until it returns true or its (load-tolerant) budget
 * elapses. `debugRunDir` (work packet 2a addition), if given, is dumped via
 * `dumpDebugState` on timeout — see its own doc comment. `timeoutMs` is a
 * floor; see `WAIT_FOR_FLOOR_MS`. */
export async function waitFor(check: () => boolean, timeoutMs = 15_000, intervalMs = 50, debugRunDir?: string): Promise<void> {
  const budget = Math.max(timeoutMs, WAIT_FOR_FLOOR_MS);
  const start = Date.now();
  while (!check()) {
    if (Date.now() - start > budget) {
      if (debugRunDir) dumpDebugState(debugRunDir);
      throw new Error(`waitFor: timed out after ${budget}ms`);
    }
    await sleep(intervalMs);
  }
}
