// `tt start <plan.json> [--root <dir>]` and `tt status <run-dir-or-id>`
// (design §1.2, phase-1 plan: "a text rendering of the log is enough for
// this phase").
//
// `tt start` creates the run directory (design §9.1) and launches the
// conductor **detached**: its own session, stdio redirected to
// `<run>/conductor.log`, so the parent (this CLI invocation) returns the
// run id immediately rather than blocking for the whole run.

import { execFileSync, spawn } from "node:child_process";
import { readFileSync, existsSync, openSync, mkdirSync, writeFileSync, rmSync, symlinkSync } from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

import { decisionStatus } from "./core/predicate.ts";
import { Conductor, createRun, rebuildState, runPaths, type Deadlines, type RunPlanFile } from "./conductor.ts";

const DEFAULT_ROOT = path.join(os.homedir(), ".tradeoffs-trace");

function usage(): never {
  process.stderr.write(
    "usage: tt start <plan.json> [--root <dir>]\n       tt status <run-dir-or-id> [--root <dir>]\n       tt state <run-dir-or-id> [--root <dir>]   (JSON)\n       tt runner install <sha> [--root <dir>]\n       tt resume <run-dir-or-id> [--root <dir>]\n",
  );
  process.exit(2);
}

function parseArgs(argv: string[]): { positional: string[]; root?: string } {
  const positional: string[] = [];
  let root: string | undefined;
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--root") {
      root = argv[++i];
    } else {
      positional.push(argv[i]);
    }
  }
  return { positional, root };
}

function resolveRunDir(rootOrId: string, root: string): string {
  if (existsSync(path.join(rootOrId, "events.jsonl")) || existsSync(rootOrId)) {
    // Looks like a path already (absolute or relative run dir).
    if (existsSync(path.join(rootOrId, "meta.json"))) return path.resolve(rootOrId);
  }
  const candidate = path.join(root, rootOrId);
  if (existsSync(path.join(candidate, "meta.json"))) return candidate;
  return path.resolve(rootOrId);
}

const RUN_CONDUCTOR_FLAG = "__run-conductor";

async function cmdStart(planPath: string, root: string): Promise<void> {
  const plan = JSON.parse(readFileSync(planPath, "utf8")) as RunPlanFile;
  if (!plan.repo) usage();
  launchDetached(createRun(root, plan));
}

/** Relaunch the conductor for an existing run (after a crash or reboot);
 * the conductor rebuilds state from the log and reconciles pending intents. */
function launchDetached(runDir: string): void {
  const p = runPaths(runDir);
  const thisScript = fileURLToPath(import.meta.url);
  const out = openSync(p.log, "a");
  const err = openSync(p.log, "a");
  const child = spawn(process.execPath, [thisScript, RUN_CONDUCTOR_FLAG, runDir], {
    detached: true,
    stdio: ["ignore", out, err],
  });
  child.unref();
  process.stdout.write(`${path.basename(runDir)}\n`);
}

/** Phase 1b work-packet item 6: a documented, test-only way for a `tt
 * start`-launched (detached, out-of-process) conductor to use fake-pi
 * instead of the real `pi` binary — the detached process has no in-process
 * JS hook (unlike `Conductor` being constructed directly, the way every
 * other test in this packet drives it) to inject that itself. Refused
 * unless `TT_TEST_MODE=1` is set (never the default, and never on by
 * accident): with it unset, or set to anything else, this returns `{}` and
 * `runConductorProcess` uses the real `pi` binary exactly as before.
 * `TT_TEST_PI_COMMAND` is the command to run in place of `pi` (tests use
 * `process.execPath`, i.e. `node`); `TT_TEST_PI_ARGS_PREFIX` is a JSON array
 * of argv tokens prepended before `Conductor`'s own `launchArgs(...)` — e.g.
 * `["/path/to/fake-pi.ts"]`, since fake-pi ignores that trailing argv
 * entirely and only needs `FAKE_PI_SCRIPT` (see `piEnvFor`, not available
 * here — a `tt start` caller instead sets one shared `FAKE_PI_SCRIPT` for
 * every agent, or a `FAKE_PI_SCRIPT_DIR` a real extension/test setup could
 * key per-agent-id itself; this packet's own exit-gate test uses a single
 * shared script since its worker and reviewers all just submit
 * immediately). Both env vars are inherited by this detached process from
 * whatever set `TT_TEST_MODE=1` in the first place (`cmdStart`'s spawn does
 * not override `env`, so the parent CLI invocation's environment passes
 * straight through), not read from a CLI flag — never something a plan
 * file or a stray shell alias could turn on by accident. */
function testPiInjection(): { piCommand?: string; piArgsPrefix?: string[] } {
  if (process.env.TT_TEST_MODE !== "1") return {};
  const piCommand = process.env.TT_TEST_PI_COMMAND;
  const prefixRaw = process.env.TT_TEST_PI_ARGS_PREFIX;
  let piArgsPrefix: string[] | undefined;
  if (prefixRaw) {
    try {
      const parsed: unknown = JSON.parse(prefixRaw);
      if (Array.isArray(parsed) && parsed.every((x) => typeof x === "string")) piArgsPrefix = parsed;
    } catch {
      // Malformed TT_TEST_PI_ARGS_PREFIX: ignored, falls back to none —
      // this is a test-only knob, not user-facing input to validate loudly.
    }
  }
  return { piCommand, piArgsPrefix };
}

/** Same `TT_TEST_MODE=1` gate as `testPiInjection`, for the crash suite's
 * own item-2 speed requirement (phase-1 work-packet 1c): the detached
 * `__run-conductor` process it spawns has no in-process hook to pass
 * `ConductorOptions.deadlines` the way a directly-constructed `Conductor`
 * in every other test does — `TT_TEST_DEADLINES` (a JSON object of
 * `Partial<Deadlines>`) is that hook, read the same way
 * `TT_TEST_PI_ARGS_PREFIX` is. Malformed JSON is ignored (falls back to
 * `DEFAULT_DEADLINES`), matching `testPiInjection`'s own "test-only knob,
 * not user-facing input" stance. */
function testDeadlines(): Partial<Deadlines> | undefined {
  if (process.env.TT_TEST_MODE !== "1") return undefined;
  const raw = process.env.TT_TEST_DEADLINES;
  if (!raw) return undefined;
  try {
    const parsed: unknown = JSON.parse(raw);
    if (parsed && typeof parsed === "object") return parsed as Partial<Deadlines>;
  } catch {
    // ignored — see doc comment above.
  }
  return undefined;
}

/** Work packet 2a: same `TT_TEST_MODE=1` gate as `testPiInjection`/
 * `testDeadlines` — `tt start`'s pre-existing exit-gate tests
 * (`tt-start-happy-path`, the crash suite) predate the real two-turn
 * review protocol and script a stub reviewer with no `submit_discovery`
 * call at all; `TT_TEST_STUB_REVIEWS=1` opts a detached `__run-conductor`
 * process into `Conductor`'s own `stubReviews: true`, the same way a
 * directly-constructed test `Conductor` does. Never on by default — a real
 * `tt start` run gets real reviewers unless a test explicitly asks for the
 * stub path. */
function testStubReviews(): boolean {
  return process.env.TT_TEST_MODE === "1" && process.env.TT_TEST_STUB_REVIEWS === "1";
}

/** The detached conductor's own entry point: `tt __run-conductor <runDir>`,
 * spawned by `cmdStart` above with stdio redirected to `<run>/conductor.log`.
 * Not a user-facing subcommand — kept in this file (rather than a separate
 * script) to stay inside this packet's file scope. */
async function runConductorProcess(runDir: string): Promise<void> {
  const p = runPaths(runDir);
  const plan = JSON.parse(readFileSync(path.join(p.plan, "v1.json"), "utf8")) as RunPlanFile;
  const { piCommand, piArgsPrefix } = testPiInjection();
  const deadlines = testDeadlines();
  const stubReviews = testStubReviews();
  writeFileSync(path.join(runDir, "conductor.pid"), String(process.pid));
  const conductor = new Conductor({ runDir, plan, piCommand, piArgsPrefix, deadlines, stubReviews });
  process.on("SIGTERM", () => void conductor.stop().then(() => process.exit(0)));
  process.on("SIGINT", () => void conductor.stop().then(() => process.exit(0)));
  await conductor.start();
}

/** design §9.2/round-of-review item 2: `tt status` rebuilds state the same
 * way the conductor does — by folding `events.jsonl`'s logged transitions
 * through `reduce()` — rather than reading a stored snapshot (there is no
 * such snapshot any more: the log holds transitions, intents, completions
 * and applied commands only). `views/status.txt` (§9.2) may be regenerated
 * from this same rebuilt state; it is never read back. */
function renderStatus(runDir: string): string {
  const p = runPaths(runDir);
  const plan = JSON.parse(readFileSync(path.join(p.plan, "v1.json"), "utf8")) as RunPlanFile;
  const state = rebuildState(runDir, plan, { lenient: true });
  const phase = state.phase as unknown as Record<string, unknown>;
  const lines: string[] = [];
  lines.push(`run: ${path.basename(runDir)}`);
  lines.push(`run status: ${state.run}`);
  lines.push(`phase: ${phase.phaseId} — ${phase.phase}`);
  const attempt = phase.attempt as { n: number; interrupted?: boolean } | undefined;
  if (attempt) lines.push(`attempt: ${attempt.n}${attempt.interrupted ? " (interrupted)" : ""}`);
  const candidate = phase.candidate as { sha: string } | undefined;
  if (candidate) lines.push(`candidate: ${candidate.sha}`);
  const checks = phase.checks as { passed?: boolean; interrupted?: boolean } | undefined;
  if (checks) lines.push(`checks: ${checks.interrupted ? "interrupted" : checks.passed ? "passed" : "failed"}`);
  const probe = phase.probe as { passed?: boolean; probedI?: string } | undefined;
  if (probe) lines.push(`probe: ${probe.passed ? `passed (I=${probe.probedI})` : "failed"}`);
  const reviews = phase.reviews as Record<string, { review?: unknown }> | undefined;
  if (reviews) {
    for (const who of ["M", "A", "B"]) {
      lines.push(`review ${who}: ${reviews[who]?.review ? "submitted" : "pending"}`);
    }
  }
  const ownerRequests = (phase.ownerRequests as Array<{ status: string }>) ?? [];
  const openRequests = ownerRequests.filter((r) => r.status === "open");
  lines.push(`open owner requests: ${openRequests.length}`);
  if (phase.blockedReason) lines.push(`blocked: ${phase.blockedReason}`);
  if (phase.publishedI) lines.push(`published: ${phase.publishedI}`);
  return `${lines.join("\n")}\n`;
}

async function cmdStatus(runIdOrDir: string, root: string): Promise<void> {
  const runDir = resolveRunDir(runIdOrDir, root);
  process.stdout.write(renderStatus(runDir));
}

async function main(): Promise<void> {
  const [cmd, ...rest] = process.argv.slice(2);
  if (cmd === RUN_CONDUCTOR_FLAG) {
    await runConductorProcess(rest[0]);
    return;
  }
  const { positional, root } = parseArgs(rest);
  const runRoot = root ?? DEFAULT_ROOT;
  if (cmd === "start") {
    if (positional.length !== 1) usage();
    await cmdStart(positional[0], runRoot);
  } else if (cmd === "status") {
    if (positional.length !== 1) usage();
    await cmdStatus(positional[0], runRoot);
  } else if (cmd === "resume") {
    if (positional.length !== 1) usage();
    launchDetached(resolveRunDir(positional[0], runRoot));
  } else if (cmd === "state") {
    // Machine-readable run state for the Emacs front end: meta, plan and
    // the state rebuilt by folding the control log (never a stored snapshot).
    if (positional.length !== 1) usage();
    const runDir = resolveRunDir(positional[0], runRoot);
    const p = runPaths(runDir);
    const plan = JSON.parse(readFileSync(path.join(p.plan, "v1.json"), "utf8")) as RunPlanFile;
    const meta = JSON.parse(readFileSync(p.meta, "utf8"));
    const state = rebuildState(runDir, plan, { lenient: true });
    let alive = false;
    try {
      const pid = Number(readFileSync(path.join(runDir, "conductor.pid"), "utf8"));
      if (pid > 0) {
        process.kill(pid, 0);
        alive = true;
      }
    } catch {
      alive = false;
    }
    // Plan 2c: the tally's view of every decision on the current candidate
    // (passed / failed with its reason / suspended / pending / superseded),
    // so front ends never infer it from individual ballots.
    const decisionStatuses = Object.fromEntries(
      state.phase.decisions.map((d) => [d.id, decisionStatus(d, state.phase)]),
    );
    const round = state.phase.round ?? 0;
    process.stdout.write(`${JSON.stringify({ runDir, meta, plan, state, round, decisionStatuses, conductorAlive: alive })}\n`);
  } else if (cmd === "runner" && positional[0] === "install") {
    // `tt runner install <sha>`: freeze an accepted revision outside every
    // worktree at <root>/runner/<sha>/, so a run that edits tradeoffs-trace
    // never executes the code under review.
    const sha = positional[1];
    if (!sha) usage();
    const pkgRoot = fileURLToPath(new URL("..", import.meta.url));
    const repo = execFileSync("git", ["-C", pkgRoot, "rev-parse", "--show-toplevel"], { encoding: "utf8" }).trim();
    const full = execFileSync("git", ["-C", repo, "rev-parse", "--verify", `${sha}^{commit}`], { encoding: "utf8" }).trim();
    const dest = path.join(runRoot, "runner", full);
    mkdirSync(dest, { recursive: true });
    execFileSync("/bin/sh", ["-c", `git -C "$1" archive "$2" tradeoffs-trace | tar -x -C "$3"`, "sh", repo, full, dest]);
    writeFileSync(path.join(dest, "tradeoffs-trace", "RUNNER_SHA"), `${full}\n`);
    const current = path.join(runRoot, "runner", "current");
    rmSync(current, { force: true });
    symlinkSync(full, current);
    process.stdout.write(`${path.join(dest, "tradeoffs-trace")}\n`);
  } else {
    usage();
  }
}

void main();
