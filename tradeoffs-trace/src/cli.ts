// `tt start <plan.json> [--root <dir>]` and `tt status <run-dir-or-id>`
// (design §1.2, phase-1 plan: "a text rendering of the log is enough for
// this phase").
//
// `tt start` creates the run directory (design §9.1) and launches the
// conductor **detached**: its own session, stdio redirected to
// `<run>/conductor.log`, so the parent (this CLI invocation) returns the
// run id immediately rather than blocking for the whole run.

import { execFileSync, spawn } from "node:child_process";
import { readFileSync, existsSync, openSync, mkdirSync, writeFileSync, rmSync, symlinkSync, readdirSync, statSync } from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

import { decisionStatus } from "./core/predicate.ts";
import { acquireLock } from "./effects/lock.ts";
import { Conductor, createRun, rebuildState, runPaths, type Deadlines, type RunPlanFile } from "./conductor.ts";
import { buildView, prSummary, timingReport, timingText } from "./view.ts";
import { removedTestsBetween } from "./effects/git.ts";
import {
  loggedSecretStatus,
  planSecretNames,
  redactJson,
  redactRunDir,
  redactText,
  resolveSecrets,
  secretNames,
  type Secret,
} from "./effects/secrets.ts";
import type { ProgramFile } from "./core/program.ts";
import {
  addProgramDirective,
  appendProgramEvent,
  createProgram,
  foldProgram,
  programPaths,
  programPidAlive,
  programsRoot,
  programStatusLines,
  runScheduler,
  withdrawProgramDirective,
} from "./program.ts";

const DEFAULT_ROOT = path.join(os.homedir(), ".tradeoffs-trace");

function usage(): never {
  process.stderr.write(
    "usage: tt start <plan.json> [--root <dir>]\n       tt stop <run-dir-or-id> [--root <dir>]\n       tt list [--json] [--root <dir>]\n       tt summary <run-dir-or-id> [--root <dir>]   (PR body, Markdown)\n       tt program start <program.json> | status <id> | state <id> | stop <id> | resume <id> | list | prs <id>  [--root <dir>]\n       tt program directive <id> <text> | withdraw <id> <ODP-n>  [--root <dir>]\n       tt timing <run-dir-or-id> [--json] [--root <dir>]\n       tt status <run-dir-or-id> [--root <dir>]\n       tt state <run-dir-or-id> [--root <dir>]   (JSON)\n       tt redact <run-dir-or-id> | --all  [--secrets NAME…] [--force] [--root <dir>]\n       tt runner install <sha> [--root <dir>]\n       tt resume <run-dir-or-id> [--root <dir>]\n",
  );
  process.exit(2);
}

function parseArgs(argv: string[]): { positional: string[]; root?: string; json: boolean } {
  const positional: string[] = [];
  let root: string | undefined;
  let json = false;
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--root") {
      root = argv[++i];
    } else if (argv[i] === "--json") {
      json = true;
    } else {
      positional.push(argv[i]);
    }
  }
  return { positional, root, json };
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
const RUN_PROGRAM_FLAG = "__run-program";

/** Phase 4: starts a program's scheduler as a detached process. */
function launchProgramScheduler(dir: string): void {
  const thisScript = fileURLToPath(import.meta.url);
  const out = openSync(programPaths(dir).log, "a");
  const child = spawn(process.execPath, [thisScript, RUN_PROGRAM_FLAG, dir], { detached: true, stdio: ["ignore", out, out] });
  child.unref();
}

function resolveProgramDir(idOrDir: string, root: string): string {
  if (existsSync(path.join(idOrDir, "program.json"))) return path.resolve(idOrDir);
  return path.join(programsRoot(root), idOrDir);
}

/** Skill fix 3: the PR body for a finished run — the review outcome, the
 * blocking findings fixed on the way, flagged decisions, every open advisory
 * finding (accepted, not fixed) and tests removed from surviving files.
 * Also written to <run>/views/pr.md. */
function runSummary(runDir: string): string {
  const plan = readPlan(runDir);
  const state = rebuildState(runDir, plan, { lenient: true });
  let removed: string[] = [];
  const C = state.phase.candidate?.sha;
  if (C) {
    try {
      removed = removedTestsBetween(plan.repo, state.phase.integrationHead, C);
    } catch {
      removed = [];
    }
  }
  const md = prSummary(runDir, plan, { removedTests: removed });
  // Plan 01a: the body quotes findings and decisions, which can carry a
  // secret value an agent echoed; `views/pr.md` is written redacted.
  const redacted = redactText(md, secretsForRun(runDir));
  try {
    writeFileSync(path.join(runPaths(runDir).views, "pr.md"), redacted);
  } catch {
    // views/ may be missing on a very old run; printing is what matters
  }
  return redacted;
}

/** Plan 01a: the values this run's plan declares, resolved from this
 * process's own environment — `tt state`/`tt timing`/`tt status`/`tt summary`
 * redact what they print with them, so a value an agent echoed into a
 * finding, a decision or a command is never shown. */
function secretsForRun(runDir: string): Secret[] {
  try {
    return resolveSecrets(secretNames(readPlan(runDir).secrets)).maskable;
  } catch {
    return [];
  }
}

async function cmdProgram(sub: string | undefined, args: string[], root: string, json: boolean): Promise<void> {
  if (sub === "start") {
    if (args.length !== 1) usage();
    const program = JSON.parse(readFileSync(args[0], "utf8")) as ProgramFile;
    const dir = createProgram(root, program);
    launchProgramScheduler(dir);
    process.stdout.write(`${path.basename(dir)}\n`);
  } else if (sub === "status") {
    if (args.length !== 1) usage();
    process.stdout.write(`${programStatusLines(resolveProgramDir(args[0], root)).join("\n")}\n`);
  } else if (sub === "state") {
    if (args.length !== 1) usage();
    const dir = resolveProgramDir(args[0], root);
    const { program, nodes, state } = foldProgram(dir);
    process.stdout.write(
      `${JSON.stringify({ dir, title: program.title, maxParallel: program.maxParallel, nodes, state, schedulerAlive: programPidAlive(dir), lines: programStatusLines(dir) })}\n`,
    );
  } else if (sub === "stop") {
    if (args.length !== 1) usage();
    const dir = resolveProgramDir(args[0], root);
    appendProgramEvent(dir, { type: "PROGRAM_STOPPED" });
    try {
      process.kill(Number(readFileSync(programPaths(dir).pid, "utf8")), "SIGTERM");
    } catch {
      // not running
    }
    const { state } = foldProgram(dir);
    for (const [node, n] of Object.entries(state.nodes)) {
      if (!n.runId || !["running", "needs-you"].includes(n.status)) continue;
      await cmdStop(n.runId, root);
      // The scheduler is gone, so record the stop here: the program status
      // must not keep showing the node as running.
      appendProgramEvent(dir, { type: "NODE_STATUS", node, status: "stopped" });
    }
    process.stdout.write(`stopped program ${path.basename(dir)}\n`);
  } else if (sub === "resume") {
    if (args.length !== 1) usage();
    const dir = resolveProgramDir(args[0], root);
    // Undo a stop, restart every node run that is not running (stopped by
    // the owner or crashed), then the scheduler if it is not running.
    const { state } = foldProgram(dir);
    if (state.stopped) appendProgramEvent(dir, { type: "PROGRAM_RESUMED" });
    const restarted: string[] = [];
    for (const [node, s] of Object.entries(state.nodes)) {
      if (!s.runId || ["done", "blocked", "waiting"].includes(s.status)) continue;
      const runDir = path.join(root, s.runId);
      if (conductorAlive(runDir)) continue;
      appendProgramEvent(dir, { type: "NODE_RESUMED", node, reason: "owner" });
      launchDetached(runDir);
      restarted.push(node);
    }
    if (!programPidAlive(dir)) launchProgramScheduler(dir);
    process.stdout.write(
      `resumed program ${path.basename(dir)}${restarted.length > 0 ? ` (restarted ${restarted.join(", ")})` : ""}\n`,
    );
  } else if (sub === "prs") {
    // Skill fix 3: one PR per DONE node, stacked on its dependency's branch.
    // Prints the commands; pushing and opening PRs stay the owner's call.
    if (args.length !== 1) usage();
    const dir = resolveProgramDir(args[0], root);
    const { program, nodes, state } = foldProgram(dir);
    const out: string[] = [];
    for (const n of nodes) {
      const s = state.nodes[n.id];
      if (s.status !== "done" || !s.runId || !s.branch) continue;
      const runDir = path.join(root, s.runId);
      runSummary(runDir);
      const entry = program.entries.find((e) => e.id === n.entry)!;
      // A join's base is its first parent; a root's is its plan's TT_BRANCH.
      const base = (s.base ?? entry.plan.integrationBranch).split(" + ")[0];
      const title = `${entry.plan.title}`.replace(/"/g, "'");
      out.push(`git -C ${entry.plan.repo} push -u origin ${s.branch}`);
      out.push(`(cd ${entry.plan.repo} && gh pr create --base ${base} --head ${s.branch} --title "${title}" --body-file ${path.join(runDir, "views", "pr.md")})`);
    }
    process.stdout.write(out.length > 0 ? `${out.join("\n")}\n` : "no DONE nodes yet\n");
  } else if (sub === "directive") {
    // Plan 01i: a program-wide owner directive — every running node is
    // steered at once, every node started later is started with it.
    if (args.length !== 2) usage();
    const dir = resolveProgramDir(args[0], root);
    process.stdout.write(`${addProgramDirective(root, dir, args[1]).id}\n`);
  } else if (sub === "withdraw") {
    if (args.length !== 2) usage();
    const dir = resolveProgramDir(args[0], root);
    if (!withdrawProgramDirective(root, dir, args[1])) {
      process.stderr.write(`no program directive ${args[1]} is in force\n`);
      process.exitCode = 1;
      return;
    }
    process.stdout.write(`withdrew ${args[1]}\n`);
  } else if (sub === "list") {
    let ids: string[] = [];
    try {
      ids = readdirSync(programsRoot(root));
    } catch {
      ids = [];
    }
    const rows = ids.map((id) => programStatusLines(path.join(programsRoot(root), id)).slice(0, 2).join(" · "));
    process.stdout.write(json ? `${JSON.stringify(ids)}\n` : rows.map((r) => `${r}\n`).join(""));
  } else {
    usage();
  }
}

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
  // A plan's own limits (TT_*_MINUTES), then the test-only override.
  const planDeadlines = plan.deadlines ?? {};
  const t = testDeadlines();
  const deadlines = t || Object.keys(planDeadlines).length > 0 ? { ...planDeadlines, ...(t ?? {}) } : undefined;
  const stubReviews = testStubReviews();
  writeFileSync(path.join(runDir, "conductor.pid"), String(process.pid));
  // A clean stop leaves this marker; a conductor that dies without it
  // crashed, and a program scheduler restarts it (src/program.ts).
  const stoppedMarker = path.join(runDir, "stopped");
  rmSync(stoppedMarker, { force: true });
  const conductor = new Conductor({ runDir, plan, piCommand, piArgsPrefix, deadlines, stubReviews });
  const cleanStop = () =>
    void conductor.stop().then(() => {
      writeFileSync(stoppedMarker, new Date().toISOString());
      process.exit(0);
    });
  process.on("SIGTERM", cleanStop);
  process.on("SIGINT", cleanStop);
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
  // Plan 01a: a declared secret that was unset, or set to a value too short
  // to mask safely, is reported here (names only); the run still runs.
  const secretStatus = loggedSecretStatus(runDir);
  for (const name of secretStatus.missing) lines.push(`secret ${name} not set`);
  for (const name of secretStatus.tooShort) lines.push(`secret ${name} too short to mask (value under 4 characters)`);
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
  // Plan 01i: every directive in force, with its scope, so `tt status` names
  // the rulings the phase is bound to.
  const directives = (phase.ownerDirectives as Array<{ id: string; text: string; scope: string; status: string }>) ?? [];
  for (const d of directives.filter((d) => d.status === "in-force")) {
    lines.push(`directive ${d.id} (${d.scope === "program" ? "whole program" : "this phase"}): ${d.text}`);
  }
  if (phase.blockedReason) lines.push(`blocked: ${phase.blockedReason}`);
  if (phase.publishedI) lines.push(`published: ${phase.publishedI}`);
  // Plan 3b: the same readable view the status buffer shows.
  const view = buildView(runDir, plan, conductorAlive(runDir));
  lines.push(`pipeline: ${view.pipeline}`);
  lines.push(`gates: ${view.gates}`);
  // Plan 01f: the conductor's gate record, cited (tt summary cites the same).
  if (view.gate) lines.push(`gate: ${view.gate}`);
  // Plan 01e: the base's own pre-existing check failures (D2), when it has any.
  if (view.baseline) lines.push(`base: ${view.baseline}`);
  lines.push(`reviews: ${view.reviewLine}`);
  // Plan 01g: every amendment record, applied or reverted, with old → new.
  if (view.amendments) lines.push(`amendments: ${view.amendments}`);
  if (view.verdict) lines.push(`verdict: ${view.verdict}`);
  if (view.time) lines.push(`time: ${view.time}`);
  return `${lines.join("\n")}\n`;
}

function conductorAlive(runDir: string): boolean {
  try {
    return pidAlive(Number(readFileSync(path.join(runDir, "conductor.pid"), "utf8")));
  } catch {
    return false;
  }
}

function readPlan(runDir: string): RunPlanFile {
  return JSON.parse(readFileSync(path.join(runPaths(runDir).plan, "v1.json"), "utf8")) as RunPlanFile;
}

/** Plan 3b: `tt list` — one line per run (newest activity first), the
 * summary the Emacs runs list and mode-line indicator render. */
function cmdList(root: string, json: boolean): void {
  let names: string[] = [];
  try {
    names = readdirSync(root).filter((n) => existsSync(path.join(root, n, "meta.json")));
  } catch {
    names = [];
  }
  const rows = names
    .map((n) => {
      const runDir = path.join(root, n);
      try {
        const alive = conductorAlive(runDir);
        const meta = JSON.parse(readFileSync(runPaths(runDir).meta, "utf8")) as { title?: string };
        const v = buildView(runDir, readPlan(runDir), alive);
        // Plan 01a: a title or an attention line can quote a value.
        const secrets = secretsForRun(runDir);
        return {
          id: n,
          runDir,
          title: redactText(meta.title ?? "", secrets),
          phase: v.timeline.state.phase.phase,
          stage: v.stage,
          stageElapsed: v.stageElapsed,
          elapsed: v.elapsed,
          reviews: v.reviewLine,
          attention: v.attention ? redactText(v.attention, secrets) : null,
          needsYou: v.needsYou,
          alive,
          activity: statSync(runPaths(runDir).events).mtimeMs,
        };
      } catch {
        return undefined;
      }
    })
    .filter((r): r is NonNullable<typeof r> => r !== undefined)
    .sort((a, b) => b.activity - a.activity);
  if (json) {
    process.stdout.write(`${JSON.stringify(rows)}\n`);
    return;
  }
  for (const r of rows) {
    process.stdout.write(
      `${r.id}  ${(r.alive ? "●" : "○")} ${r.stage.padEnd(10)} ${r.stageElapsed.padStart(7)}  ${r.reviews}  ${r.attention ? `⚑ ${r.attention}  ` : ""}${r.title}\n`,
    );
  }
}

/** Plan 01a: `tt redact <run-dir-or-id | --all> [--secrets NAME…] [--force]`
 * rewrites existing run directories in place — the streams, the control log,
 * the check logs, the refs copies and the views — replacing every declared
 * secret's value with `***NAME***`. The values come from this process's
 * environment (there is nowhere else to read them from), so a past run can be
 * cleaned by exporting the keys it used and naming their variables here.
 * Without `--secrets`, a run's own plan snapshot supplies the names.
 *
 * A run whose conductor is still alive is refused: it keeps writing (Pi's own
 * session file, the stream, the log), so a "redacted" run could regain the
 * value a second later. `--force` overrides that, and the warning says what
 * the live run will keep writing. */
function cmdRedact(argv: string[], defaultRoot: string): void {
  const positional: string[] = [];
  const nameArgs: string[] = [];
  let all = false;
  let force = false;
  let root = defaultRoot;
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--all") all = true;
    else if (arg === "--force") force = true;
    else if (arg === "--root") root = argv[++i];
    else if (arg === "--secrets") {
      while (i + 1 < argv.length && !argv[i + 1].startsWith("--")) nameArgs.push(argv[++i]);
    } else if (arg.startsWith("--secrets=")) nameArgs.push(arg.slice("--secrets=".length));
    else positional.push(arg);
  }
  if (all === (positional.length > 0) || positional.length > 1) usage();

  let dirs: string[];
  if (all) {
    try {
      dirs = readdirSync(root)
        .filter((n) => existsSync(path.join(root, n, "meta.json")))
        .map((n) => path.join(root, n));
    } catch {
      dirs = [];
    }
  } else {
    dirs = positional.map((p) => resolveRunDir(p, root));
  }
  if (dirs.length === 0) {
    process.stdout.write(`no runs to redact under ${root}\n`);
    return;
  }

  const named = secretNames(nameArgs);
  let files = 0;
  let runs = 0;
  const unset: string[] = [];
  const tooShort: string[] = [];
  const unnamed: string[] = [];
  const notRuns: string[] = [];
  const live: string[] = [];
  const opaque: string[] = [];
  for (const dir of dirs) {
    if (!existsSync(path.join(dir, "meta.json"))) {
      notRuns.push(dir);
      continue;
    }
    if (conductorAlive(dir) && !force) {
      live.push(path.basename(dir));
      continue;
    }
    const declared = named.length > 0 ? named : planSecretNames(dir);
    if (declared.length === 0) {
      unnamed.push(path.basename(dir));
      continue;
    }
    const { maskable, missing, tooShort: short } = resolveSecrets(declared);
    unset.push(...missing);
    tooShort.push(...short);
    const result = redactRunDir(dir, maskable);
    files += result.changed;
    for (const file of result.opaque) opaque.push(path.relative(dir, file));
    runs += 1;
  }
  process.stdout.write(`redacted ${runs} of ${dirs.length} run(s), ${files} file(s)\n`);
  if (live.length > 0) {
    process.stdout.write(
      `conductor still running: ${live.join(", ")} — refused (it keeps writing stream, sessions and log); stop it first, or pass --force\n`,
    );
  }
  for (const name of [...new Set(unset)]) {
    process.stdout.write(`secret ${name} not set: its value is not in this environment, nothing was redacted for it\n`);
  }
  for (const name of [...new Set(tooShort)]) {
    process.stdout.write(`secret ${name} too short to mask (value under 4 characters): masking it would rewrite unrelated text\n`);
  }
  if (unnamed.length > 0) process.stdout.write(`no declared secrets (pass --secrets NAME…): ${unnamed.join(", ")}\n`);
  for (const dir of notRuns) process.stdout.write(`not a run directory (no meta.json): ${dir}\n`);
  if (opaque.length > 0) {
    process.stdout.write(
      `not UTF-8 text, searched UTF-8/UTF-16 only — check these yourself: ${opaque.join(", ")}\n`,
    );
  }
}

function pidAlive(pid: number): boolean {
  if (!Number.isFinite(pid) || pid <= 0) return false;
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

/** Plan 2d: `tt stop <run>` ends a run's conductor cleanly within 15 s —
 * SIGTERM to the recorded pid, then wait for the process to exit and the
 * run lock to be released. The conductor itself logs the stop event (see
 * `Conductor#doStop`). A run with no live conductor is a no-op. */
async function cmdStop(runIdOrDir: string, root: string): Promise<void> {
  const runDir = resolveRunDir(runIdOrDir, root);
  const p = runPaths(runDir);
  let pid = 0;
  try {
    pid = Number(readFileSync(path.join(runDir, "conductor.pid"), "utf8"));
  } catch {
    pid = 0;
  }
  if (!pidAlive(pid)) {
    process.stdout.write(`run ${path.basename(runDir)} has no running conductor\n`);
    return;
  }
  try {
    process.kill(pid, "SIGTERM");
  } catch {
    // raced with the conductor exiting on its own; fall through to the wait
  }
  const deadline = Date.now() + 15_000;
  while (pidAlive(pid) && Date.now() < deadline) {
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  if (pidAlive(pid)) {
    process.stderr.write(`conductor ${pid} did not stop within 15s\n`);
    process.exitCode = 1;
    return;
  }
  // The lock's flock is released by the OS when the conductor (and its perl
  // helper) die; prove it by taking and immediately releasing it (design
  // §9.1: exactly one conductor per run). Retry briefly: a SIGKILLed
  // conductor's helper may need a beat to notice its stdin closed.
  for (;;) {
    try {
      const lock = await acquireLock(p.lock);
      await lock.release();
      process.stdout.write(`stopped ${path.basename(runDir)}\n`);
      return;
    } catch {
      if (Date.now() >= deadline) {
        process.stderr.write(`conductor ${pid} exited but the run lock is still held\n`);
        process.exitCode = 1;
        return;
      }
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
  }
}

/** Plan 2d: pending owner-input files the conductor has not yet picked up
 * (design §7.4/§9.3). `tt state` carries them so the status buffer can show
 * a command the owner sent while no conductor was running as "not picked
 * up" after 30 s, rather than silently losing it. */
function pendingOwnerInputs(runDir: string): Array<{ id: string; kind: string; text: string; at: string }> {
  let names: string[];
  try {
    names = readdirSync(path.join(runDir, "inbox"));
  } catch {
    return [];
  }
  const out: Array<{ id: string; kind: string; text: string; at: string }> = [];
  for (const name of names.sort()) {
    if (!name.endsWith(".json")) continue;
    try {
      const raw = JSON.parse(readFileSync(path.join(runDir, "inbox", name), "utf8")) as Record<string, unknown>;
      const kind = typeof raw.type === "string" ? raw.type : typeof raw.kind === "string" ? raw.kind : undefined;
      if (kind !== "steer" && kind !== "note" && kind !== "correction") continue;
      if (typeof raw.text !== "string") continue;
      const at = statSync(path.join(runDir, "inbox", name)).mtime.toISOString();
      out.push({ id: name.slice(0, -".json".length), kind, text: raw.text, at });
    } catch {
      // A file still being written, or malformed: the conductor will reject
      // it; not this view's job to guess.
    }
  }
  return out;
}

async function main(): Promise<void> {
  const [cmd, ...rest] = process.argv.slice(2);
  if (cmd === RUN_PROGRAM_FLAG) {
    // <root>/programs/<id>: node runs live in <root>, like any other run.
    const dir = rest[0];
    const outcome = await runScheduler(dir, { runRoot: path.dirname(path.dirname(dir)), launch: launchDetached });
    process.stdout.write(`program ${path.basename(dir)}: ${outcome}\n`);
    return;
  }
  if (cmd === RUN_CONDUCTOR_FLAG) {
    await runConductorProcess(rest[0]);
    return;
  }
  const { positional, root, json } = parseArgs(rest);
  const runRoot = root ?? DEFAULT_ROOT;
  if (cmd === "start") {
    if (positional.length !== 1) usage();
    await cmdStart(positional[0], runRoot);
  } else if (cmd === "status") {
    if (positional.length !== 1) usage();
    const runDir = resolveRunDir(positional[0], runRoot);
    process.stdout.write(redactText(renderStatus(runDir), secretsForRun(runDir)));
  } else if (cmd === "resume") {
    if (positional.length !== 1) usage();
    launchDetached(resolveRunDir(positional[0], runRoot));
  } else if (cmd === "program") {
    await cmdProgram(positional[0], positional.slice(1), runRoot, json);
  } else if (cmd === "list") {
    cmdList(runRoot, json);
  } else if (cmd === "summary") {
    if (positional.length !== 1) usage();
    process.stdout.write(runSummary(resolveRunDir(positional[0], runRoot)));
  } else if (cmd === "timing") {
    if (positional.length !== 1) usage();
    const runDir = resolveRunDir(positional[0], runRoot);
    const secrets = secretsForRun(runDir);
    const times = timingReport(runDir);
    process.stdout.write(json ? `${JSON.stringify(redactJson(times, secrets))}\n` : `${redactText(timingText(times), secrets)}\n`);
  } else if (cmd === "stop") {
    if (positional.length !== 1) usage();
    await cmdStop(positional[0], runRoot);
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
    const ownerInputs = state.phase.ownerInputs ?? [];
    const { timeline: _timeline, ...view } = buildView(runDir, plan, alive);
    // Plan 01i: the program this run is a node of, when a scheduler started
    // it — the front end needs it to tell the truth about `C-u`'s scope (a
    // hand-started run has nothing program-wide to reach).
    let program: { programId?: string; node?: string } | null = null;
    try {
      program = JSON.parse(readFileSync(path.join(runDir, "program.json"), "utf8"));
    } catch {
      program = null;
    }
    const payload = {
      runDir,
      meta,
      plan,
      state,
      round,
      decisionStatuses,
      conductorAlive: alive,
      program,
      ownerInputs,
      pendingOwnerInputs: pendingOwnerInputs(runDir),
      // Plan 01a: the plan's declared secret names, and which were unset or
      // unusable when the conductor started (names only — never a value).
      secrets: {
        declared: planSecretNames(runDir),
        missing: loggedSecretStatus(runDir).missing,
        tooShort: loggedSecretStatus(runDir).tooShort,
      },
      view,
    };
    // Plan 01a: "what `tt state` prints" carries no secret value either —
    // redacted structurally, so the JSON stays JSON.
    process.stdout.write(`${JSON.stringify(redactJson(payload, secretsForRun(runDir)))}\n`);
  } else if (cmd === "redact") {
    cmdRedact(rest, runRoot);
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
