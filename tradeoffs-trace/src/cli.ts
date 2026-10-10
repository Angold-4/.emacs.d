// `tt start <plan.json> [--root <dir>]` and `tt status <run-dir-or-id>`
// (design §1.2, phase-1 plan: "a text rendering of the log is enough for
// this phase").
//
// `tt start` creates the run directory (design §9.1) and launches the
// conductor **detached**: its own session, stdio redirected to
// `<run>/conductor.log`, so the parent (this CLI invocation) returns the
// run id immediately rather than blocking for the whole run.

import { execFileSync, spawn, spawnSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import { readFileSync, existsSync, openSync, mkdirSync, writeFileSync, renameSync, rmSync, symlinkSync, readdirSync, statSync } from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

import { projectLedger, projectMessages } from "./core/messages.ts";
import { expandEntryCommand } from "./core/owner-inbox.ts";
import type { ReviewLintResult } from "./core/review-lint.ts";
import { candidateAnchorFreshness, pendingOwnerInputs, projectEntryReview, renderStatusText, renderStatusView, reviewMessageFiles, runIds, statusViewInput } from "./render.ts";
import { metricsForRunDir, projectMetrics } from "./metrics.ts";
import { reduce } from "./core/reduce.ts";
import { resolveBinding } from "./core/binding.ts";
import { decisionStatus } from "./core/predicate.ts";
import {
  checkCoverageWarnings,
  checkVerifyReach,
  formatFindings,
  hasLintErrors,
  isProgramInput,
  lintPlan,
  lintProgram,
  type LintFinding,
  type LintPlanInput,
  type LintProgramInput,
} from "./core/plan-lint.ts";
import { readRepoFacts, readVerifyReachFacts } from "./core/repo-facts.ts";
import { PLAN_TEMPLATE, parseOrgPlan } from "./core/org-plan.ts";
import { EventLog } from "./effects/log.ts";
import { acquireLock } from "./effects/lock.ts";
import { loggedHeld, processAlive, signalProcess } from "./effects/sweep.ts";
import { currentChoice, installedRunnerRoot, recordedRunnerRevision, setRunnerChooser, type RunnerChoice } from "./effects/runner.ts";
import { Conductor, createRun, rebuildState, rebuildTimelineWithEvents, runnerRevision, runPaths, type Deadlines, type RunPlanFile } from "./conductor.ts";
import { planModelSelector } from "./core/roles.ts";
import { distinctModelGroups, modelsCheckRefused, planModelTargets, type ModelsCheck } from "./core/models-check.ts";
import { runModelsCheck } from "./effects/models-check.ts";
import { effectiveChecks } from "./core/checks.ts";
// Plan 05i: the program commands preflight every node they will start, before
// any run is created, so a missing toolchain refuses visibly instead of
// creating runs that are immediately environment-blocked.
import { envPreflight } from "./core/env-preflight.ts";
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
import { programOutcome, type ProgramFile } from "./core/program.ts";
import {
  addProgramDirective,
  appendProgramEvent,
  createProgram,
  foldProgram,
  programPaths,
  programPidAlive,
  programsRoot,
  programStatusLines,
  programWaitingNodes,
  readProgramSource,
  recordProgramSource,
  runScheduler,
  withdrawProgramDirective,
  writeProgramChart,
} from "./program.ts";

const DEFAULT_ROOT = path.join(os.homedir(), ".tradeoffs-trace");

function usage(): never {
  process.stderr.write(
    "usage: tt start <plan.json> [--root <dir>] [--env-file <KEY=value file>] [--skip-models-check]\n       tt lint <plan.json|program.json|plan.org>   (findings; non-zero on errors)\n       tt plan template                  (print the plan skeleton)\n       tt evidence <run-dir-or-id> <item> <file-or-text>\n       tt stop <run-dir-or-id> [--root <dir>]\n       tt list [--json] [--root <dir>]\n       tt summary <run-dir-or-id> [--root <dir>]   (PR body, Markdown)\n       tt program start <program.json> [--source <org-file>] [--skip-models-check] | status <id> | state <id> | stop <id> | pause <id> | resume <id> | retry <id> <node> | list | prs <id>  [--root <dir>]\n       tt program directive <id> <text> | withdraw <id> <ODP-n>  [--root <dir>]\n       tt models check <plan.json|program.json>   (probe each configured model)\n       tt timing <run-dir-or-id> [--json] [--root <dir>]\n       tt status <run-dir-or-id> [--root <dir>]\n       tt state <run-dir-or-id> [--root <dir>]   (JSON)\n       tt redact <run-dir-or-id> | --all  [--secrets NAME…] [--force] [--root <dir>]\n       tt runner install <sha> [--root <dir>]\n       tt resume <run-dir-or-id> [--root <dir>]\n       tt defer <run-dir-or-id> <finding-or-decision-id> [--test <name>] [--owner-ruling <id>] [--to <phase>]   (a deferral needs a guard)\n       tt recheck <run-dir-or-id> --reason <why the failure is the machine>   (re-run a failed candidate's checks; no worker, no round)\n       tt verdict <run-dir-or-id> <messageId> <accept|refuse> [--reason <text>] [--candidate-sha <sha>] [--message-version <n>] [--contract-version <n>] [--contract-sha256 <sha>] [--run-id <id>] [--phase-id <id>] [--root <dir>]\n       tt contract rebuild <run-dir-or-id> [--root <dir>]\n       tt contract check <run-dir-or-id> [--root <dir>]\n",
  );
  process.exit(2);
}

/** Plan 01c: lint a parsed JSON file (a plan or a program). Every entry of a
 * program is linted. Pure wrapper so `tt lint` and the start commands share
 * one call. */
function lintJson(json: unknown): LintFinding[] {
  return isProgramInput(json) ? lintProgram(json as LintProgramInput) : lintPlan(json as LintPlanInput);
}

/** Plan 06j (A1): the coverage warnings a plan's repository yields. The
 * repository is read once per plan (a program's entries may name different
 * repos), and a program prefixes each entry's phase id exactly as
 * `lintProgram` does. No repository, or one that does not exist, yields
 * nothing. */
function coverageFindings(json: unknown): LintFinding[] {
  if (isProgramInput(json)) {
    const out: LintFinding[] = [];
    for (const entry of (json as LintProgramInput).entries ?? []) {
      const plan = entry.plan;
      if (!plan?.repo) continue;
      for (const f of checkCoverageWarnings(plan, readRepoFacts(plan.repo))) {
        out.push({ ...f, phaseId: entry.id ? `${entry.id}/${f.phaseId}` : f.phaseId });
      }
    }
    return out;
  }
  const plan = json as LintPlanInput;
  if (!plan?.repo) return [];
  return checkCoverageWarnings(plan, readRepoFacts(plan.repo));
}

/** Plan 06j (A1): every finding a plan (or program) yields, lint rules plus
 * repository coverage. `tt lint` and both start commands use this one
 * function, so the warnings they show are the same. */
function allFindings(json: unknown): LintFinding[] {
  return [...lintJson(json), ...coverageFindings(json), ...verifyReachFindings(json)];
}

/** 06k1's lesson: a named test the phase's checks never run (an error). Read
 * per plan like coverage, and prefixed per program entry the same way. */
function verifyReachFindings(json: unknown): LintFinding[] {
  if (isProgramInput(json)) {
    const out: LintFinding[] = [];
    for (const entry of (json as LintProgramInput).entries ?? []) {
      const plan = entry.plan;
      if (!plan?.repo) continue;
      for (const f of checkVerifyReach(plan, readVerifyReachFacts(plan))) {
        out.push({ ...f, phaseId: entry.id ? `${entry.id}/${f.phaseId}` : f.phaseId });
      }
    }
    return out;
  }
  const plan = json as LintPlanInput;
  if (!plan?.repo) return [];
  return checkVerifyReach(plan, readVerifyReachFacts(plan));
}

/** Plan 01c: print findings to OUT (stderr for a start that is about to be
 * refused, stdout for `tt lint`). Nothing is printed when there are none. */
function reportFindings(findings: readonly LintFinding[], file: string, out: NodeJS.WriteStream): void {
  if (findings.length === 0) return;
  out.write(`${formatFindings(findings, file)}\n`);
}

/** Plan 01c: `tt lint <plan.json|program.json>`. Prints every finding and
 * exits non-zero when any is an error, so a script (and Emacs) can branch on
 * the exit status. Warnings alone exit 0. */
function cmdLint(file: string): void {
  // Plan 06b: `tt lint` reads a phase subtree too — an `.org` file is parsed
  // by the lint-side Org reader, a `.json` file is read directly. Both reach
  // the same item rules.
  const json = file.endsWith(".org") ? parseOrgPlan(readFileSync(file, "utf8"), file) : (JSON.parse(readFileSync(file, "utf8")) as unknown);
  const findings = allFindings(json);
  reportFindings(findings, file, process.stdout);
  if (hasLintErrors(findings)) process.exitCode = 1;
}

/** Plan 06b: `tt plan template` — the Org skeleton owners and agents start
 * from. It is lint-clean, so a new plan begins from a passing shape. */
function cmdPlanTemplate(): void {
  process.stdout.write(PLAN_TEMPLATE);
}

function parseArgs(argv: string[]): {
  positional: string[];
  root?: string;
  json: boolean;
  reason?: string;
  candidateSha?: string;
  messageVersion?: number;
  contractVersion?: number;
  contractSha256?: string;
  runId?: string;
  phaseId?: string;
  source?: string;
  skipModelsCheck: boolean;
  envFile?: string;
  toPhase?: string;
  /** Plan 06i: `tt defer --test <name>` / `--owner-ruling <id>`, the guard. */
  deferTest?: string;
  deferOwnerRuling?: string;
} {
  const positional: string[] = [];
  let root: string | undefined;
  let json = false;
  let skipModelsCheck = false;
  let envFile: string | undefined;
  let reason: string | undefined;
  let toPhase: string | undefined;
  let candidateSha: string | undefined;
  let messageVersion: number | undefined;
  let contractVersion: number | undefined;
  let contractSha256: string | undefined;
  let runId: string | undefined;
  let phaseId: string | undefined;
  let source: string | undefined;
  let deferTest: string | undefined;
  let deferOwnerRuling: string | undefined;
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--root") {
      root = argv[++i];
    } else if (arg === "--json") {
      json = true;
    } else if (arg === "--reason") {
      reason = argv[++i];
    } else if (arg.startsWith("--reason=")) {
      reason = arg.slice("--reason=".length);
    } else if (arg === "--candidate-sha") {
      candidateSha = argv[++i];
    } else if (arg.startsWith("--candidate-sha=")) {
      candidateSha = arg.slice("--candidate-sha=".length);
    } else if (arg === "--message-version") {
      messageVersion = Number(argv[++i]);
    } else if (arg.startsWith("--message-version=")) {
      messageVersion = Number(arg.slice("--message-version=".length));
    } else if (arg === "--contract-version") {
      contractVersion = Number(argv[++i]);
    } else if (arg.startsWith("--contract-version=")) {
      contractVersion = Number(arg.slice("--contract-version=".length));
    } else if (arg === "--contract-sha256") {
      contractSha256 = argv[++i];
    } else if (arg.startsWith("--contract-sha256=")) {
      contractSha256 = arg.slice("--contract-sha256=".length);
    } else if (arg === "--run-id") {
      runId = argv[++i];
    } else if (arg.startsWith("--run-id=")) {
      runId = arg.slice("--run-id=".length);
    } else if (arg === "--phase-id") {
      phaseId = argv[++i];
    } else if (arg.startsWith("--phase-id=")) {
      phaseId = arg.slice("--phase-id=".length);
    } else if (arg === "--source") {
      source = argv[++i];
    } else if (arg.startsWith("--source=")) {
      source = arg.slice("--source=".length);
    } else if (arg === "--skip-models-check") {
      skipModelsCheck = true;
    } else if (arg === "--env-file") {
      envFile = argv[++i];
    } else if (arg.startsWith("--env-file=")) {
      envFile = arg.slice("--env-file=".length);
    } else if (arg === "--to") {
      toPhase = argv[++i];
    } else if (arg.startsWith("--to=")) {
      toPhase = arg.slice("--to=".length);
    } else if (arg === "--test") {
      deferTest = argv[++i];
    } else if (arg.startsWith("--test=")) {
      deferTest = arg.slice("--test=".length);
    } else if (arg === "--owner-ruling") {
      deferOwnerRuling = argv[++i];
    } else if (arg.startsWith("--owner-ruling=")) {
      deferOwnerRuling = arg.slice("--owner-ruling=".length);
    } else {
      positional.push(argv[i]);
    }
  }
  return { positional, root, json, reason, candidateSha, messageVersion, contractVersion, contractSha256, runId, phaseId, source, skipModelsCheck, envFile, toPhase, deferTest, deferOwnerRuling };
}

/** Plan 03c: resolve a readable id `<program>-NN` to the node's run
 * directory. NN is the node's position in the program file, so the id survives
 * a retry (which starts a fresh run for the same position). */
function resolveReadableRunDir(ref: string, root: string): string | undefined {
  const m = ref.match(/^(.+)-([0-9]+)$/);
  if (!m) return undefined;
  const dir = path.join(programsRoot(root), m[1]);
  if (!existsSync(path.join(dir, "program.json"))) return undefined;
  try {
    const { nodes, state } = foldProgram(dir);
    const node = nodes[Number(m[2]) - 1];
    const runId = node ? state.nodes[node.id]?.runId : undefined;
    if (!runId) return undefined;
    const runDir = path.join(root, runId);
    return existsSync(path.join(runDir, "meta.json")) ? runDir : undefined;
  } catch {
    return undefined;
  }
}

/** A4 (plan 06e): the ONE place that chooses a runner for a run — the
 * recorded revision's installed copy when it differs from this checkout,
 * otherwise this checkout. `status`, `program status`, the program review and
 * the scheduler all read through it. Declared here, at the CLI boundary, as
 * the architecture names; `effects/runner.ts` reads a run through the chooser
 * this file registers. */
export function runnerFor(runDir: string, root: string): RunnerChoice {
  const mine = runnerRevision();
  const recorded = recordedRunnerRevision(runDir);
  if (recorded === undefined || recorded === mine) return currentChoice();
  const installed = installedRunnerRoot(root, recorded);
  if (installed === undefined) return currentChoice();
  return { revision: recorded, packageRoot: installed, current: false, cliPath: path.join(installed, "src", "cli.ts") };
}

// program.ts reads node runs through `effects/runner.ts`, which cannot import
// this file (it runs main() on import), so the one chooser is registered here.
setRunnerChooser(runnerFor);

/** A4 (plan 06e): read a run with the runner that recorded it, when that
 * runner is installed. `runnerFor` is the only chooser; this re-execs the
 * chosen runner's own CLI with the same argv and exits with its status, so a
 * run recorded under an older installed revision is served by that
 * revision's code (its status text, its `state` JSON). `TT_RUNNER_SERVED=1`
 * stops a second hop: the chosen runner answers with its own code. */
function delegateToRecordedRunner(runDir: string, root: string): void {
  if (process.env.TT_RUNNER_SERVED === "1") return;
  const choice = runnerFor(runDir, root);
  if (choice.current) return;
  const result = spawnSync(process.execPath, [choice.cliPath, ...process.argv.slice(2)], {
    stdio: "inherit",
    env: { ...process.env, TT_RUNNER_SERVED: "1" },
  });
  process.exit(result.status ?? 1);
}

function resolveRunDir(rootOrId: string, root: string): string {
  if (existsSync(path.join(rootOrId, "events.jsonl")) || existsSync(rootOrId)) {
    // Looks like a path already (absolute or relative run dir).
    if (existsSync(path.join(rootOrId, "meta.json"))) return path.resolve(rootOrId);
  }
  const candidate = path.join(root, rootOrId);
  if (existsSync(path.join(candidate, "meta.json"))) return candidate;
  const readable = resolveReadableRunDir(rootOrId, root);
  if (readable) return readable;
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
    const plan = readPlan(runDir);
    return resolveSecrets(secretNames(plan.secrets), process.env, plan.envFile).maskable;
  } catch {
    return [];
  }
}

/** A conductor resolves its declared secrets from its OWN environment, once,
 * at start. Starting one from a shell that lacks them used to run every node
 * keyless for hours (plan 14: `tt program resume` from a shell without the
 * vendor keys; the worker could not run the live gate and deferred it). So a
 * start or resume that would launch a conductor refuses, naming what is
 * missing, unless TT_ALLOW_MISSING_SECRETS=1. Plan 06e (A1): a declared name
 * is satisfied by the environment first, then by the env file, so the refusal
 * names the variable and every source it checked. Names only; never a value. */
function refuseMissingSecrets(
  declared: readonly (string | undefined)[] | undefined,
  what: string,
  envFile?: string,
): boolean {
  const names = [...new Set(secretNames((declared ?? []).filter((n): n is string => typeof n === "string")))];
  const { missing } = resolveSecrets(names, process.env, envFile);
  if (missing.length === 0 || process.env.TT_ALLOW_MISSING_SECRETS === "1") return false;
  const where = envFile === undefined ? "this environment" : `this environment or the env file ${envFile}`;
  process.stderr.write(
    `refusing to ${what}: declared secret(s) not set in ${where}: ${missing.join(", ")}\n` +
      `export them in the shell (or Emacs) that runs this command, add them to the env file, or set TT_ALLOW_MISSING_SECRETS=1 to run without them\n`,
  );
  process.exitCode = 1;
  return true;
}

/** The plans of a program's entries whose node has not finished (a done node
 * can never start again, so its secrets are no longer required). */
function programActivePlans(dir: string): RunPlanFile[] {
  const { program, state } = foldProgram(dir);
  return program.entries.filter((e) => state.nodes[e.id]?.status !== "done").map((e) => e.plan);
}

/** Plan 06e (A1): the same refusal for a program, whose entries may each name
 * their own env file. A name missing from every source is refused once, with
 * the sources named. */
function refuseMissingSecretsForPlans(plans: readonly RunPlanFile[], what: string, envFile?: string): boolean {
  const missing = new Set<string>();
  const sources = new Set<string>();
  for (const plan of plans) {
    const file = envFile ?? plan.envFile;
    sources.add(file === undefined ? "this environment" : `the env file ${file}`);
    const { missing: m } = resolveSecrets(secretNames(plan.secrets), process.env, file);
    for (const name of m) missing.add(name);
  }
  if (missing.size === 0 || process.env.TT_ALLOW_MISSING_SECRETS === "1") return false;
  process.stderr.write(
    `refusing to ${what}: declared secret(s) not set in ${[...sources].join(" or ")}: ${[...missing].join(", ")}\n` +
      `export them in the shell (or Emacs) that runs this command, add them to the env file, or set TT_ALLOW_MISSING_SECRETS=1 to run without them\n`,
  );
  process.exitCode = 1;
  return true;
}

/** Plan 05i: resolve one executable in this caller's own environment with
 * `command -v`, exactly as the shell that would run a node's checks does. */
function resolveExecutable(name: string, env: NodeJS.ProcessEnv, cwd: string): string | undefined {
  // `command -v` does not expand a tilde, so do it here (A-3).
  const target = name.startsWith("~/") ? path.join(os.homedir(), name.slice(2)) : name;
  try {
    const out = execFileSync("/bin/sh", ["-c", 'command -v -- "$1"', "tt-env-preflight", target], {
      encoding: "utf8",
      env,
      cwd: existsSync(cwd) ? cwd : undefined,
      stdio: ["ignore", "pipe", "ignore"],
    }).trim();
    return out.length > 0 ? out.split("\n")[0] : undefined;
  } catch {
    return undefined;
  }
}

/** Plan 05i: every command a plan could run for any of its phases. */
function planPreflightCommands(plan: RunPlanFile): string[] {
  const commands: string[] = [];
  for (const phase of plan.phases) {
    commands.push(...effectiveChecks(plan.checks, phase.checks));
    if (phase.gate) commands.push(phase.gate);
    if (phase.gateCleanup) commands.push(phase.gateCleanup);
  }
  return commands;
}

/** Plan 05i: refuse a program command when a node it would start needs a
 * command this caller's PATH cannot resolve — naming the missing tools, and
 * before any run is created. Mirrors `refuseMissingSecrets`'s honest, visible
 * refusal. */
function refuseMissingTools(plans: readonly RunPlanFile[], what: string): boolean {
  const missing = new Set<string>();
  let pathValue = process.env.PATH ?? "";
  for (const plan of plans) {
    const result = envPreflight(planPreflightCommands(plan), {
      path: process.env.PATH ?? "",
      resolve: (name) => resolveExecutable(name, process.env, plan.repo),
    });
    pathValue = result.path;
    for (const name of result.missing) missing.add(name);
  }
  if (missing.size === 0) return false;
  process.stderr.write(
    `refusing to ${what}: command(s) not on PATH: ${[...missing].join(", ")}\n` +
      `PATH: ${pathValue}\n` +
      `fix the PATH in the shell (or Emacs) that runs this command, then retry\n`,
  );
  process.exitCode = 1;
  return true;
}

/** 06a finding #24: the print-mode Pi command — the real `pi` binary, or the
 * `TT_TEST_MODE=1` fake-pi injection the conductor already uses. The probe's
 * 60 s bound is overridable only under `TT_TEST_MODE=1`, so a unit test can
 * exercise `unreachable` without waiting a minute. */
function modelsCheckOptions(): { command: string; argsPrefix: string[]; timeoutMs?: number } {
  const { piCommand, piArgsPrefix } = testPiInjection();
  let timeoutMs: number | undefined;
  if (process.env.TT_TEST_MODE === "1") {
    const raw = Number(process.env.TT_MODELS_CHECK_TIMEOUT_MS);
    if (Number.isFinite(raw) && raw > 0) timeoutMs = raw;
  }
  return { command: piCommand ?? "pi", argsPrefix: piArgsPrefix ?? [], ...(timeoutMs !== undefined ? { timeoutMs } : {}) };
}

/** The distinct configured models of a plan (empty when it declares none). */
async function runPlanModelsCheck(plan: RunPlanFile): Promise<ModelsCheck> {
  return runModelsCheck(distinctModelGroups(planModelTargets(plan.models, plan.seats)), modelsCheckOptions());
}

/** The distinct configured models across every entry of a program. */
async function runProgramModelsCheck(program: ProgramFile): Promise<ModelsCheck> {
  return runModelsCheck(distinctModelGroups(program.entries.flatMap((e) => planModelTargets(e.plan.models, e.plan.seats))), modelsCheckOptions());
}

/** Write the check's record next to the run/program it belongs to. */
function writeModelsCheck(dir: string, check: ModelsCheck): void {
  writeFileSync(path.join(dir, "models-check.json"), `${JSON.stringify(check, null, 2)}\n`);
}

/** 06a finding #24: refuse a start when a configured model is refused, naming
 * the gateway's own message. `--skip-models-check` bypasses this. */
function refuseRefusedModels(check: ModelsCheck, what: string): boolean {
  const refused = modelsCheckRefused(check);
  if (refused.length === 0) return false;
  const list = refused.map((p) => `${p.key}${p.message ? ` (${p.message})` : ""}`).join(", ");
  process.stderr.write(
    `refusing to ${what}: configured model(s) refused by the gateway: ${list}\n` +
      `fix #+TT_MODELS, or pass --skip-models-check to start anyway\n`,
  );
  process.exitCode = 1;
  return true;
}

/** 06a finding #24: `tt models check [plan-or-program]`. Prints one line per
 * distinct configured model; exits non-zero when any is not `ok`. */
async function cmdModels(sub: string | undefined, args: string[]): Promise<void> {
  if (sub !== "check") usage();
  if (args.length !== 1) usage();
  const json = JSON.parse(readFileSync(args[0], "utf8")) as unknown;
  const check = isProgramInput(json) ? await runProgramModelsCheck(json as ProgramFile) : await runPlanModelsCheck(json as RunPlanFile);
  if (check.probes.length === 0) {
    process.stdout.write("no #+TT_MODELS configured; nothing to check\n");
    return;
  }
  for (const p of check.probes) {
    const detail = p.status === "refused" && p.message ? ` (${p.message})` : "";
    process.stdout.write(`${p.key} ${p.status}${detail}\n`);
  }
  // A refused or unreachable model is a failed check; `tt start` itself only
  // refuses on `refused` (an unreachable model may be a transient network
  // problem, not a gateway policy), but `tt models check` exits non-zero for
  // either so a script can branch on it.
  if (check.probes.some((p) => p.status !== "ok")) process.exitCode = 1;
}

async function cmdProgram(sub: string | undefined, args: string[], root: string, json: boolean, source?: string, skipModelsCheck = false, envFile?: string): Promise<void> {
  if (sub === "start") {
    if (args.length !== 1) usage();
    const program = JSON.parse(readFileSync(args[0], "utf8")) as ProgramFile;
    // Plan 06e (A1): --env-file overrides every entry's own #+TT_ENV_FILE, and
    // is recorded in each entry's plan snapshot so a resume resolves it too.
    const resolvedEnvFile = envFile === undefined ? undefined : path.resolve(envFile);
    if (resolvedEnvFile !== undefined) for (const e of program.entries) e.plan.envFile = resolvedEnvFile;
    // Plan 01c: every entry's plan is linted before the scheduler starts. An
    // error refuses the whole program; warnings (including plan 06j's
    // coverage warnings) are printed and it starts.
    const findings = allFindings(program);
    reportFindings(findings, args[0], process.stderr);
    if (hasLintErrors(findings)) {
      process.exitCode = 1;
      return;
    }
    if (refuseMissingSecretsForPlans(program.entries.map((e) => e.plan), "start the program", resolvedEnvFile)) return;
    // Plan 05i: every node's declared commands must resolve in THIS caller's
    // PATH before the program (or any of its runs) exists.
    if (refuseMissingTools(program.entries.map((e) => e.plan), "start the program")) return;
    const dir = createProgram(root, program);
    // Plan 03c: the Org program file the program was started from (Emacs
    // passes it), shown in the program buffer's header.
    if (source) recordProgramSource(dir, source);
    // 06a finding #24: probe every distinct configured model, record the
    // result in the program directory, and refuse before the scheduler starts
    // when one is refused (unless --skip-models-check).
    const check = await runProgramModelsCheck(program);
    writeModelsCheck(dir, check);
    if (!skipModelsCheck && refuseRefusedModels(check, "start the program")) return;
    if (skipModelsCheck && modelsCheckRefused(check).length > 0) {
      process.stderr.write("warning: --skip-models-check: starting with refused model(s)\n");
    }
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
      `${JSON.stringify({
        id: path.basename(dir),
        dir,
        title: program.title,
        sourcePath: readProgramSource(dir) ?? null,
        maxParallel: program.maxParallel,
        nodes,
        state,
        schedulerAlive: programPidAlive(dir),
        lines: programStatusLines(dir),
      })}\n`,
    );
  } else if (sub === "stop") {
    if (args.length !== 1) usage();
    const dir = resolveProgramDir(args[0], root);
    appendProgramEvent(dir, { type: "PROGRAM_STOPPED" });
    try {
      // A2/C3 (plan 06e): the sweep owns the signalling decision.
      signalProcess(Number(readFileSync(programPaths(dir).pid, "utf8")), "SIGTERM");
    } catch {
      // not running
    }
    // Let the scheduler die before recording statuses, so its last tick cannot
    // overwrite the chart this command is about to write (M-7).
    const deadBy = Date.now() + 5_000;
    while (programPidAlive(dir) && Date.now() < deadBy) await new Promise((r) => setTimeout(r, 50));
    const { state } = foldProgram(dir);
    for (const [node, n] of Object.entries(state.nodes)) {
      if (!n.runId || !["running", "needs-you"].includes(n.status)) continue;
      await cmdStop(n.runId, root);
      // The scheduler is gone, so record the stop here: the program status
      // must not keep showing the node as running.
      appendProgramEvent(dir, { type: "NODE_STATUS", node, status: "stopped" });
    }
    // The chart is a projection like the status lines: rewrite it now that
    // every node is stopped, since no scheduler is left to tick (F-A-6/M-7).
    writeProgramChart(dir);
    process.stdout.write(`stopped program ${path.basename(dir)}\n`);
  } else if (sub === "pause") {
    // Plan 06d (A3): a pause is a logged program event, never a kill. The
    // running nodes finish; the scheduler (still alive, or relaunched later)
    // starts nothing new until `resume`. The scheduler keeps watching while a
    // node is active so its finish is observed, then exits reporting paused.
    if (args.length !== 1) usage();
    const dir = resolveProgramDir(args[0], root);
    appendProgramEvent(dir, { type: "PROGRAM_PAUSED" });
    writeProgramChart(dir);
    process.stdout.write(`paused program ${path.basename(dir)} (running nodes finish; no new node starts until resume)\n`);
  } else if (sub === "resume") {
    if (args.length !== 1) usage();
    const dir = resolveProgramDir(args[0], root);
    if (refuseMissingSecretsForPlans(programActivePlans(dir), `resume program ${path.basename(dir)}`)) return;
    // Plan 05i: preflight every node this resume could start — the stopped /
    // crashed runs it restarts now AND the waiting nodes the scheduler will
    // start later — before it starts any of them, so a missing tool refuses
    // visibly rather than launching runs that are immediately
    // environment-blocked. An entry every one of whose nodes is done or
    // blocked can never start again and is not preflighted.
    const { program, nodes, state } = foldProgram(dir);
    const resumePlans = program.entries
      .filter((e) =>
        nodes.some((n) => n.entry === e.id && !["done", "blocked"].includes(state.nodes[n.id]?.status ?? "waiting")),
      )
      .map((e) => e.plan);
    if (refuseMissingTools(resumePlans, `resume program ${path.basename(dir)}`)) return;
    // Undo a stop or pause, restart every node run that is not running
    // (stopped by the owner or crashed), then the scheduler if it is not
    // running.
    if (state.stopped || state.paused) appendProgramEvent(dir, { type: "PROGRAM_RESUMED" });
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
  } else if (sub === "retry") {
    // A blocked or stopped node runs again as a fresh run on its existing
    // branch; the scheduler is (re)started to pick it up.
    if (args.length !== 2) usage();
    const dir = resolveProgramDir(args[0], root);
    const { program, state } = foldProgram(dir);
    const node = state.nodes[args[1]];
    if (!node) {
      process.stderr.write(`no node ${args[1]} in program ${path.basename(dir)}\n`);
      process.exitCode = 1;
      return;
    }
    if (node.status === "done" || node.status === "running") {
      process.stderr.write(`node ${args[1]} is ${node.status}; only a blocked, stopped or needs-you node can be retried\n`);
      process.exitCode = 1;
      return;
    }
    if (refuseMissingSecretsForPlans(programActivePlans(dir), `retry ${args[1]}`)) return;
    // Plan 05i: the retried node's declared commands must resolve now, before
    // its fresh run is created.
    const retryEntry = program.entries.find((e) => e.id === args[1] || args[1].startsWith(`${e.id}/`));
    if (refuseMissingTools(retryEntry ? [retryEntry.plan] : [], `retry ${args[1]}`)) return;
    if (node.runId && conductorAlive(path.join(root, node.runId))) await cmdStop(node.runId, root);
    if (state.stopped) appendProgramEvent(dir, { type: "PROGRAM_RESUMED" });
    appendProgramEvent(dir, { type: "NODE_RETRY", node: args[1] });
    if (!programPidAlive(dir)) launchProgramScheduler(dir);
    process.stdout.write(`retrying ${args[1]} in program ${path.basename(dir)}\n`);
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
    const dirs = ids.map((id) => path.join(programsRoot(root), id));
    if (json) {
      // Plan 01b: Emacs's mode-line reads the oldest wait from here, so a
      // node that needs the owner shows as `⚑ <node> waiting <duration>`.
      // The multi-root picker (this packet) also reads one JSON object per
      // program: its title, aggregate state, node count, the Org file it was
      // started from and its last activity, so choosing a program across
      // roots costs one call per root and never a per-program file read.
      const rows = dirs
        .map((dir) => {
          try {
            const { program, nodes, state } = foldProgram(dir);
            const started = statMtime(programPaths(dir).program);
            return {
              id: path.basename(dir),
              title: program.title,
              state: programOutcome(nodes, state),
              nodeCount: nodes.length,
              source: readProgramSource(dir) ?? null,
              started,
              activity: statMtime(programPaths(dir).events) ?? started ?? 0,
              alive: programPidAlive(dir),
              waiting: programWaitingNodes(dir),
            };
          } catch {
            // A program directory without a readable program.json: skipped,
            // exactly as a run with no meta.json is skipped by `tt list`.
            return undefined;
          }
        })
        .filter((r): r is NonNullable<typeof r> => r !== undefined)
        .sort((a, b) => b.activity - a.activity);
      process.stdout.write(`${JSON.stringify(rows)}\n`);
    } else {
      // Plan 01h: `tt program list` only shows the first two lines, so it
      // skips the per-node cost/trade-off detail (no view is built).
      const rows = dirs.map((dir) => programStatusLines(dir, new Date(), { nodeDetail: false }).slice(0, 2).join(" · "));
      process.stdout.write(rows.map((r) => `${r}\n`).join(""));
    }
  } else {
    usage();
  }
}

async function cmdStart(planPath: string, root: string, skipModelsCheck = false, envFile?: string): Promise<void> {
  const plan = JSON.parse(readFileSync(planPath, "utf8")) as RunPlanFile;
  if (!plan.repo) usage();
  // Plan 06e (A1): --env-file overrides the plan's own #+TT_ENV_FILE. The path
  // is absolute, so the detached conductor (which re-reads the plan snapshot)
  // resolves the same file.
  if (envFile !== undefined) plan.envFile = path.resolve(envFile);
  // Plan 01c: lint before any run exists. An error refuses to start (message
  // on stderr, non-zero exit); warnings (including plan 06j's coverage
  // warnings) are printed and the run starts.
  const findings = allFindings(plan);
  reportFindings(findings, planPath, process.stderr);
  if (hasLintErrors(findings)) {
    process.exitCode = 1;
    return;
  }
  if (refuseMissingSecrets(plan.secrets, "start the run", plan.envFile)) return;
  // 06a finding #24: create the run, probe its distinct configured models,
  // record the result in the run directory, and refuse to launch when one is
  // refused (unless --skip-models-check).
  const runDir = createRun(root, plan);
  const check = await runPlanModelsCheck(plan);
  writeModelsCheck(runDir, check);
  if (!skipModelsCheck && refuseRefusedModels(check, "start the run")) return;
  if (skipModelsCheck && modelsCheckRefused(check).length > 0) {
    process.stderr.write("warning: --skip-models-check: starting with refused model(s)\n");
  }
  launchDetached(runDir);
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
  // #+TT_MODELS: the plan's per-role provider/model reaches every launch here,
  // the one place a run's Conductor is built. `testPiInjection` above still
  // supplies the fake-pi command for a test-launched run; the two do not
  // interact (one picks the binary, the other the model flags).
  const conductor = new Conductor({ runDir, plan, piCommand, piArgsPrefix, providerModelFor: planModelSelector(plan, plan.seats), deadlines, stubReviews, briefs: true });
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
  // A4 (plan 06e): a finished run's projection is a pure function of its
  // immutable log. DONE and BLOCKED are terminal, so the rebuilt phase (and
  // the recorded `views/status.txt` the front end serves as it was written)
  // can never change — `tt status` reads the same outcome on every call.
  const state = rebuildState(runDir, plan, { lenient: true });
  const view = buildView(runDir, plan, conductorAlive(runDir));
  return renderStatusText(runDir, state, view, loggedSecretStatus(runDir), loggedHeld(runDir));
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

/** The mtime of FILE in milliseconds, or undefined when it is missing. */
function statMtime(file: string): number | undefined {
  try {
    return statSync(file).mtimeMs;
  } catch {
    return undefined;
  }
}

/** The readable id (`<program>-NN`) recorded in a program node's run, or
 * undefined for a hand-started run. Read here, on the host that owns the run,
 * so the Emacs picker never has to read the run's files over TRAMP. */
function runReadableId(runDir: string): string | undefined {
  try {
    const info = JSON.parse(readFileSync(path.join(runDir, "program.json"), "utf8")) as { readableId?: unknown };
    return typeof info.readableId === "string" ? info.readableId : undefined;
  } catch {
    return undefined;
  }
}

/** The Org file a run was started from, recorded by Emacs in `emacs.json'.
 * Present in `tt list --json` so resolving a plan's run is a filter over the
 * listing, never one file read per run over TRAMP. */
function runPlanPath(runDir: string): string | undefined {
  try {
    const info = JSON.parse(readFileSync(path.join(runDir, "emacs.json"), "utf8")) as { planPath?: unknown };
    return typeof info.planPath === "string" ? info.planPath : undefined;
  } catch {
    return undefined;
  }
}

/** Plan 06f (A1): one run's row, exactly as `tt list` emits it. */
interface ListRow {
  id: string;
  runDir: string;
  readableId: string | null;
  planPath: string | null;
  title: string;
  phase: string;
  stage: string;
  stageElapsed: string;
  elapsed: string;
  reviews: string;
  attention: string | null;
  needsYou: number;
  alive: boolean;
  activity: number;
}

/** The expensive part of a list row: read `meta.json`, fold the run's log
 * (`buildView`) and redact what it carries. Only called on a cache miss. */
function buildListRow(runDir: string, id: string, activity: number): ListRow {
  const alive = conductorAlive(runDir);
  const meta = JSON.parse(readFileSync(runPaths(runDir).meta, "utf8")) as { title?: string };
  const v = buildView(runDir, readPlan(runDir), alive);
  // Plan 01a: a title or an attention line can quote a value.
  const secrets = secretsForRun(runDir);
  return {
    id,
    runDir,
    readableId: runReadableId(runDir) ?? null,
    planPath: runPlanPath(runDir) ?? null,
    title: redactText(meta.title ?? "", secrets),
    phase: v.timeline.state.phase.phase,
    stage: v.stage,
    stageElapsed: v.stageElapsed,
    elapsed: v.elapsed,
    reviews: v.reviewLine,
    attention: v.attention ? redactText(v.attention, secrets) : null,
    needsYou: v.needsYou,
    alive,
    activity,
  };
}

/** Plan 06f (A1): the ONE reader and writer of `<run>/views/row.json`.
 * `row.json` holds `{ key: { size, mtimeMs }, row }`, keyed by the size and
 * mtime of the run's `events.jsonl`. The row is rebuilt (and the file
 * rewritten) only when that key differs from the log's current stat, so a
 * listing with nothing changed pays one stat and one small file read per
 * run instead of folding every log. Returns undefined for a run with no
 * `events.jsonl`, exactly as the pre-cache listing skipped it. */
function listRow(runDir: string, id: string): ListRow | undefined {
  const events = runPaths(runDir).events;
  let size: number;
  let mtimeMs: number;
  try {
    const st = statSync(events);
    size = st.size;
    mtimeMs = st.mtimeMs;
  } catch {
    return undefined;
  }
  const rowFile = path.join(runPaths(runDir).views, "row.json");
  try {
    const cached = JSON.parse(readFileSync(rowFile, "utf8")) as {
      key?: { size?: number; mtimeMs?: number };
      row?: ListRow;
    };
    if (cached.key?.size === size && cached.key?.mtimeMs === mtimeMs && cached.row) return cached.row;
  } catch {
    // no readable cache: rebuild below
  }
  let row: ListRow;
  try {
    row = buildListRow(runDir, id, mtimeMs);
  } catch {
    return undefined;
  }
  try {
    mkdirSync(path.dirname(rowFile), { recursive: true });
    const tmp = `${rowFile}.${process.pid}.tmp`;
    writeFileSync(tmp, JSON.stringify({ key: { size, mtimeMs }, row }));
    renameSync(tmp, rowFile);
  } catch {
    // a cache that cannot be written must never stop the listing
  }
  return row;
}

/** Plan 3b: `tt list` — one line per run (newest activity first), the
 * summary the Emacs runs list renders. Plan 06f (A1): each row comes from
 * `listRow`, cached by its `events.jsonl` size and mtime. */
function cmdList(root: string, json: boolean): void {
  let names: string[] = [];
  try {
    names = readdirSync(root).filter((n) => existsSync(path.join(root, n, "meta.json")));
  } catch {
    names = [];
  }
  const rows = names
    .map((n) => listRow(path.join(root, n), n))
    .filter((r): r is ListRow => r !== undefined)
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
  return processAlive(pid);
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
  signalProcess(pid, "SIGTERM");
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

/** Contract v1: `tt contract rebuild|check <run>`. The projections are
 * rebuilt from `events.jsonl` (state), never the other way round. */
function cmdContract(sub: string | undefined, runDir: string): void {
  const p = runPaths(runDir);
  const plan = readPlan(runDir);
  const state = rebuildState(runDir, plan, { lenient: true });
  const messages = projectMessages(state.phase);
  const ledger = projectLedger(state.phase);
  // Plan 05j: review.org is the entry projection (one topic once), linted on
  // every render. `views/entries/<id>.org` is each live entry's own file.
  const entryReview = projectEntryReview(
    { ...state.phase, ...runIds(runDir) },
    { anchorFreshness: candidateAnchorFreshness(state.phase.candidate ? path.join(p.candidates, state.phase.candidate.sha) : undefined) },
  );
  const review = entryReview.text;
  const messageFiles = reviewMessageFiles(state.phase);
  // Plan 04c: `views/metrics.json` is a projection of state and the log.
  const { timeline, events } = rebuildTimelineWithEvents(runDir, plan);
  const metrics = projectMetrics(metricsForRunDir(runDir, state.phase, timeline, events));
  // Plan 05h: the loop tape is a projection too. `buildView` builds it from
  // the same log read, and the conductor redacts what it writes, so the
  // expected bytes are redacted the same way before the comparison.
  const view = buildView(runDir, plan, false);
  const tape = redactText(view.tape, secretsForRun(runDir));
  if (sub === "rebuild") {
    writeFileSync(p.messages, messages);
    writeFileSync(p.ledger, ledger);
    writeFileSync(p.review, review);
    writeFileSync(p.metrics, metrics);
    writeFileSync(p.tape, tape);
    mkdirSync(p.messagesView, { recursive: true });
    const ids = new Set(messageFiles.map((f) => f.id));
    for (const f of messageFiles) writeFileSync(path.join(p.messagesView, `${f.id}.org`), f.contents);
    // B-12/A-14: rebuild is a projection of state, so it also removes a
    // message file whose id state no longer has (a superseded id).
    for (const name of readdirSync(p.messagesView)) {
      if (name.endsWith(".org") && !ids.has(name.slice(0, -4))) rmSync(path.join(p.messagesView, name), { force: true });
    }
    // Plan 05j: one file per live entry, pruned the same way.
    mkdirSync(p.entriesView, { recursive: true });
    const entryIds = new Set(entryReview.files.map((f) => f.id));
    for (const f of entryReview.files) writeFileSync(path.join(p.entriesView, `${f.id}.org`), f.contents);
    for (const name of readdirSync(p.entriesView)) {
      if (name.endsWith(".org") && !entryIds.has(name.slice(0, -4))) rmSync(path.join(p.entriesView, name), { force: true });
    }
    // Plan 06b: the item evidence files the matrix cells open.
    mkdirSync(p.itemsView, { recursive: true });
    const itemIds = new Set(entryReview.itemFiles.map((f) => f.id));
    for (const f of entryReview.itemFiles) writeFileSync(path.join(p.itemsView, `${f.id}.org`), f.contents);
    for (const name of readdirSync(p.itemsView)) {
      if (name.endsWith(".org") && !itemIds.has(name.slice(0, -4))) rmSync(path.join(p.itemsView, name), { force: true });
    }
    // Plan 05j: a render that named a violation records it (findings A-18,
    // B-13). `check` never mutates, only `rebuild`.
    recordLintFailures(runDir, entryReview.lint);
    process.stdout.write(`rebuilt ${path.basename(runDir)}: messages.jsonl, ledger.jsonl, views/review.org, views/entries/, views/messages/, views/metrics.json, views/tape.txt\n`);
    return;
  }
  if (sub !== "check") usage();
  const actualMessages = existsSync(p.messages) ? readFileSync(p.messages, "utf8") : "";
  const actualLedger = existsSync(p.ledger) ? readFileSync(p.ledger, "utf8") : "";
  const actualReview = existsSync(p.review) ? readFileSync(p.review, "utf8") : "";
  const actualMetrics = existsSync(p.metrics) ? readFileSync(p.metrics, "utf8") : "";
  const actualTape = existsSync(p.tape) ? readFileSync(p.tape, "utf8") : "";
  const mismatches: string[] = [];
  if (actualMessages !== messages) mismatches.push("messages.jsonl");
  if (actualLedger !== ledger) mismatches.push("ledger.jsonl");
  if (actualReview !== review) mismatches.push("views/review.org");
  if (actualMetrics !== metrics) mismatches.push("views/metrics.json");
  if (actualTape !== tape) mismatches.push("views/tape.txt");
  for (const f of messageFiles) {
    const file = path.join(p.messagesView, `${f.id}.org`);
    const actual = existsSync(file) ? readFileSync(file, "utf8") : "";
    if (actual !== f.contents) mismatches.push(`views/messages/${f.id}.org`);
  }
  for (const f of entryReview.files) {
    const file = path.join(p.entriesView, `${f.id}.org`);
    const actual = existsSync(file) ? readFileSync(file, "utf8") : "";
    if (actual !== f.contents) mismatches.push(`views/entries/${f.id}.org`);
  }
  // B-12/A-14: an extra file for an id state no longer has is a mismatch, so
  // `check` sees the same stale file `rebuild` now prunes.
  if (existsSync(p.messagesView)) {
    const expected = new Set(messageFiles.map((f) => f.id));
    for (const name of readdirSync(p.messagesView)) {
      if (name.endsWith(".org") && !expected.has(name.slice(0, -4))) mismatches.push(`views/messages/${name}`);
    }
  }
  if (mismatches.length > 0) {
    process.stderr.write(
      `contract check failed for ${path.basename(runDir)}: ${mismatches.join(", ")} do not match state; run \`tt contract rebuild ${path.basename(runDir)}\`\n`,
    );
    process.exitCode = 1;
    return;
  }
  process.stdout.write(`contract check ok for ${path.basename(runDir)}\n`);
}

/** A-13: a live `tt verdict` reports the OUTCOME. The command is written to
 * the inbox, then this waits for the conductor to move it to
 * `inbox/applied` or `inbox/rejected` and returns which, with the reason.
 * 20 s (up from 5 s): under the whole suite's four-way load the conductor's
 * inbox poll can lag far past 5 s, and a live run's verdict then read
 * `queued` even though the conductor was about to apply it. A genuinely
 * stopped conductor still returns `queued` after the window. */
async function awaitInboxVerdict(
  runDir: string,
  commandId: string,
  timeoutMs: number,
): Promise<{ kind: "applied" | "rejected" | "pending"; reason?: string }> {
  const p = runPaths(runDir);
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const reasonFile = path.join(p.inboxRejected, `${commandId}.reason.txt`);
    if (existsSync(reasonFile)) return { kind: "rejected", reason: readFileSync(reasonFile, "utf8").trim() };
    if (existsSync(path.join(p.inboxApplied, `${commandId}.json`))) return { kind: "applied" };
    if (Date.now() >= deadline) return { kind: "pending" };
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
}

/** Plan 06b: `tt evidence <run> <item> <file-or-text>` — the owner records an
 * `evidence` item. A file path is read; any other argument is the text
 * itself. Written through the inbox, so the conductor validates the item and
 * resumes the parked phase. */
async function cmdEvidence(positional: string[], root: string): Promise<void> {
  const [runArg, item, ...rest] = positional;
  const arg = rest.join(" ");
  const runDir = resolveRunDir(runArg, root);
  let text = arg;
  try {
    if (arg.length > 0 && existsSync(arg) && statSync(arg).isFile()) text = readFileSync(arg, "utf8");
  } catch {
    // keep the literal argument
  }
  const inbox = path.join(runDir, "inbox");
  mkdirSync(inbox, { recursive: true });
  const commandId = `evidence-${Date.now().toString(36)}-${randomUUID().slice(0, 6)}`;
  writeFileSync(path.join(inbox, `${commandId}.json`), JSON.stringify({ type: "evidence", item, text }, null, 2));
  const outcome = await awaitInboxVerdict(runDir, commandId, 20000);
  if (outcome.kind === "applied") {
    process.stdout.write(`evidence recorded for ${item} in run ${path.basename(runDir)}\n`);
  } else if (outcome.kind === "rejected") {
    process.stdout.write(`evidence rejected: ${outcome.reason}\n`);
    process.exitCode = 1;
  } else {
    process.stdout.write(`queued evidence ${commandId} for ${item} (queued, not yet applied)\n`);
  }
}

async function cmdVerdict(
  positional: string[],
  root: string,
  reason: string | undefined,
  overrides: {
    candidateSha?: string;
    messageVersion?: number;
    contractVersion?: number;
    contractSha256?: string;
    runId?: string;
    phaseId?: string;
  } = {},
): void {
  const runDir = resolveRunDir(positional[0], root);
  const messageId = positional[1];
  const verdict = positional[2] as "accept" | "refuse" | undefined;
  if (verdict !== "accept" && verdict !== "refuse") usage();
  const plan = readPlan(runDir);
  const state = rebuildState(runDir, plan, { lenient: true });
  const message = (state.phase.messages ?? []).find((m) => m.id === messageId);
  if (!message) {
    process.stdout.write(`verdict rejected: no message ${messageId} in run ${path.basename(runDir)}\n`);
    process.exitCode = 1;
    return;
  }
  // `--candidate-sha`/`--message-version`/`--contract-version`/… let the
  // caller send the binding it actually saw (e.g. after a carry), so a stale
  // verdict is expressible and is rejected by the same path as any other
  // stale command. The binding the heading carries is the full tuple.
  const boundCandidateSha = overrides.candidateSha ?? message.boundCandidateSha;
  const boundRecordVersion = overrides.messageVersion ?? message.messageVersion;
  const boundContractVersion = {
    ...message.boundContractVersion,
    ...(overrides.contractVersion !== undefined ? { snapshot: overrides.contractVersion } : {}),
    ...(overrides.contractSha256 !== undefined ? { sectionSha256: overrides.contractSha256 } : {}),
  };
  let boundRunId = overrides.runId ?? state.phase.runId;
  const boundPhaseId = overrides.phaseId ?? state.phase.phaseId;
  // The heading carries the full binding. Plan 06d (A2/C3): resolveBinding is
  // the one matcher, so the run may be named by its internal id, its
  // directory id or its readable id. An id that names nothing is refused with
  // all three that would have bound — never silently dropped when the daemon
  // has exited, and never accepted as a different run.
  if (overrides.runId !== undefined && overrides.runId !== "") {
    const resolved = resolveBinding(
      {
        runId: state.phase.runId,
        dirId: path.basename(runDir),
        ...(runReadableId(runDir) ? { readableId: runReadableId(runDir)! } : {}),
      },
      overrides.runId,
    );
    if (!resolved.ok) {
      process.stdout.write(`verdict rejected: ${resolved.reason}\n`);
      process.exitCode = 1;
      return;
    }
    boundRunId = resolved.runId;
  }
  if (overrides.phaseId !== undefined && overrides.phaseId !== "" && overrides.phaseId !== state.phase.phaseId) {
    process.stdout.write(`verdict rejected: phase id ${overrides.phaseId} is not this phase (${state.phase.phaseId})\n`);
    process.exitCode = 1;
    return;
  }
  const withReason = reason !== undefined && reason.trim().length > 0 ? { reason } : {};
  const event = {
    type: "OWNER_VERDICT" as const,
    messageId,
    verdict,
    ...withReason,
    boundCandidateSha,
    boundContractVersion,
    boundRecordVersion,
  };
  if (conductorAlive(runDir)) {
    // Live run: through the inbox, so the conductor checks the binding. The
    // CLI then waits for the outcome (A-13), so the front end shows the
    // rejection reason rather than only `queued'.
    const inbox = path.join(runDir, "inbox");
    mkdirSync(inbox, { recursive: true });
    const commandId = `verdict-${Date.now().toString(36)}-${randomUUID().slice(0, 6)}`;
    const command = {
      type: "verdict",
      verdict,
      ...withReason,
      binding: {
        runId: boundRunId,
        phaseId: boundPhaseId,
        candidateSha: boundCandidateSha,
        contractVersion: boundContractVersion,
        recordId: messageId,
        recordVersion: boundRecordVersion,
      },
    };
    writeFileSync(path.join(inbox, `${commandId}.json`), JSON.stringify(command, null, 2));
    const outcome = await awaitInboxVerdict(runDir, commandId, 20000);
    if (outcome.kind === "applied") {
      process.stdout.write(`verdict applied: ${verdict} recorded for ${messageId} in run ${path.basename(runDir)}\n`);
    } else if (outcome.kind === "rejected") {
      process.stdout.write(`verdict rejected: ${outcome.reason}\n`);
      process.exitCode = 1;
    } else {
      process.stdout.write(`queued verdict ${commandId} on ${messageId} for run ${path.basename(runDir)} (queued, not yet applied)\n`);
    }
    return;
  }
  // No live conductor: this is a late verdict. Validate it against a dry-run
  // reduce first, then append it to the authoritative log and refresh the
  // projections.
  const result = reduce(state, event);
  if (!result.ok) {
    // A-13: the rejection reason is the command's outcome, so print it on
    // stdout; the non-zero exit is what tells the front end it failed.
    process.stdout.write(`verdict rejected: ${result.reason}\n`);
    process.exitCode = 1;
    return;
  }
  const p = runPaths(runDir);
  const maskable = resolveSecrets(secretNames(plan.secrets)).maskable;
  const log = new EventLog(p.events, maskable);
  try {
    log.append("event", event);
  } finally {
    log.close();
  }
  const after = rebuildState(runDir, plan, { lenient: true });
  writeFileSync(p.messages, projectMessages(after.phase));
  writeFileSync(p.ledger, projectLedger(after.phase));
  const lateEntryReview = projectEntryReview(
    { ...after.phase, ...runIds(runDir) },
    { anchorFreshness: candidateAnchorFreshness(after.phase.candidate ? path.join(p.candidates, after.phase.candidate.sha) : undefined) },
  );
  recordLintFailures(runDir, lateEntryReview.lint);
  writeFileSync(p.review, lateEntryReview.text);
  mkdirSync(p.entriesView, { recursive: true });
  const lateEntryIds = new Set(lateEntryReview.files.map((f) => f.id));
  for (const f of lateEntryReview.files) writeFileSync(path.join(p.entriesView, `${f.id}.org`), f.contents);
  for (const name of readdirSync(p.entriesView)) {
    if (name.endsWith(".org") && !lateEntryIds.has(name.slice(0, -4))) rmSync(path.join(p.entriesView, name), { force: true });
  }
  const lateTimeline = rebuildTimelineWithEvents(runDir, plan);
  writeFileSync(p.metrics, projectMetrics(metricsForRunDir(runDir, after.phase, lateTimeline.timeline, lateTimeline.events)));
  // A run from before this view has no views/messages/: create it, and prune
  // ids the state no longer has (same as the conductor and the rebuild path).
  const files = reviewMessageFiles(after.phase);
  const ids = new Set(files.map((f) => f.id));
  mkdirSync(p.messagesView, { recursive: true });
  for (const f of files) writeFileSync(path.join(p.messagesView, `${f.id}.org`), f.contents);
  for (const name of readdirSync(p.messagesView)) {
    if (name.endsWith(".org") && !ids.has(name.slice(0, -4))) rmSync(path.join(p.messagesView, name), { force: true });
  }
  // Plan 03b: the status view is refreshed too, so the Emacs status buffer
  // never shows a message the review buffer already has. Plan 05h: the loop
  // tape is refreshed on the same beat, so `tt contract check` stays green
  // after a late verdict (the tape is a projection too).
  const lateView = buildView(runDir, plan, false);
  writeFileSync(
    p.status,
    redactText(
      renderStatusView(
        statusViewInput({
          runDir,
          plan,
          state: after,
          view: lateView,
          alive: false,
          secrets: loggedSecretStatus(runDir),
          held: loggedHeld(runDir),
        }),
      ),
      maskable,
    ),
  );
  writeFileSync(p.tape, redactText(lateView.tape, maskable));
  process.stdout.write(`recorded ${verdict} for ${messageId} in run ${path.basename(runDir)}\n`);
}

/** Plan 06g (A5): `tt carry <run> <id> --to <phase-id>` — the owner carries a
 * finding or message into a later phase's plan. Through the inbox, as every
 * owner command does: the conductor's owner-commands.ts decides what a carry
 * does (mark the item carried with its target; once no blocking item remains
 * uncarried, accept the candidate). The binding the heading would carry is
 * the record's own current one, so a stale carry is refused by the same path
 * as any other stale command. */
async function cmdCarry(positional: string[], root: string, toPhase: string | undefined): Promise<void> {
  const runDir = resolveRunDir(positional[0], root);
  const recordId = positional[1];
  if (!recordId) usage();
  if (!toPhase || toPhase.trim().length === 0) {
    process.stdout.write(`carry rejected: a carry needs --to <phase-id>\n`);
    process.exitCode = 1;
    return;
  }
  const plan = readPlan(runDir);
  const state = rebuildState(runDir, plan, { lenient: true });
  const finding = state.phase.findings.find((f) => f.id === recordId);
  const message = (state.phase.messages ?? []).find((m) => m.id === recordId);
  if (!finding && !message) {
    process.stdout.write(`carry rejected: no finding or message ${recordId} in run ${path.basename(runDir)}\n`);
    process.exitCode = 1;
    return;
  }
  // OD addendum A: a second owner act on the same id is refused, naming the
  // existing act. A deferral already put this item's fix off; carrying it too
  // would silently drop one act, so it is refused here (and by the conductor
  // for a hand-written inbox file).
  const existingDefer = (state.phase.deferrals ?? []).find((d) => d.itemId === recordId && d.status === "open");
  if (existingDefer) {
    process.stdout.write(`carry rejected: ${recordId} is already deferred (${existingDefer.id}); resolve or drop that deferral first\n`);
    process.exitCode = 1;
    return;
  }
  const recordKind = finding ? "finding" : "message";
  // Like every record command, the binding is the PHASE's current candidate
  // and contract plus the record's own version (owner-commands.ts's
  // checkItemCarried matches it the same way).
  const boundCandidateSha = state.phase.candidate?.sha ?? "";
  const boundContractVersion = state.phase.contract.contractVersion;
  const boundRecordVersion = finding ? finding.version : message!.messageVersion;
  const inbox = path.join(runDir, "inbox");
  mkdirSync(inbox, { recursive: true });
  const commandId = `carry-${Date.now().toString(36)}-${randomUUID().slice(0, 6)}`;
  const command = {
    type: "carry",
    recordKind,
    toPhase: toPhase.trim(),
    binding: {
      runId: state.phase.runId,
      phaseId: state.phase.phaseId,
      candidateSha: boundCandidateSha,
      contractVersion: boundContractVersion,
      recordId,
      recordVersion: boundRecordVersion,
    },
  };
  writeFileSync(path.join(inbox, `${commandId}.json`), JSON.stringify(command, null, 2));
  const outcome = await awaitInboxVerdict(runDir, commandId, 20000);
  if (outcome.kind === "applied") {
    process.stdout.write(`carried ${recordId} to ${toPhase.trim()} in run ${path.basename(runDir)}\n`);
  } else if (outcome.kind === "rejected") {
    process.stdout.write(`carry rejected: ${outcome.reason}\n`);
    process.exitCode = 1;
  } else {
    process.stdout.write(`queued carry ${commandId} for ${recordId} (queued, not yet applied)\n`);
  }
}

/** Plan 06i: `tt defer <run> <id> [--test <name>] [--owner-ruling <id>]
 * [--to <phase>]` — the owner puts off an item's fix. A deferral needs a
 * guard: a test that shows the item's current cost, or a recorded owner
 * ruling. Neither given: refused with the reason (never written to the
 * inbox). Either given: queued like every owner command; the conductor
 * records it and `tt summary` lists it under "Open deferrals" until
 * resolved. */
async function cmdDefer(
  positional: string[],
  root: string,
  guard: { test?: string; ownerRuling?: string; toPhase?: string },
): Promise<void> {
  const runDir = resolveRunDir(positional[0], root);
  const recordId = positional[1];
  if (!recordId) usage();
  const test = guard.test?.trim();
  const ownerRuling = guard.ownerRuling?.trim();
  if (!test && !ownerRuling) {
    process.stdout.write(
      "defer rejected: a deferral needs a guard — --test <test that shows its current cost> or --owner-ruling <ruling id>\n",
    );
    process.exitCode = 1;
    return;
  }
  const plan = readPlan(runDir);
  const state = rebuildState(runDir, plan, { lenient: true });
  const finding = state.phase.findings.find((f) => f.id === recordId);
  const decision = state.phase.decisions.find((d) => d.id === recordId);
  if (!finding && !decision) {
    process.stdout.write(`defer rejected: no finding or decision ${recordId} in run ${path.basename(runDir)}\n`);
    process.exitCode = 1;
    return;
  }
  // OD addendum A: a second owner act on the same id is refused, naming the
  // existing act.
  if ((state.phase.carriedItems ?? []).includes(recordId)) {
    process.stdout.write(`defer rejected: ${recordId} is already carried (to ${state.phase.carriedTo?.[recordId] ?? "next"}); resolve that carry first\n`);
    process.exitCode = 1;
    return;
  }
  // Plan 06i (A4/R7): the guard must be SUBSTANTIVE, not just spelled. A
  // `--test` must be a test the item resolver already resolved against this
  // phase's check run (or one the repo's own test files contain); an
  // `--owner-ruling` must name a recorded owner input/directive/request.
  if (test && !deferralTestExists(runDir, plan.repo, state, test)) {
    process.stdout.write(`defer rejected: no resolved or existing test named "${test}" shows the item's current cost\n`);
    process.exitCode = 1;
    return;
  }
  if (ownerRuling && !recordedOwnerRuling(state, ownerRuling)) {
    process.stdout.write(`defer rejected: ${ownerRuling} is not a recorded owner input, directive or request\n`);
    process.exitCode = 1;
    return;
  }
  const boundRecordVersion = finding ? finding.version : decision!.version;
  const inbox = path.join(runDir, "inbox");
  mkdirSync(inbox, { recursive: true });
  const commandId = `defer-${Date.now().toString(36)}-${randomUUID().slice(0, 6)}`;
  const command = {
    type: "defer",
    text: finding ? finding.evidence : decision!.choice,
    ...(test ? { test } : {}),
    ...(ownerRuling ? { ownerRuling } : {}),
    ...(guard.toPhase ? { toPhase: guard.toPhase.trim() } : {}),
    binding: {
      runId: state.phase.runId,
      phaseId: state.phase.phaseId,
      candidateSha: state.phase.candidate?.sha ?? "",
      contractVersion: state.phase.contract.contractVersion,
      recordId,
      recordVersion: boundRecordVersion,
    },
  };
  writeFileSync(path.join(inbox, `${commandId}.json`), JSON.stringify(command, null, 2));
  const outcome = await awaitInboxVerdict(runDir, commandId, 20000);
  if (outcome.kind === "applied") {
    process.stdout.write(`deferred ${recordId}${guard.toPhase ? ` to ${guard.toPhase.trim()}` : ""} in run ${path.basename(runDir)}\n`);
  } else if (outcome.kind === "rejected") {
    process.stdout.write(`defer rejected: ${outcome.reason}\n`);
    process.exitCode = 1;
  } else {
    process.stdout.write(`queued defer ${commandId} for ${recordId} (queued, not yet applied)\n`);
  }
}

/** Plan 06i (A4/R7): a deferral's `--test` guard is substantive only when
 * the item resolver already resolved that test against the phase's check run,
 * or the repo's own test files contain it. */
function deferralTestExists(runDir: string, repo: string, state: ReturnType<typeof rebuildState>, test: string): boolean {
  // The item resolver must have RESOLVED the test against this phase's check
  // run. A mere mention of the name in the repo (a comment, a doc, a string
  // literal) is not a test that shows the item's current cost, so there is no
  // substring fallback.
  void runDir;
  void repo;
  return (state.phase.checkResolution ?? []).some((r) => r.name === test && r.outcome === "passed");
}

/** Plan 06i (A4/R7): an `--owner-ruling` guard is substantive only when it
 * names a RECORDED OWNER INPUT (an inbox command id). An auto-opened triage
 * owner request is a question, not a ruling, and an owner directive is not a
 * ruling on this item either. */
function recordedOwnerRuling(state: ReturnType<typeof rebuildState>, id: string): boolean {
  return (state.phase.ownerInputs ?? []).some((i) => i.id === id);
}

/** Plan 06j (A3): `tt recheck <run> --reason "<text>"`. The owner asks the
 * conductor to re-run the frozen candidate's own checks because the failure
 * looks like the machine. It is accepted only after the current candidate's
 * checks just failed and before a newer candidate exists; it dispatches no
 * worker and spends no round. Written through the inbox, so the conductor
 * validates the same conditions a hand-written file cannot bypass. */
async function cmdRecheck(positional: string[], root: string, reason: string | undefined): Promise<void> {
  const runDir = resolveRunDir(positional[0], root);
  const text = reason?.trim() ?? "";
  const refuse = (why: string): void => {
    process.stdout.write(`recheck refused: ${why}\n`);
    process.exitCode = 1;
  };
  if (text.length === 0) {
    refuse('a recheck needs --reason "<why this failure is the machine>"');
    return;
  }
  const plan = readPlan(runDir);
  const state = rebuildState(runDir, plan, { lenient: true });
  const name = state.phase.phase;
  if (name === "DONE" || name === "BLOCKED") {
    refuse(`the phase is ${name}; the run no longer accepts owner input`);
    return;
  }
  const candidate = state.phase.candidate;
  const checks = state.phase.checks;
  if (!candidate) {
    refuse("the phase has no frozen candidate yet");
    return;
  }
  if (checks?.passed === true) {
    refuse("the current candidate's checks passed");
    return;
  }
  if (checks && checks.candidateSha !== candidate.sha) {
    refuse(`a newer candidate exists (${candidate.sha.slice(0, 9)}); recheck only the candidate whose checks just failed`);
    return;
  }
  if (!checks || checks.passed !== false || checks.interrupted === true) {
    refuse("the current candidate's checks did not just fail");
    return;
  }
  // Plan 06j (A3, owner steer 19:41Z): a check failure with budget left
  // starts a repair attempt by itself. The owner may still recheck while it
  // has not submitted; the conductor stops the worker and gives the round
  // back. Any other state is refused.
  if (name !== "AWAITING_OWNER" && name !== "IMPLEMENTING") {
    refuse(`the phase is ${name}; only a parked or repairing phase whose checks just failed accepts a recheck`);
    return;
  }
  const tier = checks.tier === "final" ? "final" : "round";
  const inbox = path.join(runDir, "inbox");
  mkdirSync(inbox, { recursive: true });
  const commandId = `recheck-${Date.now().toString(36)}-${randomUUID().slice(0, 6)}`;
  const command = {
    type: "recheck",
    reason: text,
    tier,
    binding: {
      runId: state.phase.runId,
      phaseId: state.phase.phaseId,
      candidateSha: candidate.sha,
      contractVersion: state.phase.contract.contractVersion,
    },
  };
  writeFileSync(path.join(inbox, `${commandId}.json`), JSON.stringify(command, null, 2));
  const outcome = await awaitInboxVerdict(runDir, commandId, 20000);
  if (outcome.kind === "applied") {
    process.stdout.write(`recheck requested for ${candidate.sha.slice(0, 9)} in run ${path.basename(runDir)}: ${text}\n`);
  } else if (outcome.kind === "rejected") {
    refuse(outcome.reason ?? "the conductor rejected it");
  } else {
    process.stdout.write(`queued recheck ${commandId} for ${candidate.sha.slice(0, 9)} (queued, not yet applied)\n`);
  }
}

/** Plan 05j: record a lint violation the render just named. Every render
 * (conductor, `tt contract rebuild`, a late verdict, an entry command) must
 * leave the log's copy, not just the view's red first line (findings A-18,
 * B-13, A-11). A detail already in the log is not appended twice. */
function recordLintFailures(runDir: string, lint: ReviewLintResult): void {
  if (lint.ok) return;
  const p = runPaths(runDir);
  let existing = "";
  try {
    existing = readFileSync(p.events, "utf8");
  } catch {
    // no log yet: every violation is new
  }
  const missing = lint.violations.filter((v) => !existing.includes(JSON.stringify(v.detail)));
  if (missing.length === 0) return;
  const maskable = resolveSecrets(secretNames(readPlan(runDir).secrets)).maskable;
  const log = new EventLog(p.events, maskable);
  try {
    for (const v of missing) log.append("event", { type: "REVIEW_LINT_FAILED", rule: v.rule, detail: v.detail });
  } finally {
    log.close();
  }
}

/** Plan 05j: the owner's entry corrections the review view sends. `s` sends
 * ENTRY_SPLIT, `m` ENTRY_MERGED_BY_OWNER, `A`/`D` an ENTRY_STATE; a retitle is
 * the same path. A live run goes through the inbox (the conductor applies
 * it); a stopped run's late command is validated by a dry-run reduce and then
 * appended to the log. */
async function cmdEntry(positional: string[], root: string, reason: string | undefined): Promise<void> {
  const runDir = resolveRunDir(positional[0], root);
  const op = positional[1];
  const entryId = positional[2];
  if (!op || !entryId) usage();
  const plan = readPlan(runDir);
  const state = rebuildState(runDir, plan, { lenient: true });
  const base = { runId: state.phase.runId, phaseId: state.phase.phaseId, entryId };
  let command: Record<string, unknown>;
  if (op === "split") {
    const messageId = positional[3];
    if (!messageId) usage();
    command = { type: "entry-split", ...base, messageId };
  } else if (op === "merge") {
    const intoEntryId = positional[3];
    if (!intoEntryId) usage();
    command = { type: "entry-merge", ...base, intoEntryId };
  } else if (op === "retitle") {
    const title = positional.slice(3).join(" ").trim();
    if (!title) usage();
    command = { type: "entry-retitle", ...base, title };
  } else if (op === "accept" || op === "refuse") {
    command = { type: "entry-verdict", ...base, verdict: op === "accept" ? "accept" : "refuse", ...(reason ? { reason } : {}) };
  } else {
    usage();
  }
  // Expand once, against the rebuilt state, so a rejected command is refused
  // before it is queued and a raw message that will not be settled is
  // REPORTED to the owner (record A-72 / M-63).
  const expanded = expandEntryCommand(command, state.phase.entries ?? [], state.phase.messages ?? []);
  if (!expanded.ok) {
    process.stdout.write(`entry ${op} rejected: ${expanded.reason}\n`);
    process.exitCode = 1;
    return;
  }
  if (expanded.skipped.length > 0) {
    process.stdout.write(`entry ${op}: ${expanded.skipped.length} message(s) not yet frozen, not settled: ${expanded.skipped.join(", ")}\n`);
  }
  if (conductorAlive(runDir)) {
    const inbox = path.join(runDir, "inbox");
    mkdirSync(inbox, { recursive: true });
    const commandId = `entry-${Date.now().toString(36)}-${randomUUID().slice(0, 6)}`;
    writeFileSync(path.join(inbox, `${commandId}.json`), JSON.stringify(command, null, 2));
    const outcome = await awaitInboxVerdict(runDir, commandId, 20000);
    if (outcome.kind === "applied") process.stdout.write(`entry ${op} applied: ${entryId} in run ${path.basename(runDir)}\n`);
    else if (outcome.kind === "rejected") {
      process.stdout.write(`entry ${op} rejected: ${outcome.reason}\n`);
      process.exitCode = 1;
    } else process.stdout.write(`queued entry ${op} ${commandId} for run ${path.basename(runDir)} (queued, not yet applied)\n`);
    return;
  }
  let check = state;
  for (const event of expanded.events) {
    const result = reduce(check, event);
    if (!result.ok) {
      process.stdout.write(`entry ${op} rejected: ${result.reason}\n`);
      process.exitCode = 1;
      return;
    }
    check = result.state;
  }
  const p = runPaths(runDir);
  const maskable = resolveSecrets(secretNames(plan.secrets)).maskable;
  const log = new EventLog(p.events, maskable);
  try {
    for (const event of expanded.events) log.append("event", event);
  } finally {
    log.close();
  }
  const after = rebuildState(runDir, plan, { lenient: true });
  const entryReview = projectEntryReview(
    { ...after.phase, ...runIds(runDir) },
    { anchorFreshness: candidateAnchorFreshness(after.phase.candidate ? path.join(p.candidates, after.phase.candidate.sha) : undefined) },
  );
  recordLintFailures(runDir, entryReview.lint);
  writeFileSync(p.review, entryReview.text);
  mkdirSync(p.entriesView, { recursive: true });
  const entryIds = new Set(entryReview.files.map((f) => f.id));
  for (const f of entryReview.files) writeFileSync(path.join(p.entriesView, `${f.id}.org`), f.contents);
  for (const name of readdirSync(p.entriesView)) {
    if (name.endsWith(".org") && !entryIds.has(name.slice(0, -4))) rmSync(path.join(p.entriesView, name), { force: true });
  }
  mkdirSync(p.itemsView, { recursive: true });
  const itemIds = new Set(entryReview.itemFiles.map((f) => f.id));
  for (const f of entryReview.itemFiles) writeFileSync(path.join(p.itemsView, `${f.id}.org`), f.contents);
  for (const name of readdirSync(p.itemsView)) {
    if (name.endsWith(".org") && !itemIds.has(name.slice(0, -4))) rmSync(path.join(p.itemsView, name), { force: true });
  }
  process.stdout.write(`recorded entry ${op} for ${entryId} in run ${path.basename(runDir)}\n`);
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
  const { positional, root, json, reason, candidateSha, messageVersion, contractVersion, contractSha256, runId, phaseId, source, skipModelsCheck, envFile, toPhase, deferTest, deferOwnerRuling } = parseArgs(rest);
  const runRoot = root ?? DEFAULT_ROOT;
  if (cmd === "start") {
    if (positional.length !== 1) usage();
    await cmdStart(positional[0], runRoot, skipModelsCheck, envFile);
  } else if (cmd === "models") {
    await cmdModels(positional[0], positional.slice(1));
  } else if (cmd === "lint") {
    if (positional.length !== 1) usage();
    cmdLint(positional[0]);
  } else if (cmd === "plan" && positional[0] === "template") {
    cmdPlanTemplate();
  } else if (cmd === "evidence") {
    if (positional.length < 3) usage();
    await cmdEvidence(positional, runRoot);
  } else if (cmd === "status") {
    if (positional.length !== 1) usage();
    const runDir = resolveRunDir(positional[0], runRoot);
    delegateToRecordedRunner(runDir, runRoot);
    process.stdout.write(redactText(renderStatus(runDir), secretsForRun(runDir)));
  } else if (cmd === "resume") {
    if (positional.length !== 1) usage();
    const runDir = resolveRunDir(positional[0], runRoot);
    const runPlan = readPlan(runDir);
    if (refuseMissingSecrets(runPlan.secrets, "resume the run", runPlan.envFile)) return;
    launchDetached(runDir);
  } else if (cmd === "program") {
    await cmdProgram(positional[0], positional.slice(1), runRoot, json, source, skipModelsCheck, envFile);
  } else if (cmd === "list") {
    cmdList(runRoot, json);
  } else if (cmd === "contract") {
    if (positional.length !== 2) usage();
    cmdContract(positional[0], resolveRunDir(positional[1], runRoot));
  } else if (cmd === "verdict") {
    if (positional.length !== 3) usage();
    await cmdVerdict(positional, runRoot, reason, { candidateSha, messageVersion, contractVersion, contractSha256, runId, phaseId });
  } else if (cmd === "carry") {
    if (positional.length !== 2) usage();
    await cmdCarry(positional, runRoot, toPhase);
  } else if (cmd === "defer") {
    if (positional.length !== 2) usage();
    await cmdDefer(positional, runRoot, { test: deferTest, ownerRuling: deferOwnerRuling, toPhase });
  } else if (cmd === "recheck") {
    if (positional.length !== 1) usage();
    await cmdRecheck(positional, runRoot, reason);
  } else if (cmd === "entry") {
    if (positional.length < 3) usage();
    await cmdEntry(positional, runRoot, reason);
  } else if (cmd === "summary") {
    if (positional.length !== 1) usage();
    const runDir = resolveRunDir(positional[0], runRoot);
    delegateToRecordedRunner(runDir, runRoot);
    process.stdout.write(runSummary(runDir));
  } else if (cmd === "timing") {
    if (positional.length !== 1) usage();
    const runDir = resolveRunDir(positional[0], runRoot);
    delegateToRecordedRunner(runDir, runRoot);
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
    delegateToRecordedRunner(runDir, runRoot);
    const p = runPaths(runDir);
    const plan = JSON.parse(readFileSync(path.join(p.plan, "v1.json"), "utf8")) as RunPlanFile;
    const meta = JSON.parse(readFileSync(p.meta, "utf8"));
    const state = rebuildState(runDir, plan, { lenient: true });
    let alive = false;
    try {
      const pid = Number(readFileSync(path.join(runDir, "conductor.pid"), "utf8"));
      if (pid > 0) alive = processAlive(pid);
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
    const { timeline: _timeline, tape: _tape, ...view } = buildView(runDir, plan, alive);
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
