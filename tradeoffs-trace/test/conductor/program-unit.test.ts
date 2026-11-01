// 07 design A1 (§3.1): ADMIT is the gate `tt start`, `tt resume` and a
// program node's own start all run before anything exists. The plan must
// lint, TT_REPO must be a git work tree, every check command must parse
// (`bash -n`) and the pinned pi must be the one on the resolved PATH. A plan
// defect is `X:PLAN_DEFECT` with its reason, an environment fault
// `X:ENV_FAULT`, and neither leaves an event behind.
//
// The two defects here are the ones a run used to discover only after it
// already existed:
//   * a TT_REPO that is not a git work tree (the run was created, then its
//     conductor failed on the first `git rev-parse`);
//   * a check command with a shell syntax error — exex 01c3b, 2026-10-28: a
//     split `AST_GREP` command made every candidate red with a shell syntax
//     error, one whole build after the plan could have been refused.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { cleanupDir, makeRepo } from "./harness.ts";
import { admitCommands } from "../../src/core/admit.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));
const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));

function shortTmp(prefix: string): string {
  return fs.mkdtempSync(path.join("/tmp", `${prefix}-`));
}

/** A plan file and a program file naming the same single-phase plan, in a
 * scratch directory. */
function writePlanAndProgram(dir: string, repo: string, checks: string[]): { plan: string; program: string } {
  const plan = {
    title: "admit unit",
    repo,
    integrationBranch: "main",
    checks,
    phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks, boundaries: [], reserved: [] }],
  };
  const planPath = path.join(dir, "plan.json");
  fs.writeFileSync(planPath, JSON.stringify(plan, null, 2));
  const programPath = path.join(dir, "program.json");
  fs.writeFileSync(programPath, JSON.stringify({ title: "admit program", maxParallel: 1, entries: [{ id: "a", after: [], plan }] }, null, 2));
  return { plan: planPath, program: programPath };
}

/** The CLI's own environment: a fake pi is injected (`TT_TEST_MODE=1`), so no
 * test needs the machine's real binary, and the root is a scratch directory. */
function cliEnv(scriptsDir: string): NodeJS.ProcessEnv {
  return {
    ...process.env,
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    TT_TEST_STUB_REVIEWS: "1",
    FAKE_PI_SCRIPT: scriptsDir,
  };
}

/** Run one `tt` command, returning its exit code and streams (never throwing:
 * a refusal is what these tests assert on). */
function runCli(args: string[], env: NodeJS.ProcessEnv): { code: number; stdout: string; stderr: string } {
  try {
    const stdout = execFileSync(process.execPath, [CLI_PATH, ...args], { encoding: "utf8", env, stdio: ["ignore", "pipe", "pipe"] });
    return { code: 0, stdout, stderr: "" };
  } catch (err) {
    const e = err as { status?: number; stdout?: string; stderr?: string };
    return { code: e.status ?? 1, stdout: e.stdout ?? "", stderr: e.stderr ?? "" };
  }
}

/** Every event file this root holds — a run's `events.jsonl` or a program's.
 * Nothing that ADMIT refused may leave one. */
function eventFilesUnder(root: string): string[] {
  const out: string[] = [];
  const walk = (dir: string): void => {
    let entries: fs.Dirent[] = [];
    try {
      entries = fs.readdirSync(dir, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries) {
      const full = path.join(dir, entry.name);
      if (entry.isDirectory()) walk(full);
      else if (entry.name === "events.jsonl") out.push(full);
    }
  };
  walk(root);
  return out;
}

test("a plan whose TT_REPO is not a git work tree is refused before any event", () => {
  // A real directory that is not a repository (and not inside one).
  const notARepo = shortTmp("tt-admit-notgit");
  const work = shortTmp("tt-admit-plans");
  const scripts = shortTmp("tt-admit-scripts");
  const root = shortTmp("tt-admit-root");
  const programRoot = shortTmp("tt-admit-prog-root");
  const env = cliEnv(scripts);
  try {
    fs.writeFileSync(path.join(notARepo, "README.md"), "not a repository\n");
    const paths = writePlanAndProgram(work, notARepo, ["true"]);

    // `tt start`
    const start = runCli(["start", paths.plan, "--root", root], env);
    assert.notEqual(start.code, 0, `tt start must refuse: ${start.stdout}${start.stderr}`);
    assert.match(start.stderr, /X:PLAN_DEFECT/, `the refusal is a named plan defect:\n${start.stderr}`);
    assert.match(start.stderr, /is not a git work tree/, "the reason says what TT_REPO is not");
    assert.deepEqual(fs.readdirSync(root), [], "no run directory was created");
    assert.deepEqual(eventFilesUnder(root), [], "no event exists");

    // `tt program start`
    const program = runCli(["program", "start", paths.program, "--root", programRoot], env);
    assert.notEqual(program.code, 0, `tt program start must refuse: ${program.stdout}${program.stderr}`);
    assert.match(program.stderr, /X:PLAN_DEFECT/, `the refusal is a named plan defect:\n${program.stderr}`);
    assert.match(program.stderr, /is not a git work tree/, "the reason says what TT_REPO is not");
    assert.deepEqual(fs.readdirSync(programRoot), [], "no program directory was created");
    assert.deepEqual(eventFilesUnder(programRoot), [], "no event exists");
  } finally {
    for (const dir of [notARepo, work, scripts, root, programRoot]) cleanupDir(dir);
  }
});

test("a check command that does not parse is refused before start", () => {
  const repo = makeRepo();
  const work = shortTmp("tt-admit-plans");
  const scripts = shortTmp("tt-admit-scripts");
  const root = shortTmp("tt-admit-root");
  const programRoot = shortTmp("tt-admit-prog-root");
  const env = cliEnv(scripts);
  // exex 01c3b, 2026-10-28: the `||` of a split AST_GREP command was written
  // as `| |`, which bash parses as a syntax error — every candidate went red
  // on it a whole build after the plan could have been refused.
  const broken = 'AST_GREP="rg --json"; $AST_GREP --files | | grep -v node_modules';
  try {
    const paths = writePlanAndProgram(work, repo.dir, [broken]);

    const start = runCli(["start", paths.plan, "--root", root], env);
    assert.notEqual(start.code, 0, `tt start must refuse: ${start.stdout}${start.stderr}`);
    assert.match(start.stderr, /X:PLAN_DEFECT/, `the refusal is a named plan defect:\n${start.stderr}`);
    assert.ok(start.stderr.includes(broken), `the reason names the command that does not parse:\n${start.stderr}`);
    assert.deepEqual(fs.readdirSync(root), [], "no run directory was created");
    assert.deepEqual(eventFilesUnder(root), [], "no event exists");

    const program = runCli(["program", "start", paths.program, "--root", programRoot], env);
    assert.notEqual(program.code, 0, `tt program start must refuse: ${program.stdout}${program.stderr}`);
    assert.match(program.stderr, /X:PLAN_DEFECT/, `the refusal is a named plan defect:\n${program.stderr}`);
    assert.ok(program.stderr.includes(broken), `the reason names the command that does not parse:\n${program.stderr}`);
    assert.deepEqual(fs.readdirSync(programRoot), [], "no program directory was created");
    assert.deepEqual(eventFilesUnder(programRoot), [], "no event exists");
  } finally {
    cleanupDir(work);
    cleanupDir(scripts);
    cleanupDir(root);
    cleanupDir(programRoot);
    cleanupDir(repo.dir);
  }
});

test("ADMIT parses every declared command: the checks, the final checks and the gate", () => {
  // The commands ADMIT must parse are exactly the ones a phase can run — a
  // gate command with a syntax error is as fatal as a check with one.
  assert.deepEqual(
    admitCommands({
      checks: ["plan-check"],
      phases: [
        { checks: ["phase-check"], finalChecks: ["final-check"], gate: "gate-cmd", gateCleanup: "cleanup-cmd" },
        // A second phase that repeats a command adds it once.
        { checks: ["phase-check"] },
      ],
    }),
    ["plan-check", "phase-check", "final-check", "gate-cmd", "cleanup-cmd"],
  );
});
