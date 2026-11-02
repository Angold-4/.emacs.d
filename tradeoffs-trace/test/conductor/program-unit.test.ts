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
import { appendProgramEvent, createProgram, foldProgram, programStatusLines, schedulerTick } from "../../src/program.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import type { RunPlanFile } from "../../src/conductor.ts";

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

/** 07h3 (H2/R6): `#+TT_BASE: B` on a program is the branch EVERY root node is
 * cut from — not each entry's own `TT_BRANCH` — and it is the branch the nodes
 * name as their PR base. Two root nodes (no dependency between them, so the
 * base cannot come from a sibling) prove "every root node"; a program without
 * the keyword is the control, cutting from its plan's own branch as before. */
test("TT_BASE sets every root node's base and the PR base", () => {
  const repo = makeRepo();
  const root = shortTmp("tt-07h3-base-root");
  const scriptsDir = shortTmp("tt-07h3-base-scripts");
  const phase = { id: "p1", goal: "g", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] };
  const plan = (title: string): RunPlanFile => ({ title, repo: repo.dir, integrationBranch: "main", checks: ["true"], phases: [phase] });
  // Branch B (`release`) is a commit ahead of main, so "cut from B" is a fact
  // about the node branches' content, not only their name.
  execFileSync("git", ["-C", repo.dir, "checkout", "-q", "-b", "release"], { encoding: "utf8" });
  fs.writeFileSync(path.join(repo.dir, "release-only.txt"), "release\n");
  execFileSync("git", ["-C", repo.dir, "add", "-A"], { encoding: "utf8" });
  execFileSync("git", ["-C", repo.dir, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "release"], { encoding: "utf8" });
  const releaseHead = execFileSync("git", ["-C", repo.dir, "rev-parse", "release"], { encoding: "utf8" }).trim();
  execFileSync("git", ["-C", repo.dir, "checkout", "-q", "main"], { encoding: "utf8" });

  const withBase = (ids: string[]): ProgramFile => ({
    title: "base-program",
    maxParallel: 2,
    base: "release",
    entries: ids.map((id) => ({ id, after: [], plan: plan(id) })),
  });
  const source = path.join(scriptsDir, "source.json");
  fs.writeFileSync(source, JSON.stringify(withBase(["a", "b"])));
  const dir = createProgram(root, withBase(["a", "b"]), "baseprog");
  try {
    const launched: string[] = [];
    schedulerTick(dir, {
      runRoot: root,
      launch: (runDir) => launched.push(runDir),
      exporter: (file) => JSON.parse(fs.readFileSync(file, "utf8")) as ProgramFile,
      runner: () => "test-runner",
    });
    assert.equal(launched.length, 2, "both root nodes started (neither depends on the other)");
    // Every root node's branch is cut from B, and its recorded base is B.
    for (const id of ["a", "b"]) {
      assert.equal(
        execFileSync("git", ["-C", repo.dir, "rev-parse", `main--${id}`], { encoding: "utf8" }).trim(),
        releaseHead,
        `${id}'s branch starts at #+TT_BASE, not at its plan's TT_BRANCH`,
      );
      assert.match(
        execFileSync("git", ["-C", repo.dir, "ls-tree", "--name-only", `main--${id}`], { encoding: "utf8" }),
        /^release-only\.txt$/m,
        `${id}'s branch carries B's own commit`,
      );
    }
    // ...and the program names B as the PR base of both nodes.
    const { state } = foldProgram(dir);
    assert.equal(state.nodes.a.base, "release");
    assert.equal(state.nodes.b.base, "release");
    const status = programStatusLines(dir, new Date(), { nodeDetail: false }).join("\n");
    assert.match(status, /branch main--a\s+\(PR base: release\)/, `status names the PR base B:\n${status}`);
    assert.match(status, /branch main--b\s+\(PR base: release\)/, `status names the PR base B:\n${status}`);
    assert.match(status, /^base {6}release$/m, `status also names the program's root base:\n${status}`);

    // The control: without #+TT_BASE the same graph is cut from its plan's
    // own TT_BRANCH, exactly as on the base.
    const plain = createProgram(root, { title: "no-base", maxParallel: 2, entries: [{ id: "c", after: [], plan: plan("c") }] }, "nobaseprog");
    schedulerTick(plain, {
      runRoot: root,
      launch: () => undefined,
      exporter: () => ({ title: "no-base", maxParallel: 2, entries: [{ id: "c", after: [], plan: plan("c") }] }),
      runner: () => "test-runner",
    });
    assert.equal(
      execFileSync("git", ["-C", repo.dir, "rev-parse", "main--c"], { encoding: "utf8" }).trim(),
      repo.head,
      "without the keyword a root node is cut from its plan's TT_BRANCH",
    );
    assert.equal(foldProgram(plain).state.nodes.c.base, "main");
  } finally {
    cleanupDir(root);
    cleanupDir(scriptsDir);
    cleanupDir(repo.dir);
  }
});
