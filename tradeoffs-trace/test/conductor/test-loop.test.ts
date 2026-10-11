// Plan 06l (A2): the narrow worker test loop. `tt test --changed` runs only
// the plan's check test files that import a changed file, reuses a recorded
// pass when the tree hash is unchanged, prints failures only, and has its own
// time budget (never the conductor's 6-minute shell cap).

import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { CHANGED_TEST_BUDGET_MS, checkTestFiles, isChangedTestCommand, testFilesImporting } from "../../src/core/checks.ts";
import { cleanupDir } from "./harness.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

function tt(args: string[], env: NodeJS.ProcessEnv): { status: number; stdout: string; stderr: string } {
  const r = spawnSync(process.execPath, [CLI, ...args], { encoding: "utf8", env: { ...process.env, ...env } });
  return { status: r.status ?? 1, stdout: r.stdout ?? "", stderr: r.stderr ?? "" };
}

/** A one-test node:test file that appends `which` to TT_RUN_LOG and asserts
 * its imported function's value. */
function tsTestBody(which: string, modulePath: string, fn: string, value: number): string {
  return [
    'import { appendFileSync } from "node:fs";',
    'import test from "node:test";',
    'import assert from "node:assert/strict";',
    `import { ${fn} } from "${modulePath}";`,
    `test("${which} test", () => {`,
    `  appendFileSync(process.env.TT_RUN_LOG!, "${which}\\n");`,
    `  assert.equal(${fn}(), ${value});`,
    "});",
    "",
  ].join("\n");
}

test("plan 06l: tt test --changed runs only affected test files and reuses a pass for an unchanged tree", () => {
  const repo = fs.mkdtempSync("/tmp/tt-test-changed-");
  const logDir = fs.mkdtempSync("/tmp/tt-test-changed-log-");
  const marker = path.join(logDir, "ran.log");
  try {
    git(["init", "-q", "-b", "main"], repo);
    fs.mkdirSync(path.join(repo, "src"));
    fs.mkdirSync(path.join(repo, "test"));
    fs.writeFileSync(path.join(repo, "src", "a.ts"), "export function a(): number { return 1; }\n");
    fs.writeFileSync(path.join(repo, "src", "b.ts"), "export function b(): number { return 2; }\n");
    fs.writeFileSync(path.join(repo, "test", "a.test.ts"), tsTestBody("a", "../src/a.ts", "a", 1));
    fs.writeFileSync(path.join(repo, "test", "b.test.ts"), tsTestBody("b", "../src/b.ts", "b", 2));
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repo);

    const plan = {
      title: "narrow",
      repo,
      integrationBranch: "main",
      checks: [],
      phases: [{ id: "p1", goal: "x", acceptance: ["it works"], checks: ["node --test test/a.test.ts test/b.test.ts"], boundaries: [], reserved: [] }],
    };
    const planPath = path.join(repo, "plan.json");
    fs.writeFileSync(planPath, JSON.stringify(plan));

    // A change to ONE source file: only test/a.test.ts imports it.
    fs.writeFileSync(path.join(repo, "src", "a.ts"), "export function a(): number { return 1; } // changed\n");
    const env = { TT_RUN_LOG: marker };
    const first = tt(["test", "--changed", "--plan", planPath, "--repo", repo], env);
    assert.equal(first.status, 0, first.stderr);
    assert.equal(fs.readFileSync(marker, "utf8"), "a\n", "only the test file importing the changed file ran");

    // The SAME tree hash: nothing runs, the recorded pass is reported.
    const second = tt(["test", "--changed", "--plan", planPath, "--repo", repo], env);
    assert.equal(second.status, 0, second.stderr);
    assert.match(second.stderr, /reused a recorded pass/, "the recorded pass is reported");
    assert.equal(fs.readFileSync(marker, "utf8"), "a\n", "the second call ran nothing");

    // A change to the other source file (with the first reverted) runs only
    // the other test.
    git(["checkout", "--", "src/a.ts"], repo);
    fs.writeFileSync(path.join(repo, "src", "b.ts"), "export function b(): number { return 2; } // changed\n");
    const third = tt(["test", "--changed", "--plan", planPath, "--repo", repo], env);
    assert.equal(third.status, 0, third.stderr);
    assert.equal(fs.readFileSync(marker, "utf8"), "a\nb\n", "the changed file's own test ran");

    // A changed file no test imports runs nothing, and records NO pass, so a
    // second call reports the same thing instead of a false "recorded pass".
    git(["checkout", "--", "src/b.ts"], repo);
    fs.writeFileSync(path.join(repo, "src", "c.ts"), "export const c = 3; // no test imports this\n");
    const noMatch = tt(["test", "--changed", "--plan", planPath, "--repo", repo], env);
    assert.equal(noMatch.status, 0, noMatch.stderr);
    assert.match(noMatch.stderr, /no test file imports/);
    assert.doesNotMatch(noMatch.stderr, /reused a recorded pass/);
    const noMatchAgain = tt(["test", "--changed", "--plan", planPath, "--repo", repo], env);
    assert.match(noMatchAgain.stderr, /no test file imports/, "no pass was recorded for a run that ran no tests");

    // Its own budget, not the 6-minute shell cap.
    assert.ok(CHANGED_TEST_BUDGET_MS > 6 * 60_000, `tt test --changed's budget (${CHANGED_TEST_BUDGET_MS}ms) must exceed the 6-minute shell cap`);
    assert.equal(isChangedTestCommand('tt test --changed --run "$TT_RUN_DIR"'), true);
    assert.equal(isChangedTestCommand("node /x/cli.ts test --changed --run /tmp/r"), true);
    assert.equal(isChangedTestCommand("node --test test/a.test.ts"), false);
    // A shell separator must not let another long command ride the budget.
    assert.equal(isChangedTestCommand("tt test --changed --run /tmp/r && make check"), false);
  } finally {
    cleanupDir(repo);
    cleanupDir(logDir);
  }
});

test("plan 06l: tt test --changed resolves a check's -C subdirectory and untracked files", () => {
  // This repo's own layout: the checks run `make -C pkg check-e2e FILES="…"`,
  // so the test file is relative to `pkg` while git's changed paths are
  // relative to the repository root. And a NEW directory is reported as
  // `?? pkg/src/deep/` unless --untracked-files=all expands it.
  const repo = fs.mkdtempSync("/tmp/tt-test-changed-sub-");
  const logDir = fs.mkdtempSync("/tmp/tt-test-changed-sub-log-");
  const marker = path.join(logDir, "ran.log");
  try {
    git(["init", "-q", "-b", "main"], repo);
    fs.mkdirSync(path.join(repo, "pkg", "src"), { recursive: true });
    fs.mkdirSync(path.join(repo, "pkg", "test"), { recursive: true });
    fs.writeFileSync(path.join(repo, "pkg", "src", "a.ts"), "export function a(): number { return 1; }\n");
    fs.writeFileSync(path.join(repo, "pkg", "test", "a.test.ts"), tsTestBody("a", "../src/a.ts", "a", 1));
    git(["add", "-A"], repo);
    git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], repo);
    const plan = {
      title: "sub",
      repo,
      integrationBranch: "main",
      checks: [],
      phases: [{ id: "p1", goal: "x", acceptance: ["it works"], checks: ['make -C pkg check-e2e FILES="test/a.test.ts test/deep.test.ts"'], boundaries: [], reserved: [] }],
    };
    const planPath = path.join(repo, "plan.json");
    fs.writeFileSync(planPath, JSON.stringify(plan));
    const env = { TT_RUN_LOG: marker };

    // A tracked change under the subdirectory.
    fs.writeFileSync(path.join(repo, "pkg", "src", "a.ts"), "export function a(): number { return 1; } // changed\n");
    const first = tt(["test", "--changed", "--plan", planPath, "--repo", repo], env);
    assert.equal(first.status, 0, first.stderr);
    assert.equal(fs.readFileSync(marker, "utf8"), "a\n", "the subdirectory check's test ran");

    // An UNTRACKED source and test in a NEW directory: --untracked-files=all
    // must name the files, not the directory.
    git(["checkout", "--", "pkg/src/a.ts"], repo);
    fs.mkdirSync(path.join(repo, "pkg", "src", "deep"), { recursive: true });
    fs.writeFileSync(path.join(repo, "pkg", "src", "deep", "c.ts"), "export function c(): number { return 3; }\n");
    fs.writeFileSync(path.join(repo, "pkg", "test", "deep.test.ts"), tsTestBody("deep", "../src/deep/c.ts", "c", 3));
    const second = tt(["test", "--changed", "--plan", planPath, "--repo", repo], env);
    assert.equal(second.status, 0, second.stderr);
    assert.equal(fs.readFileSync(marker, "utf8"), "a\ndeep\n", "an untracked file in a new directory runs its importing test");
  } finally {
    cleanupDir(repo);
    cleanupDir(logDir);
  }
});

test("plan 06l: the changed-test helpers pick the importing files only", () => {
  assert.deepEqual(checkTestFiles(["node --test test/a.test.ts test/b.test.ts", 'make check-e2e FILES="test/c.test.ts"']), [
    "test/a.test.ts",
    "test/b.test.ts",
    "test/c.test.ts",
  ]);
  const files: Record<string, string> = {
    "test/a.test.ts": 'import { a } from "../src/a.ts";\n',
    "test/b.test.ts": 'import { b } from "../src/b.ts";\n',
    "src/a.ts": "export const a = 1;\n",
    "src/b.ts": "export const b = 2;\n",
  };
  const affected = testFilesImporting(
    ["src/a.ts"],
    ["test/a.test.ts", "test/b.test.ts"],
    (f) => files[f],
    (f) => files[f] !== undefined,
  );
  assert.deepEqual(affected, ["test/a.test.ts"]);
});

test("plan 06l: deleting an imported source file selects the tests that import it", () => {
  // src/a.ts is deleted: it is a changed path but no longer exists.
  const files: Record<string, string> = {
    "test/a.test.ts": 'import { a } from "../src/a.ts";\n',
    "test/b.test.ts": 'import { b } from "../src/b.ts";\n',
    "src/b.ts": "export const b = 2;\n",
  };
  const affected = testFilesImporting(
    ["src/a.ts"],
    ["test/a.test.ts", "test/b.test.ts"],
    (f) => files[f],
    (f) => files[f] !== undefined,
  );
  assert.deepEqual(affected, ["test/a.test.ts"]);
});
