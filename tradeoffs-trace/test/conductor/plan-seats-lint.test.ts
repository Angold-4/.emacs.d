// Plan 06h (A1/A4): `tt lint` refuses a seat list or lane count outside the
// rule, each finding with the file and line the owner can jump to. The plan
// under test is parsed by the REAL Emacs parser (`+tt-parse-program`), then
// linted by the one implementation (`src/core/plan-lint.ts`) through the CLI,
// so the parser and the linter are exercised together.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomBytes } from "node:crypto";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { cleanupDir } from "./harness.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));
const EMACS_LOAD = fileURLToPath(new URL("../../../test/tradeoffs-trace-test.el", import.meta.url));
const CORE_DIR = fileURLToPath(new URL("../../../core", import.meta.url));
const TEST_DIR = fileURLToPath(new URL("../../../test", import.meta.url));

function tmpDir(prefix: string): string {
  return fs.mkdtempSync(path.join("/tmp", `${prefix}-${randomBytes(3).toString("hex")}-`));
}

function runCli(args: string[], env: NodeJS.ProcessEnv = {}): Promise<{ code: number | null; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI, ...args], { env: { ...process.env, ...env } });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ code, stdout, stderr }));
  });
}

/** Parse a program file with the real Emacs parser and return its JSON. */
function parseProgramWithEmacs(programPath: string): string {
  return execFileSync(
    "emacs",
    [
      "--batch",
      "-Q",
      "-L",
      CORE_DIR,
      "-L",
      TEST_DIR,
      "-l",
      EMACS_LOAD,
      "--eval",
      `(with-temp-buffer (insert-file-contents "${programPath}") (org-mode) (setq buffer-file-name "${programPath}") (princ (json-encode (plist-get (+tt-parse-program) :program))))`,
    ],
    { encoding: "utf8" },
  );
}

/** Write a one-entry program whose entry's plan carries `keywords`, and
 * return the path to the program Org file. The plan file is `plan.org`. */
function writeProgram(dir: string, keywords: string[]): string {
  const planPath = path.join(dir, "plan.org");
  fs.writeFileSync(
    planPath,
    [
      "#+TITLE: p",
      "#+TT_REPO: /tmp/x",
      "#+TT_BRANCH: main",
      ...keywords,
      "",
      "* P1",
      "  :PROPERTIES:",
      "  :ID: p1",
      "  :CHECKS: true",
      "  :END:",
      "  Goal: g",
      "  Acceptance:",
      "  - it works",
    ].join("\n") + "\n",
  );
  const programPath = path.join(dir, "program.org");
  fs.writeFileSync(
    programPath,
    ["#+TITLE: prog", "#+TT_PROGRAM: 1", "* e1", "  :PROPERTIES:", "  :ID: e1", `  :PLAN: ${planPath}`, "  :END:"].join("\n") + "\n",
  );
  return programPath;
}

/** Parse + lint a plan with the given keyword lines, and return the lint
 * output (stdout) and the plan file path the findings name. */
async function lintKeywords(dir: string, keywords: string[]): Promise<{ output: string; planFile: string }> {
  const programPath = writeProgram(dir, keywords);
  const json = parseProgramWithEmacs(programPath);
  const jsonPath = path.join(dir, "program.json");
  fs.writeFileSync(jsonPath, json);
  const r = await runCli(["lint", jsonPath]);
  assert.notEqual(r.code, 0, `lint must refuse:\n${r.stdout}`);
  return { output: r.stdout, planFile: path.join(dir, "plan.org") };
}

test("plan 06h: tt lint on an org program parsed by the real Emacs parser refuses 4 workers with 3 reviewers naming 5", async () => {
  const dir = tmpDir("tt-seats-lint");
  try {
    // 1. Four workers with the default three reviewers: the smallest valid
    //    reviewer count is 5, and the finding names it.
    const four = await lintKeywords(dir, ["#+TT_WORKERS: 4", "#+TT_REVIEWERS: M A B"]);
    assert.match(four.output, new RegExp(`${four.planFile.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\d+: error: \\[(?:e1/)?workers\\]`));
    assert.match(four.output, /needs at least 5 reviewers/);

    // 2. An even seat list.
    const even = await lintKeywords(dir, ["#+TT_REVIEWERS: M A B C"]);
    assert.match(even.output, new RegExp(`${even.planFile.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\d+: error: \\[(?:e1/)?reviewers\\]`));
    assert.match(even.output, /count must be odd/);

    // 3. A duplicate seat.
    const duplicate = await lintKeywords(dir, ["#+TT_REVIEWERS: M A B A"]);
    assert.match(duplicate.output, new RegExp(`${duplicate.planFile.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\d+: error: \\[(?:e1/)?reviewers\\]`));
    assert.match(duplicate.output, /names A more than once/);

    // 4. An unknown leader.
    const leader = await lintKeywords(dir, ["#+TT_REVIEWERS: M A B", "#+TT_LEADER: X"]);
    assert.match(leader.output, new RegExp(`${leader.planFile.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\d+: error: \\[(?:e1/)?leader\\]`));
    assert.match(leader.output, /not one of the reviewer seats/);

    // 5. reviewer.X for an undeclared X.
    const reviewer = await lintKeywords(dir, ["#+TT_REVIEWERS: M A B", "#+TT_MODELS: reviewer.X=gateway:model"]);
    assert.match(reviewer.output, new RegExp(`${reviewer.planFile.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\d+: error: \\[(?:e1/)?models\\]`));
    assert.match(reviewer.output, /reviewer\.X, which #\+TT_REVIEWERS does not declare/);

    // 6. worker.5 with four lanes.
    const lane = await lintKeywords(dir, ["#+TT_WORKERS: 4", "#+TT_REVIEWERS: M A B C D", "#+TT_MODELS: worker.5=gateway:model"]);
    assert.match(lane.output, new RegExp(`${lane.planFile.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\d+: error: \\[(?:e1/)?models\\]`));
    assert.match(lane.output, /worker lane worker\.5/);
    assert.match(lane.output, /#\+TT_WORKERS declares 4/);
  } finally {
    cleanupDir(dir);
  }
});

test("plan 06h: tt lint refuses TT_ROUNDS 0 and 6, and accepts 1 to 5", async () => {
  const dir = tmpDir("tt-rounds-lint");
  try {
    const zero = await lintKeywords(dir, ["#+TT_ROUNDS: 0"]);
    assert.match(zero.output, new RegExp(`${zero.planFile.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}:\\d+: error: \\[(?:e1/)?rounds\\]`));
    assert.match(zero.output, /from 1 to 5, got 0/);

    const six = await lintKeywords(dir, ["#+TT_ROUNDS: 6"]);
    assert.match(six.output, /from 1 to 5, got 6/);

    // 1, 3 and 5 are accepted (3 is the default).
    for (const n of [1, 3, 5]) {
      const programPath = writeProgram(dir, [`#+TT_ROUNDS: ${n}`]);
      const json = parseProgramWithEmacs(programPath);
      const jsonPath = path.join(dir, `program-${n}.json`);
      fs.writeFileSync(jsonPath, json);
      const r = await runCli(["lint", jsonPath]);
      assert.equal(r.code, 0, `#+TT_ROUNDS: ${n} must lint clean:\n${r.stdout}`);
    }
  } finally {
    cleanupDir(dir);
  }
});
