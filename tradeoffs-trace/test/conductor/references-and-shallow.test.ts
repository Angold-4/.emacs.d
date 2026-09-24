// Skill fixes from the plan-12 program (runs 9ab9188b and aea875c4):
// - a plan's reference documents are snapshotted into the run and named in
//   the agents' prompts, and find/grep/ls outside the checkout and those
//   references is refused (reviewers had searched the home directory for 6
//   minutes);
// - a run refuses to start on a shallow clone (three freeze attempts were
//   lost to it).

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, makeRepo, makeRunRoot } from "./harness.ts";
import { createRun, referenceLines, runReferences, type RunPlanFile } from "../../src/conductor.ts";
import { guardedSearchPath } from "../../extension/guards.ts";

function plan(repo: string, references?: string[]): RunPlanFile {
  return {
    title: "refs",
    repo,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    ...(references ? { references } : {}),
  } as RunPlanFile;
}

test("references: a plan's documents are snapshotted into the run and named in the prompt", () => {
  const repo = makeRepo();
  const root = makeRunRoot();
  const docs = fs.mkdtempSync("/tmp/tt-refs-");
  try {
    const a = path.join(docs, "12_ref_contract.md");
    fs.writeFileSync(a, "v1\n");
    const runDir = createRun(root, plan(repo.dir, [a, path.join(docs, "gone.md")]));
    fs.writeFileSync(a, "edited after the run started\n");
    const refs = runReferences(runDir);
    const copy = refs.find((r) => r.endsWith("12_ref_contract.md"))!;
    assert.ok(copy.startsWith(path.join(runDir, "refs")), "copied into the run");
    assert.equal(fs.readFileSync(copy, "utf8"), "v1\n", "the run keeps the version it started with");
    assert.match(fs.readFileSync(path.join(runDir, "refs", "MISSING.txt"), "utf8"), /gone\.md/);
    const lines = referenceLines(refs).join("\n");
    assert.match(lines, /do not search for them/);
    assert.ok(lines.includes(copy));
    assert.deepEqual(referenceLines([]), [], "no references, no prompt lines");
  } finally {
    cleanupDir(root);
    cleanupDir(repo.dir);
    fs.rmSync(docs, { recursive: true, force: true });
  }
});

test("references: find/grep/ls are limited to the checkout and the references", () => {
  const checkout = fs.mkdtempSync("/tmp/tt-search-co-");
  const refs = fs.mkdtempSync("/tmp/tt-search-refs-");
  try {
    const roots = [checkout, refs];
    assert.equal(guardedSearchPath(undefined, checkout, roots), undefined, "the default (cwd) is the checkout");
    assert.equal(guardedSearchPath("src", checkout, roots), undefined);
    assert.equal(guardedSearchPath(refs, checkout, roots), undefined);
    const home = guardedSearchPath(process.env.HOME ?? "/Users", checkout, roots);
    assert.match(home ?? "", /search only inside your checkout or the plan's reference documents/);
    assert.ok(guardedSearchPath("../..", checkout, roots), "escaping upwards is refused");
    assert.equal(guardedSearchPath(process.env.HOME, checkout, []), undefined, "no roots configured: no restriction");
  } finally {
    fs.rmSync(checkout, { recursive: true, force: true });
    fs.rmSync(refs, { recursive: true, force: true });
  }
});

test("shallow: a run refuses to start on a shallow clone, naming the fix", () => {
  const origin = makeRepo();
  const root = makeRunRoot();
  const shallow = fs.mkdtempSync("/tmp/tt-shallow-");
  try {
    execFileSync("git", ["-C", origin.dir, "commit", "-q", "--allow-empty", "-m", "second"], {
      env: { ...process.env, GIT_AUTHOR_NAME: "t", GIT_AUTHOR_EMAIL: "t@t", GIT_COMMITTER_NAME: "t", GIT_COMMITTER_EMAIL: "t@t" },
    });
    fs.rmSync(shallow, { recursive: true, force: true });
    execFileSync("git", ["clone", "-q", "--depth", "1", `file://${origin.dir}`, shallow]);
    assert.throws(() => createRun(root, plan(shallow)), /shallow clone.*fetch --unshallow/);
    assert.equal(fs.readdirSync(root).length, 0, "no run directory is left behind");
    assert.ok(createRun(root, plan(origin.dir)), "a full clone starts normally");
  } finally {
    cleanupDir(root);
    cleanupDir(origin.dir);
    fs.rmSync(shallow, { recursive: true, force: true });
  }
});
