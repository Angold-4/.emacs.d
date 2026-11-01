import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";
import { createWorktree, removeWorktree } from "../../src/effects/git.ts";
import { pruneRunTargets } from "../../src/effects/prune.ts";
import { resumeKeptWorktree } from "../../src/effects/kept-worktree.ts";

test("a kept lane with an unmerged index is saved, never pruned unsaved", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-kept-unmerged-"));
  const repo = path.join(dir, "repo"), run = path.join(dir, "run");
  const git = (...args: string[]) => execFileSync("git", ["-C", repo, ...args], { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();
  const commit = (text: string) => {
    fs.writeFileSync(path.join(repo, "file"), text);
    git("add", "-A"); git("-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", text);
  };
  try {
    fs.mkdirSync(repo); fs.mkdirSync(run);
    git("init", "-q", "-b", "main"); commit("base\n");
    git("checkout", "-qb", "other"); commit("other\n");
    git("checkout", "-q", "main"); commit("ours\n");
    const lanes = ["a", "b"].map((lane) => path.join(run, "worktrees", `lane-${lane}`));
    for (const wt of lanes) {
      createWorktree(repo, wt, git("rev-parse", "HEAD"));
      assert.throws(() => execFileSync("git", ["-C", wt, "-c", "user.name=t", "-c", "user.email=t@t", "merge", "other"], { stdio: "pipe" }));
      fs.writeFileSync(path.join(wt, "file"), "unique partial resolution\n");
      fs.writeFileSync(path.join(wt, "new"), "new uncommitted file\n");
    }
    git("-c", "user.name=t", "-c", "user.email=t@t", "commit", "--allow-empty", "-qm", "new base");
    const base = git("rev-parse", "HEAD");
    const keptRef = "refs/tt/unmerged/kept/a/snapshot";
    const note = resumeKeptWorktree(repo, lanes[0], base, keptRef);
    assert.match(note, /full working-file snapshot.*restored/);
    assert.match(note, /unmerged index was not restored/);
    assert.equal(fs.readFileSync(path.join(lanes[0], "new"), "utf8"), "new uncommitted file\n");
    assert.equal(fs.readFileSync(path.join(lanes[0], "file"), "utf8"), "unique partial resolution\n");
    assert.equal(git("show", `${keptRef}:file`), "unique partial resolution");
    // Even a total save failure must retain the dirty checkout at terminal
    // prune; a later successful prune saves both resumed AND unmerged lanes.
    pruneRunTargets(run, { lanes }, (wt) => removeWorktree(repo, wt), { repo: path.join(dir, "missing"), runId: "unmerged" });
    assert.ok(lanes.every((wt) => fs.existsSync(wt)));
    assert.match(fs.readFileSync(path.join(run, "conductor.log"), "utf8"), /kept .*lane-b: could not save uncommitted state/);
    pruneRunTargets(run, { lanes }, (wt) => removeWorktree(repo, wt), { repo, runId: "unmerged" });
    assert.ok(lanes.every((wt) => !fs.existsSync(wt)));
    const refs = git("for-each-ref", "--format=%(refname)", "refs/tt/unmerged/pruned").split("\n");
    assert.equal(refs.length, 2);
    for (const ref of refs) {
      assert.equal(git("show", `${ref}:file`), "unique partial resolution");
      assert.equal(git("show", `${ref}:new`), "new uncommitted file");
    }
    assert.match(fs.readFileSync(path.join(run, "conductor.log"), "utf8"), /saved .*lane-b as snapshot to refs\/tt\/unmerged\/pruned/);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("a resumed kept lane restores index-only edits and reports any lost staged split", async (t) => {
  for (const indexConflict of [false, true]) await t.test(indexConflict ? "plain-apply fallback names the saved index" : "MM index-only content survives", () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-kept-index-only-"));
    const repo = path.join(dir, "repo"), wt = path.join(dir, "lane");
    const git = (cwd: string, ...args: string[]) => execFileSync("git", ["-C", cwd, ...args], { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();
    const commit = () => { git(repo, "add", "-A"); git(repo, "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "base"); };
    try {
      fs.mkdirSync(repo); git(repo, "init", "-q");
      fs.writeFileSync(path.join(repo, "tracked"), "base\n"); commit();
      createWorktree(repo, wt, git(repo, "rev-parse", "HEAD"));
      fs.writeFileSync(path.join(wt, "tracked"), "staged work\n"); git(wt, "add", "tracked");
      fs.writeFileSync(path.join(wt, "tracked"), "base\n");
      fs.writeFileSync(path.join(wt, "new"), "new file\n");
      assert.match(git(wt, "status", "--porcelain"), /MM tracked/);
      fs.writeFileSync(path.join(repo, indexConflict ? "tracked" : "unrelated"), indexConflict ? "staged work\n" : "new base\n"); commit();
      const base = git(repo, "rev-parse", "HEAD"), ref = "refs/tt/index/kept/a/saved";
      const note = resumeKeptWorktree(repo, wt, base, ref);
      assert.equal(git(repo, "show", `${ref}^2:tracked`), "staged work", "the saved ref keeps the original index");
      assert.equal(git(wt, "rev-parse", "HEAD"), base);
      assert.equal(fs.readFileSync(path.join(wt, "new"), "utf8"), "new file\n");
      if (indexConflict) {
        assert.match(note, /staged\/unstaged split was not restored/);
        assert.ok(note.includes(ref));
        assert.match(note, /original index as its second parent/);
      } else {
        assert.match(git(wt, "status", "--porcelain"), /MM tracked/);
        assert.equal(git(wt, "show", ":tracked"), "staged work");
        assert.equal(fs.readFileSync(path.join(wt, "tracked"), "utf8"), "base\n");
        assert.match(note, /including their staged\/unstaged split/);
      }
    } finally { fs.rmSync(dir, { recursive: true, force: true }); }
  });
});

test("a kept-lane recreation failure retains its saved ref, and a save failure retains the dirty checkout", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-kept-failed-"));
  const repo = path.join(dir, "repo"), wt = path.join(dir, "lane");
  const git = (...args: string[]) => execFileSync("git", ["-C", repo, ...args], { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();
  try {
    fs.mkdirSync(repo);
    git("init", "-q");
    fs.writeFileSync(path.join(repo, "tracked"), "base\n");
    git("add", "-A");
    git("-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "base");
    createWorktree(repo, wt, git("rev-parse", "HEAD"));
    fs.writeFileSync(path.join(wt, "tracked"), "dirty\n");
    fs.writeFileSync(path.join(wt, "new"), "untracked\n");
    // update-ref refuses this invalid name: nothing may be removed yet.
    assert.throws(() => resumeKeptWorktree(repo, wt, "invalid-base", "bad ref"));
    assert.equal(fs.readFileSync(path.join(wt, "tracked"), "utf8"), "dirty\n");
    assert.equal(fs.readFileSync(path.join(wt, "new"), "utf8"), "untracked\n");
    const ref = "refs/tt/test/kept/a/saved";
    assert.throws(() => resumeKeptWorktree(repo, wt, "invalid-base", ref), /kept edits saved to refs\/tt\/test\/kept\/a\/saved/);
    assert.equal(git("show", `${ref}:tracked`), "dirty");
    assert.equal(git("show", `${ref}:new`), "untracked");
    createWorktree(repo, wt, git("rev-parse", "HEAD"));
    git("-c", "user.name=t", "-c", "user.email=t@t", "commit", "--allow-empty", "-qm", "new base");
    const base = git("rev-parse", "HEAD");
    assert.match(resumeKeptWorktree(repo, wt, base, ref), /was clean and recreated/);
    assert.equal(execFileSync("git", ["-C", wt, "rev-parse", "HEAD"], { encoding: "utf8" }).trim(), base);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});
