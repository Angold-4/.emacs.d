import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";
import { createWorktree } from "../../src/effects/git.ts";
import { resumeKeptWorktree } from "../../src/effects/kept-worktree.ts";

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
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});
