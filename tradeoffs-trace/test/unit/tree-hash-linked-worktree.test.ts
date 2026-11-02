import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { test } from "node:test";

import { verifyIntegrity } from "../../src/effects/git.ts";

function git(cwd: string, ...args: string[]): string {
  return execFileSync("git", ["-C", cwd, ...args], { encoding: "utf8" }).trim();
}

// A plan's repo may be a linked worktree (`git worktree add`), whose git dir
// holds no objects of its own; the integrity check must still read the
// candidate's tree from the shared object store.
test("verifyIntegrity: a linked-worktree repo still verifies an intact checkout", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-linked-"));
  try {
    const main = path.join(root, "main");
    fs.mkdirSync(main);
    git(main, "init", "-q");
    git(main, "config", "user.email", "t@example.com");
    git(main, "config", "user.name", "t");
    fs.writeFileSync(path.join(main, "a.txt"), "one\n");
    git(main, "add", "a.txt");
    git(main, "commit", "-q", "-m", "one");
    const sha = git(main, "rev-parse", "HEAD");

    const linked = path.join(root, "linked");
    git(main, "worktree", "add", "-q", "--detach", linked, sha);

    const checkout = path.join(root, "checkout");
    git(main, "worktree", "add", "-q", "--detach", checkout, sha);

    assert.ok(verifyIntegrity(linked, checkout, sha), "an untouched checkout matches through a linked-worktree repo");
    fs.writeFileSync(path.join(checkout, "a.txt"), "two\n");
    assert.ok(!verifyIntegrity(linked, checkout, sha), "a modified tracked file still fails");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
