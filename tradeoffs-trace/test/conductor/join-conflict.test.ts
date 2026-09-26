// Programs 14 and 01: a join node whose bases conflict was BLOCKED with only
// the conflicting files named. The message now says how to unblock it, and a
// hand-made merge at the node's branch is what the scheduler then uses.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { cleanupDir, makeRepo } from "./harness.ts";
import { prepareBranch } from "../../src/program.ts";

function git(dir: string, ...args: string[]): string {
  return execFileSync("git", ["-C", dir, "-c", "user.name=t", "-c", "user.email=t@t", ...args], { encoding: "utf8" }).trim();
}

test("join conflict: the blocked reason names the fix, and a hand-made merge at the branch is kept", () => {
  const repo = makeRepo();
  try {
    for (const [b, text] of [["left", "left\n"], ["right", "right\n"]] as const) {
      git(repo.dir, "checkout", "-q", "-b", b, "main");
      fs.writeFileSync(path.join(repo.dir, "shared.txt"), text);
      git(repo.dir, "add", "shared.txt");
      git(repo.dir, "commit", "-q", "-m", b);
    }
    git(repo.dir, "checkout", "-q", "main");
    const blocked = prepareBranch(repo.dir, "main--join", ["left", "right"], "join");
    assert.equal(blocked.ok, false);
    const reason = (blocked as { reason: string }).reason;
    assert.match(reason, /conflicts/);
    assert.match(reason, /To unblock: merge left \+ right by hand into a new branch main--join, then `tt program retry <program> join`/);

    // The owner's hand-made merge: the scheduler keeps it as the branch.
    git(repo.dir, "checkout", "-q", "-b", "main--join", "left");
    try {
      git(repo.dir, "merge", "-q", "right", "-m", "merge");
    } catch {
      fs.writeFileSync(path.join(repo.dir, "shared.txt"), "left\nright\n");
      git(repo.dir, "add", "shared.txt");
      git(repo.dir, "commit", "-q", "-m", "merge right for join");
    }
    const merged = git(repo.dir, "rev-parse", "HEAD");
    git(repo.dir, "checkout", "-q", "main");
    assert.deepEqual(prepareBranch(repo.dir, "main--join", ["left", "right"], "join"), { ok: true });
    assert.equal(git(repo.dir, "rev-parse", "main--join"), merged, "the hand-made merge is used as-is");
  } finally {
    cleanupDir(repo.dir);
  }
});
