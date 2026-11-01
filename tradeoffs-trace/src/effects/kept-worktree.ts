// R11/C3: a kept lane is never removed until its dirty state is durable.
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { createWorktree, headOf, removeWorktree } from "./git.ts";

/** Reuse a stopped lane, or save and reapply its edits on the new base.
 * The saved ref is deliberately retained on success, conflict AND failure. */
export function resumeKeptWorktree(repo: string, worktree: string, base: string, ref: string): string {
  if (headOf(worktree) === base) return "is unchanged, so your uncommitted edits are still in it. Continue from them; do not start over.";
  const git = (args: string[], env = process.env, input?: Buffer): string =>
    execFileSync("git", ["-C", worktree, ...args], { encoding: "utf8", env, input, stdio: ["pipe", "pipe", "pipe"] }).trim();
  // `stash create` alone omits untracked files. Add them ONLY to a temporary
  // copy of the lane index: the real index/HEAD/worktree stay untouched until
  // update-ref succeeds. Tracked staged/unstaged edits retain their stash trees.
  const temp = fs.mkdtempSync(path.join(os.tmpdir(), "tt-kept-index-"));
  let saved: string;
  try {
    const index = path.resolve(worktree, git(["rev-parse", "--git-path", "index"]));
    const tmpIndex = path.join(temp, "index");
    fs.copyFileSync(index, tmpIndex);
    const env = { ...process.env, GIT_INDEX_FILE: tmpIndex };
    const untracked = execFileSync("git", ["-C", worktree, "ls-files", "--others", "--exclude-standard", "-z"]);
    if (untracked.length > 0) git(["add", "--pathspec-from-file=-", "--pathspec-file-nul"], env, untracked);
    saved = git(["-c", "user.name=tradeoffs-trace", "-c", "user.email=tradeoffs-trace@localhost", "stash", "create"], env);
    if (saved.length === 0) throw new Error("kept lane has no stashable state; refusing to remove it");
    execFileSync("git", ["-C", repo, "update-ref", ref, saved]);
  } finally {
    fs.rmSync(temp, { recursive: true, force: true });
  }
  // Everything above is non-destructive. Even a failure below leaves `ref`
  // reachable; no lane-worktree-failed path deletes it.
  try {
    removeWorktree(repo, worktree);
    createWorktree(repo, worktree, base);
  } catch (err) {
    throw new Error(`kept edits saved to ${ref}; worktree recreation failed: ${String((err as Error)?.message ?? err)}`);
  }
  try {
    git(["stash", "apply", ref]);
    return `was recreated on new base ${base}; your uncommitted edits were re-applied. A saved copy remains at ${ref}.`;
  } catch {
    return `is on new base ${base}, but your edits did not apply cleanly. They are saved to the exact ref ${ref}; inspect the worktree conflicts and recover from that ref before continuing.`;
  }
}
