// R11/C3: no stopped or terminal lane is removed before its edits are durable.
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { createWorktree, headOf, removeWorktree } from "./git.ts";

function git(worktree: string, args: string[], env = process.env, input?: Buffer): string {
  return execFileSync("git", ["--literal-pathspecs", "-C", worktree, ...args], { encoding: "utf8", env, input, stdio: ["pipe", "pipe", "pipe"] }).trim();
}
const IDENTITY = ["-c", "user.name=tradeoffs-trace", "-c", "user.email=tradeoffs-trace@localhost", "-c", "core.hooksPath=/dev/null"];
export interface SavedWorktree { kind: "stash" | "snapshot"; ref: string }

/** A stash preserves the index/worktree split. An unmerged index cannot be
 * stashed: fall back to a commit of the actual working files (including new
 * files and partial conflict resolutions). Neither save changes the real index.
 * Clean checkouts need no backup. Errors propagate BEFORE any removal. */
export function saveWorktreeState(repo: string, worktree: string, ref: string): SavedWorktree | undefined {
  if (git(worktree, ["status", "--porcelain", "--untracked-files=all"]).length === 0) return undefined;
  const temp = fs.mkdtempSync(path.join(os.tmpdir(), "tt-kept-index-"));
  try {
    const index = path.resolve(worktree, git(worktree, ["rev-parse", "--git-path", "index"]));
    const tmpIndex = path.join(temp, "index");
    fs.copyFileSync(index, tmpIndex);
    const env = { ...process.env, GIT_INDEX_FILE: tmpIndex };
    const untracked = execFileSync("git", ["-C", worktree, "ls-files", "--others", "--exclude-standard", "-z"]);
    if (untracked.length > 0) git(worktree, ["add", "--pathspec-from-file=-", "--pathspec-file-nul"], env, untracked);
    let sha = "";
    try { sha = git(worktree, [...IDENTITY, "stash", "create"], env); } catch { /* unmerged index: snapshot below */ }
    const kind = sha.length > 0 ? "stash" : "snapshot";
    if (kind === "snapshot") {
      git(worktree, ["add", "-A"], env);
      const tree = git(worktree, ["write-tree"], env);
      sha = git(worktree, [...IDENTITY, "commit-tree", tree, "-p", "HEAD", "-m", "tt kept worktree snapshot"], env);
    }
    // Saved candidate checkouts are independent clones, unlike lane worktrees.
    // Import their backup objects before publishing the ref in the run's repo.
    try { git(repo, ["cat-file", "-e", `${sha}^{commit}`]); }
    catch { git(repo, ["fetch", "--no-tags", "--no-write-fetch-head", worktree, sha]); }
    git(repo, ["update-ref", ref, sha]);
    return { kind, ref };
  } finally {
    fs.rmSync(temp, { recursive: true, force: true });
  }
}

/** Reuse a stopped lane, or save and restore its edits on the new base.
 * All saved refs survive success, conflict AND worktree-creation failure. */
export function resumeKeptWorktree(repo: string, worktree: string, base: string, ref: string): string {
  if (headOf(worktree) === base) return "is unchanged, so your uncommitted edits are still in it. Continue from them; do not start over.";
  const saved = saveWorktreeState(repo, worktree, ref);
  try {
    removeWorktree(repo, worktree);
    createWorktree(repo, worktree, base);
  } catch (err) {
    throw new Error(`${saved ? `kept edits saved to ${ref}` : "clean kept worktree"}; worktree recreation failed: ${String((err as Error)?.message ?? err)}`);
  }
  if (saved === undefined) return `was clean and recreated on new base ${base}; there were no uncommitted edits to restore.`;
  try {
    if (saved.kind === "snapshot") {
      // A snapshot is not a stash/three-way merge. Restore its whole file tree,
      // including new files absent from the base; HEAD/index stay on the base.
      git(worktree, ["restore", `--source=${ref}`, "--worktree", "--", "."]);
      return `was recreated on new base ${base}; your full working-file snapshot (including partial conflict resolutions and new files) was restored from ${ref}. The prior unmerged index was not restored; inspect the snapshot's differences against the new base.`;
    }
    try {
      git(worktree, ["stash", "apply", "--index", ref]);
    } catch {
      // A failed index restore may have partially applied the stash. Discard
      // only that restore attempt (all its data is in ref), then retry on the
      // clean base. No worker has started editing this recreated checkout yet.
      git(worktree, ["reset", "--hard", base]);
      git(worktree, ["clean", "-fd"]);
      git(worktree, ["stash", "apply", ref]);
      return `was recreated on new base ${base}; your uncommitted edits were re-applied from a stash, but the staged/unstaged split was not restored. The saved ref ${ref} keeps the original index as its second parent.`;
    }
    return `was recreated on new base ${base}; your uncommitted edits were re-applied from a stash, including their staged/unstaged split. A saved copy remains at ${ref}.`;
  } catch {
    return `is on new base ${base}, but your edits did not apply cleanly. The ${saved.kind} is saved to the exact ref ${ref}; inspect the worktree conflicts and recover from that ref before continuing.${saved.kind === "stash" ? " The staged/unstaged split was not restored; the saved ref keeps the original index as its second parent." : ""}`;
  }
}
