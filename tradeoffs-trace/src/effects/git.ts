// Git effects: worktrees, the freeze commit, read-only/disposable
// checkouts, integrity verification and the probe/publish integration
// steps (design §6.2, §6.4, §9.1, §9.3).
//
// Every commit made by this module (freeze, probe merge) is authored with
// `-c user.name=… -c user.email=…` rather than relying on any global git
// config, per the environment note in the phase brief, and with
// `-c core.hooksPath=/dev/null` plus `--no-verify` so a repository's own
// hooks never run for a conductor-made commit.
//
// **Checkout strategy.** `materializeCandidate` and `disposableCheckout`
// both produce a plain `git clone --no-checkout` of the origin repo
// (`--no-hardlinks`, so chmod'ing a materialized checkout read-only can
// never affect the source repo's own object files) followed by
// `checkout --detach <sha>`. This is deliberately *not* `git worktree add`:
// a worktree is registered in the origin repo's administrative state and
// shares its ref/index locking, which is exactly the kind of link back to
// the worker's live checkout design §6.2 wants severed ("that must not be
// a worktree the worker can commit into"). A clone is a fully independent
// repository on disk; deleting its directory leaves no trace in the
// origin repo at all.
//
// `probe` is the one place that *does* use `git worktree add`, because a
// probe's merge commit must be reachable from the origin repo's own object
// database (so that `publishCAS`'s `update-ref` can point a branch at it
// without an import step), and the probe branch itself lives in the origin
// repo's refs.

import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

const IDENTITY_ARGS = ["-c", "user.name=tradeoffs-trace", "-c", "user.email=tradeoffs-trace@localhost"];
const NO_HOOKS_ARGS = ["-c", "core.hooksPath=/dev/null"];

function git(args: string[], opts: { cwd?: string; env?: NodeJS.ProcessEnv } = {}): string {
  return execFileSync("git", args, { encoding: "utf8", cwd: opts.cwd, env: opts.env }).trim();
}

function gitCapturingFailure(args: string[], opts: { cwd?: string } = {}): { ok: boolean; output: string } {
  try {
    const output = execFileSync("git", args, { encoding: "utf8", cwd: opts.cwd });
    return { ok: true, output };
  } catch (err) {
    const e = err as { stdout?: string | Buffer; stderr?: string | Buffer };
    const stdout = e.stdout ? e.stdout.toString() : "";
    const stderr = e.stderr ? e.stderr.toString() : "";
    return { ok: false, output: stdout + stderr };
  }
}

// ---------------------------------------------------------------------------
// Worktrees (the worker's live, editable checkout)
// ---------------------------------------------------------------------------

/** Creates a real git worktree at `worktreePath`, checked out from
 * `baseSha`. When `branch` is given, a new branch by that name is created
 * pointing at `baseSha` and checked out; otherwise the worktree is a
 * detached checkout of `baseSha`. This is the worker's live, editable
 * directory. */
export function createWorktree(repo: string, worktreePath: string, baseSha: string, branch?: string): void {
  const args = ["-C", repo, "worktree", "add", "-q"];
  if (branch) args.push("-b", branch);
  args.push(worktreePath, baseSha);
  git(args);
}

export function removeWorktree(repo: string, worktreePath: string): void {
  try {
    git(["-C", repo, "worktree", "remove", "--force", worktreePath]);
  } catch {
    // The directory may already be gone (e.g. after a force-kill); fall
    // back to removing it directly and pruning the registration.
    fs.rmSync(worktreePath, { recursive: true, force: true });
  }
  try {
    git(["-C", repo, "worktree", "prune"]);
  } catch {
    // Best effort.
  }
}

// ---------------------------------------------------------------------------
// The freeze boundary (design §6.2)
// ---------------------------------------------------------------------------

/** Commits everything currently in `worktree` (`git add -A` then commit),
 * with trailer `TT-Action: <actionId>` and hooks disabled. Uses
 * `--allow-empty` because a freeze always happens once `submit_phase`
 * arrives, whether or not the attempt actually changed anything. Returns
 * the new commit's sha — the candidate C. */
export function freezeCommit(worktree: string, actionId: string, message: string): string {
  git(["-C", worktree, "add", "-A"]);
  git([
    "-C",
    worktree,
    ...IDENTITY_ARGS,
    ...NO_HOOKS_ARGS,
    "commit",
    "--no-verify",
    "--allow-empty",
    "-m",
    `${message}\n\nTT-Action: ${actionId}`,
  ]);
  return git(["-C", worktree, "rev-parse", "HEAD"]);
}

/** Finds the commit (searching all refs) whose `TT-Action` trailer equals
 * `actionId`, for recovery after a crash between the freeze commit and its
 * completion event (design §9.3: "worktree HEAD carries trailer
 * `TT-Action: <id>` → record it"). Returns `undefined` if none is found. */
export function findCommitByTrailer(worktree: string, actionId: string): string | undefined {
  const out = git([
    "-C",
    worktree,
    "log",
    "--all",
    "--format=%H\x01%(trailers:key=TT-Action,valueonly,separator=%x2C)",
  ]);
  if (out.length === 0) return undefined;
  for (const line of out.split("\n")) {
    const sep = line.indexOf("\x01");
    if (sep === -1) continue;
    const sha = line.slice(0, sep);
    const trailer = line.slice(sep + 1).trim();
    if (trailer === actionId) return sha;
  }
  return undefined;
}

// ---------------------------------------------------------------------------
// Checkouts: read-only for reviewers, disposable for checks/reproductions
// ---------------------------------------------------------------------------

function cloneCheckout(repo: string, sha: string, dir: string): void {
  fs.mkdirSync(path.dirname(dir), { recursive: true });
  // A local clone copies the object files one by one; a concurrent gc in
  // the origin repo can delete a loose object mid-copy ("failed to copy
  // file ... No such file or directory", run 0c99b1ff's first freeze). One
  // retry after the repack settles. (`--no-local` would avoid the race but
  // only transfers objects reachable from refs, and candidates are not.)
  const clone = () => git(["clone", "-q", "--no-hardlinks", "--no-checkout", repo, dir]);
  try {
    clone();
  } catch {
    fs.rmSync(dir, { recursive: true, force: true });
    clone();
  }
  git(["-C", dir, "checkout", "-q", "--detach", sha]);
}

/** Collects regular files and directories under `root`, **never following
 * symlinks**: a symlink is neither a file to chmod nor a directory to
 * descend into. (A symlink's own mode is irrelevant on every platform this
 * runs on, and `chmod` would otherwise follow it to an external target.) */
function walkPaths(root: string): { files: string[]; dirs: string[] } {
  const files: string[] = [];
  const dirs: string[] = [];
  const stack = [root];
  while (stack.length > 0) {
    const current = stack.pop()!;
    const entries = fs.readdirSync(current, { withFileTypes: true });
    for (const entry of entries) {
      const full = path.join(current, entry.name);
      if (entry.isSymbolicLink()) continue; // never follow or chmod a link
      if (entry.isDirectory()) {
        dirs.push(full);
        stack.push(full);
      } else {
        files.push(full);
      }
    }
  }
  dirs.push(root);
  return { files, dirs };
}

/** Toggles just the owning user's write bit on `p`, preserving every other
 * permission bit (executable bits above all: git records a tracked file's
 * exec bit in its tree, so losing it would corrupt the tree hash). Symlinks
 * are skipped entirely — `chmod` follows them and would change an external
 * target's mode. */
function setOwnerWriteBit(p: string, writable: boolean): void {
  const st = fs.lstatSync(p);
  if (st.isSymbolicLink()) return;
  const perms = st.mode & 0o7777;
  const next = writable ? perms | 0o200 : perms & ~0o200;
  if (next !== perms) fs.chmodSync(p, next);
}

/** Recursively toggles the owner-write bit on every regular file and
 * directory under `dir`, making a tree writable (`u+w`) or read-only
 * (clearing `u+w`). Executable and all other bits are preserved, and
 * symlinks are never chmod'ed or followed. Exported so tests can force a
 * materialized candidate writable again before tampering with it (the
 * `integrity-detect` exit-gate test does exactly that). */
export function setTreeWritable(dir: string, writable: boolean): void {
  const { files, dirs } = walkPaths(dir);
  for (const file of files) {
    try {
      setOwnerWriteBit(file, writable);
    } catch {
      // Best effort — a file removed mid-walk, etc.
    }
  }
  for (const d of dirs) {
    try {
      setOwnerWriteBit(d, writable);
    } catch {
      // Best effort.
    }
  }
}

/** Materializes a read-only checkout of candidate `sha` under `dir`, for
 * reviewers. It is a plain clone (see the module comment), not a `git
 * worktree`, and every file and directory under it is made non-writable.
 * This protects against accidental writes, not a determined adversary
 * (design §2.2's own caveat: same-account processes can still chmod their
 * own files back). */
export function materializeCandidate(repo: string, sha: string, dir: string): void {
  cloneCheckout(repo, sha, dir);
  setTreeWritable(dir, false);
}

export interface DisposableCheckout {
  dir: string;
  dispose: () => void;
}

/** A fresh, writable checkout of `sha`, for one check or reproduction run.
 * Builds write files, so each one gets its own directory, created here and
 * removed by calling `dispose()`. */
export function disposableCheckout(repo: string, sha: string): DisposableCheckout {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-checkout-"));
  cloneCheckout(repo, sha, dir);
  return {
    dir,
    dispose: () => {
      // In case a check left anything read-only (or the caller called
      // materializeCandidate-style helpers on it), make sure removal
      // cannot fail on permissions.
      try {
        setTreeWritable(dir, true);
      } catch {
        // Best effort.
      }
      fs.rmSync(dir, { recursive: true, force: true });
    },
  };
}

// ---------------------------------------------------------------------------
// Integrity (design §2.2, §6.2)
// ---------------------------------------------------------------------------

/** The tree hash `dir`'s content would have if committed on top of
 * candidate `sha`, computed **without trusting anything in `dir`'s own
 * `.git`** — no `git status`, no reliance on `dir`'s HEAD, its index, or
 * any `update-index --assume-unchanged` bit set in it. A same-account
 * process that can write to `dir` can make its own `.git` lie (spoof HEAD,
 * mark a tampered file assume-unchanged, forge a clean `git status`); none
 * of that is consulted here.
 *
 * Mechanism: a scratch bare git directory with a fresh temporary index
 * (`GIT_INDEX_FILE`), `GIT_WORK_TREE=dir`, and `GIT_ALTERNATE_OBJECT_DIRECTORIES`
 * pointing at `repo`'s own object store (so the scratch dir can resolve
 * `sha` without importing anything). The index is **seeded from the
 * candidate's own tree** with `git read-tree <sha>` — not built from
 * scratch — specifically so every tracked path of the candidate is
 * present up front, with no filesystem stat cached against it (a tree has
 * no mtimes). `git add -A` then restats and rehashes `dir`'s actual
 * on-disk content against that seeded index:
 *
 * - A **modified or deleted tracked file** always changes the tree,
 *   because there is no shortcut for git to trust: the seeded index entry
 *   carries no real stat, so the "unchanged, skip rehashing" fast path
 *   never fires (this is what catches both a same-size edit with the
 *   original mtime restored, and an `assume-unchanged` bit set in `dir`'s
 *   own — unconsulted — index).
 * - `.gitignore` never hides a **tracked** path from `add -A` (git only
 *   ever applies ignore rules to untracked paths), so a `.gitignore` added
 *   or edited inside `dir` cannot hide a modification to a file the
 *   candidate already tracks.
 * - A genuinely **new, untracked** file is still subject to `dir`'s
 *   current `.gitignore` rules, same as any `git add -A`. So a worker (or
 *   a tampering process) that adds both a new file and a new `.gitignore`
 *   rule that covers it can hide that specific new file from this check.
 *   That is a real, documented limit: detection for a cooperative worker
 *   (design §2.2), not containment against a deliberate adversary who can
 *   already write anywhere the owning OS account can write. */
export function treeHashOf(repo: string, dir: string, sha: string): string {
  const objectsDir = path.join(git(["-C", repo, "rev-parse", "--absolute-git-dir"]), "objects");
  const scratchGitDir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-treehash-"));
  try {
    git(["init", "-q", "--bare", scratchGitDir]);
    const env: NodeJS.ProcessEnv = {
      ...process.env,
      GIT_DIR: scratchGitDir,
      GIT_WORK_TREE: dir,
      GIT_INDEX_FILE: path.join(scratchGitDir, "index"),
      GIT_ALTERNATE_OBJECT_DIRECTORIES: objectsDir,
    };
    git(["read-tree", sha], { cwd: dir, env });
    git(["add", "-A"], { cwd: dir, env });
    return git(["write-tree"], { cwd: dir, env });
  } finally {
    fs.rmSync(scratchGitDir, { recursive: true, force: true });
  }
}

/** True iff `dir`'s actual content, computed by `treeHashOf` (which
 * ignores `dir`'s own `.git` entirely — see its doc comment for exactly
 * what that does and does not defend against), equals candidate `sha`'s
 * real tree as recorded in `repo`. This is the only check: no HEAD
 * comparison against `dir`'s own (untrustworthy) idea of its HEAD, per
 * design §2.2's "no modified, added or deleted files, ignored build
 * output excepted." */
export function verifyIntegrity(repo: string, dir: string, sha: string): boolean {
  try {
    const expected = git(["-C", repo, "rev-parse", `${sha}^{tree}`]);
    const actual = treeHashOf(repo, dir, sha);
    return actual === expected;
  } catch {
    return false;
  }
}

// ---------------------------------------------------------------------------
// Integration: probe (before acceptance) and publish (after) — design §6.4
// ---------------------------------------------------------------------------

export interface ProbeSuccess {
  ok: true;
  /** The result of merging C onto H. Equals `candidateSha` on a fast
   * forward (the normal case for serial phases). */
  I: string;
  /** A live checkout (a real `git worktree`, since it must stay linked to
   * the origin repo for the merge commit to be reachable from it) of I,
   * for running `CHECKS` against. Discard with `discardProbe`. */
  checkoutDir: string;
  probeBranch: string;
  fastForward: boolean;
}

export interface ProbeConflict {
  ok: false;
  conflict: true;
  output: string;
}

export type ProbeResult = ProbeSuccess | ProbeConflict;

/** Creates the disposable branch `tt/<runId>/probe/<candidateSha>` at
 * `headSha` and merges `candidateSha` into it. On success, returns the
 * merge result I (fast-forwarded to C when possible) and a checkout of it;
 * the integration branch itself is never touched. On a genuine conflict,
 * the probe branch and its checkout are discarded before returning, and
 * `output` carries the merge's combined stdout/stderr. */
export function probe(
  repo: string,
  opts: { runId: string; candidateSha: string; headSha: string },
): ProbeResult {
  const { runId, candidateSha, headSha } = opts;
  const probeBranch = `tt/${runId}/probe/${candidateSha}`;
  const checkoutDir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-probe-"));
  git(["-C", repo, "worktree", "add", "-q", "-b", probeBranch, checkoutDir, headSha]);

  const merge = gitCapturingFailure(
    [
      "-C",
      checkoutDir,
      ...IDENTITY_ARGS,
      ...NO_HOOKS_ARGS,
      "merge",
      "--no-verify",
      "-m",
      `merge candidate ${candidateSha}\n\nTT-Action: probe-${runId}-${candidateSha}`,
      candidateSha,
    ],
    { cwd: checkoutDir },
  );

  if (!merge.ok) {
    try {
      git(["-C", checkoutDir, "merge", "--abort"]);
    } catch {
      // Nothing to abort, or already clean.
    }
    discardProbe(repo, { probeBranch, checkoutDir });
    return { ok: false, conflict: true, output: merge.output };
  }

  const I = git(["-C", checkoutDir, "rev-parse", "HEAD"]);
  return { ok: true, I, checkoutDir, probeBranch, fastForward: I === candidateSha };
}

/** Removes a probe's worktree checkout and its branch. Safe to call after
 * either outcome of `probe` (a conflict already discards internally, but
 * calling this again is harmless), and after a successful probe once its
 * checks have run and the phase has moved on. */
export function discardProbe(repo: string, target: { probeBranch: string; checkoutDir: string }): void {
  try {
    git(["-C", repo, "worktree", "remove", "--force", target.checkoutDir]);
  } catch {
    // Already removed.
  }
  fs.rmSync(target.checkoutDir, { recursive: true, force: true });
  try {
    git(["-C", repo, "branch", "-D", target.probeBranch]);
  } catch {
    // Already gone.
  }
  try {
    git(["-C", repo, "worktree", "prune"]);
  } catch {
    // Best effort.
  }
}

/** Recovery-only helper (design §9.3's probe row: "discard the probe branch
 * and its checkout"): rediscovers a probe's worktree checkout from just
 * `runId`/`candidateSha` — the deterministic branch name `probe` itself uses
 * (`tt/<runId>/probe/<candidateSha>`) — without needing the checkout
 * directory's (randomly generated) path, which a crashed conductor's
 * completion-less intent record never recorded. Safe to call whether or not
 * the branch/worktree still exist (a probe interrupted before `probe()`
 * even created them is a no-op here). */
export function discardProbeByBranch(repo: string, runId: string, candidateSha: string): void {
  const probeBranch = `tt/${runId}/probe/${candidateSha}`;
  let checkoutDir: string | undefined;
  try {
    const porcelain = git(["-C", repo, "worktree", "list", "--porcelain"]);
    let currentWorktree: string | undefined;
    for (const line of porcelain.split("\n")) {
      if (line.startsWith("worktree ")) currentWorktree = line.slice("worktree ".length);
      if (line === `branch refs/heads/${probeBranch}` && currentWorktree) {
        checkoutDir = currentWorktree;
        break;
      }
    }
  } catch {
    // No worktrees at all, or git failed listing them — nothing to find.
  }
  if (checkoutDir) {
    discardProbe(repo, { probeBranch, checkoutDir });
    return;
  }
  // No worktree registered for it (already removed, or never created) —
  // still make sure the branch itself is gone.
  try {
    git(["-C", repo, "branch", "-D", probeBranch]);
  } catch {
    // Already gone.
  }
}

export interface PublishSuccess {
  ok: true;
}
export interface PublishStale {
  ok: false;
  actualHead: string;
}
export type PublishResult = PublishSuccess | PublishStale;

/** Compare-and-swap: moves `refs/heads/<branch>` from `H` to `I` only if it
 * still points at `H` (design §6.4 publish steps 2–3). On failure, reports
 * the branch's actual current head; the branch itself is left untouched. */
export function publishCAS(repo: string, branch: string, I: string, H: string): PublishResult {
  const ref = `refs/heads/${branch}`;
  const attempt = gitCapturingFailure(["-C", repo, "update-ref", ref, I, H]);
  if (attempt.ok) return { ok: true };
  const actualHead = git(["-C", repo, "rev-parse", ref]);
  return { ok: false, actualHead };
}

// ---------------------------------------------------------------------------
// Diff introspection (work packet 2a: boundary triggers, §3.5 sampling)
// ---------------------------------------------------------------------------

/** Every path touched between `base` and `candidateSha` (design §3.3's
 * boundary-trigger inputs and §3.5's unreferenced-hunk sampling both start
 * from the same diff). Both shas must already be reachable in `repo`'s own
 * object database (true for a freeze commit made on a worktree of `repo`). */
export function diffNameOnly(repo: string, base: string, candidateSha: string): string[] {
  const out = git(["-C", repo, "diff", "--name-only", `${base}..${candidateSha}`]);
  return out.length === 0 ? [] : out.split("\n").filter((l) => l.length > 0);
}

/** The raw unified diff between `base` and `candidateSha`, for showing a
 * reviewer at turn 1 (design §3.3). */
export function diffText(repo: string, base: string, candidateSha: string): string {
  return git(["-C", repo, "diff", `${base}..${candidateSha}`]);
}

export interface DiffHunk {
  file: string;
  header: string; // the "@@ -a,b +c,d @@" line, verbatim
}

/** Parses `git diff -U0`'s own hunk headers into one entry per hunk, per
 * file (design §3.5: "diff hunks no decision/finding cites"). Deliberately
 * coarse — it does not track individual line numbers beyond the header
 * text itself, which is enough to name a hunk in the sample for a human or
 * phase 3's renderer to look at. */
export function diffHunks(repo: string, base: string, candidateSha: string): DiffHunk[] {
  const out = git(["-C", repo, "diff", "-U0", `${base}..${candidateSha}`]);
  const hunks: DiffHunk[] = [];
  let file = "";
  for (const line of out.split("\n")) {
    if (line.startsWith("+++ b/")) {
      file = line.slice("+++ b/".length);
    } else if (line.startsWith("@@ ")) {
      hunks.push({ file, header: line });
    }
  }
  return hunks;
}
