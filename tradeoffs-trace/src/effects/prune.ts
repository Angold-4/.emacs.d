// Plan 07h2 (A3/R4): a finished run leaves nothing big behind.
//
// Run d170bb37 stayed alive 7.5 hours after DONE and had 134 GB of build
// targets on disk: a terminal run has no use for either. So a conductor that
// reaches DONE or BLOCKED does two things before its process exits:
//
//   1. it removes the run's own worktree and every lane worktree under
//      `<run>/worktrees/` (they are checkouts of commits that are already in
//      `candidates/<sha>` or on the run's branch — nothing is lost), and
//   2. it removes the BUILD TARGET directories inside the run directories it
//      owns (`<run>/candidates/<sha>/target`, `node_modules`, `dist`, …).
//
// What is NOT pruned: the run's control log, its views, its recorded check
// outputs, its candidate checkouts and its plan snapshot. Those are the run's
// evidence, and `tt status`/`tt review` read them after DONE.
//
// The prune is deliberately conservative about what counts as a build target:
// only the well-known directories in `BUILD_TARGET_DIRS`, only inside this
// run's own directory, and never a path the caller did not name.
//
// A repository is not flat, so the walk is not depth-limited: a node's build
// target may sit under a package directory (`crates/api/target`,
// `packages/web/node_modules`) as easily as at the checkout's root. The walk
// therefore visits every directory of a candidate checkout, never descends
// into `.git` or a symbolic link (so it cannot loop), and still removes only
// the names in `BUILD_TARGET_DIRS`.

import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";

/** Directory names that hold only build products in the ecosystems tradeoffs
 * trace runs (rust, node, python, go, java). A name match alone is not enough
 * — `pruneRunTargets` only ever looks inside `<run>/candidates/<sha>` and the
 * worktrees it is about to delete anyway. Candidate folders holding tracked
 * files are always kept. */
export const BUILD_TARGET_DIRS = ["target", "node_modules", "dist", "build", ".next", "coverage", "__pycache__", ".pytest_cache"] as const;

/** How many directories one candidate checkout's walk may visit. A bound on
 * a pathological tree (a checkout is not supposed to contain millions of
 * directories); it is not a depth limit, so a nested package's target is
 * reached at any depth. */
const MAX_VISITED = 200_000;

export interface PruneResult {
  /** Every directory this prune removed, in the order it removed them. */
  removed: string[];
}

function rmDir(dir: string, removed: string[]): void {
  // A candidate checkout is materialized READ-ONLY (a check must not be able to
  // edit it), so the build targets inside it cannot be removed until the
  // directories are writable again — including the target's parent, which is
  // what a recursive remove needs write access to.
  makeWritable(path.dirname(dir));
  makeWritable(dir);
  try {
    fs.rmSync(dir, { recursive: true, force: true, maxRetries: 3 });
    removed.push(dir);
  } catch {
    // A directory that will not go away (a reader holding a file open) is not
    // a reason to keep the conductor alive; the next prune tries again.
  }
}

/** Best effort: give `dir` and everything under it its owner's write bit back,
 * so a read-only candidate checkout's build targets can be removed. */
function makeWritable(dir: string): void {
  try {
    fs.chmodSync(dir, 0o755);
  } catch {
    // not there, or not ours
  }
  let entries: fs.Dirent[];
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      // A recursive remove descends into every directory it deletes, so each
      // one needs write permission, not just the target itself.
      makeWritable(full);
      continue;
    }
    try {
      fs.chmodSync(full, 0o644);
    } catch {
      // a file that cannot be chmodded is not fatal: the remove reports it
    }
  }
}

function pruneTargetsUnder(checkout: string, dir: string, visited: { count: number }, removed: string[], log: (line: string) => void): void {
  if (visited.count >= MAX_VISITED) return;
  visited.count += 1;
  let entries: fs.Dirent[];
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const entry of entries) {
    // `isDirectory()` is false for a symbolic link, so the walk never follows
    // one out of the checkout (and never loops through one).
    if (!entry.isDirectory()) continue;
    if (entry.name === ".git") continue;
    const full = path.join(dir, entry.name);
    if ((BUILD_TARGET_DIRS as readonly string[]).includes(entry.name)) {
      const relative = path.relative(checkout, full);
      try {
        const tracked = execFileSync("git", ["-C", checkout, "ls-files", "-z", "--", relative], { stdio: ["ignore", "pipe", "pipe"] });
        if (tracked.length > 0) {
          log(`kept ${full}: holds tracked files`);
          continue;
        }
      } catch {
        // If git cannot establish that a folder is generated, keep it.
        log(`kept ${full}: could not check tracked files`);
        continue;
      }
      rmDir(full, removed);
      continue;
    }
    pruneTargetsUnder(checkout, full, visited, removed, log);
  }
}

/** Plan 07h2 (A3/R4): remove a finished run's worktrees and build targets.
 * `runWorktree`/`laneWorktrees` are the worktree paths the conductor itself
 * created (it passes `runPaths(runDir).worktree` and `<run>/worktrees/*`);
 * `removeWorktree` is the git-aware remover, injected so this module never has
 * to know the plan's repo. Idempotent: a missing path is simply skipped. */
export function pruneRunTargets(
  runDir: string,
  worktrees: { run?: string; lanes?: string[] },
  removeWorktree: (dir: string) => void = (dir) => fs.rmSync(dir, { recursive: true, force: true }),
): PruneResult {
  const removed: string[] = [];
  const log = (line: string): void => {
    try {
      fs.appendFileSync(path.join(runDir, "conductor.log"), `${new Date().toISOString()} prune ${line}\n`);
    } catch {
      // Logging is best effort; preserving tracked content is not.
    }
  };
  for (const worktree of [worktrees.run, ...(worktrees.lanes ?? [])]) {
    if (worktree === undefined || !fs.existsSync(worktree)) continue;
    try {
      removeWorktree(worktree);
      removed.push(worktree);
    } catch {
      rmDir(worktree, removed);
    }
  }
  // The lane worktrees' parent, once every lane inside it is gone.
  const lanesDir = path.join(runDir, "worktrees");
  if (fs.existsSync(lanesDir) && fs.readdirSync(lanesDir).length === 0) rmDir(lanesDir, removed);
  const candidates = path.join(runDir, "candidates");
  if (fs.existsSync(candidates)) {
    for (const entry of fs.readdirSync(candidates, { withFileTypes: true })) {
      if (!entry.isDirectory()) continue;
      // One visit budget per candidate checkout, so one huge tree does not
      // starve the next.
      const checkout = path.join(candidates, entry.name);
      pruneTargetsUnder(checkout, checkout, { count: 0 }, removed, log);
    }
  }
  return { removed };
}
