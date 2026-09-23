import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, test } from "node:test";
import {
  createWorktree,
  discardProbe,
  disposableCheckout,
  findCommitByTrailer,
  freezeCommit,
  materializeCandidate,
  probe,
  publishCAS,
  removeWorktree,
  setTreeWritable,
  treeHashOf,
  verifyIntegrity,
} from "../../src/effects/git.ts";

let root: string;
let repo: string;

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

function initRepo(dir: string): void {
  git(["init", "-q", "-b", "main"], dir);
  fs.writeFileSync(path.join(dir, "f.txt"), "base\n");
  git(["add", "-A"], dir);
  git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], dir);
}

beforeEach(() => {
  root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-git-test-"));
  repo = path.join(root, "repo");
  fs.mkdirSync(repo);
  initRepo(repo);
});

afterEach(() => {
  // Anything materialized read-only needs write bits back before rm can
  // remove it; best-effort across the whole scratch root.
  try {
    setTreeWritable(root, true);
  } catch {
    // Best effort.
  }
  fs.rmSync(root, { recursive: true, force: true });
});

test("freeze-boundary: live-worktree writes never appear in materializeCandidate or disposableCheckout of that sha", () => {
  const worktreePath = path.join(root, "worktree1");
  const baseSha = git(["rev-parse", "HEAD"], repo);
  createWorktree(repo, worktreePath, baseSha, "work");

  fs.appendFileSync(path.join(worktreePath, "f.txt"), "worker-change\n");
  fs.writeFileSync(path.join(worktreePath, "new.txt"), "worker-added\n");
  git(["add", "-A"], worktreePath);
  const candidateSha = freezeCommit(worktreePath, "act-freeze-1", "freeze");

  // Now mutate the live worktree AFTER the freeze: new file, modified
  // file, deleted file.
  fs.appendFileSync(path.join(worktreePath, "f.txt"), "post-freeze-edit\n");
  fs.writeFileSync(path.join(worktreePath, "post-freeze-new.txt"), "should never appear\n");
  fs.rmSync(path.join(worktreePath, "new.txt"));

  const candidateDir = path.join(root, "candidate");
  materializeCandidate(repo, candidateSha, candidateDir);
  assert.equal(fs.readFileSync(path.join(candidateDir, "f.txt"), "utf8"), "base\nworker-change\n");
  assert.equal(fs.readFileSync(path.join(candidateDir, "new.txt"), "utf8"), "worker-added\n");
  assert.equal(fs.existsSync(path.join(candidateDir, "post-freeze-new.txt")), false);

  const disposable = disposableCheckout(repo, candidateSha);
  try {
    assert.equal(fs.readFileSync(path.join(disposable.dir, "f.txt"), "utf8"), "base\nworker-change\n");
    assert.equal(fs.readFileSync(path.join(disposable.dir, "new.txt"), "utf8"), "worker-added\n");
    assert.equal(fs.existsSync(path.join(disposable.dir, "post-freeze-new.txt")), false);
  } finally {
    disposable.dispose();
  }

  removeWorktree(repo, worktreePath);
});

test("freezeCommit trailer round-trips through findCommitByTrailer", () => {
  const worktreePath = path.join(root, "worktree-trailer");
  const baseSha = git(["rev-parse", "HEAD"], repo);
  createWorktree(repo, worktreePath, baseSha, "work-trailer");
  fs.writeFileSync(path.join(worktreePath, "g.txt"), "x\n");
  const sha = freezeCommit(worktreePath, "act-trailer-42", "freeze commit");
  assert.equal(findCommitByTrailer(worktreePath, "act-trailer-42"), sha);
  assert.equal(findCommitByTrailer(worktreePath, "act-does-not-exist"), undefined);
  removeWorktree(repo, worktreePath);
});

function restoreMtime(file: string, mtime: Date): void {
  fs.utimesSync(file, mtime, mtime);
}

test("integrity-detect: a modified disposable checkout is caught, an ignored build artifact is not", () => {
  const sha = git(["rev-parse", "HEAD"], repo);
  const disposable = disposableCheckout(repo, sha);
  try {
    assert.equal(verifyIntegrity(repo, disposable.dir, sha), true);

    // A real content modification must be caught.
    fs.appendFileSync(path.join(disposable.dir, "f.txt"), "tampered\n");
    assert.equal(verifyIntegrity(repo, disposable.dir, sha), false);
  } finally {
    disposable.dispose();
  }

  // An `update-index --assume-unchanged` bit set in the checkout's own
  // (untrusted) index must not hide a real edit.
  const assumeUnchanged = disposableCheckout(repo, sha);
  try {
    git(["update-index", "--assume-unchanged", "f.txt"], assumeUnchanged.dir);
    fs.writeFileSync(path.join(assumeUnchanged.dir, "f.txt"), "TAMPERED\n");
    assert.equal(
      git(["status", "--porcelain=v1"], assumeUnchanged.dir),
      "",
      "assume-unchanged should indeed hide the edit from `git status` in the checkout itself",
    );
    assert.equal(verifyIntegrity(repo, assumeUnchanged.dir, sha), false);
  } finally {
    assumeUnchanged.dispose();
  }

  // A same-size edit with the original mtime restored (racy-clean) must
  // still be caught — the seeded index carries no filesystem stat to
  // short-circuit against.
  const racyClean = disposableCheckout(repo, sha);
  try {
    const f = path.join(racyClean.dir, "f.txt");
    const original = fs.readFileSync(f, "utf8");
    const originalStat = fs.statSync(f);
    const replacement = "X".repeat(original.length);
    assert.equal(replacement.length, original.length, "test setup: same-size replacement");
    fs.writeFileSync(f, replacement);
    restoreMtime(f, originalStat.mtime);
    assert.equal(verifyIntegrity(repo, racyClean.dir, sha), false);
  } finally {
    racyClean.dispose();
  }

  // Editing the checkout's own `.git` to spoof HEAD elsewhere must not
  // make a genuinely tampered tree pass (nor, symmetrically, make an
  // untampered one fail — verified by the "clean" assertion above already
  // not depending on HEAD at all).
  const headSpoof = disposableCheckout(repo, sha);
  try {
    fs.appendFileSync(path.join(headSpoof.dir, "f.txt"), "tampered\n");
    fs.writeFileSync(path.join(headSpoof.dir, ".git", "HEAD"), "ref: refs/heads/does-not-exist\n");
    assert.equal(verifyIntegrity(repo, headSpoof.dir, sha), false);
  } finally {
    headSpoof.dispose();
  }

  // Commit a .gitignore into the repo so a fresh checkout of *that* sha
  // starts clean, then verify a gitignored build artifact does not flip
  // verifyIntegrity to false, and that a *new* .gitignore rule covering an
  // already-tracked, modified file does not hide that modification.
  fs.writeFileSync(path.join(repo, ".gitignore"), "dist/\n");
  git(["add", "-A"], repo);
  git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "add gitignore"], repo);
  const shaWithIgnore = git(["rev-parse", "HEAD"], repo);

  const clean = disposableCheckout(repo, shaWithIgnore);
  try {
    assert.equal(verifyIntegrity(repo, clean.dir, shaWithIgnore), true);
    fs.mkdirSync(path.join(clean.dir, "dist"));
    fs.writeFileSync(path.join(clean.dir, "dist", "out.txt"), "built\n");
    assert.equal(
      verifyIntegrity(repo, clean.dir, shaWithIgnore),
      true,
      "an ignored build artifact must not flip integrity",
    );
  } finally {
    clean.dispose();
  }

  const ignoreHidesModification = disposableCheckout(repo, shaWithIgnore);
  try {
    fs.appendFileSync(path.join(ignoreHidesModification.dir, "f.txt"), "tampered\n");
    fs.appendFileSync(path.join(ignoreHidesModification.dir, ".gitignore"), "f.txt\n");
    assert.equal(
      verifyIntegrity(repo, ignoreHidesModification.dir, shaWithIgnore),
      false,
      "a new .gitignore rule must not hide a modification to an already-tracked file",
    );
  } finally {
    ignoreHidesModification.dispose();
  }

  // Same read-only-candidate case as before, after forcing it writable.
  const candidateDir = path.join(root, "candidate-integrity");
  materializeCandidate(repo, shaWithIgnore, candidateDir);
  assert.equal(verifyIntegrity(repo, candidateDir, shaWithIgnore), true);
  assert.throws(() => fs.appendFileSync(path.join(candidateDir, "f.txt"), "should fail"));
  setTreeWritable(candidateDir, true);
  fs.appendFileSync(path.join(candidateDir, "f.txt"), "tampered\n");
  assert.equal(verifyIntegrity(repo, candidateDir, shaWithIgnore), false);
});

test("checked-equals-candidate: treeHashOf a disposable checkout equals sha^{tree}", () => {
  const worktreePath = path.join(root, "worktree-tree");
  const baseSha = git(["rev-parse", "HEAD"], repo);
  createWorktree(repo, worktreePath, baseSha, "work-tree");
  fs.mkdirSync(path.join(worktreePath, "sub"));
  fs.writeFileSync(path.join(worktreePath, "sub", "nested.txt"), "nested\n");
  const sha = freezeCommit(worktreePath, "act-tree-1", "freeze for tree hash");
  removeWorktree(repo, worktreePath);

  const disposable = disposableCheckout(repo, sha);
  try {
    const actual = git(["rev-parse", `${sha}^{tree}`], repo);
    assert.equal(treeHashOf(repo, disposable.dir, sha), actual);
  } finally {
    disposable.dispose();
  }
});

test("probe: fast forward gives I === C", () => {
  const H = git(["rev-parse", "HEAD"], repo);
  const worktreePath = path.join(root, "worktree-ff");
  createWorktree(repo, worktreePath, H, "work-ff");
  fs.writeFileSync(path.join(worktreePath, "ff.txt"), "ff\n");
  const C = freezeCommit(worktreePath, "act-ff-1", "candidate for ff probe");
  removeWorktree(repo, worktreePath);

  const result = probe(repo, { runId: "run-ff", candidateSha: C, headSha: H });
  assert.equal(result.ok, true);
  if (!result.ok) return;
  assert.equal(result.I, C);
  assert.equal(result.fastForward, true);
  assert.equal(fs.readFileSync(path.join(result.checkoutDir, "ff.txt"), "utf8"), "ff\n");
  discardProbe(repo, result);
  assert.equal(fs.existsSync(result.checkoutDir), false);
  assert.throws(() => git(["rev-parse", "--verify", result.probeBranch], repo));
});

test("probe: a genuine conflict leaves the integration branch untouched and discards the probe branch", () => {
  const H0 = git(["rev-parse", "HEAD"], repo);
  const worktreePath = path.join(root, "worktree-conflict");
  createWorktree(repo, worktreePath, H0, "work-conflict");
  fs.writeFileSync(path.join(worktreePath, "f.txt"), "candidate-side\n");
  const C = freezeCommit(worktreePath, "act-conflict-1", "candidate side change");
  removeWorktree(repo, worktreePath);

  // Advance main independently so the merge genuinely conflicts.
  fs.writeFileSync(path.join(repo, "f.txt"), "head-side\n");
  git(["add", "-A"], repo);
  git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "head side change"], repo);
  const H = git(["rev-parse", "HEAD"], repo);

  const result = probe(repo, { runId: "run-conflict", candidateSha: C, headSha: H });
  assert.equal(result.ok, false);
  if (result.ok) return;
  assert.equal(result.conflict, true);
  assert.match(result.output, /conflict/i);
  assert.equal(git(["rev-parse", "main"], repo), H, "the integration branch must never move for a probe");

  const branches = git(["branch", "--list", `tt/run-conflict/probe/*`], repo);
  assert.equal(branches, "", "the probe branch must be discarded on conflict");
  const worktrees = git(["worktree", "list", "--porcelain"], repo);
  assert.ok(!worktrees.includes("tt-probe-"), "the probe checkout must be discarded on conflict");
});

test("publishCAS: success moves the branch, stale head reports the mismatch and leaves it untouched", () => {
  const H = git(["rev-parse", "HEAD"], repo);
  const worktreePath = path.join(root, "worktree-publish");
  createWorktree(repo, worktreePath, H, "work-publish");
  fs.writeFileSync(path.join(worktreePath, "p.txt"), "p\n");
  const C = freezeCommit(worktreePath, "act-publish-1", "candidate for publish");
  removeWorktree(repo, worktreePath);

  const okResult = publishCAS(repo, "main", C, H);
  assert.deepEqual(okResult, { ok: true });
  assert.equal(git(["rev-parse", "main"], repo), C);

  // A second publish with the same (now stale) H must fail and report the
  // real current head, leaving the branch exactly where it is.
  const staleResult = publishCAS(repo, "main", C, H);
  assert.equal(staleResult.ok, false);
  if (staleResult.ok) return;
  assert.equal(staleResult.actualHead, C);
  assert.equal(git(["rev-parse", "main"], repo), C);
});
