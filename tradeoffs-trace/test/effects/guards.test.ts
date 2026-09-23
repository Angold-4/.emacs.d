// Unit tests for extension/guards.ts (design §9.5). Run outside Pi's
// extension loader — see that module's own comment for why it has no
// typebox/Pi imports. Per the phase-1b brief: "use real Pi for guard tests
// only if you can do so without a model call; otherwise unit-test the guard
// function directly and say so" — this is that direct unit test.

import { test } from "node:test";
import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import { guardedShCommand, guardedWritePath } from "../../extension/guards.ts";

test("guardedWritePath allows a write inside the worktree", () => {
  const reason = guardedWritePath("src/a.ts", "/run/worktree", { worktree: "/run/worktree" });
  assert.equal(reason, undefined);
});

test("guardedWritePath blocks a write outside the worktree", () => {
  const reason = guardedWritePath("/etc/passwd", "/run/worktree", { worktree: "/run/worktree" });
  assert.match(reason ?? "", /outside the worktree/);
});

test("guardedWritePath blocks a relative escape (../) outside the worktree", () => {
  const reason = guardedWritePath("../../etc/passwd", "/run/worktree/sub", { worktree: "/run/worktree" });
  assert.match(reason ?? "", /outside the worktree/);
});

test("guardedWritePath blocks run metadata beside the worktree (real nested layout)", () => {
  // The real layout is `worktree = <runDir>/worktree`, i.e. the worktree
  // is *inside* the run directory. Run metadata is everything under
  // runDir that is not under the worktree. (The previous version of this
  // test had runDir nested inside the worktree — the inverse of the real
  // layout — which is exactly the configuration that made the guard
  // reject every ordinary worktree edit; see F05.)
  const runDir = "/run/tt-run";
  const worktree = path.join(runDir, "worktree");
  for (const target of [
    path.join(runDir, "events.jsonl"),
    path.join(runDir, "checks", "abc123", "x.log"),
    path.join(runDir, "conductor.lock"),
  ]) {
    const reason = guardedWritePath(target, worktree, { worktree, runDir });
    assert.match(reason ?? "", /run directory/);
  }
  // ...while an ordinary file inside the worktree stays allowed.
  assert.equal(guardedWritePath("src/a.ts", worktree, { worktree, runDir }), undefined);
});

test("guardedWritePath allows a write in the worktree when the worktree lives inside the run directory (the real layout)", () => {
  const config = { worktree: "/r/run1/worktree", runDir: "/r/run1" };
  assert.equal(guardedWritePath("tradeoffs-trace/src/core/types.ts", "/r/run1/worktree", config), undefined);
  assert.equal(guardedWritePath("/r/run1/worktree/a.ts", "/r/run1/worktree", config), undefined);
  // the rest of the run directory stays protected
  assert.match(guardedWritePath("/r/run1/events.jsonl", "/r/run1/worktree", config) ?? "", /outside the worktree|run directory/);
  assert.match(guardedWritePath("../inbox/x.json", "/r/run1/worktree", config) ?? "", /outside the worktree|run directory/);
});

test("guardedWritePath blocks a write to a protected acceptance file", () => {
  const reason = guardedWritePath("ACCEPTANCE.md", "/run/worktree", {
    worktree: "/run/worktree",
    protectedPaths: ["ACCEPTANCE.md"],
  });
  assert.match(reason ?? "", /protected acceptance file/);
});

test("guardedWritePath allows other files when a protected file is configured", () => {
  const reason = guardedWritePath("src/other.ts", "/run/worktree", {
    worktree: "/run/worktree",
    protectedPaths: ["ACCEPTANCE.md"],
  });
  assert.equal(reason, undefined);
});

test("guardedShCommand blocks git commit", () => {
  const reason = guardedShCommand("git commit -m 'sneaky'", {});
  assert.match(reason ?? "", /blocked/);
});

test("guardedShCommand blocks git push", () => {
  const reason = guardedShCommand("git push origin main", {});
  assert.match(reason ?? "", /blocked/);
});

test("guardedShCommand allows an ordinary command", () => {
  const reason = guardedShCommand("npm test", {});
  assert.equal(reason, undefined);
});

test("guardedShCommand blocks a command mentioning the run directory", () => {
  const reason = guardedShCommand("cat /run/tt-run/events.jsonl", { runDir: "/run/tt-run" });
  assert.match(reason ?? "", /run directory/);
});

// Plan 2c: backgrounding and long sleeps (observed: 40–60% of each worker
// attempt in dogfood run 4ec5e0f8 went to nohup'd suite runs and polling).
test("guardedShCommand blocks backgrounded work and long sleeps, but not && or redirections", () => {
  const cfg = { worktree: "/r/run1/worktree", runDir: "/r/run1" };
  for (const blocked of [
    "node --test 'test/**/*.test.ts' > /tmp/all.log 2>&1 &",
    "nohup make check > /tmp/x.log",
    "setsid node server.js",
    "make check & echo started",
    "sleep 150; tail /tmp/all.log",
    "sleep 2m",
  ]) {
    assert.ok(guardedShCommand(blocked, cfg), `expected blocked: ${blocked}`);
  }
  for (const allowed of [
    "npm test && npm run lint",
    "node --test test/unit/x.test.ts 2>&1 | tail -20",
    "make check &> /tmp/out.log",
    "echo 'a & b' && grep -n '&' src/x.ts",
    "sleep 2 && cat /tmp/x",
    "sleep 30",
  ]) {
    assert.equal(guardedShCommand(allowed, cfg), undefined, `expected allowed: ${allowed}`);
  }
});

test("guardedShCommand allows commands that mention the worktree inside the run directory", () => {
  const cfg = { worktree: "/r/run1/worktree", runDir: "/r/run1" };
  assert.equal(guardedShCommand("cat /r/run1/worktree/src/a.ts", cfg), undefined);
  assert.match(guardedShCommand("cat /r/run1/events.jsonl", cfg) ?? "", /run directory/);
});

// --- R3 gates: real nested layout + canonicalization -----------------------

/** Builds `<base>/tt-run-XXXX/worktree` and returns the real run/worktree
 * paths plus a cleanup function. */
function makeRun(base: string): { runDir: string; worktree: string; cleanup: () => void } {
  const runDir = fs.mkdtempSync(path.join(base, "tt-run-"));
  const worktree = path.join(runDir, "worktree");
  fs.mkdirSync(worktree);
  return {
    runDir,
    worktree,
    cleanup: () => fs.rmSync(runDir, { recursive: true, force: true }),
  };
}

test("R3.normal-edit: the real nested layout allows ordinary worktree edits", () => {
  const { runDir, worktree, cleanup } = makeRun(os.tmpdir());
  try {
    const config = { worktree, runDir };
    fs.writeFileSync(path.join(worktree, "existing.txt"), "old\n");

    // existing file, relative to cwd = worktree
    assert.equal(guardedWritePath("existing.txt", worktree, config), undefined);

    // new file in a new nested subdirectory (its parent does not exist)
    assert.equal(guardedWritePath("sub/dir/new.txt", worktree, config), undefined);

    // absolute path into the worktree
    assert.equal(guardedWritePath(path.join(worktree, "sub", "abs.txt"), worktree, config), undefined);

    // relative path from a cwd *inside* the worktree
    fs.mkdirSync(path.join(worktree, "src"));
    assert.equal(guardedWritePath("b.ts", path.join(worktree, "src"), config), undefined);
    assert.equal(guardedWritePath("../top.ts", path.join(worktree, "src"), config), undefined);
  } finally {
    cleanup();
  }
});

test("R3.protection: run metadata, escapes, protected files and symlink escapes are denied; canonical aliases are allowed", () => {
  const { runDir, worktree, cleanup } = makeRun(os.tmpdir());
  const outsideDir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-guard-outside-"));
  try {
    const config = { worktree, runDir, protectedPaths: ["ACCEPTANCE.md"] };
    fs.writeFileSync(path.join(worktree, "ACCEPTANCE.md"), "acceptance\n");

    // run metadata: a log file, a nested per-sha check log, and the lock
    assert.match(guardedWritePath(path.join(runDir, "events.jsonl"), worktree, config) ?? "", /run directory/);
    assert.match(guardedWritePath(path.join(runDir, "checks", "abc", "x.log"), worktree, config) ?? "", /run directory/);
    assert.match(guardedWritePath(path.join(runDir, "conductor.lock"), worktree, config) ?? "", /run directory/);
    // a *nonexistent* metadata path still resolves through its existing ancestor
    assert.match(guardedWritePath(path.join(runDir, "checks", "abc", "new.log"), worktree, config) ?? "", /run directory/);

    // outside the worktree
    assert.match(guardedWritePath("/etc/passwd", worktree, config) ?? "", /outside the worktree/);
    assert.match(guardedWritePath(path.join(outsideDir, "x.txt"), worktree, config) ?? "", /outside the worktree/);

    // a `..` escape: one that lands in the run directory is run metadata,
    // one that lands outside it is outside the worktree — both denied.
    assert.match(guardedWritePath("../outside.txt", worktree, config) ?? "", /run directory/);
    assert.match(guardedWritePath("../../escape.txt", worktree, config) ?? "", /outside the worktree/);

    // a configured protected acceptance file
    assert.match(guardedWritePath("ACCEPTANCE.md", worktree, config) ?? "", /protected acceptance file/);

    // a symlink *inside* the worktree that points at an external target:
    // the canonical target escapes the worktree, so it must be denied.
    const external = path.join(outsideDir, "external.txt");
    fs.writeFileSync(external, "external\n");
    fs.symlinkSync(external, path.join(worktree, "link-to-external"));
    assert.match(guardedWritePath("link-to-external", worktree, config) ?? "", /outside the worktree/);

    // canonical alias of an allowed path: a symlinked directory inside the
    // worktree that resolves inside the worktree is allowed.
    fs.mkdirSync(path.join(worktree, "real"));
    fs.symlinkSync(path.join(worktree, "real"), path.join(worktree, "alias"));
    assert.equal(guardedWritePath("alias/f.ts", worktree, config), undefined);

    // canonical alias of the whole worktree: on macOS /tmp is a symlink to
    // /private/tmp, so the same directory via either spelling must agree.
    if (fs.existsSync("/tmp") && fs.existsSync("/private/tmp")) {
      const run2 = makeRun("/private/tmp");
      try {
        const aliasRun = run2.runDir.replace(/^\/private/, "");
        assert.notEqual(aliasRun, run2.runDir, "test setup: /private/tmp alias");
        const viaAlias = path.join(aliasRun, "worktree", "new.txt");
        assert.equal(guardedWritePath(viaAlias, run2.worktree, { worktree: run2.worktree, runDir: run2.runDir }), undefined);
        // and a metadata file reached through the alias is still denied
        assert.match(
          guardedWritePath(path.join(aliasRun, "events.jsonl"), run2.worktree, { worktree: run2.worktree, runDir: run2.runDir }) ?? "",
          /run directory/,
        );
      } finally {
        run2.cleanup();
      }
    }
  } finally {
    fs.rmSync(outsideDir, { recursive: true, force: true });
    cleanup();
  }
});
