// Unit tests for extension/guards.ts (design §9.5). Run outside Pi's
// extension loader — see that module's own comment for why it has no
// typebox/Pi imports. Per the phase-1b brief: "use real Pi for guard tests
// only if you can do so without a model call; otherwise unit-test the guard
// function directly and say so" — this is that direct unit test.

import { test } from "node:test";
import assert from "node:assert/strict";

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

test("guardedWritePath blocks a write under the run directory even if it is inside the worktree", () => {
  const reason = guardedWritePath("run/events.jsonl", "/run/worktree", {
    worktree: "/run/worktree",
    runDir: "/run/worktree/run",
  });
  assert.match(reason ?? "", /run directory/);
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
