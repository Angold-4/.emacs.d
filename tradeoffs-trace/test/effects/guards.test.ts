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
