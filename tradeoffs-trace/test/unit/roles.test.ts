import assert from "node:assert/strict";
import { test } from "node:test";

import { assertToolSet, launchArgs, PI_VERSION, ROLE_TOOLS } from "../../src/core/roles.ts";

test("roles: PI_VERSION is the pinned version", () => {
  assert.equal(PI_VERSION, "0.87.0");
});

test("roles: ROLE_TOOLS matches design §2.1's launch table exactly", () => {
  assert.deepEqual(ROLE_TOOLS.worker, ["read", "edit", "write", "grep", "find", "ls", "sh", "submit_phase"]);
  assert.deepEqual(ROLE_TOOLS.reviewer, ["read", "grep", "find", "ls", "submit_discovery", "submit_review"]);
});

test("roles: launchArgs never uses --exclude-tools, always an explicit --tools allowlist", () => {
  for (const role of Object.keys(ROLE_TOOLS) as Array<keyof typeof ROLE_TOOLS>) {
    const args = launchArgs(role, { extensionPath: "/tmp/ext.ts" });
    assert.ok(!args.includes("--exclude-tools"), `${role} launch must never use --exclude-tools`);
    const toolsIndex = args.indexOf("--tools");
    assert.ok(toolsIndex >= 0, `${role} launch must pass --tools`);
    assert.equal(args[toolsIndex + 1], ROLE_TOOLS[role].join(","));
    assert.ok(args.includes("--mode"));
    assert.equal(args[args.indexOf("--mode") + 1], "rpc");
    assert.ok(args.includes("--extension"));
    assert.equal(args[args.indexOf("--extension") + 1], "/tmp/ext.ts");
    assert.ok(args.includes("--no-extensions"), "must isolate from the owner's own user extensions");
    assert.ok(args.includes("--no-skills"), "must isolate from the owner's own user skills");
  }
});

test("roles: launchArgs defaults to --no-session (no prompt is ever dispatched by this packet)", () => {
  const args = launchArgs("worker", { extensionPath: "/tmp/ext.ts" });
  assert.ok(args.includes("--no-session"));
  assert.ok(!args.includes("--session-dir"));
});

test("roles: launchArgs uses --session-dir when a session dir is given and noSession is not forced", () => {
  const args = launchArgs("worker", { extensionPath: "/tmp/ext.ts", sessionDir: "/tmp/run1" });
  assert.ok(!args.includes("--no-session"));
  const idx = args.indexOf("--session-dir");
  assert.ok(idx >= 0);
  assert.equal(args[idx + 1], "/tmp/run1");
});

test("roles: launchArgs includes provider/model only when given", () => {
  const bare = launchArgs("reviewer", { extensionPath: "/tmp/ext.ts" });
  assert.ok(!bare.includes("--provider"));
  assert.ok(!bare.includes("--model"));

  const withModel = launchArgs("reviewer", {
    extensionPath: "/tmp/ext.ts",
    provider: "vercel-ai-gateway",
    model: "deepseek/deepseek-v4.1-flash",
  });
  assert.equal(withModel[withModel.indexOf("--provider") + 1], "vercel-ai-gateway");
  assert.equal(withModel[withModel.indexOf("--model") + 1], "deepseek/deepseek-v4.1-flash");
});

test("assertToolSet: exact match is ok", () => {
  assert.deepEqual(assertToolSet("worker", [...ROLE_TOOLS.worker]), { ok: true });
});

test("assertToolSet: order does not matter", () => {
  assert.deepEqual(assertToolSet("worker", [...ROLE_TOOLS.worker].reverse()), { ok: true });
});

test("assertToolSet: missing tools are reported", () => {
  const result = assertToolSet("reviewer", ["read", "grep"]);
  assert.equal(result.ok, false);
  if (!result.ok) {
    assert.deepEqual(result.missing.sort(), ["find", "ls", "submit_discovery", "submit_review"].sort());
    assert.deepEqual(result.extra, []);
    assert.deepEqual(result.duplicates, []);
  }
});

test("assertToolSet: extra tools are reported (the --exclude-tools pitfall)", () => {
  const result = assertToolSet("worker", ["read", "edit", "write", "sh", "submit_phase", "submit_discovery", "submit_review"]);
  assert.equal(result.ok, false);
  if (!result.ok) {
    assert.deepEqual(result.missing.sort(), ["find", "grep", "ls"].sort());
    assert.deepEqual(result.extra.sort(), ["submit_discovery", "submit_review"].sort());
  }
});

test("assertToolSet: a duplicated tool name is a mismatch even when the set is otherwise exact", () => {
  const result = assertToolSet("worker", [...ROLE_TOOLS.worker, "read"]);
  assert.equal(result.ok, false);
  if (!result.ok) {
    assert.deepEqual(result.duplicates, ["read"]);
    assert.deepEqual(result.missing, []);
    assert.deepEqual(result.extra, []);
  }
});
