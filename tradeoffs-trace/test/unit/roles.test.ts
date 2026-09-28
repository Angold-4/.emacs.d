import assert from "node:assert/strict";
import { test } from "node:test";

import { assertToolSet, launchArgs, PI_VERSION, planModelSelector, ROLE_TOOLS } from "../../src/core/roles.ts";

test("roles: PI_VERSION is the pinned version", () => {
  assert.equal(PI_VERSION, "0.87.0");
});

test("roles: ROLE_TOOLS matches design §2.1's launch table exactly", () => {
  assert.deepEqual(ROLE_TOOLS.worker, ["read", "edit", "write", "grep", "find", "ls", "sh", "submit_phase", "raise_tradeoff"]);
  assert.deepEqual(ROLE_TOOLS.reviewer, ["read", "grep", "find", "ls", "submit_discovery", "submit_review"]);
  // Plan 04a: the evaluator reads the candidate and returns through
  // submit_evaluation (and may raise a trade-off it spots); no write tools.
  assert.deepEqual(ROLE_TOOLS.evaluator, ["read", "grep", "find", "ls", "submit_evaluation"]);
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
    assert.deepEqual(result.missing.sort(), ["find", "grep", "ls", "raise_tradeoff"].sort());
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

// #+TT_MODELS per seat (design §2.1): one selector answers for a role and,
// for the reviewer and panel roles, a seat — the same resolution the launch
// sites and the views use.

test("planModelSelector: a plan with only the four flat roles is unchanged", () => {
  const select = planModelSelector({ models: { worker: { model: "w" }, reviewer: { model: "r" }, evaluator: { model: "e" }, panel: { model: "p" } } });
  assert.deepEqual(select("worker"), { model: "w" });
  assert.deepEqual(select("reviewer"), { model: "r" });
  assert.deepEqual(select("reviewer", "M"), { model: "r" }, "every seat uses reviewer's model");
  assert.deepEqual(select("evaluator"), { model: "e" });
  assert.deepEqual(select("panel"), { model: "p" });
  assert.deepEqual(select("panel", 2), { model: "p" }, "every panel seat uses panel's model");
});

test("planModelSelector: reviewer.M/A/B win over reviewer, seat by seat", () => {
  const select = planModelSelector({
    models: {
      reviewer: { model: "fallback" },
      reviewerSeats: { M: { provider: "vercel-ai-gateway", model: "anthropic/claude-opus-5.5" }, B: { model: "spacexai/grok-4.6" } },
    },
  });
  assert.deepEqual(select("reviewer", "M"), { provider: "vercel-ai-gateway", model: "anthropic/claude-opus-5.5" });
  assert.deepEqual(select("reviewer", "A"), { model: "fallback" });
  assert.deepEqual(select("reviewer", "B"), { model: "spacexai/grok-4.6" });
});

test("planModelSelector: panel.N wins; panelFrom=reviewers follows the reviewer of the seat's position", () => {
  const models = {
    reviewerSeats: { M: { model: "m" }, A: { model: "a" }, B: { model: "b" } },
    panelFrom: "reviewers" as const,
  };
  const select = planModelSelector({ models });
  assert.deepEqual(select("panel", 1), { model: "m" });
  assert.deepEqual(select("panel", 2), { model: "a" });
  assert.deepEqual(select("panel", 3), { model: "b" });
  // An explicit panel seat beats panelFrom for that seat only.
  const mixed = planModelSelector({ models: { ...models, panelSeats: { "2": { model: "own" } } } });
  assert.deepEqual(mixed("panel", 1), { model: "m" });
  assert.deepEqual(mixed("panel", 2), { model: "own" });
  assert.deepEqual(mixed("panel", 3), { model: "b" });
});

test("planModelSelector: panel.N wins over a flat panel model; a plan without models selects nothing", () => {
  const select = planModelSelector({ models: { panel: { model: "flat" }, panelSeats: { "1": { model: "one" } } } });
  assert.deepEqual(select("panel", 1), { model: "one" });
  assert.deepEqual(select("panel", 2), { model: "flat" });
  const none = planModelSelector({});
  for (const role of ["worker", "reviewer", "evaluator", "panel", "curator"] as const) assert.equal(none(role), undefined);
  assert.equal(none("reviewer", "M"), undefined);
  // Plan 05j: the curator agent is opt-in — a plan must name `curator`; it is
  // never inferred from the evaluator's model, so a plan that names only the
  // four roles launches exactly those four.
  const four = planModelSelector({ models: { worker: { model: "w" }, reviewer: { model: "r" }, evaluator: { model: "e" }, panel: { model: "p" } } });
  assert.equal(four("curator"), undefined);
  const withCurator = planModelSelector({ models: { curator: { provider: "p-c", model: "m-c" } } });
  assert.deepEqual(withCurator("curator"), { provider: "p-c", model: "m-c" });
  assert.equal(none("panel", 3), undefined);
});
