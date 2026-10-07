// 06a finding #24: `tt models check` and the pure classification it rests on.
//
// The pure half (`core/models-check.ts`) resolves the distinct configured
// models and classifies a probe's answer; the CLI half spawns Pi print mode.
// These tests drive a fake Pi (the same `test/fake-pi/fake-pi.ts` the
// conductor tests use) that answers `ok`, returns a 403 body or never answers,
// with the probe's 60 s bound shortened under `TT_TEST_MODE=1`.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import {
  classifyModelProbe,
  distinctModelGroups,
  modelsCheckStatusText,
  parseModelsCheck,
  planModelTargets,
} from "../../src/core/models-check.ts";
import type { PlanModels } from "../../src/core/roles.ts";
import type { RunPlanFile } from "../../src/conductor.ts";
import type { ProgramFile } from "../../src/core/program.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));
const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));

// ---------------------------------------------------------------------------
// Pure: distinct configured models and classification
// ---------------------------------------------------------------------------

test("models-check: distinct groups dedupe a model shared by role and seat", () => {
  const models: PlanModels = {
    worker: { model: "deepseek/deepseek-v4.1-flash-fast" },
    reviewerSeats: {
      M: { provider: "vercel-ai-gateway", model: "anthropic/claude-opus-5.5" },
      A: { provider: "vercel-ai-gateway", model: "openai/gpt-6.1-sol" },
      B: { provider: "vercel-ai-gateway", model: "spacexai/grok-4.6" },
    },
    evaluator: { provider: "vercel-ai-gateway", model: "anthropic/claude-opus-5.5" },
    panelFrom: "reviewers",
  };
  const groups = distinctModelGroups(planModelTargets(models));
  assert.deepEqual(
    groups.map((g) => g.key),
    [
      "deepseek/deepseek-v4.1-flash-fast",
      "vercel-ai-gateway:anthropic/claude-opus-5.5",
      "vercel-ai-gateway:openai/gpt-6.1-sol",
      "vercel-ai-gateway:spacexai/grok-4.6",
    ],
  );
  // M, the evaluator and panel seat 1 all run the same opus: one probe.
  assert.deepEqual(groups[1].roles, ["reviewer.M", "evaluator", "panel.1"]);
});

test("models-check: classify is ok on exit 0, refused on a 403 body, unreachable otherwise", () => {
  assert.deepEqual(classifyModelProbe({ exitCode: 0, timedOut: false, output: "OK\n" }), { status: "ok" });
  const refused = classifyModelProbe({ exitCode: 1, timedOut: false, output: "403 restricted\n" });
  assert.equal(refused.status, "refused");
  assert.equal(refused.message, "403 restricted");
  // A timeout is unreachable even when its truncated output mentions 403.
  assert.deepEqual(classifyModelProbe({ exitCode: null, timedOut: true, output: "403 restricted" }), { status: "unreachable" });
  assert.deepEqual(classifyModelProbe({ exitCode: 1, timedOut: false, output: "connect ETIMEDOUT\n" }), { status: "unreachable" });
});

test("models-check: the status note names a refusal and is absent when nothing was probed", () => {
  assert.equal(modelsCheckStatusText({ at: "", probes: [] }), undefined);
  assert.equal(modelsCheckStatusText({ at: "", probes: [{ key: "m", model: "m", roles: ["worker"], status: "ok" }] }), "check ok");
  assert.equal(
    modelsCheckStatusText({ at: "", probes: [{ key: "g", model: "grok-4.7", roles: ["reviewer.B"], status: "refused", message: "403 restricted" }] }),
    "check grok-4.7 refused (403 restricted)",
  );
});

test("models-check: parseModelsCheck rejects a malformed record", () => {
  assert.equal(parseModelsCheck(null), undefined);
  assert.equal(parseModelsCheck({ at: "x" }), undefined);
  assert.equal(parseModelsCheck({ at: "x", probes: [{ key: "k", model: "m", status: "bogus" }] }), undefined);
  const ok = parseModelsCheck({ at: "x", probes: [{ key: "k", model: "m", roles: ["worker"], status: "refused", message: "403" }] });
  assert.ok(ok);
  assert.equal(ok.probes[0].message, "403");
});

// ---------------------------------------------------------------------------
// CLI: `tt models check <plan.json>` against a fake Pi
// ---------------------------------------------------------------------------

function writePlan(dir: string, models: PlanModels): string {
  const plan = { title: "models", repo: "/tmp/repo", integrationBranch: "main", checks: ["true"], models, phases: [] } as unknown as RunPlanFile;
  const file = path.join(dir, "plan.json");
  fs.writeFileSync(file, JSON.stringify(plan));
  return file;
}

function writePrintScript(dir: string, print: { mode: "ok" | "refused" | "hang"; text?: string }): string {
  const file = path.join(dir, "script.json");
  fs.writeFileSync(file, JSON.stringify({ steps: [], print }));
  return file;
}

function runModelsCheckCli(args: string[], script: string, timeoutMs: number): { status: number | null; stdout: string; stderr: string } {
  const r = spawnSync(process.execPath, [CLI, ...args], {
    encoding: "utf8",
    env: {
      ...process.env,
      TT_TEST_MODE: "1",
      TT_TEST_PI_COMMAND: process.execPath,
      TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
      FAKE_PI_SCRIPT: script,
      TT_MODELS_CHECK_TIMEOUT_MS: String(timeoutMs),
    },
  });
  return { status: r.status, stdout: r.stdout ?? "", stderr: r.stderr ?? "" };
}

test("tt models check: a fake Pi that answers reports ok", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-models-"));
  try {
    const plan = writePlan(dir, { worker: { provider: "p", model: "m" } });
    const script = writePrintScript(dir, { mode: "ok" });
    const r = runModelsCheckCli(["models", "check", plan], script, 5_000);
    assert.equal(r.status, 0, r.stderr);
    assert.match(r.stdout, /^p:m ok$/m);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("tt models check: a fake Pi that returns a 403 body reports refused with the message", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-models-"));
  try {
    const plan = writePlan(dir, { worker: { provider: "p", model: "grok-4.7" } });
    const script = writePrintScript(dir, { mode: "refused", text: "403 restricted" });
    const r = runModelsCheckCli(["models", "check", plan], script, 5_000);
    assert.equal(r.status, 1, "a refusal exits non-zero");
    assert.match(r.stdout, /^p:grok-4\.7 refused \(403 restricted\)$/m);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("tt models check: a fake Pi that never answers reports unreachable", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-models-"));
  try {
    const plan = writePlan(dir, { worker: { provider: "p", model: "m" } });
    const script = writePrintScript(dir, { mode: "hang" });
    const r = runModelsCheckCli(["models", "check", plan], script, 700);
    assert.equal(r.status, 1, "unreachable also exits non-zero");
    assert.match(r.stdout, /^p:m unreachable$/m);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("tt models check: a plan with no #+TT_MODELS probes nothing", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-models-"));
  try {
    const plan = writePlan(dir, {});
    const script = writePrintScript(dir, { mode: "ok" });
    const r = runModelsCheckCli(["models", "check", plan], script, 5_000);
    assert.equal(r.status, 0, r.stderr);
    assert.match(r.stdout, /nothing to check/);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("tt models check: a program probes every entry's distinct models", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-models-"));
  try {
    const program: ProgramFile = {
      title: "prog",
      maxParallel: 1,
      entries: [
        { id: "a", after: [], plan: { title: "a", repo: "/tmp/r", integrationBranch: "main", checks: ["true"], models: { worker: { provider: "p", model: "m" } }, phases: [] } as unknown as RunPlanFile },
        { id: "b", after: ["a"], plan: { title: "b", repo: "/tmp/r", integrationBranch: "main", checks: ["true"], models: { worker: { provider: "p2", model: "m2" } }, phases: [] } as unknown as RunPlanFile },
      ],
    };
    const file = path.join(dir, "program.json");
    fs.writeFileSync(file, JSON.stringify(program));
    const script = writePrintScript(dir, { mode: "ok" });
    const r = runModelsCheckCli(["models", "check", file], script, 5_000);
    assert.equal(r.status, 0, r.stderr);
    assert.match(r.stdout, /^p:m ok$/m);
    assert.match(r.stdout, /^p2:m2 ok$/m);
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});
