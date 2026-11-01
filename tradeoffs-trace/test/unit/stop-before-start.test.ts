import assert from "node:assert/strict";
import { test } from "node:test";
import * as fs from "node:fs";
import { Conductor, createRun, type RunPlanFile } from "../../src/conductor.ts";

test("stop releases a conductor whose start has not initialized phase state", async () => {
  const root = fs.mkdtempSync("/tmp/tt-stop-unstarted-");
  const plan: RunPlanFile = { title: "parser", repo: root, integrationBranch: "main", checks: [], phases: [
    { id: "p1", goal: "parse", acceptance: [], checks: [], boundaries: [], reserved: [], workers: 2 },
  ] };
  try {
    const runDir = createRun(root, plan, "run");
    const conductor = new Conductor({ runDir, plan });
    await assert.doesNotReject(conductor.stop());
    await assert.doesNotReject(conductor.stop());
  } finally { fs.rmSync(root, { recursive: true, force: true }); }
});
