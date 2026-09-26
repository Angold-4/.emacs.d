// Plan 01b: `tt program status` (programStatusLines) puts waiting (needs-you)
// nodes first, each with `waiting <duration>` and the one-line reason, so the
// program buffer and the CLI surface the owner wait at the top.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import type { RunPlanFile } from "../../src/conductor.ts";
import { programStatusLines, programWaitingNodes, programsRoot } from "../../src/program.ts";
import type { ProgramFile } from "../../src/core/program.ts";

function plan(title: string): RunPlanFile {
  return {
    title,
    repo: "/tmp/repo",
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
  } as unknown as RunPlanFile;
}

test("program status: waiting nodes come first, with waiting <duration> and the reason", () => {
  const root = fs.mkdtempSync("/tmp/tt-prog-status-");
  try {
    const dir = path.join(programsRoot(root), "p1");
    fs.mkdirSync(dir, { recursive: true });
    // 13g is listed first but waits on 13f; 13f is the one needing the owner.
    const program: ProgramFile = {
      title: "plan 13",
      maxParallel: 2,
      entries: [
        { id: "13g", after: ["13f"], plan: plan("13g") },
        { id: "13f", after: [], plan: plan("13f") },
      ],
    };
    fs.writeFileSync(path.join(dir, "program.json"), JSON.stringify(program));
    const events = [
      { ts: "2025-12-31T23:00:00.000Z", event: { type: "NODE_STARTED", node: "13f", runId: "r13f", branch: "main--13f", base: "main" } },
      {
        ts: "2026-01-01T00:00:00.000Z",
        event: { type: "NODE_STATUS", node: "13f", status: "needs-you", reason: "the repair budget ran out while items remained open" },
      },
    ];
    fs.writeFileSync(path.join(dir, "events.jsonl"), `${events.map((e) => JSON.stringify(e)).join("\n")}\n`);

    const now = new Date("2026-01-01T01:12:00.000Z");
    const lines = programStatusLines(dir, now);
    const text = lines.join("\n");
    assert.match(text, /⚑ 13f\s+waiting 1h12m/, "the waiting line names the node and its wait");
    assert.match(text, /the repair budget ran out while items remained open/, "the one-line reason is shown");
    assert.ok(
      lines.findIndex((l) => l.includes("13f")) < lines.findIndex((l) => l.includes("13g")),
      "the waiting node is listed before the node waiting on it",
    );

    const waiting = programWaitingNodes(dir, now);
    assert.deepEqual(waiting.map((w) => w.node), ["13f"]);
    assert.equal(waiting[0].duration, "1h12m");
    assert.equal(waiting[0].since, "2026-01-01T00:00:00.000Z");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
