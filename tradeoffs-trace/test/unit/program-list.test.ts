// Multi-machine packet: `tt program list --json` is the one call per root the
// Emacs program picker makes. It returns one JSON object per program with the
// fields the picker labels a row with (id, title, state, node count, the Org
// source file and last activity), so a listing never reads a program's files
// one by one over TRAMP.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { programsRoot } from "../../src/program.ts";
import type { ProgramFile } from "../../src/core/program.ts";
import type { RunPlanFile } from "../../src/conductor.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function plan(title: string): RunPlanFile {
  return {
    title,
    repo: "/tmp/repo",
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
  } as unknown as RunPlanFile;
}

function makeProgram(root: string, id: string, title: string, source: string, events: string[]): string {
  const dir = path.join(programsRoot(root), id);
  fs.mkdirSync(dir, { recursive: true });
  const program: ProgramFile = {
    title,
    maxParallel: 1,
    entries: [
      { id: "a", after: [], plan: plan(`${title}a`) },
      { id: "b", after: ["a"], plan: plan(`${title}b`) },
    ],
  };
  fs.writeFileSync(path.join(dir, "program.json"), JSON.stringify(program));
  fs.writeFileSync(path.join(dir, "source.json"), JSON.stringify({ path: source }));
  fs.writeFileSync(path.join(dir, "events.jsonl"), events.length > 0 ? `${events.join("\n")}\n` : "");
  return dir;
}

function cli(root: string, ...args: string[]): string {
  return execFileSync(process.execPath, [CLI, ...args, "--root", root], { encoding: "utf8" });
}

test("tt program list --json prints id, title, state, node count, source and activity", () => {
  const root = fs.mkdtempSync("/tmp/tt-program-list-");
  try {
    makeProgram(root, "prog0001", "plan 14", "/home/me/orgw/05_program.org", [
      JSON.stringify({ ts: "2026-01-01T00:00:00.000Z", event: { type: "NODE_STARTED", node: "a", runId: "run-a" } }),
      JSON.stringify({ ts: "2026-01-01T01:00:00.000Z", event: { type: "NODE_BLOCKED", node: "a", reason: "broke" } }),
    ]);
    const rows = JSON.parse(cli(root, "program", "list", "--json")) as Array<Record<string, unknown>>;
    assert.equal(rows.length, 1);
    const row = rows[0];
    assert.equal(row.id, "prog0001");
    assert.equal(row.title, "plan 14");
    assert.equal(row.state, "stuck", "a blocked node's dependants can never start");
    assert.equal(row.nodeCount, 2);
    assert.equal(row.source, "/home/me/orgw/05_program.org");
    assert.equal(typeof row.activity, "number");
    assert.ok((row.activity as number) > 0);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("tt program list --json is newest activity first", () => {
  const root = fs.mkdtempSync("/tmp/tt-program-list-order-");
  try {
    const older = makeProgram(root, "older", "older", "/tmp/older.org", [
      JSON.stringify({ ts: "2026-01-01T00:00:00.000Z", event: { type: "NODE_STARTED", node: "a", runId: "run-a" } }),
    ]);
    const newer = makeProgram(root, "newer", "newer", "/tmp/newer.org", [
      JSON.stringify({ ts: "2026-01-02T00:00:00.000Z", event: { type: "NODE_STARTED", node: "a", runId: "run-b" } }),
    ]);
    // Pin the mtimes so the comparison cannot depend on creation order.
    fs.utimesSync(path.join(older, "events.jsonl"), new Date("2026-01-01T00:00:00Z"), new Date("2026-01-01T00:00:00Z"));
    fs.utimesSync(path.join(newer, "events.jsonl"), new Date("2026-01-02T00:00:00Z"), new Date("2026-01-02T00:00:00Z"));
    const rows = JSON.parse(cli(root, "program", "list", "--json")) as Array<{ id: string }>;
    assert.deepEqual(
      rows.map((r) => r.id),
      ["newer", "older"],
    );
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
