// Plan 03c: a readable id `<program>-NN` resolves to the same run a run id
// does, for every command that accepts a run id, and a retried node keeps the
// readable id of its position (the retry is a fresh run under the same name).

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { createRun, type RunPlanFile } from "../../src/conductor.ts";
import { programPaths, writeProgramChart } from "../../src/program.ts";
import type { ProgramFile } from "../../src/core/program.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function plan(title: string): RunPlanFile {
  return {
    title,
    repo: "",
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
  } as unknown as RunPlanFile;
}

function cli(root: string, ...args: string[]): string {
  return execFileSync(process.execPath, [CLI, ...args, "--root", root], { encoding: "utf8", maxBuffer: 16 * 1024 * 1024 });
}

/** `tt state` mints a runId from the log's init event; a run `createRun` made
 * without one gets a fresh random id per invocation, so it is not comparable.
 * Everything else must be byte-identical for the two ways to name the run. */
function stableState(line: string): string {
  const parsed = JSON.parse(line) as { state: { phase: { runId?: string } } };
  delete parsed.state.phase.runId;
  return JSON.stringify(parsed);
}

function makeProgram(root: string, runIds: Record<string, string>): string {
  const dir = path.join(root, "programs", "prog0001");
  fs.mkdirSync(dir, { recursive: true });
  const program: ProgramFile = {
    title: "plan 14",
    maxParallel: 2,
    entries: [
      { id: "a", after: [], plan: plan("14a") },
      { id: "b", after: ["a"], plan: plan("14b") },
    ],
    readableIds: { a: "prog0001-01", b: "prog0001-02" },
  };
  fs.writeFileSync(path.join(dir, "program.json"), JSON.stringify(program));
  const events: Array<{ ts: string; event: unknown }> = [];
  for (const [node, runId] of Object.entries(runIds)) {
    events.push({ ts: new Date().toISOString(), event: { type: "NODE_STARTED", node, runId } });
  }
  fs.writeFileSync(path.join(dir, "events.jsonl"), `${events.map((e) => JSON.stringify(e)).join("\n")}\n`);
  return dir;
}

test("tt status and tt state accept <program>-NN and equal the run id's output", () => {
  const root = fs.mkdtempSync("/tmp/tt-readable-");
  try {
    const runA = path.basename(createRun(root, plan("14a"), "run-a"));
    createRun(root, plan("14b"), "run-b");
    makeProgram(root, { a: runA, b: "run-b" });

    assert.equal(cli(root, "status", "prog0001-02"), cli(root, "status", "run-b"));
    assert.equal(stableState(cli(root, "state", "prog0001-02")), stableState(cli(root, "state", "run-b")));
    assert.equal(cli(root, "status", "prog0001-01"), cli(root, "status", "run-a"));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("tt program state carries the id, the readable ids and the source path", () => {
  const root = fs.mkdtempSync("/tmp/tt-readable-state-");
  try {
    const runA = path.basename(createRun(root, plan("14a"), "run-a"));
    const dir = makeProgram(root, { a: runA });
    fs.writeFileSync(path.join(dir, "source.json"), JSON.stringify({ path: "/home/me/orgw/work/atlas/indexps/14_program.org" }));
    const s = JSON.parse(cli(root, "program", "state", "prog0001")) as { id: string; sourcePath: string; lines: string[] };
    assert.equal(s.id, "prog0001");
    assert.equal(s.sourcePath, "/home/me/orgw/work/atlas/indexps/14_program.org");
    assert.ok(s.lines.some((l) => l.includes("prog0001-01")), "the status lines name the readable id");
    // The chart is written beside the program and uses the readable ids.
    writeProgramChart(dir);
    const chart = fs.readFileSync(programPaths(dir).programView, "utf8");
    assert.match(chart, /prog0001-01\s+a/);
    assert.match(chart, /after: prog0001-01/);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("a 100th node's readable id (three digits) still resolves", () => {
  const root = fs.mkdtempSync("/tmp/tt-readable-100-");
  try {
    const run100 = path.basename(createRun(root, plan("14a"), "run-100"));
    const dir = path.join(root, "programs", "prog0100");
    fs.mkdirSync(dir, { recursive: true });
    const phases = Array.from({ length: 100 }, (_, i) => ({ id: `p${i + 1}`, goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }));
    const program: ProgramFile = { title: "100 nodes", maxParallel: 1, entries: [{ id: "e", after: [], plan: { ...plan("e"), phases } as RunPlanFile }] };
    fs.writeFileSync(path.join(dir, "program.json"), JSON.stringify(program));
    fs.writeFileSync(
      path.join(dir, "events.jsonl"),
      `${JSON.stringify({ ts: new Date().toISOString(), event: { type: "NODE_STARTED", node: "e/p100", runId: run100 } })}\n`,
    );
    // The id is <program>-100; the resolver must accept three digits (A-4).
    assert.equal(cli(root, "status", "prog0100-100"), cli(root, "status", run100));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("a retried node keeps its readable id and points at the new run", () => {
  const root = fs.mkdtempSync("/tmp/tt-readable-retry-");
  try {
    const oldRun = path.basename(createRun(root, plan("14b"), "run-old"));
    makeProgram(root, { b: oldRun });
    // The retry: a fresh run for the same node position.
    const newRun = path.basename(createRun(root, plan("14b"), "run-new"));
    const dir = path.join(root, "programs", "prog0001");
    fs.appendFileSync(
      path.join(dir, "events.jsonl"),
      `${JSON.stringify({ ts: new Date().toISOString(), event: { type: "NODE_RETRY", node: "b" } })}\n` +
        `${JSON.stringify({ ts: new Date().toISOString(), event: { type: "NODE_STARTED", node: "b", runId: newRun } })}\n`,
    );
    assert.equal(cli(root, "status", "prog0001-02"), cli(root, "status", newRun));
    assert.equal(stableState(cli(root, "state", "prog0001-02")), stableState(cli(root, "state", newRun)));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
