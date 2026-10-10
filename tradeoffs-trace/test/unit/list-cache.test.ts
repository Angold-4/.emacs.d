// Plan 06f (A1): `tt list` caches each run's row in `<run>/views/row.json`,
// keyed by the size and mtime of its `events.jsonl`, and rebuilds only the
// rows whose log changed. These tests run the REAL `tt list` command against
// fixture runs (no conductor), so they exercise the shipped CLI path.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import * as fs from "node:fs";
import * as path from "node:path";
import { performance } from "node:perf_hooks";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function plan(title: string): unknown {
  return {
    title,
    repo: "/tmp/tt-list-cache-repo",
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
  };
}

/** One fixture run: `meta.json`, a plan snapshot, and an `events.jsonl` with
 * only an `init` record (so `buildView` never shells out to git). */
function makeRun(root: string, id: string, title: string): string {
  const dir = path.join(root, id);
  fs.mkdirSync(path.join(dir, "plan"), { recursive: true });
  fs.mkdirSync(path.join(dir, "views"), { recursive: true });
  fs.writeFileSync(path.join(dir, "meta.json"), JSON.stringify({ title }));
  fs.writeFileSync(path.join(dir, "plan", "v1.json"), JSON.stringify(plan(title)));
  fs.writeFileSync(
    path.join(dir, "events.jsonl"),
    `${JSON.stringify({ seq: 1, ts: "2026-01-01T00:00:00.000Z", kind: "init", event: { runId: id, integrationHead: "abc" } })}\n`,
  );
  return dir;
}

function cli(root: string, ...args: string[]): string {
  return execFileSync(process.execPath, [CLI, ...args, "--root", root], { encoding: "utf8", maxBuffer: 32 * 1024 * 1024 });
}

function rowFiles(root: string): Map<string, string> {
  const out = new Map<string, string>();
  for (const name of fs.readdirSync(root)) {
    const file = path.join(root, name, "views", "row.json");
    if (fs.existsSync(file)) out.set(name, fs.readFileSync(file, "utf8"));
  }
  return out;
}

function digest(text: string): string {
  return createHash("sha256").update(text).digest("hex");
}

test("plan 06f: tt list --json over 100 fixture runs with none changed finishes under 0.3 s", () => {
  const root = fs.mkdtempSync("/tmp/tt-list-cache-100-");
  try {
    for (let i = 0; i < 100; i++) makeRun(root, `run-${String(i).padStart(3, "0")}`, `run ${i}`);
    // First call: every row is a miss and is written to views/row.json.
    const first = cli(root, "list", "--json");
    assert.equal((JSON.parse(first) as unknown[]).length, 100);
    // The rows are now cached; the timed call must not rebuild any of them.
    // The best of three samples measures the command's own cost: a single
    // sample also carries process-scheduling noise from the rest of the
    // concurrently running suite (standalone this command takes ~0.14s).
    let best = Infinity;
    let second = first;
    for (let i = 0; i < 3; i++) {
      const start = performance.now();
      second = cli(root, "list", "--json");
      best = Math.min(best, (performance.now() - start) / 1000);
    }
    assert.equal(second, first, "a fully cached listing is byte-identical");
    assert.ok(best < 0.3, `100 cached rows listed in ${best.toFixed(3)}s (must be under 0.3s)`);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06f: only a changed run's row is rebuilt", () => {
  const root = fs.mkdtempSync("/tmp/tt-list-cache-one-");
  try {
    for (let i = 0; i < 5; i++) makeRun(root, `run-${i}`, `run ${i}`);
    cli(root, "list", "--json");
    const before = rowFiles(root);
    assert.equal(before.size, 5, "every run has a cached row after the first listing");
    // Append a valid but state-neutral record (an `intent`, which every fold
    // ignores) to exactly one run's log: its key (size/mtime) changes, and
    // only its row may be rebuilt.
    const changed = "run-2";
    fs.appendFileSync(
      path.join(root, changed, "events.jsonl"),
      `${JSON.stringify({ seq: 2, ts: "2026-01-01T00:00:01.000Z", kind: "intent", actionId: "x", event: {} })}\n`,
    );
    cli(root, "list", "--json");
    const after = rowFiles(root);
    const changedNames: string[] = [];
    for (const [name, text] of after) {
      if (digest(text) !== digest(before.get(name) ?? "")) changedNames.push(name);
    }
    assert.deepEqual(changedNames, [changed], "exactly the run whose events.jsonl grew has a rebuilt row");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 06f: tt list output is unchanged for a fixture root", () => {
  const root = fs.mkdtempSync("/tmp/tt-list-cache-out-");
  try {
    for (let i = 0; i < 6; i++) makeRun(root, `run-${i}`, `run ${i}`);
    // The cached output (rows populated by the first call).
    cli(root, "list", "--json");
    const cachedJson = cli(root, "list", "--json");
    const cachedPlain = cli(root, "list");
    // The pre-cache output: delete every row.json and list again, so the CLI
    // rebuilds each row from scratch, exactly as it did before the cache.
    for (const name of fs.readdirSync(root)) {
      fs.rmSync(path.join(root, name, "views", "row.json"), { force: true });
    }
    const rebuiltJson = cli(root, "list", "--json");
    const rebuiltPlain = cli(root, "list");
    assert.equal(cachedJson, rebuiltJson, "--json output is identical cached and rebuilt");
    assert.equal(cachedPlain, rebuiltPlain, "plain output is identical cached and rebuilt");
    // And the cache is populated again by the rebuilt listing.
    assert.equal(rowFiles(root).size, 6);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
