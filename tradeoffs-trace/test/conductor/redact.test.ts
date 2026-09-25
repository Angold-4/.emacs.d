// Plan 01a: `tt redact <run-dir-or-id | --all> [--secrets NAME…]` — the tool
// that cleans a run that already leaked a value (721 occurrences across seven
// phases and the refs/ copies of the vendor docs in atlas plan 13, runtime doc
// §7). It rewrites run directories in place, reading the values from this
// process's environment, and it must not corrupt JSONL.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { cleanupDir, makeRepo, makeRunRoot } from "./harness.ts";
import { createRun, runPaths, type RunPlanFile } from "../../src/conductor.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function runCli(args: string[], env: NodeJS.ProcessEnv = {}): Promise<{ code: number | null; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, ...env } });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ code, stdout, stderr }));
  });
}

/** A run directory with the value planted in the stream, the control log and
 * the check logs — the three places the measured run leaked into. */
function plantedRun(root: string, id: string, value: string, withPlanSecrets: boolean): string {
  const runDir = path.join(root, id);
  fs.mkdirSync(path.join(runDir, "stream"), { recursive: true });
  fs.mkdirSync(path.join(runDir, "checks", "c0ffee"), { recursive: true });
  fs.mkdirSync(path.join(runDir, "plan"), { recursive: true });
  fs.writeFileSync(path.join(runDir, "meta.json"), JSON.stringify({ title: id }));
  fs.writeFileSync(
    path.join(runDir, "plan", "v1.json"),
    JSON.stringify(withPlanSecrets ? { secrets: ["FAKE_KEY"] } : { title: "old plan" }),
  );
  fs.appendFileSync(
    path.join(runDir, "events.jsonl"),
    `${JSON.stringify({ seq: 1, ts: "2026-01-01T00:00:00.000Z", kind: "event", event: { type: "SUBMIT_PHASE", text: `PYTH_ACCESS_TOKEN='${value}'` } })}\n`,
  );
  fs.appendFileSync(
    path.join(runDir, "stream", "worker-1.jsonl"),
    `${JSON.stringify({ agentId: "worker-1", event: { type: "tool_execution_start", args: { command: `echo ${value}` } } })}\n`,
  );
  fs.writeFileSync(path.join(runDir, "checks", "c0ffee", "true.log"), `$ echo ${value}\n${value}\nexit 0\n`);
  return runDir;
}

function jsonlLinesParse(file: string): void {
  for (const line of fs.readFileSync(file, "utf8").split("\n")) {
    if (line.length === 0) continue;
    assert.doesNotThrow(() => JSON.parse(line), `${file}: every line must still parse: ${line.slice(0, 80)}`);
  }
}

test("tt redact --all: a planted value disappears and every JSONL line still parses", async () => {
  const root = fs.mkdtempSync("/tmp/tt-redact-root-");
  const value = `sk-live-${randomBytes(12).toString("hex")}`;
  try {
    const declared = plantedRun(root, "aaaa1111", value, true);
    const undeclared = plantedRun(root, "bbbb2222", value, false);
    // An older run's plan has no `secrets` field, so its names come from the
    // command line (the values always come from the environment).
    const result = await runCli(["redact", "--all", "--secrets", "FAKE_KEY", "--root", root], { FAKE_KEY: value });
    assert.equal(result.code, 0, result.stderr);

    for (const runDir of [declared, undeclared]) {
      for (const rel of ["events.jsonl", "stream/worker-1.jsonl", "checks/c0ffee/true.log"]) {
        const text = fs.readFileSync(path.join(runDir, rel), "utf8");
        assert.ok(!text.includes(value), `${rel} of ${path.basename(runDir)} still holds the value`);
        assert.match(text, /\*\*\*FAKE_KEY\*\*\*/);
      }
      jsonlLinesParse(path.join(runDir, "events.jsonl"));
      jsonlLinesParse(path.join(runDir, "stream", "worker-1.jsonl"));
    }
    assert.match(result.stdout, /redacted 2 of 2 run\(s\), \d+ file\(s\)/);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("tt redact: a run whose conductor is alive is refused, and --force overrides it", async () => {
  const root = fs.mkdtempSync("/tmp/tt-redact-live-");
  const value = `sk-live-${randomBytes(12).toString("hex")}`;
  try {
    const runDir = plantedRun(root, "eeee5555", value, true);
    // A pid that is alive: this very test process.
    fs.writeFileSync(path.join(runDir, "conductor.pid"), String(process.pid));
    const refused = await runCli(["redact", "--all", "--root", root], { FAKE_KEY: value });
    assert.equal(refused.code, 0, refused.stderr);
    assert.match(refused.stdout, /redacted 0 of 1 run\(s\)/);
    assert.match(refused.stdout, /conductor still running: eeee5555/);
    assert.ok(fs.readFileSync(path.join(runDir, "events.jsonl"), "utf8").includes(value), "a live run is left alone");

    const forced = await runCli(["redact", "--all", "--force", "--root", root], { FAKE_KEY: value });
    assert.equal(forced.code, 0, forced.stderr);
    assert.match(forced.stdout, /redacted 1 of 1 run\(s\)/);
    assert.ok(!fs.readFileSync(path.join(runDir, "events.jsonl"), "utf8").includes(value));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("tt redact: a UTF-16 document is cleaned, and a file it could not search is named", async () => {
  const root = fs.mkdtempSync("/tmp/tt-redact-utf16-");
  const value = `sk-live-${randomBytes(12).toString("hex")}`;
  // A value holding a quote is stored JSON-escaped wherever it was written into
  // JSON text (finding M-9), and it appears that way in UTF-16 too.
  const quoted = 'sk-"q"';
  try {
    const runDir = plantedRun(root, "ffff6666", value, true);
    // The vendor reference doc case: UTF-16, so its bytes contain NULs.
    fs.mkdirSync(path.join(runDir, "refs"), { recursive: true });
    fs.writeFileSync(path.join(runDir, "refs", "vendor.md"), Buffer.from(`key ${value} end`, "utf16le"));
    fs.writeFileSync(path.join(runDir, "refs", "vendor.json"), `{"k":${JSON.stringify(quoted)}}\n`);
    fs.writeFileSync(path.join(runDir, "refs", "vendor-utf16.json"), Buffer.from(`{"k":${JSON.stringify(quoted)}}`, "utf16le"));
    // A genuinely binary artefact, holding nothing this tool can search.
    fs.mkdirSync(path.join(runDir, "checks", "c0ffee"), { recursive: true });
    fs.writeFileSync(path.join(runDir, "checks", "c0ffee", "artifact.png"), Buffer.from([0x89, 0x50, 0x00, 0x00, 0x01]));
    const result = await runCli(["redact", runDir, "--secrets", "FAKE_KEY QUOTED_KEY"], { FAKE_KEY: value, QUOTED_KEY: quoted });
    assert.equal(result.code, 0, result.stderr);
    assert.equal(
      fs.readFileSync(path.join(runDir, "refs", "vendor.md"), "utf16le"),
      "key ***FAKE_KEY*** end",
      "a UTF-16 document is masked, not skipped",
    );
    assert.equal(fs.readFileSync(path.join(runDir, "refs", "vendor.json"), "utf8"), '{"k":"***QUOTED_KEY***"}\n');
    assert.doesNotThrow(() => JSON.parse(fs.readFileSync(path.join(runDir, "refs", "vendor.json"), "utf8")));
    assert.equal(fs.readFileSync(path.join(runDir, "refs", "vendor-utf16.json"), "utf16le"), '{"k":"***QUOTED_KEY***"}');
    assert.match(result.stdout, /not UTF-8 text, searched UTF-8\/UTF-16 only[^\n]*artifact\.png/);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("tt redact --all: a run's own plan snapshot supplies the names", async () => {
  const root = fs.mkdtempSync("/tmp/tt-redact-plan-");
  const value = `sk-live-${randomBytes(12).toString("hex")}`;
  try {
    const runDir = plantedRun(root, "dddd4444", value, true);
    const result = await runCli(["redact", "--all", "--root", root], { FAKE_KEY: value });
    assert.equal(result.code, 0, result.stderr);
    assert.ok(!fs.readFileSync(path.join(runDir, "events.jsonl"), "utf8").includes(value));
    jsonlLinesParse(path.join(runDir, "events.jsonl"));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("tt status/state/timing: a value a run already leaked is not printed", async () => {
  const repo = makeRepo();
  const root = makeRunRoot();
  const value = `sk-live-${randomBytes(12).toString("hex")}`;
  try {
    const plan: RunPlanFile = {
      title: "leaky",
      repo: repo.dir,
      integrationBranch: "main",
      checks: ["true"],
      secrets: ["FAKE_KEY"],
      phases: [{ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }],
    };
    const runDir = createRun(root, plan);
    // As a run started before this plan left them: the value inside the
    // control log and inside an agent's stream file.
    const p = runPaths(runDir);
    fs.appendFileSync(
      p.events,
      `${JSON.stringify({ seq: 1, ts: "2026-01-01T00:00:00.000Z", kind: "init", event: { runId: "r1", integrationHead: repo.head } })}\n`,
    );
    fs.appendFileSync(
      p.events,
      `${JSON.stringify({ seq: 2, ts: "2026-01-01T00:00:01.000Z", kind: "event", event: { type: "X", text: `PYTH_ACCESS_TOKEN='${value}'` } })}\n`,
    );
    fs.writeFileSync(
      path.join(p.stream, "worker-1.jsonl"),
      `${JSON.stringify({ agentId: "worker-1", ts: "2026-01-01T00:00:02.000Z", event: { type: "tool_execution_start", toolCallId: "t1", toolName: "sh", args: { command: `curl -H 'Bearer ${value}' x` } } })}\n` +
        `${JSON.stringify({ agentId: "worker-1", ts: "2026-01-01T00:00:03.000Z", event: { type: "tool_execution_end", toolCallId: "t1", result: { content: [{ type: "text", text: "ok" }] } } })}\n`,
    );

    for (const args of [["status", runDir], ["state", runDir], ["timing", runDir]]) {
      const result = await runCli(args, { FAKE_KEY: value });
      assert.equal(result.code, 0, `${args[0]}: ${result.stderr}`);
      assert.ok(!result.stdout.includes(value), `tt ${args[0]} printed the value`);
    }
    // `tt state`'s JSON must still parse after redaction.
    const state = await runCli(["state", runDir], { FAKE_KEY: value });
    assert.doesNotThrow(() => JSON.parse(state.stdout));
    const timing = await runCli(["timing", runDir], { FAKE_KEY: value });
    assert.match(timing.stdout, /\*\*\*FAKE_KEY\*\*\*/, "the timing view shows the mask, not the value");
  } finally {
    cleanupDir(root);
    cleanupDir(repo.dir);
  }
});

test("tt redact: an unset secret is reported, and a single run directory can be named", async () => {
  const root = fs.mkdtempSync("/tmp/tt-redact-one-");
  const value = `sk-live-${randomBytes(12).toString("hex")}`;
  try {
    const runDir = plantedRun(root, "cccc3333", value, false);
    const unset = await runCli(["redact", runDir, "--secrets", "FAKE_KEY"]);
    assert.equal(unset.code, 0, unset.stderr);
    assert.match(unset.stdout, /secret FAKE_KEY not set/);
    assert.ok(fs.readFileSync(path.join(runDir, "events.jsonl"), "utf8").includes(value), "nothing was redacted");

    const redacted = await runCli(["redact", runDir, "--secrets", "FAKE_KEY"], { FAKE_KEY: value });
    assert.equal(redacted.code, 0, redacted.stderr);
    assert.match(redacted.stdout, /redacted 1 of 1 run\(s\)/);
    assert.ok(!fs.readFileSync(path.join(runDir, "events.jsonl"), "utf8").includes(value));
    jsonlLinesParse(path.join(runDir, "events.jsonl"));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
