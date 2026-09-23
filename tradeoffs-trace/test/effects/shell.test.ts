import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, test } from "node:test";
import { killGroup, runCommand } from "../../src/effects/shell.ts";

let dir: string;

beforeEach(() => {
  dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-shell-test-"));
});

afterEach(() => {
  fs.rmSync(dir, { recursive: true, force: true });
});

function groupMembers(pgid: number): string[] {
  try {
    const out = execFileSync("ps", ["-Ao", "pid=,pgid=,comm="], { encoding: "utf8" });
    return out
      .split("\n")
      .map((l) => l.trim())
      .filter((l) => l.length > 0)
      .filter((l) => {
        const parts = l.split(/\s+/);
        return parts[1] === String(pgid);
      });
  } catch {
    return [];
  }
}

function waitFor(predicate: () => boolean, timeoutMs = 3000): Promise<void> {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const poll = () => {
      if (predicate()) {
        resolve();
        return;
      }
      if (Date.now() - start > timeoutMs) {
        reject(new Error("timed out waiting for condition"));
        return;
      }
      setTimeout(poll, 20);
    };
    poll();
  });
}

test("onIntent observes the pgid strictly before the command's first side effect", async () => {
  const marker = path.join(dir, "marker");
  let markerExistedDuringIntent = true;
  let intentPgid: number | undefined;
  const { result } = runCommand({
    command: `touch ${marker}`,
    onIntent: ({ pgid }) => {
      intentPgid = pgid;
      markerExistedDuringIntent = fs.existsSync(marker);
    },
  });
  const outcome = await result;
  assert.equal(outcome.exitCode, 0);
  assert.equal(markerExistedDuringIntent, false);
  assert.ok(intentPgid && intentPgid > 0);
  assert.ok(fs.existsSync(marker));
});

test("cancel() kills the whole group, including grandchildren", async () => {
  let pgid!: number;
  const { result, cancel, pgid: pgidPromise } = runCommand({
    command: "sleep 100 & sleep 100 & wait",
  });
  pgid = await pgidPromise;
  await waitFor(() => groupMembers(pgid).length >= 3);
  cancel();
  const outcome = await result;
  assert.equal(outcome.cancelled, true);
  await waitFor(() => groupMembers(pgid).length === 0);
  assert.deepEqual(groupMembers(pgid), []);
});

test("deadline escalation: a TERM-ignoring command ends in SIGKILL", async () => {
  const { result } = runCommand({
    command: "trap '' TERM; sleep 100",
    deadlineMs: 100,
    termGraceMs: 300,
  });
  const outcome = await result;
  assert.equal(outcome.timedOut, true);
  assert.deepEqual(outcome.signalsSent, ["SIGTERM", "SIGKILL"]);
  assert.equal(outcome.signal, "SIGKILL");
});

test("a command that honors SIGTERM does not escalate to SIGKILL", async () => {
  const { result } = runCommand({
    command: "trap 'exit 7' TERM; sleep 100",
    deadlineMs: 100,
    termGraceMs: 2000,
  });
  const outcome = await result;
  assert.equal(outcome.timedOut, true);
  assert.deepEqual(outcome.signalsSent, ["SIGTERM"]);
});

test("cancel()", async () => {
  const { result, cancel, pgid: pgidPromise } = runCommand({ command: "sleep 100" });
  const pgid = await pgidPromise;
  cancel();
  const outcome = await result;
  assert.equal(outcome.cancelled, true);
  assert.ok(outcome.signalsSent.includes("SIGTERM"));
  await waitFor(() => groupMembers(pgid).length === 0);
});

test("exit code propagation", async () => {
  const { result } = runCommand({ command: "exit 42" });
  const outcome = await result;
  assert.equal(outcome.exitCode, 42);
  assert.equal(outcome.signal, null);
  assert.equal(outcome.timedOut, false);
  assert.equal(outcome.cancelled, false);
});

test("output capture, streamed and accumulated", async () => {
  const streamed: string[] = [];
  const { result } = runCommand({
    command: "echo hello; echo world 1>&2",
    onOutput: (chunk) => streamed.push(chunk),
  });
  const outcome = await result;
  assert.equal(outcome.exitCode, 0);
  assert.ok(outcome.output.includes("hello"));
  assert.ok(outcome.output.includes("world"));
  assert.ok(streamed.join("").includes("hello"));
  assert.ok(streamed.join("").includes("world"));
});

test("killGroup terminates a previously recorded pgid (recovery path)", async () => {
  const { pgid: pgidPromise, result } = runCommand({ command: "sleep 100 & sleep 100 & wait" });
  const pgid = await pgidPromise;
  await waitFor(() => groupMembers(pgid).length >= 3);
  const { signalsSent } = await killGroup(pgid, { termGraceMs: 200 });
  assert.ok(signalsSent.includes("SIGTERM"));
  await waitFor(() => groupMembers(pgid).length === 0);
  await result;
});
