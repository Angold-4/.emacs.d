import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, test } from "node:test";
import { sweep } from "../../src/effects/sweep.ts";

let dir: string;

beforeEach(() => {
  dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-sweep-test-"));
});

afterEach(() => {
  fs.rmSync(dir, { recursive: true, force: true });
});

function processAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
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

test("finds, kills and reports a process that escaped its group", async () => {
  // `setpgrp(0,0)` puts this process into its own new process group, so it
  // is not reachable via `kill(-pgid, ...)` on whatever group started it —
  // exactly the "detached descendant" scenario design §2.2 says a sweep
  // (not group-kill) has to catch.
  const escapee = spawn(
    "/usr/bin/perl",
    ["-e", 'setpgrp(0,0); chdir $ARGV[0] or die; sleep 100;', dir],
    { detached: true, stdio: "ignore" },
  );
  await waitFor(() => {
    try {
      const out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "p"], { encoding: "utf8" });
      return out.includes(`p${escapee.pid}`);
    } catch {
      return false;
    }
  });

  const result = await sweep(dir);
  assert.equal(result.tainted, true);
  assert.equal(result.killed.length, 1);
  assert.equal(result.killed[0].pid, escapee.pid);
  assert.match(result.killed[0].command, /perl/);

  await waitFor(() => !processAlive(escapee.pid!));
});

test("an empty directory sweeps clean", async () => {
  const result = await sweep(dir);
  assert.deepEqual(result.killed, []);
  assert.equal(result.tainted, false);
});

test("exceptPids protects an intentionally-running process", async () => {
  const survivor = spawn(
    "/usr/bin/perl",
    ["-e", 'setpgrp(0,0); chdir $ARGV[0] or die; sleep 100;', dir],
    { detached: true, stdio: "ignore" },
  );
  try {
    await waitFor(() => {
      try {
        const out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "p"], { encoding: "utf8" });
        return out.includes(`p${survivor.pid}`);
      } catch {
        return false;
      }
    });
    const result = await sweep(dir, { exceptPids: [survivor.pid!] });
    assert.deepEqual(result.killed, []);
    assert.equal(result.tainted, false);
    assert.ok(processAlive(survivor.pid!));
  } finally {
    survivor.kill("SIGKILL");
  }
});
