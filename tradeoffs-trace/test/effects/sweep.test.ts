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

test("a process outside the run's recorded process groups is reported held, never killed (A2)", async () => {
  // `setpgrp(0,0)` puts this process into its own new process group, so its
  // group is not one the run recorded at spawn — a detached descendant the
  // run cannot prove it owns. A2: it is HELD, not signalled.
  const escapee = spawn(
    "/usr/bin/perl",
    ["-e", 'setpgrp(0,0); chdir $ARGV[0] or die; sleep 100;', dir],
    { detached: true, stdio: "ignore" },
  );
  try {
    await waitFor(() => {
      try {
        const out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "p"], { encoding: "utf8" });
        return out.includes(`p${escapee.pid}`);
      } catch {
        return false;
      }
    });

    const result = await sweep(dir);
    assert.deepEqual(result.killed, [], "a foreign group is never signalled");
    assert.equal(result.tainted, false);
    assert.equal(result.held?.length, 1);
    assert.equal(result.held?.[0].pid, escapee.pid);
    assert.match(result.held?.[0].command, /perl/);
    assert.ok(processAlive(escapee.pid!), "the held process is still alive");
  } finally {
    escapee.kill("SIGKILL");
  }
});

test("kills a process in one of the run's own recorded process groups (A2)", async () => {
  const own = spawn(
    "/usr/bin/perl",
    ["-e", 'setpgrp(0,0); chdir $ARGV[0] or die; sleep 100;', dir],
    { detached: true, stdio: "ignore" },
  );
  await waitFor(() => {
    try {
      const out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "p"], { encoding: "utf8" });
      return out.includes(`p${own.pid}`);
    } catch {
      return false;
    }
  });

  const result = await sweep(dir, { ownPgids: [own.pid!] });
  assert.equal(result.tainted, true);
  assert.equal(result.killed.length, 1);
  assert.equal(result.killed[0].pid, own.pid);
  await waitFor(() => !processAlive(own.pid!));
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

test("a process that only holds a file under the worktree (a bind mount's file server) is reported, never killed", async () => {
  // Docker Desktop's file sharing holds worktree files open for a live
  // gate's bind mounts while its cwd is elsewhere; sweeping it killed Docker
  // Desktop at every freeze (program 14, 14i).
  fs.writeFileSync(path.join(dir, "mounted.txt"), "x");
  const holder = spawn(
    "/usr/bin/perl",
    ["-e", 'setpgrp(0,0); open(my $f, "<", "$ARGV[0]/mounted.txt") or die; chdir "/" or die; sleep 100;', dir],
    { detached: true, stdio: "ignore" },
  );
  try {
    // lsof can exit 1 while still listing a process that holds a file (not
    // a cwd) under the directory; its stdout is authoritative, as in sweep.ts.
    await waitFor(() => {
      let out = "";
      try {
        out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "p"], { encoding: "utf8" });
      } catch (err) {
        out = String((err as { stdout?: string }).stdout ?? "");
      }
      return out.includes(`p${holder.pid}`);
    });
    const result = await sweep(dir);
    assert.deepEqual(result.killed, [], "nothing whose cwd is elsewhere is killed");
    assert.equal(result.tainted, false);
    assert.equal(result.held?.length, 1);
    assert.equal(result.held?.[0].pid, holder.pid);
    assert.ok(processAlive(holder.pid!), "the holder is still alive");
  } finally {
    holder.kill("SIGKILL");
  }
});

test("system and application processes are protected by path", async () => {
  const { isProtectedCommand } = await import("../../src/effects/sweep.ts");
  assert.equal(isProtectedCommand("/Applications/Docker.app/Contents/MacOS/com.docker.backend"), true);
  assert.equal(isProtectedCommand("/System/Library/Frameworks/Virtualization.framework/Versions/A/XPCServices/x"), true);
  assert.equal(isProtectedCommand("/usr/bin/perl"), false);
  assert.equal(isProtectedCommand("node"), false);
});

// C1 (plan 06e): every sweep path (stop, timeout, tainted reset, crash
// recovery) passes the run's own recorded process groups; this is the
// primitive they all call. A group the run did not record is never signalled,
// whatever the process's cwd is.
test("plan 06e: the sweep never signals a process outside the run's process groups", async () => {
  // One process with cwd under the worktree, one holding a file with cwd
  // elsewhere: both are foreign (their groups were never recorded), so both
  // are HELD.
  const cwdHolder = spawn(
    "/usr/bin/perl",
    ["-e", 'setpgrp(0,0); chdir $ARGV[0] or die; sleep 100;', dir],
    { detached: true, stdio: "ignore" },
  );
  fs.writeFileSync(path.join(dir, "mounted.txt"), "x");
  const fileHolder = spawn(
    "/usr/bin/perl",
    ["-e", 'setpgrp(0,0); open(my $f, "<", "$ARGV[0]/mounted.txt") or die; chdir "/" or die; sleep 100;', dir],
    { detached: true, stdio: "ignore" },
  );
  try {
    await waitFor(() => {
      let out = "";
      try {
        out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "p"], { encoding: "utf8" });
      } catch (err) {
        out = String((err as { stdout?: string }).stdout ?? "");
      }
      return out.includes(`p${cwdHolder.pid}`) && out.includes(`p${fileHolder.pid}`);
    });

    const result = await sweep(dir, { ownPgids: [process.pid] });
    assert.deepEqual(result.killed, [], "no foreign process is ever signalled");
    assert.equal(result.tainted, false);
    assert.equal(result.held?.length, 2);
    assert.ok(processAlive(cwdHolder.pid!), "the cwd-holder is still alive");
    assert.ok(processAlive(fileHolder.pid!), "the file-holder is still alive");
  } finally {
    cwdHolder.kill("SIGKILL");
    fileHolder.kill("SIGKILL");
  }
});
