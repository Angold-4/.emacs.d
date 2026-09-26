import assert from "node:assert/strict";
import { type ChildProcess, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, test } from "node:test";
import { acquireLock, acquireWaitingLock, Lock, LockError } from "../../src/effects/lock.ts";

let dir: string;
const spawned: ChildProcess[] = [];

beforeEach(() => {
  dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-lock-test-"));
});

afterEach(async () => {
  for (const child of spawned.splice(0)) {
    if (child.exitCode === null && child.signalCode === null) {
      try {
        child.kill("SIGKILL");
      } catch {
        // Already gone.
      }
    }
  }
  fs.rmSync(dir, { recursive: true, force: true });
});

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

test("a second acquire fails fast, naming the lock path", async () => {
  const lockPath = path.join(dir, "conductor.lock");
  const first = await acquireLock(lockPath);
  try {
    const start = Date.now();
    await assert.rejects(
      acquireLock(lockPath),
      (err: unknown) => {
        assert.ok(err instanceof LockError);
        assert.ok(err.message.includes(lockPath), `error should name the lock path: ${err.message}`);
        return true;
      },
    );
    const elapsed = Date.now() - start;
    assert.ok(elapsed < 2000, `second acquire should fail fast, took ${elapsed}ms`);
  } finally {
    await first.release();
  }
});

// Plan 01f: the machine-wide gate lock WAITS for the holder instead of
// failing, so two phases never gate at once: the second acquirer resolves
// only after the first releases.
test("acquireWaitingLock waits for the holder, then succeeds", async () => {
  const lockPath = path.join(dir, "gate.lock");
  const first = await acquireWaitingLock(lockPath);
  let secondSettled = false;
  const second = acquireWaitingLock(lockPath).then((lock) => {
    secondSettled = true;
    return lock;
  });
  await delay(300);
  assert.equal(secondSettled, false, "the second acquirer must wait while the first holds the lock");
  await first.release();
  const lock2 = await second;
  assert.equal(secondSettled, true);
  await lock2.release();
});

test("release lets a subsequent acquire succeed", async () => {
  const lockPath = path.join(dir, "conductor.lock");
  const first = await acquireLock(lockPath);
  await first.release();
  const second = await acquireLock(lockPath);
  await second.release();
});

/** Runs a script under a fresh `node` process that acquires the lock and
 * then idles, so this test can SIGKILL that whole process (simulating the
 * conductor dying) without touching the perl helper directly, and observe
 * that the flock — held by the perl grandchild — is released anyway
 * because the OS closes the node parent's pipe to it. */
function spawnHolder(lockPath: string): ChildProcess {
  const script = `
    const { acquireLock } = await import(${JSON.stringify(
      path.join(import.meta.dirname, "..", "..", "src", "effects", "lock.ts"),
    )});
    const lock = await acquireLock(${JSON.stringify(lockPath)});
    console.log("holder-ready");
    await new Promise(() => {});
  `;
  const child = spawn(process.execPath, ["--input-type=module", "-e", script], {
    stdio: ["ignore", "pipe", "pipe"],
  });
  spawned.push(child);
  return child;
}

test("SIGKILLing the holder process releases the lock", async () => {
  const lockPath = path.join(dir, "conductor.lock");
  const holder = spawnHolder(lockPath);

  await new Promise<void>((resolve, reject) => {
    let out = "";
    holder.stdout!.on("data", (chunk: Buffer) => {
      out += chunk.toString();
      if (out.includes("holder-ready")) resolve();
    });
    holder.once("exit", (code) => reject(new Error(`holder exited early with code ${code}`)));
    setTimeout(() => reject(new Error("holder never reported ready")), 5000);
  });

  // Confirm it really is held.
  await assert.rejects(acquireLock(lockPath), LockError);

  holder.kill("SIGKILL");
  await new Promise<void>((resolve) => holder.once("exit", () => resolve()));

  // The perl helper's stdin (inherited from the killed node process) hits
  // EOF as soon as the OS closes the node process's fds, so this should
  // succeed without a long poll — but give it a short grace window in
  // case of scheduling delay.
  let lock: Lock | undefined;
  const deadline = Date.now() + 3000;
  let lastErr: unknown;
  while (!lock && Date.now() < deadline) {
    try {
      lock = await acquireLock(lockPath);
    } catch (err) {
      lastErr = err;
      await delay(50);
    }
  }
  assert.ok(lock, `lock should be acquirable again after the holder was SIGKILLed: ${lastErr}`);
  await lock!.release();
});
