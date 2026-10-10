// conductor.lock: exactly one conductor per run (design §2.1, §9.1).
//
// Node has no built-in `flock(2)`. This environment has neither `flock(1)`
// nor `timeout(1)` installed, but `/usr/bin/perl` (system perl) is present
// and its `Fcntl` module wraps real `flock(2)`. So the lock is held by a
// small perl helper, spawned as a child process:
//
//   1. It opens (without truncating up front) `conductor.lock` and takes
//      `LOCK_EX | LOCK_NB` on it. On failure (`EWOULDBLOCK`) it prints
//      "busy" to stderr and exits non-zero immediately — so a second
//      `acquire` fails fast, per the design's "there is exactly one
//      conductor per run" (§2.1).
//   2. On success, it truncates the file and writes its own pid, so a
//      failed acquirer can report who holds the lock.
//   3. It prints a "ready" line on stdout, then blocks reading stdin.
//   4. It exits (and therefore releases the flock) the moment stdin hits
//      EOF — which happens automatically when the parent (conductor)
//      process dies for *any* reason, because the OS closes the pipe fd
//      the parent held open. This is what gives "if the conductor dies,
//      the lock is released" without the JS side having to do anything
//      when it is the one that dies.
//
// `release()` is the cooperative path: close the helper's stdin and wait
// for it to exit.

import { type ChildProcess, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

const PERL_BIN = "/usr/bin/perl";

// NB: written as a single perl -e argument. Kept simple and defensive:
// any failure to open/lock prints one line to stderr and exits 1.
const HELPER_SCRIPT = `
use strict;
use warnings;
use Fcntl qw(:flock);
use IO::Handle;

my $path = $ARGV[0];
open(my $fh, "+>>", $path) or do { print STDERR "open failed: $!\\n"; exit 1; };
unless (flock($fh, LOCK_EX | LOCK_NB)) {
  print STDERR "busy\\n";
  exit 1;
}
seek($fh, 0, 0);
truncate($fh, 0);
print $fh "$$\\n";
$fh->flush;

STDOUT->autoflush(1);
print STDOUT "ready $$\\n";

# Block until the parent closes our stdin (EOF), whether that is a clean
# release() or the parent dying. Either way, process exit releases LOCK_EX.
while (my $line = <STDIN>) { }
exit 0;
`;

/** Plan 01f: the same helper, but with a BLOCKING `flock(LOCK_EX)`: the
 * machine-wide gate lock (`~/.tradeoffs-trace/gate.lock`) must make a second
 * gate wait for the first, never fail it. A holder that dies releases the
 * lock the same way (its stdin hits EOF), so a crashed gate cannot wedge
 * every later one. */
const WAITING_HELPER_SCRIPT = HELPER_SCRIPT.replace("flock($fh, LOCK_EX | LOCK_NB)", "flock($fh, LOCK_EX)");

// ---------------------------------------------------------------------------
// Plan 06k1 (A4): the host-wide check window.
//
// A check run of ANY run on the host pauses the worker processes of every
// OTHER run while it holds the machine. The window is a process-global set
// plus one marker file per holder under `~/.tradeoffs-trace/check-window/`,
// so two conductors in one process (a test) and two conductors in separate
// processes (a host) both see each other's windows. The marker file holds
// the holder's pid; a crashed holder leaves a stale file, so a marker older
// than `CHECK_WINDOW_STALE_MS` is ignored.
// ---------------------------------------------------------------------------

export interface CheckWindow {
  release: () => void;
}

const activeCheckWindows = new Set<string>();
const CHECK_WINDOW_STALE_MS = 6 * 60 * 60 * 1000;

/** The window directory for a check lock: beside the lock, so a machine-wide
 * lock (`~/.tradeoffs-trace/check.lock`) gives a machine-wide window and a
 * per-run lock (tests) gives a per-run one. */
export function checkWindowDir(checkLockPath: string): string {
  return `${checkLockPath}.window.d`;
}

/** True while any check window for this check lock is open. */
export function anyCheckWindowOpen(checkLockPath: string): boolean {
  const dir = checkWindowDir(checkLockPath);
  if (activeCheckWindows.has(dir)) return true;
  try {
    if (!fs.existsSync(dir)) return false;
    const now = Date.now();
    for (const name of fs.readdirSync(dir)) {
      const file = path.join(dir, name);
      try {
        if (now - fs.statSync(file).mtimeMs > CHECK_WINDOW_STALE_MS) {
          fs.rmSync(file, { force: true });
          continue;
        }
      } catch {
        // a vanished file is not a window
      }
      return true;
    }
  } catch {
    // an unreadable dir is not a window
  }
  return false;
}

/** Opens a check window for its caller's check run. The returned `release`
 * closes it; call it in a `finally`. */
export function openCheckWindow(checkLockPath: string): CheckWindow {
  const dir = checkWindowDir(checkLockPath);
  activeCheckWindows.add(dir);
  const file = path.join(dir, `${process.pid}-${Math.random().toString(36).slice(2, 10)}.json`);
  try {
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(file, `${JSON.stringify({ pid: process.pid, at: new Date().toISOString() })}\n`);
  } catch {
    // A read-only home must not stop the check; the in-process set still
    // pauses this process's own other runs.
  }
  let released = false;
  return {
    release: () => {
      if (released) return;
      released = true;
      activeCheckWindows.delete(dir);
      try {
        fs.rmSync(file, { force: true });
      } catch {
        // best effort
      }
    },
  };
}

/** Plan 06k1 (A4): a timer whose clock stops while it is paused. Used for a
 * worker attempt's deadline: while another run's check window is open the
 * worker is SIGSTOPped and its attempt clock must not run, so a check that
 * takes minutes does not time the worker out. */
export class PausableTimer<T> {
  readonly promise: Promise<T>;
  #resolve!: (value: T) => void;
  #remaining: number;
  #timer: NodeJS.Timeout | undefined;
  #startedAt: number;
  #paused = false;
  #done = false;
  #value: T;

  constructor(ms: number, value: T) {
    this.#remaining = ms;
    this.#value = value;
    this.#startedAt = Date.now();
    this.promise = new Promise<T>((resolve) => {
      this.#resolve = resolve;
    });
    this.#timer = setTimeout(() => this.#fire(), ms);
  }

  #fire(): void {
    if (this.#done) return;
    this.#done = true;
    this.#resolve(this.#value);
  }

  pause(): void {
    if (this.#paused || this.#done) return;
    this.#paused = true;
    if (this.#timer) clearTimeout(this.#timer);
    this.#timer = undefined;
    this.#remaining = Math.max(0, this.#remaining - (Date.now() - this.#startedAt));
  }

  resume(): void {
    if (!this.#paused || this.#done) return;
    this.#paused = false;
    this.#startedAt = Date.now();
    this.#timer = setTimeout(() => this.#fire(), this.#remaining);
  }

  cancel(): void {
    this.#done = true;
    if (this.#timer) clearTimeout(this.#timer);
    this.#timer = undefined;
  }
}

export class LockError extends Error {
  lockPath: string;
  holderPid: string | undefined;

  constructor(lockPath: string, holderPid: string | undefined, reason: string) {
    super(`could not acquire lock ${lockPath}${holderPid ? ` (held by pid ${holderPid})` : ""}: ${reason}`);
    this.name = "LockError";
    this.lockPath = lockPath;
    this.holderPid = holderPid;
  }
}

function readHolderPid(lockPath: string): string | undefined {
  try {
    const text = fs.readFileSync(lockPath, "utf8").trim();
    return text.length > 0 ? text : undefined;
  } catch {
    return undefined;
  }
}

/** A held `conductor.lock`. Obtained via `acquireLock`. */
export class Lock {
  #child: ChildProcess;
  readonly path: string;
  readonly holderPid: number;

  private constructor(child: ChildProcess, path: string, holderPid: number) {
    this.#child = child;
    this.path = path;
    this.holderPid = holderPid;
  }

  static _fromChild(child: ChildProcess, path: string, holderPid: number): Lock {
    return new Lock(child, path, holderPid);
  }

  /** Closes the helper's stdin and waits for it to exit, releasing the
   * flock. Idempotent: calling it more than once, or after the helper has
   * already exited on its own, resolves immediately. */
  release(): Promise<void> {
    return new Promise((resolve) => {
      if (this.#child.exitCode !== null || this.#child.signalCode !== null) {
        resolve();
        return;
      }
      this.#child.once("exit", () => resolve());
      this.#child.stdin?.end();
    });
  }
}

/** Spawns the perl helper and resolves once it reports the lock is held,
 * or rejects with a `LockError` (naming `lockPath` and, if known, the
 * holder's pid) if the lock is already held by someone else or the helper
 * fails to start. Fails fast: it never waits for a timeout, because the
 * perl side uses `LOCK_EX | LOCK_NB`. */
export function acquireLock(lockPath: string): Promise<Lock> {
  return spawnLockHelper(HELPER_SCRIPT, lockPath);
}

/** Plan 01f: a machine-wide lock that WAITS for the holder instead of
 * failing (the perl side takes a blocking `LOCK_EX`). Used for the gate, so
 * two phases — in the same program or in different runs — never run their
 * expensive gate command at once; the second waits for the first (design
 * 01_ref_design.md's `~/.tradeoffs-trace/gate.lock`). A holder that died
 * releases the lock when its helper exits, so the wait is bounded by the
 * holder's own gate limit, never by this call. */
export function acquireWaitingLock(lockPath: string): Promise<Lock> {
  return spawnLockHelper(WAITING_HELPER_SCRIPT, lockPath);
}

function spawnLockHelper(helperScript: string, lockPath: string): Promise<Lock> {
  // A machine-wide lock lives under `~/.tradeoffs-trace/`, which a fresh
  // machine (a CI runner) does not have yet; perl's open does not create it.
  try {
    fs.mkdirSync(path.dirname(lockPath), { recursive: true });
  } catch {
    // The helper's open reports the real failure.
  }
  return new Promise((resolve, reject) => {
    const child = spawn(PERL_BIN, ["-e", helperScript, lockPath], {
      stdio: ["pipe", "pipe", "pipe"],
    });

    let settled = false;
    let stdout = "";
    let stderr = "";

    child.stdout?.on("data", (chunk: Buffer) => {
      stdout += chunk.toString("utf8");
      if (settled) return;
      const match = stdout.match(/ready (\d+)/);
      if (match) {
        settled = true;
        resolve(Lock._fromChild(child, lockPath, Number(match[1])));
      }
    });

    child.stderr?.on("data", (chunk: Buffer) => {
      stderr += chunk.toString("utf8");
    });

    child.once("error", (err) => {
      if (settled) return;
      settled = true;
      reject(new LockError(lockPath, undefined, String(err)));
    });

    child.once("exit", (code) => {
      if (settled) return;
      settled = true;
      const holderPid = readHolderPid(lockPath);
      reject(new LockError(lockPath, holderPid, stderr.trim() || `helper exited with code ${code}`));
    });
  });
}
