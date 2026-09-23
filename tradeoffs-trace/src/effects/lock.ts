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
  return new Promise((resolve, reject) => {
    const child = spawn(PERL_BIN, ["-e", HELPER_SCRIPT, lockPath], {
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
