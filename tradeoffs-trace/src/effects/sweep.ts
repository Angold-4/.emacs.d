// Sweep: cleanup, not containment (design §2.2).
//
// `runCommand`/`killGroup` (shell.ts) kill everything in a command's
// recorded process group, but a detached descendant that calls `setsid` (or
// otherwise leaves the group) escapes that. The sweep is the fallback: list
// every process whose current working directory or an open file lies under
// the worktree (`lsof +D <dir>`), kill what is found, and report it.
//
// IMPORTANT (documented per the brief): an empty sweep does not prove no
// process survived. A detached process can `chdir` away, close every open
// file under the worktree, and reopen a path under it again later — `lsof
// +D` only sees what is true *at the moment it runs*. Sweeping is run
// after any cancellation and before freezing a candidate (design §2.2), not
// as a one-shot guarantee.

import { execFileSync } from "node:child_process";

export interface SweepKilled {
  pid: number;
  command: string;
}

export interface SweepResult {
  killed: SweepKilled[];
  /** True iff any survivor was found — design §2.2: "A sweep that found
   * survivors marks the worktree tainted." */
  tainted: boolean;
}

export interface SweepOptions {
  /** Pids never to touch even if `lsof` reports them under `dir` (e.g. a
   * command the conductor is intentionally still running there). */
  exceptPids?: number[];
  /** Grace period between SIGTERM and SIGKILL for survivors. Default 500ms
   * (short: a sweep target is, by definition, something that already
   * escaped normal control, so there is no reason to give it design §8.2's
   * full 10s). */
  termGraceMs?: number;
}

function lsofPidsUnder(dir: string): number[] {
  let out: string;
  try {
    out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "p"], { encoding: "utf8" });
  } catch (err) {
    // lsof exits 1 (and prints nothing) when nothing matches. Any stdout
    // captured on the error object is still authoritative; anything else
    // is a real failure to run lsof at all.
    const stdout = (err as { stdout?: string }).stdout ?? "";
    if (stdout.length === 0) return [];
    out = stdout;
  }
  const pids: number[] = [];
  for (const line of out.split("\n")) {
    // `-F p` prefixes a pid line with the literal `p`; other lines (`f...`
    // for each open file/fd) are not process identifiers.
    if (line.startsWith("p")) {
      const pid = Number.parseInt(line.slice(1), 10);
      if (Number.isFinite(pid)) pids.push(pid);
    }
  }
  return pids;
}

function commandFor(pid: number): string {
  try {
    return execFileSync("ps", ["-o", "comm=", "-p", String(pid)], { encoding: "utf8" }).trim();
  } catch {
    return "<unknown>";
  }
}

/** Own pid plus every ancestor up to (but not including) pid 1, so a
 * sweep never signals the conductor itself or anything above it — `lsof
 * +D` can legitimately report the conductor's own process if its cwd is
 * under the worktree. */
function selfAndAncestors(): Set<number> {
  const pids = new Set<number>();
  let pid = process.pid;
  while (pid > 1 && !pids.has(pid)) {
    pids.add(pid);
    try {
      const out = execFileSync("ps", ["-o", "ppid=", "-p", String(pid)], { encoding: "utf8" }).trim();
      const ppid = Number.parseInt(out, 10);
      if (!Number.isFinite(ppid) || ppid <= 0) break;
      pid = ppid;
    } catch {
      break;
    }
  }
  return pids;
}

function processAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/** Lists, kills and reports every process found under `dir` by `lsof +D`,
 * excluding the conductor's own process/ancestors and `options.exceptPids`.
 * Survivors get SIGTERM, then SIGKILL after `options.termGraceMs` (default
 * 500ms) if still alive. See the module comment for what an empty result
 * does and does not prove. */
export async function sweep(dir: string, options: SweepOptions = {}): Promise<SweepResult> {
  const termGraceMs = options.termGraceMs ?? 500;
  const excluded = new Set<number>([...(options.exceptPids ?? []), ...selfAndAncestors()]);
  const candidates = lsofPidsUnder(dir).filter((pid) => !excluded.has(pid));

  const killed: SweepKilled[] = [];
  for (const pid of candidates) {
    const command = commandFor(pid);
    try {
      process.kill(pid, "SIGTERM");
      killed.push({ pid, command });
    } catch {
      // Already gone between the lsof snapshot and now — not a survivor.
    }
  }

  if (killed.length > 0) {
    await delay(termGraceMs);
    for (const { pid } of killed) {
      if (processAlive(pid)) {
        try {
          process.kill(pid, "SIGKILL");
        } catch {
          // Already gone.
        }
      }
    }
  }

  return { killed, tainted: killed.length > 0 };
}
