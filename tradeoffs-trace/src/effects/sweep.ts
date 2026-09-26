// Sweep: cleanup, not containment (design §2.2).
//
// `runCommand`/`killGroup` (shell.ts) kill everything in a command's
// recorded process group, but a detached descendant that calls `setsid` (or
// otherwise leaves the group) escapes that. The sweep is the fallback: list
// every process with a file under the worktree (`lsof +D <dir>`), kill the
// ones whose current WORKING DIRECTORY lies under it (a daemon the attempt
// started there), and report the rest without killing them.
//
// Why not kill every process with an open file there: a live gate runs a
// Docker stack that bind-mounts worktree directories, so Docker Desktop's own
// file-sharing processes (com.docker.backend, the Virtualization XPC service)
// hold files under the worktree. Program 14's 14i freezes killed Docker
// Desktop that way (sweep records at 18:06:53 and 18:48:15 UTC on
// 2026-09-26), five times in one day. A process that only holds a file is
// reported as `held`; system and application processes are never signalled.
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
  /** Processes with a file open under the worktree but their working
   * directory elsewhere (a bind mount's file server, an editor), or a
   * protected system/application process: reported, never signalled, and
   * not a reason to taint. */
  held?: SweepKilled[];
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

/** Executables the sweep never signals, whatever `lsof` reports: the OS and
 * installed applications (Docker Desktop and its VM helpers live here). */
const PROTECTED_PREFIXES = ["/System/", "/Applications/", "/Library/", "/usr/libexec/", "/usr/sbin/"];

export function isProtectedCommand(command: string): boolean {
  return PROTECTED_PREFIXES.some((p) => command.startsWith(p));
}

/** Every pid with a file under `dir`, and whether that file is its cwd. */
function lsofUnder(dir: string): Map<number, { cwd: boolean }> {
  let out: string;
  try {
    out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "pf"], { encoding: "utf8" });
  } catch (err) {
    // lsof exits 1 (and prints nothing) when nothing matches. Any stdout
    // captured on the error object is still authoritative; anything else
    // is a real failure to run lsof at all.
    const stdout = (err as { stdout?: string }).stdout ?? "";
    if (stdout.length === 0) return new Map();
    out = stdout;
  }
  // `-F pf`: a `p<pid>` line starts each process, followed by one `f<fd>`
  // line per file it has under `dir`; `fcwd` is its working directory.
  const found = new Map<number, { cwd: boolean }>();
  let current: { cwd: boolean } | undefined;
  for (const line of out.split("\n")) {
    if (line.startsWith("p")) {
      const pid = Number.parseInt(line.slice(1), 10);
      current = Number.isFinite(pid) ? { cwd: false } : undefined;
      if (current) found.set(pid, current);
    } else if (line === "fcwd" && current) {
      current.cwd = true;
    }
  }
  return found;
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
  const killed: SweepKilled[] = [];
  const held: SweepKilled[] = [];
  for (const [pid, { cwd }] of lsofUnder(dir)) {
    if (excluded.has(pid)) continue;
    const command = commandFor(pid);
    if (!cwd || isProtectedCommand(command)) {
      held.push({ pid, command });
      continue;
    }
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

  return { killed, held, tainted: killed.length > 0 };
}
