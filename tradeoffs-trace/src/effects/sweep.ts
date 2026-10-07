// Sweep: cleanup, not containment (design §2.2).
//
// `runCommand`/`killGroup` (shell.ts) kill everything in a command's
// recorded process group, but a detached descendant that calls `setsid` (or
// otherwise leaves the group) escapes that. The sweep is the fallback: list
// every process with a file under the worktree (`lsof +D <dir>`), and end the
// ones that belong to a process group THIS RUN recorded at spawn.
//
// A2 (plan 06e): the sweep is the only code that signals a process, and it
// signals only the run's own process groups, recorded at spawn. Any other
// process with a file under the worktree — a process that left its group, a
// bind mount's file server, an editor, Docker Desktop's own helpers — is
// reported as `HELD {pid, command, cwd}` and never signalled. Safety wins
// over cleanup: a foreign process is never killed, even when it looks like a
// survivor. (Program 14's 14i freezes killed Docker Desktop because an
// earlier sweep killed every process with a cwd under the worktree.)
//
// IMPORTANT (documented per the brief): an empty sweep does not prove no
// process survived. A detached process can `chdir` away, close every open
// file under the worktree, and reopen a path under it again later — `lsof
// +D` only sees what is true *at the moment it runs*. Sweeping is run
// after any cancellation and before freezing a candidate (design §2.2), not
// as a one-shot guarantee.

import { execFileSync } from "node:child_process";
import * as path from "node:path";

import { readLog } from "./log.ts";

// ---------------------------------------------------------------------------
// The one owner of the signalling decision (A2/C3)
// ---------------------------------------------------------------------------
//
// Every signal the run sends — a `sh` command's group on timeout, an agent
// group on termination, a pgid read from a dead conductor's log on crash
// recovery, the conductor/scheduler pid an owner stops — goes through a
// function here. No other module calls `process.kill` with a signal, so there
// is exactly one place to read (and change) what the run is allowed to signal.
// The sweep itself is the only code that signals a process it *discovered*,
// and it signals one only when its group is in `ownPgids`.

export type SignalName = NodeJS.Signals | 0;

/** True when PID exists (signal 0). */
export function processAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

/** Sends SIGNAL to one process the caller already owns (a conductor pid, a
 * scheduler pid, a probe child). Returns whether it was delivered. */
export function signalProcess(pid: number, signal: SignalName): boolean {
  try {
    process.kill(pid, signal);
    return true;
  } catch {
    return false;
  }
}

/** Sends SIGNAL to a whole process group (the run's own, recorded at spawn).
 * Returns whether it was delivered. */
export function signalGroup(pgid: number, signal: SignalName): boolean {
  try {
    process.kill(-pgid, signal);
    return true;
  } catch {
    return false;
  }
}

/** True when any process in PGID's group still exists (signal 0). */
export function groupAlive(pgid: number): boolean {
  return signalGroup(pgid, 0);
}

/** SIGTERM to PGID's whole group, then SIGKILL after `termGraceMs` if it is
 * still alive. The one escalation path; returns the signals actually sent. */
export function killGroup(pgid: number, opts: { termGraceMs?: number } = {}): Promise<{ signalsSent: string[] }> {
  const termGraceMs = opts.termGraceMs ?? 10_000;
  return new Promise((resolve) => {
    const signalsSent: string[] = [];
    if (!groupAlive(pgid)) {
      resolve({ signalsSent });
      return;
    }
    if (!signalGroup(pgid, "SIGTERM")) {
      resolve({ signalsSent });
      return;
    }
    signalsSent.push("SIGTERM");
    setTimeout(() => {
      if (groupAlive(pgid) && signalGroup(pgid, "SIGKILL")) signalsSent.push("SIGKILL");
      resolve({ signalsSent });
    }, termGraceMs);
  });
}

/** True when PGID's leader exists and started no later than `atMs` (plus a
 * slack): a pgid whose leader started AFTER the run recorded it has been
 * recycled by an unrelated process. Used before signalling a group read from
 * a previous conductor's log (crash recovery), so a recycled pgid is never
 * signalled. A group with no leader has nothing to signal, so a failed `ps`
 * is reported as true (the caller's own liveness check decides). */
export function groupStartedBefore(pgid: number, atMs: number, slackMs = 10_000): boolean {
  let startMs: number;
  try {
    startMs = Date.parse(execFileSync("ps", ["-o", "lstart=", "-p", String(pgid)], { encoding: "utf8" }).trim());
  } catch {
    return true;
  }
  if (!Number.isFinite(startMs)) return true;
  return startMs <= atMs + slackMs;
}

export interface SweepKilled {
  pid: number;
  command: string;
}

/** A process with a file under the worktree that the run does NOT own: it is
 * reported, never signalled (A2). `cwd` is where it is running, so the owner
 * can tell an agent's escaped daemon from an unrelated file server. */
export interface SweepHeld extends SweepKilled {
  cwd: string;
}

export interface SweepResult {
  /** The run's own processes the sweep ended. */
  killed: SweepKilled[];
  /** Foreign processes with a file under the worktree: reported, never
   * signalled, and not a reason to taint. */
  held?: SweepHeld[];
  /** True iff the sweep ended one of the run's own processes — design §2.2:
   * "A sweep that found survivors marks the worktree tainted." A `held`
   * foreign process does NOT taint the worktree. */
  tainted: boolean;
}

export interface SweepOptions {
  /** The process groups THIS RUN recorded at spawn (the pgids of its intent
   * records, its live agents and its live shell commands). A2: the sweep
   * signals a process only when its group is in this set. Omitted means the
   * run owns nothing here, so every process found is `held` — never killed. */
  ownPgids?: number[];
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

/** Every pid with a file under `dir`. A2 does not care whether the file is
 * the process's cwd: the deciding fact is its process group. */
function lsofUnder(dir: string): Set<number> {
  let out: string;
  try {
    out = execFileSync("/usr/sbin/lsof", ["+D", dir, "-F", "p"], { encoding: "utf8" });
  } catch (err) {
    // lsof exits 1 (and prints nothing) when nothing matches. Any stdout
    // captured on the error object is still authoritative; anything else
    // is a real failure to run lsof at all.
    const stdout = (err as { stdout?: string }).stdout ?? "";
    if (stdout.length === 0) return new Set();
    out = stdout;
  }
  const found = new Set<number>();
  for (const line of out.split("\n")) {
    if (!line.startsWith("p")) continue;
    const pid = Number.parseInt(line.slice(1), 10);
    if (Number.isFinite(pid)) found.add(pid);
  }
  return found;
}

/** Where PID is running. `lsof +D` does not name a cwd outside the swept
 * tree, so a `HELD` process's cwd is read with a second, targeted query.
 * `<unknown>` when the process is already gone or `lsof` cannot say. */
function cwdFor(pid: number): string {
  try {
    const out = execFileSync("/usr/sbin/lsof", ["-a", "-p", String(pid), "-d", "cwd", "-F", "n"], { encoding: "utf8" });
    const named = out.split("\n").find((line) => line.startsWith("n"));
    return named ? named.slice(1) : "<unknown>";
  } catch {
    return "<unknown>";
  }
}

function commandFor(pid: number): string {
  try {
    return execFileSync("ps", ["-o", "comm=", "-p", String(pid)], { encoding: "utf8" }).trim();
  } catch {
    return "<unknown>";
  }
}

/** The process group PID belongs to, or undefined when `ps` cannot say (a pid
 * that already exited between the `lsof` snapshot and now). */
function pgidFor(pid: number): number | undefined {
  try {
    const pgid = Number.parseInt(execFileSync("ps", ["-o", "pgid=", "-p", String(pid)], { encoding: "utf8" }).trim(), 10);
    return Number.isFinite(pgid) ? pgid : undefined;
  } catch {
    return undefined;
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

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/** Lists, kills and reports every process found under `dir` by `lsof +D`,
 * excluding the conductor's own process/ancestors and `options.exceptPids`.
 * A process is signalled ONLY when its process group is in
 * `options.ownPgids` (A2); every other process is returned under `held` and
 * left alone. The run's own survivors get SIGTERM, then SIGKILL after
 * `options.termGraceMs` (default 500ms) if still alive. See the module
 * comment for what an empty result does and does not prove. */
export async function sweep(dir: string, options: SweepOptions = {}): Promise<SweepResult> {
  const termGraceMs = options.termGraceMs ?? 500;
  const own = new Set<number>(options.ownPgids ?? []);
  const excluded = new Set<number>([...(options.exceptPids ?? []), ...selfAndAncestors()]);
  const killed: SweepKilled[] = [];
  const held: SweepHeld[] = [];
  for (const pid of lsofUnder(dir)) {
    if (excluded.has(pid)) continue;
    const command = commandFor(pid);
    const pgid = pgidFor(pid);
    // A2: only the run's own recorded process groups are signalled. A
    // protected command is reported even when its group is ours.
    if (pgid === undefined || !own.has(pgid) || isProtectedCommand(command)) {
      held.push({ pid, command, cwd: cwdFor(pid) });
      continue;
    }
    if (signalProcess(pid, "SIGTERM")) killed.push({ pid, command });
    // else: already gone between the lsof snapshot and now — not a survivor.
  }

  if (killed.length > 0) {
    await delay(termGraceMs);
    for (const { pid } of killed) {
      if (processAlive(pid)) signalProcess(pid, "SIGKILL");
    }
  }

  return { killed, held, tainted: killed.length > 0 };
}

/** The `held` processes a run last recorded (`tt status` lists them). Read
 * from the newest `sweep` record in the control log — the sweep's own result,
 * never re-derived. Names only; the pid, command and cwd are the process's,
 * not a secret. */
export function loggedHeld(runDir: string): SweepHeld[] {
  let records: Array<{ kind: string; event: unknown }>;
  try {
    records = readLog(path.join(runDir, "events.jsonl")).records;
  } catch {
    return [];
  }
  let held: SweepHeld[] = [];
  for (const record of records) {
    if (record.kind !== "sweep") continue;
    const raw = (record.event as { held?: unknown })?.held;
    if (!Array.isArray(raw)) {
      held = [];
      continue;
    }
    held = raw
      .filter((h): h is { pid: number; command?: unknown; cwd?: unknown } => !!h && typeof h === "object" && typeof (h as { pid?: unknown }).pid === "number")
      .map((h) => ({ pid: h.pid, command: typeof h.command === "string" ? h.command : "<unknown>", cwd: typeof h.cwd === "string" ? h.cwd : "<unknown>" }));
  }
  return held;
}
