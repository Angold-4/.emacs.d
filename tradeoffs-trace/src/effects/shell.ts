// The conductor owns every shell command (design §2.2). This module is the
// only place that actually spawns one on the conductor's behalf, whether
// for the worker's `sh` tool, a check, a reproduction or the integration
// probe's build commands.
//
// Requirement (design §2.2): "the conductor spawns the command in a new
// process group and records the pgid in an intent event *before* the
// command starts." A pgid is only knowable once the process exists, so
// "before the command starts" has to mean before the *command's* first
// side effect, not before the OS creates the process. The mechanism here:
//
//   /bin/sh -c 'kill -STOP $$; exec /bin/sh -c "$1"' tt-sh <command>
//
// spawned with `detached: true` (so it leads a new process group whose
// pgid equals its own pid). The outer shell stops itself with SIGSTOP
// before it ever execs the real command, so nothing the command does can
// happen until something sends SIGCONT. We poll `ps -o stat=` for the `T`
// (stopped) state, then call `onIntent({pgid})`, await it (so the intent
// event is fsynced to the log first), and only then send SIGCONT to the
// whole group. This was verified empirically (see the phase brief) against
// a command that touches a marker file: the marker does not exist while
// the process is in state T, and appears only after SIGCONT.
//
// Everything below operates on the *process group* (`kill(-pgid, sig)`),
// never a lone pid, because a command's own children inherit its pgid
// unless they deliberately escape it (§2.2's "assumes cooperative
// workers").

import { type ChildProcessWithoutNullStreams, execFileSync, spawn } from "node:child_process";

const DEFAULT_TERM_GRACE_MS = 10_000;
const STOP_POLL_INTERVAL_MS = 5;
const STOP_POLL_TIMEOUT_MS = 5_000;

/** The Node test runner sets two variables on the processes it launches —
 * `NODE_TEST_CONTEXT` and `NODE_TEST_WORKER_ID` — purely to coordinate its
 * own recursive file discovery. They are test-runner-only: they carry no
 * configuration any ordinary child process needs. When a check (or probe,
 * `sh`, reproduction) itself spawns `node --test`, inheriting them makes the
 * child's `node --test` believe it is already inside a test file and print
 * `node:test run() is being called recursively within a test file. skipping
 * running files.` and exit 0 — a real failing test is reported as a pass.
 *
 * `childEnv` removes exactly those two markers and nothing else: PATH, HOME,
 * NODE_OPTIONS, provider/credential variables and `TT_*` all survive, so
 * ordinary child configuration is untouched. One shared helper is used for
 * every command the conductor spawns (checks, probe and the worker `sh`
 * path), so all of them are isolated identically. */
export function childEnv(env: NodeJS.ProcessEnv = process.env): NodeJS.ProcessEnv {
  const copy: NodeJS.ProcessEnv = { ...env };
  delete copy.NODE_TEST_CONTEXT;
  delete copy.NODE_TEST_WORKER_ID;
  return copy;
}

export type EndReason = "exit" | "timeout" | "cancelled";

/** A3 (plan 06e): what a killed command's note reports — how much memory it
 * used (its peak resident set, in MiB) and how much the machine had left
 * (free memory in MiB) when it was killed. */
export interface ResourceSample {
  peakRssMB: number;
  freeMemMB: number;
}

/** A3: the sampler a kill note takes its numbers from. It is a plain function
 * of the command's pid, so a test replaces it with one that returns fixed
 * values. */
export type ResourceSampler = (pid: number) => { rssMB: number; freeMemMB: number };

/** The real sampler: the leader process's resident set right now (`ps -o
 * rss=`), and the machine's free pages (`vm_stat`). A failure on either side
 * is 0 rather than a thrown error — a kill note must never be what fails. */
export function realSampler(pid: number): { rssMB: number; freeMemMB: number } {
  let rssMB = 0;
  try {
    rssMB = Math.round(Number(execFileSync("ps", ["-o", "rss=", "-p", String(pid)], { encoding: "utf8" }).trim()) / 1024);
  } catch {
    rssMB = 0;
  }
  let freeMemMB = 0;
  try {
    const out = execFileSync("vm_stat", { encoding: "utf8" });
    const pages = out.match(/Pages free:\s+(\d+)/);
    const pageSize = Number((out.match(/page size of (\d+) bytes/) ?? [])[1] ?? 4096);
    if (pages) freeMemMB = Math.round((Number(pages[1]) * pageSize) / (1024 * 1024));
  } catch {
    freeMemMB = 0;
  }
  return { rssMB, freeMemMB };
}

/** How often the running command's memory is sampled while it runs. A peak
 * needs more than one reading; 1 s is cheap (one `ps`/`vm_stat` pair) and
 * still catches a command that grows over a multi-minute limit. The first
 * sample is taken at spawn, so a short deadline always has a reading. */
const SAMPLE_INTERVAL_MS = 1000;

export interface RunCommandOptions {
  command: string;
  cwd?: string;
  env?: NodeJS.ProcessEnv;
  /** Kill the group if the command has not finished after this many ms. */
  deadlineMs?: number;
  /** How long to wait after SIGTERM before escalating to SIGKILL, on
   * deadline expiry or `cancel()`. Default 10s (design §8.2); tests pass a
   * short value. */
  termGraceMs?: number;
  /** Called once, after the pgid is known but before the command's first
   * side effect, and awaited before SIGCONT is sent. This is where the
   * caller writes the intent event (design §2.2). */
  onIntent?: (info: { pgid: number }) => void | Promise<void>;
  onOutput?: (chunk: string, stream: "stdout" | "stderr") => void;
  /** A3: the memory sampler. Defaults to `realSampler`; a test passes fixed
   * values so a kill note is deterministic. */
  sampler?: ResourceSampler;
}

export interface RunCommandResult {
  exitCode: number | null;
  signal: NodeJS.Signals | null;
  timedOut: boolean;
  cancelled: boolean;
  output: string;
  signalsSent: string[];
  /** A3: the command's peak resident set and the machine's free memory, from
   * the sampler, when at least one sample was taken. Present on every
   * outcome; a kill note uses it when the command was killed at its limit. */
  resourceSample?: ResourceSample;
}

export interface RunningCommand {
  /** Resolves once the command (and, if escalation was needed, the group)
   * has fully exited. */
  result: Promise<RunCommandResult>;
  /** Terminates the command's whole group now (design §8.2): SIGTERM, wait
   * `termGraceMs`, then SIGKILL if it is still alive. Safe to call more
   * than once, and a no-op after the command has already finished. */
  cancel: () => void;
  /** Resolves with the pgid as soon as it is known — i.e. once the group
   * exists and is stopped, before SIGCONT. Mainly useful for tests. */
  pgid: Promise<number>;
}

function stat(pid: number): string | null {
  try {
    return execFileSync("ps", ["-o", "stat=", "-p", String(pid)], { encoding: "utf8" }).trim();
  } catch {
    // The process may not exist yet, or (rarely) may already have exited.
    return null;
  }
}

function isStopped(pid: number): boolean {
  const s = stat(pid);
  return s !== null && s.includes("T");
}

function waitUntilStopped(pid: number): Promise<void> {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const poll = () => {
      if (isStopped(pid)) {
        resolve();
        return;
      }
      if (Date.now() - start > STOP_POLL_TIMEOUT_MS) {
        reject(new Error(`command pid ${pid} never reached the stopped state within ${STOP_POLL_TIMEOUT_MS}ms`));
        return;
      }
      setTimeout(poll, STOP_POLL_INTERVAL_MS);
    };
    poll();
  });
}

function groupAlive(pgid: number): boolean {
  try {
    // Signal 0 sent to -pgid: any process in the group receiving it is
    // enough to prove the group is non-empty; ESRCH means it is gone.
    process.kill(-pgid, 0);
    return true;
  } catch {
    return false;
  }
}

/** Sends SIGTERM to the whole group, then SIGKILL after `termGraceMs` if
 * any member is still alive. Returns the signals actually sent. Exported
 * standalone for recovery: killing a pgid recorded in a previous
 * conductor's intent event, when there is no live `ChildProcess` handle
 * for it any more. */
export function killGroup(pgid: number, opts: { termGraceMs?: number } = {}): Promise<{ signalsSent: string[] }> {
  const termGraceMs = opts.termGraceMs ?? DEFAULT_TERM_GRACE_MS;
  return new Promise((resolve) => {
    const signalsSent: string[] = [];
    if (!groupAlive(pgid)) {
      resolve({ signalsSent });
      return;
    }
    try {
      process.kill(-pgid, "SIGTERM");
      signalsSent.push("SIGTERM");
    } catch {
      // Already gone.
      resolve({ signalsSent });
      return;
    }
    setTimeout(() => {
      if (groupAlive(pgid)) {
        try {
          process.kill(-pgid, "SIGKILL");
          signalsSent.push("SIGKILL");
        } catch {
          // Already gone.
        }
      }
      resolve({ signalsSent });
    }, termGraceMs);
  });
}

/** Spawns `options.command` in a fresh process group and drives it through
 * the stop/intent/continue handshake described at the top of this file.
 * See `RunCommandOptions`/`RunCommandResult`/`RunningCommand`. */
export function runCommand(options: RunCommandOptions): RunningCommand {
  const termGraceMs = options.termGraceMs ?? DEFAULT_TERM_GRACE_MS;
  let pgidResolve!: (pgid: number) => void;
  let pgidReject!: (err: Error) => void;
  const pgid = new Promise<number>((res, rej) => {
    pgidResolve = res;
    pgidReject = rej;
  });

  let cancelRequested = false;
  let timedOut = false;
  let deadlineTimer: NodeJS.Timeout | undefined;
  let termTimer: NodeJS.Timeout | undefined;
  const signalsSent: string[] = [];
  let output = "";
  let child: ChildProcessWithoutNullStreams | undefined;
  let resolvedPgid: number | undefined;
  const sampler = options.sampler ?? realSampler;
  let peakRssMB = 0;
  let freeMemMB = 0;
  let sampled = false;
  let sampleTimer: NodeJS.Timeout | undefined;

  function takeSample(): void {
    if (resolvedPgid === undefined) return;
    const s = sampler(resolvedPgid);
    if (!s) return;
    peakRssMB = Math.max(peakRssMB, s.rssMB);
    freeMemMB = s.freeMemMB;
    sampled = true;
  }

  function stopSampling(): void {
    if (sampleTimer !== undefined) {
      clearInterval(sampleTimer);
      sampleTimer = undefined;
    }
  }

  function terminate(reason: EndReason): void {
    if (reason === "timeout") timedOut = true;
    if (reason === "cancelled") cancelRequested = true;
    if (resolvedPgid === undefined || !groupAlive(resolvedPgid)) return;
    try {
      process.kill(-resolvedPgid, "SIGTERM");
      signalsSent.push("SIGTERM");
    } catch {
      return;
    }
    termTimer = setTimeout(() => {
      if (resolvedPgid !== undefined && groupAlive(resolvedPgid)) {
        try {
          process.kill(-resolvedPgid, "SIGKILL");
          signalsSent.push("SIGKILL");
        } catch {
          // Already gone.
        }
      }
    }, termGraceMs);
  }

  const result = new Promise<RunCommandResult>((resolveResult, rejectResult) => {
    child = spawn(
      "/bin/sh",
      ["-c", 'kill -STOP $$; exec /bin/sh -c "$1"', "tt-sh", options.command],
      {
        cwd: options.cwd,
        env: options.env,
        detached: true,
        stdio: ["ignore", "pipe", "pipe"],
      },
    );
    const c = child;
    // design §2.2: the pgid is known the moment the process exists —
    // `detached: true` makes it a process-group leader, so its pid IS the
    // pgid. Record it synchronously here (before the STOP handshake, and so
    // before the command's first side effect) so a concurrent `stop()` can
    // never miss a group that already exists. The old placement — inside
    // `waitUntilStopped` — left a window in which a just-spawned command's
    // group existed but was neither in a live handle's set nor in the log,
    // so `stop()` could leave it running (found when the force-kill-shell
    // SIGKILLed-worker test waited out its own `sleep 300`).
    if (c.pid !== undefined) resolvedPgid = c.pid;
    // A3: sample immediately (a short deadline may expire before the first
    // interval tick) and then while the command runs, so the note reports a
    // peak rather than a single reading.
    takeSample();
    sampleTimer = setInterval(takeSample, SAMPLE_INTERVAL_MS);
    sampleTimer.unref?.();
    const intentRecorded =
      c.pid !== undefined ? Promise.resolve(options.onIntent?.({ pgid: c.pid })) : Promise.resolve();

    c.stdout.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      output += text;
      options.onOutput?.(text, "stdout");
    });
    c.stderr.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      output += text;
      options.onOutput?.(text, "stderr");
    });

    c.once("error", (err) => {
      pgidReject(err);
      rejectResult(err);
    });

    waitUntilStopped(c.pid!)
      .then(async () => {
        resolvedPgid = c.pid!;
        pgidResolve(resolvedPgid);
        // The intent was recorded at spawn; await it here so it is durable
        // before SIGCONT lets the command run.
        await intentRecorded;
        if (cancelRequested) {
          // cancel() ran while we were waiting on onIntent; terminate()
          // already tried and found nothing alive yet because the group
          // was still stopped (SIGTERM/SIGKILL work fine on a stopped
          // process too, but re-run it now that the pgid is set).
          terminate("cancelled");
          return;
        }
        if (options.deadlineMs !== undefined) {
          deadlineTimer = setTimeout(() => terminate("timeout"), options.deadlineMs);
        }
        try {
          process.kill(-resolvedPgid, "SIGCONT");
        } catch (err) {
          rejectResult(err as Error);
        }
      })
      .catch((err) => {
        rejectResult(err);
      });

    c.once("exit", (code, signal) => {
      if (deadlineTimer) clearTimeout(deadlineTimer);
      if (termTimer) clearTimeout(termTimer);
      stopSampling();
      resolveResult({
        exitCode: code,
        signal: signal as NodeJS.Signals | null,
        timedOut,
        cancelled: cancelRequested,
        output,
        signalsSent,
        ...(sampled ? { resourceSample: { peakRssMB, freeMemMB } } : {}),
      });
    });
  });

  return {
    result,
    pgid,
    cancel: () => terminate("cancelled"),
  };
}
