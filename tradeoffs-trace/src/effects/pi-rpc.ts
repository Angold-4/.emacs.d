// Spawns one Pi agent (a real `pi --mode rpc` process, or an injected
// command such as `node test/fake-pi/fake-pi.ts` for tests) in its own
// process group, speaks the strict-JSONL RPC protocol from
// src/core/protocol.ts on its stdin/stdout, and forwards every event both to
// a listener and to a per-agent file under `<run>/stream/` (design §9.4:
// "the conductor forwards each agent's RPC events over conductor.sock, and
// ... appended here without fsync").
//
// Termination follows design §8.2 exactly: RPC `abort`, wait (default 30s),
// `SIGTERM` the process group, wait (default 10s), `SIGKILL`. Every interval
// is a constructor option so tests can use ms-scale values.

import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomUUID } from "node:crypto";

import { JSONLDecoder, encodeLine, type PiRpcCommand, type PiRpcEvent, type PiRpcResponse } from "../core/protocol.ts";
import type { Role } from "../core/roles.ts";

export interface PiAgentOptions {
  /** The `pi` binary, or an injected stand-in (fake-pi) for tests. Defaults
   * to `"pi"`. */
  command?: string;
  /** Full argv (after the command), e.g. from `launchArgs(role, ...)`. */
  args: string[];
  cwd?: string;
  env?: NodeJS.ProcessEnv;
  role: Role;
  agentId: string;
  /** Absolute path of the file every RPC event is appended to (no fsync —
   * design §9.2's "high-volume" stream). Parent directory must exist. */
  streamFile?: string;
  /** design §8.2 defaults; overridable for tests. */
  abortGraceMs?: number;
  termGraceMs?: number;
  onEvent?: (event: PiRpcEvent) => void;
}

const DEFAULT_ABORT_GRACE_MS = 30_000;
const DEFAULT_TERM_GRACE_MS = 10_000;

export type TerminateReason = "abort" | "sigterm" | "sigkill" | "already-exited";

export interface TerminateResult {
  signalsSent: TerminateReason[];
}

function groupAlive(pgid: number): boolean {
  try {
    process.kill(-pgid, 0);
    return true;
  } catch {
    return false;
  }
}

/** A live Pi agent process, driven over RPC. */
export class PiAgent {
  #child: ChildProcessWithoutNullStreams;
  #decoder = new JSONLDecoder();
  #pending = new Map<string, { resolve: (r: PiRpcResponse) => void; reject: (e: Error) => void }>();
  #streamFd: number | undefined;
  #onEvent: ((event: PiRpcEvent) => void) | undefined;
  #settled: Promise<void>;
  #resolveSettled!: () => void;
  #exitPromise: Promise<{ code: number | null; signal: NodeJS.Signals | null }>;
  #exited = false;
  #exitInfo: { code: number | null; signal: NodeJS.Signals | null } | undefined;
  #abortGraceMs: number;
  #termGraceMs: number;
  #terminating: Promise<TerminateResult> | undefined;
  readonly pgid: number;
  readonly agentId: string;
  readonly role: Role;

  constructor(opts: PiAgentOptions) {
    this.agentId = opts.agentId;
    this.role = opts.role;
    this.#abortGraceMs = opts.abortGraceMs ?? DEFAULT_ABORT_GRACE_MS;
    this.#termGraceMs = opts.termGraceMs ?? DEFAULT_TERM_GRACE_MS;
    this.#onEvent = opts.onEvent;

    if (opts.streamFile) {
      fs.mkdirSync(path.dirname(opts.streamFile), { recursive: true });
      this.#streamFd = fs.openSync(opts.streamFile, "a");
    }

    const child = spawn(opts.command ?? "pi", opts.args, {
      cwd: opts.cwd,
      env: { ...process.env, ...opts.env, TT_AGENT_ID: opts.agentId, TT_ROLE: opts.role },
      detached: true,
      stdio: ["pipe", "pipe", "pipe"],
    });
    this.#child = child;
    if (!child.pid) throw new Error("failed to spawn pi agent: no pid");
    this.pgid = child.pid;

    this.#settled = new Promise((resolve) => {
      this.#resolveSettled = resolve;
    });

    child.stdout.on("data", (chunk: Buffer) => {
      const events = this.#decoder.push(chunk) as Array<PiRpcEvent | PiRpcResponse>;
      for (const ev of events) this.#onMessage(ev);
    });
    // Forwarded to the conductor's own stderr only under TT_DEBUG=1 — a
    // debugging aid for tests, not part of the run's recorded state.
    child.stderr.on("data", (chunk: Buffer) => {
      if (process.env.TT_DEBUG) process.stderr.write(`[${opts.agentId} stderr] ${chunk.toString()}`);
    });

    this.#exitPromise = new Promise((resolve) => {
      child.once("exit", (code, signal) => {
        this.#exited = true;
        this.#exitInfo = { code, signal };
        if (this.#streamFd !== undefined) {
          try {
            fs.closeSync(this.#streamFd);
          } catch {
            // already closed
          }
          this.#streamFd = undefined;
        }
        resolve(this.#exitInfo);
        // A dead process cannot settle any further — unblock anyone waiting.
        this.#resolveSettled();
      });
    });
  }

  #onMessage(msg: PiRpcEvent | PiRpcResponse): void {
    if ((msg as PiRpcResponse).type === "response") {
      const resp = msg as PiRpcResponse;
      const id = resp.id;
      if (id !== undefined) {
        const pending = this.#pending.get(id);
        if (pending) {
          this.#pending.delete(id);
          pending.resolve(resp);
        }
      }
      return;
    }
    const event = msg as PiRpcEvent;
    if (this.#streamFd !== undefined) {
      try {
        fs.writeSync(this.#streamFd, encodeLine({ agentId: this.agentId, ts: new Date().toISOString(), event }));
      } catch {
        // best effort: losing the stream loses display detail, never state.
      }
    }
    this.#onEvent?.(event);
    if (event.type === "agent_settled") this.#resolveSettled();
  }

  get exited(): boolean {
    return this.#exited;
  }

  get exitInfo(): { code: number | null; signal: NodeJS.Signals | null } | undefined {
    return this.#exitInfo;
  }

  /** Resolves once the process has exited (any reason). */
  waitExit(): Promise<{ code: number | null; signal: NodeJS.Signals | null }> {
    return this.#exitPromise;
  }

  /** Resolves the first time `agent_settled` arrives, or immediately if the
   * process has already exited (nothing further will ever settle). */
  waitSettled(): Promise<void> {
    return this.#settled;
  }

  #send(command: PiRpcCommand): Promise<PiRpcResponse> {
    const id = command.id ?? randomUUID();
    const withId = { ...command, id };
    return new Promise((resolve, reject) => {
      if (this.#exited) {
        reject(new Error(`cannot send '${command.type}': agent ${this.agentId} has already exited`));
        return;
      }
      this.#pending.set(id, { resolve, reject });
      try {
        this.#child.stdin.write(encodeLine(withId));
      } catch (err) {
        this.#pending.delete(id);
        reject(err as Error);
      }
    });
  }

  prompt(message: string): Promise<PiRpcResponse> {
    return this.#send({ type: "prompt", message });
  }

  steer(message: string): Promise<PiRpcResponse> {
    return this.#send({ type: "steer", message });
  }

  abort(): Promise<PiRpcResponse> {
    return this.#send({ type: "abort" });
  }

  getState(): Promise<PiRpcResponse> {
    return this.#send({ type: "get_state" });
  }

  /** design §8.2: RPC abort, wait `abortGraceMs`, SIGTERM the process
   * group, wait `termGraceMs`, SIGKILL. Resolves once the process has
   * actually exited. Safe to call more than once — concurrently or not —
   * and a no-op once exited: every caller (the attempt/review dispatch that
   * owns this agent, and `Conductor.stop()`, which may legitimately race
   * with it while tearing down) gets the SAME in-flight sequence rather
   * than each independently sending its own `abort`/SIGTERM/SIGKILL, which
   * was harmless in effect but left two overlapping timers each racing the
   * same `#exitPromise` — exactly the kind of leftover handle round-of-
   * review item 1 asks not to have outlive a terminal state. */
  terminate(opts?: { abortGraceMs?: number; termGraceMs?: number }): Promise<TerminateResult> {
    if (!this.#terminating) {
      this.#terminating = this.#doTerminate(opts?.abortGraceMs ?? this.#abortGraceMs, opts?.termGraceMs ?? this.#termGraceMs);
    }
    return this.#terminating;
  }

  async #doTerminate(abortGraceMs: number, termGraceMs: number): Promise<TerminateResult> {
    const signalsSent: TerminateReason[] = [];
    if (this.#exited) return { signalsSent: ["already-exited"] };

    try {
      await this.abort();
      signalsSent.push("abort");
    } catch {
      // Agent may already be unreachable (e.g. killed out from under us) —
      // proceed straight to signals.
    }
    // Plan 2c: an RPC-mode Pi does not exit after `abort` — it waits for the
    // next command — so without this every termination (each freeze, each
    // finished reviewer) waited out the whole abort grace before SIGTERM
    // (32 of 85 s in a live run). Closing stdin ends the RPC session and Pi
    // exits cleanly; the grace below still bounds an agent that does not.
    try {
      this.#child.stdin.end();
    } catch {
      // already closed
    }

    const exited = await raceExit(this.#exitPromise, abortGraceMs);
    if (exited || this.#exited) return { signalsSent };

    if (groupAlive(this.pgid)) {
      try {
        process.kill(-this.pgid, "SIGTERM");
        signalsSent.push("sigterm");
      } catch {
        // already gone
      }
    }
    const exitedAfterTerm = await raceExit(this.#exitPromise, termGraceMs);
    if (exitedAfterTerm || this.#exited) return { signalsSent };

    if (groupAlive(this.pgid)) {
      try {
        process.kill(-this.pgid, "SIGKILL");
        signalsSent.push("sigkill");
      } catch {
        // already gone
      }
    }
    await this.#exitPromise;
    return { signalsSent };
  }
}

function raceExit(exitPromise: Promise<unknown>, ms: number): Promise<boolean> {
  return new Promise((resolve) => {
    let done = false;
    const timer = setTimeout(() => {
      if (!done) {
        done = true;
        resolve(false);
      }
    }, ms);
    exitPromise.then(() => {
      if (!done) {
        done = true;
        clearTimeout(timer);
        resolve(true);
      }
    });
  });
}

/** Spawns a Pi agent and waits for it to be ready to receive a prompt. Pi's
 * own RPC mode has no explicit "ready" handshake beyond accepting stdin, so
 * this simply constructs the `PiAgent`; callers send `hello` readiness via
 * the run socket (socket.ts) before dispatching a prompt. */
export function spawnPiAgent(opts: PiAgentOptions): PiAgent {
  return new PiAgent(opts);
}
