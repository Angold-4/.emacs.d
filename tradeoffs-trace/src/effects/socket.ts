// The run socket server: `<run>/conductor.sock`, spoken by every agent's
// extension (extension/tradeoffs-trace.ts) and by fake-pi.ts in tests
// (design §2.1, §2.2, §9.1).
//
// Responsibilities:
//  - `hello`: the conductor refuses to dispatch a prompt to an agent until
//    its hello arrives and the tool set matches exactly (design §2.1) — a
//    mismatch is reported to the caller as a launch failure, never silently
//    tolerated.
//  - `submit`: forwarded to a handler that turns it into core events (or
//    rejects it), replied to with `submit_reply`.
//  - `sh`: run via effects/shell.ts in the agent's own worktree, honoring a
//    per-command deadline (design §8.1); the pgid is reported (and can be
//    logged as an intent) *before* the command's first side effect;
//    output is streamed back as `sh_output` and finished with `sh_exit`.
//
// macOS unix domain socket paths are limited to 104 bytes (`sun_path`), so
// `startRunSocketServer` fails fast with a clear message if the path is too
// long — long temp dirs under `/var/folders/...` are a common way to trip
// this in tests; use a short root such as `/tmp/tt-XXXX` instead.

import { createServer, type Server, type Socket } from "node:net";
import { existsSync, unlinkSync } from "node:fs";

import { JSONLDecoder, encodeLine, type HelloMessage, type RunSocketMessage, type ShMessage, type SubmitMessage } from "../core/protocol.ts";
import { childEnv, runCommand } from "./shell.ts";

const MAX_SOCKET_PATH_BYTES = 104;

export class SocketPathTooLongError extends Error {
  constructor(path: string) {
    super(
      `run socket path is ${Buffer.byteLength(path, "utf8")} bytes, over the macOS unix-socket limit of ${MAX_SOCKET_PATH_BYTES} — use a shorter run root (e.g. /tmp/tt-XXXX)`,
    );
    this.name = "SocketPathTooLongError";
  }
}

export interface SubmitResult {
  ok: boolean;
  reason?: string;
}

export interface HelloResult {
  ok: boolean;
  reason?: string;
  /** Set when `ok` is false because of a tool-set mismatch specifically
   * (design §2.1), so the caller can turn it into a LAUNCH_FAILED event with
   * the actual expected/missing/extra sets rather than re-parsing a message
   * string. Absent for any other hello failure (e.g. an unknown agent). */
  mismatch?: { expected: string[]; missing: string[]; extra: string[] };
}

export interface ShDeadlineOptions {
  deadlineMs?: number;
  termGraceMs?: number;
}

export interface RunSocketHandlers {
  /** Called when an agent's `hello` arrives. Returning `{ok: false}`
   * (e.g. a tool-set mismatch) is a launch failure the caller must record —
   * this function only reports the outcome, it never disconnects the
   * socket itself. */
  onHello: (agentId: string, hello: HelloMessage) => HelloResult | Promise<HelloResult>;
  /** Turns a validated submission into core events (or rejects it). */
  onSubmit: (agentId: string, msg: SubmitMessage) => SubmitResult | Promise<SubmitResult>;
  /** The directory an agent's `sh` commands run in (its worktree). */
  cwdFor: (agentId: string) => string | undefined;
  /** Called once the pgid is known, before the command's first side effect
   * (design §2.2) — this is where the caller logs the intent event. Must be
   * awaited by the caller before resuming (runCommand does this itself). */
  onShIntent?: (agentId: string, commandId: string, pgid: number) => void | Promise<void>;
  /** Called once the command's process group has finished (exit, timeout or
   * cancel), so the caller stops tracking the pgid. */
  onShExit?: (agentId: string, commandId: string, pgid: number) => void;
  /** Per-command deadline (design §8.1's "each `sh` command the worker
   * runs"). Defaults live here so tests can override with ms-scale values. */
  shDeadline?: ShDeadlineOptions;
  /** Plan 01a: refuse a command before it runs. The extension's own guard
   * (`guards.ts`) already refuses one containing a secret value, but a
   * scripted agent (fake-pi in tests) speaks this socket directly, so the
   * conductor refuses at the same place it runs every command. Returns the
   * reason to hand the agent, or `undefined` to run the command. Never echo
   * the command back in the reason — it holds the value. */
  refuseSh?: (agentId: string, command: string) => string | undefined;
  /** design §3.3 item 1 / §9.5: the worker's extension reports this once its
   * two settle reminders are exhausted with no accepted `submit_phase`. See
   * `NoSubmissionMessage` in core/protocol.ts for why this is a pure
   * addition. Optional so existing handler wiring keeps compiling. */
  onNoSubmission?: (agentId: string) => void | Promise<void>;
}

interface Connection {
  socket: Socket;
  decoder: JSONLDecoder;
  agentId?: string;
}

/** A live run socket server. One per run. */
export class RunSocketServer {
  #server: Server;
  #handlers: RunSocketHandlers;
  #connections = new Set<Connection>();
  #agentSockets = new Map<string, Socket>();

  private constructor(server: Server, handlers: RunSocketHandlers) {
    this.#server = server;
    this.#handlers = handlers;
  }

  static async start(path: string, handlers: RunSocketHandlers): Promise<RunSocketServer> {
    if (Buffer.byteLength(path, "utf8") > MAX_SOCKET_PATH_BYTES) {
      throw new SocketPathTooLongError(path);
    }
    // design §9.3's crash-safe restart: a conductor that crashed (design
    // §9.3's own fault injection — `TT_CRASH_AT`) never got to close this
    // socket, so its file is still on disk (unlike the *lock*, whose
    // flock-holding perl helper the OS itself releases on death, a unix
    // domain socket's path is a plain filesystem entry that outlives the
    // process that bound it). By the time a restarted conductor gets here
    // it has already taken the run's lock (`acquireLock`, ahead of this
    // call in `Conductor.start()`), which only one conductor can hold at a
    // time — so any leftover file at this exact path is provably stale,
    // never a live server, and safe to remove before rebinding.
    if (existsSync(path)) {
      try {
        unlinkSync(path);
      } catch {
        // Race with something else removing it, or a permissions oddity —
        // `server.listen` below will surface a real problem on its own.
      }
    }
    const server = createServer();
    const instance = new RunSocketServer(server, handlers);
    server.on("connection", (socket) => instance.#onConnection(socket));
    await new Promise<void>((resolve, reject) => {
      server.once("error", reject);
      server.listen(path, () => {
        server.removeListener("error", reject);
        resolve();
      });
    });
    return instance;
  }

  #onConnection(socket: Socket): void {
    const conn: Connection = { socket, decoder: new JSONLDecoder() };
    this.#connections.add(conn);
    socket.on("data", (chunk) => {
      const messages = conn.decoder.push(chunk) as RunSocketMessage[];
      for (const msg of messages) void this.#onMessage(conn, msg);
    });
    socket.on("close", () => {
      this.#connections.delete(conn);
      if (conn.agentId && this.#agentSockets.get(conn.agentId) === socket) {
        this.#agentSockets.delete(conn.agentId);
      }
    });
    socket.on("error", () => {
      // A peer that dies mid-write (e.g. SIGKILLed) surfaces here; the
      // 'close' handler above does the actual cleanup.
    });
  }

  async #onMessage(conn: Connection, msg: RunSocketMessage): Promise<void> {
    if (msg.type === "hello") {
      conn.agentId = msg.agentId;
      this.#agentSockets.set(msg.agentId, conn.socket);
      await this.#handlers.onHello(msg.agentId, msg);
      return;
    }
    if (msg.type === "submit") {
      const agentId = conn.agentId ?? "unknown";
      const result = await this.#handlers.onSubmit(agentId, msg);
      this.#write(conn.socket, { type: "submit_reply", id: msg.id, ok: result.ok, reason: result.reason });
      return;
    }
    if (msg.type === "sh") {
      await this.#runSh(conn, msg);
      return;
    }
    if (msg.type === "no_submission") {
      await this.#handlers.onNoSubmission?.(msg.agentId);
      return;
    }
    // sh_output/sh_exit/submit_reply are server->client only; ignore if a
    // peer ever echoes one back.
  }

  async #runSh(conn: Connection, msg: ShMessage): Promise<void> {
    const agentId = conn.agentId ?? "unknown";
    const refusal = this.#handlers.refuseSh?.(agentId, msg.command);
    if (refusal !== undefined) {
      // The agent gets a normal tool failure: no process group is created,
      // so there is nothing for the deadline or the sweep to clean up.
      this.#write(conn.socket, { type: "sh_output", commandId: msg.commandId, chunk: `${refusal}\n`, stream: "stderr" });
      this.#write(conn.socket, { type: "sh_exit", commandId: msg.commandId, code: 2, signal: null });
      return;
    }
    const cwd = this.#handlers.cwdFor(agentId);
    const deadline = this.#handlers.shDeadline ?? {};
    let groupId: number | undefined;
    const running = runCommand({
      command: msg.command,
      cwd: msg.cwd ?? cwd,
      // F13: a worker's `sh` command may itself run `node --test`; isolate
      // it from the test runner's own recursion markers exactly like checks
      // and the probe (see `childEnv`'s doc comment).
      env: childEnv(),
      deadlineMs: deadline.deadlineMs,
      termGraceMs: deadline.termGraceMs,
      onIntent: async ({ pgid }) => {
        groupId = pgid;
        await this.#handlers.onShIntent?.(agentId, msg.commandId, pgid);
      },
      onOutput: (chunk, stream) => {
        this.#write(conn.socket, { type: "sh_output", commandId: msg.commandId, chunk, stream });
      },
    });
    const result = await running.result;
    if (groupId !== undefined) this.#handlers.onShExit?.(agentId, msg.commandId, groupId);
    if (result.timedOut) {
      this.#write(conn.socket, {
        type: "sh_output",
        commandId: msg.commandId,
        chunk: shTimeoutNote(deadline.deadlineMs),
        stream: "stderr",
      });
    }
    this.#write(conn.socket, {
      type: "sh_exit",
      commandId: msg.commandId,
      code: result.exitCode,
      signal: result.signal,
    });
  }

  #write(socket: Socket, msg: RunSocketMessage): void {
    if (socket.destroyed) return;
    try {
      socket.write(encodeLine(msg));
    } catch {
      // peer gone
    }
  }

  /** Sends a message to a specific agent's connection, if it is still
   * connected (e.g. for pushing status; unused by phase 1's own tests but
   * kept for future use by cli.ts's `tt status --watch`). */
  sendTo(agentId: string, msg: RunSocketMessage): boolean {
    const socket = this.#agentSockets.get(agentId);
    if (!socket || socket.destroyed) return false;
    this.#write(socket, msg);
    return true;
  }

  async close(): Promise<void> {
    for (const conn of this.#connections) conn.socket.destroy();
    await new Promise<void>((resolve) => this.#server.close(() => resolve()));
  }
}

/** What an agent reads when its command hits the per-command limit. A bare
 * "timed out" made agents rerun the same command (run b46255dc: a test file
 * that could not finish inside the limit, piped through `tail`, returned no
 * output three times in a row), so say what to do instead. */
export function shTimeoutNote(deadlineMs: number | undefined): string {
  const limit = deadlineMs === undefined ? "the time limit" : `${Math.round(deadlineMs / 1000)} s`;
  return (
    `\n[tt: command killed after ${limit} (the per-command limit). The same command will be killed again. ` +
    "Narrow it (one test: --test-name-pattern) or find why it does not finish (a test waiting for an event that never comes). " +
    "Output piped through tail or head is lost when a command is killed.]\n"
  );
}
