// fake-pi: an executable (`node test/fake-pi/fake-pi.ts`) that speaks Pi's
// RPC protocol on stdin/stdout well enough for tradeoffs-trace's own tests,
// and replays a JSON script from `FAKE_PI_SCRIPT`. It also drives the SAME
// run-socket protocol (src/core/protocol.ts) the real extension uses, so a
// scripted `submit`/`sh` step exercises exactly what a conductor's socket
// server will see from a real Pi + the tradeoffs-trace extension.
//
// This file has no dependency on the real `pi` binary or on
// `@earendil-works/pi-coding-agent` — it only implements the wire protocol
// described in docs/rpc.md, to the extent tradeoffs-trace's own tests need.
//
// Script shape (see also test/fake-pi/fake-pi.test.ts for worked examples):
//
//   {
//     "hello": { "role": "worker", "tools": ["read", ...], "agentId": "w1" },
//     "steps": [
//       { "kind": "emit", "event": { "type": "agent_start" } },
//       { "kind": "call-submit", "tool": "submit_phase", "args": { ... } },
//       { "kind": "call-sh", "command": "echo hi" },
//       { "kind": "sleep", "ms": 10 },
//       { "kind": "hang-until-abort" },
//       { "kind": "hang-forever" },
//       { "kind": "crash", "code": 7 }
//     ]
//   }
//
// `hello` is sent once at startup (mirroring `session_start`), if
// `TT_SOCKET` is set. Steps run once, in order, after the first `prompt`
// command is received; unless a step hangs or crashes, the run finishes
// with an automatic `agent_end` + `agent_settled`.

import { createConnection, type Socket } from "node:net";
import { readFileSync } from "node:fs";
import { randomUUID } from "node:crypto";

import { JSONLDecoder, encodeLine, type RunSocketMessage } from "../../src/core/protocol.ts";

interface HelloSpec {
  role: "worker" | "reviewer";
  tools: string[];
  agentId?: string;
  piVersion?: string;
}

type Step =
  | { kind: "emit"; event: Record<string, unknown> }
  | { kind: "call-submit"; tool: "submit_phase" | "submit_discovery" | "submit_review"; args: unknown }
  | { kind: "call-sh"; command: string; cwd?: string }
  | { kind: "sleep"; ms: number }
  | { kind: "hang-until-abort" }
  | { kind: "hang-forever" }
  | { kind: "crash"; code: number };

interface Script {
  hello?: HelloSpec;
  steps: Step[];
}

function readEnv(name: string): string | undefined {
  const v = process.env[name];
  return v && v.length > 0 ? v : undefined;
}

function loadScript(): Script {
  const path = readEnv("FAKE_PI_SCRIPT");
  if (!path) throw new Error("FAKE_PI_SCRIPT is required");
  return JSON.parse(readFileSync(path, "utf8")) as Script;
}

function writeStdout(obj: unknown): void {
  process.stdout.write(encodeLine(obj));
}

// --- run-socket client (same protocol module the real extension uses) ---

class RunSocket {
  #socket: Socket | undefined;
  #decoder = new JSONLDecoder();
  #pendingSubmit = new Map<string, (reply: RunSocketMessage) => void>();
  #pendingShOutput = new Map<string, (msg: RunSocketMessage) => void>();
  #pendingShExit = new Map<string, (msg: RunSocketMessage) => void>();

  async connect(path: string): Promise<void> {
    await new Promise<void>((resolve, reject) => {
      const socket = createConnection(path);
      socket.once("connect", () => {
        this.#socket = socket;
        resolve();
      });
      socket.once("error", reject);
      socket.on("data", (chunk) => {
        const messages = this.#decoder.push(chunk) as RunSocketMessage[];
        for (const msg of messages) this.#dispatch(msg);
      });
    });
  }

  #dispatch(msg: RunSocketMessage): void {
    if (msg.type === "submit_reply") {
      this.#pendingSubmit.get(msg.id)?.(msg);
      this.#pendingSubmit.delete(msg.id);
    } else if (msg.type === "sh_output") {
      this.#pendingShOutput.get(msg.commandId)?.(msg);
    } else if (msg.type === "sh_exit") {
      this.#pendingShExit.get(msg.commandId)?.(msg);
      this.#pendingShExit.delete(msg.commandId);
    }
  }

  send(msg: RunSocketMessage): void {
    this.#socket?.write(encodeLine(msg));
  }

  async submit(tool: "submit_phase" | "submit_discovery" | "submit_review", args: unknown): Promise<RunSocketMessage> {
    const id = randomUUID();
    const reply = new Promise<RunSocketMessage>((resolve) => this.#pendingSubmit.set(id, resolve));
    this.send({ type: "submit", id, tool, args });
    return reply;
  }

  async sh(command: string, cwd: string | undefined, onOutput: (chunk: string) => void): Promise<RunSocketMessage> {
    const id = randomUUID();
    const commandId = randomUUID();
    this.#pendingShOutput.set(commandId, (msg) => {
      if (msg.type === "sh_output") onOutput(msg.chunk);
    });
    const exit = new Promise<RunSocketMessage>((resolve) => this.#pendingShExit.set(commandId, resolve));
    this.send({ type: "sh", id, commandId, command, cwd });
    return exit;
  }
}

// --- main ---

async function main(): Promise<void> {
  const script = loadScript();
  const runSocket = new RunSocket();
  const socketPath = readEnv("TT_SOCKET");

  if (socketPath && script.hello) {
    await runSocket.connect(socketPath);
    runSocket.send({
      type: "hello",
      agentId: script.hello.agentId ?? readEnv("TT_AGENT_ID") ?? "fake-1",
      role: script.hello.role,
      tools: script.hello.tools,
      piVersion: script.hello.piVersion,
    });
  }

  let abortRequested = false;
  let resolveAbortWait: (() => void) | undefined;
  const rpcDecoder = new JSONLDecoder();
  let ranOnce = false;

  async function runSteps(): Promise<void> {
    writeStdout({ type: "agent_start" });
    for (const step of script.steps) {
      switch (step.kind) {
        case "emit":
          writeStdout(step.event);
          break;
        case "call-submit": {
          const toolCallId = randomUUID();
          writeStdout({ type: "tool_execution_start", toolCallId, toolName: step.tool, args: step.args });
          const reply = await runSocket.submit(step.tool, step.args);
          const ok = reply.type === "submit_reply" && reply.ok;
          writeStdout({
            type: "tool_execution_end",
            toolCallId,
            toolName: step.tool,
            result: { content: [{ type: "text", text: ok ? "submission accepted" : "submission rejected" }] },
            isError: !ok,
          });
          break;
        }
        case "call-sh": {
          const toolCallId = randomUUID();
          writeStdout({ type: "tool_execution_start", toolCallId, toolName: "sh", args: { command: step.command, cwd: step.cwd } });
          const chunks: string[] = [];
          await runSocket.sh(step.command, step.cwd, (chunk) => chunks.push(chunk));
          writeStdout({
            type: "tool_execution_end",
            toolCallId,
            toolName: "sh",
            result: { content: [{ type: "text", text: chunks.join("") }] },
            isError: false,
          });
          break;
        }
        case "sleep":
          await new Promise((resolve) => setTimeout(resolve, step.ms));
          break;
        case "hang-until-abort":
          if (!abortRequested) {
            await new Promise<void>((resolve) => {
              resolveAbortWait = resolve;
            });
          }
          writeStdout({ type: "agent_end", messages: [], willRetry: false });
          writeStdout({ type: "agent_settled" });
          return;
        case "hang-forever":
          await new Promise(() => {
            // never resolves — survives abort by design, for phase 1's
            // escalation-to-SIGTERM/SIGKILL tests.
          });
          return;
        case "crash":
          process.exit(step.code);
      }
    }
    writeStdout({ type: "agent_end", messages: [], willRetry: false });
    writeStdout({ type: "agent_settled" });
  }

  process.stdin.on("data", (chunk) => {
    const commands = rpcDecoder.push(chunk) as Array<Record<string, unknown>>;
    for (const cmd of commands) {
      const id = cmd.id as string | undefined;
      switch (cmd.type) {
        case "prompt":
        case "steer":
        case "follow_up":
          writeStdout({ type: "response", id, command: cmd.type, success: true });
          if (cmd.type === "prompt" && !ranOnce) {
            ranOnce = true;
            void runSteps();
          }
          break;
        case "abort":
          abortRequested = true;
          resolveAbortWait?.();
          writeStdout({ type: "response", id, command: "abort", success: true });
          break;
        case "get_state":
          writeStdout({
            type: "response",
            id,
            command: "get_state",
            success: true,
            data: { isStreaming: ranOnce, sessionFile: null },
          });
          break;
        default:
          writeStdout({ type: "response", id, command: String(cmd.type), success: false, reason: `unknown command '${String(cmd.type)}'` });
      }
    }
  });

  process.stdin.on("end", () => {
    // A real pi process would exit on stdin EOF; match that so tests can
    // rely on process exit as a signal.
    process.exit(0);
  });
}

void main();
