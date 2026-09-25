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
//       { "kind": "emit-env", "name": "FAKE_KEY" },
//       { "kind": "sleep", "ms": 10 },
//       { "kind": "hang-until-abort" },
//       { "kind": "hang-forever" },
//       { "kind": "crash", "code": 7 },
//       { "kind": "wait-for-prompt" }
//     ]
//   }
//
// `hello` is sent once at startup (mirroring `session_start`), if
// `TT_SOCKET` is set. Steps run once, in order, after the first `prompt`
// command is received; unless a step hangs or crashes, the run finishes
// with an automatic `agent_end` + `agent_settled`. Work packet 2a's real
// two-turn review sends a SECOND `prompt` (turn 2) to the same process —
// `wait-for-prompt` pauses the step list until that second (or any later)
// `prompt` arrives, so one script can cover both turns.

import { createConnection, type Socket } from "node:net";
import { appendFileSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
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
  // Plan 01a: report one environment variable of THIS agent process as a tool
  // result, so a test can assert what the conductor put in an agent's
  // environment (a real agent runs commands through the conductor, whose
  // environment is not the agent's).
  | { kind: "emit-env"; name: string }
  | { kind: "sleep"; ms: number }
  | { kind: "hang-until-abort" }
  | { kind: "hang-forever" }
  | { kind: "crash"; code: number }
  // Work packet 2a: a real two-turn review is one `steps` array spanning
  // two `prompt` RPC commands (the conductor's own turn-1/turn-2 prompts) —
  // this step pauses `runSteps` until the SECOND (and any later) `prompt`
  // arrives, so a script can run turn-1 steps, wait, then run turn-2 steps,
  // all as one continuous script (see `RunSocket`'s own doc comment).
  | { kind: "wait-for-prompt" };

interface Script {
  hello?: HelloSpec;
  steps: Step[];
}

let hangingForever = false;

function readEnv(name: string): string | undefined {
  const v = process.env[name];
  return v && v.length > 0 ? v : undefined;
}

/** Phase 1b work-packet item 6: a `call-submit` step's `args` is a static
 * JSON script, but a `tt start`-launched (as opposed to in-process-test)
 * reviewer script has no other way to learn a value the conductor only
 * knows at dispatch time, such as the live candidate sha — a real reviewer
 * is simply told this directly. Any string value that is *exactly* one of
 * these tokens is substituted with the named env var at the moment the
 * step runs (recursing into objects and arrays; every other value, and a
 * token naming an unset env var, passes through unchanged). Keeps this to
 * a small, explicit allowlist rather than a general `$VAR` syntax, so a
 * script's own literal string content is never at risk of accidental
 * substitution. */
const ENV_TOKENS = ["$TT_CANDIDATE_SHA", "$TT_REVIEWER"] as const;

function substituteEnvTokens(value: unknown): unknown {
  if (typeof value === "string") {
    if ((ENV_TOKENS as readonly string[]).includes(value)) {
      const name = value.slice(1);
      const v = process.env[name];
      return v !== undefined && v.length > 0 ? v : value;
    }
    return value;
  }
  if (Array.isArray(value)) return value.map(substituteEnvTokens);
  if (value && typeof value === "object") {
    const out: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(value as Record<string, unknown>)) out[k] = substituteEnvTokens(v);
    return out;
  }
  return value;
}

/** Phase 1b work-packet item 6: a `tt start`-launched conductor has no
 * `piEnvFor` hook to give the worker and each reviewer their own
 * `FAKE_PI_SCRIPT`, only whatever env it inherited once for the whole run.
 * If `FAKE_PI_SCRIPT` names a directory (rather than a file) instead of one
 * shared script for every role, this reads `<dir>/<TT_ROLE>.json` — `TT_ROLE`
 * is always set by the conductor at spawn time (pi-rpc.ts), so a plan that
 * needs the worker and reviewers to behave differently can still use one
 * env var for the whole run. */
function loadScript(): Script {
  const scriptPath = readEnv("FAKE_PI_SCRIPT");
  if (!scriptPath) throw new Error("FAKE_PI_SCRIPT is required");
  const resolved = statSync(scriptPath).isDirectory()
    ? join(scriptPath, `${readEnv("TT_ROLE") ?? "worker"}.json`)
    : scriptPath;
  return JSON.parse(readFileSync(resolved, "utf8")) as Script;
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
  let resolveNextPromptWait: (() => void) | undefined;
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
          const args = substituteEnvTokens(step.args);
          writeStdout({ type: "tool_execution_start", toolCallId, toolName: step.tool, args });
          const reply = await runSocket.submit(step.tool, args);
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
        case "emit-env": {
          const toolCallId = randomUUID();
          writeStdout({ type: "tool_execution_start", toolCallId, toolName: "env", args: { name: step.name } });
          writeStdout({
            type: "tool_execution_end",
            toolCallId,
            toolName: "env",
            result: {
              content: [{ type: "text", text: `${step.name}=${process.env[step.name] ?? "(unset)"}` }],
            },
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
          // An unresponsive agent ignores abort AND stdin EOF (plan 2c:
          // terminate() now closes stdin after abort), so escalation to
          // SIGTERM/SIGKILL is still exercised.
          hangingForever = true;
          await new Promise(() => {
            // never resolves — survives abort by design, for phase 1's
            // escalation-to-SIGTERM/SIGKILL tests.
          });
          return;
        case "crash":
          process.exit(step.code);
        case "wait-for-prompt":
          // Like real Pi, the current turn settles before the next prompt
          // starts a new one (the conductor waits for this settle).
          writeStdout({ type: "agent_end", messages: [], willRetry: false });
          writeStdout({ type: "agent_settled" });
          await new Promise<void>((resolve) => {
            resolveNextPromptWait = resolve;
          });
          writeStdout({ type: "agent_start" });
          break;
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
          // Test-only prompt capture (opt-in via FAKE_PI_PROMPT_LOG): lets a
          // conductor test assert what the conductor actually sent — e.g.
          // that a queued `note` reached the next worker attempt's prompt.
          if (cmd.type === "prompt" && readEnv("FAKE_PI_PROMPT_LOG")) {
            appendFileSync(readEnv("FAKE_PI_PROMPT_LOG")!, `${String(cmd.message)}\n=====\n`);
          }
          // Plan 2d test-only steer capture (opt-in via FAKE_PI_STEER_LOG).
          if (cmd.type === "steer" && readEnv("FAKE_PI_STEER_LOG")) {
            appendFileSync(readEnv("FAKE_PI_STEER_LOG")!, `${String(cmd.message)}\n=====\n`);
          }
          if (cmd.type === "prompt") {
            if (!ranOnce) {
              ranOnce = true;
              void runSteps();
            } else if (resolveNextPromptWait) {
              const resolve = resolveNextPromptWait;
              resolveNextPromptWait = undefined;
              resolve();
            }
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
    if (hangingForever) return;
    process.exit(0);
  });
}

void main();
