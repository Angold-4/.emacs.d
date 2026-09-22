import assert from "node:assert/strict";
import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { createServer, type Server, type Socket } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { after, test } from "node:test";

import { JSONLDecoder, encodeLine, type RunSocketMessage } from "../../src/core/protocol.ts";
import { assertToolSet } from "../../src/core/roles.ts";

const FAKE_PI = new URL("./fake-pi.ts", import.meta.url).pathname;

const tmpRoot = mkdtempSync(join(tmpdir(), "tt-fake-pi-"));
after(() => {
  try {
    rmSync(tmpRoot, { recursive: true, force: true });
  } catch {
    // best-effort
  }
});

function writeScript(name: string, script: unknown): string {
  const path = join(tmpRoot, `${name}-${Math.random().toString(36).slice(2)}.json`);
  writeFileSync(path, JSON.stringify(script));
  return path;
}

interface StubServer {
  server: Server;
  socketPath: string;
  received: RunSocketMessage[];
}

/** A stand-in run-socket server: records everything it decodes, and
 * replies `ok: true` to every `submit`. */
function startStubServer(onMessage?: (msg: RunSocketMessage, socket: Socket) => void): StubServer {
  const socketPath = join(tmpRoot, `stub-${Math.random().toString(36).slice(2)}.sock`);
  const received: RunSocketMessage[] = [];
  const server = createServer((socket: Socket) => {
    const decoder = new JSONLDecoder();
    socket.on("data", (chunk) => {
      const messages = decoder.push(chunk) as RunSocketMessage[];
      for (const msg of messages) {
        received.push(msg);
        if (onMessage) {
          onMessage(msg, socket);
        } else if (msg.type === "submit") {
          socket.write(encodeLine({ type: "submit_reply", id: msg.id, ok: true }));
        }
      }
    });
  });
  server.listen(socketPath);
  return { server, socketPath, received };
}

function spawnFakePi(scriptPath: string, socketPath?: string): ChildProcessWithoutNullStreams {
  return spawn(process.execPath, [FAKE_PI], {
    env: { ...process.env, FAKE_PI_SCRIPT: scriptPath, ...(socketPath ? { TT_SOCKET: socketPath, TT_AGENT_ID: "fake-1" } : {}) },
    stdio: ["pipe", "pipe", "pipe"],
  });
}

function collectEvents(child: ChildProcessWithoutNullStreams): { events: Array<Record<string, unknown>>; decoder: JSONLDecoder } {
  const events: Array<Record<string, unknown>> = [];
  const decoder = new JSONLDecoder();
  child.stdout.on("data", (chunk) => {
    for (const msg of decoder.push(chunk)) events.push(msg as Record<string, unknown>);
  });
  return { events, decoder };
}

function sendCommand(child: ChildProcessWithoutNullStreams, cmd: Record<string, unknown>): void {
  child.stdin.write(encodeLine(cmd));
}

async function waitUntil(predicate: () => boolean, timeoutMs = 5000): Promise<void> {
  const start = Date.now();
  while (!predicate()) {
    if (Date.now() - start > timeoutMs) throw new Error("timed out waiting for condition");
    await new Promise((r) => setTimeout(r, 20));
  }
}

async function waitForExit(child: ChildProcessWithoutNullStreams, timeoutMs = 5000): Promise<number | null> {
  if (child.exitCode !== null) return child.exitCode;
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error("timed out waiting for exit")), timeoutMs);
    child.once("exit", (code) => {
      clearTimeout(timer);
      resolve(code);
    });
  });
}

// --- Framing (shared protocol.ts module) ---------------------------------

test("fake-pi framing: split chunks, multiple records per chunk, CRLF, and U+2028 inside a string", () => {
  const decoder = new JSONLDecoder();

  // Split mid-record across two pushes.
  const record = JSON.stringify({ type: "agent_start", note: "hello" });
  const half = Math.floor(record.length / 2);
  assert.deepEqual(decoder.push(record.slice(0, half)), []);
  assert.deepEqual(decoder.push(`${record.slice(half)}\n`), [{ type: "agent_start", note: "hello" }]);

  // Multiple records in one chunk.
  const two = `${JSON.stringify({ type: "a" })}\n${JSON.stringify({ type: "b" })}\n`;
  assert.deepEqual(decoder.push(two), [{ type: "a" }, { type: "b" }]);

  // CRLF line endings.
  const crlf = `${JSON.stringify({ type: "c" })}\r\n`;
  assert.deepEqual(decoder.push(crlf), [{ type: "c" }]);

  // U+2028 (LINE SEPARATOR) inside a JSON string must not be treated as a
  // record boundary — only a literal \n is (this is exactly what makes
  // Node's `readline` non-compliant for RPC framing).
  const withSeparator = `${JSON.stringify({ type: "d", text: "line one line two" })}\n`;
  const decoded = decoder.push(withSeparator);
  assert.deepEqual(decoded, [{ type: "d", text: "line one line two" }]);
});

// --- Deterministic replay --------------------------------------------------

test("fake-pi: deterministic replay — same script produces the same event sequence", async () => {
  const script = {
    steps: [
      { kind: "emit", event: { type: "agent_start" } },
      { kind: "emit", event: { type: "message_update", assistantMessageEvent: { type: "text_delta", delta: "hi" } } },
      { kind: "sleep", ms: 5 },
    ],
  };
  const path = writeScript("deterministic", script);

  async function run(): Promise<Array<Record<string, unknown>>> {
    const child = spawnFakePi(path);
    const { events } = collectEvents(child);
    sendCommand(child, { type: "prompt", id: "1", message: "go" });
    await waitUntil(() => events.some((e) => e.type === "agent_settled"));
    child.stdin.end();
    await waitForExit(child).catch(() => undefined);
    return events;
  }

  const first = await run();
  const second = await run();
  assert.deepEqual(first, second);
});

// --- abort ends hang-until-abort ------------------------------------------

test("fake-pi: abort ends a hang-until-abort step with agent_end + agent_settled", async () => {
  const path = writeScript("hang-until-abort", { steps: [{ kind: "hang-until-abort" }] });
  const child = spawnFakePi(path);
  const { events } = collectEvents(child);

  sendCommand(child, { type: "prompt", id: "1", message: "go" });
  await waitUntil(() => events.some((e) => e.type === "agent_start"));

  sendCommand(child, { type: "abort", id: "2" });
  await waitUntil(() => events.some((e) => e.type === "agent_settled"));

  assert.ok(events.some((e) => e.type === "agent_end"));
  assert.ok(events.some((e) => e.type === "response" && e.command === "abort" && e.success === true));

  child.stdin.end();
  await waitForExit(child).catch(() => undefined);
});

// --- hang-forever survives abort -------------------------------------------

test("fake-pi: hang-forever survives abort (no agent_end/agent_settled; process stays alive)", async () => {
  const path = writeScript("hang-forever", { steps: [{ kind: "hang-forever" }] });
  const child = spawnFakePi(path);
  const { events } = collectEvents(child);

  sendCommand(child, { type: "prompt", id: "1", message: "go" });
  await waitUntil(() => events.some((e) => e.type === "agent_start"));
  sendCommand(child, { type: "abort", id: "2" });

  // Give it a real chance to (wrongly) settle before asserting it didn't.
  await new Promise((r) => setTimeout(r, 300));
  assert.equal(
    events.some((e) => e.type === "agent_end" || e.type === "agent_settled"),
    false,
    "hang-forever must ignore abort",
  );
  assert.equal(child.exitCode, null, "process must still be running");

  child.kill("SIGKILL");
  await waitForExit(child).catch(() => undefined);
});

// --- crash exits with scripted code ----------------------------------------

test("fake-pi: crash exits with the scripted code", async () => {
  const path = writeScript("crash", { steps: [{ kind: "crash", code: 7 }] });
  const child = spawnFakePi(path);
  sendCommand(child, { type: "prompt", id: "1", message: "go" });
  const code = await waitForExit(child);
  assert.equal(code, 7);
});

// --- scripted submit reaches a test socket server ---------------------------

test("fake-pi: a scripted submit reaches a test socket server", async () => {
  const stub = startStubServer();
  const script = {
    hello: { role: "worker", tools: ["read", "edit", "write", "grep", "find", "ls", "sh", "submit_phase"] },
    steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
  };
  const path = writeScript("submit", script);
  const child = spawnFakePi(path, stub.socketPath);
  const { events } = collectEvents(child);

  await waitUntil(() => stub.received.some((m) => m.type === "hello"));
  sendCommand(child, { type: "prompt", id: "1", message: "go" });
  await waitUntil(() => events.some((e) => e.type === "tool_execution_end"));

  const submitMsg = stub.received.find((m) => m.type === "submit");
  assert.ok(submitMsg && submitMsg.type === "submit");
  if (submitMsg && submitMsg.type === "submit") {
    assert.equal(submitMsg.tool, "submit_phase");
    assert.deepEqual(submitMsg.args, { decisions: [], assumptions: [], deviations: [] });
  }
  const end = events.find((e) => e.type === "tool_execution_end") as { isError: boolean } | undefined;
  assert.equal(end?.isError, false);

  child.stdin.end();
  await waitForExit(child).catch(() => undefined);
  stub.server.close();
});

// --- scripted wrong hello tool list is caught by assertToolSet -------------

test("fake-pi: a scripted wrong hello tool list is caught by assertToolSet", async () => {
  const stub = startStubServer();
  const script = {
    // A worker reported with the reviewer's submission tools and no shell —
    // the same shape the --exclude-tools pitfall produces.
    hello: { role: "worker", tools: ["read", "edit", "write", "submit_discovery", "submit_review"] },
    steps: [],
  };
  const path = writeScript("bad-hello", script);
  const child = spawnFakePi(path, stub.socketPath);

  await waitUntil(() => stub.received.some((m) => m.type === "hello"));
  const hello = stub.received.find((m) => m.type === "hello");
  assert.ok(hello && hello.type === "hello");
  if (hello && hello.type === "hello") {
    const result = assertToolSet("worker", hello.tools);
    assert.equal(result.ok, false);
    if (!result.ok) {
      assert.ok(result.missing.includes("sh"));
      assert.ok(result.extra.includes("submit_discovery"));
      assert.ok(result.extra.includes("submit_review"));
    }
  }

  child.stdin.end();
  await waitForExit(child).catch(() => undefined);
  stub.server.close();
});
