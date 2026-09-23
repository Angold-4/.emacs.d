// Unit test for the `no_submission` protocol addition (design §3.3 item 1,
// §9.5; src/core/protocol.ts's `NoSubmissionMessage`). This is a pure,
// additive message: extension/tradeoffs-trace.ts's `agent_before_settle`
// hook sends it once its two allowed settle continuations are exhausted
// with no accepted `submit_phase` (see that file's own test coverage via
// extension/guards.ts's unit tests for the guard side); this test covers
// the wire/dispatch side directly against `RunSocketServer`, without
// needing a real or fake Pi process.
//
// Also covers, per the phase-1b brief item 6: `submit_discovery` stays an
// accepted no-op for phase 1's stub reviews (the real discovery/correction
// loop is phase 2) — documented here, not just in conductor.ts's comment.

import assert from "node:assert/strict";
import { createConnection } from "node:net";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { encodeLine, JSONLDecoder, type RunSocketMessage } from "../../src/core/protocol.ts";
import { RunSocketServer } from "../../src/effects/socket.ts";

function shortSockPath(): string {
  const dir = fs.mkdtempSync(path.join("/tmp", "tt-sock-"));
  return path.join(dir, "s.sock");
}

test("no_submission: the run socket forwards it to onNoSubmission with the sender's agentId", async () => {
  const sockPath = shortSockPath();
  const noSubmissions: string[] = [];
  const server = await RunSocketServer.start(sockPath, {
    onHello: () => ({ ok: true }),
    onSubmit: () => ({ ok: true }),
    cwdFor: () => undefined,
    onNoSubmission: (agentId) => {
      noSubmissions.push(agentId);
    },
  });

  try {
    const socket = createConnection(sockPath);
    await new Promise<void>((resolve, reject) => {
      socket.once("connect", resolve);
      socket.once("error", reject);
    });

    socket.write(encodeLine({ type: "hello", agentId: "worker-1", role: "worker", tools: [] }));
    socket.write(encodeLine({ type: "no_submission", agentId: "worker-1" }));

    await new Promise((resolve) => setTimeout(resolve, 100));
    assert.deepEqual(noSubmissions, ["worker-1"]);

    socket.destroy();
  } finally {
    await server.close();
    fs.rmSync(path.dirname(sockPath), { recursive: true, force: true });
  }
});

test("no_submission: a server with no onNoSubmission handler does not throw", async () => {
  const sockPath = shortSockPath();
  const server = await RunSocketServer.start(sockPath, {
    onHello: () => ({ ok: true }),
    onSubmit: () => ({ ok: true }),
    cwdFor: () => undefined,
  });

  try {
    const socket = createConnection(sockPath);
    await new Promise<void>((resolve, reject) => {
      socket.once("connect", resolve);
      socket.once("error", reject);
    });
    socket.write(encodeLine({ type: "no_submission", agentId: "worker-1" }));
    await new Promise((resolve) => setTimeout(resolve, 50));
    socket.destroy();
  } finally {
    await server.close();
    fs.rmSync(path.dirname(sockPath), { recursive: true, force: true });
  }
});

test("submit_discovery: accepted as a no-op in phase 1 (stub reviews) via the socket's onSubmit contract", async () => {
  const sockPath = shortSockPath();
  const submitted: RunSocketMessage[] = [];
  const server = await RunSocketServer.start(sockPath, {
    onHello: () => ({ ok: true }),
    // Mirrors Conductor#onSubmit's own handling: submit_discovery is
    // accepted unconditionally (phase 1's stub reviewers are not expected
    // to call it, but a scripted one must not fail outright).
    onSubmit: (_agentId, msg) => {
      submitted.push(msg);
      if (msg.tool === "submit_discovery") return { ok: true };
      return { ok: false, reason: "unexpected" };
    },
    cwdFor: () => undefined,
  });

  try {
    const socket = createConnection(sockPath);
    await new Promise<void>((resolve, reject) => {
      socket.once("connect", resolve);
      socket.once("error", reject);
    });

    const decoder = new JSONLDecoder();
    const replies: RunSocketMessage[] = [];
    socket.on("data", (chunk) => {
      for (const msg of decoder.push(chunk) as RunSocketMessage[]) replies.push(msg);
    });

    socket.write(encodeLine({ type: "hello", agentId: "reviewer-M-1", role: "reviewer", tools: [] }));
    socket.write(encodeLine({ type: "submit", id: "s1", tool: "submit_discovery", args: { discoveries: [] } }));

    await new Promise((resolve) => setTimeout(resolve, 100));
    assert.equal(submitted.length, 1);
    assert.equal(submitted[0].tool, "submit_discovery");
    const reply = replies.find((r) => r.type === "submit_reply");
    assert.ok(reply, "expected a submit_reply");
    assert.equal((reply as { ok: boolean }).ok, true);

    socket.destroy();
  } finally {
    await server.close();
    fs.rmSync(path.dirname(sockPath), { recursive: true, force: true });
  }
});
