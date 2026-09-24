// terminate() on an agent that never acknowledges the abort (a hung Pi) must
// still finish — it used to await the acknowledgement forever — and a prompt
// or steer racing it must fail its own promise, not crash the conductor with
// ERR_STREAM_WRITE_AFTER_END (seen intermittently in force-kill-shell under a
// loaded full suite, and when stopping a run while a reviewer was prompted).

import assert from "node:assert/strict";
import { test } from "node:test";

import { spawnPiAgent } from "../../src/effects/pi-rpc.ts";

test("pi-rpc: a send after terminate() is rejected, never an unhandled stream error", async () => {
  // A stand-in agent that never answers any command (so never acknowledges
  // the abort) and exits only when its stdin ends.
  const agent = spawnPiAgent({
    command: process.execPath,
    args: ["-e", "process.stdin.resume(); process.stdin.on('end', () => process.exit(0));"],
    role: "worker",
    agentId: "worker-test",
    abortGraceMs: 200,
    termGraceMs: 200,
  });
  let crashed: unknown;
  const onError = (err: unknown) => {
    crashed = err;
  };
  process.on("uncaughtException", onError);
  try {
    await agent.terminate();
    await assert.rejects(agent.prompt("late"), /terminating|already exited/);
    await assert.rejects(agent.steer("late"), /terminating|already exited/);
    await new Promise((r) => setTimeout(r, 100));
    assert.equal(crashed, undefined, "no uncaught stream error");
  } finally {
    process.off("uncaughtException", onError);
  }
});
