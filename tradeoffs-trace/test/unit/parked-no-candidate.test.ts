import { test } from "node:test";
import assert from "node:assert/strict";
import { notAcceptedReasons } from "../../src/core/verdict.ts";
import { PiAgent } from "../../src/effects/pi-rpc.ts";
import { basePhase } from "./helpers.ts";

test("exex 07f: a phase parked with every lane red names each lane's failed check", () => {
  const phase = {
    ...basePhase(),
    candidate: undefined,
    rounds: [{ round: 3, lanes: ["a", "b"], candidates: [
      { lane: "a", sha: "ec98092819307e90", ok: false, note: "named inventory failed" },
      { lane: "b", sha: "1e70696679857f56", ok: false, note: "affected nextest failed" },
    ] }],
  } as unknown as Parameters<typeof notAcceptedReasons>[0];
  const reasons = notAcceptedReasons(phase);
  assert.equal(reasons.length, 2);
  assert.match(reasons[0], /round 3 lane a candidate ec98092819 failed its checks: named inventory failed/);
  assert.match(reasons[1], /lane b candidate 1e70696679 failed its checks: affected nextest failed/);
});

test("exex 07f: a round with no submitted candidate still gives a reason", () => {
  const phase = { ...basePhase(), candidate: undefined, rounds: [{ round: 1, lanes: ["a"], candidates: [] }] } as unknown as Parameters<typeof notAcceptedReasons>[0];
  assert.deepEqual(notAcceptedReasons(phase), ["round 1: no lane submitted a checked candidate"]);
});

test("exex 07f: a pi agent spawned in a missing directory throws and never crashes the process", async () => {
  let unhandled: unknown;
  const onError = (err: unknown) => { unhandled = err; };
  process.on("uncaughtException", onError);
  try {
    assert.throws(
      () => new PiAgent({ agentId: "x", role: "evaluator", command: process.execPath, args: ["-e", ""], cwd: "/nonexistent/tt-07f-missing-cwd" } as unknown as ConstructorParameters<typeof PiAgent>[0]),
      /failed to spawn pi agent: no pid/,
    );
    await new Promise((r) => setTimeout(r, 200));
    assert.equal(unhandled, undefined);
  } finally {
    process.off("uncaughtException", onError);
  }
});
