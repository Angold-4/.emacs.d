import assert from "node:assert/strict";
import { test } from "node:test";
import { infrastructureTimeout, runCheckWithInfrastructureRetry, type InfrastructureRetry } from "../../src/effects/check-retry.ts";
import type { RunCommandResult } from "../../src/effects/shell.ts";

const timeout = (output = "Compiling crate v1.0\n"): RunCommandResult => ({ exitCode: null, signal: "SIGTERM", timedOut: true, cancelled: false, output, signalsSent: [] });
const pass: RunCommandResult = { exitCode: 0, signal: null, timedOut: false, cancelled: false, output: "ok", signalsSent: [] };

async function run(results: RunCommandResult[], load1 = 0, prior?: InfrastructureRetry) {
  let calls = 0;
  let retry: InfrastructureRetry | undefined;
  let completed: InfrastructureRetry | undefined;
  const result = await runCheckWithInfrastructureRetry({ command: "cargo test", load: () => load1, stopped: () => false, prior,
    run: async () => { assert.ok(calls < results.length, "no third attempt"); return results[calls++]; },
    onRetry: (r) => { retry = JSON.parse(JSON.stringify(r)); },
    onRetried: (r) => { completed = JSON.parse(JSON.stringify(r)); },
  });
  return { result, calls, retry, completed };
}

test("compilation timeout retries the same check once and recovers", async () => {
  const r = await run([timeout(), pass]);
  assert.equal(r.calls, 2);
  assert.equal(r.retry?.first.timedOut, true);
  assert.equal(r.completed?.result?.exitCode, 0);
  assert.equal(r.result.exitCode, 0);
  assert.equal(r.result.infrastructureStall, undefined);
});

test("a second timeout parks with the check and both host loads", async () => {
  const r = await run([timeout(), timeout("running test")], 12.5);
  assert.equal(r.calls, 2);
  assert.match(r.result.infrastructureStall!, /infrastructure stall.*cargo test.*timed out again.*load1 12.5 -> 12.5/);
  const recovered = await run([], 0, r.completed);
  assert.equal(recovered.calls, 0);
  assert.equal(recovered.result.infrastructureStall, r.result.infrastructureStall);
  const interrupted = await run([], 0, r.retry);
  assert.match(interrupted.result.infrastructureStall!, /retry interrupted/);
  const cancelled = await run([], 0, { ...r.completed!, result: { ...timeout(), cancelled: true } });
  assert.match(cancelled.result.infrastructureStall!, /retry interrupted/);
});

test("high load timeout retries without compiling; ordinary candidate failures and cancelled checks do not retry", async () => {
  assert.equal(infrastructureTimeout(timeout("running tests"), 8), false);
  assert.equal(infrastructureTimeout(timeout("running tests"), 8.01), true);
  assert.equal(infrastructureTimeout(timeout("Compiling crate\nFinished dev profile\nrunning tests"), 0), false);
  assert.equal(infrastructureTimeout(timeout("Compiling failure v0.1.8\nwarning: compiling dependency"), 0), true);
  assert.equal((await run([timeout("running tests"), pass], 10)).calls, 2);
  const red = { ...pass, exitCode: 1, output: "Compiling crate\nerror: invalid code" };
  assert.equal((await run([red], 25)).calls, 1);
  assert.equal((await run([{ ...timeout(), cancelled: true }], 25)).calls, 1);
  assert.equal((await run([timeout("running tests")])).calls, 1);
});

test("a real failure on the retry stays a candidate failure", async () => {
  const red = { ...pass, exitCode: 1, output: "assertion failed" };
  const r = await run([timeout(), red]);
  assert.equal(r.result.exitCode, 1);
  assert.equal(r.result.infrastructureStall, undefined);
});

test("a stop between attempts suppresses the retry", async () => {
  let stopped = false;
  let calls = 0;
  await runCheckWithInfrastructureRetry({ command: "cargo test", load: () => 0, stopped: () => stopped,
    run: async () => { calls++; return timeout(); }, onRetry: () => { stopped = true; }, onRetried: () => assert.fail("stopped"),
  });
  assert.equal(calls, 1);
});
