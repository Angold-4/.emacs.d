import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";
import { registerHooks } from "node:module";
import { EventLog, readLog } from "../../src/effects/log.ts";
import { parseCheckRecord } from "../../src/core/checks.ts";
import { baseState } from "./helpers.ts";
import type { RunCommandResult } from "../../src/effects/shell.ts";

const seams = ["checkWithInfrastructureRetry", "checkRecordCommand", "checkLogName", "state", "paths", "stopRequested", "closed", "recordCheck", "log", "recordBound", "secretMaskable", "infrastructureLoad1"];
const url = new URL("../../src/conductor.ts?infrastructure-driver", import.meta.url).href;
const hooks = registerHooks({ load(target, context, load) {
  const result = load(target, context);
  return target === url ? { ...result, source: result.source.toString().replace(new RegExp(`#(${seams.join("|")})\\b`, "g"), "test_$1") } : result;
} });
const { Conductor } = await import(url);
hooks.deregister();

test("the production check driver persists its one retry and restores its infrastructure outcome", async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-infrastructure-driver-"));
  const file = path.join(dir, "events.jsonl");
  let log = new EventLog(file);
  try {
    const bounds: unknown[][] = [];
    const recorded: string[] = [];
    const driver = () => {
      const conductor = Object.create(Conductor.prototype);
      conductor.test_secretMaskable = [];
      conductor.test_infrastructureLoad1 = () => 12;
      conductor.test_state = baseState({ phase: "CHECKING", repairRoundsGranted: 47 });
      conductor.test_paths = { events: file };
      conductor.test_stopRequested = false;
      conductor.test_closed = false;
      conductor.test_log = log;
      conductor.test_recordCheck = (_dir: string, command: string) => { recorded.push(command); };
      conductor.test_recordBound = (...args: unknown[]) => bounds.push(args);
      return conductor;
    };
    let executions = 0;
    const run = async (): Promise<RunCommandResult> => {
      executions++;
      return { exitCode: null, signal: "SIGTERM", timedOut: true, cancelled: false, output: "Compiling crate v1", signalsSent: ["SIGTERM"] };
    };
    const first = driver();
    const result = await first.test_checkWithInfrastructureRetry("C1", "cargo test", "round", dir, 720_000, run);
    assert.equal(executions, 2);
    assert.equal(recorded.length, 1, "the first attempt keeps its own log");
    assert.equal(bounds.length, 1, "the retry records the first timeout's bound");
    assert.equal(first.test_state.phase.repairRoundsUsed, 0);
    const records = readLog(file).records;
    assert.deepEqual(records.map((r) => r.kind), ["check_infrastructure_retry", "check_infrastructure_retried"]);
    const command = first.test_checkRecordCommand("cargo test", result, Date.now());
    assert.equal(command.class, "infrastructure-stall");
    assert.equal(command.passed, false);
    assert.match(command.note, /cargo test.*load1/);
    const record = parseCheckRecord({ candidateSha: "C1", tier: "round", passed: false, commands: [command], finalCommands: [], load1: 12, freeMemMB: 4096, at: new Date().toISOString() });
    assert.equal(record?.commands[0].class, "infrastructure-stall");
    log.close();
    log = new EventLog(file);
    const resumed = driver();
    const restored = await resumed.test_checkWithInfrastructureRetry("C1", "cargo test", "round", dir, 720_000, run);
    assert.equal(executions, 2, "restart executes no third check");
    assert.equal(restored.infrastructureStall, result.infrastructureStall);
    assert.equal(resumed.test_state.phase.repairRoundsUsed, 0);
  } finally { log.close(); fs.rmSync(dir, { recursive: true, force: true }); }
});

for (const [output, load1, calls] of [["START", 0, 1], ["", 8, 1], ["START", 8.01, 2], ["Compiling crate v1", 0, 2]] as const) {
  test(`production timeout policy: output ${JSON.stringify(output)}, load1 ${load1}, ${calls} attempt(s)`, async () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-timeout-policy-"));
    const file = path.join(dir, "events.jsonl");
    const log = new EventLog(file);
    try {
      const conductor = Object.create(Conductor.prototype);
      conductor.test_state = baseState({ phase: "CHECKING" });
      conductor.test_paths = { events: file };
      conductor.test_infrastructureLoad1 = () => load1;
      conductor.test_stopRequested = false;
      conductor.test_closed = false;
      conductor.test_log = log;
      conductor.test_recordCheck = () => undefined;
      conductor.test_recordBound = () => undefined;
      let executions = 0;
      const result = await conductor.test_checkWithInfrastructureRetry("C1", "fake check", "round", dir, 700, async () => {
        executions++;
        return { exitCode: null, signal: "SIGTERM", timedOut: true, cancelled: false, output, signalsSent: ["SIGTERM"] };
      });
      assert.equal(executions, calls);
      assert.equal(result.timedOut, true);
      assert.equal(Boolean(result.infrastructureStall), calls === 2);
      assert.equal(readLog(file).records.length, calls === 1 ? 0 : 2);
      assert.equal(conductor.test_state.phase.repairRoundsUsed, 0);
    } finally { log.close(); fs.rmSync(dir, { recursive: true, force: true }); }
  });
}
