import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";
import { registerHooks } from "node:module";
import { EventLog, readLog } from "../../src/effects/log.ts";
import { reduce } from "../../src/core/reduce.ts";
import { baseState } from "./helpers.ts";
import type { Event } from "../../src/core/types.ts";

const seams = ["runChecks", "reconcileOne", "checkWithInfrastructureRetry", "killRecordedGroup", "applyEvent",
  "openCheckWindow", "state", "paths", "log", "closed", "stopRequested", "infrastructureLoad1"];
const url = new URL("../../src/conductor.ts?check-recovery-driver", import.meta.url).href;
const hooks = registerHooks({ load(target, context, load) {
  const result = load(target, context);
  if (target !== url) return result;
  return { ...result, source: result.source.toString()
    .replaceAll("await acquireCheckSlot(this.#checkLockPath, this.#checkSlots, this.#runDir)", "await this.test_acquireCheckSlot()")
    .replace(new RegExp(`#(${seams.join("|")})\\b`, "g"), "test_$1") };
} });
const { Conductor } = await import(url);
hooks.deregister();

test("a recovered check waiting for its slot releases that slot if the conductor stops before acquisition", async () => {
  const conductor = Object.create(Conductor.prototype);
  let acquire!: (lock: { release: () => Promise<void> }) => void;
  let released = 0;
  conductor.test_acquireCheckSlot = () => new Promise((resolve) => { acquire = resolve; });
  conductor.test_openCheckWindow = () => assert.fail("a stopped check must not open a check window");
  conductor.test_log = { intent: () => assert.fail("the stopped conductor's log is already closed") };
  const pending = conductor.test_runChecks("recovered-check", "C1");
  conductor.test_closed = true;
  conductor.test_stopRequested = true;
  acquire({ release: async () => { released++; } });
  await pending;
  assert.equal(released, 1, "the lock helper must not outlive the stopped conductor");
});

test("ordinary orphan recovery kills first and re-runs; only a durable infrastructure retry parks on interruption", async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-check-recovery-driver-"));
  const file = path.join(dir, "events.jsonl");
  const log = new EventLog(file);
  try {
    const conductor = Object.create(Conductor.prototype);
    conductor.test_state = baseState({ phase: "CHECKING", inFlight: { run_checks: { actionId: "old-check" } } });
    conductor.test_state.phase.candidate = { sha: "C1", contractVersion: conductor.test_state.phase.contract.contractVersion };
    conductor.test_paths = { events: file };
    conductor.test_log = log;
    conductor.test_infrastructureLoad1 = () => 100;
    conductor.test_closed = false;
    conductor.test_stopRequested = false;
    log.intent("old-check", { candidateSha: "C1" });
    log.intent("check-sh-old-check-123", { pgid: 123 });
    const order: string[] = [];
    conductor.test_killRecordedGroup = async (pgid: number) => { assert.equal(pgid, 123); order.push("kill"); };
    conductor.test_applyEvent = (event: Event) => {
      order.push(event.type);
      const result = reduce(conductor.test_state, event);
      assert.ok(result.ok);
      conductor.test_state = result.state;
    };
    await conductor.test_reconcileOne("run_checks", "old-check");
    assert.deepEqual(order, ["kill", "CHECKS_INTERRUPTED"]);
    const pass = { exitCode: 0, signal: null, timedOut: false, cancelled: false, output: "ok", signalsSent: [] };
    let executions = 0;
    const run = async () => { executions++; return pass; };
    const result = await conductor.test_checkWithInfrastructureRetry("C1", "fake check", "round", dir, 700, run);
    assert.equal(executions, 1, "crash-recovery completion is not a durable infrastructure retry");
    assert.equal(result.infrastructureStall, undefined);
    assert.equal(readLog(file).records.some((r) => r.kind === "check_infrastructure_retry"), false);
    log.append("check_infrastructure_retry", { key: "p1:C1:round:fake check", retry: {
      load1: 100, reason: "timeout under high host load", first: { ...pass, exitCode: null, timedOut: true },
    } });
    const interrupted = await conductor.test_checkWithInfrastructureRetry("C1", "fake check", "round", dir, 700, run);
    assert.equal(executions, 1, "a real interrupted infrastructure retry is not attempted again");
    assert.match(interrupted.infrastructureStall, /infrastructure retry interrupted/);
  } finally { log.close(); fs.rmSync(dir, { recursive: true, force: true }); }
});
