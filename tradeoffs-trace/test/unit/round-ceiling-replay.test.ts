import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";
import { test } from "node:test";
import { registerHooks } from "node:module";
import { initialState, rebuildState, rebuildTimeline, type RunPlanFile } from "../../src/conductor.ts";
import { roundCeiling } from "../../src/core/rounds.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { Event, State } from "../../src/core/types.ts";

const seams = ["dispatch", "state", "applyEvent", "reconcileInFlight", "paths", "killRecordedGroup"];
const url = new URL("../../src/conductor.ts?ceiling-resume", import.meta.url).href;
const hooks = registerHooks({ load(target, context, load) {
  const result = load(target, context);
  return target === url ? { ...result, source: result.source.toString().replace(new RegExp(`#(${seams.join("|")})\\b`, "g"), "test_$1") } : result;
} });
const { Conductor } = await import(url);
hooks.deregister();

const plan: RunPlanFile = { title: "legacy", repo: ".", integrationBranch: "main", checks: [], phases: [
  { id: "p", goal: "finish", acceptance: [], checks: [], boundaries: [], reserved: [] },
] };

test("legacy grants replay strictly, but resuming an over-ceiling phase parks before dispatch", async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-ceiling-replay-"));
  try {
    let state = initialState("r", plan.phases[0], "H");
    state.phase.contract.roundCeiling = Number.MAX_SAFE_INTEGER;
    const records: unknown[] = [{ kind: "init", seq: 1, ts: new Date().toISOString(), event: { runId: "r", integrationHead: "H" } }];
    const apply = (event: Event) => {
      const result = reduce(state, event);
      assert.ok(result.ok, result.ok ? "" : result.reason);
      state = result.state;
      records.push({ kind: "event", seq: records.length + 1, ts: new Date().toISOString(), event });
    };
    apply({ type: "ATTEMPT_STARTED", baselineNeeded: false });
    for (let round = 1; round < 8; round++) {
      apply({ type: "ATTEMPT_NO_SUBMISSION" });
      if (state.phase.phase === "AWAITING_OWNER") apply({ type: "OWNER_CORRECTION", correctionId: `owner-${round}`, text: "repair the plan" });
      apply({ type: "REPAIR_ATTEMPT_STARTED" });
    }
    fs.writeFileSync(path.join(dir, "events.jsonl"), records.map((r) => JSON.stringify(r)).join("\n") + "\n");
    const restored = rebuildState(dir, plan);
    assert.equal(restored.phase.repairRoundsUsed, 7);
    assert.equal(restored.phase.repairRoundsGranted, 8);
    assert.equal(roundCeiling(restored.phase.contract), 6);
    assert.equal(rebuildTimeline(dir, plan).state.phase.repairRoundsUsed, 7);
    const conductor = Object.create(Conductor.prototype);
    conductor.test_state = restored;
    const emitted: Event[] = [];
    conductor.test_applyEvent = (event: Event) => {
      emitted.push(event);
      const result = reduce(conductor.test_state as State, event);
      assert.ok(result.ok);
      conductor.test_state = result.state;
    };
    conductor.test_dispatch({ type: "dispatch_worker" });
    assert.deepEqual(emitted.map((e) => e.type), ["PHASE_STUCK"]);
    assert.equal(conductor.test_state.phase.phase, "AWAITING_OWNER");
    conductor.test_state = { ...restored, phase: { ...restored.phase, phase: "REVIEWING", inFlight: {
      review_M: { actionId: "review-m" }, review_A: { actionId: "review-a" },
    } } };
    conductor.test_paths = { events: path.join(dir, "events.jsonl") };
    conductor.test_killRecordedGroup = async () => undefined;
    await conductor.test_reconcileInFlight();
    assert.equal(conductor.test_state.phase.phase, "AWAITING_OWNER");
    assert.deepEqual(conductor.test_state.phase.inFlight, {});
    assert.deepEqual(emitted.map((e) => e.type), ["PHASE_STUCK", "PHASE_STUCK"]);
    records.push({ kind: "round_ceiling", seq: records.length + 1, ts: new Date().toISOString(), event: { roundCeiling: 6 } });
    records.push({ kind: "event", seq: records.length + 1, ts: new Date().toISOString(), event: emitted[0] });
    fs.writeFileSync(path.join(dir, "events.jsonl"), records.map((r) => JSON.stringify(r)).join("\n") + "\n");
    assert.equal(rebuildState(dir, plan).phase.phase, "AWAITING_OWNER");
    assert.equal(rebuildTimeline(dir, plan).state.phase.phase, "AWAITING_OWNER");
    // A new writer's init freezes the cap from its very first round.
    (records[0] as { event: object }).event = { runId: "r", integrationHead: "H", roundCeiling: 6 };
    fs.writeFileSync(path.join(dir, "events.jsonl"), records.map((r) => JSON.stringify(r)).join("\n") + "\n");
    assert.throws(() => rebuildState(dir, plan), /no longer reduces/);
  } finally { fs.rmSync(dir, { recursive: true, force: true }); }
});
