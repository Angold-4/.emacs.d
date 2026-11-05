import assert from "node:assert/strict";
import { test } from "node:test";
import { registerHooks } from "node:module";
import { acceptanceBlockers } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import { baseState } from "./helpers.ts";
import type { Event } from "../../src/core/types.ts";

const seams = ["recordRoundsExhausted", "roundsBoundReported", "plan", "recordBound", "applyEvent"];
const url = new URL("../../src/conductor.ts?rounds-bound-driver", import.meta.url).href;
const hooks = registerHooks({ load(target, context, load) {
  const result = load(target, context);
  return target === url ? { ...result, source: result.source.toString().replace(new RegExp(`#(${seams.join("|")})\\b`, "g"), "test_$1") } : result;
} });
const { Conductor } = await import(url);
hooks.deregister();

test("the production rounds bound reports exactly the acceptance list below the hard ceiling, once", () => {
  const state = baseState({ phase: "CHECKING", repairRoundsGranted: 0 });
  state.phase.contract.roundsAllowed = 1;
  state.phase.candidate = { sha: "C1", contractVersion: state.phase.contract.contractVersion };
  const parked = reduce(state, { type: "CHECKS_FAILED" });
  assert.ok(parked.ok);
  const conductor = Object.create(Conductor.prototype);
  conductor.test_plan = { rounds: 1 };
  const bounds: unknown[][] = [];
  const events: Event[] = [];
  conductor.test_recordBound = (...args: unknown[]) => bounds.push(args);
  conductor.test_applyEvent = (event: Event) => events.push(event);
  conductor.test_recordRoundsExhausted(parked.state.phase);
  conductor.test_recordRoundsExhausted(parked.state.phase);
  assert.deepEqual(bounds, [["rounds", "p1", 1, 0]]);
  assert.deepEqual(events, [{ type: "LEAD_CALLED", reasons: acceptanceBlockers(parked.state.phase).map((b) => b.detail) }]);
  assert.equal(parked.state.phase.ownerRequests[0].reason, "the checks kept failing");
});
