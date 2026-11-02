import assert from "node:assert/strict";
import { test } from "node:test";
import { registerHooks } from "node:module";
import { reduce } from "../../src/core/reduce.ts";
import { applyOwnerRequestResolved } from "../../src/core/owner-commands.ts";
import { next } from "../../src/core/next.ts";
import { baseState } from "./helpers.ts";
import type { Event, State } from "../../src/core/types.ts";

// Execute the production domain driver with effects replaced at its private
// boundaries. No socket, git, model, or copied driver algorithm is involved.
const seams = ["state", "log", "runDomainRounds", "recordRegression", "declaredTestCommands", "ownerRepairBaseFact",
  "applyEvent", "openPointsOn", "fixRound", "parkOpenPoints", "parkMergedCheck"];
const url = new URL("../../src/conductor.ts?budget-driver", import.meta.url).href;
const hooks = registerHooks({ load(target, context, load) {
  const result = load(target, context);
  if (target !== url) return result;
  return { ...result, source: result.source.toString().replace(new RegExp(`#(${seams.join("|")})\\b`, "g"), "test_$1") };
} });
const { Conductor } = await import(url);
hooks.deregister();

function applied(state: State, event: Event): State {
  const result = reduce(state, event);
  assert.ok(result.ok, result.ok ? "" : result.reason);
  return result.state;
}

function initial(): State {
  const state = baseState({ phase: "IMPLEMENTING", repairRoundsUsed: 2, repairRoundsGranted: 2,
    candidate: { sha: "C3", contractVersion: baseState().phase.contract.contractVersion },
    rounds: [{ round: 3, base: "B", lanes: ["a", "b"], candidates: [], votes: [], picked: { lane: "a", sha: "C3", votes: 3 } }],
    findings: [{ id: "F1", version: 1, phaseId: "p1", kind: "defect", severity: "blocking", raisedBy: "M", status: "open",
      domain: "booking", where: "booking/cache.ts:12", evidence: "wrong value", expected: "preserve prices", doneWhen: "test -f fixed", boundCandidateSha: "C3" }],
  });
  state.phase.contract.roundsAllowed = 3;
  state.phase.contract.domains = [{ name: "booking", globs: ["booking/**"] }];
  return state;
}

function grant(state: State, id: string): State {
  const request = { id, version: 1, phaseId: "p1", origin: "open_finding" as const, linkedFindingId: "F1", status: "open" as const,
    reason: "repair the point", options: [{ id: "repair", label: "repair" }] };
  state = applied(state, { type: "OWNER_REQUEST_OPENED", request });
  const phase = applyOwnerRequestResolved(state.phase, { type: "OWNER_REQUEST_RESOLVED", requestId: id, option: "repair",
    boundCandidateSha: state.phase.candidate!.sha, boundContractVersion: state.phase.contract.contractVersion, boundRequestVersion: 1 });
  return { ...state, phase };
}

function driver(state: State) {
  const plan = { title: "budget", repo: "/tmp", integrationBranch: "main", checks: [], rounds: 3, phases: [{ id: "p1" }] };
  const conductor = new Conductor({ runDir: "/tmp/tt-budget-driver", plan });
  const events: Event[] = [];
  const dispatches: number[] = [];
  const parks: string[] = [];
  conductor.test_state = state;
  conductor.test_log = { intent() {}, append() {} };
  conductor.test_recordRegression = () => {};
  conductor.test_declaredTestCommands = () => ({ checks: [] });
  conductor.test_ownerRepairBaseFact = async () => ({ exitCode: 1, timedOut: false });
  conductor.test_applyEvent = (event: Event) => { events.push(event); conductor.test_state = applied(conductor.test_state, event); };
  conductor.test_openPointsOn = () => [];
  conductor.test_fixRound = async (round: number) => { dispatches.push(round); return "stopped"; };
  conductor.test_parkOpenPoints = (_round, reason) => parks.push(reason);
  conductor.test_parkMergedCheck = (_round, reason) => parks.push(reason);
  return { conductor, events, dispatches, parks, run: () => conductor.test_runDomainRounds("test") };
}

function finish(state: State): State {
  const round = state.phase.rounds!.at(-1)!;
  state = applied(state, { type: "ROUND_FIXED", round: round.round, base: round.base,
    tasks: [{ domain: "booking", findingIds: ["F1"] }], merges: [{ sha: `D${round.round}` }], candidate: `C${round.round}` });
  return { ...state, phase: { ...state.phase, candidate: { sha: `C${round.round}`, contractVersion: state.phase.contract.contractVersion } } };
}

test("a recorded repair grant after the last original round dispatches round four", async () => {
  const d = driver(grant(initial(), "OR1"));
  await d.run();
  assert.deepEqual(d.dispatches, [4]);
  assert.deepEqual(d.parks, []);
  assert.equal(d.conductor.test_state.phase.repairRoundsUsed, 3);
});

test("a second recorded repair grant lifts the same driver allowance again", async () => {
  let state = grant(initial(), "OR1");
  for (let round = 4; round <= 6; round++) {
    const d = driver(state); await d.run();
    assert.deepEqual(d.dispatches, [round]);
    state = finish(d.conductor.test_state);
  }
  const d = driver(grant(state, "OR2")); await d.run();
  assert.deepEqual(d.dispatches, [7]);
  assert.equal(d.conductor.test_state.phase.repairRoundsGranted, 8);
  assert.equal(d.conductor.test_state.phase.repairRoundsUsed, 6);
});

test("exhaustion of an owner grant agrees with the state-table guard", async () => {
  let state = grant(initial(), "OR1");
  for (let round = 4; round <= 6; round++) {
    const d = driver(state); await d.run();
    assert.deepEqual(d.dispatches, [round]);
    state = finish(d.conductor.test_state);
  }
  const d = driver(state); await d.run();
  assert.deepEqual(d.dispatches, []);
  assert.match(d.parks[0], /round bound/);
  assert.equal(applied({ ...state, phase: { ...state.phase, phase: "REPAIRING" } }, { type: "REPAIR_BUDGET_EXHAUSTED" }).phase.phase, "AWAITING_OWNER");
  assert.equal(reduce({ ...state, phase: { ...state.phase, phase: "REPAIRING" } }, { type: "REPAIR_ATTEMPT_STARTED" }).ok, false);
});

for (const checkpoint of ["before ROUND_STARTED", "after ROUND_STARTED", "after ROUND_FIXED"]) test(`restart ${checkpoint} consumes each lane-free round once`, async () => {
  let state = grant(initial(), "OR1");
  state = applied({ ...state, phase: { ...state.phase, phase: "REPAIRING" } }, { type: "REPAIR_ATTEMPT_STARTED" });
  assert.equal(state.phase.repairRoundsUsed, 2, "the phase entry does not charge the domain round");
  if (checkpoint !== "before ROUND_STARTED") state = applied(state, { type: "ROUND_STARTED", round: 4, base: "C3", lanes: [] });
  if (checkpoint === "after ROUND_FIXED") state = applied(state, { type: "ROUND_FIXED", round: 4, base: "C3",
    tasks: [{ domain: "booking", findingIds: ["F1", "F-p1-owner-repair-OR1"] }], merges: [{ sha: "D4" }], candidate: "C4" });
  // JSON round-trip models a process restart: all budget inputs are recorded.
  const d = driver(JSON.parse(JSON.stringify(state))); await d.run();
  assert.deepEqual(d.dispatches, [4]);
  assert.equal(d.conductor.test_state.phase.repairRoundsUsed, 3);
  assert.equal(d.events.filter((e) => e.type === "ROUND_STARTED").length, checkpoint === "before ROUND_STARTED" ? 1 : 0);
  if (checkpoint !== "before ROUND_STARTED") {
    const duplicate = applied(d.conductor.test_state, { type: "ROUND_STARTED", round: 4, base: "C3", lanes: [] });
    assert.deepEqual(duplicate, d.conductor.test_state, "a duplicate start retains the checkpoint and spends nothing");
  }
  assert.equal(next({ ...d.conductor.test_state, phase: { ...d.conductor.test_state.phase, phase: "REPAIRING" } })[0].type, "repair_attempt_started");
});
