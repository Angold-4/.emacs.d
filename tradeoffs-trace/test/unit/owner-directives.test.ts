// Plan 01i (01_ref_design.md §8, D5): the pure half of owner directives —
// the record-only events the phase folds back after a restart, the withdraw
// text parser, the prompt section, and the contract statement the reviewer
// turn-2 prompt carries.

import assert from "node:assert/strict";
import { test } from "node:test";

import {
  DIRECTIVE_BINDING_STATEMENT,
  allocateDirectiveId,
  buildReviewerPrompt,
  buildWorkerPrompt,
  directiveLines,
  parseWithdrawInput,
} from "../../src/conductor.ts";
import { reduce } from "../../src/core/reduce.ts";
import { initialProgramState, nextProgramDirectiveId, reduceProgram } from "../../src/core/program.ts";
import type { OwnerDirective, State } from "../../src/core/types.ts";
import { baseState } from "./helpers.ts";

function directive(overrides: Partial<OwnerDirective> = {}): OwnerDirective {
  return {
    id: "OD-1",
    seq: 1,
    text: "the 14 exchange-state-machine failures are pre-existing, not yours",
    scope: "phase",
    status: "in-force",
    commandId: "cmd-1",
    at: "2026-09-25T00:00:00.000Z",
    targets: ["worker", "M"],
    deliveries: {},
    ...overrides,
  };
}

function step(state: State, event: unknown): State {
  const result = reduce(state, event);
  assert.ok(result.ok, `reduce rejected ${JSON.stringify(event)}: ${!result.ok ? result.reason : ""}`);
  return result.state;
}

test("owner-directives: DIRECTIVE_ADDED appends the record and a replay does not duplicate it", () => {
  let state = baseState();
  state = step(state, { type: "DIRECTIVE_ADDED", directive: directive() });
  assert.equal(state.phase.ownerDirectives?.length, 1);
  assert.equal(state.phase.ownerDirectives?.[0].id, "OD-1");
  // The same id twice is rejected: the conductor never mints two OD-1s, and
  // a malformed replay must not silently shadow the recorded ruling.
  const again = reduce(state, { type: "DIRECTIVE_ADDED", directive: directive({ text: "different" }) });
  assert.equal(again.ok, false);
});

test("owner-directives: a directive must carry text and a known scope", () => {
  const noText = reduce(baseState(), { type: "DIRECTIVE_ADDED", directive: directive({ text: "  " }) });
  assert.equal(noText.ok, false);
  const badScope = reduce(baseState(), {
    type: "DIRECTIVE_ADDED",
    directive: directive({ scope: "galaxy" as unknown as OwnerDirective["scope"] }),
  });
  assert.equal(badScope.ok, false);
});

test("owner-directives: DIRECTIVE_DELIVERED records each target, new record wins", () => {
  let state = step(baseState(), { type: "DIRECTIVE_ADDED", directive: directive() });
  state = step(state, { type: "DIRECTIVE_DELIVERED", directiveId: "OD-1", target: "worker", state: "delivered" });
  state = step(state, { type: "DIRECTIVE_DELIVERED", directiveId: "OD-1", target: "M", state: "delivered" });
  state = step(state, { type: "DIRECTIVE_DELIVERED", directiveId: "OD-1", target: "A", state: "delivery-uncertain" });
  assert.deepEqual(state.phase.ownerDirectives?.[0].deliveries, {
    worker: "delivered",
    M: "delivered",
    A: "delivery-uncertain",
  });
  const unknown = reduce(state, { type: "DIRECTIVE_DELIVERED", directiveId: "OD-9", target: "M", state: "delivered" });
  assert.equal(unknown.ok, false);
});

test("owner-directives: withdrawing keeps the record and refuses an unknown or already-withdrawn id", () => {
  let state = step(baseState(), { type: "DIRECTIVE_ADDED", directive: directive() });
  state = step(state, { type: "DIRECTIVE_WITHDRAWN", directiveId: "OD-1", at: "2026-09-25T01:00:00.000Z" });
  assert.equal(state.phase.ownerDirectives?.[0].status, "withdrawn");
  assert.equal(state.phase.ownerDirectives?.[0].withdrawnAt, "2026-09-25T01:00:00.000Z");
  const again = reduce(state, { type: "DIRECTIVE_WITHDRAWN", directiveId: "OD-1" });
  assert.equal(again.ok, false);
  const unknown = reduce(state, { type: "DIRECTIVE_WITHDRAWN", directiveId: "OD-7" });
  assert.equal(unknown.ok, false);
});

test("owner-directives: `withdraw OD-n`/`ODP-n` is recognised even with trailing prose; a malformed one is refused, never inverted", () => {
  assert.deepEqual(parseWithdrawInput("withdraw OD-1"), { kind: "withdraw", id: "OD-1" });
  assert.deepEqual(parseWithdrawInput("  Withdraw od-12  "), { kind: "withdraw", id: "OD-12" });
  assert.deepEqual(parseWithdrawInput("withdraw ODP-3"), { kind: "withdraw", id: "ODP-3" });
  // The owner's natural phrasing retracts OD-1; it must not become a new
  // binding directive that leaves OD-1 in force.
  assert.deepEqual(parseWithdrawInput("withdraw OD-1 because it is stale"), { kind: "withdraw", id: "OD-1" });
  // A withdrawal that names no id is refused, never recorded as a ruling.
  assert.equal(parseWithdrawInput("withdraw")?.kind, "malformed");
  assert.equal(parseWithdrawInput("withdraw the Stork exception")?.kind, "malformed");
  // Not a withdrawal at all: an ordinary directive.
  assert.equal(parseWithdrawInput("please withdraw OD-1"), undefined);
  assert.equal(parseWithdrawInput("OD-1"), undefined);
});

test("owner-directives: ids never collide across levels — a program ruling keeps its ODP-n even when OD-n exists locally", () => {
  const local = [directive({ id: "OD-1", seq: 1 })];
  // The phase's own next id skips nothing and renumbers nothing.
  assert.deepEqual(allocateDirectiveId([]), { id: "OD-1", seq: 1 });
  assert.deepEqual(allocateDirectiveId(local), { id: "OD-2", seq: 2 });
  // A program-wide ruling is stored under the program's own id, so `withdraw
  // ODP-1` names it and `withdraw OD-1` names the node's own ruling.
  assert.deepEqual(allocateDirectiveId(local, "ODP-1"), { id: "ODP-1", seq: 1 });
  // …and a program id never advances the phase's own counter.
  assert.deepEqual(allocateDirectiveId([directive({ id: "ODP-1", seq: 1 })]), { id: "OD-1", seq: 1 });
  assert.deepEqual(allocateDirectiveId([directive({ id: "ODP-1", seq: 1 }), directive({ id: "OD-1", seq: 1 })]), {
    id: "OD-2",
    seq: 2,
  });
});

test("owner-directives: program directives are numbered in their own ODP namespace", () => {
  let state = initialProgramState([]);
  assert.equal(nextProgramDirectiveId(state), "ODP-1");
  state = reduceProgram(state, {
    type: "DIRECTIVE_ADDED",
    directive: { id: "ODP-1", text: "no vendor adapters", at: "2026-09-25T00:00:00.000Z" },
  });
  assert.equal(nextProgramDirectiveId(state), "ODP-2");
  // A replay of the same id is a no-op, and a withdrawal keeps the record.
  state = reduceProgram(state, {
    type: "DIRECTIVE_ADDED",
    directive: { id: "ODP-1", text: "a duplicate", at: "2026-09-25T00:00:00.000Z" },
  });
  assert.equal(state.directives?.length, 1);
  state = reduceProgram(state, { type: "DIRECTIVE_WITHDRAWN", directiveId: "ODP-1" });
  assert.equal(state.directives?.[0].withdrawn, true);
  assert.equal(state.directives?.[0].text, "no vendor adapters");
});

test("owner-directives: the prompt section lists every directive in force, verbatim, newest last, and omits withdrawn ones", () => {
  const directives: OwnerDirective[] = [
    directive({ id: "OD-1", text: "first ruling" }),
    directive({ id: "OD-2", seq: 2, text: "second ruling", scope: "program" }),
    directive({ id: "OD-3", seq: 3, text: "withdrawn ruling", status: "withdrawn" }),
  ];
  const lines = directiveLines(directives);
  assert.equal(lines[0], "");
  assert.equal(lines[1], "Owner directives (binding):");
  assert.equal(lines[2], "- OD-1: first ruling");
  assert.equal(lines[3], "- OD-2 (whole program): second ruling");
  assert.equal(lines.length, 4, "a withdrawn directive is omitted");
  assert.deepEqual(directiveLines([]), []);
  assert.deepEqual(directiveLines(undefined), []);
});

test("owner-directives: the worker prompt carries every in-force directive verbatim", () => {
  const prompt = buildWorkerPrompt(
    baseState().phase.contract,
    undefined,
    undefined,
    undefined,
    [],
    [],
    [directive({ id: "OD-1", text: "fix the Stork live link in this phase" })],
  );
  assert.match(prompt, /Owner directives \(binding\):/);
  assert.ok(prompt.includes("OD-1: fix the Stork live link in this phase"));
});

test("owner-directives: the reviewer turn-2 prompt states that directives are binding and following one is not a defect", () => {
  // The turn-2 prompt is built from live state, so this exercises the same
  // statement through the exported reviewer prompt, which shares it.
  const phase = baseState().phase;
  const prompt = buildReviewerPrompt(phase, "A", [], [directive()]);
  assert.ok(prompt.includes("OD-1: the 14 exchange-state-machine failures are pre-existing, not yours"));
  assert.ok(prompt.includes(DIRECTIVE_BINDING_STATEMENT), "the binding statement must be in the prompt");
  assert.match(prompt, /blocking contract finding that cites the directive id/);
  assert.match(prompt, /cannot be faulted for doing so/);
});
