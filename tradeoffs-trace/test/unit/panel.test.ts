// Plan 04b: the blocker panel's pure rules — the vote count, the owner
// options a `block` majority puts to the owner, and how resolving one of
// those options is routed.
//
// Two round-3 review points are pinned here:
//   - a `block` vote must propose two or three DISTINCT options (advisory
//     B-2: a repeated id collapsed the owner's choice to one, and the old
//     fallback then offered two options that both did the opposite of what
//     they said);
//   - `accept_risk` on an escalated blocker settles the blocker and lets the
//     candidate stand, while every other option starts the repair it
//     describes — never a repair round for an option that says "stand".

import assert from "node:assert/strict";
import { test } from "node:test";

import { isRepairForcingOption } from "../../src/core/owner-requests.ts";
import {
  DEFAULT_BLOCKER_OPTIONS,
  panelOptionsFor,
  panelOutcome,
  panelSeatSettled,
  panelSeatsSettled,
} from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import { baseState, CV, makeMessage } from "./helpers.ts";

const K = CV();

function panelWithSeats(seats: Record<string, object>, decided?: object) {
  return { blockers: { "B-1": { seats, ...(decided ? { decided } : {}) } } };
}

const BLOCK_OPTIONS = [
  { id: "repair_cancel", label: "repair the cancel path (grant 3 rounds)" },
  { id: "accept_risk", label: "accept the risk and let the candidate stand" },
];

test("panel rules: a majority block escalates, a majority downgrade downgrades, anything else is incomplete", () => {
  const seat = (vote: "block" | "downgrade") => ({ dispatches: 1, vote, reason: "because" });
  assert.equal(panelOutcome({ seats: { "1": seat("block"), "2": seat("block"), "3": seat("downgrade") } }), "escalate");
  assert.equal(panelOutcome({ seats: { "1": seat("downgrade"), "2": seat("downgrade"), "3": seat("block") } }), "downgrade");
  assert.equal(panelOutcome({ seats: { "1": seat("block"), "2": seat("downgrade"), "3": seat("downgrade") } }), "downgrade");
  // 1-1 with a missing seat, and two unavailable seats: no majority.
  assert.equal(panelOutcome({ seats: { "1": seat("block"), "2": seat("downgrade") } }), "incomplete");
  assert.equal(
    panelOutcome({
      seats: { "1": { dispatches: 2, unavailable: true }, "2": { dispatches: 2, unavailable: true }, "3": seat("block") },
    }),
    "incomplete",
  );
});

test("panel rules: a seat is settled by a vote, or unavailable after its one retry", () => {
  assert.equal(panelSeatSettled({ dispatches: 1 }), false, "dispatched but silent is not settled");
  assert.equal(panelSeatSettled({ dispatches: 1, unavailable: true }), false, "a first loss is still re-dispatchable");
  assert.equal(panelSeatSettled({ dispatches: 2, unavailable: true }), true);
  assert.equal(panelSeatSettled({ dispatches: 2, vote: "block", reason: "x" }), true);
  assert.equal(panelSeatsSettled({ seats: {} }), false, "an empty panel has not settled its three seats");
  assert.equal(
    panelSeatsSettled({
      seats: { "1": { dispatches: 1, vote: "block" }, "2": { dispatches: 2, unavailable: true }, "3": { dispatches: 2, unavailable: true } },
    }),
    true,
  );
});

test("panel rules: the escalation options are every block vote's own options, deduped and capped at three", () => {
  const seats = {
    "1": { dispatches: 1, vote: "block", reason: "x", options: BLOCK_OPTIONS },
    "2": {
      dispatches: 1,
      vote: "block",
      reason: "y",
      options: [
        BLOCK_OPTIONS[1],
        { id: "escalate_to_vendor", label: "raise it with the vendor" },
      ],
    },
    "3": { dispatches: 1, vote: "downgrade", reason: "z" },
  };
  assert.deepEqual(panelOptionsFor({ seats }), [
    BLOCK_OPTIONS[0],
    BLOCK_OPTIONS[1],
    { id: "escalate_to_vendor", label: "raise it with the vendor" },
  ]);
});

test("panel rules: the defensive fallback options do what they say (advisory B-2)", () => {
  // Reachable only from a degenerate vote, which the reducer now refuses; the
  // pair must still match `accept_risk`/repair-forcing, so neither label lies.
  assert.deepEqual(
    DEFAULT_BLOCKER_OPTIONS.map((o) => o.id),
    ["accept_risk", "repair"],
  );
  assert.equal(isRepairForcingOption("blocker_panel", "accept_risk"), false, "accepting the risk starts no repair");
  assert.equal(isRepairForcingOption("blocker_panel", "repair"), true);
  assert.equal(isRepairForcingOption("blocker_panel", "repair_cancel"), true);
});

test("panel rules: a block vote with a repeated option id or label is refused, whole", () => {
  const message = makeMessage({ id: "B-1", type: "blocker", state: "raw", raisedAsBlocker: true, boundCandidateSha: "C1", boundContractVersion: K });
  const state = baseState({
    phase: "EVALUATING",
    candidate: { sha: "C1", contractVersion: K },
    messages: [message],
    panel: panelWithSeats({}),
  });
  const sameId = reduce(state, {
    type: "PANEL_VOTE",
    blockerId: "B-1",
    seat: 1,
    vote: "block",
    reason: "x",
    options: [
      { id: "same", label: "one thing" },
      { id: "same", label: "another thing" },
    ],
  });
  assert.equal(sameId.ok, false, "two options sharing an id are not a choice");
  const sameLabel = reduce(state, {
    type: "PANEL_VOTE",
    blockerId: "B-1",
    seat: 1,
    vote: "block",
    reason: "x",
    options: [
      { id: "a", label: "the same words" },
      { id: "b", label: "the same words" },
    ],
  });
  assert.equal(sameLabel.ok, false, "two options reading the same are not a choice");
  const good = reduce(state, { type: "PANEL_VOTE", blockerId: "B-1", seat: 1, vote: "block", reason: "x", options: BLOCK_OPTIONS });
  assert.equal(good.ok, true, !good.ok ? good.reason : "");
});
