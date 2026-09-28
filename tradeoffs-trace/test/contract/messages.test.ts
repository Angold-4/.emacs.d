// Contract v1 acceptance: the message state machine and the settled ledger.
//
//  1. Every row of MESSAGE_TRANSITIONS has a fixture; the event moves the
//     message to the row's `to` state.
//  2. An event with no matching row is rejected by reduce() (OWNER_VERDICT on
//     a dropped message; OWNER_VERDICT on a raw, pre-freeze message says
//     "not yet frozen").
//  3. Carry: an unchanged settlement survives a version bump; a changed one,
//     or any message after a contract amendment, is marked invalidated.
//  4. A verdict naming the pre-carry version of an unchanged message is
//     applied; of a changed one rejected.
//  5. refuse during REVIEWING raises an owner blocking finding and accept()
//     cannot hold; refuse after DONE is a follow-up that changes no phase
//     state; accept settles only the version it named.

import assert from "node:assert/strict";
import { test } from "node:test";

import { contentHashOf, ledgerEntries, MESSAGE_TRANSITIONS } from "../../src/core/messages.ts";
import { accept } from "../../src/core/predicate.ts";
import { reduce } from "../../src/core/reduce.ts";
import type { Event, Message, State } from "../../src/core/types.ts";
import { approvingReview, baseState, CV, makeMessage } from "../unit/helpers.ts";

const K = CV();
const C1 = "C1";

function withMessage(message: Message, phase = baseState().phase): State {
  return baseState({ ...phase, messages: [message] });
}

function binding(message: Message) {
  return {
    messageId: message.id,
    boundCandidateSha: message.boundCandidateSha,
    boundContractVersion: message.boundContractVersion,
    boundRecordVersion: message.messageVersion,
  };
}

interface RowFixture {
  state: State;
  event: Event;
}

/** One fixture per MESSAGE_TRANSITIONS row, keyed by the row's id. */
const FIXTURES: Record<string, RowFixture> = {
  "message-raised": {
    state: baseState(),
    event: { type: "MESSAGE_RAISED", message: makeMessage({ state: "raw" }) },
  },
  "message-published": {
    state: withMessage(makeMessage({ state: "raw" })),
    event: { type: "MESSAGE_PUBLISHED", messageId: "T-1", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-merged": {
    state: withMessage(makeMessage({ state: "raw" })),
    event: { type: "MESSAGE_MERGED", messageId: "T-1", by: "evaluator", reason: "duplicate of T-9", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-dropped": {
    state: withMessage(makeMessage({ state: "raw" })),
    event: { type: "MESSAGE_DROPPED", messageId: "T-1", by: "evaluator", reason: "not reviewable", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-dropped-published": {
    state: withMessage(makeMessage({ state: "published" })),
    event: { type: "MESSAGE_DROPPED", messageId: "T-1", by: "panel", reason: "the panel did not keep it", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "owner-verdict-accept": {
    state: withMessage(makeMessage({ state: "published" })),
    event: { type: "OWNER_VERDICT", messageId: "T-1", verdict: "accept", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "owner-verdict-refuse": {
    state: withMessage(makeMessage({ state: "published" })),
    event: { type: "OWNER_VERDICT", messageId: "T-1", verdict: "refuse", reason: "the choice is wrong for the goal", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-superseded-published": {
    state: withMessage(makeMessage({ state: "published" })),
    event: { type: "MESSAGE_SUPERSEDED", messageId: "T-1", reason: "replaced by T-2", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-resolved-published": {
    state: withMessage(makeMessage({ state: "published" })),
    event: { type: "MESSAGE_RESOLVED", messageId: "T-1", by: "evaluator", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-superseded-refused": {
    state: withMessage(makeMessage({ state: "refused" })),
    event: { type: "MESSAGE_SUPERSEDED", messageId: "T-1", reason: "replaced by T-2", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-superseded-accepted": {
    state: withMessage(makeMessage({ state: "accepted" })),
    event: { type: "MESSAGE_SUPERSEDED", messageId: "T-1", reason: "its record was superseded", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-superseded-merged": {
    state: withMessage(makeMessage({ state: "merged" })),
    event: { type: "MESSAGE_SUPERSEDED", messageId: "T-1", reason: "its record was superseded", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-superseded-dropped": {
    state: withMessage(makeMessage({ state: "dropped" })),
    event: { type: "MESSAGE_SUPERSEDED", messageId: "T-1", reason: "its record was superseded", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
  "message-resolved-refused": {
    state: withMessage(makeMessage({ state: "refused" })),
    event: { type: "MESSAGE_RESOLVED", messageId: "T-1", by: "owner", boundCandidateSha: C1, boundContractVersion: K, boundRecordVersion: 1 },
  },
};

test("every MESSAGE_TRANSITIONS row has a fixture and moves to its to-state", () => {
  for (const row of MESSAGE_TRANSITIONS) {
    const fixture = FIXTURES[row.id];
    assert.ok(fixture, `no fixture for MESSAGE_TRANSITIONS row '${row.id}'`);
    const result = reduce(fixture.state, fixture.event);
    assert.equal(result.ok, true, `row '${row.id}' was rejected: ${result.ok ? "" : result.reason}`);
    const messages = result.state.phase.messages ?? [];
    const message = messages.find((m) => m.id === (fixture.event as { messageId?: string }).messageId) ?? messages[0];
    assert.ok(message, `row '${row.id}' produced no message`);
    assert.equal(message.state, row.to, `row '${row.id}' landed in '${message.state}', expected '${row.to}'`);
  }
});

test("every fixture key names a real MESSAGE_TRANSITIONS row (no orphan fixtures)", () => {
  const ids = new Set(MESSAGE_TRANSITIONS.map((r) => r.id));
  for (const key of Object.keys(FIXTURES)) assert.ok(ids.has(key), `fixture '${key}' has no row`);
});

test("an event with no matching row is rejected by reduce (OWNER_VERDICT on dropped)", () => {
  const dropped = makeMessage({ state: "dropped" });
  const result = reduce(withMessage(dropped), {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "accept",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, false);
  assert.match(result.ok ? "" : result.reason, /no rule from message T-1's state 'dropped'/);
});

test("a verdict on a raw, pre-freeze message is rejected with 'not yet frozen'", () => {
  const raw = makeMessage({ state: "raw" });
  const result = reduce(withMessage(raw), {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "accept",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, false);
  assert.match(result.ok ? "" : result.reason, /not yet frozen/);
});

test("a stale candidateSha is rejected with a visible reason", () => {
  const published = makeMessage({ state: "published" });
  const result = reduce(withMessage(published), {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "accept",
    boundCandidateSha: "C9",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, false);
  assert.match(result.ok ? "" : result.reason, /candidate/);
});

test("accept settles only the version it named", () => {
  const published = makeMessage({ state: "published" });
  const result = reduce(withMessage(published), {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "accept",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, true);
  const message = result.state.phase.messages![0];
  assert.equal(message.state, "accepted");
  assert.equal(message.settlement?.messageVersion, 1);
  assert.equal(message.settlement?.settledBy, "owner");
  assert.equal(message.settlement?.contentHash, message.contentHash);
});

test("refuse during REVIEWING raises an owner blocking finding and accept() cannot hold", () => {
  const published = makeMessage({ state: "published" });
  const reviewed = baseState({
    phase: "REVIEWING",
    candidate: { sha: C1, contractVersion: K },
    checks: { candidateSha: C1, passed: true },
    probe: { candidateSha: C1, head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: approvingReview("M", C1, K) },
      A: { review: approvingReview("A", C1, K) },
      B: { review: approvingReview("B", C1, K) },
    },
    messages: [published],
  });
  assert.equal(accept(reviewed.phase, C1, K), true);
  const result = reduce(reviewed, {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "refuse",
    reason: "the batch hides per-request latency",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, true);
  const finding = result.state.phase.findings.find((f) => f.raisedBy === "owner");
  assert.ok(finding, "expected an owner-raised finding");
  assert.equal(finding!.severity, "blocking");
  assert.equal(finding!.status, "open");
  assert.match(finding!.evidence, /batch hides per-request latency/);
  assert.equal(accept(result.state.phase, C1, K), false);
});

test("refuse after DONE is a follow-up that changes no phase state", () => {
  const published = makeMessage({ state: "published" });
  const done = baseState({ phase: "DONE", candidate: { sha: C1, contractVersion: K }, messages: [published] });
  const result = reduce(done, {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "refuse",
    reason: "worth revisiting later",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, true);
  assert.equal(result.state.phase.phase, "DONE");
  assert.equal(result.state.phase.findings.length, 0);
  assert.equal(result.state.phase.messages![0].followUp, true);
});

function acceptedOnce(overrides: Partial<Message> = {}): { state: State; before: Message } {
  const before = makeMessage({ state: "published", ...overrides });
  const result = reduce(withMessage(before), {
    type: "OWNER_VERDICT",
    messageId: before.id,
    verdict: "accept",
    boundCandidateSha: before.boundCandidateSha,
    boundContractVersion: before.boundContractVersion,
    boundRecordVersion: before.messageVersion,
  });
  assert.equal(result.ok, true, result.ok ? "" : result.reason);
  return { state: result.state, before };
}

test("refuse before DONE (CHECKING) raises an owner blocking finding", () => {
  const published = makeMessage({ state: "published" });
  const checking = baseState({ phase: "CHECKING", candidate: { sha: C1, contractVersion: K }, messages: [published] });
  const result = reduce(checking, {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "refuse",
    reason: "not the trade-off the goal needed",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, true);
  assert.equal(result.state.phase.phase, "CHECKING");
  assert.ok(
    result.state.phase.findings.some((f) => f.raisedBy === "owner" && f.severity === "blocking" && f.status === "open"),
    "a pre-DONE refusal must raise an owner blocking finding",
  );
});

test("ledger: an invalidated entry keeps who settled it, its state and its bindings", () => {
  const raw = makeMessage({ state: "raw" });
  const merged = reduce(withMessage(raw), {
    type: "MESSAGE_MERGED",
    messageId: "T-1",
    by: "evaluator",
    reason: "duplicate of T-9",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(merged.ok, true, merged.ok ? "" : merged.reason);
  const amended: State = {
    ...merged.state,
    phase: { ...merged.state.phase, contract: { ...merged.state.phase.contract, contractVersion: CV(2, "b".repeat(64)) } },
  };
  const carried = reduce(amended, {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: raw.contentHash,
    unchanged: true,
  });
  assert.equal(carried.ok, true, carried.ok ? "" : carried.reason);
  const entry = ledgerEntries(carried.state.phase.messages!).find((e) => e.messageId === "T-1")!;
  assert.equal(entry.settledBy, "evaluator");
  assert.equal(entry.state, "merged");
  assert.equal(entry.candidateSha, C1);
  assert.equal(entry.invalidated?.reason, "contract amended");
});

test("supersede keeps a prior settlement in the ledger, marked superseded", () => {
  const raw = makeMessage({ state: "raw" });
  const pub = reduce(withMessage(raw), {
    type: "MESSAGE_PUBLISHED",
    messageId: "T-1",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(pub.ok, true, pub.ok ? "" : pub.reason);
  const refused = reduce(pub.state, {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "refuse",
    reason: "wrong choice",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(refused.ok, true, refused.ok ? "" : refused.reason);
  const superseded = reduce(refused.state, {
    type: "MESSAGE_SUPERSEDED",
    messageId: "T-1",
    reason: "its record was withdrawn",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(superseded.ok, true, superseded.ok ? "" : superseded.reason);
  const message = superseded.state.phase.messages![0];
  assert.equal(message.state, "superseded");
  assert.equal(message.settlement?.settledBy, "owner");
  const entry = ledgerEntries(superseded.state.phase.messages!).find((e) => e.messageId === "T-1")!;
  assert.equal(entry.state, "refused");
  assert.equal(entry.supersededBy, "its record was withdrawn");
});

test("carry: a later unchanged carry never rebinds an invalidated settlement", () => {
  const { state, before } = acceptedOnce();
  const changed = { type: "tradeoff" as const, title: "a rewritten choice", summary: "new summary", context: "new context", evidence: ["new evidence"], planRef: "p" };
  const invalidated = reduce(state, {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: contentHashOf(changed),
    unchanged: false,
    content: changed,
  });
  assert.equal(invalidated.ok, true);
  // A second, unchanged carry must NOT rebase the invalidated settlement.
  const again = reduce(invalidated.state, {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: "C2",
    toCandidate: "C3",
    fromVersion: 2,
    toVersion: 3,
    contentHash: contentHashOf(changed),
    unchanged: true,
  });
  assert.equal(again.ok, true, again.ok ? "" : again.reason);
  const message = again.state.phase.messages![0];
  assert.equal(message.settlement?.candidateSha, C1);
  assert.equal(message.settlement?.messageVersion, 1);
  assert.equal(message.settlement?.contentHash, before.contentHash);
  assert.equal(message.invalidated?.reason, "content changed");
  const entry = ledgerEntries(again.state.phase.messages!).find((e) => e.messageId === "T-1")!;
  assert.equal(entry.candidateSha, C1);
  assert.equal(entry.messageVersion, 1);
});

test("carry: an unchanged settlement stays settled on the new candidate", () => {
  const { state } = acceptedOnce();
  const result = reduce(state, {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: state.phase.messages![0].contentHash,
    unchanged: true,
  });
  assert.equal(result.ok, true);
  const message = result.state.phase.messages![0];
  assert.equal(message.state, "accepted");
  assert.equal(message.messageVersion, 2);
  assert.equal(message.boundCandidateSha, "C2");
  assert.equal(message.settlement?.candidateSha, "C2");
  assert.equal(message.settlement?.messageVersion, 2);
  assert.equal(message.invalidated, undefined);
});

test("carry: a changed settlement is invalidated, keeps who settled it, and needs a new verdict", () => {
  const { state, before } = acceptedOnce();
  const changed = { type: "tradeoff" as const, title: "a rewritten choice", summary: "new summary", context: "new context", evidence: ["new evidence"], planRef: "p" };
  const result = reduce(state, {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: contentHashOf(changed),
    unchanged: false,
    content: changed,
  });
  assert.equal(result.ok, true, result.ok ? "" : result.reason);
  const message = result.state.phase.messages![0];
  assert.equal(message.state, "published");
  // The old settlement is kept for the ledger, but marked invalidated.
  assert.equal(message.settlement?.settledBy, "owner");
  assert.equal(message.settlement?.candidateSha, C1);
  assert.equal(message.invalidated?.reason, "content changed");
  assert.notEqual(message.contentHash, before.contentHash);
  assert.equal(message.title, "a rewritten choice");
  assert.equal(message.contentHash, contentHashOf(changed));
});

test("carry: a changed contentHash without the new content is rejected", () => {
  const { state } = acceptedOnce();
  const result = reduce(state, {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: "f".repeat(64),
    unchanged: false,
  });
  assert.equal(result.ok, false);
  assert.match(result.ok ? "" : result.reason, /without carrying the new content/);
});

test("carry: a settlement after a contract amendment is invalidated", () => {
  const { state } = acceptedOnce();
  const amended: State = { ...state, phase: { ...state.phase, contract: { ...state.phase.contract, contractVersion: CV(2, "b".repeat(64)) } } };
  const result = reduce(amended, {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: state.phase.messages![0].contentHash,
    unchanged: true,
  });
  assert.equal(result.ok, true);
  const message = result.state.phase.messages![0];
  assert.equal(message.invalidated?.reason, "contract amended");
});

test("carry: a verdict naming the pre-carry version of an UNCHANGED message is applied", () => {
  const published = makeMessage({ state: "published" });
  const afterCarry = reduce(withMessage(published), {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: published.contentHash,
    unchanged: true,
  });
  assert.equal(afterCarry.ok, true);
  const result = reduce(afterCarry.state, {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "accept",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, true, result.ok ? "" : result.reason);
  assert.equal(result.state.phase.messages![0].state, "accepted");
});

test("carry: a verdict naming the pre-carry version of a CHANGED message is rejected", () => {
  const published = makeMessage({ state: "published" });
  const changed = { type: "tradeoff" as const, title: "a rewritten choice", summary: "new summary", context: "new context", evidence: ["new evidence"], planRef: "p" };
  const afterCarry = reduce(withMessage(published), {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: contentHashOf(changed),
    unchanged: false,
    content: changed,
  });
  assert.equal(afterCarry.ok, true);
  const result = reduce(afterCarry.state, {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "accept",
    boundCandidateSha: C1,
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(result.ok, false);
  assert.match(result.ok ? "" : result.reason, /invalidated|changed v1 → v2/);
});

test("carry: a new verdict on the current version clears the invalidation", () => {
  const { state } = acceptedOnce();
  const changed = { type: "tradeoff" as const, title: "a rewritten choice", summary: "new summary", context: "new context", evidence: ["new evidence"], planRef: "p" };
  const carried = reduce(state, {
    type: "MESSAGE_CARRIED",
    messageId: "T-1",
    fromCandidate: C1,
    toCandidate: "C2",
    fromVersion: 1,
    toVersion: 2,
    contentHash: contentHashOf(changed),
    unchanged: false,
    content: changed,
  });
  assert.equal(carried.ok, true);
  assert.equal(carried.state.phase.messages![0].invalidated?.reason, "content changed");
  const result = reduce(carried.state, {
    type: "OWNER_VERDICT",
    messageId: "T-1",
    verdict: "accept",
    boundCandidateSha: "C2",
    boundContractVersion: K,
    boundRecordVersion: 2,
  });
  assert.equal(result.ok, true, result.ok ? "" : result.reason);
  const message = result.state.phase.messages![0];
  assert.equal(message.state, "accepted");
  assert.equal(message.invalidated, undefined);
  assert.equal(message.settlement?.messageVersion, 2);
});

test("contentHash covers exactly the reviewable fields and is stable", () => {
  const a = contentHashOf({ type: "tradeoff", title: "t", summary: "s", context: "c", evidence: ["e"], planRef: "p" });
  const b = contentHashOf({ type: "tradeoff", title: "t", summary: "s", context: "c", evidence: ["e"], planRef: "p" });
  const c = contentHashOf({ type: "tradeoff", title: "t2", summary: "s", context: "c", evidence: ["e"], planRef: "p" });
  assert.equal(a, b);
  assert.notEqual(a, c);
  assert.equal(a.length, 64);
});
