// Phase 2b core tests: the pure half of the inbox mechanism. Every
// conductor-state owner command (design §7.4/§9.3) maps to a core event
// (owner-inbox.ts), and the additive record-only events for note/unneeded/
// miss apply through reduce() correctly.

import assert from "node:assert/strict";
import { test } from "node:test";

import { reduce } from "../../src/core/reduce.ts";
import { acceptInput, expandEntryCommand, normalizeDecisionViewCommand, ownerCommandToEvent } from "../../src/core/owner-inbox.ts";
import type { Entry } from "../../src/core/entries.ts";
import type { Event, State } from "../../src/core/types.ts";
import { CV, baseState, makeMessage } from "./helpers.ts";

const K = CV();

function step(state: State, event: Event): State {
  const result = reduce(state, event);
  assert.equal(result.ok, true, !result.ok ? result.reason : "");
  return result.state;
}

test("owner-inbox: every conductor-state kind maps to its core event", () => {
  assert.deepEqual(ownerCommandToEvent({ kind: "resolve", requestId: "OR-1", requestVersion: 3, option: "grant", boundCandidateSha: "C1", boundContractVersion: K }, "cmd-1"), {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "grant",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 3,
  });
  assert.deepEqual(
    ownerCommandToEvent({ kind: "override", decisionId: "D-1", decisionVersion: 2, vote: "approve", boundCandidateSha: "C1", boundContractVersion: K }, "cmd-2"),
    {
      type: "OVERRIDE_CAST",
      override: { decisionId: "D-1", vote: "approve", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 2 },
    },
  );
  assert.deepEqual(
    ownerCommandToEvent({ kind: "accept-finding", findingId: "F-1", findingVersion: 1, scope: "known risk", boundCandidateSha: "C1", boundContractVersion: K }, "cmd-3"),
    {
      type: "FINDING_ACCEPTED_BY_OWNER",
      findingId: "F-1",
      scope: "known risk",
      by: "owner",
      boundCandidateSha: "C1",
      boundContractVersion: K,
      boundRecordVersion: 1,
    },
  );
  const revise = ownerCommandToEvent({ kind: "revise", targetRecordId: "D-1", targetRecordVersion: 2, correctionText: "do it this way", contractChange: false, boundCandidateSha: "C1", boundContractVersion: K }, "cmd-4");
  assert.equal(revise?.type, "REVISE");
  assert.deepEqual((revise as Extract<Event, { type: "REVISE" }>).correctionId, "C-cmd-4");
  assert.deepEqual(
    ownerCommandToEvent({ kind: "amend", phaseId: "p1", replacingContractVersion: K, newContractVersion: CV(2) }, "cmd-5"),
    { type: "AMEND", replacingContractVersion: K, newContractVersion: CV(2) },
  );
  assert.deepEqual(ownerCommandToEvent({ kind: "note", phaseId: "p1", text: "keep it fast" }, "cmd-6"), {
    type: "NOTE_ADDED",
    phaseId: "p1",
    text: "keep it fast",
  });
  assert.deepEqual(ownerCommandToEvent({ kind: "unneeded", requestId: "OR-9" }, "cmd-7"), {
    type: "OWNER_REQUEST_MARKED_UNNEEDED",
    requestId: "OR-9",
  });
  assert.deepEqual(ownerCommandToEvent({ kind: "miss", recordId: "D-p1-1", sample: "detail" }, "cmd-8"), {
    type: "MISS_RECORDED",
    recordId: "D-p1-1",
    sample: "detail",
  });
  // steer is external delivery, not conductor state — phase 2c's work.
  assert.equal(ownerCommandToEvent({ kind: "steer", text: "x", boundAttemptId: "a" }, "cmd-9"), undefined);
});

test("owner-inbox: NOTE_ADDED queues a note for this phase and rejects one for another", () => {
  let state = baseState({ phase: "AWAITING_OWNER" });
  state = step(state, { type: "NOTE_ADDED", phaseId: "p1", text: "first" });
  state = step(state, { type: "NOTE_ADDED", phaseId: "p1", text: "second" });
  assert.deepEqual(state.phase.ownerNotes, ["first", "second"]);
  const wrong = reduce(state, { type: "NOTE_ADDED", phaseId: "p2", text: "nope" });
  assert.equal(wrong.ok, false);
  assert.match(!wrong.ok ? wrong.reason : "", /phase p2/);
});

test("owner-inbox: OWNER_REQUEST_MARKED_UNNEEDED records the pilot metric and leaves the request resolvable", () => {
  const request = {
    id: "OR-1",
    version: 1,
    phaseId: "p1",
    reason: "F1 open",
    origin: "open_finding" as const,
    linkedFindingId: "F1",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    options: [{ id: "repair", label: "repair" }],
    status: "open" as const,
  };
  let state = baseState({
    phase: "AWAITING_OWNER",
    candidate: { sha: "C1", contractVersion: K },
    ownerRequests: [request],
  });
  state = step(state, { type: "OWNER_REQUEST_MARKED_UNNEEDED", requestId: "OR-1" });
  // The metric is recorded, but the request itself stays open: closing it
  // would reject a follow-up resolve and — for the record-less fallback
  // request — strand AWAITING_OWNER with nothing to act on (the blocking
  // finding on this candidate).
  assert.equal(state.phase.ownerRequests[0].status, "open");
  assert.deepEqual(state.phase.unneededRequestIds, ["OR-1"]);
  assert.equal(state.phase.phase, "AWAITING_OWNER");
  const unknown = reduce(state, { type: "OWNER_REQUEST_MARKED_UNNEEDED", requestId: "OR-nope" });
  assert.equal(unknown.ok, false);
  const twice = reduce(state, { type: "OWNER_REQUEST_MARKED_UNNEEDED", requestId: "OR-1" });
  assert.equal(twice.ok, false);
  assert.match(!twice.ok ? twice.reason : "", /already marked unneeded/);
  // A follow-up resolve on the same request now works (it is still open)
  // and unparks the phase through the ordinary repair-forcing row.
  const resolved = step(state, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "OR-1",
    option: "repair",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 1,
  });
  assert.equal(resolved.phase.phase, "REPAIRING");
});

test("owner-inbox: NOTES_DELIVERED advances the delivered count so a note is sent once", () => {
  let state = baseState({ phase: "AWAITING_OWNER" });
  state = step(state, { type: "NOTE_ADDED", phaseId: "p1", text: "first" });
  state = step(state, { type: "NOTE_ADDED", phaseId: "p1", text: "second" });
  state = step(state, { type: "NOTES_DELIVERED", phaseId: "p1", count: 1 });
  assert.equal(state.phase.deliveredNoteCount, 1);
  const wrongPhase = reduce(state, { type: "NOTES_DELIVERED", phaseId: "p2", count: 1 });
  assert.equal(wrongPhase.ok, false);
  const zero = reduce(state, { type: "NOTES_DELIVERED", phaseId: "p1", count: 0 });
  assert.equal(zero.ok, false);
});

test("owner-inbox: a resolve can carry the scope note an accept_risk option requires", () => {
  const request = {
    id: "OR-1",
    version: 1,
    phaseId: "p1",
    reason: "finding F1 is still open",
    origin: "open_finding" as const,
    linkedFindingId: "F1",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    options: [
      { id: "accept_risk", label: "accept the risk" },
      { id: "repair", label: "repair (grant 3 rounds)" },
    ],
    status: "open" as const,
  };
  const finding = {
    id: "F1",
    version: 1,
    phaseId: "p1",
    kind: "defect" as const,
    severity: "blocking" as const,
    evidence: "a race the tests miss",
    raisedBy: "B" as const,
    status: "open" as const,
    boundCandidateSha: "C1",
  };
  const state = baseState({
    phase: "AWAITING_OWNER",
    candidate: { sha: "C1", contractVersion: K },
    ownerRequests: [request],
    findings: [finding],
  });

  const binding = { runId: "r1", phaseId: "p1", candidateSha: "C1", contractVersion: K, recordId: "OR-1", recordVersion: 1 };
  const normalized = normalizeDecisionViewCommand(
    { type: "resolve", option: "accept_risk", note: "known race, accepted for one release", binding },
    "cmd-note",
  );
  assert.equal(normalized.ok, true);
  assert.equal((normalized as { event: Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }> }).event.note, "known race, accepted for one release");
  const applied = step(state, (normalized as { event: Event }).event);
  assert.equal(applied.phase.findings[0].status, "accepted");
  assert.equal(applied.phase.findings[0].acceptedScope, "known race, accepted for one release");

  // Without a note the same option is still rejected visibly.
  const noNote = normalizeDecisionViewCommand({ type: "resolve", option: "accept_risk", binding }, "cmd-nonote");
  assert.equal(noNote.ok, true);
  const rejected = reduce(state, (noNote as { event: Event }).event);
  assert.equal(rejected.ok, false);
  assert.match(!rejected.ok ? rejected.reason : "", /scope note/);
});

test("owner-inbox: MISS_RECORDED records the observed miss sample once", () => {
  let state = baseState();
  state = step(state, { type: "MISS_RECORDED", recordId: "D-p1-1", sample: "detail" });
  assert.deepEqual(state.phase.misses, ["D-p1-1"]);
  const twice = reduce(state, { type: "MISS_RECORDED", recordId: "D-p1-1" });
  assert.equal(twice.ok, false);
  assert.match(!twice.ok ? twice.reason : "", /already marked as a miss/);
  const empty = reduce(state, { type: "MISS_RECORDED", recordId: "  " });
  assert.equal(empty.ok, false);
});

test("owner-inbox: the decision view's encoding (type + binding) maps to the same core events", () => {
  const binding = { runId: "r1", phaseId: "p1", candidateSha: "C1", contractVersion: K, recordId: "D-1", recordVersion: 2 };

  const override = normalizeDecisionViewCommand({ type: "override", vote: "reject", binding }, "cmd-1");
  assert.equal(override.ok, true);
  assert.deepEqual((override as { event: Event }).event, {
    type: "OVERRIDE_CAST",
    override: { decisionId: "D-1", vote: "reject", boundCandidateSha: "C1", boundContractVersion: K, boundRecordVersion: 2 },
  });

  const revise = normalizeDecisionViewCommand({ type: "revise", text: "do it this way", changesContract: true, binding }, "cmd-2");
  assert.equal(revise.ok, true);
  assert.deepEqual((revise as { event: Event }).event, {
    type: "REVISE",
    correctionId: "C-cmd-2",
    targetRecordId: "D-1",
    correctionText: "do it this way",
    contractChange: true,
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 2,
  });

  const resolve = normalizeDecisionViewCommand({ type: "resolve", option: "grant", recordKind: "request", binding }, "cmd-3");
  assert.equal(resolve.ok, true);
  assert.deepEqual((resolve as { event: Event }).event, {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: "D-1",
    option: "grant",
    boundCandidateSha: "C1",
    boundContractVersion: K,
    boundRecordVersion: 2,
  });

  const accept = normalizeDecisionViewCommand({ type: "accept-finding", scope: "known risk", binding }, "cmd-4");
  assert.equal(accept.ok, true);
  assert.equal((accept as { event: Extract<Event, { type: "FINDING_ACCEPTED_BY_OWNER" }> }).event.type, "FINDING_ACCEPTED_BY_OWNER");

  const miss = normalizeDecisionViewCommand({ type: "miss", recordKind: "decision", binding }, "cmd-5");
  assert.equal(miss.ok, true);
  assert.deepEqual((miss as { event: Event }).event, { type: "MISS_RECORDED", recordId: "D-1", sample: "decision" });

  const unneeded = normalizeDecisionViewCommand({ type: "unneeded", binding }, "cmd-6");
  assert.equal(unneeded.ok, true);
  assert.deepEqual((unneeded as { event: Event }).event, { type: "OWNER_REQUEST_MARKED_UNNEEDED", requestId: "D-1" });

  // A note carries only run/phase in its binding (no candidate exists).
  const note = normalizeDecisionViewCommand({ type: "note", text: "keep it fast", binding: { runId: "r1", phaseId: "p1" } }, "cmd-7");
  assert.equal(note.ok, true);
  assert.deepEqual((note as { event: Event }).event, { type: "NOTE_ADDED", phaseId: "p1", text: "keep it fast" });

  // A binding-less command and an unknown type are rejected with a reason.
  const noBinding = normalizeDecisionViewCommand({ type: "resolve", option: "grant" }, "cmd-8");
  assert.equal(noBinding.ok, false);
  assert.match(!noBinding.ok ? noBinding.reason : "", /needs a 'binding'/);
  const unknown = normalizeDecisionViewCommand({ type: "steer", text: "x", binding: { runId: "r1", phaseId: "p1" } }, "cmd-9");
  assert.equal(unknown.ok, false);
  assert.match(!unknown.ok ? unknown.reason : "", /not a conductor-state command/);
});

// Plan 05j: the owner's entry corrections (s / m / A / D in the review view).
test("owner-inbox: the review view's entry commands expand to ENTRY_*/OWNER_VERDICT events and reduce", () => {
  const locate = { runId: "r1", phaseId: "p1" };
  const a = makeMessage({ id: "F-1", type: "finding", evidence: ["src/a.rs:10 x"], state: "published" });
  const b = makeMessage({ id: "T-2", type: "tradeoff", evidence: ["src/a.rs:11 y"], state: "published" });
  const raw = makeMessage({ id: "F-3", type: "finding", evidence: ["src/a.rs:12 z"], state: "raw" });
  const entries: Entry[] = [
    { id: "E-1", phaseId: "p1", title: "one topic", type: "finding", state: "open", anchor: { kind: "file", path: "src/a.rs", lines: [1, 20] }, links: [{ messageId: "F-1", anchor: { kind: "file", path: "src/a.rs", lines: [10, 10] }, reason: "opened" }, { messageId: "T-2", anchor: { kind: "file", path: "src/a.rs", lines: [11, 11] }, reason: "linked" }, { messageId: "F-3", anchor: { kind: "file", path: "src/a.rs", lines: [12, 12] }, reason: "linked" }] },
    { id: "E-2", phaseId: "p1", title: "another", type: "tradeoff", state: "open", anchor: { kind: "file", path: "src/b.rs", lines: [1, 2] }, links: [] },
  ];
  const expand = (raw: Record<string, unknown>) => expandEntryCommand({ ...locate, ...raw }, entries, [a, b, raw]);
  const split = expand({ type: "entry-split", entryId: "E-1", messageId: "T-2" });
  assert.ok(split.ok);
  assert.deepEqual(split.ok ? split.events : [], [{ type: "ENTRY_SPLIT", entryId: "E-1", messageId: "T-2", by: "owner" }]);
  const merge = expand({ type: "entry-merge", entryId: "E-1", intoEntryId: "E-2" });
  assert.ok(merge.ok);
  assert.deepEqual(merge.ok ? merge.events : [], [{ type: "ENTRY_MERGED_BY_OWNER", entryId: "E-1", intoEntryId: "E-2", by: "owner" }]);
  // A verdict is one OWNER_VERDICT per linked message, carrying its binding.
  const refuse = expand({ type: "entry-verdict", entryId: "E-1", verdict: "refuse", reason: "not this round" });
  assert.ok(refuse.ok);
  assert.equal(refuse.ok ? refuse.events.length : 0, 2);
  // A raw linked message cannot be settled yet, and is REPORTED, not silently
  // skipped (record A-72 / M-63).
  assert.deepEqual(refuse.ok ? refuse.skipped : [], ["F-3"]);
  assert.deepEqual(refuse.ok ? refuse.events[0] : {}, {
    type: "OWNER_VERDICT",
    messageId: "F-1",
    verdict: "refuse",
    reason: "not this round",
    boundCandidateSha: a.boundCandidateSha,
    boundContractVersion: a.boundContractVersion,
    boundRecordVersion: a.messageVersion,
  });
  // A bad op, an unknown entry, or one missing its message is refused.
  assert.equal(expand({ type: "entry-split", entryId: "E-1" }).ok, false);
  assert.equal(expand({ type: "entry-frobnicate", entryId: "E-1" }).ok, false);
  assert.equal(expand({ type: "entry-merge", entryId: "E-1", intoEntryId: "E-9" }).ok, false);
  // The event reduces: an entry opens with a message, and ENTRY_STATE settles it.
  const opened = step(baseState({ entries: [] }), {
    type: "ENTRY_OPENED",
    phaseId: "p1",
    title: "a topic",
    anchor: { kind: "decision", id: "D-1" },
  });
  const settled = step(opened, { type: "ENTRY_STATE", entryId: "E-1", state: "resolved", sha: "C1", by: "owner" });
  assert.equal(settled.phase.entries?.[0].state, "resolved");
  assert.equal(settled.phase.entries?.[0].stateSha, "C1");
  // The round's curator pass is recorded, so the evaluators may start.
  const curated = step(opened, { type: "ENTRY_CURATED", candidateSha: "C1", count: 1 });
  assert.equal(curated.phase.curatedFor, "C1");
});

test("plan 06d: acceptInput decides applied, queued or refused and only DONE/BLOCKED refuse", () => {
  const at = (phase: string) => baseState({ phase: phase as never });
  // A correction while AWAITING_OWNER applies now.
  assert.deepEqual(acceptInput(at("AWAITING_OWNER"), { kind: "correction", text: "x", workerRunning: false }), {
    kind: "applied",
    effect: "correction",
  });
  // A steer with a live worker applies now.
  assert.deepEqual(acceptInput(at("IMPLEMENTING"), { kind: "steer", text: "x", workerRunning: true }), {
    kind: "applied",
    effect: "steer",
  });
  // A correction outside AWAITING_OWNER queues for the next worker attempt.
  assert.deepEqual(acceptInput(at("REVIEWING"), { kind: "correction", text: "x", workerRunning: false }), {
    kind: "queued",
    until: "next_attempt",
  });
  // A steer with no worker queues the same way.
  assert.deepEqual(acceptInput(at("CHECKING"), { kind: "steer", text: "x", workerRunning: false }), {
    kind: "queued",
    until: "next_attempt",
  });
  // A note always queues.
  assert.deepEqual(acceptInput(at("REVIEWING"), { kind: "note", text: "x", workerRunning: false }), {
    kind: "queued",
    until: "next_attempt",
  });
  // Only DONE and BLOCKED refuse.
  for (const phase of ["DONE", "BLOCKED"]) {
    const r = acceptInput(at(phase), { kind: "note", text: "x", workerRunning: false });
    assert.equal(r.kind, "refused");
    assert.match((r as { reason: string }).reason, new RegExp(phase));
  }
});
