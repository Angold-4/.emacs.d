// design §7.4/§9.3: maps an owner command (the JSON Emacs writes to
// <run>/inbox/<id>.json, schemas/owner-command.schema.json) to the core
// event that applies it. This is the pure half of the inbox mechanism; the
// conductor owns reading the inbox directory, the exactly-once log append,
// the move to applied/ and the visible rejection into rejected/.
//
// The §9.3 conductor-state commands all reduce to a core event here:
// resolve/override/accept-finding/revise/amend reuse the record-level events
// the core already had; note/unneeded/miss are the additive record-only
// events types.ts defines. Any other kind (steer, which is external delivery
// rather than conductor state, and pause/resume/mode) is not this phase's
// work and maps to `undefined`, which the conductor rejects visibly.

import { entryVerdictEvents, type Entry } from "./entries.ts";
import type { ContractVersion, Event, Message, OwnerCommand, State } from "./types.ts";

// ---------------------------------------------------------------------------
// Plan 06d (A1): what an inbox input does
// ---------------------------------------------------------------------------
//
// The owner's input box can reach a run in exactly three ways: it is applied
// now, queued for the moment the run can apply it, or refused with a reason.
// `acceptInput` is the ONE place that decides which, from the phase state and
// whether a worker is live right now. The conductor only executes the
// decision (steer, queue a note, or record the refusal), so no second caller
// can invent a different outcome.

export type InputKind = "steer" | "note" | "correction";

export type InputOutcome =
  | { kind: "applied"; effect: InputKind }
  | { kind: "queued"; until: "next_attempt" | "awaiting_owner" }
  | { kind: "refused"; reason: string };

export interface InputCommand {
  kind: InputKind;
  text: string;
  /** True when a worker agent of this run is live right now. The conductor
   * observes it; it never decides the outcome itself. */
  workerRunning: boolean;
}

/** Decide what one owner input does now. Only DONE and BLOCKED refuse: every
 * other phase either applies the input or queues it for the next worker
 * attempt, so owner input is never lost. */
export function acceptInput(state: State, command: InputCommand): InputOutcome {
  const name = state.phase.phase;
  if (name === "DONE" || name === "BLOCKED") {
    return { kind: "refused", reason: `the phase is ${name}; the run no longer accepts owner input` };
  }
  // A correction is applied now only while the phase is parked on the owner:
  // that is the one moment resolving the open requests is what it means.
  if (command.kind === "correction" && name === "AWAITING_OWNER") {
    return { kind: "applied", effect: "correction" };
  }
  // A steer reaches the worker directly only while one is live.
  if (command.kind === "steer" && command.workerRunning) {
    return { kind: "applied", effect: "steer" };
  }
  // A note, a correction outside AWAITING_OWNER, or a steer with no worker:
  // queued for the next worker attempt. It reaches that attempt's prompt (and
  // the live reviewers' notes) and is marked delivered there.
  return { kind: "queued", until: "next_attempt" };
}

/** Plan 05j: the entry commands the review view sends (`s`, `m`, a retitle,
 * and the owner's A/D). The owner's verdict is a verdict on each linked
 * message, not an ENTRY_STATE: accepting a trade-off is not a code fix
 * (record M-12), so `entry-verdict` expands to one OWNER_VERDICT per
 * settleable message. Any other entry command is refused here. */
export function expandEntryCommand(
  raw: unknown,
  entries: readonly Entry[],
  messages: readonly Message[],
): { ok: true; runId: string; phaseId: string; events: Event[]; skipped: string[] } | { ok: false; reason: string } {
  if (!raw || typeof raw !== "object") return { ok: false, reason: "entry command must be a JSON object" };
  const r = raw as Record<string, unknown>;
  if (typeof r.type !== "string" || !r.type.startsWith("entry-")) {
    return { ok: false, reason: `not an entry command: '${typeof r.type === "string" ? r.type : "(none)"}'` };
  }
  const runId = typeof r.runId === "string" ? r.runId : "";
  const phaseId = typeof r.phaseId === "string" ? r.phaseId : "";
  const entryId = typeof r.entryId === "string" && r.entryId.length > 0 ? r.entryId : undefined;
  if (!entryId) return { ok: false, reason: `'${r.type}' needs an entryId` };
  const entry = entries.find((e) => e.id === entryId);
  if (!entry) return { ok: false, reason: `unknown entry ${entryId}` };
  switch (r.type) {
    case "entry-split": {
      const messageId = typeof r.messageId === "string" && r.messageId.length > 0 ? r.messageId : undefined;
      if (!messageId) return { ok: false, reason: "'entry-split' needs a messageId" };
      if (!entry.links.some((l) => l.messageId === messageId)) {
        return { ok: false, reason: `entry ${entryId} does not link message ${messageId}` };
      }
      const newEntryId = typeof r.newEntryId === "string" && r.newEntryId.length > 0 ? r.newEntryId : undefined;
      return { ok: true, runId, phaseId, skipped: [], events: [{ type: "ENTRY_SPLIT", entryId, messageId, ...(newEntryId ? { newEntryId } : {}), by: "owner" }] };
    }
    case "entry-merge": {
      const intoEntryId = typeof r.intoEntryId === "string" && r.intoEntryId.length > 0 ? r.intoEntryId : undefined;
      if (!intoEntryId) return { ok: false, reason: "'entry-merge' needs an intoEntryId" };
      if (!entries.some((e) => e.id === intoEntryId)) return { ok: false, reason: `unknown entry ${intoEntryId}` };
      return { ok: true, runId, phaseId, skipped: [], events: [{ type: "ENTRY_MERGED_BY_OWNER", entryId, intoEntryId, by: "owner" }] };
    }
    case "entry-retitle": {
      const title = typeof r.title === "string" ? r.title.trim() : "";
      if (!title) return { ok: false, reason: "'entry-retitle' needs a non-empty title" };
      return { ok: true, runId, phaseId, skipped: [], events: [{ type: "ENTRY_RETITLED", entryId, title, by: "owner" }] };
    }
    case "entry-verdict": {
      if (r.verdict !== "accept" && r.verdict !== "refuse") return { ok: false, reason: "'entry-verdict' needs verdict accept or refuse" };
      const reason = typeof r.reason === "string" && r.reason.trim().length > 0 ? r.reason.trim() : undefined;
      const plan = entryVerdictEvents(entry, messages, r.verdict, reason);
      if (plan.events.length === 0) {
        return { ok: false, reason: `entry ${entryId} has no published message to settle` };
      }
      // The settleable messages are settled; a raw one is REPORTED, never
      // silently dropped (record A-72 / M-63).
      return { ok: true, runId, phaseId, events: plan.events, skipped: plan.skipped };
    }
    default:
      return { ok: false, reason: `unknown entry command '${r.type}'` };
  }
}

/** Maps one inbox owner command to its core event, or `undefined` when the
 * kind is not a conductor-state command this phase implements. `commandId`
 * is the inbox file's basename (design §9.1: the id lives in the filename,
 * not the JSON body — schemas/owner-command.schema.json's objects carry no
 * `id` field); it is used to make the revise correction id stable across a
 * crash/restart between the log append and the inbox file move. */
export function ownerCommandToEvent(command: OwnerCommand, commandId: string): Event | undefined {
  switch (command.kind) {
    case "resolve":
      return {
        type: "OWNER_REQUEST_RESOLVED",
        requestId: command.requestId,
        option: command.option,
        // Needed by the open_finding `accept_risk` option (design §4.2);
        // omitted entirely when absent so the event shape is unchanged for
        // every other option.
        ...(command.note && command.note.trim().length > 0 ? { note: command.note } : {}),
        boundCandidateSha: command.boundCandidateSha,
        boundContractVersion: command.boundContractVersion,
        boundRecordVersion: command.requestVersion,
      };
    case "override":
      return {
        type: "OVERRIDE_CAST",
        override: {
          decisionId: command.decisionId,
          vote: command.vote,
          boundCandidateSha: command.boundCandidateSha,
          boundContractVersion: command.boundContractVersion,
          boundRecordVersion: command.decisionVersion,
        },
      };
    case "accept-finding":
      return {
        type: "FINDING_ACCEPTED_BY_OWNER",
        findingId: command.findingId,
        scope: command.scope,
        by: "owner",
        boundCandidateSha: command.boundCandidateSha,
        boundContractVersion: command.boundContractVersion,
        boundRecordVersion: command.findingVersion,
      };
    case "revise":
      return {
        type: "REVISE",
        correctionId: `C-${commandId}`,
        targetRecordId: command.targetRecordId,
        correctionText: command.correctionText,
        contractChange: command.contractChange,
        boundCandidateSha: command.boundCandidateSha,
        boundContractVersion: command.boundContractVersion,
        boundRecordVersion: command.targetRecordVersion,
      };
    case "amend":
      return {
        type: "AMEND",
        replacingContractVersion: command.replacingContractVersion,
        newContractVersion: command.newContractVersion,
      };
    case "note":
      return { type: "NOTE_ADDED", phaseId: command.phaseId, text: command.text };
    case "correction":
      return { type: "OWNER_CORRECTION", correctionId: `C-${commandId}`, text: command.text };
    case "unneeded":
      return { type: "OWNER_REQUEST_MARKED_UNNEEDED", requestId: command.requestId };
    case "verdict":
      return {
        type: "OWNER_VERDICT",
        messageId: command.messageId,
        verdict: command.verdict,
        ...(command.reason && command.reason.trim().length > 0 ? { reason: command.reason } : {}),
        boundCandidateSha: command.boundCandidateSha,
        boundContractVersion: command.boundContractVersion,
        boundRecordVersion: command.messageVersion,
      };
    case "miss":
      return { type: "MISS_RECORDED", recordId: command.recordId, sample: command.sample };
    case "steer":
    case "pause":
    case "resume":
    case "mode":
      return undefined;
  }
}

// ---------------------------------------------------------------------------
// The decision view's encoding (design §10.4): the Emacs front end
// (core/init-tradeoffs-trace.el) writes commands as `{ commandId, type,
// recordKind?, binding, ...type-specific fields }`, where `binding` is the
// full design §7.1 tuple (schemas/binding.schema.json). This is the shape
// that actually lands in `<run>/inbox/<id>.json`, so the conductor must
// accept it in addition to `owner-command.schema.json`'s flat `kind` form
// (which is also what `tt cmd`/tests may write). `normalizeDecisionViewCommand`
// is the pure mapping for that encoding.
// ---------------------------------------------------------------------------

export type NormalizedDecisionViewCommand =
  | { ok: true; event: Event; runId: string; phaseId: string }
  | { ok: false; reason: string };

function bindingParts(binding: Record<string, unknown>): {
  runId: string;
  phaseId: string;
  recordId: string;
  candidateSha: string;
  recordVersion: number | undefined;
  contractVersion: unknown;
} {
  return {
    runId: typeof binding.runId === "string" ? binding.runId : "",
    phaseId: typeof binding.phaseId === "string" ? binding.phaseId : "",
    recordId: typeof binding.recordId === "string" ? binding.recordId : "",
    candidateSha: typeof binding.candidateSha === "string" ? binding.candidateSha : "",
    recordVersion: typeof binding.recordVersion === "number" ? binding.recordVersion : undefined,
    contractVersion: binding.contractVersion,
  };
}

export function normalizeDecisionViewCommand(raw: unknown, commandId: string): NormalizedDecisionViewCommand {
  if (!raw || typeof raw !== "object") return { ok: false, reason: "owner command must be a JSON object" };
  const r = raw as Record<string, unknown>;
  if (typeof r.type !== "string") {
    return { ok: false, reason: "owner command is neither the flat 'kind' schema form nor a decision-view 'type' command" };
  }
  // Plan 05j: the entry commands the review view sends. They are not bound to
  // a message version (an entry is a topic spanning messages), only to the
  // run/phase they were viewed in. Handled before the message-binding check.
  // Plan 05j: entry commands are expanded to one or more events by
  // expandEntryCommand (an entry-verdict is one OWNER_VERDICT per linked
  // message); the single-event decision-view mapping does not carry them.
  if (r.type.startsWith("entry-")) {
    return { ok: false, reason: `entry command '${r.type}' is expanded by expandEntryCommand` };
  }
  const binding = r.binding;
  if (!binding || typeof binding !== "object") {
    return { ok: false, reason: `decision-view '${r.type}' command needs a 'binding' tuple` };
  }
  const b = bindingParts(binding as Record<string, unknown>);
  const bound = (): { boundCandidateSha: string; boundContractVersion: ContractVersion; boundRecordVersion: number } | { error: string } => {
    if (b.recordId === "") return { error: "the binding is missing 'recordId'" };
    if (b.candidateSha === "") return { error: "the binding is missing 'candidateSha'" };
    if (b.recordVersion === undefined) return { error: "the binding is missing 'recordVersion'" };
    if (!b.contractVersion || typeof b.contractVersion !== "object") return { error: "the binding is missing 'contractVersion'" };
    return {
      boundCandidateSha: b.candidateSha,
      boundContractVersion: b.contractVersion as ContractVersion,
      boundRecordVersion: b.recordVersion,
    };
  };

  switch (r.type) {
    case "resolve": {
      const w = bound();
      if ("error" in w) return { ok: false, reason: w.error };
      if (typeof r.option !== "string" || r.option.length === 0) return { ok: false, reason: "a resolve command needs an 'option'" };
      const note = typeof r.note === "string" && r.note.trim().length > 0 ? r.note : undefined;
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: { type: "OWNER_REQUEST_RESOLVED", requestId: b.recordId, option: r.option, ...(note ? { note } : {}), ...w },
      };
    }
    case "override": {
      const w = bound();
      if ("error" in w) return { ok: false, reason: w.error };
      if (r.vote !== "approve" && r.vote !== "reject") return { ok: false, reason: "an override command needs 'vote' approve or reject" };
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: { type: "OVERRIDE_CAST", override: { decisionId: b.recordId, vote: r.vote, ...w } },
      };
    }
    case "accept-finding": {
      const w = bound();
      if ("error" in w) return { ok: false, reason: w.error };
      if (typeof r.scope !== "string" || r.scope.trim().length === 0) {
        return { ok: false, reason: "an accept-finding command needs a non-empty 'scope'" };
      }
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: { type: "FINDING_ACCEPTED_BY_OWNER", findingId: b.recordId, scope: r.scope, by: "owner", ...w },
      };
    }
    case "carry": {
      // Plan 06g (A5): the owner carries a finding or message to a later
      // phase's plan. The binding names the record and its version, so a
      // stale carry (the record changed, or the phase moved to a new
      // candidate) is rejected like any other owner command.
      const w = bound();
      if ("error" in w) return { ok: false, reason: w.error };
      if (typeof r.toPhase !== "string" || r.toPhase.trim().length === 0) {
        return { ok: false, reason: "a carry command needs a non-empty 'toPhase'" };
      }
      const recordKind = r.recordKind === "finding" || r.recordKind === "message" ? r.recordKind : undefined;
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: {
          type: "ITEM_CARRIED",
          recordId: b.recordId,
          ...(recordKind ? { recordKind } : {}),
          toPhase: r.toPhase.trim(),
          ...w,
        },
      };
    }
    case "defer": {
      // Plan 06i: a deferral puts off an item's fix. It needs a guard — a test
      // that shows its current cost, or a recorded owner ruling — or it is
      // refused with the reason (reduce() refuses it too, so a hand-written
      // inbox file cannot bypass this).
      const w = bound();
      if ("error" in w) return { ok: false, reason: w.error };
      const test = typeof r.test === "string" && r.test.trim().length > 0 ? r.test.trim() : undefined;
      const ownerRuling =
        typeof r.ownerRuling === "string" && r.ownerRuling.trim().length > 0 ? r.ownerRuling.trim() : undefined;
      if (!test && !ownerRuling) {
        return { ok: false, reason: "a defer command needs a guard: --test <test that shows its current cost> or --owner-ruling <ruling id>" };
      }
      const toPhase = typeof r.toPhase === "string" && r.toPhase.trim().length > 0 ? r.toPhase.trim() : undefined;
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: {
          type: "DEFERRAL_RECORDED",
          deferral: {
            id: `DEF-${commandId}`,
            itemId: b.recordId,
            text: typeof r.text === "string" && r.text.trim().length > 0 ? r.text.trim() : `deferred ${b.recordId}`,
            ...(test ? { test } : {}),
            ...(ownerRuling ? { ownerRuling } : {}),
            ...(toPhase ? { toPhase } : {}),
            status: "open",
          },
        },
      };
    }
    case "revise": {
      const w = bound();
      if ("error" in w) return { ok: false, reason: w.error };
      if (typeof r.text !== "string" || r.text.trim().length === 0) return { ok: false, reason: "a revise command needs a non-empty 'text'" };
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: {
          type: "REVISE",
          correctionId: `C-${commandId}`,
          targetRecordId: b.recordId,
          correctionText: r.text,
          contractChange: r.changesContract === true,
          ...w,
        },
      };
    }
    case "amend": {
      const replacing = r.replacingContractVersion ?? b.contractVersion;
      const next = r.newContractVersion;
      if (!replacing || typeof replacing !== "object") {
        return { ok: false, reason: "an amend command needs 'replacingContractVersion' (or binding.contractVersion)" };
      }
      if (!next || typeof next !== "object") return { ok: false, reason: "an amend command needs 'newContractVersion'" };
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: { type: "AMEND", replacingContractVersion: replacing as ContractVersion, newContractVersion: next as ContractVersion },
      };
    }
    case "note": {
      if (typeof r.text !== "string" || r.text.trim().length === 0) return { ok: false, reason: "a note command needs a non-empty 'text'" };
      if (b.phaseId === "") return { ok: false, reason: "a note command needs 'binding.phaseId'" };
      return { ok: true, runId: b.runId, phaseId: b.phaseId, event: { type: "NOTE_ADDED", phaseId: b.phaseId, text: r.text } };
    }
    case "correction": {
      if (typeof r.text !== "string" || r.text.trim().length === 0) {
        return { ok: false, reason: "a correction command needs a non-empty 'text'" };
      }
      if (b.phaseId === "") return { ok: false, reason: "a correction command needs 'binding.phaseId'" };
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: { type: "OWNER_CORRECTION", correctionId: `C-${commandId}`, text: r.text },
      };
    }
    case "unneeded": {
      if (b.recordId === "") return { ok: false, reason: "an unneeded command needs 'binding.recordId'" };
      return { ok: true, runId: b.runId, phaseId: b.phaseId, event: { type: "OWNER_REQUEST_MARKED_UNNEEDED", requestId: b.recordId } };
    }
    case "verdict": {
      const w = bound();
      if ("error" in w) return { ok: false, reason: w.error };
      if (r.verdict !== "accept" && r.verdict !== "refuse") {
        return { ok: false, reason: "a verdict command needs 'verdict' accept or refuse" };
      }
      const reason = typeof r.reason === "string" && r.reason.trim().length > 0 ? r.reason : undefined;
      return {
        ok: true,
        runId: b.runId,
        phaseId: b.phaseId,
        event: { type: "OWNER_VERDICT", messageId: b.recordId, verdict: r.verdict, ...(reason ? { reason } : {}), ...w },
      };
    }
    case "miss": {
      if (b.recordId === "") return { ok: false, reason: "a miss command needs 'binding.recordId'" };
      const sample = typeof r.recordKind === "string" ? r.recordKind : undefined;
      return { ok: true, runId: b.runId, phaseId: b.phaseId, event: { type: "MISS_RECORDED", recordId: b.recordId, sample } };
    }
    default:
      return { ok: false, reason: `decision-view command type '${r.type}' is not a conductor-state command this phase applies` };
  }
}
