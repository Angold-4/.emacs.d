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

import type { ContractVersion, Event, OwnerCommand } from "./types.ts";

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
    case "unneeded":
      return { type: "OWNER_REQUEST_MARKED_UNNEEDED", requestId: command.requestId };
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
    case "unneeded": {
      if (b.recordId === "") return { ok: false, reason: "an unneeded command needs 'binding.recordId'" };
      return { ok: true, runId: b.runId, phaseId: b.phaseId, event: { type: "OWNER_REQUEST_MARKED_UNNEEDED", requestId: b.recordId } };
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
