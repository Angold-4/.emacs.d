// Contract v1: messages, their lifecycle, and the settled ledger.
//
// A trade-off, a finding or a blocker is a first-class record with a state
// machine of its own. `MESSAGE_TRANSITIONS` is the normative, data-shaped
// table of that machine — the same discipline as `TRANSITIONS` in
// transitions.ts: every row has a test fixture (test/contract/messages.test.ts),
// and reduce() only ever changes a message's state through a row whose
// `from`/`trigger` match and whose guard accepts. An event with no matching
// row (for example OWNER_VERDICT on a `dropped` message) is rejected.
//
// `events.jsonl` stays the ONE authoritative history: every message, every
// verdict and every carry is an event reduced by reduce(). `messages.jsonl`
// and `ledger.jsonl` are pure projections `projectMessages()` /
// `projectLedger()` rebuild from state, never sources of truth.
//
// The ledger is bound to CONTENT, not to a version number. Every message
// version carries a `contentHash` (sha256 of its reviewable fields). A carry
// to a new candidate bumps the version; the settlement carries exactly when
// the contentHash is unchanged AND the contract version is the same. A
// changed message, or any message after a contract amendment, has its ledger
// entry marked `invalidated` and needs a new verdict.

import { createHash } from "node:crypto";

import { sameVersion } from "./predicate.ts";
import type {
  ContractVersion,
  Message,
  MessageSettlement,
  MessageState,
  MessageType,
} from "./types.ts";

/** The reviewable content a message version hashes. `contentHash` is sha256
 * over exactly these fields, so a settlement can say "the content did not
 * change" without trusting a version number. */
export interface MessageContent {
  type: MessageType;
  title: string;
  summary: string;
  context: string;
  evidence: string[];
  planRef?: string;
  /** Plan 04a: evaluator metadata. Deliberately NOT part of `contentHashOf`:
   * it is the evaluator's judgement, not the reviewable claim. */
  importance?: "high" | "medium" | "low";
}

export function contentHashOf(content: MessageContent): string {
  const canonical = JSON.stringify({
    type: content.type,
    title: content.title,
    summary: content.summary,
    context: content.context,
    evidence: content.evidence,
    planRef: content.planRef ?? "",
  });
  return createHash("sha256").update(canonical).digest("hex");
}

// ---------------------------------------------------------------------------
// The transition table
// ---------------------------------------------------------------------------

export type MessageEventType =
  | "MESSAGE_RAISED"
  | "MESSAGE_PUBLISHED"
  | "MESSAGE_MERGED"
  | "MESSAGE_DROPPED"
  | "OWNER_VERDICT"
  | "MESSAGE_RESOLVED"
  | "MESSAGE_SUPERSEDED";

export interface MessageEventLike {
  type: MessageEventType;
  verdict?: "accept" | "refuse";
  by?: "owner" | "evaluator" | "panel" | "vote";
  reason?: string;
}

export interface MessageTransitionRow {
  id: string; // stable name, used by the fixture test to check coverage
  from: MessageState | "none";
  trigger: MessageEventType;
  guardName: string;
  guard: (message: Message | undefined, event: MessageEventLike) => boolean;
  to: MessageState;
  apply: (message: Message | undefined, event: MessageEventLike) => Message;
}

function settle(message: Message, state: MessageSettlement["state"], by: MessageSettlement["settledBy"], event: MessageEventLike): Message {
  return {
    ...message,
    state,
    // A fresh settlement clears an earlier carry invalidation.
    invalidated: undefined,
    settlement: {
      state,
      settledBy: by,
      reason: event.reason,
      candidateSha: message.boundCandidateSha,
      contractVersion: message.boundContractVersion,
      messageVersion: message.messageVersion,
      contentHash: message.contentHash,
    },
  };
}

const rows: MessageTransitionRow[] = [];

function addRow(row: MessageTransitionRow) {
  rows.push(row);
}

addRow({
  id: "message-raised",
  from: "none",
  trigger: "MESSAGE_RAISED",
  guardName: "noExistingMessage",
  guard: (m) => m === undefined,
  to: "raw",
  apply: (m, event) => (event as unknown as { type: "MESSAGE_RAISED"; message: Message }).message,
});

addRow({
  id: "message-published",
  from: "raw",
  trigger: "MESSAGE_PUBLISHED",
  guardName: "always",
  guard: () => true,
  to: "published",
  // Plan 04a: the evaluator publishes a message with the clean, human
  // wording it produced (title ≤ 80 characters, summary, context,
  // evidence). When the event carries that content it replaces the raw
  // fields and the contentHash is recomputed; a bare publish (no evaluator)
  // leaves the raised fields untouched.
  apply: (m, event) => {
    const e = event as unknown as { content?: MessageContent; unevaluated?: boolean };
    const base: Message = { ...m!, state: "published", unevaluated: e.unevaluated === true ? true : undefined };
    if (!e.content) return base;
    return { ...base, ...e.content, contentHash: contentHashOf(e.content) };
  },
});

addRow({
  id: "message-merged",
  from: "raw",
  trigger: "MESSAGE_MERGED",
  guardName: "always",
  guard: () => true,
  to: "merged",
  apply: (m, event) => settle(m!, "merged", event.by ?? "evaluator", event),
});

addRow({
  id: "message-dropped",
  from: "raw",
  trigger: "MESSAGE_DROPPED",
  guardName: "always",
  guard: () => true,
  to: "dropped",
  apply: (m, event) => settle(m!, "dropped", event.by ?? "evaluator", event),
});

addRow({
  id: "owner-verdict-accept",
  from: "published",
  trigger: "OWNER_VERDICT",
  guardName: "verdictAccept",
  guard: (_m, event) => event.verdict === "accept",
  to: "accepted",
  apply: (m, event) => settle(m!, "accepted", "owner", event),
});

addRow({
  id: "owner-verdict-refuse",
  from: "published",
  trigger: "OWNER_VERDICT",
  guardName: "verdictRefuse",
  guard: (_m, event) => event.verdict === "refuse",
  to: "refused",
  apply: (m, event) => settle(m!, "refused", "owner", event),
});

addRow({
  id: "message-superseded-published",
  from: "published",
  trigger: "MESSAGE_SUPERSEDED",
  guardName: "always",
  guard: () => true,
  to: "superseded",
  // A supersede is a state change, not an erasure: any prior settlement stays
  // in the ledger, marked superseded (so a refused or merged entry is never
  // silently dropped).
  apply: (m, event) => ({ ...m!, state: "superseded", supersededBy: event.reason ?? "superseded" }),
});

addRow({
  id: "message-resolved-published",
  from: "published",
  trigger: "MESSAGE_RESOLVED",
  guardName: "always",
  guard: () => true,
  to: "resolved",
  apply: (m, event) => settle(m!, "resolved", event.by ?? "evaluator", event),
});

addRow({
  id: "message-superseded-refused",
  from: "refused",
  trigger: "MESSAGE_SUPERSEDED",
  guardName: "always",
  guard: () => true,
  to: "superseded",
  apply: (m, event) => ({ ...m!, state: "superseded", supersededBy: event.reason ?? "superseded" }),
});

// A message whose backing record was withdrawn or superseded is superseded
// too, whatever it had settled to — its settlement stays in the ledger,
// marked `supersededBy`, never cleared.
for (const from of ["accepted", "merged", "dropped"] as const) {
  addRow({
    id: `message-superseded-${from}`,
    from,
    trigger: "MESSAGE_SUPERSEDED",
    guardName: "always",
    guard: () => true,
    to: "superseded",
    apply: (m, event) => ({ ...m!, state: "superseded", supersededBy: event.reason ?? "superseded" }),
  });
}

addRow({
  id: "message-resolved-refused",
  from: "refused",
  trigger: "MESSAGE_RESOLVED",
  guardName: "always",
  guard: () => true,
  to: "resolved",
  apply: (m, event) => settle(m!, "resolved", event.by ?? "evaluator", event),
});

export const MESSAGE_TRANSITIONS: readonly MessageTransitionRow[] = rows;

export function messageRowsFor(from: MessageState | "none", trigger: MessageEventType): MessageTransitionRow[] {
  return MESSAGE_TRANSITIONS.filter((r) => r.from === from && r.trigger === trigger);
}

// ---------------------------------------------------------------------------
// Applying one message event to a phase's message list
// ---------------------------------------------------------------------------

export interface MessageApplyOk {
  ok: true;
  messages: Message[];
}
export interface MessageApplyRejected {
  ok: false;
  reason: string;
}
export type MessageApplyResult = MessageApplyOk | MessageApplyRejected;

function findMessage(messages: Message[], id: string): Message | undefined {
  return messages.find((m) => m.id === id);
}

/** The content-hash/version binding every verdict-like event carries. Returns
 * a human-readable reason when the event is stale. A verdict bound to a
 * pre-carry version of an UNCHANGED message is not stale: versionContentHashes
 * records that the two versions hash to the same content. */
export function checkMessageBinding(
  message: Message,
  event: {
    boundCandidateSha: string;
    boundContractVersion: ContractVersion;
    boundRecordVersion: number;
  },
): { ok: true } | { ok: false; reason: string } {
  // An invalidation only blocks a verdict naming the OLD version; a new
  // verdict on the current version is exactly what the invalidation asked for.
  if (message.invalidated && event.boundRecordVersion !== message.messageVersion) {
    return { ok: false, reason: `message ${message.id} was invalidated (${message.invalidated.reason}); it needs a new verdict` };
  }
  const sameContent =
    event.boundRecordVersion === message.messageVersion ||
    message.versionContentHashes?.[event.boundRecordVersion] === message.contentHash;
  if (!sameContent) {
    return {
      ok: false,
      reason: `message ${message.id} changed v${event.boundRecordVersion} → v${message.messageVersion} since you viewed it`,
    };
  }
  const carried = message.carriedFrom?.some(
    (c) => c.candidateSha === event.boundCandidateSha && c.version === event.boundRecordVersion,
  );
  if (event.boundCandidateSha !== message.boundCandidateSha && !carried) {
    return {
      ok: false,
      reason: `message ${message.id} is bound to candidate ${event.boundCandidateSha}, but the phase is now at candidate ${message.boundCandidateSha} — it changed since you viewed it`,
    };
  }
  if (!sameVersion(event.boundContractVersion, message.boundContractVersion)) {
    return {
      ok: false,
      reason: `message ${message.id} is bound to contract v${event.boundContractVersion.snapshot}, but the phase is now at contract v${message.boundContractVersion.snapshot} — it changed since you viewed it`,
    };
  }
  return { ok: true };
}

/** Applies one MESSAGE_* or OWNER_VERDICT event to `messages`. Pure: the
 * caller (reduce.ts) owns the phase-level side effects (an owner refusal
 * during REVIEWING raises a blocking finding). Binding is checked first,
 * with a visible reason, then the transition table decides. */
export function applyMessageEvent(messages: Message[], event: MessageEventLike & Record<string, unknown>): MessageApplyResult {
  const id = event.messageId as string | undefined;

  if (event.type === "MESSAGE_RAISED") {
    const message = (event as unknown as { message: Message }).message;
    if (!message || typeof message.id !== "string" || message.id.length === 0) {
      return { ok: false, reason: "a MESSAGE_RAISED event must carry a message with an id" };
    }
    if (findMessage(messages, message.id)) {
      return { ok: false, reason: `message ${message.id} already exists` };
    }
    const row = messageRowsFor("none", "MESSAGE_RAISED")[0];
    return { ok: true, messages: [...messages, row.apply(undefined, event)] };
  }

  if (typeof id !== "string" || id.length === 0) {
    return { ok: false, reason: `${event.type} must name its messageId` };
  }
  const message = findMessage(messages, id);
  if (!message) return { ok: false, reason: `unknown message ${id}` };

  // Every event after MESSAGE_RAISED is bound to a version of the message.
  if (event.type === "MESSAGE_CARRIED") {
    return applyCarry(messages, message, event as unknown as MessageCarryEvent);
  }
  const binding = checkMessageBinding(message, event as unknown as { boundCandidateSha: string; boundContractVersion: ContractVersion; boundRecordVersion: number });
  if (!binding.ok) return { ok: false, reason: binding.reason };

  const matching = messageRowsFor(message.state, event.type).find((r) => {
    try {
      return r.guard(message, event);
    } catch {
      return false;
    }
  });
  if (!matching) {
    return {
      ok: false,
      reason: `event '${event.type}' has no rule from message ${id}'s state '${message.state}'`,
    };
  }
  const updated = matching.apply(message, event);
  return { ok: true, messages: messages.map((m) => (m.id === id ? updated : m)) };
}

// ---------------------------------------------------------------------------
// Carry
// ---------------------------------------------------------------------------

export interface MessageCarryEvent {
  type: "MESSAGE_CARRIED";
  messageId: string;
  fromCandidate: string;
  toCandidate: string;
  fromVersion: number;
  toVersion: number;
  contentHash: string;
  unchanged: boolean;
  /** The message's reviewable content as of the new version. Present only
   * when it changed; the conductor re-derives it from the underlying record
   * so `contentHash` is not asserted blindly. */
  content?: MessageContent;
}

/** Applies one MESSAGE_CARRIED: bumps the version to the new candidate, and
 * carries the settlement exactly when the content is unchanged AND the
 * message's contract version is the phase's current one. Otherwise the
 * settlement is invalidated and the message needs a new verdict. */
export function applyCarry(messages: Message[], message: Message, event: MessageCarryEvent): MessageApplyResult {
  if (event.fromVersion !== message.messageVersion) {
    return {
      ok: false,
      reason: `carry of message ${message.id} names v${event.fromVersion}, but it is v${message.messageVersion}`,
    };
  }
  if (event.toVersion !== event.fromVersion + 1) {
    return { ok: false, reason: `carry of message ${message.id} must bump the version by one` };
  }
  // A changed content is carried as data, not just a different hash, or the
  // stored fields and the hash would disagree.
  let content: Partial<MessageContent> = {};
  if (event.content) {
    const hash = contentHashOf(event.content);
    if (hash !== event.contentHash) {
      return {
        ok: false,
        reason: `carry of message ${message.id} declares contentHash ${event.contentHash}, but its content hashes to ${hash}`,
      };
    }
    content = { ...event.content };
  } else if (event.contentHash !== message.contentHash) {
    return {
      ok: false,
      reason: `carry of message ${message.id} changes contentHash without carrying the new content`,
    };
  }
  const versionContentHashes = { ...(message.versionContentHashes ?? {}), [event.fromVersion]: message.contentHash };
  const carriedFrom = [...(message.carriedFrom ?? []), { candidateSha: event.fromCandidate, version: event.fromVersion }];
  const carried: Message = {
    ...message,
    ...content,
    messageVersion: event.toVersion,
    boundCandidateSha: event.toCandidate,
    contentHash: event.contentHash,
    sourceContentHash: event.sourceContentHash ?? message.sourceContentHash,
    versionContentHashes,
    carriedFrom,
  };
  return { ok: true, messages: messages.map((m) => (m.id === message.id ? carried : m)) };
}

/** Carry with the phase's current contract version, so the "contract
 * amended" branch can be decided (reduce.ts has the phase). */
export function applyCarryWithContract(
  messages: Message[],
  message: Message,
  event: MessageCarryEvent,
  currentContractVersion: ContractVersion,
): MessageApplyResult {
  const base = applyCarry(messages, message, event);
  if (!base.ok) return base;
  const carried = base.messages.find((m) => m.id === message.id)!;
  const contractSame = sameVersion(message.boundContractVersion, currentContractVersion);
  const next: Message = { ...carried, boundContractVersion: currentContractVersion };
  if (!next.settlement) return { ok: true, messages: base.messages.map((m) => (m.id === message.id ? next : m)) };

  // An already-invalidated settlement is never rebound to a later candidate:
  // the ledger reports it under the bindings it was actually settled under.
  if (message.invalidated) {
    return { ok: true, messages: base.messages.map((m) => (m.id === message.id ? next : m)) };
  }

  const contentChanged = !(event.unchanged && event.contentHash === message.contentHash);
  if (!contentChanged && contractSame) {
    // The settlement stays: rebind it to the new version/candidate.
    return {
      ok: true,
      messages: base.messages.map((m) =>
        m.id === message.id
          ? {
              ...next,
              settlement: {
                ...next.settlement!,
                candidateSha: event.toCandidate,
                contractVersion: currentContractVersion,
                messageVersion: event.toVersion,
              },
            }
          : m,
      ),
    };
  }
  // The settlement is kept in the ledger (who settled it, and under which
  // bindings) but marked invalidated; the message itself returns to
  // `published` and needs a new verdict.
  const reason = !contractSame ? "contract amended" : "content changed";
  return {
    ok: true,
    messages: base.messages.map((m) =>
      m.id === message.id
        ? {
            ...next,
            state: "published",
            invalidated: { reason: reason as "content changed" | "contract amended", atCandidate: event.toCandidate },
          }
        : m,
    ),
  };
}

// ---------------------------------------------------------------------------
// The settled ledger (contract §2)
// ---------------------------------------------------------------------------

export interface LedgerEntry {
  messageId: string;
  type: MessageType;
  state: MessageState;
  settledBy: MessageSettlement["settledBy"];
  reason?: string;
  candidateSha: string;
  contractVersion: ContractVersion;
  messageVersion: number;
  contentHash: string;
  invalidated?: { reason: "content changed" | "contract amended"; atCandidate: string };
  /** Set when the message was later superseded: its settlement is kept, but
   * the ledger marks that it no longer stands. */
  supersededBy?: string;
  /** A refusal recorded after the phase reached DONE: a follow-up, not a
   * blocker. */
  followUp?: boolean;
  /** Plan 04a item 4: the evaluator's report on an owner-refused message —
   * whether this candidate addressed the owner's reason. `false` leaves the
   * refusal standing but records that it was checked. */
  addressed?: boolean;
  addressedReason?: string;
}

/** Every settled message, in id order. A settlement is a terminal message
 * state carrying a `settlement`; an invalidated carry keeps the entry with
 * its `invalidated` reason, never silently dropping it. */
export function ledgerEntries(messages: Message[]): LedgerEntry[] {
  return [...messages]
    .filter((m) => m.settlement !== undefined)
    .sort((a, b) => a.id.localeCompare(b.id))
    .map((m) => {
      const s = m.settlement!;
      return {
        messageId: m.id,
        type: m.type,
        // The settled state, not the message's current one: an invalidated
        // entry still says how it was settled (the `invalidated` field says
        // that settlement no longer stands).
        state: s.state,
        settledBy: s.settledBy,
        ...(s.reason !== undefined ? { reason: s.reason } : {}),
        candidateSha: s.candidateSha,
        contractVersion: s.contractVersion,
        messageVersion: s.messageVersion,
        contentHash: s.contentHash,
        ...(m.invalidated ? { invalidated: m.invalidated } : {}),
        ...(m.supersededBy ? { supersededBy: m.supersededBy } : {}),
        ...(m.followUp ? { followUp: true } : {}),
        ...(m.addressedReport
          ? { addressed: m.addressedReport.addressed, ...(m.addressedReport.reason ? { addressedReason: m.addressedReport.reason } : {}) }
          : {}),
      };
    });
}

function stableStringify(value: unknown): string {
  return JSON.stringify(value);
}

/** `messages.jsonl`: one message per line, id order — the projection a front
 * end reads. Rebuilt from state, never authoritative. */
export function projectMessages(phase: { messages?: Message[] }): string {
  const messages = [...(phase.messages ?? [])].sort((a, b) => a.id.localeCompare(b.id));
  return messages.map((m) => stableStringify(m)).join("\n") + (messages.length > 0 ? "\n" : "");
}

/** `ledger.jsonl`: one settled entry per line, id order. */
export function projectLedger(phase: { messages?: Message[] }): string {
  const entries = ledgerEntries(phase.messages ?? []);
  return entries.map((e) => stableStringify(e)).join("\n") + (entries.length > 0 ? "\n" : "");
}

/** The `messageId` prefix for a message type: T-n, F-n, B-n. */
export function messageIdPrefix(type: MessageType, index: number): string {
  const letter = type === "tradeoff" ? "T" : type === "finding" ? "F" : "B";
  return `${letter}-${index}`;
}
