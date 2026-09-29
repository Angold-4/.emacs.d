// Plan 05j: the review ledger's entries. One entry is ONE TOPIC. A topic is
// the thing the owner actually decides on — a trade-off the implementation
// made, a finding against the plan or the code, or a blocker that stops
// acceptance — no matter which message type raised it and no matter how many
// rounds re-raised it under a new id.
//
// The problem this module solves (15a's first round, run 1989d1d0): about six
// distinct points reached the owner as nineteen entries. Merged and dropped
// messages were still rendered; the SAME point arrived as a blocker, a
// trade-off and a finding (B-1 = T-14 = F-1) because evaluators run per type;
// and every later round re-raised an old point under a new id. Six points, 31
// raw messages, 19 rows.
//
// An entry fixes that by being a pure projection of the event log:
//
//   ENTRY_OPENED          open one topic, with its anchor
//   MESSAGE_LINKED        link one message to an entry it shares an anchor with
//   ENTRY_RETITLED        the owner (or curator) improves the title
//   ENTRY_SPLIT           the owner pulls one linked message into its own entry
//   ENTRY_STATE           open | resolved in <sha> | dropped with a reason
//   ENTRY_MERGED_BY_OWNER the owner merges two near-duplicate entries
//
// No agent ever writes a view: `review.org` and `programs/<id>/views/review.org`
// are rendered from the entries, and the same events render byte-identical
// views (see renderEntryReview). Nothing is dropped silently: a message that is
// not linked opens its own entry, a message that was dropped carries its
// reason, and `accounting()` reconciles the raw count to zero unaccounted.
//
// Anchors. Every message has a normalised anchor derived from its evidence:
// a file and a line range, a decision id, or a plan clause. A link is accepted
// only when the message and the entry share an anchor — overlapping line
// ranges in the same file, the same decision id, or the same plan clause.
// Anything else is refused and logged, and the message opens its own entry.
// Near-duplicates that share no anchor are never merged by the curator: the
// entry carries a deterministic `≈ E-n` hint instead, and only the owner's `m`
// merges them (as ENTRY_MERGED_BY_OWNER).

import type { Decision, DecisionBrief, Message, MessageType, OwnerRequest } from "./types.ts";
import { renderBriefOrg, renderBriefsSection } from "./briefs.ts";

// ---------------------------------------------------------------------------
// Anchors
// ---------------------------------------------------------------------------

export interface FileAnchor {
  kind: "file";
  path: string;
  /** Inclusive [start, end] 1-based lines. */
  lines: [number, number];
}
export interface DecisionAnchor {
  kind: "decision";
  id: string;
}
export interface PlanAnchor {
  kind: "plan";
  clause: string;
}
/** A message that carries NO real anchor (no file:line evidence, no decision
 * id, no plan clause). It still gets an entry, so nothing is hidden, but the
 * anchor is the message's own id: two such messages never merge, and the
 * lint reports the entry as having no anchor rather than letting a made-up
 * one pass (findings A-34, M-38). */
export interface MessageAnchor {
  kind: "message";
  id: string;
}
export type EntryAnchor = FileAnchor | DecisionAnchor | PlanAnchor | MessageAnchor;

/** How fresh a file anchor is against the newest candidate. */
export type AnchorFreshness = "fresh" | "stale" | "unverified";

function normalisePath(path: string): string {
  return path.replace(/\\/g, "/").replace(/^\.\//, "").trim();
}

/** Parse one evidence string into a file anchor, when it names a file and a
 * line (or line range). `src/cancel.rs:88-104 the frame counter` → file
 * anchor. Anything else yields undefined. */
export function anchorFromEvidence(evidence: string): FileAnchor | undefined {
  const m = /^(\S+?):(\d+)(?:-(\d+))?(?:\s|$)/.exec(evidence.trim());
  if (!m) return undefined;
  const path = normalisePath(m[1]);
  if (path.length === 0) return undefined;
  const start = Number(m[2]);
  const end = m[3] !== undefined ? Number(m[3]) : start;
  if (!Number.isFinite(start) || !Number.isFinite(end) || start < 1 || end < start) return undefined;
  return { kind: "file", path, lines: [start, end] };
}

/** Every normalised anchor a message carries: its explicit `anchor` (plan
 * 04a), every evidence line that names a file, its backing decision id, and
 * its plan clause. Deduplicated, in a deterministic order (file, decision,
 * plan), so two runs derive the same anchors from the same message. */
export function anchorsOfMessage(message: Message): EntryAnchor[] {
  const out: EntryAnchor[] = [];
  const push = (a: EntryAnchor | undefined) => {
    if (!a) return;
    if (!out.some((x) => anchorsEqual(x, a))) out.push(a);
  };
  if (message.anchor) push({ kind: "file", path: normalisePath(message.anchor.path), lines: message.anchor.lines });
  for (const ev of message.evidence ?? []) push(anchorFromEvidence(ev));
  const record = message.sourceRecordId;
  if (record && /^D[-\w]/.test(record)) push({ kind: "decision", id: record });
  // A planRef equal to the phase id is a placeholder the conductor once set on
  // every message; treating it as a plan clause made two unrelated prose
  // findings share ONE anchor and merge (finding A-34).
  const planRef = message.planRef?.trim() ?? "";
  if (planRef.length > 0 && planRef !== message.phaseId) push({ kind: "plan", clause: normalisePlanClause(planRef) });
  return out;
}

function normalisePlanClause(clause: string): string {
  return clause.trim().replace(/\s+/g, " ");
}

export function anchorsEqual(a: EntryAnchor, b: EntryAnchor): boolean {
  if (a.kind !== b.kind) return false;
  if (a.kind === "file" && b.kind === "file") return a.path === b.path && a.lines[0] === b.lines[0] && a.lines[1] === b.lines[1];
  if (a.kind === "decision" && b.kind === "decision") return a.id === b.id;
  if (a.kind === "message" && b.kind === "message") return a.id === b.id;
  if (a.kind === "plan" && b.kind === "plan") return normalisePlanClause(a.clause) === normalisePlanClause(b.clause);
  return false;
}

/** Overlapping line ranges in the same file, the same decision id, or the
 * same plan clause. */
export function anchorsOverlap(a: EntryAnchor, b: EntryAnchor): boolean {
  if (a.kind !== b.kind) return false;
  if (a.kind === "file" && b.kind === "file") {
    return a.path === b.path && a.lines[0] <= b.lines[1] && b.lines[0] <= a.lines[1];
  }
  if (a.kind === "decision" && b.kind === "decision") return a.id === b.id;
  if (a.kind === "plan" && b.kind === "plan") return normalisePlanClause(a.clause) === normalisePlanClause(b.clause);
  // A no-anchor message's anchor is its own id: only itself.
  if (a.kind === "message" && b.kind === "message") return a.id === b.id;
  return false;
}

/** The anchor two message/entry anchors share, if any. */
export function sharedAnchor(a: readonly EntryAnchor[], b: readonly EntryAnchor[]): EntryAnchor | undefined {
  for (const x of a) for (const y of b) if (anchorsOverlap(x, y)) return x;
  return undefined;
}

export function anchorKey(a: EntryAnchor): string {
  if (a.kind === "file") return `file:${a.path}:${a.lines[0]}-${a.lines[1]}`;
  if (a.kind === "decision") return `decision:${a.id}`;
  if (a.kind === "message") return `message:${a.id}`;
  return `plan:${normalisePlanClause(a.clause)}`;
}

export function formatAnchor(a: EntryAnchor | undefined): string {
  if (!a) return "no anchor";
  if (a.kind === "file") return `${a.path}:${a.lines[0]}-${a.lines[1]}`;
  if (a.kind === "decision") return a.id;
  if (a.kind === "message") return "no anchor";
  return a.clause;
}

// ---------------------------------------------------------------------------
// Entry records and their events
// ---------------------------------------------------------------------------

export type EntryType = "blocker" | "finding" | "tradeoff";
export type EntryStateName = "open" | "resolved" | "dropped";

export interface EntryLink {
  messageId: string;
  /** The anchor the message and the entry share (the reason the link was
   * accepted). */
  anchor: EntryAnchor;
  reason: string;
  by?: string;
  at?: string;
}

export interface Entry {
  id: string;
  phaseId: string;
  /** The program an entry belongs to, when it was opened in a program run, so
   * the program view can tag it. */
  programId?: string;
  title: string;
  /** The highest type among the linked messages (see entryTypeOf). Stored so
   * a split keeps the type it was opened with; computed for the view. */
  type: EntryType;
  state: EntryStateName;
  /** The candidate a `resolved`/`dropped` state was recorded against. */
  stateSha?: string;
  stateReason?: string;
  /** The entry's own anchor, the one a proposed link is checked against. */
  anchor: EntryAnchor;
  links: EntryLink[];
  /** The entry this one was merged into by the owner. */
  mergedInto?: string;
  /** The entry this one was split from. */
  splitFrom?: string;
  createdBy?: string;
  createdAt?: string;
}

export type EntryEvent =
  | {
      type: "ENTRY_OPENED";
      entryId?: string;
      phaseId: string;
      programId?: string;
      title: string;
      entryType?: EntryType;
      anchor?: EntryAnchor;
      /** The message that opens the entry, linked in the same event. */
      messageId?: string;
      by?: string;
      at?: string;
    }
  | {
      type: "MESSAGE_LINKED";
      messageId: string;
      entryId: string;
      anchor: EntryAnchor;
      reason: string;
      by?: string;
      at?: string;
    }
  | { type: "ENTRY_RETITLED"; entryId: string; title: string; by?: string; at?: string }
  | { type: "ENTRY_SPLIT"; entryId: string; messageId: string; newEntryId?: string; title?: string; anchor?: EntryAnchor; by?: string; at?: string }
  | { type: "ENTRY_STATE"; entryId: string; state: EntryStateName; sha?: string; reason?: string; by?: string; at?: string }
  | { type: "ENTRY_MERGED_BY_OWNER"; entryId: string; intoEntryId: string; by?: string; at?: string };

export interface EntryApplyOk {
  ok: true;
  entries: Entry[];
  /** Set when the event refused a proposed link (the message then opens its
   * own entry). The caller logs this. */
  refused?: { messageId: string; entryId: string; reason: string };
}
export interface EntryApplyRejected {
  ok: false;
  reason: string;
}
export type EntryApplyResult = EntryApplyOk | EntryApplyRejected;

export function nextEntryId(entries: readonly Entry[]): string {
  const max = entries.reduce((m, e) => Math.max(m, Number(e.id.match(/^E-(\d+)$/)?.[1] ?? 0)), 0);
  return `E-${max + 1}`;
}

function findEntry(entries: readonly Entry[], id: string): Entry | undefined {
  return entries.find((e) => e.id === id);
}

/** The type of an entry, by the existing rules: a blocker if a blocker vote
 * or a blocking finding stops acceptance; otherwise a finding if any linked
 * message is a defect against the plan or the code; otherwise a trade-off —
 * the implementation's choice where it departs from or fills a gap in the
 * plan. Type is not the owner's to change and never lowers: a blocker linked
 * to a finding entry makes the entry a blocker. */
export function entryTypeOf(messages: readonly Message[]): EntryType {
  let type: EntryType = "tradeoff";
  for (const m of messages) {
    if (m.type === "blocker" && (m.raisedAsBlocker === true || isBlocking(m))) return "blocker";
    if (m.type === "finding" && isBlocking(m)) return "blocker";
    if (m.type === "finding") type = "finding";
    if (m.type === "blocker") type = type === "tradeoff" ? "finding" : type;
  }
  return type;
}

function isBlocking(m: Message): boolean {
  // A message carries no severity of its own (the finding it came from does);
  // `raisedAsBlocker` is the one blocker mark a message owns.
  return m.raisedAsBlocker === true || m.type === "blocker";
}

export type LinkCheck = { ok: true; anchor: EntryAnchor } | { ok: false; reason: string };

/** Validate a proposed link: the message and the entry must share an anchor.
 * Returns the shared anchor or a refusal reason (which the caller logs, and
 * the message opens its own entry). */
export function validateLink(entry: Entry, message: Message, proposed?: EntryAnchor): LinkCheck {
  const messageAnchors = anchorsOfMessage(message);
  const entryAnchors = entry.anchor ? [entry.anchor] : [];
  // A no-anchor entry holds exactly the message it names.
  if (entry.anchor.kind === "message") {
    return entry.anchor.id === message.id
      ? { ok: true, anchor: entry.anchor }
      : { ok: false, reason: `entry ${entry.id} has no real anchor and holds only ${entry.anchor.id}` };
  }
  // The message and the ENTRY must share an anchor. A proposed anchor that
  // matches neither (or matches only the entry) is not a shared anchor and
  // does not make the link valid.
  const shared = sharedAnchor(entryAnchors, messageAnchors);
  if (!shared || (proposed && !anchorsOverlap(proposed, shared))) {
    return {
      ok: false,
      reason: `message ${message.id} and entry ${entry.id} share no anchor (${formatAnchor(entry.anchor)} vs ${messageAnchors.map(formatAnchor).join(", ") || "none"})`,
    };
  }
  return { ok: true, anchor: shared };
}

/** Applies one entry event. Pure; the caller (reduce.ts) owns phase state.
 * A link that fails the anchor rule is NOT an error: the event is accepted as
 * a refusal (`refused`) so the runtime can log it and let the message open its
 * own entry — never silently merged, never hidden. */
export function applyEntryEvent(entries: readonly Entry[], event: EntryEvent, messages: readonly Message[] = []): EntryApplyResult {
  switch (event.type) {
    case "ENTRY_OPENED": {
      const id = event.entryId ?? nextEntryId(entries);
      if (findEntry(entries, id)) return { ok: false, reason: `entry ${id} already exists` };
      if (typeof event.title !== "string" || event.title.trim().length === 0) {
        return { ok: false, reason: `entry ${id} must have a non-empty title` };
      }
      const opener = event.messageId ? messages.find((m) => m.id === event.messageId) : undefined;
      // Opening a NEW entry for a message an open entry already holds would
      // be a split in disguise, which the plan reserves to the owner (OD-2 /
      // finding M-22). Refused, never applied.
      if (opener) {
        const holder = entries.find((e) => e.state === "open" && e.links.some((l) => l.messageId === opener.id));
        if (holder) return { ok: false, reason: `message ${opener.id} already belongs to entry ${holder.id}; splitting is the owner's action` };
      }
      const anchor = event.anchor ?? (opener ? anchorsOfMessage(opener)[0] : undefined);
      if (!anchor) return { ok: false, reason: `entry ${id} has no anchor (no message to derive one from)` };
      const link: EntryLink[] = [];
      if (opener) link.push({ messageId: opener.id, anchor, reason: "opened", by: event.by, at: event.at });
      const type = event.entryType ?? (opener ? entryTypeOf([opener]) : "tradeoff");
      const entry: Entry = {
        id,
        phaseId: event.phaseId,
        ...(event.programId ? { programId: event.programId } : {}),
        title: event.title.trim(),
        type,
        state: "open",
        anchor,
        links: link,
        ...(event.by ? { createdBy: event.by } : {}),
        ...(event.at ? { createdAt: event.at } : {}),
      };
      return { ok: true, entries: [...entries, entry] };
    }
    case "MESSAGE_LINKED": {
      const entry = findEntry(entries, event.entryId);
      if (!entry) return { ok: false, reason: `unknown entry ${event.entryId}` };
      const message = messages.find((m) => m.id === event.messageId);
      if (!message) return { ok: false, reason: `unknown message ${event.messageId}` };
      const check = validateLink(entry, message, event.anchor);
      if (!check.ok) {
        return { ok: true, entries: [...entries], refused: { messageId: message.id, entryId: entry.id, reason: check.reason } };
      }
      if (entry.links.some((l) => l.messageId === message.id)) return { ok: true, entries: [...entries] };
      const anchor = check.anchor;
      const link: EntryLink = { messageId: message.id, anchor, reason: event.reason, ...(event.by ? { by: event.by } : {}), ...(event.at ? { at: event.at } : {}) };
      // One message belongs to one entry: linking it here removes it from any
      // other entry (the auto-opened one a reviewer's `sameAs` supersedes).
      return {
        ok: true,
        entries: entries.map((e) => {
          if (e.id === entry.id) return { ...e, links: [...e.links, link] };
          if (e.links.some((l) => l.messageId === message.id)) return { ...e, links: e.links.filter((l) => l.messageId !== message.id) };
          return e;
        }),
      };
    }
    case "ENTRY_RETITLED": {
      const entry = findEntry(entries, event.entryId);
      if (!entry) return { ok: false, reason: `unknown entry ${event.entryId}` };
      if (typeof event.title !== "string" || event.title.trim().length === 0) {
        return { ok: false, reason: `entry ${entry.id} must keep a non-empty title` };
      }
      return { ok: true, entries: entries.map((e) => (e.id === entry.id ? { ...e, title: event.title.trim() } : e)) };
    }
    case "ENTRY_SPLIT": {
      const entry = findEntry(entries, event.entryId);
      if (!entry) return { ok: false, reason: `unknown entry ${event.entryId}` };
      const link = entry.links.find((l) => l.messageId === event.messageId);
      if (!link) return { ok: false, reason: `entry ${entry.id} does not link message ${event.messageId}` };
      if (entry.links.length < 2) return { ok: false, reason: `entry ${entry.id} has only one message; nothing to split` };
      const id = event.newEntryId ?? nextEntryId(entries);
      if (findEntry(entries, id)) return { ok: false, reason: `entry ${id} already exists` };
      const message = messages.find((m) => m.id === event.messageId);
      // The split entry takes the SPLIT MESSAGE's own anchor, not the source
      // entry's (OD-2 / finding A-25): the owner has decided they are two
      // separate topics, so the new entry must stand on its own anchor.
      const own = message ? anchorForMessage(message) : undefined;
      const anchor = event.anchor ?? own ?? link.anchor;
      const split: Entry = {
        id,
        phaseId: entry.phaseId,
        ...(entry.programId ? { programId: entry.programId } : {}),
        title: (event.title ?? (message ? message.title : entry.title)).trim(),
        type: message ? entryTypeOf([message]) : entry.type,
        state: "open",
        anchor,
        links: [{ ...link, reason: "split" }],
        splitFrom: entry.id,
        ...(event.by ? { createdBy: event.by } : {}),
        ...(event.at ? { createdAt: event.at } : {}),
      };
      return {
        ok: true,
        entries: [...entries.filter((e) => e.id !== entry.id), { ...entry, links: entry.links.filter((l) => l.messageId !== event.messageId) }, split],
      };
    }
    case "ENTRY_STATE": {
      const entry = findEntry(entries, event.entryId);
      if (!entry) return { ok: false, reason: `unknown entry ${event.entryId}` };
      return {
        ok: true,
        entries: entries.map((e) =>
          e.id === entry.id ? { ...e, state: event.state, stateSha: event.sha, stateReason: event.reason } : e,
        ),
      };
    }
    case "ENTRY_MERGED_BY_OWNER": {
      const entry = findEntry(entries, event.entryId);
      const into = findEntry(entries, event.intoEntryId);
      if (!entry) return { ok: false, reason: `unknown entry ${event.entryId}` };
      if (!into) return { ok: false, reason: `unknown entry ${event.intoEntryId}` };
      if (entry.id === into.id) return { ok: false, reason: "an entry cannot merge into itself" };
      // The absorbed entry's links move into the target; the absorbed entry is
      // marked merged, never deleted (its history stays in the log).
      const moved = entry.links.map((l) => ({ ...l, reason: `merged from ${entry.id}`, by: event.by ?? "owner", at: event.at }));
      return {
        ok: true,
        entries: entries.map((e) => {
          // The links MOVE, not copy: the source keeps none, so the message
          // is linked to exactly one (live) entry after the merge (M-14).
          if (e.id === entry.id) return { ...e, state: "dropped", stateReason: `merged into ${into.id}`, mergedInto: into.id, links: [] };
          if (e.id === into.id) {
            const existing = new Set(e.links.map((l) => l.messageId));
            return { ...e, links: [...e.links, ...moved.filter((l) => !existing.has(l.messageId))] };
          }
          return e;
        }),
      };
    }
  }
}

/** The full list of entry event types, for the curator tool's allow-list. */
export const ENTRY_EVENT_TYPES = [
  "ENTRY_OPENED",
  "MESSAGE_LINKED",
  "ENTRY_RETITLED",
  "ENTRY_SPLIT",
  "ENTRY_STATE",
  "ENTRY_MERGED_BY_OWNER",
] as const;

// ---------------------------------------------------------------------------
// Projection: entries + messages → live views, with conservation
// ---------------------------------------------------------------------------

/** A message's own lifecycle state, as the entry view cares about it. */
export type MessageLiveness = "live" | "merged" | "dropped" | "resolved" | "superseded";

export function messageLiveness(m: Message): MessageLiveness {
  switch (m.state) {
    case "merged":
      return "merged";
    case "dropped":
      return "dropped";
    case "resolved":
      return "resolved";
    case "superseded":
      return "superseded";
    default:
      return "live";
  }
}

/** Plan 05j: the curator's allow-list. A curator agent (the evaluator's
 * model) sees every new raw message of every type plus every open entry of
 * the program and may ONLY propose `link`, `open` and `retitle`. It cannot
 * drop, resolve or change a type — those are the owner's, the evaluator's and
 * the state at projection time. */
export type CuratorOp = "link" | "open" | "retitle";
export const CURATOR_OPS: readonly CuratorOp[] = ["link", "open", "retitle"];

export interface CuratorProposal {
  op: string;
  entryId?: string;
  messageId?: string;
  title?: string;
  anchor?: EntryAnchor;
  reason?: string;
}

export type CuratorValidation = { ok: true; op: CuratorOp } | { ok: false; reason: string };

/** Validates one curator proposal against the allow-list. Anything else —
 * `drop`, `resolve`, `state`, `type`, `merge` — is refused with a reason. */
export function validateCuratorProposal(proposal: CuratorProposal): CuratorValidation {
  const op = typeof proposal?.op === "string" ? proposal.op : "";
  if (!CURATOR_OPS.includes(op as CuratorOp)) {
    return { ok: false, reason: `the curator may only ${CURATOR_OPS.join(", ")}; '${op || "(none)"}' is not allowed` };
  }
  if (op === "link" && (!proposal.entryId || !proposal.messageId)) {
    return { ok: false, reason: "a link proposal must name its messageId and entryId" };
  }
  if (op === "retitle" && !proposal.entryId) {
    // A retitle with no entryId would emit an undefined target and, under the
    // batch dry run, discard every other proposal (findings A-36, M-31).
    return { ok: false, reason: "a retitle proposal must name its entryId" };
  }
  if ((op === "open" || op === "retitle") && typeof proposal.title !== "string") {
    return { ok: false, reason: `a ${op} proposal must carry a title` };
  }
  return { ok: true, op: op as CuratorOp };
}

/** Turns a validated curator proposal into the entry event the conductor
 * applies. A proposal that fails validation is refused here, never applied. */
export function curatorEvent(proposal: CuratorProposal, phaseId: string): { ok: true; event: EntryEvent } | { ok: false; reason: string } {
  const valid = validateCuratorProposal(proposal);
  if (!valid.ok) return valid;
  switch (valid.op) {
    case "link":
      return {
        ok: true,
        event: { type: "MESSAGE_LINKED", messageId: proposal.messageId!, entryId: proposal.entryId!, anchor: proposal.anchor!, reason: proposal.reason ?? "curator" , by: "curator" },
      };
    case "open":
      return {
        ok: true,
        event: { type: "ENTRY_OPENED", phaseId, title: proposal.title!, messageId: proposal.messageId, ...(proposal.anchor ? { anchor: proposal.anchor } : {}), by: "curator" },
      };
    case "retitle":
      return { ok: true, event: { type: "ENTRY_RETITLED", entryId: proposal.entryId!, title: proposal.title!, by: "curator" } };
  }
}

/** Plan 05j: the owner's A/D on an entry is a verdict on each linked
 * message, not a new entry state: approving a trade-off is not "resolved in a
 * sha" (a fix, 05e) and it keeps the message's full binding (record M-12).
 * Only published (and owner-refused) messages can be settled; a raw one is
 * skipped and its id reported. */
export interface EntryVerdictPlan {
  events: Array<{
    type: "OWNER_VERDICT";
    messageId: string;
    verdict: "accept" | "refuse";
    reason?: string;
    boundCandidateSha: string;
    boundContractVersion: Message["boundContractVersion"];
    boundRecordVersion: number;
  }>;
  skipped: string[];
}

export function entryVerdictEvents(
  entry: Entry,
  messages: readonly Message[],
  verdict: "accept" | "refuse",
  reason?: string,
): EntryVerdictPlan {
  const byId = new Map(messages.map((m) => [m.id, m]));
  const events: EntryVerdictPlan["events"] = [];
  const skipped: string[] = [];
  for (const link of entry.links) {
    const message = byId.get(link.messageId);
    if (!message) {
      skipped.push(link.messageId);
      continue;
    }
    if (message.state !== "published" && message.state !== "refused") {
      skipped.push(message.id);
      continue;
    }
    events.push({
      type: "OWNER_VERDICT",
      messageId: message.id,
      verdict,
      ...(reason ? { reason } : {}),
      boundCandidateSha: message.boundCandidateSha,
      boundContractVersion: message.boundContractVersion,
      boundRecordVersion: message.messageVersion,
    });
  }
  return { events, skipped };
}

export interface EntryView {
  entry: Entry;
  type: EntryType;
  /** The entry's state against the newest candidate. */
  state: EntryStateName;
  /** True when the entry is live (open) and still shown. */
  live: boolean;
  messages: Message[];
  anchor: EntryAnchor;
  /** The anchor's file or lines no longer exist in the newest candidate. */
  staleAnchor: boolean;
  /** The candidate checkout could not be read, so the anchor's freshness
   * could not be re-checked. Shown as `anchor unverified`, never silently
   * treated as fresh (record A-68). */
  unverifiedAnchor: boolean;
  /** The near-duplicate hint (`≈ E-4`) the view carries, if any. */
  hint?: string;
  raisedBy: string[];
  staleState: boolean;
}

export interface Accounting {
  raw: number;
  entries: number;
  linked: number;
  dropped: number;
  merged: number;
  resolved: number;
  unaccounted: number;
  /** Trade-offs raised by reviewers that the worker did not raise. */
  unexposed: number;
}

export interface ProjectedEntries {
  views: EntryView[];
  accounting: Accounting;
  /** Links that failed the anchor rule, as logged refusals. */
  refusedLinks: Array<{ messageId: string; entryId: string; reason: string }>;
}

export interface ProjectEntriesOptions {
  messages?: readonly Message[];
  entries?: readonly Entry[];
  /** The newest candidate the view reflects (the header names it). */
  newestCandidateSha?: string;
  /** Whether a file anchor still resolves in the newest candidate. Defaults
   * to "yes" (nothing known to be missing). Kept for callers that only need
   * the two-way answer; `anchorFreshness` distinguishes "could not check". */
  anchorResolves?: (anchor: EntryAnchor) => boolean;
  /** The three-way answer: a file anchor is fresh, stale (its file or lines
   * are gone), or unverified (the candidate checkout could not be read). */
  anchorFreshness?: (anchor: EntryAnchor) => AnchorFreshness;
  /** The deterministic near-duplicate threshold over normalised titles. */
  similarityThreshold?: number;
}

/** The deterministic similarity of two titles, over normalised words: the
 * Jaccard overlap of their word sets, ignoring case and punctuation. One
 * constant (ENTRY_SIMILARITY_THRESHOLD) is the only knob. */
export const ENTRY_SIMILARITY_THRESHOLD = 0.6;

export function normalisedTitleWords(title: string): Set<string> {
  return new Set(
    title
      .toLowerCase()
      .replace(/[^a-z0-9\s]/g, " ")
      .split(/\s+/)
      .filter((w) => w.length > 0),
  );
}

export function titleSimilarity(a: string, b: string): number {
  const wa = normalisedTitleWords(a);
  const wb = normalisedTitleWords(b);
  if (wa.size === 0 || wb.size === 0) return 0;
  let inter = 0;
  for (const w of wa) if (wb.has(w)) inter += 1;
  return inter / (wa.size + wb.size - inter);
}

function raisedByTags(messages: readonly Message[]): string[] {
  const seen: string[] = [];
  for (const m of messages) {
    const who = m.raisedBy ?? (m.type === "tradeoff" ? "worker" : "reviewer");
    if (!seen.includes(who)) seen.push(who);
  }
  return seen;
}

function isLiveMessage(m: Message): boolean {
  return messageLiveness(m) === "live";
}

/** An entry's state against the messages it links: an explicit state stands;
 * otherwise an entry all of whose messages are settled leaves the live view —
 * resolved when one was resolved, otherwise dropped (a merged or superseded
 * message is just as settled as a dropped one; OD-2 / findings A-24, A-28). */
function resolveStateOf(entry: Entry, byId: Map<string, Message>): EntryStateName {
  if (entry.state !== "open") return entry.state;
  const own = entry.links.map((l) => byId.get(l.messageId)).filter((m): m is Message => !!m);
  if (own.length > 0 && own.every((m) => !isLiveMessage(m))) {
    return own.some((m) => m.state === "resolved") ? "resolved" : "dropped";
  }
  return "open";
}

/** The anchor a message-grounded new entry uses: the message's strongest
 * anchor, preferring a file anchor (the code keeps moving; the line range is
 * what a reviewer checks), then a decision, then a plan clause. */
function anchorForMessage(m: Message): EntryAnchor | undefined {
  const anchors = anchorsOfMessage(m);
  return anchors.find((a) => a.kind === "file") ?? anchors[0];
}

/**
 * Plan 05j: the entry events the RUNTIME must persist for the messages it has
 * that no entry links yet. This is the round-time pass — not a render-time
 * one — so `ENTRY_OPENED`/`MESSAGE_LINKED` are in the log, entry ids are
 * stable, and an owner's `s`/`m`/`A`/`D` names an entry `applyEntryEvent` can
 * find.
 *
 * A live message that shares an anchor with an OPEN entry is linked to it by
 * the same rule the curator's links are checked against; every other live
 * message opens its own entry. Merged, dropped and resolved messages never
 * open an entry (the lint forbids rendering one as a topic). Deterministic:
 * messages in raise order, ids assigned in that order.
 */
export function planEntryEvents(messages: readonly Message[], entries: readonly Entry[] = []): EntryEvent[] {
  const byId = new Map(messages.map((m) => [m.id, m]));
  const linked = new Set<string>();
  for (const entry of entries) for (const l of entry.links) linked.add(l.messageId);
  const events: EntryEvent[] = [];
  const working: Entry[] = entries.map((e) => ({ ...e, links: [...e.links] }));
  for (const m of messages) {
    if (linked.has(m.id)) continue;
    if (!isLiveMessage(m)) continue;
    const anchors = anchorsOfMessage(m);
    const host = working.find((e) => resolveStateOf(e, byId) === "open" && sharedAnchor([e.anchor], anchors) !== undefined);
    if (host) {
      const anchor = sharedAnchor([host.anchor], anchors)!;
      host.links = [...host.links, { messageId: m.id, anchor, reason: "shared anchor" }];
      linked.add(m.id);
      events.push({ type: "MESSAGE_LINKED", messageId: m.id, entryId: host.id, anchor, reason: "shared anchor", by: "runtime" });
      continue;
    }
    // No real anchor (no file:line evidence, no decision id, no plan clause):
    // the entry anchors to the message's own id, so the message still renders
    // (nothing hidden) and two such messages never merge; the lint reports the
    // entry as having no anchor (findings A-34, M-38).
    const anchor = anchorForMessage(m) ?? ({ kind: "message" as const, id: m.id });
    const id = nextEntryId(working);
    working.push({
      id,
      phaseId: m.phaseId,
      title: m.title,
      type: entryTypeOf([m]),
      state: "open",
      anchor,
      links: [{ messageId: m.id, anchor, reason: "opened" }],
    });
    linked.add(m.id);
    events.push({ type: "ENTRY_OPENED", phaseId: m.phaseId, title: m.title, entryType: entryTypeOf([m]), anchor, messageId: m.id, by: "runtime" });
  }
  return events;
}

/**
 * Builds the live entry views from the stored entries and the raw messages.
 *
 * Conservation: every message is accounted for exactly once — linked to an
 * entry, dropped with a reason, merged, or resolved — and any message not
 * already linked OPENS ITS OWN ENTRY (a link without a shared anchor is never
 * honoured). `accounting().unaccounted` is always 0 by construction.
 *
 * Latest state: an entry whose linked messages are all non-live is resolved
 * (05e's MESSAGE_RESOLVED) or dropped and leaves the live view; a live entry
 * whose anchor no longer resolves in the newest candidate is kept and marked
 * `stale anchor`, never hidden.
 */
export function projectEntries(opts: ProjectEntriesOptions): ProjectedEntries {
  const messages = [...(opts.messages ?? [])];
  const byId = new Map(messages.map((m) => [m.id, m]));
  const entries: Entry[] = (opts.entries ?? []).map((e) => ({ ...e, links: [...e.links] }));
  const refusedLinks: Array<{ messageId: string; entryId: string; reason: string }> = [];

  // 1. A link is honoured only while it shares an anchor with the entry it is
  //    on; a link whose entry vanished (or that never shared one) opens its own
  //    entry below.
  const linked = new Set<string>();
  for (const entry of entries) {
    entry.links = entry.links.filter((link) => {
      const message = byId.get(link.messageId);
      if (!message) return true; // keep a link to a message not in this slice
      // The owner's `m` is the ONE exception to the anchor rule: it merges two
      // near-duplicates that deliberately share no anchor, so the links it
      // moved must survive this filter (finding M-14).
      const ownerMerged = link.reason?.startsWith("merged from") ?? false;
      // A no-anchor entry holds exactly the message it names (finding A-34).
      const selfAnchor = entry.anchor.kind === "message" && entry.anchor.id === message.id;
      const shared = sharedAnchor([entry.anchor], anchorsOfMessage(message));
      if (!shared && !ownerMerged && !selfAnchor) {
        refusedLinks.push({ messageId: message.id, entryId: entry.id, reason: `message ${message.id} and entry ${entry.id} share no anchor` });
        return false;
      }
      if (linked.has(message.id)) {
        // A message belongs to exactly one entry; a second link is refused
        // (and logged), so the projection never shows a topic twice.
        refusedLinks.push({ messageId: message.id, entryId: entry.id, reason: `message ${message.id} is already linked to another entry` });
        return false;
      }
      if (shared) link.anchor = shared;
      linked.add(message.id);
      return true;
    });
  }

  // 2. `planEntryEvents` (the conductor's round-time pass) is what persists
  //    an entry for every message; the projection itself is a pure fold of
  //    the stored entries, so a log that never persisted one leaves its
  //    messages deliberately unaccounted (the lint then says so).

  // 3. Resolve state from the messages: an entry all of whose messages are
  //    settled (resolved/dropped/merged/superseded) is not live. An explicit
  //    ENTRY_STATE is honoured too.
  const resolveFor = (entry: Entry): EntryStateName => resolveStateOf(entry, byId);

  // 4. Merge hints between live entries with similar titles but no shared
  //    anchor. Deterministic: earlier entry id wins the hint pairing.
  const live = entries.filter((e) => resolveFor(e) === "open").sort((a, b) => a.id.localeCompare(b.id));
  const hints = new Map<string, string>();
  const threshold = opts.similarityThreshold ?? ENTRY_SIMILARITY_THRESHOLD;
  // The plan's near-duplicate signal is "an entry whose title AND summary are
  // close to another's", so the compared text is the title plus the first
  // linked message's summary (finding A-9 / record B-21).
  const textOf = (e: Entry): string => `${e.title} ${e.links.map((l) => byId.get(l.messageId)?.summary ?? "").find((s) => s.length > 0) ?? ""}`;
  for (let i = 0; i < live.length; i++) {
    for (let j = i + 1; j < live.length; j++) {
      const a = live[i];
      const b = live[j];
      if (sharedAnchor([a.anchor], [b.anchor])) continue; // shared anchor: already the same topic
      if (titleSimilarity(textOf(a), textOf(b)) < threshold) continue;
      if (!hints.has(a.id)) hints.set(a.id, b.id);
      if (!hints.has(b.id)) hints.set(b.id, a.id);
    }
  }

  const freshnessOf = (anchor: EntryAnchor): AnchorFreshness => {
    if (opts.anchorFreshness) return opts.anchorFreshness(anchor);
    if (opts.anchorResolves) return opts.anchorResolves(anchor) ? "fresh" : "stale";
    return "fresh";
  };
  const views: EntryView[] = entries
    // An entry with no linked message has no topic left (a reviewer's
    // `sameAs E-n` moved the message it was auto-opened for): it is not
    // rendered, so no two entries can share the anchor it would carry.
    .filter((entry) => entry.links.length > 0)
    .map((entry) => {
      const own = entry.links.map((l) => byId.get(l.messageId)).filter((m): m is Message => !!m);
      const state = resolveFor(entry);
      const liveEntry = state === "open";
      const anchor = entry.anchor;
      const freshness = liveEntry && anchor.kind === "file" ? freshnessOf(anchor) : "fresh";
      const staleAnchor = freshness === "stale";
      const unverifiedAnchor = freshness === "unverified";
      const stateSha = entry.stateSha ?? opts.newestCandidateSha;
      // Only a LIVE message can make an entry stale: an old settled message
      // that was linked as history is not a computation against an older
      // candidate (finding M-6).
      const staleState = liveEntry && !!stateSha && own.some((m) => isLiveMessage(m) && m.boundCandidateSha !== stateSha);
      return {
        entry,
        type: entryTypeOf(own.length > 0 ? own : [{ ...emptyMessage(entry) } as Message]),
        state,
        live: liveEntry,
        messages: own,
        anchor,
        staleAnchor,
        unverifiedAnchor,
        hint: hints.get(entry.id),
        raisedBy: raisedByTags(own),
        staleState,
      };
    })
    .sort((a, b) => a.entry.id.localeCompare(b.entry.id));

  // Buckets are disjoint: a live message is linked to exactly one entry; a
  // non-live message is counted once as dropped, merged (superseded counts as
  // merged: a link into another record) or resolved. The two never overlap,
  // so unaccounted is 0 exactly when every message is accounted for.
  let dropped = 0;
  let merged = 0;
  let resolved = 0;
  for (const m of messages) {
    const l = messageLiveness(m);
    if (l === "dropped") dropped += 1;
    else if (l === "merged") merged += 1;
    else if (l === "resolved") resolved += 1;
    else if (l === "superseded") merged += 1;
  }
  // `linked` is the set of messages an entry actually holds, so a live
  // message no entry links is unaccounted (finding M-15) — the lint's
  // accounting rule can then genuinely fail.
  const linkedLive = messages.filter((m) => isLiveMessage(m) && linked.has(m.id)).length;
  const unaccounted = messages.length - (linkedLive + dropped + merged + resolved);
  const unexposed = messages.filter((m) => m.type === "tradeoff" && m.raisedBy !== undefined && m.raisedBy !== "worker").length;
  const accounting: Accounting = {
    raw: messages.length,
    entries: views.filter((v) => v.live).length,
    linked: linkedLive,
    dropped,
    merged,
    resolved,
    unaccounted,
    unexposed,
  };
  return { views, accounting, refusedLinks };
}

function emptyMessage(entry: Entry): Message {
  return {
    id: entry.id,
    phaseId: entry.phaseId,
    type: entry.type,
    title: entry.title,
    summary: "",
    context: "",
    evidence: [],
    state: "raw",
    messageVersion: 1,
    boundCandidateSha: "",
    boundContractVersion: { snapshot: 0, sectionSha256: "" },
    contentHash: "",
  };
}

/** The accounting line, always reconciling to 0 unaccounted. */
export function accountingLine(a: Accounting): string {
  return `${a.raw} raw → ${a.entries} entries · ${a.linked} linked · ${a.dropped} dropped · ${a.unaccounted} unaccounted · unexposed ${a.unexposed}`;
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

export interface EntryReviewOptions {
  /** The phase view: one phase. */
  messages?: readonly Message[];
  entries?: readonly Entry[];
  readableId?: string;
  dirId?: string;
  phaseId?: string;
  /** Decision briefs: one per open owner item, rendered as a `* Needs you'
   * section above every entry. Each brief's question is its heading and its
   * evidence is folded under TAB. */
  briefs?: readonly DecisionBrief[];
  /** The open owner requests the briefs answer, so the rendered brief
   * carries the resolve binding. */
  ownerRequests?: readonly OwnerRequest[];
  /** The ids of the briefs that are still showable: open owner requests plus
   * live flagged reserved decisions. A recorded brief whose item is settled
   * is never shown under `Needs you' again. */
  briefableIds?: ReadonlySet<string>;
  /** Live decisions, so a reserved decision's brief is showable while the
   * decision is still on this candidate. */
  decisions?: readonly Decision[];
  /** The binding a resolve command from a brief needs. */
  resolveBinding?: { runId: string; phaseId: string; candidateSha: string; recordVersion: number; contractVersion: { snapshot: number; sectionSha256: string } };
  /** The program view: entries of every phase, tagged by phase. */
  program?: {
    id: string;
    phases: Array<{
      phaseId: string;
      readableId?: string;
      /** Each phase's OWN newest candidate: a state must be computed against
       * the candidate that phase is at, not the program's last one (finding
       * A-30). */
      candidate?: { sha: string };
      runId?: string;
      contract?: { contractVersion?: { snapshot: number; sectionSha256: string } };
      messages?: readonly Message[];
      entries?: readonly Entry[];
      decisions?: readonly Decision[];
      briefs?: readonly DecisionBrief[];
      ownerRequests?: readonly OwnerRequest[];
    }>;
  };
  newestCandidateSha?: string;
  anchorResolves?: (anchor: EntryAnchor) => boolean;
  anchorFreshness?: (anchor: EntryAnchor) => AnchorFreshness;
  /** A lint violation, if any: it becomes the view's first line. */
  lintError?: string;
}

const SECTION_ORDER: Array<{ kind: EntryType; label: string }> = [
  { kind: "blocker", label: "Blockers" },
  { kind: "finding", label: "Findings" },
  { kind: "tradeoff", label: "Trade-offs" },
];

function oneLine(text: string | undefined): string {
  return (text ?? "").replace(/\s+/g, " ").trim();
}

/** The tags after an entry's title: who raised it, its linked count, its
 * anchor, and (program view) its phase. */
function entryTags(view: EntryView, phaseTag?: string): string {
  const tags: string[] = [];
  if (phaseTag) tags.push(phaseTag);
  if (view.raisedBy.length > 0) tags.push(view.raisedBy.join("·"));
  const extra = Math.max(0, view.messages.length - 1);
  if (extra > 0) tags.push(`+${extra} linked`);
  tags.push(formatAnchor(view.anchor));
  if (view.staleAnchor) tags.push("stale anchor");
  if (view.unverifiedAnchor) tags.push("anchor unverified");
  if (view.staleState) tags.push("stale state");
  if (view.hint) tags.push(`≈ ${view.hint}`);
  return `[${tags.join(" · ")}]`;
}

function entryBody(view: EntryView): string[] {
  const out: string[] = [];
  const summary = oneLine(view.messages.find((m) => m.summary)?.summary) || oneLine(view.entry.title);
  out.push(`  ${summary}`);
  if (view.messages.length > 0) {
    out.push("");
    out.push("  * Linked messages");
    for (const m of [...view.messages].sort((a, b) => a.id.localeCompare(b.id))) {
      out.push(`    - ${m.id} ${oneLine(m.title)} — ${oneLine(m.summary)}`);
    }
  }
  if (view.entry.stateReason) out.push("", `  State: ${view.entry.state} — ${oneLine(view.entry.stateReason)}`);
  return out;
}

/** One section: one heading (`* Blockers`) and one heading per live entry. */
function renderSection(views: EntryView[], kind: EntryType, label: string, phaseTagOf: (v: EntryView) => string | undefined): string[] {
  const own = views.filter((v) => v.type === kind && v.live).sort((a, b) => a.entry.id.localeCompare(b.entry.id));
  const out = [`* ${label}`];
  if (own.length === 0) {
    out.push("(none)", "");
    return out;
  }
  for (const v of own) {
    out.push(`** ${v.entry.id} ${oneLine(v.entry.title)}  ${entryTags(v, phaseTagOf(v))}`);
    out.push("   :PROPERTIES:");
    out.push(`   :ID: ${v.entry.id}`);
    out.push(`   :TYPE: ${v.type}`);
    out.push(`   :STATE: ${v.state}`);
    out.push(`   :ANCHOR: ${formatAnchor(v.anchor)}`);
    out.push(`   :RAISED_BY: ${v.raisedBy.join(",") || "unknown"}`);
    out.push(`   :LINKED: ${v.messages.map((m) => m.id).join(",")}`);
    if (v.hint) out.push(`   :HINT: ${v.hint}`);
    out.push("   :END:");
    out.push(...entryBody(v));
    out.push("");
  }
  return out;
}

/** The header every entry view carries: the candidate it reflects, so the
 * owner always knows which code the view is against. */
function reviewHeader(opts: EntryReviewOptions): string[] {
  const lines: string[] = [];
  if (opts.lintError) lines.push(`review lint: ${oneLine(opts.lintError)}`);
  lines.push(`#+TITLE: tradeoffs-trace review — ${entryReviewLabel(opts)}`);
  lines.push("#+CONTRACT_VERSION: v1");
  if (opts.newestCandidateSha) lines.push(`#+CANDIDATE: ${opts.newestCandidateSha}`);
  return lines;
}

function entryReviewLabel(opts: EntryReviewOptions): string {
  if (opts.program) return `program ${opts.program.id}`;
  const readable = opts.readableId;
  const dir = opts.dirId;
  if (readable && dir) return `${readable} · ${dir}`;
  return readable ?? dir ?? opts.phaseId ?? "";
}

/** The ids a `Needs you' brief may still be shown for: every open owner
 * request, plus every live flagged reserved decision on the current
 * candidate (a reserved decision never becomes an owner request, so it would
 * otherwise reach the owner as an engineer note). */
export function briefableIdsFor(
  requests: readonly OwnerRequest[] | undefined,
  decisions: readonly Decision[] | undefined,
  candidateSha: string | undefined,
): Set<string> {
  const ids = new Set((requests ?? []).filter((r) => r.status === "open").map((r) => r.id));
  for (const d of decisions ?? []) {
    if (d.class !== "reserved" || d.amendment) continue;
    if (d.supersededBy || d.supersededByCorrection) continue;
    if (candidateSha && d.boundCandidateSha !== candidateSha) continue;
    ids.add(d.id);
  }
  return ids;
}

/** `views/review.org` for one phase: three sections (Blockers, Findings,
 * Trade-offs), one heading per live entry, and the accounting footer. */
export function renderEntryReview(opts: EntryReviewOptions): string {
  const projected = projectEntries({ messages: opts.messages, entries: opts.entries, newestCandidateSha: opts.newestCandidateSha, anchorResolves: opts.anchorResolves, anchorFreshness: opts.anchorFreshness });
  const lines = reviewHeader(opts);
  lines.push("");
  // Decision briefs first: the owner reads the question and the choice before
  // any entry's evidence. Only items that are still open are shown; a recorded
  // brief whose request is settled is not resurrected (finding M-3).
  const showable = opts.briefableIds ?? briefableIdsFor(opts.ownerRequests, opts.decisions, opts.newestCandidateSha);
  const briefs = (opts.briefs ?? []).filter((b) => showable.has(b.requestId));
  lines.push(
    ...renderBriefsSection(briefs, {
      requestFor: (id) => (opts.ownerRequests ?? []).find((r) => r.id === id),
      binding: opts.resolveBinding,
      recordVersionFor: (id) =>
        (opts.ownerRequests ?? []).find((r) => r.id === id)?.version ?? (opts.decisions ?? []).find((d) => d.id === id)?.version,
    }),
  );
  for (const s of SECTION_ORDER) lines.push(...renderSection(projected.views, s.kind, s.label, () => undefined));
  lines.push(accountingLine(projected.accounting));
  return `${lines.join("\n")}\n`;
}

/** `programs/<id>/views/review.org`: every phase's entries, grouped by the
 * same three sections. Cross-phase links are only through shared anchors, so
 * two entries of different phases that share an anchor are shown once with
 * both phase tags; entries with similar titles but no shared anchor stay
 * separate and carry the `≈` hint. */
export function renderProgramEntryReview(opts: EntryReviewOptions): string {
  const program = opts.program;
  const lines = reviewHeader({ ...opts, messages: [] });
  lines.push("");
  if (!program) {
    lines.push("* Blockers", "(none)", "", "* Findings", "(none)", "", "* Trade-offs", "(none)", "", accountingLine(emptyAccounting()));
    return `${lines.join("\n")}\n`;
  }
  // Project each phase, then fold entries across phases by shared anchor.
  const perPhase = program.phases.map((p) => ({
    phaseId: p.phaseId,
    readableId: p.readableId,
    projected: projectEntries({ messages: p.messages, entries: p.entries, newestCandidateSha: p.candidate?.sha ?? opts.newestCandidateSha, anchorResolves: opts.anchorResolves, anchorFreshness: opts.anchorFreshness }),
  }));
  // Decision briefs: every phase's open owner items, one section at the top.
  // The brief's own requestId stays the heading id (never qualified), so a
  // resolve from the program review names the real request (findings B-9,
  // M-2); the phase tag is shown beside the heading and in a :PHASE:
  // property. Only items still open are shown (finding M-3).
  const allBriefs: Array<{
    brief: DecisionBrief;
    tag: string;
    requests: readonly OwnerRequest[];
    recordVersion?: number;
    binding?: { runId: string; phaseId: string; candidateSha: string; recordVersion: number; contractVersion: { snapshot: number; sectionSha256: string } };
  }> = [];
  for (const p of program.phases) {
    const tag = p.readableId ?? p.phaseId;
    const showable = briefableIdsFor(p.ownerRequests, p.decisions, p.candidate?.sha ?? opts.newestCandidateSha);
    const requests = p.ownerRequests ?? [];
    const binding =
      p.contract?.contractVersion && p.candidate?.sha && p.runId && p.phaseId
        ? { runId: p.runId, phaseId: p.phaseId, candidateSha: p.candidate.sha, recordVersion: 1, contractVersion: p.contract.contractVersion }
        : undefined;
    for (const brief of p.briefs ?? []) {
      if (!showable.has(brief.requestId)) continue;
      const recordVersion = requests.find((r) => r.id === brief.requestId)?.version ?? (p.decisions ?? []).find((d) => d.id === brief.requestId)?.version;
      allBriefs.push({ brief, tag, requests, recordVersion, binding });
    }
  }
  if (allBriefs.length > 0) {
    lines.push(`* Needs you (${allBriefs.length})`);
    for (const b of allBriefs) {
      const request = (b.requests ?? []).find((r) => r.id === b.brief.requestId);
      const binding = b.binding && b.recordVersion !== undefined ? { ...b.binding, recordVersion: b.recordVersion } : (b.binding ?? opts.resolveBinding);
      lines.push(renderBriefOrg(b.brief, { request, binding, tag: b.tag, recordVersion: b.recordVersion }), "");
    }
  }
  const all: Array<{ view: EntryView; phaseTag: string; phaseIndex: number }> = [];
  for (const ph of perPhase) {
    const tag = ph.readableId ?? ph.phaseId;
    for (const raw of ph.projected.views) {
      if (!raw.live) continue;
      // Message ids are numbered per phase (T-1 repeats), so qualify every
      // message with its phase tag: a colliding id is then never mistaken for
      // an already-shown one and silently dropped (finding A-26).
      const view: EntryView = { ...raw, messages: raw.messages.map((m) => ({ ...m, id: `${tag}:${m.id}` })) };
      const existing = all.find((x) => sharedAnchor([x.view.anchor], [view.anchor]));
      if (existing) {
        // Same anchor across phases: one topic, both phases. Merge the
        // messages so nothing is hidden; ids are already phase-qualified.
        const ids = new Set(existing.view.messages.map((m) => m.id));
        existing.view.messages = [...existing.view.messages, ...view.messages.filter((m) => !ids.has(m.id))];
        existing.view.raisedBy = [...new Set([...existing.view.raisedBy, ...view.raisedBy])];
        existing.phaseTag = [...new Set([...existing.phaseTag.split("·"), tag])].join("·");
        // The folded topic's TYPE is the highest of all its messages: a
        // blocker raised in a later phase must not stay a trade-off (findings
        // M-32, A-35).
        existing.view.type = entryTypeOf(existing.view.messages);
        continue;
      }
      all.push({ view, phaseTag: tag, phaseIndex: perPhase.indexOf(ph) });
    }
  }
  for (const s of SECTION_ORDER) {
    lines.push(`* ${s.label}`);
    const own = all.filter((x) => x.view.type === s.kind).sort((a, b) => a.view.entry.id.localeCompare(b.view.entry.id));
    if (own.length === 0) {
      lines.push("(none)", "");
      continue;
    }
    for (const x of own) {
      // Entry ids are phase-local (every phase numbers E-1 from scratch), so
      // the heading qualifies the id with its phase tags: two `** E-1`
      // headings can never be confused (finding M-32).
      lines.push(`** ${x.phaseTag}:${x.view.entry.id} ${oneLine(x.view.entry.title)}  ${entryTags(x.view, x.phaseTag)}`);
      lines.push("   :PROPERTIES:");
      lines.push(`   :ID: ${x.view.entry.id}`);
      lines.push(`   :TYPE: ${x.view.type}`);
      lines.push(`   :STATE: ${x.view.state}`);
      lines.push(`   :ANCHOR: ${formatAnchor(x.view.anchor)}`);
      lines.push(`   :PHASE: ${x.phaseTag}`);
      lines.push(`   :RAISED_BY: ${x.view.raisedBy.join(",") || "unknown"}`);
      lines.push(`   :LINKED: ${x.view.messages.map((m) => m.id).join(",")}`);
      if (x.view.hint) lines.push(`   :HINT: ${x.view.hint}`);
      lines.push("   :END:");
      lines.push(...entryBody(x.view));
      lines.push("");
    }
  }
  const a = combineAccounting(perPhase.map((p) => p.projected.accounting));
  lines.push(accountingLine(a));
  return `${lines.join("\n")}\n`;
}

function emptyAccounting(): Accounting {
  return { raw: 0, entries: 0, linked: 0, dropped: 0, merged: 0, resolved: 0, unaccounted: 0, unexposed: 0 };
}

function combineAccounting(list: readonly Accounting[]): Accounting {
  const out = emptyAccounting();
  for (const a of list) {
    out.raw += a.raw;
    out.entries += a.entries;
    out.linked += a.linked;
    out.dropped += a.dropped;
    out.merged += a.merged;
    out.resolved += a.resolved;
    out.unaccounted += a.unaccounted;
    out.unexposed += a.unexposed;
  }
  return out;
}

/** One entry's own file `views/entries/<id>.org`, the RET target: the full
 * history of every linked message. */
export function renderEntryFile(view: EntryView): string {
  const lines: string[] = [
    `#+TITLE: ${view.entry.id} — ${oneLine(view.entry.title)}`,
    `#+TYPE: ${view.type}`,
    "",
    `* ${view.entry.id} ${oneLine(view.entry.title)}  ${entryTags(view)}`,
    "  :PROPERTIES:",
    `  :ID: ${view.entry.id}`,
    `  :TYPE: ${view.type}`,
    `  :STATE: ${view.state}`,
    `  :ANCHOR: ${formatAnchor(view.anchor)}`,
    "  :END:",
  ];
  lines.push(...entryBody(view));
  lines.push("", "* History");
  for (const m of view.messages) {
    lines.push(`  - ${m.id} ${oneLine(m.title)} (v${m.messageVersion}, ${m.boundCandidateSha}, ${m.state})`);
  }
  return `${lines.join("\n")}\n`;
}
