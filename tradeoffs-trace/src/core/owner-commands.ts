// Shared, single-source-of-truth logic for the three owner commands that
// close a record-level open item — OWNER_REQUEST_RESOLVED,
// FINDING_ACCEPTED_BY_OWNER and OVERRIDE_CAST — used both by reduce.ts's
// ordinary (phase-unchanged) handling and by transitions.ts's AWAITING_OWNER
// rows (design §6.1: "AWAITING_OWNER ... leaves only through an owner
// command"), so the binding check and the mutation are never duplicated.

import { checkBinding, checkTupleBinding } from "./binding.ts";
import { applyMessageEvent } from "./messages.ts";
import { isBudgetGateRequest, isRepairForcingOption } from "./owner-requests.ts";
import { findOwnerRequest, ownerRequestIsStale } from "./triage.ts";
import type { BindingTuple, ContractVersion, Decision, Event, Message, PhaseState, RuledDecision } from "./types.ts";

/** Plan 06k1 (A2): the words an owner uses to rule a decision, as opposed to
 * merely mentioning it. A correction or directive that names a decision id
 * AND one of these words rules it; the ruling carries forward by id+choice. */
const RULING_WORDS = /\b(ruled|rule|settled|settle|decided|decide|final|no ballots|no vote|without a vote)\b/i;

/** Plan 06k1 (A2, finding A-4): a negation attached to the ruling word, so
 * "D-1 is not ruled" rules nothing. Kept narrow (not a bare "no") so a
 * phrase like "D-1 has no open findings and is ruled" still rules. */
const RULING_NEGATION =
  /\b(not|never|isn't|isnt|aren't|arent)\s+(?:owner[- ])?(ruled|rule|settled|settle|decided|decide)\b|\bunruled\b|\bundecided\b/i;

/** The clauses a text splits into, so a ruling word attaches only to the ids
 * in its own clause: `D-1 is ruled; D-2 remains undecided` rules D-1 alone. */
function rulingClauses(text: string): string[] {
  return text
    .split(/[;\n]+|(?<=[.!?])\s+/)
    .map((s) => s.trim())
    .filter((s) => s.length > 0);
}

/** Plan 06k1 (A2, finding A-4): the decision ids an owner correction or
 * directive text names as ruled, PER ID. A ruling attaches to the id(s) in
 * its own clause; a negated ruling rules nothing. Pure. */
export function ruledDecisionIds(text: string, decisions: readonly Decision[]): string[] {
  if (!text) return [];
  const clauses = rulingClauses(text);
  const out: string[] = [];
  for (const d of decisions) {
    if (d.id.length === 0) continue;
    const escaped = d.id.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    const mentions = new RegExp(`(^|[^A-Za-z0-9_-])${escaped}([^A-Za-z0-9_-]|$)`);
    for (const clause of clauses) {
      if (!mentions.test(clause)) continue;
      if (!RULING_WORDS.test(clause)) continue;
      if (RULING_NEGATION.test(clause)) continue;
      if (!out.includes(d.id)) out.push(d.id);
      break;
    }
  }
  return out;
}

/** Plan 06k2 (A8): the decision ids a text NEGATES a ruling on — `D-1 is not
 * ruled`, `D-1 is unruled`, `D-1 remains undecided`. A later owner word that
 * negates a ruling clears it, restoring the decision's ballots (finding
 * D-B-79: a ruling must not survive a negation). Pure. */
export function negatedRulingIds(text: string, decisions: readonly Decision[]): string[] {
  if (!text) return [];
  const clauses = rulingClauses(text);
  const out: string[] = [];
  for (const d of decisions) {
    if (d.id.length === 0) continue;
    const escaped = d.id.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    const mentions = new RegExp(`(^|[^A-Za-z0-9_-])${escaped}([^A-Za-z0-9_-]|$)`);
    for (const clause of clauses) {
      if (!mentions.test(clause)) continue;
      if (!RULING_NEGATION.test(clause)) continue;
      if (!out.includes(d.id)) out.push(d.id);
      break;
    }
  }
  return out;
}

/** Plan 06k2 (A8): clear the rulings a text negates (and the rulings carried
 * by a withdrawn directive, `by` the directive's own id). Returns the
 * surviving list, or undefined when none remain. */
export function clearRulings(
  existing: readonly RuledDecision[] | undefined,
  text: string,
  decisions: readonly Decision[],
  by?: string,
): RuledDecision[] | undefined {
  if (!existing || existing.length === 0) return undefined;
  const negated = new Set(negatedRulingIds(text, decisions));
  const out = existing.filter((r) => !negated.has(r.id) && (by === undefined || r.by !== by));
  return out.length > 0 ? out : undefined;
}

/** Plan 06k2 (A6, finding A-3/D-B-17): true when an owner text ASKS for an
 * unchanged resubmission. A negated clause (`do not resubmit unchanged`,
 * `don't resubmit unchanged`, `never submit it unchanged`) asks for nothing.
 * Pure. */
export function asksForUnchangedResubmission(text: string): boolean {
  if (!text) return false;
  const clauses = text
    .split(/[;\n]+|(?<=[.!?])\s+/)
    .map((s) => s.trim())
    .filter((s) => s.length > 0);
  for (const clause of clauses) {
    const asks =
      /\b(?:re-?submit|submit|resend|send)\b[^.]{0,40}\bunchanged\b/i.test(clause) ||
      /\bunchanged\s+(?:re)?submission\b/i.test(clause) ||
      /\bresubmit\s+(?:it\s+)?as[- ]is\b/i.test(clause);
    if (!asks) continue;
    // A negation in the same clause asks for the opposite, not permission.
    if (/\b(?:not|never|don'?t|doesn'?t|isn'?t|no|mustn'?t|shouldn'?t)\b/i.test(clause)) continue;
    return true;
  }
  return false;
}

/** Plan 06k2 (A6, finding A-3/D-B-17): the keys of the owner steers that ask
 * for an unchanged resubmission. Only an in-force directive counts — a
 * withdrawn one no longer asks for anything. Keys are `note:<i>` (the
 * ownerNotes index) or `directive:<id>`, so a caller can consume one steer's
 * permission for exactly the next submission. Pure. */
export function unchangedResubmissionRequesters(
  ownerNotes: readonly string[],
  ownerDirectives: readonly { id: string; text: string; status: string }[],
): string[] {
  const out: string[] = [];
  // Plan 06k3 (A4, findings D-M-37/F-15): the SAME ask written as a note and
  // an in-force directive is ONE permission, not two. Without this the first
  // submission consumes the note and the directive lifts a SECOND unchanged
  // submission — including after a restart, where the directive key is still
  // unconsumed.
  const seen = new Set<string>();
  const push = (text: string, key: string): void => {
    if (!asksForUnchangedResubmission(text)) return;
    const t = text.trim();
    if (seen.has(t)) return;
    seen.add(t);
    out.push(key);
  };
  ownerNotes.forEach((text, i) => push(text, `note:${i}`));
  for (const d of ownerDirectives) {
    if (d.status === "in-force") push(d.text, `directive:${d.id}`);
  }
  return out;
}

/** Plan 06k1 (A2): record the rulings a correction/directive text carries,
 * keyed by id and pinned to the decision's current choice. A second ruling
 * on the same id replaces the first, so a re-ruled choice is what counts. */
export function recordRulings(
  existing: readonly RuledDecision[] | undefined,
  text: string,
  decisions: readonly Decision[],
  by: string,
): RuledDecision[] | undefined {
  const ids = ruledDecisionIds(text, decisions);
  if (ids.length === 0) return existing ? [...existing] : undefined;
  const out = [...(existing ?? [])];
  for (const id of ids) {
    const decision = decisions.find((d) => d.id === id);
    if (!decision) continue;
    const at = out.findIndex((r) => r.id === id);
    const ruling: RuledDecision = { id, choice: decision.choice, by };
    if (at >= 0) out[at] = ruling;
    else out.push(ruling);
  }
  return out;
}

export interface CommandCheck {
  ok: boolean;
  reason?: string;
}

function bindingTuple(phase: PhaseState, recordId: string, candidateSha: string, contractVersion: ContractVersion, recordVersion: number): BindingTuple {
  return { runId: phase.runId, phaseId: phase.phaseId, candidateSha, contractVersion, recordId, recordVersion };
}

/** Plan 04b: resolve a blocker message the owner just decided about. Only a
 * published (or refused) message can be resolved — a dropped/merged one was
 * never a standing claim, and resolving it is meaningless. */
function resolveOwnerBlockerMessage(messages: readonly Message[], messageId: string, reason: string): Message[] {
  const message = messages.find((m) => m.id === messageId);
  if (!message || (message.state !== "published" && message.state !== "refused")) return [...messages];
  const result = applyMessageEvent([...messages], {
    type: "MESSAGE_RESOLVED",
    messageId,
    by: "owner",
    reason,
    boundCandidateSha: message.boundCandidateSha,
    boundContractVersion: message.boundContractVersion,
    boundRecordVersion: message.messageVersion,
  });
  return result.ok ? result.messages : [...messages];
}

/** Resolve any OPEN owner request linked to `recordId` (a decision,
 * finding or correction) as a side effect of the owner acting on that
 * record directly (override, accept-finding, or revise) — the record-level
 * action addresses the request about it. */
export function autoResolveLinkedRequest(
  phase: PhaseState,
  recordId: string,
  option: string,
  candidateSha: string,
  contractVersion: ContractVersion,
): PhaseState {
  const ownerRequests = phase.ownerRequests.map((r) =>
    r.status === "open" &&
    (r.linkedDecisionId === recordId || r.linkedFindingId === recordId || r.linkedCorrectionId === recordId)
      ? { ...r, status: "resolved" as const, resolution: { option }, resolvedBinding: { candidateSha, contractVersion } }
      : r,
  );
  return { ...phase, ownerRequests };
}

// --- OWNER_REQUEST_RESOLVED -------------------------------------------

type EvOwnerRequestResolved = Extract<Event, { type: "OWNER_REQUEST_RESOLVED" }>;

export function checkOwnerRequestResolved(phase: PhaseState, event: EvOwnerRequestResolved): CommandCheck {
  const request = findOwnerRequest(phase, event.requestId);
  if (!request) return { ok: false, reason: `unknown owner request ${event.requestId}` };
  if (request.status !== "open") {
    return { ok: false, reason: `owner request ${event.requestId} is already ${request.status}, not open` };
  }
  // Binding (design §7.1) first — is this even the version of the request
  // the sender saw? — before what they chose is judged at all.
  const tuple = bindingTuple(phase, event.requestId, event.boundCandidateSha, event.boundContractVersion, event.boundRecordVersion);
  const bindingCheck = checkTupleBinding(tuple, phase);
  if (!bindingCheck.ok) return bindingCheck;
  // design round-3 review item 1: the option is named by its stable id, and
  // an id the request does not offer is rejected — never matched by label.
  if (!request.options.some((o) => o.id === event.option)) {
    return {
      ok: false,
      reason: `'${event.option}' is not an option owner request ${event.requestId} offers (offered: ${request.options.map((o) => o.id).join(", ")})`,
    };
  }
  if (request.origin === "open_finding" && event.option === "accept_risk" && (!event.note || event.note.trim().length === 0)) {
    return { ok: false, reason: `accepting the risk for finding ${request.linkedFindingId} requires a non-empty scope note` };
  }
  return { ok: true };
}

export function applyOwnerRequestResolved(phase: PhaseState, event: EvOwnerRequestResolved): PhaseState {
  const request = findOwnerRequest(phase, event.requestId)!;
  const ownerRequests = phase.ownerRequests.map((r) =>
    r === request
      ? {
          ...r,
          status: "resolved" as const,
          resolution: { option: event.option, note: event.note },
          resolvedBinding: { candidateSha: event.boundCandidateSha, contractVersion: event.boundContractVersion },
        }
      : r,
  );
  let next: PhaseState = { ...phase, ownerRequests };

  // Every option has a defined effect (round-3 review item 1) — none is a
  // no-op. "budget.granted by owner request <id>": the allowance is granted
  // exactly like a correction's, independent of the exhausted budget.
  if (isBudgetGateRequest(request) && event.option === "grant") {
    // Plan 06g (A4, owner-verified): the budget gate grants exactly ONE more
    // round — the owner lifts the park for a single further candidate, never a
    // fresh three-round allowance. One round is one candidate reviewed.
    next = { ...next, repairRoundsGranted: next.repairRoundsGranted + 1 };
  }
  if (isRepairForcingOption(request.origin, event.option)) {
    next = { ...next, repairRoundsGranted: next.repairRoundsGranted + 3 };
  }
  // Plan 06g (A6): the owner accepts the candidate with the open items
  // carried. `accept()` honours the flag, and the carried ids are recorded
  // here (the views list them, and the next phase's plan is built from them).
  if (isBudgetGateRequest(request) && event.option === "accept_carried") {
    const carriedItems = next.findings.filter((f) => f.status === "open").map((f) => f.id);
    next = {
      ...next,
      acceptedWithCarried: true,
      // Plan 06g (ODP-2): the candidate the carry was given for. `accept()`
      // accepts only when this equals the candidate under acceptance.
      carriedCandidateSha: event.boundCandidateSha,
      carriedItems,
      // A5: every carried item is listed WITH its target. This request has no
      // explicit `--to` (the owner's one decision carries the leftovers into
      // the next phase), so the target is the next phase's plan.
      carriedTo: Object.fromEntries(carriedItems.map((id) => [id, "next"])),
    };
  }
  if (request.origin === "open_finding" && event.option === "accept_risk" && request.linkedFindingId) {
    // design §4.2: equivalent to accept-finding, with the required scope note.
    next = {
      ...next,
      findings: next.findings.map((f) =>
        f.id === request.linkedFindingId ? { ...f, status: "accepted" as const, acceptedScope: event.note } : f,
      ),
    };
  }
  if (request.origin === "unaddressed_correction" && event.option === "withdraw" && request.linkedCorrectionId) {
    // design §7.5: withdrawn by the owner only — it no longer gates accept().
    next = {
      ...next,
      corrections: next.corrections.map((c) =>
        c.id === request.linkedCorrectionId ? { ...c, status: "withdrawn" as const } : c,
      ),
    };
  }
  if (request.origin === "blocker_panel") {
    // Plan 04b: the owner's choice resolves the blocker — both things it was
    // raised as. The blocking finding is accepted under the option the owner
    // chose (so it can never keep blocking acceptance), and the raw `blocker`
    // message is resolved by the owner (never left dangling in the ledger).
    const optionLabel = request.options.find((o) => o.id === event.option)?.label ?? event.option;
    if (request.linkedFindingId) {
      next = {
        ...next,
        findings: next.findings.map((f) =>
          f.id === request.linkedFindingId && f.status === "open"
            ? { ...f, status: "accepted" as const, acceptedScope: `${optionLabel} (owner's choice on blocker ${request.linkedMessageId ?? f.id})` }
            : f,
        ),
      };
    }
    if (request.linkedMessageId) {
      next = { ...next, messages: resolveOwnerBlockerMessage(next.messages ?? [], request.linkedMessageId, optionLabel) };
    }
  }
  // "accept_as_implemented" (failed_vote) and "approve" (reserved_decision)
  // settle the decision on THIS candidate through the resolved request's own
  // `resolution.option` (predicate.ts). Plan 06k1 (A2, finding A-5): the
  // settlement must CARRY FORWARD to later candidates by id and unchanged
  // choice, so it is recorded as an owner ruling too — otherwise a later
  // candidate whose D-1 has no ballots would no longer be settled.
  const rulingTarget =
    request.linkedDecisionId !== undefined &&
    ((request.origin === "failed_vote" && event.option === "accept_as_implemented") ||
      (request.origin === "reserved_decision" && event.option === "approve"))
      ? request.linkedDecisionId
      : undefined;
  if (rulingTarget !== undefined) {
    const decision = next.decisions.find((d) => d.id === rulingTarget);
    if (decision) {
      const kept = (next.ruledDecisions ?? []).filter((r) => r.id !== rulingTarget);
      next = { ...next, ruledDecisions: [...kept, { id: rulingTarget, choice: decision.choice, by: "owner" }] };
    }
  }
  return next;
}

// --- FINDING_ACCEPTED_BY_OWNER -----------------------------------------

type EvFindingAcceptedByOwner = Extract<Event, { type: "FINDING_ACCEPTED_BY_OWNER" }>;

export function checkFindingAcceptedByOwner(phase: PhaseState, event: EvFindingAcceptedByOwner): CommandCheck {
  if (event.by !== "owner") return { ok: false, reason: `only the owner may accept a finding, not '${String(event.by)}'` };
  const finding = phase.findings.find((f) => f.id === event.findingId);
  if (!finding) return { ok: false, reason: `unknown finding ${event.findingId}` };
  // design §4.2: for a `contract` finding, accepting "means amending the
  // contract" — the only disposition is AMEND (§7.3), never a direct
  // acceptance. Reject it here so both reduce()'s record-event path and
  // transitions.ts's AWAITING_OWNER rows share the same rule.
  if (finding.kind === "contract") {
    return {
      ok: false,
      reason: `finding ${event.findingId} is a contract finding; accepting it means amending the contract (AMEND), not FINDING_ACCEPTED_BY_OWNER`,
    };
  }
  // design §4.2/§10.4: acceptance must record the scope it was accepted
  // under — a blank or whitespace-only scope is no scope at all.
  if (!event.scope || event.scope.trim().length === 0) {
    return { ok: false, reason: `accepting finding ${event.findingId} requires a non-empty scope note` };
  }
  const tuple = bindingTuple(phase, event.findingId, event.boundCandidateSha, event.boundContractVersion, event.boundRecordVersion);
  return checkTupleBinding(tuple, phase);
}

export function applyFindingAcceptedByOwner(phase: PhaseState, event: EvFindingAcceptedByOwner): PhaseState {
  const findings = phase.findings.map((f) =>
    f.id === event.findingId ? { ...f, status: "accepted" as const, acceptedScope: event.scope } : f,
  );
  const withFinding = { ...phase, findings };
  return autoResolveLinkedRequest(withFinding, event.findingId, "accepted", event.boundCandidateSha, event.boundContractVersion);
}

// --- ITEM_CARRIED --------------------------------------------------------
//
// Plan 06g (A5): the owner's `tt carry <run> <id> --to <phase-id>`. It takes
// a finding or a message id. Through the inbox it (1) marks the item carried,
// with the target phase, (2) resolves the open owner request about that item
// (and the plain budget gate, whose "accept with carried items" decision the
// carry is), and (3) once no blocking item remains uncarried, accepts the
// candidate as it stands (`acceptedWithCarried`, honoured by `accept()`).
// This function is the ONLY place that decides what a carry does.

type EvItemCarried = Extract<Event, { type: "ITEM_CARRIED" }>;

/** The finding or message `recordId` names, with the version its binding
 * must match. A finding is looked up first unless `recordKind` says
 * otherwise; a message needs its own lookup because `currentVersionsFor`
 * (binding.ts) knows decisions, findings and owner requests, not messages. */
function carriedRecord(
  phase: PhaseState,
  recordId: string,
  recordKind: EvItemCarried["recordKind"],
): { kind: "finding" | "message"; candidateSha: string; contractVersion: ContractVersion; recordVersion: number; label: string } | undefined {
  if (recordKind !== "message") {
    const finding = phase.findings.find((f) => f.id === recordId);
    if (finding) {
      return {
        kind: "finding",
        candidateSha: phase.candidate?.sha ?? "",
        contractVersion: phase.contract.contractVersion,
        recordVersion: finding.version,
        label: `finding ${finding.id}`,
      };
    }
  }
  if (recordKind !== "finding") {
    const message = (phase.messages ?? []).find((m) => m.id === recordId);
    if (message) {
      // Like a finding, a message is checked against the PHASE's current
      // candidate and contract (binding.ts's currentVersionsFor does the same
      // for decisions and findings); only its record version is its own.
      return {
        kind: "message",
        candidateSha: phase.candidate?.sha ?? "",
        contractVersion: phase.contract.contractVersion,
        recordVersion: message.messageVersion,
        label: `message ${message.id}`,
      };
    }
  }
  return undefined;
}

export function checkItemCarried(phase: PhaseState, event: EvItemCarried): CommandCheck {
  if (!event.toPhase || event.toPhase.trim().length === 0) {
    return { ok: false, reason: `carrying ${event.recordId} needs a target phase (--to <phase-id>)` };
  }
  const record = carriedRecord(phase, event.recordId, event.recordKind);
  if (!record) return { ok: false, reason: `unknown finding or message ${event.recordId}` };
  if ((phase.carriedItems ?? []).includes(event.recordId)) {
    return { ok: false, reason: `${event.recordId} is already carried` };
  }
  // OD addendum A: a second owner act on the same id is refused, naming the
  // existing act. A deferral already put this item's fix off; carrying it too
  // would silently drop one act.
  const existingDefer = (phase.deferrals ?? []).find((d) => d.itemId === event.recordId && d.status === "open");
  if (existingDefer) {
    return { ok: false, reason: `${event.recordId} is already deferred (${existingDefer.id}); resolve or drop that deferral first` };
  }
  return checkBinding(
    { candidateSha: event.boundCandidateSha, contractVersion: event.boundContractVersion, recordVersion: event.boundRecordVersion },
    { candidateSha: record.candidateSha, contractVersion: record.contractVersion, recordVersion: record.recordVersion },
    record.label,
  );
}

export function applyItemCarried(phase: PhaseState, event: EvItemCarried): PhaseState {
  const carriedItems = [...(phase.carriedItems ?? [])];
  const carriedTo = { ...(phase.carriedTo ?? {}) };
  const mark = (id: string) => {
    if (!carriedItems.includes(id)) carriedItems.push(id);
    carriedTo[id] = event.toPhase;
  };
  mark(event.recordId);
  // A message carry also carries the finding it was raised from (its
  // `sourceRecordId`), so the blocking item behind a blocker message is
  // answered too.
  const message = (phase.messages ?? []).find((m) => m.id === event.recordId);
  if (message?.sourceRecordId && phase.findings.some((f) => f.id === message.sourceRecordId)) mark(message.sourceRecordId);

  // The record-level action answers the open request about it — including a
  // request linked to it as a MESSAGE (a blocker's own request carries
  // `linkedMessageId`, which `autoResolveLinkedRequest` does not match).
  let next = autoResolveLinkedRequest({ ...phase, carriedItems, carriedTo }, event.recordId, "carried", event.boundCandidateSha, event.boundContractVersion);
  next = {
    ...next,
    ownerRequests: next.ownerRequests.map((r) =>
      r.status === "open" && r.linkedMessageId === event.recordId
        ? {
            ...r,
            status: "resolved" as const,
            resolution: { option: "carried" },
            resolvedBinding: { candidateSha: event.boundCandidateSha, contractVersion: event.boundContractVersion },
          }
        : r,
    ),
  };

  // The plain budget gate (the phase parked with only advisories open) is the
  // owner's ONE "accept with carried items" decision: carrying any item
  // carries every open advisory at once and resolves the gate.
  const gate = next.ownerRequests.find((r) => r.status === "open" && isBudgetGateRequest(r));
  if (gate) {
    for (const f of next.findings.filter((f) => f.status === "open" && f.severity === "advisory")) mark(f.id);
    next = {
      ...next,
      carriedItems,
      carriedTo,
      ownerRequests: next.ownerRequests.map((r) =>
        r.id === gate.id
          ? {
              ...r,
              status: "resolved" as const,
              resolution: { option: "accept_carried" },
              resolvedBinding: { candidateSha: event.boundCandidateSha, contractVersion: event.boundContractVersion },
            }
          : r,
      ),
    };
  }

  // Accept once no blocking item remains uncarried; advisories never block
  // acceptance (owner directive ODP-1). The candidate the carry was given for
  // is recorded so `accept()` can refuse a stale or failing one (ODP-2).
  const blockingLeft = next.findings.some(
    (f) => f.status === "open" && f.severity === "blocking" && !carriedItems.includes(f.id),
  );
  return blockingLeft ? next : { ...next, acceptedWithCarried: true, carriedCandidateSha: event.boundCandidateSha };
}

// --- OVERRIDE_CAST -------------------------------------------------------

type EvOverrideCast = Extract<Event, { type: "OVERRIDE_CAST" }>;

export function checkOverrideCast(phase: PhaseState, event: EvOverrideCast): CommandCheck {
  const decision = phase.decisions.find((d) => d.id === event.override.decisionId);
  if (!decision) return { ok: false, reason: `decision ${event.override.decisionId} does not exist` };
  if (!phase.candidate) return { ok: false, reason: `phase ${phase.phaseId} has no frozen candidate yet` };
  return checkBinding(
    {
      candidateSha: event.override.boundCandidateSha,
      contractVersion: event.override.boundContractVersion,
      recordVersion: event.override.boundRecordVersion,
    },
    { candidateSha: phase.candidate.sha, contractVersion: phase.contract.contractVersion, recordVersion: decision.version },
    `decision ${decision.id}`,
  );
}

export function applyOverrideCast(phase: PhaseState, event: EvOverrideCast): PhaseState {
  const withOverride = { ...phase, overrides: [...phase.overrides, event.override] };
  return autoResolveLinkedRequest(
    withOverride,
    event.override.decisionId,
    "override",
    event.override.boundCandidateSha,
    event.override.boundContractVersion,
  );
}

// --- Routing out of AWAITING_OWNER --------------------------------------

/** What the phase should do once a command has just been applied while it
 * was AWAITING_OWNER: stay parked if any owner request is still open;
 * otherwise resume — REPAIRING if an open correction still needs a round,
 * RESOLVING if the candidate's checks/probe/reviews are already valid
 * (guaranteed once a candidate exists, since AWAITING_OWNER's record-level
 * requests only ever arise from RESOLVING), or AWAITING_OWNER defensively
 * if there is no candidate at all (should not happen for these three
 * commands, which all require a record that itself requires a candidate). */
export type AwaitingOwnerTarget = "AWAITING_OWNER" | "REPAIRING" | "RESOLVING";

export function awaitingOwnerTarget(phase: PhaseState): AwaitingOwnerTarget {
  // The owner's "accept with carried items" for this candidate answers every
  // open request, as accept() does; an open correction still needs its round.
  const carried = Boolean(phase.acceptedWithCarried && phase.candidate && phase.carriedCandidateSha === phase.candidate.sha);
  // Plan 06i: a non-blocking request (a late discovery's escalation) does not
  // park the phase, and neither does a stale one (its item is closed or
  // superseded): the park agrees with accept(), which ignores both.
  if (!carried && phase.ownerRequests.some((r) => r.status === "open" && r.blocking !== false && !ownerRequestIsStale(phase, r))) return "AWAITING_OWNER";
  if (phase.corrections.some((c) => c.status === "open")) return "REPAIRING";
  if (phase.candidate) return "RESOLVING";
  return "AWAITING_OWNER";
}
