// Plan 06i: triage — the loop decides about what it finds, honestly and
// visibly.
//
// Every finding and every discovered decision of the ledger ends in exactly
// ONE disposition:
//
//   fix       — it blocks acceptance until repaired;
//   tradeoff  — accepted, with what was chosen, the alternative and why;
//   escalate  — an owner request is open.
//
// `disposition(record, evidence)` is PURE and is the ONLY place the decision
// is made. Nothing else (blocksAcceptance, the evaluator prompt code, the
// views) computes a disposition itself; they call this function or read its
// record.
//
// The one rule that separates fix from trade-off:
//   - anything that gives a wrong value, offer or output on a reachable
//     path, or contradicts the current golden source or the plan, must be
//     fixed;
//   - only judgement calls (style, hardening, ergonomics) may be trade-offs;
//   - matching an earlier drafting decision is no exemption.
//
// Severity follows impact, not the reviewer's label: a finding raised at
// advisory severity whose impact is `wrong-output` is a fix, and a blocking
// finding whose evaluator classifies it `judgement` is a trade-off that does
// NOT block acceptance. `accept()` and `blocksAcceptance` read the item's
// disposition, never the reviewer's original label. A wrong-output record is
// item-linked by definition, so 06g's round-2 downgrade never applies to it.

import type { Decision, Finding, PhaseState } from "./types.ts";

/** Plan 06i (A3/F-A-97): the owner request an escalation names. An
 * escalation must point at a request the owner can actually answer, so if an
 * OPEN request already links the item (an `open_finding` park request, a
 * `blocker_panel` request) that request's id is reused; only when the item
 * has no open request does the deterministic triage request id apply. Pure:
 * the conductor opens the request this id names when it does not exist. */
export function escalationRequestId(phase: PhaseState, itemId: string): string {
  const existing = (phase.ownerRequests ?? []).find(
    (r) => r.status === "open" && (r.linkedFindingId === itemId || r.linkedDecisionId === itemId),
  );
  return existing?.id ?? uniqueOwnerRequestId(phase, `OR-${phase.phaseId}-triage-${itemId}`);
}

/** An owner request id no request in the phase has used yet: `base`, or
 * `base-2`, `base-3`, ... A record escalated again in a later round (its first
 * request already resolved) gets a NEW id, so a command naming an id always
 * names exactly one request (valuation 02g: three requests shared one id, the
 * resolve found the resolved copy and the open copy parked the phase for good). */
export function uniqueOwnerRequestId(phase: PhaseState, base: string): string {
  const taken = new Set((phase.ownerRequests ?? []).map((r) => r.id));
  if (!taken.has(base)) return base;
  let n = 2;
  while (taken.has(`${base}-${n}`)) n += 1;
  return `${base}-${n}`;
}

/** The request a command or event names. Logs written before ids were unique
 * can hold several requests with one id; the open one is the one the owner can
 * still answer, so it wins, then the latest. */
export function findOwnerRequest<R extends { id: string; status: string }>(phase: { ownerRequests: R[] }, id: string): R | undefined {
  const same = (phase.ownerRequests ?? []).filter((r) => r.id === id);
  return same.find((r) => r.status === "open") ?? same[same.length - 1];
}

/** What a record's impact on the work is. `wrong-output` covers a wrong
 * value, offer or output on a reachable path; `contract` covers a record
 * that contradicts the current golden source or the plan; `judgement`
 * covers style, hardening and ergonomics. */
export type Impact = "wrong-output" | "contract" | "judgement";

/** The three dispositions. */
export type Disposition =
  | { kind: "fix"; reason: string }
  | { kind: "tradeoff"; chosen: string; alternative: string; why: string }
  | { kind: "escalate"; reason: string; ownerRequestId: string };

/** One finding's or discovered decision's triage record. A record without a
 * disposition at the end of evaluation is a defect of the loop, never a
 * pass; the conductor re-prompts once and then records `escalate`. */
export interface TriageRecord {
  itemId: string;
  source: "finding" | "decision";
  impact: Impact;
  disposition?: Disposition;
  /** Plan 06i (C3): set when this record is a duplicate of another; its
   * disposition is the original's (06c B4's "merged", never "disproved"). */
  duplicateOf?: string;
}

/** A deferral: an item whose fix is put off. It needs a guard — a test that
 * shows its current cost, or a recorded owner ruling — or it is refused with
 * the reason. Listed under "Open deferrals" in `tt summary` and the status
 * until resolved. */
export interface Deferral {
  id: string;
  itemId: string;
  text: string;
  /** The guard: a test name/command that shows the item's current cost. */
  test?: string;
  /** The guard: the id of the owner ruling that recorded the deferral. */
  ownerRuling?: string;
  toPhase?: string;
  status: "open" | "resolved";
}

/** Everything `disposition()` may read besides the record itself. Every
 * field is a fact the conductor (or an evaluator's item check) established;
 * `disposition()` invents none of them. */
export interface TriageEvidence {
  /** The evaluator's impact class for this record. Absent means the record
   * is NOT classified, which escalates (never a silent default). */
  classified?: boolean;
  /** The evaluator re-checked a wrong-output claim against the candidate and
   * confirmed it (06c anchors rule). */
  wrongOutputConfirmed?: boolean;
  /** The record contradicts the current golden source. */
  contradictsGolden?: boolean;
  /** The record contradicts the plan (a drafting decision included). */
  contradictsPlan?: boolean;
  /** A judgement call's chosen / alternative / why. All three, or the record
   * escalates instead of becoming a trade-off. */
  chosen?: string;
  alternative?: string;
  why?: string;
  /** The round panel dropped this finding (a recorded review outcome: it did
   * not keep it blocking). It is a trade-off, with the panel's own reason. */
  panelDropped?: boolean;
  panelReason?: string;
  /** A duplicate whose original has no triage record: it cannot copy a
   * disposition, so it escalates and the owner decides. */
  orphanDuplicate?: boolean;
  /** A discovery that arrived after the round's ballot/evaluation window. It
   * is recorded and offered to the owner, never dropped, but it does not gate
   * acceptance (the owner ruled it non-blocking). */
  lateDiscovery?: boolean;
  /** A discovered decision that received no ballots at all. */
  noBallots?: boolean;
  /** A failed vote whose outcome would change the output. */
  failedVoteChangesOutput?: boolean;
  /** The record could not be classified. */
  unclassified?: boolean;
  /** The record is closed already: it was repaired (a fix), or disproved /
   * superseded / accepted by the owner (a trade-off). */
  fate?: "repaired" | "disproved" | "superseded" | "accepted";
  fateReason?: string;
  /** The owner request id to name when escalating. Defaults to a stable id
   * derived from the item, so a pure call still produces a usable record. */
  ownerRequestId?: string;
  /** A human-readable reason to record on a fix or an escalate. */
  reason?: string;
}

function isNonEmpty(v: string | undefined): v is string {
  return typeof v === "string" && v.trim().length > 0;
}

function escalate(itemId: string, reason: string, evidence: TriageEvidence): Disposition {
  return {
    kind: "escalate",
    reason,
    ownerRequestId: isNonEmpty(evidence.ownerRequestId) ? evidence.ownerRequestId : `OR-${itemId}`,
  };
}

/**
 * The only place fix / trade-off / escalate is decided. Pure: no clock, no
 * I/O, no model.
 */
export function disposition(record: TriageRecord, evidence: TriageEvidence = {}): Disposition {
  // An unclassified record escalates: the evaluator's impact class is what
  // the rule reads, and guessing one is how a wrong value would silently
  // become a trade-off.
  if (evidence.unclassified === true || evidence.classified === false) {
    return escalate(record.itemId, isNonEmpty(evidence.reason) ? evidence.reason : "the evaluator gave no impact classification", evidence);
  }
  // A discovery that arrived after the round's ballot/evaluation window: the
  // owner ruled it must be visible but non-blocking, never dropped. The
  // DECISION lives here, not in the conductor; the conductor only records the
  // disposition this returns and opens the non-blocking request it names.
  if (evidence.lateDiscovery === true) {
    return escalate(
      record.itemId,
      isNonEmpty(evidence.reason) ? evidence.reason : "the discovery arrived after the review window and is offered to the owner, never dropped",
      evidence,
    );
  }
  // A duplicate whose original has no triage record cannot copy a
  // disposition: it escalates and the owner decides.
  if (evidence.orphanDuplicate === true) {
    return escalate(
      record.itemId,
      isNonEmpty(evidence.reason) ? evidence.reason : "the original of this duplicate has no disposition",
      evidence,
    );
  }
  // A wrong value, offer or output on a reachable path is a fix, whatever the
  // reviewer's label, whatever round it was raised in, and whatever the round
  // panel voted: a panel drop can NEVER downgrade a confirmed wrong value.
  if (record.impact === "wrong-output" || evidence.wrongOutputConfirmed === true) {
    return { kind: "fix", reason: isNonEmpty(evidence.reason) ? evidence.reason : "a confirmed wrong output on a reachable path must be fixed" };
  }
  // Contradicting the current golden source or the plan is a fix, even when
  // the record matches an earlier drafting decision.
  if (evidence.contradictsGolden === true || evidence.contradictsPlan === true) {
    return { kind: "fix", reason: isNonEmpty(evidence.reason) ? evidence.reason : "the record contradicts the current golden source or the plan" };
  }
  // A record already repaired was a fix: it blocked until it was fixed.
  if (evidence.fate === "repaired") {
    return { kind: "fix", reason: isNonEmpty(evidence.fateReason) ? evidence.fateReason : "repaired at a later candidate" };
  }
  // Plan 06i (OD-20): a panel drop is NOT by itself a trade-off. The panel's
  // severity vote is neither the evaluator's classification nor a source of
  // chosen/alternative/why, so a panel-lowered finding goes through the same
  // judgement path as any other: a real evaluator classification with all
  // three fields is a trade-off; a missing field escalates. The panel's
  // severityReason stays visible as context in the summary, but it never
  // fills a trade-off field.
  // A closed record whose fate was not a repair is a recorded trade-off: the
  // ledger keeps it and the summary lists it, so nothing disappears. This is
  // checked BEFORE the contract branch so a superseded/disproved contract
  // record is not misread as an open owner question.
  if (evidence.fate === "disproved" || evidence.fate === "superseded" || evidence.fate === "accepted") {
    return {
      kind: "tradeoff",
      chosen: evidence.fate === "accepted" ? "accepted by the owner" : evidence.fate === "disproved" ? "recorded as disproved" : "superseded by a later candidate",
      alternative: "repair it or keep it open",
      why: isNonEmpty(evidence.fateReason) ? evidence.fateReason : evidence.fate,
    };
  }
  // A discovered decision with no ballots, or a failed vote whose outcome
  // would change the output, escalates: the owner decides, and acceptance
  // waits for the request.
  if (evidence.noBallots === true) {
    return escalate(record.itemId, isNonEmpty(evidence.reason) ? evidence.reason : "a discovered decision received no ballots", evidence);
  }
  if (evidence.failedVoteChangesOutput === true) {
    return escalate(record.itemId, isNonEmpty(evidence.reason) ? evidence.reason : "a failed vote would change the output", evidence);
  }
  // A contract record that does not contradict the golden source or the plan
  // still needs the owner: it is not a judgement call.
  if (record.impact === "contract") {
    return escalate(record.itemId, isNonEmpty(evidence.reason) ? evidence.reason : "a contract record needs the owner's decision", evidence);
  }
  // Only a judgement call may be a trade-off, and only with all three fields.
  if (isNonEmpty(evidence.chosen) && isNonEmpty(evidence.alternative) && isNonEmpty(evidence.why)) {
    return { kind: "tradeoff", chosen: evidence.chosen, alternative: evidence.alternative, why: evidence.why };
  }
  return escalate(record.itemId, isNonEmpty(evidence.reason) ? evidence.reason : "a judgement call with no chosen/alternative/why must be escalated", evidence);
}

/** One plain sentence for a disposition, for the views and the status. */
export function dispositionReason(d: Disposition): string {
  switch (d.kind) {
    case "fix":
      return `fix: ${d.reason}`;
    case "tradeoff":
      return `trade-off: ${d.chosen} (alternative: ${d.alternative}; why: ${d.why})`;
    case "escalate":
      return `escalate: ${d.reason}`;
  }
}

export function isFix(d: Disposition | undefined): d is Extract<Disposition, { kind: "fix" }> {
  return d?.kind === "fix";
}

export function isTradeoff(d: Disposition | undefined): d is Extract<Disposition, { kind: "tradeoff" }> {
  return d?.kind === "tradeoff";
}

export function isEscalate(d: Disposition | undefined): d is Extract<Disposition, { kind: "escalate" }> {
  return d?.kind === "escalate";
}

/** Plan 06k1 (A3): the default number of consecutive rounds the same
 * requirement may take a new blocking finding of the same kind before the
 * owner is asked instead of another repair. `#+TT_VARIANT_LIMIT` overrides
 * it. */
export const DEFAULT_VARIANT_LIMIT = 3;

/** Plan 06k1 (A3): the frozen `#+TT_VARIANT_LIMIT`, or the default of 3. */
export function variantLimitOf(contract: { variantLimit?: number } | undefined): number {
  const n = contract?.variantLimit;
  if (typeof n !== "number" || !Number.isFinite(n) || n < 1) return DEFAULT_VARIANT_LIMIT;
  return Math.floor(n);
}

/** Plan 06k1 (A3): the requirement+kind whose blocking findings reached the
 * variant limit in the last N consecutive rounds, or undefined. A finding
 * counts once per round it was raised in (`roundRaised`); the phase's own
 * round budget is not consulted — this is a separate escalation. */
export function variantEscalation(
  phase: PhaseState,
  limit = variantLimitOf(phase.contract),
): { itemId: string; kind: Finding["kind"]; rounds: number[] } | undefined {
  const current = phase.round ?? 0;
  if (limit < 2 || current < limit) return undefined;
  const byKey = new Map<string, { itemId: string; kind: Finding["kind"]; rounds: Set<number> }>();
  for (const f of phase.findings) {
    if (f.severity !== "blocking" || !f.itemId || typeof f.roundRaised !== "number") continue;
    // Finding D-M-60: only a REVIEWER's finding that is still open or was
    // repaired is a real variant. A conductor-raised finding is re-raised on
    // every freeze (one per candidate, not a new variant), and a withdrawn,
    // panel-dropped, superseded or disproved finding never establishes a
    // streak. (A panel drop lowers severity to advisory, so the check above
    // already excludes it.)
    if (f.raisedBy === "conductor" || f.raisedBy === "owner") continue;
    if (f.status !== "open" && f.status !== "repaired") continue;
    const key = `${f.itemId}\u0000${f.kind}`;
    let entry = byKey.get(key);
    if (!entry) {
      entry = { itemId: f.itemId, kind: f.kind, rounds: new Set() };
      byKey.set(key, entry);
    }
    entry.rounds.add(f.roundRaised);
  }
  for (const entry of byKey.values()) {
    const window: number[] = [];
    let all = true;
    for (let r = current - limit + 1; r <= current; r += 1) {
      if (!entry.rounds.has(r)) {
        all = false;
        break;
      }
      window.push(r);
    }
    if (all) return { itemId: entry.itemId, kind: entry.kind, rounds: window };
  }
  return undefined;
}

/** Plan 06k1 (A3): true when a requirement reached the variant limit and the
 * owner must be asked instead of another repair round. */
export function variantLimitReached(phase: PhaseState): boolean {
  return variantEscalation(phase) !== undefined;
}

/** The triage record for one item, if any. */
export function triageRecordFor(records: readonly TriageRecord[] | undefined, itemId: string): TriageRecord | undefined {
  return (records ?? []).find((r) => r.itemId === itemId);
}

/** True when the triage says the item must be fixed — the one query
 * `blocksAcceptance` uses, so it never re-derives the rule. */
export function triageFixFor(records: readonly TriageRecord[] | undefined, itemId: string): boolean {
  return isFix(triageRecordFor(records, itemId)?.disposition);
}

/** The records that still block acceptance: a `fix` disposition whose item is
 * an open finding or a live discovered decision of the current candidate. A
 * repaired finding's fix record is history — it no longer blocks. */
export function blockingTriageRecords(phase: PhaseState): TriageRecord[] {
  const open = new Set<string>();
  // EVERY open finding counts, whatever candidate it is bound to: a blocking
  // finding raised on an earlier candidate keeps blocking until its reviewer
  // confirms it repaired (design §4.2).
  for (const f of phase.findings.filter((x) => x.status === "open")) open.add(f.id);
  for (const d of discoveredDecisions(phase)) open.add(d.id);
  return (phase.triage ?? []).filter((r) => isFix(r.disposition) && open.has(r.itemId));
}

/** Plan 06i: an open owner request whose linked item is already closed
 * (superseded/repaired/accepted/disproved) is moot — it must not keep
 * blocking acceptance. A request with no linked item (a budget/gate request)
 * is never stale. */
export function ownerRequestIsStale(
  phase: PhaseState,
  request: { linkedFindingId?: string; linkedDecisionId?: string },
): boolean {
  if (request.linkedFindingId) {
    const finding = phase.findings.find((f) => f.id === request.linkedFindingId);
    if (finding && finding.status !== "open") return true;
  }
  if (request.linkedDecisionId) {
    const decision = phase.decisions.find((d) => d.id === request.linkedDecisionId);
    if (decision && (decision.supersededBy || decision.supersededByCorrection)) return true;
  }
  return false;
}

/** True when any triage record still blocks acceptance — the one query
 * `accept()` uses, so the rule lives here, not in the predicate. */
export function hasBlockingFix(phase: PhaseState): boolean {
  return blockingTriageRecords(phase).length > 0;
}

/** A blocking-severity finding that has NOT been triaged yet. Acceptance
 * fails safe on it (the label still blocks) until triage decides; once triage
 * records a trade-off, the label no longer blocks (A2: acceptance follows the
 * disposition, not the reviewer's label). */
export function undispositionedBlockingFinding(phase: PhaseState): boolean {
  return phase.findings.some(
    (f) => f.status === "open" && f.severity === "blocking" && triageRecordFor(phase.triage, f.id) === undefined,
  );
}

/** True when the finding's own triage disposition is a trade-off, so its
 * original blocking label must not keep it blocking. */
export function findingDispositionIsTradeoff(records: readonly TriageRecord[] | undefined, itemId: string): boolean {
  return isTradeoff(triageRecordFor(records, itemId)?.disposition);
}

/** Every OPEN finding — the findings triage must classify this round,
 * whatever their severity and whatever candidate they were raised on. A
 * blocking finding raised on an earlier candidate and not yet confirmed
 * repaired is still open, so it still owes a fresh classification; freeze
 * clears `itemChecks`, so without this it would be escalated without ever
 * being re-asked (A3). */
export function openFindings(phase: PhaseState): Finding[] {
  return phase.findings.filter((f) => f.status === "open");
}

/** Every live discovered decision of the current candidate. */
export function discoveredDecisions(phase: PhaseState): Decision[] {
  const C = phase.candidate?.sha;
  return phase.decisions.filter(
    (d) => d.source === "reviewer-discovered" && !d.supersededBy && !d.supersededByCorrection && (!C || d.boundCandidateSha === C),
  );
}

/** EVERY finding and EVERY discovered decision of the ledger, as the records
 * that must each carry exactly one disposition (C3: nothing leaves the
 * ledger without one). */
export function ledgerRecords(phase: PhaseState): Array<{ source: "finding" | "decision"; id: string }> {
  return [
    ...phase.findings.map((f) => ({ source: "finding" as const, id: f.id })),
    ...phase.decisions.filter((d) => d.source === "reviewer-discovered").map((d) => ({ source: "decision" as const, id: d.id })),
  ];
}

/** The evaluator's impact classification, when it gave one for this id. */
export function impactOverride(phase: PhaseState, itemId: string): Impact | undefined {
  const check = (phase.itemChecks ?? []).find((c) => c.itemId === itemId);
  const impact = check?.impact;
  return impact === "wrong-output" || impact === "contract" || impact === "judgement" ? impact : undefined;
}

/** True when the evaluator classified this id: a `confirmed` check with an
 * explicit impact. Anything else is unclassified and escalates. */
export function isClassified(phase: PhaseState, itemId: string): boolean {
  const check = (phase.itemChecks ?? []).find((c) => c.itemId === itemId);
  return check?.verdict === "confirmed" && impactOverride(phase, itemId) !== undefined;
}

/** A `contract` finding's impact needs no evaluator: it is a contract
 * objection by construction (design §4.2), so it is classified `contract`. */
function isContractFinding(phase: PhaseState, id: string): boolean {
  return phase.findings.some((f) => f.id === id && f.kind === "contract");
}

/** A conductor-raised item finding (an unmet/deviating item) needs no
 * evaluator either: the item loop raised it FROM the recorded verdicts, so
 * its impact is plain — it is a defect that blocks until repaired. */
function isConductorItemFinding(phase: PhaseState, id: string): boolean {
  return phase.findings.some((f) => f.id === id && f.raisedBy === "conductor" && f.itemId !== undefined);
}

/** The record's impact class: the evaluator's, or the one the record itself
 * establishes. */
export function recordImpact(phase: PhaseState, id: string): Impact {
  if (impactOverride(phase, id)) return impactOverride(phase, id)!;
  if (isContractFinding(phase, id)) return "contract";
  if (isConductorItemFinding(phase, id)) return "wrong-output";
  return "judgement";
}

/** True when the record is classified: the evaluator classified it, or the
 * record itself establishes its impact (a contract finding, or a
 * conductor-raised item finding). Anything else escalates. */
export function recordClassified(phase: PhaseState, source: "finding" | "decision", id: string): boolean {
  if (source !== "finding") return isClassified(phase, id);
  return isContractFinding(phase, id) || isConductorItemFinding(phase, id) || isClassified(phase, id);
}

/** Plan 06i (A3): a `contract` classification must be grounded in the golden
 * note when the phase declares one — a distinctive run of the note's own
 * words, so an evaluator cannot assert a contradiction it did not read. */
export function goldenCited(phase: PhaseState, evidence: string): boolean {
  const golden = phase.contract.golden;
  if (!golden || golden.trim().length === 0) return false;
  const words = (s: string) => s.toLowerCase().replace(/[^a-z0-9\s]+/g, " ").split(/\s+/).filter(Boolean);
  const sig = words(golden).filter((w) => w.length >= 4);
  if (sig.length === 0) return false;
  const e = new Set(words(evidence));
  const matched = sig.filter((w) => e.has(w)).length;
  return matched >= Math.min(3, sig.length) && matched / sig.length >= 0.5;
}

/** The triage evidence for one finding, assembled from facts the conductor
 * and the evaluator established. Nothing here is model-invented: the
 * judgement fields come from the evaluator's own check, never stock text. */
export function evidenceForFinding(finding: Finding, phase: PhaseState): TriageEvidence {
  const check = (phase.itemChecks ?? []).find((c) => c.itemId === finding.id);
  const contractFinding = finding.kind === "contract";
  const conductorItemFinding = finding.raisedBy === "conductor" && finding.itemId !== undefined;
  const classified = recordClassified(phase, "finding", finding.id);
  const impact = impactOverride(phase, finding.id) ?? (contractFinding ? "contract" : conductorItemFinding ? "wrong-output" : undefined);
  if (finding.status === "repaired") {
    return { classified: true, fate: "repaired", fateReason: `repaired at candidate ${(finding.repairedByCandidateSha ?? "").slice(0, 8) || "a later candidate"}` };
  }
  if (finding.status === "merged") {
    return { classified: true, fate: "disproved", fateReason: `merged into ${finding.mergedInto ?? "an existing record"}` };
  }
  if (finding.status === "disproved") {
    return { classified: true, fate: "disproved", fateReason: finding.disprovedEvidence ?? "disproved by the evaluator" };
  }
  if (finding.status === "accepted") {
    return { classified: true, fate: "accepted", fateReason: finding.acceptedScope ?? "accepted by the owner" };
  }
  if (finding.status === "superseded") {
    return { classified: true, fate: "superseded", fateReason: finding.supersededBy ?? "superseded by a later candidate" };
  }
  // A contract finding is a contract record by construction, even without an
  // evaluator check; a defect finding needs the evaluator's classification.
  const contractImpact = contractFinding || (check?.verdict === "confirmed" && impact === "contract");
  const hasGolden = typeof phase.contract.golden === "string" && phase.contract.golden.trim().length > 0;
  const cited = contractImpact && check ? goldenCited(phase, check.evidence) : false;
  // A round-panel drop is the recorded review outcome: the panel voted not to
  // keep the finding blocking. An evaluator/reviewer severity change is not.
  const panelDropped = finding.severity === "advisory" && finding.severityChangedBy === "panel";
  return {
    panelDropped,
    panelReason: finding.severityReason,
    // A `contract` classification with a declared golden note must cite it;
    // otherwise the record is unclassified and escalates (A3's golden
    // re-check). A reviewer's severity label NEVER classifies a record: only
    // an evaluator's confirmed impact does.
    classified: classified && (!contractImpact || !hasGolden || cited),
    wrongOutputConfirmed: check?.verdict === "confirmed" && impact === "wrong-output",
    contradictsGolden: cited,
    contradictsPlan: contractImpact && (!hasGolden || !check),
    // The judgement fields are the evaluator's own, never stock text, and a
    // missing one is NEVER borrowed from another field: without all three the
    // record escalates (A2/R4).
    chosen: check?.chosen,
    alternative: check?.alternative,
    why: check?.why,
    reason:
      contractImpact && hasGolden && !cited
        ? `the contract classification does not cite the golden note`
        : check?.verdict === "confirmed" && impact === "wrong-output"
          ? `wrong output confirmed by the evaluator: ${check.evidence}`
          : undefined,
  };
}

/** The triage evidence for one discovered decision. `voteFailed` is the vote
 * outcome the conductor computed (triage.ts must not import the predicate's
 * tally), so a failed vote that would change output escalates. */
export function evidenceForDecision(decision: Decision, phase: PhaseState, voteFailed = false): TriageEvidence {
  const check = (phase.itemChecks ?? []).find((c) => c.itemId === decision.id);
  const classified = recordClassified(phase, "decision", decision.id);
  const impact = impactOverride(phase, decision.id);
  if (decision.supersededBy || decision.supersededByCorrection) {
    return { classified: true, fate: "superseded", fateReason: decision.supersededBy ?? (decision.supersededByCorrection ? `superseded by correction ${decision.supersededByCorrection}` : "") };
  }
  // A discovered decision bound to an earlier candidate is not of the current
  // candidate (see the finding case above).
  if (phase.candidate && decision.boundCandidateSha !== phase.candidate.sha) {
    return { classified: true, fate: "superseded", fateReason: `superseded by candidate ${phase.candidate.sha.slice(0, 8)}` };
  }
  // Only a decision that NEEDS a vote (delegated or reserved) can disappear
  // by receiving none. A `detail` decision is settled by its class.
  const noBallots = decision.class !== "detail" && !(phase.ballots ?? []).some((b) => b.decisionId === decision.id);
  const contractImpact = check?.verdict === "confirmed" && impact === "contract";
  const hasGolden = typeof phase.contract.golden === "string" && phase.contract.golden.trim().length > 0;
  const cited = contractImpact && check ? goldenCited(phase, check.evidence) : false;
  return {
    classified: classified && (!contractImpact || !hasGolden || cited),
    noBallots,
    failedVoteChangesOutput: voteFailed,
    wrongOutputConfirmed: check?.verdict === "confirmed" && impact === "wrong-output",
    contradictsGolden: cited,
    contradictsPlan: contractImpact && !hasGolden,
    chosen: check?.chosen,
    alternative: check?.alternative,
    why: check?.why,
    reason:
      contractImpact && hasGolden && !cited
        ? `the contract classification of ${decision.id} does not cite the golden note`
        : noBallots
          ? `decision ${decision.id} received no ballots`
          : voteFailed
            ? `decision ${decision.id} failed its vote; its outcome would change the output`
            : undefined,
  };
}
