// reduce(state, event) -> ReduceResult
//
// A TOTAL function: it never throws, and it explicitly rejects unknown or
// out-of-order events rather than silently ignoring them or crashing. The
// only way it changes state is by applying a row from transitions.ts whose
// `from`/`axis` matches the current state, whose `trigger` matches the
// event's type, and whose `guard` accepts (state, event) — or, for events
// that only append a record without moving the phase's own FSM state
// (ballots, overrides, findings, owner requests, ACTION_STARTED), by
// `applyRecordEvent` below.
//
// Every owner-authored or reviewer-authored event that design §7.1 binds to
// a (candidate, contract, record version) is checked against that binding
// here — via binding.ts — with a visible, human-readable reason on
// mismatch, before anything else about the event is considered.

import { checkBallotBinding, checkBinding, checkTupleBinding, currentVersionsFor } from "./binding.ts";
import { next as computeNext } from "./next.ts";
import {
  applyFindingAcceptedByOwner,
  applyOverrideCast,
  applyOwnerRequestResolved,
  checkFindingAcceptedByOwner,
  checkOverrideCast,
  checkOwnerRequestResolved,
} from "./owner-commands.ts";
import { isLiveDecision, reviewIngestionIssue, sameVersion } from "./predicate.ts";
import { rowsFor } from "./transitions.ts";
import type { BindingTuple, ContractVersion, Event, InFlightKey, ReduceResult, State } from "./types.ts";

const KNOWN_EVENT_TYPES = new Set<string>([
  "ATTEMPT_STARTED",
  "SUBMIT_PHASE",
  "ATTEMPT_TIMED_OUT",
  "ATTEMPT_NO_SUBMISSION",
  "ATTEMPT_INTERRUPTED",
  "FREEZE_COMPLETED",
  "FREEZE_TIMED_OUT",
  "FREEZE_INTERRUPTED",
  "CHECKS_PASSED",
  "CHECKS_FAILED",
  "CHECKS_INTERRUPTED",
  "PROBE_PASSED",
  "PROBE_FAILED",
  "PROBE_INTERRUPTED",
  "REVIEW_SUBMITTED",
  "REVIEW_TIMED_OUT",
  "ACTION_STARTED",
  "BALLOT_CAST",
  "OVERRIDE_CAST",
  "FINDING_RAISED",
  "FINDING_CONFIRMED_REPAIRED",
  "FINDING_DISPROVED",
  "FINDING_ACCEPTED_BY_OWNER",
  "FINDING_SEVERITY_LOWERED",
  "DECISION_CLASS_LOWERED",
  "OWNER_REQUEST_OPENED",
  "OWNER_REQUEST_RESOLVED",
  "ACCEPTED",
  "PUBLISH_INTENT",
  "PUBLISH_COMPLETED",
  "PUBLISH_STALE",
  "REPAIR_ATTEMPT_STARTED",
  "REPAIR_BUDGET_EXHAUSTED",
  "RESOLVING_INCOMPLETE",
  "GATE_REQUIRED",
  "GATE_FAILED",
  "GATE_INTERRUPTED",
  "REVISE",
  "AMEND",
  "CRITERION_AMENDED",
  "CRITERION_REVERTED",
  "RUN_BUDGET_EXCEEDED",
  "RUN_RESUMED",
  "LAUNCH_FAILED",
  "INTEGRITY_VIOLATED",
  "DECISION_ADDED",
  "NOTE_ADDED",
  "OWNER_INPUT_RECORDED",
  "DIRECTIVE_ADDED",
  "DIRECTIVE_WITHDRAWN",
  "DIRECTIVE_DELIVERED",
  "OWNER_CORRECTION",
  "OWNER_REQUEST_MARKED_UNNEEDED",
  "MISS_RECORDED",
  "NOTES_DELIVERED",
  "DECISION_MATCHED",
  "FINDING_ALSO_RAISED",
]);

function ok(state: State): ReduceResult {
  return { ok: true, state };
}

function rejected(state: State, reason: string): ReduceResult {
  return { ok: false, reason, state };
}

function bindingTupleForRecord(
  state: State,
  recordId: string,
  tuple: { candidateSha: string; contractVersion: ContractVersion; recordVersion: number },
): BindingTuple {
  return {
    runId: state.phase.runId,
    phaseId: state.phase.phaseId,
    candidateSha: tuple.candidateSha,
    contractVersion: tuple.contractVersion,
    recordId,
    recordVersion: tuple.recordVersion,
  };
}

function inFlightKeyFor(action: string, reviewer?: string): string {
  return action === "dispatch_review" ? `review_${reviewer}` : action;
}

/** Events handled directly by reduce.ts, not by the transitions table: they
 * only ever append or amend a record without moving the phase's own FSM
 * state, so they have no row in transitions.ts (which is scoped to
 * phase-state-changing edges, per the plan's phase-0 brief). */
function applyRecordEvent(state: State, event: Event): ReduceResult | undefined {
  const p = state.phase;
  switch (event.type) {
    case "ACTION_STARTED": {
      // Only an action next() is actually recommending right now may be
      // marked in flight — this both prevents starting arbitrary work and
      // is exactly what makes a second next() call return [] (no double
      // dispatch).
      const outstanding = computeNext(state).some(
        (a) => a.type === event.action && (event.action !== "dispatch_review" || a.reviewer === event.reviewer),
      );
      if (!outstanding) {
        return rejected(
          state,
          `action '${event.action}'${event.reviewer ? ` (${event.reviewer})` : ""} is not currently outstanding in phase ${p.phase}`,
        );
      }
      const key = inFlightKeyFor(event.action, event.reviewer) as InFlightKey;
      return ok({ ...state, phase: { ...p, inFlight: { ...p.inFlight, [key]: { actionId: event.actionId } } } });
    }

    case "BALLOT_CAST": {
      // design §7.1: bound to candidate, contract version AND the
      // decision's current record version.
      const check = checkBallotBinding(event.ballot, p);
      if (!check.ok) return rejected(state, check.reason!);
      const target = p.decisions.find((d) => d.id === event.ballot.decisionId);
      if (target && !isLiveDecision(target)) {
        return rejected(state, `decision ${target.id} is superseded (${target.supersededBy ?? "by correction"}) and is not votable`);
      }

      let next = { ...p, ballots: [...p.ballots, event.ballot] };
      if (event.ballot.contractObjection) {
        const decision = next.decisions.find((d) => d.id === event.ballot.decisionId);
        if (decision && !decision.linkedFindingId) {
          const finding = {
            id: `F-${next.phaseId}-contract-${next.findings.length + 1}`,
            version: 1,
            phaseId: next.phaseId,
            kind: "contract" as const,
            severity: "blocking" as const,
            evidence: event.ballot.evidence.join("; "),
            raisedBy: event.ballot.reviewer,
            status: "open" as const,
            linkedDecisionId: decision.id,
            boundCandidateSha: p.candidate!.sha,
          };
          next = {
            ...next,
            findings: [...next.findings, finding],
            decisions: next.decisions.map((d) =>
              d.id === decision.id ? { ...d, linkedFindingId: finding.id, version: d.version + 1 } : d,
            ),
          };
        }
      }
      return ok({ ...state, phase: next });
    }

    case "OVERRIDE_CAST": {
      // design §7.4: override is "recorded beside the ballots" — same
      // binding rule as a ballot (candidate, contract, decision version).
      const check = checkOverrideCast(p, event);
      if (!check.ok) return rejected(state, check.reason!);
      return ok({ ...state, phase: applyOverrideCast(p, event) });
    }

    case "FINDING_RAISED": {
      // design §4.1: "a finding needs evidence" — reject one with none.
      if (!event.finding.evidence || event.finding.evidence.trim().length === 0) {
        return rejected(state, "a finding must carry evidence; got none");
      }
      return ok({ ...state, phase: { ...p, findings: [...p.findings, event.finding] } });
    }

    case "DECISION_ADDED": {
      // Work packet 2a: a reviewer-discovered decision or a conductor
      // boundary trigger — the other two decision sources besides a
      // worker's disclosure (design §3.3). Bound to the phase's current
      // candidate/contract, same as any other record design §7.1 binds.
      const { decision } = event;
      if (p.decisions.some((d) => d.id === decision.id)) {
        return rejected(state, `decision ${decision.id} already exists`);
      }
      if (!p.candidate || decision.boundCandidateSha !== p.candidate.sha) {
        return rejected(state, `decision ${decision.id} must be bound to the current candidate`);
      }
      if (!sameVersion(decision.boundContractVersion, p.contract.contractVersion)) {
        return rejected(state, `decision ${decision.id} must be bound to the current contract version`);
      }
      return ok({ ...state, phase: { ...p, decisions: [...p.decisions, decision] } });
    }

    case "FINDING_CONFIRMED_REPAIRED": {
      // design §4.2: "a new candidate; checks pass on it; the raising
      // reviewer confirms, in its review of that candidate, that the
      // finding no longer holds". Every clause is checked, not assumed.
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      if (finding.raisedBy !== event.byReviewer) {
        return rejected(
          state,
          `only the raising reviewer (${finding.raisedBy}) may confirm finding ${event.findingId} repaired, not ${event.byReviewer}`,
        );
      }
      if (!p.candidate || event.candidateSha !== p.candidate.sha) {
        return rejected(state, `finding ${event.findingId} must be confirmed against the current candidate`);
      }
      if (event.candidateSha === finding.boundCandidateSha) {
        return rejected(
          state,
          `finding ${event.findingId} cannot be confirmed repaired on the same candidate it was raised on (${finding.boundCandidateSha})`,
        );
      }
      if (!p.checks || p.checks.candidateSha !== event.candidateSha || p.checks.passed !== true) {
        return rejected(state, `checks must have passed on candidate ${event.candidateSha} before finding ${event.findingId} can close`);
      }
      const review = p.reviews[event.byReviewer]?.review;
      const statement = review?.findingStatements.find((s) => s.findingId === event.findingId);
      if (!review || review.candidateSha !== event.candidateSha || !statement || statement.status !== "confirm") {
        return rejected(
          state,
          `finding ${event.findingId} repaired requires ${event.byReviewer}'s review of candidate ${event.candidateSha} to confirm it`,
        );
      }
      const findings = p.findings.map((f) =>
        f.id === event.findingId
          ? { ...f, status: "repaired" as const, repairedByCandidateSha: event.candidateSha }
          : f,
      );
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "FINDING_DISPROVED": {
      if (!event.evidence || event.evidence.trim().length === 0) {
        return rejected(state, `disproving finding ${event.findingId} requires counter-evidence; got none`);
      }
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      if (finding.raisedBy !== event.byReviewer) {
        return rejected(
          state,
          `only the raising reviewer (${finding.raisedBy}) may withdraw finding ${event.findingId}, not ${event.byReviewer}`,
        );
      }
      const findings = p.findings.map((f) =>
        f.id === event.findingId ? { ...f, status: "disproved" as const, disprovedEvidence: event.evidence } : f,
      );
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "FINDING_ACCEPTED_BY_OWNER": {
      // design §4.2: "accepted" requires the owner, and only the owner —
      // and, like every owner command, is bound to (C, K, record version).
      const check = checkFindingAcceptedByOwner(p, event);
      if (!check.ok) return rejected(state, check.reason!);
      return ok({ ...state, phase: applyFindingAcceptedByOwner(p, event) });
    }

    case "FINDING_SEVERITY_LOWERED": {
      // design §3.4: only the owner may lower a finding's severity.
      if (event.by !== "owner") {
        return rejected(state, `only the owner may lower a finding's severity, not '${String(event.by)}'`);
      }
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      const tuple = bindingTupleForRecord(state, event.findingId, {
        candidateSha: event.boundCandidateSha,
        contractVersion: event.boundContractVersion,
        recordVersion: event.boundRecordVersion,
      });
      const check = checkTupleBinding(tuple, p);
      if (!check.ok) return rejected(state, check.reason!);
      const findings = p.findings.map((f) => (f.id === event.findingId ? { ...f, severity: event.severity } : f));
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "DECISION_CLASS_LOWERED": {
      // design §3.4: only the owner may lower a decision's class.
      if (event.by !== "owner") {
        return rejected(state, `only the owner may lower a decision's class, not '${String(event.by)}'`);
      }
      const decision = p.decisions.find((d) => d.id === event.decisionId);
      if (!decision) return rejected(state, `unknown decision ${event.decisionId}`);
      const tuple = bindingTupleForRecord(state, event.decisionId, {
        candidateSha: event.boundCandidateSha,
        contractVersion: event.boundContractVersion,
        recordVersion: event.boundRecordVersion,
      });
      const check = checkTupleBinding(tuple, p);
      if (!check.ok) return rejected(state, check.reason!);
      const decisions = p.decisions.map((d) =>
        d.id === event.decisionId ? { ...d, class: event.class, version: d.version + 1 } : d,
      );
      return ok({ ...state, phase: { ...p, decisions } });
    }

    case "OWNER_REQUEST_OPENED": {
      return ok({ ...state, phase: { ...p, ownerRequests: [...p.ownerRequests, event.request] } });
    }

    case "OWNER_REQUEST_RESOLVED": {
      const check = checkOwnerRequestResolved(p, event);
      if (!check.ok) return rejected(state, check.reason!);
      return ok({ ...state, phase: applyOwnerRequestResolved(p, event) });
    }

    case "INTEGRITY_VIOLATED": {
      // design §2.2: a checkout that no longer matches its candidate commit
      // invalidates that gate's result (never counted as passed) and marks
      // the run integrity-violated for the owner. Idempotent: once set, a
      // second occurrence (e.g. a repair round's own check) is still just
      // `true`.
      return ok({ ...state, phase: { ...p, integrityViolated: true } });
    }

    case "SUBMIT_PHASE": {
      // The raw disclosures are stashed on the phase; the submit-phase row
      // itself moves IMPLEMENTING -> FREEZING. FREEZE_COMPLETED is what
      // assembles these into bound Decision records (design §6.2, §7.1).
      return undefined;
    }

    case "CRITERION_REVERTED": {
      // Plan 01g: the owner's correction naming an amendment id restores the
      // criterion's original wording. A phase with a candidate has its own
      // transition rows (transitions.ts) that also invalidate the evidence
      // bound to the replaced contract version and return to CHECKING. This
      // handler is the fallback for a phase with no candidate yet
      // (IMPLEMENTING/FREEZING/REPAIRING), where there is no evidence to
      // invalidate: it only restores the wording so the next freeze binds to
      // it. Both paths reject an unknown, not-yet-applied or already-reverted
      // amendment, and the row guard and this handler agree on validity so
      // next() can never emit an event the reducer refuses.
      const decision = p.decisions.find((d) => d.amendment?.id === event.amendmentId);
      if (!decision || !decision.amendment) {
        return rejected(state, `unknown amendment ${event.amendmentId}`);
      }
      if (decision.amendment.status === "reverted") {
        return rejected(state, `amendment ${event.amendmentId} is already reverted`);
      }
      if (decision.amendment.status !== "applied") {
        return rejected(state, `amendment ${event.amendmentId} has not been applied, so there is no wording to restore`);
      }
      if (!Array.isArray(event.newAcceptance) || event.newAcceptance.length === 0 || event.newAcceptance.some((a) => typeof a !== "string" || a.length === 0)) {
        return rejected(state, `reverting amendment ${event.amendmentId} needs the restored acceptance list`);
      }
      if (!p.contract.acceptance.includes(decision.amendment.proposedWording)) {
        return rejected(state, `amendment ${event.amendmentId}'s wording is not in the current contract, so there is nothing to restore`);
      }
      const decisions = p.decisions.map((d) =>
        d.id === decision.id
          ? {
              ...d,
              version: d.version + 1,
              boundContractVersion: event.newContractVersion,
              amendment: { ...d.amendment!, status: "reverted" as const, revertedAt: new Date().toISOString() },
            }
          : d,
      );
      return ok({
        ...state,
        phase: {
          ...p,
          contract: { ...p.contract, acceptance: event.newAcceptance, contractVersion: event.newContractVersion },
          candidate: p.candidate && { sha: p.candidate.sha, contractVersion: event.newContractVersion },
          decisions,
        },
      });
    }

    case "NOTE_ADDED": {
      // §7.4 `note` (conductor state): queued for the next worker attempt's
      // prompt. Record-only — it moves no phase.
      if (event.phaseId !== p.phaseId) {
        return rejected(state, `note is for phase ${event.phaseId}, but this run is on phase ${p.phaseId}`);
      }
      if (!event.text || event.text.trim().length === 0) {
        return rejected(state, "a note must carry non-empty text");
      }
      return ok({ ...state, phase: { ...p, ownerNotes: [...(p.ownerNotes ?? []), event.text] } });
    }

    case "DIRECTIVE_ADDED": {
      // Plan 01i: a numbered owner directive. Record-only (moves no phase
      // state name) but binding: every later prompt quotes it verbatim
      // until it is withdrawn. The conductor assigns the id/seq and the
      // delivery targets; reduce() only stores the record, exactly like
      // NOTE_ADDED's queue.
      const directive = event.directive;
      if (!directive || typeof directive.id !== "string" || directive.id.length === 0) {
        return rejected(state, "an owner directive must carry its id (OD-n)");
      }
      if (typeof directive.text !== "string" || directive.text.trim().length === 0) {
        return rejected(state, "an owner directive must carry the text the owner sent");
      }
      if (directive.scope !== "phase" && directive.scope !== "program") {
        return rejected(state, `owner directive ${directive.id} must have scope phase or program`);
      }
      const existing = p.ownerDirectives ?? [];
      if (existing.some((d) => d.id === directive.id)) {
        return rejected(state, `owner directive ${directive.id} already exists`);
      }
      const added = { ...directive, targets: directive.targets ?? [], deliveries: directive.deliveries ?? {} };
      // Kept in ascending id order, never in the order the inbox happened to
      // hand the files over: a lexicographic scan applies `cmd-prog-ODP-10`
      // before `cmd-prog-ODP-2`, and prompts and the status must still read
      // oldest → newest. `seq` is the number in the id (`OD-n` or `ODP-n`).
      const ownerDirectives = [...existing, added].sort((a, b) => a.seq - b.seq);
      return ok({ ...state, phase: { ...p, ownerDirectives } });
    }

    case "DIRECTIVE_WITHDRAWN": {
      // Plan 01i: `withdraw OD-n`. Record-only: the record stays and the
      // status shows it withdrawn; every later prompt omits it.
      const directive = (p.ownerDirectives ?? []).find((d) => d.id === event.directiveId);
      if (!directive) return rejected(state, `unknown owner directive ${event.directiveId}`);
      if (directive.status === "withdrawn") {
        return rejected(state, `owner directive ${event.directiveId} is already withdrawn`);
      }
      const ownerDirectives = (p.ownerDirectives ?? []).map((d) =>
        d.id === event.directiveId ? { ...d, status: "withdrawn" as const, withdrawnAt: event.at ?? new Date().toISOString() } : d,
      );
      return ok({ ...state, phase: { ...p, ownerDirectives } });
    }

    case "DIRECTIVE_DELIVERED": {
      // Plan 01i: the observed outcome of the immediate steer to one live
      // agent, keyed by target. Record-only; a later record for the same
      // target (a re-dispatched agent) is the truer one.
      const directive = (p.ownerDirectives ?? []).find((d) => d.id === event.directiveId);
      if (!directive) return rejected(state, `unknown owner directive ${event.directiveId}`);
      if (event.state !== "delivered" && event.state !== "delivery-uncertain") {
        return rejected(state, `owner directive ${event.directiveId} delivery must be delivered or delivery-uncertain`);
      }
      const ownerDirectives = (p.ownerDirectives ?? []).map((d) =>
        d.id === event.directiveId ? { ...d, deliveries: { ...d.deliveries, [event.target]: event.state } } : d,
      );
      return ok({ ...state, phase: { ...p, ownerDirectives } });
    }

    case "OWNER_INPUT_RECORDED": {
      // Plan 2d (§7.4/§9.3): the recorded effect of one input the owner
      // sent. Record-only (moves no phase state); keyed by the inbox
      // command id so a later, more definitive record (e.g. a recovered
      // steer's `delivered` after its `delivery-uncertain`) updates rather
      // than duplicates. Newest wins — the conductor only ever writes this
      // after observing the effect, so the later record is the truer one.
      const input = event.input;
      if (!input || typeof input.id !== "string" || input.id.length === 0) {
        return rejected(state, "an owner-input record must name its inbox command id");
      }
      if (typeof input.text !== "string" || input.text.trim().length === 0) {
        return rejected(state, "an owner-input record must carry the text the owner sent");
      }
      const existing = p.ownerInputs ?? [];
      const ownerInputs = existing.some((i) => i.id === input.id)
        ? existing.map((i) => (i.id === input.id ? { ...i, ...input } : i))
        : [...existing, input];
      return ok({ ...state, phase: { ...p, ownerInputs } });
    }

    case "OWNER_REQUEST_MARKED_UNNEEDED": {
      // §7.4 `unneeded` / §11.4's "unnecessary escalations" pilot metric:
      // recorded, not resolved (nothing was answered). Record-only, and the
      // request deliberately STAYS `open`: closing it would (a) reject a
      // follow-up OWNER_REQUEST_RESOLVED as "already unneeded" and (b) for
      // the record-less fallback request (no candidate/decision/finding)
      // leave AWAITING_OWNER with no open request and no command that can
      // unstick it — the blocking finding on this candidate. The metric
      // lives in `unneededRequestIds`, so the owner can still grant/stop/
      // repair the request afterwards.
      const request = p.ownerRequests.find((r) => r.id === event.requestId);
      if (!request) return rejected(state, `unknown owner request ${event.requestId}`);
      if (request.status !== "open") {
        return rejected(state, `owner request ${event.requestId} is already ${request.status}, not open`);
      }
      const already = p.unneededRequestIds ?? [];
      if (already.includes(event.requestId)) {
        return rejected(state, `owner request ${event.requestId} is already marked unneeded`);
      }
      return ok({ ...state, phase: { ...p, unneededRequestIds: [...already, event.requestId] } });
    }

    case "NOTES_DELIVERED": {
      // §7.4 `note`: the conductor delivered the first `count` queued notes
      // in a worker attempt's prompt; later attempts send only the rest, so
      // a note reaches the NEXT attempt and does not keep steering every
      // one (finding F-p2b-...-B-2). Record-only.
      if (event.phaseId !== p.phaseId) {
        return rejected(state, `notes are for phase ${event.phaseId}, but this run is on phase ${p.phaseId}`);
      }
      if (!Number.isInteger(event.count) || event.count <= 0) {
        return rejected(state, "a notes-delivered event must carry a positive integer count");
      }
      return ok({ ...state, phase: { ...p, deliveredNoteCount: (p.deliveredNoteCount ?? 0) + event.count } });
    }

    case "DECISION_MATCHED": {
      // Plan 2c (design §3.3): a reviewer's own discovery is the same choice
      // as another listed record. The discovery stops being a separate,
      // votable record; the reviewer is recorded on the matched record.
      const discovery = p.decisions.find((d) => d.id === event.decisionId);
      const target = p.decisions.find((d) => d.id === event.sameAs);
      if (!discovery) return rejected(state, `unknown decision ${event.decisionId}`);
      if (!target) return rejected(state, `unknown decision ${event.sameAs}`);
      if (discovery.id === target.id) return rejected(state, "a decision cannot match itself");
      if (discovery.source !== "reviewer-discovered") {
        return rejected(state, `only a reviewer-discovered record can be matched, not ${discovery.source} ${discovery.id}`);
      }
      if (!isLiveDecision(discovery) || !isLiveDecision(target)) {
        return rejected(state, `cannot match superseded records (${discovery.id} → ${target.id})`);
      }
      const decisions = p.decisions.map((d) => {
        if (d.id === discovery.id) return { ...d, supersededBy: `same as ${target.id}`, version: d.version + 1 };
        if (d.id === target.id) {
          const seen = d.alsoSeenBy ?? [];
          return seen.includes(event.reviewer) ? d : { ...d, alsoSeenBy: [...seen, event.reviewer] };
        }
        return d;
      });
      return ok({ ...state, phase: { ...p, decisions } });
    }

    case "FINDING_ALSO_RAISED": {
      // Plan 2c: "same as F-…" — the reviewer agrees with an open finding
      // instead of filing a duplicate. Record-only.
      const finding = p.findings.find((f) => f.id === event.findingId);
      if (!finding) return rejected(state, `unknown finding ${event.findingId}`);
      if (finding.status !== "open") return rejected(state, `finding ${finding.id} is ${finding.status}, not open`);
      const also = finding.alsoRaisedBy ?? [];
      if (finding.raisedBy === event.reviewer || also.includes(event.reviewer)) return ok(state);
      const findings = p.findings.map((f) => (f.id === finding.id ? { ...f, alsoRaisedBy: [...also, event.reviewer] } : f));
      return ok({ ...state, phase: { ...p, findings } });
    }

    case "MISS_RECORDED": {
      // §3.5/§10.4 `s`: the observed miss sample. Record-only.
      if (!event.recordId || event.recordId.trim().length === 0) {
        return rejected(state, "a miss must name the sampled record it refers to");
      }
      const misses = p.misses ?? [];
      if (misses.includes(event.recordId)) {
        return rejected(state, `record ${event.recordId} is already marked as a miss`);
      }
      return ok({ ...state, phase: { ...p, misses: [...misses, event.recordId] } });
    }

    default:
      return undefined;
  }
}

export function reduce(state: State, event: unknown): ReduceResult {
  try {
    if (!event || typeof event !== "object" || typeof (event as { type?: unknown }).type !== "string") {
      return rejected(state, "malformed event: missing string 'type'");
    }
    const ev = event as Event;
    if (!KNOWN_EVENT_TYPES.has(ev.type)) {
      return rejected(state, `unknown event type: ${String((event as { type: unknown }).type)}`);
    }

    // REVISE and AMEND get a dedicated, human-readable staleness check up
    // front (design §7.1) rather than a boolean guard failing silently into
    // the generic "no rule matched" message — see the stale-binding tests.
    if (ev.type === "REVISE") {
      const current = currentVersionsFor(state.phase, ev.targetRecordId);
      if (!current) {
        return rejected(state, `revise target ${ev.targetRecordId} does not exist in phase ${state.phase.phaseId}`);
      }
      const check = checkBinding(
        { candidateSha: ev.boundCandidateSha, contractVersion: ev.boundContractVersion, recordVersion: ev.boundRecordVersion },
        current,
        current.label,
      );
      if (!check.ok) return rejected(state, check.reason!);
    }
    if (ev.type === "AMEND") {
      if (!sameVersion(ev.replacingContractVersion, state.phase.contract.contractVersion)) {
        return rejected(
          state,
          `phase ${state.phase.phaseId}'s contract changed v${ev.replacingContractVersion.snapshot} → v${state.phase.contract.contractVersion.snapshot} since you viewed it`,
        );
      }
      // design §4.2/§4.3: an amend may resolve `contract` findings directly
      // — but only ones that exist, are the right kind, and are still open.
      for (const findingId of ev.resolvesFindingIds ?? []) {
        const finding = state.phase.findings.find((f) => f.id === findingId);
        if (!finding) return rejected(state, `amend cannot resolve unknown finding ${findingId}`);
        if (finding.kind !== "contract") {
          return rejected(state, `amend can only resolve 'contract' findings; ${findingId} is '${finding.kind}'`);
        }
        if (finding.status !== "open") {
          return rejected(state, `finding ${findingId} is already ${finding.status}, not open`);
        }
      }
    }

    // A review that states the same correction's disposition twice is
    // malformed at ingestion (F02): reject it before it can become state,
    // so `addressed` never has to choose between contradictory statements.
    if (ev.type === "REVIEW_SUBMITTED") {
      const issue = reviewIngestionIssue(ev.review);
      if (issue) return rejected(state, issue);
    }

    // The raw disclosures alongside SUBMIT_PHASE are stashed before the
    // phase-table row (which only moves phase -> FREEZING) runs. They stay
    // pending until FREEZE_COMPLETED assembles and binds them.
    let working = state;
    if (ev.type === "SUBMIT_PHASE") {
      working = {
        ...state,
        phase: { ...state.phase, pendingDisclosures: ev.disclosures, pendingPrior: ev.prior, pendingDispute: ev.dispute },
      };
    }

    const rows = rowsFor(working, ev.type);
    if (rows.length === 0) {
      const recordResult = applyRecordEvent(working, ev);
      if (recordResult) return recordResult;
      return rejected(
        state,
        `out of order or unknown transition: '${ev.type}' has no rule from phase=${working.phase.phase} run=${working.run}`,
      );
    }

    const matching = rows.find((r) => {
      try {
        return r.guard(working, ev);
      } catch {
        return false;
      }
    });

    if (!matching) {
      const recordResult = applyRecordEvent(working, ev);
      if (recordResult) return recordResult;
      return rejected(
        state,
        `event '${ev.type}' arrived in phase=${working.phase.phase} but no guard accepted it (tried: ${rows
          .map((r) => r.guardName)
          .join(", ")})`,
      );
    }

    const nextState = matching.apply(working, ev);
    return ok(nextState);
  } catch (err) {
    return rejected(state, `reduce threw and was recovered: ${String((err as Error)?.message ?? err)}`);
  }
}
