// design §7.1: everything is bound to what it evaluated —
//   run id · phase id · candidate sha · contract version · record id · record version
//
// A command or ballot whose binding no longer matches is rejected visibly,
// with a human-readable reason naming what changed, e.g. "decision D-7
// changed v2 → v3 since you viewed it" (design §7.1's own example).

import { findOwnerRequest } from "./triage.ts";
import type { Ballot, BindingTuple, ContractVersion, PhaseState } from "./types.ts";

export interface BindingCheckResult {
  ok: boolean;
  reason?: string;
}

// ---------------------------------------------------------------------------
// Plan 06d (A2): naming a run
// ---------------------------------------------------------------------------
//
// A binding may name the run it was written for by any of the three ids the
// owner can see: the internal run id the log records (`phase.runId`), the run
// directory id (`tt list` shows it), or the readable id a program node was
// given (`<program>-NN`). `resolveBinding` is the ONE place that decides
// whether a binding names this run, so the conductor, the CLI and Emacs never
// each invent their own match. A binding that names nothing is refused with
// the ids it could have used, never silently dropped.

export interface RunBinding {
  /** The internal run id (`phase.runId`, a separate uuid in the init record). */
  runId: string;
  /** The run directory's own id (its basename). */
  dirId: string;
  /** The readable id (`<program>-NN`) when a program scheduler started the
   * run; absent for a hand-started run. */
  readableId?: string;
}

export type BindingResolution = { ok: true; runId: string } | { ok: false; reason: string };

/** The three ids a binding could have used for RUN, in the order the reason
 * names them. */
export function runBindingIds(run: RunBinding): string[] {
  return [run.runId, run.dirId, ...(run.readableId ? [run.readableId] : [])].filter((id) => id.length > 0);
}

/** Resolve a binding's run reference (a plain id, or the `binding` object a
 * decision-view command carries) against RUN. An absent/empty reference means
 * "this run" and resolves to its internal id. An unknown id is refused with
 * every id that would have bound, so the owner can retry with one of them. */
export function resolveBinding(run: RunBinding, binding: string | { runId?: string } | undefined): BindingResolution {
  const wanted = typeof binding === "string" ? binding : binding?.runId;
  if (wanted === undefined || wanted === "") return { ok: true, runId: run.runId };
  if (runBindingIds(run).includes(wanted)) return { ok: true, runId: run.runId };
  return {
    ok: false,
    reason: `no run matches ${wanted}; this run can be named by ${runBindingIds(run).join(", ")}`,
  };
}

function sameVersion(a: ContractVersion, b: ContractVersion): boolean {
  return a.snapshot === b.snapshot && a.sectionSha256 === b.sectionSha256;
}

/** Generic staleness check: `bound` is what the sender saw; `current` is
 * what the record now is. `recordLabel` is how the record should read in
 * the reason, e.g. "decision D-7". */
export function checkBinding(
  bound: { candidateSha: string; contractVersion: ContractVersion; recordVersion: number },
  current: { candidateSha: string; contractVersion: ContractVersion; recordVersion: number },
  recordLabel: string,
): BindingCheckResult {
  if (bound.candidateSha !== current.candidateSha) {
    return {
      ok: false,
      reason: `${recordLabel} is bound to candidate ${bound.candidateSha}, but the phase is now at candidate ${current.candidateSha} — it changed since you viewed it`,
    };
  }
  if (!sameVersion(bound.contractVersion, current.contractVersion)) {
    return {
      ok: false,
      reason: `${recordLabel} is bound to contract v${bound.contractVersion.snapshot}, but the phase is now at contract v${current.contractVersion.snapshot} — it changed since you viewed it`,
    };
  }
  if (bound.recordVersion !== current.recordVersion) {
    return {
      ok: false,
      reason: `${recordLabel} changed v${bound.recordVersion} → v${current.recordVersion} since you viewed it`,
    };
  }
  return { ok: true };
}

/** A ballot's binding against the phase's current candidate, contract AND
 * the decision's current record version (design §7.1: a ballot is bound to
 * the full tuple, not just the candidate). */
export function checkBallotBinding(ballot: Ballot, phase: PhaseState): BindingCheckResult {
  if (!phase.candidate) {
    return { ok: false, reason: `phase ${phase.phaseId} has no frozen candidate yet` };
  }
  const decision = phase.decisions.find((d) => d.id === ballot.decisionId);
  if (!decision) {
    return { ok: false, reason: `decision ${ballot.decisionId} does not exist in phase ${phase.phaseId}` };
  }
  return checkBinding(
    {
      candidateSha: ballot.boundCandidateSha,
      contractVersion: ballot.boundContractVersion,
      recordVersion: ballot.boundRecordVersion,
    },
    { candidateSha: phase.candidate.sha, contractVersion: phase.contract.contractVersion, recordVersion: decision.version },
    `decision ${decision.id}`,
  );
}

/** Look up the current (candidateSha, contractVersion, recordVersion) a
 * binding tuple should be checked against, for the record kind it names.
 * The candidate and contract are always the *phase's* current ones — a
 * decision or finding does not carry its own frozen candidate once the
 * phase has moved on to a later one; only its own recordVersion is
 * record-specific. */
export function currentVersionsFor(
  phase: PhaseState,
  recordId: string,
): { candidateSha: string; contractVersion: ContractVersion; recordVersion: number; label: string } | undefined {
  const candidateSha = phase.candidate?.sha ?? "";
  const contractVersion = phase.contract.contractVersion;

  const decision = phase.decisions.find((d) => d.id === recordId);
  if (decision) {
    return { candidateSha, contractVersion, recordVersion: decision.version, label: `decision ${decision.id}` };
  }
  const finding = phase.findings.find((f) => f.id === recordId);
  if (finding) {
    return { candidateSha, contractVersion, recordVersion: finding.version, label: `finding ${finding.id}` };
  }
  const request = findOwnerRequest(phase, recordId);
  if (request) {
    return { candidateSha, contractVersion, recordVersion: request.version, label: `owner request ${request.id}` };
  }
  return undefined;
}

/** Check a binding tuple (as carried by a ballot, review, finding
 * disposition or owner command, design §7.1) against the phase's current
 * state, by looking up the record it names. */
export function checkTupleBinding(tuple: BindingTuple, phase: PhaseState): BindingCheckResult {
  const current = currentVersionsFor(phase, tuple.recordId);
  if (!current) {
    return { ok: false, reason: `record ${tuple.recordId} no longer exists in phase ${tuple.phaseId}` };
  }
  return checkBinding(
    { candidateSha: tuple.candidateSha, contractVersion: tuple.contractVersion, recordVersion: tuple.recordVersion },
    current,
    current.label,
  );
}
