// Plan 07c (design D4/D5): fix rounds. After a round whose pick the phase
// did not accept, the open findings triaged `fix` — and the decisions the
// seats voted down or vetoed — are grouped by their declared domain and one
// fixer runs per domain, in parallel, from the same base. This module is the
// ONE place that decides those tasks; the conductor only runs what it
// returns. It is pure: no clock, no I/O, no model.

import { domainOf, whereKey, type Domain } from "./domains.ts";
import type { Disposition } from "./triage.ts";
import type { Ballot, Decision, DecisionClass, Finding, FindingFact } from "./types.ts";

/** Plan 07c (D4): the default `#+TT_FIXERS` — the most fixer tasks (one per
 * domain) a fix round may run. */
export const DEFAULT_FIXERS = 4;

/** A finding as `fixTasks` reads it: the frozen finding plus its triage
 * disposition, when the evaluator has recorded one. A finding with no
 * disposition is not triaged `fix`, so it is in no task. */
export interface FixFinding extends Finding {
  source?: "finding";
  disposition?: Disposition;
}

/** Fix packet Z: a decision bound to the picked candidate whose vote failed
 * or that the leader seat vetoed, as a fixer point. The seats already chose:
 * the fixer applies their alternative (or removes the vetoed change) and
 * decides nothing itself. It has no doneWhen — the re-vote on the next
 * candidate closes it. Built only by `decisionFixPoint`. */
export interface DecisionFixPoint {
  source: "decision";
  id: string;
  class: DecisionClass;
  choice: string;
  /** `vetoed`: the leader seat rejected it; `failed`: too few seats approved. */
  outcome: "vetoed" | "failed";
  domain?: string;
  /** The plan item the decision is anchored to, when it names one. */
  item?: string;
  /** The dissent evidence path the domain was read from, when it was. */
  where?: string;
  /** One line per rejecting seat: `<seat>: <rationale> (evidence …)`. */
  reasons: string[];
  /** The alternative the seats recommend. */
  alternative: string;
  /** What the fixer must reach: the reasons plus the alternative. */
  expected: string;
}

/** One point `fixTasks` reads: a finding or a voted-down decision. */
export type FixPoint = FixFinding | DecisionFixPoint;

export function isDecisionPoint(p: FixPoint): p is DecisionFixPoint {
  return p.source === "decision";
}

/** One fixer task: one domain and the points it owns, in point order.
 * `findingIds` names every point (finding and decision ids alike); it is what
 * the fixer's brief names and what `ROUND_FIXED` records. */
export interface FixTask {
  domain: string;
  findingIds: string[];
  findings: FixFinding[];
  decisions: DecisionFixPoint[];
}

/** Plan 07c (D4.4): the fixer's duty, in the brief's fixed first position.
 * Exported so the conductor's prompt and its tests name exactly one text. */
export const FIXER_DUTY =
  "You are a fixer. You fix exactly the findings below and nothing else: change only what they need. " +
  "A decision point below is a choice the reviewer seats voted down or vetoed: apply the alternative the seats chose (or remove the vetoed change) — the seats decided it, you still decide no trade-off yourself. An owner (reserved) decision is never your task. " +
  "You decide no trade-off; if you meet a decision that changes scope, behaviour or cost, report it as a disclosure in submit_phase instead of deciding it. " +
  "When you are done, call submit_phase with your decisions, assumptions and deviations; you owe no item coverage and no answers to other findings.";

function nonEmpty(v: string | undefined): v is string {
  return typeof v === "string" && v.trim().length > 0;
}

/** True when a point becomes fixer work: an OPEN finding triaged `fix`, or a
 * decision point that is not a reserved (owner) decision; either with a
 * domain. */
function isFixWork(p: FixPoint): boolean {
  if (!nonEmpty(p.domain)) return false;
  if (isDecisionPoint(p)) return p.class !== "reserved";
  return p.status === "open" && p.disposition?.kind === "fix";
}

/** Plan 07c (D4.2): the fixer tasks for one fix round. Every OPEN finding
 * whose triage disposition is `fix` and every decision point (fix packet Z:
 * a non-reserved decision the seats voted down or vetoed), each with a
 * domain, is grouped by that domain, in the domains' declaration order, in
 * point order within a domain; at most `cap` domains become tasks (the rest
 * wait for a later round). A finding triaged `tradeoff` or `escalate`, a
 * closed finding, a reserved decision and a point without a domain are in no
 * task. The only place the task rule lives. */
export function fixTasks(points: readonly FixPoint[], domains: readonly Domain[], cap: number): FixTask[] {
  const limit = Number.isFinite(cap) && cap > 0 ? Math.floor(cap) : 0;
  if (limit === 0 || domains.length === 0) return [];
  const tasks: FixTask[] = [];
  for (const domain of domains) {
    const group = points.filter((p) => isFixWork(p) && p.domain === domain.name);
    if (group.length === 0) continue;
    tasks.push({
      domain: domain.name,
      findingIds: group.map((p) => p.id),
      findings: group.filter((p): p is FixFinding => !isDecisionPoint(p)),
      decisions: group.filter(isDecisionPoint),
    });
    if (tasks.length >= limit) break;
  }
  return tasks;
}

/** What `decisionFixPoint` reads besides the decision: facts the conductor
 * established (the vote outcome comes from the predicate, which this module
 * must not import). */
export interface DecisionPointFacts {
  /** Every ballot of the phase; only the decision's current ones are read. */
  ballots: readonly Ballot[];
  /** The seats in order, and the leader whose reject is a veto. */
  seats: readonly string[];
  leader: string;
  /** The decision is settled (passed its vote, overridden, ruled). */
  settled: boolean;
  /** The `:DOMAIN:` of the decision's plan item, when it names one. */
  itemDomain?: string;
  domains: readonly Domain[];
  /** The evaluator's `alternative` for this decision, when it gave one. */
  evaluatorAlternative?: string;
}

/** Fix packet Z: the fixer point for one decision, or undefined when it is
 * not fixer work. A point is a `delegated` decision (never a reserved/owner
 * one, an amendment, or a `detail`) that is live, was balloted on its
 * candidate and version, and did not settle — the leader vetoed it or too few
 * seats approved. Its domain is its plan item's `:DOMAIN:`, else the domain
 * of the first dissent evidence path (`where`/anchor) a declared domain owns;
 * without one it is still returned, domainless, and `fixTasks` leaves it out
 * (it stays an owner request, as before). */
export function decisionFixPoint(decision: Decision, facts: DecisionPointFacts): DecisionFixPoint | undefined {
  if (decision.class !== "delegated" || decision.amendment) return undefined;
  if (decision.supersededBy || decision.supersededByCorrection) return undefined;
  if (facts.settled) return undefined;
  const current = facts.ballots.filter(
    (b) => b.decisionId === decision.id && b.boundCandidateSha === decision.boundCandidateSha && b.boundRecordVersion === decision.version,
  );
  if (current.length === 0) return undefined;
  // The latest ballot per seat, leader first, then the other seats in order.
  const order = [facts.leader, ...facts.seats.filter((s) => s !== facts.leader)];
  for (const b of current) {
    if (!order.includes(b.reviewer)) order.push(b.reviewer);
  }
  const latest = order
    .map((seat) => [...current].reverse().find((b) => b.reviewer === seat))
    .filter((b): b is Ballot => b !== undefined);
  const dissent = latest.filter((b) => b.vote === "reject");
  const vetoed = dissent.some((b) => b.reviewer === facts.leader);
  const reasons = dissent.map((b) => `${b.reviewer}: ${b.rationale.trim()}${b.evidence.length > 0 ? ` (evidence: ${b.evidence.join(", ")})` : ""}`);
  let domain = nonEmpty(facts.itemDomain) ? facts.itemDomain : undefined;
  let where: string | undefined;
  if (!domain) {
    for (const b of dissent) {
      for (const e of b.evidence) {
        const p = whereKey(e).split(" ")[0];
        const d = domainOf(p, facts.domains);
        if (d) {
          domain = d;
          where = e;
          break;
        }
      }
      if (domain) break;
    }
  }
  const alternative = nonEmpty(facts.evaluatorAlternative)
    ? facts.evaluatorAlternative.trim()
    : decision.alternatives.length > 0
      ? decision.alternatives.map((a) => `${a.option} (${a.consequence})`).join("; ")
      : "remove the vetoed change";
  const outcome: DecisionFixPoint["outcome"] = vetoed ? "vetoed" : "failed";
  const expected = [
    `The seats did not approve "${decision.choice}" (${vetoed ? `vetoed by ${facts.leader}` : "its vote failed"}).`,
    reasons.length > 0 ? `Their reasons: ${reasons.join("; ")}.` : "No seat gave a reason.",
    `The alternative the seats recommend: ${alternative}. Apply it, or remove the vetoed change.`,
  ].join(" ");
  return {
    source: "decision",
    id: decision.id,
    class: decision.class,
    choice: decision.choice,
    outcome,
    ...(domain ? { domain } : {}),
    ...(decision.itemId ? { item: decision.itemId } : {}),
    ...(where ? { where } : {}),
    reasons,
    alternative,
    expected,
  };
}

/** Plan 07c (D4.4): the one-line index of the OTHER tasks the fixer's brief
 * carries — each other task's domain and point ids, never its points'
 * fields. */
export function otherTaskIndexLines(tasks: readonly FixTask[], own: FixTask): string[] {
  return tasks
    .filter((t) => t.domain !== own.domain)
    .map((t) => `- fix-${t.domain}: ${t.findingIds.join(", ")} (domain ${t.domain})`);
}

/** 07 (reduce by facts): true when `finding` is a domain point whose
 * doneWhen fact on `candidateSha` did not exit 0 (it failed or timed out): the
 * point is still open whatever an evaluator drop says (e.g. a drop as "already
 * tracked by E-n", a ledger entry that no fixer reads). False when there is no
 * candidate, no doneWhen, no domain, or no fact for this candidate. */
export function factKeepsOpen(finding: Finding, facts: readonly FindingFact[], candidateSha: string | undefined): boolean {
  if (!candidateSha || !nonEmpty(finding.doneWhen) || !nonEmpty(finding.domain)) return false;
  const fact = facts.find((f) => f.findingId === finding.id && f.candidateSha === candidateSha);
  return fact !== undefined && (fact.timedOut || fact.exitCode !== 0);
}
