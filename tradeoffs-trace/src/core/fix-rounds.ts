// 07g1 (design A3): the fix-round task rule. A fix round takes POINTS only —
// an open finding triaged `fix`, with the domain of the files it cites — and
// groups them by domain, one fixer per domain. A decision the seats vetoed or
// the evaluator triaged is never fixer work by itself: it goes to the seats'
// vote or the owner. An owner repair ruling creates a finding before MAP. This module is the ONE place that decides those tasks; the
// conductor only runs what it returns. It is pure: no clock, no I/O, no model.

import { domainOf, whereKey, type Domain } from "./domains.ts";
import { testNames } from "./test-names.ts";
import type { Disposition } from "./triage.ts";
import type { Finding, FindingDisclosure, FindingFact, OwnerRequest, PhaseState, Review, RoundRecord } from "./types.ts";
import type { DomainLoopOutcome } from "../fix-loop.ts";

/** Approval evidence is a projection of the merged checkpoints, never a new
 * phase flag. Follow the candidate's recorded bases so an unmerged or abandoned
 * domain head cannot settle anything on a different candidate. */
export interface DomainEvidenceState {
  candidate?: { sha: string };
  rounds?: readonly RoundRecord[];
  findings?: readonly Finding[];
  doneWhenFacts?: readonly FindingFact[];
}

export function domainResolvedPoints(phase: DomainEvidenceState, candidateSha = phase.candidate?.sha): Map<string, string> {
  const out = new Map<string, string>();
  let head = candidateSha;
  for (const round of [...(phase.rounds ?? [])].reverse()) {
    const fixed = round.fixed;
    if (round.lanes.length > 0 || !head || !fixed?.candidate || fixed.candidate !== head) continue;
    for (const task of fixed.tasks) {
      if (!task.sha || !fixed.merges.some((m) => m.sha === task.sha)) continue;
      const pass = fixed.domains?.findLast((p) => p.sha === task.sha && p.review);
      if (!pass?.facts?.checkPassed || !pass.review?.quality.ok || (pass.facts.testRemovals?.length ?? 0) > 0 ||
        !task.findingIds.every((id) => pass.facts!.points.some((p) => p.id === id && p.onHead) &&
          pass.review!.points.some((p) => p.pointId === id && p.fixed))) continue;
      for (const id of task.findingIds) {
        const fact = phase.doneWhenFacts?.find((f) => f.findingId === id && f.candidateSha === candidateSha);
        if (fact && (fact.timedOut || fact.exitCode !== 0)) continue;
        const verdict = pass.review.points.find((p) => p.pointId === id)!;
        const pointFact = pass.facts.points.find((p) => p.id === id)!;
        if (!out.has(id)) out.set(id, `domain ${task.domain} approved point ${id} at ${task.sha} (round ${round.round}, pass ${pass.iteration}); ` +
          `green fix checks; quality ok; ${pointFact.doneWhen ?? "review-only point"}: holds${pointFact.outputTail ? ` — ${pointFact.outputTail}` : ""}; ` +
          `${verdict.why}${verdict.anchors?.length ? ` (${verdict.anchors.join(", ")})` : ""}`);
      }
    }
    head = fixed.base;
  }
  return out;
}

/** An untouched item still belongs to the seats. Every earlier open point
 * on a covered item must be approved. A new ACCEPT finding on this candidate
 * blocks in its own right; it cannot make the other seats re-grade the item. */
export function domainMetItems(phase: DomainEvidenceState): Map<string, string[]> {
  const resolved = domainResolvedPoints(phase);
  const out = new Map<string, string[]>();
  for (const finding of phase.findings ?? []) {
    const item = finding.item ?? ("itemId" in finding && typeof finding.itemId === "string" ? finding.itemId : undefined);
    const evidence = resolved.get(finding.id);
    if (item && evidence) out.set(item, [...(out.get(item) ?? []), evidence]);
  }
  for (const finding of phase.findings ?? []) {
    const item = finding.item ?? ("itemId" in finding && typeof finding.itemId === "string" ? finding.itemId : undefined);
    if (item && finding.status === "open" && finding.severity === "blocking" && !resolved.has(finding.id) &&
      finding.boundCandidateSha !== phase.candidate?.sha) out.delete(item);
  }
  return out;
}

export function acceptanceDomainLines(phase: DomainEvidenceState): string[] {
  const resolved = domainResolvedPoints(phase);
  if (resolved.size === 0) return [];
  return ["Acceptance review: the integrated domains, their points and verdicts:",
    ...[...resolved].map(([id, evidence]) => `- ${id}: ${evidence}`),
    `Domain-resolved items: ${[...domainMetItems(phase).keys()].join(", ") || "(none)"}.`,
    "Assume every point resolved by an approved domain is resolved, with the approving review and facts above as evidence. Do not re-grade those points or the items they close; no further item vote is needed for them.",
    "Find NEW issues only: integration defects across domains, regressions, scope and conceptual-integrity violations, and items no domain touched (submit the usual item verdicts for those).",
    "Reopen a resolved point only as a NEW finding citing a concrete defect: path:line and its failing condition, or a failing check. Re-grading the same point (such as 'still partial') is advisory and does not block acceptance."];
}

/** A negative item ballot is not a new defect. Preserve that observation as
 * an ordinary advisory unless the seat also filed a concrete new finding. */
export function acceptanceReviewAdvisories(phase: DomainEvidenceState, review: Review): FindingDisclosure[] {
  const met = domainMetItems(phase);
  return [...(review.items ?? []), ...(review.arch ?? [])].filter((v) => met.has(v.id) &&
    v.verdict !== "met" && v.verdict !== "fits" &&
    ![...(review.findings ?? []), ...(review.blockers ?? [])].some((f) =>
      f.item === v.id && concreteDomainFinding(f)))
    .map((v) => ({ kind: "defect", severity: "advisory",
      evidence: `${v.id}: ${review.reviewer} marked ${v.verdict}: ${v.evidence ?? ""}. Domain approval settles this item; no concrete new finding accompanied this re-grade.` }));
}

/** Require a location and a stated failing condition, rather than a grade
 * repeated in the findings array. Check names can supply the location for
 * a recorded regression through the ordinary acceptance gate. */
export function concreteDomainFinding(finding: { evidence: string; where?: string; item?: string }): boolean {
  const text = `${finding.where ?? ""}\n${finding.evidence}`;
  if (!/\b[\w./-]+:\d+(?::\d+)?\b/.test(text)) return false;
  const condition = finding.evidence.replace(/\b[\w./-]+:\d+(?::\d+)?\b/g, "")
    .replaceAll(finding.item ?? "\0", "").replace(/\b(still|partial|unmet|met|not|resolved|fixed|incomplete|unclear|deviates)\b/gi, "")
    .replace(/[^A-Za-z0-9]/g, "");
  return condition.length > 0;
}

/** A domain's exhausted local judgement is an owner decision now, even if
 * the phase has spare rounds. Both options use the existing repair route;
 * rewrite supplies explicit point fields in the owner's resolution note. */
export function domainFailureRequest(phase: Pick<PhaseState, "phaseId" | "candidate" | "contract">, task: FixTask,
  round: number, outcome: DomainLoopOutcome): OwnerRequest | undefined {
  if (outcome.status !== "dropped" && outcome.status !== "open") return undefined;
  const last = outcome.records.findLast((r) => r.review);
  const facts = outcome.records.findLast((r) => r.facts)?.facts;
  return {
    id: `OR-${phase.phaseId}-domain-${round}-${task.domain}`, version: 1, phaseId: phase.phaseId,
    reason: [`domain ${task.domain} was not fixed in ${outcome.reviews} loops: ${outcome.reason ?? outcome.status}`,
      `Points: ${task.findingIds.join(", ")}`,
      `Last review: ${JSON.stringify(last?.review ?? null)}`, `Facts: ${JSON.stringify(facts ?? null)}`].join("\n"),
    origin: "open_finding", linkedFindingId: task.findingIds[0],
    ...(phase.candidate ? { boundCandidateSha: phase.candidate.sha } : {}), boundContractVersion: phase.contract.contractVersion,
    options: [{ id: "repair", label: "repair the domain (grant 3 rounds)" },
      { id: "rewrite_plan_point", label: "rewrite the plan point (supply domain, item, where, expected and doneWhen; grant 3 rounds)" }],
    status: "open",
  };
}

/** Plan 07c (D4): the default `#+TT_FIXERS` — the most fixer tasks (one per
 * domain) a fix round may run. */
export const DEFAULT_FIXERS = 4;

/** 07g2 (A1): the default `#+TT_INTEGRATE_MAX` — how many attempts the
 * integrate duty gets on one conflicted merge before the last domain is
 * dropped to the next loop. */
export const DEFAULT_INTEGRATE_MAX = 2;

/** A finding as `fixTasks` reads it: the frozen finding plus its triage
 * disposition, when the evaluator has recorded one. A finding with no
 * disposition is not triaged `fix`, so it is in no task. */
export interface FixFinding extends Finding {
  source?: "finding";
  disposition?: Disposition;
}

/** One point `fixTasks` reads. Plan 07g1 (A3): a point is a finding; a
 * decision is never fixer work. */
export type FixPoint = FixFinding;

/** One fixer task: one domain and the points it owns, in point order.
 * `findingIds` names every point; it is what the fixer's brief names and what
 * `ROUND_FIXED` records. */
export interface FixTask {
  domain: string;
  findingIds: string[];
  findings: FixFinding[];
}

/** Plan 07c (D4.4): the fixer's duty, in the brief's fixed first position.
 * Exported so the conductor's prompt and its tests name exactly one text. */
export const FIXER_DUTY =
  "You are a fixer. You fix exactly the points below and nothing else: change only what they need. " +
  "An owner repair ruling reaches you as a point; carry out its expected outcome. " +
  "A voted-down or vetoed decision alone goes to the seats or the owner; you still decide no trade-off yourself. " +
  "If you meet a decision that changes scope, behaviour or cost, report it as a disclosure in submit_phase instead of deciding it. " +
  "When you are done, call submit_phase with your decisions, assumptions and deviations; you owe no item coverage and no answers to other findings.";

function nonEmpty(v: string | undefined): v is string {
  return typeof v === "string" && v.trim().length > 0;
}

/** The domain a point belongs to: its own `domain`, else the domain of the
 * file its `where` cites. Plan 07g1 (A3): every point with a file reaches a
 * domain, so nothing with a file falls back to a whole-round repair. */
export function pointDomain(point: FixFinding, domains: readonly Domain[]): string | undefined {
  if (nonEmpty(point.domain)) return point.domain;
  const where = point.where ?? "";
  if (!nonEmpty(where)) return undefined;
  return domainOf(whereKey(where).split(" ")[0], domains);
}

/** True when a point becomes fixer work: an OPEN finding triaged `fix` with a
 * domain (its own or the one its cited file belongs to). A decision-shaped
 * record is never fixer work, even when a caller hands it a domain, an open
 * status and a `fix` disposition: it goes to the seats' vote or the owner. */
function isFixWork(p: FixFinding, domains: readonly Domain[]): boolean {
  if ((p as { source?: string }).source === "decision") return false;
  return p.status === "open" && p.disposition?.kind === "fix" && pointDomain(p, domains) !== undefined;
}

/** Plan 07g1 (A3): the fixer tasks for one fix round. Every OPEN finding
 * whose triage disposition is `fix` is grouped by its domain, in the domains'
 * declaration order, in point order within a domain; at most `cap` domains
 * become tasks (the rest wait for a later round). A finding triaged
 * `tradeoff` or `escalate`, a closed finding and a point without a domain are
 * in no task. A decision is never passed here: the seats' vote or the owner
 * settles it. The only place the task rule lives. */
export function fixTasks(points: readonly FixPoint[], domains: readonly Domain[], cap: number): FixTask[] {
  const limit = Number.isFinite(cap) && cap > 0 ? Math.floor(cap) : 0;
  if (limit === 0 || domains.length === 0) return [];
  const tasks: FixTask[] = [];
  for (const domain of domains) {
    const group = points.filter((p) => isFixWork(p, domains) && pointDomain(p, domains) === domain.name);
    if (group.length === 0) continue;
    tasks.push({ domain: domain.name, findingIds: group.map((p) => p.id), findings: group });
    if (tasks.length >= limit) break;
  }
  return tasks;
}

/** No fixer ran in this MAP and REDUCE kept its base. A prior checkpoint
 * may be reused after a stop, but its old tasks are not a new repair attempt.
 * The caller parks for the lead before handing the same tree to the seats. */
export function repairProducedNoChange(round: RoundRecord, checkpoint?: RoundRecord["fixed"]): boolean {
  const dispatched = checkpoint === undefined && (round.fixed?.tasks.length ?? 0) > 0;
  return !dispatched && (round.fixed?.candidate ?? round.base) === round.base;
}

/** 07g1: test removals a mapper may make. An allowance names either a test
 * file (`test/a.test.ts`) or one test (`test/a.test.ts: the test name`). */
function testRemovalAllowed(entry: string, allowed: readonly string[]): boolean {
  const value = entry.trim();
  return allowed.some((a) => {
    const allow = a.trim();
    return allow === value || allow === value.split(/::|:/)[0].trim();
  });
}

/** 07g1: removed test files and test functions not named by
 * `#+TT_TEST_REMOVALS`. A deleted test file is always a violation unless the
 * owner allowed that exact file; a function removed from a file that still
 * exists is a violation unless allowed by file or by `file: name`. */
export function testRemovalViolations(files: readonly { path: string; base?: string; candidate?: string }[], allowed: readonly string[] = []): string[] {
  const out: string[] = [];
  const inCandidate = new Set<string>();
  for (const f of files) if (f.candidate !== undefined) for (const n of testNames(f.path, f.candidate)) inCandidate.add(`${f.path}::${n}`);
  for (const f of files) {
    if (f.base === undefined) continue;
    const baseTests = testNames(f.path, f.base);
    if (f.candidate === undefined) {
      const fileAllowed = allowed.some((a) => a.trim() === f.path || a.trim().startsWith(`${f.path}::`));
      if (baseTests.length > 0 && !fileAllowed) out.push(f.path);
      continue;
    }
    for (const n of baseTests) {
      const key = `${f.path}::${n}`;
      if (!inCandidate.has(key) && !testRemovalAllowed(key, allowed)) out.push(key);
    }
  }
  return out;
}

/** 07g1: the domain review's deterministic brief, built from the task and the
 * recorded loop history (never from live memory), so a stop/resume produces
 * the same text for the same log. The task's point details are the log's own
 * point records; the loop history carries every fact, verdict and quality
 * remark recorded before this review. */
export function domainReviewBrief(task: FixTask, records: readonly DomainLoopRecord[]): string {
  const pointLines: string[] = [];
  for (const id of task.findingIds) {
    const f = task.findings.find((x) => x.id === id);
    pointLines.push(`- ${id}${f?.item ? ` (item ${f.item})` : ""}: ${f?.where ?? "(no file named)"}`);
    if (f?.evidence) pointLines.push(`  why it is open: ${f.evidence}`);
    if (f?.expected) pointLines.push(`  expected (the plan/design it cites): ${f.expected}`);
    if (f?.doneWhen) pointLines.push(`  doneWhen: ${f.doneWhen}`);
    if (f?.from) pointLines.push(`  from: ${f.from.sha} (${f.from.paths.join(", ")})`);
  }
  const lines = [
    `You are the evaluator on its domain-review duty (conceptual integrity §2): judge one domain's fix against its points and for quality.`,
    `You are the domain reviewer for ${task.domain}. You are a fresh, stateless reviewer: judge the points below on the commit you are shown, not on a fixer's story.`,
    "You have read-only tools. Never edit, commit or run a mutating shell command; call submit_domain_review once.",
    "",
    `Points of domain ${task.domain}:`,
    ...pointLines,
  ];
  const history = [...records].sort((a, b) => a.iteration - b.iteration);
  if (history.length > 0) {
    lines.push("", "The loop so far (oldest first):");
    for (const r of history) {
      lines.push(`- pass ${r.iteration}, commit ${r.sha ? r.sha.slice(0, 9) : "(none)"}: ${r.note ?? "checked"}`);
      for (const p of r.facts?.points ?? []) {
        lines.push(
          `  - ${p.id}: ${p.onHead ? "passes" : "FAILS"} its doneWhen on the head${p.onBase ? " (also passes on the base: it proves nothing)" : ""}${p.outputTail ? ` — ${p.outputTail.split("\n").slice(-4).join(" / ")}` : ""}`,
        );
      }
      for (const v of r.review?.points ?? []) lines.push(`  - review: ${v.pointId} ${v.fixed ? "fixed" : "NOT fixed"}: ${v.why}`);
      if (r.review && !r.review.quality.ok) lines.push(`  - quality: ${(r.review.quality.problems ?? []).join("; ") || "not ok"}`);
      if (r.review?.notes && r.review.notes.length > 0) lines.push(`  - notes: ${r.review.notes.join("; ")}`);
    }
  }
  lines.push(
    "",
    "Call submit_domain_review with one entry for EVERY point id above: { pointId, fixed, why, anchors }. Set fixed=false for anything you cannot verify yourself on this commit; name the code anchors you read. Add quality { ok, problems } for anything outside the points that would make this domain's work unsafe: a quality problem that names a plan item id or a design line (§N, file.md:line) it breaks sends the work back to the same fixer, exactly as an unfixed point does; any other quality remark is recorded as a note on this domain's approval and costs no fixer round. Add notes for the next fixer.",
  );
  return lines.join("\n");
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
