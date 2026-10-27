// Plan 07c (design D4/D5): fix rounds. After a round whose pick the phase
// did not accept, the open findings triaged `fix` are grouped by their
// declared domain and one fixer runs per domain, in parallel, from the same
// base. This module is the ONE place that decides those tasks; the conductor
// only runs what it returns. It is pure: no clock, no I/O, no model.

import type { Domain } from "./domains.ts";
import type { Disposition } from "./triage.ts";
import type { Finding } from "./types.ts";

/** Plan 07c (D4): the default `#+TT_FIXERS` — the most fixer tasks (one per
 * domain) a fix round may run. */
export const DEFAULT_FIXERS = 4;

/** A finding as `fixTasks` reads it: the frozen finding plus its triage
 * disposition, when the evaluator has recorded one. A finding with no
 * disposition is not triaged `fix`, so it is in no task. */
export interface FixFinding extends Finding {
  disposition?: Disposition;
}

/** One fixer task: one domain and the open fix findings it owns, in finding
 * order. `findingIds` is what the fixer's brief names and what `ROUND_FIXED`
 * records. */
export interface FixTask {
  domain: string;
  findingIds: string[];
  findings: FixFinding[];
}

/** Plan 07c (D4.4): the fixer's duty, in the brief's fixed first position.
 * Exported so the conductor's prompt and its tests name exactly one text. */
export const FIXER_DUTY =
  "You are a fixer. You fix exactly the findings below and nothing else: change only what they need. " +
  "You decide no trade-off; if you meet a decision that changes scope, behaviour or cost, report it as a disclosure in submit_phase instead of deciding it. " +
  "When you are done, call submit_phase with your decisions, assumptions and deviations; you owe no item coverage and no answers to other findings.";

/** Plan 07c (D4.2): the fixer tasks for one fix round. Every OPEN finding
 * whose triage disposition is `fix` and that carries a domain is grouped by
 * that domain, in the domains' declaration order; at most `cap` domains
 * become tasks (the rest wait for a later round). A finding triaged
 * `tradeoff` or `escalate`, a closed finding, and a finding without a domain
 * are in no task. The only place the task rule lives. */
export function fixTasks(findings: readonly FixFinding[], domains: readonly Domain[], cap: number): FixTask[] {
  const limit = Number.isFinite(cap) && cap > 0 ? Math.floor(cap) : 0;
  if (limit === 0 || domains.length === 0) return [];
  const tasks: FixTask[] = [];
  for (const domain of domains) {
    const group = findings.filter(
      (f) =>
        f.status === "open" &&
        f.disposition?.kind === "fix" &&
        typeof f.domain === "string" &&
        f.domain.length > 0 &&
        f.domain === domain.name,
    );
    if (group.length === 0) continue;
    tasks.push({ domain: domain.name, findingIds: group.map((f) => f.id), findings: group });
    if (tasks.length >= limit) break;
  }
  return tasks;
}

/** Plan 07c (D4.4): the one-line index of the OTHER tasks the fixer's brief
 * carries — each other task's domain and finding ids, never its findings'
 * fields. */
export function otherTaskIndexLines(tasks: readonly FixTask[], own: FixTask): string[] {
  return tasks
    .filter((t) => t.domain !== own.domain)
    .map((t) => `- fix-${t.domain}: ${t.findingIds.join(", ")} (domain ${t.domain})`);
}
