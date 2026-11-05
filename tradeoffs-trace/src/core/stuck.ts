// Reconstruct reviewed rounds from the existing event/ledger history. No
// counter or flag can drift on restart; a recheck of one candidate counts once.
import { itemsFromPhase, phaseItemOutcomes, tallyItems } from "./items.ts";
import { sameVersion } from "./predicate.ts";
import { seatsOf } from "./seats.ts";
import type { Event, Message, PhaseState, Review } from "./types.ts";

type Reviewed = { sha: string; outcomes: Map<string, string>; duplicates: Map<string, string[]> };

export function repeatedItemStall(phase: PhaseState, events: readonly Event[]): string | undefined {
  const seats = seatsOf(phase.contract);
  const rounds = new Map<string, Reviewed>();
  const messages = new Map<string, Message>();
  let workerRound = 1;
  let sha: string | undefined;
  let reviews = new Map<string, Review>();
  let overturns: PhaseState["overturns"] = [];
  let duplicates = new Map<string, string[]>();
  const items = itemsFromPhase(phase.contract);
  const ids = [...items.architecture, ...items.requirements, ...items.constraints].map((i) => i.id);
  for (const ev of events) {
    if (ev.type === "REPAIR_ATTEMPT_STARTED") {
      workerRound += 1;
    } else if (ev.type === "ROUND_STARTED") {
      workerRound = Math.max(workerRound, ev.round);
    } else if (ev.type === "FREEZE_COMPLETED") {
      sha = ev.candidateSha;
      reviews = new Map();
      overturns = [];
      duplicates = new Map();
    } else if (ev.type === "REVIEW_SUBMITTED" && ev.review.candidateSha === sha &&
      sameVersion(ev.review.contractVersion, phase.contract.contractVersion)) {
      reviews.set(ev.review.reviewer, ev.review);
    } else if (ev.type === "ITEM_STATE_UPDATED" && ev.overturns) {
      overturns = ev.overturns;
    } else if (ev.type === "MESSAGE_RAISED") {
      messages.set(ev.message.id, ev.message);
    } else if (ev.type === "MESSAGE_DROPPED" && ev.boundCandidateSha === sha &&
      /duplicat|same (?:topic|finding|as)|already[- ]open/i.test(ev.reason)) {
      const m = messages.get(ev.messageId);
      if (!m) continue;
      const f = phase.findings.find((f) => f.id === m.sourceRecordId);
      const text = [m.title, m.summary, m.context, ...(m.evidence ?? []), f?.evidence ?? ""].join("\n");
      for (const id of ids) {
        if (f?.itemId !== id && !text.split(/[^\w-]+/).includes(id)) continue;
        // References are the evaluator's existing ledger ids, not inferred
        // semantic similarity between arbitrary prose.
        const refs = ev.reason.match(/\b(?:E|F|T)-[\w-]+/g) ?? [];
        for (const ref of refs) {
          const entry = phase.entries?.find((e) => e.id === ref);
          const prior = (phase.messages ?? []).filter((m) => m.id === ref || entry?.links.some((l) => l.messageId === m.id));
          const openFinding = phase.findings.some((f) => f.status === "open" && (f.id === ref || prior.some((m) => m.sourceRecordId === f.id)));
          if (!openFinding && !prior.some((m) => m.state === "published" || m.state === "refused")) continue;
          duplicates.set(id, [...new Set([...(duplicates.get(id) ?? []), ref])]);
        }
      }
    } else if (ev.type === "EVALUATION_COMPLETED" && sha && seats.every((s) => reviews.has(s))) {
      const outcomes = tallyItems(items, seats.map((seat) => ({ seat, items: reviews.get(seat) })), overturns);
      const verdicts = new Map(outcomes.map((o) => {
        const complete = seats.every((s) => (o.item.kind === "architecture" ? reviews.get(s)?.arch : reviews.get(s)?.items)?.some((i) => i.id === o.item.id));
        return [o.item.id, complete ? o.outcome : "missing"];
      }));
      rounds.set(`${workerRound}:${sha}`, { sha, outcomes: verdicts, duplicates: new Map(duplicates) });
    }
  }
  const last = [...rounds.values()].slice(-3);
  if (last.length !== 3 || last[2].sha !== phase.candidate?.sha) return undefined;
  const unmet = phaseItemOutcomes(phase).filter((o) => o.outcome !== "met" && o.outcome !== "fits");
  const repeated = unmet.filter(({ item }) => {
    const verdicts = last.map((r) => r.outcomes.get(item.id));
    if (verdicts.some((v) => !v || v === "met" || v === "fits" || v === "missing")) return false;
    return verdicts.every((v) => v === verdicts[0]) ||
      (last[0].duplicates.get(item.id) ?? []).some((id) => last.every((r) => r.duplicates.get(item.id)?.includes(id)));
  });
  if (repeated.length === 0) return undefined;
  const findings = new Set(repeated.flatMap(({ item }) => [
    ...last.flatMap((r) => r.duplicates.get(item.id) ?? []),
    ...phase.findings.filter((f) => f.status === "open" && f.itemId === item.id).map((f) => `${f.id}: ${f.evidence}`),
  ]));
  return `stuck: ${repeated.map((o) => o.item.id).join(", ")} unchanged for 3 rounds` +
    `\nReviewed candidates: ${last.map((r) => r.sha).join(", ")}` +
    `\nRepeated findings: ${findings.size ? [...findings].join("; ") : repeated.map((o) => `${o.item.id}: ${o.outcome}; ${o.evidence.join("; ")}`).join("\n")}`;
}
