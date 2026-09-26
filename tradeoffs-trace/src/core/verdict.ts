// Plan 3b: what the owner reads about a review round, computed from state
// alone (pure; no I/O). Front ends render these strings and never infer an
// outcome from individual ballots themselves (observed in run 4ec5e0f8: "M ✓
// A ✓ B ✓" read as all approved while M had vetoed and a blocking finding was
// open).

import { decisionStatus, isLiveDecision } from "./predicate.ts";
import { currentBallot, isValidBallot } from "./tally.ts";
import type { PhaseState, Reviewer } from "./types.ts";

export const REVIEWERS: readonly Reviewer[] = ["M", "A", "B"];

export interface ReviewerOutcome {
  reviewer: Reviewer;
  /** "pending": no review of the current candidate yet; "objects": a reject
   * ballot or a blocking finding; "approves": submitted with neither. */
  state: "pending" | "approves" | "objects";
  approvals: number;
  rejections: number;
  blocking: number;
  advisory: number;
  /** e.g. "M ✗ 2 reject · 1 blocking", "A ✓", "B ✓ 3 advisory", "M ⧗". */
  label: string;
}

/** Each reviewer's outcome on the current candidate. */
export function reviewerOutcomes(phase: PhaseState): ReviewerOutcome[] {
  const C = phase.candidate?.sha;
  return REVIEWERS.map((reviewer) => {
    const review = phase.reviews[reviewer]?.review;
    const submitted = Boolean(C && review && review.candidateSha === C);
    const ballots = phase.ballots.filter((b) => b.reviewer === reviewer && b.boundCandidateSha === C);
    const approvals = ballots.filter((b) => b.vote === "approve").length;
    const rejections = ballots.filter((b) => b.vote === "reject").length;
    const raised = phase.findings.filter(
      (f) => f.boundCandidateSha === C && (f.raisedBy === reviewer || (f.alsoRaisedBy ?? []).includes(reviewer)),
    );
    const blocking = raised.filter((f) => f.severity === "blocking").length;
    const advisory = raised.filter((f) => f.severity === "advisory").length;
    const state = !submitted ? "pending" : rejections > 0 || blocking > 0 ? "objects" : "approves";
    const parts = [
      rejections > 0 ? `${rejections} reject` : "",
      blocking > 0 ? `${blocking} blocking` : "",
      advisory > 0 ? `${advisory} advisory` : "",
    ].filter(Boolean);
    const mark = state === "pending" ? "⧗" : state === "objects" ? "✗" : "✓";
    return {
      reviewer,
      state,
      approvals,
      rejections,
      blocking,
      advisory,
      label: `${reviewer} ${mark}${parts.length > 0 ? ` ${parts.join(" · ")}` : ""}`,
    };
  });
}

/** Why the current candidate is not (or not yet) accepted, one short clause
 * each; empty when nothing stands in the way. Only failures that already
 * happened are listed; a gate still running is not a reason. */
export function notAcceptedReasons(phase: PhaseState): string[] {
  const C = phase.candidate?.sha;
  if (!C) return [];
  const reasons: string[] = [];
  if (phase.checks?.candidateSha === C && phase.checks.passed === false) reasons.push("checks failed");
  if (phase.probe?.candidateSha === C && phase.probe.passed === false) reasons.push("integration probe failed");
  // Failed decisions, grouped by why: "D-3 vetoed by M", or "16 decisions
  // failed (missing ballot from M)" rather than sixteen clauses.
  const failedBy = new Map<string, string[]>();
  for (const d of phase.decisions) {
    if (!isLiveDecision(d) || d.boundCandidateSha !== C) continue;
    // Plan 01g: a failed amendment is not a reason the candidate was not
    // accepted — it leaves the criterion unchanged and blocks nothing.
    if (d.amendment) continue;
    const s = decisionStatus(d, phase);
    if (s.status !== "failed") continue;
    const why = s.reason ?? "failed";
    failedBy.set(why, [...(failedBy.get(why) ?? []), shortId(d.id)]);
  }
  for (const [why, ids] of failedBy) {
    const verb = why === "M veto" ? "vetoed by M" : `failed (${why})`;
    reasons.push(ids.length <= 2 ? `${ids.join(", ")} ${verb}` : `${ids.length} decisions ${verb}`);
  }
  for (const f of phase.findings) {
    if (f.status === "open" && f.severity === "blocking") reasons.push(`blocking finding ${shortId(f.id)} open`);
  }
  return reasons;
}

// ---------------------------------------------------------------------------
// Plan 01h: the live trade-offs panel
// ---------------------------------------------------------------------------

/** The ordered kinds the owner sees while a run is live, most important
 * first (design 01_ref_design.md goal 5). */
export type TradeoffKind = "directive" | "amendment" | "flagged" | "veto" | "dissent" | "advisories";

/** One self-contained line of the Trade-offs panel. `recordId` names the
 * record the line is about (a decision id, or an advisory finding id for the
 * count line), so `RET` on the line can open the decision view at it. */
export interface TradeoffEntry {
  kind: TradeoffKind;
  text: string;
  recordId?: string;
}

/** The most lines the panel may show (design 01h: at most 6, most important
 * first). */
export const MAX_TRADEOFFS = 6;

/** Plan 01h: the few trade-offs that matter, most important first, at most
 * `max` lines. Everything comes from the phase's own records — the tally
 * (decisionStatus), the ballots (M's veto reason, the dissenters), the
 * findings (the advisory count), the amendment records and the owner
 * directives with their recorded delivery state. Nothing is inferred, and a
 * decision appears at most once, under its most actionable heading. */
export function tradeoffEntries(phase: PhaseState, max = MAX_TRADEOFFS): TradeoffEntry[] {
  const C = phase.candidate?.sha;
  const K = phase.contract.contractVersion;
  const directives: TradeoffEntry[] = [];
  const amendments: TradeoffEntry[] = [];
  const flagged: TradeoffEntry[] = [];
  const vetoes: TradeoffEntry[] = [];
  const dissents: TradeoffEntry[] = [];
  const advisories: TradeoffEntry[] = [];

  // (0) owner directives in force (plan 01i) whose immediate steer to a
  // recorded live agent has not been acknowledged — a ruling still in the
  // air is what the owner must know first. A directive with NO recorded
  // targets is not shown: "no agent was live at the time", and for a
  // program-wide directive seeded into a node's plan (conductor.ts's
  // `seededDirective`) the empty list means it reached this run through its
  // prompts all along. Nothing recorded says an agent has not received it,
  // so nothing here claims one has not (finding A-1).
  for (const d of phase.ownerDirectives ?? []) {
    if (d.status !== "in-force") continue;
    if (d.targets.length === 0) continue;
    const pending = d.targets.filter((t) => (d.deliveries ?? {})[t] !== "delivered");
    if (pending.length === 0) continue;
    directives.push({
      kind: "directive",
      recordId: d.id,
      text: `directive ${d.id} not yet delivered to ${pending.join(", ")}: ${oneLine(d.text, 80)}`,
    });
  }

  // (1) disputed or amended criteria (plan 01g), old → new. A superseded
  // record is history — the decision view does not render it, so a line
  // pointing at it could not be followed (finding A-2).
  for (const d of phase.decisions) {
    const a = d.amendment;
    if (!a || !isLiveDecision(d)) continue;
    const arrow = a.status === "reverted" ? `${a.proposedWording} → ${a.criterion}` : `${a.criterion} → ${a.proposedWording}`;
    const status = decisionStatus(d, phase).status;
    const label =
      a.status === "applied"
        ? "⚑ AMENDED"
        : a.status === "reverted"
          ? "⚑ REVERTED"
          : status === "failed"
            ? "⚑ AMENDMENT REJECTED"
            : "⚑ AMENDMENT (pending)";
    amendments.push({ kind: "amendment", recordId: d.id, text: `${label} ${a.id}: ${oneLine(arrow, 140)}` });
  }

  // (2)-(4) the current candidate's live decisions, each under exactly one
  // heading: a flagged (reserved) decision, an M veto, or a pass with dissent.
  const live = phase.decisions.filter((d) => isLiveDecision(d) && d.source !== "trigger" && d.boundCandidateSha === C && !d.amendment);
  for (const d of live) {
    const status = decisionStatus(d, phase);
    const choice = oneLine(d.choice, 70);
    if (status.status === "failed" && (status.reason ?? "").includes("M veto")) {
      const m = C ? currentBallot(phase.ballots, d.id, "M", C, K, d.version) : undefined;
      const why = m && isValidBallot(m) ? oneLine(m.rationale, 80) : "no reason recorded";
      vetoes.push({ kind: "veto", recordId: d.id, text: `vetoed by M: ${shortId(d.id)} ${choice} — ${why}` });
      continue;
    }
    const dissenters = C
      ? (["M", "A", "B"] as const).filter((who) => {
          const b = currentBallot(phase.ballots, d.id, who, C, K, d.version);
          return isValidBallot(b) && b.vote === "reject";
        })
      : [];
    if (d.class === "reserved") {
      const who = dissenters.length > 0 ? `; ${dissenters.join(", ")} rejected` : "";
      flagged.push({ kind: "flagged", recordId: d.id, text: `⚑ flagged: ${shortId(d.id)} ${choice} — ${status.status}${who}` });
      continue;
    }
    if (status.status === "passed" && dissenters.length > 0) {
      dissents.push({ kind: "dissent", recordId: d.id, text: `passed with dissent (${dissenters.join(", ")} rejected): ${shortId(d.id)} ${choice}` });
    }
  }

  // (5) one line counting the advisories instead of listing them.
  const openAdvisories = phase.findings.filter((f) => f.severity === "advisory" && f.status === "open");
  if (openAdvisories.length > 0) {
    const fresh = C ? openAdvisories.filter((f) => f.boundCandidateSha === C) : [];
    advisories.push({
      kind: "advisories",
      recordId: (fresh[0] ?? openAdvisories[0]).id,
      text: `${openAdvisories.length} advisories (${fresh.length} new) — C-c m d`,
    });
  }

  return [...directives, ...amendments, ...flagged, ...vetoes, ...dissents, ...advisories].slice(0, max);
}

/** One line, whitespace collapsed and truncated at `max` characters. */
function oneLine(text: string, max: number): string {
  const t = text.replace(/\s+/g, " ").trim();
  return t.length > max ? `${t.slice(0, max - 1)}…` : t;
}

/** A record id without its phase and candidate prefix: "D-p2d-x-dbe24445-4"
 * → "D-4", "F-p2d-x-M-3" → "F-M-3". */
export function shortId(id: string): string {
  const m = id.match(/^([A-Z]+)-.*?-((?:[MAB]-)?(?:[a-z]+-)?\d+)$/);
  if (!m) return id;
  const tail = m[2].replace(/^(?:disc|trigger)-/, "");
  return `${m[1]}-${tail}`;
}
