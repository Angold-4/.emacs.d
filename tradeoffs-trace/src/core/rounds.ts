// Plan 2c: decision records across review rounds.
//
// v1 invalidates all evidence when a new candidate is frozen (design §7.2):
// ballots, reviews, checks. Decision *records* used to survive unchanged, so
// a record describing the previous candidate stayed votable. Reviewers then
// correctly rejected it as "no longer true of the new code", and acceptance
// failed on records the worker could not change (dogfood run 4ec5e0f8,
// round 2). This module decides, at FREEZE_COMPLETED, which records carry
// forward to the new candidate.
//
// - A worker record the repairing worker marked `kept` is rebound to the new
//   candidate; `changed` is rebound with the new plain-language text. The
//   version bumps either way, so no ballot on the old record can count.
// - A worker record marked `withdrawn`, or not mentioned at all, is
//   superseded.
// - Reviewer-discovered and trigger records belong to the review of the old
//   candidate. They are superseded: reviewers rediscover (and match) against
//   the new candidate, and triggers are recomputed from the new diff.
//
// Superseded records are kept for history and never votable (predicate.ts's
// `isLiveDecision`).

import { isLiveDecision } from "./predicate.ts";
import { DEFAULT_SEATS } from "./seats.ts";
import { currentBallot, tally } from "./tally.ts";
import type {
  Ballot,
  ContractVersion,
  Decision,
  Finding,
  FindingSeverity,
  LaneCandidateRecord,
  PhaseContract,
  PickVote,
  PriorDecisionStatement,
  RoundRecord,
} from "./types.ts";
// (ContractVersion is used by carryDecisionsForward's optional 4th argument.)

export function carryDecisionsForward(
  decisions: Decision[],
  prior: PriorDecisionStatement[] | undefined,
  newCandidateSha: string,
  newContractVersion?: ContractVersion,
): Decision[] {
  const statements = new Map((prior ?? []).map((p) => [p.id, p]));
  const short = newCandidateSha.slice(0, 7);
  return decisions.map((d) => {
    if (!isLiveDecision(d) || d.boundCandidateSha === newCandidateSha) return d;
    // Plan 01g: an amendment record belongs to the phase's contract, not to
    // one candidate. A still-proposed one is carried forward (rebound to the
    // new candidate and contract) so it can be voted on in a later round; an
    // applied or reverted one stays as history. It never blocks acceptance.
    if (d.amendment) {
      return {
        ...d,
        boundCandidateSha: newCandidateSha,
        ...(newContractVersion ? { boundContractVersion: newContractVersion } : {}),
        version: d.version + 1,
      };
    }
    const statement = d.source === "worker" ? statements.get(d.id) : undefined;
    if (statement?.status === "kept") {
      return { ...d, boundCandidateSha: newCandidateSha, version: d.version + 1 };
    }
    if (statement?.status === "changed") {
      return {
        ...d,
        choice: statement.choice ?? d.choice,
        whyItMatters: statement.whyItMatters ?? d.whyItMatters,
        alternatives: statement.alternatives ?? d.alternatives,
        recommendation: statement.recommendation ?? d.recommendation,
        boundCandidateSha: newCandidateSha,
        version: d.version + 1,
      };
    }
    const why =
      statement?.status === "withdrawn"
        ? `withdrawn by the worker at candidate ${short}`
        : d.source === "worker"
          ? `candidate ${short}: not carried forward by the worker`
          : `candidate ${short}: ${d.source} record from an earlier round`;
    return { ...d, supersededBy: why, version: d.version + 1 };
  });
}

/** Skill fix 5: ballots a new round inherits. When the worker keeps a
 * decision unchanged (`kept`) and it PASSED its vote on the previous
 * candidate, each reviewer's ballot carries over, rebound to the new
 * candidate and record version and marked `carriedFrom`. Reviewers see the
 * record as carried and vote again only if the new changes affect it; a
 * fresh ballot is appended after the carried one, so it wins (currentBallot
 * takes the latest). Run 0a35ae40 (12b) re-cast every ballot in each of its
 * three rounds, 47 in the last one alone.
 *
 * `prevDecisions`/`prevBallots`/`findings` are the phase before the freeze;
 * `carried` is carryDecisionsForward's result. */
export function carryBallotsForward(
  prevDecisions: Decision[],
  prevBallots: Ballot[],
  findings: Finding[],
  prior: PriorDecisionStatement[] | undefined,
  prevCandidateSha: string | undefined,
  contractVersion: ContractVersion,
  carried: Decision[],
  newCandidateSha: string,
  seats: readonly string[] = DEFAULT_SEATS,
): Ballot[] {
  if (!prevCandidateSha) return [];
  const kept = new Set((prior ?? []).filter((p) => p.status === "kept").map((p) => p.id));
  const out: Ballot[] = [];
  for (const d of carried) {
    if (!kept.has(d.id) || d.boundCandidateSha !== newCandidateSha) continue;
    const before = prevDecisions.find((x) => x.id === d.id);
    if (!before || before.boundCandidateSha !== prevCandidateSha) continue;
    if (tally(before, prevBallots, findings, prevCandidateSha, contractVersion, seats) !== "pass") continue;
    for (const reviewer of seats) {
      const b = currentBallot(prevBallots, d.id, reviewer, prevCandidateSha, contractVersion, before.version);
      if (!b) continue;
      out.push({ ...b, boundCandidateSha: newCandidateSha, boundRecordVersion: d.version, carriedFrom: prevCandidateSha });
    }
  }
  return out;
}

// ---------------------------------------------------------------------------
// Plan 06g: the round — K lanes, one base, one winner
// ---------------------------------------------------------------------------
//
// With `#+TT_WORKERS: 2` one round starts from ONE version and runs two
// lanes, each with its own worktree, worker and sweep, on the same prompt.
// Each lane freezes its own candidate; the candidates are checked one after
// the other under the machine-wide lock; M, A and B review every passing
// candidate; then each seat votes in a pick turn. This module is the round's
// only decision point: `pickWinner` decides the winner, `blocksAcceptance`
// decides whether a finding blocks acceptance, and `roundBudget` decides how
// many rounds a phase may spend. No model ever decides any of the three.
//
// With no `#+TT_WORKERS` (or 1) none of this runs: no round event is
// recorded and the phase behaves exactly as it did before this plan.

/** The default number of rounds one phase may spend (owner, 2026-10-07:
 * "we cannot afford the loop becomes infinity … likely most of the loop
 * should finish within 3 rounds"). `#+TT_ROUNDS` overrides it. */
export const DEFAULT_ROUND_BUDGET = 3;

/** The lane ids of a K-lane round: `a`, `b`, `c`, … Lane 0 is `a`, so a
 * one-lane round's single candidate is `C<round>-a`. */
export function laneIds(k: number): string[] {
  const n = Number.isFinite(k) && k >= 1 ? Math.floor(k) : 1;
  return Array.from({ length: n }, (_, i) => String.fromCharCode("a".charCodeAt(0) + i));
}

/** How a candidate is named in the views: `C2-b` (round 2, lane b). */
export function candidateLabel(round: number, lane: string): string {
  return `C${round}-${lane}`;
}

/** `roundBudget(contract)` is the ONLY place that decides how many rounds a
 * phase may spend. It reads the contract's frozen `#+TT_ROUNDS` and falls
 * back to the default of 3. Both the one-lane and the two-lane loop ask this
 * function, never a constant of their own. */
export function roundBudget(contract: Pick<PhaseContract, "roundsAllowed"> | undefined): number {
  const n = contract?.roundsAllowed;
  if (typeof n !== "number" || !Number.isFinite(n) || n < 1) return DEFAULT_ROUND_BUDGET;
  return Math.floor(n);
}

/** The candidates of a round that passed their checks: the only ones that are
 * reviewed and voted on. A lane that crashed or timed out has no `sha`, and a
 * candidate that failed checks has `ok === false`. */
export function passingCandidates(round: RoundRecord): LaneCandidateRecord[] {
  return round.candidates.filter((c) => typeof c.sha === "string" && c.sha.length > 0 && c.ok === true);
}

export interface RoundWinner {
  lane: string;
  sha: string;
  /** The pick votes the winner took; 0 when it was the only passing
   * candidate and won without a vote. */
  votes: number;
}

/** `pickWinner(round, seats)` is the ONLY place that decides a round's
 * winner, in code, never by a model (plan 06g, A3):
 *
 * - no candidate passes: no winner — the round repeats from the same base;
 * - exactly one passes: it wins without a vote;
 * - two or more: the lane with a strict majority of the seats' pick votes
 *   (2 of 3, 3 of 5, 4 of 7) wins. A round whose votes give no lane a
 *   majority has no winner. */
export function pickWinner(round: RoundRecord, seats: number | readonly string[], leader?: string): RoundWinner | undefined {
  const passing = passingCandidates(round);
  if (passing.length === 0) return undefined;
  if (passing.length === 1) return { lane: passing[0].lane, sha: passing[0].sha as string, votes: 0 };
  const seatList = typeof seats === "number" ? undefined : seats;
  const seatCount = typeof seats === "number" ? seats : seats.length;
  const needed = Math.floor(seatCount / 2) + 1;
  const counts = countVotes(round.votes, passing);
  let best: RoundWinner | undefined;
  for (const c of passing) {
    const votes = counts.get(c.lane) ?? 0;
    if (votes >= needed && (!best || votes > best.votes)) best = { lane: c.lane, sha: c.sha as string, votes };
  }
  if (best) return best;
  // Plan 06h (A3): 3+ candidates with no strict majority go to one revote
  // between the top two. A tie there is broken by the leader's vote.
  if (passing.length < 3 || !round.revote) return undefined;
  const pair = round.revote.lanes;
  const inPair = passing.filter((c) => pair.includes(c.lane));
  // The leader is the tiebreak, so the majority is over the OTHER seats; a
  // tie among them is broken by the leader's own revote vote. Counting the
  // leader here would make a tie impossible with an odd seat count.
  const leaderSeat = leader ?? seatList?.[0] ?? DEFAULT_SEATS[0];
  const nonLeader = seatList ? seatList.filter((s) => s !== leaderSeat) : undefined;
  const revoteVotes = nonLeader ? round.revote.votes.filter((v) => nonLeader.includes(v.seat)) : round.revote.votes;
  const revoteCounts = countVotes(revoteVotes, inPair);
  const revoteNeeded = nonLeader ? Math.floor(nonLeader.length / 2) + 1 : needed;
  let rBest: RoundWinner | undefined;
  for (const c of inPair) {
    const votes = revoteCounts.get(c.lane) ?? 0;
    if (votes >= revoteNeeded && (!rBest || votes > rBest.votes)) rBest = { lane: c.lane, sha: c.sha as string, votes };
  }
  if (rBest) return rBest;
  const leaderVote = round.revote.votes.find((v) => v.seat === leaderSeat);
  const picked = leaderVote ? inPair.find((c) => c.lane === leaderVote.lane) : undefined;
  if (picked) return { lane: picked.lane, sha: picked.sha as string, votes: revoteCounts.get(picked.lane) ?? 0 };
  return undefined;
}

/** Plan 06h (A3): the two lanes a 3+-candidate round must revote between, or
 * undefined when the first vote already gave a lane a strict majority (or
 * there are fewer than three candidates). `runRound` uses this to decide
 * whether to run the revote turn. */
export function revotePair(round: RoundRecord, seats: number | readonly string[]): string[] | undefined {
  const passing = passingCandidates(round);
  if (passing.length < 3) return undefined;
  const seatCount = typeof seats === "number" ? seats : seats.length;
  const needed = Math.floor(seatCount / 2) + 1;
  const counts = countVotes(round.votes, passing);
  if (passing.some((c) => (counts.get(c.lane) ?? 0) >= needed)) return undefined;
  const ranked = [...passing].sort((a, b) => (counts.get(b.lane) ?? 0) - (counts.get(a.lane) ?? 0));
  if (ranked.length < 2) return undefined;
  return [ranked[0].lane, ranked[1].lane];
}

function countVotes(votes: readonly PickVote[], candidates: readonly LaneCandidateRecord[]): Map<string, number> {
  const counts = new Map<string, number>();
  for (const vote of votes) {
    if (!candidates.some((c) => c.lane === vote.lane)) continue;
    counts.set(vote.lane, (counts.get(vote.lane) ?? 0) + 1);
  }
  return counts;
}

/** One lane's pick votes, in seat order. */
export function votesFor(round: RoundRecord, lane: string): PickVote[] {
  return round.votes.filter((v) => v.lane === lane);
}

/** What `blocksAcceptance` may look at besides the finding itself. The
 * conductor builds it from the contract and the round's own check records;
 * nothing here is model-supplied. */
export interface AcceptanceGate {
  /** Every item id a finding may name: the contract's R, C and A ids, plus
   * every owner correction id in force. */
  itemIds: string[];
  /** Tests (or named behaviours) that failed in this candidate's checks but
   * passed at the round's base — the regressions. */
  regressions: string[];
}

/** A `file:line` citation: `src/core/rounds.ts:12` or `foo.ts:12:3`. */
const FILE_LINE_RE = /\b[A-Za-z0-9_./-]+\.[A-Za-z0-9]+:\d+(?::\d+)?\b/;

/** True when the finding's text names one of the contract's (or an owner
 * correction's) item ids as a word. */
function namesItem(text: string, itemIds: readonly string[]): boolean {
  for (const id of itemIds) {
    if (id.length === 0) continue;
    const re = new RegExp(`(^|[^A-Za-z0-9_-])${id.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}([^A-Za-z0-9_-]|$)`);
    if (re.test(text)) return true;
  }
  return false;
}

/** True when the finding names a regression: a test or behaviour that passed
 * at the round's base and fails now. The check's own name is matched as a
 * whole word inside the finding's evidence. */
function namesRegression(text: string, regressions: readonly string[]): boolean {
  for (const name of regressions) {
    if (name.length === 0) continue;
    if (text.includes(name)) return true;
  }
  return false;
}

/** `blocksAcceptance(finding, round, gate)` is the ONLY place that decides
 * whether a finding blocks acceptance (owner directive ODP-1, plan 06g A6):
 *
 * - round 1: every blocking finding blocks, exactly as before;
 * - from round 2: a blocking finding blocks only when it (a) names an item of
 *   the contract or of an owner correction that is unmet on the main path,
 *   with the item id and a `file:line`, or (b) is a regression — a test or
 *   behaviour that passed at the round's base and fails now.
 *
 * Anything else — a further edge path beyond the named items, hardening,
 * wording, style — is an advisory: recorded as a trade-off and carried, never
 * blocking. An advisory-severity finding never blocks, in any round. */
/** The part of a finding `blocksAcceptance` reads. A `Finding` is assignable
 * to it, so a raised-but-not-yet-recorded disclosure can be judged too. */
export interface FindingGround {
  severity: FindingSeverity;
  evidence: string;
  itemId?: string;
  criterionDisputed?: string;
}

export function blocksAcceptance(finding: FindingGround, round: number, gate: AcceptanceGate): boolean {
  if (finding.severity !== "blocking") return false;
  if (round <= 1) return true;
  const text = [finding.evidence, finding.criterionDisputed ?? ""].filter(Boolean).join("\n");
  const itemNamed = finding.itemId !== undefined && gate.itemIds.includes(finding.itemId) || namesItem(text, gate.itemIds);
  if (itemNamed && FILE_LINE_RE.test(text)) return true;
  if (namesRegression(text, gate.regressions)) return true;
  return false;
}

/** Why `blocksAcceptance` returned false — the sentence the trade-off record
 * carries. Kept beside the rule so the two can never disagree about which
 * clause applied. */
export function advisoryReason(finding: FindingGround, round: number, gate: AcceptanceGate): string {
  if (round <= 1) return "round 1: a blocking finding always blocks";
  const text = [finding.evidence, finding.criterionDisputed ?? ""].filter(Boolean).join("\n");
  const itemNamed = (finding.itemId !== undefined && gate.itemIds.includes(finding.itemId)) || namesItem(text, gate.itemIds);
  if (itemNamed && !FILE_LINE_RE.test(text)) {
    return "it names a contract item but cites no file:line, so it is an advisory from round 2";
  }
  if (finding.severity !== "blocking") return "it was raised at advisory severity";
  return "from round 2 only an unmet named item (with its id and file:line) or a regression blocks";
}

/** The open advisories a phase carries — the "accept with carried items"
 * decision's list. */
export function openAdvisories(findings: readonly Finding[]): Finding[] {
  return findings.filter((f) => f.status === "open" && f.severity === "advisory");
}
