// Plan 06g2 (A1): the round's orchestration — one base, K lanes, one winner.
//
// `runRound` is the ONLY module that orchestrates a round. It creates each
// lane's worktree, dispatches the lane workers on the same prompt, freezes
// each lane's result to a candidate, queues the candidate checks one after
// the other under the machine-wide lock, dispatches the per-candidate
// reviews and the pick turn, and hands the votes to `pickWinner` (06g's
// core/rounds.ts, which is the only place a winner is decided — never a
// model). The conductor decides nothing about lanes beyond calling it: every
// effect the round needs is a callback on `LaneHost`, which the conductor
// implements with its own worktree, agent, check, review and vote machinery.
//
// The round's own record events (06g's ROUND_STARTED / CANDIDATE_SUBMITTED /
// CANDIDATE_CHECKED / PICK_VOTE / CANDIDATE_PICKED, plus this phase's
// ROUND_REVIEW_SUBMITTED for the per-candidate reviews) are emitted through
// `LaneHost.emit`, so a restart folds them back into `phase.rounds` and the
// views render them.
//
// Nothing here runs for a plan without `#+TT_WORKERS` (or with 1): the
// conductor never calls `runRound`, and the single-candidate loop is byte for
// byte what it was.

import { candidateLabel, laneIds, passingCandidates, pickWinner, revotePair, type RoundWinner } from "./rounds.ts";
import type {
  CriterionDispute,
  DecisionDisclosure,
  Event,
  PriorDecisionStatement,
  RoundRecord,
} from "./types.ts";

/** One lane's build result: the candidate it froze, or the note saying why it
 * produced none (the lane crashed, timed out, or submitted nothing). */
export interface LaneBuild {
  lane: string;
  sha?: string;
  note?: string;
  /** The lane's own sweep found and killed a survivor in its worktree. The
   * winner's hand-off carries this into FREEZE_COMPLETED, exactly like the
   * single-candidate freeze does. */
  tainted?: boolean;
  /** The winner's hand-off needs the lane's own disclosures. */
  disclosures?: DecisionDisclosure[];
  prior?: PriorDecisionStatement[];
  dispute?: CriterionDispute;
  /** The lane's own `submit_coverage` payload, for the winner's hand-off. */
  coverage?: unknown;
}

/** One candidate's check result. `ok` false carries the note the status and
 * the next round's prompt show. */
export interface LaneCheck {
  ok: boolean;
  note?: string;
}

/** Everything `runRound` may ask the conductor to do. The conductor owns
 * every effect (worktrees, agents, checks, reviews, votes, events); the round
 * owns only the sequence. */
export interface LaneHost {
  readonly phaseId: string;
  readonly goal: string;
  /** The seats that review and vote, from the contract's `#+TT_REVIEWERS`
   * (never a model's choice). */
  readonly seats: readonly string[];
  /** Plan 06h (A3): the leader, whose vote breaks a revote tie. Absent means
   * the first seat. */
  readonly leader?: string;
  /** The round records the log holds so far. */
  rounds(): readonly RoundRecord[];
  /** The settled ledger, one line per record (the leader's pick context). */
  ledger(): readonly string[];
  /** Every earlier round's reviews and votes, one line each. */
  earlierRounds(): readonly string[];
  /** Create lane's worktree at `base` and return its path (A2). */
  createWorktree(lane: string, base: string): string;
  /** The round's worker prompt: the SAME text for every lane (A2). */
  lanePrompt(round: number, base: string): string;
  /** Run lane's worker in its own worktree and freeze its result to a
   * candidate, or say why it produced none. */
  buildLane(lane: string, round: number, base: string, prompt: string): Promise<LaneBuild>;
  /** Check one candidate, under the machine-wide lock, one after the other
   * (C2: two candidates' checks never overlap). */
  checkCandidate(round: number, lane: string, sha: string): Promise<LaneCheck>;
  /** One seat reviews one passing candidate. */
  reviewCandidate(round: number, lane: string, sha: string, seat: string): Promise<void>;
  /** One seat's pick turn. The host builds the prompt (`buildPickPrompt`)
   * and records the seat's vote; it never decides the winner. `revote` is
   * true for the top-two revote turn (plan 06h A3). */
  pickTurn(
    round: number,
    base: string,
    seat: string,
    passing: ReadonlyArray<{ lane: string; sha: string }>,
    revote?: boolean,
  ): Promise<void>;
  /** Record one round event. */
  emit(event: Event): void;
}

export interface RoundContext {
  round: number;
  /** The one version every lane starts from (round 1: the integration head;
   * a later round: the previous winner's commit). */
  base: string;
  lanes: readonly string[];
  /** 02k (2026-10-10): a lane whose candidate passed its checks in a round
   * that then failed for a missing review or vote starts the repeat from
   * that candidate, not from `base`, so its work is not thrown away. */
  laneBases?: Readonly<Record<string, string>>;
}

export interface RoundOutcome {
  round: number;
  base: string;
  lanes: readonly string[];
  /** The winner `pickWinner` computed, or undefined when no candidate passed
   * (the round repeats from the same base). */
  winner?: RoundWinner;
  builds: readonly LaneBuild[];
  /** Plan 06g2: set when the round could not complete — a seat's review of a
   * passing candidate or a seat's pick vote is missing. No winner is picked,
   * so the round repeats; a round never hands off on fewer than K × N reviews
   * and N votes. */
  failure?: string;
  /** 02k: seats whose pick vote is missing from a round that still picked,
   * because the votes cast already gave the winner a majority of all seats. */
  missingVotes?: readonly string[];
}

function errorNote(err: unknown): string {
  return String((err as Error)?.message ?? err);
}

/** The per-lane starting points for a repeat of `last`. A round with a
 * passing candidate and no winner could not complete (a missing review or
 * pick vote; `runRound` picks whenever the set is complete), so each lane
 * whose candidate passed its checks restarts from that candidate. A round in
 * which no candidate passed keeps `base` for every lane (the repeat sees the
 * failures; plan 06g). Derived from the round record, so it survives a
 * conductor restart. */
export function repeatLaneBases(last: RoundRecord | undefined): Record<string, string> | undefined {
  if (!last || last.picked) return undefined;
  const out: Record<string, string> = {};
  for (const c of passingCandidates(last)) if (typeof c.sha === "string" && c.sha.length > 0) out[c.lane] = c.sha;
  return Object.keys(out).length > 0 ? out : undefined;
}

/** One round: K lanes from one base, one winner. See the module comment. */
export async function runRound(host: LaneHost, ctx: RoundContext): Promise<RoundOutcome> {
  const lanes = [...ctx.lanes];
  host.emit({ type: "ROUND_STARTED", round: ctx.round, base: ctx.base, lanes });

  // 1. Each lane gets its own worktree, worker and sweep, on the same prompt.
  //    The lanes run in parallel; one lane's failure never stops the other
  //    (A2), so each is caught into a note.
  const prompt = host.lanePrompt(ctx.round, ctx.base);
  const builds = await Promise.all(
    lanes.map(async (lane): Promise<LaneBuild> => {
      try {
        return await host.buildLane(lane, ctx.round, ctx.laneBases?.[lane] ?? ctx.base, prompt);
      } catch (err) {
        return { lane, note: `the lane failed: ${errorNote(err)}` };
      }
    }),
  );
  for (const build of builds) {
    if (typeof build.sha === "string" && build.sha.length > 0) {
      host.emit({ type: "CANDIDATE_SUBMITTED", round: ctx.round, lane: build.lane, sha: build.sha });
    }
  }

  // 2. Each lane's candidate is checked — one after the other, never two at
  //    once (the host takes the machine-wide lock around each).
  for (const build of builds) {
    if (typeof build.sha !== "string" || build.sha.length === 0) continue;
    let check: LaneCheck;
    try {
      check = await host.checkCandidate(ctx.round, build.lane, build.sha);
    } catch (err) {
      check = { ok: false, note: `the check could not run: ${errorNote(err)}` };
    }
    host.emit({
      type: "CANDIDATE_CHECKED",
      round: ctx.round,
      lane: build.lane,
      ok: check.ok,
      ...(check.note ? { note: check.note } : {}),
    });
  }
  // A lane that produced no candidate records no candidate at all: its note
  // is what the status shows.
  for (const build of builds) {
    if (typeof build.sha === "string" && build.sha.length > 0) continue;
    host.emit({
      type: "CANDIDATE_CHECKED",
      round: ctx.round,
      lane: build.lane,
      ok: false,
      note: build.note ?? "the lane produced no candidate",
    });
  }

  const record = host.rounds().find((r) => r.round === ctx.round);
  const passing = record ? passingCandidates(record) : [];

  // 3. M, A and B each review every passing candidate — K × N reviews. A
  //    review that cannot be taken is NOT a candidate with fewer reviews: the
  //    round fails and repeats, so no winner is ever handed off with an
  //    incomplete set.
  let failure: string | undefined;
  for (const candidate of passing) {
    // The three seats review a candidate IN PARALLEL (today's REVIEWING
    // dispatches all three at once, and the discovery barrier needs all three
    // in flight); candidates are reviewed one after the other.
    const results = await Promise.all(
      host.seats.map(async (seat) => {
        try {
          await host.reviewCandidate(ctx.round, candidate.lane, candidate.sha as string, seat);
          return undefined;
        } catch (err) {
          return `${seat}'s review of ${candidateLabel(ctx.round, candidate.lane)} failed: ${errorNote(err)}`;
        }
      }),
    );
    failure = results.find((r): r is string => r !== undefined);
    if (failure) break;
  }
  if (!failure) {
    const reviewed = host.rounds().find((r) => r.round === ctx.round);
    for (const candidate of reviewed ? passingCandidates(reviewed) : []) {
      const count = (candidate.reviews ?? []).length;
      if (count < host.seats.length) {
        failure = `${candidateLabel(ctx.round, candidate.lane)} was reviewed by ${count} of ${host.seats.length} seats`;
        break;
      }
    }
  }

  // 4. The pick turn: each seat votes for one candidate. With a single
  //    passing candidate the vote is skipped — it wins without one. Every
  //    seat must vote before a winner is picked.
  let missingVotes: string[] | undefined;
  if (!failure && passing.length > 1) {
    // Every seat gets its pick turn; one seat's failure no longer stops the
    // others (02k: a pick that failed discarded the round before the
    // remaining seats voted).
    const pickFailures: string[] = [];
    for (const seat of host.seats) {
      try {
        await host.pickTurn(
          ctx.round,
          ctx.base,
          seat,
          passing.map((c) => ({ lane: c.lane, sha: c.sha as string })),
        );
      } catch (err) {
        pickFailures.push(`${seat}'s pick vote failed: ${errorNote(err)}`);
      }
    }
    const afterPick = host.rounds().find((r) => r.round === ctx.round);
    const voted = new Set((afterPick?.votes ?? []).map((v) => v.seat));
    const missing = host.seats.filter((s) => !voted.has(s));
    if (missing.length > 0) {
      // 02k/06k2 (2026-10-10, owner): a missing vote that cannot change the
      // result does not discard the round. When the votes cast already give
      // one lane a strict majority of ALL seats (pickWinner's own rule),
      // no missing vote could make another lane win, so the round picks and
      // records which seats did not vote. Otherwise the round fails as before:
      // a majority is never invented.
      const decided = afterPick && passing.length === 2 ? pickWinner(afterPick, host.seats, host.leader) : undefined;
      if (decided) missingVotes = missing;
      else failure = pickFailures[0] ?? `the pick turn is missing a vote from ${missing.join(", ")}`;
    }
  }
  let finalRecord = host.rounds().find((r) => r.round === ctx.round);

  // 4b. Plan 06h (A3): with three or more candidates and no strict majority,
  //     the top two by votes go to ONE revote. A tie in the revote is broken
  //     by the leader's vote (pickWinner).
  if (!failure && finalRecord && passing.length >= 3) {
    const pair = revotePair(finalRecord, host.seats);
    if (pair) {
      host.emit({ type: "REVOTE_STARTED", round: ctx.round, lanes: pair });
      const pairCandidates = passing.filter((c) => pair.includes(c.lane)).map((c) => ({ lane: c.lane, sha: c.sha as string }));
      for (const seat of host.seats) {
        try {
          await host.pickTurn(ctx.round, ctx.base, seat, pairCandidates, true);
        } catch (err) {
          failure = `${seat}'s revote failed: ${errorNote(err)}`;
          break;
        }
      }
      finalRecord = host.rounds().find((r) => r.round === ctx.round);
      if (!failure && finalRecord) {
        const voted = new Set((finalRecord.revote?.votes ?? []).map((v) => v.seat));
        const missing = host.seats.filter((s) => !voted.has(s));
        if (missing.length > 0) failure = `the revote is missing a vote from ${missing.join(", ")}`;
      }
    }
  }
  if (failure) return { round: ctx.round, base: ctx.base, lanes, builds, failure };

  // 5. `pickWinner` — code, never a model — decides the round.
  const winner = finalRecord ? pickWinner(finalRecord, host.seats, host.leader) : undefined;
  if (winner) {
    host.emit({ type: "CANDIDATE_PICKED", round: ctx.round, lane: winner.lane, sha: winner.sha, votes: winner.votes });
  }
  return { round: ctx.round, base: ctx.base, lanes, winner, builds, ...(missingVotes ? { missingVotes } : {}) };
}

/** The lanes a plan's frozen contract runs. A plan without `#+TT_WORKERS`
 * (or with 1) is the single-candidate loop: `laneIds(1)` is `["a"]`, and the
 * conductor never calls `runRound` for it. */
export function lanesOfContract(contract: { workers?: number }): string[] {
  return laneIds(contract.workers ?? 1);
}

/** True when this phase runs more than one lane per round. */
export function isLaneRound(contract: { workers?: number }): boolean {
  return lanesOfContract(contract).length > 1;
}

/** The lines a later round's worker prompt carries about the previous
 * round's lanes: every lane's candidate and why it failed, so a repeated
 * round tells both workers what both lanes got wrong. Pure, so the prompt is
 * unit-tested without a run. */
export function laneFailureLines(round: RoundRecord | undefined): string[] {
  if (!round) return [];
  const out: string[] = [];
  for (const lane of round.lanes) {
    const candidate = round.candidates.find((c) => c.lane === lane);
    const label = candidateLabel(round.round, lane);
    if (!candidate?.sha) {
      out.push(`${label} (lane ${lane}) produced no candidate: ${candidate?.note ?? "the lane failed before freezing"}`);
      continue;
    }
    if (candidate.ok === false) {
      out.push(`${label} (lane ${lane}) failed its checks: ${candidate.note ?? "a check command failed"}`);
      continue;
    }
    if (candidate.ok === true) out.push(`${label} (lane ${lane}) passed its checks`);
  }
  return out;
}
