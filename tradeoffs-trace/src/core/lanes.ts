// Plan 06g2 (A1): the round's orchestration — one base, K lanes, one winner.
//
// `runRound` is the ONLY module that orchestrates a round. It creates each
// lane's worktree, dispatches the lane workers on the same prompt, freezes
// each lane's result to a candidate, queues the candidate checks one after
// the other under the machine-wide lock, dispatches the per-candidate
// reviews (09-2: at the candidate's submission, while its checks run) and
// the pick turn, and hands the votes to `pickWinner` (06g's
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

import { candidateLabel, everyLaneLost, laneIds, laneMetItems, passingCandidates, pickWinner, revotePair, type PickGuard, type RoundWinner } from "./rounds.ts";
import type {
  ConsolidationTake,
  CriterionDispute,
  DecisionDisclosure,
  DirectiveAnswer,
  Event,
  FindingAnswer,
  LaneCandidateRecord,
  PhaseContract,
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
  /** Plan 06k3 (A3): the lane's answers to the open blocking findings, for
   * the winner's hand-off. */
  answers?: FindingAnswer[];
  /** Plan 06n (A4): the lane's answers to the numbered directive items, for
   * its reviewers and the winner's hand-off. */
  directiveAnswers?: DirectiveAnswer[];
  /** The lane's own `submit_coverage` payload, for the winner's hand-off. */
  coverage?: unknown;
  /** tt 09 (change 3): a consolidator's own report of what it changed beyond
   * the work it took (its submit_phase `deviations`). */
  added?: string[];
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
  /** Plan 06n (A3): the recorded build of a candidate this round already
   * holds (a round stopped mid-way and resumed): its submit_phase payload and
   * start, restored from the lane's own records. Absent keeps `{ lane, sha }`. */
  resumeLane?(round: number, lane: string, sha: string): LaneBuild;
  /** Check one candidate, under the machine-wide lock, one after the other
   * (C2: two candidates' checks never overlap). */
  checkCandidate(round: number, lane: string, sha: string): Promise<LaneCheck>;
  /** exex 01a2: a lane never hands in a red tree. When its candidate fails
   * the checks, the SAME lane's worker gets the failure back on its own
   * candidate and submits again, while its attempt time lasts. The host owns
   * the clock: a lane with no time left returns `{ lane, note }` (no sha) and
   * the red candidate stays the lane's record for this round. Absent keeps
   * the old rule (a failed check ends the lane's round). */
  fixLane?(lane: string, round: number, sha: string, failure: string): Promise<LaneBuild>;
  /** One seat reviews one candidate. 09-2 (async review): called the moment
   * the lane submits, in parallel with the candidate's `checkCandidate`; the
   * review counts only if the check then passes. */
  reviewCandidate(round: number, lane: string, sha: string, seat: string): Promise<void>;
  /** 09-2 (async review): the candidate failed its checks, so its in-flight
   * reviews are worthless. The host ends them (the conductor terminates the
   * review agents); `runRound` ignores whatever they still return, and the
   * reducer ignores a review that lands on a red candidate. Absent lets the
   * reviews run out, uncounted. */
  cancelReviews?(round: number, lane: string, sha: string): void;
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
  /** Plan 06n (A7): the phase's best candidate and the plan items the pick
   * compares each lane's reviews against. Undefined (no best candidate yet,
   * or an unstructured plan) keeps the old pick. */
  pickGuard?(): PickGuard | undefined;
  /** Plan 06n (A3): true once the owner asked the run to stop. `runRound`
   * checks it before lane checks, reviews and the pick, and records nothing
   * after it. */
  stopRequested?(): boolean;
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
  /** Plan 06n (A3): the owner stopped the run mid-round. Nothing after the
   * stop was recorded, and the partial round is left in place for the next
   * start to finish. */
  stopped?: boolean;
}

/** exex 01a2: at most this many fixes of a red candidate per lane per round;
 * past it the red candidate is the lane's record and the next round starts
 * from it. */
export const MAX_LANE_FIXES = 5;

function errorNote(err: unknown): string {
  return String((err as Error)?.message ?? err);
}

/** The per-lane starting points for a repeat of `last`. A round with a
 * passing candidate and no winner could not complete (a missing review or
 * pick vote; `runRound` picks whenever the set is complete), so each lane
 * whose candidate passed its checks restarts from that candidate, and (exex
 * 01a2) a lane whose candidate was still red when its time ran out continues
 * from that red candidate; only a lane with no candidate at all keeps `base`.
 * Derived from the round record, so it survives a conductor restart.
 *
 * Plan 06n (A7/R9b): a round that picked no lane because every passing lane
 * lost an item the best candidate met restarts EVERY lane from the best
 * candidate, never from its own regressing candidate. */
export function repeatLaneBases(last: RoundRecord | undefined, guard?: PickGuard): Record<string, string> | undefined {
  if (!last || last.picked) return undefined;
  if (everyLaneLost(last, guard)) return Object.fromEntries(last.lanes.map((lane) => [lane, guard!.best.sha]));
  const out: Record<string, string> = {};
  for (const c of passingCandidates(last)) if (typeof c.sha === "string" && c.sha.length > 0) out[c.lane] = c.sha;
  // exex 01a2: a lane whose candidate was still red when its time ran out
  // continues from that candidate, never from the round's base: restarting
  // threw away the lanes' whole removal three rounds running.
  for (const c of last.candidates) {
    if (out[c.lane] === undefined && c.ok === false && typeof c.sha === "string" && c.sha.length > 0) out[c.lane] = c.sha;
  }
  return Object.keys(out).length > 0 ? out : undefined;
}

/** One round: K lanes from one base, one winner. See the module comment. */
export async function runRound(host: LaneHost, ctx: RoundContext): Promise<RoundOutcome> {
  const lanes = [...ctx.lanes];
  // Plan 06n (A3): a round the conductor stopped mid-way is resumed, not
  // restarted. Its candidates and check results are kept; ROUND_STARTED is
  // not emitted again (which would wipe them).
  const existing = host.rounds().find((r) => r.round === ctx.round);
  if (!existing) host.emit({ type: "ROUND_STARTED", round: ctx.round, base: ctx.base, lanes });

  // 1. Each lane gets its own worktree, worker and sweep, on the same prompt.
  //    The lanes run in parallel; one lane's failure never stops the other
  //    (A2), so each is caught into a note.
  const prompt = host.lanePrompt(ctx.round, ctx.base);

  // 09-2 (async review): a candidate's reviews start the moment its lane
  // submits it, in parallel with its checks — reviews are model calls, so
  // starting them early costs no local CPU, and the seats no longer sit idle
  // through 10-15 minutes of checks. Each lane holds at most one live review
  // set, for its current candidate. A failed check cancels that set: its
  // results never count (the reducer drops and ignores a red candidate's
  // reviews), and the lane's fixed candidate gets a fresh set at its own
  // submission. The pick still waits for every passing candidate's set.
  const reviewSets = new Map<string, { sha: string; done: Promise<string | undefined> }>();
  const cancelled: Promise<unknown>[] = [];
  const startReviews = (lane: string, sha: string): void => {
    const done = Promise.all(
      // The three seats review a candidate IN PARALLEL (the discovery
      // barrier needs all three in flight).
      host.seats.map(async (seat) => {
        try {
          await host.reviewCandidate(ctx.round, lane, sha, seat);
          return undefined;
        } catch (err) {
          return `${seat}'s review of ${candidateLabel(ctx.round, lane)} failed: ${errorNote(err)}`;
        }
      }),
    ).then((results) => results.find((r): r is string => r !== undefined));
    reviewSets.set(lane, { sha, done });
  };
  const cancelReviews = (lane: string): void => {
    const set = reviewSets.get(lane);
    if (!set) return;
    reviewSets.delete(lane);
    cancelled.push(set.done);
    try {
      host.cancelReviews?.(ctx.round, lane, set.sha);
    } catch {
      // A cancel that cannot run leaves the reviews to run out; their
      // results are discarded either way.
    }
  };
  // One check of one candidate whose reviews are already in flight. A failed
  // check cancels them; a passing one leaves them to finish and count.
  const checkWithReviews = async (lane: string, sha: string): Promise<LaneCheck> => {
    let check: LaneCheck;
    try {
      check = await host.checkCandidate(ctx.round, lane, sha);
    } catch (err) {
      check = { ok: false, note: `the check could not run: ${errorNote(err)}` };
    }
    if (!check.ok) cancelReviews(lane);
    return check;
  };

  // 1. Each lane gets its own worktree, worker and sweep, on the same prompt.
  //    The lanes run in parallel; one lane's failure never stops the other
  //    (A2), so each is caught into a note.
  // Plan 06l (A1): a lane's checks start the moment THAT lane submits, while
  // the other lane is still working — not after both have. `checkCandidate`
  // takes the machine-wide lock, so the two checks still never overlap; only
  // the wait for a slow sibling is removed. A lane that produced no candidate
  // records no candidate at all: its note is what the status shows.
  const builds = await Promise.all(
    lanes.map(async (lane): Promise<LaneBuild> => {
      const prior = existing?.candidates.find((c) => c.lane === lane);
      if (prior && typeof prior.sha === "string" && prior.sha.length > 0) {
        // Plan 06n (A3): a kept candidate brings back its recorded build, so
        // its reviews and the hand-off see the lane's own payload; only an
        // unchecked one is checked.
        const sha = prior.sha;
        const kept: LaneBuild = host.resumeLane?.(ctx.round, lane, sha) ?? { lane, sha };
        if (prior.ok === false || host.stopRequested?.()) return kept;
        // 09-2: a kept candidate that is not red is reviewed from now on —
        // while its check runs, when it still needs one.
        startReviews(lane, sha);
        if (prior.ok === undefined) {
          const check = await checkWithReviews(lane, sha);
          if (host.stopRequested?.()) return kept;
          host.emit({
            type: "CANDIDATE_CHECKED",
            round: ctx.round,
            lane,
            ok: check.ok,
            ...(check.note ? { note: check.note } : {}),
          });
        }
        return kept;
      }
      // Plan 06n (A3): nothing new starts after a requested stop.
      if (host.stopRequested?.()) return { lane, note: "the run was stopped" };
      let build: LaneBuild;
      try {
        build = await host.buildLane(lane, ctx.round, ctx.laneBases?.[lane] ?? ctx.base, prompt);
      } catch (err) {
        build = { lane, note: `the lane failed: ${errorNote(err)}` };
      }
      if (typeof build.sha === "string" && build.sha.length > 0) {
        if (host.stopRequested?.()) return build;
        host.emit({ type: "CANDIDATE_SUBMITTED", round: ctx.round, lane: build.lane, sha: build.sha });
        startReviews(build.lane, build.sha);
        let check = await checkWithReviews(build.lane, build.sha);
        if (host.stopRequested?.()) return build;
        host.emit({
          type: "CANDIDATE_CHECKED",
          round: ctx.round,
          lane: build.lane,
          ok: check.ok,
          ...(check.note ? { note: check.note } : {}),
        });
        // exex 01a2: a red tree is never the lane's hand-in while it still
        // has time. Its worker fixes the failure on its own candidate and
        // submits again; each new commit is submitted, reviewed (09-2) and
        // checked like the first. Only a green candidate, or a lane out of
        // time, ends the loop.
        let fixes = 0;
        while (!check.ok && host.fixLane && fixes < MAX_LANE_FIXES && !host.stopRequested?.()) {
          fixes += 1;
          let fixed: LaneBuild;
          try {
            fixed = await host.fixLane(build.lane, ctx.round, build.sha!, check.note ?? "the checks failed");
          } catch (err) {
            fixed = { lane: build.lane, note: `the fix attempt failed: ${errorNote(err)}` };
          }
          if (typeof fixed.sha !== "string" || fixed.sha.length === 0 || fixed.sha === build.sha) break;
          build = fixed;
          if (host.stopRequested?.()) return build;
          host.emit({ type: "CANDIDATE_SUBMITTED", round: ctx.round, lane: build.lane, sha: build.sha! });
          startReviews(build.lane, build.sha!);
          check = await checkWithReviews(build.lane, build.sha!);
          if (host.stopRequested?.()) return build;
          host.emit({
            type: "CANDIDATE_CHECKED",
            round: ctx.round,
            lane: build.lane,
            ok: check.ok,
            ...(check.note ? { note: check.note } : {}),
          });
        }
      } else {
        if (host.stopRequested?.()) return build;
        host.emit({
          type: "CANDIDATE_CHECKED",
          round: ctx.round,
          lane: build.lane,
          ok: false,
          note: build.note ?? "the lane produced no candidate",
        });
      }
      return build;
    }),
  );
  // Plan 06n (A3): nothing after a requested stop is recorded, so the
  // reviews still in flight are ended too.
  if (host.stopRequested?.()) {
    for (const lane of [...reviewSets.keys()]) cancelReviews(lane);
    return { round: ctx.round, base: ctx.base, lanes, builds, stopped: true };
  }

  const record = host.rounds().find((r) => r.round === ctx.round);
  const passing = record ? passingCandidates(record) : [];

  // 3. M, A and B each review every passing candidate — K × N reviews. A
  //    review that cannot be taken is NOT a candidate with fewer reviews: the
  //    round fails and repeats, so no winner is ever handed off with an
  //    incomplete set.
  // Plan 06l (A1): every passing candidate's reviews run AT THE SAME TIME —
  // each seat reviews the two lanes as two separate turns (distinct agents,
  // distinct per-candidate discovery barriers). 09-2: they started at each
  // candidate's submission; here the round waits for them. A passing
  // candidate without its own live set (none is expected) is reviewed now.
  // A cancelled set is waited for too, so no review of this round outlives it.
  const reviewFailures = await Promise.all(
    passing.map((candidate) => {
      const set = reviewSets.get(candidate.lane);
      if (!set || set.sha !== candidate.sha) startReviews(candidate.lane, candidate.sha as string);
      return reviewSets.get(candidate.lane)!.done;
    }),
  );
  await Promise.allSettled(cancelled);
  let failure = reviewFailures.find((r): r is string => r !== undefined);
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
  if (!failure && passing.length > 1 && host.stopRequested?.()) {
    return { round: ctx.round, base: ctx.base, lanes, builds, stopped: true };
  }
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
      const decided = afterPick && passing.length === 2 ? pickWinner(afterPick, host.seats, host.leader, host.pickGuard?.()) : undefined;
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
  const winner = finalRecord ? pickWinner(finalRecord, host.seats, host.leader, host.pickGuard?.()) : undefined;
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

// ---------------------------------------------------------------------------
// tt 09 (change 3): combine the lanes
// ---------------------------------------------------------------------------
//
// exex 01a2: the two lanes specialised — lane a finished the production
// removal, lane b the test conversion — and three rounds never converged:
// each round kept ONE winner and the next round started every lane from it,
// so the losing lane's half was thrown away every time. Now, after a round
// whose pick the phase did not accept, a consolidator starts from the winner
// and takes the other lane's work for exactly the plan items that lane met
// and the winner did not (07's reduce step). Its commit goes through the round
// checks with the lane fix loop, and a green one is the next round's base for
// every lane. The rules below are pure; the conductor runs the agent and the
// checks through `ConsolidationHost`.

/** True when a phase combines its lanes: it runs two or more lanes and its
 * plan did not say `#+TT_CONSOLIDATE: off`. A single-worker phase never
 * consolidates. */
export function consolidationEnabled(contract: Pick<PhaseContract, "workers" | "consolidate">): boolean {
  return isLaneRound(contract) && contract.consolidate !== false;
}

/** What one consolidation does: start from the round's winner and take each
 * listed lane's work for its listed items. */
export interface ConsolidationPlan {
  round: number;
  winner: { lane: string; sha: string };
  takes: ConsolidationTake[];
}

/** The trigger rule. A round that picked a winner (and whose pick was not
 * rejected as a regression) consolidates when another PASSING lane met a plan
 * item the winner did not — met meaning a strict majority of the seats said
 * `met` (or `fits` for an A), `laneMetItems`'s rule. Each such item is taken
 * once, from the first lane (in lane order) that met it. Undefined when no
 * lane met anything the winner lacks: nothing changes. Derived from the round
 * record alone, so it replays after a restart. The caller decides that the
 * phase is repairing (a round is only consolidated once its pick was not
 * accepted). */
export function consolidationPlan(
  round: RoundRecord | undefined,
  itemIds: readonly string[],
  seatCount: number,
): ConsolidationPlan | undefined {
  if (!round?.picked || round.regressed || itemIds.length === 0) return undefined;
  const picked = round.picked;
  const winner = round.candidates.find((c) => c.lane === picked.lane && c.sha === picked.sha);
  if (!winner) return undefined;
  const winnerMet = laneMetItems(winner, itemIds, seatCount);
  const taken = new Set<string>();
  const takes: ConsolidationTake[] = [];
  for (const c of passingCandidates(round)) {
    if (c.lane === picked.lane) continue;
    const met = laneMetItems(c, itemIds, seatCount);
    const items = itemIds.filter((id) => met.has(id) && !winnerMet.has(id) && !taken.has(id));
    if (items.length === 0) continue;
    for (const id of items) taken.add(id);
    takes.push({ lane: c.lane, sha: c.sha as string, items });
  }
  return takes.length > 0 ? { round: round.round, winner: { lane: picked.lane, sha: picked.sha }, takes } : undefined;
}

/** The seats' evidence that one candidate met one item: `<seat>: <evidence>`
 * for each seat whose verdict was `met` (or `fits`), in `seats` order when
 * given (reviews are recorded in the order they arrived). The consolidator's
 * prompt quotes it so it knows where the work it takes lives. */
export function itemEvidence(candidate: LaneCandidateRecord, id: string, seats?: readonly string[]): string[] {
  const out: string[] = [];
  const rank = (seat: string) => (seats && seats.includes(seat) ? seats.indexOf(seat) : Number.MAX_SAFE_INTEGER);
  const reviews = [...(candidate.reviews ?? [])].sort((a, b) => rank(a.seat) - rank(b.seat));
  for (const r of reviews) {
    const item = (r.review.items ?? []).find((i) => i.id === id);
    const arch = (r.review.arch ?? []).find((a) => a.id === id);
    const hit = item?.verdict === "met" ? item : arch?.verdict === "fits" ? arch : undefined;
    if (hit) out.push(`${r.seat}: ${hit.evidence}`);
  }
  return out;
}

/** Base selection: the commit every lane of the round after `beforeRound`'s
 * last pick starts from, when that pick was consolidated — the latest picked
 * round (before `beforeRound`) whose consolidation passed its checks and
 * started from that very pick. A round that picked no lane in between does
 * not change it (its lanes started from the same consolidated base). Undefined
 * when the latest pick was not consolidated, its consolidation failed, or the
 * pick regressed: the caller keeps today's base (the winner, or the best
 * candidate). */
export function consolidatedBase(rounds: readonly RoundRecord[] | undefined, beforeRound = Number.POSITIVE_INFINITY): string | undefined {
  let latest: RoundRecord | undefined;
  for (const r of rounds ?? []) {
    if (r.round >= beforeRound || !r.picked) continue;
    if (!latest || r.round > latest.round) latest = r;
  }
  if (!latest || latest.regressed) return undefined;
  const c = latest.consolidated;
  return c?.ok && typeof c.sha === "string" && c.sha.length > 0 && c.from === latest.picked!.sha ? c.sha : undefined;
}

/** Plan 07c (D4.7): the fix round that repairs `beforeRound`'s predecessor,
 * when its merged candidate exists. Like `consolidatedBase`: the latest
 * picked round before `beforeRound` whose fix step produced a candidate. The
 * next round is then a single-lane round on that candidate, checked from the
 * fix round's base. Undefined when the latest pick was not fixed, its fixers
 * all failed, or the pick regressed. */
export function fixRoundCandidate(
  rounds: readonly RoundRecord[] | undefined,
  beforeRound = Number.POSITIVE_INFINITY,
): { candidate: string; base: string } | undefined {
  let latest: RoundRecord | undefined;
  for (const r of rounds ?? []) {
    if (r.round >= beforeRound || !r.picked) continue;
    if (!latest || r.round > latest.round) latest = r;
  }
  if (!latest || latest.regressed) return undefined;
  const fixed = latest.fixed;
  if (!fixed || typeof fixed.candidate !== "string" || fixed.candidate.length === 0) return undefined;
  return { candidate: fixed.candidate, base: fixed.base };
}

/** How a consolidated commit is named in the views: `C2-a+b` (round 2's
 * winner a, with lane b's items). */
export function consolidatedLabel(round: number, winnerLane: string, takes: readonly ConsolidationTake[]): string {
  return `${candidateLabel(round, winnerLane)}+${takes.map((t) => t.lane).join("+")}`;
}

/** The status/round line of a round's combine step, or undefined when the
 * round was not consolidated:
 * `consolidated C2-a+b 1234567 = C2-a + lane b's items R6, C3`, or why it
 * failed and that the next round starts from the winner. */
export function consolidationLine(round: RoundRecord): string | undefined {
  const c = round.consolidated;
  if (!c) return undefined;
  const winner = candidateLabel(round.round, c.lane);
  const took = c.took.map((t) => `lane ${t.lane}'s items ${t.items.join(", ")}`).join(" + ");
  if (c.ok && c.sha) {
    const fixes = c.fixes ? ` (after ${c.fixes} fix${c.fixes === 1 ? "" : "es"})` : "";
    return `consolidated ${consolidatedLabel(round.round, c.lane, c.took)} ${c.sha.slice(0, 7)} = ${winner} + ${took}${fixes}`;
  }
  return `consolidation of ${winner} + ${took} failed: ${c.note ?? "no reason recorded"} — the next round starts from ${winner}`;
}

/** The status panel's combine line: the latest round's own consolidation,
 * or — once the next round has started from it — the consolidation that
 * round's base came from. Undefined when neither applies. */
export function latestConsolidationLine(rounds: readonly RoundRecord[] | undefined): string | undefined {
  const list = [...(rounds ?? [])].sort((a, b) => a.round - b.round);
  const latest = list[list.length - 1];
  if (!latest) return undefined;
  const own = consolidationLine(latest);
  if (own) return own;
  const from = list.find((r) => r.round < latest.round && r.consolidated?.ok && r.consolidated.sha === latest.base);
  const line = from ? consolidationLine(from) : undefined;
  return line ? `round ${latest.round} starts from ${line}` : undefined;
}

/** Everything `runConsolidation` asks the conductor to do. */
export interface ConsolidationHost {
  /** Run the consolidator from the winner and freeze its commit, or say why
   * it produced none. */
  build(): Promise<LaneBuild>;
  /** The round checks on the consolidated commit, under the machine-wide
   * lock, exactly as a lane candidate's. */
  check(sha: string): Promise<LaneCheck>;
  /** The consolidator fixes its own red commit (its session, its worktree),
   * like `LaneHost.fixLane`; no sha means it could not. */
  fix(sha: string, failure: string): Promise<LaneBuild>;
  stopRequested?(): boolean;
}

export interface ConsolidationOutcome {
  ok: boolean;
  /** The green consolidated commit. */
  sha?: string;
  /** Why the consolidation failed; the next round starts from the winner. */
  note?: string;
  fixes: number;
  /** What the consolidator says it changed beyond the taken work. */
  added?: string[];
  /** The owner stopped the run mid-way: nothing is recorded. */
  stopped?: boolean;
}

/** The combine step's sequence: build, check, and the lane's bounded fix loop
 * (at most MAX_LANE_FIXES fixes, ending early on a fix that changes nothing).
 * A consolidation that produced nothing or stayed red is a failure with its
 * reason — the caller then keeps today's behaviour (the next round starts
 * from the winner). */
export async function runConsolidation(host: ConsolidationHost): Promise<ConsolidationOutcome> {
  if (host.stopRequested?.()) return { ok: false, fixes: 0, stopped: true };
  let build: LaneBuild;
  try {
    build = await host.build();
  } catch (err) {
    build = { lane: "consolidator", note: `the consolidator failed: ${errorNote(err)}` };
  }
  if (host.stopRequested?.()) return { ok: false, fixes: 0, stopped: true };
  if (typeof build.sha !== "string" || build.sha.length === 0) {
    return { ok: false, fixes: 0, note: build.note ?? "the consolidator produced no commit" };
  }
  const check = async (sha: string): Promise<LaneCheck> => {
    try {
      return await host.check(sha);
    } catch (err) {
      return { ok: false, note: `the check could not run: ${errorNote(err)}` };
    }
  };
  let result = await check(build.sha);
  let fixes = 0;
  let fixNote: string | undefined;
  while (!result.ok && fixes < MAX_LANE_FIXES && !host.stopRequested?.()) {
    fixes += 1;
    let fixed: LaneBuild;
    try {
      fixed = await host.fix(build.sha!, result.note ?? "the checks failed");
    } catch (err) {
      fixed = { lane: "consolidator", note: `the fix attempt failed: ${errorNote(err)}` };
    }
    if (typeof fixed.sha !== "string" || fixed.sha.length === 0 || fixed.sha === build.sha) {
      fixNote = fixed.note;
      break;
    }
    build = { ...fixed, added: fixed.added ?? build.added };
    result = await check(build.sha!);
  }
  if (host.stopRequested?.()) return { ok: false, fixes, stopped: true };
  if (result.ok) return { ok: true, sha: build.sha, fixes, ...(build.added ? { added: build.added } : {}) };
  return {
    ok: false,
    fixes,
    note: `its commit ${build.sha!.slice(0, 7)} failed the round checks: ${result.note ?? "a check command failed"}${fixNote ? ` (${fixNote})` : ""}`,
  };
}
