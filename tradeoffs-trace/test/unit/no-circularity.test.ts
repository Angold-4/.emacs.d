// no-circularity (phase-0 exit gate, design §6.3's closing claim; round-1
// review item 2):
//
//  1. A property test with a seeded PRNG (mulberry32, no deps). The ONLY
//     thing that decides what happens next is `next(state)` — the driver
//     never inspects `state.phase.phase` to choose an event itself. Each
//     action next() emits is answered by a simulated environment that
//     randomly passes, fails, times out, is interrupted, or (for
//     REVIEWING) casts random ballots (including contract objections),
//     raises/confirms/withdraws findings, and discloses random decisions —
//     but always eventually succeeds, so every run reaches a terminal
//     state within a bound computed from design §8.3
//     (1 + 3 rounds, + 3 per owner correction, times a per-round event
//     count), not a bare constant.
//  2. `accept` never reads a fact produced only by acceptance or later
//     (Proxy instrumentation), and its result is identical with and
//     without post-ACCEPTED events — checked both on a hand-built state
//     and, per round-1 review, on states the random runs actually produce.

import assert from "node:assert/strict";
import { test } from "node:test";
import { accept } from "../../src/core/predicate.ts";
import { next } from "../../src/core/next.ts";
import { isBudgetGateRequest, isRepairForcingOption } from "../../src/core/owner-requests.ts";
import { reduce } from "../../src/core/reduce.ts";
import type {
  Ballot,
  ContractVersion,
  Correction,
  Decision,
  DecisionClass,
  Event,
  Finding,
  PhaseState,
  Review,
  Reviewer,
  State,
} from "../../src/core/types.ts";
import { approvingReview, baseState, CV } from "./helpers.ts";

// mulberry32: a small, deterministic, dependency-free PRNG.
function mulberry32(seed: number): () => number {
  let a = seed >>> 0;
  return function () {
    a |= 0;
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

type Terminal = "DONE" | "BLOCKED" | "AWAITING_OWNER" | "PAUSED";

const K = CV();

function step(state: State, event: Event): State {
  const result = reduce(state, event);
  if (!result.ok) {
    throw new Error(`reduce rejected ${event.type} in phase ${state.phase.phase}/${state.run}: ${result.reason}`);
  }
  return result.state;
}

// design §8.3: "each phase has at most 1 + 3 candidate rounds, plus 3 per
// owner correction". The property test injects at most MAX_CORRECTIONS
// REVISE-originated corrections and MAX_GRANTS budget-request grants (each
// behaves exactly like a correction's allowance: +3 rounds), and bounds
// each round's event count generously (worker dispatch/submit, freeze,
// checks, probe, three reviews with up to one retimeout each, resolve,
// accept, publish — with slack for interruptions).
const MAX_CORRECTIONS = 2;
const MAX_GRANTS = 2;
const MAX_OWNER_INTERVENTIONS = 8; // total AWAITING_OWNER commands per run — item 3's "owner budget"
const EVENTS_PER_ROUND = 24;
const MAX_STEPS = (1 + 3 + 3 * (MAX_CORRECTIONS + MAX_GRANTS)) * EVENTS_PER_ROUND;

let idCounter = 0;
function freshId(prefix: string): string {
  idCounter += 1;
  return `${prefix}-${idCounter}`;
}

function randomDecisionClass(rng: () => number): DecisionClass {
  const r = rng();
  if (r < 0.4) return "detail";
  if (r < 0.8) return "delegated";
  return "reserved";
}

function randomDecision(rng: () => number, candidateSha: string, contractVersion: ContractVersion): Decision {
  return {
    id: freshId("D"),
    version: 1,
    phaseId: "p1",
    source: "worker",
    class: randomDecisionClass(rng),
    choice: "a randomly generated trade-off",
    whyItMatters: "exercises the vote/override/owner-request paths",
    alternatives: [{ option: "alternative", consequence: "some consequence" }],
    recommendation: { choice: "the chosen option", reason: "plausible reason" },
    boundCandidateSha: candidateSha,
    boundContractVersion: contractVersion,
  };
}

/** Recorded so the "accept is stable" property can be checked against
 * states the random runs actually produced, not only a hand-built one. */
interface AcceptRecord {
  phase: PhaseState;
  candidateSha: string;
  contractVersion: ContractVersion;
  resultAtAccept: boolean;
}

interface RunResult {
  terminal: Terminal;
  steps: number;
  finalState: State;
  acceptRecords: AcceptRecord[];
}

/** Drives a phase using ONLY `next(state)` to decide what happens; a
 * simulated environment answers each emitted action. */
function driveOnce(rng: () => number): RunResult {
  let state = baseState({ phase: "READY" });
  let candidateCounter = 0;
  let correctionsUsed = 0;
  let grantsUsed = 0;
  let ownerInterventions = 0;
  const acceptRecords: AcceptRecord[] = [];
  // Items settled while AWAITING_OWNER (round-3 review item 3's invariant).
  // Cleared on every new candidate: design §7.2 discards all prior evidence
  // (ballots, reviews) for a new candidate, so a settlement bound to the
  // OLD candidate no longer applies and a fresh failing vote on the NEW
  // candidate is not a "regenerated request for a settled item" — it's a
  // new item.
  const settledKeys = new Set<string>();

  for (let i = 0; i < MAX_STEPS; i++) {
    // Run budget: rarely pause, and rarely leave it paused as a terminal
    // outcome — otherwise resume immediately and keep going.
    if (state.run === "RUN_ACTIVE" && rng() < 0.004) {
      const paused = reduce(state, { type: "RUN_BUDGET_EXCEEDED" });
      if (paused.ok) {
        if (rng() < 0.5) return { terminal: "PAUSED", steps: i, finalState: paused.state, acceptRecords };
        state = step(paused.state, { type: "RUN_RESUMED" });
      }
    }

    const actions = next(state);

    if (actions.length === 0) {
      if (state.phase.phase === "DONE" || state.phase.phase === "BLOCKED") {
        return { terminal: state.phase.phase, steps: i, finalState: state, acceptRecords };
      }
      if (state.phase.phase === "AWAITING_OWNER") {
        // design §6.1: "AWAITING_OWNER ... leaves only through an owner
        // command". Item 1 (round 2) guarantees at least one open request;
        // the simulated owner picks one at random and answers it through
        // whichever command fits it — OWNER_REQUEST_RESOLVED (using the
        // request's own option ids, including the settling ones and
        // reject_and_repair/withdraw), FINDING_ACCEPTED_BY_OWNER,
        // OVERRIDE_CAST, AMEND (with resolvesFindingIds, for a `contract`
        // finding) or REVISE — until its intervention budget (item 3) is
        // spent, which is the only reason AWAITING_OWNER is an allowed
        // terminal.
        const open = state.phase.ownerRequests.filter((r) => r.status === "open");
        if (open.length === 0) {
          throw new Error("AWAITING_OWNER entered with no open owner request (round-2 review item 1)");
        }
        // round-3 review item 3: an owner settling choice must never be
        // followed by a regenerated request for the same item.
        for (const r of open) {
          const key = r.linkedDecisionId ? `d:${r.linkedDecisionId}` : r.linkedFindingId ? `f:${r.linkedFindingId}` : undefined;
          if (key && settledKeys.has(key)) {
            throw new Error(`item ${key} was settled but a request (${r.id}) was regenerated for it`);
          }
        }
        if (ownerInterventions >= MAX_OWNER_INTERVENTIONS || !state.phase.candidate) {
          return { terminal: "AWAITING_OWNER", steps: i, finalState: state, acceptRecords };
        }
        ownerInterventions += 1;
        const request = open[Math.floor(rng() * open.length)];
        const C = state.phase.candidate.sha;
        const Kc = state.phase.contract.contractVersion;

        if (isBudgetGateRequest(request)) {
          const canGrant = grantsUsed < MAX_GRANTS;
          const option = canGrant && rng() < 0.85 ? "grant" : "stop";
          if (option === "grant") grantsUsed += 1;
          state = step(state, {
            type: "OWNER_REQUEST_RESOLVED",
            requestId: request.id,
            option,
            boundCandidateSha: C,
            boundContractVersion: Kc,
            boundRecordVersion: request.version,
          });
          continue;
        }

        // A `contract` finding's request offers only "repair" — its other
        // disposition is amending the contract (design §4.2/§4.3), not one
        // of the request's own options.
        const linkedFinding = request.linkedFindingId ? state.phase.findings.find((f) => f.id === request.linkedFindingId) : undefined;
        if (linkedFinding?.kind === "contract" && rng() < 0.5) {
          state = step(state, {
            type: "AMEND",
            replacingContractVersion: Kc,
            newContractVersion: { snapshot: Kc.snapshot + 1, sectionSha256: "b".repeat(64) },
            resolvesFindingIds: [linkedFinding.id],
          });
          // A new contract version invalidates any settlement bound to the
          // old one (decisionSettled requires the resolution's K to match).
          settledKeys.clear();
          settledKeys.add(`f:${linkedFinding.id}`);
          continue;
        }

        const decisionVersion = (id: string) => state.phase.decisions.find((d) => d.id === id)?.version ?? 1;
        const findingVersion = (id: string) => state.phase.findings.find((f) => f.id === id)?.version ?? 1;

        const choice = rng();
        // A `contract` finding's only disposition is AMEND (design §4.2),
        // handled by the branch above; direct acceptance is valid only for
        // a non-contract finding.
        if (
          request.origin === "open_finding" &&
          request.linkedFindingId &&
          linkedFinding?.kind !== "contract" &&
          choice < 0.25
        ) {
          // FINDING_ACCEPTED_BY_OWNER directly, bypassing the request's own
          // "accept_risk" option — an equally valid way to settle it.
          state = step(state, {
            type: "FINDING_ACCEPTED_BY_OWNER",
            findingId: request.linkedFindingId,
            scope: "accepted by the simulated owner",
            by: "owner",
            boundCandidateSha: C,
            boundContractVersion: Kc,
            boundRecordVersion: findingVersion(request.linkedFindingId),
          });
          settledKeys.add(`f:${request.linkedFindingId}`);
        } else if (request.origin === "failed_vote" && request.linkedDecisionId && choice < 0.25) {
          // OVERRIDE_CAST only settles a `delegated` decision (predicate.ts's
          // decisionSettled checks `phase.overrides` for `failed_vote`, not
          // for `reserved_decision` — a `reserved` decision is settled only
          // by its own request's "approve" option).
          state = step(state, {
            type: "OVERRIDE_CAST",
            override: {
              decisionId: request.linkedDecisionId,
              vote: "approve",
              boundCandidateSha: C,
              boundContractVersion: Kc,
              boundRecordVersion: decisionVersion(request.linkedDecisionId),
            },
          });
          settledKeys.add(`d:${request.linkedDecisionId}`);
        } else if (choice < 0.5 && correctionsUsed < MAX_CORRECTIONS && (request.linkedDecisionId || request.linkedFindingId)) {
          const targetId = (request.linkedDecisionId ?? request.linkedFindingId)!;
          const recordVersion = request.linkedDecisionId ? decisionVersion(targetId) : findingVersion(targetId);
          correctionsUsed += 1;
          state = step(state, {
            type: "REVISE",
            correctionId: freshId("C"),
            targetRecordId: targetId,
            correctionText: "owner correction from AWAITING_OWNER",
            contractChange: false,
            boundCandidateSha: C,
            boundContractVersion: Kc,
            boundRecordVersion: recordVersion,
          });
        } else {
          // Resolve through one of the request's own option ids.
          const option = request.options[Math.floor(rng() * request.options.length)];
          const settles =
            (request.origin === "failed_vote" && option.id === "accept_as_implemented") ||
            (request.origin === "reserved_decision" && option.id === "approve") ||
            (request.origin === "open_finding" && option.id === "accept_risk");
          state = step(state, {
            type: "OWNER_REQUEST_RESOLVED",
            requestId: request.id,
            option: option.id,
            note: option.id === "accept_risk" ? "accepted by the simulated owner" : undefined,
            boundCandidateSha: C,
            boundContractVersion: Kc,
            boundRecordVersion: request.version,
          });
          if (settles) {
            const key = request.linkedDecisionId ? `d:${request.linkedDecisionId}` : `f:${request.linkedFindingId}`;
            settledKeys.add(key);
          }
        }
        continue;
      }
      throw new Error(`next() returned no actions in a non-terminal state: phase=${state.phase.phase}`);
    }

    for (const action of actions) {
      switch (action.type) {
        case "start_attempt": {
          state = step(state, { type: "ATTEMPT_STARTED" });
          break;
        }

        case "dispatch_worker": {
          state = step(state, { type: "ACTION_STARTED", action: "dispatch_worker", actionId: freshId("a") });
          const r = rng();
          if (r < 0.08) state = step(state, { type: "ATTEMPT_TIMED_OUT" });
          else if (r < 0.13) state = step(state, { type: "ATTEMPT_NO_SUBMISSION" });
          else if (r < 0.16) state = step(state, { type: "ATTEMPT_INTERRUPTED" });
          else {
            const decisions: Decision[] = [];
            const count = rng() < 0.5 ? 0 : rng() < 0.7 ? 1 : 2;
            for (let d = 0; d < count; d++) {
              // boundCandidateSha is filled in once FREEZE_COMPLETED assigns
              // the real candidate; the worker doesn't know it yet, so this
              // uses a placeholder the freeze doesn't need to match (decisions
              // aren't candidate-checked at disclosure time, only at vote time).
              decisions.push(randomDecision(rng, "pending", state.phase.contract.contractVersion));
            }
            state = step(state, { type: "SUBMIT_PHASE", decisions });
          }
          break;
        }

        case "freeze": {
          state = step(state, { type: "ACTION_STARTED", action: "freeze", actionId: freshId("a") });
          const r = rng();
          if (r < 0.06) state = step(state, { type: "FREEZE_TIMED_OUT" });
          else if (r < 0.1) state = step(state, { type: "FREEZE_INTERRUPTED" });
          else {
            candidateCounter += 1;
            const sha = `C${candidateCounter}`;
            state = step(state, { type: "FREEZE_COMPLETED", candidateSha: sha });
            // A new candidate discards all prior evidence (design §7.2), so
            // an earlier settlement no longer applies to it.
            settledKeys.clear();
            // Backfill the placeholder candidate on freshly disclosed decisions.
            state = {
              ...state,
              phase: {
                ...state.phase,
                decisions: state.phase.decisions.map((d) =>
                  d.boundCandidateSha === "pending" ? { ...d, boundCandidateSha: sha } : d,
                ),
              },
            };
          }
          break;
        }

        case "run_checks": {
          state = step(state, { type: "ACTION_STARTED", action: "run_checks", actionId: freshId("a") });
          const r = rng();
          if (r < 0.08) state = step(state, { type: "CHECKS_INTERRUPTED" });
          else if (r < 0.2) state = step(state, { type: "CHECKS_FAILED" });
          else state = step(state, { type: "CHECKS_PASSED" });
          break;
        }

        case "dispatch_probe": {
          state = step(state, { type: "ACTION_STARTED", action: "dispatch_probe", actionId: freshId("a") });
          const r = rng();
          if (r < 0.08) state = step(state, { type: "PROBE_INTERRUPTED" });
          else if (r < 0.2) state = step(state, { type: "PROBE_FAILED", evidence: "simulated conflict" });
          else state = step(state, { type: "PROBE_PASSED", probedI: `I${candidateCounter}` });
          break;
        }

        case "dispatch_review": {
          const reviewer = action.reviewer as Reviewer;
          state = step(state, { type: "ACTION_STARTED", action: "dispatch_review", actionId: freshId("a"), reviewer });
          const alreadyTimedOut = Boolean(state.phase.reviews[reviewer]?.timedOutOnce);
          if (!alreadyTimedOut && rng() < 0.12) {
            state = step(state, { type: "REVIEW_TIMED_OUT", reviewer });
            break;
          }

          const C = state.phase.candidate!.sha;
          const Kc = state.phase.contract.contractVersion;

          // Confirm/withdraw any of this reviewer's own OPEN findings raised
          // on an earlier candidate (design §4.2: only the raising reviewer).
          const findingStatements: Review["findingStatements"] = [];
          for (const f of state.phase.findings) {
            if (f.raisedBy !== reviewer || f.status !== "open" || f.boundCandidateSha === C) continue;
            const disposition = rng() < 0.6 ? "confirm" : "withdraw";
            findingStatements.push({ findingId: f.id, status: disposition });
          }

          // Occasionally raise a new finding (defect), if this reviewer
          // hasn't already got one open on the current candidate.
          const alreadyRaisedHere = state.phase.findings.some(
            (f) => f.raisedBy === reviewer && f.status === "open" && f.boundCandidateSha === C,
          );
          if (!alreadyRaisedHere && rng() < 0.1) {
            const finding: Finding = {
              id: freshId("F"),
              version: 1,
              phaseId: "p1",
              kind: "defect",
              severity: "blocking",
              evidence: "simulated defect evidence",
              raisedBy: reviewer,
              status: "open",
              boundCandidateSha: C,
            };
            state = step(state, { type: "FINDING_RAISED", finding });
          }

          // Cast a ballot on each votable decision not yet voted by this
          // reviewer at its current version.
          for (const d of state.phase.decisions) {
            if (d.class !== "delegated") continue;
            if (d.boundCandidateSha !== C) continue;
            const already = state.phase.ballots.some(
              (b) => b.decisionId === d.id && b.reviewer === reviewer && b.boundRecordVersion === d.version,
            );
            if (already) continue;
            const contractObjection = rng() < 0.08;
            const ballot: Ballot = {
              reviewer,
              decisionId: d.id,
              vote: contractObjection || rng() < 0.75 ? "approve" : "reject",
              rationale: "simulated rationale",
              evidence: ["src/simulated.ts:1"],
              boundCandidateSha: C,
              boundContractVersion: Kc,
              boundRecordVersion: d.version,
              contractObjection,
            };
            state = step(state, { type: "BALLOT_CAST", ballot });
          }

          // Honor/dishonor any open corrections in this review.
          const correctionStatements: Review["correctionStatements"] = state.phase.corrections
            .filter((c) => c.status === "open")
            .map((c) => ({ correctionId: c.id, status: rng() < 0.85 ? "honored" : "not_honored" }));

          const review: Review = {
            reviewer,
            phaseId: "p1",
            candidateSha: C,
            contractVersion: Kc,
            correctionStatements,
            findingStatements,
          };
          state = step(state, { type: "REVIEW_SUBMITTED", review });

          // Now that the review has confirmed any findings, close them.
          for (const stmt of findingStatements) {
            if (stmt.status === "confirm") {
              state = step(state, {
                type: "FINDING_CONFIRMED_REPAIRED",
                findingId: stmt.findingId,
                byReviewer: reviewer,
                candidateSha: C,
              });
            } else {
              state = step(state, {
                type: "FINDING_DISPROVED",
                findingId: stmt.findingId,
                byReviewer: reviewer,
                evidence: "simulated counter-evidence",
              });
            }
          }
          break;
        }

        case "resolving_incomplete": {
          state = step(state, { type: "RESOLVING_INCOMPLETE" });
          break;
        }

        case "accept": {
          // Check "accept is identical with and without post-ACCEPTED
          // events" right here, on THIS candidate's own publish flow —
          // not deferred to the run's eventual terminal state, which may
          // include a later, unrelated REVIEWING/RESOLVING episode (e.g.
          // after a stale-publish retry legitimately gathers further
          // ballots or findings for a LATER round). That would compare
          // accept()'s answer against genuinely new evidence, which is not
          // what design §6.3's "no circularity" claim is about.
          const C = state.phase.candidate!.sha;
          const Kc = state.phase.contract.contractVersion;
          const resultAtAccept = accept(state.phase, C, Kc);
          state = step(state, { type: "ACCEPTED", resolvedCorrectionIds: action.resolvedCorrectionIds as string[] });

          const publishIntentAction = next(state).find((a) => a.type === "publish_intent");
          let casSucceeded = false;
          if (publishIntentAction) {
            state = step(state, {
              type: "PUBLISH_INTENT",
              expectedHead: publishIntentAction.expectedHead as string,
              candidateI: publishIntentAction.candidateI as string,
            });
            state = step(state, { type: "ACTION_STARTED", action: "publish_cas", actionId: freshId("a") });
            if (rng() < 0.06) {
              state = step(state, { type: "PUBLISH_STALE", actualHead: `H-stale-${i}` });
            } else {
              state = step(state, { type: "PUBLISH_COMPLETED", newHead: state.phase.probe!.probedI! });
              casSucceeded = true;
            }
          }

          // Only meaningful when the CAS actually succeeded: a stale
          // publish legitimately invalidates the probe (design §6.4 step
          // 3), so accept() correctly reporting false afterward is not a
          // circularity violation — it is new evidence (the head moved),
          // not a fact acceptance itself produced.
          if (casSucceeded) {
            const recomputed = accept(state.phase, C, Kc);
            acceptRecords.push({ phase: state.phase, candidateSha: C, contractVersion: Kc, resultAtAccept });
            assert.equal(
              recomputed,
              resultAtAccept,
              `accept(${C}) changed after PUBLISH_INTENT/PUBLISH_COMPLETED (was ${resultAtAccept}, now ${recomputed})`,
            );
          }
          break;
        }

        case "repair_attempt_started": {
          state = step(state, { type: "REPAIR_ATTEMPT_STARTED" });
          break;
        }

        case "repair_budget_exhausted": {
          state = step(state, { type: "REPAIR_BUDGET_EXHAUSTED" });
          break;
        }

        case "publish_intent": {
          state = step(state, {
            type: "PUBLISH_INTENT",
            expectedHead: action.expectedHead as string,
            candidateI: action.candidateI as string,
          });
          break;
        }

        case "publish_cas": {
          state = step(state, { type: "ACTION_STARTED", action: "publish_cas", actionId: freshId("a") });
          if (rng() < 0.06) {
            state = step(state, { type: "PUBLISH_STALE", actualHead: `H-stale-${i}` });
          } else {
            state = step(state, { type: "PUBLISH_COMPLETED", newHead: state.phase.probe!.probedI! });
          }
          break;
        }

        default:
          throw new Error(`unhandled action from next(): ${action.type}`);
      }
    }
  }

  throw new Error(`did not reach a terminal state within ${MAX_STEPS} steps (stuck in ${state.phase.phase})`);
}

test("no-circularity: 500 random runs, driven only by next(), each reach DONE, BLOCKED, AWAITING_OWNER or PAUSED within bound", () => {
  const RUNS = 500;
  const baseSeed = 20260923;
  for (let run = 0; run < RUNS; run++) {
    const seed = baseSeed + run;
    const rng = mulberry32(seed);
    let result: RunResult;
    try {
      result = driveOnce(rng);
    } catch (err) {
      throw new Error(`no-circularity property failed on seed ${seed} (run ${run}): ${(err as Error).message}`);
    }
    assert.ok(
      result.terminal === "DONE" ||
        result.terminal === "BLOCKED" ||
        result.terminal === "AWAITING_OWNER" ||
        result.terminal === "PAUSED",
      `run ${run} (seed ${seed}) ended in unexpected terminal '${result.terminal}' after ${result.steps} steps`,
    );
    // The "accept is identical with and without post-ACCEPTED events"
    // property is asserted inline, in driveOnce's "accept" case, on every
    // acceptance the random run produces (not deferred to here) — see that
    // case's comment for why deferring it to the run's eventual terminal
    // state would compare against genuinely new, later evidence instead.
  }
});

test("no-circularity: accept() never reads phase.phase or phase.publishedI (facts only acceptance or later produce)", () => {
  const phase: PhaseState = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: approvingReview("M", "C1", K) },
      A: { review: approvingReview("A", "C1", K) },
      B: { review: approvingReview("B", "C1", K) },
    },
  }).phase;

  const accessed = new Set<string>();
  const proxy = new Proxy(phase, {
    get(target, prop, receiver) {
      accessed.add(String(prop));
      return Reflect.get(target, prop, receiver);
    },
  });

  const result = accept(proxy, "C1", K);
  assert.equal(result, true);
  assert.ok(!accessed.has("phase"), "accept() must not read the phase's own FSM state name");
  assert.ok(!accessed.has("publishedI"), "accept() must not read publishedI, set only by PUBLISH_COMPLETED");
});

test("no-circularity: accept()'s result is identical with and without any post-ACCEPTED events", () => {
  const initial = baseState({
    phase: "RESOLVING",
    candidate: { sha: "C1", contractVersion: K },
    integrationHead: "H0",
    checks: { candidateSha: "C1", passed: true },
    probe: { candidateSha: "C1", head: "H0", probedI: "I1", passed: true },
    reviews: {
      M: { review: approvingReview("M", "C1", K) },
      A: { review: approvingReview("A", "C1", K) },
      B: { review: approvingReview("B", "C1", K) },
    },
  });

  const before = accept(initial.phase, "C1", K);

  let after = step(initial, { type: "ACCEPTED", resolvedCorrectionIds: [] });
  after = step(after, { type: "PUBLISH_INTENT", expectedHead: "H0", candidateI: "I1" });
  after = step(after, { type: "PUBLISH_COMPLETED", newHead: "I1" });
  assert.equal(after.phase.phase, "DONE");

  const afterResult = accept(after.phase, "C1", K);
  assert.equal(before, true);
  assert.equal(afterResult, before, "accept(C, K) must not change once later events (PUBLISH_INTENT, PUBLISH_COMPLETED) are appended");
});
