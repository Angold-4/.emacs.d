// The phase-1 conductor daemon (design §2, §6, §8, §9). Single-phase: runs
// exactly one plan phase from READY through DONE/BLOCKED/AWAITING_OWNER,
// with stub reviews (fake-pi reviewer agents in tests) — the real
// discovery/correction/vote loop is phase 2.
//
// Architecture: `events.jsonl` (src/effects/log.ts) is folded through
// core's pure `reduce`/`next` to get the current `State`. `drive()` calls
// `next(state)` and, for every outstanding action, dispatches the matching
// effect. Each effect logs its own action-level intent (via `ACTION_STARTED`,
// which is itself a core event so `next()` never re-dispatches it) and,
// once finished, applies the matching completion event through `reduce`,
// which recomputes `next()` and calls `drive()` again. This is the same
// intent/completion discipline log.ts and next.ts's own docs describe.

import { createHash, randomUUID } from "node:crypto";
import { execFile, execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

import { reduce } from "./core/reduce.ts";
import { curatorEvent, entryVerdictEvents, formatAnchor, planEntryEvents, validateLink, type CuratorProposal, type Entry } from "./core/entries.ts";
import { projectLedger, projectMessages } from "./core/messages.ts";
import { candidateAnchorFreshness, projectEntryReview, renderStatusView, reviewMessageFiles, runIds, statusViewInput } from "./render.ts";
import { buildView, lanesView, updateLiveRun } from "./view.ts";
import { renderPhaseChart, statsFromTimeline } from "./charts.ts";
import { metricEvents, projectMetrics, type MetricEvent } from "./metrics.ts";
import { acceptInput, expandEntryCommand, normalizeDecisionViewCommand, ownerCommandToEvent, type InputKind } from "./core/owner-inbox.ts";
// Plan 06g: the round's only decision points — `pickWinner`, `blocksAcceptance`
// and `roundBudget` (architecture A3 and A6). Nothing in this file decides a
// winner or whether a finding blocks by itself.
import { advisoryReason, blocksAcceptance, candidateLabel, roundBudget, type AcceptanceGate, type FindingGround } from "./core/rounds.ts";
// Plan 06g2 (A1): `runRound` is the round's only orchestrator. The conductor
// implements its `LaneHost` with the worktree, agent, check, review and vote
// machinery below and calls it; it decides nothing about lanes itself.
import { isLaneRound, laneFailureLines, lanesOfContract, runRound, type LaneBuild, type LaneCheck, type LaneHost } from "./core/lanes.ts";
// Plan 06h (A1/A2): the seat list, its leader and the lane count are the
// plan's, read from one place (`seatsOf`/`leaderOf`).
import { leaderOf, seatsOf, seatsRecordOf, workerCountOf } from "./core/seats.ts";
// Plan 06i: triage — the only place fix / trade-off / escalate is decided.
import {
  blockingTriageRecords,
  discoveredDecisions,
  disposition,
  evidenceForDecision,
  evidenceForFinding,
  goldenCited,
  hasBlockingFix,
  impactOverride,
  isClassified,
  ledgerRecords,
  openFindings,
  ownerRequestIsStale,
  recordClassified,
  recordImpact,
  undispositionedBlockingFinding,
  type Disposition,
  type TriageRecord,
} from "./core/triage.ts";
import { resolveBinding } from "./core/binding.ts";
import { next } from "./core/next.ts";
import { checkCommands, checkTier, effectiveChecks, finalCheckOf, parseCheckRecord, type CheckRecord, type CheckRecordCommand } from "./core/checks.ts";
// Plan 05i: the pure environment preflight — parse every declared shell
// command's executable and resolve it in the conductor's own PATH. See the
// module's header.
import { envPreflight } from "./core/env-preflight.ts";
// Plan 01f: the pure half of the gate — the record's shape, its parser, the
// log tail a failure quotes, and the reuse rule (see core/gate.ts's header).
import {
  GATE_TAIL_LINES,
  gateCommandOf,
  gateDecision,
  gateFailureEvidence,
  gateLogHashMatches,
  gateLogTail,
  gateOutcomeText,
  parseGateRecord,
  type GateRecord,
} from "./core/gate.ts";
// Plan 01e: the pure half of the base baseline — parsing recorded check
// output for failing test names, D2's "all failures pre-existing" rule, and
// the on-disk record's shape. See the module's own header.
import {
  baselineFailureNames,
  baselineFailedCommands,
  baselineFlakeNames,
  testIsNamedIn,
  baselineKey,
  classifyCheckFailure,
  classifyRerun,
  failedNormally,
  escapeRegExp,
  parseBaseline,
  parseTestFailures,
  rerunCommandsFor,
  rerunProvesTheTestRan,
  baselineHasEnvironmentFailure,
  type Baseline,
  type BaselineCommand,
  type TestRerunOutcome,
} from "./core/test-failures.ts";
import { clampMessageTitle, contentHashOf, ledgerEntries, MESSAGE_TITLE_MAX, type MessageContent } from "./core/messages.ts";
import type {
  Action,
  Ballot,
  BallotDisclosure,
  CheckFailureClass,
  ContractVersion,
  CriterionDispute,
  Decision,
  DecisionBrief,
  DecisionDisclosure,
  EvFlakeObserved,
  DirectiveScope,
  Event,
  Finding,
  PriorDecisionStatement,
  FindingDisclosure,
  InFlightKey,
  Message,
  MessageType,
  OwnerCommand,
  OwnerDirective,
  OwnerInputKind,
  OwnerInputState,
  PhaseContract,
  PhaseState,
  Review,
  Reviewer,
  Seats,
  State,
} from "./core/types.ts";
import { computeBoundaryTriggerPaths, computeUnreferencedHunks } from "./core/boundaries.ts";
import {
  assertToolSet,
  launchArgs,
  PI_VERSION,
  ROLE_TOOLS,
  type PlanModels,
  type Role,
  type ToolSetMismatch,
} from "./core/roles.ts";
import { rerunBudgetMs } from "./core/checks.ts";
import {
  decisionSettled,
  decisionStatus,
  findingCitesAcceptanceOrReserved,
  isLiveDecision,
  reviewIngestionIssue,
  panelMajority,
  panelOptionsFor,
  panelOutcome,
  panelSeatNumbers,
  panelSeatsSettled,
  reviewsComplete,
  roundPanelItemsNeedingVote,
  roundPanelOutcomeFor,
  roundPanelSeatSettled,
  roundPanelSeatsSettled,
  sameVersion,
} from "./core/predicate.ts";
import { resolvedCorrectionIdsFor } from "./core/predicate.ts";
import { FAILED_VOTE_OPTIONS, isRepairForcingOption, openFindingOptions } from "./core/owner-requests.ts";
import { BRIEF_GLOSSARY, briefIssue, enrichBriefRelated, evidenceFile, fallbackBrief, fallbackDecisionBrief, fallbackEntryBrief, parseCatalogs, renderGlossaryOrg, stripCodeTokens, stripCounts, type Catalogs, type OpenItemConcern } from "./core/briefs.ts";
import { notAcceptedReasons } from "./core/verdict.ts";
import { itemsNeedingEvaluatorReverify, pendingEvidenceItems } from "./core/predicate.ts";
// Plan 06b: the pure item machinery (coverage, test-verify resolution,
// verdict validation, per-item majority, the matrix and the status counts).
import {
  architectureSymbols,
  checkResolutionLines,
  checklistLines,
  flatItems,
  isStructured,
  itemNeedsEvidence,
  coverageComplete,
  coverageIssues,
  coverageLines,
  coverageNoteLines,
  emptyCoverage,
  evidenceFileAnchors,
  itemsFromPhase,
  requirementAndConstraintItems,
  phaseItemOutcomes,
  repairItemLines,
  resolveTestVerifies,
  reviewItemsIssues,
  reviewRequestLines,
  reverify,
  symbolPresent,
  tallyItems,
  testVerifyProblems,
  thinMetItems,
  verdictIssues,
  type Coverage,
  type FlatItem,
  type ItemOutcome,
  type PlanItems,
  type SeatItemVerdict,
  type VerdictContext,
} from "./core/items.ts";
import type { HelloMessage, SubmitMessage } from "./core/protocol.ts";
import { validate } from "./core/schema.ts";

import { EventLog, readLog, type LogRecord } from "./effects/log.ts";
import { acquireLock, acquireWaitingLock, type Lock } from "./effects/lock.ts";
import { childEnv, runCommand, type RunCommandResult } from "./effects/shell.ts";
import { groupStartedBefore, killGroup, loggedHeld, processAlive, signalProcess, sweep, type SweepResult } from "./effects/sweep.ts";
import {
  createWorktree,
  diffHunks,
  diffNameOnly,
  diffText,
  disposableCheckout,
  discardProbeByBranch,
  freezeCommit,
  findCommitByTrailer,
  materializeCandidate,
  probe as gitProbe,
  discardProbe,
  publishCAS,
  removeWorktree,
  candidateTree,
  verifyIntegrity, removedTestsBetween } from "./effects/git.ts";
import { RunSocketServer, type HelloResult, type SubmitResult } from "./effects/socket.ts";
import { PiAgent, spawnPiAgent } from "./effects/pi-rpc.ts";
import { redactBytes, redactRecord, redactText, resolveSecrets, secretNames, secretPromptLines, utf16Kind, type Secret } from "./effects/secrets.ts";
// Plan 01a: the secret guard itself lives with the other `sh` guards (they
// are wired into the agent's `tool_call` hook, and the conductor reuses the
// same refusal at the socket, where a scripted agent's commands arrive).
import { secretUseInCommand } from "../extension/guards.ts";
// Plan 01b: owner-wait notifications (see notify.ts's own header for the rule).
import { notify, oneLine, waitReason } from "./notify.ts";
import { crashAt, CRASH_BOUNDARIES, PHASE_2_CRASH_BOUNDARIES } from "./effects/crash.ts";
import { loadavg } from "node:os";

export { CRASH_BOUNDARIES, PHASE_2_CRASH_BOUNDARIES } from "./effects/crash.ts";
export type { CrashBoundary } from "./effects/crash.ts";

const DECISION_SCHEMA: Record<string, unknown> = JSON.parse(
  fs.readFileSync(new URL("../schemas/decision.schema.json", import.meta.url), "utf8"),
);
const FINDING_SCHEMA: Record<string, unknown> = JSON.parse(
  fs.readFileSync(new URL("../schemas/finding.schema.json", import.meta.url), "utf8"),
);
const OWNER_COMMAND_SCHEMA: Record<string, unknown> = JSON.parse(
  fs.readFileSync(new URL("../schemas/owner-command.schema.json", import.meta.url), "utf8"),
);

/** Plan 01d: how many times a reviewer dispatch's incomplete `submit_review`
 * is rejected back to it before the review is accepted as-is (and logged as
 * `incomplete_review`). Two: a model that fixes its own omission gets a
 * second and third chance within the same turn, and one that never does
 * cannot wedge the turn. */
export const MAX_INCOMPLETE_REVIEW_REJECTIONS = 2;

// ---------------------------------------------------------------------------
// Config — design §8.1 defaults, overridable per run.
// ---------------------------------------------------------------------------

export interface Deadlines {
  helloTimeoutMs: number;
  workerAttemptMs: number;
  shCommandMs: number;
  freezeMs: number;
  checkMs: number;
  probeMs: number;
  /** Plan 01f: how long the gate command may run before its process group is
   * killed and the gate recorded as failed. The plan sets it with
   * `#+TT_GATE_MINUTES` (default 30): a 15-minute `--clean --build` fits,
   * the old 8-minute `sh` limit did not (runtime doc §6). */
  gateMs: number;
  /** Plan 04a: how long the EVALUATING stage's fresh evaluator may run before
   * its raw messages are published `unevaluated` and the phase moves on. The
   * plan sets it with `#+TT_EVALUATE_MINUTES` (default 10). */
  evaluateMs: number;
  /** Decision briefs: how long the brief-writing agent may run. It is its own
   * short deadline (default 90 s), not the evaluator's evaluateMs, so a
   * stalled writer cannot make the owner wait out a full evaluation budget
   * after evaluation already finished (finding disc-B-94). */
  briefMs: number;
  /** Plan 04b: each panel seat's own deadline. A seat that times out is
   * re-dispatched once; a second loss makes that seat unavailable. The plan
   * sets it with `#+TT_PANEL_MINUTES` (default 10). */
  panelMs: number;
  reviewMs: number;
  reproductionMs: number;
  abortGraceMs: number;
  termGraceMs: number;
  /** Plan 2d: how long `stop()` waits for an agent to exit after abort before
   * escalating to SIGTERM. Much shorter than `abortGraceMs` (design §8.2's
   * mid-run cancellation grace): `tt stop` must release the run within 15 s,
   * and a clean stop has no reason to wait out the full cancellation grace. */
  stopAbortGraceMs: number;
  /** Plan 3b stall watchdog: an agent that is mid-turn (prompted, not yet
   * settled), has no `sh` command of its own running, and produced no event
   * for this long is steered once ("continue, or submit what you have");
   * silent for this long again, its attempt or review ends as timed out
   * instead of waiting out workerAttemptMs/reviewMs. */
  stallMs: number;
  /** design §9.3: how often the conductor re-reads `<run>/inbox/*.json`
   * while running. Owner commands are conductor state, so the conductor
   * must pick one up even when parked in AWAITING_OWNER (when `next()`
   * dispatches nothing at all). Plan 01b: the same poll re-checks whether a
   * wait has earned its one 30-minute reminder, so a parked run needs no
   * second timer. */
  inboxPollMs: number;
  /** Plan 01b: how long a run may sit in AWAITING_OWNER before the one
   * reminder notification (design D4: "a re-notify after 30 minutes if
   * still waiting"). */
  notifyReminderMs: number;
  /** Wall-clock run execution budget (design §8.1's "run execution budget").
   * Unset (default) = unbounded. The clock counts only time spent
   * *executing* (design §8.2's own text): it pauses whenever the phase is
   * AWAITING_OWNER, and stops altogether once the run itself is
   * RUN_PAUSED_BUDGET — see Conductor's `#syncBudgetTimer`. */
  runBudgetMs?: number;
  /** Total token budget across every agent this run has spawned (design
   * §8.1's "wall and tokens"). Unset (default) = unbounded. */
  runBudgetTokens?: number;
  /** Per-attempt token cap (design §8.1's "per-attempt token cap from Pi
   * usage events"): exceeding it during a single worker attempt is treated
   * exactly like that attempt's own `workerAttemptMs` deadline (cancel,
   * ATTEMPT_TIMED_OUT, consumes a repair round). Unset (default) =
   * unbounded. */
  tokenCapPerAttempt?: number;
}

export const DEFAULT_DEADLINES: Deadlines = {
  helloTimeoutMs: 10_000,
  workerAttemptMs: 45 * 60_000,
  shCommandMs: 3 * 60_000,
  freezeMs: 2 * 60_000,
  checkMs: 5 * 60_000,
  probeMs: 10 * 60_000,
  gateMs: 30 * 60_000,
  evaluateMs: 10 * 60_000,
  briefMs: 90_000,
  panelMs: 10 * 60_000,
  reviewMs: 15 * 60_000,
  reproductionMs: 5 * 60_000,
  abortGraceMs: 30_000,
  termGraceMs: 10_000,
  stopAbortGraceMs: 2_000,
  stallMs: 3 * 60_000,
  inboxPollMs: 1_000,
  notifyReminderMs: 30 * 60_000,
};

export interface RunPlanPhase {
  id: string;
  goal: string;
  acceptance: string[];
  checks: string[];
  boundaries: string[];
  reserved: string[];
  /** Plan 06b: the structured items of a phase subtree (ref
   * refs/06_ref_plan_format.md). Absent on an old-format plan. */
  architecture?: import("./core/items.ts").ArchitectureItem[];
  requirements?: import("./core/items.ts").RequirementItem[];
  constraints?: import("./core/items.ts").ConstraintItem[];
  /** Plan 06b (OD-1 R9): true when the parser SYNTHESIZED these items from
   * an old-format `acceptance`/`:RESERVED:` list. The items exist for the
   * views and lint, but the phase owes submit_phase only, exactly like a
   * hand-written old-format JSON plan. */
  itemsSynthesized?: boolean;
  /** Plan 01c: 1-based lines in the source Org file of the `acceptance`
   * items, parallel to the array. Emacs records them so `tt lint` can point
   * at the offending line; a hand-written JSON plan has no lines and the
   * linter reports the item without one. */
  acceptanceLines?: number[];
  /** Plan 01c: the owner's own checklist. These items are the owner's to do —
   * they are never given to the worker or the reviewers as acceptance. They
   * are shown in the status buffer once the phase is DONE, and in
   * `tt summary`'s PR body as `- [ ]` items. */
  ownerChecklist?: string[];
  /** Plan 01c: 1-based lines of `ownerChecklist` in the source Org file. */
  ownerChecklistLines?: number[];
  provisional?: boolean;
  /** Plan 06g: `#+TT_WORKERS:` — how many lanes one round runs (1 or 2 in
   * this plan version; more is 06h's). Absent (or 1) is today's
   * single-candidate loop. */
  workers?: number;
  /** Plan 06g: `#+TT_ROUNDS:` — how many rounds one phase may spend before
   * it parks on the owner. Absent means the default of 3. */
  rounds?: number;
  /** Plan 06h (A1): `#+TT_REVIEWERS:` (or the phase's own `:REVIEWERS:`).
   * Absent means `M A B`. */
  seats?: string[];
  /** Plan 06h (A1): `#+TT_LEADER:` (or the phase's own `:LEADER:`). Absent
   * means the first seat. */
  leader?: string;
  /** Plan 01f: the phase's `:GATE:` command — the expensive, live proof the
   * conductor runs itself after checks, probe and reviews pass, and before
   * acceptance. Undeclared on every phase that predates plan 01f. */
  gate?: string;
  /** Plan 01f: the phase's `:GATE_CLEANUP:` command, run after the gate
   * whatever its outcome. */
  gateCleanup?: string;
  /** Plan 06c: the phase's final check (`#+TT_FINAL_CHECKS`, overridden by
   * the phase's `:FINAL_CHECKS:`). Only the candidate about to be accepted
   * runs it, once. */
  finalChecks?: string[];
  /** Plan 06i (A3): the phase's golden note (`#+TT_GOLDEN:`, overridden by
   * the phase's `:GOLDEN:`). A choice justified only by an earlier drafting
   * decision is re-checked against it; a `contract` classification must cite
   * it. */
  golden?: string;
}

/** The on-disk plan file `tt start` reads. Only phase 0 (index 0) is run by
 * this packet's single-phase conductor. */
export interface RunPlanFile {
  title: string;
  /** Plan 06g: the plan's `#+TT_WORKERS:` — how many lanes one round runs.
   * Emacs records the whole number (or the raw text, which `tt lint`
   * refuses). Absent (or 1) is today's single-candidate loop. */
  workers?: number;
  /** Plan 06g: the plan's `#+TT_ROUNDS:` — how many rounds one phase may
   * spend. Absent means the default of 3. */
  rounds?: number;
  /** Plan 06h (A1): the plan's `#+TT_REVIEWERS:`. Absent means `M A B`. */
  seats?: string[];
  /** Plan 06h (A1): the plan's `#+TT_LEADER:`. Absent means the first seat. */
  leader?: string;
  /** Plan 01c: the Org file this JSON plan was parsed from, recorded by Emacs
   * so `tt lint` can name the file the owner edited rather than its temporary
   * JSON copy. Never read by the conductor. */
  sourceFile?: string;
  /** Absolute path to the git repository this run operates on. */
  repo: string;
  /** The integration branch this phase publishes onto. Defaults to the
   * repo's current HEAD's branch name if omitted — callers should usually
   * pass it explicitly. */
  integrationBranch: string;
  checks: string[];
  phases: RunPlanPhase[];
  ownerNotes?: string;
  /** Per-plan time limits (plan keywords TT_SH_MINUTES, TT_CHECK_MINUTES,
   * TT_ATTEMPT_MINUTES), for repositories whose builds and suites take
   * longer than the defaults (e.g. a Rust workspace). */
  deadlines?: Partial<Deadlines>;
  /** Documents the plan cites that live outside the repository (absolute
   * paths; Emacs collects them from #+TT_REFS and from the plan text). They
   * are copied into <run>/refs at creation, so a run keeps the version it
   * started with, and every agent is told where they are instead of
   * searching for them (run aea875c4: reviewers spent 6 minutes running
   * recursive `find` searches over the home directory). */
  references?: string[];
  /** Plan 01a: the secrets the plan declares by name (#+TT_SECRETS). Each is
   * resolved from the conductor's own environment — never from the plan —
   * passed to every agent, and replaced by `***NAME***` in everything the
   * conductor writes. A name that is unset at start is reported in the
   * status; the run starts anyway. */
  secrets?: string[];
  /** Plan 06e (A1): a `KEY=value` file (`#+TT_ENV_FILE`, or `tt start
   * --env-file`) that supplies a declared secret when the environment does
   * not. The lookup order is the environment first, then this file. The path
   * is recorded here (never a value), so a resumed run resolves the same
   * source. Only effects/secrets.ts reads it. */
  envFile?: string;
  /** Plan 01i: program-wide owner directives already in force when this
   * run's node was started (D5). They seed the phase's directive list, so a
   * node started after the owner's ruling still carries it in every prompt.
   * Written by the program scheduler (`nodePlan`), never by hand. */
  ownerDirectives?: RunPlanDirectiveSeed[];
  /** #+TT_MODELS: per-role and per-seat provider/model overrides (design
   * §2.1: `reviewer.M`, `panel.1`, `panel=reviewers`). Absent on every plan
   * that predates the keyword, so every role and seat keeps Pi's own
   * `defaultModel`. Emacs parses it; `planModelSelector` is the one runtime
   * place that turns the role (and, for the reviewers and the panel, the
   * seat) into `launchArgs`'s `provider`/`model`. */
  models?: PlanModels;
  /** Lint-only (never read by the conductor): the 1-based line of the
   * `#+TT_MODELS` keyword in the source Org file, so `tt lint` can point at
   * it (`acceptanceLines` is the same kind of field). */
  modelsLine?: number;
  /** Lint-only: roles the keyword declared more than once. A JSON object
   * cannot carry a duplicate key, so the parser records them here. */
  modelsRepeated?: string[];
  /** Plan 05d: the `#+TT_RERUN:` single-test template, with `{name}` and
   * `{file}` placeholders. Used to re-run each new failing test alone before
   * a check fails. Absent: the built-in Node/cargo defaults, or the strict
   * rule when neither applies. */
  rerun?: string;
  /** Lint-only: the 1-based line of `#+TT_RERUN:` in the source Org file. */
  rerunLine?: number;
}

/** Plan 01i: a program-wide owner directive a node's plan was started with. */
export interface RunPlanDirectiveSeed {
  id?: string;
  text: string;
  at?: string;
}

export interface ConductorOptions {
  runDir: string;
  plan: RunPlanFile;
  deadlines?: Partial<Deadlines>;
  /** The `pi` binary, or an injected stand-in (fake-pi) for tests — e.g.
   * `"node"` with `piArgsPrefix: ["test/fake-pi/fake-pi.ts"]`. */
  piCommand?: string;
  /** Prepended before `launchArgs(role, ...)`'s own argv — how fake-pi.ts
   * (which ignores that argv entirely) is actually invoked. */
  piArgsPrefix?: string[];
  /** Per-role override of `piCommand`/`piArgsPrefix` — added for
   * test/live/live-single-phase.test.ts (phase-1 work-packet item 1c),
   * which needs the real `pi` binary for the worker role but stub (fake-pi)
   * reviewers in the same run. Consulted before the flat `piCommand`/
   * `piArgsPrefix` fields for that one role; every other role keeps using
   * the flat fields. Absent (the default): every role uses the flat
   * fields, exactly as before this option existed. */
  piCommandFor?: (role: Role) => string | undefined;
  piArgsPrefixFor?: (role: Role) => string[] | undefined;
  /** `launchArgs`'s own `provider`/`model` (design §2.1), per role — needed
   * for the same reason as `piCommandFor`/`piArgsPrefixFor`: a live run
   * against the real `pi` binary must pick a real provider/model, which
   * nothing else on `ConductorOptions` carries. Absent (the default): no
   * `--provider`/`--model` flag is added, i.e. Pi's own default. */
  providerModelFor?: (role: Role, seat?: string | number) => { provider?: string; model?: string } | undefined;
  extraEnv?: NodeJS.ProcessEnv;
  /** Plan 05i: the environment the preflight resolves declared executables
   * in (defaults to `process.env`, the conductor's own environment). Tests
   * point it at a scratch PATH to prove a missing tool blocks a run and a
   * fixed one lets `tt resume` proceed. */
  preflightEnv?: NodeJS.ProcessEnv;
  /** Per-agent environment override — tests use this to give each fake-pi
   * agent (worker, reviewer M/A/B) its own `FAKE_PI_SCRIPT`. */
  piEnvFor?: (role: Role, agentId: string) => NodeJS.ProcessEnv | undefined;
  /** Work packet 2a: phase 1's `#castStubBallots` behavior (every reviewer
   * auto-approves every delegated decision on submit_review, no real
   * ballots/findings/discovery) — kept, opt-in, for existing tests that
   * predate the real two-turn review protocol. Default `false`: a real
   * reviewer's `submit_discovery` (turn 1) and `submit_review` (turn 2,
   * carrying real `ballots`/`findings`) are processed for real (design §3,
   * §4, §5, §6.1). */
  stubReviews?: boolean;
  /** Plan 2c: reuse the candidate's passed checks for the integration probe
   * when the probed integration has exactly the candidate's tree (default
   * true). Tests that exercise the probe's own command handling turn it off. */
  probeReuse?: boolean;
  /** Plan 01b: the clock the notification reminder reads (ms since epoch).
   * Defaults to `Date.now`; a test advances an injected clock to reach the
   * 30-minute reminder without waiting. */
  now?: () => number;
  /** Plan 01f: the machine-wide gate lock's path. Defaults to
   * `~/.tradeoffs-trace/gate.lock`, so two phases (in one program or in two
   * runs) never gate at once; tests point it at a temp path so they neither
   * contend with a real run nor with each other. */
  gateLockPath?: string;
  /** Plan 06g2 (C2): the machine-wide lock every candidate check runs
   * under, so two lanes' checks — in one round, or in two runs on this
   * machine — never overlap in time. Defaults to
   * `~/.tradeoffs-trace/check.lock`. */
  checkLockPath?: string;
  /** Decision briefs: when true, the conductor records a deterministic brief
   * for every open owner item after evaluation, so the owner always has one
   * above the evidence even when the evaluator's model did not call
   * submit_brief. Default false so the many existing tests that reach
   * AWAITING_OWNER keep their exact event logs; `cli.ts` enables it for a real
   * run. */
  briefs?: boolean;
}

// ---------------------------------------------------------------------------
// Run directory layout (design §9.1)
// ---------------------------------------------------------------------------

/** Contract v1: the events that can change a rendered projection. An
 * EVALUATION_TIMED_OUT publishes a type's raw messages `unevaluated`, so it
 * changes `messages.jsonl`/`ledger.jsonl` too. Plan 05c: PANEL_DECIDED
 * changes `views/review.org`, which shows a blocker's panel outcome, so it
 * must rewrite the projections the same way a message event does. */
const MESSAGE_EVENT_TYPES = new Set<string>([
  "EVALUATION_TIMED_OUT",
  "MESSAGE_ADDRESS_REPORTED",
  "MESSAGE_RAISED",
  "MESSAGE_PUBLISHED",
  "MESSAGE_MERGED",
  "MESSAGE_DROPPED",
  "OWNER_VERDICT",
  "MESSAGE_RESOLVED",
  "MESSAGE_SUPERSEDED",
  "MESSAGE_CARRIED",
  "PANEL_DECIDED",
  // Plan 05e: the round panel stamps its reasons on each item message, and a
  // severity change alters what the review shows.
  "ROUND_PANEL_DECIDED",
  "FINDING_SEVERITY_CHANGED",
  "FINDING_VERIFIED",
  "FINDING_RESOLVED_BY_VOTE",
  "CANDIDATE_APPROVED",
  // Plan 05j: the entry ledger and the review lint are projections too, so an
  // owner's s/m/A/D on an entry (or a curator's link) must rewrite the views
  // in the same beat (finding A-7).
  "ENTRY_OPENED",
  "MESSAGE_LINKED",
  "ENTRY_RETITLED",
  "ENTRY_SPLIT",
  "ENTRY_STATE",
  "ENTRY_MERGED_BY_OWNER",
  "ENTRY_CURATED",
  "REVIEW_LINT_FAILED",
]);

export function runPaths(runDir: string) {
  return {
    root: runDir,
    meta: path.join(runDir, "meta.json"),
    lock: path.join(runDir, "conductor.lock"),
    sock: path.join(runDir, "conductor.sock"),
    log: path.join(runDir, "conductor.log"),
    plan: path.join(runDir, "plan"),
    events: path.join(runDir, "events.jsonl"),
    stream: path.join(runDir, "stream"),
    views: path.join(runDir, "views"),
    sessions: path.join(runDir, "sessions"),
    candidates: path.join(runDir, "candidates"),
    checks: path.join(runDir, "checks"),
    // Contract v1: the rebuilt projections from `events.jsonl`.
    messages: path.join(runDir, "messages.jsonl"),
    ledger: path.join(runDir, "ledger.jsonl"),
    review: path.join(runDir, "views", "review.org"),
    messagesView: path.join(runDir, "views", "messages"),
    itemsView: path.join(runDir, "views", "items"),
    // Plan 05j: one file per live ENTRY, the RET target from the review view.
    entriesView: path.join(runDir, "views", "entries"),
    status: path.join(runDir, "views", "status.txt"),
    // Plan 03c: the phase state machine as an ASCII chart (TRANSITIONS).
    loop: path.join(runDir, "views", "loop.txt"),
    // Plan 05h: the current round as a vertical tape (MAIN_PATH), the owner's
    // live view; `loop.txt` stays one key away as the reference.
    tape: path.join(runDir, "views", "tape.txt"),
    // Plan 04c: the balance metrics (a deterministic projection of state and
    // the control log; `tt contract rebuild`/`check` include it).
    metrics: path.join(runDir, "views", "metrics.json"),
    inbox: path.join(runDir, "inbox"),
    inboxApplied: path.join(runDir, "inbox", "applied"),
    inboxRejected: path.join(runDir, "inbox", "rejected"),
    worktree: path.join(runDir, "worktree"),
    refs: path.join(runDir, "refs"),
  };
}

/** True iff `repo` is a shallow clone. A candidate checkout clones the
 * repository, and cloning a shallow repository fails at freeze — run
 * 9ab9188b lost three attempts that way — so a run refuses to start on one. */
export function isShallowRepository(repo: string): boolean {
  try {
    return execFileSync("git", ["-C", repo, "rev-parse", "--is-shallow-repository"], { encoding: "utf8" }).trim() === "true";
  } catch {
    return false;
  }
}

/** The reference documents copied into a run, as absolute paths. */
export function runReferences(runDir: string): string[] {
  const dir = runPaths(runDir).refs;
  try {
    return fs.readdirSync(dir).sort().map((f) => path.join(dir, f));
  } catch {
    return [];
  }
}

/** The conductor's own source revision — `git rev-parse HEAD` of this
 * package's checkout, or `"unknown"` if that fails (e.g. a frozen copy with
 * no `.git` at all, design §9.1's "the runner is later frozen by copying a
 * checkout"). Recorded in `meta.json` (`createRun`) and in the run's own
 * `init` log record (`Conductor.start()`), so a restarted conductor's own
 * revision is always on record next to the run it is driving. This packet
 * only *records* it — comparing it against what an earlier start recorded,
 * and refusing to proceed on a mismatch, is phase 3's frozen-runner
 * enforcement, not this one's. */
export function runnerRevision(): string {
  const packageRoot = fileURLToPath(new URL("..", import.meta.url));
  // An installed (frozen) runner carries RUNNER_SHA, written by
  // `tt runner install` (plan: "The runner is frozen while it builds itself").
  try {
    const pinned = fs.readFileSync(path.join(packageRoot, "RUNNER_SHA"), "utf8").trim();
    if (pinned) return pinned;
  } catch {
    // not an installed runner
  }
  try {
    return execFileSync("git", ["-C", packageRoot, "rev-parse", "HEAD"], { encoding: "utf8" }).trim();
  } catch {
    return "unknown";
  }
}

/** Plan 03c: the model Pi uses when no provider/model is passed —
 * `defaultModel` in Pi's own `~/.pi/agent/settings.json` (the same file the
 * Emacs front end writes, see core/init-pilish.el). Read once and cached: the
 * phase chart asks for it on every status beat. Undefined when the file is
 * missing or unreadable, so the chart says `default` rather than guessing. */
let cachedPiDefaultModel: string | null | undefined;
export function piDefaultModel(): string | undefined {
  if (cachedPiDefaultModel !== undefined) return cachedPiDefaultModel ?? undefined;
  try {
    const file = path.join(os.homedir(), ".pi", "agent", "settings.json");
    const raw = JSON.parse(fs.readFileSync(file, "utf8")) as { defaultModel?: unknown };
    cachedPiDefaultModel = typeof raw.defaultModel === "string" && raw.defaultModel.length > 0 ? raw.defaultModel : null;
  } catch {
    cachedPiDefaultModel = null;
  }
  return cachedPiDefaultModel ?? undefined;
}

/** A conductor whose own revision differs from the one the run was started
 * under refuses to resume it (plan, "How this plan is executed"). */
export class RunnerMismatchError extends Error {}

/** Plan 05i: a check/baseline/probe/gate command exited 126/127 — the shell
 * could not execute it. Thrown out of the command loop so no baseline record
 * is written and no `checks failed` is recorded; `#runBaselineStage` catches
 * it and emits `ENV_CHECK_FAILED`. */
class EnvFailure extends Error {
  readonly stage: "baseline" | "checks" | "probe" | "gate";
  readonly command: string;
  readonly exitCode: number | null;
  readonly tail: string;
  constructor(stage: "baseline" | "checks" | "probe" | "gate", command: string, exitCode: number | null, output: string) {
    super(`environment failure: ${command} exited ${exitCode}`);
    this.name = "EnvFailure";
    this.stage = stage;
    this.command = command;
    this.exitCode = exitCode;
    this.tail = lastLines(output, 60);
  }
}

/** The last `n` lines of a command's output, for an environment failure's
 * evidence (the same tail shape the gate finding quotes). */
function lastLines(text: string, n: number): string {
  if (text.length === 0) return "";
  const all = text.split("\n");
  if (all[all.length - 1] === "") all.pop();
  return all.slice(Math.max(0, all.length - n)).join("\n");
}

export function contractVersionFor(phase: RunPlanPhase, snapshot = 1): ContractVersion {
  const sectionSha256 = createHash("sha256").update(JSON.stringify(phase)).digest("hex");
  return { snapshot, sectionSha256 };
}

/** Plan 01g: the contract version produced when an amendment (or the owner's
 * revert of one) replaces the phase's acceptance list. `snapshot` bumps by
 * one so every existing binding says "it changed since you viewed it", and
 * the section hash is recomputed from the amendable contract fields so two
 * different acceptance lists never share a version. */
export function amendContractVersion(contract: PhaseContract, acceptance: string[]): ContractVersion {
  const sectionSha256 = createHash("sha256")
    .update(
      JSON.stringify({
        phaseId: contract.phaseId,
        goal: contract.goal,
        acceptance,
        checks: contract.checks,
        boundaries: contract.boundaries,
        reserved: contract.reserved,
        architecture: contract.architecture,
        requirements: contract.requirements,
        constraints: contract.constraints,
        gate: contract.gate,
        gateCleanup: contract.gateCleanup,
        finalChecks: contract.finalChecks,
        seats: contract.seats,
        leader: contract.leader,
      }),
    )
    .digest("hex");
  return { snapshot: contract.contractVersion.snapshot + 1, sectionSha256 };
}

export function buildContract(phase: RunPlanPhase): PhaseContract {
  // Plan 06b (OD-1): a STRUCTURED phase declares architecture/requirements/
  // constraints and carries the item loop; an old-format phase (Goal +
  // Acceptance + :RESERVED:) declares none and owes submit_phase only. The
  // items are taken as the plan declares them — never synthesized into the
  // contract — so `#structured()` is exactly "the plan declared items".
  // OD-1 R9: items the parser synthesized from the old format are NOT
  // declared items — the phase stays unstructured and owes submit_phase only.
  const declaredItems = phase.itemsSynthesized !== true && (phase.architecture !== undefined || phase.requirements !== undefined || phase.constraints !== undefined);
  const acceptance = (phase.acceptance?.length ?? 0) > 0 ? phase.acceptance : (phase.requirements ?? []).map((r) => r.text);
  return {
    phaseId: phase.id,
    contractVersion: contractVersionFor(phase),
    goal: phase.goal,
    acceptance,
    checks: phase.checks,
    boundaries: phase.boundaries,
    reserved: phase.reserved,
    ...(phase.architecture ? { architecture: phase.architecture } : {}),
    ...(phase.requirements ? { requirements: phase.requirements } : {}),
    ...(phase.constraints ? { constraints: phase.constraints } : {}),
    ...(declaredItems ? {} : { itemsSynthesized: true }),
    // Plan 01f: a declared gate is part of the frozen contract — the FSM
    // (next.ts/transitions.ts) reads it to decide whether the phase gates at
    // all, and every prompt that mentions the gate quotes the same text.
    ...(gateCommandOf(phase) ? { gate: phase.gate } : {}),
    ...(phase.gateCleanup ? { gateCleanup: phase.gateCleanup } : {}),
    // Plan 06c: a declared final check is part of the frozen contract.
    ...(finalCheckOf(phase) ? { finalChecks: phase.finalChecks } : {}),
    ...(typeof phase.golden === "string" && phase.golden.trim().length > 0 ? { golden: phase.golden } : {}),
    // Plan 06g: the lane count and the round budget are frozen into the
    // contract, so the FSM, the prompts and the views all read one value.
    ...(workersOf(phase) > 1 ? { workers: workersOf(phase) } : {}),
    ...(typeof phase.rounds === "number" && phase.rounds > 0 ? { roundsAllowed: Math.floor(phase.rounds) } : {}),
    // Plan 06h (A2): the seats and the leader are frozen too, so every
    // reader has ONE source of the seat list (`seatsOf`).
    ...(phase.seats && phase.seats.length > 0 ? { seats: [...phase.seats] } : {}),
    ...(typeof phase.leader === "string" && phase.leader.length > 0 ? { leader: phase.leader } : {}),
  };
}

/** Plan 06g/06h: the number of lanes one round runs. `#+TT_WORKERS` when the
 * plan declares it, 1 otherwise — and 1 is today's single-candidate loop,
 * which records no round event at all. */
export function workersOf(phase: Pick<RunPlanPhase, "workers">): number {
  return workerCountOf(phase);
}

export function initialState(
  runId: string,
  phase: RunPlanPhase,
  integrationHead: string,
  programDirectives: RunPlanDirectiveSeed[] = [],
  /** Plan 06h (A1): the seats the init event recorded, so a run replays with
   * its own seats even if the plan changed. Absent (an old log) means the
   * plan's own seats, and `M A B` when it declares none. */
  initSeats?: Seats,
): State {
  const built = buildContract(phase);
  const contract: PhaseContract = initSeats
    ? {
        ...built,
        ...(initSeats.seats.length > 0 ? { seats: [...initSeats.seats] } : {}),
        ...(initSeats.leader.length > 0 ? { leader: initSeats.leader } : {}),
      }
    : built;
  const phaseState: PhaseState = {
    runId,
    phaseId: phase.id,
    contract,
    phase: "READY",
    attempt: { n: 1 },
    integrationHead,
    reviews: {},
    decisions: [],
    findings: [],
    ownerRequests: [],
    corrections: [],
    ballots: [],
    overrides: [],
    inFlight: {},
    repairRoundsUsed: 0,
    // Plan 06g (A4, owner-verified): `roundBudget(contract)` is the only place
    // that decides how many ROUNDS a phase may spend — `#+TT_ROUNDS` when the
    // plan names it, the default of 3 otherwise. One round is one candidate
    // reviewed, so the repair allowance the FSM compares against is
    // `roundBudget - 1`: the first candidate is round 1 and each later round
    // is one repair. `#+TT_ROUNDS: 3` therefore reviews exactly 3 candidates
    // (the first plus two repairs), matching ODP-1's "accepted within 3
    // rounds".
    repairRoundsGranted: roundBudget(contract) - 1,
    // Plan 01i: a program-wide directive in force when this node was started
    // seeds the phase's own list (scope `program`, seeded), so it is quoted
    // in every prompt from the first attempt. The plan snapshot is on disk,
    // so a restart folds the identical seeds.
    ...(programDirectives.length > 0
      ? {
          ownerDirectives: programDirectives.map((d, i) => seededDirective(d, i)),
        }
      : {}),
  };
  return { run: "RUN_ACTIVE", phase: phaseState };
}

/** Plan 01i: one seeded program-wide directive as a phase record. It keeps
 * the program's own `ODP-<n>` id verbatim, so one id names the same ruling at
 * both levels and `withdraw ODP-n` retires the right record everywhere. */
function seededDirective(seed: RunPlanDirectiveSeed, index: number): OwnerDirective {
  const { id, seq } = allocateDirectiveId([], typeof seed.id === "string" ? seed.id : `ODP-${index + 1}`);
  return {
    id,
    seq,
    text: seed.text,
    scope: "program",
    status: "in-force",
    commandId: `seed-${id}`,
    at: seed.at ?? "",
    targets: [],
    deliveries: {},
    seeded: true,
  };
}

/** Creates a fresh run directory (fails if it already exists) with the
 * layout design §9.1 describes, writes `meta.json` and the plan snapshot,
 * and returns the run id (the directory's basename). */
export function createRun(root: string, plan: RunPlanFile, runId = randomUUID().slice(0, 8)): string {
  if (plan.repo && isShallowRepository(plan.repo)) {
    throw new Error(`${plan.repo} is a shallow clone; candidate checkouts cannot be made from it. Run \`git -C ${plan.repo} fetch --unshallow\` first.`);
  }
  const runDir = path.join(root, runId);
  fs.mkdirSync(runDir, { recursive: false });
  const p = runPaths(runDir);
  for (const dir of [p.plan, p.stream, p.views, p.sessions, p.candidates, p.checks, p.inbox, p.inboxApplied, p.inboxRejected]) {
    fs.mkdirSync(dir, { recursive: true });
  }
  // Plan 01a's first choke point: the run's plan snapshot is written redacted.
  // A plan's own prose is a value carrier — its goal, its reference list, above
  // all its `checks` lines, which the measured runs used to paste a vendor key
  // inline (runtime doc §7), and which `#recordCheck` already treats as one
  // ("the plan's own check line"). Redacting here rather than in each reader
  // means the value cannot be in the file at all, for any consumer: agents
  // read it, `tt status`/`tt state` print it, `tt redact` would only clean it
  // later. A conductor started from this snapshot (`tt start`'s detached
  // process re-reads it, cli.ts) therefore holds the mask in the plan's own
  // text — see `#withValues`, which puts the real value back for the commands
  // the plan asks for, and only for them.
  const declared = secretNames(plan.secrets);
  const { maskable } = resolveSecrets(declared, process.env, plan.envFile);
  fs.writeFileSync(path.join(p.plan, "v1.json"), JSON.stringify(redactRecord(plan, maskable), null, 2));
  // Snapshot the plan's reference documents (same-named files get a numeric
  // prefix); a missing one is skipped and noted rather than failing the run.
  // Plan 01a: a document that quotes a declared secret is copied redacted —
  // the vendor reference docs of atlas plan 13 held the keys themselves
  // (runtime doc §7), and every agent can read these copies. A value is
  // replaced in UTF-8, UTF-16LE and UTF-16BE (raw and JSON-escaped), so a doc
  // saved as UTF-16 — which is not text by the NUL test — cannot slip through;
  // a document that is neither text nor a UTF-16 document is NOT copied at
  // all, because a leak that cannot be searched is worse than a missing
  // reference. It is named in refs/MISSING.txt either way, so the agent's
  // prompt says the copy is not there rather than silently lacking it.
  //
  // And a copy is only safe when EVERY declared value is known: any document
  // may quote any of them, so with one declared secret unset (or set too short
  // to mask) nothing can be verified and no document is copied — the plan that
  // declares a credential and does not export it gets its names in the status
  // and no unredactable vendor docs in every agent's reach (finding M-13).
  const unmaskable = declared.filter((name) => !maskable.some((s) => s.name === name));
  const refs = plan.references ?? [];
  if (refs.length > 0) {
    fs.mkdirSync(p.refs, { recursive: true });
    const used = new Set<string>();
    const missing: string[] = [];
    refs.forEach((src, i) => {
      if (!fs.existsSync(src)) {
        missing.push(src);
        return;
      }
      if (unmaskable.length > 0) {
        missing.push(`${src} (not copied: ${unmaskable.join(", ")} could not be masked — the plan declares it, so a copy cannot be verified)`);
        return;
      }
      let name = path.basename(src);
      if (used.has(name)) name = `${i}-${name}`;
      used.add(name);
      const dest = path.join(p.refs, name);
      const buf = fs.readFileSync(src);
      // A UTF-16 document's bytes ARE searched (both flavours), so it is copied
      // even when it quotes nothing; only a file that is neither text nor a
      // UTF-16 document cannot be searched exhaustively.
      const unsearchable = maskable.length > 0 && buf.includes(0) && utf16Kind(buf) === undefined;
      if (unsearchable) {
        missing.push(`${src} (binary, not copied: its bytes cannot be searched for a secret value)`);
        return;
      }
      fs.writeFileSync(dest, maskable.length > 0 ? redactBytes(buf, maskable) : buf);
    });
    if (missing.length > 0) fs.writeFileSync(path.join(p.refs, "MISSING.txt"), `${missing.join("\n")}\n`);
  }
  fs.writeFileSync(
    p.meta,
    // Plan 01a: the title is plan prose like any other, and the same value is
    // redacted wherever a title is displayed (`tt list`, the Emacs run label),
    // so the file itself must not keep it either (finding M-20).
    JSON.stringify(
      redactRecord(
        {
          title: plan.title,
          repo: plan.repo,
          createdAt: new Date().toISOString(),
          status: "created",
          runnerRevision: runnerRevision(),
        },
        maskable,
      ),
      null,
      2,
    ),
  );
  return runDir;
}

/** Phase 1b round-of-review item 2: `events.jsonl` holds transitions,
 * intents, completions and applied commands only — never a full state
 * snapshot (design §9.2: it "stays small" and "is the only source of truth
 * for recovery"). State is rebuilt by folding every logged `"event"` record
 * through `reduce()`, starting from the initial state derived from the
 * plan's phase 0 and a one-time `"init"` record (the run id and the
 * integration head observed at the *first* start — a conductor-only fact
 * recovery needs, logged once, not re-derived from a possibly-moved branch
 * on every restart). `tt status` (cli.ts) calls this directly; so does
 * `Conductor.start()`. Throws if a logged event no longer reduces cleanly
 * from the same base — a conductor/log bug, not something to paper over. */
/** `lenient`: for read-only views (tt state/status) of runs written by an
 * older runner revision; recovery always folds strictly. */
export function rebuildState(runDir: string, plan: RunPlanFile, opts: { lenient?: boolean } = {}): State {
  const p = runPaths(runDir);
  const { records } = readLog(p.events);
  const initRecord = records.find((r) => r.kind === "init");
  let runId: string;
  let integrationHead: string;
  let initSeats: Seats | undefined;
  if (initRecord) {
    const init = initRecord.event as { runId: string; integrationHead: string; seats?: Seats };
    runId = init.runId;
    integrationHead = init.integrationHead;
    initSeats = init.seats;
  } else {
    runId = randomUUID().slice(0, 8);
    integrationHead = currentHead(plan.repo, plan.integrationBranch);
  }
  return foldEvents(
    initialState(runId, plan.phases[0], integrationHead, plan.ownerDirectives ?? [], initSeats),
    records,
    opts.lenient === true,
  );
}

/** Plan 3b: one entry per phase-state change, and one per finished review
 * round (the round's candidate and why it was not accepted, or "accepted").
 * Folded the same lenient way as the read-only `rebuildState`. */
export interface Timeline {
  state: State;
  phases: Array<{ phase: string; at: string }>;
  rounds: Array<{ round: number; candidateSha: string; outcome: string }>;
  /** When an attempt was interrupted and restarted in the same phase (a
   * conductor stop/resume or a crash recovery). The worker's deadline starts
   * again at each, so the pipeline counts the current stage from the last one
   * (program 14: 14g showed "over by 22m" right after a resume, counting the
   * whole time it was stopped). */
  restarts?: string[];
  /** Plan 06c (A5): the instants a conductor stop was recorded, so the span
   * computation can exclude a stopped interval from a stage's duration. */
  stops?: string[];
}

/** Plan 06c (A5): the instants a new stage segment starts. A clean `tt stop`
 * writes a `stop` record and the resume writes no event of its own, so the
 * FIRST event after a stop is the resume; a conductor that starts on an
 * existing run writes a `resume` record. An interrupted attempt and an
 * environment unblock are new segments too. Pure, so the rule is unit-tested
 * directly rather than inferred from a hand-built timeline. */
export function restartInstants(records: readonly LogRecord[]): string[] {
  const out: string[] = [];
  let stopped = false;
  for (const record of records) {
    if (record.kind === "stop") {
      stopped = true;
      continue;
    }
    if (record.kind === "resume") {
      out.push(record.ts);
      stopped = false;
      continue;
    }
    if (record.kind !== "event") continue;
    const push = (ts: string) => {
      if (out[out.length - 1] !== ts) out.push(ts);
    };
    if (stopped) {
      push(record.ts);
      stopped = false;
    }
    const type = (record.event as { type?: string }).type;
    if (type === "ATTEMPT_INTERRUPTED" || type === "RUN_RESUMED") push(record.ts);
  }
  return out;
}

function timelineFromRecords(records: readonly LogRecord[], plan: RunPlanFile): Timeline {
  const init = records.find((r) => r.kind === "init")?.event as { runId: string; integrationHead: string; seats?: Seats } | undefined;
  let state = initialState(init?.runId ?? "", plan.phases[0], init?.integrationHead ?? "", plan.ownerDirectives ?? [], init?.seats);
  const phases: Timeline["phases"] = [];
  const rounds: Timeline["rounds"] = [];
  const restarts = restartInstants(records);
  const stops = records.filter((r) => r.kind === "stop").map((r) => r.ts);
  for (const record of records) {
    if (record.kind !== "event") continue;
    const before = state;
    const result = reduce(state, record.event);
    if (!result.ok) continue;
    state = result.state;
    const type = (record.event as { type?: string }).type;
    const prevC = before.phase.candidate?.sha;
    if (type === "FREEZE_COMPLETED" && prevC) {
      const reasons = notAcceptedReasons(before.phase);
      rounds.push({
        round: rounds.length + 1,
        candidateSha: prevC,
        outcome: reasons.length > 0 ? `not accepted: ${reasons.join("; ")}` : "not accepted",
      });
    }
    // Plan 05i: READY is the pre-start state, not a stage, and the
    // environment preflight records its tools (`ENV_CHECKED`) before any
    // phase change. Seeding an arrival from a non-phase-changing first event
    // would start the run's timeline at READY, which the pipeline renders as
    // an `implement` span — so the baseline paid before the worker would read
    // as implementation (the very defect plan 04a's own test guards). Only a
    // real phase change (or a non-READY initial phase, defensively) is an
    // arrival.
    if (state.phase.phase !== before.phase.phase || (phases.length === 0 && state.phase.phase !== "READY")) {
      phases.push({ phase: state.phase.phase, at: record.ts });
    }
  }
  return { state, phases, rounds, restarts, stops };
}

export function rebuildTimeline(runDir: string, plan: RunPlanFile): Timeline {
  const { records } = readLog(runPaths(runDir).events);
  return timelineFromRecords(records, plan);
}

/** The timeline AND the reduced events, from one read of the control log.
 * The balance metrics need both; building them from a single snapshot keeps
 * the projection consistent with the timeline it is drawn beside, and avoids
 * re-parsing the whole log once per status beat. */
export function rebuildTimelineWithEvents(runDir: string, plan: RunPlanFile): { timeline: Timeline; events: MetricEvent[] } {
  const { records } = readLog(runPaths(runDir).events);
  return { timeline: timelineFromRecords(records, plan), events: metricEvents(records) };
}

/** Folds every `"event"`-kind record in `records` (in order) through
 * `reduce()` onto `base`. Throws if one no longer reduces cleanly — see
 * `rebuildState`'s doc comment for why that is always a bug, not something
 * to paper over. */
function foldEvents(base: State, records: readonly LogRecord[], lenient = false): State {
  let state = base;
  for (let i = 0; i < records.length; i++) {
    const record = records[i];
    if (record.kind !== "event") continue;
    // A runner before the stale-review fix logged an event and THEN its
    // rejection; the live conductor never applied it, so neither does
    // recovery (otherwise such a run could never be resumed).
    if (records[i + 1]?.kind === "rejected") continue;
    const result = reduce(state, record.event);
    if (!result.ok && lenient) {
      // Read-only views of a run written by an older runner revision: skip
      // what the current core refuses (e.g. ballots on records it now
      // supersedes) instead of failing to show the run at all.
      continue;
    }
    if (!result.ok) {
      throw new Error(`recovery: logged event ${JSON.stringify(record.event)} no longer reduces: ${result.reason}`);
    }
    state = result.state;
  }
  return state;
}

/** Per-file `git diff --numstat` totals of a worktree against its HEAD,
 * untracked files counted as all-added lines; undefined if git fails. */
function worktreeNumstat(worktree: string): Promise<Map<string, [number, number]> | undefined> {
  const run = (args: string[]) =>
    new Promise<string | undefined>((resolve) =>
      execFile("git", ["-C", worktree, ...args], { maxBuffer: 16 * 1024 * 1024 }, (err, out) => resolve(err ? undefined : out)),
    );
  return Promise.all([run(["diff", "--numstat", "HEAD"]), run(["ls-files", "--others", "--exclude-standard", "-z"])]).then(
    ([diff, untracked]) => {
      if (diff === undefined) return undefined;
      const totals = new Map<string, [number, number]>();
      for (const line of diff.split("\n")) {
        const m = line.match(/^(\d+|-)\t(\d+|-)\t(.+)$/);
        if (m) totals.set(m[3], [m[1] === "-" ? 0 : Number(m[1]), m[2] === "-" ? 0 : Number(m[2])]);
      }
      for (const file of (untracked ?? "").split("\0")) {
        if (!file) continue;
        try {
          const text = fs.readFileSync(path.join(worktree, file), "utf8");
          totals.set(file, [text.length === 0 ? 0 : text.split("\n").length - (text.endsWith("\n") ? 1 : 0), 0]);
        } catch {
          // vanished between listing and reading
        }
      }
      return totals;
    },
  );
}

// ---------------------------------------------------------------------------
// The conductor
// ---------------------------------------------------------------------------

interface AgentHandle {
  agent: PiAgent;
  role: Role;
  agentId: string;
  helloResolve: (result: HelloResult) => void;
  helloPromise: Promise<HelloResult>;
  shGroups: Set<number>;
  /** Resolved once a submission (submit_phase for a worker, submit_review
   * for a reviewer) has been accepted, so the attempt-level race can stop
   * waiting on timeouts/no_submission for this dispatch. */
  doneResolve: () => void;
  donePromise: Promise<void>;
  /** Work packet 2a: resolved once a real reviewer's turn-1 `submit_discovery`
   * has been accepted (see #onSubmit) — `#runReview`'s two-turn flow awaits
   * this before ever sending the turn-2 prompt. Unused in `stubReviews`
   * mode or for a worker handle. */
  discoveryResolve: () => void;
  discoveryPromise: Promise<void>;
  /** Plan 06g2: the lane this agent belongs to (a lane worker or a lane
   * reviewer), so its submissions are recorded against the lane's candidate
   * rather than the phase's own. Absent on the single-candidate loop. */
  lane?: string;
  /** Plan 06g2: the round this agent belongs to. */
  laneRound?: number;
  /** Plan 06g2: a lane worker's `submit_phase` payload, captured for the
   * winner's hand-off (the round's lanes never move the phase themselves). */
  laneSubmission?: {
    disclosures: DecisionDisclosure[];
    prior?: PriorDecisionStatement[];
    dispute?: CriterionDispute;
  };
  /** Plan 06g2: a lane worker's `submit_coverage` payload, kept for the
   * winner's hand-off. */
  laneCoverage?: unknown;
  /** Plan 06g2: the seat this agent votes as, in the round's pick turn. */
  pickSeat?: string;
  /** Plan 06h (A3): true when this pick turn is the top-two revote. */
  pickRevote?: boolean;
  /** Plan 06g2: the (round, lane, candidate, seat) a lane review records its
   * `submit_review` against. */
  laneReview?: { round: number; lane: string; sha: string; seat: string };
  /** Plan 06g2: a lane reviewer's turn-1 discoveries, kept per lane until the
   * winner's hand-off applies them (the phase has no candidate to bind them to
   * while the round runs). */
  laneDiscoveries?: DecisionDisclosure[];
  /** Plan 06g2: waiters for this agent's next `agent_settled`, so a lane
   * review can wait out each of its two turns (the same technique
   * `#runReview` uses for the single-candidate path). Set for lane agents. */
  settleWaiters?: Array<() => void>;
  /** Plan 04a: the message type this evaluator handles. */
  messageType?: MessageType;
  /** Plan 04b: the raw blocker this panel seat votes on, and its seat number.
   * Set only for a `panel` handle. */
  blockerId?: string;
  panelSeat?: number;
  /** Plan 01d: the records this dispatch's turn-2 prompt demanded a ballot
   * for (id → its one-line choice), captured when that prompt was built and
   * never recomputed, so a record added after it (a late discovery) is never
   * demanded. Absent for a worker, a stub review, or before turn 2. */
  demandedBallots?: Map<string, string>;
  /** Plan 01d: how many incomplete `submit_review` submissions this dispatch
   * has already had rejected (at most MAX_INCOMPLETE_REVIEW_REJECTIONS). */
  incompleteReviewRejections?: number;
  /** Plan 05c: how many `submit_evaluation` submissions this evaluator has
   * had rejected for a title that ends mid-word or was cut to fit the cap
   * (at most MAX_INCOMPLETE_REVIEW_REJECTIONS, then accepted as is). */
  evaluationTitleRejections?: number;
  /** Plan 06b (OD-2 A3): how many times this evaluator has been re-prompted
   * for a missing item check (at most once, then the item is recorded
   * `unchecked`). */
  itemCheckRejections?: number;
  /** A `submit_review` from this agent is being recorded. A second call
   * while it is (run cc1992e2: B called the tool twice) is refused. */
  reviewInFlight?: boolean;
  /** Plan 05k: the item ids this brief agent was dispatched for, and the ones
   * it actually submitted. The agent is done only when it submitted every id
   * it was asked for — a retried backstop already has a brief, so existence
   * alone must never end the pass (finding M-41). */
  briefInFlightIds?: Set<string>;
  briefSubmitted?: Set<string>;
}

/** `#applyReviewFindingsAndBallots` found the round over after an await. */
const STALE_REVIEW = "stale review";

export class Conductor {
  #runDir: string;
  #paths: ReturnType<typeof runPaths>;
  /** Plan 05j: the last lint violation signature written/logged, so the
   * conductor does not append a REVIEW_LINT_FAILED on every render beat. */
  #lastLintSignature = "";
  /** Plan 05j: re-entrancy guard while #syncEntries appends entry events. */
  #syncingEntries = false;
  /** Decision briefs: the item ids the current brief-writing agent was asked
   * for, so #refreshBriefs does not dispatch a second agent for them. */
  #briefInFlight = new Set<string>();
  /** Plan 05k (OD-6): the park episode at which each backstop was recorded,
   * keyed `<candidateSha>::<itemId>`. The single retry fires only when a LATER
   * park happens on the same candidate, never on the next beat of the same
   * one. */
  #briefBackstopEpisode = new Map<string, number>();
  /** Plan 05j: candidates whose curator agent is in flight. */
  #curatorInFlight = new Set<string>();
  #plan: RunPlanFile;
  #deadlines: Deadlines;
  #piCommand: string | undefined;
  #piArgsPrefix: string[];
  #piCommandFor: ((role: Role) => string | undefined) | undefined;
  #piArgsPrefixFor: ((role: Role) => string[] | undefined) | undefined;
  #providerModelFor: ((role: Role, seat?: string | number) => { provider?: string; model?: string } | undefined) | undefined;
  #extraEnv: NodeJS.ProcessEnv;
  /** Plan 05i: the environment `#runEnvPreflight` resolves tools in. */
  #preflightEnv: NodeJS.ProcessEnv;
  #piEnvFor: ((role: Role, agentId: string) => NodeJS.ProcessEnv | undefined) | undefined;
  #stubReviews: boolean;
  #briefsEnabled: boolean;
  #probeReuse: boolean;
  /** Plan 01b: the clock `#checkNotifications` reads (injectable). */
  #now: () => number;
  /** Plan 01b: how many times the phase has entered AWAITING_OWNER so far.
   * Seeded from the log at `start()` and incremented on each new park, so the
   * notification key names the PARK, not the newest open owner request: an
   * owner resolving one request of several stays in the same episode and is
   * not re-banner-stormed, while a genuinely new park is announced again. */
  #awaitingEpisode = 0;
  /** Plan 01f: the machine-wide gate lock's path (default
   * `~/.tradeoffs-trace/gate.lock`). Held only while the gate command runs,
   * so two phases never gate at once. */
  #gateLockPath: string;
  /** Plan 06g2 (C2): the machine-wide lock each lane candidate's checks run
   * under (default `~/.tradeoffs-trace/check.lock`), held only while one
   * candidate is checked. */
  #checkLockPath: string;
  /** Plan 06g2: the winner's hand-off — the lane build and round whose
   * reviews are promoted into the phase's review slots once the probe has
   * passed (the FSM's REVIEWING state is only reachable then). */
  #pendingLaneWinner:
    | { round: number; lane: string; sha: string; build: LaneBuild; reviews: Array<{ seat: string; review: Review }> }
    | undefined;
  /** Plan 06g2: each lane candidate's combined check output this round, so
   * the winner's hand-off can resolve its `test` verifies without re-running
   * the checks. */
  #laneCheckOutputs = new Map<string, string>();
  /** Plan 06g2: each lane candidate's own `test`-verify resolution. */
  #laneCheckResolutions = new Map<string, ReturnType<typeof resolveTestVerifies>>();
  /** Plan 06g2: this round's lane builds, keyed `<round>-<lane>` — the lane
   * review prompts need each lane's own disclosures. */
  #laneBuilds = new Map<string, LaneBuild>();
  /** Plan 06g2: each lane candidate's turn-1 discoveries, keyed
   * `<round>-<lane>` then by seat. The phase has no candidate to bind them to
   * while the round runs, so the winner's are applied at the hand-off. */
  #laneDiscoveries = new Map<string, Map<string, DecisionDisclosure[]>>();
  /** Plan 06g2: each lane candidate's discovery-id plan, computed ONCE — when
   * the first of its turn-2 prompts is built — and reused at the hand-off, so
   * the ids the seats balloted are the ids that appear even though the freeze
   * grows the phase's decision list in between. */
  #laneDiscoveryPlans = new Map<string, Array<{ seat: string; disclosure: DecisionDisclosure; id: string; index: number }>>();
  /** Plan 06g2: the per-candidate discovery barrier, keyed `<round>-<lane>`. */
  #laneBarriers = new Map<string, { arrived: Set<string>; waiters: Array<() => void> }>();
  /** Plan 01a: the plan's declared secret names; the values resolved from
   * the conductor's own environment at start (`#secretValues` is every set
   * value, for the agents' environment; `#secretMaskable` is the subset long
   * enough to mask and to match in a command); and the names that were unset
   * or unusable then (reported by `tt status`, never a reason not to run). */
  #secretNames: string[] = [];
  #secretValues: Secret[] = [];
  #secretMaskable: Secret[] = [];
  #missingSecrets: string[] = [];
  #tooShortSecrets: string[] = [];
  #log!: EventLog;
  #lock: Lock | undefined;
  #socket!: RunSocketServer;
  #state!: State;
  #agents = new Map<string, AgentHandle>();
  /** Work packet 2a: agent ids (reviewer dispatches) whose turn-1
   * `submit_discovery` has already been accepted — gates `submit_review`
   * (turn 2) in non-stub mode. See `#onSubmit`. */
  #discoverySubmitted = new Set<string>();
  #driving = false;
  #redriveRequested = false;
  /** Plan 04a: while applying a batch of events that must become visible
   * together (the evaluator's message outcomes plus its own outcome record),
   * suppress the per-event `drive()` so next() cannot act on a half-applied
   * batch — the batch helper drives once at the end. */
  #driveSuspended = false;
  /** Plan 04a: the current baseline action's id, so each baseline command's
   * process group can be recorded for crash recovery (advisory A-15). */
  #baselineActionId: string | undefined;
  /** Plan 06c: the parent candidate whose passing check record this run
   * reused as its baseline (A3), named on BASELINE_COMPLETED. */
  #baselineReusedFrom: string | undefined;
  /** B-24: the baseline stage timed out; a late run must not be recorded. */
  #baselineTimedOut = false;
  #runStartedAt = Date.now();
  #budgetTimer: NodeJS.Timeout | undefined;
  /** design §8.1/§8.2: "the execution budget counts only time spent
   * executing" — ms still owed before RUN_BUDGET_EXCEEDED fires, decremented
   * as the timer runs and paused (timer cleared, remaining ms preserved)
   * whenever the phase is AWAITING_OWNER. `undefined` when no runBudgetMs
   * was configured. See `#syncBudgetTimer`. */
  #budgetRemainingMs: number | undefined;
  #budgetTimerStartedAt: number | undefined;
  /** Tokens used so far, per agent id, for design §8.1's "run execution
   * budget ... tokens" (summed for the run-wide total) and its "per-attempt
   * token cap" (read for one agent at a time). Updated from each agent's
   * `message_update` RPC events — see `#trackRunTokens`. Assumes `usage`'s
   * count is cumulative for that agent's own session, which is the common
   * shape; see that method's own comment. */
  #agentTokenTotals = new Map<string, number>();
  #closed = false;
  /** The in-flight (or finished) teardown sequence `stop()` is running —
   * memoized the same way `PiAgent#terminate()` memoizes its own, and for
   * the same reason: `#maybeAutoStop()` fires `stop()` itself, fire-and-
   * forget, the instant a phase reaches DONE/BLOCKED, so an external
   * caller's own `await conductor.stop()` right after observing that same
   * state change would otherwise see `#closed` already `true` and return
   * immediately — appearing to have finished cleanup while the *real*
   * teardown (terminating every live agent, which can take the full
   * abort/term grace periods) is still running in the background,
   * unawaited by anyone. A real bug live-single-phase.test.ts caught: it
   * observed DONE, called `stop()` itself, and asserted no agent process
   * survived — before the auto-stop it raced against had actually
   * finished killing the reviewers. */
  #stopping: Promise<void> | undefined;
  /** Every agent `sh` process group still running, across all agents —
   * including ones whose handle is already gone (a force-killed worker's
   * orphaned command). Removed when the command exits, so `stop()` only
   * ever signals groups this conductor knows are live, never a recycled pgid
   * from an old log record. */
  #liveShGroups = new Set<number>();
  /** ODP-3: the run's own STAGE command groups (a check, the baseline, the
   * gate) that are still running, each with the time it was recorded. */
  #liveStageGroups = new Map<number, number>();
  #stopRequested = false;
  #integrationBranch: string;
  /** The worker handle a `submit_phase` was just accepted from — set by
   * #onSubmit, consumed by #runFreeze (the "freeze" action's dispatch runs
   * synchronously off the same #applyEvent call, so this is never stale). */
  #activeWorkerHandle: AgentHandle | undefined;
  /** design §9.3's "agent attempt" reconciliation for a worker: "new attempt
   * on the SAME session file with an interruption note." Set by
   * `#reconcileInFlight`'s `dispatch_worker` case from the crashed attempt's
   * own intent record, consumed once by the very next `#runWorkerAttempt`
   * call (which reuses this directory instead of minting a fresh one, and
   * appends an interruption note to the prompt), then cleared. */
  #recoveredSessionDir: string | undefined;
  /** design §9.3: the inbox command ids already appended to the log, seeded
   * from the log at `start()` and updated on every apply. A file whose id is
   * in this set is only moved to applied/, never applied a second time. */
  #appliedCommandIds = new Set<string>();
  /** Plan 2d: steer command ids whose RPC send is in flight. A second scan
   * must leave the file alone (it is neither applied nor failed yet), and
   * only the acknowledgement/failure path moves it to applied/. */
  #steerInFlight = new Set<string>();
  /** The inbox poll timer; cleared by `#doStop`. */
  #inboxTimer: NodeJS.Timeout | undefined;
  /** Plan 03b: the periodic `views/status.txt` refresh, once a second while
   * the conductor runs. `buildView` rebuilds the timeline, so it stays off
   * the message-event path (the discovery-barrier tests are timing-sensitive)
   * but must still track a silent execute stage (B-5), not only message
   * events. */
  #statusTimer: NodeJS.Timeout | undefined;
  /** Plan 05i: true while `#envPreflightGate` applies its events, so its
   * `ENV_CHECKED` on an already-terminal (DONE/BLOCKED) run does not trip the
   * auto-stop before `start()` finishes scanning the inbox. */
  #envGateActive = false;
  /** Plan 06c (R6): the tools the preflight could not find, resolved once per
   * conductor and named in every agent prompt. */
  #agentToolsMissing: string[] | undefined;

  constructor(opts: ConductorOptions) {
    this.#runDir = opts.runDir;
    this.#paths = runPaths(opts.runDir);
    this.#plan = opts.plan;
    this.#deadlines = { ...DEFAULT_DEADLINES, ...opts.deadlines };
    this.#piCommand = opts.piCommand;
    this.#piCommandFor = opts.piCommandFor;
    this.#piArgsPrefixFor = opts.piArgsPrefixFor;
    this.#piArgsPrefix = opts.piArgsPrefix ?? [];
    this.#extraEnv = opts.extraEnv ?? {};
    this.#preflightEnv = opts.preflightEnv ?? process.env;
    this.#piEnvFor = opts.piEnvFor;
    this.#providerModelFor = opts.providerModelFor;
    this.#stubReviews = opts.stubReviews ?? false;
    this.#briefsEnabled = opts.briefs ?? false;
    this.#probeReuse = opts.probeReuse ?? true;
    this.#now = opts.now ?? Date.now;
    this.#gateLockPath = opts.gateLockPath ?? path.join(os.homedir(), ".tradeoffs-trace", "gate.lock");
    this.#checkLockPath = opts.checkLockPath ?? path.join(os.homedir(), ".tradeoffs-trace", "check.lock");
    this.#integrationBranch = opts.plan.integrationBranch;
    this.#budgetRemainingMs = this.#deadlines.runBudgetMs;
  }

  get state(): State {
    return this.#state;
  }

  get runDir(): string {
    return this.#runDir;
  }

  /** The pgid of every currently-live agent process, keyed by role and
   * agent id — read-only, for tests that need to signal a specific agent's
   * process group directly (e.g. the force-kill-shell exit-gate test).
   * Not used by the conductor itself. */
  get agentPgids(): Array<{ agentId: string; role: Role; pgid: number }> {
    return [...this.#agents.values()].map((h) => ({ agentId: h.agentId, role: h.role, pgid: h.agent.pgid }));
  }

  /** Every agent's tracked token total this run has seen so far (design
   * §8.1's own token bookkeeping — `#trackRunTokens`), keyed by agent id —
   * read-only, for tests/records that need to report token usage (e.g.
   * test/live/live-single-phase.test.ts's own recorded evidence). Not
   * cleared when an agent terminates, so a finished run's totals are still
   * readable afterwards. */
  get agentTokenTotals(): Record<string, number> {
    return Object.fromEntries(this.#agentTokenTotals);
  }

  #resolvePiCommand(role: Role): string | undefined {
    return this.#piCommandFor?.(role) ?? this.#piCommand;
  }

  #resolvePiArgsPrefix(role: Role): string[] {
    return this.#piArgsPrefixFor?.(role) ?? this.#piArgsPrefix;
  }

  /** Acquires the run lock (fails fast if another conductor holds it),
   * rebuilds state from `events.jsonl`, starts the run socket, and kicks
   * off `drive()`. */
  async start(): Promise<void> {
    this.#lock = await acquireLock(this.#paths.lock);
    // Plan 01a: the plan only ever names its secrets; the values come from
    // this process's own environment (never from the plan, a prompt or a
    // file), and every writer below redacts them. A declared name that is
    // unset is recorded and reported — the run still starts.
    this.#secretNames = secretNames(this.#plan.secrets);
    const resolved = resolveSecrets(this.#secretNames, process.env, this.#plan.envFile);
    this.#secretValues = resolved.values;
    this.#secretMaskable = resolved.maskable;
    this.#missingSecrets = resolved.missing;
    this.#tooShortSecrets = resolved.tooShort;
    this.#log = new EventLog(this.#paths.events, this.#secretMaskable);

    // design §2.1: assert the Pi version before ever launching it — but
    // only when the real `pi` binary is actually going to be used for at
    // least one role; tests inject fake-pi via `piCommand`/`piArgsPrefix`
    // (or, for live-single-phase's mixed run, `piCommandFor`) and never
    // touch this.
    if (this.#resolvePiCommand("worker") === undefined || this.#resolvePiCommand("reviewer") === undefined) {
      assertPiVersion();
    }

    const { records } = readLog(this.#paths.events);
    const initRecord = records.find((r) => r.kind === "init");
    if (initRecord) {
      const init = initRecord.event as { runId: string; integrationHead: string; runnerRevision?: string; seats?: Seats };
      const mine = runnerRevision();
      if (init.runnerRevision && init.runnerRevision !== mine && process.env.TT_ALLOW_RUNNER_MISMATCH !== "1") {
        throw new RunnerMismatchError(
          `run was started under runner ${init.runnerRevision}; this conductor is ${mine} — refusing to resume (reinstall that runner, or start a new run)`,
        );
      }
      this.#state = initialState(init.runId, this.#plan.phases[0], init.integrationHead, this.#plan.ownerDirectives ?? [], init.seats);
    } else {
      const head = currentHead(this.#plan.repo, this.#integrationBranch);
      const runId = randomUUID().slice(0, 8);
      // Plan 06h (A1): the init event records the run's EFFECTIVE seats (a
      // phase's own `:REVIEWERS:`/`:LEADER:` wins over the plan's), so a
      // later replay uses its own seats even after the plan changes. An old
      // log with none recorded means M, A and B.
      const phase0 = this.#plan.phases[0];
      const effectiveSeats = seatsRecordOf({
        seats: phase0.seats && phase0.seats.length > 0 ? phase0.seats : this.#plan.seats,
        leader: phase0.leader ?? this.#plan.leader,
        workers: phase0.workers ?? this.#plan.workers,
      });
      this.#log.append("init", { runId, integrationHead: head, runnerRevision: runnerRevision(), seats: effectiveSeats });
      this.#state = initialState(runId, phase0, head, this.#plan.ownerDirectives ?? [], effectiveSeats);
    }
    this.#state = foldEvents(this.#state, records);
    // Plan 06c (A5/OD-3): a conductor that starts on an existing, non-terminal
    // run records a real RUN_RESUMED event once at start-up (drive-suspended,
    // so nothing dispatches before start() is ready). The stage clock treats it
    // as the start of a new segment, so the stopped interval never counts.
    // ENV_BLOCKED keeps its own RUN_RESUMED in the preflight gate below, and a
    // budget pause keeps its own resume path.
    if (
      initRecord &&
      records.length > 1 &&
      this.#state.run === "RUN_ACTIVE" &&
      this.#state.phase.phase !== "DONE" &&
      this.#state.phase.phase !== "BLOCKED"
    ) {
      this.#driveSuspended = true;
      try {
        this.#applyEvent({ type: "RUN_RESUMED" });
      } finally {
        this.#driveSuspended = false;
      }
    }
    // Plan 05i: resolve every declared command's executable before the
    // baseline and before any agent launch. A missing tool stops the run in
    // ENV_BLOCKED (visible, and recoverable with `tt resume` once the
    // environment is fixed); it is never a code failure. A run already
    // ENV_BLOCKED whose tools now resolve is unblocked here and continues.
    if (!this.#envPreflightGate()) {
      await this.stop();
      return;
    }
    // Plan 05j: backfill the entry ledger from any messages that predate it
    // (an old log has messages but no ENTRY_OPENED), then rebuild the
    // projections. A conductor killed between an event and its projection
    // write leaves stale or missing files; the log is authoritative and this
    // restores them.
    this.#syncEntries();
    this.#writeContractProjections();
    // Plan 03b: the status view exists immediately; the coalesced write keeps
    // it fresh afterwards without rebuilding the view on every message event.
    this.#writeStatusViewSafe();
    // Plan 06f (A2): a conductor starting on an existing run republishes its
    // row so a stale `live.json` (from a crash) reflects the resumed run.
    this.#writeLive();
    // Plan 01b: seed the park-episode counter from the log, so a restarted
    // conductor keeps the same notification key for the wait it is resuming.
    // Plan 05k (OD-7): the retry gate counts only AWAITING_OWNER entries;
    // entering BLOCKED neither dispatches the retry nor spends it.
    this.#awaitingEpisode = rebuildTimeline(this.#runDir, this.#plan).phases.filter((p) => p.phase === "AWAITING_OWNER").length;
    if (this.#secretNames.length > 0) this.#recordSecrets();
    // design §9.3: "If the command ID is already in the log, the command is
    // only moved to applied/." Every applied conductor-state command's event
    // carries its inbox id, so the applied set is rebuilt from the log alone
    // after a restart — the log append is the effect, the file move is not.
    for (const record of records) {
      if (record.kind !== "event") continue;
      const id = (record.event as { commandId?: unknown } | null | undefined)?.commandId;
      if (typeof id === "string") this.#appliedCommandIds.add(id);
    }

    // design §9.3's "create worktree" reconciliation — physical-effect
    // bookkeeping only, never a core Event (the worktree exists before the
    // phase state machine has anything to dispatch). Must run before the
    // socket starts (nothing here talks to an agent) — see below for why
    // the in-flight-action reconciliation runs *after* the socket instead.
    this.#reconcileWorktree();

    this.#socket = await RunSocketServer.start(this.#paths.sock, {
      onHello: (agentId, hello) => this.#onHello(agentId, hello),
      onSubmit: (agentId, msg) => this.#onSubmit(agentId, msg),
      cwdFor: (agentId) => this.#cwdFor(agentId),
      onShIntent: (agentId, commandId, pgid) => this.#onShIntent(agentId, commandId, pgid),
      onShExit: (_agentId, _commandId, pgid) => void this.#liveShGroups.delete(pgid),
      // Plan 01a: a command carrying a secret's literal value is refused
      // here as well as in the agent's extension guard — a scripted agent
      // (fake-pi) sends its `sh` messages straight to this socket.
      refuseSh: (_agentId, command) => secretUseInCommand(command, this.#secretNames),
      shDeadline: { deadlineMs: this.#deadlines.shCommandMs, termGraceMs: this.#deadlines.termGraceMs },
      // Plan 06e (A1): a `sh` command's environment carries childEnv() plus
      // every resolved secret value, so `$NAME` works whether the environment
      // or the env file supplied the value.
      shEnv: () => this.#gateEnv(),
      onNoSubmission: (agentId) => this.#onNoSubmission(agentId),
    });

    // design §9.3's reconciliation table for every other effect: diff the
    // phase's own `inFlight` map (design §9.3's "intent" bookkeeping,
    // already tracked by core) against what actually happened on disk/in
    // the process table, and resolve each one before the first ordinary
    // `drive()` — a fresh restart with nothing pending is a no-op loop here.
    // Runs *after* the socket is listening because reconciling a
    // `dispatch_worker`/`review_*` entry can itself redispatch a new agent
    // (via `#applyEvent` -> `drive()`), and that agent's very first RPC
    // connects to this run's socket immediately.
    await this.#reconcileInFlight();

    this.#syncBudgetTimer();

    // design §9.3's conductor-state commands: read the inbox once on start
    // (a crash may have left a command un-moved), then poll while running; a
    // command is applied by appending its event to the log, so the poll must
    // run even when the phase is parked and `next()` dispatches nothing.
    this.#ensureInboxDirs();
    this.#scanInbox();
    this.#checkNotifications();
    // Reconcile above can itself reach a terminal state and fire the
    // auto-stop, so only arm the poll timer if `stop()` has not already run
    // (`#doStop` would have cleared an unset timer, and arming one here
    // afterwards would keep the process alive forever).
    if (!this.#closed) {
      this.#inboxTimer = setInterval(() => {
        this.#scanInbox();
        // The wait's one 30-minute reminder is noticed on the same beat.
        this.#checkNotifications();
      }, this.#deadlines.inboxPollMs);
      // Plan 03b: refresh views/status.txt once a second through every stage,
      // including a long silent execute, on its own beat (tests shorten the
      // inbox poll to tens of ms).
      this.#statusTimer = setInterval(() => this.#writeStatusViewSafe(), 1000);
    }

    this.drive();
  }

  // -- crash recovery (design §9.3) ----------------------------------------

  /** Plan 01a: the names the plan declared and which of them were unset when
   * this start resolved them. Names only — a value is never recorded anywhere
   * but the environment. Appended once, and again only when the answer
   * changes (a restart with the variable now exported), so `tt status` shows
   * the current truth without the log growing a record per start. */
  #recordSecrets(): void {
    const record = { declared: this.#secretNames, missing: this.#missingSecrets, tooShort: this.#tooShortSecrets };
    const { records } = readLog(this.#paths.events);
    for (let i = records.length - 1; i >= 0; i--) {
      if (records[i].kind !== "secrets") continue;
      const last = records[i].event as { declared?: unknown; missing?: unknown; tooShort?: unknown };
      const same = (a: unknown, b: unknown) => JSON.stringify(a) === JSON.stringify(b);
      if (same(last.declared, record.declared) && same(last.missing, record.missing) && same(last.tooShort, record.tooShort)) {
        return;
      }
      break;
    }
    this.#log.append("secrets", record);
  }

  /** design §9.3's "create worktree" row: "path exists at the recorded base
   * → record done; otherwise remove the partial worktree and recreate it."
   * Uses a fixed action id (`"create-worktree"`) rather than one minted per
   * call — phase 1 has exactly one worktree per run, ever, so there is
   * nothing to disambiguate. */
  #reconcileWorktree(): void {
    const ACTION_ID = "create-worktree";
    const { records } = readLog(this.#paths.events);
    const intentRecord = records.find((r) => r.kind === "intent" && r.actionId === ACTION_ID);
    const completed = records.some((r) => r.kind === "completion" && r.actionId === ACTION_ID);

    if (intentRecord && !completed) {
      const { baseSha } = intentRecord.event as { worktree: string; baseSha: string };
      if (this.#worktreeMatches(baseSha)) {
        this.#log.completion(ACTION_ID, { reused: true, baseSha });
      } else {
        removeWorktree(this.#plan.repo, this.#paths.worktree);
        crashAt("before_create_worktree");
        createWorktree(this.#plan.repo, this.#paths.worktree, baseSha);
        crashAt("after_create_worktree");
        this.#log.completion(ACTION_ID, { recreated: true, baseSha });
      }
      return;
    }

    if (this.#worktreeMatches(this.#state.phase.integrationHead)) return; // already created and completed
    if (fs.existsSync(this.#paths.worktree)) removeWorktree(this.#plan.repo, this.#paths.worktree);
    const baseSha = this.#state.phase.integrationHead;
    this.#log.intent(ACTION_ID, { worktree: this.#paths.worktree, baseSha });
    crashAt("before_create_worktree");
    createWorktree(this.#plan.repo, this.#paths.worktree, baseSha);
    crashAt("after_create_worktree");
    this.#log.completion(ACTION_ID, { created: true, baseSha });
  }

  #worktreeMatches(baseSha: string): boolean {
    if (!fs.existsSync(this.#paths.worktree)) return false;
    try {
      return execFileSync("git", ["-C", this.#paths.worktree, "rev-parse", "HEAD"], { encoding: "utf8" }).trim() === baseSha;
    } catch {
      return false;
    }
  }

  /** Every recorded `sh` group pgid for `agentId` (from `#onShIntent`'s own
   * `sh-<agentId>-<pgid>` intent records — those never get a completion of
   * their own, so `pendingIntents` is not the right lens for them; this
   * reads every logged one regardless, since a crashed agent's shell groups
   * are exactly what design §9.3's "agent attempt" row means by "every
   * recorded shell group"). */
  #recordedShGroups(records: readonly LogRecord[], agentId: string): Array<{ pgid: number; ts: number }> {
    const prefix = `sh-${agentId}-`;
    return records
      .filter((r) => r.kind === "intent" && typeof r.actionId === "string" && r.actionId.startsWith(prefix))
      .map((r) => ({ pgid: (r.event as { pgid: number }).pgid, ts: Date.parse(r.ts) }))
      .filter((g) => Number.isFinite(g.pgid));
  }

  /** Signals a process group read from a PREVIOUS conductor's log, but only
   * when its leader started no later than the record that named it. A pgid
   * the OS recycled after the crash belongs to a foreign process; A2/C3
   * forbid signalling it, so it is logged and left alone. Every recovery
   * kill goes through here — the one place crash recovery signals a group. */
  async #killRecordedGroup(pgid: number, recordedAtMs: number): Promise<void> {
    if (!Number.isFinite(recordedAtMs) || !groupStartedBefore(pgid, recordedAtMs)) {
      this.#log.append("sweep_skipped", {
        pgid,
        reason: "pgid leader did not start at the recorded time (recycled or foreign); not signalled",
      });
      return;
    }
    await killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined);
  }

  /** Every process group THIS RUN recorded at spawn: the pgid of every intent
   * record in the log, plus every live agent and shell group. A2 (plan 06e):
   * the sweep may signal only these — a process in any other group is
   * reported as HELD, never signalled. */
  #ownPgids(): number[] {
    const pgids = new Set<number>();
    try {
      for (const r of readLog(this.#paths.events).records) {
        if (r.kind !== "intent") continue;
        const pgid = (r.event as { pgid?: unknown }).pgid;
        if (typeof pgid === "number" && Number.isFinite(pgid)) pgids.add(pgid);
      }
    } catch {
      // an unreadable log: fall back to the live handles below
    }
    for (const handle of this.#agents.values()) {
      pgids.add(handle.agent.pgid);
      for (const g of handle.shGroups) pgids.add(g);
    }
    for (const g of this.#liveShGroups) pgids.add(g);
    return [...pgids];
  }

  /** design §9.3's per-effect reconciliation table, driven off
   * `phase.inFlight` (an intent recorded, with no matching completion, is
   * exactly what an in-flight entry with no live dispatcher means on a
   * restart). One entry per outstanding effect; each case restores the
   * physical world (kill processes, sweep, discard a probe) and then emits
   * whichever core event corresponds to that row — reusing the very same
   * `*_INTERRUPTED`/`FREEZE_COMPLETED`/`PUBLISH_*` events an ordinary,
   * uninterrupted run already produces, so no new core vocabulary was
   * needed for this packet (see the README's "Item 1c" section). */
  async #reconcileInFlight(): Promise<void> {
    const inFlight = { ...this.#state.phase.inFlight };
    for (const key of Object.keys(inFlight) as InFlightKey[]) {
      const entry = inFlight[key];
      if (!entry) continue;
      await this.#reconcileOne(key, entry.actionId);
    }
  }

  async #reconcileOne(key: InFlightKey, actionId: string): Promise<void> {
    const { records } = readLog(this.#paths.events);
    const intentRecord = records.find((r) => r.kind === "intent" && r.actionId === actionId);
    const payload = (intentRecord?.event ?? {}) as Record<string, unknown>;

    if (key === "run_baseline") {
      // Plan 04a: a conductor died during BASELINE. Kill any orphaned baseline
      // command (recorded as `baseline-sh-<actionId>-<pgid>`), then re-dispatch
      // once; a second loss takes the timed-out path (advisory A-15).
      const prefix = `baseline-sh-${actionId}-`;
      for (const rec of records) {
        if (rec.kind !== "intent" || typeof rec.actionId !== "string" || !rec.actionId.startsWith(prefix)) continue;
        const pgid = (rec.event as { pgid?: number }).pgid;
        if (typeof pgid === "number") await this.#killRecordedGroup(pgid, Date.parse(rec.ts));
      }
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      this.#applyEvent(
        this.#state.phase.baseline?.interruptedOnce ? { type: "BASELINE_TIMED_OUT" } : { type: "BASELINE_INTERRUPTED" },
      );
      return;
    }

    if (key.startsWith("dispatch_evaluation_")) {
      // Plan 04a: a conductor died while ONE message type's evaluator ran.
      // Kill whatever survived and re-dispatch that type once; a second loss
      // publishes that type's raw messages `unevaluated` and settles it.
      const messageType = key.slice("dispatch_evaluation_".length) as MessageType;
      const pgid = payload.pgid as number | undefined;
      if (pgid !== undefined) await this.#killRecordedGroup(pgid, Date.parse(intentRecord?.ts ?? ""));
      for (const shGroup of this.#recordedShGroups(records, payload.agentId as string)) {
        await this.#killRecordedGroup(shGroup.pgid, shGroup.ts);
      }
      this.#log.completion(actionId, { messageType, interrupted: true, reason: "crash-recovery" });
      this.#applyEvent(
        this.#state.phase.evaluation?.types?.[messageType]?.interruptedOnce
          ? { type: "EVALUATION_TIMED_OUT", messageType }
          : { type: "EVALUATION_INTERRUPTED", messageType },
      );
      return;
    }

    if (key.startsWith("dispatch_panel_")) {
      // Plan 04b: a conductor died while one panel seat voted. Kill whatever
      // survived and treat the seat exactly like a timeout — it is retried
      // once, and a second loss makes it unavailable.
      const rest = key.slice("dispatch_panel_".length);
      const sep = rest.lastIndexOf("_");
      const blockerId = rest.slice(0, sep);
      const seat = Number(rest.slice(sep + 1));
      const pgid = payload.pgid as number | undefined;
      if (pgid !== undefined) await this.#killRecordedGroup(pgid, Date.parse(intentRecord?.ts ?? ""));
      for (const shGroup of this.#recordedShGroups(records, payload.agentId as string)) {
        await this.#killRecordedGroup(shGroup.pgid, shGroup.ts);
      }
      this.#log.completion(actionId, { blockerId, seat, interrupted: true, reason: "crash-recovery" });
      this.#panelSeatUnavailable(blockerId, seat, this.#state.phase.candidate?.sha, "the conductor died while the seat voted");
      return;
    }

    if (key.startsWith("dispatch_round_panel_")) {
      // Plan 05e: a conductor died while a round-panel seat voted. Kill what
      // survived and treat the seat like a timeout — retried once; a second
      // loss makes it unavailable, and the remaining real votes decide.
      const seat = Number(key.slice("dispatch_round_panel_".length));
      const pgid = payload.pgid as number | undefined;
      if (pgid !== undefined) await this.#killRecordedGroup(pgid, Date.parse(intentRecord?.ts ?? ""));
      for (const shGroup of this.#recordedShGroups(records, payload.agentId as string)) {
        await this.#killRecordedGroup(shGroup.pgid, shGroup.ts);
      }
      this.#log.completion(actionId, { seat, interrupted: true, reason: "crash-recovery" });
      this.#roundPanelSeatUnavailable(seat, "the conductor died while the seat voted");
      return;
    }

    if (key === "dispatch_worker") {
      const pgid = payload.pgid as number | undefined;
      if (pgid !== undefined) await this.#killRecordedGroup(pgid, Date.parse(intentRecord?.ts ?? ""));
      for (const shGroup of this.#recordedShGroups(records, payload.agentId as string)) {
        await this.#killRecordedGroup(shGroup.pgid, shGroup.ts);
      }
      const sweepResult = await sweep(this.#paths.worktree, { ownPgids: this.#ownPgids(), exceptPids: [] });
      this.#log.append("sweep", sweepResult);
      // Plan 06g2: a crash during a lane round left up to two lane worktrees
      // (and their own agents) behind; each is swept inside its own tree, so a
      // lane's survivors can never be another lane's. The round itself is
      // re-dispatched from scratch (ROUND_STARTED replaces the partial round).
      for (const lane of this.#laneRoundEnabled() ? this.#lanes() : []) {
        const worktree = this.#laneWorktree(lane);
        if (!fs.existsSync(worktree)) continue;
        const laneSweep = await sweep(worktree, {
          ownPgids: this.#ownPgids(),
          exceptPids: [],
          cwdUnder: this.#laneSweepRoot(worktree),
        });
        this.#log.append("sweep", { ...laneSweep, lane, reason: "crash-recovery" });
      }
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      if (typeof payload.sessionDir === "string") this.#recoveredSessionDir = payload.sessionDir;
      this.#applyEvent({ type: "ATTEMPT_INTERRUPTED" });
      return;
    }

    if (key === "freeze") {
      const found = findCommitByTrailer(this.#paths.worktree, actionId);
      const sweepResult = await sweep(this.#paths.worktree, { ownPgids: this.#ownPgids(), exceptPids: [] });
      this.#log.append("sweep", sweepResult);
      if (found) {
        const decisions = this.#assembleDecisions(found);
        this.#log.completion(actionId, { candidateSha: found, tainted: sweepResult.tainted, recovered: true });
        this.#applyEvent({ type: "FREEZE_COMPLETED", candidateSha: found, decisions, tainted: sweepResult.tainted });
        this.#recordBoundaryDataAndSample(found);
      } else {
        this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
        this.#applyEvent({ type: "FREEZE_INTERRUPTED" });
      }
      return;
    }

    if (key === "dispatch_probe") {
      const candidateSha = payload.candidateSha as string;
      discardProbeByBranch(this.#plan.repo, this.#state.phase.runId, candidateSha);
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      this.#applyEvent({ type: "PROBE_INTERRUPTED" });
      return;
    }

    if (key === "run_checks") {
      // Plan 06c: a conductor that stopped during CHECKING leaves the check
      // in flight. Kill the orphaned check command, then rerun the check
      // (CHECKS_INTERRUPTED clears the in-flight entry and re-dispatches).
      for (const rec of records) {
        if (rec.kind !== "intent" || typeof rec.actionId !== "string") continue;
        if (!rec.actionId.startsWith(`check-sh-${actionId}-`)) continue;
        const pgid = (rec.event as { pgid?: number }).pgid;
        if (typeof pgid === "number") await this.#killRecordedGroup(pgid, Date.parse(rec.ts));
      }
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      this.#applyEvent({ type: "CHECKS_INTERRUPTED" });
      return;
    }

    if (key === "run_gate") {
      // Plan 01f / design §9.3: an interrupted gate is neither passed nor
      // failed — kill whatever survived, discard the checkout the gate used,
      // and rerun. (The process group was recorded in the intent before the
      // command started, and the checkout branch is the probe's own name, so
      // both are recoverable from this record alone.)
      const candidateSha = payload.candidateSha as string;
      // The command's own process group was recorded at spawn (an
      // `onIntent`-logged `gate-sh-<actionId>-<pgid>` intent), plus the
      // cleanup's — a killed conductor leaves both to reap.
      const prefixes = [`gate-sh-${actionId}-`, `gate-cleanup-sh-${actionId}-`];
      for (const rec of records) {
        if (rec.kind !== "intent" || typeof rec.actionId !== "string") continue;
        if (!prefixes.some((p) => rec.actionId.startsWith(p))) continue;
        const pgid = (rec.event as { pgid?: number }).pgid;
        if (typeof pgid === "number") await this.#killRecordedGroup(pgid, Date.parse(rec.ts));
      }
      discardProbeByBranch(this.#plan.repo, this.#state.phase.runId, candidateSha);
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      this.#applyEvent({ type: "GATE_INTERRUPTED" });
      return;
    }

    if (key === "run_final_checks") {
      // Plan 06c: like an interrupted gate, an interrupted final check is
      // rerun rather than counted as passed or failed.
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      this.#applyEvent({ type: "FINAL_CHECKS_INTERRUPTED" });
      return;
    }

    if (key.startsWith("review_")) {
      const reviewer = key.slice("review_".length) as Reviewer;
      const pgid = payload.pgid as number | undefined;
      if (pgid !== undefined) await this.#killRecordedGroup(pgid, Date.parse(intentRecord?.ts ?? ""));
      this.#log.completion(actionId, { reviewer, interrupted: true, reason: "crash-recovery" });
      // design §9.3: "Reviewer: discard; start a new review." REVIEW_TIMED_OUT
      // is the existing core event for exactly that (discard this review
      // dispatch, redispatch a fresh one) — reused rather than adding a new
      // one; see reduce.ts's own "first timeout" vs "already timed out"
      // guard for the one behavioral difference this reuse implies (a
      // *second* interrupted review for the same reviewer reaches BLOCKED
      // instead of a third dispatch, exactly like two ordinary timeouts
      // would).
      this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
      return;
    }

    if (key === "publish_cas") {
      const expectedHead = payload.expectedHead as string;
      const candidateI = payload.candidateI as string;
      const actualHead = currentHead(this.#plan.repo, this.#integrationBranch);
      if (actualHead === candidateI) {
        this.#log.completion(actionId, { ok: true, recovered: true });
        this.#applyEvent({ type: "PUBLISH_COMPLETED", newHead: candidateI });
      } else if (actualHead === expectedHead) {
        this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery: retrying CAS" });
        // design §9.3: "still at H ⇒ retry CAS" — `publish_cas` is already
        // recorded as outstanding in `phase.inFlight` (from the original,
        // interrupted dispatch's own ACTION_STARTED, never cleared by a
        // completion), and PUBLISHING has no "retry in place" core event
        // the way FREEZING's FREEZE_INTERRUPTED does — so this just
        // re-attempts the same physical effect directly, under a fresh
        // actionId for its own intent/completion log identity, without
        // emitting a second ACTION_STARTED (which `reduce()` would reject:
        // `next()` never re-recommends an action already marked in-flight,
        // so the action would not be "currently outstanding" a second
        // time). The eventual PUBLISH_COMPLETED/PUBLISH_STALE clears the
        // `publish_cas` in-flight entry by key, not by actionId, so this is
        // consistent with the original dispatch's own entry.
        const retryActionId = this.#log.actionId("publish_cas");
        await this.#runPublish(retryActionId, expectedHead, candidateI);
      } else {
        this.#log.completion(actionId, { ok: false, actualHead });
        this.#applyEvent({ type: "PUBLISH_STALE", actualHead });
      }
      return;
    }
  }

  /** Shared by the ordinary freeze path (`#runFreeze`) and freeze
   * reconciliation: assembles+binds `phase.pendingDisclosures` into real
   * `Decision` records for `candidateSha` (design §6.2/§7.1's binding), and
   * validates each against `schemas/decision.schema.json`. */
  #assembleDecisions(
    candidateSha: string,
    /** Plan 06g2: assemble a LANE's decisions from its own submission instead
     * of the phase's pending one, with exactly the ids the winner's hand-off
     * will produce (same disclosures, same sha, same phase). */
    lane?: { disclosures: DecisionDisclosure[]; dispute?: CriterionDispute },
  ): Decision[] {
    const disclosures: DecisionDisclosure[] = lane ? lane.disclosures : (this.#state.phase.pendingDisclosures ?? []);
    const decisions: Decision[] = disclosures.map((d, i) => ({
      id: `D-${this.#state.phase.phaseId}-${candidateSha.slice(0, 8)}-${i + 1}`,
      version: 1,
      phaseId: this.#state.phase.phaseId,
      source: "worker",
      class: d.classProposal,
      choice: d.choice,
      whyItMatters: d.whyItMatters,
      alternatives: d.alternatives,
      recommendation: d.recommendation,
      boundCandidateSha: candidateSha,
      boundContractVersion: this.#state.phase.contract.contractVersion,
    }));
    // Plan 01g: a `criterionDispute` becomes an amendment record — a
    // `reserved` decision the reviewers vote on in turn 2 like any other. A
    // passing tally rewrites the accepted item (next()'s `apply_amendment`);
    // a failing one leaves it unchanged and never blocks acceptance.
    const dispute = lane ? lane.dispute : this.#state.phase.pendingDispute;
    // Plan 06c (R7): an amendment whose proposed text equals the current
    // criterion changes nothing, so it is never raised (no decision, no
    // message, no ballot, no status line).
    const noopDispute = dispute !== undefined && dispute.proposedWording.trim() === dispute.criterion.trim();
    if (noopDispute) {
      this.#log.append("dispute_ignored", {
        raisedBy: "worker",
        criterion: dispute.criterion,
        reason: "the proposed wording equals the current criterion",
      });
    }
    // Dedup only an IDENTICAL proposal: a different wording for the same
    // criterion is a genuinely different choice, and a second dispute with
    // the same wording is logged rather than silently dropped (A-13).
    const alreadyProposed =
      !noopDispute &&
      dispute !== undefined &&
      this.#state.phase.decisions.some(
        (d) =>
          d.amendment?.status === "proposed" &&
          d.amendment.criterion === dispute.criterion &&
          d.amendment.proposedWording === dispute.proposedWording,
      );
    if (dispute && alreadyProposed) {
      this.#log.append("dispute_ignored", {
        raisedBy: "worker",
        criterion: dispute.criterion,
        reason: "an identical amendment for this criterion is already proposed",
      });
    }
    if (dispute && !alreadyProposed && !noopDispute) {
      const short = candidateSha.slice(0, 8);
      decisions.push({
        id: `D-${this.#state.phase.phaseId}-${short}-amendment`,
        version: 1,
        phaseId: this.#state.phase.phaseId,
        source: "worker",
        class: "reserved",
        choice: dispute.proposedWording,
        whyItMatters: dispute.why,
        alternatives: [
          {
            option: dispute.criterion,
            consequence: "the letter of this criterion stays in force and no candidate can satisfy it",
          },
        ],
        recommendation: { choice: dispute.proposedWording, reason: dispute.why },
        boundCandidateSha: candidateSha,
        boundContractVersion: this.#state.phase.contract.contractVersion,
        amendment: {
          id: `AM-${this.#state.phase.phaseId}-${short}`,
          criterion: dispute.criterion,
          proposedWording: dispute.proposedWording,
          ...(this.#criterionItemId(dispute.criterion) ? { itemId: this.#criterionItemId(dispute.criterion)! } : {}),
          why: dispute.why,
          raisedBy: "worker",
          status: "proposed",
          previousContractVersion: this.#state.phase.contract.contractVersion,
        },
      });
    }
    for (const decision of decisions) {
      const result = validate(DECISION_SCHEMA, decision);
      if (!result.valid) {
        throw new Error(
          `assembled a decision that fails schemas/decision.schema.json: ${result.errors.join("; ")} (decision: ${JSON.stringify(decision)})`,
        );
      }
    }
    return decisions;
  }

  stop(): Promise<void> {
    if (!this.#stopping) {
      this.#closed = true;
      this.#stopping = this.#doStop();
    }
    return this.#stopping;
  }

  async #doStop(): Promise<void> {
    this.#stopRequested = true;
    if (this.#budgetTimer) clearTimeout(this.#budgetTimer);
    if (this.#inboxTimer) clearInterval(this.#inboxTimer);
    if (this.#statusTimer) {
      clearInterval(this.#statusTimer);
      this.#statusTimer = undefined;
    }
    // Plan 03b: flush the status view once before the log closes.
    this.#writeStatusViewSafe();
    // Plan 06f (A2): a stopped conductor is neither alive nor waiting, so its
    // row leaves `live.json` here.
    this.#writeLive();
    // design §9.3: `tt stop` ends a run's conductor cleanly — the stop event
    // is logged (with why) before anything is torn down, so the record is
    // durable even if a later step is slow or fails.
    try {
      this.#log?.append("stop", { reason: "conductor stopped", at: new Date().toISOString() });
    } catch {
      // the log may already be closed (a second stop call); never fatal.
    }
    await this.#killLiveShGroups();
    // ODP-3: a check, the baseline or the gate still running when the
    // conductor stops is signalled here, like any other stage's group — and
    // only when its leader is proven to be this run's own.
    await this.#killLiveStageGroups();
    for (const handle of this.#agents.values()) {
      // Plan 2d: a clean stop uses the short stopAbortGraceMs, not the
      // 30 s cancellation grace — `tt stop` must finish within 15 s.
      await handle.agent
        .terminate({ abortGraceMs: this.#deadlines.stopAbortGraceMs })
        .catch(() => undefined);
      // design §2.2's "conductor-owned shell": an `sh` command's process
      // group is separate from its agent's own — killing the agent does
      // not touch it. `stop()` releasing "every handle ... children"
      // (round-of-review item 1) includes these, not just the agent
      // processes themselves.
      for (const pgid of handle.shGroups) {
        await killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined);
      }
    }
    // A group whose agent handle was already dropped (a force-killed
    // worker's orphaned command), or one that started while stopping.
    await this.#killLiveShGroups();
    await this.#killLiveStageGroups();
    await this.#socket?.close();
    await this.#lock?.release();
    this.#log?.close();
  }

  // -- event application ----------------------------------------------

  /** Applies one core event: log it (design §9.2 — `events.jsonl` holds
   * transitions, intents, completions and applied commands only, never a
   * full state snapshot), reduce(), and re-drive. Throws if reduce rejects
   * it (a conductor bug, since every event this file emits should always be
   * exactly what next() asked for). */
  #applyEvent(event: Event, commandId?: string): void {
    // Once `stop()` has started tearing down (or a prior auto-stop already
    // ran), a straggling background dispatch (e.g. `#runWorkerAttempt` for
    // an agent `stop()` just terminated) must not append to an already-
    // closed log or resume driving — see round-of-review item 1.
    if (this.#closed) return;
    // design §9.3: an applied inbox command's event carries its inbox
    // command id, so recovery can tell "already applied" from the log
    // without a second record. reduce() ignores the extra field.
    const raw: Event = commandId === undefined ? event : ({ ...event, commandId } as Event);
    // Plan 01a: THIS is where agent-supplied text (a decision's choice, a
    // finding's evidence, an owner's correction, a ballot's rationale) enters
    // the conductor's in-memory state, and every prompt the conductor later
    // sends is built from that state — the turn-2 reviewer prompt, the repair
    // request, the views. Redacting only the copy written to `events.jsonl`
    // would leave the raw value in memory and put it back in front of a model,
    // and it would also make a restarted conductor's state (folded from the
    // redacted log) differ from the live one. So the event is redacted once,
    // here, before reduce(), and the log's own copy is redacted from the same
    // event (`EventLog` keeps its own pass as a backstop for its other
    // callers: intents, completions, sweeps).
    const logged = redactRecord(raw, this.#secretMaskable) as Event;
    const before = this.#state.phase.phase;
    const result = reduce(this.#state, logged);
    if (!result.ok) {
      // Only an applied event is ever written as an "event": recovery folds
      // every one of them, so a rejected one would make every restart fail
      // (runs cc1992e2 and ff398f35: a late REVIEW_SUBMITTED, logged and
      // then rejected, crashed each resume).
      this.#log.append("rejected", { event: logged, reason: result.reason });
      throw new Error(`conductor emitted an event reduce() rejected: ${result.reason}`);
    }
    this.#log.append("event", logged);
    this.#state = result.state;
    // Contract v1: write the message projections only when an event can have
    // changed them. `start()` rebuilds them from the log regardless, so a
    // conductor killed before this write loses nothing; writing on every
    // event (of which there are thousands per run) slowed long runs enough to
    // matter against their test timeouts.
    if (MESSAGE_EVENT_TYPES.has(logged.type)) {
      // Plan 05j: persist an entry for every message that has none yet, then
      // render. Doing it here (not in the renderer) keeps the ids stable and
      // makes an owner's `s`/`m`/`A`/`D` name an entry reduce() can find.
      this.#syncEntries();
      this.#writeContractProjections();
    }
    // Plan 01b: a fresh park is a new notification episode; resolving some of
    // a park's requests (which bounces through AWAITING_OWNER back to itself)
    // is not.
    if (before !== "AWAITING_OWNER" && this.#state.phase.phase === "AWAITING_OWNER") this.#awaitingEpisode += 1;
    // Plan 06f (A2): the Emacs mode line reads `live.json`; every phase change
    // rewrites this run's row (and only this run's row) there.
    if (before !== this.#state.phase.phase) this.#writeLive();
    this.#syncBudgetTimer();
    if (!this.#driveSuspended) this.drive();
    // Plan 01b: before `#maybeAutoStop` can tear the log down for BLOCKED.
    this.#checkNotifications();
    this.#maybeAutoStop();
  }

  /** Plan 04a: apply a batch of events as one visible step, then drive once. */
  #applyEvents(events: Event[]): void {
    this.#driveSuspended = true;
    try {
      for (const event of events) this.#applyEvent(event);
    } finally {
      this.#driveSuspended = false;
    }
    this.drive();
  }

  // -- inbox: owner commands (design §7.4, §9.3) --------------------------

  /** Creates `<run>/inbox/`, `applied/` and `rejected/` if missing — a run
   * created before this packet, or a run directory assembled by hand, still
   * gets them on its next start. */
  #ensureInboxDirs(): void {
    for (const dir of [this.#paths.inbox, this.#paths.inboxApplied, this.#paths.inboxRejected]) {
      fs.mkdirSync(dir, { recursive: true });
    }
  }

  /** design §9.3: read every pending `<id>.json` owner command, name-sorted
   * first (so two commands written in one poll window apply in a stable
   * order), and apply or reject each exactly once. Synchronous by design: a
   * scan never interleaves with another, and `#applyEvent`'s own `drive()`
   * runs inside the same tick. */
  #scanInbox(): void {
    if (this.#closed) return;
    let names: string[];
    try {
      names = fs.readdirSync(this.#paths.inbox);
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === "ENOENT") return;
      this.#logUnexpected("scan_inbox", err);
      return;
    }
    for (const name of names.sort()) {
      if (this.#closed) return;
      if (!name.endsWith(".json")) continue;
      try {
        this.#processInboxFile(path.join(this.#paths.inbox, name));
      } catch (err) {
        this.#logUnexpected("process_inbox", err);
      }
    }
  }

  /** Rejects one inbox file visibly: a `command_rejected` log record plus
   * the file moved to rejected/ with the reason beside it. */
  #rejectInboxFile(file: string, commandId: string, reason: string): void {
    const trimmed = reason.length > 500 ? `${reason.slice(0, 500)}…` : reason;
    this.#log.append("command_rejected", { commandId, reason: trimmed });
    try {
      // Plan 01a: a rejection reason can quote the offending field (a schema
      // error names the value it refused), so the note on disk is redacted too.
      fs.writeFileSync(path.join(this.#paths.inboxRejected, `${commandId}.reason.txt`), `${redactText(trimmed, this.#secretMaskable)}\n`);
    } catch (err) {
      this.#log.append("error", { where: "inbox_rejection_note", error: String((err as Error)?.message ?? err) });
    }
    this.#moveInboxFile(file, this.#paths.inboxRejected);
  }

  /** Plan 01g: the applied amendment an explicit `revert AM-p1-…` command
   * names, or undefined. Only the command form counts: text that merely
   * mentions the id must stay a steer/note so it still reaches an agent
   * (finding B-16), and a near-miss id is not a revert. Trailing prose after
   * the id is allowed, exactly like `withdraw OD-n`. */
  #revertAmendmentForText(text: string): { decisionId: string; amendmentId: string } | undefined {
    const match = text.trim().match(/^revert\s+(\S+)/i);
    if (!match) return undefined;
    const id = match[1];
    for (const d of this.#state.phase.decisions) {
      if (!d.amendment || d.amendment.status !== "applied") continue;
      if (d.amendment.id === id) return { decisionId: d.id, amendmentId: d.amendment.id };
    }
    return undefined;
  }

  /** Plan 01g: apply the owner's revert of one amendment — restore the
   * criterion's original wording, record the input as `reverted`, and move
   * the inbox file on. A failure is rejected visibly, never silently. */
  #applyRevertAmendment(
    file: string,
    commandId: string,
    text: string,
    decisionId: string,
    amendmentId: string,
  ): void {
    const decision = this.#state.phase.decisions.find((d) => d.id === decisionId);
    const amendment = decision?.amendment;
    if (!decision || !amendment || amendment.status !== "applied") {
      this.#rejectInboxFile(file, commandId, `amendment ${amendmentId} is not an applied amendment of this phase`);
      return;
    }
    const restored = this.#state.phase.contract.acceptance.map((a) =>
      a === amendment.proposedWording ? amendment.criterion : a,
    );
    try {
      this.#applyEvent(
        {
          type: "CRITERION_REVERTED",
          amendmentId,
          newAcceptance: restored,
          newContractVersion: amendContractVersion(this.#state.phase.contract, restored),
          // OD-2: the conductor stamps the time on the event, so reduce()
          // never reads a clock and a rebuild is byte-identical.
          at: new Date().toISOString(),
        },
        commandId,
      );
    } catch (err) {
      this.#rejectInboxFile(file, commandId, `could not revert ${amendmentId}: ${String((err as Error)?.message ?? err)}`);
      return;
    }
    this.#appliedCommandIds.add(commandId);
    this.#recordOwnerInput(commandId, "correction", text, "reverted");
    crashAt("before_inbox_move");
    this.#moveInboxFile(file, this.#paths.inboxApplied);
  }

  /** Plan 2d: the three input-box kinds (design §7.4), detected by shape in
   * either the flat `kind` form or the decision view's `type` form. */
  #ownerInputKindOf(raw: unknown): InputKind | undefined {
    if (!raw || typeof raw !== "object") return undefined;
    const r = raw as Record<string, unknown>;
    const kind = typeof r.type === "string" ? r.type : typeof r.kind === "string" ? r.kind : undefined;
    return kind === "steer" || kind === "note" || kind === "correction" ? kind : undefined;
  }

  /** Plan 2d: records one owner input's actually-observed effect. Record-
   * only; keyed by the inbox command id so a later, more definitive record
   * (e.g. a recovered steer's `delivered`) updates rather than duplicates. */
  #recordOwnerInput(
    id: string,
    kind: OwnerInputKind,
    text: string,
    state: OwnerInputState,
    attemptId?: string,
    reason?: string,
  ): void {
    // `id` is the inbox command id, so the logged event carries it: recovery
    // then knows a replayed file was already applied (design §9.3) instead of
    // processing it a second time.
    this.#applyEvent(
      {
        type: "OWNER_INPUT_RECORDED",
        input: {
          id,
          kind,
          text,
          state,
          ...(attemptId ? { attemptId } : {}),
          ...(reason ? { reason } : {}),
          at: new Date().toISOString(),
        },
      },
      id,
    );
  }

  // -- plan 01i: owner directives (01_ref_design.md D5, runtime §8) --------

  /** Plan 01i: every live agent of this run, each with the target label the
   * status shows (`worker`, or the reviewer's letter). One entry per live
   * agent, deliberately: during a re-dispatch overlap two agents can share a
   * label, and every one of them must be steered (deliveries are then
   * recorded per label, newest ack wins). */
  #liveAgents(): Array<{ target: string; agentId: string; agent: PiAgent }> {
    const out: Array<{ target: string; agentId: string; agent: PiAgent }> = [];
    for (const h of this.#agents.values()) {
      if (h.agent.exited) continue;
      const target = h.role === "worker" ? "worker" : (h.agentId.match(/^reviewer-([A-Za-z0-9_]+)-/)?.[1] ?? h.agentId);
      out.push({ target, agentId: h.agentId, agent: h.agent });
    }
    return out;
  }

  /** Plan 01i: records one directive, then steers it at once to every live
   * agent except `exemptAgentIds` (an agent whose steer is already being
   * handled — the worker in `#processSteerCommand`), recording each delivery
   * as it is acknowledged. The directive itself is a logged event, so it
   * survives a restart and every later prompt quotes it verbatim.
   *
   * Ids are namespaced so one id always names one ruling: a phase's own
   * directives are `OD-<n>`, a program-wide one arrives with the program's
   * `ODP-<n>` and keeps it verbatim (the two spaces never collide, so a node
   * can never renumber a program ruling into a number of its own). */
  #addDirective(
    text: string,
    scope: DirectiveScope,
    commandId: string,
    exemptAgentIds: string[] = [],
    preferredId?: string,
  ): OwnerDirective {
    const existing = this.#state.phase.ownerDirectives ?? [];
    // Idempotent by inbox command id: a replay (the log already holds the
    // directive but the file was never moved) must not mint a second id.
    const already = existing.find((d) => d.commandId === commandId);
    if (already) return already;
    const { id, seq } = allocateDirectiveId(existing, preferredId);
    const live = this.#liveAgents();
    const directive: OwnerDirective = {
      id,
      seq,
      text,
      scope,
      status: "in-force",
      commandId,
      at: new Date().toISOString(),
      targets: [...new Set(live.map((l) => l.target))],
      deliveries: {},
    };
    this.#applyEvent({ type: "DIRECTIVE_ADDED", directive });
    this.#steerDirectiveTo(directive, exemptAgentIds);
    return directive;
  }

  /** Plan 01i: steers one directive's text to every live agent of this run
   * (except `exemptAgentIds`), logging an intent before each send and a
   * completion on acknowledgement — the same intent/completion discipline as
   * a steer, so a crash mid-send leaves a recoverable record and never a
   * silent loss. */
  #steerDirectiveTo(directive: OwnerDirective, exemptAgentIds: string[]): void {
    for (const { target, agentId, agent } of this.#liveAgents()) {
      if (exemptAgentIds.includes(agentId)) continue;
      const actionId = `deliver-${directive.commandId}-${target}-${agentId}`;
      const message = `Owner directive ${directive.id} (binding): ${directive.text}`;
      this.#log.intent(actionId, { directiveId: directive.id, target, agentId, text: directive.text });
      void agent.steer(message).then(
        () => {
          if (this.#closed) return;
          this.#log.completion(actionId, { outcome: "directive-steer-acknowledged", target, agentId });
          this.#applyEvent({ type: "DIRECTIVE_DELIVERED", directiveId: directive.id, target, state: "delivered" });
        },
        (err) => {
          if (this.#closed) return;
          this.#log.completion(actionId, {
            outcome: "directive-steer-failed",
            target,
            agentId,
            error: String((err as Error)?.message ?? err),
          });
          this.#applyEvent({ type: "DIRECTIVE_DELIVERED", directiveId: directive.id, target, state: "delivery-uncertain" });
        },
      );
    }
  }

  /** Plan 01i: an owner directive pushed by a program (a node that was
   * already running when the director sent it, D5). It is a directive like
   * any other — steered to every live agent now, quoted in every later
   * prompt — and, when scope is `program`, forwarded to the program's own
   * inbox so every other running node gets it and every node started later
   * is started with it. */
  #processDirective(
    file: string,
    commandId: string,
    text: string,
    scope: DirectiveScope,
    forward: boolean,
    programId?: string,
  ): void {
    this.#addDirective(text, scope, commandId, [], programId);
    // Recorded `noted` (it is part of the phase and in every later prompt);
    // each steer's real outcome is on the directive itself, so this record
    // never claims a delivery that has not been acknowledged.
    this.#recordOwnerInput(commandId, "directive", text, "noted");
    // Only a directive the *owner* sent from this run's own input box with
    // `C-u` is forwarded to the program: the scheduler already pushed a
    // program-wide one here, and forwarding it back would loop.
    if (forward && scope === "program") this.#forwardProgramDirective(commandId, text);
    this.#appliedCommandIds.add(commandId);
    crashAt("before_inbox_move");
    this.#moveInboxFile(file, this.#paths.inboxApplied);
  }

  /** Plan 05j: apply one entry command from the review view. The whole
   * expansion is dry-run through reduce() first, so a partial application
   * (some OWNER_VERDICTs applied, one stale) is impossible. */
  #processEntryCommand(file: string, commandId: string, raw: Record<string, unknown>): void {
    const phase = this.#state.phase.phase;
    if (phase === "DONE" || phase === "BLOCKED") {
      this.#rejectInboxFile(file, commandId, `the phase is ${phase}; the run no longer accepts owner input`);
      return;
    }
    const expanded = expandEntryCommand(raw, this.#state.phase.entries ?? [], this.#state.phase.messages ?? []);
    if (!expanded.ok) {
      this.#rejectInboxFile(file, commandId, expanded.reason);
      return;
    }
    // A raw message cannot be settled yet; it is reported, not silently
    // skipped (record A-72 / M-63).
    if (expanded.skipped.length > 0) {
      this.#log.append("entry_verdict_skipped", { commandId, entryId: raw.entryId, skipped: expanded.skipped });
    }
    if (expanded.runId) {
      // Plan 06d (A2/C3): resolveBinding is the only place that matches a run.
      const resolved = resolveBinding(this.#runBinding(), expanded.runId);
      if (!resolved.ok) {
        this.#rejectInboxFile(file, commandId, resolved.reason);
        return;
      }
    }
    if (expanded.phaseId && expanded.phaseId !== this.#state.phase.phaseId) {
      this.#rejectInboxFile(file, commandId, `command is bound to phase ${expanded.phaseId}, but this run is on phase ${this.#state.phase.phaseId}`);
      return;
    }
    let check = this.#state;
    for (const event of expanded.events) {
      const result = reduce(check, event);
      if (!result.ok) {
        this.#rejectInboxFile(file, commandId, result.reason);
        return;
      }
      check = result.state;
    }
    this.#appliedCommandIds.add(commandId);
    for (const event of expanded.events) this.#applyEvent(event, commandId);
    this.#moveInboxFile(file, this.#paths.inboxApplied);
    this.#log.append("entries_applied", { commandId, count: expanded.events.length });
  }

  /** Plan 01i: `withdraw OD-n`. The directive no longer applies: every live
   * agent is steered that it is withdrawn and every later prompt omits it.
   * An id that is unknown (or already withdrawn) is refused with the reason,
   * never silently applied. */
  #processWithdraw(
    file: string,
    commandId: string,
    text: string,
    directiveId: string,
    opts: { forwardProgram: boolean; pushed: boolean },
  ): void {
    // `pushed` is a withdrawal the program scheduler delivered (or re-
    // delivered on its next tick): for a directive that is already gone it is
    // a no-op success, never a refusal — otherwise the same file would be
    // rejected again on every tick.
    const noop = (): void => {
      this.#appliedCommandIds.add(commandId);
      crashAt("before_inbox_move");
      this.#moveInboxFile(file, this.#paths.inboxApplied);
    };
    const directive = (this.#state.phase.ownerDirectives ?? []).find((d) => d.id === directiveId);
    if (!directive) {
      if (opts.pushed) return noop();
      const reason = `no owner directive ${directiveId} exists in phase ${this.#state.phase.phaseId}`;
      this.#recordOwnerInput(commandId, "withdraw", text, "refused", undefined, reason);
      this.#rejectInboxFile(file, commandId, reason);
      return;
    }
    if (directive.status === "withdrawn") {
      if (opts.pushed) return noop();
      const reason = `owner directive ${directiveId} is already withdrawn`;
      this.#recordOwnerInput(commandId, "withdraw", text, "refused", undefined, reason);
      this.#rejectInboxFile(file, commandId, reason);
      return;
    }
    // The command id rides the logged event, so a re-push of the same
    // withdrawal file (the program scheduler writes it on every tick) is
    // moved without being re-processed — hence without a second, spurious
    // "already withdrawn" refusal.
    // OD-2: the withdrawal time rides on the event; reduce() never reads a
    // clock.
    this.#applyEvent({ type: "DIRECTIVE_WITHDRAWN", directiveId, at: new Date().toISOString() }, commandId);
    this.#appliedCommandIds.add(commandId);
    const message = `Owner directive ${directiveId} is withdrawn; it no longer applies.`;
    for (const { target, agentId, agent } of this.#liveAgents()) {
      const actionId = `deliver-${commandId}-${target}-${agentId}`;
      this.#log.intent(actionId, { directiveId, target, agentId, withdrawn: true });
      void agent.steer(message).then(
        () => {
          if (this.#closed) return;
          this.#log.completion(actionId, { outcome: "directive-withdraw-acknowledged", target, agentId });
        },
        (err) => {
          if (this.#closed) return;
          this.#log.completion(actionId, {
            outcome: "directive-withdraw-failed",
            target,
            agentId,
            error: String((err as Error)?.message ?? err),
          });
        },
      );
    }
    if (!opts.pushed) this.#recordOwnerInput(commandId, "withdraw", text, "delivered");
    // A program-wide ruling is retracted program-wide from wherever it is
    // withdrawn: the program records it and pushes the notice to every node
    // (this one included — the pushed copy is the no-op above).
    if (opts.forwardProgram && directive.scope === "program") this.#forwardProgramWithdraw(commandId, directiveId);
    crashAt("before_inbox_move");
    this.#moveInboxFile(file, this.#paths.inboxApplied);
  }

  /** Plan 01i: this run's program directory, when the scheduler started it
   * (its `program.json` names the program). `undefined` for a hand-started
   * run, which has nothing program-wide to reach. */
  #programDir(): string | undefined {
    try {
      const info = JSON.parse(fs.readFileSync(path.join(this.#runDir, "program.json"), "utf8")) as { programId?: string };
      return info.programId ? path.join(path.dirname(this.#runDir), "programs", info.programId) : undefined;
    } catch {
      return undefined;
    }
  }

  /** Plan 01i: a `C-u` input in a run's own box, applied program-wide (D5).
   * The run does NOT record a directive of its own and does NOT steer: it
   * forwards the text to the program, which mints the single program-wide id
   * (`ODP-n`) and pushes that record to every running node — this one
   * included. That record is what steers each live agent, exactly once, with
   * the id the owner will withdraw against. So one id names one ruling
   * everywhere, and no input is ever steered twice. */
  #processProgramWideInput(file: string, commandId: string, kind: OwnerInputKind, text: string): void {
    this.#forwardProgramDirective(commandId, text);
    this.#recordOwnerInput(
      commandId,
      kind,
      text,
      "noted",
      undefined,
      "forwarded to the program as a program-wide directive; the program records it and steers every node",
    );
    crashAt("before_inbox_move");
    this.#moveInboxFile(file, this.#paths.inboxApplied);
  }

  /** Plan 01i: a run started by a program scheduler (D5) forwards a
   * program-wide directive to the program's own inbox, so the scheduler
   * records it once and pushes it to every running node (this one included)
   * and starts later nodes with it. */
  #forwardProgramDirective(commandId: string, text: string): void {
    try {
      const info = JSON.parse(fs.readFileSync(path.join(this.#runDir, "program.json"), "utf8")) as { programId?: string };
      if (!info.programId) return;
      const inbox = path.join(path.dirname(this.#runDir), "programs", info.programId, "inbox");
      fs.mkdirSync(inbox, { recursive: true });
      const file = path.join(inbox, `${commandId}.json`);
      const tmp = `${file}.tmp`;
      fs.writeFileSync(
        tmp,
        JSON.stringify({ type: "directive", text, scope: "program", origin: this.#state.phase.runId, key: commandId }),
      );
      fs.renameSync(tmp, file);
    } catch (err) {
      // Not a program node (no program.json), or its inbox is unwritable: the
      // directive still applies to this phase, never dropped from this run.
      if ((err as NodeJS.ErrnoException).code !== "ENOENT") this.#logUnexpected("forward_program_directive", err);
    }
  }

  /** Plan 01i: `withdraw ODP-n` names a program-wide ruling, so retracting it
   * here must retract it everywhere: the program records the withdrawal and
   * pushes the notice to every running node (including this one, whose own
   * record is already withdrawn — the pushed copy is a no-op) and drops it
   * from every node started later. */
  #forwardProgramWithdraw(commandId: string, directiveId: string): void {
    try {
      const info = JSON.parse(fs.readFileSync(path.join(this.#runDir, "program.json"), "utf8")) as { programId?: string };
      if (!info.programId) return;
      const inbox = path.join(path.dirname(this.#runDir), "programs", info.programId, "inbox");
      fs.mkdirSync(inbox, { recursive: true });
      const file = path.join(inbox, `${commandId}.json`);
      const tmp = `${file}.tmp`;
      fs.writeFileSync(tmp, JSON.stringify({ type: "withdraw", directiveId, key: commandId }));
      fs.renameSync(tmp, file);
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code !== "ENOENT") this.#logUnexpected("forward_program_withdraw", err);
    }
  }

  /** Plan 2d (design §7.4/§9.3): external delivery of one steer. The intent
   * is logged before the RPC `steer`, the completion after Pi acknowledges
   * it. Pi has no receiver-side dedup, so a crash in between leaves the
   * outcome unknown: on restart an intent with no completion is recorded
   * `delivery-uncertain` and never resent — steering is at most once, or
   * explicitly uncertain. */
  #processSteerCommand(file: string, commandId: string, raw: unknown, text: string, scope: DirectiveScope = "phase"): void {
    const r = raw as Record<string, unknown>;
    const binding = r.binding && typeof r.binding === "object" ? (r.binding as Record<string, unknown>) : undefined;
    const boundAttemptId =
      typeof r.boundAttemptId === "string"
        ? r.boundAttemptId
        : binding && typeof binding.attemptId === "string"
          ? binding.attemptId
          : undefined;
    const actionId = `deliver-${commandId}`;
    let records: LogRecord[];
    try {
      records = readLog(this.#paths.events).records;
    } catch (err) {
      this.#logUnexpected("read_log_steer", err);
      return;
    }
    const intent = records.find((rec) => rec.kind === "intent" && rec.actionId === actionId);
    const completion = records.find((rec) => rec.kind === "completion" && rec.actionId === actionId);
    const recorded = (this.#state.phase.ownerInputs ?? []).find((i) => i.id === commandId);

    if (intent) {
      // Plan 01i: the directive was recorded just before this intent, so a
      // restart folds it back; if the crash landed between the directive's
      // own event and the intent, add it now (idempotent by command id) so
      // the recovered input is still a directive in every later prompt.
      const directive = this.#addDirective(text, scope, commandId, []);
      const recovered: "delivered" | "delivery-uncertain" = completion ? "delivered" : "delivery-uncertain";
      if (!recorded) {
        if (completion) {
          const outcome = completion.event as { agentId?: string; attemptId?: string };
          this.#recordOwnerInput(commandId, "steer", text, "delivered", outcome.attemptId ?? boundAttemptId);
        } else {
          this.#recordOwnerInput(
            commandId,
            "steer",
            text,
            "delivery-uncertain",
            boundAttemptId,
            "the conductor restarted between sending the steer and Pi acknowledging it; it is never resent automatically",
          );
        }
        this.#applyEvent({ type: "DIRECTIVE_DELIVERED", directiveId: directive.id, target: "worker", state: recovered });
      }
      this.#appliedCommandIds.add(commandId);
      crashAt("before_inbox_move");
      this.#moveInboxFile(file, this.#paths.inboxApplied);
      return;
    }

    const worker = [...this.#agents.values()].find((h) => h.role === "worker" && !h.agent.exited);
    // Plan 06d (A1/C3): the observation may have changed since the inbox
    // block last asked acceptInput (the worker can exit in between), so ask
    // the one policy again rather than deciding here. A steer with no worker
    // is queued, never refused: owner input is never lost.
    const decision = acceptInput(this.#state, { kind: "steer", text, workerRunning: worker !== undefined });
    if (decision.kind === "refused") {
      this.#recordOwnerInput(commandId, "steer", text, "refused", boundAttemptId, decision.reason);
      this.#rejectInboxFile(file, commandId, decision.reason);
      return;
    }
    if (decision.kind === "queued" || !worker) {
      const event: Event = { type: "NOTE_ADDED", phaseId: this.#state.phase.phaseId, text };
      const result = reduce(this.#state, event);
      if (!result.ok) {
        this.#recordOwnerInput(commandId, "steer", text, "refused", boundAttemptId, result.reason);
        this.#rejectInboxFile(file, commandId, result.reason);
        return;
      }
      this.#appliedCommandIds.add(commandId);
      this.#applyEvent(event, commandId);
      this.#addDirective(text, scope, commandId);
      this.#recordOwnerInput(commandId, "steer", text, "queued");
      crashAt("before_inbox_move");
      this.#moveInboxFile(file, this.#paths.inboxApplied);
      return;
    }
    const attemptId = boundAttemptId ?? worker.agentId;
    // Plan 01i: every input is also an owner directive. It is recorded before
    // the steer is sent (so a crash between the two still leaves the ruling in
    // every later prompt), and the worker's own steer below is that
    // directive's delivery — never a second, duplicate steer. The exemption
    // is that worker's *agent id* (`worker-<n>-<actionId>`), not its label.
    const directive = this.#addDirective(text, scope, commandId, [worker.agentId]);
    // Mark in flight (NOT applied): the file must stay in the inbox until the
    // acknowledgement decides its fate, so a crash here can still recover it.
    this.#steerInFlight.add(commandId);
    this.#log.intent(actionId, { commandId, agentId: worker.agentId, attemptId, text });
    const ack = worker.agent.steer(text);
    // design §9.3: the boundary between sending the steer and its
    // acknowledgement — a crash here leaves the outcome unknown.
    crashAt("before_steer_ack");
    const finish = (state: "delivered" | "delivery-uncertain", reason?: string): void => {
      this.#steerInFlight.delete(commandId);
      this.#appliedCommandIds.add(commandId);
      this.#recordOwnerInput(commandId, "steer", text, state, attemptId, reason);
      this.#applyEvent({ type: "DIRECTIVE_DELIVERED", directiveId: directive.id, target: "worker", state });
      crashAt("before_inbox_move");
      this.#moveInboxFile(file, this.#paths.inboxApplied);
    };
    void ack.then(
      () => {
        if (this.#closed) return;
        this.#log.completion(actionId, { outcome: "steer-acknowledged", agentId: worker.agentId, attemptId });
        finish("delivered");
      },
      (err) => {
        if (this.#closed) return;
        this.#log.completion(actionId, {
          outcome: "steer-failed",
          agentId: worker.agentId,
          attemptId,
          error: String((err as Error)?.message ?? err),
        });
        finish("delivery-uncertain", "Pi could not accept the steer");
      },
    );
  }

  /** One inbox file. The command id is the filename's basename (design §9.1
   * — schemas/owner-command.schema.json's bodies carry no `id`), never part
   * of the JSON body. Ordering is deliberate: (1) an id already in the log
   * is only moved; (2) a command that fails to parse/validate, names an
   * unsupported kind, or whose binding no longer matches is rejected
   * visibly — a `command_rejected` log record plus the file moved to
   * rejected/ with the reason beside it; (3) otherwise the event is appended
   * (the effect), the id is remembered, and only then is the file moved. */
  /** Plan 06b: record one `evidence` item the owner recorded with
   * `tt evidence`. When it was the last pending evidence item and the phase
   * parked on the owner for it, the fallback request is resolved so the phase
   * resumes to RESOLVING and acceptance. */
  #processEvidenceCommand(file: string, commandId: string, raw: { item?: unknown; text?: unknown }): void {
    const name = this.#state.phase.phase;
    if (name === "DONE" || name === "BLOCKED") {
      this.#rejectInboxFile(file, commandId, `the phase is ${name}; the run no longer accepts owner input`);
      return;
    }
    // OD-2 (disc-M-91): an old-format phase has no structured items and never
    // parks for evidence, so a recording there has no effect; refuse it
    // rather than accept a no-op.
    if (!this.#structured()) {
      this.#rejectInboxFile(file, commandId, "this phase has no structured items, so it has no evidence items to record");
      return;
    }
    const itemId = typeof raw.item === "string" ? raw.item.trim() : "";
    const text = typeof raw.text === "string" ? raw.text.trim() : "";
    const evidenceItems = flatItems(this.#planItems()).filter((i) => itemNeedsEvidence(i));
    const item = evidenceItems.find((i) => i.id === itemId);
    if (!item) {
      this.#rejectInboxFile(file, commandId, `${itemId || "(no item)"} is not an evidence item of this phase`);
      return;
    }
    if (text.length === 0) {
      this.#rejectInboxFile(file, commandId, "evidence needs non-empty text or an existing file");
      return;
    }
    const existing = this.#state.phase.itemEvidence ?? [];
    this.#applyEvent({
      type: "ITEM_STATE_UPDATED",
      itemEvidence: [...existing.filter((e) => e.id !== itemId), { id: itemId, text, at: new Date().toISOString(), commandId }],
    });
    this.#log.append("item_evidence_recorded", { commandId, item: itemId });
    // Resume once nothing else stands in the way: the evidence the phase
    // parked for is recorded, so the phase returns to RESOLVING (where
    // accept() now holds) and the parking request closes.
    if (this.#state.phase.phase === "AWAITING_OWNER" && pendingEvidenceItems(this.#state.phase).length === 0) {
      this.#applyEvent({ type: "EVIDENCE_RECORDED", itemId });
    }
    this.#moveInboxFile(file, this.#paths.inboxApplied);
  }

  #processInboxFile(file: string): void {
    if (this.#closed) return;
    const name = path.basename(file);
    const commandId = name.endsWith(".json") ? name.slice(0, -".json".length) : name;

    if (this.#appliedCommandIds.has(commandId)) {
      this.#moveInboxFile(file, this.#paths.inboxApplied);
      return;
    }
    if (this.#steerInFlight.has(commandId)) return;

    let text: string;
    try {
      text = fs.readFileSync(file, "utf8");
    } catch (err) {
      this.#rejectInboxFile(file, commandId, `could not read owner command file: ${String((err as Error)?.message ?? err)}`);
      return;
    }
    // A writer may still be mid-write (an empty or whitespace-only document
    // is indistinguishable from the truncate window of an in-place write).
    // That is transient, not malformed: leave the file for the next poll
    // rather than rejecting it permanently. A genuinely malformed
    // (non-empty) document is still rejected below.
    if (text.trim().length === 0) return;
    let raw: unknown;
    try {
      raw = JSON.parse(text);
    } catch (err) {
      this.#rejectInboxFile(file, commandId, `could not parse owner command JSON: ${String((err as Error)?.message ?? err)}`);
      return;
    }

    // Plan 01i (D5): a program-wide directive the scheduler pushed into this
    // node's inbox — `{type: "directive", text, scope}` — or a program-level
    // withdraw of one. Neither is an owner-input kind, so both are handled
    // before the input-kind detection below.
    const programDirective = directiveCommandOf(raw);
    if (programDirective) {
      if (this.#state.phase.phase === "DONE" || this.#state.phase.phase === "BLOCKED") {
        const reason = `the phase is ${this.#state.phase.phase}; the run no longer accepts owner input`;
        this.#recordOwnerInput(commandId, "directive", programDirective.text, "refused", undefined, reason);
        this.#rejectInboxFile(file, commandId, reason);
        return;
      }
      this.#processDirective(
        file,
        commandId,
        programDirective.text,
        programDirective.scope,
        programDirective.forward,
        programDirective.programId,
      );
      return;
    }
    const programWithdraw = withdrawCommandOf(raw);
    if (programWithdraw) {
      if (this.#state.phase.phase === "DONE" || this.#state.phase.phase === "BLOCKED") {
        const reason = `the phase is ${this.#state.phase.phase}; the run no longer accepts owner input`;
        this.#recordOwnerInput(commandId, "withdraw", programWithdraw.text, "refused", undefined, reason);
        this.#rejectInboxFile(file, commandId, reason);
        return;
      }
      this.#processWithdraw(file, commandId, programWithdraw.text, programWithdraw.directiveId, {
        forwardProgram: false,
        pushed: true,
      });
      return;
    }

    // Plan 06b: `tt evidence <run> <item> <file-or-text>` — the owner's
    // recording of an `evidence` item. Handled before the input-kind map.
    if (raw !== null && typeof raw === "object" && (raw as { type?: unknown }).type === "evidence") {
      this.#processEvidenceCommand(file, commandId, raw as { item?: unknown; text?: unknown });
      return;
    }

    // Plan 05j: the review view's entry commands. An entry-verdict expands to
    // one OWNER_VERDICT per linked message; split/merge/retitle are one
    // ENTRY_* event each. Handled before the conductor-state mapping.
    if (raw !== null && typeof raw === "object" && typeof (raw as { type?: unknown }).type === "string" && (raw as { type: string }).type.startsWith("entry-")) {
      this.#processEntryCommand(file, commandId, raw as Record<string, unknown>);
      return;
    }

    // Plan 2d: the input box's kinds are handled before the conductor-state
    // mapping. A steer is external delivery, not a core event. A note or
    // correction is a core event but also carries the owner-input record
    // the status view shows. Input after the phase is terminal is refused
    // with the reason — never silently dropped. Plan 01i: every one of them
    // is also an owner directive, phase-scoped unless the sender asked for
    // `scope: "program"` (`C-u` in a run's input box, D5).
    const inputKind = this.#ownerInputKindOf(raw);
    if (inputKind) {
      const inputText = (raw as { text?: unknown }).text;
      if (typeof inputText !== "string" || inputText.trim().length === 0) {
        this.#rejectInboxFile(file, commandId, "an owner input must carry non-empty text");
        return;
      }
      // Plan 06d (A2/C3): a binding's run id may be the internal runId, the
      // run directory id or the readable id; resolveBinding is the only
      // matcher. An unknown id is refused with all three it could have used,
      // so a mis-addressed input is never silently applied to this run.
      const rawBinding = (raw as { binding?: unknown }).binding;
      const bindingRunId = rawBinding && typeof rawBinding === "object" ? (rawBinding as { runId?: unknown }).runId : undefined;
      if (typeof bindingRunId === "string" && bindingRunId.length > 0) {
        const resolved = resolveBinding(this.#runBinding(), bindingRunId);
        if (!resolved.ok) {
          this.#recordOwnerInput(commandId, inputKind, inputText, "refused", undefined, resolved.reason);
          this.#rejectInboxFile(file, commandId, resolved.reason);
          return;
        }
      }
      const scope: DirectiveScope = (raw as { scope?: unknown }).scope === "program" ? "program" : "phase";
      // Plan 01i: `withdraw OD-n` (or `withdraw ODP-n`) in the input box
      // withdraws one directive — whatever kind the phase would otherwise
      // have made of the text. A withdrawal that names no valid id is
      // refused visibly: it must never be recorded as a *new* binding ruling,
      // which would leave the intended one in force. A withdrawal is not one
      // of the three input outcomes `acceptInput` decides, so it keeps its own
      // terminal check (the same refusal text).
      const parsedWithdraw = parseWithdrawInput(inputText);
      if (parsedWithdraw?.kind === "malformed") {
        this.#recordOwnerInput(commandId, "withdraw", inputText, "refused", undefined, parsedWithdraw.reason);
        this.#rejectInboxFile(file, commandId, parsedWithdraw.reason);
        return;
      }
      if (parsedWithdraw?.kind === "withdraw") {
        if (this.#state.phase.phase === "DONE" || this.#state.phase.phase === "BLOCKED") {
          const reason = `the phase is ${this.#state.phase.phase}; the run no longer accepts owner input`;
          this.#recordOwnerInput(commandId, "withdraw", inputText, "refused", undefined, reason);
          this.#rejectInboxFile(file, commandId, reason);
          return;
        }
        this.#processWithdraw(file, commandId, inputText, parsedWithdraw.id, { forwardProgram: true, pushed: false });
        return;
      }
      // A program-wide input (D5) in a run the scheduler started: the
      // program mints the one `ODP-n` record, so this run only forwards the
      // text — it must not mint a local id that could not match the
      // program's. A run that is NOT part of a program has nothing
      // program-wide to reach, so the input stays this phase's own directive
      // (never a `program`-scoped record in a run that has no program).
      const programWide = scope === "program" && this.#programDir() !== undefined;
      const effectiveScope: DirectiveScope = programWide ? "program" : "phase";
      // Plan 01g: an explicit CORRECTION command `revert AM-p1-…` restores an
      // applied amendment's original wording. It is deliberately not any text
      // that happens to mention the id (a steer or note quoting it must reach
      // its agents, findings B-16/A-2/B-4/M-8), and it never applies to a
      // program-wide input, which is forwarded as D5 requires (finding A-15).
      if (inputKind === "correction" && !programWide) {
        const revert = this.#revertAmendmentForText(inputText);
        if (revert) {
          this.#applyRevertAmendment(file, commandId, inputText, revert.decisionId, revert.amendmentId);
          return;
        }
      }
      // Plan 06d (A1/C3): acceptInput is the only place that decides an
      // input's outcome. The conductor observes whether a worker is live and
      // then only executes that decision — never decides it here.
      const workerRunning = this.#liveAgents().some((l) => l.target === "worker");
      const outcome = acceptInput(this.#state, { kind: inputKind, text: inputText, workerRunning });
      if (outcome.kind === "refused") {
        this.#recordOwnerInput(commandId, inputKind, inputText, "refused", undefined, outcome.reason);
        this.#rejectInboxFile(file, commandId, outcome.reason);
        return;
      }
      if (inputKind === "steer") {
        // A steer always goes through #processSteerCommand: it first recovers
        // a recorded intent (a crash between the RPC send and its ack), then
        // steers the live worker or queues for the next attempt — the same
        // at-most-once delivery discipline as before.
        if (programWide) {
          this.#processProgramWideInput(file, commandId, "steer", inputText);
          return;
        }
        this.#processSteerCommand(file, commandId, raw, inputText, effectiveScope);
        return;
      }
      // A queued note or correction is carried as a note so it reaches the
      // next worker attempt's prompt verbatim; an applied correction at
      // AWAITING_OWNER resolves the open requests and starts a repair.
      const queueAsNote = outcome.kind === "queued";
      const event: Event = queueAsNote
        ? { type: "NOTE_ADDED", phaseId: this.#state.phase.phaseId, text: inputText }
        : { type: "OWNER_CORRECTION", correctionId: `C-${commandId}`, text: inputText };
      const result = reduce(this.#state, event);
      if (!result.ok) {
        this.#recordOwnerInput(commandId, inputKind, inputText, "refused", undefined, result.reason);
        this.#rejectInboxFile(file, commandId, result.reason);
        return;
      }
      this.#appliedCommandIds.add(commandId);
      this.#applyEvent(event, commandId);
      // Plan 01i: every input is also an owner directive — recorded here, on
      // top of the event above, so it is steered to every live agent now and
      // quoted in every later prompt.
      if (programWide) this.#forwardProgramDirective(commandId, inputText);
      else this.#addDirective(inputText, effectiveScope, commandId);
      // The recorded effect the status view reads, never an inference: a
      // queued correction or steer is `queued`; a note is `noted`; a
      // correction applied at AWAITING_OWNER started a repair.
      if (queueAsNote && inputKind === "note") {
        this.#recordOwnerInput(commandId, "note", inputText, "noted");
      } else if (queueAsNote) {
        this.#recordOwnerInput(commandId, inputKind, inputText, "queued");
      } else {
        this.#recordOwnerInput(commandId, "correction", inputText, "correction-started");
      }
      crashAt("before_inbox_move");
      this.#moveInboxFile(file, this.#paths.inboxApplied);
      return;
    }

    // Two encodings reach the inbox: schemas/owner-command.schema.json's flat
    // `kind` form (a `tt cmd`/test front end), and the decision view's
    // `type` + `binding` form (design §10.4 — what Emacs actually writes).
    // Either reduces to the same core event.
    // Detect the encoding by shape, not by schema validity: the schema now
    // accepts BOTH forms, so validity alone cannot say which mapping to use.
    let event: Event | undefined;
    let boundRunId: string | undefined;
    let boundPhaseId: string | undefined;
    const looksFlat = raw !== null && typeof raw === "object" && typeof (raw as { kind?: unknown }).kind === "string";
    if (looksFlat) {
      const flat = validate(OWNER_COMMAND_SCHEMA, raw);
      if (!flat.valid) {
        this.#rejectInboxFile(file, commandId, `owner command fails schemas/owner-command.schema.json: ${flat.errors.join("; ")}`);
        return;
      }
      const command = raw as OwnerCommand;
      event = ownerCommandToEvent(command, commandId);
      if (!event) {
        this.#rejectInboxFile(file, commandId, `owner command kind '${command.kind}' is not a conductor-state command this phase applies`);
        return;
      }
    } else {
      const normalized = normalizeDecisionViewCommand(raw, commandId);
      if (!normalized.ok) {
        this.#rejectInboxFile(file, commandId, normalized.reason);
        return;
      }
      event = normalized.event;
      boundRunId = normalized.runId;
      boundPhaseId = normalized.phaseId;
    }
    // The decision view's binding carries run/phase ids too (design §7.1); a
    // command for another run or phase is stale by the same rule. Plan 06d
    // (A2/C3): resolveBinding is the only place that matches a run, and it
    // accepts the internal id, the run directory id and the readable id.
    if (boundRunId) {
      const resolved = resolveBinding(this.#runBinding(), boundRunId);
      if (!resolved.ok) {
        this.#rejectInboxFile(file, commandId, resolved.reason);
        return;
      }
    }
    if (boundPhaseId && boundPhaseId !== this.#state.phase.phaseId) {
      this.#rejectInboxFile(file, commandId, `command is bound to phase ${boundPhaseId}, but this run is on phase ${this.#state.phase.phaseId}`);
      return;
    }
    // Plan 06i (A4/R7): a deferral's guard must be substantive, so a
    // hand-written inbox file cannot bypass the CLI's own check.
    if (event.type === "DEFERRAL_RECORDED") {
      const d = event.deferral;
      if (d.test && !this.#deferralTestExists(d.test)) {
        this.#rejectInboxFile(file, commandId, `no resolved or existing test named "${d.test}" shows the item's current cost`);
        return;
      }
      if (d.ownerRuling && !this.#recordedOwnerRuling(d.ownerRuling)) {
        this.#rejectInboxFile(file, commandId, `${d.ownerRuling} is not a recorded owner input, directive or request`);
        return;
      }
    }
    const result = reduce(this.#state, event);
    if (!result.ok) {
      this.#rejectInboxFile(file, commandId, result.reason);
      return;
    }
    this.#appliedCommandIds.add(commandId);
    this.#applyEvent(event, commandId);
    crashAt("before_inbox_move");
    this.#moveInboxFile(file, this.#paths.inboxApplied);
  }

  /** Plan 06i (A4/R7): a deferral's `--test` guard is substantive only when
   * the item resolver resolved that test against this phase's check run, or
   * the repo's own test files contain it. */
  #deferralTestExists(test: string): boolean {
    // The item resolver must have RESOLVED the test against this phase's check
    // run; a mere mention in the repo is not a test that shows the cost.
    return (this.#state.phase.checkResolution ?? []).some((r) => r.name === test && r.outcome === "passed");
  }

  /** Plan 06i (A4/R7): an `--owner-ruling` guard is substantive only when it
   * names a RECORDED OWNER INPUT (an inbox command id). */
  #recordedOwnerRuling(id: string): boolean {
    return (this.#state.phase.ownerInputs ?? []).some((i) => i.id === id);
  }

  /** Moves an inbox file into `destDir`. A missing source is a no-op (it was
   * already moved); any other error is logged, never thrown, so one bad file
   * cannot stop the poll. */
  #moveInboxFile(file: string, destDir: string): void {
    const dest = path.join(destDir, path.basename(file));
    try {
      fs.renameSync(file, dest);
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === "ENOENT") return;
      this.#logUnexpected("inbox_move", err);
    }
  }

  /** design §8.1/§8.2: "the execution budget counts only time spent
   * executing ... a phase parked in AWAITING_OWNER consumes nothing." Called
   * after every state change (and once from `start()`): pauses the wall-
   * clock budget timer (preserving however much is left) whenever the phase
   * is AWAITING_OWNER or the run itself is already RUN_PAUSED_BUDGET, and
   * (re)starts it, for whatever remains, otherwise. A no-op when no
   * `runBudgetMs` was configured. */
  #syncBudgetTimer(): void {
    if (this.#deadlines.runBudgetMs === undefined || this.#closed) return;
    const paused = this.#state.phase.phase === "AWAITING_OWNER" || this.#state.run === "RUN_PAUSED_BUDGET";
    if (this.#budgetTimer) {
      clearTimeout(this.#budgetTimer);
      this.#budgetTimer = undefined;
      if (this.#budgetTimerStartedAt !== undefined && this.#budgetRemainingMs !== undefined) {
        this.#budgetRemainingMs = Math.max(0, this.#budgetRemainingMs - (Date.now() - this.#budgetTimerStartedAt));
      }
      this.#budgetTimerStartedAt = undefined;
    }
    if (paused) return;
    if (this.#budgetRemainingMs === undefined || this.#budgetRemainingMs <= 0) return;
    this.#budgetTimerStartedAt = Date.now();
    this.#budgetTimer = setTimeout(() => this.#onRunBudgetExceeded(), this.#budgetRemainingMs);
  }

  /** Plan 01b: announce (or remind about) an owner wait. Called after every
   * state change and from the inbox poll, so a run parked in AWAITING_OWNER
   * stays covered even though `next()` dispatches nothing then. `notify`
   * itself owns the once-per-wait dedup and the 30-minute reminder, so this
   * is safe to call on every tick. The key names the PARK EPISODE
   * (`#awaitingEpisode`), so an owner resolving one of several open requests
   * — which routes AWAITING_OWNER back to itself — keeps the same key and is
   * not announced again, while a later, genuinely new park is. A no-op unless
   * the phase is AWAITING_OWNER or BLOCKED; the reminder is deliberately not
   * sent for BLOCKED, which the conductor stops at once and no owner action
   * can revive. */
  #checkNotifications(): void {
    if (this.#closed) return;
    const phase = this.#state.phase;
    if (phase.phase !== "AWAITING_OWNER" && phase.phase !== "BLOCKED") return;
    const wait =
      phase.phase === "BLOCKED"
        ? `blocked:${oneLine(phase.blockedReason ?? "")}`
        : `awaiting:${phase.phaseId}:${this.#awaitingEpisode}`;
    const node = this.#programNode();
    // The user-visible run id is the directory's name (`tt list`), not the
    // conductor's own `phase.runId` (a separate uuid in the init record).
    const runId = path.basename(this.#runDir);
    notify(
      {
        id: runId,
        kind: "run",
        title: redactText(this.#plan.title, this.#secretMaskable),
        ...(node ? { node } : {}),
        reason: redactText(waitReason(phase), this.#secretMaskable),
        waitKey: `${runId}:${wait}`,
      },
      {
        root: path.dirname(this.#runDir),
        now: this.#now,
        reminderMs: this.#deadlines.notifyReminderMs,
        onError: (message) => this.#logUnexpected("notify", new Error(message)),
      },
    );
  }

  /** The program node this run is, if the scheduler started it (`program.json`
   * is written by `schedulerTick` before the run is launched). */
  #programNode(): string | undefined {
    try {
      const raw = JSON.parse(fs.readFileSync(path.join(this.#runDir, "program.json"), "utf8")) as { node?: unknown };
      return typeof raw.node === "string" ? raw.node : undefined;
    } catch {
      return undefined;
    }
  }

  /** Plan 06d (A2): the readable id (`<program>-NN`) the scheduler recorded in
   * this run's `program.json`, or undefined for a hand-started run. */
  #readableId(): string | undefined {
    try {
      const raw = JSON.parse(fs.readFileSync(path.join(this.#runDir, "program.json"), "utf8")) as { readableId?: unknown };
      return typeof raw.readableId === "string" && raw.readableId.length > 0 ? raw.readableId : undefined;
    } catch {
      return undefined;
    }
  }

  /** Plan 06d (A2): the three ids a binding may use to name this run, resolved
   * by the one `resolveBinding`. The conductor never compares ids itself. */
  #runBinding(): { runId: string; dirId: string; readableId?: string } {
    return {
      runId: this.#state.phase.runId,
      dirId: path.basename(this.#runDir),
      ...(this.#readableId() ? { readableId: this.#readableId()! } : {}),
    };
  }

  /** design §8.1's "run execution budget ... tokens" and "per-attempt token
   * cap": called from every agent's `onEvent` with each raw RPC event.
   * `message_update`'s `usage` payload's shape is Pi-version-dependent and
   * undocumented here, so `extractTokenTotal` reads whichever of the common
   * field names is present rather than assuming one; it is treated as
   * *cumulative* for that one agent's own session (the common shape), so
   * this simply records the latest reading per agent id rather than
   * summing deltas — a later, smaller reading would otherwise undercount.
   * Once the run-wide total (the sum over every agent) exceeds
   * `runBudgetTokens`, this triggers the same RUN_BUDGET_EXCEEDED path the
   * wall-clock budget uses. */
  /** Plan 3b: per worker, the worktree's `git diff --numstat` totals after
   * its last tool call, and the queue that keeps the snapshots in order. */
  #fileSnapshots = new Map<string, { totals: Map<string, [number, number]>; queue: Promise<void> }>();

  /** Plan 06b: the files each reviewer itself read in this review, from its
   * recorded `read` tool calls. A `met`/`fits` verdict must cite at least one
   * of them, never only the worker's own anchors. */
  #reviewerReads = new Map<Reviewer, Set<string>>();

  /** Plan 06b: the shell commands each reviewer itself ran in this review, so
   * a verdict that cites a command must cite one it actually ran. */
  #reviewerCommands = new Map<Reviewer, Set<string>>();

  /** Plan 3b: after each worker tool call, append one `tt_file_changes`
   * record to its stream naming the files that call changed ("path +a −r",
   * from `git diff --numstat` before and after — so edits made through sh
   * heredocs and redirects show too). Display only; never run state. */
  #trackFileChanges(agentId: string, streamFile: string, event: { type: string; toolCallId?: unknown }): void {
    if (event.type !== "tool_execution_end" && event.type !== "agent_start") return;
    let entry = this.#fileSnapshots.get(agentId);
    if (!entry) {
      entry = { totals: new Map(), queue: Promise.resolve() };
      this.#fileSnapshots.set(agentId, entry);
    }
    const snap = entry;
    const toolCallId = typeof event.toolCallId === "string" ? event.toolCallId : undefined;
    snap.queue = snap.queue.then(async () => {
      if (this.#closed) return;
      const totals = await worktreeNumstat(this.#paths.worktree);
      if (!totals) return;
      const files: Array<{ path: string; added: number; removed: number }> = [];
      for (const [file, [a, r]] of totals) {
        const [pa, pr] = snap.totals.get(file) ?? [0, 0];
        if (a !== pa || r !== pr) files.push({ path: file, added: a - pa, removed: r - pr });
      }
      for (const [file, [pa, pr]] of snap.totals) {
        if (!totals.has(file) && (pa !== 0 || pr !== 0)) files.push({ path: file, added: -pa, removed: -pr });
      }
      snap.totals = totals;
      if (event.type === "agent_start" || files.length === 0) return;
      const record = { agentId, ts: new Date().toISOString(), event: { type: "tt_file_changes", toolCallId, files } };
      try {
        // Plan 01a: a file path could hold a secret value; this writes to the
        // same stream file `pi-rpc.ts` redacts when it appends, with the same
        // line-safe rule (keys included, numbers never touched).
        fs.appendFileSync(streamFile, `${JSON.stringify(redactRecord(record, this.#secretMaskable))}\n`);
      } catch {
        // display only
      }
    });
  }

  /** Plan 3b stall watchdog state, per agent. `busy`: prompted and not yet
   * settled — the only time silence means anything. */
  #activity = new Map<string, { lastAt: number; busy: boolean }>();

  #noteActivity(agentId: string, event: { type: string }): void {
    const a = this.#activity.get(agentId) ?? { lastAt: Date.now(), busy: true };
    a.lastAt = Date.now();
    if (event.type === "agent_start" || event.type === "turn_start") a.busy = true;
    if (event.type === "agent_end" || event.type === "agent_settled") a.busy = false;
    this.#activity.set(agentId, a);
  }

  /** Races `deadline` against the stall watchdog: resolves "timeout" early
   * when the agent stalls twice (see `Deadlines.stallMs`). Watching starts
   * now, just after the agent was prompted. */
  #withStallWatch(
    agentId: string,
    agent: PiAgent,
    deadline: { promise: Promise<"timeout">; cancel: () => void },
    nudge: string,
  ): { promise: Promise<"timeout">; cancel: () => void } {
    const stallMs = this.#deadlines.stallMs;
    const a = this.#activity.get(agentId) ?? { lastAt: Date.now(), busy: true };
    a.lastAt = Date.now();
    a.busy = true;
    this.#activity.set(agentId, a);
    let nudged = false;
    let timer: NodeJS.Timeout | undefined;
    const stalled = new Promise<"timeout">((resolve) => {
      timer = setInterval(
        () => {
          const now = Date.now();
          const act = this.#activity.get(agentId);
          if (!act || !act.busy || this.#closed) return;
          const handle = this.#agents.get(agentId);
          // Its own command is running: the per-command sh limit owns that.
          if (handle && [...handle.shGroups].some((g) => this.#liveShGroups.has(g))) {
            act.lastAt = now;
            return;
          }
          if (now - act.lastAt < stallMs) return;
          if (!nudged) {
            nudged = true;
            act.lastAt = now;
            this.#log.append("stall_nudge", { agentId, quietMs: stallMs });
            agent.steer(nudge).catch(() => undefined);
            return;
          }
          this.#log.append("stalled", { agentId, quietMs: 2 * stallMs });
          resolve("timeout");
        },
        Math.max(50, Math.min(15_000, Math.floor(stallMs / 4))),
      );
      timer.unref();
    });
    return {
      promise: Promise.race([deadline.promise, stalled]),
      cancel: () => {
        deadline.cancel();
        if (timer) clearInterval(timer);
      },
    };
  }

  #trackRunTokens(agentId: string, event: { type: string; usage?: unknown }): void {
    if (event.type !== "message_update") return;
    const total = extractTokenTotal(event.usage);
    if (total === undefined) return;
    this.#agentTokenTotals.set(agentId, total);
    if (this.#deadlines.runBudgetTokens === undefined) return;
    let sum = 0;
    for (const v of this.#agentTokenTotals.values()) sum += v;
    if (sum >= this.#deadlines.runBudgetTokens) this.#onRunBudgetExceeded();
  }

  /** design §8/§9 + round-of-review item 1: a phase that reaches DONE or
   * BLOCKED is finished — there is nothing further for this packet's
   * single-phase conductor to do, so it releases every handle (timers,
   * socket, lock, agents) itself, which is what lets a `tt start` daemon
   * process exit on its own once the run is over. AWAITING_OWNER and
   * RUN_PAUSED_BUDGET deliberately do NOT auto-stop — phase 2 needs the
   * conductor listening for owner commands while parked there — so tests
   * that end in one of those states must call `stop()` themselves. */
  #maybeAutoStop(): void {
    // Plan 05i: the environment gate applies its events before `start()` has
    // finished its own setup (and its inbox scan); the blocked case is stopped
    // by `start()` explicitly after the gate returns false.
    if (this.#envGateActive) return;
    // Plan 05i: an environment block freezes the run until `tt resume`, so
    // the conductor stops itself exactly like a terminal phase — the node's
    // run is then observed as env-blocked, not left as a live no-op.
    if (
      this.#state.phase.phase === "DONE" ||
      this.#state.phase.phase === "BLOCKED" ||
      this.#state.run === "ENV_BLOCKED"
    ) {
      void this.stop();
    }
  }

  /** Computes next(state) and dispatches every outstanding action that is
   * not already being handled. Re-entrant-safe: while a drive is in
   * progress, a nested call (from a synchronous #applyEvent inside a
   * dispatch) is coalesced into one more pass after the current one. */
  drive(): void {
    if (this.#closed) return;
    if (this.#driving) {
      this.#redriveRequested = true;
      return;
    }
    this.#driving = true;
    try {
      do {
        this.#redriveRequested = false;
        // Decision briefs: once the phase is parked on the owner (after
        // evaluation), make sure every open owner item has a brief. The
        // evaluator's own model may have supplied one through submit_brief;
        // this is the deterministic backstop so the owner never faces the
        // raw finding without one. Idempotent, and a no-op while nothing is
        // open.
        this.#refreshBriefs();
        const actions = next(this.#state);
        // Plan 05j: the curator pass starts once per round, after the reviews
        // and before the evaluators. It is launched first, but it never BLOCKS
        // the evaluators: OD-1 requires the evaluator and panel to keep
        // launching with their models even when the curator agent cannot run.
        const p = this.#state.phase;
        if (p.phase === "EVALUATING" && p.candidate && p.curatedFor !== p.candidate.sha) this.#curateRound(p.candidate!.sha);
        for (const action of actions) this.#dispatch(action);
      } while (this.#redriveRequested);
    } finally {
      this.#driving = false;
    }
  }

  #dispatch(action: Action): void {
    const kind =
      action.type === "dispatch_review"
        ? `dispatch_review_${action.reviewer}`
        : action.type === "dispatch_evaluation"
          ? `dispatch_evaluation_${action.messageType}`
          : action.type;
    switch (action.type) {
      case "start_attempt":
        // Plan 04a: the base baseline is its own state. It is needed exactly
        // when the phase has effective checks and no baseline on disk already
        // covers this base tree and check list (plan 01e's reuse rule,
        // including a sibling program node's shared record).
        this.#applyEvent({ type: "ATTEMPT_STARTED", baselineNeeded: this.#baselineNeeded() });
        return;
      case "run_baseline": {
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "run_baseline", actionId });
        void this.#runBaselineStage(actionId).catch((err) => this.#logUnexpected("run_baseline", err));
        return;
      }
      case "dispatch_evaluation": {
        const actionId = this.#log.actionId(kind);
        const messageType = action.messageType as MessageType;
        this.#applyEvent({ type: "ACTION_STARTED", action: "dispatch_evaluation", actionId, messageType });
        void this.#runEvaluation(actionId, messageType).catch((err) => this.#logUnexpected("dispatch_evaluation", err));
        return;
      }
      case "evaluation_complete":
        // Plan 05e (finding #34): the round's evaluators and panels have
        // settled, so the candidate's final approval state is known.
        try {
          // Plan 06b: the item loop settles FIRST — the per-item majority, the
          // evaluator's overturns, and a blocking finding for every item a
          // majority did not meet (or fit). Its conductor-raised findings must
          // exist before triage so every one of them gets a disposition.
          this.#applyItemOutcomes();
        } catch (err) {
          this.#log.append("error", { where: "item_outcomes", error: String((err as Error)?.message ?? err) });
        }
        try {
          // Plan 06i: triage every finding and discovered decision — including
          // the item findings just raised — so nothing disappears and
          // acceptance waits on a fix or an owner request.
          this.#applyTriage();
        } catch (err) {
          // A1: a record without a disposition is a defect of the loop, never
          // a pass. The failure is recorded and the phase parks on the owner;
          // it never continues to acceptance as if triage had passed.
          const reason = String((err as Error)?.message ?? err);
          this.#log.append("triage_failed", { error: reason });
          this.#applyEvent({ type: "TRIAGE_FAILED", reason });
          return;
        }
        try {
          this.#recordCandidateApproval();
        } catch (err) {
          this.#log.append("error", { where: "candidate_approval", error: String((err as Error)?.message ?? err) });
        }
        this.#applyEvent({ type: "EVALUATION_COMPLETED" });
        return;
      case "dispatch_panel": {
        const blockerId = action.blockerId as string;
        const seat = action.seat as number;
        const actionId = this.#log.actionId(`${kind}_${blockerId}_${seat}`);
        this.#applyEvent({ type: "ACTION_STARTED", action: "dispatch_panel", actionId, blockerId, seat });
        void this.#runPanelSeat(actionId, blockerId, seat).catch((err) => this.#logUnexpected("dispatch_panel", err));
        return;
      }
      case "panel_decide": {
        // Plan 04b: every seat has settled. The OUTCOME is computed here
        // from the recorded votes by the pure core rule; the conductor may
        // not invent one.
        const blockerId = action.blockerId as string;
        const panel = this.#state.phase.panel?.blockers?.[blockerId];
        const panelCount = this.#laneSeats().length;
        if (!panel || panel.decided || !panelSeatsSettled(panel, panelCount)) return;
        const outcome = panelOutcome(panel, panelCount);
        const blockReasons = Object.values(panel.seats ?? {})
          .filter((s) => s.vote === "block" && s.reason)
          .map((s) => s.reason as string);
        this.#applyEvent({
          type: "PANEL_DECIDED",
          blockerId,
          outcome,
          ...(blockReasons.length > 0 ? { reason: blockReasons.join("; ") } : {}),
          ...(outcome === "escalate" ? { options: panelOptionsFor(panel, panelCount) } : {}),
        });
        return;
      }
      case "dispatch_round_panel": {
        const seat = action.seat as number;
        const actionId = this.#log.actionId(`${kind}_${seat}`);
        this.#applyEvent({ type: "ACTION_STARTED", action: "dispatch_round_panel", actionId, seat });
        void this.#runRoundPanelSeat(actionId, seat).catch((err) => this.#logUnexpected("dispatch_round_panel", err));
        return;
      }
      case "round_panel_decide": {
        this.#decideRoundPanel();
        return;
      }
      case "dispatch_worker": {
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "dispatch_worker", actionId });
        // Plan 06g2 (A1): with `#+TT_WORKERS: 2` one dispatch is one ROUND,
        // and `core/lanes.ts`'s `runRound` orchestrates it. The conductor
        // decides nothing about lanes beyond calling it.
        if (this.#laneRoundEnabled()) {
          void this.#runLaneRound(actionId).catch((err) => this.#logUnexpected("run_round", err));
          return;
        }
        void this.#runWorkerAttempt(actionId).catch((err) => this.#logUnexpected("dispatch_worker", err));
        return;
      }
      case "freeze": {
        // design §6.2/§9.3: SUBMIT_PHASE already fired (from #onSubmit,
        // synchronously, before the tool result was even returned); this is
        // the freeze's own intent/completion pair, logged like every other
        // dispatch. #runFreeze runs the actual §6.2 sequence.
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "freeze", actionId });
        void this.#runFreeze(actionId).catch((err) => this.#logUnexpected("freeze", err));
        return;
      }
      case "run_checks": {
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "run_checks", actionId });
        void this.#runChecks(actionId, action.candidateSha as string).catch((err) =>
          this.#logUnexpected("run_checks", err),
        );
        return;
      }
      case "dispatch_probe": {
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "dispatch_probe", actionId });
        void this.#runProbe(actionId, action.candidateSha as string, action.head as string).catch((err) =>
          this.#logUnexpected("dispatch_probe", err),
        );
        return;
      }
      case "dispatch_review": {
        const reviewer = action.reviewer as Reviewer;
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "dispatch_review", actionId, reviewer });
        void this.#runReview(actionId, reviewer).catch((err) => this.#logUnexpected("dispatch_review", err));
        return;
      }
      case "accept":
        this.#applyEvent({ type: "ACCEPTED", resolvedCorrectionIds: action.resolvedCorrectionIds as string[] });
        return;
      // Plan 01f: the phase is acceptable and its contract declares a gate —
      // enter GATING, from where next() asks for the gate itself.
      case "gate_required":
        this.#applyEvent({ type: "GATE_REQUIRED" });
        return;
      case "run_gate": {
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "run_gate", actionId });
        void this.#runGate(actionId, action.candidateSha as string).catch((err) => this.#logUnexpected("run_gate", err));
        return;
      }
      // Plan 06c: the candidate is acceptable and its contract declares a
      // final check — enter FINAL_CHECKING, from where next() asks for it.
      case "final_check_required":
        this.#applyEvent({ type: "FINAL_CHECK_REQUIRED" });
        return;
      case "run_final_checks": {
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "run_final_checks", actionId });
        void this.#runChecks(actionId, action.candidateSha as string).catch((err) => this.#logUnexpected("run_final_checks", err));
        return;
      }
      case "resolving_incomplete":
        this.#applyEvent({ type: "RESOLVING_INCOMPLETE" });
        return;
      // Plan 01g: a passing amendment rewrites one acceptance item for this
      // phase. The conductor computes the replacement acceptance list and the
      // new contract version; the core row applies them and starts a fresh
      // attempt so the next candidate is judged against the new wording.
      case "apply_amendment": {
        const decisionId = action.decisionId as string;
        const decision = this.#state.phase.decisions.find((d) => d.id === decisionId);
        const amendment = decision?.amendment;
        if (!decision || !amendment) {
          this.#logUnexpected("apply_amendment", new Error(`unknown amendment decision ${decisionId}`));
          return;
        }
        const acceptance = this.#state.phase.contract.acceptance.map((a) =>
          a === amendment.criterion ? amendment.proposedWording : a,
        );
        this.#applyEvent({
          type: "CRITERION_AMENDED",
          decisionId,
          newAcceptance: acceptance,
          newContractVersion: amendContractVersion(this.#state.phase.contract, acceptance),
        });
        return;
      }
      case "publish_intent":
        this.#applyEvent({
          type: "PUBLISH_INTENT",
          expectedHead: action.expectedHead as string,
          candidateI: action.candidateI as string,
        });
        return;
      case "publish_cas": {
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "publish_cas", actionId });
        void this.#runPublish(actionId, action.expectedHead as string, action.candidateI as string).catch((err) =>
          this.#logUnexpected("publish_cas", err),
        );
        return;
      }
      case "repair_attempt_started":
        this.#applyEvent({ type: "REPAIR_ATTEMPT_STARTED" });
        return;
      case "repair_budget_exhausted":
        this.#applyEvent({ type: "REPAIR_BUDGET_EXHAUSTED" });
        return;
      default:
        this.#logUnexpected("dispatch", new Error(`unhandled action type ${action.type}`));
    }
  }

  #logUnexpected(where: string, err: unknown): void {
    // A background dispatch's `.catch` can legitimately still be settling
    // after `stop()` already closed the log (e.g. an agent it was talking
    // to got torn down concurrently) — appending to an already-closed
    // EventLog throws EBADF, which as an unhandled rejection from a bare
    // `.catch` handler can crash the whole process. Mirrors `#applyEvent`'s
    // own `if (this.#closed) return` guard.
    if (this.#closed) return;
    this.#log.append("error", { where, error: String((err as Error)?.message ?? err) });
  }

  // -- socket handlers ---------------------------------------------------

  #onHello(agentId: string, hello: HelloMessage): HelloResult {
    const handle = this.#agents.get(agentId);
    const result = assertToolSet(hello.role, hello.tools);
    if (!handle) {
      this.#log.append("launch_failure", { agentId, reason: "hello from an agent the conductor did not launch" });
      return { ok: false, reason: "unknown agent" };
    }
    if (!result.ok) {
      const mismatch = result as ToolSetMismatch;
      this.#log.append("launch_failure", { agentId, role: hello.role, mismatch });
      const helloResult: HelloResult = {
        ok: false,
        reason: `tool-set mismatch: ${JSON.stringify(mismatch)}`,
        mismatch: { expected: ROLE_TOOLS[hello.role], missing: mismatch.missing, extra: mismatch.extra },
      };
      handle.helloResolve(helloResult);
      return { ok: false, reason: "tool-set mismatch" };
    }
    handle.helloResolve({ ok: true });
    return { ok: true };
  }

  async #onSubmit(agentId: string, msg: SubmitMessage): Promise<SubmitResult> {
    const result = await this.#onSubmitChecked(agentId, msg);
    // Plan 01a: a rejection reason is delivered back to the model (the
    // extension shows it as the tool's error), and a validation error quotes
    // the field it refused — so the reason is redacted like every other text
    // on its way to a prompt.
    return result.reason === undefined || this.#secretMaskable.length === 0
      ? result
      : { ...result, reason: redactText(result.reason, this.#secretMaskable) };
  }

  async #onSubmitChecked(agentId: string, msg: SubmitMessage): Promise<SubmitResult> {
    const handle = this.#agents.get(agentId);
    if (!handle) return { ok: false, reason: "unknown agent" };
    if (msg.tool === "submit_coverage") {
      // Plan 06b: the worker's per-item coverage. It is recorded even when
      // incomplete (so the worker and the status see what is missing), but
      // the freeze is refused until every R, C and A is covered with its
      // required note.
      if (handle.role !== "worker" || this.#state.phase.phase !== "IMPLEMENTING") {
        return { ok: false, reason: `submit_coverage is not accepted in phase ${this.#state.phase.phase}` };
      }
      // Plan 06g2: a lane worker's coverage belongs to its lane, not to the
      // phase — the winner's is applied at the hand-off. An incomplete one is
      // refused back to the lane worker like any other.
      if (handle.lane !== undefined) {
        if (!this.#structured()) return { ok: true };
        const args = msg.args as Coverage;
        const issues = coverageIssues(args, this.#planItems());
        handle.laneCoverage = args;
        if (issues.length > 0) {
          const reason = `coverage is incomplete: ${issues.join("; ")}`;
          this.#log.append("coverage_refused", { at: "submit_coverage", lane: handle.lane, issues, reason });
          return { ok: false, reason };
        }
        return { ok: true };
      }
      // OD-1: an old-format phase owes submit_phase only, so a stray coverage
      // call is a harmless no-op rather than a refusal.
      if (!this.#structured()) return { ok: true };
      const args = msg.args as Coverage;
      const issues = coverageIssues(args, this.#planItems());
      // OD-2 A2: bind the report to the attempt it was submitted in.
      this.#applyEvent({ type: "ITEM_STATE_UPDATED", coverage: args, coverageAttempt: this.#state.phase.attempt.n });
      if (issues.length > 0) {
        const reason = `coverage is incomplete: ${issues.join("; ")}`;
        this.#log.append("coverage_refused", { at: "submit_coverage", issues, reason });
        return { ok: false, reason };
      }
      if (this.#state.phase.candidate) this.#applyCoverageNotes(args, this.#state.phase.candidate.sha);
      return { ok: true };
    }
    if (msg.tool === "submit_phase") {
      if (handle.role !== "worker" || this.#state.phase.phase !== "IMPLEMENTING") {
        return { ok: false, reason: `submit_phase is not accepted in phase ${this.#state.phase.phase}` };
      }
      // Plan 06g2: a lane worker's submission is the lane's candidate, never
      // the phase's own — the round freezes it, and the winner's payload is
      // applied at the hand-off (a synthetic SUBMIT_PHASE). The coverage gate
      // is the same one the phase's own worker owes.
      if (handle.lane !== undefined) {
        if (this.#itemsEnforced()) {
          const args = handle.laneCoverage as Coverage | undefined;
          const issues = args ? coverageIssues(args, this.#planItems()) : ["no coverage was submitted for this lane"];
          if (issues.length > 0) {
            const reason = `submit_phase refused until submit_coverage is complete: ${issues.join("; ")}`;
            this.#log.append("coverage_refused", { at: "submit_phase", lane: handle.lane, issues, reason });
            return { ok: false, reason };
          }
        }
        const args = msg.args as SubmitPhaseArgs;
        handle.laneSubmission = {
          disclosures: args.decisions ?? [],
          ...(args.priorDecisions ? { prior: args.priorDecisions } : {}),
          ...(args.criterionDispute ? { dispute: args.criterionDispute } : {}),
        };
        handle.doneResolve();
        return { ok: true };
      }
      // Plan 06b: the freeze is refused until the coverage is complete AND
      // bound to the CURRENT attempt (OD-2 A2).
      if (this.#itemsEnforced()) {
        const bound = this.#state.phase.coverageAttempt === this.#state.phase.attempt.n;
        const issues = bound ? coverageIssues(this.#state.phase.coverage, this.#planItems()) : ["no coverage was submitted for this attempt"];
        if (issues.length > 0) {
          const reason = `submit_phase refused until submit_coverage is complete: ${issues.join("; ")}`;
          this.#log.append("coverage_refused", { at: "submit_phase", issues, reason });
          return { ok: false, reason };
        }
      }
      // design §6.2/§9.3 (round-of-review item 3): SUBMIT_PHASE is logged
      // with the raw disclosure *before* anything else — freeze is an
      // external effect with its own recovery rule and must not bypass
      // intent/completion. Applying it here moves IMPLEMENTING -> FREEZING,
      // which makes next() recommend `freeze`; #dispatch's "freeze" case
      // (driven synchronously by this #applyEvent, before this function
      // even returns) is what actually runs the §6.2 sequence — see
      // #runFreeze. The tool result below still goes back to the worker
      // before that sequence's first side effect (the RPC abort).
      const args = msg.args as SubmitPhaseArgs;
      // Plan 2c: prior-decision statements must name live worker records;
      // anything else is a model mistake the worker can fix and resubmit.
      const prior = args.priorDecisions ?? [];
      // Plan 01g: a dispute must name one of this phase's acceptance items
      // verbatim — an amendment of anything else could never apply, so it is
      // refused back to the worker rather than becoming a record the
      // reviewers waste a turn on.
      const dispute = args.criterionDispute;
      if (dispute && (!dispute.why || dispute.why.trim().length === 0 || !dispute.proposedWording || dispute.proposedWording.trim().length === 0)) {
        return { ok: false, reason: "criterionDispute needs a non-empty why and proposedWording" };
      }
      if (dispute && !this.#state.phase.contract.acceptance.includes(dispute.criterion)) {
        return {
          ok: false,
          reason: `criterionDispute names a criterion that is not one of this phase's acceptance items verbatim: ${JSON.stringify(dispute.criterion)}`,
        };
      }
      for (const st of prior) {
        const d = this.#state.phase.decisions.find((x) => x.id === st.id);
        if (!d || d.source !== "worker" || !isLiveDecision(d)) {
          return { ok: false, reason: `priorDecisions: ${st.id} is not one of your prior decisions listed in the prompt` };
        }
        if (st.status === "changed" && !st.choice) {
          return { ok: false, reason: `priorDecisions: ${st.id} is 'changed' but gives no new choice` };
        }
      }
      this.#activeWorkerHandle = handle;
      this.#applyEvent({
        type: "SUBMIT_PHASE",
        disclosures: args.decisions ?? [],
        ...(prior.length > 0 ? { prior } : {}),
        ...(dispute ? { dispute } : {}),
      });
      // Unblocks #runWorkerAttempt's race with outcome "submitted" (rather
      // than falling through to "settled" once the freeze's own abort makes
      // the agent settle, which would misreport this as no_submission).
      handle.doneResolve();
      return { ok: true };
    }
    if (msg.tool === "submit_review") {
      // Plan 06g2: a lane reviewer's review belongs to the round's candidate
      // (the phase is still IMPLEMENTING and has no candidate of its own). It
      // is recorded as the round's per-candidate review; the winner's is
      // promoted into the phase's review slots at the hand-off.
      if (handle.laneReview !== undefined) {
        const { round, lane, sha, seat } = handle.laneReview;
        const review = msg.args as Review;
        if (review.candidateSha !== sha) {
          return { ok: false, reason: `submit_review candidateSha ${review.candidateSha} does not match lane ${lane}'s candidate ${sha}` };
        }
        if (review.reviewer !== seat) {
          return { ok: false, reason: `submit_review reviewer ${review.reviewer} does not match the seat this dispatch reviews as (${seat})` };
        }
        const issue = reviewIngestionIssue(review);
        if (issue) return { ok: false, reason: issue };
        const itemIssues = this.#reviewItemIssues(review);
        // A ballot is required for every votable record the lane's turn-2
        // prompt listed (the worker's own decisions and the seats'
        // discoveries); an incomplete review is refused back to the model
        // within the same turn, like the single-candidate path.
        const missing = this.#missingDemandedBallots(review, handle);
        if (missing.length > 0 || itemIssues.length > 0) {
          const rejections = handle.incompleteReviewRejections ?? 0;
          if (rejections < MAX_INCOMPLETE_REVIEW_REJECTIONS) {
            handle.incompleteReviewRejections = rejections + 1;
            const reason =
              `incomplete review: a ballot is required for every record the turn-2 prompt listed. Missing: ${missing
                .map((m) => `${m.id} (${m.choice})`)
                .join("; ")}` + (itemIssues.length > 0 ? `; item verdicts: ${itemIssues.join("; ")}` : "");
            this.#log.append("incomplete_review_rejected", { reviewer: seat, agentId, lane, missing: missing.map((m) => m.id), itemIssues });
            return { ok: false, reason };
          }
          this.#log.append("incomplete_review", { reviewer: seat, agentId, lane, missing: missing.map((m) => m.id), itemIssues, rejections });
        }
        this.#applyEvent({ type: "ROUND_REVIEW_SUBMITTED", round, lane, seat, review });
        this.#log.append("lane_review_submitted", { round, lane, seat, candidateSha: sha });
        handle.doneResolve();
        return { ok: true };
      }
      if (handle.role !== "reviewer" || this.#state.phase.phase !== "REVIEWING") {
        return { ok: false, reason: `submit_review is not accepted in phase ${this.#state.phase.phase}` };
      }
      let review = msg.args as Review;
      if (review.candidateSha !== this.#state.phase.candidate?.sha) {
        return { ok: false, reason: "submit_review candidateSha does not match the current candidate" };
      }
      // Plan 01a: a reproduction command is a command the conductor runs
      // (design §8.1), so it is refused for the same reason an `sh` command
      // is, in the same words, before anything is recorded. A reviewer has to
      // write `$NAME`. (The value inside a *text* field — evidence, a ballot's
      // rationale — is redacted rather than refused: quoting the output a value
      // leaked into is legitimate evidence, and `***NAME***` keeps it useful.)
      for (const fd of review.findings ?? []) {
        const leak = fd.reproduction ? secretUseInCommand(fd.reproduction.command, this.#secretNames) : undefined;
        if (leak !== undefined) return { ok: false, reason: leak };
      }
      if (this.#state.phase.phase !== "REVIEWING" || review.candidateSha !== this.#state.phase.candidate?.sha) {
        // The round this review belongs to has ended (run cc1992e2: B's
        // review arrived after the phase moved to a repair). Acknowledge it
        // so the late agent stops, and change nothing.
        this.#log.append("stale_review_ignored", {
          reviewer: review.reviewer,
          kind: "submission",
          candidateSha: review.candidateSha,
          phase: this.#state.phase.phase,
        });
        return { ok: true };
      }
      if (!this.#stubReviews && !this.#discoverySubmitted.has(agentId)) {
        // Work packet 2a, design §6.1: turn 2 must not be accepted before
        // turn 1 (submit_discovery) was — this is the two-turn ordering
        // guarantee itself, enforced structurally (the conductor's own
        // #runReview never even sends the turn-2 prompt first), but it is
        // cheap and worth also rejecting here in case a script/model races
        // ahead of its own prompt.
        return { ok: false, reason: "submit_review called before submit_discovery was accepted (turn order)" };
      }
      if (this.#stubReviews) {
        this.#applyEvent({ type: "REVIEW_SUBMITTED", review });
        this.#castStubBallots(review.reviewer);
      } else {
        // Plan 01d: enforce a complete ballot. Every record this dispatch's
        // turn-2 prompt listed as votable (delegated/reserved, not carried)
        // needs a ballot (or a valid discoveryMatch retiring it). An
        // incomplete review is rejected back to the model — naming every
        // missing id and its one-line choice — so it resubmits within the
        // same turn, and after MAX_INCOMPLETE_REVIEW_REJECTIONS the review is
        // accepted as-is with an `incomplete_review` log record, so a
        // stubborn model cannot wedge the turn. The demanded set is the
        // prompt-time snapshot, so a late discovery is never demanded.
        const missing = this.#missingDemandedBallots(review, handle);
        // Plan 06b: the same complete-ballot rule extended to items — every
        // R and C needs a verdict, every A needs one, and every verdict must
        // cite anchors the code can follow. A refusal re-asks the reviewer.
        const itemIssues = this.#reviewItemIssues(review);
        if (missing.length > 0 || itemIssues.length > 0) {
          const rejections = handle.incompleteReviewRejections ?? 0;
          if (rejections < MAX_INCOMPLETE_REVIEW_REJECTIONS) {
            handle.incompleteReviewRejections = rejections + 1;
            // The reason is logged beside the missing ids, so the record is the
            // same text the model is refused with (a test does not have to race
            // the reviewer's stream file to read it).
            const ballots = `a ballot is required for every listed record not marked carried. Missing: ${missing
              .map((m) => `${m.id} (${m.choice})`)
              .join("; ")}`;
            const reason =
              itemIssues.length > 0
                ? `incomplete or unverifiable review: ${itemIssues.join("; ")}${missing.length > 0 ? `; ${ballots}` : ""}`
                : `incomplete review: ${ballots}`;
            this.#log.append("incomplete_review_rejected", {
              reviewer: review.reviewer,
              agentId,
              missing: missing.map((m) => m.id),
              itemIssues,
              rejection: rejections + 1,
              reason,
            });
            return { ok: false, reason };
          }
          this.#log.append("incomplete_review", {
            reviewer: review.reviewer,
            agentId,
            missing: missing.map((m) => m.id),
            itemIssues,
            rejections,
          });
        }
        // Plan 06b (finding M-16): an invalid verdict never counts, even once
        // the rejection cap accepts the review as-is. Drop every item verdict
        // the code refused, so it cannot enter the tally.
        if (itemIssues.length > 0 && this.#itemsEnforced()) {
          const items = this.#planItems();
          const flat = flatItems(items);
          const valid = (v: { id: string }): boolean => {
            const item = flat.find((i) => i.id === v.id);
            return item ? verdictIssues(item, v as import("./core/items.ts").ItemVerdict, this.#verdictContext(item, review.reviewer)).length === 0 : true;
          };
          review = { ...review, items: (review.items ?? []).filter(valid), arch: (review.arch ?? []).filter(valid) };
        }
        // Ordering matters, and in TWO conflicting directions at once — a
        // real bug this packet's own contract-objection test caught: if
        // REVIEW_SUBMITTED is applied first and this happens to be the
        // LAST of the three reviews, `#applyEvent`'s own synchronous
        // `drive()` can see `reviewsComplete()` go true and recommend
        // `accept` immediately — reading `phase.findings`/`phase.ballots`
        // as they stood BEFORE this reviewer's own ballots/findings (e.g.
        // a contract objection) were ever recorded, so a candidate could
        // be accepted with the very ballot that should have blocked it
        // still unapplied. So: findings + ballots (from `#applyReviewFindingsAndBallots`)
        // are applied FIRST — any contract objection's linked finding is
        // already open before REVIEW_SUBMITTED can trigger acceptance.
        // Only the findingStatements-derived confirm/withdraw
        // (`#applyReviewFindingStatements`) needs `phase.reviews[reviewer]
        // .review` to already exist (reduce.ts's own
        // FINDING_CONFIRMED_REPAIRED guard, design §4.2) — so that step
        // runs LAST, after REVIEW_SUBMITTED.
        if (handle.reviewInFlight) return { ok: false, reason: "this review is already being recorded" };
        handle.reviewInFlight = true;
        let error: string | undefined;
        try {
          error = await this.#applyReviewFindingsAndBallots(review);
        } catch (err) {
          error = `threw: ${String((err as Error)?.message ?? err)}`;
        } finally {
          handle.reviewInFlight = false;
        }
        if (error === STALE_REVIEW || !this.#reviewStillCurrent(review.candidateSha)) {
          // Run cc1992e2: recording findings awaits reproductions, and the
          // round ended meanwhile (the same reviewer's earlier submission
          // closed it). What was not yet applied is dropped, and the agent is
          // acknowledged so it stops.
          this.#log.append("stale_review_ignored", {
            reviewer: review.reviewer,
            kind: "submission_after_wait",
            candidateSha: review.candidateSha,
            phase: this.#state.phase.phase,
          });
          handle.doneResolve();
          return { ok: true };
        }
        if (error) {
          this.#log.append("review_outcome_error", { reviewer: review.reviewer, error });
          return { ok: false, reason: error };
        }
        // Plan 05e: once all three reviews are in, the round's resolution
        // ballots are counted (a majority `resolved` moves a message out of
        // the live view). Candidate approval is recorded later, when the
        // round's EVALUATING has settled (see `evaluation_complete`).
        try {
          this.#applyRoundResolutions(review);
        } catch (err) {
          this.#log.append("error", { where: "round_resolutions", error: String((err as Error)?.message ?? err) });
        }
        this.#applyEvent({ type: "REVIEW_SUBMITTED", review });
        try {
          this.#applyReviewFindingStatements(review);
        } catch (err) {
          // Best-effort: a confirm/withdraw statement that reduce.ts's own
          // guard would reject (e.g. arrived on the same candidate it was
          // raised on) is not a submission-fatal error — the review itself
          // is already recorded above.
          this.#log.append("error", { where: "review_finding_statements", error: String((err as Error)?.message ?? err) });
        }
      }
      handle.doneResolve();
      return { ok: true };
    }
    if (msg.tool === "submit_pick_vote") {
      // Plan 06g2: one seat's vote in the round's pick turn. The round decides
      // the winner in code (`pickWinner`); this only records the vote.
      if (handle.pickSeat === undefined) {
        return { ok: false, reason: "submit_pick_vote is only accepted from a seat's pick turn" };
      }
      const args = msg.args as { round?: unknown; seat?: unknown; lane?: unknown; why?: unknown };
      const round = typeof args.round === "number" ? args.round : Number(args.round);
      const seat = typeof args.seat === "string" ? args.seat : handle.pickSeat;
      const lane = typeof args.lane === "string" ? args.lane : "";
      const why = typeof args.why === "string" ? args.why.trim() : "";
      if (!Number.isInteger(round) || round !== handle.laneRound) {
        return { ok: false, reason: `submit_pick_vote names round ${String(args.round)}, not the pick turn's round ${String(handle.laneRound)}` };
      }
      if (seat !== handle.pickSeat) {
        return { ok: false, reason: `submit_pick_vote names seat ${seat}, not this turn's seat ${handle.pickSeat}` };
      }
      if (why.length === 0) return { ok: false, reason: "submit_pick_vote needs a non-empty why" };
      const roundRecord = (this.#state.phase.rounds ?? []).find((r) => r.round === round);
      if (!roundRecord) return { ok: false, reason: `round ${round} has not started` };
      const candidate = roundRecord.candidates.find((c) => c.lane === lane);
      if (!candidate?.sha) return { ok: false, reason: `submit_pick_vote names lane ${lane}, which submitted no candidate of round ${round}` };
      if (candidate.ok !== true) return { ok: false, reason: `submit_pick_vote names lane ${lane}, whose candidate did not pass its checks` };
      this.#applyEvent({ type: "PICK_VOTE", round, seat, lane, why, ...(handle.pickRevote ? { revote: true } : {}) });
      handle.doneResolve();
      return { ok: true };
    }
    if (msg.tool === "submit_discovery") {
      // Plan 06g2: a lane review's turn 1. The discoveries belong to the
      // lane's candidate, which the phase does not hold while the round runs,
      // so they are kept per lane until the winner's hand-off applies them.
      if (handle.laneReview !== undefined) {
        const discoveries = (msg.args as { discoveries?: DecisionDisclosure[] }).discoveries ?? [];
        handle.laneDiscoveries = discoveries;
        const key = `${handle.laneReview.round}-${handle.laneReview.lane}`;
        const bySeat = this.#laneDiscoveries.get(key) ?? new Map<string, DecisionDisclosure[]>();
        bySeat.set(handle.laneReview.seat, discoveries);
        this.#laneDiscoveries.set(key, bySeat);
        this.#log.append("discovery_submitted", {
          agentId,
          reviewer: handle.laneReview.seat,
          count: discoveries.length,
          lane: handle.laneReview.lane,
          round: handle.laneReview.round,
        });
        handle.discoveryResolve();
        return { ok: true };
      }
      if (this.#stubReviews) {
        // Phase 1's stub reviewers are not expected to call it — accepted
        // as a no-op so a scripted call does not fail a test outright.
        return { ok: true };
      }
      if (handle.role !== "reviewer" || this.#state.phase.phase !== "REVIEWING") {
        return { ok: false, reason: `submit_discovery is not accepted in phase ${this.#state.phase.phase}` };
      }
      const discoveries = (msg.args as { discoveries?: DecisionDisclosure[] }).discoveries ?? [];
      const reviewer = this.#reviewerFromAgentId(agentId);
      const error = this.#applyDiscoveries(discoveries, reviewer);
      if (error) return { ok: false, reason: error };
      this.#discoverySubmitted.add(agentId);
      // Log-only record (not a core Event — same precedent as the
      // "sampling"/"sweep" kinds): a discovery with zero findings produces
      // no DECISION_ADDED event at all, so this is the only durable trace
      // that turn 1 actually happened for this reviewer — a live test
      // checks it directly.
      this.#log.append("discovery_submitted", { agentId, reviewer, count: discoveries.length });
      handle.discoveryResolve();
      return { ok: true };
    }
    if (msg.tool === "raise_tradeoff") {
      // Plan 04a: the worker raises a choice the plan did not fix the moment
      // it makes it. Callable any time during implementation. It is a raw
      // message; the type's evaluator publishes it later. The evaluator role
      // does NOT have this tool (roles.ts), so this is worker-only.
      if (handle.role !== "worker") {
        return { ok: false, reason: `raise_tradeoff is not accepted from role ${handle.role}` };
      }
      if (this.#state.phase.phase !== "IMPLEMENTING") {
        return { ok: false, reason: `raise_tradeoff is only accepted during implementation (phase ${this.#state.phase.phase})` };
      }
      const args = msg.args as { choice?: unknown; alternative?: unknown; why?: unknown; anchor?: unknown; planRef?: unknown };
      const choice = typeof args.choice === "string" ? args.choice.trim() : "";
      const alternative = typeof args.alternative === "string" ? args.alternative.trim() : "";
      const why = typeof args.why === "string" ? args.why.trim() : "";
      if (!choice || !alternative || !why) {
        return { ok: false, reason: "raise_tradeoff needs a non-empty choice, alternative and why" };
      }
      const anchor = parseAnchor(args.anchor);
      if (!anchor) return { ok: false, reason: "raise_tradeoff needs anchor = {path, lines: [start, end]}" };
      // Plan 05e: a trade-off raised for a fix may name the message it closes
      // (`closes: F-3`); the renderer shows the link on both messages.
      const closes = typeof args.closes === "string" && args.closes.trim().length > 0 ? args.closes.trim() : undefined;
      // Bind to the current candidate when one exists; otherwise to the
      // integration head, and let the freeze's MESSAGE_CARRIED rebind it.
      const candidateSha = this.#state.phase.candidate?.sha ?? this.#state.phase.integrationHead;
      // Only a planRef the worker actually named; the phase id is not a plan
      // clause (finding A-34).
      const planRef = typeof args.planRef === "string" && args.planRef.trim().length > 0 ? args.planRef.trim() : undefined;
      this.#raiseMessage(
        "tradeoff",
        undefined,
        { type: "tradeoff", title: choice, summary: why, context: alternative, evidence: [`${anchor.path}:${anchor.lines[0]}-${anchor.lines[1]}`], ...(planRef ? { planRef } : {}) },
        candidateSha,
        { anchor, ...(closes ? { closes } : {}) },
      );
      this.#log.append("tradeoff_raised", { role: handle.role, choice, anchor, ...(closes ? { closes } : {}) });
      return { ok: true };
    }
    if (msg.tool === "submit_evaluation") {
      // Plan 04a: one type's evaluator outcome. It is the only way that
      // type's raw messages become published/merged/dropped in this profile.
      if (handle.role !== "evaluator" || this.#state.phase.phase !== "EVALUATING") {
        return { ok: false, reason: `submit_evaluation is not accepted in phase ${this.#state.phase.phase}` };
      }
      const messageType = handle.messageType;
      if (!messageType) return { ok: false, reason: "this evaluator is not bound to a message type" };
      const entries = (msg.args as { evaluations?: unknown }).evaluations;
      if (!Array.isArray(entries)) {
        return { ok: false, reason: "submit_evaluation needs an evaluations array" };
      }
      // Plan 06b (OD-2 A3): ONE item-check form, submit_evaluation.itemChecks[]
      // = { id, verdict, evidence }. For every item with a review-only
      // majority unmet/deviates the evaluator owes a check: a missing one is
      // re-prompted once, then recorded `unchecked` (visible, never silent).
      // Plan 06i: the same form carries a finding's/discovered decision's
      // impact class. Parsed BEFORE the B-16 whole-submission check, because a
      // triage classification is independent of the message evaluations: a
      // submission whose evaluations are incomplete still classifies its
      // records (they are recorded), while the messages themselves stay
      // unevaluated exactly as before.
      const owed = this.#owedItemCheckIds(messageType);
      const checkEvents: Event[] = [];
      const provided = new Set<string>();
      const itemChecks = (msg.args as { itemChecks?: unknown }).itemChecks;
      if (Array.isArray(itemChecks)) {
        for (const c of itemChecks as Array<{ id?: unknown; verdict?: unknown; evidence?: unknown; impact?: unknown; chosen?: unknown; alternative?: unknown; why?: unknown }>) {
          const id = typeof c?.id === "string" ? c.id.trim() : "";
          const verdict = c?.verdict === "confirmed" || c?.verdict === "contradicted" ? c.verdict : undefined;
          const evidence = typeof c?.evidence === "string" ? c.evidence.trim() : "";
          // Plan 06i: the same item-check form carries the evaluator's impact
          // class for a finding or discovered decision. An unknown value is
          // ignored, which leaves the record unclassified and escalates.
          const impact =
            c?.impact === "wrong-output" || c?.impact === "contract" || c?.impact === "judgement" ? c.impact : undefined;
          // Plan 06i: a judgement call's own chosen/alternative/why, from the
          // evaluator. The conductor never supplies stock text.
          const chosen = typeof c?.chosen === "string" && c.chosen.trim().length > 0 ? c.chosen.trim() : undefined;
          const alternative = typeof c?.alternative === "string" && c.alternative.trim().length > 0 ? c.alternative.trim() : undefined;
          const why = typeof c?.why === "string" && c.why.trim().length > 0 ? c.why.trim() : undefined;
          const isTriage = openFindings(this.#state.phase).some((f) => f.id === id) || discoveredDecisions(this.#state.phase).some((d) => d.id === id);
          if (id.length === 0 || !verdict || evidence.length === 0) continue;
          if (isTriage && impact === undefined) continue;
          // OD-15(2): for a finding/decision, only a CONFIRMED check with an
          // impact is a supplied classification. A `contradicted` verdict does
          // not classify (triage.ts only reads confirmed), so it must be
          // re-prompted once like a missing one, then escalated. A plan item's
          // contradicted check is still a supplied check (it overturns).
          if (!isTriage || verdict === "confirmed") provided.add(id);
          // OD-2 A3 (carried here): a `confirmed` check has its anchors
          // validated exactly like a `contradicted` one. A confirmed check
          // whose anchors do not exist in the candidate is recorded as
          // `unchecked by evaluator`, never as a confirmation.
          if (verdict === "confirmed") {
            const item = flatItems(this.#planItems()).find((i) => i.id === id);
            const ctx = item ? this.#verdictContext(item, "M") : undefined;
            // Plan 06i: a finding or discovered decision is not a plan item, so
            // its confirmed anchors are validated directly against the
            // candidate (the same 06c rule).
            // A finding or discovered decision is not a plan item: its
            // confirmed anchors are a candidate file:line. A golden note may
            // substitute ONLY for a `contract` classification — a wrong-output
            // claim still needs a reachable-path anchor (A3's 06c rule).
            const goldenOk = impact === "contract" && goldenCited(this.#state.phase, evidence);
            const valid = item
              ? Boolean(ctx) && this.#itemCheckAnchorsValid(evidence, ctx!)
              : this.#candidateAnchorsValid(evidence) || goldenOk;
            if (!valid) {
              this.#log.append("item_check_anchor_invalid", { messageType, agentId, itemId: id, evidence });
              checkEvents.push({
                type: "ITEM_CHECK_RECORDED",
                itemId: id,
                verdict: "unchecked",
                evidence: `unchecked by evaluator: the confirmed check's anchors do not exist in the candidate (${evidence})`,
              });
              continue;
            }
          }
          checkEvents.push({
            type: "ITEM_CHECK_RECORDED",
            itemId: id,
            verdict,
            evidence,
            ...(impact ? { impact } : {}),
            ...(chosen ? { chosen } : {}),
            ...(alternative ? { alternative } : {}),
            ...(why ? { why } : {}),
          });
        }
      }
      // B-16 / owner directive 1: validate the WHOLE submission before
      // applying the message evaluations. An invalid one is refused back to
      // the evaluator (like an incomplete review), never partly applied —
      // except that the independent triage classifications above are kept,
      // so a record is never left unclassified merely because the evaluator's
      // message entries were incomplete.
      const issue = this.#evaluationIssue(messageType, entries as EvaluationEntry[]);
      if (issue) {
        // Plan 06i: the item checks are independent of the message
        // evaluations, so they are recorded even when the submission is
        // refused for its message entries (B-16 governs the evaluations, not
        // the classification).
        if (checkEvents.length > 0) this.#applyEvents(checkEvents);
        return { ok: false, reason: `invalid evaluation: ${issue}` };
      }
      // Plan 05c: a published title must be one complete line, not a
      // truncation at the 80-character cap. Refused back to the model, at
      // most MAX_INCOMPLETE_REVIEW_REJECTIONS times, then accepted as is —
      // the same one-turn rule `submit_review` uses for an incomplete ballot.
      const titleIssue = this.#evaluationTitleIssue(messageType, entries as EvaluationEntry[]);
      if (titleIssue) {
        const rejections = handle.evaluationTitleRejections ?? 0;
        if (rejections < MAX_INCOMPLETE_REVIEW_REJECTIONS) {
          handle.evaluationTitleRejections = rejections + 1;
          this.#log.append("evaluation_title_rejected", { messageType, agentId, detail: titleIssue, rejection: rejections + 1 });
          return {
            ok: false,
            reason: `invalid title: ${titleIssue}. Give one complete line of at most 80 characters — rewrite it shorter rather than cutting it.`,
          };
        }
        this.#log.append("evaluation_title_accepted", { messageType, agentId, detail: titleIssue, rejections });
      }
      const events = this.#evaluationEvents(messageType, entries as EvaluationEntry[]);
      const missing = owed.filter((id) => !provided.has(id));
      if (missing.length > 0) {
        const rejections = handle.itemCheckRejections ?? 0;
        if (rejections < 1) {
          handle.itemCheckRejections = rejections + 1;
          this.#log.append("item_check_rejected", { messageType, agentId, missing });
          return {
            ok: false,
            reason: `submit_evaluation owes an itemCheck for: ${missing.join(", ")}. For a plan item give { id, verdict: confirmed|contradicted, evidence }. For a finding or discovered decision give { id, verdict: confirmed, impact: wrong-output|contract|judgement, evidence } and, when the impact is judgement, also chosen, alternative and why — without all three the record escalates to the owner.`,
          };
        }
        this.#log.append("item_check_unchecked", { messageType, agentId, missing });
        for (const id of missing) {
          checkEvents.push({ type: "ITEM_CHECK_RECORDED", itemId: id, verdict: "unchecked", evidence: "the evaluator gave no item check after a re-prompt; the record escalates" });
        }
      }
      this.#applyEvents([...events, ...checkEvents, { type: "EVALUATOR_FINISHED", messageType, evaluated: entries.length }]);
      handle.doneResolve();
      return { ok: true };
    }
    if (msg.tool === "submit_brief") {
      // Decision briefs: the evaluator's model writes one owner-readable brief
      // per owner item after a round's evaluation. Validated whole via
      // briefIssue (no code identifier in the question; a time, count or
      // duration cites the config/code it read; the option ids map one-to-one
      // to the item's own; the today example is checked against the plan's
      // calendars, including its weekly reopen). A record-only event, so a
      // restart rebuilds the briefs the views render.
      if (handle.role !== "evaluator") {
        return { ok: false, reason: `submit_brief is not accepted from role ${handle.role}` };
      }
      const brief = (msg.args ?? {}) as { requestId?: unknown };
      const requestId = typeof brief.requestId === "string" ? brief.requestId : "";
      const request = this.#state.phase.ownerRequests.find((r) => r.id === requestId && r.status === "open");
      // A flagged reserved decision never becomes an owner request, and a live
      // review entry is settled directly, so a brief for either is accepted
      // here too (each carries its own owner command).
      const decision = !request ? this.#liveReservedDecisions().find((d) => d.id === requestId) : undefined;
      const entry = !request && !decision ? this.#ownerMarkedEntries().find((e) => e.id === requestId) : undefined;
      if (!request && !decision && !entry) return { ok: false, reason: `brief ${requestId || "(no requestId)"} does not name an open owner item` };
      const requestOptions = request
        ? (request.options ?? []).map((o) => o.id)
        : decision
          ? ["approve", "reject_and_repair"]
          : ["accept", "refuse"];
      const issue = briefIssue(brief as never, { requestOptions, catalogs: this.#briefCatalogs() });
      if (issue) return { ok: false, reason: `invalid brief for ${requestId}: ${issue}` };
      const C = this.#state.phase.candidate?.sha;
      // The item class fixes the command, never the model's own field: a
      // reserved decision must send an override and an entry an entry verdict,
      // or the owner's A writes a resolve that matches nothing (F-A-16).
      const command = request ? "resolve" : decision ? "override" : "entry";
      // The conductor merges its own same-concern items into the model's
      // `related` (finding A-29), and stamps the candidate so the next round
      // rewrites the brief (finding M-30).
      const enriched = { ...enrichBriefRelated(brief as DecisionBrief, this.#briefConcerns()), candidateSha: C, command };
      this.#applyEvent({ type: "BRIEFS_RECORDED", briefs: [enriched] });
      this.#log.append("brief_recorded", { requestId, options: (brief as { options?: unknown }).options, candidateSha: C });
      // The brief agent is done only once IT has submitted every item it was
      // asked for. Existence alone is not enough: a retried backstop already
      // has a brief for this candidate, so checking the phase's briefs would
      // end the pass after the first submit (finding M-41).
      handle.briefSubmitted?.add(requestId);
      const ids = handle.briefInFlightIds ?? new Set<string>();
      if (ids.size > 0 && [...ids].every((id) => handle.briefSubmitted?.has(id))) handle.doneResolve();
      return { ok: true };
    }
    if (msg.tool === "submit_round_panel_votes") {
      // Plan 05e: one round-panel seat's batched votes on every pending
      // trade-off and blocking finding. Validated whole, then recorded.
      if (handle.role !== "panel" || this.#state.phase.phase !== "EVALUATING") {
        return { ok: false, reason: `submit_round_panel_votes is not accepted in phase ${this.#state.phase.phase}` };
      }
      const seat = handle.panelSeat;
      if (seat === undefined || !handle.agentId.startsWith("round-panel-")) {
        return { ok: false, reason: "this panel seat is not bound to the round panel" };
      }
      const seatState = this.#state.phase.panel?.round?.seats?.[String(seat)];
      if (seatState?.votes !== undefined) return { ok: false, reason: `this round panel seat (${seat}) already voted` };
      if (seatState?.unavailable === true && seatState.dispatches >= 2) {
        return { ok: false, reason: `this round panel seat (${seat}) is unavailable` };
      }
      const raw = (msg.args as { votes?: unknown }).votes;
      if (!Array.isArray(raw) || raw.length === 0) return { ok: false, reason: "submit_round_panel_votes needs a non-empty votes array" };
      const items = new Set(roundPanelItemsNeedingVote(this.#state.phase));
      const seen = new Set<string>();
      const votes: Array<{ messageId: string; verdict: "keep" | "drop" | "downgrade"; reason: string }> = [];
      for (const entry of raw as Array<{ messageId?: unknown; verdict?: unknown; reason?: unknown }>) {
        const messageId = typeof entry?.messageId === "string" ? entry.messageId : "";
        if (!items.has(messageId)) return { ok: false, reason: `${messageId || "(no messageId)"} is not a pending item of this round's panel` };
        if (seen.has(messageId)) return { ok: false, reason: `${messageId} is voted on more than once` };
        seen.add(messageId);
        const verdict = entry?.verdict;
        if (verdict !== "keep" && verdict !== "drop" && verdict !== "downgrade") {
          return { ok: false, reason: `verdict for ${messageId} must be keep, drop or downgrade` };
        }
        const reason = typeof entry?.reason === "string" ? entry.reason.trim() : "";
        if (!reason) return { ok: false, reason: `a reason is required for ${messageId}` };
        votes.push({ messageId, verdict, reason });
      }
      for (const id of items) if (!seen.has(id)) return { ok: false, reason: `every pending item needs a vote; ${id} is missing` };
      this.#applyEvent({ type: "ROUND_PANEL_VOTE", seat, votes });
      handle.doneResolve();
      return { ok: true };
    }
    if (msg.tool === "submit_panel_vote") {
      // Plan 04b: one panel seat's vote on one raw blocker. Validated whole
      // (a partial vote is never recorded), then applied as a record event;
      // the phase leaves EVALUATING only once every evaluator AND panel has
      // settled.
      if (handle.role !== "panel" || this.#state.phase.phase !== "EVALUATING") {
        return { ok: false, reason: `submit_panel_vote is not accepted in phase ${this.#state.phase.phase}` };
      }
      const blockerId = handle.blockerId;
      const seat = handle.panelSeat;
      if (!blockerId || seat === undefined) return { ok: false, reason: "this panel seat is not bound to a blocker" };
      const seatState = this.#state.phase.panel?.blockers?.[blockerId]?.seats?.[String(seat)];
      if (seatState?.vote !== undefined) return { ok: false, reason: `this seat (${seat}) already voted` };
      if (seatState?.unavailable === true && seatState.dispatches >= 2) {
        return { ok: false, reason: `this seat (${seat}) is unavailable` };
      }
      const args = msg.args as { vote?: unknown; reason?: unknown; options?: unknown };
      const vote = args.vote;
      if (vote !== "block" && vote !== "downgrade") {
        return { ok: false, reason: "submit_panel_vote needs vote: 'block' or 'downgrade'" };
      }
      const reason = typeof args.reason === "string" ? args.reason.trim() : "";
      if (!reason) return { ok: false, reason: "submit_panel_vote needs a non-empty reason" };
      const options = Array.isArray(args.options) ? (args.options as PanelOptionInput[]) : undefined;
      if (vote === "block") {
        if (!options || options.length < 2 || options.length > 3) {
          return { ok: false, reason: "a block vote must propose two or three options for the owner" };
        }
        for (const option of options) {
          if (!option || typeof option.id !== "string" || option.id.trim().length === 0 || typeof option.label !== "string" || option.label.trim().length === 0) {
            return { ok: false, reason: "every option needs a non-empty id and label" };
          }
        }
        // Two or three DISTINCT options — a repeated id or label would
        // collapse the owner's choice (round-3 review, advisory B-2).
        if (new Set(options.map((o) => o.id.trim())).size !== options.length) {
          return { ok: false, reason: "a block vote's options must have distinct ids" };
        }
        if (new Set(options.map((o) => o.label.trim())).size !== options.length) {
          return { ok: false, reason: "a block vote's options must read differently" };
        }
      }
      this.#applyEvent({
        type: "PANEL_VOTE",
        blockerId,
        seat,
        vote,
        reason,
        ...(vote === "block" ? { options: options!.map((o) => ({ id: o.id.trim(), label: o.label.trim() })) } : {}),
      });
      handle.doneResolve();
      return { ok: true };
    }
    if (msg.tool === "curate_entries") {
      // Plan 05j: the curator's link/open/retitle pass. The allow-list is the
      // tool's contract: any other op is refused before an event is applied.
      if (handle.role !== "curator") return { ok: false, reason: `curate_entries is not accepted from role ${handle.role}` };
      const proposals = (msg.args as { proposals?: unknown }).proposals;
      if (!Array.isArray(proposals)) return { ok: false, reason: "curate_entries needs a proposals array" };
      const entries = this.#state.phase.entries ?? [];
      const messages = this.#state.phase.messages ?? [];
      const events: Array<{ type: string; [key: string]: unknown }> = [];
      for (const proposal of proposals) {
        const built = curatorEvent(proposal as CuratorProposal, this.#state.phase.phaseId);
        if (!built.ok) return { ok: false, reason: built.reason };
        const event = built.event as unknown as { type: string; [key: string]: unknown };
        // A link is refused (and logged) when the message and entry share no
        // anchor; the message keeps its own entry (finding A-21). Any other
        // refusal is caught by the whole-batch dry run below, so no partial
        // application can happen and #applyEvent never throws (finding A-19).
        if (event.type === "MESSAGE_LINKED") {
          const entry = entries.find((e) => e.id === (event as { entryId?: string }).entryId);
          const message = messages.find((m) => m.id === (event as { messageId?: string }).messageId);
          const check = entry && message ? validateLink(entry, message, (event as { anchor?: never }).anchor) : { ok: false as const, reason: "unknown entry or message" };
          if (!check.ok) {
            this.#log.append("entry_link_refused", { entryId: (event as { entryId?: string }).entryId, messageId: (event as { messageId?: string }).messageId, reason: check.reason });
            continue;
          }
        }
        // A curator `open` naming a message an open entry already holds is
        // redundant (the runtime opened its entry at raise time); skip just
        // that proposal instead of refusing the whole batch (finding M-31).
        if (event.type === "ENTRY_OPENED") {
          const messageId = (event as { messageId?: string }).messageId;
          const holder = messageId ? entries.find((e) => e.state === "open" && e.links.some((l) => l.messageId === messageId)) : undefined;
          if (holder) {
            this.#log.append("entry_open_skipped", { messageId, entryId: holder.id, reason: "the message already belongs to an open entry" });
            continue;
          }
        }
        // A retitle of an entry that does not exist is skipped, not fatal to
        // the batch.
        if (event.type === "ENTRY_RETITLED" && !entries.some((e) => e.id === (event as { entryId?: string }).entryId)) {
          this.#log.append("entry_retitle_skipped", { entryId: (event as { entryId?: string }).entryId, reason: "no such entry" });
          continue;
        }
        events.push(event);
      }
      let check = this.#state;
      for (const event of events) {
        const result = reduce(check, event as never);
        if (!result.ok) return { ok: false, reason: result.reason };
        check = result.state;
      }
      for (const event of events) this.#applyEvent(event as never);
      this.#log.append("entries_curated", { count: events.length });
      handle.doneResolve();
      return { ok: true };
    }
    return { ok: false, reason: `unknown submission tool ${msg.tool}` };
  }

  /** B-16 (owner directive 1): why one `submit_evaluation` is refused before
   * anything is applied, or undefined when it is well-formed. */
  #evaluationIssue(messageType: MessageType, entries: EvaluationEntry[]): string | undefined {
    const messages = this.#state.phase.messages ?? [];
    const ids = new Set(messages.map((m) => m.id));
    const rawIds = messages.filter((m) => m.type === messageType && m.state === "raw").map((m) => m.id);
    const rawSet = new Set(rawIds);
    const refusedIds = messages.filter((m) => m.type === messageType && m.state === "refused").map((m) => m.id);
    const refusedSet = new Set(refusedIds);
    const seen = new Set<string>();
    for (const entry of entries) {
      const id = typeof entry?.messageId === "string" ? entry.messageId : "";
      if (!id) return "every evaluation entry must name a messageId";
      if (seen.has(id)) return `message ${id} is named more than once in one submit_evaluation`;
      seen.add(id);
      if (!rawSet.has(id) && !refusedSet.has(id)) {
        return `message ${id} is not a raw or owner-refused ${messageType} message of this round`;
      }
      // Owner item 4 / discs B-50, M-20, A-21: an owner-refused message is
      // NOT optional — the evaluator must report `addressed: true|false` for
      // it, so the report always reaches the ledger.
      if (refusedSet.has(id)) {
        if (typeof entry.addressed !== "boolean") {
          return `entry for owner-refused message ${id} must say addressed: true or false`;
        }
        continue;
      }
      if (entry.action !== "publish" && entry.action !== "merge" && entry.action !== "drop") {
        return `entry for ${id} must be publish, merge or drop`;
      }
      if (entry.action === "merge") {
        const into = typeof entry.into === "string" ? entry.into : "";
        // OD-1 / A-28: the target must be ANOTHER raw message of this type
        // this round — never the message itself, an already merged/dropped
        // one, a message of another type, or one from an earlier round.
        if (!into || !rawSet.has(into)) {
          return `merge of ${id} must name another raw ${messageType} message of this round (got ${JSON.stringify(into)})`;
        }
        if (into === id) return `merge of ${id} cannot target itself`;
      }
    }
    for (const id of rawIds) {
      if (!seen.has(id)) return `every raw ${messageType} message must appear exactly once; ${id} is missing`;
    }
    for (const id of refusedIds) {
      if (!seen.has(id)) return `every owner-refused ${messageType} message must appear exactly once; ${id} is missing`;
    }
    return undefined;
  }

  /** Plan 05c: the first `publish` entry whose title is not one complete line
   * — longer than the 80-character cap, ending in the ellipsis a cut leaves,
   * or ending mid-word because the model truncated the raw title — or
   * undefined when every title is fine. Named by message id so the model can
   * fix exactly that entry. */
  #evaluationTitleIssue(messageType: MessageType, entries: EvaluationEntry[]): string | undefined {
    const messages = this.#state.phase.messages ?? [];
    for (const entry of entries) {
      if (entry?.action !== "publish") continue;
      const id = typeof entry.messageId === "string" ? entry.messageId : "";
      const message = messages.find((m) => m.id === id && m.type === messageType);
      if (!message) continue;
      // A publish with no title falls back to the raw message's own title, so
      // the EFFECTIVE title is what the owner will read: a raw title over the
      // cap would be clamped to an ellipsis just the same, and is refused here
      // (finding A-3).
      const provided = typeof entry.title === "string" && entry.title.trim().length > 0 ? entry.title : message.title;
      const issue = titleIssue(message.title, provided);
      if (issue) return `${id}: ${issue}`;
    }
    return undefined;
  }

  /** Plan 04a: the message events one TYPE's evaluator submission produces.
   * Each raw message of the type is published (with its clean wording),
   * merged or dropped; an owner-refused message is resolved only when the
   * evaluator reports it addressed. The submission was already validated. */
  #evaluationEvents(messageType: MessageType, entries: EvaluationEntry[]): Event[] {
    const messages = this.#state.phase.messages ?? [];
    const byId = new Map(messages.map((m) => [m.id, m]));
    const events: Event[] = [];
    for (const entry of entries) {
      const id = typeof entry?.messageId === "string" ? entry.messageId : "";
      const message = byId.get(id);
      if (!message || message.type !== messageType) continue;
      // Plan 06i: an evaluator entry for a message that is neither raw nor
      // refused is a no-op — the message was already settled (e.g. a later
      // round's classification pass re-publishes nothing). Ignoring it keeps
      // the item-check half of the submission applicable.
      if (message.state !== "raw" && message.state !== "refused") continue;
      const binding = {
        messageId: id,
        boundCandidateSha: message.boundCandidateSha,
        boundContractVersion: message.boundContractVersion,
        boundRecordVersion: message.messageVersion,
      };
      if (message.state === "refused") {
        const reason = typeof entry.reason === "string" && entry.reason.trim().length > 0 ? entry.reason.trim() : undefined;
        if (entry.addressed === true) {
          events.push({ type: "MESSAGE_RESOLVED", ...binding, by: "evaluator", reason: reason ?? "addressed by the candidate" });
        } else {
          // addressed: false — record the report so the ledger distinguishes
          // "checked and not addressed" from "never checked".
          events.push({ type: "MESSAGE_ADDRESS_REPORTED", ...binding, addressed: false, ...(reason ? { reason } : {}), at: new Date().toISOString() });
        }
        continue;
      }
      const sourceFinding = message.sourceRecordId ? this.#state.phase.findings.find((f) => f.id === message.sourceRecordId) : undefined;
      if (entry.action === "merge") {
        const into = typeof entry.into === "string" && entry.into.trim().length > 0 ? entry.into.trim() : undefined;
        events.push({ type: "MESSAGE_MERGED", ...binding, by: "evaluator", reason: into ? `merged into ${into}` : "merged" });
        // Plan 06i (C3): the finding behind a merged message is MERGED, not
        // disproved, and linked to the record it duplicates. Its triage
        // disposition is the original's, so nothing leaves the ledger
        // without one.
        if (sourceFinding && sourceFinding.status === "open" && into) {
          events.push({ type: "FINDING_MERGED", findingId: sourceFinding.id, into, byReviewer: sourceFinding.raisedBy as Reviewer });
        }
      } else if (entry.action === "drop") {
        const reason = typeof entry.reason === "string" && entry.reason.trim().length > 0 ? entry.reason.trim() : "dropped by the evaluator";
        events.push({ type: "MESSAGE_DROPPED", ...binding, by: "evaluator", reason });
        // Plan 05e (3c): an advisory finding the evaluator cannot confirm is
        // dropped with its reason, and the finding itself is disproved — it
        // never reaches the owner as an open defect.
        if (sourceFinding && sourceFinding.status === "open") {
          events.push({ type: "FINDING_DISPROVED", findingId: sourceFinding.id, byReviewer: sourceFinding.raisedBy as Reviewer, evidence: reason });
        }
      } else {
        const text = (v: unknown, fallback: string) => (typeof v === "string" && v.trim().length > 0 ? v.trim() : fallback);
        const importance =
          entry.importance === "high" || entry.importance === "medium" || entry.importance === "low"
            ? entry.importance
            : message.importance;
        events.push({
          type: "MESSAGE_PUBLISHED",
          ...binding,
          content: {
            type: messageType,
            title: clampMessageTitle(text(entry.title, message.title)),
            summary: text(entry.summary, message.summary),
            context: text(entry.context, message.context),
            evidence: Array.isArray(entry.evidence) && entry.evidence.length > 0 ? entry.evidence.map(String) : message.evidence,
            planRef: message.planRef,
            ...(importance ? { importance } : {}),
          },
        });
        if (sourceFinding) {
          // Plan 05e: the evaluator's own citation is tagged as evaluator
          // supplied (`evaluator: …`, round-3 review disc-A-38/M-10) and
          // APPENDED to whatever already validated the finding (the 3a record
          // or a 3b run), so no earlier validation evidence is lost.
          const supplied = typeof entry.verified === "string" && entry.verified.trim().length > 0 ? entry.verified.trim() : undefined;
          const addition = supplied ? (supplied.startsWith("evaluator:") ? supplied : `evaluator: ${supplied}`) : undefined;
          const combined = appendVerified(sourceFinding.verified, addition);
          if (combined && combined !== sourceFinding.verified) events.push({ type: "FINDING_VERIFIED", findingId: sourceFinding.id, verified: combined });
          // Plan 05e (5): a blocking finding may stay blocking only when it is
          // a defect against an acceptance item or a reserved rule; the
          // evaluator lowers anything else, recording the reason.
          if (
            sourceFinding.severity === "blocking" &&
            message.type === "finding" &&
            !message.raisedAsBlocker &&
            !findingCitesAcceptanceOrReserved(sourceFinding, this.#state.phase.contract, this.#directiveIds())
          ) {
            events.push({
              type: "FINDING_SEVERITY_CHANGED",
              findingId: sourceFinding.id,
              severity: "advisory",
              reason: "the finding cites no acceptance item or reserved rule, so it may not block",
              by: "evaluator",
            });
          }
        }
      }
    }
    return events;
  }

  /** Plan 01d: the records this dispatch's turn-2 prompt demanded a ballot
   * for that `review` gives no ballot. The demanded set is the prompt-time
   * snapshot on the handle, never a live recomputation, so a record that
   * appeared after the prompt (a late discovery) is never demanded. A
   * `discoveryMatches` entry that retires the reviewer's own discovery —
   * exactly the condition `#applyReviewFindingsAndBallots` uses to apply a
   * match — also covers its discovery, so a reviewer never has to ballot a
   * record it is matching away. */
  #missingDemandedBallots(review: Review, handle: AgentHandle): Array<{ id: string; choice: string }> {
    const demanded = handle.demandedBallots;
    if (!demanded || demanded.size === 0) return [];
    const covered = new Set<string>();
    for (const b of review.ballots ?? []) covered.add(b.decisionId);
    for (const m of review.discoveryMatches ?? []) {
      const discovery = this.#state.phase.decisions.find((d) => d.id === m.discoveryId);
      const target = this.#state.phase.decisions.find((d) => d.id === m.sameAs);
      if (
        discovery &&
        target &&
        discovery.id.includes(`-disc-${review.reviewer}-`) &&
        isLiveDecision(discovery) &&
        isLiveDecision(target) &&
        discovery.id !== target.id
      ) {
        covered.add(m.discoveryId);
      }
    }
    return [...demanded.entries()].filter(([id]) => !covered.has(id)).map(([id, choice]) => ({ id, choice }));
  }

  /** Phase-1 stub (design §5/§7.1's real ballot casting is phase 2 —
   * there is no `submit_ballot` tool yet; a reviewer's only phase-1 tools
   * are `submit_discovery` and `submit_review`, neither of which carries a
   * vote). Without this, a "delegated" decision's vote never passes (a
   * missing ballot counts as reject — `tally.ts`) no matter how many
   * attempts run, its `failed_vote` owner request never resolves itself,
   * and `accept()`'s "no owner request is open" clause then blocks
   * acceptance forever — a real bug this packet's own live-single-phase
   * run caught (see the README's phase-1c note). So every reviewer's own
   * `submit_review` also casts one approving ballot, on its own behalf,
   * for every currently undecided "delegated" decision bound to the
   * candidate/contract version it just reviewed — exactly as if a real
   * reviewer had reviewed it and voted to approve. `checkBallotBinding`
   * (via `#applyEvent`) still enforces the real binding rules; this only
   * supplies the vote itself, never the binding check. */
  #castStubBallots(reviewer: Reviewer): void {
    const candidate = this.#state.phase.candidate;
    if (!candidate) return;
    const K = this.#state.phase.contract.contractVersion;
    for (const decision of this.#state.phase.decisions) {
      if (decision.class !== "delegated" && decision.class !== "reserved") continue;
      if (decision.boundCandidateSha !== candidate.sha || !sameVersion(decision.boundContractVersion, K)) continue;
      if (this.#state.phase.ballots.some((b) => b.reviewer === reviewer && b.decisionId === decision.id && b.boundCandidateSha === candidate.sha)) {
        continue; // already voted on this decision for this candidate
      }
      const ballot: Ballot = {
        reviewer,
        decisionId: decision.id,
        vote: "approve",
        rationale:
          "Phase-1 stub reviewer: approves every delegated decision on the reviewed candidate — phase 1 has no real discovery/correction loop yet (see README's phase-1c note).",
        evidence: [`stub-reviewer-${reviewer}-auto-approve`],
        boundCandidateSha: candidate.sha,
        boundContractVersion: K,
        boundRecordVersion: decision.version,
      };
      this.#applyEvent({ type: "BALLOT_CAST", ballot });
    }
  }

  // -- work packet 2a: real reviewer discovery/ballots/findings -----------

  #reviewerFromAgentId(agentId: string): Reviewer {
    return (agentId.match(/^reviewer-([A-Za-z0-9_]+)-/)?.[1] as Reviewer | undefined) ?? this.#laneSeats()[0] ?? "M";
  }

  /** design §3.3's second decision source: a reviewer's turn-1
   * `submit_discovery`, made before it is shown the worker's own
   * disclosure. v1 matching (design §12 notes dedup precision is measured
   * later): every discovery becomes a *new* `reviewer-discovered` record —
   * no attach-to-existing-id matching yet. Assembles+binds each one exactly
   * like a worker's disclosure (id/version/boundCandidateSha/
   * boundContractVersion), validates it, and emits `DECISION_ADDED`. */
  #applyDiscoveries(
    discoveries: DecisionDisclosure[],
    reviewer: Reviewer,
    /** Plan 06g2: the number the FIRST of these discoveries takes. A lane
     * round passes the numbers its turn-2 prompt listed, so the ballots a seat
     * cast name exactly the records that appear here. */
    opts: { firstIndex?: number } = {},
  ): string | undefined {
    const candidate = this.#state.phase.candidate;
    if (!candidate) return "no candidate exists yet to bind a discovered decision to";
    if (this.#discoveryClosed()) {
      // Plan 2c no-unshown-ballots: the other reviewers are already voting on
      // the merged list, so a record added now could never get their ballots.
      this.#log.append("late_discovery", { reviewer, candidateSha: candidate.sha, discoveries });
      // Plan 06i (owner ruling): a late discovery must not be DROPPED (C3),
      // but it must not stall the phase either. It is recorded with an
      // `escalate` disposition and an open, NON-blocking owner request naming
      // it, so it is visible in the status and `tt summary` and the phase
      // still reaches DONE. It is not added to `decisions` (it could never
      // get the other seats' ballots).
      const K = this.#state.phase.contract.contractVersion;
      let n = this.#state.phase.decisions.length + 1;
      for (const d of discoveries) {
        const itemId = `D-${this.#state.phase.phaseId}-${candidate.sha.slice(0, 8)}-disc-${reviewer}-late-${n}`;
        n += 1;
        const ownerRequestId = `OR-${this.#state.phase.phaseId}-triage-${itemId}`;
        // Plan 06i (C2): the disposition is computed by disposition() (the
        // sole decider); the conductor only records it and opens the request
        // it names.
        const record: TriageRecord = {
          itemId,
          source: "decision",
          impact: "judgement",
          disposition: disposition(
            { itemId, source: "decision", impact: "judgement" },
            {
              lateDiscovery: true,
              ownerRequestId,
              reason: `the discovery "${d.choice}" arrived after the review window; it is recorded and offered to the owner, never dropped`,
            },
          ),
        };
        this.#applyEvent({ type: "TRIAGE_RECORDED", record });
        this.#applyEvent({
          type: "OWNER_REQUEST_OPENED",
          request: {
            id: ownerRequestId,
            version: 1,
            phaseId: this.#state.phase.phaseId,
            reason: `a discovered decision arrived after the review window: ${d.choice}`,
            origin: "failed_vote",
            linkedDecisionId: itemId,
            boundCandidateSha: candidate.sha,
            boundContractVersion: K,
            options: FAILED_VOTE_OPTIONS,
            status: "open",
            blocking: false,
          },
        });
      }
      return undefined;
    }
    const K = this.#state.phase.contract.contractVersion;
    let index = opts.firstIndex ?? this.#state.phase.decisions.length + 1;
    for (const d of discoveries) {
      const decision: Decision = {
        id: `D-${this.#state.phase.phaseId}-${candidate.sha.slice(0, 8)}-disc-${reviewer}-${index}`,
        version: 1,
        phaseId: this.#state.phase.phaseId,
        source: "reviewer-discovered",
        class: d.classProposal,
        choice: d.choice,
        whyItMatters: d.whyItMatters,
        alternatives: d.alternatives,
        recommendation: d.recommendation,
        boundCandidateSha: candidate.sha,
        boundContractVersion: K,
      };
      const result = validate(DECISION_SCHEMA, decision);
      if (!result.valid) {
        return `discovered decision fails schemas/decision.schema.json: ${result.errors.join("; ")}`;
      }
      index += 1;
      this.#applyEvent({ type: "DECISION_ADDED", decision });
      // Plan 04a item 2: a reviewer's discovered trade-off is raised as a raw
      // message too, so the evaluator checks it in EVALUATING like any other
      // (finding M-9). Deduped by the decision id.
      this.#raiseMessage("tradeoff", decision.id, this.#decisionContent(decision), candidate.sha);
    }
    return undefined;
  }

  /** design §4.1: assembles+binds one reviewer-raised finding
   * (id/version/boundCandidateSha; kind/severity/evidence/linkedDecisionId
   * copied through), runs its optional reproduction command (design §8.1's
   * `reproductionMs` deadline, in a fresh disposable checkout — never the
   * live worktree or the reviewer's own read-only candidate checkout) if
   * one was given, and emits `FINDING_RAISED`. Returns an error string
   * instead of throwing, like `#applyDiscoveries`. */
  /** Plan 05e: the ids of the owner directives in force, which a blocking
   * finding may cite as its ground (`cite the directive id`). */
  /** Plan 06g (A6): what `blocksAcceptance` may look at — the contract's item
   * ids (R, C and A; for an old-format phase the synthesized R1..Rn), every
   * owner directive in force and every correction's id, plus the tests that
   * passed at the round's base and fail now. Nothing here is model-supplied. */
  #acceptanceGate(): AcceptanceGate {
    const contract = this.#state.phase.contract;
    const itemIds = [
      ...flatItems(itemsFromPhase(contract)).map((i) => i.id),
      ...this.#directiveIds(),
      ...this.#state.phase.corrections.map((c) => c.id),
    ].filter((id) => typeof id === "string" && id.length > 0);
    return { itemIds, regressions: this.#regressionTests(), triage: blockingTriageRecords(this.#state.phase) };
  }

  /** Plan 06g (A6b): the tests that failed in this candidate's checks and
   * passed at the round's base — the conductor's own regression split (a
   * load-only flake is not one: it passed alone). */
  #regressionTests(): string[] {
    const phase = this.#state.phase;
    const current = phase.checks;
    const failures = current?.failures ?? phase.lastCheckFailures?.failures ?? [];
    if (failures.length === 0) return [];
    // The round's base: the phase base in round 1, the previous candidate in
    // a repair (recorded at the freeze). This is the ONLY correct base — not
    // `#baselineFailedCommands()`, which is the phase base for every round.
    const base = new Set(this.#roundBaseFailures());
    // A test the base already failed is not a regression; a load-only failure
    // passed when re-run alone, so it is not one either.
    return failures.filter((f) => !f.loadOnly && !base.has(f.name)).map((f) => f.name);
  }

  /** Plan 06g (A4): the tests that failed at this round's base — the phase
   * base's failures in round 1, the previous candidate's check failures in a
   * repair. Recorded by `freezeCompleted` (`roundBaseFailures`), so a test
   * that PASSED at round 1's candidate and fails in round 2 is a regression,
   * while a test the base already failed is a persisting failure. */
  #roundBaseFailures(): string[] {
    return this.#state.phase.roundBaseFailures ?? this.#baselineFailedCommands().flatMap((c) => c.failures ?? []);
  }

  #directiveIds(): string[] {
    return (this.#state.phase.ownerDirectives ?? []).map((d) => d.id).filter((id) => typeof id === "string" && id.length > 0);
  }

  /** Plan 05e (3a): the check, probe and gate records this candidate's run
   * already holds, as `{command, exitCode, output}` per command. A log's
   * first line is `$ <command>`, its last `exit <n> signal <s>`. Read-only;
   * an absent directory is no records. */
  #checkRecordsFor(candidateSha: string): Array<{ command: string; exitCode: number | null; output: string }> {
    // The candidate's own check and gate logs, plus the probe's own records
    // for the integration it probed onto (recorded under a probe-specific
    // directory), so 3a compares against every record the run holds for the
    // candidate (plan 05e, disc-A-17).
    const dirs = [path.join(this.#paths.checks, candidateSha)];
    const probe = this.#state.phase.probe;
    if (probe && probe.candidateSha === candidateSha && typeof probe.probedI === "string" && probe.probedI.length > 0) {
      dirs.push(path.join(this.#paths.checks, "probe", probe.probedI));
    }
    const out: Array<{ command: string; exitCode: number | null; output: string }> = [];
    for (const dir of dirs) {
      let names: string[];
      try {
        names = fs.readdirSync(dir).filter((n) => n.endsWith(".log"));
      } catch {
        continue;
      }
      for (const name of names) {
        try {
          const lines = fs.readFileSync(path.join(dir, name), "utf8").split("\n");
          const first = lines[0] ?? "";
          if (!first.startsWith("$ ")) continue;
          const command = first.slice(2).trim();
          const last = [...lines].reverse().find((l) => l.startsWith("exit ")) ?? "";
          const m = last.match(/^exit (\d+)/);
          out.push({ command, exitCode: m ? Number(m[1]) : null, output: lines.slice(1).join("\n") });
        } catch {
          // best effort: a corrupt record proves nothing
        }
      }
    }
    return out;
  }

  /** Plan 05e (3a): compare a finding's own words with the candidate's check
   * record, before any agent sees it. A claim that names a check command or
   * a test the record lists as failing is `confirmed`; a claim that a named
   * command fails while the record shows it passing is `rejected` with the
   * record cited.
   *
   * Only a sentence that both names the command AND carries a failure word is
   * read as a claim about it, so a finding that merely mentions a passing
   * check ("make check passes but does not cover it") is not auto-dropped
   * (round-2 reviews A-6, M-12, B-21). */
  #claimAgainstCheckRecords(
    evidence: string,
    candidateSha: string,
  ): { kind: "rejected"; reason: string } | { kind: "confirmed"; reason: string } | undefined {
    const sentences = evidence
      .split(/[.;\n]+/)
      .map((s) => s.toLowerCase())
      .filter((s) => s.trim().length > 0);
    const failureWord = /(fail|fails|failed|failing|failure|error|errors|broken|crash|crashes|crashed|does not pass|doesn't pass|not pass|red|non-zero|nonzero|times out|timed out|timeout|regression|invalid)/;
    for (const record of this.#checkRecordsFor(candidateSha)) {
      const command = record.command.trim();
      if (command.length >= 5) {
        const needle = command.toLowerCase();
        const claiming = sentences.find((s) => s.includes(needle) && failureWord.test(s));
        if (claiming) {
          if (record.exitCode === 0) {
            return {
              kind: "rejected",
              reason: `check record for ${candidateSha.slice(0, 9)}: \`${command}\` exit 0 (passed) — the claim that it fails is contradicted by the record`,
            };
          }
          if (record.exitCode !== null) {
            return { kind: "confirmed", reason: `record (confirmed by record: \`${command}\` exit ${record.exitCode})` };
          }
        }
      }
      for (const name of parseTestFailures(record.output)) {
        if (name.length < 4) continue;
        const re = new RegExp(`(^|[^a-z0-9_])${escapeRegExp(name.toLowerCase())}([^a-z0-9_]|$)`);
        if (re.test(evidence.toLowerCase())) {
          return { kind: "confirmed", reason: `record (confirmed by record: \`${command}\` lists failing test ${name})` };
        }
      }
    }
    return undefined;
  }

  /** Plan 05e (3b): run the finding's own runnable test or command in a fresh
   * disposable checkout of the candidate, bounded by the check deadline. */
  async #runRunnable(command: string, candidateSha: string): Promise<{ exitCode: number | null; timedOut: boolean; tail: string }> {
    const checkout = disposableCheckout(this.#plan.repo, candidateSha);
    try {
      const running = runCommand({
        command,
        cwd: checkout.dir,
        deadlineMs: this.#deadlines.checkMs,
        termGraceMs: this.#deadlines.termGraceMs,
      });
      const result = await running.result;
      return { exitCode: result.exitCode, timedOut: result.timedOut, tail: result.output.slice(-2000) };
    } finally {
      checkout.dispose();
    }
  }

  /** Plan 05e (3a/3b): record a finding whose claim the record (or its own
   * passing run) already contradicted — as an immediately disproved finding
   * and a dropped message, so no evaluator or panel ever sees it. */
  #rejectFinding(
    fd: FindingDisclosure,
    reviewer: Reviewer,
    candidateSha: string,
    reason: string,
    opts: { raisedAsBlocker?: boolean } = {},
  ): void {
    const id = `F-${this.#state.phase.phaseId}-${candidateSha.slice(0, 8)}-${reviewer}-${this.#state.phase.findings.length + 1}`;
    const finding: Finding = {
      id,
      version: 1,
      phaseId: this.#state.phase.phaseId,
      kind: fd.kind,
      severity: fd.severity,
      evidence: fd.evidence,
      raisedBy: reviewer,
      status: "disproved",
      boundCandidateSha: candidateSha,
      disprovedEvidence: reason,
    };
    const result = validate(FINDING_SCHEMA, finding);
    if (result.valid) {
      this.#applyEvent({ type: "FINDING_RAISED", finding: { ...finding, status: "open" } });
      this.#applyEvent({ type: "FINDING_DISPROVED", findingId: id, byReviewer: reviewer, evidence: reason });
    }
    const messageType: MessageType = opts.raisedAsBlocker ? "blocker" : "finding";
    const messageId = this.#raiseMessage(messageType, id, this.#findingContent({ ...finding, status: "open" }, messageType), candidateSha, opts.raisedAsBlocker ? { raisedAsBlocker: true } : {});
    const message = (this.#state.phase.messages ?? []).find((m) => m.id === messageId)!;
    this.#applyEvent({
      type: "MESSAGE_DROPPED",
      messageId,
      by: "evaluator",
      reason,
      boundCandidateSha: message.boundCandidateSha,
      boundContractVersion: message.boundContractVersion,
      boundRecordVersion: message.messageVersion,
    });
    this.#log.append("finding_rejected_by_record", { reviewer, findingId: id, reason });
  }

  async #raiseFinding(
    fd: FindingDisclosure,
    reviewer: Reviewer,
    candidateSha: string,
    opts: { raisedAsBlocker?: boolean } = {},
  ): Promise<string | undefined> {
    // Plan 05e (finding #34): a new point on bytes the reviewers already
    // approved is an advisory finding, not a blocker, unless it violates an
    // acceptance item or a reserved rule. That applies to a point filed
    // through the `blockers` list too: it is raised as an ordinary finding
    // message, so no blocker panel runs on unchanged approved code
    // (round-4 review A-16).
    let severity = fd.severity;
    let asBlocker = opts.raisedAsBlocker === true;
    const approvedSha = this.#amendmentOnlyApprovedSha(candidateSha);
    const citesGround =
      approvedSha !== undefined &&
      findingCitesAcceptanceOrReserved(
        { kind: fd.kind, evidence: fd.evidence, criterionDisputed: fd.criterionDispute?.criterion } as Finding,
        this.#state.phase.contract,
        this.#directiveIds(),
      );
    if (approvedSha && !citesGround) {
      this.#log.append("amendment_only_downgrade", { reviewer, approvedCandidateSha: approvedSha, candidateSha, raisedAsBlocker: asBlocker });
      if (asBlocker) asBlocker = false;
      if (severity === "blocking") severity = "advisory";
    }
    // Plan 06g (A6, owner directive ODP-1): from round 2 a blocking finding
    // blocks only when it names an unmet item of the contract (or of an owner
    // correction) with its id and a file:line, or is a regression — a test or
    // behaviour that passed at the round's base and fails now. Anything else
    // (a further edge path, hardening, wording, style) is an advisory:
    // recorded as a carried item, never blocking. `blocksAcceptance` is the
    // only place that decides this, for one lane or two.
    const round = this.#state.phase.round ?? 1;
    let roundDowngrade: string | undefined;
    if (severity === "blocking" && round >= 2) {
      const ground: FindingGround = {
        severity,
        evidence: fd.evidence,
        ...(fd.criterionDispute?.criterion ? { criterionDisputed: fd.criterionDispute.criterion } : {}),
      };
      const gate = this.#acceptanceGate();
      if (!blocksAcceptance(ground, round, gate)) {
        roundDowngrade = `A6 round ${round}: ${advisoryReason(ground, round, gate)}`;
        this.#log.append("round_advisory", {
          reviewer,
          candidateSha,
          round,
          reason: roundDowngrade,
          raisedAsBlocker: asBlocker,
        });
        if (asBlocker) asBlocker = false;
        severity = "advisory";
      }
    }
    // Plan 05e (3a/3b): every finding — including one raised through a
    // reviewer's `blockers` list, which is a blocking finding too — goes
    // through the record comparison and the runnable re-run before any agent
    // sees it (round-3 reviews disc-A-36, M-9).
    let verified: string | undefined;
    {
      const claim = this.#claimAgainstCheckRecords(fd.evidence, candidateSha);
      if (claim?.kind === "rejected") {
        this.#rejectFinding(fd, reviewer, candidateSha, claim.reason, { raisedAsBlocker: asBlocker });
        return undefined;
      }
      if (claim?.kind === "confirmed") verified = claim.reason;
      if (fd.runnable && fd.runnable.trim().length > 0) {
        const run = await this.#runRunnable(fd.runnable.trim(), candidateSha);
        this.#log.append("finding_run", { reviewer, command: fd.runnable.trim(), exitCode: run.exitCode, timedOut: run.timedOut, tail: run.tail });
        // Plan 05e (3b): the finding is published only if the run reproduces
        // it. Exit 0 did not reproduce it; a timeout is inconclusive and must
        // not count as a reproduced failure either (round-2 reviews A-7,
        // M-3, B-22).
        if (run.timedOut || run.exitCode === 0) {
          const why = run.timedOut ? "timed out" : "exit 0";
          this.#rejectFinding(fd, reviewer, candidateSha, `run \`${fd.runnable.trim()}\` ${why} — the claimed failure did not reproduce`, { raisedAsBlocker: asBlocker });
          return undefined;
        }
        verified = `run \`${fd.runnable.trim()}\` exit ${run.exitCode}`;
      }
    }
    let reproduction: Finding["reproduction"];
    if (fd.reproduction) {
      const result = await this.#runReproduction(fd.reproduction.command, candidateSha);
      reproduction = { command: fd.reproduction.command, result };
    }
    const finding: Finding = {
      id: `F-${this.#state.phase.phaseId}-${candidateSha.slice(0, 8)}-${reviewer}-${this.#state.phase.findings.length + 1}`,
      version: 1,
      phaseId: this.#state.phase.phaseId,
      kind: fd.kind,
      severity,
      evidence: fd.evidence,
      raisedBy: reviewer,
      status: "open",
      boundCandidateSha: candidateSha,
      ...(verified !== undefined ? { verified } : {}),
      // Optional fields are omitted entirely rather than set to
      // `undefined` — schema.ts's minimal validator treats a PRESENT key
      // whose value is `undefined` as "wrong type", not "absent" (a real
      // bug this test caught: M's finding with no linkedDecisionId failed
      // validation with "expected type string, got undefined").
      ...(fd.linkedDecisionId !== undefined ? { linkedDecisionId: fd.linkedDecisionId } : {}),
      ...(fd.criterionDispute?.criterion && fd.criterionDispute.criterion.trim().length > 0
        ? { criterionDisputed: fd.criterionDispute.criterion }
        : {}),
      ...(reproduction !== undefined ? { reproduction } : {}),
      // Plan 06g (A6): why this finding is advisory although it was raised
      // blocking — the round rule's own sentence, so the record and the views
      // never disagree about it.
      ...(roundDowngrade !== undefined ? { severityReason: roundDowngrade } : {}),
    };
    const result = validate(FINDING_SCHEMA, finding);
    if (!result.valid) return `raised finding fails schemas/finding.schema.json: ${result.errors.join("; ")}`;
    this.#applyEvent({ type: "FINDING_RAISED", finding });
    // Contract v1 §1: a finding is a published message too. Plan 04b: only
    // one raised through a reviewer's `blockers` list is a BLOCKER message
    // (the stop-the-work type, voted by a panel). An ordinary `blocking`
    // finding keeps its pre-04b meaning — it blocks acceptance and forces a
    // repair — and stays a `finding` message, listed under Findings marked
    // `blocking', never as a Blocker (plan 05c).
    const messageType: MessageType = asBlocker ? "blocker" : "finding";
    this.#raiseMessage(
      messageType,
      finding.id,
      this.#findingContent(finding, messageType),
      candidateSha,
      asBlocker ? { raisedAsBlocker: true } : {},
    );
    // Plan 01g: a reviewer's finding may say the criterion cannot be met as
    // written. That is recorded as an amendment the reviewers vote on later;
    // a criterion the contract does not carry is ignored (the finding itself
    // still stands).
    if (fd.criterionDispute?.criterion && fd.criterionDispute.why && fd.criterionDispute.proposedWording) {
      if (this.#state.phase.contract.acceptance.includes(fd.criterionDispute.criterion)) {
        this.#addAmendmentDecision(fd.criterionDispute, reviewer, candidateSha);
      } else {
        this.#log.append("dispute_ignored", {
          reviewer,
          criterion: fd.criterionDispute.criterion,
          reason: "not one of this phase's acceptance items verbatim",
        });
      }
    }
    return undefined;
  }

  /** Plan 06b (OD-1 R6): the requirement item id whose text is the disputed
   * criterion, or undefined on an old-format phase. */
  #criterionItemId(criterion: string): string | undefined {
    return this.#state.phase.contract.requirements?.find((r) => r.text === criterion || r.title === criterion)?.id;
  }

  /** Plan 01g: assembles a reviewer-raised `criterionDispute` into an
   * amendment record (a `reserved` decision) bound to the candidate/contract
   * being reviewed, exactly like a worker disclosure. It is votable like any
   * other reserved decision; if it passes, next() applies it. A reviewer
   * raises one in turn 2, after this round's ballot demand was captured, so
   * it is voted in a later round (carryDecisionsForward keeps amendments). */
  #addAmendmentDecision(dispute: CriterionDispute, raisedBy: Reviewer, candidateSha: string): void {
    // Plan 06c (R7): an amendment whose proposed text equals the current
    // criterion changes nothing and is never raised or shown.
    if (dispute.proposedWording.trim() === dispute.criterion.trim()) {
      this.#log.append("dispute_ignored", {
        reviewer: raisedBy,
        criterion: dispute.criterion,
        reason: "the proposed wording equals the current criterion",
      });
      return;
    }
    const K = this.#state.phase.contract.contractVersion;
    const short = candidateSha.slice(0, 8);
    const n = this.#state.phase.decisions.length + 1;
    const decision: Decision = {
      id: `D-${this.#state.phase.phaseId}-${short}-amendment-${raisedBy}-${n}`,
      version: 1,
      phaseId: this.#state.phase.phaseId,
      source: "reviewer-discovered",
      class: "reserved",
      choice: dispute.proposedWording,
      whyItMatters: dispute.why,
      alternatives: [
        { option: dispute.criterion, consequence: "the letter of this criterion stays in force and no candidate can satisfy it" },
      ],
      recommendation: { choice: dispute.proposedWording, reason: dispute.why },
      boundCandidateSha: candidateSha,
      boundContractVersion: K,
      amendment: {
        id: `AM-${this.#state.phase.phaseId}-${short}-${raisedBy}-${n}`,
        criterion: dispute.criterion,
        proposedWording: dispute.proposedWording,
        ...(this.#criterionItemId(dispute.criterion) ? { itemId: this.#criterionItemId(dispute.criterion)! } : {}),
        why: dispute.why,
        raisedBy,
        status: "proposed",
        previousContractVersion: K,
      },
    };
    // Dedup only an IDENTICAL proposal: a different wording for the same
    // criterion is a genuinely different choice, and the conductor's own
    // amendment applies/supersedes siblings. A duplicate is logged rather
    // than silently dropped (A-1, A-13).
    if (
      this.#state.phase.decisions.some(
        (d) =>
          d.amendment?.status === "proposed" &&
          d.amendment.criterion === dispute.criterion &&
          d.amendment.proposedWording === dispute.proposedWording,
      )
    ) {
      this.#log.append("dispute_ignored", {
        reviewer: raisedBy,
        criterion: dispute.criterion,
        reason: "an identical amendment for this criterion is already proposed",
      });
      return;
    }
    const valid = validate(DECISION_SCHEMA, decision);
    if (!valid.valid) {
      this.#log.append("error", { where: "reviewer_amendment", error: valid.errors.join("; ") });
      return;
    }
    this.#applyEvent({ type: "DECISION_ADDED", decision });
  }

  /** design §8.1's reproduction-command deadline: runs `command` in a fresh
   * disposable checkout of `candidateSha` (never the reviewer's own
   * read-only checkout — a reproduction command might itself be
   * destructive), bounded by `reproductionMs`. Exit 0 = the described
   * behavior reproduced; a non-zero, non-timeout exit = it did not; a
   * timeout is inconclusive (killed, like every other conductor-run
   * command, via its own process group). */
  async #runReproduction(command: string, candidateSha: string): Promise<"reproduced" | "not_reproduced" | "inconclusive"> {
    const checkout = disposableCheckout(this.#plan.repo, candidateSha);
    try {
      const running = runCommand({
        command,
        cwd: checkout.dir,
        deadlineMs: this.#deadlines.reproductionMs,
        termGraceMs: this.#deadlines.termGraceMs,
      });
      const result = await running.result;
      if (result.timedOut) return "inconclusive";
      return result.exitCode === 0 ? "reproduced" : "not_reproduced";
    } finally {
      checkout.dispose();
    }
  }

  /** design §5/§6.1's real turn-2 review outcome: a ballot per votable
   * decision plus any newly raised findings, replacing phase 1's
   * `#castStubBallots`. Findings are raised before ballots are cast so a
   * ballot's own `contractObjection` (handled by reduce.ts's BALLOT_CAST
   * case — it opens its own linked finding and bumps the decision's
   * version) always sees the decision's latest version. Returns an error
   * string (never throws) so `#onSubmit` can reject the submission back to
   * the model as an ordinary tool error. */
  async #applyReviewFindingsAndBallots(review: Review): Promise<string | undefined> {
    const candidate = this.#state.phase.candidate;
    if (!candidate) return "no candidate exists yet to bind ballots/findings to";
    const K = this.#state.phase.contract.contractVersion;

    // Plan 2c: a reviewer's own discovery that repeats another record is
    // matched to it, before ballots, so the duplicate never needs a vote.
    for (const m of review.discoveryMatches ?? []) {
      const discovery = this.#state.phase.decisions.find((d) => d.id === m.discoveryId);
      const target = this.#state.phase.decisions.find((d) => d.id === m.sameAs);
      if (
        !discovery ||
        !target ||
        !discovery.id.includes(`-disc-${review.reviewer}-`) ||
        !isLiveDecision(discovery) ||
        !isLiveDecision(target) ||
        discovery.id === target.id
      ) {
        this.#log.append("match_ignored", { reviewer: review.reviewer, ...m });
        continue;
      }
      this.#applyEvent({ type: "DECISION_MATCHED", decisionId: discovery.id, sameAs: target.id, reviewer: review.reviewer });
    }

    // Plan 04b: a reviewer's separate `blockers` list is raised exactly like
    // a finding, at `blocking` severity (a blocker has no other severity).
    // From that moment it is two things: a raw `blocker` message (marked
    // `raisedAsBlocker`, so a panel votes on it) and an open blocking finding
    // — effective at once against acceptance.
    //
    // A blocker is NEVER deduped (round-3 review, findings B-1/A-3/M-4):
    // folding it into an existing finding via `sameAs` could drop the
    // stop-the-work request entirely — no blocker message, no panel, and no
    // blocking force at all when the target finding was advisory. It is
    // always raised and always paneled; a stray `sameAs` (the submission
    // schema does not offer one on a blocker) is logged, never honoured.
    const raised: Array<{ fd: FindingDisclosure; asBlocker: boolean }> = [
      ...(review.findings ?? []).map((fd) => ({ fd, asBlocker: false })),
      ...(review.blockers ?? []).map((b) => ({
        fd: { ...b, severity: "blocking" as const } as FindingDisclosure,
        asBlocker: true,
      })),
    ];
    for (const { fd, asBlocker } of raised) {
      // Plan 2c: "same as F-…" records agreement instead of a duplicate —
      // for an ordinary FINDING only; a blocker is immune (see above).
      if (fd.sameAs) {
        if (asBlocker) {
          this.#log.append("blocker_sameas_ignored", {
            reviewer: review.reviewer,
            sameAs: fd.sameAs,
            reason: "a blocker is always raised and paneled, never folded into an existing finding",
          });
        } else {
          const existing = this.#state.phase.findings.find((f) => f.id === fd.sameAs && f.status === "open");
          if (existing) {
            this.#applyEvent({ type: "FINDING_ALSO_RAISED", findingId: existing.id, reviewer: review.reviewer });
            // Plan 05e (5, finding #32): a `sameAs` re-raise takes the
            // re-raiser's severity DOWNWARD — a narrower advisory re-raise of
            // a fixed blocking finding no longer keeps it blocking. It never
            // raises one: a single reviewer making an advisory finding
            // blocking would block the worker on one agent's word (round-2
            // reviews A-5, M-2).
            if (fd.severity === "advisory" && existing.severity === "blocking") {
              this.#applyEvent({
                type: "FINDING_SEVERITY_CHANGED",
                findingId: existing.id,
                severity: "advisory",
                reason: `re-raised by ${review.reviewer} at advisory severity`,
                by: "reviewer",
              });
            } else if (fd.severity === "blocking" && existing.severity === "advisory") {
              this.#log.append("sameas_severity_raise_refused", {
                reviewer: review.reviewer,
                findingId: existing.id,
                reason: "a sameAs re-raise may lower a finding's severity, never raise it",
              });
            }
            continue;
          }
        }
      }
      const { sameAs, ...disclosure } = fd;
      const before = new Set((this.#state.phase.messages ?? []).map((m) => m.id));
      const error = await this.#raiseFinding(disclosure, review.reviewer, candidate.sha, { raisedAsBlocker: asBlocker });
      if (error) return error;
      if (!this.#reviewStillCurrent(candidate.sha)) return STALE_REVIEW;
      // Plan 05j: a reviewer may name an entry with `sameAs E-n` to link the
      // raise to it at raise time. The runtime accepts the link only if the
      // message and the entry share an anchor; otherwise it is refused and
      // logged (the message keeps the entry the round pass opened for it).
      if (typeof sameAs === "string" && sameAs.startsWith("E-")) {
        const newMessage = (this.#state.phase.messages ?? []).find((m) => !before.has(m.id));
        if (newMessage) this.#linkReviewerRaise(newMessage.id, sameAs, review.reviewer);
      }
    }

    for (const bd of review.ballots ?? []) {
      const decision = this.#state.phase.decisions.find((d) => d.id === bd.decisionId);
      // Plan 2c: a ballot on a record that is not votable (unknown,
      // superseded, or not 'delegated') is skipped and logged rather than
      // failing the whole review, which would cost the reviewer a resubmit.
      if (!decision || !isLiveDecision(decision) || (decision.class !== "delegated" && decision.class !== "reserved")) {
        this.#log.append("ballot_ignored", {
          reviewer: review.reviewer,
          decisionId: bd.decisionId,
          reason: !decision ? "unknown decision" : !isLiveDecision(decision) ? `superseded: ${decision.supersededBy ?? decision.supersededByCorrection}` : `class ${decision.class}`,
        });
        continue;
      }
      const ballot: Ballot = {
        reviewer: review.reviewer,
        decisionId: bd.decisionId,
        vote: bd.vote,
        rationale: bd.rationale,
        evidence: bd.evidence,
        contractObjection: bd.contractObjection,
        boundCandidateSha: candidate.sha,
        boundContractVersion: K,
        boundRecordVersion: decision.version,
      };
      this.#applyEvent({ type: "BALLOT_CAST", ballot });
    }
    return undefined;
  }

  /** design §4.2/§6.1: "per-finding confirm/withdraw statements for its own
   * raised findings" — only the raising reviewer's own statement on its own
   * open finding is ever acted on; everything else is silently ignored
   * rather than rejected outright (a stale findingId, or a statement about
   * someone else's finding, is not this reviewer's own review to fail
   * over). `confirm` needs a NEW candidate with passing checks (design
   * §4.2's own wording, mirrored by reduce.ts's own
   * FINDING_CONFIRMED_REPAIRED guard) — checked here too so a premature
   * "confirm" (same candidate, or checks not yet passed) is a quiet no-op
   * instead of a thrown, unrecoverable #applyEvent rejection. MUST be
   * called after `REVIEW_SUBMITTED` for this review — reduce.ts's own
   * FINDING_CONFIRMED_REPAIRED guard reads `phase.reviews[reviewer].review`
   * for the confirming statement (see #onSubmit's own ordering comment). */
  #applyReviewFindingStatements(review: Review): void {
    const candidate = this.#state.phase.candidate;
    if (!candidate) return;
    for (const stmt of review.findingStatements ?? []) {
      const finding = this.#state.phase.findings.find((f) => f.id === stmt.findingId);
      if (!finding || finding.raisedBy !== review.reviewer || finding.status !== "open") continue;
      if (stmt.status === "withdraw") {
        this.#applyEvent({
          type: "FINDING_DISPROVED",
          findingId: finding.id,
          byReviewer: review.reviewer,
          evidence: stmt.evidence && stmt.evidence.trim().length > 0 ? stmt.evidence : "reviewer withdrew the finding",
        });
      } else if (stmt.status === "confirm") {
        const checksOk = this.#state.phase.checks?.candidateSha === candidate.sha && this.#state.phase.checks?.passed === true;
        if (candidate.sha !== finding.boundCandidateSha && checksOk) {
          this.#applyEvent({ type: "FINDING_CONFIRMED_REPAIRED", findingId: finding.id, byReviewer: review.reviewer, candidateSha: candidate.sha });
        }
      }
    }
  }

  /** Plan 05e: the three reviews of the current candidate, with `review`
   * standing in for the one being submitted (it is not recorded yet when
   * this runs). Undefined until all three are present. */
  #threeReviewsWith(review: Review): Review[] | undefined {
    const list: Review[] = [];
    for (const who of seatsOf(this.#state.phase.contract)) {
      const r = who === review.reviewer ? review : this.#state.phase.reviews[who]?.review;
      if (!r || r.candidateSha !== review.candidateSha) return undefined;
      list.push(r);
    }
    return list;
  }

  /** Plan 05e: count the round's resolution ballots. For every earlier-round
   * finding/blocker message any reviewer marked, a 2-of-3 `resolved` majority
   * moves it to `resolved` (it leaves the owner's live view); a 2-of-3 `open`
   * majority (or no majority) leaves it live. */
  #applyRoundResolutions(review: Review): void {
    const phase = this.#state.phase;
    const C = phase.candidate?.sha;
    if (!C) return;
    const reviews = this.#threeReviewsWith(review);
    if (!reviews) return;
    const ids = new Set<string>();
    for (const r of reviews) for (const s of r.resolutionStatements ?? []) ids.add(s.messageId);
    for (const id of ids) {
      const message = (phase.messages ?? []).find((m) => m.id === id);
      if (!message || (message.state !== "published" && message.state !== "refused")) continue;
      let resolved = 0;
      for (const r of reviews) {
        const s = (r.resolutionStatements ?? []).find((x) => x.messageId === id);
        if (s?.status === "resolved") resolved += 1;
      }
      if (resolved < panelMajority(reviews.length)) continue;
      const evidence = reviews
        .flatMap((r) => (r.resolutionStatements ?? []).filter((s) => s.messageId === id && s.status === "resolved"))
        .map((s) => s.evidence)
        .filter((e): e is string => typeof e === "string" && e.length > 0)
        .join("; ");
      this.#applyEvent({
        type: "MESSAGE_RESOLVED",
        messageId: id,
        by: "vote",
        reason: evidence.length > 0 ? `resolved by a reviewer majority: ${evidence}` : "resolved by a reviewer majority",
        boundCandidateSha: message.boundCandidateSha,
        boundContractVersion: message.boundContractVersion,
        boundRecordVersion: message.messageVersion,
      });
      // The finding a resolved finding/blocker message came from is repaired
      // on this candidate, so it no longer blocks acceptance.
      const sourceFinding = message.sourceRecordId ? phase.findings.find((f) => f.id === message.sourceRecordId) : undefined;
      if (sourceFinding && sourceFinding.status === "open" && C) {
        this.#applyEvent({ type: "FINDING_RESOLVED_BY_VOTE", findingId: sourceFinding.id, candidateSha: C });
      }
    }
  }

  /** Plan 05e (finding #34): record the candidate as approved once all three
   * reviewers have reviewed it, every live decision bound to it has settled,
   * no owner request is open and no open blocking finding stands against it
   * (round-2 reviews A-4, disc-A-19, B-23). It runs when the round's
   * EVALUATING has settled, so the round panel's and evaluator's severity
   * decisions are already final. The tree object id is what a later
   * amendment-only resubmission is compared against. */
  #recordCandidateApproval(): void {
    const phase = this.#state.phase;
    const C = phase.candidate?.sha;
    if (!C) return;
    const K = phase.contract.contractVersion;
    if (!reviewsComplete(phase, C, K)) return;
    if ((phase.approvedCandidates ?? []).some((a) => a.candidateSha === C)) return;
    // Plan 06i (A2): approval follows the item's disposition, like accept().
    // A fix disposition blocks; a trade-off does not; an untriaged blocking
    // finding still blocks (fail safe).
    if (hasBlockingFix(phase) || undispositionedBlockingFinding(phase)) return;
    if (phase.ownerRequests.some((r) => r.status === "open" && r.blocking !== false && !ownerRequestIsStale(phase, r))) return;
    for (const decision of phase.decisions) {
      if (!isLiveDecision(decision)) continue;
      if (decision.amendment) continue;
      if (decision.boundCandidateSha !== C) continue;
      if (!decisionSettled(decision, phase, C, K)) return;
    }
    const tree = candidateTree(this.#plan.repo, C);
    if (!tree) return;
    this.#applyEvent({ type: "CANDIDATE_APPROVED", candidateSha: C, tree });
  }

  /** Plan 05e: the approved candidate whose shipped bytes are identical to
   * `sha`'s, if any (an amendment-only resubmission), else undefined. */
  #amendmentOnlyApprovedSha(sha: string): string | undefined {
    const approved = this.#state.phase.approvedCandidates ?? [];
    if (approved.length === 0) return undefined;
    const tree = candidateTree(this.#plan.repo, sha);
    if (!tree) return undefined;
    return approved.find((a) => a.tree === tree && a.candidateSha !== sha)?.candidateSha;
  }

  // -- work packet 2a: boundary triggers + §3.5 sampling data --------------

  /** design §3.3's boundary triggers and §3.5's sampling, computed once a
   * candidate exists (freeze time) from the diff between the phase's base
   * (its `integrationHead` when the attempt started) and the candidate.
   * Pure matching/citation logic lives in `core/boundaries.ts`; this method
   * is only the imperative shell (git diff, assembling+binding trigger
   * Decision records, logging the sample). Never throws on a bad diff —
   * best-effort, since a run should not fail over sampling data. */
  #recordBoundaryDataAndSample(candidateSha: string): void {
    let paths: string[];
    let hunks: ReturnType<typeof diffHunks>;
    try {
      paths = diffNameOnly(this.#plan.repo, this.#state.phase.integrationHead, candidateSha);
      hunks = diffHunks(this.#plan.repo, this.#state.phase.integrationHead, candidateSha);
    } catch (err) {
      this.#log.append("sampling_error", { candidateSha, error: String((err as Error)?.message ?? err) });
      return;
    }

    const contract = this.#state.phase.contract;
    const acceptanceFiles = contract.acceptance.filter((a) => a.includes("/"));
    const citationTexts = [
      ...this.#state.phase.decisions.flatMap((d) => [d.choice, d.whyItMatters, ...d.alternatives.map((a) => `${a.option} ${a.consequence}`)]),
      ...this.#state.phase.findings.map((f) => f.evidence),
    ];

    const triggerPaths = computeBoundaryTriggerPaths(paths, contract.boundaries, acceptanceFiles, citationTexts);
    const K = contract.contractVersion;
    for (const p of triggerPaths) {
      const decision: Decision = {
        id: `D-${this.#state.phase.phaseId}-${candidateSha.slice(0, 8)}-trigger-${this.#state.phase.decisions.length + 1}`,
        version: 1,
        phaseId: this.#state.phase.phaseId,
        source: "trigger",
        // design §3.4: worker proposes, reviewer may raise, only the owner
        // lowers — a conductor-computed trigger has no worker proposal at
        // all, so it starts at `delegated` (a real M+A/B vote is required
        // before it settles, unlike `detail`) rather than the maximum
        // `reserved` (owner-mandatory, 2b's scope): a reviewer's own vote
        // on it (via its ballot, this packet's "classify" mechanism for a
        // trigger — see README) is what a real reviewer would use to raise
        // it further with a `reserved`-requesting finding, or simply
        // approve it as adequately covered. Only the owner may still lower
        // it to `detail` later (2b, via DECISION_CLASS_LOWERED).
        class: "delegated",
        choice: `Diff touches boundary/dependency/acceptance-relevant path '${p}' with no decision or finding citing it`,
        whyItMatters: "Boundary-relevant paths need an explicit review disposition (design §3.3/§3.4) — silence here is not the same as approval.",
        alternatives: [
          { option: "treat it as already covered", consequence: "a boundary or dependency change could go unreviewed" },
          { option: "classify and review it explicitly", consequence: "costs one more decision to settle before acceptance" },
        ],
        recommendation: { choice: "classify and review this trigger explicitly", reason: `'${p}' matched a boundary/dependency/acceptance rule with no citing record` },
        boundCandidateSha: candidateSha,
        boundContractVersion: K,
      };
      const result = validate(DECISION_SCHEMA, decision);
      if (!result.valid) {
        this.#log.append("sampling_error", { candidateSha, error: `trigger decision invalid: ${result.errors.join("; ")}` });
        continue;
      }
      this.#applyEvent({ type: "DECISION_ADDED", decision });
    }

    const unreferencedHunks = computeUnreferencedHunks(hunks, citationTexts);
    const detailDecisionIds = this.#state.phase.decisions.filter((d) => d.class === "detail").map((d) => d.id);
    this.#log.append("sampling", {
      candidateSha,
      unreferencedHunks: unreferencedHunks.map((h) => ({ file: h.file, header: h.header })),
      detailDecisionIds,
      triggerPaths,
    });
  }

  async #killLiveShGroups(): Promise<void> {
    const groups = [...this.#liveShGroups];
    this.#liveShGroups.clear();
    await Promise.all(
      groups.map((pgid) => killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined)),
    );
  }

  #onShIntent(agentId: string, _commandId: string, pgid: number): void {
    if (this.#stopRequested) {
      // runCommand resumes the group (SIGCONT) right after this returns;
      // killing it first would make that resume fail. Kill it just after.
      setImmediate(() => void killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined));
      return;
    }
    this.#liveShGroups.add(pgid);
    this.#agents.get(agentId)?.shGroups.add(pgid);
    this.#log.append("intent", { agentId, pgid }, `sh-${agentId}-${pgid}`);
  }

  /** ODP-3 (carried from 06c, owner-confirmed): a STAGE command's own process
   * group — a check, the baseline, the gate — is tracked like an agent's `sh`
   * command, so `stop()` signals it too. A check that is still running when
   * the conductor stops must not outlive the run.
   *
   * It is tracked with the time it was recorded, and killed through
   * `#killRecordedGroup` (fail-closed): a pgid the OS recycled after the
   * command's own group ended belongs to some other process, and A2/C3 forbid
   * signalling it. That is not theoretical — without the check, a stop could
   * signal an unrelated process whose pgid had been reused (caught by
   * test/effects/shell.test.ts's TERM-ignoring escalation test under a
   * parallel `make check`). */
  #onStageShIntent(pgid: number): void {
    if (this.#stopRequested) {
      // runCommand resumes the group (SIGCONT) right after this returns;
      // killing it first would make that resume fail. Kill it just after.
      setImmediate(() => void killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined));
      return;
    }
    this.#liveStageGroups.set(pgid, Date.now());
  }

  /** The stage command in PGID finished (or was killed): stop tracking it, so
   * `stop()` does not signal a group that is already gone. */
  #onStageShExit(pgid: number): void {
    this.#liveStageGroups.delete(pgid);
  }

  /** Signal every stage command group this run is still running, each only
   * when its leader is proven to be the run's own (`#killRecordedGroup`). */
  async #killLiveStageGroups(): Promise<void> {
    const groups = [...this.#liveStageGroups.entries()];
    this.#liveStageGroups.clear();
    await Promise.all(groups.map(([pgid, at]) => this.#killRecordedGroup(pgid, at)));
  }

  #onNoSubmission(agentId: string): void {
    const handle = this.#agents.get(agentId);
    // A reviewer's missing submission is detected from agent_settled in
    // #runReview; resolving donePromise here would count it as submitted.
    if (handle && handle.role === "worker") handle.doneResolve();
  }

  #cwdFor(agentId: string): string | undefined {
    const handle = this.#agents.get(agentId);
    if (!handle) return undefined;
    // Plan 06g2: a lane agent's commands run in ITS OWN lane worktree (a lane
    // worker) or in the candidate it reviews — never in another lane's tree.
    if (handle.laneReview) return path.join(this.#paths.candidates, handle.laneReview.sha);
    if (handle.lane !== undefined) return this.#laneWorktree(handle.lane);
    return handle.role === "worker" ? this.#paths.worktree : this.#candidateDir();
  }

  #candidateDir(): string {
    const sha = this.#state.phase.candidate?.sha ?? "unknown";
    return path.join(this.#paths.candidates, sha);
  }

  /** Resolves once `agentId`'s tracked token total (see `#trackRunTokens`)
   * first reaches `cap` — design §8.1's per-attempt token cap. Polled rather
   * than event-driven since `#trackRunTokens` only updates a plain map. The
   * returned `cancel()` MUST be called once the caller's race settles
   * (exactly like `cancelableTimeout`'s own doc comment) — otherwise this
   * keeps re-arming a `setTimeout` for the rest of the process's life, the
   * same leftover-timer bug round-of-review item 1 fixed for the plain
   * deadline timers. */
  #waitTokenCapExceeded(agentId: string, cap: number): { promise: Promise<void>; cancel: () => void } {
    let cancelled = false;
    let timer: NodeJS.Timeout | undefined;
    const promise = new Promise<void>((resolve) => {
      const check = () => {
        if (cancelled) return;
        if ((this.#agentTokenTotals.get(agentId) ?? 0) >= cap) {
          resolve();
          return;
        }
        timer = setTimeout(check, 50);
      };
      check();
    });
    return {
      promise,
      cancel: () => {
        cancelled = true;
        if (timer) clearTimeout(timer);
      },
    };
  }

  #onRunBudgetExceeded(): void {
    if (this.#state.run !== "RUN_ACTIVE") return;
    this.#applyEvent({ type: "RUN_BUDGET_EXCEEDED" });
  }

  // -- worker attempt -----------------------------------------------------

  async #runWorkerAttempt(actionId: string): Promise<void> {
    // design §2.2 (round-of-review item 5): "if the sweep found survivors,
    // the worktree is tainted and the next attempt starts from a clean
    // checkout of the last candidate." This also serves recovery: on a
    // restart, `worktreeTainted` is whatever the last logged FREEZE_COMPLETED
    // said, folded through reduce() exactly like any other fact.
    if (this.#state.phase.worktreeTainted && this.#state.phase.candidate) {
      const sha = this.#state.phase.candidate.sha;
      this.#log.append("worktree_reset", { candidateSha: sha, reason: "worktree tainted by a sweep that found survivors" });
      removeWorktree(this.#plan.repo, this.#paths.worktree);
      createWorktree(this.#plan.repo, this.#paths.worktree, sha);
    }

    // Plan 04a: the base baseline now runs entirely in its own BASELINE
    // state, before this dispatch; the worker's attempt deadline begins here
    // at its own launch, exactly as before.
    const agentId = `worker-${this.#state.phase.attempt.n}-${actionId}`;
    const contract = this.#state.phase.contract;
    const streamFile = path.join(this.#paths.stream, `${agentId}.jsonl`);
    // design §9.3's "agent attempt" reconciliation for a worker: "new
    // attempt on the SAME session file with an interruption note." When a
    // prior attempt was interrupted by a conductor crash, `#reconcileOne`
    // left the crashed attempt's own session directory here — reuse it
    // (once) instead of minting a fresh one.
    const resumedSessionDir = this.#recoveredSessionDir;
    this.#recoveredSessionDir = undefined;
    // Plan 2c / design §6.1: "REPAIRING — the same worker session, a new
    // attempt". One session directory per role per phase; a later attempt
    // continues the most recent session in it, so the worker keeps its own
    // reasoning about what it built (dogfood run 4ec5e0f8's repair ran in a
    // fresh session and spent its first half re-deriving its own work).
    const sessionDir = resumedSessionDir ?? path.join(this.#paths.sessions, `worker-${this.#state.phase.phaseId}`);
    fs.mkdirSync(sessionDir, { recursive: true });
    const continueSession = hasSessionFile(sessionDir);

    const protectedPaths = contract.acceptance.filter((a) => a.includes("/")).join(",");
    const env: NodeJS.ProcessEnv = {
      ...this.#extraEnv,
      ...this.#piEnvFor?.("worker", agentId),
      TT_SOCKET: this.#paths.sock,
      // The extension waits this long for a command's result: the conductor's
      // per-command limit plus room for the kill and its report.
      TT_SH_WAIT_MS: String(this.#deadlines.shCommandMs + 30_000),
      TT_WORKTREE: this.#paths.worktree,
      TT_RUN_DIR: this.#runDir,
      TT_PROTECTED: protectedPaths,
      // Plan 06b (OD-1): a structured phase makes the worker submit coverage;
      // an old-format phase does not.
      TT_ITEMS: this.#structured() ? "1" : "0",
      // Where find/grep/ls may search: the worktree and the plan's references.
      TT_SEARCH_ROOTS: [this.#paths.worktree, this.#paths.refs].join(path.delimiter),
      // Plan 01a: the plan's secrets — the names (so the extension guard
      // knows what to look for) and each value, which lives only here and in
      // the agent's own environment. Set last: these win over any
      // test-injected env, so a guard always sees the run's real value.
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
    };

    // Plan 05d / finding #33: the spawn is a function so a hello timeout can
    // retry the launch once. `helloPromise`/`donePromise`/`discoveryPromise`
    // are created per launch and returned, so the rest of the attempt uses the
    // live process's own promises.
    const spawnWorkerAgent = (): {
      agent: PiAgent;
      handle: AgentHandle;
      helloPromise: Promise<HelloResult>;
      donePromise: Promise<void>;
      discoveryPromise: Promise<void>;
    } => {
      let helloResolve!: (r: HelloResult) => void;
      const helloPromise = new Promise<HelloResult>((resolve) => {
        helloResolve = resolve;
      });
      let doneResolve!: () => void;
      const donePromise = new Promise<void>((resolve) => {
        doneResolve = resolve;
      });
      let discoveryResolve!: () => void;
      const discoveryPromise = new Promise<void>((resolve) => {
        discoveryResolve = resolve;
      });

      const workerPiCommand = this.#resolvePiCommand("worker");
      const workerProviderModel = this.#providerModelFor?.("worker");
      const agent = spawnPiAgent({
        command: workerPiCommand,
        args: [
          ...this.#resolvePiArgsPrefix("worker"),
          ...launchArgs("worker", {
            sessionDir,
            continueSession,
            noSession: workerPiCommand !== undefined,
            provider: workerProviderModel?.provider,
            model: workerProviderModel?.model,
          }),
        ],
        cwd: this.#paths.worktree,
        env,
        role: "worker",
        agentId,
        streamFile,
        secrets: this.#secretMaskable,
        abortGraceMs: this.#deadlines.abortGraceMs,
        termGraceMs: this.#deadlines.termGraceMs,
        onEvent: (event) => {
          this.#noteActivity(agentId, event);
          this.#trackRunTokens(agentId, event);
          this.#trackFileChanges(agentId, streamFile, event);
        },
      });

      const handle: AgentHandle = {
        agent,
        role: "worker",
        agentId,
        helloResolve,
        helloPromise,
        shGroups: new Set(),
        doneResolve,
        donePromise,
        discoveryResolve,
        discoveryPromise,
      };
      this.#agents.set(agentId, handle);
      return { agent, handle, helloPromise, donePromise, discoveryPromise };
    };

    let launched = spawnWorkerAgent();
    let submittedKeepHandle = false;

    // design §9.3: the "agent attempt" row's own intent/completion pair —
    // logged once the pgid is known (agent.pgid, synchronously available
    // once spawnPiAgent returns), so a crash before this line leaves no
    // dangling in-flight entry at all (ACTION_STARTED had not been paired
    // with a live process yet); a crash after it is exactly what
    // `#reconcileOne`'s `dispatch_worker` case reconciles.
    this.#log.intent(actionId, { agentId, pgid: launched.agent.pgid, sessionDir });
    crashAt("before_dispatch_worker");

    try {
      let hello = await raceTimeout(launched.helloPromise, this.#deadlines.helloTimeoutMs, "hello");
      if (hello === "timeout") {
        // Plan 05d / finding #33: a slow start (a long `--continue` session
        // being loaded) is not the code's failure and must not consume a
        // repair attempt. Retry the launch once with a longer limit, scaled
        // to the session being continued and capped at 60 s.
        await launched.agent.terminate();
        this.#agents.delete(agentId);
        const bytes = sessionBytes(sessionDir);
        const retryMs = helloRetryTimeoutMs(this.#deadlines.helloTimeoutMs, bytes);
        this.#applyEvent({
          type: "LAUNCH_RETRIED",
          role: "worker",
          timeoutMs: this.#deadlines.helloTimeoutMs,
          retryTimeoutMs: retryMs,
          sessionBytes: bytes,
        });
        launched = spawnWorkerAgent();
        this.#log.intent(actionId, { agentId, pgid: launched.agent.pgid, sessionDir, retry: true });
        hello = await raceTimeout(launched.helloPromise, retryMs, "hello");
      }
      const { agent, handle, donePromise, discoveryPromise } = launched;
      if (hello === "timeout") {
        await agent.terminate();
        this.#agents.delete(agentId);
        this.#log.completion(actionId, { outcome: "hello-timeout" });
        // Two hello timeouts in a row are an environment problem (like 05i's
        // preflight), never a code failure and never a repair attempt.
        this.#applyAgentEnvFailure("worker hello");
        return;
      }
      if (!hello.ok) {
        await agent.terminate();
        this.#agents.delete(agentId);
        this.#log.completion(actionId, { outcome: "hello-failed" });
        // design §2.1: a tool-set mismatch is a launch failure, not a
        // warning — straight to BLOCKED, no repair round spent.
        if (hello.mismatch) {
          this.#applyEvent({ type: "LAUNCH_FAILED", role: "worker", ...hello.mismatch });
        } else {
          this.#applyEvent({ type: "ATTEMPT_TIMED_OUT" });
        }
        return;
      }

      const interruptionNote = resumedSessionDir
        ? "Note: your previous attempt was interrupted by a conductor restart, before it could observe your outcome. Continue from where you left off."
        : undefined;
      // design §7.4 `note` (conductor state): a note is queued for the NEXT
      // worker attempt. Deliver only the notes not yet delivered — tracked
      // by `deliveredNoteCount` and recorded as NOTES_DELIVERED once the
      // prompt has been sent — so a note does not keep steering every later
      // attempt (the advisory finding on this candidate). Plan-level notes
      // are static and are always included.
      const allNotes = this.#state.phase.ownerNotes ?? [];
      const deliveredCount = this.#state.phase.deliveredNoteCount ?? 0;
      const undelivered = allNotes.slice(deliveredCount);
      const queuedNotes = undelivered.join("\n");
      const ownerNotes = [this.#plan.ownerNotes, queuedNotes].filter((n) => n && n.length > 0).join("\n");
      await agent.prompt(
        buildWorkerPrompt(
          contract,
          ownerNotes.length > 0 ? ownerNotes : undefined,
          interruptionNote,
          this.#repairContext(),
          runReferences(this.#runDir),
          this.#secretNames,
          this.#state.phase.ownerDirectives,
          this.#baselineFailedCommands(),
          this.#state.phase.messages,
          this.#agentToolLines(),
        ),
      );
      // Record delivery only after the prompt was sent; a crash between the
      // prompt and this append re-delivers on the next attempt (at-least-
      // once for the first delivery), which is safer than silently losing
      // the note.
      if (undelivered.length > 0) {
        this.#applyEvent({ type: "NOTES_DELIVERED", phaseId: this.#state.phase.phaseId, count: undelivered.length });
      }

      const workerTimeout = this.#withStallWatch(
        agentId,
        agent,
        cancelableTimeout(this.#deadlines.workerAttemptMs, "timeout" as const),
        "Owner (conductor): no progress for a while. Continue the work now, or call submit_phase with what you have and disclose what is unfinished.",
      );
      const races: Array<Promise<"submitted" | "settled" | "exited" | "timeout" | "tokenCap">> = [
        donePromise.then(() => "submitted" as const),
        agent.waitSettled().then(() => "settled" as const),
        agent.waitExit().then(() => "exited" as const),
        workerTimeout.promise,
      ];
      // design §8.1: "per-attempt token cap from Pi usage events" — treated
      // exactly like the attempt's own timeout (same outcome, same event).
      const cap = this.#deadlines.tokenCapPerAttempt;
      const tokenCap = cap !== undefined ? this.#waitTokenCapExceeded(agentId, cap) : undefined;
      if (tokenCap) races.push(tokenCap.promise.then(() => "tokenCap" as const));
      const outcome = await Promise.race(races);
      workerTimeout.cancel();
      tokenCap?.cancel();
      // design §9.3's "after the effect but before its completion event"
      // boundary: the race has resolved (the effect happened) but nothing
      // below has recorded it yet.
      crashAt("after_dispatch_worker");
      this.#log.completion(actionId, { outcome });

      if (outcome === "submitted") {
        // Freeze already kicked off from #onSubmit; nothing more to do
        // here — this attempt's job is finished either way. Deliberately
        // does NOT fall through to the `finally`'s `#agents.delete` for
        // this one outcome: design §6.2 step 1 has the worker's process
        // still alive (and possibly still calling `sh`, exactly what
        // freeze's own "quiesce" step exists to end) until its own abort
        // actually lands — `#cwdFor`/`#onShIntent` need `#agents.get
        // (agentId)` to still resolve during that window, or a worker's
        // post-submission `sh` call silently gets no recorded cwd/pgid at
        // all (round-of-review-worthy bug this test caught: found via
        // freeze-e2e). `#runFreeze` itself deletes the handle once
        // quiesced, so ownership just moves to there instead of here.
        submittedKeepHandle = true;
        return;
      }
      if (outcome === "timeout" || outcome === "tokenCap") {
        await agent.terminate();
        await this.#sweepAndClear(handle);
        this.#applyEvent({ type: "ATTEMPT_TIMED_OUT" });
        return;
      }
      if (outcome === "settled") {
        // agent_settled with no submission: either the extension's
        // no_submission signal already resolved donePromise (handled
        // above) or the agent settled without ever reaching that guard
        // (e.g. a scripted fake-pi with no hello/tool-call plumbing).
        await agent.terminate();
        this.#applyEvent({ type: "ATTEMPT_NO_SUBMISSION" });
        return;
      }
      // outcome === "exited": the agent process ended on its own —
      // force-killed, crashed, or otherwise — without ever completing a
      // submission. Design §9.3's "agent attempt" reconciliation: kill
      // every recorded shell group, sweep, mark the attempt interrupted.
      await this.#sweepAndClear(handle);
      this.#applyEvent({ type: "ATTEMPT_INTERRUPTED" });
    } finally {
      if (!submittedKeepHandle) this.#agents.delete(agentId);
    }
  }

  /** Plan 2c: the repair request for the next worker attempt, built from the
   * last reviewed candidate's findings, votes and corrections. `undefined`
   * before any candidate has been reviewed (first attempt, or a retry of an
   * attempt that never produced a candidate). */
  #repairContext(): RepairContext | undefined {
    const phase = this.#state.phase;
    const C = phase.candidate?.sha;
    if (!C) return undefined;
    const reviewed = seatsOf(phase.contract).some((w) => phase.reviews[w]?.review?.candidateSha === C);
    const checksFailed = phase.checks?.candidateSha === C && phase.checks.passed === false;
    const probeFailed = phase.probe?.candidateSha === C && phase.probe.passed === false;
    if (!reviewed && !checksFailed && !probeFailed) return undefined;
    const clip = (t: string, n = 600) => (t.length > n ? `${t.slice(0, n)}…` : t);
    const findingLine = (f: Finding) =>
      `${f.id} (${f.kind}, raised by ${f.raisedBy}${f.alsoRaisedBy?.length ? ` and ${f.alsoRaisedBy.join(", ")}` : ""}): ${clip(f.evidence)}`;
    const open = phase.findings.filter((f) => f.status === "open");
    const blocking = open.filter((f) => f.severity === "blocking").map(findingLine);
    if (checksFailed) {
      blocking.unshift("The phase checks failed on the candidate (see the check output in your worktree by rerunning the failing test).");
      // Plan 05d: name each failing test's own label, so the worker repairs a
      // real failure and leaves a load-only flake alone (finding #35).
      blocking.push(...checkFailureLines(phase));
    }
    if (probeFailed) blocking.unshift("The integration probe failed: the candidate does not merge cleanly or fails the checks when merged onto the integration branch.");
    // Plan 01f: a failed gate is a repair round that shows the worker the
    // log (design 01_ref_design.md): the conductor's own record, not an
    // agent's summary of it, with the last lines inline.
    const gateRecord = this.#gateRecords().find((r) => r.candidateSha === C);
    if (gateRecord && !gateRecord.passed) {
      // One outcome phrase (core/gate.ts): a gate that never started says so
      // instead of "exited undefined" (findings B-22/A-23).
      const how = gateOutcomeText(gateRecord, { withElapsed: true });
      const tail = this.#gateTailForPrompt(C);
      blocking.unshift(
        `The conductor ran the phase's gate command and it ${how}: ${gateRecord.command} (checks/${C}/gate.log, sha256 ${gateRecord.logSha256}).` +
          (tail ? ` Last ${GATE_TAIL_LINES} lines of its log:\n${tail}` : ""),
      );
    }
    // Plan 04b: the owner's choice on an escalated blocker is an instruction
    // to the NEXT attempt (round-3 review, advisory A-6), never a permanent
    // must-fix item — see ownerBlockerChoiceLines' own rule.
    blocking.push(...ownerBlockerChoiceLines(phase, C));
    // Plan 06b: list only the items a majority did not meet (or fit), with
    // the reviewers' evidence, so the repair addresses exactly those points.
    if (this.#structured()) {
      blocking.push(...repairItemLines(this.#itemOutcomes(), phase.overturns ?? []));
    }
    const failedDecisions: string[] = [];
    for (const d of phase.decisions) {
      if (!isLiveDecision(d) || d.class === "detail") continue;
      // Plan 01g: an amendment is never a "change this decision" repair item
      // — the worker cannot edit it and it never blocks acceptance (B-10).
      if (d.amendment) continue;
      const st = decisionStatus(d, phase);
      if (st.status !== "failed" && st.status !== "suspended" && st.status !== "owner") continue;
      const rejections = phase.ballots
        .filter((b) => b.decisionId === d.id && b.vote === "reject" && b.boundCandidateSha === C)
        .map((b) => `${b.reviewer}: "${clip(b.rationale, 300)}"`);
      failedDecisions.push(`${d.id} "${d.choice}" — ${st.reason ?? st.status}${rejections.length ? `. ${rejections.join("; ")}` : ""}`);
    }
    return {
      round: phase.round ?? 1,
      previousCandidate: C,
      blocking,
      failedDecisions,
      advisory: open.filter((f) => f.severity === "advisory").map(findingLine),
      corrections: phase.corrections.filter((c) => c.status === "open").map((c) => c.correctionText),
      // Plan 01g: amendment records are phase-level, not the worker's to
      // keep/change/withdraw; exclude them so the worker never sees a record
      // whose stated change would be ignored (B-10).
      priorDecisions: phase.decisions
        .filter((d) => d.source === "worker" && isLiveDecision(d) && !d.amendment)
        .map((d) => ({ id: d.id, choice: d.choice })),
    };
  }

  // -------------------------------------------------------------------------
  // Plan 06b: the item loop. Every point of a structured plan is an A, R or
  // C item the loop carries mechanically: the worker's coverage, the check
  // resolution, the reviewers' per-item verdicts and the acceptance decision.
  // -------------------------------------------------------------------------

  /** The phase's structured items (synthesizing the old format when the
   * contract carries only an acceptance list). */
  #planItems(): PlanItems {
    return itemsFromPhase(this.#state.phase.contract);
  }

  /** OD-2 A1: the ONE structured-ness decision, shared with the prompts, the
   * views and the extension. */
  #structured(): boolean {
    return isStructured(this.#state.phase.contract);
  }

  /** The item loop (coverage, per-item verdicts, symbol pre-check, tally)
   * applies exactly to a structured phase. */
  #itemsEnforced(): boolean {
    return this.#structured();
  }

  #itemTestOutcomes(): Map<string, "passed" | "failed" | "missing"> {
    const m = new Map<string, "passed" | "failed" | "missing">();
    for (const r of this.#state.phase.checkResolution ?? []) m.set(r.name, r.outcome);
    return m;
  }

  /** The worker's own anchors (its coverage `where` entries), which a
   * `met`/`fits` verdict may not lean on alone. */
  #workerAnchors(): string[] {
    const out: string[] = [];
    for (const e of this.#state.phase.coverage?.items ?? []) out.push(...e.where);
    for (const e of this.#state.phase.coverage?.arch ?? []) out.push(...e.where);
    return out;
  }

  #reviewerReadFiles(reviewer: Reviewer): string[] {
    return [...(this.#reviewerReads.get(reviewer) ?? [])];
  }

  #reviewerRanCommands(reviewer: Reviewer): string[] {
    return [...(this.#reviewerCommands.get(reviewer) ?? [])];
  }

  /** The candidate's changed files (repo-relative), diffed against the phase
   * BASE (the integration head the phase started from), not the candidate's
   * parent commit: freeze commits stack and may be empty, so a `C^..C` diff
   * would hide an earlier attempt's change (finding disc-M-31). */
  #diffFiles(C: string): string[] {
    const base = this.#state.phase.integrationHead;
    try {
      return execFileSync("git", ["-C", this.#plan.repo, "diff", "--name-only", base, C], { encoding: "utf8" }).trim().split("\n").filter(Boolean);
    } catch {
      try {
        return execFileSync("git", ["-C", this.#plan.repo, "diff", "--name-only", `${C}^`, C], { encoding: "utf8" }).trim().split("\n").filter(Boolean);
      } catch {
        return [];
      }
    }
  }

  /** The code facts a verdict is validated against: candidate file lines,
   * the diff, the item's `:WHERE:`, the check run's test outcomes, the files
   * this reviewer read, and the worker's own anchors. */
  #verdictContext(item: FlatItem, reviewer: Reviewer): VerdictContext {
    const dir = this.#candidateDir();
    const C = this.#state.phase.candidate?.sha ?? "";
    return {
      lineCount: (p: string) => {
        try {
          return fs.readFileSync(path.join(dir, p), "utf8").split("\n").length;
        } catch {
          return undefined;
        }
      },
      diffFiles: C ? this.#diffFiles(C) : [],
      ...(item.where ? { where: item.where } : {}),
      testOutcomes: this.#itemTestOutcomes(),
      reviewerReadFiles: this.#reviewerReadFiles(reviewer),
      workerAnchors: this.#workerAnchors(),
      reviewerCommands: this.#reviewerRanCommands(reviewer),
    };
  }

  /** Every reason a review's item section must be refused and re-asked: a
   * missing verdict (the complete-ballot rule extended to items), an invalid
   * verdict value, or a verdict whose anchors the code cannot follow. */
  #reviewItemIssues(review: Review): string[] {
    if (!this.#itemsEnforced()) return [];
    const items = this.#planItems();
    const issues = reviewItemsIssues({ items: review.items ?? [], arch: review.arch ?? [] }, items);
    const flat = flatItems(items);
    for (const v of review.items ?? []) {
      const item = flat.find((i) => i.id === v.id);
      if (item) issues.push(...verdictIssues(item, v, this.#verdictContext(item, review.reviewer)));
    }
    for (const v of review.arch ?? []) {
      const item = flat.find((i) => i.id === v.id);
      if (item) issues.push(...verdictIssues(item, v, this.#verdictContext(item, review.reviewer)));
    }
    return issues;
  }

  /** The per-item majority across the three reviews. */
  #itemOutcomes(): ItemOutcome[] {
    const reviews = seatsOf(this.#state.phase.contract).map((seat) => {
      const r = this.#state.phase.reviews[seat]?.review;
      return { seat, items: r ? { items: r.items ?? [], arch: r.arch ?? [] } : undefined };
    });
    const outcomes = tallyItems(this.#planItems(), reviews, this.#state.phase.overturns ?? []);
    // A symbol the conductor could not find is `deviates` regardless of the
    // seats' own verdicts.
    const symbolDeviations = this.#state.phase.archSymbolDeviations ?? [];
    for (const o of outcomes) {
      if (o.item.kind === "architecture" && symbolDeviations.includes(o.item.id) && o.outcome !== "deviates") o.outcome = "deviates";
    }
    return outcomes;
  }

  /** The evaluator's re-verification: a majority unmet/deviates verdict the
   * code contradicts is overturned, and a unanimous thin-evidence met verdict
   * is audited. Each overturn is counted against its seat. */
  /** Plan 06i: a finding's or discovered decision's confirmed anchors must
   * cite at least one file:line that exists in the candidate, in range — the
   * same rule as a plan item's check. */
  #candidateAnchorsValid(evidence: string): boolean {
    const anchors = evidenceFileAnchors(evidence);
    if (anchors.length === 0) return false;
    const dir = this.#candidateDir();
    return anchors.every((a) => {
      try {
        const lines = fs.readFileSync(path.join(dir, a.path), "utf8").split("\n").length;
        return a.start >= 1 && a.end <= lines;
      } catch {
        return false;
      }
    });
  }

  /** OD-2 A3: an evaluator item check's evidence must cite at least one
   * file:line that exists in the candidate, in range. */
  #itemCheckAnchorsValid(evidence: string, ctx: VerdictContext): boolean {
    const anchors = evidenceFileAnchors(evidence);
    if (anchors.length === 0) return false;
    return anchors.every((a) => {
      const lines = ctx.lineCount(a.path);
      return lines !== undefined && a.start >= 1 && a.end <= lines;
    });
  }

  /** Plan 06b (OD-2 A3, ODP-2): the item ids the evaluator owes a check for.
   * The duty sits with the FINDING pass only (disc-M-89), and covers every
   * outcome that is not met/fits (ODP-2: never raise an item blocker without
   * a check). An architecture deviation the owner already accepted is not a
   * blocker and owes no check. */
  #owedItemCheckIds(messageType: MessageType): string[] {
    const ids = new Set<string>();
    // Plan 06i (A3): a discovered decision is published as a `tradeoff`
    // message, so its impact classification is owed on that pass; a finding's
    // on the `finding` pass. A missing one is re-prompted ONCE and then
    // escalated, never defaulted.
    if (messageType === "tradeoff") {
      for (const d of discoveredDecisions(this.#state.phase)) {
        if (!recordClassified(this.#state.phase, "decision", d.id)) ids.add(d.id);
      }
      return [...ids];
    }
    if (messageType !== "finding") return [];
    if (this.#itemsEnforced() && itemsNeedingEvaluatorReverify(this.#state.phase)) {
      const accepted = new Set(this.#state.phase.acceptedDeviations ?? []);
      for (const o of phaseItemOutcomes(this.#state.phase)) {
        if (o.outcome === "met" || o.outcome === "fits") continue;
        if (o.item.kind === "architecture" && accepted.has(o.item.id)) continue;
        ids.add(o.item.id);
      }
      // Plan 06c (A4/R9): a unanimous thin met/fits is an OWED item check too.
      // A missing one is re-prompted once, then recorded `unchecked`; a
      // `contradicted` check with valid anchors overturns it, and code never
      // withdraws the verdict on its own.
      for (const o of thinMetItems(phaseItemOutcomes(this.#state.phase), this.#workerAnchors())) ids.add(o.item.id);
    }
    // Plan 06i (A3): every open finding owes its impact classification
    // through the same form.
    for (const f of openFindings(this.#state.phase)) {
      if (!recordClassified(this.#state.phase, "finding", f.id)) ids.add(f.id);
    }
    return [...ids];
  }

  #itemOverturns(outcomes: readonly ItemOutcome[]): import("./core/items.ts").Overturn[] {
    const out: import("./core/items.ts").Overturn[] = [];
    const readsBySeat: Record<string, readonly string[]> = {
      M: this.#reviewerReadFiles("M"),
      A: this.#reviewerReadFiles("A"),
      B: this.#reviewerReadFiles("B"),
    };
    for (const o of outcomes) {
      const ctx = this.#verdictContext(o.item, (o.seats[0]?.seat ?? "M") as Reviewer);
      const testOverturns = reverify(o.item, o.seats, ctx, readsBySeat);
      if (testOverturns.length > 0) {
        out.push(...testOverturns);
        continue;
      }
      // Plan 06b (OD-1 R3b): the evaluator's substantive re-check. A
      // `contradicted` check overturns every seat whose verdict made the
      // majority, recorded with what the evaluator checked.
      if (o.outcome === "unmet" || o.outcome === "deviates") {
        const check = (this.#state.phase.itemChecks ?? []).find((c) => c.itemId === o.item.id && c.verdict === "contradicted");
        // OD-2 A3: a contradicted check overturns only when its evidence
        // anchors are valid (each cited file exists and its lines are in
        // range), exactly like a reviewer verdict.
        if (check && this.#itemCheckAnchorsValid(check.evidence, ctx)) {
          for (const v of o.seats.filter((s) => s.verdict === o.outcome)) {
            out.push({ seat: v.seat, id: o.item.id, kind: o.item.kind, verdict: v.verdict, effect: "flip", reason: `evaluator re-check contradicted it: ${check.evidence}` });
          }
        }
      }
      // Plan 06c (R5): a unanimous thin met/fits the evaluator contradicts
      // (with valid anchors) is overturned too.
      if ((o.outcome === "met" || o.outcome === "fits") && thinMetItems([o], this.#workerAnchors()).length > 0) {
        const check = (this.#state.phase.itemChecks ?? []).find((c) => c.itemId === o.item.id && c.verdict === "contradicted");
        if (check && this.#itemCheckAnchorsValid(check.evidence, ctx)) {
          for (const v of o.seats.filter((s) => s.verdict === o.outcome)) {
            out.push({ seat: v.seat, id: o.item.id, kind: o.item.kind, verdict: v.verdict, effect: "flip", reason: `evaluator re-check contradicted the thin met verdict: ${check.evidence}` });
          }
        }
      }
    }
    return out;
  }

  /** Plan 06b (layer 1): for an architecture item with `:WHERE:` and a named
   * type/event/function, grep the candidate for the symbol. A missing one is
   * recorded `deviates` (and a blocking finding) before any reviewer is
   * asked. Runs once the candidate exists, before REVIEWING. */
  #applyArchitectureSymbols(C: string): void {
    if (!this.#structured()) return;
    const dir = this.#candidateDir();
    const deviates: string[] = [];
    for (const arch of this.#planItems().architecture) {
      if (!arch.where) continue;
      const symbols = architectureSymbols(arch);
      if (symbols.length === 0) continue;
      let text = "";
      let missingFile = false;
      let fileShaped = false;
      for (const token of arch.where.split(/[\s,;]+/).filter(Boolean)) {
        // Only a file-shaped token (a path or a name with an extension) can be
        // grepped; a bare module name is left to the reviewers.
        if (!/[/.]/.test(token)) continue;
        fileShaped = true;
        try {
          text += `${fs.readFileSync(path.join(dir, token), "utf8")}\n`;
        } catch {
          // The `:WHERE:` file does not exist in the candidate. A missing
          // module is the clearest deviation, so it is recorded as one rather
          // than skipped (finding M-15).
          missingFile = true;
        }
      }
      if (!fileShaped) continue;
      if (missingFile || symbols.some((s) => !symbolPresent(text, s))) deviates.push(arch.id);
    }
    if (deviates.length === 0) return;
    this.#applyEvent({ type: "ITEM_STATE_UPDATED", archSymbolDeviations: deviates });
    for (const id of deviates) {
      if (this.#state.phase.findings.some((f) => f.status === "open" && f.severity === "blocking" && f.itemId === id)) continue;
      const arch = this.#planItems().architecture.find((a) => a.id === id);
      const finding: Finding = {
        id: `F-${this.#state.phase.phaseId}-symbol-${id}-${this.#state.phase.findings.length + 1}`,
        version: 1,
        phaseId: this.#state.phase.phaseId,
        kind: "defect",
        severity: "blocking",
        evidence: `${id}${arch ? ` ${arch.title}` : ""} — :WHERE: ${arch?.where ?? ""} does not name the symbol(s) ${architectureSymbols(arch!).join(", ")}`,
        raisedBy: "conductor",
        status: "open",
        boundCandidateSha: C,
        itemId: id,
      };
      this.#applyEvent({ type: "FINDING_RAISED", finding });
    }
  }

  /** Plan 06i: the triage pass — after the reviews and the evaluator's
   * item checks, before the item tally. EVERY finding and EVERY discovered
   * decision of the ledger gets exactly one disposition from `disposition()`
   * (the only place that decides); a duplicate copies its original's record.
   * An escalation opens an owner request naming the item, so acceptance
   * waits for it and nothing disappears.
   *
   * The evaluator's impact classification arrives through its item-check
   * form (`itemChecks[] = { id, verdict, impact, evidence, chosen,
   * alternative, why }`). A record the evaluator did not classify (or
   * explicitly left `unchecked` after its one re-prompt) is UNCLASSIFIED and
   * escalates — the conductor never guesses an impact. */
  #applyTriage(): void {
    const phase = this.#state.phase;
    if (!phase.candidate) return;
    const C = phase.candidate.sha;
    const K = phase.contract.contractVersion;
    // Plan 06i (A4): a deferral whose item is no longer open (repaired or
    // superseded) is resolved here, so "until resolved" is reachable and the
    // status stops listing it.
    for (const d of phase.deferrals ?? []) {
      if (d.status !== "open") continue;
      const finding = phase.findings.find((f) => f.id === d.itemId);
      const decision = phase.decisions.find((x) => x.id === d.itemId);
      const stillOpen = finding ? finding.status === "open" : decision ? !(decision.supersededBy || decision.supersededByCorrection) : false;
      if (!stillOpen) this.#applyEvent({ type: "DEFERRAL_RESOLVED", deferralId: d.id });
    }
    const dispositionFor = (source: "finding" | "decision", id: string): Disposition => {
      const ownerRequestId = `OR-${phase.phaseId}-triage-${id}`;
      const fresh = this.#state.phase;
      if (source === "finding") {
        const finding = fresh.findings.find((f) => f.id === id)!;
        const evidence = evidenceForFinding(finding, fresh);
        return disposition({ itemId: id, source, impact: recordImpact(fresh, id) }, { ...evidence, ownerRequestId });
      }
      const decision = fresh.decisions.find((d) => d.id === id)!;
      // The vote outcome is computed here (triage.ts must not import the
      // predicate's tally): a delegated/reserved decision that was balloted
      // and did not settle failed its vote, and its outcome would change the
      // output, so it escalates rather than reading as an accepted trade-off.
      const balloted = (fresh.ballots ?? []).some((b) => b.decisionId === id);
      const voteFailed = decision.class !== "detail" && balloted && !decisionSettled(decision, fresh, C, K);
      const evidence = evidenceForDecision(decision, fresh, voteFailed);
      return disposition({ itemId: id, source, impact: recordImpact(fresh, id) }, { ...evidence, ownerRequestId });
    };

    const entries = ledgerRecords(phase);
    const openRequests = new Set(
      phase.ownerRequests
        .filter((r) => r.status === "open")
        .flatMap((r) => [r.linkedFindingId, r.linkedDecisionId])
        .filter((id): id is string => typeof id === "string"),
    );
    // Pass 1: every record that is not a duplicate of another.
    const duplicates: Array<{ source: "finding" | "decision"; id: string; into: string }> = [];
    for (const { source, id } of entries) {
      const finding = source === "finding" ? phase.findings.find((f) => f.id === id) : undefined;
      const mergedInto = finding?.status === "merged" ? finding.mergedInto : undefined;
      if (mergedInto) {
        duplicates.push({ source, id, into: mergedInto });
        continue;
      }
      const record: TriageRecord = { itemId: id, source, impact: recordImpact(phase, id), disposition: dispositionFor(source, id) };
      this.#applyEvent({ type: "TRIAGE_RECORDED", record });
      if (record.disposition?.kind === "escalate" && !openRequests.has(id)) {
        this.#openTriageRequest(record, record.disposition);
        openRequests.add(id);
      }
    }
    // Pass 2: duplicates, each linked to its ORIGINAL's disposition (C3). A
    // chain of duplicates is followed transitively to the first record that is
    // not itself a duplicate, so a chain never fabricates an escalation.
    for (const dup of duplicates) {
      const originalId = this.#ultimateOriginal(dup.id, dup.into);
      const original = (this.#state.phase.triage ?? []).find((r) => r.itemId === originalId);
      const ownerRequestId = `OR-${phase.phaseId}-triage-${dup.id}`;
      const record: TriageRecord = {
        itemId: dup.id,
        source: dup.source,
        impact: original?.impact ?? "judgement",
        // Plan 06i (C2): even the orphan-duplicate fallback goes through
        // disposition() — the sole decider — rather than building a
        // disposition by hand.
        disposition:
          original?.disposition ??
          disposition(
            { itemId: dup.id, source: dup.source, impact: original?.impact ?? "judgement" },
            {
              orphanDuplicate: true,
              ownerRequestId,
              reason: `the original ${originalId} of duplicate ${dup.id} has no disposition`,
            },
          ),
        duplicateOf: originalId,
      };
      this.#applyEvent({ type: "TRIAGE_RECORDED", record });
      // M-30: an escalation must name a request that actually exists, so the
      // owner can answer it and acceptance waits for it.
      if (!original && record.disposition?.kind === "escalate" && !openRequests.has(dup.id)) {
        this.#openTriageRequest(record, record.disposition);
        openRequests.add(dup.id);
      }
    }
  }

  /** The finding behind a message id, if the message was raised from one. */
  #findingBehind(id: string): string | undefined {
    const message = (this.#state.phase.messages ?? []).find((m) => m.id === id);
    return message?.sourceRecordId ?? (this.#state.phase.findings.some((f) => f.id === id) ? id : undefined);
  }

  /** Plan 06i (C3): the first record in a duplicate chain that is not itself
   * a duplicate — the original whose disposition a duplicate copies. */
  #ultimateOriginal(dupId: string, into: string): string {
    const seen = new Set<string>([dupId]);
    let cur = this.#findingBehind(into) ?? into;
    while (!seen.has(cur)) {
      seen.add(cur);
      const finding = this.#state.phase.findings.find((f) => f.id === cur);
      if (!finding?.mergedInto) return cur;
      cur = this.#findingBehind(finding.mergedInto) ?? finding.mergedInto;
    }
    return cur;
  }

  /** Plan 06i: open the owner request an escalation names. The request's id
   * is the one the disposition recorded, so the triage record and the request
   * always name each other. It is the same request the phase's own park would
   * open (open_finding / failed_vote), so the owner has the ordinary options
   * and no duplicate request is raised later. */
  #openTriageRequest(record: TriageRecord, esc: Extract<import("./core/triage.ts").Disposition, { kind: "escalate" }>): void {
    const phase = this.#state.phase;
    const C = phase.candidate?.sha;
    const K = phase.contract.contractVersion;
    if (record.source === "finding") {
      const finding = phase.findings.find((f) => f.id === record.itemId);
      if (!finding) return;
      this.#applyEvent({
        type: "OWNER_REQUEST_OPENED",
        request: {
          id: esc.ownerRequestId,
          version: 1,
          phaseId: phase.phaseId,
          reason: esc.reason,
          origin: "open_finding",
          linkedFindingId: finding.id,
          boundCandidateSha: C,
          boundContractVersion: K,
          options: openFindingOptions(finding.kind),
          status: "open",
        },
      });
      return;
    }
    const decision = phase.decisions.find((d) => d.id === record.itemId);
    if (!decision) return;
    this.#applyEvent({
      type: "OWNER_REQUEST_OPENED",
      request: {
        id: esc.ownerRequestId,
        version: 1,
        phaseId: phase.phaseId,
        reason: esc.reason,
        origin: "failed_vote",
        linkedDecisionId: decision.id,
        relatedBallots: phase.ballots.filter((b) => b.decisionId === decision.id),
        boundCandidateSha: C,
        boundContractVersion: K,
        options: FAILED_VOTE_OPTIONS,
        status: "open",
      },
    });
  }

  /** After the reviews and evaluation settle: compute the per-item majority,
   * record the overturns, and raise a blocking finding anchored to every item
   * a majority did not meet (or fit). */
  #applyItemOutcomes(): void {
    if (!this.#itemsEnforced()) return;
    const C = this.#state.phase.candidate?.sha;
    if (!C) return;
    const raw = this.#itemOutcomes();
    const overturns = this.#itemOverturns(raw);
    this.#applyEvent({ type: "ITEM_STATE_UPDATED", overturns });
    // Recompute with the overturns applied, so an overturned verdict neither
    // blocks nor raises a finding.
    const outcomes = this.#itemOutcomes();
    const overturned = new Set(overturns.map((o) => o.id));
    for (const o of outcomes) {
      if (o.item.kind === "architecture") {
        const accepted = (this.#state.phase.acceptedDeviations ?? []).includes(o.item.id);
        if (o.outcome === "fits" || accepted) continue;
        if (overturned.has(o.item.id)) continue;
        this.#ensureItemCheck(o.item.id);
        this.#raiseItemFinding(o, C);
        continue;
      }
      if (o.outcome === "met") continue;
      if (overturned.has(o.item.id)) continue;
      this.#ensureItemCheck(o.item.id);
      this.#raiseItemFinding(o, C);
    }
  }

  /** ODP-2: an item blocker is never raised without an evaluator item check.
   * When the evaluator gave none (even after its re-prompt), the conductor
   * records its own `unchecked` check so the blocker is never silent. */
  #ensureItemCheck(id: string): void {
    if ((this.#state.phase.itemChecks ?? []).some((c) => c.itemId === id)) return;
    this.#applyEvent({ type: "ITEM_CHECK_RECORDED", itemId: id, verdict: "unchecked", evidence: "no evaluator item check was recorded" });
  }

  /** One blocking finding, anchored to its item, with the reviewers' own
   * evidence. A conductor-raised finding: no model is asked to confirm what
   * the recorded votes already say. */
  #raiseItemFinding(o: ItemOutcome, C: string): void {
    if (this.#state.phase.findings.some((f) => f.status === "open" && f.severity === "blocking" && f.itemId === o.item.id)) return;
    // OD-2 A3: name the evaluator's own re-check, so an item that blocked
    // without one is visible to the owner as `unchecked`, never silent.
    const check = (this.#state.phase.itemChecks ?? []).find((c) => c.itemId === o.item.id);
    const checkDetail = check ? (check.verdict === "unchecked" ? " — no item check was given after a re-prompt" : ` — ${check.evidence}`) : "";
    const checkNote = check ? ` [evaluator re-check: ${check.verdict}${checkDetail}]` : "";
    const evidence = `${o.item.id} ${o.item.title} — ${o.outcome}${o.evidence.length > 0 ? `: ${o.evidence.join(" | ")}` : ""}${checkNote}`;
    const finding: Finding = {
      id: `F-${this.#state.phase.phaseId}-item-${o.item.id}-${this.#state.phase.findings.length + 1}`,
      version: 1,
      phaseId: this.#state.phase.phaseId,
      // A deviating architecture item is a defect against the plan's own
      // shape, not a contract objection: the owner may accept the deviation
      // as a trade-off (`accept_risk`) or ask for a repair.
      kind: "defect",
      severity: "blocking",
      evidence,
      raisedBy: "conductor",
      status: "open",
      boundCandidateSha: C,
      itemId: o.item.id,
    };
    this.#applyEvent({ type: "FINDING_RAISED", finding });
  }

  /** A `test` verify that is missing from or failed in the candidate's check
   * run is a blocking finding anchored to its item. */
  #applyTestVerifyFindings(C: string): void {
    if (!this.#itemsEnforced()) return;
    const resolutions = this.#state.phase.checkResolution ?? [];
    const problems = testVerifyProblems(resolutions);
    if (problems.length === 0) return;
    for (const id of new Set(resolutions.filter((r) => r.outcome !== "passed").map((r) => r.id))) {
      if (this.#state.phase.findings.some((f) => f.status === "open" && f.severity === "blocking" && f.itemId === id)) continue;
      const item = flatItems(this.#planItems()).find((i) => i.id === id);
      const lines = resolutions.filter((r) => r.id === id && r.outcome !== "passed").map((r) => `test "${r.name}" ${r.outcome}`);
      const finding: Finding = {
        id: `F-${this.#state.phase.phaseId}-test-${id}-${this.#state.phase.findings.length + 1}`,
        version: 1,
        phaseId: this.#state.phase.phaseId,
        kind: "defect",
        severity: "blocking",
        evidence: `${id}${item ? ` ${item.title}` : ""} — ${lines.join(", ")}`,
        raisedBy: "conductor",
        status: "open",
        boundCandidateSha: C,
        itemId: id,
      };
      this.#applyEvent({ type: "FINDING_RAISED", finding });
    }
  }

  /** The worker's coverage as a trade-off message per partial/not_done item
   * and per deviating architecture item (ref doc: the note becomes a
   * trade-off message). Deduped by the item id. */
  #applyCoverageNotes(coverage: Coverage, C: string): void {
    for (const { id, note } of coverageNoteLines(coverage, this.#planItems())) {
      // OD-2 (disc-M-92): coverage is per attempt, so the message is keyed by
      // the candidate too — a repair's changed note is never deduplicated
      // against an earlier candidate's.
      this.#raiseMessage(
        "tradeoff",
        `coverage-${C.slice(0, 8)}-${id}`,
        { type: "tradeoff", title: `${id} not fully covered`, context: "", summary: note, evidence: [note] },
        C,
      );
    }
  }

  async #sweepAndClear(handle: AgentHandle): Promise<void> {
    for (const pgid of handle.shGroups) {
      await killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs });
    }
    // `stop()` may have already run (and closed the log/removed the
    // worktree) by the time this straggling reconciliation gets here —
    // e.g. a test tearing down right after an interrupted attempt re-
    // dispatched a new one. Nothing further to record once closed.
    if (this.#closed) return;
    const result = await sweep(this.#paths.worktree, { ownPgids: this.#ownPgids(), exceptPids: [] });
    if (this.#closed) return;
    this.#log.append("sweep", result);
  }

  /** design §6.2's freeze boundary, run once SUBMIT_PHASE has already
   * logged the raw disclosure and moved the phase to FREEZING (see
   * #onSubmit). Order matters and matches §6.2 exactly: the tool result was
   * already returned; here it's abort, wait settled, end the worker process
   * group, sweep, commit with the `TT-Action` trailer, materialize a
   * read-only checkout — THEN, and only then, assemble+bind the pending
   * disclosures into real Decision records (round-of-review item 3:
   * id/version/phase/binding are assigned here, not at SUBMIT_PHASE) and
   * validate each against schemas/decision.schema.json before logging
   * FREEZE_COMPLETED. An invalid assembly is a conductor bug and fails
   * loudly (throws), exactly like #applyEvent does for a rejected event.
   *
   * design §8.1: the *whole* sequence above is bounded by `freezeMs`, not
   * just the settle wait — on expiry the freeze itself fails (force-kill,
   * sweep, mark tainted, FREEZE_TIMED_OUT), it does not still produce a
   * candidate. `work` below is raced against a `freezeMs` deadline; if the
   * deadline wins, `timedOut` stops `work`'s own tail (the commit/assemble
   * steps) from doing anything once it eventually unblocks (it can only
   * unblock via the timeout branch's own `terminate()`, which is what makes
   * a hung `waitSettled()` resolve at all). */
  async #runFreeze(actionId: string): Promise<void> {
    return this.#runFreezeImpl(actionId);
  }

  // -------------------------------------------------------------------------
  // Plan 06g2: the two-lane round (A1/A2)
  // -------------------------------------------------------------------------

  /** The lanes this phase's frozen contract runs (a, b for `#+TT_WORKERS: 2`). */
  #lanes(): string[] {
    return lanesOfContract(this.#state.phase.contract);
  }

  /** True when this phase runs more than one lane per round. A plan without
   * `#+TT_WORKERS` (or with 1) keeps today's single-candidate loop exactly. */
  #laneRoundEnabled(): boolean {
    return isLaneRound(this.#state.phase.contract);
  }

  /** Each lane's own worktree, under the run directory (A2). The lane id is
   * part of the path, so a sweep scoped to one lane's worktree can never
   * reach the other's. */
  #laneWorktree(lane: string): string {
    return path.join(this.#runDir, "worktrees", `lane-${lane}`);
  }

  /** The lane sweep's `cwdUnder`: the SAME directory, with symlinks resolved.
   * `lsof` reports a process's real cwd (on macOS `/tmp` is `/private/tmp`),
   * and `sweepDecision`'s lane rule compares plain paths — so an unresolved
   * `cwdUnder` would make every survivor look like another lane's and be held
   * instead of killed. */
  #laneSweepRoot(worktree: string): string {
    try {
      return fs.realpathSync(worktree);
    } catch {
      return worktree;
    }
  }

  /** The environment one lane agent runs with: its OWN worktree (so the
   * extension's write/`sh` guards and `TT_SEARCH_ROOTS` are lane-scoped), the
   * same secrets and run paths as every other agent. */
  #laneEnv(worktree: string, extra: NodeJS.ProcessEnv = {}): NodeJS.ProcessEnv {
    const contract = this.#state.phase.contract;
    const protectedPaths = contract.acceptance.filter((a) => a.includes("/")).join(",");
    return {
      ...this.#extraEnv,
      ...extra,
      TT_SOCKET: this.#paths.sock,
      TT_SH_WAIT_MS: String(this.#deadlines.shCommandMs + 30_000),
      TT_WORKTREE: worktree,
      TT_RUN_DIR: this.#runDir,
      TT_PROTECTED: protectedPaths,
      TT_ITEMS: this.#structured() ? "1" : "0",
      TT_SEARCH_ROOTS: [worktree, this.#paths.refs].join(path.delimiter),
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
    };
  }

  /** Spawns one lane agent (a lane worker, a lane reviewer or a pick seat) and
   * registers its handle, exactly like the single-lane dispatches do. */
  #spawnLaneAgent(opts: {
    role: Role;
    agentId: string;
    cwd: string;
    sessionDir: string;
    env: NodeJS.ProcessEnv;
    seat?: string;
    lane?: string;
    laneRound?: number;
    laneReview?: { round: number; lane: string; sha: string; seat: string };
    pickSeat?: string;
    pickRevote?: boolean;
  }): AgentHandle {
    let helloResolve!: (r: HelloResult) => void;
    const helloPromise = new Promise<HelloResult>((resolve) => {
      helloResolve = resolve;
    });
    let doneResolve!: () => void;
    const donePromise = new Promise<void>((resolve) => {
      doneResolve = resolve;
    });
    let discoveryResolve!: () => void;
    const discoveryPromise = new Promise<void>((resolve) => {
      discoveryResolve = resolve;
    });
    fs.mkdirSync(opts.sessionDir, { recursive: true });
    const providerModel =
      opts.role === "worker"
        ? // Plan 06g2: `worker.1`/`worker.2` name a lane by its 1-based
          // position, which the caller passes as `seat`.
          this.#providerModelFor?.("worker", opts.seat ?? opts.lane)
        : // A pick seat runs on its own reviewer seat's model.
          this.#providerModelFor?.(opts.role === "picker" ? "reviewer" : opts.role, opts.seat);
    const command = this.#resolvePiCommand(opts.role);
    const streamFile = path.join(this.#paths.stream, `${opts.agentId}.jsonl`);
    // Declared before the spawn so the event callback below can never see it
    // in its temporal dead zone.
    const settleWaiters: Array<() => void> = [];
    const agent = spawnPiAgent({
      command,
      args: [
        ...this.#resolvePiArgsPrefix(opts.role),
        ...launchArgs(opts.role, {
          sessionDir: opts.sessionDir,
          continueSession: hasSessionFile(opts.sessionDir),
          noSession: command !== undefined,
          provider: providerModel?.provider,
          model: providerModel?.model,
        }),
      ],
      cwd: opts.cwd,
      env: opts.env,
      role: opts.role,
      agentId: opts.agentId,
      streamFile,
      secrets: this.#secretMaskable,
      abortGraceMs: this.#deadlines.abortGraceMs,
      termGraceMs: this.#deadlines.termGraceMs,
      onEvent: (event) => {
        this.#noteActivity(opts.agentId, event);
        this.#trackRunTokens(opts.agentId, event);
        // Plan 06g2: a lane review has two turns and must wait out each one;
        // `waitSettled()` is one-shot, so per-turn waiters are resolved here
        // (exactly like `#runReview`'s own `settleWaiters`).
        if ((event as { type?: string }).type === "agent_settled") settleWaiters.splice(0).forEach((f) => f());
      },
    });
    const handle: AgentHandle = {
      agent,
      role: opts.role,
      agentId: opts.agentId,
      helloResolve,
      helloPromise,
      shGroups: new Set(),
      doneResolve,
      donePromise,
      discoveryResolve,
      discoveryPromise,
      settleWaiters,
      ...(opts.lane !== undefined ? { lane: opts.lane } : {}),
      ...(opts.laneRound !== undefined ? { laneRound: opts.laneRound } : {}),
      ...(opts.laneReview ? { laneReview: opts.laneReview } : {}),
      ...(opts.pickSeat !== undefined ? { pickSeat: opts.pickSeat } : {}),
      ...(opts.pickRevote ? { pickRevote: true } : {}),
    };
    this.#agents.set(opts.agentId, handle);
    // Plan 06g2: record the lane agent's own process group, so a crashed
    // conductor's recovery can attribute it to this run (`#ownPgids`) and the
    // lane sweep can end it — the same intent/completion discipline every
    // other dispatch follows.
    this.#log.intent(`lane-agent-${opts.agentId}`, { agentId: opts.agentId, pgid: agent.pgid, cwd: opts.cwd });
    return handle;
  }

  /** The round's worker prompt: the SAME text for every lane (A2). It is the
   * ordinary worker prompt plus the round's own lines — which lanes exist,
   * the one base they share, and what the previous round's lanes got wrong. */
  #laneWorkerPrompt(round: number, base: string): string {
    const previous = (this.#state.phase.rounds ?? []).find((r) => r.round === round - 1);
    const failures = laneFailureLines(previous);
    const lines: string[] = [
      `Round ${round} of this phase starts from ${base.slice(0, 7)} and runs ${this.#lanes().length} lanes. You are the worker of one lane; another worker builds a second candidate from the same base with this same prompt, in its own worktree. Do not touch another lane's worktree.`,
    ];
    if (failures.length > 0) {
      lines.push(`The previous round's lanes (round ${previous!.round}):`, ...failures.map((f) => `- ${f}`));
    }
    return `${buildWorkerPrompt(
      this.#state.phase.contract,
      undefined,
      undefined,
      this.#repairContext(),
      runReferences(this.#runDir),
      this.#secretNames,
      this.#state.phase.ownerDirectives,
      this.#baselineFailedCommands(),
      this.#state.phase.messages,
      this.#agentToolLines(),
    )}\n\n${lines.join("\n")}`;
  }

  /** One lane: its own worktree at the round's base, its own worker and its
   * own sweep (A2). Returns the frozen candidate sha and the lane's
   * disclosures, or a note saying why the lane produced none. */
  async #buildLane(lane: string, round: number, base: string, prompt: string): Promise<LaneBuild> {
    const worktree = this.#laneWorktree(lane);
    if (fs.existsSync(worktree)) removeWorktree(this.#plan.repo, worktree);
    crashAt("before_create_worktree");
    createWorktree(this.#plan.repo, worktree, base);
    crashAt("after_create_worktree");
    const actionId = this.#log.actionId(`lane_${round}_${lane}`);
    this.#log.intent(actionId, { lane, round, worktree, base });
    const agentId = `lane-${round}-${lane}-${actionId}`;
    const sessionDir = path.join(this.#paths.sessions, `lane-worker-${lane}`);
    const handle = this.#spawnLaneAgent({
      role: "worker",
      agentId,
      cwd: worktree,
      sessionDir,
      env: this.#laneEnv(worktree, this.#piEnvFor?.("worker", agentId) ?? {}),
      lane,
      laneRound: round,
      // `worker.N` names the lane by its 1-based position (a = 1, b = 2).
      seat: String(this.#lanes().indexOf(lane) + 1),
    });
    try {
      const hello = await raceTimeout(handle.helloPromise, this.#deadlines.helloTimeoutMs, "hello");
      if (hello === "timeout" || !hello.ok) {
        await handle.agent.terminate();
        const note = hello === "timeout" ? "the lane's worker did not start (hello timed out)" : "the lane's worker failed to start";
        this.#log.completion(actionId, { lane, round, outcome: "no-candidate", reason: note });
        return { lane, note };
      }
      await handle.agent.prompt(prompt);
      const timeout = cancelableTimeout(this.#deadlines.workerAttemptMs, "timeout" as const);
      const outcome = await Promise.race([
        handle.donePromise.then(() => "submitted" as const),
        handle.agent.waitSettled().then(() => "settled" as const),
        handle.agent.waitExit().then(() => "exited" as const),
        timeout.promise,
      ]);
      timeout.cancel();
      if (outcome !== "submitted") {
        await handle.agent.terminate();
        const note =
          outcome === "timeout"
            ? "the lane's worker timed out before submitting"
            : `the lane's worker ended without submit_phase (${outcome})`;
        this.#log.completion(actionId, { lane, round, outcome: "no-candidate", reason: note });
        return { lane, note };
      }
      await handle.agent.terminate();
      this.#agents.delete(agentId);
      // The lane's own sweep: scoped to THIS lane's worktree (06g's
      // `sweepDecision` with `cwdUnder`), so it can never signal a process the
      // other lane started.
      const sweepResult: SweepResult = await sweep(worktree, {
        ownPgids: this.#ownPgids(),
        exceptPids: [],
        cwdUnder: this.#laneSweepRoot(worktree),
      });
      this.#log.append("sweep", { ...sweepResult, lane });
      const sha = freezeCommit(worktree, actionId, `phase ${this.#state.phase.phaseId} lane ${lane} candidate`);
      const candidateDir = path.join(this.#paths.candidates, sha);
      if (!fs.existsSync(candidateDir)) materializeCandidate(this.#plan.repo, sha, candidateDir);
      this.#log.completion(actionId, { lane, round, candidateSha: sha, tainted: sweepResult.tainted });
      return {
        lane,
        sha,
        // The lane's own sweep result, carried to FREEZE_COMPLETED at the
        // hand-off (a lane that left a survivor taints the winner's tree
        // exactly as the single-candidate freeze would).
        tainted: sweepResult.tainted,
        ...(handle.laneSubmission ?? { disclosures: [] }),
        ...(handle.laneCoverage !== undefined ? { coverage: handle.laneCoverage } : {}),
      };
    } finally {
      this.#agents.delete(agentId);
    }
  }

  /** One candidate's checks, run under the machine-wide lock (C2). */
  async #checkLaneCandidate(round: number, lane: string, sha: string): Promise<LaneCheck> {
    const lock = await acquireWaitingLock(this.#checkLockPath);
    try {
      this.#log.append("lane_check_started", { round, lane, candidateSha: sha });
      const result = await this.#runLaneChecks(lane, sha);
      this.#log.append("lane_check_finished", { round, lane, candidateSha: sha, passed: result.ok, ...(result.note ? { note: result.note } : {}) });
      return result;
    } finally {
      await lock.release();
    }
  }

  /** Runs the effective check commands for one lane candidate in a fresh
   * checkout of its commit and writes `checks/<sha>/record.json`. Nothing
   * about the phase moves here: a candidate that fails its checks records
   * CANDIDATE_CHECKED { ok: false } and the round goes on with the other
   * lane. */
  async #runLaneChecks(lane: string, candidateSha: string): Promise<LaneCheck> {
    const checkoutDir = disposableCheckout(this.#plan.repo, candidateSha);
    const outDir = path.join(this.#paths.checks, candidateSha);
    fs.mkdirSync(outDir, { recursive: true });
    const commands = checkCommands(
      this.#plan.checks,
      this.#state.phase.contract.checks,
      this.#state.phase.contract.finalChecks,
      "round",
    );
    const recordCommands: CheckRecordCommand[] = [];
    const recordLoad1 = Math.round(loadavg()[0] * 100) / 100;
    const recordFreeMemMB = Math.round(os.freemem() / (1024 * 1024));
    let passed = true;
    let combinedOutput = "";
    let firstFailure: string | undefined;
    try {
      if (!verifyIntegrity(this.#plan.repo, checkoutDir.dir, candidateSha)) {
        passed = false;
        firstFailure = "the checkout no longer matches the candidate commit";
      }
      for (const rawCommand of commands) {
        if (!passed) break;
        const command = this.#withValues(rawCommand);
        const startedAt = Date.now();
        const result = await runCommand({
          command,
          cwd: checkoutDir.dir,
          env: childEnv(),
          deadlineMs: this.#deadlines.checkMs,
          termGraceMs: this.#deadlines.termGraceMs,
          onIntent: ({ pgid }) => {
            this.#onStageShIntent(pgid);
            this.#log.intent(`lane-check-sh-${lane}-${pgid}`, { pgid });
          },
        }).result;
        combinedOutput += `${result.output}\n`;
        this.#recordCheck(outDir, command, result);
        recordCommands.push({
          command,
          exitCode: result.exitCode,
          signal: result.signal,
          timedOut: result.timedOut,
          durationMs: Date.now() - startedAt,
          passed: result.exitCode === 0 && !result.timedOut,
          log: this.#checkLogName(command),
        });
        if (!verifyIntegrity(this.#plan.repo, checkoutDir.dir, candidateSha)) {
          passed = false;
          firstFailure = "the checkout no longer matches the candidate commit";
          break;
        }
        if (result.exitCode === 0 && !result.timedOut) continue;
        // Plan 01e's pre-existing rule, unchanged: a failing check whose every
        // parsed failing test also failed on the round's base is not this
        // candidate's failure.
        if (failedNormally(result)) {
          const verdict = classifyCheckFailure(result.output, this.#baseFailuresFor(command), this.#requiredTestTexts());
          if (verdict.excused) {
            this.#log.append("check_failures_pre_existing", { candidateSha, command, failures: verdict.parsed, lane });
            continue;
          }
          if (verdict.newFailures.length > 0) firstFailure = `the check \`${command}\` failed: ${verdict.newFailures.join(", ")}`;
        }
        if (!firstFailure) firstFailure = `the check \`${command}\` ${result.timedOut ? "timed out" : `exited ${result.exitCode}`}`;
        passed = false;
      }
      this.#writeCheckRecord(outDir, {
        candidateSha,
        ...(this.#baselineBaseSha() ? { baseSha: this.#baselineBaseSha() } : {}),
        tier: "round",
        passed,
        commands: recordCommands,
        finalCommands: [],
        load1: recordLoad1,
        freeMemMB: recordFreeMemMB,
        at: new Date().toISOString(),
      });
      // The item test-verifies are resolved for the winner only (the hand-off
      // applies them); a loser's output must not touch the phase's item state.
      this.#laneCheckOutputs.set(candidateSha, combinedOutput);
      if (this.#itemsEnforced()) {
        this.#laneCheckResolutions.set(candidateSha, resolveTestVerifies(this.#planItems(), combinedOutput));
      }
      return passed ? { ok: true } : { ok: false, note: firstFailure ?? "a check command failed" };
    } finally {
      checkoutDir.dispose();
    }
  }

  /** Plan 06g2: the ids the lane's discoveries will have, in the order the
   * hand-off applies them. ONE plan for both sides: the lane's turn-2 prompt
   * shows these ids, and the hand-off passes the same numbers to
   * `#applyDiscoveries`, so a seat's ballot always names the record that
   * appears. The numbering starts after every record the phase ALREADY holds
   * (each survives FREEZE_COMPLETED, carried forward) and the lane's own
   * worker decisions — which is exactly where `#applyDiscoveries` would number
   * from — then walks M, A and B in seat order. */
  #laneDiscoveryPlan(
    round: number,
    lane: string,
    sha: string,
  ): Array<{ seat: string; disclosure: DecisionDisclosure; id: string; index: number }> {
    const key = `${round}-${lane}`;
    const cached = this.#laneDiscoveryPlans.get(key);
    if (cached) return cached;
    const build = this.#laneBuilds.get(key);
    const workerDecisions = build
      ? this.#assembleDecisions(sha, { disclosures: build.disclosures ?? [], dispute: build.dispute })
      : [];
    let n = this.#state.phase.decisions.length + workerDecisions.length;
    const out: Array<{ seat: string; disclosure: DecisionDisclosure; id: string; index: number }> = [];
    for (const seat of this.#laneSeats()) {
      for (const disclosure of this.#laneDiscoveries.get(key)?.get(seat) ?? []) {
        n += 1;
        out.push({ seat, disclosure, id: `D-${this.#state.phase.phaseId}-${sha.slice(0, 8)}-disc-${seat}-${n}`, index: n });
      }
    }
    this.#laneDiscoveryPlans.set(key, out);
    return out;
  }

  /** The lane phase a lane review is built from: the round's candidate, the
   * lane's own decisions (the worker's disclosures, assembled exactly as the
   * hand-off will assemble them), its coverage and its check resolution, and
   * the discoveries the three seats made on THIS candidate. Never the phase's
   * own candidate/decisions, which belong to another version. */
  #lanePhase(round: number, lane: string, sha: string, opts: { final?: boolean } = {}): PhaseState {
    const K = this.#state.phase.contract.contractVersion;
    const build = this.#laneBuilds.get(`${round}-${lane}`);
    const workerDecisions = build
      ? this.#assembleDecisions(sha, { disclosures: build.disclosures ?? [], dispute: build.dispute })
      : [];
    // Only turn 2 (after the discovery barrier) knows every seat's
    // discoveries, and only its plan is cached for the hand-off.
    const plan = opts.final ? this.#laneDiscoveryPlan(round, lane, sha) : [];
    const discovered: Decision[] = plan.map(({ seat, disclosure, id }) => ({
      id,
      version: 1,
      phaseId: this.#state.phase.phaseId,
      source: "reviewer-discovered",
      class: disclosure.classProposal,
      choice: disclosure.choice,
      whyItMatters: disclosure.whyItMatters,
      alternatives: disclosure.alternatives,
      recommendation: disclosure.recommendation,
      boundCandidateSha: sha,
      boundContractVersion: K,
    }));
    return {
      ...this.#state.phase,
      candidate: { sha, contractVersion: K },
      decisions: [...workerDecisions, ...discovered],
      ...(build?.coverage !== undefined ? { coverage: build.coverage as Coverage } : {}),
      checkResolution: this.#laneCheckResolutions.get(sha) ?? [],
    };
  }

  /** Plan 06g2: one seat's review of one passing lane candidate — today's
   * two-turn review, once per candidate (design §3.3): turn 1 discovers the
   * behavioural choices before the worker's own disclosure is shown, the
   * three seats' discoveries merge behind a per-candidate barrier, and turn 2
   * reviews the lane's decisions, findings and item verdicts. A review that
   * cannot be taken THROWS: the round then fails and repeats, so a winner is
   * never handed off with fewer than K × N reviews. */
  async #runLaneReview(round: number, lane: string, sha: string, seat: string): Promise<void> {
    const candidateDir = path.join(this.#paths.candidates, sha);
    const actionId = this.#log.actionId(`lane_review_${round}_${lane}_${seat}`);
    this.#log.intent(actionId, { round, lane, seat, candidateSha: sha });
    const agentId = `lane-review-${round}-${lane}-${seat}-${actionId}`;
    const handle = this.#spawnLaneAgent({
      role: "reviewer",
      agentId,
      cwd: candidateDir,
      // A FRESH session per (round, lane, seat): a seat judging the second
      // candidate must not carry the first's review in context (M's veto of
      // the shared session), and a later round re-reviews from scratch.
      sessionDir: path.join(this.#paths.sessions, `lane-reviewer-${round}-${lane}-${seat}`),
      env: this.#laneEnv(candidateDir, {
        ...(this.#piEnvFor?.("reviewer", agentId) ?? {}),
        TT_CANDIDATE_SHA: sha,
        TT_REVIEWER: seat,
      }),
      lane,
      laneRound: round,
      seat,
      laneReview: { round, lane, sha, seat },
    });
    const lanePhase = this.#lanePhase(round, lane, sha);
    try {
      const hello = await raceTimeout(handle.helloPromise, this.#deadlines.helloTimeoutMs, "hello");
      if (hello === "timeout" || !hello.ok) {
        await handle.agent.terminate();
        this.#log.completion(actionId, { round, lane, seat, ok: false, reason: "the reviewer did not start" });
        throw new Error("the reviewer did not start");
      }
      const timeout = cancelableTimeout(this.#deadlines.reviewMs, "timeout" as const);
      const nextSettle = () =>
        new Promise<"settled">((resolve) => handle.settleWaiters!.push(() => resolve("settled")));

      // Turn 1: discover, before the worker's own disclosure is shown.
      const settled1 = nextSettle();
      await handle.agent.prompt(this.#agentPrompt(this.#buildReviewerTurn1Prompt(seat as Reviewer, lanePhase, candidateDir)));
      const raced1 = await Promise.race([
        handle.discoveryPromise.then(() => "discovered" as const),
        timeout.promise,
        settled1,
        // A crashed reviewer ends the attempt at once, never at reviewMs.
        handle.agent.waitExit().then(() => "exited" as const),
      ]);
      // The discovery reply and the turn's own settle are two channels; the
      // settle can win the race. What matters is whether the discovery was
      // ACCEPTED, so the recorded discovery (not the race's winner) decides.
      const turn1 = raced1 !== "discovered" && handle.laneDiscoveries !== undefined ? ("discovered" as const) : raced1;
      if (turn1 !== "discovered") {
        timeout.cancel();
        await handle.agent.terminate();
        const why =
          turn1 === "settled"
            ? "settled without submit_discovery (turn 1)"
            : turn1 === "exited"
              ? "the reviewer's process exited before submit_discovery (turn 1)"
              : "timeout (turn 1)";
        this.#log.completion(actionId, { round, lane, seat, ok: false, reason: why });
        throw new Error(why);
      }
      const turn1Settled = await Promise.race([settled1, timeout.promise]);
      if (turn1Settled === "timeout") {
        timeout.cancel();
        await handle.agent.terminate();
        this.#log.completion(actionId, { round, lane, seat, ok: false, reason: "timeout (turn 1 did not settle)" });
        throw new Error("timeout (turn 1 did not settle)");
      }
      // The per-candidate discovery barrier: no seat gets turn 2 until all
      // three have finished turn 1 on THIS candidate, so every turn-2 prompt
      // lists the same merged records and every seat can ballot the others'.
      this.#laneArriveAtBarrier(round, lane, seat);
      const barrier = await Promise.race([this.#laneBarrierReleased(round, lane).then(() => "released" as const), timeout.promise]);
      if (barrier === "timeout") {
        timeout.cancel();
        await handle.agent.terminate();
        this.#log.completion(actionId, { round, lane, seat, ok: false, reason: "timeout (waiting for the other seats' discovery)" });
        throw new Error("timeout (waiting for the other seats' discovery)");
      }

      // Turn 2: the lane's decisions (the worker's own, plus every seat's
      // discoveries) with a ballot demanded for each votable one. The plan's
      // ids are cached here and reused at the hand-off.
      const merged = this.#lanePhase(round, lane, sha, { final: true });
      const settled2 = nextSettle();
      await handle.agent.prompt(this.#agentPrompt(this.#buildReviewerTurn2Prompt(seat as Reviewer, handle, merged, candidateDir)));
      const reviewRecorded = () =>
        ((this.#state.phase.rounds ?? []).find((r) => r.round === round)?.candidates.find((c) => c.lane === lane)?.reviews ?? []).some(
          (r) => r.seat === seat,
        );
      let turn2 = await Promise.race([
        handle.donePromise.then(() => "submitted" as const),
        timeout.promise,
        settled2,
        handle.agent.waitExit().then(() => "exited" as const),
      ]);
      // Same channel race as turn 1: the recorded review, not the race's
      // winner, says whether the submission landed.
      if (turn2 !== "submitted" && reviewRecorded()) turn2 = "submitted";
      if (turn2 === "settled") {
        this.#log.append("review_reprompt", { reviewer: seat, agentId, reason: "turn 2 settled without submit_review" });
        const settled3 = nextSettle();
        await handle.agent.prompt(
          this.#agentPrompt(
            `You ended your review turn without calling submit_review. That tool is required to finish this review. ` +
              `Call submit_review now with reviewer, phaseId, candidateSha, contractVersion, correctionStatements, ` +
              `findingStatements, items and arch, then end your turn.`,
          ),
        );
        turn2 = await Promise.race([handle.donePromise.then(() => "submitted" as const), timeout.promise, settled3]);
        if (turn2 !== "submitted" && reviewRecorded()) turn2 = "submitted";
      }
      timeout.cancel();
      await handle.agent.terminate();
      const ok = turn2 === "submitted";
      this.#log.completion(actionId, { round, lane, seat, ok, ...(ok ? {} : { reason: turn2 }) });
      if (!ok) {
        throw new Error(
          turn2 === "settled"
            ? "settled without submit_review (turn 2, after one re-prompt)"
            : turn2 === "exited"
              ? "the reviewer's process exited before submit_review (turn 2)"
              : "timeout (turn 2)",
        );
      }
    } finally {
      // A seat that died or timed out never reaches the barrier: the other
      // seats must not wait out their own deadline for it. Marking it arrived
      // releases them, and the round still fails on the missing review.
      this.#laneArriveAtBarrier(round, lane, seat);
      this.#agents.delete(agentId);
    }
  }

  /** The per-(round, lane) discovery barrier: every seat that arrives waits
   * until all of the round's seats have, exactly like the single-candidate
   * path's `#discoveryBarrier` but keyed by the candidate under review. */
  #laneArriveAtBarrier(round: number, lane: string, seat: string): void {
    const key = `${round}-${lane}`;
    const barrier = this.#laneBarriers.get(key) ?? { arrived: new Set<string>(), waiters: [] };
    if (barrier.arrived.has(seat)) return;
    barrier.arrived.add(seat);
    this.#laneBarriers.set(key, barrier);
    if (this.#laneSeats().every((s) => barrier.arrived.has(s))) barrier.waiters.splice(0).forEach((f) => f());
  }

  #laneBarrierReleased(round: number, lane: string): Promise<void> {
    const key = `${round}-${lane}`;
    const barrier = this.#laneBarriers.get(key);
    if (barrier && this.#laneSeats().every((s) => barrier.arrived.has(s))) return Promise.resolve();
    return new Promise<void>((resolve) => {
      const b = this.#laneBarriers.get(key) ?? { arrived: new Set<string>(), waiters: [] };
      b.waiters.push(resolve);
      this.#laneBarriers.set(key, b);
    });
  }

  /** One seat's pick turn. The prompt is `buildPickPrompt` (the leader seat
   * carries the ledger, every earlier round and the other candidates' diffs);
   * the vote itself is recorded by `submit_pick_vote`. */
  async #runPickTurn(
    round: number,
    base: string,
    seat: string,
    passing: ReadonlyArray<{ lane: string; sha: string }>,
    revote = false,
  ): Promise<void> {
    const actionId = this.#log.actionId(`${revote ? "revote" : "pick"}_${round}_${seat}`);
    this.#log.intent(actionId, { round, seat, lanes: passing.map((c) => c.lane), ...(revote ? { revote: true } : {}) });
    const agentId = `${revote ? "revote" : "pick"}-${round}-${seat}-${actionId}`;
    const leader = seat === this.#laneLeader();
    const otherDiffs: Record<string, string> = {};
    if (leader) {
      for (const c of passing) {
        try {
          otherDiffs[c.lane] = diffText(this.#plan.repo, base, c.sha);
        } catch {
          otherDiffs[c.lane] = "(diff unavailable)";
        }
      }
    }
    const prompt = buildPickPrompt({
      seat,
      leader,
      phaseId: this.#state.phase.phaseId,
      goal: this.#state.phase.contract.goal,
      round,
      base,
      candidates: passing.map((c) => ({ lane: c.lane, sha: c.sha, label: candidateLabel(round, c.lane) })),
      ledger: this.#laneLedgerLines(),
      earlierRounds: this.#laneEarlierRoundLines(round),
      otherDiffs,
      seats: this.#laneSeats(),
      ...(revote ? { revote: true } : {}),
    });
    const candidateDir = path.join(this.#paths.candidates, passing[0]?.sha ?? base);
    const handle = this.#spawnLaneAgent({
      role: "picker",
      agentId,
      cwd: fs.existsSync(candidateDir) ? candidateDir : this.#runDir,
      sessionDir: path.join(this.#paths.sessions, `pick-${seat}`),
      env: this.#laneEnv(this.#runDir, {
        ...(this.#piEnvFor?.("picker", agentId) ?? {}),
        TT_REVIEWER: seat,
      }),
      laneRound: round,
      seat,
      pickSeat: seat,
      ...(revote ? { pickRevote: true } : {}),
    });
    try {
      const hello = await raceTimeout(handle.helloPromise, this.#deadlines.helloTimeoutMs, "hello");
      if (hello === "timeout" || !hello.ok) {
        await handle.agent.terminate();
        this.#log.completion(actionId, { round, seat, ok: false, reason: "the seat did not start" });
        return;
      }
      await handle.agent.prompt(this.#agentPrompt(prompt));
      const timeout = cancelableTimeout(this.#deadlines.reviewMs, "timeout" as const);
      const outcome = await Promise.race([
        handle.donePromise.then(() => "submitted" as const),
        handle.agent.waitSettled().then(() => "settled" as const),
        handle.agent.waitExit().then(() => "exited" as const),
        timeout.promise,
      ]);
      timeout.cancel();
      await handle.agent.terminate();
      const ok = outcome === "submitted";
      this.#log.completion(actionId, { round, seat, ok, ...(ok ? {} : { reason: outcome }) });
      // Plan 06g2: a missing vote fails the round (it repeats); the pick is
      // never taken with fewer than all seats' votes.
      if (!ok) throw new Error(`the pick turn ${outcome} without a vote`);
    } finally {
      this.#agents.delete(agentId);
    }
  }

  /** Plan 06h (A2): the seats that review and vote, from the frozen
   * contract's `#+TT_REVIEWERS` (default `M A B`). */
  #laneSeats(): string[] {
    return seatsOf(this.#state.phase.contract);
  }

  /** Plan 06h (A2): the leader seat, from the frozen contract's
   * `#+TT_LEADER` (default the first seat). */
  #laneLeader(): string {
    return leaderOf(this.#state.phase.contract);
  }

  /** The settled ledger, one line per record — the leader seat's pick
   * context. */
  #laneLedgerLines(): string[] {
    const messages = this.#state.phase.messages ?? [];
    return ledgerEntries(messages).map((e) => {
      const message = messages.find((m) => m.id === e.messageId);
      return `${e.messageId} ${e.state}${message ? `: ${oneLine(message.title)}` : ""}`;
    });
  }

  /** Every earlier round's candidates, votes and winner, one line each. */
  #laneEarlierRoundLines(round: number): string[] {
    return (this.#state.phase.rounds ?? [])
      .filter((r) => r.round < round)
      .map((r) => {
        const votes = r.votes.map((v) => `${v.seat}→${v.lane}`).join(" ");
        const picked = r.picked ? `${candidateLabel(r.round, r.picked.lane)} won (${r.picked.votes} vote(s))` : "no winner";
        return `round ${r.round} from ${r.base.slice(0, 7)}: ${r.lanes.map((l) => candidateLabel(r.round, l)).join(", ")}${votes ? ` — votes ${votes}` : ""} — ${picked}`;
      });
  }

  /** The round's own orchestration: `runRound` (core/lanes.ts) is the only
   * module that decides the sequence; this implements its host callbacks. */
  async #runLaneRound(actionId: string): Promise<void> {
    const lanes = this.#lanes();
    // The round number: the next one, unless the last round is INCOMPLETE (a
    // conductor crash left it with fewer lane records than it has lanes) —
    // then the round is re-run under its own number, and ROUND_STARTED
    // replaces the partial record rather than leaving it as a stale round.
    const rounds = this.#state.phase.rounds ?? [];
    const last = rounds[rounds.length - 1];
    const round = last !== undefined && last.candidates.length < lanes.length ? last.round : (last?.round ?? 0) + 1;
    const base = round === 1 ? this.#state.phase.integrationHead : (this.#state.phase.candidate?.sha ?? this.#state.phase.integrationHead);
    this.#log.intent(actionId, { round, base, lanes });
    const host: LaneHost = {
      phaseId: this.#state.phase.phaseId,
      goal: this.#state.phase.contract.goal,
      seats: this.#laneSeats(),
      leader: this.#laneLeader(),
      rounds: () => this.#state.phase.rounds ?? [],
      ledger: () => this.#laneLedgerLines(),
      earlierRounds: () => this.#laneEarlierRoundLines(round),
      createWorktree: (lane, at) => {
        const worktree = this.#laneWorktree(lane);
        if (fs.existsSync(worktree)) removeWorktree(this.#plan.repo, worktree);
        createWorktree(this.#plan.repo, worktree, at);
        return worktree;
      },
      lanePrompt: (r, at) => this.#laneWorkerPrompt(r, at),
      buildLane: async (lane, r, at, prompt) => {
        const built = await this.#buildLane(lane, r, at, prompt);
        // Kept for the lane review prompts, which need the lane's own
        // disclosures before the winner's hand-off assembles them.
        this.#laneBuilds.set(`${r}-${lane}`, built);
        return built;
      },
      checkCandidate: (r, lane, sha) => this.#checkLaneCandidate(r, lane, sha),
      reviewCandidate: (r, lane, sha, seat) => this.#runLaneReview(r, lane, sha, seat),
      pickTurn: (r, at, seat, passing, revote) => this.#runPickTurn(r, at, seat, passing, revote),
      emit: (event) => this.#applyEvent(event),
    };
    const outcome = await runRound(host, { round, base, lanes });
    this.#log.completion(actionId, {
      round,
      base,
      lanes,
      ...(outcome.winner ? { winner: outcome.winner } : {}),
      ...(outcome.failure ? { failure: outcome.failure } : {}),
      candidates: outcome.builds.map((b) => ({ lane: b.lane, sha: b.sha, note: b.note })),
    });
    if (!outcome.winner) {
      // No candidate passed, OR the round could not complete (a missing
      // review or pick vote — `runRound` refuses to pick an incomplete
      // round). Either way the round repeats from the same base, and the
      // repeat costs ONE repair attempt (a round is one attempt, whatever K
      // is). ATTEMPT_NO_SUBMISSION is the ordinary "this attempt produced no
      // acceptable candidate" event; REPAIRING then starts the next round.
      if (outcome.failure) this.#log.append("round_incomplete", { round, reason: outcome.failure });
      this.#applyEvent({ type: "ATTEMPT_NO_SUBMISSION" });
      this.#clearLaneRound(round);
      return;
    }
    const build = outcome.builds.find((b) => b.lane === outcome.winner!.lane);
    await this.#handOffLaneWinner(round, outcome.winner, build);
    this.#clearLaneRound(round);
  }

  /** Drops one round's per-lane scratch state (its builds, discoveries and
   * barrier) once the round is over, so a long run does not accumulate them. */
  #clearLaneRound(round: number): void {
    for (const lane of this.#lanes()) {
      this.#laneBuilds.delete(`${round}-${lane}`);
      this.#laneDiscoveries.delete(`${round}-${lane}`);
      this.#laneDiscoveryPlans.delete(`${round}-${lane}`);
      this.#laneBarriers.delete(`${round}-${lane}`);
    }
  }

  /** Hands the round's winner to the single-candidate pipeline: the winner
   * becomes the phase's candidate, with the checks and reviews it already
   * earned — the checks are NOT re-run and the reviews are NOT re-taken. The
   * probe then runs for real (next()'s PROBING), and the winner's reviews are
   * promoted into the phase's review slots as soon as the phase reaches
   * REVIEWING. */
  async #handOffLaneWinner(
    round: number,
    winner: { lane: string; sha: string; votes: number },
    build: LaneBuild | undefined,
  ): Promise<void> {
    const roundRecord = (this.#state.phase.rounds ?? []).find((r) => r.round === round);
    const candidate = roundRecord?.candidates.find((c) => c.lane === winner.lane);
    const reviews = candidate?.reviews ?? [];
    // The worktree the phase's own worker would have used is set to the
    // winner, so every later stage that reads `paths.worktree` sees the
    // winner's tree (a repair round's lanes are created fresh from its sha).
    try {
      if (fs.existsSync(this.#paths.worktree)) removeWorktree(this.#plan.repo, this.#paths.worktree);
      createWorktree(this.#plan.repo, this.#paths.worktree, winner.sha);
    } catch (err) {
      this.#log.append("error", { where: "lane_winner_worktree", error: String((err as Error)?.message ?? err) });
    }
    fs.writeFileSync(path.join(this.#runDir, "candidate-sha.txt"), winner.sha);
    this.#pendingLaneWinner = {
      round,
      lane: winner.lane,
      sha: winner.sha,
      build: build ?? { lane: winner.lane, sha: winner.sha },
      reviews,
    };
    let assembled: Decision[] = [];
    this.#driveSuspended = true;
    try {
      // The winner's own submit_phase payload, replayed as the phase's own
      // (its disclosures are the ones the winner's worker made).
      this.#applyEvent({
        type: "SUBMIT_PHASE",
        disclosures: build?.disclosures ?? [],
        ...(build?.prior ? { prior: build.prior } : {}),
        ...(build?.dispute ? { dispute: build.dispute } : {}),
      });
      assembled = this.#assembleDecisions(winner.sha);
      this.#applyEvent({
        type: "FREEZE_COMPLETED",
        candidateSha: winner.sha,
        decisions: assembled,
        // The winner's own lane sweep, never a hard-coded false.
        tainted: build?.tainted === true,
      });
      // Plan 06g2: the winner lane's turn-1 discoveries become the phase's own
      // records, bound to the winner. The reviews' ballots name the ids the
      // lane's turn-2 prompt listed (the lane's worker decisions and these
      // discoveries), so the ids must match exactly — and they do, because
      // `#lanePhase` and `#applyDiscoveries` derive them the same way (worker
      // decisions first, then M, A and B in seat order).
      const plan = this.#laneDiscoveryPlan(round, winner.lane, winner.sha);
      for (const seat of this.#laneSeats()) {
        const entries = plan.filter((e) => e.seat === seat);
        if (entries.length === 0) continue;
        const err = this.#applyDiscoveries(
          entries.map((e) => e.disclosure),
          seat as Reviewer,
          { firstIndex: entries[0].index },
        );
        if (err) this.#log.append("error", { where: "lane_winner_discoveries", seat, error: err });
      }
      // The winner's checks already ran (under the machine-wide lock); its
      // record is applied, never re-run.
      this.#applyEvent({ type: "CHECKS_PASSED" });
      if (this.#itemsEnforced()) {
        const output = this.#laneCheckOutputs.get(winner.sha) ?? "";
        this.#applyEvent({ type: "ITEM_STATE_UPDATED", checkResolution: resolveTestVerifies(this.#planItems(), output) });
        this.#applyTestVerifyFindings(winner.sha);
        if (build?.coverage) {
          this.#applyEvent({
            type: "ITEM_STATE_UPDATED",
            coverage: build.coverage as Coverage,
            coverageAttempt: this.#state.phase.attempt.n,
          });
        }
      }
    } finally {
      this.#driveSuspended = false;
    }
    // The same bookkeeping #runFreeze does once a candidate exists: carry the
    // messages, raise each decision as a trade-off, note the coverage, sample
    // the boundary data.
    this.#carryMessages(winner.sha);
    for (const decision of assembled) {
      this.#raiseMessage("tradeoff", decision.id, this.#decisionContent(decision), winner.sha);
    }
    if (this.#itemsEnforced() && build?.coverage) this.#applyCoverageNotes(build.coverage as Coverage, winner.sha);
    this.#recordBoundaryDataAndSample(winner.sha);
    this.drive();
  }

  /** Promotes the round winner's recorded reviews into the phase's review
   * slots, once the phase has reached REVIEWING (the only state a
   * REVIEW_SUBMITTED row accepts). Called by `#runProbe` right after
   * PROBE_PASSED, with `drive()` suspended, so `next()` never dispatches a
   * second review for a seat the round already reviewed. */
  async #promoteLaneWinnerReviews(sha: string): Promise<void> {
    const pending = this.#pendingLaneWinner;
    if (!pending || pending.sha !== sha) return;
    this.#pendingLaneWinner = undefined;
    // The caller has `drive()` suspended: the winner's findings/ballots, its
    // three reviews and their statements all land before anything dispatches.
    // The order is the single-candidate path's own (see `#onSubmitChecked`):
    // findings/ballots first, then REVIEW_SUBMITTED (the third completes the
    // round), then the finding statements.
    for (const { review } of pending.reviews) {
      let error: string | undefined;
      try {
        error = await this.#applyReviewFindingsAndBallots(review);
      } catch (err) {
        error = `threw: ${String((err as Error)?.message ?? err)}`;
      }
      if (error) this.#log.append("error", { where: "lane_winner_review", error });
      this.#applyEvent({ type: "REVIEW_SUBMITTED", review, candidate: sha, promotedFrom: pending.round });
      try {
        this.#applyReviewFindingStatements(review);
      } catch (err) {
        this.#log.append("error", { where: "lane_winner_finding_statements", error: String((err as Error)?.message ?? err) });
      }
    }
  }

  async #runFreezeImpl(actionId: string): Promise<void> {
    this.#log.intent(actionId, { worktree: this.#paths.worktree });
    crashAt("before_freeze");
    const handle = this.#activeWorkerHandle;
    this.#activeWorkerHandle = undefined;
    let timedOut = false;

    const work = (async (): Promise<{ candidateSha: string; decisions: Decision[]; tainted: boolean } | undefined> => {
      if (handle) {
        // step 1 (quiesce): abort, wait for the agent to settle.
        await handle.agent.abort().catch(() => undefined);
        await handle.agent.waitSettled();
        // step: end the worker process group.
        await handle.agent.terminate();
        this.#agents.delete(handle.agentId);
      }
      if (timedOut) return undefined;

      // step 2: sweep.
      const sweepResult: SweepResult = await sweep(this.#paths.worktree, { ownPgids: this.#ownPgids(), exceptPids: [] });
      if (timedOut) return undefined;
      this.#log.append("sweep", sweepResult);

      // step 3: commit.
      const candidateSha = freezeCommit(this.#paths.worktree, actionId, `phase ${this.#state.phase.phaseId} candidate`);

      // step 4: materialize a read-only checkout for reviewers.
      const candidateDir = path.join(this.#paths.candidates, candidateSha);
      if (!fs.existsSync(candidateDir)) {
        materializeCandidate(this.#plan.repo, candidateSha, candidateDir);
      }

      // Item 6: a plain file naming the live candidate, for a test-only
      // scripted reviewer (spawned by a `tt start`-launched conductor, so
      // it has no in-process JS hook to be handed this directly) that can
      // read a file but not an env var set at its own process's spawn time
      // — see `#runReview`'s `TT_CANDIDATE_SHA` for the env-var route.
      fs.writeFileSync(path.join(this.#runDir, "candidate-sha.txt"), candidateSha);

      // Assemble + bind (round-of-review item 3): only now does a candidate
      // exist for these disclosures to be bound to.
      const decisions = this.#assembleDecisions(candidateSha);
      return { candidateSha, decisions, tainted: sweepResult.tainted };
    })();

    const deadline = cancelableTimeout(this.#deadlines.freezeMs, "timeout" as const);
    // design §8.1: "any freeze error must end in a logged failure, never an
    // in-flight entry with no completion" (work packet 2a fix — a real bug:
    // if `work` above threw synchronously, e.g. `freezeCommit` erroring on
    // a repair attempt, `Promise.race` itself rejected, which propagated
    // straight out of `#runFreeze` past every completion/event call below —
    // the dispatch call site's own `.catch` only logged an "error" record,
    // leaving the `freeze` action permanently in-flight with no
    // FREEZE_TIMED_OUT/FREEZE_COMPLETED ever emitted, so the phase could
    // never move again). A thrown `work` is now treated exactly like an
    // ordinary timeout: force-kill, sweep, taint, and FREEZE_TIMED_OUT —
    // the attempt fails and consumes a repair round instead of hanging.
    let outcome: { candidateSha: string; decisions: Decision[]; tainted: boolean } | undefined | "timeout" | "error";
    try {
      outcome = await Promise.race([work, deadline.promise]);
    } catch (err) {
      outcome = "error";
      this.#log.append("error", { where: "freeze", error: String((err as Error)?.message ?? err) });
    }
    deadline.cancel();

    if (outcome === "timeout" || outcome === "error") {
      timedOut = true;
      if (handle) {
        await handle.agent.terminate().catch(() => undefined);
        this.#agents.delete(handle.agentId);
      }
      const sweepResult: SweepResult = await sweep(this.#paths.worktree, { ownPgids: this.#ownPgids(), exceptPids: [] });
      this.#log.append("sweep", sweepResult);
      this.#log.completion(actionId, { timedOut: true, tainted: true, reason: outcome });
      this.#applyEvent({ type: "FREEZE_TIMED_OUT" });
      return;
    }

    if (!outcome) {
      // `work` itself noticed `timedOut` after the race above already
      // settled some other way — nothing left to do (the timeout branch
      // already logged/applied everything).
      return;
    }

    // design §9.3's "after the effect but before its completion event"
    // boundary: the commit (and the decision assembly) has already
    // happened; the completion record has not.
    crashAt("after_freeze");
    this.#log.completion(actionId, { candidateSha: outcome.candidateSha, tainted: outcome.tainted });
    // Plan 06b (OD-1 A2): FREEZE_COMPLETED resets the per-candidate item
    // record, so capture this candidate's coverage before applying it.
    const coverageAtFreeze = this.#state.phase.coverage;
    this.#applyEvent({
      type: "FREEZE_COMPLETED",
      candidateSha: outcome.candidateSha,
      decisions: outcome.decisions,
      tainted: outcome.tainted,
    });
    // Contract v1 §2: one explicit MESSAGE_CARRIED per live message, at every
    // freeze. The settlement carries to the new version exactly when the
    // content is unchanged and the contract version is the same; reduce()
    // marks it invalidated otherwise.
    this.#carryMessages(outcome.candidateSha);
    // Contract v1 §1: every worker decision is a trade-off message, published
    // for the owner immediately (this profile has no evaluator yet).
    for (const decision of outcome.decisions) {
      this.#raiseMessage("tradeoff", decision.id, this.#decisionContent(decision), outcome.candidateSha);
    }
    // Plan 06b: every partial/not_done coverage entry and every deviating
    // architecture item becomes a trade-off message, once the candidate
    // exists for it to bind to.
    if (this.#structured() && coverageAtFreeze) this.#applyCoverageNotes(coverageAtFreeze, outcome.candidateSha);
    // Work packet 2a: boundary triggers (design §3.3) and §3.5's sampling
    // data need a real candidate (for the diff, and for DECISION_ADDED's
    // own binding check) — only possible once FREEZE_COMPLETED above has
    // set phase.candidate.
    this.#recordBoundaryDataAndSample(outcome.candidateSha);
  }

  /** Contract v1: writes `messages.jsonl`, `ledger.jsonl` and the rendered
   * views (`views/review.org`, `views/messages/<id>.org`) from state. The
   * status view has its own one-second beat (`#statusTimer`). */
  #writeContractProjections(): void {
    try {
      const phase = this.#state.phase;
      fs.writeFileSync(this.#paths.messages, projectMessages(phase));
      fs.writeFileSync(this.#paths.ledger, projectLedger(phase));
      // Plan 05c: the header names the run by its readable id and directory
      // id, never by the internal runId. Plan 05j: the view is the entry
      // projection (one topic once), linted on every render.
      const ids = runIds(this.#runDir);
      const rendered = projectEntryReview({ ...phase, ...ids }, { anchorFreshness: candidateAnchorFreshness(this.#candidateDir()) });
      fs.writeFileSync(this.#paths.review, rendered.text);
      // Goal (4): the glossary a brief links to, written beside review.org so
      // the link resolves inside the run directory (finding M-25).
      fs.writeFileSync(path.join(path.dirname(this.#paths.review), "glossary.org"), `* Owner glossary\n${renderGlossaryOrg()}\n`);
      this.#writeEntryViews(rendered.files);
      this.#writeItemViews(rendered.itemFiles);
      this.#recordReviewLint(rendered.lint);
      this.#writeMessageViews();
    } catch (err) {
      this.#logUnexpected("write_contract_projections", err);
    }
  }

  #writeStatusViewSafe(): void {
    try {
      this.#writeStatusView();
    } catch (err) {
      this.#logUnexpected("write_status_view", err);
    }
  }

  /** Plan 06f (A2): keep this run's row in `<root>/live.json` current. The
   * conductor is one of the file's two writers (the scheduler is the other);
   * every write is atomic, and every other run's row and every waiting node
   * another writer recorded is preserved. A stopped conductor removes its own
   * row rather than leaving a stale `alive` entry behind. */
  #writeLive(): void {
    try {
      const root = path.dirname(this.#runDir);
      const id = path.basename(this.#runDir);
      const needsOwner = this.#state.phase.phase === "AWAITING_OWNER";
      // A closed run is neither alive nor waiting, so its row leaves. The one
      // exception is a run still AWAITING_OWNER: the goal counts "waiting for
      // the owner" as live even when its conductor has been stopped, so the
      // mode line keeps showing it until the owner acts (M's veto of
      // D-disc-M-24). Passing `id` explicitly is what makes the removal land.
      if (this.#closed && !needsOwner) {
        updateLiveRun(root, id, undefined);
        return;
      }
      updateLiveRun(root, id, {
        title: redactText(this.#plan.title, this.#secretMaskable),
        phase: this.#state.phase.phase,
        needsOwner,
      });
    } catch (err) {
      this.#logUnexpected("write_live", err);
    }
  }

  /** Plan 05j: one round's curator pass. It links every remaining message to
   * a shared-anchor entry (or opens its own), records `ENTRY_CURATED` so the
   * evaluators may start, and leaves the resulting links in the log. The
   * curator's allow-list (link/open/retitle only) is enforced by
   * `curate_entries`; the deterministic anchor pass is the same rule. */
  #curateRound(candidateSha: string): void {
    this.#syncEntries();
    if (this.#curatorInFlight.has(candidateSha)) return;
    this.#curatorInFlight.add(candidateSha);
    const actionId = this.#log.actionId("dispatch_curator");
    // The curator always runs (OD-2): with no configured model it launches
    // on Pi's default exactly like an evaluator would, never skipped.
    void this.#runCurator(actionId, candidateSha).catch((err) => {
      this.#logUnexpected("dispatch_curator", err);
      this.#finishCurator(candidateSha);
    });
  }

  /** Plan 05j: the round is curated (the curator submitted, timed out or
   * could not start): the evaluators may run. Idempotent. */
  #finishCurator(candidateSha: string): void {
    this.#curatorInFlight.delete(candidateSha);
    const p = this.#state.phase;
    if (p.phase !== "EVALUATING" || p.candidate?.sha !== candidateSha || p.curatedFor === candidateSha) return;
    this.#applyEvent({ type: "ENTRY_CURATED", candidateSha, count: (p.entries ?? []).length });
  }

  /** Plan 05j: a reviewer's `sameAs E-n` link. Applied through reduce, which
   * refuses (and logs) a link with no shared anchor. */
  #linkReviewerRaise(messageId: string, entryId: string, reviewer: Reviewer): void {
    const entry = (this.#state.phase.entries ?? []).find((e) => e.id === entryId);
    if (!entry) {
      this.#log.append("entry_link_ignored", { messageId, entryId, reviewer, reason: "no such entry" });
      return;
    }
    const before = (this.#state.phase.entries ?? []).find((e) => e.id === entryId)!.links.length;
    this.#applyEvent({ type: "MESSAGE_LINKED", messageId, entryId, anchor: entry.anchor, reason: `reviewer ${reviewer} sameAs`, by: `reviewer ${reviewer}` });
    const after = (this.#state.phase.entries ?? []).find((e) => e.id === entryId)!.links.length;
    if (after === before) {
      this.#log.append("entry_link_refused", { messageId, entryId, reviewer, reason: "no shared anchor" });
    }
  }

  /** Plan 05j: persist the entry ledger for every message that has none yet.
   * Called on start (backfilling an old log) and on every message event, so
   * the entries are in `events.jsonl`, not invented at render time. */
  #syncEntries(): void {
    if (this.#syncingEntries) return;
    const events = planEntryEvents(this.#state.phase.messages ?? [], this.#state.phase.entries ?? []);
    if (events.length === 0) return;
    this.#syncingEntries = true;
    try {
      for (const event of events) this.#applyEvent(event as never);
    } catch (err) {
      this.#logUnexpected("sync_entries", err);
    } finally {
      this.#syncingEntries = false;
    }
  }

  /** Plan 05j: `views/entries/<id>.org`, one per live entry, pruned of files
   * whose entry is no longer live (a resolved or merged entry is not shown). */
  #writeEntryViews(files: Array<{ id: string; contents: string }>): void {
    const ids = new Set(files.map((f) => f.id));
    fs.mkdirSync(this.#paths.entriesView, { recursive: true });
    for (const f of files) fs.writeFileSync(path.join(this.#paths.entriesView, `${f.id}.org`), f.contents);
    for (const name of fs.readdirSync(this.#paths.entriesView)) {
      if (name.endsWith(".org") && !ids.has(name.slice(0, -4))) {
        fs.rmSync(path.join(this.#paths.entriesView, name), { force: true });
      }
    }
  }

  /** Plan 06b: one `views/items/<id>.org` per item, the evidence a matrix
   * cell opens. */
  #writeItemViews(files: Array<{ id: string; contents: string }>): void {
    const ids = new Set(files.map((f) => f.id));
    fs.mkdirSync(this.#paths.itemsView, { recursive: true });
    for (const f of files) fs.writeFileSync(path.join(this.#paths.itemsView, `${f.id}.org`), f.contents);
    for (const name of fs.readdirSync(this.#paths.itemsView)) {
      if (name.endsWith(".org") && !ids.has(name.slice(0, -4))) {
        fs.rmSync(path.join(this.#paths.itemsView, name), { force: true });
      }
    }
  }

  /** Plan 05j: `REVIEW_LINT_FAILED` records a violation the view's first line
   * already names. One event per distinct violation, so a steady violation is
   * not logged on every render beat. */
  #recordReviewLint(lint: { ok: boolean; violations: Array<{ rule: string; detail: string }> }): void {
    const signature = lint.violations.map((v) => `${v.rule}:${v.detail}`).join("|");
    if (signature === this.#lastLintSignature) return;
    this.#lastLintSignature = signature;
    // Dedup against the LOG, not just memory (finding M-23): after a restart
    // the same violation would otherwise be appended again.
    let existing = "";
    try {
      existing = fs.readFileSync(this.#paths.events, "utf8");
    } catch {
      // no log yet: every violation is new
    }
    for (const v of lint.violations) {
      if (existing.includes(JSON.stringify(v.detail))) continue;
      try {
        this.#applyEvent({ type: "REVIEW_LINT_FAILED", rule: v.rule, detail: v.detail });
      } catch (err) {
        this.#logUnexpected("review_lint_event", err);
      }
    }
  }

  /** Plan 03b: `views/messages/<id>.org`, one per message, pruned of files
   * whose message no longer exists (a superseded id is never resurrected). */
  #writeMessageViews(): void {
    const files = reviewMessageFiles(this.#state.phase);
    const ids = new Set(files.map((f) => f.id));
    fs.mkdirSync(this.#paths.messagesView, { recursive: true });
    for (const f of files) fs.writeFileSync(path.join(this.#paths.messagesView, `${f.id}.org`), f.contents);
    for (const name of fs.readdirSync(this.#paths.messagesView)) {
      if (name.endsWith(".org") && !ids.has(name.slice(0, -4))) {
        fs.rmSync(path.join(this.#paths.messagesView, name), { force: true });
      }
    }
  }

  /** Plan 03b: `views/status.txt`, the status buffer's own text (title, run
   * line, rows, trade-offs with their record ids, owner input, checklist),
   * so Emacs reads a file instead of calling `tt state`. */
  #writeStatusView(): void {
    const view = buildView(this.#runDir, this.#plan, !this.#closed);
    const text = renderStatusView(
      statusViewInput({
        runDir: this.#runDir,
        plan: this.#plan,
        state: this.#state,
        view,
        alive: !this.#closed,
        secrets: { missing: this.#missingSecrets, tooShort: this.#tooShortSecrets },
        held: loggedHeld(this.#runDir),
      }),
    );
    fs.writeFileSync(this.#paths.status, redactText(text, this.#secretMaskable));
    // Plan 04c: the same beat keeps `views/metrics.json` current. `buildView`
    // already built the metrics from this beat's one log snapshot, so writing
    // them here reuses it instead of parsing the whole log a second time.
    fs.writeFileSync(this.#paths.metrics, projectMetrics(view.metrics));
    // Plan 03c: the same beat keeps the phase chart (`views/loop.txt`) current;
    // it is generated from TRANSITIONS, so it can never drift from the loop.
    const stats = statsFromTimeline(view.timeline, new Date());
    // Plan 03c: each dispatching state shows its own role's model — the
    // injected provider/model when a caller set one, otherwise Pi's own
    // default from settings.json, otherwise `default`.
    const modelFor = (role: Role, seat?: string | number): string | undefined => this.#providerModelFor?.(role, seat)?.model ?? piDefaultModel();
    // The evaluator and the panel (which shares the EVALUATING box) have a
    // model source now (#+TT_MODELS), so each reads its own instead of M-9's
    // placeholder `default`. The reviewer and panel seats resolve exactly as
    // their launch sites do, so the chart never names a model a seat did not
    // run on.
    const models = {
      worker: modelFor("worker"),
      reviewer: modelFor("reviewer"),
      evaluator: modelFor("evaluator"),
      panel: modelFor("panel"),
      reviewerSeats: { M: modelFor("reviewer", "M"), A: modelFor("reviewer", "A"), B: modelFor("reviewer", "B") },
      panelSeats: { "1": modelFor("panel", 1), "2": modelFor("panel", 2), "3": modelFor("panel", 3) },
    };
    // Plan 06g (A5): the chart names the round's lanes and its pick when the
    // phase has any (a plan without `#+TT_WORKERS` passes none, and the
    // chart is byte-identical to before).
    const roundLines = lanesView(this.#state.phase);
    fs.writeFileSync(
      this.#paths.loop,
      redactText(renderPhaseChart(undefined, { stats, models, seats: this.#laneSeats(), ...(roundLines.length > 0 ? { roundLines } : {}) }), this.#secretMaskable),
    );
    // Plan 05h: the same beat keeps the loop tape (`views/tape.txt`) current.
    // `buildView` already built it from this beat's one log snapshot, so it
    // is written here rather than rebuilt; its durations end at the log's own
    // last timestamp, so `tt contract rebuild` writes the same bytes.
    fs.writeFileSync(this.#paths.tape, redactText(view.tape, this.#secretMaskable));
  }

  /** Contract v1: the reviewable content of the message a worker decision
   * raises. Re-derived at every freeze, so a decision the worker changed
   * produces a new contentHash (and a changed carry). */
  #decisionContent(decision: Decision): MessageContent {
    return {
      type: "tradeoff",
      title: decision.choice,
      summary: decision.recommendation.reason,
      context: decision.whyItMatters,
      evidence: decision.alternatives.map((a) => `${a.option}: ${a.consequence}`),
      // No fabricated planRef: the phase id is not a plan clause, and using it
      // as one made unrelated prose messages share an anchor (finding A-34).
    };
  }

  /** Contract v1: the reviewable content of the message a finding raises.
   * `type' is the MESSAGE type the caller will raise (`blocker' only through
   * a reviewer's `blockers' list; otherwise `finding'), so the message's type
   * and its content hash never disagree. */
  #findingContent(finding: Finding, type: MessageType): MessageContent {
    return {
      type,
      title: `${finding.kind} ${finding.severity}: ${finding.evidence}`,
      summary: `raised by ${finding.raisedBy} against ${finding.boundCandidateSha}`,
      context: finding.evidence,
      evidence: [finding.evidence],
      // No fabricated planRef (finding A-34): a prose-evidence finding has no
      // real anchor, so it gets its own entry, never merged with another.
    };
  }

  /** Plan 03b: who raised a message and how much it matters, so the runtime
   * renderer can colour and group it without a second lookup. Derived from
   * the record the message was raised from. */
  #messageProvenance(type: MessageType, sourceRecordId: string): { raisedBy: string; importance: "high" | "normal" | "low" } {
    if (type === "tradeoff") {
      const d = this.#state.phase.decisions.find((x) => x.id === sourceRecordId);
      if (d?.source === "reviewer-discovered") {
        return { raisedBy: d.alsoSeenBy?.[0] ? `reviewer ${d.alsoSeenBy[0]}` : "reviewer", importance: d.class === "reserved" ? "high" : d.class === "detail" ? "low" : "normal" };
      }
      return { raisedBy: "worker", importance: d?.class === "reserved" ? "high" : d?.class === "detail" ? "low" : "normal" };
    }
    const f = this.#state.phase.findings.find((x) => x.id === sourceRecordId);
    // A blocking finding matters as much as a blocker; an advisory one is low.
    return { raisedBy: f?.raisedBy ?? "reviewer", importance: type === "blocker" || f?.severity === "blocking" ? "high" : "low" };
  }

  /** The current content of the record a message was raised from, or
   * undefined when that record is gone. */
  #currentContentFor(message: Message): MessageContent | undefined {
    if (!message.sourceRecordId) return undefined;
    const decision = this.#state.phase.decisions.find((d) => d.id === message.sourceRecordId);
    if (decision && message.type === "tradeoff") return this.#decisionContent(decision);
    const finding = this.#state.phase.findings.find((f) => f.id === message.sourceRecordId);
    if (finding && (message.type === "finding" || message.type === "blocker")) return this.#findingContent(finding, message.type);
    return undefined;
  }

  /** Whether the record a message came from is still live. A decision the
   * worker withdrew or did not carry forward is superseded (predicate.ts's
   * isLiveDecision), so its message must be superseded too — never carried
   * with its old settlement intact. */
  #backingStatus(message: Message): "live" | "superseded" | "gone" {
    // A free-standing `raise_tradeoff` has no backing record to lose: it is
    // its own message and stays live (plan 04a).
    if (!message.sourceRecordId) return "live";
    const decision = this.#state.phase.decisions.find((d) => d.id === message.sourceRecordId);
    if (decision && message.type === "tradeoff") return isLiveDecision(decision) ? "live" : "superseded";
    const finding = this.#state.phase.findings.find((f) => f.id === message.sourceRecordId);
    if (finding && (message.type === "finding" || message.type === "blocker")) return "live";
    return "gone";
  }

  /** Plan 04a: raises a raw message. The 03a compatibility producer no
   * longer publishes directly — the evaluator does, in EVALUATING. Deduped
   * by the source record id when there is one (a re-freeze or a re-review
   * never raises the same message twice); a `raise_tradeoff` has none, so
   * each call is its own message. Returns the new message's id. */
  #raiseMessage(
    type: MessageType,
    sourceRecordId: string | undefined,
    content: MessageContent,
    candidateSha: string,
    opts: { anchor?: Message["anchor"]; raisedAsBlocker?: boolean; closes?: string } = {},
  ): string {
    const existing = this.#state.phase.messages ?? [];
    if (sourceRecordId !== undefined) {
      const prior = existing.find((m) => m.sourceRecordId === sourceRecordId);
      if (prior) return prior.id;
    }
    const letter = type === "tradeoff" ? "T" : type === "finding" ? "F" : "B";
    let n = existing.filter((m) => m.type === type).length + 1;
    while (existing.some((m) => m.id === `${letter}-${n}`)) n += 1;
    const message: Message = {
      id: `${letter}-${n}`,
      phaseId: this.#state.phase.phaseId,
      type,
      ...content,
      ...(opts.anchor ? { anchor: opts.anchor } : {}),
      ...(opts.closes ? { closes: opts.closes } : {}),
      ...(opts.raisedAsBlocker ? { raisedAsBlocker: true } : {}),
      state: "raw",
      messageVersion: 1,
      boundCandidateSha: candidateSha,
      boundContractVersion: this.#state.phase.contract.contractVersion,
      contentHash: contentHashOf({ type, ...content }),
      ...(sourceRecordId !== undefined ? { sourceRecordId } : {}),
      ...this.#messageProvenance(type, sourceRecordId),
      // The record-derived hash a later carry compares against, so an
      // evaluator-rewritten title is not mistaken for a changed decision.
      sourceContentHash: contentHashOf({ type, ...content }),
    };
    this.#applyEvent({ type: "MESSAGE_RAISED", message });
    return message.id;
  }

  /** Contract v1 §2: one MESSAGE_CARRIED per live message at a freeze. The
   * content is re-derived from the underlying record, so a decision the
   * worker changed this round is carried as CHANGED (with its new content)
   * and the previous settlement is invalidated, rather than asserted
   * unchanged. */
  #carryMessages(toCandidate: string): void {
    for (const message of this.#state.phase.messages ?? []) {
      if (message.state === "superseded" || message.state === "resolved") continue;
      if (message.boundCandidateSha === toCandidate) continue;
      // A message whose backing decision was withdrawn or not carried forward
      // is superseded: its settlement (if any) stays in the ledger, marked,
      // but it can never survive as an active settlement.
      if (this.#backingStatus(message) === "superseded") {
        this.#applyEvent({
          type: "MESSAGE_SUPERSEDED",
          messageId: message.id,
          reason: `its record ${message.sourceRecordId} was superseded`,
          boundCandidateSha: message.boundCandidateSha,
          boundContractVersion: message.boundContractVersion,
          boundRecordVersion: message.messageVersion,
        });
        continue;
      }
      const current = this.#currentContentFor(message);
      // Compare the RECORD's content against the hash it had when the message
      // was raised (sourceContentHash), not against the message's visible
      // contentHash: the evaluator may have rewritten the title, and that must
      // not read as "the record changed" (finding F-A-5).
      const sourceHash = message.sourceContentHash ?? message.contentHash;
      const currentHash = current ? contentHashOf(current) : sourceHash;
      const unchanged = !current || currentHash === sourceHash;
      const contentHash = unchanged ? message.contentHash : currentHash;
      this.#applyEvent({
        type: "MESSAGE_CARRIED",
        messageId: message.id,
        fromCandidate: message.boundCandidateSha,
        toCandidate,
        fromVersion: message.messageVersion,
        toVersion: message.messageVersion + 1,
        contentHash,
        unchanged,
        ...(current ? { sourceContentHash: currentHash } : {}),
        ...(unchanged ? {} : { content: current! }),
      });
    }
  }

  // -- checks ---------------------------------------------------------------

  /** Records one gate command's evidence: its command string, the identity
   * it ran against (the enclosing directory name) and its outcome, in the
   * same `<checks>/<sha>/<sanitized command>.log` shape the C path has
   * always used. Called for every executed command in both gates, so the
   * candidate C and the probed integration I each get one file per
   * command. */
  #recordCheck(outDir: string, command: string, result: RunCommandResult): void {
    fs.mkdirSync(outDir, { recursive: true });
    // Plan 01a: a check or probe command (and its output) may carry a secret
    // value — a vendor key a command echoes, or the plan's own check line.
    fs.writeFileSync(
      path.join(outDir, this.#checkLogName(command)),
      redactText(
        `$ ${command}\n${result.output}\nexit ${result.exitCode} signal ${result.signal}${result.timedOut ? " (timed out)" : ""}\n`,
        this.#secretMaskable,
      ),
    );
  }

  /** Plan 06c: write `checks/<sha>/record.json` — the candidate's check
   * record, naming its tier, the commands it ran, the final command when
   * there was one, and the machine's load at the run. A `final` run
   * overwrites the candidate's `round` record, so the accepted candidate's
   * record is the one that ran the final check. */
  #writeCheckRecord(outDir: string, record: CheckRecord): void {
    try {
      // The commands ran with plan secrets resolved; on disk they keep the
      // mask every other run file keeps.
      const masked: CheckRecord = {
        ...record,
        commands: record.commands.map((c) => ({ ...c, command: redactText(c.command, this.#secretMaskable) })),
        finalCommands: record.finalCommands.map((c) => redactText(c, this.#secretMaskable)),
      };
      fs.writeFileSync(path.join(outDir, "record.json"), `${JSON.stringify(masked, null, 2)}\n`);
    } catch (err) {
      this.#log.append("error", { where: "check_record", error: String((err as Error)?.message ?? err) });
    }
  }

  /** Plan 06c (A3): the passing check record a parent node's accepted
   * candidate left for exactly this base commit, if one exists. The child
   * node's base IS that candidate, so its checks already passed there; the
   * record is adopted as the child's baseline and nothing runs. The program's
   * own event log names every sibling run, so this never needs a path the
   * scheduler did not record. */
  #candidateRecordBaseline(commands: readonly string[], key: string): { record: Baseline; sourceDir: string } | undefined {
    // Plan 06c (A3): the SCHEDULER chose this reuse when it started this node
    // and recorded the parent's accepted candidate and run in the node's
    // `program.json`. The conductor only consumes the given path; it never
    // scans sibling runs to decide a baseline itself.
    let reuse: { fromRunId?: unknown; fromCandidateSha?: unknown } | undefined;
    try {
      const info = JSON.parse(fs.readFileSync(path.join(this.#runDir, "program.json"), "utf8")) as {
        baselineReuse?: { fromRunId?: unknown; fromCandidateSha?: unknown };
      };
      reuse = info.baselineReuse;
    } catch {
      return undefined;
    }
    if (!reuse || typeof reuse.fromRunId !== "string" || typeof reuse.fromCandidateSha !== "string") return undefined;
    const baseSha = reuse.fromCandidateSha;
    const dir = path.join(path.dirname(this.#runDir), reuse.fromRunId, "checks", baseSha);
    let record: CheckRecord | undefined;
    try {
      record = parseCheckRecord(JSON.parse(fs.readFileSync(path.join(dir, "record.json"), "utf8")));
    } catch {
      record = undefined;
    }
    if (!record || !record.passed) return undefined;
    // The record's final command (if any) is not part of the child's own
    // check list; the phase's checks are.
    const runCommands = record.commands.filter((c) => !record.finalCommands.includes(c.command));
    if (runCommands.length !== commands.length) return undefined;
    if (!runCommands.every((c, i) => c.command === this.#baselineCommandName(commands[i]))) return undefined;
    const baseline: Baseline = {
      baseSha,
      tree: this.#baselineTree(),
      key,
      at: record.at,
      commands: runCommands.map((c) => ({
        command: c.command,
        exitCode: c.exitCode,
        signal: c.signal ?? null,
        timedOut: c.timedOut,
        durationMs: c.durationMs,
        failures: [],
        ...(c.log ? { log: c.log } : {}),
      })),
      failures: [],
    };
    return { record: baseline, sourceDir: dir };
  }

  /** The log file name a check command's evidence gets under its gate's
   * directory — shared by `#recordCheck` and the baseline record, which names
   * the same file. */
  #checkLogName(command: string): string {
    return `${sanitize(redactText(command, this.#secretMaskable))}.log`;
  }

  /** Plan 01a: the run's plan snapshot on disk is written redacted, and
   * `tt start`'s detached conductor is built by re-reading that snapshot
   * (cli.ts's `runConductorProcess`), so the plan's own text can hold the mask.
   * A command the PLAN asks for must still run as its author wrote it — the
   * values are in this process's environment, which is the only place they may
   * be read from — so the mask is exchanged for the real value here, in memory,
   * at the moment the command is handed to the shell.
   *
   * Nothing is written back: the check log records the masked command
   * (`#recordCheck`), the snapshot keeps the mask, and prompts are redacted at
   * their own choke point. This is not a general "unmask": it is applied to
   * plan-authored commands only, never to agent-supplied text, which must use
   * `$NAME` and is refused when it carries a value. */
  #withValues(text: string): string {
    let out = text;
    for (const s of this.#secretMaskable) {
      if (!out.includes(`***${s.name}***`)) continue;
      out = out.split(`***${s.name}***`).join(s.value);
    }
    return out;
  }

  // -- plan 05i: environment preflight --------------------------------------

  /** Every shell command the phase declares, resolved for secrets: its
   * effective checks, its `:GATE:` command and its `:GATE_CLEANUP:`. This is
   * exactly the set the preflight resolves before a baseline or any agent. */
  #preflightCommands(): string[] {
    const commands = this.#resolvedEffectiveChecks();
    const gate = gateCommandOf(this.#state.phase.contract);
    if (gate) commands.push(this.#withValues(gate));
    const cleanup = this.#state.phase.contract.gateCleanup;
    if (cleanup && cleanup.trim().length > 0) commands.push(this.#withValues(cleanup));
    return commands;
  }

  /** Plan 06c (R6): the tools the preflight did not find on this machine,
   * named in every agent prompt so the worker reaches for an alternative
   * instead of a tool the environment lacks. Resolved once per conductor
   * (the PATH does not change under a running conductor). */
  #agentToolLines(): string[] {
    if (this.#agentToolsMissing === undefined) {
      const names = ["timeout", "gtimeout", "rg", "jq", "docker", "cargo"];
      this.#agentToolsMissing = names.filter((n) => this.#resolveExecutable(n) === undefined);
    }
    if (this.#agentToolsMissing.length === 0) return [];
    return [
      "",
      `Tools the environment preflight did not find on this machine (use an available alternative): ${this.#agentToolsMissing.join(", ")}.`,
    ];
  }

  /** Plan 06c (A5): every agent prompt names the tools the preflight did not
   * find. `buildWorkerPrompt` takes them directly; every other prompt is
   * wrapped here so no role is left uninformed. */
  #agentPrompt(text: string): string {
    const lines = this.#agentToolLines();
    return lines.length > 0 ? `${text}\n${lines.join("\n")}` : text;
  }

  /** Resolve one executable in the conductor's own environment with
   * `command -v`, exactly as the shell that runs the checks would. Runs in
   * the repo (so a relative `./script` resolves against it) and never throws:
   * a non-zero `command -v` is a missing tool, not an error. */
  #resolveExecutable(name: string): string | undefined {
    // `command -v` does not expand a tilde, so do it here: a plan-authored
    // `~/bin/tool` names a real path A-3 asks us to resolve.
    const target = name.startsWith("~/") ? path.join(os.homedir(), name.slice(2)) : name;
    try {
      const out = execFileSync("/bin/sh", ["-c", 'command -v -- "$1"', "tt-env-preflight", target], {
        encoding: "utf8",
        env: this.#preflightEnv,
        cwd: this.#plan.repo,
        stdio: ["ignore", "pipe", "ignore"],
      }).trim();
      return out.length > 0 ? out.split("\n")[0] : undefined;
    } catch {
      return undefined;
    }
  }

  /** Plan 05i: the one gate that runs at `start()`, before the baseline and
   * before any agent. It resolves every declared executable, records the
   * resolved paths (`ENV_CHECKED`), and either applies `ENV_PREFLIGHT_FAILED`
   * (a missing tool — the run stops in `ENV_BLOCKED`) or unblocks a run whose
   * tools now resolve (`RUN_RESUMED`, so its preserved phase continues). The
   * event application is drive-suspended so nothing is dispatched between the
   * resolution and the block decision. Returns false when the run is blocked. */
  #envPreflightGate(): boolean {
    const commands = this.#preflightCommands();
    const result = envPreflight(commands, {
      path: this.#preflightEnv.PATH ?? "",
      resolve: (name) => this.#resolveExecutable(name),
    });
    // A terminal phase has no baseline to take and no agent to launch, so a
    // missing tool must not flip a DONE/BLOCKED run to ENV_BLOCKED (findings
    // M-7 / disc-A-26). The tools are still recorded below.
    const terminal = this.#state.phase.phase === "DONE" || this.#state.phase.phase === "BLOCKED";
    this.#driveSuspended = true;
    this.#envGateActive = true;
    try {
      // Unblock BEFORE recording the tools: an `ENV_CHECKED` applied while the
      // run is still ENV_BLOCKED would otherwise be the state the auto-stop
      // sees, freezing the log before `RUN_RESUMED` could be applied.
      if (result.missing.length === 0 && this.#state.run === "ENV_BLOCKED") {
        this.#applyEvent({ type: "RUN_RESUMED" });
      }
      // No declared command means no tool to record; skip the (otherwise
      // empty) record so an unchanged run's log stays quiet.
      if (result.tools.length > 0) {
        this.#applyEvent({ type: "ENV_CHECKED", path: result.path, tools: result.tools, at: new Date().toISOString() });
      }
      if (result.missing.length > 0 && !terminal) {
        this.#applyEvent({ type: "ENV_PREFLIGHT_FAILED", missing: result.missing, path: result.path, at: new Date().toISOString() });
      }
    } finally {
      this.#envGateActive = false;
      this.#driveSuspended = false;
    }
    return result.missing.length === 0 || terminal;
  }

  /** Plan 05i: a 126/127 exit is an environment failure wherever a command
   * runs. The event moves the run to `ENV_BLOCKED` with the command and the
   * log tail; the caller must return without applying the stage's own
   * failure event, so it is never a baseline, a `checks failed`, a repair or
   * a finding. */
  #applyEnvFailure(stage: "baseline" | "checks" | "probe" | "gate", command: string, result: RunCommandResult): void {
    this.#applyEvent({
      type: "ENV_CHECK_FAILED",
      stage,
      command,
      exitCode: result.exitCode,
      tail: lastLines(result.output, 60),
      at: new Date().toISOString(),
    });
  }

  /** Plan 05d / finding #33: two hello timeouts in a row are an agent
   * environment problem, reported like 05i's preflight: the run blocks in
   * ENV_BLOCKED, the attempt consumes no repair round, and a passing `tt
   * resume` re-dispatches the same attempt. */
  #applyAgentEnvFailure(what: string): void {
    this.#applyEvent({
      type: "ENV_CHECK_FAILED",
      stage: "worker",
      command: what,
      exitCode: null,
      tail: `${what}: no hello after the retry — the agent environment is unhealthy`,
      at: new Date().toISOString(),
    });
  }

  // -- plan 01e: the base baseline -----------------------------------------

  /** Plan 01e: the base baseline's own directory (`<run>/checks/base/`), the
   * sibling of the per-candidate `<run>/checks/<sha>/` dirs. */
  #baselineDir(): string {
    return path.join(this.#paths.checks, "base");
  }

  #baselinePath(): string {
    return path.join(this.#baselineDir(), "baseline.json");
  }

  /** Plan 01e: the phase's current base commit — what a check's failures are
   * compared against under D2. */
  #baselineBaseSha(): string {
    return this.#state.phase.integrationHead;
  }

  /** The effective check list as it will actually be run (plan secrets
   * resolved), which is both what the baseline records and what the C gate
   * and the probe execute. */
  #resolvedEffectiveChecks(): string[] {
    // Plan 06c (A1/C2): the round list comes from the one chooser too.
    return checkCommands(this.#plan.checks, this.#state.phase.contract.checks, this.#state.phase.contract.finalChecks, "round").map((c) => this.#withValues(c));
  }

  #baselineKey(commands: readonly string[]): string {
    return baselineKey(this.#baselineTree(), commands);
  }

  /** The base commit's full tree object id (or the commit id when git cannot
   * read it) — the identity two nodes must share to reuse one baseline. */
  #baselineTree(): string {
    const baseSha = this.#baselineBaseSha();
    return treeOf(this.#plan.repo, baseSha) ?? baseSha;
  }

  /** The run-local baseline record, if one exists (and only then). */
  #readBaseline(): Baseline | undefined {
    try {
      return parseBaseline(JSON.parse(fs.readFileSync(this.#baselinePath(), "utf8")));
    } catch {
      return undefined;
    }
  }

  /** The texts that make a test this phase's job: its goal, its acceptance
   * items and the owner's directives in force. A failing test named in any of
   * them is never excused as pre-existing (`classifyCheckFailure`). */
  #requiredTestTexts(): string[] {
    const phase = this.#state.phase;
    return [
      phase.contract.goal,
      ...phase.contract.acceptance,
      ...(phase.ownerDirectives ?? []).filter((d) => d.status === "in-force").map((d) => d.text),
    ];
  }

  /** Plan 01e: the base commands whose pre-existing failures the worker and
   * the reviewers are told about — the ones whose failures the gate would
   * actually excuse, so the promise in the prompt and the rule at the gate are
   * the same rule. Empty when the base passed, when no baseline was taken, when
   * the record no longer covers this base, or when its failing output held no
   * parsable names (the strict rule then applies, and there is nothing honest
   * to call pre-existing). */
  #baselineFailedCommands(): BaselineCommand[] {
    const commands = this.#resolvedEffectiveChecks();
    const baseline = this.#readBaseline();
    if (!baseline || !this.#baselineCovers(baseline, this.#baselineKey(commands), commands)) return [];
    // A test this phase is required to fix is never called pre-existing,
    // here or at the gate (`classifyCheckFailure`): the prompt must not tell
    // the worker to leave it failing.
    const required = this.#requiredTestTexts();
    return baselineFailedCommands(
      baseline.commands.map((c) => ({ ...c, failures: (c.failures ?? []).filter((name) => !testIsNamedIn(name, required)) })),
    );
  }

  /** Plan 01e: the base's failing names for one check command, or none when
   * no baseline covers this command (D2 then falls back to the strict rule). */
  #baseFailuresFor(command: string): string[] {
    const commands = this.#resolvedEffectiveChecks();
    const baseline = this.#readBaseline();
    if (!baseline || !this.#baselineCovers(baseline, this.#baselineKey(commands), commands)) return [];
    const name = this.#baselineCommandName(command);
    const entry = baseline.commands.find((c) => c.command === name);
    // The record should never carry names for a command that did not fail
    // normally (`#runBaseline` does not write them), but a hand-written or
    // older record is still checked here: only a completed non-zero exit may
    // excuse a candidate failure (finding M-2).
    return entry && failedNormally(entry) ? entry.failures : [];
  }

  /** Plan 01e: the baseline the phase shares with the rest of its program
   * (`<program>/baselines/<key>/`), or undefined for a hand-started run.
   * Keyed by base tree plus check list, so two nodes that start from the same
   * tree run one baseline between them, and a node with different checks
   * never reuses another's. */
  #baselineSharedDir(key: string): string | undefined {
    const programDir = this.#programDir();
    return programDir ? path.join(programDir, "baselines", key) : undefined;
  }

  /** The masked form a baseline record is written with: its commands are
   * resolved (plan secrets put back) when they run, and on disk they keep the
   * mask every other run file keeps. */
  #baselineForDisk(record: Baseline): Baseline {
    return redactRecord(record, this.#secretMaskable) as Baseline;
  }

  /** The command as a baseline record names it — masked the same way the
   * record was written, so a lookup and a reuse check compare like forms. */
  #baselineCommandName(command: string): string {
    return redactText(command, this.#secretMaskable);
  }

  /** True iff `record` was taken over this exact base — the same **full** tree
   * (never the key's shortened prefix, which is only a file name) and exactly
   * these commands, compared in the masked form the record is stored in. This
   * is the guard that keeps a stale record, another base's record, or a
   * whole-program key collision from ever hiding a new failure. */
  #baselineCovers(record: Baseline, key: string, commands: readonly string[]): boolean {
    if (record.key !== key || record.tree !== this.#baselineTree()) return false;
    if (record.commands.length !== commands.length) return false;
    // Plan 05i: a record whose commands exited 126/127 describes an
    // environment that could not run them, not a base that fails its own
    // tests (a record from before this change, or a sibling's shared copy).
    // It is ignored so the baseline re-runs and the strict rule applies.
    if (baselineHasEnvironmentFailure(record)) return false;
    return record.commands.every((c, i) => c.command === this.#baselineCommandName(commands[i]));
  }

  /** Plan 04a: whether the BASELINE state must run. True when the phase has
   * effective checks and its own run-local baseline does not yet cover this
   * base tree and check list. A false answer sends READY straight to
   * IMPLEMENTING; reduce() itself cannot see the disk, so the conductor
   * decides.
   *
   * A missing run-local record whose identical base tree already has a
   * record elsewhere (a sibling program node) is adopted HERE, synchronously
   * — copied into this run's `checks/base/`, running nothing — and the state
   * is skipped for it too (finding M-12: the docstring must match the code).
   * Only a record this run does not hold and cannot adopt makes the state
   * run. */
  #baselineNeeded(): boolean {
    const commands = this.#resolvedEffectiveChecks();
    if (commands.length === 0) return false;
    const key = this.#baselineKey(commands);
    const local = this.#readBaseline();
    if (local && this.#baselineCovers(local, key, commands)) return false;
    // Plan 01e's reuse rule names a sibling program node's identical-base
    // record too: adopt it here (copying it into this run's own checks/base/)
    // so READY skips the state entirely, while the candidate's checks still
    // have a local record to lean on (finding M-7).
    const shared = this.#baselineSharedDir(key);
    if (shared) {
      const sibling = this.#readBaselineFrom(path.join(shared, "baseline.json"));
      if (sibling && this.#baselineCovers(sibling, key, commands)) {
        this.#adoptBaseline(sibling, shared, "program");
        return false;
      }
    }
    // Plan 06c (A3): a base that IS a parent node's accepted candidate is
    // adopted by `#ensureBaseline` (no command runs, BASELINE_COMPLETED is
    // recorded with `reusedFrom`), so the stage still runs here.
    return true;
  }

  /** Plan 04a: the BASELINE stage's own logged action. It runs the base
   * baseline (or adopts one a sibling already paid for) under its own
   * deadline; whatever the outcome it completes into IMPLEMENTING and the
   * strict rule applies to a candidate whose baseline could not be taken. */
  async #runBaselineStage(actionId: string): Promise<void> {
    const commands = this.#resolvedEffectiveChecks();
    this.#log.intent(actionId, { commands });
    crashAt("before_run_baseline");
    // The plan names a flat `checkMs` for this stage (disc-A-59): the
    // observable limit must match the configured one.
    const timer = cancelableTimeout(this.#deadlines.checkMs, "timeout" as const);
    let outcome: "done" | "timeout";
    this.#baselineActionId = actionId;
    this.#baselineTimedOut = false;
    this.#baselineReusedFrom = undefined;
    try {
      outcome = await Promise.race([this.#ensureBaseline().then(() => "done" as const), timer.promise]);
    } catch (err) {
      // Plan 05i: a 126/127 during the baseline is an environment failure.
      // No record was written (`#runBaselineOnce` rethrows it), so block the
      // run here instead of completing the stage into IMPLEMENTING.
      if (err instanceof EnvFailure) {
        timer.cancel();
        this.#log.completion(actionId, { outcome: "environment", command: err.command, exitCode: err.exitCode });
        this.#applyEvent({
          type: "ENV_CHECK_FAILED",
          stage: "baseline",
          command: err.command,
          exitCode: err.exitCode,
          tail: err.tail,
          at: new Date().toISOString(),
        });
        return;
      }
      // `#ensureBaseline` is best-effort and never throws, but a bug in it
      // must still let the phase move on rather than wedge the run.
      this.#log.append("error", { where: "baseline", error: String((err as Error)?.message ?? err) });
      outcome = "done";
    } finally {
      this.#baselineActionId = undefined;
    }
    timer.cancel();
    if (outcome === "timeout") {
      // B-24: kill the baseline command's own group (recorded as
      // baseline-sh-<actionId>-<pgid>) and mark the stage timed out, so a
      // slow suite cannot keep running and its late record is ignored.
      this.#baselineTimedOut = true;
      for (const rec of readLog(this.#paths.events).records) {
        if (rec.kind !== "intent" || typeof rec.actionId !== "string") continue;
        if (!rec.actionId.startsWith(`baseline-sh-${actionId}-`)) continue;
        const pgid = (rec.event as { pgid?: number }).pgid;
        if (typeof pgid === "number") await killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined);
      }
    }
    crashAt("after_run_baseline");
    this.#log.completion(actionId, { outcome: outcome === "done" ? "completed" : "timed out" });
    if (outcome === "done" && this.#baselineReusedFrom) {
      this.#applyEvent({ type: "BASELINE_COMPLETED", reusedFrom: this.#baselineReusedFrom });
    } else {
      this.#applyEvent({ type: outcome === "done" ? "BASELINE_COMPLETED" : "BASELINE_TIMED_OUT" });
    }
  }

  /** Plan 01e: run the phase's checks once on the base, at the start of the
   * first attempt, unless a baseline for this exact base tree and check list
   * already exists — in this run (a restart) or in this node's program (a
   * sibling node). Best-effort: any failure leaves no baseline and the checks
   * stay strict, which is the safe direction. */
  async #ensureBaseline(): Promise<void> {
    const commands = this.#resolvedEffectiveChecks();
    if (commands.length === 0) return;
    const key = this.#baselineKey(commands);
    const local = this.#readBaseline();
    if (local && this.#baselineCovers(local, key, commands)) return;

    // Plan 06c (A3): a child node whose base is its parent's accepted
    // candidate reuses that candidate's passing check record instead of
    // running a baseline again.
    const candidate = this.#candidateRecordBaseline(commands, key);
    if (candidate) {
      this.#adoptBaseline(candidate.record, candidate.sourceDir, "parent candidate", candidate.record.baseSha);
      return;
    }

    // A program node reuses a sibling's identical-base baseline before paying
    // for its own run (runtime doc §4: every node of atlas plan 13's base paid
    // for the same 14 failures).
    const shared = this.#baselineSharedDir(key);
    if (!shared) {
      await this.#runBaselineOnce(commands, key);
      return;
    }

    const file = path.join(shared, "baseline.json");
    const sibling = this.#readBaselineFrom(file);
    if (sibling && this.#baselineCovers(sibling, key, commands)) {
      this.#adoptBaseline(sibling, shared, "program");
      return;
    }

    // "One check run per base tree at most" must hold for the parallel wave
    // too (two nodes whose branches differ but whose trees are identical): the
    // first node to take the lock runs it and the others wait for the record
    // it publishes. A holder that died or stalled is detected by its own pid
    // and age, so a stale lock can never wedge a run.
    const waitMs = Math.max(30_000, commands.length * this.#deadlines.checkMs + this.#deadlines.termGraceMs);
    if (!this.#acquireBaselineLock(shared)) {
      const waited = await this.#waitForSharedBaseline(file, shared, key, commands, waitMs);
      if (waited) {
        this.#adoptBaseline(waited, shared, "program");
        return;
      }
      // The holder never published: compute our own rather than wait further.
      // Publishing is safe without the lock (a per-writer temp file plus a
      // rename), and both records hold the same answer for the same tree.
      await this.#runBaselineOnce(commands, key, shared);
      return;
    }
    try {
      // Double-check under the lock: another node may have published between
      // our read above and taking it.
      const published = this.#readBaselineFrom(file);
      if (published && this.#baselineCovers(published, key, commands)) {
        this.#adoptBaseline(published, shared, "program");
        return;
      }
      await this.#runBaselineOnce(commands, key, shared);
    } finally {
      this.#releaseBaselineLock(shared);
    }
  }

  /** Runs the baseline, records it in this run and (in a program) publishes it,
   * logging the record. Never throws: a baseline that cannot be taken leaves
   * every check judged strictly. */
  async #runBaselineOnce(commands: readonly string[], key: string, shared?: string): Promise<Baseline | undefined> {
    let record: Baseline;
    try {
      record = await this.#runBaseline(commands, key);
    } catch (err) {
      // Plan 05i: an environment failure is not a baseline that "could not be
      // taken" — it must reach `#runBaselineStage`, which blocks the run and
      // writes no record at all.
      if (err instanceof EnvFailure) throw err;
      this.#log.append("baseline_error", { error: String((err as Error)?.message ?? err) });
      return undefined;
    }
    // B-24: the stage timed out (or was interrupted) while this command ran;
    // its record is late and must NOT be written or treated as a baseline —
    // the checks then stay strict, which is the honest direction.
    if (this.#baselineTimedOut) {
      this.#log.append("baseline_late_ignored", { key, reason: "the baseline stage timed out before this run finished" });
      return undefined;
    }
    this.#recordBaselineLocal(record);
    if (shared) this.#publishBaselineShared(record, shared);
    this.#log.append("baseline", { ...record, reused: false });
    return record;
  }

  /** Reuses a record another run took: the record and its logs are copied into
   * this run's own `checks/base/`, so the run directory stays self-contained,
   * and the reuse is logged with where it came from. */
  #adoptBaseline(record: Baseline, sourceDir: string, source: string, reusedFrom?: string): void {
    this.#writeBaselineLocal(record, sourceDir);
    this.#log.append("baseline", { ...record, reused: true, source, ...(reusedFrom ? { reusedFrom } : {}) });
    if (reusedFrom) this.#baselineReusedFrom = reusedFrom;
  }

  /** Waits (bounded) for the node that holds the shared baseline lock to
   * publish its record. The lock names its holder's pid and age, so a dead or
   * stalled holder is given up on instead of waited out. */
  async #waitForSharedBaseline(
    file: string,
    shared: string,
    key: string,
    commands: readonly string[],
    waitMs: number,
  ): Promise<Baseline | undefined> {
    const deadline = Date.now() + waitMs;
    for (;;) {
      if (this.#closed) return undefined;
      const record = this.#readBaselineFrom(file);
      if (record && this.#baselineCovers(record, key, commands)) return record;
      // A lock whose holder is gone (or that outlived any possible run) is
      // released here, so the next node — or this one — can compute its own.
      if (this.#baselineLockIsStale(shared, waitMs)) {
        this.#releaseBaselineLock(shared);
        return undefined;
      }
      if (Date.now() >= deadline) return undefined;
      await sleepMs(250);
    }
  }

  /** Exclusively creates the shared baseline lock, carrying this process's pid
   * and the time, so a waiter can tell a live holder from a dead one. */
  #acquireBaselineLock(shared: string): boolean {
    try {
      fs.mkdirSync(shared, { recursive: true });
      fs.writeFileSync(
        path.join(shared, "lock.json"),
        JSON.stringify({ pid: process.pid, at: Date.now() }),
        { flag: "wx" },
      );
      return true;
    } catch {
      return false;
    }
  }

  #releaseBaselineLock(shared: string): void {
    try {
      fs.rmSync(path.join(shared, "lock.json"), { force: true });
    } catch {
      // best effort
    }
  }

  /** True when the lock's holder is gone or the lock is older than any baseline
   * could possibly take. An unreadable lock file is treated as live — the
   * bounded wait handles that case. */
  #baselineLockIsStale(shared: string, waitMs: number): boolean {
    let info: { pid?: unknown; at?: unknown };
    try {
      info = JSON.parse(fs.readFileSync(path.join(shared, "lock.json"), "utf8")) as { pid?: unknown; at?: unknown };
    } catch {
      return false;
    }
    if (typeof info.at === "number" && Date.now() - info.at > waitMs) return true;
    if (typeof info.pid === "number" && !pidRunning(info.pid)) return true;
    return false;
  }

  /** Plan 01e: run every effective check command once on a disposable checkout
   * of the base, recording each one's exit, duration, parsed failing test
   * names and log. Unlike the C gate the loop does not stop at the first
   * failure — the whole base picture is the point. */
  async #runBaseline(commands: readonly string[], key: string): Promise<Baseline> {
    const outDir = this.#baselineDir();
    fs.mkdirSync(outDir, { recursive: true });
    const checkout = disposableCheckout(this.#plan.repo, this.#baselineBaseSha());
    const results: BaselineCommand[] = [];
    try {
      for (const command of commands) {
        const startedAt = Date.now();
        // Plan 05d: a base failure's own single re-run must finish inside the
        // command's deadline, exactly as a candidate's does.
        const deadlineAt = startedAt + this.#deadlines.checkMs;
        // Same isolation and per-command deadline as the C gate (F13).
        // ODP-3: tracked live, so `stop()` signals the baseline's group too.
        let baselinePgid: number | undefined;
        const running = runCommand({
          command,
          cwd: checkout.dir,
          env: childEnv(),
          deadlineMs: this.#deadlines.checkMs,
          termGraceMs: this.#deadlines.termGraceMs,
          // Plan 04a / advisory A-15: record the command's own process group
          // so a crash-recovery restart can kill the orphaned baseline run
          // instead of starting a second copy of the same heavy suite.
          onIntent: ({ pgid }) => {
            baselinePgid = pgid;
            this.#onStageShIntent(pgid);
            this.#log.intent(`baseline-sh-${this.#baselineActionId ?? "?"}-${pgid}`, { pgid });
          },
        });
        const result = await running.result;
        if (baselinePgid !== undefined) this.#onStageShExit(baselinePgid);
        this.#recordCheck(outDir, command, result);
        // Plan 05i: 126/127 means the shell could not execute this command.
        // Abort the whole baseline (no record is written) rather than
        // recording an environment failure as the base's own failing test.
        if (!result.timedOut && (result.exitCode === 126 || result.exitCode === 127)) {
          throw new EnvFailure("baseline", command, result.exitCode, result.output);
        }
        // Only a command that ran to completion and exited non-zero names a
        // failure the base is known to have: a timeout's output is truncated,
        // a signal death never printed its last failure, and a command that
        // exited 0 did not fail at all — so none of those three may put a
        // name into the set that excuses a candidate's check.
        let failures: string[] = [];
        let flakes: string[] = [];
        if (failedNormally(result)) {
          const parsed = parseTestFailures(result.output);
          if (parsed.length > 0) {
            // Plan 05d / finding #25: re-run each base failure alone once. A
            // name that passes alone is a `base flake` — visible, and never
            // part of the excuse set, so a candidate failing it is judged on
            // its own.
            const classified = await this.#classifyNewFailures({
              output: result.output,
              names: parsed,
              failingExitCode: result.exitCode,
              cwd: checkout.dir,
              deadlineAt,
              maxReruns: 1,
            });
            failures = classified.filter((c) => c.reproducesAlone).map((c) => c.name);
            flakes = classified.filter((c) => c.loadOnly).map((c) => c.name);
            if (flakes.length > 0) {
              this.#log.append("baseline_flake", { command, flakes, failures, loadAverage: Math.round(loadavg()[0] * 100) / 100 });
            }
          }
        }
        results.push({
          command,
          exitCode: result.exitCode,
          signal: result.signal,
          timedOut: result.timedOut,
          durationMs: Date.now() - startedAt,
          failures,
          ...(flakes.length > 0 ? { flakes } : {}),
          log: this.#checkLogName(command),
        });
      }
    } finally {
      checkout.dispose();
    }
    const flakes = baselineFlakeNames(results);
    return {
      baseSha: this.#baselineBaseSha(),
      tree: this.#baselineTree(),
      key,
      at: new Date().toISOString(),
      commands: results,
      failures: baselineFailureNames(results),
      ...(flakes.length > 0 ? { flakes } : {}),
    };
  }

  #readBaselineFrom(file: string): Baseline | undefined {
    try {
      return parseBaseline(JSON.parse(fs.readFileSync(file, "utf8")));
    } catch {
      return undefined;
    }
  }

  /** Writes the run-local record and, when `sourceDir` holds a reused
   * baseline, copies its logs in too, so `checks/base/` of this run is
   * self-contained. */
  #writeBaselineLocal(record: Baseline, sourceDir: string): void {
    try {
      const outDir = this.#baselineDir();
      fs.mkdirSync(outDir, { recursive: true });
      for (const command of record.commands) {
        if (!command.log) continue;
        try {
          fs.copyFileSync(path.join(sourceDir, command.log), path.join(outDir, command.log));
        } catch {
          // best effort: the record still names the command and its failures.
        }
      }
    } catch {
      // best effort; `#writeBaselineRecord` reports its own failure
    }
    this.#writeBaselineRecord(record);
  }

  #recordBaselineLocal(record: Baseline): void {
    this.#writeBaselineRecord(record);
  }

  /** The run-local `baseline.json`, best-effort: an unwritable checks dir must
   * leave the gate strict, never wedge the attempt. */
  #writeBaselineRecord(record: Baseline): void {
    try {
      fs.mkdirSync(this.#baselineDir(), { recursive: true });
      fs.writeFileSync(this.#baselinePath(), JSON.stringify(this.#baselineForDisk(record), null, 2));
    } catch (err) {
      this.#log.append("baseline_error", { error: String((err as Error)?.message ?? err) });
    }
  }

  /** Publishes a freshly-run baseline to the program's shared store, so a
   * later node with the same base and checks reuses it. Best-effort: a
   * hand-started run has none, and an unwritable program dir must not fail the
   * run. Temporary file plus rename, so a concurrent node never reads a
   * half-written record. */
  #publishBaselineShared(record: Baseline, shared: string): void {
    try {
      fs.mkdirSync(shared, { recursive: true });
      for (const command of record.commands) {
        if (!command.log) continue;
        fs.copyFileSync(path.join(this.#baselineDir(), command.log), path.join(shared, command.log));
      }
      const tmp = path.join(shared, `baseline.json.${process.pid}.${randomUUID().slice(0, 8)}.tmp`);
      fs.writeFileSync(tmp, JSON.stringify(this.#baselineForDisk(record), null, 2));
      fs.renameSync(tmp, path.join(shared, "baseline.json"));
    } catch (err) {
      this.#log.append("baseline_shared_error", { error: String((err as Error)?.message ?? err) });
    }
  }

  async #runChecks(actionId: string, candidateSha: string): Promise<void> {
    this.#log.intent(actionId, { candidateSha });
    crashAt("before_run_checks");
    const checkoutDir = disposableCheckout(this.#plan.repo, candidateSha);
    const outDir = path.join(this.#paths.checks, candidateSha);
    fs.mkdirSync(outDir, { recursive: true });
    // Plan 06c: `checkTier` decides which commands this candidate runs. From
    // CHECKING the candidate has not been reviewed yet, so the tier is
    // `round`; from FINAL_CHECKING it has passed review with no open blocker,
    // so the tier is `final` and the phase's final check is appended.
    const tier = checkTier(this.#state.phase.contract, {
      sha: candidateSha,
      reviewed: this.#state.phase.phase === "FINAL_CHECKING",
      openBlocker: false,
    });
    const finalCommands = tier === "final" ? (this.#state.phase.contract.finalChecks ?? []).map((c) => this.#withValues(c)) : [];
    const recordCommands: CheckRecordCommand[] = [];
    const recordLoad1 = Math.round(loadavg()[0] * 100) / 100;
    const recordFreeMemMB = Math.round(os.freemem() / (1024 * 1024));
    try {
      const before = verifyIntegrity(this.#plan.repo, checkoutDir.dir, candidateSha);
      let passed = before;
      let integrityViolated = !before;
      let timedOut = false;
      /** Plan 01e: the names this candidate's checks failed on that the base
       * did not — recorded with the check's completion so a repair round (and
       * the owner) can see exactly what is new. Plan 05d narrows this to the
       * names that still fail when re-run alone. */
      let newFailures: string[] = [];
      /** Plan 06b: the combined output of every check command, so each item's
       * `test` verify is resolved against the check run by name. */
      let combinedOutput = "";
      /** Plan 05d: every new failing test of the failing command, with its
       * `reproduces alone` / `load-only` label, for the repair prompt. */
      let checkFailures: CheckFailureClass[] = [];
      /** Plan 05d: load-only tests not yet emitted — emitted after the loop so
       * `savedRound` reflects the check's own outcome. */
      const pendingFlakes: EvFlakeObserved[] = [];
      if (before) {
        // F04: the effective list is the global plan checks followed by the
        // phase contract's own checks, deduped by exact command string — the
        // same list `#runProbe` executes against the merged integration I.
        // Plan 06c: the `final` tier appends the phase's final check, decided
        // by `checkTier` above, never here.
        for (const rawCommand of checkCommands(
          this.#plan.checks,
          this.#state.phase.contract.checks,
          this.#state.phase.contract.finalChecks,
          tier,
        )) {
          // Plan 01a: a plan that pasted a value into its check line gets it
          // back here (the snapshot it may have been read from is masked).
          const command = this.#withValues(rawCommand);
          // design §8.1: "each check command | 10 min | kill its group |
          // check failed: timeout" — runCommand's own deadlineMs already
          // kills the command's process group on expiry (shell.ts); this
          // just records *why* the check failed.
          const startedAt = Date.now();
          // Plan 05d: a re-run alone must finish inside this same per-command
          // deadline (design §8.1). `deadlineAt` is that deadline's wall time.
          const deadlineAt = startedAt + this.#deadlines.checkMs;
          // ODP-3: the check's own process group, tracked live so `stop()`
          // signals it like every other stage's.
          let checkPgid: number | undefined;
          const running = runCommand({
            command,
            cwd: checkoutDir.dir,
            // F13: isolate the child from the Node test runner's own
            // recursion markers so a check that runs `node --test` actually
            // runs (and can fail) instead of silently skipping.
            env: childEnv(),
            deadlineMs: this.#deadlines.checkMs,
            termGraceMs: this.#deadlines.termGraceMs,
            // Plan 06c: record the command's process group so a crash/stop
            // during CHECKING can kill the orphan and re-run the check.
            // ODP-3: it is also tracked live, so `stop()` signals it like any
            // other stage's group.
            onIntent: ({ pgid }) => {
              checkPgid = pgid;
              this.#onStageShIntent(pgid);
              this.#log.intent(`check-sh-${actionId}-${pgid}`, { pgid });
            },
          });
          const result = await running.result;
          if (checkPgid !== undefined) this.#onStageShExit(checkPgid);
          combinedOutput += `${result.output}\n`;
          // Plan 05d: the machine's load average at the failing run is part
          // of the flake evidence (findings #10, #31).
          const machineLoad = Math.round(loadavg()[0] * 100) / 100;
          if (result.timedOut) timedOut = true;
          this.#recordCheck(outDir, command, result);
          recordCommands.push({
            command,
            exitCode: result.exitCode,
            signal: result.signal,
            timedOut: result.timedOut,
            durationMs: Date.now() - startedAt,
            passed: result.exitCode === 0 && !result.timedOut,
            log: this.#checkLogName(command),
          });
          // Plan 05i: 126/127 means the shell could not execute the command
          // (`command not found` / `not executable`). That is the machine, not
          // the code: stop the run in ENV_BLOCKED instead of recording a
          // `checks failed`, a repair round or a finding.
          if (!result.timedOut && (result.exitCode === 126 || result.exitCode === 127)) {
            this.#log.completion(actionId, { candidateSha, passed: false, reason: "environment", command, exitCode: result.exitCode });
            this.#applyEnvFailure("checks", command, result);
            return;
          }
          const after = verifyIntegrity(this.#plan.repo, checkoutDir.dir, candidateSha);
          if (!after) integrityViolated = true;
          const failed = result.exitCode !== 0 || result.timedOut || !after;
          if (failed) {
            // Plan 01e / D2's default: a failing check whose every parsed
            // failing test also failed on the base is not this candidate's
            // failure — the base already had it before the phase started. The
            // rule is deliberately narrow: only a check that ran to completion
            // and exited non-zero can be excused. A timeout's output is
            // truncated; a signal death (the OOM killer's SIGKILL, a SIGSEGV)
            // reports `exitCode: null` with a non-null signal and never got to
            // print its last failure; and an integrity violation is not this
            // candidate's tree at all. In all three the output could name only
            // the base's tests while a new failure went unnamed. A check whose
            // output yields no test name at all keeps the strict rule, and any
            // parsed name the base did not fail on that same command fails the
            // gate.
            if (after && failedNormally(result)) {
              const baseFailures = this.#baseFailuresFor(command);
              const verdict = classifyCheckFailure(result.output, baseFailures, this.#requiredTestTexts());
              if (verdict.excused) {
                this.#log.append("check_failures_pre_existing", {
                  candidateSha,
                  command,
                  failures: verdict.parsed,
                  baseFailures,
                });
                continue;
              }
              if (verdict.newFailures.length > 0) {
                // Plan 05d: before any of these may fail the gate, re-run each
                // alone. `load-only` tests do not fail the check; only the
                // ones that still fail alone (or whose output named no test)
                // do (findings #4, #31, #35).
                const classifications = await this.#classifyNewFailures({
                  output: result.output,
                  names: verdict.newFailures,
                  failingExitCode: result.exitCode,
                  cwd: checkoutDir.dir,
                  deadlineAt,
                });
                const loadOnly = classifications.filter((c) => c.loadOnly);
                const real = classifications.filter((c) => c.reproducesAlone);
                for (const c of loadOnly) {
                  pendingFlakes.push({
                    type: "FLAKE_OBSERVED",
                    name: c.name,
                    command,
                    ...(c.rerunCommand !== undefined ? { rerunCommand: c.rerunCommand } : {}),
                    failingExitCode: result.exitCode,
                    rerunExitCodes: c.rerunExitCodes,
                    loadAverage: machineLoad,
                    savedRound: false,
                    candidateSha,
                  });
                }
                if (real.length === 0) {
                  // Every new failure passed alone: the check passes, with one
                  // recorded observation per test.
                  this.#log.append("check_failures_load_only", {
                    candidateSha,
                    command,
                    tests: classifications,
                    loadAverage: machineLoad,
                  });
                  continue;
                }
                checkFailures = classifications;
                newFailures = real.map((c) => c.name);
                this.#log.append("check_failure_new", {
                  candidateSha,
                  command,
                  newFailures,
                  failures: verdict.parsed,
                  classifications,
                });
              }
            }
            passed = false;
            break;
          }
        }
      }
      // design §2.2: a checkout that no longer matches its candidate commit
      // invalidates that gate's result (never counted as passed — `passed`
      // above is already forced `false` by the `!after` check) and marks
      // the run integrity-violated. This is now a logged core event (item
      // 3), not an in-memory-only flag, so it survives a conductor restart
      // via `rebuildState`.
      if (integrityViolated) {
        this.#applyEvent({ type: "INTEGRITY_VIOLATED", stage: "checks", evidence: `candidate ${candidateSha}` });
      }
      // Plan 05d: emit the flake observations now that the check's outcome is
      // known, so `savedRound` is true only when the check really passed
      // because every new failure was load-only.
      const savedRound = passed && !integrityViolated && pendingFlakes.length > 0;
      for (const flake of pendingFlakes) this.#applyEvent({ ...flake, savedRound });
      crashAt("after_run_checks");
      this.#log.completion(actionId, {
        candidateSha,
        passed,
        integrityViolated,
        ...(newFailures.length > 0 ? { newFailures } : {}),
        ...(checkFailures.length > 0 ? { checkFailures } : {}),
        reason: !passed && timedOut ? "timeout" : undefined,
      });
      // Plan 06c: the record names the tier, the final command when there was
      // one, and the machine's load at the run.
      this.#writeCheckRecord(outDir, {
        candidateSha,
        ...(this.#baselineBaseSha() ? { baseSha: this.#baselineBaseSha() } : {}),
        tier,
        passed,
        commands: recordCommands,
        finalCommands,
        load1: recordLoad1,
        freeMemMB: recordFreeMemMB,
        at: new Date().toISOString(),
      });
      // Plan 06b: resolve every item's `test` verify against the candidate's
      // own (round) check run. A named test that is missing from or failed in
      // the output is a blocking finding anchored to its item. The final
      // check's run does not re-resolve them.
      if (tier === "round" && this.#itemsEnforced()) {
        const resolutions = resolveTestVerifies(this.#planItems(), combinedOutput);
        this.#applyEvent({ type: "ITEM_STATE_UPDATED", checkResolution: resolutions });
        this.#applyTestVerifyFindings(candidateSha);
      }
      if (tier === "final") {
        if (passed) {
          this.#applyEvent({ type: "FINAL_CHECKS_PASSED", candidateSha });
        } else {
          const failing = recordCommands.filter((c) => !c.passed).map((c) => c.command);
          const tail = combinedOutput.split("\n").filter((l) => l.trim().length > 0).slice(-20).join("\n");
          // Keep the SAME rerun classification an ordinary check failure has
          // (loadOnly, reproducesAlone, rerunExitCodes): a load-only flake is
          // never relabelled a real regression, and the record says which
          // reruns were actually performed. Only when the loop classified
          // nothing (its output named no test) do we fall back to the names.
          const failingExitCode = recordCommands.find((c) => !c.passed)?.exitCode ?? null;
          const finalFailures: CheckFailureClass[] =
            checkFailures.length > 0
              ? checkFailures
              : parseTestFailures(combinedOutput).map((name) => ({
                  name,
                  reproducesAlone: true,
                  loadOnly: false,
                  failingExitCode,
                  rerunExitCodes: [],
                }));
          this.#applyEvent({
            type: "FINAL_CHECKS_FAILED",
            evidence: `the final check failed on candidate ${candidateSha.slice(0, 9)}: ${failing.join(", ") || "(a check command exited non-zero)"}${tail ? `\n${tail}` : ""}`,
            ...(finalFailures.length > 0 ? { failures: finalFailures } : {}),
          });
        }
      } else if (passed) {
        this.#applyEvent({ type: "CHECKS_PASSED" });
      } else {
        this.#applyEvent({ type: "CHECKS_FAILED", ...(checkFailures.length > 0 ? { failures: checkFailures } : {}) });
        // Plan 06g (A4): a test that PASSED at this round's base (the previous
        // candidate in a repair, the phase base in round 1) and fails now is a
        // true regression — recorded so the reason is observable, and distinct
        // from a persisting failure the base already had.
        const regressions = this.#regressionTests();
        if (regressions.length > 0) {
          this.#log.append("round_regression", { candidateSha, round: this.#state.phase.round ?? 1, tests: regressions });
        }
      }
    } finally {
      checkoutDir.dispose();
    }
  }

  /** Plan 05d: re-run each newly failing test alone, inside the failing
   * check's own deadline, and label it. Up to two re-runs per test (a test
   * that passes on the first is not run again); a re-run that times out or
   * finds no time left counts as `reproduces alone`, the strict direction. */
  async #classifyNewFailures(params: {
    output: string;
    names: readonly string[];
    failingExitCode: number | null;
    cwd: string;
    deadlineAt: number;
    /** Plan 05d: a base failure is re-run alone **once** (finding #25); a
     * candidate's new failure up to twice. */
    maxReruns?: number;
  }): Promise<CheckFailureClass[]> {
    const plans = rerunCommandsFor(params.output, params.names, this.#plan.rerun);
    const maxReruns = params.maxReruns ?? 2;
    const out: CheckFailureClass[] = [];
    for (const plan of plans) {
      const reruns: TestRerunOutcome[] = [];
      if (plan.command) {
        for (let attempt = 0; attempt < maxReruns; attempt++) {
          const budget = rerunBudgetMs(params.deadlineAt, Date.now());
          if (budget <= 0) break;
          const running = runCommand({
            command: plan.command,
            cwd: params.cwd,
            env: childEnv(),
            deadlineMs: budget,
            termGraceMs: this.#deadlines.termGraceMs,
          });
          const result = await running.result;
          // Review finding M-1: exit 0 alone is not proof — a filter that
          // matched no test exits 0. Keep the output so the classification can
          // require evidence that the test ran.
          reruns.push({ exitCode: result.exitCode, timedOut: result.timedOut, output: result.output });
          if (!result.timedOut && result.exitCode === 0 && rerunProvesTheTestRan(result.output, plan.name)) break;
        }
      }
      const classified = classifyRerun(plan.name, plan.command, params.failingExitCode, reruns);
      out.push({
        name: classified.name,
        ...(classified.rerunCommand !== undefined ? { rerunCommand: classified.rerunCommand } : {}),
        reproducesAlone: classified.reproducesAlone,
        loadOnly: classified.loadOnly,
        failingExitCode: classified.failingExitCode,
        rerunExitCodes: classified.rerunExitCodes,
      });
    }
    return out;
  }

  // -- probe ----------------------------------------------------------------

  async #runProbe(actionId: string, candidateSha: string, head: string): Promise<void> {
    this.#log.intent(actionId, { candidateSha, head });
    crashAt("before_dispatch_probe");
    const result = gitProbe(this.#plan.repo, { runId: this.#state.phase.runId, candidateSha, headSha: head });
    if (!result.ok) {
      crashAt("after_dispatch_probe");
      this.#log.completion(actionId, { candidateSha, ok: false, output: result.output });
      this.#applyEvent({ type: "PROBE_FAILED", evidence: result.output });
      return;
    }
    let passed = true;
    let timedOutCommand: string | undefined;
    // F04: the probe executes the same effective list as the C gate, on its
    // own fresh checkout of the merged integration I, and records each
    // command's evidence under a probe-specific directory keyed by that I.
    const outDir = path.join(this.#paths.checks, "probe", result.I);
    // Plan 2c: when the probed integration I has exactly the candidate's
    // tree (the normal fast-forward case, design §6.4 step 2) and the checks
    // already passed on that candidate, rerunning them on I cannot give a
    // different answer. Record the reuse instead of spending another full
    // check run (≈2.5 min per round in dogfood run 4ec5e0f8).
    const checks = this.#state.phase.checks;
    const sameTree = treeOf(this.#plan.repo, result.I) === treeOf(this.#plan.repo, candidateSha);
    const reuse = this.#probeReuse && sameTree && checks?.candidateSha === candidateSha && checks.passed === true;
    if (reuse) this.#log.append("probe_checks_reused", { candidateSha, I: result.I, reason: "I has the candidate's tree; checks passed on the candidate" });
    // Plan 06c (A1/C2): the probe re-runs the round list from the one chooser.
    for (const rawCommand of reuse ? [] : checkCommands(this.#plan.checks, this.#state.phase.contract.checks, this.#state.phase.contract.finalChecks, "round")) {
      const command = this.#withValues(rawCommand);
      // design §8.1: "integration probe (merge plus its checks) | as for
      // checks, per command | kill its group; discard the probe branch |
      // 'integration' finding: timeout" — runCommand's deadlineMs already
      // kills the command's group on expiry; the probe branch is always
      // discarded below regardless of outcome.
      const running = runCommand({
        command,
        cwd: result.checkoutDir,
        // F13: same test-runner-marker isolation as the C gate (see childEnv).
        env: childEnv(),
        deadlineMs: this.#deadlines.probeMs,
        termGraceMs: this.#deadlines.termGraceMs,
      });
      const commandResult = await running.result;
      this.#recordCheck(outDir, command, commandResult);
      // Plan 05i: 126/127 is the machine, not the code — discard the probe
      // branch and block on the environment instead of raising an
      // `integration` finding.
      if (!commandResult.timedOut && (commandResult.exitCode === 126 || commandResult.exitCode === 127)) {
        discardProbe(this.#plan.repo, { probeBranch: result.probeBranch, checkoutDir: result.checkoutDir });
        this.#log.completion(actionId, { candidateSha, ok: false, I: result.I, reason: "environment", command, exitCode: commandResult.exitCode });
        this.#applyEnvFailure("probe", command, commandResult);
        return;
      }
      if (commandResult.exitCode !== 0 || commandResult.timedOut) {
        if (commandResult.timedOut) timedOutCommand = command;
        passed = false;
        break;
      }
    }
    discardProbe(this.#plan.repo, { probeBranch: result.probeBranch, checkoutDir: result.checkoutDir });
    crashAt("after_dispatch_probe");
    this.#log.completion(actionId, {
      candidateSha,
      ok: passed,
      I: result.I,
      reason: timedOutCommand !== undefined ? "timeout" : undefined,
    });
    if (passed) {
      // Plan 06b: grep the candidate for every architecture `:WHERE:` symbol
      // before the reviewers are asked.
      this.#applyArchitectureSymbols(candidateSha);
      // Plan 06g2: PROBE_PASSED moves the phase to REVIEWING, where `next()`
      // would dispatch all three reviews — but a lane round's winner was
      // already reviewed. Both the move and the promotion happen with `drive()`
      // suspended, so the phase never sits in REVIEWING without them.
      this.#driveSuspended = true;
      try {
        this.#applyEvent({ type: "PROBE_PASSED", probedI: result.I });
        await this.#promoteLaneWinnerReviews(candidateSha);
      } finally {
        this.#driveSuspended = false;
      }
      this.drive();
    } else if (timedOutCommand !== undefined) {
      this.#applyEvent({ type: "PROBE_FAILED", evidence: `timeout: integration probe command '${timedOutCommand}' timed out on probed integration ${result.I}` });
    } else {
      this.#applyEvent({ type: "PROBE_FAILED", evidence: `checks failed on probed integration ${result.I}` });
    }
  }

  // -- plan 01f: the gate ---------------------------------------------------

  /** Every gate record this run has written so far (`checks/<sha>/gate.json`),
   * newest last — the inputs to the reuse rule in core/gate.ts. A malformed
   * or half-written record is ignored: it is never a passing gate. */
  #gateRecords(): GateRecord[] {
    let names: string[];
    try {
      names = fs.readdirSync(this.#paths.checks);
    } catch {
      return [];
    }
    const records: GateRecord[] = [];
    for (const name of names) {
      if (name === "base" || name === "probe") continue;
      try {
        const record = parseGateRecord(JSON.parse(fs.readFileSync(path.join(this.#paths.checks, name, "gate.json"), "utf8")));
        if (record) records.push(record);
      } catch {
        // no record here, or an unreadable/partial one
      }
    }
    return records.sort((a, b) => Date.parse(a.startedAt) - Date.parse(b.startedAt));
  }

  /** The `gate.log` a record names, or undefined when the file is missing or
   * its bytes no longer hash to the record's own sha256. Only a log that
   * matches is evidence: a pruned, truncated or hand-edited record must make
   * the gate rerun, never carry acceptance (runtime doc §6's substitute). */
  #verifiedGateLog(record: GateRecord): Buffer | undefined {
    try {
      const log = fs.readFileSync(path.join(this.#paths.checks, record.candidateSha, "gate.log"));
      return gateLogHashMatches(record, log) ? log : undefined;
    } catch {
      return undefined;
    }
  }

  /** Every **verified** passing gate record of this run, keyed by candidate:
   * the only records `gateDecision` may accept or reuse. A record written as a
   * reuse counts: its log is the verified copy of the run that produced the
   * evidence, so a candidate's own reuse record (and a chain of them) is
   * still this gate's evidence, and an identical tree never pays twice. */
  #verifiedPassingGateRecords(): Map<string, { record: GateRecord; log: Buffer }> {
    const out = new Map<string, { record: GateRecord; log: Buffer }>();
    for (const record of this.#gateRecords()) {
      if (!record.passed) continue;
      const log = this.#verifiedGateLog(record);
      if (log) out.set(record.candidateSha, { record, log });
    }
    return out;
  }

  /** Plan 01f: the gate record to show a reviewer for candidate `C` — the
   * record for `C` itself when one exists (an earlier pass, or a stale-publish
   * retry), otherwise the newest record of the run, which is the failed gate
   * that sent the phase into this repair round. Only records whose log still
   * matches the hash they carry are shown: an unverifiable record is not
   * evidence, and a prompt must not describe it as one. */
  #gateRecordForPrompt(candidateSha: string): GateRecord | undefined {
    const verified = this.#gateRecords().filter((r) => this.#verifiedGateLog(r) !== undefined);
    return verified.find((r) => r.candidateSha === candidateSha) ?? verified[verified.length - 1];
  }

  /** The last lines of the gate log that goes with `#gateRecordForPrompt`,
   * for a failed record (a pass needs no tail). */
  #gateTailForPrompt(candidateSha: string): string | undefined {
    const record = this.#gateRecordForPrompt(candidateSha);
    if (!record || record.passed) return undefined;
    try {
      return gateLogTail(fs.readFileSync(path.join(this.#paths.checks, record.candidateSha, "gate.log"), "utf8"));
    } catch {
      return undefined;
    }
  }

  /** Plan 01f: the environment the gate (and its cleanup) run with — the
   * same isolation as a check (`childEnv`), plus the plan's declared secret
   * values, so a gate command that needs a vendor key gets it from the
   * environment rather than from its own text (plan 01a's rule). */
  #gateEnv(): NodeJS.ProcessEnv {
    return { ...childEnv(), ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])) };
  }

  /** Plan 01f: the conductor's own gate. Takes the machine-wide gate lock
   * first, then decides run-or-reuse under it, then (only if it must run)
   * merges the candidate onto the current integration head, runs the
   * contract's `:GATE:` command in that checkout with the plan's secrets in
   * the environment, and records everything. Writes `checks/<sha>/gate.json`
   * and `checks/<sha>/gate.log`, runs `:GATE_CLEANUP:` whenever the command
   * ran (pass, fail or timeout; a candidate that no longer merges never
   * starts it and is recorded as `not started`), and applies either the
   * ACCEPTED event (a pass) or GATE_FAILED with the log's last lines.
   *
   * A candidate whose tree already passed the same command does not run it
   * again — but only a record whose own `gate.log` still hashes to what the
   * record claims is used: an unverifiable record makes the gate rerun
   * (`core/gate.ts`'s `gateDecision`/`gateLogHashMatches`). A candidate's own
   * record is accepted in place and never rewritten; another candidate's
   * pass is copied into a new record that names the head it is accepted
   * against and the head the evidence came from.
   *
   * The agent never produces this evidence (runtime doc §6): a passing gate is
   * what lets acceptance proceed, a failing one is a blocking `integration`
   * finding the worker is shown, and no agent may run the command or report
   * its result. */
  async #runGate(actionId: string, candidateSha: string): Promise<void> {
    const contract = this.#state.phase.contract;
    const head = this.#state.phase.integrationHead;
    const declared = gateCommandOf(contract);
    const outDir = path.join(this.#paths.checks, candidateSha);
    fs.mkdirSync(outDir, { recursive: true });
    const logPath = path.join(outDir, "gate.log");
    const recordPath = path.join(outDir, "gate.json");
    if (!declared) {
      // Defensive: GATING is only reachable when the contract declares a
      // gate, but a hand-built event must not silently accept a candidate.
      this.#log.completion(actionId, { candidateSha, passed: false, reason: "no gate command declared" });
      this.#applyEvent({ type: "GATE_FAILED", evidence: "the conductor was asked to gate a phase whose contract declares no :GATE: command" });
      return;
    }
    const command = this.#withValues(declared);
    const maskedCommand = redactText(command, this.#secretMaskable);
    const maskedCleanup = contract.gateCleanup ? redactText(this.#withValues(contract.gateCleanup), this.#secretMaskable) : undefined;
    const cleanupCommand = contract.gateCleanup ? this.#withValues(contract.gateCleanup) : undefined;
    const tree = treeOf(this.#plan.repo, candidateSha);
    this.#log.intent(actionId, { candidateSha, head, tree, command: maskedCommand });

    // Everything the gate does for this candidate is inside the machine-wide
    // lock: the merge that builds its checkout, the run-or-reuse decision
    // (taken here, under the lock, so a candidate whose tree another candidate
    // has just gated reuses that run's record instead of running the command
    // again), the command, the cleanup and the record. Two phases — and two
    // candidates that share a tree — never have their gate work live at once.
    const lock: Lock = await this.#acquireGateLock();
    let probed: ReturnType<typeof gitProbe> | undefined;
    let record: GateRecord | undefined;
    let logText = "";
    /** Plan 05i: set when the gate command exited 126/127 — an environment
     * failure, not a failing gate. */
    let envExit: number | null | undefined;
    try {
      // The reuse rule (core/gate.ts): a candidate whose tree has already
      // passed this exact command does not run it again — but only a record
      // whose own `gate.log` still hashes to what it claims is evidence. A
      // candidate's own record is handled separately (`gateDecision`): a
      // stale-publish retry re-gates the same candidate, and its record is
      // accepted in place, never rewritten with a claim of reusing itself.
      const passing = this.#verifiedPassingGateRecords();
      const decision = gateDecision(
        passing.get(candidateSha)?.record,
        [...passing.values()].map((v) => v.record),
        { candidateSha, tree, command: maskedCommand },
      );
      if (decision.kind === "own") {
        // Already this candidate's own evidence: write nothing, and record the
        // head being accepted now next to the head it was produced at.
        record = decision.record;
        this.#log.completion(actionId, {
          candidateSha,
          head,
          passed: true,
          reusedInPlace: true,
          gateBaseSha: decision.record.baseSha,
          logSha256: decision.record.logSha256,
        });
      } else if (decision.kind === "reuse") {
        const source = passing.get(decision.record.candidateSha)!;
        // The source log is copied byte for byte, so this record's hash is the
        // same verified hash — the evidence behind it is the run it names. A
        // source that is itself a reuse has its provenance flattened to the
        // run that actually produced the evidence.
        fs.writeFileSync(logPath, source.log);
        record = {
          ...source.record,
          candidateSha,
          // The head this candidate is being accepted against; where the
          // evidence actually came from is named separately.
          baseSha: head,
          reused: true,
          reusedFrom: source.record.reused
            ? (source.record.reusedFrom ?? source.record.candidateSha)
            : source.record.candidateSha,
          reusedFromBaseSha: source.record.reused
            ? (source.record.reusedFromBaseSha ?? source.record.baseSha)
            : source.record.baseSha,
          // The source's merge result belongs to the source's base, not this
          // head; a reused record names its bases instead of claiming an I.
          mergedI: undefined,
          logBytes: source.log.length,
          logSha256: createHash("sha256").update(source.log).digest("hex"),
        };
        fs.writeFileSync(recordPath, `${JSON.stringify(record, null, 2)}\n`);
        this.#log.completion(actionId, {
          candidateSha,
          head,
          passed: true,
          reused: true,
          reusedFrom: record.reusedFrom,
          reusedFromBaseSha: record.reusedFromBaseSha,
          logSha256: record.logSha256,
        });
      } else {
        // The probed checkout: merge the candidate onto the current head,
        // exactly as the probe does, so the gate's evidence is for the
        // integration the probe verified. (The probe passed, so a conflict
        // here means the head moved under the phase.)
        probed = gitProbe(this.#plan.repo, { runId: this.#state.phase.runId, candidateSha, headSha: head });
        if (!probed.ok) {
          // The candidate does not merge, so the gate command never starts and
          // the cleanup has nothing to release (the owner's ruling, D-B-62):
          // the record says "not started" rather than claiming an exit-less,
          // zero-duration run.
          logText = redactText(
            `$ ${maskedCommand}\nnot started: the candidate does not merge onto ${head}\n${probed.output}\n`,
            this.#secretMaskable,
          );
          fs.writeFileSync(logPath, logText);
          record = {
            candidateSha,
            tree,
            baseSha: head,
            command: maskedCommand,
            ...(maskedCleanup ? { cleanup: maskedCleanup } : {}),
            startedAt: new Date().toISOString(),
            notStarted: `the candidate does not merge onto ${head}`,
            passed: false,
            logBytes: Buffer.byteLength(logText, "utf8"),
            logSha256: createHash("sha256").update(logText).digest("hex"),
            cleanupSkipped: CLEANUP_NOT_RUN,
          };
          fs.writeFileSync(recordPath, `${JSON.stringify(record, null, 2)}\n`);
          this.#log.completion(actionId, {
            candidateSha,
            passed: false,
            reason: `not started: the candidate does not merge onto ${head}`,
          });
        } else {
          const startedAt = new Date().toISOString();
          const startedMs = Date.now();
          // ODP-3: tracked live, so `stop()` signals the gate's group too.
          let gatePgid: number | undefined;
          const running = runCommand({
            command,
            cwd: probed.checkoutDir,
            env: this.#gateEnv(),
            deadlineMs: this.#deadlines.gateMs,
            termGraceMs: this.#deadlines.termGraceMs,
            // Design §2.2: the command's process group is recorded before it
            // runs, so a crashed gate is recoverable (see `#reconcileOne`).
            onIntent: ({ pgid }) => {
              gatePgid = pgid;
              this.#onStageShIntent(pgid);
              this.#log.intent(`gate-sh-${actionId}-${pgid}`, { pgid });
            },
          });
          const gateResult = await running.result;
          if (gatePgid !== undefined) this.#onStageShExit(gatePgid);
          // Plan 05i: 126/127 is the shell failing to execute the command, not
          // the gate failing. Record it (the record carries the exit, and a
          // non-passing record is never reused) but block on the environment
          // instead of raising a blocking `integration` finding.
          if (!gateResult.timedOut && (gateResult.exitCode === 126 || gateResult.exitCode === 127)) {
            envExit = gateResult.exitCode;
          }
          // The gate command's own duration: the cleanup runs under its own
          // limit afterwards and is recorded separately (the record must not
          // present teardown time as build time).
          const durationMs = Date.now() - startedMs;
          // Whatever the outcome of the command: release what it took. The
          // cleanup holds its own (gate-length) limit; it is not part of the
          // gate's duration.
          let cleanupResult: RunCommandResult | undefined;
          let cleanupStartedAt: string | undefined;
          let cleanupMs = 0;
          if (cleanupCommand) {
            cleanupStartedAt = new Date().toISOString();
            const cleanupStartedMs = Date.now();
            // ODP-3: tracked live, so `stop()` signals the cleanup's group too.
            let cleanupPgid: number | undefined;
            const cleanup = runCommand({
              command: cleanupCommand,
              cwd: probed.checkoutDir,
              env: this.#gateEnv(),
              deadlineMs: this.#deadlines.gateMs,
              termGraceMs: this.#deadlines.termGraceMs,
              onIntent: ({ pgid }) => {
                cleanupPgid = pgid;
                this.#onStageShIntent(pgid);
                this.#log.intent(`gate-cleanup-sh-${actionId}-${pgid}`, { pgid });
              },
            });
            cleanupResult = await cleanup.result;
            if (cleanupPgid !== undefined) this.#onStageShExit(cleanupPgid);
            cleanupMs = Date.now() - cleanupStartedMs;
          }
          const passed = !gateResult.timedOut && gateResult.exitCode === 0;
          // The log holds both commands' output, redacted, with the exit
          // facts — the same shape `#recordCheck` writes.
          const parts = [
            `$ ${maskedCommand}\n${gateResult.output}\nexit ${gateResult.exitCode} signal ${gateResult.signal}${gateResult.timedOut ? ` (timed out after ${Math.round(durationMs / 1000)}s)` : ""}\n`,
          ];
          if (cleanupResult && maskedCleanup) {
            parts.push(
              `$ ${maskedCleanup}\n${cleanupResult.output}\nexit ${cleanupResult.exitCode} signal ${cleanupResult.signal}${cleanupResult.timedOut ? " (timed out)" : ""}\n`,
            );
          }
          logText = redactText(parts.join("\n"), this.#secretMaskable);
          fs.writeFileSync(logPath, logText);
          record = {
            candidateSha,
            tree,
            baseSha: head,
            mergedI: probed.I,
            command: maskedCommand,
            ...(maskedCleanup ? { cleanup: maskedCleanup } : {}),
            startedAt,
            durationMs,
            exitCode: gateResult.exitCode,
            signal: gateResult.signal,
            timedOut: gateResult.timedOut,
            passed,
            logSha256: createHash("sha256").update(logText).digest("hex"),
            logBytes: Buffer.byteLength(logText, "utf8"),
            ...(cleanupResult
              ? {
                  cleanupStartedAt,
                  cleanupDurationMs: cleanupMs,
                  cleanupExitCode: cleanupResult.exitCode,
                  cleanupTimedOut: cleanupResult.timedOut,
                }
              : {}),
          };
          fs.writeFileSync(recordPath, `${JSON.stringify(record, null, 2)}\n`);
          this.#log.completion(actionId, {
            candidateSha,
            head,
            passed,
            exitCode: gateResult.exitCode,
            timedOut: gateResult.timedOut,
            durationMs,
            logSha256: record.logSha256,
            ...(cleanupResult ? { cleanupExitCode: cleanupResult.exitCode } : {}),
          });
        }
      }
    } finally {
      // The record and the completion are written inside the lock, so the
      // window in the record is the window the lock covered (two honest
      // sequential gates can never look overlapped). Dispatching the phase's
      // next action is deliberately *outside* the lock: publishing is not the
      // gate, and other phases' gates must not wait for it.
      await lock.release();
      if (probed?.ok) discardProbe(this.#plan.repo, { probeBranch: probed.probeBranch, checkoutDir: probed.checkoutDir });
    }
    if (!record) return;
    if (envExit !== undefined) {
      this.#log.append("gate_environment_failure", { candidateSha, command: maskedCommand, exitCode: envExit });
      this.#applyEvent({
        type: "ENV_CHECK_FAILED",
        stage: "gate",
        command: maskedCommand,
        exitCode: envExit,
        tail: gateLogTail(logText),
        at: new Date().toISOString(),
      });
      return;
    }
    if (record.passed) {
      this.#applyEvent({ type: "ACCEPTED", resolvedCorrectionIds: resolvedCorrectionIdsFor(this.#state.phase, candidateSha, contract.contractVersion) });
    } else {
      this.#applyEvent({
        type: "GATE_FAILED",
        evidence: gateFailureEvidence({ record, logPath, tail: gateLogTail(logText) }),
      });
    }
  }

  /** The machine-wide gate lock (plan 01f). Waits for a holder instead of
   * failing: a second phase gates after the first is done, never alongside
   * it. It is acquired before the merge that builds the gate's checkout and
   * released after the gate command, its cleanup and the record are written;
   * a conductor that dies mid-gate releases it through the perl helper's own
   * exit, so a crashed gate cannot wedge every later one. */
  async #acquireGateLock(): Promise<Lock> {
    try {
      fs.mkdirSync(path.dirname(this.#gateLockPath), { recursive: true });
    } catch {
      // Best effort: the run root usually already exists.
    }
    return acquireWaitingLock(this.#gateLockPath);
  }

  // -- review -----------------------------------------------------------

  /** A reviewer dispatch's outcome applies only while the phase is still
   * reviewing the candidate it was started for. A late one (a stray
   * dispatch, or a reviewer finishing after the round moved on) is logged
   * and dropped: run 807d3e84 marked B timed out from a previous round's
   * stray dispatch, so B was never dispatched again and M and A waited at
   * the discovery barrier until the phase was BLOCKED. */
  #reviewStillCurrent(dispatchCandidate: string | undefined): boolean {
    const phase = this.#state.phase;
    return phase.phase === "REVIEWING" && phase.candidate?.sha === dispatchCandidate;
  }

  #reviewTimedOut(reviewer: Reviewer, dispatchCandidate: string | undefined): void {
    if (!this.#reviewStillCurrent(dispatchCandidate)) {
      this.#log.append("stale_review_ignored", { reviewer, kind: "timeout", dispatchCandidate, phase: this.#state.phase.phase });
      return;
    }
    this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
  }

  async #runReview(actionId: string, reviewer: Reviewer): Promise<void> {
    const dispatchCandidate = this.#state.phase.candidate?.sha;
    // Plan 06b (finding M-1): a fresh dispatch is a fresh review, so the
    // files this seat read in an earlier round no longer count as "read in
    // this review".
    this.#reviewerReads.set(reviewer, new Set());
    this.#reviewerCommands.set(reviewer, new Set());
    const agentId = `reviewer-${reviewer}-${actionId}`;
    const streamFile = path.join(this.#paths.stream, `${agentId}.jsonl`);
    const candidateDir = this.#candidateDir();
    const env: NodeJS.ProcessEnv = {
      ...this.#extraEnv,
      ...this.#piEnvFor?.("reviewer", agentId),
      TT_SOCKET: this.#paths.sock,
      TT_SEARCH_ROOTS: [candidateDir, this.#paths.refs].join(path.delimiter),
      // The extension waits this long for a command's result: the conductor's
      // per-command limit plus room for the kill and its report.
      TT_SH_WAIT_MS: String(this.#deadlines.shCommandMs + 30_000),
      TT_RUN_DIR: this.#runDir,
      // Plan 01a: same secrets as the worker (a reviewer's reproduction
      // command or check run may need one) — names plus values, set last.
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
      // Phase 1b work-packet item 6: a `tt start`-launched, CLI-driven
      // conductor has no in-process JS hook (unlike setupConductor's
      // `piEnvFor` callback in the test harness) that a static, on-disk
      // reviewer script could use to learn the *live* candidate sha at
      // dispatch time — a real reviewer is simply told this directly, but
      // a scripted stand-in has to be handed it some other way. This env
      // var is that other way (see also `#runFreeze`'s write of the same
      // value to `<run>/candidate-sha.txt`, for a script that can read a
      // file but not env vars set at its own process's spawn time).
      TT_CANDIDATE_SHA: this.#state.phase.candidate?.sha,
      // Same reasoning as TT_CANDIDATE_SHA above: a `tt start`-driven run
      // has one on-disk reviewer script shared by all three dispatches (no
      // `piEnvFor` to hand each its own), so it needs a way to report the
      // *correct* M/A/B letter for whichever dispatch it actually is.
      TT_REVIEWER: reviewer,
    };

    let helloResolve!: (r: HelloResult) => void;
    const helloPromise = new Promise<HelloResult>((resolve) => {
      helloResolve = resolve;
    });
    let doneResolve!: () => void;
    const donePromise = new Promise<void>((resolve) => {
      doneResolve = resolve;
    });
    let discoveryResolve!: () => void;
    const discoveryPromise = new Promise<void>((resolve) => {
      discoveryResolve = resolve;
    });

    // design §2's "A and B are fixed for the phase (kept across its repair
    // rounds)": a real Pi process is spawned fresh per dispatch either way
    // (there is no long-lived RPC connection to hand across repair rounds),
    // but a stable, reviewer-keyed session directory (not one keyed by this
    // dispatch's own actionId, unlike a worker attempt's per-attempt
    // session) lets Pi's own session resume carry the conversation forward
    // across them — reused for every dispatch of the SAME reviewer in this
    // run, exactly like the worker's session directory is reused across a
    // crash-recovered attempt. `noSession` (no persistence at all) only for
    // fake-pi, which does not understand sessions.
    const reviewerPiCommand = this.#resolvePiCommand("reviewer");
    const sessionDir = path.join(this.#paths.sessions, `reviewer-${reviewer}`);
    fs.mkdirSync(sessionDir, { recursive: true });
    // design §2: M keeps one session for the run; A and B are kept across
    // the phase's repair rounds. Continue the previous round's session.
    const continueSession = hasSessionFile(sessionDir);
    // #+TT_MODELS per-seat: M, A and B each resolve their seat's own model
    // first, else the role's shared `reviewer` model (see planModelSelector).
    const reviewerProviderModel = this.#providerModelFor?.("reviewer", reviewer);

    const agent = spawnPiAgent({
      command: reviewerPiCommand,
      args: [
        ...this.#resolvePiArgsPrefix("reviewer"),
        ...launchArgs("reviewer", {
          sessionDir,
          continueSession,
          noSession: reviewerPiCommand !== undefined,
          provider: reviewerProviderModel?.provider,
          model: reviewerProviderModel?.model,
        }),
      ],
      cwd: candidateDir,
      env,
      role: "reviewer",
      agentId,
      streamFile,
      secrets: this.#secretMaskable,
      abortGraceMs: this.#deadlines.abortGraceMs,
      termGraceMs: this.#deadlines.termGraceMs,
      onEvent: (event) => {
        this.#noteActivity(agentId, event);
        this.#trackRunTokens(agentId, event);
        // Plan 06b: record the files this reviewer itself read, so a
        // met/fits verdict can be required to cite one of them.
        if ((event as { type?: string }).type === "tool_execution_start") {
          const e = event as { toolName?: string; args?: unknown };
          if (e.toolName === "read") {
            const a = e.args as Record<string, unknown> | undefined;
            const file = a?.path ?? a?.file ?? a?.file_path;
            if (typeof file === "string" && file.trim().length > 0) {
              const set = this.#reviewerReads.get(reviewer) ?? new Set<string>();
              set.add(file.replace(/^\.\//, ""));
              this.#reviewerReads.set(reviewer, set);
            }
          }
          // A `sh` tool call is a command the reviewer ran (findings
          // F-contract-12, disc-M-33).
          if (e.toolName === "sh") {
            const a = e.args as { command?: unknown } | undefined;
            if (typeof a?.command === "string" && a.command.trim().length > 0) {
              const set = this.#reviewerCommands.get(reviewer) ?? new Set<string>();
              set.add(a.command.trim());
              this.#reviewerCommands.set(reviewer, set);
            }
          }
        }
        if ((event as { type?: string }).type === "agent_settled") settleWaiters.splice(0).forEach((f) => f());
      },
    });
    // A reviewer that settles without the submission its turn owes must fail
    // fast (re-dispatch once, then BLOCKED), not wait out reviewMs: observed
    // live with deepseek, where two reviewers settled after turn 2 without
    // calling submit_review and the run idled for the full review deadline.
    const settleWaiters: Array<() => void> = [];
    const nextSettle = () => new Promise<"settled">((resolve) => settleWaiters.push(() => resolve("settled")));

    const handle: AgentHandle = {
      agent,
      role: "reviewer",
      agentId,
      helloResolve,
      helloPromise,
      shGroups: new Set(),
      doneResolve,
      donePromise,
      discoveryResolve,
      discoveryPromise,
    };
    this.#agents.set(agentId, handle);
    // design §9.3's "agent attempt" reconciliation applies to a reviewer's
    // attempt exactly as it does a worker's: recorded here (with pgid) so a
    // crash mid-review leaves a `#reconcileOne`-visible in-flight entry.
    this.#log.intent(actionId, { reviewer, agentId, pgid: agent.pgid });

    try {
      const hello = await raceTimeout(helloPromise, this.#deadlines.helloTimeoutMs, "hello");
      if (hello === "timeout") {
        await agent.terminate();
        this.#log.completion(actionId, { reviewer, ok: false, reason: "hello timed out" });
        this.#reviewTimedOut(reviewer, dispatchCandidate);
        return;
      }
      if (!hello.ok) {
        await agent.terminate();
        // design §2.1: a tool-set mismatch is a launch failure, not a
        // warning — straight to BLOCKED, no re-dispatch spent on it.
        if (hello.mismatch) {
          this.#log.completion(actionId, { reviewer, ok: false, reason: "tool-set mismatch" });
          this.#applyEvent({ type: "LAUNCH_FAILED", role: "reviewer", reviewer, ...hello.mismatch });
        } else {
          this.#log.completion(actionId, { reviewer, ok: false, reason: "hello failed" });
          this.#reviewTimedOut(reviewer, dispatchCandidate);
        }
        return;
      }

      const reviewTimeout = this.#withStallWatch(
        agentId,
        agent,
        cancelableTimeout(this.#deadlines.reviewMs, "timeout" as const),
        "Owner (conductor): no progress for a while. Finish this review turn now and call the submit tool it asks for.",
      );

      if (this.#stubReviews) {
        await agent.prompt(
          this.#agentPrompt(buildReviewerPrompt(this.#state.phase, reviewer, this.#secretNames, this.#state.phase.ownerDirectives)),
        );
        const outcome = await Promise.race([donePromise.then(() => "submitted" as const), reviewTimeout.promise]);
        reviewTimeout.cancel();
        if (outcome === "submitted") {
          await agent.terminate();
          this.#log.completion(actionId, { reviewer, ok: true });
          return;
        }
        await agent.terminate();
        this.#log.completion(actionId, { reviewer, ok: false, reason: "timeout" });
        this.#reviewTimedOut(reviewer, dispatchCandidate);
        return;
      }

      // Work packet 2a's real two-turn review (design §6.1's REVIEWING, §3.3):
      // turn 1 shows the candidate/diff/records EXCEPT the worker's own
      // disclosure and requires submit_discovery; only once that is
      // accepted (#onSubmit resolves discoveryPromise) does turn 2 — the
      // worker's disclosed decisions, open findings/corrections — ever get
      // sent, and only then is submit_review (with a real ballot per
      // votable decision) accepted at all (#onSubmit's own turn-order
      // check). One shared `reviewMs` deadline covers both turns.
      const settled1 = nextSettle();
      await agent.prompt(this.#agentPrompt(this.#buildReviewerTurn1Prompt(reviewer)));
      const turn1 = await Promise.race([discoveryPromise.then(() => "discovered" as const), reviewTimeout.promise, settled1]);
      if (turn1 !== "discovered") {
        reviewTimeout.cancel();
        await agent.terminate();
        const why = turn1 === "settled" ? "settled without submit_discovery (turn 1)" : "timeout (turn 1: submit_discovery)";
        this.#log.completion(actionId, { reviewer, ok: false, reason: why });
        this.#reviewTimedOut(reviewer, dispatchCandidate);
        return;
      }

      // Turn 2 is a fresh prompt only after turn 1 has fully settled: sending
      // it while the reviewer is still finishing turn 1 makes Pi queue it as
      // a follow-up of turn 1 (observed live: reviewers then repeated
      // submit_discovery or settled without submit_review).
      const turn1Settled = await Promise.race([settled1, reviewTimeout.promise]);
      if (turn1Settled === "timeout") {
        reviewTimeout.cancel();
        await agent.terminate();
        this.#log.completion(actionId, { reviewer, ok: false, reason: "timeout (turn 1 did not settle)" });
        this.#reviewTimedOut(reviewer, dispatchCandidate);
        return;
      }
      // Plan 2c discovery barrier (design §3.3): no reviewer gets turn 2
      // until all three have finished turn 1 on this candidate, so every
      // turn-2 prompt lists the same merged set of records and every
      // reviewer can ballot on every other reviewer's discoveries. Without
      // it, M's turn 2 in dogfood run 4ec5e0f8 ran before A and B had
      // discovered anything, so M never balloted 16 records and the tally
      // failed on missing ballots. The wait counts against this reviewer's
      // own review deadline.
      const candidateForBarrier = this.#state.phase.candidate?.sha ?? "";
      this.#arriveAtDiscoveryBarrier(reviewer, candidateForBarrier);
      const barrier = await Promise.race([
        this.#discoveryBarrierReleased(candidateForBarrier).then(() => "released" as const),
        reviewTimeout.promise,
      ]);
      if (barrier === "timeout") {
        reviewTimeout.cancel();
        await agent.terminate();
        this.#log.completion(actionId, { reviewer, ok: false, reason: "timeout (waiting for the other reviewers' discovery)" });
        this.#reviewTimedOut(reviewer, dispatchCandidate);
        return;
      }
      const settled2 = nextSettle();
      await agent.prompt(this.#agentPrompt(this.#buildReviewerTurn2Prompt(reviewer, handle)));
      let turn2 = await Promise.race([donePromise.then(() => "submitted" as const), reviewTimeout.promise, settled2]);
      // Plan 06c (A2): the review stage owns this turn's outcome. A turn that
      // settles WITHOUT submit_review is asked once more, naming the missing
      // tool; only a second silent settle counts as the seat timing out, and
      // a submission in the second turn counts like any other. The re-prompt
      // shares the reviewer's one deadline.
      if (turn2 === "settled") {
        this.#log.append("review_reprompt", { reviewer, agentId, reason: "turn 2 settled without submit_review" });
        const settled3 = nextSettle();
        await agent.prompt(
          this.#agentPrompt(
            `You ended your review turn without calling submit_review. That tool is required to finish this review. ` +
              `Call submit_review now with reviewer, phaseId, candidateSha, contractVersion, correctionStatements, ` +
              `findingStatements, items and arch, then end your turn.`,
          ),
        );
        turn2 = await Promise.race([donePromise.then(() => "submitted" as const), reviewTimeout.promise, settled3]);
      }
      reviewTimeout.cancel();
      if (turn2 === "submitted") {
        await agent.terminate();
        this.#log.completion(actionId, { reviewer, ok: true });
        return;
      }
      await agent.terminate();
      const why2 = turn2 === "settled" ? "settled without submit_review (turn 2, after one re-prompt)" : "timeout (turn 2: submit_review)";
      this.#log.completion(actionId, { reviewer, ok: false, reason: why2 });
      this.#reviewTimedOut(reviewer, dispatchCandidate);
    } finally {
      this.#agents.delete(agentId);
    }
  }

  // -- plan 04a: evaluation -----------------------------------------------

  /** A timeout or loss applies only while the phase is still evaluating the
   * candidate this dispatch was started for. A late one is logged and
   * dropped, like a stale review. */
  #evaluationTimedOut(messageType: MessageType, dispatchCandidate: string | undefined): void {
    const phase = this.#state.phase;
    if (phase.phase !== "EVALUATING" || phase.candidate?.sha !== dispatchCandidate) {
      this.#log.append("stale_evaluation_ignored", { messageType, dispatchCandidate, phase: phase.phase });
      return;
    }
    this.#applyEvent({ type: "EVALUATION_TIMED_OUT", messageType });
  }

  /** Plan 05j: the round's curator agent. It reads every new raw message of
   * every type and every open entry, and proposes link/open/retitle through
   * `curate_entries` (the only ops its tool accepts). A submit, a timeout or
   * a launch failure all end the pass, so the evaluators are never wedged. */
  async #runCurator(actionId: string, candidateSha: string): Promise<void> {
    const agentId = `curator-${actionId}`;
    const streamFile = path.join(this.#paths.stream, `${agentId}.jsonl`);
    const candidateDir = this.#candidateDir();
    const model = this.#providerModelFor?.("curator");
    const env: NodeJS.ProcessEnv = {
      ...this.#extraEnv,
      ...this.#piEnvFor?.("curator", agentId),
      TT_SOCKET: this.#paths.sock,
      TT_SEARCH_ROOTS: [candidateDir, this.#paths.refs].join(path.delimiter),
      TT_SH_WAIT_MS: String(this.#deadlines.shCommandMs + 30_000),
      TT_RUN_DIR: this.#runDir,
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
      TT_CANDIDATE_SHA: candidateSha,
    };

    let helloResolve!: (r: HelloResult) => void;
    const helloPromise = new Promise<HelloResult>((resolve) => {
      helloResolve = resolve;
    });
    let doneResolve!: () => void;
    const donePromise = new Promise<void>((resolve) => {
      doneResolve = resolve;
    });
    // A curator that settles without submitting (or with an empty pass) ends
    // the round at once instead of waiting out evaluateMs.
    const settleWaiters: Array<() => void> = [];
    const nextSettle = () => new Promise<"settled">((resolve) => settleWaiters.push(() => resolve("settled")));
    const agent = spawnPiAgent({
      command: this.#resolvePiCommand("curator"),
      args: [
        ...this.#resolvePiArgsPrefix("curator"),
        ...launchArgs("curator", { noSession: true, provider: model?.provider, model: model?.model }),
      ],
      cwd: candidateDir,
      env,
      role: "curator",
      agentId,
      streamFile,
      secrets: this.#secretMaskable,
      abortGraceMs: this.#deadlines.abortGraceMs,
      termGraceMs: this.#deadlines.termGraceMs,
      onEvent: (event) => {
        this.#noteActivity(agentId, event);
        this.#trackRunTokens(agentId, event);
        if ((event as { type?: string }).type === "agent_settled") settleWaiters.splice(0).forEach((f) => f());
      },
    });
    const handle: AgentHandle = {
      agent,
      role: "curator",
      agentId,
      helloResolve,
      helloPromise,
      shGroups: new Set(),
      doneResolve,
      donePromise,
      discoveryResolve: () => undefined,
      discoveryPromise: Promise.resolve(),
    };
    this.#agents.set(agentId, handle);
    this.#log.intent(actionId, { agentId, candidateSha });
    try {
      const hello = await Promise.race([
        raceTimeout(helloPromise, this.#deadlines.helloTimeoutMs, "hello"),
        agent.waitExit().then(() => "exited" as const),
      ]);
      if (hello === "timeout" || hello === "exited" || !hello.ok) {
        await agent.terminate();
        this.#log.completion(actionId, { candidateSha, ok: false, reason: hello === "exited" ? "curator exited before hello" : "curator did not start" });
        return;
      }
      await agent.prompt(this.#agentPrompt(this.#buildCuratorPrompt()));
      const settled = nextSettle();
      // A cancelable deadline (never `raceTimeout`, whose timer survives the
      // race and keeps the process alive): the conductor's own `stop()`
      // terminates the agent, `waitExit` then ends the wait, and the finally
      // cancels the timer — so a spawned conductor still exits by itself.
      const timeout = cancelableTimeout(this.#deadlines.evaluateMs, "timeout" as const);
      let outcome: "submitted" | "settled" | "exited" | "timeout";
      try {
        outcome = await Promise.race([
          donePromise.then(() => "submitted" as const),
          settled,
          agent.waitExit().then(() => "exited" as const),
          timeout.promise,
        ]);
      } finally {
        timeout.cancel();
      }
      await agent.terminate();
      this.#log.completion(actionId, {
        candidateSha,
        ok: outcome === "submitted",
        ...(outcome === "submitted" ? {} : { reason: outcome === "settled" ? "curator settled without submitting" : outcome === "exited" ? "curator exited" : "curator timed out" }),
      });
    } finally {
      this.#agents.delete(agentId);
      this.#finishCurator(candidateSha);
    }
  }

  /** Plan 05j: what the curator sees — every message of every type this
   * round, and every open entry, with the op allow-list. */
  #buildCuratorPrompt(): string {
    const phase = this.#state.phase;
    const openEntries = (phase.entries ?? []).filter((e) => e.state === "open");
    const lines = [
      "You are the review curator for this round. Link each new message to the open entry it is the SAME TOPIC as, so the owner sees each topic once.",
      "Call curate_entries with a `proposals` array. Every proposal is exactly one of:",
      "- link: { op: \"link\", messageId, entryId, anchor, reason } — accepted only when the message and the entry share an anchor (overlapping file line ranges, the same decision id, or the same plan clause).",
      "- open: { op: \"open\", title, messageId, anchor? } — open a new topic when no open entry matches.",
      "- retitle: { op: \"retitle\", entryId, title } — improve a title (at most 80 characters, never cut mid-word).",
      "You may not drop, resolve, merge or change a type. Leave a message alone rather than inventing an anchor.",
      "",
      `Candidate: ${phase.candidate?.sha ?? ""}`,
    ];
    const byType: Array<[MessageType, string]> = [
      ["blocker", "Blockers"],
      ["finding", "Findings"],
      ["tradeoff", "Trade-offs"],
    ];
    for (const [type, label] of byType) {
      const msgs = (phase.messages ?? []).filter((m) => m.type === type && (m.state === "raw" || m.state === "published"));
      lines.push("", `${label}:`);
      if (msgs.length === 0) lines.push("- (none)");
      for (const m of msgs) lines.push(`- ${m.id} [${m.state}] ${m.title}\n    summary: ${m.summary}\n    evidence: ${(m.evidence ?? []).join(" | ")}`);
    }
    lines.push("", "Open entries:");
    if (openEntries.length === 0) lines.push("- (none)");
    for (const e of openEntries) lines.push(`- ${e.id} [${e.type}] ${e.title} (${formatAnchor(e.anchor)}; ${e.links.length} linked)`);
    return lines.join("\n");
  }

  /** The plan's calendars.yaml / products.yaml, or null when neither is
   * readable. Never throws: a missing catalog makes a brief say its example
   * is unverified, it never invents one. */
  #briefCatalogs(): Catalogs | null {
    const tryRead = (rel: string): string | undefined => {
      for (const candidate of [path.join(this.#plan.repo, rel), rel]) {
        try {
          return fs.readFileSync(candidate, "utf8");
        } catch {
          // keep looking
        }
      }
      return undefined;
    };
    const calendars = tryRead("config/index/calendars.yaml");
    const products = tryRead("config/index/products.yaml");
    if (!calendars && !products) return null;
    return parseCatalogs(calendars, products);
  }

  /** Decision briefs (after evaluation): record a deterministic brief for
   * every open owner item that has none yet, so the owner always has a brief
   * above the evidence even when the evaluator's model did not (or could
   * not) call submit_brief. The evaluator's own brief is never overwritten.
   * Idempotent. */
  #refreshBriefs(): void {
    if (this.#closed || !this.#briefsEnabled) return;
    const phase = this.#state.phase;
    // The brief pass runs when the owner is actually needed (F-8), never
    // during RESOLVING or GATING: briefing a proceeding phase costs a model
    // run for an item the owner may never see (finding disc-B-60).
    if (phase.phase !== "AWAITING_OWNER" && phase.phase !== "BLOCKED") return;
    const C = phase.candidate?.sha;
    // Per candidate, not per request id: the plan asks for one brief per
    // round, so a new candidate's item is rewritten (finding M-30). A brief
    // recorded for another candidate is stale.
    const existing = new Set((phase.briefs ?? []).filter((b) => b.candidateSha === C).map((b) => b.requestId));
    const itemIds: string[] = [
      ...phase.ownerRequests.filter((r) => r.status === "open").map((r) => r.id),
      ...this.#liveReservedDecisions(phase).map((d) => d.id),
      ...this.#ownerMarkedEntries(phase).map((e) => e.id),
    ];
    const missing = itemIds.filter((id) => !existing.has(id));
    // Plan 05k (OD-6/OD-7): an item whose ONLY brief on this candidate is a
    // backstop gets the writer re-dispatched once, so the owner can still
    // decide from a real brief; the retry is recorded so a later park never
    // dispatches again on this candidate. A model brief (no
    // `noRecommendationReason`) is never retried. OD-7: only an
    // AWAITING_OWNER park counts as the later park; a BLOCKED phase writes
    // any missing briefs but never spends the retry (it auto-stops right after
    // dispatch, so a retry there could never submit).
    const retried = new Set(phase.briefRetries ?? []);
    const backstops =
      phase.phase !== "AWAITING_OWNER"
        ? []
        : itemIds.filter((id) => {
            if (!existing.has(id) || retried.has(`${C}::${id}`)) return false;
            const brief = (phase.briefs ?? []).find((b) => b.requestId === id && b.candidateSha === C);
            if (brief === undefined || brief.noRecommendationReason === undefined) return false;
            // The backstop's AWAITING_OWNER episode is remembered when it is
            // recorded; a missing entry is seeded now, so the retry waits for
            // the next AWAITING_OWNER park.
            const key = `${C}::${id}`;
            const recorded = this.#briefBackstopEpisode.get(key);
            if (recorded === undefined) {
              this.#briefBackstopEpisode.set(key, this.#awaitingEpisode);
              return false;
            }
            return this.#awaitingEpisode > recorded;
          });
    // Only ids no running agent already covers are dispatched, and the set is
    // accumulated (never replaced), so a second item opening mid-pass does not
    // double-brief the first agent's ids or wipe its bookkeeping (finding
    // M-18).
    const toRetry = backstops.filter((id) => !this.#briefInFlight.has(id));
    const toDispatch = [...missing, ...toRetry].filter((id) => !this.#briefInFlight.has(id));
    if (toDispatch.length === 0) return;
    if (toRetry.length > 0) {
      // Record the retry BEFORE dispatching, so a crash or a second failure
      // cannot make it unbounded (OD-6).
      this.#applyEvent({ type: "BRIEF_RETRY_ATTEMPTED", candidateSha: C ?? "", requestIds: toRetry });
    }
    const actionId = this.#log.actionId("briefs");
    for (const id of toDispatch) this.#briefInFlight.add(id);
    void this.#runBriefAgent(actionId, toDispatch).catch((err) => {
      // A spawn/intent throw happens before #runBriefAgent's own finally, so
      // clean the ids and record the backstop here too; otherwise they would
      // stay 'in flight' and every later pass would skip them (finding A-24).
      this.#logUnexpected("briefs", err);
      for (const id of toDispatch) this.#briefInFlight.delete(id);
      this.#recordFallbackBriefs(toDispatch, "the brief writer could not start");
    });
  }

  /** The live, flagged reserved decisions of the current candidate. A
   * reserved decision never becomes an owner request, so without a brief it
   * reaches the owner as an engineer note. A decision the owner already
   * overrode is settled and no longer needs one (finding M-11). */
  #liveReservedDecisions(phase = this.#state.phase): Decision[] {
    const C = phase.candidate?.sha;
    return phase.decisions.filter((d) => {
      if (d.class !== "reserved" || d.amendment || d.supersededBy || d.supersededByCorrection) return false;
      if (!isLiveDecision(d)) return false;
      if (C && d.boundCandidateSha !== C) return false;
      return !phase.overrides.some((o) => o.decisionId === d.id && (!C || o.boundCandidateSha === C));
    });
  }

  /** The review entries the owner has explicitly MARKED: a live entry with a
   * linked message the owner refused. Briefing every live entry would show it
   * twice and bury the real decisions (findings M-58/disc-B-18). */
  #ownerMarkedEntries(phase = this.#state.phase): Array<{ id: string; title?: string; messages?: Array<{ id: string; title?: string; evidence?: string[] }> }> {
    const refused = new Set((phase.messages ?? []).filter((m) => m.state === "refused").map((m) => m.id));
    return (phase.entries ?? []).filter(
      (e) => (e as { state?: string }).state === "open" && ((e as { links?: Array<{ messageId: string }> }).links ?? []).some((l) => refused.has(l.messageId)),
    ) as never;
  }

  /** Every live entry, marked or not. They are NOT briefed, but they are
   * concerns: a same-concern trade-off or finding that never became an owner
   * item is exactly the T-54 silence `related` must surface (OD-2 / D-B-77). */
  #liveEntriesForRelated(phase = this.#state.phase): Array<{ id: string; title?: string; messages?: Array<{ id: string; title?: string; evidence?: string[] }> }> {
    return (phase.entries ?? []).filter((e) => (e as { state?: string }).state === "open") as never;
  }

  /** One open item's concern (the files and plan clauses it touches), from its
   * linked finding and messages, so `related` can name a bigger silence on the
   * same concern. */
  #concernFor(id: string, question: string, recordIds: string[], messageIds: string[] = []): OpenItemConcern {
    const phase = this.#state.phase;
    const files = new Set<string>();
    const planRefs = new Set<string>();
    for (const f of phase.findings) {
      if (!recordIds.includes(f.id)) continue;
      const file = evidenceFile(f.evidence);
      if (file) files.add(file);
    }
    for (const m of phase.messages ?? []) {
      const linked = messageIds.includes(m.id) || (m.sourceRecordId !== undefined && recordIds.includes(m.sourceRecordId));
      if (!linked) continue;
      if (m.planRef) planRefs.add(m.planRef);
      if (m.anchor?.path) files.add(m.anchor.path);
      for (const ev of m.evidence ?? []) {
        const file = evidenceFile(ev);
        if (file) files.add(file);
      }
    }
    return { id, question, files: [...files], planRefs: [...planRefs] };
  }

  /** Every open owner item of the phase as a plain concern, so a brief's
   * `related` can list the others that touch the same file or plan clause. */
  #briefConcerns(): OpenItemConcern[] {
    const phase = this.#state.phase;
    const out: OpenItemConcern[] = [];
    for (const r of phase.ownerRequests.filter((r) => r.status === "open")) {
      const linked = [r.linkedFindingId, r.linkedDecisionId, r.linkedMessageId, r.linkedCorrectionId].filter((v): v is string => typeof v === "string");
      // The owner-facing `related` label must stay plain, so it uses the brief's
      // own question when there is one and otherwise strips the request's
      // engineer prose (finding A-36).
      const question = (phase.briefs ?? []).find((b) => b.requestId === r.id)?.question ?? stripCounts(stripCodeTokens(r.reason)).replace(/\s+/g, " ").trim();
      out.push(this.#concernFor(r.id, question, linked));
    }
    for (const d of this.#liveReservedDecisions(phase)) out.push(this.#concernFor(d.id, stripCounts(stripCodeTokens(d.choice)).replace(/\s+/g, " ").trim(), [d.id]));
    // Every live entry, not only the marked ones: a same-concern trade-off or
    // finding that never became an owner item is the T-54 silence the goal
    // says `related` must surface (OD-2 / D-B-77).
    for (const e of this.#liveEntriesForRelated(phase)) {
      const messageIds = (e.messages ?? []).map((m) => m.id);
      out.push(this.#concernFor(e.id, stripCounts(stripCodeTokens(e.title ?? "")).replace(/\s+/g, " ").trim(), [], messageIds));
    }
    return out;
  }

  /** The deterministic backstop brief for every item the evaluator's model did
   * not cover: an owner request gets a resolve brief, a reserved decision an
   * override brief, a live entry an entry brief. It never asserts an
   * unchecked impact, omits a recommendation, and carries the conductor's own
   * `related`. Idempotent for this candidate. */
  #recordFallbackBriefs(ids: readonly string[], reason = "the brief writer did not run"): void {
    const phase = this.#state.phase;
    const C = phase.candidate?.sha;
    const existing = new Set((phase.briefs ?? []).filter((b) => b.candidateSha === C).map((b) => b.requestId));
    const concerns = this.#briefConcerns();
    const catalogs = this.#briefCatalogs();
    const briefs: DecisionBrief[] = [];
    for (const id of ids) {
      if (existing.has(id)) {
        // A retried item keeps its backstop (the model brief never replaced
        // it); record why the retry did not help (OD-6).
        const held = (phase.briefs ?? []).find((b) => b.requestId === id && b.candidateSha === C);
        if (held?.noRecommendationReason !== undefined) this.#log.append("brief_retry_failed", { requestId: id, reason });
        continue;
      }
      const concern = concerns.find((c) => c.id === id);
      const opts = { allItems: concerns, files: concern?.files, planRefs: concern?.planRefs, noRecommendationReason: reason };
      const request = phase.ownerRequests.find((r) => r.id === id);
      if (request) {
        briefs.push({ ...fallbackBrief(request, { catalogs, ...opts }), candidateSha: C });
        this.#briefBackstopEpisode.set(`${C}::${id}`, this.#awaitingEpisode);
        continue;
      }
      const decision = this.#liveReservedDecisions(phase).find((d) => d.id === id);
      if (decision) {
        briefs.push({ ...fallbackDecisionBrief(decision, opts), candidateSha: C });
        this.#briefBackstopEpisode.set(`${C}::${id}`, this.#awaitingEpisode);
        continue;
      }
      const entry = this.#ownerMarkedEntries(phase).find((e) => e.id === id);
      if (entry) {
        briefs.push({ ...fallbackEntryBrief(entry, opts), candidateSha: C });
        this.#briefBackstopEpisode.set(`${C}::${id}`, this.#awaitingEpisode);
      }
    }
    if (briefs.length > 0) this.#applyEvent({ type: "BRIEFS_RECORDED", briefs });
  }

  /** The brief-writing evaluator pass. One fresh evaluator-role agent is
   * shown every item that still needs a brief and the plan's calendars, and
   * writes one owner-readable brief per item through `submit_brief`. A timeout
   * or a settle without complete coverage leaves the deterministic backstop to
   * fill the rest, so the owner is never left without a brief. */
  async #runBriefAgent(actionId: string, ids: readonly string[]): Promise<void> {
    const dispatchCandidate = this.#state.phase.candidate?.sha;
    const agentId = `briefs-${actionId}`;
    const streamFile = path.join(this.#paths.stream, `${agentId}.jsonl`);
    const candidateDir = this.#candidateDir();
    const env: NodeJS.ProcessEnv = {
      ...this.#extraEnv,
      ...this.#piEnvFor?.("evaluator", agentId),
      TT_SOCKET: this.#paths.sock,
      TT_SEARCH_ROOTS: [candidateDir, this.#paths.refs].join(path.delimiter),
      TT_SH_WAIT_MS: String(this.#deadlines.shCommandMs + 30_000),
      TT_RUN_DIR: this.#runDir,
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
      TT_CANDIDATE_SHA: dispatchCandidate,
      TT_BRIEF: "1",
    };
    let helloResolve!: (r: HelloResult) => void;
    const helloPromise = new Promise<HelloResult>((resolve) => {
      helloResolve = resolve;
    });
    let doneResolve!: () => void;
    const donePromise = new Promise<void>((resolve) => {
      doneResolve = resolve;
    });
    const settleWaiters: Array<() => void> = [];
    const nextSettle = () => new Promise<"settled">((resolve) => settleWaiters.push(() => resolve("settled")));
    let fallbackReason = "the brief writer did not run";
    const evaluatorPiCommand = this.#resolvePiCommand("evaluator");
    const providerModel = this.#providerModelFor?.("evaluator");
    const agent = spawnPiAgent({
      command: evaluatorPiCommand,
      args: [
        ...this.#resolvePiArgsPrefix("evaluator"),
        ...launchArgs("evaluator", { noSession: evaluatorPiCommand !== undefined, provider: providerModel?.provider, model: providerModel?.model }),
      ],
      cwd: candidateDir,
      env,
      role: "evaluator",
      agentId,
      streamFile,
      secrets: this.#secretMaskable,
      abortGraceMs: this.#deadlines.abortGraceMs,
      termGraceMs: this.#deadlines.termGraceMs,
      onEvent: (event) => {
        this.#noteActivity(agentId, event);
        this.#trackRunTokens(agentId, event);
        if ((event as { type?: string }).type === "agent_settled") settleWaiters.splice(0).forEach((f) => f());
      },
    });
    const handle: AgentHandle = {
      agent,
      role: "evaluator",
      agentId,
      helloResolve,
      helloPromise,
      shGroups: new Set(),
      doneResolve,
      donePromise,
      discoveryResolve: () => undefined,
      discoveryPromise: Promise.resolve(),
      briefInFlightIds: new Set(ids),
      briefSubmitted: new Set(),
    };
    this.#agents.set(agentId, handle);
    this.#log.intent(actionId, { agentId, pgid: agent.pgid, briefs: ids });
    try {
      const hello = await Promise.race([
        raceTimeout(helloPromise, this.#deadlines.helloTimeoutMs, "hello"),
        agent.waitExit().then(() => "exited" as const),
      ]);
      if (hello === "timeout" || hello === "exited") {
        await agent.terminate();
        fallbackReason = hello === "exited" ? "the brief writer exited before it started" : "the brief writer did not start in time";
        this.#log.completion(actionId, { ok: false, reason: hello === "exited" ? "brief agent exited before hello" : "hello timed out" });
        return;
      }
      if (!hello.ok) {
        await agent.terminate();
        fallbackReason = hello.mismatch ? "the brief writer's tools did not match at launch" : "the brief writer failed to start";
        this.#log.completion(actionId, { ok: false, reason: hello.mismatch ? "tool-set mismatch" : "hello failed" });
        // A tool-set mismatch is NOT a phase launch failure here: the brief
        // pass runs only in AWAITING_OWNER/BLOCKED, where no LAUNCH_FAILED
        // transition exists, so applying the event would be rejected and throw
        // (OD-2 / D-M-70). Record why and let the finally backstop cover it.
        if (hello.mismatch) {
          this.#log.append("brief_agent_launch_rejected", { agentId, expected: hello.mismatch.missing, extra: hello.mismatch.extra });
        }
        return;
      }
      const briefTimeout = this.#withStallWatch(
        agentId,
        agent,
        cancelableTimeout(this.#deadlines.briefMs, "timeout" as const),
        "Owner (conductor): no progress for a while. Finish now and call submit_brief for each item listed.",
      );
      const settled = nextSettle();
      await agent.prompt(this.#agentPrompt(this.#buildBriefPrompt(ids)));
      const outcome = await Promise.race([
        donePromise.then(() => "submitted" as const),
        briefTimeout.promise,
        settled,
        agent.waitExit().then(() => "exited" as const),
      ]);
      briefTimeout.cancel();
      await agent.terminate();
      if (outcome === "timeout") fallbackReason = "the brief writer timed out";
      else if (outcome === "exited") fallbackReason = "the brief writer exited";
      else if (outcome === "settled") fallbackReason = "the brief writer settled without covering every item";
      else fallbackReason = "the brief writer did not cover every item";
      this.#log.completion(actionId, { ok: outcome === "submitted", reason: outcome === "submitted" ? undefined : outcome });
    } finally {
      this.#agents.delete(agentId);
      // Remove only THIS agent's ids: a later agent may be covering others
      // (finding M-18).
      for (const id of ids) this.#briefInFlight.delete(id);
      // Whatever the model did not cover, the backstop fills in with the
      // reason the owner reads where the recommendation would be. Stale (the
      // phase moved on): nothing to record and the next park re-runs it.
      if (dispatchCandidate === this.#state.phase.candidate?.sha) this.#recordFallbackBriefs(ids, fallbackReason);
    }
  }

  /** The brief-writing evaluator prompt: each item that still needs a brief,
   * its own option ids, the underlying evidence, the other open items on the
   * same concern, and the plan's calendars/products so `today` can name a real
   * market and session time. */
  #buildBriefPrompt(ids: readonly string[]): string {
    const phase = this.#state.phase;
    const catalogs = this.#briefCatalogs();
    const lines: string[] = [
      `You write the owner-facing decision briefs for phase ${phase.phaseId}, candidate ${(phase.candidate?.sha ?? "").slice(0, 9)}.`,
      "The owner must be able to decide from each brief alone, in under a minute.",
      "",
      "For EACH item below call submit_brief exactly once, with:",
      "- question: one plain line, NO code identifiers (no snake_case, no path/file.rs), e.g. \"Should a vendor excluded before a weekend stay excluded when its market reopens?\"",
      "- today: what the system does now, with ONE concrete example naming a real product and session time from the calendars below; a weekday reopen must match the calendar's weekly reopen. If you cannot check it, write \"(example unverified)\".",
      "- ALL owner-facing text (the question, today, impact, the options, the recommendation and the related questions) must stay PLAIN: no file paths and no code identifiers. Cite a claim by its evidence number in square brackets, e.g. '10 s after a reopen[2]'.",
      "- evidence: the numbered list of what you read, e.g. 'message: ...' then 'config: ...' or 'code: path:line'. Every time, count or duration in today, impact or an option must carry its OWN [n] reference into this list; one reference does not cover another claim.",
      "- impact: what the owner would notice (price flow, number of vendors, quality, duration) and ALWAYS whether any market stops publishing. The publishing answer itself must cite the config or code it was checked against, or say it is unverified.",
      "- options: exactly the item's own option ids, each relabelled in plain words with what happens and its cost.",
      "- recommendation: one option id and why, citing the plan or an IC section.",
      "- related: the other item ids below that touch the same file or plan clause.",
      "",
      "Calendars (calendars.yaml):",
      ...(catalogs
        ? Object.entries(catalogs.calendars).map(
            ([name, c]) => `- ${name}: weekly reopen ${c.opens ?? "(none declared)"}; sessions ${Object.entries(c.sessions).map(([s, t]) => `${s} ${t}`).join(", ")}`,
          )
        : ["(none readable)"]),
      "Products (products.yaml):",
      ...(catalogs ? Object.entries(catalogs.products).map(([sym, p]) => `- ${sym}: vendor ${p.vendor ?? "?"}, calendar ${p.calendar ?? "?"}`) : ["(none readable)"]),
      "",
      "Glossary — use these terms as-is and never explain them inline; the brief links each one:",
      ...BRIEF_GLOSSARY.map((g) => `- ${g.term}: ${g.meaning}`),
      "",
      `Items needing a brief (${ids.length}):`,
    ];
    const concerns = this.#briefConcerns();
    for (const id of ids) {
      const request = phase.ownerRequests.find((r) => r.id === id);
      const decision = !request ? this.#liveReservedDecisions(phase).find((d) => d.id === id) : undefined;
      const entry = !request && !decision ? this.#ownerMarkedEntries(phase).find((e) => e.id === id) : undefined;
      const others = concerns.find((c) => c.id === id);
      if (request) {
        lines.push(`- ${id} (owner request, origin ${request.origin}): ${request.reason}`);
        lines.push(`    its options: ${request.options.map((o) => o.id).join(", ")}`);
      } else if (decision) {
        lines.push(`- ${id} (flagged reserved decision, command override): ${decision.choice}`);
        lines.push(`    why it matters: ${decision.whyItMatters}`);
        lines.push("    its options: approve, reject_and_repair; set command to override");
      } else if (entry) {
        lines.push(`- ${id} (owner-marked review entry, command entry): ${entry.title ?? ""}`);
        lines.push("    its options: accept, refuse; set command to entry");
      }
      lines.push(`    same-concern items: ${(others?.planRefs ?? []).join(", ")} ${(others?.files ?? []).join(", ")}`.trim());
    }
    lines.push("", "Call submit_brief once per item above, then finish.");
    return lines.join("\n");
  }

  /** Plan 04a: one fresh evaluator PER MESSAGE TYPE checks that type's raw
   * messages against the candidate's diff and a read-only checkout, then
   * returns through `submit_evaluation`. A submit settles that type; a
   * timeout publishes only that type's raw messages `unevaluated`. */
  async #runEvaluation(actionId: string, messageType: MessageType): Promise<void> {
    const dispatchCandidate = this.#state.phase.candidate?.sha;
    const agentId = `evaluator-${messageType}-${actionId}`;
    const streamFile = path.join(this.#paths.stream, `${agentId}.jsonl`);
    const candidateDir = this.#candidateDir();
    const env: NodeJS.ProcessEnv = {
      ...this.#extraEnv,
      ...this.#piEnvFor?.("evaluator", agentId),
      TT_SOCKET: this.#paths.sock,
      TT_SEARCH_ROOTS: [candidateDir, this.#paths.refs].join(path.delimiter),
      TT_SH_WAIT_MS: String(this.#deadlines.shCommandMs + 30_000),
      TT_RUN_DIR: this.#runDir,
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
      TT_CANDIDATE_SHA: this.#state.phase.candidate?.sha,
      TT_MESSAGE_TYPE: messageType,
    };

    let helloResolve!: (r: HelloResult) => void;
    const helloPromise = new Promise<HelloResult>((resolve) => {
      helloResolve = resolve;
    });
    let doneResolve!: () => void;
    const donePromise = new Promise<void>((resolve) => {
      doneResolve = resolve;
    });

    const evaluatorPiCommand = this.#resolvePiCommand("evaluator");
    const providerModel = this.#providerModelFor?.("evaluator");
    // An evaluator that settles without an ACCEPTED submission fails fast
    // (that type times out and publishes unevaluated); waiting out the whole
    // evaluateMs would wedge a run whose evaluator's submission was refused.
    const settleWaiters: Array<() => void> = [];
    const nextSettle = () => new Promise<"settled">((resolve) => settleWaiters.push(() => resolve("settled")));
    const agent = spawnPiAgent({
      command: evaluatorPiCommand,
      args: [
        ...this.#resolvePiArgsPrefix("evaluator"),
        ...launchArgs("evaluator", {
          noSession: evaluatorPiCommand !== undefined,
          provider: providerModel?.provider,
          model: providerModel?.model,
        }),
      ],
      cwd: candidateDir,
      env,
      role: "evaluator",
      agentId,
      streamFile,
      secrets: this.#secretMaskable,
      abortGraceMs: this.#deadlines.abortGraceMs,
      termGraceMs: this.#deadlines.termGraceMs,
      onEvent: (event) => {
        this.#noteActivity(agentId, event);
        this.#trackRunTokens(agentId, event);
        if ((event as { type?: string }).type === "agent_settled") settleWaiters.splice(0).forEach((f) => f());
      },
    });

    const handle: AgentHandle = {
      agent,
      role: "evaluator",
      agentId,
      helloResolve,
      helloPromise,
      shGroups: new Set(),
      doneResolve,
      donePromise,
      discoveryResolve: () => undefined,
      discoveryPromise: Promise.resolve(),
      messageType,
    };
    this.#agents.set(agentId, handle);
    this.#log.intent(actionId, { agentId, pgid: agent.pgid, messageType });

    try {
      // `waitExit` is raced alongside hello: an evaluator that cannot even
      // start (e.g. no script for the role) must take the timeout path at
      // once, not wait out helloTimeoutMs.
      const hello = await Promise.race([
        raceTimeout(helloPromise, this.#deadlines.helloTimeoutMs, "hello"),
        agent.waitExit().then(() => "exited" as const),
      ]);
      if (hello === "timeout" || hello === "exited") {
        await agent.terminate();
        this.#log.completion(actionId, { messageType, ok: false, reason: hello === "exited" ? "evaluator exited before hello" : "hello timed out" });
        this.#evaluationTimedOut(messageType, dispatchCandidate);
        return;
      }
      if (!hello.ok) {
        await agent.terminate();
        this.#log.completion(actionId, { messageType, ok: false, reason: hello.mismatch ? "tool-set mismatch" : "hello failed" });
        // B-31 / design §2.1: a tool-set mismatch is a launch failure — the
        // whole run stops (BLOCKED) with evidence, exactly as for the worker
        // and the reviewer. Anything else takes the type's timeout path.
        if (hello.mismatch) {
          this.#applyEvent({ type: "LAUNCH_FAILED", role: "evaluator", ...hello.mismatch });
        } else {
          this.#evaluationTimedOut(messageType, dispatchCandidate);
        }
        return;
      }

      const evaluatorTimeout = this.#withStallWatch(
        agentId,
        agent,
        cancelableTimeout(this.#deadlines.evaluateMs, "timeout" as const),
        "Owner (conductor): no progress for a while. Finish now and call submit_evaluation.",
      );
      const settled = nextSettle();
      await agent.prompt(this.#agentPrompt(this.#buildEvaluatorPrompt(messageType)));
      const outcome = await Promise.race([
        donePromise.then(() => "submitted" as const),
        evaluatorTimeout.promise,
        settled,
        agent.waitExit().then(() => "exited" as const),
      ]);
      evaluatorTimeout.cancel();
      if (outcome === "submitted") {
        await agent.terminate();
        this.#log.completion(actionId, { messageType, ok: true });
        return;
      }
      await agent.terminate();
      this.#log.completion(actionId, { messageType, ok: false, reason: outcome });
      this.#evaluationTimedOut(messageType, dispatchCandidate);
    } finally {
      this.#agents.delete(agentId);
    }
  }

  /** Plan 04a: one type's evaluator prompt — the phase contract, the owner
   * directives in force, the settled ledger, the candidate's diff against the
   * base (its read-only checkout is the working directory), and THIS TYPE's
   * raw messages. An owner-refused message of the type is shown so the
   * evaluator can report whether it was addressed. */
  #buildEvaluatorPrompt(messageType: MessageType): string {
    const phase = this.#state.phase;
    const C = phase.candidate?.sha ?? "";
    const mine = (phase.messages ?? []).filter((m) => m.type === messageType);
    const raw = mine.filter((m) => m.state === "raw");
    const refused = mine.filter((m) => m.state === "refused");
    let diff = "";
    try {
      diff = diffText(this.#plan.repo, phase.integrationHead, C);
    } catch {
      diff = "(the diff could not be read)";
    }
    const lines = [
      `You are the ${messageType} evaluator for phase ${phase.phaseId}, candidate ${C.slice(0, 9)} (contract snapshot ${phase.contract.contractVersion.snapshot}). You evaluate ONLY the ${messageType} messages listed below.`,
      `Goal: ${phase.contract.goal}`,
      "",
      "Acceptance criteria:",
      ...phase.contract.acceptance.map((a) => `- ${a}`),
      // Plan 06i (A3): the golden note a `contract` classification must cite.
      ...(phase.contract.golden ? ["", "Golden note (the current source a choice must be re-checked against; a `contract` classification must cite it):", phase.contract.golden] : []),
      ...secretPromptLines(this.#secretNames),
      ...directiveLines(phase.ownerDirectives),
      ...ledgerPromptLines(phase.messages),
      // Plan 05j: the evaluator sees the entries its messages belong to, so a
      // topic raised as another type is visible to it (plan item 3's
      // cross-type view of the round; finding M-18).
      ...entryPromptLines(phase.entries),
      "",
      "Your read-only checkout of the candidate is the working directory. This is the diff against the base:",
      "```diff",
      redactText(diff, this.#secretMaskable),
      "```",
      "",
      `Raw ${messageType} messages to evaluate (${raw.length}) — each publish title must be ONE COMPLETE line of at most 80 characters:`,
      ...raw.map(
        (m) => `- ${m.id}: ${m.title}${m.anchor ? ` (anchor ${m.anchor.path}:${m.anchor.lines[0]}-${m.anchor.lines[1]})` : ""}\n    why: ${m.summary}\n    context: ${m.context}`,
      ),
    ];
    if (refused.length > 0) {
      lines.push(
        "",
        `Owner-refused ${messageType} messages (report whether this candidate addressed each):`,
        ...refused.map((m) => `- ${m.id} "${m.title}" — refused: ${m.settlement?.reason ?? "no reason given"}`),
      );
    }
    // Plan 06b (OD-1 R3b): a majority unmet/deviates item needs the
    // evaluator's substantive re-check against the candidate before it
    // blocks. The check is recorded with what was checked.
    const owedIds = this.#owedItemCheckIds("finding");
    const reverifyItems = itemsNeedingEvaluatorReverify(phase)
      ? phaseItemOutcomes(phase).filter((o) => owedIds.includes(o.item.id))
      : [];
    if (reverifyItems.length > 0) {
      lines.push(
        "",
        "Plan-item re-check: each item below owes an evaluator check. Re-check it against the candidate's code and record exactly what you checked:",
        ...reverifyItems.map(
          (o) =>
            `- ${o.item.id} ${o.item.title}: ${o.outcome}${o.outcome === "met" || o.outcome === "fits" ? " (thin unanimous evidence — audit it)" : " (a majority judged it unmet or deviating)"}; seats: ${o.evidence.join(" | ")}`,
        ),
        "Add one `itemChecks[]` entry = { id, verdict: confirmed|contradicted, evidence } for each item above. `evidence` must cite a file:line in the candidate. Use `contradicted` only when the candidate's code proves the verdict wrong; otherwise `confirmed`.",
      );
    }
    // Plan 06i: every finding and discovered decision gets an impact class
    // through the SAME item-check form. `wrong-output` is a wrong value,
    // offer or output on a reachable path; `contract` contradicts the golden
    // source or the plan; `judgement` is style, hardening or ergonomics.
    // A finding is classified on the `finding` pass; a discovered decision is
    // published as a `tradeoff` message, so it is classified on that pass
    // (finding M-7: the prompt must run for decisions too).
    {
      const rows =
        messageType === "finding"
          ? openFindings(phase).map((f) => `- ${f.id} (finding, raised ${f.raisedBy}, severity ${f.severity}): ${f.evidence}`)
          : messageType === "tradeoff"
            ? discoveredDecisions(phase).map((d) => `- ${d.id} (discovered decision): ${d.choice} — ${d.whyItMatters}`)
            : [];
      if (rows.length > 0) {
        lines.push(
          "",
          "Plan 06i triage: classify each finding and discovered decision below. Add one `itemChecks[]` entry = { id, verdict: confirmed, impact, evidence } for each. `impact` is one of:",
          "- wrong-output: a wrong value, offer or output on a reachable path (it MUST be fixed);",
          "- contract: it contradicts the current golden source or the plan (a drafting decision is no exemption);",
          "- judgement: style, hardening or ergonomics only (a trade-off).",
          "For a `judgement` impact also give `chosen`, `alternative` and `why`; without all three the record escalates to the owner. Do not omit the impact: a record you leave unclassified escalates.",
          ...rows,
        );
      }
    }
    lines.push(
      "",
      "For EACH raw message above, check its claim against the code and call submit_evaluation with exactly one entry:",
      "- publish: title (ONE COMPLETE line of at most 80 characters — rewrite it shorter rather than cutting the raw title mid-word; a title that ends mid-word is refused), summary (at most 3 sentences), context, evidence (the code facts you checked), importance (high|medium|low).",
      "- merge: the message says the same thing as another — name `into` that message id.",
      "- drop: it is trivial, already settled, or not reviewable — give a reason.",
    );
    if (refused.length > 0) {
      lines.push(
        "For each owner-refused message above, name it with `addressed: true` only if this candidate really addressed the owner's reason (which resolves the refusal), or `addressed: false` if it did not. Do not re-raise a refusal.",
      );
    }
    return lines.join("\n");
  }

  // -- plan 04b: the blocker panel -----------------------------------------

  /** A late timeout/loss for a seat whose phase has moved on is logged and
   * dropped, exactly like a stale evaluation or review. */
  #panelSeatUnavailable(blockerId: string, seat: number, dispatchCandidate: string | undefined, reason: string): void {
    const phase = this.#state.phase;
    const panel = phase.panel?.blockers?.[blockerId];
    const seatState = panel?.seats?.[String(seat)];
    if (
      phase.phase !== "EVALUATING" ||
      phase.candidate?.sha !== dispatchCandidate ||
      !panel ||
      panel.decided ||
      // Already voted, or already unavailable after its one retry: a late
      // loss must not be applied twice.
      seatState?.vote !== undefined ||
      (seatState?.unavailable === true && seatState.dispatches >= 2)
    ) {
      this.#log.append("stale_panel_ignored", { blockerId, seat, dispatchCandidate, phase: phase.phase });
      return;
    }
    // The same one-retry rule REVIEWING uses: the first loss clears the
    // in-flight seat so next() re-dispatches it; the second settles it as
    // unavailable, leaving the other seats to decide.
    this.#applyEvent({ type: "PANEL_SEAT_UNAVAILABLE", blockerId, seat, reason });
  }

  /** Plan 04b: one fresh panel seat for one raw blocker. It reads the phase
   * contract, the owner directives, the settled ledger, the blocker and its
   * evidence, and the candidate's diff, then votes `block` or `downgrade`
   * through `submit_panel_vote`. Three of these run in parallel; each has its
   * own deadline and its own one retry. */
  async #runPanelSeat(actionId: string, blockerId: string, seat: number): Promise<void> {
    const dispatchCandidate = this.#state.phase.candidate?.sha;
    const agentId = `panel-${blockerId}-${seat}-${actionId}`;
    const streamFile = path.join(this.#paths.stream, `${agentId}.jsonl`);
    const candidateDir = this.#candidateDir();
    const env: NodeJS.ProcessEnv = {
      ...this.#extraEnv,
      ...this.#piEnvFor?.("panel", agentId),
      TT_SOCKET: this.#paths.sock,
      TT_SEARCH_ROOTS: [candidateDir, this.#paths.refs].join(path.delimiter),
      TT_SH_WAIT_MS: String(this.#deadlines.shCommandMs + 30_000),
      TT_RUN_DIR: this.#runDir,
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
      TT_CANDIDATE_SHA: this.#state.phase.candidate?.sha,
      TT_BLOCKER_ID: blockerId,
      TT_PANEL_SEAT: String(seat),
    };

    let helloResolve!: (r: HelloResult) => void;
    const helloPromise = new Promise<HelloResult>((resolve) => {
      helloResolve = resolve;
    });
    let doneResolve!: () => void;
    const donePromise = new Promise<void>((resolve) => {
      doneResolve = resolve;
    });

    const panelPiCommand = this.#resolvePiCommand("panel");
    // #+TT_MODELS per-seat: `panel.N` first, else the reviewer of this
    // position when panel=reviewers, else the shared `panel` model.
    const providerModel = this.#providerModelFor?.("panel", seat);
    const settleWaiters: Array<() => void> = [];
    const nextSettle = () => new Promise<"settled">((resolve) => settleWaiters.push(() => resolve("settled")));
    const agent = spawnPiAgent({
      command: panelPiCommand,
      args: [
        ...this.#resolvePiArgsPrefix("panel"),
        ...launchArgs("panel", {
          noSession: panelPiCommand !== undefined,
          provider: providerModel?.provider,
          model: providerModel?.model,
        }),
      ],
      cwd: candidateDir,
      env,
      role: "panel",
      agentId,
      streamFile,
      secrets: this.#secretMaskable,
      abortGraceMs: this.#deadlines.abortGraceMs,
      termGraceMs: this.#deadlines.termGraceMs,
      onEvent: (event) => {
        this.#noteActivity(agentId, event);
        this.#trackRunTokens(agentId, event);
        if ((event as { type?: string }).type === "agent_settled") settleWaiters.splice(0).forEach((f) => f());
      },
    });

    const handle: AgentHandle = {
      agent,
      role: "panel",
      agentId,
      helloResolve,
      helloPromise,
      shGroups: new Set(),
      doneResolve,
      donePromise,
      discoveryResolve: () => undefined,
      discoveryPromise: Promise.resolve(),
      blockerId,
      panelSeat: seat,
    };
    this.#agents.set(agentId, handle);
    this.#log.intent(actionId, { agentId, pgid: agent.pgid, blockerId, seat });

    try {
      const hello = await Promise.race([
        raceTimeout(helloPromise, this.#deadlines.helloTimeoutMs, "hello"),
        agent.waitExit().then(() => "exited" as const),
      ]);
      if (hello === "timeout" || hello === "exited") {
        await agent.terminate();
        this.#log.completion(actionId, { blockerId, seat, ok: false, reason: hello === "exited" ? "panel seat exited before hello" : "hello timed out" });
        this.#panelSeatUnavailable(blockerId, seat, dispatchCandidate, "the seat never started");
        return;
      }
      if (!hello.ok) {
        await agent.terminate();
        this.#log.completion(actionId, { blockerId, seat, ok: false, reason: hello.mismatch ? "tool-set mismatch" : "hello failed" });
        // design §2.1: a tool-set mismatch is a launch failure, not a
        // warning — the whole run stops with evidence.
        if (hello.mismatch) {
          this.#applyEvent({ type: "LAUNCH_FAILED", role: "panel", ...hello.mismatch });
        } else {
          this.#panelSeatUnavailable(blockerId, seat, dispatchCandidate, "hello failed");
        }
        return;
      }

      const seatTimeout = this.#withStallWatch(
        agentId,
        agent,
        cancelableTimeout(this.#deadlines.panelMs, "timeout" as const),
        "Owner (conductor): no progress for a while. Finish now and call submit_panel_vote.",
      );
      const settled = nextSettle();
      await agent.prompt(this.#agentPrompt(this.#buildPanelPrompt(blockerId, seat)));
      const outcome = await Promise.race([
        donePromise.then(() => "submitted" as const),
        seatTimeout.promise,
        settled,
        agent.waitExit().then(() => "exited" as const),
      ]);
      seatTimeout.cancel();
      if (outcome === "submitted") {
        await agent.terminate();
        this.#log.completion(actionId, { blockerId, seat, ok: true });
        return;
      }
      await agent.terminate();
      this.#log.completion(actionId, { blockerId, seat, ok: false, reason: outcome });
      this.#panelSeatUnavailable(blockerId, seat, dispatchCandidate, `the seat did not vote (${outcome})`);
    } finally {
      // Defensive: every path above terminates the seat, but a throw between
      // the spawn and its own terminate must not leave a pi process (and its
      // session) behind, invisible to `stop()` once the handle is dropped.
      // `terminate()` is idempotent/memoized, so the normal paths pay nothing.
      await agent.terminate().catch(() => undefined);
      this.#agents.delete(agentId);
    }
  }

  /** Plan 04b: one panel seat's prompt — the phase contract, the owner
   * directives in force, the settled ledger, the candidate's diff against the
   * base, and the blocker (with its evidence). */
  #buildPanelPrompt(blockerId: string, seat: number): string {
    const phase = this.#state.phase;
    const C = phase.candidate?.sha ?? "";
    const message = (phase.messages ?? []).find((m) => m.id === blockerId);
    let diff = "";
    try {
      diff = diffText(this.#plan.repo, phase.integrationHead, C);
    } catch {
      diff = "(the diff could not be read)";
    }
    return [
      `You are panel seat ${seat} of 3, voting on blocker ${blockerId} for phase ${phase.phaseId}, candidate ${C.slice(0, 9)} (contract snapshot ${phase.contract.contractVersion.snapshot}).`,
      `Goal: ${phase.contract.goal}`,
      "",
      "Acceptance criteria:",
      ...phase.contract.acceptance.map((a) => `- ${a}`),
      ...secretPromptLines(this.#secretNames),
      ...directiveLines(phase.ownerDirectives),
      ...ledgerPromptLines(phase.messages),
      "",
      "Your read-only checkout of the candidate is the working directory. This is the diff against the base:",
      "```diff",
      redactText(diff, this.#secretMaskable),
      "```",
      "",
      "The blocker (a raw `blocker` message, and a blocking finding effective at once):",
      `- ${blockerId}: ${message?.title ?? "(the message could not be read)"}`,
      `    why: ${message?.summary ?? ""}`,
      `    context: ${message?.context ?? ""}`,
      `    evidence: ${(message?.evidence ?? []).join("; ")}`,
      "",
      "Check the blocker's claim against the code and vote once with submit_panel_vote:",
      "- `block`: the work must stop until the owner decides. Propose TWO OR THREE options the owner can choose from (id + label) — the escalation reaches the owner with them.",
      "- `downgrade`: the work should not stop for the owner; it becomes an ordinary blocking finding the worker must repair. Give no options.",
      "Either way, give a reason. You are one of three independent seats; vote what the evidence shows.",
    ].join("\n");
  }

  // -- plan 05e: the round panel (trade-offs and blocking findings) ---------

  /** A late timeout/loss for a round-panel seat whose phase has moved on is
   * logged and dropped, exactly like a blocker seat's. */
  #roundPanelSeatUnavailable(seat: number, reason: string): void {
    const phase = this.#state.phase;
    const seatState = phase.panel?.round?.seats?.[String(seat)];
    if (
      phase.phase !== "EVALUATING" ||
      phase.panel?.round?.decided ||
      seatState?.votes !== undefined ||
      (seatState?.unavailable === true && seatState.dispatches >= 2)
    ) {
      this.#log.append("stale_round_panel_ignored", { seat, reason, phase: phase.phase });
      return;
    }
    this.#applyEvent({ type: "ROUND_PANEL_SEAT_UNAVAILABLE", seat, reason });
  }

  /** Plan 05e: one fresh round-panel seat. It reads the phase contract, the
   * owner directives, the ledger, the candidate's diff and every pending item
   * (with its 3a/3b evidence), and returns its batched `keep`/`drop` votes
   * through `submit_round_panel_votes`. Three run in parallel; each has its
   * own deadline and its own one retry. */
  async #runRoundPanelSeat(actionId: string, seat: number): Promise<void> {
    const dispatchCandidate = this.#state.phase.candidate?.sha;
    const agentId = `round-panel-${seat}-${actionId}`;
    const streamFile = path.join(this.#paths.stream, `${agentId}.jsonl`);
    const candidateDir = this.#candidateDir();
    const env: NodeJS.ProcessEnv = {
      ...this.#extraEnv,
      ...this.#piEnvFor?.("panel", agentId),
      TT_SOCKET: this.#paths.sock,
      TT_SEARCH_ROOTS: [candidateDir, this.#paths.refs].join(path.delimiter),
      TT_SH_WAIT_MS: String(this.#deadlines.shCommandMs + 30_000),
      TT_RUN_DIR: this.#runDir,
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
      TT_CANDIDATE_SHA: this.#state.phase.candidate?.sha,
      TT_PANEL_SEAT: String(seat),
      TT_ROUND_PANEL: "1",
    };

    let helloResolve!: (r: HelloResult) => void;
    const helloPromise = new Promise<HelloResult>((resolve) => {
      helloResolve = resolve;
    });
    let doneResolve!: () => void;
    const donePromise = new Promise<void>((resolve) => {
      doneResolve = resolve;
    });

    const panelPiCommand = this.#resolvePiCommand("panel");
    const providerModel = this.#providerModelFor?.("panel", seat);
    const settleWaiters: Array<() => void> = [];
    const nextSettle = () => new Promise<"settled">((resolve) => settleWaiters.push(() => resolve("settled")));
    const agent = spawnPiAgent({
      command: panelPiCommand,
      args: [
        ...this.#resolvePiArgsPrefix("panel"),
        ...launchArgs("panel", {
          noSession: panelPiCommand !== undefined,
          provider: providerModel?.provider,
          model: providerModel?.model,
        }),
      ],
      cwd: candidateDir,
      env,
      role: "panel",
      agentId,
      streamFile,
      secrets: this.#secretMaskable,
      abortGraceMs: this.#deadlines.abortGraceMs,
      termGraceMs: this.#deadlines.termGraceMs,
      onEvent: (event) => {
        this.#noteActivity(agentId, event);
        this.#trackRunTokens(agentId, event);
        if ((event as { type?: string }).type === "agent_settled") settleWaiters.splice(0).forEach((f) => f());
      },
    });

    const handle: AgentHandle = {
      agent,
      role: "panel",
      agentId,
      helloResolve,
      helloPromise,
      shGroups: new Set(),
      doneResolve,
      donePromise,
      discoveryResolve: () => undefined,
      discoveryPromise: Promise.resolve(),
      panelSeat: seat,
    };
    this.#agents.set(agentId, handle);
    this.#log.intent(actionId, { agentId, pgid: agent.pgid, seat });

    try {
      const hello = await Promise.race([
        raceTimeout(helloPromise, this.#deadlines.helloTimeoutMs, "hello"),
        agent.waitExit().then(() => "exited" as const),
      ]);
      if (hello === "timeout" || hello === "exited") {
        await agent.terminate();
        this.#log.completion(actionId, { seat, ok: false, reason: hello === "exited" ? "round panel seat exited before hello" : "hello timed out" });
        this.#roundPanelSeatUnavailable(seat, "the seat never started");
        return;
      }
      if (!hello.ok) {
        await agent.terminate();
        this.#log.completion(actionId, { seat, ok: false, reason: hello.mismatch ? "tool-set mismatch" : "hello failed" });
        if (hello.mismatch) {
          this.#applyEvent({ type: "LAUNCH_FAILED", role: "panel", ...hello.mismatch });
        } else {
          this.#roundPanelSeatUnavailable(seat, "hello failed");
        }
        return;
      }

      const seatTimeout = this.#withStallWatch(
        agentId,
        agent,
        cancelableTimeout(this.#deadlines.panelMs, "timeout" as const),
        "Owner (conductor): no progress for a while. Finish now and call submit_round_panel_votes.",
      );
      const settled = nextSettle();
      await agent.prompt(this.#agentPrompt(this.#buildRoundPanelPrompt(seat)));
      const outcome = await Promise.race([
        donePromise.then(() => "submitted" as const),
        seatTimeout.promise,
        settled,
        agent.waitExit().then(() => "exited" as const),
      ]);
      seatTimeout.cancel();
      if (outcome === "submitted") {
        await agent.terminate();
        this.#log.completion(actionId, { seat, ok: true });
        return;
      }
      await agent.terminate();
      this.#log.completion(actionId, { seat, ok: false, reason: outcome });
      this.#roundPanelSeatUnavailable(seat, `the seat did not vote (${outcome})`);
    } finally {
      await agent.terminate().catch(() => undefined);
      this.#agents.delete(agentId);
    }
  }

  /** Plan 05e: the round panel seat's prompt — every pending item once, with
   * its 3a/3b evidence, and the panel's two questions. */
  #buildRoundPanelPrompt(seat: number): string {
    const phase = this.#state.phase;
    const C = phase.candidate?.sha ?? "";
    const items = roundPanelItemsNeedingVote(phase);
    let diff = "";
    try {
      diff = diffText(this.#plan.repo, phase.integrationHead, C);
    } catch {
      diff = "(the diff could not be read)";
    }
    const lines: string[] = [
      `You are panel seat ${seat} of 3, voting on ${items.length} item(s) for phase ${phase.phaseId}, candidate ${C.slice(0, 9)} (contract snapshot ${phase.contract.contractVersion.snapshot}).`,
      `Goal: ${phase.contract.goal}`,
      "",
      "Acceptance criteria:",
      ...phase.contract.acceptance.map((a) => `- ${a}`),
      ...secretPromptLines(this.#secretNames),
      ...directiveLines(phase.ownerDirectives),
      ...ledgerPromptLines(phase.messages),
      "",
      "Your read-only checkout of the candidate is the working directory. This is the diff against the base:",
      "```diff",
      redactText(diff, this.#secretMaskable),
      "```",
      "",
      "For EACH item below answer two questions, then vote `keep` or `drop` once per item with a reason:",
      "1. Is it accurate against the candidate? Cite the evidence you checked.",
      "2. Is it a real trade-off with a credible alternative the owner could choose? A description of what the code does is NOT.",
      "A `keep` majority publishes a trade-off to the owner; otherwise it is dropped. A `keep` majority also keeps a blocking finding blocking; otherwise the finding becomes advisory.",
      "",
    ];
    for (const id of items) {
      const m = (phase.messages ?? []).find((x) => x.id === id);
      if (!m) continue;
      const finding = m.sourceRecordId ? phase.findings.find((f) => f.id === m.sourceRecordId) : undefined;
      lines.push(
        `${m.id} [${m.type === "tradeoff" ? "trade-off" : "blocking finding"}] ${m.title}`,
        `    why: ${m.summary}`,
        `    context: ${m.context}`,
        `    evidence: ${(m.evidence ?? []).join("; ")}`,
        ...(finding?.verified ? [`    validated: ${finding.verified}`] : []),
      );
    }
    lines.push(
      "",
      "Call submit_round_panel_votes once with `votes`: one entry {messageId, verdict: 'keep'|'drop', reason} per item above. You are one of three independent seats; vote what the evidence shows.",
    );
    return lines.join("\n");
  }

  /** Plan 05e: count the round panel's recorded votes, record the outcome on
   * every item message, then apply each item's consequence: a non-keep
   * trade-off is dropped, a non-keep blocking finding becomes advisory. */
  #decideRoundPanel(): void {
    const phase = this.#state.phase;
    const round = phase.panel?.round;
    const panelCount = this.#laneSeats().length;
    if (!round || round.decided || !roundPanelSeatsSettled(round, panelCount)) return;
    const items = roundPanelItemsNeedingVote(phase);
    if (items.length === 0) return;
    const decisions = items.map((messageId) => {
      const m = (phase.messages ?? []).find((x) => x.id === messageId)!;
      const kind = m.type === "finding" ? "finding" : "tradeoff";
      const outcome = roundPanelOutcomeFor(round, messageId, kind, panelCount);
      const reasons = panelSeatNumbers(panelCount)
        .map((n) => ({ n: String(n), vote: round.seats?.[String(n)]?.votes?.find((v) => v.messageId === messageId) }))
        .filter((x): x is { n: string; vote: NonNullable<typeof x.vote> } => x.vote !== undefined)
        .map((x) => `seat ${x.n}: ${x.vote.reason}`);
      return { messageId, outcome, reason: reasons.join("; ") };
    });
    this.#applyEvent({ type: "ROUND_PANEL_DECIDED", decisions: decisions.map(({ messageId, outcome, reason }) => ({ messageId, outcome, ...(reason ? { reason } : {}) })) });
    for (const { messageId, outcome, reason } of decisions) {
      const message = (this.#state.phase.messages ?? []).find((m) => m.id === messageId);
      if (!message) continue;
      const binding = {
        messageId,
        boundCandidateSha: message.boundCandidateSha,
        boundContractVersion: message.boundContractVersion,
        boundRecordVersion: message.messageVersion,
      };
      if (message.type === "tradeoff") {
        if (outcome !== "keep") {
          this.#applyEvent({ type: "MESSAGE_DROPPED", ...binding, by: "panel", reason: reason || "the round panel did not keep it" });
        }
        continue;
      }
      const findingId = message.sourceRecordId;
      if (!findingId) continue;
      if (outcome !== "keep") {
        this.#applyEvent({
          type: "FINDING_SEVERITY_CHANGED",
          findingId,
          severity: "advisory",
          reason: reason || "the round panel did not keep it blocking",
          by: "panel",
        });
      }
      // #34 / plan 05e: approved code stays approved — a new blocking point
      // on bytes the reviewers already approved is advisory, not a blocker.
      const finding = (this.#state.phase.findings ?? []).find((f) => f.id === findingId);
      const combined = appendVerified(finding?.verified, `panel ${outcome} (${reason || "no reason"})`);
      this.#applyEvent({ type: "FINDING_VERIFIED", findingId, verified: combined ?? `panel ${outcome}` });
    }
  }

  // -- plan 2c: discovery barrier ------------------------------------------

  #discoveryBarrier: { candidate: string; arrived: Set<Reviewer>; released: boolean; waiters: Array<() => void> } | undefined;

  #barrierFor(candidate: string) {
    if (!this.#discoveryBarrier || this.#discoveryBarrier.candidate !== candidate) {
      this.#discoveryBarrier = { candidate, arrived: new Set(), released: false, waiters: [] };
    }
    return this.#discoveryBarrier;
  }

  /** True once every reviewer has either finished turn 1 on `candidate` in
   * this conductor, or already has a submitted review for it (a restarted
   * conductor must not wait for a reviewer that is already done). */
  #barrierComplete(candidate: string): boolean {
    const b = this.#barrierFor(candidate);
    const phase = this.#state.phase;
    return seatsOf(phase.contract).every((w) => b.arrived.has(w) || phase.reviews[w]?.review?.candidateSha === candidate);
  }

  #arriveAtDiscoveryBarrier(reviewer: Reviewer, candidate: string): void {
    const b = this.#barrierFor(candidate);
    b.arrived.add(reviewer);
    if (!b.released && this.#barrierComplete(candidate)) {
      b.released = true;
      this.#log.append("discovery_barrier_released", { candidateSha: candidate, arrived: [...b.arrived] });
      for (const w of b.waiters.splice(0)) w();
    }
  }

  #discoveryBarrierReleased(candidate: string): Promise<void> {
    const b = this.#barrierFor(candidate);
    if (b.released) return Promise.resolve();
    return new Promise<void>((resolve) => b.waiters.push(resolve));
  }

  /** True once the barrier for the current candidate has released: any
   * discovery submitted after that (a reviewer re-dispatched after a
   * timeout) could not be balloted by the reviewers already in turn 2, so
   * it is logged as a late observation instead of a votable record. */
  #discoveryClosed(): boolean {
    const C = this.#state.phase.candidate?.sha;
    return !!C && this.#discoveryBarrier?.candidate === C && this.#discoveryBarrier.released;
  }

  // -- work packet 2a: two-turn reviewer prompts ---------------------------

  /** Turn 1 (design §3.3): the contract verbatim, the read-only candidate
   * checkout path, the diff vs. the phase's base, and every record under
   * review EXCEPT the worker's own disclosure (`source === "worker"`) — a
   * reviewer must discover its own choices from the diff before ever seeing
   * what the worker disclosed. */
  #buildReviewerTurn1Prompt(reviewer: Reviewer, phaseOverride?: PhaseState, candidateDirOverride?: string): string {
    const phase = phaseOverride ?? this.#state.phase;
    const candidateDir = candidateDirOverride ?? this.#candidateDir();
    let diff = "(diff unavailable)";
    try {
      diff = diffText(this.#plan.repo, phase.integrationHead, phase.candidate!.sha);
    } catch {
      // best-effort — the reviewer still has the candidate checkout itself.
    }
    return [
      `You are reviewer ${reviewer}. Turn 1 of 2: discover the behavioural choices in this candidate BEFORE you see what the worker disclosed (design §3.3).`,
      `Goal: ${phase.contract.goal}`,
      "Acceptance criteria:",
      ...phase.contract.acceptance.map((a) => `- ${a}`),
      ...secretPromptLines(this.#secretNames),
      `Candidate checkout (read-only): ${candidateDir}`,
      ...referenceLines(runReferences(this.#runDir)),
      ...directiveLines(phase.ownerDirectives),
      // Plan 04a: the settled ledger, so a fresh reviewer never re-raises
      // what is already settled.
      ...ledgerPromptLines(phase.messages),
      // Plan 05j: the open ENTRIES, so a reviewer links to an existing topic
      // instead of re-raising it under a new id.
      ...entryPromptLines(phase.entries),
      // Plan 01f: turn 1 is told the conductor owns the gate evidence too (a
      // reviewer that only learns it in turn 2 could demand or accept a
      // substitute first). The failed record from an earlier candidate is
      // named here; its log tail is shown in turn 2, with the records under
      // review.
      ...gatePromptLines(phase.contract.gate, this.#gateRecordForPrompt(phase.candidate?.sha ?? "")),
      `Diff vs. phase base (${phase.integrationHead.slice(0, 7)}):`,
      "```diff",
      // Plan 01a: the diff is the candidate's own content, and a candidate can
      // carry a value a guard never saw (a file the worker wrote through
      // `edit`, not `sh`). It is a prompt, so the value is masked — and the
      // mask still tells the reviewer that this file holds a secret.
      redactText(diff, this.#secretMaskable),
      "```",
      "Call submit_discovery with at most 5 choices that change behaviour, interfaces, guarantees or cost where the plan left room. Each choice is one plain sentence of at most 20 words. Do not list implementation details (helper structure, naming, file layout) and do not give review advice here — correctness problems are findings, which you raise in turn 2. An empty list is fine.",
    ].join("\n");
  }

  /** Turn 2 (design §6.1): every live record on this candidate (the worker's
   * disclosures, all reviewers' discoveries after the discovery barrier, and
   * triggers), open findings and open corrections. */
  #buildReviewerTurn2Prompt(
    reviewer: Reviewer,
    handle?: AgentHandle,
    /** Plan 06g2: the lane's own phase (its candidate, its decisions, its
     * coverage) when this is a lane review; the phase's own state otherwise. */
    phaseOverride?: PhaseState,
    candidateDirOverride?: string,
  ): string {
    const phase = phaseOverride ?? this.#state.phase;
    const candidateDir = candidateDirOverride ?? this.#candidateDir();
    const C = phase.candidate?.sha ?? "";
    const K = phase.contract.contractVersion;
    const live = phase.decisions.filter((d) => isLiveDecision(d) && d.boundCandidateSha === C);
    // Skill fix 5: a kept decision that passed last round carries its ballots.
    const carriedIds = new Set(phase.ballots.filter((b) => b.boundCandidateSha === C && b.carriedFrom).map((b) => b.decisionId));
    // Plan 01d: the votable records (delegated or reserved) this prompt
    // demands a ballot for, minus the carried ones — recorded HERE, on this
    // dispatch's own handle, so the submit-time check reads exactly what this
    // prompt listed and a record added afterwards (a late discovery) can
    // never be demanded.
    if (handle) {
      // Plan 01g: an applied (or reverted) amendment record is shown but no
      // longer needs a ballot — its vote is history. Only a still-proposed
      // amendment is demanded.
      handle.demandedBallots = new Map(
        live
          .filter(
            (d) =>
              (d.class === "delegated" || d.class === "reserved") &&
              !carriedIds.has(d.id) &&
              (!d.amendment || d.amendment.status === "proposed"),
          )
          .map((d) => [d.id, d.choice]),
      );
      handle.incompleteReviewRejections = 0;
      // Durable trace of the exact demand, for observability and to make the
      // "a late discovery is never demanded" guarantee checkable: it is a
      // prompt-time snapshot, so a record added later is not in this list.
      this.#log.append("ballot_demanded", {
        reviewer,
        agentId: handle.agentId,
        candidateSha: C,
        records: [...handle.demandedBallots.entries()].map(([id, choice]) => ({ id, choice })),
      });
    }
    const record = (d: Decision) => {
      const who = d.source === "worker" ? "worker" : d.source === "trigger" ? "trigger" : `discovered by ${d.id.includes(`-disc-${reviewer}-`) ? "YOU" : "a reviewer"}`;
      const carried = carriedIds.has(d.id) ? " [carried: kept unchanged and approved last round; vote again only if this candidate's changes affect it]" : "";
      return `- ${d.id} [${d.class}, ${who}]${carried}: ${d.choice}\n    why: ${d.whyItMatters}\n    alternatives: ${d.alternatives.map((a) => `${a.option} → ${a.consequence}`).join(" | ")}`;
    };
    const own = live.filter((d) => d.id.includes(`-disc-${reviewer}-`));
    const openFindings = phase.findings.filter((f) => f.status === "open");
    const openCorrections = phase.corrections.filter((c) => c.status === "open");
    const lines = [
      `Turn 2 of 2 for candidate ${C.slice(0, 7)} (contract snapshot ${K.snapshot}). All three reviewers finished turn 1; this is the complete list of records on this candidate.`,
      `Candidate checkout (read-only): ${candidateDir}`,
      ...secretPromptLines(this.#secretNames),
      // Plan 01f: reviewers are told the conductor produces the gate's
      // evidence and shown the record when one exists (the failed gate that
      // sent the phase into this repair round, or a record reused for this
      // candidate) — a substitute gate proof has to be refused, not accepted.
      ...gatePromptLines(phase.contract.gate, this.#gateRecordForPrompt(C), this.#gateTailForPrompt(C)),
      // Plan 01i: the directives in force, and the contract rule that binds
      // them — always stated, whether or not one is in force right now.
      ...reviewerTurn2DirectiveSection(phase.ownerDirectives),
      // Plan 01e: the base's own failing tests, so a reviewer does not raise
      // them as this candidate's defect (runtime doc §8: reviewers kept
      // flagging the 14 pre-existing exchange-state-machine failures).
      ...baselinePromptLines(this.#baselineFailedCommands()),
      // Plan 05d: the candidate's own failing tests, each re-run alone, so a
      // reviewer never calls a real regression a flake or a flake a defect.
      ...checkFailurePromptLines(phase),
      // Plan 06b: the same checklist the worker saw, the worker's coverage,
      // the check resolution, and the verdicts this review must carry.
      ...(this.#structured()
        ? [
            ...checklistLines(this.#planItems()),
            ...coverageLines(phase.coverage, this.#planItems()),
            ...checkResolutionLines(phase.checkResolution ?? []),
            ...reviewRequestLines(this.#planItems()),
          ]
        : []),
      // Plan 04a: the settled ledger, so a fresh reviewer never re-raises
      // what is already settled.
      ...ledgerPromptLines(phase.messages),
      // Plan 05j: the same open-entry list on the turn-2 prompt.
      ...entryPromptLines(phase.entries),
      "Records:",
      ...(live.length > 0 ? live.map(record) : ["- (none)"]),
    ];
    if (openFindings.length > 0) {
      lines.push(
        "Open findings:",
        ...openFindings.map((f) => `- ${f.id} [${f.severity} ${f.kind}, raised by ${f.raisedBy}]: ${f.evidence}`),
      );
    }
    // Plan 05e (4): every earlier round's open finding/blocker message, so
    // each reviewer marks each `resolved` or `open` with evidence; a 2-of-3
    // `resolved` majority moves it out of the owner's live view.
    //
    // The message itself is REBOUND to the current candidate by
    // MESSAGE_CARRIED at every freeze, so the filter reads the source
    // RECORD's own binding (carried messages keep pointing at the finding,
    // whose binding is the round it was raised on) — round-3 reviews M-8,
    // A-11, B-14.
    const earlierRound = (phase.messages ?? []).filter((m) => {
      if (m.type !== "finding" && m.type !== "blocker") return false;
      if (m.state !== "published" && m.state !== "refused") return false;
      const record = m.sourceRecordId ? phase.findings.find((f) => f.id === m.sourceRecordId) : undefined;
      if (record) return record.boundCandidateSha !== C;
      return m.messageVersion > 1 || (m.carriedFrom ?? []).length > 0;
    });
    if (earlierRound.length > 0) {
      lines.push(
        "Earlier rounds' live findings and blockers (mark each resolved or open in `resolutionStatements`, with evidence):",
        ...earlierRound.map((m) => `- ${m.id} [${m.type}, ${m.state}] ${m.title}`),
      );
    }
    // Plan 05e (finding #34): an amendment-only resubmission of bytes M, A
    // and B already approved re-reviews only the amended criterion.
    const approvedSha = this.#amendmentOnlyApprovedSha(C);
    if (approvedSha) {
      lines.push(
        "",
        `This is an amendment-only resubmission: the shipped bytes are identical to candidate ${approvedSha.slice(0, 9)}, which M, A and B already approved. Review ONLY the amended criterion. A new point on the unchanged code is an advisory finding for the owner or the next phase, not a blocker, unless it violates an acceptance item or a reserved rule.`,
      );
    }
    if (openCorrections.length > 0) {
      lines.push("Open owner corrections (state honored / not_honored for each):", ...openCorrections.map((c) => `- ${c.id}: ${c.correctionText}`));
    }
    // Skill fix 4: a test deleted from a file that still exists may drop
    // coverage of live behaviour; make every one visible to the reviewers.
    let removed: string[] = [];
    try {
      removed = removedTestsBetween(this.#plan.repo, phase.integrationHead, C);
    } catch {
      removed = [];
    }
    if (removed.length > 0) {
      lines.push(
        `Tests removed from files that still exist (${removed.length}). For each, check that it was replaced or that the behaviour it tested was removed on purpose; a removed test of behaviour that is still live is a blocking finding:`,
        ...removed.slice(0, 60).map((t) => `- ${redactText(t, this.#secretMaskable)}`),
        ...(removed.length > 60 ? [`- … and ${removed.length - 60} more`] : []),
      );
    }
    lines.push(
      "",
      "Call submit_review with:",
      "- `ballots`: one ballot for EVERY record above whose class is 'delegated' or 'reserved' (approve or reject, a rationale, at least one evidence citation), except records marked carried: your previous ballot stands for those, and a new ballot replaces it. A ballot with contractObjection=true opens a contract finding and suspends that vote.",
      "- `findings`: correctness problems only — defects, contract violations — with file:line or a scenario as evidence and a severity. A candidate that violates an owner directive is a blocking contract finding: cite the directive id as its evidence. If a problem is already an open finding above, set `sameAs` to its id instead of repeating it. If the problem is that a criterion cannot be met AS WRITTEN, add `criterionDispute` = { criterion: <the acceptance item verbatim>, why, proposedWording }: the conductor records an amendment voted on like any reserved record (a passing one replaces the wording; a failed one leaves it unchanged). An unmet-but-clear criterion is an ordinary defect finding. Set `runnable` when the finding is a runnable test or command: the conductor re-runs it and publishes the finding only if it fails.",
      "- `resolutionStatements`: for EVERY earlier-round finding or blocker listed above, { messageId, status: 'resolved' | 'open', evidence }. A 2-of-3 `resolved` majority moves the message out of the owner's live view.",
      "- A blocking finding must cite an acceptance item or a reserved rule; the evaluator lowers anything else to advisory.",
      "- `blockers`: use this ONLY to stop the work until the owner decides. Each entry is {kind, evidence} like a finding (it is raised at once as a raw blocker message and a blocking finding), and a panel of three fresh agents then votes `block` or `downgrade`. A `block` majority parks the phase for the owner, with options the panel proposes; a `downgrade` majority makes it an ordinary blocking finding for the next worker attempt. A blocker is never folded into an existing finding (no `sameAs`): state the issue's own evidence. An ordinary defect that should be fixed but need not stop the run belongs in `findings`, not here.",
      // Plan 01g: an amendment record is a reserved decision like any other;
      // it must get a ballot, and it never blocks acceptance on its own.
      ...(phase.decisions.some((d) => d.amendment && d.boundCandidateSha === C && isLiveDecision(d))
        ? [
            "- An amendment record (class reserved, shown with `proposedWording`) is the worker's or a reviewer's claim that a criterion cannot be met as written. Vote on it like any other reserved decision; a passing normal tally replaces that acceptance item for this phase.",
          ]
        : []),
      own.length > 0
        ? `- \`discoveryMatches\`: for each of YOUR discoveries (${own.map((d) => d.id).join(", ")}) that is the same choice as another record above, give {discoveryId, sameAs}.`
        : "- `discoveryMatches`: none needed (you have no discoveries on this list).",
      "- `findingStatements` for findings YOU raised: `confirm` if this candidate fixes it, `withdraw` only if the finding was wrong in the first place.",
      "- `correctionStatements` for each open owner correction.",
      `- reviewer ${reviewer}, phaseId ${phase.phaseId}, candidateSha ${C}, contractVersion ${JSON.stringify(K)}.`,
    );
    return lines.join("\n");
  }

  // -- publish --------------------------------------------------------------

  async #runPublish(actionId: string, expectedHead: string, candidateI: string): Promise<void> {
    this.#log.intent(actionId, { expectedHead, candidateI });
    crashAt("before_publish_cas");
    const result = publishCAS(this.#plan.repo, this.#integrationBranch, candidateI, expectedHead);
    crashAt("after_publish_cas");
    this.#log.completion(actionId, result);
    // Plan 01g: if the phase left PUBLISHING while the CAS ran (an owner
    // correction reverted an amendment, or any other command moved it), the
    // completion has no row to land on; applying it would be rejected and
    // throw. Record it and stop — the log is still the truth of what the
    // CAS did.
    if (this.#state.phase.phase !== "PUBLISHING") {
      this.#log.append("publish_completion_ignored", {
        reason: `the phase moved to ${this.#state.phase.phase} while the publish CAS ran`,
        result,
      });
      return;
    }
    if (result.ok) {
      this.#applyEvent({ type: "PUBLISH_COMPLETED", newHead: candidateI });
    } else {
      this.#applyEvent({ type: "PUBLISH_STALE", actualHead: result.actualHead });
    }
  }
}

// ---------------------------------------------------------------------------
// Small helpers
// ---------------------------------------------------------------------------

/** Plan 04b: one option a panel seat's `block` vote proposes. */
interface PanelOptionInput {
  id: string;
  label: string;
}

/** Plan 04a: one `submit_evaluation` entry, as the evaluator sends it. */
/** Plan 05e: join validation markers without duplicating or dropping an
 * earlier one, so `verified` accumulates (`record …; run …; evaluator: …;
 * panel keep`). */
function appendVerified(existing: string | undefined, addition: string | undefined): string | undefined {
  const parts = [existing, addition]
    .flatMap((v) => (v ?? "").split("; "))
    .map((v) => v.trim())
    .filter((v) => v.length > 0);
  const unique: string[] = [];
  for (const part of parts) if (!unique.includes(part)) unique.push(part);
  return unique.length > 0 ? unique.join("; ") : undefined;
}

interface EvaluationEntry {
  messageId?: unknown;
  action?: unknown;
  title?: unknown;
  summary?: unknown;
  context?: unknown;
  evidence?: unknown;
  importance?: unknown;
  into?: unknown;
  reason?: unknown;
  /** Owner-refused messages only: whether this candidate addressed it. */
  addressed?: unknown;
  /** Plan 05e: a finding's own validation evidence (`file:line …`) — what the
   * evaluator checked to confirm it. */
  verified?: unknown;
}

/** Plan 04a: an anchor is `{path, lines: [start, end]}`. Anything else is
 * refused back to the raiser rather than recorded as an unusable anchor. */
function parseAnchor(raw: unknown): { path: string; lines: [number, number] } | undefined {
  if (!raw || typeof raw !== "object") return undefined;
  const a = raw as { path?: unknown; lines?: unknown };
  if (typeof a.path !== "string" || a.path.trim().length === 0) return undefined;
  if (!Array.isArray(a.lines) || a.lines.length !== 2) return undefined;
  const [start, end] = a.lines;
  if (!Number.isInteger(start) || !Number.isInteger(end) || start < 1 || end < start) return undefined;
  return { path: a.path.trim(), lines: [start as number, end as number] };
}

/** Plan 05c: why an evaluator's title is not one complete line, or undefined
 * when it is. A title is refused when it is longer than the cap, ends with
 * the ellipsis a cut leaves (`…' or `...'), or ends mid-word — the last case
 * detected when the evaluator's title is a strict prefix of the raw message's
 * own title and the raw title continues with a word character, which is what
 * a model that truncated instead of rewriting produces. */
export function titleIssue(rawTitle: string | undefined, title: string | undefined): string | undefined {
  const t = (title ?? "").replace(/\s+/g, " ").trim();
  // No title given: the raw message's own title is used, already complete.
  if (t.length === 0) return undefined;
  if (t.length > MESSAGE_TITLE_MAX) return `the title is ${t.length} characters long, over the ${MESSAGE_TITLE_MAX}-character cap`;
  if (/…$/.test(t) || /\.\.\.$/.test(t)) return "the title ends with an ellipsis, so it was cut to fit the cap";
  const raw = (rawTitle ?? "").replace(/\s+/g, " ").trim();
  if (raw.length > t.length && raw.startsWith(t) && /[A-Za-z0-9]/.test(raw.charAt(t.length))) {
    return "the title ends mid-word: it is a truncated start of the message's own title";
  }
  return undefined;
}

/** Plan 04a: the settled ledger every prompt carries under "Settled (do not
 * re-raise)" — what has already been decided, by whom, and why, so a fresh
 * agent does not re-argue it. */
export function ledgerPromptLines(messages: readonly Message[] | undefined): string[] {
  const entries = ledgerEntries([...(messages ?? [])]);
  if (entries.length === 0) return [];
  return [
    "",
    "Settled (do not re-raise):",
    ...entries.map((e) => {
      const reason = e.reason ? ` — ${e.reason}` : "";
      const bad = e.invalidated ? ` (invalidated: ${e.invalidated.reason})` : "";
      return `- ${e.messageId} [${e.type}] ${e.state} by ${e.settledBy}${reason}${bad}`;
    }),
  ];
}

/** Plan 04b: the "must be fixed" lines carrying an owner's choice on an
 * escalated blocker (round-3 review, advisory A-6). The choice is an
 * instruction to the NEXT attempt only, so a line is produced only when:
 *   - the request was resolved against `currentCandidate` — at the moment a
 *     repair attempt's prompt is built, `phase.candidate` is still the
 *     candidate the owner was looking at, and a later round's is not; and
 *   - the option actually asks for work (`accept_risk` settles the blocker
 *     and lets the candidate stand, so nothing is "to be fixed"; it must not
 *     appear under a must-fix heading even when some unrelated open
 *     correction is what sent the phase back to a repair).
 * Exported so a unit test exercises exactly what a repair prompt carries. */
export function ownerBlockerChoiceLines(phase: PhaseState, currentCandidate: string | undefined): string[] {
  const out: string[] = [];
  for (const r of phase.ownerRequests) {
    if (r.origin !== "blocker_panel" || r.status !== "resolved" || !r.resolution?.option) continue;
    if (r.resolvedBinding?.candidateSha !== currentCandidate) continue;
    if (!isRepairForcingOption(r.origin, r.resolution.option)) continue;
    const label = r.options.find((o) => o.id === r.resolution!.option)?.label ?? r.resolution.option;
    out.push(`The owner's choice on blocker ${r.linkedMessageId ?? r.linkedFindingId ?? r.id}: ${label}. Carry that choice out.`);
  }
  return out;
}

/** Plan 04a: an owner-refused message the next worker attempt must address:
 * the message, and the owner's reason, verbatim. */
export function refusedPromptLines(messages: readonly Message[] | undefined): string[] {
  const refused = (messages ?? []).filter((m) => m.state === "refused");
  if (refused.length === 0) return [];
  return [
    "",
    "Owner-refused (must address):",
    ...refused.map((m) => `- ${m.id} "${m.title}" — refused: ${m.settlement?.reason ?? "no reason given"}`),
  ];
}

interface SubmitPhaseArgs {
  decisions?: DecisionDisclosure[];
  priorDecisions?: PriorDecisionStatement[];
  criterionDispute?: CriterionDispute;
  assumptions?: string[];
  deviations?: string[];
}

async function raceTimeout<T>(promise: Promise<T>, ms: number, _label: string): Promise<T | "timeout"> {
  let timer: NodeJS.Timeout;
  const timeout = new Promise<"timeout">((resolve) => {
    timer = setTimeout(() => resolve("timeout"), ms);
  });
  const result = await Promise.race([promise, timeout]);
  clearTimeout(timer!);
  return result;
}

/** A `setTimeout`-backed promise for use inside a `Promise.race`, with a
 * `cancel()` the caller MUST call once the race settles — otherwise the
 * timer (which can be design §8.1's multi-minute or multi-hour deadlines)
 * keeps the event loop alive long after the race's other branch already
 * won, which is exactly what made every conductor test process hang (round
 * of review item 1). Every `Promise.race([..., timer.promise])` in this
 * file cancels its timer in a `finally` immediately after the race. */
/** A plain `setTimeout` promise, for the baseline lock's bounded wait (plan
 * 01e) — unlike `cancelableTimeout` it has no deadline value to return. */
function sleepMs(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/** True iff `pid` still exists (owned by any user, which on this machine means
 * this account) — how a waiter tells a live baseline-lock holder from a dead
 * one. A recycled pid can only make a stale lock look live for one bounded
 * wait, never wedge a run. */
function pidRunning(pid: number): boolean {
  return processAlive(pid);
}

function cancelableTimeout<T>(ms: number, value: T): { promise: Promise<T>; cancel: () => void } {
  let timer: NodeJS.Timeout;
  const promise = new Promise<T>((resolve) => {
    timer = setTimeout(() => resolve(value), ms);
  });
  return { promise, cancel: () => clearTimeout(timer) };
}

/** Best-effort extraction of a cumulative token count from a
 * `message_update` RPC event's `usage` payload (design §8.1's "per-attempt
 * token cap from Pi usage events" and the run-wide token budget). The real
 * shape is Pi-version-dependent; this tries the field names actually seen
 * across Pi's own usage reporting and fake-pi's test scripts
 * (`totalTokens`/`total_tokens`, or `inputTokens`+`outputTokens` /
 * `input_tokens`+`output_tokens`) and returns `undefined` if none match,
 * which callers treat as "no usage information in this event". */
function extractTokenTotal(usage: unknown): number | undefined {
  if (!usage || typeof usage !== "object") return undefined;
  const u = usage as Record<string, unknown>;
  if (typeof u.totalTokens === "number") return u.totalTokens;
  if (typeof u.total_tokens === "number") return u.total_tokens;
  const inputA = typeof u.inputTokens === "number" ? u.inputTokens : undefined;
  const outputA = typeof u.outputTokens === "number" ? u.outputTokens : undefined;
  if (inputA !== undefined || outputA !== undefined) return (inputA ?? 0) + (outputA ?? 0);
  const inputB = typeof u.input_tokens === "number" ? u.input_tokens : undefined;
  const outputB = typeof u.output_tokens === "number" ? u.output_tokens : undefined;
  if (inputB !== undefined || outputB !== undefined) return (inputB ?? 0) + (outputB ?? 0);
  return undefined;
}

function sanitize(command: string): string {
  return command.replace(/[^a-zA-Z0-9._-]/g, "_").slice(0, 60) || "check";
}

// -- plan 01i: owner directives (D5) ---------------------------------------

/** Plan 01i: the id a new directive gets.
 *
 * A program-wide directive (`preferredId`, `ODP-<n>`) keeps the program's own
 * id verbatim — the `ODP` space never collides with a phase's `OD` space, so
 * a node never renumbers a program ruling and `withdraw ODP-n` retires the
 * same record everywhere. Otherwise the phase's next free `OD-<n>` is used;
 * program-wide ids never advance that counter. */
export function allocateDirectiveId(
  existing: readonly OwnerDirective[],
  preferredId?: string,
): { id: string; seq: number } {
  const preferred = preferredId?.match(/^((?:ODP|OD)-(\d+))$/);
  if (preferred) return { id: preferred[1], seq: Number(preferred[2]) };
  const localMax = existing.reduce((m, d) => {
    const num = d.id.match(/^OD-(\d+)$/);
    return num ? Math.max(m, Number(num[1])) : m;
  }, 0);
  return { id: `OD-${localMax + 1}`, seq: localMax + 1 };
}

/** Plan 01i: what an input-box text means for withdrawal.
 * `undefined`: not a withdrawal at all (an ordinary directive).
 * `withdraw`: retract the named directive; `text` may carry trailing prose
 * ("withdraw OD-1 because it is stale") without becoming a new ruling.
 * `malformed`: it opens with `withdraw` but names no valid id — refused with
 * the reason, never inverted into a fresh binding directive. */
export type WithdrawInput =
  | { kind: "withdraw"; id: string }
  | { kind: "malformed"; reason: string };

export function parseWithdrawInput(text: string): WithdrawInput | undefined {
  const t = text.trim();
  if (!/^withdraw\b/i.test(t)) return undefined;
  const rest = t.replace(/^withdraw\b/i, "").trim();
  const id = rest.match(/^((?:ODP|OD)-\d+)\b/i);
  if (!id) {
    return {
      kind: "malformed",
      reason: `a withdrawal must name a directive id, e.g. \`withdraw OD-1\` (got: ${JSON.stringify(t.slice(0, 80))})`,
    };
  }
  return { kind: "withdraw", id: id[1].toUpperCase() };
}

/** Plan 01i: a directive the program scheduler pushed into this node's inbox
 * (a node that was already running when the owner ruled program-wide). */
export function directiveCommandOf(
  raw: unknown,
): { text: string; scope: DirectiveScope; forward: boolean; programId?: string } | undefined {
  if (!raw || typeof raw !== "object") return undefined;
  const r = raw as Record<string, unknown>;
  const kind = typeof r.type === "string" ? r.type : typeof r.kind === "string" ? r.kind : undefined;
  if (kind !== "directive") return undefined;
  if (typeof r.text !== "string" || r.text.trim().length === 0) return undefined;
  // `pushed` marks a directive the program scheduler delivered here; it must
  // not be forwarded back to the program (that would loop). `programId` is the
  // program's own numbering, kept when the local number is free.
  return {
    text: r.text,
    scope: r.scope === "phase" ? "phase" : "program",
    forward: r.pushed !== true,
    ...(typeof r.programId === "string" ? { programId: r.programId } : {}),
  };
}

/** Plan 01i: a program-level withdrawal the scheduler pushed into this node. */
export function withdrawCommandOf(raw: unknown): { directiveId: string; text: string } | undefined {
  if (!raw || typeof raw !== "object") return undefined;
  const r = raw as Record<string, unknown>;
  const kind = typeof r.type === "string" ? r.type : typeof r.kind === "string" ? r.kind : undefined;
  if (kind !== "withdraw-directive") return undefined;
  if (typeof r.directiveId !== "string" || r.directiveId.length === 0) return undefined;
  const text = typeof r.text === "string" && r.text.trim().length > 0 ? r.text : `withdraw ${r.directiveId}`;
  return { directiveId: r.directiveId, text };
}

/** Plan 01i: the prompt section that makes the owner's directives binding —
 * every directive still in force, verbatim, newest last. Empty when none is
 * in force, so a prompt with no directive gains no section. */
export function directiveLines(directives: readonly OwnerDirective[] | undefined): string[] {
  const inForce = (directives ?? []).filter((d) => d.status === "in-force");
  if (inForce.length === 0) return [];
  return [
    "",
    "Owner directives (binding):",
    ...inForce.map((d) => `- ${d.id}${d.scope === "program" ? " (whole program)" : ""}: ${d.text}`),
  ];
}

/** Plan 01e: the prompt section that tells an agent which check failures were
 * already on the phase base before this phase began, so it neither tries to
 * fix them nor treats them as a defect. Empty when the base passed (or no
 * baseline was taken), so a prompt with no pre-existing failure gains no
 * section. Exported (and used by `buildWorkerPrompt` and
 * `#buildReviewerTurn2Prompt`) so a unit test exercises exactly the words the
 * two prompts send, rather than a look-alike built somewhere else. */
/** Plan 05j: the open entries a reviewer's prompt lists, so it can link at
 * raise time instead of re-raising a topic under a new id. */
export function entryPromptLines(entries: readonly Entry[] | undefined): string[] {
  const open = (entries ?? []).filter((e) => e.state === "open");
  if (open.length === 0) return [];
  return [
    "",
    "Open entries (one topic each; raise the topic once, and do not repeat one already listed):",
    ...open.map((e) => `- ${e.id} [${e.type}] ${e.title} (${formatAnchor(e.anchor)})`),
  ];
}

export function baselinePromptLines(commands: readonly BaselineCommand[] | undefined): string[] {
  const failed = baselineFailedCommands(commands ?? []);
  if (failed.length === 0) return [];
  return [
    "",
    "Pre-existing check failures on the phase base (NOT this phase's to fix):",
    "These already failed on the base commit before any change here, each under the command shown. The conductor does not count a check as failed when every failing test it names is one of these for that same command; any other failing test fails the gate.",
    ...failed.map((c) => `- \`${c.command}\`: ${c.failures.join(", ")}`),
  ];
}

/** Plan 05d / finding #35: label the current candidate's new failing tests so
 * a real regression is never read as a flake and a flake is never repaired.
 * Empty when the last check passed or its output named no test. Exported (and
 * used by `#repairContext` and `#buildReviewerTurn2Prompt`) so a unit test
 * exercises the exact words both prompts send. */
export function checkFailureLines(phase: PhaseState): string[] {
  const failures = phase.checks?.passed === false ? phase.checks.failures ?? [] : [];
  return failures.map((f) =>
    f.loadOnly
      ? `\`${f.name}\`: load-only (passed when re-run alone; a flake — do not repair it)`
      : `\`${f.name}\`: reproduces alone (a real failure — fix it)`,
  );
}

/** Plan 05d: the reviewer section for a check failure. A check that fails
 * sends its candidate to REPAIRING, never to REVIEWING, so in the normal flow
 * the reviewers see the split of the candidate they are reviewing **only** as
 * the failure their candidate repairs: the previous candidate's split, kept
 * across the freeze in `lastCheckFailures` (finding A-5). */
export function checkFailurePromptLines(phase: PhaseState): string[] {
  const current = checkFailureLines(phase);
  if (current.length > 0) {
    return [
      "",
      "Candidate check failures, each re-run alone (a real regression is not a flake, and a flake is not a repair item):",
      ...current.map((l) => `- ${l}`),
    ];
  }
  const last = phase.lastCheckFailures;
  // Only the candidate that repairs the failed check is told about it: the
  // freeze marks it `repairedBy`, so a later candidate (repairing a review
  // finding, say) is never shown a two-candidates-stale split (finding A-9).
  if (!last || last.failures.length === 0 || last.repairedBy !== phase.candidate?.sha) return [];
  return [
    "",
    `The check failure this candidate repairs (previous candidate ${last.candidateSha.slice(0, 7)}; every test was re-run alone):`,
    ...last.failures.map(
      (f) => `- \`${f.name}\`: ${f.loadOnly ? "load-only (a flake; the check passed on it)" : "reproduces alone (a real failure)"}`,
    ),
  ];
}

/** Plan 01i: what a reviewer must know about a directive it is shown: it binds
 * as part of the contract, following one is never a defect even where the plan
 * says otherwise, and violating one is a blocking contract finding. */
export const DIRECTIVE_BINDING_STATEMENT =
  "The owner's directives are binding on you as part of the contract: a candidate that follows one cannot be faulted for doing so, even where the plan's text says otherwise; a candidate that violates one is a blocking contract finding that cites the directive id.";

/** Plan 01f: what every agent must know about the gate. Only the conductor
 * runs it and only its record is accepted evidence (runtime doc §6: agents
 * ran the expensive command inside their attempts — 82 minutes of docker in
 * 13i/13j, some killed at the 8-minute command limit — and, because they had
 * to produce the live proof, wrote substitutes: a sentinel `code_sha`,
 * `pending_owner_live_run`, a fingerprint-only record). */
/** Plan 01f: the note a gate record carries when the command never started.
 * The owner's ruling (D-B-62): the cleanup runs whenever the gate command ran
 * (pass, fail or timeout); a candidate that no longer merges never starts the
 * command, so there is nothing to clean up. */
export const CLEANUP_NOT_RUN =
  "the cleanup runs whenever the gate command ran (pass, fail or timeout); the candidate does not merge, so the gate never started and nothing was cleaned";

export const GATE_BINDING_STATEMENT =
  "The conductor runs the phase's gate command itself, once, after the checks, the probe and all three reviews pass, and only its record (checks/<sha>/gate.json) counts as the live proof. Never run the gate command yourself, and never report, substitute or fabricate its evidence.";

/** Plan 01f: the gate section a prompt carries — the binding statement
 * (always: no agent may run the gate or substitute its evidence, gate or no
 * gate), the command when the contract declares one, and the record when one
 * exists (facts only; `tail` adds the log's last lines for a failed
 * record). */
export function gatePromptLines(
  gate: string | undefined,
  record?: GateRecord,
  tail?: string,
): string[] {
  const lines = ["", GATE_BINDING_STATEMENT];
  if (gate) lines.push(`The phase's gate command is: \`${gate}\``);
  if (record) {
    // One outcome phrase (core/gate.ts), so a gate that never started is
    // never rendered as "failed (exit undefined)" (findings B-22/A-23).
    const how = record.reused
      ? `reused candidate ${record.reusedFrom?.slice(0, 9) ?? "?"}'s passing record`
      : gateOutcomeText(record, { withElapsed: true });
    lines.push(`Gate record for candidate ${record.candidateSha.slice(0, 9)}: ${record.command} — ${how}; log sha256 ${record.logSha256}`);
    if (!record.passed && tail && tail.trim().length > 0) {
      lines.push(`Last lines of that gate's log:`, tail);
    }
  }
  return lines;
}


/** Plan 01i: the reviewer turn-2 directive section — every directive in force,
 * verbatim, newest last, then the contract rule that binds them. Exported (and
 * used by `#buildReviewerTurn2Prompt`) so a unit test exercises exactly what
 * turn 2 sends, rather than a look-alike built from state somewhere else. */
export function reviewerTurn2DirectiveSection(directives?: readonly OwnerDirective[]): string[] {
  return [...directiveLines(directives), "", DIRECTIVE_BINDING_STATEMENT];
}

/** Plan 2c: what a repair attempt must know about the candidate that was
 * not accepted (design §5.2 "the rejecting ballots go to the worker's
 * session as a repair request", §4.2, §7.5). */
export interface RepairContext {
  round: number;
  previousCandidate: string;
  blocking: string[];
  failedDecisions: string[];
  advisory: string[];
  corrections: string[];
  priorDecisions: Array<{ id: string; choice: string }>;
}

/** The prompt lines naming a run's reference documents, or none. */
export function referenceLines(references: string[]): string[] {
  if (references.length === 0) return [];
  return [
    "",
    "Reference documents for this plan (outside the repository; read them with the read tool, do not search for them):",
    ...references.map((r) => `- ${r}`),
  ];
}

export function buildWorkerPrompt(
  contract: PhaseContract,
  ownerNotes?: string,
  interruptionNote?: string,
  repair?: RepairContext,
  references: string[] = [],
  secrets: readonly string[] = [],
  directives?: readonly OwnerDirective[],
  baselineCommands?: readonly BaselineCommand[],
  messages?: readonly Message[],
  /** Plan 06c (R6): the tools the preflight did not find, named so the worker
   * knows what its environment lacks. */
  toolLines: readonly string[] = [],
): string {
  const structured = isStructured(contract);
  const lines: string[] = [
    `Goal: ${contract.goal}`,
    "",
    "Acceptance criteria:",
    ...contract.acceptance.map((a) => `- ${a}`),
    ...(structured ? checklistLines(itemsFromPhase(contract)) : []),
    ...secretPromptLines(secrets),
    ...referenceLines(references),
    ...baselinePromptLines(baselineCommands),
    ...toolLines,
  ];
  if (structured) {
    lines.push(
      "",
      "End your attempt with submit_coverage: for every requirement and constraint above, give status done, partial or not_done with where and tests; for every architecture item, give fits or deviates with where. A partial, not_done or deviates entry must carry a note, which becomes a trade-off message. The phase cannot freeze until the coverage is complete.",
    );
  }
  if (contract.boundaries.length > 0) lines.push("", "Boundaries:", ...contract.boundaries.map((b) => `- ${b}`));
  if (ownerNotes) lines.push("", `Owner notes: ${ownerNotes}`);
  lines.push(...directiveLines(directives));
  // Plan 04a: the settled ledger, and any message the owner refused (which
  // the next attempt must address, with the owner's reason).
  lines.push(...refusedPromptLines(messages));
  lines.push(...ledgerPromptLines(messages));
  // Plan 01f: the gate is the conductor's proof to produce, never the
  // worker's (runtime doc §6's structural incentive to substitute it).
  lines.push(...gatePromptLines(contract.gate));
  if (interruptionNote) lines.push("", interruptionNote);
  if (repair) {
    lines.push(
      "",
      `REPAIR (round ${repair.round + 1}): your previous candidate ${repair.previousCandidate.slice(0, 7)} was reviewed and NOT accepted. Fix what blocks acceptance; keep what was approved.`,
    );
    if (repair.blocking.length > 0) lines.push("Blocking (must be fixed):", ...repair.blocking.map((b) => `- ${b}`));
    if (repair.failedDecisions.length > 0) {
      lines.push("Decisions whose vote failed (change them, or keep them and answer the objection with evidence):", ...repair.failedDecisions.map((d) => `- ${d}`));
    }
    if (repair.corrections.length > 0) lines.push("Owner corrections (verbatim, must be honored):", ...repair.corrections.map((c) => `- ${c}`));
    if (repair.advisory.length > 0) lines.push("Advisory findings (fix if cheap and safe):", ...repair.advisory.map((a) => `- ${a}`));
    if (repair.priorDecisions.length > 0) {
      lines.push(
        "Your prior decisions. In submit_phase, list EACH one in `priorDecisions` with status kept, changed (give the new choice, whyItMatters, alternatives, recommendation) or withdrawn; put only genuinely new choices in `decisions`:",
        ...repair.priorDecisions.map((d) => `- ${d.id}: ${d.choice}`),
      );
    }
  }
  lines.push(
    "",
    "If a criterion cannot be met AS WRITTEN (not merely unmet yet), say so instead of faking it: call submit_phase with `criterionDispute` = { criterion: <one acceptance item above, verbatim>, why, proposedWording }. The conductor records it as an amendment the reviewers vote on; if the normal tally passes, the wording is replaced for this phase and the next candidate is judged against it. A criterion that is merely unmet is a normal repair, not a dispute.",
    "How to work:",
    `- The conductor runs the phase checks itself on a fresh checkout after you call submit_phase${contract.checks.length > 0 ? ` (${contract.checks.join("; ")})` : ""}. Do NOT run the full check suite yourself; run only the narrow tests for the files you change (one test file, or the project's fast test target). With node --test, pass --test-force-exit so a test that leaks a process cannot hold the command open. Do not pipe test output through tail or head: a command killed at the per-command time limit then returns nothing. If a test file is slow, run one test at a time with --test-name-pattern.`,
    "- Run commands in the foreground. Backgrounding (&, nohup, setsid) and sleeps longer than 30 s are refused.",
    "- Edit files with the edit and write tools.",
    "- In submit_phase, write each decision's choice as one plain sentence of at most 20 words; put the reasoning in whyItMatters and the alternatives. Disclose choices that change behaviour, interfaces, guarantees or cost.",
    "",
    "When finished, call submit_phase with your decisions, assumptions and deviations.",
  );
  return lines.join("\n");
}

export function buildReviewerPrompt(
  phase: PhaseState,
  reviewer: Reviewer,
  secrets: readonly string[] = [],
  directives?: readonly OwnerDirective[],
): string {
  return [
    `You are reviewer ${reviewer}. Review candidate ${phase.candidate?.sha} for phase ${phase.phaseId}.`,
    `Goal: ${phase.contract.goal}`,
    `Contract version: snapshot ${phase.contract.contractVersion.snapshot}`,
    ...secretPromptLines(secrets),
    ...directiveLines(directives),
    // Plan 01f: the same rule every reviewer prompt carries — the conductor
    // owns the gate evidence, and no agent may run the command or substitute
    // for its record.
    ...gatePromptLines(phase.contract.gate),
    // Plan 05d: name the candidate's own failing tests (each re-run alone), so
    // a reviewer never calls a real regression a flake or a flake a defect.
    ...checkFailurePromptLines(phase),
    // Plan 06b: the same checklist the worker saw, plus the worker's coverage
    // and the check resolution, and the verdicts this review must carry.
    ...(isStructured(phase.contract)
      ? [
          ...checklistLines(itemsFromPhase(phase.contract)),
          ...coverageLines(phase.coverage, itemsFromPhase(phase.contract)),
          ...checkResolutionLines(phase.checkResolution ?? []),
          ...reviewRequestLines(itemsFromPhase(phase.contract)),
        ]
      : []),
    ...((directives ?? []).some((d) => d.status === "in-force") ? ["", DIRECTIVE_BINDING_STATEMENT] : []),
    "Call submit_review with reviewer, phaseId, candidateSha, contractVersion, correctionStatements, findingStatements, items and arch.",
  ].join("\n");
}

/** Plan 06g: one candidate as a pick prompt names it. */
export interface PickPromptCandidate {
  lane: string;
  sha: string;
  /** `C<round>-<lane>`. */
  label: string;
}

export interface PickPromptInput {
  /** The seat voting: M, A or B. */
  seat: string;
  /** True for the leader seat (M), the only one that carries the full
   * context (plan 06g, A4). */
  leader: boolean;
  phaseId: string;
  goal: string;
  round: number;
  base: string;
  candidates: PickPromptCandidate[];
  /** Leader only: the settled ledger, one line per record. */
  ledger?: readonly string[];
  /** Leader only: every earlier round's reviews and votes, one line each. */
  earlierRounds?: readonly string[];
  /** Leader only: the other candidates' diffs, by lane. The candidate the
   * leader is voting on is in `candidates`, not here. */
  otherDiffs?: Readonly<Record<string, string>>;
  /** The seats that vote (the configured reviewer seats). */
  seats: readonly string[];
  /** Plan 06h (A3): true for the top-two revote turn, where the leader's
   * vote breaks a tie. */
  revote?: boolean;
}

/** Plan 06g (A4): the pick turn's prompt. Each seat votes for one candidate
 * with a one-line why. The leader (M) carries the full context — the settled
 * ledger, every earlier round's reviews and votes, and the other candidate's
 * diff — and its vote counts once, exactly like the others'. A and B carry
 * none of it: they see the round's candidates and vote.
 *
 * Pure and exported so a unit test exercises exactly the words the conductor
 * sends. */
export function buildPickPrompt(input: PickPromptInput): string {
  const lines: string[] = [
    `You are reviewer ${input.seat}${input.leader ? " (the leader seat)" : ""}. Cast your pick vote for phase ${input.phaseId}, round ${input.round}.`,
    `Goal: ${input.goal}`,
    `Round ${input.round} started from ${input.base}.`,
    "",
    `Candidates of round ${input.round}:`,
    ...input.candidates.map((c) => `- ${c.label} ${c.sha.slice(0, 7)} (lane ${c.lane})`),
  ];
  if (input.leader) {
    lines.push(
      "",
      "The settled ledger:",
      ...(input.ledger && input.ledger.length > 0 ? input.ledger.map((l) => `- ${l}`) : ["- (nothing settled yet)"]),
      "",
      "Earlier rounds' reviews and votes:",
      ...(input.earlierRounds && input.earlierRounds.length > 0 ? input.earlierRounds.map((l) => `- ${l}`) : ["- (this is round 1)"]),
    );
    for (const c of input.candidates) {
      const diff = input.otherDiffs?.[c.lane];
      if (diff === undefined) continue;
      lines.push("", `Candidate ${c.label}'s diff:`, diff.trim().length > 0 ? diff : "(no diff)");
    }
  }
  lines.push(
    "",
    `Vote for exactly one candidate of round ${input.round}, by lane, with a one-line why. The candidates are checked one after the other and only a passing candidate can win.`,
    input.revote
      ? `This is the REVOTE between the top two. Seats voting: ${input.seats.join(", ")}. The seats other than the leader need a strict majority; a tie among them is broken by the leader's vote.`
      : `Seats voting: ${input.seats.join(", ")}. A candidate needs a strict majority (${Math.floor(input.seats.length / 2) + 1} of ${input.seats.length}); with a single passing candidate the vote is skipped and it wins.`,
    "Your vote counts once, exactly like every other seat's.",
    "Cast it with submit_pick_vote: { round, seat, lane, why }.",
  );
  return lines.join("\n");
}

/** The tree object id of `rev` in `repo`, or undefined if it cannot be read. */
function treeOf(repo: string, rev: string): string | undefined {
  try {
    return execFileSync("git", ["-C", repo, "rev-parse", `${rev}^{tree}`], { encoding: "utf8" }).trim();
  } catch {
    return undefined;
  }
}

/** True iff `dir` already holds a Pi session file (a previous attempt or
 * round of this role), so the next launch should `--continue` it. */
function hasSessionFile(dir: string): boolean {
  try {
    return fs.readdirSync(dir).some((f) => f.endsWith(".jsonl"));
  } catch {
    return false;
  }
}

/** Plan 05d / finding #33: the bytes a session directory holds — the scale a
 * retried hello's longer limit uses. Best-effort: an unreadable directory
 * reports 0, which leaves the base limit. */
export function sessionBytes(dir: string): number {
  let total = 0;
  try {
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
      const file = path.join(dir, entry.name);
      try {
        if (entry.isFile()) total += fs.statSync(file).size;
        else if (entry.isDirectory()) total += sessionBytes(file);
      } catch {
        // best effort per entry
      }
    }
  } catch {
    return 0;
  }
  return total;
}

/** Plan 05d / finding #33: the limit a retried hello gets. A session being
 * continued with `--continue` takes longer to load the bigger it is, so the
 * limit grows with its bytes (50 ms per KiB) but never below the configured
 * limit and never above 60 s. Pure, so the scale is unit-tested directly. */
export function helloRetryTimeoutMs(baseMs: number, sessionBytes: number): number {
  const scaled = Math.ceil(Math.max(0, sessionBytes) / 1024) * 50;
  return Math.min(60_000, Math.max(baseMs, scaled));
}

function currentHead(repo: string, branch: string): string {
  return execFileSync("git", ["-C", repo, "rev-parse", branch], { encoding: "utf8" }).trim();
}

/** design §2.1: assert the pinned Pi version before ever launching the real
 * `pi` binary — an upgrade is a deliberate PR that reruns the phase 0
 * contract tests, not something that should silently drift. Only called
 * when no `piCommand` override was given (never for fake-pi in tests). */
function assertPiVersion(): void {
  let output: string;
  try {
    output = execFileSync("pi", ["--version"], { encoding: "utf8" }).trim();
  } catch (err) {
    throw new Error(
      `could not run 'pi --version' to assert the pinned Pi version (${PI_VERSION}): ${String((err as Error)?.message ?? err)}`,
    );
  }
  if (!output.includes(PI_VERSION)) {
    throw new Error(
      `pi reports version '${output}', expected the pinned ${PI_VERSION} (design §2.1) — upgrading is a deliberate PR that reruns the phase 0 contract tests`,
    );
  }
}
