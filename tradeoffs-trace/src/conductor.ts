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
import { normalizeDecisionViewCommand, ownerCommandToEvent } from "./core/owner-inbox.ts";
import { next } from "./core/next.ts";
import { effectiveChecks } from "./core/checks.ts";
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
  baselineKey,
  classifyCheckFailure,
  failedNormally,
  parseBaseline,
  parseTestFailures,
  type Baseline,
  type BaselineCommand,
} from "./core/test-failures.ts";
import type {
  Action,
  Ballot,
  BallotDisclosure,
  ContractVersion,
  CriterionDispute,
  Decision,
  DecisionDisclosure,
  DirectiveScope,
  Event,
  Finding,
  PriorDecisionStatement,
  FindingDisclosure,
  InFlightKey,
  OwnerCommand,
  OwnerDirective,
  OwnerInputKind,
  OwnerInputState,
  PhaseContract,
  PhaseState,
  Review,
  Reviewer,
  State,
} from "./core/types.ts";
import { computeBoundaryTriggerPaths, computeUnreferencedHunks } from "./core/boundaries.ts";
import { assertToolSet, launchArgs, PI_VERSION, ROLE_TOOLS, type Role, type ToolSetMismatch } from "./core/roles.ts";
import { decisionStatus, isLiveDecision, sameVersion } from "./core/predicate.ts";
import { resolvedCorrectionIdsFor } from "./core/predicate.ts";
import { notAcceptedReasons } from "./core/verdict.ts";
import type { HelloMessage, SubmitMessage } from "./core/protocol.ts";
import { validate } from "./core/schema.ts";

import { EventLog, readLog, type LogRecord } from "./effects/log.ts";
import { acquireLock, acquireWaitingLock, type Lock } from "./effects/lock.ts";
import { killGroup, childEnv, runCommand, type RunCommandResult } from "./effects/shell.ts";
import { sweep, type SweepResult } from "./effects/sweep.ts";
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
  verifyIntegrity, removedTestsBetween } from "./effects/git.ts";
import { RunSocketServer, type HelloResult, type SubmitResult } from "./effects/socket.ts";
import { PiAgent, spawnPiAgent } from "./effects/pi-rpc.ts";
import { redactBytes, redactRecord, redactText, resolveSecrets, secretNames, secretPromptLines, utf16Kind, type Secret } from "./effects/secrets.ts";
// Plan 01a: the secret guard itself lives with the other `sh` guards (they
// are wired into the agent's `tool_call` hook, and the conductor reuses the
// same refusal at the socket, where a scripted agent's commands arrive).
import { secretUseInCommand } from "../extension/guards.ts";
import { crashAt, CRASH_BOUNDARIES, PHASE_2_CRASH_BOUNDARIES } from "./effects/crash.ts";

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
   * dispatches nothing at all). */
  inboxPollMs: number;
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
  reviewMs: 15 * 60_000,
  reproductionMs: 5 * 60_000,
  abortGraceMs: 30_000,
  termGraceMs: 10_000,
  stopAbortGraceMs: 2_000,
  stallMs: 3 * 60_000,
  inboxPollMs: 1_000,
};

export interface RunPlanPhase {
  id: string;
  goal: string;
  acceptance: string[];
  checks: string[];
  boundaries: string[];
  reserved: string[];
  provisional?: boolean;
  /** Plan 01f: the phase's `:GATE:` command — the expensive, live proof the
   * conductor runs itself after checks, probe and reviews pass, and before
   * acceptance. Undeclared on every phase that predates plan 01f. */
  gate?: string;
  /** Plan 01f: the phase's `:GATE_CLEANUP:` command, run after the gate
   * whatever its outcome. */
  gateCleanup?: string;
}

/** The on-disk plan file `tt start` reads. Only phase 0 (index 0) is run by
 * this packet's single-phase conductor. */
export interface RunPlanFile {
  title: string;
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
  /** Plan 01i: program-wide owner directives already in force when this
   * run's node was started (D5). They seed the phase's directive list, so a
   * node started after the owner's ruling still carries it in every prompt.
   * Written by the program scheduler (`nodePlan`), never by hand. */
  ownerDirectives?: RunPlanDirectiveSeed[];
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
  providerModelFor?: (role: Role) => { provider?: string; model?: string } | undefined;
  extraEnv?: NodeJS.ProcessEnv;
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
  /** Plan 01f: the machine-wide gate lock's path. Defaults to
   * `~/.tradeoffs-trace/gate.lock`, so two phases (in one program or in two
   * runs) never gate at once; tests point it at a temp path so they neither
   * contend with a real run nor with each other. */
  gateLockPath?: string;
}

// ---------------------------------------------------------------------------
// Run directory layout (design §9.1)
// ---------------------------------------------------------------------------

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

/** A conductor whose own revision differs from the one the run was started
 * under refuses to resume it (plan, "How this plan is executed"). */
export class RunnerMismatchError extends Error {}

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
        gate: contract.gate,
        gateCleanup: contract.gateCleanup,
      }),
    )
    .digest("hex");
  return { snapshot: contract.contractVersion.snapshot + 1, sectionSha256 };
}

export function buildContract(phase: RunPlanPhase): PhaseContract {
  return {
    phaseId: phase.id,
    contractVersion: contractVersionFor(phase),
    goal: phase.goal,
    acceptance: phase.acceptance,
    checks: phase.checks,
    boundaries: phase.boundaries,
    reserved: phase.reserved,
    // Plan 01f: a declared gate is part of the frozen contract — the FSM
    // (next.ts/transitions.ts) reads it to decide whether the phase gates at
    // all, and every prompt that mentions the gate quotes the same text.
    ...(gateCommandOf(phase) ? { gate: phase.gate } : {}),
    ...(phase.gateCleanup ? { gateCleanup: phase.gateCleanup } : {}),
  };
}

export function initialState(
  runId: string,
  phase: RunPlanPhase,
  integrationHead: string,
  programDirectives: RunPlanDirectiveSeed[] = [],
): State {
  const contract = buildContract(phase);
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
    repairRoundsGranted: 3,
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
  const { maskable } = resolveSecrets(declared);
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
  if (initRecord) {
    const init = initRecord.event as { runId: string; integrationHead: string };
    runId = init.runId;
    integrationHead = init.integrationHead;
  } else {
    runId = randomUUID().slice(0, 8);
    integrationHead = currentHead(plan.repo, plan.integrationBranch);
  }
  return foldEvents(
    initialState(runId, plan.phases[0], integrationHead, plan.ownerDirectives ?? []),
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
}

export function rebuildTimeline(runDir: string, plan: RunPlanFile): Timeline {
  const { records } = readLog(runPaths(runDir).events);
  const init = records.find((r) => r.kind === "init")?.event as { runId: string; integrationHead: string } | undefined;
  let state = initialState(init?.runId ?? "", plan.phases[0], init?.integrationHead ?? "", plan.ownerDirectives ?? []);
  const phases: Timeline["phases"] = [];
  const rounds: Timeline["rounds"] = [];
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
    if (state.phase.phase !== before.phase.phase || phases.length === 0) {
      phases.push({ phase: state.phase.phase, at: record.ts });
    }
  }
  return { state, phases, rounds };
}

/** Folds every `"event"`-kind record in `records` (in order) through
 * `reduce()` onto `base`. Throws if one no longer reduces cleanly — see
 * `rebuildState`'s doc comment for why that is always a bug, not something
 * to paper over. */
function foldEvents(base: State, records: readonly LogRecord[], lenient = false): State {
  let state = base;
  for (const record of records) {
    if (record.kind !== "event") continue;
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
  /** Plan 01d: the records this dispatch's turn-2 prompt demanded a ballot
   * for (id → its one-line choice), captured when that prompt was built and
   * never recomputed, so a record added after it (a late discovery) is never
   * demanded. Absent for a worker, a stub review, or before turn 2. */
  demandedBallots?: Map<string, string>;
  /** Plan 01d: how many incomplete `submit_review` submissions this dispatch
   * has already had rejected (at most MAX_INCOMPLETE_REVIEW_REJECTIONS). */
  incompleteReviewRejections?: number;
}

export class Conductor {
  #runDir: string;
  #paths: ReturnType<typeof runPaths>;
  #plan: RunPlanFile;
  #deadlines: Deadlines;
  #piCommand: string | undefined;
  #piArgsPrefix: string[];
  #piCommandFor: ((role: Role) => string | undefined) | undefined;
  #piArgsPrefixFor: ((role: Role) => string[] | undefined) | undefined;
  #providerModelFor: ((role: Role) => { provider?: string; model?: string } | undefined) | undefined;
  #extraEnv: NodeJS.ProcessEnv;
  #piEnvFor: ((role: Role, agentId: string) => NodeJS.ProcessEnv | undefined) | undefined;
  #stubReviews: boolean;
  #probeReuse: boolean;
  /** Plan 01f: the machine-wide gate lock's path (default
   * `~/.tradeoffs-trace/gate.lock`). Held only while the gate command runs,
   * so two phases never gate at once. */
  #gateLockPath: string;
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
    this.#piEnvFor = opts.piEnvFor;
    this.#stubReviews = opts.stubReviews ?? false;
    this.#probeReuse = opts.probeReuse ?? true;
    this.#gateLockPath = opts.gateLockPath ?? path.join(os.homedir(), ".tradeoffs-trace", "gate.lock");
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
    const resolved = resolveSecrets(this.#secretNames);
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
      const init = initRecord.event as { runId: string; integrationHead: string; runnerRevision?: string };
      const mine = runnerRevision();
      if (init.runnerRevision && init.runnerRevision !== mine && process.env.TT_ALLOW_RUNNER_MISMATCH !== "1") {
        throw new RunnerMismatchError(
          `run was started under runner ${init.runnerRevision}; this conductor is ${mine} — refusing to resume (reinstall that runner, or start a new run)`,
        );
      }
      this.#state = initialState(init.runId, this.#plan.phases[0], init.integrationHead, this.#plan.ownerDirectives ?? []);
    } else {
      const head = currentHead(this.#plan.repo, this.#integrationBranch);
      const runId = randomUUID().slice(0, 8);
      this.#log.append("init", { runId, integrationHead: head, runnerRevision: runnerRevision() });
      this.#state = initialState(runId, this.#plan.phases[0], head, this.#plan.ownerDirectives ?? []);
    }
    this.#state = foldEvents(this.#state, records);
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
    // Reconcile above can itself reach a terminal state and fire the
    // auto-stop, so only arm the poll timer if `stop()` has not already run
    // (`#doStop` would have cleared an unset timer, and arming one here
    // afterwards would keep the process alive forever).
    if (!this.#closed) {
      this.#inboxTimer = setInterval(() => this.#scanInbox(), this.#deadlines.inboxPollMs);
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
  #recordedShGroups(records: readonly LogRecord[], agentId: string): number[] {
    const prefix = `sh-${agentId}-`;
    return records
      .filter((r) => r.kind === "intent" && typeof r.actionId === "string" && r.actionId.startsWith(prefix))
      .map((r) => (r.event as { pgid: number }).pgid)
      .filter((pgid) => Number.isFinite(pgid));
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

    if (key === "dispatch_worker") {
      const pgid = payload.pgid as number | undefined;
      if (pgid !== undefined) await killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined);
      for (const shPgid of this.#recordedShGroups(records, payload.agentId as string)) {
        await killGroup(shPgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined);
      }
      const sweepResult = await sweep(this.#paths.worktree, { exceptPids: [] });
      this.#log.append("sweep", sweepResult);
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      if (typeof payload.sessionDir === "string") this.#recoveredSessionDir = payload.sessionDir;
      this.#applyEvent({ type: "ATTEMPT_INTERRUPTED" });
      return;
    }

    if (key === "freeze") {
      const found = findCommitByTrailer(this.#paths.worktree, actionId);
      const sweepResult = await sweep(this.#paths.worktree, { exceptPids: [] });
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

    if (key === "run_checks") {
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      this.#applyEvent({ type: "CHECKS_INTERRUPTED" });
      return;
    }

    if (key === "dispatch_probe") {
      const candidateSha = payload.candidateSha as string;
      discardProbeByBranch(this.#plan.repo, this.#state.phase.runId, candidateSha);
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      this.#applyEvent({ type: "PROBE_INTERRUPTED" });
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
        if (typeof pgid === "number") await killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined);
      }
      discardProbeByBranch(this.#plan.repo, this.#state.phase.runId, candidateSha);
      this.#log.completion(actionId, { interrupted: true, reason: "crash-recovery" });
      this.#applyEvent({ type: "GATE_INTERRUPTED" });
      return;
    }

    if (key === "review_M" || key === "review_A" || key === "review_B") {
      const reviewer = key.slice("review_".length) as Reviewer;
      const pgid = payload.pgid as number | undefined;
      if (pgid !== undefined) await killGroup(pgid, { termGraceMs: this.#deadlines.termGraceMs }).catch(() => undefined);
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
  #assembleDecisions(candidateSha: string): Decision[] {
    const disclosures: DecisionDisclosure[] = this.#state.phase.pendingDisclosures ?? [];
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
    const dispute = this.#state.phase.pendingDispute;
    if (dispute) {
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
    // design §9.3: `tt stop` ends a run's conductor cleanly — the stop event
    // is logged (with why) before anything is torn down, so the record is
    // durable even if a later step is slow or fails.
    try {
      this.#log?.append("stop", { reason: "conductor stopped", at: new Date().toISOString() });
    } catch {
      // the log may already be closed (a second stop call); never fatal.
    }
    await this.#killLiveShGroups();
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
    const result = reduce(this.#state, logged);
    this.#log.append("event", logged);
    if (!result.ok) {
      this.#log.append("rejected", { event: logged, reason: result.reason });
      throw new Error(`conductor emitted an event reduce() rejected: ${result.reason}`);
    }
    this.#state = result.state;
    this.#syncBudgetTimer();
    this.drive();
    this.#maybeAutoStop();
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

  /** Plan 2d: the three input-box kinds (design §7.4), detected by shape in
   * either the flat `kind` form or the decision view's `type` form. */
  /** Plan 01g: the applied amendment whose id the owner's correction names,
   * or undefined. The id is matched as a whole token (`AM-p1-abcd1234`), so
   * prose around it is fine but a near-miss is not a revert. */
  #revertAmendmentForText(text: string): { decisionId: string; amendmentId: string } | undefined {
    for (const d of this.#state.phase.decisions) {
      if (!d.amendment || d.amendment.status !== "applied") continue;
      const escaped = d.amendment.id.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
      if (new RegExp(`\\b${escaped}\\b`).test(text)) return { decisionId: d.id, amendmentId: d.amendment.id };
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

  #ownerInputKindOf(raw: unknown): OwnerInputKind | undefined {
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
      const target = h.role === "worker" ? "worker" : (h.agentId.match(/^reviewer-([MAB])-/)?.[1] ?? h.agentId);
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
    this.#applyEvent({ type: "DIRECTIVE_WITHDRAWN", directiveId }, commandId);
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
    if (!worker) {
      const reason = "no worker attempt is running; the conductor refused the steer (it was not delivered)";
      this.#recordOwnerInput(commandId, "steer", text, "refused", boundAttemptId, reason);
      this.#rejectInboxFile(file, commandId, reason);
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

    // Plan 2d: the input box's kinds are handled before the conductor-state
    // mapping. A steer is external delivery, not a core event. A note or
    // correction is a core event but also carries the owner-input record
    // the status view shows. Input after the phase is terminal is refused
    // with the reason — never silently dropped. Plan 01i: every one of them
    // is also an owner directive, phase-scoped unless the sender asked for
    // `scope: "program"` (`C-u` in a run's input box, D5).
    const inputKind = this.#ownerInputKindOf(raw);
    let noteKindText: { text: string; scope: DirectiveScope; programWide: boolean } | undefined;
    if (inputKind) {
      const inputText = (raw as { text?: unknown }).text;
      if (typeof inputText !== "string" || inputText.trim().length === 0) {
        this.#rejectInboxFile(file, commandId, "an owner input must carry non-empty text");
        return;
      }
      if (this.#state.phase.phase === "DONE" || this.#state.phase.phase === "BLOCKED") {
        const reason = `the phase is ${this.#state.phase.phase}; the run no longer accepts owner input`;
        this.#recordOwnerInput(commandId, inputKind, inputText, "refused", undefined, reason);
        this.#rejectInboxFile(file, commandId, reason);
        return;
      }
      const scope: DirectiveScope = (raw as { scope?: unknown }).scope === "program" ? "program" : "phase";
      // Plan 01i: `withdraw OD-n` (or `withdraw ODP-n`) in the input box
      // withdraws one directive — whatever kind the phase would otherwise
      // have made of the text. A withdrawal that names no valid id is
      // refused visibly: it must never be recorded as a *new* binding ruling,
      // which would leave the intended one in force.
      const parsedWithdraw = parseWithdrawInput(inputText);
      if (parsedWithdraw?.kind === "malformed") {
        this.#recordOwnerInput(commandId, "withdraw", inputText, "refused", undefined, parsedWithdraw.reason);
        this.#rejectInboxFile(file, commandId, parsedWithdraw.reason);
        return;
      }
      if (parsedWithdraw?.kind === "withdraw") {
        this.#processWithdraw(file, commandId, inputText, parsedWithdraw.id, { forwardProgram: true, pushed: false });
        return;
      }
      // Plan 01g: a correction (or note) naming an applied amendment id
      // restores that criterion's original wording. It is a targeted owner
      // action, not a directive, and it never waits for the phase to be
      // parked on the owner.
      if (inputKind === "note" || inputKind === "correction") {
        const revert = this.#revertAmendmentForText(inputText);
        if (revert) {
          this.#applyRevertAmendment(file, commandId, inputText, revert.decisionId, revert.amendmentId);
          return;
        }
      }
      // A program-wide input (D5) in a run the scheduler started: the
      // program mints the one `ODP-n` record, so this run only forwards the
      // text — it must not mint a local id that could not match the
      // program's. A run that is NOT part of a program has nothing
      // program-wide to reach, so the input stays this phase's own directive
      // (never a `program`-scoped record in a run that has no program).
      const programWide = scope === "program" && this.#programDir() !== undefined;
      const effectiveScope: DirectiveScope = programWide ? "program" : "phase";
      if (inputKind === "steer") {
        if (programWide) {
          this.#processProgramWideInput(file, commandId, "steer", inputText);
          return;
        }
        this.#processSteerCommand(file, commandId, raw, inputText, effectiveScope);
        return;
      }
      noteKindText = { text: inputText, scope: effectiveScope, programWide };
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
    // command for another run or phase is stale by the same rule.
    if (boundRunId && boundRunId !== this.#state.phase.runId) {
      this.#rejectInboxFile(file, commandId, `command is bound to run ${boundRunId}, but this run is ${this.#state.phase.runId}`);
      return;
    }
    if (boundPhaseId && boundPhaseId !== this.#state.phase.phaseId) {
      this.#rejectInboxFile(file, commandId, `command is bound to phase ${boundPhaseId}, but this run is on phase ${this.#state.phase.phaseId}`);
      return;
    }
    const result = reduce(this.#state, event);
    if (!result.ok) {
      this.#rejectInboxFile(file, commandId, result.reason);
      return;
    }
    this.#appliedCommandIds.add(commandId);
    this.#applyEvent(event, commandId);
    // Plan 01i: a note or a correction is a directive too — recorded here,
    // on top of the event above, so it is steered to every live agent now
    // and quoted in every later prompt (a correction still resolves the open
    // requests and grants its 3 rounds first: today's behaviour, unchanged).
    if (noteKindText) {
      if (noteKindText.programWide) {
        this.#forwardProgramDirective(commandId, noteKindText.text);
      } else {
        this.#addDirective(noteKindText.text, noteKindText.scope, commandId);
      }
    }
    // The recorded effect for the input box's status view: a note is queued
    // for the next attempt the moment it is applied; a correction has just
    // resolved the open requests and started a repair.
    const recordedText = (raw as { text?: unknown }).text;
    if (event.type === "NOTE_ADDED" && typeof recordedText === "string") {
      this.#recordOwnerInput(commandId, "note", recordedText, "noted");
    } else if (event.type === "OWNER_CORRECTION" && typeof recordedText === "string") {
      this.#recordOwnerInput(commandId, "correction", recordedText, "correction-started");
    }
    crashAt("before_inbox_move");
    this.#moveInboxFile(file, this.#paths.inboxApplied);
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
    if (this.#state.phase.phase === "DONE" || this.#state.phase.phase === "BLOCKED") {
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
        const actions = next(this.#state);
        for (const action of actions) this.#dispatch(action);
      } while (this.#redriveRequested);
    } finally {
      this.#driving = false;
    }
  }

  #dispatch(action: Action): void {
    const kind = action.type === "dispatch_review" ? `dispatch_review_${action.reviewer}` : action.type;
    switch (action.type) {
      case "start_attempt":
        this.#applyEvent({ type: "ATTEMPT_STARTED" });
        return;
      case "dispatch_worker": {
        const actionId = this.#log.actionId(kind);
        this.#applyEvent({ type: "ACTION_STARTED", action: "dispatch_worker", actionId });
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
    if (msg.tool === "submit_phase") {
      if (handle.role !== "worker" || this.#state.phase.phase !== "IMPLEMENTING") {
        return { ok: false, reason: `submit_phase is not accepted in phase ${this.#state.phase.phase}` };
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
      if (handle.role !== "reviewer" || this.#state.phase.phase !== "REVIEWING") {
        return { ok: false, reason: `submit_review is not accepted in phase ${this.#state.phase.phase}` };
      }
      const review = msg.args as Review;
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
        if (missing.length > 0) {
          const rejections = handle.incompleteReviewRejections ?? 0;
          if (rejections < MAX_INCOMPLETE_REVIEW_REJECTIONS) {
            handle.incompleteReviewRejections = rejections + 1;
            this.#log.append("incomplete_review_rejected", {
              reviewer: review.reviewer,
              agentId,
              missing: missing.map((m) => m.id),
              rejection: rejections + 1,
            });
            return {
              ok: false,
              reason: `incomplete review: a ballot is required for every listed record not marked carried. Missing: ${missing
                .map((m) => `${m.id} (${m.choice})`)
                .join("; ")}`,
            };
          }
          this.#log.append("incomplete_review", {
            reviewer: review.reviewer,
            agentId,
            missing: missing.map((m) => m.id),
            rejections,
          });
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
        let error: string | undefined;
        try {
          error = await this.#applyReviewFindingsAndBallots(review);
        } catch (err) {
          error = `threw: ${String((err as Error)?.message ?? err)}`;
        }
        if (error) {
          this.#log.append("review_outcome_error", { reviewer: review.reviewer, error });
          return { ok: false, reason: error };
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
    if (msg.tool === "submit_discovery") {
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
    return { ok: false, reason: `unknown submission tool ${msg.tool}` };
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
    return (agentId.match(/^reviewer-([MAB])-/)?.[1] as Reviewer | undefined) ?? "M";
  }

  /** design §3.3's second decision source: a reviewer's turn-1
   * `submit_discovery`, made before it is shown the worker's own
   * disclosure. v1 matching (design §12 notes dedup precision is measured
   * later): every discovery becomes a *new* `reviewer-discovered` record —
   * no attach-to-existing-id matching yet. Assembles+binds each one exactly
   * like a worker's disclosure (id/version/boundCandidateSha/
   * boundContractVersion), validates it, and emits `DECISION_ADDED`. */
  #applyDiscoveries(discoveries: DecisionDisclosure[], reviewer: Reviewer): string | undefined {
    const candidate = this.#state.phase.candidate;
    if (!candidate) return "no candidate exists yet to bind a discovered decision to";
    if (this.#discoveryClosed()) {
      // Plan 2c no-unshown-ballots: the other reviewers are already voting on
      // the merged list, so a record added now could never get their ballots.
      this.#log.append("late_discovery", { reviewer, candidateSha: candidate.sha, discoveries });
      return undefined;
    }
    const K = this.#state.phase.contract.contractVersion;
    for (const d of discoveries) {
      const decision: Decision = {
        id: `D-${this.#state.phase.phaseId}-${candidate.sha.slice(0, 8)}-disc-${reviewer}-${this.#state.phase.decisions.length + 1}`,
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
      this.#applyEvent({ type: "DECISION_ADDED", decision });
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
  async #raiseFinding(fd: FindingDisclosure, reviewer: Reviewer, candidateSha: string): Promise<string | undefined> {
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
      severity: fd.severity,
      evidence: fd.evidence,
      raisedBy: reviewer,
      status: "open",
      boundCandidateSha: candidateSha,
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
    };
    const result = validate(FINDING_SCHEMA, finding);
    if (!result.valid) return `raised finding fails schemas/finding.schema.json: ${result.errors.join("; ")}`;
    this.#applyEvent({ type: "FINDING_RAISED", finding });
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

  /** Plan 01g: assembles a reviewer-raised `criterionDispute` into an
   * amendment record (a `reserved` decision) bound to the candidate/contract
   * being reviewed, exactly like a worker disclosure. It is votable like any
   * other reserved decision; if it passes, next() applies it. A reviewer
   * raises one in turn 2, after this round's ballot demand was captured, so
   * it is voted in a later round (carryDecisionsForward keeps amendments). */
  #addAmendmentDecision(dispute: CriterionDispute, raisedBy: Reviewer, candidateSha: string): void {
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
        why: dispute.why,
        raisedBy,
        status: "proposed",
        previousContractVersion: K,
      },
    };
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

    for (const fd of review.findings ?? []) {
      // Plan 2c: "same as F-…" records agreement instead of a duplicate.
      if (fd.sameAs) {
        const existing = this.#state.phase.findings.find((f) => f.id === fd.sameAs && f.status === "open");
        if (existing) {
          this.#applyEvent({ type: "FINDING_ALSO_RAISED", findingId: existing.id, reviewer: review.reviewer });
          continue;
        }
      }
      const { sameAs: _sameAs, ...disclosure } = fd;
      const error = await this.#raiseFinding(disclosure, review.reviewer, candidate.sha);
      if (error) return error;
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

  #onNoSubmission(agentId: string): void {
    const handle = this.#agents.get(agentId);
    // A reviewer's missing submission is detected from agent_settled in
    // #runReview; resolving donePromise here would count it as submitted.
    if (handle && handle.role === "worker") handle.doneResolve();
  }

  #cwdFor(agentId: string): string | undefined {
    const handle = this.#agents.get(agentId);
    if (!handle) return undefined;
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

    // Plan 01e: before the run's first attempt, run the phase's checks once on
    // the base and remember which test names already fail there, so (D2) the
    // candidate's gate can tell a pre-existing failure from a new one and the
    // worker is told which ones are not its to fix. A repair attempt never
    // re-runs it — the base tree has not moved — and `#ensureBaseline`
    // reuses an existing record for the same base tree and check list
    // (including one a sibling program node already paid for).
    if (this.#state.phase.attempt.n === 1) await this.#ensureBaseline();

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
      // Where find/grep/ls may search: the worktree and the plan's references.
      TT_SEARCH_ROOTS: [this.#paths.worktree, this.#paths.refs].join(path.delimiter),
      // Plan 01a: the plan's secrets — the names (so the extension guard
      // knows what to look for) and each value, which lives only here and in
      // the agent's own environment. Set last: these win over any
      // test-injected env, so a guard always sees the run's real value.
      TT_SECRETS: this.#secretNames.join(" "),
      ...Object.fromEntries(this.#secretValues.map((s) => [s.name, s.value])),
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
    let submittedKeepHandle = false;

    // design §9.3: the "agent attempt" row's own intent/completion pair —
    // logged once the pgid is known (agent.pgid, synchronously available
    // once spawnPiAgent returns), so a crash before this line leaves no
    // dangling in-flight entry at all (ACTION_STARTED had not been paired
    // with a live process yet); a crash after it is exactly what
    // `#reconcileOne`'s `dispatch_worker` case reconciles.
    this.#log.intent(actionId, { agentId, pgid: agent.pgid, sessionDir });
    crashAt("before_dispatch_worker");

    try {
      const hello = await raceTimeout(helloPromise, this.#deadlines.helloTimeoutMs, "hello");
      if (hello === "timeout") {
        await agent.terminate();
        this.#agents.delete(agentId);
        this.#log.completion(actionId, { outcome: "hello-timeout" });
        this.#applyEvent({ type: "ATTEMPT_TIMED_OUT" });
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
    const reviewed = (["M", "A", "B"] as const).some((w) => phase.reviews[w]?.review?.candidateSha === C);
    const checksFailed = phase.checks?.candidateSha === C && phase.checks.passed === false;
    const probeFailed = phase.probe?.candidateSha === C && phase.probe.passed === false;
    if (!reviewed && !checksFailed && !probeFailed) return undefined;
    const clip = (t: string, n = 600) => (t.length > n ? `${t.slice(0, n)}…` : t);
    const findingLine = (f: Finding) =>
      `${f.id} (${f.kind}, raised by ${f.raisedBy}${f.alsoRaisedBy?.length ? ` and ${f.alsoRaisedBy.join(", ")}` : ""}): ${clip(f.evidence)}`;
    const open = phase.findings.filter((f) => f.status === "open");
    const blocking = open.filter((f) => f.severity === "blocking").map(findingLine);
    if (checksFailed) blocking.unshift("The phase checks failed on the candidate (see the check output in your worktree by rerunning the failing test).");
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
    const failedDecisions: string[] = [];
    for (const d of phase.decisions) {
      if (!isLiveDecision(d) || d.class === "detail") continue;
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
      priorDecisions: phase.decisions
        .filter((d) => d.source === "worker" && isLiveDecision(d))
        .map((d) => ({ id: d.id, choice: d.choice })),
    };
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
    const result = await sweep(this.#paths.worktree, { exceptPids: [] });
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
      const sweepResult: SweepResult = await sweep(this.#paths.worktree, { exceptPids: [] });
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
      const sweepResult: SweepResult = await sweep(this.#paths.worktree, { exceptPids: [] });
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
    this.#applyEvent({
      type: "FREEZE_COMPLETED",
      candidateSha: outcome.candidateSha,
      decisions: outcome.decisions,
      tainted: outcome.tainted,
    });
    // Work packet 2a: boundary triggers (design §3.3) and §3.5's sampling
    // data need a real candidate (for the diff, and for DECISION_ADDED's
    // own binding check) — only possible once FREEZE_COMPLETED above has
    // set phase.candidate.
    this.#recordBoundaryDataAndSample(outcome.candidateSha);
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
    return effectiveChecks(this.#plan.checks, this.#state.phase.contract.checks).map((c) => this.#withValues(c));
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
    return baselineFailedCommands(baseline.commands);
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
    return record.commands.every((c, i) => c.command === this.#baselineCommandName(commands[i]));
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
      this.#recordBaselineLocal(record);
    } catch (err) {
      this.#log.append("baseline_error", { error: String((err as Error)?.message ?? err) });
      return undefined;
    }
    if (shared) this.#publishBaselineShared(record, shared);
    this.#log.append("baseline", { ...record, reused: false });
    return record;
  }

  /** Reuses a record another run took: the record and its logs are copied into
   * this run's own `checks/base/`, so the run directory stays self-contained,
   * and the reuse is logged with where it came from. */
  #adoptBaseline(record: Baseline, sourceDir: string, source: string): void {
    this.#writeBaselineLocal(record, sourceDir);
    this.#log.append("baseline", { ...record, reused: true, source });
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
        // Same isolation and per-command deadline as the C gate (F13).
        const running = runCommand({
          command,
          cwd: checkout.dir,
          env: childEnv(),
          deadlineMs: this.#deadlines.checkMs,
          termGraceMs: this.#deadlines.termGraceMs,
        });
        const result = await running.result;
        this.#recordCheck(outDir, command, result);
        results.push({
          command,
          exitCode: result.exitCode,
          signal: result.signal,
          timedOut: result.timedOut,
          durationMs: Date.now() - startedAt,
          // Only a command that ran to completion and exited non-zero names a
          // failure the base is known to have: a timeout's output is truncated,
          // a signal death never printed its last failure, and a command that
          // exited 0 did not fail at all — so none of those three may put a
          // name into the set that excuses a candidate's check.
          failures: failedNormally(result) ? parseTestFailures(result.output) : [],
          log: this.#checkLogName(command),
        });
      }
    } finally {
      checkout.dispose();
    }
    return {
      baseSha: this.#baselineBaseSha(),
      tree: this.#baselineTree(),
      key,
      at: new Date().toISOString(),
      commands: results,
      failures: baselineFailureNames(results),
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
    try {
      const before = verifyIntegrity(this.#plan.repo, checkoutDir.dir, candidateSha);
      let passed = before;
      let integrityViolated = !before;
      let timedOut = false;
      /** Plan 01e: the names this candidate's checks failed on that the base
       * did not — recorded with the check's completion so a repair round (and
       * the owner) can see exactly what is new. */
      let newFailures: string[] = [];
      if (before) {
        // F04: the effective list is the global plan checks followed by the
        // phase contract's own checks, deduped by exact command string — the
        // same list `#runProbe` executes against the merged integration I.
        for (const rawCommand of effectiveChecks(this.#plan.checks, this.#state.phase.contract.checks)) {
          // Plan 01a: a plan that pasted a value into its check line gets it
          // back here (the snapshot it may have been read from is masked).
          const command = this.#withValues(rawCommand);
          // design §8.1: "each check command | 10 min | kill its group |
          // check failed: timeout" — runCommand's own deadlineMs already
          // kills the command's process group on expiry (shell.ts); this
          // just records *why* the check failed.
          const running = runCommand({
            command,
            cwd: checkoutDir.dir,
            // F13: isolate the child from the Node test runner's own
            // recursion markers so a check that runs `node --test` actually
            // runs (and can fail) instead of silently skipping.
            env: childEnv(),
            deadlineMs: this.#deadlines.checkMs,
            termGraceMs: this.#deadlines.termGraceMs,
          });
          const result = await running.result;
          if (result.timedOut) timedOut = true;
          this.#recordCheck(outDir, command, result);
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
              const verdict = classifyCheckFailure(result.output, baseFailures);
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
                newFailures = verdict.newFailures;
                this.#log.append("check_failure_new", {
                  candidateSha,
                  command,
                  newFailures: verdict.newFailures,
                  failures: verdict.parsed,
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
      crashAt("after_run_checks");
      this.#log.completion(actionId, {
        candidateSha,
        passed,
        integrityViolated,
        ...(newFailures.length > 0 ? { newFailures } : {}),
        reason: !passed && timedOut ? "timeout" : undefined,
      });
      this.#applyEvent(passed ? { type: "CHECKS_PASSED" } : { type: "CHECKS_FAILED" });
    } finally {
      checkoutDir.dispose();
    }
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
    for (const rawCommand of reuse ? [] : effectiveChecks(this.#plan.checks, this.#state.phase.contract.checks)) {
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
      this.#applyEvent({ type: "PROBE_PASSED", probedI: result.I });
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
          const running = runCommand({
            command,
            cwd: probed.checkoutDir,
            env: this.#gateEnv(),
            deadlineMs: this.#deadlines.gateMs,
            termGraceMs: this.#deadlines.termGraceMs,
            // Design §2.2: the command's process group is recorded before it
            // runs, so a crashed gate is recoverable (see `#reconcileOne`).
            onIntent: ({ pgid }) => this.#log.intent(`gate-sh-${actionId}-${pgid}`, { pgid }),
          });
          const gateResult = await running.result;
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
            const cleanup = runCommand({
              command: cleanupCommand,
              cwd: probed.checkoutDir,
              env: this.#gateEnv(),
              deadlineMs: this.#deadlines.gateMs,
              termGraceMs: this.#deadlines.termGraceMs,
              onIntent: ({ pgid }) => this.#log.intent(`gate-cleanup-sh-${actionId}-${pgid}`, { pgid }),
            });
            cleanupResult = await cleanup.result;
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

  async #runReview(actionId: string, reviewer: Reviewer): Promise<void> {
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
    const reviewerProviderModel = this.#providerModelFor?.("reviewer");

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
        this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
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
          this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
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
          buildReviewerPrompt(this.#state.phase, reviewer, this.#secretNames, this.#state.phase.ownerDirectives),
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
        this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
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
      await agent.prompt(this.#buildReviewerTurn1Prompt(reviewer));
      const turn1 = await Promise.race([discoveryPromise.then(() => "discovered" as const), reviewTimeout.promise, settled1]);
      if (turn1 !== "discovered") {
        reviewTimeout.cancel();
        await agent.terminate();
        const why = turn1 === "settled" ? "settled without submit_discovery (turn 1)" : "timeout (turn 1: submit_discovery)";
        this.#log.completion(actionId, { reviewer, ok: false, reason: why });
        this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
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
        this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
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
        this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
        return;
      }
      const settled2 = nextSettle();
      await agent.prompt(this.#buildReviewerTurn2Prompt(reviewer, handle));
      const turn2 = await Promise.race([donePromise.then(() => "submitted" as const), reviewTimeout.promise, settled2]);
      reviewTimeout.cancel();
      if (turn2 === "submitted") {
        await agent.terminate();
        this.#log.completion(actionId, { reviewer, ok: true });
        return;
      }
      await agent.terminate();
      const why2 = turn2 === "settled" ? "settled without submit_review (turn 2)" : "timeout (turn 2: submit_review)";
      this.#log.completion(actionId, { reviewer, ok: false, reason: why2 });
      this.#applyEvent({ type: "REVIEW_TIMED_OUT", reviewer });
    } finally {
      this.#agents.delete(agentId);
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
    return (["M", "A", "B"] as const).every((w) => b.arrived.has(w) || phase.reviews[w]?.review?.candidateSha === candidate);
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
  #buildReviewerTurn1Prompt(reviewer: Reviewer): string {
    const phase = this.#state.phase;
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
      `Candidate checkout (read-only): ${this.#candidateDir()}`,
      ...referenceLines(runReferences(this.#runDir)),
      ...directiveLines(phase.ownerDirectives),
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
  #buildReviewerTurn2Prompt(reviewer: Reviewer, handle?: AgentHandle): string {
    const phase = this.#state.phase;
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
      "Records:",
      ...(live.length > 0 ? live.map(record) : ["- (none)"]),
    ];
    if (openFindings.length > 0) {
      lines.push(
        "Open findings:",
        ...openFindings.map((f) => `- ${f.id} [${f.severity} ${f.kind}, raised by ${f.raisedBy}]: ${f.evidence}`),
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
      "- `findings`: correctness problems only — defects, contract violations — with file:line or a scenario as evidence and a severity. A candidate that violates an owner directive is a blocking contract finding: cite the directive id as its evidence. If a problem is already an open finding above, set `sameAs` to its id instead of repeating it. If the problem is that a criterion cannot be met AS WRITTEN, add `criterionDispute` = { criterion: <the acceptance item verbatim>, why, proposedWording }: the conductor records an amendment voted on like any reserved record (a passing one replaces the wording; a failed one leaves it unchanged). An unmet-but-clear criterion is an ordinary defect finding.",
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
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
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
): string {
  const lines: string[] = [
    `Goal: ${contract.goal}`,
    "",
    "Acceptance criteria:",
    ...contract.acceptance.map((a) => `- ${a}`),
    ...secretPromptLines(secrets),
    ...referenceLines(references),
    ...baselinePromptLines(baselineCommands),
  ];
  if (contract.boundaries.length > 0) lines.push("", "Boundaries:", ...contract.boundaries.map((b) => `- ${b}`));
  if (ownerNotes) lines.push("", `Owner notes: ${ownerNotes}`);
  lines.push(...directiveLines(directives));
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
    ...((directives ?? []).some((d) => d.status === "in-force") ? ["", DIRECTIVE_BINDING_STATEMENT] : []),
    "Call submit_review with reviewer, phaseId, candidateSha, contractVersion, correctionStatements and findingStatements.",
  ].join("\n");
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
