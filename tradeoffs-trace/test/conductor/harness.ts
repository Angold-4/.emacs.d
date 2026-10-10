// Shared test harness for conductor tests: a disposable git repo, a short
// run root (macOS's 104-byte unix-socket path limit rules out the default
// `os.tmpdir()` on this machine — see socket.ts's module comment — so every
// helper here roots temp dirs at `/tmp/tt-*` explicitly, never
// `os.tmpdir()`), and small utilities for driving fake-pi through the
// conductor.

import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

import { Conductor, createRun, runPaths, type ConductorOptions, type RunPlanFile } from "../../src/conductor.ts";
import { planModelSelector, ROLE_TOOLS, type PlanModels } from "../../src/core/roles.ts";
import { itemsNeedingEvaluatorReverify, roundPanelItemsNeedingVote } from "../../src/core/predicate.ts";
import { phaseItemOutcomes, thinMetItems, workerAnchorsOf } from "../../src/core/items.ts";

import type { Reviewer, State } from "../../src/core/types.ts";
import { readLog, type LogRecord } from "../../src/effects/log.ts";

export const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));

function shortTmp(prefix: string): string {
  // `mkdtemp` guarantees the name is NEW (it retries on collision). A plain
  // random name written into with `mkdirSync(..., {recursive: true})` silently
  // reuses an existing directory when the name collides — and thousands of
  // `tt-*` directories accumulate in /tmp from earlier runs, so that is a real
  // draw. A reused repo directory is a confusing failure: the fresh
  // `git init` sees the old `base` commit, the identical README changes
  // nothing, and `git commit` fails with "nothing to commit, working tree
  // clean" (observed failing the phase's own `make check`).
  return fs.mkdtempSync(path.join("/tmp", `${prefix}-`));
}

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

export interface TestRepo {
  dir: string;
  head: string;
}

/** A fresh git repo with one commit on `main`, under a short `/tmp` path. */
export function makeRepo(): TestRepo {
  const dir = shortTmp("tt-repo");
  git(["init", "-q", "-b", "main"], dir);
  fs.writeFileSync(path.join(dir, "README.md"), "base\n");
  git(["add", "-A"], dir);
  git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], dir);
  // Names the real cause if the commit above did not happen: `rev-parse HEAD`
  // fails with "Needed a single revision" instead of leaving a test to fail
  // later for reasons that look unrelated.
  const head = git(["rev-parse", "HEAD"], dir);
  return { dir, head };
}

export function makeRunRoot(): string {
  return shortTmp("tt-run");
}

export function cleanupDir(dir: string): void {
  try {
    // Materialized candidates are read-only; make everything writable
    // first so rm can remove it.
    execFileSync("chmod", ["-R", "u+w", dir]);
  } catch {
    // best effort
  }
  fs.rmSync(dir, { recursive: true, force: true });
}

export interface FakePiStep {
  kind: string;
  [key: string]: unknown;
}

export function writeScript(dir: string, name: string, script: { hello?: unknown; steps: FakePiStep[] }): string {
  const file = path.join(dir, `${name}.json`);
  fs.writeFileSync(file, JSON.stringify(script));
  return file;
}

export function defaultWorkerHello() {
  return { role: "worker" as const, tools: ROLE_TOOLS.worker };
}

export function defaultReviewerHello() {
  return { role: "reviewer" as const, tools: ROLE_TOOLS.reviewer };
}

export interface TestConductorSetup {
  repo: TestRepo;
  runRoot: string;
  scriptsDir: string;
  plan: RunPlanFile;
  runDir: string;
  conductor: Conductor;
}

/** Starts a Conductor wired to fake-pi, with a worker script written up
 * front (`workerScript`) and reviewer scripts generated lazily via
 * `reviewerScriptFor` (called once per reviewer, at the moment the
 * conductor is about to spawn it, so the script can embed the *live*
 * candidate sha the conductor just froze — a real reviewer is simply told
 * this by the conductor; fake-pi has to be handed it via its script file
 * instead). */
export async function setupConductor(opts: {
  /** Shorthand: sets both the global and phase check lists to this value
   * (the harness's historical behavior). Use `globalChecks`/`phaseChecks`
   * for a run whose two lists differ. */
  checks?: string[];
  /** The plan's global `TT_CHECKS` list. Defaults to `checks`, then
   * `["true"]`. */
  globalChecks?: string[];
  /** The phase contract's own `:CHECKS:` list. Defaults to `checks`, then
   * `["true"]`. */
  phaseChecks?: string[];
  workerScript: (setup: { repo: TestRepo }) => { hello?: unknown; steps: FakePiStep[] };
  /** Work packet 2a: an attempt-aware alternative to the static
   * `workerScript` — needed by tests where a repair round's second attempt
   * must submit different (or empty) decisions than the first (e.g.
   * contract-objection.test.ts, so the repair does not re-disclose a
   * duplicate decision). Takes precedence over `workerScript` when given. */
  workerScriptForAttempt?: (attempt: number, setup: { repo: TestRepo }) => { hello?: unknown; steps: FakePiStep[] };
  reviewerScriptFor?: (reviewer: Reviewer, state: State) => { hello?: unknown; steps: FakePiStep[] };
  /** Plan 04a: the EVALUATING stage's fresh evaluator, ONE PER MESSAGE TYPE.
   * The default script returns an empty `submit_evaluation`, which the
   * conductor refuses (a raw message of the type is uncovered); the type then
   * times out and its raw messages are published unchanged, marked
   * `unevaluated` — enough for the ~30 tests that never raise a message to
   * evaluate. A test that wants real publication supplies its own script,
   * keyed by the message type it is dispatched for. */
  evaluatorScriptFor?: (messageType: string, state: State) => { hello?: unknown; steps: FakePiStep[] };
  /** Plan 05j: the curator's script. Default: an empty `curate_entries` pass. */
  curatorScriptFor?: (agentId: string, state: State) => { hello?: unknown; steps: FakePiStep[] };
  /** Decision briefs: the brief-writing evaluator pass. Default: settle
   * without a brief, so the conductor's deterministic backstop covers every
   * item. A test supplies a script that calls `submit_brief`. */
  briefScriptFor?: (state: State) => { hello?: unknown; steps: FakePiStep[] };
  /** Decision briefs: enable the conductor's brief step (default false, as
   * for every existing test). */
  briefs?: boolean;
  /** Plan 04b: one fresh panel seat per raw blocker per seat number. The
   * default script calls `submit_panel_vote` with a `downgrade` vote, so a
   * blocker that a test raises never parks an otherwise-passing run. A test
   * that wants a real panel verdict supplies its own script, keyed by the
   * blocker id and seat. */
  panelScriptFor?: (blockerId: string, seat: number, state: State) => { hello?: unknown; steps: FakePiStep[] };
  /** Plan 04b: the default panel seat script's vote. */
  defaultPanelVote?: "block" | "downgrade";
  /** Plan 05e: one script per round-panel seat. The default votes `keep` on
   * every pending item, which changes no message's state (the pre-05e
   * behaviour for a published trade-off / blocking finding). */
  roundPanelScriptFor?: (seat: number, state: State) => { hello?: unknown; steps: FakePiStep[] };
  /** Plan 05e: the default round-panel vote for every item. */
  defaultRoundPanelVote?: "keep" | "drop" | "downgrade";
  deadlines?: ConductorOptions["deadlines"];
  /** Extra argv tokens prepended before fake-pi.ts's own path — fake-pi
   * never parses argv, so these are inert except as a unique, greppable
   * marker in every agent process's command line (`ps`/`pgrep -f`), e.g.
   * for a test that must assert no orphan process remains afterwards. */
  extraPiArgsPrefix?: string[];
  /** Work packet 2a: `Conductor`'s own default is real reviewers
   * (`stubReviews: false`); this harness defaults to `true` instead, so the
   * ~30 pre-existing conductor tests (whose reviewer scripts predate the
   * real two-turn discovery/review protocol) keep working unchanged. Tests
   * exercising the real protocol pass `stubReviews: false` explicitly. */
  stubReviews?: boolean;
  /** Plan 2c: pass `false` to make the integration probe rerun the checks
   * even when the probed tree equals the candidate's. */
  probeReuse?: boolean;
  /** Phase 2b: extra env vars merged into every fake-pi worker's
   * environment (e.g. `FAKE_PI_PROMPT_LOG`, to capture the prompt text the
   * conductor actually sent). */
  extraWorkerEnv?: NodeJS.ProcessEnv;
  /** Plan 2c: extra env vars per reviewer (e.g. a per-reviewer
   * `FAKE_PI_PROMPT_LOG`). */
  extraReviewerEnv?: (reviewer: Reviewer) => NodeJS.ProcessEnv | undefined;
  /** Work packet 2a: BOUNDARIES globs for the phase's contract, so a test
   * can exercise conductor-computed boundary triggers (design §3.3).
   * Defaults to `[]` (no boundaries), exactly as before this option
   * existed. */
  boundaries?: string[];
  /** Work packet 2a: path-shaped acceptance criteria, exercising the
   * "acceptance files" boundary-trigger input. Appended after the fixed
   * `"it works"` acceptance criterion. */
  acceptanceFiles?: string[];
  /** Plan 01a: the plan's `#+TT_SECRETS` names, as Emacs would put them in
   * the JSON plan. The values are read from the harness process's own
   * environment (set them with `process.env.NAME = …` before calling). */
  secrets?: string[];
  /** The phase's own goal text (default "do the thing"). A plan's prose is a
   * secret-value carrier too, so a test can quote one in it. */
  goal?: string;
  /** Plan 06b (OD-2 R1): an exact phase object (e.g. the real Emacs parser's
   * output) to use instead of the harness's built one. */
  phase?: import("../../src/conductor.ts").RunPlanPhase;
  /** Plan 06b: the phase's structured items (architecture/requirements/
   * constraints). When given, the acceptance list is derived from the
   * requirements' texts so the item loop (coverage, per-item verdicts, item
   * acceptance) applies exactly as for an Emacs-parsed plan. */
  items?: {
    architecture?: import("../../src/core/items.ts").ArchitectureItem[];
    requirements?: import("../../src/core/items.ts").RequirementItem[];
    constraints?: import("../../src/core/items.ts").ConstraintItem[];
  };
  /** Plan 01f: the phase's `:GATE:` command (omitted = no gate, the
   * pre-01f pipeline) and its `:GATE_CLEANUP:` companion. */
  gate?: string;
  gateCleanup?: string;
  /** Plan 01f: the machine-wide gate lock's path. Two conductors in one test
   * share it to prove their gates never overlap. */
  gateLockPath?: string;
  /** The plan's title (default "test plan") — plan prose like any other. */
  title?: string;
  /** #+TT_MODELS: per-role provider/model for the plan (design §2.1). The
   * harness turns it into `providerModelFor` with the same one-liner
   * `tt start` uses; a test asserts the resulting launch argv. */
  models?: PlanModels;
  /** Extra env merged into every agent's own environment (the Conductor's
   * `extraEnv`), e.g. `FAKE_PI_ARGV_LOG` to record what each role was
   * launched with. */
  extraEnv?: NodeJS.ProcessEnv;
  /** Plan 01b: the conductor's notification clock (injectable), so a test
   * can advance past the 30-minute reminder without waiting. */
  now?: () => number;
}): Promise<TestConductorSetup> {
  const repo = makeRepo();
  const runRoot = makeRunRoot();
  const scriptsDir = shortTmp("tt-scripts");

  const plan: RunPlanFile = {
    title: opts.title ?? "test plan",
    repo: repo.dir,
    integrationBranch: "main",
    checks: opts.globalChecks ?? opts.checks ?? ["true"],
    ...(opts.secrets ? { secrets: opts.secrets } : {}),
    ...(opts.models ? { models: opts.models } : {}),
    phases: [
      opts.phase ?? {
        id: "p1",
        goal: opts.goal ?? "do the thing",
        acceptance: opts.items?.requirements ? opts.items.requirements.map((r) => r.text) : ["it works", ...(opts.acceptanceFiles ?? [])],
        checks: opts.phaseChecks ?? opts.checks ?? ["true"],
        boundaries: opts.boundaries ?? [],
        reserved: [],
        ...(opts.items ?? {}),
        ...(opts.gate ? { gate: opts.gate } : {}),
        ...(opts.gateCleanup ? { gateCleanup: opts.gateCleanup } : {}),
      },
    ],
  };

  const runDir = createRun(runRoot, plan);
  const workerScriptPath = opts.workerScriptForAttempt ? undefined : writeScript(scriptsDir, "worker", opts.workerScript({ repo }));
  const workerScriptPaths = new Map<number, string>();

  const reviewerScriptPaths = new Map<string, string>();
  const evaluatorScriptPaths = new Map<string, string>();
  const briefScriptPaths = new Map<string, string>();
  const curatorScriptPaths = new Map<string, string>();
  const panelScriptPaths = new Map<string, string>();
  const roundPanelScriptPaths = new Map<string, string>();

  const defaultPanelScript = (blockerId: string, seat: number) => ({
    hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
    steps: [
      {
        kind: "call-submit",
        tool: "submit_panel_vote",
        args:
          opts.defaultPanelVote === "block"
            ? {
                blockerId,
                seat,
                vote: "block",
                reason: `seat ${seat} votes to stop`,
                options: [
                  { id: "repair", label: "repair it (grant 3 rounds)" },
                  { id: "accept_risk", label: "accept the risk" },
                ],
              }
            : { blockerId, seat, vote: "downgrade", reason: `seat ${seat} votes to keep working` },
      },
    ],
  });

  const defaultRoundPanelScript = (state: State) => {
    const vote = opts.defaultRoundPanelVote ?? "keep";
    const items = roundPanelItemsNeedingVote(state.phase);
    return {
      hello: { role: "panel" as const, tools: ROLE_TOOLS.panel },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_round_panel_votes",
          args: { votes: items.map((messageId) => ({ messageId, verdict: vote, reason: `default round-panel ${vote}` })) },
        },
      ],
    };
  };

  // Plan 06b (OD-2 A3): the default evaluator answers any owed item re-check
  // with a `confirmed` check, so a test that does not script the evaluator is
  // not re-prompted. A test that wants the re-prompt/unchecked path supplies
  // its own `evaluatorScriptFor` with no itemChecks.
  const defaultEvaluatorScript = (state: State, messageType: string) => {
    // Plan 06c (R9): the default evaluator cites a real file of the candidate
    // (every harness repo has README.md) so a `confirmed` check's anchors
    // validate; a confirmed check with anchors that do not exist is recorded
    // `unchecked by evaluator`.
    const owed = messageType === "finding" && itemsNeedingEvaluatorReverify(state.phase) ? owedItemIds(state) : [];
    return {
      hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_evaluation",
          args: {
            evaluations: [],
            ...(owed.length > 0
              ? { itemChecks: owed.map((id) => ({ id, verdict: "confirmed", evidence: "README.md:1 evaluator: re-checked the candidate and found no contradiction" })) }
              : {}),
          },
        },
      ],
    };
  };

  const conductor = new Conductor({
    runDir,
    plan,
    piCommand: process.execPath,
    // FAKE_PI_PATH must be the first token (it's the script `node` runs);
    // any extra marker tokens go after it, as fake-pi's own (ignored)
    // argv — before it, `node` would try to parse them as its own CLI
    // flags and refuse to start.
    piArgsPrefix: [FAKE_PI_PATH, ...(opts.extraPiArgsPrefix ?? [])],
    // The same one-liner `tt start` uses, so a plan's #+TT_MODELS reaches
    // every launch in tests exactly as it does in production.
    providerModelFor: planModelSelector(plan),
    extraEnv: opts.extraEnv,
    deadlines: opts.deadlines,
    stubReviews: opts.stubReviews ?? true,
    briefs: opts.briefs ?? false,
    probeReuse: opts.probeReuse,
    ...(opts.now ? { now: opts.now } : {}),
    gateLockPath: opts.gateLockPath,
    piEnvFor: (role, agentId) => {
      if (role === "worker") {
        if (opts.workerScriptForAttempt) {
          const attempt = Number(agentId.match(/^worker-(\d+)-/)?.[1] ?? "1");
          if (!workerScriptPaths.has(attempt)) {
            workerScriptPaths.set(attempt, writeScript(scriptsDir, `worker-${attempt}`, opts.workerScriptForAttempt(attempt, { repo })));
          }
          return { FAKE_PI_SCRIPT: workerScriptPaths.get(attempt)!, ...(opts.extraWorkerEnv ?? {}) };
        }
        return { FAKE_PI_SCRIPT: workerScriptPath!, ...(opts.extraWorkerEnv ?? {}) };
      }
      if (role === "evaluator") {
        // Decision briefs: the brief-writing pass is its own evaluator agent
        // (`briefs-<actionId>`); if a test did not script it, it settles with
        // no submission and the deterministic backstop covers the items.
        if (agentId.startsWith("briefs-")) {
          if (!briefScriptPaths.has(agentId)) {
            const script = opts.briefScriptFor
              ? opts.briefScriptFor(conductor.state)
              : { hello: { role: "evaluator" as const, tools: ROLE_TOOLS.evaluator }, steps: [] };
            briefScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
          }
          return { FAKE_PI_SCRIPT: briefScriptPaths.get(agentId)! };
        }
        // One evaluator per message type per dispatch; a fresh script for
        // each so a test can vary by type and round.
        const messageType = agentId.match(/^evaluator-([a-z]+)-/)?.[1] ?? "tradeoff";
        if (!evaluatorScriptPaths.has(agentId)) {
          const script = opts.evaluatorScriptFor
            ? opts.evaluatorScriptFor(messageType, conductor.state)
            : defaultEvaluatorScript(conductor.state, messageType);
          evaluatorScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
        }
        return { FAKE_PI_SCRIPT: evaluatorScriptPaths.get(agentId)! };
      }
      if (role === "curator") {
        // Plan 05j: the round's curator. The default pass proposes nothing,
        // so the round is marked curated at once and the evaluators are never
        // delayed; a test may supply its own proposals.
        if (!curatorScriptPaths.has(agentId)) {
          const script = opts.curatorScriptFor
            ? opts.curatorScriptFor(agentId, conductor.state)
            : { hello: { role: "curator" as const, tools: ROLE_TOOLS.curator }, steps: [{ kind: "call-submit", tool: "curate_entries", args: { proposals: [] } }] };
          curatorScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
        }
        return { FAKE_PI_SCRIPT: curatorScriptPaths.get(agentId)! };
      }
      if (role === "panel") {
        // Plan 05e: the round panel's agentId is `round-panel-<seat>-<actionId>`.
        const round = agentId.match(/^round-panel-(\d+)-/);
        if (round) {
          const seat = Number(round[1]);
          if (!roundPanelScriptPaths.has(agentId)) {
            const script = opts.roundPanelScriptFor ? opts.roundPanelScriptFor(seat, conductor.state) : defaultRoundPanelScript(conductor.state);
            roundPanelScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
          }
          return { FAKE_PI_SCRIPT: roundPanelScriptPaths.get(agentId)! };
        }
        // Plan 04b: one script per dispatched seat (the agentId is
        // `panel-<blockerId>-<seat>-<actionId>`); a retry has a fresh
        // actionId, so a test can give the retry different behaviour.
        // Blocker message ids are `B-<n>`; anchoring on that keeps the
        // blocker id out of the greedy match (the actionId's own `<kind>` has
        // dashes and digits too).
        const m = agentId.match(/^panel-(B-\d+)-(\d+)-/);
        const blockerId = m?.[1] ?? "B-1";
        const seat = Number(m?.[2] ?? "1");
        if (!panelScriptPaths.has(agentId)) {
          const script = opts.panelScriptFor
            ? opts.panelScriptFor(blockerId, seat, conductor.state)
            : defaultPanelScript(blockerId, seat);
          panelScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
        }
        return { FAKE_PI_SCRIPT: panelScriptPaths.get(agentId)! };
      }
      const reviewer = (agentId.match(/^reviewer-([MAB])-/)?.[1] ?? "M") as Reviewer;
      if (!reviewerScriptPaths.has(agentId) && opts.reviewerScriptFor) {
        const script = opts.reviewerScriptFor(reviewer, conductor.state);
        reviewerScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
      }
      const p = reviewerScriptPaths.get(agentId);
      return p ? { FAKE_PI_SCRIPT: p, ...(opts.extraReviewerEnv?.(reviewer) ?? {}) } : undefined;
    },
  });

  return { repo, runRoot, scriptsDir, plan, runDir, conductor };
}

/** Plan 06c: the item ids the finding-pass evaluator owes a check for — every
 * outcome that is not met/fits, plus every unanimous thin met/fits. Shared
 * with the conductor's `#owedItemCheckIds` so the harness's default evaluator
 * answers exactly what is owed. */
function owedItemIds(state: State): string[] {
  const outcomes = phaseItemOutcomes(state.phase);
  const ids = new Set(outcomes.filter((o) => o.outcome !== "met" && o.outcome !== "fits").map((o) => o.item.id));
  // Plan 06c (A4/R9): a unanimous thin met/fits is an owed item check too.
  for (const o of thinMetItems(outcomes, workerAnchorsOf(state.phase.coverage))) ids.add(o.item.id);
  return [...ids];
}

export function readEvents(runDir: string): LogRecord[] {
  return readLog(runPaths(runDir).events).records;
}

export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/** Dumps the tail of `<runDir>/events.jsonl` plus each agent's last few raw
 * RPC stream lines (`<runDir>/stream/*.jsonl`) to stderr — for a
 * `waitFor` timeout on a real (non-fake) reviewer flow, where the failure
 * otherwise gives no clue which side (conductor vs. the scripted agent)
 * stopped responding. */
function dumpDebugState(runDir: string): void {
  try {
    const eventsPath = runPaths(runDir).events;
    if (fs.existsSync(eventsPath)) {
      const tail = fs.readFileSync(eventsPath, "utf8").split("\n").filter(Boolean).slice(-30);
      console.error(`--- waitFor timeout: tail of ${eventsPath} ---`);
      for (const line of tail) console.error(line);
    }
    const streamDir = runPaths(runDir).stream;
    if (fs.existsSync(streamDir)) {
      for (const file of fs.readdirSync(streamDir)) {
        const lines = fs.readFileSync(path.join(streamDir, file), "utf8").split("\n").filter(Boolean);
        console.error(`--- waitFor timeout: last events of ${file} (${lines.length} total) ---`);
        for (const line of lines.slice(-8)) console.error(line);
      }
    }
  } catch (err) {
    console.error(`waitFor timeout debug dump failed: ${String((err as Error)?.message ?? err)}`);
  }
}

/** Plan 2d: the minimum budget every `waitFor` gets. `node --test` runs
 * test files in parallel, so under a full `make check` the host can be
 * several times slower than a single-file run; a correct run whose
 * transition takes 40 s instead of 3 s must not be reported as a failure
 * just because the host was busy. A genuinely stuck run still fails, just
 * after at least this many milliseconds. 150 s (up from 90 s): measured under
 * the phase's own `make check` (4-way file concurrency, the whole suite), a
 * stage that takes ~15 s alone can exceed 90 s — the base's notify test ran
 * 34 s and then timed out past 90 s on a loaded candidate. */
export const WAIT_FOR_FLOOR_MS = 150_000;

/** Polls `check()` until it returns true or its (load-tolerant) budget
 * elapses. `debugRunDir` (work packet 2a addition), if given, is dumped via
 * `dumpDebugState` on timeout — see its own doc comment. `timeoutMs` is a
 * floor; see `WAIT_FOR_FLOOR_MS`. */
export async function waitFor(check: () => boolean, timeoutMs = 15_000, intervalMs = 50, debugRunDir?: string): Promise<void> {
  const budget = Math.max(timeoutMs, WAIT_FOR_FLOOR_MS);
  const start = Date.now();
  while (!check()) {
    if (Date.now() - start > budget) {
      if (debugRunDir) dumpDebugState(debugRunDir);
      throw new Error(`waitFor: timed out after ${budget}ms`);
    }
    await sleep(intervalMs);
  }
}
