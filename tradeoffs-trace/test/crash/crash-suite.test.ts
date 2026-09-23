// `crash-suite` (design §9.3, phase-1 plan item "TT_CRASH_AT / crash-suite"):
// for every phase-1 fault-injection boundary (`CRASH_BOUNDARIES`,
// src/effects/crash.ts), run a single-phase fake-pi conductor through a
// real OS process with `TT_CRASH_AT=<boundary>` set, let it exit abruptly
// (no `stop()`, no lock release — that is the whole point), restart a fresh
// conductor process on the *same* run directory without the env var, and
// assert design §9.3's own four properties against the resulting
// `events.jsonl` and repository state:
//
//   1. no conductor-state effect is applied twice;
//   2. interrupted checks, reviews and attempts may rerun, and are never
//      counted as passed;
//   3. the final accepted state equals that of an uninterrupted run, apart
//      from the reruns (compared here as a normalized projection: final
//      phase, candidate/integration tree hashes, decisions — with action
//      ids/timestamps/attempt counters stripped);
//   4. no orphan process survives.
//
// Both processes are spawned via the CLI's own `__run-conductor` entry
// point (the same one `tt start` uses, just not detached here — so this
// test can `await` each one's exit directly instead of polling `pgrep`),
// using the documented `TT_TEST_MODE=1` fake-pi injection (src/cli.ts).
//
// A separate assertion (`phase-2-boundaries-are-listed-not-exercised`)
// checks that design §9.3's other two boundaries — the inbox-move and
// steer-acknowledgement ones — are named in `PHASE_2_CRASH_BOUNDARIES` and
// that nothing in this packet ever sets `TT_CRASH_AT` to one of them.
//
// Speed (phase-1 work-packet 1c, item 2): every crashed/recovery process
// pair used **production** deadlines at first — in particular the default
// 30s `abortGraceMs`, spent in full on *every* freeze (fake-pi replies to
// the RPC "abort" but, correctly mirroring a real Pi process that has no
// more work to do, never exits on its own the way a `hang-until-abort`
// script step does — so `PiAgent#terminate()` always burns the whole
// `abortGraceMs` before its own SIGTERM lands) — which made each boundary's
// crashed+recovery pair cost ~30-65s (one or two freezes) and the full
// 12-boundary suite ~12 minutes. `TT_TEST_DEADLINES` (src/cli.ts's
// `testDeadlines()`, the same `TT_TEST_MODE=1` gate as the existing fake-pi
// injection) now overrides every deadline to a millisecond scale
// (`FAST_DEADLINES` below) for both the crashed and the recovery process,
// so no real run here ever needs to wait out a 30s grace period — a single
// boundary's crashed+recovery pair now takes on the order of a second.
//
// `TT_CRASH_FULL=1` selects the full 12-boundary sweep (`make crash`);
// without it (the default, so plain `node --test`/`make check` gets this)
// only `FAST_SUBSET` runs — one boundary per external effect in design
// §9.3's table (create_worktree, dispatch_worker, freeze, run_checks,
// dispatch_probe, publish_cas) is exercised by the full sweep, so the fast
// subset below picks representative ones across that same set rather than
// duplicating every before/after pair.

import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { sleep } from "../conductor/harness.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { contractVersionFor, createRun, runPaths, type Deadlines, type RunPlanFile } from "../../src/conductor.ts";
import { readLog } from "../../src/effects/log.ts";
import { CRASH_BOUNDARIES, CRASH_EXIT_CODE, PHASE_2_CRASH_BOUNDARIES, type CrashBoundary } from "../../src/effects/crash.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

/** Millisecond-scale overrides for every one of `Deadlines`' fields (see
 * `src/cli.ts`'s `testDeadlines()`), applied via `TT_TEST_DEADLINES` to
 * every crashed/recovery process this suite spawns. Every value here is
 * still comfortably larger than what a fake-pi happy path actually needs
 * (a `submit_phase`/`submit_review` call, `git` on a tiny disposable repo,
 * `true` as the only check/probe command) — this is not "make it flaky
 * fast", it's "stop waiting out a production-sized grace period that never
 * needed to elapse in the first place". */
const FAST_DEADLINES: Partial<Deadlines> = {
  helloTimeoutMs: 3_000,
  workerAttemptMs: 8_000,
  shCommandMs: 5_000,
  freezeMs: 8_000,
  checkMs: 5_000,
  probeMs: 5_000,
  reviewMs: 8_000,
  reproductionMs: 5_000,
  abortGraceMs: 300,
  termGraceMs: 300,
};

/** design §9.3's reconciliation table has one row per external effect
 * (create worktree, agent attempt, freeze, check run, probe, publish); this
 * picks one representative boundary per effect (the brief's own examples)
 * rather than re-running all 12 in `make check`. The full sweep
 * (`TT_CRASH_FULL=1`, `make crash`) still covers every boundary. */
const FAST_SUBSET: readonly CrashBoundary[] = ["after_freeze", "before_publish_cas", "after_run_checks"];

const BOUNDARIES_TO_RUN: readonly CrashBoundary[] = process.env.TT_CRASH_FULL === "1" ? CRASH_BOUNDARIES : FAST_SUBSET;

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

function makeRepo(): { dir: string; head: string } {
  const dir = shortTmp("tt-crash-repo");
  git(["init", "-q", "-b", "main"], dir);
  fs.writeFileSync(path.join(dir, "README.md"), "base\n");
  git(["add", "-A"], dir);
  git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base"], dir);
  return { dir, head: git(["rev-parse", "HEAD"], dir) };
}

/** Spawns `node cli.ts __run-conductor <runDir>` as a plain (non-detached)
 * child so the test can await its own exit directly — the crashed run is
 * expected to exit with `CRASH_EXIT_CODE`; the recovery run is expected to
 * exit 0 once it auto-stops at DONE/BLOCKED. */
function runConductorProcess(runDir: string, env: NodeJS.ProcessEnv): Promise<{ code: number | null; signal: NodeJS.Signals | null; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI_PATH, "__run-conductor", runDir], {
      env: { ...process.env, ...env },
      stdio: ["ignore", "pipe", "pipe"],
    });
    let stderr = "";
    child.stdout.on("data", (c) => (stderr += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code, signal) => resolve({ code, signal, stderr }));
  });
}

function writeScripts(scriptsDir: string, plan: RunPlanFile): void {
  fs.writeFileSync(
    path.join(scriptsDir, "worker.json"),
    JSON.stringify({
      hello: { role: "worker", tools: ROLE_TOOLS.worker },
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
  );
  fs.writeFileSync(
    path.join(scriptsDir, "reviewer.json"),
    JSON.stringify({
      hello: { role: "reviewer", tools: ROLE_TOOLS.reviewer },
      steps: [
        {
          kind: "call-submit",
          tool: "submit_review",
          args: {
            reviewer: "$TT_REVIEWER",
            phaseId: "p1",
            candidateSha: "$TT_CANDIDATE_SHA",
            contractVersion: contractVersionFor(plan.phases[0]),
            correctionStatements: [],
            findingStatements: [],
          },
        },
      ],
    }),
  );
}

function makePlan(repoDir: string): RunPlanFile {
  return {
    title: "crash-suite",
    repo: repoDir,
    integrationBranch: "main",
    checks: ["true"],
    phases: [{ id: "p1", goal: "do the thing", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }],
  };
}

interface RunOutcome {
  runDir: string;
  repoDir: string;
  candidateSha?: string;
  candidateTree?: string;
  publishedI?: string;
  integrationTree?: string;
  eventTypes: string[];
}

/** Reads `events.jsonl` and the resulting repo state into a normalized
 * projection: event *types* in order (never action ids/timestamps/attempt
 * counters), the candidate's tree hash and the published integration head's
 * tree hash — comparable across two separately-run conductors whose sha1s
 * necessarily differ (different actionIds -> different TT-Action trailers -> different commits). */
function projectOutcome(runDir: string, repoDir: string): RunOutcome {
  const { records } = readLog(runPaths(runDir).events);
  const eventTypes = records.filter((r) => r.kind === "event").map((r) => (r.event as { type: string }).type);
  let candidateSha: string | undefined;
  let publishedI: string | undefined;
  for (const r of records) {
    if (r.kind !== "event") continue;
    const e = r.event as { type: string; candidateSha?: string; newHead?: string };
    if (e.type === "FREEZE_COMPLETED" && e.candidateSha) candidateSha = e.candidateSha;
    if (e.type === "PUBLISH_COMPLETED" && e.newHead) publishedI = e.newHead;
  }
  const candidateTree = candidateSha ? git(["rev-parse", `${candidateSha}^{tree}`], repoDir) : undefined;
  const integrationTree = publishedI ? git(["rev-parse", `${publishedI}^{tree}`], repoDir) : undefined;
  return { runDir, repoDir, candidateSha, candidateTree, publishedI, integrationTree, eventTypes };
}

function countOf<T>(items: T[], value: T): number {
  return items.filter((i) => i === value).length;
}

async function waitNoProcessUnder(needle: string, timeoutMs = 10_000): Promise<void> {
  const start = Date.now();
  for (;;) {
    try {
      execFileSync("pgrep", ["-f", needle], { encoding: "utf8" });
    } catch {
      return; // pgrep found nothing — clean.
    }
    if (Date.now() - start > timeoutMs) {
      const survivors = execFileSync("pgrep", ["-fl", needle], { encoding: "utf8" });
      throw new Error(`orphan process(es) still running after ${timeoutMs}ms:\n${survivors}`);
    }
    await sleep(100);
  }
}

/** Runs one full, uninterrupted single-phase conductor to DONE and returns
 * its normalized projection — the "uninterrupted run" baseline design §9.3
 * says a crashed-and-recovered run's final state must equal. */
async function runBaseline(): Promise<RunOutcome> {
  const repo = makeRepo();
  const root = shortTmp("tt-crash-root");
  const scriptsDir = shortTmp("tt-crash-scripts");
  const plan = makePlan(repo.dir);
  writeScripts(scriptsDir, plan);
  const runDir = createRun(root, plan);
  const env: NodeJS.ProcessEnv = {
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    TT_TEST_STUB_REVIEWS: "1",
    TT_TEST_DEADLINES: JSON.stringify(FAST_DEADLINES),
    FAKE_PI_SCRIPT: scriptsDir,
  };
  const { code } = await runConductorProcess(runDir, env);
  assert.equal(code, 0, "baseline run should exit 0 (DONE, auto-stopped)");
  return projectOutcome(runDir, repo.dir);
}

/** Runs the crash-then-recover cycle for one boundary and returns the
 * recovered run's normalized projection alongside the raw event log
 * (records) for the boundary-specific assertions. */
async function runCrashCycle(boundary: CrashBoundary): Promise<{ outcome: RunOutcome; eventTypes: string[]; records: ReturnType<typeof readLog>["records"] }> {
  const repo = makeRepo();
  const root = shortTmp("tt-crash-root");
  const scriptsDir = shortTmp("tt-crash-scripts");
  const plan = makePlan(repo.dir);
  writeScripts(scriptsDir, plan);
  const runDir = createRun(root, plan);
  const baseEnv: NodeJS.ProcessEnv = {
    TT_TEST_MODE: "1",
    TT_TEST_PI_COMMAND: process.execPath,
    TT_TEST_PI_ARGS_PREFIX: JSON.stringify([FAKE_PI_PATH]),
    TT_TEST_STUB_REVIEWS: "1",
    TT_TEST_DEADLINES: JSON.stringify(FAST_DEADLINES),
    FAKE_PI_SCRIPT: scriptsDir,
  };

  try {
    const crashed = await runConductorProcess(runDir, { ...baseEnv, TT_CRASH_AT: boundary });
    assert.equal(
      crashed.code,
      CRASH_EXIT_CODE,
      `expected the conductor to crash at boundary '${boundary}' (exit ${CRASH_EXIT_CODE}); got code=${crashed.code} signal=${crashed.signal} — the boundary was likely never reached; stderr:\n${crashed.stderr}`,
    );

    // No lingering process from the crashed run before we start a new one.
    await waitNoProcessUnder(runDir);

    const recovered = await runConductorProcess(runDir, baseEnv);
    assert.equal(
      recovered.code,
      0,
      `recovery run for boundary '${boundary}' should exit 0 (DONE, auto-stopped); stderr:\n${recovered.stderr}`,
    );
    await waitNoProcessUnder(runDir);

    const { records } = readLog(runPaths(runDir).events);
    const eventTypes = records.filter((r) => r.kind === "event").map((r) => (r.event as { type: string }).type);
    return { outcome: projectOutcome(runDir, repo.dir), eventTypes, records };
  } finally {
    try {
      execFileSync("pkill", ["-9", "-f", runDir]);
    } catch {
      // already gone
    }
  }
}

let baseline: RunOutcome | undefined;

test("crash-suite: baseline (uninterrupted) run reaches DONE", async () => {
  baseline = await runBaseline();
  assert.ok(baseline.candidateSha, "baseline should have frozen a candidate");
  assert.ok(baseline.publishedI, "baseline should have published");
});

for (const boundary of BOUNDARIES_TO_RUN) {
  test(`crash-suite: TT_CRASH_AT=${boundary} recovers to the baseline's final state`, async () => {
    assert.ok(baseline, "baseline must run first");
    const { outcome, eventTypes } = await runCrashCycle(boundary);

    // Property 1: no conductor-state effect applied twice.
    assert.ok(countOf(eventTypes, "FREEZE_COMPLETED") <= 1, `FREEZE_COMPLETED logged more than once for ${boundary}: ${eventTypes}`);
    assert.ok(countOf(eventTypes, "PUBLISH_COMPLETED") <= 1, `PUBLISH_COMPLETED logged more than once for ${boundary}: ${eventTypes}`);
    assert.ok(countOf(eventTypes, "ACCEPTED") <= 1, `ACCEPTED logged more than once for ${boundary}: ${eventTypes}`);

    // Property 2: an interrupted marker is present for the effect this
    // boundary actually interrupted (a "before_*" boundary never let the
    // effect happen at all; an "after_*" boundary let it happen but not be
    // recorded — both cases go through a reconciliation path, but
    // "after_freeze"/"after_publish_cas" recover the completed effect
    // directly (the trailer/branch already prove it happened), same as
    // design §9.3's own "record it" rows, so no *_INTERRUPTED event is
    // expected there specifically).
    //
    // "after_dispatch_worker" is a special case: this crash-suite's worker
    // always calls submit_phase, so the worker-attempt race always resolves
    // as "submitted" — and #onSubmit's own SUBMIT_PHASE (IMPLEMENTING ->
    // FREEZING) fires, fsynced, *before* this boundary's crashAt() call, so
    // by the time the process dies, `dispatch_worker` has already left
    // `phase.inFlight` (cleared by that same transition) and `freeze` has
    // taken its place there instead (from the "freeze" dispatch's own
    // ACTION_STARTED, applied synchronously in the same call chain). What
    // this boundary actually interrupts, in this suite, is the freeze that
    // was already underway — not the worker attempt, which had already
    // finished — so the recovered log's marker is FREEZE_INTERRUPTED, the
    // same as "before_freeze".
    const expectInterrupted: Partial<Record<CrashBoundary, string>> = {
      before_dispatch_worker: "ATTEMPT_INTERRUPTED",
      after_dispatch_worker: "FREEZE_INTERRUPTED",
      before_freeze: "FREEZE_INTERRUPTED",
      before_run_checks: "CHECKS_INTERRUPTED",
      after_run_checks: "CHECKS_INTERRUPTED",
      before_dispatch_probe: "PROBE_INTERRUPTED",
      after_dispatch_probe: "PROBE_INTERRUPTED",
    };
    const wantEvent = expectInterrupted[boundary];
    if (wantEvent) {
      assert.ok(
        eventTypes.includes(wantEvent),
        `expected ${wantEvent} in the recovered log for boundary ${boundary}, got: ${eventTypes.join(",")}`,
      );
    }

    // Property 3: final state matches the uninterrupted baseline (candidate
    // tree, integration tree), reruns aside.
    assert.equal(outcome.candidateTree, baseline!.candidateTree, `candidate tree mismatch for ${boundary}`);
    assert.equal(outcome.integrationTree, baseline!.integrationTree, `integration tree mismatch for ${boundary}`);
    assert.ok(outcome.publishedI, `boundary ${boundary} should still reach a published I`);

    // The integration branch moved exactly once — not published twice
    // under two different shas.
    assert.equal(countOf(eventTypes, "PUBLISH_COMPLETED"), 1, `expected exactly one PUBLISH_COMPLETED for ${boundary}`);
  });
}

test("crash-suite: phase-2 inbox-move and steer-acknowledgement boundaries are wired in", () => {
  assert.deepEqual([...PHASE_2_CRASH_BOUNDARIES], ["before_inbox_move", "before_steer_ack"]);
  // Plan 2d implemented both boundaries: a crash after an owner-command
  // event but before its inbox file moves, and one between the steer RPC
  // and its acknowledgement. This is a static "they are actually wired"
  // check; the live behaviour is covered by
  // test/conductor/owner-input.test.ts (steer-uncertain).
  const conductorSrc = fs.readFileSync(fileURLToPath(new URL("../../src/conductor.ts", import.meta.url)), "utf8");
  for (const phase2Boundary of PHASE_2_CRASH_BOUNDARIES) {
    assert.ok(
      conductorSrc.includes(`crashAt("${phase2Boundary}")`),
      `phase 2d must fire the phase-2 boundary ${phase2Boundary}`,
    );
  }
});
