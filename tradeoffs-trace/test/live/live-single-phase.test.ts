// Live smoke test for the phase-1 conductor (design/plan "mandatory recorded
// evidence", phase-1 work-packet 1c item 3): a REAL `pi` worker (provider
// `vercel-ai-gateway`, model `deepseek/deepseek-v4.1-flash`) driven by this
// packet's own `Conductor`, with fake-pi stub reviewers standing in for M/A/B
// (phase 1 has no real discovery/correction loop for a reviewer to exercise
// yet — see the README's phase-1b section), in a disposable
// `/tmp/tt-live-*` git repo holding a tiny two-file Node project. Runs ONLY
// with TT_LIVE=1 (`make live` sets it) — otherwise it prints why it is
// skipped and does nothing else, exactly like test/live/live-submission.test.ts.
//
// The single phase: add a `subtract` function to sum.js with a node:test
// test for it; CHECKS is `node --test`. This exercises the real thing
// crash-suite.test.ts's fake-pi worker never does — an actual model reading
// a real prompt, editing real files with the real `edit`/`write` tools, and
// deciding for itself when to call submit_phase — through the *same*
// Conductor code path (worktree, freeze, checks, probe, publish) the crash
// suite drives with a scripted stand-in.
//
// Mixing a real worker with fake reviewers in one Conductor needs
// `ConductorOptions.piCommandFor`/`piArgsPrefixFor`/`providerModelFor`
// (added to src/conductor.ts for exactly this test) rather than `tt start`,
// which has only one flat `piCommand` for every role.
//
// Bounded at 15 minutes total; on timeout this fails loudly with the run's
// own log tail rather than reporting a false success (see `runWithBound`).
// Writes one evidence record plus a relativized copy of the run's
// events.jsonl under test/live/records/phase-1/.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import {
  Conductor,
  contractVersionFor,
  createRun,
  runPaths,
  type Deadlines,
  type RunPlanFile,
} from "../../src/conductor.ts";
import { PI_VERSION, ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer } from "../../src/core/types.ts";
import { readLog, type LogRecord } from "../../src/effects/log.ts";

const PROVIDER = "vercel-ai-gateway";
const MODEL = "deepseek/deepseek-v4.1-flash";
const RUN_BOUND_MS = 15 * 60_000;

if (process.env.TT_LIVE !== "1") {
  test("live-single-phase (skipped)", (t) => {
    t.skip("TT_LIVE is not set to '1' — live smoke tests require real Pi, real model credentials and network access; run `make live` to opt in.");
  });
} else {
  runLiveSingePhase();
}

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

/** A tiny, real Node project: `sum.js` exports `sum`; `test.js` is a
 * node:test test for it. The worker's job is to add `subtract` (and its own
 * test) alongside it — CHECKS (`node --test`) is what actually verifies
 * that, not this test asserting file contents directly. */
function makeLiveRepo(): { dir: string; head: string } {
  const dir = shortTmp("tt-live-repo");
  git(["init", "-q", "-b", "main"], dir);
  fs.writeFileSync(
    path.join(dir, "sum.js"),
    ["function sum(a, b) {", "  return a + b;", "}", "", "module.exports = { sum };", ""].join("\n"),
  );
  fs.writeFileSync(
    path.join(dir, "test.js"),
    [
      "const test = require('node:test');",
      "const assert = require('node:assert/strict');",
      "const { sum } = require('./sum.js');",
      "",
      "test('sum adds two numbers', () => {",
      "  assert.equal(sum(2, 3), 5);",
      "});",
      "",
    ].join("\n"),
  );
  git(["add", "-A"], dir);
  git(["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "base: sum.js + test.js"], dir);
  return { dir, head: git(["rev-parse", "HEAD"], dir) };
}

function makePlan(repoDir: string): RunPlanFile {
  return {
    title: "live-single-phase",
    repo: repoDir,
    integrationBranch: "main",
    checks: ["node --test"],
    phases: [
      {
        id: "p1",
        goal:
          "Add a subtract(a, b) function to sum.js that returns a - b, exported the same way sum is. " +
          "Add a node:test test for it in test.js, following the existing test's style.",
        acceptance: [
          "sum.js exports a subtract(a, b) function that returns a - b",
          "test.js has a passing node:test test for subtract",
          "node --test passes",
        ],
        checks: ["node --test"],
        boundaries: ["Do not modify package.json or add any dependency — this project has none."],
        reserved: [],
      },
    ],
  };
}

function writeReviewerScript(scriptsDir: string, plan: RunPlanFile): string {
  const file = path.join(scriptsDir, "reviewer.json");
  fs.writeFileSync(
    file,
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
  return file;
}

type TerminalOutcome = "DONE" | "BLOCKED" | "AWAITING_OWNER" | "RUN_PAUSED_BUDGET";

/** Races `conductor.start()` (via a `state`-polling loop, since `Conductor`
 * has no "wait until terminal" promise of its own) against a hard wall
 * clock. Returns as soon as the run reaches ANY terminal-for-a-test state —
 * `DONE`, `BLOCKED`, `AWAITING_OWNER` (a phase state — the conductor keeps
 * listening there for an owner command that phase 1 never sends, so it is
 * terminal for this test even though the conductor itself does not
 * auto-stop there) or `RUN_PAUSED_BUDGET` (a run state) — never waiting out
 * the full bound once the run has already stopped making progress. Only a
 * genuine hang (nothing reaching any of those) burns the whole bound; on
 * that timeout it stops the conductor and rejects with the run's own log
 * tail — the brief's "fail with the log if exceeded, never fake success". */
async function waitForTerminal(conductor: Conductor, boundMs: number, eventsPath: string): Promise<TerminalOutcome> {
  const start = Date.now();
  for (;;) {
    const phase = conductor.state.phase.phase;
    if (phase === "DONE" || phase === "BLOCKED" || phase === "AWAITING_OWNER") return phase;
    if (conductor.state.run === "RUN_PAUSED_BUDGET") return "RUN_PAUSED_BUDGET";
    if (Date.now() - start > boundMs) {
      await conductor.stop().catch(() => undefined);
      const tail = fs.existsSync(eventsPath) ? fs.readFileSync(eventsPath, "utf8").split("\n").slice(-40).join("\n") : "(no events.jsonl)";
      throw new Error(
        `live-single-phase exceeded its ${boundMs}ms bound without reaching a terminal state (last phase: ${phase}). ` +
          `Tail of events.jsonl:\n${tail}`,
      );
    }
    await new Promise((resolve) => setTimeout(resolve, 500));
  }
}

interface StreamSummary {
  toolCallCounts: Record<string, number>;
  lastUsage?: unknown;
}

/** Reads the worker's own raw RPC stream file (`<run>/stream/worker-*.jsonl`
 * — every event Pi's RPC mode emits, written by `PiAgent`'s `streamFile`)
 * for evidence `Conductor` itself does not expose: per-tool call counts and
 * the last reported `usage` payload (tokens/cost — real Pi's own shape,
 * recorded as-is rather than reshaped, since design leaves it
 * provider-dependent). */
function summarizeWorkerStream(runDir: string): StreamSummary {
  const streamDir = runPaths(runDir).stream;
  const summary: StreamSummary = { toolCallCounts: {} };
  if (!fs.existsSync(streamDir)) return summary;
  const workerFile = fs.readdirSync(streamDir).find((f) => f.startsWith("worker-"));
  if (!workerFile) return summary;
  const lines = fs.readFileSync(path.join(streamDir, workerFile), "utf8").split("\n").filter(Boolean);
  for (const line of lines) {
    let record: Record<string, unknown>;
    try {
      record = JSON.parse(line);
    } catch {
      continue;
    }
    // pi-rpc.ts's `#onMessage` writes each line as `{agentId, ts, event}` —
    // the actual RPC event (design §9.4/§9.2) is nested under `.event`, not
    // at this line's own top level.
    const event = record.event as Record<string, unknown> | undefined;
    if (!event) continue;
    if (event.type === "tool_execution_start" && typeof event.toolName === "string") {
      summary.toolCallCounts[event.toolName] = (summary.toolCallCounts[event.toolName] ?? 0) + 1;
    }
    if (event.type === "message_update" && "usage" in event) {
      summary.lastUsage = event.usage;
    }
  }
  return summary;
}

function relativizeHomePaths(text: string): string {
  const home = os.homedir();
  return home ? text.split(home).join("~") : text;
}

function findLastEvent<T extends { type: string }>(records: LogRecord[], type: string): T | undefined {
  for (let i = records.length - 1; i >= 0; i--) {
    const r = records[i];
    if (r.kind === "event" && (r.event as { type: string }).type === type) return r.event as T;
  }
  return undefined;
}

function runLiveSingePhase(): void {
  test(
    "live-single-phase: a real Pi worker + fake-pi reviewers complete one phase end to end",
    { timeout: RUN_BOUND_MS + 60_000 },
    async () => {
      const startedAt = Date.now();
      const repo = makeLiveRepo();
      const root = shortTmp("tt-live-run");
      const scriptsDir = shortTmp("tt-live-scripts");
      const plan = makePlan(repo.dir);
      const reviewerScript = writeReviewerScript(scriptsDir, plan);
      const runDir = createRun(root, plan);
      const eventsPath = runPaths(runDir).events;

      // Grace periods matter only for fake-pi (it never exits on its own —
      // see crash-suite.test.ts's own header comment); a real `pi` worker
      // is left at production defaults. Everything else stays at
      // DEFAULT_DEADLINES — a real model doing a two-file edit comfortably
      // fits inside them, and this is a smoke test of the real path, not a
      // speed test.
      const deadlines: Partial<Deadlines> = {};

      const conductor = new Conductor({
        runDir,
        plan,
        deadlines,
        piCommandFor: (role) => (role === "reviewer" ? process.execPath : undefined),
        piArgsPrefixFor: (role) => (role === "reviewer" ? [FAKE_PI_PATH] : []),
        providerModelFor: (role) => (role === "worker" ? { provider: PROVIDER, model: MODEL } : undefined),
        piEnvFor: (role) => (role === "reviewer" ? { FAKE_PI_SCRIPT: reviewerScript } : undefined),
        // Phase 1c's own scope: a real worker with fake-pi stub reviewers,
        // pre-dating work packet 2a's real two-turn review protocol — see
        // that packet's live-review test for real M/A/B reviewers instead.
        stubReviews: true,
      });

      const seenPgids = new Set<number>();
      const pollPgids = setInterval(() => {
        for (const a of conductor.agentPgids) seenPgids.add(a.pgid);
      }, 500);

      let outcome: TerminalOutcome | "timed-out" | "error" = "error";
      let notes: string | undefined;

      try {
        await conductor.start();
        outcome = await waitForTerminal(conductor, RUN_BOUND_MS, eventsPath);
      } catch (err) {
        outcome = "timed-out";
        notes = String((err as Error)?.message ?? err);
      } finally {
        clearInterval(pollPgids);
      }

      // Capture the open owner requests BEFORE stop() — they are read from
      // live in-memory state, not from the log — while it still reflects
      // the terminal state waitForTerminal just observed.
      const openOwnerRequests = outcome === "AWAITING_OWNER" ? conductor.state.phase.ownerRequests.filter((r) => r.status === "open") : [];
      await conductor.stop().catch(() => undefined);
      const terminalPhase = outcome;

      const { records } = readLog(eventsPath);
      const eventTypes = records.filter((r) => r.kind === "event").map((r) => (r.event as { type: string }).type);
      const freezeCompleted = findLastEvent<{ candidateSha: string; tainted?: boolean }>(records, "FREEZE_COMPLETED");
      const publishCompleted = findLastEvent<{ newHead: string }>(records, "PUBLISH_COMPLETED");
      const checksPassedEvt = eventTypes.includes("CHECKS_PASSED");
      const checksFailedEvt = eventTypes.includes("CHECKS_FAILED");
      const streamSummary = summarizeWorkerStream(runDir);

      // No orphan process: every pgid this run ever reported must be dead
      // now that the conductor has stopped.
      const survivors = [...seenPgids].filter((pgid) => {
        try {
          process.kill(-pgid, 0);
          return true;
        } catch {
          return false;
        }
      });

      const record = {
        piVersion: PI_VERSION,
        provider: PROVIDER,
        model: MODEL,
        runId: path.basename(runDir),
        eventTypes,
        candidateSha: freezeCompleted?.candidateSha,
        publishedI: publishCompleted?.newHead,
        checks: { passed: checksPassedEvt, failed: checksFailedEvt },
        workerToolCallCounts: streamSummary.toolCallCounts,
        lastWorkerUsage: streamSummary.lastUsage,
        durationMs: Date.now() - startedAt,
        outcome,
        terminalPhase,
        openOwnerRequests,
        survivorPgids: survivors,
        notes,
      };

      const recordsDir = new URL("./records/phase-1/", import.meta.url).pathname;
      fs.mkdirSync(recordsDir, { recursive: true });
      const stamp = new Date().toISOString().replace(/[:.]/g, "-");
      fs.writeFileSync(path.join(recordsDir, `${stamp}-live-single-phase.json`), JSON.stringify(record, null, 2));
      if (fs.existsSync(eventsPath)) {
        const relativized = relativizeHomePaths(fs.readFileSync(eventsPath, "utf8"));
        fs.writeFileSync(path.join(recordsDir, `${stamp}-live-single-phase.events.jsonl`), relativized);
      }

      try {
        execFileSync("chmod", ["-R", "u+w", root]);
      } catch {
        // best effort
      }
      fs.rmSync(root, { recursive: true, force: true });
      fs.rmSync(repo.dir, { recursive: true, force: true });
      fs.rmSync(scriptsDir, { recursive: true, force: true });

      // The assertions the brief calls out, in order:
      assert.equal(
        outcome,
        "DONE",
        `expected the run to reach DONE; got outcome=${outcome} terminalPhase=${terminalPhase} notes=${notes}. ` +
          `${outcome === "AWAITING_OWNER" ? `open owner requests: ${JSON.stringify(openOwnerRequests)}. ` : ""}` +
          `events: ${eventTypes.join(",")}`,
      );
      // Tool-set assertion held before ever prompting: a mismatch would
      // have produced LAUNCH_FAILED -> BLOCKED instead of ever reaching
      // SUBMIT_PHASE, so DONE (asserted above) already proves this: this
      // is a direct, redundant check of the same fact.
      assert.ok(!eventTypes.includes("LAUNCH_FAILED"), "no LAUNCH_FAILED (tool-set mismatch) should occur");
      assert.ok(eventTypes.includes("SUBMIT_PHASE"), "submit_phase should have been accepted");
      assert.ok(freezeCompleted, "the worker's submission should have been frozen into a candidate");
      assert.ok(checksPassedEvt, `node --test should have passed on the frozen candidate; events: ${eventTypes.join(",")}`);
      assert.ok(eventTypes.includes("PROBE_PASSED"), "the integration probe should have passed");
      assert.equal(eventTypes.filter((t) => t === "REVIEW_SUBMITTED").length >= 3, true, "all three stub reviews should have been submitted");
      assert.ok(eventTypes.includes("ACCEPTED"), "the candidate should have been accepted");
      assert.ok(publishCompleted, "publish (compare-and-swap) should have completed");
      assert.equal(survivors.length, 0, `no leftover pi process should survive the run; still alive: ${survivors.join(",")}`);
    },
  );
}
