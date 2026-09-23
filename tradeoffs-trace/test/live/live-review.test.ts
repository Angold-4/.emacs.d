// Live smoke test for work packet 2a's real two-turn review protocol: a
// REAL `pi` worker AND real `pi` M/A/B reviewers (all
// `vercel-ai-gateway`/`deepseek/deepseek-v4.1-flash` by default), in a
// disposable `/tmp/tt-live-*` git repo (the same tiny two-file Node
// project live-single-phase.test.ts uses). Runs ONLY with TT_LIVE=1
// (`make live` sets it) — otherwise it prints why it is skipped.
//
// The task is chosen to invite at least one real decision: "add input
// validation to sum(): decide how to handle non-numbers" — genuinely
// underspecified, so a real worker must choose (and disclose) a behavior,
// and real reviewers have something to discover/vote on.
//
// Bounded at 20 minutes; on timeout (or any terminal state) this stops
// immediately and reports what happened rather than waiting out the full
// bound or faking success. Writes one evidence record plus a relativized
// copy of events.jsonl under test/live/records/phase-2/.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import { Conductor, createRun, runPaths, type Deadlines, type RunPlanFile } from "../../src/conductor.ts";
import { PI_VERSION } from "../../src/core/roles.ts";
import type { Decision } from "../../src/core/types.ts";
import { readLog, type LogRecord } from "../../src/effects/log.ts";

const PROVIDER = process.env.TT_LIVE_PROVIDER ?? "vercel-ai-gateway";
const MODEL = process.env.TT_LIVE_MODEL ?? "deepseek/deepseek-v4.1-flash";
const RUN_BOUND_MS = 20 * 60_000;

if (process.env.TT_LIVE !== "1") {
  test("live-review (skipped)", (t) => {
    t.skip("TT_LIVE is not set to '1' — live smoke tests require real Pi, real model credentials and network access; run `make live` to opt in.");
  });
} else {
  runLiveReview();
}

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

function git(args: string[], cwd: string): string {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

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
    title: "live-review",
    repo: repoDir,
    integrationBranch: "main",
    checks: ["node --test"],
    phases: [
      {
        id: "p1",
        goal:
          "Add input validation to sum(a, b) in sum.js: decide for yourself how it should handle non-numeric " +
          "arguments (throw, coerce, return NaN, or something else), implement it, and add a node:test test " +
          "for the behavior you chose, following the existing test's style. Disclose this as a decision.",
        acceptance: [
          "sum.js's sum(a, b) has defined, deliberate behavior for non-numeric arguments",
          "test.js has a passing node:test test for that behavior",
          "node --test passes",
        ],
        checks: ["node --test"],
        boundaries: [],
        reserved: [],
      },
    ],
  };
}

type TerminalOutcome = "DONE" | "BLOCKED" | "AWAITING_OWNER" | "RUN_PAUSED_BUDGET";

async function waitForTerminal(conductor: Conductor, boundMs: number, eventsPath: string): Promise<TerminalOutcome> {
  const start = Date.now();
  for (;;) {
    const phase = conductor.state.phase.phase;
    if (phase === "DONE" || phase === "BLOCKED" || phase === "AWAITING_OWNER") return phase;
    if (conductor.state.run === "RUN_PAUSED_BUDGET") return "RUN_PAUSED_BUDGET";
    if (Date.now() - start > boundMs) {
      await conductor.stop().catch(() => undefined);
      const tail = fs.existsSync(eventsPath) ? fs.readFileSync(eventsPath, "utf8").split("\n").slice(-60).join("\n") : "(no events.jsonl)";
      throw new Error(
        `live-review exceeded its ${boundMs}ms bound without reaching a terminal state (last phase: ${phase}). ` +
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

/** Per-agent (worker + each reviewer dispatch) raw RPC stream summary —
 * same technique as live-single-phase.test.ts's `summarizeWorkerStream`,
 * generalized to every agent id whose stream file exists. */
function summarizeStream(runDir: string, agentIdPrefix: string): StreamSummary {
  const streamDir = runPaths(runDir).stream;
  const summary: StreamSummary = { toolCallCounts: {} };
  if (!fs.existsSync(streamDir)) return summary;
  const files = fs.readdirSync(streamDir).filter((f) => f.startsWith(agentIdPrefix));
  for (const file of files) {
    const lines = fs.readFileSync(path.join(streamDir, file), "utf8").split("\n").filter(Boolean);
    for (const line of lines) {
      let record: Record<string, unknown>;
      try {
        record = JSON.parse(line);
      } catch {
        continue;
      }
      const event = record.event as Record<string, unknown> | undefined;
      if (!event) continue;
      if (event.type === "tool_execution_start" && typeof event.toolName === "string") {
        summary.toolCallCounts[event.toolName] = (summary.toolCallCounts[event.toolName] ?? 0) + 1;
      }
      if (event.type === "message_update" && "usage" in event) {
        summary.lastUsage = event.usage;
      }
    }
  }
  return summary;
}

function relativizeHomePaths(text: string): string {
  const home = os.homedir();
  return home ? text.split(home).join("~") : text;
}

function eventsOf(records: LogRecord[]): { type: string; [k: string]: unknown }[] {
  return records.filter((r) => r.kind === "event").map((r) => r.event as { type: string; [k: string]: unknown });
}

function plainLanguageNonEmpty(d: Decision): boolean {
  if (!d.choice?.trim() || !d.whyItMatters?.trim()) return false;
  if (!Array.isArray(d.alternatives) || d.alternatives.length === 0) return false;
  if (d.alternatives.some((a) => !a.option?.trim() || !a.consequence?.trim())) return false;
  if (!d.recommendation?.choice?.trim() || !d.recommendation?.reason?.trim()) return false;
  return true;
}

function runLiveReview(): void {
  test(
    "live-review: a real Pi worker + real Pi M/A/B reviewers run the real two-turn review protocol end to end",
    { timeout: RUN_BOUND_MS + 60_000 },
    async () => {
      const startedAt = Date.now();
      const repo = makeLiveRepo();
      const root = shortTmp("tt-live-run");
      const plan = makePlan(repo.dir);
      const runDir = createRun(root, plan);
      const eventsPath = runPaths(runDir).events;

      const deadlines: Partial<Deadlines> = {};

      const conductor = new Conductor({
        runDir,
        plan,
        deadlines,
        providerModelFor: () => ({ provider: PROVIDER, model: MODEL }),
        // stubReviews defaults to false — this is exactly the point of
        // this test: real M/A/B, real two-turn discovery/review.
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

      const openOwnerRequests = outcome === "AWAITING_OWNER" ? conductor.state.phase.ownerRequests.filter((r) => r.status === "open") : [];
      const finalDecisions = conductor.state.phase.decisions;
      const finalFindings = conductor.state.phase.findings;
      await conductor.stop().catch(() => undefined);
      const terminalPhase = outcome;

      const { records } = readLog(eventsPath);
      const types = eventsOf(records).map((e) => e.type);
      const discoverySubmittedBy = records
        .filter((r) => r.kind === "discovery_submitted")
        .map((r) => (r.event as unknown as { reviewer: string }).reviewer);
      const reviewSubmittedBy = eventsOf(records)
        .filter((e) => e.type === "REVIEW_SUBMITTED")
        .map((e) => (e as unknown as { review: { reviewer: string } }).review.reviewer);

      const workerUsage = summarizeStream(runDir, "worker-");
      const reviewerUsage: Record<string, StreamSummary> = {
        M: summarizeStream(runDir, "reviewer-M-"),
        A: summarizeStream(runDir, "reviewer-A-"),
        B: summarizeStream(runDir, "reviewer-B-"),
      };
      const tokenTotals = conductor.agentTokenTotals;

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
        eventTypes: types,
        decisions: finalDecisions,
        findings: finalFindings,
        ballots: (() => {
          const ballots: unknown[] = [];
          for (const e of eventsOf(records)) if (e.type === "BALLOT_CAST") ballots.push(e.ballot);
          return ballots;
        })(),
        discoverySubmittedBy,
        reviewSubmittedBy,
        outcome,
        terminalPhase,
        openOwnerRequests,
        tokenUsage: { perAgent: tokenTotals, worker: workerUsage, reviewers: reviewerUsage },
        durationMs: Date.now() - startedAt,
        survivorPgids: survivors,
        notes,
      };

      const recordsDir = new URL("./records/phase-2/", import.meta.url).pathname;
      fs.mkdirSync(recordsDir, { recursive: true });
      const stamp = new Date().toISOString().replace(/[:.]/g, "-");
      fs.writeFileSync(path.join(recordsDir, `${stamp}-live-review.json`), JSON.stringify(record, null, 2));
      if (fs.existsSync(eventsPath)) {
        const relativized = relativizeHomePaths(fs.readFileSync(eventsPath, "utf8"));
        fs.writeFileSync(path.join(recordsDir, `${stamp}-live-review.events.jsonl`), relativized);
      }

      try {
        execFileSync("chmod", ["-R", "u+w", root]);
      } catch {
        // best effort
      }
      fs.rmSync(root, { recursive: true, force: true });
      fs.rmSync(repo.dir, { recursive: true, force: true });

      // The brief's assertions, in order. Note: the gate is the decision
      // record and the reviewer protocol having actually run, NOT
      // necessarily DONE — AWAITING_OWNER/BLOCKED are reported (with the
      // reason) rather than failed outright, per the brief.
      assert.ok(
        outcome === "DONE" || outcome === "AWAITING_OWNER" || outcome === "BLOCKED",
        `expected a terminal state; got outcome=${outcome} notes=${notes}. events: ${types.join(",")}`,
      );
      const decisionsWithFullFields = finalDecisions.filter(plainLanguageNonEmpty);
      assert.ok(
        decisionsWithFullFields.length >= 1,
        `expected at least one decision with every plain-language field non-empty; got ${JSON.stringify(finalDecisions)}`,
      );
      for (const reviewer of ["M", "A", "B"]) {
        assert.ok(discoverySubmittedBy.includes(reviewer), `${reviewer} should have submitted discovery (turn 1); got ${JSON.stringify(discoverySubmittedBy)}`);
        assert.ok(reviewSubmittedBy.includes(reviewer), `${reviewer} should have submitted review (turn 2); got ${JSON.stringify(reviewSubmittedBy)}`);
      }
      assert.equal(survivors.length, 0, `no leftover pi process should survive the run; still alive: ${survivors.join(",")}`);
    },
  );
}
