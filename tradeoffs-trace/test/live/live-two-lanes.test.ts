// Live smoke test for plan 06g2's two-lane round: a REAL `pi` worker on each
// lane and real `pi` M/A/B reviewers and pick seats, in a disposable
// `/tmp/tt-live-lanes-*` git repo. Runs ONLY with TT_LIVE=1 (`make live` sets
// it); otherwise it prints why it is skipped.
//
// It is the harness the owner's own 02 slugify session uses: a plan with
// `#+TT_WORKERS: 2` runs one round, both candidates are checked one after the
// other under the machine-wide lock, M/A/B review every passing candidate and
// vote in the pick turn, and the winner is handed to the single-candidate
// pipeline. Bounded at 45 minutes; on timeout it stops and reports what
// happened rather than faking success.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomBytes } from "node:crypto";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { Conductor, createRun, runPaths, type Deadlines, type RunPlanFile } from "../../src/conductor.ts";
import { planModelSelector } from "../../src/core/roles.ts";
import { roundsSection } from "../../src/view.ts";
import { readLog } from "../../src/effects/log.ts";

const PROVIDER = process.env.TT_LIVE_PROVIDER ?? "vercel-ai-gateway";
const WORKER_MODEL = process.env.TT_LIVE_MODEL ?? "deepseek/deepseek-v4.1-flash-fast";
const REVIEWER_M = process.env.TT_LIVE_REVIEWER_M ?? "anthropic/claude-opus-5.5";
const REVIEWER_A = process.env.TT_LIVE_REVIEWER_A ?? "openai/gpt-6.1-sol";
const REVIEWER_B = process.env.TT_LIVE_REVIEWER_B ?? "spacexai/grok-4.6";
const RUN_BOUND_MS = 45 * 60_000;

if (process.env.TT_LIVE !== "1") {
  test("live-two-lanes (skipped)", (t) => {
    t.skip("TT_LIVE is not set to '1' — live smoke tests require real Pi, real model credentials and network access; run `make live` to opt in.");
  });
} else {
  runLiveTwoLanes();
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
  const dir = shortTmp("tt-live-lanes-repo");
  git(["init", "-q", "-b", "main"], dir);
  fs.writeFileSync(path.join(dir, "sum.js"), ["function sum(a, b) {", "  return a + b;", "}", "", "module.exports = { sum };", ""].join("\n"));
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

/** The plan this live run uses: the owner's own plan when TT_LIVE_PLAN names
 * one (an org file is parsed by the real Emacs parser, a JSON file is read as
 * it stands), else the built-in two-lane plan below. Either way the run is
 * forced to two lanes and every phase to two lanes. */
function loadOwnerPlan(): RunPlanFile | undefined {
  const file = process.env.TT_LIVE_PLAN;
  if (!file) return undefined;
  if (!fs.existsSync(file)) throw new Error(`TT_LIVE_PLAN names ${file}, which does not exist`);
  let plan: RunPlanFile;
  if (file.endsWith(".json")) {
    plan = JSON.parse(fs.readFileSync(file, "utf8")) as RunPlanFile;
  } else {
    const emacsLoad = fileURLToPath(new URL("../../../test/tradeoffs-trace-test.el", import.meta.url));
    const coreDir = fileURLToPath(new URL("../../../core", import.meta.url));
    const testDir = fileURLToPath(new URL("../../../test", import.meta.url));
    const out = execFileSync(
      "emacs",
      [
        "--batch",
        "-Q",
        "-L",
        coreDir,
        "-L",
        testDir,
        "-l",
        emacsLoad,
        "--eval",
        `(with-temp-buffer (insert-file-contents "${file}") (org-mode) (setq buffer-file-name "${file}") (princ (json-encode (plist-get (+tt-parse-plan) :plan))))`,
      ],
      { encoding: "utf8" },
    );
    plan = JSON.parse(out) as RunPlanFile;
  }
  return { ...plan, workers: 2, phases: plan.phases.map((p) => ({ ...p, workers: 2 })) };
}

function makePlan(repoDir: string): RunPlanFile {
  return {
    title: "live two lanes",
    repo: repoDir,
    integrationBranch: "main",
    workers: 2,
    checks: ["node --test"],
    models: {
      workerLanes: {
        "1": { provider: PROVIDER, model: WORKER_MODEL },
        "2": { provider: PROVIDER, model: WORKER_MODEL },
      },
      reviewerSeats: {
        M: { provider: PROVIDER, model: REVIEWER_M },
        A: { provider: PROVIDER, model: REVIEWER_A },
        B: { provider: PROVIDER, model: REVIEWER_B },
      },
    },
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
        workers: 2,
      },
    ],
  };
}

function runLiveTwoLanes(): void {
  test(
    "plan 06g: the owner's live two-lane run builds two candidates, checks them one after the other, runs six reviews and three pick votes, and names the winner in tt summary",
    { timeout: RUN_BOUND_MS + 60_000 },
    async () => {
      const owner = loadOwnerPlan();
      const repo = owner ? { dir: owner.repo, head: git(["rev-parse", owner.integrationBranch], owner.repo) } : makeLiveRepo();
      const root = shortTmp("tt-live-lanes-run");
      const plan = owner ?? makePlan(repo.dir);
      const runDir = createRun(root, plan);
      const eventsPath = runPaths(runDir).events;
      const deadlines: Partial<Deadlines> = {};
      // The same one-liner `tt start` uses: the plan's own worker.1/worker.2
      // and reviewer.M/A/B models reach every launch.
      const conductor = new Conductor({ runDir, plan, deadlines, providerModelFor: planModelSelector(plan) });

      let outcome: string = "error";
      let notes: string | undefined;
      try {
        await conductor.start();
        const start = Date.now();
        for (;;) {
          const phase = conductor.state.phase.phase;
          if (phase === "DONE" || phase === "BLOCKED" || phase === "AWAITING_OWNER") {
            outcome = phase;
            break;
          }
          if (Date.now() - start > RUN_BOUND_MS) {
            outcome = "timed-out";
            break;
          }
          await new Promise((resolve) => setTimeout(resolve, 500));
        }
      } catch (err) {
        outcome = "error";
        notes = String((err as Error)?.message ?? err);
      }
      const state = conductor.state;
      await conductor.stop().catch(() => undefined);

      const records = readLog(eventsPath).records;
      const events = records.filter((r) => r.kind === "event").map((r) => r.event as { type: string });
      const count = (type: string) => events.filter((e) => e.type === type).length;
      const round = (state.phase.rounds ?? [])[0];
      // The record the owner reads: both candidates checked, six reviews,
      // three pick votes, the winner named.
      assert.equal(count("ROUND_STARTED"), 1, `one round; outcome ${outcome} ${notes ?? ""}`);
      assert.equal(count("CANDIDATE_SUBMITTED"), 2, "both lanes froze a candidate");
      assert.equal(count("CANDIDATE_CHECKED"), 2, "both candidates were checked");
      assert.equal(count("ROUND_REVIEW_SUBMITTED"), 6, "M, A and B reviewed both passing candidates");
      assert.equal(count("PICK_VOTE"), 3, "three pick votes");
      assert.equal(count("CANDIDATE_PICKED"), 1, "the winner was recorded");
      assert.ok(round?.picked, "the round names its winner");
      assert.equal(outcome, "DONE", `the phase reached DONE (got ${outcome} ${notes ?? ""})`);
      assert.equal(state.phase.candidate?.sha, round!.picked!.sha, "the phase's candidate is the winner");
      // The winner is named in `tt summary`'s own rounds section.
      const summary = roundsSection(state.phase).join("\n");
      assert.match(summary, /winner C1-|winner C2-/, `tt summary names the winner:\n${summary}`);
      assert.match(summary, new RegExp(`winner C${round!.round}-${round!.picked!.lane}`));
      assert.ok(
        (state.phase.rounds ?? []).length === 1 || state.phase.repairRoundsUsed >= 1,
        "a second round costs a repair attempt",
      );
      // The checks never overlapped: the started/finished pairs are disjoint.
      const started = records.filter((r) => r.kind === "lane_check_started");
      const finished = records.filter((r) => r.kind === "lane_check_finished");
      assert.equal(started.length, finished.length, "every check finished");
      const intervals = started.map((s) => {
        const lane = (s.event as { lane: string }).lane;
        const f = finished.find((r) => (r.event as { lane: string }).lane === lane)!;
        return { start: Date.parse(s.ts), finish: Date.parse(f.ts) };
      });
      const sorted = intervals.sort((a, b) => a.start - b.start);
      for (let i = 1; i < sorted.length; i += 1) {
        assert.ok(sorted[i - 1].finish <= sorted[i].start, "the candidates' checks never overlap");
      }
    },
  );
}
