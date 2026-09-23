// Taint-reset test (phase 1b work-packet item 5, design §2.2): "if the
// sweep found survivors, the worktree is tainted, and the next attempt
// starts from a clean checkout of the last candidate."
//
// Attempt 1's worker writes a normal file, launches a persistent
// background survivor writer (never stops on its own — exactly like
// freeze-e2e.test.ts), then submits. Its freeze's own sweep kills the
// survivor and reports tainted:true. `plan.checks` is chosen so candidate
// C1 (which does not yet contain a "verified" marker) fails checks,
// consuming a repair round — forcing attempt 2. Attempt 2's own worker
// script (a different fake-pi script than attempt 1's, keyed by agent id)
// checks, from *inside the freshly reset worktree*, that the survivor's
// file is exactly what candidate C1 committed (no further writes landed)
// and that the tree has no stray extra files, then writes the marker
// candidate C2 needs to pass checks.
//
// Asserts: a `worktree_reset` log record naming candidate C1 appears
// between the two attempts, and that attempt 2's own in-worktree check
// (run before it does anything else) found the reset worktree clean.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { randomBytes } from "node:crypto";
import { test } from "node:test";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  makeRepo,
  makeRunRoot,
  readEvents,
  waitFor,
  writeScript,
  type TestRepo,
} from "./harness.ts";
import { Conductor, createRun, type RunPlanFile } from "../../src/conductor.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));

function shortTmp(prefix: string): string {
  const dir = path.join("/tmp", `${prefix}-${randomBytes(4).toString("hex")}`);
  fs.mkdirSync(dir, { recursive: true });
  return dir;
}

test("tainted-reset: after a tainted freeze, the next attempt's worktree is reset to a clean checkout of the last candidate", async () => {
  const repo: TestRepo = makeRepo();
  const runRoot = makeRunRoot();
  const scriptsDir = shortTmp("tt-scripts");

  // checks: candidate must contain "verified.txt" to pass — attempt 1's
  // candidate never creates it (checks fail, one repair round consumed);
  // attempt 2's own script creates it only after checking the reset
  // worktree is clean.
  const checks = ["test -f verified.txt"];
  const plan: RunPlanFile = {
    title: "tainted-reset test plan",
    repo: repo.dir,
    integrationBranch: "main",
    checks,
    phases: [
      { id: "p1", goal: "do the thing", acceptance: ["it works"], checks, boundaries: [], reserved: [] },
    ],
  };
  const runDir = createRun(runRoot, plan);

  const attempt1Script = writeScript(scriptsDir, "attempt1", {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: "echo hi > attempt1.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      {
        kind: "call-sh",
        // Never stops on its own — only the freeze's sweep can end it, so
        // the freeze is guaranteed to observe (and report) a survivor.
        command:
          '( i=0; while true; do i=$((i+1)); echo "survivor-$i" >> survivor.txt; sleep 0.05; done ) >/dev/null 2>&1 &',
      },
    ],
  });

  const verifyResultPath = path.join(scriptsDir, "verify-result.txt");
  const attempt2Script = writeScript(scriptsDir, "attempt2", {
    hello: defaultWorkerHello(),
    steps: [
      {
        kind: "call-sh",
        // Runs first, against the just-reset worktree: attempt1.txt (part
        // of candidate C1) must be present, survivor.txt must equal
        // whatever C1 actually committed (no further writes since — the
        // survivor was killed before any more could land), and nothing
        // else must be lying around beyond C1's own tree plus .git.
        command: [
          "ok=yes",
          "test -f attempt1.txt || ok=no",
          `[ "$(git status --porcelain)" = "" ] || ok=no`,
          `echo "$ok" > "${verifyResultPath}"`,
        ].join(" && "),
      },
      { kind: "call-sh", command: "echo done > verified.txt" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  });

  const reviewerScriptPaths = new Map<string, string>();

  const conductor = new Conductor({
    runDir,
    plan,
    piCommand: process.execPath,
    piArgsPrefix: [FAKE_PI_PATH],
    deadlines: {
      abortGraceMs: 500,
      termGraceMs: 500,
      helloTimeoutMs: 5_000,
      workerAttemptMs: 20_000,
      checkMs: 10_000,
      freezeMs: 20_000,
      reviewMs: 10_000,
    },
    piEnvFor: (role, agentId) => {
      if (role === "worker") {
        const attemptMatch = agentId.match(/^worker-(\d+)-/);
        const n = attemptMatch ? Number(attemptMatch[1]) : 1;
        return { FAKE_PI_SCRIPT: n === 1 ? attempt1Script : attempt2Script };
      }
      const reviewer = (agentId.match(/^reviewer-([MAB])-/)?.[1] ?? "M") as Reviewer;
      if (!reviewerScriptPaths.has(agentId)) {
        const script = {
          hello: defaultReviewerHello(),
          steps: [
            {
              kind: "call-submit",
              tool: "submit_review" as const,
              args: reviewArgsFor(reviewer, conductor.state),
            },
          ],
        };
        reviewerScriptPaths.set(agentId, writeScript(scriptsDir, agentId, script));
      }
      return { FAKE_PI_SCRIPT: reviewerScriptPaths.get(agentId)! };
    },
  });

  function reviewArgsFor(reviewer: Reviewer, state: State) {
    return {
      reviewer,
      phaseId: state.phase.phaseId,
      candidateSha: state.phase.candidate?.sha,
      contractVersion: state.phase.contract.contractVersion,
      correctionStatements: [],
      findingStatements: [],
    };
  }

  await conductor.start();
  try {
    // Attempt 1's freeze must report tainted:true (its survivor writer is
    // still alive when the freeze's own sweep runs).
    await waitFor(() => {
      const events = readEvents(runDir);
      return events.some(
        (e) => e.kind === "event" && (e.event as { type?: string; tainted?: boolean }).type === "FREEZE_COMPLETED" && (e.event as { tainted?: boolean }).tainted === true,
      );
    }, 20_000);

    // Checks fail (no verified.txt yet) -> a repair round -> attempt 2,
    // which resets the tainted worktree to a clean checkout of C1 first.
    await waitFor(() => {
      const events = readEvents(runDir);
      return events.some((e) => e.kind === "worktree_reset");
    }, 20_000);
    const resetRecord = readEvents(runDir).find((e) => e.kind === "worktree_reset")!;
    const candidate1Sha = (resetRecord.event as { candidateSha: string }).candidateSha;
    assert.ok(candidate1Sha, "expected the worktree_reset record to name candidate C1's sha");

    // Attempt 2's own in-worktree check (run against the freshly reset
    // worktree, before it does anything else) found it clean.
    await waitFor(() => fs.existsSync(verifyResultPath), 20_000);
    const verifyResult = fs.readFileSync(verifyResultPath, "utf8").trim();
    assert.equal(verifyResult, "yes", "the reset worktree must exactly equal candidate C1: attempt1.txt present, git status clean");

    await waitFor(() => conductor.state.phase.phase === "DONE", 30_000);
  } finally {
    await conductor.stop();
    cleanupDir(runRoot);
    cleanupDir(scriptsDir);
  }
});
