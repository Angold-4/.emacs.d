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
import { execFileSync } from "node:child_process";
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
  setupConductor,
  waitFor,
  writeScript,
  type TestRepo,
} from "./harness.ts";
import { Conductor, createRun, runPaths, type RunPlanFile } from "../../src/conductor.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

const FAKE_PI_PATH = fileURLToPath(new URL("../fake-pi/fake-pi.ts", import.meta.url));
const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function processAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

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
    stubReviews: true,
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
    // worktree, before it does anything else) found it clean. Wait for the
    // file's CONTENT, not just its existence: the shell's `>` creates the file
    // before `echo` writes it, so a load-delayed write was read as '' and
    // flaked this assertion.
    await waitFor(() => {
      try {
        return fs.readFileSync(verifyResultPath, "utf8").trim().length > 0;
      } catch {
        return false;
      }
    }, 20_000);
    const verifyResult = fs.readFileSync(verifyResultPath, "utf8").trim();
    assert.equal(verifyResult, "yes", "the reset worktree must exactly equal candidate C1: attempt1.txt present, git status clean");

    await waitFor(() => conductor.state.phase.phase === "DONE", 30_000);
  } finally {
    await conductor.stop();
    cleanupDir(runRoot);
    cleanupDir(scriptsDir);
  }
});

// Plan 06e (A2/R3/C1): a process with a file under the worktree whose process
// group the run never recorded is HELD, never signalled, and `tt status`
// lists it. The holder leaves its group and runs with cwd `/`, exactly like a
// bind mount's file server or an escaped detached descendant.
test("plan 06e: a process holding a worktree file outside the run's process groups is reported held and never signalled", async () => {
  const markerDir = fs.mkdtempSync("/tmp/tt-held-");
  const ready = path.join(markerDir, "ready");
  const pidFile = path.join(markerDir, "holder.pid");
  const perl = path.join(markerDir, "holder.pl");
  fs.writeFileSync(
    perl,
    [
      "use strict; use warnings;",
      "my ($file, $ready, $pidfile) = @ARGV;",
      "setpgrp(0,0);",
      "open(my $f, '<', $file) or die \"open: $!\";",
      "chdir '/' or die \"chdir: $!\";",
      "open(my $r, '>', $ready) or die \"ready: $!\";",
      "open(my $p, '>', $pidfile) or die \"pid: $!\"; print $p $$; close($p); close($r);",
      "sleep 300;",
    ].join("\n") + "\n",
  );

  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        {
          // The agent's `sh` runs in the worktree. The holder opens
          // `holder.txt` there, then leaves its group and changes cwd, so the
          // run cannot own it. The ready file makes the sweep timing
          // deterministic.
          kind: "call-sh",
          command: `touch holder.txt; /usr/bin/perl ${perl} holder.txt ${ready} ${pidFile} & i=0; while [ ! -f ${ready} ] && [ $i -lt 200 ]; do i=$((i+1)); sleep 0.05; done`,
        },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 5_000, workerAttemptMs: 60_000, checkMs: 10_000, freezeMs: 20_000, reviewMs: 10_000 },
  });

  let holderPid = 0;
  try {
    await setup.conductor.start();
    await waitFor(() => fs.existsSync(ready), 20_000);
    holderPid = Number(fs.readFileSync(pidFile, "utf8").trim());
    assert.ok(holderPid > 0, "expected the holder's pid");

    // The freeze's own sweep must have run and reported the holder.
    await waitFor(() => readEvents(setup.runDir).some((e) => e.kind === "sweep"), 30_000);
    const sweeps = readEvents(setup.runDir).filter((e) => e.kind === "sweep");
    const held = sweeps.flatMap(
      (e) => (e.event as { held?: Array<{ pid: number; command: string; cwd: string }> }).held ?? [],
    );
    assert.ok(held.some((h) => h.pid === holderPid), `expected the holder in held: ${JSON.stringify(held)}`);
    assert.match(held.find((h) => h.pid === holderPid)!.cwd, /^\//, "held reports the process's cwd");
    const killed = sweeps.flatMap((e) => (e.event as { killed?: Array<{ pid: number }> }).killed ?? []);
    assert.ok(!killed.some((k) => k.pid === holderPid), "the holder is never signalled");
    assert.ok(processAlive(holderPid), "the held process is still alive");

    // `tt status` lists it under held.
    const status = execFileSync(process.execPath, [CLI_PATH, "status", setup.runDir], { encoding: "utf8" });
    assert.match(status, /^held: /m, `expected a held line in:\n${status}`);
    assert.match(status, new RegExp(`held: ${holderPid} `), `expected held pid ${holderPid} in:\n${status}`);
  } finally {
    await setup.conductor.stop();
    if (holderPid > 0) {
      try {
        process.kill(holderPid, "SIGKILL");
      } catch {
        // already gone
      }
    }
    cleanupDir(markerDir);
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(setup.repo.dir);
  }
});
