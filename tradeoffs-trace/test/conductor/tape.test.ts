// Plan 05h: the live loop tape is written on the conductor's own status beat
// and is a projection of the control log — deleting `views/tape.txt` and
// running `tt contract rebuild` restores the identical bytes, and
// `tt contract check` agrees. A real conductor reaches DONE first, so the
// assertion covers every step the run passed through, not a hand-built state.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, rmSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, setupConductor, waitFor } from "./harness.ts";
import { runPaths } from "../../src/conductor.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function tt(args: string[]): { status: number; stdout: string; stderr: string } {
  try {
    return { status: 0, stdout: execFileSync(process.execPath, [CLI, ...args], { encoding: "utf8" }), stderr: "" };
  } catch (err) {
    const e = err as { status?: number; stdout?: Buffer | string; stderr?: Buffer | string };
    return { status: e.status ?? 1, stdout: e.stdout?.toString() ?? "", stderr: e.stderr?.toString() ?? "" };
  }
}

test("the loop tape is written on the status beat and rebuilds identically", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_review",
          args: {
            reviewer,
            phaseId: state.phase.phaseId,
            candidateSha: state.phase.candidate?.sha,
            contractVersion: state.phase.contract.contractVersion,
            correctionStatements: [],
            findingStatements: [],
          },
        },
      ],
    }),
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
  });

  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000);
    const p = runPaths(setup.runDir);
    assert.ok(existsSync(p.tape), "views/tape.txt must exist after the run");
    const live = readFileSync(p.tape, "utf8");
    // The header is the phase's readable id, short title, round, attempt and
    // elapsed time; at DONE every main-path row is drawn and the head is DONE.
    assert.match(live, /^p1 · round 1 · attempt 1\/3 · /m);
    assert.match(live, /^  ▶  DONE\b/m);
    assert.match(live, /^  ✓  IMPLEMENT\b/m);
    assert.doesNotMatch(live, /^  .  GATE\b/m);

    // Delete the live file and rebuild it from the log: identical bytes.
    rmSync(p.tape);
    const rebuilt = tt(["contract", "rebuild", setup.runDir]);
    assert.equal(rebuilt.status, 0, rebuilt.stderr);
    assert.ok(existsSync(p.tape), "rebuild must write views/tape.txt");
    assert.equal(readFileSync(p.tape, "utf8"), live);
    assert.match(tt(["contract", "check", setup.runDir]).stdout, /contract check ok/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
