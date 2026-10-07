// Plan phase 3 exit gate `runner-pinned`: a run started from runner X refuses
// to resume under runner Y, and an installed runner is a frozen copy, so
// editing the repository's tradeoffs-trace/ does not change the code a
// running conductor executes.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { EventLog } from "../../src/effects/log.ts";
import { RunnerMismatchError, runPaths } from "../../src/conductor.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { cleanupDir, defaultWorkerHello, setupConductor, waitFor } from "./harness.ts";

const pkgRoot = fileURLToPath(new URL("../..", import.meta.url));
const cli = path.join(pkgRoot, "src", "cli.ts");

test("runner-pinned: a run started under another runner revision refuses to resume", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [] }),
  });
  try {
    // The run's init record says it was started under a different runner.
    new EventLog(runPaths(setup.runDir).events).append("init", {
      runId: "pinned",
      integrationHead: execFileSync("git", ["-C", setup.repo.dir, "rev-parse", "HEAD"], { encoding: "utf8" }).trim(),
      runnerRevision: "0000000000000000000000000000000000000000",
    });
    await assert.rejects(() => setup.conductor.start(), (err: unknown) => {
      assert.ok(err instanceof RunnerMismatchError);
      assert.match((err as Error).message, /refusing to resume/);
      return true;
    });
  } finally {
    await setup.conductor.stop().catch(() => undefined);
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("runner-pinned: `tt runner install` freezes a copy that edits to the repository do not reach", () => {
  const root = fs.mkdtempSync("/tmp/tt-runner-");
  try {
    const head = execFileSync("git", ["-C", pkgRoot, "rev-parse", "HEAD"], { encoding: "utf8" }).trim();
    const out = execFileSync(process.execPath, [cli, "runner", "install", head, "--root", root], { encoding: "utf8" }).trim();
    assert.equal(out, path.join(root, "runner", head, "tradeoffs-trace"));
    assert.equal(fs.readFileSync(path.join(out, "RUNNER_SHA"), "utf8").trim(), head);
    assert.equal(fs.readlinkSync(path.join(root, "runner", "current")), head);
    // The frozen copy is the committed tree, independent of the working tree.
    const frozen = fs.readFileSync(path.join(out, "src", "conductor.ts"), "utf8");
    const committed = execFileSync("git", ["-C", pkgRoot, "show", `${head}:tradeoffs-trace/src/conductor.ts`], {
      encoding: "utf8",
      maxBuffer: 64 * 1024 * 1024,
    });
    assert.equal(frozen, committed);
    // An installed runner reports the revision in its RUNNER_SHA (checked on a
    // copy of the current package, since the committed revision may predate it).
    const copy = path.join(root, "copy");
    fs.cpSync(pkgRoot, copy, { recursive: true, filter: (src) => !src.includes(`${path.sep}.git`) });
    fs.writeFileSync(path.join(copy, "RUNNER_SHA"), "abc123pinned\n");
    const rev = execFileSync(
      process.execPath,
      ["-e", `import("${path.join(copy, "src", "conductor.ts")}").then(m => process.stdout.write(m.runnerRevision()))`],
      { encoding: "utf8" },
    );
    assert.equal(rev, "abc123pinned");
  } finally {
    cleanupDir(root);
  }
});

// Plan 06e (A4/R5/C2): a finished run recorded under an older runner
// revision. `runnerFor` picks that revision's installed copy when one exists;
// when it does not, the current runner reads the run, and its projection is
// the one the log recorded (a finished run never changes).
const OLD_REVISION = "0000000000000000000000000000000000000005";

/** A real finished run (DONE) whose recorded revision is `old`. The events
 * are the 05-era shape: `init` plus transitions, read leniently. */
async function finishedRunRecordedUnder(old: string) {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
    }),
    reviewerScriptFor: (reviewer, state) => ({
      hello: { role: "reviewer", tools: ROLE_TOOLS.reviewer },
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
    deadlines: { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 5_000, workerAttemptMs: 30_000, reviewMs: 10_000 },
  });
  await setup.conductor.start();
  await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000);
  await setup.conductor.stop();
  // Rewrite the recorded revision in both places `runnerFor` reads: meta.json
  // and the run's init record.
  const p = runPaths(setup.runDir);
  const meta = JSON.parse(fs.readFileSync(p.meta, "utf8")) as { runnerRevision?: string };
  meta.runnerRevision = old;
  fs.writeFileSync(p.meta, JSON.stringify(meta, null, 2));
  const events = fs
    .readFileSync(p.events, "utf8")
    .split("\n")
    .map((line) => (line.includes('"kind":"init"') ? line.replace(/"runnerRevision":"[^"]*"/, `"runnerRevision":"${old}"`) : line))
    .join("\n");
  fs.writeFileSync(p.events, events);
  return { setup, cleanup: () => { cleanupDir(setup.runRoot); cleanupDir(setup.scriptsDir); cleanupDir(setup.repo.dir); } };
}

function ttStatus(runDir: string, root: string): string {
  return execFileSync(process.execPath, [cli, "status", runDir, "--root", root], { encoding: "utf8" });
}

test("plan 06e: a fixture run recorded under an older runner revision still reports its recorded phase", async () => {
  const fixture = await finishedRunRecordedUnder(OLD_REVISION);
  try {
    assert.equal(fs.existsSync(path.join(fixture.setup.runRoot, "runner", OLD_REVISION)), false, "the recorded revision is not installed");
    const status = ttStatus(fixture.setup.runDir, fixture.setup.runRoot);
    assert.match(status, /phase: p1 — DONE/, `expected the recorded phase in:\n${status}`);
  } finally {
    fixture.cleanup();
  }
});

test("plan 06e: a finished run's projection is identical when the current runner reads it", async () => {
  const fixture = await finishedRunRecordedUnder(OLD_REVISION);
  try {
    // The recorded revision is not installed, so the current runner reads it.
    const first = ttStatus(fixture.setup.runDir, fixture.setup.runRoot);
    const second = ttStatus(fixture.setup.runDir, fixture.setup.runRoot);
    assert.equal(first, second, "a finished run's projection never changes between reads");
    assert.match(first, /phase: p1 — DONE/, "the recorded outcome is served as it was");
    // The view the recording runner wrote is frozen and still says DONE too.
    assert.match(fs.readFileSync(runPaths(fixture.setup.runDir).status, "utf8"), /· DONE ·/);
  } finally {
    fixture.cleanup();
  }
});

test("plan 06e: an installed runner for the recorded revision reads the run", async () => {
  const fixture = await finishedRunRecordedUnder(OLD_REVISION);
  try {
    // A frozen copy of the recorded revision: its own CLI marks that it
    // served the read, so delegation is observable.
    const installed = path.join(fixture.setup.runRoot, "runner", OLD_REVISION, "tradeoffs-trace");
    fs.mkdirSync(path.join(installed, "src"), { recursive: true });
    fs.writeFileSync(path.join(installed, "RUNNER_SHA"), `${OLD_REVISION}\n`);
    fs.writeFileSync(
      path.join(installed, "src", "cli.ts"),
      `process.stdout.write("SERVED BY ${OLD_REVISION}\\n");\n`,
    );
    const status = ttStatus(fixture.setup.runDir, fixture.setup.runRoot);
    assert.match(status, new RegExp(`SERVED BY ${OLD_REVISION}`), "the recorded runner served the read");
  } finally {
    fixture.cleanup();
  }
});
