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
import { RunnerMismatchError, runPaths, runnerRevision } from "../../src/conductor.ts";
import { cleanupDir, defaultWorkerHello, setupConductor } from "./harness.ts";

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

// Plan 06e (A4/R5/C2): a real, finished 05-era run, checked in under
// test/fixtures/runs/05-era and recorded by the 05j runner
// (feat/tradeoffs-trace-v1-models--05j). `runnerFor` picks that revision's
// installed copy when one exists; when it does not, the current runner reads
// the run, and its projection is the one the log recorded.
const FIXTURE_05 = fileURLToPath(new URL("../fixtures/runs/05-era", import.meta.url));
const FIXTURE_05_REVISION = "28a599f1fb75daa0897b8dece80ede26d3d28ed8";

/** Copy the checked-in 05-era run into a fresh run root, so a test never
 * mutates the fixture. */
function installFixture05(): { runDir: string; runRoot: string; cleanup: () => void } {
  const runRoot = fs.mkdtempSync("/tmp/tt-05-era-");
  const runDir = path.join(runRoot, "05-era");
  fs.cpSync(FIXTURE_05, runDir, { recursive: true });
  return { runDir, runRoot, cleanup: () => cleanupDir(runRoot) };
}

function ttStatus(runDir: string, root: string): string {
  return execFileSync(process.execPath, [cli, "status", runDir, "--root", root], { encoding: "utf8" });
}

test("plan 06e: a fixture run recorded under an older runner revision still reports its recorded phase", () => {
  const fixture = installFixture05();
  try {
    const meta = JSON.parse(fs.readFileSync(runPaths(fixture.runDir).meta, "utf8")) as { runnerRevision?: string };
    assert.equal(meta.runnerRevision, FIXTURE_05_REVISION, "the fixture is a real run recorded by the 05j runner");
    assert.notEqual(meta.runnerRevision, runnerRevision(), "the recorded revision is older than this runner");
    assert.equal(
      fs.existsSync(path.join(fixture.runRoot, "runner", FIXTURE_05_REVISION)),
      false,
      "the recorded revision is not installed, so the current runner reads the run",
    );
    const status = ttStatus(fixture.runDir, fixture.runRoot);
    assert.match(status, /phase: p1 — DONE/, `expected the recorded phase in:\n${status}`);
  } finally {
    fixture.cleanup();
  }
});

test("plan 06e: a finished run's projection is identical when the current runner reads it", () => {
  const fixture = installFixture05();
  try {
    // The recorded revision is not installed, so the current runner reads it.
    const first = ttStatus(fixture.runDir, fixture.runRoot);
    const second = ttStatus(fixture.runDir, fixture.runRoot);
    assert.equal(first, second, "a finished run's projection never changes between reads");
    assert.match(first, /phase: p1 — DONE/, "the recorded outcome is served as it was");
    // The view the recording runner wrote is frozen and still says DONE too.
    assert.match(fs.readFileSync(runPaths(fixture.runDir).status, "utf8"), /· DONE ·/);
  } finally {
    fixture.cleanup();
  }
});

test("plan 06e: an installed runner for the recorded revision reads the run", () => {
  const fixture = installFixture05();
  try {
    // A frozen copy of the recorded revision: its own CLI marks that it
    // served the read, so delegation is observable.
    const installed = path.join(fixture.runRoot, "runner", FIXTURE_05_REVISION, "tradeoffs-trace");
    fs.mkdirSync(path.join(installed, "src"), { recursive: true });
    fs.writeFileSync(path.join(installed, "RUNNER_SHA"), `${FIXTURE_05_REVISION}\n`);
    fs.writeFileSync(
      path.join(installed, "src", "cli.ts"),
      `process.stdout.write("SERVED BY ${FIXTURE_05_REVISION}\\n");\n`,
    );
    const status = ttStatus(fixture.runDir, fixture.runRoot);
    assert.match(status, new RegExp(`SERVED BY ${FIXTURE_05_REVISION}`), "the recorded runner served the read");
  } finally {
    fixture.cleanup();
  }
});
