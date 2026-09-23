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
