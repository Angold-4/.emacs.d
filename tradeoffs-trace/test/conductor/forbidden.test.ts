// 07g1 acceptance: #+TT_FORBIDDEN is refused by the mapper's shell tool and
// recorded. The command would create a marker file; if it ran, the marker
// would exist.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { buildContract } from "../../src/conductor.ts";
import { readEvents } from "./harness.ts";
import { cleanupDir, defaultWorkerHello, setupConductor, waitFor, type FakePiStep } from "./harness.ts";

test("a mapper command matching TT_FORBIDDEN is refused and recorded", async () => {
  const marker = "forbidden-ran.txt";
  const setup = await setupConductor({
    phase: {
      id: "p1",
      goal: "forbidden",
      acceptance: ["it works"],
      checks: ["true"],
      boundaries: [],
      reserved: [],
      domains: [{ name: "x", globs: ["src/**"] }],
      forbidden: ["--include-ignored"],
    },
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `touch ${marker} --include-ignored` } as FakePiStep,
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () =>
        readEvents(setup.runDir).some(
          (r) => r.kind === "event" && (r.event as { type?: string }).type === "FORBIDDEN_REFUSED",
        ),
      30_000,
      50,
      setup.runDir,
    );
    const refused = readEvents(setup.runDir).find(
      (r) => r.kind === "event" && (r.event as { type?: string }).type === "FORBIDDEN_REFUSED",
    )!.event as { pattern?: string; command?: string };
    assert.equal(refused.pattern, "--include-ignored");
    assert.match(refused.command ?? "", /--include-ignored/);
    assert.ok(!fs.existsSync(path.join(setup.repo.dir, marker)), "the refused command never ran");
    assert.deepEqual(buildContract({ ...setup.plan.phases[0] }).forbidden, ["--include-ignored"]);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.repo.dir);
    cleanupDir(setup.scriptsDir);
  }
});
