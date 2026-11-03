import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import { rebuildState } from "../../src/conductor.ts";
import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor } from "./harness.ts";

const FAST = { abortGraceMs: 150, termGraceMs: 150, helloTimeoutMs: 5_000,
  workerAttemptMs: 20_000, checkMs: 10_000, finalCheckMs: 10_000, probeMs: 10_000,
  freezeMs: 10_000, reviewMs: 10_000, evaluateMs: 10_000, reproductionMs: 10_000 };
const STALE = "if test -f src/fixed; then printf 'ok 1 - stale expectation\\n'; else printf 'not ok 1 - stale expectation\\nsrc/stale.test.ts:1 old expectation\\n'; exit 1; fi";
const SECOND = "if test -f src/second-fixed; then printf 'ok 1 - second stale\\n'; else printf 'not ok 1 - second stale\\nsrc/other.test.ts:1 old expectation\\n'; exit 1; fi";
const worker = (command: string) => ({ hello: defaultWorkerHello(), steps: [
  { kind: "call-sh", command }, { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
] });
const review = (reviewer: string, state: any, lane = false) => ({ hello: defaultReviewerHello(), steps: [
  ...(lane ? [{ kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } }, { kind: "wait-for-prompt" }] : []),
  { kind: "call-submit", tool: "submit_review", args: { reviewer, phaseId: "p1",
    candidateSha: lane ? "$TT_CANDIDATE_SHA" : state.phase.candidate?.sha,
    contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [], ballots: [], findings: [] } },
] });

for (const lanes of [false, true]) test(`early compatibility inventory hands every stale failure to ${lanes ? "domain MAP" : "the next worker"} and refreshes after repair`, async () => {
  let repairFindings: any[] = [];
  const setup = await setupConductor({
    phase: { id: "p1", goal: "inventory before acceptance", acceptance: ["it works"], checks: ["true"],
      finalChecks: [STALE, SECOND, "printf 'ok 1 - unaffected\\n'"], boundaries: ["src/**"], reserved: [],
      ...(lanes ? { workers: 2, rounds: 3, domains: [{ name: "core", globs: ["src/**"] }], fixRounds: "domains", fixChecks: "true" } : {}) },
    checks: ["true"], stubReviews: true, deadlines: FAST,
    workerScript: () => worker("mkdir -p src; printf initial > src/change"),
    workerScriptForAttempt: (attempt, { state }) => {
      if (attempt > 1) repairFindings = state.phase.findings.filter((f) => f.status === "open" && f.id.includes("-compat-"));
      return worker(attempt === 1 ? "mkdir -p src; printf initial > src/change" : "touch src/fixed src/second-fixed");
    },
    laneWorkerScriptFor: (lane) => lane === "b" ? { hello: defaultWorkerHello(), steps: [{ kind: "crash", code: 9 }] }
      : worker("mkdir -p src; printf initial > src/change"),
    laneReviewerScriptFor: (seat, _sha, state) => review(seat, state, true),
    reviewerScriptFor: (seat, state) => review(seat, state),
    fixerScriptFor: (_domain, _round, _n, state) => {
      repairFindings = state.phase.findings.filter((f) => f.status === "open" && f.id.includes("-compat-"));
      return worker("touch src/fixed src/second-fixed");
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => ["DONE", "AWAITING_OWNER", "BLOCKED"].includes(setup.conductor.state.phase.phase), 90_000, 25, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE", JSON.stringify(setup.conductor.state.phase.ownerRequests));
    assert.equal(repairFindings.length, 2, "both failing final commands reach the first repair");
    assert.deepEqual(repairFindings.map((f) => f.doneWhen).sort(), [STALE, SECOND].sort());
    assert.ok(repairFindings.every((f) => /Failing command:/.test(f.evidence)));
    const events = readEvents(setup.runDir);
    const inventories = events.filter((r) => r.kind === "compatibility_inventory_finished");
    assert.equal(inventories.length, 2, "first green candidate plus material repair, without acceptance-only delay");
    const first = inventories[0].event as any;
    assert.deepEqual(first.tests.map((t: any) => t.outcome), ["failed", "failed", "passed"]);
    const firstIndex = events.indexOf(inventories[0]);
    if (lanes) assert.ok(firstIndex < events.findIndex((r) => r.kind === "event" && (r.event as any).type === "CANDIDATE_PICKED"),
      "a green lane gets its inventory before pick/acceptance, even when no review can complete");
    assert.ok(!events.slice(0, firstIndex).some((r) => r.kind === "event" && (r.event as any).type === "FINAL_CHECK_REQUIRED"));
    const second = inventories[1].event as any;
    assert.ok(second.tests.every((t: any) => t.outcome === "passed"));
    const kept = JSON.parse(fs.readFileSync(path.join(setup.runDir, "checks", first.candidateSha, "compatibility.json"), "utf8"));
    assert.equal(kept.commands.length, 3);
    assert.equal(kept.commands[1].tests[0].name, "second stale");
    assert.deepEqual(rebuildState(setup.runDir, setup.plan).phase.findings, setup.conductor.state.phase.findings,
      "strict replay preserves inventory findings and their closure evidence");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot); cleanupDir(setup.scriptsDir); cleanupDir(setup.repo.dir);
  }
});

test("compatibility inventory masks declared secrets and reuses the masked command record at acceptance", async () => {
  const name = "TT_COMPATIBILITY_TEST_SECRET";
  const previous = process.env[name];
  const value = `compat-${randomUUID()}`;
  process.env[name] = value;
  const command = `printf '%s\\n' '${value}'; printf 'ok 1 - compatible\\n'`;
  const setup = await setupConductor({
    secrets: [name], checks: ["true"], stubReviews: true, deadlines: FAST,
    phase: { id: "p1", goal: "masked inventory", acceptance: ["it works"], checks: ["true"],
      finalChecks: [command], boundaries: ["src/**"], reserved: [] },
    workerScript: () => worker("mkdir -p src; printf initial > src/change"),
    reviewerScriptFor: (seat, state) => review(seat, state),
  });
  try {
    await setup.conductor.start();
    await waitFor(() => ["DONE", "AWAITING_OWNER", "BLOCKED"].includes(setup.conductor.state.phase.phase), 60_000, 25, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    const events = readEvents(setup.runDir);
    assert.equal(events.filter((r) => r.kind === "compatibility_inventory_finished").length, 1);
    assert.ok(events.some((r) => r.kind === "compatibility_check_reused"), "redaction must preserve same-candidate cache lookup");
    const disk = fs.readFileSync(path.join(setup.runDir, "checks", setup.conductor.state.phase.candidate!.sha, "compatibility.json"), "utf8");
    assert.ok(!disk.includes(value), "neither command nor output may persist the secret");
    assert.ok(disk.includes(`***${name}***`));
    assert.equal(rebuildState(setup.runDir, setup.plan).phase.phase, "DONE");
  } finally {
    if (previous === undefined) delete process.env[name]; else process.env[name] = previous;
    await setup.conductor.stop();
    cleanupDir(setup.runRoot); cleanupDir(setup.scriptsDir); cleanupDir(setup.repo.dir);
  }
});
