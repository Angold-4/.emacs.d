// Plan 01f: the gate stage. The conductor runs the phase's `:GATE:` command
// itself, once per candidate whose checks, probe and three reviews passed,
// before acceptance, in the probed checkout, under the machine-wide gate
// lock, and records `checks/<sha>/gate.json` + `gate.log`. The measured
// problem (runtime doc §1, §6): agents ran the expensive live gate inside
// their attempts — 82 minutes of docker in 13i/13j, some killed at the
// 8-minute command limit — and, because they had to produce the live proof,
// wrote substitutes (a sentinel `code_sha`, `pending_owner_live_run`, a
// fingerprint-only record).
//
// These tests cover: the pass path (exactly one run, record, DONE), the f ail
// path (blocking integration finding with the log tail, a repair round that
// shows the worker the log), an identical tree reusing a passing record
// without running the command, two conductors sharing the lock never gating
// at once, and a gate killed at its limit. The pure record/reuse helpers and
// the prompt lines are tested here too.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import * as fs from "node:fs";
import * as path from "node:path";
import { describe, test } from "node:test";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
} from "./harness.ts";
import {
  buildContract,
  buildReviewerPrompt,
  buildWorkerPrompt,
  GATE_BINDING_STATEMENT,
  gatePromptLines,
  runPaths,
  type ConductorOptions,
  type RunPlanFile,
} from "../../src/conductor.ts";
import { baseState } from "../unit/helpers.ts";
import type { Reviewer, State } from "../../src/core/types.ts";
import { buildView, gateSummaryLine, prSummary } from "../../src/view.ts";
import {
  GATE_TAIL_LINES,
  gateDecision,
  gateFailureEvidence,
  gateLogHashMatches,
  gateLogTail,
  parseGateRecord,
  reusableGate,
  type GateRecord,
} from "../../src/core/gate.ts";

// The wait budget for these end-to-end tests. The harness floors every
// `waitFor` at 90 s, but under `make check` the whole suite runs four files at
// once: a full conductor cycle that takes 5 s alone can take over a minute.
// 150 s keeps a healthy-but-slow run green and still fails a stuck one (the
// runner's per-test timeout is 180 s).
const WAIT_MS = 150_000;

const FAST_DEADLINES: ConductorOptions["deadlines"] = {
  abortGraceMs: 500,
  termGraceMs: 300,
  helloTimeoutMs: 5_000,
  reviewMs: 10_000,
  checkMs: 30_000,
  probeMs: 30_000,
  gateMs: 30_000,
};

function workerScript() {
  return {
    hello: defaultWorkerHello(),
    steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
  };
}

function reviewerScriptFor(reviewer: Reviewer, state: State) {
  return {
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
  };
}

function shortTmp(prefix: string): string {
  return fs.mkdtempSync(path.join("/tmp", `${prefix}-`));
}

function gateRecordAt(runDir: string, sha: string): GateRecord {
  const record = parseGateRecord(JSON.parse(fs.readFileSync(path.join(runPaths(runDir).checks, sha, "gate.json"), "utf8")));
  assert.ok(record, `expected a parseable gate record for ${sha}`);
  return record;
}

function sha256File(file: string): string {
  return createHash("sha256").update(fs.readFileSync(file, "utf8")).digest("hex");
}

// ---------------------------------------------------------------------------
// The pure record / reuse helpers (core/gate.ts)
// ---------------------------------------------------------------------------

function recordFixture(overrides: Partial<GateRecord> = {}): GateRecord {
  return {
    candidateSha: "c1",
    tree: "tree-a",
    baseSha: "h1",
    command: "deploy --clean --build",
    startedAt: "2026-09-25T00:00:00.000Z",
    durationMs: 900_000,
    exitCode: 0,
    timedOut: false,
    passed: true,
    logSha256: "a".repeat(64),
    logBytes: 10,
    ...overrides,
  };
}

test("gate: parseGateRecord accepts a full record and refuses one missing a design-required field", () => {
  const full = { ...recordFixture(), cleanup: "docker compose down", cleanupExitCode: 0 };
  assert.deepEqual(parseGateRecord(full), full);
  for (const missing of ["candidateSha", "baseSha", "command", "exitCode", "durationMs", "startedAt", "logSha256", "timedOut", "passed"]) {
    const broken = { ...recordFixture() } as Record<string, unknown>;
    delete broken[missing];
    assert.equal(parseGateRecord(broken), undefined, `${missing} must be required`);
  }
  assert.equal(parseGateRecord("not a record"), undefined);
  assert.equal(parseGateRecord(null), undefined);
});

test("gate: gateLogTail returns the last N lines (the tail a failure quotes)", () => {
  const text = `${Array.from({ length: 70 }, (_, i) => `line-${i + 1}`).join("\n")}\n`;
  const tail = gateLogTail(text, 60).split("\n");
  assert.equal(tail.length, 60);
  assert.equal(tail[0], "line-11");
  assert.equal(tail[59], "line-70");
  assert.equal(gateLogTail("", 60), "");
});

test("gate: reusableGate reuses only a passing record for the same tree and command, newest first", () => {
  const old = recordFixture({ candidateSha: "c-old", startedAt: "2026-09-25T00:00:00.000Z" });
  const newer = recordFixture({ candidateSha: "c-new", startedAt: "2026-09-25T01:00:00.000Z" });
  const failed = recordFixture({ candidateSha: "c-fail", passed: false, exitCode: 3, startedAt: "2026-09-25T02:00:00.000Z" });
  const otherTree = recordFixture({ candidateSha: "c-tree", tree: "tree-b", startedAt: "2026-09-25T03:00:00.000Z" });
  const otherCommand = recordFixture({ candidateSha: "c-cmd", command: "other", startedAt: "2026-09-25T04:00:00.000Z" });
  const records = [old, newer, failed, otherTree, otherCommand];
  assert.equal(reusableGate(records, { tree: "tree-a", command: "deploy --clean --build" })?.candidateSha, "c-new");
  // A failed gate is never reused: a rerun is the honest answer for a new
  // candidate even with an identical tree.
  assert.equal(reusableGate([failed], { tree: "tree-a", command: "deploy --clean --build" }), undefined);
  assert.equal(reusableGate(records, { tree: "tree-c", command: "deploy --clean --build" }), undefined);
  assert.equal(reusableGate(records, { tree: "tree-a", command: "different" }), undefined);
  // Without a known tree we cannot prove the same question, so the gate runs.
  assert.equal(reusableGate(records, { tree: undefined, command: "deploy --clean --build" }), undefined);
});

test("gate: gateLogHashMatches refuses a log whose bytes are not the record's own", () => {
  const log = Buffer.from("the gate's real output\n", "utf8");
  const record = recordFixture({ logSha256: createHash("sha256").update(log).digest("hex"), logBytes: log.length });
  assert.equal(gateLogHashMatches(record, log), true);
  assert.equal(gateLogHashMatches(record, Buffer.from("a pruned or edited log\n", "utf8")), false);
  assert.equal(gateLogHashMatches({ ...record, logBytes: log.length + 1 }, log), false);
  assert.equal(gateLogHashMatches({ ...record, logSha256: "b".repeat(64) }, log), false);
});

test("gate: gateDecision accepts the candidate's own verified pass, never reuses it as a source, and reruns otherwise", () => {
  const own = recordFixture({ candidateSha: "c1", tree: "tree-a" });
  const other = recordFixture({ candidateSha: "c2", tree: "tree-a", startedAt: "2026-09-25T01:00:00.000Z" });
  // The candidate's own record wins and is never rewritten (A-9: a
  // stale-publish retry re-gates the same candidate; a reuse record claiming
  // to reuse itself would overwrite the evidence).
  assert.deepEqual(gateDecision(own, [own, other], { candidateSha: "c1", tree: "tree-a", command: own.command }), {
    kind: "own",
    record: own,
  });
  // Without a verified own record, another candidate's verified pass for the
  // same tree is reused — and the candidate's own record is never returned as
  // the reuse source.
  assert.deepEqual(gateDecision(undefined, [own], { candidateSha: "c1", tree: "tree-a", command: own.command }), {
    kind: "run",
  });
  assert.deepEqual(gateDecision(undefined, [own, other], { candidateSha: "c3", tree: "tree-a", command: own.command }), {
    kind: "reuse",
    record: other,
  });
  // An unverifiable or mismatching record is not evidence: the gate reruns.
  assert.deepEqual(gateDecision(undefined, [], { candidateSha: "c1", tree: "tree-a", command: own.command }), { kind: "run" });
  assert.deepEqual(gateDecision(undefined, [other], { candidateSha: "c2", tree: "other-tree", command: own.command }), { kind: "run" });
});

test("gate: gateFailureEvidence quotes the log's tail, the exit status and the log hash", () => {
  const record = recordFixture({ passed: false, exitCode: 3, candidateSha: "candidate-sha-1234" });
  const evidence = gateFailureEvidence({
    record,
    logPath: "/run/checks/candidate-sha-1234/gate.log",
    tail: gateLogTail("a\nb\nboom\n", GATE_TAIL_LINES),
  });
  assert.match(evidence, /exited 3/);
  assert.match(evidence, /candidate/);
  assert.match(evidence, /boom/);
  assert.match(evidence, new RegExp(record.logSha256));
  assert.match(evidence, /gate\.log/);
});

// ---------------------------------------------------------------------------
// Prompt lines: the worker must never run or substitute the gate
// ---------------------------------------------------------------------------

test("gate: the worker prompt says the conductor runs the gate and forbids running or fabricating it", () => {
  const contract = buildContract({
    id: "p1",
    goal: "g",
    acceptance: ["a"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    gate: "deploy/atlas.sh --clean --build",
    gateCleanup: "docker compose down -v",
  });
  const prompt = buildWorkerPrompt(contract);
  assert.match(prompt, new RegExp(GATE_BINDING_STATEMENT.slice(0, 40)));
  assert.match(prompt, /Never run the gate command yourself/);
  assert.match(prompt, /deploy\/atlas\.sh --clean --build/);
  // A gate-less contract still gets the rule (substitutes are never welcome),
  // but names no command.
  const plain = buildWorkerPrompt(buildContract({ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] }));
  assert.match(plain, /Never run the gate command yourself/);
  assert.ok(!plain.includes("The phase's gate command is:"));
});

test("gate: the record section names the command, the outcome and the log hash, with the tail when it failed", () => {
  const failed = recordFixture({ passed: false, exitCode: 3, candidateSha: "cand123456" });
  const lines = gatePromptLines("deploy --build", failed, "line-a\nboom").join("\n");
  assert.match(lines, /deploy --build/);
  assert.match(lines, /failed \(exit 3\)/);
  assert.match(lines, /boom/);
  assert.match(lines, new RegExp(failed.logSha256));
  assert.match(lines, /Never run the gate command yourself/);
  // A passing record needs no tail, and a phase with no gate and no record
  // gets no section at all.
  const ok = gatePromptLines("deploy --build", recordFixture()).join("\n");
  assert.match(ok, /passed \(exit 0\)/);
  assert.ok(!ok.includes("boom"));
  // A gate-less phase still gets the binding rule (a substitute is never
  // evidence), just no command.
  const plain = gatePromptLines(undefined, undefined).join("\n");
  assert.match(plain, /Never run the gate command yourself/);
  assert.ok(!plain.includes("The phase's gate command is:"));
});

test("gate: the stub reviewer prompt carries the gate rule too", () => {
  // buildReviewerPrompt is the one-turn reviewer prompt (stub reviewers), and
  // is the function finding B-8 named: it must state that the conductor owns
  // the gate evidence, with or without a gate.
  const gate = "deploy --clean --build";
  const contract = buildContract({ id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [], gate });
  const gated = baseState({ phase: "REVIEWING", contract, candidate: { sha: "c1", contractVersion: contract.contractVersion } });
  const prompt = buildReviewerPrompt(gated.phase, "M");
  assert.match(prompt, /Never run the gate command yourself/);
  assert.ok(prompt.includes(gate), "the reviewer prompt must name the gate command");
  const plain = buildReviewerPrompt(baseState({ phase: "REVIEWING" }).phase, "M");
  assert.match(plain, /Never run the gate command yourself/);
  assert.ok(!plain.includes("The phase's gate command is:"));
});

// ---------------------------------------------------------------------------
// End to end
//
// Wrapped in one `describe` with `concurrency: 4`: each of these drives a real
// conductor (a couple of seconds of process spawning and git checkouts), and
// this phase's own `:CHECKS:` is the whole `make -C tradeoffs-trace check`
// suite under the plan's check deadline, so the file must not add serial
// minutes to it. Every test builds its own temp repo, run root and gate lock,
// so they are independent. Three is the ceiling: at six concurrent conductors
// the host serializes them and a wait budget can be exhausted.
// ---------------------------------------------------------------------------

describe("gate: end to end", { concurrency: 3 }, () => {

test("gate: a passing gate runs exactly once, records the candidate, the exit status and the log hash, and reaches DONE", async () => {
  const dir = shortTmp("tt-gate-pass");
  const counter = path.join(dir, "runs.log");
  const lock = path.join(dir, "gate.lock");
  const setup = await setupConductor({
    checks: ["true"],
    workerScript,
    reviewerScriptFor,
    // The cleanup takes 3 s and fails with its own exit status while the gate
    // is instant: the record must show the gate's own duration and status and
    // the cleanup's separately (folding them would cite a build time the gate
    // did not spend, and a cleanup failure it did not have).
    gate: `echo gate-ran >> ${counter}; echo gate-ok`,
    gateCleanup: `echo cleanup-ran >> ${counter}; sleep 3; exit 7`,
    gateLockPath: lock,
    deadlines: FAST_DEADLINES,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", WAIT_MS, 50, setup.runDir);

    const C = setup.conductor.state.phase.candidate!.sha;
    const record = gateRecordAt(setup.runDir, C);
    assert.equal(record.candidateSha, C);
    assert.equal(record.exitCode, 0);
    assert.equal(record.passed, true);
    assert.equal(record.timedOut, false);
    assert.ok(record.durationMs >= 0);
    const logFile = path.join(runPaths(setup.runDir).checks, C, "gate.log");
    assert.equal(record.logSha256, sha256File(logFile));
    assert.equal(record.logBytes, fs.statSync(logFile).size);
    const logText = fs.readFileSync(logFile, "utf8");
    assert.match(logText, /gate-ok/);
    assert.match(logText, /cleanup-ran/);
    // The cleanup's own outcome, kept separate from the gate's: a non-zero
    // cleanup does not turn a passing gate into a failing one.
    assert.equal(record.cleanupExitCode, 7);
    assert.equal(record.cleanupTimedOut, false);
    assert.ok(record.cleanupDurationMs !== undefined && record.cleanupDurationMs >= 3000, `cleanup duration: ${record.cleanupDurationMs}`);
    // The gate's own duration cannot have included a 3 s cleanup. A fixed
    // ceiling, not a ratio: the gate command itself is instant here, so a
    // loaded host cannot push its window near the cleanup's sleep.
    assert.ok(record.durationMs < 3000, `the gate's duration must not include the cleanup (got ${record.durationMs}ms)`);

    // The command ran exactly once, and the cleanup ran after it.
    const runs = fs.readFileSync(counter, "utf8").trim().split("\n");
    assert.deepEqual(runs, ["gate-ran", "cleanup-ran"]);

    // Exactly one gate dispatch in the control log.
    const gateCompletions = readEvents(setup.runDir).filter(
      (r) => r.kind === "completion" && typeof r.actionId === "string" && r.actionId.startsWith("run_gate-"),
    );
    assert.equal(gateCompletions.length, 1, "the gate must be dispatched once for an accepted candidate");

    // The pipeline shows the new stage, and the gate record is cited.
    const plan = JSON.parse(fs.readFileSync(path.join(runPaths(setup.runDir).plan, "v1.json"), "utf8")) as RunPlanFile;
    const view = buildView(setup.runDir, plan, false);
    assert.match(view.pipeline, /gate/);
    assert.match(view.gates, /gate ✓/);
    assert.match(view.gate ?? "", /gate passed \(exit 0\)/);
    assert.match(gateSummaryLine(record), new RegExp(C.slice(0, 9)));
    // `tt summary <run>` is prSummary's Markdown verbatim (cli.ts's
    // runSummary), so this is the citation the PR body carries.
    const summary = prSummary(setup.runDir, plan);
    assert.match(summary, /- Gate: gate passed \(exit 0\)/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(dir);
  }
});

test("gate: a failing gate becomes a blocking integration finding with the log tail and a repair round that shows the worker the log", async () => {
  const dir = shortTmp("tt-gate-fail");
  const promptLog = path.join(dir, "prompts.log");
  const lock = path.join(dir, "gate.lock");
  const setup = await setupConductor({
    checks: ["true"],
    workerScript,
    reviewerScriptFor,
    gate: `echo gate-header; i=1; while [ $i -le 70 ]; do echo tail-line-$i; i=$((i+1)); done; exit 3`,
    gateLockPath: lock,
    deadlines: FAST_DEADLINES,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
  });
  try {
    await setup.conductor.start();
    // The first candidate's gate fails; the phase repairs, and the repair
    // attempt's prompt is what must carry the log.
    await waitFor(() => fs.existsSync(promptLog) && fs.readFileSync(promptLog, "utf8").includes("tail-line-70"), WAIT_MS, 50, setup.runDir);
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.kind === "integration" && f.status === "open"),
      WAIT_MS,
      50,
      setup.runDir,
    );

    const state = setup.conductor.state;
    // A repair round was spent on it (the worker is on its second attempt or
    // beyond).
    assert.ok(state.phase.repairRoundsUsed >= 1, "a gate failure must consume a repair round");
    assert.ok(state.phase.attempt.n >= 2, `expected attempt 2+, got ${state.phase.attempt.n}`);
    const gateFinding = state.phase.findings.find((f) => f.kind === "integration" && f.status === "open")!;
    assert.ok(gateFinding, "a failed gate must raise a blocking integration finding");
    assert.equal(gateFinding.severity, "blocking");
    assert.equal(gateFinding.raisedBy, "conductor");

    const evidenceLines = gateFinding.evidence.split("\n");
    assert.ok(evidenceLines.includes("tail-line-70"), "the finding must carry the log's last lines");
    assert.ok(!evidenceLines.includes("tail-line-1"), "the finding must carry the tail, not the head");
    const tailLines = evidenceLines.filter((l) => /^tail-line-\d+$/.test(l));
    assert.ok(tailLines.length >= 50 && tailLines.length <= GATE_TAIL_LINES, `expected about ${GATE_TAIL_LINES} tail lines, got ${tailLines.length}`);

    // The repair round shows the worker the conductor's own record and log.
    const prompts = fs.readFileSync(promptLog, "utf8");
    assert.match(prompts, /The conductor ran the phase's gate command and it exited 3/);
    assert.match(prompts, /tail-line-70/);

    // The record itself is a failing one, and no agent produced it: it is in
    // the conductor's own checks/<sha>/ directory.
    const failedRecord = gateRecordAt(setup.runDir, gateFinding.boundCandidateSha);
    assert.equal(failedRecord.passed, false);
    assert.equal(failedRecord.exitCode, 3);
    assert.match(fs.readFileSync(path.join(runPaths(setup.runDir).checks, gateFinding.boundCandidateSha, "gate.log"), "utf8"), /tail-line-70/);

    // The gate's own evidence names the log hash, not an agent's summary.
    assert.equal(failedRecord.logSha256, sha256File(path.join(runPaths(setup.runDir).checks, gateFinding.boundCandidateSha, "gate.log")));
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(dir);
  }
});

// The reviewers' side of the rule (findings B-8/M-3): every reviewer prompt
// says the conductor owns the gate evidence, and a reviewer is shown the
// conductor's record when one exists. Two candidate cycles are needed for a
// record to exist while reviews run, so this test waits for the *first*
// prompt that carries it (turn 1 of candidate 2), not for turn 2's tail —
// `gatePromptLines` covers the tail itself.
test("gate: a reviewer is told the conductor owns the gate and shown the failed record", async () => {
  const dir = shortTmp("tt-gate-reviewer");
  const reviewerPrompts = path.join(dir, "reviewer-prompts.log");
  const lock = path.join(dir, "gate.lock");
  const setup = await setupConductor({
    checks: ["true"],
    // The real two-turn review: turn 1 discovers, turn 2 votes and raises
    // findings — and is where the gate section belongs.
    stubReviews: false,
    workerScript,
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
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
    gate: `echo reviewer-visible-tail; exit 4`,
    gateLockPath: lock,
    deadlines: FAST_DEADLINES,
    extraReviewerEnv: () => ({ FAKE_PI_PROMPT_LOG: reviewerPrompts }),
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => fs.existsSync(reviewerPrompts) && fs.readFileSync(reviewerPrompts, "utf8").includes("Gate record for candidate"),
      WAIT_MS,
      50,
      setup.runDir,
    );
    const logged = fs.readFileSync(reviewerPrompts, "utf8");
    assert.match(logged, /Never run the gate command yourself/);
    assert.match(logged, /Gate record for candidate/);
    assert.match(logged, /failed \(exit 4\)/);
    // It is the conductor's record (its own sha256), not an agent's prose.
    assert.match(logged, /log sha256 [a-f0-9]{64}/);
    // Every turn-1 prompt carries the rule: a reviewer that only learns it
    // later could demand or accept a substitute first.
    const turn1Messages = logged.split("\n=====\n").filter((m) => m.includes("Turn 1 of 2"));
    assert.ok(turn1Messages.length > 0, "expected at least one turn-1 prompt in the log");
    for (const m of turn1Messages) {
      assert.match(m, /Never run the gate command yourself/, "every turn-1 prompt must carry the gate rule");
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(dir);
  }
});

test("gate: a candidate whose tree already has a passing record reuses it without running the command", async () => {
  const dir = shortTmp("tt-gate-reuse");
  const marker = path.join(dir, "ran.marker");
  const lock = path.join(dir, "gate.lock");
  // If this command ever ran, the gate would fail — so reaching DONE is proof
  // the record was reused.
  const command = `touch ${marker}; exit 9`;
  const setup = await setupConductor({
    checks: ["true"],
    workerScript,
    reviewerScriptFor,
    gate: command,
    gateLockPath: lock,
    deadlines: FAST_DEADLINES,
  });
  try {
    // A passing record for the candidate's tree, written before the run: the
    // worker changes nothing, so the frozen candidate's tree is the base tree.
    const tree = execFileSync("git", ["-C", setup.repo.dir, "rev-parse", "HEAD^{tree}"], { encoding: "utf8" }).trim();
    const sourceSha = "beefbeefbeefbeef";
    const sourceDir = path.join(runPaths(setup.runDir).checks, sourceSha);
    fs.mkdirSync(sourceDir, { recursive: true });
    const sourceLog = "fabricated gate log for the reuse test\n";
    fs.writeFileSync(path.join(sourceDir, "gate.log"), sourceLog);
    fs.writeFileSync(
      path.join(sourceDir, "gate.json"),
      `${JSON.stringify(
        {
          candidateSha: sourceSha,
          tree,
          // Deliberately a different base from the candidate's: the reused
          // record must name the head it is accepted against and the head its
          // evidence came from (finding B-5).
          baseSha: "0".repeat(40),
          command,
          startedAt: "2026-09-25T00:00:00.000Z",
          durationMs: 900_000,
          exitCode: 0,
          signal: null,
          timedOut: false,
          passed: true,
          logSha256: createHash("sha256").update(sourceLog).digest("hex"),
          logBytes: Buffer.byteLength(sourceLog, "utf8"),
        },
        null,
        2,
      )}\n`,
    );

    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", WAIT_MS, 50, setup.runDir);

    const C = setup.conductor.state.phase.candidate!.sha;
    const reused = gateRecordAt(setup.runDir, C);
    assert.equal(reused.passed, true);
    assert.equal(reused.reused, true);
    assert.equal(reused.reusedFrom, sourceSha);
    assert.equal(reused.tree, tree);
    // The head this candidate is accepted against, and where the evidence
    // came from, are both recorded.
    assert.equal(reused.baseSha, setup.repo.head);
    assert.equal(reused.reusedFromBaseSha, "0".repeat(40));
    // The evidence's own facts are the source run's, not "now".
    assert.equal(reused.startedAt, "2026-09-25T00:00:00.000Z");
    // No merge result is claimed for a head the command never ran at.
    assert.equal(reused.mergedI, undefined);
    assert.equal(reused.logSha256, sha256File(path.join(runPaths(setup.runDir).checks, C, "gate.log")));
    assert.ok(!fs.existsSync(marker), "the gate command must not run when an identical tree has a passing record");
    const plan = JSON.parse(fs.readFileSync(path.join(runPaths(setup.runDir).plan, "v1.json"), "utf8")) as RunPlanFile;
    assert.match(buildView(setup.runDir, plan, false).gates, /gate ✓ \(reused\)/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(dir);
  }
});

test("gate: two conductors sharing the machine-wide lock never gate at once (the second waits)", async () => {
  const dir = shortTmp("tt-gate-lock");
  const lock = path.join(dir, "gate.lock");
  const marker = path.join(dir, "in-gate.marker");
  // A gate command that proves exclusivity twice: an O_EXCL-style marker (a
  // concurrent gate would find it and fail with exit 7), and its own wall
  // clock window (recorded in gate.json).
  const command = `if [ -e ${marker} ]; then echo OVERLAP; exit 7; fi; touch ${marker}; sleep 1.5; rm -f ${marker}; echo gate-ok`;
  const mk = () =>
    setupConductor({
      checks: ["true"],
      workerScript,
      reviewerScriptFor,
      gate: command,
      gateLockPath: lock,
      deadlines: { ...FAST_DEADLINES, gateMs: 30_000 },
    });
  const a = await mk();
  const b = await mk();
  try {
    await Promise.all([a.conductor.start(), b.conductor.start()]);
    await waitFor(() => a.conductor.state.phase.phase === "DONE" && b.conductor.state.phase.phase === "DONE", WAIT_MS, 50, a.runDir);
    const recA = gateRecordAt(a.runDir, a.conductor.state.phase.candidate!.sha);
    const recB = gateRecordAt(b.runDir, b.conductor.state.phase.candidate!.sha);
    assert.equal(recA.exitCode, 0, "the first gate must not have seen another gate's marker");
    assert.equal(recB.exitCode, 0, "the second gate must not have seen another gate's marker");
    assert.equal(recA.passed, true);
    assert.equal(recB.passed, true);
    // Sequential, never overlapping: one window ends before the other starts.
    const startA = Date.parse(recA.startedAt);
    const endA = startA + recA.durationMs;
    const startB = Date.parse(recB.startedAt);
    const endB = startB + recB.durationMs;
    assert.ok(endA <= startB || endB <= startA, `gate windows overlap: A ${recA.startedAt}+${recA.durationMs}ms, B ${recB.startedAt}+${recB.durationMs}ms`);
    assert.ok(!fs.existsSync(marker), "no gate may leave its marker behind");
  } finally {
    await a.conductor.stop();
    await b.conductor.stop();
    for (const s of [a, b]) {
      cleanupDir(s.runRoot);
      cleanupDir(s.scriptsDir);
      cleanupDir(s.repo.dir);
    }
    cleanupDir(dir);
  }
});

// One run covers two things: a gate killed at its limit is a failed gate, and
// a passing record whose log no longer hashes to it is not evidence — the
// conductor must run the command rather than accept the record. The fabricated
// record claims a pass for the candidate's tree with no log behind it; the
// hanging command is what proves the gate ran (and was killed), not reused.
test("gate: a gate exceeding its limit is killed and reported as a failed gate", async () => {
  const dir = shortTmp("tt-gate-kill");
  const lock = path.join(dir, "gate.lock");
  const setup = await setupConductor({
    checks: ["true"],
    workerScript,
    reviewerScriptFor,
    gate: "echo starting-gate; sleep 30",
    gateLockPath: lock,
    deadlines: { ...FAST_DEADLINES, gateMs: 800, termGraceMs: 300 },
  });
  try {
    // A record that *claims* a pass for the candidate's tree, with a hash that
    // matches nothing and no log file (a pruned or hand-edited run dir).
    const tree = execFileSync("git", ["-C", setup.repo.dir, "rev-parse", "HEAD^{tree}"], { encoding: "utf8" }).trim();
    const sourceSha = "fadedfadedfaded1";
    const sourceDir = path.join(runPaths(setup.runDir).checks, sourceSha);
    fs.mkdirSync(sourceDir, { recursive: true });
    fs.writeFileSync(
      path.join(sourceDir, "gate.json"),
      `${JSON.stringify(
        {
          candidateSha: sourceSha,
          tree,
          baseSha: "0".repeat(40),
          command: "echo starting-gate; sleep 30",
          startedAt: "2026-09-25T00:00:00.000Z",
          durationMs: 900_000,
          exitCode: 0,
          timedOut: false,
          passed: true,
          logSha256: "c".repeat(64),
          logBytes: 10,
        },
        null,
        2,
      )}\n`,
    );

    await setup.conductor.start();
    // The killed gate's finding is open only until the next candidate's probe
    // passes, so capture it (and its candidate) the moment it appears.
    await waitFor(
      () => setup.conductor.state.phase.findings.some((f) => f.kind === "integration" && f.status === "open"),
      WAIT_MS,
      50,
      setup.runDir,
    );
    const finding = setup.conductor.state.phase.findings.find((f) => f.kind === "integration" && f.status === "open")!;
    assert.equal(finding.severity, "blocking");
    assert.equal(finding.raisedBy, "conductor");
    assert.match(finding.evidence, /killed at its limit/);
    // The gate ran for the finding's candidate (the unverifiable record was
    // not reused), and its own fresh record says it was killed.
    const C = finding.boundCandidateSha;
    const record = gateRecordAt(setup.runDir, C);
    assert.equal(record.candidateSha, C);
    assert.equal(record.reused, undefined, "an unverifiable record must not be reused");
    assert.equal(record.timedOut, true, "the gate must be killed at its limit");
    assert.equal(record.passed, false);
    assert.ok(record.durationMs < 10_000, `expected a prompt kill, took ${record.durationMs}ms`);
    assert.match(fs.readFileSync(path.join(runPaths(setup.runDir).checks, C, "gate.log"), "utf8"), /starting-gate/);
    assert.notEqual(setup.conductor.state.phase.phase, "DONE");
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    cleanupDir(setup.repo.dir);
    cleanupDir(dir);
  }
});
});
