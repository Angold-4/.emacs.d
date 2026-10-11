// Plan 06m (A1/A2/A3/A4): the owner works a run through `tt`, not through
// files. These tests drive the real conductor with fake-pi and the real CLI:
// a steer reaches every live lane worker by name, an owner input with a
// missing/foreign binding is refused with the reason, the status view leads
// with the current attempt and the trade-offs being decided, and a park runs
// the notify command once with the run, phase, reason and answering command.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { createRun } from "../../src/conductor.ts";
import { readLog } from "../../src/effects/log.ts";
import { renderStatusView, type StatusViewInput } from "../../src/render.ts";
import { notificationsPath, readNotifications } from "../../src/notify.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { seatRoundStates } from "../../src/core/seat-table.ts";
import type { LogRecord } from "../../src/effects/log.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  waitFor,
  type FakePiStep,
  type TestConductorSetup,
} from "./harness.ts";

const CLI_PATH = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

/** Runs the real CLI asynchronously: the conductor runs in THIS process, so a
 * synchronous `execFileSync` would block the inbox poll that applies the
 * command. */
function runCli(args: string[], env: NodeJS.ProcessEnv = {}): Promise<{ stdout: string; code: number }> {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], { env: { ...process.env, TT_NOTIFY_COMMAND: ":", ...env } });
    let stdout = "";
    child.stdout.on("data", (d) => (stdout += String(d)));
    child.on("close", (code) => resolve({ stdout, code: code ?? 0 }));
  });
}

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 40_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 10_000,
  reviewMs: 20_000,
  panelMs: 10_000,
  inboxPollMs: 40,
};

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
  cleanupDir(setup.repo.dir);
}

function lanePhase(workers: number) {
  return {
    id: "p1",
    goal: "build candidates from one base",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers,
  };
}

/** A lane worker that writes its file, stays alive long enough for a steer to
 * be delivered, then submits. */
function laneWorker(lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane}\n' > lane-${lane}.txt` },
      { kind: "call-sh", command: "sleep 8" },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

function laneReview(seat: Reviewer, contractVersion: unknown): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      {
        kind: "call-submit",
        tool: "submit_review",
        args: { reviewer: seat, phaseId: "p1", candidateSha: "$TT_CANDIDATE_SHA", contractVersion, correctionStatements: [], findingStatements: [], ballots: [], findings: [] },
      },
    ],
  };
}

function writeInbox(runDir: string, id: string, raw: unknown): void {
  const inbox = path.join(runDir, "inbox");
  fs.mkdirSync(inbox, { recursive: true });
  fs.writeFileSync(path.join(inbox, `${id}.json`), JSON.stringify(raw));
}

test("plan 06m: tt steer binds the run and phase, and prints each lane's delivery", async () => {
  const setup = await setupConductor({
    phase: lanePhase(2),
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [] }),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat) => ({ hello: { role: "picker", tools: ROLE_TOOLS.picker }, steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { seat, round: 1, winner: "a", loserHad: { lane: "b", what: "nothing" }, reason: "lane a" } }] }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => readEvents(setup.runDir).filter((r) => r.kind === "intent" && String(r.actionId).startsWith("lane-agent-")).length >= 2,
      30_000,
      50,
      setup.runDir,
    );
    const out = await runCli(["steer", setup.runDir, "cap the retry loop at 64", "--root", setup.runRoot]);
    assert.equal(out.code, 0, out.stdout);
    assert.match(out.stdout, /delivered to worker-a: delivered/);
    assert.match(out.stdout, /delivered to worker-b: delivered/);
    await waitFor(() => {
      const d = (setup.conductor.state.phase.ownerDirectives ?? []).find((x) => x.text === "cap the retry loop at 64");
      return Boolean(d && d.targets.includes("worker-a") && d.targets.includes("worker-b") && d.deliveries["worker-a"] === "delivered" && d.deliveries["worker-b"] === "delivered");
    }, 30_000, 50, setup.runDir);
  } finally {
    await teardown(setup);
  }
});

test("plan 06m: a steer during a two-lane attempt is delivered to worker-a and worker-b by name", async () => {
  const setup = await setupConductor({
    phase: lanePhase(2),
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [] }),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, candidate, state) => laneReview(seat as Reviewer, state.phase.contract.contractVersion),
    pickScriptFor: (seat) => ({ hello: { role: "picker", tools: ROLE_TOOLS.picker }, steps: [{ kind: "call-submit", tool: "submit_pick_vote", args: { seat, round: 1, winner: "a", loserHad: { lane: "b", what: "nothing" }, reason: "lane a" } }] }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(
      () => readEvents(setup.runDir).filter((r) => r.kind === "intent" && String(r.actionId).startsWith("lane-agent-")).length >= 2,
      30_000,
      50,
      setup.runDir,
    );
    const out = await runCli(["steer", setup.runDir, "both lanes get this", "--root", setup.runRoot]);
    assert.equal(out.code, 0, out.stdout);
    await waitFor(() => {
      const d = (setup.conductor.state.phase.ownerDirectives ?? []).find((x) => x.text === "both lanes get this");
      return Boolean(d && d.targets.includes("worker-a") && d.targets.includes("worker-b"));
    }, 30_000, 50, setup.runDir);
    const directive = (setup.conductor.state.phase.ownerDirectives ?? []).find((x) => x.text === "both lanes get this")!;
    assert.deepEqual([...directive.targets].sort(), ["worker-a", "worker-b"], "the recorded targets are worker-a and worker-b");
    await waitFor(
      () => directive.deliveries["worker-a"] === "delivered" && directive.deliveries["worker-b"] === "delivered",
      30_000,
      50,
      setup.runDir,
    );
  } finally {
    await teardown(setup);
  }
});

test("plan 06m: an owner input with a null runId or another phase's id is refused with the reason", async () => {
  const setup = await setupConductor({
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "sleep 20" }, { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }] }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    const internal = setup.conductor.state.phase.runId;
    const phaseId = setup.conductor.state.phase.phaseId;
    writeInbox(setup.runDir, "cmd-null-run", { type: "note", text: "null run", binding: { runId: "", phaseId } });
    writeInbox(setup.runDir, "cmd-missing-binding", { type: "note", text: "no binding" });
    writeInbox(setup.runDir, "cmd-other-run", { type: "note", text: "other run", binding: { runId: "no-such-run", phaseId } });
    writeInbox(setup.runDir, "cmd-other-phase", { type: "note", text: "other phase", binding: { runId: internal, phaseId: "p2" } });
    const rejected = path.join(setup.runDir, "inbox", "rejected");
    await waitFor(() => fs.existsSync(path.join(rejected, "cmd-other-phase.json")), 30_000, 50, setup.runDir);
    for (const id of ["cmd-null-run", "cmd-missing-binding", "cmd-other-run", "cmd-other-phase"]) {
      const reason = fs.readFileSync(path.join(rejected, `${id}.reason.txt`), "utf8");
      assert.ok(reason.trim().length > 0, `${id} has a reason`);
      const record = (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.id === id);
      assert.equal(record?.state, "refused", `${id} was recorded refused, never applied`);
    }
    assert.match(fs.readFileSync(path.join(rejected, "cmd-other-phase.reason.txt"), "utf8"), /bound to phase p2/);

    // Plan 06m (R2/A1): the CLI writer Emacs uses (`tt input`) keeps the
    // caller's binding — a command bound to another phase or run, or with a
    // null runId, is refused with the reason and printed, never rebound to
    // the run it was sent to.
    const scratch = fs.mkdtempSync(path.join(setup.runRoot, "input-"));
    const sendInput = async (name: string, raw: unknown) => {
      const file = path.join(scratch, `${name}.json`);
      fs.writeFileSync(file, JSON.stringify(raw));
      return runCli(["input", setup.runDir, file, "--root", setup.runRoot]);
    };
    const foreignPhase = await sendInput("foreign-phase", { type: "note", text: "cli other phase", binding: { runId: internal, phaseId: "p2" } });
    assert.equal(foreignPhase.code, 1, foreignPhase.stdout);
    assert.match(foreignPhase.stdout, /^note rejected: command is bound to phase p2, but this run is on phase /m);
    const foreignRun = await sendInput("foreign-run", { type: "note", text: "cli other run", binding: { runId: "run-B", phaseId } });
    assert.equal(foreignRun.code, 1, foreignRun.stdout);
    assert.match(foreignRun.stdout, /^note rejected: /m);
    const nullRun = await sendInput("null-run", { type: "note", text: "cli null run", binding: { runId: null, phaseId } });
    assert.equal(nullRun.code, 1, nullRun.stdout);
    assert.match(nullRun.stdout, /^note rejected: the binding is missing 'runId'/m);
    for (const text of ["cli other phase", "cli other run", "cli null run"]) {
      const record = (setup.conductor.state.phase.ownerInputs ?? []).find((i) => i.text === text);
      assert.ok(!record || record.state === "refused", `${text} is never applied`);
      assert.ok(!(setup.conductor.state.phase.ownerDirectives ?? []).some((d) => d.text === text), `${text} becomes no directive`);
    }
    // `tt note` (what Emacs's input box calls, text in an @file) keeps the
    // binding the caller passes too.
    const noteFile = path.join(scratch, "note.txt");
    fs.writeFileSync(noteFile, "cli note for another phase\n");
    const foreignNote = await runCli(["note", setup.runDir, `@${noteFile}`, "--run-id", internal, "--phase-id", "p2", "--root", setup.runRoot]);
    assert.equal(foreignNote.code, 1, foreignNote.stdout);
    assert.match(foreignNote.stdout, /^note rejected: command is bound to phase p2/m);
    assert.ok(!(setup.conductor.state.phase.ownerDirectives ?? []).some((d) => d.text === "cli note for another phase"));
    // A correctly bound command through the same writer is applied, and the
    // writer prints the recorded outcome (not just an id).
    const good = await sendInput("good", { type: "note", text: "cli bound note", binding: { runId: internal, phaseId } });
    assert.equal(good.code, 0, good.stdout);
    assert.match(good.stdout, /^note \S+ in run /m);
  } finally {
    await teardown(setup);
  }
});

test("plan 06m: tt withdraw OD-1 marks the directive withdrawn", async () => {
  const setup = await setupConductor({
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-sh", command: "sleep 20" }, { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }] }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    // The worker sleeps, so the phase stays live; a steer with no worker yet
    // is queued and still becomes a directive.
    const steer = await runCli(["steer", setup.runDir, "keep the cap at 64", "--root", setup.runRoot]);
    assert.equal(steer.code, 0, steer.stdout);
    await waitFor(() => (setup.conductor.state.phase.ownerDirectives ?? []).some((d) => d.text === "keep the cap at 64"), 20_000, 50, setup.runDir);
    const od = (setup.conductor.state.phase.ownerDirectives ?? []).find((d) => d.text === "keep the cap at 64")!;
    const w = await runCli(["withdraw", setup.runDir, od.id, "--root", setup.runRoot]);
    assert.equal(w.code, 0, w.stdout);
    assert.match(w.stdout, /withdrawn OD-/);
    await waitFor(() => (setup.conductor.state.phase.ownerDirectives ?? []).find((d) => d.id === od.id)?.status === "withdrawn", 20_000, 50, setup.runDir);
  } finally {
    await teardown(setup);
  }
});

test("plan 06m: tt verdict <finding> refuse settles the finding and creates nothing to answer", async () => {
  const setup = await setupConductor({
    stubReviews: false,
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }] }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => ({
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
            ballots: [],
            findings: reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "the guard is missing" }] : [],
          },
        },
      ],
    }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => (setup.conductor.state.phase.findings ?? []).some((f) => f.status === "open"), 90_000, 50, setup.runDir);
    const finding = (setup.conductor.state.phase.findings ?? []).find((f) => f.status === "open")!;
    // The blocking finding is on the review as a blocker message.
    const linked = () => (setup.conductor.state.phase.messages ?? []).filter((m) => m.sourceRecordId === finding.id);
    await waitFor(() => linked().length > 0, 30_000, 50, setup.runDir);
    // M-94: the message is PUBLISHED (a live blocker on the review) when the
    // owner refuses — the case whose settlement used to be dropped.
    await waitFor(() => linked().some((m) => m.state === "published"), 30_000, 50, setup.runDir);
    const before = (setup.conductor.state.phase.findings ?? []).length;
    fs.writeFileSync(path.join(setup.runDir, "conductor.pid"), String(process.pid));
    const out = await runCli(["verdict", setup.runDir, finding.id, "refuse", "--reason", "not a code defect", "--root", setup.runRoot]);
    assert.equal(out.code, 0, out.stdout);
    assert.match(out.stdout, /refused finding/);
    await waitFor(() => (setup.conductor.state.phase.findings ?? []).find((f) => f.id === finding.id)?.status === "disproved", 20_000, 50, setup.runDir);
    assert.equal((setup.conductor.state.phase.findings ?? []).length, before, "no new finding is created");
    // Plan 06m (R2, M-94): the refusal settles the linked blocker message, so
    // review.org and the status list exactly the blockers that remain — none.
    const live = new Set(["raw", "published", "refused"]);
    await waitFor(() => linked().every((m) => !live.has(m.state)), 20_000, 50, setup.runDir);
    for (const m of linked()) assert.ok(!live.has(m.state), `message ${m.id} is settled (${m.state})`);
    assert.ok(
      (setup.conductor.state.phase.findings ?? []).every((f) => !(f.status === "open" && f.severity === "blocking")),
      "no open blocking finding is left behind",
    );
    await waitFor(() => {
      const review = fs.readFileSync(path.join(setup.runDir, "views", "review.org"), "utf8");
      const blockers = review.slice(review.indexOf("* Blockers"), review.indexOf("\n* ", review.indexOf("* Blockers") + 1));
      return /\(none\)/.test(blockers) && !blockers.includes(finding.id) && linked().every((m) => !blockers.includes(m.id));
    }, 20_000, 50, setup.runDir);
    const statusOut = await runCli(["status", setup.runDir, "--root", setup.runRoot]);
    assert.doesNotMatch(statusOut.stdout, /^Open blockers/m, "the status lists no open blocker");
  } finally {
    await teardown(setup);
  }
});

test("plan 06m: the status view shows this attempt's time, never a cumulative overrun, and trade-offs with titles", async () => {
  // The renderer's own top block: a third attempt with an attempt clock, a
  // carried item, a deferral and a trade-off, each with a title.
  const phase = {
    phaseId: "p1",
    phase: "IMPLEMENTING",
    runId: "run-1",
    attempt: { n: 3 },
    repairRoundsUsed: 2,
    repairRoundsGranted: 3,
    carriedItems: ["F-1"],
    carriedTo: { "F-1": "06n" },
    deferrals: [{ id: "DEF-1", itemId: "F-2", text: "a later hardening", status: "open", test: "the guard test" }],
    findings: [
      { id: "F-1", evidence: "the guard is missing", status: "open", severity: "advisory" },
      { id: "F-2", evidence: "the cap is wrong", status: "open", severity: "blocking" },
    ],
    decisions: [{ id: "D-1", choice: "cap the retry loop at 64" }],
    ballots: [
      { reviewer: "M", decisionId: "D-1", vote: "approve" },
      { reviewer: "A", decisionId: "D-1", vote: "reject" },
      { reviewer: "B", decisionId: "D-1", vote: "approve" },
    ],
    messages: [],
    // A phase-wide item check result: never a lane's first failing test.
    checkResolution: [{ id: "R3", outcome: "failed", name: "a phase-wide item test" }],
    rounds: [
      {
        round: 3,
        base: "base123",
        lanes: ["a", "b"],
        candidates: [
          { lane: "a", sha: "1234567abcdef", ok: true, reviews: [{ seat: "M", review: {} }] },
          { lane: "b", sha: "89abcde123456", ok: false, note: "the check `make check` failed: the failing test, another test" },
        ],
        picked: { lane: "a", sha: "1234567abcdef", votes: 2 },
        votes: [{ seat: "M", lane: "a" }],
      },
    ],
    contract: { seats: ["M", "A", "B"] },
  } as unknown as Record<string, unknown>;
  const view = {
    stage: "implement",
    elapsed: "1h02m",
    loop: "▶ IMPLEMENT 40m00s of 45m",
    pipeline: "implement#3 40m00s… (5m00s left)",
    gates: "pending (round 3)",
    round: 3,
    reviewLine: "M ✗ 1 blocking   A ✓   B ⧗",
    lanes: ["a a 1234567 checks ✓ — picked (2 votes)", "b b 89abcde checks ✗", "round 3 from base: 2 worker run(s), 6 review(s)"],
    laneElapsed: { a: "3m00s", b: "5m00s" },
    reviewers: [
      { reviewer: "M", state: "objects", approvals: 1, rejections: 1, blocking: 1, advisory: 0, label: "M ✗ 1 blocking" },
      { reviewer: "A", state: "approves", approvals: 2, rejections: 0, blocking: 0, advisory: 1, label: "A ✓ 1 advisory" },
      { reviewer: "B", state: "pending", approvals: 0, rejections: 0, blocking: 0, advisory: 0, label: "B ⧗" },
    ],
    // Cumulative counters from earlier rounds: they must not label THIS
    // round (A seat's timeout two rounds ago is not a timeout now).
    seatTable: {
      seats: [
        { seat: "M", model: "opus", reprompts: 1 },
        { seat: "A", model: "gpt", turnMinutes: { p50: 10, max: 25 }, timeouts: 3 },
        { seat: "B", model: "gemini", timeouts: 1 },
      ],
    },
    // This round's lifecycle, from the seats' own log records.
    seatRound: [
      { seat: "M", state: "review submitted", incidents: ["re-prompted"] },
      { seat: "A", state: "discovery submitted", incidents: [] },
      { seat: "B", state: "timed out", incidents: ["timed out"] },
    ],
    tradeoffs: [{ kind: "veto", text: "vetoed by M: D-1 cap the retry loop at 64", recordId: "D-1", title: "cap the retry loop at 64", seats: "vetoed by M" }],
    cost: { text: "3 rounds · 62m total", rounds: 3, totalMinutes: 62, stageMinutes: [], ownerWaitMinutes: 0 },
    envTools: [],
    metricsLine: "metrics 3 rounds · review 20%",
  };
  const input = {
    runDir: "/tmp/tt-06m-status",
    title: "cap the loop",
    phase,
    alive: true,
    view,
    ownerInputs: [],
    pendingOwnerInputs: [],
    ownerDirectives: [],
  } as unknown as StatusViewInput;
  const text = renderStatusView(input);
  assert.match(text, /^stage\s+implement · round 3 · attempt 3 · repairs 2\/3/m, "the master panel's now-line");
  assert.match(text, /^loop\s+▶ IMPLEMENT 40m00s of 45m$/m, "the attempt clock is in the master panel");
  assert.doesNotMatch(text, /over by/i, "no cumulative overrun line");
  // The status panel shows both lane columns, the three seat rows and the
  // trade-off with per-seat ballot state (OD-1).
  assert.match(text, /lane a/);
  assert.match(text, /lane b/);
  assert.match(text, /checks fail 89abcde/, "lane b's state and short sha");
  assert.match(text, /3m00s/, "lane a's own elapsed time");
  assert.match(text, /5m00s/, "lane b's own elapsed time");
  assert.match(text, /first fail: the failing test/, "lane b's first failing test");
  assert.match(text, /trade-offs open 1/, "the gauge counts undecided decisions");
  // Plan 06m (OD-1/A3): each seat's state is its lifecycle THIS round, not
  // its verdict: M submitted a blocking review (it is not "rejected"), after
  // a re-prompt this round; A has only submitted its discovery; B timed out.
  assert.match(text, /^  M  opus  ! review submitted \(re-prompted\)  blocking 1 · advisory 0/m, "the M seat row");
  assert.match(text, /^  A  gpt  … discovery submitted  blocking 0 · advisory 1/m, "the A seat row: no cumulative timeout");
  assert.match(text, /^  B  gemini  ✗ timed out  blocking 0/m, "the B seat row");
  assert.doesNotMatch(text, /rejected/, "a seat's verdict is never shown as its lifecycle state");
  assert.match(text, /^seats\s+M ! · A … · B ✗$/m, "one health glyph per seat in the master panel");
  assert.doesNotMatch(text, /a phase-wide item test/, "a lane's first failing test is its own check's, not the phase's");
  assert.match(text, /M approve · A reject · B approve/, "per-seat ballot state");
  assert.match(text, /cap the retry loop at 64/, "the trade-off has a title");
  assert.match(text, /^Open blockers \(1\)$/m);
  assert.match(text, /F-2 the cap is wrong/, "the blocker has a title");
  assert.match(text, /F-1 → 06n — the guard is missing/, "the carried item has a title");
  assert.match(text, /F-2 \(test the guard test\) — a later hardening/, "the deferral has a title");
  assert.match(text, /^Details$/m);
  // The seat lifecycle comes from the seats' own log records in the CURRENT
  // round: an earlier round's timeout does not carry over.
  let seq = 0;
  const rec = (kind: string, event: unknown, actionId?: string): LogRecord => ({ seq: ++seq, ts: new Date(1_700_000_000_000 + seq * 1000).toISOString(), kind, event, ...(actionId ? { actionId } : {}) });
  const records: LogRecord[] = [
    rec("event", { type: "ROUND_STARTED", round: 1, base: "b", lanes: ["a", "b"] }),
    rec("intent", { seat: "B", candidateSha: "c1" }, "lane-review-1"),
    rec("event", { type: "REVIEW_TIMED_OUT", reviewer: "B" }),
    rec("review_reprompt", { reviewer: "M", agentId: "x" }),
    rec("event", { type: "ROUND_STARTED", round: 2, base: "b", lanes: ["a", "b"] }),
    rec("intent", { seat: "M", candidateSha: "c2" }, "lane-review-2"),
    rec("intent", { seat: "A", candidateSha: "c2" }, "lane-review-3"),
    rec("intent", { seat: "B", candidateSha: "c2" }, "lane-review-4"),
    rec("discovery_submitted", { reviewer: "M", agentId: "m", count: 0, lane: "a", round: 2 }),
    rec("discovery_submitted", { reviewer: "A", agentId: "a", count: 0, lane: "a", round: 2 }),
    rec("review_reprompt", { reviewer: "A", agentId: "a" }),
    rec("event", { type: "ROUND_REVIEW_SUBMITTED", round: 2, lane: "a", seat: "M", review: { reviewer: "M", findings: [{ severity: "blocking" }] } }),
    rec("lane_review_retried", { round: 2, lane: "a", seat: "B", reason: "crashed" }),
  ];
  assert.deepEqual(seatRoundStates(records, ["M", "A", "B"]), [
    { seat: "M", state: "review submitted", incidents: [] },
    { seat: "A", state: "re-prompted", incidents: ["re-prompted"] },
    { seat: "B", state: "retried", incidents: ["retried"] },
  ], "this round's lifecycle per seat; round 1's timeout and re-prompt do not carry over");

  // The master panel (OD-1) is at most 6 gauge lines, before the first blank.
  const afterRun = text.split("\n").slice(3);
  const master: string[] = [];
  for (const line of afterRun) {
    if (line.trim() === "") break;
    master.push(line);
  }
  assert.ok(master.length <= 6, `master panel is at most 6 lines (got ${master.length}):\n${master.join("\n")}`);

  // `tt status` renders the SAME text as views/status.txt, live from the
  // same renderer (no cached file): a DONE run's projection is fixed.
  const setup = await setupConductor({
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }] }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => ({
      hello: defaultReviewerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_review",
          args: { reviewer, phaseId: state.phase.phaseId, candidateSha: state.phase.candidate?.sha, contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [] },
        },
      ],
    }),
    deadlines: FAST,
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    // Parity holds while the run is still live: `tt status` refreshes the
    // projection with the same bytes it prints (A-29).
    const live = await runCli(["status", setup.runDir, "--root", setup.runRoot]);
    assert.equal(live.stdout, fs.readFileSync(path.join(setup.runDir, "views", "status.txt"), "utf8"), "live tt status equals views/status.txt");
    await setup.conductor.stop();
    const out = await runCli(["status", setup.runDir, "--root", setup.runRoot]);
    assert.equal(out.stdout, fs.readFileSync(path.join(setup.runDir, "views", "status.txt"), "utf8"), "tt status renders the same text as views/status.txt");
    // The program status prints the same dashboard for its running node.
    const progDir = path.join(setup.runRoot, "programs", "prog");
    fs.mkdirSync(progDir, { recursive: true });
    const program = {
      title: "prog",
      maxParallel: 1,
      entries: [{ id: "a", after: [], plan: { title: "p", repo: setup.repo.dir, integrationBranch: "main", checks: ["true"], phases: [{ id: "p1", goal: "g", acceptance: ["it works"], checks: ["true"], boundaries: [], reserved: [] }] } }],
    };
    fs.writeFileSync(path.join(progDir, "program.json"), JSON.stringify(program));
    fs.writeFileSync(
      path.join(progDir, "events.jsonl"),
      `${JSON.stringify({ ts: new Date().toISOString(), event: { type: "NODE_STARTED", node: "a", runId: path.basename(setup.runDir), branch: "main--a", base: "main" } })}\n${JSON.stringify({ ts: new Date().toISOString(), event: { type: "NODE_STATUS", node: "a", status: "running" } })}\n`,
    );
    const progOut = await runCli(["program", "status", "prog", "--root", setup.runRoot]);
    assert.match(progOut.stdout, /^stage\s+/m, "the node's master panel is shown");
    assert.match(progOut.stdout, /^Seats$/m, `the node's status panel is shown:\n${progOut.stdout}`);
    const progMaster: string[] = [];
    const progLines = progOut.stdout.split("\n");
    const runLine = progLines.findIndex((l) => /^run .* · conductor/.test(l));
    for (const line of progLines.slice(runLine + 2)) {
      if (line.trim() === "") break;
      progMaster.push(line);
    }
    assert.ok(progMaster.length > 0 && progMaster.length <= 6, `program status master panel is at most 6 lines (got ${progMaster.length})`);
  } finally {
    await teardown(setup);
  }
});

test("plan 06m: a run that parks for the owner runs the notify command once with the answering command", async () => {
  const callsDir = fs.mkdtempSync("/tmp/tt-06m-notify-");
  const calls = path.join(callsDir, "calls.log");
  const setup = await setupConductor({
    checks: ["false"],
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }] }),
    reviewerScriptFor: (reviewer: Reviewer, state: State) => ({
      hello: defaultReviewerHello(),
      steps: [
        {
          kind: "call-submit",
          tool: "submit_review",
          args: { reviewer, phaseId: state.phase.phaseId, candidateSha: state.phase.candidate?.sha, contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [] },
        },
      ],
    }),
    deadlines: FAST,
  });
  const previous = process.env.TT_NOTIFY_COMMAND;
  // The configured command receives the run, phase, reason and the answering
  // command as environment variables (F-A-6).
  process.env.TT_NOTIFY_COMMAND = `printf '%s|%s|%s|%s\\n' "$TT_NOTIFY_RUN" "$TT_NOTIFY_PHASE" "$TT_NOTIFY_REASON" "$TT_NOTIFY_ANSWER" >> ${calls}`;
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 50, setup.runDir);
    await waitFor(() => readNotifications(setup.runRoot).length >= 1, 30_000, 50, setup.runDir);
    const record = readNotifications(setup.runRoot)[0];
    assert.equal(record.id, path.basename(setup.runDir), "the run is named");
    assert.equal(record.phase, "p1", "the phase is named");
    assert.ok(record.reason.length > 0, "the reason is named");
    assert.ok(record.command && record.command.length > 0, "the answering command is named");
    const lines = fs.readFileSync(calls, "utf8").trim().split("\n");
    assert.equal(lines.length, 1, "the notify command ran exactly once");
    const [run, phase, why, answer] = lines[0].split("|");
    assert.equal(run, path.basename(setup.runDir), "the notify command receives the run");
    assert.equal(phase, "p1", "the notify command receives the phase");
    assert.ok(why && why.length > 0, "the notify command receives the reason");
    assert.ok(answer && answer.length > 0, "the notify command receives the answering command");
    assert.ok(fs.existsSync(notificationsPath(setup.runRoot)));
  } finally {
    if (previous === undefined) delete process.env.TT_NOTIFY_COMMAND;
    else process.env.TT_NOTIFY_COMMAND = previous;
    await teardown(setup);
    cleanupDir(callsDir);
  }
});
