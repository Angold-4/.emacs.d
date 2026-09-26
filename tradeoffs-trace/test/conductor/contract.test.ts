// Contract v1 end-to-end: a fake-pi run with two decisions and one finding
// produces `messages.jsonl` and `ledger.jsonl` that pass `tt contract check`;
// deleting the projections and running `tt contract rebuild` restores
// identical bytes; and a late `tt verdict … refuse` after the run reached
// DONE (and its daemon exited) appends the event to `events.jsonl` and shows
// up in the ledger as a follow-up — with no reducer shortcut: the CLI path
// itself is what is exercised.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, FAKE_PI_PATH, readEvents, setupConductor, waitFor } from "./harness.ts";
import { Conductor, runPaths } from "../../src/conductor.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function tt(args: string[]): string {
  return execFileSync(process.execPath, [CLI, ...args], { encoding: "utf8" });
}

const DECISIONS = [
  {
    choice: "Use a simple loop rather than a library helper",
    whyItMatters: "Keeps the change dependency-free, matching the plan's zero-dependency goal",
    alternatives: [{ option: "pull in a small utility library", consequence: "adds a dependency for one function" }],
    recommendation: { choice: "keep the loop", reason: "no dependency needed for something this small" },
    classProposal: "detail",
  },
  {
    choice: "Keep the existing file layout",
    whyItMatters: "A layout change would make the diff harder to review",
    alternatives: [{ option: "reorganize files", consequence: "a noisier diff for no behavioral gain" }],
    recommendation: { choice: "keep the existing layout", reason: "smallest reviewable diff" },
    classProposal: "detail",
  },
];

test("a verdict on a live run goes through the inbox", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: DECISIONS, assumptions: [], deviations: [] } }],
    }),
    // Reviewers hang, so the phase stays in REVIEWING long enough for the
    // owner's verdict to arrive through the inbox.
    reviewerScriptFor: () => ({
      hello: defaultReviewerHello(),
      steps: [{ kind: "hang-until-abort" }],
    }),
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 30_000 },
  });

  try {
    await setup.conductor.start();
    await waitFor(
      () => setup.conductor.state.phase.phase === "REVIEWING" && (setup.conductor.state.phase.messages ?? []).length >= 1,
      90_000,
      20,
      setup.runDir,
    );
    const message = setup.conductor.state.phase.messages![0];
    const inbox = `${setup.runDir}/inbox`;
    mkdirSync(inbox, { recursive: true });
    // The in-process Conductor writes no pid file (only the detached
    // `tt __run-conductor` does), so fake it: `tt verdict` then sees a live
    // run and must go through the inbox rather than appending the event.
    writeFileSync(`${setup.runDir}/conductor.pid`, String(process.pid));
    assert.match(
      tt(["verdict", setup.runDir, message.id, "refuse", "--reason", "not the trade-off the goal needed"]),
      /queued verdict/,
    );
    await waitFor(
      () => (setup.conductor.state.phase.messages ?? []).some((m) => m.id === message.id && m.state === "refused"),
      30_000,
      20,
      setup.runDir,
    );
    const refused = setup.conductor.state.phase.messages!.find((m) => m.id === message.id)!;
    assert.equal(refused.state, "refused");
    assert.equal(refused.settlement?.settledBy, "owner");
    assert.ok(
      setup.conductor.state.phase.findings.some((f) => f.raisedBy === "owner" && f.severity === "blocking"),
      "a refusal during REVIEWING must raise an owner blocking finding",
    );

    // A stale verdict is rejected into inbox/rejected with its reason.
    writeFileSync(
      `${inbox}/verdict-stale.json`,
      JSON.stringify({
        type: "verdict",
        verdict: "accept",
        binding: {
          runId: setup.conductor.state.phase.runId,
          phaseId: setup.conductor.state.phase.phaseId,
          candidateSha: "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
          contractVersion: message.boundContractVersion,
          recordId: message.id,
          recordVersion: message.messageVersion,
        },
      }),
    );
    await waitFor(() => existsSync(`${runPaths(setup.runDir).inboxRejected}/verdict-stale.json`), 30_000, 20, setup.runDir);
    const reason = readdirSync(runPaths(setup.runDir).inboxRejected)
      .filter((n) => n.startsWith("verdict-stale") && n.endsWith(".reason.txt"))
      .map((n) => readFileSync(`${runPaths(setup.runDir).inboxRejected}/${n}`, "utf8"))
      .join("\n");
    assert.match(reason, /candidate/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("a conductor killed before its projection write rebuilds them on start", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: DECISIONS, assumptions: [], deviations: [] } }],
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
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    await setup.conductor.stop();
    const p = runPaths(setup.runDir);
    const expectedMessages = readFileSync(p.messages, "utf8");
    const expectedLedger = readFileSync(p.ledger, "utf8");

    // Simulate a kill between an event and its projection write: both files
    // vanish. The log is authoritative, so the next start restores them.
    rmSync(p.messages);
    rmSync(p.ledger);
    const restarted = new Conductor({
      runDir: setup.runDir,
      plan: setup.plan,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      stubReviews: true,
      deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
    });
    await restarted.start();
    try {
      assert.ok(existsSync(p.messages), "a restart must rebuild messages.jsonl");
      assert.ok(existsSync(p.ledger), "a restart must rebuild ledger.jsonl");
      assert.equal(readFileSync(p.messages, "utf8"), expectedMessages);
      assert.equal(readFileSync(p.ledger, "utf8"), expectedLedger);
    } finally {
      await restarted.stop();
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("MESSAGE_CARRIED is emitted per live message at every freeze", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: () => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'attempt %s\n' "$RANDOM" > attempt.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: DECISIONS.slice(0, 1), assumptions: [], deviations: [] } },
      ],
    }),
    // M raises a blocking finding on the first candidate; after the repair,
    // M confirms it repaired on the new candidate, forcing a second freeze.
    reviewerScriptFor: (reviewer, state) => {
      const open = state.phase.findings.find((f) => f.raisedBy === "M" && f.status === "open");
      const repaired = open && open.boundCandidateSha !== state.phase.candidate?.sha;
      return {
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
              findingStatements: reviewer === "M" && repaired ? [{ findingId: open!.id, status: "confirm" }] : [],
              ballots: [],
              findings:
                reviewer === "M" && !open
                  ? [{ kind: "defect", severity: "blocking", evidence: "the loop does not terminate on empty input" }]
                  : [],
            },
          },
        ],
      };
    },
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
  });

  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 120_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    assert.equal(setup.conductor.state.phase.round, 2, "the run must have frozen two candidates");
    const types = readEvents(setup.runDir)
      .filter((r) => r.kind === "event")
      .map((r) => (r.event as { type: string }).type);
    assert.ok(types.includes("MESSAGE_CARRIED"), `a second freeze must emit MESSAGE_CARRIED; events=${JSON.stringify(types)}`);
    const message = setup.conductor.state.phase.messages!.find((m) => m.type === "tradeoff")!;
    assert.ok(message.messageVersion >= 2, `the carried trade-off must have bumped its version, got ${message.messageVersion}`);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("contract v1 projections pass check, rebuild identically, and record a late verdict", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: DECISIONS, assumptions: [], deviations: [] } }],
    }),
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
            ballots: [],
            findings:
              reviewer === "M"
                ? [{ kind: "defect", severity: "advisory", evidence: "worth double-checking the loop terminates", reproduction: { command: "true" } }]
                : [],
          },
        },
      ],
    }),
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
  });

  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 120_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    await setup.conductor.stop();

    const p = runPaths(setup.runDir);
    assert.ok(existsSync(p.messages), "messages.jsonl must be written");
    assert.ok(existsSync(p.ledger), "ledger.jsonl must be written");
    const messages = readFileSync(p.messages, "utf8").trim().split("\n").filter(Boolean).map((l) => JSON.parse(l));
    const tradeoffs = messages.filter((m: { type: string }) => m.type === "tradeoff");
    const findings = messages.filter((m: { type: string }) => m.type === "finding" || m.type === "blocker");
    assert.equal(tradeoffs.length, 2, `expected two trade-off messages, got ${JSON.stringify(messages)}`);
    assert.equal(findings.length, 1, `expected one finding message, got ${JSON.stringify(messages)}`);

    // The projections match state byte for byte.
    const before = { messages: readFileSync(p.messages, "utf8"), ledger: readFileSync(p.ledger, "utf8") };
    assert.match(tt(["contract", "check", setup.runDir]), /contract check ok/);

    // Deleting them and rebuilding restores identical bytes.
    rmSync(p.messages);
    rmSync(p.ledger);
    tt(["contract", "rebuild", setup.runDir]);
    assert.equal(readFileSync(p.messages, "utf8"), before.messages);
    assert.equal(readFileSync(p.ledger, "utf8"), before.ledger);
    assert.match(tt(["contract", "check", setup.runDir]), /contract check ok/);

    // A late verdict: the daemon has exited (the in-process conductor wrote no
    // pid file), so `tt verdict` must append the event itself.
    const target = tradeoffs[0] as { id: string };
    assert.match(tt(["verdict", setup.runDir, target.id, "refuse", "--reason", "the goal needed per-request latency"]), /recorded refuse/);
    const ledger = readFileSync(p.ledger, "utf8").trim().split("\n").filter(Boolean).map((l) => JSON.parse(l));
    const entry = ledger.find((e: { messageId: string }) => e.messageId === target.id);
    assert.ok(entry, "the refused message must appear in the ledger");
    assert.equal(entry.state, "refused");
    assert.equal(entry.settledBy, "owner");
    assert.equal(entry.followUp, true, "the ledger must show the follow-up");
    assert.match(JSON.stringify(messages), /tradeoff/);
    assert.match(tt(["contract", "check", setup.runDir]), /contract check ok/);
    // The review view and `tt summary` show the follow-up too.
    assert.match(readFileSync(p.review, "utf8"), /FOLLOW_UP: true/);
    assert.match(tt(["summary", setup.runDir]), /Follow-ups/);
    assert.match(tt(["summary", setup.runDir]), new RegExp(target.id));
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
