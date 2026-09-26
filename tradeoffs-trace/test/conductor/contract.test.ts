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

    // A stale verdict, sent through `tt verdict`'s own binding override, is
    // rejected into inbox/rejected with its reason (not hand-written JSON).
    assert.match(
      tt(["verdict", setup.runDir, message.id, "accept", "--candidate-sha", "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"]),
      /queued verdict/,
    );
    await waitFor(
      () => readdirSync(runPaths(setup.runDir).inboxRejected).some((n) => n.endsWith(".reason.txt")),
      30_000,
      20,
      setup.runDir,
    );
    const reason = readdirSync(runPaths(setup.runDir).inboxRejected)
      .filter((n) => n.endsWith(".reason.txt"))
      .map((n) => readFileSync(`${runPaths(setup.runDir).inboxRejected}/${n}`, "utf8"))
      .join("\n");
    assert.match(reason, /candidate/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("a verdict file left in the inbox before exit is applied on the next start", async () => {
  const deadlines = { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 };
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
    deadlines,
  });

  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 50, setup.runDir);
    await setup.conductor.stop();
    const message = setup.conductor.state.phase.messages!.find((m) => m.state === "published")!;
    // The owner left the verdict file in the inbox; the daemon then exited.
    const inbox = `${setup.runDir}/inbox`;
    mkdirSync(inbox, { recursive: true });
    writeFileSync(
      `${inbox}/verdict-left.json`,
      JSON.stringify({
        type: "verdict",
        verdict: "refuse",
        reason: "worth revisiting after the run",
        binding: {
          runId: setup.conductor.state.phase.runId,
          phaseId: setup.conductor.state.phase.phaseId,
          candidateSha: message.boundCandidateSha,
          contractVersion: message.boundContractVersion,
          recordId: message.id,
          recordVersion: message.messageVersion,
        },
      }),
    );
    // The next start scans the inbox and applies it — the conductor's own
    // inbox path, not a reducer shortcut.
    const restarted = new Conductor({
      runDir: setup.runDir,
      plan: setup.plan,
      piCommand: process.execPath,
      piArgsPrefix: [FAKE_PI_PATH],
      stubReviews: true,
      deadlines,
    });
    await restarted.start();
    try {
      await waitFor(
        () => (restarted.state.phase.messages ?? []).some((m) => m.id === message.id && m.state === "refused"),
        30_000,
        20,
        setup.runDir,
      );
      const refused = restarted.state.phase.messages!.find((m) => m.id === message.id)!;
      assert.equal(refused.followUp, true);
      const p = runPaths(setup.runDir);
      assert.match(readFileSync(p.ledger, "utf8"), /"followUp":true/);
      assert.match(readFileSync(p.review, "utf8"), /FOLLOW_UP: true/);
      assert.match(tt(["summary", setup.runDir]), /Follow-ups/);
    } finally {
      await restarted.stop();
    }
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

test("MESSAGE_CARRIED is emitted per live message, and a changed decision invalidates its settlement", async () => {
  let priorId = "";
  const deadlines = { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 };
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'attempt ${attempt}\n' > attempt.txt` },
        attempt === 1
          ? { kind: "call-submit", tool: "submit_phase", args: { decisions: DECISIONS.slice(0, 1), assumptions: [], deviations: [] } }
          : {
              kind: "call-submit",
              tool: "submit_phase",
              args: {
                decisions: [],
                assumptions: [],
                deviations: [],
                priorDecisions: [
                  {
                    id: priorId,
                    status: "changed",
                    choice: "A rewritten choice that names the cost",
                    whyItMatters: "the earlier wording hid the cost from the owner",
                    alternatives: [{ option: "keep the old wording", consequence: "the trade-off stays misreported" }],
                    recommendation: { choice: "use the new wording", reason: "it names the cost" },
                  },
                ],
              },
            },
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
    deadlines,
  });

  try {
    await setup.conductor.start();
    // Publish attempt 1's trade-off, settle it with an owner accept through
    // the inbox, then let the repair change the decision it came from.
    await waitFor(
      () => setup.conductor.state.phase.phase === "REVIEWING" && (setup.conductor.state.phase.messages ?? []).length >= 1,
      90_000,
      20,
      setup.runDir,
    );
    const published = setup.conductor.state.phase.messages!.find((m) => m.type === "tradeoff" && m.state === "published")!;
    priorId = setup.conductor.state.phase.decisions.find((d) => d.source === "worker")!.id;
    writeFileSync(`${setup.runDir}/conductor.pid`, String(process.pid));
    assert.match(tt(["verdict", setup.runDir, published.id, "accept"]), /queued verdict/);
    await waitFor(
      () => (setup.conductor.state.phase.messages ?? []).some((m) => m.id === published.id && m.state === "accepted"),
      30_000,
      20,
      setup.runDir,
    );

    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 120_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    assert.equal(setup.conductor.state.phase.round, 2, "the run must have frozen two candidates");
    const carries = readEvents(setup.runDir).filter(
      (r) => r.kind === "event" && (r.event as { type: string }).type === "MESSAGE_CARRIED",
    );
    assert.ok(carries.length >= 1, "a second freeze must emit MESSAGE_CARRIED");
    const changedCarry = carries.find(
      (r) =>
        (r.event as { messageId: string }).messageId === published.id && (r.event as { unchanged: boolean }).unchanged === false,
    );
    assert.ok(changedCarry, "the changed decision must be carried as unchanged:false");
    assert.ok((changedCarry!.event as { content?: unknown }).content, "a changed carry must carry the new content");

    const message = setup.conductor.state.phase.messages!.find((m) => m.id === published.id)!;
    assert.ok(message.messageVersion >= 2, `the carried trade-off must have bumped its version, got ${message.messageVersion}`);
    assert.equal(message.invalidated?.reason, "content changed");
    assert.equal(message.title, "A rewritten choice that names the cost");
    // The ledger keeps who settled it and the bindings it was settled under.
    const ledger = readFileSync(runPaths(setup.runDir).ledger, "utf8")
      .trim()
      .split("\n")
      .filter(Boolean)
      .map((l) => JSON.parse(l));
    const entry = ledger.find((e: { messageId: string }) => e.messageId === published.id)!;
    assert.equal(entry.settledBy, "owner");
    assert.equal(entry.invalidated?.reason, "content changed");
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
