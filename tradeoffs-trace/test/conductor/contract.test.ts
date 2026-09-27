// Contract v1 end-to-end: a fake-pi run with two decisions and one finding
// produces `messages.jsonl` and `ledger.jsonl` that pass `tt contract check`;
// deleting the projections and running `tt contract rebuild` restores
// identical bytes; and a late `tt verdict … refuse` after the run reached
// DONE (and its daemon exited) appends the event to `events.jsonl` and shows
// up in the ledger as a follow-up — with no reducer shortcut: the CLI path
// itself is what is exercised.

import assert from "node:assert/strict";
import { execFileSync, spawn, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { cleanupDir, defaultReviewerHello, defaultWorkerHello, FAKE_PI_PATH, readEvents, setupConductor, waitFor } from "./harness.ts";
import { Conductor, runPaths } from "../../src/conductor.ts";

const CLI = fileURLToPath(new URL("../../src/cli.ts", import.meta.url));

function tt(args: string[]): string {
  return execFileSync(process.execPath, [CLI, ...args], { encoding: "utf8" });
}

/** `tt` that does not throw on a non-zero exit, so a rejected verdict's
 * stdout and status can be asserted (A-13). */
function ttResult(args: string[]): { status: number; stdout: string; stderr: string } {
  const r = spawnSync(process.execPath, [CLI, ...args], { encoding: "utf8" });
  return { status: r.status ?? 1, stdout: r.stdout ?? "", stderr: r.stderr ?? "" };
}

/** `tt` that runs while the test's event loop keeps turning, so an
 * in-process conductor can process the inbox while `tt verdict` waits for the
 * outcome (A-13). */
function ttAsync(args: string[]): Promise<{ status: number; stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [CLI, ...args]);
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (c) => (stdout += c.toString()));
    child.stderr.on("data", (c) => (stderr += c.toString()));
    child.once("error", reject);
    child.once("exit", (code) => resolve({ status: code ?? 1, stdout, stderr }));
  });
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
    // Plan 04a: a message is published only once the evaluator has run at
    // EVALUATING. M raises a blocking finding so the phase parks in a repair
    // round, and the second attempt hangs — the run stays live with the
    // message published, long enough for the owner's verdict to arrive.
    workerScriptForAttempt: (attempt) =>
      attempt === 1
        ? {
            hello: defaultWorkerHello(),
            steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: DECISIONS, assumptions: [], deviations: [] } }],
          }
        : { hello: defaultWorkerHello(), steps: [{ kind: "hang-until-abort" }] },
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
            findings: reviewer === "M" ? [{ kind: "defect", severity: "blocking", evidence: "the loop does not terminate on empty input" }] : [],
          },
        },
      ],
    }),
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 30_000 },
  });

  try {
    await setup.conductor.start();
    await waitFor(
      () => (setup.conductor.state.phase.messages ?? []).some((m) => m.type === "tradeoff" && m.state === "published"),
      90_000,
      20,
      setup.runDir,
    );
    const message = setup.conductor.state.phase.messages!.find((m) => m.type === "tradeoff" && m.state === "published")!;
    const inbox = `${setup.runDir}/inbox`;
    mkdirSync(inbox, { recursive: true });
    // The in-process Conductor writes no pid file (only the detached
    // `tt __run-conductor` does), so fake it: `tt verdict` then sees a live
    // run and must go through the inbox rather than appending the event.
    writeFileSync(`${setup.runDir}/conductor.pid`, String(process.pid));
    // A-13: the CLI reports the outcome; the conductor processes the inbox
    // while the CLI waits, so the verdict is reported applied.
    const accepted = await ttAsync(["verdict", setup.runDir, message.id, "refuse", "--reason", "not the trade-off the goal needed"]);
    assert.equal(accepted.status, 0, accepted.stderr);
    assert.match(accepted.stdout, /verdict applied: refuse recorded/);
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
      "a refusal before DONE must raise an owner blocking finding",
    );

    // A stale verdict, sent through `tt verdict`'s own binding override, is
    // rejected into inbox/rejected, and the CLI reports the reason (A-13).
    const stale = await ttAsync([
      "verdict",
      setup.runDir,
      message.id,
      "accept",
      "--candidate-sha",
      "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
    ]);
    assert.equal(stale.status, 1, "a rejected verdict must exit non-zero");
    assert.match(stale.stdout, /verdict rejected: .*candidate/);
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
  const deadlines = { abortGraceMs: 100, termGraceMs: 100, helloTimeoutMs: 5_000, reviewMs: 30_000, inboxPollMs: 100 };
  // A-27: capture the repair attempt's prompt (and the reviewers') so the
  // ledger-reaching-prompts criterion has a conductor-level proof.
  const promptDir = mkdtempSync("/tmp/tt-carried-prompts-");
  const workerPromptLog = `${promptDir}/worker.log`;
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'attempt ${attempt}\n' > attempt.txt` },
        // Plan 04a: hold the repair attempt briefly, so the owner's verdict on
        // the published message is applied before the next freeze carries it.
        // The inbox poll is 100 ms here and the graces are short, so 2 s is
        // ample; the old 4 s guess was below this machine's pipeline cost
        // under the full suite's own load (the wait then timed out).
        ...(attempt === 1 ? [] : [{ kind: "sleep", ms: 2000 }]),
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
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: workerPromptLog },
    extraReviewerEnv: (reviewer) => ({ FAKE_PI_PROMPT_LOG: `${promptDir}/${reviewer}.log` }),
    deadlines,
  });

  try {
    await setup.conductor.start();
    // Publish attempt 1's trade-off (the evaluator publishes it at
    // EVALUATING), settle it with an owner accept through the inbox, then let
    // the repair change the decision it came from.
    await waitFor(
      () => (setup.conductor.state.phase.messages ?? []).some((m) => m.type === "tradeoff" && m.state === "published"),
      90_000,
      20,
      setup.runDir,
    );
    const published = setup.conductor.state.phase.messages!.find((m) => m.type === "tradeoff" && m.state === "published")!;
    priorId = setup.conductor.state.phase.decisions.find((d) => d.source === "worker")!.id;
    writeFileSync(`${setup.runDir}/conductor.pid`, String(process.pid));
    const accepted = await ttAsync(["verdict", setup.runDir, published.id, "accept"]);
    assert.equal(accepted.status, 0, accepted.stderr);
    assert.match(accepted.stdout, /verdict applied: accept recorded/);
    await waitFor(
      () => (setup.conductor.state.phase.messages ?? []).some((m) => m.id === published.id && m.state === "accepted"),
      30_000,
      20,
      setup.runDir,
    );

    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 150_000, 50, setup.runDir);
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

    // A-27: the repair attempt's worker prompt and a reviewer's later turn-2
    // prompt carry the settled ledger.
    const attempts = readFileSync(workerPromptLog, "utf8")
      .split("\n=====\n")
      .filter((a) => a.trim().length > 0);
    assert.ok(attempts.length >= 2, "expected a repair attempt's prompt");
    assert.match(attempts[attempts.length - 1], /Settled \(do not re-raise\)/);
    assert.match(attempts[attempts.length - 1], new RegExp(`${published.id} \\[tradeoff\\]`));
    for (const r of ["M", "A", "B"]) {
      const prompts = readFileSync(`${promptDir}/${r}.log`, "utf8");
      assert.match(prompts, /Settled \(do not re-raise\)/, `${r}'s turn-2 prompt must carry the ledger`);
    }
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
    rmSync(promptDir, { recursive: true, force: true });
  }
});

test("a withdrawn decision supersedes its message and keeps the settlement marked", async () => {
  let priorId = "";
  const deadlines = { abortGraceMs: 100, termGraceMs: 100, helloTimeoutMs: 5_000, reviewMs: 30_000, inboxPollMs: 100 };
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScriptForAttempt: (attempt) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: `printf 'attempt ${attempt}\\n' > attempt.txt` },
        // Plan 04a: hold the repair attempt briefly, so the owner's verdict on
        // the published message is applied before the next freeze
        // carries/supersedes it. The inbox poll is 100 ms here and the graces
        // are short, so 2 s is ample; the old 4 s guess was below this
        // machine's pipeline cost under the full suite's own load.
        ...(attempt === 1 ? [] : [{ kind: "sleep", ms: 2000 }]),
        attempt === 1
          ? { kind: "call-submit", tool: "submit_phase", args: { decisions: DECISIONS.slice(0, 1), assumptions: [], deviations: [] } }
          : { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [], priorDecisions: [{ id: priorId, status: "withdrawn" }] } },
      ],
    }),
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
    await waitFor(
      () => (setup.conductor.state.phase.messages ?? []).some((m) => m.type === "tradeoff" && m.state === "published"),
      90_000,
      20,
      setup.runDir,
    );
    const published = setup.conductor.state.phase.messages!.find((m) => m.type === "tradeoff" && m.state === "published")!;
    priorId = setup.conductor.state.phase.decisions.find((d) => d.source === "worker")!.id;
    writeFileSync(`${setup.runDir}/conductor.pid`, String(process.pid));
    const accepted = await ttAsync(["verdict", setup.runDir, published.id, "accept"]);
    assert.equal(accepted.status, 0, accepted.stderr);
    assert.match(accepted.stdout, /verdict applied: accept recorded/);
    await waitFor(
      () => (setup.conductor.state.phase.messages ?? []).some((m) => m.id === published.id && m.state === "accepted"),
      30_000,
      20,
      setup.runDir,
    );

    await waitFor(() => setup.conductor.state.phase.phase === "DONE" || setup.conductor.state.phase.phase === "BLOCKED", 150_000, 50, setup.runDir);
    assert.equal(setup.conductor.state.phase.phase, "DONE");
    const types = readEvents(setup.runDir)
      .filter((r) => r.kind === "event")
      .map((r) => (r.event as { type: string }).type);
    assert.ok(types.includes("MESSAGE_SUPERSEDED"), "a withdrawn decision must supersede its message");
    const message = setup.conductor.state.phase.messages!.find((m) => m.id === published.id)!;
    assert.equal(message.state, "superseded");
    assert.equal(message.settlement?.settledBy, "owner");
    assert.match(message.supersededBy ?? "", /superseded/);
    const entry = readFileSync(runPaths(setup.runDir).ledger, "utf8")
      .trim()
      .split("\n")
      .filter(Boolean)
      .map((l) => JSON.parse(l))
      .find((e: { messageId: string }) => e.messageId === published.id)!;
    assert.equal(entry.state, "accepted");
    assert.equal(entry.settledBy, "owner");
    assert.match(entry.supersededBy ?? "", /superseded/);
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

    // B-12/A-14: a file for a message id state no longer has is a mismatch,
    // and rebuild prunes it (the projection is state, not an accumulation).
    writeFileSync(`${p.messagesView}/OLD-99.org`, "* stale\n");
    const staleCheck = ttResult(["contract", "check", setup.runDir]);
    assert.equal(staleCheck.status, 1, "an extra message file must fail the check");
    assert.match(staleCheck.stderr, /views\/messages\/OLD-99\.org/);
    tt(["contract", "rebuild", setup.runDir]);
    assert.ok(!existsSync(`${p.messagesView}/OLD-99.org`), "rebuild must prune the stale file");
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

    // A-13 (exited path): a stale late verdict prints its reason on stdout and
    // exits non-zero, and the event is never written.
    const other = tradeoffs[1] as { id: string };
    const staleLate = ttResult(["verdict", setup.runDir, other.id, "accept", "--candidate-sha", "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"]);
    assert.equal(staleLate.status, 1, "a stale late verdict must exit non-zero");
    assert.match(staleLate.stdout, /verdict rejected: .*candidate/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});

test("a run writes views/review.org and views/status.txt, and a new message updates both", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    workerScript: () => ({
      hello: defaultWorkerHello(),
      steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: DECISIONS, assumptions: [], deviations: [] } }],
    }),
    // The reviewers raise one advisory finding while the phase is REVIEWING,
    // after a pause, so the views are written once for the trade-offs and
    // again when the finding is added (a message is added).
    reviewerScriptFor: (reviewer, state) => ({
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        ...(reviewer === "M" ? [{ kind: "sleep" as const, ms: 1500 }] : []),
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
                ? [{ kind: "defect", severity: "advisory", evidence: "src/sum.ts:9 a slow path" }]
                : [],
          },
        },
      ],
    }),
    deadlines: { abortGraceMs: 500, termGraceMs: 500, helloTimeoutMs: 5_000, reviewMs: 10_000 },
  });

  try {
    await setup.conductor.start();
    const p = runPaths(setup.runDir);
    // The trade-offs are raised at freeze; the views exist before any finding.
    await waitFor(() => (setup.conductor.state.phase.messages ?? []).filter((m) => m.type === "tradeoff").length >= 2, 90_000, 20, setup.runDir);
    await waitFor(() => existsSync(p.review) && existsSync(p.status), 30_000, 20, setup.runDir);
    // Plan 03c: the same beat writes the phase chart from TRANSITIONS.
    await waitFor(() => existsSync(p.loop), 30_000, 20, setup.runDir);
    assert.match(readFileSync(p.loop, "utf8"), /^tradeoffs-trace phase chart — generated from TRANSITIONS/);
    assert.match(readFileSync(p.loop, "utf8"), /IMPLEMENTING|REVIEWING|CHECKING/);
    const reviewBefore = readFileSync(p.review, "utf8");
    assert.match(reviewBefore, /T-1/);
    assert.match(reviewBefore, /^\* Trade-offs$/m);
    const statusBefore = readFileSync(p.status, "utf8");
    assert.match(statusBefore, /^run /m);
    assert.match(statusBefore, /^phase /m);

    // A finding message is added; both views are rewritten from the new state
    // (the status view is coalesced, so it is waited for).
    await waitFor(() => (setup.conductor.state.phase.messages ?? []).some((m) => m.type === "finding"), 90_000, 20, setup.runDir);
    await waitFor(() => readFileSync(p.review, "utf8") !== reviewBefore, 30_000, 20, setup.runDir);
    const reviewAfter = readFileSync(p.review, "utf8");
    assert.match(reviewAfter, /F-1/);
    assert.match(reviewAfter, /^\* Findings$/m);
    assert.ok(existsSync(`${p.messagesView}/F-1.org`), "a message file is written for the new message");
    assert.match(readFileSync(`${p.messagesView}/F-1.org`, "utf8"), /^\* Evidence$/m);
    // Plan 04c join: wait until the status reflects the published finding —
    // the review outcome alone changes the file, and asserting the joined
    // behaviour on that intermediate write is a race. The finding is an open
    // advisory, so the plan 01h trade-offs panel now carries its record line.
    await waitFor(() => {
      const s = readFileSync(p.status, "utf8");
      return s !== statusBefore && /\t:RECORD:F-/.test(s);
    }, 30_000, 20, setup.runDir);
    // The status view is the buffer's own text: it carries the trade-offs and,
    // on each trade-off line, the record marker that keeps plan 01h's RET.
    const statusAfter = readFileSync(p.status, "utf8");
    assert.match(statusAfter, /^Trade-offs \(\d+\)$/m);
    assert.match(statusAfter, /\t:RECORD:[A-Za-z][-A-Za-z0-9]*/);
    // Plan 04c: the same beat writes the balance metrics line.
    assert.match(statusAfter, /^metrics   /m);
    assert.ok(existsSync(p.metrics), "the balance metrics projection is written");
    assert.match(readFileSync(p.metrics, "utf8"), /"reviewShare"/);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.scriptsDir);
  }
});
