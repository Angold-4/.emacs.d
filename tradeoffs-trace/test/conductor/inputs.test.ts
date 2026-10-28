// Plan 07f (R4/R5/R7): owner input is a ruling, not a correction.
// A ruling applies at once (no queued correction, no forced loop), it settles
// a record of any kind (a decision or a contract finding), and it is read in
// every later reducer brief.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { rulingLines, directiveLines } from "../../src/conductor.ts";
import { accept, acceptanceBlockers } from "../../src/core/predicate.ts";
import type { Reviewer } from "../../src/core/types.ts";
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

/** Runs the real CLI while the conductor runs in this process, so its inbox
 * poll can apply the command (a synchronous spawn would block that poll). */
function runCli(args: string[], input?: string): Promise<{ stdout: string; code: number }> {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [CLI_PATH, ...args], {
      env: { ...process.env, TT_NOTIFY_COMMAND: ":" },
    });
    let stdout = "";
    child.stdout.on("data", (d) => (stdout += String(d)));
    child.stderr.on("data", (d) => (stdout += String(d)));
    child.on("close", (code) => resolve({ stdout, code: code ?? 0 }));
    if (input !== undefined) child.stdin.end(input);
    else child.stdin.end();
  });
}

const FAST = {
  abortGraceMs: 300,
  termGraceMs: 300,
  helloTimeoutMs: 10_000,
  workerAttemptMs: 30_000,
  checkMs: 20_000,
  probeMs: 20_000,
  freezeMs: 20_000,
  evaluateMs: 15_000,
  reviewMs: 20_000,
  panelMs: 10_000,
  inboxPollMs: 100,
};

const WRITE = "mkdir -p src/core && printf 'interface Round { n: number }\\n' > src/core/rounds.ts";

const ITEM = (id: string) => ({ id, title: `${id} proves it`, text: `${id} proves it`, arch: [], verify: ["review"] });
const COVERAGE = (id: string) => ({
  items: [{ id, status: "done", where: ["src/core/rounds.ts:1"], tests: [] }],
  arch: [],
});

function disclosure(choice: string) {
  return {
    choice,
    whyItMatters: "The plan left this choice open and callers depend on the answer",
    alternatives: [{ option: "the other obvious behaviour", consequence: "would surprise callers" }],
    recommendation: { choice, reason: "keeps the contract's own promise" },
    classProposal: "delegated",
  };
}

function workerWithDecision(id: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: WRITE },
      { kind: "call-submit", tool: "submit_coverage", args: COVERAGE(id) },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [disclosure("batch cancels per tick")], assumptions: [], deviations: [] } },
    ],
  };
}

function reviewWithBallot(id: string, vote: "approve" | "reject", itemsMet = true) {
  return (reviewer: Reviewer, state: { phase: { phaseId: string; candidate?: { sha: string }; contract: { contractVersion: unknown }; decisions: Array<{ id: string }> } }) => {
    const decisionId = state.phase.decisions.find((d) => d.source !== "reviewer-discovered")?.id ?? state.phase.decisions[0]?.id;
    return {
      hello: defaultReviewerHello(),
      steps: [
        { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
        { kind: "wait-for-prompt" },
        // Keep the seats in review long enough that the owner's ruling really
        // lands while they review (the R4 case), not after their veto.
        { kind: "sleep", ms: 2_000 },
        { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
        { kind: "call-tool", tool: "read", args: { path: "README.md" } },
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
            ballots: decisionId ? [{ decisionId, vote, rationale: "reviewed it", evidence: ["src/core/rounds.ts:1"] }] : [],
            items: [{ id, verdict: itemsMet ? "met" : "unmet", evidence: "src/core/rounds.ts:1 README.md:1" }],
          },
        },
      ],
    };
  };
}

async function teardown(setup: TestConductorSetup): Promise<void> {
  await setup.conductor.stop();
  cleanupDir(setup.runRoot);
  cleanupDir(setup.scriptsDir);
}

async function sendRuling(runArg: string, runDir: string, recordId: string, text: string): Promise<string> {
  const state = await import("../../src/conductor.ts").then((m) => m.rebuildState(runDir, JSON.parse(fs.readFileSync(path.join(runDir, "plan", "v1.json"), "utf8")) as never, { lenient: true }));
  const binding = { runId: state.phase.runId, phaseId: state.phase.phaseId };
  const command = { type: "ruling", text, recordIds: [recordId], binding };
  const file = path.join(runDir, `ruling-${Date.now()}.json`);
  fs.writeFileSync(file, JSON.stringify(command));
  try {
    const out = await runCli(["input", runArg, file], "");
    return out.stdout;
  } finally {
    fs.rmSync(file, { force: true });
  }
}

async function sendResolve(runArg: string, runDir: string, requestId: string, option: string, note?: string): Promise<string> {
  const conductor = await import("../../src/conductor.ts");
  const state = conductor.rebuildState(runDir, JSON.parse(fs.readFileSync(path.join(runDir, "plan", "v1.json"), "utf8")) as never, { lenient: true });
  const request = state.phase.ownerRequests.find((r) => r.id === requestId)!;
  const command = {
    type: "resolve",
    recordKind: "request",
    option,
    ...(note ? { note } : {}),
    binding: {
      runId: state.phase.runId,
      phaseId: state.phase.phaseId,
      candidateSha: state.phase.candidate?.sha,
      contractVersion: state.phase.contract.contractVersion,
      recordId: request.id,
      recordVersion: request.version,
    },
  };
  const file = path.join(runDir, `resolve-${Date.now()}.json`);
  fs.writeFileSync(file, JSON.stringify(command));
  try {
    const out = await runCli(["input", runArg, file], "");
    return out.stdout;
  } finally {
    fs.rmSync(file, { force: true });
  }
}

// -- R4: a ruling during review is applied at once, never queued ------------

test("a ruling sent while the seats review is applied at once and forces no extra round", async () => {
  const promptLog = path.join(fs.mkdtempSync("/tmp/tt-07f-rule-prompt-"), "prompts.log");
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")], finalChecks: ["sleep 2"] } as never,
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    extraEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => workerWithDecision("R1"),
    reviewerScriptFor: reviewWithBallot("R1", "reject"),
  } as never);
  try {
    await setup.conductor.start();
    // The decision exists once the candidate freezes. The phase is REVIEWING.
    await waitFor(
      () => setup.conductor.state.phase.phase === "REVIEWING" && setup.conductor.state.phase.decisions.length > 0,
      60_000,
      20,
      setup.runDir,
    );
    const decision = setup.conductor.state.phase.decisions[0];
    const rulingText = `Rule ${decision.id} as recorded: batch cancels per tick is the behaviour.`;
    const out = await sendRuling(setup.runDir, setup.runDir, decision.id, rulingText);
    assert.match(out, /ruling applied/, `the ruling applies through tt input: ${out}`);
    await waitFor(
      () => setup.conductor.state.phase.rulings?.some((r) => r.text === rulingText) === true,
      20_000,
      20,
      setup.runDir,
    );
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);

    const phase = setup.conductor.state.phase;
    assert.deepEqual(phase.rulings?.map((r) => r.recordIds), [[decision.id]], "the ruling names the decision");
    assert.equal(phase.queuedCorrections?.length ?? 0, 0, "a ruling never queues a correction");
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "REPAIR_ATTEMPT_STARTED").length,
      0,
      "the phase was accepted in the same round, with no repair attempt",
    );
    assert.equal(
      readEvents(setup.runDir).filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "RESOLVING_INCOMPLETE").length,
      0,
      "no extra resolving round was forced",
    );
    const prompt = fs.existsSync(promptLog) ? fs.readFileSync(promptLog, "utf8") : "";
    // The ruling is in the next reducer brief (the evaluator, when it ran)
    // and in the exported prompt section every later brief carries.
    assert.ok(rulingLines(phase.rulings).some((line) => line.includes(rulingText)), "the ruling line names the text");
    if (prompt.includes("You are the evaluator")) assert.ok(prompt.includes(rulingText), "the evaluator brief carries the ruling");
  } finally {
    await teardown(setup);
  }
});

// -- R5: a contract finding the owner accepts with a ruling no longer blocks -

test("the owner accepts a contract finding with one ruling", async () => {
  const setup = await setupConductor({
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => workerWithDecisionNoBallot(),
    reviewerScriptFor: (reviewer: Reviewer, state: { phase: { phaseId: string; candidate?: { sha: string }; contract: { contractVersion: unknown }; decisions: Array<{ id: string }> } }) => {
      const decisionId = state.phase.decisions[0]?.id;
      if (reviewer === "B") {
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
                findingStatements: [],
                findings: [{ kind: "contract", severity: "blocking", evidence: "the contract says nothing about this case" }],
              },
            },
          ],
        };
      }
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
              findingStatements: [],
              ballots: decisionId ? [{ decisionId, vote: "approve", rationale: "fine", evidence: ["src/core/rounds.ts:1"] }] : [],
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.kind === "contract" && f.status === "open"), 60_000, 20, setup.runDir);
    const finding = setup.conductor.state.phase.findings.find((f) => f.kind === "contract" && f.status === "open")!;
    assert.ok(
      acceptanceBlockers(setup.conductor.state.phase).some((b) => b.kind === "finding"),
      `the open contract finding is an acceptance blocker before the ruling: ${JSON.stringify(acceptanceBlockers(setup.conductor.state.phase))}`,
    );
    const text = "Accept the contract finding as recorded; the plan text already covers this case.";
    const out = await sendRuling(setup.runDir, setup.runDir, finding.id, text);
    assert.match(out, /ruling applied/, out);
    await waitFor(
      () => setup.conductor.state.phase.findings.find((f) => f.id === finding.id)?.status === "accepted",
      20_000,
      20,
      setup.runDir,
    );
    assert.ok(
      !acceptanceBlockers(setup.conductor.state.phase).some((b) => b.kind === "finding"),
      "the accepted finding is no longer a blocker",
    );
    assert.ok(rulingLines(setup.conductor.state.phase.rulings).some((l) => l.includes(text)), "the ruling is in every later reducer brief");
  } finally {
    await teardown(setup);
  }
});

function workerWithDecisionNoBallot(): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: WRITE },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

// -- R7: a withdrawn directive reaches every live fixer at once -------------

const FIX_DOMAINS = [
  { name: "booking", globs: ["booking/**"] },
  { name: "gates", globs: ["gates/**"] },
];

function fixPhase() {
  return {
    id: "p1",
    goal: "fix the points",
    acceptance: ["it works"],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    workers: 2,
    domains: FIX_DOMAINS,
    fixRounds: "domains",
    fixers: 4,
    fixChecks: "true",
  };
}

function laneWorker(round: number, lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane} r${round}\\n' > lane-${lane}-r${round}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

function laneReview(seat: Reviewer, contractVersion: unknown, findings: unknown[] = []): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultReviewerHello(),
    steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
      { kind: "wait-for-prompt" },
      {
        kind: "call-submit",
        tool: "submit_review",
        args: {
          reviewer: seat,
          phaseId: "p1",
          candidateSha: "$TT_CANDIDATE_SHA",
          contractVersion,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings,
        },
      },
    ],
  };
}

function reviewerWithFixPoints() {
  return (seat: Reviewer, _candidate: string, state: { phase: { contract: { contractVersion: unknown }; rounds?: unknown[] } }): { hello: unknown; steps: FakePiStep[] } => {
    const K = state.phase.contract.contractVersion;
    const round = state.phase.rounds?.length ?? 1;
    if (round === 1 && seat === "M") {
      return laneReview("M", K, [
        {
          kind: "defect",
          severity: "blocking",
          evidence: "the booking path is wrong",
          domain: "booking",
          item: "R1",
          where: "booking/a.ts:1 fn",
          expected: "BOOKING_EXPECTED",
          doneWhen: "test -f booking-fixed.txt",
        },
        {
          kind: "defect",
          severity: "blocking",
          evidence: "the gates path is wrong",
          domain: "gates",
          item: "R2",
          where: "gates/b.ts:1 fn",
          expected: "GATES_EXPECTED",
          doneWhen: "test -f gates-fixed.txt",
        },
      ]);
    }
    return laneReview(seat, K);
  };
}

function pickVote(seat: Reviewer, state: { phase: { rounds?: unknown[] } }): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [
      {
        kind: "call-submit",
        tool: "submit_pick_vote",
        args: {
          inspected: ["a", "b", "c", "d"],
          round: state.phase.rounds?.length ?? 1,
          seat,
          lane: "a",
          why: `${seat} picks a`,
          loserHad: { yes: false, anchors: [] },
        },
      },
    ],
  };
}

test("a withdrawn directive is delivered to every live fixer session at once", async () => {
  const promptDir = fs.mkdtempSync("/tmp/tt-07f-withdraw-prompt-");
  const promptLog = path.join(promptDir, "prompts.log");
  const setup = await setupConductor({
    phase: fixPhase(),
    checks: ["true"],
    phaseChecks: ["true"],
    stubReviews: true,
    deadlines: FAST,
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    workerScript: () => laneWorker(1, "a"),
    laneWorkerScriptFor: (lane: string, round: number) => laneWorker(round, lane),
    laneReviewerScriptFor: (seat: Reviewer, _candidate: string, state: { phase: { contract: { contractVersion: unknown }; rounds?: unknown[] } }) =>
      reviewerWithFixPoints()(seat, _candidate, state),
    pickScriptFor: (seat: string, state: { phase: { rounds?: unknown[] } }) => pickVote(seat as Reviewer, state),
    fixerScriptFor: (domain: string) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "sleep", ms: 4_000 },
        { kind: "call-sh", command: `printf '${domain} fixed\\n' > ${domain}-fixed.txt` },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  } as never);
  try {
    await setup.conductor.start();
    // Both fixers are live and have their own briefs before the owner speaks.
    await waitFor(() => {
      try {
        const prompts = fs.readFileSync(promptLog, "utf8");
        return prompts.includes("You are fixer fix-booking") && prompts.includes("You are fixer fix-gates");
      } catch {
        return false;
      }
    }, 180_000, 50, setup.runDir);

    const steerText = "R7: keep the booking and gates fixes scoped to their own files.";
    const steered = await runCli(["steer", setup.runDir, steerText], "");
    assert.equal(steered.code, 0, steered.stdout);
    await waitFor(
      () => setup.conductor.state.phase.ownerDirectives?.some((d) => d.text === steerText) === true,
      20_000,
      20,
      setup.runDir,
    );
    const directive = setup.conductor.state.phase.ownerDirectives!.find((d) => d.text === steerText)!;

    const withdrawn = await runCli(["withdraw", setup.runDir, directive.id], "");
    assert.equal(withdrawn.code, 0, withdrawn.stdout);
    await waitFor(
      () => setup.conductor.state.phase.ownerDirectives?.find((d) => d.id === directive.id)?.status === "withdrawn",
      20_000,
      20,
      setup.runDir,
    );
    const atWithdrawal = fs.statSync(promptLog).size;

    // The one withdrawal poll steers every live fixer: two withdraw intents,
    // one per fixer session, recorded together rather than on later rounds.
    await waitFor(
      () =>
        readEvents(setup.runDir).filter(
          (r) => r.kind === "intent" && (r.event as { withdrawn?: boolean; directiveId?: string }).withdrawn === true && (r.event as { directiveId?: string }).directiveId === directive.id,
        ).length === 2,
      20_000,
      20,
      setup.runDir,
    );
    const intents = readEvents(setup.runDir).filter(
      (r) =>
        r.kind === "intent" &&
        (r.event as { withdrawn?: boolean; directiveId?: string }).withdrawn === true &&
        (r.event as { directiveId?: string }).directiveId === directive.id,
    );
    const ids = new Set(intents.map((r) => (r.event as { agentId: string }).agentId));
    assert.equal(ids.size, 2, "one intent per live fixer session");
    assert.ok([...ids].every((id) => id.startsWith("fix-")), `the targets are the fixers: ${JSON.stringify([...ids])}`);
    const times = intents.map((r) => Date.parse(r.ts));
    assert.ok(Math.max(...times) - Math.min(...times) < 1_000, "both withdrawal steers landed in the same poll");

    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 180_000, 50, setup.runDir);
    // Every later brief is built from `directiveLines`: the withdrawn one is
    // no longer in it (and any prompt text written after withdrawal omits it).
    const later = fs.readFileSync(promptLog, "utf8").slice(atWithdrawal);
    assert.ok(!directiveLines(setup.conductor.state.phase.ownerDirectives).some((line) => line.includes(steerText)), "a later brief's directive section omits the withdrawn text");
    assert.ok(!later.includes(steerText), "the withdrawn directive is omitted from every prompt written after withdrawal");
  } finally {
    await teardown(setup);
    cleanupDir(promptDir);
  }
});

// -- R7: resolve + ruling together clear the blockers and accept ------------

test("plan 07f (R1/R7): accepting the failed vote settles it and the phase accepts that round", async () => {
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")] } as never,
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScript: () => workerWithDecision("R1"),
    reviewerScriptFor: (reviewer: Reviewer, state: { phase: { phaseId: string; candidate?: { sha: string }; contract: { contractVersion: unknown }; decisions: Array<{ id: string }> } }) => {
      const decisionId = state.phase.decisions[0]?.id;
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          { kind: "call-tool", tool: "read", args: { path: "README.md" } },
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
              ballots: decisionId ? [{ decisionId, vote: "reject", rationale: "not this way", evidence: ["src/core/rounds.ts:1"] }] : [],
              items: [{ id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1 README.md:1" }],
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 20, setup.runDir);
    const before = setup.conductor.state.phase;
    const request = before.ownerRequests.find((r) => r.status === "open")!;
    const kinds = acceptanceBlockers(before).map((b) => b.kind);
    assert.ok(kinds.includes("decision"), `the failed vote is a decision blocker: ${JSON.stringify(kinds)}`);
    assert.ok(kinds.includes("owner-request"), `its request is a blocker too: ${JSON.stringify(kinds)}`);

    // `accept_as_implemented` settles the failed vote on THIS candidate; it
    // does not ask for a repair. That leaves no blocker and the phase accepts
    // in the same round.
    const resolved = await sendResolve(setup.runDir, setup.runDir, request.id, "accept_as_implemented");
    assert.match(resolved, /resolve applied/, resolved);
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const after = setup.conductor.state.phase;
    assert.equal(after.phase, "DONE", "the phase is accepted in that round, not left AWAITING_OWNER");
    assert.ok(!after.ownerRequests.some((r) => r.id === request.id && r.status === "open"), "the request is resolved");
    assert.ok(
      after.ownerRequests.some((r) => r.id === request.id && r.resolution?.option === "accept_as_implemented"),
      "the owner's accept_as_implemented option settled the decision",
    );
    assert.equal(after.repairRoundsUsed, before.repairRoundsUsed, "settling the failed vote started no repair round");
    assert.deepEqual(acceptanceBlockers(after), [], "no acceptance blocker is left after the resolve");
  } finally {
    await teardown(setup);
  }
});

// Plan 07f1b (C1): the R7 test above used to prove the other half of the same
// gate — an owner `resolve` plus an owner `ruling` together clear the failed
// vote's blockers — and `accept_as_implemented` must not displace that
// coverage. Both commands are exercised here, in the order the owner sends
// them: the resolve (which asks for a repair) closes the request, and the
// ruling settles the decision; the ONE predicate then agrees.
test("plan 07f (R1/R7): tt input resolves an owner request and rules a decision, and with no blocker left accept() is true", async () => {
  // The failed vote's retries resubmit the same tree, so the round parks as
  // today (07f F2). The repair round the owner's `resolve` then grants must
  // change the tree, or its resubmission is refused and the run parks again.
  // The decision is disclosed once, on the first attempt, so no repair round
  // re-discloses it (a new candidate would get a new decision id).
  const setup = await setupConductor({
    items: { requirements: [ITEM("R1")] } as never,
    checks: ["true"],
    stubReviews: false,
    deadlines: FAST,
    workerScriptForAttempt: (attempt, { state }) => ({
      hello: defaultWorkerHello(),
      steps: [
        { kind: "call-sh", command: WRITE },
        ...(state.phase.ownerRequests.some((r) => r.resolution?.option === "reject_and_repair")
          ? [{ kind: "call-sh" as const, command: `printf 'attempt ${attempt}\\n' > attempt.txt` }]
          : []),
        { kind: "call-submit", tool: "submit_coverage", args: COVERAGE("R1") },
        {
          kind: "call-submit",
          tool: "submit_phase",
          args: { decisions: attempt === 1 ? [disclosure("batch cancels per tick")] : [], assumptions: [], deviations: [] },
        },
      ],
    }),
    reviewerScriptFor: (reviewer: Reviewer, state: { phase: { phaseId: string; candidate?: { sha: string }; contract: { contractVersion: unknown }; decisions: Array<{ id: string }> } }) => {
      const decisionId = state.phase.decisions[0]?.id;
      return {
        hello: defaultReviewerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } },
          { kind: "wait-for-prompt" },
          { kind: "call-tool", tool: "read", args: { path: "src/core/rounds.ts" } },
          { kind: "call-tool", tool: "read", args: { path: "README.md" } },
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
              ballots: decisionId ? [{ decisionId, vote: "reject", rationale: "not this way", evidence: ["src/core/rounds.ts:1"] }] : [],
              items: [{ id: "R1", verdict: "met", evidence: "src/core/rounds.ts:1 README.md:1" }],
            },
          },
        ],
      };
    },
  });
  try {
    await setup.conductor.start();
    await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 90_000, 20, setup.runDir);
    const before = setup.conductor.state.phase;
    const request = before.ownerRequests.find((r) => r.status === "open")!;
    const decision = before.decisions.find((d) => d.class !== "detail")!;
    const kinds = acceptanceBlockers(before).map((b) => b.kind);
    assert.ok(kinds.includes("decision"), `the failed vote is a decision blocker: ${JSON.stringify(kinds)}`);
    assert.ok(kinds.includes("owner-request"), `its request is a blocker too: ${JSON.stringify(kinds)}`);

    const resolved = await sendResolve(setup.runDir, setup.runDir, request.id, "reject_and_repair");
    assert.match(resolved, /resolve applied/, resolved);
    const ruled = await sendRuling(setup.runDir, setup.runDir, decision.id, "Rule the decision as recorded; the recorded choice stands.");
    assert.match(ruled, /ruling applied/, ruled);
    // Plan 07f1b (C1): this test asserts no acceptance blocker is left, so it
    // must wait for the phase to be DONE — never for DONE *or* AWAITING_OWNER,
    // which would let a parked run pass the assertion below.
    await waitFor(() => setup.conductor.state.phase.phase === "DONE", 90_000, 20, setup.runDir);
    const after = setup.conductor.state.phase;
    assert.equal(after.phase, "DONE", "the resolve and the ruling leave the phase accepted, never parked");
    // The repair started by the resolve may bring a new candidate; either way
    // both owner commands are applied, and the ONE predicate ACCEPTS: no
    // blocker of the failed vote is left and `accept()` is true for the
    // candidate.
    assert.ok(!after.ownerRequests.some((r) => r.id === request.id && r.status === "open"), "the request is resolved");
    assert.ok(after.rulings?.some((r) => r.recordIds.includes(decision.id)), "the decision is ruled");
    assert.deepEqual(
      acceptanceBlockers(after).filter((b) => b.kind === "decision" || b.kind === "owner-request"),
      [],
      "neither the failed vote nor its request blocks acceptance any more",
    );
    assert.deepEqual(acceptanceBlockers(after), [], "no acceptance blocker is left after the resolve and the ruling");
    assert.equal(
      accept(after, after.candidate!.sha, after.contract.contractVersion),
      true,
      "with no blocker left accept() is true",
    );
  } finally {
    await teardown(setup);
  }
});
