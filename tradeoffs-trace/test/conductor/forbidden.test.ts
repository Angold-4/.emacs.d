// 07g1 acceptance: #+TT_FORBIDDEN is refused by the mapper's shell tool and
// recorded. The command would create a marker file; if it ran, the marker
// would exist. 07g1b (R9): the exact case is a FIXER running the corpus by
// hand — `cargo test -- --include-ignored` — refused with the rule's text,
// recorded, and never run (exex 01: five corpus runs, 3-16 minutes each).

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import { buildContract } from "../../src/conductor.ts";
import { readEvents } from "./harness.ts";
import { cleanupDir, defaultReviewerHello, defaultWorkerHello, setupConductor, waitFor, type FakePiStep } from "./harness.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import type { Reviewer, State } from "../../src/core/types.ts";

function laneWorker(lane: string): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: defaultWorkerHello(),
    steps: [
      { kind: "call-sh", command: `printf 'lane ${lane}\n' > lane-${lane}.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ],
  };
}

/** M raises one point in domain x on round 1, so the phase has fixer work. */
function reviewerWithPoint(seat: Reviewer, state: State): { hello: unknown; steps: FakePiStep[] } {
  const K = state.phase.contract.contractVersion;
  const round = state.phase.rounds?.length ?? 1;
  const findings =
    round === 1 && seat === "M"
      ? [
          {
            kind: "defect",
            severity: "blocking",
            evidence: "the x path is wrong",
            domain: "x",
            item: "R1",
            where: "src/x.ts:1 fn",
            expected: "X_EXPECTED",
            doneWhen: "true",
          },
        ]
      : [];
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
          contractVersion: K,
          correctionStatements: [],
          findingStatements: [],
          ballots: [],
          findings,
        },
      },
    ],
  };
}

function pickVote(seat: Reviewer, round: number): { hello: unknown; steps: FakePiStep[] } {
  return {
    hello: { role: "picker" as const, tools: ROLE_TOOLS.picker },
    steps: [
      {
        kind: "call-submit",
        tool: "submit_pick_vote",
        args: { inspected: ["a", "b", "c", "d"], round, seat, lane: "a", why: `${seat} picks a`, loserHad: { yes: false, anchors: [] } },
      },
    ],
  };
}

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
      workers: 2,
      domains: [{ name: "x", globs: ["src/**"] }],
      fixRounds: "domains",
      fixers: 1,
      fixChecks: "true",
      forbidden: ["--include-ignored"],
    },
    checks: ["true"],
    phaseChecks: ["true"],
    deadlines: { workerAttemptMs: 60_000, checkMs: 20_000, probeMs: 20_000, freezeMs: 20_000, evaluateMs: 10_000, reviewMs: 20_000, panelMs: 10_000 },
    workerScript: () => laneWorker("a"),
    laneWorkerScriptFor: (lane) => laneWorker(lane),
    laneReviewerScriptFor: (seat, _candidate, state) => reviewerWithPoint(seat as Reviewer, state),
    pickScriptFor: (seat, state) => pickVote(seat as Reviewer, state.phase.rounds?.length ?? 1),
    fixerScriptFor: () => ({
      hello: defaultWorkerHello(),
      steps: [
        // R9: the exact corpus run a fixer made by hand in exex 01.
        { kind: "call-sh", command: `touch ${marker} && cargo test -- --include-ignored` } as FakePiStep,
        { kind: "call-sh", command: "printf 'x fixed\\n' > src/x-fixed.txt" },
        { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
      ],
    }),
  });
  try {
    await setup.conductor.start();
    // The fixer's own agent (the fixer is a mapper: role worker, agent id
    // `fix-…`) is the one R9 is about.
    await waitFor(
      () =>
        readEvents(setup.runDir).some(
          (r) =>
            r.kind === "event" &&
            (r.event as { type?: string }).type === "FORBIDDEN_REFUSED" &&
            /cargo test -- --include-ignored/.test((r.event as { command?: string }).command ?? ""),
        ),
      180_000,
      50,
      setup.runDir,
    );
    const refusals = readEvents(setup.runDir)
      .filter((r) => r.kind === "event" && (r.event as { type?: string }).type === "FORBIDDEN_REFUSED")
      .map((r) => r.event as { agentId?: string; pattern?: string; command?: string });
    assert.ok(refusals.length >= 1);
    for (const refused of refusals) {
      assert.equal(refused.pattern, "--include-ignored", "the rule's own text is recorded");
      assert.match(refused.command ?? "", /--include-ignored/);
    }
    const fixerRefusal = refusals.find((r) => /^fix-/.test(r.agentId ?? ""));
    assert.ok(fixerRefusal, `the fixer's command is refused and recorded (agents: ${refusals.map((r) => r.agentId).join(", ")})`);
    assert.ok(!fs.existsSync(path.join(setup.repo.dir, marker)), "the refused command never ran");
    assert.deepEqual(buildContract({ ...setup.plan.phases[0] }).forbidden, ["--include-ignored"]);
  } finally {
    await setup.conductor.stop();
    cleanupDir(setup.runRoot);
    cleanupDir(setup.repo.dir);
    cleanupDir(setup.scriptsDir);
  }
});
