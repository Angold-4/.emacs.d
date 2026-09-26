// Plan 01h (design 01_ref_design.md goal 5, runtime doc §2–§5): the live
// Trade-offs panel and the cost meter.
//
// The panel is at most 6 self-contained lines, most important first:
// owner directives not yet delivered, amended criteria (old → new), flagged
// (reserved) decisions, M vetoes with M's reason, passes with dissent, then
// one line counting the advisories. The cost meter reports rounds, per-stage
// minutes, owner-wait minutes and a next-round estimate that is the median of
// this phase's completed rounds (freeze → freeze).
//
// `view.tradeoffs` and `view.cost` are computed by buildView (src/view.ts)
// from a hand-written control log, so no conductor process is needed.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";

import { buildContract, type RunPlanFile } from "../../src/conductor.ts";
import { tradeoffEntries } from "../../src/core/verdict.ts";
import type { Ballot, Decision, PhaseState } from "../../src/core/types.ts";
import { programStatusLines } from "../../src/program.ts";
import { buildView } from "../../src/view.ts";
import { basePhase, CV, makeDecision } from "./helpers.ts";

const C0 = "c0".repeat(20);
const C2 = "c2".repeat(20);
const T0 = Date.parse("2026-01-01T00:00:00.000Z");
const at = (min: number) => new Date(T0 + min * 60_000).toISOString();

const plan = {
  title: "tradeoffs test",
  repo: "/tmp/tt-view-tradeoffs",
  integrationBranch: "main",
  checks: ["true"],
  phases: [
    {
      id: "p1",
      goal: "make cancellation race-free",
      acceptance: ["it works", "the tests pass"],
      checks: ["true"],
      boundaries: [],
      reserved: ["public API"],
    },
  ],
} as unknown as RunPlanFile;

const K = buildContract(plan.phases[0]).contractVersion;

function decision(over: Partial<Decision> & { id: string }): Decision {
  return {
    version: 1,
    phaseId: "p1",
    source: "worker",
    class: "delegated",
    choice: over.id,
    whyItMatters: "it matters",
    alternatives: [{ option: "a", consequence: "b" }],
    recommendation: { choice: "a", reason: "b" },
    boundCandidateSha: C2,
    boundContractVersion: K,
    ...over,
  };
}

function ballot(over: Partial<Ballot> & { reviewer: Ballot["reviewer"]; decisionId: string; vote: Ballot["vote"] }): Ballot {
  return {
    rationale: `${over.vote} because`,
    evidence: ["src/cancel.ts:42"],
    boundCandidateSha: C2,
    boundContractVersion: K,
    boundRecordVersion: 1,
    ...over,
  };
}

/** A program of one node, so `programStatusLines` can find the fixture run. */
function program(root: string, runId: string): string {
  const dir = path.join(root, "programs", "prog1");
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(
    path.join(dir, "program.json"),
    JSON.stringify({
      title: "plan 13",
      maxParallel: 1,
      entries: [{ id: "13a", after: [], plan }],
    }),
  );
  const events = [
    { ts: at(0), event: { type: "NODE_STARTED", node: "13a", runId, branch: "main--13a", base: "main" } },
  ];
  fs.writeFileSync(path.join(dir, "events.jsonl"), `${events.map((e) => JSON.stringify(e)).join("\n")}\n`);
  return dir;
}

/** Writes a run directory whose control log produces the fixture phase: three
 * candidates (C0, C1, C2), one owner-wait episode, and on C2 an applied and a
 * reverted amendment, a flagged decision, an M veto and one dissent, plus
 * three open advisories. */
function runDir(root: string): string {
  const dir = path.join(root, "run1");
  fs.mkdirSync(path.join(dir, "plan"), { recursive: true });
  fs.mkdirSync(path.join(dir, "stream"), { recursive: true });
  fs.writeFileSync(path.join(dir, "plan", "v1.json"), JSON.stringify(plan));

  let seq = 0;
  const lines: string[] = [];
  const push = (ts: string, event: unknown) => {
    seq += 1;
    lines.push(JSON.stringify({ seq, ts, kind: "event", event }));
  };
  lines.push(JSON.stringify({ seq: 1, ts: at(0), kind: "init", event: { runId: "r1", integrationHead: "H0" } }));

  // A first candidate chain whose evidence the reviewer never accepted —
  // enough history for two completed rounds (freeze 12 → 31 → 50).
  push(at(0), { type: "ATTEMPT_STARTED" });
  // Plan 01b/01h: the owner-wait episode the cost meter must count (4→10).
  push(at(1), { type: "ATTEMPT_TIMED_OUT" });
  push(at(1), { type: "REPAIR_ATTEMPT_STARTED" });
  push(at(2), { type: "ATTEMPT_TIMED_OUT" });
  push(at(2), { type: "REPAIR_ATTEMPT_STARTED" });
  push(at(3), { type: "ATTEMPT_TIMED_OUT" });
  push(at(3), { type: "REPAIR_ATTEMPT_STARTED" });
  push(at(4), { type: "ATTEMPT_TIMED_OUT" }); // budget exhausted -> AWAITING_OWNER
  push(at(10), { type: "OWNER_CORRECTION", correctionId: "c1", text: "carry on" });
  push(at(10), { type: "REPAIR_ATTEMPT_STARTED" });
  push(at(11), { type: "SUBMIT_PHASE", disclosures: [] });
  push(at(12), { type: "FREEZE_COMPLETED", candidateSha: C0, decisions: [] });
  push(at(20), { type: "CHECKS_FAILED" });
  push(at(20), { type: "REPAIR_ATTEMPT_STARTED" });
  push(at(30), { type: "SUBMIT_PHASE", disclosures: [] });
  push(at(31), { type: "FREEZE_COMPLETED", candidateSha: "c1".repeat(20), decisions: [] });
  push(at(40), { type: "CHECKS_FAILED" });
  push(at(40), { type: "REPAIR_ATTEMPT_STARTED" });
  push(at(49), { type: "SUBMIT_PHASE", disclosures: [] });

  const dAmend1 = decision({
    id: "D-p1-1",
    version: 2,
    class: "reserved",
    choice: "the tests pass",
    amendment: {
      id: "AM-p1-C1",
      criterion: "it works",
      proposedWording: "the tests pass",
      why: "the literal wording cannot be met",
      raisedBy: "worker",
      status: "applied",
    },
  });
  const dAmend2 = decision({
    id: "D-p1-2",
    version: 2,
    class: "reserved",
    choice: "no fill after cancel",
    amendment: {
      id: "AM-p1-C2",
      criterion: "no fill after cancel",
      proposedWording: "no fill after cancel within 1s",
      why: "the venue acknowledges late",
      raisedBy: "M",
      status: "reverted",
    },
  });
  const dFlagged = decision({ id: "D-p1-3", class: "reserved", choice: "Errors are thrown, not returned" });
  const dVeto = decision({ id: "D-p1-4", choice: "Batch cancels per tick" });
  const dDissent = decision({ id: "D-p1-5", choice: "Cache misses rebuild from the log" });
  push(at(50), { type: "FREEZE_COMPLETED", candidateSha: C2, decisions: [dAmend1, dAmend2, dFlagged, dVeto, dDissent] });
  push(at(51), { type: "CHECKS_PASSED" });
  push(at(52), { type: "PROBE_PASSED", probedI: "I1" });
  const review = (reviewer: "M" | "A" | "B", ts: string) =>
    push(ts, {
      type: "REVIEW_SUBMITTED",
      review: {
        reviewer,
        phaseId: "p1",
        candidateSha: C2,
        contractVersion: K,
        correctionStatements: [],
        findingStatements: [],
      },
    });
  review("M", at(55));
  review("A", at(56));
  review("B", at(57));

  push(at(58), { type: "BALLOT_CAST", ballot: ballot({ reviewer: "M", decisionId: "D-p1-3", vote: "approve" }) });
  push(at(58), { type: "BALLOT_CAST", ballot: ballot({ reviewer: "A", decisionId: "D-p1-3", vote: "approve" }) });
  push(
    at(58),
    {
      type: "BALLOT_CAST",
      ballot: ballot({
        reviewer: "M",
        decisionId: "D-p1-4",
        vote: "reject",
        rationale: "a lone cancel still waits a tick",
      }),
    },
  );
  push(at(58), { type: "BALLOT_CAST", ballot: ballot({ reviewer: "A", decisionId: "D-p1-4", vote: "approve" }) });
  push(at(58), { type: "BALLOT_CAST", ballot: ballot({ reviewer: "M", decisionId: "D-p1-5", vote: "approve" }) });
  push(at(58), { type: "BALLOT_CAST", ballot: ballot({ reviewer: "A", decisionId: "D-p1-5", vote: "approve" }) });
  push(
    at(58),
    {
      type: "BALLOT_CAST",
      ballot: ballot({ reviewer: "B", decisionId: "D-p1-5", vote: "reject", rationale: "cache churn" }),
    },
  );
  const finding = (id: string, bound: string, ts: string) =>
    push(ts, {
      type: "FINDING_RAISED",
      finding: {
        id,
        version: 1,
        phaseId: "p1",
        kind: "defect",
        severity: "advisory",
        evidence: `${id} evidence`,
        raisedBy: id.includes("-A-") ? "A" : "M",
        status: "open",
        boundCandidateSha: bound,
      },
    });
  finding("F-p1-M-1", C2, at(59));
  finding("F-p1-A-1", C2, at(59));
  finding("F-p1-M-2", C0, at(59));

  fs.writeFileSync(path.join(dir, "events.jsonl"), `${lines.join("\n")}\n`);
  return dir;
}

test("view-tradeoffs: the panel is ordered amendments → flagged → M vetoes → dissent → advisories, and the cost is the phase's own history", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-view-tradeoffs-"));
  try {
    const dir = runDir(root);
    const view = buildView(dir, plan, false, new Date(at(59)));

    const tradeoffs = view.tradeoffs ?? [];
    assert.deepEqual(
      tradeoffs.map((t) => t.kind),
      ["amendment", "amendment", "flagged", "veto", "dissent", "advisories"],
      "most important first, one line per record",
    );
    assert.match(tradeoffs[0].text, /⚑ AMENDED AM-p1-C1: it works → the tests pass/);
    assert.match(tradeoffs[1].text, /⚑ REVERTED AM-p1-C2: no fill after cancel within 1s → no fill after cancel/);
    assert.match(tradeoffs[2].text, /⚑ flagged: D-3 Errors are thrown, not returned — passed/);
    assert.match(tradeoffs[3].text, /vetoed by M: D-4 Batch cancels per tick — a lone cancel still waits a tick/);
    assert.match(tradeoffs[4].text, /passed with dissent \(B rejected\): D-5 Cache misses rebuild from the log/);
    assert.equal(tradeoffs[5].text, "3 advisories (2 new) — C-c m d");
    assert.equal(tradeoffs[5].recordId, "F-p1-M-1", "the count line targets a record RET can reach");

    const cost = view.cost;
    assert.ok(cost);
    assert.equal(cost.rounds, 3, "three candidates frozen");
    assert.equal(cost.ownerWaitMinutes, 6, "the one AWAITING_OWNER episode (4→10)");
    assert.equal(cost.nextRoundMinutes, 19, "median of the two completed freeze→freeze rounds (19, 19)");
    assert.equal(cost.totalMinutes, 59);
    assert.deepEqual(
      cost.stageMinutes.map((s) => s.stage),
      ["implement", "checks", "review", "freeze", "resolve", "probe"],
    );
    assert.match(cost.text, /^3 rounds · 59m total · implement 24m · checks 18m/);
    assert.match(cost.text, /owner wait 6m · next round ≈ 19 min$/);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("view-tradeoffs: the panel is capped at 6 entries", () => {
  const amendments: Decision[] = [1, 2, 3, 4, 5].map((n) =>
    ({
      id: `D-p1-${n}`,
      version: 2,
      phaseId: "p1",
      source: "worker",
      class: "reserved",
      choice: `amendment ${n}`,
      whyItMatters: "x",
      alternatives: [{ option: "a", consequence: "b" }],
      recommendation: { choice: "a", reason: "b" },
      boundCandidateSha: "C1",
      boundContractVersion: { snapshot: 1, sectionSha256: "a".repeat(64) },
      amendment: {
        id: `AM-p1-${n}`,
        criterion: `criterion ${n}`,
        proposedWording: `wording ${n}`,
        why: "cannot be met as written",
        raisedBy: "worker",
        status: "proposed",
      },
    }) satisfies Decision,
  );
  const flagged = [1, 2].map((n) => makeDecision({ id: `D-p1-f${n}`, class: "reserved", choice: `flagged ${n}` }));
  const entries = tradeoffEntries(
    basePhase({ candidate: { sha: "C1", contractVersion: CV() }, decisions: [...amendments, ...flagged] }),
  );
  assert.equal(entries.length, 6, "seven candidates for six lines");
  assert.deepEqual(
    entries.map((e) => e.kind),
    ["amendment", "amendment", "amendment", "amendment", "amendment", "flagged"],
  );
});

test("view-tradeoffs: a directive in force that an agent has not received ranks first", () => {
  const phase: PhaseState = basePhase({
    ownerDirectives: [
      {
        id: "OD-1",
        seq: 1,
        text: "the 14 exchange-state-machine failures are pre-existing, not yours",
        scope: "phase",
        status: "in-force",
        commandId: "c1",
        at: at(0),
        targets: ["worker", "M"],
        deliveries: { worker: "delivered" },
      },
    ],
  });
  const entries = tradeoffEntries(phase);
  assert.equal(entries.length, 1);
  assert.equal(entries[0].kind, "directive");
  assert.equal(entries[0].recordId, "OD-1");
  assert.match(entries[0].text, /directive OD-1 not yet delivered to M:/);
});

test("view-tradeoffs: a delivered directive and an empty phase show nothing", () => {
  const delivered = basePhase({
    ownerDirectives: [
      {
        id: "OD-1",
        seq: 1,
        text: "already landed",
        scope: "phase",
        status: "in-force",
        commandId: "c1",
        at: at(0),
        targets: ["worker"],
        deliveries: { worker: "delivered" },
      },
    ],
  });
  assert.deepEqual(tradeoffEntries(delivered), []);
  assert.deepEqual(tradeoffEntries(basePhase()), []);
});

test("program status: each node gets its rounds, minutes, owner wait and top trade-off", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-view-prog-"));
  try {
    const dir = runDir(root);
    const prog = program(root, path.basename(dir));
    const lines = programStatusLines(prog, new Date(at(59)));
    const text = lines.join("\n");
    assert.match(text, /3 rounds · 59m · owner wait 6m · top: ⚑ AMENDED AM-p1-C1: it works → the tests pass/);
    // Without the detail (the `tt program list` path) the line is not built.
    const lean = programStatusLines(prog, new Date(at(59)), { nodeDetail: false }).join("\n");
    assert.doesNotMatch(lean, /3 rounds/);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
