// tt 09 (change 3): combine the lanes. exex 01a2's two lanes specialised
// (lane a finished the production removal, lane b the test conversion) and
// three rounds never converged, because each round kept one winner and threw
// the loser's half away. These tests pin the pure half of the combine step:
// the trigger rule (an item the loser met and the winner did not), the base
// the next round starts from, the bounded build/check/fix sequence, the
// reduced record, and the plan keyword and model role.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";

import {
  consolidatedBase,
  consolidatedLabel,
  consolidationEnabled,
  consolidationLine,
  consolidationPlan,
  itemEvidence,
  latestConsolidationLine,
  MAX_LANE_FIXES,
  runConsolidation,
  type ConsolidationHost,
} from "../../src/core/lanes.ts";
import { buildContract } from "../../src/conductor.ts";
import { parseOrgPlan } from "../../src/core/org-plan.ts";
import { lintPlan } from "../../src/core/plan-lint.ts";
import { reduce } from "../../src/core/reduce.ts";
import { planModelSelector } from "../../src/core/roles.ts";
import { validate } from "../../src/core/schema.ts";
import type { LaneCandidateRecord, Review, RoundRecord } from "../../src/core/types.ts";
import { baseState } from "./helpers.ts";

const SEATS = ["M", "A", "B"];
const IDS = ["R1", "R2", "C1"];

/** A passing lane candidate whose three seats give these verdicts per item. */
function reviewed(lane: string, sha: string, verdicts: Record<string, string[]>, ok = true): LaneCandidateRecord {
  return {
    lane,
    sha,
    ok,
    reviews: SEATS.map((seat, i) => ({
      seat,
      review: {
        items: Object.entries(verdicts).map(([id, v]) => ({ id, verdict: v[i], evidence: `lane-${lane}.txt:${i + 1} ${id} by ${seat}` })),
        arch: [],
      } as unknown as Review,
    })),
  };
}

const MET = ["met", "met", "met"];
const UNMET = ["unmet", "unmet", "unmet"];

/** Round 2: lane a (picked) met R1 only; lane b met R2 and C1. */
function round2(overrides: Partial<RoundRecord> = {}): RoundRecord {
  return {
    round: 2,
    base: "W1",
    lanes: ["a", "b"],
    candidates: [reviewed("a", "C2a", { R1: MET, R2: UNMET, C1: UNMET }), reviewed("b", "C2b", { R1: UNMET, R2: MET, C1: MET })],
    votes: SEATS.map((seat) => ({ seat, lane: "a", why: "a" })),
    picked: { lane: "a", sha: "C2a", votes: 3 },
    ...overrides,
  };
}

test("tt 09: an item the losing lane met and the winner did not triggers a consolidation that takes exactly those items", () => {
  assert.deepEqual(consolidationPlan(round2(), IDS, 3), {
    round: 2,
    winner: { lane: "a", sha: "C2a" },
    takes: [{ lane: "b", sha: "C2b", items: ["R2", "C1"] }],
  });
  // An item both met is the winner's already: only what the winner lacks is taken.
  const both = round2({ candidates: [reviewed("a", "C2a", { R1: MET, R2: MET, C1: UNMET }), reviewed("b", "C2b", { R1: MET, R2: MET, C1: MET })] });
  assert.deepEqual(consolidationPlan(both, IDS, 3)?.takes, [{ lane: "b", sha: "C2b", items: ["C1"] }]);
});

test("tt 09: no consolidation when the loser met nothing the winner lacks, the round picked nothing, the pick regressed or the loser is red", () => {
  // The loser met nothing more: nothing changes.
  const none = round2({ candidates: [reviewed("a", "C2a", { R1: MET, R2: MET, C1: UNMET }), reviewed("b", "C2b", { R1: MET, R2: UNMET, C1: UNMET })] });
  assert.equal(consolidationPlan(none, IDS, 3), undefined);
  // [met, partial, partial] is not met: a strict majority of the seats is.
  const thin = round2({ candidates: [reviewed("a", "C2a", { R1: MET, R2: UNMET, C1: UNMET }), reviewed("b", "C2b", { R1: MET, R2: ["met", "partial", "partial"], C1: UNMET })] });
  assert.equal(consolidationPlan(thin, IDS, 3), undefined);
  assert.equal(consolidationPlan(round2({ picked: undefined }), IDS, 3), undefined, "a round with no pick");
  assert.equal(consolidationPlan(round2({ regressed: { items: ["R1"], sha: "C2a" } }), IDS, 3), undefined, "a regressed pick is never the base");
  // A lane whose candidate failed its checks is not a passing candidate.
  const red = round2({ candidates: [reviewed("a", "C2a", { R1: MET, R2: UNMET, C1: UNMET }), reviewed("b", "C2b", { R1: UNMET, R2: MET, C1: MET }, false)] });
  assert.equal(consolidationPlan(red, IDS, 3), undefined);
  // An unstructured plan (no item ids) never consolidates.
  assert.equal(consolidationPlan(round2(), [], 3), undefined);
  assert.equal(consolidationPlan(undefined, IDS, 3), undefined);
});

test("tt 09: with three lanes each item is taken once, from the first lane that met it", () => {
  const three = round2({
    lanes: ["a", "b", "c"],
    candidates: [
      reviewed("a", "C2a", { R1: MET, R2: UNMET, C1: UNMET }),
      reviewed("b", "C2b", { R1: UNMET, R2: MET, C1: UNMET }),
      reviewed("c", "C2c", { R1: UNMET, R2: MET, C1: MET }),
    ],
  });
  assert.deepEqual(consolidationPlan(three, IDS, 3)?.takes, [
    { lane: "b", sha: "C2b", items: ["R2"] },
    { lane: "c", sha: "C2c", items: ["C1"] },
  ]);
});

test("tt 09: the consolidator is told each item's evidence from the seats that said met", () => {
  const b = round2().candidates[1];
  assert.deepEqual(itemEvidence(b, "R2"), ["M: lane-b.txt:1 R2 by M", "A: lane-b.txt:2 R2 by A", "B: lane-b.txt:3 R2 by B"]);
  assert.deepEqual(itemEvidence(b, "R1"), [], "no seat said met");
  // Reviews arrive in any order; the prompt lists them in seat order.
  const late = { ...b, reviews: [...b.reviews!].reverse() };
  assert.deepEqual(itemEvidence(late, "R2", SEATS), ["M: lane-b.txt:1 R2 by M", "A: lane-b.txt:2 R2 by A", "B: lane-b.txt:3 R2 by B"]);
});

test("tt 09: base selection — a green consolidation is the next round's base, anything else keeps the winner", () => {
  const ok = { from: "C2a", lane: "a", took: [{ lane: "b", sha: "C2b", items: ["R2", "C1"] }], ok: true, sha: "K2" };
  const r1 = round2({ round: 1, base: "H0", picked: { lane: "a", sha: "C1a", votes: 3 }, candidates: [reviewed("a", "C1a", { R1: MET, R2: UNMET, C1: UNMET })] });
  assert.equal(consolidatedBase([r1, round2({ consolidated: ok })]), "K2");
  assert.equal(consolidatedBase([r1, round2({ consolidated: { ...ok, ok: false, sha: undefined, note: "red" } })]), undefined, "a failed consolidation keeps the winner");
  assert.equal(consolidatedBase([r1, round2()]), undefined, "no consolidation keeps the winner");
  assert.equal(consolidatedBase([r1, round2({ consolidated: ok, regressed: { items: ["R1"], sha: "C2a" } })]), undefined, "a regressed pick is never a base");
  assert.equal(consolidatedBase([r1, round2({ consolidated: { ...ok, from: "C9z" } })]), undefined, "a consolidation of another commit");
  // A round that picked nothing (it started from K2) keeps K2 for the repeat.
  const r3 = { round: 3, base: "K2", lanes: ["a", "b"], candidates: [], votes: [] } as RoundRecord;
  assert.equal(consolidatedBase([r1, round2({ consolidated: ok }), r3]), "K2");
  // A later pick supersedes it; `beforeRound` only looks at earlier rounds.
  const r3picked = { ...r3, picked: { lane: "b", sha: "C3b", votes: 2 } };
  assert.equal(consolidatedBase([r1, round2({ consolidated: ok }), r3picked]), undefined);
  assert.equal(consolidatedBase([r1, round2({ consolidated: ok }), r3picked], 3), "K2");
  assert.equal(consolidatedBase([r1, round2({ consolidated: ok })], 2), undefined, "round 2's own consolidation is not round 2's base");
});

test("tt 09: the combine step runs only with two or more lanes and no #+TT_CONSOLIDATE: off", () => {
  assert.equal(consolidationEnabled({ workers: 2 }), true);
  assert.equal(consolidationEnabled({ workers: 2, consolidate: false }), false);
  assert.equal(consolidationEnabled({ workers: 1 }), false, "a single-worker plan is untouched");
  assert.equal(consolidationEnabled({}), false);
  // The contract freezes only an explicit off, so older contracts keep their shape.
  const phase = { id: "p1", goal: "g", acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [], workers: 2 };
  assert.equal(buildContract({ ...phase, consolidate: "off" }).consolidate, false);
  assert.equal(buildContract({ ...phase, consolidate: "OFF" }).consolidate, false);
  assert.equal("consolidate" in buildContract({ ...phase, consolidate: "on" }), false);
  assert.equal("consolidate" in buildContract(phase), false);
  assert.equal(consolidationEnabled(buildContract({ ...phase, consolidate: "off" })), false);
  assert.equal(consolidationEnabled(buildContract(phase)), true);
});

test("tt 09: the round view names the consolidated commit and what it took, or why it failed", () => {
  const took = [{ lane: "b", sha: "C2b", items: ["R6", "C3"] }];
  assert.equal(consolidatedLabel(2, "a", took), "C2-a+b");
  assert.equal(
    consolidationLine(round2({ consolidated: { from: "C2a", lane: "a", took, ok: true, sha: "1234567890" } })),
    "consolidated C2-a+b 1234567 = C2-a + lane b's items R6, C3",
  );
  assert.equal(
    consolidationLine(round2({ consolidated: { from: "C2a", lane: "a", took, ok: false, note: "its commit abc failed the round checks" } })),
    "consolidation of C2-a + lane b's items R6, C3 failed: its commit abc failed the round checks — the next round starts from C2-a",
  );
  assert.equal(consolidationLine(round2()), undefined);
  // The status names it between the rounds, then as the next round's base.
  const done = round2({ consolidated: { from: "C2a", lane: "a", took, ok: true, sha: "1234567890" } });
  assert.equal(latestConsolidationLine([done]), "consolidated C2-a+b 1234567 = C2-a + lane b's items R6, C3");
  const r3 = { round: 3, base: "1234567890", lanes: ["a", "b"], candidates: [], votes: [] } as RoundRecord;
  assert.equal(latestConsolidationLine([done, r3]), "round 3 starts from consolidated C2-a+b 1234567 = C2-a + lane b's items R6, C3");
  assert.equal(latestConsolidationLine([round2(), { ...r3, base: "C2a" }]), undefined);
});

/** A scripted consolidation host: each build/fix returns the next sha (or a
 * note), each check the next result. */
function host(builds: Array<string | { note: string }>, checks: boolean[], stopAfter?: number): ConsolidationHost & { calls: string[] } {
  const calls: string[] = [];
  let b = 0;
  let c = 0;
  const next = () => {
    const r = builds[b++];
    return typeof r === "string" ? { lane: "consolidator", sha: r, added: [`added ${r}`] } : { lane: "consolidator", note: r?.note ?? "none" };
  };
  return {
    calls,
    build: async () => {
      calls.push("build");
      return next();
    },
    check: async (sha) => {
      calls.push(`check ${sha}`);
      const ok = checks[c++] ?? false;
      return ok ? { ok } : { ok, note: `the check failed on ${sha}` };
    },
    fix: async (sha) => {
      calls.push(`fix ${sha}`);
      return next();
    },
    stopRequested: () => stopAfter !== undefined && calls.length >= stopAfter,
  };
}

test("tt 09: a green consolidation is ok at once; a red one goes back to the consolidator, like a lane's fix", async () => {
  const green = host(["K1"], [true]);
  assert.deepEqual(await runConsolidation(green), { ok: true, sha: "K1", fixes: 0, added: ["added K1"] });
  const fixed = host(["K1", "K2"], [false, true]);
  assert.deepEqual(await runConsolidation(fixed), { ok: true, sha: "K2", fixes: 1, added: ["added K2"] });
  assert.deepEqual(fixed.calls, ["build", "check K1", "fix K1", "check K2"]);
});

test("tt 09: a consolidation that produces nothing or stays red fails with its reason, bounded by the lane fix limit", async () => {
  assert.deepEqual(await runConsolidation(host([{ note: "the consolidator timed out before submitting" }], [])), {
    ok: false,
    fixes: 0,
    note: "the consolidator timed out before submitting",
  });
  // A fix that produced no new commit ends the loop.
  const stuck = await runConsolidation(host(["K1", { note: "its fix of K1 changed nothing" }], [false]));
  assert.equal(stuck.ok, false);
  assert.match(stuck.note!, /^its commit K1 failed the round checks: the check failed on K1 \(its fix of K1 changed nothing\)$/);
  // Always red: at most MAX_LANE_FIXES fixes.
  const shas = Array.from({ length: MAX_LANE_FIXES + 3 }, (_, i) => `K${i}`);
  const red = host(shas, []);
  const outcome = await runConsolidation(red);
  assert.equal(outcome.ok, false);
  assert.equal(outcome.fixes, MAX_LANE_FIXES);
  assert.equal(red.calls.filter((c) => c.startsWith("fix")).length, MAX_LANE_FIXES);
  // A build that throws is a failure, never a crash.
  const throws: ConsolidationHost = { build: async () => { throw new Error("boom"); }, check: async () => ({ ok: true }), fix: async () => ({ lane: "consolidator" }) };
  assert.deepEqual(await runConsolidation(throws), { ok: false, fixes: 0, note: "the consolidator failed: boom" });
});

test("tt 09: a stop mid-way records nothing", async () => {
  assert.equal((await runConsolidation(host(["K1"], [true], 1))).stopped, true);
});

test("tt 09: ROUND_CONSOLIDATED validates, marks the round's pick and survives the next round's start (replay)", () => {
  const schema = JSON.parse(readFileSync(new URL("../../schemas/event.schema.json", import.meta.url), "utf8"));
  const event = { type: "ROUND_CONSOLIDATED", round: 1, from: "C1a", lane: "a", took: [{ lane: "b", sha: "C1b", items: ["R2"] }], ok: true, sha: "K1", added: ["src/x.ts — an import"], fixes: 0 };
  assert.ok(validate(schema, event).valid, "the event validates");
  let state = baseState({ phase: "IMPLEMENTING" });
  for (const e of [
    { type: "ROUND_STARTED", round: 1, base: "B0", lanes: ["a", "b"] },
    { type: "CANDIDATE_SUBMITTED", round: 1, lane: "a", sha: "C1a" },
    { type: "CANDIDATE_CHECKED", round: 1, lane: "a", ok: true },
    { type: "CANDIDATE_SUBMITTED", round: 1, lane: "b", sha: "C1b" },
    { type: "CANDIDATE_CHECKED", round: 1, lane: "b", ok: true },
  ]) {
    const r = reduce(state, e);
    assert.ok(r.ok, `${e.type}: ${!r.ok ? r.reason : ""}`);
    state = r.state;
  }
  assert.equal(reduce(state, event).ok, false, "a round with no pick cannot be consolidated");
  const picked = reduce(state, { type: "CANDIDATE_PICKED", round: 1, lane: "a", sha: "C1a", votes: 3 });
  assert.ok(picked.ok);
  assert.equal(reduce(picked.state, { ...event, from: "C1b" }).ok, false, "it starts from the pick");
  assert.equal(reduce(picked.state, { ...event, sha: undefined }).ok, false, "an ok consolidation names its commit");
  const r = reduce(picked.state, event);
  assert.ok(r.ok, !r.ok ? r.reason : "");
  const expected = { from: "C1a", lane: "a", took: [{ lane: "b", sha: "C1b", items: ["R2"] }], ok: true, sha: "K1", added: ["src/x.ts — an import"], fixes: 0 };
  assert.deepEqual(r.state.phase.rounds![0].consolidated, expected);
  // The next round's start keeps it, and it is that round's base.
  const next = reduce(r.state, { type: "ROUND_STARTED", round: 2, base: "K1", lanes: ["a", "b"] });
  assert.ok(next.ok);
  assert.deepEqual(next.state.phase.rounds![0].consolidated, expected);
  assert.equal(consolidatedBase(next.state.phase.rounds, 2), "K1");
  // A failed consolidation records why and needs no commit.
  const failed = reduce(picked.state, { ...event, ok: false, sha: undefined, note: "red" });
  assert.ok(failed.ok);
  assert.equal(consolidatedBase(failed.state.phase.rounds), undefined);
});

test("tt 09: the consolidator runs on #+TT_MODELS consolidator=…, else the leader seat's model", () => {
  const models = {
    worker: { model: "deepseek-flash" },
    reviewer: { model: "shared" },
    reviewerSeats: { M: { model: "claude-opus-5.5" }, A: { model: "gpt-6.1-sol" } },
  };
  const select = planModelSelector({ models }, ["M", "A", "B"]);
  assert.deepEqual(select("consolidator", "M"), { model: "claude-opus-5.5" }, "the leader M's model");
  assert.deepEqual(select("consolidator", "A"), { model: "gpt-6.1-sol" }, "a leader A's model");
  assert.deepEqual(select("consolidator"), { model: "claude-opus-5.5" }, "no leader named: the first seat");
  assert.deepEqual(select("consolidator", "B"), { model: "shared" }, "a seat with no own model: the shared reviewer model");
  const own = planModelSelector({ models: { ...models, consolidator: { provider: "anthropic", model: "claude-opus-5.5" } } }, ["M", "A", "B"]);
  assert.deepEqual(own("consolidator", "A"), { provider: "anthropic", model: "claude-opus-5.5" });
  assert.equal(planModelSelector({}, ["M"])("consolidator", "M"), undefined, "no models: Pi's default");
});

test("tt 09: lint knows #+TT_CONSOLIDATE (on/off) and the consolidator model role", () => {
  const org = (keyword: string) =>
    [
      "#+TITLE: t",
      "#+TT_WORKERS: 2",
      keyword,
      "#+TT_MODELS: consolidator=anthropic:claude-opus-5.5",
      "* Phase",
      "  :PROPERTIES:",
      "  :ID: p1",
      "  :CHECKS: true",
      "  :END:",
      "  Goal: g",
      "  Acceptance:",
      "  - it works",
    ].join("\n");
  const off = parseOrgPlan(org("#+TT_CONSOLIDATE: off"), "/tmp/p.org");
  assert.equal(off.consolidate, "off");
  assert.equal(off.consolidateLine, 3);
  assert.deepEqual(lintPlan(off).filter((f) => f.rule === "consolidate" || f.rule === "model-declaration"), []);
  assert.deepEqual(lintPlan(parseOrgPlan(org("#+TT_CONSOLIDATE: ON"))).filter((f) => f.rule === "consolidate"), []);
  const bad = lintPlan(parseOrgPlan(org("#+TT_CONSOLIDATE: maybe"), "/tmp/p.org")).filter((f) => f.rule === "consolidate");
  assert.equal(bad.length, 1);
  assert.equal(bad[0].severity, "error");
  assert.equal(bad[0].line, 3);
  assert.match(bad[0].problem, /#\+TT_CONSOLIDATE: must be on or off, got "maybe"/);
  // A phase's own :CONSOLIDATE: is checked the same way.
  const phase = lintPlan({ phases: [{ id: "p1", consolidate: "sometimes" }] }).filter((f) => f.rule === "consolidate");
  assert.equal(phase.length, 1);
  assert.equal(phase[0].phaseId, "p1");
});
