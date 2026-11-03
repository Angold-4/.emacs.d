import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { rebuildState } from "../../src/conductor.ts";
import { phaseItemOutcomes } from "../../src/core/items.ts";
import { ROLE_TOOLS } from "../../src/core/roles.ts";
import { cleanupDir, defaultReviewerHello, defaultWorkerHello, readEvents, setupConductor, waitFor, type FakePiStep } from "./harness.ts";

const items = { architecture: [], constraints: [], requirements: ["R1", "R2", "R3"].map((id, i) => ({
  id, title: id, text: `${id} works`, arch: [], verify: ["review"], domain: i === 1 ? "gates" : "booking",
})) };
const deadlines = { abortGraceMs: 300, termGraceMs: 300, helloTimeoutMs: 10_000, workerAttemptMs: 30_000,
  checkMs: 20_000, probeMs: 20_000, freezeMs: 20_000, evaluateMs: 10_000, reviewMs: 20_000, reproductionMs: 20_000 };
const point = (domain: string, item: string) => ({ kind: "defect", severity: "blocking", domain, item,
  where: `${domain}/a.ts:1`, evidence: `${item} ${domain}/a.ts:1 returns wrong output`, expected: "correct output",
  doneWhen: `test -f ${domain}-fixed.txt` });

for (const mode of ["ballot", "grade-finding", "defect"]) test(`ACCEPT trusts domain approvals: ${mode === "defect" ? "a new concrete defect blocks" : mode === "grade-finding" ? "a re-grade in findings is advisory" : "a bare partial is carried as advisory"}`, async () => {
  const defect = mode === "defect";
  const prompts = fs.mkdtempSync("/tmp/tt-stage-prompts-");
  const promptLog = path.join(prompts, "prompts.txt");
  const setup = await setupConductor({
    workerScript: () => ({ hello: defaultWorkerHello(), steps: [] }),
    items, stubReviews: false, checks: ["true"], phaseChecks: ["true"], deadlines,
    phase: { ...items, id: "p1", goal: "fix both domains", acceptance: ["works"], checks: ["true"], boundaries: [], reserved: [],
      workers: 2, domains: [{ name: "booking", globs: ["booking/**"] }, { name: "gates", globs: ["gates/**"] }],
      fixRounds: "domains", fixChecks: "true", rounds: 2 },
    extraWorkerEnv: { FAKE_PI_PROMPT_LOG: promptLog },
    extraReviewerEnv: () => ({ FAKE_PI_PROMPT_LOG: promptLog }),
    laneWorkerScriptFor: () => ({ hello: defaultWorkerHello(), steps: [
      { kind: "call-sh", command: "mkdir -p booking gates; printf 'base\n' > booking/a.ts; printf 'base\n' > gates/a.ts" },
      { kind: "call-submit", tool: "submit_coverage", args: { arch: [], items: items.requirements.map((i) => ({ id: i.id, status: "done", where: [], tests: [] })) } },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ] }),
    laneReviewerScriptFor: (seat, _sha, state) => ({ hello: defaultReviewerHello(), steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } }, { kind: "wait-for-prompt" },
      { kind: "call-tool", tool: "read", args: { path: "booking/a.ts" } },
      { kind: "call-submit", tool: "submit_review", args: { reviewer: seat, phaseId: "p1", candidateSha: "$TT_CANDIDATE_SHA",
        contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [], ballots: [],
        findings: seat === "M" ? [point("booking", "R1"), point("gates", "R2")] : [],
        items: items.requirements.map((i) => ({ id: i.id, verdict: i.id === "R3" ? "met" : "unmet", evidence: "booking/a.ts:1 returns wrong output" })), arch: [] } },
    ] }),
    pickScriptFor: (seat) => ({ hello: { role: "picker", tools: ROLE_TOOLS.picker }, steps: [
      { kind: "call-submit", tool: "submit_pick_vote", args: { inspected: ["a", "b"], round: 1, seat, lane: "a", why: "fix its points", loserHad: { yes: false, anchors: [] } } },
    ] }),
    fixerScriptFor: (domain) => ({ hello: defaultWorkerHello(), steps: [
      { kind: "call-sh", command: `printf 'fixed\n' > ${domain}-fixed.txt` },
      { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
    ] }),
    reviewerScriptFor: (seat, state) => ({ hello: defaultReviewerHello(), steps: [
      { kind: "call-submit", tool: "submit_discovery", args: { discoveries: [] } }, { kind: "wait-for-prompt" },
      { kind: "call-tool", tool: "read", args: { path: "booking/a.ts" } },
      { kind: "call-submit", tool: "submit_review", args: { reviewer: seat, phaseId: "p1", candidateSha: "$TT_CANDIDATE_SHA",
        contractVersion: state.phase.contract.contractVersion, correctionStatements: [], findingStatements: [], ballots: [],
        findings: seat !== "M" ? [] : defect ? [{ ...point("booking", "R1"), evidence: "booking/a.ts:1 returns duplicate output when both domains retry", doneWhen: "test -f regression-fixed.txt" }]
          : mode === "grade-finding" ? [{ ...point("booking", "R1"), evidence: "R1 booking/a.ts:1 still partial" }] : [],
        items: [{ id: "R3", verdict: "met", evidence: "booking/a.ts:1 read and verified" },
          ...(seat === "M" ? [{ id: "R1", verdict: "partial", evidence: "still partial" }] : [])], arch: [] } },
    ] as FakePiStep[] }),
  });
  try {
    await setup.conductor.start();
    const afterHandoff = () => {
      const records = readEvents(setup.runDir);
      const index = records.findIndex((r) => r.kind === "event" && (r.event as { type?: string; candidateSha?: string }).type === "FREEZE_COMPLETED" &&
        setup.conductor.state.phase.rounds?.some((round) => round.fixed?.candidate === (r.event as { candidateSha?: string }).candidateSha));
      return index < 0 ? [] : records.slice(index + 1);
    };
    await waitFor(() => afterHandoff().some((r) => r.kind === "incomplete_review_rejected") || readEvents(setup.runDir).some((r) => r.kind === "event" &&
      (r.event as { type?: string; review?: { candidateSha: string } }).type === "REVIEW_SUBMITTED" &&
      setup.conductor.state.phase.rounds?.some((round) => round.fixed?.candidate === (r.event as { review: { candidateSha: string } }).review?.candidateSha)), 60_000, 50, setup.runDir);
    assert.ok(!afterHandoff().some((r) => r.kind === "incomplete_review_rejected"), "approved domain items need no further item ballot or anchors");
    const p = setup.conductor.state.phase;
    const candidate = p.rounds!.find((r) => r.fixed?.candidate)!.fixed!.candidate!;
    if (defect) {
      await waitFor(() => setup.conductor.state.phase.findings.some((f) => f.evidence.includes("both domains retry")), 20_000, 50, setup.runDir);
      assert.ok(setup.conductor.state.phase.findings.some((f) => f.evidence.includes("both domains retry") && f.status === "open" && f.severity === "blocking"));
      assert.notEqual(setup.conductor.state.phase.phase, "DONE");
      await waitFor(() => afterHandoff().some((r) => r.kind === "incomplete_review_rejected") ||
        Object.values(setup.conductor.state.phase.reviews).filter((r) => r?.review?.candidateSha === candidate).length === 3, 20_000, 50, setup.runDir);
      assert.ok(!afterHandoff().some((r) => r.kind === "incomplete_review_rejected"), "a new finding does not make the other seats re-grade the old point");
    } else {
      await waitFor(() => setup.conductor.state.phase.phase === "DONE", 60_000, 50, setup.runDir);
      const current = setup.conductor.state.phase;
      assert.deepEqual(phaseItemOutcomes(current).map((o) => o.outcome), ["met", "met", "met"]);
      assert.ok(current.findings.some((f) => f.severity === "advisory" && /R1.*partial/.test(f.evidence)), "the unsupported ballot is recorded through the advisory path");
      assert.deepEqual(phaseItemOutcomes(rebuildState(setup.runDir, setup.plan).phase), phaseItemOutcomes(current), "approval evidence survives replay");
    }
    const text = fs.readFileSync(promptLog, "utf8");
    assert.match(text, /Assume every point resolved by an approved domain is resolved/);
    assert.match(text, /integration defects across domains/);
    assert.match(text, /green fix checks; test -f booking-fixed\.txt: holds/);
    assert.match(text, /the point is fixed on the reviewed commit/);
  } finally {
    await setup.conductor.stop(); cleanupDir(setup.runRoot); cleanupDir(setup.scriptsDir); cleanupDir(prompts);
  }
});
