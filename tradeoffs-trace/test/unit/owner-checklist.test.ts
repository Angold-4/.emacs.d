// Plan 01c: the plan's `Owner checklist:` items are the owner's, not the
// worker's or the reviewers'. They must never reach a prompt (so no reviewer
// blocks the phase for not doing them), and they must show in `tt summary`'s
// PR body as open `- [ ]` items.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import {
  buildContract,
  buildReviewerPrompt,
  buildWorkerPrompt,
  type RunPlanFile,
  type RunPlanPhase,
} from "../../src/conductor.ts";
import type { PhaseState } from "../../src/core/types.ts";
import { prSummary } from "../../src/view.ts";

const CHECKLIST_ITEM = "the owner records a live `ips-split` run with the five keys exported";

function phase(): RunPlanPhase {
  return {
    id: "13.10",
    goal: "split the components",
    acceptance: ["`transport_envelope_round_trips_source_tick` passes", "all existing tests still pass"],
    acceptanceLines: [30, 31],
    ownerChecklist: [CHECKLIST_ITEM, "the owner rules on the n>=4 blend"],
    ownerChecklistLines: [39, 40],
    checks: ["true"],
    boundaries: [],
    reserved: [],
    provisional: false,
  };
}

function plan(p: RunPlanPhase): RunPlanFile {
  return {
    title: "owner checklist",
    sourceFile: "/x/13j.org",
    repo: "/tmp/repo",
    integrationBranch: "main",
    checks: ["true"],
    phases: [p],
  };
}

test("owner checklist: not part of the contract, so absent from the worker and reviewer prompts", () => {
  const contract = buildContract(phase());
  assert.deepEqual(contract.acceptance, ["`transport_envelope_round_trips_source_tick` passes", "all existing tests still pass"]);
  assert.ok(!("ownerChecklist" in (contract as unknown as Record<string, unknown>)));

  const worker = buildWorkerPrompt(contract);
  assert.doesNotMatch(worker, /the owner records a live/);
  assert.doesNotMatch(worker, /the owner rules on the n>=4 blend/);
  // The acceptance items are still there, word for word.
  assert.match(worker, /transport_envelope_round_trips_source_tick/);

  const reviewerState = {
    phaseId: "13.10",
    contract,
    candidate: { sha: "0123456789abcdef" },
  } as unknown as PhaseState;
  const reviewer = buildReviewerPrompt(reviewerState, "M");
  assert.doesNotMatch(reviewer, /the owner records a live/);
});

test("owner checklist: `tt summary` (PR body) carries each item as an open checkbox", () => {
  const runDir = fs.mkdtempSync(path.join("/tmp", "tt-owner-checklist-"));
  try {
    const md = prSummary(runDir, plan(phase()));
    assert.match(md, /### Owner checklist/);
    assert.match(md, /- \[ \] the owner records a live `ips-split` run with the five keys exported/);
    assert.match(md, /- \[ \] the owner rules on the n>=4 blend/);
  } finally {
    fs.rmSync(runDir, { recursive: true, force: true });
  }
});

test("owner checklist: a plan without one adds no section to the PR body", () => {
  const runDir = fs.mkdtempSync(path.join("/tmp", "tt-owner-checklist-"));
  try {
    const p = phase();
    delete p.ownerChecklist;
    delete p.ownerChecklistLines;
    const md = prSummary(runDir, plan(p));
    assert.doesNotMatch(md, /Owner checklist/);
  } finally {
    fs.rmSync(runDir, { recursive: true, force: true });
  }
});
