// Plan 01c: the pure plan linter (src/core/plan-lint.ts).
//
// A plan must not ask a worker to do the owner's job (13j's "the owner records
// a live run" parked the phase), must not depend on a future the gate cannot
// see (13i's "the current rebased tip"), and should say what tolerance a
// comparison allows (13i's "p99 ≤ its contracted interval" made every vendor a
// gap). The last block runs the real atlas plans copied into
// test/fixtures/atlas-plans/ and asserts the linter finds exactly one error —
// 13j's owner-actor item — and no false error anywhere else.

import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { formatFinding, hasLintErrors, lintPlan, lintProgram, type LintFinding, type LintPhaseInput, type LintPlanInput } from "../../src/core/plan-lint.ts";

const FIXTURES = fileURLToPath(new URL("../fixtures/atlas-plans", import.meta.url));

const plan = (phases: LintPhaseInput[], sourceFile = "/x/PLAN.org"): LintPlanInput => ({ sourceFile, phases });

test("plan-lint: an owner-actor acceptance item is an error with its line", () => {
  const findings = lintPlan(
    plan([{ id: "13.10", acceptance: ["the owner records a live run", "the report is in the repo"], acceptanceLines: [39, 40] }]),
  );
  assert.equal(findings.length, 1);
  assert.equal(findings[0].severity, "error");
  assert.equal(findings[0].rule, "owner-actor");
  assert.equal(findings[0].phaseId, "13.10");
  assert.equal(findings[0].line, 39);
  assert.match(findings[0].item, /the owner records/);
  // The message says how to rewrite it: to the Owner checklist, or a result a
  // worker produces.
  assert.match(findings[0].fix, /Owner checklist/);
  assert.equal(hasLintErrors(findings), true);
  // The formatted finding carries file:line: severity: for compilation-mode.
  assert.match(formatFinding(findings[0]), /^\/x\/PLAN\.org:39: error: \[13\.10\]/);
});

test("plan-lint: a human actor (manually / someone) is an error too", () => {
  const findings = lintPlan(plan([{ id: "p", acceptance: ["the migration is done manually", "someone approves the release"] }]));
  assert.deepEqual(findings.map((f) => f.rule), ["human-actor", "human-actor"]);
  assert.ok(findings.every((f) => f.severity === "error" && f.line === undefined));
});

test("plan-lint: the passive owner phrasing the real plans use is not an error", () => {
  // 13c-13f/14a-14e and 14f all qualify a noun with "owner"; the owner is not
  // the actor, so no error (a warning may still fire for other reasons).
  const findings = lintPlan(
    plan([
      { id: "13.5", acceptance: ["the owner live check is recorded in the phase decisions"] },
      { id: "14.6", acceptance: ["the owner's K4 ruling is recorded in the phase decisions with its blast radius"] },
      { id: "12d", acceptance: ["the owner end-to-end gate is run and recorded with the code SHA"] },
    ]),
  );
  assert.equal(hasLintErrors(findings), false);
});

test("plan-lint: future-dependent items warn", () => {
  const findings = lintPlan(
    plan([
      {
        id: "13.9",
        acceptance: ["the recorded live-run SHA is the current rebased tip", "the gate is rerun after merge", "the endpoint serves traffic once deployed"],
        acceptanceLines: [34, 35, 36],
      },
    ]),
  );
  assert.deepEqual(findings.map((f) => f.rule), ["future-dependency", "future-dependency", "future-dependency"]);
  assert.ok(findings.every((f) => f.severity === "warning"));
  assert.deepEqual(findings.map((f) => f.line), [34, 35, 36]);
  assert.equal(hasLintErrors(findings), false);
});

test("plan-lint: a comparison with no stated tolerance warns; a margin or a gap clears it", () => {
  const bare = lintPlan(plan([{ id: "13.9", acceptance: ["each fragment's measured p99 is ≤ its contracted interval"] }]));
  assert.deepEqual(bare.map((f) => f.rule), ["no-tolerance"]);
  assert.equal(bare[0].severity, "warning");
  assert.match(bare[0].fix, /margin|recorded gap/);

  const withMargin = lintPlan(
    plan([
      // An explicit margin, a factor, or a named fallback all say what a miss
      // means, so no warning.
      {
        id: "13.3",
        acceptance: [
          "p99 ≤ 200 ms plus network jitter (contracted interval)",
          "p99 ≤ the contracted interval × 1.1",
          "no row exceeds the contracted band, or the market is a recorded gap",
        ],
      },
    ]),
  );
  assert.equal(withMargin.length, 0);
});

test("plan-lint: a clean plan has no findings", () => {
  const findings = lintPlan(
    plan([
      {
        id: "p1",
        acceptance: [
          "`blend_runs_over_the_real_adapters` passes",
          "the file moves to inbox/applied/",
          "`cargo test --workspace` passes",
          "all existing tests still pass",
        ],
      },
    ]),
  );
  assert.deepEqual(findings, []);
});

test("plan-lint: a program lints every entry and prefixes the phase id", () => {
  const findings = lintProgram({
    entries: [
      { id: "13a", plan: plan([{ id: "p1", acceptance: ["fine"] }]) },
      { id: "13j", plan: plan([{ id: "ips13-process-split", acceptance: ["the owner records a live run"] }], "/x/13j.org") },
    ],
  });
  assert.equal(findings.length, 1);
  assert.equal(findings[0].phaseId, "13j/ips13-process-split");
  assert.equal(findings[0].sourceFile, "/x/13j.org");
});

// ---------------------------------------------------------------------------
// The real plans: exactly one error, and it is 13j's owner-actor item.
// ---------------------------------------------------------------------------

/** The linter's slice of an Org plan, extracted the way `+tt-parse-plan`
 * does: every level-1 heading with an `Acceptance:` list on its own line, and
 * the `- ` items that follow it, with their line numbers. Only enough Org to
 * feed the rules — the rules themselves are what this test exercises. */
function extractOrgPlans(text: string): LintPhaseInput[] {
  const lines = text.split("\n");
  const phases: LintPhaseInput[] = [];
  let cur: LintPhaseInput | undefined;
  for (let i = 0; i < lines.length; i++) {
    const heading = /^\* (.+)$/.exec(lines[i]);
    if (heading) {
      cur = { id: heading[1], acceptance: [], acceptanceLines: [] };
      phases.push(cur);
      continue;
    }
    if (cur && /^\s*Acceptance:\s*$/.test(lines[i])) {
      let j = i + 1;
      while (j < lines.length && /^\s*- /.test(lines[j])) {
        cur.acceptance!.push(lines[j].replace(/^\s*- /, ""));
        cur.acceptanceLines!.push(j + 1);
        j++;
      }
      i = j - 1;
    }
  }
  return phases;
}

test("plan-lint: the existing atlas plans have no false error except 13j's owner item", () => {
  const files = readdirSync(FIXTURES).filter((f) => f.endsWith(".org")).sort();
  assert.ok(files.length >= 20, `expected the atlas plan fixtures, found ${files.length}`);
  const errors: Array<{ file: string; finding: LintFinding }> = [];
  let warnings = 0;
  for (const file of files) {
    const text = readFileSync(path.join(FIXTURES, file), "utf8");
    const findings = lintPlan({ sourceFile: file, phases: extractOrgPlans(text) });
    for (const f of findings) {
      if (f.severity === "error") errors.push({ file, finding: f });
      else warnings += 1;
    }
  }
  assert.equal(
    errors.length,
    1,
    `expected exactly one error (13j's owner item); got:\n${errors.map((e) => `${e.file}: ${formatFinding(e.finding, e.file)}`).join("\n")}`,
  );
  assert.equal(errors[0].file, "13j_process_split.org");
  assert.equal(errors[0].finding.rule, "owner-actor");
  assert.match(errors[0].finding.item, /the owner records a live/);
  // The line points at the item in the Org file (13j line 39), so the owner
  // can jump straight to it.
  assert.equal(errors[0].finding.line, 39);
  // Warnings are allowed on the real plans; they never stop a run.
  assert.ok(warnings >= 1, "expected at least the 13i future/tolerance warnings");
});
