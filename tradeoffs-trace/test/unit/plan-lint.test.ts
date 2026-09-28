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

test("plan-lint: #+TT_RERUN accepts {name}/{file} and rejects any other placeholder", () => {
  const base = plan([{ id: "p", acceptance: ["it works"] }]);
  assert.deepEqual(lintPlan(base).filter((f) => f.rule === "rerun-template"), []);

  const ok = lintPlan({ ...base, rerun: "node --test --test-name-pattern {name} {file}", rerunLine: 5 });
  assert.deepEqual(ok.filter((f) => f.rule === "rerun-template"), []);

  const unknown = lintPlan({ ...base, rerun: "cargo test --test {file} -- --exact {crate}", rerunLine: 6 });
  const finding = unknown.find((f) => f.rule === "rerun-template");
  assert.ok(finding, "the unknown placeholder must be reported");
  assert.equal(finding!.severity, "error");
  assert.equal(finding!.phaseId, "rerun");
  assert.equal(finding!.line, 6);
  assert.match(finding!.problem, /unknown placeholder \{crate\}/);
  assert.match(finding!.fix, /\{name\}/);

  const noName = lintPlan({ ...base, rerun: "sh -c 'exit 0'", rerunLine: 7 });
  assert.match(noName.find((f) => f.rule === "rerun-template")!.problem, /does not name the failing test/);
});

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

test("plan-lint: #+TT_MODELS rejects an unknown role, a repeated role and an empty model", () => {
  const base = plan([{ id: "p1", acceptance: ["fine"] }]);
  // A plan without the keyword: no model findings at all.
  assert.deepEqual(lintPlan(base).filter((f) => f.rule === "model-declaration"), []);

  const unknown = lintPlan({ ...base, models: { foo: { model: "x" } }, modelsLine: 7 });
  assert.deepEqual(unknown.map((f) => f.rule), ["model-declaration"]);
  assert.equal(unknown[0].severity, "error");
  assert.equal(unknown[0].line, 7);
  assert.equal(unknown[0].phaseId, "models");
  assert.match(unknown[0].problem, /unknown role foo/);
  assert.match(unknown[0].fix, /worker, reviewer, evaluator, panel/);

  // A JSON object cannot hold a duplicate key, so the parser records the
  // role it saw twice; the linter reports it.
  const repeated = lintPlan({ ...base, models: { worker: { model: "b" } }, modelsRepeated: ["worker"], modelsLine: 3 });
  assert.deepEqual(repeated.map((f) => f.rule), ["model-declaration"]);
  assert.equal(repeated[0].line, 3);
  assert.match(repeated[0].problem, /worker more than once/);

  // An empty (or missing) model is an error: it would reach launchArgs as a
  // bare --model with nothing after it.
  const empty = lintPlan({ ...base, models: { reviewer: {} }, modelsLine: 12 });
  assert.deepEqual(empty.map((f) => f.rule), ["model-declaration"]);
  assert.equal(empty[0].line, 12);
  assert.match(empty[0].problem, /empty model/);

  // A well-formed declaration (provider optional, model with a slash) passes.
  const ok = lintPlan({
    ...base,
    models: { worker: { model: "deepseek/deepseek-v4.1-flash" }, reviewer: { provider: "vercel-ai-gateway", model: "anthropic/claude-sonnet-5" } },
  });
  assert.deepEqual(ok, []);
});

test("plan-lint: a program's own #+TT_MODELS is reported once, not per entry", () => {
  // Mirrors what Emacs emits: the program's `foo` is copied into the entry's
  // `models` and recorded in `modelsFromProgram`, so the entry must not
  // re-report it (which would name the program's line against the entry file).
  const findings = lintProgram({
    sourceFile: "/x/program.org",
    models: { foo: { model: "x" } },
    modelsLine: 2,
    entries: [
      {
        id: "e1",
        plan: {
          ...plan([{ id: "p1", acceptance: ["fine"] }], "/x/e1.org"),
          models: { foo: { model: "x" }, bar: { model: "y" } },
          modelsLine: 5,
          modelsFromProgram: ["foo"],
        },
      },
    ],
  });
  // Once for the program's `foo`, at the program's own file and line; once
  // for the entry's own `bar`, at the entry's file and line.
  assert.equal(findings.length, 2);
  const programFinding = findings.find((f) => f.sourceFile === "/x/program.org")!;
  assert.match(programFinding.problem, /unknown role foo/);
  assert.equal(programFinding.line, 2);
  assert.equal(programFinding.phaseId, "models");
  const entryFinding = findings.find((f) => f.sourceFile === "/x/e1.org")!;
  assert.match(entryFinding.problem, /unknown role bar/);
  assert.equal(entryFinding.line, 5);
  assert.equal(entryFinding.phaseId, "e1/models");
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

test("plan-lint: #+TT_MODELS rejects an unknown reviewer/panel seat with file and line", () => {
  const base = plan([{ id: "p1", acceptance: ["fine"] }], "/x/PLAN.org");
  const unknownSeat = lintPlan({ ...base, models: { reviewerSeats: { X: { model: "x" } } }, modelsLine: 4 });
  assert.deepEqual(unknownSeat.map((f) => f.rule), ["model-declaration"]);
  assert.equal(unknownSeat[0].severity, "error");
  assert.equal(unknownSeat[0].line, 4);
  assert.equal(unknownSeat[0].sourceFile, "/x/PLAN.org");
  assert.match(unknownSeat[0].problem, /unknown reviewer seat reviewer\.X/);
  assert.match(unknownSeat[0].fix, /reviewer\.M, reviewer\.A or reviewer\.B/);

  const unknownPanel = lintPlan({ ...base, models: { panelSeats: { "4": { model: "y" } } }, modelsLine: 6 });
  assert.match(unknownPanel[0].problem, /unknown panel seat panel\.4/);
  assert.match(unknownPanel[0].fix, /panel\.1, panel\.2 or panel\.3/);
});

test("plan-lint: a seat named twice is an error at the keyword's line", () => {
  const base = plan([{ id: "p1", acceptance: ["fine"] }], "/x/PLAN.org");
  const findings = lintPlan({ ...base, models: { reviewerSeats: { M: { model: "b" } } }, modelsRepeated: ["reviewer.M"], modelsLine: 5 });
  assert.deepEqual(findings.map((f) => f.rule), ["model-declaration"]);
  assert.equal(findings[0].line, 5);
  assert.match(findings[0].problem, /reviewer\.M more than once/);
});

test("plan-lint: panel=reviewers with an explicit panel.N is an error", () => {
  const base = plan([{ id: "p1", acceptance: ["fine"] }], "/x/PLAN.org");
  const findings = lintPlan({
    ...base,
    models: { panelFrom: "reviewers", reviewerSeats: { M: { model: "m" } }, panelSeats: { "2": { model: "own" } } },
    modelsLine: 8,
  });
  assert.equal(findings.length, 1);
  assert.equal(findings[0].severity, "error");
  assert.equal(findings[0].line, 8);
  assert.equal(findings[0].sourceFile, "/x/PLAN.org");
  assert.match(findings[0].problem, /panel=reviewers together with an explicit panel seat/);
});

test("plan-lint: a full per-seat declaration passes and an empty seat model fails", () => {
  const base = plan([{ id: "p1", acceptance: ["fine"] }], "/x/PLAN.org");
  const ok = lintPlan({
    ...base,
    models: {
      worker: { model: "deepseek/deepseek-v4.1-flash" },
      reviewerSeats: { M: { provider: "vercel-ai-gateway", model: "anthropic/claude-opus-5.5" }, A: { model: "deepseek/deepseek-v4.1-flash" } },
      evaluator: { model: "anthropic/claude-opus-5.5" },
      panelFrom: "reviewers",
    },
    modelsLine: 3,
  });
  assert.deepEqual(ok, []);

  const empty = lintPlan({ ...base, models: { reviewerSeats: { B: {} } }, modelsLine: 3 });
  assert.deepEqual(empty.map((f) => f.rule), ["model-declaration"]);
  assert.match(empty[0].problem, /empty model/);
  assert.match(empty[0].problem, /reviewer\.B/);
});
