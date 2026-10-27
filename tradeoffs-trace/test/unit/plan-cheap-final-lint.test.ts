// tt 09-1: `tt lint` warns, per phase, for each cheap top-level segment of
// #+TT_FINAL_CHECKS that #+TT_CHECKS does not also run. exex 01a's "green"
// candidates with the work undone passed every round check because the cheap
// mechanical checks (rg, git diff, jq, test) ran only at acceptance. The
// commands below are shaped like exex 01's real plans.

import assert from "node:assert/strict";
import { test } from "node:test";

import { isCheapCheckSegment, lintCheapFinalChecks, lintPlan, topLevelAndSegments, type LintPlanInput } from "../../src/core/plan-lint.ts";
import { parseOrgPlan } from "../../src/core/org-plan.ts";

const ROUND = "cargo fmt --all -- --check && cargo clippy -p tcl-equivalence --all-targets -- -D warnings && cargo test -p tcl-equivalence";

const FINAL = [
  "TCL_EQ_LOGICAL_ONLY=1 cargo test --release -p tcl-equivalence --test gates -- --include-ignored",
  "bash testing/tcl-equivalence/reference.sh --marketdata --check",
  "cargo run --release -p tcl-equivalence --bin tcl-compare -- --goldens testing/tcl-equivalence/goldens > target/md-compare.txt",
  `test "$(grep -c '^MATCH ' target/md-compare.txt)" -eq "$(ls testing/tcl-equivalence/goldens/marketdata/*.jsonl.lz4 | wc -l)"`,
  "jq -e '[.items[] | select(.count == 0 and .unreachable == null)] | length == 0' testing/tcl-equivalence/coverage.json",
  "git diff --quiet feat/tcl-v2--01a1 -- crates testing/benchmarks Cargo.toml matrix.yaml",
  `test -z "$(rg -n --type rust 'Vec<PositionFact>' crates/modules/prm/domain/src && true)"`,
  "! grep -n 'finalizer' crates/modules/trade-core/replica/Cargo.toml",
  "cargo xtask check workspace",
  "scripts/verus-verify.sh prm-domain",
  'test "$(du -sk testing/tcl-equivalence | cut -f1)" -le 16384',
].join(" && ");

function plan(checks: string[], finalChecks: string[], phaseChecks?: string[]): LintPlanInput {
  return { checks, phases: [{ id: "p1", line: 7, finalChecks, ...(phaseChecks ? { checks: phaseChecks } : {}) }] };
}

test("tt 09-1: a chain splits only at top-level &&, never inside $( ), quotes or backticks", () => {
  assert.deepEqual(topLevelAndSegments("a && b&&c"), ["a", "b", "c"]);
  assert.deepEqual(topLevelAndSegments(`test -z "$(rg x && rg y)" && echo 'a && b' && \`p && q\` && (c && d)`), [
    `test -z "$(rg x && rg y)"`,
    "echo 'a && b'",
    "`p && q`",
    "(c && d)",
  ]);
  assert.deepEqual(topLevelAndSegments("a || b ; c | d && e"), ["a || b ; c | d", "e"], "only && splits");
  assert.equal(topLevelAndSegments(FINAL).length, 11);
});

test("tt 09-1: cheap means no cargo, npm/ast-grep, Verus, reference.sh or release build", () => {
  for (const cheap of [
    "git diff --quiet feat/tcl-v2--01a1 -- crates",
    "git status --porcelain -- testing",
    "! rg -n 'cargo test' crates",
    "jq -e '.items | length == 0' coverage.json",
    'test "$(du -sk testing/tcl-equivalence | cut -f1)" -le 16384',
    "python3 scripts/check_names.py",
    "cmp a.rs b.rs",
  ]) {
    assert.equal(isCheapCheckSegment(cheap), true, cheap);
  }
  for (const slow of [
    "cargo test -p tcl-equivalence",
    "TCL_EQ_LOGICAL_ONLY=1 cargo test --release -p x",
    "cargo xtask check workspace",
    "bash testing/tcl-equivalence/reference.sh --check",
    "testing/tcl-equivalence/reference.sh --marketdata --check",
    "scripts/verus-verify.sh prm-domain",
    "npm test",
    "ast-grep scan",
    "cargo-nextest run",
    `test -z "$(cargo tree -p tcl-equivalence -e normal | grep finalizer)"`,
    "make build --release",
  ]) {
    assert.equal(isCheapCheckSegment(slow), false, slow);
  }
});

test("tt 09-1: each cheap acceptance-only segment warns once with the owner's wording", () => {
  const findings = lintCheapFinalChecks(plan([ROUND], [FINAL]));
  assert.deepEqual(
    findings.map((f) => f.item),
    [
      "jq -e '[.items[] | select(.count == 0 and .unreachable == null)] | length == 0' testing/tcl-equivalence/coverage.json",
      "git diff --quiet feat/tcl-v2--01a1 -- crates testing/benchmarks Cargo.toml matrix.yaml",
      `test -z "$(rg -n --type rust 'Vec<PositionFact>' crates/modules/prm/domain/src && true)"`,
      "! grep -n 'finalizer' crates/modules/trade-core/replica/Cargo.toml",
      'test "$(du -sk testing/tcl-equivalence | cut -f1)" -le 16384',
    ],
    "the MATCH count reads what the slow tcl-compare run wrote, so it is not reported",
  );
  const f = findings[1];
  assert.equal(f.severity, "warning");
  assert.equal(f.rule, "cheap-final-check");
  assert.equal(f.phaseId, "p1");
  assert.equal(f.line, 7);
  assert.equal(
    f.problem,
    "the acceptance-only check `git diff --quiet feat/tcl-v2--01a1 -- crates testing/benchmarks Cargo.toml matrix.yaml` is cheap; put it in #+TT_CHECKS too, so a candidate that has not done it cannot go green",
  );
});

test("tt 09-1: a segment the round checks already run is not reported, from #+TT_CHECKS or the phase's :CHECKS:", () => {
  const diff = "git diff --quiet base -- crates";
  const grep = "! grep -n finalizer Cargo.toml";
  assert.deepEqual(lintCheapFinalChecks(plan([`${ROUND} && ${diff}`], [`cargo test --release && ${diff}`])), []);
  assert.deepEqual(lintCheapFinalChecks(plan([ROUND], [`${grep} && ${diff}`], [`${grep}  &&  ${diff}`])), [], "whitespace does not matter");
  assert.deepEqual(lintCheapFinalChecks(plan([ROUND], [`cd testing && ${diff}`], [diff])), [], "a bare cd is setup, not a check");
  assert.deepEqual(lintCheapFinalChecks(plan([ROUND], ["cargo test --release -p x"])), [], "slow work stays final-only");
  assert.deepEqual(
    lintCheapFinalChecks(plan([ROUND], ["cargo run --release --bin tcl-replay -- --timing target/t.json && jq -e '.logs | length == 2' target/t.json"])),
    [],
    "a jq over what the slow replay wrote under target/ cannot run alone",
  );
  assert.deepEqual(lintCheapFinalChecks(plan([ROUND], [])), []);
});

test("tt 09-1: `tt lint` on an Org plan reports it, with #+TT_CHECKS read from the plan", () => {
  const org = [
    "#+TITLE: exex",
    `#+TT_CHECKS: ${ROUND}`,
    "#+TT_FINAL_CHECKS: cargo test --release -p tcl-equivalence -- --ignored && git status --porcelain -- testing/tcl-equivalence/goldens",
    "* Phase one",
    "  :PROPERTIES:",
    "  :ID: p1",
    "  :END:",
    "** Goal",
    "   Keep the goldens frozen.",
    "",
  ].join("\n");
  const parsed = parseOrgPlan(org, "plan.org");
  assert.deepEqual(parsed.checks, [ROUND]);
  const warnings = lintPlan(parsed).filter((f) => f.rule === "cheap-final-check");
  assert.deepEqual(warnings.map((w) => w.item), ["git status --porcelain -- testing/tcl-equivalence/goldens"]);
  assert.equal(warnings[0].sourceFile, "plan.org");
});
