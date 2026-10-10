// 06k1's lesson (2026-10-10): C1's `:VERIFY: test "plan 06g: …"` lived in
// test/conductor/lanes.test.ts while the phase ran `check-e2e FILES="…"`
// without it. The probe reported the test missing on every candidate, and a
// missing named test is a gate no owner command waives, so an all-met
// candidate could never be accepted. `tt lint` now refuses such a plan.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";

import { checkVerifyReach, type LintPlanInput } from "../../src/core/plan-lint.ts";
import { readTestSelection, readVerifyReachFacts } from "../../src/core/repo-facts.ts";

/** A git repository with `files` committed (git grep reads tracked files). */
function repo(files: Record<string, string>): string {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-verify-reach-"));
  for (const [rel, body] of Object.entries(files)) {
    fs.mkdirSync(path.dirname(path.join(root, rel)), { recursive: true });
    fs.writeFileSync(path.join(root, rel), body);
  }
  const git = (...args: string[]) => execFileSync("git", ["-C", root, ...args], { stdio: "ignore" });
  git("init", "-q");
  git("add", "-A");
  git("-c", "user.email=t@t", "-c", "user.name=t", "commit", "-q", "-m", "fixture");
  return root;
}

/** tt's own Makefile shape: a fast `check` over unit globs, and `check-e2e`
 * over the files a phase names. */
const MAKEFILE = [
  ".PHONY: check check-e2e test-fast",
  "check: test-fast",
  "check-e2e:",
  '\t@test -n "$(FILES)" || { echo \'usage: make check-e2e FILES="…"\' >&2; exit 2; }',
  "\tcd $(CURDIR) && TT_NOTIFY_COMMAND=':' node --test --test-force-exit --test-concurrency=3 $(FILES)",
  "test-fast:",
  "\tcd $(CURDIR) && TT_NOTIFY_COMMAND=':' node --test --test-force-exit 'test/unit/**/*.test.ts'",
  "",
].join("\n");

function plan(root: string, checks: string, verify: string[]): LintPlanInput {
  return {
    repo: root,
    sourceFile: "plan.org",
    phases: [{ id: "p1", checks: [checks], requirements: verify.map((v, i) => ({ id: `R${i + 1}`, verify: [v], verifyLine: 10 + i })) }],
  };
}

const lint = (p: LintPlanInput) => checkVerifyReach(p, readVerifyReachFacts(p)).filter((f) => f.rule === "verify-reach");

test("verify-reach: a named test defined only in a conductor file the check-e2e list omits is a lint error", () => {
  const root = repo({
    "tt/Makefile": MAKEFILE,
    "tt/test/unit/core.test.ts": 'test("a unit test the fast check runs", () => {});\n',
    "tt/test/conductor/lanes.test.ts": 'test("plan 06g: b wins 2-1", () => {});\n',
    "tt/test/conductor/lane-value.test.ts": 'test("a lane-value test", () => {});\n',
  });
  try {
    const checks = 'make -C tt check && make -C tt check-e2e FILES="test/conductor/lane-value.test.ts"';
    const findings = lint(plan(root, checks, ['test "plan 06g: b wins 2-1"', 'test "a unit test the fast check runs"', 'test "a lane-value test"']));
    assert.equal(findings.length, 1, JSON.stringify(findings));
    assert.equal(findings[0].severity, "error");
    assert.equal(findings[0].item, "plan 06g: b wins 2-1");
    assert.equal(findings[0].line, 10);
    assert.match(findings[0].problem, /tt\/test\/conductor\/lanes\.test\.ts/);
    assert.match(findings[0].fix, /check-e2e FILES/);

    // Naming the file in the e2e list clears it.
    const fixed = 'make -C tt check && make -C tt check-e2e FILES="test/conductor/lane-value.test.ts test/conductor/lanes.test.ts"';
    assert.deepEqual(lint(plan(root, fixed, ['test "plan 06g: b wins 2-1"'])), []);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("verify-reach: a test the worker has not written yet, or a check the lint cannot resolve, is never an error", () => {
  const root = repo({
    "tt/Makefile": MAKEFILE,
    "tt/test/conductor/lanes.test.ts": 'test("plan 06g: b wins 2-1", () => {});\n',
    "scripts/ci.sh": "#!/bin/sh\nnode --test\n",
  });
  try {
    const checks = 'make -C tt check-e2e FILES="test/conductor/other.test.ts"';
    assert.deepEqual(lint(plan(root, checks, ['test "a test the worker will write"'])), [], "no site yet: not judged");
    assert.deepEqual(lint(plan(root, "./scripts/ci.sh", ['test "plan 06g: b wins 2-1"'])), [], "a script could run anything");
    assert.deepEqual(lint(plan(root, "npm test", ['test "plan 06g: b wins 2-1"'])), [], "a package script is unknown");
    assert.deepEqual(lint(plan(root, "make -C tt no-such-target", ['test "plan 06g: b wins 2-1"'])), [], "an unknown goal is unknown");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("verify-reach: a Rust test in a crate the cargo test -p list omits is a lint error naming the crate", () => {
  const root = repo({
    "Cargo.toml": '[workspace]\nmembers = ["crates/a", "crates/b"]\n',
    "crates/a/Cargo.toml": '[package]\nname = "a"\nversion = "0.1.0"\n',
    "crates/a/src/lib.rs": "#[test]\nfn a_is_checked() {}\n",
    "crates/b/Cargo.toml": '[package]\nname = "b"\nversion = "0.1.0"\n',
    "crates/b/tests/flow.rs": "#[test]\nfn the_runner_records_every_fact() {}\n",
  });
  try {
    const verify = ['test "the_runner_records_every_fact"', 'test "a_is_checked"'];
    const findings = lint(plan(root, "cargo fmt --all -- --check && SKIP=1 cargo test -p a", verify));
    assert.equal(findings.length, 1, JSON.stringify(findings));
    assert.match(findings[0].problem, /crates\/b\/tests\/flow\.rs \(b\)/);
    assert.match(findings[0].fix, /-p b/);

    assert.deepEqual(lint(plan(root, "cargo test -p a -p b", verify)), [], "-p b clears it");
    assert.deepEqual(lint(plan(root, "cargo test --workspace", verify)), [], "a workspace run reaches every crate");
    assert.deepEqual(lint(plan(root, "cargo test", verify)), [], "no -p reaches every crate");
    // A module-path name is found by its last segment.
    const qualified = lint(plan(root, "cargo test -p a", ['test "flow::the_runner_records_every_fact"']));
    assert.equal(qualified.length, 1, JSON.stringify(qualified));
    assert.match(qualified[0].fix, /-p b/);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("verify-reach: make targets expand through prerequisites, $(CURDIR) and the call's variables", () => {
  const root = repo({ "tt/Makefile": MAKEFILE });
  try {
    const sel = readTestSelection(root, ['make -C tt check && make -C tt check-e2e FILES="test/conductor/a.test.ts test/conductor/b.test.ts"']);
    assert.equal(sel.nodeUnknown, false);
    assert.deepEqual(sel.nodeFiles.sort(), ["tt/test/conductor/a.test.ts", "tt/test/conductor/b.test.ts", "tt/test/unit/**/*.test.ts"]);
    assert.deepEqual(sel.cargo, []);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
