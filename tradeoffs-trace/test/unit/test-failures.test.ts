// Plan 01e: the pure half of the base baseline. Recorded check output of the
// three runners the plan names (cargo, node:test, ERT) must yield exactly the
// failing test names it names — and output that names nothing must yield none,
// because D2's default then keeps the strict rule and never hides a failure.

import assert from "node:assert/strict";
import { test } from "node:test";

import { baselineCoversCommands, baselineFailedCommands, baselineFailureNames, baselineKey, baselineStatusLine, classifyCheckFailure, failedNormally, parseBaseline, parseTestFailures, type Baseline, type BaselineCommand, testIsNamedIn } from "../../src/core/test-failures.ts";

// A real `cargo test` tail: the per-test FAILED lines, the summary list, and
// the result line. Only the `test <name> ... FAILED` lines are names.
const CARGO_OUTPUT = [
  "   Compiling exchange-state-machine v0.1.0 (/repo)",
  "    Finished `test` profile [unoptimized] target(s) in 1.23s",
  "     Running unittests src/lib.rs (target/debug/deps/exchange_state_machine-abc123)",
  "",
  "running 3 tests",
  "test exchange_state_machine::tests::applies_fill ... ok",
  "test exchange_state_machine::tests::cancels_order ... FAILED",
  "test exchange_state_machine::tests::expires_order ... FAILED",
  "",
  "failures:",
  "",
  "    exchange_state_machine::tests::cancels_order",
  "    exchange_state_machine::tests::expires_order",
  "",
  "test result: FAILED. 1 passed; 2 failed; 0 ignored; 0 measured; 0 filtered out",
  "",
].join("\n");

// node --test with its TAP reporter (`--test-reporter=tap`).
const NODE_TAP_OUTPUT = [
  "TAP version 13",
  "# Subtest: alpha fails",
  "not ok 1 - alpha fails",
  "  ---",
  "  duration_ms: 1.076",
  "  type: 'test'",
  "  location: '/tmp/x.test.js:3:1'",
  "  ---",
  "ok 2 - beta passes",
  "1..2",
  "# failing tests: 1",
].join("\n");

// node --test with its default spec reporter (node 25 uses it even piped).
const NODE_SPEC_OUTPUT = [
  "✖ alpha fails (0.961ms)",
  "✔ beta passes (0.062208ms)",
  "ℹ tests 2",
  "ℹ pass 1",
  "ℹ fail 1",
  "✖ failing tests:",
  "",
  "test at x.test.js:3:1",
  "✖ alpha fails (0.961ms)",
].join("\n");

// ERT batch output: the per-test line carries a counter, the trailing summary
// line does not; both name the same test.
const ERT_OUTPUT = [
  "Running 2 tests (2026-09-25 17:56:41+0800, selector ‘t’)",
  "   passed  1/2  probe-passing-test (0.000024 sec)",
  "   FAILED  2/2  probe-failing-test (0.000048 sec) at ../../tmp/t.el:1",
  "",
  "Ran 2 tests, 1 results as expected, 1 unexpected (2026-09-25 17:56:45+0800, 0.025030 sec)",
  "",
  "1 unexpected results:",
  "   FAILED  probe-failing-test",
].join("\n");

test("test-failures: cargo's `test <name> ... FAILED` lines are parsed", () => {
  assert.deepEqual(parseTestFailures(CARGO_OUTPUT), [
    "exchange_state_machine::tests::cancels_order",
    "exchange_state_machine::tests::expires_order",
  ]);
});

test("test-failures: node:test TAP `not ok N - <name>` lines are parsed", () => {
  assert.deepEqual(parseTestFailures(NODE_TAP_OUTPUT), ["alpha fails"]);
});

test("test-failures: the node spec reporter's `✖ <name>` lines are parsed, without its summary heading", () => {
  assert.deepEqual(parseTestFailures(NODE_SPEC_OUTPUT), ["alpha fails"]);
});

test("test-failures: ERT `FAILED <name>` lines are parsed once, with or without the counter", () => {
  assert.deepEqual(parseTestFailures(ERT_OUTPUT), ["probe-failing-test"]);
});

test("test-failures: ansi color codes do not hide a failing name", () => {
  const colored = "test exchange_state_machine::tests::cancels_order ... \u001b[31mFAILED\u001b[0m\n";
  assert.deepEqual(parseTestFailures(colored), ["exchange_state_machine::tests::cancels_order"]);
});

test("test-failures: output that names no test yields none", () => {
  const compileError = [
    "   Compiling atlas v0.1.0",
    "error[E0425]: cannot find value `x` in this scope",
    "  --> src/lib.rs:3:5",
    "error: could not compile `atlas` (lib test) due to 1 previous error",
    "",
  ].join("\n");
  assert.deepEqual(parseTestFailures(compileError), []);
  assert.deepEqual(parseTestFailures(""), []);
  assert.deepEqual(parseTestFailures("exit 1\n"), []);
});

test("test-failures: D2 excuses a failure only when every name already failed on the base", () => {
  const failing = "test a ... FAILED\ntest b ... FAILED\n";
  const onlyBase = classifyCheckFailure(failing, ["a", "b", "c"]);
  assert.deepEqual(onlyBase.parsed, ["a", "b"]);
  assert.deepEqual(onlyBase.newFailures, []);
  assert.equal(onlyBase.excused, true);

  const oneNew = classifyCheckFailure(failing, ["a"]);
  assert.deepEqual(oneNew.newFailures, ["b"]);
  assert.equal(oneNew.excused, false, "one new name must fail the check and be named");

  const noParsedNames = classifyCheckFailure("error: build failed\n", ["a"]);
  assert.deepEqual(noParsedNames.parsed, []);
  assert.equal(noParsedNames.excused, false, "unparsable output keeps the strict rule");

  const noBaseFailures = classifyCheckFailure(failing, []);
  assert.deepEqual(noBaseFailures.newFailures, ["a", "b"]);
  assert.equal(noBaseFailures.excused, false);
});

test("test-failures: the baseline key depends on the base tree and the command list", () => {
  const tree = "0123456789abcdef0123456789abcdef01234567";
  assert.equal(baselineKey(tree, ["make check"]), baselineKey(tree, ["make check"]));
  assert.notEqual(baselineKey(tree, ["make check"]), baselineKey(tree, ["cargo test --workspace"]));
  assert.notEqual(baselineKey(tree, ["make check"]), baselineKey("f".repeat(40), ["make check"]));
});

test("test-failures: a record no longer covering the effective check list is not trusted by the status", () => {
  const record: Baseline = {
    baseSha: "a".repeat(40),
    tree: "t".repeat(40),
    key: "k",
    at: "",
    commands: [
      { command: "make check", exitCode: 1, timedOut: false, durationMs: 5, failures: ["x"] },
      { command: "cargo test --workspace", exitCode: 101, timedOut: false, durationMs: 5, failures: ["y"] },
    ],
    failures: ["x", "y"],
  };
  assert.equal(baselineCoversCommands(record, ["make check", "cargo test --workspace"]), true);
  // An amended contract changed the list: the record no longer describes what
  // the gate runs, so the status must not show it (finding A-6).
  assert.equal(baselineCoversCommands(record, ["make check", "make test"]), false);
  assert.equal(baselineCoversCommands(record, ["make check"]), false);
  assert.equal(baselineCoversCommands(record, ["make check", "cargo test --workspace", "extra"]), false);
  assert.equal(baselineCoversCommands(undefined, ["make check"]), false);
});

test("test-failures: baselineFailureNames dedupes across commands, in order", () => {
  const commands: BaselineCommand[] = [
    { command: "cargo test", exitCode: 101, timedOut: false, durationMs: 5, failures: ["x", "y"] },
    { command: "make check", exitCode: 2, timedOut: false, durationMs: 5, failures: ["y", "z"] },
  ];
  assert.deepEqual(baselineFailureNames(commands), ["x", "y", "z"]);
});

test("test-failures: only a completed non-zero exit may put names in the excuse set", () => {
  assert.equal(failedNormally({ exitCode: 1, signal: null, timedOut: false }), true);
  assert.equal(failedNormally({ exitCode: 0, signal: null, timedOut: false }), false, "exit 0 did not fail");
  assert.equal(failedNormally({ exitCode: null, signal: "SIGKILL", timedOut: false }), false, "a signal death printed no ending");
  assert.equal(failedNormally({ exitCode: 1, signal: null, timedOut: true }), false, "a timeout's output is truncated");
  // A command that exits 0 while printing FAILED lines (a `|| echo done`
  // wrapper) must not contribute names, and a signal death must not either.
  const commands: BaselineCommand[] = [
    { command: "wrapped", exitCode: 0, signal: null, timedOut: false, durationMs: 5, failures: ["x"] },
    { command: "oom", exitCode: null, signal: "SIGKILL", timedOut: false, durationMs: 5, failures: ["y"] },
    { command: "real", exitCode: 1, signal: null, timedOut: false, durationMs: 5, failures: ["z"] },
  ];
  const shown = baselineFailedCommands(commands);
  assert.deepEqual(shown.map((c) => c.command), ["real"], "only the normally-failed command is named as pre-existing");
});

test("test-failures: the status line reports a failing base, and nothing for a passing one", () => {
  const failing: Baseline = {
    baseSha: "a".repeat(40),
    tree: "t".repeat(40),
    key: "k",
    at: "",
    commands: [{ command: "cargo test", exitCode: 101, timedOut: false, durationMs: 5, failures: ["x", "y"] }],
    failures: ["x", "y"],
  };
  assert.equal(baselineStatusLine(failing), "base fails: 2 tests: x, y");

  const unparsable: Baseline = {
    ...failing,
    commands: [{ command: "cargo test", exitCode: 101, timedOut: false, durationMs: 5, failures: [] }],
    failures: [],
  };
  assert.match(baselineStatusLine(unparsable) ?? "", /base fails: 0 tests/);
  assert.match(baselineStatusLine(unparsable) ?? "", /strict/);

  const passing: Baseline = {
    ...failing,
    commands: [{ command: "cargo test", exitCode: 0, timedOut: false, durationMs: 5, failures: [] }],
    failures: [],
  };
  assert.equal(baselineStatusLine(passing), undefined);
  assert.equal(baselineStatusLine(undefined), undefined);
});

test("test-failures: parseBaseline rejects junk and recomputes a missing aggregate", () => {
  assert.equal(parseBaseline(undefined), undefined);
  assert.equal(parseBaseline("nope"), undefined);
  assert.equal(parseBaseline({ key: "k" }), undefined);
  const parsed = parseBaseline({
    baseSha: "a".repeat(40),
    tree: "b".repeat(40),
    key: "k",
    at: "t",
    commands: [{ command: "cargo test", exitCode: 1, failures: ["x"] }],
  });
  assert.ok(parsed);
  assert.deepEqual(parsed!.failures, ["x"]);
  assert.equal(parsed!.tree, "b".repeat(40));
  // A record written before the tree field existed has no identity to trust:
  // it parses, but as the empty tree — never equal to a real base's tree.
  const legacy = parseBaseline({ baseSha: "a", key: "k", commands: [] });
  assert.equal(legacy!.tree, "");
});

test("test-failures: a failing test the phase is required to fix is never excused as pre-existing (plan 14h)", () => {
  const output = "test gate_is_rerun_when_the_base_moves ... FAILED\ntest other_flaky ... FAILED\n";
  const base = ["gate_is_rerun_when_the_base_moves", "other_flaky"];
  // Without a requirement both match the base, so the check is excused.
  assert.equal(classifyCheckFailure(output, base).excused, true);
  // An owner directive names one of them: that one is the candidate's failure.
  const directive = "Re-run the live gate. `gate_is_rerun_when_the_base_moves` must PASS, not merely match the base.";
  const verdict = classifyCheckFailure(output, base, [directive]);
  assert.equal(verdict.excused, false);
  assert.deepEqual(verdict.newFailures, ["gate_is_rerun_when_the_base_moves"]);
  // Whole words only, and module paths match on the test's own name.
  assert.equal(testIsNamedIn("tests::coverage::gate_is_rerun_when_the_base_moves", [directive]), true);
  assert.equal(testIsNamedIn("gate_is_rerun", [directive]), false);
});
