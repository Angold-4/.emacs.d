// Plan 01e: the pure half of the base baseline. Recorded check output of the
// three runners the plan names (cargo, node:test, ERT) must yield exactly the
// failing test names it names — and output that names nothing must yield none,
// because D2's default then keeps the strict rule and never hides a failure.

import assert from "node:assert/strict";
import { test } from "node:test";

import { baselineCoversCommands, baselineFailedCommands, baselineFlakeNames, baselineFailureNames, baselineKey, baselineStatusLine, classifyCheckFailure, classifyRerun, escapeRegExp, failedNormally, parseBaseline, parseTestFailures, parseTestFailuresDetailed, rerunCommandsFor, rerunProvesTheTestRan, rerunTemplateIssue, shellQuote, singleTestCommand, type Baseline, type BaselineCommand, testIsNamedIn } from "../../src/core/test-failures.ts";

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

test("test-failures: the detailed parse tags the runner and node's own file", () => {
  const tap = [
    "not ok 1 - alpha fails",
    "  ---",
    "  duration_ms: 1.076",
    "  location: '/tmp/x.test.js:3:1'",
    "  ---",
    "ok 2 - beta passes",
  ].join("\n");
  assert.deepEqual(parseTestFailuresDetailed(tap), [{ name: "alpha fails", runner: "node", file: "/tmp/x.test.js" }]);

  const spec = [
    "test at y.test.js:7:1",
    "✖ gamma fails (0.5ms)",
    "ℹ tests 1",
  ].join("\n");
  assert.deepEqual(parseTestFailuresDetailed(spec), [{ name: "gamma fails", runner: "node", file: "y.test.js" }]);

  assert.deepEqual(parseTestFailuresDetailed(CARGO_OUTPUT).map((f) => f.runner), ["cargo", "cargo"]);
  assert.deepEqual(parseTestFailuresDetailed(ERT_OUTPUT), [{ name: "probe-failing-test", runner: "ert" }]);
});

// Plan 05d: the single-test command a newly failing test is re-run with —
// the plan's `#+TT_RERUN:` template, the Node/cargo defaults, or none.
test("test-failures: the cargo and Node defaults build the right single-test command", () => {
  assert.equal(
    singleTestCommand("exchange_state_machine::tests::cancels_order", CARGO_OUTPUT),
    "cargo test -- --exact 'exchange_state_machine::tests::cancels_order'",
  );
  // The spec reporter locates the test's file, so the Node default names it.
  // Every substituted value is one single-quoted word, and the pattern is
  // escaped: Node reads it as a regular expression (review finding M-1).
  assert.equal(singleTestCommand("alpha fails", NODE_SPEC_OUTPUT), "node --test --test-name-pattern 'alpha fails' 'x.test.js'");
  // The TAP reporter's own location block is read too.
  assert.equal(singleTestCommand("alpha fails", NODE_TAP_OUTPUT), "node --test --test-name-pattern 'alpha fails' '/tmp/x.test.js'");
  // A Node name with no file the reporter located has no safe default:
  // `node --test --test-name-pattern` exits 0 when nothing matches, which
  // could report a real failure as a flake.
  assert.equal(singleTestCommand("alpha fails", "not ok 1 - alpha fails\n"), undefined);
  // ERT names parse but have no built-in re-run command: the strict rule.
  assert.equal(singleTestCommand("probe-failing-test", ERT_OUTPUT), undefined);
  // A name the output never named cannot be built either.
  assert.equal(singleTestCommand("who?", CARGO_OUTPUT), undefined);
  // A name with regular-expression metacharacters is escaped, so the pattern
  // matches it literally instead of matching nothing and exiting 0.
  const meta = ["test at meta.test.js:1:1", "✖ a failing test ... is never excused as pre-existing (plan 14h) (0.3ms)"].join("\n");
  assert.equal(
    singleTestCommand("a failing test ... is never excused as pre-existing (plan 14h)", meta),
    "node --test --test-name-pattern 'a failing test \\.\\.\\. is never excused as pre-existing \\(plan 14h\\)' 'meta.test.js'",
  );
});

test("test-failures: a #+TT_RERUN template substitutes {name}/{file}/{crate} as shell words", () => {
  assert.equal(
    singleTestCommand("alpha fails", NODE_SPEC_OUTPUT, "node --test --test-name-pattern {name} {file}"),
    "node --test --test-name-pattern 'alpha fails' 'x.test.js'",
  );
  assert.equal(
    singleTestCommand("alpha fails", NODE_SPEC_OUTPUT, "cargo test -- --exact {name}"),
    "cargo test -- --exact 'alpha fails'",
  );
  // The plan's own cargo example: {crate} is the test's `::` path root.
  assert.equal(
    singleTestCommand("exchange_state_machine::tests::cancels_order", CARGO_OUTPUT, "cargo test -p {crate} -- --exact {name}"),
    "cargo test -p 'exchange_state_machine' -- --exact 'exchange_state_machine::tests::cancels_order'",
  );
  // A unit-test target has no `--test` file, so the whole example is not run.
  assert.equal(
    singleTestCommand("exchange_state_machine::tests::cancels_order", CARGO_OUTPUT, "cargo test -p {crate} --test {file} -- --exact {name}"),
    undefined,
  );
  // An integration target's `Running tests/x.rs …` line names the `--test` file.
  const cargoIntegration = ["     Running tests/x.rs (target/debug/deps/x-abc123)", "test cancels_order ... FAILED", ""].join("\n");
  assert.equal(
    singleTestCommand("cancels_order", cargoIntegration, "cargo test --test {file} -- --exact {name}"),
    "cargo test --test 'x' -- --exact 'cancels_order'",
  );
  // A name that is not a `::` path has no crate, so a {crate} template is not run.
  assert.equal(singleTestCommand("alpha fails", NODE_SPEC_OUTPUT, "cargo test -p {crate} -- --exact {name}"), undefined);
  // `{file}` with no file the output located cannot be built.
  assert.equal(singleTestCommand("alpha fails", "not ok 1 - alpha fails\n", "node --test {file} {name}"), undefined);
  // A name with a quote is one shell word, not an injection.
  assert.equal(shellQuote("it's"), "'it'\\''s'");
  assert.equal(escapeRegExp("a.b (c)"), "a\\.b \\(c\\)");
  assert.equal(singleTestCommand("it's failing", "not ok 1 - it's failing\n", "echo {name}"), "echo 'it'\\''s failing'");

  assert.equal(rerunTemplateIssue("node --test --test-name-pattern {name} {file}"), undefined);
  assert.equal(rerunTemplateIssue("cargo test -p {crate} --test {file} -- --exact {name}"), undefined);
  assert.match(rerunTemplateIssue("run --test {pattern}") ?? "", /unknown placeholder \{pattern\}/);
  assert.match(rerunTemplateIssue("test {name} {NAME}") ?? "", /unknown placeholder \{NAME\}/);
  assert.match(rerunTemplateIssue("sh -c 'exit 0'") ?? "", /does not name the failing test/);
});

test("test-failures: a re-run counts as a flake only when it passed AND shows the named test ran", () => {
  // A real node pass: exit 0 plus the named test's own `✔` line.
  const passed = classifyRerun("alpha", "node --test alpha", 1, [{ exitCode: 0, timedOut: false, output: "✔ alpha (0.1ms)\nℹ pass 1" }]);
  assert.equal(passed.loadOnly, true);
  assert.equal(passed.reproducesAlone, false);
  assert.deepEqual(passed.rerunExitCodes, [0]);
  assert.equal(passed.failingExitCode, 1);

  // Review finding M-7: a summary count is NOT evidence. A describe block
  // whose tests were all filtered out exits 0 with `tests 0`, `suites 1` and
  // only the suite's own `✔` line.
  const suiteOnly = classifyRerun("the test", "node --test x", 1, [
    { exitCode: 0, timedOut: false, output: "✔ describe suite (0.2ms)\nℹ tests 0\nℹ suites 1\nℹ pass 0" },
  ]);
  assert.equal(suiteOnly.loadOnly, false);
  assert.equal(suiteOnly.reproducesAlone, true);

  // Older Node reports filtered tests as skipped: `tests 3`, `skipped 3`,
  // `pass 0` must not be read as a pass.
  assert.equal(rerunProvesTheTestRan("ℹ tests 3\nℹ suites 1\nℹ pass 0\nℹ skipped 3", "the test"), false);
  // A TAP skipped test is not a pass either.
  assert.equal(rerunProvesTheTestRan("ok 1 - the test # SKIP", "the test"), false);
  // Nor is another test that happened to match an unescaped pattern.
  assert.equal(rerunProvesTheTestRan("✔ aab (0.1ms)\nℹ pass 1", "a+b"), false);
  assert.equal(rerunProvesTheTestRan("ℹ tests 0\nℹ pass 0", "alpha"), false);
  assert.equal(rerunProvesTheTestRan("nothing here", "alpha"), false);

  // The named test's own passing line is evidence, per runner.
  assert.equal(rerunProvesTheTestRan("✔ alpha (0.1ms)", "alpha"), true);
  assert.equal(rerunProvesTheTestRan("  ✔ alpha", "alpha"), true);
  assert.equal(rerunProvesTheTestRan("ok 1 - alpha", "alpha"), true);
  assert.equal(rerunProvesTheTestRan("test alpha ... ok\n\ntest result: ok. 1 passed; 0 failed", "alpha"), true);
  assert.equal(rerunProvesTheTestRan("   passed  1/1  alpha (0.001 sec)", "alpha"), true);
  assert.equal(rerunProvesTheTestRan("✔ beta (0.1ms)", "alpha"), false, "another test's pass is not this test's proof");

  const failedTwice = classifyRerun("alpha", "node --test alpha", 1, [
    { exitCode: 1, timedOut: false, output: "✖ alpha" },
    { exitCode: 1, timedOut: false, output: "✖ alpha" },
  ]);
  assert.equal(failedTwice.reproducesAlone, true);
  assert.equal(failedTwice.loadOnly, false);

  // A re-run that timed out proves nothing: the strict direction.
  const timedOut = classifyRerun("alpha", "node --test alpha", 1, [{ exitCode: null, timedOut: true }]);
  assert.equal(timedOut.reproducesAlone, true);
  assert.equal(timedOut.loadOnly, false);
  assert.equal(timedOut.rerunTimedOut, true);

  // No command at all cannot prove a flake either.
  const noCommand = classifyRerun("alpha", undefined, 1, []);
  assert.equal(noCommand.reproducesAlone, true);
  assert.equal(noCommand.rerunCommand, undefined);
});

// Plan 05d / finding #25: a base flake is recorded, visible, and never
// excuses a candidate's own failure of the same test.
test("test-failures: base flakes are recorded, named in the status, and never excuses", () => {
  const commands: BaselineCommand[] = [
    { command: "node --test", exitCode: 1, signal: null, timedOut: false, durationMs: 5, failures: ["real one"], flakes: ["flaky base"] },
  ];
  assert.deepEqual(baselineFailureNames(commands), ["real one"]);
  assert.deepEqual(baselineFlakeNames(commands), ["flaky base"]);
  const record: Baseline = { baseSha: "a".repeat(40), tree: "t".repeat(40), key: "k", at: "", commands, failures: ["real one"], flakes: ["flaky base"] };
  const line = baselineStatusLine(record) ?? "";
  assert.match(line, /base fails: 1 tests: real one/);
  assert.match(line, /base flakes \(passed alone, never excusing\): flaky base/);
  // The classification used at the gate sees only the real failures, so a
  // candidate failing `flaky base` is a NEW failure.
  const verdict = classifyCheckFailure("not ok 1 - flaky base\n", record.failures);
  assert.deepEqual(verdict.newFailures, ["flaky base"]);
  assert.equal(verdict.excused, false);
  // A round trip through parseBaseline keeps the flakes.
  const parsed = parseBaseline(JSON.parse(JSON.stringify(record)))!;
  assert.deepEqual(parsed.flakes, ["flaky base"]);
  assert.deepEqual(parsed.commands[0].flakes, ["flaky base"]);
  // An older record without flakes recomputes none.
  const legacy = parseBaseline({ key: "k", baseSha: "a", tree: "t", commands: [{ command: "c", exitCode: 1, failures: ["x"] }] })!;
  assert.equal(legacy.flakes, undefined);
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
