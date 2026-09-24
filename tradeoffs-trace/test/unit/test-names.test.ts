// Skill fix 4: tests removed from files that still exist (core/test-names.ts),
// on the shapes run 9120dca7 (plan 12c) produced.

import assert from "node:assert/strict";
import { test } from "node:test";

import { removedTests, testNames } from "../../src/core/test-names.ts";

test("test-names: Rust, JS/TS, Elisp, Python and Go declarations", () => {
  const rs = "#[test]\nfn a() {}\n#[tokio::test(flavor = \"multi_thread\")]\n#[ignore]\nasync fn b() {}\nfn helper() {}\n";
  assert.deepEqual(testNames("src/lib.rs", rs), ["a", "b"]);
  assert.deepEqual(testNames("t/x.test.ts", 'test("one", () => {});\nit(\'two\', () => {});'), ["one", "two"]);
  assert.deepEqual(testNames("test/x-test.el", "(ert-deftest my-test ()\n t)"), ["my-test"]);
  assert.deepEqual(testNames("t.py", "def test_a():\n  pass\ndef helper(): pass"), ["test_a"]);
  assert.deepEqual(testNames("x_test.go", "func TestA(t *testing.T) {}"), ["TestA"]);
  assert.deepEqual(testNames("README.md", "#[test]\nfn a() {}"), [], "unknown file types have no tests");
});

test("test-names: removed from a surviving file is reported; moved or deleted-with-its-file is not", () => {
  const files = [
    // a test dropped from a file that stays: reported
    { path: "run.rs", base: "#[test]\nfn keeps() {}\n#[test]\nfn dropped() {}\n", candidate: "#[test]\nfn keeps() {}\n" },
    // a test moved to another changed file: not reported
    { path: "a.rs", base: "#[test]\nfn moved() {}\n", candidate: "fn nothing() {}\n" },
    { path: "b.rs", base: "", candidate: "#[test]\nfn moved() {}\n" },
    // a whole file deleted (with the code it tested): not reported
    { path: "identity.rs", base: "#[test]\nfn gone_with_code() {}\n", candidate: undefined },
  ];
  assert.deepEqual(removedTests(files), ["run.rs: dropped"]);
});
