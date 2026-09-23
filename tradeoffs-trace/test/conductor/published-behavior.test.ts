// Normal-suite proof that the published-behavior verifier does what it
// claims: it accepts a candidate that really implements `subtract` and
// rejects a no-op candidate that never added it. The live test
// (test/live/live-single-phase.test.ts) runs the same verifier on a fresh
// checkout of the published integration commit I.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, test } from "node:test";

import { verifySubtractFeature } from "../live/published-behavior.ts";

const dirs: string[] = [];

function makeCandidate(sumJs: string): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-behavior-"));
  dirs.push(dir);
  fs.writeFileSync(path.join(dir, "sum.js"), sumJs);
  fs.writeFileSync(
    path.join(dir, "test.js"),
    [
      "const test = require('node:test');",
      "const assert = require('node:assert/strict');",
      "const { sum } = require('./sum.js');",
      "test('sum adds two numbers', () => { assert.equal(sum(2, 3), 5); });",
      "",
    ].join("\n"),
  );
  return dir;
}

afterEach(() => {
  for (const dir of dirs.splice(0)) fs.rmSync(dir, { recursive: true, force: true });
});

test("R2.behavior: verifier accepts a candidate that added subtract and kept sum", () => {
  const dir = makeCandidate(
    [
      "function sum(a, b) { return a + b; }",
      "function subtract(a, b) { return a - b; }",
      "module.exports = { sum, subtract };",
      "",
    ].join("\n"),
  );
  const result = verifySubtractFeature(dir);
  assert.equal(result.ok, true, `expected the correct candidate to pass, detail: ${result.detail}`);
  assert.match(result.detail, /BEHAVIOR-OK/);
});

test("R2.behavior: verifier rejects a no-op candidate that never added subtract", () => {
  const dir = makeCandidate(
    ["function sum(a, b) { return a + b; }", "module.exports = { sum };", ""].join("\n"),
  );
  const result = verifySubtractFeature(dir);
  assert.equal(result.ok, false, "expected the no-op candidate to be rejected");
  assert.match(result.detail, /subtract must be exported as a function/);
});
