// Plan 07e (E1/R1): the six new bound keywords reach plan.deadlines through
// the Emacs parser, and a plan that writes none keeps today's values — the
// parser writes no field, so the conductor's own DEFAULT_DEADLINES applies.
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { DEFAULT_DEADLINES } from "../../src/conductor.ts";

const CORE_DIR = fileURLToPath(new URL("../../../core", import.meta.url));
const TEST_DIR = fileURLToPath(new URL("../../../test", import.meta.url));
const EMACS_LOAD = path.join(TEST_DIR, "tradeoffs-trace-test.el");

function planOrg(keywords: readonly string[]): string {
  return [
    "#+TITLE: bounds",
    "#+TT_REPO: /tmp/tt-07e-bounds",
    "#+TT_BRANCH: main",
    ...keywords,
    "",
    "* Phase 1: p1",
    "  :PROPERTIES:",
    "  :ID: p1",
    "  :CHECKS: true",
    "  :END:",
    "  Goal: the bounds reach the plan",
    "  Acceptance:",
    "  - it works",
    "",
  ].join("\n");
}

/** Parse a plan with Emacs's own parser and return its deadlines. */
function deadlinesOf(keywords: readonly string[]): Record<string, number> | undefined {
  const dir = fs.mkdtempSync("/tmp/tt-07e-deadlines-");
  try {
    const file = path.join(dir, "PLAN.org");
    fs.writeFileSync(file, planOrg(keywords));
    const out = execFileSync(
      "emacs",
      [
        "--batch",
        "-Q",
        "-L",
        CORE_DIR,
        "-L",
        TEST_DIR,
        "-l",
        EMACS_LOAD,
        "--eval",
        `(with-temp-buffer (insert-file-contents "${file}") (org-mode) (setq buffer-file-name "${file}") (princ (json-encode (plist-get (+tt-parse-plan) :plan))))`,
      ],
      { encoding: "utf8" },
    );
    return (JSON.parse(out) as { deadlines?: Record<string, number> }).deadlines;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

test("the six new bound keywords reach plan deadlines through the Emacs parser and default to today's values", () => {
  const written = deadlinesOf([
    "#+TT_EVALUATE_MINUTES: 11",
    "#+TT_FACT_MINUTES: 6",
    "#+TT_FIX_ATTEMPT_MINUTES: 40",
    "#+TT_MERGE_MINUTES: 9",
    "#+TT_FINAL_CHECK_MINUTES: 7",
    "#+TT_OWNER_WAIT_MINUTES: 200",
  ]);
  assert.deepEqual(written, {
    evaluateMs: 660_000,
    reproductionMs: 360_000,
    fixerAttemptMs: 2_400_000,
    mergeMs: 540_000,
    finalCheckMs: 420_000,
    ownerWaitMs: 12_000_000,
  });

  // No keyword, no override: the parser writes nothing, so today's conductor
  // defaults apply unchanged.
  assert.equal(deadlinesOf([]), undefined, "an unwritten bound stays unset");
  assert.equal(DEFAULT_DEADLINES.evaluateMs, 10 * 60_000);
  assert.equal(DEFAULT_DEADLINES.reproductionMs, 5 * 60_000);
  assert.equal(DEFAULT_DEADLINES.fixerAttemptMs, 45 * 60_000);
  assert.equal(DEFAULT_DEADLINES.mergeMs, 10 * 60_000);
  assert.equal(DEFAULT_DEADLINES.finalCheckMs, 5 * 60_000);
  assert.equal(DEFAULT_DEADLINES.ownerWaitMs, 240 * 60_000);
});
