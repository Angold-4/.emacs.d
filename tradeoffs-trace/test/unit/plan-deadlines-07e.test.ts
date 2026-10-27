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
import { BOUNDS, boundDefaultMs, type BoundKind } from "../../src/core/bounds.ts";

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

// A1/E1: the table itself is the contract — one row per bound kind, each with
// its plan keyword, today's default and the action the conductor takes. The
// conductor's own DEFAULT_DEADLINES is what a plan without the keyword gets,
// so the two must never drift: half the rows are derived from the table and
// the rest are literals, which is exactly where a stale default would hide.
test("every bound in the table names its keyword, today's default and its action", () => {
  const kinds = BOUNDS.map((b) => b.kind);
  assert.equal(new Set(kinds).size, kinds.length, `each kind appears once: ${kinds.join(", ")}`);
  assert.equal(
    new Set(BOUNDS.map((b) => b.keyword)).size,
    BOUNDS.length,
    "each keyword names exactly one bound",
  );
  for (const bound of BOUNDS) {
    assert.match(bound.keyword, /^TT_[A-Z_]+$/, `${bound.kind} names its Org keyword`);
    assert.ok(bound.field.trim().length > 0, `${bound.kind} names the deadline it fills`);
    assert.ok(bound.action.trim().length > 0, `${bound.kind} names what reaching it does`);
    if (bound.unit === "count") continue;
    assert.equal(
      boundDefaultMs(bound.kind as Exclude<BoundKind, "rounds">),
      (DEFAULT_DEADLINES as unknown as Record<string, number>)[bound.field],
      `${bound.kind}'s table default is the conductor's own default`,
    );
  }
  // The rounds budget is a count, not a duration: today's default is three
  // rounds and it has no milliseconds to compare.
  const rounds = BOUNDS.find((b) => b.kind === "rounds")!;
  assert.equal(rounds.unit, "count");
  assert.equal(rounds.keyword, "TT_ROUNDS");
  assert.equal(rounds.defaultValue, 3, "today's rounds default");
  // The two bounds this phase adds are in the table beside the old ones.
  assert.deepEqual(
    BOUNDS.filter((b) => ["merge", "owner-wait"].includes(b.kind)).map((b) => b.keyword),
    ["TT_MERGE_MINUTES", "TT_OWNER_WAIT_MINUTES"],
  );
  assert.match(
    BOUNDS.find((b) => b.kind === "merge")!.action,
    /merges done so far/,
    "the merge bound keeps the merges done so far as the candidate",
  );
});
