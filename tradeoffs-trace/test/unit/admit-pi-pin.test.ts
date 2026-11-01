// Plan 07h1 (A1): ADMIT asserts the PINNED pi, not a string that merely holds
// the pin's text. The resolved pi reports its own version and only a complete,
// equal version passes — `10.87.0` is a different pi and an X:ENV_FAULT.

import assert from "node:assert/strict";
import { test } from "node:test";

import { ADMIT_PINNED_PI, admitVerdict, type AdmitFacts } from "../../src/core/admit.ts";

function facts(version: string | undefined): AdmitFacts {
  return {
    lint: [],
    repo: { path: "/repo", ok: true },
    unparsed: [],
    path: "/bin",
    pi: { name: "pi", pinned: ADMIT_PINNED_PI, path: "/bin/pi", ...(version !== undefined ? { version } : {}) },
  };
}

test("admit-pi-pin: the pinned version on the resolved PATH passes", () => {
  assert.equal(admitVerdict(facts(ADMIT_PINNED_PI)).ok, true);
  // A `--version` line that prints words around the number still asserts it.
  assert.equal(admitVerdict(facts(`pi ${ADMIT_PINNED_PI}`)).ok, true);
  assert.equal(admitVerdict(facts(`v${ADMIT_PINNED_PI}`)).ok, true);
});

test("admit-pi-pin: a different complete version that contains the pin is an X:ENV_FAULT", () => {
  const wrong = `1${ADMIT_PINNED_PI}`;
  const verdict = admitVerdict(facts(wrong));
  assert.equal(verdict.ok, false, "a different version containing the pin must be refused");
  if (verdict.ok) return;
  assert.equal(verdict.kind, "X:ENV_FAULT");
  assert.ok(verdict.reason.includes(wrong), "the reason names the version pi reported");
  assert.ok(verdict.reason.includes("/bin/pi"), "the reason names the binary it resolved");
});

test("admit-pi-pin: a pin with a prerelease suffix is not the plain pin", () => {
  assert.equal(admitVerdict(facts(`${ADMIT_PINNED_PI}-rc.1`)).ok, false);
});
