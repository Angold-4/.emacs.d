// Plan 07e (E1/E2), OD-2: the owner-wait bound keeps its own record even when
// the ordinary reminder lands on the same poll. One bounded `notify` call per
// poll reports the bound in `NotifyResult.bound`; the conductor records
// `BOUND_REACHED` from it. These are the notify-level scenarios the lead asked
// for, with no conductor around them.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import { test } from "node:test";

import { notificationsPath, notify, readNotifications } from "../../src/notify.ts";

function tempRoot(): string {
  return fs.mkdtempSync("/tmp/tt-07e-notify-bound-");
}

function entry() {
  return { id: "r1", kind: "run" as const, title: "t", reason: "needs you", waitKey: "k" };
}

test("plan 07e: a coincident ordinary reminder and owner-wait bound make one record and report the bound", () => {
  const root = tempRoot();
  const t0 = Date.parse("2026-01-01T00:00:00.000Z");
  let now = t0;
  const opts = {
    root,
    now: () => now,
    reminderMs: 1_000,
    everyMs: 1_000,
    env: { TT_NOTIFY_COMMAND: ":" },
  };
  try {
    const initial = notify(entry(), opts);
    assert.equal(initial.appended, "initial", "the wait is announced at t0");
    assert.equal(initial.bound, undefined, "the announcement is not a bound");

    now = t0 + 1_000;
    const due = notify(entry(), opts);
    assert.equal(due.appended, "reminder", "the coincident poll appends one reminder");
    assert.deepEqual(due.bound, { limitMs: 1_000, elapsedMs: 1_000 }, "the same call reports the bound");
    assert.equal(readNotifications(root).length, 2, "t0 and t0+1000 are one record each");

    const again = notify(entry(), opts);
    assert.equal(again.appended, "none", "a second call at the same moment does nothing");
    assert.equal(again.bound, undefined);
    assert.equal(readNotifications(root).length, 2, "the bound is recorded once");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("plan 07e: an ordinary reminder before the bound neither hides nor duplicates the bound record", () => {
  const root = tempRoot();
  const t0 = Date.parse("2026-01-01T00:00:00.000Z");
  let now = t0;
  const opts = {
    root,
    now: () => now,
    reminderMs: 400,
    everyMs: 1_000,
    env: { TT_NOTIFY_COMMAND: ":" },
  };
  try {
    notify(entry(), opts);
    now = t0 + 400;
    const reminder = notify(entry(), opts);
    assert.equal(reminder.appended, "reminder", "the ordinary reminder lands at t0+400");
    assert.equal(reminder.bound, undefined, "the ordinary reminder is not the bound");
    assert.equal(readNotifications(root).length, 2);

    now = t0 + 1_000;
    const bound = notify(entry(), opts);
    assert.equal(bound.appended, "reminder", "the bound appends its own record");
    assert.deepEqual(bound.bound, { limitMs: 1_000, elapsedMs: 1_000 });
    assert.equal(readNotifications(root).length, 3, "initial, ordinary reminder and bound are separate records");
    assert.ok(fs.existsSync(notificationsPath(root)));
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
