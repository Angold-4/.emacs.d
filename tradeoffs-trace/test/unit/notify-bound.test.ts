// Plan 07e (E1/E2), owner directive OD-2 (1): one `notify` call per poll is
// the whole contract. A bound that falls due appends at most one record, that
// call reports the bound in `NotifyResult.bound`, and a second poll at the
// same instant appends nothing. The ordinary `reminderMs` reminder keeps its
// own cadence and reports no bound.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";

import { notificationsPath, notify, readNotifications, type NotifyEntry } from "../../src/notify.ts";

const T0 = Date.parse("2026-10-27T10:00:00.000Z");

function entry(): NotifyEntry {
  return {
    id: "run-1",
    kind: "run",
    title: "07e: bounded protocol",
    reason: "needs you",
    waitKey: "owner-request:1",
  };
}

function setup(): { root: string; at: (ms: number) => () => number } {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-07e-notify-bound-"));
  return { root, at: (ms) => () => T0 + ms };
}

function cleanup(root: string): void {
  fs.rmSync(root, { recursive: true, force: true });
}

test("plan 07e (OD-2): a bound reminder is one record, reported by the one call that appends it", () => {
  const { root, at } = setup();
  try {
    const opts = { root, env: {}, reminderMs: 1000, everyMs: 1000 };

    // t0: the announcement, and no bound yet.
    const initial = notify(entry(), { ...opts, now: at(0) });
    assert.equal(initial.appended, "initial");
    assert.equal(initial.bound, undefined, "the announcement is not a bound");
    assert.equal(readNotifications(root).length, 1);

    // t0+1000: the reminder and the bound coincide — exactly ONE record.
    const due = notify(entry(), { ...opts, now: at(1000) });
    assert.equal(due.appended, "reminder");
    assert.deepEqual(due.bound, { limitMs: 1000, elapsedMs: 1000 }, "the call that appends the bound's record reports it");
    const afterDue = readNotifications(root);
    assert.equal(afterDue.length, 2, "one call appends at most one record");
    assert.equal(afterDue[1]!.reminder, true);
    assert.equal(Date.parse(afterDue[1]!.at), T0 + 1000);

    // A second poll at the same instant appends nothing and reports no bound.
    const again = notify(entry(), { ...opts, now: at(1000) });
    assert.equal(again.appended, "none");
    assert.equal(again.bound, undefined);
    assert.equal(readNotifications(root).length, 2, "the bound is not served twice");
  } finally {
    cleanup(root);
  }
});

test("plan 07e (OD-2): the ordinary reminder reports no bound and never postpones the bound", () => {
  const { root, at } = setup();
  try {
    const opts = { root, env: {}, reminderMs: 400, everyMs: 1000 };

    assert.equal(notify(entry(), { ...opts, now: at(0) }).appended, "initial");

    // t0+400: the ordinary reminder, on its own cadence, with no bound.
    const reminder = notify(entry(), { ...opts, now: at(400) });
    assert.equal(reminder.appended, "reminder");
    assert.equal(reminder.bound, undefined, "the reminderMs reminder is not the owner-wait bound");
    assert.equal(readNotifications(root).length, 2);

    // t0+1000: the bound falls due on its own lattice point and appends one
    // record, even though the ordinary reminder already landed.
    const bound = notify(entry(), { ...opts, now: at(1000) });
    assert.equal(bound.appended, "reminder");
    assert.deepEqual(bound.bound, { limitMs: 1000, elapsedMs: 1000 });
    assert.equal(readNotifications(root).length, 3);
    assert.equal(fs.existsSync(notificationsPath(root)), true);
  } finally {
    cleanup(root);
  }
});
