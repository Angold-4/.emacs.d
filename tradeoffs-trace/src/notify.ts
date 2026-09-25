// Plan 01b: owner-wait notifications (design D4, runtime doc §3). The
// measured program waited on the owner for 28 % of its wall clock, and 13f
// sat in AWAITING_OWNER for more than a day because nothing told the owner.
//
// The rule this file implements, stated once so every caller agrees:
//   * a run entering AWAITING_OWNER or BLOCKED, or a program ending done or
//     stuck, APPENDS one record to <root>/notifications.jsonl — after
//     redacting any declared secret value — and runs the notifier once;
//   * a wait still unbroken `reminderMs` later (default 30 min) appends one
//     more record and runs the notifier once more, and never again for that
//     wait;
//   * the notifier is best-effort: a command that fails, times out or cannot
//     be spawned is handed to `onError` and otherwise ignored, so a broken
//     notifier can never stop a run or the scheduler.
//
// `waitKey` is what makes "one record per wait" durable across conductor
// restarts: it is derived from the state that created the wait (the newest
// open owner request, or the blocked reason), so a restart that folds the
// same state back and calls `notify` again sees the record already on disk
// and does nothing. `reminder: true` marks the single allowed reminder.
//
// This module is pure I/O with no conductor state: `conductor.ts` and
// `program.ts` both call it, and `core/*` never does.

import * as fs from "node:fs";
import * as path from "node:path";
import { spawnSync } from "node:child_process";

/** The default `reminderMs`: notify once more when a wait passes 30 minutes. */
export const NOTIFY_REMINDER_MS = 30 * 60_000;

/** One line of `notifications.jsonl`. `kind` and `id` name the run or the
 * program; `node` is set for a run that is one node of a program; `reason`
 * is the one-line why. `waitKey` is internal to dedup (it is still written,
 * so the file explains itself) and `reminder` marks the 30-minute second
 * notification. */
export interface NotificationRecord {
  id: string;
  kind: "run" | "program";
  title: string;
  node?: string;
  reason: string;
  waitKey: string;
  at: string;
  reminder?: boolean;
}

/** What a caller knows about a wait; `at`/`reminder` are `notify`'s own. */
export type NotifyEntry = Omit<NotificationRecord, "at" | "reminder">;

export interface NotifyOptions {
  /** The trace root: `<runDir>`'s parent, or `<programDir>`'s grandparent. */
  root: string;
  /** Injectable clock (ms since epoch); default `Date.now`. */
  now?: () => number;
  /** Environment to read `TT_NOTIFY_COMMAND` from and to pass to it;
   * default `process.env`. */
  env?: NodeJS.ProcessEnv;
  /** Override the 30-minute reminder window (tests). */
  reminderMs?: number;
  /** How long the notifier may run before it is killed and logged. */
  timeoutMs?: number;
  /** Called once with a message when the notifier fails or times out. Never
   * thrown from; a throwing handler is swallowed too. */
  onError?: (message: string) => void;
}

export function notificationsPath(root: string): string {
  return path.join(root, "notifications.jsonl");
}

/** Every readable record, in file order; a missing or unreadable file is
 * an empty list (a corrupt line is skipped, never fatal). */
export function readNotifications(root: string): NotificationRecord[] {
  let text: string;
  try {
    text = fs.readFileSync(notificationsPath(root), "utf8");
  } catch {
    return [];
  }
  const out: NotificationRecord[] = [];
  for (const line of text.split("\n")) {
    if (!line.trim()) continue;
    try {
      const parsed = JSON.parse(line) as Partial<NotificationRecord>;
      if (typeof parsed.id === "string" && typeof parsed.waitKey === "string" && typeof parsed.at === "string") {
        out.push(parsed as NotificationRecord);
      }
    } catch {
      // a torn last line from a crash mid-append
    }
  }
  return out;
}

/** Collapse arbitrary agent text to one short line, so a record stays a
 * line-oriented JSONL entry and a notification body stays readable. */
export function oneLine(text: string, max = 300): string {
  const t = text.replace(/\s+/g, " ").trim();
  return t.length > max ? `${t.slice(0, max - 1)}…` : t;
}

/** The one-line reason a run is waiting, derived from its phase state. Kept
 * here (not in `core`) so `conductor.ts` and `program.ts` render the same
 * text. */
export function waitReason(phase: {
  phase: string;
  blockedReason?: string;
  ownerRequests: ReadonlyArray<{ id: string; reason: string; status: string }>;
}): string {
  if (phase.phase === "BLOCKED") return oneLine(`blocked: ${phase.blockedReason ?? "see the log"}`);
  const open = phase.ownerRequests.filter((r) => r.status === "open");
  if (open.length === 0) return "needs you";
  const reason = oneLine(open[open.length - 1].reason);
  return open.length === 1 ? reason : `${open.length} open: ${reason}`;
}

/** Append a record and run the notifier if this wait has not been announced
 * yet, or if it is due its one reminder. Returns what (if anything)
 * happened: `"initial"`, `"reminder"` or `"none"`. Never throws. */
export function notify(entry: NotifyEntry, opts: NotifyOptions): "initial" | "reminder" | "none" {
  const now = opts.now?.() ?? Date.now();
  const reminderMs = opts.reminderMs ?? NOTIFY_REMINDER_MS;
  const previous = readNotifications(opts.root).filter((r) => r.waitKey === entry.waitKey);
  const last = previous[previous.length - 1];
  if (last?.reminder) return "none";
  if (last && now - Date.parse(last.at) < reminderMs) return "none";
  const record: NotificationRecord = {
    ...entry,
    at: new Date(now).toISOString(),
    ...(last ? { reminder: true } : {}),
  };
  try {
    fs.appendFileSync(notificationsPath(opts.root), `${JSON.stringify(record)}\n`);
  } catch (err) {
    fail(opts, `notifications.jsonl: ${String((err as Error)?.message ?? err)}`);
  }
  runNotifier(record, opts);
  return last ? "reminder" : "initial";
}

/** The command the notifier runs, or undefined when there is none to run.
 * `TT_NOTIFY_COMMAND` wins (tests point it at a script); on macOS the
 * default is an `osascript` banner; everywhere else there is no default. */
export function notifierCommand(record: NotificationRecord, env: NodeJS.ProcessEnv): string | undefined {
  const override = env.TT_NOTIFY_COMMAND;
  if (typeof override === "string" && override.trim() !== "") return override;
  if (process.platform === "darwin") {
    const body = record.node ? `${record.title} [${record.node}]: ${record.reason}` : `${record.title}: ${record.reason}`;
    return `osascript -e 'display notification "${escapeAppleScript(body)}" with title "tradeoffs-trace"'`;
  }
  return undefined;
}

function escapeAppleScript(text: string): string {
  return text.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

function runNotifier(record: NotificationRecord, opts: NotifyOptions): void {
  const env = opts.env ?? process.env;
  let command: string | undefined;
  try {
    command = notifierCommand(record, env);
  } catch (err) {
    fail(opts, String((err as Error)?.message ?? err));
    return;
  }
  if (!command) return;
  let result;
  try {
    result = spawnSync("/bin/sh", ["-c", command], { env: { ...env }, timeout: opts.timeoutMs ?? 5_000, stdio: "ignore" });
  } catch (err) {
    fail(opts, String((err as Error)?.message ?? err));
    return;
  }
  if (result.error) {
    fail(opts, String(result.error.message ?? result.error));
  } else if (result.status !== 0) {
    fail(opts, `exit ${result.status ?? `signal ${result.signal}`}`);
  }
}

function fail(opts: NotifyOptions, message: string): void {
  try {
    opts.onError?.(`notifier failed: ${message}`);
  } catch {
    // a logging failure must never stop a run either
  }
}
