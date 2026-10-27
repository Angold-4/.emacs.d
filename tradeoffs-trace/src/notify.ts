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
//   * 07e (E1): a wait given `everyMs` (the owner-wait bound) also reminds
//     once per bound, counted from the wait's start and independent of the
//     `reminderMs` reminder; one call appends at most one record, so a bound
//     that falls due together with the ordinary reminder is one record — and
//     the call still reports the bound in `NotifyResult.bound`;
//   * the notifier is best-effort: a command that fails, times out or cannot
//     be spawned is handed to `onError` and otherwise ignored, so a broken
//     notifier can never stop a run or the scheduler.
//
// `waitKey` is what makes "one record per wait" durable across conductor
// restarts: it is derived from the state that created the wait (the newest
// open owner request, or the blocked reason), so a restart that folds the
// same state back and calls `notify` again sees the record already on disk
// and does nothing. `reminder: true` marks every record after the first, the
// single `reminderMs` reminder or a bound reminder alike.
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
 * so the file explains itself) and `reminder` marks any record after the
 * wait's announcement (the 30-minute reminder or a bound reminder). */
export interface NotificationRecord {
  id: string;
  kind: "run" | "program";
  title: string;
  node?: string;
  /** Plan 06m (A4): the phase that is waiting, so the notification names it
   * and not only the run. Absent for a program-level notification. */
  phase?: string;
  reason: string;
  /** Plan 06m (A4): the command that answers this wait (e.g. a `tt verdict`
   * or `tt carry`), so the owner is told what unblocks it. */
  command?: string;
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
  /** 07e (E1): when set, the wait also reminds the owner every `everyMs` —
   * the owner-wait bound. Unlimited while the wait stays open; each bound
   * reminder is a `reminder: true` record and the call that writes it reports
   * it in `NotifyResult.bound`. */
  everyMs?: number;
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

/** Plan 06m (A4): the command that answers a run's wait, in one line. The
 * owner request's own links decide it: a blocker or finding is answered by a
 * verdict, a decision or finding by a carry/verdict, and a budget gate by a
 * carry or a correction. Kept beside `waitReason` so a notification names
 * the run, the phase, the why and the answer from one place. */
export function answeringCommand(
  runId: string,
  phase: { ownerRequests: ReadonlyArray<{ status: string; origin?: string; linkedMessageId?: string; linkedFindingId?: string; linkedDecisionId?: string }> },
): string | undefined {
  const open = phase.ownerRequests.filter((r) => r.status === "open");
  if (open.length === 0) return undefined;
  const r = open[open.length - 1];
  if (r.linkedMessageId) return `tt verdict ${runId} ${r.linkedMessageId} accept|refuse`;
  if (r.origin === "repair_budget_exhausted" || r.origin === "failed_vote" || r.origin === "reserved_decision") {
    return `tt carry ${runId} ${r.linkedFindingId ?? r.linkedDecisionId ?? "<item>"} --to <next-phase>  (or tt verdict ${runId} <messageId> accept|refuse)`;
  }
  if (r.linkedFindingId || r.linkedDecisionId) {
    return `tt carry ${runId} ${r.linkedFindingId ?? r.linkedDecisionId} --to <next-phase>`;
  }
  return `tt correct ${runId} "<text>"`;
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

/** What one `notify` call did: the record it appended (`"initial"`,
 * `"reminder"` or `"none"`) and, when `opts.everyMs` is set, the owner-wait
 * bound this call reports. */
export interface NotifyResult {
  appended: "initial" | "reminder" | "none";
  /** 07e (E2/A2): set when a bound's reminder is due now and this call is the
   * one that reports it. It is true whether the appended record is the bound's
   * own reminder or the ordinary reminder landing on the same lattice point,
   * so the caller records `BOUND_REACHED` from this and never has to infer it
   * from what the append turned out to be. */
  bound?: { limitMs: number; elapsedMs: number };
}

/** Append a record and run the notifier if this wait has not been announced
 * yet, is due its one reminder, or is due a bound reminder. Never throws. */
export function notify(entry: NotifyEntry, opts: NotifyOptions): NotifyResult {
  const now = opts.now?.() ?? Date.now();
  const reminderMs = opts.reminderMs ?? NOTIFY_REMINDER_MS;
  const everyMs = opts.everyMs;
  const previous = readNotifications(opts.root).filter((r) => r.waitKey === entry.waitKey);
  const first = previous[0];
  const last = previous[previous.length - 1];
  // Today's rule, unchanged: one announcement, then exactly one reminder
  // `reminderMs` later, never again for that wait (an unreadable timestamp
  // still counts as due, as it did before).
  const reminderDue = last === undefined || (!last.reminder && !(now - Date.parse(last.at) < reminderMs));
  // 07e (E1/E2): the owner-wait bound keeps its OWN cadence, counted from the
  // wait's start (its first record) and gated by the timestamp of the last
  // record appended, so the ordinary 30-minute reminder can neither postpone
  // the first bound (270 instead of 240 minutes at the defaults) nor suppress
  // one when the two intervals coincide: a record appended at the same moment
  // serves both, and this call still reports the bound. One call appends at
  // most one record, so a late poll cannot append twice for the same bound.
  let boundDue = false;
  let bound: NotifyResult["bound"];
  if (everyMs !== undefined && first !== undefined) {
    const start = Date.parse(first.at);
    if (Number.isFinite(start)) {
      const elapsed = now - start;
      const recordedAt = last === undefined ? Number.NaN : Date.parse(last.at);
      const served = Number.isFinite(recordedAt) ? Math.floor((recordedAt - start) / everyMs) : 0;
      if (Math.floor(elapsed / everyMs) > served) {
        boundDue = true;
        bound = { limitMs: everyMs, elapsedMs: Math.max(everyMs, elapsed) };
      }
    } else if (!(now - Date.parse(last!.at) < everyMs)) {
      boundDue = true;
      bound = { limitMs: everyMs, elapsedMs: everyMs };
    }
  }
  if (!reminderDue && !boundDue) return { appended: "none" };
  const record: NotificationRecord = {
    ...entry,
    at: new Date(now).toISOString(),
    ...(last ? { reminder: true } : {}),
  };
  try {
    fs.appendFileSync(notificationsPath(opts.root), `${JSON.stringify(record)}\n`);
  } catch (err) {
    // No record on disk means the next poll would see an unseen waitKey and
    // re-enter here, so failing to append must NOT fall through to the
    // notifier: that would run TT_NOTIFY_COMMAND on every inbox poll. Log it
    // and report nothing done; the wait is already unannounceable, and a due
    // bound is retried on the next poll because nothing was served.
    fail(opts, `notifications.jsonl: ${String((err as Error)?.message ?? err)}`);
    return { appended: "none" };
  }
  runNotifier(record, opts);
  return { appended: last ? "reminder" : "initial", ...(bound ? { bound } : {}) };
}

/** How the notifier is launched: an argv (no shell, so the record's own text
 * is never re-parsed) or a shell command (the `TT_NOTIFY_COMMAND` override,
 * which the operator controls verbatim). */
export interface NotifierCommand {
  command: string;
  args: string[];
}

/** The notifier to run, or undefined when there is none.
 *
 * `TT_NOTIFY_COMMAND` wins (tests point it at a script) and runs through
 * `sh -c` — it is the operator's command, never built from the record. The
 * macOS default runs `osascript` DIRECTLY with `-e <script>` as argv: a wait
 * reason is model-written prose (an owner-request evidence line can hold any
 * character, including `'`), and passing it inside a shell single-quoted
 * `-e '...'` argument let the reason close that quote — the blocking defect.
 * With argv there is no shell to break; only the AppleScript string literal
 * still needs `"` and `\` escaped. */
export function notifierCommand(record: NotificationRecord, env: NodeJS.ProcessEnv): NotifierCommand | undefined {
  const override = env.TT_NOTIFY_COMMAND;
  if (typeof override === "string" && override.trim() !== "") return { command: "/bin/sh", args: ["-c", override] };
  if (process.platform === "darwin") {
    const where = record.node ? `${record.title} [${record.node}]` : record.title;
    const phase = record.phase ? ` (${record.phase})` : "";
    const answer = record.command ? ` — answer: ${record.command}` : "";
    const body = `${where}${phase}: ${record.reason}${answer}`;
    const script = `display notification "${escapeAppleScript(body)}" with title "tradeoffs-trace"`;
    return { command: "osascript", args: ["-e", script] };
  }
  return undefined;
}

function escapeAppleScript(text: string): string {
  return text.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

function runNotifier(record: NotificationRecord, opts: NotifyOptions): void {
  const env = opts.env ?? process.env;
  let spec: NotifierCommand | undefined;
  try {
    spec = notifierCommand(record, env);
  } catch (err) {
    fail(opts, String((err as Error)?.message ?? err));
    return;
  }
  if (!spec) return;
  let result;
  try {
    // Plan 06m (A4): the configured command receives the run, phase, reason
    // and the command that answers the wait, so a notifier script can act on
    // them (the record is also in notifications.jsonl).
    const payloadEnv: NodeJS.ProcessEnv = {
      ...env,
      TT_NOTIFY_RUN: record.id,
      TT_NOTIFY_TITLE: record.title,
      ...(record.node ? { TT_NOTIFY_NODE: record.node } : {}),
      ...(record.phase ? { TT_NOTIFY_PHASE: record.phase } : {}),
      TT_NOTIFY_REASON: record.reason,
      ...(record.command ? { TT_NOTIFY_ANSWER: record.command } : {}),
    };
    result = spawnSync(spec.command, spec.args, { env: payloadEnv, timeout: opts.timeoutMs ?? 5_000, stdio: "ignore" });
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
