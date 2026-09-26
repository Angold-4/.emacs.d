// events.jsonl: the run's append-only control log (design §9.1, §9.2, §9.3).
//
// Every external effect the conductor performs gets an action ID, an
// "intent" record written before the effect and a "completion" record
// written after. Recovery diffs intents against completions to find work
// that was interrupted mid-effect (see `pendingIntents`).
//
// The log is fsynced after every append. That is deliberately not
// "buffered and flushed periodically" — design §9.2 calls `events.jsonl`
// "the only source of truth for recovery", so a completion that the OS has
// not yet written to disk before a crash must not be considered to have
// happened. `pendingIntents` and readers of a reopened log rely on that.
//
// This module has no opinion on *what* a record's `event` payload looks
// like beyond it being JSON-serializable; that is for the conductor to
// define per action kind. It does redact one thing on every append: the
// plan's secret values (see `secrets.ts` and the constructor), so a value an
// agent echoed into a finding or a disclosure never reaches the log.

import { randomBytes } from "node:crypto";
// `fs`'s default export is the same mutable CJS `module.exports` object,
// unlike the frozen `import * as fs` namespace — used here (rather than
// changing behavior) so tests can `mock.method(fs, "fsyncSync")` to prove
// every append fsyncs.
import fs from "node:fs";

import { redactRecord, type Secret } from "./secrets.ts";

/** One line of `events.jsonl`. `kind` is `"intent"` or `"completion"` for
 * effect records, and free-form (e.g. `"state"`, `"applied"`) for anything
 * else the conductor logs. `actionId` is present on intent/completion
 * records and absent on others. */
export interface LogRecord {
  seq: number;
  ts: string;
  kind: string;
  actionId?: string;
  event: unknown;
}

/** Thrown when a line other than the final one in the file fails to parse
 * or does not have the shape of a `LogRecord`. A torn *final* line is not
 * an error — see `readLog`. */
export class LogCorruptionError extends Error {
  constructor(path: string, lineNumber: number, cause: string) {
    super(`corrupt log ${path} at line ${lineNumber}: ${cause}`);
    this.name = "LogCorruptionError";
  }
}

function isLogRecordShape(value: unknown): value is LogRecord {
  if (typeof value !== "object" || value === null) return false;
  const v = value as Record<string, unknown>;
  return typeof v.seq === "number" && typeof v.ts === "string" && typeof v.kind === "string" && "event" in v;
}

export interface ReadLogResult {
  records: LogRecord[];
  /** The raw text of a dropped, torn final line — present only when the
   * file's last line was incomplete (a crash mid-`fs.writeSync`). Every
   * other malformed line throws `LogCorruptionError` instead. */
  tornLine?: string;
}

/** Reads and parses every record in `path`. Tolerates *only* a torn final
 * line (the file does not end with a newline, or its last line is not
 * valid JSON / not a `LogRecord`): that line is dropped and reported via
 * `tornLine`, never thrown. Corruption anywhere else in the file throws
 * `LogCorruptionError`. A missing file yields an empty result. */
export function readLog(path: string): ReadLogResult {
  if (!fs.existsSync(path)) return { records: [] };
  const content = fs.readFileSync(path, "utf8");
  if (content.length === 0) return { records: [] };
  const lines = content.split("\n");
  // A well-formed file ends with "record\n", which split() turns into a
  // trailing empty string — not a torn line, just drop it.
  const endedWithNewline = lines[lines.length - 1] === "";
  if (endedWithNewline) lines.pop();

  const records: LogRecord[] = [];
  let tornLine: string | undefined;
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const isLast = i === lines.length - 1;
    let parsed: unknown;
    let parseError: string | undefined;
    try {
      parsed = JSON.parse(line);
      if (!isLogRecordShape(parsed)) parseError = "not a log record";
    } catch (err) {
      parseError = String((err as Error)?.message ?? err);
    }
    if (parseError === undefined) {
      records.push(parsed as LogRecord);
      continue;
    }
    // Only the final line of a file that never got its closing newline can
    // be a torn write; anything else (or a missing trailing newline that
    // is NOT the last line) is real corruption.
    if (isLast && !endedWithNewline) {
      tornLine = line;
    } else {
      throw new LogCorruptionError(path, i + 1, parseError);
    }
  }
  return { records, tornLine };
}

/** Intents (`kind === "intent"`) with no matching completion
 * (`kind === "completion"` with the same `actionId`) anywhere in
 * `records`. Order is preserved. Used by recovery to find effects that
 * were started but never confirmed done (design §9.3). */
export function pendingIntents(records: readonly LogRecord[]): LogRecord[] {
  const completed = new Set<string>();
  for (const r of records) {
    if (r.kind === "completion" && r.actionId !== undefined) completed.add(r.actionId);
  }
  return records.filter((r) => r.kind === "intent" && r.actionId !== undefined && !completed.has(r.actionId));
}

/** An open handle on a run's `events.jsonl`. Appends are synchronous and
 * each one is followed by `fsyncSync` before the call returns, so a caller
 * that has received `append`'s return value knows the record is durable. */
export class EventLog {
  #fd: number;
  #seq: number;
  #path: string;
  #secrets: readonly Secret[];

  /** `secrets` (plan 01a): every appended record's payload is redacted before
   * it is serialized, so no value ever lands in `events.jsonl` — the file a
   * crash-recovered conductor, `tt state` and every view read back. The same
   * redaction the offline `tt redact` path uses, so a value used as a JSON key
   * is masked here too and a number is never touched. */
  constructor(path: string, secrets: readonly Secret[] = []) {
    this.#path = path;
    this.#secrets = secrets;
    let lastSeq = 0;
    // Reopening: continue the seq counter from whatever is already on
    // disk (including a torn final line's seq if it happened to parse a
    // seq before failing shape validation — not possible per readLog's
    // contract, so we only ever see valid prior records here).
    const { records } = readLog(path);
    for (const r of records) if (r.seq > lastSeq) lastSeq = r.seq;
    this.#seq = lastSeq;
    this.#fd = fs.openSync(path, "a");
  }

  /** Appends one record and fsyncs before returning. The record's `event`
   * payload is redacted (see the constructor), never the caller's own
   * object. */
  append(kind: string, event: unknown, actionId?: string): LogRecord {
    this.#seq += 1;
    const redacted = redactRecord(event, this.#secrets);
    const record: LogRecord =
      actionId === undefined
        ? { seq: this.#seq, ts: new Date().toISOString(), kind, event: redacted }
        : { seq: this.#seq, ts: new Date().toISOString(), kind, actionId, event: redacted };
    fs.writeSync(this.#fd, `${JSON.stringify(record)}\n`);
    fs.fsyncSync(this.#fd);
    return record;
  }

  /** Generates a fresh, stable action ID of the form `<kind>-<seq>-<rand>`,
   * without appending anything. The caller passes it to both `intent` and
   * the later `completion`. */
  actionId(kind: string): string {
    return `${kind}-${this.#seq + 1}-${randomBytes(4).toString("hex")}`;
  }

  /** Records that effect `actionId` (`action`, e.g. `{pgid}` or
   * `{worktreePath, baseSha}`) is about to be performed. Must be written
   * before the effect happens (design §9.3). */
  intent(actionId: string, action: unknown): LogRecord {
    return this.append("intent", action, actionId);
  }

  /** Records that effect `actionId` finished, with `outcome` describing
   * the result. */
  completion(actionId: string, outcome: unknown): LogRecord {
    return this.append("completion", outcome, actionId);
  }

  close(): void {
    fs.closeSync(this.#fd);
  }
}
