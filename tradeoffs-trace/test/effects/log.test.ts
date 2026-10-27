import assert from "node:assert/strict";
import fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, mock, test } from "node:test";
import { EventLog, LogCorruptionError, pendingIntents, readLog } from "../../src/effects/log.ts";

let dir: string;

beforeEach(() => {
  dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-log-test-"));
});

afterEach(() => {
  fs.rmSync(dir, { recursive: true, force: true });
});

test("append fsyncs after every record", () => {
  const fsyncMock = mock.method(fs, "fsyncSync");
  try {
    const log = new EventLog(path.join(dir, "events.jsonl"));
    log.append("state", { a: 1 });
    log.append("state", { a: 2 });
    log.append("state", { a: 3 });
    log.close();
    assert.equal(fsyncMock.mock.callCount(), 3);
  } finally {
    fsyncMock.mock.restore();
  }
});

test("plan 07e (E3/E4): ENOSPC is reported once, each append retries once, and a refused record is marked dropped", () => {
  const logPath = path.join(dir, "events.jsonl");
  const enospc = Object.assign(new Error("no space left on device"), { code: "ENOSPC" });
  const writeMock = mock.method(fs, "writeSync", () => {
    throw enospc;
  });
  const onDiskFull = mock.fn();
  const log = new EventLog(logPath, [], onDiskFull);
  try {
    const first = log.append("state", { a: 1 });
    assert.equal(first.seq, 1, "the attempted record is still handed back (OD-1.3)");
    assert.equal(first.dropped, true, "the refused record is explicitly marked dropped");
    assert.equal(first.durable, false, "the refused record is explicitly marked not durable");
    assert.equal(log.diskFull, true);
    assert.equal(log.droppedAppends, 2, "both attempts of the ordinary append failed");
    assert.equal(onDiskFull.mock.callCount(), 1);
    assert.equal(writeMock.mock.callCount(), 2, "an ordinary append retries exactly once");
    const second = log.append("state", { a: 2 });
    assert.equal(second.dropped, true, "an append the disk keeps refusing is handed back marked dropped");
    assert.equal(second.seq, 2);
    assert.equal(writeMock.mock.callCount(), 4, "the next append retries once too");
    assert.equal(onDiskFull.mock.callCount(), 1, "the notice is not repeated");
    assert.equal(log.droppedAppends, 4, "every failed attempt is counted, never passed off as durable");
    assert.deepEqual(readLog(logPath).records, [], "nothing the caller was told about is on disk");
  } finally {
    writeMock.mock.restore();
    log.close();
  }
});

test("plan 07e (E4): an ordinary append's retry lands and clears the full-disk state", () => {
  const logPath = path.join(dir, "events.jsonl");
  const enospc = Object.assign(new Error("no space left on device"), { code: "ENOSPC" });
  const realWriteSync = fs.writeSync;
  let calls = 0;
  const writeMock = mock.method(fs, "writeSync", (...args: Parameters<typeof fs.writeSync>) => {
    calls += 1;
    if (calls === 1) throw enospc;
    return realWriteSync.apply(fs, args);
  });
  const log = new EventLog(logPath, [], mock.fn());
  try {
    const record = log.append("state", { a: 1 });
    assert.equal(record.dropped, undefined, "a retry that landed hands back the durable record");
    assert.equal(record.seq, 1);
    assert.equal(log.diskFull, false, "the retried write landed, so the disk is writable again");
    assert.equal(log.droppedAppends, 1);
    log.close();
    const { records } = readLog(logPath);
    assert.deepEqual(records.map((r) => r.kind), ["state"], "the record survived the retry");
  } finally {
    writeMock.mock.restore();
  }
});

test("plan 07e (E4): the shutdown and final-failure records retry once after ENOSPC, bounded", () => {
  const logPath = path.join(dir, "events.jsonl");
  const enospc = Object.assign(new Error("no space left on device"), { code: "ENOSPC" });
  const writeMock = mock.method(fs, "writeSync", () => {
    throw enospc;
  });
  const log = new EventLog(logPath, [], mock.fn());
  try {
    log.append("state", { a: 1 });
    assert.equal(writeMock.mock.callCount(), 2);
    const critical = log.appendCritical("stop", { reason: "conductor stopped" });
    assert.equal(critical.dropped, true, "a critical append the disk refused is handed back marked dropped");
    // One bounded retry: two attempts for this record, never a loop.
    assert.equal(writeMock.mock.callCount(), 4, "the shutdown record is retried exactly once");
    assert.equal(log.droppedAppends, 4, "every failed attempt is counted");
  } finally {
    writeMock.mock.restore();
    log.close();
  }
});

test("plan 07e (E4): a shutdown record lands on disk when storage recovers after ENOSPC", () => {
  const logPath = path.join(dir, "events.jsonl");
  const enospc = Object.assign(new Error("no space left on device"), { code: "ENOSPC" });
  const realWriteSync = fs.writeSync;
  let calls = 0;
  const writeMock = mock.method(fs, "writeSync", (...args: Parameters<typeof fs.writeSync>) => {
    calls += 1;
    // Fail both attempts of the ordinary append, then let the shutdown record
    // land on its first attempt.
    if (calls <= 2) throw enospc;
    return realWriteSync.apply(fs, args);
  });
  const log = new EventLog(logPath, [], mock.fn());
  try {
    log.append("state", { a: 1 });
    assert.equal(log.diskFull, true);
    const critical = log.appendCritical("stop", { reason: "conductor stopped" });
    assert.equal(critical.dropped, undefined, "a retry that landed hands back the durable record");
    assert.equal(critical.seq, 2);
    assert.equal(log.diskFull, false, "the retried write landed, so the disk is writable again");
    assert.equal(log.droppedAppends, 2);
    log.close();
    const { records } = readLog(logPath);
    assert.deepEqual(records.map((r) => r.kind), ["stop"], "the shutdown state was preserved");
    assert.deepEqual(records[0].event, { reason: "conductor stopped" });
  } finally {
    writeMock.mock.restore();
  }
});

test("seq is monotonic within a session and across reopen", () => {
  const logPath = path.join(dir, "events.jsonl");
  const log1 = new EventLog(logPath);
  const r1 = log1.append("state", { a: 1 });
  const r2 = log1.append("state", { a: 2 });
  assert.ok(r1 && r2, "both appends landed");
  assert.equal(r1.seq, 1);
  assert.equal(r2.seq, 2);
  log1.close();

  const log2 = new EventLog(logPath);
  const r3 = log2.append("state", { a: 3 });
  assert.ok(r3, "the reopened log's append landed");
  assert.equal(r3.seq, 3);
  log2.close();

  const { records } = readLog(logPath);
  assert.deepEqual(
    records.map((r) => r.seq),
    [1, 2, 3],
  );
});

test("intent/completion round trip and actionId helper", () => {
  const logPath = path.join(dir, "events.jsonl");
  const log = new EventLog(logPath);
  const id = log.actionId("worktree");
  assert.match(id, /^worktree-\d+-[0-9a-f]+$/);
  log.intent(id, { path: "/tmp/x" });
  log.completion(id, { ok: true });
  log.close();

  const { records } = readLog(logPath);
  assert.equal(records.length, 2);
  assert.equal(records[0].kind, "intent");
  assert.equal(records[0].actionId, id);
  assert.equal(records[1].kind, "completion");
  assert.equal(records[1].actionId, id);
});

test("readLog tolerates a torn final line and reports it", () => {
  const logPath = path.join(dir, "events.jsonl");
  const log = new EventLog(logPath);
  log.append("state", { a: 1 });
  log.append("state", { a: 2 });
  log.close();
  // Simulate a crash mid-write: append a truncated JSON fragment with no
  // trailing newline.
  fs.appendFileSync(logPath, '{"seq":3,"ts":"2024-01-01T00:00:00.000Z","kind":"stat');

  const { records, tornLine } = readLog(logPath);
  assert.equal(records.length, 2);
  assert.ok(tornLine && tornLine.includes('"seq":3'));
});

test("readLog throws on corruption that is not the final line", () => {
  const logPath = path.join(dir, "events.jsonl");
  const log = new EventLog(logPath);
  log.append("state", { a: 1 });
  log.close();
  fs.appendFileSync(logPath, "not json at all\n");
  fs.appendFileSync(logPath, '{"seq":3,"ts":"x","kind":"state","event":{}}\n');

  assert.throws(() => readLog(logPath), LogCorruptionError);
});

test("pendingIntents finds intents with no completion", () => {
  const logPath = path.join(dir, "events.jsonl");
  const log = new EventLog(logPath);
  const id1 = log.actionId("sh");
  const id2 = log.actionId("sh");
  log.intent(id1, { pgid: 1 });
  log.completion(id1, { exitCode: 0 });
  log.intent(id2, { pgid: 2 });
  log.close();

  const { records } = readLog(logPath);
  const pending = pendingIntents(records);
  assert.equal(pending.length, 1);
  assert.equal(pending[0].actionId, id2);
});

test("readLog on a missing file returns an empty result", () => {
  const { records, tornLine } = readLog(path.join(dir, "does-not-exist.jsonl"));
  assert.deepEqual(records, []);
  assert.equal(tornLine, undefined);
});
