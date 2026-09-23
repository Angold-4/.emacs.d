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

test("seq is monotonic within a session and across reopen", () => {
  const logPath = path.join(dir, "events.jsonl");
  const log1 = new EventLog(logPath);
  const r1 = log1.append("state", { a: 1 });
  const r2 = log1.append("state", { a: 2 });
  assert.equal(r1.seq, 1);
  assert.equal(r2.seq, 2);
  log1.close();

  const log2 = new EventLog(logPath);
  const r3 = log2.append("state", { a: 3 });
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
