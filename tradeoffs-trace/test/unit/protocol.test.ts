import assert from "node:assert/strict";
import { test } from "node:test";

import { encodeLine, JSONLDecoder } from "../../src/core/protocol.ts";

test("protocol: encodeLine terminates with a single bare LF", () => {
  const line = encodeLine({ type: "hello" });
  assert.ok(line.endsWith("\n"));
  assert.ok(!line.endsWith("\r\n"));
  assert.equal(line, `${JSON.stringify({ type: "hello" })}\n`);
});

test("protocol: JSONLDecoder round-trips whatever encodeLine produces", () => {
  const decoder = new JSONLDecoder();
  const msg = { type: "submit", id: "1", tool: "submit_phase", args: { decisions: [] } };
  assert.deepEqual(decoder.push(encodeLine(msg)), [msg]);
});

test("protocol: JSONLDecoder buffers a partial record across pushes", () => {
  const decoder = new JSONLDecoder();
  const text = encodeLine({ type: "a", n: 1 });
  const cut = Math.floor(text.length * 0.4);
  assert.deepEqual(decoder.push(text.slice(0, cut)), []);
  assert.equal(decoder.pending().length, cut);
  assert.deepEqual(decoder.push(text.slice(cut)), [{ type: "a", n: 1 }]);
  assert.equal(decoder.pending(), "");
});

test("protocol: JSONLDecoder decodes several records delivered in one chunk", () => {
  const decoder = new JSONLDecoder();
  const chunk = encodeLine({ type: "a" }) + encodeLine({ type: "b" }) + encodeLine({ type: "c" });
  assert.deepEqual(decoder.push(chunk), [{ type: "a" }, { type: "b" }, { type: "c" }]);
});

test("protocol: JSONLDecoder strips a trailing CR (CRLF input)", () => {
  const decoder = new JSONLDecoder();
  assert.deepEqual(decoder.push(`${JSON.stringify({ type: "a" })}\r\n`), [{ type: "a" }]);
});

test("protocol: JSONLDecoder does not treat U+2028/U+2029 inside a string as a record boundary", () => {
  const decoder = new JSONLDecoder();
  const text = "para one para two para three";
  const line = encodeLine({ type: "d", text });
  assert.deepEqual(decoder.push(line), [{ type: "d", text }]);
});

test("protocol: JSONLDecoder skips blank lines and reports malformed ones via onError instead of throwing", () => {
  const decoder = new JSONLDecoder();
  const errors: string[] = [];
  const chunk = `\n${JSON.stringify({ type: "ok" })}\nnot json\n`;
  const results = decoder.push(chunk, (e) => errors.push(e.line));
  assert.deepEqual(results, [{ type: "ok" }]);
  assert.deepEqual(errors, ["not json"]);
});
