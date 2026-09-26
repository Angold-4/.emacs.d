// Plan 01a: the secrets module — name parsing, resolving from the
// environment, redaction of text/JSON/JSONL, prompt lines and the run
// directory rewrite `tt redact` is built on.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import {
  loggedSecretStatus,
  maskableSecrets,
  planSecretNames,
  redactBytes,
  redactJson,
  redactJsonLine,
  redactJsonl,
  redactRecord,
  redactRunDir,
  redactText,
  resolveSecrets,
  secretNames,
  secretPromptLines,
  utf16Kind,
} from "../../src/effects/secrets.ts";

const VALUE = "sk-live-4f8a2b1c9d3e";

test("secrets: names are parsed, deduped and validated; values come from the environment", () => {
  assert.deepEqual(secretNames(["FAKE_KEY"]), ["FAKE_KEY"]);
  assert.deepEqual(secretNames(["FAKE_KEY  OTHER_KEY,FAKE_KEY"]), ["FAKE_KEY", "OTHER_KEY"]);
  assert.deepEqual(secretNames(undefined), []);
  assert.deepEqual(secretNames(["1BAD", "BAD-NAME", "OK_KEY"]), ["OK_KEY"], "only shell-assignable names");

  const resolved = resolveSecrets(["FAKE_KEY", "GONE_KEY"], { FAKE_KEY: VALUE, GONE_KEY: "" });
  assert.deepEqual(resolved.values, [{ name: "FAKE_KEY", value: VALUE }]);
  assert.deepEqual(resolved.maskable, [{ name: "FAKE_KEY", value: VALUE }]);
  assert.deepEqual(resolved.missing, ["GONE_KEY"], "an empty variable counts as unset");
  assert.deepEqual(resolved.tooShort, []);
});

test("secrets: a value too short to mask is reported, never applied", () => {
  // A plan declaring TT with TT=1: masking it would rewrite every id, count
  // and timestamp in the log, so it is used for the environment but never for
  // masking or for refusing a command.
  const resolved = resolveSecrets(["TT", "OK_KEY"], { TT: "1", OK_KEY: VALUE });
  assert.deepEqual(resolved.missing, []);
  assert.deepEqual(resolved.tooShort, ["TT"]);
  assert.deepEqual(resolved.values.map((s) => s.name), ["TT", "OK_KEY"], "the agent still gets it");
  assert.deepEqual(resolved.maskable, [{ name: "OK_KEY", value: VALUE }]);
  assert.equal(redactText("x 1 y", resolved.maskable), "x 1 y", "unrelated text is untouched");
  assert.deepEqual(maskableSecrets(["TT"], { TT: "1" }), []);
});

test("secrets: an overlapping value cannot leave a suffix of another behind", () => {
  const short = { name: "A_KEY", value: "sk-live" };
  const long = { name: "AB_KEY", value: "sk-live-abcd1234" };
  for (const order of [[short, long], [long, short]]) {
    const out = redactText(`x ${long.value} y`, order);
    assert.equal(out, "x ***AB_KEY*** y", `no suffix may survive (order ${JSON.stringify(order.map((s) => s.name))})`);
    assert.ok(!out.includes("abcd1234"));
  }
  const both = redactText("a=sk-live b=sk-live-abcd1234", [short, long]);
  assert.equal(both, "a=***A_KEY*** b=***AB_KEY***");
});

test("secrets: redaction keeps JSON and JSONL valid, and does not touch a torn line", () => {
  const secrets = [{ name: "FAKE_KEY", value: VALUE }];
  assert.equal(redactText(`token=${VALUE}!`, secrets), "token=***FAKE_KEY***!");
  assert.equal(redactText(`token=${VALUE}!`, []), `token=${VALUE}!`);

  const deep = redactJson({ a: [`x ${VALUE}`], b: { c: VALUE }, n: 7 }, secrets);
  assert.deepEqual(deep, { a: ["x ***FAKE_KEY***"], b: { c: "***FAKE_KEY***" }, n: 7 });

  // A complete line is parsed and re-serialized, so it stays valid JSON.
  const line = JSON.stringify({ seq: 1, event: { text: `echo ${VALUE}` } });
  const redactedLine = redactJsonl(`${line}\n`, secrets);
  assert.equal(redactedLine, `${JSON.stringify({ seq: 1, event: { text: "echo ***FAKE_KEY***" } })}\n`);
  assert.doesNotThrow(() => JSON.parse(redactedLine.trim()));

  // A torn final line (no closing newline) is redacted and left partial: no
  // newline is added and no line is split or joined.
  const torn = `${line}\n{"seq":2,"event":{"text":"${VALUE}`;
  const redactedTorn = redactJsonl(torn, secrets);
  assert.ok(redactedTorn.endsWith('{"seq":2,"event":{"text":"***FAKE_KEY***'), `torn line redacted in place: ${redactedTorn}`);
  assert.equal(redactedTorn.split("\n").length, 2, "still two lines, no newline added");
  assert.ok(redactedTorn.startsWith(redactedLine), "the complete line is unchanged in shape");
  assert.ok(!redactedTorn.includes(VALUE));

  // A line that is not JSON at all is redacted textually, never thrown.
  assert.equal(redactJsonl(`$ echo ${VALUE}\n`, secrets), "$ echo ***FAKE_KEY***\n");

  // A value JSON has to escape (a quote, a backslash) is matched in its
  // escaped form too, wherever it sits — value, torn line, even a key.
  const odd = { name: "FAKE_KEY", value: 'sk-"odd"\\key' };
  const oddLine = JSON.stringify({ text: `x ${odd.value} y`, [odd.value]: 1 });
  assert.ok(oddLine.includes('sk-\\"odd\\"'), `JSON escaped the value: ${oddLine}`);
  const redactedOdd = redactJsonl(`${oddLine}\n`, [odd]);
  assert.ok(!redactedOdd.includes("odd"), `value survived: ${redactedOdd}`);
  assert.doesNotThrow(() => JSON.parse(redactedOdd.trim()));
});

test("secrets: a value that is also a JSON number token is left alone (finding A-16)", () => {
  // `1234` is long enough to be a declared value (MIN_SECRET_LENGTH is 4), so
  // this is reachable: a whole-line textual pass would turn {"seq":1234} into
  // {"seq":***FAKE_KEY***}, which no longer parses.
  const secrets = [{ name: "FAKE_KEY", value: "1234" }];
  assert.equal(redactJsonl('{"seq":1234}\n', secrets), '{"seq":1234}\n', "a number token is never rewritten");
  assert.equal(redactJsonLine('{"seq":1234,"ok":true,"none":null}', secrets), '{"seq":1234,"ok":true,"none":null}');

  const line = '{"seq":1234,"event":{"type":"X","text":"id 1234 here"}}';
  const out = redactJsonl(`${line}\n`, secrets).trim();
  const parsed = JSON.parse(out) as { seq: number; event: { text: string } };
  assert.equal(parsed.seq, 1234, "the number stays a number");
  assert.equal(parsed.event.text, "id ***FAKE_KEY*** here", "a string is masked");
  // A value used as an object KEY is a string token, so it is masked — and the
  // line stays valid JSON (finding M-14's case).
  const keyed = redactJsonl(`{"args":{"1234":"x"},"n":1234}\n`, secrets).trim();
  assert.deepEqual(JSON.parse(keyed), { args: { "***FAKE_KEY***": "x" }, n: 1234 });

  // redactRecord (the live writers) uses the same rule.
  const record = { seq: 1234, args: { "1234": "x" }, text: "a 1234 b" };
  const redacted = redactRecord(record, secrets) as typeof record;
  assert.equal(redacted.seq, 1234);
  assert.deepEqual(redacted.args, { "***FAKE_KEY***": "x" });
  assert.equal(redacted.text, "a ***FAKE_KEY*** b");
  assert.equal(redactRecord(record, []), record, "no secrets: the caller's own object, unchanged");
});

test("secrets: the prompt lines name the secrets and say to use $NAME", () => {
  assert.deepEqual(secretPromptLines([]), []);
  const lines = secretPromptLines(["FAKE_KEY", "OTHER_KEY"]).join("\n");
  assert.match(lines, /declares FAKE_KEY, OTHER_KEY/);
  assert.match(lines, /\$NAME/);
  assert.match(lines, /\$FAKE_KEY, \$OTHER_KEY/);
  assert.match(lines, /never paste a value/);
});

/** A run directory with a value planted exactly where the conductor writes. */
function plantedRunDir(root: string): { runDir: string; eventsLine: string } {
  const runDir = path.join(root, "abcd1234");
  fs.mkdirSync(path.join(runDir, "stream"), { recursive: true });
  fs.mkdirSync(path.join(runDir, "checks", "c0ffee"), { recursive: true });
  fs.mkdirSync(path.join(runDir, "plan"), { recursive: true });
  fs.mkdirSync(path.join(runDir, "views"), { recursive: true });
  fs.writeFileSync(path.join(runDir, "meta.json"), JSON.stringify({ title: "t" }));
  fs.writeFileSync(path.join(runDir, "plan", "v1.json"), JSON.stringify({ secrets: ["FAKE_KEY"] }));
  const eventsLine = JSON.stringify({ seq: 1, ts: "2026-01-01T00:00:00.000Z", kind: "event", event: { type: "X", text: `echo ${VALUE}` } });
  fs.writeFileSync(path.join(runDir, "events.jsonl"), `${eventsLine}\n`);
  fs.writeFileSync(
    path.join(runDir, "stream", "worker-1.jsonl"),
    `${JSON.stringify({ agentId: "worker-1", event: { type: "tool_execution_end", result: { text: VALUE } } })}\n`,
  );
  fs.writeFileSync(path.join(runDir, "checks", "c0ffee", "true.log"), `$ echo ${VALUE}\nexit 0\n`);
  fs.writeFileSync(path.join(runDir, "views", "pr.md"), `key ${VALUE}\n`);
  return { runDir, eventsLine };
}

test("secrets: redactRunDir rewrites the planted value out and leaves JSONL parseable", () => {
  const root = fs.mkdtempSync("/tmp/tt-redact-unit-");
  try {
    const { runDir } = plantedRunDir(root);
    assert.deepEqual(planSecretNames(runDir), ["FAKE_KEY"], "the plan snapshot supplies the names");
    const { maskable, missing } = resolveSecrets(planSecretNames(runDir), { FAKE_KEY: VALUE });
    assert.deepEqual(missing, []);
    const first = redactRunDir(runDir, maskable);
    assert.equal(first.changed, 4, "every planted file changed");
    assert.deepEqual(first.opaque, []);

    for (const file of ["events.jsonl", "stream/worker-1.jsonl", "checks/c0ffee/true.log", "views/pr.md"]) {
      const text = fs.readFileSync(path.join(runDir, file), "utf8");
      assert.ok(!text.includes(VALUE), `${file} still holds the value`);
      assert.match(text, /\*\*\*FAKE_KEY\*\*\*/);
    }
    for (const line of fs.readFileSync(path.join(runDir, "events.jsonl"), "utf8").trimEnd().split("\n")) {
      assert.doesNotThrow(() => JSON.parse(line), "every events.jsonl line still parses");
    }
    for (const line of fs.readFileSync(path.join(runDir, "stream", "worker-1.jsonl"), "utf8").trimEnd().split("\n")) {
      assert.doesNotThrow(() => JSON.parse(line), "every stream line still parses");
    }
    // Nothing changes on a second pass.
    assert.equal(redactRunDir(runDir, maskable).changed, 0);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("secrets: a UTF-16 document is masked, and a file that could not be searched is named", () => {
  const secrets = [{ name: "FAKE_KEY", value: VALUE }];
  // UTF-16LE, as a vendor doc saved from a Windows editor: the value's bytes
  // are interleaved with NULs, so no UTF-8 pass can see it.
  const utf16 = Buffer.from(`key ${VALUE} end`, "utf16le");
  assert.ok(utf16.includes(0), "a UTF-16 document contains NUL bytes");
  const masked = redactBytes(utf16, secrets);
  assert.ok(!masked.includes(Buffer.from(VALUE, "utf16le")), "the value is replaced, not skipped");
  assert.equal(masked.toString("utf16le"), "key ***FAKE_KEY*** end");
  // …and UTF-16BE too.
  const be = Buffer.from(`k ${VALUE}`, "utf16le").swap16();
  assert.equal(redactBytes(be, secrets).swap16().toString("utf16le"), "k ***FAKE_KEY***");

  // A JSON-escaped form is matched as well (finding M-9): a document (or a
  // plan snapshot) storing the value inside JSON text holds it escaped.
  const odd = { name: "FAKE_KEY", value: 'sk-"odd"' };
  const escapedJson = Buffer.from(`{"k":"sk-\\"odd\\""}`, "utf8");
  assert.equal(redactBytes(escapedJson, [odd]).toString("utf8"), '{"k":"***FAKE_KEY***"}');
  const escapedUtf16 = Buffer.from('{"k":"sk-\\"odd\\""}', "utf16le");
  assert.equal(redactBytes(escapedUtf16, [odd]).toString("utf16le"), '{"k":"***FAKE_KEY***"}');
  assert.equal(redactText('{"k":"sk-\\"odd\\""}', [odd]), '{"k":"***FAKE_KEY***"}');

  const root = fs.mkdtempSync("/tmp/tt-redact-binary-");
  try {
    const runDir = path.join(root, "abcd1234");
    fs.mkdirSync(path.join(runDir, "refs"), { recursive: true });
    fs.mkdirSync(path.join(runDir, "checks"), { recursive: true });
    fs.writeFileSync(path.join(runDir, "refs", "vendor-utf16.md"), utf16);
    // A clean UTF-16 document was still searched, so it is not "opaque".
    fs.writeFileSync(path.join(runDir, "refs", "clean-utf16.md"), Buffer.from("nothing secret here", "utf16le"));
    // A genuinely binary artefact (no value in any encoding we search): the
    // value is not found, so it is NAMED rather than silently declared clean.
    fs.writeFileSync(path.join(runDir, "checks", "artifact.png"), Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x00, 0x01, 0x02, 0x03]));
    const result = redactRunDir(runDir, secrets);
    assert.equal(result.changed, 1, "the UTF-16 document changed");
    assert.equal(fs.readFileSync(path.join(runDir, "refs", "vendor-utf16.md"), "utf16le"), "key ***FAKE_KEY*** end");
    assert.deepEqual(result.opaque, [path.join(runDir, "checks", "artifact.png")]);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("secrets: utf16Kind tells a UTF-16 document from a file that cannot be searched", () => {
  assert.equal(utf16Kind(Buffer.from("plain utf8 text", "utf8")), undefined);
  assert.equal(utf16Kind(Buffer.from("\ufeffutf16 with a BOM", "utf16le")), "utf16le");
  assert.equal(utf16Kind(Buffer.from("utf16le ascii", "utf16le")), "utf16le");
  assert.equal(utf16Kind(Buffer.from("utf16be ascii", "utf16le").swap16()), "utf16be");
  // A document whose characters are mostly non-Latin-1 — an English heading and
  // a CJK body, as a vendor doc has — has NUL bytes but no every-other-byte
  // pattern, so the BOM is what identifies it (finding B-10).
  const cjk = "\ufeffVendor notes / 日本語の参考文書です。";
  assert.ok(Buffer.from(cjk, "utf16le").includes(0), "it does contain NULs");
  assert.equal(utf16Kind(Buffer.from(cjk, "utf16le")), "utf16le");
  const cjkBe = Buffer.from(cjk, "utf16le");
  cjkBe.swap16();
  cjkBe.writeUInt16BE(0xfeff, 0);
  assert.equal(utf16Kind(cjkBe), "utf16be");
  assert.equal(utf16Kind(Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x00, 0x01, 0x02, 0x03])), undefined, "a PNG is not text");
  assert.equal(utf16Kind(Buffer.alloc(0)), undefined);
});

test("secrets: the recorded missing names come from the run's own log", () => {
  const root = fs.mkdtempSync("/tmp/tt-redact-log-");
  try {
    const { runDir } = plantedRunDir(root);
    assert.deepEqual(loggedSecretStatus(runDir), { missing: [], tooShort: [] }, "no record yet");
    fs.appendFileSync(
      path.join(runDir, "events.jsonl"),
      `${JSON.stringify({ seq: 2, ts: "2026-01-01T00:00:01.000Z", kind: "secrets", event: { declared: ["FAKE_KEY", "TT"], missing: ["FAKE_KEY"], tooShort: ["TT"] } })}\n`,
    );
    assert.deepEqual(loggedSecretStatus(runDir), { missing: ["FAKE_KEY"], tooShort: ["TT"] });
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
