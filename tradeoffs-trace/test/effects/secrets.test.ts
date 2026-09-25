// Plan 01a: the secrets module — name parsing, resolving from the
// environment, redaction of text/JSON/JSONL, prompt lines and the run
// directory rewrite `tt redact` is built on.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";

import {
  loggedMissingSecrets,
  planSecretNames,
  redactJson,
  redactJsonl,
  redactRunDir,
  redactText,
  resolveSecrets,
  secretNames,
  secretPromptLines,
} from "../../src/effects/secrets.ts";

const VALUE = "sk-live-4f8a2b1c9d3e";

test("secrets: names are parsed, deduped and validated; values come from the environment", () => {
  assert.deepEqual(secretNames(["FAKE_KEY"]), ["FAKE_KEY"]);
  assert.deepEqual(secretNames(["FAKE_KEY  OTHER_KEY,FAKE_KEY"]), ["FAKE_KEY", "OTHER_KEY"]);
  assert.deepEqual(secretNames(undefined), []);
  assert.deepEqual(secretNames(["1BAD", "BAD-NAME", "OK_KEY"]), ["OK_KEY"], "only shell-assignable names");

  const { secrets, missing } = resolveSecrets(["FAKE_KEY", "GONE_KEY"], { FAKE_KEY: VALUE, GONE_KEY: "" });
  assert.deepEqual(secrets, [{ name: "FAKE_KEY", value: VALUE }]);
  assert.deepEqual(missing, ["GONE_KEY"], "an empty variable counts as unset");
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
    const { secrets, missing } = resolveSecrets(planSecretNames(runDir), { FAKE_KEY: VALUE });
    assert.deepEqual(missing, []);
    const changed = redactRunDir(runDir, secrets);
    assert.equal(changed, 4, "every planted file changed");

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
    assert.equal(redactRunDir(runDir, secrets), 0);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});

test("secrets: the recorded missing names come from the run's own log", () => {
  const root = fs.mkdtempSync("/tmp/tt-redact-log-");
  try {
    const { runDir } = plantedRunDir(root);
    assert.deepEqual(loggedMissingSecrets(runDir), [], "no record yet");
    fs.appendFileSync(
      path.join(runDir, "events.jsonl"),
      `${JSON.stringify({ seq: 2, ts: "2026-01-01T00:00:01.000Z", kind: "secrets", event: { declared: ["FAKE_KEY"], missing: ["FAKE_KEY"] } })}\n`,
    );
    assert.deepEqual(loggedMissingSecrets(runDir), ["FAKE_KEY"]);
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
