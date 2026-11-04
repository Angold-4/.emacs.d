import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { compatibilityTests, inventoryCargoArgs } from "../../src/core/compatibility.ts";
import { runCompatibilityInventory } from "../../src/effects/compatibility.ts";

test("compatibility keeps each test's result rather than assigning the group exit to every test", () => {
  const tests = compatibilityTests(`PASS [0.01s] (1/3) pkg::suite good
FAIL [0.02s] (2/3) pkg::suite stale
SKIP [      ] (3/3) pkg::suite ignored
not ok 1 - affected group
ok 2 - other group`, "python3 final.py");
  assert.deepEqual(tests.map(({ name, outcome }) => [name, outcome]), [
    ["pkg::suite good", "passed"], ["pkg::suite stale", "failed"], ["pkg::suite ignored", "skipped"],
    ["affected group", "failed"], ["other group", "passed"],
  ]);
  assert.equal(tests[1].command, "python3 final.py");
  const tap = compatibilityTests("not ok 1 - stale\n  ---\n  location: 'test/stale.test.js:4:1'\n  ...", "node --test");
  assert.equal(tap[0].file, "test/stale.test.js");
  assert.match(tap[0].command, /node --test --test-name-pattern 'stale' 'test\/stale.test.js'/);
});

test("compatibility disables cargo fail-fast without changing list, build or test-binary arguments", () => {
  assert.deepEqual(inventoryCargoArgs(["nextest", "run", "--fail-fast", "--max-fail", "1", "-p", "toy"]),
    ["nextest", "run", "-p", "toy", "--no-fail-fast"]);
  assert.deepEqual(inventoryCargoArgs(["+nightly", "test", "-p", "toy", "--", "--exact", "x"]),
    ["+nightly", "test", "-p", "toy", "--no-fail-fast", "--", "--exact", "x"]);
  assert.deepEqual(inventoryCargoArgs(["nextest", "list"]), ["nextest", "list"]);
});

test("compatibility runs later final commands after failures, including cargo behind an opaque wrapper", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-compatibility-test-");
  try {
    fs.writeFileSync(path.join(dir, "cargo"), "#!/bin/sh\nprintf '%s\\n' \"$@\"\n", { mode: 0o755 });
    const record = await runCompatibilityInventory({ candidateSha: "C1", cwd: dir,
      commands: ["printf 'not ok 1 - stale\\n'; exit 1", "sh -c 'cargo nextest run --fail-fast'", "printf 'ok 1 - later\\n'"],
      env: { ...process.env, PATH: `${dir}:${process.env.PATH}` }, deadlineMs: 10_000, termGraceMs: 100,
      stopped: () => false, onIntent: () => {}, onExit: () => {},
    });
    assert.equal(record.complete, true);
    assert.deepEqual(record.commands.map((c) => c.exitCode), [1, 0, 0]);
    assert.match(record.commands[1].output, /--no-fail-fast/);
    assert.doesNotMatch(record.commands[1].output, /^--fail-fast$/m);
    assert.equal(record.commands[0].tests[0].outcome, "failed");
    assert.equal(record.commands[2].tests[0].outcome, "passed");
  } finally { fs.rmSync(dir, { recursive: true, force: true }); }
});

test("compatibility has one total deadline and never reports an unexecuted suffix complete", async () => {
  const record = await runCompatibilityInventory({ candidateSha: "C1", cwd: "/tmp",
    commands: ["sleep 10", "printf 'ok 1 - never ran\\n'"], env: process.env,
    deadlineMs: 200, termGraceMs: 100, stopped: () => false, onIntent: () => {}, onExit: () => {},
  });
  assert.equal(record.complete, false);
  assert.equal(record.commands.length, 1);
  assert.equal(record.commands[0].timedOut, true);
});

test("a signalled suite cannot claim a complete inventory, and later final commands still run", async () => {
  const record = await runCompatibilityInventory({ candidateSha: "C1", cwd: "/tmp",
    commands: ["kill -KILL $$", "printf 'ok 1 - later suite\\n'"], env: process.env,
    deadlineMs: 5_000, termGraceMs: 100, stopped: () => false, onIntent: () => {}, onExit: () => {},
  });
  assert.equal(record.complete, false);
  assert.equal(record.commands[0].exitCode, null);
  assert.equal(record.commands[1].tests[0].outcome, "passed");
});
