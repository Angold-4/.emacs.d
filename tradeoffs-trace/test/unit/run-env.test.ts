// exex 01a2: a conductor relaunched from a bare shell (no ~/.local/bin on its
// PATH) could not find `pi` and crashed at its next agent launch.

import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { test } from "node:test";
import { adoptRecordedPath } from "../../src/effects/run-env.ts";

test("exex 01a2: a relaunched conductor puts the run's recorded PATH ahead of the shell's", () => {
  const runDir = mkdtempSync(path.join(tmpdir(), "tt-run-env-"));
  writeFileSync(path.join(runDir, "check-env.json"), JSON.stringify({ PATH: "/home/coder/.local/bin:/usr/bin" }));
  const env: NodeJS.ProcessEnv = { PATH: "/usr/bin:/bin" };
  adoptRecordedPath(runDir, env);
  assert.equal(env.PATH, "/home/coder/.local/bin:/usr/bin:/usr/bin:/bin");
});

test("exex 01a2: with no recorded PATH the environment is left as it is", () => {
  const runDir = mkdtempSync(path.join(tmpdir(), "tt-run-env-"));
  const env: NodeJS.ProcessEnv = { PATH: "/usr/bin" };
  adoptRecordedPath(runDir, env);
  assert.equal(env.PATH, "/usr/bin");
  writeFileSync(path.join(runDir, "check-env.json"), "not json");
  adoptRecordedPath(runDir, env);
  assert.equal(env.PATH, "/usr/bin");
});
