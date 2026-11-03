import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { test } from "node:test";
import { checkEnvironment, recordCheckEnvironment, runPlanPreflight } from "../../src/effects/check-env.ts";
import type { RunPlanFile } from "../../src/conductor.ts";

test("preflight runs once in the recorded check environment at the program base and refuses a broken toolchain", async () => {
  const dir = fs.mkdtempSync("/tmp/tt-preflight-");
  const repo = path.join(dir, "repo");
  fs.mkdirSync(repo);
  const git = (...args: string[]) => execFileSync("git", ["-C", repo, ...args], { encoding: "utf8" });
  try {
    git("init", "-q", "-b", "main");
    fs.writeFileSync(path.join(repo, "version"), "base\n");
    git("add", ".");
    git("-c", "user.name=Test", "-c", "user.email=test@test", "commit", "-qm", "base");
    git("branch", "program-base");
    fs.writeFileSync(path.join(repo, "version"), "later main\n");
    git("add", ".");
    git("-c", "user.name=Test", "-c", "user.email=test@test", "commit", "-qm", "later main");
    fs.writeFileSync(path.join(repo, "version"), "dirty\n");
    const bin = path.join(dir, "bin");
    fs.mkdirSync(bin);
    const marker = path.join(dir, "calls");
    fs.writeFileSync(path.join(bin, "toolchain-guard"), `#!/bin/sh\necho once >> '${marker}'\nIFS= read -r version < version\nprintf '%s:%s:%s:%s\\n' "$version" "$TOOLCHAIN" "\${SHOULD_STAY_UNSET-unset}" "$NO_COLOR"\necho 'CMake 3.30 or higher is required' >&2\nexit 9\n`, { mode: 0o755 });
    const plan = {
      title: "preflight", repo, integrationBranch: "main", checks: ["echo $TOOLCHAIN $SHOULD_STAY_UNSET"],
      preflight: [`printf 'first\\n' >> '${marker}'`, "toolchain-guard", `printf 'must-not-run\\n' >> '${marker}'`], phases: [],
    } as RunPlanFile;
    const record = recordCheckEnvironment(plan, { PATH: bin, TOOLCHAIN: "recorded" });
    assert.equal(record.SHOULD_STAY_UNSET, null);
    const failure = await runPlanPreflight(plan, "program-base", record);
    assert.match(failure ?? "", /preflight `toolchain-guard` exited 9/);
    assert.match(failure!, /base:recorded:unset:1/);
    assert.match(failure!, /CMake 3.30 or higher is required/);
    assert.equal(fs.readFileSync(marker, "utf8"), "first\nonce\n");
    assert.equal(fs.readFileSync(path.join(repo, "version"), "utf8"), "dirty\n");
    fs.rmSync(marker);
    assert.equal(await runPlanPreflight({ ...plan, preflight: [
      `printf 'one\\n' >> '${marker}'`, `printf 'two\\n' >> '${marker}'`,
    ] }, "main", record), undefined);
    assert.equal(fs.readFileSync(marker, "utf8"), "one\ntwo\n", "every successful command runs once, in order");
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});

test("preflight preserves command order, succeeds without rounds, and is optional", async () => {
  const plan = { repo: "/does/not/exist", checks: [], phases: [] } as unknown as RunPlanFile;
  assert.equal(await runPlanPreflight(plan, "main", {}), undefined);
  const env = checkEnvironment({ PATH: "/recorded", ABSENT: null }, { PATH: "/caller", ABSENT: "new", FORCE_COLOR: "3", NODE_TEST_CONTEXT: "child" });
  assert.deepEqual(env, { PATH: "/recorded", NO_COLOR: "1" });
  assert.deepEqual(recordCheckEnvironment({ ...plan, secrets: ["SECRET"], checks: ["echo $SECRET"] }, { PATH: "/bin", SECRET: "private" }), { PATH: "/bin" });
});
