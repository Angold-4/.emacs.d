// Plan 05i: the pure environment preflight. It parses every declared shell
// command into the first word of each simple command (skipping shell builtins
// and variable assignments) and resolves those words with an injected
// `resolve`; the conductor and the program commands supply `command -v`.

import assert from "node:assert/strict";
import { test } from "node:test";

import { commandExecutables, envBlockedLine, envPreflight, envToolsLines } from "../../src/core/env-preflight.ts";

test("env-preflight: executables of a && chain, a flag-style command, an assignment+builtin chain and a pipeline", () => {
  assert.deepEqual(
    commandExecutables("cargo fmt --all -- --check && cargo test -p x && cargo xtask check dep-rules"),
    ["cargo"],
  );
  assert.deepEqual(commandExecutables("make -C dir check"), ["make"]);
  assert.deepEqual(commandExecutables("cd d && FOO=1 node --test"), ["node"]);
  assert.deepEqual(commandExecutables("cargo test -p x | tee out.log"), ["cargo", "tee"]);
});

test("env-preflight: builtins, keywords, variable assignments and expansions are skipped", () => {
  assert.deepEqual(commandExecutables("cd d && test -f x && true && [ -n \"$x\" ]"), []);
  assert.deepEqual(commandExecutables("FOO=1 BAR=2 cargo test"), ["cargo"]);
  // The command inside the `$(...)` substitution is deliberately not
  // resolved (its value has no single command token); the compound
  // statement's own command is.
  assert.deepEqual(commandExecutables("n=$(cat x || echo 0); if [ $n -ge 3 ]; then sleep 1; fi"), ["sleep"]);
  assert.deepEqual(commandExecutables("exit 1"), []);
  assert.deepEqual(commandExecutables("echo $n > /tmp/out"), []);
});

test("env-preflight: resolves every executable and reports the missing ones with the PATH", () => {
  const paths: Record<string, string> = {
    cargo: "/Users/x/.cargo/bin/cargo",
    node: "/usr/local/bin/node",
  };
  const result = envPreflight(
    ["cargo fmt && cargo test -p x && cargo xtask check dep-rules", "cd d && FOO=1 node --test"],
    { path: "/Users/x/.cargo/bin:/usr/local/bin", resolve: (name) => paths[name] },
  );
  assert.deepEqual(result.missing, []);
  assert.deepEqual(result.tools, [
    { name: "cargo", path: "/Users/x/.cargo/bin/cargo" },
    { name: "node", path: "/usr/local/bin/node" },
  ]);
  assert.equal(result.path, "/Users/x/.cargo/bin:/usr/local/bin");

  const missing = envPreflight(["cargo test", "make -C dir check"], {
    path: "/usr/bin",
    resolve: (name) => (name === "cargo" ? "/Users/x/.cargo/bin/cargo" : undefined),
  });
  assert.deepEqual(missing.missing, ["make"]);
  assert.deepEqual(missing.tools, [
    { name: "cargo", path: "/Users/x/.cargo/bin/cargo" },
    { name: "make" },
  ]);
});

test("env-preflight: the owner-facing block line and tool rows", () => {
  assert.equal(
    envBlockedLine({ kind: "preflight", missing: ["cargo"], path: "/usr/bin:/bin" }),
    "env blocked · cargo not found on PATH (/usr/bin:/bin)",
  );
  assert.equal(
    envBlockedLine({ kind: "check", stage: "checks", command: "cargo test", exitCode: 127 }),
    "env blocked · cargo test exit 127 — the tool is not available here",
  );
  assert.deepEqual(envToolsLines([{ name: "cargo", path: "/Users/x/.cargo/bin/cargo" }, { name: "make" }]), [
    "env  cargo /Users/x/.cargo/bin/cargo",
    "env  make (not found)",
  ]);
});
