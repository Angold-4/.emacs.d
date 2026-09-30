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
  assert.deepEqual(commandExecutables("n=$(cat x || echo 0); if [ $n -ge 3 ]; then sleep 1; fi"), ["echo", "sleep"]);
  assert.deepEqual(commandExecutables("exit 1"), []);
});

// Owner OD-1 / finding A-2: redirections are not separators and shell
// keywords (and their loop variables) are never executable names.
test("env-preflight: fd redirections are not separators, and keywords/loop variables are not commands", () => {
  assert.deepEqual(commandExecutables("cargo test 2>&1 | tee out.log"), ["cargo", "tee"]);
  assert.deepEqual(commandExecutables("for f in a b; do echo $f; done"), ["echo"]);
  // Every fd-redirection form the finding names.
  assert.deepEqual(commandExecutables("foo 2>&1"), ["foo"]);
  assert.deepEqual(commandExecutables("foo >&2"), ["foo"]);
  assert.deepEqual(commandExecutables("foo &>out"), ["foo"]);
  assert.deepEqual(commandExecutables("foo 2>/dev/null"), ["foo"]);
  assert.deepEqual(commandExecutables("foo 2>>log"), ["foo"]);
  assert.deepEqual(commandExecutables("foo >&bar"), ["foo"]);
  // Compound statements: the command in the body, never a keyword or the
  // `for` loop variable.
  assert.deepEqual(commandExecutables("while [ $i -lt 3 ]; do i=$((i+1)); done"), []);
  assert.deepEqual(commandExecutables("until false; do :; done"), []);
  assert.deepEqual(commandExecutables("if [ -f x ]; then make; fi"), ["make"]);
  assert.deepEqual(commandExecutables("for f in a b; do make; done"), ["make"]);
  assert.deepEqual(commandExecutables("case x in a) echo a;; esac"), []);
  // A lone `&` is still a separator.
  assert.deepEqual(commandExecutables("foo & bar"), ["foo", "bar"]);
});

// Owner OD-1 / finding A-3: a plan may name its tool by path.
test("env-preflight: absolute, relative and tilde-prefixed paths are command words", () => {
  assert.deepEqual(commandExecutables("/usr/bin/foo --x"), ["/usr/bin/foo"]);
  assert.deepEqual(commandExecutables("./gradlew build"), ["./gradlew"]);
  assert.deepEqual(commandExecutables("~/bin/x"), ["~/bin/x"]);
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
