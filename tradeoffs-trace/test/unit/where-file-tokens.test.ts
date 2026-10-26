import assert from "node:assert/strict";
import { test } from "node:test";
import { whereFileTokens } from "../../src/conductor.ts";

test("whereFileTokens keeps file paths and leaves globs and bare module names to the reviewers", () => {
  assert.deepEqual(
    whereFileTokens("crates/modules/me/domain/src/** crates/x/src/lib.rs, overlay module; tests/*.rs Cargo.toml"),
    ["crates/x/src/lib.rs", "Cargo.toml"],
  );
  assert.deepEqual(whereFileTokens("src/core/{a,b}.ts src/[ab].ts src/a?.ts"), []);
});
