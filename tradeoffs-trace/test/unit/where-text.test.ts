// Plan 06b symbol check: a `:WHERE:` token may name a directory (a crate or a
// package). Its text is every source file under it, so a symbol declared in a
// nested file is found; build output is skipped; a missing path still throws,
// which the conductor records as a deviation.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";

import { readWhereText } from "../../src/conductor.ts";

test("plan 06b: a :WHERE: directory is read as every source file under it", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tt-where-"));
  try {
    fs.mkdirSync(path.join(root, "crate", "src", "nested"), { recursive: true });
    fs.mkdirSync(path.join(root, "crate", "target", "debug"), { recursive: true });
    fs.writeFileSync(path.join(root, "crate", "src", "lib.rs"), "pub enum BuildError {}\n");
    fs.writeFileSync(path.join(root, "crate", "src", "nested", "ext.rs"), "pub struct ValuationExtension;\n");
    fs.writeFileSync(path.join(root, "crate", "target", "debug", "gen.rs"), "pub struct OnlyInTarget;\n");
    const text = readWhereText(path.join(root, "crate"));
    assert.match(text, /pub enum BuildError/);
    assert.match(text, /pub struct ValuationExtension/);
    assert.doesNotMatch(text, /OnlyInTarget/, "build output is skipped");
    assert.equal(readWhereText(path.join(root, "crate", "src", "lib.rs")), "pub enum BuildError {}\n", "a file is read as it is");
    assert.throws(() => readWhereText(path.join(root, "missing")), "a missing path throws");
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
});
