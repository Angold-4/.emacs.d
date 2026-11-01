import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";
import { pruneRunTargets } from "../../src/effects/prune.ts";

test("prune keeps a build-output-named folder that holds tracked files", () => {
  const run = fs.mkdtempSync(path.join(os.tmpdir(), "tt-prune-tracked-"));
  const checkout = path.join(run, "candidates", "abc");
  try {
    fs.mkdirSync(checkout, { recursive: true });
    execFileSync("git", ["init", "-q", checkout]);
    const tracked = path.join(checkout, "packages", "web", "target");
    const generated = path.join(checkout, "packages", "api", "target");
    fs.mkdirSync(tracked, { recursive: true });
    fs.mkdirSync(generated, { recursive: true });
    fs.writeFileSync(path.join(tracked, "source.txt"), "tracked content\n");
    fs.writeFileSync(path.join(tracked, "generated.txt"), "also keep the whole folder\n");
    fs.writeFileSync(path.join(generated, "output.bin"), "generated\n");
    execFileSync("git", ["-C", checkout, "add", "packages/web/target/source.txt"]);
    const result = pruneRunTargets(run, {});
    assert.equal(fs.readFileSync(path.join(tracked, "source.txt"), "utf8"), "tracked content\n");
    assert.equal(fs.existsSync(path.join(tracked, "generated.txt")), true);
    assert.equal(fs.existsSync(generated), false);
    assert.deepEqual(result.removed, [generated]);
    assert.ok(fs.readFileSync(path.join(run, "conductor.log"), "utf8").includes(`kept ${tracked}: holds tracked files`));
    // A git failure cannot be interpreted as an empty tracked-file list.
    fs.rmSync(path.join(checkout, ".git"), { recursive: true });
    pruneRunTargets(run, {});
    assert.equal(fs.existsSync(tracked), true);
  } finally {
    fs.rmSync(run, { recursive: true, force: true });
  }
});
