import assert from "node:assert/strict";
import { test } from "node:test";
import { computeBoundaryTriggerPaths } from "../../src/core/boundaries.ts";

test("permitted paths create no mechanical decision, including manifests and acceptance files", () => {
  assert.deepEqual(computeBoundaryTriggerPaths(
    ["src/a.ts", "src/package.json", "test/acceptance.test.ts", "Cargo.lock", "outside/package.json"],
    ["src/**", "test/**", "Cargo.lock"], ["test/acceptance.test.ts"], [],
  ), ["outside/package.json"]);
});

test("an outside path still triggers unless a decision or finding cites it", () => {
  assert.deepEqual(computeBoundaryTriggerPaths(["other/package.json", "other/Cargo.toml"], ["src/**"], [], ["other/package.json:1 needs approval"]), ["other/Cargo.toml"]);
});
