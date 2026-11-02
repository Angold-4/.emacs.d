import { test } from "node:test";
import assert from "node:assert/strict";
import { runCommand } from "../src/effects/shell.ts";

test("step trace", async () => {
  const r = await runCommand({ command: "true && test -z x && true", traceSteps: true, termGraceMs: 200 }).result;
  console.log(JSON.stringify({ exit: r.exitCode, out: r.output, steps: r.steps }, null, 2));
  assert.equal(r.exitCode, 1);
  assert.equal(r.steps?.at(-1)?.command, "test -z x");
});
test("multi", async () => {
  const r = await runCommand({ command: "cd /tmp && export TT_X=1 && echo \"a b\" && false || echo done", traceSteps: true, termGraceMs: 200 }).result;
  console.log(JSON.stringify({ exit: r.exitCode, out: r.output, steps: r.steps }, null, 2));
});
