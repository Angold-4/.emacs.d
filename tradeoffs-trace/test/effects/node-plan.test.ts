import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { test } from "node:test";
import { planForNodeStart, type NodeStartInputs } from "../../src/effects/node-plan.ts";

test("node start with a source and no exporter is a plan defect from the effect", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-node-plan-"));
  try {
    const inputs: NodeStartInputs = {
      nodeId: "n1", entryId: "entry", recorded: { phases: [] },
      source: "/plans/phase.org", exporter: "snapshot", runner: "current",
      logPath: path.join(dir, "program.log"),
    };
    const expected = {
      ok: false,
      reason: "X:PLAN_DEFECT: /plans/phase.org cannot be re-exported for node n1: no exporter is configured (set TT_EXPORT_COMMAND, or run on a host with Emacs and the runner's exporter)",
    };
    assert.deepEqual(planForNodeStart(inputs), expected);
    // A missing re-export function is equally a defect, whatever the label.
    assert.deepEqual(planForNodeStart({ ...inputs, exporter: "command" }), expected);
    assert.equal(fs.existsSync(inputs.logPath), false, "no misleading snapshot-fallback log");
    const { source: _source, ...withoutSource } = inputs;
    const legacy = planForNodeStart(withoutSource);
    assert.equal(legacy.ok, true, "no-source programs retain snapshot mode");
    if (legacy.ok) {
      assert.equal(legacy.snapshot.exporter, "snapshot");
      assert.equal(legacy.snapshot.reexported, false);
    }
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
});
