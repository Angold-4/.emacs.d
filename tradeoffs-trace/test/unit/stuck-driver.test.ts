import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";
import { test } from "node:test";
import { registerHooks } from "node:module";
import { reduce } from "../../src/core/reduce.ts";
import { approvingReview, baseState } from "./helpers.ts";
import type { Event, PhaseStateName, Review } from "../../src/core/types.ts";

const seams = ["dispatch", "state", "paths", "applyEvent"];
const url = new URL("../../src/conductor.ts?stuck-driver", import.meta.url).href;
const hooks = registerHooks({ load(target, context, load) {
  const result = load(target, context);
  return target === url ? { ...result, source: result.source.toString().replace(new RegExp(`#(${seams.join("|")})\\b`, "g"), "test_$1") } : result;
} });
const { Conductor } = await import(url);
hooks.deregister();

for (const [from, action] of [["RESOLVING", "resolving_incomplete"], ["REPAIRING", "repair_attempt_started"], ["IMPLEMENTING", "dispatch_worker"]]) {
  test(`the real round driver stops repeated verdicts before ${action}`, () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "tt-stuck-driver-"));
    try {
      const state = baseState({ phase: from as PhaseStateName, repairRoundsGranted: 47 });
      state.phase.contract.requirements = [{ id: "R1", title: "atomic", text: "atomic writes", arch: [], verify: ["review"] }];
      const events: Event[] = [];
      for (const sha of ["C1", "C2", "C3"]) {
        events.push({ type: "FREEZE_COMPLETED", candidateSha: sha, decisions: [] });
        const reviews: Review[] = ["M", "A", "B"].map((seat) => ({
          ...approvingReview(seat, sha, state.phase.contract.contractVersion),
          items: [{ id: "R1", verdict: "unmet", evidence: "src/write.ts:12" }],
        }));
        events.push(...reviews.map((review): Event => ({ type: "REVIEW_SUBMITTED", review })), { type: "EVALUATION_COMPLETED" });
        state.phase.candidate = { sha, contractVersion: state.phase.contract.contractVersion };
        state.phase.reviews = Object.fromEntries(reviews.map((review) => [review.reviewer, { review }]));
      }
      const file = path.join(dir, "events.jsonl");
      fs.writeFileSync(file, events.map((event, i) => JSON.stringify({ kind: "event", event, seq: i + 1, ts: new Date().toISOString() })).join("\n") + "\n");
      const conductor = Object.create(Conductor.prototype);
      conductor.test_state = state;
      conductor.test_paths = { events: file };
      const emitted: Event[] = [];
      conductor.test_applyEvent = (event: Event) => {
        emitted.push(event);
        const result = reduce(conductor.test_state, event);
        assert.ok(result.ok);
        conductor.test_state = result.state;
      };
      conductor.test_dispatch({ type: action });
      assert.deepEqual(emitted.map((e) => e.type), ["PHASE_STUCK"]);
      assert.equal(conductor.test_state.phase.phase, "AWAITING_OWNER");
      assert.equal(conductor.test_state.phase.repairRoundsUsed, 0);
      assert.match(conductor.test_state.phase.ownerRequests.at(-1).reason, /^stuck: R1 unchanged for 3 rounds/);
    } finally { fs.rmSync(dir, { recursive: true, force: true }); }
  });
}
