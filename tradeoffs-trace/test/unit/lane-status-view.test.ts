// tt 09-6: `tt status` on a lane round shows, per lane, its current
// candidate's check result (green, or red with the first failing command, and
// the round's fix count) and each seat's item verdicts (met/total, and which
// items are not met by whom), so the owner sees per-item progress every round
// without asking. Snapshot of the status panel's lane block.

import assert from "node:assert/strict";
import { test } from "node:test";

import { laneDetailLines, renderStatusView, type StatusViewInput } from "../../src/render.ts";

const verdicts = (met: string[], unmet: string[]) => ({
  items: [...met.map((id) => ({ id, verdict: "met", evidence: "e" })), ...unmet.map((id) => ({ id, verdict: "unmet", evidence: "e" }))],
});

function lanePhase(): Record<string, unknown> {
  return {
    phaseId: "p1",
    phase: "LANE_REVIEW",
    runId: "run-1",
    attempt: { n: 2 },
    repairRoundsUsed: 1,
    repairRoundsGranted: 3,
    findings: [],
    decisions: [],
    ballots: [],
    messages: [],
    contract: { seats: ["M", "A", "B"] },
    rounds: [
      { round: 1, base: "base000", lanes: ["a", "b"], candidates: [{ lane: "a", sha: "0000000aaaa", ok: true }], votes: [] },
      {
        round: 2,
        base: "base111",
        lanes: ["a", "b", "c"],
        candidates: [
          {
            lane: "a",
            sha: "1234567abcdef",
            ok: true,
            fixes: 1,
            reviews: [
              { seat: "M", review: verdicts(["R1", "R2", "R3"], ["R4"]) },
              { seat: "A", review: verdicts(["R1", "R2", "R3", "R4"], []) },
            ],
          },
          {
            lane: "b",
            sha: "89abcde123456",
            ok: false,
            fixes: 2,
            note: "the check `cargo test -p tcl-equivalence && test -z \"$(rg -n 'Vec<PositionFact>' crates/modules/prm/domain/src)\"` exited 1",
          },
          { lane: "c", ok: false, note: "the lane failed: the worker crashed" },
        ],
        votes: [],
      },
    ],
  };
}

const view = {
  stage: "lane review",
  elapsed: "41m",
  round: 2,
  laneElapsed: { a: "9m10s", b: "12m00s" },
  envTools: [],
};

test("tt 09-6: the status shows each lane's check result and the seats' item verdicts", () => {
  const text = renderStatusView({
    runDir: "/tmp/tt-09-6-status",
    title: "remove the fact vectors",
    phase: lanePhase(),
    alive: true,
    view,
    ownerInputs: [],
    pendingOwnerInputs: [],
    ownerDirectives: [],
  } as unknown as StatusViewInput);
  const lines = text.split("\n");
  const start = lines.indexOf("Lanes (round 2)");
  assert.ok(start >= 0, text);
  let end = start + 1;
  while (end < lines.length && lines[end].startsWith(" ")) end += 1;
  const block = lines.slice(start, end);
  // The 06m lane table (header and one padded cell row) stays as it was ...
  assert.equal(block[1], "  lane a  |  lane b  |  lane c");
  assert.match(block[2], /^ {2}in review 1234567 9m10s +\| {2}checks fail 89abcde 12m00s {2}first fail: /);
  // ... and under it, a few lines per lane (the snapshot).
  assert.equal(
    block.slice(3).join("\n"),
    [
      "  a  1234567 green · fixes 1",
      "     items M 3/4 · A 4/4 · B … — not met: R4 (M)",
      "  b  89abcde red — the check `cargo test -p tcl-equivalence && test -z \"$(rg -n 'Vec<Posi…` exited 1 · fixes 2",
      "  c  no candidate: the lane failed: the worker crashed",
    ].join("\n"),
  );
});

test("tt 09-6: a lane still checking, and a phase without rounds", () => {
  const phase = lanePhase();
  const rounds = phase.rounds as Array<{ candidates: Array<Record<string, unknown>> }>;
  rounds[1].candidates[0] = { lane: "a", sha: "1234567abcdef" };
  assert.deepEqual(laneDetailLines(phase).slice(0, 1), ["  a  1234567 checking · fixes 0"]);
  assert.deepEqual(laneDetailLines({ ...phase, rounds: [] }), []);
});
