// Phase 4: the program scheduler's pure rules (core/program.ts), on the
// shapes of the atlas plans: 12 is a serial chain of four phases, 13 is
// 13a → 13b → four parallel vendors → 13g join → 13h.

import assert from "node:assert/strict";
import { test } from "node:test";

import type { RunPlanFile } from "../../src/conductor.ts";
import {
  expandProgram,
  initialProgramState,
  nextStarts,
  nodeBases,
  nodeBranch,
  nodePlan,
  programOutcome,
  reduceProgram,
  type ProgramFile,
  type ProgramState,
} from "../../src/core/program.ts";

function plan(title: string, phaseIds: string[], branch = "feat/x"): RunPlanFile {
  return {
    title,
    repo: "/tmp/repo",
    integrationBranch: branch,
    checks: ["true"],
    phases: phaseIds.map((id) => ({ id, goal: `goal ${id}`, acceptance: ["a"], checks: ["true"], boundaries: [], reserved: [] })),
  } as unknown as RunPlanFile;
}

const program13: ProgramFile = {
  title: "plan 13",
  maxParallel: 4,
  entries: [
    { id: "13a", after: [], plan: plan("13a", ["p"]) },
    { id: "13b", after: ["13a"], plan: plan("13b", ["p"]) },
    { id: "13c", after: ["13b"], plan: plan("13c", ["p"]) },
    { id: "13d", after: ["13b"], plan: plan("13d", ["p"]) },
    { id: "13e", after: ["13b"], plan: plan("13e", ["p"]) },
    { id: "13f", after: ["13b"], plan: plan("13f", ["p"]) },
    { id: "13g", after: ["13c", "13d", "13e", "13f"], plan: plan("13g", ["p"]) },
    { id: "13h", after: ["13g"], plan: plan("13h", ["p"]) },
  ],
};

function finish(state: ProgramState, ids: string[]): ProgramState {
  let s = state;
  for (const id of ids) {
    s = reduceProgram(s, { type: "NODE_STARTED", node: id, runId: `r-${id}` });
    s = reduceProgram(s, { type: "NODE_STATUS", node: id, status: "done" });
  }
  return s;
}

test("program: 13's graph runs serially, then the four vendors in parallel, then the join", () => {
  const nodes = expandProgram(program13);
  let s = initialProgramState(nodes);
  assert.deepEqual(nextStarts(nodes, s, 4), ["13a"]);
  s = finish(s, ["13a"]);
  assert.deepEqual(nextStarts(nodes, s, 4), ["13b"]);
  s = finish(s, ["13b"]);
  assert.deepEqual(nextStarts(nodes, s, 4), ["13c", "13d", "13e", "13f"], "the parallel wave");
  assert.deepEqual(nextStarts(nodes, s, 2), ["13c", "13d"], "maxParallel caps the wave");
  s = finish(s, ["13c", "13d", "13e"]);
  assert.deepEqual(nextStarts(nodes, s, 4), ["13f"], "13g waits for every vendor");
  s = finish(s, ["13f"]);
  assert.deepEqual(nextStarts(nodes, s, 4), ["13g"]);
  s = finish(s, ["13g", "13h"]);
  assert.equal(programOutcome(nodes, s), "done");
});

test("program: a running, needs-you or stopped node holds its slot; only DONE releases dependents", () => {
  const nodes = expandProgram(program13);
  let s = finish(initialProgramState(nodes), ["13a", "13b"]);
  s = reduceProgram(s, { type: "NODE_STARTED", node: "13c", runId: "r" });
  s = reduceProgram(s, { type: "NODE_STATUS", node: "13c", status: "needs-you" });
  assert.deepEqual(nextStarts(nodes, s, 2), ["13d"], "13c still holds one of two slots");
  assert.equal(programOutcome(nodes, s), "running", "an owner correction may still finish it");
});

test("program: a BLOCKED node leaves its dependents waiting and the program stuck once nothing else can run", () => {
  const nodes = expandProgram(program13);
  let s = finish(initialProgramState(nodes), ["13a", "13b", "13c", "13d", "13e"]);
  s = reduceProgram(s, { type: "NODE_BLOCKED", node: "13f", reason: "merge conflict" });
  assert.deepEqual(nextStarts(nodes, s, 4), []);
  assert.equal(programOutcome(nodes, s), "stuck");
});

test("program: a multi-phase plan expands to serial nodes, and its first phase waits for its dependencies", () => {
  const p: ProgramFile = {
    title: "plan 12",
    maxParallel: 2,
    entries: [
      { id: "prep", after: [], plan: plan("prep", ["only"]) },
      { id: "12", after: ["prep"], plan: plan("12", ["12a", "12b", "12c", "12d"]) },
    ],
  };
  const nodes = expandProgram(p);
  assert.deepEqual(
    nodes.map((n) => [n.id, n.deps]),
    [
      ["prep", []],
      ["12/12a", ["prep"]],
      ["12/12b", ["12/12a"]],
      ["12/12c", ["12/12b"]],
      ["12/12d", ["12/12c"]],
    ],
  );
  const node = nodes.find((n) => n.id === "12/12c")!;
  const np = nodePlan(p, node);
  assert.equal(np.phases.length, 1, "each node's run gets exactly its one phase");
  assert.equal(np.phases[0].id, "12c");
});

test("program: stacked branches — each node publishes to its own branch, cut from its dependencies'", () => {
  const nodes = expandProgram(program13);
  const byId = (id: string) => nodes.find((n) => n.id === id)!;
  assert.equal(nodeBranch(program13, byId("13c")), "feat/x--13c");
  assert.deepEqual(nodeBases(program13, nodes, byId("13a")), ["feat/x"], "a root node starts from TT_BRANCH");
  assert.deepEqual(nodeBases(program13, nodes, byId("13c")), ["feat/x--13b"], "one PR per phase, stacked on 13b");
  assert.deepEqual(
    nodeBases(program13, nodes, byId("13g")),
    ["feat/x--13c", "feat/x--13d", "feat/x--13e", "feat/x--13f"],
    "the join starts from all four vendors",
  );
  assert.equal(nodePlan(program13, byId("13g")).integrationBranch, "feat/x--13g");
  assert.equal(nodeBranch({ ...program13, branches: "shared" }, byId("13c")), "feat/x");
});

test("program: invalid graphs are refused with the reason", () => {
  const bad = (entries: ProgramFile["entries"]) => () => expandProgram({ title: "t", maxParallel: 1, entries });
  assert.throws(bad([{ id: "a", after: ["zz"], plan: plan("a", ["p"]) }]), /unknown entry zz/);
  assert.throws(
    bad([
      { id: "a", after: ["b"], plan: plan("a", ["p"]) },
      { id: "b", after: ["a"], plan: plan("b", ["p"]) },
    ]),
    /dependency cycle/,
  );
  assert.throws(
    bad([
      { id: "a", after: [], plan: plan("a", ["p"]) },
      { id: "a", after: [], plan: plan("a", ["p"]) },
    ]),
    /duplicate/,
  );
});
