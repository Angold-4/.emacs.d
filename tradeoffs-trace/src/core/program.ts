// Phase 4: programs — several plans (and multi-phase plans) run as one
// deterministic dependency graph. Pure: no I/O. src/program.ts is the effect
// layer that turns `nextStarts` into ordinary single-phase runs.
//
// A program is a list of nodes. Each node is ONE phase of a plan and becomes
// one ordinary tradeoffs-trace run, with the same conductor and review loop.
// A node starts only when every node it depends on is DONE, so its run begins
// from an integration head that already contains their published results.
// Independent nodes run in parallel, up to `maxParallel`. Parallel nodes may
// share an integration branch: publishing is a compare-and-swap, and a stale
// publish re-probes against the new head (design §6.4).
//
// A multi-phase plan expands into one node per phase, each after the
// previous one, and the first after the plan's own dependencies.

import type { RunPlanFile } from "../conductor.ts";

export interface ProgramPlanEntry {
  /** Unique id of the entry in the program (e.g. "13c"). */
  id: string;
  /** Entries this one starts after (all of their phases). */
  after: string[];
  plan: RunPlanFile;
}

export interface ProgramFile {
  title: string;
  maxParallel: number;
  entries: ProgramPlanEntry[];
  /** "stack" (default): each node publishes to its own branch, based on its
   * dependencies' branches, so every phase becomes its own (stacked) PR.
   * "shared": every node publishes to its plan's TT_BRANCH. */
  branches?: "stack" | "shared";
}

export interface ProgramNode {
  /** "<entry>" for a one-phase plan, "<entry>/<phaseId>" otherwise. */
  id: string;
  entry: string;
  phaseIndex: number;
  deps: string[];
}

/** Plan 01i (D5): a program-wide owner directive. It is a logged program
 * event, rebuilt by folding, delivered to every running node through its
 * inbox and included in the plan of every node started later. */
export interface ProgramDirective {
  id: string; // "ODP-<n>" — the program-wide namespace (see `nextProgramDirectiveId`)
  text: string;
  at: string;
  withdrawn?: boolean;
  /** The node run that issued it (`C-u` in a run's input box), if any; the
   * scheduler does not send it back to that node (it already applies it). */
  origin?: string;
  /** The issuing node's inbox command id, when a node forwarded it: the same
   * file may be re-written after a crash, and the key makes that a no-op. */
  key?: string;
}

/** What the scheduler knows about a node. `needs-you` and `stopped` are not
 * terminal: the owner can correct or resume the run, and it may still reach
 * DONE. Only `done` releases dependents; only `blocked` is final. */
export type NodeStatus = "waiting" | "running" | "needs-you" | "stopped" | "done" | "blocked";

export interface ProgramState {
  nodes: Record<string, { status: NodeStatus; runId?: string; branch?: string; base?: string; reason?: string; resumes?: number }>;
  stopped: boolean;
  /** Plan 01i: program-wide owner directives in force, in the order the owner
   * sent them. Rebuilt by folding the program's own event log. */
  directives?: ProgramDirective[];
}

export type ProgramEvent =
  | { type: "NODE_STARTED"; node: string; runId: string; branch?: string; base?: string }
  | { type: "NODE_BLOCKED"; node: string; reason: string }
  | { type: "NODE_RESUMED"; node: string; reason: "crashed" | "owner" }
  | { type: "PROGRAM_RESUMED" }
  /** `tt program retry`: a blocked (or stopped) node goes back to waiting and
   * runs again as a fresh run; its branch is kept. */
  | { type: "NODE_RETRY"; node: string }
  | { type: "NODE_STATUS"; node: string; status: Exclude<NodeStatus, "waiting"> }
  | { type: "DIRECTIVE_ADDED"; directive: ProgramDirective }
  | { type: "DIRECTIVE_WITHDRAWN"; directiveId: string }
  | { type: "PROGRAM_STOPPED" };

/** Expands entries into phase nodes and validates the graph: unique ids,
 * known dependencies, no cycle. Throws with a readable reason. */
export function expandProgram(program: ProgramFile): ProgramNode[] {
  const ids = new Set<string>();
  for (const e of program.entries) {
    if (ids.has(e.id)) throw new Error(`duplicate program entry id ${e.id}`);
    ids.add(e.id);
    if (e.plan.phases.length === 0) throw new Error(`entry ${e.id}: its plan has no phases`);
  }
  const lastNodeOf = new Map<string, string>();
  const nodes: ProgramNode[] = [];
  for (const e of program.entries) {
    for (const d of e.after) {
      if (!ids.has(d)) throw new Error(`entry ${e.id} starts after unknown entry ${d}`);
    }
  }
  for (const e of program.entries) {
    const single = e.plan.phases.length === 1;
    e.plan.phases.forEach((ph, i) => {
      nodes.push({
        id: single ? e.id : `${e.id}/${ph.id}`,
        entry: e.id,
        phaseIndex: i,
        deps: i === 0 ? [] : [single ? e.id : `${e.id}/${e.plan.phases[i - 1].id}`],
      });
    });
    lastNodeOf.set(e.id, nodes[nodes.length - 1].id);
  }
  // An entry's first phase waits for the LAST phase of each dependency.
  for (const n of nodes) {
    if (n.phaseIndex !== 0) continue;
    const entry = program.entries.find((e) => e.id === n.entry)!;
    n.deps = entry.after.map((d) => lastNodeOf.get(d)!);
  }
  assertAcyclic(nodes);
  return nodes;
}

function assertAcyclic(nodes: ProgramNode[]): void {
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const mark = new Map<string, "visiting" | "done">();
  const visit = (id: string, path: string[]): void => {
    if (mark.get(id) === "done") return;
    if (mark.get(id) === "visiting") throw new Error(`dependency cycle: ${[...path, id].join(" → ")}`);
    mark.set(id, "visiting");
    for (const d of byId.get(id)!.deps) visit(d, [...path, id]);
    mark.set(id, "done");
  };
  for (const n of nodes) visit(n.id, []);
}

export function initialProgramState(nodes: ProgramNode[]): ProgramState {
  return { nodes: Object.fromEntries(nodes.map((n) => [n.id, { status: "waiting" as NodeStatus }])), stopped: false, directives: [] };
}

/** Plan 01i: the program-wide directives still in force, oldest first by
 * their `ODP-<n>` number — never in the order the inbox happened to hand the
 * files over. */
export function programDirectivesInForce(state: ProgramState): ProgramDirective[] {
  return (state.directives ?? [])
    .filter((d) => !d.withdrawn)
    .sort((a, b) => directiveNumber(a.id) - directiveNumber(b.id));
}

function directiveNumber(id: string): number {
  return Number(id.match(/^(?:ODP|OD)-(\d+)$/)?.[1] ?? 0);
}

/** Plan 01i: the program's next free directive id.
 *
 * Program-wide rulings live in their own namespace (`ODP-<n>`) so that one id
 * always names one ruling: a node's own phase directives are `OD-<n>`, and a
 * node can never have to renumber a program ruling into one of its own
 * numbers. `withdraw OD-n` / `withdraw ODP-n` then retires exactly the record
 * it names, at either level. */
export function nextProgramDirectiveId(state: ProgramState): string {
  const max = (state.directives ?? []).reduce((m, d) => {
    const n = Number(d.id.match(/^(?:ODP|OD)-(\d+)$/)?.[1] ?? 0);
    return Math.max(m, n);
  }, 0);
  return `ODP-${max + 1}`;
}

export function reduceProgram(state: ProgramState, event: ProgramEvent): ProgramState {
  switch (event.type) {
    case "NODE_STARTED":
      return {
        ...state,
        nodes: { ...state.nodes, [event.node]: { status: "running", runId: event.runId, branch: event.branch, base: event.base } },
      };
    case "NODE_BLOCKED":
      return { ...state, nodes: { ...state.nodes, [event.node]: { ...state.nodes[event.node], status: "blocked", reason: event.reason } } };
    case "NODE_STATUS": {
      const prev = state.nodes[event.node];
      if (!prev) return state;
      return { ...state, nodes: { ...state.nodes, [event.node]: { ...prev, status: event.status } } };
    }
    case "PROGRAM_STOPPED":
      return { ...state, stopped: true };
    case "PROGRAM_RESUMED":
      return { ...state, stopped: false };
    case "NODE_RETRY": {
      const prev = state.nodes[event.node];
      if (!prev || prev.status === "done" || prev.status === "running") return state;
      return { ...state, nodes: { ...state.nodes, [event.node]: { status: "waiting", branch: prev.branch, base: prev.base } } };
    }
    case "NODE_RESUMED": {
      const prev = state.nodes[event.node];
      if (!prev) return state;
      return { ...state, nodes: { ...state.nodes, [event.node]: { ...prev, status: "running", resumes: (prev.resumes ?? 0) + 1 } } };
    }
    case "DIRECTIVE_ADDED": {
      // Plan 01i: record-only, keyed by id — a replay must not duplicate it.
      const existing = state.directives ?? [];
      if (existing.some((d) => d.id === event.directive.id)) return state;
      return { ...state, directives: [...existing, event.directive] };
    }
    case "DIRECTIVE_WITHDRAWN": {
      return {
        ...state,
        directives: (state.directives ?? []).map((d) => (d.id === event.directiveId ? { ...d, withdrawn: true } : d)),
      };
    }
  }
}

const ACTIVE: NodeStatus[] = ["running", "needs-you", "stopped"];

/** The nodes to start now, in program order: waiting nodes whose
 * dependencies are all DONE, while fewer than `maxParallel` runs are active.
 * A run that needs the owner or was stopped still holds its slot. */
export function nextStarts(nodes: ProgramNode[], state: ProgramState, maxParallel: number): string[] {
  if (state.stopped) return [];
  const active = Object.values(state.nodes).filter((n) => ACTIVE.includes(n.status)).length;
  const room = Math.max(0, maxParallel - active);
  const ready = nodes.filter(
    (n) => state.nodes[n.id].status === "waiting" && n.deps.every((d) => state.nodes[d]?.status === "done"),
  );
  return ready.slice(0, room).map((n) => n.id);
}

export type ProgramOutcome = "running" | "done" | "stuck" | "stopped";

/** `done`: every node is DONE. `stuck`: nothing is running or startable and
 * some node can never start (a dependency is BLOCKED). */
export function programOutcome(nodes: ProgramNode[], state: ProgramState): ProgramOutcome {
  if (nodes.every((n) => state.nodes[n.id].status === "done")) return "done";
  if (state.stopped) return "stopped";
  const anyActive = nodes.some((n) => ACTIVE.includes(state.nodes[n.id].status));
  if (anyActive) return "running";
  const startable = nextStarts(nodes, state, Number.MAX_SAFE_INTEGER).length > 0;
  return startable ? "running" : "stuck";
}

/** The branch a node publishes to. Stack mode: "<TT_BRANCH>--<node>"
 * ("/" in a node id becomes "-"; a ref cannot be both a branch and a
 * directory, so "<TT_BRANCH>/<node>" would clash with TT_BRANCH itself). */
export function nodeBranch(program: ProgramFile, node: ProgramNode): string {
  const entry = program.entries.find((e) => e.id === node.entry)!;
  if ((program.branches ?? "stack") === "shared") return entry.plan.integrationBranch;
  return `${entry.plan.integrationBranch}--${node.id.replace(/\//g, "-")}`;
}

/** The branches a node's branch is cut from: its dependencies' branches, or
 * its plan's TT_BRANCH for a node with none. Several bases are merged. */
export function nodeBases(program: ProgramFile, nodes: ProgramNode[], node: ProgramNode): string[] {
  if (node.deps.length === 0) {
    return [program.entries.find((e) => e.id === node.entry)!.plan.integrationBranch];
  }
  return node.deps.map((d) => nodeBranch(program, nodes.find((n) => n.id === d)!));
}

/** The plan a node's run receives: the entry's plan narrowed to its phase,
 * publishing to the node's branch. Plan 01i: the in-force program-wide
 * directives are seeded into it, so a node started after the owner ruled
 * still carries the ruling in every prompt. */
export function nodePlan(program: ProgramFile, node: ProgramNode, directives: ProgramDirective[] = []): RunPlanFile {
  const entry = program.entries.find((e) => e.id === node.entry)!;
  const phase = entry.plan.phases[node.phaseIndex];
  return {
    ...entry.plan,
    title: `${entry.plan.title} [${node.id}]`,
    integrationBranch: nodeBranch(program, node),
    phases: [phase],
    ...(directives.length > 0
      ? { ownerDirectives: directives.map((d) => ({ id: d.id, text: d.text, at: d.at })) }
      : {}),
  };
}
