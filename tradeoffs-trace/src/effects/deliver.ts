// Plan 06m (A5/A6): `tt deliver` and `tt restack` — the effects that turn
// accepted phases into one commit per layer, push them with ledger leases,
// open or update each stacked PR, and (restack) replay the stack onto a new
// base.
//
// Everything that mutates a remote, opens a PR or links a stack runs only
// with `--apply`. Without it the same planner prints what it WOULD do and
// changes nothing (C1). The ledger (`programs/<id>/delivery.json`) records
// the pushed SHA per layer, so a push pins `--force-with-lease` to a recorded
// SHA and never to a local branch ref, and a rerun is idempotent.

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import * as fs from "node:fs";
import * as path from "node:path";

import { foldProgram } from "../program.ts";
import { readRunState } from "./runner.ts";
import { nodeBases, nodeBranch, type ProgramNode, type ProgramState } from "../core/program.ts";
import type { PhaseState, State } from "../core/types.ts";
import { tradeoffEntries } from "../core/verdict.ts";

export interface CmdResult {
  status: number;
  stdout: string;
  stderr: string;
}

export type Cmd = (file: string, args: string[], opts?: { cwd?: string; input?: string }) => CmdResult;

export const defaultCmd: Cmd = (file, args, opts = {}) => {
  try {
    const stdout = execFileSync(file, args, {
      encoding: "utf8",
      cwd: opts.cwd,
      input: opts.input,
      stdio: [opts.input === undefined ? "ignore" : "pipe", "pipe", "pipe"],
      maxBuffer: 64 * 1024 * 1024,
    });
    return { status: 0, stdout, stderr: "" };
  } catch (err) {
    const e = err as { status?: number; stdout?: string | Buffer; stderr?: string | Buffer };
    return {
      status: e.status ?? 1,
      stdout: e.stdout ? e.stdout.toString() : "",
      stderr: e.stderr ? e.stderr.toString() : String((err as Error)?.message ?? err),
    };
  }
};

export interface DeliveryLedgerEntry {
  node: string;
  branch: string;
  sha: string;
  /** The delivered commit's tree (after any regeneration). */
  tree: string;
  /** Plan 06m (M-17): the accepted candidate's tree, so a rerun after a
   * restack (where `tree` was regenerated) still recognises the layer as
   * delivered and never recommits the pre-restack tree. */
  candidateTree?: string;
  /** Plan 06m (A5/R7, OD-3(1)): the commit this layer's delivered commit
   * sits on — the parent layer's delivered commit, or the base commit for
   * the root layer. A redelivery and a restack resolve a layer's parent from
   * here, never from the current tip of a branch. */
  parent?: string;
  pr?: string;
  /** The PR base this layer's PR was last reconciled to. */
  prBase?: string;
  /** sha256 of the PR body last written, so a changed stack graph or
   * trade-offs table is reconciled and an unchanged one is left alone. */
  prBody?: string;
}

export interface DeliveryLedger {
  remote?: string;
  /** Plan 06m (A5/A6): the root layer's base branch once a restack moved the
   * stack onto another branch; absent while it is the plan's integration
   * branch. */
  base?: string;
  layers: Record<string, DeliveryLedgerEntry>;
}

export function deliveryLedgerPath(programDir: string): string {
  return path.join(programDir, "delivery.json");
}

export function readDeliveryLedger(programDir: string): DeliveryLedger {
  try {
    const raw = JSON.parse(fs.readFileSync(deliveryLedgerPath(programDir), "utf8")) as DeliveryLedger;
    return { remote: raw.remote, ...(raw.base ? { base: raw.base } : {}), layers: raw.layers ?? {} };
  } catch {
    return { layers: {} };
  }
}

function writeDeliveryLedger(programDir: string, ledger: DeliveryLedger): void {
  fs.writeFileSync(deliveryLedgerPath(programDir), `${JSON.stringify(ledger, null, 2)}\n`);
}

/** One accepted layer's delivery plan. */
export interface DeliveryLayer {
  node: string;
  phaseId: string;
  runId: string;
  title: string;
  goal: string;
  branch: string;
  /** The branch of the layer this one is based on (the root's integration
   * branch for the first layer). */
  parentBranch: string;
  /** The node id of the parent layer, when this layer has one. */
  parentNode?: string;
  /** The commit the layer's new commit is made on (the parent's delivered
   * commit, or the base ref's commit). */
  parentCommit: string;
  candidateSha: string;
  /** Plan 06m (A-9): the delivered ledger commit for this layer, when one
   * was recorded. Restack replays this commit (whose tree is the accepted
   * tree on its parent layer), never the bare candidate. */
  deliveredSha?: string;
  tree: string;
  labels: string[];
  /** The PR body: the phase summary, the whole-stack graph and the
   * trade-offs table, and no generated-by footer. */
  body: string;
  /** The previously pushed SHA (the `--force-with-lease` value); `""` when
   * nothing has been pushed for this layer yet. */
  lease: string;
}

export interface DeliveryPlan {
  repo: string;
  remote: string;
  base: string;
  layers: DeliveryLayer[];
}

export interface DeliveryOptions {
  programDir: string;
  runRoot: string;
  apply: boolean;
  remote?: string;
  gh?: string;
  cmd?: Cmd;
  /** Test/integration hook: read a node run's state. Defaults to
   * `readRunState` (the real runner choice). */
  readRun?: (runDir: string, root: string) => { state: State } | undefined;
}

function gitSha(cmd: Cmd, repo: string, ref: string): string | undefined {
  const r = cmd("git", ["-C", repo, "rev-parse", "--verify", `${ref}^{commit}`]);
  return r.status === 0 ? r.stdout.trim() : undefined;
}

function gitTree(cmd: Cmd, repo: string, ref: string): string | undefined {
  const r = cmd("git", ["-C", repo, "rev-parse", `${ref}^{tree}`]);
  return r.status === 0 ? r.stdout.trim() : undefined;
}

/** The whole-stack graph, from the root base down through every layer, as a
 * tree (a layer sits under its parent layer). `current`, when given, marks
 * the layer whose PR body this is. */
export function stackGraphText(
  layers: readonly Pick<DeliveryLayer, "node" | "branch" | "parentNode">[],
  base: string,
  current?: string,
): string {
  const lines = [`base: ${base}`];
  const ids = new Set(layers.map((l) => l.node));
  const walk = (parent: string | undefined, indent: string): void => {
    for (const l of layers) {
      const p = l.parentNode && ids.has(l.parentNode) ? l.parentNode : undefined;
      if (p !== parent) continue;
      lines.push(`${indent}└─ ${l.branch} (${l.node})${l.node === current ? "  ← this PR" : ""}`);
      walk(l.node, `${indent}     `);
    }
  };
  walk(undefined, "  ");
  return lines.join("\n");
}

/** The trade-offs table for one phase's body. Plan 06m (A-34/disc-M-42): it
 * lists every settled decision of the phase — including a unanimously
 * approved delegated one — never only the capped live attention panel. */
export function tradeoffsTableText(phase: PhaseState): string {
  const decisions = phase.decisions.filter(
    (d) => !d.supersededBy && !d.supersededByCorrection && !d.amendment && d.source !== "trigger",
  );
  if (decisions.length === 0) return "No trade-offs recorded.";
  const rows = decisions.map((d) => `| ${d.id} | ${d.class} | ${(d.choice ?? "").replace(/\|/g, "\\|")} |`);
  return ["| id | class | choice |", "| --- | --- | --- |", ...rows].join("\n");
}

/** The PR body: the phase summary, the whole-stack graph and the trade-offs
 * table — never a generated-by footer. */
export function deliveryBody(goal: string, graph: string, tradeoffs: string): string {
  return [
    "## Summary",
    "",
    goal.trim(),
    "",
    "## Stack",
    "",
    "```",
    graph,
    "```",
    "",
    "## Trade-offs",
    "",
    tradeoffs,
    "",
  ].join("\n");
}

/** Plan the delivery of a program's DONE nodes, in dependency order. Pure
 * reads only (git rev-parse/rev-parse ^{tree}); no mutation. */
export function planDelivery(opts: { programDir: string; runRoot: string; cmd?: Cmd; remote?: string; gh?: string; readRun?: (runDir: string, root: string) => { state: State } | undefined }): { ok: true; plan: DeliveryPlan } | { ok: false; reason: string } {
  const cmd = opts.cmd ?? defaultCmd;
  const readRun = opts.readRun ?? readRunState;
  const { program, nodes, state } = foldProgram(opts.programDir);
  const ledger = readDeliveryLedger(opts.programDir);
  // Plan 06m (A5): a shared-branch program has no per-layer branches, so a
  // delivery would push every layer to the base branch itself. Refused.
  if ((program.branches ?? "stack") === "shared") {
    return { ok: false, reason: "the program uses one shared branch; tt deliver delivers stacked layers only" };
  }
  const ordered = deliveryOrder(nodes, state);
  if (ordered.length === 0) return { ok: false, reason: "no accepted (DONE) phase to deliver" };
  const repo = program.entries[0]?.plan.repo;
  if (!repo) return { ok: false, reason: "the program names no repository" };
  const remote = opts.remote ?? ledger.remote ?? "origin";
  // The root base: the branch a restack moved the stack onto, else the plan's
  // integration branch.
  const base = ledger.base ?? program.entries[0]!.plan.integrationBranch;
  const gh = opts.gh ?? "gh";
  const orderedIds = new Set(ordered.map((n) => n.id));
  const layers: DeliveryLayer[] = [];
  const shape = ordered.map((n) => ({ node: n.id, branch: nodeBranch(program, n), ...(n.deps[0] ? { parentNode: n.deps[0] } : {}) }));
  for (const node of ordered) {
    // Plan 06m (disc-M-44): a join has no single parent layer; refuse rather
    // than silently commit and base it on the first dependency only.
    if (node.deps.length > 1) {
      return { ok: false, reason: `node ${node.id} has ${node.deps.length} dependencies; tt deliver supports a linear stack only` };
    }
    const parentDep = node.deps[0];
    // Plan 06m (A5, contract-92): a layer's PR base is its parent LAYER; a
    // parent that is not accepted has no layer to be based on.
    if (parentDep && !orderedIds.has(parentDep)) {
      return { ok: false, reason: `node ${node.id} depends on ${parentDep}, which is not accepted; deliver its parent first` };
    }
    const s = state.nodes[node.id];
    const entry = program.entries.find((e) => e.id === node.entry)!;
    const phase = entry.plan.phases[node.phaseIndex];
    const read = s.runId ? readRun(path.join(opts.runRoot, s.runId), opts.runRoot) : undefined;
    const candidateSha = read?.state.phase.candidate?.sha;
    if (!candidateSha) return { ok: false, reason: `node ${node.id} is DONE but its accepted candidate is not readable` };
    const tree = gitTree(cmd, repo, candidateSha);
    if (!tree) return { ok: false, reason: `node ${node.id}: candidate ${candidateSha} is not in ${repo}` };
    const branch = nodeBranch(program, node);
    // The PR base: the parent layer's branch, or the root base.
    const parentBranch = parentDep ? nodeBases(program, nodes, node)[0] : base;
    const recorded = ledger.layers[node.id];
    // Plan 06m (OD-3(1)): the parent commit comes from the ledger — the
    // parent layer's delivered commit, or (root) the parent this layer was
    // recorded on. Only a never-delivered root reads the base ref.
    const parentCommit = parentDep
      ? ledger.layers[parentDep]?.sha ?? ""
      : recorded?.parent ?? (recorded?.sha ? gitSha(cmd, repo, `${recorded.sha}^`) : undefined) ?? gitSha(cmd, repo, base) ?? "";
    if (parentCommit === "" && !parentDep) return { ok: false, reason: `node ${node.id}: base ${base} cannot be resolved in ${repo}` };
    const graph = stackGraphText(shape, base, node.id);
    const body = deliveryBody(phase.goal, graph, read ? tradeoffsTableText(read.state.phase) : "No open trade-offs.");
    layers.push({
      node: node.id,
      phaseId: phase.id,
      runId: s.runId!,
      title: `${entry.plan.title} [${node.id}]`,
      goal: phase.goal,
      branch,
      parentBranch,
      ...(parentDep ? { parentNode: parentDep } : {}),
      parentCommit,
      candidateSha,
      tree,
      labels: entry.plan.labels ?? [],
      body,
      lease: recorded?.sha ?? "",
      ...(recorded?.sha ? { deliveredSha: recorded.sha } : {}),
    });
  }
  void gh;
  return { ok: true, plan: { repo, remote, base, layers } };
}

/** Dependency order: a node after all its dependencies. Stable for a DAG. */
function deliveryOrder(nodes: readonly ProgramNode[], state: ProgramState): ProgramNode[] {
  const done = nodes.filter((n) => state.nodes[n.id]?.status === "done");
  const doneIds = new Set(done.map((n) => n.id));
  const out: ProgramNode[] = [];
  const seen = new Set<string>();
  const visit = (n: ProgramNode): void => {
    if (seen.has(n.id) || !doneIds.has(n.id)) return;
    seen.add(n.id);
    for (const d of n.deps) {
      const dep = nodes.find((x) => x.id === d);
      if (dep) visit(dep);
    }
    out.push(n);
  };
  for (const n of done) visit(n);
  return out;
}

/** Render the plan as the lines `tt deliver` prints without `--apply`. */
export function renderDeliveryPlan(plan: DeliveryPlan): string {
  const lines = [`deliver ${plan.layers.length} layer(s) to ${plan.remote}, base ${plan.base}`];
  for (const l of plan.layers) {
    lines.push(`  ${l.branch} (${l.node})`);
    const on = l.parentCommit ? l.parentCommit.slice(0, 12) : `${l.parentNode}'s new commit`;
    lines.push(`    commit tree ${l.tree.slice(0, 12)} on ${on} (base ${l.parentBranch})`);
    lines.push(`    push --force-with-lease=${l.branch}:${l.lease.slice(0, 12)}`);
    lines.push(`    gh pr create --base ${l.parentBranch} --head ${l.branch} (body: stack graph + trade-offs)`);
  }
  lines.push(`  gh stack link --base ${plan.base}`);
  return lines.join("\n");
}

export interface DeliveryResult {
  ok: boolean;
  applied: boolean;
  messages: string[];
  ledger: DeliveryLedger;
}

/** Apply (or print) the delivery. Without `apply` nothing mutates. */
export function deliver(opts: DeliveryOptions): DeliveryResult {
  const cmd = opts.cmd ?? defaultCmd;
  const planned = planDelivery({ programDir: opts.programDir, runRoot: opts.runRoot, cmd, remote: opts.remote, gh: opts.gh, readRun: opts.readRun });
  if (!planned.ok) return { ok: false, applied: false, messages: [planned.reason], ledger: readDeliveryLedger(opts.programDir) };
  const { plan } = planned;
  const ledger = readDeliveryLedger(opts.programDir);
  const messages: string[] = [];
  if (!opts.apply) {
    messages.push(renderDeliveryPlan(plan));
    return { ok: true, applied: false, messages, ledger };
  }
  const gh = opts.gh ?? "gh";
  // The SHA each layer was delivered at in THIS run, so a child's parent is
  // the commit just made even though no local branch was created.
  const delivered: Record<string, string> = {};
  for (const layer of plan.layers) {
    const previous = ledger.layers[layer.node];
    // Plan 06m (OD-3(1), contract-92): the parent commit is resolved from the
    // ledger — the parent layer's delivered commit (this run's, else the
    // recorded one), or for the root the parent it was recorded on (a
    // restack moves it) — never the current tip of the base branch, so a
    // retried PR reconciliation can never re-parent a restacked layer.
    const parentCommit = layer.parentNode
      ? delivered[layer.parentNode] ?? ledger.layers[layer.parentNode]?.sha ?? ""
      : layer.parentCommit;
    if (!parentCommit) {
      return { ok: false, applied: false, messages: [...messages, `no parent commit for ${layer.node} (${layer.parentBranch})`], ledger };
    }
    // Idempotent: a layer already delivered with the same accepted tree on the
    // same parent keeps its commit (M-17: compare the CANDIDATE tree; A-35: a
    // re-delivered parent changes the expected parent, so it is recommitted).
    const reuse =
      previous !== undefined &&
      (previous.candidateTree ?? previous.tree) === layer.tree &&
      gitSha(cmd, plan.repo, previous.sha) === previous.sha &&
      gitSha(cmd, plan.repo, `${previous.sha}^`) === parentCommit;
    let sha: string;
    let pushed = false;
    if (reuse) {
      sha = previous!.sha;
    } else {
      const commit = cmd("git", [
        "-C",
        plan.repo,
        "-c",
        "user.name=tradeoffs-trace",
        "-c",
        "user.email=tradeoffs-trace@localhost",
        "commit-tree",
        layer.tree,
        "-p",
        parentCommit,
        "-m",
        `${layer.title}\n\nTT-Deliver: ${layer.node}`,
      ]);
      if (commit.status !== 0) {
        return { ok: false, applied: false, messages: [...messages, `commit-tree failed for ${layer.node}: ${commit.stderr.trim()}`], ledger };
      }
      sha = commit.stdout.trim();
      // The lease is the SHA the ledger recorded for this layer (empty: the
      // branch must not exist yet), never a local branch ref.
      const push = cmd("git", [
        "-C",
        plan.repo,
        "push",
        `--force-with-lease=${layer.branch}:${previous?.sha ?? ""}`,
        plan.remote,
        `${sha}:refs/heads/${layer.branch}`,
      ]);
      if (push.status !== 0) {
        return { ok: false, applied: false, messages: [...messages, `push failed for ${layer.branch}: ${push.stderr.trim()}`], ledger };
      }
      pushed = true;
      // Persist the pushed SHA before the PR step, so a PR failure leaves the
      // ledger able to supply the correct lease on a rerun (A-7).
      const deliveredTree = gitTree(cmd, plan.repo, sha) ?? layer.tree;
      ledger.layers[layer.node] = { node: layer.node, branch: layer.branch, sha, tree: deliveredTree, candidateTree: layer.tree, parent: parentCommit };
      ledger.remote = plan.remote;
      writeDeliveryLedger(opts.programDir, ledger);
    }
    delivered[layer.node] = sha;
    // Open or update the PR: base is ALWAYS the parent layer (A-8/M-14), or
    // the root base for the first layer; the body has the whole-stack graph
    // and the trade-offs table, never a generated-by footer. A PR whose base
    // or body changed since it was last written (a new layer joined the
    // stack, a restack moved the root base) is reconciled; an unchanged one
    // is left alone. `gh` runs in the repo, so it acts on this repository
    // (M-13).
    const entry = ledger.layers[layer.node]!;
    const bodyHash = sha256(layer.body);
    if (entry.pr && entry.prBase === layer.parentBranch && entry.prBody === bodyHash) {
      messages.push(`${layer.branch}: ${pushed ? `${sha.slice(0, 12)} pushed` : `already delivered ${sha.slice(0, 12)}`}`);
      continue;
    }
    const view = cmd(gh, ["pr", "view", layer.branch, "--json", "number"], { cwd: plan.repo });
    const exists = view.status === 0;
    const args = exists
      ? ["pr", "edit", layer.branch, "--base", layer.parentBranch, "--title", layer.title, "--body-file", "-"]
      : ["pr", "create", "--base", layer.parentBranch, "--head", layer.branch, "--title", layer.title, "--body-file", "-"];
    for (const label of layer.labels) args.push(exists ? "--add-label" : "--label", label);
    const pr = cmd(gh, args, { input: layer.body, cwd: plan.repo });
    if (pr.status !== 0) {
      return { ok: false, applied: false, messages: [...messages, `gh pr ${exists ? "edit" : "create"} failed for ${layer.branch}: ${pr.stderr.trim()}`], ledger };
    }
    ledger.layers[layer.node] = { ...entry, pr: layer.branch, prBase: layer.parentBranch, prBody: bodyHash };
    writeDeliveryLedger(opts.programDir, ledger);
    messages.push(`${layer.branch}: ${pushed ? `${sha.slice(0, 12)} pushed` : `${sha.slice(0, 12)} kept`}, PR ${exists ? "updated" : "opened"}`);
  }
  const link = cmd(gh, ["stack", "link", "--base", plan.base], { cwd: plan.repo });
  if (link.status !== 0) {
    // Plan 06m (A-33/disc-M-43): an unlinked stack is an incomplete delivery,
    // not a success.
    return { ok: false, applied: true, messages: [...messages, `gh stack link failed: ${link.stderr.trim()}`], ledger };
  }
  messages.push(`gh stack link --base ${plan.base}`);
  return { ok: true, applied: true, messages, ledger };
}

function sha256(text: string): string {
  return createHash("sha256").update(text).digest("hex");
}

// ---------------------------------------------------------------------------
// restack
// ---------------------------------------------------------------------------

export interface RestackLayerResult {
  node: string;
  branch: string;
  oldSha: string;
  newSha: string;
  regenerated: string[];
}

export interface RestackResult {
  ok: boolean;
  applied: boolean;
  messages: string[];
  layers: RestackLayerResult[];
}

/** `tt restack <program> --onto <ref> [--apply]`: replay every delivered layer
 * onto the new base. A conflict whose every path the plan declares generated
 * takes the base side for those hunks only (git's `-X ours`), then the plan's
 * regenerate commands run and the layer is amended, then its check runs. Any
 * other conflict, or a failing check, stops with the layer and paths named. */
export function restack(opts: {
  programDir: string;
  runRoot: string;
  onto: string;
  apply: boolean;
  remote?: string;
  cmd?: Cmd;
  readRun?: (runDir: string, root: string) => { state: State } | undefined;
}): RestackResult {
  const cmd = opts.cmd ?? defaultCmd;
  const planned = planDelivery({ programDir: opts.programDir, runRoot: opts.runRoot, cmd, remote: opts.remote, readRun: opts.readRun });
  if (!planned.ok) return { ok: false, applied: false, messages: [planned.reason], layers: [] };
  const { plan } = planned;
  const ledger = readDeliveryLedger(opts.programDir);
  const messages: string[] = [];
  const results: RestackLayerResult[] = [];
  // Plan 06m (A6/OD-3(1)): restack moves a DELIVERED stack. Every layer's
  // old parent comes from the ledger, so a layer that was never delivered has
  // nothing to replay.
  const undelivered = plan.layers.filter((l) => !ledger.layers[l.node]?.sha);
  if (undelivered.length > 0) {
    return {
      ok: false,
      applied: false,
      messages: [`restack needs a delivered stack: ${undelivered.map((l) => `${l.branch} (${l.node})`).join(", ")} not delivered; run tt deliver --apply first`],
      layers: [],
    };
  }
  if (!opts.apply) {
    messages.push(`restack ${plan.layers.length} layer(s) onto ${opts.onto}`);
    for (const l of plan.layers) messages.push(`  ${l.branch} (${l.node})`);
    return { ok: true, applied: false, messages, layers: [] };
  }
  const { program, nodes } = foldProgram(opts.programDir);
  const phaseOf = (nodeId: string) => {
    const node = nodes.find((n) => n.id === nodeId);
    if (!node) return undefined;
    return program.entries.find((e) => e.id === node.entry)?.plan.phases[node.phaseIndex];
  };
  const generatedFor = (nodeId: string): string[] => phaseOf(nodeId)?.generated ?? [];
  const regenFor = (nodeId: string): string[] => phaseOf(nodeId)?.regen ?? [];
  // Plan 06m (B-21): run EVERY configured check of the layer, not only the
  // first.
  const checksFor = (nodeId: string): string[] => phaseOf(nodeId)?.checks ?? [];
  const onto = gitSha(cmd, plan.repo, opts.onto);
  if (!onto) return { ok: false, applied: false, messages: [`--onto ${opts.onto} does not resolve in ${plan.repo}`], layers: [] };
  // Each layer is replayed onto its parent layer's NEW commit; the root onto
  // the new base. Nothing is pushed until every layer replayed, regenerated
  // and passed its check, so a stop leaves the remote stack untouched.
  const newSha: Record<string, string> = {};
  const replayed: Array<{ layer: DeliveryLayer; sha: string; parent: string; regenerated: string[] }> = [];
  for (const layer of plan.layers) {
    const recorded = ledger.layers[layer.node]!;
    const parentCommit = layer.parentNode ? newSha[layer.parentNode] : onto;
    if (!parentCommit) {
      return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch} (${layer.node}): its parent layer ${layer.parentNode} was not restacked`], layers: results };
    }
    const generated = generatedFor(layer.node);
    // Plan 06m (A-9/OD-3(1)): replay the DELIVERED ledger commit, whose
    // merge base is the parent the ledger recorded it on — the parent
    // layer's old delivered commit, or for the root the OLD base, never the
    // new one — so a clean replay is never reported as a conflict.
    const layerCommit = recorded.sha;
    const mergeBase = recorded.parent ?? gitSha(cmd, plan.repo, `${layerCommit}^`);
    if (!mergeBase) {
      return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch} (${layer.node}): its delivered parent is not recorded`], layers: results };
    }
    // Detect conflicts and their paths without mutating anything.
    const dry = cmd("git", ["-C", plan.repo, "merge-tree", "--write-tree", "--name-only", "--merge-base", mergeBase, parentCommit, layerCommit]);
    if (dry.status !== 0) {
      // `--name-only` prints the tree oid, then one path per line, then a
      // blank line and the human messages. Take only the path lines.
      const conflictPaths: string[] = [];
      for (const line of dry.stdout.split("\n").slice(1)) {
        const trimmed = line.trim();
        if (trimmed === "") break;
        conflictPaths.push(trimmed);
      }
      const offenders = conflictPaths.filter((p) => !matchesGenerated(p, generated));
      if (offenders.length > 0 || conflictPaths.length === 0) {
        return {
          ok: false,
          applied: false,
          messages: [...messages, `restack stopped at ${layer.branch} (${layer.node}): conflict in ${offenders.length > 0 ? offenders.join(", ") : "(unnamed paths)"}`],
          layers: results,
        };
      }
    }
    // Resolve: take the base side on conflicting hunks only, keep every
    // non-conflicting change (git's -X ours), and build the new commit.
    const merged = cmd("git", ["-C", plan.repo, "merge-tree", "--write-tree", "--merge-base", mergeBase, "-X", "ours", parentCommit, layerCommit]);
    if (merged.status !== 0) {
      return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch}: merge failed: ${merged.stderr.trim()}`], layers: results };
    }
    const tree = merged.stdout.split("\n")[0].trim();
    let sha = commitTree(cmd, plan.repo, tree, parentCommit, `${layer.title}\n\nTT-Restack: ${layer.node}`);
    if (!sha) return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch}: commit-tree failed`], layers: results };
    // Regenerate declared paths per layer, then amend. A `git worktree` of the
    // real repo shares its object store, so the amended commit is directly
    // usable; a clone would not have its objects here.
    const regen = regenFor(layer.node);
    const regenerated: string[] = [];
    if (regen.length > 0) {
      const wt = fs.mkdtempSync(path.join("/tmp", "tt-restack-"));
      try {
        const added = cmd("git", ["-C", plan.repo, "worktree", "add", "-q", "--detach", wt, sha]);
        if (added.status !== 0) {
          return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch}: worktree add failed: ${added.stderr.trim()}`], layers: results };
        }
        for (const command of regen) {
          const r = cmd("/bin/sh", ["-c", command], { cwd: wt });
          if (r.status !== 0) {
            return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch}: regen '${command}' failed: ${r.stderr.trim()}`], layers: results };
          }
          regenerated.push(command);
        }
        cmd("git", ["-C", wt, "add", "-A"]);
        const amend = cmd("git", ["-C", wt, "-c", "user.name=tradeoffs-trace", "-c", "user.email=tradeoffs-trace@localhost", "commit", "--amend", "--no-edit", "--allow-empty"]);
        if (amend.status !== 0) {
          return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch}: amend failed: ${amend.stderr.trim()}`], layers: results };
        }
        const head = cmd("git", ["-C", wt, "rev-parse", "HEAD"]);
        if (head.status === 0) sha = head.stdout.trim();
      } finally {
        cmd("git", ["-C", plan.repo, "worktree", "remove", "--force", wt]);
        fs.rmSync(wt, { recursive: true, force: true });
      }
    }
    // Every configured check runs on the final commit; a worktree that
    // cannot be created STOPS (A-10), never silently skips the check.
    const checks = checksFor(layer.node);
    if (checks.length > 0) {
      const wt = fs.mkdtempSync(path.join("/tmp", "tt-restack-check-"));
      try {
        const added = cmd("git", ["-C", plan.repo, "worktree", "add", "-q", "--detach", wt, sha]);
        if (added.status !== 0) {
          return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch}: worktree add for the check failed: ${added.stderr.trim()}`], layers: results };
        }
        for (const check of checks) {
          const r = cmd("/bin/sh", ["-c", check], { cwd: wt });
          if (r.status !== 0) {
            return { ok: false, applied: false, messages: [...messages, `restack stopped at ${layer.branch} (${layer.node}): layer check failed: ${check}`], layers: results };
          }
        }
      } finally {
        cmd("git", ["-C", plan.repo, "worktree", "remove", "--force", wt]);
        fs.rmSync(wt, { recursive: true, force: true });
      }
    }
    newSha[layer.node] = sha;
    replayed.push({ layer, sha, parent: parentCommit, regenerated });
  }
  // Push every layer with the lease the ledger recorded for it (never a local
  // branch ref), recording each pushed SHA and its new parent at once so a
  // push failure part-way leaves the ledger naming what the remote holds.
  for (const { layer, sha, parent, regenerated } of replayed) {
    const recorded = ledger.layers[layer.node]!;
    const push = cmd("git", ["-C", plan.repo, "push", `--force-with-lease=${layer.branch}:${recorded.sha}`, plan.remote, `${sha}:refs/heads/${layer.branch}`]);
    if (push.status !== 0) {
      return { ok: false, applied: results.length > 0, messages: [...messages, `restack push failed for ${layer.branch}: ${push.stderr.trim()}`], layers: results };
    }
    // Plan 06m (M-17): record the AMENDED tree, so a later `tt deliver` does
    // not recommit the pre-restack accepted tree over the new base. The PR
    // reconciliation fields are kept (disc-M-42/M).
    const deliveredTree = gitTree(cmd, plan.repo, sha) ?? recorded.tree;
    ledger.layers[layer.node] = { ...recorded, node: layer.node, branch: layer.branch, sha, tree: deliveredTree, candidateTree: layer.tree, parent };
    writeDeliveryLedger(opts.programDir, ledger);
    messages.push(`${layer.branch}: restacked onto ${parent.slice(0, 12)} → ${sha.slice(0, 12)}${regenerated.length > 0 ? ` (regenerated ${regenerated.length})` : ""}`);
    results.push({ node: layer.node, branch: layer.branch, oldSha: recorded.sha, newSha: sha, regenerated });
  }
  // Plan 06m (A5/A6): `--onto` naming a branch other than the current root
  // base moves the stack onto it; the root PR's base follows on the next
  // `tt deliver --apply` (it reconciles every PR whose base changed).
  const ontoBranch = branchNameOf(cmd, plan.repo, plan.remote, opts.onto);
  if (ontoBranch && ontoBranch !== plan.base) {
    ledger.base = ontoBranch;
    writeDeliveryLedger(opts.programDir, ledger);
    messages.push(`root base is now ${ontoBranch}; tt deliver --apply moves the root PR onto it`);
  }
  return { ok: true, applied: true, messages, layers: results };
}

/** The branch name `ref` names — a local branch, or `<remote>/<branch>` —
 * or undefined for a commit, a tag or anything else. */
function branchNameOf(cmd: Cmd, repo: string, remote: string, ref: string): string | undefined {
  if (cmd("git", ["-C", repo, "show-ref", "--verify", "--quiet", `refs/heads/${ref}`]).status === 0) return ref;
  if (ref.startsWith(`${remote}/`) && cmd("git", ["-C", repo, "show-ref", "--verify", "--quiet", `refs/remotes/${ref}`]).status === 0) {
    return ref.slice(remote.length + 1);
  }
  return undefined;
}

function commitTree(cmd: Cmd, repo: string, tree: string, parent: string, message: string): string | undefined {
  const r = cmd("git", ["-C", repo, "-c", "user.name=tradeoffs-trace", "-c", "user.email=tradeoffs-trace@localhost", "commit-tree", tree, "-p", parent, "-m", message]);
  return r.status === 0 ? r.stdout.trim() : undefined;
}

/** True when PATH is covered by one of the plan's `#+TT_GENERATED:` globs.
 * A glob matches the whole path or any path segment prefix (`dist/**`,
 * `*.gen.ts`, `src/generated`). */
export function matchesGenerated(path: string, globs: readonly string[]): boolean {
  for (const glob of globs) {
    const g = glob.trim();
    if (g.length === 0) continue;
    if (globToRegExp(g).test(path)) return true;
  }
  return false;
}

function globToRegExp(glob: string): RegExp {
  // `**` matches any run (including `/`); `*` matches within a segment; a
  // bare directory name matches the directory and everything under it.
  const escaped = glob
    .replace(/[.+^${}()|[\]\\]/g, "\\$&")
    .replace(/\*\*/g, "\u0000")
    .replace(/\*/g, "[^/]*")
    .replace(/\u0000/g, ".*");
  return new RegExp(`^(?:${escaped})(?:/.*)?$`);
}

