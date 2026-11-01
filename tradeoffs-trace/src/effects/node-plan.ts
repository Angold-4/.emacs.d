// Plan 07h2 (A2/R3, C3): the plan a program node starts from.
//
// Run 01 had to restart a program to pick up a fixed plan file, and a node that
// re-used the plan the program was started with ran checks its owner had
// already removed. So the node's own start takes the plan once, here, and the
// policy of what "once" means lives in this effects module rather than in the
// scheduler loop: the scheduler only observes (the program's source, its
// exporter, the runner it would launch) and this module decides.
//
// The rule:
//   - a program with a `--source` file and an exporter is re-exported NOW, so a
//     plan fixed between nodes applies without restarting the program;
//   - the re-exported entry must still be the node's entry, must keep its phase
//     count and must lint — the same lint `tt program start` runs;
//   - an export or lint failure is the node's named `X:PLAN_DEFECT`, never a
//     silent fall back to the older plan;
//   - only without a source the snapshot the program was started with stands,
//     and the program log says so (07h C1). A source without an exporter is
//     a plan defect, never a stale-plan fallback.
//
// What the node's run records about all this is `NodeSnapshot`: the source, the
// exporter, the program's CURRENT runner and a fingerprint of the plan.

import { createHash } from "node:crypto";
import * as fs from "node:fs";

import { hasLintErrors, lintPlan } from "../core/plan-lint.ts";
import type { ProgramFile } from "../core/program.ts";

/** One entry of a program file: its id and the plan the node runs. */
export type NodePlan = ProgramFile["entries"][number]["plan"];

/** Plan 07h2 (A2/R3): what a node's start recorded about the plan it ran —
 * the source it was re-exported from, the exporter, the program's current
 * runner and the sha of the plan snapshot the run was created with. Written
 * into the node's `program.json`, beside the program id. */
export interface NodeSnapshot {
  source: string;
  exporter: "command" | "emacs" | "snapshot";
  runner: string;
  planSha256: string;
  reexported: boolean;
}

export type NodeStartPlan = { ok: true; entryId: string; plan: NodePlan; snapshot: NodeSnapshot } | { ok: false; reason: string };

/** Everything the node's start observed. The scheduler computes these; the
 * decision below is this module's. */
export interface NodeStartInputs {
  /** The node whose run is starting. */
  nodeId: string;
  /** The entry the node was created for. */
  entryId: string;
  /** The plan the program's own snapshot holds for that entry. */
  recorded: NodePlan;
  /** The program's `--source` file, when it has one. */
  source?: string;
  /** Which exporter the start would use; `"snapshot"` means none is available. */
  exporter: "command" | "emacs" | "snapshot";
  /** The runner the program is on now (never the one its nodes started with). */
  runner: string;
  /** Re-export the source now. Throws with the exporter's own message. */
  reexport?: (source: string) => ProgramFile;
  /** The program's log, for the line that says the snapshot stood. */
  logPath: string;
}

/** A stable fingerprint of one plan, recorded with the node's snapshot. */
export function planSha256(plan: unknown): string {
  return createHash("sha256").update(JSON.stringify(plan)).digest("hex");
}

/** Plan 07h2 (A2/R3): the plan the node starts with, and the snapshot its run
 * records — or the named defect that blocks the node before its run exists. */
export function planForNodeStart(inputs: NodeStartInputs): NodeStartPlan {
  const { nodeId, entryId, recorded, source, exporter, runner } = inputs;
  const snapshotOf = (plan: NodePlan, reexported: boolean, sourceText: string, kind: NodeSnapshot["exporter"]): NodeStartPlan => ({
    ok: true,
    entryId,
    plan,
    snapshot: { source: sourceText, exporter: kind, runner, planSha256: planSha256(plan), reexported },
  });
  const kept = (why: string): NodeStartPlan => {
    try {
      fs.appendFileSync(inputs.logPath, `${new Date().toISOString()} node ${nodeId}: starting from the snapshot (${why})\n`);
    } catch {
      // a log line that cannot be written must never stop a node
    }
    return snapshotOf(recorded, false, source ?? "", "snapshot");
  };
  if (source === undefined) return kept("the program has no source file");
  if (exporter === "snapshot" || inputs.reexport === undefined) {
    return {
      ok: false,
      reason: `X:PLAN_DEFECT: ${source} cannot be re-exported for node ${nodeId}: no exporter is configured (set TT_EXPORT_COMMAND, or run on a host with Emacs and the runner's exporter)`,
    };
  }
  let fresh: ProgramFile;
  try {
    fresh = inputs.reexport(source);
  } catch (err) {
    return { ok: false, reason: `X:PLAN_DEFECT: ${source} could not be re-exported for node ${nodeId}: ${String((err as Error)?.message ?? err).split("\n")[0]}` };
  }
  const freshEntry = (fresh.entries ?? []).find((e) => e.id === entryId);
  if (freshEntry === undefined) {
    return { ok: false, reason: `X:PLAN_DEFECT: ${source} no longer has an entry ${entryId} for node ${nodeId}` };
  }
  if ((freshEntry.plan.phases ?? []).length !== (recorded.phases ?? []).length) {
    return {
      ok: false,
      reason: `X:PLAN_DEFECT: ${source}'s entry ${entryId} has ${(freshEntry.plan.phases ?? []).length} phase(s); node ${nodeId} was created for ${(recorded.phases ?? []).length}`,
    };
  }
  // The same lint `tt program start` runs, so a re-exported plan that does not
  // lint blocks the node instead of running.
  const findings = lintPlan(freshEntry.plan);
  if (hasLintErrors(findings)) {
    return {
      ok: false,
      reason: `X:PLAN_DEFECT: ${source}'s entry ${entryId} does not lint: ${findings.find((f) => f.severity === "error")?.message ?? "see tt lint"}`,
    };
  }
  return snapshotOf(freshEntry.plan, true, source, exporter);
}
