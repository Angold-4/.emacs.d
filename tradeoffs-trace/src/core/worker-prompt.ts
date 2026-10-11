// A per-lane worker system prompt (`#+TT_WORKER_PROMPT: a=<file>[,<file>…]
// [b=…]`): an A/B test of worker dispositions across the lanes of a round.
//
// The plan names, per lane, the files appended to that lane's worker system
// prompt (Pi's `--append-system-prompt`, once per file, in order). Only a
// worker ever receives them — reviewers, pickers, the evaluator, the panel and
// the curator never do. A single-candidate run (no `#+TT_WORKERS`, or 1) uses
// lane `a`'s entry.
//
// The conductor copies each file into the run at its first start and records
// the copy, its source and its sha256 in the `init` record, so the prompt a
// run used is reproducible (a later edit of the source never reaches a resumed
// run) and visible (`tt status`'s details block, `tt state`). This module is
// pure: the record's shape, the launch flags and the one-line text.

import * as os from "node:os";
import * as path from "node:path";

/** The plan's map, as Emacs (or `tt lint`'s Org reader) wrote it: lane id
 * (`a`, `b`, …) to the absolute paths appended to that lane's worker, in
 * order. An empty list is `none` (a phase clearing the program's lane). */
export type WorkerPromptMap = Record<string, string[]>;

/** One file a run appended to a lane's worker, as the `init` record keeps it. */
export interface WorkerPromptFile {
  /** The file the plan named (absolute). */
  source: string;
  /** The run's own copy (absolute) — the path the worker is launched with. */
  path: string;
  /** The source's basename, for the views. */
  name: string;
  sha256: string;
}

/** Per lane, the files the run appends, in order. */
export type WorkerPromptRecord = Record<string, WorkerPromptFile[]>;

/** `~` and `~/…` expanded against the home directory; anything else resolved
 * to an absolute path. Emacs already expands; a hand-written plan.json may
 * not. */
export function expandPromptPath(p: string): string {
  if (p === "~") return os.homedir();
  if (p.startsWith("~/")) return path.join(os.homedir(), p.slice(2));
  return path.resolve(p);
}

/** The lane a single-candidate worker takes its prompt from. */
export const SINGLE_WORKER_LANE = "a";

/** A lane id is one lowercase letter (`laneIds` names lanes a, b, c, …). */
export function isLaneId(lane: string): boolean {
  return /^[a-z]$/.test(lane);
}

/** The `init` record's `workerPrompts`, or undefined when absent/malformed
 * (an old run, or a plan without the keyword). */
export function parseWorkerPromptRecord(raw: unknown): WorkerPromptRecord | undefined {
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) return undefined;
  const out: WorkerPromptRecord = {};
  for (const [lane, files] of Object.entries(raw as Record<string, unknown>)) {
    if (!Array.isArray(files)) continue;
    const kept = files.filter(
      (f): f is WorkerPromptFile =>
        !!f && typeof f === "object" && typeof (f as WorkerPromptFile).path === "string" && typeof (f as WorkerPromptFile).sha256 === "string",
    );
    out[lane] = kept;
  }
  return Object.keys(out).length > 0 ? out : undefined;
}

/** The files a worker of LANE is launched with (lane `a` for a single
 * worker); empty when the lane has none. */
export function workerPromptPaths(record: WorkerPromptRecord | undefined, lane: string): string[] {
  return (record?.[lane] ?? []).map((f) => f.path);
}

/** The details row's value: `a=SKILL.md@1a2b3c4d,shim.md@9f8e7d6c  b=none`,
 * or undefined when the run appended nothing to any worker. */
export function workerPromptLineText(record: WorkerPromptRecord | undefined): string | undefined {
  if (!record) return undefined;
  const lanes = Object.keys(record).sort();
  if (!lanes.some((l) => record[l].length > 0)) return undefined;
  return lanes
    .map((lane) => {
      const files = record[lane];
      return `${lane}=${files.length === 0 ? "none" : files.map((f) => `${f.name}@${f.sha256.slice(0, 8)}`).join(",")}`;
    })
    .join("  ");
}
