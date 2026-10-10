// A4 (plan 06e): a run is always read by the runner that recorded it, when
// that runner is installed, and a finished run's reported outcome never
// changes. `runnerFor(run)` is the ONLY place that chooses a runner — every
// reader (status, program status, the program review, the scheduler) goes
// through it.
//
// The recorded revision is the one `createRun`/`Conductor.start()` wrote into
// the run's `meta.json` (and its `init` record). An installed runner is a
// frozen copy at `<root>/runner/<sha>/tradeoffs-trace` (see `tt runner
// install`). When the recorded revision is not installed, or is the running
// checkout, the current runner reads the run — and a finished (DONE/BLOCKED)
// run's projection is a pure function of its immutable log, so it is the same
// one the recording runner would have produced.

import { spawnSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

import { rebuildState, runPaths, runnerRevision, type RunPlanFile } from "../conductor.ts";
import type { State } from "../core/types.ts";

export interface RunnerChoice {
  /** The revision this runner serves. */
  revision: string;
  /** The package root (holds `src/` and `RUNNER_SHA`). */
  packageRoot: string;
  /** True when this is the running checkout, not an installed frozen copy. */
  current: boolean;
  /** The CLI entry point a delegated read runs. */
  cliPath: string;
}

function currentPackageRoot(): string {
  return fileURLToPath(new URL("../../", import.meta.url));
}

/** The choice that reads a run with this checkout. */
export function currentChoice(): RunnerChoice {
  const packageRoot = currentPackageRoot();
  return { revision: runnerRevision(), packageRoot, current: true, cliPath: path.join(packageRoot, "src", "cli.ts") };
}

/** The revision a run was recorded under: `meta.json`'s `runnerRevision`,
 * else the first `init` record's. Undefined for a fixture that records
 * neither (such a run is read by the current runner). */
export function recordedRunnerRevision(runDir: string): string | undefined {
  try {
    const meta = JSON.parse(fs.readFileSync(runPaths(runDir).meta, "utf8")) as { runnerRevision?: unknown };
    if (typeof meta.runnerRevision === "string" && meta.runnerRevision.length > 0) return meta.runnerRevision;
  } catch {
    // fall through to the log
  }
  try {
    const text = fs.readFileSync(runPaths(runDir).events, "utf8");
    for (const line of text.split("\n")) {
      if (line.length === 0) continue;
      const parsed = JSON.parse(line) as { kind?: string; event?: { runnerRevision?: unknown } };
      if (parsed.kind !== "init") continue;
      if (typeof parsed.event?.runnerRevision === "string" && parsed.event.runnerRevision.length > 0) {
        return parsed.event.runnerRevision;
      }
    }
  } catch {
    // no log yet
  }
  return undefined;
}

/** The frozen copy of REVISION under ROOT, when one is installed. */
export function installedRunnerRoot(root: string, revision: string): string | undefined {
  const dir = path.join(root, "runner", revision, "tradeoffs-trace");
  return fs.existsSync(path.join(dir, "src", "cli.ts")) ? dir : undefined;
}

// A4 (plan 06e): `runnerFor` — the one place that chooses a runner — is
// declared in cli.ts (the architecture's :WHERE:), because the CLI is the
// boundary that owns the choice. program.ts cannot import cli.ts (cli.ts runs
// main() on import), so cli.ts registers its `runnerFor` here and this module
// uses it; when program.ts is used on its own (tests), no chooser is
// registered and `readRunState` reads locally with this checkout.
type RunnerChooser = (runDir: string, root: string) => RunnerChoice;
let registeredChooser: RunnerChooser | undefined;

/** cli.ts registers its `runnerFor` here at load. */
export function setRunnerChooser(chooser: RunnerChooser | undefined): void {
  registeredChooser = chooser;
}

/** A run's plan snapshot and rebuilt state, read locally by this runner. */
function readLocally(runDir: string): { plan: RunPlanFile; state: State } | undefined {
  try {
    const plan = JSON.parse(fs.readFileSync(path.join(runPaths(runDir).plan, "v1.json"), "utf8")) as RunPlanFile;
    return { plan, state: rebuildState(runDir, plan, { lenient: true }) };
  } catch {
    return undefined;
  }
}

/** A4: read a run's plan and state through `runnerFor`. A run recorded under
 * a different, installed runner is read by that runner's own `state`
 * subcommand; anything else (no recorded revision, the current revision, an
 * uninstalled revision, or a frozen copy that cannot serve) is read here. The
 * fallback keeps a broken install from making a run unreadable. */
export function readRunState(runDir: string, root: string): { plan: RunPlanFile; state: State } | undefined {
  const choice = registeredChooser?.(runDir, root);
  if (choice === undefined || choice.current) return readLocally(runDir);
  const result = spawnSync(process.execPath, [choice.cliPath, "state", runDir, "--root", root], {
    encoding: "utf8",
    env: { ...process.env, TT_RUNNER_SERVED: "1" },
    maxBuffer: 64 * 1024 * 1024,
  });
  if (result.status === 0 && result.stdout) {
    try {
      const parsed = JSON.parse(result.stdout) as { plan?: RunPlanFile; state?: State };
      if (parsed.plan && parsed.state) return { plan: parsed.plan, state: parsed.state };
    } catch {
      // a frozen copy that did not print JSON: fall back
    }
  }
  return readLocally(runDir);
}
