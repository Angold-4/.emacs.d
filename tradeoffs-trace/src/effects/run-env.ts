// exex 01a2: the environment a relaunched conductor runs with.

import { existsSync, readFileSync } from "node:fs";
import path from "node:path";

/** exex 01a2: a conductor relaunched from a shell without the run's PATH (a
 * bare ssh session) could not find `pi` and crashed at its next agent launch
 * (`spawn pi ENOENT`). The PATH the run recorded at start (check-env.json)
 * leads, ahead of whatever the relaunching shell had. */
export function adoptRecordedPath(runDir: string, env: NodeJS.ProcessEnv = process.env): void {
  const file = path.join(runDir, "check-env.json");
  if (!existsSync(file)) return;
  try {
    const recorded = (JSON.parse(readFileSync(file, "utf8")) as Record<string, unknown>).PATH;
    if (typeof recorded !== "string" || recorded.length === 0) return;
    env.PATH = env.PATH && env.PATH.length > 0 ? `${recorded}:${env.PATH}` : recorded;
  } catch {
    // An unreadable record leaves the environment as it is.
  }
}
