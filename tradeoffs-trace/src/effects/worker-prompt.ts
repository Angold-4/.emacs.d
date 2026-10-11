// `#+TT_WORKER_PROMPT`'s one filesystem effect: at a run's first start, copy
// each lane's prompt files into the run and hash them, so the `init` record
// names exactly what every worker of that lane is launched with. See
// core/worker-prompt.ts for the record and why it is copied.

import { createHash } from "node:crypto";
import * as fs from "node:fs";
import * as path from "node:path";

import { expandPromptPath, type WorkerPromptMap, type WorkerPromptRecord } from "../core/worker-prompt.ts";

/** Copy every lane's files into `<destDir>/<lane>-<n>-<basename>` and record
 * each one's source, copy and sha256. Throws, naming the file, when one is
 * missing or unreadable — a run never starts without the prompt its plan
 * asked for (Pi would take an unreadable path as literal prompt text). The
 * file is copied byte for byte: a skill's YAML front matter is kept. */
export function resolveWorkerPrompts(map: WorkerPromptMap | undefined, destDir: string): WorkerPromptRecord | undefined {
  if (!map || Object.keys(map).length === 0) return undefined;
  const out: WorkerPromptRecord = {};
  for (const lane of Object.keys(map).sort()) {
    const files = Array.isArray(map[lane]) ? map[lane] : [];
    out[lane] = files.map((raw, i) => {
      const source = expandPromptPath(String(raw));
      let bytes: Buffer;
      try {
        bytes = fs.readFileSync(source);
      } catch (err) {
        throw new Error(`#+TT_WORKER_PROMPT: lane ${lane}'s file ${source} cannot be read: ${(err as Error).message}`);
      }
      fs.mkdirSync(destDir, { recursive: true });
      const name = path.basename(source);
      const copy = path.join(destDir, `${lane}-${i + 1}-${name}`);
      fs.writeFileSync(copy, bytes);
      return { source, path: copy, name, sha256: createHash("sha256").update(bytes).digest("hex") };
    });
  }
  return out;
}
