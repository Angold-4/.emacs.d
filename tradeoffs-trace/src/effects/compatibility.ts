import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { compatibilityTests, inventoryCargoArgs, type CompatibilityTest } from "../core/compatibility.ts";
import { runCommand } from "./shell.ts";

export interface CompatibilityRecord {
  candidateSha: string;
  at: string;
  complete: boolean;
  commands: Array<{ command: string; exitCode: number | null; timedOut: boolean; output: string; tests: CompatibilityTest[] }>;
}

/** One bounded inventory, with no fail-fast between declared commands or
 * inside cargo test/nextest (even when invoked by a Python/shell wrapper).
 * No baseline forgiveness: stale tests are repair work too. */
export async function runCompatibilityInventory(opts: {
  candidateSha: string; commands: readonly string[]; cwd: string; env: NodeJS.ProcessEnv;
  deadlineMs: number; termGraceMs: number; rerun?: string;
  stopped: () => boolean;
  onIntent: (info: { pgid: number }) => void;
  onExit: (pgid: number) => void;
}): Promise<CompatibilityRecord> {
  const adapter = fs.mkdtempSync(path.join(os.tmpdir(), "tt-inventory-"));
  // Resolve cargo from the pinned check PATH, without reading credentials.
  const cargo = (opts.env.PATH ?? "").split(path.delimiter).map((dir) => path.join(dir, "cargo"))
    .find((file) => { try { fs.accessSync(file, fs.constants.X_OK); return true; } catch { return false; } });
  if (cargo) fs.writeFileSync(path.join(adapter, "cargo"), `#!${process.execPath}\n` +
    `const { spawnSync } = require('node:child_process');\n` +
    `const inventoryCargoArgs = ${inventoryCargoArgs.toString()};\n` +
    `const result = spawnSync(${JSON.stringify(cargo)}, inventoryCargoArgs(process.argv.slice(2)), { stdio: 'inherit' });\n` +
    `if (result.error) console.error(result.error.message);\nprocess.exit(result.status ?? 1);\n`, { mode: 0o755 });
  const record: CompatibilityRecord = { candidateSha: opts.candidateSha, at: new Date().toISOString(), complete: false, commands: [] };
  const deadlineAt = Date.now() + opts.deadlineMs;
  try {
    for (const command of opts.commands) {
      if (opts.stopped() || Date.now() >= deadlineAt) return record;
      let pgid: number | undefined;
      const result = await runCommand({ command, cwd: opts.cwd,
        env: { ...opts.env, PATH: `${adapter}${path.delimiter}${opts.env.PATH ?? ""}` },
        deadlineMs: Math.max(1, deadlineAt - Date.now()), termGraceMs: opts.termGraceMs,
        onIntent: (info) => { pgid = info.pgid; opts.onIntent(info); },
      }).result;
      if (pgid !== undefined) opts.onExit(pgid);
      record.commands.push({ command, exitCode: result.exitCode, timedOut: result.timedOut, output: result.output,
        tests: compatibilityTests(result.output, command, opts.rerun).map((t) => ({ ...t,
          command: t.command.replaceAll(`${opts.cwd}/`, ""),
          ...(t.file ? { file: t.file.replace(`${opts.cwd}/`, "") } : {}),
        })) });
    }
    record.complete = !opts.stopped() && record.commands.every((c) => c.exitCode !== null && !c.timedOut);
    return record;
  } finally {
    fs.rmSync(adapter, { recursive: true, force: true });
  }
}
