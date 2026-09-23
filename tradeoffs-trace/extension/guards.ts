// Worker guards (design §9.5) — "workflow guards, not a security boundary":
// a same-account process can still get around a string-level path or
// command check. Deliberately NO import of typebox or any Pi package (same
// reasoning as ./param-shapes.ts): this lets a plain `node --test` process
// import and unit-test these functions directly, without going through
// Pi's jiti-based extension loader — see test/effects/guards.test.ts.
//
// extension/tradeoffs-trace.ts wires these into the `tool_call` hook.

import { resolve as resolvePath, sep as pathSep } from "node:path";

export interface GuardConfig {
  /** The worker's live worktree (`TT_WORKTREE`). Writes outside it are
   * blocked. */
  worktree?: string;
  /** The run directory (`TT_RUN_DIR`). Writes under it, and `sh` commands
   * that mention it, are blocked. */
  runDir?: string;
  /** Paths (relative to `worktree`, or absolute) `edit`/`write` may never
   * touch — the phase's own acceptance files (`TT_PROTECTED`, comma
   * separated). */
  protectedPaths?: string[];
}

function isUnder(candidate: string, root: string): boolean {
  const c = resolvePath(candidate);
  const r = resolvePath(root);
  return c === r || c.startsWith(r.endsWith(pathSep) ? r : r + pathSep);
}

/** True iff writing to `targetPath` (an `edit`/`write` tool's `path`
 * argument, resolved against `cwd` if relative) must be blocked: outside
 * the worktree, under the run directory, or one of the phase's protected
 * acceptance files. Returns a human-readable reason, or `undefined` if the
 * write is allowed. */
export function guardedWritePath(targetPath: string, cwd: string, config: GuardConfig): string | undefined {
  const abs = resolvePath(cwd, targetPath);
  if (config.worktree && !isUnder(abs, config.worktree)) {
    return `edit/write outside the worktree (${config.worktree}) is blocked: ${targetPath}`;
  }
  if (config.runDir && isUnder(abs, config.runDir)) {
    return `edit/write under the run directory (${config.runDir}) is blocked: ${targetPath}`;
  }
  for (const p of config.protectedPaths ?? []) {
    const protectedAbs = resolvePath(config.worktree ?? cwd, p);
    if (abs === protectedAbs) {
      return `edit/write to the phase's protected acceptance file is blocked: ${targetPath}`;
    }
  }
  return undefined;
}

const BLOCKED_SH_PATTERNS: RegExp[] = [/\bgit\s+commit\b/, /\bgit\s+push\b/];

/** True iff `command` (the `sh` tool's `command` argument) must be blocked:
 * `git commit`/`git push`, or any mention of the run directory. This is a
 * string-level check only — see the module comment above. Returns a
 * human-readable reason, or `undefined` if the command is allowed. */
export function guardedShCommand(command: string, config: GuardConfig): string | undefined {
  for (const pattern of BLOCKED_SH_PATTERNS) {
    if (pattern.test(command)) {
      return `sh command containing '${pattern.source}' is blocked (workflow guard, not a security boundary): ${command}`;
    }
  }
  if (config.runDir && command.includes(config.runDir)) {
    return `sh command touching the run directory (${config.runDir}) is blocked: ${command}`;
  }
  return undefined;
}

function readEnv(name: string): string | undefined {
  const v = process.env[name];
  return v && v.length > 0 ? v : undefined;
}

export function readGuardConfigFromEnv(): GuardConfig {
  return {
    worktree: readEnv("TT_WORKTREE"),
    runDir: readEnv("TT_RUN_DIR"),
    protectedPaths: (readEnv("TT_PROTECTED") ?? "")
      .split(",")
      .map((s) => s.trim())
      .filter((s) => s.length > 0),
  };
}
