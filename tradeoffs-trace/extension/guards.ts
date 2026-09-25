// Worker guards (design §9.5) — "workflow guards, not a security boundary":
// a same-account process can still get around a string-level path or
// command check. Deliberately NO import of typebox or any Pi package (same
// reasoning as ./param-shapes.ts): this lets a plain `node --test` process
// import and unit-test these functions directly, without going through
// Pi's jiti-based extension loader — see test/effects/guards.test.ts.
//
// extension/tradeoffs-trace.ts wires these into the `tool_call` hook.

import * as fs from "node:fs";
import { resolve as resolvePath, dirname, basename, join as joinPath, sep as pathSep } from "node:path";

import { maskableSecrets } from "../src/effects/secrets.ts";

export interface GuardConfig {
  /** The worker's live worktree (`TT_WORKTREE`). Writes outside it are
   * blocked. */
  worktree?: string;
  /** The run directory (`TT_RUN_DIR`). Paths under it that are *not* under
   * the worktree are run metadata and are blocked; `sh` commands that
   * mention it are blocked. (The real layout nests the worktree inside the
   * run directory, so metadata protection is "under runDir but not under
   * worktree" — see `guardedWritePath`.) */
  runDir?: string;
  /** Paths (relative to `worktree`, or absolute) `edit`/`write` may never
   * touch — the phase's own acceptance files (`TT_PROTECTED`, comma
   * separated). */
  protectedPaths?: string[];
  /** The plan's declared secret names (`TT_SECRETS`, space separated). Their
   * values are in this process's own environment; a command containing one
   * is refused (see `secretUseInCommand`). */
  secretNames?: string[];
}

/** Canonicalizes `pathname` the way the filesystem sees it, tolerating a
 * path whose final components do not exist yet (a new file's parent may not
 * exist). The nearest existing ancestor is resolved with `realpath` and the
 * nonexistent suffix appended. A symlink — including a dangling one — is
 * resolved to its target, so an escape to an external target is visible.
 *
 * Why this matters: on macOS `/tmp` is a symlink to `/private/tmp`, so a
 * lexical `resolve` makes an allowed path and its canonical spelling compare
 * unequal. Both the target and the configured roots go through here, so
 * canonical aliases of an allowed path are allowed. Bounded to avoid a
 * symlink cycle. (Still just accidental-write prevention, not a security
 * boundary — see the module comment.) */
function canonicalize(pathname: string, depth = 0): string {
  const abs = resolvePath(pathname);
  if (depth > 64) return abs;
  try {
    return fs.realpathSync(abs);
  } catch {
    // Absent, or a dangling symlink.
  }
  try {
    if (fs.lstatSync(abs).isSymbolicLink()) {
      const linkTarget = fs.readlinkSync(abs);
      return canonicalize(resolvePath(dirname(abs), linkTarget), depth + 1);
    }
  } catch {
    // Nothing at all at `abs`.
  }
  const parent = dirname(abs);
  if (parent === abs) return abs; // filesystem root
  return joinPath(canonicalize(parent, depth + 1), basename(abs));
}

function isUnder(candidate: string, root: string): boolean {
  return candidate === root || candidate.startsWith(root.endsWith(pathSep) ? root : root + pathSep);
}

/** True iff writing to `targetPath` (an `edit`/`write` tool's `path`
 * argument, resolved against `cwd` if relative) must be blocked: outside
 * the worktree, run-directory metadata (under the run directory but not
 * under the worktree — the real layout nests `worktree` *inside* `runDir`),
 * or one of the phase's protected acceptance files. Returns a
 * human-readable reason, or `undefined` if the write is allowed.
 *
 * The target and the configured roots are canonicalized through their
 * nearest existing ancestor (see `canonicalize`), so a new file is allowed
 * inside the worktree, canonical aliases resolve consistently, and a
 * symlink that escapes the worktree is denied. This is accidental-write
 * prevention, not a security boundary. */
export function guardedWritePath(targetPath: string, cwd: string, config: GuardConfig): string | undefined {
  const requested = resolvePath(cwd, targetPath);
  const target = canonicalize(requested);
  const worktree = config.worktree ? canonicalize(config.worktree) : undefined;
  const runDir = config.runDir ? canonicalize(config.runDir) : undefined;

  // The worker's worktree lives inside the run directory (<run>/worktree), so
  // the run-directory rule exempts it: otherwise every edit/write in the
  // worktree is refused and workers fall back to sh heredocs (observed in
  // dogfood run 4ec5e0f8 and live-single-phase).
  const inWorktree = worktree !== undefined && isUnder(target, worktree);

  // Run metadata: under the run directory but *not* under the worktree.
  // Checked before the general worktree rule so the reason names run
  // metadata rather than a plain outside-the-worktree rejection.
  if (runDir !== undefined && isUnder(target, runDir) && !inWorktree) {
    return `edit/write under the run directory (${config.runDir}) is blocked: ${targetPath}`;
  }
  if (worktree !== undefined && !inWorktree) {
    return `edit/write outside the worktree (${config.worktree}) is blocked: ${targetPath}`;
  }
  for (const p of config.protectedPaths ?? []) {
    const protectedAbs = canonicalize(resolvePath(config.worktree ?? cwd, p));
    if (target === protectedAbs) {
      return `edit/write to the phase's protected acceptance file is blocked: ${targetPath}`;
    }
  }
  return undefined;
}

const BLOCKED_SH_PATTERNS: RegExp[] = [/\bgit\s+commit\b/, /\bgit\s+push\b/];

/** Plan 2c: the conductor runs the phase checks itself on a fresh checkout
 * after submit_phase and enforces every command's deadline. Backgrounded
 * work escapes that deadline, and long sleeps are almost always polling of
 * such background work — in dogfood run 4ec5e0f8 nohup'd suite runs plus
 * sleep/tail polling took 40–60% of each worker attempt. */
export const MAX_SLEEP_SECONDS = 30;

const BACKGROUND_WORDS = /(^|[\s;|&(])(nohup|setsid|disown)\b/;

/** True iff `command` puts work in the background with a shell `&` (not
 * `&&`, and not the redirections `2>&1`, `&>`, `>&`). Quoted strings are
 * ignored. A string-level heuristic, like every guard here. */
export function backgroundsWork(command: string): boolean {
  const unquoted = command.replace(/'[^']*'/g, "''").replace(/"(?:[^"\\]|\\.)*"/g, '""');
  const withoutRedirects = unquoted.replace(/\d*>&\d*/g, " ").replace(/&>/g, " ").replace(/&&/g, " ");
  return /&/.test(withoutRedirects);
}

/** The longest `sleep N` in `command`, in seconds (s/m/h suffixes). */
export function longestSleepSeconds(command: string): number {
  let max = 0;
  for (const m of command.matchAll(/\bsleep\s+(\d+(?:\.\d+)?)([smhd]?)\b/g)) {
    const n = Number(m[1]);
    const unit = m[2] === "m" ? 60 : m[2] === "h" ? 3600 : m[2] === "d" ? 86400 : 1;
    max = Math.max(max, n * unit);
  }
  return max;
}

/** Plan 01a: the secret whose value `command` contains, or `undefined`.
 * The values are read from THIS process's environment (`env`, the agent's own
 * environment — the conductor hands each secret over as a variable, so a
 * guard never needs a value to be passed to it any other way), and only the
 * maskable ones count: a value too short to mask safely (a declared secret
 * exported as "1") would otherwise refuse nearly every command. The reason
 * names the variable to use instead and never repeats the value or the
 * command (which holds it). */
export function secretUseInCommand(
  command: string,
  names: readonly string[],
  env: NodeJS.ProcessEnv = process.env,
): string | undefined {
  for (const { name, value } of maskableSecrets(names, env)) {
    if (!command.includes(value)) continue;
    return (
      `sh command contains the value of the secret ${name}. Use the environment variable instead — write "$${name}". ` +
      "Nothing was run. A secret value must never be pasted into a command, an edit or a submission."
    );
  }
  return undefined;
}

/** A `find` (or `fd`, `locate`, `mdfind`) over the whole disk or the home
 * directory: the 13a worker of program 59163ee2 ran `find / -name …` for its
 * plan's reference documents, which are listed in its prompt. */
export function searchesWholeDisk(command: string): boolean {
  return (
    /(?:^|[;&|(]\s*)(?:sudo\s+)?find\s+(?:-[A-Za-z]+\s+)*(?:\/|~\/?|\$HOME\/?|\/Users\/?|\/home\/?)(?=\s|$)/.test(command) ||
    /(?:^|[;&|(]\s*)(?:locate|mdfind)\b/.test(command)
  );
}

/** True iff `command` (the `sh` tool's `command` argument) must be blocked:
 * a secret's literal value, `git commit`/`git push`, backgrounded work, a
 * sleep longer than MAX_SLEEP_SECONDS, or any mention of the run directory
 * outside the worker's own worktree. This is a string-level check only — see
 * the module comment above. Returns a human-readable reason, or `undefined`
 * if the command is allowed. */
export function guardedShCommand(command: string, config: GuardConfig): string | undefined {
  // First: the one check whose reason must not repeat the command.
  const leak = secretUseInCommand(command, config.secretNames ?? []);
  if (leak !== undefined) return leak;
  if (searchesWholeDisk(command)) {
    return (
      "searching the whole disk or home directory is refused: the plan's reference documents are listed in your " +
      `prompt (read them directly), and the code is in your checkout. Search there instead: ${command}`
    );
  }
  for (const pattern of BLOCKED_SH_PATTERNS) {
    if (pattern.test(command)) {
      return `sh command containing '${pattern.source}' is blocked (workflow guard, not a security boundary): ${command}`;
    }
  }
  if (BACKGROUND_WORDS.test(command) || backgroundsWork(command)) {
    return (
      "sh commands may not run work in the background (&, nohup, setsid, disown): the conductor enforces each " +
      "command's deadline and runs the phase CHECKS itself after submit_phase. Run the narrow test you need in " +
      `the foreground instead: ${command}`
    );
  }
  const sleep = longestSleepSeconds(command);
  if (sleep > MAX_SLEEP_SECONDS) {
    return (
      `sh commands may not sleep longer than ${MAX_SLEEP_SECONDS}s (found ${sleep}s): there is nothing to wait for — ` +
      `run commands in the foreground, and leave the full CHECKS to the conductor: ${command}`
    );
  }
  if (config.runDir) {
    // The worktree lives inside the run directory (<run>/worktree); a
    // mention of the worktree itself is not a mention of the run directory.
    const outsideWorktree = config.worktree ? command.split(config.worktree).join("") : command;
    if (outsideWorktree.includes(config.runDir)) {
      return `sh command touching the run directory (${config.runDir}) is blocked: ${command}`;
    }
  }
  return undefined;
}

function readEnv(name: string): string | undefined {
  const v = process.env[name];
  return v && v.length > 0 ? v : undefined;
}

/** Where `find`/`grep`/`ls` may look (`TT_SEARCH_ROOTS`: the agent's
 * checkout and the run's reference copies). A search elsewhere is refused
 * with the roots named: run aea875c4's reviewers ran recursive `find` searches over the
 * whole home directory for 6 minutes looking for the plan's documents. An
 * unset list allows everything. */
export function guardedSearchPath(targetPath: string | undefined, cwd: string, roots: string[]): string | undefined {
  if (roots.length === 0) return undefined;
  const target = canonicalize(resolvePath(cwd, targetPath ?? "."));
  if (roots.some((r) => isUnder(target, canonicalize(r)))) return undefined;
  return `search only inside your checkout or the plan's reference documents (${roots.join(", ")}); ${targetPath} is outside them. The plan's documents are listed in your prompt: read them directly.`;
}

export function readSearchRootsFromEnv(): string[] {
  return (readEnv("TT_SEARCH_ROOTS") ?? "")
    .split(":")
    .map((s) => s.trim())
    .filter((s) => s.length > 0);
}

export function readGuardConfigFromEnv(): GuardConfig {
  return {
    worktree: readEnv("TT_WORKTREE"),
    runDir: readEnv("TT_RUN_DIR"),
    protectedPaths: (readEnv("TT_PROTECTED") ?? "")
      .split(",")
      .map((s) => s.trim())
      .filter((s) => s.length > 0),
    secretNames: (readEnv("TT_SECRETS") ?? "")
      .split(/[\s,]+/)
      .map((s) => s.trim())
      .filter((s) => s.length > 0),
  };
}
