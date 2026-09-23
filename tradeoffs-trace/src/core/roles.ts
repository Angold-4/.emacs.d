// Pure launch/role data for tradeoffs-trace's Pi agents (design §2.1).
//
// No process, socket or filesystem access here beyond resolving the
// skeleton extension's own path with `import.meta.url` (a pure string
// computation, not an I/O call) — the actual `pi` process is spawned by an
// effects module in a later phase, or directly by tests in this one.

import { fileURLToPath } from "node:url";

/** Pinned Pi version (design "Conventions" table). Asserted at conductor
 * start; an upgrade is a deliberate PR that reruns the phase 0 contract
 * tests. */
export const PI_VERSION = "0.87.0";

export type Role = "worker" | "reviewer";

/** design §2.1's launch table. Every role uses an explicit allowlist, never
 * `--exclude-tools` (Pi's default set omits `grep`, `find` and `ls`, so a
 * denylist leaves gaps and, for a worker, leaks the reviewer submission
 * tools — see the negative case in role-tool-sets.test.ts). */
export const ROLE_TOOLS: Record<Role, string[]> = {
  worker: ["read", "edit", "write", "grep", "find", "ls", "sh", "submit_phase"],
  reviewer: ["read", "grep", "find", "ls", "submit_discovery", "submit_review"],
};

/** The skeleton extension's own file, resolved relative to this module so
 * it works regardless of the caller's cwd. */
export function defaultExtensionPath(): string {
  return fileURLToPath(new URL("../../extension/tradeoffs-trace.ts", import.meta.url));
}

export interface LaunchOptions {
  /** Absolute path to the extension file. Defaults to the skeleton
   * extension shipped in this package. */
  extensionPath?: string;
  provider?: string;
  model?: string;
  /** Directory for session storage. Omit (or pass `noSession: true`) for
   * an ephemeral run — the phase-0 contract test never persists a session. */
  sessionDir?: string;
  /** Defaults to true: contract tests and role launches never send a
   * prompt or need a session on disk. */
  noSession?: boolean;
  /** Plan 2c: resume the most recent session in `sessionDir` (Pi's
   * `--continue`), so a repair attempt or a later review round keeps the
   * agent's own earlier reasoning (design §2, §6.1). */
  continueSession?: boolean;
}

/** Builds the full `pi` argv (everything after the `pi` binary itself) for
 * launching one role's agent, per design §2.1: `--mode rpc`, the role's
 * explicit `--tools` allowlist, the skeleton extension, `--no-extensions`
 * and `--no-skills` so the owner's own user extensions/skills under
 * `~/.pi/agent` never leak into a role's tool set, and no prompt is ever
 * included — dispatching a prompt is a later phase's job. */
export function launchArgs(role: Role, opts: LaunchOptions = {}): string[] {
  const extensionPath = opts.extensionPath ?? defaultExtensionPath();
  const args: string[] = [
    "--mode",
    "rpc",
    "--tools",
    ROLE_TOOLS[role].join(","),
    "--extension",
    extensionPath,
    "--no-extensions",
    "--no-skills",
  ];
  if (opts.provider) args.push("--provider", opts.provider);
  if (opts.model) args.push("--model", opts.model);
  if (opts.sessionDir && !opts.noSession) {
    args.push("--session-dir", opts.sessionDir);
    if (opts.continueSession) args.push("--continue");
  } else {
    args.push("--no-session");
  }
  return args;
}

export interface ToolSetOk {
  ok: true;
}

export interface ToolSetMismatch {
  ok: false;
  missing: string[];
  extra: string[];
  duplicates: string[];
}

export type ToolSetResult = ToolSetOk | ToolSetMismatch;

/** Exact set equality between a role's expected tools (`ROLE_TOOLS[role]`)
 * and what Pi actually reported active (`pi.getActiveTools()` at
 * `session_start`, forwarded over the run socket as the `hello` message's
 * `tools`). A repeated tool name is itself a mismatch — Pi should report
 * each active tool once. */
export function assertToolSet(role: Role, reported: string[]): ToolSetResult {
  const expected = ROLE_TOOLS[role];
  const expectedSet = new Set(expected);
  const reportedSet = new Set(reported);

  const counts = new Map<string, number>();
  for (const t of reported) counts.set(t, (counts.get(t) ?? 0) + 1);
  const duplicates = [...counts.entries()].filter(([, n]) => n > 1).map(([t]) => t);

  const missing = expected.filter((t) => !reportedSet.has(t));
  const extra = [...reportedSet].filter((t) => !expectedSet.has(t));

  if (missing.length === 0 && extra.length === 0 && duplicates.length === 0) {
    return { ok: true };
  }
  return { ok: false, missing, extra, duplicates };
}
