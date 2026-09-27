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

export type Role = "worker" | "reviewer" | "evaluator" | "panel";

/** The reviewer seats (design §2.1): three reviewers of different model
 * families, whose disagreement is the point of having three. */
export type ReviewerSeat = "M" | "A" | "B";

/** The three panel seats, named by position. */
export type PanelSeat = 1 | 2 | 3;

/** `panel=reviewers`: each panel seat gets the reviewer model of its
 * position, so the panel disagrees with the same variety the reviewers do. */
export type PanelFrom = "reviewers";

/** A role's provider/model as a plan declares it (#+TT_MODELS). `provider`
 * is the part before the first `:` **when that part has no `/`** — Pi's
 * `--model` accepts a thinking suffix (`openai/gpt-6-sol:high`), so a value
 * whose first `:` follows a `/` is all model. A model id may itself contain
 * `/` (`vercel-ai-gateway:anthropic/claude-sonnet-5`). */
export interface RoleModel {
  provider?: string;
  model?: string;
}

/** A plan's `#+TT_MODELS` map (design §2.1). The four flat roles are how 05a
 * declared it; the per-seat maps are how 05b lets M, A, B and panel seats 1–3
 * each run on a different model, and `panelFrom: "reviewers"` makes each
 * panel seat follow the reviewer of its position. Absent: every role and seat
 * keeps Pi's `defaultModel`. */
export interface PlanModels {
  worker?: RoleModel;
  reviewer?: RoleModel;
  evaluator?: RoleModel;
  panel?: RoleModel;
  /** Done by Emacs for `reviewer.M`, `reviewer.A`, `reviewer.B`; a seat with
   * no entry uses `reviewer`'s. */
  reviewerSeats?: Partial<Record<ReviewerSeat, RoleModel>>;
  /** Done by Emacs for `panel.1`, `panel.2`, `panel.3`; a seat with no entry
   * uses the reviewer of its position when `panelFrom` is set, else `panel`'s. */
  panelSeats?: Partial<Record<string, RoleModel>>;
  /** `panel=reviewers` in the keyword. */
  panelFrom?: PanelFrom;
}

/** The model each panel seat takes from the reviewer seats, by position. */
const PANEL_SEAT_REVIEWER: Record<string, ReviewerSeat> = { "1": "M", "2": "A", "3": "B" };

/** The one place that answers "which provider/model does ROLE (and, for the
 * reviewer and panel roles, which seat) run with?" — a plan's own `models`
 * map, when it declares one. Resolution, exactly as the runbook documents:
 *
 *  - reviewer M/A/B: the seat's own model, else `reviewer`'s;
 *  - panel seat N: `panel.N`, else the reviewer of position N when
 *    `panelFrom` is `"reviewers"`, else `panel`'s.
 *
 * Absent (the default): the caller passes neither `--provider` nor `--model`
 * and Pi uses its `defaultModel`. */
export function planModelSelector(
  plan: { models?: PlanModels },
): (role: Role, seat?: ReviewerSeat | PanelSeat | string | number) => RoleModel | undefined {
  const models = plan.models;
  const select = (role: Role, seat?: ReviewerSeat | PanelSeat | string | number): RoleModel | undefined => {
    if (!models) return undefined;
    if (role === "reviewer") {
      if (seat !== undefined) {
        const own = models.reviewerSeats?.[String(seat) as ReviewerSeat];
        if (own) return own;
      }
      return models.reviewer;
    }
    if (role === "panel") {
      if (seat !== undefined) {
        const own = models.panelSeats?.[String(seat)];
        if (own) return own;
        if (models.panelFrom === "reviewers") {
          const from = PANEL_SEAT_REVIEWER[String(seat)];
          const inherited = from ? select("reviewer", from) : undefined;
          if (inherited) return inherited;
        }
      }
      return models.panel;
    }
    return models[role];
  };
  return select;
}

/** design §2.1's launch table. Every role uses an explicit allowlist, never
 * `--exclude-tools` (Pi's default set omits `grep`, `find` and `ls`, so a
 * denylist leaves gaps and, for a worker, leaks the reviewer submission
 * tools — see the negative case in role-tool-sets.test.ts). */
export const ROLE_TOOLS: Record<Role, string[]> = {
  worker: ["read", "edit", "write", "grep", "find", "ls", "sh", "submit_phase", "raise_tradeoff"],
  reviewer: ["read", "grep", "find", "ls", "submit_discovery", "submit_review"],
  // Plan 04a: the evaluator checks a round's raw messages against the code
  // it can read, and returns through `submit_evaluation`. No write tools, and
  // no `raise_tradeoff`: the plan gives that tool to the worker, and a tool
  // the evaluator could never use would be a dead interface.
  evaluator: ["read", "grep", "find", "ls", "submit_evaluation"],
  // Plan 04b: one fresh panel seat per blocker vote. It reads the phase
  // contract, the owner directives, the ledger, the blocker and its evidence,
  // and the candidate's diff, and returns a single `block`/`downgrade` vote
  // through `submit_panel_vote`.
  panel: ["read", "grep", "find", "ls", "submit_panel_vote"],
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
