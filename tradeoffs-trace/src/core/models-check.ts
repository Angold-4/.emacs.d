// 06a finding #24: `tt models check [plan-or-program]`.
//
// A plan's `#+TT_MODELS` can name a model the gateway refuses (the owner's
// 2026-10-07 probe found `grok-4.7` answering 403 `restricted`), and until now
// a run only discovered that when the first agent was launched — after the
// baseline, the worktree and the freeze. This module is the pure half of a
// preflight that sends one tiny prompt per DISTINCT configured provider/model
// in Pi's print mode, classifies each answer as `ok`, `refused` (with the
// gateway's own message) or `unreachable`, and lets `tt start` /
// `tt program start` refuse before any run is launched.
//
// The process spawning lives in `src/effects/models-check.ts`; this module is
// pure so the classification and the distinct-model resolution are
// unit-testable directly.

import { planModelSelector, type PlanModels, type RoleModel } from "./roles.ts";

/** One role or seat's configured model, with the label the status line uses
 * (`worker`, `reviewer.M`, `panel.2`, …). */
export interface ModelTarget {
  role: string;
  provider?: string;
  model: string;
}

/** Every role and seat a plan's `#+TT_MODELS` configures, in the pipeline's
 * order. A role or seat with no model is omitted, exactly as the status
 * `models` line omits it. The seat expansion mirrors `view.ts`'s
 * `modelEntries`: per-seat maps when present, else the shared role. */
export function planModelTargets(models: PlanModels | undefined): ModelTarget[] {
  if (!models) return [];
  const declaredSeats = models.reviewerSeats ? Object.keys(models.reviewerSeats) : [];
  const select = planModelSelector({ models }, declaredSeats.length > 0 ? declaredSeats : undefined);
  const out: ModelTarget[] = [];
  const push = (role: string, m: RoleModel | undefined): void => {
    if (!m?.model) return;
    out.push({ role, ...(m.provider ? { provider: m.provider } : {}), model: m.model });
  };
  push("worker", models.worker);
  // Plan 06h (A2): the seats the plan actually declares, not a fixed three.
  const reviewerSeatKeys = models.reviewerSeats ? Object.keys(models.reviewerSeats) : [];
  if (reviewerSeatKeys.length > 0) {
    for (const seat of reviewerSeatKeys) push(`reviewer.${seat}`, select("reviewer", seat));
  } else {
    push("reviewer", models.reviewer);
  }
  push("evaluator", models.evaluator);
  // Plan 05j: the curator runs on the evaluator's model unless the plan names
  // its own; either way it is a model this run will use.
  if (models.curator) push("curator", models.curator);
  const panelSeatKeys = models.panelSeats ? Object.keys(models.panelSeats) : [];
  if (panelSeatKeys.length > 0) {
    for (const seat of panelSeatKeys) push(`panel.${seat}`, select("panel", seat));
  } else if (models.panelFrom === "reviewers" && reviewerSeatKeys.length > 0) {
    reviewerSeatKeys.forEach((_, i) => push(`panel.${i + 1}`, select("panel", String(i + 1))));
  } else {
    push("panel", models.panel);
  }
  return out;
}

/** One DISTINCT provider/model and every role/seat that runs on it. The
 * check sends one prompt per group, not per role: the same
 * `vercel-ai-gateway:anthropic/claude-opus-5.5` for M, the evaluator and
 * panel seat 1 is one probe. */
export interface ModelGroup {
  /** Canonical `provider:model`, or just `model` when no provider is named. */
  key: string;
  provider?: string;
  model: string;
  roles: string[];
}

/** Group `targets` by distinct `provider:model`, preserving first-seen order
 * and collecting every role that shares the group. */
export function distinctModelGroups(targets: readonly ModelTarget[]): ModelGroup[] {
  const byKey = new Map<string, ModelGroup>();
  for (const t of targets) {
    const key = t.provider ? `${t.provider}:${t.model}` : t.model;
    const existing = byKey.get(key);
    if (existing) {
      if (!existing.roles.includes(t.role)) existing.roles.push(t.role);
      continue;
    }
    byKey.set(key, { key, ...(t.provider ? { provider: t.provider } : {}), model: t.model, roles: [t.role] });
  }
  return [...byKey.values()];
}

/** The tiny prompt every probe sends. It only has to prove the model answers
 * at all; the content is deliberately trivial. */
export const MODELS_CHECK_PROMPT = "Reply with the single word OK.";

/** design 06a: a 60 s bound per probe, the same order as one agent turn. */
export const MODELS_CHECK_TIMEOUT_MS = 60_000;

export type ModelStatus = "ok" | "refused" | "unreachable";

export interface ModelProbeOutcome {
  /** Process exit status, or null when it was killed by a signal. */
  exitCode: number | null;
  timedOut: boolean;
  /** stdout and stderr, concatenated. */
  output: string;
}

export interface ModelProbeResult {
  key: string;
  provider?: string;
  model: string;
  roles: string[];
  status: ModelStatus;
  /** The gateway's own refusal message, when one could be read (e.g. `403
   * restricted`). */
  message?: string;
}

/** What a run writes to `<run|program>/models-check.json`. */
export interface ModelsCheck {
  at: string;
  probes: ModelProbeResult[];
}

const ANSI = /\u001b\[[0-9;]*m/g;
/** A refusal names the HTTP status or the gateway's own words. Anything else
 * that exits non-zero is `unreachable` (a DNS failure, a crash, a timeout). */
const REFUSAL = /(403|restricted|forbidden|permission denied|not authorized|not allowed)/i;

/** The first line that reads like a refusal, trimmed and bounded, or
 * undefined when nothing does. */
export function refusalMessage(output: string): string | undefined {
  const text = output.replace(ANSI, "");
  for (const line of text.split("\n")) {
    const trimmed = line.trim();
    if (trimmed.length > 0 && REFUSAL.test(trimmed)) return trimmed.length > 200 ? `${trimmed.slice(0, 199)}…` : trimmed;
  }
  return undefined;
}

/** design 06a: `ok` (exit 0), `refused` (a non-zero exit whose output names a
 * 403/restricted refusal) or `unreachable` (anything else — a timeout, a
 * crash, a connection error). A timeout is always `unreachable`, even if its
 * truncated output happened to mention 403. */
export function classifyModelProbe(outcome: ModelProbeOutcome): { status: ModelStatus; message?: string } {
  if (outcome.timedOut) return { status: "unreachable" };
  if (outcome.exitCode === 0) return { status: "ok" };
  const message = refusalMessage(outcome.output);
  if (message !== undefined) return { status: "refused", message };
  return { status: "unreachable" };
}

/** Every probe the gateway refused. `tt start` / `tt program start` refuse to
 * launch when this is non-empty (unless `--skip-models-check`). */
export function modelsCheckRefused(check: ModelsCheck | undefined): ModelProbeResult[] {
  return (check?.probes ?? []).filter((p) => p.status === "refused");
}

/** The status `models` line's own note, e.g. `check ok` or `check refused:
 * reviewer.B (403 restricted)`. Undefined when nothing was probed, so a plan
 * with no `#+TT_MODELS` (or an old run with no record) keeps the plain
 * models line. */
export function modelsCheckStatusText(check: ModelsCheck | undefined): string | undefined {
  const probes = check?.probes ?? [];
  if (probes.length === 0) return undefined;
  const bad = probes.filter((p) => p.status !== "ok");
  if (bad.length === 0) return "check ok";
  const parts = bad.map((p) => `${p.model} ${p.status}${p.message ? ` (${p.message})` : ""}`);
  return `check ${parts.join("; ")}`;
}

/** Shape-check a JSON-parsed `models-check.json`. Undefined for anything that
 * is not one (a corrupt file), so a view falls back to the plain models
 * line. */
export function parseModelsCheck(value: unknown): ModelsCheck | undefined {
  if (value === null || typeof value !== "object") return undefined;
  const raw = value as Record<string, unknown>;
  if (typeof raw.at !== "string" || !Array.isArray(raw.probes)) return undefined;
  const probes: ModelProbeResult[] = [];
  for (const entry of raw.probes) {
    if (entry === null || typeof entry !== "object") return undefined;
    const p = entry as Record<string, unknown>;
    if (typeof p.key !== "string" || typeof p.model !== "string" || typeof p.status !== "string") return undefined;
    if (p.status !== "ok" && p.status !== "refused" && p.status !== "unreachable") return undefined;
    probes.push({
      key: p.key,
      ...(typeof p.provider === "string" ? { provider: p.provider } : {}),
      model: p.model,
      roles: Array.isArray(p.roles) ? p.roles.filter((r): r is string => typeof r === "string") : [],
      status: p.status,
      ...(typeof p.message === "string" ? { message: p.message } : {}),
    });
  }
  return { at: raw.at, probes };
}
