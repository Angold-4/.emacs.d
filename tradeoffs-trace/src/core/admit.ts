// ADMIT (07 design §3.1): the one gate that runs before any event, on every
// start — `tt start`, `tt resume` and a program node's own start. It answers
// one question: may this run exist at all? Four facts decide it, and each
// failure is a NAMED exception with its reason (never a silent retry, never a
// half-created run):
//
//   * the plan lints                                     → X:PLAN_DEFECT
//   * TT_REPO is a git work tree                          → X:PLAN_DEFECT
//   * every check command parses (`bash -n`)              → X:PLAN_DEFECT
//   * the pinned pi is the one on the resolved PATH       → X:ENV_FAULT
//
// This module is PURE: it holds the facts ADMIT collected and decides. The
// effect layer (`src/effects/admit.ts`) collects them with real commands, so
// the decision is unit-testable and one function serves start, resume and a
// program node's start (07 design A1).
//
// The env check is re-run on EVERY start: it resolves PATH and the pinned pi
// from the shell that runs the command now, and never from `check-env.json`.
// On 2026-10-28 (exex 07f1) a resume adopted the PATH a run had recorded at
// start — which by then held a brew `pi` 1.4.0 — and the conductor crashed at
// its first agent launch instead of naming the environment fault.

import { PI_VERSION } from "./roles.ts";
import { formatFinding, lintPlan, type LintPlanInput } from "./plan-lint.ts";

/** The two named exceptions ADMIT can raise (07 design §3.4). A plan defect
 * is fixed by amending the plan; an environment fault is fixed on the machine
 * and ADMIT's env check re-run. */
export type AdmitException = "X:PLAN_DEFECT" | "X:ENV_FAULT";

/** Everything ADMIT saw on this start. Recorded (`<run>/admit.json`) so the
 * owner can read what was checked and against what — never read back as the
 * source of a later decision. */
export interface AdmitFacts {
  /** The plan's own lint errors, one `file:line: severity: problem` line
   * each (empty = it lints). */
  lint: string[];
  /** `TT_REPO`, re-read as a git work tree on this start. */
  repo: { path: string; ok: boolean; error?: string };
  /** Each declared command `bash -n` refused, with bash's own message. */
  unparsed: Array<{ command: string; error: string }>;
  /** The PATH this start resolved on — the shell's own, re-read every time. */
  path: string;
  /** The pinned pi this start found on that PATH. `skipped` names why the
   * version check did not run at all (a test's injected fake-pi binary, which
   * is never the real one). */
  pi: { name: string; pinned: string; path?: string; version?: string; error?: string; skipped?: string };
}

export interface AdmitPass {
  ok: true;
  facts: AdmitFacts;
}

export interface AdmitFailure {
  ok: false;
  kind: AdmitException;
  /** One line, naming what is wrong and where. */
  reason: string;
  /** What ADMIT saw, recorded even when it refuses. */
  facts: AdmitFacts;
}

export type AdmitVerdict = AdmitPass | AdmitFailure;

/** The exception a refused ADMIT is, as the CLI prints it and a caller
 * reports it: `X:PLAN_DEFECT: <reason>`. */
export function admitRefusalText(kind: AdmitException, reason: string): string {
  return `${kind}: ${reason}`;
}

/** The error every caller throws or reports for a refusing ADMIT. It carries
 * the kind, so a caller can branch on a plan defect versus an environment
 * fault without parsing the message. */
export class AdmitRefusal extends Error {
  readonly kind: AdmitException;
  readonly reason: string;
  readonly facts: AdmitFacts;

  constructor(kind: AdmitException, reason: string, facts: AdmitFacts) {
    super(admitRefusalText(kind, reason));
    this.name = "AdmitRefusal";
    this.kind = kind;
    this.reason = reason;
    this.facts = facts;
  }
}

/** The plan shape ADMIT reads: its global checks and every phase's own
 * commands. Structural, so a plan file, a program entry's plan and a run's
 * plan snapshot all pass unchanged. */
export interface AdmitPlanShape {
  checks?: string[];
  phases?: Array<{ checks?: string[]; finalChecks?: string[]; gate?: string; gateCleanup?: string }>;
}

/** Every command a plan can run for any of its phases, in declaration order,
 * deduped: the global checks, then each phase's checks, final checks, gate
 * and gate cleanup. These are exactly the strings that must parse. */
export function admitCommands(plan: AdmitPlanShape): string[] {
  const out: string[] = [];
  const seen = new Set<string>();
  const add = (command: string | undefined): void => {
    if (typeof command !== "string" || command.length === 0) return;
    if (seen.has(command)) return;
    seen.add(command);
    out.push(command);
  };
  for (const command of plan.checks ?? []) add(command);
  for (const phase of plan.phases ?? []) {
    for (const command of phase.checks ?? []) add(command);
    for (const command of phase.finalChecks ?? []) add(command);
    add(phase.gate);
    add(phase.gateCleanup);
  }
  return out;
}

/** A complete version token: `0.87.0`, a `v`-prefixed one, or a
 * prerelease/build suffix on it. Anchored to whole tokens, so `10.87.0` is one
 * token, never the pin inside another one. */
const VERSION_TOKEN = /(?<![\w.-])[vV]?\d+\.\d+\.\d+(?:[-+][0-9A-Za-z.-]+)?(?![\w.-])/g;

/** A version as compared: trimmed, with a leading `v` dropped. */
function normVersion(text: string): string {
  return text.trim().replace(/^[vV]/, "");
}

/** Does what the resolved pi reported (`pi --version`'s own line) assert the
 * pin? Only a version shown in full counts: `0.87.0` passes the pin `0.87.0`,
 * while `10.87.0` — which merely CONTAINS the pin's text — must not. A
 * `--version` that prints words around the number (`pi 0.87.0`, `pi v0.87.0`)
 * still passes when the only complete version in it is the pin. */
function reportsPin(reported: string, pinned: string): boolean {
  const pin = normVersion(pinned);
  if (normVersion(reported) === pin) return true;
  const tokens = reported.match(VERSION_TOKEN) ?? [];
  return tokens.length > 0 && tokens.every((token) => normVersion(token) === pin);
}

/** The first line of a command's stderr, for a one-line reason. */
function firstLine(text: string | undefined): string {
  const line = (text ?? "").split("\n").map((l) => l.trim()).find((l) => l.length > 0);
  return line ?? "";
}

/** ADMIT's decision, from the facts the effect layer collected. Plan defects
 * are decided before the environment: a plan that can never run is not an
 * environment fault, whatever the machine looks like. */
export function admitVerdict(facts: AdmitFacts): AdmitVerdict {
  if (facts.lint.length > 0) {
    const first = firstLine(facts.lint[0]);
    const more = facts.lint.length > 1 ? ` (and ${facts.lint.length - 1} more lint error(s))` : "";
    return { ok: false, kind: "X:PLAN_DEFECT", reason: `the plan does not lint: ${first}${more}`, facts };
  }
  if (!facts.repo.ok) {
    const where = facts.repo.path.length > 0 ? `${facts.repo.path}` : "TT_REPO";
    return {
      ok: false,
      kind: "X:PLAN_DEFECT",
      reason: `${where} is not a git work tree: ${facts.repo.error ?? "git refused it"}`,
      facts,
    };
  }
  if (facts.unparsed.length > 0) {
    const [first, ...rest] = facts.unparsed;
    const more = rest.length > 0 ? ` (and ${rest.length} more command(s) that do not parse)` : "";
    return {
      ok: false,
      kind: "X:PLAN_DEFECT",
      reason: `the check command does not parse: \`${first.command}\` — ${first.error}${more}`,
      facts,
    };
  }
  if (facts.pi.skipped) return { ok: true, facts };
  if (facts.pi.version === undefined) {
    return {
      ok: false,
      kind: "X:ENV_FAULT",
      reason:
        facts.pi.error !== undefined
          ? `could not run \`${facts.pi.path ?? facts.pi.name} --version\` to assert the pinned pi (${facts.pi.pinned}): ${facts.pi.error}`
          : `no \`${facts.pi.name}\` on the PATH ${facts.path} — the pinned pi ${facts.pi.pinned} must be the one on the PATH`,
      facts,
    };
  }
  if (!reportsPin(facts.pi.version, facts.pi.pinned)) {
    return {
      ok: false,
      kind: "X:ENV_FAULT",
      reason: `pi at ${facts.pi.path ?? facts.pi.name} reports ${facts.pi.version}, not the pinned ${facts.pi.pinned} (PATH: ${facts.path})`,
      facts,
    };
  }
  return { ok: true, facts };
}

/** The pinned pi version this build asserts (design §2.1). Re-exported here
 * so a caller of `admit` needs one import, not two. */
export const ADMIT_PINNED_PI = PI_VERSION;

/** ADMIT's lint half, as the facts carry it: the plan's own errors only. A
 * warning (a coverage or reach note) never refuses a run. */
export function admitLintErrors(plan: unknown): string[] {
  const findings = lintPlan(plan as LintPlanInput);
  return findings.filter((f) => f.severity === "error").map((f) => formatFinding(f, "<plan>"));
}
