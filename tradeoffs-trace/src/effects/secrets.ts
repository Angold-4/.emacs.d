// Secrets (design §7: "never leak a credential"; atlas plan 13 §7: 721
// occurrences of vendor keys in one program's stream files, the Emacs trace
// and the refs/ copies of the vendor docs).
//
// A plan declares its secrets by NAME (`#+TT_SECRETS: NAME1 NAME2`, parsed by
// Emacs into the JSON plan's `secrets`). The conductor resolves each value
// from *its own* environment, passes it to every agent's environment, and
// replaces every value with `***NAME***` in everything it writes. Nothing in
// this module ever prints, logs or returns a value to a caller that is not
// about to substitute it back into an environment.
//
// The replacement text (`***NAME***`) contains no quote, backslash or
// newline, so it can never break a JSON line (or a JSONL file) that a
// value was redacted out of.

import * as fs from "node:fs";
import * as path from "node:path";

import { readLog } from "./log.ts";

export interface Secret {
  name: string;
  /** The value, read once from the conductor's own environment. */
  value: string;
}

/** Only shell-assignable names (`NAME=value` must work in the agent's env). */
const SECRET_NAME_RE = /^[A-Za-z_][A-Za-z0-9_]*$/;

function placeholder(name: string): string {
  return `***${name}***`;
}

/** How a value appears inside JSON text: a value holding a quote or a
 * backslash is escaped there, so a textual rewrite has to match that form
 * too (the structural path never sees it — it walks the parsed strings). */
function jsonEscaped(value: string): string {
  return JSON.stringify(value).slice(1, -1);
}

/** The secret names a plan declares: deduped, order kept, non-names dropped. */
export function secretNames(raw: readonly string[] | undefined): string[] {
  const out: string[] = [];
  for (const chunk of raw ?? []) {
    for (const name of String(chunk).split(/[\s,]+/)) {
      if (SECRET_NAME_RE.test(name) && !out.includes(name)) out.push(name);
    }
  }
  return out;
}

/** Which of NAMES are set in ENV (the conductor's own environment — a secret
 * is never carried anywhere but the environment). An unset or empty variable
 * is `missing`: the run starts anyway and the status says so. */
export function resolveSecrets(
  names: readonly string[],
  env: NodeJS.ProcessEnv = process.env,
): { secrets: Secret[]; missing: string[] } {
  const secrets: Secret[] = [];
  const missing: string[] = [];
  for (const name of names) {
    const value = env[name];
    if (value !== undefined && value.length > 0) secrets.push({ name, value });
    else missing.push(name);
  }
  return { secrets, missing };
}

/** TEXT with every secret value replaced by `***NAME***`. */
export function redactText(text: string, secrets: readonly Secret[]): string {
  let out = text;
  for (const s of secrets) {
    if (s.value.length === 0 || !out.includes(s.value)) continue;
    out = out.split(s.value).join(placeholder(s.name));
  }
  return out;
}

/** A copy of a JSON-shaped VALUE (agent stream events, log payloads) with
 * every string redacted. Keys are structure, not content, and are kept. */
export function redactJson(value: unknown, secrets: readonly Secret[]): unknown {
  if (secrets.length === 0 || value === null || value === undefined) return value;
  if (typeof value === "string") return redactText(value, secrets);
  if (Array.isArray(value)) return value.map((v) => redactJson(v, secrets));
  if (typeof value === "object") {
    const out: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(value as Record<string, unknown>)) out[k] = redactJson(v, secrets);
    return out;
  }
  return value;
}

/** Textual replacement for HITS (the raw and the JSON-escaped form of each
 * value): SAFE on JSON text, because `***NAME***` holds no quote, backslash
 * or newline. Used for a line that does not parse, and for the rare value
 * that survives the structural pass (one sitting in a JSON *key*, which the
 * structural walk leaves alone because keys are structure). */
function redactTextual(text: string, hits: readonly Secret[]): string {
  let out = redactText(text, hits);
  for (const s of hits) out = out.split(jsonEscaped(s.value)).join(placeholder(s.name));
  return out;
}

/** One JSONL line, redacted. A line that parses is redacted structurally and
 * re-serialized (so it stays valid JSON); one that does not — a torn final
 * line, or a file that was never JSONL — falls back to textual replacement,
 * which cannot corrupt JSON either. */
export function redactJsonLine(line: string, secrets: readonly Secret[]): string {
  if (line.length === 0 || secrets.length === 0) return line;
  const hits = secrets.filter((s) => s.value.length > 0 && (line.includes(s.value) || line.includes(jsonEscaped(s.value))));
  if (hits.length === 0) return line;
  try {
    const text = JSON.stringify(redactJson(JSON.parse(line), secrets));
    const survives = hits.some((s) => text.includes(s.value) || text.includes(jsonEscaped(s.value)));
    return survives ? redactTextual(text, hits) : text;
  } catch {
    return redactTextual(line, hits);
  }
}

/** A whole JSONL file's text, redacted line by line. A trailing partial line
 * (a crash mid-write, no closing newline) is redacted too, and left partial:
 * no newline is added, and no line is split or joined. */
export function redactJsonl(text: string, secrets: readonly Secret[]): string {
  if (secrets.length === 0 || text.length === 0) return text;
  const lines = text.split("\n");
  const endedWithNewline = lines[lines.length - 1] === "";
  if (endedWithNewline) lines.pop();
  const out = lines.map((line) => redactJsonLine(line, secrets)).join("\n");
  return endedWithNewline ? `${out}\n` : out;
}

/** True (and rewrites the file) when FILE contained a secret value. */
export function redactFileInPlace(file: string, secrets: readonly Secret[], jsonl: boolean): boolean {
  let buf: Buffer;
  try {
    buf = fs.readFileSync(file);
  } catch {
    return false;
  }
  if (buf.includes(0)) return false; // binary: never text-rewrite it
  const text = buf.toString("utf8");
  const next = jsonl ? redactJsonl(text, secrets) : redactText(text, secrets);
  if (next === text) return false;
  fs.writeFileSync(file, next);
  return true;
}

function filesUnder(dir: string): string[] {
  let entries: fs.Dirent[];
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return [];
  }
  const out: string[] = [];
  for (const e of entries) {
    const p = path.join(dir, e.name);
    if (e.isDirectory()) out.push(...filesUnder(p));
    else if (e.isFile()) out.push(p);
  }
  return out;
}

/** Rewrites a run directory in place (`tt redact`): every file a secret
 * value can sit in — `events.jsonl`, `stream/*.jsonl`, `sessions/`, `checks/**`,
 * `refs/**`, `views/**`, `plan/`, `conductor.log`, `meta.json`. The worker's
 * own `worktree/` and the reviewers' `candidates/` checkouts are git trees,
 * not conductor output, and are left alone. Returns how many files changed. */
export function redactRunDir(runDir: string, secrets: readonly Secret[]): number {
  if (secrets.length === 0) return 0;
  const targets: Array<{ file: string; jsonl: boolean }> = [
    { file: path.join(runDir, "events.jsonl"), jsonl: true },
    { file: path.join(runDir, "conductor.log"), jsonl: false },
    { file: path.join(runDir, "meta.json"), jsonl: false },
  ];
  const JSONL_DIRS = new Set(["stream", "sessions"]);
  for (const dir of ["plan", "stream", "sessions", "checks", "refs", "views"]) {
    for (const file of filesUnder(path.join(runDir, dir))) targets.push({ file, jsonl: JSONL_DIRS.has(dir) });
  }
  let changed = 0;
  for (const t of targets) if (redactFileInPlace(t.file, secrets, t.jsonl)) changed += 1;
  return changed;
}

/** The prompt lines naming a run's declared secrets, for every agent (the
 * worker and all three reviewers). Names only, never a value. */
export function secretPromptLines(names: readonly string[]): string[] {
  if (names.length === 0) return [];
  const dollars = names.map((n) => `$${n}`).join(", ");
  return [
    "",
    `Secrets: this plan declares ${names.join(", ")}. Each value is already in your environment: reference it as ` +
      `$NAME — here ${dollars} — in every command; never paste a value into a command, an edit, a file or a ` +
      "submission, and never repeat one back. The conductor refuses a command that contains one.",
  ];
}

/** The names the run's plan snapshot declares, for a reader that has only
 * the run directory (`tt redact`, the CLI views). */
export function planSecretNames(runDir: string): string[] {
  try {
    const plan = JSON.parse(fs.readFileSync(path.join(runDir, "plan", "v1.json"), "utf8")) as { secrets?: string[] };
    return secretNames(plan.secrets);
  } catch {
    return [];
  }
}

/** The declared secrets the conductor found unset when it started this run
 * (the `secrets` record it logs then), for `tt status`. Read from the log so
 * a later `tt status` reports what the *run* saw, not what this shell has. */
export function loggedMissingSecrets(runDir: string): string[] {
  try {
    const { records } = readLog(path.join(runDir, "events.jsonl"));
    for (let i = records.length - 1; i >= 0; i--) {
      if (records[i].kind !== "secrets") continue;
      const missing = (records[i].event as { missing?: unknown }).missing;
      return Array.isArray(missing) ? missing.filter((n): n is string => typeof n === "string") : [];
    }
  } catch {
    // An unreadable log is not this view's problem.
  }
  return [];
}
