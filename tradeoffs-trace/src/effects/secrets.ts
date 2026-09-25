// Secrets (design §7: "never leak a credential"; atlas plan 13 §7: 721
// occurrences of vendor keys in one program's stream files, the Emacs trace
// and the refs/ copies of the vendor docs).
//
// A plan declares its secrets by NAME (`#+TT_SECRETS: NAME1 NAME2`, parsed by
// Emacs into the JSON plan's `secrets`). The conductor resolves each value
// from *its own* environment, passes it to every agent's environment, and
// replaces every value with `***NAME***` in everything it writes — and, more
// importantly, at the one boundary where agent text enters the conductor's
// in-memory state (see `conductor.ts`'s `#applyEvent`), because prompts are
// built from that state and a value must never reach a prompt either.
//
// Two shapes of "the value is still there" this module refuses to report as
// success:
//  - a value that OVERLAPS another (`A_KEY=sk-live`, `AB_KEY=sk-live-abcd`):
//    the longer value is masked first, so masking one cannot leave a suffix
//    of the other;
//  - a value that is too SHORT to mask safely (see MIN_SECRET_LENGTH): masking
//    it would rewrite unrelated text (`1` inside every id and timestamp), so
//    it is reported instead of used.
//
// The replacement text (`***NAME***`) contains no quote, backslash or
// newline, so it can never break a JSON line (or a JSONL file) that a value
// was redacted out of.

import * as fs from "node:fs";
import * as path from "node:path";

import { readLog } from "./log.ts";

export interface Secret {
  name: string;
  /** The value, read once from the conductor's own environment. */
  value: string;
}

/** A value shorter than this cannot be masked safely: replacing every
 * occurrence of `1` (a declared secret named TT exported as "1") would rewrite
 * every id, timestamp and count in the log, so such a value is reported in the
 * status instead of being used (plan: an unusable declaration must be visible,
 * and the run still runs). */
export const MIN_SECRET_LENGTH = 4;

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
 * is never carried anywhere but the environment), and which are declared but
 * unusable. `values` is every set secret, for the agents' environment;
 * `maskable` is the subset long enough to mask and to match in a command (see
 * MIN_SECRET_LENGTH); `missing` (unset) and `tooShort` are what a status line
 * reports. The run starts in every case. */
export function resolveSecrets(
  names: readonly string[],
  env: NodeJS.ProcessEnv = process.env,
): { values: Secret[]; maskable: Secret[]; missing: string[]; tooShort: string[] } {
  const values: Secret[] = [];
  const maskable: Secret[] = [];
  const missing: string[] = [];
  const tooShort: string[] = [];
  for (const name of names) {
    const value = env[name];
    if (value === undefined || value.length === 0) {
      missing.push(name);
      continue;
    }
    values.push({ name, value });
    if (value.length < MIN_SECRET_LENGTH) tooShort.push(name);
    else maskable.push({ name, value });
  }
  // Longest value first: a value that contains another (A_KEY=sk-live,
  // AB_KEY=sk-live-abcd) must be replaced before the shorter one can leave a
  // suffix of it behind.
  maskable.sort((a, b) => b.value.length - a.value.length);
  return { values, maskable, missing, tooShort };
}

/** The maskable secrets among NAMES, for a caller holding only the names (the
 * extension's guard): it resolves them from its OWN process environment, which
 * the conductor handed the values to. */
export function maskableSecrets(names: readonly string[], env: NodeJS.ProcessEnv = process.env): Secret[] {
  return resolveSecrets(names, env).maskable;
}

/** SECRETS ordered so a longer value is replaced before a shorter one it
 * contains. `resolveSecrets` already returns them in that order; this keeps a
 * hand-built list honest (and returns the array unchanged when it is already
 * ordered, so the common path allocates nothing). */
export function byLengthDesc(secrets: readonly Secret[]): readonly Secret[] {
  for (let i = 1; i < secrets.length; i++) {
    if (secrets[i - 1].value.length < secrets[i].value.length) {
      return [...secrets].sort((a, b) => b.value.length - a.value.length);
    }
  }
  return secrets;
}

/** TEXT with every secret value replaced by `***NAME***`. */
export function redactText(text: string, secrets: readonly Secret[]): string {
  let out = text;
  for (const s of byLengthDesc(secrets)) {
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
  for (const s of byLengthDesc(hits)) out = out.split(jsonEscaped(s.value)).join(placeholder(s.name));
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

/** The byte encodings a file may hold a value in: UTF-8, UTF-16LE and
 * UTF-16BE. A document saved as UTF-16 contains NUL bytes, so it is not text
 * by the usual test — and it is exactly the "vendor doc in refs/" shape the
 * evidence describes, whose value would otherwise survive every text-level
 * pass. */
const BYTE_ENCODINGS: Array<(text: string) => Buffer> = [
  (text) => Buffer.from(text, "utf8"),
  (text) => Buffer.from(text, "utf16le"),
  // Node has no "utf16be": encode little-endian and swap the pairs.
  (text) => Buffer.from(text, "utf16le").swap16(),
];

/** BUF with every occurrence of NEEDLE replaced by REPLACEMENT, or undefined
 * when NEEDLE does not occur (so a caller keeps the original buffer). */
function replaceBytes(buf: Buffer, needle: Buffer, replacement: Buffer): Buffer | undefined {
  if (needle.length === 0 || buf.indexOf(needle) === -1) return undefined;
  const parts: Buffer[] = [];
  let from = 0;
  for (;;) {
    const at = buf.indexOf(needle, from);
    if (at === -1) break;
    parts.push(buf.subarray(from, at), replacement);
    from = at + needle.length;
  }
  parts.push(buf.subarray(from));
  return Buffer.concat(parts);
}

/** BUF with every secret value replaced by `***NAME***` in UTF-8, UTF-16LE and
 * UTF-16BE. This is how a non-text file (a NUL byte: a UTF-16 document, or a
 * genuinely binary one) is handled — the value is *replaced*, not skipped, so
 * the leak the evidence describes (a vendor doc copied into refs/) cannot
 * survive a cleanup that reports success. Returns BUF itself when nothing
 * matched. */
export function redactBytes(buf: Buffer, secrets: readonly Secret[]): Buffer {
  let out = buf;
  for (const s of byLengthDesc(secrets)) {
    if (s.value.length === 0) continue;
    for (const encode of BYTE_ENCODINGS) {
      out = replaceBytes(out, encode(s.value), encode(placeholder(s.name))) ?? out;
    }
  }
  return out;
}

/** Rewrites FILE in place if it held a secret value, and reports whether it
 * changed. A JSONL file is rewritten line by line (`jsonl`); a non-text file
 * (a NUL byte) is searched in UTF-8/UTF-16 by `redactBytes`; anything else is
 * text. `opaque` is true for a non-text file in which nothing was found — the
 * caller names it, because such a file can still hold the value in an encoding
 * this module does not search. */
export function redactFileInPlace(
  file: string,
  secrets: readonly Secret[],
  jsonl: boolean,
): { changed: boolean; opaque: boolean } {
  let buf: Buffer;
  try {
    buf = fs.readFileSync(file);
  } catch {
    return { changed: false, opaque: false };
  }
  const binary = buf.includes(0);
  if (binary) {
    const next = redactBytes(buf, secrets);
    if (next.equals(buf)) return { changed: false, opaque: true };
    fs.writeFileSync(file, next);
    return { changed: true, opaque: false };
  }
  const text = buf.toString("utf8");
  const rewritten = jsonl ? redactJsonl(text, secrets) : redactText(text, secrets);
  if (rewritten === text) return { changed: false, opaque: false };
  fs.writeFileSync(file, rewritten);
  return { changed: true, opaque: false };
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

export interface RedactRunResult {
  /** Files whose bytes changed. */
  changed: number;
  /** Non-text files (a NUL byte) in which the value was NOT found: they can
   * still hold it in an encoding `redactBytes` does not search, so a caller
   * must name them rather than report an unqualified success. */
  opaque: string[];
}

/** Rewrites a run directory in place (`tt redact`): every file a secret
 * value can sit in — `events.jsonl`, `stream/*.jsonl`, `sessions/`, `checks/**`,
 * `refs/**`, `views/**`, `plan/`, `inbox/`, `conductor.log`, `meta.json`. The
 * worker's own `worktree/` and the reviewers' `candidates/` checkouts are git
 * trees, not conductor output, and are left alone. */
export function redactRunDir(runDir: string, secrets: readonly Secret[]): RedactRunResult {
  if (secrets.length === 0) return { changed: 0, opaque: [] };
  const targets: Array<{ file: string; jsonl: boolean }> = [
    { file: path.join(runDir, "events.jsonl"), jsonl: true },
    { file: path.join(runDir, "conductor.log"), jsonl: false },
    { file: path.join(runDir, "meta.json"), jsonl: false },
  ];
  const JSONL_DIRS = new Set(["stream", "sessions"]);
  for (const dir of ["plan", "stream", "sessions", "checks", "refs", "views", "inbox"]) {
    for (const file of filesUnder(path.join(runDir, dir))) targets.push({ file, jsonl: JSONL_DIRS.has(dir) });
  }
  const result: RedactRunResult = { changed: 0, opaque: [] };
  for (const t of targets) {
    const outcome = redactFileInPlace(t.file, secrets, t.jsonl);
    if (outcome.changed) result.changed += 1;
    else if (outcome.opaque) result.opaque.push(t.file);
  }
  return result;
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

/** What the conductor recorded about the run's declared secrets when it
 * started (`tt status`, `tt state`): which were unset, and which are set but
 * too short to mask safely. Names only — never a value. Read from the log so
 * a later `tt status` reports what the *run* saw, not what this shell has. */
export function loggedSecretStatus(runDir: string): { missing: string[]; tooShort: string[] } {
  const strings = (v: unknown): string[] => (Array.isArray(v) ? v.filter((n): n is string => typeof n === "string") : []);
  try {
    const { records } = readLog(path.join(runDir, "events.jsonl"));
    for (let i = records.length - 1; i >= 0; i--) {
      if (records[i].kind !== "secrets") continue;
      const event = records[i].event as { missing?: unknown; tooShort?: unknown };
      return { missing: strings(event.missing), tooShort: strings(event.tooShort) };
    }
  } catch {
    // An unreadable log is not this view's problem.
  }
  return { missing: [], tooShort: [] };
}
