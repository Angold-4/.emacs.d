// Environment preflight (plan 05i): before the conductor takes a baseline or
// launches any agent, and before `tt program start|resume|retry` creates a
// run, resolve the executable of every shell command the phase declares —
// each check, the `:GATE:` command and the `:GATE_CLEANUP:` companion — in the
// caller's own PATH. A missing executable is an environment problem, not a
// code failure: the run stops in `ENV_BLOCKED` with the missing names and the
// PATH, and the check is never written as a baseline, reused by a sibling,
// counted as `checks failed`, or turned into a repair or a finding.
//
// This module is PURE: it parses a shell command string into the first word of
// every simple command (skipping shell builtins and variable assignments) and
// resolves those words with an injected `resolve`. It never spawns anything;
// the effect layer (`conductor.ts`, `cli.ts`) supplies `command -v`.

import type { EnvBlockInfo, EnvTool } from "./types.ts";

/**
 * Shell builtins and reserved words whose "executable" is the shell itself.
 * `command -v cd` would print `cd`, but `cd` is never a tool that can be
 * missing from PATH, so preflighting it would be a false positive. The list is
 * deliberately broad: a command that is actually a small program on PATH
 * (`kill`, `printf`) still resolves harmlessly whether or not it is skipped,
 * while a keyword (`if`, `then`, `fi`) skipped here can never block a run.
 */
export const SHELL_BUILTINS = new Set<string>([
  // POSIX special builtins
  ":", ".", "break", "continue", "eval", "exec", "exit", "export", "readonly",
  "return", "set", "shift", "times", "trap", "unset",
  // POSIX regular builtins
  "alias", "bg", "cd", "command", "false", "fc", "fg", "getopts", "hash",
  "jobs", "kill", "pwd", "read", "true", "type", "ulimit", "umask", "unalias",
  "wait", "test", "[", "]",
  // Common shell builtins / reserved words
  "printf", "let", "local", "declare", "typeset", "source", "builtin",
  "if", "then", "else", "elif", "fi", "while", "until", "for", "do", "done",
  "case", "esac", "in", "{", "}", "!", "select", "function", "time",
]);

/** `VAR=value`, a leading variable assignment (its value may be a command
 * substitution, which is why the whole first token is skipped). */
const ASSIGNMENT = /^[A-Za-z_][A-Za-z0-9_]*=/;

/** Reserved words that may precede a command in a shell compound statement
 * (`then sleep 1`, `do echo x`, `while [ ... ]`). They are skipped like a
 * leading assignment so the command they introduce is still resolved; a
 * segment that opens with `for`/`select`/`case` names no executable at all
 * (the command is in the body), so it yields nothing. */
const SHELL_KEYWORDS = new Set<string>([
  "if", "then", "else", "elif", "fi", "while", "until", "for", "do",
  "done", "case", "esac", "in", "select", "function", "time", "{", "}",
]);

/** A plausible executable word: a bare name, a relative or absolute path
 * (`/usr/bin/foo`, `./gradlew`, `~/bin/x`) or a name with an extension.
 * Anything starting with `$`, a redirection, a parenthesis or a quote after
 * tokenizing is not an executable this preflight can resolve and is ignored
 * (never reported missing). */
const COMMAND_WORD = /^[A-Za-z0-9_~./][A-Za-z0-9_./:+-]*$/;

/** A file-descriptor redirection: `>`, `2>`, `2>&1`, `>&2`, `&>file`, `2>>`,
 * `<`, `<<`, `<>`, … The leading fd is optional. A token beginning with one
 * of these is not a command word. */
const REDIRECTION = /^(?:[0-9]*(?:>>?|<<?|<>)|&>>?)/;

function isRedirection(token: string): boolean {
  return REDIRECTION.test(token);
}

/** Split a shell expression on its control operators: `&&`, `||`, `;`, `|`,
 * a newline and a LONE `&`. A `&` that belongs to a redirection (`2>&1`,
 * `>&2`, `&>file`, `2<&0`) is not a separator, so `cargo test 2>&1` is one
 * simple command whose first word is `cargo` — never `1`. Quotes are
 * respected. */
function splitSimpleCommands(command: string): string[] {
  const segments: string[] = [];
  let current = "";
  let quote: '"' | "'" | null = null;
  for (let i = 0; i < command.length; i++) {
    const c = command[i];
    if (quote) {
      current += c;
      if (c === quote) quote = null;
      continue;
    }
    if (c === '"' || c === "'") {
      quote = c;
      current += c;
      continue;
    }
    if ((c === "&" && command[i + 1] === "&") || (c === "|" && command[i + 1] === "|")) {
      segments.push(current);
      current = "";
      i += 1;
      continue;
    }
    if (c === ";" || c === "|" || c === "\n") {
      segments.push(current);
      current = "";
      continue;
    }
    if (c === "&") {
      // A redirection's ampersand: the previous non-space character is a
      // redirection operator (`2>&1`, `>&2`) or the next character is `>`
      // (`&>file`, `&>>file`). Anything else is a lone `&` (background).
      let prev = i - 1;
      while (prev >= 0 && /\s/.test(command[prev])) prev -= 1;
      if (command[i + 1] === ">" || command[prev] === ">" || command[prev] === "<") {
        current += c;
        continue;
      }
      segments.push(current);
      current = "";
      continue;
    }
    current += c;
  }
  segments.push(current);
  return segments;
}

/** Tokenize one simple command on whitespace, honouring single and double
 * quotes so `'a b'` is one token. Only the first tokens matter here. */
function tokenize(segment: string): string[] {
  const out: string[] = [];
  let current = "";
  let quote: '"' | "'" | null = null;
  for (const char of segment) {
    if (quote) {
      if (char === quote) quote = null;
      else current += char;
      continue;
    }
    if (char === '"' || char === "'") {
      quote = char;
      continue;
    }
    if (/\s/.test(char)) {
      if (current.length > 0) {
        out.push(current);
        current = "";
      }
      continue;
    }
    current += char;
  }
  if (current.length > 0) out.push(current);
  return out;
}

/** The first word of one simple command: leading variable assignments,
 * reserved words and a leading `!` are skipped, then the token that would be
 * executed. An assignment whose value begins a command substitution
 * (`n=$(cat x ...)`) has no single token for its command word, so the whole
 * segment is skipped rather than mistaking a substitution argument (`x`) for
 * an executable. */
function firstCommandWord(segment: string): string | undefined {
  const tokens = tokenize(segment);
  let i = 0;
  while (i < tokens.length && ASSIGNMENT.test(tokens[i])) {
    if (tokens[i].includes("$(") || tokens[i].includes("`")) return undefined;
    i += 1;
  }
  while (i < tokens.length && (tokens[i] === "!" || SHELL_KEYWORDS.has(tokens[i]))) {
    // A `for`/`select` header names its loop variable(s) and `case` names the
    // word under test; the executable is in the body, so the segment yields
    // nothing (never the loop variable `f`).
    if (tokens[i] === "for" || tokens[i] === "select" || tokens[i] === "case") return undefined;
    i += 1;
  }
  if (i >= tokens.length) return undefined;
  // A segment that begins with a redirection has no command word before it.
  if (isRedirection(tokens[i])) return undefined;
  return tokens[i];
}

/** Every executable a shell command string would resolve, in first-seen
 * order, deduped. Shell builtins, keywords, variable assignments and
 * non-command tokens (`$VAR`, redirections, `(...)`) are skipped, so a
 * command like `n=$(cat x || echo 0); if [ $n -ge 3 ]; then sleep 1; fi`
 * yields only `cat`, `echo` and `sleep`. */
export function commandExecutables(command: string): string[] {
  const out: string[] = [];
  const seen = new Set<string>();
  for (const segment of splitSimpleCommands(command)) {
    const word = firstCommandWord(segment);
    if (word === undefined) continue;
    if (SHELL_BUILTINS.has(word)) continue;
    if (isRedirection(word)) continue;
    if (!COMMAND_WORD.test(word)) continue;
    // A bare file-descriptor number (`1` from a mis-split `2>&1`, or `0`) is
    // never a command, even though it matches COMMAND_WORD.
    if (/^[0-9]+$/.test(word)) continue;
    if (seen.has(word)) continue;
    seen.add(word);
    out.push(word);
  }
  return out;
}

export interface EnvPreflightOptions {
  /** The PATH being resolved against, recorded verbatim when a tool is
   * missing so the owner sees the environment the run actually had. */
  path: string;
  /** Resolve one executable name to its absolute path, or undefined when it
   * is not on PATH. The effect layer supplies `command -v`. */
  resolve: (name: string) => string | undefined;
}

export interface EnvPreflight {
  path: string;
  tools: EnvTool[];
  missing: string[];
}

/** Resolve every executable of every declared shell command. */
export function envPreflight(commands: readonly string[], opts: EnvPreflightOptions): EnvPreflight {
  const tools: EnvTool[] = [];
  const missing: string[] = [];
  const seen = new Set<string>();
  for (const command of commands) {
    for (const name of commandExecutables(command)) {
      if (seen.has(name)) continue;
      seen.add(name);
      const resolved = opts.resolve(name);
      tools.push({ name, ...(resolved ? { path: resolved } : {}) });
      if (!resolved) missing.push(name);
    }
  }
  return { path: opts.path, tools, missing };
}

/** The one-line, owner-facing reason a run is environment-blocked: the
 * visible form `env blocked · cargo not found on PATH (/usr/bin:/bin)`, or,
 * for a 126/127 at a check, the command and exit. */
export function envBlockedLine(blocked: EnvBlockInfo): string {
  if (blocked.kind === "preflight") {
    const names = (blocked.missing ?? []).join(", ");
    const path = blocked.path ?? "";
    return `env blocked · ${names} not found on PATH (${path})`;
  }
  const code = blocked.exitCode === null || blocked.exitCode === undefined ? "on a signal" : `exit ${blocked.exitCode}`;
  return `env blocked · ${blocked.command ?? "check"} ${code} — the tool is not available here`;
}

/** The status rows naming each tool the preflight resolved, e.g.
 * `env  cargo /Users/x/.cargo/bin/cargo`. */
export function envToolsLines(tools: readonly EnvTool[] | undefined): string[] {
  return (tools ?? []).map((t) => `env  ${t.name} ${t.path ?? "(not found)"}`);
}
