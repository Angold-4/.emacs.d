// Plan 06j (A1): read the repository facts the coverage lint needs.
//
// `checkCoverageWarnings` is pure (it receives a `RepoFacts`); this module is
// the I/O half that builds one from a repository directory. It reads the
// cargo workspace members and the GitHub Actions workflow commands. Anything
// unreadable or unrecognized yields an empty fact set, never a thrown error —
// a lint must not fail because a repository has an unusual layout.

import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";

import { parseVerify } from "./items.ts";
import {
  emptyRepoFacts,
  type LintPlanInput,
  type RepoFacts,
  type RepoPackage,
  type TestSelection,
  type VerifyReachFacts,
  type VerifySite,
} from "./plan-lint.ts";

/** The body of a Cargo.toml table (`[name]`), up to the next table header. A
 * line whose first non-space character is `[` starts a new table. */
function tomlTable(text: string, name: string): string | undefined {
  const header = new RegExp(`^\\s*\\[${name}\\]\\s*(?:#.*)?$`, "m");
  const m = header.exec(text);
  if (!m) return undefined;
  const rest = text.slice(m.index + m[0].length);
  const next = rest.search(/^\s*\[/m);
  return next === -1 ? rest : rest.slice(0, next);
}

/** The package name from a Cargo.toml's `[package]` table, unquoted. A
 * `name =` under any other table (`[workspace.package]`, `[workspace.metadata]`,
 * a dependency table) is not a package (E-76/M-27). */
function cargoPackageName(text: string): string | undefined {
  const body = tomlTable(text, "package");
  if (body === undefined) return undefined;
  const raw = body.match(/^\s*name\s*=\s*(.+)$/m)?.[1]?.trim();
  if (!raw) return undefined;
  const m = raw.match(/^"([^"]+)"|^'([^']+)'/);
  return m ? (m[1] ?? m[2]) : raw.replace(/[",]/g, "").trim() || undefined;
}

/** A `members = ["a", "b"]` array, single- or multi-line. */
function cargoMembers(text: string): string[] {
  const key = text.search(/^\s*members\s*=/m);
  if (key === -1) return [];
  // Take everything from the `[` after the key to the first `]`, so a
  // single-line and a multi-line array both parse.
  const open = text.indexOf("[", key);
  const close = open === -1 ? -1 : text.indexOf("]", open);
  if (open === -1 || close === -1) return [];
  const array = text.slice(open, close + 1);
  return [...array.matchAll(/"([^"]+)"|'([^']+)'/g)].map((m) => m[1] ?? m[2]);
}

function readFileOrUndefined(file: string): string | undefined {
  try {
    return fs.readFileSync(file, "utf8");
  } catch {
    return undefined;
  }
}

function isDirectory(p: string): boolean {
  try {
    return fs.statSync(p).isDirectory();
  } catch {
    return false;
  }
}

/** Expand one workspace member pattern. A pattern without a wildcard is the
 * directory itself (even when it is missing, so `readPackages` can warn); a
 * trailing `/*` lists the parent's subdirectories. A parent that cannot be
 * read is surfaced (OD-18/M-97): an enumeration failure is not an empty
 * workspace. */
function expandMember(root: string, member: string, issues: string[]): string[] {
  const clean = member.replace(/\/+$/, "");
  if (!clean.includes("*")) return [clean];
  const parent = clean.slice(0, clean.lastIndexOf("/"));
  const parentDir = path.join(root, parent);
  let entries: string[] = [];
  try {
    entries = fs.readdirSync(parentDir, { withFileTypes: true }).filter((e) => e.isDirectory()).map((e) => e.name);
  } catch {
    issues.push(`cannot read ${parent.length > 0 ? parent : "."} to expand ${member}`);
    return [];
  }
  return entries.map((name) => (parent ? `${parent}/${name}` : name));
}

/** Every package/crate the repository defines: the root package when the root
 * Cargo.toml has a `[package]` table, plus its workspace members. There is NO
 * basename fallback: without a `[package]` header the root is never a package
 * (OD-11/OD-12), so a virtual manifest (even with empty members) yields no
 * root package, and a member with no package name is skipped. */
function readPackages(root: string): { packages: RepoPackage[]; issues: string[] } {
  const rootToml = readFileOrUndefined(path.join(root, "Cargo.toml"));
  if (rootToml === undefined) return { packages: [], issues: [] };
  const packages: RepoPackage[] = [];
  const issues: string[] = [];
  const rootName = cargoPackageName(rootToml);
  if (rootName !== undefined && rootName.length > 0) packages.push({ name: rootName, dir: "." });
  for (const dir of cargoMembers(rootToml).flatMap((m) => expandMember(root, m, issues))) {
    const toml = readFileOrUndefined(path.join(root, dir, "Cargo.toml"));
    if (toml === undefined) {
      // M-93: a listed member with no readable manifest is surfaced, never
      // skipped — a crate the lint cannot judge must not be a quiet green.
      issues.push(`the workspace member ${dir} has no readable Cargo.toml`);
      continue;
    }
    // A nested virtual manifest (no [package] table) is not a crate: the ONLY
    // branch skipped silently (OD-16/OD-18).
    if (tomlTable(toml, "package") === undefined) continue;
    const name = cargoPackageName(toml);
    if (name === undefined || name.length === 0) {
      issues.push(`the workspace member ${dir} manifest has [package] but no readable name`);
      continue;
    }
    packages.push({ name, dir });
  }
  return { packages, issues };
}

/** Decode a YAML double-quoted scalar body: `\"`/`\n`/`\t`/`\\` escapes. */
function decodeDoubleQuoted(body: string): string {
  let out = "";
  for (let i = 0; i < body.length; i++) {
    const c = body[i];
    if (c === "\\" && i + 1 < body.length) {
      const n = body[i + 1];
      out += n === "n" ? "\n" : n === "t" ? "\t" : n === '"' ? '"' : n === "\\" ? "\\" : n;
      i += 1;
    } else {
      out += c;
    }
  }
  return out;
}

/** Decode one YAML scalar the way GitHub's `run:` values use it: plain,
 * single-quoted (`''` is an escaped `'`) or double-quoted. */
function decodeYamlScalar(raw: string): string {
  const value = raw.trim();
  if (value.length >= 2 && value.startsWith("'") && value.endsWith("'")) {
    return value.slice(1, -1).replace(/''/g, "'");
  }
  if (value.length >= 2 && value.startsWith('"') && value.endsWith('"')) {
    return decodeDoubleQuoted(value.slice(1, -1));
  }
  return value;
}

/** Decode a YAML block scalar (`|`/`>` with optional `-`/`+` chomping): strip
 * the block's common indentation; literal keeps line breaks, folded joins
 * lines with spaces. */
function decodeBlock(lines: readonly string[], folded: boolean): string {
  const nonEmpty = lines.filter((l) => l.trim().length > 0);
  const minIndent = nonEmpty.length > 0 ? Math.min(...nonEmpty.map((l) => l.match(/^(\s*)/)![1].length)) : 0;
  const stripped = lines.map((l) => l.slice(Math.min(minIndent, l.length)));
  return (folded ? stripped.join(" ") : stripped.join("\n")).trim();
}

/** Every command a GitHub Actions workflow runs. A `run:` value is decoded
 * the way YAML does for the forms workflows use: a plain scalar, a single- or
 * double-quoted scalar, or a `|`/`>` block (OD-15). */
function readCiCommands(root: string): string[] {
  const dir = path.join(root, ".github", "workflows");
  let files: string[] = [];
  try {
    files = fs.readdirSync(dir).filter((f) => f.endsWith(".yml") || f.endsWith(".yaml"));
  } catch {
    return [];
  }
  const out: string[] = [];
  for (const file of files) {
    const text = readFileOrUndefined(path.join(dir, file));
    if (text === undefined) continue;
    const lines = text.split("\n");
    for (let i = 0; i < lines.length; i++) {
      const m = /^(\s*)(?:-\s*)?run:\s*(.*)$/.exec(lines[i]);
      if (!m) continue;
      const indent = m[1].length;
      const value = m[2].trim();
      const block = /^([|>])([-+]?)\s*$/.exec(value);
      if (block) {
        const collected: string[] = [];
        for (let j = i + 1; j < lines.length; j++) {
          const line = lines[j];
          if (line.trim().length === 0) {
            collected.push("");
            continue;
          }
          if (line.match(/^(\s*)/)![1].length <= indent) break;
          collected.push(line);
        }
        const command = decodeBlock(collected, block[1] === ">");
        if (command.length > 0) out.push(command);
      } else if (value.length > 0) {
        out.push(decodeYamlScalar(value));
      }
    }
  }
  return out;
}

/** Build the `RepoFacts` `checkCoverageWarnings` reads. Never throws: an
 * unreadable directory or file yields an empty fact set. */
export function readRepoFacts(root: string): RepoFacts {
  try {
    if (!isDirectory(root)) return emptyRepoFacts();
    const cargo = fs.existsSync(path.join(root, "Cargo.toml"));
    const { packages, issues } = readPackages(root);
    return { packages, ciCommands: readCiCommands(root), cargo, ...(issues.length > 0 ? { memberIssues: issues } : {}) };
  } catch {
    return emptyRepoFacts();
  }
}

// ---------------------------------------------------------------------------
// Verify reach (06k1's lesson): where each named test is defined, and what a
// phase's checks run. Anything the walker cannot resolve marks that kind
// "unknown", so the pure rule stays quiet instead of guessing.
// ---------------------------------------------------------------------------

/** Files tracked by git that match `pattern` (`git grep -l`), repo-relative.
 * No match, or no git, is an empty list. */
function gitGrepFiles(root: string, args: string[], pathspecs: string[]): string[] {
  try {
    const out = execFileSync("git", ["-C", root, "grep", "-l", ...args, "--", ...pathspecs], {
      encoding: "utf8",
      stdio: ["ignore", "pipe", "ignore"],
      maxBuffer: 16 * 1024 * 1024,
    });
    return out.split("\n").filter((l) => l.length > 0);
  } catch {
    return [];
  }
}

const NODE_TEST_FILES = ["*.test.ts", "*.test.js", "*.test.mjs", "*.spec.ts", "*.spec.js"];

/** Where a named test is defined: a node test file that contains the name as
 * written, or a Rust `fn <last path segment>` (owned by the longest package
 * directory that contains it). */
function testSites(root: string, name: string, packages: readonly RepoPackage[]): VerifySite[] {
  const sites: VerifySite[] = gitGrepFiles(root, ["-F", "-e", name], NODE_TEST_FILES).map((file) => ({ file, kind: "node" as const }));
  const fn = name.split("::").pop() ?? name;
  if (packages.length > 0 && /^[A-Za-z_][A-Za-z0-9_]*$/.test(fn)) {
    for (const file of gitGrepFiles(root, ["-E", "-e", `fn[[:space:]]+${fn}[[:space:]]*[(<]`], ["*.rs"])) {
      const owner = packages
        .filter((p) => p.dir === "." || file === p.dir || file.startsWith(`${p.dir}/`))
        .sort((a, b) => b.dir.length - a.dir.length)[0];
      sites.push({ file, kind: "cargo", ...(owner ? { pkg: owner.name } : {}) });
    }
  }
  return sites;
}

/** Shell-ish words: quotes group and are removed; nothing is expanded. */
function shellWords(text: string): string[] {
  const out: string[] = [];
  let cur = "";
  let quote: string | undefined;
  let any = false;
  for (const c of text) {
    if (quote) {
      if (c === quote) quote = undefined;
      else cur += c;
    } else if (c === '"' || c === "'") {
      quote = c;
      any = true;
    } else if (/\s/.test(c)) {
      if (cur.length > 0 || any) out.push(cur);
      cur = "";
      any = false;
    } else {
      cur += c;
    }
  }
  if (cur.length > 0 || any) out.push(cur);
  return out;
}

/** One command line's segments (`&&`, `||`, `;`, `|`, newlines), quotes kept. */
function shellSegments(line: string): string[] {
  const out: string[] = [];
  let cur = "";
  let quote: string | undefined;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (quote) {
      if (c === quote) quote = undefined;
      cur += c;
    } else if (c === '"' || c === "'") {
      quote = c;
      cur += c;
    } else if (c === ";" || c === "\n" || (c === "|" && line[i + 1] !== "|") || ((c === "&" || c === "|") && line[i + 1] === c)) {
      if (c !== ";" && c !== "\n" && line[i + 1] === c) i++;
      out.push(cur);
      cur = "";
    } else {
      cur += c;
    }
  }
  out.push(cur);
  return out.map((s) => s.trim()).filter((s) => s.length > 0);
}

interface MakeRule {
  prereqs: string[];
  recipe: string[];
}

/** A Makefile's explicit rules (target → prerequisites and recipe lines) and
 * its default goal. Pattern rules, variables and includes are not modelled;
 * a recipe that needs them resolves to "unknown" when walked. */
function readMakefile(dir: string): { rules: Map<string, MakeRule>; first?: string } | undefined {
  const text = ["GNUmakefile", "makefile", "Makefile"].map((n) => readFileOrUndefined(path.join(dir, n))).find((t) => t !== undefined);
  if (text === undefined) return undefined;
  const rules = new Map<string, MakeRule>();
  let first: string | undefined;
  let current: MakeRule[] = [];
  for (const line of text.split("\n")) {
    if (line.startsWith("\t")) {
      for (const r of current) r.recipe.push(line.slice(1));
      continue;
    }
    const m = /^([A-Za-z0-9_.\-\/ ]+?)\s*:(?![=:])\s*(.*)$/.exec(line);
    if (!m) {
      if (line.trim().length > 0 && !line.trim().startsWith("#")) current = [];
      continue;
    }
    current = [];
    for (const target of m[1].split(/\s+/).filter(Boolean)) {
      if (target.startsWith(".")) continue;
      const rule = rules.get(target) ?? { prereqs: [], recipe: [] };
      rule.prereqs.push(...m[2].replace(/#.*/, "").split(/\s+/).filter(Boolean));
      rules.set(target, rule);
      current.push(rule);
      first ??= target;
    }
  }
  return { rules, first };
}

const IGNORED_COMMANDS = new Set([
  "cd", "echo", "printf", "test", "[", "true", "false", "exit", "export", "set", "unset", "mkdir", "rm", "cp", "mv",
  "cat", "git", "emacs", "rustfmt", "sleep", "env", "command", "type", "which",
]);
const NODE_RUNNERS = new Set(["npm", "npx", "pnpm", "yarn", "bun", "deno", "vitest", "jest", "mocha"]);
const NODE_VALUE_FLAGS = new Set(["--test-name-pattern", "--test-skip-pattern", "--test-reporter", "--test-reporter-destination", "--import", "--require", "-r", "--loader"]);

/** Walk one command line from `cwd`, adding what it runs to `sel`; `depth`
 * bounds recursion through make. */
function walkCommand(root: string, line: string, cwd: string, sel: TestSelection, depth: number): void {
  if (depth > 8) {
    sel.nodeUnknown = true;
    sel.cargoUnknown = true;
    return;
  }
  for (const segment of shellSegments(line)) {
    let words = shellWords(segment);
    while (words.length > 0 && (words[0] === "{" || words[0] === "(" || words[0] === "}" || words[0] === ")" || words[0] === "!")) words = words.slice(1);
    while (words.length > 0 && /^[A-Za-z_][A-Za-z0-9_]*=/.test(words[0])) words = words.slice(1);
    if (words.length === 0) continue;
    const cmd = words[0].replace(/^[@-]+/, "");
    if (cmd === "cd") {
      if (words[1] !== undefined) cwd = path.resolve(cwd, words[1]);
      continue;
    }
    if (cmd === "make" || cmd === "$(MAKE)" || cmd === "gmake") {
      walkMake(root, words.slice(1), cwd, sel, depth);
      continue;
    }
    if (cmd === "node" && words.includes("--test")) {
      const files: string[] = [];
      for (let i = 1; i < words.length; i++) {
        const w = words[i];
        if (NODE_VALUE_FLAGS.has(w)) {
          i++;
          continue;
        }
        if (w.startsWith("-")) continue;
        files.push(w);
      }
      if (files.length === 0 || files.some((f) => f.includes("$"))) sel.nodeUnknown = true;
      else for (const f of files) sel.nodeFiles.push(path.relative(root, path.resolve(cwd, f)));
      continue;
    }
    if (cmd === "cargo" || cmd === "cargo-nextest") {
      let i = 1;
      if (words[i]?.startsWith("+")) i++;
      const sub = cmd === "cargo-nextest" ? "nextest" : words[i];
      if (sub !== "test" && sub !== "nextest") continue;
      const args = words.slice(i + 1);
      if (args.some((a) => a.startsWith("--manifest-path"))) {
        sel.cargoUnknown = true;
        continue;
      }
      const pkgs: string[] = [];
      for (let j = 0; j < args.length; j++) {
        const a = args[j];
        if ((a === "-p" || a === "--package") && args[j + 1] !== undefined) pkgs.push(args[++j]);
        else if (a.startsWith("--package=")) pkgs.push(a.slice("--package=".length));
        else if (a.startsWith("-p") && a.length > 2) pkgs.push(a.slice(2).replace(/^=/, ""));
      }
      if (pkgs.length === 0 || args.includes("--workspace") || args.includes("--all")) sel.cargo = "all";
      else if (sel.cargo !== "all") sel.cargo.push(...pkgs);
      continue;
    }
    if (NODE_RUNNERS.has(cmd)) {
      sel.nodeUnknown = true;
      continue;
    }
    if (cmd === "cargo" || IGNORED_COMMANDS.has(cmd)) continue;
    // A script or any other program could run anything.
    sel.nodeUnknown = true;
    sel.cargoUnknown = true;
  }
}

/** `make [-C dir] [VAR=value] [target…]`: walk each target's prerequisites
 * and recipe in the Makefile's directory, with `$(CURDIR)` and the call's
 * variables substituted. An unreadable Makefile or target is unknown. */
function walkMake(root: string, args: string[], cwd: string, sel: TestSelection, depth: number): void {
  let dir = cwd;
  const vars: Record<string, string> = {};
  const targets: string[] = [];
  for (let i = 0; i < args.length; i++) {
    const a = args[i];
    if (a === "-C" && args[i + 1] !== undefined) dir = path.resolve(dir, args[++i]);
    else if (a.startsWith("-C") && a.length > 2) dir = path.resolve(dir, a.slice(2));
    else if (/^[A-Za-z_][A-Za-z0-9_]*=/.test(a)) vars[a.slice(0, a.indexOf("="))] = a.slice(a.indexOf("=") + 1);
    else if (!a.startsWith("-")) targets.push(a);
  }
  const make = readMakefile(dir);
  if (!make) {
    sel.nodeUnknown = true;
    sel.cargoUnknown = true;
    return;
  }
  const seen = new Set<string>();
  const visit = (target: string): void => {
    if (seen.has(target)) return;
    seen.add(target);
    const rule = make.rules.get(target);
    if (!rule) {
      // A file prerequisite (no rule) runs nothing; an unknown goal is unknown.
      if (targets.includes(target)) {
        sel.nodeUnknown = true;
        sel.cargoUnknown = true;
      }
      return;
    }
    for (const p of rule.prereqs) visit(p);
    for (const raw of rule.recipe) {
      const line = raw.replace(/\$[({]([A-Za-z_][A-Za-z0-9_]*)[)}]/g, (whole, name: string) =>
        name === "CURDIR" ? dir : name === "MAKE" ? "make" : (vars[name] ?? whole),
      );
      walkCommand(root, line.replace(/^\s*[@-]+/, ""), dir, sel, depth + 1);
    }
  };
  const goals = targets.length > 0 ? targets : make.first ? [make.first] : [];
  if (goals.length === 0) {
    sel.nodeUnknown = true;
    sel.cargoUnknown = true;
  }
  for (const g of goals) visit(g);
}

/** What a phase's checks run, resolved from the repository root. */
export function readTestSelection(root: string, commands: readonly string[]): TestSelection {
  const sel: TestSelection = { nodeFiles: [], nodeUnknown: false, cargo: [], cargoUnknown: false };
  for (const c of commands) walkCommand(root, c, root, sel, 0);
  return sel;
}

/** Build the facts `checkVerifyReach` reads for one plan. Never throws: no
 * repository, or anything unreadable, yields facts that judge nothing. */
export function readVerifyReachFacts(plan: LintPlanInput): VerifyReachFacts {
  const facts: VerifyReachFacts = { sites: {}, selections: {} };
  const root = plan.repo;
  try {
    if (!root || !isDirectory(root)) return facts;
    const { packages } = readPackages(root);
    for (const phase of plan.phases ?? []) {
      const names = [...(phase.requirements ?? []), ...(phase.constraints ?? [])]
        .flatMap((i) => (i.verify ?? []).flatMap((v) => parseVerify(v)))
        .flatMap((v) => (v.kind === "test" && v.name.length > 0 && !v.file ? [v.name] : []));
      if (names.length === 0) continue;
      facts.selections[phase.id ?? ""] = readTestSelection(root, phase.checks ?? []);
      for (const name of names) facts.sites[name] ??= testSites(root, name, packages);
    }
  } catch {
    return { sites: {}, selections: {} };
  }
  return facts;
}
