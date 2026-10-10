// Plan 06j (A1): read the repository facts the coverage lint needs.
//
// `checkCoverageWarnings` is pure (it receives a `RepoFacts`); this module is
// the I/O half that builds one from a repository directory. It reads the
// cargo workspace members and the GitHub Actions workflow commands. Anything
// unreadable or unrecognized yields an empty fact set, never a thrown error —
// a lint must not fail because a repository has an unusual layout.

import * as fs from "node:fs";
import * as path from "node:path";

import { emptyRepoFacts, type RepoFacts, type RepoPackage } from "./plan-lint.ts";

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
 * directory itself; a trailing `/*` lists the parent's subdirectories. */
function expandMember(root: string, member: string): string[] {
  const clean = member.replace(/\/+$/, "");
  if (!clean.includes("*")) return isDirectory(path.join(root, clean)) ? [clean] : [];
  const parent = clean.slice(0, clean.lastIndexOf("/"));
  const parentDir = path.join(root, parent);
  let entries: string[] = [];
  try {
    entries = fs.readdirSync(parentDir, { withFileTypes: true }).filter((e) => e.isDirectory()).map((e) => e.name);
  } catch {
    return [];
  }
  return entries.map((name) => (parent ? `${parent}/${name}` : name));
}

/** Every package/crate the repository defines: the root package when the root
 * Cargo.toml has a `[package]` table, plus its workspace members. There is NO
 * basename fallback: without a `[package]` header the root is never a package
 * (OD-11/OD-12), so a virtual manifest (even with empty members) yields no
 * root package, and a member with no package name is skipped. */
function readPackages(root: string): RepoPackage[] {
  const rootToml = readFileOrUndefined(path.join(root, "Cargo.toml"));
  if (rootToml === undefined) return [];
  const out: RepoPackage[] = [];
  const rootName = cargoPackageName(rootToml);
  if (rootName !== undefined && rootName.length > 0) out.push({ name: rootName, dir: "." });
  for (const dir of cargoMembers(rootToml).flatMap((m) => expandMember(root, m))) {
    const toml = readFileOrUndefined(path.join(root, dir, "Cargo.toml"));
    if (toml === undefined) continue;
    const name = cargoPackageName(toml);
    if (name === undefined || name.length === 0) continue;
    out.push({ name, dir });
  }
  return out;
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
    return { packages: readPackages(root), ciCommands: readCiCommands(root), cargo };
  } catch {
    return emptyRepoFacts();
  }
}
