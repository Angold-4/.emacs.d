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

/** One `key = value` line of a Cargo.toml, trimmed. Only the shapes the
 * coverage rule needs: `members = [...]` and `name = "..."`. */
function cargoValue(text: string, key: string): string | undefined {
  const re = new RegExp(`^\\s*${key}\\s*=\\s*(.+)$`, "m");
  const m = text.match(re);
  return m ? m[1].trim() : undefined;
}

/** A Cargo.toml `name`, unquoted. */
function cargoPackageName(text: string): string | undefined {
  const raw = cargoValue(text, "name");
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

/** Every package/crate the repository defines: the workspace members, or the
 * root package when there is no `[workspace]`. */
function readPackages(root: string): RepoPackage[] {
  const rootToml = readFileOrUndefined(path.join(root, "Cargo.toml"));
  if (rootToml === undefined) return [];
  const members = cargoMembers(rootToml);
  const dirs = members.length > 0 ? members.flatMap((m) => expandMember(root, m)) : [""];
  const out: RepoPackage[] = [];
  for (const dir of dirs) {
    const toml = dir === "" ? rootToml : readFileOrUndefined(path.join(root, dir, "Cargo.toml"));
    if (toml === undefined) continue;
    const name = cargoPackageName(toml) ?? (dir === "" ? path.basename(root) : path.basename(dir));
    if (name.length === 0) continue;
    out.push({ name, dir: dir === "" ? "." : dir });
  }
  return out;
}

/** Every command a GitHub Actions workflow runs. A `run:` scalar is the
 * command; a `run: |`/`run: >` block collects the more-indented lines. */
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
      if (/^[|>]/.test(value)) {
        const block: string[] = [];
        for (let j = i + 1; j < lines.length; j++) {
          const line = lines[j];
          if (line.trim().length === 0) {
            block.push("");
            continue;
          }
          if (line.match(/^(\s*)/)![1].length <= indent) break;
          block.push(line.trim());
        }
        const command = block.join("\n").trim();
        if (command.length > 0) out.push(command);
      } else if (value.length > 0) {
        out.push(value);
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
