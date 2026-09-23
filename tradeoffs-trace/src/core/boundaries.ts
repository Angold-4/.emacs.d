// Work packet 2a addition (pure, additive — no existing row's semantics
// changed): design §3.3's boundary triggers ("diff paths matching the
// phase's BOUNDARIES globs, dependency manifests ... or acceptance files,
// with no citing record") and §3.5's sampling ("detail decisions and
// unreferenced hunks"). Pure functions only — the conductor is the one that
// runs `git diff` (src/effects/git.ts's diffNameOnly/diffHunks) and passes
// the resulting paths/hunks in here; nothing in this file touches a
// process, a socket or the filesystem.

import type { DiffHunk } from "../effects/git.ts";

/** The dependency-manifest filenames design §3.3 names explicitly. Matched
 * against a path's basename, so `packages/foo/package.json` matches too. */
const DEPENDENCY_MANIFEST_BASENAMES = new Set([
  "package.json",
  "package-lock.json",
  "npm-shrinkwrap.json",
  "Cargo.toml",
  "Cargo.lock",
  "go.mod",
  "go.sum",
  "pyproject.toml",
]);

function basename(p: string): string {
  const i = p.lastIndexOf("/");
  return i === -1 ? p : p.slice(i + 1);
}

/** `requirements*.txt` is the one manifest name design §3.3 gives as a
 * pattern rather than a literal filename. */
export function isDependencyManifest(p: string): boolean {
  const b = basename(p);
  if (DEPENDENCY_MANIFEST_BASENAMES.has(b)) return true;
  return /^requirements.*\.txt$/.test(b);
}

/** A small, dependency-free glob matcher for BOUNDARIES entries: `*` matches
 * within one path segment, `**` matches across segments, everything else is
 * a literal. Good enough for the glob shapes design §1.1/§3.3 actually use
 * (e.g. `src/**`, `*.md`) — not a general glob implementation. */
export function matchesGlob(glob: string, p: string): boolean {
  let re = "^";
  for (let i = 0; i < glob.length; i++) {
    const c = glob[i];
    if (c === "*" && glob[i + 1] === "*") {
      re += ".*";
      i++;
      // swallow an immediately following slash so `a/**` matches `a` itself
      if (glob[i + 1] === "/") i++;
    } else if (c === "*") {
      re += "[^/]*";
    } else if (c === "?") {
      re += "[^/]";
    } else if (".+^${}()|[]\\".includes(c)) {
      re += "\\" + c;
    } else {
      re += c;
    }
  }
  re += "$";
  return new RegExp(re).test(p);
}

/** True if `p` is one of `contract.boundaries`'s globs, a dependency
 * manifest, or one of the acceptance criteria's file paths (the same
 * path-shaped acceptance entries the conductor already treats as
 * "protected paths" — see conductor.ts's `protectedPaths`). */
export function isBoundaryRelevant(p: string, boundaries: string[], acceptanceFiles: string[]): boolean {
  if (isDependencyManifest(p)) return true;
  if (boundaries.some((g) => matchesGlob(g, p))) return true;
  if (acceptanceFiles.some((a) => a === p || p.endsWith(`/${a}`) || a.endsWith(`/${p}`))) return true;
  return false;
}

/** design §3.3: a boundary-relevant path with no record citing it becomes a
 * `trigger` the reviewer must classify. "Citing" is deliberately coarse for
 * this work packet (design §12 notes precision here is measured later): a
 * path counts as cited if it appears, as a plain substring, in any of the
 * given citation texts (decision choice/whyItMatters/alternative text,
 * finding evidence, worker assumptions/deviations). */
export function computeBoundaryTriggerPaths(
  diffPaths: string[],
  boundaries: string[],
  acceptanceFiles: string[],
  citationTexts: string[],
): string[] {
  return diffPaths.filter((p) => {
    if (!isBoundaryRelevant(p, boundaries, acceptanceFiles)) return false;
    return !citationTexts.some((text) => text.includes(p));
  });
}

/** design §3.5: "unreferenced hunks" — diff hunks no decision or finding
 * cites, by the same coarse substring-citation rule as boundary triggers. */
export function computeUnreferencedHunks(hunks: DiffHunk[], citationTexts: string[]): DiffHunk[] {
  return hunks.filter((h) => !citationTexts.some((text) => text.includes(h.file)));
}
