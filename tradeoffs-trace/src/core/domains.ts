// Plan 07a (design D1): the one place that decides a file's domain and the
// one place that turns a finding's `where` into a stable key.
//
// A phase plan declares its domains with `#+TT_DOMAINS: name=glob[,glob…] …`
// (a per-plan keyword, like `#+TT_CONSOLIDATE`) and each R/A/C item names its
// own with a `:DOMAIN:` property. Domain mode is on exactly when the plan
// declares at least one domain. `domainOf` and `whereKey` are the only
// path→domain and where→key mappings (constraint C3), so the parser, the
// lint rule, the ingestion check and the duplicate fold all read the same
// rule.

import { matchesGlob } from "./boundaries.ts";

/** One declared domain: a name and the repository-relative globs it owns. */
export interface Domain {
  name: string;
  globs: string[];
}

/** A repository-relative path, without a leading `./`. */
function normalizePath(p: string): string {
  const trimmed = (p ?? "").trim();
  return trimmed.startsWith("./") ? trimmed.slice(2) : trimmed;
}

/** The domain NAME a repository-relative path belongs to: the first declared
 * domain (in declaration order) whose glob matches, or `undefined` when none
 * matches (or no domains are declared). */
export function domainOf(path: string, domains: readonly Domain[] | undefined): string | undefined {
  if (!domains || domains.length === 0) return undefined;
  const p = normalizePath(path);
  if (p.length === 0) return undefined;
  for (const domain of domains) {
    for (const glob of domain.globs ?? []) {
      if (matchesGlob(glob, p)) return domain.name;
    }
  }
  return undefined;
}

/** The stable key of a finding's `where` (`path:line symbol`): the path (the
 * text before the first `:` or space) and the symbol (the text after the
 * first space); the line number is ignored. Two reviewers naming the same
 * symbol at different lines therefore agree; a different path or symbol does
 * not. */
export function whereKey(where: string): string {
  const w = (where ?? "").trim();
  if (w.length === 0) return "";
  const colon = w.indexOf(":");
  const space = w.indexOf(" ");
  let end = w.length;
  if (colon >= 0) end = Math.min(end, colon);
  if (space >= 0) end = Math.min(end, space);
  const path = w.slice(0, end);
  const symbol = space >= 0 ? w.slice(space + 1).trim() : "";
  return symbol.length > 0 ? `${path} ${symbol}` : path;
}

/** Parse a `#+TT_DOMAINS:` value — whitespace-separated `name=glob[,glob…]`
 * tokens — into domains in declaration order. A token without `=` keeps the
 * whole token as its name and owns no globs (so lint can still report the
 * items it covers as undeclared). */
export function parseDomains(value: string | undefined): Domain[] {
  if (typeof value !== "string") return [];
  const out: Domain[] = [];
  for (const token of value.split(/[ \t]+/).filter(Boolean)) {
    const eq = token.indexOf("=");
    const name = (eq >= 0 ? token.slice(0, eq) : token).trim();
    const globs = eq >= 0 ? token.slice(eq + 1).split(/[ \t,]+/).map((g) => g.trim()).filter(Boolean) : [];
    out.push({ name, globs });
  }
  return out;
}
