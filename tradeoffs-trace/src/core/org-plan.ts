// Plan 06b: a minimal Org reader for `tt lint` and `tt plan template`.
//
// The reference doc says "Emacs and `tt lint` turn the subtree into
// plan.json". Emacs remains the authoring parser (it knows the buffer, the
// lines and org-element); this module is the lint-side reader, so a skeleton
// `tt plan template` prints can be checked before any run exists and `tt lint
// plan.org` reports the same item rules as `tt lint plan.json`.
//
// It is deliberately small: headings, property drawers, the plan keywords,
// and the structured items (with their sub-lists and source blocks kept
// whole). It is not a general Org parser and does not aim to be — every
// construct the plan format uses is handled, and anything else is left in the
// item text.

import type { LintItemInput, LintModels, LintPhaseInput, LintPlanInput } from "./plan-lint.ts";

interface OrgNode {
  level: number;
  title: string;
  tags: string[];
  props: Record<string, string>;
  /** The 1-based line of the headline. */
  line: number;
  /** The 1-based line of each property, when present. */
  propLines: Record<string, number>;
  /** The node's body: every line until the next headline of level <= this
   * one, property drawer removed. Source blocks are kept verbatim. */
  body: string[];
  /** The literal source body, captured without any line classification (the
   * property drawer included). `rawText` is built from this, so `tt lint` can
   * still see a line the parsed `text` dropped. */
  rawBody: string[];
  children: OrgNode[];
}

function splitTitle(raw: string): { title: string; tags: string[] } {
  const m = /^(.*?)\s+(:[A-Za-z0-9_@#%:]+:)\s*$/.exec(raw);
  if (!m) return { title: raw.trim(), tags: [] };
  const tags = m[2].split(":").filter(Boolean);
  return { title: m[1].trim(), tags };
}

/** Parse an Org buffer into its headline tree. Exported for the tests that
 * prove the reader keeps an item's whole body. */
export function parseOrgTree(text: string): { keywords: Array<{ key: string; value: string; line: number }>; roots: OrgNode[] } {
  const lines = text.split("\n");
  const keywords: Array<{ key: string; value: string; line: number }> = [];
  const roots: OrgNode[] = [];
  const stack: OrgNode[] = [];
  let current: OrgNode | undefined;
  let inDrawer = false;
  // Plan 06b (finding A-6): inside `#+begin_src … #+end_src` every line is
  // verbatim content — a `* headline` or a `#+name: example` in an example
  // block is not a headline or a plan keyword.
  let inSrc = false;

  const pushRaw = (line: string) => {
    if (current) current.rawBody.push(line);
  };

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineNo = i + 1;
    if (inSrc) {
      pushRaw(line);
      if (current) current.body.push(line);
      if (/^\s*#\+end_src\b/i.test(line)) inSrc = false;
      continue;
    }
    if (/^\s*#\+begin_src\b/i.test(line)) {
      pushRaw(line);
      if (current) current.body.push(line);
      inSrc = true;
      continue;
    }
    const headline = /^(\*+)\s+(.*)$/.exec(line);
    if (headline) {
      inDrawer = false;
      const level = headline[1].length;
      const { title, tags } = splitTitle(headline[2]);
      const node: OrgNode = { level, title, tags, props: {}, line: lineNo, propLines: {}, body: [], rawBody: [], children: [] };
      while (stack.length > 0 && stack[stack.length - 1].level >= level) stack.pop();
      if (stack.length > 0) stack[stack.length - 1].children.push(node);
      else roots.push(node);
      stack.push(node);
      current = node;
      continue;
    }
    // The raw body keeps every non-headline line, including the property
    // drawer and any keyword a later parse might drop.
    pushRaw(line);
    const keyword = /^#\+([A-Za-z0-9_-]+):\s*(.*)$/.exec(line);
    if (keyword) {
      keywords.push({ key: keyword[1].toUpperCase(), value: keyword[2].trim(), line: lineNo });
      if (current) current.body.push(line);
      continue;
    }
    if (current && /^\s*:PROPERTIES:\s*$/i.test(line)) {
      inDrawer = true;
      continue;
    }
    if (current && inDrawer && /^\s*:END:\s*$/i.test(line)) {
      inDrawer = false;
      continue;
    }
    const prop = /^\s*:([A-Za-z0-9_]+):\s*(.*)$/.exec(line);
    if (current && inDrawer && prop) {
      current.props[prop[1].toUpperCase()] = prop[2].trim();
      current.propLines[prop[1].toUpperCase()] = lineNo;
      continue;
    }
    if (current) current.body.push(line);
  }
  return { keywords, roots };
}

function bodyText(node: OrgNode): string {
  return node.body.join("\n").trim();
}

function itemInput(node: OrgNode, kind: "architecture" | "requirement" | "constraint"): LintItemInput {
  const text = bodyText(node);
  // The raw text comes from the literal source, not from the parsed text, so
  // `lostTextLines` can actually see a line the parse dropped (finding M-2).
  const rawText = node.rawBody.join("\n").trim();
  const item: LintItemInput = {
    id: node.props.ID,
    title: node.title,
    text,
    rawText,
    line: node.line,
  };
  if (kind === "architecture") {
    if (node.props.WHERE) item.where = node.props.WHERE;
    if (node.propLines.WHERE) item.whereLine = node.propLines.WHERE;
    (item as LintItemInput & { tags?: string[] }).tags = node.tags;
  }
  if (kind === "requirement") {
    item.arch = (node.props.ARCH ?? "").split(/[ \t,]+/).filter(Boolean);
    if (node.propLines.ARCH) item.archLine = node.propLines.ARCH;
  }
  if (kind === "requirement" || kind === "constraint") {
    item.verify = node.props.VERIFY ? [node.props.VERIFY] : [];
    if (node.propLines.VERIFY) item.verifyLine = node.propLines.VERIFY;
  }
  return item;
}

/** Parse one level-1 phase subtree. */
function parsePhaseNode(node: OrgNode, acceptanceLines: number[] = [], globalFinalChecks?: string): LintPhaseInput {
  const byTitle = (name: string) => node.children.find((c) => c.title === name);
  const goalNode = byTitle("Goal");
  const architecture = byTitle("Architecture");
  const requirements = byTitle("Requirements");
  const constraints = byTitle("Constraints");
  const structured = Boolean(architecture || requirements || constraints);
  // Plan 06c: a phase's own :FINAL_CHECKS: overrides the plan's
  // #+TT_FINAL_CHECKS; neither leaves the plan behaving as before.
  const finalChecks = node.props.FINAL_CHECKS ?? globalFinalChecks;
  const phase: LintPhaseInput = {
    id: node.props.ID,
    ...(goalNode ? { goal: bodyText(goalNode) } : {}),
    // Plan 06j (A1): the coverage rule reads the phase's own :CHECKS: and
    // :BOUNDARIES:. A missing :CHECKS: means no phase check (the plan's
    // global #+TT_CHECKS still applies); a missing :BOUNDARIES: means none.
    ...(node.props.CHECKS ? { checks: [node.props.CHECKS] } : {}),
    ...(node.props.BOUNDARIES
      ? { boundaries: node.props.BOUNDARIES.split(/[\s,]+/).map((s) => s.trim()).filter(Boolean) }
      : {}),
    acceptance: [],
    acceptanceLines,
    ...(finalChecks ? { finalChecks: [finalChecks] } : {}),
  };
  if (structured) {
    phase.architecture = (architecture?.children ?? []).map((c) => itemInput(c, "architecture"));
    phase.requirements = (requirements?.children ?? []).map((c) => itemInput(c, "requirement"));
    phase.constraints = (constraints?.children ?? []).map((c) => itemInput(c, "constraint"));
    // An old-format acceptance list may sit beside the structured headings;
    // it is kept for the owner-actor rules.
    const acceptance = acceptanceFromBody(node);
    phase.acceptance = acceptance.items;
    phase.acceptanceLines = acceptance.lines;
  } else {
    const acceptance = acceptanceFromBody(node);
    phase.acceptance = acceptance.items;
    phase.acceptanceLines = acceptance.lines;
    const reserved = (node.props.RESERVED ?? "").split(";").map((s) => s.trim()).filter(Boolean);
    phase.requirements = acceptance.items.map((text, i) => ({
      id: `R${i + 1}`,
      title: text,
      text,
      rawText: text,
      arch: [],
      verify: [/^\s*evidence\s*:/i.test(text) ? "evidence" : "review"],
      line: acceptance.lines[i],
    }));
    if (reserved.length > 0) {
      phase.constraints = [{ id: "C1", title: reserved.join("; "), text: reserved.join("; "), rawText: reserved.join("; "), verify: ["review"] }];
    }
  }
  return phase;
}

function acceptanceFromBody(node: OrgNode): { items: string[]; lines: number[] } {
  const items: string[] = [];
  const lines: number[] = [];
  const body = node.body;
  for (let i = 0; i < body.length; i++) {
    if (!/^\s*Acceptance:\s*$/.test(body[i])) continue;
    for (let j = i + 1; j < body.length; j++) {
      const m = /^\s*-\s+(.*)$/.exec(body[j]);
      if (!m) break;
      items.push(m[1].trim());
      // The node's body started at node.line + 1, but the property drawer was
      // removed; recompute from the original line is not tracked here, so the
      // lint line is best-effort for an Org file. Emacs is the authoring
      // parser and records exact lines.
      lines.push(node.line + j + 1);
    }
    break;
  }
  return { items, lines };
}

/** Plan 06g: `#+TT_WORKERS:`/`#+TT_ROUNDS:` — a whole number, kept as
 * written so the linter can refuse a nonsense value rather than silently
 * defaulting. Absent leaves the plan exactly as before. */
function parseCount(keyword: { value: string; line: number } | undefined): number | undefined {
  if (!keyword) return undefined;
  const value = keyword.value.trim();
  if (value.length === 0) return undefined;
  const n = Number(value);
  return Number.isFinite(n) ? n : Number.NaN;
}

/** Plan 06h (A1): `#+TT_REVIEWERS:` — the whitespace/comma separated seat
 * list, kept exactly as written (the linter refuses an even or duplicate
 * list). */
function parseSeats(keyword: { value: string; line: number } | undefined): Pick<LintPlanInput, "seats" | "seatsLine"> {
  if (!keyword) return {};
  const seats = keyword.value
    .split(/[ \t,]+/)
    .map((s) => s.trim())
    .filter(Boolean);
  return { seats, seatsLine: keyword.line };
}

function parseModels(keyword: { value: string; line: number } | undefined): Pick<LintPlanInput, "models" | "modelsLine" | "modelsRepeated"> {
  if (!keyword) return {};
  const models: LintModels = {};
  const reviewerSeats: Record<string, { provider?: string; model?: string }> = {};
  const panelSeats: Record<string, { provider?: string; model?: string }> = {};
  const seen = new Set<string>();
  const repeated: string[] = [];
  let panelFrom: string | undefined;
  for (const token of keyword.value.split(/[ \t,]+/).filter(Boolean)) {
    const eq = token.indexOf("=");
    const key = eq >= 0 ? token.slice(0, eq) : token;
    const value = eq >= 0 ? token.slice(eq + 1) : "";
    if (seen.has(key)) repeated.push(key);
    seen.add(key);
    const dot = key.indexOf(".");
    const role = dot >= 0 ? key.slice(0, dot) : key;
    const seat = dot >= 0 ? key.slice(dot + 1) : undefined;
    if (!dot && key === "panel" && value === "reviewers") {
      panelFrom = "reviewers";
      continue;
    }
    const colon = value.indexOf(":");
    const provider = colon > 0 && !value.slice(0, colon).includes("/") ? value.slice(0, colon) : undefined;
    const model = provider ? value.slice(colon + 1) : value;
    const entry = provider ? { provider, model } : { model };
    if (seat && role === "reviewer") reviewerSeats[seat] = entry;
    else if (seat && role === "panel") panelSeats[seat] = entry;
    else models[key] = entry;
  }
  if (Object.keys(reviewerSeats).length > 0) models.reviewerSeats = reviewerSeats;
  if (Object.keys(panelSeats).length > 0) models.panelSeats = panelSeats;
  if (panelFrom) models.panelFrom = panelFrom;
  return {
    models,
    modelsLine: keyword.line,
    ...(repeated.length > 0 ? { modelsRepeated: repeated } : {}),
  };
}

/** Parse an Org plan into the linter's input shape. `sourceFile` is recorded
 * so a finding names the Org file the owner edited. */
export function parseOrgPlan(text: string, sourceFile?: string): LintPlanInput {
  const { keywords, roots } = parseOrgTree(text);
  const first = (key: string) => keywords.find((k) => k.key === key);
  const globalFinalChecks = first("TT_FINAL_CHECKS")?.value;
  const phases = roots.filter((r) => r.level === 1).map((r) => parsePhaseNode(r, [], globalFinalChecks));
  const rerun = first("TT_RERUN");
  const envFile = first("TT_ENV_FILE")?.value;
  const workers = first("TT_WORKERS");
  const rounds = first("TT_ROUNDS");
  const leader = first("TT_LEADER");
  const workersValue = parseCount(workers);
  const roundsValue = parseCount(rounds);
  const repo = first("TT_REPO")?.value;
  return {
    ...(sourceFile ? { sourceFile } : {}),
    ...(repo && repo.trim().length > 0 ? { repo: repo.trim() } : {}),
    ...(envFile && envFile.trim().length > 0 ? { envFile: envFile.trim() } : {}),
    phases,
    ...parseModels(first("TT_MODELS")),
    ...parseSeats(first("TT_REVIEWERS")),
    ...(rerun ? { rerun: rerun.value, rerunLine: rerun.line } : {}),
    ...(workersValue !== undefined ? { workers: workersValue, workersLine: workers!.line } : {}),
    ...(roundsValue !== undefined ? { rounds: roundsValue, roundsLine: rounds!.line } : {}),
    ...(leader ? { leader: leader.value.trim(), leaderLine: leader.line } : {}),
  };
}

/** The Org skeleton `tt plan template` prints. It is a complete, lint-clean
 * plan: one phase with a goal, one architecture item, one requirement and one
 * constraint, each with an `:ID:` and a verify. */
export const PLAN_TEMPLATE = `#+TITLE: my plan
#+TT_REPO: /absolute/path/to/repo
#+TT_BRANCH: main
#+TT_CHECKS: make check

* Stage 1: one outcome
  :PROPERTIES:
  :ID:          stage-1
  :CHECKS:      make check
  :BOUNDARIES:  src/**
  :END:

** Goal
   One paragraph: the outcome and why, in the owner's words.

** Architecture
*** A1 The data the stage writes                                       :data:
    :PROPERTIES:
    :ID:       A1
    :WHERE:    src/core/rounds.ts
    :END:
    #+begin_src typescript
    interface Round { n: number }
    #+end_src
    One or two sentences: who writes it, who reads it, its invariant.

** Requirements
*** R1 The outcome a worker produces
    :PROPERTIES:
    :ID:       R1
    :ARCH:     A1
    :VERIFY:   test "the test that proves it"
    :END:
    One paragraph. Sub-lists are allowed and parsed as part of the item:
    - the first point
    - the second point

** Constraints
*** C1 What must not change
    :PROPERTIES:
    :ID:       C1
    :VERIFY:   review
    :END:
    One paragraph.
`;
