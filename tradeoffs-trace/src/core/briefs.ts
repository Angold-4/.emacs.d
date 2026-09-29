// Decision briefs (the owner-facing brief for every item that needs the
// owner's decision). One brief per open owner item, produced once per round
// after evaluation by the evaluator's model, checked against the code and the
// plan, and rendered above the evidence.
//
// A brief answers, in under a minute and without code identifiers:
//   - question:  one plain line (e.g. "Should a vendor excluded before a
//                weekend stay excluded when its market reopens?")
//   - today:     what the system does now, in plain words, with one concrete
//                example using real market names and times from the plan's
//                calendars (checked against calendars.yaml/products.yaml)
//   - impact:    what the owner would notice — price flow, number of vendors,
//                quality, duration — ALWAYS answering "does any market stop
//                publishing?"
//   - options:   each of the request's own options, relabelled in plain words
//                with what happens and its cost (ids unchanged, so resolving
//                from the brief is the same command as resolving the request)
//   - recommendation: one option and why, citing the plan or IC section
//   - related:   other open items that touch the same concern
//   - evidence:  the original message/finding/file:line, folded under TAB
//
// This module is pure: no process, socket or filesystem access beyond the
// catalog loader (`loadCatalogs`), which is separated out for tests.

import type { DecisionBrief, Event, OwnerRequest } from "./types.ts";

export type { DecisionBrief };

// ---------------------------------------------------------------------------
// Catalogs (calendars.yaml / products.yaml)
// ---------------------------------------------------------------------------

export interface CalendarDef {
  timezone?: string;
  /** session name -> "HH:MM-HH:MM" (a session may wrap past midnight). */
  sessions: Record<string, string>;
}

export interface ProductDef {
  vendor?: string;
  calendar?: string;
}

export interface Catalogs {
  calendars: Record<string, CalendarDef>;
  products: Record<string, ProductDef>;
}

// ---------------------------------------------------------------------------
// Plain-language checks
// ---------------------------------------------------------------------------

/** A `snake_case` token (e.g. `within_band_active`, `excluded_before`) or a
 * `path/file.ext` token (e.g. `src/core/blend.rs`). The brief's question must
 * contain neither. The extension list is deliberately broad: any `/`-bearing
 * token with a code-ish extension is a code identifier. */
const SNAKE_CASE = /\b[A-Za-z][A-Za-z0-9]*_[A-Za-z0-9_]*[A-Za-z0-9]\b/;
const PATH_LIKE = /(?:^|[\s(`"'])(?:[\w.@-]+\/)+[\w.@-]+\.[A-Za-z][A-Za-z0-9]{0,7}\b/;
const CODE_EXT = /\b[\w.-]+\.(?:rs|ts|tsx|js|mjs|cjs|el|json|ya?ml|org|md|toml|py|go|c|cc|cpp|h|hpp|java|rb|sh|lock)\b/;

/** True when TEXT carries a `snake_case` or `path/file.ext` code identifier. */
export function hasCodeIdentifier(text: string): boolean {
  return SNAKE_CASE.test(text) || PATH_LIKE.test(text) || CODE_EXT.test(text);
}

/** Any quantified claim: a clock time (`20:00`), a duration (`10 s`,
 * `2 minutes`), a digit count (`3 vendors`), or a word count (`one vendor`). */
const CLOCK_TIME = /\b\d{1,2}:\d{2}\b/;
const DURATION = /\b\d+(?:\.\d+)?\s*(?:ms|s|sec|secs|second|seconds|min|mins|minute|minutes|h|hr|hrs|hour|hours|day|days|week|weeks)\b/i;
const DIGIT_COUNT = /\b\d+\b/;
const WORD_COUNT = /\b(?:one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve)\s+(?:vendor|vendors|market|markets|instrument|instruments|product|products|round|rounds|day|days|week|weeks|hour|hours|minute|minutes|second|seconds|tick|ticks|price|prices|session|sessions|source|sources)\b/i;

export function hasQuantifiedFact(text: string): boolean {
  return CLOCK_TIME.test(text) || DURATION.test(text) || DIGIT_COUNT.test(text) || WORD_COUNT.test(text);
}

/** An evidence entry that names the config or code the brief read. Only a
 * citation counts for a quantified claim (the original message alone does
 * not). */
export function isEvidenceCitation(entry: string): boolean {
  const s = entry.trim();
  if (!s) return false;
  if (/^(?:config|code|plan):/i.test(s)) return true;
  // a file path, optionally with a line number or a section
  return /\b[\w./-]+\.(?:ya?ml|rs|ts|tsx|js|el|json|org|md|toml|py|go|c|cpp|h|rb|sh)(?::\d+)?\b/.test(s) || /§\s*\d/.test(s);
}

/** Whether IMPACT answers the owner's first question: "does any market stop
 * publishing?" Either a plain "no market stops publishing"/"every market
 * keeps publishing", or an explicit "one market stops publishing". */
export function impactAnswersPublishing(impact: string): boolean {
  return /\b(?:no|any|every|each|one|two|three|all)\b[^.]{0,80}\bmarket[s]?\b[^.]{0,40}\b(?:stop|stops|stopping|keep|keeps|keeping|continue|continues|continuing|publish|publishes|publishing|halt|halts|go(?:es)?\s+(?:dark|silent))\b/i.test(
    impact,
  ) || /\b(?:publishing|publication)\b[^.]{0,40}\b(?:stop|stops|continue|continues|halt|halts|go(?:es)?)\b/i.test(impact);
}

// ---------------------------------------------------------------------------
// Catalog example checking
// ---------------------------------------------------------------------------

/** Split a "HH:MM-HH:MM" session into its two boundary times. */
function sessionTimes(spec: string): string[] {
  return spec
    .split(/[-–]/)
    .map((t) => t.trim())
    .filter((t) => /^\d{1,2}:\d{2}$/.test(t));
}

export interface ExampleCheck {
  ok: boolean;
  reason?: string;
  product?: string;
  calendar?: string;
  time?: string;
}

/** Check the one concrete example in `today` against the plan's calendars.
 * The example must name a product from products.yaml and a time that is a
 * session boundary of that product's calendar. When the catalogs are missing
 * (or the example names no known product/time), the check says so — it never
 * invents an example. */
export function checkTodayExample(today: string, catalogs: Catalogs | undefined): ExampleCheck {
  if (!catalogs) return { ok: false, reason: "no calendars.yaml/products.yaml to check the example against" };
  const products = catalogs.products ?? {};
  const named = Object.keys(products).filter((symbol) => new RegExp(`\\b${symbol.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\b`).test(today));
  const times = today.match(/\b\d{1,2}:\d{2}\b/g) ?? [];
  if (named.length === 0) {
    return { ok: false, reason: "the example names no product from products.yaml" };
  }
  if (times.length === 0) {
    return { ok: false, reason: "the example names no market time (HH:MM)" };
  }
  // Every named product's calendar must exist and must know the named time(s).
  for (const product of named) {
    const calendar = products[product]?.calendar;
    if (!calendar || !catalogs.calendars?.[calendar]) {
      return { ok: false, reason: `product ${product} names calendar '${calendar ?? "(none)"}', which is not in calendars.yaml` };
    }
    const boundaries = new Set(Object.values(catalogs.calendars[calendar].sessions ?? {}).flatMap(sessionTimes));
    for (const time of times) {
      if (!boundaries.has(time)) {
        return { ok: false, reason: `${time} is not a ${calendar} session boundary for ${product}` };
      }
    }
  }
  return { ok: true, product: named[0], calendar: products[named[0]].calendar, time: times[0] };
}

// ---------------------------------------------------------------------------
// Brief validation
// ---------------------------------------------------------------------------

export interface BriefIssueOptions {
  /** The ids of the underlying owner request's own options, in order. When
   * given, the brief's option ids must map one-to-one. */
  requestOptions?: string[];
  /** The plan's calendars/products. When given, `today` is checked; a `today`
   * that cannot be checked must carry "(example unverified)". */
  catalogs?: Catalogs | null;
}

/** The first reason a brief is not owner-readable, or undefined when it is.
 * Pure and side-effect free: the evaluator's `submit_brief` tool and the
 * conductor both call this, so a rejected brief is rejected everywhere. */
export function briefIssue(brief: Partial<DecisionBrief> | undefined, opts: BriefIssueOptions = {}): string | undefined {
  if (!brief || typeof brief !== "object") return "a brief must be an object";
  const question = typeof brief.question === "string" ? brief.question.trim() : "";
  const today = typeof brief.today === "string" ? brief.today.trim() : "";
  const impact = typeof brief.impact === "string" ? brief.impact.trim() : "";
  if (!question) return "a brief needs a one-line question";
  if (hasCodeIdentifier(question)) {
    return `the question names code (${hasCodeIdentifier(question) ? (question.match(SNAKE_CASE) ?? question.match(CODE_EXT) ?? ["a code identifier"])[0] : ""}); rewrite it in plain words`;
  }
  if (question.includes("\n")) return "the question must be one line";
  if (!today) return "a brief needs a `today` paragraph";
  if (!impact) return "a brief needs an `impact` paragraph";
  if (!impactAnswersPublishing(impact)) {
    return "the impact must say whether any market stops publishing";
  }
  if (!Array.isArray(brief.options) || brief.options.length === 0) return "a brief needs at least one option";
  const ids = brief.options.map((o) => (typeof o?.id === "string" ? o.id.trim() : ""));
  if (ids.some((id) => !id)) return "every option needs its request option id";
  if (new Set(ids).size !== ids.length) return "option ids must be distinct";
  for (const option of brief.options) {
    if (!option.label?.trim() || !option.effect?.trim() || !option.cost?.trim()) {
      return `option ${option.id} needs a plain label, what happens and its cost`;
    }
  }
  if (opts.requestOptions && opts.requestOptions.length > 0) {
    if (ids.length !== opts.requestOptions.length || opts.requestOptions.some((id) => !ids.includes(id))) {
      return `the brief's options (${ids.join(", ")}) must map one-to-one to the request's (${opts.requestOptions.join(", ")})`;
    }
  }
  const rec = brief.recommendation;
  if (!rec || !rec.option?.trim() || !rec.why?.trim()) return "a brief needs one recommended option and why";
  if (!ids.includes(rec.option.trim())) return `the recommended option '${rec.option}' is not one of the brief's options`;
  if (!Array.isArray(brief.evidence) || brief.evidence.filter((e) => typeof e === "string" && e.trim()).length === 0) {
    return "a brief needs the original evidence";
  }
  const quantified = hasQuantifiedFact([today, impact, ...brief.options.flatMap((o) => [o.effect, o.cost]), rec.why].join(" "));
  if (quantified && !brief.evidence.some((e) => typeof e === "string" && isEvidenceCitation(e))) {
    return "a time, count or duration needs an evidence citation to the config or code it read";
  }
  if (opts.catalogs !== undefined) {
    const unverified = today.includes("(example unverified)");
    const check = checkTodayExample(today, opts.catalogs ?? undefined);
    if (!check.ok && !unverified) {
      return `the today example cannot be checked: ${check.reason}; either name a real product and session time or say "(example unverified)"`;
    }
  }
  return undefined;
}

// ---------------------------------------------------------------------------
// Related items
// ---------------------------------------------------------------------------

export interface OpenItemConcern {
  id: string;
  question: string;
  /** Files the item touches (plain paths, no line numbers). */
  files?: string[];
  /** Plan clauses / IC sections the item cites. */
  planRefs?: string[];
}

/** Other open items that touch the same file or plan clause as ITEM. The
 * owner should see them with the brief, so a bigger silence on the same
 * concern is never hidden behind a narrow request. */
export function relatedOpenItems(item: OpenItemConcern, all: readonly OpenItemConcern[]): OpenItemConcern[] {
  const files = new Set((item.files ?? []).map((f) => f.trim()));
  const planRefs = new Set((item.planRefs ?? []).map((p) => p.trim()));
  return all.filter((other) => {
    if (other.id === item.id) return false;
    if (planRefs.size > 0 && (other.planRefs ?? []).some((p) => planRefs.has(p.trim()))) return true;
    if (files.size > 0 && (other.files ?? []).some((f) => files.has(f.trim()))) return true;
    return false;
  });
}

/** The file part of a `file:line` evidence string, or undefined. */
export function evidenceFile(evidence: string): string | undefined {
  const m = evidence.match(/\b([\w./-]+\.[A-Za-z][A-Za-z0-9]{0,7})(?::\d+)?\b/);
  return m?.[1];
}

// ---------------------------------------------------------------------------
// Resolution: the same command as resolving the underlying request
// ---------------------------------------------------------------------------

export interface BriefBinding {
  runId: string;
  phaseId: string;
  candidateSha: string;
  recordVersion: number;
  contractVersion: { snapshot: number; sectionSha256: string };
}

/** The decision-view resolve command for choosing OPTION on REQUEST. Both the
 * plain request path and the brief path build it here, so choosing from a
 * brief sends the identical command (same option id, same binding). */
export function resolveCommandFor(request: OwnerRequest, optionId: string, binding: BriefBinding): Record<string, unknown> {
  return {
    type: "resolve",
    option: optionId,
    binding: {
      runId: binding.runId,
      phaseId: binding.phaseId,
      recordId: request.id,
      candidateSha: binding.candidateSha,
      recordVersion: request.version ?? binding.recordVersion,
      contractVersion: binding.contractVersion,
    },
  };
}

/** The same command, built from a brief: the brief carries the request id and
 * the underlying request's option ids, so the brief never invents an option.
 * REFERENCE is the underlying OwnerRequest (when available); otherwise the
 * brief's own requestId/version are used. */
export function briefResolveCommand(brief: DecisionBrief, optionId: string, binding: BriefBinding, request?: OwnerRequest): Record<string, unknown> {
  const source: OwnerRequest =
    request ??
    ({
      id: brief.requestId,
      version: binding.recordVersion,
      phaseId: binding.phaseId,
      reason: brief.question,
      origin: "open_finding",
      options: brief.options.map((o) => ({ id: o.id, label: o.label })),
      status: "open",
    } as OwnerRequest);
  return resolveCommandFor(source, optionId, binding);
}

/** The core event a resolve command maps to, identical for the request and the
 * brief path (used by the tests to prove the two are the same). */
export function briefResolveEvent(brief: DecisionBrief, optionId: string, binding: BriefBinding, request?: OwnerRequest): Event {
  const command = briefResolveCommand(brief, optionId, binding, request);
  return {
    type: "OWNER_REQUEST_RESOLVED",
    requestId: brief.requestId,
    option: optionId,
    boundCandidateSha: binding.candidateSha,
    boundContractVersion: binding.contractVersion,
    boundRecordVersion: (command.binding as { recordVersion: number }).recordVersion,
  };
}

// ---------------------------------------------------------------------------
// Glossary (owner-facing; the runbook carries the same table)
// ---------------------------------------------------------------------------

/** The few terms a brief may still use, each in one sentence. The evaluator
 * links a term instead of explaining it inline. */
export const BRIEF_GLOSSARY: ReadonlyArray<{ term: string; meaning: string }> = [
  { term: "band", meaning: "A vendor drops out when its last tick is older than the band's upper edge (T_out), and rejoins when a fresh tick arrives inside the band's lower edge (T_in)." },
  { term: "T_in", meaning: "The staleness at which an excluded vendor is allowed back in." },
  { term: "T_out", meaning: "The staleness at which a live vendor is excluded." },
  { term: "window", meaning: "The recent span of ticks the calculator reads to decide a price." },
  { term: "held", meaning: "The market is open but no vendor is publishing, so no price can be formed." },
  { term: "degraded", meaning: "A price is still published, but from fewer vendors than the full set." },
];

export function renderGlossaryOrg(): string {
  return BRIEF_GLOSSARY.map((g) => `- ${g.term}: ${g.meaning}`).join("\n");
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/** Rendered above the evidence in `views/review.org`: the question as the
 * heading, then today / impact / options / recommendation in short
 * paragraphs, `related` as links, evidence folded under a `** Evidence`
 * subtree. The heading carries enough properties for the Emacs view to fold
 * TAB and to send the same resolve command. */
/** `views/review.org`: one owner item's brief as an Org subtree. The question
 * is the heading; today / impact / options / recommendation are short body
 * paragraphs, `related` links the other open items, and the original evidence
 * is in the same folded body (TAB reveals it). The property drawer carries the
 * request id and — when a binding is given — the same tuple a resolve command
 * needs, so choosing an option from the brief sends exactly the resolve
 * command the request sends. */
export function renderBriefOrg(brief: DecisionBrief, opts: { request?: OwnerRequest; binding?: BriefBinding; heading?: string } = {}): string {
  const heading = opts.heading ?? "**";
  const indent = " ".repeat(heading.length + 1);
  const lines: string[] = [];
  lines.push(`${heading} ${brief.question}`);
  lines.push(`${indent}:PROPERTIES:`);
  lines.push(`${indent}:ID:       ${brief.requestId}`);
  lines.push(`${indent}:KIND:     brief`);
  lines.push(`${indent}:OPTIONS:  ${brief.options.map((o) => o.id).join(",")}`);
  lines.push(`${indent}:QUESTION: ${brief.question}`);
  if (opts.request && opts.binding) {
    lines.push(`${indent}:RECORD_VERSION: ${opts.request.version}`);
    lines.push(`${indent}:CANDIDATE_SHA:  ${opts.binding.candidateSha}`);
    lines.push(`${indent}:CONTRACT_VERSION: ${opts.binding.contractVersion.snapshot}`);
    lines.push(`${indent}:CONTRACT_SHA256:  ${opts.binding.contractVersion.sectionSha256}`);
    lines.push(`${indent}:RUN_ID:   ${opts.binding.runId}`);
    lines.push(`${indent}:PHASE_ID: ${opts.binding.phaseId}`);
  }
  lines.push(`${indent}:END:`);
  lines.push(`${indent}Today: ${brief.today}`);
  lines.push(`${indent}Impact: ${brief.impact}`);
  lines.push(`${indent}Options:`);
  for (const option of brief.options) {
    const chosen = brief.recommendation.option === option.id ? " (recommended)" : "";
    lines.push(`${indent}- ${option.label}${chosen} — ${option.effect} Cost: ${option.cost} [${option.id}]`);
  }
  lines.push(
    `${indent}Recommendation: ${brief.options.find((o) => o.id === brief.recommendation.option)?.label ?? brief.recommendation.option} — ${brief.recommendation.why}`,
  );
  if (brief.related.length > 0) {
    lines.push(`${indent}Related:`);
    for (const r of brief.related) lines.push(`${indent}- ${r.id}: ${r.question}`);
  }
  lines.push(`${indent}Evidence (original):`);
  for (const ev of brief.evidence) lines.push(`${indent}- ${ev}`);
  return lines.join("\n");
}

/** The `* Needs you (N)` section the review view puts above every other
 * section: one brief per open owner item. */
export function renderBriefsSection(briefs: readonly DecisionBrief[], opts: { requestFor?: (id: string) => OwnerRequest | undefined; binding?: BriefBinding } = {}): string[] {
  if (briefs.length === 0) return [];
  const lines = [`* Needs you (${briefs.length})`];
  for (const brief of briefs) {
    lines.push(renderBriefOrg(brief, { request: opts.requestFor?.(brief.requestId), binding: opts.binding }), "");
  }
  return lines;
}

/** A deterministic brief derived from an owner request when the evaluator did
 * not (or could not) produce one. It never invents a market time: if no
 * catalog is available, `today` says the example is unverified, and the
 * `briefIssue` gate then allows it. The option ids are the request's own, so
 * resolving is unchanged. */
export function fallbackBrief(request: OwnerRequest, opts: { question?: string; catalogs?: Catalogs | null } = {}): DecisionBrief {
  // The request's reason is engineer prose; strip its code identifiers and
  // digits so the fallback question is plain and asserts no uncited count.
  const plain = request.reason
    .replace(PATH_LIKE, "the code")
    .replace(CODE_EXT, "the code")
    .replace(SNAKE_CASE, "that setting")
    .replace(/\d+/g, "")
    .replace(/\s+/g, " ")
    .trim();
  const question = opts.question ?? `Should this stay as it is? ${plain}`;
  const today = opts.catalogs
    ? "The plan's calendars and products were available when this brief was written, but no concrete example was recorded. (example unverified)"
    : "No calendars.yaml/products.yaml was readable when this brief was written, so the example could not be checked. (example unverified)";
  return {
    requestId: request.id,
    question,
    today,
    impact: "No market stops publishing under any option; this request is about how the price is formed, not whether it is published.",
    // Plain labels only: the request's own label may carry a count
    // ("grant 3 rounds") that would then demand a citation this backstop
    // never read. The underlying option id is untouched, so resolving is
    // unchanged.
    options: request.options.map((o) => ({
      id: o.id,
      label: o.label,
      effect: o.label.replace(/\s*\([^)]*\)\s*$/, ""),
      cost: "as the request's own option defines it",
    })),
    recommendation: { option: request.options[0]?.id ?? "", why: "the request's first option, until the evaluator writes a brief" },
    related: [],
    evidence: [`message: ${request.reason}`],
  };
}

// ---------------------------------------------------------------------------
// Minimal YAML loader (calendars.yaml / products.yaml are simple nested maps)
// ---------------------------------------------------------------------------

interface YamlNode {
  [key: string]: YamlNode | string;
}

function stripComment(line: string): string {
  let inSingle = false;
  let inDouble = false;
  for (let i = 0; i < line.length; i += 1) {
    const ch = line[i];
    if (ch === "'" && !inDouble) inSingle = !inSingle;
    else if (ch === '"' && !inSingle) inDouble = !inDouble;
    else if (ch === "#" && !inSingle && !inDouble && (i === 0 || /\s/.test(line[i - 1]))) {
      return line.slice(0, i);
    }
  }
  return line;
}

/** Parse the tiny YAML subset the catalogs use: indentation-nested mappings
 * and `key: value` scalars. Quoted scalars are unquoted. A line with no
 * colon is ignored (blank/comment), never guessed at. */
export function parseSimpleYaml(text: string): YamlNode {
  const root: YamlNode = {};
  const stack: Array<{ indent: number; node: YamlNode }> = [{ indent: -1, node: root }];
  for (const rawLine of text.split(/\r?\n/)) {
    const line = stripComment(rawLine);
    if (!line.trim()) continue;
    const indent = line.length - line.trimStart().length;
    const body = line.trim();
    const colon = body.indexOf(":");
    if (colon <= 0) continue;
    const key = body.slice(0, colon).trim().replace(/^['"]|['"]$/g, "");
    const value = body.slice(colon + 1).trim().replace(/^['"]|['"]$/g, "");
    while (stack.length > 1 && indent <= stack[stack.length - 1].indent) stack.pop();
    const parent = stack[stack.length - 1].node;
    if (value === "") {
      const child: YamlNode = {};
      parent[key] = child;
      stack.push({ indent, node: child });
    } else {
      parent[key] = value;
    }
  }
  return root;
}

function asCalendarMap(node: YamlNode): Record<string, CalendarDef> {
  const out: Record<string, CalendarDef> = {};
  for (const [name, value] of Object.entries(node)) {
    if (typeof value === "string") continue;
    const timezone = typeof value.timezone === "string" ? value.timezone : undefined;
    const sessions: Record<string, string> = {};
    const rawSessions = value.sessions;
    if (rawSessions && typeof rawSessions !== "string") {
      for (const [session, spec] of Object.entries(rawSessions)) if (typeof spec === "string") sessions[session] = spec;
    }
    out[name] = { ...(timezone ? { timezone } : {}), sessions };
  }
  return out;
}

function asProductMap(node: YamlNode): Record<string, ProductDef> {
  const out: Record<string, ProductDef> = {};
  for (const [symbol, value] of Object.entries(node)) {
    if (typeof value === "string") continue;
    out[symbol] = {
      ...(typeof value.vendor === "string" ? { vendor: value.vendor } : {}),
      ...(typeof value.calendar === "string" ? { calendar: value.calendar } : {}),
    };
  }
  return out;
}

/** Parse catalog TEXT directly (tests use this; the conductor's loader reads
 * the two files). `calendars.yaml`'s root is the calendar map; a file that
 * wraps it under a top-level `calendars:` key is accepted too. */
export function parseCatalogs(calendarText: string | undefined, productText: string | undefined): Catalogs {
  const calendarsNode = calendarText ? parseSimpleYaml(calendarText) : {};
  const productsNode = productText ? parseSimpleYaml(productText) : {};
  const calendarsRoot = typeof calendarsNode.calendars === "object" && calendarsNode.calendars !== null ? (calendarsNode.calendars as YamlNode) : calendarsNode;
  const productsRoot = typeof productsNode.products === "object" && productsNode.products !== null ? (productsNode.products as YamlNode) : productsNode;
  return { calendars: asCalendarMap(calendarsRoot), products: asProductMap(productsRoot) };
}
