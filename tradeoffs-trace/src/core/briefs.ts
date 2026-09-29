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
  /** The weekly reopen, when the calendar has one (`"Sun 20:00"`), or
   * `"always"`. A weekday claim in `today` can only be checked when this is
   * present; without it a weekday claim must be refused or marked unverified
   * (finding M-10). */
  opens?: string;
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

/** The glossary terms the runbook says a brief may still use: they contain an
 * underscore but are owner-facing, so the question gate exempts them (finding
 * disc-M-55). */
const GLOSSARY_TERM = /\bT_(?:in|out)\b/g;

/** True when TEXT carries a `snake_case` or `path/file.ext` code identifier.
 * The owner-facing glossary terms (`T_in`, `T_out`) are exempt. */
export function hasCodeIdentifier(text: string): boolean {
  const plain = text.replace(GLOSSARY_TERM, "the band edge");
  return SNAKE_CASE.test(plain) || PATH_LIKE.test(plain) || CODE_EXT.test(plain);
}

/** Any quantified claim: a clock time (`20:00`), a duration (`10 s`,
 * `2 minutes`), a digit count (`3 vendors`), or a word count (`one vendor`). */
const CLOCK_TIME = /\b\d{1,2}:\d{2}\b/;
const DURATION = /\b\d+(?:\.\d+)?\s*(?:ms|s|sec|secs|second|seconds|min|mins|minute|minutes|h|hr|hrs|hour|hours|day|days|week|weeks)\b/i;
const DIGIT_COUNT = /\b\d+\b/;
// Up to two words may sit between the number word and its noun, so 'three
// more rounds' and 'one more round' are counts too (finding A-27).
const WORD_COUNT = /\b(?:one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve)(?:\s+\w+){0,2}\s+(?:vendor|vendors|market|markets|instrument|instruments|product|products|round|rounds|day|days|week|weeks|hour|hours|minute|minutes|second|seconds|tick|ticks|price|prices|session|sessions|source|sources)\b/i;

export function hasQuantifiedFact(text: string): boolean {
  // A plan/IC section reference (`§5`) is not a count; only strip the section
  // marker, so an actual `10 s` in the same sentence is still quantified.
  const t = text.replace(/§\s*\d+/g, " ");
  return CLOCK_TIME.test(t) || DURATION.test(t) || DIGIT_COUNT.test(t) || WORD_COUNT.test(t);
}

/** The sentences of TEXT, so each quantified claim can be paired with its own
 * citation rather than sharing one across the whole brief. */
export function sentences(text: string): string[] {
  return text
    .split(/(?<=[.!?])\s+/)
    .map((s) => s.trim())
    .filter((s) => s.length > 0);
}

/** An evidence entry that cites the CONFIG or CODE the brief actually read
 * (`config: ...` / `code: ...`, or a `path/file.ext:line`). A plan clause or
 * a bare `§` reference is not a config/code citation and does not satisfy the
 * quantified-claim rule (finding disc-M-54). */
export function isEvidenceCitation(entry: string): boolean {
  const s = entry.trim();
  if (!s) return false;
  // `config:` / `code:` may sit inline at the end of a claim, not only at the
  // start of an evidence line.
  if (/\b(?:config|code):/i.test(s)) return true;
  return /\b[\w./-]+\.(?:ya?ml|rs|ts|tsx|js|el|json|org|md|toml|py|go|c|cpp|h|rb|sh):\d+\b/.test(s);
}

/** Whether IMPACT answers the owner's first question: "does any market stop
 * publishing?". A model brief must state it plainly; the honest
 * "not established" wording is reserved for the deterministic backstop and
 * is recognized by `impactUnestablished`, never by this strict check (finding
 * disc-M-56). */
export function impactAnswersPublishing(impact: string): boolean {
  if (impactUnestablished(impact)) return false;
  return /\b(?:no|any|every|each|one|two|three|all)\b[^.]{0,80}\bmarket[s]?\b[^.]{0,40}\b(?:stop|stops|stopping|keep|keeps|keeping|continue|continues|continuing|publish|publishes|publishing|halt|halts|go(?:es)?\s+(?:dark|silent))\b/i.test(
    impact,
  ) || /\b(?:publishing|publication)\b[^.]{0,40}\b(?:stop|stops|continue|continues|halt|halts|go(?:es)?)\b/i.test(impact);
}

/** The backstop's honest "this was not established" wording. Only the
 * deterministic fallback may use it; a model brief's `impact` is refused when
 * it does (finding disc-M-56). */
export function impactUnestablished(impact: string): boolean {
  return /\b(?:not|never)\s+(?:been\s+)?(?:established|checked|verified|known)\b|\bunknown\b|\bcannot be (?:established|checked)\b|\bcould not (?:be )?(?:establish|check)\b/i.test(impact);
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

const WEEKDAY_WORDS: Record<string, number> = {
  sun: 0, sunday: 0,
  mon: 1, monday: 1,
  tue: 2, tues: 2, tuesday: 2,
  wed: 3, wednesday: 3,
  thu: 4, thur: 4, thurs: 4, thursday: 4,
  fri: 5, friday: 5,
  sat: 6, saturday: 6,
};

function normaliseWeekday(token: string): number | undefined {
  return WEEKDAY_WORDS[token.trim().toLowerCase().replace(/\.$/, "")];
}

/** The week reopen claim in TEXT, if any: a `reopen`/`open`/`return` verb
 * followed by `Weekday HH:MM`. Only an OPEN claim is checked against the
 * calendar's weekly schedule; a `closes Friday 17:00` is a session end, not a
 * reopen, and must not be read as one. */
function weekdayClaim(text: string): { day: number; time: string } | undefined {
  const m = text.match(/(?:reopen(?:s|ing)?|open(?:s|ing)?|return(?:s|ing)?)\b[^.]{0,40}?\b(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sun|Mon|Tue|Tues|Wed|Thu|Thur|Thurs|Fri|Sat)\b[^0-9]{0,12}(\d{1,2}:\d{2})/i);
  if (!m) return undefined;
  const day = normaliseWeekday(m[1]);
  return day === undefined ? undefined : { day, time: m[2] };
}

/** Check the one concrete example in `today` against the plan's calendars.
 * The example must name a product from products.yaml and a time that is a
 * session boundary of that product's calendar (or the calendar's weekly
 * `opens`). A weekday claim is checked against the calendar's `opens` when it
 * has one, and refused when it does not — a weekday is never guessed. When
 * the catalogs are missing (or the example names no known product/time), the
 * check says so; it never invents an example. */
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
  const claim = weekdayClaim(today);
  // Every named product's calendar must exist and must know the named time(s).
  for (const product of named) {
    const calendarName = products[product]?.calendar;
    const calendar = calendarName ? catalogs.calendars?.[calendarName] : undefined;
    if (!calendar) {
      return { ok: false, reason: `product ${product} names calendar '${calendarName ?? "(none)"}', which is not in calendars.yaml` };
    }
    const boundaries = new Set(Object.values(calendar.sessions ?? {}).flatMap(sessionTimes));
    const opens = calendar.opens && calendar.opens.trim().toLowerCase() !== "always" ? calendar.opens : undefined;
    const opensMatch = opens?.match(/(\w+)\s+(\d{1,2}:\d{2})/);
    const opensDay = opensMatch ? normaliseWeekday(opensMatch[1]) : undefined;
    const opensTime = opensMatch?.[2];
    if (claim) {
      if (!opens || opensDay === undefined) {
        return { ok: false, reason: `the calendar '${calendarName}' has no weekly schedule to check a weekday claim against` };
      }
      if (claim.day !== opensDay || claim.time !== opensTime) {
        return { ok: false, reason: `${today.match(/\b(?:Sun|Mon|Tue|Wed|Thu|Fri|Sat)[a-z]*\b/i)?.[0] ?? "the weekday"} ${claim.time} is not ${calendarName}'s weekly reopen (${opens})` };
      }
    }
    for (const time of times) {
      if (!boundaries.has(time) && time !== opensTime) {
        return { ok: false, reason: `${time} is not a ${calendarName} session boundary for ${product}` };
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
  /** Only the deterministic backstop may say the impact was not established;
   * a model brief must answer whether any market stops publishing (finding
   * disc-M-56). */
  allowUnverifiedImpact?: boolean;
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
  if (opts.allowUnverifiedImpact ? !impactAnswersPublishing(impact) && !impactUnestablished(impact) : !impactAnswersPublishing(impact)) {
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
  // Every MODEL brief must carry a recommendation citing the plan or IC (OD-2/
  // D-B-79). Only the deterministic backstop may omit it, which it signals with
  // `allowUnverifiedImpact` (its mode).
  const rec = brief.recommendation;
  if (rec !== undefined) {
    if (!rec || typeof rec !== "object" || !rec.option?.trim() || !rec.why?.trim()) return "a brief's recommendation needs one option and why";
    if (!ids.includes(rec.option.trim())) return `the recommended option '${rec.option}' is not one of the brief's options`;
    // A bare mention of 'plan' is not a citation; require a section reference
    // (`IC §5`, `plan §4`, or `§5`) (finding disc-M-103).
    if (!/(?:\bIC\b|\bplan\b)\s*§?\s*\d|§\s*\d/i.test(rec.why)) return "the recommendation must cite the plan or an IC section";
  } else if (!opts.allowUnverifiedImpact) {
    return "a brief needs one recommended option and why, citing the plan or IC section";
  }
  if (!Array.isArray(brief.evidence) || brief.evidence.filter((e) => typeof e === "string" && e.trim()).length === 0) {
    return "a brief needs the original evidence";
  }
  if (opts.catalogs !== undefined) {
    // The catalog example is checked before the per-claim citation rule, so a
    // wrong weekday or an uncheckable example is reported as such rather than
    // as a missing citation.
    // `(example unverified)` may only excuse a today that names NO market and
    // NO time. If it already names a product or a clock time, the example is
    // checkable, so an inconsistency is refused even with the marker (a
    // wrong reopen must never reach the owner behind it).
    const unverified = today.includes("(example unverified)");
    const check = checkTodayExample(today, opts.catalogs ?? undefined);
    if (!check.ok) {
      const namesTime = /\b\d{1,2}:\d{2}\b/.test(today);
      const namesProduct = opts.catalogs ? Object.keys(opts.catalogs.products ?? {}).some((s) => new RegExp(`\\b${s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\b`).test(today)) : false;
      if (namesTime || namesProduct || !unverified) {
        return `the today example cannot be checked: ${check.reason}; either name a real product and session time or say "(example unverified)"`;
      }
    }
  }
  // The owner-facing text stays plain: no file paths or code identifiers in
  // today, impact or the options (OD-3 / D-M-126). A claim cites the evidence
  // by number (`[n]`) instead, and the path is rendered only under Evidence.
  const ownerText: Array<[string, string]> = [
    ["today", today],
    ["impact", impact],
    ...brief.options.flatMap((o): Array<[string, string]> => [
      [`option ${o.id} label`, o.label],
      [`option ${o.id} effect`, o.effect],
      [`option ${o.id} cost`, o.cost],
    ]),
  ];
  for (const [where, text] of ownerText) {
    if (hasCodeIdentifier(text)) return `the ${where} names code; keep the owner-facing text plain and cite the evidence with [n]`;
  }
  // Every quantified claim must carry its OWN evidence reference, and that
  // evidence must be the config/code it read: one citation cannot license an
  // unrelated `10 s` somewhere else (finding disc-M-85).
  const evidence = brief.evidence ?? [];
  const cited = (claim: string): boolean =>
    [...claim.matchAll(/\[(\d+)\]/g)].some((m) => {
      const n = Number(m[1]);
      return n >= 1 && isEvidenceCitation(evidence[n - 1] ?? "");
    });
  const claims = [
    ...sentences(today),
    ...sentences(impact),
    ...brief.options.flatMap((o) => [o.label, o.effect, o.cost]),
    ...(rec?.why ? [rec.why] : []),
  ];
  for (const claim of claims) {
    if (hasQuantifiedFact(claim) && !cited(claim)) {
      return `the claim "${claim.trim()}" states a time, count or duration without its own evidence reference [n]`;
    }
  }
  // The publishing answer is the claim the owner relies on most, so it must be
  // cited too (OD-3 / D-M-127); only the backstop's honest "unverified" is
  // exempt.
  if (!impactUnestablished(impact) && !cited(impact)) {
    return "the impact's answer to whether any market stops publishing must cite the evidence it was checked against";
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

/** The two option ids the owner's A/D keys name: the option that accepts and
 * the option that refuses. Chosen by the option's own meaning (its id), never
 * by its position in the list, so the mapping is stable however the request
 * orders its options. RET still prompts for any option. */
const ACCEPT_OPTION_IDS = new Set(["approve", "accept", "accept_as_implemented", "accept_risk", "grant", "grant_correction"]);
const REFUSE_OPTION_IDS = new Set(["reject_and_repair", "repair", "stop", "withdraw", "refuse"]);

export function briefVerdictOptions(brief: DecisionBrief): { accept?: string; refuse?: string } {
  const ids = brief.options.map((o) => o.id);
  const accept = ids.find((id) => ACCEPT_OPTION_IDS.has(id)) ?? ids[0];
  const refuse = ids.find((id) => REFUSE_OPTION_IDS.has(id)) ?? ids[ids.length - 1];
  return { ...(accept ? { accept } : {}), ...(refuse ? { refuse } : {}) };
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

/** The command a brief's option sends: a `resolve` (the underlying owner
 * request) or an `override` (a flagged reserved decision). The brief's own
 * `command` decides; the option id is passed through unchanged. */
export function briefCommandFor(brief: DecisionBrief, optionId: string, binding: BriefBinding, request?: OwnerRequest): Record<string, unknown> {
  const command = brief.command ?? "resolve";
  if (command === "override") {
    return {
      type: "override",
      vote: optionId === "approve" ? "approve" : "reject",
      binding: {
        runId: binding.runId,
        phaseId: binding.phaseId,
        recordId: brief.requestId,
        candidateSha: binding.candidateSha,
        recordVersion: binding.recordVersion,
        contractVersion: binding.contractVersion,
      },
    };
  }
  if (command === "entry") {
    // The entry command the review view's A/D writes: `expandEntryCommand`
    // turns it into one OWNER_VERDICT per settleable linked message.
    return { type: "entry-verdict", runId: binding.runId, phaseId: binding.phaseId, entryId: brief.requestId, verdict: optionId === "accept" ? "accept" : "refuse" };
  }
  return briefResolveCommand(brief, optionId, binding, request);
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
/** The runbook's owner glossary, the target of every glossary link in a brief. */
export const GLOSSARY_LINK = "glossary.org::Owner glossary";

/** The glossary terms BRIEF actually uses, so the renderer links exactly those
 * and never explains a term inline (goal item 4). */
export function glossaryTermsIn(brief: DecisionBrief): string[] {
  const text = [brief.question, brief.today, brief.impact, ...brief.options.flatMap((o) => [o.label, o.effect, o.cost]), brief.recommendation?.why ?? ""].join(" ");
  return BRIEF_GLOSSARY.filter((g) => new RegExp(`\\b${g.term.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\b`).test(text)).map((g) => g.term);
}

/** One related item as an Org link the owner can follow (goal item 3): an entry
 * or message links to its own detail file beside `views/review.org`, anything
 * else links to its brief heading. */
export function relatedLink(r: { id: string; question: string }): string {
  const label = `${r.id} — ${r.question}`;
  if (/^E-/.test(r.id)) return `[[file:entries/${r.id}.org][${label}]]`;
  if (/^[TFB]-[0-9]+$/.test(r.id)) return `[[file:messages/${r.id}.org][${label}]]`;
  return `[[*${r.question.replace(/]/g, ")")}][${label}]]`;
}

/** `views/review.org`: one owner item's brief as an Org subtree. The question
 * is the heading; today / impact / options / recommendation are short body
 * paragraphs, `related` links the other open items, and the original evidence
 * is in the same folded body (TAB reveals it). The property drawer carries the
 * request id and — when a binding is given — the same tuple a resolve command
 * needs, so choosing an option from the brief sends exactly the resolve
 * command the request sends. */
export function renderBriefOrg(brief: DecisionBrief, opts: { request?: OwnerRequest; binding?: BriefBinding; heading?: string; tag?: string; recordVersion?: number } = {}): string {
  const heading = opts.heading ?? "**";
  const indent = " ".repeat(heading.length + 1);
  const verdict = briefVerdictOptions(brief);
  const recordVersion = opts.recordVersion ?? opts.request?.version;
  const lines: string[] = [];
  lines.push(`${heading} ${brief.question}${opts.tag ? `  [${opts.tag}]` : ""}`);
  lines.push(`${indent}:PROPERTIES:`);
  lines.push(`${indent}:ID:       ${brief.requestId}`);
  lines.push(`${indent}:KIND:     brief`);
  lines.push(`${indent}:COMMAND:  ${brief.command ?? "resolve"}`);
  lines.push(`${indent}:OPTIONS:  ${brief.options.map((o) => o.id).join(",")}`);
  if (verdict.accept) lines.push(`${indent}:ACCEPT_OPTION: ${verdict.accept}`);
  if (verdict.refuse) lines.push(`${indent}:REFUSE_OPTION: ${verdict.refuse}`);
  if (opts.tag) lines.push(`${indent}:PHASE:    ${opts.tag}`);
  lines.push(`${indent}:QUESTION: ${brief.question}`);
  if (opts.binding && recordVersion !== undefined) {
    lines.push(`${indent}:RECORD_VERSION: ${recordVersion}`);
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
    // `recommendation` is optional (the backstop omits it), so it is read
    // defensively here; the `[id]` sits right after the plain label so RET can
    // complete on the label alone (finding A-14).
    const chosen = brief.recommendation?.option === option.id ? " (recommended)" : "";
    lines.push(`${indent}- ${option.label} [${option.id}]${chosen} — ${option.effect} Cost: ${option.cost}`);
  }
  if (brief.recommendation) {
    lines.push(
      `${indent}Recommendation: ${brief.options.find((o) => o.id === brief.recommendation!.option)?.label ?? brief.recommendation.option} — ${brief.recommendation.why}`,
    );
  } else {
    // The backstop says, in plain words where the recommendation would be,
    // why there is none (OD-3 / D-B-79).
    lines.push(`${indent}No recommendation: the brief writer was unavailable${brief.noRecommendationReason ? ` (${brief.noRecommendationReason})` : ""}; decide from the evidence.`);
  }
  if (brief.related.length > 0) {
    lines.push(`${indent}Related:`);
    for (const r of brief.related) lines.push(`${indent}- ${relatedLink(r)}`);
  }
  // Goal (4): a glossary term the brief uses is linked, never explained inline.
  const terms = glossaryTermsIn(brief);
  if (terms.length > 0) {
    lines.push(`${indent}Glossary: ${terms.map((t) => `[[file:${GLOSSARY_LINK}][${t}]]`).join(" ")}`);
  }
  lines.push(`${indent}Evidence (original):`);
  brief.evidence.forEach((ev, i) => lines.push(`${indent}- [${i + 1}] ${ev}`));
  return lines.join("\n");
}

/** The `* Needs you (N)` section the review view puts above every other
 * section: one brief per open owner item. */
export function renderBriefsSection(
  briefs: readonly DecisionBrief[],
  opts: { requestFor?: (id: string) => OwnerRequest | undefined; binding?: BriefBinding; recordVersionFor?: (id: string) => number | undefined } = {},
): string[] {
  if (briefs.length === 0) return [];
  const lines = [`* Needs you (${briefs.length})`];
  for (const brief of briefs) {
    lines.push(
      renderBriefOrg(brief, {
        request: opts.requestFor?.(brief.requestId),
        binding: opts.binding,
        recordVersion: opts.recordVersionFor?.(brief.requestId),
      }),
      "",
    );
  }
  return lines;
}

/** A deterministic brief derived from an owner request when the evaluator did
 * not (or could not) produce one. It never invents a market time and never
 * asserts an unchecked impact (it says the impact was not established); if no
 * catalog is available, `today` says the example is unverified. The option ids
 * are the request's own, so resolving is unchanged. */
export function fallbackBrief(
  request: OwnerRequest,
  opts: { question?: string; catalogs?: Catalogs | null; allItems?: readonly OpenItemConcern[]; files?: string[]; planRefs?: string[]; noRecommendationReason?: string } = {},
): DecisionBrief {
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
    ? "The plan's calendars were available, but no concrete example was recorded. (example unverified)"
    : "No readable calendars were available when this brief was written, so the example could not be checked. (example unverified)";
  const related = opts.allItems
    ? relatedOpenItems({ id: request.id, question, files: opts.files, planRefs: opts.planRefs }, opts.allItems).map((r) => ({ id: r.id, question: r.question }))
    : [];
  return {
    requestId: request.id,
    question,
    today,
    // The backstop never asserts what it did not check.
    impact: "Whether any market stops publishing is not established by this backstop; the request's own evidence is below.",
    noRecommendationReason: opts.noRecommendationReason ?? "the brief writer did not run",
    // No recommendation: the backstop never recommends an option by position
    // without a citation (finding M-31).
    // Plain labels only: the request's own label may carry a count
    // ("grant 3 rounds") that would then demand a citation this backstop
    // never read. The underlying option id is untouched, so resolving is
    // unchanged.
    options: request.options.map((o) => ({
      id: o.id,
      // Plain labels only: a count in the request's own label ("grant 3
      // rounds") would otherwise state an uncited number.
      label: o.label.replace(/\s*\([^)]*\)\s*$/, ""),
      effect: o.label.replace(/\s*\([^)]*\)\s*$/, ""),
      cost: "as the request's own option defines it",
    })),
    related,
    evidence: [`message: ${request.reason}`],
  };
}

/** A deterministic brief for a flagged reserved decision the evaluator did
 * not brief. Its options are the decision's own owner commands (`approve` /
 * `reject_and_repair`), and it carries `command: override`, so A/D/RET send
 * an override command instead of a resolve. */
export function fallbackDecisionBrief(
  decision: { id: string; choice: string; whyItMatters?: string },
  opts: { allItems?: readonly OpenItemConcern[]; files?: string[]; planRefs?: string[]; noRecommendationReason?: string } = {},
): DecisionBrief {
  const plain = (decision.choice ?? "")
    .replace(PATH_LIKE, "the code")
    .replace(CODE_EXT, "the code")
    .replace(SNAKE_CASE, "that setting")
    .replace(/\d+/g, "")
    .replace(/\s+/g, " ")
    .trim();
  const question = `Should this choice stand? ${plain}`;
  const related = opts.allItems
    ? relatedOpenItems({ id: decision.id, question, files: opts.files, planRefs: opts.planRefs }, opts.allItems).map((r) => ({ id: r.id, question: r.question }))
    : [];
  return {
    requestId: decision.id,
    command: "override",
    question,
    today: "No concrete example was recorded for this flagged choice. (example unverified)",
    impact: "Whether any market stops publishing is not established by this backstop; the decision's own reasoning is what the reviewers voted on.",
    noRecommendationReason: opts.noRecommendationReason ?? "the brief writer did not run",
    options: [
      { id: "approve", label: "Approve it", effect: "the choice stands", cost: "none beyond what the choice already does" },
      { id: "reject_and_repair", label: "Reject and repair", effect: "a new attempt revisits the choice with a further allowance", cost: "a further repair round" },
    ],
    related,
    evidence: [`decision: ${decision.id} ${plain}`],
  };
}

/** A deterministic brief for a live review entry the owner must settle. Its
 * options are the entry's own owner commands (`accept` / `refuse`), and it
 * carries `command: entry`, so A/D/RET settle the entry the same way the
 * review view does. */
export function fallbackEntryBrief(
  entry: { id: string; title: string; messages?: ReadonlyArray<{ id: string; title?: string; evidence?: string[] }> },
  opts: { allItems?: readonly OpenItemConcern[]; files?: string[]; planRefs?: string[]; noRecommendationReason?: string } = {},
): DecisionBrief {
  const plain = (entry.title ?? "")
    .replace(PATH_LIKE, "the code")
    .replace(CODE_EXT, "the code")
    .replace(SNAKE_CASE, "that setting")
    .replace(/\d+/g, "")
    .replace(/\s+/g, " ")
    .trim();
  const question = `Should this stand? ${plain}`;
  const related = opts.allItems
    ? relatedOpenItems({ id: entry.id, question, files: opts.files, planRefs: opts.planRefs }, opts.allItems).map((r) => ({ id: r.id, question: r.question }))
    : [];
  const evidence = (entry.messages ?? []).map((m) => `message: ${m.id} ${(m.title ?? "").replace(/\s+/g, " ").trim()}`.trim());
  return {
    requestId: entry.id,
    command: "entry",
    question,
    today: "No concrete example was recorded for this entry. (example unverified)",
    impact: "Whether any market stops publishing is not established by this backstop; the entry's own linked messages are below.",
    noRecommendationReason: opts.noRecommendationReason ?? "the brief writer did not run",
    options: [
      { id: "accept", label: "Accept it", effect: "the entry is settled as it stands", cost: "none beyond what it already does" },
      { id: "refuse", label: "Refuse it", effect: "the entry is refused; your reason reaches the next worker attempt", cost: "a further repair round" },
    ],
    related,
    evidence: evidence.length > 0 ? evidence : [`entry: ${entry.id} ${plain}`],
  };
}

/** Merge the conductor's own same-concern computation into a brief's
 * `related`, so a bigger silence (e.g. T-54) is never hidden behind the
 * model's own list (finding A-29). */
export function enrichBriefRelated(brief: DecisionBrief, allItems: readonly OpenItemConcern[]): DecisionBrief {
  const concern = allItems.find((c) => c.id === brief.requestId);
  if (!concern) return brief;
  const byId = new Map(brief.related.map((r) => [r.id, r]));
  for (const r of relatedOpenItems(concern, allItems)) if (!byId.has(r.id)) byId.set(r.id, { id: r.id, question: r.question });
  return { ...brief, related: [...byId.values()] };
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
    const opens = typeof value.opens === "string" ? value.opens : undefined;
    const sessions: Record<string, string> = {};
    const rawSessions = value.sessions;
    if (rawSessions && typeof rawSessions !== "string") {
      for (const [session, spec] of Object.entries(rawSessions)) if (typeof spec === "string") sessions[session] = spec;
    }
    out[name] = { ...(timezone ? { timezone } : {}), ...(opens ? { opens } : {}), sessions };
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
