// Plan 06b: every point of a plan is a parsed, identified item the loop
// carries mechanically from the plan to the worker, the checks, the reviewers
// and the acceptance decision (refs/06_ref_plan_format.md).
//
// A phase subtree names four headings — Goal, Architecture, Requirements,
// Constraints — and every item under the last three is a subheading with an
// `:ID:`. This module is the pure half of that contract: it parses the
// `:VERIFY:` strings, validates the worker's `submit_coverage`, resolves the
// `test` verifies against the check run's output, validates a reviewer's item
// verdicts by code, tallies them by seat, and renders the item-by-seat matrix
// and the status counts. No I/O, no clock, no model: the conductor, `tt
// summary`, `tt lint` and the Emacs front end all project from these
// functions so there is exactly one implementation of each rule.
//
// The old format (a `Goal:` paragraph, an `Acceptance:` list and
// `:RESERVED:`) still parses: `itemsFromPhase` synthesizes `R1..Rn` from the
// acceptance list (each `review`, or `evidence` for an item that starts
// `evidence:`) and `C1` from `:RESERVED:`. No existing plan changes meaning.

import type { Reviewer } from "./types.ts";

// ---------------------------------------------------------------------------
// The plan model
// ---------------------------------------------------------------------------

/** An architecture item's kind tag (ref doc: data, rule, flow, boundary). */
export const ARCH_TAGS = ["data", "rule", "flow", "boundary"] as const;
export type ArchTag = (typeof ARCH_TAGS)[number];

export interface ArchitectureItem {
  id: string;
  title: string;
  text: string;
  tags: string[];
  /** The module/area the item lives in (`:WHERE:`). Optional. */
  where?: string;
  /** The exact Org body, when the parser recorded it. Lint uses it to prove
   * no text was lost; a hand-written JSON plan may omit it. */
  rawText?: string;
  /** 1-based line of the item's headline in the source Org file. */
  line?: number;
}

export interface RequirementItem {
  id: string;
  title: string;
  text: string;
  /** The architecture item ids this requirement is realised by (`:ARCH:`). */
  arch: string[];
  /** The raw `:VERIFY:` strings, in order. Empty = `review`. */
  verify: string[];
  rawText?: string;
  line?: number;
}

export interface ConstraintItem {
  id: string;
  title: string;
  text: string;
  verify: string[];
  rawText?: string;
  line?: number;
}

/** The structured plan of one phase, exactly what `plan.json` carries. */
export interface PlanItems {
  goal: string;
  architecture: ArchitectureItem[];
  requirements: RequirementItem[];
  constraints: ConstraintItem[];
}

/** The slice of a phase `itemsFromPhase` needs. `RunPlanPhase`/`PlanPhase`
 * are structurally assignable, so callers pass a parsed plan directly. */
export interface PhaseItemInput {
  goal?: string;
  acceptance?: string[];
  reserved?: string[];
  architecture?: ArchitectureItem[];
  requirements?: RequirementItem[];
  constraints?: ConstraintItem[];
}

export type ItemKind = "architecture" | "requirement" | "constraint";

/** One item of any kind, flattened for prompts, coverage and verdicts. */
export interface FlatItem {
  kind: ItemKind;
  id: string;
  title: string;
  text: string;
  arch: string[];
  verify: string[];
  where?: string;
  tags: string[];
}

/** The old-format `Acceptance:` item that asks the owner for a recording. */
export function isEvidenceAcceptanceItem(item: string): boolean {
  return /^\s*evidence\s*:/i.test(item);
}

/** The old-format item's text without its leading `evidence:` marker. */
export function acceptanceItemText(item: string): string {
  return isEvidenceAcceptanceItem(item) ? item.replace(/^\s*evidence\s*:\s*/i, "") : item;
}

/** The plan's structured items, synthesizing the old format when the phase
 * carries only an acceptance list. This is the one place the two formats
 * meet: everything downstream reads a `PlanItems`. */
export function itemsFromPhase(phase: PhaseItemInput): PlanItems {
  const architecture = phase.architecture ?? [];
  if ((phase.requirements?.length ?? 0) > 0 || (phase.constraints?.length ?? 0) > 0 || architecture.length > 0) {
    return {
      goal: phase.goal ?? "",
      architecture,
      requirements: phase.requirements ?? [],
      constraints: phase.constraints ?? [],
    };
  }
  const requirements: RequirementItem[] = (phase.acceptance ?? []).map((text, i) => ({
    id: `R${i + 1}`,
    title: text,
    text,
    arch: [],
    verify: [isEvidenceAcceptanceItem(text) ? "evidence" : "review"],
  }));
  const reserved = (phase.reserved ?? []).filter((r) => r.trim().length > 0);
  const constraints: ConstraintItem[] = reserved.length > 0
    ? [{ id: "C1", title: reserved.join("; "), text: reserved.join("; "), verify: ["review"] }]
    : [];
  return { goal: phase.goal ?? "", architecture, requirements, constraints };
}

/** Every item, architecture first, then requirements, then constraints. */
export function flatItems(items: PlanItems): FlatItem[] {
  return [
    ...items.architecture.map((a) => ({ kind: "architecture" as const, id: a.id, title: a.title, text: a.text, arch: [], verify: [], where: a.where, tags: a.tags })),
    ...items.requirements.map((r) => ({ kind: "requirement" as const, id: r.id, title: r.title, text: r.text, arch: r.arch, verify: r.verify, tags: [] })),
    ...items.constraints.map((c) => ({ kind: "constraint" as const, id: c.id, title: c.title, text: c.text, arch: [], verify: c.verify, tags: [] })),
  ];
}

/** The R and C items (the ones a worker's coverage and a reviewer's item
 * verdicts must cover). */
export function requirementAndConstraintItems(items: PlanItems): FlatItem[] {
  return flatItems(items).filter((i) => i.kind !== "architecture");
}

/** One item by id, any kind. */
export function itemById(items: PlanItems, id: string): FlatItem | undefined {
  return flatItems(items).find((i) => i.id === id);
}

/** Every architecture id. */
export function architectureIds(items: PlanItems): Set<string> {
  return new Set(items.architecture.map((a) => a.id));
}

// ---------------------------------------------------------------------------
// VERIFY kinds
// ---------------------------------------------------------------------------

export type Verify = { kind: "test"; name: string; file?: string } | { kind: "review" } | { kind: "evidence" };

/** Parse one `:VERIFY:` string into its kinds. `review` is the default when
 * the string is empty. `test "name"` and `test file::name` are both accepted;
 * a bare `test` (no name) parses to a nameless test verify, which lint
 * rejects. Several kinds may be listed (`test "…" review`). */
export function parseVerify(raw: string | undefined): Verify[] {
  const text = (raw ?? "").trim();
  if (text.length === 0) return [{ kind: "review" }];
  const out: Verify[] = [];
  const re = /(\btest\b)(?:\s+(?:"([^"]*)"|(\S+)))?|(\breview\b)|(\bevidence\b)/gi;
  let m: RegExpExecArray | null;
  while ((m = re.exec(text)) !== null) {
    if (m[1] !== undefined) {
      const rawName = m[2] ?? m[3] ?? "";
      const name = rawName.trim();
      const sep = name.indexOf("::");
      if (sep > 0) out.push({ kind: "test", file: name.slice(0, sep), name: name.slice(sep + 2) });
      else out.push({ kind: "test", name });
    } else if (m[4] !== undefined) {
      out.push({ kind: "review" });
    } else if (m[5] !== undefined) {
      out.push({ kind: "evidence" });
    }
  }
  return out.length > 0 ? out : [{ kind: "review" }];
}

/** Every verify of one item's raw strings. */
export function itemVerifies(item: FlatItem): Verify[] {
  const raw = item.verify.length > 0 ? item.verify : ["review"];
  return raw.flatMap((v) => parseVerify(v));
}

/** The test verifies of one item. */
export function itemTestVerifies(item: FlatItem): Array<{ kind: "test"; name: string; file?: string }> {
  return itemVerifies(item).filter((v): v is { kind: "test"; name: string; file?: string } => v.kind === "test");
}

/** True when an item must be recorded by the owner before acceptance. */
export function itemNeedsEvidence(item: FlatItem): boolean {
  return itemVerifies(item).some((v) => v.kind === "evidence");
}

/** One short human label for a verify list: `test "x", review`. */
export function verifyLabel(verify: readonly string[]): string {
  const kinds = parseVerify(verify.join(" ")).map((v) =>
    v.kind === "test" ? `test "${v.name || "(no name)"}"` : v.kind,
  );
  return kinds.join(", ");
}

// ---------------------------------------------------------------------------
// The worker's coverage
// ---------------------------------------------------------------------------

export type CoverageStatus = "done" | "partial" | "not_done";

export interface CoverageEntry {
  id: string;
  status: CoverageStatus;
  where: string[];
  tests: string[];
  note?: string;
}

export interface ArchCoverageEntry {
  id: string;
  fits: "yes" | "deviates";
  where: string[];
  note?: string;
}

/** The worker's `submit_coverage` payload. */
export interface Coverage {
  items: CoverageEntry[];
  arch: ArchCoverageEntry[];
}

export function emptyCoverage(): Coverage {
  return { items: [], arch: [] };
}

function isStatus(v: unknown): v is CoverageStatus {
  return v === "done" || v === "partial" || v === "not_done";
}

/** Why a coverage payload is incomplete or malformed: a missing id, an
 * unknown id, a bad status or a missing note on a partial/not_done item or a
 * deviating architecture item. Empty means complete. */
export function coverageIssues(coverage: Coverage | undefined, items: PlanItems): string[] {
  const out: string[] = [];
  if (!coverage) return ["no coverage was submitted"];
  const need = requirementAndConstraintItems(items);
  const seen = new Map<string, CoverageEntry>();
  for (const entry of coverage.items ?? []) {
    if (!entry || typeof entry.id !== "string") {
      out.push("a coverage entry has no id");
      continue;
    }
    if (seen.has(entry.id)) out.push(`coverage names ${entry.id} more than once`);
    seen.set(entry.id, entry);
    if (!isStatus(entry.status)) out.push(`coverage for ${entry.id} has no valid status (done, partial or not_done)`);
    if ((entry.status === "partial" || entry.status === "not_done") && !(entry.note ?? "").trim()) {
      out.push(`coverage for ${entry.id} is ${entry.status} but carries no note`);
    }
  }
  for (const item of need) {
    if (!seen.has(item.id)) out.push(`coverage omits ${item.id} (${item.title})`);
  }
  const known = new Set(need.map((i) => i.id));
  for (const id of seen.keys()) if (!known.has(id)) out.push(`coverage names the unknown item ${id}`);
  const archSeen = new Map<string, ArchCoverageEntry>();
  for (const entry of coverage.arch ?? []) {
    if (!entry || typeof entry.id !== "string") {
      out.push("an architecture coverage entry has no id");
      continue;
    }
    if (archSeen.has(entry.id)) out.push(`architecture coverage names ${entry.id} more than once`);
    archSeen.set(entry.id, entry);
    if (entry.fits !== "yes" && entry.fits !== "deviates") out.push(`architecture coverage for ${entry.id} says neither fits nor deviates`);
    if (entry.fits === "deviates" && !(entry.note ?? "").trim()) {
      out.push(`architecture coverage for ${entry.id} deviates but carries no note`);
    }
  }
  for (const arch of items.architecture) {
    if (!archSeen.has(arch.id)) out.push(`architecture coverage omits ${arch.id} (${arch.title})`);
  }
  const archKnown = new Set(items.architecture.map((a) => a.id));
  for (const id of archSeen.keys()) if (!archKnown.has(id)) out.push(`architecture coverage names the unknown item ${id}`);
  return out;
}

/** True when the coverage covers every R, C and A with the required notes. */
export function coverageComplete(coverage: Coverage | undefined, items: PlanItems): boolean {
  return coverageIssues(coverage, items).length === 0;
}

/** The note a partial/not_done/deviating coverage entry must carry, as the
 * trade-off message's text. One line per item, in plan order. */
export function coverageNoteLines(coverage: Coverage, items: PlanItems): Array<{ id: string; note: string }> {
  const out: Array<{ id: string; note: string }> = [];
  for (const item of requirementAndConstraintItems(items)) {
    const entry = coverage.items?.find((e) => e.id === item.id);
    if (entry && (entry.status === "partial" || entry.status === "not_done") && (entry.note ?? "").trim()) {
      out.push({ id: item.id, note: `${item.id} ${entry.status}: ${entry.note!.trim()}` });
    }
  }
  for (const arch of items.architecture) {
    const entry = coverage.arch?.find((e) => e.id === arch.id);
    if (entry?.fits === "deviates" && (entry.note ?? "").trim()) {
      out.push({ id: arch.id, note: `${arch.id} deviates: ${entry.note!.trim()}` });
    }
  }
  return out;
}

// ---------------------------------------------------------------------------
// Checks: resolving `test` verifies against the check run's output
// ---------------------------------------------------------------------------

export type TestOutcome = "passed" | "failed" | "missing";

/** One test verify's resolution. */
export interface VerifyResolution {
  id: string;
  name: string;
  file?: string;
  outcome: TestOutcome;
}


/** Whether the check run's combined output names `name` as passing, failing or
 * not at all. The name must be the reporter's own test-name token, not a
 * substring of a longer one (finding M-14): `✔ name (1.2ms)` and TAP's
 * `ok N - name` both match, while `lanes: tally` never matches `lanes: tally
 * extended`. A failure anywhere wins over a pass. */
export function testOutcomeIn(output: string, name: string): TestOutcome {
  if (name.trim().length === 0) return "missing";
  const escaped = name.trim().replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const nodePass = new RegExp(`^\\s*(?:✔|✓)\\s+${escaped}(?:\\s*\\(.*\\))?\\s*$`);
  const nodeFail = new RegExp(`^\\s*(?:✖|✗|×)\\s+${escaped}(?:\\s*\\(.*\\))?\\s*$`);
  const tapPass = new RegExp(`^\\s*ok\\s+\\d+\\s+-\\s+${escaped}(?:\\s*#.*)?\\s*$`);
  const tapFail = new RegExp(`^\\s*not\\s+ok\\s+\\d+\\s+-\\s+${escaped}(?:\\s*#.*)?\\s*$`);
  let passed = false;
  for (const line of output.split("\n")) {
    if (tapFail.test(line) || nodeFail.test(line)) return "failed";
    if (tapPass.test(line) || nodePass.test(line)) passed = true;
  }
  return passed ? "passed" : "missing";
}

/** Resolve every `test` verify of every R, C (and A) item against the check
 * run's output. An item with no test verify produces no rows. */
export function resolveTestVerifies(items: PlanItems, checkOutput: string): VerifyResolution[] {
  const out: VerifyResolution[] = [];
  for (const item of flatItems(items)) {
    for (const v of itemTestVerifies(item)) {
      out.push({ id: item.id, name: v.name, ...(v.file ? { file: v.file } : {}), outcome: testOutcomeIn(checkOutput, v.name) });
    }
  }
  return out;
}

/** The failing/missing test verifies that block acceptance: `id — test
 * "name" (missing|failed)`. Empty when every named test passed. */
export function testVerifyProblems(resolutions: readonly VerifyResolution[]): string[] {
  return resolutions
    .filter((r) => r.outcome !== "passed")
    .map((r) => `${r.id} — test "${r.name}" ${r.outcome === "missing" ? "is missing from the check output" : "failed"}`);
}

/** The ids whose `test` verify is a problem, for anchoring a finding. */
export function testVerifyProblemIds(resolutions: readonly VerifyResolution[]): string[] {
  return [...new Set(resolutions.filter((r) => r.outcome !== "passed").map((r) => r.id))];
}

// ---------------------------------------------------------------------------
// Reviewer item verdicts
// ---------------------------------------------------------------------------

export type ItemVerdictValue = "met" | "unmet" | "partial";
export type ArchVerdictValue = "fits" | "deviates" | "unclear";

export interface ItemVerdict {
  id: string;
  verdict: ItemVerdictValue;
  evidence: string;
  note?: string;
}

export interface ArchVerdict {
  id: string;
  verdict: ArchVerdictValue;
  evidence: string;
  note?: string;
}

/** The reviewer's `submit_review` item section. */
export interface ReviewItems {
  items: ItemVerdict[];
  arch: ArchVerdict[];
}

export function isItemVerdictValue(v: unknown): v is ItemVerdictValue {
  return v === "met" || v === "unmet" || v === "partial";
}

export function isArchVerdictValue(v: unknown): v is ArchVerdictValue {
  return v === "fits" || v === "deviates" || v === "unclear";
}

/** Why a review's item section is incomplete: an R or C with no verdict, an A
 * with none, a bad verdict value, or a non-met verdict without evidence.
 * Empty means complete — the existing complete-ballot rule then treats a
 * missing id like a missing ballot. */
export function reviewItemsIssues(review: ReviewItems | undefined, items: PlanItems): string[] {
  const out: string[] = [];
  if (!review) return ["the review carries no item verdicts"];
  const need = requirementAndConstraintItems(items);
  const seen = new Set<string>();
  for (const v of review.items ?? []) {
    if (!v || typeof v.id !== "string") {
      out.push("an item verdict has no id");
      continue;
    }
    if (seen.has(v.id)) out.push(`item verdict for ${v.id} appears more than once`);
    seen.add(v.id);
    if (!isItemVerdictValue(v.verdict)) out.push(`item verdict for ${v.id} is not met, unmet or partial`);
  }
  for (const item of need) if (!seen.has(item.id)) out.push(`the review omits a verdict for ${item.id} (${item.title})`);
  const known = new Set(need.map((i) => i.id));
  for (const id of seen) if (!known.has(id)) out.push(`the review names the unknown item ${id}`);

  const archSeen = new Set<string>();
  for (const v of review.arch ?? []) {
    if (!v || typeof v.id !== "string") {
      out.push("an architecture verdict has no id");
      continue;
    }
    if (archSeen.has(v.id)) out.push(`architecture verdict for ${v.id} appears more than once`);
    archSeen.add(v.id);
    if (!isArchVerdictValue(v.verdict)) out.push(`architecture verdict for ${v.id} is not fits, deviates or unclear`);
  }
  for (const a of items.architecture) if (!archSeen.has(a.id)) out.push(`the review omits a verdict for ${a.id} (${a.title})`);
  const archKnown = new Set(items.architecture.map((a) => a.id));
  for (const id of archSeen) if (!archKnown.has(id)) out.push(`the review names the unknown architecture item ${id}`);
  return out;
}

// ---------------------------------------------------------------------------
// Code validation of a verdict (layer 1)
// ---------------------------------------------------------------------------

/** A file:line-range anchor parsed out of an evidence string. */
export interface FileAnchor {
  path: string;
  start: number;
  end: number;
}

const FILE_ANCHOR_RE = /(^|[\s("'`[])((?:[\w.@~/-]+\/)?[\w.@-]+\.[A-Za-z0-9_]+):(\d+)(?:-(\d+))?/g;

/** Every `file:line` / `file:line-range` anchor in an evidence string. */
export function evidenceFileAnchors(evidence: string): FileAnchor[] {
  const out: FileAnchor[] = [];
  for (const m of evidence.matchAll(FILE_ANCHOR_RE)) {
    const start = Number(m[3]);
    const end = m[4] !== undefined ? Number(m[4]) : start;
    out.push({ path: m[2], start, end: Math.max(start, end) });
  }
  return out;
}

/** The `test "name"` names cited in an evidence string. */
export function evidenceTestNames(evidence: string): string[] {
  const out: string[] = [];
  for (const m of evidence.matchAll(/\btest\s+"([^"]+)"/g)) out.push(m[1]);
  for (const m of evidence.matchAll(/\btest\s+([\w./:-]+::[\w.:-]+)/g)) {
    const name = m[1];
    out.push(name.includes("::") ? name.slice(name.indexOf("::") + 2) : name);
  }
  return out;
}

/** A command a reviewer ran, quoted as `` `…` `` in its evidence. */
export function evidenceCommands(evidence: string): string[] {
  const out: string[] = [];
  for (const m of evidence.matchAll(/`([^`]+)`/g)) {
    const cmd = m[1].trim();
    if (/(?:^|\s)(?:node|npm|make|cargo|python|pytest|go|git|sh|bash|deno|bun|yarn|pnpm)\b/.test(cmd)) out.push(cmd);
  }
  return out;
}

/** True when an evidence string carries at least one anchor a code check can
 * follow: a file:line range, a test name or a command the reviewer ran. */
export function evidenceHasAnchor(evidence: string): boolean {
  return evidenceFileAnchors(evidence).length > 0 || evidenceTestNames(evidence).length > 0 || evidenceCommands(evidence).length > 0;
}

/** What the code can read about the candidate and this review's own tool
 * calls, so a verdict can be validated without a model. */
export interface VerdictContext {
  /** Line count of a candidate file, or undefined when it does not exist. */
  lineCount: (path: string) => number | undefined;
  /** The candidate's changed files (diff paths, repo-relative). */
  diffFiles: readonly string[];
  /** The item's `:WHERE:` text, when it names a file or glob. */
  where?: string;
  /** The check run's test resolutions, by test name. */
  testOutcomes: ReadonlyMap<string, TestOutcome>;
  /** Files this reviewer itself read in this review (from its tool calls). */
  reviewerReadFiles: readonly string[];
  /** The worker's own coverage anchors, which a met/fits verdict may not
   * lean on alone. */
  workerAnchors: readonly string[];
  /** Plan 06b (finding B-18): for an architecture item, whether the
   * candidate's `:WHERE:` file exists and names every symbol the item
   * declares. A majority `deviates` contradicted by this is overturned. */
  archSymbolsPresent?: (item: FlatItem) => boolean;
}

/** True when `path` (or the `:WHERE:` text) matches one of `files`. A
 * `file:line` anchor and a bare `file` name match each other: the line is a
 * locator, not part of the path. */
function pathInList(path: string, files: readonly string[]): boolean {
  const norm = (p: string) => p.replace(/^(\.\/)+/, "").replace(/:\d+(?:-\d+)?$/, "");
  const p = norm(path);
  return files.some((f) => {
    const q = norm(f);
    return p === q || p.endsWith(`/${q}`) || q.endsWith(`/${p}`);
  });
}

function whereMatches(path: string, where: string | undefined): boolean {
  if (!where) return false;
  const tokens = where.split(/[\s,;]+/).filter(Boolean);
  return tokens.some((t) => {
    const glob = t.endsWith("/**") ? t.slice(0, -3) : t;
    if (glob.length === 0) return false;
    return path === glob || path.startsWith(`${glob}/`) || path.endsWith(`/${glob}`);
  });
}

/** Validate one verdict against the candidate by code. Returns the reasons it
 * must be refused and re-asked; empty means the verdict counts. */
export function verdictIssues(
  item: FlatItem,
  verdict: ItemVerdict | ArchVerdict,
  ctx: VerdictContext,
): string[] {
  const out: string[] = [];
  const evidence = verdict.evidence ?? "";
  const value = verdict.verdict;
  if (!evidenceHasAnchor(evidence)) {
    out.push(`${item.id}: the verdict cites no anchor (a file:line-range, a test name, or a command you ran)`);
    return out;
  }
  // Every cited file and line range must exist in the candidate.
  const anchors = evidenceFileAnchors(evidence);
  for (const a of anchors) {
    const lines = ctx.lineCount(a.path);
    if (lines === undefined) {
      out.push(`${item.id}: the cited file ${a.path} does not exist in the candidate`);
      continue;
    }
    if (a.start < 1 || a.end > lines) {
      out.push(`${item.id}: the cited lines ${a.path}:${a.start}-${a.end} do not exist in the candidate (it has ${lines} lines)`);
    }
  }
  // A cited test must have passed in this check run.
  for (const name of evidenceTestNames(evidence)) {
    const outcome = ctx.testOutcomes.get(name);
    if (outcome !== "passed") {
      out.push(`${item.id}: the cited test "${name}" ${outcome === undefined || outcome === "missing" ? "is not in this check run" : "did not pass in this check run"}`);
    }
  }
  const isMet = value === "met" || value === "fits";
  if (isMet && anchors.length > 0) {
    const touchesDiff = anchors.some((a) => pathInList(a.path, ctx.diffFiles));
    const touchesWhere = anchors.some((a) => whereMatches(a.path, ctx.where));
    if (!touchesDiff && !touchesWhere) {
      out.push(`${item.id}: the ${value} anchor touches neither the candidate's diff nor the item's :WHERE:`);
    }
  }
  if (isMet) {
    const ownRead = anchors.some((a) => pathInList(a.path, ctx.reviewerReadFiles));
    if (!ownRead) {
      out.push(`${item.id}: a ${value} verdict must cite at least one file you read yourself in this review, not only the worker's anchors`);
    }
  }
  return out;
}

// ---------------------------------------------------------------------------
// The evaluator's re-verification (layer 3)
// ---------------------------------------------------------------------------

/** One seat's verdict on one item, as recorded in its review. */
export interface SeatItemVerdict {
  seat: Reviewer;
  verdict: ItemVerdictValue;
  evidence: string;
}

/** An overturned verdict: the evaluator's re-verification contradicted it.
 * `flip` is a contradicted `unmet`/`deviates` (the code proves the opposite);
 * `drop` is a thin `met`/`fits` the audit withdraws (it no longer counts). */
export interface Overturn {
  seat: Reviewer;
  id: string;
  kind: ItemKind;
  /** The verdict the seat gave. */
  verdict: string;
  effect: "flip" | "drop";
  reason: string;
}

/** The evaluator's re-verification of a majority `unmet`/`deviates` verdict
 * against the candidate, and of a unanimous `met`/`fits` verdict whose
 * evidence is thin. Pure: `ctx` carries the code facts. A majority `unmet` for
 * an item whose own `test` verify PASSED is contradicted by the code and is
 * overturned; a unanimous `met` whose only anchors are the worker's is
 * overturned. Every overturn is counted against its seat. */
export function reverify(
  item: FlatItem,
  verdicts: readonly SeatItemVerdict[],
  ctx: VerdictContext,
  /** The files each seat itself read, keyed by seat. A unanimous met verdict
   * is only "thin" when every seat's cited anchors are the worker's own and
   * none is a file that seat read. */
  readsBySeat: Record<string, readonly string[]> = {},
): Overturn[] {
  if (verdicts.length === 0) return [];
  const contradicted =
    item.kind === "architecture" ? verdicts.filter((v) => v.verdict === "deviates") : verdicts.filter((v) => v.verdict === "unmet");
  if (contradicted.length >= 2) {
    const tests = itemTestVerifies(item);
    const testPassed = tests.length > 0 && tests.every((t) => ctx.testOutcomes.get(t.name) === "passed");
    // An architecture item whose :WHERE: file exists and names its declared
    // symbol(s) is contradicted by a `deviates` too (finding B-18), so a
    // review-only architecture item gets a real re-verification.
    const archFits = item.kind === "architecture" && ctx.archSymbolsPresent?.(item) === true;
    if (testPassed || archFits) {
      // Every contradicted seat is overturned and counted (finding M-3): the
      // code proves the opposite, so each `unmet`/`deviates` is flipped.
      const reason = testPassed
        ? `the item's own test verify passed in this check run (${tests.map((t) => `"${t.name}"`).join(", ")})`
        : "the item's :WHERE: file exists in the candidate and names its declared symbol(s)";
      return contradicted.map((v) => ({ seat: v.seat, id: item.id, kind: item.kind, verdict: v.verdict, effect: "flip" as const, reason }));
    }
    return [];
  }
  // A unanimous met/fits verdict whose every seat cites only the worker's own
  // anchors (and read none of them) is audited: each such verdict is
  // withdrawn, so it no longer counts toward the majority.
  const met = verdicts.filter((v) => v.verdict === (item.kind === "architecture" ? "fits" : "met"));
  if (met.length === verdicts.length && met.length === 3) {
    const thinSeats = met.filter((v) => {
      const anchors = evidenceFileAnchors(v.evidence).map((a) => a.path);
      const own = readsBySeat[v.seat] ?? ctx.reviewerReadFiles;
      return anchors.length <= 1 && anchors.every((p) => pathInList(p, ctx.workerAnchors)) && anchors.every((p) => !pathInList(p, own));
    });
    if (thinSeats.length === met.length) {
      const reason = "unanimous met verdict with only the worker's anchors";
      return met.map((v) => ({ seat: v.seat, id: item.id, kind: item.kind, verdict: v.verdict, effect: "drop" as const, reason }));
    }
  }
  return [];
}

// ---------------------------------------------------------------------------
// Tally: a strict majority of seats decides each item
// ---------------------------------------------------------------------------

export interface ItemOutcome {
  item: FlatItem;
  /** met/fits for R and C, or fits for A; deviates for A; unmet for R/C. */
  outcome: "met" | "unmet" | "partial" | "fits" | "deviates" | "unclear" | "incomplete";
  /** The seats' verdicts, in seat order. */
  seats: SeatItemVerdict[];
  /** The reviewers' evidence for the majority outcome. */
  evidence: string[];
}

/** Tally one item across the seats: a strict majority (2 of 3) decides. An
 * R or C is met when at least two seats say met; an A fits when at least two
 * say fits. Anything else that a majority says (unmet/partial for R and C,
 * deviates/unclear for A) is that outcome. */
export function tallyItem(item: FlatItem, verdicts: readonly SeatItemVerdict[]): ItemOutcome {
  const seats = [...verdicts].sort((a, b) => a.seat.localeCompare(b.seat));
  const count = (v: string) => seats.filter((s) => s.verdict === v).length;
  const majority = (v: string) => count(v) >= 2;
  const evidenceFor = (v: string) => seats.filter((s) => s.verdict === v).map((s) => `${s.seat}: ${s.evidence}`);
  let outcome: ItemOutcome["outcome"] = "incomplete";
  if (item.kind === "architecture") {
    if (majority("fits")) outcome = "fits";
    else if (majority("deviates")) outcome = "deviates";
    else if (majority("unclear")) outcome = "unclear";
  } else {
    if (majority("met")) outcome = "met";
    else if (majority("unmet")) outcome = "unmet";
    else if (majority("partial")) outcome = "partial";
  }
  return { item, outcome, seats, evidence: evidenceFor(outcome) };
}

/** The opposite of a verdict the evaluator flipped: an overturned unmet is
 * met, an overturned deviates fits (the only two `flip` overturns — see
 * `reverify`). */
function flippedValue(v: string): string {
  if (v === "unmet") return "met";
  if (v === "deviates") return "fits";
  return v;
}

/** Tally every item from the three reviews' item sections. An overturned
 * `unmet`/`deviates` counts as its opposite; an audited thin `met`/`fits` is
 * dropped, so the majority and acceptance reflect what the code proved. */
export function tallyItems(
  items: PlanItems,
  reviews: Array<{ seat: Reviewer; items?: ReviewItems }>,
  overturns: readonly Overturn[] = [],
): ItemOutcome[] {
  const bySeat = new Map(overturns.map((o) => [`${o.seat}:${o.id}`, o]));
  return flatItems(items).map((item) => {
    const verdicts: SeatItemVerdict[] = [];
    for (const review of reviews) {
      if (item.kind === "architecture") {
        const v = review.items?.arch?.find((a) => a.id === item.id);
        if (v) verdicts.push({ seat: review.seat, verdict: v.verdict, evidence: v.evidence ?? "" });
      } else {
        const v = review.items?.items?.find((i) => i.id === item.id);
        if (v) verdicts.push({ seat: review.seat, verdict: v.verdict, evidence: v.evidence ?? "" });
      }
    }
    const effective: SeatItemVerdict[] = [];
    for (const v of verdicts) {
      const o = bySeat.get(`${v.seat}:${item.id}`);
      if (o && o.verdict === v.verdict) {
        if (o.effect === "drop") continue;
        effective.push({ ...v, verdict: flippedValue(v.verdict) as ItemVerdictValue });
      } else {
        effective.push(v);
      }
    }
    return tallyItem(item, effective);
  });
}

// ---------------------------------------------------------------------------
// Status counts and the item-by-seat matrix
// ---------------------------------------------------------------------------

export interface ItemCounts {
  requirementsMet: number;
  requirementsTotal: number;
  architectureFit: number;
  architectureTotal: number;
  constraintsMet: number;
  constraintsTotal: number;
}

export function itemCounts(items: PlanItems, outcomes: readonly ItemOutcome[]): ItemCounts {
  const byId = new Map(outcomes.map((o) => [o.item.id, o]));
  const met = (id: string) => byId.get(id)?.outcome === "met";
  const fit = (id: string) => byId.get(id)?.outcome === "fits";
  return {
    requirementsMet: items.requirements.filter((r) => met(r.id)).length,
    requirementsTotal: items.requirements.length,
    architectureFit: items.architecture.filter((a) => fit(a.id)).length,
    architectureTotal: items.architecture.length,
    constraintsMet: items.constraints.filter((c) => met(c.id)).length,
    constraintsTotal: items.constraints.length,
  };
}

/** The slice of a phase's state the views need to tally its items. `PhaseState`
 * is structurally assignable. */
export interface ItemLoopState {
  contract: PhaseItemInput;
  reviews?: Partial<Record<string, { review?: { items?: ItemVerdict[]; arch?: ArchVerdict[] } } | undefined>>;
  coverage?: Coverage;
  checkResolution?: VerifyResolution[];
  overturns?: Overturn[];
}

/** Tally every item of a phase from its recorded reviews, with the
 * evaluator's overturns applied. */
export function phaseItemOutcomes(phase: ItemLoopState): ItemOutcome[] {
  const reviews = (["M", "A", "B"] as const).map((seat) => {
    const r = phase.reviews?.[seat]?.review;
    return { seat, items: r ? { items: r.items ?? [], arch: r.arch ?? [] } : undefined };
  });
  return tallyItems(itemsFromPhase(phase.contract), reviews, phase.overturns ?? []);
}

/** The counts line's inputs for a phase. */
export function phaseItemCounts(phase: ItemLoopState): ItemCounts {
  const items = itemsFromPhase(phase.contract);
  return itemCounts(items, phaseItemOutcomes(phase));
}

/** The item-by-seat matrix as Markdown rows for `tt summary` and the status
 * view: a header, then one row per A, R and C. */
export function matrixMarkdown(phase: ItemLoopState): string[] {
  const items = itemsFromPhase(phase.contract);
  if (flatItems(items).length === 0) return [];
  const outcomes = phaseItemOutcomes(phase);
  const rows = itemMatrix(items, outcomes, phase.coverage, phase.checkResolution ?? []);
  return [
    "| item | worker | check | M | A | B |",
    "| --- | --- | --- | --- | --- | --- |",
    ...rows.map((r) => `| ${r.id} ${r.title.replace(/\s+/g, " ").trim()} | ${r.cells.map((c) => c.text).join(" | ")} |`),
  ];
}

/** The item-by-seat matrix as an Org table whose first cell links to the
 * item's evidence file (`views/items/<id>.org`), so a cell opens its
 * evidence in the review buffer. */
export function matrixOrg(phase: ItemLoopState): string[] {
  const items = itemsFromPhase(phase.contract);
  if (flatItems(items).length === 0) return [];
  const outcomes = phaseItemOutcomes(phase);
  const rows = itemMatrix(items, outcomes, phase.coverage, phase.checkResolution ?? []);
  return [
    "| item | worker | check | M | A | B |",
    "|------+--------+-------+---+---+---|",
    ...rows.map(
      (r) =>
        `| [[items/${r.id}.org][${r.id} ${r.title.replace(/\s+/g, " ").trim()}]] | ${r.cells.map((c) => c.text).join(" | ")} |`,
    ),
  ];
}

/** One `views/items/<id>.org` per item: its text, the worker's coverage, the
 * check resolution and every seat's verdict with its evidence. */
export function itemEvidenceFiles(phase: ItemLoopState): Array<{ id: string; contents: string }> {
  const items = itemsFromPhase(phase.contract);
  const outcomes = new Map(phaseItemOutcomes(phase).map((o) => [o.item.id, o]));
  return flatItems(items).map((item) => {
    const lines = [`#+TITLE: ${item.id} — ${item.title}`, "", `* ${item.id} ${item.title}`, "  :PROPERTIES:", `  :ID: ${item.id}`, "  :END:", `  ${item.text.replace(/\n/g, "\n  ")}`];
    lines.push("", "* Worker coverage");
    if (item.kind === "architecture") {
      const e = phase.coverage?.arch?.find((x) => x.id === item.id);
      lines.push(`  - ${e ? (e.fits === "yes" ? "fits" : "deviates") : "(none)"}${e?.where?.length ? ` at ${e.where.join(", ")}` : ""}${e?.note ? ` — ${e.note}` : ""}`);
    } else {
      const e = phase.coverage?.items?.find((x) => x.id === item.id);
      lines.push(`  - ${e ? e.status : "(none)"}${e?.where?.length ? ` at ${e.where.join(", ")}` : ""}${e?.tests?.length ? `; tests: ${e.tests.join(", ")}` : ""}${e?.note ? ` — ${e.note}` : ""}`);
    }
    const tests = (phase.checkResolution ?? []).filter((r) => r.id === item.id);
    if (tests.length > 0) {
      lines.push("", "* Check");
      for (const t of tests) lines.push(`  - test "${t.name}": ${t.outcome}`);
    }
    const o = outcomes.get(item.id);
    lines.push("", `* Verdict: ${o?.outcome ?? "incomplete"}`);
    if (o && o.seats.length > 0) for (const s of o.seats) lines.push(`  - ${s.seat}: ${s.verdict} — ${s.evidence}`);
    else lines.push("  (no item verdict was submitted)");
    return { id: item.id, contents: `${lines.join("\n")}\n` };
  });
}

/** Per-seat overturn counts, in seat order (only seats with a count). */
export function overturnCounts(overturns: readonly Overturn[]): Array<{ seat: Reviewer; count: number }> {
  const out: Array<{ seat: Reviewer; count: number }> = [];
  for (const seat of ["M", "A", "B"] as const) {
    const count = overturns.filter((o) => o.seat === seat).length;
    if (count > 0) out.push({ seat, count });
  }
  return out;
}

/** The one-line status counts, e.g. `R 7/8 met · A 3/3 fit · C 2/2`. */
export function countsLine(counts: ItemCounts): string {
  const parts: string[] = [];
  if (counts.requirementsTotal > 0) parts.push(`R ${counts.requirementsMet}/${counts.requirementsTotal} met`);
  if (counts.architectureTotal > 0) parts.push(`A ${counts.architectureFit}/${counts.architectureTotal} fit`);
  if (counts.constraintsTotal > 0) parts.push(`C ${counts.constraintsMet}/${counts.constraintsTotal}`);
  return parts.join(" · ");
}

/** One cell of the item-by-seat matrix. */
export interface MatrixCell {
  /** "worker" status, "check" result, or a seat's verdict. */
  by: "worker" | "check" | string;
  text: string;
  /** The evidence a front end opens when the cell is activated. */
  evidence?: string;
}

export interface MatrixRow {
  id: string;
  kind: ItemKind;
  title: string;
  outcome: ItemOutcome["outcome"];
  cells: MatrixCell[];
}

/** The item-by-seat matrix: one row per A, R and C, one column per seat plus
 * the worker's coverage status and the check result. Each cell's `evidence`
 * is what a front end opens. */
export function itemMatrix(
  items: PlanItems,
  outcomes: readonly ItemOutcome[],
  coverage: Coverage | undefined,
  resolutions: readonly VerifyResolution[],
): MatrixRow[] {
  const byId = new Map(outcomes.map((o) => [o.item.id, o]));
  return flatItems(items).map((item) => {
    const outcome = byId.get(item.id);
    const cells: MatrixCell[] = [];
    if (item.kind === "architecture") {
      const entry = coverage?.arch?.find((e) => e.id === item.id);
      cells.push({ by: "worker", text: entry ? entry.fits : "—", ...(entry?.where?.length ? { evidence: entry.where.join("\n") } : {}) });
    } else {
      const entry = coverage?.items?.find((e) => e.id === item.id);
      cells.push({ by: "worker", text: entry ? entry.status : "—", ...(entry ? { evidence: [...entry.where, ...entry.tests].join("\n") } : {}) });
    }
    const tests = resolutions.filter((r) => r.id === item.id);
    cells.push({
      by: "check",
      text: tests.length === 0 ? "—" : tests.every((t) => t.outcome === "passed") ? "pass" : tests.map((t) => `${t.name}: ${t.outcome}`).join("; "),
    });
    for (const seat of ["M", "A", "B"] as const) {
      const v = outcome?.seats.find((s) => s.seat === seat);
      cells.push({ by: seat, text: v ? v.verdict : "—", ...(v ? { evidence: v.evidence } : {}) });
    }
    return { id: item.id, kind: item.kind, title: item.title, outcome: outcome?.outcome ?? "incomplete", cells };
  });
}

/** The repair prompt's item lines: only the items not met or not fitting,
 * with the reviewers' evidence. Empty when every item is met/fits. */
export function repairItemLines(outcomes: readonly ItemOutcome[], overturns: readonly Overturn[] = []): string[] {
  const overturned = new Set(overturns.map((o) => `${o.seat}:${o.id}`));
  const out: string[] = [];
  for (const o of outcomes) {
    const ok = o.outcome === "met" || o.outcome === "fits";
    if (ok) continue;
    const ev = o.evidence.filter((e) => !overturned.has(e.slice(0, 1) + ":" + o.item.id));
    out.push(`${o.item.id} (${o.item.kind}) ${o.outcome}: ${o.item.title}${ev.length > 0 ? ` — ${ev.join(" | ")}` : ""}`);
  }
  return out;
}

/** True when acceptance's item half holds: every R and C met by majority,
 * every A fits by majority or its deviation accepted by the owner as a
 * trade-off, and every evidence item recorded. `acceptedDeviations` holds the
 * A ids the owner accepted; `evidenceRecorded` the item ids already recorded. */
export function itemsAccept(
  items: PlanItems,
  outcomes: readonly ItemOutcome[],
  opts: { acceptedDeviations?: readonly string[]; evidenceRecorded?: readonly string[] } = {},
): boolean {
  const accepted = new Set(opts.acceptedDeviations ?? []);
  const recorded = new Set(opts.evidenceRecorded ?? []);
  for (const o of outcomes) {
    if (o.item.kind === "architecture") {
      if (o.outcome !== "fits" && !accepted.has(o.item.id)) return false;
      continue;
    }
    if (o.outcome !== "met") return false;
    if (itemNeedsEvidence(o.item) && !recorded.has(o.item.id)) return false;
  }
  return true;
}

// ---------------------------------------------------------------------------
// The checklist prompts render
// ---------------------------------------------------------------------------

/** The checklist every worker and reviewer prompt carries: every A, R and C
 * with its id, its text and its verify kinds. */
export function checklistLines(items: PlanItems): string[] {
  const out: string[] = [];
  const section = (title: string, list: FlatItem[]) => {
    if (list.length === 0) return;
    out.push("", title);
    for (const item of list) {
      const verify = item.kind === "architecture" ? "" : ` [verify: ${verifyLabel(item.verify)}]`;
      const arch = item.arch.length > 0 ? ` [arch: ${item.arch.join(", ")}]` : "";
      const where = item.where ? ` [where: ${item.where}]` : "";
      const tags = item.tags.length > 0 ? ` [${item.tags.join(" ")}]` : "";
      out.push(`- ${item.id}${where}${tags}${arch}${verify}: ${item.text.replace(/\s+/g, " ").trim()}`);
    }
  };
  section("Architecture:", items.architecture.map((a) => ({ kind: "architecture", id: a.id, title: a.title, text: a.text, arch: [], verify: [], where: a.where, tags: a.tags })));
  section("Requirements:", items.requirements.map((r) => ({ kind: "requirement", id: r.id, title: r.title, text: r.text, arch: r.arch, verify: r.verify, tags: [] })));
  section("Constraints:", items.constraints.map((c) => ({ kind: "constraint", id: c.id, title: c.title, text: c.text, arch: [], verify: c.verify, tags: [] })));
  return out;
}

/** The worker's coverage section in the reviewer prompt: the worker's own
 * status, where and tests per item, so a reviewer sees what it must check. */
export function coverageLines(coverage: Coverage | undefined, items: PlanItems): string[] {
  if (!coverage) return [];
  const out = ["", "The worker's coverage:"];
  for (const item of requirementAndConstraintItems(items)) {
    const entry = coverage.items?.find((e) => e.id === item.id);
    if (!entry) continue;
    const where = entry.where.length > 0 ? ` at ${entry.where.join(", ")}` : "";
    const tests = entry.tests.length > 0 ? `; tests: ${entry.tests.join(", ")}` : "";
    const note = entry.note ? ` — ${entry.note}` : "";
    out.push(`- ${item.id}: ${entry.status}${where}${tests}${note}`);
  }
  for (const arch of items.architecture) {
    const entry = coverage.arch?.find((e) => e.id === arch.id);
    if (!entry) continue;
    const where = entry.where.length > 0 ? ` at ${entry.where.join(", ")}` : "";
    const note = entry.note ? ` — ${entry.note}` : "";
    out.push(`- ${arch.id}: ${entry.fits === "yes" ? "fits" : "deviates"}${where}${note}`);
  }
  return out;
}

/** The check-resolution section in the reviewer prompt. */
export function checkResolutionLines(resolutions: readonly VerifyResolution[]): string[] {
  if (resolutions.length === 0) return [];
  return ["", "Test verifies, resolved against the candidate's check run:", ...resolutions.map((r) => `- ${r.id} test "${r.name}": ${r.outcome}`)];
}

/** The item verdicts a reviewer must submit, rendered as a checklist. */
export function reviewRequestLines(items: PlanItems): string[] {
  const lines = ["", "For EVERY item below, submit_review must carry a verdict (missing ids are an incomplete review):"];
  for (const item of flatItems(items)) {
    if (item.kind === "architecture") {
      lines.push(`- ${item.id} (architecture): fits | deviates | unclear, with evidence`);
    } else {
      lines.push(`- ${item.id} (${item.kind}): met | unmet | partial, with evidence`);
    }
  }
  lines.push(
    "Every verdict must cite an anchor the conductor can check: a file:line-range in the candidate, a test name that passed in this check run, or a command you ran. A met/fits verdict must cite at least one file you read yourself in this review — the worker's own anchors are not enough.",
  );
  return lines;
}

// ---------------------------------------------------------------------------
// Architecture symbols (`:WHERE:` with a named symbol)
// ---------------------------------------------------------------------------

const SYMBOL_RE = /\b(?:type|interface|class|function|const|let|var|enum|struct|def|fn)\s+([A-Za-z_$][\w$]*)/g;

/** The named types, events and functions an architecture item mentions. Used
 * to grep the candidate for a symbol before any reviewer is asked: a missing
 * one is `deviates`. */
export function architectureSymbols(item: ArchitectureItem): string[] {
  const out: string[] = [];
  for (const m of `${item.title} ${item.text}`.matchAll(SYMBOL_RE)) out.push(m[1]);
  // A bare `PascalCase` name in the title or a code span also counts.
  for (const m of item.text.matchAll(/`([A-Za-z_$][\w$]*)`/g)) out.push(m[1]);
  return [...new Set(out)];
}

/** True when `symbol` appears as a word in the candidate file's text. */
export function symbolPresent(text: string, symbol: string): boolean {
  const escaped = symbol.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  return new RegExp(`(^|[^A-Za-z0-9_$])${escaped}([^A-Za-z0-9_$]|$)`).test(text);
}
