// Plan 03b: the runtime renderer. From a run's state (messages, ledger and
// the records they came from, contract v1) it writes the owner-facing views:
//
//   views/review.org          one subtree per message, three type sections
//   views/messages/<id>.org   evidence, plan excerpt, history and votes
//   views/status.txt          the plain-text status `tt status` prints
//
// Emacs only reads these files and writes inbox commands, so the front end is
// the same locally and over TRAMP and never calls `tt state' to render the
// review. Rebuilt from state, never authoritative: `tt contract rebuild'
// regenerates them and `tt contract check' compares them.

import * as fs from "node:fs";
import * as path from "node:path";

import type { Ballot, Decision, DecisionBrief, EnvBlockInfo, EnvTool, Finding, Message, Override, OwnerRequest } from "./core/types.ts";
import { envBlockedLine, envToolsLines } from "./core/env-preflight.ts";
import {
  projectEntries,
  renderEntryFile,
  renderEntryReview,
  renderProgramEntryReview,
  type AnchorFreshness,
  type Entry,
  type EntryAnchor,
} from "./core/entries.ts";
import { runReviewLint, type ReviewLintResult } from "./core/review-lint.ts";
import { ledgerEntries } from "./core/messages.ts";
import { countsLine, itemEvidenceFiles, matrixMarkdown, matrixOrg, overturnCounts, phaseItemCounts, type ItemLoopState } from "./core/items.ts";
import type { Timeline } from "./conductor.ts";
import type { RunView } from "./view.ts";

/** The slice of a phase the review renderer reads. `PhaseState` satisfies it;
 * a test may build just these fields. */
export interface ReviewPhase {
  runId?: string;
  phaseId?: string;
  contract?: { phaseId?: string; readableId?: string; dirId?: string };
  /** Plan 05c: the run's readable id (`<program>-NN') and the run directory's
   * basename, so the header names the run the way the owner does. The
   * internal `runId' never shows here; it stays inside the property drawers,
   * where a verdict's binding needs it. */
  readableId?: string;
  dirId?: string;
  messages?: Message[];
  decisions?: Decision[];
  findings?: Finding[];
  ballots?: Ballot[];
  /** Plan 04b: each raw blocker's panel, keyed by blocker message id, so a
   * published blocker shows its panel's outcome. */
  panel?: { blockers?: Record<string, { decided?: { outcome?: string; reason?: string } }> };
}

const IMPORTANCE_RANK: Record<string, number> = { high: 0, normal: 1, low: 2 };

/** The review's three sections, in the order the buffer shows them. */
export type ReviewSection = "blocker" | "tradeoff" | "finding";

const SECTIONS: Array<{ section: ReviewSection; label: string }> = [
  { section: "blocker", label: "Blockers" },
  { section: "tradeoff", label: "Trade-offs" },
  { section: "finding", label: "Findings" },
];

/** Which section a message is listed under. Only a message raised through a
 * reviewer's `blockers` list is a Blocker; a blocking *finding* raised
 * through the ordinary `findings` list lists under Findings. A blocker
 * message raised before plan 05c (so without `raisedAsBlocker`) is treated
 * as the blocking finding it was, not as a stop-the-work blocker. */
export function sectionOf(message: Message): ReviewSection {
  if (message.type === "tradeoff") return "tradeoff";
  if (message.type === "blocker" && message.raisedAsBlocker === true) return "blocker";
  return "finding";
}

/** Whether a message is still awaiting an evaluator: a raw message, or one an
 * evaluator timeout published unchanged (`unevaluated', the raw title kept).
 * Neither may be shown as if it had been evaluated. */
export function awaitingEvaluation(message: Message): boolean {
  return message.state === "raw" || (message.state === "published" && message.unevaluated === true);
}

/** Whether the review lists a message as a titled entry. Only a message an
 * evaluator published (and its later states) is an entry: a message still
 * awaiting evaluation is one `N raw, awaiting evaluation' line, a merged one
 * is named in its target's own file, and a dropped one is only a `N dropped'
 * count. */
export function isReviewEntry(message: Message): boolean {
  return !awaitingEvaluation(message) && message.state !== "merged" && message.state !== "dropped";
}

/** The run's readable id and directory id, read from `program.json' when the
 * scheduler started this run. The readable id is absent for a hand-started
 * run, which the directory id then names alone. */
export function runIds(runDir: string): { readableId?: string; dirId: string } {
  let readableId: string | undefined;
  try {
    const raw = JSON.parse(fs.readFileSync(path.join(runDir, "program.json"), "utf8")) as { readableId?: unknown };
    if (typeof raw.readableId === "string" && raw.readableId.length > 0) readableId = raw.readableId;
  } catch {
    // no program.json (a hand-started run): the directory id names it
  }
  return { readableId, dirId: path.basename(runDir) };
}

/** The review row the status view shows, in trade-off vocabulary. `T/F/B'
 * count the messages in front of the owner — the titled entries plus the raw
 * ones awaiting evaluation — and the parenthetical breaks out how many of
 * them are raw and how many were dropped, so the counts match `review.org'. */
export function reviewSummary(messages: readonly Message[] | undefined): string {
  const part = (kind: ReviewSection, letter: string): string => {
    const own = (messages ?? []).filter((m) => sectionOf(m) === kind);
    const entries = own.filter(isReviewEntry).length;
    const raw = own.filter(awaitingEvaluation).length;
    const dropped = own.filter((m) => m.state === "dropped").length;
    const bits: string[] = [];
    if (raw > 0) bits.push(`${raw} raw`);
    if (dropped > 0) bits.push(`${dropped} dropped`);
    return `${letter} ${entries + raw}${bits.length > 0 ? ` (${bits.join(", ")})` : ""}`;
  };
  return `${part("tradeoff", "T")} · ${part("finding", "F")} · ${part("blocker", "B")} · C-c m d`;
}

function oneLine(text: string | undefined): string {
  return (text ?? "").replace(/\s+/g, " ").trim();
}

/** The finding or decision a message was raised from, if the phase has it. */
function sourceRecord(phase: ReviewPhase, message: Message): Decision | Finding | undefined {
  if (!message.sourceRecordId) return undefined;
  if (message.type === "tradeoff") return phase.decisions?.find((d) => d.id === message.sourceRecordId);
  return phase.findings?.find((f) => f.id === message.sourceRecordId);
}

/** Who raised a message: the worker for a trade-off, the reviewer (or owner,
 * or conductor) for a finding. A message may carry it explicitly (the
 * conductor sets it when raising); otherwise it is derived from the record. */
export function raisedByOf(message: Message, phase: ReviewPhase = {}): string {
  if (message.raisedBy) return message.raisedBy;
  if (message.type === "tradeoff") {
    const d = sourceRecord(phase, message) as Decision | undefined;
    if (d?.source === "reviewer-discovered") {
      const who = d.alsoSeenBy?.[0];
      return who ? `reviewer ${who}` : "reviewer";
    }
    return "worker";
  }
  const f = sourceRecord(phase, message) as Finding | undefined;
  return f?.raisedBy ?? "reviewer";
}

/** How much a message matters: a blocker is high; an advisory finding is
 * low; a trade-off follows its decision's class (reserved high, detail low,
 * delegated normal). Folded under "Minor" when low. */
export function importanceOf(message: Message, phase: ReviewPhase = {}): "high" | "normal" | "low" {
  // The evaluator's own judgement, when it made one (04a uses `medium' for
  // the middle of the three).
  if (message.importance === "high") return "high";
  if (message.importance === "low") return "low";
  if (message.importance === "normal" || message.importance === "medium") return "normal";
  // A finding-like message (a finding, or a legacy blocker that is really a
  // blocking finding) matters by its severity: blocking is high.
  if (message.type === "finding" || (message.type === "blocker" && message.raisedAsBlocker !== true)) {
    return (sourceRecord(phase, message) as Finding | undefined)?.severity === "blocking" ? "high" : "low";
  }
  if (message.type === "blocker") return "high";
  const d = sourceRecord(phase, message) as Decision | undefined;
  if (d?.class === "reserved") return "high";
  if (d?.class === "detail") return "low";
  return "normal";
}

/** The severity of a finding-like message, from the finding it was raised
 * from. A blocker message is always `blocking'; a trade-off has none. */
export function severityOf(message: Message, phase: ReviewPhase = {}): "blocking" | "advisory" | undefined {
  if (message.type === "tradeoff") return undefined;
  if (message.raisedAsBlocker) return "blocking";
  const f = sourceRecord(phase, message) as Finding | undefined;
  return f?.severity;
}

/** Plan 05e: what validated a finding (the check record, a run, a citation,
 * or the round panel), or undefined. */
export function verifiedOf(message: Message, phase: ReviewPhase = {}): string | undefined {
  const f = sourceRecord(phase, message) as Finding | undefined;
  return f?.verified;
}

/** Plan 05e: the message ids this message closes / is fixed by. A worker
 * trade-off names the finding it closes (`closes: F-n`); the finding's own
 * file names the trade-offs that close it. */
export function closesLinks(message: Message, phase: ReviewPhase = {}): { closes: string[]; fixedBy: string[] } {
  const messages = phase.messages ?? [];
  if (message.type === "tradeoff") {
    return { closes: message.closes ? [message.closes] : [], fixedBy: [] };
  }
  return { closes: [], fixedBy: messages.filter((m) => m.closes === message.id).map((m) => m.id) };
}

function prop(key: string, value: string | undefined): string {
  return `:${key}: ${oneLine(value)}`;
}

/** The property drawer every message heading carries: the id, type, state,
 * provenance, importance, the owner's verdict and the binding a verdict
 * needs (messageVersion, candidateSha, contractVersion, runId, phaseId). */
function messageProperties(message: Message, phase: ReviewPhase): string[] {
  const phaseId = message.phaseId || phase.phaseId || phase.contract?.phaseId || "";
  const cv = message.boundContractVersion;
  const severity = severityOf(message, phase);
  const lines = [
    prop("ID", message.id),
    prop("TYPE", message.type),
    ...(severity ? [prop("SEVERITY", severity)] : []),
    prop("STATE", message.state),
    prop("RAISED_BY", raisedByOf(message, phase)),
    prop("IMPORTANCE", importanceOf(message, phase)),
    prop("VERDICT", message.settlement?.state ?? "none"),
    prop("MESSAGE_VERSION", String(message.messageVersion)),
    prop("CANDIDATE_SHA", message.boundCandidateSha),
    prop("CONTRACT_VERSION", String(cv?.snapshot ?? "")),
    prop("CONTRACT_SHA256", cv?.sectionSha256 ?? ""),
    prop("RUN_ID", phase.runId ?? ""),
    prop("PHASE_ID", phaseId),
  ];
  const verified = verifiedOf(message, phase);
  if (verified) lines.push(prop("VERIFIED", verified));
  const links = closesLinks(message, phase);
  for (const c of links.closes) lines.push(prop("CLOSES", c));
  for (const f of links.fixedBy) lines.push(prop("FIXED_BY", f));
  if (message.followUp) lines.push(prop("FOLLOW_UP", "true"));
  if (message.invalidated) lines.push(prop("INVALIDATED", `${message.invalidated.reason} (${message.invalidated.atCandidate})`));
  // 04a/04b: the evaluator's report on an owner-refused message.
  if (message.addressedReport) lines.push(prop("ADDRESSED", String(message.addressedReport.addressed)));
  return lines;
}

function settlementLine(message: Message): string | undefined {
  const s = message.settlement;
  if (!s) return undefined;
  const reason = s.reason ? ` — ${oneLine(s.reason)}` : "";
  return `Verdict: ${s.state} by ${s.settledBy}${reason}`;
}

/** The panel's outcome for a blocker message, one line, or undefined when
 * the panel has not decided (or the message is not a blocker). */
function panelLine(phase: ReviewPhase, message: Message): string | undefined {
  if (sectionOf(message) !== "blocker") return undefined;
  const decided = phase.panel?.blockers?.[message.id]?.decided;
  if (!decided?.outcome) return undefined;
  const reason = decided.reason ? ` — ${oneLine(decided.reason)}` : "";
  return `Panel: ${decided.outcome}${reason}`;
}

function heading(message: Message, level: number, phase: ReviewPhase): string[] {
  const body: string[] = [];
  // A blocker is already in the red stop-the-work section; only a finding-like
  // message needs the `blocking' mark visible next to its title.
  const blocking = severityOf(message, phase) === "blocking" && sectionOf(message) !== "blocker";
  body.push(`${"*".repeat(level)} ${message.id} ${oneLine(message.title)}${blocking ? " [blocking]" : ""}`);
  body.push("  :PROPERTIES:");
  for (const p of messageProperties(message, phase)) body.push(`  ${p}`);
  body.push("  :END:");
  body.push(`  ${oneLine(message.summary)}`);
  if (message.context && message.context.trim().length > 0) {
    body.push("");
    for (const line of message.context.replace(/\r/g, "").split("\n")) body.push(`  ${line}`);
  }
  const panel = panelLine(phase, message);
  if (panel) {
    body.push("");
    body.push(`  ${panel}`);
  }
  const verdict = settlementLine(message);
  if (verdict) {
    body.push("");
    body.push(`  ${verdict}`);
  }
  if (message.followUp) {
    body.push("  Follow-up: yes (refused after DONE; not a blocker)");
  }
  if (message.invalidated) {
    body.push(`  Invalidated: ${message.invalidated.reason} at candidate ${message.invalidated.atCandidate}`);
  }
  body.push("");
  return body;
}

/** One section of `views/review.org`. The counts line comes first (`N raw,
 * awaiting evaluation' and/or `N dropped'), then the published entries. High
 * and normal messages are direct children (high first); low ones are folded
 * under `** Minor (N)`. */
function section(phase: ReviewPhase, kind: ReviewSection, label: string, messages: Message[]): string[] {
  const own = messages.filter((m) => sectionOf(m) === kind);
  const entries = own.filter(isReviewEntry);
  const rawCount = own.filter(awaitingEvaluation).length;
  const droppedCount = own.filter((m) => m.state === "dropped").length;
  const out = [`* ${label}`];
  if (rawCount > 0) out.push(`${rawCount} raw, awaiting evaluation`);
  if (droppedCount > 0) out.push(`${droppedCount} dropped`);
  if (entries.length === 0) {
    if (rawCount === 0 && droppedCount === 0) out.push("(none)");
    out.push("");
    return out;
  }
  const direct = entries
    .filter((m) => importanceOf(m, phase) !== "low")
    .sort((a, b) => (IMPORTANCE_RANK[importanceOf(a, phase)]! - IMPORTANCE_RANK[importanceOf(b, phase)]!) || a.id.localeCompare(b.id));
  const minor = entries
    .filter((m) => importanceOf(m, phase) === "low")
    .sort((a, b) => a.id.localeCompare(b.id));
  for (const m of direct) out.push(...heading(m, 2, phase));
  if (minor.length > 0) {
    out.push(`** Minor (${minor.length})`);
    for (const m of minor) out.push(...heading(m, 3, phase));
  }
  out.push("");
  return out;
}

/** The run label the review header shows: the readable id and the directory
 * id (`cebd7fcb-01 · 33c41174'). The internal `runId' is never in the
 * header; it stays inside the property drawers where a verdict's binding
 * needs it. */
function reviewRunLabel(phase: ReviewPhase): string {
  const readable = [phase.readableId, phase.contract?.readableId].find((v) => typeof v === "string" && v.length > 0);
  const dir = [phase.dirId, phase.contract?.dirId].find((v) => typeof v === "string" && v.length > 0);
  if (readable && dir) return `${readable} · ${dir}`;
  return readable ?? dir ?? phase.phaseId ?? phase.contract?.phaseId ?? "";
}

/** `views/review.org`: the runtime-rendered review view (contract v1). Three
 * top-level sections (Blockers, Trade-offs, Findings), blockers first. Only
 * published messages (and their later states) are entries; raw messages are
 * one count line per section, dropped ones only a count, and merged ones live
 * in their target's own file. Rebuilt from state, never authoritative. */
export function projectReview(phase: ReviewPhase): string {
  const messages = [...(phase.messages ?? [])];
  const lines: string[] = [
    `#+TITLE: tradeoffs-trace review — ${reviewRunLabel(phase)}`,
    "#+CONTRACT_VERSION: v1",
    "",
  ];
  for (const s of SECTIONS) lines.push(...section(phase, s.section, s.label, messages));
  return lines.join("\n");
}

// ---------------------------------------------------------------------------
// Plan 05j: the entry review (one heading per topic)
// ---------------------------------------------------------------------------

/** The slice of a phase the entry review reads. `PhaseState` satisfies it; a
 * test may build just these fields. */
export interface EntryReviewPhase {
  phaseId?: string;
  readableId?: string;
  dirId?: string;
  runId?: string;
  candidate?: { sha: string };
  /** Decision briefs: one per open owner item, rendered as a `* Needs you'
   * section above every entry. */
  briefs?: DecisionBrief[];
  ownerRequests?: OwnerRequest[];
  decisions?: Decision[];
  overrides?: Override[];
  contract?: {
    contractVersion?: { snapshot: number; sectionSha256: string };
    // Plan 06b: the structured items, so the review buffer shows the
    // item-by-seat matrix.
    architecture?: import("./core/items.ts").ArchitectureItem[];
    requirements?: import("./core/items.ts").RequirementItem[];
    constraints?: import("./core/items.ts").ConstraintItem[];
  };
  /** Plan 06b: the per-item state the matrix renders. */
  reviews?: import("./core/types.ts").PhaseState["reviews"];
  coverage?: import("./core/items.ts").Coverage;
  checkResolution?: import("./core/items.ts").VerifyResolution[];
  overturns?: import("./core/items.ts").Overturn[];
  messages?: Message[];
  entries?: Entry[];
  /** The program a program-level view spans. */
  program?: {
    id: string;
    phases: Array<{ phaseId: string; readableId?: string; candidate?: { sha: string }; messages?: Message[]; entries?: Entry[] }>;
  };
}

/** Plan 05j: the three-way anchor freshness the live view uses. A file
 * anchor whose file or lines no longer exist in the candidate checkout is
 * `stale anchor`. When no candidate exists yet there is nothing to verify, so
 * anchors are fresh; when a candidate EXISTS but its checkout cannot be read,
 * freshness could not be re-checked and the view says `anchor unverified`,
 * never silently treating it as fresh (record A-68 / M-67). */
export function candidateAnchorFreshness(candidateDir: string | undefined): (anchor: EntryAnchor) => AnchorFreshness {
  if (!candidateDir) return () => "fresh";
  if (!fs.existsSync(candidateDir)) return (anchor) => (anchor.kind === "file" ? "unverified" : "fresh");
  return (anchor) => {
    if (anchor.kind !== "file") return "fresh";
    try {
      const file = path.join(candidateDir, anchor.path);
      if (!fs.existsSync(file)) return "stale";
      const lineCount = fs.readFileSync(file, "utf8").split("\n").length;
      // Both ends of the range must exist (finding A-10): an anchor whose
      // END line is past EOF no longer exists either.
      return anchor.lines[0] >= 1 && anchor.lines[1] <= lineCount ? "fresh" : "stale";
    } catch {
      return "unverified";
    }
  };
}

/** The two-way answer, kept for callers/tests that only ask whether an anchor
 * resolves (`unverified` counts as not resolved). */
export function candidateAnchorResolves(candidateDir: string | undefined): (anchor: EntryAnchor) => boolean {
  const freshness = candidateAnchorFreshness(candidateDir);
  return (anchor) => freshness(anchor) === "fresh";
}

export interface EntryReviewRender {
  /** `views/review.org`'s bytes. */
  text: string;
  /** One `views/entries/<id>.org` per live entry. */
  files: Array<{ id: string; contents: string }>;
  /** Plan 06b: one `views/items/<id>.org` per item, the evidence a matrix
   * cell opens. */
  itemFiles: Array<{ id: string; contents: string }>;
  lint: ReviewLintResult;
}

/** `views/review.org` as plan 05j renders it: one heading per live ENTRY in
 * three sections (Blockers, Findings, Trade-offs), with the accounting footer
 * and the lint's first line when a rule fails. Pure projection of
 * `phase.entries` and `phase.messages`; no agent writes it. */
export function projectEntryReview(
  phase: EntryReviewPhase,
  opts: { anchorResolves?: (anchor: EntryAnchor) => boolean; anchorFreshness?: (anchor: EntryAnchor) => AnchorFreshness; lintError?: string } = {},
  program = false,
): EntryReviewRender {
  const messages = phase.messages ?? [];
  const entries = phase.entries ?? [];
  const newestCandidateSha = phase.candidate?.sha;
  const projected = projectEntries({ messages, entries, newestCandidateSha, anchorResolves: opts.anchorResolves, anchorFreshness: opts.anchorFreshness });
  const lint = runReviewLint({ projected, newestCandidateSha, messages });
  let text = program
    ? renderProgramEntryReview({
        program: phase.program,
        newestCandidateSha,
        anchorResolves: opts.anchorResolves,
        anchorFreshness: opts.anchorFreshness,
        lintError: opts.lintError ?? (lint.ok ? undefined : lint.firstLine),
      })
    : renderEntryReview({
        messages,
        entries,
        phaseId: phase.phaseId,
        readableId: phase.readableId,
        dirId: phase.dirId,
        newestCandidateSha,
        briefs: phase.briefs,
        ownerRequests: phase.ownerRequests,
        decisions: phase.decisions,
        overrides: phase.overrides,
        resolveBinding:
          phase.contract?.contractVersion && phase.candidate?.sha && phase.runId && phase.phaseId
            ? {
                runId: phase.runId,
                phaseId: phase.phaseId,
                candidateSha: phase.candidate.sha,
                recordVersion: 1,
                contractVersion: phase.contract.contractVersion,
              }
            : undefined,
        anchorResolves: opts.anchorResolves,
        anchorFreshness: opts.anchorFreshness,
        lintError: opts.lintError ?? (lint.ok ? undefined : lint.firstLine),
      });
  // Plan 06b: the review buffer's own item-by-seat matrix, appended to the
  // rendered review so the owner and reviewers see every item's worker
  // status, check result and seat verdict in one place.
  const itemsPhase = {
    contract: phase.contract ?? {},
    reviews: phase.reviews,
    coverage: phase.coverage,
    checkResolution: phase.checkResolution,
    overturns: phase.overturns,
  } as ItemLoopState;
  const structuredContract = Boolean(phase.contract) && (phase.contract!.architecture !== undefined || phase.contract!.requirements !== undefined || phase.contract!.constraints !== undefined);
  const matrix = structuredContract ? matrixOrg(itemsPhase) : [];
  if (matrix.length > 0) {
    text += `\n* Plan items\n  ${countsLine(phaseItemCounts(itemsPhase))}\n\n${matrix.map((l) => `  ${l}`).join("\n")}\n`;
  }
  const files = projected.views.filter((v) => v.live).map((v) => ({ id: v.entry.id, contents: renderEntryFile(v) }));
  const itemFiles = structuredContract ? itemEvidenceFiles(itemsPhase) : [];
  return { text, files, itemFiles, lint };
}

/** The messages an evaluator merged into TARGET, so the target's own file
 * names what was merged into it (a merged message is not an entry). */
export function mergedInto(target: Message, phase: ReviewPhase = {}): Message[] {
  return (phase.messages ?? []).filter((m) => {
    if (m.id === target.id || m.state !== "merged") return false;
    return m.settlement?.reason?.match(/^merged into (\S+)/)?.[1] === target.id;
  });
}

/** One message's own file: `views/messages/<id>.org`. Evidence (path:lines
 * and the quote), the plan excerpt it concerns, what was merged into it, its
 * history (every version), its ledger entry and the votes it drew. */
export function renderMessageFile(message: Message, phase: ReviewPhase = {}): string {
  const blocking = severityOf(message, phase) === "blocking" && sectionOf(message) !== "blocker";
  const headingText = `${message.id} ${oneLine(message.title)}${blocking ? " [blocking]" : ""}`;
  const lines: string[] = [
    `#+TITLE: ${message.id} — ${oneLine(message.title)}`,
    `#+TYPE: ${message.type}`,
    "",
    `* ${headingText}`,
    "  :PROPERTIES:",
  ];
  for (const p of messageProperties(message, phase)) lines.push(`  ${p}`);
  lines.push("  :END:", `  ${oneLine(message.summary)}`);
  lines.push("", "* Evidence");
  if ((message.evidence ?? []).length === 0) lines.push("  (none)");
  else for (const ev of message.evidence) lines.push(`  - ${ev}`);
  // Plan 05e: what validated a finding, and the round panel's own votes (so
  // a dropped item shows the three reasons).
  const verified = verifiedOf(message, phase);
  if (verified) lines.push("", "* Validation", `  verified: ${oneLine(verified)}`);
  if ((message.panelVotes ?? []).length > 0) {
    lines.push("", `* Panel votes (${oneLine(message.panelOutcome)} by ${message.panelVotes!.length} seat(s))`);
    for (const v of [...message.panelVotes!].sort((a, b) => a.seat - b.seat)) lines.push(`  - seat ${v.seat}: ${v.verdict} — ${oneLine(v.reason)}`);
  }
  const links = closesLinks(message, phase);
  if (links.closes.length > 0) lines.push("", "* Closes", ...links.closes.map((c) => `  - ${c}`));
  if (links.fixedBy.length > 0) lines.push("", "* Fixed by", ...links.fixedBy.map((f) => `  - ${f}`));
  lines.push("", "* Plan", `  ${oneLine(message.planRef) || "(none)"}`);
  const mergedIn = mergedInto(message, phase);
  if (mergedIn.length > 0) {
    lines.push("", "* Merged in");
    for (const m of mergedIn) lines.push(`  - ${m.id} ${oneLine(m.title)}`);
  }
  lines.push("", "* History");
  for (const h of messageHistory(message, phase)) lines.push(`  - v${h.version} · ${h.candidate} · ${h.hash}`);
  lines.push("", "* Ledger");
  const entry = ledgerEntries(phase.messages ?? [message]).find((e) => e.messageId === message.id);
  if (entry) {
    const reason = entry.reason ? ` — ${oneLine(entry.reason)}` : "";
    lines.push(`  - ${entry.state} by ${entry.settledBy} (v${entry.messageVersion}, ${entry.candidateSha})${reason}`);
  } else {
    lines.push("  (not settled)");
  }
  lines.push("", "* Votes");
  const votes = (phase.ballots ?? []).filter((b) => b.decisionId === message.sourceRecordId);
  if (votes.length === 0) lines.push("  (none)");
  else for (const b of votes) lines.push(`  - ${b.reviewer} ${b.vote} — ${oneLine(b.rationale)}`);
  return lines.join("\n");
}

interface VersionHistory {
  version: number;
  candidate: string;
  hash: string;
}

/** Every version the message ever had, oldest first. The current version is
 * always present; past versions come from `versionContentHashes` and the
 * `(candidate, version)` pairs a carry recorded. */
export function messageHistory(message: Message, phase: ReviewPhase = {}): VersionHistory[] {
  const versions = new Map<number, VersionHistory>();
  for (const [v, hash] of Object.entries(message.versionContentHashes ?? {})) {
    const version = Number(v);
    const from = (message.carriedFrom ?? []).find((c) => c.version === version);
    versions.set(version, { version, candidate: from?.candidateSha ?? message.boundCandidateSha, hash });
  }
  versions.set(message.messageVersion, { version: message.messageVersion, candidate: message.boundCandidateSha, hash: message.contentHash });
  return [...versions.values()].sort((a, b) => a.version - b.version);
}

/** The files `projectReview` expects beside `review.org`. */
export function reviewMessageFiles(phase: ReviewPhase): Array<{ id: string; contents: string }> {
  return [...(phase.messages ?? [])]
    .sort((a, b) => a.id.localeCompare(b.id))
    .map((m) => ({ id: m.id, contents: renderMessageFile(m, phase) }));
}

// ---------------------------------------------------------------------------
// views/status.txt
// ---------------------------------------------------------------------------

export interface StatusSecretStatus {
  missing: string[];
  tooShort: string[];
}

/** `views/status.txt`: the same readable status `tt status` prints, from a
 * rebuilt (or live) state and the view computed from the run directory. The
 * Emacs status buffer reads it instead of calling `tt state' every poll. */
export function renderStatusText(
  runDir: string,
  state: { run: string; phase: unknown },
  view: RunView & { timeline: Timeline },
  secretStatus: StatusSecretStatus = { missing: [], tooShort: [] },
): string {
  const phase = state.phase as Record<string, unknown>;
  const lines: string[] = [];
  lines.push(`run: ${runDir.split(/[\\/]/).filter(Boolean).pop() ?? ""}`);
  lines.push(`run status: ${state.run}`);
  for (const name of secretStatus.missing) lines.push(`secret ${name} not set`);
  for (const name of secretStatus.tooShort) lines.push(`secret ${name} too short to mask (value under 4 characters)`);
  lines.push(`phase: ${phase.phaseId} — ${phase.phase}`);
  const attempt = phase.attempt as { n: number; interrupted?: boolean } | undefined;
  if (attempt) lines.push(`attempt: ${attempt.n}${attempt.interrupted ? " (interrupted)" : ""}`);
  const candidate = phase.candidate as { sha: string } | undefined;
  if (candidate) lines.push(`candidate: ${candidate.sha}`);
  const checks = phase.checks as { passed?: boolean; interrupted?: boolean } | undefined;
  if (checks) lines.push(`checks: ${checks.interrupted ? "interrupted" : checks.passed ? "passed" : "failed"}`);
  const probe = phase.probe as { passed?: boolean; probedI?: string } | undefined;
  if (probe) lines.push(`probe: ${probe.passed ? `passed (I=${probe.probedI})` : "failed"}`);
  const reviews = phase.reviews as Record<string, { review?: unknown }> | undefined;
  if (reviews) {
    for (const who of ["M", "A", "B"]) lines.push(`review ${who}: ${reviews[who]?.review ? "submitted" : "pending"}`);
  }
  const ownerRequests = (phase.ownerRequests as Array<{ status: string }>) ?? [];
  lines.push(`open owner requests: ${ownerRequests.filter((r) => r.status === "open").length}`);
  const directives = (phase.ownerDirectives as Array<{ id: string; text: string; scope: string; status: string }>) ?? [];
  for (const d of directives.filter((d) => d.status === "in-force")) {
    lines.push(`directive ${d.id} (${d.scope === "program" ? "whole program" : "this phase"}): ${d.text}`);
  }
  if (phase.blockedReason) lines.push(`blocked: ${phase.blockedReason}`);
  // Plan 05i: the toolchain the run resolved at start, then the block (if
  // any). The `env` label column matches the secret rows.
  const env = phase.env as { tools?: EnvTool[]; blocked?: EnvBlockInfo } | undefined;
  for (const line of envToolsLines(env?.tools)) lines.push(line);
  if (env?.blocked) lines.push(envBlockedLine(env.blocked));
  if (phase.publishedI) lines.push(`published: ${phase.publishedI}`);
  lines.push(`pipeline: ${view.pipeline}`);
  lines.push(`gates: ${view.gates}`);
  if (view.gate) lines.push(`gate: ${view.gate}`);
  if (view.baseline) lines.push(`base: ${view.baseline}`);
  lines.push(`reviews: ${view.reviewLine}`);
  if (view.metricsLine) lines.push(view.metricsLine);
  if (view.loop) lines.push(row("loop", view.loop)!);
  if (view.models) lines.push(row("models", view.models)!);
  if (view.amendments) lines.push(`amendments: ${view.amendments}`);
  if (view.verdict) lines.push(`verdict: ${view.verdict}`);
  for (const t of view.tradeoffs ?? []) lines.push(`trade-off: ${t.text}`);
  if (view.cost) lines.push(`cost: ${view.cost.text}`);
  if (view.time) lines.push(`time: ${view.time}`);
  return `${lines.join("\n")}\n`;
}

// ---------------------------------------------------------------------------
// views/status.txt: the Emacs status buffer's own text
// ---------------------------------------------------------------------------

/** The marker a rendered trade-off line carries after its text, so the status
 * buffer can still open the decision view (plan 01h's RET binding) from the
 * plain file. `+tt-open-tradeoff' reads the `+tt-record' property the Emacs
 * side restores by parsing this. */
export const STATUS_RECORD_MARKER = "\t:RECORD:";

/** Plan 2d: pending owner-input files the conductor has not yet picked up.
 * `tt state' carries them so the status view can show a command the owner
 * sent while no conductor was running as `not picked up' after 30 s. */
export function pendingOwnerInputs(runDir: string): Array<{ id: string; kind: string; text: string; at: string }> {
  let names: string[];
  try {
    names = fs.readdirSync(path.join(runDir, "inbox"));
  } catch {
    return [];
  }
  const out: Array<{ id: string; kind: string; text: string; at: string }> = [];
  for (const name of names.sort()) {
    if (!name.endsWith(".json")) continue;
    try {
      const raw = JSON.parse(fs.readFileSync(path.join(runDir, "inbox", name), "utf8")) as Record<string, unknown>;
      const kind = typeof raw.type === "string" ? raw.type : typeof raw.kind === "string" ? raw.kind : undefined;
      if (kind !== "steer" && kind !== "note" && kind !== "correction") continue;
      if (typeof raw.text !== "string") continue;
      const at = fs.statSync(path.join(runDir, "inbox", name)).mtime.toISOString();
      out.push({ id: name.slice(0, -".json".length), kind, text: raw.text, at });
    } catch {
      // A file still being written, or malformed: the conductor will reject
      // it; not this view's job to guess.
    }
  }
  return out;
}

export interface OwnerInputLike {
  id?: string;
  kind?: string;
  text?: string;
  state?: string;
  reason?: string;
  at?: string;
}

export interface OwnerDirectiveLike {
  id?: string;
  seq?: number;
  text?: string;
  scope?: string;
  status?: string;
  targets?: string[];
  deliveries?: Record<string, string>;
}

export interface StatusViewInput {
  runDir: string;
  title: string;
  /** `state.phase` — the same shape `tt state` carries. */
  phase: Record<string, unknown>;
  alive: boolean;
  view: RunView & { timeline: Timeline };
  secrets?: { missing: string[]; tooShort: string[] };
  ownerInputs?: OwnerInputLike[];
  pendingOwnerInputs?: OwnerInputLike[];
  ownerDirectives?: OwnerDirectiveLike[];
  ownerChecklist?: string[];
}

/** Assemble a `StatusViewInput` from the pieces a caller already has, so the
 * conductor and the CLI's late-verdict path render the same view. */
export function statusViewInput(opts: {
  runDir: string;
  plan: { title?: string; phases?: Array<{ ownerChecklist?: string[] }> };
  state: { phase: unknown };
  view: RunView & { timeline: Timeline };
  alive: boolean;
  secrets?: { missing: string[]; tooShort: string[] };
}): StatusViewInput {
  const phase = opts.state.phase as Record<string, unknown>;
  return {
    runDir: opts.runDir,
    title: opts.plan.title ?? "",
    phase,
    alive: opts.alive,
    view: opts.view,
    secrets: opts.secrets,
    ownerInputs: (phase.ownerInputs as OwnerInputLike[] | undefined) ?? [],
    pendingOwnerInputs: pendingOwnerInputs(opts.runDir),
    ownerDirectives: (phase.ownerDirectives as OwnerDirectiveLike[] | undefined) ?? [],
    ownerChecklist: opts.plan.phases?.[0]?.ownerChecklist,
  };
}

function truncate(text: string | undefined, width: number): string {
  const s = oneLine(text);
  return s.length > width ? `${s.slice(0, width - 1)}…` : s;
}

function row(label: string, value: string | number | undefined): string | undefined {
  if (value === undefined) return undefined;
  const s = String(value);
  if (s.length === 0) return undefined;
  return `${label.padEnd(10)}${s}`;
}

function ownerInputStateLabel(state: string | undefined, reason: string | undefined): string {
  switch (state) {
    case "delivered": return "delivered";
    case "noted": return "noted";
    case "correction-started": return "correction started";
    case "reverted": return "reverted an amendment";
    case "delivery-uncertain": return `delivery uncertain${reason ? ` (${reason})` : ""}`;
    case "refused": return `refused: ${reason ?? "not accepted"}`;
    default: return state ?? "sent";
  }
}

function directiveDelivery(d: OwnerDirectiveLike): string {
  const targets = d.targets ?? [];
  if (targets.length === 0) return "(no live agent; carried in every later prompt)";
  return targets
    .map((t) => {
      const state = d.deliveries?.[t];
      return `${t} ${state === "delivered" ? "✓" : state === "delivery-uncertain" ? "?" : "⧗"}`;
    })
    .join(" ");
}

function renderOwnerInputs(lines: string[], input: StatusViewInput): void {
  const recorded = input.ownerInputs ?? [];
  const pending = input.pendingOwnerInputs ?? [];
  const now = Date.now();
  const entries: Array<{ label: string; r: OwnerInputLike }> = [
    ...recorded.map((r) => ({ label: ownerInputStateLabel(r.state, r.reason), r })),
    ...pending.map((r) => ({ label: r.at && now - Date.parse(r.at) > 30_000 ? "not picked up" : "sent", r })),
  ];
  if (entries.length > 0) {
    lines.push("", `Owner input (${entries.length})`);
    for (const e of entries) {
      lines.push(`  - ${truncate(e.r.text, 70)} — ${e.label}${e.r.kind ? ` (${e.r.kind})` : ""}`);
    }
  }
  const directives = [...(input.ownerDirectives ?? [])].sort((a, b) => (a.seq ?? 0) - (b.seq ?? 0));
  if (directives.length > 0) {
    lines.push("", `Owner directives (${directives.length})`);
    for (const d of directives) {
      const scope = d.scope === "program" ? "whole program" : "this phase";
      const status = d.status === "withdrawn" ? "withdrawn" : "in force";
      lines.push(`  - ${d.id} [${scope}, ${status}] ${truncate(d.text, 70)} — ${directiveDelivery(d)}`);
    }
  }
}

/** `views/status.txt`: the text the Emacs status buffer shows — the title, the
 * `run <id> · conductor … · <elapsed>' line, every status row from the view,
 * the trade-offs (each tagged with its record id so RET still opens the
 * decision view), the cost, the record counts, the DONE owner checklist, the
 * owner input and directives, and the attention line. `tt status' keeps its
 * own plain-text rendering; this is the front end's view as a file. */
export function renderStatusView(input: StatusViewInput): string {
  const { phase, view } = input;
  const name = phase.phase as string | undefined;
  const lines: string[] = [];
  const push = (r: string | undefined) => { if (r !== undefined) lines.push(r); };
  lines.push(input.title);
  lines.push(`run ${path.basename(input.runDir)} · ${input.alive ? "conductor running" : "conductor stopped"} · ${view.elapsed}`);
  lines.push("");
  const attempt = phase.attempt as { n?: number } | undefined;
  // Plan 05h: the tape's current row, replacing plan 03c's `chart C-c m g`
  // hint — the owner sees the loop itself in the status, not just where to
  // find a chart of it.
  push(row("loop", view.loop));
  push(row("phase", `${phase.phaseId} · ${name} · round ${view.round} · attempt ${attempt?.n ?? "?"} · repairs ${phase.repairRoundsUsed ?? 0}/${phase.repairRoundsGranted ?? 0}`));
  push(row("pipeline", view.pipeline));
  // Plan 06b: the status line's item counts, e.g. `R 7/8 met · A 3/3 fit · C 2/2`.
  try {
    const contract = phase.contract as { architecture?: unknown; requirements?: unknown; constraints?: unknown } | undefined;
    const structured = contract !== undefined && (contract.architecture !== undefined || contract.requirements !== undefined || contract.constraints !== undefined);
    if (structured) {
      const loopState = {
        contract,
        reviews: phase.reviews,
        coverage: phase.coverage,
        checkResolution: phase.checkResolution,
        overturns: phase.overturns,
      } as unknown as ItemLoopState;
      const counts = countsLine(phaseItemCounts(loopState));
      push(row("items", counts));
      const overturns = overturnCounts(loopState.overturns ?? []);
      if (overturns.length > 0) push(row("overturns", overturns.map((o) => `${o.seat} ${o.count}`).join(" · ")));
    }
  } catch {
    // A view must never fail to render for a count.
  }
  push(row("time", view.time));
  push(row("gates", view.gates));
  push(row("gate", view.gate));
  push(row("base", view.baseline));
  push(row("amended", view.amendments));
  push(row("previous", view.previousRound));
  push(row("reviews", view.reviewLine));
  push(row("models", view.models));
  push(view.metricsLine);
  push(row("verdict", view.verdict));
  const tradeoffs = view.tradeoffs ?? [];
  if (tradeoffs.length > 0) {
    lines.push("", `Trade-offs (${tradeoffs.length})`);
    for (const t of tradeoffs) {
      lines.push(`  - ${t.text ?? ""}${t.recordId ? `${STATUS_RECORD_MARKER}${t.recordId}` : ""}`);
    }
  }
  push(row("cost", view.cost?.text));
  // Plan 05c: the `records' row said `decisions' and disagreed with the
  // review; the `review' row counts in trade-off vocabulary, matching the
  // entries, the raw count and the dropped count `review.org' shows.
  push(row("review", view.review));
  // Plan 3b: boundary files changed are the worker's trigger records the
  // reviewers must classify. The old `records' row carried this; it keeps its
  // own row so the owner still sees a change that no reviewer has classified
  // (advisory A-5).
  if (view.boundaryFilesChanged > 0) {
    push(row("boundary", `files changed: ${view.boundaryFilesChanged} (reviewers classify)`));
  }
  push(row("blocked", phase.blockedReason as string | undefined));
  // Plan 05i: the resolved tools, then the environment block itself.
  for (const line of view.envTools) lines.push(line);
  if (view.envBlocked) lines.push(view.envBlocked);
  for (const nm of input.secrets?.missing ?? []) push(row("secret", `${nm} not set`));
  for (const nm of input.secrets?.tooShort ?? []) push(row("secret", `${nm} too short to mask`));
  if (name === "DONE" && (input.ownerChecklist?.length ?? 0) > 0) {
    lines.push("", `Owner checklist (${input.ownerChecklist!.length}) — yours, not the worker's`);
    for (const item of input.ownerChecklist!) lines.push(`  - ${truncate(item, 200)}`);
  }
  renderOwnerInputs(lines, input);
  if (view.attention) {
    const suffix =
      view.attention === "needs you"
        ? " — type a correction in the input box (C-c m d to read the review)"
        : view.attention === "conductor stopped" ? " — M-x +tt-resume" : "";
    // Decision briefs: the `needs you` (or `BLOCKED`) line names the owner's
    // actual question, not a finding id (disc-M-185).
    const label =
      (view.attention === "needs you" || view.attention === "BLOCKED") && view.attentionQuestion
        ? `${view.attention} — ${view.attentionQuestion}`
        : view.attention;
    lines.push("", `⚑ ${label}${suffix}`);
  }
  return `${lines.join("\n")}\n`;
}