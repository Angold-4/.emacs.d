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

import type { Ballot, Decision, Finding, Message } from "./core/types.ts";
import { ledgerEntries } from "./core/messages.ts";
import type { Timeline } from "./conductor.ts";
import type { RunView } from "./view.ts";

/** The slice of a phase the review renderer reads. `PhaseState` satisfies it;
 * a test may build just these fields. */
export interface ReviewPhase {
  runId?: string;
  phaseId?: string;
  contract?: { phaseId?: string };
  messages?: Message[];
  decisions?: Decision[];
  findings?: Finding[];
  ballots?: Ballot[];
}

const IMPORTANCE_RANK: Record<string, number> = { high: 0, normal: 1, low: 2 };

/** The type's section, in the order the review buffer shows them. */
const SECTIONS: Array<{ type: Message["type"]; label: string }> = [
  { type: "blocker", label: "Blockers" },
  { type: "tradeoff", label: "Trade-offs" },
  { type: "finding", label: "Findings" },
];

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
  if (message.importance) return message.importance;
  if (message.type === "blocker") return "high";
  if (message.type === "finding") return "low";
  const d = sourceRecord(phase, message) as Decision | undefined;
  if (d?.class === "reserved") return "high";
  if (d?.class === "detail") return "low";
  return "normal";
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
  const lines = [
    prop("ID", message.id),
    prop("TYPE", message.type),
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

function heading(message: Message, level: number, phase: ReviewPhase): string[] {
  const body: string[] = [];
  body.push(`${"*".repeat(level)} ${message.id} ${oneLine(message.title)}`);
  body.push("  :PROPERTIES:");
  for (const p of messageProperties(message, phase)) body.push(`  ${p}`);
  body.push("  :END:");
  body.push(`  ${oneLine(message.summary)}`);
  if (message.context && message.context.trim().length > 0) {
    body.push("");
    for (const line of message.context.replace(/\r/g, "").split("\n")) body.push(`  ${line}`);
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

/** One section of `views/review.org`. High and normal messages are direct
 * children (high first); low ones are folded under `** Minor (N)`. */
function section(phase: ReviewPhase, type: Message["type"], label: string, messages: Message[]): string[] {
  const own = messages.filter((m) => m.type === type);
  const out = [`* ${label}`];
  if (own.length === 0) {
    out.push("(none)", "");
    return out;
  }
  const direct = own
    .filter((m) => importanceOf(m, phase) !== "low")
    .sort((a, b) => (IMPORTANCE_RANK[importanceOf(a, phase)]! - IMPORTANCE_RANK[importanceOf(b, phase)]!) || a.id.localeCompare(b.id));
  const minor = own
    .filter((m) => importanceOf(m, phase) === "low")
    .sort((a, b) => a.id.localeCompare(b.id));
  for (const m of direct) out.push(...heading(m, 2, phase));
  if (minor.length > 0) {
    out.push(`** Minor (${minor.length})`);
    for (const m of minor) out.push(...heading(m, 3, phase));
  }
  return out;
}

/** `views/review.org`: the runtime-rendered review view (contract v1). Three
 * top-level sections (Blockers, Trade-offs, Findings), blockers first; each
 * message one heading carrying its summary, context, reviewable state and the
 * binding a verdict needs. Rebuilt from state, never authoritative. */
export function projectReview(phase: ReviewPhase): string {
  const phaseId = phase.phaseId ?? phase.contract?.phaseId ?? "";
  const messages = [...(phase.messages ?? [])];
  const lines: string[] = [
    `#+TITLE: tradeoffs-trace review — ${phaseId}`,
    `#+RUN_ID: ${phase.runId ?? ""}`,
    "#+CONTRACT_VERSION: v1",
    "",
  ];
  for (const s of SECTIONS) lines.push(...section(phase, s.type, s.label, messages));
  return lines.join("\n");
}

/** One message's own file: `views/messages/<id>.org`. Evidence (path:lines
 * and the quote), the plan excerpt it concerns, its history (every version),
 * its ledger entry and the votes it drew. */
export function renderMessageFile(message: Message, phase: ReviewPhase = {}): string {
  const lines: string[] = [
    `#+TITLE: ${message.id} — ${oneLine(message.title)}`,
    `#+TYPE: ${message.type}`,
    "",
    `* ${message.id} ${oneLine(message.title)}`,
    "  :PROPERTIES:",
  ];
  for (const p of messageProperties(message, phase)) lines.push(`  ${p}`);
  lines.push("  :END:", `  ${oneLine(message.summary)}`);
  lines.push("", "* Evidence");
  if ((message.evidence ?? []).length === 0) lines.push("  (none)");
  else for (const ev of message.evidence) lines.push(`  - ${ev}`);
  lines.push("", "* Plan", `  ${oneLine(message.planRef) || "(none)"}`);
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
  push(
    row(
      "records",
      `${view.liveDecisions} decisions${view.failedDecisions > 0 ? ` (${view.failedDecisions} failed)` : ""}${(view.flaggedDecisions ?? 0) > 0 ? ` · ${view.flaggedDecisions} flagged for you` : ""} · ${view.openFindings} open findings${view.boundaryFilesChanged > 0 ? ` · boundary files changed: ${view.boundaryFilesChanged} (reviewers classify)` : ""}`,
    ),
  );
  push(row("blocked", phase.blockedReason as string | undefined));
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
    lines.push("", `⚑ ${view.attention}${suffix}`);
  }
  return `${lines.join("\n")}\n`;
}