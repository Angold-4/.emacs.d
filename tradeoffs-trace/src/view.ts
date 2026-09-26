// Plan 3b: the owner-facing view of a run — computed here once, rendered by
// Emacs (core/init-tradeoffs-trace.el) and `tt status`/`tt timing`/`tt list`
// as plain strings. Everything is derived from the run directory: the control
// log (via rebuildTimeline) and the agents' stream files. Nothing here moves
// run state.

import * as fs from "node:fs";
import * as path from "node:path";

import { DEFAULT_DEADLINES, rebuildTimeline, runPaths, type RunPlanFile, type Timeline } from "./conductor.ts";
import { decisionStatus, isLiveDecision } from "./core/predicate.ts";
import { notAcceptedReasons, reviewerOutcomes, type ReviewerOutcome } from "./core/verdict.ts";
import type { PhaseState } from "./core/types.ts";

// ---------------------------------------------------------------------------
// Pipeline
// ---------------------------------------------------------------------------

const STAGE_OF: Record<string, string> = {
  READY: "implement",
  IMPLEMENTING: "implement",
  REPAIRING: "implement",
  FREEZING: "freeze",
  CHECKING: "checks",
  PROBING: "probe",
  REVIEWING: "review",
  RESOLVING: "resolve",
  ACCEPTED: "publish",
  PUBLISHING: "publish",
  AWAITING_OWNER: "needs you",
  DONE: "DONE",
  BLOCKED: "BLOCKED",
};

/** The stages the conductor runs itself: no agent is expected to be active,
 * so "no agent activity" there is not idleness. */
export const CONDUCTOR_STAGES = new Set(["freeze", "checks", "probe", "resolve", "publish"]);

const STAGE_DEADLINE_MS: Record<string, number> = {
  implement: DEFAULT_DEADLINES.workerAttemptMs,
  freeze: DEFAULT_DEADLINES.freezeMs,
  checks: DEFAULT_DEADLINES.checkMs,
  probe: DEFAULT_DEADLINES.probeMs,
  review: DEFAULT_DEADLINES.reviewMs,
};

export interface StageSpan {
  stage: string;
  startedAt: string;
  ms: number;
  failed: boolean; // the stage ended by sending the phase back to implement
  current: boolean;
}

/** `lastEventAt`: when the conductor is not running, the current stage
 * stops counting at the last logged event instead of running on. */
export function stageSpans(timeline: Timeline, now: Date, lastEventAt?: string): StageSpan[] {
  const spans: StageSpan[] = [];
  for (const p of timeline.phases) {
    const stage = STAGE_OF[p.phase] ?? p.phase.toLowerCase();
    const last = spans[spans.length - 1];
    if (last && last.stage === stage) continue;
    if (last) {
      last.ms = Date.parse(p.at) - Date.parse(last.startedAt);
      last.current = false;
      // A gate or review that hands the phase back to implement failed.
      if (stage === "implement" && last.stage !== "implement") last.failed = true;
    }
    spans.push({ stage, startedAt: p.at, ms: 0, failed: false, current: true });
  }
  const last = spans[spans.length - 1];
  if (last) {
    if (last.stage === "DONE" || last.stage === "BLOCKED") last.current = false;
    else {
      last.ms = (lastEventAt ? Date.parse(lastEventAt) : now.getTime()) - Date.parse(last.startedAt);
      if (lastEventAt) last.current = false;
    }
  }
  return spans;
}

export function formatDuration(ms: number): string {
  const s = Math.max(0, Math.round(ms / 1000));
  if (s < 60) return `${s}s`;
  const m = Math.floor(s / 60);
  if (m < 60) return `${m}m${String(s % 60).padStart(2, "0")}s`;
  return `${Math.floor(m / 60)}h${String(m % 60).padStart(2, "0")}m`;
}

/** "implement 30s → freeze 1s → checks ✗ 10m00s → implement 26m → … →
 * review 12s… (14m48s left)". Attempts after the first are numbered. */
export function pipelineLine(spans: StageSpan[]): string {
  let attempt = 0;
  const parts = spans.map((s) => {
    let name = s.stage;
    if (s.stage === "implement") {
      attempt += 1;
      if (attempt > 1) name = `implement#${attempt}`;
    }
    if (s.stage === "DONE" || s.stage === "BLOCKED") return s.stage;
    const d = formatDuration(s.ms);
    if (s.current) {
      const limit = STAGE_DEADLINE_MS[s.stage];
      const left =
        limit === undefined ? "" : limit >= s.ms ? ` (${formatDuration(limit - s.ms)} left)` : ` (over by ${formatDuration(s.ms - limit)})`;
      return `${name} ${d}…${left}`;
    }
    return `${name}${s.failed ? " ✗" : ""} ${d}`;
  });
  // Long runs: keep the first stage and the most recent ones readable.
  if (parts.length > 9) return [parts[0], "…", ...parts.slice(-7)].join(" → ");
  return parts.join(" → ");
}

// ---------------------------------------------------------------------------
// Agent time (the reference behaviour is ~/.tradeoffs-trace/tools/tt-timing)
// ---------------------------------------------------------------------------

export type TimeCategory = "polling" | "full tests" | "narrow tests" | "file writes" | "reads/greps" | "other";

export interface ToolCall {
  name: string;
  arg: string;
  startedAt: string;
  ms: number;
  running: boolean;
  category: TimeCategory;
}

export interface AgentTime {
  agentId: string;
  startedAt: string;
  lastAt: string;
  elapsedMs: number;
  toolMs: number;
  modelMs: number;
  byCategory: Record<TimeCategory, { calls: number; ms: number }>;
  slowest: ToolCall[];
  running?: ToolCall;
}

export function categorize(name: string, arg: string): TimeCategory {
  if (name === "read" || name === "grep" || name === "ls" || name === "find") return "reads/greps";
  if (name === "edit" || name === "write") return "file writes";
  if (name !== "sh" && name !== "bash") return "other";
  if (/\bsleep\b|\btail -f\b/.test(arg)) return "polling";
  if (/make\b.*\b(check|test|crash|live)\b/.test(arg) || (/node --test/.test(arg) && /\*\*|crash/.test(arg))) return "full tests";
  if (/node --test|\bert-run-tests|npm (run )?test/.test(arg)) return "narrow tests";
  if (/cat >|tee |python3? -/.test(arg)) return "file writes";
  return "other";
}

function argOf(args: unknown): string {
  if (!args || typeof args !== "object") return "";
  const a = args as Record<string, unknown>;
  const v = a.command ?? a.path ?? a.pattern ?? "";
  return typeof v === "string" ? v : JSON.stringify(v);
}

/** Tool-call timing for one agent stream file. Reads line by line and skips
 * the (large, frequent) message_update records without parsing them. */
export function agentTime(file: string, now: Date, live: boolean): AgentTime | undefined {
  let text: string;
  try {
    text = fs.readFileSync(file, "utf8");
  } catch {
    return undefined;
  }
  let first: string | undefined;
  let last: string | undefined;
  const open = new Map<string, { name: string; arg: string; at: string }>();
  const calls: ToolCall[] = [];
  for (const line of text.split("\n")) {
    if (line.length === 0) continue;
    const tsMatch = line.match(/"ts":"([^"]+)"/);
    if (tsMatch) {
      first ??= tsMatch[1];
      last = tsMatch[1];
    }
    if (line.includes('"message_update"')) continue;
    if (!line.includes('"tool_execution_')) continue;
    let rec: { ts: string; event: { type: string; toolCallId: string; toolName?: string; args?: unknown } };
    try {
      rec = JSON.parse(line);
    } catch {
      continue;
    }
    const e = rec.event;
    if (e.type === "tool_execution_start") {
      open.set(e.toolCallId, { name: e.toolName ?? "?", arg: argOf(e.args), at: rec.ts });
    } else if (e.type === "tool_execution_end") {
      const s = open.get(e.toolCallId);
      if (!s) continue;
      open.delete(e.toolCallId);
      calls.push({
        name: s.name,
        arg: s.arg,
        startedAt: s.at,
        ms: Date.parse(rec.ts) - Date.parse(s.at),
        running: false,
        category: categorize(s.name, s.arg),
      });
    }
  }
  if (!first || !last) return undefined;
  const end = live ? now.getTime() : Date.parse(last);
  let running: ToolCall | undefined;
  for (const s of open.values()) {
    const call = {
      name: s.name,
      arg: s.arg,
      startedAt: s.at,
      ms: end - Date.parse(s.at),
      running: true,
      category: categorize(s.name, s.arg),
    };
    calls.push(call);
    if (live) running = call;
  }
  const byCategory = Object.fromEntries(
    (["polling", "full tests", "narrow tests", "file writes", "reads/greps", "other"] as TimeCategory[]).map((c) => [
      c,
      { calls: 0, ms: 0 },
    ]),
  ) as AgentTime["byCategory"];
  let toolMs = 0;
  for (const c of calls) {
    byCategory[c.category].calls += 1;
    byCategory[c.category].ms += c.ms;
    toolMs += c.ms;
  }
  const elapsedMs = Math.max(1, end - Date.parse(first));
  return {
    agentId: path.basename(file, ".jsonl"),
    startedAt: first,
    lastAt: last,
    elapsedMs,
    toolMs,
    modelMs: Math.max(0, elapsedMs - toolMs),
    byCategory,
    slowest: [...calls].sort((a, b) => b.ms - a.ms).slice(0, 5),
    running,
  };
}

const pct = (part: number, whole: number) => `${Math.round((100 * part) / Math.max(1, whole))}%`;

/** "worker-2: model 61% · polling 0% · full tests 12% · ⧗ $ node --test … 42s". */
export function timeLine(t: AgentTime): string {
  const role = t.agentId.replace(/-dispatch_.*$|-\d+$/, "");
  const parts = [
    `model ${pct(t.modelMs, t.elapsedMs)}`,
    `polling ${pct(t.byCategory.polling.ms, t.elapsedMs)}`,
    `full tests ${pct(t.byCategory["full tests"].ms, t.elapsedMs)}`,
  ];
  if (t.running) {
    const verb = t.running.name === "sh" ? "$" : t.running.name;
    const arg = t.running.arg.replace(/\s+/g, " ");
    parts.push(`⧗ ${verb} ${arg.length > 200 ? `${arg.slice(0, 199)}…` : arg} ${formatDuration(t.running.ms)}`);
  }
  const warn = t.byCategory.polling.ms > t.elapsedMs / 4 ? "  ⚠ polling over a quarter of the attempt" : "";
  return `${role}: ${parts.join(" · ")}${warn}`;
}

export function streamFiles(runDir: string): string[] {
  const dir = runPaths(runDir).stream;
  try {
    return fs
      .readdirSync(dir)
      .filter((f) => f.endsWith(".jsonl"))
      .map((f) => path.join(dir, f))
      .sort((a, b) => fs.statSync(a).mtimeMs - fs.statSync(b).mtimeMs);
  } catch {
    return [];
  }
}

// ---------------------------------------------------------------------------
// The view
// ---------------------------------------------------------------------------

export interface RunView {
  stage: string;
  stageElapsed: string;
  elapsed: string;
  pipeline: string;
  /** Checks/probe/reviews for the current candidate only; while a repair
   * attempt implements they are "pending (round N)". */
  gates: string;
  previousRound?: string;
  reviewers: ReviewerOutcome[];
  reviewLine: string;
  /** Why the phase did or did not accept, and what happens next. */
  verdict?: string;
  addressing: string[];
  rounds: Timeline["rounds"];
  round: number;
  time?: string;
  runningTool?: string;
  boundaryFilesChanged: number;
  liveDecisions: number;
  failedDecisions: number;
  /** Reserved decisions on the current candidate: voted like any other,
   * flagged so the owner can look (and override through the input box). */
  flaggedDecisions: number;
  openFindings: number;
  needsYou: number;
  /** Minutes since the active agent last produced an event (agent stages only). */
  idleMinutes?: number;
  attention?: string;
}

export function buildView(runDir: string, plan: RunPlanFile, alive: boolean, now = new Date()): RunView & { timeline: Timeline } {
  const timeline = rebuildTimeline(runDir, plan);
  const phase = timeline.state.phase;
  const spans = stageSpans(timeline, now, alive ? undefined : lastEventAt(runDir));
  const current = spans[spans.length - 1];
  const stage = current?.stage ?? "implement";
  const repairing = (stage === "implement" && phase.attempt.n > 1) || stage === "needs you";
  const C = phase.candidate?.sha;
  const reviewers = reviewerOutcomes(phase);
  const reasons = notAcceptedReasons(phase);
  const round = timeline.rounds.length + (C ? 1 : 0);

  const gate = (r: { candidateSha: string; passed?: boolean } | undefined, reused = false) =>
    !r || r.candidateSha !== C ? "⧗" : r.passed === true ? `✓${reused ? " (reused)" : ""}` : r.passed === false ? "✗" : "⧗";
  let gates: string;
  let previousRound: string | undefined;
  if (repairing && stage === "implement") {
    gates = `pending (round ${round + 1})`;
    previousRound = C ? `round ${round} · ${C.slice(0, 7)} · ${reasons.length > 0 ? `not accepted: ${reasons.join("; ")}` : "not accepted"}` : undefined;
  } else {
    gates = C ? `checks ${gate(phase.checks)} · probe ${gate(phase.probe, probeReused(runDir, C))}` : "no candidate yet";
  }

  let verdict: string | undefined;
  if (phase.phase === "DONE") verdict = "accepted and published";
  else if (phase.phase === "BLOCKED") verdict = `blocked: ${phase.blockedReason ?? "see the log"}`;
  else if (reasons.length > 0) {
    const next =
      phase.phase === "AWAITING_OWNER"
        ? "needs you (type a correction below)"
        : stage === "implement"
          ? `repair attempt ${phase.attempt.n}`
          : stage;
    verdict = `not accepted: ${reasons.join("; ")} → ${next}`;
  } else if (phase.phase === "AWAITING_OWNER") verdict = "needs you (type a correction below)";

  const files = streamFiles(runDir);
  const agentStage = stage === "implement" || stage === "review";
  const activeFile = files[files.length - 1];
  const t = activeFile && alive && agentStage ? agentTime(activeFile, now, true) : undefined;
  const idleMinutes = t ? (now.getTime() - Date.parse(t.lastAt)) / 60_000 : undefined;
  const needsYou = phase.ownerRequests.filter((r) => r.status === "open").length;
  const live = phase.decisions.filter((d) => isLiveDecision(d) && d.source !== "trigger");
  const failed = live.filter((d) => decisionStatus(d, phase).status === "failed").length;
  const flagged = live.filter((d) => d.class === "reserved" && d.boundCandidateSha === C).length;
  const openFindings = phase.findings.filter((f) => f.status === "open").length;

  let attention: string | undefined;
  if (phase.phase === "BLOCKED") attention = "BLOCKED";
  else if (needsYou > 0 || phase.phase === "AWAITING_OWNER") attention = "needs you";
  else if (!alive && phase.phase !== "DONE") attention = "conductor stopped";
  else if (idleMinutes !== undefined && idleMinutes > 5) attention = `idle ${Math.round(idleMinutes)}m`;
  else if (t && t.byCategory.polling.ms > t.elapsedMs / 4) attention = "heavy polling";

  const firstAt = timeline.phases[0]?.at;
  const endAt = phase.phase === "DONE" || phase.phase === "BLOCKED" ? Date.parse(timeline.phases[timeline.phases.length - 1].at) : now.getTime();
  return {
    timeline,
    stage,
    stageElapsed: formatDuration(current?.ms ?? 0),
    elapsed: firstAt ? formatDuration(endAt - Date.parse(firstAt)) : "0s",
    pipeline: pipelineLine(spans),
    gates,
    previousRound,
    reviewers,
    reviewLine: reviewers.map((r) => r.label).join("   "),
    verdict,
    addressing: repairing ? reasons : [],
    rounds: timeline.rounds,
    round,
    time: t ? timeLine(t) : undefined,
    runningTool: t?.running ? `${t.running.name} ${t.running.arg}` : undefined,
    boundaryFilesChanged: phase.decisions.filter((d) => d.source === "trigger" && d.boundCandidateSha === C).length,
    liveDecisions: live.length,
    failedDecisions: failed,
    flaggedDecisions: flagged,
    openFindings,
    needsYou,
    idleMinutes,
    attention,
  };
}

function lastEventAt(runDir: string): string | undefined {
  try {
    const lines = fs.readFileSync(runPaths(runDir).events, "utf8").trimEnd().split("\n");
    return (JSON.parse(lines[lines.length - 1]) as { ts?: string }).ts;
  } catch {
    return undefined;
  }
}

function probeReused(runDir: string, C: string): boolean {
  try {
    const text = fs.readFileSync(runPaths(runDir).events, "utf8");
    return text.includes('"probe_checks_reused"') && text.includes(C);
  } catch {
    return false;
  }
}

/** `tt timing`: per agent, where the time went. */
export function timingReport(runDir: string, now = new Date()): AgentTime[] {
  return streamFiles(runDir)
    .map((f) => agentTime(f, now, false))
    .filter((t): t is AgentTime => t !== undefined);
}

export function timingText(times: AgentTime[]): string {
  const out: string[] = [];
  for (const t of times) {
    out.push(
      `${t.agentId}  ${t.startedAt.slice(11, 19)}–${t.lastAt.slice(11, 19)}  elapsed ${formatDuration(t.elapsedMs)} · tools ${formatDuration(t.toolMs)} · model ${formatDuration(t.modelMs)} (${pct(t.modelMs, t.elapsedMs)})`,
    );
    for (const [cat, v] of Object.entries(t.byCategory).sort((a, b) => b[1].ms - a[1].ms)) {
      if (v.calls === 0) continue;
      out.push(`  ${cat.padEnd(14)} ${String(v.calls).padStart(3)} calls  ${formatDuration(v.ms).padStart(7)}  ${pct(v.ms, t.elapsedMs).padStart(4)}`);
    }
    for (const c of t.slowest.slice(0, 3)) {
      out.push(`    ${formatDuration(c.ms).padStart(7)}  ${c.name}: ${c.arg.replace(/\s+/g, " ").slice(0, 90)}`);
    }
  }
  return out.join("\n");
}

/** Pure helper for PhaseState-only callers (tests). */
export function reviewLineFor(phase: PhaseState): string {
  return reviewerOutcomes(phase)
    .map((r) => r.label)
    .join("   ");
}

// ---------------------------------------------------------------------------
// PR summary (skill fix 3): what a reviewer of the PR needs, including the
// advisory findings the loop accepted without fixing — run 9120dca7 (12c)
// finished with 9 of them, and nothing carried them past the run.
// ---------------------------------------------------------------------------

export function prSummary(runDir: string, plan: RunPlanFile, extra: { removedTests?: string[] } = {}): string {
  const v = buildView(runDir, plan, false);
  const phase = v.timeline.state.phase;
  const C = phase.candidate?.sha;
  const goal = phase.contract.goal.length > 600 ? `${phase.contract.goal.slice(0, 599)}…` : phase.contract.goal;
  const live = phase.decisions.filter((d) => isLiveDecision(d) && d.source !== "trigger" && d.boundCandidateSha === C);
  const flagged = live.filter((d) => d.class === "reserved");
  const advisories = phase.findings.filter((f) => f.status === "open" && f.severity === "advisory");
  const fixed = phase.findings.filter((f) => f.severity === "blocking" && f.status === "repaired");
  // Plan 01i: the owner's rulings in force are part of the PR body — binding
  // on reviewers, and the reason a `⚑` decision reads the way it does.
  const directives = (phase.ownerDirectives ?? []).filter((d) => d.status === "in-force");
  const lines = [
    `## ${phase.phaseId}`,
    "",
    goal,
    "",
    ...(directives.length > 0
      ? [
          "### Owner directives (binding)",
          "",
          ...directives.map((d) => `- **${d.id}**${d.scope === "program" ? " (whole program)" : ""}: ${d.text}`),
          "",
        ]
      : []),
    "### Review (tradeoffs-trace)",
    "",
    `- Pipeline: ${v.pipeline}`,
    `- Reviews on the accepted candidate ${C ? C.slice(0, 9) : "?"}: ${v.reviewLine}`,
    `- ${v.round} review round(s); ${fixed.length} blocking finding(s) raised and fixed before acceptance`,
    `- ${live.length} decision(s), ${flagged.length} flagged for the owner`,
  ];
  if (fixed.length > 0) {
    lines.push("", "### Blocking findings fixed during review", "");
    for (const f of fixed) lines.push(`- **${f.raisedBy}**: ${oneLine(f.evidence, 300)}`);
  }
  if (flagged.length > 0) {
    lines.push("", "### Flagged decisions (reserved: worth an owner's look)", "");
    for (const d of flagged) lines.push(`- ${d.choice} — ${decisionStatus(d, phase).status}`);
  }
  if (advisories.length > 0) {
    lines.push("", `### Open advisory findings (${advisories.length}) — accepted, not fixed`, "");
    for (const f of advisories) lines.push(`- **${f.raisedBy}**: ${oneLine(f.evidence, 400)}`);
  }
  if (extra.removedTests && extra.removedTests.length > 0) {
    lines.push("", `### Tests removed from files that still exist (${extra.removedTests.length})`, "");
    for (const t of extra.removedTests) lines.push(`- ${t}`);
  }
  return `${lines.join("\n")}\n`;
}

function oneLine(text: string, max: number): string {
  const t = text.replace(/\s+/g, " ").trim();
  return t.length > max ? `${t.slice(0, max - 1)}…` : t;
}
