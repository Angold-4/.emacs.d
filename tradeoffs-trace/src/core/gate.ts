// Plan 01f: the gate — the phase's own expensive, live command (atlas plan
// 13's `deploy/atlas.sh … --clean --build` of a 40-service stack, ≈15 min).
// The conductor runs it itself, once per candidate whose checks, probe and
// three reviews have passed, before acceptance (design 01_ref_design.md D3,
// runtime doc §6: agents ran it inside their attempts, 82 minutes of docker
// in 13i/13j, and — because an agent had to produce the live proof — they
// wrote substitutes for it: a sentinel `code_sha`, `pending_owner_live_run`,
// a fingerprint-only record).
//
// This module is the pure half: the on-disk record's shape (`gate.json`),
// its parser, the log tail a failure quotes, and the reuse rule that keeps
// an identical tree from paying for the same gate twice. The conductor
// writes the record; the view and `tt summary` read it.
//
// Nothing here decides *whether* a phase gates: `contract.gate` (the plan's
// `:GATE:` command, frozen into the contract) does, and next.ts/
// transitions.ts read it.

/** The gate command a contract declares, or undefined when the phase has no
 * gate (every run that predates plan 01f, and any phase whose plan does not
 * write a `:GATE:` property). */
export function gateCommandOf(contract: { gate?: string } | undefined): string | undefined {
  const command = contract?.gate;
  return typeof command === "string" && command.trim().length > 0 ? command : undefined;
}

/** How many lines of the gate's log a failure quotes (the finding's
 * evidence, and the repair prompt the worker is shown). */
export const GATE_TAIL_LINES = 60;

/** One gate run's evidence, as `checks/<sha>/gate.json` holds it. The
 * candidate and base SHA, the command, the exit status, the duration, the
 * start time and the log's sha256 are the design's own required fields
 * (01_ref_design.md D3); the rest is what makes the record checkable and
 * reusable. */
export interface GateRecord {
  /** The candidate the gate ran for. */
  candidateSha: string;
  /** The candidate's tree object id — the identity a reuse decision is made
   * on (a repair attempt that changes nothing produces a new commit with the
   * same tree, and the gate's answer cannot differ for it). */
  tree?: string;
  /** The integration head H the candidate was probed onto and the gate ran
   * against. */
  baseSha: string;
  /** The merge result I the gate's checkout held (equals the candidate on
   * the normal fast-forward probe). */
  mergedI?: string;
  /** The command as the conductor ran it (masked, like every other run
   * file). */
  command: string;
  /** The `:GATE_CLEANUP:` command, when the plan declares one. */
  cleanup?: string;
  /** When the command started — or, for a reused record, when the reuse was
   * recorded (the gate itself did not run for this candidate). */
  startedAt: string;
  /** How long the command took — the source run's duration for a reused
   * record. */
  durationMs: number;
  exitCode: number | null;
  signal?: string | null;
  timedOut: boolean;
  /** exitCode 0 and not killed at the limit. */
  passed: boolean;
  /** sha256 of the (redacted) `gate.log` written beside this record. */
  logSha256: string;
  logBytes: number;
  /** True when the command was not run at all: an identical tree had already
   * passed the same gate, and that record is the evidence. */
  reused?: boolean;
  /** The candidate whose gate is being reused (set only with `reused`). */
  reusedFrom?: string;
  /** The base SHA that reused record's evidence came from, when it differs
   * from this candidate's (the honest form: the evidence is the older run's). */
  reusedFromBaseSha?: string;
  cleanupStartedAt?: string;
  cleanupDurationMs?: number;
  cleanupExitCode?: number | null;
  cleanupTimedOut?: boolean;
  /** Set instead of the cleanup fields when there was no checkout to clean
   * (a candidate that no longer merges): recorded rather than guessed. */
  cleanupSkipped?: string;
}

function isString(v: unknown): v is string {
  return typeof v === "string" && v.length > 0;
}

function isNumber(v: unknown): v is number {
  return typeof v === "number" && Number.isFinite(v);
}

function isExitCode(v: unknown): v is number | null {
  return v === null || (isNumber(v) && Number.isInteger(v));
}

/** Reads a `gate.json` back, returning undefined for anything malformed — a
 * hand-edited or half-written record is treated as "no record", never as a
 * passing gate. The required fields are the ones the design names: the
 * candidate and base SHA, the command, the exit status, the duration, the
 * start time and the log hash. */
export function parseGateRecord(value: unknown): GateRecord | undefined {
  if (!value || typeof value !== "object") return undefined;
  const r = value as Record<string, unknown>;
  if (!isString(r.candidateSha)) return undefined;
  if (!isString(r.baseSha)) return undefined;
  if (typeof r.command !== "string") return undefined;
  if (!isExitCode(r.exitCode)) return undefined;
  if (!isNumber(r.durationMs) || r.durationMs < 0) return undefined;
  if (!isString(r.startedAt)) return undefined;
  if (!isString(r.logSha256)) return undefined;
  if (typeof r.timedOut !== "boolean") return undefined;
  if (typeof r.passed !== "boolean") return undefined;
  const record: GateRecord = {
    candidateSha: r.candidateSha,
    baseSha: r.baseSha,
    command: r.command,
    startedAt: r.startedAt,
    durationMs: r.durationMs,
    exitCode: r.exitCode,
    timedOut: r.timedOut,
    passed: r.passed,
    logSha256: r.logSha256,
    logBytes: isNumber(r.logBytes) ? r.logBytes : 0,
  };
  for (const key of ["tree", "mergedI", "cleanup", "reusedFrom", "reusedFromBaseSha", "cleanupSkipped"] as const) {
    if (isString(r[key])) record[key] = r[key] as string;
  }
  if (typeof r.signal === "string" || r.signal === null) record.signal = r.signal as string | null;
  if (r.reused === true) record.reused = true;
  if (isNumber(r.cleanupDurationMs)) record.cleanupDurationMs = r.cleanupDurationMs;
  if (isString(r.cleanupStartedAt)) record.cleanupStartedAt = r.cleanupStartedAt;
  // Present-but-null is meaningful (killed by a signal); absent is not set.
  if (isExitCode(r.cleanupExitCode)) record.cleanupExitCode = r.cleanupExitCode as number | null;
  if (typeof r.cleanupTimedOut === "boolean") record.cleanupTimedOut = r.cleanupTimedOut;
  return record;
}

/** The last `lines` lines of a gate log, for the finding's evidence and the
 * worker's repair prompt. The tail, never the head: a build that fails says
 * why at the end (the phase brief's "with the log's last 60 lines"). */
export function gateLogTail(text: string, lines = GATE_TAIL_LINES): string {
  if (text.length === 0) return "";
  const all = text.split("\n");
  if (all[all.length - 1] === "") all.pop();
  return all.slice(Math.max(0, all.length - lines)).join("\n");
}

/** The evidence a failed gate becomes: what ran, how it ended, where the
 * whole log is, and its last lines. This string is the blocking
 * `integration` finding's evidence, so it carries the log tail verbatim —
 * the worker must be able to see the failure, not a summary of it. */
export function gateFailureEvidence(opts: {
  record: Pick<GateRecord, "candidateSha" | "command" | "exitCode" | "timedOut" | "durationMs" | "startedAt" | "logSha256">;
  logPath: string;
  tail: string;
  reason?: string;
}): string {
  const { record } = opts;
  const how = record.timedOut
    ? `was killed at its limit after ${Math.round(record.durationMs / 1000)}s`
    : `exited ${record.exitCode === null ? "on a signal" : record.exitCode}`;
  const lines = [
    `gate command ${how} on candidate ${record.candidateSha.slice(0, 9)}: ${record.command}`,
    `log: ${opts.logPath} (sha256 ${record.logSha256})`,
  ];
  if (opts.reason) lines.push(opts.reason);
  if (opts.tail.trim().length > 0) {
    lines.push(`last ${GATE_TAIL_LINES} lines of the gate log:`, opts.tail);
  } else {
    lines.push("the gate log is empty");
  }
  return lines.join("\n");
}

/** True iff `record` is a passing gate that answers the same question this
 * candidate asks: the same tree, gated with the same command. The base SHA
 * deliberately does not have to match — the gate is dispatched once per
 * candidate (a stale-publish retry re-probes the same candidate, and a
 * repair attempt that changes nothing freezes a new commit with the same
 * tree), and the record names the base the evidence came from
 * (`reusedFromBaseSha`) when it differs. */
export function gateRecordAnswers(record: GateRecord | undefined, want: { tree?: string; command: string }): boolean {
  // A reused record is not a source: the original pass is the evidence, and
  // reusing a reuse would just add indirection.
  if (!record || !record.passed || record.reused) return false;
  if (!want.tree || !record.tree) return false;
  return record.tree === want.tree && record.command === want.command;
}

/** The record to reuse for a candidate with this tree and this command, or
 * undefined when the gate must run: the newest passing record among the
 * run's gate records that answers the same question (a repair round that
 * changes nothing reuses the previous round's pass instead of paying for the
 * same build again — the disk-filling repeats of runtime doc §6). */
export function reusableGate(
  records: readonly GateRecord[],
  want: { tree?: string; command: string },
): GateRecord | undefined {
  const answering = records.filter((r) => gateRecordAnswers(r, want));
  if (answering.length === 0) return undefined;
  return answering.reduce((best, r) => (Date.parse(r.startedAt) > Date.parse(best.startedAt) ? r : best));
}
