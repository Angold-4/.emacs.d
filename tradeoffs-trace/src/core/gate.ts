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
// an identical tree from paying for the same gate twice (including the
// re-hash that refuses a record whose log no longer matches it). The
// conductor writes the record; the view and `tt summary` read it.
//
// Nothing here decides *whether* a phase gates: `contract.gate` (the plan's
// `:GATE:` command, frozen into the contract) does, and next.ts/
// transitions.ts read it.

import { createHash } from "node:crypto";

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
  /** The integration head this record's evidence stands for: the head the
   * gate ran against, and — for a record written as a reuse — the head the
   * candidate is being accepted against (`reusedFromBaseSha` then names the
   * head the command actually ran on). See `gateDecision`/`reusableGate`. */
  baseSha: string;
  /** The merge result I the gate's checkout held (equals the candidate on
   * the normal fast-forward probe). Absent on a reused record: its evidence
   * was produced for `reusedFromBaseSha`, not for this head. */
  mergedI?: string;
  /** The command as the conductor ran it (masked, like every other run
   * file). */
  command: string;
  /** The `:GATE_CLEANUP:` command, when the plan declares one. */
  cleanup?: string;
  /** When the command that produced this evidence started, and how long it
   * took: a reused record keeps the source run's own facts (the evidence was
   * produced then, not when it was reused). Absent `durationMs` and
   * `exitCode` mean the command never started — `notStarted` says why. */
  startedAt: string;
  durationMs?: number;
  exitCode?: number | null;
  signal?: string | null;
  timedOut?: boolean;
  /** exitCode 0 and not killed at the limit. */
  passed: boolean;
  /** sha256 of the (redacted) `gate.log` written beside this record. */
  logSha256: string;
  logBytes: number;
  /** True when the command was not run at all: an identical tree had already
   * passed the same gate, and that record's verified log is the evidence. */
  reused?: boolean;
  /** The candidate whose gate is being reused (set only with `reused`). */
  reusedFrom?: string;
  /** The head the reused command actually ran against (set with `reused`;
   * `baseSha` is the head this candidate is being accepted against). */
  reusedFromBaseSha?: string;
  cleanupStartedAt?: string;
  cleanupDurationMs?: number;
  cleanupExitCode?: number | null;
  cleanupTimedOut?: boolean;
  /** Set instead of the cleanup fields when there was no checkout to clean. */
  cleanupSkipped?: string;
  /** Set (with no `exitCode`/`durationMs`/`timedOut`) when the gate command
   * never started at all: today the only case is a candidate that no longer
   * merges onto the integration head, so there was nothing to run and nothing
   * to clean up. The record says so rather than claiming an exit-less run. */
  notStarted?: string;
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
 * start time and the log hash — except on a record that says the command
 * never started (`notStarted`), which carries no exit status or duration. */
export function parseGateRecord(value: unknown): GateRecord | undefined {
  if (!value || typeof value !== "object") return undefined;
  const r = value as Record<string, unknown>;
  if (!isString(r.candidateSha)) return undefined;
  if (!isString(r.baseSha)) return undefined;
  if (typeof r.command !== "string") return undefined;
  if (!isString(r.startedAt)) return undefined;
  if (!isString(r.logSha256)) return undefined;
  if (typeof r.passed !== "boolean") return undefined;
  const notStarted = isString(r.notStarted);
  if (!notStarted) {
    if (!isExitCode(r.exitCode)) return undefined;
    if (!isNumber(r.durationMs) || r.durationMs < 0) return undefined;
    if (typeof r.timedOut !== "boolean") return undefined;
  }
  const record: GateRecord = {
    candidateSha: r.candidateSha,
    baseSha: r.baseSha,
    command: r.command,
    startedAt: r.startedAt,
    passed: r.passed,
    logSha256: r.logSha256,
    logBytes: isNumber(r.logBytes) ? r.logBytes : 0,
  };
  if (!notStarted) {
    record.durationMs = r.durationMs as number;
    record.exitCode = r.exitCode as number | null;
    record.timedOut = r.timedOut as boolean;
  } else {
    record.notStarted = r.notStarted as string;
  }
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
  record: Pick<GateRecord, "candidateSha" | "command" | "exitCode" | "timedOut" | "durationMs" | "startedAt" | "logSha256" | "notStarted">;
  logPath: string;
  tail: string;
  reason?: string;
}): string {
  const { record } = opts;
  // A gate that never started did not exit, and did not get killed: say what
  // actually happened instead of dressing it up as an exit-less run.
  const how = record.notStarted
    ? `did not start: ${record.notStarted}`
    : record.timedOut
      ? `was killed at its limit after ${Math.round((record.durationMs ?? 0) / 1000)}s`
      : `exited ${record.exitCode === null || record.exitCode === undefined ? "on a signal" : record.exitCode}`;
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

/** True iff `log`'s bytes are exactly the log this record names: its sha256
 * matches `logSha256` (and its length matches `logBytes` when the record
 * carries one). The conductor re-hashes before it reuses anything — a
 * record whose log was pruned, truncated or edited is **not** evidence, and
 * the gate reruns rather than accepting on it (runtime doc §6's substitute
 * problem is exactly this shape: a record with no output behind it). */
export function gateLogHashMatches(record: GateRecord, log: Buffer): boolean {
  if (!isString(record.logSha256)) return false;
  if (record.logBytes > 0 && record.logBytes !== log.length) return false;
  return createHash("sha256").update(log).digest("hex") === record.logSha256;
}

/** What the conductor must do for a candidate's gate: accept the candidate's
 * **own** verified passing record (written for this candidate earlier — a
 * stale-publish retry re-gates the same candidate — and never rewritten),
 * reuse **another** candidate's verified passing record that answers the same
 * question, or run the command.
 *
 * Both branches go through `gateRecordAnswers`, so a record that answered a
 * different question is never evidence — a different `:GATE:` command (the
 * plan was re-read or amended) or a different tree reruns the gate. The
 * candidate's own record is never "reused": a self-reuse record would
 * overwrite the run's own evidence with a claim of reusing it. Callers pass
 * only records whose logs they have verified (`gateLogHashMatches`); an
 * unverified record must be passed as `undefined`/absent so the gate reruns. */
export function gateDecision(
  ownVerifiedPass: GateRecord | undefined,
  records: readonly GateRecord[],
  want: { candidateSha: string; tree?: string; command: string },
): { kind: "own"; record: GateRecord } | { kind: "reuse"; record: GateRecord } | { kind: "run" } {
  if (gateRecordAnswers(ownVerifiedPass, want)) return { kind: "own", record: ownVerifiedPass! };
  const reused = reusableGate(
    records.filter((r) => r.candidateSha !== want.candidateSha),
    want,
  );
  return reused ? { kind: "reuse", record: reused } : { kind: "run" };
}
/** True iff `record` is a passing gate that answers the same question this
 * candidate asks: the same tree, gated with the same command. The base SHA
 * deliberately does not have to match — the gate is dispatched once per
 * candidate (a repair attempt that changes nothing freezes a new commit with
 * the same tree), and the record names the head the evidence came from
 * (`reusedFromBaseSha`). A record for the candidate itself is not a reuse:
 * `gateDecision` handles that case separately. */
/** True iff `record` is a passing gate that answers the same question this
 * candidate asks: the same tree, gated with the same command. The base SHA
 * deliberately does not have to match — the gate is dispatched once per
 * candidate (a repair attempt that changes nothing freezes a new commit with
 * the same tree), and the record names the head the evidence came from
 * (`reusedFromBaseSha`).
 *
 * A record that was **itself written as a reuse** still answers the question:
 * its log is the verified copy of the run that produced the evidence, so a
 * candidate's own reuse record, and a chain of them, count as passing (the
 * alternative — dropping them — made an identical tree pay for the gate
 * again, and made a stale-publish re-gate rewrite a candidate's own record).
 *
 * A record whose command differs is not evidence for this gate: the plan
 * snapshot is re-read at every restart and an amended contract can name a
 * different `:GATE:` command, so a changed command means the old record
 * answered a different question. A record whose tree differs is another
 * candidate's pass. Callers verify the log's hash separately
 * (`gateLogHashMatches`). */
export function gateRecordAnswers(record: GateRecord | undefined, want: { tree?: string; command: string }): boolean {
  if (!record || !record.passed) return false;
  if (!want.tree || !record.tree) return false;
  return record.tree === want.tree && record.command === want.command;
}

/** The record to reuse for a candidate with this tree and this command, or
 * undefined when the gate must run: the newest passing record among the
 * run's gate records that answers the same question (a repair round that
 * changes nothing reuses the previous round's pass instead of paying for the
 * same build again — the disk-filling repeats of runtime doc §6). Callers
 * exclude the candidate's own record (`gateDecision` does) and verify the
 * chosen record's log before writing anything from it. A record that is
 * itself a reuse is a valid source: the conductor flattens its provenance to
 * the run that actually produced the evidence. */
export function reusableGate(
  records: readonly GateRecord[],
  want: { tree?: string; command: string },
): GateRecord | undefined {
  const answering = records.filter((r) => gateRecordAnswers(r, want));
  if (answering.length === 0) return undefined;
  return answering.reduce((best, r) => (Date.parse(r.startedAt) > Date.parse(best.startedAt) ? r : best));
}
