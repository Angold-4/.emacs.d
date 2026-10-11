// Plan 06l (A5): the seat table. Per phase and per program, for each seat
// and model: findings raised, unique (raised by no other seat), accepted,
// dropped, review turns, timeouts (REVIEW_TIMED_OUT), retries and re-prompts,
// refused submissions, and minutes per turn (p50/max).
//
// The table is a deterministic projection of the run's control log: no wall
// clock, no agent's summary. The conductor writes `views/seats.txt` from it,
// `tt seats <run|program>` prints it, and the phase summary appends it. A
// program's table keeps a row per (seat, model), so a seat that ran on two
// models in two nodes never has its findings credited to the wrong one.
//
// Pure: `computeSeatTable` reads only `LogRecord[]` (and the seat list), so a
// restart and `tt contract rebuild` produce the same bytes.

import type { LogRecord } from "../effects/log.ts";

/** One seat-and-model row: what the seat cost and what it found. */
export interface SeatCounts {
  seat: string;
  /** The reviewer model this seat ran on, when the run recorded one. */
  model?: string;
  /** FINDING_RAISED records whose `raisedBy` is this seat. */
  findingsRaised: number;
  /** Of those, the ones no other seat also raised (FINDING_ALSO_RAISED). */
  findingsUnique: number;
  /** Findings of this seat the owner accepted (FINDING_ACCEPTED_BY_OWNER). */
  findingsAccepted: number;
  /** Findings of this seat disproved/dropped (FINDING_DISPROVED). */
  findingsDropped: number;
  /** Review submissions: REVIEW_SUBMITTED + ROUND_REVIEW_SUBMITTED. */
  reviewTurns: number;
  /** REVIEW_TIMED_OUT records for this seat. */
  timeouts: number;
  /** `lane_review_retried` + `pick_retried` records (a retry STARTED once). */
  retries: number;
  /** `review_reprompt` records (a turn that settled without its submission). */
  reprompts: number;
  /** `incomplete_review_rejected` records: a submission the conductor refused. */
  refusedSubmissions: number;
  /** Per-review-turn wall minutes: the median and the maximum. */
  turnMinutes: { p50: number; max: number };
  /** The raw per-turn durations behind `turnMinutes`, so a program can pool
   * them exactly instead of averaging the per-node percentiles. Not rendered. */
  turnMs?: number[];
}

export interface SeatTable {
  seats: SeatCounts[];
}

interface MutableSeat {
  seat: string;
  model?: string;
  findingsRaised: number;
  findingsAccepted: number;
  findingsDropped: number;
  reviewTurns: number;
  timeouts: number;
  retries: number;
  reprompts: number;
  refusedSubmissions: number;
  turnMs: number[];
}

function seatFromEvent(event: unknown): string | undefined {
  if (!event || typeof event !== "object") return undefined;
  const e = event as Record<string, unknown>;
  if (typeof e.seat === "string" && e.seat.length > 0) return e.seat;
  if (typeof e.reviewer === "string" && e.reviewer.length > 0) return e.reviewer;
  const review = e.review;
  if (review && typeof review === "object") {
    const r = review as Record<string, unknown>;
    if (typeof r.reviewer === "string" && r.reviewer.length > 0) return r.reviewer;
  }
  return undefined;
}

/** A `#+TT_MODELS` map from the init record, when one was recorded. */
function modelsFromInit(records: readonly LogRecord[]): Record<string, string> {
  const init = records.find((r) => r.kind === "init");
  const models = init && typeof init.event === "object" && init.event !== null ? (init.event as { models?: unknown }).models : undefined;
  if (!models || typeof models !== "object") return {};
  const out: Record<string, string> = {};
  for (const [role, value] of Object.entries(models as Record<string, unknown>)) {
    if (typeof value === "string") out[role] = value;
    else if (value && typeof value === "object") {
      const v = value as { model?: unknown };
      if (typeof v.model === "string") out[role] = v.model;
    }
  }
  return out;
}

function median(values: number[]): number {
  if (values.length === 0) return 0;
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 0 ? (sorted[mid - 1] + sorted[mid]) / 2 : sorted[mid];
}

/** Build the seat table from one run's control log. `seats` is the phase's
 * seat list (the same list every other view uses); a seat with no records
 * still gets a zero row, so the table always names every seat. `seatModels`
 * names the model each seat ran on (from the plan's `#+TT_MODELS`), when
 * known; the init event's own models win when both are present. */
export function computeSeatTable(records: readonly LogRecord[], seats: readonly string[], seatModels: Record<string, string | undefined> = {}): SeatTable {
  const rows = new Map<string, MutableSeat>();
  // A seat's own model (from the plan) is the row every record for that seat
  // lands in, so a finding and a review turn for seat M never split across a
  // modelled and an unmodelled M row.
  const defaultModel = new Map<string, string>();
  for (const seat of seats) {
    const m = seatModels[seat];
    if (m) defaultModel.set(seat, m);
  }
  const keyOf = (seat: string, model: string | undefined): string => `${seat}\u0000${model ?? ""}`;
  const ensure = (seat: string, model?: string): MutableSeat => {
    const key = keyOf(seat, model ?? defaultModel.get(seat));
    let row = rows.get(key);
    if (!row) {
      const resolved = model ?? defaultModel.get(seat);
      row = {
        seat,
        ...(resolved ? { model: resolved } : {}),
        findingsRaised: 0,
        findingsAccepted: 0,
        findingsDropped: 0,
        reviewTurns: 0,
        timeouts: 0,
        retries: 0,
        reprompts: 0,
        refusedSubmissions: 0,
        turnMs: [],
      };
      rows.set(key, row);
    }
    return row;
  };
  for (const seat of seats) ensure(seat);
  const reviewerModel = modelsFromInit(records)["reviewer"];
  if (reviewerModel) {
    // The init record's model is per role, so it applies to every seat row
    // that has no more specific model; a row keyed by an explicit model keeps
    // its own.
    for (const row of rows.values()) if (!row.model) row.model = reviewerModel;
  }

  // findingId -> the seat that raised it, so later dispositions count against
  // the raiser rather than the disposer. `coRaisers` records every other seat
  // that agreed with the finding (a later FINDING_ALSO_RAISED event), so the
  // finding is not unique for anyone.
  const findingOwner = new Map<string, string>();
  const coRaisers = new Map<string, Set<string>>();
  // actionId -> review start, for the per-turn durations.
  const reviewStart = new Map<string, { seat: string; at: number }>();
  const turnMs = new Map<string, number[]>();

  for (const record of records) {
    const event = record.event;
    const e = event && typeof event === "object" ? (event as Record<string, unknown>) : {};
    if (record.kind === "intent") {
      const seat = seatFromEvent(event);
      // A review intent names either a lane review's `seat` (with a
      // candidateSha) or the single-candidate path's `reviewer`.
      if (seat && (e.candidateSha !== undefined || e.reviewer !== undefined)) {
        reviewStart.set(record.actionId ?? `${record.seq}`, { seat, at: Date.parse(record.ts) });
      }
      continue;
    }
    if (record.kind === "completion") {
      const start = record.actionId ? reviewStart.get(record.actionId) : undefined;
      // The single-candidate review completion logs `{ reviewer, ok }`; the
      // lane review logs `{ seat, ok }`. Accept either.
      const seat = typeof e.seat === "string" ? e.seat : typeof e.reviewer === "string" ? e.reviewer : undefined;
      if (start && seat !== undefined) {
        const ms = Date.parse(record.ts) - start.at;
        if (Number.isFinite(ms) && ms >= 0) {
          const list = turnMs.get(start.seat) ?? [];
          list.push(ms);
          turnMs.set(start.seat, list);
        }
      }
      continue;
    }
    if (record.kind !== "event") continue;
    const type = typeof e.type === "string" ? e.type : "";
    switch (type) {
      case "FINDING_RAISED": {
        const finding = e.finding as { id?: unknown; raisedBy?: unknown; alsoRaisedBy?: unknown } | undefined;
        if (!finding || typeof finding.raisedBy !== "string") break;
        ensure(finding.raisedBy).findingsRaised += 1;
        if (typeof finding.id === "string") {
          findingOwner.set(finding.id, finding.raisedBy);
          const already = new Set<string>();
          if (Array.isArray(finding.alsoRaisedBy)) for (const r of finding.alsoRaisedBy) if (typeof r === "string") already.add(r);
          if (already.size > 0) coRaisers.set(finding.id, already);
        }
        break;
      }
      case "FINDING_ALSO_RAISED": {
        // Plan 2c: another reviewer agreed with an open finding. That seat is
        // a co-raiser, so the finding is not unique for its original raiser.
        const findingId = typeof e.findingId === "string" ? e.findingId : undefined;
        const reviewer = typeof e.reviewer === "string" ? e.reviewer : undefined;
        if (!findingId || !reviewer) break;
        const set = coRaisers.get(findingId) ?? new Set<string>();
        set.add(reviewer);
        coRaisers.set(findingId, set);
        break;
      }
      case "FINDING_DISPROVED": {
        const owner = typeof e.findingId === "string" ? findingOwner.get(e.findingId) : undefined;
        if (owner) ensure(owner).findingsDropped += 1;
        break;
      }
      case "FINDING_ACCEPTED_BY_OWNER": {
        const owner = typeof e.findingId === "string" ? findingOwner.get(e.findingId) : undefined;
        if (owner) ensure(owner).findingsAccepted += 1;
        break;
      }
      case "REVIEW_SUBMITTED":
      case "ROUND_REVIEW_SUBMITTED": {
        const seat = seatFromEvent(event);
        if (seat) ensure(seat).reviewTurns += 1;
        break;
      }
      case "REVIEW_TIMED_OUT": {
        const seat = seatFromEvent(event);
        if (seat) ensure(seat).timeouts += 1;
        break;
      }
      default:
        break;
    }
  }

  // Log records that are not `event` kinds: retries, re-prompts and refused
  // submissions are `append(...)` records with their own kind.
  for (const record of records) {
    if (record.kind === "event" || record.kind === "intent" || record.kind === "completion") continue;
    const seat = seatFromEvent(record.event);
    if (!seat) continue;
    // A retry is counted once, at its START: `lane_review_retry_failed` (and
    // `pick_retry_failed`) is the same retry failing, not a second retry.
    if (record.kind === "lane_review_retried" || record.kind === "pick_retried") {
      ensure(seat).retries += 1;
    } else if (record.kind === "review_reprompt") {
      ensure(seat).reprompts += 1;
    } else if (record.kind === "incomplete_review_rejected") {
      ensure(seat).refusedSubmissions += 1;
    }
  }

  // Findings: unique = raised by this seat and no other.
  const unique = new Map<string, number>();
  for (const [id, owner] of findingOwner) {
    if ((coRaisers.get(id)?.size ?? 0) > 0) continue;
    unique.set(owner, (unique.get(owner) ?? 0) + 1);
  }

  const out: SeatCounts[] = [];
  for (const row of rows.values()) {
    const ms = turnMs.get(row.seat) ?? [];
    out.push({
      seat: row.seat,
      ...(row.model ? { model: row.model } : {}),
      findingsRaised: row.findingsRaised,
      findingsUnique: unique.get(row.seat) ?? 0,
      findingsAccepted: row.findingsAccepted,
      findingsDropped: row.findingsDropped,
      reviewTurns: row.reviewTurns,
      timeouts: row.timeouts,
      retries: row.retries,
      reprompts: row.reprompts,
      refusedSubmissions: row.refusedSubmissions,
      turnMinutes: {
        p50: round2(median(ms) / 60_000),
        max: round2((ms.length > 0 ? Math.max(...ms) : 0) / 60_000),
      },
      ...(ms.length > 0 ? { turnMs: ms } : {}),
    });
  }
  // Stable order: the declared seat order, then by model.
  const order = new Map(seats.map((s, i) => [s, i]));
  out.sort((a, b) => (order.get(a.seat) ?? 999) - (order.get(b.seat) ?? 999) || (a.model ?? "").localeCompare(b.model ?? ""));
  return { seats: out };
}

/** Merge per-run tables into a program table. Rows are keyed by (seat,
 * model), so a seat that ran on two models keeps two rows; counts add and the
 * per-turn minutes are pooled from the raw durations. */
export function mergeSeatTables(tables: readonly SeatTable[]): SeatTable {
  const merged = new Map<string, SeatCounts>();
  const order: string[] = [];
  for (const table of tables) {
    for (const row of table.seats) {
      const key = `${row.seat}\u0000${row.model ?? ""}`;
      let existing = merged.get(key);
      if (!existing) {
        existing = { ...row, turnMs: row.turnMs ? [...row.turnMs] : [] };
        merged.set(key, existing);
        order.push(key);
      } else {
        existing.findingsRaised += row.findingsRaised;
        existing.findingsUnique += row.findingsUnique;
        existing.findingsAccepted += row.findingsAccepted;
        existing.findingsDropped += row.findingsDropped;
        existing.reviewTurns += row.reviewTurns;
        existing.timeouts += row.timeouts;
        existing.retries += row.retries;
        existing.reprompts += row.reprompts;
        existing.refusedSubmissions += row.refusedSubmissions;
        existing.turnMs = [...(existing.turnMs ?? []), ...(row.turnMs ?? [])];
      }
    }
  }
  const out = order.map((key) => {
    const row = merged.get(key)!;
    const ms = row.turnMs ?? [];
    const { turnMs: _turnMs, ...rest } = row;
    return {
      ...rest,
      turnMinutes: {
        p50: round2(median(ms) / 60_000),
        max: round2((ms.length > 0 ? Math.max(...ms) : 0) / 60_000),
      },
      ...(ms.length > 0 ? { turnMs: ms } : {}),
    };
  });
  return { seats: out };
}

function round2(n: number): number {
  return Math.round(n * 100) / 100;
}

/** `views/seats.txt`: one header line and one line per seat/model. Stable and
 * machine-readable enough that `tt seats` prints the same bytes for a run. */
export function renderSeatTable(table: SeatTable): string {
  const header = ["seat", "model", "raised", "unique", "accepted", "dropped", "turns", "timeouts", "retries", "reprompts", "refused", "min/turn p50", "max"];
  const rows = table.seats.map((s) => [
    s.seat,
    s.model ?? "-",
    String(s.findingsRaised),
    String(s.findingsUnique),
    String(s.findingsAccepted),
    String(s.findingsDropped),
    String(s.reviewTurns),
    String(s.timeouts),
    String(s.retries),
    String(s.reprompts),
    String(s.refusedSubmissions),
    s.turnMinutes.p50.toFixed(2),
    s.turnMinutes.max.toFixed(2),
  ]);
  const widths = header.map((h, i) => Math.max(h.length, ...rows.map((r) => r[i].length)));
  const line = (cells: string[]) => cells.map((c, i) => c.padEnd(widths[i])).join("  ").trimEnd();
  return [`# seats`, line(header), ...rows.map(line)].join("\n") + "\n";
}

/** The `tt seats`/phase-summary text: the same table, with a totals line. */
export function seatTableLines(table: SeatTable): string[] {
  return renderSeatTable(table).trimEnd().split("\n");
}
