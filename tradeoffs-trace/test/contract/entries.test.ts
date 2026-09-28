// Plan 05j acceptance: entries are one topic each, anchoring decides links,
// nothing is hidden and nothing is unaccounted.
//
// The regression fixture is atlas 15a's first round (run 1989d1d0): 31 raw
// messages that reached the owner as 19 rows across three types (B-1 = T-14 =
// F-1, T-7 = F-3, T-15 = F-4, plus re-raises under new ids). With entries it
// is one entry per topic.

import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import {
  accountingLine,
  anchorFromEvidence,
  anchorsOfMessage,
  anchorsOverlap,
  applyEntryEvent,
  ENTRY_SIMILARITY_THRESHOLD,
  entryTypeOf,
  formatAnchor,
  normalisedTitleWords,
  curatorEvent,
  planEntryEvents,
  projectEntries,
  renderEntryFile,
  renderEntryReview,
  renderProgramEntryReview,
  sharedAnchor,
  titleSimilarity,
  validateCuratorProposal,
  type Entry,
  type EntryEvent,
} from "../../src/core/entries.ts";
import { runReviewLint, reviewLintFailedEvents } from "../../src/core/review-lint.ts";
import { computeMetrics } from "../../src/metrics.ts";
import type { Finding, Message } from "../../src/core/types.ts";
import { basePhase, makeMessage } from "../unit/helpers.ts";

const GOLDEN = fileURLToPath(new URL("../fixtures/entries", import.meta.url));

function golden(rel: string): string {
  return readFileSync(`${GOLDEN}/${rel}`, "utf8");
}

function assertGolden(rel: string, actual: string): void {
  if (process.env.TT_UPDATE_GOLDEN) {
    mkdirSync(`${GOLDEN}/${rel.split("/").slice(0, -1).join("/")}`, { recursive: true });
    writeFileSync(`${GOLDEN}/${rel}`, actual);
    return;
  }
  assert.ok(existsSync(`${GOLDEN}/${rel}`), `missing golden ${rel}; run with TT_UPDATE_GOLDEN=1 to write it`);
  assert.equal(actual, golden(rel));
}

// ---------------------------------------------------------------------------
// Anchors
// ---------------------------------------------------------------------------

test("a file:line evidence becomes a normalised file anchor", () => {
  assert.deepEqual(anchorFromEvidence("src/cancel.rs:88-104 the frame counter"), { kind: "file", path: "src/cancel.rs", lines: [88, 104] });
  assert.deepEqual(anchorFromEvidence("./src/x.ts:10"), { kind: "file", path: "src/x.ts", lines: [10, 10] });
  assert.equal(anchorFromEvidence("no file here"), undefined);
});

test("anchors overlap only in the same file or on the same id/clause", () => {
  const a = { kind: "file" as const, path: "src/a.rs", lines: [10, 20] as [number, number] };
  const b = { kind: "file" as const, path: "src/a.rs", lines: [20, 30] as [number, number] };
  const c = { kind: "file" as const, path: "src/b.rs", lines: [10, 20] as [number, number] };
  assert.ok(anchorsOverlap(a, b));
  assert.ok(!anchorsOverlap(a, c));
  assert.ok(anchorsOverlap({ kind: "decision", id: "D-1" }, { kind: "decision", id: "D-1" }));
  assert.ok(!anchorsOverlap({ kind: "decision", id: "D-1" }, { kind: "decision", id: "D-2" }));
  assert.ok(anchorsOverlap({ kind: "plan", clause: "plan/14a.org" }, { kind: "plan", clause: "plan/14a.org " }));
});

test("a message derives file, decision and plan anchors from its evidence", () => {
  const m = makeMessage({
    evidence: ["src/priced_frame.rs:88-104 the counter is never priced"],
    sourceRecordId: "D-M-36",
    planRef: "plan/15a.org",
  });
  const anchors = anchorsOfMessage(m);
  assert.deepEqual(anchors.map(formatAnchor).sort(), ["D-M-36", "plan/15a.org", "src/priced_frame.rs:88-104"].sort());
});

// ---------------------------------------------------------------------------
// Events and the anchor rule
// ---------------------------------------------------------------------------

function message(id: string, type: Message["type"], evidence: string[], overrides: Partial<Message> = {}): Message {
  return makeMessage({ id, type, evidence, summary: `summary of ${id}`, ...overrides });
}

/** Fold the runtime's round-time pass: an entry for every message. */
function openAll(messages: Message[], entries: Entry[] = []): Entry[] {
  let es = entries;
  for (const ev of planEntryEvents(messages, es)) {
    const r = applyEntryEvent(es, ev, messages);
    if (r.ok) es = r.entries;
  }
  return es;
}

test("a link without a shared anchor is refused and logged; the message opens its own entry", () => {
  const entry: Entry = {
    id: "E-1",
    phaseId: "p1",
    title: "no priced-frame counter",
    type: "blocker",
    state: "open",
    anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] },
    links: [],
  };
  const far = message("F-9", "finding", ["src/z.rs:1 a different problem"]);
  const refused = applyEntryEvent([entry], { type: "MESSAGE_LINKED", messageId: "F-9", entryId: "E-1", anchor: entry.anchor, reason: "curator" }, [far]);
  assert.ok(refused.ok);
  assert.ok(refused.ok && refused.refused);
  assert.match(refused.ok && refused.refused ? refused.refused.reason : "", /share no anchor/);
  // The runtime's round-time pass opens the message its own entry.
  const planned = planEntryEvents([far], [entry]);
  assert.equal(planned.length, 1);
  assert.equal(planned[0].type, "ENTRY_OPENED");
  const opened = openAll([far], [entry]);
  const projected = projectEntries({ messages: [far], entries: opened });
  // Only the message's own entry is a topic; the empty pre-existing entry
  // renders nothing.
  assert.equal(projected.views.length, 1);
  assert.equal(projected.views[0].entry.links[0].messageId, "F-9");
  assert.equal(projected.accounting.unaccounted, 0);
});

test("the runtime links the same topic raised as a blocker, a trade-off and a finding into one entry", () => {
  // 15a's B-1 = T-14 = F-1: the same point under three types, one anchor.
  const anchor = "src/priced_frame.rs:88-104 the counter is never priced";
  const b = message("B-1", "blocker", [anchor], { raisedAsBlocker: true, title: "no priced-frame counter" });
  const t = message("T-14", "tradeoff", [anchor], { title: "no priced-frame counter" });
  const f = message("F-1", "finding", [anchor], { title: "no priced-frame counter" });
  const entries = openAll([b, t, f]);
  const projected = projectEntries({ messages: [b, t, f], entries });
  assert.equal(projected.views.length, 1);
  assert.equal(projected.views[0].type, "blocker");
  assert.deepEqual(projected.views[0].messages.map((m) => m.id).sort(), ["B-1", "F-1", "T-14"]);
  assert.equal(projected.accounting.unaccounted, 0);
});

test("entries are persisted as events, so an owner's split names an entry reduce() can find (finding M-1)", () => {
  const m = message("F-1", "finding", ["src/a.rs:10-20 a bug"]);
  // The conductor's round-time pass produces the events and reduces them.
  const planned = planEntryEvents([m], []);
  assert.equal(planned.length, 1);
  const opened = applyEntryEvent([], planned[0], [m]);
  assert.ok(opened.ok);
  const entryId = opened.ok ? opened.entries[0].id : "";
  // A second message sharing the anchor is linked by the same pass.
  const m2 = message("T-2", "tradeoff", ["src/a.rs:15-25 the fix"]);
  const planned2 = planEntryEvents([m, m2], opened.ok ? opened.entries : []);
  assert.equal(planned2.length, 1);
  assert.equal(planned2[0].type, "MESSAGE_LINKED");
  const linked = applyEntryEvent(opened.ok ? opened.entries : [], planned2[0], [m, m2]);
  assert.ok(linked.ok);
  // The persisted entry id is what an owner command names: a split resolves.
  const split = applyEntryEvent(linked.ok ? linked.entries : [], { type: "ENTRY_SPLIT", entryId, messageId: "T-2", by: "owner" }, [m, m2]);
  assert.ok(split.ok, !split.ok ? split.reason : "");
  assert.equal(split.ok ? split.entries.length : 0, 2);
});

test("a link with overlapping line ranges or the same decision id is accepted", () => {
  const byLines: Entry = {
    id: "E-1",
    phaseId: "p1",
    title: "counter",
    type: "tradeoff",
    state: "open",
    anchor: { kind: "file", path: "src/a.rs", lines: [10, 30] },
    links: [],
  };
  const overlapping = message("T-2", "tradeoff", ["src/a.rs:25-40 also this"]);
  const okLines = applyEntryEvent([byLines], { type: "MESSAGE_LINKED", messageId: "T-2", entryId: "E-1", anchor: { kind: "file", path: "src/a.rs", lines: [10, 30] }, reason: "same lines" }, [overlapping]);
  assert.ok(okLines.ok && !okLines.refused);
  assert.equal(okLines.ok ? okLines.entries[0].links.length : 0, 1);

  const byDecision: Entry = { ...byLines, id: "E-2", anchor: { kind: "decision", id: "D-1" }, links: [] };
  const sameDecision = message("T-3", "tradeoff", ["src/b.rs:1 unrelated"], { sourceRecordId: "D-1" });
  const okDecision = applyEntryEvent([byDecision], { type: "MESSAGE_LINKED", messageId: "T-3", entryId: "E-2", anchor: { kind: "decision", id: "D-1" }, reason: "same decision" }, [sameDecision]);
  assert.ok(okDecision.ok && !okDecision.refused);
});

test("the type of an entry is the highest type of its linked messages", () => {
  const tradeoff = message("T-1", "tradeoff", ["src/a.rs:1"]);
  const finding = message("F-1", "finding", ["src/a.rs:1"]);
  const blocker = message("B-1", "blocker", ["src/a.rs:1"], { raisedAsBlocker: true });
  assert.equal(entryTypeOf([tradeoff]), "tradeoff");
  assert.equal(entryTypeOf([tradeoff, finding]), "finding");
  assert.equal(entryTypeOf([tradeoff, finding, blocker]), "blocker");
});

// ---------------------------------------------------------------------------
// Conservation and determinism
// ---------------------------------------------------------------------------

function seededRandom(seed: number): () => number {
  let s = seed >>> 0;
  return () => {
    s = (s * 1664525 + 1013904223) >>> 0;
    return s / 0x100000000;
  };
}

test("conservation: for random logs every message is linked to one entry or dropped; accounting is 0 unaccounted", () => {
  const rand = seededRandom(1989);
  for (let iter = 0; iter < 50; iter++) {
    const messages: Message[] = [];
    const count = 5 + Math.floor(rand() * 26);
    for (let i = 0; i < count; i++) {
      const type = rand() < 0.4 ? "tradeoff" : rand() < 0.7 ? "finding" : "blocker";
      const file = `src/f${Math.floor(rand() * 3)}.rs`;
      const start = 1 + Math.floor(rand() * 40);
      const m = message(`${type === "tradeoff" ? "T" : type === "finding" ? "F" : "B"}-${i + 1}`, type, [`${file}:${start} problem ${i}`]);
      if (rand() < 0.1) {
        m.state = "dropped";
        m.settlement = {
          state: "dropped",
          settledBy: "evaluator",
          reason: "trivial",
          candidateSha: "C1",
          contractVersion: m.boundContractVersion,
          messageVersion: 1,
          contentHash: m.contentHash,
        };
      }
      messages.push(m);
    }
    const projected = projectEntries({ messages, entries: openAll(messages) });
    assert.equal(projected.accounting.unaccounted, 0, `iteration ${iter}: ${accountingLine(projected.accounting)}`);
    // Every message is in exactly one bucket.
    const live = messages.filter((m) => m.state !== "dropped" && m.state !== "merged" && m.state !== "resolved" && m.state !== "superseded").length;
    assert.equal(projected.accounting.linked, live);
    assert.equal(projected.accounting.dropped, messages.length - live);
    const line = accountingLine(projected.accounting);
    assert.equal(line, `${messages.length} raw → ${projected.accounting.entries} entries · ${live} linked · ${projected.accounting.dropped} dropped · 0 unaccounted · unexposed ${projected.accounting.unexposed}`);
  }
});

test("rendering the same state twice is byte-identical (determinism)", () => {
  const phase = atlas15aRound1();
  const a = renderEntryReview({ messages: phase.messages, entries: phase.entries, newestCandidateSha: "C1", phaseId: "p1", readableId: "15a-01", dirId: "1989d1d0" });
  const b = renderEntryReview({ messages: phase.messages, entries: phase.entries, newestCandidateSha: "C1", phaseId: "p1", readableId: "15a-01", dirId: "1989d1d0" });
  assert.equal(a, b);
});

// ---------------------------------------------------------------------------
// Latest state
// ---------------------------------------------------------------------------

test("an entry resolved in a later round leaves the live view; a live one with a gone anchor is `stale anchor`", () => {
  const round1 = message("F-1", "finding", ["src/a.rs:10-20 a bug"]);
  const round2fix = message("T-40", "tradeoff", ["src/a.rs:10-20 the fix"], { boundCandidateSha: "C2" });
  const resolved = { ...round1, state: "resolved" as const, settlement: { state: "resolved" as const, settledBy: "evaluator" as const, reason: "addressed by C2", candidateSha: "C2", contractVersion: round1.boundContractVersion, messageVersion: 1, contentHash: round1.contentHash } };
  const entries: Entry[] = [
    { id: "E-1", phaseId: "p1", title: "a bug", type: "finding", state: "resolved", stateSha: "C2", anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] }, links: [{ messageId: "F-1", anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] }, reason: "opened" }, { messageId: "T-40", anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] }, reason: "fix" }] },
    { id: "E-2", phaseId: "p1", title: "an open bug", type: "finding", state: "open", anchor: { kind: "file", path: "src/gone.rs", lines: [1, 5] }, links: [{ messageId: "F-2", anchor: { kind: "file", path: "src/gone.rs", lines: [1, 5] }, reason: "opened" }] },
  ];
  const f2 = message("F-2", "finding", ["src/gone.rs:1-5 still open"]);
  const projected = projectEntries({
    messages: [resolved, round2fix, f2],
    entries,
    newestCandidateSha: "C2",
    anchorResolves: (a) => a.kind === "file" && a.path !== "src/gone.rs",
  });
  const e1 = projected.views.find((v) => v.entry.id === "E-1")!;
  assert.equal(e1.state, "resolved");
  assert.equal(e1.live, false);
  const view = renderEntryReview({ messages: [resolved, round2fix, f2], entries, newestCandidateSha: "C2", phaseId: "p1", anchorResolves: (a) => a.kind === "file" && a.path !== "src/gone.rs" });
  assert.doesNotMatch(view, /E-1 /);
  const e2 = projected.views.find((v) => v.entry.id === "E-2")!;
  assert.equal(e2.live, true);
  assert.equal(e2.staleAnchor, true);
  assert.match(view, /E-2 .*stale anchor/);
});

// ---------------------------------------------------------------------------
// Similarity, hints and the owner's merge/split
// ---------------------------------------------------------------------------

test("near-duplicates without a shared anchor stay separate with a ≈ hint and only the owner merges them", () => {
  const a = message("T-2", "tradeoff", ["src/one.rs:1 tolerances raised on the fill path"]);
  const b = message("T-41", "tradeoff", ["src/two.rs:9 tolerances raised on the fill path again"]);
  assert.ok(titleSimilarity(a.title, b.title) >= ENTRY_SIMILARITY_THRESHOLD);
  const projected = projectEntries({ messages: [a, b], entries: openAll([a, b]) });
  assert.equal(projected.views.length, 2);
  const hints = projected.views.map((v) => v.hint).filter(Boolean);
  assert.equal(hints.length, 2);
  assert.ok(hints.every((h) => h!.startsWith("E-")));
  // The hint is only a tag until the owner merges.
  const view = renderEntryReview({ messages: [a, b], entries: openAll([a, b]), phaseId: "p1", newestCandidateSha: "C1" });
  assert.match(view, /≈ E-\d/);
  const merged = applyEntryEvent(projected.views.map((v) => v.entry), { type: "ENTRY_MERGED_BY_OWNER", entryId: projected.views[0].entry.id, intoEntryId: projected.views[1].entry.id, by: "owner" });
  assert.ok(merged.ok);
  // The merged entry keeps BOTH messages even though they share no anchor
  // (finding M-14): the owner's `m` is the exception to the anchor rule.
  const afterMerge = merged.ok ? merged.entries : [];
  const target = afterMerge.find((e) => e.id === projected.views[1].entry.id)!;
  assert.deepEqual(target.links.map((l) => l.messageId).sort(), ["T-2", "T-41"]);
  const mergedProjection = projectEntries({ messages: [a, b], entries: afterMerge });
  assert.equal(mergedProjection.views.filter((v) => v.live).length, 1);
  assert.deepEqual(mergedProjection.views[0].messages.map((m) => m.id).sort(), ["T-2", "T-41"]);
  assert.equal(mergedProjection.accounting.unaccounted, 0);
});

test("the curator's tool rejects any operation other than link, open and retitle", () => {
  assert.ok(validateCuratorProposal({ op: "link", messageId: "T-1", entryId: "E-1" }).ok);
  assert.ok(validateCuratorProposal({ op: "open", title: "a topic" }).ok);
  assert.ok(validateCuratorProposal({ op: "retitle", entryId: "E-1", title: "a better title" }).ok);
  for (const op of ["drop", "resolve", "state", "type", "merge", "split", ""]) {
    const result = validateCuratorProposal({ op });
    assert.ok(!result.ok, `op '${op}' must be refused`);
    assert.match(!result.ok ? result.reason : "", /may only link, open, retitle/);
  }
  // A curator proposal that is allowed still cannot name a state.
  const built = curatorEvent({ op: "state", entryId: "E-1", title: "x" }, "p1");
  assert.ok(!built.ok);
});

test("ENTRY_SPLIT pulls one message into its own entry", () => {
  const a = message("B-1", "blocker", ["src/a.rs:10-20 one"]);
  const b = message("F-1", "finding", ["src/a.rs:10-20 two"]);
  const entries: Entry[] = [
    { id: "E-1", phaseId: "p1", title: "one topic", type: "blocker", state: "open", anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] }, links: [{ messageId: "B-1", anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] }, reason: "opened" }, { messageId: "F-1", anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] }, reason: "linked" }] },
  ];
  const split = applyEntryEvent(entries, { type: "ENTRY_SPLIT", entryId: "E-1", messageId: "F-1", newEntryId: "E-2" }, [a, b]);
  assert.ok(split.ok);
  if (split.ok) {
    const e1 = split.entries.find((e) => e.id === "E-1")!;
    const e2 = split.entries.find((e) => e.id === "E-2")!;
    assert.deepEqual(e1.links.map((l) => l.messageId), ["B-1"]);
    assert.deepEqual(e2.links.map((l) => l.messageId), ["F-1"]);
    assert.equal(e2.splitFrom, "E-1");
  }
});

// ---------------------------------------------------------------------------
// The 15a regression fixture
// ---------------------------------------------------------------------------

interface A15Fixture {
  messages: Message[];
  entries: Entry[];
}

/** 15a's first round as 31 raw messages. Three cross-type duplicates (B-1 =
 * T-14 = F-1, T-7 = F-3, T-15 = F-4) are linked into one entry each; later
 * re-raises under new ids (T-40 for B-1, T-27 for F-2) link too; two dropped
 * messages carry their reasons; two near-duplicates without a shared anchor
 * stay separate with a hint. */
function atlas15aRound1(): A15Fixture {
  const frame = "src/priced_frame.rs:88-104 the priced-frame counter is never incremented";
  const closed = "src/priced_frame.rs:210-224 a priced CLOSED frame is dropped";
  const rth = "src/fixtures/rth.rs:1-40 the weekday RTH fixture is missing";
  const tol = "src/fills.rs:12-18 tolerances are raised on the fill path";
  const tol2 = "src/fills.rs:90-96 tolerances are raised on the fill path";
  const messages: Message[] = [
    message("B-1", "blocker", [frame], { raisedAsBlocker: true, raisedBy: "M", title: "no priced-frame counter" }),
    message("T-14", "tradeoff", [frame], { raisedBy: "worker", title: "no priced-frame counter" }),
    message("F-1", "finding", [frame], { raisedBy: "A", title: "no priced-frame counter" }),
    message("T-7", "tradeoff", [closed], { raisedBy: "worker", title: "priced CLOSED frames dropped" }),
    message("F-3", "finding", [closed], { raisedBy: "A", title: "priced CLOSED frames are dropped" }),
    message("T-15", "tradeoff", [rth], { raisedBy: "worker", title: "no weekday RTH fixture" }),
    message("F-4", "finding", [rth], { raisedBy: "B", title: "no weekday RTH fixture" }),
    message("T-40", "tradeoff", [frame], { raisedBy: "worker", title: "the counter the fix adds" }),
    message("F-2", "finding", ["src/util.rs:3 a slow path returns NaN"], { raisedBy: "M", title: "a slow path returns NaN" }),
    message("T-27", "tradeoff", ["src/util.rs:3 guard the slow path"], { raisedBy: "worker", title: "guarding the slow path" }),
    message("T-2", "tradeoff", [tol], { raisedBy: "worker", title: "tolerances raised on the fill path" }),
    message("T-41", "tradeoff", [tol2], { raisedBy: "worker", title: "tolerances raised on the fill path" }),
    message("T-8", "tradeoff", ["src/a.rs:1 batch per tick"], { raisedBy: "worker", title: "batch per tick" }),
    message("T-10", "tradeoff", ["src/a.rs:1 batch per tick"], { raisedBy: "worker", title: "batch per tick" }),
    message("T-20", "tradeoff", ["src/a.rs:1 batch per tick"], { raisedBy: "worker", title: "batch per tick" }),
    message("T-25", "tradeoff", ["src/a.rs:1 batch per tick"], { raisedBy: "worker", title: "batch per tick" }),
    message("T-1", "tradeoff", ["src/b.rs:1 keep the layout"], { raisedBy: "worker", title: "keep the layout" }),
    message("T-9", "tradeoff", ["src/b.rs:1 keep the layout"], { raisedBy: "worker", title: "keep the layout" }),
    message("T-18", "tradeoff", ["src/b.rs:1 keep the layout"], { raisedBy: "worker", title: "keep the layout" }),
    message("T-4", "tradeoff", ["src/c.rs:1 rename it"], { raisedBy: "worker", title: "rename it" }),
    message("T-12", "tradeoff", ["src/c.rs:1 rename it"], { raisedBy: "worker", title: "rename it" }),
    message("T-17", "tradeoff", ["src/c.rs:1 rename it"], { raisedBy: "worker", title: "rename it" }),
    message("T-5", "tradeoff", ["src/d.rs:1 one lock"], { raisedBy: "worker", title: "one lock per request" }),
    message("T-11", "tradeoff", ["src/d.rs:1 one lock"], { raisedBy: "worker", title: "one lock per request" }),
    // Five findings of one round, each its own topic (distinct files).
    message("F-5", "finding", ["src/e.rs:1 a missing test"], { raisedBy: "M", title: "a missing test" }),
    message("F-6", "finding", ["src/f.rs:1 a wrong error"], { raisedBy: "A", title: "a wrong error" }),
    message("F-7", "finding", ["src/g.rs:1 an off-by-one"], { raisedBy: "B", title: "an off-by-one" }),
    message("F-8", "finding", ["src/h.rs:1 a leak"], { raisedBy: "M", title: "a leak" }),
    message("F-9", "finding", ["src/i.rs:1 a stale cache"], { raisedBy: "A", title: "a stale cache" }),
  ];
  // Two dropped with a reason.
  const dropped = (id: string, title: string, reason: string): Message => {
    const m = message(id, "tradeoff", [`src/j.rs:${id === "T-3" ? 1 : 2} noise`], { title });
    m.state = "dropped";
    m.settlement = { state: "dropped", settledBy: "evaluator", reason, candidateSha: "C1", contractVersion: m.boundContractVersion, messageVersion: 1, contentHash: m.contentHash };
    return m;
  };
  messages.push(dropped("T-3", "a trivial rename", "trivial, not reviewable"), dropped("T-6", "a whitespace change", "trivial, not reviewable"));
  // The entries are the RUNTIME's own linking pass over these messages
  // (`planEntryEvents`), not a hand-built list: this fixture shows that the
  // shared-anchor rule collapses the cross-type duplicates (finding M-17).
  return { messages, entries: openAll(messages) };
}

test("15a's first round renders as one entry per topic, three sections only, with 0 unaccounted", () => {
  const { messages, entries } = atlas15aRound1();
  assert.equal(messages.length, 31);
  const review = renderEntryReview({ messages, entries, newestCandidateSha: "C1", phaseId: "p1", readableId: "15a-01", dirId: "1989d1d0" });
  assertGolden("review.org", review);
  // Only the three sections, in order.
  const sectionHeads = [...review.matchAll(/^\* (.+)$/gm)].map((m) => m[1]);
  assert.deepEqual(sectionHeads, ["Blockers", "Findings", "Trade-offs"]);
  assert.ok(review.indexOf("* Blockers") < review.indexOf("* Findings"));
  assert.ok(review.indexOf("* Findings") < review.indexOf("* Trade-offs"));
  // One entry per topic: the blocker topic shows once, with its trade-off and
  // finding under it. The two near-duplicates without a shared anchor stay
  // separate and carry the ≈ hint.
  const projected0 = projectEntries({ messages, entries });
  assert.equal([...review.matchAll(/^\*\* /gm)].length, projected0.views.filter((v) => v.live).length);
  assert.equal(projected0.views.length, 15);
  assert.equal(projected0.views.filter((v) => v.hint).length, 2);
  // The blocker topic appears as exactly one entry heading.
  assert.match(review, /^\*\* E-1 no priced-frame counter/m);
  assert.equal([...review.matchAll(/^\*\* E-1 /gm)].length, 1);
  // Accounting reconciles.
  const line = review.trim().split("\n").pop()!;
  assert.match(line, /^31 raw → 15 entries · 29 linked · 2 dropped · 0 unaccounted · unexposed \d+$/);
  const projected = projectEntries({ messages, entries });
  assert.equal(projected.accounting.unaccounted, 0);
  const lint = runReviewLint({ projected, newestCandidateSha: "C1", messages });
  assert.ok(lint.ok, JSON.stringify(lint.violations));
});

test("the entry view is a pure projection: the curator linking does not change the metrics over the raw messages", () => {
  const { messages } = atlas15aRound1();
  const entries = openAll(messages);
  // The balance metrics read phase.messages, which linking never edits, so
  // every balance number is identical before and after the curator runs.
  const timeline = { phases: [{ phase: "READY", at: "2026-09-28T00:00:00.000Z" }] };
  const phaseBefore = basePhase({ runId: "r1", phaseId: "p1", messages });
  const phaseAfter = basePhase({ runId: "r1", phaseId: "p1", messages, entries });
  const mBefore = computeMetrics(phaseBefore, timeline, []);
  const mAfter = computeMetrics(phaseAfter, timeline, []);
  assert.deepEqual(
    { messages: mBefore.messages, mergeRate: mBefore.mergeRate, dropRate: mBefore.dropRate, unexposed: mBefore.unexposedTradeoffs },
    { messages: mAfter.messages, mergeRate: mAfter.mergeRate, dropRate: mAfter.dropRate, unexposed: mAfter.unexposedTradeoffs },
  );
  // Only the cleanness metric moves: linking groups messages into topics.
  assert.equal(mBefore.cleanness.liveEntries, 0);
  assert.equal(mAfter.cleanness.liveEntries, projectEntries({ messages, entries }).views.filter((v) => v.live).length);
});

// ---------------------------------------------------------------------------
// Program view
// ---------------------------------------------------------------------------

test("programs/<id>/views/review.org lists entries of every phase with phase tags and cross-phase links only through shared anchors", () => {
  const a = message("T-1", "tradeoff", ["src/shared.rs:10-20 batch per tick"], { phaseId: "p1", title: "batch per tick" });
  const b = message("T-2", "tradeoff", ["src/shared.rs:15-25 batch per tick"], { phaseId: "p2", title: "batch per tick" });
  const c = message("T-3", "tradeoff", ["src/other.rs:1 a different point"], { phaseId: "p2", title: "a different point" });
  const view = renderProgramEntryReview({
    program: {
      id: "prog",
      phases: [
        { phaseId: "p1", readableId: "prog-01", messages: [a], entries: openAll([a]) },
        { phaseId: "p2", readableId: "prog-02", messages: [b, c], entries: openAll([b, c]) },
      ],
    },
    newestCandidateSha: "C1",
  });
  // The cross-phase entry (same anchor) is shown once with both phase tags.
  assert.equal([...view.matchAll(/^\*\* E-\d+ batch per tick/gm)].length, 1);
  assert.match(view, /prog-01·prog-02/);
  // The different anchor stays its own entry, tagged to its phase.
  assert.match(view, /^\*\* E-\d+ a different point.*prog-02/m);
  const lint = runReviewLint({ projected: projectEntries({ messages: [a] }), newestCandidateSha: "C1" });
  assert.ok(lint.ok);
  // Determinism: the same program state renders byte-identical bytes.
  const opts = {
    program: {
      id: "prog",
      phases: [
        { phaseId: "p1", readableId: "prog-01", messages: [a], entries: openAll([a]) },
        { phaseId: "p2", readableId: "prog-02", messages: [b, c], entries: openAll([b, c]) },
      ],
    },
    newestCandidateSha: "C1",
  };
  assert.equal(renderProgramEntryReview(opts), renderProgramEntryReview(opts));
});

// ---------------------------------------------------------------------------
// Lint
// ---------------------------------------------------------------------------

function lintFor(entries: Entry[], messages: Message[], newestCandidateSha = "C1") {
  const projected = projectEntries({ messages, entries, newestCandidateSha });
  return runReviewLint({ projected, newestCandidateSha, messages });
}

test("one-anchor: two live entries sharing an anchor fail the lint with a red first line, and the log records REVIEW_LINT_FAILED", () => {
  const a = message("T-1", "tradeoff", ["src/a.rs:10-20 x"]);
  const b = message("T-2", "tradeoff", ["src/a.rs:15-25 y"]);
  const entries: Entry[] = [
    { id: "E-1", phaseId: "p1", title: "x", type: "tradeoff", state: "open", anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] }, links: [{ messageId: "T-1", anchor: { kind: "file", path: "src/a.rs", lines: [10, 20] }, reason: "opened" }] },
    { id: "E-2", phaseId: "p1", title: "y", type: "tradeoff", state: "open", anchor: { kind: "file", path: "src/a.rs", lines: [15, 25] }, links: [{ messageId: "T-2", anchor: { kind: "file", path: "src/a.rs", lines: [15, 25] }, reason: "opened" }] },
  ];
  const lint = lintFor(entries, [a, b]);
  assert.ok(!lint.ok);
  assert.ok(lint.violations.some((v) => v.rule === "one-anchor"));
  const events = reviewLintFailedEvents(lint, "2026-09-28T00:00:00Z");
  assert.ok(events.length > 0 && events[0].type === "REVIEW_LINT_FAILED");
  const view = renderEntryReview({ messages: [a, b], entries, phaseId: "p1", newestCandidateSha: "C1", lintError: lint.firstLine });
  assert.match(view.split("\n")[0], /^review lint: 2 entries share src\/a\.rs:10-20/);
});

test("live-only: a merged/dropped/resolved message is never rendered as its own entry", () => {
  const dropped = message("T-1", "tradeoff", ["src/a.rs:1 x"]);
  dropped.state = "dropped";
  dropped.settlement = { state: "dropped", settledBy: "evaluator", reason: "trivial", candidateSha: "C1", contractVersion: dropped.boundContractVersion, messageVersion: 1, contentHash: dropped.contentHash };
  // A crafted projection that (wrongly) renders the dropped message as a live
  // entry of its own; the lint must catch it without touching the entries.
  const entry: Entry = { id: "E-1", phaseId: "p1", title: "x", type: "tradeoff", state: "open", anchor: { kind: "file", path: "src/a.rs", lines: [1, 1] }, links: [{ messageId: "T-1", anchor: { kind: "file", path: "src/a.rs", lines: [1, 1] }, reason: "opened" }] };
  const projected = { views: [{ entry, type: "tradeoff" as const, state: "open" as const, live: true, messages: [dropped], anchor: entry.anchor, staleAnchor: false, raisedBy: ["worker"], staleState: false }], accounting: { raw: 1, entries: 1, linked: 1, dropped: 0, merged: 0, resolved: 0, unaccounted: 0, unexposed: 0 }, refusedLinks: [] };
  const lint = runReviewLint({ projected, newestCandidateSha: "C1", messages: [dropped] });
  assert.ok(lint.violations.some((v) => v.rule === "live-only"));
});

test("evidence: an entry whose message has neither evidence nor a vote fails the lint", () => {
  const bare = message("F-1", "finding", [], { sourceRecordId: "D-1" });
  const entries: Entry[] = [
    { id: "E-1", phaseId: "p1", title: "x", type: "finding", state: "open", anchor: { kind: "decision", id: "D-1" }, links: [{ messageId: "F-1", anchor: { kind: "decision", id: "D-1" }, reason: "opened" }] },
  ];
  const lint = lintFor(entries, [bare]);
  assert.ok(lint.violations.some((v) => v.rule === "evidence"));
  // A finding with words but no citation and no vote is not validation either.
  const words = message("F-2", "finding", ["just some words here"], { sourceRecordId: "D-1" });
  const wordsEntry: Entry[] = [{ ...entries[0], id: "E-2", links: [{ messageId: "F-2", anchor: { kind: "decision", id: "D-1" }, reason: "opened" }] }];
  assert.ok(lintFor(wordsEntry, [words]).violations.some((v) => v.rule === "evidence"));
});

test("title: an empty title or one cut mid-word fails the lint", () => {
  const m = message("T-1", "tradeoff", ["src/a.rs:1 x"]);
  const empty: Entry[] = [{ id: "E-1", phaseId: "p1", title: "", type: "tradeoff", state: "open", anchor: { kind: "file", path: "src/a.rs", lines: [1, 1] }, links: [{ messageId: "T-1", anchor: { kind: "file", path: "src/a.rs", lines: [1, 1] }, reason: "opened" }] }];
  assert.ok(lintFor(empty, [m]).violations.some((v) => v.rule === "title"));
  const cut = "x".repeat(80);
  const cutEntries: Entry[] = [{ ...empty[0], title: cut }];
  assert.ok(lintFor(cutEntries, [m]).violations.some((v) => v.rule === "title"));
});

test("newest-candidate: a state computed against an older candidate fails the lint", () => {
  const m = message("T-1", "tradeoff", ["src/a.rs:1 x"]);
  const entries: Entry[] = [
    { id: "E-1", phaseId: "p1", title: "x", type: "tradeoff", state: "open", anchor: { kind: "file", path: "src/a.rs", lines: [1, 1] }, links: [{ messageId: "T-1", anchor: { kind: "file", path: "src/a.rs", lines: [1, 1] }, reason: "opened" }] },
  ];
  // The entry's message is bound to C1 while the view reflects C2.
  const lint = lintFor(entries, [m], "C2");
  assert.ok(lint.violations.some((v) => v.rule === "newest-candidate"));
});

test("accounting: an unreconciled footer fails the lint", () => {
  const m = message("T-1", "tradeoff", ["src/a.rs:1 x"]);
  const projected = projectEntries({ messages: [m] });
  projected.accounting.unaccounted = 3;
  const lint = runReviewLint({ projected, newestCandidateSha: "C1", messages: [m] });
  assert.ok(lint.violations.some((v) => v.rule === "accounting"));
});

test("the lint never alters the entries it reads", () => {
  const { messages, entries } = atlas15aRound1();
  const before = JSON.stringify(entries);
  runReviewLint({ projected: projectEntries({ messages, entries }), newestCandidateSha: "C1", messages });
  assert.equal(JSON.stringify(entries), before);
});

test("an entry file carries the full history of its linked messages", () => {
  const { messages, entries } = atlas15aRound1();
  const projected = projectEntries({ messages, entries });
  const e1 = projected.views.find((v) => v.entry.id === "E-1")!;
  const file = renderEntryFile(e1);
  assert.match(file, /B-1 no priced-frame counter/);
  assert.match(file, /T-14/);
  assert.match(file, /F-1/);
});
