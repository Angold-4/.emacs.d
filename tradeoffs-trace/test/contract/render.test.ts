// Plan 03b / 05c acceptance: the runtime renders `views/review.org` and one
// `views/messages/<id>.org` per message from the message fixtures, and the
// bytes match golden files.
//
// Plan 05c: only messages the evaluator published (and their later states) are
// entries; raw messages are one `N raw, awaiting evaluation` line per section,
// dropped ones only a count, and merged ones live in their target's own file.
// `* Blockers` holds only messages raised through a reviewer's `blockers` list
// (with their panel's outcome); a blocking finding stays under `* Findings`,
// marked `blocking`. The header names the readable id and the directory id,
// never the internal runId (which lives only in the property drawers).

import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { projectReview, renderMessageFile, reviewSummary } from "../../src/render.ts";
import { renderStatusView, type StatusViewInput } from "../../src/render.ts";
import { contentHashOf } from "../../src/core/messages.ts";
import type { Ballot, Decision, Finding, Message, MessageSettlement, PhaseState } from "../../src/core/types.ts";
import { notAcceptedReasons } from "../../src/core/verdict.ts";
import { approvingReview, basePhase, CV, makeMessage } from "../unit/helpers.ts";

const GOLDEN = fileURLToPath(new URL("../fixtures/render", import.meta.url));

/** The fixture phase also carries the run's readable and directory ids (plan
 * 05c), which the review header shows. */
type ReviewFixturePhase = PhaseState & { readableId?: string; dirId?: string };

function decision(overrides: Partial<Decision> & { id: string; choice: string }): Decision {
  return {
    version: 1,
    phaseId: "p1",
    source: "worker",
    class: "delegated",
    whyItMatters: "it keeps the change reviewable",
    alternatives: [{ option: "the other way", consequence: "a noisier diff" }],
    recommendation: { choice: "this way", reason: "smallest reviewable change" },
    boundCandidateSha: "C1",
    boundContractVersion: CV(),
    ...overrides,
  };
}

function finding(overrides: Partial<Finding> & { id: string; evidence: string }): Finding {
  return {
    version: 1,
    phaseId: "p1",
    kind: "defect",
    severity: "advisory",
    raisedBy: "M",
    status: "open",
    boundCandidateSha: "C1",
    ...overrides,
  };
}

function settleMessage(
  message: Message,
  state: MessageSettlement["state"],
  reason: string,
  settledBy: MessageSettlement["settledBy"] = "evaluator",
): Message {
  return {
    ...message,
    state,
    settlement: {
      state,
      settledBy,
      reason,
      candidateSha: message.boundCandidateSha,
      contractVersion: message.boundContractVersion,
      messageVersion: message.messageVersion,
      contentHash: message.contentHash,
    },
  };
}

/** One blocker raised through a reviewer's `blockers` list (with an escalated
 * panel), one ordinary blocking finding, one advisory finding and one raw
 * finding; three trade-offs at each importance, one accepted, one refused. */
function fixturePhase(): ReviewFixturePhase {
  const decisions = [
    decision({ id: "D-1", choice: "Batch cancels per tick", class: "reserved" }),
    decision({ id: "D-2", choice: "Rename the helper", class: "detail" }),
    decision({ id: "D-3", choice: "Keep the existing file layout", class: "delegated" }),
  ];
  const findings = [
    finding({ id: "F-p1-M-0", severity: "blocking", evidence: "src/cancel.ts:10 the cancel path can deadlock" }),
    finding({ id: "F-p1-M-1", severity: "blocking", evidence: "src/loop.ts:12 does not break on empty input" }),
    finding({ id: "F-p1-M-2", evidence: "src/util.ts:3 a slow path. It returns NaN to callers." }),
    finding({ id: "F-p1-A-3", raisedBy: "A", evidence: "src/util.ts:9 an unreachable branch" }),
  ];
  const ballots: Ballot[] = [
    {
      reviewer: "M",
      decisionId: "D-1",
      vote: "approve",
      rationale: "within the latency budget",
      evidence: ["src/cancel.ts:42"],
      boundCandidateSha: "C1",
      boundContractVersion: CV(),
      boundRecordVersion: 1,
    },
  ];
  // A blocker ONLY through the `blockers` list: red, stop the work, with the
  // panel's escalated outcome.
  const blocker = makeMessage({
    id: "B-1",
    type: "blocker",
    raisedAsBlocker: true,
    title: "the loop does not terminate on empty input",
    summary: "raised by M against C1",
    context: "an empty input leaves the cursor where it started",
    evidence: ["src/cancel.ts:10 the cancel path can deadlock"],
    planRef: "plan/03b.org",
    state: "published",
    sourceRecordId: "F-p1-M-0",
  });
  // The reviewer's ordinary `blocking` finding: a finding message, never a
  // blocker, listed under Findings and marked blocking.
  const blocking = makeMessage({
    id: "F-1",
    type: "finding",
    title: "the loop does not terminate on empty input",
    summary: "raised by M against C1",
    context: "src/loop.ts:12 does not break on empty input",
    evidence: ["src/loop.ts:12 does not break on empty input"],
    planRef: "plan/03b.org",
    state: "published",
    sourceRecordId: "F-p1-M-1",
  });
  const accepted = settleMessage(
    makeMessage({
      id: "T-1",
      title: "Batch cancels per tick",
      summary: "fewer lock acquisitions under load",
      context: "the cancel path took one lock per request",
      evidence: ["src/cancel.ts:42"],
      planRef: "plan/03b.org",
      state: "published",
      sourceRecordId: "D-1",
    }),
    "accepted",
    "the goal needs the smaller lock window",
    "owner",
  );
  const minorTradeoff = makeMessage({
    id: "T-2",
    title: "Rename the helper",
    summary: "the name hid the retry behaviour",
    context: "callers had to read the body to know it retried",
    evidence: ["src/util.ts:1"],
    planRef: "plan/03b.org",
    state: "published",
    sourceRecordId: "D-2",
  });
  const refused = settleMessage(
    makeMessage({
      id: "T-3",
      title: "Keep the existing file layout",
      summary: "a layout change would make the diff harder to review",
      context: "the reviewer asked for the smallest reviewable diff",
      evidence: ["src/layout.ts:1"],
      planRef: "plan/03b.org",
      state: "published",
      sourceRecordId: "D-3",
      followUp: true,
    }),
    "refused",
    "worth revisiting after the run",
    "owner",
  );
  const advisory = makeMessage({
    id: "F-2",
    type: "finding",
    title: "a slow path returns NaN to callers",
    summary: "raised by M against C1",
    context: "accepted, not fixed",
    evidence: ["src/util.ts:3 a slow path. It returns NaN to callers."],
    planRef: "plan/03b.org",
    state: "published",
    sourceRecordId: "F-p1-M-2",
  });
  const raw = makeMessage({
    id: "F-3",
    type: "finding",
    title: "an unreachable branch",
    summary: "raised by A",
    context: "not frozen yet",
    evidence: ["src/util.ts:9 an unreachable branch"],
    planRef: "plan/03b.org",
    state: "raw",
    sourceRecordId: "F-p1-A-3",
  });
  return basePhase({
    runId: "07ca06e2",
    phaseId: "p1",
    readableId: "cebd7fcb-01",
    dirId: "33c41174",
    decisions,
    findings,
    ballots,
    messages: [blocker, blocking, accepted, minorTradeoff, refused, advisory, raw],
    panel: { blockers: { "B-1": { decided: { outcome: "escalate", reason: "the cancel path can deadlock" } } } },
  }) as ReviewFixturePhase;
}

/** A phase with six raw trade-offs, before any evaluator has run. */
function sixRawPhase(): PhaseState {
  const messages = Array.from({ length: 6 }, (_, i) =>
    makeMessage({ id: `T-${i + 1}`, state: "raw", title: `A raw trade-off ${i + 1}` }),
  );
  return basePhase({ runId: "33c41174", messages });
}

/** Six raw trade-offs after an evaluation that publishes four, merges one of
 * the others into T-4 and drops the last. */
function evaluatedPhase(): PhaseState {
  const published = [1, 2, 3, 4].map((n) =>
    makeMessage({ id: `T-${n}`, state: "published", title: `A published trade-off ${n}` }),
  );
  const merged = settleMessage(makeMessage({ id: "T-5", title: "A duplicate trade-off" }), "merged", "merged into T-4");
  const dropped = settleMessage(makeMessage({ id: "T-6", title: "A trivial trade-off" }), "dropped", "trivial, not reviewable");
  return basePhase({ runId: "33c41174", messages: [...published, merged, dropped] });
}

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

function escapeRe(text: string): string {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

test("review.org matches its golden file, blockers first", () => {
  const phase = fixturePhase();
  const review = projectReview(phase);
  assertGolden("review.org", review);
  assert.match(review, /^\* Blockers$/m);
  assert.match(review, /^\* Trade-offs$/m);
  assert.match(review, /^\* Findings$/m);
  assert.ok(review.indexOf("* Blockers") < review.indexOf("* Trade-offs"));
  assert.ok(review.indexOf("* Trade-offs") < review.indexOf("* Findings"));
});

test("only a blockers-list message is a Blocker; a blocking finding is a Finding", () => {
  const review = projectReview(fixturePhase());
  // The blocker (raisedAsBlocker) lists under Blockers, with its panel outcome.
  const blockerSection = review.slice(review.indexOf("* Blockers"), review.indexOf("* Trade-offs"));
  assert.match(blockerSection, /^\*\* B-1 /m);
  assert.match(blockerSection, /^\s*Panel: escalate — the cancel path can deadlock$/m);
  // The ordinary blocking finding lists under Findings, marked `blocking`, and
  // never appears under Blockers.
  const findingSection = review.slice(review.indexOf("* Findings"));
  assert.match(findingSection, /^\*\* F-1 .*\[blocking\]$/m);
  assert.doesNotMatch(blockerSection, /F-1/);
});

test("six raw trade-offs show no entries, only an awaiting-evaluation line", () => {
  const review = projectReview(sixRawPhase());
  assertGolden("six-raw.org", review);
  assert.doesNotMatch(review, /^\*\* T-\d/m);
  assert.match(review, /^6 raw, awaiting evaluation$/m);
  assert.match(review, /^\(none\)$/m); // Findings and Blockers have nothing
});

test("after evaluation only the published entries show; drops count, merges live in their target", () => {
  const phase = evaluatedPhase();
  const review = projectReview(phase);
  assertGolden("evaluated.org", review);
  for (const n of [1, 2, 3, 4]) {
    assert.match(review, new RegExp(`^\\*\\* T-${n} A published trade-off ${n}$`, "m"));
  }
  assert.doesNotMatch(review, /^\*\* T-5/m);
  assert.doesNotMatch(review, /^\*\* T-6/m);
  assert.doesNotMatch(review, /awaiting evaluation/);
  assert.match(review, /^1 dropped$/m);
  const target = phase.messages!.find((m) => m.id === "T-4")!;
  const file = renderMessageFile(target, phase);
  assert.match(file, /^\* Merged in$/m);
  assert.match(file, /- T-5 A duplicate trade-off/);
});

test("a message published unevaluated by a timeout is not an entry", () => {
  // An evaluator timeout publishes the raw message unchanged with
  // `unevaluated: true` (core/reduce.ts). Its raw title must not be shown as
  // if an evaluator had rewritten it; it is one awaiting-evaluation line.
  const unevaluated = makeMessage({
    id: "T-1",
    state: "published",
    unevaluated: true,
    title: "The raw first sentence the evaluator never rewrote",
  });
  const phase = basePhase({ messages: [unevaluated] });
  const review = projectReview(phase);
  assert.doesNotMatch(review, /^\*\* T-1/m);
  assert.match(review, /^1 raw, awaiting evaluation$/m);
  assert.equal(reviewSummary(phase.messages), "T 1 (1 raw) · F 0 · B 0 · C-c m d");
  // Once the owner settles it, it is a real entry with its verdict again.
  const accepted = settleMessage(unevaluated, "accepted", "the raw wording is fine", "owner");
  const after = basePhase({ messages: [accepted] });
  assert.match(projectReview(after), /^\*\* T-1 The raw first sentence the evaluator never rewrote$/m);
  assert.equal(reviewSummary(after.messages), "T 1 · F 0 · B 0 · C-c m d");
});

test("the header names the readable id and directory id; the internal runId stays in the drawers", () => {
  const review = projectReview(fixturePhase());
  assert.match(review, /^#\+TITLE: tradeoffs-trace review — cebd7fcb-01 · 33c41174$/m);
  assert.doesNotMatch(review, /^#\+.*07ca06e2/m);
  assert.match(review, /^ *:RUN_ID: 07ca06e2$/m);
});

test("low-importance messages fold under Minor (N)", () => {
  const review = projectReview(fixturePhase());
  // T-2 is the low trade-off; F-2 is the low finding.
  assert.match(review, /^\*\* Minor \(1\)$/m);
  assert.match(review, /^\*\*\* T-2 Rename the helper$/m);
  assert.match(review, /^\*\*\* F-2 a slow path returns NaN to callers$/m);
  assert.match(review, /^\*\* T-1 Batch cancels per tick$/m);
});

test("a message with a verdict shows it in the heading and the body", () => {
  const review = projectReview(fixturePhase());
  assert.match(review, /^ *:VERDICT: accepted$/m);
  assert.match(review, /Verdict: accepted by owner — the goal needs the smaller lock window/);
  assert.match(review, /^ *:VERDICT: refused$/m);
  assert.match(review, /:FOLLOW_UP: true/);
});

test("the heading properties carry the full verdict binding", () => {
  const review = projectReview(fixturePhase());
  assert.match(review, /^ *:MESSAGE_VERSION: 1$/m);
  assert.match(review, /^ *:CANDIDATE_SHA: C1$/m);
  assert.match(review, /^ *:CONTRACT_VERSION: 1$/m);
  assert.match(review, /^ *:RUN_ID: 07ca06e2$/m);
  assert.match(review, /^ *:PHASE_ID: p1$/m);
  assert.match(review, /^ *:RAISED_BY: M$/m);
  assert.match(review, /^ *:IMPORTANCE: high$/m);
  assert.match(review, /^ *:SEVERITY: blocking$/m);
});

test("each message file matches its golden file", () => {
  const phase = fixturePhase();
  for (const message of phase.messages!) {
    assertGolden(`messages/${message.id}.org`, renderMessageFile(message, phase));
  }
});

test("a message file carries evidence, plan, history, ledger and votes", () => {
  const phase = fixturePhase();
  const t1 = phase.messages!.find((m) => m.id === "T-1")!;
  const file = renderMessageFile(t1, phase);
  assert.match(file, /^\* Evidence$/m);
  assert.match(file, /- src\/cancel\.ts:42/);
  assert.match(file, /^\* Plan$/m);
  assert.match(file, /plan\/03b\.org/);
  assert.match(file, /^\* History$/m);
  assert.match(file, /- v1 · C1 · [0-9a-f]{64}/);
  assert.match(file, /^\* Ledger$/m);
  assert.match(file, /- accepted by owner \(v1, C1\) — the goal needs the smaller lock window/);
  assert.match(file, /^\* Votes$/m);
  assert.match(file, /- M approve — within the latency budget/);
});

test("history records every version a carry produced", () => {
  const phase = fixturePhase();
  const carried = makeMessage({
    id: "T-9",
    title: "a changed choice",
    state: "published",
    messageVersion: 2,
    boundCandidateSha: "C2",
    versionContentHashes: { 1: contentHashOf({ type: "tradeoff", title: "old", summary: "s", context: "c", evidence: [] }) },
    carriedFrom: [{ candidateSha: "C1", version: 1 }],
  });
  phase.messages = [carried];
  const file = renderMessageFile(carried, phase);
  assert.match(file, /- v1 · C1 · [0-9a-f]{64}/);
  assert.match(file, /- v2 · C2 · [0-9a-f]{64}/);
});

test("failed records read as trade-offs, and no status or review text says `decisions`", () => {
  const C = { sha: "C1", contractVersion: CV() };
  const decisions = Array.from({ length: 16 }, (_, i) =>
    decision({ id: `D-${i + 1}`, choice: `choice ${i + 1}`, boundCandidateSha: "C1" }),
  );
  const phase = basePhase({
    phase: "REVIEWING",
    candidate: C,
    decisions,
    checks: { candidateSha: "C1", passed: true },
    reviews: {
      M: { review: approvingReview("M", "C1", CV()) },
      A: { review: approvingReview("A", "C1", CV()) },
      B: { review: approvingReview("B", "C1", CV()) },
    },
  });
  const reasons = notAcceptedReasons(phase);
  assert.match(reasons.join("; "), /16 trade-offs failed \(missing ballot from M/);
  assert.doesNotMatch(reasons.join("; "), /decisions/);
  const summary = reviewSummary(phase.messages);
  const status = renderStatusView({
    runDir: "/tmp/tt-render-status",
    title: "sum validation",
    phase: phase as unknown as Record<string, unknown>,
    alive: false,
    view: {
      elapsed: "1m",
      pipeline: "review 12s",
      gates: "checks ✓",
      reviewLine: "M ✓   A ✓   B ✓",
      review: summary,
      verdict: `not accepted: ${reasons.join("; ")} → repair attempt 2`,
      previousRound: `round 1 · C1 · not accepted: ${reasons.join("; ")}`,
      envTools: [],
    },
    ownerInputs: [],
    pendingOwnerInputs: [],
    ownerDirectives: [],
  } as unknown as StatusViewInput);
  assert.doesNotMatch(status, /decisions/);
  assert.match(status, /16 trade-offs failed/);
});

test("the status `review` line counts the same messages review.org shows, with no `decisions`", () => {
  const phase = sixRawPhase();
  const review = projectReview(phase);
  const summary = reviewSummary(phase.messages);
  assert.equal(summary, "T 6 (6 raw) · F 0 · B 0 · C-c m d");
  const input = {
    runDir: "/tmp/tt-render-status",
    title: "sum validation",
    phase: phase as unknown as Record<string, unknown>,
    alive: false,
    view: {
      elapsed: "1m",
      pipeline: "review 12s",
      gates: "checks ✓",
      reviewLine: "M ✓   A ✓   B ✓",
      review: summary,
      boundaryFilesChanged: 2,
      envTools: [],
    },
    ownerInputs: [],
    pendingOwnerInputs: [],
    ownerDirectives: [],
  } as unknown as StatusViewInput;
  const status = renderStatusView(input);
  assert.match(status, new RegExp(`^review {4}${escapeRe(summary)}$`, "m"));
  assert.doesNotMatch(status, /decisions/);
  // Advisory A-5: the boundary-changed note keeps a row of its own.
  assert.match(status, /^boundary {2}files changed: 2 \(reviewers classify\)$/m);
  assert.match(review, /^6 raw, awaiting evaluation$/m);
  // A phase with entries: the counts follow the entries and the drop count.
  const evaluated = evaluatedPhase();
  assert.equal(reviewSummary(evaluated.messages), "T 4 (1 dropped) · F 0 · B 0 · C-c m d");
  assert.match(projectReview(evaluated), /^1 dropped$/m);
});
