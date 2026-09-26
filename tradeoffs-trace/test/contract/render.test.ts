// Plan 03b acceptance: the runtime renders `views/review.org` and one
// `views/messages/<id>.org` per message from the 03a message fixtures, and
// the bytes match golden files. Blockers come first, low-importance messages
// fold under "Minor (N)", and a settled message shows its verdict.

import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { test } from "node:test";

import { projectReview, renderMessageFile } from "../../src/render.ts";
import { contentHashOf } from "../../src/core/messages.ts";
import type { Ballot, Decision, Finding, Message, PhaseState } from "../../src/core/types.ts";
import { basePhase, CV, makeMessage } from "../unit/helpers.ts";

const GOLDEN = fileURLToPath(new URL("../fixtures/render", import.meta.url));

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

function settle(message: Message, state: "accepted" | "refused", reason?: string): Message {
  return {
    ...message,
    state,
    settlement: {
      state,
      settledBy: "owner",
      ...(reason ? { reason } : {}),
      candidateSha: message.boundCandidateSha,
      contractVersion: message.boundContractVersion,
      messageVersion: message.messageVersion,
      contentHash: message.contentHash,
    },
  };
}

/** The 03a fixture phase: one blocker, several trade-offs at each importance,
 * an advisory finding, a raw finding, a settled message and a refusal. */
function fixturePhase(): PhaseState {
  const decisions = [
    decision({ id: "D-1", choice: "Batch cancels per tick", class: "reserved" }),
    decision({ id: "D-2", choice: "Rename the helper", class: "detail" }),
    decision({ id: "D-3", choice: "Keep the existing file layout", class: "delegated" }),
  ];
  const findings = [
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
  const blocker = makeMessage({
    id: "B-1",
    type: "blocker",
    title: "the loop does not terminate on empty input",
    summary: "raised by M against C1",
    context: "an empty input leaves the cursor where it started",
    evidence: ["src/loop.ts:12 does not break on empty input"],
    planRef: "plan/03b.org",
    state: "published",
    sourceRecordId: "F-p1-M-1",
  });
  const accepted = settle(
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
  const refused = settle(
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
  );
  const advisory = makeMessage({
    id: "F-1",
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
    id: "F-2",
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
    runId: "r-03b",
    phaseId: "p1",
    decisions,
    findings,
    ballots,
    messages: [blocker, accepted, minorTradeoff, refused, advisory, raw],
  });
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

test("review.org matches its golden file, blockers first", () => {
  const phase = fixturePhase();
  const review = projectReview(phase);
  assertGolden("review.org", review);
  assert.ok(review.indexOf("B-1") < review.indexOf("T-1"), "blockers come before trade-offs");
  assert.match(review, /^\* Blockers$/m);
  assert.match(review, /^\* Trade-offs$/m);
  assert.match(review, /^\* Findings$/m);
  assert.ok(review.indexOf("* Blockers") < review.indexOf("* Trade-offs"));
  assert.ok(review.indexOf("* Trade-offs") < review.indexOf("* Findings"));
});

test("low-importance messages fold under Minor (N)", () => {
  const review = projectReview(fixturePhase());
  assert.match(review, /^\*\* Minor \(1\)$/m);
  // T-2 is the low trade-off; F-1 and F-2 are low findings.
  assert.match(review, /^\*\*\* T-2 Rename the helper$/m);
  // A reserved trade-off is high and stays a direct child of Trade-offs.
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
  assert.match(review, /^ *:RUN_ID: r-03b$/m);
  assert.match(review, /^ *:PHASE_ID: p1$/m);
  assert.match(review, /^ *:RAISED_BY: M$/m);
  assert.match(review, /^ *:IMPORTANCE: high$/m);
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