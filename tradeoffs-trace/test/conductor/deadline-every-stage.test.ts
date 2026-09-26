// `deadline-every-stage` (phase 1b work-packet item 2): one sub-test per
// design §8.1 row this packet wires up, each forcing that stage past its
// deadline and asserting the resulting state/log event and that no orphan
// process remains. Every agent process in this file is launched with a
// `--tt-marker=<unique>` argv token (fake-pi.ts ignores unknown argv; see
// harness.ts's `extraPiArgsPrefix`) purely so `pgrep -f <marker>` can prove
// nothing survives, and every `sh`/check/probe command line embeds its own
// unique marker text for the same reason.
//
// The file's own timeout is raised above the Makefile's 180s default: the 11
// sub-tests are inherently slow (each waits out a real deadline), and under
// the suite's four-way concurrency they already summed to 164s there — 92% of
// a limit that then failed the phase's own `make check` when the last
// sub-test's window was too narrow (see it below). The check as a whole still
// finishes far inside its own 5-minute budget.

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import { randomUUID } from "node:crypto";
import { test } from "node:test";

import {
  cleanupDir,
  defaultReviewerHello,
  defaultWorkerHello,
  readEvents,
  setupConductor,
  sleep,
  waitFor,
} from "./harness.ts";

function marker(tag: string): string {
  return `tt-deadline-${tag}-${randomUUID().slice(0, 8)}`;
}

async function assertNoOrphan(mark: string): Promise<void> {
  // A short grace period for signals already sent to actually reap.
  await sleep(300);
  let out = "";
  try {
    out = execFileSync("pgrep", ["-f", mark], { encoding: "utf8" }).trim();
  } catch {
    out = ""; // pgrep exits 1 (no match) — that's the success case.
  }
  assert.equal(out, "", `expected no process matching '${mark}' to survive, found: ${out}`);
}

test("deadline-every-stage", { timeout: 300_000 }, async (t) => {
  await t.test("worker attempt: exceeding workerAttemptMs cancels, times out, consumes a repair round", async () => {
    const mark = marker("worker-attempt");
    const setup = await setupConductor({
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "hang-forever" }] }),
      deadlines: { abortGraceMs: 200, termGraceMs: 200, helloTimeoutMs: 5_000, workerAttemptMs: 700 },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => {
        const events = readEvents(setup.runDir);
        return events.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "ATTEMPT_TIMED_OUT");
      }, 15_000);
      // consumes a round: a new attempt is dispatched (still IMPLEMENTING).
      await waitFor(() => setup.conductor.state.phase.attempt.n >= 2, 15_000);
      assert.equal(setup.conductor.state.phase.phase, "IMPLEMENTING");
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("worker attempt: exceeding the per-attempt token cap is treated exactly like a timeout", async () => {
    const mark = marker("token-cap");
    const setup = await setupConductor({
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          { kind: "emit", event: { type: "message_update", usage: { totalTokens: 10 }, assistantMessageEvent: {} } },
          { kind: "emit", event: { type: "message_update", usage: { totalTokens: 999_999 }, assistantMessageEvent: {} } },
          { kind: "hang-forever" },
        ],
      }),
      deadlines: {
        abortGraceMs: 200,
        termGraceMs: 200,
        helloTimeoutMs: 5_000,
        workerAttemptMs: 60_000, // large — the token cap must be what fires, not the wall-clock one
        tokenCapPerAttempt: 1_000,
      },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => {
        const events = readEvents(setup.runDir);
        return events.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "ATTEMPT_TIMED_OUT");
      }, 15_000);
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("settle reminders exhausted: an agent that settles without submitting is attempt no_submission, consumes a round", async () => {
    const mark = marker("no-submission");
    const setup = await setupConductor({
      // No submit_phase call at all — fake-pi finishes its (empty) step
      // list and settles on its own, exactly what the extension's own
      // "two settle reminders exhausted" path also ends in from the
      // conductor's point of view (see core/protocol.ts's NoSubmissionMessage
      // doc comment — the conductor treats both the same).
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [] }),
      deadlines: { abortGraceMs: 200, termGraceMs: 200, helloTimeoutMs: 5_000, workerAttemptMs: 15_000 },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => {
        const events = readEvents(setup.runDir);
        return events.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "ATTEMPT_NO_SUBMISSION");
      }, 15_000);
      await waitFor(() => setup.conductor.state.phase.attempt.n >= 2, 15_000);
      assert.equal(setup.conductor.state.phase.phase, "IMPLEMENTING");
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("worker sh command: exceeding shCommandMs kills its group and returns a timeout tool result", async () => {
    const mark = marker("sh-cmd");
    const setup = await setupConductor({
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-sh", command: `echo START-${mark}; sleep 300` },
          { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
        ],
      }),
      deadlines: { abortGraceMs: 200, termGraceMs: 200, helloTimeoutMs: 5_000, workerAttemptMs: 60_000, shCommandMs: 500 },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      // The command's own process group must actually be gone.
      await waitFor(() => {
        try {
          execFileSync("pgrep", ["-f", mark], { encoding: "utf8" });
          return false;
        } catch {
          return true;
        }
      }, 5_000);
      // The worker still reaches submission afterwards (the tool result
      // was `timeout`, not a fatal error, so the script's next step runs).
      await waitFor(() => setup.conductor.state.phase.phase !== "IMPLEMENTING", 15_000);
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("freeze: exceeding freezeMs force-kills, sweeps, taints, and fails the attempt", async () => {
    const mark = marker("freeze");
    const setup = await setupConductor({
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [
          { kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } },
          { kind: "hang-forever" }, // ignores the freeze's own `abort` — never settles on its own
        ],
      }),
      deadlines: { abortGraceMs: 200, termGraceMs: 200, helloTimeoutMs: 5_000, workerAttemptMs: 60_000, freezeMs: 700 },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => {
        const events = readEvents(setup.runDir);
        return events.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "FREEZE_TIMED_OUT");
      }, 15_000);
      assert.equal(setup.conductor.state.phase.worktreeTainted, true, "a timed-out freeze must taint the worktree");
      // Attempt failed: no candidate was ever produced by this freeze.
      assert.equal(setup.conductor.state.phase.candidate, undefined);
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("check command: exceeding checkMs kills its group and fails the check as a timeout", async () => {
    const mark = marker("check");
    const setup = await setupConductor({
      checks: [`echo START-${mark}; sleep 300`],
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
      }),
      deadlines: { abortGraceMs: 200, termGraceMs: 200, helloTimeoutMs: 5_000, workerAttemptMs: 30_000, checkMs: 700, freezeMs: 15_000 },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => {
        const events = readEvents(setup.runDir);
        return events.some((e) => e.kind === "completion" && (e.event as { reason?: string }).reason === "timeout");
      }, 15_000);
      await waitFor(() => {
        const events = readEvents(setup.runDir);
        return events.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "CHECKS_FAILED");
      }, 5_000);
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("integration probe: a command exceeding probeMs is killed, the probe branch is discarded, and it is an integration timeout finding", async () => {
    const mark = marker("probe");
    const probeMarkerFile = `/tmp/${mark}-seen`;
    const setup = await setupConductor({
      // Passes quickly the first time (during CHECKING); the second time
      // the very same command runs (during PROBING, against the probed
      // integration checkout) it sleeps long enough to hit probeMs.
      checks: [`test -f ${probeMarkerFile} && { echo START-${mark}; sleep 300; } || touch ${probeMarkerFile}`],
      // The probe must actually rerun the checks for this case (plan 2c
      // otherwise reuses the candidate's passed checks on a fast-forward).
      probeReuse: false,
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
      }),
      deadlines: {
        abortGraceMs: 200,
        termGraceMs: 200,
        helloTimeoutMs: 5_000,
        workerAttemptMs: 30_000,
        checkMs: 10_000,
        freezeMs: 15_000,
        probeMs: 700,
      },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => {
        const events = readEvents(setup.runDir);
        return events.some(
          (e) => e.kind === "event" && (e.event as { type?: string; evidence?: string }).type === "PROBE_FAILED" && (e.event as { evidence?: string }).evidence?.includes("timeout"),
        );
      }, 15_000);
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      fs.rmSync(probeMarkerFile, { force: true });
      await assertNoOrphan(mark);
    }
  });

  await t.test("review: exceeding reviewMs cancels and re-dispatches once, then BLOCKED reviewer unavailable", async () => {
    const mark = marker("review");
    const setup = await setupConductor({
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
      }),
      reviewerScriptFor: () => ({ hello: defaultReviewerHello(), steps: [{ kind: "hang-forever" }] }),
      deadlines: {
        abortGraceMs: 200,
        termGraceMs: 200,
        helloTimeoutMs: 5_000,
        workerAttemptMs: 30_000,
        freezeMs: 15_000,
        reviewMs: 600,
      },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => setup.conductor.state.phase.phase === "BLOCKED", 20_000);
      assert.match(setup.conductor.state.phase.blockedReason ?? "", /reviewer unavailable/);
      const timeouts = readEvents(setup.runDir).filter(
        (e) => e.kind === "event" && (e.event as { type?: string }).type === "REVIEW_TIMED_OUT",
      );
      assert.ok(timeouts.length >= 2, "expected at least one re-dispatch (2 REVIEW_TIMED_OUT events) before BLOCKED");
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("repair rounds exhausted: open items become owner requests (AWAITING_OWNER)", async () => {
    const mark = marker("repair-rounds");
    const setup = await setupConductor({
      checks: ["false"], // every candidate fails checks, forever
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
      }),
      deadlines: { abortGraceMs: 200, termGraceMs: 200, helloTimeoutMs: 5_000, workerAttemptMs: 20_000, checkMs: 10_000, freezeMs: 15_000 },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 40_000);
      // design §8.1's "open items become owner requests": with no
      // record-level item to blame (repeated CHECKS_FAILED, not a finding/
      // decision/correction), reduce.ts's own escape hatch
      // (owner-requests.ts's `repair_budget_exhausted` origin) opens a
      // generic owner request carrying the cause text.
      const ownerRequests = setup.conductor.state.phase.ownerRequests;
      assert.ok(
        ownerRequests.some((r) => r.origin === "repair_budget_exhausted"),
        `expected an open owner request for the exhausted repair budget, got: ${JSON.stringify(ownerRequests)}`,
      );
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("run execution budget: exceeding it while actively executing stops dispatching (PAUSED: budget)", async () => {
    const mark = marker("budget-active");
    const setup = await setupConductor({
      workerScript: () => ({ hello: defaultWorkerHello(), steps: [{ kind: "hang-forever" }] }),
      deadlines: {
        abortGraceMs: 200,
        termGraceMs: 200,
        helloTimeoutMs: 5_000,
        workerAttemptMs: 60_000, // must not fire before the budget does
        runBudgetMs: 700,
      },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => setup.conductor.state.run === "RUN_PAUSED_BUDGET", 10_000);
      const events = readEvents(setup.runDir);
      assert.ok(
        events.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "RUN_BUDGET_EXCEEDED"),
        "expected a logged RUN_BUDGET_EXCEEDED event",
      );
    } finally {
      // RUN_PAUSED_BUDGET does not auto-stop — stop it ourselves.
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });

  await t.test("run execution budget: time spent AWAITING_OWNER is not counted against it", async () => {
    const mark = marker("budget-paused");
    // checks always fail -> repair rounds exhaust (~9s of real, actively
    // *executing* fake-pi round trips on an idle machine, 13–20s while the
    // rest of `make test` runs alongside this file) -> AWAITING_OWNER.
    //
    // The two numbers below are one window, and it is deliberately not a
    // guess about how long that flow takes: the dwell is LONGER THAN THE
    // WHOLE BUDGET, so a budget clock that kept running while the phase was
    // parked would necessarily have fired by the time this sub-test checks,
    // whatever the flow cost — and the budget still leaves room for the flow
    // to take twice as long as measured. A fixed 10s dwell with a 15s budget
    // did need that guess, and under four-way test concurrency the flow
    // itself outlasted the 15s budget: the run paused before it ever parked
    // (`RUN_PAUSED_BUDGET` does not auto-stop) and this sub-test timed out.
    const budgetMs = 45_000;
    const setup = await setupConductor({
      checks: ["false"],
      workerScript: () => ({
        hello: defaultWorkerHello(),
        steps: [{ kind: "call-submit", tool: "submit_phase", args: { decisions: [], assumptions: [], deviations: [] } }],
      }),
      deadlines: {
        abortGraceMs: 200,
        termGraceMs: 200,
        helloTimeoutMs: 5_000,
        workerAttemptMs: 20_000,
        checkMs: 10_000,
        freezeMs: 15_000,
        runBudgetMs: budgetMs,
      },
      extraPiArgsPrefix: [`--tt-marker=${mark}`],
    });
    await setup.conductor.start();
    try {
      await waitFor(() => setup.conductor.state.phase.phase === "AWAITING_OWNER", 40_000);
      // Sit parked for longer than the whole budget. If the budget clock ran
      // during this dwell, RUN_BUDGET_EXCEEDED would already have fired —
      // design §8.1/§8.2: "a phase parked in AWAITING_OWNER consumes
      // nothing."
      await sleep(budgetMs + 1_000);
      assert.equal(setup.conductor.state.run, "RUN_ACTIVE", "budget must not be consumed while AWAITING_OWNER");
      const events = readEvents(setup.runDir);
      assert.ok(
        !events.some((e) => e.kind === "event" && (e.event as { type?: string }).type === "RUN_BUDGET_EXCEEDED"),
        "no RUN_BUDGET_EXCEEDED should have fired while parked",
      );
    } finally {
      await setup.conductor.stop();
      cleanupDir(setup.runRoot);
      cleanupDir(setup.scriptsDir);
      await assertNoOrphan(mark);
    }
  });
});
