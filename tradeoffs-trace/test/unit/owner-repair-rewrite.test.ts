import assert from "node:assert/strict";
import { registerHooks } from "node:module";
import { test } from "node:test";
import { reduce } from "../../src/core/reduce.ts";
import { approvingReview, baseState, makeBallot, makeDecision } from "./helpers.ts";
import type { Event, OwnerRequest, State } from "../../src/core/types.ts";

// Run the production driver, replacing only its external effects. Parks,
// request generation, owner resolutions and replay use the real reducer.
const seams = ["state", "log", "runDomainRounds", "recordRegression", "declaredTestCommands", "ownerRepairBaseFact", "applyEvent", "fixRound"];
const url = new URL("../../src/conductor.ts?rewrite-driver", import.meta.url).href;
const hooks = registerHooks({ load(target, context, load) {
  const result = load(target, context);
  if (target !== url) return result;
  return { ...result, source: result.source.toString().replace(new RegExp(`#(${seams.join("|")})\\b`, "g"), "test_$1") };
} });
const { Conductor, ownerRepairPointFor } = await import(url);
hooks.deregister();

function apply(state: State, event: Event): State {
  const result = reduce(state, event);
  assert.ok(result.ok, result.ok ? "" : result.reason);
  return result.state;
}

function fixture() {
  const state = baseState({ phase: "IMPLEMENTING", candidate: { sha: "C1", contractVersion: baseState().phase.contract.contractVersion },
    decisions: [makeDecision()], rounds: [{ round: 1, base: "B", lanes: ["a", "b"], candidates: [], votes: [], picked: { lane: "a", sha: "C1", votes: 3 } }],
    ownerRequests: [], inFlight: { dispatch_worker: "first-map" } });
  state.phase.contract.domains = [{ name: "booking", globs: ["booking/**"] }];
  state.phase.reviews = Object.fromEntries(["M", "A", "B"].map((seat) => [seat, { review: approvingReview(seat, "C1", state.phase.contract.contractVersion) }]));
  state.phase.ballots = [makeBallot({ vote: "reject", evidence: ["booking/cache.ts:12"], rationale: "stale price" })];
  const request: OwnerRequest = { id: "OR1", version: 1, phaseId: "p1", origin: "failed_vote", linkedDecisionId: "D1", status: "open",
    reason: "booking/cache.ts:12 stale price", options: [{ id: "reject_and_repair", label: "repair" }, { id: "accept_as_implemented", label: "accept" }] };
  return { state, request };
}

function driver(state: State, events: Event[], order: string[]) {
  const conductor = new Conductor({ runDir: "/tmp/tt-rewrite-driver", plan: { title: "rewrite", repo: "/tmp", integrationBranch: "main", checks: [], rounds: 3, phases: [{ id: "p1" }] } });
  conductor.test_state = state;
  conductor.test_log = { intent() {}, append(kind: string, fact: { command?: string; reason?: string }) { order.push(`${kind}:${fact.command ?? fact.reason ?? ""}`); } };
  conductor.test_recordRegression = () => {};
  conductor.test_declaredTestCommands = () => ({ checks: [] });
  const facts = new Map<string, { exitCode: number; timedOut: boolean }>();
  conductor.test_ownerRepairBaseFact = async (finding) => {
    // Model the production effect's durable fact lookup as well as its write.
    if (!facts.has(finding.doneWhen)) {
      order.push(`done_when_base:${finding.doneWhen}`);
      facts.set(finding.doneWhen, { exitCode: finding.doneWhen === "true" ? 0 : 1, timedOut: false });
    }
    return facts.get(finding.doneWhen);
  };
  conductor.test_applyEvent = (event: Event) => {
    events.push(event); order.push(event.type);
    conductor.test_state = apply(conductor.test_state, event);
  };
  conductor.test_fixRound = async () => { order.push("dispatch"); return "stopped"; };
  return { conductor, send: conductor.test_applyEvent, run: () => conductor.test_runDomainRounds("test") };
}

for (const original of [undefined, "true"]) test(`UNPROVABLE ${original ?? "missing command"} can be rewritten before MAP, live and replayed`, async () => {
  const { state: initial, request } = fixture();
  const events: Event[] = [], order: string[] = [];
  const d = driver(initial, events, order);
  d.send({ type: "OWNER_REQUEST_OPENED", request });
  const resolve = (r: OwnerRequest, note?: string): Event => ({ type: "OWNER_REQUEST_RESOLVED", requestId: r.id, option: "reject_and_repair", note,
    boundCandidateSha: "C1", boundContractVersion: initial.phase.contract.contractVersion, boundRecordVersion: r.version });
  d.send(resolve(request, original && `doneWhen: ${original}`));
  await d.run();
  assert.equal(d.conductor.test_state.phase.phase, "AWAITING_OWNER");
  assert.ok(order.some((s) => /lead_called:X:UNPROVABLE/.test(s)));
  assert.ok(!order.includes("FINDING_RAISED") && !order.includes("dispatch"));
  const replacement = d.conductor.test_state.phase.ownerRequests.find((r) => r.status === "open" && r.linkedDecisionId === "D1")!;
  assert.ok(replacement, "the exception offers a replacement resolution for the same work");
  d.send(resolve(replacement, "doneWhen: test -f booking-fixed.txt"));
  d.send({ type: "REPAIR_ATTEMPT_STARTED" });
  const replayed = events.reduce(apply, initial);
  assert.deepEqual(replayed, d.conductor.test_state);
  for (const state of [d.conductor.test_state, replayed]) {
    const writes: Event[] = [], trace: string[] = [];
    const resumed = driver(state, writes, trace);
    await resumed.run();
    const fact = trace.indexOf("done_when_base:test -f booking-fixed.txt");
    assert.ok(fact >= 0 && fact < trace.indexOf("FINDING_RAISED"), trace.join("\n"));
    assert.ok(trace.indexOf("FINDING_RAISED") < trace.indexOf("dispatch"), trace.join("\n"));
    assert.equal(writes.filter((e) => e.type === "FINDING_RAISED").length, 1);
    const point = resumed.conductor.test_state.phase.findings[0];
    assert.equal(point.doneWhen, "test -f booking-fixed.txt");
    assert.equal(point.id, `F-p1-owner-repair-${replacement.id}`);
    assert.ok(!trace.some((s) => s.startsWith("lead_called")), trace.join("\n"));
    // Restart after admission reuses the point and the recorded base fact.
    await resumed.run();
    assert.equal(writes.filter((e) => e.type === "FINDING_RAISED").length, 1);
    assert.equal(trace.filter((s) => s === "done_when_base:test -f booking-fixed.txt").length, 1);
  }
});

test("a later ruling supersedes an unmaterialized repair, only at its binding", () => {
  const { state, request } = fixture();
  const K = state.phase.contract.contractVersion;
  const first = { ...request, status: "resolved" as const, resolution: { option: "reject_and_repair" }, resolvedBinding: { candidateSha: "C1", contractVersion: K } };
  const ruling = { ...first, id: "OR2", resolution: { option: "accept_as_implemented" } };
  state.phase.ownerRequests = [first, ruling];
  assert.equal(ownerRepairPointFor(state.phase, first, { checks: [] }), undefined);
  assert.equal(ownerRepairPointFor(state.phase, ruling, { checks: [] }), undefined);
  ruling.resolvedBinding = { candidateSha: "another", contractVersion: K };
  assert.ok("reason" in ownerRepairPointFor(state.phase, first, { checks: [] }));
});

test("a later repair reuses already materialized work rather than raising a duplicate", () => {
  const { state, request } = fixture();
  const first = { ...request, status: "resolved" as const, resolution: { option: "reject_and_repair", note: "doneWhen: false" },
    resolvedBinding: { candidateSha: "C1", contractVersion: state.phase.contract.contractVersion } };
  state.phase.ownerRequests = [first];
  const point = ownerRepairPointFor(state.phase, first, { checks: [] });
  assert.ok(point && "finding" in point);
  state.phase.findings = [point.finding];
  const latest = { ...first, id: "OR2", resolution: { option: "reject_and_repair", note: "doneWhen: different" } };
  state.phase.ownerRequests.push(latest);
  const reused = ownerRepairPointFor(state.phase, latest, { checks: [] });
  assert.ok(reused && "finding" in reused);
  assert.equal(reused.finding.id, point.finding.id);
  assert.equal(ownerRepairPointFor(state.phase, first, { checks: [] }), undefined);
});

test("a direct final owner ruling prevents an obsolete repair from recreating work", () => {
  const { state, request } = fixture();
  const repair = { ...request, status: "resolved" as const, resolution: { option: "reject_and_repair" },
    resolvedBinding: { candidateSha: "C1", contractVersion: state.phase.contract.contractVersion } };
  state.phase.ownerRequests = [repair];
  state.phase.ruledDecisions = [{ id: "D1", choice: state.phase.decisions[0].choice, by: "owner" }];
  assert.equal(ownerRepairPointFor(state.phase, repair, { checks: [] }), undefined);
});
