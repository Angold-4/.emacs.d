# tradeoffs-trace: implementation plan

Status: **Plan for review.** This plan turns the design in
[tradeoffs-trace](tradeoffs-trace.md) into phases that can actually be built.
Section references such as "design §6.3" point into that document.

Every phase ends at an **exit gate**: evidence that must exist before the next
phase starts. A phase that cannot produce its evidence is a design finding, and
the design document is revised before work continues. Each phase is one pull
request against a `feat/tradeoffs-trace` integration branch, which merges to
`main` only after phase 3.

## How this plan is executed

tradeoffs-trace cannot run itself until it exists, so phases 0 to 3 are run
with the current skill-based `/delegate` workflow: one worker per phase and
master review. The owner reviews each phase's PR against its exit gate.

From phase 4 onwards, each phase is written as a tradeoffs-trace plan and run
through the pipeline built in phases 0 to 3. That is the first real use of the
system on non-trivial work, and its records feed the pilot in phase 5.

**The runner is frozen while it builds itself.** A run that works on
tradeoffs-trace must not execute the code under review:

- The conductor, the Pi extension and the Emacs module used for a run come
  from an **installed runner**: a checkout of an accepted revision at
  `~/.tradeoffs-trace/runner/<sha>/`, outside every worker's worktree.
  `C-c m r` launches from there, never from the repository being edited.
- The runner sha is recorded in the run's `meta.json` and first event. A
  restart or repair of that run reuses the same runner, and a conductor whose
  own revision does not match the recorded sha refuses to resume.
- The runner is upgraded only **between runs**, and only to a revision
  accepted through that phase's exit gate: `tt runner install <sha>`.
- `/delegate` stays available to repair a broken runner, because the pipeline
  cannot be relied on to fix itself.

## Conventions

| Topic | Choice |
| --- | --- |
| Code location | `tradeoffs-trace/` in this repository (a Node package) and `core/init-tradeoffs-trace.el`. Moving it to its own repository later is cheap while nothing else depends on it. **Owner decision D0-1.** |
| Language | TypeScript, run with Node ≥ 22.19 (Pi 0.87.0's floor; locally 25.9) |
| Pi | pinned to **0.87.0**. The version is asserted at conductor start, and an upgrade is a deliberate PR that reruns the phase 0 contract tests |
| Tests | `node --test` for TypeScript; ERT in batch mode (`emacs -batch`) for Emacs Lisp; one `make check` target runs both |
| Structure | **functional core, imperative shell.** All state transitions, predicates, the tally and version binding are pure functions over the event log, tested without Pi, git or a filesystem. Effects (Pi, shell, git, files, socket) are thin adapters around that core |
| Fake agent | `test/fake-pi/`: an executable speaking Pi's RPC protocol that replays a script (tool calls, submissions, hangs, crashes). Deterministic tests use it. Live tests with real Pi and real models are opt-in for ordinary CI, but **mandatory recorded evidence** at the exit gates that name them (see below) |
| Plan parsing | Emacs parses the plan with `org-element` at `C-c m r` and writes `plan/v<n>.json` beside the Org snapshot. The conductor validates the JSON against a schema and never parses Org itself. Tests use JSON fixtures |

**Live evidence is part of the gate.** A test marked *required, recorded*
must have been run against real Pi and real models, with its log, candidate
shas and outcome committed under `test/live/records/<phase>/`, before that
phase is accepted. Passing fake-agent tests never substitutes for it. If
credentials or models are unavailable, the gate stays **incomplete**. It is
not waived, and the phase is not reported as done.

```text
tradeoffs-trace/
  src/core/      types, events, reduce, next, predicate, tally, binding, schema
  src/effects/   log, lock, pi-rpc, shell, git, sweep, inbox, socket, clock
  src/conductor.ts   the daemon: core + effects + deadlines
  src/cli.ts         tt start | status | cmd | crash-suite
  extension/     tradeoffs-trace.ts (the Pi extension: sh, submit_*, guards)
  test/          unit, contract, fake-pi, crash, live
core/init-tradeoffs-trace.el
test/tradeoffs-trace-test.el
```

---

## Phase 0: executable contracts

**Goal.** Make the design executable as data and pure functions, and prove the
lifecycle can always finish, before any process is started.

**Deliverables**

- `src/core/types.ts` and JSON schemas for: plan, phase contract, every event
  type, decision, finding, owner request, correction, ballot, review, owner
  command, and the binding tuple (design §7.1).
- `src/core/reduce.ts`: `reduce(state, event) → state`, a total function
  that rejects unknown or out-of-order events explicitly.
- `src/core/next.ts`: `next(state) → actions`. This is the only place that
  decides what happens next, and it covers every transition in design §6.1.
- `src/core/predicate.ts`: `accept(C, K)`, `addressed(X, C, K)` and
  `done(phase)`, exactly as in design §6.3.
- `src/core/tally.ts`: the master-veto two-of-three rule and its validity rules
  (design §5.1).
- `src/core/binding.ts`: staleness checks for ballots and commands.
- `extension/tradeoffs-trace.ts`, skeleton only: registers `sh`,
  `submit_phase`, `submit_discovery` and `submit_review`, and reports
  `pi.getActiveTools()` at `session_start`.
- The launch commands for each role, recorded as constants, both as explicit
  allowlists: worker `--tools read,edit,write,grep,find,ls,sh,submit_phase`;
  reviewers `--tools read,grep,find,ls,submit_discovery,submit_review`. Do
  not use `--exclude-tools`. Against Pi 0.87.0 it leaves the worker with
  `submit_discovery` and `submit_review` and without `grep`, `find` and `ls`.

**Tasks**

1. Transcribe design §6.1 into a transition table, one row per transition,
   with its guard and its emitted actions. `next.ts` is generated from the
   table or checked against it, so that the document and the code cannot
   drift silently.
2. Write the predicate and tally as pure functions over a state snapshot.
3. Write the tool-set contract test against the **real installed Pi**. It
   launches each role's command with the skeleton extension, receives the
   reported active tools, and asserts exact equality. No model call is made.

**Required evidence (exit gate)**

| Test | Asserts |
| --- | --- |
| `correction-closure` | a correction raised on candidate C1 is addressed by C2 whose three reviews say "honored"; `accept(C2)` holds; the `ACCEPTED` event marks it `resolved`. The reverse case, where one review says "not honored", does not accept |
| `integration-recovery` | probe of C1 fails → `integration` finding → repair → C2 probe passes → finding repaired → accept → publish moves the branch from H to I only via compare-and-swap |
| `role-tool-sets` | against the real installed Pi, the reviewer launch reports exactly `read, grep, find, ls, submit_discovery, submit_review`, and the worker launch exactly `read, edit, write, grep, find, ls, sh, submit_phase`. Any extra or missing tool fails, including `bash` or another role's submission tool |
| `no-circularity` | a property test: for random event sequences that the owner and reviewers eventually satisfy, every phase reaches `DONE`, `BLOCKED`, `AWAITING_OWNER` or `PAUSED`, and `accept` never reads a fact that only acceptance or later produces |
| `vote-table` | all eight M/A/B combinations, plus missing, malformed and evidence-free ballots counting as reject |
| `stale-binding` | a ballot or command bound to a superseded candidate, contract or record version is rejected with a visible reason |
| `finding-authority` | no vote outcome can close a finding; only the raising reviewer or the owner can |

**Owner decisions:** D0-1 code location. D0-2: accept the transition table as
the normative version of design §6.1.

---

## Phase 1: execution foundation

**Goal.** A single-phase conductor that runs a real worker end to end and can
be killed, crashed and restarted at any point without lying about what
happened. Reviews are stubbed: the fake agent submits scripted reviews.

**Deliverables**

- `src/effects/log.ts`: append-only `events.jsonl`, fsync per event, and
  intent/completion helpers with action IDs.
- `src/effects/lock.ts`: `flock` on `conductor.lock`; a second conductor
  fails fast.
- `src/effects/pi-rpc.ts`: spawns `pi --mode rpc` in its own process group,
  frames RPC, forwards events to the stream, exposes `prompt`, `steer`,
  `abort`, and waits for `agent_settled`.
- `src/effects/shell.ts` and the extension's `sh`: commands forwarded over
  `conductor.sock`, spawned by the conductor in recorded process groups,
  with output streamed back and a per-command deadline.
- `src/effects/sweep.ts`: `lsof +D` sweep, kill and record; `tainted`
  marking.
- `src/effects/git.ts`: worktree creation, freeze commit with the
  `TT-Action` trailer, read-only candidate checkouts, disposable check
  checkouts, integrity verification, probe branch, compare-and-swap publish.
- Deadlines for every stage in design §8.1, cancellation as in design §8.2,
  and the run execution budget.
- `TT_CRASH_AT=<boundary>` injection at every intent/effect/completion
  boundary in design §9.3.
- `tt start <plan.json>` and `tt status`: a text rendering of the log is
  enough for this phase.

**Required evidence (exit gate)**

| Test | Asserts |
| --- | --- |
| `force-kill-shell` | `SIGKILL` Pi while a worker `sh` command runs: the command's group is gone, the sweep finds nothing, and the candidate checkout is unchanged |
| `deadline-every-stage` | using the fake agent, each stage in design §8.1 is forced past its deadline and ends in the specified state, with no orphan processes |
| `freeze-boundary` | after `submit_phase`, a scripted write to the live worktree never appears in a check's or review's checkout |
| `integrity-detect` | modifying a candidate checkout during a check invalidates that check and flags `integrity-violated` |
| `checked-equals-candidate` | for every recorded check, the checkout tree hash equals the candidate commit's tree |
| `crash-suite` | every `TT_CRASH_AT` boundary: no conductor-state effect applied twice, interrupted gates rerun and are never counted as passed, and the final state equals an uninterrupted run apart from reruns |
| `live-single-phase` (real model; **required**, recorded) | one small real phase in a scratch repository reaches `DONE` through the real worker and stub reviews |

**Owner decisions:** D1-1 default deadlines for the pilot repository
(design §8.1 placeholders). D1-2: which scratch repository and phase to use
for `live-single-phase`.

---

## Phase 2: review and correction loop

**Goal.** Replace the stub reviews with the real review loop, and prove that an
owner correction produces a new candidate, is re-checked and re-reviewed,
resolves, and lets the pipeline continue with no further owner action.

**Deliverables**

- Reviewer dispatch: M, A and B as separate Pi processes, with the two-turn
  review (discovery submitted before the worker's disclosure is shown). A
  and B are fixed for the phase; model assignment comes from run config.
- Decisions from all three sources: worker disclosure, reviewer discovery,
  boundary triggers (design §3.3). Classification and the owner-only
  downgrade rule.
- Findings with evidence, severity and optional reproduction; closure rules
  (design §4.2); contract objections opening linked findings and suspending
  votes.
- Owner requests, and the owner commands in design §7.4 applied through
  `inbox/` files, with `tt cmd <command> …` as the test and CLI front end.
- Steering, with `deliver.intent`/`deliver.done` and `delivery uncertain`.
- Revise, as in design §7.5: correction record, pausing affected execution,
  the new repair allowance, the verbatim repair prompt, and `addressed`.
- Sampling data: `detail` decisions and unreferenced hunks (design §3.5).

**Required evidence (exit gate)**

| Test | Asserts |
| --- | --- |
| `correction-e2e` (fake agents) | revise an accepted decision → the phase pauses → a repair attempt with the correction verbatim → new candidate → fresh checks, probe and three reviews → addressed → accepted → resolved → published, with no owner command after the revise |
| `correction-after-budget` | a correction submitted after three failed rounds still gets a repair attempt |
| `contract-objection` | the cancellation example from design §4.3: M and A approve, B objects on contract, and the phase does not accept until B confirms a repair or the owner acts |
| `steer-uncertain` | a crash between `steer` and its acknowledgement yields `delivery uncertain`, and the steer is never resent automatically |
| `unreferenced-hunks` | a diff hunk no record cites appears in the sample |
| `live-review` (real models; **required**, recorded) | one real phase with real M, A and B produces at least one decision record with every required plain-language field filled |

**Owner decisions:** D2-1 model assignment for M, A and B. D2-2: the stubborn
raising reviewer question (design §12). Keep the budget escape, or let a passing
reproduction close a `reproduced` finding?

---

## Phase 3: minimal Emacs experience

**Goal.** The owner can do everything from Emacs, and the run survives an Emacs
restart. Phases 1 to 3 together are the single-phase prototype in design
§11.1.

**Deliverables** (`core/init-tradeoffs-trace.el`)

- `C-c m r` in a plan buffer: `org-element` parse → validation with a
  `*tt-plan-errors*` buffer and jump-to-line → Org and JSON snapshots → run
  creation → daemon launch → workspace tab.
- The socket client: live events in, commands out; `inbox/` fallback when
  the daemon is down; reconnection.
- The workspace (design §1.3): trace buffer rendering forwarded RPC events,
  status buffer, and input buffer (steer or note).
- History rebuild from session files plus `stream/` on reopen (design §9.4).
  No Pilish command that resumes or mutates a session is reachable from these
  buffers.
- `C-c m s` focus-or-rebuild; run resolution across buffers (design §1.4).
- The frozen runner (see "How this plan is executed"): `tt runner install
  <sha>`, launching from the installed runner, recording the runner sha in
  `meta.json`, and refusing to resume under a different revision. It is
  needed before phase 4 can run on tradeoffs-trace itself.
- `C-c m d`: the decision view with sections, entries and keys as in design
  §10, and the revise buffer with its consequence preview (design §7.5).

**Required evidence (exit gate)**

| Test | Asserts |
| --- | --- |
| ERT `plan-validation` | invalid plans produce the errors buffer with correct line positions and start no run |
| `runner-pinned` | a run started from runner X refuses to resume under runner Y, and a worker editing the repository's `tradeoffs-trace/` does not change the code the running conductor executes |
| ERT `run-resolution` | the three resolution rules in design §1.4, including the prompt when a plan has several active runs |
| ERT `decision-render` | the pending and accepted fixtures render as in design §10.2 and §10.3, with details folded |
| ERT `commands-bound` | every key in the decision view writes a command carrying the full binding tuple; stale commands show the "changed since you viewed it" message |
| manual `restart-revise` (scripted checklist, recorded) | start a run → restart Emacs mid-worker-turn → `C-c m s` reconnects and the trace shows the in-progress turn → `C-c m d` → revise an accepted decision → observe it reach `resolved` and the phase reach `DONE` |

**Owner decisions:** D3-1: whether the trace and status layout in design §1.3
works on the owner's actual screens. Adjusting it is expected and does not count
as a design change.

---

## Phase 4: multi-phase execution

**Goal.** Serial multi-phase runs, with the master reviewer spanning phases and
corrections to an already integrated phase.

Run through tradeoffs-trace itself, from a plan written in its own format.

**Deliverables**

- Phase sequencing: each phase's worktree starts from the published head;
  provisional phases become executable only when their contract validates.
- The master reviewer spanning phases. It is rebuilt at each phase from the
  plan plus the control log, with its session kept for continuity. This
  settles the first open question in design §12 unless the owner decides
  otherwise.
- Corrections and amendments to a `DONE` phase while no later phase has been
  integrated (design §7.5): cancel the later phase, move the branch back to
  the recorded head, reopen, then probe and publish again. With a later phase
  integrated, the revise buffer offers a new run from an amended plan.

**Required evidence (exit gate)**

| Test | Asserts |
| --- | --- |
| `three-phase` (fake agents) | three serial phases reach `DONE`, each starting from the previous published head |
| `correct-earlier-phase` | correcting phase 1 while phase 2 is in flight cancels phase 2, reopens phase 1, republishes it, and restarts phase 2 from the new head |
| `correct-after-later-integrated` | the revise buffer refuses and offers a new run, and nothing changes |
| `master-continuity` | the master reviewer's phase-2 review cites a phase-1 accepted decision from its rebuilt context |
| dogfood record | this phase's own run: its `events.jsonl`, decision view and any owner corrections, kept for phase 5 |

**Owner decisions:** D4-1: the master reviewer's context rule above.

---

## Phase 5: comparative pilot

**Goal.** Establish whether tradeoffs-trace reduces the owner's effort, at an
acceptable cost and miss rate, compared with the current workflow (design
§11.4).

**Deliverables**

- Instrumentation: an Emacs activity timer on run buffers and on the old
  workflow's transcripts; token and cost aggregation per role from Pi usage
  events; the `u` and `s` marks already recorded in the log.
- An audit procedure. After each task, the owner and a separate audit agent
  that has seen none of the run's records list the consequential choices in
  the raw diff. Each one not surfaced during the run is a miss.
- At least three matched tasks per arm (current `/delegate` against
  tradeoffs-trace), chosen before either arm is run.
- `docs/tradeoffs-trace-pilot.md`: the report.

**Required evidence (exit gate)**

The report, per task and per arm: active owner minutes, unnecessary
escalations, audited missed choices, sampling misses (reported separately as an
observed rate), escaped defects over a fixed period, elapsed time, and model
usage. It ends with a recommendation: make it the default, restrict it to
risk-tagged phases, or change the design.

**Owner decisions:** D5-1 the task set, fixed before running. D5-2 the
thresholds for "better", fixed before looking at results.

---

## Summary

| Phase | Deliverable | Exit gate |
| --- | --- | --- |
| 0 | Executable contracts: schemas, reducer, transitions, predicate, tally, pinned tool configuration | correction closure, integration recovery and reviewer submission proven; no circularity |
| 1 | Single-phase conductor: log, lock, Pi adapter, conductor-owned shell, deadlines, freeze, probe and publish, recovery | force-kill, deadline, freeze, integrity and crash suites pass; checked files match the candidate |
| 2 | Three reviewers, findings, voting, owner requests, resolve, revise, steering | a correction produces a new candidate, is re-checked and re-reviewed, resolves, and the run continues on its own |
| 3 | `C-c m r/s/d`, live trace, status, input, decision view, revise buffer | restart Emacs mid-run, reconnect, revise a decision, observe completion |
| 4 | Multi-phase, the master reviewer across phases, corrections to integrated phases | correcting an earlier phase invalidates and restarts the affected unfinished work |
| 5 | Comparative pilot | report on effort, misses, escalations, defects, time and cost |
