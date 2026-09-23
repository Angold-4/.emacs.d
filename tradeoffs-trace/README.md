# tradeoffs-trace

A programmed review pipeline for Pi in Emacs. See
[`../docs/tradeoffs-trace.md`](../docs/tradeoffs-trace.md) for the design and
[`../docs/tradeoffs-trace-plan.md`](../docs/tradeoffs-trace-plan.md) for the
build plan.

This package is plain TypeScript run by Node's native type stripping (Node
≥ 22.19, developed against 25.9.0): **no build step, zero npm dependencies**.
Import modules with explicit `.ts` extensions. The runner is later frozen by
copying a checkout with no `npm install`, so zero dependencies is a hard
constraint, not a style preference.

## Phase 0: executable contracts

`src/core/` is the pure "functional core" described in the design's
"functional core, imperative shell" convention: state transitions, the
acceptance predicate, the vote tally and version binding, all as pure
functions with no process, git or filesystem access. `schemas/` holds the
JSON Schema documents for every record kind; `src/core/schema.ts` is a
minimal validator supporting only the keywords those schemas use (no `ajv`).

`src/core/roles.ts` and `src/core/protocol.ts` extend the pure core with the
launch/role data (design §2.1's tool-allowlist table, `launchArgs`,
`assertToolSet`) and the shared strict-JSONL message protocol used both by
Pi's own RPC mode and by the run socket between the extension and the
conductor. `extension/tradeoffs-trace.ts` is the (still guard-free, design
§9.5 comes later) Pi extension skeleton: it registers `sh`, `submit_phase`,
`submit_discovery` and `submit_review` in every agent, reports
`pi.getActiveTools()` to the run socket at `session_start`, and validates
every submission with the core validator before ever forwarding it.

`schemas/submission.schema.json` is the single source of truth for what a
worker or reviewer can actually supply to `submit_phase`/`submit_discovery`:
its `$defs.decisionDisclosure` holds design §3.2's plain-language fields
(`choice`, `whyItMatters`, `alternatives`, `recommendation`) plus
`classProposal`, kept byte-for-byte identical (for the fields they share) to
`decision.schema.json`'s own definitions — enforced by
`test/contract/submission-schema.test.ts`. `submit_review` needs no such
split and validates directly against `schemas/review.schema.json`, since a
Review carries no conductor-assigned binding fields at all.
`extension/param-shapes.ts` holds the same submission tools' parameter
field lists as plain data (no `typebox` import), so both
`extension/tradeoffs-trace.ts`'s real typebox parameter schemas (built by
mapping over these arrays) and the contract test (which cannot import
`typebox` outside Pi's extension loader) are tied to one list of field
names each.

**What phase 1's conductor must do with a disclosed decision.** A worker's
`submit_phase` (and a reviewer's `submit_discovery`) can only supply the
`decisionDisclosure` shape above — a Decision's identity and binding fields
(`id`, `version`, `phaseId`, `source`, `class`, `boundCandidateSha`,
`boundContractVersion`) don't exist yet at submission time, since the
candidate itself is produced by the freeze that `SUBMIT_PHASE` triggers
(`reduce.ts`'s `SUBMIT_PHASE` case stores `Decision[]` as-is, so a real
conductor must construct the full records, not just relay the tool
arguments). Once `FREEZE_COMPLETED` gives it a `candidateSha`, the
conductor must: assign each disclosed item an `id` and `version: 1`, set
`phaseId` and `source` (`"worker"` for a `submit_phase` decision,
`"reviewer-discovered"` for a `submit_discovery` item), copy `choice` /
`whyItMatters` / `alternatives` / `recommendation` through unchanged, set
`class` from the disclosed `classProposal`, and set `boundCandidateSha` /
`boundContractVersion` to the phase's current candidate and contract
version — then validate the assembled record against
`schemas/decision.schema.json` before it becomes part of phase state. (This
packet's live smoke test's socket-server stub does exactly this, as a
stand-in for that conductor step — see
`test/live/live-submission.test.ts`.)

`test/fake-pi/fake-pi.ts` is a scriptable stand-in for a real Pi process,
speaking the same RPC and run-socket protocols, used by deterministic
tests. `test/contract/role-tool-sets.test.ts` runs against the **real
installed Pi** (no model call). `test/live/live-submission.test.ts` is the
opt-in live smoke test against real Pi and a real model — run it with `make
live` (requires `TT_LIVE=1` and provider credentials); it writes one record
per role under `test/live/records/phase-0/`.

Extensions run inside Pi's own process (loaded via its `jiti`-based
extension loader), so `typebox`, `@earendil-works/pi-ai` and
`@earendil-works/pi-coding-agent` are resolved from Pi's own installation,
not from this package's `node_modules` — tradeoffs-trace's own
`package.json` still declares zero dependencies.

## Phase 1a: effect adapters

`src/effects/` holds the thin, individually tested imperative modules the
phase-1 conductor (a later packet) composes; nothing here talks to Pi, a
socket or a CLI yet.

- `log.ts`: `EventLog` — an append-only `events.jsonl`, fsynced after every
  append, with `intent`/`completion` helpers keyed by an action ID and a
  `readLog`/`pendingIntents` pair for recovery (design §9.2–9.3). `readLog`
  tolerates only a torn final line (a crash mid-`fsyncSync`); any other
  corruption throws `LogCorruptionError`.
- `lock.ts`: `acquireLock`/`Lock.release` — `conductor.lock` held by a
  spawned perl helper using real `flock(2)` (`LOCK_EX | LOCK_NB`), since
  neither `flock(1)` nor `timeout(1)` is installed here but system perl is.
  The helper blocks reading its own stdin and exits (releasing the lock) on
  EOF, so the lock is released automatically if the conductor process dies
  for any reason — no explicit cleanup path needed on that side. A second
  `acquireLock` fails fast, naming the lock path and (when known) the
  holder's pid.
- `shell.ts`: `runCommand`/`killGroup` — every command the conductor runs is
  spawned in its own process group and stopped (`SIGSTOP`) before it execs
  the real command, so `onIntent({pgid})` can be awaited (and logged)
  *before* the command's first side effect (design §2.2), then resumed with
  `SIGCONT`. Deadline/`cancel()` escalate `SIGTERM` → (after `termGraceMs`,
  default 10s) `SIGKILL` on the whole group.
- `sweep.ts`: `sweep(dir, {exceptPids})` — `lsof +D <dir> -F p` to find
  processes that escaped their recorded group (e.g. via `setsid`), kill
  them, and report `tainted: true` when any were found. An empty sweep does
  not prove none survived (see the module comment).
- `git.ts`: worktree creation/removal, `freezeCommit` (with the
  `TT-Action` trailer) and `findCommitByTrailer`, read-only
  `materializeCandidate` and disposable `disposableCheckout` (both plain
  clones, not `git worktree`s, so a reviewer's checkout has no path back
  into the worker's live worktree), and the integration
  `probe`/`publishCAS`/`discardProbe` steps (design §6.2, §6.4).
  `verifyIntegrity(repo, dir, sha)` and `treeHashOf(repo, dir, sha)`
  deliberately never consult `dir`'s own `.git`: they seed a scratch index
  from the *candidate's* tree (`git read-tree <sha>`, objects resolved via
  `GIT_ALTERNATE_OBJECT_DIRECTORIES` into `repo`) and then `git add -A`
  over `dir`'s actual on-disk content, so a spoofed HEAD, an
  `update-index --assume-unchanged` bit, or a same-size/mtime-restored
  edit in the checkout's own index cannot pass — none of that state is
  read. A `.gitignore` added or edited inside the checkout still cannot
  hide a change to an already-tracked file (git only ever applies ignore
  rules to untracked paths); it can still hide a genuinely *new* untracked
  file, which is documented as a real, cooperative-worker-only limit
  (design §2.2: detection, not containment).

Tests live under `test/effects/`, use real processes and real disposable
git repos in `fs.mkdtempSync` directories (no mocks, no `Pi`), and clean up
after themselves.

## Phase 1b: the conductor daemon

`src/conductor.ts` is the phase-1 conductor: `events.jsonl` (the control
log) is folded through core's `reduce`/`next` to get the current `State`;
`Conductor.drive()` calls `next(state)` and dispatches every outstanding
action by composing the phase-1a effect adapters. `src/cli.ts` (`bin/tt`)
is the thin CLI: `tt start <plan.json>` launches the conductor as a
detached daemon; `tt status <run>` rebuilds and renders its state.

Two structural rules this packet enforces, both round-of-review fixes over
an earlier pass at this same packet:

- **No state snapshots in the log** (design §9.2). `events.jsonl` holds
  transitions, intents, completions and applied commands only — never a
  full `State` dump. Both `Conductor.start()` and `tt status` (via the
  exported `rebuildState`) get the current state the same way: fold every
  logged `"event"` record through `reduce()`, starting from the initial
  state derived from the plan's phase 0 and a one-time `"init"` record (the
  run id and the integration head observed at the run's first start).
  Conductor-only facts recovery needs (a worktree reset, a sweep's
  `killed`/`tainted` result, a check's `integrityViolated` flag) are
  recorded as intent/completion payloads, not as state dumps.
- **Freeze goes through intent/completion like every other effect**
  (design §6.2, §9.3). An accepted `submit_phase` logs `SUBMIT_PHASE` with
  the *raw* disclosure (see the core addition below) and returns the tool
  result; `next()` then recommends `freeze`, and the conductor logs
  `ACTION_STARTED` before running §6.2's sequence in order (abort, wait
  settled, end the worker's process group, sweep, commit with the
  `TT-Action` trailer, materialize a read-only checkout) and only then
  assembles the pending disclosures into bound `Decision` records —
  validated against `schemas/decision.schema.json` — and logs
  `FREEZE_COMPLETED` with them and the sweep's `tainted` result.

**Core additions (pure, additive — no existing row's semantics changed):**

- `SUBMIT_PHASE` now carries `disclosures: DecisionDisclosure[]` (the raw,
  unbound plain-language fields) instead of `decisions: Decision[]`;
  `phase.pendingDisclosures` holds them until `FREEZE_COMPLETED` — which
  now carries `decisions: Decision[]` (assembled + bound by the conductor)
  and an optional `tainted?: boolean` — assembles and binds them. See
  `DecisionDisclosure` in `src/core/types.ts`.
- A tool-set mismatch (design §2.1: "a mismatch is a launch failure, not a
  warning") is its own core event, `LAUNCH_FAILED`, with new transition
  rows `IMPLEMENTING -> BLOCKED` and `REVIEWING -> BLOCKED` carrying the
  expected/missing/extra tool sets in `blockedReason` — no repair round is
  spent retrying a launch that cannot succeed.
- Worktree tainting (design §2.2: "if the sweep found survivors, the
  worktree is tainted") is now driven by the *actual* sweep result at
  freeze time (`FREEZE_COMPLETED`'s `tainted` field), not only by the
  freeze-timeout row; the next worker attempt resets the worktree to a
  fresh checkout of the last candidate when tainted (`Conductor
  .#runWorkerAttempt`).
- **`INTEGRITY_VIOLATED { stage, evidence? }`** (design §2.2: "a mismatch
  ... marks the run integrity-violated for the owner") is a new,
  record-only core event (handled by `reduce.ts`'s `applyRecordEvent`, like
  `BALLOT_CAST` — it never changes the phase's own state name, so it needed
  no new `transitions.ts` row) that sets `phase.integrityViolated = true`.
  This is this packet's own item-3 addition: it makes the flag a *logged*
  fact that survives `rebuildState`, replacing an earlier, in-memory-only
  version of the same flag. See "Item 3" below for the full detail and the
  conductor side (`Conductor#runChecks`) that emits it.

Other fixes worth knowing about, all in `src/effects/pi-rpc.ts` and
`src/conductor.ts`:

- Every `Promise.race([..., someDeadline])` in the conductor now uses a
  cancelable timer (`cancelableTimeout`) instead of a bare `setTimeout`-
  backed promise with nothing clearing it — the leftover timer (up to
  design §8.1's multi-hour `workerAttemptMs`/`reviewMs` defaults) was why a
  finished test's `node --test` process stayed alive well past the test's
  own pass line.
- `PiAgent.terminate()` is now idempotent/memoized: `Conductor.stop()` and
  the attempt/review dispatch that owns an agent can both legitimately race
  to terminate it (e.g. the last reviewer's submission both unblocks its
  own dispatch and, via acceptance/publish, reaches DONE and auto-stops the
  whole conductor); before this fix each caller drove its own independent
  abort/SIGTERM/SIGKILL sequence, which was a real (if usually short) hang.
- `Conductor.stop()` also kills every agent's recorded `sh` process groups
  (design §2.2's "conductor-owned shell": a command's process group is
  separate from its agent's), and `#applyEvent`/`#sweepAndClear` no-op once
  `stop()` has started, so a straggling background dispatch never writes to
  an already-closed log.
- A phase reaching `DONE` or `BLOCKED` now makes the conductor call
  `stop()` on itself (`#maybeAutoStop`), which is what lets a `tt start`
  daemon process exit on its own once the run is over.  `AWAITING_OWNER`
  and the run-budget-paused state deliberately do **not** auto-stop (phase
  2 needs the conductor listening for owner commands while parked there),
  so a test ending in one of those states must call `stop()` itself.

**Exit-gate tests added this packet** (`test/conductor/`):
`process-exit.test.ts` (a conductor spawned as a real OS process for a
fake-pi happy path exits on its own shortly after DONE — the literal
"the test process hangs" regression check) and `force-kill-shell.test.ts`
(SIGKILLing the worker's own process group mid-`sh`: the conductor
notices, the recorded shell group and the sweep's own findings are both
clean afterwards, the attempt is recorded interrupted, and a previously
frozen candidate checkout elsewhere is untouched). `test/effects/
socket-no-submission.test.ts` unit-tests the `no_submission` protocol
message directly against `RunSocketServer` and documents `submit_discovery`
as an accepted no-op for phase 1's stub reviews.

**Every §8.1 stage now has a wired deadline** (`Deadlines` in
`src/conductor.ts`, all overridable per run; `runBudgetMs`/`runBudgetTokens`/
`tokenCapPerAttempt` unset means unbounded):

- worker attempt (`workerAttemptMs`) and per-attempt token cap
  (`tokenCapPerAttempt`, read from `message_update`'s `usage` payload) both
  cancel the attempt and consume a repair round the same way
  (`ATTEMPT_TIMED_OUT`);
- settle reminders are the *extension's* own job (`extension/
  tradeoffs-trace.ts`'s `agent_before_settle` handler, from an earlier
  packet) — the conductor treats its `no_submission` message, or a plain
  `agent_settled` with no accepted submission, the same way
  (`ATTEMPT_NO_SUBMISSION`);
- each worker `sh` command (`shCommandMs`) was already enforced by
  `effects/shell.ts`'s own `deadlineMs`/`killGroup`, via the run socket;
- freeze (`freezeMs`) now bounds the *whole* quiesce-sweep-commit sequence,
  not just the settle wait: on expiry, `#runFreeze` force-kills the worker,
  sweeps, taints the worktree, and emits `FREEZE_TIMED_OUT` — no candidate
  is produced by a timed-out freeze;
- each check command (`checkMs`) and each integration-probe command
  (`probeMs`) were already killed on expiry by `runCommand`'s own
  `deadlineMs`; their completion records now also carry `reason: "timeout"`
  (checks) or an evidence string containing `"timeout"` (the probe), so a
  timeout is distinguishable from an ordinary failure in the log;
- each review (`reviewMs`) was already cancel-then-redispatch-once-then-
  `BLOCKED: reviewer unavailable`, decided by `reduce.ts`'s own
  `timedOutOnce` flag, not by the conductor;
- repair rounds exhausted was already core-side (`enterAwaitingOwner` /
  `owner-requests.ts`'s `repair_budget_exhausted` origin) — the conductor
  path already reaches it, confirmed by this packet's own test;
- the run execution budget (`runBudgetMs` wall-clock, `runBudgetTokens`
  total tokens) now actually pauses/resumes: `Conductor#syncBudgetTimer`
  clears the wall-clock timer (preserving whatever is left) whenever the
  phase is `AWAITING_OWNER` or the run is already `RUN_PAUSED_BUDGET`, and
  only re-arms it for the remainder otherwise — "AWAITING_OWNER consumes
  nothing" is asserted directly by `deadline-every-stage`'s own sub-test.

`test/conductor/deadline-every-stage.test.ts` is the named exit-gate test:
one sub-test per bullet above, each forcing that stage past its deadline
and asserting the resulting state/log event plus (via a unique
`--tt-marker=<id>` argv token on every fake-pi process, or a unique marker
string embedded in every `sh`/check/probe command line, and `pgrep -f` on
it in the sub-test's own `finally`) that no orphan process survives it.

**A real bug this packet's own `freeze-e2e` test caught and fixed**: once a
worker's `submit_phase` was accepted, `Conductor` used to remove that
worker's handle from its live-agents map immediately (the attempt itself is
"done" at that point) — but design §6.2 step 1 has the worker's *process*
still alive (and, as `freeze-e2e.test.ts` demonstrates, still able to call
`sh`) until the freeze's own `abort` actually lands. With the handle already
gone, `cwdFor`/`onShIntent` had nothing to look up, so a post-submission `sh`
call silently ran with no recorded cwd (defaulting to the *conductor's own*
process cwd, not the worktree) and no recorded pgid at all — invisible to
the freeze's own sweep. The handle now stays registered through that
quiesce window; `#runFreeze` itself removes it once quiesced.

**Item 3 — `integrityViolated` is now a logged fact, not in-memory only**
(`src/core/types.ts`/`reduce.ts`/`schemas/event.schema.json`): a new,
additive core event, `INTEGRITY_VIOLATED { stage, evidence? }`, handled by
`reduce.ts`'s `applyRecordEvent` (a record-only event, like `BALLOT_CAST` —
no phase-state-name change, so no new `transitions.ts` row was needed or
added; see `test/unit/reduce.test.ts`'s own INTEGRITY_VIOLATED tests instead
of a `transitions.test.ts` row test). `Conductor#runChecks` emits it (before
the check's own `CHECKS_PASSED`/`CHECKS_FAILED`) whenever the before/after
`verifyIntegrity` comparison around a check command disagrees — the
affected check's own result was already forced to "not passed" by that same
`!after` condition; this just makes `phase.integrityViolated` survive a
conductor restart via `rebuildState` folding the log, which an in-memory-
only flag (this packet's first pass) did not.
`test/conductor/integrity-violated.test.ts` is the named exit-gate test: a
second check command acts as the test hook, tampering with its own
disposable checkout mid-run; the run reaches `AWAITING_OWNER` with
`integrityViolated: true`, and `rebuildState` from `events.jsonl` alone
reproduces the same flag afterwards.

**Item 4 — `test/conductor/freeze-e2e.test.ts`**: a fake-pi worker calls
`submit_phase` and then launches a background shell loop that keeps
appending to a file in the live worktree, never stopping on its own. The
freeze's own sweep is the only thing that can end it; the test asserts a
`sweep` log record actually found and killed it, `FREEZE_COMPLETED` reports
`tainted: true`, the writer process is provably dead afterwards, and that
neither the materialized candidate checkout nor a fresh disposable checkout
of the same candidate (what a real check gets) ever reflects a write made
to the live worktree after the run finished — they are independent git
clones of a fixed commit, never a view onto the live worktree.

**Item 5 — `test/conductor/tainted-reset.test.ts`**: attempt 1's worker
writes a normal file, launches the same kind of never-stopping survivor
writer, then submits; its freeze reports `tainted: true`. `plan.checks` is
chosen so that candidate doesn't yet pass, consuming a repair round.
Attempt 2's own script checks, from *inside the freshly reset worktree, as
the very first thing it does*, that attempt 1's file is present and `git
status --porcelain` is empty — i.e. the worktree is exactly a clean
checkout of the last candidate, with the survivor's own further writes
gone — before it does anything else.

**Item 6 — `tt start` with fake-pi**: `src/cli.ts`'s `testPiInjection()` is
the documented test-only injection point: refused unless `TT_TEST_MODE=1`
is set (never the default), it reads `TT_TEST_PI_COMMAND`/
`TT_TEST_PI_ARGS_PREFIX` (a JSON array) and passes them straight through to
`Conductor`'s own `piCommand`/`piArgsPrefix`, inherited by the detached
daemon process from whatever set them before calling `tt start` (`cmdStart`'s
`spawn` does not override `env`). Since a `tt start`-launched conductor has
no `piEnvFor` hook to give the worker and each reviewer their own script,
two small, additive `test/fake-pi/fake-pi.ts` extensions cover that: (a) if
`FAKE_PI_SCRIPT` names a *directory* rather than a file, it reads
`<dir>/<TT_ROLE>.json` (`TT_ROLE` is always set by the conductor at spawn
time), so one shared env var can still give the worker and reviewers
different scripts; (b) an explicit, small allowlist of `"$TT_..."` string
tokens inside a `call-submit` step's `args` (currently `$TT_CANDIDATE_SHA`
and `$TT_REVIEWER`) is substituted from the conductor's own env at spawn
time (`Conductor#runReview` now sets both), so a reviewer script can report
the live candidate sha and which of M/A/B it is without being regenerated
per dispatch the way the in-process test harness's `piEnvFor` callback does
it. `test/conductor/tt-start-happy-path.test.ts` is the named exit-gate
test: `bin/tt start <plan.json> --root /tmp/tt-…` returns a run id
immediately, the detached daemon (driven purely through this injection
point, using real `DEFAULT_DEADLINES` — so this genuinely takes about a
minute, dominated by the freeze's default 30s `abortGraceMs`) reaches DONE
with the integration branch moved by CAS, the daemon process exits on its
own, and `bin/tt status <run>` shows DONE with the candidate and the
published `I`.

## Phase 1c: crash recovery, speed, and a live single-phase run

Phase 1b's own "known gaps" left design §9.3's crash-recovery reconciliation
(`Conductor#reconcileWorktree`/`#reconcileInFlight`/`#reconcileOne`) and its
`test/crash/crash-suite.test.ts` exit-gate test to this packet. That test
and the reconciliation code it exercises were both already written by the
time this packet picked the work back up; two of its 12 boundaries were
still failing, and the whole suite took ~12 minutes.

**Two real bugs, both in the reconciliation, not in the test's own
correctness assertions:**

- **`before_publish_cas`** crashed the *recovery* run itself: design §9.3's
  "still at H ⇒ retry CAS" row re-emitted a second `ACTION_STARTED` for
  `publish_cas` under a fresh action id, but the phase's own `inFlight` map
  still held the *original*, never-cleared `publish_cas` entry (`PUBLISHING`
  has no "retry in place" core event the way `FREEZING`'s
  `FREEZE_INTERRUPTED` does, so nothing had cleared it) — `reduce()`
  correctly rejected the second `ACTION_STARTED` as "not currently
  outstanding", which crashed the *recovery* conductor process outright.
  Fixed by not re-emitting `ACTION_STARTED` for the retry at all:
  `#reconcileOne`'s `publish_cas` case now just re-runs `#runPublish`
  directly (under a fresh actionId, for its own intent/completion log
  identity) — the original dispatch's `inFlight` entry is still exactly
  correct for it, and the eventual `PUBLISH_COMPLETED`/`PUBLISH_STALE`
  clears it by key, not by actionId.
- **`after_dispatch_worker`**'s own test expectation was wrong, not the
  conductor: this suite's fake-pi worker always calls `submit_phase`, so the
  worker-attempt race always resolves `"submitted"` — and `#onSubmit`'s
  `SUBMIT_PHASE` (which clears `dispatch_worker` from `inFlight` and starts
  `freeze`, adding *its own* `inFlight` entry) fires and is fsynced
  synchronously, *before* this boundary's `crashAt()` call. So by the time
  the process dies, what is actually left dangling is the `freeze` that had
  just started, not the worker attempt (already finished). The recovered
  log correctly shows `FREEZE_INTERRUPTED`, the same as `before_freeze` —
  the test's `expectInterrupted` table was asserting the wrong marker for
  this one boundary; fixed there, with the actual mechanism spelled out in
  the test's own comment.

**Speed** (the other reason `after_dispatch_worker`/`before_publish_cas`
took ~35s each and the full suite ~12 minutes): every crashed/recovery
process pair was running at **production** deadlines, in particular the
default 30s `abortGraceMs` — spent in full on every single freeze, because
fake-pi correctly mirrors a real Pi process that has no more work to do
(replies to the RPC `abort` but never exits on its own the way a
`hang-until-abort` script step does), so `PiAgent#terminate()` always burned
the whole grace period before its own `SIGTERM` landed. `src/cli.ts` gained
`testDeadlines()` — the same `TT_TEST_MODE=1` gate as the existing fake-pi
injection, reading `TT_TEST_DEADLINES` (a JSON `Partial<Deadlines>`) and
passing it to the detached conductor's own `Conductor` construction — and
`crash-suite.test.ts` now sets it to millisecond-scale values
(`FAST_DEADLINES`) for every process it spawns. A single boundary's
crashed+recovery pair now takes on the order of a second instead of ~35-65s.

Even at that speed, `make check` keeps only a **3-boundary fast subset**
(`after_freeze`, `before_publish_cas`, `after_run_checks` —
`FAST_SUBSET`/`BOUNDARIES_TO_RUN` in the test file), selected by whether
`TT_CRASH_FULL=1` is set; the full 12-boundary sweep is `make crash`
(`TT_CRASH_FULL=1`). Measured on this machine: `make check`'s fast subset
run in this file, ~18s; the full `make crash` sweep, ~1 minute; the whole
`make check` suite (317 tests total), ~90s.

**`test/live/live-single-phase.test.ts`** (`make live`, opt-in `TT_LIVE=1`):
a **real** `pi` worker (`vercel-ai-gateway`, `deepseek/deepseek-v4.1-flash`)
driven by this packet's own `Conductor`, with fake-pi stub reviewers for
M/A/B, against a disposable `/tmp/tt-live-*` git repo holding a tiny
two-file Node project (`sum.js` + `test.js`, `node:test`, no
dependencies). The single phase: add a `subtract` function to `sum.js` with
a test; CHECKS is `node --test`. Mixing a real worker with fake reviewers
in one run needed `ConductorOptions.piCommandFor`/`piArgsPrefixFor`/
`providerModelFor` (phase 1b had added the first two, and the option shape
for the third, but never actually wired `providerModelFor` into either
`launchArgs(...)` call — a real gap this packet closed, since without it a
live run had no way to pick a real provider/model at all) rather than `tt
start`, which has only one flat `piCommand` for every role. The test asserts,
in order: no tool-set mismatch (`LAUNCH_FAILED` never fires — a real
mismatch would prevent `submit_phase` from ever being reachable, so this is
provable directly from the event log), `submit_phase` accepted, the
candidate frozen, `node --test` passing on it, the probe passing, all three
stub reviews submitted, acceptance, and publish (CAS) — reaching `DONE`,
bounded at 15 minutes (never faking success past that: the test fails with
the run's own log tail on timeout), and no leftover `pi` process (every pgid
`Conductor#agentPgids` — a new read-only getter — ever reported for this run
is checked dead afterwards). It records one JSON evidence file (Pi version,
provider/model, event types in order, candidate sha, published `I`, check
outcome, worker tool-call counts and last reported token/cost usage — read
from the worker's own raw RPC stream file under `<run>/stream/`, since
`Conductor` itself does not expose that; a new `Conductor#agentTokenTotals`
getter exposes the cumulative total it does track — duration, outcome) plus
a relativized copy of the run's own `events.jsonl`, both under
`test/live/records/phase-1/`.

**Known gaps, explicitly not done in this packet** (noted here so they are
not mistaken for oversights): the real Pi version assertion
(`assertPiVersion` in `src/conductor.ts`) now *is* exercised against a real
`pi` binary (`live-single-phase` uses a real worker, which triggers it) but
only implicitly — there is no dedicated test asserting what happens on a
genuine version mismatch; the integration probe's own `git merge` step has
no deadline of its own (only its per-check commands do, matching design
§8.1's "as for checks, per command"); design §2.2's integrity verification
is only actually wired around checks (`Conductor#runChecks`) — it is not
yet also run before/after each review, so a review-time integrity
violation is not yet detected the same way; the reproduction-command
deadline (§8.1's `reproductionMs`) has no conductor code path to attach to
yet (phase 1 has no reproduction step at all); `live-single-phase.test.ts`
exercises exactly one worker attempt with no repair round and no owner
request — a real model needing a second attempt, or the phase reaching
`AWAITING_OWNER`, is not covered by any live test yet.

**R2 repair — effective `CHECKS` and nested-test isolation.** Two
regression fixes, with tests that fail on the unfixed behavior. (F04) The
check loops in `Conductor#runChecks` (candidate C) and `Conductor#runProbe`
(probed integration I) each iterated the global plan checks only, silently
ignoring the phase contract's own `:CHECKS:`. They now resolve one effective
list — global commands first, then the phase's own, with exact duplicate
command strings dropped at their later occurrence (no splitting,
normalization or reordering) — via the pure helper `src/core/checks.ts`'s
`effectiveChecks`, so both gates execute the same list. Each executed
command's evidence is recorded per gate: the C path keeps
`<checks>/<candidateSha>/<sanitized command>.log`, and the probe now writes
the same shape under `<checks>/probe/<probedI>/`. (F13) A check (or probe or
worker `sh`) command that itself runs `node --test` used to inherit the
Node test runner's own `NODE_TEST_CONTEXT`/`NODE_TEST_WORKER_ID` when the
conductor was launched under `node --test`; the child then printed
`node:test run() is being called recursively within a test file. skipping
running files.` and exited 0, recording a real failure as a pass.
`effects/shell.ts`'s `childEnv` removes exactly those two test-runner-only
markers (PATH, HOME, `NODE_OPTIONS`, provider/credential and `TT_*` all
survive) and is applied to checks, the probe and the worker `sh` path;
there is still no reproduction-command path in phase 1 to adapt.
`test/conductor/effective-checks.test.ts` is the F04 gate (a failing
phase-only check fails and never accepts; a failing global check fails; the
dedup rule proven by a real once-per-gate side effect; a phase check that
passes on C but fails on I yields `PROBE_FAILED` and blocks publication,
with a DONE control). Its F13 sub-tests drive a conductor whose check runs
`node --test` over a deliberately failing (and, separately, passing) test
and assert the log shows the real test name and `fail 1`/`pass 1` with no
recursion warning; `test/effects/shell.test.ts` additionally invokes the
real Node binary with and without `childEnv` to show the skip-vs-run
difference. `test/live/published-behavior.ts` is a shared verifier that
runs the feature's real API (`subtract` on positive/negative/zero inputs,
and `sum` still working) on a fresh checkout of the published I — wired
into `live-single-phase.test.ts` before its fixtures are deleted, with a
normal-suite test proving it accepts a correct candidate and rejects a
no-op one.

## Running the tests

```sh
make check
# or, equivalently, from anywhere:
node --test 'tradeoffs-trace/test/**/*.test.ts'

# the full crash-suite (every TT_CRASH_AT boundary, not just make check's
# 3-boundary fast subset):
make crash

# opt-in live smoke tests (real Pi + real model + real network + real
# provider credentials):
make live
```

Tests use `node:test` + `node:assert/strict`. There is no `tsc`; correctness
is proven by tests, not by a type-checker pass. `make check` requires `pi`
(pinned to the version in `src/core/roles.ts`'s `PI_VERSION`) on `PATH` for
`role-tool-sets.test.ts`, but sends it no prompt and makes no model call —
it takes ~90s. `make live` runs both `test/live/live-submission.test.ts`
(phase 0) and `test/live/live-single-phase.test.ts` (phase 1c); each is a
no-op printing why it skipped unless `TT_LIVE=1` is set (`make live` sets
it). Neither is part of `make check` — both cost real time, tokens and
provider spend.
