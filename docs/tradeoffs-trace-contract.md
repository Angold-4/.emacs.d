# tradeoffs-trace contract

**Contract version 1.** This document is the interface between the runtime
and every front end (Emacs today, anything else later). It defines the
runtime's states and events; the runtime and each front end build against
this document alone.

The normative parts are the JSON shapes, the state machine and the carry
rule. Where this document and the code disagree, the code's own fixture
tests (`test/contract/messages.test.ts`, `test/conductor/contract.test.ts`)
are the tie-breaker and the document is corrected.

## 0. One authoritative history

`events.jsonl` is the **one** authoritative history of a run. State is always
rebuilt by folding its `"event"` records through `reduce()`; nothing else is
a source of truth. Every message, verdict, settlement and carry is an event
in that file.

Everything else under a run directory is a **projection** rebuilt from state:

| File | Contents |
| --- | --- |
| `events.jsonl` | the authoritative history (events, intents, completions, applied commands) |
| `messages.jsonl` | one current message per line, id order (`projectMessages`) |
| `ledger.jsonl` | one settled entry per line, id order (`projectLedger`) |
| `views/review.org` | the runtime-rendered review view (`projectReview`, `src/render.ts`) |
| `views/messages/<id>.org` | one message's evidence, plan excerpt, history, ledger and votes (`renderMessageFile`) |
| `views/status.txt` | the status buffer's own text (`renderStatusView`), with trade-off record markers |
| `views/tape.txt` | the current round as a vertical tape (`renderLoopTape`), drawn from the declared `MAIN_PATH` and refreshed with `views/status.txt` |
| `views/loop.txt` | the phase state machine as a chart (`renderPhaseChart`), drawn from `TRANSITIONS` and refreshed with `views/status.txt` |
| `views/metrics.json` | the balance metrics (`computeMetrics`, `projectMetrics`), a deterministic projection of state, the timeline and the control log; `tt summary` renders the same numbers |

Projections are written when an event can have changed them (any `MESSAGE_*`
or `OWNER_VERDICT` event) and again on every conductor start. Because
messages only change through those events, a mid-run `tt contract check` sees
current projections; and a conductor killed between an event and its
projection write rebuilds them on the next start — the log is the only
authority. `views/metrics.json` has no state-only write path of its own (it
re-times the timeline), so it is refreshed on every status beat and on stop;
a mid-run `tt contract check` may lag it by up to a second.

`tt contract rebuild <run>` rewrites the projections (including one file per
message, `views/metrics.json` and `views/tape.txt`) from `events.jsonl`.
`tt contract check <run>` compares them to state and exits non-zero with the
mismatching file names when they differ. `views/status.txt` is time-dependent
and is generated, not compared; `views/metrics.json` and `views/tape.txt` are
deterministic (every duration ends at the last event timestamp in the log,
never at a wall clock), so they are compared like the other projections.

## 1. Messages and the message state machine

A **message** is a trade-off, a finding or a blocker. Ids are `T-n`, `F-n`
and `B-n`, unique within a run. The lifecycle is a transition table,
`MESSAGE_TRANSITIONS` in `src/core/messages.ts`, with the same discipline as
`TRANSITIONS`: every row has a test fixture, and `reduce()` rejects any event
with no matching row. The same states and rows as a chart, drawn by the
generator (`renderMessageChart`, `src/charts.ts`), so this document and the
runbook quote exactly what the code says:

<!-- BEGIN message-chart (generated from MESSAGE_TRANSITIONS; do not edit by hand) -->
```text
tradeoffs-trace message chart — generated from MESSAGE_TRANSITIONS (src/core/messages.ts); do not edit

  +------------+
  | none       |
  +------------+
      +- MESSAGE_RAISED                 -> raw         [message-raised]

  +------------+
  | raw        |
  +------------+
      +- MESSAGE_PUBLISHED              -> published   [message-published]
      +- MESSAGE_MERGED                 -> merged      [message-merged]
      +- MESSAGE_DROPPED                -> dropped     [message-dropped]

  +------------+
  | published  |
  +------------+
      +- MESSAGE_DROPPED                -> dropped     [message-dropped-published]
      +- OWNER_VERDICT (verdictAccept)  -> accepted    [owner-verdict-accept]
      +- OWNER_VERDICT (verdictRefuse)  -> refused     [owner-verdict-refuse]
      +- MESSAGE_SUPERSEDED             -> superseded  [message-superseded-published]
      +- MESSAGE_RESOLVED               -> resolved    [message-resolved-published]

  +------------+
  | merged     |
  +------------+
      +- MESSAGE_SUPERSEDED             -> superseded  [message-superseded-merged]

  +------------+
  | dropped    |
  +------------+
      +- MESSAGE_SUPERSEDED             -> superseded  [message-superseded-dropped]

  +------------+
  | accepted   |
  +------------+
      +- MESSAGE_SUPERSEDED             -> superseded  [message-superseded-accepted]

  +------------+
  | refused    |
  +------------+
      +- MESSAGE_SUPERSEDED             -> superseded  [message-superseded-refused]
      +- MESSAGE_RESOLVED               -> resolved    [message-resolved-refused]

  +------------+
  | resolved   |
  +------------+

  +------------+
  | superseded |
  +------------+
```
<!-- END message-chart -->

```
raw       → published | merged | dropped
published → accepted | refused | superseded | resolved
refused   → resolved | superseded
accepted | merged | dropped → superseded   (a settled message whose backing
                                            record went away)
```

Every row:

| id | from | trigger | guard | to |
| --- | --- | --- | --- | --- |
| `message-raised` | (none) | `MESSAGE_RAISED` | no message with that id exists | `raw` |
| `message-published` | `raw` | `MESSAGE_PUBLISHED` | always | `published` |
| `message-merged` | `raw` | `MESSAGE_MERGED` | always | `merged` |
| `message-dropped` | `raw` | `MESSAGE_DROPPED` | always | `dropped` |
| `message-dropped-published` | `published` | `MESSAGE_DROPPED` | always | `dropped` |
| `owner-verdict-accept` | `published` | `OWNER_VERDICT` | `verdict === "accept"` | `accepted` |
| `owner-verdict-refuse` | `published` | `OWNER_VERDICT` | `verdict === "refuse"` | `refused` |
| `message-superseded-published` | `published` | `MESSAGE_SUPERSEDED` | always | `superseded` |
| `message-resolved-published` | `published` | `MESSAGE_RESOLVED` | always | `resolved` |
| `message-superseded-refused` | `refused` | `MESSAGE_SUPERSEDED` | always | `superseded` |
| `message-resolved-refused` | `refused` | `MESSAGE_RESOLVED` | always | `resolved` |
| `message-superseded-accepted` | `accepted` | `MESSAGE_SUPERSEDED` | always | `superseded` |
| `message-superseded-merged` | `merged` | `MESSAGE_SUPERSEDED` | always | `superseded` |
| `message-superseded-dropped` | `dropped` | `MESSAGE_SUPERSEDED` | always | `superseded` |

A supersede never clears a settlement: a settled message that is superseded
keeps its settlement in the ledger, marked `supersededBy`. A message whose
backing decision was withdrawn or not carried forward is superseded at the
next freeze, so a withdrawn record's settlement can never survive as active
(`MESSAGE_CARRIED` only ever goes to a live message).

An event with no row is rejected with a visible reason. `OWNER_VERDICT` on a
`dropped` message is rejected (`no rule from message …'s state 'dropped'`);
`OWNER_VERDICT` on a `raw`, pre-freeze message is rejected with
`… is not yet frozen; it must be published before a verdict`.

The events, all reduced by `reduce()`:

- `MESSAGE_RAISED { message }` — raises the message as `raw`.
- `MESSAGE_PUBLISHED { messageId, boundCandidateSha, boundContractVersion, boundRecordVersion }`
- `MESSAGE_MERGED { messageId, by, reason?, …binding }`
- `MESSAGE_DROPPED { messageId, by, reason?, …binding }` — from `raw` (an evaluator) or from `published` (the round panel's non-`keep` majority)
- `OWNER_VERDICT { messageId, verdict: "accept" | "refuse", reason?, …binding }`
- `MESSAGE_RESOLVED { messageId, by, reason?, …binding }`
- `MESSAGE_SUPERSEDED { messageId, reason?, …binding }`
- `ROUND_STARTED { round, base, lanes }` — plan 06g: a round of a phase with
  `#+TT_WORKERS` began, from one base, with its lane ids (`a`, `b`).
- `CANDIDATE_SUBMITTED { round, lane, sha }` — one lane froze a candidate;
  views name it `C<round>-<lane>`.
- `CANDIDATE_CHECKED { round, lane, ok }` — one lane's checks ran. A lane that
  crashed, timed out or submitted nothing records no candidate at all.
- `PICK_VOTE { round, seat, lane, why }` — one seat's vote in the round's pick
  turn, with its one-line reason. A later vote from the same seat replaces it.
- `CANDIDATE_PICKED { round, lane, sha, votes }` — the round's winner, decided
  by `pickWinner` in code (never by a model), with the votes it took (`0` when
  it was the only passing candidate and won without a vote).
- `ROUND_REVIEW_SUBMITTED { round, lane, seat, review }` — plan 06g2: one
  seat's review of one lane candidate, recorded while the round runs (the phase
  is still IMPLEMENTING, so it has no review slot to hold it). Record-only, like
  the five above; it is the round's own per-candidate review record, which the
  review buffer groups by candidate. The winner's reviews are promoted into the
  phase's review slots at hand-off (`REVIEW_SUBMITTED` with `promotedFrom`), so
  acceptance reads the winner's reviews and nothing else.
- `ITEM_CARRIED { recordId, recordKind?, toPhase, …binding }` — plan 06g
  (A5): the owner carried one finding (or message) to a later phase's plan
  (`tt carry <run> <id> --to <phase-id>`). It marks the item carried (with its
  target), answers the open owner request about it (and the plain budget gate),
  and once no blocking item remains uncarried accepts the candidate
  (`acceptedWithCarried`). `owner-commands.ts` is the only place that decides
  what a carry does.

All six round events are **record-only**: they are reduced into
`phase.rounds` (the views and `tt summary` read them) and move no phase state.
All six are absent from a run without `#+TT_WORKERS`, whose log is
byte-identical to before this plan. `REVIEW_SUBMITTED` carries an extra
optional `candidate` field when a round reviewed more than one candidate, and
an optional `promotedFrom` naming the round whose winner's review it promotes
into the phase's slots (so a view counting a round's reviews counts it once).
- `MESSAGE_CARRIED { messageId, fromCandidate, toCandidate, fromVersion, toVersion, contentHash, unchanged, content? }`
  — `content` is present exactly when the reviewable content changed, so the
  stored fields and `contentHash` can never disagree.

`by` is one of `owner`, `evaluator`, `panel`, `vote`.

### Owner verdict side effects

- **`refuse` before `DONE`** (in any phase: `CHECKING`, `PROBING`,
  `REVIEWING`, `ACCEPTED`, …) appends an owner-authored **blocking** finding
  bound to the current candidate. Because `accept(C, K)` fails on any open
  blocking finding, the candidate cannot reach `ACCEPTED`; the run returns to
  repair or, when the budget is exhausted, parks on the owner.
- **`refuse` after `DONE`** is a **follow-up**: the message is marked
  `followUp` and no phase state changes.
- **`accept`** settles exactly the message version the command named.

### The vote rule per message type (plan 05e)

Which agent validates a message, and how many, depends on its type. Nothing
reaches the owner, and nothing blocks the worker, on one agent's word except
an advisory finding.

| Type | Validation | Votes | Effect |
| --- | --- | --- | --- |
| trade-off (`T-n`) | the round panel, unless the backing decision already carries valid M, A and B ballots (then no panel) | `keep` / `drop`, batched | a `keep` majority publishes it; otherwise `MESSAGE_DROPPED` from `published` (`by: panel`), with the seats' reasons on the message |
| advisory finding (`F-n`) | the `finding` evaluator, after the conductor's 3a/3b checks | none | published with `verified` evidence, or `MESSAGE_DROPPED` + `FINDING_DISPROVED` with the evaluator's reason |
| blocking finding (`F-n`) | the same round panel as the trade-offs, each seat seeing the 3a/3b evidence | `keep` / `drop` | blocks only with a 2-of-3 `keep`; otherwise `FINDING_SEVERITY_CHANGED` to `advisory` |
| blocker (`B-n`) | its own panel of three, per blocker (unchanged) | `block` / `downgrade` | `block` majority escalates to the owner; `downgrade` makes it an ordinary blocking finding |

One **round panel** per round: three seats, one batched
`ROUND_PANEL_VOTE` each, dispatched only after the evaluators have published,
inside `EVALUATING`. `ROUND_PANEL_DECIDED` stamps the outcome and every seat's
reason on each item message (`panelOutcome`, `panelVotes`).

Three deterministic checks precede any model:

- **3a, facts first:** the conductor compares a finding's claim with the
  check, probe and gate records it already holds for that candidate. A claim
  that a named check, test or command fails while the record shows it passing
  is rejected (`FINDING_DISPROVED` + `MESSAGE_DROPPED`, both citing the
  record) before any agent sees it; a claim the record confirms gets
  `verified: record (confirmed by record: …)`. Only a sentence that names the
  command AND carries a failure word is a claim about it. This runs for a
  blocker's finding too.
- **3b, run what can be run:** a finding naming a runnable test or command is
  re-run in the candidate's disposable checkout, bounded by the check
  deadline. Exit 0, or a timeout that reproduces nothing, drops it (`run <cmd>
  exit 0` / `run <cmd> timed out`); a non-zero exit records `verified: run
  <cmd> exit N`. A blocker's `runnable` is checked the same way.
- **severity against the plan (5):** a blocking finding that cites no
  acceptance item or reserved rule is lowered by the evaluator
  (`FINDING_SEVERITY_CHANGED`, by `evaluator`, with its reason). The GROUND
  decides, not the kind: an `integration` finding citing an acceptance item
  may block. A citation is the item verbatim, a phrasing-preserving
  paraphrase, a numbered reference, a reserved rule, an owner directive id,
  or a `criterionDispute`. A `sameAs` re-raise takes the re-raiser's severity
  **downward only**; one reviewer cannot raise an advisory finding to
  blocking.

`FINDING_VERIFIED` records what validated a finding — `record`, `run <cmd>
exit N`, a `file:line …` citation, or `panel keep` — and the renderer shows it
in the property drawer (`VERIFIED`) and in `views/messages/<id>.org`.
Validations accumulate: the 3a/3b evidence, the panel's vote and an
evaluator-supplied citation (`evaluator: <text>`, tagged because the conductor
did not check it) are joined rather than overwritten.

### Resolution across rounds (plan 05e)

Every later round's turn-2 prompt lists the earlier rounds' open findings and
blockers; each reviewer's `Review.resolutionStatements` marks each `resolved`
or `open` with evidence. A 2-of-3 `resolved` majority emits `MESSAGE_RESOLVED`
(`by: vote`) and `FINDING_RESOLVED_BY_VOTE` (the finding is repaired), so the
message leaves the owner's live view and no longer blocks. A 2-of-3 `open`
majority, or no majority, leaves it live. A trade-off may carry
`closes: <messageId>`; both message files render the link (`CLOSES` /
`FIXED_BY`).

### Approved code stays approved (plan 05e)

When all three reviewers review a candidate, every live decision bound to it
has settled, no owner request is open and no open blocking finding stands,
`CANDIDATE_APPROVED` records its git tree. It is emitted when the round's
`EVALUATING` has settled, so the round panel's severity decisions are already
final. A later round whose candidate ships the same tree (an amendment-only
resubmission) re-reviews only the amended criterion; a new blocking finding on
the unchanged code is raised as advisory, unless it cites an acceptance item
or a reserved rule. A point filed through a reviewer's `blockers` list on such
a round is raised as an ordinary finding message (advisory when it cites no
ground), so no blocker panel runs on unchanged approved code. A candidate
whose ballots were rejected is never approved, so an identical resubmission is
re-reviewed normally.

## 2. Bindings and `contentHash`

Every message carries its binding: `boundCandidateSha`, `boundContractVersion`
and `messageVersion` (the record version). Every verdict-like event carries
the same tuple plus `runId`/`phaseId` in the inbox command's `RecordBinding`.
A stale `candidateSha`, `contractVersion` or `messageVersion` is rejected with
a reason naming what changed.

A settlement is bound to the message's **content**, not its version number.
Every message version carries `contentHash` = sha256 of exactly

```
type, title, summary, context, evidence, planRef
```

so `contentHash` changes if and only if the reviewable content changes.

## 3. The settled ledger and carry

`ledger.jsonl` is derived, not stored: one entry for every message with a
terminal settlement (`accepted`, `refused`, `merged`, `dropped`, `resolved`),
with who settled it (`owner`, `evaluator`, `panel`, `vote`), the reason, and
the bindings it was settled under:

```json
{"messageId":"T-1","type":"tradeoff","state":"accepted","settledBy":"owner",
 "candidateSha":"…","contractVersion":{"snapshot":1,"sectionSha256":"…"},
 "messageVersion":1,"contentHash":"…"}
```

At every `FREEZE_COMPLETED` the conductor emits **one explicit
`MESSAGE_CARRIED` per live message**. The rule:

> A settlement carries to the new version exactly when `unchanged` is true
> (the same `contentHash`) **and** the contract version is the same.
> Otherwise the ledger entry is **kept** (who settled it and its bindings)
> and marked `invalidated` with the reason (`content changed` or `contract
> amended`); the message itself returns to `published` and needs a new
> verdict.

The conductor re-derives each message's content from the decision or finding
it came from at every freeze, so a decision the worker changed this round is
carried as `unchanged: false` with its new content — the content-changed
branch is reachable from a real run, not only from fixtures. An
already-invalidated settlement is never rebound by a later carry: the ledger
reports it under the candidate, version and contentHash it was actually
settled under.

Carrying bumps `messageVersion` and rebinds `boundCandidateSha`. The message
records every past `(candidate, version)` and each version's contentHash, so a
verdict naming the pre-carry version of an **unchanged** message is applied,
while the same verdict on a **changed** message is rejected.

## 4. The owner's verdict

The owner command is `verdict`:

```json
{ "kind": "verdict", "messageId": "T-1", "verdict": "accept",
  "reason": "…", "boundCandidateSha": "…",
  "boundContractVersion": {"snapshot":1,"sectionSha256":"…"}, "messageVersion": 1 }
```

The decision view's encoding writes the same command as `type: "verdict"` with
a full `RecordBinding` whose `recordId` is the message id and whose
`recordVersion` is the message version (`schemas/owner-command.schema.json`,
`$defs/Verdict` and `$defs/DVVerdict`).

The CLI is:

```
tt verdict <run-dir-or-id> <messageId> <accept|refuse> [--reason <text>] [--candidate-sha <sha>] [--message-version <n>] [--contract-version <n>] [--contract-sha256 <sha>] [--run-id <id>] [--phase-id <id>] [--root <dir>]
```

`--candidate-sha` and `--message-version` send a binding other than the
message's current one (what the owner saw before a carry); the same stale-
binding check rejects it.

- On a **live** run (a conductor is running) the command is written to
  `<run>/inbox/<id>.json`; the conductor validates the binding and a stale one
  lands in `inbox/rejected/` with the reason. The CLI then waits briefly for
  the outcome and prints it on stdout (`verdict applied: …`, or
  `verdict rejected: <reason>` with a non-zero exit); a command the conductor
  does not answer in time prints `(queued, not yet applied)`.
- On a run whose daemon has **exited** the command is a **late verdict**: the
  CLI dry-runs `reduce()` and, if it accepts, appends the `OWNER_VERDICT`
  event to `events.jsonl` and refreshes the projections. A stale or
  out-of-order verdict is refused, never written, and the reason is printed on
  stdout with a non-zero exit.

## 5. Rendering

- `messages.jsonl` and `ledger.jsonl` are the machine view.
- `views/review.org` is the human view. Its header names the run by its
  readable id and directory id (`cebd7fcb-01 · 33c41174`); the internal
  `runId` appears only inside the property drawers, where a verdict's binding
  needs it. Three top-level sections follow (Blockers, Trade-offs, Findings),
  blockers first. Only a message the evaluator **published** (and its later
  states, accepted/refused/resolved/superseded) is a titled entry: a raw
  message — or one an evaluator timeout left `unevaluated` with its raw
  wording — is one `N raw, awaiting evaluation` line per section, a dropped
  one only `N dropped`, and a merged one is named in its target's own file (under
  `* Merged in`). Each entry is one heading (`<id> <title>`) carrying its
  summary and context and a property drawer with its id, type, severity (for
  a finding), state, `raisedBy`, `importance`, the owner's verdict (if any)
  and the binding a verdict needs (`messageVersion`, `candidateSha`,
  `contractVersion`, `runId`, `phaseId`). Within a section, high and normal
  messages come first; low-importance ones are folded under `Minor (N)`.
  `* Blockers` holds only messages raised through a reviewer's `blockers`
  list, each with its panel's outcome; an ordinary `blocking` finding is a
  Finding, marked `[blocking]`. A message's own file,
  `views/messages/<id>.org`, carries its evidence (path and lines), the plan
  excerpt it concerns, what was merged into it, every version's history, its
  ledger entry and its votes.
- `views/status.txt` is the text the Emacs status buffer shows: the title, the run line, every status row, the trade-offs (each tagged `\t:RECORD:<id>` so RET still opens the decision view), the cost, the `review` row (the same counts `review.org` shows, in trade-off vocabulary: `T 6 (6 raw) · F 0 · B 0 · C-c m d`), the DONE owner checklist, the owner input and directives, and the attention line. `tt status` keeps its own plain-text rendering.
- `tt summary` (the PR body) lists refused-after-`DONE` follow-ups under
  `### Follow-ups` and the same balance numbers under `### Balance metrics`.
- `views/metrics.json` is the machine view of the same balance: review share
  of wall time, rounds, raw vs published messages per type, merge and drop
  rates, the owner's A/D counts and D rate, owner wait time, blockers
  escalated vs downgraded, and the unexposed-decision proxy (trade-offs a
  reviewer raised that the worker did not raise itself). The status view
  renders it as one `metrics` line.

Front ends never infer a message's outcome from anything but these
projections (or the `tt state` payload, which carries the same state).

## 6. Environment preflight and `ENV_BLOCKED`

The **run axis** has three states: `RUN_ACTIVE`, `RUN_PAUSED_BUDGET` (the run
execution budget, §8.1) and `ENV_BLOCKED` (plan 05i). `ENV_BLOCKED` freezes
dispatch exactly like a pause, while the **phase state is preserved** — a 127
during `CHECKING` leaves the phase in `CHECKING`, so a passing resume re-runs
the checks instead of losing the candidate.

Before it takes the base baseline or launches any agent, and again on every
`tt resume`, the conductor resolves the executable of every declared shell
command (each effective check, the contract's `:GATE:` and `:GATE_CLEANUP:`)
with `command -v` in its own `PATH`. It records the resolved path of each tool
it checked (`ENV_CHECKED`, a record-only event stored in `phase.env.tools`,
shown as `env  cargo /Users/…/.cargo/bin/cargo`). The preflight parses a
shell command into the first word of every simple command, splitting on `&&`,
`||`, `;`, `|` and a lone `&`, treating file-descriptor redirections
(`2>&1`, `>&2`, `&>file`, `2>/dev/null`) as redirections rather than
separators, and skipping shell builtins, keywords (and a `for`/`select`
loop's variables) and variable assignments (`src/core/env-preflight.ts`).

The run-axis rows (all in `TRANSITIONS`):

| id | from | trigger | to |
| --- | --- | --- | --- |
| `env-preflight-failed` | `RUN_ACTIVE` | `ENV_PREFLIGHT_FAILED` | `ENV_BLOCKED` |
| `env-preflight-failed-already-blocked` | `ENV_BLOCKED` | `ENV_PREFLIGHT_FAILED` | `ENV_BLOCKED` |
| `env-preflight-failed-from-budget` | `RUN_PAUSED_BUDGET` | `ENV_PREFLIGHT_FAILED` | `ENV_BLOCKED` |
| `env-check-failed` | `RUN_ACTIVE` | `ENV_CHECK_FAILED` | `ENV_BLOCKED` |
| `env-check-failed-already-blocked` | `ENV_BLOCKED` | `ENV_CHECK_FAILED` | `ENV_BLOCKED` |
| `env-check-failed-from-budget` | `RUN_PAUSED_BUDGET` | `ENV_CHECK_FAILED` | `ENV_BLOCKED` |
| `env-resumed` | `ENV_BLOCKED` | `RUN_RESUMED` | `RUN_ACTIVE` |
| `env-resumed-to-budget` | `ENV_BLOCKED` | `RUN_RESUMED` | `RUN_PAUSED_BUDGET` |

A run that a conductor can start or resume is in `RUN_ACTIVE`, `RUN_PAUSED_BUDGET`
or `ENV_BLOCKED`, and the preflight runs on every start — so the failure rows
exist from all three.

Events:

- `ENV_CHECKED { path, tools: [{name, path?}] }` — record-only; stores the
  resolved tools in `phase.env` for the status views and moves no state.
- `ENV_PREFLIGHT_FAILED { missing, path }` — a declared command's executable
  is not on the conductor's `PATH`. The run becomes `ENV_BLOCKED` before the
  baseline and before any agent, with the missing names and the exact `PATH`.
- `ENV_CHECK_FAILED { stage, command, exitCode, tail }` — a check, baseline,
  probe or gate command exited 126/127. The run becomes `ENV_BLOCKED`
  carrying the command and the log tail. It is **never** written as a
  baseline, reused by a sibling, recorded as `checks failed`, or turned into
  a repair or a finding.
- `RUN_RESUMED` — from `ENV_BLOCKED` (and, separately, from
  `RUN_PAUSED_BUDGET`): the preflight (or the budget) no longer blocks; the
  frozen phase continues. A block entered from a budget pause remembers that
  pause (`phase.env.resumeRun`) and clears back to `RUN_PAUSED_BUDGET`, so
  clearing the environment block never silently runs past an exhausted
  budget (`env-resumed-to-budget`).

A baseline record whose commands exited 126/127 — a record written before
this change, or a sibling's shared copy — is **ignored on read**
(`baselineHasEnvironmentFailure`), so the baseline re-runs in a fixed
environment instead of excusing a candidate against a broken base. The
visible line is `env blocked · <tool> not found on PATH (<path>)`, or
`env blocked · <command> exit <126|127> — the tool is not available here`.

`tt program start`/`resume`/`retry` run the same preflight in the caller's
`PATH` for every node they will start and refuse, naming the missing tools,
before any run is created.

## 7. Version 1

This is contract version 1. A future change to the message states, the event
names, the binding shape, the `contentHash` inputs or the carry rule bumps the
version and is documented here.

## 7. Entries (plan 05j)

The message ledger counts messages; the owner decides on TOPICS. An **entry**
is one topic: a trade-off the implementation made where it departed from or
filled a gap in the plan (its choice, the alternative given up, and who
approved it), a finding that is a defect against the plan or the code, or a
blocker that stops acceptance. The same issue raised as a blocker, a
trade-off and a finding — 15a's `B-1 = T-14 = F-1` — is one entry.

- **Type.** An entry's type is the highest type of its linked messages: a
  blocker if a blocker vote or a blocking finding stops acceptance, otherwise
  a finding if any linked message is a defect, otherwise a trade-off. No
  curator may change it.
- **Events.** `ENTRY_OPENED`, `MESSAGE_LINKED` (message, entry, shared anchor,
  reason), `ENTRY_RETITLED`, `ENTRY_SPLIT` (owner), `ENTRY_STATE` (open,
  resolved in `<sha>`, dropped with a reason), `ENTRY_MERGED_BY_OWNER`
  (owner) and `ENTRY_CURATED` (one round's curator pass is done, so the
  evaluators may start). They are appended to `events.jsonl`; the entries are
  a pure projection of them. `REVIEW_LINT_FAILED` records a violated rule.
- **Anchors.** Every message gets a normalised anchor from its evidence: a
  file and line range, a decision id, or a plan clause. The runtime accepts a
  link only when the message and the entry share an anchor (an overlapping
  line range in the same file, the same decision id, or the same plan clause).
  Any other link is refused and logged, and the message opens its own entry —
  nothing is silently merged, nothing hidden. A message with NO real anchor
  (prose evidence, no decision id, no plan clause) still gets its own entry,
  anchored to the message itself, so two such messages never merge; the lint
  reports that entry as having no anchor. The phase id is never treated as a
  plan clause (`A-34`).
- **The curator.** The runtime opens an entry for every live message as it
  is raised (a round-time pass, so `ENTRY_OPENED`/`MESSAGE_LINKED` are in the
  log and entry ids are stable), linking a message to an open entry it
  shares an anchor with. Once per round, after the reviews and before the
  evaluators, that pass also starts a curator AGENT on the evaluator's model
  (a plan may override it with `#+TT_MODELS curator=…`; with no model
  configured it launches on Pi's default exactly like an evaluator, never
  skipped) and records `ENTRY_CURATED`. The agent is launched first but never
  blocks the evaluators: the evaluator and panel still launch with their own
  models even when the curator cannot run. The agent sees every message of every type plus
  every open entry and returns through `curate_entries`, whose only ops are
  `link`, `open` and `retitle` (any other op is refused before an event is
  applied; it cannot drop, resolve, merge or change a type).
  Reviewers' and evaluators' prompts also list the open entries, so an agent
  sees the topic a message belongs to, and a reviewer's `sameAs E-n` links its
  raise to that entry (refused and logged when they share no anchor). The owner's `m` is the one exception to the anchor rule:
  it merges two near-duplicates that deliberately share no anchor, so the
  links it moves are kept.
- **Latest state.** Each entry's state is computed against the newest
  candidate; the view's header names it. An entry all of whose linked
  messages are settled — resolved, dropped, merged or superseded — leaves the
  live view. An
  open entry whose anchor no longer resolves in the newest code is kept and
  tagged `stale anchor`, never hidden; when a candidate exists but its
  checkout could not be read, the anchor could not be re-checked and is tagged
  `anchor unverified`, never assumed fresh.
- **Views.** `views/review.org` (phase, `C-c m d`) and
  `programs/<id>/views/review.org` (program, `C-c m D`) have three sections —
  Blockers, Findings, Trade-offs — one heading per live entry: title, then its
  tags (phase id in the program view; who raised it, e.g. `M·A`; `+2 linked`;
  its anchor). In the program view the heading id is qualified with its phase
  tags (`prog-01:E-1`), since entry ids are numbered per phase, and a folded
  cross-phase topic takes the highest type of its messages. `TAB` shows the entry's summary and each linked message; `RET`
  opens `views/entries/<id>.org` with the full history; `s` on a linked
  message splits it into its own entry, which takes that message's OWN anchor
  (the owner has decided the two are separate topics, so the pair the split
  produced is exempt from the one-anchor rule); `m` on a `≈` hint merges the
  two as the owner's own action; `A`/`D` are verdicts on every linked message
  (`OWNER_VERDICT`, so accepting a trade-off is not a code fix) — a message
  still raw cannot be settled and is reported to the owner, never silently
  skipped. The program view links
  entries across phases only through shared anchors, the same rule.
- **Accounting.** The last line reconciles every raw message:
  `31 raw → 9 entries · 18 linked · 2 dropped · 0 unaccounted · unexposed 3`.
  A message that is neither linked, dropped, merged nor resolved is
  unaccounted, and the lint fails. Near-duplicates that share no anchor are
  never merged by the curator; a deterministic similarity over normalised
  words (threshold in `ENTRY_SIMILARITY_THRESHOLD`) gives each an `≈ E-n` hint
  tag instead.
- **Review lint** (`src/core/review-lint.ts`) runs on every render: one live
  entry per anchor; only live entries rendered; every entry has a type, an
  anchor and its validation evidence (a citation, a `verified:` marker or a
  vote); titles non-empty, at most 80 characters, not cut mid-word; every
  state computed against the newest candidate; the accounting reconciles to 0
  unaccounted. A violation is never repaired silently — the projection does
  not rewrite a title or drop a message: the view's first line names it
  (`review lint: 2 entries share capital.rs:88-104`) and a
  `REVIEW_LINT_FAILED` event records it, on the conductor and on every CLI
  render (`tt contract rebuild`, a late verdict, an entry command).
- **Cleanness metrics** (`views/metrics.json`, the status `metrics` line and
  `tt summary`): live entries per phase (a warning above a budget, default
  12), entries per distinct anchor, open `≈` hints, the owner's `m`/`s`
  corrections (the ground truth of curator errors) and lint violations.
