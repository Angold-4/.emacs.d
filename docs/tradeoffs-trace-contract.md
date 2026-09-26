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
| `views/status.txt` | the plain-text status `tt status` prints (`renderStatusText`) |

Projections are written when an event can have changed them (any `MESSAGE_*`
or `OWNER_VERDICT` event) and again on every conductor start. Because
messages only change through those events, a mid-run `tt contract check` sees
current projections; and a conductor killed between an event and its
projection write rebuilds them on the next start — the log is the only
authority.

`tt contract rebuild <run>` rewrites the projections (including one file per
message) from `events.jsonl`. `tt contract check <run>` compares them to
state and exits non-zero with the mismatching file names when they differ.
`views/status.txt` is time-dependent and is regenerated, not compared.

## 1. Messages and the message state machine

A **message** is a trade-off, a finding or a blocker. Ids are `T-n`, `F-n`
and `B-n`, unique within a run. The lifecycle is a transition table,
`MESSAGE_TRANSITIONS` in `src/core/messages.ts`, with the same discipline as
`TRANSITIONS`: every row has a test fixture, and `reduce()` rejects any event
with no matching row.

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
- `MESSAGE_DROPPED { messageId, by, reason?, …binding }`
- `OWNER_VERDICT { messageId, verdict: "accept" | "refuse", reason?, …binding }`
- `MESSAGE_RESOLVED { messageId, by, reason?, …binding }`
- `MESSAGE_SUPERSEDED { messageId, reason?, …binding }`
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
tt verdict <run-dir-or-id> <messageId> <accept|refuse> [--reason <text>] [--candidate-sha <sha>] [--message-version <n>] [--root <dir>]
```

`--candidate-sha` and `--message-version` send a binding other than the
message's current one (what the owner saw before a carry); the same stale-
binding check rejects it.

- On a **live** run (a conductor is running) the command is written to
  `<run>/inbox/<id>.json`; the conductor validates the binding and a stale one
  lands in `inbox/rejected/` with the reason.
- On a run whose daemon has **exited** the command is a **late verdict**: the
  CLI dry-runs `reduce()` and, if it accepts, appends the `OWNER_VERDICT`
  event to `events.jsonl` and refreshes the projections. A stale or
  out-of-order verdict is refused, never written.

## 5. Rendering

- `messages.jsonl` and `ledger.jsonl` are the machine view.
- `views/review.org` is the human view: three top-level sections (Blockers,
  Trade-offs, Findings), blockers first. Each message is one heading (`<id>
  <title>`) carrying its summary and context and a property drawer with its
  id, type, state, `raisedBy`, `importance`, the owner's verdict (if any) and
  the binding a verdict needs (`messageVersion`, `candidateSha`,
  `contractVersion`, `runId`, `phaseId`). Within a section, high and normal
  messages come first; low-importance ones are folded under `Minor (N)`. A
  message's own file, `views/messages/<id>.org`, carries its evidence (path
  and lines), the plan excerpt it concerns, every version's history, its
  ledger entry and its votes.
- `views/status.txt` is the plain-text status the Emacs status buffer reads
  instead of calling `tt state` every poll.
- `tt summary` (the PR body) lists refused-after-`DONE` follow-ups under
  `### Follow-ups`.

Front ends never infer a message's outcome from anything but these
projections (or the `tt state` payload, which carries the same state).

## 6. Version 1

This is contract version 1. A future change to the message states, the event
names, the binding shape, the `contentHash` inputs or the carry rule bumps the
version and is documented here.
