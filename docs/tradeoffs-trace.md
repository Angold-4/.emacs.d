# tradeoffs-trace: a programmed review pipeline for Pi in Emacs

Status: **Proposal for review.** Nothing here is implemented. It describes the
first workflow built on the rationale in
[Human judgment and programmed agent workflows](agent-evaluation-and-programmed-workflows.md),
and scopes a first version whose only job is to work end to end and test the
assumptions the design rests on.

## 0. What this is

`tradeoffs-trace` runs a multi-phase plan through Pi agents and does two things
a prompt-driven loop cannot promise:

1. It **enforces the loop**. Checks run, reviews happen, every stage has a
   deadline, and a phase advances only when recorded evidence satisfies a
   predicate that code evaluates. It never advances because a model thinks the
   work "looks good enough".
2. It **traces trade-offs**. Choices made during implementation are collected
   as decision records from several independent sources, reviewed by
   independent agents, and presented in one Emacs view the owner can read
   instead of transcripts.

What it does **not** promise is that every consequential choice is caught. The
worker may not disclose a choice, reviewers can share blind spots, and triggers
only see declared boundaries. Completeness is something this workflow
**measures** (§3.5, §9.3), not something it guarantees.

### A skill here is a program, not a prompt

In this workflow a "skill" is an executable pipeline: a conductor program that
starts Pi agents, owns the state machine, enforces deadlines and records every
outcome. Prompts still exist inside it as role instructions for the worker and
reviewers, but they describe *how to do a bounded task*. They never decide
*whether the loop may continue*.

```text
prompt-driven loop                     tradeoffs-trace
------------------                     ---------------
model reads SKILL.md                   conductor program owns the loop
model remembers the obligations        obligations are states and transitions
model decides "good enough"            code evaluates the acceptance predicate
master context = project history       agents are fresh; history = the log
```

There is no LLM "master" holding the project in its context. The conductor and
its log are the only things that span the whole run.

### Three kinds of record, three kinds of authority

The central rule of the design. It is what keeps the pipeline serving the
owner's judgment rather than merely automating agreement among agents:

| Record | What it claims | Resolved by | Never resolved by |
| --- | --- | --- | --- |
| **Decision** | "this is a choice within the permitted scope" | review vote (§5), or the owner | — |
| **Finding** | "the candidate is wrong": a defect or a contract violation | evidence: repaired, disproved, or accepted by the owner (§4) | a vote |
| **Owner request** | "this needs the owner's authority" | the owner only | a vote, a timeout, silence |

**Voting resolves permitted preferences. It does not resolve factual defects,
contract violations or missing human authority.** A majority can decide
between two acceptable designs. It cannot outvote a reviewer who says the
candidate breaks the contract.

## 1. From a plan to a running pipeline

### 1.1 The plan

A run starts from a plan in an Org file. Anyone can write it: the owner, Claude
Code in another session, or a Pi agent. tradeoffs-trace does not care who
authored it, only that it validates. The plan is not expected to be perfect.
Only the **next executable phase** needs a precise contract; later phases can
be tagged `:provisional:` and are revised as implementation teaches us things.

```org
#+TITLE: matching-engine cancellation refactor
#+TT_CHECKS: npm test && npm run lint
#+TT_BUDGET: wall=10h tokens=6M

* Phase 1: extract the cancel path
  :PROPERTIES:
  :ID:          p1
  :CHECKS:      npm test -- cancel
  :BOUNDARIES:  src/api/** src/schema/**
  :RESERVED:    public API types; persistence format
  :END:
  Goal: move cancellation logic out of orderbook.ts without behavior change.
  Acceptance:
  - existing cancel tests pass unchanged
  - no change to public API types

* Phase 2: make cancellation race-free                         :provisional:
  ...
```

- `CHECKS` are shell commands the conductor runs itself. They must be safe to
  rerun, because resume may rerun an interrupted check (§7).
- `BOUNDARIES` are path globs whose modification automatically creates a
  decision record (§3.3).
- `RESERVED` names choices that must always reach the owner, in addition to the
  standing reserved classes in §3.4.
- The goal and acceptance list are given to the worker and to every reviewer
  word for word.

### 1.2 Starting a run: `C-c m r`

```text
visit the plan file (find-file, a link, wherever it came from)
    │
    ▼  C-c m r   (in the plan buffer)
validate      – next phase has ID, CHECKS, goal, acceptance; globs parse
    │           errors → *tt-plan-errors* with jump-to-line, and no run starts
    ▼
snapshot      – copy to <run>/plan/v1.org; record its sha256
    │           the run never reads the original file again
    ▼
create run    – run id, run directory, conductor lock, meta.json (plan path,
    │           plan sha, mode, created, status)
    ▼
start         – conductor process; ask: delegate or co-work (switchable later)
    │
    ▼
open workspace – a tab-bar tab "tt:<project>/<run-id>" with the layout below;
                 status is shown automatically
```

If the plan buffer already has an active run, `C-c m r` asks whether to focus
that run or start a new one. It never silently starts a duplicate.

### 1.3 The workspace

One tab per run:

```text
┌───────────── agent trace ──────────────┬──────────── status ─────────────┐
│ *tt-trace: engine/r-0923a*             │ *tt-status: engine/r-0923a*     │
│                                        │                                 │
│ [worker p2 · attempt 2 · 14:32]        │ mode: co-work    budget 3.1h/10h│
│ > reading src/cancel.ts                │                                 │
│ Moving removal under the book lock…    │ p1 extract cancel   DONE        │
│ ▸ tool: edit src/cancel.ts (folded)    │ p2 race-free        REVIEWING   │
│ ▸ tool: bash npm test -- cancel        │    round 2/3 · cand 7c1e0a4     │
│                                        │    checks ✓ 2/2                 │
│ follows the active agent; `a` picks    │    reviews M ✓  A ⧗  B ⧗        │
│ another agent of this run              │    open: 1 finding, 0 decisions │
│                                        │ p3 cancel metrics   PROVISIONAL │
│                                        │                                 │
│                                        │ ⚑ needs you: 1   (C-c m d)      │
├────────────────────────────────────────┴─────────────────────────────────┤
│ *tt-input: engine/r-0923a*   → steer worker p2 (attempt 2)               │
│ keep the lock hold under 50µs; measure before and after                  │
└───────────────────────────────────────────────────────────────────────────┘
```

- **Trace** (left): a read-only rendering of the active agent's Pi session
  (§8.3). It follows whichever agent is running; `a` chooses a specific agent
  of the run.
- **Status** (right): rendered from the log. `RET` on a phase opens its diff;
  `c` its check output; `d` its decisions; `m` switches mode; `p` pauses.
- **Input** (bottom): the text is sent as a **steer** to the worker currently
  running, bound to that worker's attempt (§6). If no worker is running, the
  text becomes an **owner note**, shown in status and delivered at the start
  of the next worker attempt. Reviewers cannot be steered. Their independence
  is the point of having them.

`C-c m s` focuses the run's tab. If the tab or any of its windows has been
closed, it rebuilds the layout. After an Emacs restart, the same command
reopens it from the run directory.

### 1.4 Which run a command means

Several plans and runs can be open at once. Every `C-c m` command resolves its
run the same way:

1. A tradeoffs-trace buffer (trace, status, input, decisions, session view)
   has a buffer-local run id. Use it.
2. A plan buffer: use the most recent run whose `meta.json` records this
   plan's path. If more than one of them is active, ask.
3. Anywhere else: `completing-read` over runs, sorted by last activity, active
   runs first, each labelled with project, plan title, status and "needs you"
   count.

Runs are discovered by scanning `~/.tradeoffs-trace/*/*/meta.json`. There is no
separate registry to fall out of sync.

## 2. Roles and processes

| Role | Lifetime | Sees | Produces |
| --- | --- | --- | --- |
| **Conductor** (code, not a model) | whole run | everything | the log, worktrees, candidate commits, projections |
| **Worker** | one phase; resumed for its own repairs | phase contract, repository, prior accepted decisions, owner notes | code in its phase worktree; `submit_phase` |
| **Master reviewer** M | whole run, one Pi session | plan, every phase's diff and records, its own earlier reviews | `submit_review` |
| **Fresh reviewers** A, B | one phase (kept across that phase's repair rounds) | phase contract, repository, diff, records under review | `submit_review` |

The master reviewer is the one intentionally stateful agent. It carries the
plan's intent across phases, so it can notice when phase 4 quietly undoes a
trade-off accepted in phase 2. The fresh reviewers counter the master's
weakness: an agent that has approved three phases of an approach is anchored
on it. A and B never see the worker's reasoning or the master's reviews before
they submit their own. A and B should use a different model family from M where
the gateway allows it.

Keeping A and B fixed across one phase's repair rounds is a continuity
trade-off: repairs converge on the objections that were actually raised,
rather than chasing a new reviewer's taste every round. It is a pilot
hypothesis, like the whole master-plus-fresh arrangement.

### Each agent is its own process

The conductor starts each agent as a child `pi --mode rpc --extension
tradeoffs-trace.ts` process in **its own process group**, and talks to it over
Pi's RPC protocol. Pi also offers an in-process SDK, which is why this needs
stating. Separate processes are what make deadlines enforceable: killing a
process group ends the agent and every command its bash tool started, and an
agent crash cannot take the conductor down with it. The extension provides the
`submit_phase` and `submit_review` tools and the guards in §8.4.

## 3. Decisions

### 3.1 What a decision is

A choice made during implementation that changes behavior, interfaces,
guarantees or cost in a way the contract left open. "Batch cancels per tick
instead of taking the lock per request" is a decision. "Cancel can now
acknowledge before fills stop, while the contract says no fill after cancel"
is **not** a decision. It is an allegation that the candidate violates the
contract, and it is a finding (§4).

### 3.2 What a record must say

Records are written for a person deciding quickly. `submit_phase` and
`submit_review` require these fields, and the conductor rejects a submission
with any of them empty:

- **choice**: one plain sentence.
- **why it matters**: in terms of the plan's goal, not the code.
- **alternatives**, each with its consequence.
- **recommendation**, and the reason for it.

The conductor checks that the fields are present, not that they are good. The
quality of these fields is part of what the pilot measures (A5).

### 3.3 Where decisions come from

1. **Worker disclosure.** The worker cannot finish a phase with prose. It must
   call `submit_phase`, whose arguments include its decisions, assumptions and
   deviations from the plan. The extension's `agent_before_settle` handler
   requests a continuation while no submission exists: at most two, then the
   attempt fails.
2. **Reviewer discovery.** Each reviewer lists the behavioral choices it sees in
   the diff *before* the conductor gives it the worker's disclosure. The
   review happens in two turns, and the second is sent only after the first
   is submitted. A discovered choice that matches no disclosed decision
   becomes a record marked `reviewer-discovered`.
3. **Observable triggers.** The conductor diffs the candidate against
   `BOUNDARIES`, dependency manifests and acceptance files. A hit with no
   corresponding record creates one marked `trigger`, which a reviewer must
   classify. This source is code, not a model.

### 3.4 Classification

Each decision gets a class. The worker proposes it, any reviewer may raise it,
and **only the owner may lower it**:

| Class | Example | Treatment |
| --- | --- | --- |
| `detail` | local helper structure, naming | recorded, not voted; **sampled** (§3.5) |
| `delegated` | a trade-off inside the contract's stated intent | voted by reviewers (§5) |
| `reserved` | changes a requirement, public interface, guarantee in the acceptance list, persistence format, dependency, or anything in `RESERVED` | owner request, in every mode |

No vote can pass a reserved decision.

### 3.5 Measuring what the classifier might hide

A `detail` label skips the vote, so the classifier could hide exactly the
choices the owner cares about. The decision view (§9) therefore always includes
a **sample**, drawn by the conductor rather than a model:

- a share of `detail` decisions (v1: all of them in the pilot, since volume is
  unknown);
- **unreferenced changes**: diff hunks that no decision or finding cites. The
  conductor computes these mechanically from hunk ranges and record citations;
- a share of unanimously accepted decisions.

When the owner marks a sampled item "should have been surfaced", that is a
recorded miss, and the miss rate is the completeness measurement.

## 4. Findings

### 4.1 What a finding is

A reviewer's allegation that the candidate is wrong:

- `defect`: behaviour is incorrect, whether or not the checks catch it. A race
  the tests miss is the typical case.
- `contract`: the candidate violates the phase's goal, acceptance or a
  recorded accepted decision.

A finding needs evidence: file and line, a scenario, a check result, or a plan
clause. It has a severity. `blocking` is the default for both kinds, and only
the owner can lower a finding to `advisory`.

A ballot on a decision can carry the objection "this violates the contract".
When it does, the conductor opens a linked `contract` finding and **suspends the
vote** on that decision until the finding is closed.

A reviewer may attach a proposed reproduction (a test file or a command). The
conductor runs it against a scratch copy of the candidate. A reproduction that
fails as claimed marks the finding `reproduced`. One that passes does **not**
close the finding; it is shown to the raising reviewer as evidence.

### 4.2 How a finding closes

| Disposition | Requires | Recorded |
| --- | --- | --- |
| **repaired** | a new candidate; checks pass on it; the **raising reviewer** confirms, in its review of that candidate, that the finding no longer holds | candidate, confirming review |
| **disproved** | counter-evidence (a citation, or a test demonstrating the property); the **raising reviewer** withdraws the finding | the evidence, the withdrawal |
| **accepted** | **the owner only**. For `contract`, this means amending the contract (§6.3); for `defect`, a recorded risk acceptance with its scope | owner command, bound to versions |

Other reviewers can agree or disagree in their reviews, and the view shows it.
They cannot close someone else's finding. If a finding is still open when the
phase's repair budget runs out, it becomes an owner request.

### 4.3 The worked example, correctly routed

Phase 2's acceptance says "no fill can occur after cancel is acknowledged". The
worker moves cancellation onto a queue and discloses it as a `delegated`
decision. Reviewer B's ballot objects with a contract citation, which opens a
`contract` finding and suspends the vote. The phase is then in repair:

- The worker either restores synchronous removal (repaired, once B confirms),
- or argues that the acknowledgement already implies the guarantee (disproved
  only if B withdraws),
- or the budget runs out, and the owner gets a request: keep the guarantee, or
  amend the contract to "acknowledge means queued".

M and A approving the queue has no effect on any of these outcomes.

## 5. The vote

### 5.1 The rule

Voting applies **only** to `delegated` decisions with no open linked finding.
Voters: M, A and B. Each ballot is `approve` or `reject`, with a rationale and
at least one evidence citation.

> **A decision passes if and only if M approves and at least one of A, B
> approves.**

| M | A | B | result | why |
| --- | --- | --- | --- | --- |
| approve | approve | approve | pass | |
| approve | approve | reject | pass | 2 of 3; dissent shown |
| approve | reject | approve | pass | 2 of 3; dissent shown |
| approve | reject | reject | **fail** | the fresh reviewers override the anchored one |
| reject | any | any | **fail** | the master holds the plan's intent and has a veto |

This is a two-thirds majority in which the master has a veto. The tally is
ordinary code.

- A missing, malformed or evidence-free ballot counts as **reject**, following
  the "missing support is FAIL" rule in OrgBrain #96.
- A ballot is bound to its versions (§6.1). A ballot on a superseded candidate
  is discarded, not counted.

### 5.2 When a decision fails

The rejecting ballots go to the worker's session as a repair request: change
the implementation, or keep it and answer the objection with evidence. The
next candidate is checked and reviewed again in full (§6.2). A decision that
keeps failing until the budget runs out becomes an owner request carrying every
ballot.

### 5.3 Where Jev / SemIf fits

Not in v1. The tally is deterministic and needs no model. The natural later use
is a **ballot- and finding-validity gate**: does the cited evidence support the
claim? That is the claim-to-evidence shape OrgBrain #96 pilots. It would run
first in observation mode, and become enforcing only if it measurably catches
unsupported claims.

## 6. Versions and authority

### 6.1 Everything is bound to what it evaluated

Every ballot, review, finding disposition and owner command carries:

```text
run id · phase id · candidate sha · contract version · record id · record version
```

- **candidate sha**: at `submit_phase`, the conductor commits the worktree
  itself (the worker does not commit). That commit is the candidate.
- **contract version**: the plan snapshot number, plus the sha256 of the
  phase's section in it.
- **record version**: increments whenever a record's content, class or linked
  findings change.

A command or ballot whose binding no longer matches is **rejected visibly**. It
gets a log event, a message in the echo area, and a line in status saying what
changed ("decision D-7 changed v2 → v3 since you viewed it"), and the view is
refreshed. Nothing is applied to a version the sender did not see.

### 6.2 v1: a new candidate invalidates all of the phase's evidence

Deciding which evidence survives a change is hard to get right. In v1, any new
candidate reruns **all** of the phase's checks and **all three** reviews.
Every ballot and closure confirmation on the old candidate is discarded. Open
findings carry forward, to be re-examined by their raising reviewer on the new
candidate. Steering therefore costs a full re-review if it changes code, and
that cost is intended.

### 6.3 Amending the contract

`amend` writes a new plan snapshot. For v1:

- The current phase's evidence is entirely invalidated, and the phase returns
  to checks and review under the new contract version.
- Provisional later phases simply read the new snapshot.
- Amending the contract of a phase **already integrated** is out of scope for
  v1. It requires a new run, because it would mean re-verifying integrated
  work.

### 6.4 Owner commands

| Command | Effect | Bound to |
| --- | --- | --- |
| steer | message delivered to the running worker through Pi's steer | worker attempt id |
| note | queued for the next worker attempt | phase |
| resolve | answer an owner request (choose an option or write one) | request id and version, candidate, contract |
| override | approve or reject a delegated decision; recorded **beside** the ballots | decision version, candidate, contract |
| accept-finding | the owner's disposition of a finding (§4.2) | finding version, candidate, contract |
| amend | new contract version (§6.3) | contract version being replaced |
| pause / resume / mode | control only | run |

Steering, overriding and amending are deliberately separate. Asking "would a
queue help here?" is conversation. Rejecting the queue is a decision. Changing
what "cancelled" means is a contract revision. The log keeps them apart.

## 7. Bounds, cancellation and termination

### 7.1 Every stage has a deadline

Every stage below has a conductor-enforced deadline:

| Stage | Default (plan may override) | On expiry | Resulting state |
| --- | --- | --- | --- |
| worker attempt, start to `submit_phase` | 45 min, plus a per-attempt token cap | cancel (§7.2) | attempt `timed_out`; consumes a repair round |
| settle reminders | 2 continuations | attempt fails | attempt `no_submission`; consumes a repair round |
| each check command | 10 min | cancel | check `failed: timeout` |
| each review | 15 min | cancel, re-dispatch once | then the phase is `BLOCKED: reviewer unavailable` |
| reproduction command | 5 min | cancel | reproduction `inconclusive` |
| repair rounds per phase | 3 | — | open items become owner requests |
| run execution budget | from `TT_BUDGET` (wall and tokens) | stop dispatching | run `PAUSED: budget` until the owner resumes it with more budget |

The execution budget counts only time spent executing. A phase parked in
`AWAITING_OWNER` consumes nothing, because waiting for a person is an explicit
state, not work.

### 7.2 Cancellation

- **Agent**: send RPC `abort`, wait 30 s, then `SIGTERM` the process group,
  wait 10 s, then `SIGKILL` it. The worktree is left as it is, for inspection.
- **Check or reproduction**: started in its own process group; `SIGTERM`, 10 s,
  `SIGKILL`.
- Every cancellation writes an event naming the stage, the reason and the
  signals sent.

### 7.3 Why an unattended run terminates

The phase list is finite. Each phase has at most 1 + 3 candidate rounds. Each
round is bounded by the worker, check and review deadlines, and the execution
budget caps the total. So an unattended run always reaches one of `DONE`,
`BLOCKED`, `PAUSED: budget`, or `AWAITING_OWNER` in bounded execution time.
None of those states advances without a recorded cause.

## 8. State, recovery and the Emacs surface

### 8.1 The run directory

```text
~/.tradeoffs-trace/<project>/<run-id>/
  meta.json         plan path, plan sha, mode, status, created, last activity
  conductor.lock    held with flock by the one conductor for this run
  plan/v1.org …     immutable plan snapshots
  events.jsonl      append-only, fsync per event; the only source of truth
  views/            status.org, decisions.org — regenerated, never edited
  inbox/            owner commands, one file each: <command-id>.json
  inbox/applied/    commands already applied
  sessions/         Pi session files, one per agent
  checks/           check and reproduction output, by candidate
```

Workers cannot write here. Their `tool_call` guard blocks it (§8.4), with the
limit stated there.

### 8.2 Crash-safe resume

**One conductor per run**: the conductor holds `conductor.lock` with `flock`
for its lifetime. Emacs refuses to start a second one, and a crashed
conductor's lock releases with its process.

**Every external effect has an ID, an intent event and a completion event**,
and a defined reconciliation for "intent recorded, completion missing":

| Effect | Reconciliation on restart |
| --- | --- |
| create worktree | path exists at the recorded base → record done; otherwise remove the partial worktree and recreate it |
| agent attempt | kill the orphaned process group (its pgid is in the intent). Mark the attempt `interrupted`. Worker: new attempt on the same session file with an interruption note. Reviewer: discard; start a new review |
| candidate commit | worktree HEAD carries trailer `TT-Action: <id>` → record it; otherwise commit again |
| check run | mark `interrupted` and rerun (checks are required to be rerunnable) |
| integrate | integration branch has a commit with trailer `TT-Action: <id>` → done; otherwise reset to the recorded pre-merge sha and retry |
| owner command | command id already in the log → only move the file to `applied/`; otherwise apply it, log it, then move it |

Owner command IDs are generated by Emacs (a UUID in the file name), so a
command is applied at most once whatever happens to the file.

**Gates have three outcomes: `passed`, `failed` and `interrupted`.** Only a
recorded completion event counts as `passed`. A gate whose completion cannot be
established is `interrupted` and is rerun or reconciled; it is never assumed.

Recovery is tested by fault injection. `TT_CRASH_AT=<boundary>` makes the
conductor exit at a named point: before an effect, after the effect but before
its completion event, after the event but before the inbox file moves. The
suite runs every boundary and asserts that no effect happens twice and the
final state is the same as an uninterrupted run.

### 8.3 Viewing agent sessions

Pilish's `pilish-open-session-file` resumes a session file as a **live** Pi
process. Opening a session the conductor still owns would put two writers on
one file, and even opening a finished one would change a record kept as
evidence. So tradeoffs-trace needs its own viewer, and that is an explicit
integration task:

- `tt-session-view` reads a Pi session JSONL file and renders it read-only:
  message text, and tool calls folded to one line each. It follows appended
  lines with `file-notify` and never starts a process.
- No Pilish command that mutates or resumes a session is reachable from these
  buffers.
- **Continue in Pilish** (only after the run is finished, and deliberately
  invoked): copy the session file into Pi's session directory as a new file,
  then open *the copy* with Pilish. The run's original stays unchanged.

The trace column of the workspace is this viewer, pointed at the active agent.

### 8.4 Worker guards, and their limit

The worker's extension:

- blocks writes outside its worktree and to the phase's acceptance files, and
  blocks `git push`, `git commit` and anything under the run directory
  (`tool_call`);
- refuses to settle without `submit_phase` (`agent_before_settle`, bounded).

These hooks are **workflow guards, not a security boundary**. A worker with a
bash tool can get around them. Protecting control state needs filesystem and
process permissions, which v1 does not implement. This is recorded as a known
gap, not claimed as done.

### 8.5 Keys

These extend the `C-c m` prefix from the Pilish integration (#12):

| Key | Action |
| --- | --- |
| `C-c m r` | in a plan buffer: validate, snapshot, create and start a run; open its workspace |
| `C-c m s` | focus or rebuild the run's workspace (resolved per §1.4) |
| `C-c m d` | open the run's decision view (§9) |

## 9. The decision view: `C-c m d`

### 9.1 What it is

`*tt-decisions: <project>/<run-id>*` is a read-only Org buffer in
`tt-decisions-mode`. It is rendered from the log into `views/decisions.org`, so
it outlives Emacs. After a restart, `C-c m d` resolves the run as described in
§1.4 (finished runs included) and renders it again. You act through keys, never
by editing text, and every key sends a version-bound command (§6).

It is ordered by what needs the owner:

```text
Needs you (1)            owner requests, blocking
Open findings (1)        blocking findings not yet closed, and who raised them
Accepted with dissent    passed votes where a reviewer rejected
Accepted                 passed votes, unanimous
For sampling             detail decisions, unreferenced changes (§3.5)
```

Each entry leads with what a person needs in order to judge it. Ballots,
hashes, logs and evidence sit in a folded **Details** subtree, with links.

### 9.2 A pending decision

```org
* NEEDS-YOU Cancel may be acknowledged before fills stop               :p2:
Phase 2 — make cancellation race-free · raised by reviewer B · blocking

Why it matters: the plan promises that once cancel is acknowledged, the
order cannot fill. The current change acknowledges when the request is
queued, so a fill can still land after the caller is told the order is
dead.

Options
  1. Keep the guarantee: remove the order from the book synchronously.
     Consequence: cancel p99 rises about 0.4 ms in the phase benchmark.
  2. Relax the guarantee: "acknowledged" means "queued".
     Consequence: changes phase 2's acceptance; API users must be told.

Recommendation: 1. The latency cost is inside the phase's budget, and the
guarantee is what the refactor exists to protect.

Disagreement: the worker and reviewer A preferred option 2 for
throughput. Reviewer B and the master say it breaks the stated guarantee.
Three repair rounds did not settle it.

You: 1 / 2 choose · w write your own · RET evidence · phase 2 waits on this

** Details
   - finding F-p2-02 (contract) · candidate 7c1e0a4 · contract v1#9e2c
   - ballots, reviews, check output, worker session → links
```

### 9.3 An automatically accepted trade-off

```org
* ACCEPTED Batch cancels per tick instead of one lock per request      :p2:
Phase 2 — make cancellation race-free · decided by review (2 of 3)

Why it matters: fewer lock acquisitions under load; cancels in the
same tick still apply in arrival order, and no guarantee changes.

Alternative: a lock per request — simpler, but twice the contention in
the phase benchmark.

Dissent: reviewer A worried a lone cancel can wait up to one tick
(≤1 ms). The master and reviewer B judged that within the latency budget.

You: nothing required · o reopen (override) · RET evidence

** Details
   - decision D-p2-05 v2 · delegated · M approve, A reject, B approve
   - candidate 7c1e0a4 · contract v1#9e2c · links to ballots and diff hunks
```

### 9.4 Keys in the view

| Key | Action |
| --- | --- |
| `1`–`9`, `w` | resolve an owner request with an option, or write one |
| `o` | override a decision (approve or reject) |
| `x` | accept a finding (§4.2), with a required scope note |
| `s` | on a sampled item: "should have been surfaced" (records a miss) |
| `RET` | open the evidence at point: diff hunk, file:line, check output, session |
| `TAB` | expand or collapse details |
| `g` | refresh |

## 10. v1: make it work, and test the assumptions

v1 is judged on one question: **does the pipeline run end to end on a real
task, and are the design's assumptions about Pi, Emacs and review true?**
Polish, performance and cost optimization come later.

### 10.1 In scope

- One run at a time, phases serial, one worker per phase.
- Plan validation and snapshot, and the workspace (§1).
- Conductor with the log, lock, deadlines, cancellation and crash-safe resume
  (§7, §8.2).
- Deterministic checks; decisions from all three sources; findings with
  dispositions; the M + A + B vote; version binding; full re-review on a new
  candidate.
- Both modes; steer, note, resolve, override, accept-finding, pause.
- `C-c m r/s/d`, the read-only session viewer, the decision view with sampling.

### 10.2 Out of scope

- OrgBrain integration (context in, admission of decisions out). Keep the
  record shape admission-friendly so this is a later write, not a redesign.
- Jev / SemIf gates (§5.3).
- Two independent implementations per phase with a judge.
- Parallel phases, concurrent runs of one plan, amending integrated phases.
- Permission sandboxing of workers (§8.4).

### 10.3 Assumptions v1 must test

| # | Assumption | How v1 tests it | What would falsify it |
| --- | --- | --- | --- |
| A1 | Pi RPC agents can be run, bounded and killed for hours from a conductor | a real multi-phase run; forced timeouts at every stage | orphaned processes, hangs past a deadline, lost session state across repairs |
| A2 | Extension hooks can force structured submission | count attempts ending `no_submission` | workers regularly fail to submit, or loop on reminders |
| A3 | Reviewers find what the worker did not disclose | count `reviewer-discovered` decisions and findings; owner sampling misses (§3.5) | zero discoveries while sampling finds undisclosed choices |
| A4 | The vote and the finding split change outcomes usefully | rejected-then-repaired decisions; findings closed as repaired vs disproved; owner agreement on a sample | votes always unanimous; findings rarely confirmed; owner disagrees with most outcomes |
| A5 | The decision view costs less attention than transcripts | owner time to review a finished run; how often a transcript had to be opened | owner routinely needs transcripts to understand an entry |
| A6 | Emacs can show live trace and status for a run without disturbing it | use both modes on the pilot run | viewer breaks on a file being appended; refresh churn is unusable |
| A7 | Resume is crash-safe | the `TT_CRASH_AT` suite, and one manual kill during the pilot | any effect applied twice, or a divergent final state |

A falsified assumption is a useful result. It changes the design before more is
built on top of it.

### 10.4 Build order

Each step is usable on its own:

1. **Conductor skeleton.** Plan validation and snapshot, one worker per phase in
   a worktree over RPC, checks, deadlines, cancellation, log with
   intent/completion, lock, crash suite, `status.org`. Tests A1 and A7.
2. **Submission and triggers.** `submit_phase`, the settle and tool guards,
   boundary triggers. Tests A2.
3. **Review.** M, A and B with the two-turn review, findings with dispositions,
   the vote, version binding, bounded repair. Tests A3 and A4.
4. **Emacs surface.** `C-c m r/s/d`, workspace, session viewer, decision view,
   inbox commands, both modes. Tests A5 and A6.
5. **Pilot.** One real task of three or more phases, run once in delegate mode
   and once in co-work mode, with the §10.3 table filled in from the results.

## 11. Open questions

- **The master reviewer on long runs.** Its session will compact. Should its
  prompt be rebuilt at each phase from the plan plus the log, keeping the
  session for continuity but not as the source of truth?
- **A stubborn raising reviewer.** Only the raising reviewer can confirm a
  repair or withdraw a finding. If it never does, the budget sends the finding
  to the owner. Is that the right escape, or should a finding that has been
  `reproduced`-then-fixed close on the passing reproduction alone?
- **Decision de-duplication.** Reviewers describe the same choice differently.
  v1 lets them attach to an existing record ID; whether that is reliable is
  part of A3.
- **Default deadlines and budgets.** The §7.1 numbers are placeholders to be
  calibrated from the pilot.
- **Cost.** Three reviewers per round with full re-review. Is the reviewer count
  tied to a phase risk tag from the start, or measured at full strength first?
- **Where the code lives.** In this repository next to `init-pilish.el`, or as
  its own package that the Emacs module launches.

## Related

- [Human judgment and programmed agent workflows](agent-evaluation-and-programmed-workflows.md):
  the rationale this design implements.
- [Emacs Pi integration PR #12](https://github.com/Angold-4/.emacs.d/pull/12):
  the Pilish sessions and `C-c m` prefix this extends.
- [OrgBrain issue #96](https://github.com/Angold-4/orgbrain/issues/96): the gate
  contract behind "missing support is FAIL" and the later validity gate.
- [Pi RPC](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/rpc.md)
  and [extensions](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/extensions.md):
  `--mode rpc`, `--extension`, `abort`, `steer`, `tool_call`,
  `agent_before_settle`.
- [Pilish](https://github.com/dnouri/pilish): `pilish-open-session-file` resumes
  a live session, which is why §8.3 specifies a separate viewer.
