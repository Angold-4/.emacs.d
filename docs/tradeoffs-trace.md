# tradeoffs-trace: a programmed review pipeline for Pi in Emacs

Status: **Proposal for review.** Nothing here is implemented. It describes the
first workflow built on the rationale in
[Human judgment and programmed agent workflows](agent-evaluation-and-programmed-workflows.md),
and scopes a first version whose only job is to work end to end and test the
assumptions the design rests on.

## 0. What this is

`tradeoffs-trace` runs a multi-phase plan through Pi agents and does two things
a prompt-driven loop cannot promise:

1. It **enforces the loop**. Checks run, reviews happen, repairs are bounded,
   and a phase advances only when recorded evidence allows it. Code makes
   those transitions, not a model's sense that the work "looks good enough".
2. It **traces trade-offs**. Every consequential choice made during
   implementation becomes a decision record, reviewed and voted on by
   independent agents, and collected in one Org buffer the owner can read
   instead of transcripts.

The name is the second point. The first point is what makes the second
dependable.

### A skill here is a program, not a prompt

In this workflow a "skill" is an executable pipeline: a TypeScript module that
drives Pi sessions through the SDK, owns the state machine, and records every
outcome. Prompts still exist inside it as role instructions for the worker and
reviewers, but they describe *how to do a bounded task*. They never decide
*whether the loop may continue*.

```text
prompt-driven loop                     tradeoffs-trace
------------------                     ---------------
model reads SKILL.md                   conductor program owns the loop
model remembers the obligations        obligations are states and transitions
model decides "good enough"            code routes recorded outcomes
master context = project history       every agent is fresh; history = the log
```

There is no LLM "master" that holds the project in its context. The loop
program and its log are the only things that span the whole run.

## 1. Inputs

### The plan is still needed

A run starts from an initial plan, written the way it is today: by discussing
the problem with an agent and then drafting phases. The plan is not expected to
be perfect. Only the **next executable phase** needs a precise contract; later
phases can stay provisional and are revised as implementation teaches us
things.

The plan is an Org file, because it must be readable by the owner and parseable
by the conductor:

```org
#+TITLE: matching-engine cancellation refactor
#+PROPERTY: CHECKS npm test && npm run lint

* Phase 1: extract the cancel path
  :PROPERTIES:
  :ID:         p1
  :CHECKS:     npm test -- cancel
  :BOUNDARIES: src/api/** src/schema/**
  :END:
  Goal: move cancellation logic out of orderbook.ts without behavior change.
  Acceptance:
  - existing cancel tests pass unchanged
  - no change to public API types

* Phase 2: make cancellation race-free                         :provisional:
  ...
```

- `CHECKS` are shell commands the conductor runs itself. The worker never
  reports test results; the conductor observes them.
- `BOUNDARIES` are path globs whose modification automatically produces a
  decision record (see §3.3), whatever the worker disclosed.
- The prose goal and acceptance list are given to the worker and to every
  reviewer word for word.

### Run state lives outside the worktree

Each run has a directory the workers cannot write to:

```text
~/.tradeoffs-trace/<project>/<run-id>/
  PLAN.org          frozen copy of the plan; revisions are new versions
  events.jsonl      append-only log; the only source of truth
  run.org           projection: phases, iterations, last results
  decisions.org     projection: every decision, its ballots, its outcome
  inbox/            human commands dropped here by Emacs
  sessions/         Pi session files for every agent in the run
```

`run.org` and `decisions.org` are regenerated from `events.jsonl` after every
event. Nobody edits them, including the owner; the owner acts through commands
(§6.2). That keeps one writer per file and makes crash recovery a replay.

## 2. Roles

| Role | Lifetime | Sees | Writes |
| --- | --- | --- | --- |
| **Conductor** (code, not a model) | whole run | everything | `events.jsonl`, worktrees, projections |
| **Worker** | one phase; resumed for its own repairs | phase contract, repository, prior accepted decisions | code in its phase worktree |
| **Master reviewer** | whole run, one Pi session | plan, every phase's diff and decisions, its own earlier ballots | ballots |
| **Fresh reviewers** F1, F2 | one phase | phase contract, repository, diff, decisions under vote | ballots, discovered decisions |

The master reviewer is the one intentionally stateful agent. It carries the
plan's intent across phases, so it can notice when phase 4 quietly undoes a
trade-off accepted in phase 2. The fresh reviewers exist to counter the
master's weakness: an agent that has approved three phases of an approach is
anchored on it. F1 and F2 never see the worker's reasoning or the master's
earlier ballots before they vote. They see the requirement and the diff, and
judge.

To reduce correlated blind spots, F1 and F2 should use a different model family
from the master where the gateway allows it.

## 3. Decisions

### 3.1 What a decision is

A decision is a choice made during implementation that changes behavior,
interfaces, guarantees or cost in a way the contract did not fully settle. A
decision is not a defect. "The cancel test fails" is a defect, and a check
catches it. "Cancellation now returns once the request is queued rather than
once fills are impossible" is a decision. It may be correct, and the owner
still has to know about it.

### 3.2 A decision record

Each decision is one heading in `decisions.org`:

```org
* PASSED D-p2-03 cancel acknowledges on enqueue, not on book removal  :p2:consequential:
  :PROPERTIES:
  :SOURCE:     worker-disclosed
  :CLASS:      delegated-consequential
  :REVISION:   3f9a1c2
  :VOTE:       M=approve F1=approve F2=reject
  :RULE:       master-veto-2of3/v1
  :ATTEMPT:    1
  :END:
** What changed
   cancel() returns after pushing to cancelQueue; a fill can still occur
   between acknowledgement and dequeue.
** Alternatives
   - remove from book synchronously under the book lock (slower p99)
** Worker rationale
   ...
** Ballots
*** M approve: cites src/cancel.ts:41, spec allows eventual cancel
*** F1 approve: ...
*** F2 reject: acceptance says "no fill after cancel"; cites PLAN.org p2
** Evidence
   [[file:.../worktrees/p2/src/cancel.ts::41][src/cancel.ts:41]]  checks: 3f9a1c2 exit 0
```

Dissent is kept even when a decision passes. F2's objection above is exactly
what the owner should see when sampling passed decisions.

### 3.3 Where decisions come from

The disclosure sources are the ones the rationale document lists. The worker's
own list cannot be trusted to be complete:

1. **Worker disclosure.** A worker cannot finish a phase with prose. It must
   call a `submit_phase` tool whose arguments include its decisions,
   assumptions and deviations from the plan. A Pi extension hook on the
   worker session stops it from settling until that tool has been called
   (bounded: two reminders, then the attempt fails).
2. **Reviewer discovery.** Each reviewer is asked first to list the
   behavioral choices it sees in the diff, *before* it reads the worker's
   disclosure. A discovered choice that matches no disclosed decision becomes
   a new record with `SOURCE: reviewer-discovered`. How often this happens is
   itself a measurement (§8).
3. **Observable triggers.** The conductor diffs the candidate against
   `BOUNDARIES`, dependency manifests and the acceptance files. Any hit
   without a corresponding decision creates a record with `SOURCE: trigger`.
   This source is code, not a model.

### 3.4 Classification and delegation policy

Each decision gets a class. The worker proposes it, a reviewer may raise it, and
nobody may lower it:

| Class | Example | Treatment |
| --- | --- | --- |
| `detail` | local helper structure, naming | recorded, not voted |
| `delegated-consequential` | a performance/consistency trade-off within the contract's intent | voted by reviewers (§4) |
| `reserved` | changes a requirement, a public interface, a guarantee in the acceptance list, or anything the plan marks reserved | **always** requires the owner, in every mode |

The `reserved` rule is what the rationale document means by "silence does not
authorize a choice that requires a response". No vote result can pass a reserved
decision.

## 4. The vote

### 4.1 The rule

Three voters: the master reviewer M and the fresh reviewers F1 and F2. Each
ballot is `approve` or `reject`, with a rationale and at least one evidence
citation (a file and line, a check result, or a plan clause).

> **A decision passes if and only if M approves and at least one of F1, F2
> approves.**

| M | F1 | F2 | result | why |
| --- | --- | --- | --- | --- |
| approve | approve | approve | pass | |
| approve | approve | reject | pass | 2 of 3; dissent recorded |
| approve | reject | approve | pass | 2 of 3; dissent recorded |
| approve | reject | reject | **fail** | fresh reviewers override the anchored one |
| reject | any | any | **fail** | the master holds the plan's intent and has a veto |

This is a two-thirds majority in which the master has a veto. The tally is
ordinary code: ballots in, outcome out, same result every time.

Validity rules, all enforced by code:

- A ballot with no evidence citation, a malformed ballot, or a reviewer that
  times out counts as **reject**, not abstain. Missing support is `FAIL`,
  matching the gate contract in OrgBrain issue #96.
- A ballot is bound to a revision. If the worker changes the candidate, every
  ballot on it is invalidated and the affected decisions are voted again.
- Votes judge trade-offs, not facts. A failing check blocks the phase whatever
  the vote says. One reproduced defect outweighs any number of approvals.

### 4.2 When a decision fails

1. The conductor sends the rejecting ballots to the **same worker session** as
   a repair request: change the implementation, or keep it and answer the
   objection with evidence.
2. The candidate is re-checked. Then only the affected decisions are voted
   again, by the same M, F1 and F2 for that phase. Reviewers stay fixed
   within a phase so that repairs converge instead of chasing a new
   reviewer's taste every round.
3. The attempt budget is **3 repair rounds per phase**. When it runs out, the
   decision becomes a human request with every ballot attached, and the phase
   is blocked.

### 4.3 Where Jev / SemIf fits

Not in v1. The tally is deterministic and needs no model. The natural later use
is a **ballot-validity gate**: does a ballot's cited evidence actually support
its claim? That is the claim-to-evidence shape OrgBrain #96 pilots. It would
run first in observation mode, beside the plain rule, and become enforcing only
if it measurably catches unsupported ballots.

## 5. The phase lifecycle

```text
READY ─▶ IMPLEMENT ─▶ SUBMITTED ─▶ CHECKS ─┬─fail──▶ REPAIR ─┐
  ▲                                         │                 │ (≤3 rounds)
  │                                         ▼                 │
  │                                   DECISIONS COLLECTED     │
  │                                         │                 │
  │                                         ▼                 │
  │                                       VOTE ──some fail────┘
  │                                         │
  │                              all pass / resolved
  │                                         ▼
  │            reserved open? ──yes──▶ AWAITING_HUMAN
  │                  │ no                    │ owner decides
  │                  ▼                       ▼
  │              INTEGRATE ─▶ INTEGRATED CHECKS ─▶ PHASE DONE
  │                                                     │
  └──────────────────── next phase ─────────────────────┘

budget exhausted anywhere ─▶ BLOCKED (explicit, with evidence)
```

- A worker's "done" is `SUBMITTED`. It starts verification and advances
  nothing by itself.
- `INTEGRATE` merges the phase worktree into the run's integration branch.
  The checks run again on the integrated result before the next phase starts,
  because two individually passing phases can fail together.
- Every transition is an event in `events.jsonl` recording the revision, the
  evidence and the rule version that allowed it.

## 6. Two modes, one pipeline

The modes do not fork the pipeline. They change only what happens at human
points and how much the conductor surfaces.

### 6.1 Delegate mode: the owner is away

For lunch, a meeting, or overnight. The loop runs unattended:

- `detail` decisions are recorded, and `delegated-consequential` decisions are
  settled by the vote.
- A `reserved` decision, or an exhausted repair budget, puts that phase in
  `AWAITING_HUMAN`. v1 runs phases serially, so the run waits there. Running
  independent phases in parallel is a later extension, not v1.
- On return, the owner opens `decisions.org` (`C-c m d`) and reads **only**:
  what is awaiting them, what passed with dissent, and a sample of what passed
  unanimously. The owner never has to read transcripts, though every decision
  links to the evidence and the session.

### 6.2 Co-work mode: the owner is at the desk

Same loop, but the owner watches and intervenes while it runs.

- The status buffer (`C-c m s`, rendered from `run.org`) shows every phase, its
  state, the current repair round, the last check results, the last vote, and
  the decisions made so far with their outcomes.
- From the status or decision buffer, the owner can:

| Command | Effect |
| --- | --- |
| steer | message to the current worker session, delivered through Pi's steer |
| override | approve or reject a decision; recorded as a human decision *beside* the ballots, never replacing them |
| resolve | answer an `AWAITING_HUMAN` request |
| amend | edit the phase contract; creates a new plan version and invalidates the evidence it affects |
| pause / resume | stop dispatching after the current agent turn |

Every command is written as a JSON file into `inbox/`. The conductor picks it
up, applies it as an event and regenerates the projections, which Emacs shows
through `auto-revert-mode`. v1 needs no socket protocol.

Steering, override and amend are deliberately different operations. Asking the
worker "would a queue help here?" is conversation. Rejecting weaker
cancellation semantics is a decision. Changing what "cancelled" means is a
contract revision. The log keeps them apart.

## 7. Emacs and Pi surface

### 7.1 Keys

These extend the `C-c m` prefix from the Pilish integration (#12):

| Key | Action |
| --- | --- |
| `C-c m r` | start a run from the current PLAN.org, choosing delegate or co-work mode |
| `C-c m s` | status buffer for the active run |
| `C-c m d` | `decisions.org` for the active run: the review surface |
| `RET` on a decision or phase | jump to evidence: diff, file:line, check output |
| `o` on a phase | open that agent's Pi session read-only through Pilish |

The decisions buffer is ordinary Org: TODO keywords (`AWAITING`, `PASSED`,
`FAILED`, `OVERRIDDEN`), tags for phase and class, and `org-sparse-tree` or
agenda views for "show me only what needs me".

### 7.2 Processes

```text
Emacs
  │ make-process: node conductor.js --run <dir> --mode delegate|cowork
  │ reads run.org / decisions.org (auto-revert); writes inbox/*.json
  ▼
Conductor (Node, Pi SDK)
  │ createAgentSession per agent, cwd = phase worktree
  │ SessionManager file-backed under <run>/sessions/
  │ worker extension: tool_call guard + settle guard + submit_phase tool
  │ reviewers: read-only toolset + submit_ballot tool
  ▼
git worktrees per phase, integration branch per run
```

The conductor is a separate process from Emacs, so the run survives closing
buffers, and it can be restarted from `events.jsonl`. Pilish keeps its own
job: interactive sessions. For a conductor-owned session it is only a viewer.
Two Pi processes appending to one session file is not safe, so steering goes
through the conductor.

### 7.3 Enforcement in v1, and its limit

The worker session's extension:

- blocks writes outside its worktree and to the phase's acceptance files
  (`tool_call`);
- blocks `git push` and edits to anything under the run directory;
- refuses to settle until `submit_phase` has been called (`agent_before_settle`
  with `continue: true`, bounded).

These hooks are **workflow guards, not a security boundary**. A worker with
bash could still get around them. The rationale document is explicit that
filesystem and process permissions are needed to protect control state. That
is out of scope for v1 and is recorded as a known gap, not claimed as done.

## 8. v1: make it work, and test the assumptions

v1 is judged on one question: **does the pipeline run end to end on a real
task, and are the design's assumptions about Pi, Emacs and reviewer voting
true?** Polish, performance and cost optimization come later.

### 8.1 In scope

- One run at a time, phases serial, one worker per phase.
- Conductor with `events.jsonl`, projections, and resume after a crash.
- Deterministic checks from `CHECKS`.
- Decisions from all three sources, and the M + F1 + F2 vote with the rule in
  §4.1.
- Both modes, with steer, override, resolve and pause. `amend` may be
  replaced in v1 by "stop the run, edit PLAN.org, start a new run".
- `C-c m r`, `C-c m s`, `C-c m d`, and read-only session viewing.

### 8.2 Out of scope

- OrgBrain integration: context packs going in, admission of decisions coming
  out. The decision record's shape should be kept admission-friendly so this
  is a later write, not a redesign.
- Jev / SemIf gates (§4.3).
- Two independent implementations per phase with a judge.
- Parallel phases, multiple concurrent runs, cost dashboards.
- Permission sandboxing of workers (§7.3).

### 8.3 Assumptions v1 must test

| # | Assumption | How v1 tests it | What would falsify it |
| --- | --- | --- | --- |
| A1 | The Pi SDK can drive worker and reviewer sessions headless for hours from a long-lived process | a real multi-phase run completes without manual restarts | sessions hang, leak, or lose state across repairs |
| A2 | Extension hooks can force structured submission | count phases ending without `submit_phase` | workers regularly end in prose or loop on reminders |
| A3 | Fresh reviewers find decisions the worker did not disclose | count `reviewer-discovered` records; owner samples 5 phases by reading the diff | zero discoveries while owner sampling finds undisclosed choices |
| A4 | The master-veto 2/3 rule changes outcomes usefully | count rejected-then-repaired decisions; owner rates a sample of passed and failed ones | votes are always unanimous, or the owner disagrees with most outcomes |
| A5 | `decisions.org` costs less attention than transcripts | owner's time to review a finished run, and whether they had to open a transcript | owner routinely needs transcripts to understand a decision |
| A6 | Emacs can show live status and view conductor-owned sessions while they are written | use both modes on the pilot run | viewer breaks on a file being appended; auto-revert churn is unusable |
| A7 | Resume from the log works | kill the conductor mid-phase and restart | completed gates rerun, or state diverges |

A falsified assumption is a useful result. It changes the design before more
is built on top of it.

### 8.4 Build order

Each step is usable on its own:

1. **Skeleton.** Conductor reads PLAN.org, runs one worker per phase in a
   worktree, runs `CHECKS`, logs events, renders `run.org`. Tests A1 and A7.
2. **Submission and triggers.** `submit_phase` tool, settle and tool guards,
   boundary triggers. Tests A2.
3. **Review and vote.** Master and fresh reviewers, ballots, tally, bounded
   repair. Tests A3 and A4.
4. **Emacs surface.** `C-c m r/s/d`, inbox commands, both modes, session
   viewing. Tests A5 and A6.
5. **Pilot.** One real task of three or more phases, run once in delegate mode
   and once in co-work mode, with the §8.3 table filled in from the results.

## 9. Open questions

- **Master reviewer on long runs.** Its session will compact. Should its prompt
  be rebuilt at each phase from the plan plus the decision log, keeping the
  session for continuity but not as the source of truth?
- **Decision de-duplication.** Three reviewers describe the same choice in
  three different ways. v1 can let reviewers attach to an existing decision ID;
  whether that is reliable is part of A3.
- **Cost.** Three reviewers per phase, plus revotes, plus a long-lived master.
  Is the reviewer count tied to a phase risk tag from the start, or measured
  first at full strength?
- **Where the code lives.** Inside this repository next to `init-pilish.el`, or
  as its own package that the Emacs module launches.
- **Model assignment.** Which models back M, F1 and F2 through the Vercel AI
  Gateway, and whether model diversity measurably changes A3 and A4.

## Related

- [Human judgment and programmed agent workflows](agent-evaluation-and-programmed-workflows.md):
  the rationale this design implements.
- [Emacs Pi integration PR #12](https://github.com/Angold-4/.emacs.d/pull/12):
  the Pilish sessions and `C-c m` prefix this extends.
- [OrgBrain issue #96](https://github.com/Angold-4/orgbrain/issues/96): the gate
  contract behind "missing support is FAIL" and the later ballot-validity gate.
- [Pi SDK](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/sdk.md)
  and [extensions](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/extensions.md):
  `createAgentSession`, `steer`, `tool_call`, `agent_before_settle`. The
  exact API surface is to be confirmed in build step 1.
