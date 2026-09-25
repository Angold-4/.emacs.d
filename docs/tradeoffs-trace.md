# tradeoffs-trace: a programmed review pipeline for Pi in Emacs

Status: **Proposal for review.** Nothing here is implemented. It describes the
first workflow built on the rationale in
[Human judgment and programmed agent workflows](agent-evaluation-and-programmed-workflows.md).
The first milestone is a single-phase prototype that tests the hard
integration boundaries (§11.1). A multi-phase interface and a comparison
against the current workflow follow only if that prototype holds.

## 0. What this is

`tradeoffs-trace` runs a multi-phase plan through Pi agents and does two things
a prompt-driven loop cannot promise:

1. It **enforces the loop**. Checks run, reviews happen, every stage has a
   deadline, and a phase advances only when recorded evidence satisfies a
   predicate that code evaluates (§6.3). It never advances because a model
   thinks the work "looks good enough".
2. It **traces trade-offs**. Choices made during implementation are collected
   as decision records from several independent sources, reviewed by
   independent agents, and presented in one Emacs view where the owner can
   accept them or **correct them mid-run** (§7.5).

What it does **not** promise is that every consequential choice is caught. The
worker may not disclose a choice, reviewers can share blind spots, and triggers
only see declared boundaries. The workflow reports an **observed miss rate**
from sampling and audits (§3.5, §11.4). It does not claim to know how many
choices went undiscovered.

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

- `CHECKS` are shell commands the conductor runs itself, always in a fresh
  checkout of a frozen candidate (§6.2). They must be safe to rerun, because
  recovery may rerun an interrupted check (§9.3).
- `BOUNDARIES` are path globs whose modification automatically creates a
  decision record (§3.3).
- `RESERVED` names choices the owner wants flagged, in addition to the
  standing reserved classes in §3.4. They are voted like any other decision;
  the owner is never waited for (owner-optional).
- `GATE` is the phase's expensive, live proof (a `--clean --build` of a
  whole stack, say). The conductor runs it itself, once per candidate whose
  checks, probe and three reviews have passed, and records the evidence;
  §6.5 is the normative account. `GATE_CLEANUP` is the command that releases
  what the gate took, run after it whatever the outcome. `#+TT_GATE_MINUTES`
  sets the gate's limit (default 30 minutes). No agent may run the gate or
  report its result.
- `#+TT_SECRETS` (plan level) names the credentials the plan needs, by name
  only. The conductor reads each value from its own environment, passes it to
  every agent as an environment variable, refuses a command containing one, and
  writes `***NAME***` wherever a value would otherwise land. See the runbook's
  "Secrets (credentials)" and `tt redact` for a run that already leaked one.
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
create run    – run id, run directory, meta.json (plan path, plan sha, mode,
    │           created, status)
    ▼
start         – launch the conductor as a detached daemon (§2.1); ask:
    │           delegate or co-work (switchable later)
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
│ ▸ tool: sh npm test -- cancel          │    round 2/3 · cand 7c1e0a4     │
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

- **Trace** (left): the live stream of the active agent (§9.4). It follows
  whichever agent is running; `a` chooses a specific agent of the run.
- **Status** (right): rendered from the control log. `RET` on a phase opens its
  diff; `c` its check output; `d` its decisions; `m` switches mode; `p`
  pauses.
- **Input** (bottom): every text is an **owner directive** (plan 01i): a
  numbered record (`OD-1`, …) steered at once to every live agent of the run
  (the worker and any reviewer mid-turn, bound to its attempt, §7.4) and quoted
  verbatim under "Owner directives (binding)" in every later prompt. A
  directive applies to its phase, or to the whole program with `C-u C-c C-c` or
  from a program buffer; `withdraw OD-n` withdraws one. See the runbook's
  "Steer it".

`C-c m s` focuses the run's tab. If the tab or any of its windows has been
closed, it rebuilds the layout. After an Emacs restart, the same command
reconnects to the run's conductor and reopens it from the run directory.

### 1.4 Which run a command means

Several plans and runs can be open at once. Every `C-c m` command resolves its
run the same way:

1. A tradeoffs-trace buffer (trace, status, input, decisions, revise, session
   view) has a buffer-local run id. Use it.
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
| **Conductor** (code, not a model) | whole run | everything | the control log, worktrees, candidates, projections |
| **Worker** | one phase; resumed for its own repairs | phase contract, repository, prior accepted decisions, owner notes and corrections | code in its phase worktree; `submit_phase` |
| **Master reviewer** M | whole run, one Pi session | plan, every phase's diff and records, its own earlier reviews | `submit_review` |
| **Fresh reviewers** A, B | one phase (kept across that phase's repair rounds) | phase contract, frozen candidate, diff, records under review | `submit_review` |

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

### 2.1 Processes

```text
Emacs ──unix socket <run>/conductor.sock──▶ Conductor daemon (Node, detached)
  │  live events in, commands out                │
  │  inbox/ files when the daemon is down        ├─ pi --mode rpc  (worker)
  └─ reads views/ and sessions/ to rebuild       ├─ pi --mode rpc  (M, A, B)
                                                 └─ shell commands it owns (§2.2)
```

- The conductor is a **detached daemon**, not a child of Emacs. The run
  survives closing buffers or restarting Emacs. It holds `conductor.lock` with
  `flock`, so there is exactly one conductor per run.
- Each agent is a child `pi --mode rpc --extension tradeoffs-trace.ts` process
  in its own process group, spoken to over Pi's RPC protocol. The extension
  provides `submit_phase`, `submit_review` and the guards in §9.5.
- Reviewers run with read-only built-in tools and no shell: `--tools
  read,grep,find,ls,submit_discovery,submit_review`. Pi's `--tools`
  allowlist applies to extension tools too, so the submission tools must be
  named explicitly, or reviewers could never submit. Checks are the
  conductor's job, not theirs.
- **Tool-set assertion.** Before dispatching any prompt, the conductor obtains
  the agent's actual active tool set and refuses to dispatch unless it equals
  the expected set for that role exactly. RPC `get_state` does not include
  tools in Pi 0.87.0 (checked), so the extension reports
  `pi.getActiveTools()` over the run socket at `session_start`. A mismatch
  is a launch failure, not a warning.
- **Every role uses an explicit allowlist**, never a denylist. The extension
  registers all of its tools in every agent, and Pi's default set omits
  `grep`, `find` and `ls`. So `--exclude-tools bash` gives a worker the
  reviewer submission tools and no search tools. Launch sets:

  | Role | `--tools` |
  | --- | --- |
  | worker | `read,edit,write,grep,find,ls,sh,submit_phase` |
  | reviewer | `read,grep,find,ls,submit_discovery,submit_review` |

### 2.2 The conductor owns every shell command

Killing Pi's process group does **not** kill the commands Pi started. Pi's
built-in bash tool spawns each command `detached` (its own process group) on
non-Windows platforms. An isolated probe against Pi 0.87.0 confirmed it: after
the Pi group was killed, the shell kept running. So:

- Workers run without the built-in bash tool: it is simply absent from their
  `--tools` allowlist (§2.1).
- The extension registers a replacement shell tool, `sh`. It forwards each
  command to the conductor over the run's socket and streams the output back.
- The **conductor** spawns the command in a new process group and records the
  pgid in an intent event *before* the command starts. It applies the
  per-command deadline (§8.1) and kills the group on expiry or cancellation.
- Pi's `edit`, `write` and `read` tools run inside the Pi process, so killing Pi
  ends them.

Ownership alone does not rule out an escaped descendant, for example one that
calls `setsid`. The prototype **assumes cooperative workers**: agents that do
not deliberately evade control. For those, two measures are cleanup and
detection. They are not containment:

- **Sweep (cleanup).** After any cancellation, and before freezing a candidate,
  the conductor lists processes whose working directory or open files lie
  under the worktree (`lsof +D <worktree>`), kills them, and records what it
  killed. A sweep that found survivors marks the worktree `tainted`, and the
  next attempt starts from a clean checkout of the last candidate. A detached
  process can change directory, close its files and reopen a path later, so
  an empty sweep does not prove no process survived.
- **Integrity verification (detection).** Checks and reviews never read the
  live worktree (§6.2). Before and after every check and review, the
  conductor verifies that the checkout still matches its candidate commit
  exactly (no modified, added or deleted files, ignored build output
  excepted). A mismatch invalidates that gate's result and marks the run
  `integrity-violated` for the owner.

A process running under the same OS account can still write to any path the
owner can, including candidate checkouts. Read-only permissions protect against
accidents, not adversaries. Real containment needs a separate user, container
or sandbox (§9.5).

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

The conductor checks that the fields are present, not that they are good. Their
quality is part of what the prototype and the pilot measure (§11).

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
| `reserved` | changes a requirement, public interface, guarantee in the acceptance list, persistence format, dependency, or anything in `RESERVED` | voted by reviewers like `delegated` (§5), and **flagged** for the owner in the status and decision view |

A reserved decision never waits for the owner: a long-running run must not
stall on someone who is not watching. The owner reviews flagged decisions if
they care, and overrides one through the input box (§7.4). (Before
owner-optional, a reserved decision was settled only by an owner request, and
a run whose reviewers approved everything still stopped for the owner.)

### 3.5 Sampling what the classifier might hide

A `detail` label skips the vote, so the classifier could hide exactly the
choices the owner cares about. The decision view (§10) therefore always
includes a **sample**, drawn by the conductor rather than a model:

- a share of `detail` decisions (in the prototype and pilot: all of them,
  since volume is unknown);
- **unreferenced changes**: diff hunks that no decision or finding cites. The
  conductor computes these mechanically from hunk ranges and record citations;
- a share of unanimously accepted decisions.

When the owner marks a sampled item "should have been surfaced", that is a
recorded miss. The miss rate within the sample is an **observed** rate. Misses
outside the sample are estimated by the independent audit in §11.4, not by
this sample.

## 4. Findings

### 4.1 What a finding is

A reviewer's allegation that the candidate is wrong:

- `defect`: behaviour is incorrect, whether or not the checks catch it. A race
  the tests miss is the typical case.
- `contract`: the candidate violates the phase's goal, acceptance or a
  recorded accepted decision.
- `integration`: the candidate passed on its own but failed its integration
  probe (§6.4). The conductor raises this kind itself.

A finding needs evidence: file and line, a scenario, a check result, or a plan
clause. It has a severity. `blocking` is the default, and only the owner can
lower a finding to `advisory`.

A ballot on a decision can carry the objection "this violates the contract".
When it does, the conductor opens a linked `contract` finding and **suspends the
vote** on that decision until the finding is closed.

A reviewer may attach a proposed reproduction (a test file or a command). The
conductor runs it against a fresh checkout of the candidate. A reproduction
that fails as claimed marks the finding `reproduced`. One that passes does
**not** close the finding; it is shown to the raising reviewer as evidence.

### 4.2 How a finding closes

| Disposition | Requires | Recorded |
| --- | --- | --- |
| **repaired** | a new candidate; checks pass on it; the **raising reviewer** confirms, in its review of that candidate, that the finding no longer holds | candidate, confirming review |
| **disproved** | counter-evidence (a citation, or a test demonstrating the property); the **raising reviewer** withdraws the finding | the evidence, the withdrawal |
| **accepted** | **the owner only**. For `contract`, this means amending the contract (§7.3); for `defect`, a recorded risk acceptance with its scope | owner command, bound to versions |

Other reviewers can agree or disagree in their reviews, and the view shows it.
They cannot close someone else's finding. An `integration` finding closes as
repaired when a later candidate's integration probe passes (§6.4). The probe
runs before acceptance, so closing it never depends on acceptance. If a finding
is still open when the phase's repair budget runs out, it becomes an owner
request.

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
- A ballot is bound to its versions (§7.1). A ballot on a superseded candidate
  is discarded, not counted.

### 5.2 When a decision fails

The rejecting ballots go to the worker's session as a repair request: change
the implementation, or keep it and answer the objection with evidence. The
next candidate goes through the whole lifecycle again (§6). A decision that
keeps failing until the budget runs out becomes an owner request carrying every
ballot.

### 5.3 Where Jev / SemIf fits

Not in v1. The tally is deterministic and needs no model. The natural later use
is a **ballot- and finding-validity gate**: does the cited evidence support the
claim? That is the claim-to-evidence shape OrgBrain #96 pilots. It would run
first in observation mode, and become enforcing only if it measurably catches
unsupported claims.

## 6. Phase lifecycle and acceptance

This section is the single authoritative account of how a phase moves from
ready to done. Every other section refers to it.

### 6.1 Transitions

```text
READY
  │ start worker attempt n (worktree based on the integration head)
  ▼
IMPLEMENTING ──deadline / no_submission──▶ attempt failed ──▶ REPAIRING
  │ submit_phase
  ▼
FREEZING      quiesce worker, sweep, commit, create read-only candidate (§6.2)
  ▼
CHECKING      CHECKS in a fresh disposable checkout of candidate C
  │ any failure ─────────────────────────────────────────────▶ REPAIRING
  ▼
PROBING       merge C onto the integration head H on a disposable probe
  │           branch, giving I; CHECKS on a fresh checkout of I (§6.4)
  │ conflict or failure → raise `integration` finding ──────▶ REPAIRING
  │ success → close open `integration` findings as repaired
  ▼
REVIEWING     M, A, B review C under contract K (two turns each)
  ▼
RESOLVING     decisions voted; findings, owner requests, corrections open?
  │ open items remain and budget remains ────────────────────▶ REPAIRING
  │ open items remain, budget exhausted ─────────────────────▶ AWAITING_OWNER
  │ accept(C, K) holds (§6.3), and the contract declares no gate
  ├──────────────────────────────────────────────────────────▶ ACCEPTED(C)
  │ accept(C, K) holds and the contract declares a `GATE` command
  ▼
GATING        the conductor runs the gate itself, once per candidate (§6.5)
  │ the gate fails → blocking `integration` finding with its log tail
  │ ────────────────────────────────────────────────────────▶ REPAIRING
  │ the gate passes (or an identical tree reuses a pass)
  ▼
ACCEPTED(C)   atomically: corrections addressed by C recorded resolved
  ▼
PUBLISHING    move the integration branch from H to the probed I (§6.4)
  ▼
DONE(I)       the next phase starts from I

REPAIRING     the same worker session, a new attempt with the open items
              ──▶ IMPLEMENTING (consumes one repair round)
AWAITING_OWNER  parked; no execution; leaves only through an owner command
BLOCKED         explicit terminal state with cause and evidence
```

Every transition is an event in the control log naming the evidence and the
rule version that allowed it.

### 6.2 The freeze boundary

A commit hash does not freeze a live worktree: the worker, or anything it
started, could keep editing files while they are being checked. So when
`submit_phase` arrives, the conductor:

1. **Quiesces the worker.** It returns the tool result, sends RPC `abort`,
   waits for `agent_settled`, and ends the worker process. The session file
   remains, and a repair resumes it in a new process. A worker process never
   outlives its attempt.
2. **Sweeps** the worktree for surviving processes (§2.2).
3. **Commits** the worktree itself, with trailer `TT-Action: <id>`. The worker
   cannot commit. That commit is the candidate C.
4. **Materializes** a read-only checkout of C under `<run>/candidates/<sha>/`
   for reviewers. Each check or reproduction gets its own disposable checkout
   of C, created fresh and deleted afterwards, because builds write files.

Checks and reviews only ever see checkouts of C. Nothing reads the live
worktree after the freeze.

### 6.3 The acceptance predicate

Code evaluates this predicate for candidate C under contract version K. It is
the only way a phase reaches `ACCEPTED`:

```text
accept(C, K) ⇔
      every CHECKS command passed on a fresh checkout of C
  ∧   the integration probe of C onto the current head H passed, giving I
  ∧   M, A and B each submitted a valid review bound to (C, K)
        — required even when there are no decisions to vote on
  ∧   no finding is open with severity blocking
  ∧   every decision on C is detail, passed by vote bound to (C, K),
        or resolved by the owner bound to (C, K)
  ∧   no owner request is open
  ∧   every open owner correction X satisfies addressed(X, C, K)

addressed(X, C, K) ⇔
      X is bound to contract K
  ∧   M, A and B each stated, in their review bound to (C, K),
        that X is honored — none states it is not
```

Corrections are assessed, not closed, before acceptance, so the predicate never
depends on its own outcome. The `ACCEPTED(C)` event itself records every
addressed correction as `resolved`, in the same log append.

```text
done(phase) ⇔ accept(C, K) ∧ the integration branch points at the probed I
```

Every input to `accept` is evaluated before `ACCEPTED`. Nothing it requires is
produced by acceptance or after it. That is the property that keeps the
lifecycle free of circular dependencies, and phase 0 of the implementation
plan tests it.

### 6.4 Integration

Integration is split into a **probe** before acceptance and a **publish** after
it, so that the integration result is part of the evidence acceptance requires.

**Probe** (state `PROBING`):

1. Record `probe.intent` with C and the current integration head H.
2. Create the disposable branch `tt/<run>/probe/<C>` at H, and merge C into it
   with trailer `TT-Action: <id>`, giving I. Phases are serial and each
   worktree starts from H, so this is normally a fast-forward and I equals C.
3. Run `CHECKS` on a fresh checkout of I.
4. On success, record `probe.passed(C, H, I)` and close open `integration`
   findings as repaired. On a conflict or failure, raise a blocking
   `integration` finding with the output and return the phase to
   `REPAIRING`. The integration branch is never touched by a probe.

**Publish** (state `PUBLISHING`, after `ACCEPTED`):

1. Record `publish.intent(H, I)`.
2. Move the integration branch from H to I **only if it still points at H**
   (`git update-ref <branch> I H`). The published commit is exactly the one
   the probe checked, so no further checks are needed.
3. Record `DONE(I)`. If the branch no longer points at H, which cannot happen
   with serial phases but is checked anyway, the probe result is stale: the
   phase returns to `PROBING` against the new head.

### 6.5 The gate (plan 01f)

Some phases need a proof that is too expensive for the ordinary check loop: the
atlas plan's `deploy/atlas.sh … --clean --build` of a 40-service stack takes
about 15 minutes. Run inside an agent's attempt, that command was paid for on
every repair round (82 minutes of docker across 13i/13j), was sometimes killed
by the per-command limit before answering, and — because the agent had to
produce the live proof — invited substitutes: a sentinel `code_sha`, a
`pending_owner_live_run`, a fingerprint-only record ([runtime evidence](01_ref_runtime_f8ecf5e3.md) §1, §6). So the
conductor runs the gate itself, and only the conductor produces its evidence.

- **Declaration.** A phase declares `GATE` (and optionally `GATE_CLEANUP`) in
  its property drawer; `#+TT_GATE_MINUTES` sets the limit (default 30). A
  phase without a gate accepts on `accept(C, K)` alone, exactly as before.
- **One run, after everything else.** The gate is dispatched from `GATING`,
  which is only entered when `accept(C, K)` already holds. It runs in a fresh
  checkout of the candidate merged onto the current integration head — the
  integration the probe verified — with the plan's secrets in its environment.
  A gate that exceeds its limit has its process group killed and counts as a
  failure.
- **The machine-wide lock.** The gate takes `~/.tradeoffs-trace/gate.lock`
  before it merges the candidate into its checkout and holds it through the
  command, the cleanup and the record, so two phases — in one program or in
  two runs — never have gate checkouts or build commands live at once. A
  second gate waits for the first; a crashed holder releases the lock through
  the OS.
- **The record.** The conductor writes `checks/<sha>/gate.json` (candidate and
  base SHA, the merged tree, the command, the exit status, the gate command's
  own duration, start time, the log's sha256, and the cleanup's own exit and
  duration) and `checks/<sha>/gate.log` (stdout and stderr, redacted). The
  `ACCEPTED` event a passing gate releases is the only thing that lets
  acceptance proceed. The cleanup runs under its own limit and its time is
  never folded into the gate's duration — the lock is held for both. The rule
  is exactly: **the cleanup runs whenever the gate command ran (pass, fail or
  timeout); if the candidate no longer merges, the gate never starts and
  nothing is cleaned.** Such a candidate is recorded as `not started` (with no
  exit status and no duration), never as an exit-less run.
- **The run-or-reuse decision is taken under the lock.** The gate takes the
  lock, then asks whether this tree and command already have a verified pass;
  only if not does it merge the candidate and run the command. Two candidates
  with the same tree never both run the gate.
- **A failure is a blocking `integration` finding** whose evidence is the
  log's last 60 lines. The phase returns to `REPAIRING` (or `AWAITING_OWNER`
  when the budget is exhausted), and the repair prompt shows the worker the
  conductor's record and log tail verbatim.
- **Reuse, not repeat, and only for the same question.** A record is
  evidence only when it answers *this* gate: the same candidate tree, the same
  `:GATE:` command, and a `gate.log` that still hashes to the sha256 the
  record carries. A repair attempt that changes nothing freezes a new commit
  with the same tree, so its gate is reused rather than paid for again; a
  record whose log is missing or edited, whose command differs (the plan was
  re-read or the contract amended), or whose tree is another candidate's makes
  the gate rerun. A record that was itself written as a reuse counts as
  passing — its log is the verified copy of the run behind it, so a chain of
  reuses still saves the build. A failed gate is always rerun.
- **A record is never rewritten.** When the *same* candidate is gated again
  (a stale publish sends the phase back through `PROBING` with its reviews
  still valid), its own passing record — verified against the same tree and
  command — is accepted in place. The record keeps the head its evidence was
  produced at, and both the control log's completion record and the status/
  `tt summary` citation name the head being accepted now next to it, so a pass
  is never read as evidence for a head it was not produced against. For a
  reused *other* candidate's record, the new record's `baseSha` is the head
  being accepted and `reusedFromBaseSha` names the head the command ran on.
- **No agent may produce this evidence.** The worker's prompt and every
  reviewer's prompt — turn 1 and turn 2 — say the conductor runs the gate
  and that running it, reporting its result, or substituting evidence for it
  is forbidden. Reviewers are shown the gate record when one exists and its
  log still matches.
- **The JSON schemas trail the change.** `schemas/**` is outside this phase's
  boundary, so `event.schema.json` does not list `GATE_REQUIRED`,
  `GATE_FAILED` or `GATE_INTERRUPTED`, and `plan.schema.json` /
  `phase-contract.schema.json` do not list `gate`/`gateCleanup` (they already
  lag the emitted plan's `repo`, `deadlines`, `secrets` and directive
  fields). Nothing in the conductor validates a produced record against them:
  the core's `Event` type and `buildContract` are the enforced vocabulary, and
  the schema tests exercise their own fixtures.

## 7. Versions and authority

### 7.1 Everything is bound to what it evaluated

Every ballot, review, finding disposition and owner command carries:

```text
run id · phase id · candidate sha · contract version · record id · record version
```

- **candidate sha**: the freeze commit (§6.2).
- **contract version**: the plan snapshot number, plus the sha256 of the
  phase's section in it.
- **record version**: increments whenever a record's content, class or linked
  findings change.

A command or ballot whose binding no longer matches is **rejected visibly**. It
gets a log event, a message in the echo area, and a line in status saying what
changed ("decision D-7 changed v2 → v3 since you viewed it"), and the view is
refreshed. Nothing is applied to a version the sender did not see.

### 7.2 v1: a new candidate invalidates all of the phase's evidence

Deciding which evidence survives a change is hard to get right. In v1, any new
candidate goes through the full lifecycle again: all checks and all three
reviews. Every ballot and closure confirmation on the old candidate is
discarded. Open findings carry forward, to be re-examined by their raising
reviewer on the new candidate. Steering therefore costs a full re-review if it
changes code, and that cost is intended.

### 7.3 Amending the contract

`amend` writes a new plan snapshot. For v1:

- The current phase's evidence is entirely invalidated, and the phase returns
  to checks and review under the new contract version.
- Provisional later phases simply read the new snapshot.
- Amending the contract of a phase **already integrated** follows the same rule
  as correcting one (§7.5): supported only while no later phase has been
  integrated; otherwise a new run is required.

### 7.4 Owner commands

Commands differ in where their effect lands, and that decides what recovery can
promise (§9.3):

| Command | Effect | Kind | Bound to |
| --- | --- | --- | --- |
| steer | message delivered to the running worker through Pi's RPC `steer` | **external delivery** | worker attempt id |
| note | queued for the next worker attempt | conductor state | phase |
| directive | every input-box text, recorded as `OD-n` (a program-wide one as `ODP-n`): steered at once to every live agent, quoted in every later prompt, binding until withdrawn (§7.6) | conductor state (plus external delivery) | phase (or the whole program) |
| resolve | answer an owner request (choose an option or write one) | conductor state | request id and version, candidate, contract |
| override | approve or reject a delegated decision; recorded **beside** the ballots | conductor state | decision version, candidate, contract |
| accept-finding | the owner's disposition of a finding (§4.2) | conductor state | finding version, candidate, contract |
| revise | correct a decision or finding and repair the phase (§7.5) | conductor state (then its own attempt) | record version, candidate, contract |
| amend | new contract version (§7.3) | conductor state | contract version being replaced |
| unneeded | mark an owner request as "did not need me" (a pilot metric) | conductor state | request id |
| pause / resume / mode | control only | conductor state | run |

Steering, overriding, revising and amending are deliberately separate. Asking
"would a queue help here?" is conversation. Rejecting the queue is a decision.
"This trade-off doesn't make sense; do it this way" is a correction. Changing
what "cancelled" means is a contract revision. The log keeps them apart.

### 7.6 Owner directives

Every text the owner sends through an input box is an **owner directive**: a
numbered, logged record (`OD-1`, `OD-2`, …) that is part of its phase until
withdrawn. It is (1) steered at once to **every live agent of the run** — the
worker and any reviewer mid-turn — with each delivery recorded; (2) included
verbatim, newest last, under **Owner directives (binding)** in every later
prompt: every worker attempt and repair, both reviewer turns, and any
re-dispatched or fresh agent; and (3) **binding on reviewers as part of the
contract** — a candidate that violates one is a blocking `contract` finding
citing the directive id, and a candidate that follows one cannot be faulted for
doing so, even where the plan's text says otherwise.

At `AWAITING_OWNER` the text still does what §7.5's correction does (resolves
the open requests and grants 3 rounds) *and* becomes a directive. A
withdrawal — `withdraw OD-n` in the input box — steers the live agents that it
no longer applies and drops it from every later prompt; an unknown or malformed
id is refused with the reason (a near-miss is never inverted into a new
ruling). Trailing prose after the id is accepted.

A directive applies to its own phase and is numbered `OD-n`. Sent from a
program buffer, or with `C-u` in a run's input box, it applies
**program-wide**: the program records it under its own `ODP-n` id, every
running node is steered now, every node started later is started with it, and
the program's event log carries it (so it survives a restart). A node never
mints or renumbers a program id — it forwards the text and adopts the record
the program pushes, so one id names one ruling at both levels and
a `withdraw ODP-n` from any node retires it on every node. The input header
states the scope before sending; the status view shows each directive with its
scope, whether it is in force, and its delivery state per agent.

### 7.5 Correcting a decision mid-run: revise

From any entry in the decision view (pending, accepted, sampled `detail`, or a
finding), the owner can press `r`, say what does not make sense, and describe
the intended change. The conductor applies the correction and the pipeline
continues on its own.

**The revise buffer** shows the consequence before anything is sent:

```text
Revise: Batch cancels per tick instead of one lock per request     [p2 · D-p2-05 v2]

Your correction:
  A lone cancellation must not wait for the next tick. Keep immediate
  handling even if batching improves throughput.

This changes the contract?   [ ] no, it's feedback within the contract

Submitting will:
  - pause phase 2 (cancel review round 2 in progress)
  - start a repair attempt with your correction, the original contract and
    the decision's evidence; grant 3 new repair rounds
  - rerun checks and all three reviews on the new candidate
  - phase 3 has not started; nothing else is affected

C-c C-c submit      C-c C-k cancel
```

**Feedback versus a changed requirement.** A correction is feedback within the
existing contract unless the owner says otherwise. The conductor never turns
conversational feedback into a new requirement on its own authority:

- If the owner ticks "this changes the contract", the buffer shows the proposed
  edit to the phase's section of the plan as a diff. Submitting then performs
  an `amend` (§7.3) together with the correction, as one explicit act.
- If the owner leaves it unticked but a reviewer or the worker judges that the
  correction conflicts with the contract, the conductor does not guess. It
  raises an owner request showing the conflict and the contract edit that would
  resolve it.

**What happens on submit:**

1. The correction becomes a record linked to the decision. The original
   decision is preserved and marked `superseded by correction C-…`.
2. Affected execution is paused. A running worker attempt or review is
   cancelled (§8.2), and its partial results are discarded.
3. A **new repair allowance** is granted: 3 rounds, recorded as
   `budget.granted by correction C-…`. It is independent of any exhausted
   budget, so a correction made after three failed attempts can still be
   acted on.
4. The worker session gets a repair attempt containing the original contract,
   the decision and its evidence, and the correction **verbatim**.
5. The new candidate goes through the whole lifecycle (§6). Each review of
   that candidate must state whether the correction is honored. The
   correction counts as **addressed** by the candidate when all three reviews
   say it is honored and none says it is not (§6.3). Acceptance requires every
   open correction to be addressed, and the `ACCEPTED` event records it as
   `resolved`. A single "not honored" keeps the candidate from being accepted,
   exactly like a finding. If the new allowance runs out, it becomes an owner
   request.

**Already integrated work.** Phases are serial in v1, so every later phase
depends on the corrected one:

- If the corrected phase is `DONE` and **no later phase has been integrated**,
  the conductor cancels the later phase's in-flight attempt, moves the
  integration branch back to the head H recorded in the corrected phase's
  publish event, and reopens the corrected phase. Its repaired candidate is
  probed and published as usual, and the later phase restarts from the new
  integration head.
- If **a later phase has been integrated**, rebuilding that chain is not
  supported in v1. The revise buffer says so before submission and offers to
  start a new run from an amended plan instead.

The entry in the decision view then tracks the correction:

```text
Correction received → repairing → checking → reviewing → resolved
original: D-p2-05 v2 (preserved)   replacement: candidate 91be3d0 · D-p2-05 v3
```

## 8. Bounds, cancellation and termination

### 8.1 Every stage has a deadline

Every stage below has a conductor-enforced deadline:

| Stage | Default (plan may override) | On expiry | Resulting state |
| --- | --- | --- | --- |
| worker attempt, start to `submit_phase` | 45 min, plus a per-attempt token cap | cancel (§8.2) | attempt `timed_out`; consumes a repair round |
| settle reminders | 2 continuations | attempt fails | attempt `no_submission`; consumes a repair round |
| each `sh` command the worker runs | 10 min | kill its group | tool result `timeout` returned to the worker |
| freeze (quiesce, sweep, commit) | 2 min | force-kill, sweep, mark worktree `tainted` | attempt failed |
| each check command | 10 min | kill its group | check `failed: timeout` |
| integration probe (merge plus its checks) | as for checks, per command | kill its group; discard the probe branch | `integration` finding: timeout |
| each review | 15 min | cancel, re-dispatch once | then the phase is `BLOCKED: reviewer unavailable` |
| the phase gate command (`GATE`) | 30 min (`#+TT_GATE_MINUTES`) | kill its process group, run `GATE_CLEANUP` | gate failed: blocking `integration` finding |
| reproduction command | 5 min | kill its group | reproduction `inconclusive` |
| repair rounds per phase | 3, plus 3 per owner correction | — | open items become owner requests |
| run execution budget | from `TT_BUDGET` (wall and tokens) | stop dispatching | run `PAUSED: budget` until the owner resumes it with more budget |

The execution budget counts only time spent executing. A phase parked in
`AWAITING_OWNER` consumes nothing, because waiting for a person is an explicit
state, not work.

### 8.2 Cancellation

- **Agent**: send RPC `abort`, wait 30 s, then `SIGTERM` the Pi process group,
  wait 10 s, then `SIGKILL` it. Then kill every shell group the conductor
  started for that agent (§2.2), then sweep the worktree.
- **Check or reproduction**: started by the conductor in its own process group;
  `SIGTERM`, 10 s, `SIGKILL`; the disposable checkout is deleted.
- Every cancellation writes an event naming the stage, the reason, the signals
  sent and anything the sweep killed.

### 8.3 Why an unattended run terminates

The phase list is finite. Each phase has at most 1 + 3 candidate rounds, plus 3
per owner correction, and corrections only arrive from the owner. Each round is
bounded by the worker, freeze, check and review deadlines, and the execution
budget caps the total. So an unattended run always reaches one of `DONE`,
`BLOCKED`, `PAUSED: budget`, or `AWAITING_OWNER` in bounded execution time.
None of those states advances without a recorded cause.

## 9. State, recovery and the Emacs surface

### 9.1 The run directory

```text
~/.tradeoffs-trace/<project>/<run-id>/
  meta.json         plan path, plan sha, mode, status, created, last activity
  conductor.lock    held with flock by the one conductor for this run
  conductor.sock    live events and commands
  plan/v1.org …     immutable plan snapshots
  events.jsonl      control log: state transitions only, fsync per event
  stream/           live agent events, one file per agent; not fsynced, rotated
  views/            status.org, decisions.org — regenerated, never edited
  inbox/            owner commands, one file each: <command-id>.json
  inbox/applied/    commands already applied
  sessions/         Pi session files, one per agent
  candidates/       read-only checkouts of frozen candidates
  checks/           check and reproduction output, by candidate
  checks/<sha>/gate.json  the conductor's own gate record, when the phase
  checks/<sha>/gate.log   declares a gate (§6.5) — never an agent's evidence
```

Workers cannot write here. Their `tool_call` guard blocks it (§9.5), within the
limit stated there.

### 9.2 Two logs with different jobs

- **`events.jsonl`, the control log.** Every state transition, intent,
  completion and applied command. It is fsynced per event and is the only
  source of truth for recovery. It stays small.
- **`stream/`.** Token deltas, tool progress and other high-volume RPC events,
  forwarded to Emacs over the socket and appended here without fsync, so that a
  reopened trace can show the tail of a live turn. Losing the stream loses
  display detail, never state.

### 9.3 Crash-safe recovery

**One conductor per run** (§2.1). Every external effect has an **action ID**, an
**intent event** written before it and a **completion event** written after.
Each has a reconciliation for "intent recorded, completion missing":

| Effect | Reconciliation on restart |
| --- | --- |
| create worktree | path exists at the recorded base → record done; otherwise remove the partial worktree and recreate it |
| agent attempt | kill the Pi group and every recorded shell group; sweep. Mark the attempt `interrupted`. Worker: new attempt on the same session file with an interruption note. Reviewer: discard; start a new review |
| freeze commit | worktree HEAD carries trailer `TT-Action: <id>` → record it; otherwise redo the freeze |
| check run | mark `interrupted` and rerun (checks are required to be rerunnable) |
| probe | discard the probe branch and its checkout; mark the probe `interrupted`; probe again (the integration branch was never touched) |
| gate | kill the gate's recorded process group and its cleanup's; discard the gate's checkout; mark the gate `interrupted` and rerun it — an interrupted gate is neither passed nor failed (§6.5) |
| publish | integration branch points at I → record `DONE(I)`; still at H → retry the compare-and-swap; anywhere else → the probe is stale, return to `PROBING` |

**Owner commands come in two kinds, with different guarantees:**

- **Conductor-state commands** (everything in §7.4 except steer). Applying one
  *is* appending its event to the control log. If the command ID is already in
  the log, the command is only moved to `applied/`. Otherwise it is applied
  (logged), then moved. These are applied **exactly once**, because the log
  append is the effect.
- **External delivery** (steer). The conductor writes `deliver.intent` with the
  command ID, sends the RPC `steer`, and writes `deliver.done` when Pi
  acknowledges it. Pi has no receiver-side deduplication, so a crash between
  send and acknowledgement leaves the outcome unknown. Recovery **does not
  resend**. By then the attempt it was bound to has been interrupted anyway.
  The command is marked `delivery uncertain` in status, with one key to re-send
  its text as a note to the next attempt. **Steering is at most once, or
  explicitly uncertain. It is never "exactly once".**

**Gates have three outcomes: `passed`, `failed` and `interrupted`.** Only a
recorded completion event counts as `passed`. A gate whose completion cannot be
established is `interrupted` and is rerun or reconciled; it is never assumed.

**Fault injection.** `TT_CRASH_AT=<boundary>` makes the conductor exit at a
named point: before an effect, after the effect but before its completion
event, after a command event but before its inbox file moves, and between
`steer` and its acknowledgement. The suite runs every boundary and asserts:

- no conductor-state effect is applied twice;
- no steer is delivered twice; uncertain deliveries are marked as such;
- interrupted checks, reviews and attempts may rerun, and are never counted as
  passed;
- the final accepted state equals that of an uninterrupted run, apart from the
  reruns.

A separate test force-kills Pi (`SIGKILL`) while a worker `sh` command is
running, then asserts that the command's group is gone, the sweep finds nothing
under the worktree, and the frozen candidate checkout is unchanged.

### 9.4 The live trace and session history

Pi writes a message to its session file only when the message ends. Live text
and tool progress exist only as RPC events. So the trace has two sources:

- **Live:** the conductor forwards each agent's RPC events over
  `conductor.sock`, and the trace buffer renders them as they arrive.
- **History:** when a trace is reopened after a restart, it is rebuilt from the
  agent's Pi session file (completed messages) plus `stream/` for the tail of a
  turn still in progress.

Pilish's `pilish-open-session-file` resumes a session file as a **live** Pi
process. Opening a session the conductor owns would put two writers on one
file, and even opening a finished one would change a record kept as evidence.
So the trace and `tt-session-view` are tradeoffs-trace's own read-only
renderers: message text, and tool calls folded to one line each. No Pilish
command that resumes or mutates a session is reachable from these buffers.

**Continue in Pilish** (only after the run is finished, and deliberately
invoked): copy the session file into Pi's session directory as a new file,
then open *the copy* with Pilish. The run's original stays unchanged.

### 9.5 Worker guards, and their limit

The worker's extension:

- blocks writes outside its worktree and to the phase's acceptance files, and
  blocks `git push`, `git commit` and anything under the run directory
  (`tool_call`, applied to `edit`, `write` and `sh`);
- refuses to settle without `submit_phase` (`agent_before_settle`, bounded);
- replaces the built-in bash tool with the conductor-owned `sh` (§2.2).

These hooks are **workflow guards, not a security boundary**. A shell command
can still get around a path check. Protecting control state needs filesystem
and process permissions, which v1 does not implement. This is recorded as a
known gap, not claimed as done. In v1 the freeze boundary (§6.2) stops a
**cooperative** worker's continued edits from reaching evidence, and
integrity verification (§2.2) **detects** tampering with a candidate checkout.
Neither contains a process that deliberately evades control.

### 9.6 Keys

These extend the `C-c m` prefix from the Pilish integration (#12):

| Key | Action |
| --- | --- |
| `C-c m r` | in a plan buffer: validate, snapshot, create and start a run; open its workspace |
| `C-c m s` | focus or rebuild the run's workspace (resolved per §1.4) |
| `C-c m d` | open the run's decision view (§10) |

## 10. The decision view: `C-c m d`

### 10.1 What it is

`*tt-decisions: <project>/<run-id>*` is a read-only Org buffer in
`tt-decisions-mode`. It is rendered from the control log into
`views/decisions.org`, so it outlives Emacs. After a restart, `C-c m d`
resolves the run as described in §1.4 (finished runs included) and renders it
again. You act through keys, never by editing text, and every key sends a
version-bound command (§7).

It is ordered by what needs the owner:

```text
Needs you (1)            owner requests, blocking
Open findings (1)        blocking findings not yet closed, and who raised them
Corrections (1)          your revisions and where each one is in the lifecycle
Accepted with dissent    passed votes where a reviewer rejected
Accepted                 passed votes, unanimous
For sampling             detail decisions, unreferenced changes (§3.5)
```

Each entry leads with what a person needs in order to judge it. Ballots,
hashes, logs and evidence sit in a folded **Details** subtree, with links.

### 10.2 A pending decision

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

You: 1 / 2 choose · w write your own · r revise · RET evidence
     phase 2 waits on this

** Details
   - finding F-p2-02 (contract) · candidate 7c1e0a4 · contract v1#9e2c
   - ballots, reviews, check output, worker session → links
```

### 10.3 An automatically accepted trade-off

```org
* ACCEPTED Batch cancels per tick instead of one lock per request      :p2:
Phase 2 — make cancellation race-free · decided by review (2 of 3)

Why it matters: fewer lock acquisitions under load; cancels in the
same tick still apply in arrival order, and no guarantee changes.

Alternative: a lock per request — simpler, but twice the contention in
the phase benchmark.

Dissent: reviewer A worried a lone cancel can wait up to one tick
(≤1 ms). The master and reviewer B judged that within the latency budget.

You: nothing required · r revise · o override · RET evidence

** Details
   - decision D-p2-05 v2 · delegated · M approve, A reject, B approve
   - candidate 7c1e0a4 · contract v1#9e2c · links to ballots and diff hunks
```

If the owner agrees with reviewer A, `r` on this entry opens the revise buffer
in §7.5. The entry then moves to **Corrections** and shows its progress there.

### 10.4 Keys in the view

| Key | Action |
| --- | --- |
| `1`–`9`, `w` | resolve an owner request with an option, or write one |
| `r` | revise: correct this decision or finding and repair the phase (§7.5) |
| `o` | override a decision (approve or reject) without further explanation |
| `x` | accept a finding (§4.2), with a required scope note |
| `s` | on a sampled item: "should have been surfaced" (records a miss) |
| `u` | on an owner request: "did not need me" (records an unneeded escalation) |
| `RET` | open the evidence at point: diff hunk, file:line, check output, session |
| `TAB` | expand or collapse details |
| `g` | refresh |

## 11. Prototype first, then compare

Two different questions, answered in order:

1. **Does it work?** Can the difficult integration boundaries hold on a real
   task? This is §11.1.
2. **Is it better?** Does it save the owner's attention compared with the
   current workflow, at an acceptable cost? This is §11.4. It cannot be
   answered by running one task in two modes.

### 11.1 Milestone 1: a single-phase end-to-end prototype

One phase, end to end, on a real repository:

open the plan → `C-c m r` → run a worker → freeze its candidate → check it →
review it (M, A, B) → surface at least one decision → **resolve it, and in a
second run revise it** → integration probe → publish → `DONE`. It must also
recover from one deliberately failing integration probe.

It must include, deliberately triggered:

- a forced cancellation of a worker while an `sh` command runs;
- a conductor crash and restart at the boundaries in §9.3;
- an Emacs restart mid-run, followed by `C-c m s` and `C-c m d`.

The multi-phase interface, the master reviewer's cross-phase role and the pilot
are not built until this prototype holds.

### 11.2 Scope

In v1, after the prototype:

- One run at a time, phases serial, one worker per phase.
- Everything in §1–§10, including revise.

Out of scope:

- OrgBrain integration (context in, admission of decisions out). Keep the
  record shape admission-friendly so this is a later write, not a redesign.
- Jev / SemIf gates (§5.3).
- Two independent implementations per phase with a judge.
- Parallel phases, concurrent runs of one plan, and corrections or amendments
  once a later phase has been integrated.
- Permission sandboxing of workers (§9.5).

### 11.3 Assumptions the prototype must test

| # | Assumption | Test | What would falsify it |
| --- | --- | --- | --- |
| A1 | Pi RPC agents can be run, bounded and killed from a conductor | forced timeouts at every stage; the force-kill test in §9.3 | a process outlives cancellation; a hang passes a deadline; session state is lost across repairs |
| A2 | Extension hooks force structured submission, and the `sh` replacement is usable | attempts ending `no_submission`; the worker completes real tasks through `sh` | workers regularly fail to submit, loop on reminders, or cannot work without the built-in bash |
| A3 | The freeze boundary holds for cooperative workers, and tampering is detected | modify the live worktree after submit; separately, modify a candidate checkout during a check | a check or review sees a live-worktree change; a modified checkout is not flagged `integrity-violated` |
| A4 | Recovery behaves as specified | the `TT_CRASH_AT` suite | any criterion in §9.3 fails |
| A5 | The live trace and views work across restarts | reopen after Emacs restart mid-turn | live stream missing; history not rebuilt; views stale |
| A6 | Revise closes the loop | a correction on an accepted decision reaches `resolved` with no further owner action | the correction is lost, silently reinterpreted, or cannot act because of an exhausted budget |

### 11.4 The comparative pilot

Only after the prototype holds. Run **several similar tasks** (at least three
per arm, matched in size and kind) through the current skill-based `/delegate`
workflow and through tradeoffs-trace, and record for every task:

| Measure | How |
| --- | --- |
| active owner minutes | time the owner spends in the run's buffers or the old workflow's transcripts, from a simple Emacs activity timer, plus self-report |
| unnecessary escalations | owner requests marked `u` |
| missed consequential choices | after each task, an **independent audit** of the raw diff (the owner, helped by a separate audit agent that has seen none of the run's records) lists consequential choices; each one not surfaced during the run is a miss |
| escaped defects | defects found after acceptance, for a fixed period |
| elapsed time | plan start to done |
| model usage | tokens and cost per role, from Pi's usage events |

Sampling misses (§3.5) are reported separately, as the **observed miss rate in
the sample**. The audit is the only estimate of misses outside the sample.

The review overhead is real and must be measured, not assumed away. With the
two-turn review, each candidate round is one worker prompt plus six reviewer
prompts: for three phases, 21 role-level prompts with no repairs, and 84 if
every phase uses all four rounds, before tool continuations or retries. If the
pilot shows that this pays for itself only on consequential work, the
reviewer count gets tied to a phase risk tag rather than being the default for
small tasks.

### 11.5 Build order

The concrete phases, deliverables and required evidence are in
[the implementation plan](tradeoffs-trace-plan.md). In summary:

0. **Executable contracts.** Pure state machine, schemas and pinned Pi tool
   configuration, with tests showing the lifecycle has no circular
   dependency.
1. **Execution foundation.** A single-phase conductor: log, Pi adapter,
   conductor-owned shell, deadlines, frozen candidates, probe and publish,
   recovery.
2. **Review and correction loop.** Three reviewers, findings, voting, owner
   requests, resolve and revise.
3. **Minimal Emacs experience.** `C-c m r/s/d`, live trace, the decision view.
   Phases 1–3 together are the milestone in §11.1.
4. **Multi-phase execution.**
5. **Comparative pilot (§11.4).**

## 12. Open questions

- **The master reviewer on long runs.** Its session will compact. Should its
  prompt be rebuilt at each phase from the plan plus the control log, keeping
  the session for continuity but not as the source of truth?
- **A stubborn raising reviewer.** Only the raising reviewer can confirm a
  repair or withdraw a finding. If it never does, the budget sends the finding
  to the owner. Is that the right escape, or should a `reproduced` finding
  close when its reproduction passes on the new candidate?
- **Decision de-duplication.** Reviewers describe the same choice differently.
  v1 lets them attach to an existing record ID; whether that is reliable is
  measured in the pilot.
- **Default deadlines and budgets.** The §8.1 numbers are placeholders to be
  calibrated from the prototype.
- **`lsof +D` cost.** It is slow on large trees. If the sweep is too slow, an
  alternative is to track descendants by the conductor-assigned session ID.
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
  `--mode rpc`, `--extension`, `--tools`, `abort`, `steer`,
  `tool_call`, `agent_before_settle`.
- [Pi bash tool](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/src/core/tools/bash.ts):
  spawns commands `detached`, which is why §2.2 moves shell ownership to the
  conductor.
- [Pilish](https://github.com/dnouri/pilish): `pilish-open-session-file` resumes
  a live session, which is why §9.4 specifies a separate viewer.
