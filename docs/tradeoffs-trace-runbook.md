# tradeoffs-trace runbook

How to run, watch, steer and recover tradeoffs-trace runs. The design is in
[`tradeoffs-trace.md`](tradeoffs-trace.md), and the plan format is in
`~/orgw/PLAN_TEMPLATE.md`. This page covers operations only.

## What a run is

One run is one phase of one Org plan. A detached **conductor** (Node) drives
it through a fixed pipeline. Each attempt and each reviewer is a separate Pi
agent with its own session:

```text
implement ─▶ freeze ─▶ checks ─▶ probe ─▶ review (M, A, B) ─▶ resolve ─▶ publish ─▶ DONE
    ▲                    │          │             │                │
    └──── repair (up to 3 rounds, with the findings and rejected decisions) ◀┘
```

- **Worker:** implements in its own git worktree, `~/.tradeoffs-trace/<run>/worktree`. It never touches your checkout.
- **Freeze:** commits the worktree as the candidate.
- **Checks:** run the plan's check command on a fresh checkout of the candidate.
- **Probe:** merges the candidate onto the current integration branch and runs the checks again. It reuses the check results when the tree is identical.
- **Review:** three independent reviewers. In turn 1 each finds decisions in the diff. At a barrier they see each other's discoveries. In turn 2 they vote on every decision and raise findings. M holds a veto; otherwise 2 of 3 decide.
- **Publish:** fast-forwards the plan's integration branch in the **local** repository. Nothing is pushed.

## Prerequisites

| What | Where / how |
|---|---|
| Node 25+ | runs the TypeScript directly, no build step |
| Pi 0.87.0 | `pi --version`; the conductor asserts this version and each role's tool set |
| Model and credentials | Pi's own configuration, `~/.pi/agent/settings.json` (`defaultProvider`, `defaultModel`) and `auth.json`. Every role uses Pi's default model. |
| Emacs front end | `core/init-tradeoffs-trace.el`, loaded by the Emacs config |
| An installed runner | see below |

## Install or upgrade the runner

The conductor always runs from a **frozen copy** of an accepted revision, never
from the repository being edited. That lets a run change tradeoffs-trace itself
safely.

```sh
cd ~/.emacs.d/tradeoffs-trace
node src/cli.ts runner install <sha>     # copies <sha> to ~/.tradeoffs-trace/runner/<sha>, points runner/current at it
readlink ~/.tradeoffs-trace/runner/current
```

- Run it from a repository checkout, not from an installed runner.
- **Never upgrade while a run is in flight.** A run refuses to resume under a different runner.
- For the CLI, alias the installed copy:
  `alias tt=~/.tradeoffs-trace/runner/current/tradeoffs-trace/bin/tt`

## Start a run

1. Write or open an Org plan (`~/orgw/PLAN_TEMPLATE.md`). Set `#+TT_BRANCH` to the branch the result publishes to, and `#+TT_CHECKS` to a check command that finishes in a few minutes.
2. Make sure no checkout has `TT_BRANCH` checked out, or publishing will collide with it.
3. In the plan buffer, press `C-c m r`. Emacs validates the plan (errors open in `*tt-plan-errors*`), starts the conductor and opens the workspace tab.

From a shell: `tt start <plan.json>`. It takes the JSON plan that Emacs writes.

- **Reference documents.** Every `*.md` file the plan names that exists
  (relative to the plan, or under `~`), plus anything in `#+TT_REFS`, is copied
  into `<run>/refs/` when the run starts, and every agent's prompt lists those
  paths. Agents may `find`/`grep`/`ls` only inside their checkout and `refs/`,
  so no agent searches your home directory for a document.
- **Shallow clones are refused.** A run (or a program node) won't start on a
  shallow clone, because candidate checkouts can't be made from one. Run
  `git fetch --unshallow` first.

## Lint a plan before it runs

Every start lints the plan first — `C-c m r` (a single run, or every entry of a
program), `tt start` and `tt program start`. The rules live in one place,
`tradeoffs-trace/src/core/plan-lint.ts`; Emacs mirrors them by calling
`tt lint <plan.json>`, so there is only one implementation.

```sh
tt lint plan.json     # the findings; exit 1 if any is an error
tt lint program.json  # every entry's plan, each finding prefixed with the entry
```

Two kinds of finding:

- **Error — stops the start.** An acceptance item whose actor is the owner or a
  human (`the owner records …`, `manually …`, `someone …`). No worker or
  reviewer can satisfy it. Move it to the plan's `Owner checklist:` list, or
  rewrite it as an observable result a worker produces. Emacs shows errors in
  `*tt-plan-errors*` (jump-to-line to the Org file) and starts nothing.
- **Warning — shown, and the start continues.** An item that depends on a
  future that does not exist when the checks run and the reviewers judge the
  candidate (`after merge`, `the current rebased tip`, `once deployed`), or a
  comparison against a contracted limit with no stated tolerance
  (`p99 ≤ its contracted interval`). State a fact that is true when the run
  finishes, or the allowed margin (or the recorded gap a miss becomes).

The measured run is the reason (runtime doc §4): `13j`'s "the owner records a
live run" parked a phase, and `13i`'s "the recorded live-run SHA is the current
rebased tip" could never be true, because the rebased tip does not exist until
the conductor commits the candidate. The existing plans in
`~/orgw/work/atlas/indexps/` are a lint fixture in the package
(`tradeoffs-trace/test/fixtures/atlas-plans/`): the linter finds exactly one
error across them, `13j`'s owner-actor item.

## Owner checklist

A plan may carry the owner's own to-dos next to `Acceptance:`:

```org
  Acceptance:
  - the report exists
  Owner checklist:
  - record a live run with the five keys exported
```

`Owner checklist:` items are **not** given to the worker or the reviewers — they
are never acceptance criteria, so no reviewer blocks a phase for them. Once the
phase is `DONE`, the status buffer lists them, and `tt summary`'s PR body
carries them as `- [ ]` items to tick off. Use the list for anything that is the
owner's to do: a live run with keys, a ruling, a push. That is where the
linter's error message asks you to move an owner-actor item.

## Run several phases or plans: programs

A **program** runs several plans, and plans with several phases, as one
dependency graph. It's a wrapper around the loop above: every phase is still
one ordinary run with the same three reviewers and three repair rounds. A
detached scheduler starts each phase when everything it depends on is DONE,
and runs independent phases in parallel.

**Write a program file** next to the plans:

```org
#+TITLE: plan 13
#+TT_PROGRAM: 4                 max phases running at once
#+TT_BRANCHES: stack            default; "shared" publishes every phase to TT_BRANCH

* 13a
  :PROPERTIES:
  :PLAN:   13a_shared_markets.org
  :END:
* 13b
  :PROPERTIES:
  :PLAN:   13b_adapter_contract.org
  :AFTER:  13a
  :END:
* 13c
  :PROPERTIES:
  :PLAN:   13c_vendor_pyth.org
  :AFTER:  13b
  :END:
# … 13d–13f likewise after 13b …
* 13g
  :PROPERTIES:
  :PLAN:   13g_calculator_blend.org
  :AFTER:  13c 13d 13e 13f
  :END:
```

- Each heading is an **entry**: `:PLAN:` is a plan file (relative to the program file), and `:AFTER:` lists the entries it waits for (all of their phases).
- A plan file with several phases expands into one node per phase, run in order. Pressing `C-c m r` on such a plan (no program file) runs its phases in order.
- Start: `C-c m r` in the program buffer, or `tt program start <program.json>`. Every entry's plan is linted first (see above); one error blocks the whole program.

**Branches (stack mode, the default).** Every node publishes to its own branch,
`<TT_BRANCH>--<node>`:
- A node with no dependencies is cut from `TT_BRANCH`.
- A node with one dependency is cut from that dependency's branch, so a chain like 12a → 12b → 12c → 12d becomes **four stacked PRs**.
- A **join** (13g after 13c–13f) is cut from a merge commit of all its parents. If that merge conflicts, the node stops as blocked, with the files named.
- `TT_BRANCH` itself is never moved. Push the branches and open the PRs yourself: each node's PR base is shown in the program status.
- Parallel phases work in separate worktrees and publish to separate branches, so they never touch each other's files on disk.

**Watch and control:**

| Where | What |
|---|---|
| program buffer (`C-c m p`) | every node: `·` waiting, `▶` running, `⚑` needs you, `○` stopped, `✓` done, `✗` blocked; its run id, branch and PR base. Nodes waiting for you come first, with `waiting <duration>` and the reason. `RET` opens a node's run workspace (status, trace, decisions, input box), `k` stops the program, `R` resumes it. |
| CLI | `tt program status <id>`, `tt program state <id>` (JSON), `tt program list`, `tt program stop <id>`, `tt program resume <id>` |

**Review economy across rounds.** When the worker keeps a decision unchanged and it passed its vote last round, the reviewers' ballots carry over. The record is marked *carried*, and a reviewer votes again only if the new changes affect it; a fresh ballot replaces the carried one. The reviewers also see every test removed from a file that still exists, and must confirm each one was replaced or that its behaviour was removed on purpose.

**Rules the scheduler follows:**
- A node starts only when all its dependencies are **DONE**. A node that needs you, or whose run was stopped, keeps its slot and holds back its dependents until it finishes. Correct it or resume it as for any run.
- A **blocked** node never finishes. Its dependents wait, independent branches of the graph continue, and the program ends `stuck` when nothing else can run.
- The scheduler's state is folded from `~/.tradeoffs-trace/programs/<id>/events.jsonl`. `tt program resume` continues after a restart and never recreates an existing node branch.

**Time limits per plan.** A repository whose builds and suites take longer than
the defaults sets its own limits: `#+TT_SH_MINUTES` (one agent command),
`#+TT_CHECK_MINUTES` (checks and the probe) and `#+TT_ATTEMPT_MINUTES` (one worker
attempt). Put them in a plan file, or in the program file, where they apply
to every entry that doesn't set its own. For Rust, also share one cargo target
directory across worktrees and checkouts. Otherwise every fresh checkout
rebuilds from scratch:

```elisp
(setenv "CARGO_TARGET_DIR" (expand-file-name "~/.cache/dragon-target"))  ; before C-c m r
```

**Stop, resume and crashes.**
- `k` in the program buffer or `tt program stop <id>` stops the scheduler and every running phase cleanly. Published branches and each run's state stay on disk.
- `R` in the program buffer or `tt program resume <id>` undoes the stop, restarts every node run that isn't running (each recovers from its own control log), and relaunches the scheduler. It never recreates an existing node branch.
- **Crashes recover on their own.** When a node's conductor dies without a clean stop (Ctrl-C, crash, sleep), the scheduler restarts it, up to 3 times per node (`NODE_RESUMED` in the program log). If the scheduler itself died, for example after a reboot, `tt program resume <id>` brings everything back.
- One phase alone: `k` / `R` in the runs list, or `tt stop <run>` / `tt resume <run>`.

**Before an unattended program:**
1. `TT_BRANCH` exists in the repository, and nothing has it or a node branch checked out.
2. Every plan's check command finishes in a few minutes (the checks limit is 5).
3. The runner is installed at the revision you want (`readlink ~/.tradeoffs-trace/runner/current`).
4. The plan declares every credential it needs with `#+TT_SECRETS` (see below), and each one is exported in the shell that starts Emacs or `tt`.

## Secrets (credentials)

A plan declares the credentials it needs **by name** and nothing else:

```org
#+TT_SECRETS: PYTH_ACCESS_TOKEN KAIKO_KEY
```

- The name goes into the JSON plan (`secrets`); the plan never holds a value.
- The conductor resolves each value from **its own environment** (the shell that
  starts Emacs or `tt`) when the run starts, and passes it to the worker and
  every reviewer as an environment variable. In a command, write `$PYTH_ACCESS_TOKEN`.
- Every agent's prompt names the declared secrets and says to reference them as
  `$NAME`, never to paste a value.
- An `sh` command containing a secret's literal value is refused (before it
  runs), with a message naming the variable to use instead.
- Redaction happens at choke points, not at each caller:
  - the run's **plan snapshot** (`<run>/plan/v1.json`) is written with every
    declared value replaced, so a plan whose goal or check line quotes one
    cannot put it on disk;
  - **everything sent to an agent** — the worker prompt, every reviewer
    prompt, a steer, the stall nudge — is redacted before it is written to the
    agent's stdin, so no prompt can carry a value;
  - agent text entering the conductor's **in-memory state**, the agent stream
    files, `events.jsonl`, the check logs, the `refs/` copies, `views/pr.md`,
    and what `tt status`/`tt state`/`tt timing` print are all redacted too, with
    one line-safe rule: a value is replaced inside JSON strings (keys included)
    and never inside a number, so every JSONL line stays parseable.
  A check the plan wrote still runs exactly as written, even when the conductor
  was started from the masked snapshot: the value is put back, in memory, for
  the commands the plan asks for and only for those (the check log keeps the
  mask). Write `$NAME` anyway — it is clearer and survives a run started
  without the value. Emacs masks a declared secret's value as well, using its
  own environment, so nothing it displays can show one.
- A `refs/` copy is written masked: a document saved as UTF-16 (its bytes hold
  NULs) is searched in UTF-8/UTF-16 and masked too, and — when the plan declares
  secrets — a document that is neither text nor a UTF-16 document is **not copied
  at all**: it is named in `refs/MISSING.txt`, because a leak that cannot be
  searched must not be handed to every agent. A UTF-16 document that quotes
  nothing *is* copied: its bytes were searched. A plan that declares no secrets
  copies its references exactly as before.
- **A declared secret that could not be resolved withholds the references.** Any
  document may quote any declared value, so with one name unset (or set too short
  to mask) no copy can be verified: each document is left out and named in
  `refs/MISSING.txt` with the reason, and the status says which name was unset.
  Export every key the plan declares and the documents come back, masked.
- A declared secret whose value is **shorter than 4 characters** is not used
  for masking or for refusing commands (masking `1` would rewrite every id and
  timestamp in the log); it is reported by name and the run still runs. Give a
  secret a real value, or it is only an environment variable.
- A declared secret that is unset at start is reported by name (`secret FAKE_KEY
  not set` in `tt status` and the status buffer) and **the run still starts** — a
  missing credential fails whatever check needs it, not the run. Its reference
  documents are withheld as above, since a value nobody can supply cannot be
  masked.
- Text that merely *quotes* a value (a finding's evidence, a ballot's rationale)
  is masked, not refused, so the evidence stays readable; a **command** carrying
  a value (the `sh` tool, or a finding's reproduction command) is refused with a
  message naming `$NAME` and never runs.
- The input box is the owner's own channel: what you type is delivered to the
  worker as written. Everything the conductor *records* about it is masked.
- Two files a run can still hold a value in are not the conductor's to write at
  the moment it is written: Pi's own session file under `<run>/sessions/`, and a
  file the worker itself edited in `<run>/worktree/`. `tt redact` cleans the
  sessions; the worktree and the reviewers' `candidates/` checkouts are git
  trees and are left to you.

Cleaning a run that already leaked a value (for example a run started before its
plan declared its secrets):

```sh
PYTH_ACCESS_TOKEN=…  tt redact <run-dir-or-id> --secrets PYTH_ACCESS_TOKEN
PYTH_ACCESS_TOKEN=…  tt redact --all --secrets PYTH_ACCESS_TOKEN
```

`tt redact` rewrites run directories **in place** — `events.jsonl`,
`stream/*.jsonl`, `sessions/` (Pi's own session files, which hold whatever an
agent echoed), `checks/**`, `refs/**`, `views/**`, `plan/`, `inbox/`,
`conductor.log` — replacing the value with `***NAME***`. The values are read
from the command's own environment; the names come from `--secrets` or, when it
is omitted, from each run's own plan snapshot. JSONL files are rewritten line by
line, so every line still parses (a torn final line stays torn and unnewlined).
A UTF-16 document is searched in UTF-8 and UTF-16 and masked, in both the raw
and the JSON-escaped form of a value; a file whose bytes could not be searched
(neither text nor a UTF-16 document) is **named** in the output rather than
counted as clean.

It **refuses a run whose conductor is still alive** (that run keeps writing its
stream, its sessions and its log, so a run reported as redacted could regain the
value a second later) — stop the run first, or pass `--force` if you accept
that. `--root` chooses the run root.

A worker's `worktree/` and the reviewers' `candidates/` checkouts are git trees,
not conductor output, and are left alone — check them yourself if a worker ever
pasted a value into the code. The trace buffer masks a declared secret's value
that a stream file still holds, using Emacs's own environment, so an unredacted
past run cannot display one either.

## Watch it

| Where | What you see |
|---|---|
| status buffer | pipeline with stage times and time left; `time`: where the active agent's time goes (model, polling, full tests) and its running tool; gates for the current candidate; each reviewer's **outcome** (`M ✗ 2 reject · 1 blocking`); `verdict`: why the phase did or did not accept, and what happens next |
| trace buffer | one line per tool call (time, command, ✓/✗ exit, duration, last output line), plus `path +a −r` for each file the call changed; the running call in the header. `a` pins another agent. |
| decision view (`C-c m d`) | the current round's decisions, each labelled by the tally, with the options, recommendation and each reviewer's ballot; findings grouped by file; earlier rounds one line each. Read-only. |
| runs list (`C-c m l`) | every run: `RET` opens, `k` stops, `R` resumes |
| mode line | live runs with stage, time and reviews; the oldest owner wait (`⚑ 13f waiting 1h12m`) with a warning-face flash when a new notification arrives |
| CLI | `tt list`, `tt status <run>`, `tt state <run>` (JSON), `tt timing <run>` (per-agent time breakdown), `tt redact` (see Secrets) |

## Notifications when something needs you

Nothing waits for the owner by default, so the system tells you when a run has
stopped making progress on its own:

- A run entering **AWAITING_OWNER** ("needs you") or **BLOCKED**, and a program
  ending **done** or **stuck**, append one record to
  `~/.tradeoffs-trace/notifications.jsonl` — the run or program id, its title,
  the program node (for a node run), a one-line reason, and the time.
- The same moment, the notifier runs: on macOS an `osascript` banner, and
  nothing on other platforms. A run parked in AWAITING_OWNER keeps its
  conductor alive and notifies once more 30 minutes later if the owner has
  still not acted; a program that ends **stuck** keeps its scheduler watching
  and does the same, then exits. A program that ends **done** is announced
  once — it is not waiting on anyone. One wait is never announced twice before
  that reminder, and each new park (or new stuck program) is a new wait. A
  notification that fails, or cannot be written, is logged (to the run's log
  or `scheduler.log`) and ignored; it never stops a run or the scheduler.
- Emacs watches the file (`core/init-tradeoffs-trace.el`): each new line is
  shown in the echo area, the mode-line indicator flashes a warning face, and
  it shows how long the **oldest** wait has lasted (`⚑ 13f waiting 1h12m`).
- `tt program status` and the program buffer list waiting nodes first, each with
  `waiting <duration>` and its reason, so one glance says who needs you.

Override the notifier with `TT_NOTIFY_COMMAND` — the shell command the
conductor and the scheduler run instead of the default. Tests point it at a
script that appends to a file; on a headless host it can be anything that
reaches you (a webhook, a mail command, or `:` to disable it). It is a plain
`sh -c` command with no arguments; read `notifications.jsonl` for the detail.

```sh
TT_NOTIFY_COMMAND='curl -s -d "tradeoffs-trace needs you" https://ntfy.sh/my-topic' tt program start program.json
```

## Steer it

The input box (bottom window) is the **only** way to intervene. Its header line
says what sending does right now.

| Phase | Sending your text |
|---|---|
| IMPLEMENTING / FREEZING | **steer**: delivered to the running worker immediately (at most once) |
| CHECKING / PROBING / REVIEWING | **note**: delivered at the start of the next worker attempt |
| AWAITING_OWNER ("needs you") | **correction**: resolves the open requests, grants 3 repair rounds, repairs with your text verbatim |
| DONE / BLOCKED | refused, with the reason |

The status buffer's **Owner input** section shows each text's recorded effect:
delivered, noted, correction started, refused, delivery uncertain, or not
picked up after 30 s.

`RET` sends in Evil normal state; `C-c C-c` sends from any state.

## Stop and resume

```sh
tt stop <run>      # clean stop within 15 s: kills agents and their commands, logs the stop, releases the lock
tt resume <run>    # restarts the conductor; recovers from the control log
```

In Emacs, use `k` and `R` in the runs list. The run directory and worktree stay
on disk after a stop.

## Time limits (defaults)

| Limit | Default | When it fires |
|---|---|---|
| worker attempt | 45 min | the attempt times out and uses a repair round |
| one agent `sh` command | 3 min | the command's process group is killed; the agent is told not to rerun it as is |
| stall watchdog | 3 min + 3 min | a mid-turn agent silent with no command running is nudged once, then its attempt or review ends |
| freeze | 2 min | |
| checks | 5 min | counts as failed checks |
| probe | 10 min | |
| review (both turns) | 15 min | the reviewer is re-dispatched once, then the phase is BLOCKED |
| repair rounds | 3 | then AWAITING_OWNER |

**The owner is never waited for by default.** Reserved decisions (the plan's
`RESERVED` list, and choices that change an interface, a persistence format or
a dependency) are voted on by M, A and B like any other and marked
`⚑ FLAGGED` in the decision view; the status counts them ("N flagged for
you"). Read them if you care and override one through the input box. A run
stops for you only when its repair rounds are exhausted.

## Outcomes

- **DONE:** the result is on the local `TT_BRANCH`. `tt summary <run>` writes the PR body (`<run>/views/pr.md`): the review outcome, blocking findings fixed during review, flagged decisions, **every open advisory finding** (accepted, not fixed) and tests removed from surviving files. Push and open the PR yourself. For a program, `tt program prs <id>` writes each DONE node's body and prints the `git push` and `gh pr create` commands with the stacked bases.
- **AWAITING_OWNER ("needs you"):** repair rounds are exhausted. Read the verdict, then type a correction (it grants 3 more rounds), or `tt stop`.
- **BLOCKED:** the run cannot continue, for example a reviewer is unavailable twice. The reason is in the status buffer. Fix the cause and start a new run.

## Where things are

```text
~/.tradeoffs-trace/
  runner/<sha>/, runner/current     frozen runners
  <run>/events.jsonl                control log: the only source of truth (tt state folds it)
  <run>/worktree/                   the worker's worktree
  <run>/stream/<agent>.jsonl        each agent's raw Pi event stream (the trace renders it)
  <run>/sessions/                   Pi sessions (repairs continue the worker's session)
  <run>/checks/<sha>/               per-command check logs
  <run>/inbox/{,applied/,rejected/} owner input and commands, with rejection reasons
  notifications.jsonl               one line per owner wait or finished program (see Notifications)
```

## Troubleshooting

Each row was observed in a real run.

| Symptom | Cause | Action |
|---|---|---|
| a command in the `time` row approaches 3 min, again and again | a test hangs (often waiting for an event that never comes) and gets killed | steer the worker with the cause, or tell it to run one test with `--test-name-pattern` |
| an agent is silent mid-turn | model call hung | nothing: the stall watchdog nudges after 3 min and ends the attempt after 6 |
| checks fail on a test that passes alone | load-sensitive test under the parallel suite | rerun; if it repeats, mark the test for the plan's worker to fix |
| freeze fails with "failed to copy file … objects" | a concurrent `git gc` in the source repository | nothing: the clone retries once |
| review shows many decisions for a small change | every reviewer discovers up to 5 more | expected; read only REJECTED and flagged ones |
| `tt stop` reports the lock still held | conductor killed hard | wait a moment and rerun `tt stop`; `tt list` shows the real state |
| a key was pasted into a command or a file by an agent | the plan did not declare it (`#+TT_SECRETS`) | clean the run with `tt redact` (see Secrets), add the name to the plan, and export the value in the shell that starts the run |

## Known limitations

- **One phase per run.** A multi-phase plan or several plans run as a program (see above), where each phase is still its own run.
- **Publishing is local.** Pushing and PRs stay manual.
- **Stopping during reviewer dispatch** can log `ERR_STREAM_WRITE_AFTER_END` from a late prompt write; the run is still stopped.
- **The write guard checks paths, not git commands.** A worker's `git worktree add /tmp/...` is not refused. Clean up with `git worktree prune`.
