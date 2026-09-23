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
- Start: `C-c m r` in the program buffer, or `tt program start <program.json>`.

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
| program buffer (`C-c m p`) | every node: `·` waiting, `▶` running, `⚑` needs you, `○` stopped, `✓` done, `✗` blocked; its run id, branch and PR base. `RET` opens a node's run workspace (status, trace, decisions, input box), `k` stops the program, `R` resumes it. |
| CLI | `tt program status <id>`, `tt program state <id>` (JSON), `tt program list`, `tt program stop <id>`, `tt program resume <id>` |

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

**Before an unattended program:**
1. `TT_BRANCH` exists in the repository, and nothing has it or a node branch checked out.
2. Every plan's check command finishes in a few minutes (the checks limit is 5).
3. The runner is installed at the revision you want (`readlink ~/.tradeoffs-trace/runner/current`).
4. Credentials the checks need (for example vendor keys) are exported in the shell that starts Emacs or `tt`.

## Watch it

| Where | What you see |
|---|---|
| status buffer | pipeline with stage times and time left; `time`: where the active agent's time goes (model, polling, full tests) and its running tool; gates for the current candidate; each reviewer's **outcome** (`M ✗ 2 reject · 1 blocking`); `verdict`: why the phase did or did not accept, and what happens next |
| trace buffer | one line per tool call (time, command, ✓/✗ exit, duration, last output line), plus `path +a −r` for each file the call changed; the running call in the header. `a` pins another agent. |
| decision view (`C-c m d`) | the current round's decisions, each labelled by the tally, with the options, recommendation and each reviewer's ballot; findings grouped by file; earlier rounds one line each. Read-only. |
| runs list (`C-c m l`) | every run: `RET` opens, `k` stops, `R` resumes |
| mode line | live runs with stage, time and reviews; a warning face when something needs attention |
| CLI | `tt list`, `tt status <run>`, `tt state <run>` (JSON), `tt timing <run>` (per-agent time breakdown) |

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

- **DONE:** the result is on the local `TT_BRANCH`. Review it, then push and open or update the PR yourself.
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

## Known limitations

- **One phase per run.** A multi-phase plan or several plans run as a program (see above), where each phase is still its own run.
- **Publishing is local.** Pushing and PRs stay manual.
- **Stopping during reviewer dispatch** can log `ERR_STREAM_WRITE_AFTER_END` from a late prompt write; the run is still stopped.
- **The write guard checks paths, not git commands.** A worker's `git worktree add /tmp/...` is not refused. Clean up with `git worktree prune`.
