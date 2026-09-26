# tradeoffs-trace runbook

How to run, watch, steer and recover tradeoffs-trace runs. The design is in
[`tradeoffs-trace.md`](tradeoffs-trace.md), and the plan format is in
`~/orgw/PLAN_TEMPLATE.md`. This page covers operations only.

## What a run is

One run is one phase of one Org plan. A detached **conductor** (Node) drives
it through a fixed pipeline. Each attempt and each reviewer is a separate Pi
agent with its own session:

```text
implement ─▶ freeze ─▶ checks ─▶ probe ─▶ review (M, A, B) ─▶ resolve ─▶ gate ─▶ publish ─▶ DONE
    ▲                    │          │             │                │       │
    └──── repair (up to 3 rounds, with the findings and rejected decisions) ◀┘
```

`gate` is the phase's own expensive, live proof, run by the conductor itself
and only for a phase that declares `:GATE:` (see *The gate* below). A phase
without one goes straight from `resolve` to `publish`.

- **Worker:** implements in its own git worktree, `~/.tradeoffs-trace/<run>/worktree`. It never touches your checkout.
- **Freeze:** commits the worktree as the candidate.
- **Checks:** run the plan's check command on a fresh checkout of the candidate. Failures the base already had do not count against it (see *Pre-existing check failures* below).
- **Probe:** merges the candidate onto the current integration branch and runs the checks again. It reuses the check results when the tree is identical.
- **Review:** three independent reviewers. In turn 1 each finds decisions in the diff. At a barrier they see each other's discoveries. In turn 2 they vote on every decision and raise findings. M holds a veto; otherwise 2 of 3 decide.
- **Gate:** runs the phase's `:GATE:` command itself, once per candidate the reviewers accepted, under a machine-wide lock, and records the evidence (see *The gate* below).
- **Publish:** fast-forwards the plan's integration branch in the **local** repository. Nothing is pushed.

## The gate (`:GATE:`)

Some phases need a live proof that is far more expensive than the check loop —
for example a full `deploy/atlas.sh … --clean --build` of a 40-service stack
(~15 min). Declare it on the phase:

```org
* 13i: the live atlas gate
  :PROPERTIES:
  :ID:          13i
  :CHECKS:      make check
  :GATE:        deploy/atlas.sh --clean --build
  :GATE_CLEANUP: docker compose -f deploy/atlas.yml down -v
  :END:
```

- The **conductor runs it**, never an agent, after the checks, the probe and
  all three reviews have passed and before acceptance. It is the only accepted
  live proof; the worker and the reviewers are told so and never to run the
  command or report its result themselves. A substitute (a sentinel sha, a
  "pending owner live run", a fingerprint) is a blocking finding, not evidence.
- It runs in a fresh checkout of the candidate merged onto the current
  integration head, with the plan's secrets (`#+TT_SECRETS`) in its
  environment. It takes `~/.tradeoffs-trace/gate.lock` before that merge and
  holds it through the command, the cleanup and the record, so two phases —
  in one program or in two runs — never have gate checkouts or builds live at
  once; the second waits.
- **The evidence** is `<run>/checks/<sha>/gate.json` (candidate and base SHA,
  the merged tree, the command, exit status, the gate's own duration, start,
  the log's sha256, and the cleanup's own exit and duration) plus
  `<run>/checks/<sha>/gate.log` (redacted stdout/stderr). `tt status` and `tt
  summary` cite it. The cleanup runs whenever the gate command ran (pass, fail
  or timeout), under its own limit; its time is not counted as the gate's. If
  the candidate no longer merges, the gate never starts and nothing is
  cleaned — the record says `not started` rather than claiming a run.
- **A failure** (non-zero exit, or killed at its limit) is a blocking
  `integration` finding whose evidence is the log's last 60 lines. The phase
  repairs (the worker is shown the log) or parks on you when the rounds run
  out.
- **An identical tree with the same command reuses a passing record**: the
  command does not run again (a repair attempt that changes nothing freezes a
  new commit with the same tree). The run-or-reuse decision happens under the
  gate lock, so a candidate whose tree another candidate just gated reuses
  that pass. The record is re-hashed first: if its `gate.log` is missing or no
  longer matches the sha256 it carries — or the plan's `:GATE:` text has
  changed — the gate reruns instead of passing on it. A record that is itself
  a reuse counts as passing, so a chain of reuses still saves the build.
  A failed gate is always rerun, and a record is never overwritten: a
  candidate re-gated after a stale publish accepts on its own record in place,
  and the status/`tt summary` line names both the head the evidence came from
  and the head being accepted.
- **The limit** is 30 min by default; `#+TT_GATE_MINUTES: 45` raises it. A gate
  over the limit has its process group killed and counts as failed.
- A phase without `:GATE:` is unaffected: it accepts as soon as the reviews
  pass, exactly as before.

## Pre-existing check failures (the base baseline)

A phase's base can already fail the phase's own checks before the worker touches
anything — a red `cargo test --workspace` with 14 deterministic
`exchange-state-machine` failures, say. Left alone, every attempt pays for the
same red output and an acceptance item like "`cargo test --workspace` passes"
can never be met by any worker.

So **before the run's first attempt** the conductor runs the phase's checks once
on the base (`integrationHead`), in its own disposable checkout, and records a
`baseline` in `events.jsonl` and `<run>/checks/base/baseline.json`: per command
its exit status, duration, and the failing test names it can parse. Names are
parsed from the three runners plans actually use — cargo
(`test <name> ... FAILED`), node:test (`not ok N - <name>` under
`--test-reporter=tap`, and the default spec reporter's `✖ <name>`) and ERT
(`FAILED <name>`). Output that names no test parses to nothing, and **only a
command that ran to completion and exited non-zero may contribute names**: a
timeout's output is truncated, a signal death (the OOM killer's `SIGKILL`, a
`SIGSEGV`) never printed its last failure, and a command that exited 0 did not
fail at all — none of those three can put a name into the set that excuses a
candidate's check.

- **D2 default — no new failures:** a candidate's failing check still counts as
  passing when every failing test it names also failed on the base *for that
  same command*, and at least one name was parsed. The status says
  `checks ✓ (base has 1 failures)`. Any test the base did not fail fails the
  gate, and the new names are recorded with the check's completion and in a
  `check_failure_new` record.
- **Strict fallback:** a check whose failing output yields no parsable test name
  (a compile error, a timeout, a signal death, an integrity violation) always
  fails. A baseline can therefore only ever *reduce* the failures blamed on a
  candidate — a new failure is never hidden.
- **The integration probe still judges strictly** (it is the last gate before
  publish). In the normal fast-forward case it reuses the candidate's passed
  checks instead of running them again, so a pre-existing failure does not
  block acceptance; if the integration moved and the probe runs, a failing
  check there fails the probe.
- **The worker and the reviewers are told.** The worker's prompt and each
  reviewer's turn-2 prompt list the base's failing tests as "Pre-existing check
  failures on the phase base (NOT this phase's to fix)", each under the command
  it failed in, so neither tries to fix them nor raises them as a defect — and
  a name from one command is never mistaken for a licence to fail it in
  another.
- **It is paid for once per base tree.** The record carries the base's full
  tree id, and the key is that tree plus the effective check list; a program
  node with the same base and the same checks reuses a sibling node's record
  (under `programs/<id>/baselines/<key>/`) instead of running the checks again,
  and a run restarted after a crash reuses its own. Two nodes that start in the
  same parallel wave cannot both pay: the first to take a lock under that
  directory runs it, the others wait for the record it publishes (a holder that
  died is detected by its pid and age, so a stale lock never wedges a run). The
  base itself is never edited, and the gate still runs on the candidate. A
  record that no longer covers the phase's current check list — after a
  contract amendment changes one — is not trusted by the gate, the prompts or
  the status.

When the base fails, the status shows `base fails: N tests: <names>` — or
`base fails: 0 tests (no test names parsed; checks stay strict)` when its
failing output names none.

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
| program buffer (`C-c m p`) | every node: `·` waiting, `▶` running, `⚑` needs you, `○` stopped, `✓` done, `✗` blocked; its run id, branch and PR base. Nodes waiting for you come first, with `waiting <duration>` and the reason. `RET` opens a node's run workspace (status, trace, decisions, input box), `i` opens the program's input box (a program-wide owner directive), `k` stops the program, `R` resumes it. It also lists the program's owner directives in force. |
| CLI | `tt program status <id>`, `tt program state <id>` (JSON), `tt program list`, `tt program directive <id> <text>`, `tt program withdraw <id> <ODP-n>`, `tt program stop <id>`, `tt program resume <id>` |

**Review economy across rounds.** When the worker keeps a decision unchanged and it passed its vote last round, the reviewers' ballots carry over. The record is marked *carried*, and a reviewer votes again only if the new changes affect it; a fresh ballot replaces the carried one. The reviewers also see every test removed from a file that still exists, and must confirm each one was replaced or that its behaviour was removed on purpose.

**Rules the scheduler follows:**
- A node starts only when all its dependencies are **DONE**. A node that needs you, or whose run was stopped, keeps its slot and holds back its dependents until it finishes. Correct it or resume it as for any run.
- A **blocked** node never finishes. Its dependents wait, independent branches of the graph continue, and the program ends `stuck` when nothing else can run.
- The scheduler's state is folded from `~/.tradeoffs-trace/programs/<id>/events.jsonl` — including the program's owner directives, so a program-wide ruling survives a restart. `tt program resume` continues after a restart and never recreates an existing node branch.
- A program-wide directive (`i` in the program buffer, `C-u C-c C-c` in a run's input box, or `tt program directive`) is steered to every running node at once and seeded into the plan of every node started later, so it binds the whole program (see "Steer it").

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
| status buffer | pipeline with stage times and time left; `time`: where the active agent's time goes (model, polling, full tests) and its running tool; gates for the current candidate; `amended`: every passed or reverted criterion amendment with old → new; each reviewer's **outcome** (`M ✗ 2 reject · 1 blocking`); `verdict`: why the phase did or did not accept, and what happens next; directly under the verdict, the **Trade-offs** panel and the **cost** row |
| Trade-offs panel (in the status buffer) | the few trade-offs that matter while the run is live, most important first, at most 6 lines: an owner directive some agent has not received yet; a disputed or amended criterion (old → new); a flagged (reserved) decision and its tally; a decision M vetoed this round, with M's one-line reason; a decision that passed with dissent; and **one** line counting the advisories (`3 advisories (2 new) — C-c m d`) instead of listing them. Each line is self-contained, and `RET` on it opens the decision view at that record. |
| cost row | what the run has cost so far: rounds, total minutes and per-stage minutes, owner-wait minutes, and a plain estimate for one more round from this phase's own completed rounds (`4 rounds · 106m total · implement 34m · checks 8m · review 30m · owner wait 34m · next round ≈ 14 min`). Per node, `tt program status` and the program buffer show the same in one line: rounds, minutes, owner wait and the node's top trade-off. |
| trace buffer | one line per tool call (time, command, ✓/✗ exit, duration, last output line), plus `path +a −r` for each file the call changed; the running call in the header. `a` pins another agent. |
| decision view (`C-c m d`) | the current round's decisions in the same order as the Trade-offs panel (amendments, flagged, M vetoes, dissent, the rest), each labelled by the tally, with the options, recommendation and each reviewer's ballot; a passed amendment reads `⚑ AMENDED` and shows the old → new wording; the open advisories folded under one `Advisories (N)` heading; other findings grouped by file; earlier rounds one line each. Read-only. |
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
says what sending does right now, **including the scope** (this phase, or the
whole program).

**Every text you send is an owner directive** — a numbered record (`OD-1`,
`OD-2`, …) that is part of its phase until you withdraw it:

1. **delivered at once** to every live agent of the run — the worker and any
   reviewer mid-turn — as a Pi steer, with each delivery recorded;
2. **quoted verbatim, newest last, under "Owner directives (binding)" in every
   later prompt**: every worker attempt and repair, both reviewer turns, and any
   re-dispatched or fresh agent;
3. **binding on reviewers as part of the contract**: a candidate that violates a
   directive is a blocking contract finding that cites the directive id, and a
   candidate that follows one cannot be faulted for doing so, even where the
   plan's text says otherwise.

| Phase | Sending your text |
|---|---|
| IMPLEMENTING / FREEZING | **steer**: delivered to the running worker immediately (at most once), and recorded as an owner directive |
| CHECKING / PROBING / REVIEWING | steers every live agent now (the reviewers, mid-turn) as an owner directive; the note is also delivered at the start of the next worker attempt |
| AWAITING_OWNER ("needs you") | **correction**: resolves the open requests, grants 3 repair rounds, repairs with your text verbatim — and is an owner directive in every later prompt |
| DONE / BLOCKED | refused, with the reason |

**Taking back an amendment.** A dispute (§ below) that passed becomes an
amendment. To restore the criterion's original wording, type the **correction**
`revert AM-p1-7c1e0a4a` into the input box (the input box sends that command
as a correction). It
works from any running phase (it does not wait for AWAITING_OWNER and grants no
repair rounds): it restores the wording, invalidates the evidence bound to the
replaced version and returns the phase to checks under the restored contract.
The status shows the input as `reverted an amendment`, and a later `revert` of
the same amendment is refused. Text that merely mentions an amendment id (a
steer or a note) stays advisory, reaches its agents and never rewrites the
contract; only the `revert <id>` command reverts.

**Scope.** A directive applies to its own phase by default, and is numbered
`OD-1`, `OD-2`, …. It applies to the **whole program** — every running node is
steered now, and every node started later is started with it in its prompts —
when it is sent with `C-u C-c C-c` from a run's input box, or from a program
buffer's input box (`i` in the program buffer). A program-wide ruling is
numbered `ODP-1`, `ODP-2`, … : the `ODP` namespace is the program's own, so a
node never renumbers a program ruling and one id always names one ruling. The
header line states the scope before you send. A run that is **not part of a
program** has nothing program-wide to reach: its header says so, and a `C-u`
there stays this phase's own directive.

**Withdraw one.** Type `withdraw OD-n` (or `withdraw ODP-n` for a program-wide
one), for example `withdraw OD-1`, into any input box — a run's or the
program's. Every live agent is steered that it no longer applies, and every
later prompt omits it. Withdrawing a program-wide ruling from *any* node
retracts it **everywhere**: the program records the withdrawal, every running
node is told, and every node started later is no longer given it. Trailing
prose is fine (`withdraw OD-1 because it is stale` retracts OD-1). A withdrawal
that names no id, an id that does not exist, or one already withdrawn is
refused with the reason, and nothing changes — it is never turned into a new
ruling. (The program buffer's box goes through `tt program withdraw`, which
refuses an unknown id or a phase id (`OD-n`) the same way.)

The status buffer's **Owner input** section shows each text's recorded effect:
delivered, noted, correction started, refused, delivery uncertain, or not
picked up after 30 s. Under **Owner directives** it shows each directive with
its scope, whether it is in force or withdrawn, and its delivery state per
agent (`worker ✓ M ✓ A ⧗ B ✓` — `⧗` is not acknowledged yet, `?` could not be
sent). `tt summary <run>` lists the directives in force in the PR body, and
`tt program status <id>` lists the program's own.

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
| checks | 5 min | counts as failed checks (each baseline command too) |
| probe | 10 min | |
| the phase gate (`:GATE:`) | 30 min (`#+TT_GATE_MINUTES`) | the command's group is killed and the gate counts as failed |
| review (both turns) | 15 min | the reviewer is re-dispatched once, then the phase is BLOCKED |
| repair rounds | 3 | then AWAITING_OWNER |

**The owner is never waited for by default.** Reserved decisions (the plan's
`RESERVED` list, and choices that change an interface, a persistence format or
a dependency) are voted on by M, A and B like any other and marked
`⚑ FLAGGED` in the decision view; the status counts them ("N flagged for
you"). Read them if you care and override one through the input box. A run
stops for you only when its repair rounds are exhausted.

**Unmeetable criteria do not stop the run either.** A worker (or a reviewer)
that finds a criterion cannot be met **as written** — not merely unmet —
records a *criterion dispute* (`criterionDispute: {criterion, why,
proposedWording}` in `submit_phase`, or the same field on a turn-2 finding).
The conductor turns it into an **amendment** record, voted on like any other
reserved decision. If the normal tally passes (M, plus one of A/B) the
criterion's wording is replaced **for this phase only, from the next candidate
on**: the contract version bumps, contract findings citing the old wording are
closed as **superseded**, and the status, the decision view and `tt summary`
show `⚑ AMENDED` with the old and the new text. The amendment starts a fresh
attempt, not a repair round, and a failed one leaves the wording unchanged and
blocks nothing. A criterion that is merely unmet is still an ordinary blocking
finding and a normal repair. You can undo an amendment at any time with a
correction naming its id (`revert AM-p1-7c1e0a4a`); a plain note mentioning
the id does not revert anything.

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
  <run>/checks/base/                the base baseline: baseline.json + per-command logs
  <run>/checks/<sha>/               per-command check logs; when the phase
                                    declares :GATE:, gate.json + gate.log too
  ~/.tradeoffs-trace/gate.lock      the machine-wide gate lock (two phases
                                    never run their gate command at once)
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
