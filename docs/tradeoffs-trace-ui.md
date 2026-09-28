# tradeoffs-trace in Emacs: everything is a buffer (plan 03)

Status: **Owner's direction, recorded 2026-09-27.** Not implemented. Plan 03 is
the UI. It renders the message model that plan 04 defines
([the programmable review pipeline](tradeoffs-trace-pipeline.md)) and improves
what the front end already shows today.

## 0. Flag: Emacs becomes a front end only

> **If things go well, the Pi-agent pipeline moves out of `~/.emacs.d`, and
> Emacs becomes just a front end.**

Every UI decision below should hold after that move. Emacs reads the runs'
files on disk and sends owner input. It owns no runtime state and runs no
agents. It works the same on the server itself or on a laptop reading the
server's disk over TRAMP (`+tt-root` set to `/ssh:host:~/.tradeoffs-trace/`,
supported since #39). A run keeps going when every Emacs is closed; opening it
again just renders where it is.

## 1. The theory: everything is a buffer, driven by Evil

The same rule as the rest of this Emacs: **every piece of work is a buffer**.
A program, a phase, a review, a message, an agent's trace: each one is
navigated and controlled with Evil in normal state, and none depends on
window chrome. Emacs runs inside tmux, in a terminal, so nothing may rely on
the tab bar or a mouse.

## 2. The review buffer: trade-offs, findings, blockers

One buffer per phase lists all three message types, each in its own colour:

| Type | Colour (suggested) | Raised by |
|---|---|---|
| Trade-off | neutral or blue | mainly the worker |
| Finding | yellow | reviewers |
| Blocker | red | reviewers, escalated by the blocker panel (plan 04 §3.3) |

- **Each message is one clean title line.** The title is the evaluator's
  rewrite (plan 04), not the model's first sentence.
- **`TAB`** expands the description and context in place.
- **`RET`** opens the message's own **org file**: its evidence, the part of the
  original plan it concerns, and the votes and history.
- **The owner's verdict**, in normal state:
  - **`A`** accepts the message.
  - **`D`** refuses it. Next review round, the evaluators pick up every
    message the owner tagged `D` and evaluate it again, together with any
    comment the owner wrote in the input box.

  `A` and `D` override Evil's append and delete-to-end-of-line, which is fine
  in a read-only view buffer, but it needs checking.
- **The buffer stays clean:** no raw ballots, no truncated first sentences,
  and no duplicates.

## 3. Two graphs, drawn from the plan

Rendered as ASCII from the plan file itself, so the owner always knows where
things stand:

1. **The loop graph** (one phase): the stages of this phase's loop as the
   pipeline config defines them (plan 04 §6):
   - the worker, the reviewers, the evaluators and the blocker panel;
   - for each, the **role**, the **model**, **iteration numbers** and **time
     and cost spent**;
   - **where the phase is now**.
2. **The program graph** (the program): the dependency graph of phases, each
   node's status, and which stage the running ones are in.

This is worth a phase of its own in plan 03. Both graphs exist today in weaker
forms: the pipeline line (`implement → freeze → checks → …`) and the program
status list.

## 4. Ids and provenance

- **A program id,** plus **phase ids formed from the program id and the phase
  number** (for example `<program>-01`, `<program>-02`). Easy to say, type and
  recognise. Today run ids are random, unrelated 8-hex strings.
- **`C-c m p` shows the source plan file** next to the program id, for example
  `~/orgw/work/atlas/indexps/14_program.org`. That header replaces the tab bar,
  so the tab bar can go.

## 5. Keys that don't fight Evil

| Action | Today | Plan 03 |
|---|---|---|
| Stop (pause) a phase or program | `k` in the program and runs buffers | **`C-c m k`**, only with point in a phase buffer (the loop graph) or the program buffer; asks for confirmation (`RET` confirms, `n` cancels) |
| Resume or continue | `R` | **`C-c m c`** |
| Accept or refuse a message | none | **`A`** / **`D`** in the review buffer |
| Expand a message | `TAB` (org folding in the decisions view) | `TAB` |
| Open a message's org file | `RET` jumps within the decisions view | `RET` opens its own file |

The rule: global actions live under `C-c m`. Single letters are used only in
read-only view buffers, and never ones Evil users reach for when moving around
(`k`, `R`, `g`, `i`).

## 6. Today vs plan 03

| Plan 03 | Today (PR #39 HEAD) |
|---|---|
| Emacs is only a front end | The runner lives in `~/.emacs.d/tradeoffs-trace/`. The front end reads files and calls `tt`, and works over TRAMP. |
| One colour-coded review buffer | The decisions view (`C-c m d`): decision blocks, then findings, then advisories, all model text verbatim |
| `TAB` context, `RET` message file | Org folding; `RET` moves within one buffer |
| `A`/`D` verdicts feed the next round | Only free text through the input box (steer, note, correction) |
| Loop graph and program graph | A one-line pipeline string and a status list |
| `program-NN` phase ids, source file shown | Random run ids; the source plan isn't shown |
| No tab bar | One `tab-bar` tab per run workspace |
| `C-c m k` / `C-c m c` with confirmation | `k` / `R` (conflict with Evil) |

## 7. Dependencies

- **From plan 04:** the message model: trade-offs, findings and blockers, the
  evaluator's rewritten titles, and the `D` feedback loop to the evaluators.
  Plan 03 renders it. Until 04 lands, the review buffer can render today's
  records with the same layout and keys.
- **The loop graph** needs the pipeline config (plan 04 §6) as its source. For
  today's fixed loop, a built-in description of the tradeoffs-trace profile
  stands in.

## 8. Notes (2026-09-27)

- **Let the runtime render and Emacs display.** The runtime writes a
  ready-made `review.org` and the graph text per phase; Emacs reverts files
  instead of running `tt state` every 2 s. This makes remote use cheap, and
  any future front end gets the same view.
- **One file per phase, details on demand.** A single review file with a
  subtree per message; the message's own detail file is fetched only when the
  owner presses `RET`, keeping TRAMP round-trips low.
- **`D` asks for a one-line reason** (minibuffer), which becomes the
  evaluators' input. The input box stays for anything longer.
- **Stable ids across retries.** A retried node gets a new run, so `program-NN`
  must name the phase, with the attempt or run as a suffix.

### Sequencing: the contract first, then 03 and 04 in parallel

Plans 03 and 04 both change the runtime. They can still run in parallel if a
short **contract phase comes first**:

- **The contract** is a versioned on-disk spec with fixture files:
  - the run directory layout;
  - the message schema (trade-off, finding, blocker);
  - the **settled ledger**;
  - the verdict files (`A`/`D` plus reason);
  - the rendered view files (`review.org`, the loop graph, the program graph);
  - the inbox commands.

  It is the real interface between the runtime and any front end, so moving
  the runtime out of `~/.emacs.d` later changes nothing for Emacs.
- **04 owns the producing side:** the loop, the agents, the message flow and
  the ledger. It writes messages and the ledger in the contract's format.
- **03 owns the rendering side:** the runtime renderer (contract files in,
  `review.org` and graph text out) and Emacs, which only displays and writes
  verdicts and inbox commands.
- After the contract, 03 builds against the fixtures while 04 builds the
  runtime that produces them. Neither waits for the other.

Proposed order:
1. The contract and the runtime-rendered views. This serves 03, 04 and remote
   use at once, and replaces Emacs calling `tt state` every 2 s, which is
   costly over TRAMP.
2. **In parallel:**
   - 04 step 1: the message format, the evaluator and the ledger, inside
     today's fixed loop;
   - 03: the review buffer and `A`/`D`, on the contract fixtures.
3. 04 steps 2–3: models per unit, then the pipeline language. 03's loop graph
   is drawn from it.

Test each step with a small practice program like 02. Before starting, agree
what "better" means in numbers, for example owner minutes per phase, rounds
per phase, and how often the owner refuses something (`D`).
