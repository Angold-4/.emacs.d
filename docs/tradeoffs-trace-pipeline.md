# tradeoffs-trace as a programmable review pipeline (plan 04)

Status: **Owner's direction, recorded 2026-09-26.** Not implemented. This is the
intent that plan 04 builds toward; plan 03 (the UI) is parked behind it. It
refines [tradeoffs-trace](tradeoffs-trace.md) and follows from
[Human judgment and programmed agent workflows](agent-evaluation-and-programmed-workflows.md).

## 1. The spirit

> Two developers using the same agents can produce very different results. The
> difference is not explained by their initial prompts alone. It also comes from
> their judgment throughout execution: which assumptions they challenge, which
> decisions they recognize as wrong for the goal, which compromises they accept,
> and when they redirect the work.

tradeoffs-trace exists to put that judgment in the loop, programmatically.
Reviewers are there to **help the owner review**: to spot the trade-offs and the
facts worth surfacing, and to present them so the owner can evaluate them
quickly. In steady state, **about 90% of the loop's time is review.**

We are not building another agent harness (DeepSeek's, OpenClaw). Pi stays the
agent runtime. What we build is the **pipeline and message flow** around the
agents.

## 2. Vocabulary: trade-offs, findings, blockers

- **Trade-off**, not "decision". A decision means nothing without the context
  it was made in. A trade-off carries it: what was chosen, what was given up,
  and why it matters for the goal. Trade-offs are raised **mainly by the
  worker**, as it makes them.
- **Finding:** a fact a reviewer raises about the implementation: a defect, a
  gap against the plan, or evidence worth knowing.
- **Blocker:** a finding that says the work should stop until the owner
  decides.

All three are **structured messages** in one shared format, so a program can
capture, route, deduplicate, evaluate and render them. Prose in a log isn't
enough.

## 3. The message flows

### 3.1 Trade-offs: raised by the worker, evaluated, then posted

1. **Raised.** Whenever the worker makes a trade-off, it raises it as a
   structured message: the intent, the choice, the alternative given up, and
   the evidence. How to make the worker raise these reliably and in structure,
   without a fancy prompt, is an open design question (§8).
2. **Evaluated.** Each trade-off goes to a **fresh evaluation agent**. It checks
   the claim against the implementation, adds the context that makes it
   reviewable or rejects it as invalid, and rewrites it in clean, human
   wording.
3. **Posted.** It is appended to the run's trade-offs org file, which is what
   `C-c m d` shows:
   - **The title is one clear line.** If the owner is concerned, `TAB` or `RET`
     expands the full context and evidence the evaluator wants them to know.
   - **Trade-offs are listed first,** before the rest. Reviewers vote on them;
     the owner reads them when they choose to.

### 3.2 Findings: raised by reviewers, evaluated, then fed back

Reviewers raise findings in the same shared format. Each goes through a fresh
evaluation agent, which confirms it against the implementation and polishes
it. Confirmed findings are aggregated into the **next worker round's input**,
and posted for the owner.

### 3.3 Blockers: a voted escalation

The plan will never be perfect: surprises come up in implementation and at
runtime. The owner **encourages** raising blockers, but through a pipeline, not
straight to their desk:

1. Any reviewer may raise a blocker.
2. The blocker goes to a **panel of three or more fresh evaluation agents**.
   Each sees the original plan, the owner's intent from the input box and the
   blocker's context.
3. The panel **votes**:
   - **Majority (for example 2/3) says block:** the whole implementation
     stops, and a **red blocker** goes to the owner, with the context and the
     options to choose from.
   - **Otherwise:** the blocker becomes a **finding**. It is recorded and
     aggregated into the next implementation round for the worker.

### 3.4 The balance to find

Too many trade-offs and too much evidence, and the owner can't review. Too
few, and a version ships with unexposed decisions, which is hard to review
after the fact. The evaluation steps (dedupe, validate, polish, then publish
or drop) are where that balance is set. The pipeline must make it tunable and
measurable.

## 4. Fresh agents, no fancy prompts

- **Every reviewer is a new agent,** with no memory of earlier reviews. It
  knows only the plan file, the owner's intent from the input box, and the
  current trade-offs, findings and decisions. So context-window size stops
  being a concern, and prompts can stay plain.
- **Every worker round is fresh too,** from the same inputs plus the findings
  aggregated for it.
- **Evaluation agents are fresh per message.**

## 5. Models per unit

Every unit's model is configurable: the worker, each reviewer, each evaluator
and the blocker panel. Reviewers can mix vendors (DeepSeek, Claude, GPT). In
general, **the further the loop goes, the more serious the model**. The
escalation is part of the loop's own configuration.

## 6. The programmable pipeline (plan 04)

Today `program.org` defines only the order of plans and each plan's reference
files. Plan 04 extends its semantics so that **the loop itself is defined in
org**, using org's embedded structure, and a parser turns it into the runtime
pipeline. The configuration covers:

- **the units:** worker, reviewers, evaluators, blocker panel; how many of
  each, the model for each, and when each is fresh;
- **the message types and their routing:** who raises what, who evaluates it,
  the vote thresholds, and where the result goes (owner, next round, dropped);
- **the loop structure:** rounds, repair budgets, and escalation of models
  across rounds;
- **what the owner sees:** trade-offs file, findings, blockers.

**tradeoffs-trace becomes one profile of this pipeline:** a single worker, a
panel of reviewers that vote, and trade-offs and facts spotted along the way.
Other workflows are other profiles; the runtime is the same.

## 7. What today's implementation does (evidence from plans 13, 14, 01 and 02)

| Intent (above) | Today |
|---|---|
| Trade-offs raised by the worker, evaluated, polished | The worker and reviewers write records (`submit_phase` disclosures, `submit_discovery`), which are shown **verbatim**. No evaluator validates or rewrites them. The 02a run showed near-duplicate records from two reviewers and trivia listed next to the one real trade-off. |
| Clean titles, details on `TAB` | The title is the choice's first sentence, truncated. |
| Findings routed back to the worker | Blocking findings do go back; advisories accumulate (190 in plan 13's run) and are shown as-is. |
| Blockers voted by a panel before reaching the owner | A reviewer's blocking finding or a veto (M) goes straight to a repair round, and after three rounds to the owner. There is no evaluator panel. Plan 14's 14h spent 15 rounds, many of them on contract disputes. |
| Fresh agents | Already true: each attempt and each review dispatch is a fresh Pi session. |
| Model per unit, escalation | One model for every unit (Pi's default). |
| Loop defined in org | Fixed in the conductor's state machine: one worker, reviewers M/A/B, M veto, 2 of 3, 3 repair rounds. |

## 8. Open questions

1. **Structured raising by the worker.** How does the worker raise a trade-off
   the moment it makes one, as data, not as prose at the end? A tool call per
   trade-off? A required field on each commit? Diff-anchored notes?
2. **Measuring the balance.** What does the owner count to know the pipeline
   is exposing enough and not too much? For example: trade-offs published per
   round, owner expansions per trade-off, trade-offs later found wrong.
3. **The evaluator's inputs.** It needs the plan, the owner's intent, the
   message and the implementation (the diff and a checkout). Does it also see
   the other open messages, so it can deduplicate?
4. **Cost.** A fresh evaluator per message, and three or more per blocker. The
   budget should be visible in the pipeline config and in the runtime view.
