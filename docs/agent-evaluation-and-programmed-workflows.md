# Human judgment and programmed agent workflows

Status: **Design rationale for discussion.** This document explains the direction
for agent work in Emacs; it does not describe an implemented orchestrator.

## The problem: the same agents can produce very different outcomes

Two developers using the same agents can produce very different results. The
difference is not explained by their initial prompts alone. It also comes from
their judgment throughout execution: which assumptions they challenge, which
decisions they recognize as wrong for the goal, which compromises they accept,
and when they redirect the work.

A clear prompt helps establish direction. Skills help agents apply useful
methods. Neither settles every choice that a large project will reveal during
implementation. Final quality depends on the evaluation and correction loop,
including the human's ability to recognize when an apparently good solution is
solving the wrong problem or paying the wrong price for the intended result.

When agents perform nearly all of the coding, the developer's scarce resource
becomes attention: expressing intent, recognizing consequential choices, and
deciding whether the result serves the intended purpose. Generating more code
does not necessarily increase useful throughput. It can increase the amount of
unexamined work waiting for a human decision.

A typical workflow already delegates implementation:

```text
Read the plan.
For each phase:
    understand the phase
    launch a worker
    inspect the result
    request fixes until the result is acceptable
    continue
```

An agent can follow these instructions for hours and produce useful work. But
the developer still has to wait, inspect long reports, reconstruct context, and
notice when implementation has changed the meaning of the plan. The agent also
has to remember which obligations remain outstanding.

Two claims motivate a different approach:

1. Human judgment inside the loop shapes the quality of the result: achieving a
   particular goal requires particular trade-offs, and the owner must recognize
   whether the work is making the right ones for that goal.
2. Workflow obligations need executable enforcement, even when models become
   more capable and better at following instructions.

The objective is to increase both human and agent bandwidth: let many tasks
advance without constant supervision, while making the decisions that need
human judgment visible at the time that judgment can still change the outcome.

## 1. Quality depends on judgment throughout the loop

The important human contribution is more than permission to approve the final
output. It is the ability to judge the work against its purpose while there is
still an opportunity to change it. A developer who cannot recognize unsuitable
trade-offs can approve a polished implementation that undermines the goal. A
developer who notices them can use the same agents to produce a better result.

For example, two developers ask the same agent to make a service faster. Both
receive a cache that improves the benchmark. One accepts the improvement; the
other notices that stale results would violate the service's purpose and asks
for a different approach. For a different service, bounded staleness might be
exactly the right compromise. The judgment is about the relationship between
the goal and the cost of the improvement, not whether caching is generally a
good technique.

### A goal gives a trade-off its meaning

There is no universally best balance of speed, consistency, compatibility,
maintainability and delivery time. Achieving a specific goal requires choosing
which properties to protect and which costs to accept. The owner often knows
why the project exists, what failure would mean, and which obligations matter
in ways that are only partially expressed in a prompt. Implementation also
reveals constraints the owner could not have specified beforehand.

An agent evaluates against the objective and context available to it. Better
reasoning can improve that evaluation, but it does not by itself establish that
the objective captures the owner's actual priorities. When the representation
is incomplete, the agent may optimize an apparently sensible criterion while
moving the project away from its purpose.

This is the useful meaning of keeping evaluation human: consequential trade-offs
must remain connected to the owner's understanding of the goal, and that
understanding must be able to correct the work as it develops. It is not a
claim that every human judgment is better than every agent judgment. An agent
can expose a trade-off the owner misunderstood and help the owner change their
mind. Nor does it require the owner to make every implementation choice; clear
priorities and explicit delegation let agents handle many choices well.

### Technical evaluation and goal judgment are different

“AI can never evaluate better than a human” would overstate the argument. An
agent may find a defect a person misses, identify an inconsistency, or supply a
better argument. A test runner can establish an observed result more reliably
than a person reading a transcript. These capabilities support judgment; they
do not establish that an unexamined compromise serves the owner's goal.

There are three different questions inside evaluation:

| Question | Appropriate authority or evidence |
| --- | --- |
| Did the specified check pass on this revision? | Recorded execution of the check |
| Does this design satisfy the requirements, and what could fail? | Tests, measurements, analysis and independent review |
| Are these the right requirements, and is this trade-off acceptable? | The human owner, within explicitly delegated policy |

The last question depends on priorities that the model does not acquire merely
by becoming better at reasoning. Faster delivery, compatibility, maintainability,
cost and risk can conflict. The owner can delegate choices among them, but the
scope of that delegation must be explicit. A model's preferred compromise is
not automatically the owner's preferred compromise.

Consider a disagreement between a developer and an agent:

1. **The human is right.** The human supplies evidence, the agent revises its
   assessment, and the result improves.
2. **The agent is right, but the human remains unconvinced.** The human can still
   choose the product direction or accept a risk. The evidence and disagreement
   should remain recorded; an accommodating answer from the agent does not make
   the human's factual claim correct.
3. **The agent is right and persuades the human.** The human changes the decision
   because the agent exposed something useful.

In all three cases, the human's judgment affects what is ultimately delivered,
including when that judgment is mistaken. The value of the loop is that evidence
can improve the judgment before the choice becomes part of the result. Merely
obtaining approval does not make the evaluation good. Keep **evidence,
judgment, and authorization** distinct.

The system should therefore help the human make better decisions, including by
challenging the human. Agreement alone is a weak acceptance criterion: both
parties can agree on something false. A useful evaluation record preserves the
supporting evidence, unresolved objections and the scope of any accepted risk.

### The evaluation pipeline also embodies judgment

Choosing what to test, what reviewers should inspect, which findings block
delivery, and which decisions reach the owner determines what the workflow
treats as quality. If the pipeline rewards only benchmark speed, it may
consistently accept changes that break freshness guarantees. More review rounds
against that same incomplete criterion need not expose the problem.

A weak evaluation process can therefore make a capable agent reliably produce
the wrong outcome. Fancy prompts cannot compensate for acceptance criteria
that miss the goal. A programmed pipeline cannot compensate for them either:
it can enforce the chosen checks faithfully while enforcing the wrong standard.
The human must be able to inspect and revise that standard as evidence arrives.

This matters especially in large projects, where individual changes can look
reasonable while their combined trade-offs undermine the intended system.
Evaluation needs both local correctness checks and opportunities to reconsider
whether the integrated result still serves the goal.

## 2. The most dangerous failure is an unexposed decision

The value of judgment depends on having something meaningful to judge. The
three disagreement cases assume that the human sees the issue. A more
dangerous case happens before disagreement is even possible:

```text
The agent encounters an ambiguity.
It chooses an interpretation.
That interpretation changes behavior.
The change is absent from the decision report.
The human sees a successful delivery and never evaluates the choice.
```

For example, a worker improves a matching engine by moving cancellation onto an
asynchronous queue. The implementation passes the existing tests. But successful
cancellation now means “the request was queued,” while the owner expected “no
further fill can occur after acknowledgement.” A summary saying “refactored
cancellation; all tests pass” hides the consequential change.

The decision can be wrong without the code being obviously broken. It can also
be a reasonable engineering choice that the owner would have rejected because
of a constraint the agent did not know. Either way, the human lost the chance
to decide. An unexposed choice that happens to be correct is still an oversight
gap, because its consequences were never considered by the responsible person.

This failure is difficult to catch by asking the human to review everything.
As output increases, reading every diff, transcript and test becomes the next
bottleneck. A final “looks good” cannot reasonably count as informed approval of
every unstated assumption inside the result.

### Disclosure must have several sources

Requiring a worker to list its decisions helps, but cannot establish that the
list is complete. The same agent may fail to recognize the significance of a
choice it made. A stronger workflow combines:

- **Worker disclosure:** assumptions, behavior changes, deviations from the
  plan, alternatives considered and unresolved questions.
- **Independent review:** inspect requirements and the candidate diff before
  reading the worker's persuasive account of why its design is correct.
- **Observable triggers:** detect changes to public interfaces, persistence,
  dependencies, permissions, acceptance criteria and other declared boundaries.
- **Human sampling:** inspect some apparently routine accepted work to discover
  systematic omissions in the disclosure and review process.

Reviewers should identify evidence and consequences, not merely vote. One
reproduced defect outweighs several approvals. Some valid findings require an
argument against a requirement rather than an executable reproduction.

None of these mechanisms proves that every important decision was exposed.
Fresh agents can share blind spots, tests can omit a requirement, and disclosure
can be incomplete. The purpose is to reduce those failures, preserve evidence
when they occur, and improve the process from observed misses.

## 3. Why stronger models do not remove the need for a pipeline

Prompt instructions and executable control have different jobs. A skill can
explain how to review a design, investigate a failure or write maintainable
code. It is a useful method description. Whether a required review happened is
a property the surrounding system must establish.

In a prompt-driven loop, the model is responsible for both doing the work and
remembering the conditions under which it may continue. It can skip a review,
mistake a confident report for evidence, lose a requirement after compaction,
or stop repairing because the result seems good enough.

A larger context window can reduce forgotten instructions. A better model can
improve judgment. Even a model that follows every instruction still operates in
a system with process failures, changing repositories, concurrent work, costs,
human decisions and external side effects. Those need explicit coordination.

| Obligation | What programmed control contributes |
| --- | --- |
| Required checks | Runs the named commands and records their results |
| Independent review | Dispatches the required role with controlled inputs |
| Valid evidence | Associates checks, reviews and approvals with a revision and contract version |
| Recovery | Reconstructs pending work from durable state after interruption |
| Bounded work | Enforces attempt, time and spending limits |
| Human authority | Waits for a recorded decision where policy requires one |
| Concurrency | Coordinates ownership and dependencies across active tasks |
| Auditability | Explains which evidence and policy allowed each transition |

Repeatedly asking a model whether a known process exited successfully spends
time and tokens on something ordinary code can establish. Persisting the result
also avoids asking a future session to reconstruct it from conversation.

The pipeline's long-term role is to make this evaluation loop dependable: gather
evidence, expose consequential choices, apply the agreed standard, and preserve
the opportunity to revise that standard. It makes judgment usable across many
tasks without requiring a person to watch every agent turn.

More capable agents may make oversight more important because they can make
more changes before a person reads the output. The useful scaling target is
completed work per unit of human attention, with an acceptable defect rate.
Raw agent throughput is insufficient.

### Deterministic control does not imply deterministic correctness

The workflow can guarantee that a recorded outcome and state select a defined
next action. It cannot guarantee that a semantic assessment is correct merely
because its output is structured or its routing is deterministic.

Use programs for observable invariants: exit status, required artifacts,
revision identity, permission, budget and state transitions. Use models for
bounded judgments: whether a claim follows from evidence, whether an assumption
conflicts with a requirement, or whether a design deserves closer review.

Semantic gates need explicit criteria, measured error rates, failure routes and
retry limits. A `PASS` is an assessment under those conditions, not proof of
overall correctness. Missing or malformed evidence cannot silently count as
success. Adding a model gate to every step would recreate the latency and
friction the system is meant to remove.

## 4. What controlled execution should look like

A programmed workflow should own the lifecycle while agents propose and carry
out bounded work:

```text
Ready phase and versioned acceptance contract
    -> worker produces a candidate revision
    -> runner executes required checks
    -> independent reviewers assess the candidate
    -> code routes the recorded outcomes
        -> repair within the attempt budget
        -> wait for a human decision
        -> stop with an explicit blocked or failed result
        -> accept and integrate the candidate
    -> verify the integrated result before dependent work advances
```

The worker's “done” starts verification. It does not advance the phase by
itself. Checks and approvals refer to the revision and requirements they
actually evaluated. A changed candidate invalidates affected evidence. A
changed requirement creates a visible revision of the contract.

The plan can evolve. Only the next executable phase needs a sufficiently
precise contract; later phases can remain provisional. When implementation
reveals a new constraint, the system records a proposed change, applies the
appropriate decision policy, and revisits dependent work. Controlled execution
should make adaptation traceable, rather than demand a perfect initial plan.

The control boundary must also exist outside the prompt. Workers must not be
able to overwrite authoritative approvals or forge the runner's test records.
Separate worktrees organize edits; filesystem and process permissions are
needed to restrict access to control state.

### Human attention is part of the workflow

Every choice does not need an interruption. Use an explicit delegation policy:

| Choice | Treatment |
| --- | --- |
| Reversible implementation detail inside delegated scope | Proceed and record |
| Consequential choice already permitted by policy | Notify with its rationale and evidence |
| Requirement change, unresolved conflict, or reserved trade-off | Request a decision and block affected work |

A decision request should show what must be decided, why it matters now,
alternatives, the recommendation, evidence, and which work depends on it.
Unrelated authorized tasks can continue while the human is away. Silence does
not authorize a choice that requires a response.

Discussion, steering and approval should be distinct operations. Asking whether
a queue might help is exploratory conversation. Adopting weaker cancellation
semantics is a decision that changes the task. The interface should preserve
that distinction without forcing the human to write formal prompts for every
exchange.

## 5. Why Emacs, Pi and OrgBrain fit this direction

Emacs provides a place to think, inspect evidence and decide while work runs in
the background. A task buffer can hold intent and current progress; a decision
inbox can collect consequential choices across projects; agent transcripts can
remain available for investigation. The human should not have to monitor every
stream to discover a question that blocks work.

The proposed division of responsibility is:

- **Pi** supplies agent execution and sessions. The workflow service owns
  dispatch, recovery and the conditions under which a result is accepted.
- **OrgBrain** assembles relevant project context, records decisions and
  exposes bounded judgment services. Its deterministic control layer can own
  workflow transitions and operational state.
- **GBrain** supplies knowledge storage and retrieval. Execution traces,
  tentative claims and accepted project knowledge retain distinct meanings.
- **Emacs** exposes the work and its decisions through ordinary buffers, with
  direct access to supporting diffs, checks and prior constraints.

Recording a worker's claim must not automatically promote that claim into
accepted knowledge. Otherwise the developer has to police every conversation
to keep the brain clean. Operational history should support inspection and
recovery; selected outcomes should enter durable knowledge through a separate,
scoped admission path.

This direction deliberately extends OrgBrain's existing bandwidth model, which
leaves the loop with the agent host. The proposed change gives code ownership
of workflow obligations while retaining agents as replaceable workers. It
should begin with one worker, actual checks, an independent reviewer and a
durable decision inbox. Additional reviewers and semantic gates should earn
their cost through measured improvements.

## 6. How to judge whether the direction works

Evaluate the workflow by the burden it removes and the failures it catches:

- Human active time per completed task.
- Repeated context explanations and manual handoffs.
- Decision requests that actually required human judgment.
- Consequential choices discovered only after acceptance.
- Defects that escaped checks and review.
- Recovery after interruption, end-to-end latency and execution cost.

A reduction in notifications alone is not success: it can mean the system has
started hiding decisions. More gates alone are not success either: they can
consume attention without catching meaningful failures. Compare both the
quality of delivered work and the human effort needed to obtain it.

The durable principle is that stronger models expand what can be delegated,
while judgment determines whether that work advances the intended goal.
Explicit authority, inspectable evidence and executable workflow obligations
make that judgment usable at scale. Their success depends on exposing the right
choices and applying a standard that the human can challenge and improve.

## Related project context

- [OrgBrain bandwidth model](https://github.com/Angold-4/orgbrain/blob/main/docs/18-bandwidth-model.md):
  the input and output costs this proposal aims to reduce.
- [OrgBrain project knowledge](https://github.com/Angold-4/orgbrain/blob/main/docs/16-project-knowledge.md):
  the separation of operational state, durable knowledge and the human interface.
- [OrgBrain issue #96](https://github.com/Angold-4/orgbrain/issues/96): bounded
  semantic assessments with explicit criteria and deterministic routing.
- [Pi RPC documentation](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/rpc.md):
  the headless execution interface relevant to a workflow service.
- [Emacs Pi integration PR #12](https://github.com/Angold-4/.emacs.d/pull/12):
  the Pilish integration that motivates a native buffer interface.
