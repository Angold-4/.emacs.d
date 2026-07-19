# Delegation Prompt - Git Review Workbench Phase 3

You are the implementation worker for Phase 3 of a personal Emacs Git review
workbench. Implement canonical repository identity and explicit local contexts
on the existing PR #5 branch. Phase 0, Phase 1, and Phase 2 are accepted
foundations. The lead agent will review your filesystem changes and automated
evidence; the owner will then exercise the result in terminal and graphical
Emacs before Phase 4 begins.

## Repository, branch, and instructions

- Repository: `/home/ango/.emacs.d`
- Required branch: `feat/git-review-workbench-phase-1`
- Pull request: `https://github.com/Angold-4/.emacs.d/pull/5`
- Accepted Phase 2 commit: `3f248ff`
- Phase 3 remains in PR #5 by explicit owner decision. Ignore the older
  `feat/git-review-03-repository-store` branch suggestion in `git-plan.md`.
- Read `/home/ango/.codex/RTK.md` completely before running commands.
- Prefix every shell command with `rtk`.
- Inspect `rtk git status --short --branch` before editing. Stop and report if
  the branch differs or if unrelated uncommitted changes are present.
- Do not create or switch branches.
- Do not stage, commit, push, amend, rebase, reset, restore, clean, fetch, or
  modify PR #5. The lead owns Git history and remote metadata after review.
- Do not install, update, rebuild, or repin packages.
- Do not contact any remote during implementation or tests.

Read these files before editing:

1. `git-plan.md`, especially the invariants, ownership table, and Phase 3.
2. `docs/git.md`, especially Sections 2.4, 3.6, and 4.1 through 4.8.
3. `docs/git-baseline.md`.
4. `git-phase-1-worker-prompt.md` and `git-phase-2-worker-prompt.md` for the
   accepted navigation and local-review contracts.
5. `core/init-git.el`, `core/init-git-ui.el`, `init.el`, and all files in
   `test/`.
6. The locally installed Magit and Forge sources when verifying APIs. Do not
   guess API signatures.

Run the existing suite before editing:

```bash
rtk emacs --batch -Q -l init.el -L test \
  -l test/git-review-test.el -f ert-run-tests-batch-and-exit
```

The starting gate is 40 passing ERT tests. Preserve it throughout Phase 3.

## Product outcome for this phase

Introduce one reliable repository registry that knows that these are the same
remote repository:

```text
git@github.com:org/dragon.git
ssh://git@github.com/org/dragon.git
https://github.com/org/dragon.git

canonical id: github.com/org/dragon
```

At the same time, preserve the fact that two clones or linked worktrees have
different local state:

```text
canonical repository: github.com/org/dragon

local context A
  root: ~/work/dragon
  branch: main
  index/worktree: A only

local context B
  root: ~/work/dragon-review
  branch: feature
  index/worktree: B only
```

Shared immutable review identity may use the canonical repository and object
IDs. Worktree, staged, and local branch review must remain tied to the exact
local context that owns the branch, index, and files.

Phase 3 is an identity and ownership phase. It must not fetch, create a mirror,
query GitHub, synchronize repositories, or implement PR/issue UI.

## Scope: Phase 3 only

Expected paths:

- new `core/init-git-store.el`;
- `init.el` for dependency order;
- narrow integration changes in `core/init-git-ui.el`;
- `test/git-review-test.el`;
- new `test/git-review-integration-test.el` for clones/worktrees/registry;
- existing fixtures only when reusable local repository helpers are needed.

Keep ownership:

```text
init-git -> init-git-store -> init-git-ui
```

Do not move existing Forge configuration into a new module in this phase. Do
not create `init-forge.el`, mirrors, sync commands, PR buffers, issue buffers,
or speculative remote abstractions.

## 1. Create explicit repository and local-context structures

Create `core/init-git-store.el` with explicit `cl-defstruct` values or an
equally inspectable representation.

A canonical repository record must contain at least:

```text
repository-id
forge-repository        optional, never required
local-contexts
shared-buffers
cache-generation
last-success            reserved/local metadata only
last-error              reserved/local metadata only
```

Phase 4 fields such as mirror directory, sync process, or waiting buffers may
be represented as nil placeholders only when that materially simplifies the
stable structure. Do not implement their behavior.

A local context must contain at least:

```text
context-id
repository-id
root
git-dir
git-common-dir
current-branch
head
upstream
remote-name
remote-url
```

Definitions:

- `root`: normalized worktree root;
- `git-dir`: absolute per-worktree Git directory;
- `git-common-dir`: absolute shared Git storage directory;
- `current-branch`: symbolic branch or nil for detached HEAD;
- `head`: resolved HEAD OID or nil for an unborn repository;
- `upstream`: symbolic upstream or nil;
- `context-id`: stable identity that distinguishes independent clones and
  linked worktrees. The per-worktree `git-dir` must participate.

Use public local Git commands with argument lists, never shell command strings.
Handle spaces and Unicode in every filesystem path.

## 2. Canonicalize remote repository identity

Implement a pure, directly tested URL-to-identity normalizer.

These must compare equal:

```text
git@github.com:org/dragon.git
ssh://git@github.com/org/dragon.git
https://github.com/org/dragon.git
git://github.com/org/dragon.git
```

Expected identity:

```text
github.com/org/dragon
```

Rules:

- Ignore transport and user information.
- Lowercase the host.
- Remove redundant leading/trailing slashes and one trailing `.git`.
- Retain the complete repository path after the host so nested forge groups do
  not collide.
- Retain meaningful non-default host ports.
- Same owner/name on different hosts must never collide.
- Do not key by local root, remote alias, push URL, or transport.
- Reject malformed/ambiguous URLs cleanly instead of manufacturing an unsafe
  global identity.
- Local paths and `file://` remotes must remain local identities and must not
  collide with hosted repositories.

Prefer a public Forge parsed-repository API when Forge is already available
and the remote is supported. Verify the API against the installed Forge
source. Forge must remain optional: the pure normalizer and registry must work
when Forge is unavailable and must not open or query the Forge database.

Choose the fetch remote deterministically:

1. the configured upstream remote for the current branch, when present;
2. `origin`, when present;
3. the lexically first remote with a fetch URL;
4. a safe local-only repository identity when no usable remote exists.

Renaming a remote alias without changing its fetch URL must preserve the
canonical identity. Never use a push-only URL as canonical input.

## 3. Build an in-memory registry with safe thin persistence

Maintain one live in-memory canonical record per repository identity. Repeated
registration must return the same record and update its local contexts rather
than duplicate it.

Required behavior:

- Register or refresh a context from a repository root.
- Look up a canonical repository by id.
- List live contexts deterministically.
- Look up a context by id or normalized root.
- Prune dead buffers from `shared-buffers`.
- Mark missing/deleted context roots unavailable without breaking the rest of
  the canonical record.
- Increment `cache-generation` only when stored registry/context state changes,
  not on every read.
- Reset and rebind the registry cleanly in tests.

Persist only thin scalar metadata under a configurable Phase 3 registry path
inside:

```text
~/.emacs.d/.cache/git-review/repositories/
```

Use an auditable data format such as JSON and an atomic same-directory
temporary-file plus rename. Never serialize buffers, processes, markers,
closures, or package objects. Malformed persistence must degrade to an empty
registry with a useful message and must not evaluate Lisp.

Tests must bind the storage root to a temporary directory. Do not touch the
owner's real registry or review state.

Do not create `mirror.git`, `repository.json`, `checks.json`, or
`sync-state.json` yet. Those belong to later phases.

## 4. Define canonical versus local identity ownership

Extend the accepted Phase 2 review target only as needed to carry:

- canonical `repository-id`;
- originating `context-id`;
- enough legacy identity information to preserve existing review state;
- active edit context for shared immutable views, preferably as a separate
  buffer-local value rather than mutating immutable target identity.

Identity rules:

### Worktree and staged review

- Persistence and buffer identity include canonical repository id plus exact
  local context id.
- Two clones of the same remote must never share worktree/staged buffers or
  checkmarks.
- Linked worktrees must never share index/worktree state or buffers.

### Local branch review

- Persistence includes canonical repository id, exact local context id, and
  symbolic base/head labels.
- Immutable buffer identity additionally includes resolved base/head OIDs.
- A branch advancing in one context must not mutate or replace another
  context's branch review buffer.

### Commit review

- Persistence and immutable buffer identity use canonical repository id plus
  resolved commit/base OIDs.
- The same commit opened from two same-origin clones may reuse the same
  immutable review/tree/file-diff buffers.
- Same OIDs on different canonical repositories must not collide.

This separation is mandatory. Do not reintroduce the earlier bug where one
identifier was used simultaneously for stable persistence and changing
immutable buffer identity.

## 5. Preserve and migrate Phase 2 reviewed state safely

Phase 2 currently has root-based review-state filenames. Moving to canonical
and context-aware keys must not silently discard existing owner checkmarks.

Implement a narrow compatibility path:

- Compute the new canonical/context-aware state key.
- If new state exists, use it.
- If it does not exist and the exact legacy root-based state file exists, load
  it with the existing malformed-state safeguards, validate fingerprints, and
  atomically write the new generation.
- Do not delete or rewrite the legacy file automatically.
- Never merge unrelated local worktree/staged/branch state across contexts.

Unchanged fingerprint semantics remain mandatory: unchanged files retain
reviewed state; changed or renamed paths become unreviewed unless their exact
key/fingerprint still matches.

## 6. Add active local-context selection without changing window behavior

Add an ASCII-safe `L` action to `+git-review-buffer-mode` for selecting the
active local context when the current review is safely shareable.

Behavior:

- Shared immutable commit review buffers may select among registered live
  contexts for the same canonical repository.
- Selection uses existing local contexts only. Never clone or create a
  worktree automatically.
- A selected context must contain the reviewed objects before it becomes the
  active context. Check with local Git only.
- `e` uses the active context root for the writable file destination.
- Reopening a shared immutable commit buffer from another live clone may make
  that clone the active edit context without changing the immutable review id.
- Worktree, staged, and local branch reviews are fixed to their originating
  context. `L` must explain that clearly rather than silently redirect local
  state.
- If the active context disappears, choose another eligible registered context
  deterministically or signal a clear user error.
- `L` and `e` must preserve Phase 1 same-window and `q` return behavior.

Provide a noninteractive selection/helper API so ERT does not have to drive
the minibuffer.

Make shared identity visible without building a dashboard. A compact ASCII
header suffix or a small inspect command is sufficient, for example:

```text
repo: github.com/org/dragon | context: ~/work/dragon
```

Repository-provided host/path/context text may contain Unicode, but all new UI
chrome and separators must remain ASCII-safe.

## 7. Degrade safely when contexts or remotes change

Handle:

- remote alias renamed with the same URL;
- remote URL changed to another canonical repository;
- repository with no remote;
- detached HEAD;
- unborn HEAD;
- deleted/moved local context root;
- original clone for a shared immutable buffer deleted while another clone is
  still registered;
- no registered context containing a requested commit.

Never rewrite `.git/config`, remotes, branches, worktrees, or Git alternates.
Never make existing clones depend on Emacs cache files.

## 8. Preserve Phase 0-2 interaction and performance

All accepted guarantees remain mandatory:

- `C-x g` remains standard Magit status.
- `C-c g r/s/c/b` keeps the accepted semantics.
- `t`, `RET`, `o`, `e`, `SPC`, `gr`, and `q` keep their behavior.
- Ordinary navigation preserves the selected window/layout.
- Generated review buffers start in Evil normal state.
- Review checkmarks never mutate Git.
- UI chrome remains ASCII-safe with explicit four-space tree indentation.
- Open, registry lookup, `L`, `e`, tree, diff, and `gr` are offline.
- No Delta or Difftastic process starts automatically.
- The 100-file Changes Tree remains below one second warm median and retains a
  bounded Git process count.

Avoid rediscovering repository identity with many Git processes during every
file/tree render. Cache context identity safely and refresh it at explicit
repository/context boundaries. Do not cache worktree/index contents.

## Automated tests

Add `test/git-review-integration-test.el` and load it from
`test/git-review-test.el`. Extend Phase 2 tests only where target/state identity
integration requires it.

At minimum test:

1. SSH SCP, `ssh://`, HTTPS, and `git://` forms normalize to one canonical id.
2. Same owner/name on different hosts does not collide.
3. Nested forge group paths and meaningful ports remain distinct.
4. Malformed URLs and local/file remotes degrade safely.
5. Remote alias rename preserves identity.
6. Two independent temporary repositories configured with same-origin URLs
   share one canonical record but have different context ids, Git dirs, local
   branches, worktree targets, staged targets, and mutable buffers.
7. Two real linked Git worktrees share `git-common-dir` but have different
   roots, `git-dir`, branches, indexes, working changes, context ids, and local
   buffers.
8. Same commit review opened from two same-origin clones has canonical immutable
   identity and eligible shared-buffer reuse.
9. Same commit OID under different canonical repository ids does not collide.
10. Worktree/staged/branch reviewed state never leaks between contexts.
11. Commit reviewed state can be reused across same-origin contexts when path
    and fingerprint match.
12. Legacy Phase 2 state migrates once without deleting the legacy file.
13. Registry save is atomic; simulated write/rename failure preserves the prior
    valid generation.
14. Malformed registry persistence yields an empty usable registry and never
    evaluates arbitrary Lisp.
15. Deleted context roots prune/degrade safely; another live context can be
    selected for an immutable buffer.
16. `L` changes only active edit context on shared immutable reviews; it rejects
    worktree/staged/local-branch retargeting.
17. `e` opens the chosen live context's file and `q` returns correctly.
18. Public register/open/tree/diff/refresh/context-selection operations are
    instrumented with the Phase 0 hard network guard.
19. All existing 40 Phase 0-2 tests remain green.
20. Temporary repositories, clones, worktrees, registry files, buffers,
    windows, hooks, and advice are cleaned with `unwind-protect` after success
    or failure.

Use local temporary repositories and `git worktree`; do not clone or fetch from
the network. Same-origin behavior can be tested by configuring deterministic
fake fetch URLs on local repositories.

## Verification

Run and report:

```bash
rtk emacs --batch -Q -l init.el \
  --eval '(princ "CONFIG-LOAD-OK\n")'

rtk emacs --batch -Q -l init.el -L test \
  -l test/git-review-test.el -f ert-run-tests-batch-and-exit

rtk git diff --check -- .gitignore core init.el test

rtk rg -n "[[:blank:]]+$" core test git-phase-3-worker-prompt.md

rtk git status --short --branch
```

Also:

- perform byte-compilation diagnostics using temporary destinations outside
  the repository;
- report new warnings separately from known optional Forge/Difftastic warnings;
- report registry/context Git process counts for cached and refreshed paths;
- rerun the 100-file Changes Tree warm measurement with at least five samples;
- prove no fetch, remote update, HTTP(S), `gh`, Delta, or Difftastic executable
  was invoked.

## Owner manual gate

Provide exact terminal and graphical Emacs steps for this gate:

1. Open clone A and run a worktree review.
2. Open same-origin clone B with different local changes.
3. Show both share one canonical repository id.
4. Show their worktree/staged/branch targets and checkmarks remain separate.
5. Open the same immutable commit from both clones and confirm buffer reuse.
6. Use `L` to choose clone A or B as the edit context.
7. Press `e` and confirm the selected clone's source file opens.
8. Delete or temporarily move one test clone and confirm the other context
   remains usable.

## Non-goals and hard boundaries

Do not implement:

- a bare mirror or any directory named `mirror.git`;
- network fetch, pull, push, remote update, timers, async synchronization, `@`,
  repository sync, or sync-all;
- PR or issue lists, PR-number review, comments, approvals, checks, or merges;
- direct Forge SQLite reads, writes, migrations, or handwritten SQL;
- `gh` integration;
- automatic clone/worktree creation;
- Git alternates or remote rewrites;
- Phase 4 cache files or sync state;
- side-by-side source rendering;
- unrelated Magit, Evil, theme, Org, completion, or package cleanup.

Do not weaken the accepted Phase 2 tests to accommodate a new identity model.
Migrate behavior deliberately and strengthen assertions.

## Acceptance criteria

Phase 3 is ready for lead review only when:

1. One canonical record represents transport-equivalent hosted remotes.
2. Independent clones and linked worktrees remain explicit, separate local
   contexts.
3. Worktree, staged, and branch state cannot leak across contexts.
4. Immutable commit identity can be shared safely by canonical repository and
   OIDs.
5. Active edit context is explicit, selectable, local-only, and safe when a
   context disappears.
6. Existing reviewed state migrates without silent loss or deletion.
7. Registry persistence is thin, atomic, configurable, and malformed-safe.
8. All open/navigation/refresh/context operations remain offline.
9. All accepted Phase 0-2 UI, ASCII, Evil, window, and performance contracts
   remain green.
10. No Phase 4 or later behavior is introduced.

## Required handoff to the lead reviewer

Return a concise but complete report containing:

1. User-visible outcome and what `L` now does.
2. Exact files added, changed, or removed.
3. Repository/context struct fields and registry APIs.
4. Canonical URL normalization and remote-selection rules.
5. Independent clone versus linked-worktree identity behavior.
6. Review-target and state-key identity rules by scope.
7. Legacy state migration behavior.
8. Active edit-context selection and missing-context fallback.
9. Persistence format/location and atomic-write behavior.
10. Test commands, pass/fail totals, and named integration coverage.
11. Cached/refreshed process counts, 100-file timings, and offline evidence.
12. Known risks, optional Forge API use, or private Magit dependencies.
13. Exact owner terminal/GUI gate steps.
14. Suggested rollback limited to Phase 3 paths.
15. Confirmation that nothing was staged, committed, pushed, fetched, or
    installed.

Stop after Phase 3. Do not begin Phase 4.
