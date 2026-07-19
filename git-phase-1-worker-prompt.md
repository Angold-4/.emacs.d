# Delegation Prompt — Git Review Workbench Phase 1

You are the implementation worker for Phase 1 of a personal Emacs Git review
workbench. Implement **Phase 1 only** on the branch and pull request already
prepared by the lead. The lead agent will review your filesystem changes and
automated evidence; the owner will then test the approved behavior in a real
terminal Emacs session.

## Repository, branch, and instructions

- Repository: `/home/ango/.emacs.d`
- Required branch: `feat/git-review-workbench-phase-1`
- Pull request: `https://github.com/Angold-4/.emacs.d/pull/5`
- Phase 0 baseline commit: `c31e570`
- Read `/home/ango/.codex/RTK.md` completely before running commands.
- Prefix every shell command with `rtk`, as required by the repository.
- Inspect `rtk git status --short --branch` before editing. Stop and report if
  the branch differs or if unrelated uncommitted changes are present.
- Do not create or switch branches. Do not stage, commit, push, amend, rebase,
  reset, restore, clean, or modify the pull request. The lead owns Git history.

Read these files before editing:

1. `git-plan.md`, especially the invariants, delivery rules, and Phase 1.
2. `docs/git.md`, especially the target architecture and interaction model.
3. `docs/git-baseline.md`, including the owner terminal baseline.
4. `core/init-git.el`, `core/init-tools.el`, `core/init-evil.el`, and `init.el`.
5. `test/git-review-fixtures.el`, `test/git-review-baseline.el`, and
   `test/git-review-test.el`.

The Phase 0 files and planning documents are reviewed and committed. Preserve
them unless a test needs a tightly scoped Phase 1 extension. Do not rewrite the
plans to match an implementation shortcut.

## Product outcome for this phase

Make Magit and Forge-generated review views behave like ordinary Evil buffers:

```text
source buffer
  -> status
  -> revision or cached Forge topic
  -> unified diff
  -> writable worktree file
  -> q returns through the same caller chain
```

Normal navigation replaces the selected window. It must not unexpectedly add,
delete, or rearrange windows. An explicit other-window command may add or reuse
one right-hand window. Generated review buffers start in Evil normal state and
share one consistent navigation vocabulary.

Phase 0 measured this terminal behavior from an existing two-window layout:

```text
before status -> after status -> after unified diff
      2        ->       3      ->          4
```

The Phase 1 acceptance target is:

```text
      2        ->       2      ->          2
```

This phase changes navigation and interaction foundations. It does not yet
deliver the Changes Tree, final diff styling, shared PR database, or batch
synchronization.

## Scope: Phase 1 only

Expected paths include:

- `core/init-git.el`
- new `core/init-git-ui.el`
- `core/init-tools.el`
- `core/init-evil.el` if duplicate Magit bindings must be removed
- `init.el` for the new module load order
- focused ERT files under `test/`

Keep changes smaller where possible, but do not leave duplicate ownership just
to avoid touching an expected file. Preserve unrelated configuration.

### 1. Establish clear module ownership

Move the Magit package declaration, its `cond-let` dependency setup, Magit
commands/bindings, and all Magit settings out of `core/init-tools.el` and into
`core/init-git.el`. Remove duplicates instead of letting load order decide
which value wins.

Create `core/init-git-ui.el` and load it from `init.el` immediately after
`init-git`. Move review-specific UI concerns there:

- review minor mode and Evil keymaps;
- Magit display/quit behavior;
- diff faces;
- `diff-hl` integration;
- existing Delta and Difftastic display integration.

Do not install, update, fetch, rebuild, or repin packages. Use the existing
Straight checkouts and lockfile. Preserve Projectile, Treemacs, and unrelated
tool configuration in `init-tools.el`.

### 2. Same-window display by default

Install exactly one documented `magit-display-buffer-function` owned by
`init-git-ui.el`.

Default visits for status, log, cached Forge topic, revision, unified diff,
blob, and source/worktree navigation must replace the selected window. They
must preserve a pre-existing split layout and selected window. Do not solve
this with a broad `display-buffer-alist` rule that changes unrelated buffers.

Required behavior:

- `C-x g` opens Magit status in the selected window.
- The new `C-c g` dispatch can open status in the selected window.
- `RET` and `e` navigate in the selected window.
- Nested review navigation records enough caller state for `q` to return one
  level at a time.
- `q` eventually restores the original source buffer and original window
  layout, including a layout that was already split before review began.
- Normal navigation never creates a window.
- Explicit `o` and `|` may create one right-hand window and must reuse it on
  subsequent explicit other-window visits rather than accumulating splits.

Use window parameters, buffer history, or a small frame/window-scoped stack as
needed. Avoid one unscoped global window configuration that breaks nested
navigation or multiple frames. Do not kill user source buffers on return.

### 3. One Evil review mode

Implement `+git-review-buffer-mode` and enable it for Magit and Forge-generated
review buffers that are used for status, log, revision, diff, blob, topic, and
related review views. Generated buffers must enter Evil normal state, including
the first buffer created in a session.

Implement this normal-state vocabulary consistently:

| Key | Required action |
|---|---|
| `j` / `k` | move one visual line down/up |
| `J` / `K` | move eight visual lines down/up |
| `TAB` | toggle the section at point |
| `S-TAB` | cycle global section visibility |
| `RET` | native primary visit in the selected window |
| `e` | visit the writable worktree target in the selected window |
| `o` | explicit primary visit in the reusable right-hand window |
| `|` | explicit primary visit in the reusable right-hand window |
| `[f` / `]f` | previous/next file section |
| `[h` / `]h` | previous/next hunk section |
| `[c` / `]c` | previous/next commit section |
| `n` / `N` | Evil search next/previous, never section movement |
| `gr` | refresh the current local buffer only |
| `q` | return to the recorded caller/layout |

Retain normal Evil motions, search, visual selection, and yank behavior. Do not
turn normal-state printable keys into an inconsistent mix of Magit and Emacs
bindings outside this documented vocabulary. Remove redundant Magit bindings
from `core/init-evil.el` if the review mode now owns them.

Navigation helpers must use Magit sections or documented Magit APIs. Do not
scan rendered text to find the next file, hunk, or commit.

### 4. Replace the hand-written visit parser

Remove `+git/open-file-at-point` and its regular-expression parsing of rendered
diff text. Replace its behavior with native APIs such as:

- `magit-visit-thing` for the primary action;
- `magit-diff-visit-file` for the revision/blob target;
- `magit-diff-visit-worktree-file` for writable worktree editing;
- Magit section values and native visit functions for precise line mapping.

Use the APIs supported by the locally installed Magit revision; inspect that
checkout instead of guessing function signatures.

Required cases:

- added, removed, and context lines map to the correct target line;
- renamed files resolve the correct old/new path for the chosen target;
- a deleted file can still be visited as a read-only revision/blob target;
- `e` gives a clear user error when no writable worktree target exists;
- a normal writable target opens without forcing Insert state.

`RET` on a Forge PR or issue must visit the already cached Forge topic buffer.
Opening or visiting must not fetch refs or contact GitHub.

### 5. Remove hidden Forge networking from visits

Remove these old implementation paths from production configuration:

- `+forge/resolve-ref`;
- `+forge/visit-pullreq-around`;
- advice around `forge-visit-pullreq` that resolves/fetches PR refs;
- any equivalent open-time advice that invokes fetch, pull, `gh`, an HTTP URL,
  Delta, or Difftastic.

This phase does not replace that behavior with another automatic fetch. Remote
state is cached state until a later explicit synchronization phase. Do not
delete, migrate, copy, or directly manipulate `forge-database.sqlite`.

### 6. Replace the `C-c g` prefix with a small Transient

Define `+git-dispatch` using Transient and bind the command itself to `C-c g`.
Retain `C-x g` as direct `magit-status`.

The dispatch must expose at least the existing local entry points with clear,
stable labels:

- status;
- working-tree review;
- staged review;
- review one commit;
- compact log.

Do not add Phase 2 Changes Tree or later network synchronization commands as
fake placeholders. Invoking `C-c g` must not contact a remote.

## Automated tests

Extend the existing offline ERT suite. Prefer focused helper functions and
tests over assertions tied to exact rendered strings. Reuse Phase 0 fixtures
and its hard network/process guard where appropriate.

At minimum cover:

1. Magit has one configuration owner; obsolete duplicate definitions and visit
   advice are absent.
2. Relevant generated modes enable `+git-review-buffer-mode` and start in Evil
   normal state.
3. `n` and `N` resolve to Evil search next/previous in the review map.
4. Starting from two windows, source -> status -> revision/diff preserves the
   count `2 -> 2 -> 2` and leaves the unrelated window untouched.
5. Repeated explicit `o` or `|` visits create at most one additional window and
   reuse it.
6. Repeated `q` unwinds the caller chain and restores the original buffer,
   selected window, and window layout.
7. Native visits map added, removed, context, renamed, and deleted-file cases
   correctly, including a writable `e` target where one exists.
8. Open, visit, `gr`, and `C-c g` fail the test if they attempt fetch/pull/push,
   Forge pull, `gh`, HTTP(S), Delta, or Difftastic.
9. Existing Phase 0 fixture/baseline tests still pass.
10. Temporary buffers, advice, hooks, window layouts, and repositories are
    cleaned with `unwind-protect`, even after a failed assertion.

Tests must be deterministic, work with temporary paths containing spaces, use
local Git identity, and require no network or package changes. Do not weaken
the Phase 0 network guard or tests to make Phase 1 green.

## Verification

Run and report at least:

```bash
rtk emacs --batch -Q -l init.el --eval '(princ "CONFIG-LOAD-OK\n")'
rtk emacs --batch -Q -l init.el -L test \
  -l test/git-review-test.el -f ert-run-tests-batch-and-exit
rtk git diff --check -- core init.el test
rtk git status --short --branch
```

Also report a search demonstrating that the removed Forge visit advice, manual
diff parser, and duplicate `C-c g` prefix bindings are gone. If the overall
`rtk git diff --check` fails only on a pre-existing committed file outside your
scope, show both the overall failure and the clean scoped check; otherwise fix
your whitespace errors.

Do not claim interactive terminal behavior based only on batch tests. Provide
the owner with a short manual script for both one-window and pre-split layouts.

## Non-goals and hard boundaries

Do not implement any of the following in Phase 1:

- the Phase 2 Changes Tree or file checklist;
- the final red/green/white-on-black diff redesign;
- side-by-side Difftastic as a default view;
- PR/issue dashboards, review comments, approvals, merges, or CI actions;
- the shared repository identity/data layer;
- batch repository synchronization, timers, or background fetch;
- Forge database migration or direct SQL;
- package installation, update, rebuild, or network access;
- unrelated Emacs cleanup or broad keybinding redesign.

If a requirement conflicts with the locally installed Magit/Forge API, stop
and document the exact API evidence and smallest proposed adjustment. Do not
silently substitute text parsing, shell parsing, or hidden networking.

## Acceptance criteria

Phase 1 is ready for lead review only when:

1. Magit configuration has one clear owner and the new UI module loads cleanly.
2. Default review navigation preserves the selected window and existing layout.
3. Explicit other-window navigation adds at most one reusable right window.
4. `q` restores nested callers and the original layout.
5. Generated views consistently start in Evil normal state with the documented
   bindings, including Evil `n/N` search.
6. Native Magit visits replace the rendered-text parser and cover difficult
   path/line cases.
7. Opening, visiting, refreshing, and dispatching are offline operations.
8. Configuration load, all ERT tests, and the scoped diff check pass.
9. No Phase 2 or later behavior is introduced.

## Required handoff to the lead reviewer

Return a concise but complete report with:

1. Outcome and user-visible behavior.
2. Exact files added, changed, or removed.
3. Important design choices for display history, `q`, and explicit
   other-window reuse.
4. Old duplicate/parser/advice paths removed.
5. Test commands and complete pass/fail counts.
6. Automated evidence for `2 -> 2 -> 2`, Evil bindings, native line mapping,
   other-window reuse, return restoration, and no networking.
7. Assumptions, compatibility risks, or incomplete items.
8. Exact manual owner test steps in terminal Emacs.
9. Suggested rollback limited to Phase 1 paths.

Do not start Phase 2. Do not stage, commit, or push. Stop after the Phase 1
handoff so the lead can review the filesystem implementation before the owner
tests it interactively.
