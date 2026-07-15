# Git Review Workbench — Hands-on Delivery Plan

Status: ready for owner approval and phased implementation
Architecture: `docs/git.md`
Current user guide: `git.md`

## Outcome

Build one consistent Emacs interface for GitHub code review:

```text
Repository status
  -> PR overview
  -> Changes Tree
  -> unified file diff
  -> optional side-by-side detail
  -> real source or Markdown editor
```

Normal navigation replaces the selected buffer. `o` and `|` are the only
commands that intentionally create a second review window. Generated buffers
are read-only Evil normal-state buffers. Opening and `gr` are offline-only;
`@` is the explicit network boundary.

## Approved working defaults

- Unified black/red/green Magit diff is the default review surface.
- `|` opens side-by-side source detail; `D` remains optional Difftastic.
- The Changes Tree groups files by folder and uses `SPC` to mark reviewed.
  Review checkmarks never stage or modify Git state.
- Forge remains authoritative for PR, issue, and comment metadata.
- Remote data is shared by canonical repository identity. Worktree, index, and
  branch state remain local to each clone/worktree.
- Global sync uses an explicit allowlist of active repositories.
- Version one selects an existing clone/worktree for editing and never creates
  one automatically.

## Delivery rules

1. Implement one phase per branch, based on the last accepted phase.
2. Keep `C-x g` usable at every commit.
3. Do not delete or migrate `forge-database.sqlite`.
4. Do not rewrite clone remotes or configure Git alternates.
5. Do not add network access to buffer open, `RET`, `gr`, or navigation.
6. Prefer Magit/Forge public APIs. Any advice requires a written reason and a
   regression test.
7. Stop at every phase gate for owner use and review. Do not stack later work
   on an unaccepted interaction model.

Current repository note: `docs/git.md` already contains uncommitted planning
changes. Workers must preserve them. The planning files should be committed or
moved to a planning branch before Phase 0 begins.

## Target file ownership

| File | Responsibility |
|---|---|
| `core/init-git.el` | Magit package/configuration, top-level Transient |
| `core/init-git-ui.el` | display policy, Evil review mode, local review, Changes Tree, diff faces |
| `core/init-git-store.el` | canonical identity, local contexts, mirror, cache, synchronization |
| `core/init-forge.el` | Forge package/configuration, PR/issue buffers, comments, checks |
| `test/git-review-test.el` | fast ERT unit and buffer behavior tests |
| `test/git-review-integration-test.el` | temporary repositories, clones, worktrees, sync tests |
| `docs/git.md` | architecture and decisions |
| `git.md` | concise end-user workflow after implementation stabilizes |

`init.el` loads the modules in dependency order:

```text
init-git -> init-git-store -> init-git-ui -> init-forge
```

Modules not yet implemented may be omitted until their phase begins.

## Standard verification

Every phase runs at least:

```bash
rtk git diff --check
rtk emacs --batch -Q -l init.el --eval '(princ "CONFIG-LOAD-OK\n")'
rtk emacs --batch -Q -l init.el -L test \
  -l test/git-review-test.el -f ert-run-tests-batch-and-exit
```

Integration phases also load `test/git-review-integration-test.el`. Visual
phases require a manual terminal/tmux pass and one screenshot.

Each worker hands back:

- branch and commit list;
- files changed and design decisions;
- automated-test output;
- manual workflow results;
- before/after timings and external-process log;
- known limitations and rollback instructions.

## Phase 0 — Freeze decisions and establish a baseline

Branch: `chore/git-review-00-baseline`

Status: **COMPLETE — implementation reviewed and owner terminal baseline
recorded 2026-07-15.**

### Change

- Add `forge-database.sqlite*` to `.gitignore` without touching the database.
- Create `test/` and the initial ERT runner.
- Add helpers that create temporary small and large Git repositories during
  tests; do not commit generated repositories.
- Record Emacs, Git, Magit, Forge, Evil, `gh`, Delta, and Difftastic versions in
  `docs/git-baseline.md`.
- Instrument a manual run to record:
  - window count before/after `C-x g` and diff open;
  - warm status and diff render time;
  - `gr` refresh time;
  - external Git/Delta/Difftastic processes started during refresh.

### Test

- Configuration loads in batch mode.
- Test fixtures cover modified, staged, untracked, added, deleted, and renamed
  files.
- The baseline can be repeated without network access.

### Gate

The owner accepts the recorded baseline and the working defaults above. No
behavior has changed yet.

## Phase 1 — Fix buffer navigation and Evil behavior

Branch: `feat/git-review-01-buffer-foundation`

This phase fixes the most immediate pain: Magit behaves like the rest of the
buffer-based Emacs environment.

### Change

1. Move the Magit declaration and all Magit settings from `init-tools.el` into
   `init-git.el`; remove duplicate settings.
2. Create `init-git-ui.el` and move review keymaps, display rules, diff faces,
   diff-hl, and Difftastic integration there.
3. Install one `magit-display-buffer-function` implementation:
   - status, log, Forge topic, revision, unified diff, blob, and source
     navigation replace the selected window;
   - explicit `o` and `|` may create/reuse one right-hand window;
   - `q` restores the caller and its window layout.
4. Implement `+git-review-buffer-mode` for Magit/Forge generated buffers:

   | Key | Action |
   |---|---|
   | `j/k`, `J/K` | one-line/eight-line movement |
   | `TAB`, `S-TAB` | local/global section visibility |
   | `RET` | native primary visit |
   | `e` | visit writable worktree target |
   | `o` | explicit other-window visit |
   | `[f`/`]f`, `[h`/`]h`, `[c`/`]c` | file, hunk, commit movement |
   | `n/N` | retain Evil search next/previous |
   | `gr` | local refresh |
   | `q` | return to caller |

5. Replace `+git/open-file-at-point` and its diff-text parser with native
   `magit-visit-thing`, `magit-diff-visit-file`, and
   `magit-diff-visit-worktree-file` paths.
6. Remove `+forge/resolve-ref`, `+forge/visit-pullreq-around`, and the
   `forge-visit-pullreq` advice. `RET` on a PR opens the cached Forge topic.
7. Replace the old `C-c g` prefix bindings with a small `+git-dispatch`
   Transient. Retain `C-x g` as direct status.

### Automated test

- All generated review modes start in Evil normal state.
- `n/N` still invoke Evil search behavior.
- Navigation through source -> status -> revision -> diff does not increase
  window count.
- `o` creates at most one extra window and reuses it.
- `q` restores the previous buffer/window configuration.
- Native visiting reaches the correct line for added, removed, context,
  renamed, and deleted-file cases.
- Test instrumentation fails if open, visit, or `gr` invokes `git fetch`, Forge
  pull, Delta, or Difftastic.

### Manual test

From a real project inside tmux:

1. Open status with `C-x g` and `C-c g`.
2. Walk status, commits, diffs, and Forge topics using only normal-state keys.
3. Visit and edit a worktree line, then `q` back through the exact buffer chain.
4. Repeat in one-window and pre-split layouts.

### Gate

The owner confirms that Magit now feels like an ordinary Evil buffer. Window
placement and navigation must be accepted before visual or PR work begins.

## Phase 2 — Deliver local code review and Changes Tree

Branch: `feat/git-review-02-local-review`

This is the first daily-usable milestone. It must work without Forge, `gh`,
Delta, or Difftastic.

### Change: review targets

- Introduce one internal review-target value containing repository root, scope
  (`worktree`, `staged`, `commit`, or `branch`), base object, head object, and
  stable buffer identity.
- Make working-tree, staged, commit, and branch commands construct that value
  and reuse the corresponding buffer.
- Keep staging/unstaging/discard actions available only for local targets.

### Change: Changes Tree

- Add `+git-changes-tree-mode`, backed by `magit-section`.
- Obtain machine-readable file data from Git, never from rendered diff text:
  `git diff --name-status -z`, `git diff --numstat -z`, and
  `git ls-files --others --exclude-standard -z` for untracked files.
- Merge the results into folder and file nodes with status and line totals.
- Render collapsible folders, reviewed/partial/unreviewed checkboxes, file
  status, additions, and deletions.
- Bind `t` from any review buffer to the tree.
- In the tree: `SPC` toggles reviewed, `RET` opens unified file diff, `o` opens
  explicitly on the right, `|` opens comparison later, and `q` returns.
- Persist review state atomically under `.cache/git-review/state/`. Key state by
  repository, target type, base/head, path, and file-content fingerprint.
  Changed files become unreviewed; unchanged files retain progress.
- Render folders lazily. Opening the tree must not generate file diffs.

### Change: unified diff appearance

- Use native Magit rendering and refinement only.
- Apply theme-safe faces after theme load:
  - default/context: black background with readable white/gray text;
  - removed line: dark red background;
  - added line: dark green background;
  - refined region: brighter red/green;
  - file/hunk headings: clear but visually quieter than changed code.
- Ensure terminal truecolor and graphical Emacs are both readable.
- Remove `magit-delta` and its face-remapping advice after owner visual approval.
- Keep Difftastic only on explicit `D`.
- Replace the disabled hand-built untracked section with tested native sections
  or Git-produced diffs. Never create incomplete Magit section markers.

### Automated test

- Tree construction: nested folders, untracked, binary, rename/copy, delete,
  submodule, spaces/unicode in paths, empty directories, and 100+ files.
- Directory checkbox rollup and partial state.
- Review-state invalidation when one file changes between head revisions.
- Tree and diff buffers are reused, locally refreshed, and offline.
- Warm status and tree render under one second on the large fixture.
- Status and tree refresh start neither Delta nor Difftastic.

### Manual test

Use the workflow for real AI-generated changes:

```text
C-c g -> r -> t -> choose file -> RET -> e -> edit -> q -> SPC reviewed
```

Review, stage, refresh, and re-review at least one mixed change containing a new
file and rename. Test in terminal/tmux and graphical Emacs if available.

### Gate and feedback loop

The owner uses Phase 2 for normal development before Phase 3 starts. Record
friction in navigation, colors, tree density, checkbox behavior, and buffer
return paths; adjust Phase 2 until it is comfortable enough for daily use.

## Phase 3 — Canonical repositories and local contexts

Branch: `feat/git-review-03-repository-store`

### Change

- Create `init-git-store.el` with explicit structures for canonical repository,
  local context, cache generation, and active edit context.
- Prefer Forge's parsed host/owner/name identity for supported forges. Normalize
  transport-independent fetch URLs elsewhere.
- Treat SSH and HTTPS URLs for the same host/owner/repository as one identity.
- Distinguish independent clones by `git-dir` and linked worktrees by
  `git-common-dir`.
- Persist only the thin registry and review state; do not extend or query Forge
  SQLite with handwritten SQL.
- Move Changes Tree keys from local root identity to canonical identity when
  available.

### Test

- Two independent same-origin clones share one canonical record.
- Two linked worktrees share common Git storage but retain separate branches,
  indexes, working changes, and buffers.
- Same owner/name on different hosts does not collide.
- Remote rename and deleted local context degrade safely.

### Gate

Open the same repository from two local copies. Shared identity is visible, but
status, branch, staging, and working changes remain unquestionably local.

## Phase 4 — Shared mirror and explicit synchronization

Branch: `feat/git-review-04-sync`

### Change

- Create one bare mirror per canonical identity under
  `.cache/git-review/repositories/<hash>/mirror.git`.
- Seed a new mirror from an existing clone when possible, then fetch remote
  branches, tags, and PR refs only into the mirror.
- Implement a per-repository asynchronous sync state machine:

```text
queued -> fetching mirror -> pulling Forge -> fetching checks
       -> atomically publish generation -> refresh waiting buffers
```

- Coalesce duplicate in-process requests and use a filesystem lock across Emacs
  instances.
- `@`/`C-c g f` sync the current repository. `C-c g F` syncs only repositories
  in the configured active allowlist, with bounded concurrency.
- Preserve the last successful generation when any step fails. Show stale age
  and a compact error without breaking offline navigation.
- Copy PR objects from the mirror into a clone only after an explicit checkout
  or edit action.

### Test

- Two clones request sync concurrently and produce one remote fetch.
- A second Emacs process respects the mirror lock.
- Cancellation, offline failure, partial Forge/check failure, and interrupted
  writes preserve the last successful cache.
- After a successful sync, open and `gr` work with networking disabled.

### Gate

Process logs prove one fetch per canonical repository. The user can disconnect
from the network and continue reviewing all synchronized data.

## Phase 5 — Build the clean PR overview

Branch: `feat/git-review-05-pr-workspace`

### Change

- Create `init-forge.el` and move all Forge configuration out of the old Git
  module.
- Add one shared PR buffer keyed by canonical identity and PR number.
- Render cached `magit-section` sections in this order:

```text
PR title/state/base/head/review status
local edit context and cache age
Changes Tree progress
changed files
commits, oldest first
checks summary
description
conversation
```

- Use Forge objects/APIs for metadata and conversation; use mirror objects for
  ranges, blobs, and diffs.
- Reuse the Phase 2 Changes Tree for the PR merge-base/head range.
- `RET` opens the selected item in the same window. `t` opens the tree. `e`
  selects an existing local context before opening writable source. Reply/edit
  opens a writable Markdown composition buffer.
- Handle fork PRs and merge commits explicitly.

### Test

- Opening the same PR from two clones reuses one buffer.
- Opening, tree navigation, commit navigation, comments, and `gr` perform no
  network calls.
- PR range, oldest-first commit order, fork refs, rename/delete/binary files,
  and context selection are correct.
- A changed PR head invalidates only reviewed files whose blob changed.

### Gate and feedback loop

The owner reviews a real small PR and a large AI-generated PR entirely inside
Emacs. Adjust section order, density, keys, and Changes Tree behavior before
side-by-side work begins.

## Phase 6 — Add source-aware side-by-side detail

Branch: `feat/git-review-06-side-by-side`

### Change

- `|` opens before/after source or blob buffers in two ordinary same-frame
  windows with the existing vertical divider.
- Use the correct major mode and syntax highlighting on both sides.
- Use Ediff's mapping engine if helpful, but suppress its control frame and keep
  control inside the review buffers.
- Synchronize `[h`/`]h`, `[f`/`]f`, and scroll position.
- Apply dark-red/dark-green region and bright fine-change faces.
- Parse only the active file. Reuse both windows while stepping files.
- For local review, allow the after side to be the writable worktree file.
- Fall back clearly for binary, oversized, submodule, or unavailable blobs.
- `q` restores the exact pre-comparison layout.

### Test

- Added, deleted, renamed, copied, binary, and large files.
- Correct old/new line and blob selection.
- Repeated file stepping never creates a third review window.
- Normal file open is under 300 ms when objects are local.

### Gate

The owner approves terminal and graphical screenshots and successfully edits a
worktree file from the detailed view without losing the PR/tree position.

## Phase 7 — Complete issues, comments, and checks

Branch: `feat/git-review-07-github-actions`

### Change

- Add shared issue buffers using the same section and Evil vocabulary.
- Finish reply, edit, quote, and submit flows in Markdown composition buffers.
- During explicit sync, use a provider adapter to cache GitHub check rollups.
  GitHub is the first implementation; other providers show checks unavailable.
- Show cached check state in PR overview. Fetch failed logs only through a
  separate explicit command.
- Add explicit Forge commands to `+git-dispatch` without depending on Forge's
  broken default transient insertion target.

### Test

- Offline issue/PR conversation rendering.
- Markdown submit success, validation failure, cancellation, and draft recovery.
- Check cache success, stale state, API failure, and missing `gh`.
- No API request occurs outside explicit sync/log commands.

### Gate

The owner can review changes, inspect checks, and write a PR or issue comment
without leaving Emacs.

## Phase 8 — Harden, document, and remove legacy paths

Branch: `chore/git-review-08-hardening`

### Change

- Run the complete identity, clone/worktree, sync, buffer, diff, and display
  matrix from `docs/git.md`.
- Add `+git/doctor`, a read-only diagnostic for dependencies, authentication,
  canonical identity, local contexts, Forge registration, mirror health, locks,
  cache age, and active syncs.
- Remove dead functions, obsolete key documentation, disabled section code, and
  any temporary compatibility shims.
- Replace root `git.md` with the concise, tested user workflow. Keep
  `docs/git.md` as architecture and this file as delivery history.
- Record final timings against Phase 0; unchanged operations may not regress by
  more than 20% without owner approval.

### Gate

Fresh install, upgrade with an existing Forge database, terminal/tmux, GUI,
offline, large PR, and rollback tests all pass. The owner signs off on the daily
workflow and the old Git review implementation is no longer loaded.

## Project checkpoints

| Checkpoint | User-visible result | Stop condition |
|---|---|---|
| Phase 1 | Magit/Forge act like consistent Evil buffers | window or return behavior feels wrong |
| Phase 2 | local AI-change review plus Changes Tree | not comfortable for daily use |
| Phase 4 | one explicit, shared, offline-capable sync | duplicate fetch or cache corruption |
| Phase 5 | clean PR overview and file/commit review | PR structure or context is confusing |
| Phase 6 | polished side-by-side source detail | colors, speed, or window restoration fail |
| Phase 7 | complete GitHub review/comment/check loop | leaving Emacs is still required for core review |

The next implementation action is **Phase 1 only**. Its buffer/window gate is
measured against the Phase 0 terminal sequence `2 -> 3 -> 4`, with a target of
`2 -> 2 -> 2` for ordinary navigation.
