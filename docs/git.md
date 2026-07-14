# Local Git Review Workbench

Status: **Draft for review**  
Date: 2026-07-14  
Proposed implementation branch: `feat/local-git-review-workbench`

This document describes the current Git architecture in this Emacs
configuration and proposes a local-first architecture for reviewing branches,
commits, pull requests, issues, checks, and diffs without leaving Emacs.

No implementation should begin until the decisions in [Review checklist](#review-checklist)
have been reviewed.

## 1. Goals

The workbench should provide:

1. A consistent Emacs-buffer experience for every Git object:
   repository status, branch, commit, pull request, issue, and diff.
2. Consistent Evil behavior in generated review buffers:
   normal state for navigation and insert state when requested, with `TAB` and
   `RET` available in both states.
3. Fast local navigation after one explicit batch synchronization.
4. One remote fetch per canonical repository, even when the same repository is
   cloned into multiple directories.
5. A clear side-by-side before/after view with a vertical divider, dark
   background, readable source text, red removals, and green additions.
6. Commit-by-commit and file-by-file PR review using the same navigation model.
7. Offline access to previously synchronized PRs, issues, comments, commits,
   refs, and CI summaries.
8. Exact navigation from a review item to the corresponding source or Markdown
   composition buffer.

## 2. Core invariants

### 2.1 Everything is a buffer

The UI must use ordinary Emacs buffers, windows, keymaps, major/minor modes,
and `magit-section`. It must not introduce a browser-like web view.

Stable buffer names should make ownership clear:

```text
*git github.com/org/dragon · PR #42*
*git ~/Work/dragon · branch feature-x*
*git ~/Work2/dragon · branch feature-x*
*git github.com/org/dragon · commit a12bc34*
*git github.com/org/dragon · diff main...a12bc34*
*git github.com/org/dragon · issue #91*
```

Opening the same shared PR, issue, remote commit, or immutable diff again should
reuse its existing buffer. Branch and working-tree buffers must remain local to
their clone.

### 2.2 Network access is explicit

Opening a status, PR, issue, commit, or diff buffer must never fetch from a
remote. Only the synchronization commands `@` and `C-c g f` may contact the
network.

`gr` means "re-render from local Git objects and caches." It must remain fast
and useful while offline.

### 2.3 Shared remote data and local work state are different

Remote repository data is shared by canonical repository identity. Worktree,
index, branch, reflog, and uncommitted changes belong to a particular local Git
context and must never be shared accidentally.

### 2.4 Generated views are not source files

PR, issue, commit, status, and diff overview buffers are reconstructed from Git
objects or caches. They remain read-only because inserting arbitrary text into
them would not change Git or GitHub and would disappear on refresh.

Evil may still enter insert state in these buffers. Local insert-state bindings
make `TAB`, `S-TAB`, `RET`, and `ESC` work consistently; other editing attempts
receive the normal read-only error.

Real editing happens in ordinary writable buffers:

- `e` or `RET` on worktree code opens the real file at the exact line.
- Editing remote PR code first selects or creates a local worktree.
- Replying or commenting opens a writable `markdown-mode` composition buffer.
- Source buffers keep their normal insert behavior: `TAB` indents and `RET`
  inserts a newline.

## 3. Current architecture

### 3.1 Module loading

The current load order is:

```text
init.el
  ├── core/init-tools.el   installs and configures Magit
  └── core/init-git.el     adds the custom review workflow after init-tools
```

Git configuration is therefore split between two modules:

- `core/init-tools.el` owns the Magit package declaration and base settings.
- `core/init-git.el` repeats some Magit settings and contains all custom review,
  Forge, Difftastic, Delta, diff-hl, faces, and navigation code.

This makes ownership unclear and allows the same Magit variable to be assigned
in two locations.

### 3.2 Current components

| Component | Current responsibility |
|---|---|
| Magit | Status, staging, commits, logs, unified diffs, Git operations |
| Forge | PR/issue metadata, comments, Markdown, topic lists, SQLite cache |
| diff-hl | Live worktree indicators in file margins/fringes |
| Difftastic | Optional AST-aware full or per-file diff buffers |
| magit-delta | Optional syntax-colored diff washing; disabled by default |
| magit-todos | TODO/FIXME/HACK section in Magit status |
| Evil | Normal-state movement and custom review/status bindings |

### 3.3 Current entry points

```text
C-x g       magit-status
C-c g g     magit-status
C-c g r     working-tree review
C-c g s     staged review
C-c g c     select and review one commit
C-c g l     compact log
```

`C-c g` is currently a prefix, not a command that opens status by itself.

Review buffers currently add normal-state bindings for navigation, section
toggle, staging, refreshing, and opening a file. Insert-state bindings for
`TAB` and `RET` are not defined.

### 3.4 Current diff flow

Working-tree, staged, and commit review call Magit diff commands directly:

```text
+git/review          -> magit-diff-working-tree
+git/review-staged   -> magit-diff-staged
+git/review-commit   -> magit-diff-range
```

Files start collapsed. The current custom open-at-point function manually
parses diff headers and hunk line counts to decide which worktree file and line
to visit. Magit already has native blob/worktree visiting APIs, so this custom
parser is unnecessary and fragile around renames, deleted files, combined
diffs, and upstream format changes.

Difftastic is available through `D` for a whole diff and `d` for the file at
point. It is configured for side-by-side display and reuses an existing
Difftastic window asynchronously.

`magit-delta` is installed but intentionally not enabled globally because
running an external Delta process during Magit refresh caused multi-second
stalls. Additional advice attempts to reconcile Delta overlays with custom
Magit faces.

### 3.5 Current PR and issue flow

Forge adds PR and issue sections to `magit-status` and stores topic data in:

```text
~/.emacs.d/forge-database.sqlite
```

The current database file is not ignored by Git and appears as an untracked
file in this repository.

Forge itself identifies repositories using forge host, owner, and repository
name. This already allows one database record to represent the same remote
repository from different local directories.

The current custom advice changes `forge-visit-pullreq` when it is invoked from
Magit status:

1. Resolve or fetch the PR base branch in the current clone.
2. Resolve or fetch `refs/pull/N/head` into `refs/forge-review/N` in that clone.
3. Open a Magit range diff instead of the normal Forge PR topic page.

Consequences:

- Opening a PR can unexpectedly access the network.
- Every independent clone may fetch the same PR objects again.
- The normal cached description/comment page is bypassed from Magit status.
- PR behavior depends on the clone from which the PR was opened.

Forge disables its default binding injection in the current config because one
transient insertion target was incompatible with the installed Magit. As a
result there is no single explicit batch-sync key in Magit status, even though
Forge's pull commands remain callable interactively.

### 3.6 Current local repository identity

Each independent clone owns its own `.git` object database and refs:

```text
~/Work/dragon/.git
~/Work2/dragon/.git
```

There is no shared Git-object or PR-ref cache. Forge metadata is global, but Git
fetches still occur per clone. Forge also remembers only one convenient
worktree path on its repository object; that field is not sufficient to model
several independent clones and worktrees simultaneously.

### 3.7 Known mismatches and risks

1. Git configuration is concentrated in a large `core/init-git.el` and partly
   duplicated in `core/init-tools.el`.
2. Review keys are normal-state-only.
3. Generated PR review and cached Forge topic views are separate experiences.
4. PR open can perform an on-demand fetch.
5. Same-origin clones fetch identical remote objects independently.
6. File visiting relies on manual diff parsing.
7. Delta, Magit faces, and Difftastic provide overlapping rendering paths.
8. Root `git.md` says full untracked-file diffs are enabled, but the custom
   untracked-file section hook is currently disabled because it could create
   invalid Magit section markers.
9. `forge-database.sqlite` is not ignored.
10. CI/check state is not cached or rendered in the current PR flow.

## 4. Target architecture

### 4.1 Layers

```text
┌──────────────────────────────────────────────────────────────┐
│ Buffer UI                                                    │
│ Magit status · PR · issue · branch · commit · diff · source │
├──────────────────────────────────────────────────────────────┤
│ Consistent interaction                                      │
│ +git-review-buffer-mode · Evil maps · magit-section         │
├──────────────────────────────────────────────────────────────┤
│ Review/session model                                        │
│ range · commit list · file list · position · local context  │
├──────────────────────────────────────────────────────────────┤
│ Repository registry                                         │
│ canonical identity · local contexts · buffer registry       │
│ sync coalescing · cache generation                          │
├───────────────────────┬──────────────────────────────────────┤
│ Shared remote state   │ Per-local-context state              │
│ bare mirror           │ branch/index/worktree/reflog         │
│ Forge DB              │ uncommitted and local-only commits   │
│ checks cache          │ test/build commands                  │
└───────────────────────┴──────────────────────────────────────┘
```

### 4.2 Canonical repository identity

These URLs must resolve to the same identity:

```text
git@github.com:org/dragon.git
ssh://git@github.com/org/dragon.git
https://github.com/org/dragon.git
```

Canonical identity:

```text
github.com/org/dragon
```

GitHub Enterprise and other hosts retain their host component, so two
repositories with the same owner/name on different hosts never collide.

Use Forge's parsed repository identity when the remote belongs to a supported
forge. For other remotes, use a normalized transport-independent URL. Do not
key shared data by local path, remote alias (`origin`), or push URL.

### 4.3 Repository registry

Maintain one in-memory record per canonical repository:

```elisp
(repository-id
 :forge-repository
 :mirror-directory
 :local-contexts
 :shared-buffers
 :sync-process
 :waiting-buffers
 :cache-generation
 :last-success
 :last-error)
```

A local context records:

```elisp
(:root
 :git-dir
 :git-common-dir
 :current-branch
 :head
 :upstream)
```

Independent clones have different Git directories. Git worktrees can have
different `git-dir` values but share one `git-common-dir`.

### 4.4 Shared bare mirror

Create one bare mirror per canonical repository:

```text
~/.emacs.d/.cache/git-review/repositories/<identity-hash>/
  mirror.git/
  repository.json
  checks.json
  sync-state.json
```

The mirror stores remote branches, tags, PR head refs, commits, trees, and
blobs. It is used for immutable remote and PR review.

The mirror may be seeded from the first available local clone to avoid an
unnecessary full download. Network synchronization updates only the mirror.

Do not modify existing clones to use Git alternates; a clone must not silently
depend on the continued existence of the Emacs cache. If a clone needs PR
objects for checkout or editing, fetch them from the mirror over the local
filesystem.

### 4.5 Data ownership

| Data | Key | Storage |
|---|---|---|
| PRs, issues, comments | canonical repository | Forge SQLite database |
| CI/check rollups | canonical repository + PR number | shared JSON cache |
| Remote branches/tags/PR refs | canonical repository | shared bare mirror |
| Immutable commit/diff buffers | repository + object IDs/range | shared buffer registry |
| Branch buffer | repository + local Git context + branch/HEAD | local buffer |
| Worktree/index diff | local Git context | local buffer |
| Uncommitted changes | local Git context | local `.git` and files |
| Active edit destination | shared PR buffer + selected context | buffer-local selection |

### 4.6 Consistent review-buffer mode

`+git-review-buffer-mode` is a minor mode enabled in generated Magit, Forge,
PR, issue, commit, and diff review buffers.

It provides one Evil interaction vocabulary:

| Key | Action |
|---|---|
| `TAB` | Toggle section at point |
| `S-TAB` | Cycle visibility of all sections |
| `RET` | Expand a container or visit its primary target |
| `n` / `p` | Next/previous item at the current review level |
| `N` / `P` | Next/previous hunk or difference |
| `] f` / `[ f` | Next/previous changed file |
| `] c` / `[ c` | Next/previous commit |
| `e` | Open the writable source/comment target |
| `L` | Select active local context |
| `gr` | Refresh only from local state |
| `@` | Synchronize shared remote state |
| `/` | Search current buffer |
| `q` | Bury buffer and return to caller |

`TAB`, `S-TAB`, and `RET` receive local bindings in both Evil normal and insert
states. Generated review buffers start in normal state.

### 4.7 Git dispatch

Replace the passive `C-c g` prefix with a Transient command that opens or
selects Magit status and then presents:

```text
g  status
r  working-tree review
s  staged review
c  commit review
b  branch review
p  pull-request list/review
i  issue list
f  synchronize repository
```

Keep `C-x g` as direct, standard `magit-status`.

### 4.8 Status and branch buffer

Do not replace Magit's default status section hook. Add a small section using
`magit-add-section-hook`:

```text
Current branch review
  Local context: ~/Work/dragon
  Associated PR: #42
  Base/upstream: origin/main
  Ahead/behind: 5/0
  Cached checks: 12 pass, 1 pending
  Commits since merge-base
```

The remainder of status stays native Magit: staged, unstaged, untracked,
unpushed, recent commits, PRs, and issues.

A branch buffer is tied to its local context because two same-origin clones can
have different branches or different commits under the same branch name.

### 4.9 PR buffer

A PR buffer is shared by canonical repository and PR number:

```text
PR #42 — Improve transaction execution
OPEN · review required · main <- feature/transactions
Repository: github.com/org/dragon
Edit context: ~/Work/dragon                       [L to switch]
Cache: synced 3 minutes ago

Changes (14 files, +381 -97)
Commits (5)
Checks
Description
Conversation
```

All sections are collapsible. `RET` behaves according to the row:

- Changed file: open its unified diff or side-by-side review.
- Commit: open its commit buffer and diff.
- Check: show cached details; an explicit action may fetch logs.
- Comment: expand thread or open the Forge action.
- Reply/edit: open a writable Markdown composition buffer.

The active local context affects editing, testing, and worktree creation. It
does not affect the cached PR content or immutable diff.

### 4.10 Commit and diff buffers

A commit buffer contains metadata, message, parent selection, changed files,
and a unified Magit diff. PR commits appear oldest-to-newest for review.

Range semantics:

- Whole PR: merge-base of base/head versus PR head.
- One commit: selected parent versus commit.
- Merge commit: first parent by default, with an explicit parent selector.
- Working tree: `HEAD` versus files.
- Staged: `HEAD` versus index.

New, deleted, renamed, copied, submodule, and binary files require explicit
handling. No feature may infer file/line identity by parsing display text.

### 4.11 Side-by-side source review

The primary detailed view uses ordinary source/blob buffers displayed left and
right with a vertical divider. Built-in Ediff may provide difference mapping,
but it must use plain same-frame windows and no separate control frame.

```text
BEFORE: base:path/to/file       │ AFTER: head:path/to/file
───────────────────────────────┼────────────────────────────────
unchanged source               │ unchanged source
dark-red removed expression    │ dark-green replacement
more source                    │ more source
```

Requirements:

- Correct major mode and syntax highlighting on both sides.
- Synchronized difference navigation.
- Dark red background for removed regions.
- Dark green background for added regions.
- Brighter fine-grained token changes.
- Context remains readable on black.
- `|` toggles side-by-side/stacked layout.
- Difftastic remains an optional per-file structural view on `D`.
- Oversized or binary files fall back cleanly instead of blocking redisplay.

For worktree review, the after buffer can be the real editable file. For a
remote PR, both revisions are immutable until the user selects or creates a
local edit context.

### 4.12 Batch synchronization

`@` and `C-c g f` start one asynchronous synchronization per canonical
repository:

```text
resolve canonical identity
  -> join existing sync if one is running
  -> update shared bare mirror and PR refs
  -> forge-pull PRs/issues/comments
  -> fetch one CI/check rollup snapshot for open PRs
  -> atomically update cache generation and timestamp
  -> refresh every waiting buffer across every local context
```

If `~/Work/dragon` starts a sync and `~/Work2/dragon` requests one while it is
running, the second request joins the first job. GitHub is contacted once.

Use an in-process job registry and a filesystem lock so two Emacs instances do
not update the same mirror concurrently.

Failure requirements:

- Keep the last successful cache intact.
- Mark buffers stale and show the error summary.
- Allow all local/offline navigation to continue.
- Never replace valid cached data with partial output.
- Never block Emacs redisplay while waiting for the network.

## 5. Proposed module ownership

```text
core/init-git.el
  Magit package configuration, faces, dispatch, top-level integration

core/init-git-repository.el
  Canonical identity, local-context registry, bare mirrors, sync coalescing

core/init-git-review.el
  Review-session model, branch/commit/diff buffers, source navigation

core/init-forge.el
  Forge configuration, PR/issue buffers, checks cache, topic actions

core/init-git-keys.el
  +git-review-buffer-mode and Evil normal/insert bindings

docs/git.md
  Architecture, plan, decisions, workflow, and acceptance criteria
```

If five modules prove unnecessarily granular during implementation,
`init-git-keys.el` may be folded into `init-git-review.el`. Repository identity
and synchronization should remain isolated because they have the strongest
correctness and test requirements.

## 6. Implementation plan

All implementation occurs on `feat/local-git-review-workbench`, created from a
reviewed, clean `main`. Preserve the existing Forge database.

### Phase 0: Baseline and hygiene

- Add `forge-database.sqlite*` to `.gitignore`.
- Confirm no tracked work is overwritten.
- Add a repeatable performance fixture.
- Record warm status, cached topic, unified diff, and Difftastic timings.
- Document installed Magit, Forge, Git, Emacs, `gh`, and Difftastic versions.

Commit: `chore(git): ignore local review state and add baselines`

### Phase 1: Consolidate the current configuration

- Move review-specific ownership out of `init-tools.el`.
- Remove duplicate Magit variable assignments.
- Split the large `init-git.el` by the ownership described above.
- Preserve current user-visible behavior during this phase.
- Add batch-load and byte-compilation checks.

Commit: `refactor(git): separate review and repository concerns`

### Phase 2: Consistent buffer interaction

- Implement `+git-review-buffer-mode`.
- Set generated review buffers to initial Evil normal state.
- Bind `TAB`, `S-TAB`, and `RET` in normal and insert states.
- Add stable buffer identity/reuse.
- Replace manual file/hunk display parsing with native Magit APIs.
- Add writable source and Markdown composition transitions.

Commit: `feat(git): unify review buffer interaction`

### Phase 3: Repository identity and local contexts

- Normalize forge and non-forge remotes.
- Discover independent clones and Git worktrees using Git directory/common
  directory identity.
- Register multiple local contexts under one canonical repository.
- Add active-context selection to shared buffers.
- Test SSH/HTTPS equivalence and cross-host separation.

Commit: `feat(git): add canonical repository registry`

### Phase 4: Shared mirror and deduplicated sync

- Seed and maintain one bare mirror per canonical repository.
- Fetch remote branches/tags/PR refs only into the mirror.
- Coalesce concurrent sync requests.
- Add filesystem locking and atomic cache updates.
- Fetch clone-local PR objects from the mirror when explicitly requested.
- Remove on-open PR fetches and the `forge-visit-pullreq` advice.

Commit: `feat(git): share remote objects across local clones`

### Phase 5: Branch, commit, and diff review

- Add current-branch review section without replacing Magit defaults.
- Implement branch and commit buffers with stable identity.
- Implement PR and per-commit ranges.
- Implement consistent file/commit/hunk navigation.
- Handle new/deleted/renamed/copied/binary/submodule cases.
- Add exact blob/worktree visiting.

Commit: `feat(git): add branch and commit review sessions`

### Phase 6: Side-by-side review

- Configure same-frame, plain-window Ediff/source comparison.
- Apply dark red/green and fine-difference faces after each theme change.
- Support file and commit stepping without rebuilding unrelated buffers.
- Preserve/restore the caller's window configuration.
- Keep Difftastic as the optional structural view.
- Remove magit-delta and its face-remapping advice after equivalence testing.

Commit: `feat(git): add source-aware side-by-side review`

### Phase 7: PR, issue, comments, and CI

- Restore normal cached PR/topic opening from status.
- Build the shared PR workspace and section order.
- Render Forge Markdown and cached conversations.
- Add async CI rollup snapshot during explicit sync.
- Add cached check details and explicit failed-log retrieval.
- Keep comment/reply/edit actions inside Emacs Markdown buffers.
- Bind Forge dispatch explicitly without relying on its failing transient
  insertion point.

Commit: `feat(forge): add local PR workspace and checks`

### Phase 8: Verification and documentation

- Add ERT integration fixtures for multiple clones and worktrees.
- Measure the performance acceptance criteria.
- Rewrite the old root `git.md` as a concise user guide or replace it with a
  link to this document after review.
- Add `+git/doctor` to report dependencies, authentication, repository
  identity, Forge registration, refspecs, mirror state, and cache age without
  modifying anything.

Commit: `test(git): verify local-first review workflow`  
Commit: `docs(git): document review workbench usage`

## 7. Test matrix

### 7.1 Repository identity

- Two independent clones with the same basename under different parents.
- Two Git worktrees with one shared common Git directory.
- SSH and HTTPS URLs for the same GitHub repository.
- Same owner/name on two different hosts.
- Remote URL change or repository rename.
- Clone removed while it is the active PR edit context.

### 7.2 Shared versus local state

- One shared PR/issue buffer across two clones.
- Distinct branch and working-tree buffers across those clones.
- Same branch name pointing at different commits.
- Different staged and unstaged changes in each clone.
- Local-only commit available in one clone but not the mirror.
- Context switch followed by source editing and test execution.

### 7.3 Synchronization

- Two contexts request sync concurrently; only one network fetch occurs.
- Second Emacs instance encounters the mirror lock.
- Offline open after successful sync.
- Git fetch succeeds but Forge pull fails.
- Forge succeeds but CI request fails.
- Process interruption preserves the last successful generation.
- New PR appears after one explicit sync and opens without another fetch.

### 7.4 Buffer and Evil behavior

- `TAB`, `S-TAB`, and `RET` in normal state.
- `TAB`, `S-TAB`, and `RET` in insert state in generated review buffers.
- Source buffers retain indentation/newline behavior in insert state.
- Stable buffers are reused rather than duplicated.
- `q` returns to the correct caller and restores windows.
- Search, copy, file stepping, commit stepping, and hunk stepping.

### 7.5 Diff correctness

- Working tree, index, single commit, merge commit, branch, and full PR.
- New, deleted, renamed, copied, binary, and submodule changes.
- Fork PR with head repository different from base repository.
- Exact source line on both old and new sides.
- Large source file and PR with at least 100 changed files.

### 7.6 Display environments

- Graphical Emacs.
- Terminal Emacs inside tmux with truecolor.
- Dark theme.
- Light theme with readable fallback faces.
- Narrow frame and wide side-by-side frame.

## 8. Performance acceptance criteria

1. Opening an already-created PR/issue/commit buffer is effectively immediate
   and performs no process or network call.
2. Warm Magit status and a cached PR workspace render in under one second on
   the large fixture.
3. Moving to the next normal-sized source file completes in under 300 ms after
   its Git objects are local.
4. Status refresh invokes neither Delta nor Difftastic.
5. Side-by-side comparison parses only the active file, not the entire PR.
6. Remote synchronization is asynchronous and never blocks redisplay.
7. A second same-repository sync request joins the first job.
8. Large/binary limits produce a visible fallback instead of a frozen buffer.

The worker must report before/after timings in the branch description. If a
fixed threshold is unreliable across machines, the implementation must at
least show no regression greater than 20% from the recorded baseline for
unchanged operations.

## 9. Migration and rollback

- Do not delete or reset `forge-database.sqlite`.
- Do not rewrite existing clones' remotes or Git alternates.
- Keep `C-x g` operational throughout the migration.
- Land phases as separate commits so the shared mirror, PR workspace, or
  side-by-side renderer can be reverted independently.
- Retain the current Difftastic path until the new detailed view passes its
  correctness and performance tests.
- Do not remove the old root `git.md` until this document is approved.

## 10. Non-goals for the first implementation

- Reimplementing Forge's remote API client.
- Reimplementing Git storage or replacing Magit status.
- Editing arbitrary characters directly inside generated diff text.
- Automatically contacting the network on a timer or buffer open.
- Automatically changing existing clones to share Git object alternates.
- Depending on a young all-in-one PR package for the core architecture.
- Reproducing every GitHub web feature before the local review path is fast and
  reliable.

## 11. Review checklist

Please review these decisions before implementation:

- [ ] Generated review buffers may enter Evil insert state, but remain
      read-only; code/comment editing opens the real writable buffer.
- [ ] `TAB`, `S-TAB`, and `RET` have review actions in both normal and insert
      states only in generated review buffers.
- [ ] `C-c g` opens Magit status and a Git Transient; `C-x g` remains direct
      Magit status.
- [ ] PRs/issues/remote commits are shared by canonical repository identity;
      branches, index, and worktree diffs remain local-context-specific.
- [ ] A central bare mirror is the only network fetch target for remote Git
      objects and PR refs.
- [ ] Existing clones are not rewritten to depend on the mirror.
- [ ] Opening or locally refreshing a buffer never contacts the network.
- [ ] Forge remains the authoritative PR/issue/comment database.
- [ ] GitHub CLI JSON supplies the cached CI/check adapter.
- [ ] Side-by-side source review is primary; Difftastic remains optional.
- [ ] `magit-delta` is removed after the replacement passes verification.
- [ ] The implementation waits for approval and occurs only on
      `feat/local-git-review-workbench` off `main`.

## 12. Expected result

After one explicit synchronization, the workflow is:

1. Press `C-c g` from any local clone.
2. See local branch/worktree state plus shared cached PRs and issues.
3. Open the same PR instantly from any clone without another remote fetch.
4. Read its changes, commits, checks, Markdown, and comments as Emacs sections.
5. Move file-by-file or commit-by-commit with consistent Evil keys.
6. Open a red/green side-by-side source comparison.
7. Select a local context and edit the real source in an ordinary buffer.
8. Write replies or comments in an ordinary Markdown buffer.
9. Return to the review buffer and refresh locally.
10. Press `@` only when new remote data is wanted.

The result remains recognizably Emacs and Magit: buffers, sections, modes,
keys, source files, and local data rather than a browser embedded in the
editor.
