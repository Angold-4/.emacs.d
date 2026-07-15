# Local Git Review Workbench

Status: **Refined product and implementation plan**
Date: 2026-07-15
Implementation status: **Not started; owner decisions required**

This document describes the current Git architecture in this Emacs
configuration and proposes a local-first architecture for reviewing branches,
commits, pull requests, issues, checks, and diffs without leaving Emacs.

The delivery order is intentionally local-first. The first usable milestone fixes
Magit windowing, Evil navigation, native file visiting, and unified diff rendering.
The shared-origin PR/issue system follows only after that daily review loop is
stable. No implementation should begin until the decisions in
[Owner decision gate](#11-owner-decision-gate) have been reviewed.

## 1. Goals

The workbench should provide:

1. A consistent Emacs-buffer experience for every Git object:
   repository status, branch, commit, pull request, issue, and diff.
2. Consistent Evil behavior in generated review buffers: normal state is the
   complete navigation and action interface; editing transitions to a real
   writable source or Markdown buffer.
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
9. A useful local working-tree, staged, and commit-review workflow before any
   custom remote cache or PR workspace is introduced.
10. A collapsible folder tree of all changed files with persistent, local
    reviewed/unreviewed checkmarks.

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

### 2.2 Navigation replaces the selected buffer

Opening status, logs, commits, Forge topics, and unified diffs replaces the
buffer in the selected window. It does not create a new window. This is the
default navigation path, equivalent to following a link inside one Vim window.

Creating another window is always explicit:

- `o` opens the target in a right-hand window.
- `|` opens or toggles the two-window detailed comparison.
- process buffers may use a non-selected utility window only when requested or
  when an operation fails.

`q` buries the generated buffer and restores its caller when possible. Reopening
an existing review buffer reuses it instead of creating another buffer or
window.

### 2.3 Network access is explicit

Opening a status, PR, issue, commit, or diff buffer must never fetch from a
remote. Only the synchronization commands `@`, `C-c g f`, and `C-c g F` may
contact the network.

`gr` means "re-render from local Git objects and caches." It must remain fast
and useful while offline.

### 2.4 Shared remote data and local work state are different

Remote repository data is shared by canonical repository identity. Worktree,
index, branch, reflog, and uncommitted changes belong to a particular local Git
context and must never be shared accidentally.

### 2.5 Generated views are not source files

PR, issue, commit, status, and diff overview buffers are reconstructed from Git
objects or caches. They remain read-only because inserting arbitrary text into
them would not change Git or GitHub and would disappear on refresh.

Generated review buffers start and remain in Evil normal state. Entering insert
state in a read-only rendering is misleading because typed text cannot have a
durable meaning. Commands such as `e`, `c`, and `r` transition to an appropriate
writable buffer, which starts in insert state when composition is the immediate
task.

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

The current database file is not ignored by Git. It is absent in the audited
worktree, but would appear as an untracked file as soon as Forge creates it.

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
11. Magit's default display function intentionally opens status and diff buffers
    in other windows; no project-level buffer/window policy overrides it.
12. The custom `n`/`p` and `N`/`P` bindings replace standard Vim search
    navigation with section navigation.
13. `+git/next-file` and `+git/prev-file` move between section siblings, so their
    meaning changes depending on whether point is in a file, hunk, or child
    section.

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

### 4.6 Buffer and window contract

The workbench has two display actions. Every command must choose one explicitly:

| Action | Used for | Window behavior |
|---|---|---|
| Navigate | status, log, topic, commit, unified diff, source/blob | replace selected window |
| Compare | explicit `o` or `|`, before/after source | create/reuse one right-hand window |

The Magit integration installs a project display function through
`magit-display-buffer-function`. It displays interactive Magit and Forge
navigation buffers in the selected window, including `magit-status-mode`,
`magit-diff-mode`, and `magit-revision-mode`. It must not advise individual
opening commands to repair window placement.

The contract is tested as a state transition:

```text
source A --C-c g--> status --RET--> commit/diff --q--> status --q--> source A
```

No transition above increases the window count. The explicit comparison path
may increase it by one, reuses that window while stepping files, and restores
the original layout on quit.

### 4.7 Consistent review-buffer mode

`+git-review-buffer-mode` is a minor mode enabled in generated Magit, Forge,
PR, issue, commit, and diff review buffers.

It provides one Evil normal-state interaction vocabulary:

| Key | Action |
|---|---|
| `TAB` | Toggle section at point |
| `S-TAB` | Cycle visibility of all sections |
| `RET` | Expand a container or visit its primary target |
| `j` / `k` | Move one display line |
| `J` / `K` | Move eight display lines |
| `] f` / `[ f` | Next/previous changed file |
| `] h` / `[ h` | Next/previous hunk or difference |
| `] c` / `[ c` | Next/previous commit |
| `n` / `N` | Next/previous Vim search result |
| `e` | Open the writable source/comment target |
| `o` | Open the primary target in a right-hand window |
| `|` | Toggle side-by-side/stacked detailed comparison |
| `L` | Select active local context |
| `gr` | Refresh only from local state |
| `@` | Synchronize shared remote state |
| `/` | Search current buffer |
| `q` | Bury buffer and return to caller |

Generated review buffers start in normal state. They do not redefine insert
state because they are read-only. Source buffers keep normal Evil behavior, and
Markdown composition buffers may start in insert state.

### 4.8 Git dispatch

Replace the passive `C-c g` prefix with one command that opens or selects Magit
status in the current window and then presents a small Transient:

```text
g  status
r  working-tree review
s  staged review
c  commit review
b  branch review
p  pull-request list/review
i  issue list
f  synchronize repository
F  synchronize all registered repositories
```

Keep `C-x g` as direct, standard `magit-status`.

### 4.9 Status and branch buffer

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

### 4.10 PR buffer

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

### 4.11 Commit and diff buffers

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

### 4.12 Changes tree

Every working-tree, staged, commit, branch, and PR review provides a separate
Changes Tree buffer. It groups changed files by directory and shows review
progress without mixing that progress with Git staging state:

```text
Changes  12/18 reviewed  ·  +842 -193

[✓] src/
    [✓] api/
        [✓] users.ts                 M   +24  -3
        [✓] sessions.ts              A   +91  -0
    [✓] domain/
        [✓] account.ts               R   +12  -8
[ ] tests/
    [✓] api/
        [✓] users.test.ts            M   +44 -12
    [ ] integration/
        [ ] login.test.ts            M  +103 -19
[ ] README.md                        M    +7  -2
```

Requirements:

- Directories are collapsible `magit-section` sections with aggregate file and
  line counts.
- Files show reviewed state, Git status (`A`, `M`, `D`, `R`, `C`, binary, or
  submodule), additions, and deletions.
- `SPC` toggles the reviewed checkbox at point. Toggling a directory applies to
  its descendants; a directory displays partial state when only some children
  are reviewed.
- `RET` opens the file's unified diff in the current window; `o` opens it in the
  explicit right-hand window; `|` opens detailed side-by-side comparison.
- `TAB` expands/collapses a directory, and `[ f` / `] f` move between files.
- `/` searches paths, and an optional filter shows only unreviewed files.
- Review checkmarks are local metadata. They never stage, unstage, discard, or
  submit a GitHub review.
- Progress is keyed by repository, review target, base object, and head object.
  When the head changes, files whose content changed become unreviewed while
  unchanged files retain their state.
- Large trees render lazily and do not generate diffs until a file is opened.

The tree is available from a review buffer with `t`, and `q` returns to that
review buffer without changing the window count.

### 4.13 Diff presentation and side-by-side source review

The unified Magit diff is the overview and default reading surface. It renders
in one buffer with a pure black default background, white readable context,
dark-red removed lines, dark-green added lines, and brighter refined changes.
It never invokes Delta or Difftastic during render or refresh.

The explicit detailed view uses ordinary source/blob buffers displayed left and
right with a vertical divider. This preserves a fast one-buffer overview while
providing exact source-aware comparison only for the active file. Built-in
Ediff may provide difference mapping, but it must use plain same-frame windows
and no separate control frame.

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

### 4.14 Batch synchronization

`@` and `C-c g f` start one asynchronous synchronization for the current
canonical repository. `C-c g F` starts the same operation for every registered
repository with a bounded concurrency limit; duplicate identities still
coalesce to one job:

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
  Package declarations, top-level dispatch, module loading

core/init-git-ui.el
  Display contract, review mode, Evil maps, faces, local/commit review

core/init-git-store.el
  Canonical identity, local contexts, mirror, cache, sync coalescing

core/init-forge.el
  Forge configuration, PR/issue buffers, checks cache, topic actions

docs/git.md
  Architecture, plan, decisions, workflow, and acceptance criteria
```

The four-module boundary is deliberate. Keys and window behavior belong to the
UI contract and should not become a separate grab-bag. Repository identity and
synchronization remain isolated because they have the strongest correctness and
concurrency requirements. `init-tools.el` no longer owns Git configuration.

## 6. Implementation plan

The work is delivered as sequential, reviewable phase branches. Each branch is
based on the last accepted phase, and each phase must leave `C-x g` usable. A
phase is not accepted on code inspection alone: its automated checks, manual
workflow script, and before/after timings must pass.

### Phase 0: Decision lock, baseline, and hygiene

- Add `forge-database.sqlite*` to `.gitignore`.
- Preserve the Forge database and confirm no tracked work is overwritten.
- Record the audited package revisions (Emacs 30.2, Magit `1288f65`, Forge
  `9628f76`, Evil `729d9a5`, Evil Collection `cd6cdf3`).
- Add fixtures for a small repository and a large AI-style change set.
- Record window count, warm status time, unified diff time, and refresh time.
- Turn the decisions in Section 11 into checked acceptance assumptions.

Commit: `chore(git): ignore local review state and add baselines`

Gate: the baseline is reproducible and the owner has approved Section 11.

### Phase 1: Daily-flow repair — buffers, Evil, and native visits

- Move all Git ownership out of `init-tools.el` and remove duplicate settings.
- Install the same-window navigation contract through
  `magit-display-buffer-function`.
- Implement `+git-review-buffer-mode` with one normal-state keymap and stable
  `j/k`, `J/K`, search, section, file, hunk, and quit behavior.
- Replace manual diff-header and line-count parsing with
  `magit-diff-visit-file` and `magit-diff-visit-worktree-file`.
- Remove the on-open network fetch and `forge-visit-pullreq` advice. Restore the
  normal cached Forge topic buffer.
- Add batch-load, byte-compilation, ERT keymap tests, and window-transition tests.

Commit: `feat(git): enforce buffer-first review navigation`

Gate: status, log, commit, Forge topic, and unified diff navigation never opens
an extra window; `q` returns correctly; no open or `gr` action contacts the
network.

### Phase 2: Daily-flow review — local changes and visual diff

- Make working-tree, staged, commit, and branch review consistent vertical
  slices using native Magit sections.
- Implement the reusable Changes Tree for local, staged, branch, and commit
  ranges, including reviewed checkmarks and exact diff opening.
- Render the unified overview on black with white context, dark-red removals,
  dark-green additions, and bright refined changes in GUI and terminal Emacs.
- Ensure refresh invokes neither Delta nor Difftastic. Remove `magit-delta` and
  its face-remapping advice after visual equivalence is accepted.
- Handle untracked files using valid native sections or a tested Git-produced
  diff; do not synthesize incomplete Magit section objects.
- Give new, deleted, renamed, copied, binary, and submodule entries explicit
  behavior and fallback messages.
- Add stable buffer identity for local status/diff/commit views.

Commit: `feat(git): deliver local code review workflow`

Gate: the owner can perform the full daily loop—review, visit, edit, return,
stage, refresh, and re-review—without PR/cache features. This is the first
production-usable milestone.

### Phase 3: Canonical repository identity and storage

- Use Forge's repository identity for supported forges and normalized fetch URLs
  elsewhere; never use a push URL or local basename.
- Discover independent clones and linked worktrees through `git-dir` and
  `git-common-dir`.
- Register several local contexts under one canonical repository identity.
- Add active-context selection to shared buffers.
- Keep Forge SQLite authoritative for PRs, issues, and comments. Add only a thin
  review registry/cache keyed by the same canonical identity; do not modify
  Forge's schema.
- Test SSH/HTTPS equivalence, cross-host separation, rename, fork, and deleted
  context behavior.

Commit: `feat(git): add canonical repository registry`

Gate: two same-origin clones resolve to one remote record while retaining
distinct worktree, index, branch, and buffer state.

### Phase 4: Shared mirror and explicit batch synchronization

- Seed and maintain one bare mirror per canonical repository.
- Fetch remote branches/tags/PR refs only into the mirror.
- Coalesce concurrent sync requests.
- Add filesystem locking, generation IDs, atomic cache updates, bounded global
  sync concurrency, cancellation, and stale/error state.
- Implement current-repository sync and all-registered-repositories sync.
- Fetch clone-local PR objects from the mirror when explicitly requested.
- Prove by process logging that two same-origin contexts issue one remote Git
  fetch and one Forge update.

Commit: `feat(git): deduplicate repository synchronization`

Gate: after one sync, every cached remote view opens offline; an interrupted or
partially failed sync preserves the previous successful generation.

### Phase 5: Shared PR workspace

- Build the PR buffer from cached Forge data and mirror objects.
- Reuse the Changes Tree for the PR range and persist progress by base/head
  object IDs.
- Render summary, changed files, oldest-first commits, description, conversation,
  and cached check rollup as collapsible sections.
- Reuse one buffer per canonical repository and PR number across all clones.
- Implement exact PR and per-commit ranges, including forks and merge commits.
- Select an existing local context before editing or running commands; never
  silently edit whichever clone Forge last remembered.
- Open replies and edits in writable Markdown composition buffers.

Commit: `feat(forge): add shared pull-request workspace`

Gate: the same PR buffer is reused from two clones, performs no network call,
and preserves the chosen edit context.

### Phase 6: Source-aware side-by-side detail

- Configure same-frame, plain-window Ediff/source comparison.
- Apply dark red/green and fine-difference faces after each theme change.
- Parse and render only the active file.
- Support file and commit stepping without rebuilding unrelated buffers or
  creating additional windows.
- Preserve/restore the caller's window configuration.
- Keep Difftastic as the optional structural view.
- Fall back visibly for binary, oversized, or unavailable blobs.

Commit: `feat(git): add source-aware side-by-side review`

Gate: stepping a normal file is under 300 ms when objects are local, and quit
restores the exact pre-comparison layout.

### Phase 7: Issues, comments, and CI completion

- Add shared issue buffers and complete cached conversation actions.
- Add an asynchronous GitHub CI rollup adapter during explicit sync.
- Add cached check details and explicit failed-log retrieval.
- Bind Forge dispatch explicitly without relying on its failing transient
  insertion point.
- Keep provider-specific CI code behind an adapter; GitHub is the first
  implementation and other forges degrade to "checks unavailable."

Commit: `feat(forge): complete issue and check review`

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

### Worker delivery contract

Every implementation worker receives exactly one phase with its gate and must
return:

1. A focused branch with no unrelated configuration changes.
2. A short design note naming the native Magit/Forge APIs used and any advice
   added. New advice requires explicit justification.
3. ERT coverage for state, identity, and key/window behavior appropriate to the
   phase.
4. A manual tmux/terminal walkthrough and, for visual phases, a screenshot of
   the black/red/green result.
5. Before/after timings plus a process log proving whether network and external
   diff tools ran.
6. Known limitations and a clean rollback description.

The reviewer first verifies the phase gate, then checks maintainability and
visual quality. Later workers do not begin on top of an unaccepted phase.

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
- `i` does not enter insert state in generated read-only buffers.
- Source/comment actions enter the correct writable buffer; composition may
  start in insert state.
- Source buffers retain indentation/newline behavior in insert state.
- Stable buffers are reused rather than duplicated.
- `q` returns to the correct caller and restores windows.
- Navigation through status, log, topic, commit, and unified diff preserves the
  window count.
- `o` and `|` create at most one comparison window and reuse it.
- Search, copy, file stepping, commit stepping, and hunk stepping.
- Changes Tree directory collapse, file opening, path search, reviewed toggles,
  partial directory state, and review progress after a changed PR head.

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
9. Same-window navigation never increases window count; repeated explicit
   comparison never exceeds two review windows.
10. Phase 2 local review remains fully usable when Forge, `gh`, Delta, and
    Difftastic are unavailable.

The worker must report before/after timings in the branch description. If a
fixed threshold is unreliable across machines, the implementation must at
least show no regression greater than 20% from the recorded baseline for
unchanged operations.

## 9. Migration and rollback

- Do not delete or reset `forge-database.sqlite`.
- Do not rewrite existing clones' remotes or Git alternates.
- Keep `C-x g` operational throughout the migration.
- Land phases as separate reviewed branches/commits so the shared mirror, PR
  workspace, or side-by-side renderer can be reverted independently.
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

## 11. Owner decision gate

These defaults follow the buffer-first philosophy and are recommended unless
the owner changes them:

- [ ] **Window model:** every normal navigation action replaces the selected
      buffer. Only `o` and `|` create/reuse a second window.
- [ ] **Evil state model:** generated status/review/topic buffers stay read-only
      and in normal state. Editing opens a real source or Markdown buffer.
- [ ] **Key model:** preserve Vim `n`/`N` search behavior; use bracket motions
      for files, hunks, and commits; retain `J`/`K` as eight-line movement.
- [ ] **Diff model:** a fast unified red/green/black Magit buffer is the default
      overview; `|` opens the source-aware two-window detail; `D` is optional
      Difftastic.
- [ ] **Entry point:** `C-c g` opens status in the current window and presents
      the Git Transient; `C-x g` remains direct status.
- [ ] **Data model:** Forge SQLite remains the single authoritative store for
      shared PR, issue, and comment metadata. The workbench adds a canonical
      repository registry, Git-object mirror, and CI snapshot cache without
      extending Forge's schema.
- [ ] **Local/remote boundary:** PRs, issues, remote commits, and immutable diffs
      are shared by canonical identity; branch, index, and working-tree state
      remain local-context-specific.
- [ ] **Network model:** opening and `gr` are offline-only. `@` syncs the current
      repository; `C-c g F` syncs all registered repositories asynchronously.
- [ ] **Clone safety:** existing clones are never rewritten to depend on the
      mirror. Importing objects into a clone is an explicit local copy.
- [ ] **Provider scope:** GitHub is the first complete CI adapter. Forge topic
      support remains provider-neutral and other providers degrade cleanly.
- [ ] **Dependency cleanup:** remove `magit-delta` after Phase 2 visual approval;
      retain Difftastic as an optional, explicit structural view.
- [ ] **Edit context:** version one selects from existing clones/worktrees and
      never creates a new worktree automatically.
- [ ] **Delivery:** use sequential phase branches, accepting each gate before
      the next worker builds on it.

Three decisions have the largest product impact and should be answered
explicitly:

1. Should generated read-only buffers reject insert state as recommended, or
   should `i` enter a read-only insert state with navigation commands rebound?
2. Should unified diff remain the default overview with side-by-side on `|`, or
   should every changed file open side-by-side by default?
3. Should global sync cover every repository ever registered, or only a
   user-maintained allowlist of active repositories? The allowlist is safer for
   credentials, archived projects, and API usage.

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
