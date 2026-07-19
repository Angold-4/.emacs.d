# Git Review Workbench Architecture

Status: **Phases 0–5 implemented on PR #5**

This is the architecture and behavior reference for the Emacs Git review
workbench. The concise daily workflow is in [../git.md](../git.md),
authentication is in [git-review-auth.md](git-review-auth.md), and the original
performance baseline is in [git-baseline.md](git-baseline.md).

## Product contract

### Everything is an Emacs buffer

Status, PR overview, commit, Changes Tree, and unified diff views use ordinary
Emacs buffers, windows, modes, keymaps, and Magit sections. Generated views are
read-only and start in Evil normal state. Real edits happen in normal writable
source or Markdown buffers.

### Navigation is same-window by default

Opening a normal review destination replaces the selected window. `o` is the
explicit reusable-other-window action. `q` restores the recorded caller,
point, and window configuration when possible.

### Network access is explicit

Only these normal user entry points may contact a remote:

```text
C-c g f    synchronize the current canonical repository
C-c g F    synchronize explicitly allowlisted repositories
```

Opening buffers, `RET`, `e`, `t`, `gr`, navigation, and reviewed-state changes
are local-only. `gr` means “rebuild from local Git objects and caches.”

### Shared remote state and local work state are distinct

Canonical remote identity owns shared immutable data: mirror objects, Forge
metadata, PR buffers, commit buffers, and PR reviewed state. A local context
owns its worktree, index, branch, uncommitted changes, and local review buffers.

### The normal-mode API stays small

The common review mode exposes movement, sections, file/hunk/commit navigation,
visit, edit, reviewed state, local refresh, tree, search, and return. Mutation,
network, comments, edit-context selection, merge-parent selection, and optional
renderers remain behind Transient or `M-x`.

## Module ownership and load order

`init.el` loads the Git modules in this order:

```text
core/init-git.el
  -> core/init-git-store.el
  -> core/init-git-sync.el
  -> core/init-git-ui.el
  -> core/init-forge.el
  -> core/init-git-pr.el
```

| Module | Responsibility |
|---|---|
| `init-git.el` | Magit ownership/settings, local review entry commands, `C-c g` Transient, compact log, magit-todos |
| `init-git-store.el` | Canonical repository identity, local contexts, persistent registry |
| `init-git-sync.el` | Shared mirror, locks, async synchronization, publication/recovery, Forge sync adapter |
| `init-git-ui.el` | Window policy, Evil review mode, review targets, Changes Tree, reviewed state, Magit diffs/faces, optional renderers |
| `init-forge.el` | Forge package ownership, cond-let compatibility, cached PR adapter, optional 1Password token provider |
| `init-git-pr.el` | Shared PR model, overview, PR refresh, file/commit navigation, cached conversation |

Magit package ownership is no longer split with `init-tools.el`. Forge no
longer advises PR visits to fetch or replace the topic with an ad-hoc range.

## User entry points

`C-c g` runs `+git-dispatch`:

| Suffix | Command | Scope | Network |
|---|---|---|---|
| `g` | `magit-status` | current local context | No |
| `r` | `+git/review` | worktree/index | No |
| `s` | `+git/review-staged` | index | No |
| `c` | `+git/review-commit` | immutable commit | No |
| `b` | `+git/review-branch` | immutable merge-base/head range | No |
| `p` | `+git/review-pull-request` | cached PR | No |
| `l` | `+git/log-oneline` | local log | No |
| `f` | `+git/sync` | canonical repository | **Yes** |
| `F` | `+git/sync-all` | configured allowlist | **Yes** |

`C-x g` remains direct Magit status.

## Common buffer and Evil behavior

`+git-review-buffer-mode` is enabled in Magit, Forge topic, Changes Tree, PR,
and generated review buffers.

| Key | Behavior |
|---|---|
| `h/j/k/l` | Evil character/visual-line movement |
| `H/L` | Beginning/end of line |
| `J/K` | Eight lines down/up |
| `TAB`, `S-TAB` | Toggle section / cycle all sections |
| `gf`, `gF` | Next/previous file section |
| `gh`, `gH` | Next/previous hunk |
| `gc`, `gC` | Next/previous PR commit when a PR model is attached |
| `RET` | Native primary visit or exact file diff |
| `o` | Explicit reusable review window |
| `e` | Writable file in active edit context |
| `SPC` | Reviewed toggle in Changes Tree only |
| `/`, `?`, `n`, `N` | Evil search |
| `gr` | Local-only refresh |
| `t` | Changes Tree |
| `q` | Restore caller/layout |
| `C-h/j/k/l` | Window movement |

Notably absent are the old `n/p` section movement, `N/P` hunk movement,
normal-mode stage/unstage/discard keys, `[`/`]` navigation, `@` sync, `L`
context selection, and direct `D` Difftastic binding.

## Review targets

`+git-review-target` describes every local/PR review:

```text
root
scope
base-ref / base-oid
head-ref / head-oid
repository-id / context-id
family-id / overview-id
mutable-p
pr-number
```

| Scope | Range | Persistence identity | Buffer identity |
|---|---|---|---|
| `worktree` | `HEAD` vs index/worktree + untracked | repository + context | stable per context |
| `staged` | `HEAD` vs index | repository + context | stable per context |
| `commit` | first parent vs commit; empty tree for root | repository + commit | resolved base/head OIDs |
| `branch` | merge-base(base, head) vs head | repository + context + labels | resolved base/head OIDs |
| `pullreq` | cached merge-base vs exact PR head | repository + PR number | stable PR workspace |

Worktree and staged targets are mutable. Commit, branch, and pull-request
targets reject stage/unstage/discard operations.

PR and PR-commit object operations use the published bare mirror. `e` resolves
the selected local edit context independently. If a local `r`/`s` review is
launched from a PR buffer, it uses that edit worktree and explicitly refuses to
treat `mirror.git` as a worktree.

## Changes Tree

`+git-changes-tree-mode` is a lazy Magit-section tree. It batches Git
machine-readable output:

```text
git diff --name-status -z
git diff --numstat -z
git diff --raw -z
git ls-files -z --others --exclude-standard    # worktree scope only
git hash-object --stdin-paths                  # one batch when needed
```

It never parses rendered Magit text and does not create per-file diff buffers
until requested. Directories carry full relative paths internally while the UI
shows basenames with four-space indentation.

The tree represents modified, added, deleted, renamed, copied, untracked,
binary, and submodule entries. Directory checkboxes roll up descendant state:

```text
[ ] unreviewed
[-] partially reviewed
[x] reviewed
```

Reviewed state maps path to a content/object fingerprint. Refresh preserves
unchanged entries and clears only changed or removed fingerprints.

## Diff rendering and visits

The default renderer is native Magit unified diff with project faces reapplied
after theme changes:

- black context;
- dark red removed lines;
- dark green added lines;
- brighter refined regions;
- quiet but legible file/hunk headings.

File visits use Magit APIs and structured `+git-review-file` values, not diff
header parsing. Rename/copy diffs use old and new pathspecs. Untracked files use
Git-produced `--no-index` patches. Deleted files remain reviewable but have no
writable `e` target. Binary and submodule buffers keep native Magit sections
and add explicit presentation notes.

`magit-delta` is installed only when `delta` exists and is not enabled
automatically. Difftastic is optional, has no review normal-mode binding, and
is available through its Magit Transient/`M-x` commands when `difft` exists.

## Canonical repository store

`+git-store-repository` owns shared repository state. A
`+git-store-local-context` represents one independent clone or linked
worktree.

Canonical URL normalization treats transport-equivalent GitHub URLs as the
same repository:

```text
git@github.com:org/project.git
ssh://git@github.com/org/project.git
https://github.com/org/project.git
```

Hosts, explicit ports, and distinct nested paths do not collide. Relative and
local remotes resolve against the repository root and use a local identity.

Independent clones have distinct `context-id`/`git-dir` values. Linked
worktrees share `git-common-dir` but retain distinct roots, branches, and
indexes. Context eligibility always checks repository identity, availability,
root existence, and required OIDs when applicable.

## Persistent storage

All paths are under `user-emacs-directory`:

| Data | Path |
|---|---|
| Forge metadata | `forge-database.sqlite` |
| Canonical registry | `.cache/git-review/repositories/registry.json` |
| Shared mirror | `.cache/git-review/repositories/<sha256>/mirror.git` |
| Sync state/lock | same repository cache directory |
| Reviewed state | `.cache/git-review/state/<sha256>.eld` |

Forge remains the sole PR/issue/comment metadata database; the workbench does
not maintain handwritten Forge SQL or a second PR database.

The registry and sync state use JSON and atomic temporary-file rename. Reviewed
state uses Emacs Lisp data written by this configuration and supports migration
from the Phase 2 root-based identity. Malformed state degrades to empty state
with an explanatory message.

## Synchronization

`+git-sync-start` runs an asynchronous state machine:

```text
idle
  -> queued
  -> preparing-mirror
  -> fetching-mirror
  -> pulling-forge
  -> fetching-checks
  -> publishing
  -> succeeded | failed | cancelled
```

The mirror candidate is prepared locally, fetched with
`GIT_TERMINAL_PROMPT=0`, and published only after the complete candidate is
ready. Publication uses a backup plus publish-intent so a state-write or rename
failure restores or exposes the previous consistent generation.

In-process requests coalesce by repository. Cross-process exclusion uses an
atomically created directory containing PID, host, timestamp, and nonce. A
clean Emacs exit cancels its processes, removes its candidate, and releases its
owned lock. A later process may atomically quarantine a lock only when its
same-host owner PID is confirmed dead. Live and malformed locks remain busy.

GitHub sync fetches branches, tags, and `refs/pull/*/{head,merge}`. GitLab
fetches branches, tags, and merge-request heads. Generic providers fetch
branches/tags and report provider PR refs unavailable.

Forge pull is part of the same explicit job. Git success followed by Forge
failure does not publish a new generation. The previous mirror and Forge cache
remain usable offline.

The `fetching-checks` state is currently an extension point; it does not fetch
CI data yet.

## Pull-request workspace

`C-c g p` reads Forge’s local cache and opens one `+git-pr-mode` buffer keyed by
canonical repository + PR number. Opening the same PR from another same-origin
clone reuses the buffer and adopts that clone as edit context.

The overview renders:

```text
title and state
review status
base/head refs and exact OIDs
repository and edit context
cache generation/age/Forge state
changes summary
commits (oldest first)
checks placeholder
description
cached conversation/reviews
```

The Git range is derived from the cached exact base/head OIDs and the published
mirror’s merge base. Provider pull refs are discovery inputs; a provider merge
ref is never substituted for the PR head. Merged and squash-merged PRs retain
their original review range when the objects remain cached.

Before opening a tree/file or refreshing, the workspace verifies that its
generation, mirror path, and exact range are still valid. A new generation,
force-push, or replaced mirror rebuilds the model before Magit receives the
range. A failed rebuild leaves the previous workspace visible with an
actionable message.

Commit review uses `rev-list --reverse --topo-order`. The commit buffer renders
the full subject/body and native parent-to-commit diff. `c` focuses the oldest
commit row, `RET` opens it, and `gc`/`gC` step without losing the PR workspace
return state.

## Forge and authentication

Forge is loaded through `init-forge.el`. A compatibility bridge is installed
before Forge macro expansion because the pinned Forge and cond-let versions use
different internal threading macro names.

Forge reads remain local. The explicit sync adapter uses Forge’s pull path and
tracks asynchronous URL/Ghub callbacks so cancellation and errors terminate the
job rather than leaving it in `pulling-forge`.

Ghub first uses ordinary Auth Source. If no token is found for package `forge`
and a host has an `op://` reference, the optional adapter runs:

```text
op read REFERENCE
```

Only the non-secret reference appears in argv. The token is held in memory for
the Emacs session and can be cleared with
`M-x +forge-1password-clear-token-cache`. See
[git-review-auth.md](git-review-auth.md).

## Safety and failure behavior

- Buffer open and local refresh never fetch.
- Failed sync preserves the prior published generation.
- Duplicate sync requests coalesce.
- A live foreign lock is never stolen.
- Cancel affects only the selected repository job.
- Remote credentials are sanitized from recorded errors.
- Import destinations are restricted to `refs/git-review/*` and validated by
  `git check-ref-format`.
- Immutable reviews reject index/worktree mutation commands.
- Missing/deleted edit contexts fall back only to eligible same-repository
  contexts.
- Source buffers are not killed by review return handling.

## Verification

The complete suite is:

```bash
rtk emacs --batch -Q -l init.el \
  --eval '(princ "CONFIG-LOAD-OK\n")'

rtk emacs --batch -Q -l init.el -L test \
  -l test/git-review-test.el -f ert-run-tests-batch-and-exit

rtk git diff --check
```

At the Phase 5 acceptance point, plus the documentation audit:

- 137 ERT tests pass with zero unexpected results;
- documentation contract tests reject stale commands and broken local links;
- warm 100-file Changes Tree rendering is approximately 0.05 seconds;
- tree refresh uses 10 bounded Git processes and creates no eager file diffs;
- warm cached PR rendering is approximately 0.05 seconds;
- offline guards reject network-capable Git, Ghub/URL, `gh`, Delta, and
  Difftastic calls in local paths;
- cross-process tests cover publication rollback, lock races, clean exit,
  dead-owner restart, cancellation, and late Forge callbacks;
- 1Password tests cover Auth Source priority, one-shot fallback, memory cache,
  CLI failure, and token exclusion from argv.

The historical pre-customization measurements remain in
[git-baseline.md](git-baseline.md).

## Implemented and deferred scope

Implemented in Phases 0–5:

1. baseline/safety fixtures;
2. same-window Evil buffer contract;
3. local review targets and Changes Tree;
4. canonical repository/local-context registry;
5. durable explicit mirror + Forge synchronization;
6. cached PR overview with file-by-file and commit-by-commit review.

Deferred:

- Phase 6: native source-aware before/after detail view;
- Phase 7: complete issue/comment/check workflows;
- Phase 8: doctor command, final installation/upgrade hardening, and removal of
  remaining compatibility paths.

Optional Difftastic exists today, but it is not the planned native Phase 6
interface. Checks intentionally render unavailable until Phase 7.
