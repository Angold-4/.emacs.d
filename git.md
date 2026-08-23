# Git Review Workbench

This is the daily user guide for the Emacs Git and GitHub review workflow.
The design and storage reference is in [docs/git.md](docs/git.md), and
authentication setup is in
[docs/git-review-auth.md](docs/git-review-auth.md).

## Start here

Open a repository file, then press `C-c g` to open the Git dispatch:

| Key | Action | Network |
|---|---|---|
| `g` | Current PR home; Magit status when no PR matches | No |
| `r` | Review working-tree changes | No |
| `s` | Review staged changes | No |
| `u` | Review unstaged and untracked changes | No |
| `c` | Review one commit | No |
| `b` | Review a base/head branch range | No |
| `p` | Open a cached pull request by number | No |
| `l` | Compact local log | No |
| `f` | Synchronize this repository | **Yes** |
| `F` | Synchronize allowlisted repositories | **Yes** |

`C-x g` remains the direct Magit status command.

## Review local changes

Use one of:

```text
C-c g r    HEAD versus working tree/index, including untracked files
C-c g s    HEAD versus index
C-c g u    index versus worktree, including untracked files
C-c g c    parent versus selected commit
C-c g b    merge-base(base, head) versus head
```

Press `t` in the review overview to open the Changes Tree:

```text
Changes  1/3 reviewed  |  +24 -8
[ ] src/                          +20 -7
    [x] api/                      +8 -2
        [x] users.ts       M      +8 -2
    [ ] ui/                       +12 -5
        [ ] panel.tsx      M      +12 -5
[ ] test/                         +4 -1
    [ ] panel-test.ts      A      +4 -1
```

The checkboxes are personal review progress. `SPC` toggles the file or folder
at point. It does not stage, unstage, discard, edit, commit, or submit
anything.

A typical local loop is:

```text
C-c g r -> t -> RET -> read diff -> e -> edit -> q -> SPC
```

Use Magit status for staging, committing, rebasing, and other Git mutations.
Commit/branch/PR review targets are immutable and reject staging or discard
operations.

## Review a pull request

### Authentication

Forge is the local GitHub metadata database used by the PR workspace. Git may
authenticate through the 1Password SSH agent, but Forge also needs a GitHub API
username and token. See [Git review authentication](docs/git-review-auth.md).

On macOS, run `M-x +forge-store-token-in-macos-keychain` once and paste the
token at the hidden prompt. Later Emacs launches retrieve it automatically.
On Linux and WSL, put the Ghub entry in encrypted `~/.authinfo.gpg`. Use
`M-x +forge-set-session-token` only when no persistent store is approved.

If 1Password opens during a Git fetch, that is the separate SSH agent signing
the Git operation; Forge does not call 1Password or its `op` CLI. PAT rotation
instructions for macOS Keychain and Linux/WSL are in the authentication guide.
After replacing a token, restart Emacs or run
`M-x +forge-clear-token-cache` before the next sync.

### Synchronize and open

From a file or review buffer belonging to the repository:

```text
C-c g f    fetch the shared Git mirror and update Forge
C-c g p    enter the PR number
C-c g g    open the current PR home (or local Magit status)
```

The first `C-c g f` also registers an unknown repository with Forge inside the
same explicit network operation. It does not add a pull-request fetch refspec
to the working clone. Do not use `forge-add-repository` for this workflow.

Synchronization is asynchronous. `fetching-mirror` means it is still running.
Completion looks like:

```text
Synced github.com/OWNER/REPOSITORY (generation N, forge: current)
```

Use `M-x +git/sync-status` for details. `gr` is intentionally different: it
only rereads the already-published mirror, Forge database, and review state.

### Continue work on the current PR branch

`C-c g g` first matches the checked-out branch name against open or draft PRs
in the local Forge cache. A differently named local pick-up branch must track
the provider branch explicitly, for example:

```bash
git branch --set-upstream-to=origin/feat/rate-limit-runtime-enforcement vp/pr569-runtime
```

Detection then matches Git's configured upstream; it never guesses from commit
ancestry and never contacts GitHub. Run `C-c g f` first when the PR is new or
its cached metadata is stale.

The PR workspace deliberately keeps the cached committed PR range separate
from work that exists only in the current clone. Its **Local continuation**
section reports the checked-out branch and four independent layers:

```text
C    commits after the cached PR head
s    staged changes only
u    unstaged tracked changes plus untracked files
r    all staged, unstaged, and untracked work together
```

This makes “what reviewers already see” stable while still providing a clean
view of partially implemented follow-up work. If the branch is behind or has
diverged from the cached PR head, the workspace reports that relationship
instead of constructing a misleading local-commit range.

On a matching branch, `C-c g g` always returns to this PR workspace—the home
for previous PR commits and all local continuation layers. On a branch without
a matching cached open/draft PR, the same command opens Magit status in the
active worktree. The internal `mirror.git` path remains the object store for
the immutable PR range and is never used as a local-command root.

### File-by-file review

This is the default for a small or medium PR:

```text
C-c g p -> PR number -> t
```

Then:

1. Move through folders/files with `j` and `k`, or `gf` and `gF`.
2. Press `RET` for the exact unified file diff.
3. Press `e` to open that path in the selected local edit context when it
   exists.
4. Press `q` to return.
5. Press `SPC` to mark the file reviewed.

Reviewed state is local Emacs metadata. If the PR head advances, files whose
fingerprint changed become unreviewed; unchanged files keep their state.

### Commit-by-commit review

This is useful when a large PR has meaningful commits:

```text
C-c g p -> PR number -> c -> RET
```

`c` moves to the oldest commit row and `RET` opens it. The commit buffer shows
the author/date, subject, full commit body, summary, and native diff. Use:

```text
gc    next commit
gC    previous commit
t     Changes Tree for the whole PR
q     return to the PR workspace
```

Merge commits use their first parent by default. Advanced parent selection
remains an explicit command rather than a normal-mode key.

## Normal-mode keys

The generated Git/Forge buffers deliberately expose a small vocabulary:

| Key | Action |
|---|---|
| `h/j/k/l` | Evil movement |
| `H/L` | Beginning/end of line |
| `J/K` | Move by eight lines |
| `TAB` | Toggle section |
| `S-TAB` | Cycle all sections |
| `gf` / `gF` | Next/previous changed file |
| `gh` / `gH` | Next/previous hunk |
| `gc` / `gC` | Next/previous PR commit |
| `C` | Review local commits after the cached PR head (PR workspace) |
| `r` / `s` / `u` | Review combined/staged/unstaged continuation (PR workspace) |
| `RET` | Visit item or open file diff |
| `o` | Visit in one reusable review window |
| `e` | Open the writable worktree file |
| `SPC` | Toggle reviewed state in Changes Tree |
| `/`, `?`, `n`, `N` | Evil search |
| `gr` | Local-only refresh |
| `t` | Open/reuse Changes Tree |
| `q` | Return in the selected window; preserve other splits |
| `C-h/j/k/l` | Move between Emacs windows |

There are no Git-specific normal bindings for `[`/`]`, staging, discarding,
sync, edit-context selection, commenting, or Difftastic. Those operations stay
behind Magit/Forge Transients, `C-c g`, or `M-x`.

## Buffers and windows

Ordinary navigation replaces the selected window. It should not create a new
window. `q` restores the caller only in the selected window and leaves every
other user-created split and buffer untouched.

`o` is the explicit exception: it creates or reuses one review window. Source
buffers are never killed by the return mechanism.

The same PR or immutable commit is shared across clones of the same canonical
remote. Worktree, staged, and unstaged reviews remain specific to one
clone/worktree.
PR Git objects come from the shared bare mirror; `e` opens files from the
selected writable local context.

## Persistence and Emacs restarts

The following survive Emacs restarts:

- Forge PR/issue/comment metadata;
- the published bare Git mirror and sync generation;
- canonical repository/local-context registry;
- reviewed checkmarks.

The active network process and API token cache are session-local. A clean Emacs
exit releases its sync process lock. A later Emacs automatically reclaims a
same-host lock only when the recorded owner PID is confirmed dead; it never
steals a lock from another live Emacs.

### Configure sync-all

`C-c g F` intentionally does nothing until canonical repository IDs are
allowlisted. IDs use the normalized `host/owner/repository` form shown by
`M-x +git/sync-status`, for example:

```elisp
(setq +git-sync-active-repositories
      '("github.com/Angold-4/.emacs.d"
        "github.com/ORG/PROJECT"))
```

An empty list never means “all repositories.” This keeps global network access
opt-in and bounded.

## Diff display

The default surface is native Magit unified diff rendering:

- black context background;
- dark green added lines;
- dark red removed lines;
- brighter refined regions;
- readable file and hunk headings.

Delta is not enabled automatically. Difftastic is optional and available only
through its Magit Transient or `M-x` commands when the `difft` executable is
installed.

## Troubleshooting

### `forge: unavailable`

The Git mirror may be usable while Forge metadata was not updated. Confirm:

1. `github.user` is configured;
2. macOS Keychain, Linux/WSL Auth Source, or `+forge-set-session-token` can
   provide the Forge token;
3. `C-c g f` ends with `forge: current`.

### `already syncing elsewhere`

Another live Emacs owns the repository sync. Let that job finish or, from a
buffer in that repository in the Emacs process that started it, run:

```text
M-x +git/sync-cancel
```

If the owner process exited, the next `C-c g f` recovers its dead-owner lock
automatically.

### A PR or range changed remotely

Run `C-c g f`, wait for success, then use `gr` or reopen with `C-c g p`.
The workspace validates its cached range before giving it to Magit and rebuilds
when the mirror generation or objects changed.

### Configuration changed while buffers are open

Restart Emacs and reopen the review buffer. Existing buffers can retain old
buffer-local targets and keymaps after an implementation update.

## Current boundary

Phases 0–5 are implemented: buffer/Evil behavior, local Changes Tree, canonical
repository contexts, durable synchronization, and cached PR file/commit review.

Checks, full issue/comment workflows, and a native source-aware side-by-side
view remain later work. The optional Difftastic view is not the canonical
review interface.
