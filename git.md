# Git Review Workbench

This is the daily user guide for the Emacs Git and GitHub review workflow.
The design and storage reference is in [docs/git.md](docs/git.md), and
authentication setup is in
[docs/git-review-auth.md](docs/git-review-auth.md).

## Start here

Open a repository file, then press `C-c g` to open the Git dispatch:

| Key | Action | Network |
|---|---|---|
| `g` | Magit status | No |
| `r` | Review working-tree changes | No |
| `s` | Review staged changes | No |
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

### One-time repository setup

Forge is the local GitHub metadata database used by the PR workspace. Register
each new remote repository once:

```text
M-x forge-add-repository
```

Git fetch may use SSH, but Forge needs a GitHub API username and token. See
[Git review authentication](docs/git-review-auth.md).

### Synchronize and open

From a file or review buffer belonging to the repository:

```text
C-c g f    fetch the shared Git mirror and update Forge
C-c g p    enter the PR number
```

Synchronization is asynchronous. `fetching-mirror` means it is still running.
Completion looks like:

```text
Synced github.com/OWNER/REPOSITORY (generation N, forge: current)
```

Use `M-x +git/sync-status` for details. `gr` is intentionally different: it
only rereads the already-published mirror, Forge database, and review state.

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
| `RET` | Visit item or open file diff |
| `o` | Visit in one reusable review window |
| `e` | Open the writable worktree file |
| `SPC` | Toggle reviewed state in Changes Tree |
| `/`, `?`, `n`, `N` | Evil search |
| `gr` | Local-only refresh |
| `t` | Open/reuse Changes Tree |
| `q` | Return to the caller and prior layout |
| `C-h/j/k/l` | Move between Emacs windows |

There are no Git-specific normal bindings for `[`/`]`, staging, discarding,
sync, edit-context selection, commenting, or Difftastic. Those operations stay
behind Magit/Forge Transients, `C-c g`, or `M-x`.

## Buffers and windows

Ordinary navigation replaces the selected window. It should not create a new
window. `q` restores the caller and saved layout.

`o` is the explicit exception: it creates or reuses one review window. Source
buffers are never killed by the return mechanism.

The same PR or immutable commit is shared across clones of the same canonical
remote. Worktree and staged reviews remain specific to one clone/worktree.
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

1. the repository was registered with `M-x forge-add-repository`;
2. `github.user` is configured;
3. Auth Source or 1Password can provide the Forge token;
4. `C-c g f` ends with `forge: current`.

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
