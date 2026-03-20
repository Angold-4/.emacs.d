# Git Diff Review System

A clean, read-only git diff review workflow optimized for reviewing
AI-generated code changes inside Emacs. Built on top of magit.

## Quick Start

Restart Emacs (or `M-x eval-buffer` on `init.el`), then go to any git repo:

```
C-c g r    — Review all working-tree changes (unstaged + untracked)
C-c g s    — Review staged changes only
C-c g c    — Review a specific commit (prompts for ref)
C-c g l    — Compact one-line git log
C-c g g    — Open magit-status (same as C-x g)
```

## Testing Checklist

Open a terminal, make some changes in a git repo (or let an AI tool
make changes), then come back to Emacs and test each step:

### 1. Open the review buffer

```
C-c g r
```

You should see a diff buffer with:
- File stat summary at the top (files changed, insertions, deletions)
- Each modified file as a **collapsed** section (press `TAB` to expand)
- Green lines = added, red lines = removed, with **syntax highlighting**
- Word-level highlighting within changed lines (refined hunks)
- Untracked (new) files shown with full content as green diffs
  (in a collapsible "N untracked files" section — press `TAB` to expand)

### 2. Navigate with hjkl / J K

| Key   | Action                               |
|-------|--------------------------------------|
| `j`   | Move down one line                   |
| `k`   | Move up one line                     |
| `J`   | Move down 8 lines (quick scroll)     |
| `K`   | Move up 8 lines (quick scroll)       |
| `H`   | Beginning of line                    |
| `L`   | End of line                          |
| `gg`  | Go to top of buffer                  |
| `G`   | Go to bottom of buffer               |

### 3. Jump between files and hunks

| Key | Action                      |
|-----|-----------------------------|
| `n` | Jump to next file section   |
| `p` | Jump to previous file       |
| `N` | Jump to next hunk           |
| `P` | Jump to previous hunk       |

### 4. Expand / collapse sections

| Key       | Action                              |
|-----------|-------------------------------------|
| `TAB`     | Toggle section under cursor         |
| `S-TAB`   | Cycle all sections (expand/collapse)|

### 5. Adjust context (like GitHub's "expand" button)

| Key | Action                                 |
|-----|----------------------------------------|
| `+` | Show 3 more context lines around hunks |
| `=` | Same as `+`                            |
| `-` | Show 3 fewer context lines             |

Start at 6 lines of context (GitHub default). You can keep pressing `+`
to see up to 50 lines of surrounding code.

### 6. Open files from the diff

| Key   | Action                                                |
|-------|-------------------------------------------------------|
| `RET` | Open file at point in a **right side split**          |
|       | Jumps to the exact line shown in the diff             |
| `o`   | Open file in other window (simpler split)             |

This is the core "review then edit" flow:
1. Read the diff
2. Press `RET` on a line you want to change
3. Edit the file in the side window
4. `C-h` to go back to the diff buffer (windmove left)
5. Continue reviewing

### 7. Stage / unstage / discard from the review

| Key | Action                          |
|-----|---------------------------------|
| `s` | Stage the hunk or file at point |
| `u` | Unstage hunk or file            |
| `x`  | Discard hunk or file (careful!) |
| `gr` | Refresh the diff buffer         |
| `q`  | Quit the review buffer          |

### 8. Window navigation

`C-h/j/k/l` moves between windows (left/down/up/right), so you can
quickly switch between the diff buffer and any file you opened.

### 9. Search

| Key | Action          |
|-----|-----------------|
| `/` | Search forward  |
| `?` | Search backward |

### 10. Visual mode (select & copy)

Press `v` to enter visual mode, select text, then `C-y` to copy
to tmux's paste buffer. Then switch to any other tmux pane
(e.g. OpenCode) and press `C-a P` to paste.

Flow: **Emacs `C-y`** → tmux buffer → **`C-a P`** → paste into target pane

## Other Features

### diff-hl: Gutter indicators

Every file buffer now shows git change indicators in the left fringe:
- Green bar = new lines
- Orange bar = modified lines  
- Red bar = deleted lines

These update live as you edit (flydiff mode).

### Magit Status (C-x g) improvements

The magit-status buffer also has the enhanced navigation:
- `J/K` for 8-line jumps
- `RET` opens files in a side split
- `+/-` adjusts context
- `gr` refreshes (instead of bare `g` which conflicted with `gg`)
- `x` discards (instead of `0`)

### Diff colors

The diff faces are tuned for the dark theme (noctilux/black bg):
- Added: very subtle green tint — light enough for white syntax text
- Removed: very subtle red tint — same principle
- Word-level changes: delta highlights changed words/chars with brighter bg
- Context: dim gray (doesn't distract from changes)
- File headers: bold white on dark blue
- Hunk headers: dim blue
- Section headers: gold/yellow

### Syntax highlighting in diffs (via delta)

Diffs are syntax-highlighted using [delta](https://github.com/dandavison/delta),
a Rust-based syntax-highlighting pager for git output. The integration uses
the [magit-delta](https://github.com/dandavison/magit-delta) package which
pipes raw diff output through `delta --color-only` and converts ANSI colors
to Emacs overlays.

- Dark theme: **Nord** (light theme: GitHub)
- `+/-` markers are hidden (replaced with spaces) for cleaner diffs
- Word-level diff highlighting is handled by delta (magit's `refine-hunk` is
  disabled when magit-delta is active)
- Toggle on/off: `M-x magit-delta-mode`
- When toggled off, falls back to the custom background-only diff faces

**Dependency:** `delta` CLI tool, installed at `~/.cargo/bin/delta`
(installed via `cargo install git-delta`).

### Untracked files (collapsible)

New untracked files appear at the bottom of `C-c g r` as a collapsible
"N untracked files" section. Each file is its own sub-section:
- The outer "untracked files" section starts collapsed — press `TAB` to expand
- Each file within is also collapsible with `TAB`
- Content is shown as all-green (+) lines with syntax highlighting
- Navigate them with `n/p` just like tracked file diffs

## Workflow: Reviewing AI Changes

Typical flow after an AI agent modifies your codebase:

1. `C-c g r` — open the review buffer
2. `n/p` — jump between changed files
3. `TAB` — collapse files you've already reviewed
4. `+` — expand context to understand surrounding code
5. `RET` — jump into a file to make manual fixes
6. `C-h` — return to diff buffer
7. `s` — stage hunks you approve
8. `x` — discard hunks you reject
9. `C-c g s` — review what you've staged before committing
10. `C-x g` then `c c` — commit via magit

## Files Changed

| File | What changed |
|------|-------------|
| `core/init-git.el` | Review logic, delta-powered syntax-highlighted diffs, collapsible untracked files, diff-hl, keybindings, clipboard comment |
| `core/init-core.el` | `+copy-to-system-clipboard` — copies to tmux paste buffer via `tmux load-buffer -` |
| `core/init-evil.el` | Keybinding docs, magit bindings (`gr`/`x`), `C-y` visual copy, clipboard comments |
| `core/init-tools.el` | Enhanced magit base config (more commands, auto-revert) |
| `init.el` | Added `init-git` to module load list |
| `~/.config/tmux/tmux.conf` | `C-a P` = paste-buffer, `C-y` in copy-mode, `set-clipboard external` |

## Clipboard

Clipboard sharing between tmux panes (e.g. Emacs ↔ OpenCode):

| Step | Key | What happens |
|------|-----|--------------|
| 1. Select text in Emacs | `v` + motion | Enter visual mode, select region |
| 2. Copy | `C-y` | Copies to Emacs kill ring + tmux paste buffer |
| 3. Switch to target pane | `C-a` + arrow/number | Go to the pane where you want to paste |
| 4. Paste | `C-a P` | Pastes tmux buffer content into the pane |

This works over SSH without requiring OSC 52 terminal support.

## Dependencies

| Tool | Install | Purpose |
|------|---------|---------|
| `delta` | `cargo install git-delta` | Syntax highlighting for diffs |
| `cargo` | System package or rustup | Needed to install delta |
| `tmux` | System package | Clipboard sharing between panes |
