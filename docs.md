# Emacs Configuration - Keybinding Reference

> Modular Emacs configuration for Emacs 30.2 (stable) with Evil mode.
> Config root: `~/.emacs.d/core/`

---

## General Navigation (Evil)

| Key | Action | Mode |
|-----|--------|------|
| `h/j/k/l` | Standard vim movement | Normal |
| `H` | Beginning of line | Normal |
| `L` | End of line | Normal |
| `J` | Move down 8 lines | Normal |
| `K` | Move up 8 lines | Normal |
| `gJ` | Join lines (original `J` behavior) | Normal |
| `jk` | Escape to normal mode | Insert |
| `C-h/j/k/l` | Window navigation (left/down/up/right) | Normal |

## Files & Buffers

| Key | Action |
|-----|--------|
| `C-p` | Find file in project (Projectile) |
| `C-c C-f` | Search with ripgrep |
| `C-x b` | Switch buffer (perspective-aware, filters internal buffers) |
| `C-x C-b` | IBuffer (full buffer list) |
| `C-c r` | Rename buffer |
| `M-RET` | Toggle fullscreen |

## LSP

LSP is **not auto-enabled**. It must be toggled on manually per buffer.

### Control

| Key | Action |
|-----|--------|
| `C-x l` | Toggle LSP on/off for current buffer |
| `C-x i` | Toggle inlay hints (type annotations) |

### Navigation (requires LSP active)

| Key | Action |
|-----|--------|
| `M-.` | Jump to definition |
| `M-,` | Jump back |
| `M-?` | Find references |
| `M-/` | Find type definition |

### Diagnostics

| Key | Action |
|-----|--------|
| `C-c e n` | Go to next error |
| `C-c e p` | Go to previous error |
| `C-c e l` | List all errors in buffer |
| `C-c C-k` | Show detailed error at point in a buffer below |
| *(hover)* | Errors show in echo area automatically via eldoc |

### Code Actions & Documentation

| Key | Action |
|-----|--------|
| `C-c .` | Show documentation (eldoc) |
| `C-c C-d` | Show doc popup (lsp-ui) |
| `M-l` | Code actions (quick fix) |
| `C-c f` | Format buffer (prog-mode only; in org-mode this runs cleanup instead) |

### Supported Language Servers

| Language | Server | Notes |
|----------|--------|-------|
| C/C++ | `clangd` | Needs `compile_commands.json` |
| Rust | `rust-analyzer` | Custom workspace root detection |
| Go | `gopls` | |
| Python | `pyright` / `pylsp` | |
| TypeScript/JS | `typescript-language-server` | |

## Minibuffer Completion (Vertico + Orderless)

Vertico provides a vertical completion UI for all `completing-read` prompts.
Orderless enables fuzzy, multi-component matching — type space-separated
patterns in any order.

| Key | Action |
|-----|--------|
| `C-p` | Find file in project (uses Vertico) |
| `M-x` | Execute command (fuzzy matched) |
| `C-x b` | Switch buffer (fuzzy matched) |
| `C-n` / `C-p` | Next / previous candidate in minibuffer |
| `RET` | Select candidate |
| `C-g` | Cancel |

**Fuzzy matching examples:**
- `binance ts` matches `hybrid/src/libs/exchanges/core/binance.ts`
- `init lang` matches `core/init-languages.el`

Marginalia adds rich annotations next to candidates (file sizes, docstrings,
keybindings).

## Autocomplete (Company)

Company mode is enabled globally on startup.

| Key | Action |
|-----|--------|
| `TAB` | Accept completion |
| `RET` | Accept completion |
| `C-n` | Next candidate |
| `C-p` | Previous candidate |

Completion triggers after 2 characters with a 0.2s delay.
Backends: `company-capf` (integrates with LSP) > `company-dabbrev-code` + keywords + files > `company-dabbrev`.

## Git and code review

| Key | Action |
|-----|--------|
| `C-x g` | Magit status |
| `C-x M-g` | Full Magit dispatch |
| `C-c g` | Git review dispatch |

The review dispatch provides local worktree/staged/commit/branch review, cached
pull-request review, compact log, and the explicit synchronization commands.
Generated review buffers use the shared Evil vocabulary: `TAB`, `RET`, `e`,
`o`, `t`, `gf/gF`, `gh/gH`, `gc/gC`, `gr`, and `q`.

Use Magit status/Transients for staging, committing, pushing, pulling, and other
Git mutations. See [git.md](git.md) for the tested daily workflow and
[docs/git.md](docs/git.md) for architecture and storage behavior.

## Treemacs (File Tree)

| Key | Action |
|-----|--------|
| `C-x t t` | Toggle treemacs sidebar |
| `` s-` `` | Select treemacs window |

## Workspaces (Persp-mode)

| Key | Action |
|-----|--------|
| `C-c w s` | Switch workspace |
| `C-c w n` | Next workspace |
| `C-c w p` | Previous workspace |
| `C-c w k` | Kill workspace |
| `C-c w r` | Rename workspace |
| `C-c w l` | List workspaces |
| `C-c w b` | Switch buffer (all buffers, ignoring perspective) |

## Projectile (Project Management)

| Key | Action |
|-----|--------|
| `C-p` | Find file in project |
| `C-c C-f` | Ripgrep search in project |
| `C-c p` | Projectile command map prefix |

## Shell / Terminal

### Eshell

| Key | Action |
|-----|--------|
| `C-p` / `Up` | Previous input (insert mode) |
| `C-n` / `Down` | Next input (insert mode) |
| `C-r` | Search history backwards (insert mode) |

Use `M-x +eshell/toggle` to toggle eshell at the bottom, or `M-x +eshell/new`
to open a new eshell buffer.

### Vterm

| Key | Action |
|-----|--------|
| `C-c v` | Toggle vterm at bottom |
| `C-c V` | Open new vterm |

Vterm starts in insert state. A selected insert-mode terminal gives live output
ownership of its cursor and viewport. Every unselected vterm window is frozen,
even if that buffer remained in insert state after `windmove`; normal or visual
state also freezes the selected window like a read-only scrollback buffer.
Projectile `C-p`, buffer switches, and agent redraws therefore cannot move a
terminal you are watching in another split. Deliberate normal-mode navigation
updates only that window's saved view, while returning to a selected insert-mode
terminal reconnects it to the real terminal cursor. Agent TUI output is
coalesced into complete 20 FPS redraws to avoid painting partial-frame flashes.
The underlying PTY remains live throughout.

## Pi Agent (Pilish)

`init-pilish.el` drives [Pi](https://pi.dev), a minimal and extensible coding
agent, through [Pilish](https://github.com/dnouri/pilish). Pilish talks to the
`pi` CLI over its JSON-RPC mode, so the conversation is rendered Markdown in one
window and the prompt is an ordinary Emacs buffer in the other: Evil motions,
yank, search and narrowing all work, and no PTY or key forwarding is involved.

`C-c m` is the dedicated prefix, a deliberately small subset matching the
OpenCode integration:

| Key | Action |
|-----|--------|
| `C-c m c` | Create (start or focus) a session in this workspace |
| `C-c m m` | Browse every previous session (all projects) |
| `C-c m a` | Pick the agent's model |

`C-c m m` runs `+pilish/sessions`: when this project has no live session it
first starts/reuses one with `pilish` (Pilish's browser is meant to be opened
from a live session, and its RET guard needs a live linked chat buffer), then
opens `pilish-session-browser`. RET resumes any session in the list. As a
safety net for the no-session-linked case, `init-pilish.el` also advises
`pilish--browse-switch-session` to call `pilish-open-session-file` on the
selected session when no live process is linked (or its process has died).

Pilish's own `C-c C-*` bindings (`C-c C-c` send, `C-c C-s` steering,
`C-c C-k` abort, `C-c C-p` menu, `C-c C-r` sessions, `C-c C-e` export,
`C-c C-m` model, `C-c C-t` thinking, `C-c C-y` copy last, `C-c C-n` new) are
removed from both buffers, so this config's global `C-c` bindings apply inside
Pilish buffers. A prompt is sent with RET in the input buffer's normal state
(type, ESC, RET); RET in insert state still inserts a newline. Prompt history
(`M-p`/`M-n`), path/command completion (`TAB`) and the `M-x pilish-*` commands
are unchanged.

### Multiple sessions

Pilish supports several live sessions in one project. `C-u C-c m c` calls
`pilish` with a prefix argument, which prompts for a session name; each named
session is a separate buffer pair (`*pilish-chat:dir<name>*` /
`*pilish-input:dir<name>*`) with its own `pi` process, so sessions run
concurrently (verified: `alpha` and `beta` in one directory yield two live
processes and two buffer pairs). Without a name, `C-c m c` reuses the unnamed
session.

`M-x pilish RET name` re-focuses a named session's buffers; `M-x pilish-toggle`
hides/shows the project's session in the current frame. Since tabs and frames
each carry their own window configuration, the practical way to watch two at
once is one session pair per tab (`M-x tab-bar-new-tab`, then start or focus the
session there) or per frame. The session browser (`C-c m m`) reads persisted
sessions from disk, so each named session also appears there as its own file.

The read-only chat buffer is switched from Pilish's default Evil *motion*
state to **normal** state, so this config's normal-state map applies: hjkl and
`w` motions, `H`/`L` beginning/end of line, `J`/`K` the 8-line jumps, `v`/`V`
visual selection and yank. Only the Pilish keys that normal state would shadow
and that matter in a read-only transcript are re-asserted: `i`/`a` focus the
input window, RET visits the file at point, TAB folds a tool/thinking block.
Message motion (`n`/`p`) and fork (`f`) fall through to Evil's native
bindings; the `M-x pilish-*` commands still cover them.

Chat and input are two windows in one frame — chat on top and the input in the
lower third (`pilish-input-window-height` 0.3, `pilish-input-window-display`
`always`) — and each keeps its own point and scroll. Pilish's follow logic
makes that independent: a window parked at the buffer end follows new output
while a window you scrolled up in stays put. They are not merged into a single
buffer, because the chat is read-only rendered Markdown (tree-sitter,
foldable tool sections) while the input is editable text; a split window pair
already gives per-window scrolling.

Models are served through the **Vercel AI Gateway**. Pi's provider id is
`vercel-ai-gateway`, so authenticate it in Pi's own store — either export
`AI_GATEWAY_API_KEY` in the environment Emacs launches from, or add

```json
{ "vercel-ai-gateway": { "type": "api_key", "key": "<your-key>" } }
```

to `~/.pi/agent/auth.json`. This is deliberately separate from OpenCode's
`~/.local/share/opencode/auth.json`, whose provider id is `vercel`; Pi does not
read that file. `+pilish-provider` (default `"vercel-ai-gateway"`) and
`+pilish-model` (default nil) are forwarded to the CLI as `--provider` and
`--model`; set either to nil to let Pi choose, and its picker still lists every
authenticated provider. The `pi` CLI comes from
`npm install -g @earendil-works/pi-coding-agent`; Pilish offers to install the
Markdown tree-sitter grammars on first run.

Pi's RPC `set_model` does not persist a default, so a model picked with
`C-c m a` would be forgotten by the next session. `init-pilish.el` therefore
advises `pilish--update-state-from-response' and, on a successful `set_model`
or `cycle_model`, writes `defaultProvider` and `defaultModel` into Pi's own
`~/.pi/agent/settings.json` (read-modify-write, atomic, mode 0600), preserving
every other key. Pi then applies the remembered model to each new session
through its normal resolution order. Set `+pilish-remember-model` to nil to
opt out; `+pilish-model` still forces an explicit model for the CLI.

## Org-mode

See the [Org-Mode Workflow](#org-mode-workflow) section below for comprehensive
documentation on task management, time tracking, and calendar sync.

| Key | Action |
|-----|--------|
| `C-c l` | Store link |
| `C-c a` | Org agenda |
| `C-c c` | Org capture (quick entry) |
| `C-c d` | Set deadline |
| `C-c s` | Schedule |
| `C-c f` | Cleanup: refile DONE tasks from Inbox to Completed, normalize formatting |
| `C-c o w` | Open `weids/tasks.org` |
| `C-c o z` | Open `zynerise/tasks.org` |
| `C-c o a` | Open `agenda/tasks.org` |
| `C-c o g` | Open `gcal.org` |
| `C-c C-t` | Cycle TODO state |
| `TAB` | Cycle visibility |

## Symbol Overlay

| Key | Action |
|-----|--------|
| `C-c s i` | Highlight symbol at point |
| `C-c s n` | Jump to next occurrence |
| `C-c s p` | Jump to previous occurrence |
| `C-c s c` | Remove all highlights |

## Clipboard

| Key | Action |
|-----|--------|
| `C-y` | Copy to system clipboard (visual mode) |

WSL, macOS, and Linux X11 clipboard integration is automatic.

## Misc

| Key | Action |
|-----|--------|
| `C-h f` | Describe function (helpful) |
| `C-h v` | Describe variable (helpful) |
| `C-h k` | Describe key (helpful) |
| `C-h x` | Describe command (helpful) |

## Tree-sitter

Tree-sitter grammars are **not auto-installed**. Run the following to install all configured grammars:

```
M-x +treesit/install-all-grammars
```

When a grammar is installed, the corresponding major mode is automatically remapped to its `-ts-mode` variant (e.g. `c++-mode` -> `c++-ts-mode`).

## Formatting (`C-c f`)

In programming modes, `C-c f` runs the code formatter. In org-mode, the same
key runs the cleanup command instead (see [Cleanup](#cleanup-c-c-f) below).

The formatter used depends on the current major mode:

| Mode | Formatter |
|------|-----------|
| C/C++ | `clang-format` |
| Rust | `rustfmt` (reads edition from `Cargo.toml`) |
| Go | `gofmt` |
| Python | `black` or `autopep8` |
| JS/TS/CSS/HTML | `prettier` |
| Any (with LSP) | LSP formatter takes priority |

---

# Org-Mode Workflow

Complete guide to the multi-project task management, time tracking, and
calendar sync system.

---

## File Structure

```
~/org/
  weids/
    tasks.org          <-- Central task file (agenda source)
    notes.org          <-- Free-form notes
    *.org              <-- Any other notes, research, docs...
  zynerise/
    tasks.org          <-- Central task file (agenda source)
    notes.org          <-- Free-form notes
    *.org              <-- Any other notes, research, docs...
  agenda/
    tasks.org          <-- Personal task file (agenda source)
    *.org              <-- Legacy notes, reflections, etc.
  gcal.org             <-- Google Calendar events (org-gcal managed)
  calendar.ics         <-- Auto-generated ICS for Thunderbird
```

**Key design**: Only the 3 `tasks.org` files + `gcal.org` feed into the
agenda. Notes files are free-form -- create as many `.org` files as you want
in each directory and they won't pollute the agenda.

Each `tasks.org` file has three sections:

- **Inbox** -- new tasks land here (from capture or manual entry)
- **Someday** -- tasks deferred for the future, not actively tracked
- **Completed** -- tasks automatically refiled here when marked DONE or CANCELLED

---

## TODO States

Tasks cycle through these states via `C-c C-t`:

```
TODO  -->  IN-PROGRESS  -->  WAITING  --|-->  DONE
                                        |-->  CANCELLED
```

The `|` separates active (left) from done (right) states.

### Changing a TODO state

| Action | Key |
|--------|-----|
| Cycle to next state | `C-c C-t` |
| Pick state by letter | `C-c C-t` then `t`/`i`/`w`/`d`/`c` |
| Mark DONE directly | `C-c C-t d` |
| Cancel | `C-c C-t c` (prompts for a reason) |

### Fast-access keys

After pressing `C-c C-t`, a menu appears:

| Key | State | Logging |
|-----|-------|---------|
| `t` | TODO | -- |
| `i` | IN-PROGRESS | Logs timestamp |
| `w` | WAITING | Prompts for note, logs timestamp on exit |
| `d` | DONE | Logs timestamp |
| `c` | CANCELLED | Prompts for reason |

### Example flow

Start:
```org
* TODO Implement ZK proof verifier :weids:
  SCHEDULED: <2026-03-20 Fri>
```

Press `C-c C-t i`:
```org
* IN-PROGRESS Implement ZK proof verifier :weids:
  SCHEDULED: <2026-03-20 Fri>
  :LOGBOOK:
  - State "IN-PROGRESS" from "TODO"  [2026-03-19 Wed 14:30]
  :END:
```

Press `C-c C-t d` when finished:
```org
* DONE Implement ZK proof verifier :weids:
  CLOSED: [2026-03-19 Wed 17:00]
  SCHEDULED: <2026-03-20 Fri>
  :LOGBOOK:
  - State "DONE"        from "IN-PROGRESS" [2026-03-19 Wed 17:00]
  - State "IN-PROGRESS" from "TODO"        [2026-03-19 Wed 14:30]
  :END:
```

---

## Capture (Quick Entry)

Press `C-c c` from anywhere in Emacs to open the capture menu.

### Available templates

| Key | Name | Goes to | Tags |
|-----|------|---------|------|
| `t` | Quick TODO | `agenda/tasks.org > Inbox` | -- |
| `w t` | Weids TODO | `weids/tasks.org > Inbox` | `:weids:` |
| `w n` | Weids Note | `weids/notes.org > Notes` | `:weids:note:` |
| `w m` | Weids Meeting | `weids/tasks.org > Inbox` | `:weids:meeting:` |
| `z t` | Zynerise TODO | `zynerise/tasks.org > Inbox` | `:zynerise:` |
| `z n` | Zynerise Note | `zynerise/notes.org > Notes` | `:zynerise:note:` |
| `z m` | Zynerise Meeting | `zynerise/tasks.org > Inbox` | `:zynerise:meeting:` |
| `n` | Note | `agenda/notes.org > Notes` | `:note:` |
| `j` | Journal | `journal.org` (date-tree) | -- |
| `a` | Appointment | `gcal.org` | -- |

### Using capture

1. `C-c c` -- opens template selector
2. Press the key (e.g., `w t` for a Weids TODO)
3. Type your entry
4. `C-c C-c` -- save and file it
5. `C-c C-k` -- abort

Meeting templates (`w m`, `z m`) auto-start the clock so meeting time is
tracked.

---

## Agenda Views

Open the dispatcher with `C-c a`, then pick a view:

| Key | View | Description |
|-----|------|-------------|
| `d` | Dashboard | Today's agenda + In Progress + Waiting + Backlog |
| `W` | Weids Tasks | All active Weids tasks |
| `Z` | Zynerise Tasks | All active Zynerise tasks |
| `w` | Weekly Review | Week calendar + completed items |
| `t` | All TODOs | Every TODO across all projects |
| `a` | Built-in Agenda | Standard weekly/daily view |

### Navigating inside the agenda buffer

| Key | Action |
|-----|--------|
| `RET` | Jump to the org entry |
| `t` | Change TODO state |
| `I` | Clock in |
| `O` | Clock out |
| `s` | Schedule |
| `d` | Set deadline |
| `f` / `b` | Forward / backward in time |
| `d` / `w` / `m` | Day / week / month view |
| `r` | Refresh |
| `R` | Toggle clock report overlay |
| `l` | Toggle log mode (show clocked tasks) |
| `q` | Quit agenda |

---

## Scheduling & Deadlines

### Schedule (when to START working)

```
C-c s   (or C-c C-s)
```

```org
* TODO Write proposal :zynerise:
  SCHEDULED: <2026-03-20 Fri>
```

Shows in agenda on that date. Nags daily until completed.

### Deadline (when it must be FINISHED)

```
C-c d   (or C-c C-d)
```

```org
* TODO Submit grant application :weids:
  DEADLINE: <2026-04-01 Wed>
```

Warnings appear in the agenda starting 14 days before.

### Appointments (specific date & time)

Use `C-c .` to insert an active timestamp:

```org
* Team sync call :zynerise:meeting:
  <2026-03-20 Fri 14:00-15:00>
```

### Recurring tasks

Add a repeater after the date:

```org
* TODO Weekly standup :zynerise:
  SCHEDULED: <2026-03-20 Fri +1w>
```

| Repeater | Meaning |
|----------|---------|
| `+1d`, `+1w`, `+1m`, `+1y` | Shift from original date |
| `++1w` | Shift from today |
| `.+1w` | Shift from when marked DONE |

---

## Time Tracking (Clocking)

### Core commands

| Key | Action |
|-----|--------|
| `C-c C-x C-i` | Clock IN on current heading |
| `C-c C-x C-o` | Clock OUT |
| `C-c C-x C-x` | Re-clock the last task |
| `C-c C-x C-q` | Cancel clock (discard time) |
| `C-c C-x C-j` | Jump to the currently clocked task |
| `C-c C-x C-d` | Show time totals in buffer |
| `C-c C-x o` | Force clock-out any running clock (`+org/clock-out-all`) |

### Force clock-out (forgot to stop)

```
C-c C-x o       or       M-x +org/clock-out-all
```

This stops any running clock globally. Use it when you forgot to clock out.

### Idle time detection

If you've been idle for 15 minutes while a clock is running, Emacs asks:

- **Keep**: Count idle time as work
- **Subtract**: Clock out at the moment you went idle
- **Cancel**: Discard the entire clock entry

### What clocked time looks like

```org
* IN-PROGRESS Build auth module :zynerise:
  :LOGBOOK:
  CLOCK: [2026-03-19 Wed 09:00]--[2026-03-19 Wed 10:30] =>  1:30
  CLOCK: [2026-03-18 Tue 14:00]--[2026-03-18 Tue 15:45] =>  1:45
  :END:
```

### Typical daily workflow

1. `C-c a d` -- open Dashboard
2. Pick a task, `RET` to jump to it
3. `C-c C-x C-i` -- clock in
4. Work on it
5. `C-c C-x C-o` -- clock out
6. `C-c C-t i` (IN-PROGRESS) or `C-c C-t d` (DONE)
7. Pick next task, repeat
8. End of day: `C-c C-x o` to force-stop any forgotten clock

---

## Clock Reports

### Inline clock table

Insert this block in any org file, then `C-c C-c` on it to generate:

```org
#+BEGIN: clocktable :scope file :maxlevel 3
#+END:
```

### Useful options

| Option | Example | Description |
|--------|---------|-------------|
| `:scope` | `file`, `subtree`, `agenda` | What to report on |
| `:maxlevel` | `3` | Heading depth |
| `:block` | `today`, `thisweek`, `lastweek` | Time period |
| `:tstart`/`:tend` | `"2026-03-01"` | Custom date range |
| `:fileskip0` | `t` | Skip files with 0 time |

### Cross-project weekly report

```org
#+BEGIN: clocktable :scope agenda :maxlevel 3 :block thisweek :fileskip0 t
#+END:
```

### Clock report in agenda

Press `R` in the agenda buffer to toggle the clock report overlay.

---

## Tags

Set tags with `C-c C-q` on a heading. Capture templates auto-apply project
tags.

| Tag | Key | Purpose |
|-----|-----|---------|
| `weids` | `w` | Weids project |
| `zynerise` | `z` | Zynerise project |
| `personal` | `p` | Personal |
| `meeting` | `m` | Meeting |
| `deadline` | `d` | Hard deadline |
| `idea` | `i` | Idea / brainstorm |
| `bug` | `b` | Bug fix |

Note: `#+FILETAGS: :weids:` at the top of `weids/tasks.org` means every
heading inherits `:weids:` automatically.

---

## Google Calendar Sync (org-gcal)

Two-way sync between Google Calendar and `~/org/gcal.org`.

### Step 1: Create Google Cloud credentials

1. Go to [Google Cloud Console](https://console.cloud.google.com)
2. Create a new project (e.g., "Emacs Org Sync")
3. **APIs & Services > Library** -- search "Google Calendar API", enable it
4. **APIs & Services > Credentials** -- click **Create Credentials > OAuth client ID**
5. If prompted, configure the **OAuth consent screen**:
   - User type: **External**
   - App name: "Emacs Org"
   - Add your email as a test user
6. Create **OAuth client ID**:
   - Application type: **Desktop app**
7. Copy the **Client ID** and **Client Secret**

### Step 2: Configure Emacs

Create `~/.emacs.d/secrets.el` (this file is gitignored) with your credentials:

```elisp
(setq org-gcal-client-id     "123456-abcdef.apps.googleusercontent.com"
      org-gcal-client-secret "GOCSPX-your-secret-here")
```

`init-org.el` loads this file automatically before org-gcal starts. If the
file is missing, a warning is printed and org-gcal won't authenticate.

The calendar ID is set to `awang@weids.dev`. To add more calendars,
edit `init-org.el`:

```elisp
(setq org-gcal-fetch-file-alist
      '(("awang@weids.dev" . "~/org/gcal.org")
        ("other-id@group.calendar.google.com" . "~/org/gcal-other.org")))
```

Find the calendar ID in Google Calendar: **Settings > (your calendar) >
Integrate calendar > Calendar ID**.

### Step 3: First sync

1. Open Emacs and run `M-x org-gcal-fetch`
2. A Google authorization URL is displayed in the minibuffer — copy it to
   your browser
3. Log in and authorize the app
4. Copy the `code=` parameter from the redirect URL back to Emacs
5. Events are pulled into `~/org/gcal.org`

Auto-sync is enabled: org-gcal fetches on startup (after 10s idle) and
runs a two-way sync every 15 minutes.

### Sync commands

| Command | Action |
|---------|--------|
| `M-x org-gcal-fetch` | Pull events from Google → local |
| `M-x org-gcal-sync` | Two-way sync (fetch + push) |
| `M-x org-gcal-post-at-point` | Push heading at point to Google |
| `M-x org-gcal-delete-at-point` | Delete event at point from Google |

Batch scripts (`scripts/gcal-fetch.sh`, `scripts/gcal-sync.sh`,
`scripts/gcal-auth.sh`) still exist for CLI use but are no longer required
— all commands work natively in interactive Emacs.

### Creating events from Emacs

`C-c c a` (Appointment) -- creates an entry in `gcal.org`. Then run
`M-x org-gcal-sync` to push it to Google.

### Automatic push from task files

When you schedule a task with a **time** (e.g., `<2026-03-20 Fri 14:00>`),
a mirror entry is automatically created in `gcal.org` and posted to Google
Calendar. Tasks with only a date (no time) stay local.

**Flow:**

1. `C-c s` (schedule) → enter date+time → mirror entry created in `gcal.org`
   with `:calendar-id:` and `:org-gcal:` drawer → posted to Google Calendar
2. Work on the task, clocking in/out as you go
3. `C-c C-t d` (mark DONE) → the `gcal.org` mirror is updated with actual
   clock range (earliest clock-in to latest clock-out) → re-posted to Google

If no clock data exists when marked DONE, the original scheduled time is kept.

### Cleanup (C-c f)

Press `C-c f` in any task file to:

1. **Refile** all DONE/CANCELLED tasks from **Inbox** to **Completed**
2. **Normalize formatting** -- collapse excessive blank lines, ensure
   consistent spacing between headings

The Completed heading is created automatically if it doesn't exist.

---

## ICS Export for Thunderbird

Export all tasks/events to `~/org/calendar.ics` for calendar app subscription.

### Manual export

```
M-x +org/export-ics
```

### Auto-export

ICS is auto-exported after every `org-gcal` sync. For manual task changes,
run `M-x +org/export-ics`.

### Subscribing in Thunderbird

**Option A: Local file**

1. Thunderbird > **Calendar > New Calendar > On My Computer**
2. Format: ICS, browse to `~/org/calendar.ics`
3. Name: "Org Agenda"

**Option B: Local HTTP (auto-refresh)**

```bash
cd ~/org && python3 -m http.server 8042 &
```

Then in Thunderbird: **New Calendar > On the Network** >
`http://localhost:8042/calendar.ics`, set refresh interval.

**Option C: Google Calendar as bridge (recommended)**

Since org-gcal syncs to Google, subscribe Thunderbird to Google's feed:

1. Google Calendar > **Settings > (calendar) > Integrate calendar**
2. Copy **Secret address in iCal format**
3. Thunderbird: **New Calendar > On the Network**, paste the URL

This is the most seamless -- Emacs syncs to Google, Thunderbird subscribes to
Google.
