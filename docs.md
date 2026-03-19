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
| `C-c f` | Format buffer (LSP formatter, or language-specific fallback) |

### Supported Language Servers

| Language | Server | Notes |
|----------|--------|-------|
| C/C++ | `clangd` | Needs `compile_commands.json` |
| Rust | `rust-analyzer` | Custom workspace root detection |
| Go | `gopls` | |
| Python | `pyright` / `pylsp` | |
| TypeScript/JS | `typescript-language-server` | |

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

## Magit (Git)

| Key | Action |
|-----|--------|
| `C-x g` | Magit status (main entry point) |
| `C-x M-g` | Magit dispatch (menu of all git commands) |

### Inside Magit Status Buffer

| Key | Action |
|-----|--------|
| `s` | Stage hunk/file |
| `u` | Unstage hunk/file |
| `c c` | Commit |
| `P p` | Push |
| `F p` | Pull |
| `TAB` | Expand/collapse diff hunks |
| `d d` | Show diff |
| `l l` | Show log |
| `b b` | Switch branch |
| `g` | Refresh |
| `0` | Discard change |
| `q` | Quit magit buffer |

> **Note:** Git blame is available via `M-x magit-blame-addition` but has no keybinding configured.

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
| `C-c t` | Toggle eshell at bottom |
| `C-c T` | Open new eshell |
| `C-p` / `Up` | Previous input (insert mode) |
| `C-n` / `Down` | Next input (insert mode) |
| `C-r` | Search history backwards (insert mode) |

### Vterm

| Key | Action |
|-----|--------|
| `C-c v` | Toggle vterm at bottom |
| `C-c V` | Open new vterm |

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
| `w m` | Weids Meeting | `weids/tasks.org > Meetings` | `:weids:meeting:` |
| `z t` | Zynerise TODO | `zynerise/tasks.org > Inbox` | `:zynerise:` |
| `z n` | Zynerise Note | `zynerise/notes.org > Notes` | `:zynerise:note:` |
| `z m` | Zynerise Meeting | `zynerise/tasks.org > Meetings` | `:zynerise:meeting:` |
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
