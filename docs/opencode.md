# OpenCode in Emacs

`init-opencode.el` runs OpenCode headless (`opencode serve`) and drives it with
[sczi/opencode.el](https://codeberg.org/sczi/opencode.el) from ordinary Emacs
buffers.

## Principle

We are **not** re-implementing an agent in Emacs. OpenCode owns the agents,
models and sessions (and is itself the adaptor to Claude Code, Codex, and the
rest); Emacs is only the buffer layer. Everything is a buffer: the session
manager is a vtable, a session is a comint buffer, a saved session is an org
file.

## Keys

`+opencode-prefix` (default `C-c m`) is the single prefix, bound globally and
inside OpenCode buffers:

| Key | Command | Purpose |
|-----|---------|---------|
| `C-c m m` | `+opencode/sessions` | Global session list, every project |
| `C-c m o` | `+opencode/open` | Session manager for the current project |
| `C-c m i` | `+opencode/input` | Compose in a dedicated input buffer |
| `C-c m c` | `+opencode/new` | New session in this workspace |
| `C-c m M` | `+opencode/model` | Select model (flattened across providers) |
| `C-c m a` | `+opencode/provider` | Select provider, then a model from it |
| `C-c m v` | `+opencode/variant` | Select model variant |
| `C-c m s` | `+opencode/save` | Save this session as an org file |
| `C-c m d` | `+opencode/open-directory` | Dired the saved-sessions directory |

`:opencode` / `:oc` (Evil ex) open the global list. Set `+opencode-prefix` to
something outside `C-c`, e.g. `s-o`, if preferred.

## Editing vs sending (Evil)

The package leaves a session buffer in Emacs state, so every key reaches comint
and `RET` sends the moment it is typed. Here it is an ordinary Evil buffer
instead:

- sessions open in **insert state** — type and edit freely; `RET` inserts a
  newline, so a prompt can be several lines.
- **normal state** is for reading, motions, visual selection, and sending:
  `RET` (or `C-<return>`) sends the input to the agent.
- `C-<return>` in insert state also sends, for one-keystroke submit.

The session manager (vtable) starts in normal state so `j`/`k` and the
package's own bindings work.

## Connecting

The package reads `OPENCODE_SERVER_USERNAME` / `OPENCODE_SERVER_PASSWORD` only
when it *starts* a server, so connecting to one that is already running (a
shell with the password exported, or a previous Emacs) answers
`/global/health` with 401 and autoconnect refuses.

Credentials are resolved before every connect attempt, in order:

1. `+opencode-server-password` (override; set it only if the rest somehow fail)
2. the process environment
3. **the environment of the server already listening on `opencode-port`**,
   read with `lsof` + `ps -Eww`, which works even when Emacs never had the
   variable (a daemon, or a GUI launch)

On macOS the capital `E` matters — `ps -eww` does not show the environment —
and `lsof` lives in `/usr/sbin`, which is why the module adds it to
`exec-path`.

## Reasoning

Reasoning/thinking blocks are hidden by default (`+opencode-show-reasoning`,
nil). The package has no option for this, so the trace is dropped at
`opencode--insert-reasoning-block` and `opencode--render-region` skips the
`reasoning` type. Set the option to `t` to show it again.

## Input buffer

`C-c m i` (`+opencode/input`) focuses a plain buffer, one per session, for
writing a prompt away from the transcript. It is shown **and focused**
automatically beneath each session (`opencode-open-session` is advised), so a
session opens with the cursor in the input box, ready to type. It is an ordinary Evil buffer (insert state to write, normal `RET` or
`C-<return>` / `C-c C-c` to send) and it never shares a buffer with streaming
output, so you can compose at any time, including while the agent is working.
Sending calls the package's own `opencode-session--send-synthetic-input`, so
the session's agent, model and context are used unchanged; the buffer is
cleared on success.

## New sessions and naming

`C-c m c` starts **composing** a session in the current workspace — the current
session's directory, else the project root. In the global list, `c` does the
same, and `RET`/`o` opens the session at point.

The OpenCode session is **created only on the first send**, so an empty session
is never created server-side or written to disk. That is why `C-c m c` opens an
input buffer with no session behind it yet.

The server owns the `ses_…` id; we control the readable parts:

- **title**: `<date> <first 10 chars of branch>` — e.g. `2026-09-22 feat/openc`.
  A new session is created with this title, so the session list and the session
  buffer name read sensibly.
- **org file**: `<date>-<branch10>-<last 6 of id>.org` under the project
  directory, with `#+opencode_title` alongside the id/directory/branch/saved
  metadata.

## Resuming a session

Opening a session replays its transcript, and the package replays **all** of
it synchronously — a 511-message session is a 7 MB fetch plus 511 markdown
renders, i.e. ~20 s. `+opencode-replay-limit` (default 40) caps the render to
the newest messages and says how many were omitted; set it to nil for the full
transcript. The server keeps the complete history, and the session's org file
has all of it, so nothing is lost.

(The mode-line strings are escaped for `%`, since a literal percent in a
mode-line string must be `%%`.)

## Global session list

`C-c m m` aggregates `/project` and `/session` into one vtable with **Source ·
Project · Branch · Updated · Title**, appending the archived org files below the
live sessions. The server scopes `session/list` to the `x-opencode-directory`
header, so one request is made per project worktree; worktrees share a
repository, so the results are de-duplicated by session id. `g` refreshes.

## Sessions as files

Each session has one **merged** org file under
`+opencode-sessions-directory` (default `~/.emacs.d/opencode-sessions/`), named
for its title plus the last six characters of its id. It is written
automatically when a turn completes, and `C-c m s` rewrites it on demand:

```org
#+title: 2026-09-22 feat/openc
#+opencode_id: ses_...
#+opencode_title: 2026-09-22 feat/openc
#+opencode_directory: /path/to/project/
#+opencode_branch: feat/opencode-emacs
#+opencode_updated: 2026-09-22 16:01

* Prompt 2026-09-22 16:00  [opencode/big-pickle]
what does this do?

* Response 2026-09-22 16:01  [opencode/big-pickle:high]
it does X
- tool: bash completed
```

One file per session, so it persists, commits, and can be sent to another
machine. Reasoning is omitted; tool calls are one line each.

## Archived sessions

`C-c m m` lists both **live** sessions (from OpenCode) and **archived** ones
(the org files), in a `Source` column:

| Source | `RET` / `o` | `C` | `x` |
|---|---|---|---|
| `live` | resume via the API | — | delete the server session |
| `file` | visit the org file | **continue**: open an input buffer seeded with the transcript, then `RET` creates a new session | — |

A session is read from OpenCode while it is live, and from its file when it is
not. `+opencode/continue-from-file` is the portability path: it seeds a new
session from an archived file rather than pretending to restore the old one.

## Layout and mode-line bars

`C-c m c` opens the input buffer as an ordinary buffer in a window — full
until a session exists — rather than a forced bottom strip. Nothing is split
until the first send: only then does the session appear, as the window below
the input, and the two are arranged once (other windows are untouched).

There is no header line. Each buffer carries its information in the
**mode line** (the bottom bar):

- **input** (`*OpenCode Input*`): the model it will send with (the session's
  model when live, else the remembered one) and the session title, then
  `draft` or `live`.
- **output** (a session): the model and variant, the context window **used**
  (`ctx 795k/1049k (76%)`), and the busy/idle status.

The context figure is per turn, not the session lifetime: it is the latest
assistant message's `input + cache.read + cache.write`, which is the prompt
that turn actually sent. It is divided by the **model that turn ran on**
(resolved from the catalog by the message's `providerID`/`modelID`), *not* by
whatever model is currently selected — a session on a 1M-context model would
otherwise be measured against a 200k one. A zero-token update is ignored so
the figure cannot collapse to 0.

## Roadmap

- Import a saved org file back into a new OpenCode session.
- Evil-native bindings inside sessions (the package ships its own `C-c` map).
- Surface branch/PR next to the session title, as the git workbench already
  knows them.
- Optional: make the org file the durable record for a session and sync it on
  turn end.
