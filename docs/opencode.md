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
| `C-c m n` | `+opencode/new` | New session |
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

## Reasoning

Reasoning/thinking blocks are hidden by default (`+opencode-show-reasoning`,
nil). The package has no option for this, so the trace is dropped at
`opencode--insert-reasoning-block` and `opencode--render-region` skips the
`reasoning` type. Set the option to `t` to show it again.

## Input buffer

`C-c m i` (`+opencode/input`) opens a plain buffer, one per session, for
writing a prompt away from the transcript. It is an ordinary Evil buffer
(insert state to write, normal `RET` or `C-<return>` / `C-c C-c` to send) and
it never shares a buffer with streaming output, so you can compose at any time,
including while the agent is working. Sending calls the package's own
`opencode-session--send-synthetic-input`, so the session's agent, model and
context are used unchanged; the buffer is cleared on success.

## Global session list

`C-c m m` aggregates `/project` and `/session` into one vtable with **Project ·
Branch · Last Updated · Title**. The server scopes `session/list` to the
`x-opencode-directory` header, so one request is made per project worktree;
worktrees share a repository, so the results are de-duplicated by session id.
`RET`/`o` opens a session, `s` saves it, `x` deletes it, `g` refreshes.

## Sessions as files

`C-c m s` writes the current session to
`+opencode-sessions-directory` (default `~/.emacs.d/opencode-sessions/`) as an
org file under the project directory:

```org
#+title: OpenCode session ses_...
#+opencode_id: ses_...
#+opencode_directory: /path/to/project/
#+opencode_branch: feature/x
#+opencode_saved: 2026-09-21 16:00

<markdown transcript from the session messages>
```

The file persists, commits, and can be sent to another machine. Re-importing a
file back into OpenCode is deliberately out of scope for now.

## Roadmap

- Import a saved org file back into a new OpenCode session.
- Evil-native bindings inside sessions (the package ships its own `C-c` map).
- Surface branch/PR next to the session title, as the git workbench already
  knows them.
- Optional: make the org file the durable record for a session and sync it on
  turn end.
