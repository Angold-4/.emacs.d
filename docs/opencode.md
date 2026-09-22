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
| `C-c m n` | `+opencode/new` | New session |
| `C-c m M` | `+opencode/model` | Select model |
| `C-c m v` | `+opencode/variant` | Select model variant |
| `C-c m s` | `+opencode/save` | Save this session as an org file |
| `C-c m d` | `+opencode/open-directory` | Dired the saved-sessions directory |

`:opencode` / `:oc` (Evil ex) open the global list. Set `+opencode-prefix` to
something outside `C-c`, e.g. `s-o`, if preferred.

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
