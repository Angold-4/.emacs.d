# Emacs Configuration

A modular, lightweight Emacs configuration for Emacs 30+ targeting code review,
note-taking with Org-mode, and a unified buffer experience with Evil mode.

## Overview

This configuration was refactored from a monolithic setup into a clean, modular
architecture. The key insight is that **LSP should be on-demand** - you don't
need a language server running just to read code.

### What Makes This Different

| Feature | This Config | Typical Config |
|---------|-------------|----------------|
| LSP Activation | Manual (`C-x l`) | Auto-enabled |
| Tree-sitter | Emacs 30 built-in | External package |
| Startup Time | ~1-2 seconds | Often 3-5+ seconds |
| Keybindings | Shared vocabulary + mode-local maps | Ad hoc per package |
| Theme Toggle | `M-x +theme/toggle` | Usually manual |

### Package Summary (~58 packages)

| Category | Packages | Purpose |
|----------|----------|---------|
| **Core** | straight.el, use-package | Package management |
| **Evil** | evil, evil-collection, key-chord | Vim emulation |
| **Completion** | vertico, orderless, marginalia, company, yasnippet | Fuzzy minibuffer + in-buffer completion |
| **LSP** | lsp-mode, lsp-ui | Language servers (on-demand) |
| **Languages** | rust-mode, go-mode, typescript-mode, etc. | Language support |
| **Syntax** | Built-in tree-sitter (Emacs 30) | Highlighting |
| **Org** | org-bullets, htmlize | Note-taking & export |
| **Tools** | magit, projectile, treemacs | Development |
| **UI** | ligature, rainbow-delimiters | Visual polish |

## Design Principles

1. **Lightweight** - LSP is on-demand (not auto-enabled), tree-sitter for syntax highlighting
2. **Unified** - Everything is a buffer, navigable with hjkl via Evil mode
3. **Modular** - Each feature in its own file under `core/`
4. **Easy Setup** - Clone, open Emacs, packages install automatically

## Quick Start

```bash
# Clone this repo
git clone https://github.com/Angold-4/.emacs.d ~/.emacs.d

# Start Emacs (packages will install on first run)
emacs
```

On first startup, straight.el will bootstrap and install all packages.
This takes 2-3 minutes. Subsequent starts are fast (~1-2 seconds).

### Post-Install Steps

```
;; Install tree-sitter grammars (optional, for better syntax highlighting)
M-x +treesit/install-all-grammars

;; If you want to verify LSP works
;; Open a .rs/.go/.ts file, then:
C-x l   ; Start LSP manually
```

## Module Structure

```
~/.emacs.d/
├── early-init.el          # GC optimization, disable package.el
├── init.el                 # Entry point, loads modules
├── core/
│   ├── init-core.el        # Macros and utilities
│   ├── init-straight.el    # Package management
│   ├── init-ui.el          # Fonts, ligatures, fringes
│   ├── init-themes.el      # Dark/light theme toggle
│   ├── init-evil.el        # Evil and global keybindings
│   ├── init-completion.el  # Vertico, Orderless, Company
│   ├── init-lsp.el         # LSP-mode on demand
│   ├── init-languages.el   # Language modes and tree-sitter
│   ├── init-org.el         # Org-mode configuration
│   ├── init-tools.el       # Projectile, Treemacs, terminal tools
│   ├── init-git.el         # Magit ownership and Git review dispatch
│   ├── init-git-store.el   # Canonical repositories and local contexts
│   ├── init-git-sync.el    # Durable shared mirror synchronization
│   ├── init-git-ui.el      # Evil review buffers, Changes Tree, diffs
│   ├── init-forge.el       # Forge cache and authentication adapter
│   ├── init-git-pr.el      # Cached pull-request workspace
│   └── init-agent-shell.el # OpenCode / Claude Code as native Emacs buffers
└── themes/
    ├── noctilux-theme.el       # Dark theme (existing)
    └── minimal-light-theme.el  # Light theme (new)

```


## Keybindings

### Navigation (Evil Normal Mode)

| Key | Action |
|-----|--------|
| `h/j/k/l` | Standard Vim movement |
| `H` | Beginning of line |
| `L` | End of line |
| `J` | Move down 8 lines |
| `K` | Move up 8 lines |
| `C-h/j/k/l` | Window navigation (left/down/up/right) |
| `jk` | Exit insert mode (chord) |

### Files & Buffers

| Key | Action |
|-----|--------|
| `C-p` | Find file in project (fuzzy match via Vertico) |
| `C-c C-f` | Search with ripgrep |
| `C-c r` | Rename buffer |
| `C-x b` | Switch buffer (persp-aware) |
| `C-x g` | Magit status |
| `C-c g` | Git/code-review dispatch |

The Git workbench supports local and cached pull-request review, a collapsible
Changes Tree, persistent reviewed checkmarks, and explicit offline-capable
synchronization. See [git.md](git.md) for the daily workflow and
[docs/git.md](docs/git.md) for the architecture.

### AI Agents (OpenCode / Claude Code)

OpenCode and Claude Code run as **native Emacs buffers**, not terminals, via
[agent-shell](https://github.com/xenodium/agent-shell) over the Agent Client
Protocol. The transcript is ordinary buffer text, so every Vim habit already
works: `j`/`k`/`h`/`l`, `J`/`K`, `G`, visual selection, `yy`, search, and
narrowing all copy and move across the whole conversation — no vterm, no
alternate screen, no key forwarding.

Launch:

| Key | Action |
|-----|--------|
| `C-c o` | OpenCode session (`opencode acp`) |
| `C-c O` | Claude Code session (`claude-agent-acp`) |
| `M-x ashell` | Default agent (OpenCode) |
| `M-x agent-shell` | Pick any ACP agent found on PATH |

Inside a session (`C-c k`):

| Key | Action |
|-----|--------|
| `C-c k m` | Switch model |
| `C-c k r` | Resume the most recent session in this workspace |
| `C-c k R` | Pick a session in this workspace to resume |
| `C-c k n` / `C-c k s` | New session / switch shells |
| `C-c k i` | Interrupt |

The same commands remain on the global `C-c A` prefix.

**Top bar.** agent-shell's own SVG header and mode-line copy are disabled.
Each session shows a compact Emacs header line with just what matters here:
the current model, the PR id (from the git workbench's cached forge
snapshots), the branch, and the context window left.

**Evil.** Sessions start in insert state. `RET` in insert adds a newline;
`C-<return>` or `M-RET` sends the prompt. In normal state `RET` sends it, and
the usual motions, visual selection, `G` and `yy` work over the transcript.

**Slash commands.** ACP exposes only the agent's *skills* as `/commands`
(see the "Available /commands" section in the shell). OpenCode advertises
`/delegate`, `/review`, `/init`, and friends, but its TUI commands such as
`/sessions`, `/models` and `/new` are not part of ACP and cannot be typed
here — use the `C-c k` bindings above. Note that ACP scopes `session/list` to
the workspace directory, so `r`/`R` only see sessions in the current
workspace.

OpenCode is the default agent and reuses `opencode auth login`; Claude Code
reuses the `claude` CLI's login. Claude Code needs its ACP bridge installed
once:

```bash
npm install -g @agentclientprotocol/claude-agent-acp
```

### LSP & Code

| Key | Action |
|-----|--------|
| `C-x l` | **Start LSP** for current buffer |
| `C-c .` | Show documentation (eldoc) |
| `M-l` | Code actions |
| `M-/` | Find type definition |
| `M-?` | Find references |
| `C-c f` | Format buffer |
| `C-c e n/p` | Next/previous error |

### Org-mode

| Key | Action |
|-----|--------|
| `C-c l` | Store link |
| `C-c a` | Org agenda |
| `C-c d` | Set deadline |
| `C-c s` | Schedule |
| `TAB` | Cycle visibility |

### Theme

| Command | Action |
|---------|--------|
| `M-x +theme/toggle` | Toggle dark/light theme |
| `M-x +theme/load-dark` | Load dark theme (noctilux) |
| `M-x +theme/load-light` | Load light theme (minimal-light) |

### Other

| Key | Action |
|-----|--------|
| `s-`` | Select treemacs window |
| `C-x t t` | Toggle treemacs |
| `M-RET` | Toggle fullscreen |
| `C-y` (visual) | Copy to system clipboard |

## Theme

Two themes are included:

- **noctilux** (dark) - Default, pure black background (#000000)
- **minimal-light** (light) - Pure white background, blue/yellow syntax

Toggle with `M-x +theme/toggle`.

## LSP Support

LSP is **not** enabled by default. Press `C-x l` to start LSP for the current buffer.

Supported language servers (install separately):
- **C/C++**: clangd
- **Rust**: rust-analyzer
- **Go**: gopls
- **Python**: pyright or pylsp
- **TypeScript/JavaScript**: typescript-language-server

## Dependencies

### Required
- Emacs 30+
- Git

### Recommended
- ripgrep (`rg`) - for fast project search
- fd - for fast file finding

### Language Servers (optional)
```bash
# Rust
rustup component add rust-analyzer

# Go
go install golang.org/x/tools/gopls@latest

# Python
pip install pyright

# TypeScript/JavaScript
npm install -g typescript-language-server typescript

# C/C++
# Ubuntu/Debian
sudo apt install clangd
# macOS
brew install llvm
```

### Formatters (optional)
```bash
# Rust
rustup component add rustfmt

# Python
pip install black

# JavaScript/TypeScript
npm install -g prettier

# C/C++
# Usually comes with clang
```

## Customization

### Changing the font

Edit `init-ui.el`:
```elisp
(defvar +ui/default-font "Your Font Name")
(defvar +ui/default-font-size 130)  ; 130 = 13pt
```

### Org files location

Edit `init-org.el`:
```elisp
(setq org-directory "~/your/org/path/")
(setq org-agenda-files '("~/your/org/path/"))
```

### Adding new modules

Create a new file in `core/` (e.g., `init-mymodule.el`) and add it to the
`+init-modules` list in `init.el`.

## Troubleshooting

### Tree-sitter version mismatch
```
Cannot activate tree-sitter, because language grammar for X is unavailable (version-mismatch)
```

Run `M-x +treesit/install-all-grammars` to reinstall grammars.

### Slow startup

- Check startup time with the message: "Emacs loaded in X seconds"
- Native compilation should be enabled (check `native-comp-available-p`)
- Ensure you're not auto-starting LSP (it should only start with `C-x l`)

### Package issues

```bash
# Remove and reinstall straight.el
rm -rf ~/.emacs.d/straight
# Then restart Emacs
```

## License

MIT
