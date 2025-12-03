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
| Keybindings | Centralized in one file | Scattered everywhere |
| Theme Toggle | `M-x +theme/toggle` | Usually manual |

### Package Summary (~55 packages)

| Category | Packages | Purpose |
|----------|----------|---------|
| **Core** | straight.el, use-package | Package management |
| **Evil** | evil, evil-collection, key-chord | Vim emulation |
| **Completion** | company, yasnippet | Auto-completion |
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
├── early-init.el          # GC optimization, disable package.el (66 lines)
├── init.el                 # Entry point, loads modules (141 lines)
├── core/
│   ├── init-core.el        # Macros, utilities (215 lines)
│   ├── init-straight.el    # Package management (63 lines)
│   ├── init-ui.el          # Fonts, ligatures, fringes (180 lines)
│   ├── init-themes.el      # Dark/light theme toggle (157 lines)
│   ├── init-evil.el        # Evil + ALL keybindings (230 lines)
│   ├── init-completion.el  # Company mode (86 lines)
│   ├── init-lsp.el         # LSP-mode on-demand (163 lines)
│   ├── init-languages.el   # Language modes, tree-sitter (288 lines)
│   ├── init-org.el         # Org-mode config (166 lines)
│   └── init-tools.el       # Magit, Projectile, etc. (221 lines)
└── themes/
    ├── noctilux-theme.el       # Dark theme (existing)
    └── minimal-light-theme.el  # Light theme (new)

Total: ~1,976 lines of well-documented Elisp
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
| `C-p` | Find file in project (Projectile) |
| `C-c C-f` | Search with ripgrep |
| `C-c r` | Rename buffer |
| `C-x b` | Switch buffer (persp-aware) |
| `C-x g` | Magit status |

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
