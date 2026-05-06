;;; init-git.el --- Git diff review system -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Clean git diff review for AI-assisted coding workflows

;;; Commentary:
;; This module provides a read-only git diff review system optimized for
;; reviewing AI-generated code changes. It builds on top of magit to give
;; a GitHub-like diff viewing experience inside Emacs.
;;
;; Two main entry points:
;;   +git/review          — Review all unstaged + untracked changes (working tree)
;;   +git/review-staged   — Review staged changes only
;;   +git/review-commit   — Review a specific commit (prompts for commit)
;;
;; The review buffer is read-only and uses evil normal-state navigation.
;;
;; Keybindings in review buffer:
;; ┌─────────────────────────────────────────────────────────────────┐
;; │ Navigation                                                      │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ j/k         : Move line by line                                 │
;; │ J/K         : Move 8 lines (page-like scrolling)                │
;; │ n/p         : Jump to next/previous file section                │
;; │ N/P         : Jump to next/previous hunk                        │
;; │ gg/G        : Top/bottom of buffer                              │
;; │ H/L         : Beginning/end of line                             │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Actions                                                         │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ RET         : Open file at point in side window                 │
;; │ o           : Open file at point in other window                │
;; │ TAB         : Toggle section (expand/collapse file or hunk)     │
;; │             : Files start collapsed — TAB to expand             │
;; │ S-TAB       : Cycle all sections (expand/collapse all)          │
;; │ +/-         : Increase/decrease diff context lines              │
;; │ s           : Stage hunk or file at point                       │
;; │ u           : Unstage hunk or file at point                     │
;; │ x           : Discard hunk or file at point                     │
;; │ gr          : Refresh the review buffer                         │
;; │ q           : Quit the review buffer                            │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Visual                                                          │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ v           : Enter visual mode (select text for yank)          │
;; │ C-y         : Copy selection to tmux buffer (C-a P to paste)    │
;; │ /           : Search in buffer                                  │
;; └─────────────────────────────────────────────────────────────────┘
;;
;; Launch:
;;   C-c g r     : +git/review           (working tree changes)
;;   C-c g s     : +git/review-staged    (staged changes)
;;   C-c g c     : +git/review-commit    (specific commit)
;;   C-c g l     : +git/log-oneline      (compact git log)
;;   C-x g       : magit-status          (existing binding)

;;; Code:

;; =============================================================================
;; Clipboard: copy via tmux paste buffer
;; =============================================================================

;; Clipboard copy is handled by `+copy-to-system-clipboard' in init-core.el.
;; It writes to tmux's paste buffer via `tmux load-buffer -'.
;; In tmux, C-a P is bound to `paste-buffer' to paste into any pane.
;;
;; This approach works reliably over SSH without OSC 52 support:
;;   Emacs C-y (visual) → tmux paste buffer → tmux C-a P → any pane

;; =============================================================================
;; Enhanced Magit Diff Display
;; =============================================================================

;; These settings make magit diffs cleaner and more readable,
;; affecting both magit-status and our review buffers.

(with-eval-after-load 'magit
  ;; --- Diff display ---
  ;; Show word-level granularity on all hunks (not just selected)
  (setq magit-diff-refine-hunk 'all)

  ;; Start with a generous amount of context (like GitHub's default)
  (setq magit-diff-extra-context 6)

  ;; --- Initial visibility: collapse file sections ---
  ;; Start with all files collapsed so the review buffer is a compact
  ;; overview.  Press TAB to expand individual files, S-TAB to toggle all.
  (setq magit-section-initial-visibility-alist
        '((file . hide)
          (untracked . hide)))

  ;; Paint added/removed stats in the file header
  (setq magit-diff-paint-whitespace-lines 'both)

  ;; Adjust diff arguments for cleaner output
  (setq magit-diff-arguments '("--no-ext-diff"))

  ;; --- Status buffer: show everything ---
  ;; Show fine (word-level) diffs inline
  (setq magit-diff-refine-ignore-whitespace t)

  ;; Disabled: hand-rolled `magit-status-sections-hook' was a verbatim
  ;; copy of an older magit's default.  In current magit some of those
  ;; function names changed (e.g. `magit-insert-unpushed-to-upstream'
  ;; → `magit-insert-unpushed-to-upstream-or-recent'); calling a
  ;; non-existent function in `run-hooks' aborts buffer construction
  ;; mid-way, leaving the status buffer blank.  Magit's own default
  ;; already includes everything we want.
  ;; (setq magit-status-sections-hook
  ;;       '(magit-insert-status-headers
  ;;         ...))

  ;; --- Performance: limit diff size for huge repos ---
  (setq magit-diff-highlight-hunk-region-functions
        '(magit-diff-highlight-hunk-region-dim-outside
          magit-diff-highlight-hunk-region-using-face))

  ;; --- Better faces for diff readability ---
  ;; Background-only faces for added/removed/context lines.
  ;; No :foreground is set so that syntax highlighting (applied via the
  ;; `face' text property) can provide language-aware text colors on
  ;; top of the diff background tint.  This gives a GitHub-like look:
  ;; green/red backgrounds with properly colored code.
  (custom-set-faces
   ;; Added lines — subtle green tint (delta's bg is stripped; these control line color)
   '(magit-diff-added ((t (:background "#0a1a0a" :extend t))))
   '(magit-diff-added-highlight ((t (:background "#0f220f" :extend t))))
   ;; Removed lines — subtle red tint
   '(magit-diff-removed ((t (:background "#1a0a0a" :extend t))))
   '(magit-diff-removed-highlight ((t (:background "#220f0f" :extend t))))
   ;; Context (unchanged) — very subtle background
   '(magit-diff-context ((t (:foreground "#888888" :extend t))))
   '(magit-diff-context-highlight ((t (:foreground "#999999" :background "#111111" :extend t))))
   ;; File headers — prominent
   '(magit-diff-file-heading ((t (:foreground "#e0e0e0" :weight bold :extend t))))
   '(magit-diff-file-heading-highlight ((t (:foreground "#ffffff" :background "#1a1a2e" :weight bold :extend t))))
   ;; Hunk headers — dim blue
   '(magit-diff-hunk-heading ((t (:foreground "#8888bb" :background "#1a1a22" :extend t))))
   '(magit-diff-hunk-heading-highlight ((t (:foreground "#aaaadd" :background "#222233" :extend t))))
   ;; Section headers
   '(magit-section-heading ((t (:foreground "#c0a040" :weight bold))))
   '(magit-section-highlight ((t (:background "#0a0a0a"))))))

;; =============================================================================
;; Diff-hl: Show git changes in the fringe
;; =============================================================================

(use-package diff-hl
  :straight t
  :hook ((after-init . global-diff-hl-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Update indicators immediately (not just on save)
  (diff-hl-flydiff-mode 1)

  ;; Use margin instead of fringe in terminal
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

;; =============================================================================
;; Git Review Buffer — Core
;; =============================================================================

(defvar +git/review-context-lines 6
  "Number of context lines to show around each change.
Use +/- in the review buffer to adjust.")

(defun +git/review ()
  "Open a clean diff review buffer showing all working-tree changes.
This shows both unstaged changes and untracked files as diffs,
giving a complete picture of what the AI (or you) changed."
  (interactive)
  (require 'magit)
  (let ((default-directory (or (magit-toplevel)
                               (user-error "Not inside a Git repository"))))
    (magit-diff-working-tree "HEAD" '("--stat" "--no-ext-diff"
                                      "-U6"))))

(defun +git/review-staged ()
  "Open a clean diff review buffer showing staged changes only."
  (interactive)
  (require 'magit)
  (let ((default-directory (or (magit-toplevel)
                               (user-error "Not inside a Git repository"))))
    (magit-diff-staged nil '("--no-ext-diff" "-U6"))))

(defun +git/review-commit (&optional commit)
  "Open a clean diff review buffer for a specific COMMIT.
If COMMIT is nil, prompt for one."
  (interactive
   (progn (require 'magit)
          (list (magit-read-branch-or-commit "Review commit"))))
  (let ((default-directory (or (magit-toplevel)
                               (user-error "Not inside a Git repository"))))
    (magit-diff-range (format "%s~..%s" commit commit)
                      '("--no-ext-diff" "-U6"))))

(defun +git/log-oneline ()
  "Show a compact one-line-per-commit git log."
  (interactive)
  (require 'magit)
  (let ((default-directory (or (magit-toplevel)
                               (user-error "Not inside a Git repository"))))
    (magit-log-current '("--oneline" "--graph" "--decorate") '("-n100"))))

;; =============================================================================
;; Review Buffer Navigation & Actions
;; =============================================================================

(defun +git/open-file-at-point ()
  "Open the file at point in a side window (right split).
Jump to the exact line in the diff if possible."
  (interactive)
  (let ((file (magit-file-at-point))
        (line nil))
    (unless file
      ;; Try to get file from diff context
      (save-excursion
        (when (re-search-backward "^diff --git a/\\(.+\\) b/" nil t)
          (setq file (match-string 1)))))
    (unless file
      (user-error "No file at point"))
    ;; Try to extract the line number from the hunk header or diff line
    (save-excursion
      (let ((current-pos (point)))
        ;; Find the hunk header above us
        (when (re-search-backward "^@@ -[0-9,]+ \\+\\([0-9]+\\)" nil t)
          (let ((hunk-start-line (string-to-number (match-string 1))))
            ;; Count lines from hunk header to current position
            ;; (skip removed lines, count added and context lines)
            (forward-line 1)
            (let ((offset 0))
              (while (< (point) current-pos)
                (let ((char (char-after)))
                  (unless (eq char ?-)  ; Skip removed lines
                    (setq offset (1+ offset))))
                (forward-line 1))
              (setq line (+ hunk-start-line offset -1)))))))
    ;; Open in side window
    (let* ((root (magit-toplevel))
           (full-path (expand-file-name file root))
           (buf (find-file-noselect full-path)))
      (display-buffer buf '(display-buffer-in-direction
                            . ((direction . right)
                               (window-width . 0.5))))
      (let ((win (get-buffer-window buf)))
        (when win
          (select-window win)
          (when line
            (goto-char (point-min))
            (forward-line (1- line))
            (recenter)))))))

(defun +git/open-file-other-window ()
  "Open the file at point in other window."
  (interactive)
  (let ((file (magit-file-at-point)))
    (unless file
      (save-excursion
        (when (re-search-backward "^diff --git a/\\(.+\\) b/" nil t)
          (setq file (match-string 1)))))
    (unless file
      (user-error "No file at point"))
    (let* ((root (magit-toplevel))
           (full-path (expand-file-name file root)))
      (find-file-other-window full-path))))

(defun +git/increase-context ()
  "Show more context lines around each change."
  (interactive)
  (setq +git/review-context-lines (min 50 (+ +git/review-context-lines 3)))
  (message "Context: %d lines" +git/review-context-lines)
  (+git/refresh-with-context))

(defun +git/decrease-context ()
  "Show fewer context lines around each change."
  (interactive)
  (setq +git/review-context-lines (max 0 (- +git/review-context-lines 3)))
  (message "Context: %d lines" +git/review-context-lines)
  (+git/refresh-with-context))

(defun +git/refresh-with-context ()
  "Refresh the current diff buffer with updated context lines."
  (when (derived-mode-p 'magit-diff-mode)
    ;; Must setq the buffer-local variable (not let-bind) so the value
    ;; persists through the refresh and subsequent refreshes.
    (setq magit-buffer-diff-args
          (list "--stat" "--no-ext-diff"
                (format "-U%d" +git/review-context-lines)))
    (magit-refresh)))

(defun +git/next-file ()
  "Jump to the next file section in the diff."
  (interactive)
  (magit-section-forward-sibling)
  (recenter 3))

(defun +git/prev-file ()
  "Jump to the previous file section in the diff."
  (interactive)
  (magit-section-backward-sibling)
  (recenter 3))

(defun +git/next-hunk ()
  "Jump to the next hunk in the diff."
  (interactive)
  (magit-section-forward)
  (recenter 3))

(defun +git/prev-hunk ()
  "Jump to the previous hunk in the diff."
  (interactive)
  (magit-section-backward)
  (recenter 3))

;; =============================================================================
;; Evil Keybindings for Magit Diff Buffers
;; =============================================================================

(defun +git/setup-review-evil-keys ()
  "Setup Evil keybindings for magit diff review buffers.
Makes the diff buffer behave as a clean read-only review surface."
  (when (bound-and-true-p evil-mode)
    ;; -- Navigation --
    (evil-local-set-key 'normal (kbd "j") 'evil-next-line)
    (evil-local-set-key 'normal (kbd "k") 'evil-previous-line)
    (evil-local-set-key 'normal (kbd "J") '+evil/move-lines-down)
    (evil-local-set-key 'normal (kbd "K") '+evil/move-lines-up)
    (evil-local-set-key 'normal (kbd "H") 'evil-beginning-of-line)
    (evil-local-set-key 'normal (kbd "L") 'evil-end-of-line)
    (evil-local-set-key 'normal (kbd "gg") 'evil-goto-first-line)
    (evil-local-set-key 'normal (kbd "G") 'evil-goto-line)

    ;; -- File/hunk jumping --
    (evil-local-set-key 'normal (kbd "n") '+git/next-file)
    (evil-local-set-key 'normal (kbd "p") '+git/prev-file)
    (evil-local-set-key 'normal (kbd "N") '+git/next-hunk)
    (evil-local-set-key 'normal (kbd "P") '+git/prev-hunk)

    ;; -- Actions --
    (evil-local-set-key 'normal (kbd "RET") '+git/open-file-at-point)
    (evil-local-set-key 'normal (kbd "o") '+git/open-file-other-window)
    (evil-local-set-key 'normal (kbd "TAB") 'magit-section-toggle)
    (evil-local-set-key 'normal (kbd "<backtab>") 'magit-section-cycle-global)
    (evil-local-set-key 'normal (kbd "+") '+git/increase-context)
    (evil-local-set-key 'normal (kbd "=") '+git/increase-context)
    (evil-local-set-key 'normal (kbd "-") '+git/decrease-context)
    (evil-local-set-key 'normal (kbd "s") 'magit-stage)
    (evil-local-set-key 'normal (kbd "u") 'magit-unstage)
    (evil-local-set-key 'normal (kbd "x") 'magit-discard)
    (evil-local-set-key 'normal (kbd "gr") 'magit-refresh)
    (evil-local-set-key 'normal (kbd "q") 'magit-mode-bury-buffer)

    ;; -- Search --
    (evil-local-set-key 'normal (kbd "/") 'evil-search-forward)
    (evil-local-set-key 'normal (kbd "?") 'evil-search-backward)

    ;; -- Windmove --
    (evil-local-set-key 'normal (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'normal (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'normal (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'normal (kbd "C-k") 'windmove-up)

    ;; -- Visual state: clipboard copy --
    (evil-local-set-key 'visual (kbd "C-y") '+copy-to-system-clipboard)))

;; Apply to all magit diff-related modes
(dolist (hook '(magit-diff-mode-hook
                magit-revision-mode-hook))
  (add-hook hook #'+git/setup-review-evil-keys))

;; Also improve the magit-status buffer navigation
(defun +git/setup-status-evil-keys ()
  "Setup enhanced Evil keybindings for magit-status."
  (when (bound-and-true-p evil-mode)
    ;; Quick navigation
    (evil-local-set-key 'normal (kbd "J") '+evil/move-lines-down)
    (evil-local-set-key 'normal (kbd "K") '+evil/move-lines-up)
    (evil-local-set-key 'normal (kbd "H") 'evil-beginning-of-line)
    (evil-local-set-key 'normal (kbd "L") 'evil-end-of-line)

    ;; Open file in side window
    (evil-local-set-key 'normal (kbd "RET") '+git/open-file-at-point)
    (evil-local-set-key 'normal (kbd "o") '+git/open-file-other-window)

    ;; Context control
    (evil-local-set-key 'normal (kbd "+") '+git/increase-context)
    (evil-local-set-key 'normal (kbd "=") '+git/increase-context)
    (evil-local-set-key 'normal (kbd "-") '+git/decrease-context)

    ;; Windmove
    (evil-local-set-key 'normal (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'normal (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'normal (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'normal (kbd "C-k") 'windmove-up)

    ;; Visual state: clipboard copy
    (evil-local-set-key 'visual (kbd "C-y") '+copy-to-system-clipboard)))

(add-hook 'magit-status-mode-hook #'+git/setup-status-evil-keys)

;; =============================================================================
;; Global Keybindings — C-c g prefix
;; =============================================================================

(global-set-key (kbd "C-c g r") '+git/review)
(global-set-key (kbd "C-c g s") '+git/review-staged)
(global-set-key (kbd "C-c g c") '+git/review-commit)
(global-set-key (kbd "C-c g l") '+git/log-oneline)
(global-set-key (kbd "C-c g g") 'magit-status)

;; =============================================================================
;; Syntax Highlighting in Diff Buffers (via magit-delta)
;; =============================================================================

;; magit-delta pipes raw git diff output through the `delta` CLI tool
;; (https://github.com/dandavison/delta) which provides language-aware
;; syntax highlighting.  The ANSI-colored output is converted to Emacs
;; overlays via `xterm-color'.
;;
;; When active, magit-delta:
;;   - Overrides magit's diff faces (added/removed/context) with delta's colors
;;   - Disables magit-diff-refine-hunk (delta does its own word-level diffs)
;;   - Toggle on/off with M-x magit-delta-mode
;;
;; The custom diff faces defined above remain as fallback when magit-delta
;; is toggled off.

(use-package magit-delta
  :straight t
  :after magit
  ;; Only activate when the `delta' binary is actually on PATH.
  ;; Otherwise the wash-diffs hook errors with "file-missing" and the
  ;; status/diff buffer renders blank.  Install with: `brew install git-delta'.
  :if (executable-find "delta")
  :custom
  ;; Let magit-delta resolve `delta' via PATH instead of hardcoding it
  ;; (the previous "/home/awang/.cargo/bin/delta" was a Linux path that
  ;; doesn't exist on macOS).
  (magit-delta-default-dark-theme "Nord")
  (magit-delta-default-light-theme "GitHub")
  (magit-delta-hide-plus-minus-markers t)
  :hook
  (magit-mode . magit-delta-mode)
  :config
  ;; --- Fix 1: Undo face remapping ---
  ;; magit-delta remaps diff faces (added/removed/context) to `default',
  ;; which kills their full-line backgrounds.  We undo that remapping so
  ;; magit's own faces provide green/red line backgrounds (:extend t),
  ;; while delta's overlays add syntax colors on top.
  (defun +git/undo-delta-face-remap (&rest _)
    "Remove magit-delta's face remapping so diff backgrounds show."
    (setq face-remapping-alist
          (cl-remove-if (lambda (entry)
                          (memq (car entry)
                                '(magit-diff-context-highlight
                                  magit-diff-added
                                  magit-diff-added-highlight
                                  magit-diff-removed
                                  magit-diff-removed-highlight)))
                        face-remapping-alist)))
  (advice-add 'magit-delta-mode :after #'+git/undo-delta-face-remap)

  ;; NOTE: Attempted "Fix 2" (stripping delta overlay :background and
  ;; boosting dim :foreground via magit-refresh-buffer-hook) was removed.
  ;; The code executed but produced no visible improvement — likely due
  ;; to Emacs display-engine caching or overlay/face composition order.
  ;; Delta's own background colors are acceptable as-is.
  )  ; end use-package magit-delta

;; =============================================================================
;; magit-todos — Surface TODO/FIXME/HACK in magit-status
;; =============================================================================
;; Adds a "TODOs" section to the magit-status buffer scanning the working
;; tree for keyword comments.  Particularly useful for AI-generated code,
;; which tends to leave placeholder TODO/FIXME notes that should not slip
;; into a commit unnoticed.  Scanning uses ripgrep when available
;; (falls back to `git grep'); requires `brew install ripgrep'.

(use-package magit-todos
  :straight t
  :after magit
  ;; Only load if a scanner is available — otherwise the section can
  ;; error mid-render and leave magit-status blank.
  :if (or (executable-find "rg") (executable-find "ag"))
  :config
  (setq magit-todos-keywords '("TODO" "FIXME" "HACK" "XXX" "NOTE")
        magit-todos-max-items 50
        magit-todos-recursive t
        magit-todos-exclude-globs '(".git/" "node_modules/" "target/" "dist/"))
  (magit-todos-mode 1))

;; =============================================================================
;; difftastic — AST-aware structural diffs
;; =============================================================================
;; Difftastic understands code structure, so renaming a variable or
;; reflowing a block doesn't show up as a wall of red/green noise.  Much
;; cleaner for reviewing AI rewrites that touch the same region twice.
;;
;; Adds two suffixes to the magit-diff transient (press `d' in magit-status):
;;   D  — difftastic dwim diff (working tree, staged, range — depending on context)
;;   S  — difftastic show (for a specific commit, like magit-show)
;;
;; Requires the `difft' CLI: `brew install difftastic'.
;; The diff opens in its own buffer (`difftastic-mode'); press `q' to bury.

(use-package difftastic
  :straight t
  :after magit
  ;; No-op if `difft' isn't installed — keeps the transient clean.
  :if (executable-find "difft")
  ;; Required: without `:demand', use-package defers loading and the
  ;; `transient-append-suffix' below never runs (no `:bind' / `:hook' /
  ;; `:commands' to trigger loading otherwise).
  :demand t
  :config
  ;; Side-by-side layout makes much better use of wide-screen real estate
  ;; than the default inline (single-column) view.  difftastic.el doesn't
  ;; emit `--display' itself, so the env var is honored.
  ;; (DFT_BACKGROUND is *not* honored — difftastic.el always emits an
  ;; explicit `--background=<frame-bg>' flag based on the frame, which
  ;; overrides any env var.)
  (setenv "DFT_DISPLAY" "side-by-side")

  ;; Color overrides.
  ;;
  ;; difftastic maps ANSI red/green to `magit-diff-removed' and
  ;; `magit-diff-added' (see `difftastic-normal-colors-vector' default).
  ;; In this config those faces are background-only with very dark tints
  ;; — appropriate for line-level diffs in magit, but invisible for
  ;; token-level highlighting in difftastic.  Use bright foreground
  ;; faces here, matching the accent colors used elsewhere (TODO/DONE
  ;; in init-org.el) for visual consistency.
  (defface +difftastic/removed
    '((t (:foreground "#ff6c6b" :weight bold)))
    "Bright red for difftastic removed tokens (token-level, fg-only).")
  (defface +difftastic/added
    '((t (:foreground "#98be65" :weight bold)))
    "Bright green for difftastic added tokens (token-level, fg-only).")

  (setq difftastic-normal-colors-vector
        (vector (aref ansi-color-normal-colors-vector 0)
                '+difftastic/removed
                '+difftastic/added
                'magit-diff-file-heading
                'font-lock-comment-face
                'font-lock-string-face
                'font-lock-warning-face
                (aref ansi-color-normal-colors-vector 7))
        difftastic-bright-colors-vector
        (vector (aref ansi-color-bright-colors-vector 0)
                '+difftastic/removed
                '+difftastic/added
                'magit-diff-file-heading
                'font-lock-comment-face
                'font-lock-string-face
                'font-lock-warning-face
                (aref ansi-color-bright-colors-vector 7))
        ;; Disable the highlight-face remapping since our new faces
        ;; don't have separate highlight variants.  difftastic falls
        ;; back to its default underline for emphasized changes.
        difftastic-highlight-alist nil)

  ;; Direct keybindings — skip magit's `d' transient, which has lots
  ;; of options that aren't useful for an AI-review workflow:
  ;;   D  Difftastic diff of the entire working tree
  ;;   d  Difftastic diff for the file at point (falls back to full)
  ;;   q  Quit + kill the difftastic buffer (in difftastic-mode)
  ;; These shadow evil-collection's `d' (transient) in magit buffers.

  ;; Reuse the existing difftastic window in-place rather than splitting
  ;; a new one each time, AND kill the stale buffer once the new one is
  ;; placed.  Doing the kill from inside this function (rather than from
  ;; a wrapper around `difftastic-magit-diff') is critical: difftastic
  ;; runs the diff asynchronously and only invokes this display fn from
  ;; the process sentinel, after the new buffer has content.  Killing
  ;; the old buffer earlier orphans the difftastic window (it'd switch
  ;; to magit-status), and the new diff would then pop up in a third
  ;; window — that's the "extra magit review + difftastic" symptom.
  (setq difftastic-display-buffer-function
        (lambda (buffer _requested-width)
          (let* ((existing-win
                  (cl-find-if
                   (lambda (w)
                     (and (not (eq (window-buffer w) buffer))
                          (with-current-buffer (window-buffer w)
                            (derived-mode-p 'difftastic-mode))))
                   (window-list)))
                 (existing-buf
                  (cl-find-if
                   (lambda (b)
                     (and (not (eq b buffer))
                          (with-current-buffer b
                            (derived-mode-p 'difftastic-mode))))
                   (buffer-list))))
            ;; Place the new buffer first.
            (if existing-win
                (progn (set-window-buffer existing-win buffer)
                       (select-window existing-win))
              (pop-to-buffer buffer))
            ;; Then drop the stale buffer (its window is already showing
            ;; the new buffer above).
            (when existing-buf
              (kill-buffer existing-buf)))))

  (defun +difftastic/full ()
    "Show a difftastic diff of the entire working tree."
    (interactive)
    (difftastic-magit-diff nil nil))

  (defun +difftastic/at-point ()
    "Show a difftastic diff restricted to the file at point.
Falls back to `+difftastic/full' when point isn't on a file."
    (interactive)
    (let ((file (magit-file-at-point)))
      (if file
          (difftastic-magit-diff nil (list file))
        (+difftastic/full))))

  (defun +difftastic/quit ()
    "Quit and kill the current difftastic buffer + close its window.
Custom rather than `quit-window' because reusing the difftastic window
via `set-window-buffer' (in our display fn) clobbers the window's
`quit-restore' parameter, so `quit-window' no longer recognises it
as a popup and falls back to showing whatever was previously in
that window (typically magit-status).  Just delete the window
explicitly when there's more than one in the frame."
    (interactive)
    (let ((win (selected-window)))
      (kill-buffer (current-buffer))
      (when (and (window-live-p win)
                 (not (one-window-p)))
        (delete-window win))))

  (defun +difftastic/setup-evil-keys ()
    "Bind `D' / `d' to difftastic shortcuts in magit buffers."
    (when (bound-and-true-p evil-mode)
      (evil-local-set-key 'normal (kbd "D") '+difftastic/full)
      (evil-local-set-key 'normal (kbd "d") '+difftastic/at-point)))

  (defun +difftastic/setup-buffer-keys ()
    "Inside a difftastic buffer, bind `q' to quit + kill."
    (when (bound-and-true-p evil-mode)
      (evil-local-set-key 'normal (kbd "q") '+difftastic/quit))
    ;; Also set in the global keymap so non-evil callers get q.
    (local-set-key (kbd "q") '+difftastic/quit))

  (dolist (hook '(magit-status-mode-hook
                  magit-diff-mode-hook
                  magit-revision-mode-hook))
    (add-hook hook #'+difftastic/setup-evil-keys))

  (add-hook 'difftastic-mode-hook #'+difftastic/setup-buffer-keys)

  (with-eval-after-load 'magit-diff
    ;; Append to both `magit-diff' (the dispatch transient you get from
    ;; magit-status with `d') and `magit-diff-refresh' (the transient
    ;; you get with `d' from inside an already-open diff buffer), so
    ;; `D'/`S' work in either context.
    (dolist (prefix '(magit-diff magit-diff-refresh))
      (transient-append-suffix prefix '(-1 -1)
        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
         ("S" "Difftastic show"        difftastic-magit-show)]))))

;; =============================================================================
;; Untracked Files: Show Full Diff (with proper magit sections)
;; =============================================================================

;; By default, magit-diff-working-tree doesn't show untracked file content.
;; For AI code review, we want to see new files as full green diffs.
;; This uses magit's section API so the sections are collapsible with TAB
;; and navigable with n/p like any other diff section.
;;
;; We add this to `magit-diff-sections-hook' so the sections are part of
;; the normal buffer generation and survive refreshes (unlike post-refresh
;; hooks which append content that gets erased on next refresh).

(defun +git/insert-untracked-diff-sections ()
  "Insert untracked files as proper magit diff sections.
Added to `magit-diff-sections-hook' and only activates for
working-tree diff buffers (created by `+git/review')."
  ;; Only for working-tree diffs: magit-buffer-diff-type is 'committed
  ;; and magit-buffer-diff-typearg is nil (not --cached)
  (when (and (derived-mode-p 'magit-diff-mode)
             (bound-and-true-p magit-buffer-diff-type)
             (eq magit-buffer-diff-type 'committed)
             (null magit-buffer-diff-typearg)
             (magit-toplevel))
    (let ((untracked (magit-untracked-files))
          (root (magit-toplevel)))
      (when untracked
        ;; Outer container section
        (magit-insert-section (untracked nil t)
          (magit-insert-heading
            (format "%d untracked file%s"
                    (length untracked)
                    (if (= 1 (length untracked)) "" "s")))
          (magit-insert-section-body
            (dolist (file untracked)
              (let* ((full-path (expand-file-name file root))
                     (content (condition-case nil
                                  (with-temp-buffer
                                    (insert-file-contents full-path)
                                    (buffer-string))
                                (error "")))
                     (lines (split-string content "\n"))
                     (nlines (length lines))
                     (mode (assoc-default file auto-mode-alist
                                          'string-match))
                     (fontified-lines
                      (if (and mode (fboundp mode))
                          (with-temp-buffer
                            (insert content)
                            (delay-mode-hooks (funcall mode))
                            (font-lock-ensure)
                            (split-string (buffer-string) "\n"))
                        lines)))
                ;; File section (collapsed by default)
                (magit-insert-section (file file t)
                  (magit-insert-heading
                    (propertize (format "new file   %s  (%d lines)\n"
                                       file nlines)
                                'font-lock-face
                                'magit-diff-file-heading))
                  (magit-insert-section-body
                    ;; Single hunk for the entire file
                    (magit-insert-section (hunk nil nil
                                               :from-range '(0 0)
                                               :to-range (list 1 nlines))
                      (magit-insert-heading
                        (propertize
                         (format "@@ -0,0 +1,%d @@ (new file)\n" nlines)
                         'font-lock-face 'magit-diff-hunk-heading))
                      ;; Insert each line with diff-added face + syntax highlighting
                      (let ((i 0))
                        (dolist (line fontified-lines)
                          (let* ((fline (nth i fontified-lines))
                                 (text (concat "+" (or fline line) "\n"))
                                 (result (copy-sequence text)))
                            (add-face-text-property
                             0 (length result) 'magit-diff-added
                             'append result)
                            (insert result))
                          (setq i (1+ i)))))
                    (insert "\n")))))
            (insert "\n")))))))

;; Disabled: this hand-rolled inserter creates `hunk' sections with nil
;; position markers under some conditions, which breaks
;; `magit-section-post-command-hook' with
;;   (wrong-type-argument number-or-marker-p nil)
;; magit's built-in untracked-files section already shows new files in
;; magit-status; re-enable this only if you want full-content green
;; diffs for untracked files in `+git/review' buffers.
;; (with-eval-after-load 'magit
;;   (add-hook 'magit-diff-sections-hook #'+git/insert-untracked-diff-sections t))

;;; init-git.el ends here
