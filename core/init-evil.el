;;; init-evil.el --- Evil mode and keybindings -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Evil mode configuration and all keybindings

;;; Commentary:
;; This module configures:
;; - Evil mode (Vim emulation)
;; - Evil-collection for consistent Evil bindings
;; - Key-chord for 'jk' escape
;; - All custom keybindings in one central place
;;
;; Design Philosophy:
;; - All buffers should be navigable with hjkl
;; - Consistent window navigation with C-h/j/k/l
;; - J/K for quick 8-line jumps in normal mode
;; - H/L for beginning/end of line
;;
;; Keybinding Summary:
;; ┌─────────────────────────────────────────────────────────────────┐
;; │ Navigation                                                       │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ h/j/k/l     : Standard vim movement                              │
;; │ H           : Beginning of line                                  │
;; │ L           : End of line                                        │
;; │ J           : Move down 8 lines                                  │
;; │ K           : Move up 8 lines                                    │
;; │ gJ          : Join lines (original J behavior)                   │
;; │ C-h/j/k/l   : Window navigation (left/down/up/right)             │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Files & Buffers                                                  │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ C-p         : Find file in project (projectile)                  │
;; │ C-c C-f     : Search with ripgrep                                │
;; │ C-c r       : Rename buffer                                      │
;; │ C-x b       : Switch buffer (persp-aware)                        │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ LSP & Code                                                       │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ C-x l       : Start LSP for current buffer                       │
;; │ C-c .       : Show documentation (eldoc)                         │
;; │ M-l         : Code actions                                       │
;; │ M-/         : Find type definition                               │
;; │ M-?         : Find references                                    │
;; │ C-c f       : Format buffer (prog) / Cleanup (org)                │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Org-mode                                                         │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ C-c l       : Store link                                         │
;; │ C-c a       : Org agenda                                         │
;; │ C-c d       : Set deadline                                       │
;; │ C-c s       : Schedule                                           │
;; │ C-c o w     : Open weids/tasks.org                               │
;; │ C-c o z     : Open zynerise/tasks.org                            │
;; │ C-c o a     : Open agenda/tasks.org                              │
;; │ C-c o g     : Open gcal.org                                      │
;; │ TAB         : Cycle visibility (in org-mode)                     │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Misc                                                             │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ jk          : Escape to normal mode (insert mode)                │
;; │ C-y         : Copy to tmux buffer (visual); C-a P to paste     │
;; │ M-RET       : Toggle fullscreen                                  │
;; │ s-`         : Select treemacs window                             │
;; │ C-x t t     : Toggle treemacs                                    │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Git Review (owned by init-git / init-git-ui)                     │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ C-x g       : Magit status                                       │
;; │ C-c g       : +git-dispatch Transient (status/review/log)        │
;; │ Review mode : h/j/k/l H/J/K/L TAB RET e o t gf/gF gh/gH gc/gC  │
;; └─────────────────────────────────────────────────────────────────┘

;;; Code:

;; =============================================================================
;; Evil Mode
;; =============================================================================

;; Must be set before evil loads
(setq evil-want-keybinding nil)  ; Let evil-collection handle mode keybindings
(setq evil-want-integration t)

(use-package evil
  :straight t
  :demand t
  :init
  ;; Enable system clipboard integration
  (setq select-enable-clipboard t
        select-enable-primary t           ; Also use PRIMARY selection (Linux)
        save-interprogram-paste-before-kill t)  ; Save clipboard to kill ring
  ;; Use C-u for scroll (like vim)
  (setq evil-want-C-u-scroll nil)  ; Keep C-u for universal-argument
  ;; Make * / # search the whole SYMBOL under point (includes _), so
  ;; pressing * on funding_payment_dto searches the full identifier rather
  ;; than just the `funding' word fragment.
  (setq evil-symbol-word-search t)
  :config
  (evil-mode 1)
  
  ;; Set cursor shapes for different states
  (setq evil-insert-state-cursor '(bar . 2)
        evil-normal-state-cursor 'box
        evil-visual-state-cursor 'hollow)
  
  ;; Make evil use system clipboard for yank/paste
  (setq evil-kill-on-visual-paste nil))  ; Don't replace clipboard when pasting over selection

;; Clipboard helpers live in init-core.el (`+clipboard/get', `+clipboard/set',
;; `+clipboard/paste', `+copy-to-system-clipboard').  In terminal Emacs, `p'
;; and `C-v` paste from the host clipboard; visual `C-y` copies to it.

;; =============================================================================
;; Terminal clipboard paste (evil)
;; =============================================================================

(defun +evil/terminal-clipboard-paste-p ()
  "Non-nil in terminal Emacs where OSC 52 clipboard is unreliable."
  (not (display-graphic-p)))

(defun +evil/sync-yank-to-clipboard (&rest _)
  "After evil yank, mirror text to the host clipboard (terminal only)."
  (when (and (+evil/terminal-clipboard-paste-p) (car kill-ring))
    (+clipboard/set (car kill-ring))))

(defun +evil/paste (count)
  "Paste in evil: prefer host clipboard when it differs from the kill ring.
In terminal Emacs this makes external copy → `p' work; internal `y' then `p'
still uses the kill ring once yank has synced via `+evil/sync-yank-to-clipboard'."
  (interactive "P")
  (let ((count (or count 1)))
    (if (+evil/terminal-clipboard-paste-p)
        (let ((clip (+clipboard/get))
              (kill (and kill-ring (current-kill 0))))
          (if (and clip (not (string-empty-p clip))
                   (or (null kill) (not (string= clip kill))))
              (progn (kill-new clip) (evil-paste count 'no-autoselect))
            (evil-paste count)))
      (evil-paste count))))

(with-eval-after-load 'evil
  ;; Only hook explicit yank commands — not evil-copy (visual C-y already
  ;; calls `+copy-to-system-clipboard') or delete paths that reuse yank internals.
  (dolist (fn '(evil-yank evil-yank-window evil-yank-line))
    (advice-add fn :after #'+evil/sync-yank-to-clipboard))
  (when (+evil/terminal-clipboard-paste-p)
    (define-key evil-normal-state-map (kbd "p") #'+evil/paste)
    (define-key evil-normal-state-map (kbd "P") #'+evil/paste)
    (define-key evil-visual-state-map (kbd "p") #'+evil/paste)
    (define-key evil-visual-state-map (kbd "P") #'+evil/paste)
    (define-key evil-insert-state-map (kbd "C-v") #'+clipboard/paste)))

;; =============================================================================
;; Evil Collection (consistent bindings across modes)
;; =============================================================================

(use-package evil-collection
  :straight t
  :after evil
  :demand t
  :config
  ;; Setup evil-collection for common modes
  ;; Note: Only include modes that exist in evil-collection
  (evil-collection-init '(magit
                          forge
                          diff-mode
                          dired
                          ibuffer
                          help
                          info
                          minibuffer
                          compile
                          xref
                          calendar
                          debug
                          eshell
                          term
                          vterm)))

;; =============================================================================
;; Key-chord (for 'jk' escape)
;; =============================================================================

(use-package key-chord
  :straight t
  :demand t
  :config
  (key-chord-mode 1)
  ;; Exit insert mode with 'jk' - works globally
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  
  ;; Ensure jk works in eshell too
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook
              (lambda ()
                (key-chord-define-local "jk" 'evil-normal-state)))))

;; =============================================================================
;; Movement Functions
;; =============================================================================

(defun +evil/move-lines-down (n)
  "Move cursor N lines downwards (default 8)."
  (interactive "p")
  (evil-next-line (* (or n 1) 8)))

(defun +evil/move-lines-up (n)
  "Move cursor N lines upwards (default 8)."
  (interactive "p")
  (evil-previous-line (* (or n 1) 8)))

;; =============================================================================
;; Normal State Keybindings
;; =============================================================================

(with-eval-after-load 'evil
  ;; Clear some bindings we'll override
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-r") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  
  ;; Quick line navigation
  (define-key evil-normal-state-map (kbd "H") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "J") '+evil/move-lines-down)
  (define-key evil-normal-state-map (kbd "K") '+evil/move-lines-up)
  
  ;; Since J is remapped, use gJ for join-line (original J behavior)
  (define-key evil-normal-state-map (kbd "g J") 'evil-join)
  
  ;; Window navigation with C-h/j/k/l
  ;; These are global, mode-specific hooks will reinforce them
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up))

;; =============================================================================
;; Visual State Keybindings
;; =============================================================================

(with-eval-after-load 'evil
  ;; Copy to system clipboard
  (define-key evil-visual-state-map (kbd "C-y") '+copy-to-system-clipboard))

;; =============================================================================
;; Mode-Specific Evil Keybindings
;; =============================================================================

;; Function to ensure window navigation works in all modes
(defun +evil/set-windmove-keys ()
  "Set windmove keybindings for the current mode in Evil normal state.
This ensures C-h/j/k/l work for window navigation in all buffers."
  (when (and (boundp 'evil-mode) evil-mode)
    (evil-local-set-key 'normal (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'normal (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'normal (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'normal (kbd "C-k") 'windmove-up)))

;; Apply to common modes that might override these keys
(dolist (hook '(org-mode-hook
                org-agenda-mode-hook
                magit-mode-hook
                magit-diff-mode-hook
                magit-revision-mode-hook
                compilation-mode-hook
                shell-mode-hook
                help-mode-hook
                Info-mode-hook
                dired-mode-hook
                treemacs-mode-hook))
  (add-hook hook '+evil/set-windmove-keys))

;; Org-mode specific bindings
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))

;; Org-agenda evil bindings — hjkl navigation + RET to jump to heading
(with-eval-after-load 'org-agenda
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-define-key 'normal org-agenda-mode-map
    "j"  'org-agenda-next-line
    "k"  'org-agenda-previous-line
    (kbd "RET") 'org-agenda-switch-to       ; jump to heading in org file
    "t"  'org-agenda-todo                   ; cycle TODO state
    "u"  'org-agenda-undo
    "I"  'org-agenda-clock-in
    "O"  'org-agenda-clock-out
    "d"  'org-agenda-day-view
    "w"  'org-agenda-week-view
    "f"  'org-agenda-later
    "b"  'org-agenda-earlier
    "."  'org-agenda-goto-today
    "gr" 'org-agenda-redo
    "q"  'org-agenda-quit
    "s"  'org-agenda-schedule
    "S"  'org-agenda-sunrise-sunset
    "/"  'org-agenda-filter-by-tag))

;; Magit review bindings (gr/q/RET/...) are owned by +git-review-buffer-mode
;; in init-git-ui.el. Keep only discard here as a Magit action not covered
;; by the Phase 1 review vocabulary.
(with-eval-after-load 'magit
  (evil-define-key 'normal magit-mode-map (kbd "x") 'magit-discard))

;; Shell mode (for history navigation)
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key shell-mode-map (kbd "<down>") 'comint-next-input)
            (define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
            (evil-local-set-key 'insert (kbd "C-r") 'comint-history-isearch-backward)))

;; =============================================================================
;; Global Keybindings
;; =============================================================================

;; Buffer/file management
(global-set-key (kbd "C-c r") 'rename-buffer)
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

;; Org-mode global bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; =============================================================================
;; Windmove (fallback)
;; =============================================================================

(require 'windmove)
(windmove-default-keybindings)

;;; init-evil.el ends here
