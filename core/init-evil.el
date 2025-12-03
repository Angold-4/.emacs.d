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
;; │ C-c f       : Format buffer                                      │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Org-mode                                                         │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ C-c l       : Store link                                         │
;; │ C-c a       : Org agenda                                         │
;; │ C-c d       : Set deadline                                       │
;; │ C-c s       : Schedule                                           │
;; │ TAB         : Cycle visibility (in org-mode)                     │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Misc                                                             │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ jk          : Escape to normal mode (insert mode)                │
;; │ C-y         : Copy to system clipboard (visual mode)             │
;; │ M-RET       : Toggle fullscreen                                  │
;; │ s-`         : Select treemacs window                             │
;; │ C-x t t     : Toggle treemacs                                    │
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
  (setq select-enable-clipboard t)
  ;; Use C-u for scroll (like vim)
  (setq evil-want-C-u-scroll nil)  ; Keep C-u for universal-argument
  :config
  (evil-mode 1)
  
  ;; Set cursor shapes for different states
  (setq evil-insert-state-cursor '(bar . 2)
        evil-normal-state-cursor 'box
        evil-visual-state-cursor 'hollow))

;; =============================================================================
;; Evil Collection (consistent bindings across modes)
;; =============================================================================

(use-package evil-collection
  :straight t
  :after evil
  :demand t
  :config
  ;; Setup evil-collection for common modes
  (evil-collection-init '(magit
                          dired
                          ibuffer
                          help
                          info
                          minibuffer
                          compile
                          xref
                          calendar
                          debug)))

;; =============================================================================
;; Key-chord (for 'jk' escape)
;; =============================================================================

(use-package key-chord
  :straight t
  :demand t
  :config
  (key-chord-mode 1)
  ;; Exit insert mode with 'jk'
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

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

;; Magit specific bindings
(with-eval-after-load 'magit
  (evil-define-key 'normal magit-mode-map (kbd "g") 'magit-refresh)
  (evil-define-key 'normal magit-mode-map (kbd "0") 'magit-discard))

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

;; =============================================================================
;; Windmove (fallback)
;; =============================================================================

(require 'windmove)
(windmove-default-keybindings)

;;; init-evil.el ends here
