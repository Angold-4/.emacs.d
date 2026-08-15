;;; init-tools.el --- Development tools -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Development tools: project management, file tree, shells

;;; Commentary:
;; This module configures development tools:
;; - Projectile: Project management
;; - Treemacs: File tree sidebar
;; - Persp-mode: Workspace/perspective management
;; - TRAMP: Remote file editing
;; Magit/Forge ownership lives in init-git.el and init-git-ui.el.

;;; Code:

;; =============================================================================
;; Shell Mode (with directory tracking)
;; =============================================================================

;; Track directory changes in shell buffers
;; This makes Emacs aware of 'cd' commands so completion works correctly
(use-package shell
  :straight (:type built-in)
  :hook (shell-mode . +shell/setup-directory-tracking)
  :config
  (defun +shell/setup-directory-tracking ()
    "Enable directory tracking in shell mode."
    ;; Enable directory tracking via prompt
    (shell-dirtrack-mode -1)  ; Disable simple dirtrack
    (dirtrack-mode 1))        ; Enable regex-based dirtrack
  
  ;; Configure dirtrack to parse common prompt formats
  ;; Matches prompts like: user@host:/path$ or /path$
  (use-package dirtrack
    :straight (:type built-in)
    :config
    ;; Match typical prompts: [user@host path]$ or user@host:path$ or just path$
    (setq dirtrack-list '("^.*?\\(?::\\|\\]\\)?\\s-*\\([^$#\n]+\\)[$#]\\s-*" 1)))
  
  ;; Alternative: Use DIRTRACK with PROMPT_COMMAND (more reliable)
  ;; Add this to your .bashrc:
  ;; PROMPT_COMMAND='echo -e "\033AnSiTc" "${PWD}"'
  ;; Then enable ansi-color to parse it:
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Enable ANSI colors
              (ansi-color-for-comint-mode-on)
              ;; Process escape sequences for directory tracking
              (setq comint-process-echoes nil))))

;; Eshell (alternative shell with better Emacs integration)
(use-package eshell
  :straight (:type built-in)
  :commands eshell
  :hook (eshell-mode . +eshell/setup)
  :init
  ;; Fix: Remove eshell-term from modules list (doesn't exist in newer Emacs)
  (setq eshell-modules-list
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-unix))
  :config
  ;; Basic settings
  (setq eshell-scroll-to-bottom-on-input 'this
        eshell-scroll-to-bottom-on-output nil
        eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-destroy-buffer-when-process-dies t)
  
  ;; Eshell prompt
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (abbreviate-file-name (eshell/pwd)) 'face 'font-lock-keyword-face)
           (if (= (user-uid) 0) " # " " $ ")))
        eshell-prompt-regexp "^[^$#\n]*[#$] ")
  
  ;; Visual commands (run in term instead of eshell)
  (setq eshell-visual-commands '("htop" "top" "less" "more" "vim" "vi" "nano"))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
  
  ;; Setup function for eshell buffers
  (defun +eshell/setup ()
    "Setup eshell buffer with proper Evil integration."
    ;; Start in insert mode (ready to type)
    (when (bound-and-true-p evil-mode)
      (evil-insert-state))
    ;; Enable completion
    (setq-local company-backends '(company-files company-capf))
    ;; Don't use pager
    (setenv "PAGER" "cat"))
  
  ;; Toggle eshell at bottom
  (defun +eshell/toggle ()
    "Toggle eshell window at the bottom of the frame."
    (interactive)
    (if-let ((eshell-window (cl-find-if
                             (lambda (w)
                               (with-current-buffer (window-buffer w)
                                 (eq major-mode 'eshell-mode)))
                             (window-list))))
        (delete-window eshell-window)
      (let ((eshell-buffer (or (cl-find-if
                                (lambda (b)
                                  (with-current-buffer b
                                    (eq major-mode 'eshell-mode)))
                                (buffer-list))
                               (eshell))))
        (select-window (split-window-below -15))
        (switch-to-buffer eshell-buffer))))
  
  ;; Open new eshell
  (defun +eshell/new ()
    "Open a new eshell buffer."
    (interactive)
    (eshell t))
  
  ;; Useful eshell aliases (defined in Elisp)
  (defalias 'eshell/e 'find-file)
  (defalias 'eshell/ee 'find-file-other-window)
  
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  
  (defun eshell/d (&optional path)
    "Open dired in PATH or current directory."
    (dired (or path "."))))

;; =============================================================================
;; Eshell Evil Integration (must be done via hook)
;; =============================================================================

(defun +eshell/evil-setup ()
  "Setup Evil keybindings for eshell. Called from eshell-mode-hook."
  (when (bound-and-true-p evil-mode)
    ;; Set initial state to insert
    (evil-set-initial-state 'eshell-mode 'insert)
    
    ;; Use local-set-key for reliable binding
    ;; ESC to normal mode
    (evil-local-set-key 'insert (kbd "<escape>") 'evil-normal-state)
    
    ;; Window navigation in normal mode
    (evil-local-set-key 'normal (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'normal (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'normal (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'normal (kbd "C-k") 'windmove-up)
    
    ;; Also in insert mode for quick window switching
    (evil-local-set-key 'insert (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'insert (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'insert (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'insert (kbd "C-k") 'windmove-up)
    
    ;; History navigation in insert mode
    (evil-local-set-key 'insert (kbd "C-p") 'eshell-previous-input)
    (evil-local-set-key 'insert (kbd "C-n") 'eshell-next-input)
    (evil-local-set-key 'insert (kbd "<up>") 'eshell-previous-input)
    (evil-local-set-key 'insert (kbd "<down>") 'eshell-next-input)
    (evil-local-set-key 'insert (kbd "C-r") 'eshell-isearch-backward)
    
    ;; Navigation in normal mode
    (evil-local-set-key 'normal (kbd "G") 'end-of-buffer)
    (evil-local-set-key 'normal (kbd "gg") 'beginning-of-buffer)
    (evil-local-set-key 'normal (kbd "0") 'eshell-bol)
    
    ;; Go to prompt and insert
    (evil-local-set-key 'normal (kbd "i")
      (lambda () (interactive)
        (goto-char (point-max))
        (evil-insert-state)))
    (evil-local-set-key 'normal (kbd "a")
      (lambda () (interactive)
        (goto-char (point-max))
        (evil-insert-state)))
    (evil-local-set-key 'normal (kbd "A")
      (lambda () (interactive)
        (goto-char (point-max))
        (evil-insert-state)))
    (evil-local-set-key 'normal (kbd "I")
      (lambda () (interactive)
        (goto-char (point-max))
        (eshell-bol)
        (evil-insert-state)))
    
    ;; RET in normal mode
    (evil-local-set-key 'normal (kbd "RET")
      (lambda () (interactive)
        (goto-char (point-max))
        (eshell-send-input)))
    
    ;; dd to clear line
    (evil-local-set-key 'normal (kbd "dd")
      (lambda () (interactive)
        (eshell-bol)
        (kill-line)))
    
    ;; Paste function for eshell (with WSL support)
    (defun +eshell/paste ()
      "Paste from system clipboard at eshell prompt.
Supports WSL by using powershell.exe to access Windows clipboard."
      (interactive)
      (goto-char (point-max))
      (let ((clip (+clipboard/get)))
        (if (and clip (not (string-empty-p clip)))
            (insert clip)
          (message "Clipboard is empty"))))
    
    ;; p to paste from clipboard (go to prompt first)
    ;; Use define-key directly on the local map for higher priority
    (define-key evil-normal-state-local-map (kbd "p") '+eshell/paste)
    (define-key evil-normal-state-local-map (kbd "P") '+eshell/paste)
    
    ;; C-v to paste in insert mode (common shortcut)
    (define-key evil-insert-state-local-map (kbd "C-v") '+eshell/paste)))

;; Add our Evil setup to eshell-mode-hook
(add-hook 'eshell-mode-hook #'+eshell/evil-setup)

;; =============================================================================
;; Vterm (Full Terminal Emulator via libvterm)
;; =============================================================================

(defvar +vterm/counter 0
  "Monotonic counter for naming terminals created by `+vterm/new' (1sh, 2sh...).")

(defvar +vterm/last-buffer nil
  "Most recently created terminal buffer, used by `+vterm/goto-last' (:v).")

(defun +vterm/start-directory ()
  "Return the stable directory in which new vterm shells should start."
  (let ((dir (and (boundp '+emacs-launch-directory)
                  +emacs-launch-directory)))
    (file-name-as-directory
     (expand-file-name
      (if (and (stringp dir) (file-directory-p dir))
          dir
        user-emacs-directory)))))

(defun +vterm/use-start-directory (&rest _)
  "Make the current new vterm buffer start in the Emacs launch directory."
  ;; `default-directory' is permanent-local, so this survives the major-mode
  ;; reset and is already in force when vterm creates its shell process.
  (setq-local default-directory (+vterm/start-directory)))

(use-package vterm
  :straight t
  :commands (vterm vterm-other-window vterm-mode)
  :bind (("C-c v" . +vterm/toggle)
         ("C-c V" . +vterm/new))
  :config
  ;; General settings
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t
        ;; Agent TUIs redraw the whole screen in short bursts.  Coalesce those
        ;; writes into complete frames instead of painting partial frames at
        ;; 100 Hz, which appears as a flash on macOS.
        vterm-timer-delay 0.05)

  ;; Shell to use (default to user's shell)
  (setq vterm-shell (or (getenv "SHELL") "/bin/bash"))

  ;; Enable directory tracking so Emacs knows the CWD
  ;; Add this to your .bashrc/.zshrc:
  ;;   vterm_printf(){ printf "\e]%s\e\\" "$1"; }
  ;;   vterm_prompt_end(){ vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"; }
  ;;   PS1=$PS1'\[$(vterm_prompt_end)\]'

  ;; Toggle vterm at bottom
  (defun +vterm/toggle ()
    "Toggle vterm window at the bottom of the frame."
    (interactive)
    (if-let ((vterm-window (cl-find-if
                            (lambda (w)
                              (with-current-buffer (window-buffer w)
                                (eq major-mode 'vterm-mode)))
                            (window-list))))
        (delete-window vterm-window)
      (let ((vterm-buffer (or (cl-find-if
                               (lambda (b)
                                 (with-current-buffer b
                                   (eq major-mode 'vterm-mode)))
                               (buffer-list))
                              (vterm))))
        (select-window (split-window-below -15))
        (switch-to-buffer vterm-buffer)))))

;; -----------------------------------------------------------------------------
;; Terminal workflow: :vterm opens a new numbered terminal, :v jumps to it
;; -----------------------------------------------------------------------------
;; Stock `vterm' reuses the single `*vterm*' buffer, so a second one needs a
;; manual rename first.  Instead:
;;   :vterm  -> always open a NEW terminal in the Emacs launch dir
;;              (1sh, 2sh, ...)
;;   :v      -> jump to the most recently created terminal
;;
;; Defined at top level (not in vterm's :config) and requiring vterm itself,
;; so the ex commands work even before vterm has loaded for the first time.
(defun +vterm/new ()
  "Open a new terminal in the Emacs launch directory with a numbered name.
Buffers are named 1sh, 2sh, 3sh, ... instead of the default *vterm*, so
several can coexist without renaming.  The new buffer is recorded as the
most-recently-created terminal for `+vterm/goto-last' (:v)."
  (interactive)
  (require 'vterm)
  (setq +vterm/counter (1+ +vterm/counter))
  (let ((dir (+vterm/start-directory))
        (buf (generate-new-buffer (format "%dsh" +vterm/counter))))
    (with-current-buffer buf
      (setq default-directory dir)
      (vterm-mode))
    ;; Explicitly attach to the current perspective so the new terminal
    ;; reliably shows up in the persp-aware C-x b switcher (the after-major-
    ;; mode auto-add can miss buffers created with `generate-new-buffer').
    (when (and (bound-and-true-p persp-mode) (fboundp 'persp-add-buffer))
      (persp-add-buffer buf))
    (setq +vterm/last-buffer buf)
    (pop-to-buffer-same-window buf)
    buf))

(defun +vterm/live-terminals ()
  "Return all live terminal buffers, most-recently-used first.
Uses the global buffer list, so terminals in *every* perspective are
included (persp-mode partitions buffers per perspective otherwise)."
  (cl-remove-if-not
   (lambda (b)
     (and (buffer-live-p b)
          (with-current-buffer b (derived-mode-p 'vterm-mode))))
   (buffer-list)))

(defun +vterm/goto-last ()
  "Switch to the most recently created terminal (see `+vterm/new').
If it's gone, fall back to the most recent live terminal, or create one."
  (interactive)
  (let ((target (if (buffer-live-p +vterm/last-buffer)
                    +vterm/last-buffer
                  (car (+vterm/live-terminals)))))
    (if target
        (progn (setq +vterm/last-buffer target)
               (pop-to-buffer-same-window target))
      (+vterm/new))))

(defun +vterm/switch ()
  "Switch to any live terminal across ALL perspectives.
This never hides a terminal the way the persp-aware C-x b can."
  (interactive)
  (let ((names (mapcar #'buffer-name (+vterm/live-terminals))))
    (if names
        (let ((choice (completing-read "Terminal: " names nil t nil nil (car names))))
          (when (and choice (not (string-empty-p choice)))
            (setq +vterm/last-buffer (get-buffer choice))
            (switch-to-buffer choice)))
      (+vterm/new))))

;; Note: C-c v is bound to +vterm/toggle (a command, not a prefix), so we
;; can't hang C-c v v off it. The terminal switcher is reached via :vt.
(with-eval-after-load 'evil
  (evil-ex-define-cmd "vterm" #'+vterm/new)
  (evil-ex-define-cmd "v" #'+vterm/goto-last)
  (evil-ex-define-cmd "vt" #'+vterm/switch))

;; A vterm has two intentionally different display modes:
;;
;;   selected insert terminal output owns point and the viewport;
;;   normal/visual  Emacs owns them, like an ordinary read-only buffer;
;;   unselected     Emacs freezes that window, regardless of Evil state.
;;
;; Keep exactly one snapshot per window.  In particular, never take a fresh
;; snapshot while vterm redraws: by then libvterm may already have moved point,
;; and recording that position is what caused the old cursor/view "snap".
;; The PTY remains live in both modes; this freezes only the Emacs view.
(defvar +vterm/frozen-views (make-hash-table :test #'eq)
  "Normal/visual vterm views keyed by window.
Each value is (BUFFER POINT WINDOW-START).")

(defvar +vterm/command-window nil
  "Window in which the current command began.")

(defvar +vterm/command-buffer nil
  "Buffer in which the current command began.")

(defun +vterm/frozen-state-p (&optional buffer)
  "Return non-nil when BUFFER's vterm view should behave like a text buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'vterm-mode)
         (bound-and-true-p evil-mode)
         (memq evil-state '(normal visual)))))

(defun +vterm/window-vterm-p (window)
  "Return non-nil when live WINDOW displays a vterm buffer."
  (and (window-live-p window)
       (with-current-buffer (window-buffer window)
         (derived-mode-p 'vterm-mode))))

(defun +vterm/window-should-freeze-p (window)
  "Return non-nil when WINDOW's vterm viewport belongs to Emacs.
Every unselected vterm is frozen.  A selected one is frozen only while Evil is
in normal or visual state."
  (and (+vterm/window-vterm-p window)
       (or (not (eq window (selected-window)))
           (+vterm/frozen-state-p (window-buffer window)))))

(defun +vterm/visible-windows ()
  "Return visible non-minibuffer windows across all live frames."
  (let (windows)
    (dolist (frame (frame-list))
      (when (frame-live-p frame)
        (setq windows (nconc (window-list frame 'nomini) windows))))
    windows))

(defun +vterm/freeze-window-view (window)
  "Save WINDOW's current Emacs-owned vterm viewport."
  (when (+vterm/window-should-freeze-p window)
    (puthash window
             (list (window-buffer window)
                   (window-point window)
                   (window-start window))
             +vterm/frozen-views)))

(defun +vterm/restore-window-view (window)
  "Restore WINDOW's frozen vterm viewport, if it still applies."
  (let ((saved (gethash window +vterm/frozen-views)))
    (cond
     ((or (not (window-live-p window))
          (not (eq (car-safe saved) (window-buffer window)))
          (not (+vterm/window-should-freeze-p window)))
      (remhash window +vterm/frozen-views))
     (saved
      (let ((buffer (car saved))
            (saved-point (nth 1 saved))
            (saved-start (nth 2 saved)))
        (with-current-buffer buffer
          (setq saved-point
                (min (max (point-min) saved-point) (point-max))
                saved-start
                (min (max (point-min) saved-start) (point-max))))
        ;; Conditional setters matter for interactive TUIs: setting an
        ;; unchanged window position still schedules needless redisplay.
        (unless (= (window-point window) saved-point)
          (set-window-point window saved-point))
        (unless (= (window-start window) saved-start)
          (set-window-start window saved-start t)))))))

(defun +vterm/freeze-current-buffer-views ()
  "Freeze every visible window showing the current normal/visual vterm."
  (when (+vterm/frozen-state-p)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (+vterm/freeze-window-view window))))

(defun +vterm/remove-current-buffer-views ()
  "Remove every frozen view belonging to the current vterm buffer."
  (let ((buffer (current-buffer))
        stale)
    (maphash (lambda (window saved)
               (when (or (not (window-live-p window))
                         (eq (car-safe saved) buffer))
                 (push window stale)))
             +vterm/frozen-views)
    (dolist (window stale)
      (remhash window +vterm/frozen-views))))

(defun +vterm/release-selected-window-view ()
  "Return the selected insert-state vterm window to its live cursor.
Unselected windows showing the same buffer retain their independent frozen
views."
  (let ((window (selected-window)))
    (when (and (+vterm/window-vterm-p window)
               (eq (window-buffer window) (current-buffer)))
      (when (gethash window +vterm/frozen-views)
        (remhash window +vterm/frozen-views)
        ;; A window can remain in insert state while `windmove' selects another
        ;; split.  On return there is no Evil state transition, so explicitly
        ;; reconnect point to libvterm's real terminal cursor once.
        (when (fboundp 'vterm-reset-cursor-point)
          (vterm-reset-cursor-point))))))

(defun +vterm/buffer-has-frozen-view-p (buffer)
  "Return non-nil when BUFFER has at least one frozen window snapshot."
  (let (found)
    (maphash (lambda (_window saved)
               (when (eq (car-safe saved) buffer)
                 (setq found t)))
             +vterm/frozen-views)
    found))

(defun +vterm/restore-frozen-views (&optional buffer)
  "Restore all frozen vterm views, optionally only those for BUFFER."
  (let (views)
    ;; Copy first so stale entries can safely be removed while restoring.
    (maphash (lambda (window saved)
               (when (or (null buffer) (eq (car-safe saved) buffer))
                 (push window views)))
             +vterm/frozen-views)
    (dolist (window views)
      (+vterm/restore-window-view window))))

(defun +vterm/before-command ()
  "Restore stable vterm views before a command and record its origin."
  (setq +vterm/command-window (selected-window)
        +vterm/command-buffer (current-buffer))
  (+vterm/restore-frozen-views))

(defun +vterm/after-command ()
  "Maintain stable vterm views after a command.
Only a command that began and ended in the same selected normal/visual vterm
may advance that window's snapshot.  Other visible vterms are restored."
  (let ((selected (selected-window)))
    (dolist (window (+vterm/visible-windows))
      (when (+vterm/window-vterm-p window)
        (let ((saved (gethash window +vterm/frozen-views)))
          (if (+vterm/window-should-freeze-p window)
              (cond
               ((and (eq window selected)
                     (eq window +vterm/command-window)
                     (eq (window-buffer window) +vterm/command-buffer))
                (+vterm/freeze-window-view window))
               ((eq (car-safe saved) (window-buffer window))
                (+vterm/restore-window-view window))
               (t
                (+vterm/freeze-window-view window)))
            ;; Selected insert-state vterms are live.  Release a snapshot left
            ;; by the same window while it was unselected.
            (with-current-buffer (window-buffer window)
              (+vterm/release-selected-window-view)))))))
  ;; Also prune stale snapshots belonging to replaced or deleted windows.
  (+vterm/restore-frozen-views))

(defun +vterm/advice-delayed-redraw (orig-fn buffer)
  "Run ORIG-FN for BUFFER without letting redraw move a frozen view."
  (if (and (buffer-live-p buffer)
           (+vterm/buffer-has-frozen-view-p buffer))
      ;; Keep redisplay inhibited across both libvterm's redraw and our
      ;; restore.  Otherwise macOS can paint the intermediate cursor position
      ;; as a one-frame flash on every character of agent output.
      (let ((inhibit-redisplay t))
        (funcall orig-fn buffer)
        (+vterm/restore-frozen-views buffer))
    (funcall orig-fn buffer)))

(defun +vterm/install-view-stability ()
  "Install the single-owner vterm viewport policy."
  ;; Remove names from the earlier implementation when this file is reloaded
  ;; in a long-running Emacs session.
  (remove-hook 'pre-command-hook #'+vterm/pre-command-remember-view)
  (remove-hook 'post-command-hook #'+vterm/post-command-maintain-view)
  (add-hook 'pre-command-hook #'+vterm/before-command)
  (add-hook 'post-command-hook #'+vterm/after-command)
  (when (advice-member-p #'+vterm/advice-delayed-redraw
                         #'vterm--delayed-redraw)
    (advice-remove #'vterm--delayed-redraw #'+vterm/advice-delayed-redraw))
  (advice-add #'vterm--delayed-redraw :around #'+vterm/advice-delayed-redraw))

(with-eval-after-load 'vterm
  (unless (advice-member-p #'+vterm/use-start-directory 'vterm-mode)
    (advice-add 'vterm-mode :before #'+vterm/use-start-directory))
  (+vterm/install-view-stability))

;; Evil integration for vterm
(defun +vterm/evil-setup ()
  "Setup Evil keybindings for vterm."
  (when (bound-and-true-p evil-mode)
    ;; Normal/visual state freezes the Emacs view.  Insert state (and buffer
    ;; teardown) removes its snapshots so live terminal display resumes.
    (add-hook 'evil-normal-state-entry-hook
              #'+vterm/freeze-current-buffer-views nil t)
    (add-hook 'evil-visual-state-entry-hook
              #'+vterm/freeze-current-buffer-views nil t)
    (add-hook 'evil-insert-state-entry-hook
              #'+vterm/release-selected-window-view nil t)
    (add-hook 'kill-buffer-hook
              #'+vterm/remove-current-buffer-views nil t)

    ;; Start in insert state (vterm handles its own input)
    (evil-set-initial-state 'vterm-mode 'insert)

    ;; Window navigation in normal mode
    (evil-local-set-key 'normal (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'normal (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'normal (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'normal (kbd "C-k") 'windmove-up)

    ;; Also in insert mode for quick window switching
    (evil-local-set-key 'insert (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'insert (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'insert (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'insert (kbd "C-k") 'windmove-up)

    ;; i/a left to evil-collection's smart versions
    ;; (`evil-collection-vterm-insert' / `evil-collection-vterm-append'),
    ;; which align the shell cursor to point via `vterm-goto-char' instead
    ;; of jumping to end-of-buffer.

    ;; Paste from host clipboard in normal mode (WSL → Windows, etc.)
    (evil-local-set-key 'normal (kbd "p")
      (lambda () (interactive)
        (vterm-send-string (or (ignore-errors (+clipboard/get)) (current-kill 0)))))

    ;; C-v to paste in insert mode
    (evil-local-set-key 'insert (kbd "C-v")
      (lambda () (interactive)
        (vterm-send-string (or (ignore-errors (+clipboard/get)) (current-kill 0)))))

    ;; Special keys that evil's insert-state map shadows by default.
    ;; Without these, RET / TAB / Backspace try to edit the (read-only)
    ;; vterm buffer instead of being forwarded to the shell.
    (evil-local-set-key 'insert (kbd "RET")         #'vterm-send-return)
    (evil-local-set-key 'insert (kbd "<return>")    #'vterm-send-return)
    (evil-local-set-key 'insert (kbd "TAB")         #'vterm-send-tab)
    (evil-local-set-key 'insert (kbd "<tab>")       #'vterm-send-tab)
    (evil-local-set-key 'insert (kbd "DEL")         #'vterm-send-backspace)
    (evil-local-set-key 'insert (kbd "<backspace>") #'vterm-send-backspace)

    ;; Force the current buffer into insert state.  `evil-set-initial-state'
    ;; above only affects future buffers; on macOS the very first vterm
    ;; opens in normal state without this explicit switch.
    (evil-insert-state)))

(add-hook 'vterm-mode-hook #'+vterm/evil-setup)

;; =============================================================================
;; Magit lives in init-git.el / init-git-ui.el (single ownership)
;; =============================================================================

;; =============================================================================
;; Projectile (Project Management)
;; =============================================================================

(use-package projectile
  :straight t
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-p" . projectile-find-file)
         ("C-c C-f" . projectile-ripgrep))
  :config
  (projectile-mode +1)
  
  ;; 'alien indexing shells out to `git ls-files -zco --exclude-standard'
  ;; (fast even on huge repos, respects .gitignore, and includes new
  ;; untracked-but-not-ignored files). It's quick enough that we DON'T need
  ;; the file-list cache — disabling caching means new files appear in C-p
  ;; immediately instead of after a manual `C-c p i'. (The old 'hybrid +
  ;; caching combo was the reason new files didn't show up until invalidated.)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching nil
        projectile-sort-order 'recently-active
        ;; Use default completing-read (enhanced by Vertico + Orderless)
        projectile-completion-system 'default)
  
  ;; Project root = the VCS root only (the whole repo), NOT the nearest
  ;; language manifest. Projectile walks upward and stops at the first marker
  ;; it finds, so listing Cargo.toml/package.json/etc. here made each Rust
  ;; crate (or JS package) its own "project" — C-p would then only see that
  ;; crate. Keeping just .git/.projectile makes C-p search the entire repo
  ;; (e.g. ~/Work3/dragon) regardless of which crate the file lives in. Drop
  ;; a `.projectile' file somewhere if you ever want a non-git root.
  (setq projectile-project-root-files-bottom-up
        '(".projectile" ".git" ".hg" ".svn"))
  
  ;; Evil keybinding for projectile
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global (kbd "C-p") 'projectile-find-file)))

;; Integrate projectile with persp-mode for per-project workspaces
(use-package persp-mode-projectile-bridge
  :straight t
  :after (persp-mode projectile)
  :hook (persp-mode . persp-mode-projectile-bridge-mode)
  :config
  ;; When switching to a project, create/switch to its perspective
  (setq persp-mode-projectile-bridge-persp-name-prefix "[P]")
  
  ;; Auto-create perspective when switching projects
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (when (and (bound-and-true-p persp-mode)
                         (projectile-project-p))
                (persp-switch (projectile-project-name))))))

;; Ripgrep integration
(use-package ripgrep
  :straight t
  :commands ripgrep-regexp)

;; =============================================================================
;; Treemacs (File Tree Sidebar)
;; =============================================================================

(use-package treemacs
  :straight t
  :defer t
  :commands (treemacs treemacs-select-window)
  :bind (("s-`" . treemacs-select-window)
         ("C-x t t" . +treemacs/toggle)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :config
  ;; Visual settings
  (setq treemacs-width 30
        treemacs-no-png-images t  ; Use text icons for consistency
        treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-missing-project-action 'remove)
  
  ;; File watching
  (treemacs-filewatch-mode t)
  (treemacs-follow-mode -1)  ; Don't auto-follow (can be distracting)
  
  ;; Git integration
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))
  
  ;; Windmove keys in treemacs
  (with-eval-after-load 'evil
    (evil-define-key 'treemacs treemacs-mode-map (kbd "C-h") 'windmove-left)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "C-l") 'windmove-right)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "C-j") 'windmove-down)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "C-k") 'windmove-up)))

(defun +treemacs/toggle ()
  "Toggle treemacs, showing current project only."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (if (project-current)
           (treemacs-add-and-display-current-project-exclusively)
         (treemacs)))))

;; Magit integration
(use-package treemacs-magit
  :straight t
  :after (treemacs magit)
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage) . treemacs-magit--schedule-update))

;; =============================================================================
;; Persp-mode (Workspaces)
;; =============================================================================

(use-package persp-mode
  :straight t
  :hook (after-init . persp-mode)
  :bind (("C-x b" . +buffer/switch-persp)
         :map persp-mode-map
         ("C-c w s" . persp-switch)
         ("C-c w n" . persp-next)
         ("C-c w p" . persp-prev)
         ("C-c w k" . persp-kill)
         ("C-c w r" . persp-rename)
         ("C-c w l" . +persp/list-workspaces)
         ("C-c w b" . +buffer/switch-all))  ; Access ALL buffers
  :init
  (setq persp-keymap-prefix (kbd "C-c w")
        persp-nil-name "⊥")  ; Name for the default perspective
  :config
  ;; Do NOT kill buffers when they're removed from a perspective. With the
  ;; old 'kill-weak, a non-file buffer (like a vterm terminal) got reaped the
  ;; moment its perspective was killed or shuffled — which is exactly how
  ;; long-running agent terminals were silently disappearing. nil keeps them
  ;; alive; reach any of them with :vt / C-c v v.
  (setq persp-autokill-buffer-on-remove nil
        persp-reset-windows-on-nil-window-conf nil
        persp-add-buffer-on-after-change-major-mode t
        persp-set-last-persp-for-new-frames t
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time 0   ; Don't auto-restore (start fresh)
        ;; Don't prompt when killing buffers not in the current perspective.
        ;; org-gcal opens temporary buffers (gcal.org, task files) that may
        ;; not belong to the active perspective — this prevents interactive
        ;; prompts that block automated sync.
        persp-kill-foreign-buffer-behaviour nil)
  
  ;; Filter out temporary buffers from saving
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              (or (not (buffer-live-p b))
                  (string-prefix-p " *" (buffer-name b))
                  (string-prefix-p "*" (buffer-name b)))))
  
  ;; List workspaces
  (defun +persp/list-workspaces ()
    "List all workspaces/perspectives and switch to one."
    (interactive)
    (let* ((persps (persp-names))
           (current (persp-name (get-current-persp)))
           (choice (completing-read
                    (format "Switch workspace (current: %s): " current)
                    persps nil t)))
      (when choice
        (persp-switch choice))))
  
  ;; Custom buffer switcher that filters internal buffers
  (defun +buffer/switch-persp ()
    "Switch buffer within perspective, filtering out internal buffers.
Pressing Enter without input switches to the previous buffer."
    (interactive)
    (let* ((persp (get-current-persp))
           (buffers (if persp
                        (persp-buffers persp)
                      (buffer-list)))
           ;; Filter to useful buffers only
           (useful-buffers (cl-remove-if-not
                            (lambda (b)
                              (and (buffer-live-p b)
                                   (not (eq b (current-buffer)))  ; Exclude current
                                   (or (buffer-file-name b)  ; File buffers always shown
                                       (with-current-buffer b
                                         (derived-mode-p 'shell-mode 'eshell-mode
                                                        'vterm-mode 'term-mode
                                                        'compilation-mode 'dired-mode
                                                        'magit-mode 'org-mode))
                                       ;; Show *scratch* and *Messages*
                                       (member (buffer-name b) '("*scratch*" "*Messages*")))))
                            buffers))
           (buffer-names (mapcar #'buffer-name useful-buffers))
           ;; Default to most recent buffer (first in list after current)
           (default-buffer (car buffer-names)))
      (if buffer-names
          (let* ((prompt (if default-buffer
                             (format "Switch to buffer (default %s): " default-buffer)
                           "Switch to buffer: "))
                 (choice (completing-read prompt buffer-names nil t nil nil default-buffer)))
            (when (and choice (not (string-empty-p choice)))
              (switch-to-buffer choice)))
        (message "No other buffers in perspective"))))
  
  ;; Simple function to show ALL buffers (including hidden)
  (defun +buffer/switch-all ()
    "Switch to any buffer, including hidden internal buffers."
    (interactive)
    (switch-to-buffer (read-buffer "Switch to buffer (all): " nil t))))

;; Treemacs integration with perspectives
(use-package treemacs-persp
  :straight t
  :after (treemacs persp-mode)
  :config
  (treemacs-set-scope-type 'Perspectives))

;; =============================================================================
;; TRAMP (Remote File Editing)
;; =============================================================================

(use-package tramp
  :straight (:type built-in)
  :defer t
  :config
  (setq tramp-default-method "ssh"
        tramp-verbose 1  ; Reduce verbosity
        tramp-auto-save-directory (expand-file-name "tramp-autosave/" user-emacs-directory))
  
  ;; Use remote PATH
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  
  ;; Speed up tramp by using controlmaster
  (setq tramp-use-ssh-controlmaster-options t))

;; =============================================================================
;; Which-key (Keybinding Help)
;; =============================================================================

(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-sort-order 'which-key-key-order-alpha))

;; =============================================================================
;; Symbol Overlay (Highlight symbols)
;; =============================================================================

(use-package symbol-overlay
  :straight t
  :hook ((prog-mode . symbol-overlay-mode))
  :bind (("C-c s i" . symbol-overlay-put)
         ("C-c s n" . symbol-overlay-switch-forward)
         ("C-c s p" . symbol-overlay-switch-backward)
         ("C-c s c" . symbol-overlay-remove-all))
  :config
  (setq symbol-overlay-temp-highlight-on-region t))

;; =============================================================================
;; Helpful (Better help buffers)
;; =============================================================================

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;;; init-tools.el ends here
