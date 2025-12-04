;;; init-tools.el --- Development tools -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Development tools: Git, project management, file tree

;;; Commentary:
;; This module configures development tools:
;; - Magit: Git interface
;; - Projectile: Project management
;; - Treemacs: File tree sidebar
;; - Persp-mode: Workspace/perspective management
;; - TRAMP: Remote file editing

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
  :bind (("C-c t" . +eshell/toggle)
         ("C-c T" . +eshell/new))
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
;; Magit (Git Interface)
;; =============================================================================

(use-package magit
  :straight t
  :commands (magit-status magit-dispatch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  ;; Show diff when committing
  (setq magit-commit-show-diff t)
  
  ;; Don't ask before saving buffers
  (setq magit-save-repository-buffers 'dontask)
  
  ;; Show granular diffs
  (setq magit-diff-refine-hunk 'all))

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
  
  ;; Use native indexing for better performance
  (setq projectile-indexing-method 'hybrid
        projectile-enable-caching t
        projectile-sort-order 'recently-active)
  
  ;; Recognize common project roots
  (setq projectile-project-root-files-bottom-up
        '(".projectile" ".git" "Cargo.toml" "go.mod" "package.json"
          "pyproject.toml" "setup.py" "Makefile" "CMakeLists.txt"))
  
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
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-add-buffer-on-after-change-major-mode t
        persp-set-last-persp-for-new-frames t
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time 0)  ; Don't auto-restore (start fresh)
  
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
