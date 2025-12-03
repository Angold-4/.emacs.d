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
