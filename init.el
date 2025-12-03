;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Author: Ango Wang
;; Description: Modular Emacs configuration for Emacs 30+

;;; Commentary:
;; This is the main entry point for the Emacs configuration.
;; It loads modular configuration files from the core/ directory.
;;
;; Design Principles:
;; - Lightweight and fast startup
;; - LSP/language servers on-demand (not auto-enabled)
;; - Consistent Evil keybindings across all buffers
;; - Everything is a buffer, navigable with hjkl
;; - Dark (noctilux) and Light (minimal) themes available
;;
;; Module Structure:
;; - init-core.el     : Core settings, macros, utilities
;; - init-straight.el : Package management (straight.el + use-package)
;; - init-ui.el       : UI settings, fonts, ligatures
;; - init-themes.el   : Theme configuration with toggle
;; - init-evil.el     : Evil mode and all keybindings
;; - init-completion.el: Completion framework (company)
;; - init-lsp.el      : LSP-mode (on-demand, C-x l to start)
;; - init-languages.el: Language-specific settings, tree-sitter
;; - init-org.el      : Org-mode configuration
;; - init-tools.el    : Magit, Projectile, Treemacs, etc.

;;; Code:

;; =============================================================================
;; Startup Performance
;; =============================================================================

;; Measure startup time
(defun +emacs/display-startup-time ()
  "Display startup time and GC count."
  (message "Emacs loaded in %.2f seconds with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'+emacs/display-startup-time)

;; Reset GC threshold after startup (set to 16MB for smooth operation)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; =============================================================================
;; Core Settings
;; =============================================================================

;; User info (change as needed)
(setq user-full-name "Ango Wang"
      user-mail-address "ango@weids.dev")

;; Define the core configuration directory
(defvar +emacs-core-dir (expand-file-name "core/" user-emacs-directory)
  "Directory containing modular configuration files.")

;; Helper function to load configuration modules
(defun +load-module (module)
  "Load a configuration MODULE from the core directory."
  (let ((file (expand-file-name (concat (symbol-name module) ".el") +emacs-core-dir)))
    (if (file-exists-p file)
        (load-file file)
      (message "Warning: Module %s not found at %s" module file))))

;; =============================================================================
;; Load Configuration Modules (in order)
;; =============================================================================

;; List of modules to load (order matters)
(defvar +init-modules
  '(init-core       ; Core settings, macros, utilities (load first)
    init-straight   ; Package management
    init-ui         ; UI settings
    init-themes     ; Theme configuration
    init-evil       ; Evil mode and keybindings
    init-completion ; Completion framework
    init-lsp        ; LSP support (on-demand)
    init-languages  ; Language-specific settings
    init-org        ; Org-mode configuration
    init-tools      ; Development tools
    )
  "List of configuration modules to load.")

;; Load all modules
(dolist (module +init-modules)
  (+load-module module))

;; =============================================================================
;; Final Settings
;; =============================================================================

;; These settings are applied after all modules are loaded

;; Disable backup and auto-save files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Silence bell
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Smooth scrolling
(setq scroll-conservatively 101
      scroll-margin 2
      scroll-preserve-screen-position t)

;; Show column number in mode line
(column-number-mode 1)

;; Don't wrap lines by default (extend beyond window)
(set-default 'truncate-lines t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Confirm before quit
(setq confirm-kill-emacs 'y-or-n-p)

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Set PATH from shell (important for LSP servers)
(when (memq window-system '(mac ns x))
  (let ((shell-path (shell-command-to-string "bash -c 'echo -n $PATH'")))
    (setenv "PATH" shell-path)
    (setq exec-path (split-string shell-path ":"))))

;;; init.el ends here
