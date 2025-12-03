;;; init-org.el --- Org-mode configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Org-mode settings for note-taking and publishing

;;; Commentary:
;; This module configures Org-mode for:
;; - Note-taking and organization
;; - Agenda and task management
;; - Clocking and time tracking
;; - HTML export for website publishing
;; - Markdown export
;;
;; Keybindings (set in init-evil.el):
;; - C-c l   : Store link
;; - C-c a   : Org agenda
;; - C-c d   : Set deadline (in org-mode)
;; - C-c s   : Schedule (in org-mode)
;; - TAB     : Cycle visibility (in org-mode)

;;; Code:

;; =============================================================================
;; Org Mode Core
;; =============================================================================

(use-package org
  :straight (:type built-in)
  :defer t
  :config
  
  ;; ---------------------------------------------------------------------------
  ;; Basic Settings
  ;; ---------------------------------------------------------------------------
  
  ;; Log done time
  (setq org-log-done 'time)
  
  ;; Use return to follow links
  (setq org-return-follows-link t)
  
  ;; Hide emphasis markers (*bold*, /italic/, etc.)
  (setq org-hide-emphasis-markers t)
  
  ;; Start with all sections collapsed
  (setq org-startup-folded t)
  
  ;; Indent mode (cleaner look)
  (setq org-startup-indented t)
  
  ;; Source block settings
  (setq org-src-fontify-natively t          ; Syntax highlighting in source blocks
        org-src-tab-acts-natively t         ; Tab works like in the language
        org-src-preserve-indentation t      ; Don't change indentation
        org-edit-src-content-indentation 0) ; No extra indentation in src blocks
  
  ;; ---------------------------------------------------------------------------
  ;; Org Files Location
  ;; ---------------------------------------------------------------------------
  
  ;; Default directory for org files
  (setq org-directory "~/org/")
  
  ;; Agenda files
  (setq org-agenda-files '("~/org/"))
  
  ;; Default notes file
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
  
  ;; ---------------------------------------------------------------------------
  ;; Agenda Settings
  ;; ---------------------------------------------------------------------------
  
  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Daily Agenda" agenda ""
           ((org-agenda-span 'day)))
          ("w" "Weekly Review" agenda ""
           ((org-agenda-span 'week)
            (org-agenda-start-on-weekday 1)))
          ("t" "All TODOs" alltodo "")))
  
  ;; Clock report in agenda
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  
  ;; ---------------------------------------------------------------------------
  ;; Clocking Settings
  ;; ---------------------------------------------------------------------------
  
  ;; Persist clock across sessions
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t
        org-clock-in-resume t
        org-clock-out-when-done t
        org-clock-report-include-clocking-task t)
  
  ;; Remove empty clock drawers
  (defun +org/remove-empty-clock-drawer ()
    "Remove empty LOGBOOK drawers."
    (org-clock-remove-empty-clock-drawer))
  (add-hook 'org-clock-out-hook #'+org/remove-empty-clock-drawer)
  
  ;; ---------------------------------------------------------------------------
  ;; Keybindings (org-mode specific)
  ;; ---------------------------------------------------------------------------
  
  (define-key org-mode-map (kbd "C-c d") 'org-deadline)
  (define-key org-mode-map (kbd "C-c s") 'org-schedule))

;; =============================================================================
;; Org Bullets (prettier headings)
;; =============================================================================

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; =============================================================================
;; Org Export Settings
;; =============================================================================

;; HTML Export
(with-eval-after-load 'ox-html
  (setq org-html-validation-link nil           ; Remove validation link
        org-html-head-include-scripts nil      ; No default scripts
        org-html-head-include-default-style nil ; No default CSS
        org-html-doctype "html5"
        org-html-html5-fancy t))

;; Markdown Export
(with-eval-after-load 'org
  (require 'ox-md))

;; HTMLize for code block syntax highlighting in exports
(use-package htmlize
  :straight t
  :defer t)

;; =============================================================================
;; Org Capture Templates
;; =============================================================================

(with-eval-after-load 'org
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "~/org/tasks.org" "Inbox")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
           "* %? :note:\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\n  Entered on %U\n  %i"))))

;; =============================================================================
;; Org Babel (code execution)
;; =============================================================================

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (C . t))))

;;; init-org.el ends here
