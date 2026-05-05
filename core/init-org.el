;;; init-org.el --- Minimal org-mode configuration (macos branch) -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Ango Wang
;; Description: Plain note-taking with org-mode.  All notes live under
;; ~/orgw/.  No calendar sync, no multi-project scaffolding, no ICS
;; export — just normal org.

;;; Commentary:
;; All .org files anywhere under ~/orgw/ feed into the agenda.
;;
;; Keybindings (see also init-evil.el for global C-c l/a/c):
;;   C-c a   Org agenda
;;   C-c c   Org capture
;;   C-c d   Set deadline   (org-mode)
;;   C-c s   Schedule       (org-mode)

;;; Code:

(defvar +org/root (expand-file-name "~/orgw/")
  "Root directory for all org files on this machine.")

;; Make sure the root exists so first-launch capture/agenda don't error.
(unless (file-directory-p +org/root)
  (make-directory +org/root t))

(use-package org
  :straight (:type built-in)
  :defer t
  :config
  ;; ── Basics ─────────────────────────────────────────────────────────
  (setq org-directory +org/root
        org-default-notes-file (expand-file-name "notes.org" +org/root)
        org-log-done 'time
        org-log-into-drawer t
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-startup-folded t
        org-startup-indented t
        org-adapt-indentation nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0)
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

  ;; ── TODO keywords ──────────────────────────────────────────────────
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@/!)" "|"
                    "DONE(d!)" "CANCELLED(c@)"))
        org-todo-keyword-faces
        '(("TODO"        . (:foreground "#ff6c6b" :weight bold))
          ("IN-PROGRESS" . (:foreground "#ECBE7B" :weight bold))
          ("WAITING"     . (:foreground "#da8548" :weight bold))
          ("DONE"        . (:foreground "#98be65" :weight bold))
          ("CANCELLED"   . (:foreground "#5B6268" :weight bold))))

  ;; ── Agenda: every .org file under ~/orgw/ (recursive) ────────────
  (setq org-agenda-files
        (when (file-directory-p +org/root)
          (directory-files-recursively +org/root "\\.org\\'"))
        org-agenda-span 'day
        org-agenda-start-on-weekday 1
        org-agenda-window-setup 'current-window
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)

  ;; ── Capture: TODO and free-form Note, both into notes.org ────────
  (setq org-capture-templates
        `(("t" "TODO" entry
           (file+headline ,org-default-notes-file "Inbox")
           "* TODO %?\n  %i" :empty-lines 1)
          ("n" "Note" entry
           (file+headline ,org-default-notes-file "Notes")
           "* %?\n  %i" :empty-lines 1)))

  ;; ── Clocking ───────────────────────────────────────────────────────
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t
        org-clock-in-resume t
        org-clock-out-when-done t
        org-clock-idle-time 15
        org-clock-out-remove-zero-time-clocks t)

  ;; ── Per-mode keys ──────────────────────────────────────────────────
  (define-key org-mode-map (kbd "C-c d") 'org-deadline)
  (define-key org-mode-map (kbd "C-c s") 'org-schedule))

;; Prettier headings
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

;;; init-org.el ends here
