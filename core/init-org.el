;;; init-org.el --- Org-mode configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Ango Wang
;; Description: Org-mode for task management, time tracking, calendar sync

;;; Commentary:
;; Multi-project org workflow:
;;
;;   Tasks (centralized — these are the only agenda sources):
;;     ~/org/weids/tasks.org      Weids startup (verifiable computing)
;;     ~/org/zynerise/tasks.org   Zynerise collab startup
;;     ~/org/agenda/tasks.org     Personal / legacy
;;     ~/org/gcal.org             Google Calendar sync (org-gcal)
;;
;;   Notes (distributed — any .org file in these dirs):
;;     ~/org/weids/*.org          ~/org/zynerise/*.org
;;     ~/org/agenda/*.org
;;
;; See ~/.emacs.d/docs.md for full usage documentation.
;;
;; Keybindings (see also init-evil.el):
;;   C-c l     Store link
;;   C-c a     Org agenda
;;   C-c c     Org capture
;;   C-c d     Set deadline     (org-mode)
;;   C-c s     Schedule         (org-mode)
;;   TAB       Cycle visibility (org-mode)

;;; Code:

;; =============================================================================
;; Org Directory Structure
;; =============================================================================

(defvar +org/weids-dir    (expand-file-name "weids/"    "~/org/"))
(defvar +org/zynerise-dir (expand-file-name "zynerise/" "~/org/"))
(defvar +org/agenda-dir   (expand-file-name "agenda/"   "~/org/"))

;; Central task files (one per project)
(defvar +org/weids-tasks    (expand-file-name "tasks.org" +org/weids-dir))
(defvar +org/zynerise-tasks (expand-file-name "tasks.org" +org/zynerise-dir))
(defvar +org/personal-tasks (expand-file-name "tasks.org" +org/agenda-dir))
(defvar +org/gcal-file      (expand-file-name "gcal.org"  "~/org/"))

(use-package org
  :straight (:type built-in)
  :defer t
  :config

  ;; ---------------------------------------------------------------------------
  ;; Basic Settings
  ;; ---------------------------------------------------------------------------

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)            ; Put state change logs into LOGBOOK drawer
  (setq org-return-follows-link t)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded t)
  (setq org-startup-indented t)

  ;; List / body indentation (fix for equal-indent lists)
  (setq org-adapt-indentation nil)
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

  ;; Source block settings
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0)

  ;; ---------------------------------------------------------------------------
  ;; TODO Keywords
  ;; ---------------------------------------------------------------------------
  ;; C-c C-t  cycles through states. Fast-access keys in parens.
  ;; ! = log timestamp, @ = prompt for note

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

  (setq org-todo-keyword-faces
        '(("TODO"        . (:foreground "#ff6c6b" :weight bold))
          ("IN-PROGRESS" . (:foreground "#ECBE7B" :weight bold))
          ("WAITING"     . (:foreground "#da8548" :weight bold))
          ("DONE"        . (:foreground "#98be65" :weight bold))
          ("CANCELLED"   . (:foreground "#5B6268" :weight bold))))

  ;; ---------------------------------------------------------------------------
  ;; Org Files & Agenda Sources
  ;; ---------------------------------------------------------------------------

  (setq org-directory "~/org/")

  ;; Only the 3 central task files + gcal feed into agenda.
  ;; Notes files in each directory are NOT included — they stay free-form.
  (setq org-agenda-files
        (list +org/weids-tasks
              +org/zynerise-tasks
              +org/personal-tasks
              +org/gcal-file))

  (setq org-default-notes-file (expand-file-name "notes.org" +org/agenda-dir))

  ;; ---------------------------------------------------------------------------
  ;; Agenda Views
  ;; ---------------------------------------------------------------------------

  (setq org-agenda-span 'day)
  (setq org-agenda-start-on-weekday 1)            ; Monday
  (setq org-agenda-include-diary nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3 :fileskip0 t))
  (setq org-agenda-default-appointment-duration 60) ; 1-hour default for ICS export

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "Today")))
            (todo "IN-PROGRESS" ((org-agenda-overriding-header "In Progress")))
            (todo "WAITING"     ((org-agenda-overriding-header "Waiting On")))
            (todo "TODO"        ((org-agenda-overriding-header "Backlog")
                                 (org-agenda-sorting-strategy '(priority-down))))))

          ("W" "Weids Tasks"
           ((tags-todo "+weids"
                       ((org-agenda-overriding-header "Weids Tasks")))))

          ("Z" "Zynerise Tasks"
           ((tags-todo "+zynerise"
                       ((org-agenda-overriding-header "Zynerise Tasks")))))

          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-overriding-header "This Week")))
            (todo "DONE"
                  ((org-agenda-overriding-header "Completed This Week")))))

          ("t" "All TODOs" alltodo "")))

  ;; ---------------------------------------------------------------------------
  ;; Tags
  ;; ---------------------------------------------------------------------------

  (setq org-tag-alist
        '((:startgroup)
          ("weids"    . ?w)
          ("zynerise" . ?z)
          ("personal" . ?p)
          (:endgroup)
          ("meeting"  . ?m)
          ("deadline" . ?d)
          ("idea"     . ?i)
          ("bug"      . ?b)))

  ;; ---------------------------------------------------------------------------
  ;; Clocking
  ;; ---------------------------------------------------------------------------

  (org-clock-persistence-insinuate)
  (setq org-clock-persist t
        org-clock-in-resume t
        org-clock-out-when-done t
        org-clock-report-include-clocking-task t
        org-clock-idle-time 15              ; Ask after 15 min idle
        org-clock-out-remove-zero-time-clocks t
        org-clock-history-length 25)

  ;; Remove empty clock drawers
  (defun +org/remove-empty-clock-drawer ()
    "Remove empty LOGBOOK drawers."
    (org-clock-remove-empty-clock-drawer))
  (add-hook 'org-clock-out-hook #'+org/remove-empty-clock-drawer)

  ;; ---------------------------------------------------------------------------
  ;; Custom Clock Helpers
  ;; ---------------------------------------------------------------------------

  (defun +org/clock-out-all ()
    "Force clock-out of any running clock, no matter what buffer."
    (interactive)
    (if (org-clocking-p)
        (progn
          (org-clock-out)
          (message "Clocked out of: %s" org-clock-heading))
      (message "No active clock.")))

  (defun +org/clock-in-last ()
    "Re-clock the most recent task from clock history."
    (interactive)
    (org-clock-in-last))

  (defun +org/clock-report ()
    "Show a clock report for today in a temporary buffer."
    (interactive)
    (org-clock-display))

  ;; Global keybindings for clock commands
  (global-set-key (kbd "C-c C-x o") #'+org/clock-out-all)

  ;; ---------------------------------------------------------------------------
  ;; ICS Export (ox-icalendar)
  ;; ---------------------------------------------------------------------------

  ;; Export combined agenda to a single .ics file for Thunderbird subscription
  (setq org-icalendar-combined-agenda-file (expand-file-name "~/org/calendar.ics"))
  (setq org-icalendar-include-todo t)
  (setq org-icalendar-use-deadline '(event-if-not-todo todo-due))
  (setq org-icalendar-use-scheduled '(event-if-not-todo todo-start))
  (setq org-icalendar-store-UID t)
  (setq org-icalendar-timezone "Asia/Hong_Kong")
  (setq org-icalendar-alarm-time 15)         ; 15 min reminder

  ;; Auto-export ICS after every sync / save of agenda files
  (defun +org/export-ics ()
    "Export all agenda files to ~/org/calendar.ics."
    (interactive)
    (org-icalendar-combine-agenda-files)
    (message "Exported calendar to ~/org/calendar.ics"))

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
  (setq org-html-validation-link nil
        org-html-head-include-scripts nil
        org-html-head-include-default-style nil
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
        `(;; ── Quick Inbox ──────────────────────────────────────────────
          ("t" "Quick TODO" entry
           (file+headline ,+org/personal-tasks "Inbox")
           "* TODO %?\n  %i\n  %a"
           :empty-lines 1)

          ;; ── Weids ────────────────────────────────────────────────────
          ("w" "Weids")
          ("wt" "Weids TODO" entry
           (file+headline ,+org/weids-tasks "Inbox")
           "* TODO %? :weids:\n  %i\n  %a"
           :empty-lines 1)
          ("wn" "Weids Note" entry
           (file+headline ,(expand-file-name "notes.org" +org/weids-dir) "Notes")
           "* %? :weids:note:\n  %i\n  %a")
          ("wm" "Weids Meeting" entry
           (file+headline ,+org/weids-tasks "Meetings")
           "* %? :weids:meeting:\n  %^T\n** Attendees\n   - \n** Notes\n   \n** Action Items\n   - [ ] "
           :clock-in t :clock-resume t)

          ;; ── Zynerise ─────────────────────────────────────────────────
          ("z" "Zynerise")
          ("zt" "Zynerise TODO" entry
           (file+headline ,+org/zynerise-tasks "Inbox")
           "* TODO %? :zynerise:\n  %i\n  %a"
           :empty-lines 1)
          ("zn" "Zynerise Note" entry
           (file+headline ,(expand-file-name "notes.org" +org/zynerise-dir) "Notes")
           "* %? :zynerise:note:\n  %i\n  %a")
          ("zm" "Zynerise Meeting" entry
           (file+headline ,+org/zynerise-tasks "Meetings")
           "* %? :zynerise:meeting:\n  %^T\n** Attendees\n   - \n** Notes\n   \n** Action Items\n   - [ ] "
           :clock-in t :clock-resume t)

          ;; ── Personal ─────────────────────────────────────────────────
          ("n" "Note" entry
           (file+headline ,(expand-file-name "notes.org" +org/agenda-dir) "Notes")
           "* %? :note:\n  %i\n  %a")

          ("j" "Journal" entry
           (file+olp+datetree ,(expand-file-name "journal.org" "~/org/"))
           "* %?\n  Entered on %U\n  %i")

          ;; ── Calendar / Appointment ───────────────────────────────────
          ("a" "Appointment" entry
           (file ,+org/gcal-file)
           "* %?\n  %^T\n"
           :empty-lines 1))))

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

;; =============================================================================
;; org-gcal (Google Calendar two-way sync)
;; =============================================================================
;;
;; See ~/.emacs.d/docs.md for setup instructions.
;; Credentials are set in :init (before load) so oauth2-auto can register
;; the provider at load time.

(use-package org-gcal
  :straight t
  :after org
  :commands (org-gcal-fetch org-gcal-sync org-gcal-post-at-point org-gcal-delete-at-point)
  :init
  ;; Load credentials from secrets.el (not tracked by git).
  ;; Must be set BEFORE org-gcal loads so oauth2-auto can find the provider.
  (let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
    (if (file-exists-p secrets-file)
        (load-file secrets-file)
      (message "Warning: %s not found. org-gcal credentials not set. See docs.md." secrets-file)))
  :config
  ;; Map each Google Calendar to an org file
  (setq org-gcal-fetch-file-alist
        `(("awang@weids.dev" . ,+org/gcal-file)))

  ;; Timezone
  (setq org-gcal-local-timezone "Asia/Hong_Kong")

  ;; Fetch range: 30 days back, 90 days forward
  (setq org-gcal-up-days 30
        org-gcal-down-days 90)

  ;; Don't prompt for GPG every time
  (setq plstore-cache-passphrase-for-symmetric-encryption t)

  ;; Pre-populate the passphrase cache for the oauth2 plstore so GPG never
  ;; prompts interactively.  This is safe — the file is local-only and the
  ;; OAuth credentials are already visible in this config.
  (require 'plstore)
  (let ((file (file-truename (expand-file-name "oauth2-auto.plist" user-emacs-directory))))
    (unless (assoc file plstore-passphrase-alist)
      (push (cons file "emacs") plstore-passphrase-alist)))

  ;; Work around oauth2-auto issue #6: the debug advice on `insert' can
  ;; mistakenly fire and abort the plstore save.
  ;; We patch plstore-write to store tokens as PUBLIC entries (no GPG
  ;; encryption needed).
  (defun +org-gcal/plstore-write-public (username provider plist)
    "Save PLIST for USERNAME/PROVIDER to plstore as public (unencrypted) data.
This avoids both GPG passphrase prompts and oauth2-auto issue #6."
    (let ((id (oauth2-auto--compute-id username provider))
          (plstore (plstore-open oauth2-auto-plstore)))
      (unwind-protect
          (prog1 plist
            ;; Store as public (2nd arg) not secret (3rd arg)
            (plstore-put plstore id plist nil)
            (when (buffer-live-p (plstore--get-buffer plstore))
              (plstore-save plstore)
              (puthash id plist oauth2-auto--plstore-cache)))
        (plstore-close plstore))))

  (advice-add 'oauth2-auto--plstore-write :override #'+org-gcal/plstore-write-public)

  ;; Use manual-auth mode: shows URL to paste into browser, then asks
  ;; for the authorization code. Works reliably in terminal / WSL2
  ;; without needing browse-url to open a local redirect listener.
  (setq oauth2-auto-manually-auth t)

  ;; Auto-export ICS after syncing
  (add-hook 'org-gcal-after-update-entry-functions
            (lambda (&rest _) (+org/export-ics)))

  ;; Auto-sync: fetch on startup, two-way sync every 15 minutes
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-idle-timer 10 nil #'org-gcal-fetch)
              (run-with-timer 900 900 #'org-gcal-sync))))

;;; init-org.el ends here
