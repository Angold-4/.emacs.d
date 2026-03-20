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
;;   C-c f     Cleanup: refile DONE tasks, normalize formatting (org-mode)
;;   C-c o w   Open weids/tasks.org       (global)
;;   C-c o z   Open zynerise/tasks.org    (global)
;;   C-c o a   Open agenda/tasks.org      (global)
;;   C-c o g   Open gcal.org              (global)
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

  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)

  ;; Skip org-managed mirror entries in gcal.org from the agenda.
  ;; These are duplicates of task file entries — we only want to show
  ;; entries that came from Google Calendar (managed = "gcal") or have
  ;; no managed property (manual entries).
  (defun +org/agenda-skip-gcal-mirrors ()
    "Skip gcal.org entries that are org-managed mirrors of task file entries."
    (when (and buffer-file-name
               (string= (file-truename buffer-file-name)
                         (file-truename +org/gcal-file)))
      (let ((managed (org-entry-get nil "org-gcal-managed"))
            (source-id (org-entry-get nil "source-id")))
        ;; Skip if: managed = "org" (created from task files), OR
        ;; has a :source-id: (our mirror system)
        (when (or (string= managed "org")
                  source-id)
          (org-end-of-subtree t)))))

  (setq org-agenda-skip-function-global #'+org/agenda-skip-gcal-mirrors)

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
  (define-key org-mode-map (kbd "C-c s") 'org-schedule)
  (define-key org-mode-map (kbd "C-c f") '+org/cleanup))

;; =============================================================================
;; Quick Task File Navigation (C-c o prefix — global)
;; =============================================================================

(defvar +org/nav-map (make-sparse-keymap)
  "Keymap for quick navigation to org task files.")

(global-set-key (kbd "C-c o") +org/nav-map)

(define-key +org/nav-map (kbd "w")
  (lambda () (interactive) (find-file +org/weids-tasks)))
(define-key +org/nav-map (kbd "z")
  (lambda () (interactive) (find-file +org/zynerise-tasks)))
(define-key +org/nav-map (kbd "a")
  (lambda () (interactive) (find-file +org/personal-tasks)))
(define-key +org/nav-map (kbd "g")
  (lambda () (interactive) (find-file +org/gcal-file)))

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
           "* TODO %?\n  %i"
           :empty-lines 1)

          ;; ── Weids ────────────────────────────────────────────────────
          ("w" "Weids")
          ("wt" "Weids TODO" entry
           (file+headline ,+org/weids-tasks "Inbox")
           "* TODO %? :weids:"
           :empty-lines 1)
          ("wn" "Weids Note" entry
           (file+headline ,(expand-file-name "notes.org" +org/weids-dir) "Notes")
           "* %? :weids:note:\n  %i\n  %a")
          ("wm" "Weids Meeting" entry
           (file+headline ,+org/weids-tasks "Inbox")
           "* TODO %? :weids:meeting:\n  %^T\n** Attendees\n   - \n** Notes\n   \n** Action Items\n   - [ ] "
           :clock-in t :clock-resume t)

          ;; ── Zynerise ─────────────────────────────────────────────────
          ("z" "Zynerise")
          ("zt" "Zynerise TODO" entry
           (file+headline ,+org/zynerise-tasks "Inbox")
           "* TODO %? :zynerise:"
           :empty-lines 1)
          ("zn" "Zynerise Note" entry
           (file+headline ,(expand-file-name "notes.org" +org/zynerise-dir) "Notes")
           "* %? :zynerise:note:\n  %i\n  %a")
          ("zm" "Zynerise Meeting" entry
           (file+headline ,+org/zynerise-tasks "Inbox")
           "* TODO %? :zynerise:meeting:\n  %^T\n** Attendees\n   - \n** Notes\n   \n** Action Items\n   - [ ] "
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
;; Cleanup: refile DONE tasks and normalize formatting  (C-c f)
;; =============================================================================

(defun +org/cleanup ()
  "Refile DONE/CANCELLED tasks from Inbox to Completed, then normalize
the buffer formatting (indentation, blank lines).

Only operates on task files with Inbox and Completed headings."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  ;; ── Find Inbox heading ──
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Inbox$" nil t)
      (user-error "No \"* Inbox\" heading found in this file")))

  ;; ── Ensure Completed heading exists ──
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Completed$" nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* Completed\n")))

  ;; ── Collect and move DONE/CANCELLED entries from Inbox ──
  ;; We extract subtrees bottom-up to keep positions stable, then
  ;; insert them under Completed.
  (let ((entries nil)
        (moved 0))

    ;; Pass 1: collect entries to move (bottom-up from Inbox)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^\\* Inbox$" nil t)
      (let ((inbox-beg (line-beginning-position))
            (inbox-end (save-excursion (org-end-of-subtree t) (point))))
        ;; Find all level-2 headings inside Inbox
        (goto-char inbox-end)
        (while (re-search-backward "^\\*\\* " inbox-beg t)
          (let ((state (org-get-todo-state)))
            (when (member state '("DONE" "CANCELLED"))
              (let* ((entry-beg (line-beginning-position))
                     (entry-end (save-excursion
                                  (org-end-of-subtree t)
                                  ;; Include trailing blank line if present
                                  (when (looking-at "\n") (forward-char 1))
                                  (point)))
                     (text (buffer-substring entry-beg entry-end)))
                (push (cons entry-beg (cons entry-end text)) entries)))))))

    ;; Pass 2: delete entries from Inbox (bottom-up so positions stay valid)
    (let ((sorted (sort (copy-sequence entries)
                        (lambda (a b) (> (car a) (car b))))))
      (dolist (entry sorted)
        (delete-region (car entry) (cadr entry))
        (setq moved (1+ moved))))

    ;; Pass 3: insert under Completed
    (when entries
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* Completed$" nil t)
        (let ((insert-pos (save-excursion (org-end-of-subtree t) (point))))
          (goto-char insert-pos)
          (unless (bolp) (insert "\n"))
          (dolist (entry entries)
            (insert (cddr entry))
            ;; Ensure blank line after each entry
            (unless (bolp) (insert "\n"))))))

    ;; Pass 4: sort Completed section by CLOSED timestamp (most recent first)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* Completed$" nil t)
        (let ((completed-beg (point))
              (completed-end (save-excursion (org-end-of-subtree t) (point)))
              subtrees)
          ;; Collect all level-2 subtrees with their CLOSED timestamps
          (goto-char completed-beg)
          (while (re-search-forward "^\\*\\* " completed-end t)
            (let* ((entry-beg (line-beginning-position))
                   (entry-end (save-excursion
                                (org-end-of-subtree t)
                                (when (looking-at "\n") (forward-char 1))
                                (point)))
                   (text (buffer-substring entry-beg entry-end))
                   ;; Extract CLOSED timestamp from the entry text
                   (closed-time
                    (if (string-match
                         "CLOSED: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]+ [0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]"
                         text)
                        (float-time
                         (org-time-string-to-time (match-string 1 text)))
                      0.0)))
              (push (list closed-time entry-beg entry-end text) subtrees)))
          ;; Sort by CLOSED time, most recent first
          (setq subtrees (sort subtrees (lambda (a b) (> (car a) (car b)))))
          ;; Replace the Completed section content
          (when subtrees
            ;; Delete old content (after the Completed heading line)
            (goto-char completed-beg)
            (forward-line 1)
            (let ((content-beg (point)))
              (goto-char (save-excursion
                           (goto-char completed-beg)
                           (org-end-of-subtree t)
                           (point)))
              (delete-region content-beg (point))
              ;; Re-insert sorted entries
              (goto-char content-beg)
              (dolist (entry subtrees)
                (insert (nth 3 entry))
                (unless (bolp) (insert "\n"))))))))

    ;; ── Normalize formatting ──

    ;; 1. Remove excessive blank lines (3+ consecutive → 2)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\n\\(\n\\)\\{2,\\}" nil t)
        (replace-match "\n\n")))

    ;; 2. Ensure file ends with a single newline
    (save-excursion
      (goto-char (point-max))
      ;; Remove trailing blank lines
      (when (re-search-backward "[^ \t\n]" nil t)
        (forward-char 1)
        (delete-region (point) (point-max))
        (insert "\n")))

    (save-buffer)
    (if (> moved 0)
        (message "Cleaned up: refiled %d task%s to Completed"
                 moved (if (= moved 1) "" "s"))
      (message "Cleaned up: no tasks to refile"))))

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
  :defer t
  :init
  ;; Load org-gcal after org is available.
  ;; :after org doesn't work reliably with straight.el + built-in org,
  ;; so we use with-eval-after-load directly.
  (with-eval-after-load 'org
    (require 'org-gcal))
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

  ;; Rebuild org-gcal byte-code if it was compiled against a stale org version.
  ;; Prevents "invalid-function org-element-with-disabled-cache" errors.
  (unless (fboundp 'org-element-with-disabled-cache)
    ;; org 9.7+ removed this macro; org-gcal was compiled with it.
    ;; Force a rebuild so it picks up the current org API.
    (when (fboundp 'straight-rebuild-package)
      (straight-rebuild-package "org-gcal")))

  ;; ---------------------------------------------------------------------------
  ;; Push scheduled tasks to Google Calendar
  ;; ---------------------------------------------------------------------------
  ;;
  ;; Flow:
  ;;   1. C-c C-s (schedule with time) → creates entry in gcal.org and
  ;;      calls org-gcal-post-at-point to push it to Google Calendar.
  ;;   2. You work on the task, clocking in/out as you go.
  ;;   3. C-c C-t → DONE → the gcal.org entry is updated with actual
  ;;      clock time and re-posted to Google Calendar.
  ;;
  ;; If no clock data exists when marked DONE, the scheduled time is kept.

  ;; Don't prompt when posting org-managed entries — just push.
  (setq org-gcal-managed-post-at-point-update-existing 'always-push)

  ;; ---------------------------------------------------------------------------
  ;; Suppress interactive prompts during automated org-gcal operations
  ;; ---------------------------------------------------------------------------
  ;;
  ;; org-gcal (via org-generic-id) scans agenda files during sync, adding
  ;; :entry-id: properties which dirty buffers. When org-gcal cleans up,
  ;; this triggers save-some-buffers prompts. We suppress these by:
  ;;   1. Auto-saving any org buffers that were dirtied
  ;;   2. Suppressing yes-or-no-p / y-or-n-p during automated operations

  (require 'cl-lib)

  (defun +org-gcal/post-at-point-no-prompts ()
    "Call `org-gcal-post-at-point' suppressing all interactive prompts.
Auto-saves any org buffers dirtied by the operation."
    (let ((modified-buffers
           ;; Track which org buffers are clean before the operation
           (cl-remove-if #'buffer-modified-p
                         (cl-remove-if-not
                          (lambda (b)
                            (and (buffer-file-name b)
                                 (string-suffix-p ".org" (buffer-file-name b))))
                          (buffer-list)))))
      ;; Suppress interactive prompts during the operation
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (org-gcal-post-at-point nil nil))
      ;; Silently save any org buffers that were clean but got dirtied
      (dolist (buf modified-buffers)
        (when (and (buffer-live-p buf)
                   (buffer-modified-p buf))
          (with-current-buffer buf
            (save-buffer))))))

  (defun +org-gcal/find-mirror (source-id &optional heading)
    "Find the position of a mirror entry in gcal.org.
First searches by :source-id: property (stable). Falls back to heading
text match if SOURCE-ID is nil or not found, and HEADING is provided.
When falling back to heading, prefers entries with :org-gcal-managed: org
that are NOT under * Completed.
Returns the point at the beginning of the heading, or nil."
    (with-current-buffer (find-file-noselect +org/gcal-file)
      (save-excursion
        (goto-char (point-min))
        ;; Try source-id first
        (let ((found nil))
          (when source-id
            (while (and (not found)
                        (re-search-forward
                         (format ":source-id:\\s-+%s" (regexp-quote source-id))
                         nil t))
              (org-back-to-heading t)
              (setq found (point))))
          ;; Fall back to heading text match — find an org-managed mirror
          ;; not under * Completed
          (when (and (not found) heading)
            (goto-char (point-min))
            ;; Find the boundary of * Completed (if any) so we skip it
            (let ((completed-beg (save-excursion
                                   (when (re-search-forward "^\\* Completed$" nil t)
                                     (line-beginning-position)))))
              (goto-char (point-min))
              (while (and (not found)
                          (search-forward heading nil t))
                (org-back-to-heading t)
                (let ((pos (point)))
                  ;; Only match if this is a top-level heading, org-managed,
                  ;; and not inside * Completed
                  (when (and (= (org-outline-level) 1)
                             (string= (or (org-entry-get nil "org-gcal-managed") "") "org")
                             (or (null completed-beg) (< pos completed-beg)))
                    (setq found pos))
                  ;; Move past this heading to continue searching
                  (org-end-of-subtree t)))))
          found))))

  (defun +org-gcal/clock-range ()
    "Return (start-time . end-time) from clock entries of current heading.
Start = earliest clock-in, End = latest clock-out.
Returns nil if no complete clock entries found."
    (let ((start nil) (end nil))
      (save-excursion
        (org-back-to-heading t)
        (let ((bound (save-excursion (org-end-of-subtree t) (point))))
          (while (re-search-forward
                  "CLOCK: \\[\\([0-9-]+ [A-Za-z]+ [0-9:]+\\)\\]--\\[\\([0-9-]+ [A-Za-z]+ [0-9:]+\\)\\]"
                  bound t)
            ;; Capture match strings immediately before anything can clobber match data
            (let* ((start-str (match-string-no-properties 1))
                   (end-str   (match-string-no-properties 2))
                   (clock-start (org-time-string-to-time start-str))
                   (clock-end   (org-time-string-to-time end-str)))
              (when (or (null start) (time-less-p clock-start start))
                (setq start clock-start))
              (when (or (null end) (time-less-p end clock-end))
                (setq end clock-end))))))
      (when (and start end)
        (cons start end))))

  (defun +org-gcal/scheduled-to-gcal-ts (scheduled)
    "Convert org SCHEDULED string to org-gcal range timestamp.
Handles these org timestamp formats:
  <2026-03-20 Fri 14:00-15:00>  -> <2026-03-20 Fri 14:00>--<2026-03-20 Fri 15:00>
  <2026-03-20 Fri 14:00>        -> <2026-03-20 Fri 14:00>--<2026-03-20 Fri 15:00> (1h default)
Returns the range string, or nil if no time component found."
    (let ((ts (string-trim scheduled)))
      (cond
       ;; Time range: <2026-03-20 Fri 14:00-15:00>
       ((string-match
         "<\\([0-9-]+ [A-Za-z]+ \\)\\([0-9]+:[0-9]+\\)-\\([0-9]+:[0-9]+\\)>"
         ts)
        (let ((date-prefix (match-string 1 ts))
              (start-time (match-string 2 ts))
              (end-time (match-string 3 ts)))
          (format "<%s%s>--<%s%s>"
                  date-prefix start-time
                  date-prefix end-time)))
       ;; Single time: <2026-03-20 Fri 14:00> -> add 1 hour
       ((string-match
         "<\\([0-9-]+ [A-Za-z]+ \\)\\([0-9]+\\):\\([0-9]+\\)>"
         ts)
        (let* ((date-prefix (match-string 1 ts))
               (hour (string-to-number (match-string 2 ts)))
               (minute (string-to-number (match-string 3 ts)))
               (end-hour (+ hour 1))
               (end-minute minute))
          (when (>= end-hour 24) (setq end-hour (- end-hour 24)))
          (format "<%s%02d:%02d>--<%s%02d:%02d>"
                  date-prefix hour minute
                  date-prefix end-hour end-minute)))
       (t nil))))

  (defun +org-gcal/push-to-calendar (&optional heading scheduled source-id)
    "Create/update a mirror entry in gcal.org and post it to Google Calendar.
When called with HEADING, SCHEDULED, and SOURCE-ID, uses those values
directly instead of reading from point (for use from timer callbacks).
SOURCE-ID is the org-id of the source task entry — used to link the
mirror back to the task stably (survives heading renames)."
    (interactive)
    (let ((heading (or heading
                       (and (derived-mode-p 'org-mode)
                            (org-get-heading t t t t))))
          (scheduled (or scheduled
                         (and (derived-mode-p 'org-mode)
                              (org-entry-get nil "SCHEDULED"))))
          (source-id (or source-id
                         (and (derived-mode-p 'org-mode)
                              (org-id-get-create))))
          (source-file (buffer-file-name)))
      ;; Only act on task files (not gcal.org itself), with a timed schedule
      (when (and heading scheduled source-id
                 (or (not source-file)
                     (not (string= (file-truename source-file)
                                   (file-truename +org/gcal-file))))
                 (string-match "[0-9]\\{2\\}:[0-9]\\{2\\}" scheduled))
        (let ((gcal-ts (+org-gcal/scheduled-to-gcal-ts scheduled)))
          (when gcal-ts
            (save-window-excursion
              (let ((gcal-buf (find-file-noselect +org/gcal-file))
                    (mirror-pos nil))
                (with-current-buffer gcal-buf
                  ;; Find existing mirror by source-id, fall back to heading text
                  (let ((existing (+org-gcal/find-mirror source-id heading)))
                    (if existing
                        ;; Update existing mirror
                        (progn
                          (goto-char existing)
                          (setq mirror-pos existing)
                          ;; Update heading text (may have changed)
                          (when (looking-at "^\\*+ \\(.+\\)$")
                            (replace-match (format "* %s" heading) t t))
                          ;; Update timestamp
                          (let ((bound (save-excursion (org-end-of-subtree t) (point))))
                            (when (re-search-forward
                                   "<[0-9][^>]*>\\(--<[^>]+>\\)?" bound t)
                              (replace-match gcal-ts t t))))
                      ;; Create new entry at end of file (before * Completed if it exists)
                      (goto-char (point-min))
                      (let ((insert-pt (if (re-search-forward "^\\* Completed$" nil t)
                                           (line-beginning-position)
                                         (point-max))))
                        (goto-char insert-pt)
                        (unless (bolp) (insert "\n"))
                        (insert (format "\n* %s\n:PROPERTIES:\n:calendar-id: awang@weids.dev\n:org-gcal-managed: org\n:source-id: %s\n:END:\n:org-gcal:\n%s\n:END:\n"
                                        heading source-id gcal-ts)))
                      ;; Move to the new heading
                      (re-search-backward (format "^\\* %s$" (regexp-quote heading)) nil t)
                      (setq mirror-pos (point))))
                  ;; Ensure required properties are set
                  (goto-char mirror-pos)
                  (unless (org-entry-get nil "calendar-id")
                    (org-entry-put nil "calendar-id" "awang@weids.dev"))
                  (unless (org-entry-get nil "source-id")
                    (org-entry-put nil "source-id" source-id))
                  ;; Clear stale ETag so org-gcal sends PATCH without
                  ;; If-Match — avoids 412 when we've updated the timestamp
                  ;; locally.  org-gcal will store the fresh ETag from the
                  ;; server response automatically.
                  (org-entry-delete nil "ETag")
                  (save-buffer)
                  ;; Post to Google Calendar
                  (goto-char mirror-pos)
                  (+org-gcal/post-at-point-no-prompts)
                  (message "Pushed to Google Calendar: %s" heading)))))))))

  (defun +org-gcal/update-on-done ()
    "When a task is marked DONE, update its gcal.org mirror with actual clock time.
Captures all data (heading, source-id, clock range) immediately -- before
any refile moves the entry -- then pushes via idle timer."
    (condition-case err
        (when (and (member org-state '("DONE" "CANCELLED"))
                   (not (member org-last-state '("DONE" "CANCELLED")))
                   (buffer-file-name)
                   (not (string= (file-truename (buffer-file-name))
                                  (file-truename +org/gcal-file))))
          ;; Capture everything NOW, before refile moves the entry
          (let* ((heading (org-get-heading t t t t))
                 (source-id (org-entry-get nil "ID"))
                 (clock-range (condition-case clock-err
                                  (+org-gcal/clock-range)
                                (error
                                 (message "[gcal-update] clock-range error: %S" clock-err)
                                 nil)))
                 (mirror-pos (+org-gcal/find-mirror source-id heading)))
            (message "[gcal-update] heading=%s source-id=%s clock-range=%s mirror-pos=%s"
                     heading source-id clock-range mirror-pos)
            (if (not mirror-pos)
                (message "[gcal-update] No mirror found for: %s" heading)
              (let ((new-ts (when clock-range
                              (let ((start (car clock-range))
                                    (end   (cdr clock-range)))
                                (format "<%s>--<%s>"
                                        (format-time-string "%Y-%m-%d %a %H:%M" start)
                                        (format-time-string "%Y-%m-%d %a %H:%M" end))))))
                (message "[gcal-update] new-ts=%s, scheduling idle timer" new-ts)
                ;; Use idle timer so other hooks complete first, but we already
                ;; have all the data we need captured in closures.
                (run-with-idle-timer
                 0.5 nil
                 (lambda ()
                   (condition-case timer-err
                       (progn
                         (message "[gcal-update] idle timer fired for: %s" heading)
                         (save-window-excursion
                           (with-current-buffer (find-file-noselect +org/gcal-file)
                             (goto-char mirror-pos)
                             ;; Verify we're at the right heading
                             (message "[gcal-update] at pos %d, line: %s"
                                      mirror-pos
                                      (buffer-substring (line-beginning-position) (line-end-position)))
                             ;; Backfill :source-id: if missing
                             (when (and source-id (not (org-entry-get nil "source-id")))
                               (org-entry-put nil "source-id" source-id)
                             (message "[gcal-update] backfilled source-id"))
                             ;; Ensure :calendar-id: is set
                             (unless (org-entry-get nil "calendar-id")
                               (org-entry-put nil "calendar-id" "awang@weids.dev"))
                             ;; Clear stale ETag to avoid 412 on PATCH
                             (org-entry-delete nil "ETag")
                             (when new-ts
                               (let ((bound (save-excursion (org-end-of-subtree t) (point))))
                                 (message "[gcal-update] searching for timestamp in range %d-%d" (point) bound)
                                 (if (re-search-forward
                                      "<[0-9][^>]*>\\(--<[^>]+>\\)?" bound t)
                                     (progn
                                       (message "[gcal-update] found ts: %s -> replacing with: %s"
                                                (match-string 0) new-ts)
                                       (replace-match new-ts t t))
                                   (message "[gcal-update] NO timestamp found in subtree!"))))
                             (save-buffer)
                             (goto-char mirror-pos)
                             (+org-gcal/post-at-point-no-prompts)
                             (message "[gcal-update] Updated Google Calendar with actual time: %s"
                                      heading))))
                     (error (message "[gcal-update] timer error: %S" timer-err)))))))))
      (error (message "[gcal-update] hook error: %S" err))))

  ;; Capture heading + schedule + source-id at advice time, pass to idle timer
  (advice-add 'org-schedule :after
              (lambda (&rest _)
                (when (derived-mode-p 'org-mode)
                  (let ((heading (org-get-heading t t t t))
                        (scheduled (org-entry-get nil "SCHEDULED"))
                        (source-id (org-id-get-create)))
                    (when (and heading scheduled)
                      (run-with-idle-timer
                       0.5 nil
                       (lambda ()
                         (+org-gcal/push-to-calendar heading scheduled source-id))))))))

  (add-hook 'org-after-todo-state-change-hook #'+org-gcal/update-on-done)

  ;; Auto-sync: fetch on startup, two-way sync every 15 minutes
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-idle-timer 10 nil #'org-gcal-fetch)
              (run-with-timer 900 900 #'org-gcal-sync))))

;;; init-org.el ends here
