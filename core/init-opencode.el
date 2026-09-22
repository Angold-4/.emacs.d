;;; init-opencode.el --- OpenCode sessions as native Emacs buffers -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ango Wang

;;; Commentary:
;;
;; OpenCode runs headless (`opencode serve') and sczi/opencode.el
;; (https://codeberg.org/sczi/opencode.el) drives it from ordinary Emacs
;; buffers.  We are not re-implementing an agent here: OpenCode owns the
;; agents, models and sessions; Emacs is only the buffer layer, so anything
;; OpenCode can talk to (Claude Code, Codex, ...) comes along for free.
;;
;; One dedicated prefix, `+opencode-prefix' (default "C-c m"), is bound both
;; globally and inside OpenCode buffers:
;;
;;   C-c m m   global session list (every project)  +opencode/sessions
;;   C-c m o   project session manager              +opencode/open
;;   C-c m n   new session                          +opencode/new
;;   C-c m M   select model                         +opencode/model
;;   C-c m v   select model variant                 +opencode/variant
;;   C-c m s   save this session as an org file     +opencode/save
;;   C-c m d   open the saved-sessions directory    +opencode/open-directory
;;
;; Sessions as files: `+opencode/save' writes a session to
;; `+opencode-sessions-directory' as org with metadata (id, directory,
;; branch, saved time) followed by the markdown transcript.  The file
;; persists, commits and travels to another machine; re-importing a file
;; into OpenCode is deliberately out of scope for now.
;;
;; The package and its `plz' dependencies are not on MELPA, so all four are
;; pinned here.  OpenCode itself is found via `~/.opencode/bin'.
;;
;; This replaces the old vterm-based `init-opencode.el'.

;;; Code:

(require 'project)
(require 'vtable)

;; Keep the executable reachable before anything tries to resolve it.
(add-to-list 'exec-path (expand-file-name "~/.opencode/bin/"))

;; sczi/opencode.el needs the plz stack; none of it is on MELPA, so pin the
;; recipes before the package below resolves its dependencies.
(use-package plz
  :straight (:type git :host github :repo "alphapapa/plz.el"))

(use-package plz-media-type
  :straight (:type git :host github :repo "r0man/plz-media-type"))

(use-package plz-event-source
  :straight (:type git :host github :repo "r0man/plz-event-source"))

(use-package opencode
  :straight (:type git :host codeberg :repo "sczi/opencode.el")
  :commands (opencode opencode-connect opencode-select-project
             opencode-new-session opencode-select-open-session
             opencode-open-session opencode-select-model
             opencode-select-variant opencode-kill-session
             opencode-visit-last-idle opencode-add-buffer-dwim
             opencode-add-region opencode-add-file-dwim)
  :config
  (setq opencode-command (or (executable-find "opencode") "opencode"))
  ;; Start a headless server on demand when none is running.
  (setq opencode-auto-start-server t))

;; =============================================================================
;; Prefix command map
;; =============================================================================

(defcustom +opencode-prefix "C-c m"
  "Prefix key for the OpenCode commands in `+opencode-command-map'.
Set it to something outside `C-c', for example \"s-o\", if you prefer."
  :type 'key-sequence
  :group 'tools)

(defcustom +opencode-sessions-directory
  (locate-user-emacs-file "opencode-sessions/")
  "Directory where `+opencode/save' writes session org files."
  :type 'directory
  :group 'tools)

(defun +opencode/open ()
  "Open the session manager for the current project."
  (interactive)
  (require 'opencode)
  (call-interactively #'opencode))

(defun +opencode/new ()
  "Start a new OpenCode session."
  (interactive)
  (require 'opencode)
  (call-interactively #'opencode-new-session))

(defun +opencode/model ()
  "Choose the model for the current session."
  (interactive)
  (require 'opencode)
  (call-interactively #'opencode-select-model))

(defun +opencode/variant ()
  "Choose the model variant for the current session."
  (interactive)
  (require 'opencode)
  (call-interactively #'opencode-select-variant))

(defun +opencode/open-directory ()
  "Open `+opencode-sessions-directory' in Dired."
  (interactive)
  (make-directory +opencode-sessions-directory t)
  (dired +opencode-sessions-directory))

;; =============================================================================
;; Global session list
;; =============================================================================

(defvar +opencode--sessions-buffer "*OpenCode Sessions*")

(defvar +opencode-global-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'+opencode/sessions-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for the global OpenCode session list.")

(define-derived-mode +opencode-global-mode special-mode "OpenCode"
  "Major mode for the global OpenCode session list.")

(defun +opencode/sessions ()
  "List every OpenCode session across projects."
  (interactive)
  (require 'opencode)
  (let ((buffer (get-buffer-create +opencode--sessions-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+opencode-global-mode)
        (+opencode-global-mode))
      (+opencode/sessions-refresh))
    (pop-to-buffer buffer)))

(defun +opencode/sessions-refresh ()
  "Refresh the global session list."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Loading OpenCode sessions...\n"))
  ;; The server scopes `session/list' to the `x-opencode-directory' header, so
  ;; aggregate over every project, one request per worktree.
  (opencode-api-projects projects
    (let ((pending (length projects))
          (rows nil))
      (if (zerop pending)
          (+opencode--sessions-render nil)
        (dolist (project projects)
          (let ((default-directory
                  (file-name-as-directory
                   (expand-file-name (alist-get 'worktree project)))))
            (opencode-api-sessions sessions
              (dolist (session sessions)
                (push session rows))
              (when (zerop (setq pending (1- pending)))
                ;; Worktrees share a repository, so different project queries
                ;; return the same sessions; keep one per id.
                (let ((seen (make-hash-table :test #'equal))
                      (unique nil))
                  (dolist (session rows)
                    (unless (gethash (alist-get 'id session) seen)
                      (puthash (alist-get 'id session) t seen)
                      (push session unique)))
                  (+opencode--sessions-render (nreverse unique)))))))))))

(defun +opencode--sessions-render (rows)
  "Render ROWS, a list of session alists tagged with a `project' key."
  (with-current-buffer (get-buffer-create +opencode--sessions-buffer)
    (let ((inhibit-read-only t)
          (cache (make-hash-table :test #'equal)))
      (erase-buffer)
      (if (null rows)
          (insert "No OpenCode sessions.\n")
        (make-vtable
         :columns '((:name "Project" :min-width 12)
                    (:name "Branch" :min-width 8)
                    (:name "Last Updated" :width 12
                     :formatter opencode--format-time-ago
                     :primary ascend)
                    "Title")
         :objects rows
         :actions '("o" +opencode--open-session
                    "RET" +opencode--open-session
                    "s" +opencode--save-session
                    "x" opencode-kill-session)
         :getter (lambda (object column vtable)
                   (let-alist object
                     (pcase (vtable-column vtable column)
                       ("Project"
                        (if .directory
                            (file-name-nondirectory (directory-file-name .directory))
                          "-"))
                       ("Branch"
                        (if (and .directory (file-exists-p .directory))
                            (let ((default-directory .directory))
                              (with-memoization (gethash .directory cache)
                                (magit-get-current-branch)))
                          "-"))
                       ("Last Updated" (opencode--time-ago object 'updated))
                       ("Title" (or .title "(untitled)")))))
         :separator-width 3
         :keymap +opencode-global-mode-map)))))

(defun +opencode--open-session (session)
  "Open SESSION from the global list."
  (opencode-open-session session))

(defun +opencode--save-session (session)
  "Save SESSION from the global list."
  (+opencode/save-session session))

;; =============================================================================
;; Sessions as org files
;; =============================================================================

(defun +opencode/save-session (&optional session)
  "Write SESSION (or the current session) to an org file.
The file lands in `+opencode-sessions-directory' under the project name and
holds metadata plus the full markdown transcript."
  (interactive)
  (require 'opencode)
  (let ((id (or (alist-get 'id session)
                (and (boundp 'opencode-session-id) opencode-session-id))))
    (unless id
      (user-error "Not in an OpenCode session"))
    (opencode-api-session-messages id
        messages
      (let* ((directory (file-name-as-directory
                         (expand-file-name
                          (or (alist-get 'directory session) default-directory))))
             (project (file-name-nondirectory (directory-file-name directory)))
             (branch (let ((default-directory directory))
                       (ignore-errors (magit-get-current-branch))))
             (target-dir (expand-file-name project +opencode-sessions-directory))
             (file (expand-file-name (format "%s.org" id) target-dir)))
        (make-directory target-dir t)
        (with-temp-file file
          (insert "#+title: OpenCode session " id "\n")
          (insert "#+opencode_id: " id "\n")
          (insert "#+opencode_directory: " directory "\n")
          (when branch
            (insert "#+opencode_branch: " branch "\n"))
          (insert "#+opencode_saved: " (format-time-string "%Y-%m-%d %H:%M") "\n\n")
          (insert (opencode--conversation-to-markdown messages) "\n"))
        (message "Saved OpenCode session to %s" file)))))

(defun +opencode/save ()
  "Save the current session as an org file."
  (interactive)
  (+opencode/save-session nil))

;; =============================================================================
;; Keys
;; =============================================================================

(defvar +opencode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'+opencode/sessions)
    (define-key map (kbd "o") #'+opencode/open)
    (define-key map (kbd "n") #'+opencode/new)
    (define-key map (kbd "M") #'+opencode/model)
    (define-key map (kbd "v") #'+opencode/variant)
    (define-key map (kbd "s") #'+opencode/save)
    (define-key map (kbd "d") #'+opencode/open-directory)
    map)
  "Prefix map for OpenCode commands.")

(global-set-key (kbd +opencode-prefix) +opencode-command-map)

(with-eval-after-load 'evil
  (evil-ex-define-cmd "opencode" #'+opencode/sessions)
  (evil-ex-define-cmd "oc" #'+opencode/sessions))

;; Keep the prefix consistent inside OpenCode buffers, where the package's own
;; C-c map would otherwise shadow it.
(with-eval-after-load 'opencode
  (define-key opencode-session-mode-map (kbd +opencode-prefix) +opencode-command-map)
  (define-key opencode-session-control-mode-map (kbd +opencode-prefix) +opencode-command-map))

(provide 'init-opencode)
;;; init-opencode.el ends here
