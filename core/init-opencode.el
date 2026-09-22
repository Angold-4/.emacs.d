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
;;   C-c m m   global session list (every project)   +opencode/sessions
;;   C-c m o   project session manager               +opencode/open
;;   C-c m i   focus the input buffer                +opencode/input
;;   C-c m c   new session in this workspace         +opencode/new
;;   C-c m M   select model                          +opencode/model
;;   C-c m a   provider, then model                  +opencode/provider
;;   C-c m v   select model variant                  +opencode/variant
;;   C-c m s   save this session as an org file      +opencode/save
;;   C-c m d   open the saved-sessions directory     +opencode/open-directory
;;
;; The model's reasoning/thinking blocks are hidden (`+opencode-show-reasoning'
;; nil); the package has no switch for this, so reasoning is dropped at its one
;; insertion point and its re-render is skipped.
;;
;; A session opens in Evil insert state; `RET` there is an ordinary newline and
;; normal-state `RET` sends.  A plain input buffer is shown under each session
;; so a prompt can be written while the agent is still working.
;;
;; Sessions as files: `+opencode/save' writes a session to
;; `+opencode-sessions-directory' as org with metadata (id, directory, branch,
;; saved time) followed by the markdown transcript.  The file persists, commits
;; and travels to another machine; re-importing a file into OpenCode is
;; deliberately out of scope for now.
;;
;; The package and its `plz' dependencies are not on MELPA, so all four are
;; pinned here.  OpenCode itself is found via `~/.opencode/bin'.
;;
;; This replaces the old vterm-based `init-opencode.el'.

;;; Code:

(require 'project)
(require 'seq)
(require 'vtable)

;; Keep the executable reachable before anything tries to resolve it, and make
;; `lsof' discoverable (it lives in /usr/sbin on macOS).
(add-to-list 'exec-path (expand-file-name "~/.opencode/bin/"))
(add-to-list 'exec-path "/usr/sbin")

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
  (setq opencode-command (or (executable-find "opencode") "opencode")
        ;; Start a headless server on demand when none is running.
        opencode-auto-start-server t)
  (+opencode--resolve-credentials))

;; =============================================================================
;; Server connection
;; =============================================================================

(defcustom +opencode-server-password nil
  "Password for a password-protected OpenCode server.
When nil, the password is taken from `OPENCODE_SERVER_PASSWORD', and failing
that from the environment of the server process already listening on
`opencode-port'.  Set it here only to override both."
  :type '(choice (const :tag "Discover" nil) string)
  :group 'tools)

(defun +opencode--listening-pid ()
  "Return the PID listening on `opencode-port', or nil."
  (ignore-errors
    (let ((out (string-trim
                (shell-command-to-string
                 (format "%s -ti tcp:%d -sTCP:LISTEN 2>/dev/null"
                         (or (executable-find "lsof") "/usr/sbin/lsof")
                         opencode-port)))))
      (unless (string-empty-p out)
        (car (split-string out "\n"))))))

(defun +opencode--server-process-var (name)
  "Return NAME from the listening OpenCode server's environment, or nil.
The package only reads its credential variables when it *starts* a server,
so a server that is already running has to be asked directly.  Both `lsof'
and the capital `E' of `ps -Eww' matter: lowercase `e' does not show the
environment on macOS."
  (when-let ((pid (+opencode--listening-pid)))
    (ignore-errors
      (with-temp-buffer
        (call-process (or (executable-find "ps") "/bin/ps") nil t nil "-Eww" "-p" pid)
        (goto-char (point-min))
        (when (re-search-forward (format "%s=\\([^ ]+\\)" (regexp-quote name)) nil t)
          (match-string 1))))))

(defun +opencode--resolve-credentials (&rest _)
  "Fill the package's credential variables for a running server.
Precedence: `+opencode-server-password', the process environment, then the
listening server's environment.  Runs as :before advice on
`opencode-autoconnect', hence the ignored arguments."
  (setq opencode-server-username
        (or (getenv "OPENCODE_SERVER_USERNAME")
            (+opencode--server-process-var "OPENCODE_SERVER_USERNAME")
            opencode-server-username))
  (setq opencode-server-password
        (or +opencode-server-password
            (getenv "OPENCODE_SERVER_PASSWORD")
            (+opencode--server-process-var "OPENCODE_SERVER_PASSWORD"))))

;; =============================================================================
;; Hide the thinking trace
;; =============================================================================

(defcustom +opencode-show-reasoning nil
  "Whether to show the model's reasoning/thinking blocks.
The package has no switch for this; when nil, reasoning is dropped at the
one insertion point and its region re-render is skipped."
  :type 'boolean
  :group 'tools)

(defun +opencode--hide-reasoning-insert (orig-fn text)
  "Call ORIG-FN on TEXT only when reasoning is shown."
  (when +opencode-show-reasoning
    (funcall orig-fn text)))

(defun +opencode--hide-reasoning-region (orig-fn type start &optional end)
  "Call ORIG-FN unless TYPE is `reasoning' and reasoning is hidden."
  (when (or +opencode-show-reasoning (not (eq type 'reasoning)))
    (funcall orig-fn type start end)))

;; =============================================================================
;; Input buffer
;; =============================================================================

(defcustom +opencode-input-buffer-name "*OpenCode Input*"
  "Base name for a workspace's input buffer."
  :type 'string
  :group 'tools)

(defvar-local +opencode-input-session nil
  "Session buffer this input buffer sends to, or nil for one not yet created.")

(defvar-local +opencode-input-directory nil
  "Workspace directory a not-yet-created session belongs to.")

(defvar-local +opencode-input-title nil
  "Readable title to give the session when it is first created.")

(defvar +opencode-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'+opencode/send-input)
    (define-key map (kbd "C-c C-c") #'+opencode/send-input)
    map)
  "Keymap for the OpenCode input buffer.")

(defun +opencode--model-label (model)
  "Return a short label for MODEL, preferring its name over its ids."
  (when model
    (let* ((provider (alist-get 'providerID model))
           (id (alist-get 'modelID model))
           (variant (alist-get 'variant model))
           (catalog (and (boundp 'opencode-providers)
                         (seq-find (lambda (p) (equal (alist-get 'id p) provider))
                                   opencode-providers)))
           (name (cdr (assoc id (alist-get 'models catalog)))))
      (concat (or (alist-get 'name name) (format "%s/%s" provider id))
              (and variant (format ":%s" variant))))))

(defun +opencode--input-model ()
  "Return the model the input buffer will send with.
A live session's own model, else the model remembered for new sessions."
  (cond
   ((buffer-live-p +opencode-input-session)
    (with-current-buffer +opencode-input-session
      (and (boundp 'opencode-session-agent) opencode-session-agent
           (alist-get 'model opencode-session-agent))))
   ((boundp 'opencode-last-model) opencode-last-model)))

(defun +opencode--input-bar ()
  "Mode-line segment for the input buffer: model, session title, state.
The buffer name itself is already in the mode line."
  (let ((model (+opencode--input-model))
        (title +opencode-input-title))
    (concat (when model (format " %s" (+opencode--model-label model)))
            (when title (format " · %s" title))
            (format " · %s "
                    (if (buffer-live-p +opencode-input-session) "live" "draft")))))

(define-derived-mode +opencode-input-mode text-mode "OpenCode-Input"
  "Major mode for composing an OpenCode prompt in its own buffer."
  (setq-local mode-line-process '(:eval (+opencode--input-bar))))

(defun +opencode--session-buffer ()
  "Return a session buffer to act on: the current one, or the most recent."
  (or (and (derived-mode-p 'opencode-session-mode) (current-buffer))
      (seq-find (lambda (buffer)
                  (with-current-buffer buffer
                    (derived-mode-p 'opencode-session-mode)))
                (buffer-list))))

(defun +opencode--branch-name (directory)
  "Return DIRECTORY's checked-out branch, or nil."
  (when-let ((root (locate-dominating-file directory ".git")))
    (when-let ((context (ignore-errors (+git-store-context-for-root root))))
      (ignore-errors (+git-store-local-context-current-branch context)))))

(defun +opencode--session-title (directory)
  "Return a readable title for a new session in DIRECTORY.
The creation date and the first ten characters of the branch."
  (let ((branch (or (+opencode--branch-name directory) "no-branch")))
    (format "%s %s"
            (format-time-string "%Y-%m-%d")
            (substring branch 0 (min 10 (length branch))))))

(defun +opencode--input-buffer (directory)
  "Return DIRECTORY's input buffer, creating it if needed."
  (let ((name (file-name-nondirectory
               (directory-file-name
                (file-name-as-directory (expand-file-name directory))))))
    (get-buffer-create (format "%s <%s>" +opencode-input-buffer-name name))))

(defun +opencode/send-input ()
  "Send the input buffer's text, creating the session on the first send.
Sending reuses the package's own `opencode-session--send-synthetic-input',
so the session's model, agent and context are untouched and a send works
while the agent is still working."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    (cond
     ((buffer-live-p +opencode-input-session)
      (with-current-buffer +opencode-input-session
        (opencode-session--send-synthetic-input text))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (message "Sent to %s" +opencode-input-session))
     ((and +opencode-input-directory +opencode-input-title)
      (+opencode--create-session-and-send +opencode-input-directory
                                          +opencode-input-title
                                          text))
     (t
      (user-error "This input buffer is not attached to a session")))))

(defun +opencode--create-session-and-send (directory title text)
  "Create a session in DIRECTORY titled TITLE, send TEXT, and save it.
An empty session is never created: this only runs on the first send."
  (let ((default-directory (file-name-as-directory (expand-file-name directory)))
        (input (current-buffer)))
    (opencode-autoconnect
     (lambda ()
       (opencode--download-slash-commands default-directory)
       (opencode-api-create-session (list (cons 'title title))
           session
         (opencode-open-session
          session
          :callback
          (lambda (opened)
            (let ((session-buffer (current-buffer)))
              (opencode-session--send-synthetic-input text)
              (when (buffer-live-p input)
                (with-current-buffer input
                  (setq +opencode-input-session session-buffer
                        +opencode-input-directory nil
                        +opencode-input-title nil)
                  (let ((inhibit-read-only t))
                    (erase-buffer)))
                (+opencode--arrange input session-buffer (alist-get 'title opened)))
              (+opencode/save-session opened)))))))))

(defun +opencode--prepare-input (directory)
  "Return DIRECTORY's input buffer, ready to use."
  (let ((buffer (+opencode--input-buffer directory)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+opencode-input-mode)
        (+opencode-input-mode)))
    buffer))

(defun +opencode/input ()
  "Focus the input buffer for the current (or most recent) session."
  (interactive)
  (require 'opencode)
  (let ((session (+opencode--session-buffer)))
    (unless (buffer-live-p session)
      (user-error "No OpenCode session open yet"))
    (pop-to-buffer (+opencode--prepare-input
                    (with-current-buffer session default-directory)))))

(defun +opencode--compact-number (n)
  "Format N compactly, e.g. 171000 as \"171k\"."
  (if (and (numberp n) (>= n 1000))
      (format "%.0fk" (/ n 1000.0))
    (number-to-string (or n 0))))

(defun +opencode--output-bar ()
  "Mode-line segment for a session buffer: model, context used, status."
  (let* ((agent opencode-session-agent)
         (model (ignore-errors (opencode--current-model)))
         (name (or (alist-get 'name model) ""))
         (variant (alist-get 'variant agent))
         (limit (map-nested-elt model '(limit context)))
         (used opencode-session-tokens)
         (known (and (numberp limit) (numberp used) (> limit 0)))
         (pct (and known (* 100.0 (/ (float used) limit)))))
    (concat " " name
            (when variant (format " %s" variant))
            (when known
              (format " · ctx %s/%s (%.0f%%)"
                      (+opencode--compact-number used)
                      (+opencode--compact-number limit)
                      pct))
            (format " · %s " (or opencode-session-status "idle")))))

(defun +opencode--show-input (buffer)
  "Give a session just opened in BUFFER its mode-line status bar."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'opencode-session-mode)
        (setq-local mode-line-process '(:eval (+opencode--output-bar))))))
  buffer)

(defun +opencode--arrange (input session &optional title)
  "Show INPUT on top and SESSION below, by splitting INPUT's window.
Other windows are left alone; the input buffer is full until a session
exists, and the transcript only appears once there is one.  TITLE, when
given, is the OpenCode session title shown in the input's mode line."
  (with-current-buffer input
    (setq +opencode-input-session session)
    (when title
      (setq +opencode-input-title title)))
  (let ((window (or (get-buffer-window input (selected-frame))
                    (selected-window))))
    (select-window window)
    (set-window-buffer window input)
    (let ((below (split-window-below)))
      (set-window-buffer below session)
      (set-window-start below (point-min)))
    (select-window window)))

;; =============================================================================
;; Session commands
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

(defun +opencode/workspace-directory ()
  "Return the directory a new session should belong to.
The current session's directory, else the current project root, else
`default-directory'."
  (cond
   ((derived-mode-p 'opencode-session-mode) default-directory)
   ((project-current) (project-root (project-current)))
   (t default-directory)))

(defun +opencode/new ()
  "Compose a new session in the current workspace.
The OpenCode session itself is created on the first send, so an empty one
is never recorded."
  (interactive)
  (require 'opencode)
  (let* ((directory (+opencode/workspace-directory))
         (title (+opencode--session-title directory))
         (buffer (+opencode--input-buffer directory)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+opencode-input-mode)
        (+opencode-input-mode))
      (setq +opencode-input-session nil
            +opencode-input-directory directory
            +opencode-input-title title))
    (pop-to-buffer buffer)
    (message "New %s (created on first send)" title)))

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

(defun +opencode--provider-candidates ()
  "Completion candidates for `opencode-providers', provider-first."
  (cl-loop for provider in opencode-providers
           collect (list (or (alist-get 'name provider)
                             (alist-get 'id provider))
                         provider
                         (alist-get 'id provider))))

(defun +opencode--model-candidates (provider)
  "Completion candidates for PROVIDER's models."
  (let (candidates)
    (dolist (entry (alist-get 'models provider))
      (let ((model (cdr entry)))
        (push (list (or (alist-get 'name model)
                        (alist-get 'id model))
                    model
                    (alist-get 'id model))
              candidates)))
    (nreverse candidates)))

(defun +opencode/provider ()
  "Choose the model provider, then a model from it.
Unlike `opencode-select-model', which flattens every provider into one
list, this asks for the provider first."
  (interactive)
  (require 'opencode)
  (unless (derived-mode-p 'opencode-session-mode)
    (user-error "Not in an OpenCode session"))
  (unless opencode-session-agent
    (user-error "No agent in this session"))
  (unless opencode-providers
    (user-error "No providers reported by the server"))
  (when-let ((provider (opencode--annotated-completion
                        "Provider: " (+opencode--provider-candidates))))
    (when-let ((model (opencode--annotated-completion
                       "Model: " (+opencode--model-candidates provider))))
      (let ((entry `((providerID . ,(alist-get 'id provider))
                     (modelID . ,(alist-get 'id model)))))
        (setf (alist-get 'model opencode-session-agent) entry
              opencode-last-model entry)
        ;; Drop a variant the new model does not offer.
        (when-let ((variant (alist-get 'variant opencode-session-agent)))
          (unless (alist-get variant (alist-get 'variants model))
            (setq opencode-session-agent
                  (assq-delete-all 'variant opencode-session-agent))))
        (message "Model: %s/%s"
                 (alist-get 'id provider) (alist-get 'id model))))))

(defun +opencode/send ()
  "Send the session's current input to the agent.
Bound to RET in normal state so editing stays in insert state."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (user-error "Not in an OpenCode session"))
  (comint-send-input))

(defun +opencode/open-directory ()
  "Open `+opencode-sessions-directory' in Dired."
  (interactive)
  (make-directory +opencode-sessions-directory t)
  (dired +opencode-sessions-directory))

;; =============================================================================
;; Sessions as org files
;; =============================================================================

(defvar +opencode--save-timer nil
  "Pending debounce timer for an automatic session save.")

(defun +opencode--org-time (milliseconds)
  "Format MILLISECONDS since the epoch, or nil."
  (when (numberp milliseconds)
    (format-time-string "%Y-%m-%d %H:%M" (/ milliseconds 1000))))

(defun +opencode--messages-to-org (messages)
  "Render MESSAGES as an org transcript of `Prompt' and `Response' sections.
Reasoning is omitted (see `+opencode-show-reasoning'); tool calls become a
single line so the file stays readable."
  (let (lines)
    (dolist (message messages)
      (let* ((info (alist-get 'info message))
             (role (alist-get 'role info))
             (time (alist-get 'created (alist-get 'time info)))
             (model (alist-get 'model info)))
        (push (format "* %s%s%s"
                      (if (equal role "user") "Prompt" "Response")
                      (if time (concat " " (+opencode--org-time time)) "")
                      (if-let ((id (alist-get 'modelID model)))
                          (format "  [%s/%s%s]"
                                  (alist-get 'providerID model) id
                                  (if-let ((v (alist-get 'variant model)))
                                      (format ":%s" v) ""))
                        ""))
              lines)
        (dolist (part (alist-get 'parts message))
          (pcase (alist-get 'type part)
            ("text"
             (let ((text (string-trim (or (alist-get 'text part) ""))))
               (unless (string-empty-p text)
                 (push text lines))))
            ("tool"
             (push (format "- tool: %s %s"
                           (or (alist-get 'tool part) "?")
                           (or (alist-get 'status (alist-get 'state part)) ""))
                   lines))
            (_ nil)))
        (push "" lines)))
    (string-join (nreverse lines) "\n")))

(defun +opencode--session-file (session id)
  "Return the org file path for SESSION with ID."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or (alist-get 'directory session)
                                           default-directory))))
         (project (file-name-nondirectory (directory-file-name directory)))
         (title (or (alist-get 'title session) "session"))
         (slug (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" title))
         (short (substring id (max 0 (- (length id) 6)))))
    (expand-file-name (format "%s-%s.org" slug short)
                      (expand-file-name project +opencode-sessions-directory))))

(defun +opencode/save-session (&optional session)
  "Write SESSION (or the current session) to its merged org file.
The file holds metadata plus the whole conversation, so one file per
session is self-contained and portable."
  (interactive)
  (require 'opencode)
  (let ((id (or (alist-get 'id session)
                (and (boundp 'opencode-session-id) opencode-session-id))))
    (unless id
      (user-error "Not in an OpenCode session"))
    (opencode-api-session-messages id
        messages
      (let* ((directory (file-name-as-directory
                         (expand-file-name (or (alist-get 'directory session)
                                               default-directory))))
             (title (or (alist-get 'title session) "session"))
             (branch (or (alist-get 'branch session)
                         (ignore-errors (+opencode--branch-name directory))))
             (file (+opencode--session-file session id)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (insert "#+title: " title "\n")
          (insert "#+opencode_id: " id "\n")
          (insert "#+opencode_title: " title "\n")
          (insert "#+opencode_directory: " directory "\n")
          (when branch
            (insert "#+opencode_branch: " branch "\n"))
          (insert "#+opencode_updated: " (format-time-string "%Y-%m-%d %H:%M") "\n\n")
          (insert (+opencode--messages-to-org messages) "\n"))
        (message "Saved session to %s" file)))))

(defun +opencode/save ()
  "Save the current session as an org file."
  (interactive)
  (+opencode/save-session nil))

(defun +opencode--save-by-id (session-id)
  "Save SESSION-ID without requiring its buffer to be current."
  (ignore-errors
    (opencode--with-session-buffer session-id
      (+opencode/save-session nil))))

(defun +opencode--schedule-save (session-id)
  "Coalesce automatic saves for SESSION-ID."
  (when +opencode--save-timer
    (cancel-timer +opencode--save-timer))
  (setq +opencode--save-timer
        (run-at-time 1 nil
                     (lambda ()
                       (setq +opencode--save-timer nil)
                       (+opencode--save-by-id session-id)))))

(defun +opencode--auto-save (orig-fn info)
  "Run ORIG-FN, then save the session when a turn completes."
  (funcall orig-fn info)
  (when (and (equal (map-nested-elt info '(role)) "assistant")
             (map-nested-elt info '(time completed)))
    (when-let ((id (map-nested-elt info '(sessionID))))
      (+opencode--schedule-save id))))

;; =============================================================================
;; Global session list
;; =============================================================================

(defvar +opencode--sessions-buffer "*OpenCode Sessions*")

(defvar +opencode-global-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'+opencode/sessions-refresh)
    (define-key map (kbd "c") #'+opencode/new)
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
        (+opencode-global-mode)))
    (pop-to-buffer buffer)
    (+opencode/sessions-refresh)))

(defun +opencode/sessions-refresh ()
  "Refresh the global session list, connecting to a server first."
  (interactive)
  (require 'opencode)
  (let ((buffer (get-buffer-create +opencode--sessions-buffer)))
    ;; `opencode-api-url' is only set by the connect flow, and the request
    ;; callbacks error without it, so reuse or start a server before asking.
    (opencode-autoconnect
     (lambda ()
       (with-current-buffer buffer
         (+opencode--sessions-fetch))))))

(defun +opencode--sessions-fetch ()
  "Fill the global session list.  Assumes a live server connection."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Loading OpenCode sessions...\n"))
  ;; The server scopes `session/list' to the `x-opencode-directory' header, so
  ;; aggregate over every project, one request per worktree.
  (opencode-api-projects projects
    (let ((pending (length projects))
          (rows nil))
      (if (zerop pending)
          (+opencode--sessions-render (+opencode--archived-sessions))
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
                  (+opencode--sessions-render
                   (append (nreverse unique) (+opencode--archived-sessions))))))))))))

(defun +opencode--file-keyword (file key)
  "Return FILE's `#+KEY' value, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward
           (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote key)) nil t)
      (string-trim (match-string-no-properties 1)))))

(defun +opencode--file-body (file)
  "Return FILE's transcript, from its first `* ' heading."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (buffer-substring-no-properties
     (if (re-search-forward "^\\* " nil t) (match-beginning 0) (point-min))
     (point-max))))

(defun +opencode--archived-sessions ()
  "Return rows read from the session org files in the sessions directory."
  (let (rows)
    (when (file-directory-p +opencode-sessions-directory)
      (dolist (file (directory-files-recursively +opencode-sessions-directory
                                                 "\\.org\\'"))
        (push (list (cons 'kind 'archived)
                    (cons 'file file)
                    (cons 'id (+opencode--file-keyword file "OPENCODE_ID"))
                    (cons 'title (or (+opencode--file-keyword file "OPENCODE_TITLE")
                                     (file-name-base file)))
                    (cons 'directory (+opencode--file-keyword file "OPENCODE_DIRECTORY"))
                    (cons 'branch (+opencode--file-keyword file "OPENCODE_BRANCH"))
                    (cons 'updated (+opencode--file-keyword file "OPENCODE_UPDATED")))
              rows)))
    (nreverse rows)))

(defun +opencode--sessions-render (rows)
  "Render ROWS, live sessions and archived files, in the global list."
  (with-current-buffer (get-buffer-create +opencode--sessions-buffer)
    (let ((inhibit-read-only t)
          (cache (make-hash-table :test #'equal)))
      (erase-buffer)
      (if (null rows)
          (insert "No OpenCode sessions.\n")
        (make-vtable
         :columns '((:name "Source" :width 6)
                    (:name "Project" :min-width 12)
                    (:name "Branch" :min-width 8)
                    (:name "Updated" :width 17 :primary ascend)
                    "Title")
         :objects rows
         :actions '("o" +opencode--open-row
                    "RET" +opencode--open-row
                    "C" +opencode--continue-row
                    "s" +opencode--save-session
                    "x" +opencode--kill-row)
         :getter (lambda (object column vtable)
                   (let ((col (vtable-column vtable column)))
                     (if (eq (alist-get 'kind object) 'archived)
                         (pcase col
                           ("Source" "file")
                           ("Project"
                            (let ((dir (alist-get 'directory object)))
                              (if dir
                                  (file-name-nondirectory (directory-file-name dir))
                                "-")))
                           ("Branch" (or (alist-get 'branch object) "-"))
                           ("Updated" (or (alist-get 'updated object) "-"))
                           ("Title" (or (alist-get 'title object) "(untitled)")))
                       (let-alist object
                         (pcase col
                           ("Source" "live")
                           ("Project"
                            (if .directory
                                (file-name-nondirectory (directory-file-name .directory))
                              "-"))
                           ("Branch"
                            (if (and .directory (file-exists-p .directory))
                                (with-memoization (gethash .directory cache)
                                  (or (+opencode--branch-name .directory) "-"))
                              "-"))
                           ("Updated" (opencode--format-time-ago
                                       (opencode--time-ago object 'updated)))
                           ("Title" (or .title "(untitled)")))))))
         :separator-width 3
         :keymap +opencode-global-mode-map)))))

(defun +opencode--open-row (row)
  "Open ROW: resume a live session as input over transcript, or visit a file."
  (if (eq (alist-get 'kind row) 'archived)
      (find-file (alist-get 'file row))
    (let ((session (opencode-open-session row)))
      (when (buffer-live-p session)
        (let ((input (+opencode--prepare-input
                      (with-current-buffer session default-directory))))
          (+opencode--arrange input session (alist-get 'title row))))
      session)))

(defun +opencode--continue-row (row)
  "Seed a new session from an archived ROW."
  (unless (eq (alist-get 'kind row) 'archived)
    (user-error "Only archived sessions can be continued"))
  (+opencode/continue-from-file (alist-get 'file row)))

(defun +opencode--kill-row (row)
  "Delete ROW's server session; archived files are refused."
  (if (eq (alist-get 'kind row) 'archived)
      (user-error "Delete the file to remove an archived session")
    (opencode-kill-session row)))

(defun +opencode--save-session (row)
  "Save live ROW; archived files are already on disk."
  (if (eq (alist-get 'kind row) 'archived)
      (message "Already on disk: %s" (alist-get 'file row))
    (+opencode/save-session row)))

(defun +opencode/continue-from-file (file)
  "Seed a new session from the archived session FILE."
  (interactive "fSession file: ")
  (require 'opencode)
  (let* ((directory (or (+opencode--file-keyword file "OPENCODE_DIRECTORY")
                        default-directory))
         (title (or (+opencode--file-keyword file "OPENCODE_TITLE")
                    (file-name-base file)))
         (buffer (+opencode--input-buffer directory)))
    (with-current-buffer buffer
      (unless (derived-mode-p '+opencode-input-mode)
        (+opencode-input-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Continue the session archived at %s.\n\n%s"
                        file (+opencode--file-body file))))
      (setq +opencode-input-session nil
            +opencode-input-directory directory
            +opencode-input-title (format "%s (cont.)" title)))
    (pop-to-buffer buffer)
    (message "Review, then RET to create the continued session")))

;; =============================================================================
;; Prefix map and installation
;; =============================================================================

(defvar +opencode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'+opencode/sessions)
    (define-key map (kbd "o") #'+opencode/open)
    (define-key map (kbd "i") #'+opencode/input)
    (define-key map (kbd "c") #'+opencode/new)
    (define-key map (kbd "M") #'+opencode/model)
    (define-key map (kbd "a") #'+opencode/provider)
    (define-key map (kbd "v") #'+opencode/variant)
    (define-key map (kbd "s") #'+opencode/save)
    (define-key map (kbd "d") #'+opencode/open-directory)
    map)
  "Prefix map for OpenCode commands.")

(global-set-key (kbd +opencode-prefix) +opencode-command-map)

(with-eval-after-load 'evil
  (evil-ex-define-cmd "opencode" #'+opencode/sessions)
  (evil-ex-define-cmd "oc" #'+opencode/sessions)
  ;; The input buffer is a normal buffer: insert state to write, normal RET to
  ;; send.  No comint, so typing while the agent works costs nothing.
  (evil-set-initial-state '+opencode-input-mode 'insert)
  (evil-define-key 'normal +opencode-input-mode-map (kbd "RET") #'+opencode/send-input))

;; Keep the prefix consistent inside OpenCode buffers, where the package's own
;; C-c map would otherwise shadow it.
(with-eval-after-load 'opencode
  (define-key opencode-session-mode-map (kbd +opencode-prefix) +opencode-command-map)
  (define-key opencode-session-control-mode-map (kbd +opencode-prefix) +opencode-command-map)

  ;; The package leaves the session buffer in Emacs state, so every key reaches
  ;; comint and RET sends the moment it is typed.  Run it as an ordinary Evil
  ;; buffer instead: edit in insert state, and send deliberately from normal
  ;; state.
  (evil-set-initial-state 'opencode-session-mode 'insert)
  (evil-define-key 'insert opencode-session-mode-map (kbd "RET") #'newline)
  (evil-define-key 'insert opencode-session-mode-map (kbd "C-<return>") #'+opencode/send)
  (evil-define-key 'normal opencode-session-mode-map (kbd "RET") #'+opencode/send)
  (evil-define-key 'normal opencode-session-mode-map (kbd "C-<return>") #'+opencode/send)

  ;; The session manager is a vtable; normal state lets j/k and the package's
  ;; own evil bindings (r/n/gv) work.
  (evil-set-initial-state 'opencode-session-control-mode 'normal)

  ;; Re-read the server's credentials before every connect attempt: the server
  ;; may have been started (or restarted) after this module loaded.
  (advice-add 'opencode-autoconnect :before #'+opencode--resolve-credentials)

  ;; Show the input buffer under every session that opens, so it is visible
  ;; without having to remember a key.
  (advice-add 'opencode-open-session :filter-return #'+opencode--show-input)

  ;; Auto-store: write the session's org file when a turn completes.
  (advice-add 'opencode-session--message-updated
              :around #'+opencode--auto-save)

  ;; Hide the model's thinking trace.
  (advice-add 'opencode--insert-reasoning-block
              :around #'+opencode--hide-reasoning-insert)
  (advice-add 'opencode--render-region
              :around #'+opencode--hide-reasoning-region))

(provide 'init-opencode)
;;; init-opencode.el ends here
