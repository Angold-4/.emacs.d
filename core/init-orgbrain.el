;;; init-orgbrain.el --- OrgBrain client workspace -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Two-buffer OrgBrain client driving the Vienna daemon over SSH

;;; Commentary:
;; `:orgbrain' opens a two-buffer workspace that talks to the OrgBrain daemon
;; on another host.  The point is bandwidth: a buffer takes a long, messy
;; brief, lets it be edited before sending, and keeps every answer as ordinary
;; buffer text that `hjkl', search, and yank already understand.
;;
;; - `*orgbrain*' on top is read-only output in `+orgbrain-mode', Evil normal.
;; - `*orgbrain-input*' below is an ordinary editable buffer; point starts here.
;; - The header line carries `project', `mode', and the host state.  This
;;   config sets a per-buffer `mode-line-format' nowhere, so status lives in a
;;   header line like the Git review buffers do.
;;
;; Request modes are data, not control flow: `+orgbrain-modes' maps a mode
;; symbol to a builder that returns the CLI argument list and the stdin text.
;; Adding the coming `prompt' and `digest' modes is one entry each.
;;
;; - ask       `orgbrain ask - --json --entity projects/<slug>', text on stdin.
;; - remember  the same `ask' call with `remember that ' prefixed.  The CLI
;;             `remember' verb is deliberately NOT used: it fails
;;             `remember_not_admitted' when the text does not route to a
;;             write, and the conversational path through `ask' is the one that
;;             scopes correctly.
;; - recall    `orgbrain recall <text> --json'.  `recall' takes the query as a
;;             positional argument, so this mode sends no stdin.  Recall stays
;;             unscoped (raw retrieval across the brain), matching the v0.1.0
;;             contract.
;;
;; Keys (Evil normal state, inside the workspace buffers):
;;
;; - TAB           cycle the request mode (input buffer)
;; - RET, C-c C-c  send
;; - <up>/<down>   walk the current project's dialogue and replay an exchange
;; - gp, C-c C-p   switch project (`completing-read')
;; - q             bury the workspace
;;
;; The project switcher is NOT on `C-x o p' as issue #6 sketched: `C-x o' is
;; `other-window' territory and this config navigates windows with
;; `C-h/C-j/C-k/C-l', so a global `C-x o' prefix would be off-idiom and would
;; shadow a standard binding.  It lives in the module's own maps instead, plus
;; the `:orgbrain-project' ex-command.
;;
;; Two server-side gaps are handled by degrading, never by failing:
;;
;; - `orgbrain project list --json' does not exist yet.  It is called first
;;   anyway; when it fails the project set is derived from `request.entity'
;;   values in `orgbrain history --json' plus `+orgbrain-default-projects',
;;   and the header line says `projects: history' so the fallback is visible.
;;   The client lights up automatically when the verb lands.
;; - `orgbrain history --json' has no project filter, so `+orgbrain-exchanges'
;;   filters client-side.  That single function is the swap point for the
;;   per-project dialogue store.
;;
;; Requests are serialised: `assert_service_idle' refuses a GBrain write while
;; any job runs and an ask takes 19-25 s, so exactly one request is in flight
;; at a time and send refuses (in the echo area) while one is outstanding.
;; Sends are asynchronous — a 25 second `call-process-region' would freeze
;; Emacs outright — while the sub-second read-only calls (history, project
;; list) run synchronously.  Both go through one swappable transport.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'seq)
(require 'windmove)

;; Optional, and only ever touched behind `fboundp' guards.
(defvar persp-switch-to-added-buffer)

;; =============================================================================
;; Customization
;; =============================================================================

(defgroup orgbrain nil
  "Client for the OrgBrain daemon."
  :group 'tools
  :prefix "+orgbrain-")

(defcustom +orgbrain-ssh-host "vienna"
  "SSH host running the OrgBrain daemon.
This is the `Host' entry from `~/.ssh/config', not a bare address, so the
tunnel, port, and key configuration stay in SSH's hands."
  :type 'string
  :group 'orgbrain)

(defcustom +orgbrain-command "orgbrain"
  "Name of the OrgBrain executable on the daemon host."
  :type 'string
  :group 'orgbrain)

(defcustom +orgbrain-default-projects '("orgbrain")
  "Project slugs always offered by the project switcher.
Used together with whatever the server or the job history reports, so the
workspace has a usable scope on a brain with no history yet."
  :type '(repeat string)
  :group 'orgbrain)

(defcustom +orgbrain-history-limit 50
  "Number of recent jobs to request from `orgbrain history'.
Dialogue navigation filters these client-side by project, so this bounds
how far back `<up>' can walk.  Tests should bind this to a small value."
  :type 'integer
  :group 'orgbrain)

(defcustom +orgbrain-connect-timeout 8
  "Seconds SSH may spend establishing the connection before giving up.
`BatchMode' only suppresses prompts; without this a down tunnel blocks in
the TCP connect for the system default, and the first `+orgbrain/open'
makes two synchronous calls.  Tests should bind this to a small value."
  :type 'integer
  :group 'orgbrain)

(defcustom +orgbrain-timeout 120
  "Seconds to wait for a synchronous OrgBrain call before giving up."
  :type 'integer
  :group 'orgbrain)

;; =============================================================================
;; State
;; =============================================================================

(defvar +orgbrain--project nil
  "Project slug the workspace is currently scoped to, or nil for unscoped.")

(defvar +orgbrain--mode 'ask
  "Current request mode; a key of `+orgbrain-modes'.")

(defvar +orgbrain--status 'idle
  "Workspace transport state: `idle', `working', or `error'.")

(defvar +orgbrain--pending nil
  "Description of the in-flight request, or nil when none.
A plist with `:mode' and `:started'.  Non-nil blocks a second request:
the daemon refuses a write while any job is running.")

(defvar +orgbrain--tick-timer nil
  "Timer refreshing the header line while a request is in flight.
A consulted ask runs for about five minutes, and a header that says
`working\=' for five minutes is indistinguishable from one that is stuck.")

(defvar +orgbrain--projects-source nil
  "Where the last project list came from: `server', `history', or `default'.")

(defvar +orgbrain--exchanges nil
  "Cached dialogue exchanges for `+orgbrain--exchanges-project', oldest first.")

(defvar +orgbrain--exchanges-project nil
  "Project slug `+orgbrain--exchanges' was collected for.")

(defvar +orgbrain--exchange-index nil
  "Index into `+orgbrain--exchanges' of the replayed exchange, or nil.")

;; =============================================================================
;; Transport
;; =============================================================================

(defun +orgbrain--run (command stdin callback)
  "Run COMMAND, a program and argument list, feeding STDIN to it.
Never signal: a program that cannot be started, or that closes its input
before the whole of STDIN is written, is reported as an ordinary failure
result with `:exit' `failed'.  Signalling here would leave
`+orgbrain--pending' set and wedge the client until Emacs restarted.
The result is a plist with `:exit', `:stdout', `:stderr', keeping SSH
diagnostics away from the output pane.  With CALLBACK nil this blocks and
returns the result; otherwise it returns the process, or nil when the
process never started, and calls CALLBACK with the result."
  (let* ((out (generate-new-buffer " *orgbrain-stdout*"))
         (err (generate-new-buffer " *orgbrain-stderr*"))
         (result nil)
         (done nil)
         (cleanup
          (lambda ()
            (when (buffer-live-p out) (kill-buffer out))
            (when (buffer-live-p err) (kill-buffer err))))
         (deliver
          ;; Exactly once, however many times a sentinel fires.
          (lambda (value)
            (unless done
              (setq done t result value)
              (funcall cleanup)
              (when callback (funcall callback result)))))
         (collect
          (lambda (proc)
            (funcall deliver
                     (list :exit (process-exit-status proc)
                           :stdout (with-current-buffer out (buffer-string))
                           :stderr (with-current-buffer err (buffer-string))))))
         (proc nil)
         (failure nil))
    (condition-case signalled
        (progn
          (setq proc
                (make-process
                 :name "orgbrain"
                 :buffer out
                 :stderr err
                 :command command
                 :connection-type 'pipe
                 :noquery t
                 :sentinel
                 (lambda (proc _msg)
                   (when (memq (process-status proc) '(exit signal))
                     ;; Collect on a zero timer so output still queued on the
                     ;; separate stderr pipe has been delivered first.
                     (run-at-time 0 nil collect proc)))))
          (set-process-query-on-exit-flag proc nil)
          ;; The stderr pipe has its own process, whose default sentinel would
          ;; write "Process orgbrain stderr finished" into the diagnostics we
          ;; report.
          (let ((stderr-proc (get-buffer-process err)))
            (when stderr-proc (set-process-sentinel stderr-proc #'ignore)))
          (when stdin
            (process-send-string proc stdin))
          (process-send-eof proc))
      (error
       (setq failure (error-message-string signalled))))
    (cond
     (failure
      ;; The sentinel may already be queued; `deliver' keeps that harmless.
      (when proc (ignore-errors (delete-process proc)))
      (let ((stderr-proc (get-buffer-process err)))
        (when stderr-proc (ignore-errors (delete-process stderr-proc))))
      (funcall deliver (list :exit 'failed :stdout "" :stderr failure))
      (if callback nil result))
     (callback proc)
     (t
      (let ((deadline (+ (float-time) +orgbrain-timeout)))
        (while (and (null result) (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (or result
            (progn
              (ignore-errors (kill-process proc))
              ;; Through `deliver' so a sentinel arriving after the deadline
              ;; cannot read the buffers this tears down.
              (funcall deliver
                       (list :exit 'timeout :stdout ""
                             :stderr (format "timed out after %ss"
                                             +orgbrain-timeout)))
              result)))))))

(defun +orgbrain--transport-ssh (args stdin callback)
  "Run the OrgBrain CLI with ARGS on `+orgbrain-ssh-host' over SSH.
STDIN and CALLBACK are as in `+orgbrain--run'.  BatchMode keeps a locked
key from stopping on a password prompt, and `ConnectTimeout' bounds the
other way a down tunnel freezes Emacs: the synchronous project-list and
history calls block in the TCP connect otherwise."
  (+orgbrain--run (list "ssh" "-o" "BatchMode=yes"
                        "-o" (format "ConnectTimeout=%d"
                                     +orgbrain-connect-timeout)
                        +orgbrain-ssh-host
                        (mapconcat #'shell-quote-argument
                                   (cons +orgbrain-command args) " "))
                  stdin callback))

(defun +orgbrain--transport-local (args stdin callback)
  "Run the OrgBrain CLI with ARGS on this machine.
For running the workspace on the daemon host itself.  STDIN and CALLBACK
are as in `+orgbrain--run'."
  (+orgbrain--run (cons +orgbrain-command args) stdin callback))

(defvar +orgbrain-transport #'+orgbrain--transport-ssh
  "Function performing an OrgBrain CLI call.
Called with the CLI argument list, a stdin string or nil, and a callback
or nil, and behaves like `+orgbrain--run'.  The buffer UX never depends on
the transport: set this to `+orgbrain--transport-local' on the daemon host.")

(defun +orgbrain--result-error (result)
  "Return a human-readable failure string for RESULT, or nil when it succeeded."
  (let ((exit (plist-get result :exit))
        (detail (string-trim (or (plist-get result :stderr) ""))))
    (cond
     ((eq exit 'timeout) (format "OrgBrain call %s" detail))
     ((eq exit 'failed)
      (format "OrgBrain call could not run: %s"
              (if (string-empty-p detail) "the process did not start" detail)))
     ((not (eq exit 0))
      (format "OrgBrain call failed (exit %s)%s"
              exit (if (string-empty-p detail) "" (format ": %s" detail))))
     (t nil))))

(defun +orgbrain--cli (args &optional stdin callback)
  "Call the OrgBrain CLI with ARGS and STDIN through `+orgbrain-transport'.
Synchronously with CALLBACK nil: return stdout, signalling `user-error' on
failure.  Otherwise call CALLBACK with (STDOUT . ERROR), exactly one of
which is non-nil."
  (if callback
      (funcall +orgbrain-transport args stdin
               (lambda (result)
                 (let ((problem (+orgbrain--result-error result)))
                   (funcall callback
                            (if problem nil (plist-get result :stdout))
                            problem))))
    (let* ((result (funcall +orgbrain-transport args stdin nil))
           (problem (+orgbrain--result-error result)))
      (when problem (user-error "%s" problem))
      (plist-get result :stdout))))

;; =============================================================================
;; JSON
;; =============================================================================

(defun +orgbrain--read-json (string)
  "Parse STRING as JSON, signalling when it is malformed.
Objects become alists with string keys and arrays become lists."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'string)
          (json-false :false))
      (json-read))))

(defun +orgbrain--parse-json (string)
  "Parse STRING as JSON, returning nil when it is not JSON."
  (and (stringp string)
       (not (string-empty-p (string-trim string)))
       (condition-case nil (+orgbrain--read-json string) (error nil))))

(defun +orgbrain--parse-json-strict (string what)
  "Parse STRING as JSON, signalling a `user-error' naming WHAT on failure.
`+orgbrain--parse-json' cannot tell an empty array from ssh noise, since
both come back nil; this can."
  (condition-case nil
      (+orgbrain--read-json (or string ""))
    (error
     (user-error "Could not read %s: %s" what
                 (truncate-string-to-width
                  (string-trim (or string "no output")) 120 nil nil t)))))

(defun +orgbrain--dig (object &rest keys)
  "Return the value at the KEYS path in parsed JSON OBJECT, or nil."
  (let ((node object))
    (dolist (key keys)
      (setq node (and (consp node) (cdr (assoc key node)))))
    node))

(defun +orgbrain--truthy (value)
  "Return VALUE unless it is JSON null or false."
  (unless (memq value '(:false :null)) value))

;; =============================================================================
;; Request modes
;; =============================================================================

(defun +orgbrain--entity-args (project)
  "Return the `--entity' arguments scoping a call to PROJECT, or nil."
  (when (and project (not (string-empty-p project)))
    (list "--entity" (concat "projects/" project))))

(defun +orgbrain--build-ask (text project)
  "Build an `ask' call sending TEXT on stdin, scoped to PROJECT."
  (list :args (append (list "ask" "-" "--json") (+orgbrain--entity-args project))
        :stdin text))

(defun +orgbrain--build-remember (text project)
  "Build the conversational remember call for TEXT, scoped to PROJECT.
This is an `ask' with a `remember that ' prefix.  The CLI `remember' verb
refuses text that does not route to a write (`remember_not_admitted'), so
it is never used here."
  (+orgbrain--build-ask (concat "remember that " text) project))

(defun +orgbrain--build-recall (text project)
  "Build a `recall' call for TEXT.
`recall' takes the query positionally, so nothing goes on stdin.  PROJECT
is ignored: v0.1.0 recall is raw retrieval across the whole brain."
  (ignore project)
  (list :args (list "recall" text "--json")
        :stdin nil))

(defun +orgbrain--build-consult (text project)
  "Build a forced-consult `ask' for TEXT, scoped to PROJECT.

`--consult' makes the kernel run one one-shot consultant after the local
answer, append its reply to the evidence ledger as a take, and compose
once more so the answer can cite it.  Measured on Vienna: **293 s**, of
which the consultant is ~90 s -- an order of magnitude slower than a
plain ask, and the prompt leaves the machine.  So it is its own mode
rather than a flag: TAB has to be turned to it deliberately, and the
label is visible in the header line while it runs.

`orgbrain ask --consult' requires the daemon to carry the consult
pipeline.  An older one drops the flag and answers locally, which looks
identical apart from `consults' being absent from the receipt."
  (list :args (append (list "ask" "-" "--json" "--consult")
                      (+orgbrain--entity-args project))
        :stdin text))

(defvar +orgbrain-modes
  '((ask
     :label "ask"
     :builder +orgbrain--build-ask
     :hint "compose an answer from the brain")
    (remember
     :label "remember"
     :builder +orgbrain--build-remember
     :hint "conversational write through ask")
    (recall
     :label "recall"
     :builder +orgbrain--build-recall
     :hint "raw retrieval, no compose")
    (consult
     :label "consult"
     :builder +orgbrain--build-consult
     :hint "local answer, then one Grok take -- ~5 min, leaves the host"))
  "Ordered request modes, each a plist keyed by mode symbol.
`:builder' is called with the input text and the current project slug and
returns a plist with `:args' (the CLI argument list) and `:stdin' (a
string or nil).  TAB cycles this list in order, so a new mode is one
entry here and no change anywhere else.")

(defun +orgbrain--mode-plist (mode)
  "Return the definition plist for MODE, signalling when it is unknown."
  (or (cdr (assq mode +orgbrain-modes))
      (error "Unknown OrgBrain mode: %s" mode)))

(defun +orgbrain--mode-label (mode)
  "Return the display label for MODE."
  (plist-get (+orgbrain--mode-plist mode) :label))

(defun +orgbrain--next-mode (mode)
  "Return the mode after MODE in `+orgbrain-modes', wrapping around."
  (let* ((order (mapcar #'car +orgbrain-modes))
         (rest (cdr (memq mode order))))
    (or (car rest) (car order))))

(defun +orgbrain--build-request (mode text project)
  "Return the CLI request plist for MODE, TEXT, and PROJECT."
  (funcall (plist-get (+orgbrain--mode-plist mode) :builder) text project))

;; =============================================================================
;; Projects
;; =============================================================================

(defun +orgbrain--strip-project-prefix (slug)
  "Return SLUG without a leading `projects/', or nil when SLUG is not a string."
  (when (stringp slug)
    (let ((clean (string-trim slug)))
      (unless (string-empty-p clean)
        (if (string-prefix-p "projects/" clean)
            (substring clean (length "projects/"))
          clean)))))

(defun +orgbrain--parse-project-list (json)
  "Return the project slugs in JSON, the parsed `project list --json' output.
The verb does not exist on the daemon yet, so accept the plausible
shapes: an array of slugs, an array of objects carrying a slug, or an
object wrapping either under `projects'."
  (let ((items (cond
                ((and (consp json) (assoc "projects" json))
                 (cdr (assoc "projects" json)))
                ((listp json) json))))
    (delq nil
          (mapcar (lambda (item)
                    (+orgbrain--strip-project-prefix
                     (cond
                      ((stringp item) item)
                      ((consp item)
                       (or (cdr (assoc "slug" item))
                           (cdr (assoc "name" item))
                           (cdr (assoc "id" item))
                           (cdr (assoc "entity" item)))))))
                  (and (listp items) items)))))

(defun +orgbrain--projects-from-records (records)
  "Return the project slugs appearing as `request.entity' in RECORDS."
  (delq nil (mapcar (lambda (record)
                      (+orgbrain--strip-project-prefix
                       (+orgbrain--truthy
                        (+orgbrain--dig record "request" "entity"))))
                    records)))

(defun +orgbrain--merge-projects (&rest lists)
  "Return the sorted union of the project slugs in LISTS."
  (sort (delete-dups (delq nil (apply #'append lists))) #'string<))

(defun +orgbrain-projects ()
  "Return (SLUGS . SOURCE), the project set the switcher offers.
SOURCE is `server' when `orgbrain project list --json' answered,
`server-truncated' when it answered but reported an incomplete list,
`history' when the set was derived from job history, and `default' when
only `+orgbrain-default-projects' is known."
  (let* ((payload
          (condition-case nil
              (+orgbrain--parse-json (+orgbrain--cli '("project" "list" "--json")))
            (error nil)))
         (from-server (and payload (+orgbrain--parse-project-list payload))))
    (if from-server
        (cons (+orgbrain--merge-projects from-server +orgbrain-default-projects)
              ;; `project list' reports its own completeness because the
              ;; sanitizer caps the page list at 24.  Discarding that here
              ;; would show a short list as though it were the whole set.
              (if (+orgbrain--truthy
                   (+orgbrain--dig payload "truncated"))
                  'server-truncated
                'server))
      (let ((from-history
             (condition-case nil
                 (+orgbrain--projects-from-records (+orgbrain--history-records))
               (error nil))))
        (cons (+orgbrain--merge-projects from-history +orgbrain-default-projects)
              (if from-history 'history 'default))))))

(defun +orgbrain--projects-note (source)
  "Return the header-line note describing where the project list came from."
  (pcase source
    ('server "")
    ('server-truncated "  |  projects: truncated")
    ('history "  |  projects: history")
    ('default "  |  projects: default")
    (_ "")))

;; =============================================================================
;; Dialogue exchanges
;; =============================================================================

(defun +orgbrain--history-records ()
  "Return the parsed `orgbrain history --json' array, oldest first."
  (let* ((out (+orgbrain--cli (list "history"
                                    "--limit" (number-to-string +orgbrain-history-limit)
                                    "--json")))
         (parsed (+orgbrain--parse-json-strict out "`orgbrain history --json'")))
    (unless (listp parsed)
      (user-error "`orgbrain history --json' did not return a job array"))
    (sort (copy-sequence parsed)
          (lambda (a b)
            (string< (or (+orgbrain--dig a "created_at") "")
                     (or (+orgbrain--dig b "created_at") ""))))))

(defun +orgbrain--record-sent-text (record)
  "Return the text the owner sent in RECORD.
Kind `ask' carries `request.text', `recall' carries `request.query', and
the legacy `remember' verb carries `request.fact'."
  (or (+orgbrain--truthy (+orgbrain--dig record "request" "text"))
      (+orgbrain--truthy (+orgbrain--dig record "request" "query"))
      (+orgbrain--truthy (+orgbrain--dig record "request" "fact"))
      ""))

(defun +orgbrain--record-exchange (record)
  "Return the exchange plist for job RECORD."
  (list :id (+orgbrain--truthy (+orgbrain--dig record "id"))
        :kind (or (+orgbrain--truthy (+orgbrain--dig record "kind")) "ask")
        :state (or (+orgbrain--truthy (+orgbrain--dig record "state")) "unknown")
        :entity (+orgbrain--truthy (+orgbrain--dig record "request" "entity"))
        :created (+orgbrain--truthy (+orgbrain--dig record "created_at"))
        :sent (+orgbrain--record-sent-text record)
        :record record))

(defun +orgbrain--exchanges-from-records (records project)
  "Return the exchanges in RECORDS belonging to PROJECT, oldest first.
A nil PROJECT keeps every record, scoped or not."
  (let ((entity (and project (concat "projects/" project))))
    (mapcar #'+orgbrain--record-exchange
            (if entity
                (seq-filter (lambda (record)
                              (equal entity
                                     (+orgbrain--truthy
                                      (+orgbrain--dig record "request" "entity"))))
                            records)
              records))))

(defun +orgbrain-exchanges (project)
  "Return PROJECT's dialogue exchanges, oldest first.

This is the only place that knows how a dialogue is stored.  Today it
reads `orgbrain history --json' and filters client-side on
`request.entity', because history has no project filter; when OrgBrain
grows a per-project dialogue store, replacing this function is the whole
migration."
  (+orgbrain--exchanges-from-records (+orgbrain--history-records) project))

;; =============================================================================
;; Rendering
;; =============================================================================

(defun +orgbrain--format-count (value)
  "Return VALUE as a display string, counting a list and blanking JSON null.
Floats are rounded: raw millisecond timings run to fifteen digits."
  (cond
   ((null (+orgbrain--truthy value)) "none")
   ((floatp value) (format "%.1f" value))
   ((listp value) (number-to-string (length value)))
   (t (format "%s" value))))

(defun +orgbrain--format-gbrain-calls (calls)
  "Return the one-line summary of the `gbrain_calls' object CALLS."
  (if (consp calls)
      (format "kernel %s  worker %s  total %s  gbrain_ms %s"
              (+orgbrain--format-count (+orgbrain--dig calls "kernel"))
              (+orgbrain--format-count (+orgbrain--dig calls "worker"))
              (+orgbrain--format-count (+orgbrain--dig calls "total"))
              (+orgbrain--format-count (+orgbrain--dig calls "gbrain_ms")))
    "none"))

(defun +orgbrain--format-receipt (result)
  "Return the plainly formatted receipt lines for a job RESULT."
  (let ((receipt (+orgbrain--dig result "answer_receipt")))
    (string-join
     (list
      "-- receipt --"
      (format "grounding:       %s"
              (+orgbrain--format-count (+orgbrain--dig receipt "grounding")))
      (format "citations:       %s"
              (+orgbrain--format-count
               (or (+orgbrain--dig receipt "citations")
                   (+orgbrain--dig result "citations"))))
      (format "content_entries: %s"
              (+orgbrain--format-count
               (+orgbrain--dig receipt "utilisation" "content_entries")))
      (format "gbrain_calls:    %s"
              (+orgbrain--format-gbrain-calls
               (or (+orgbrain--dig receipt "gbrain_calls")
                   (+orgbrain--dig result "gbrain_calls"))))
      (format "latency_ms:      %s"
              (+orgbrain--format-count (+orgbrain--dig receipt "latency_ms")))
      (format "model_revision:  %s"
              (+orgbrain--format-count (+orgbrain--dig receipt "model_revision")))
      (format "failure_class:   %s"
              (+orgbrain--format-count (+orgbrain--dig receipt "failure_class"))))
     "\n")))

(defun +orgbrain--format-gaps (result)
  "Return the `gaps' block of RESULT, or nil when there are none."
  (let ((gaps (or (+orgbrain--truthy (+orgbrain--dig result "gaps"))
                  (+orgbrain--truthy
                   (+orgbrain--dig result "answer_receipt" "gaps")))))
    (when (consp gaps)
      (concat "-- gaps --\n"
              (mapconcat (lambda (gap) (format "  - %s" gap)) gaps "\n")))))

(defun +orgbrain--format-memory (memory)
  "Return the formatted `result.memory' block MEMORY of a recall receipt."
  (concat
   (format "-- memory --\ntotal: %s  budget_used: %s  search_degraded: %s\n"
           (+orgbrain--format-count (+orgbrain--dig memory "total"))
           (+orgbrain--format-count (+orgbrain--dig memory "budget_used"))
           (+orgbrain--format-count (+orgbrain--dig memory "search_degraded")))
   (let ((facts (+orgbrain--truthy (+orgbrain--dig memory "facts"))))
     (if (consp facts)
         (mapconcat (lambda (fact)
                      (format "  - [%s] %s"
                              (+orgbrain--format-count
                               (+orgbrain--dig fact "entity_slug"))
                              (+orgbrain--format-count
                               (+orgbrain--dig fact "fact"))))
                    facts "\n")
       "  (no facts)"))))

(defun +orgbrain--format-body (record)
  "Return the answer or memory body of job RECORD."
  (let* ((result (+orgbrain--dig record "result"))
         (answer (+orgbrain--truthy (+orgbrain--dig result "answer")))
         (memory (+orgbrain--truthy (+orgbrain--dig result "memory")))
         (problem (+orgbrain--truthy (+orgbrain--dig record "error")))
         (state (+orgbrain--truthy (+orgbrain--dig record "state"))))
    (string-join
     (delq nil
           (list (when (and state (not (equal state "succeeded")))
                   (format "state: %s" state))
                 (when problem (format "error: %s" problem))
                 (when answer (string-trim-right answer))
                 (when (consp memory) (+orgbrain--format-memory memory))
                 (unless (or answer memory problem)
                   "(no answer in this receipt)")
                 (+orgbrain--format-gaps result)
                 (+orgbrain--format-receipt result)))
     "\n\n")))

(defun +orgbrain--format-exchange (exchange &optional heading)
  "Return the transcript text for EXCHANGE, titled with HEADING.
HEADING defaults to the exchange kind."
  (let ((record (plist-get exchange :record)))
    (format "=== %s  %s  %s ===\n> %s\n\n%s\n\n"
            (or heading (plist-get exchange :kind))
            (or (plist-get exchange :entity) "unscoped")
            (or (plist-get exchange :created) "")
            (string-join (split-string (string-trim (plist-get exchange :sent)) "\n")
                         "\n> ")
            (+orgbrain--format-body record))))

;; =============================================================================
;; Buffers
;; =============================================================================

(defun +orgbrain--buffer-name (kind)
  "Return the workspace buffer name for KIND, either `output' or `input'."
  (pcase kind
    ('output "*orgbrain*")
    ('input "*orgbrain-input*")
    (_ (error "Unknown OrgBrain buffer kind: %s" kind))))

;; Keymaps must exist before `define-derived-mode', which would otherwise
;; create empty maps that a later `defvar' cannot replace.
(defvar +orgbrain-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+orgbrain/send)
    (define-key map (kbd "C-c C-p") #'+orgbrain/switch-project)
    (define-key map (kbd "<up>") #'+orgbrain/previous-exchange)
    (define-key map (kbd "<down>") #'+orgbrain/next-exchange)
    (define-key map (kbd "q") #'+orgbrain/quit)
    map)
  "Keymap for `+orgbrain-mode'.")

(define-derived-mode +orgbrain-mode special-mode "OrgBrain"
  "Mode for the read-only OrgBrain output transcript."
  :interactive nil
  :group 'orgbrain
  (setq truncate-lines nil)
  ;; `global-so-long-mode' would strip font-lock from a single long answer.
  (when (boundp 'so-long-threshold)
    (set (make-local-variable 'so-long-threshold) nil)))

(defvar +orgbrain-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+orgbrain/send)
    (define-key map (kbd "C-c C-p") #'+orgbrain/switch-project)
    map)
  "Keymap for `+orgbrain-input-mode'.")

(define-derived-mode +orgbrain-input-mode text-mode "OrgBrain-Input"
  "Mode for the editable OrgBrain input buffer."
  :interactive nil
  :group 'orgbrain
  (setq truncate-lines nil))

(defun +orgbrain--buffer (kind)
  "Return the workspace buffer for KIND, creating and initialising it."
  (let* ((name (+orgbrain--buffer-name kind))
         (buf (or (get-buffer name) (get-buffer-create name))))
    (with-current-buffer buf
      (pcase kind
        ('output (unless (derived-mode-p '+orgbrain-mode) (+orgbrain-mode)))
        ('input (unless (derived-mode-p '+orgbrain-input-mode)
                  (+orgbrain-input-mode)))))
    ;; persp-mode hides buffers it has never been told about.  Bind
    ;; `persp-switch-to-added-buffer' off: it defaults to t, which makes
    ;; `persp-add-buffer' switch the SELECTED window to the buffer it was
    ;; handed.  Building the split calls this twice, so the second call
    ;; clobbered the window the transcript had just been put in and both
    ;; panes ended up showing the input buffer -- with the answer appended
    ;; to a buffer displayed nowhere.
    (when (fboundp 'persp-add-buffer)
      (let ((persp-switch-to-added-buffer nil))
        (ignore-errors (persp-add-buffer buf))))
    buf))

(defun +orgbrain--live-buffer (kind)
  "Return the existing workspace buffer for KIND, or nil."
  (get-buffer (+orgbrain--buffer-name kind)))

(defun +orgbrain--header-line (kind)
  "Return the header line for the KIND pane, `output' or `input'.
Each pane is labelled, because the two are otherwise indistinguishable
at a glance -- issue #6's own sketch labels them OUTPUT and INPUT.  Only
the input pane carries `mode\=': the mode decides what a send does, and a
send is issued from there, so showing it on the transcript said nothing."
  (concat
   (pcase kind ('output "OUTPUT") ('input "INPUT"))
   (format "  |  project: %s" (or +orgbrain--project "unscoped"))
   (when (eq kind 'input)
     (format "  |  mode: %s" (+orgbrain--mode-label +orgbrain--mode)))
   (format "  |  %s %s%s"
           (if (eq +orgbrain-transport #'+orgbrain--transport-local)
               "local"
             +orgbrain-ssh-host)
           +orgbrain--status
           ;; Elapsed seconds, not a spinner: the useful question during a
           ;; long send is "how long has this been going", and a consulted
           ;; ask legitimately runs into the hundreds.
           (let ((seconds (+orgbrain--elapsed)))
             (if seconds (format " %ds" seconds) "")))
   (+orgbrain--projects-note +orgbrain--projects-source)))

(defun +orgbrain--elapsed ()
  "Whole seconds since the in-flight request started, or nil when idle."
  (let ((started (plist-get +orgbrain--pending :started)))
    (and (numberp started) (max 0 (floor (- (float-time) started))))))

(defun +orgbrain--stop-tick ()
  "Cancel the header-line refresh timer."
  (when (timerp +orgbrain--tick-timer)
    (cancel-timer +orgbrain--tick-timer))
  (setq +orgbrain--tick-timer nil))

(defun +orgbrain--start-tick ()
  "Refresh the header line every second while a request is outstanding.
The timer cancels itself when nothing is pending, so a lost callback
cannot leave it running for the rest of the session."
  (+orgbrain--stop-tick)
  (setq +orgbrain--tick-timer
        (run-at-time 1 1 (lambda ()
                           (if +orgbrain--pending
                               (+orgbrain--refresh-header)
                             (+orgbrain--stop-tick))))))

(defun +orgbrain--refresh-header ()
  "Re-render the header line in both workspace buffers."
  (dolist (kind '(output input))
    (let ((buf (+orgbrain--live-buffer kind)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local header-line-format (+orgbrain--header-line kind))
          (force-mode-line-update))))))

(defun +orgbrain--set-status (status)
  "Set the workspace transport STATUS and refresh the header line."
  (setq +orgbrain--status status)
  (+orgbrain--refresh-header))

(defun +orgbrain--append (text)
  "Append TEXT to the output buffer and show its end in every window."
  (let ((buf (+orgbrain--buffer 'output)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text))
      (setq-local buffer-read-only t)
      (dolist (win (get-buffer-window-list buf nil t))
        (set-window-point win (point-max))))))

(defun +orgbrain--set-input (text)
  "Replace the input buffer contents with TEXT."
  (with-current-buffer (+orgbrain--buffer 'input)
    (erase-buffer)
    (insert (or text ""))
    (goto-char (point-min))))

(defun +orgbrain--input-text ()
  "Return the trimmed contents of the input buffer."
  (let ((buf (+orgbrain--live-buffer 'input)))
    (if (buffer-live-p buf)
        (with-current-buffer buf (string-trim (buffer-string)))
      "")))

;; =============================================================================
;; Window layout
;; =============================================================================

(defun +orgbrain--output-window ()
  "Return the window the output buffer should occupy.
Reuses a window already showing it, so reopening the workspace does not
split the frame again.  The selected window is deliberately not trusted:
point normally sits in the input pane, and treating that pane as the
output pane split it once per `+orgbrain/open' call."
  (let* ((frame (selected-frame))
         (input (frame-parameter frame '+orgbrain-input-window))
         ;; Killing the input buffer leaves its window showing the output
         ;; buffer.  Claiming that window as the output pane would split the
         ;; frame again and leave two panes on the same transcript.
         (existing (seq-find (lambda (win) (not (eq win input)))
                             (get-buffer-window-list
                              (+orgbrain--buffer 'output) nil frame)))
         (selected (selected-window)))
    (cond
     ((window-live-p existing) existing)
     ((and (window-live-p input) (eq input selected))
      (or (seq-find (lambda (win) (not (eq win input))) (window-list frame))
          (split-window input nil 'above)))
     (t selected))))

(defun +orgbrain--input-window (output-window)
  "Return the reusable input window below OUTPUT-WINDOW, creating it if needed."
  (let* ((frame (selected-frame))
         (win (or (get-buffer-window (+orgbrain--buffer 'input) frame)
                  (frame-parameter frame '+orgbrain-input-window))))
    (unless (and (window-live-p win) (not (eq win output-window)))
      (let* ((height (window-height output-window))
             ;; 0.3 is this config's proportion for a secondary pane, but an
             ;; unclamped 6-line floor asks for a negative upper window on a
             ;; short pane, which `split-window' rejects outright.
             (keep (max window-min-height
                        (min (max 6 (round (* 0.3 height)))
                             (- height window-min-height)))))
        (when (< height (* 2 window-min-height))
          (user-error "orgbrain: window too short to split; enlarge the frame"))
        (setq win (split-window output-window (- height keep) 'below))))
    (set-frame-parameter frame '+orgbrain-input-window win)
    win))

(defun +orgbrain--normal-state ()
  "Put the current buffer in Evil normal state when Evil is live.
`evil-set-initial-state' only affects buffers created later, so newly
displayed buffers need this explicitly."
  (when (and (bound-and-true-p evil-mode) (fboundp 'evil-normal-state))
    (evil-normal-state)))

(defun +orgbrain--display-workspace ()
  "Show the output buffer on top and the input buffer below, selecting input."
  (let* ((output (+orgbrain--buffer 'output))
         (input (+orgbrain--buffer 'input))
         (output-window (+orgbrain--output-window)))
    (set-window-buffer output-window output)
    (let ((input-window (+orgbrain--input-window output-window)))
      (set-window-buffer input-window input)
      (with-current-buffer output (+orgbrain--normal-state))
      (select-window input-window)
      (with-current-buffer input (+orgbrain--normal-state)))))

;; =============================================================================
;; Commands
;; =============================================================================

(defun +orgbrain/open ()
  "Open the OrgBrain workspace: output on top, input below, point in input."
  (interactive)
  (unless +orgbrain--project
    (let ((projects (+orgbrain-projects)))
      (setq +orgbrain--project (car (car projects))
            +orgbrain--projects-source (cdr projects))))
  (+orgbrain--display-workspace)
  (+orgbrain--refresh-header)
  (message "orgbrain: %s, mode %s.  TAB cycles mode, RET sends"
           (or +orgbrain--project "unscoped")
           (+orgbrain--mode-label +orgbrain--mode)))

(defun +orgbrain/cycle-mode ()
  "Cycle the request mode to the next entry in `+orgbrain-modes'."
  (interactive)
  (setq +orgbrain--mode (+orgbrain--next-mode +orgbrain--mode))
  (+orgbrain--refresh-header)
  (message "orgbrain mode: %s (%s)"
           (+orgbrain--mode-label +orgbrain--mode)
           (plist-get (+orgbrain--mode-plist +orgbrain--mode) :hint)))

(defconst +orgbrain--unscoped-choice "(unscoped)"
  "Completion candidate selecting no project at all.
The switcher offers it because most jobs in the daemon's history carry
`request.entity: null' — an unscoped workspace is the only way to replay
those, and without this the switcher could never return to one.")

(defun +orgbrain/switch-project ()
  "Switch the workspace project, scoping every later call to it.
Choosing `+orgbrain--unscoped-choice' clears the scope: calls pass no
`--entity' and the dialogue walk covers every exchange."
  (interactive)
  (let* ((projects (+orgbrain-projects))
         (slugs (car projects))
         (source (cdr projects)))
    (unless slugs
      (user-error "No OrgBrain projects known; customize `+orgbrain-default-projects'"))
    (let ((choice (completing-read "OrgBrain project: "
                                   (cons +orgbrain--unscoped-choice slugs)
                                   nil t nil nil
                                   (or +orgbrain--project
                                       +orgbrain--unscoped-choice))))
      (setq +orgbrain--projects-source source
            +orgbrain--project (unless (equal choice +orgbrain--unscoped-choice)
                                choice)
            +orgbrain--exchanges nil
            +orgbrain--exchanges-project nil
            +orgbrain--exchange-index nil))
    (+orgbrain--refresh-header)
    (message "orgbrain project: %s (project list from %s)"
             (or +orgbrain--project "unscoped") source)))

(defun +orgbrain--finish-send (mode text stdout problem)
  "Render the reply to a MODE request of TEXT, or report PROBLEM.
STDOUT is the raw CLI output when the call succeeded."
  (+orgbrain--stop-tick)
  (setq +orgbrain--pending nil
        +orgbrain--exchanges nil
        +orgbrain--exchanges-project nil
        +orgbrain--exchange-index nil)
  (let ((label (+orgbrain--mode-label mode))
        (record (and (null problem) (+orgbrain--parse-json stdout))))
    (cond
     (problem
      (+orgbrain--set-status 'error)
      (message "orgbrain: %s" problem))
     ((null record)
      (+orgbrain--set-status 'error)
      (message "orgbrain: could not parse the %s receipt; see %s"
               label (+orgbrain--buffer-name 'output))
      (+orgbrain--append (format "=== %s  unparseable receipt ===\n%s\n\n"
                                 label (string-trim (or stdout "")))))
     (t
      ;; A fresh receipt echoes no request, so keep what was actually sent.
      (let ((exchange (plist-put (+orgbrain--record-exchange record) :sent text)))
        (+orgbrain--set-status 'idle)
        (+orgbrain--append (+orgbrain--format-exchange exchange label))
        ;; Clear the brief only once the transcript holds it, so a failed
        ;; send never costs the owner the thought they typed.
        (+orgbrain--set-input "")
        (message "orgbrain %s: done in %s ms" label
                 (+orgbrain--format-count
                  (+orgbrain--dig record "result" "answer_receipt"
                                  "latency_ms"))))))))

(defun +orgbrain/send ()
  "Send the input buffer through the current request mode.
Refuses while another request is in flight: the daemon rejects a write
while any job is running, and losing a thought is worse than waiting."
  (interactive)
  (when +orgbrain--pending
    ;; A flag can outlive the request it describes -- a reloaded module, or a
    ;; build that left it set after a signal -- and then the callback that
    ;; would clear it never runs, so obeying it refuses every later send
    ;; forever.  A flag is judged stale when it records a process that has
    ;; died, or records no `:process' key at all (the shape older state has).
    ;; A flag that records nil is left alone: a transport that returns no
    ;; process cannot be second-guessed, and `+orgbrain/reset' is the hatch.
    (if (and (plist-member +orgbrain--pending :process)
             (let ((proc (plist-get +orgbrain--pending :process)))
               (or (null proc) (process-live-p proc))))
        (user-error "orgbrain: %s still in flight; send is disabled until it returns (`M-x +orgbrain/reset' if it is not)"
                    (+orgbrain--mode-label (plist-get +orgbrain--pending :mode)))
      (setq +orgbrain--pending nil)))
  (let ((text (+orgbrain--input-text))
        (mode +orgbrain--mode))
    (when (string-empty-p text)
      (user-error "orgbrain: the input buffer is empty"))
    (let* ((request (+orgbrain--build-request mode text +orgbrain--project))
           (label (+orgbrain--mode-label mode)))
      (setq +orgbrain--pending
            (list :mode mode :started (float-time) :process nil))
      (+orgbrain--set-status 'working)
      (+orgbrain--start-tick)
      (message "orgbrain %s: sent to %s, waiting (%s)"
               label +orgbrain-ssh-host
               (if (eq mode 'consult)
                   "a consulted ask takes ~5 min"
                 "an ask takes 19-25s"))
      ;; If the dispatch signals, `+orgbrain--pending' must not survive it:
      ;; a stuck pending refuses every later send until Emacs restarts.
      (condition-case signalled
          (let ((proc (+orgbrain--cli
                       (plist-get request :args)
                       (plist-get request :stdin)
                       (lambda (stdout problem)
                         (+orgbrain--finish-send mode text stdout problem)))))
            ;; Record the process so a flag left behind by a lost callback can
            ;; be recognised as stale.  The reply may already have landed and
            ;; cleared the flag, hence the guard.
            (when (and +orgbrain--pending (processp proc))
              (setq +orgbrain--pending
                    (plist-put +orgbrain--pending :process proc))))
        (error
         (+orgbrain--stop-tick)
         (setq +orgbrain--pending nil)
         (+orgbrain--set-status 'error)
         (signal (car signalled) (cdr signalled)))))))

(defun +orgbrain/reset ()
  "Clear the workspace's request state and rebuild the split.
The escape hatch for a client that believes a request is outstanding when
none is, and for buffers left behind by reloading the module."
  (interactive)
  (+orgbrain--stop-tick)
  (when (process-live-p (plist-get +orgbrain--pending :process))
    (ignore-errors (delete-process (plist-get +orgbrain--pending :process))))
  (setq +orgbrain--pending nil
        +orgbrain--status 'idle
        +orgbrain--exchanges nil
        +orgbrain--exchanges-project nil
        +orgbrain--exchange-index nil)
  (+orgbrain/open)
  (message "orgbrain: reset; project %s, mode %s"
           (or +orgbrain--project "unscoped")
           (+orgbrain--mode-label +orgbrain--mode)))

(defun +orgbrain--ensure-exchanges ()
  "Return the cached exchanges for the current project, fetching when stale."
  (unless (and +orgbrain--exchanges
               (equal +orgbrain--exchanges-project +orgbrain--project))
    (setq +orgbrain--exchanges (+orgbrain-exchanges +orgbrain--project)
          +orgbrain--exchanges-project +orgbrain--project
          +orgbrain--exchange-index nil))
  +orgbrain--exchanges)

(defun +orgbrain--replay (index)
  "Replay the exchange at INDEX into the input and output buffers."
  (let ((exchange (nth index +orgbrain--exchanges)))
    (setq +orgbrain--exchange-index index)
    (+orgbrain--set-input (plist-get exchange :sent))
    (+orgbrain--append
     (+orgbrain--format-exchange
      exchange (format "replay %d/%d %s"
                       (1+ index) (length +orgbrain--exchanges)
                       (plist-get exchange :kind))))
    (message "orgbrain: exchange %d/%d for %s"
             (1+ index) (length +orgbrain--exchanges)
             (or +orgbrain--project "unscoped"))))

(defun +orgbrain/previous-exchange ()
  "Replay the previous exchange in the current project's dialogue."
  (interactive)
  (let* ((exchanges (+orgbrain--ensure-exchanges))
         (count (length exchanges)))
    (when (zerop count)
      (user-error "orgbrain: no recorded exchanges for %s"
                  (or +orgbrain--project "this brain")))
    (let ((index (1- (or +orgbrain--exchange-index count))))
      (when (< index 0)
        (user-error "orgbrain: already at the oldest exchange"))
      (+orgbrain--replay index))))

(defun +orgbrain/next-exchange ()
  "Replay the next exchange in the current project's dialogue."
  (interactive)
  (let* ((exchanges (+orgbrain--ensure-exchanges))
         (count (length exchanges)))
    (when (zerop count)
      (user-error "orgbrain: no recorded exchanges for %s"
                  (or +orgbrain--project "this brain")))
    (let ((index (1+ (or +orgbrain--exchange-index -1))))
      (when (>= index count)
        (user-error "orgbrain: already at the newest exchange"))
      (+orgbrain--replay index))))

(defun +orgbrain/quit ()
  "Bury the workspace, leaving the transcript intact."
  (interactive)
  (let ((input-window (frame-parameter (selected-frame) '+orgbrain-input-window)))
    (when (window-live-p input-window)
      (ignore-errors (delete-window input-window))
      (set-frame-parameter (selected-frame) '+orgbrain-input-window nil)))
  (dolist (kind '(input output))
    (let ((buf (+orgbrain--live-buffer kind)))
      (when (buffer-live-p buf)
        (dolist (win (get-buffer-window-list buf nil t))
          (ignore-errors (quit-window nil win)))
        (bury-buffer buf)))))

;; =============================================================================
;; Key bindings (Evil; base maps are defined above the derived modes)
;; =============================================================================

(with-eval-after-load 'evil
  (when (fboundp 'evil-ex-define-cmd)
    (evil-ex-define-cmd "orgbrain" #'+orgbrain/open)
    (evil-ex-define-cmd "orgbrain-project" #'+orgbrain/switch-project)
    (evil-ex-define-cmd "orgbrain-reset" #'+orgbrain/reset))
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +orgbrain-mode-map
      (kbd "RET") #'+orgbrain/send
      (kbd "<up>") #'+orgbrain/previous-exchange
      (kbd "<down>") #'+orgbrain/next-exchange
      "gp" #'+orgbrain/switch-project
      "q" #'+orgbrain/quit
      (kbd "C-h") #'windmove-left
      (kbd "C-l") #'windmove-right
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up)
    (evil-define-key 'normal +orgbrain-input-mode-map
      (kbd "TAB") #'+orgbrain/cycle-mode
      (kbd "<tab>") #'+orgbrain/cycle-mode
      (kbd "RET") #'+orgbrain/send
      (kbd "<up>") #'+orgbrain/previous-exchange
      (kbd "<down>") #'+orgbrain/next-exchange
      "gp" #'+orgbrain/switch-project
      "q" #'+orgbrain/quit
      (kbd "C-h") #'windmove-left
      (kbd "C-l") #'windmove-right
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up)
    ;; The input buffer is edited in insert state, where an unbound C-j
    ;; would insert a newline instead of moving to the window below.
    (evil-define-key 'insert +orgbrain-input-mode-map
      (kbd "C-h") #'windmove-left
      (kbd "C-l") #'windmove-right
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state '+orgbrain-mode 'normal)
    (evil-set-initial-state '+orgbrain-input-mode 'normal)))

(provide 'init-orgbrain)

;;; init-orgbrain.el ends here
