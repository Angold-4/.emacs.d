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
;; - propose   the same `ask' call with `remember that ' prefixed.  The CLI
;;             `remember' verb is deliberately NOT used: it fails
;;             `remember_not_admitted' when the text does not route to a
;;             write, and the conversational path through `ask' is the one that
;;             scopes correctly.  It proposes rather than writes -- see below.
;; - recall    `orgbrain recall <text> --json'.  `recall' takes the query as a
;;             positional argument, so this mode sends no stdin.  Recall stays
;;             unscoped (raw retrieval across the brain), and is the one verb
;;             with no conversation parser, so it starts no conversation.
;;
;; Conversations (`Angold-4/orgbrain#95'):
;;
;; An ask reaches conversational memory only when it carries a
;; `--conversation-id'.  Without one the daemon's `validate_request' returns
;; nil and the job falls through to the legacy single-turn `_ask': nothing is
;; captured, no dialogue history is retrieved, and no proposal is issued.  So
;; the workspace holds a conversation ID, starts a fresh one on `gn', and
;; starts one automatically when the project changes -- the daemon binds an ID
;; to owner + transport + project, and carrying one across a switch makes the
;; next turn a different scope under the same ID.
;;
;; Capture is not knowledge admission, and the two succeed or fail separately.
;; A conversational `remember that X' captures the dialogue and creates a
;; candidate in `pending_confirmation'; accepted knowledge is unchanged until
;; the owner approves that exact candidate.  Hence `propose', not `remember'.
;; Approval binds the candidate ID, its 64-hex hash, and its version, and the
;; daemon refuses every shortcut around them -- so `gy' sends the three fields
;; structurally out of the receipt rather than making the owner yank a hash out
;; of the transcript, which is exactly the tax this client exists to remove.
;;
;; The conversation receipt reports four independent outcomes -- capture,
;; knowledge, answer, delivery -- and all four are rendered.  A failed
;; composition is a normal outcome, not an error to hide: capture and the
;; proposal are decided and journaled before COMPOSE runs, so an answer reading
;; "I could not compose an answer" can sit above a verified memory effect, and
;; no assistant turn is captured in that state.
;;
;; Keys (Evil normal state, inside the workspace buffers):
;;
;; - TAB           cycle the request mode (input buffer)
;; - RET, C-c C-c  send
;; - <up>/<down>   walk the current project's dialogue and replay an exchange
;; - gp, C-c C-p   switch project (`completing-read')
;; - gn, C-c C-n   start a new conversation
;; - gy, C-c C-y   approve the pending knowledge proposal;  gN rejects it
;; - gr, C-c C-r   aim the next send at the replayed exchange's assistant
;;                 turn; with a prefix argument, at its user turn
;; - gc            toggle `--capture-discussion'
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
;; - `#95' is unmerged, and a daemon without it fails `--conversation-id' at
;;   argparse with `unrecognized arguments' and a nonzero exit -- at the cost
;;   of whatever brief was just typed.  So `ask --help' is probed once at open,
;;   the flags are dropped when it does not mention them, and the header line
;;   says `conv: unsupported'.  A send that is refused that way anyway (the
;;   probe can go stale) downgrades the client rather than repeating itself.
;; - `ORGBRAIN_CONVERSATION_RETENTION' defaults off on the daemon, so a perfect
;;   conversation ID can still accumulate no history.  The receipt says so
;;   (`capture: {"status": "disabled"}') and the header line reports
;;   `capture: OFF (retention)' rather than letting it look like it worked.
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

;; Evil motions named in the keymaps below. Declared rather than left to the
;; byte-compiler because this file compiled without warnings before the motion
;; vocabulary was added, and that is a property worth keeping -- `init-git-ui.el'
;; emits fifty-one of these and they are no longer read.
(declare-function evil-next-visual-line "evil-commands")
(declare-function evil-previous-visual-line "evil-commands")
(declare-function evil-next-line "evil-commands")
(declare-function evil-previous-line "evil-commands")
(declare-function evil-beginning-of-line "evil-commands")
(declare-function evil-end-of-line "evil-commands")

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

(defvar +orgbrain--conversation nil
  "Conversation ID every scoped call carries, or nil before the first one.
The ID is what makes turns cohere on the daemon, so it is neither
regenerated per send nor kept forever: `+orgbrain/new-conversation' starts
a fresh one, and switching project starts one automatically because the
daemon binds an ID to owner + transport + project.")

(defvar +orgbrain--conversation-project nil
  "Project slug `+orgbrain--conversation' was started under.
A conversation carried into another project is a different scope under the
same ID, which the daemon rejects and history renders confusingly.")

(defvar +orgbrain--conversation-support 'unknown
  "Whether the daemon understands `--conversation-id': `yes', `no', `unknown'.
`unknown' and `no' both send no conversation flags, so an older daemon
answers single-turn asks instead of failing at argparse.")

(defvar +orgbrain--capture-state 'unknown
  "What the last conversation receipt said about capture.
`on' when a turn was captured, `disabled' when the daemon reported capture
off, `unknown' before any answer.  The daemon defaults
`ORGBRAIN_CONVERSATION_RETENTION' off, so a perfect conversation ID can
still accumulate no history; this is the only way the buffer can say so.")

(defvar +orgbrain--capture-discussion nil
  "Non-nil to pass `--capture-discussion', forcing capture of a plain turn.
Retention off means an ordinary question preserves nothing.  This asks for
this turn to be kept anyway; the daemon still needs a conversation source
configured and refuses `conversation_source_required' when it has none.")

(defvar +orgbrain--reply-target nil
  "Turn ID the next send replies to, or nil.
Armed from a replayed exchange by `+orgbrain/set-reply-target' and cleared
once used.  The daemon refuses to guess a target -- `Remember this' with
none resolved returns one clarification and commits nothing -- so this is
never inferred from \"the latest visible message\".")

(defvar +orgbrain--candidate nil
  "The pending knowledge proposal from the last answer, or nil.
A plist with `:id', `:hash', `:version', `:text', `:proposal', `:planned',
`:conversation', and `:entity'.  Approval binds the exact candidate, so
every field is carried verbatim rather than reconstructed.")

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
failure.  Otherwise call CALLBACK with STDOUT and ERROR.  A failed job
can supply both: its JSON receipt still describes independently verified
effects."
  (if callback
      (funcall +orgbrain-transport args stdin
               (lambda (result)
                 (let ((problem (+orgbrain--result-error result)))
                   (funcall callback
                            (plist-get result :stdout)
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
;; Conversations
;; =============================================================================
;;
;; `Angold-4/orgbrain#95' routes an ask through `execute_conversation' only
;; when the request carries a conversation ID; `validate_request' returns nil
;; without one and the job falls back to the legacy single-turn `_ask'.  So the
;; ID is not a nicety -- it is the whole switch between this client reaching
;; conversational memory and never reaching it.

(defconst +orgbrain--conversation-id-rule "\\`[A-Za-z0-9][A-Za-z0-9_-]\\{0,127\\}\\'"
  "Mirror of the daemon's conversation ID rule (`orgbrain/conversation.py').
Checked here so a slug with a dot or a slash in it costs no round trip and
no `invalid_conversation_identity'.")

(defun +orgbrain--conversation-id-valid-p (id)
  "Non-nil when ID is a conversation ID the daemon will accept."
  (and (stringp id) (string-match-p +orgbrain--conversation-id-rule id)))

(defvar +orgbrain--conversation-serial 0
  "Counter making every conversation ID of this session distinct.
A timestamp alone is not enough: two `gn' presses inside the same tick
would return the same ID, and `gn' would report a new conversation while
silently continuing the old one, with its turns still cohering.  A
counter cannot collide within a session, and the timestamp separates
sessions.")

(defun +orgbrain--new-conversation-id (project)
  "Return a fresh conversation ID for PROJECT.
A slug, a millisecond timestamp, and a per-session counter: readable in
`history', unique per conversation, and within the daemon's character
class once the slug is sanitised.  `gn' promises a *fresh* conversation,
and an ID that silently repeats breaks that invisibly."
  (let* ((stem (or project "unscoped"))
         (safe (replace-regexp-in-string "[^A-Za-z0-9_-]" "-" stem))
         (safe (if (string-match-p "\\`[A-Za-z0-9]" safe) safe (concat "c" safe)))
         (id (format "%s-%s-%d" safe (format-time-string "%Y%m%dt%H%M%S%3N")
                     (cl-incf +orgbrain--conversation-serial))))
    ;; Truncation cannot break the leading-character rule, and 128 characters
    ;; is far more than a slug plus a timestamp needs.
    ;; GBrain canonicalizes page slugs to lowercase. Preserve the same ID
    ;; through capture and exact readback instead of emitting an uppercase T.
    (downcase (substring id 0 (min (length id) 128)))))

(defun +orgbrain--ensure-conversation ()
  "Return the current conversation ID, starting one when there is none."
  (unless (and +orgbrain--conversation
               (equal +orgbrain--conversation-project +orgbrain--project))
    (setq +orgbrain--conversation (+orgbrain--new-conversation-id +orgbrain--project)
          +orgbrain--conversation-project +orgbrain--project
          +orgbrain--capture-state 'unknown
          +orgbrain--reply-target nil
          +orgbrain--candidate nil))
  +orgbrain--conversation)

(defun +orgbrain--probe-conversation-support ()
  "Ask the daemon whether `ask' takes `--conversation-id', and cache it.

Probed rather than assumed, and probed with `--help' rather than with a
real send: `#95' is unmerged, and a daemon without it fails
`--conversation-id' at argparse with a nonzero exit and `unrecognized
arguments' -- not a friendly error, and at the cost of whatever brief was
just typed.  This is the same shape as the `project list' fallback: try,
degrade, and say so in the header line.

A probe that cannot run at all leaves the support state `unknown', which
sends no conversation flags either, so a down tunnel never turns into a
wrong answer about the daemon's features."
  (setq +orgbrain--conversation-support
        (condition-case nil
            (let ((help (+orgbrain--cli '("ask" "--help"))))
              (if (and (stringp help)
                       (string-match-p "--conversation-id" help))
                  'yes
                'no))
          (error 'unknown))))

(defun +orgbrain--conversation-args ()
  "Return the conversation flags for a send, or nil when they must be dropped.
Reads state only; the probe lives in `+orgbrain--probe-conversation-support'
so the request builders stay pure and testable."
  (when (and (eq +orgbrain--conversation-support 'yes)
             (+orgbrain--conversation-id-valid-p +orgbrain--conversation))
    (append (list "--conversation-id" +orgbrain--conversation)
            (when +orgbrain--capture-discussion (list "--capture-discussion"))
            (when +orgbrain--reply-target
              (list "--reply-to-turn-id" +orgbrain--reply-target)))))

(defun +orgbrain--conversation-receipt (result)
  "Return the `conversation_receipt' of RESULT, wherever it is carried.
A succeeded job puts it at the top level; a job whose composition failed
carries it under `answer_receipt.conversation', which is also where
`recover_captures' rewrites it after a retried projection."
  (or (+orgbrain--truthy (+orgbrain--dig result "conversation_receipt"))
      (+orgbrain--truthy (+orgbrain--dig result "answer_receipt" "conversation"))))

(defun +orgbrain--conversation-note ()
  "Return the header-line note describing the conversation scope."
  (pcase +orgbrain--conversation-support
    ('no "  |  conv: unsupported")
    ('unknown "  |  conv: unprobed")
    (_ (format "  |  conv: %s  |  capture: %s"
               (or +orgbrain--conversation "none")
               (pcase +orgbrain--capture-state
                 ('on "on")
                 ('disabled "OFF (retention)")
                 (_ "?"))))))

;; =============================================================================
;; Request modes
;; =============================================================================

(defun +orgbrain--entity-args (project)
  "Return the `--entity' arguments scoping a call to PROJECT, or nil."
  (when (and project (not (string-empty-p project)))
    (list "--entity" (concat "projects/" project))))

(defun +orgbrain--build-ask (text project)
  "Build an `ask' call sending TEXT on stdin, scoped to PROJECT.
Carries the conversation flags when the daemon understands them: without a
`--conversation-id' the daemon\='s `validate_request' returns nil and the
job falls through to the legacy single-turn path, which captures nothing,
retrieves no dialogue history, and issues no proposals."
  (list :args (append (list "ask" "-" "--json")
                      (+orgbrain--entity-args project)
                      (+orgbrain--conversation-args))
        :stdin text))

(defconst +orgbrain--remember-supplied
  "\\`[ \t\n]*\\(?:\\(?:can\\|could\\|would\\|will\\)[ \t\n]+you[ \t\n]+\\)?\\(?:please[ \t\n]+\\)?\\(?:remember\\|save\\|keep\\|capture\\|store\\)\\(?:[ \t\n]*:\\|[ \t\n]+\\(?:this\\|that\\|it\\|the[ \t\n]+\\(?:discussion\\|conversation\\|above\\|following\\)\\)\\b\\)"
  "Mirror of the daemon's `_PREFIX' + `_SUPPLIED' opener.
From `orgbrain/conversation_memory.py' on `feat/conversational-memory'.

Deliberately narrow.  Preservation authority requires the turn to supply
what is preserved: a deictic pointer at the present dialogue, or content
after a colon.  Matching the verb alone -- `remember', `save', `capture'
-- is what an earlier version of this file did, and it was wrong in the
direction that fails silently: `save my notes about the replay decision'
opens with a preservation verb, so the prefix was skipped, but the daemon
reads it as a recall and proposes nothing.

Used only to decide whether `propose' needs to add a prefix at all.  The
daemon, not this regexp, decides what is actually preserved -- and because
a mirror drifts whenever the server rule moves, the outcome is also
reported from the receipt by `+orgbrain--propose-warning', which cannot
drift.")

(defun +orgbrain--build-propose (text project)
  "Build the proposal call for TEXT, scoped to PROJECT.

This is an `ask', never the CLI `remember' verb: that verb fails
`remember_not_admitted' when the text does not route to a write, and the
conversational path through `ask' is the one that scopes correctly.

What it now does is propose, not write.  Under `orgbrain#95' a
conversational `remember that X' captures the dialogue and creates a
candidate in `pending_confirmation'; accepted knowledge is unchanged until
the owner approves that exact candidate, which is `+orgbrain/approve'.

The `remember that ' prefix is added unless the text already supplies what
is to be preserved.  The case that must be left alone is `Remember this'
with a resolved `--reply-to-turn-id': that is the daemon\='s designed path
for preserving the turn replied to, and prefixing it would turn it into
ordinary prose read as a statement.

Everything else is prefixed, including text that merely opens with a
preservation verb -- `remember why we chose sequential execution' is a
recall to the daemon, and left alone it would answer and propose nothing
while the owner sat in `propose' mode.  The prefixed form can read
awkwardly, but the receipt prints the exact proposal and its planned
operations before anything is admitted, so an awkward extraction is caught
at the gate.  A silent no-op is not caught anywhere."
  (unless (and (eq +orgbrain--conversation-support 'yes)
               (+orgbrain--conversation-id-valid-p +orgbrain--conversation))
    (user-error "orgbrain: propose requires conversational memory; input kept. Reconnect to an updated daemon"))
  (+orgbrain--build-ask
   (if (string-match-p +orgbrain--remember-supplied text)
       text
     (concat "remember that " text))
   project))

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
                      (+orgbrain--entity-args project)
                      ;; `consult' rides the `ask' parser, so it takes the
                      ;; conversation flags too -- and a five-minute answer is
                      ;; the last one worth losing from the dialogue.
                      (+orgbrain--conversation-args))
        :stdin text))

(defun +orgbrain--build-confirm (candidate admit)
  "Build the approval call for CANDIDATE, admitting it when ADMIT is non-nil.

The three `--confirm-candidate-*' fields are sent structurally, straight
from the receipt, so the owner never yanks a 64-hex hash by hand.  The
daemon refuses every shortcut around them -- an ID-only \"yes\", a stale
hash, a changed replacement target -- so this automates the typing and
nothing else.

The confirmation text is sent as the request body as well, and not because
belt and braces are pretty: `validate_request' calls `validate_text'
before any confirm flag is read, and rejects an empty body with
`empty_conversation_text'.  An `ask \"\"' carrying only the structured
fields never reaches the confirmation path at all.

`--conversation-id' is the conversation the proposal was made in.  A
different one is rejected as `confirmation_scope_mismatch', so it is
carried on the candidate rather than read from the current workspace."
  (let ((text (format "%s %s %s v%s"
                      (if admit "confirm" "reject")
                      (plist-get candidate :id)
                      (plist-get candidate :hash)
                      (plist-get candidate :version))))
    (list :args (append (list "ask" text "--json")
                        (+orgbrain--entity-args (plist-get candidate :entity))
                        (list "--conversation-id"
                              (plist-get candidate :conversation))
                        (when admit
                          (list "--confirm-candidate-id" (plist-get candidate :id)
                                "--confirm-candidate-hash" (plist-get candidate :hash)
                                "--confirm-candidate-version"
                                (format "%s" (plist-get candidate :version)))))
          :stdin nil)))

(defvar +orgbrain-modes
  '((ask
     :label "ask"
     :builder +orgbrain--build-ask
     :hint "compose an answer from the brain")
    (propose
     :label "propose"
     :builder +orgbrain--build-propose
     :hint "propose a fact; approval is a separate step")
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
  "Return the exchange plist for job RECORD.
The turn IDs are carried because they are the only thing a reply target
may be built from: the daemon refuses to resolve `Remember this' against
\"the latest visible message\" and returns a clarification instead, so
approximating the target here would defeat that guard on purpose."
  (let ((conversation (+orgbrain--conversation-receipt
                       (+orgbrain--dig record "result"))))
    (list :id (+orgbrain--truthy (+orgbrain--dig record "id"))
          :kind (or (+orgbrain--truthy (+orgbrain--dig record "kind")) "ask")
          :state (or (+orgbrain--truthy (+orgbrain--dig record "state")) "unknown")
          :entity (+orgbrain--truthy (+orgbrain--dig record "request" "entity"))
          :created (+orgbrain--truthy (+orgbrain--dig record "created_at"))
          :sent (+orgbrain--record-sent-text record)
          :conversation (+orgbrain--truthy
                         (+orgbrain--dig conversation "conversation_id"))
          :user-turn (+orgbrain--truthy
                      (+orgbrain--dig conversation "user_turn_id"))
          :assistant-turn (+orgbrain--truthy
                           (+orgbrain--dig conversation "assistant_turn_id"))
          :record record)))

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

(defun +orgbrain--consult-rows (result)
  "Return the ledger entry numbers in RESULT that came from a consult.
`origin' is what distinguishes a consultant's take from a brain fact, and
both are ordinary citable rows, so without this the transcript cannot say
which half of a composite answer came from off the host."
  (delq nil
        (mapcar (lambda (entry)
                  (and (equal (+orgbrain--dig entry "origin") "consult")
                       (+orgbrain--truthy (+orgbrain--dig entry "n"))))
                (or (+orgbrain--dig result "evidence_ledger" "entries") nil))))

(defun +orgbrain--format-citations (result)
  "Return the citation list of RESULT, marking any row that came from a consult."
  (let* ((cites (or (+orgbrain--truthy (+orgbrain--dig result "citations"))
                    (+orgbrain--truthy
                     (+orgbrain--dig result "answer_receipt" "citations"))))
         (consulted (seq-intersection (and (listp cites) cites)
                                      (+orgbrain--consult-rows result))))
    (cond
     ((not (consp cites)) (+orgbrain--format-count cites))
     (consulted (format "%s  (%s from consult)"
                        cites
                        (mapconcat #'number-to-string consulted ", ")))
     (t (format "%s" cites)))))

(defun +orgbrain--format-consults (receipt)
  "Return the `consults' summary line of RECEIPT, or nil when none ran.
Printed only when a consult was requested: on an ordinary ask the block is
all zeros and would be noise.  Without it the transcript gives no sign that
a consult fired at all, which makes the mode unverifiable from the buffer."
  (let ((consults (+orgbrain--dig receipt "consults")))
    (when (and (consp consults)
               (> (or (+orgbrain--truthy (+orgbrain--dig consults "requested")) 0) 0))
      (format "consults:        ran %s  refused %s  failed %s  trigger %s%s"
              (+orgbrain--format-count (+orgbrain--dig consults "ran"))
              (+orgbrain--format-count (+orgbrain--dig consults "refused"))
              (+orgbrain--format-count (+orgbrain--dig consults "failed"))
              (+orgbrain--format-count (+orgbrain--dig consults "trigger"))
              ;; A take that did not reach GBrain lives for this job only.
              ;; Silence here would make an evaporating take look persisted.
              (let ((problem (+orgbrain--truthy
                              (+orgbrain--dig consults "persist_error"))))
                (if problem "\n                 NOT PERSISTED (take is job-local)" ""))))))

(defun +orgbrain--format-receipt (result)
  "Return the plainly formatted receipt lines for a job RESULT."
  (let ((receipt (+orgbrain--dig result "answer_receipt")))
    (string-join
     (delq nil
     (list
      "-- receipt --"
      (format "grounding:       %s"
              (+orgbrain--format-count (+orgbrain--dig receipt "grounding")))
      (format "citations:       %s" (+orgbrain--format-citations result))
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
              (+orgbrain--format-count (+orgbrain--dig receipt "failure_class")))
      (+orgbrain--format-consults receipt)))
     "\n")))

(defun +orgbrain--capture-summary (capture)
  "Return the one-line summary of the CAPTURE block of a conversation receipt.
The block is either `{\"status\": \"disabled\"}' -- capture off, so nothing
was preserved however good the conversation ID was -- or a map of role to
that role\\='s own outcome.  A missing `assistant' key is not an omission: a
failed composition captures the user turn and never reaches an assistant
one, and saying so is the difference between a normal outcome and what
reads as a client bug."
  (cond
   ((not (consp capture)) "unknown")
   ((equal (+orgbrain--dig capture "status") "disabled")
    "disabled -- the daemon is not retaining dialogue (ORGBRAIN_CONVERSATION_RETENTION)")
   (t
    (mapconcat
     (lambda (cell)
       (let ((role (car cell))
             (problem (+orgbrain--truthy (+orgbrain--dig (cdr cell) "error"))))
         (format "%s %s%s" role
                 (+orgbrain--format-count (+orgbrain--dig (cdr cell) "status"))
                 (if problem (format " (%s)" problem) ""))))
     capture "  "))))

(defun +orgbrain--format-planned (planned)
  "Return the planned knowledge operations PLANNED, one per line, or nil."
  (when (consp planned)
    (mapconcat
     (lambda (op)
       (format "                 %s %s -- %s"
               (+orgbrain--format-count (+orgbrain--dig op "op"))
               (+orgbrain--format-count (+orgbrain--dig op "entity"))
               (+orgbrain--format-count (+orgbrain--dig op "fact"))))
     planned "\n")))

(defun +orgbrain--format-knowledge (knowledge)
  "Return the knowledge lines of a conversation receipt block KNOWLEDGE.
The proposal statement and its planned operations are printed in full for
`pending_confirmation', because inspecting exactly what is about to be
admitted is the entire point of the approval gate."
  (let ((status (+orgbrain--dig knowledge "status")))
    (delq nil
          (list
           (format "knowledge:       %s%s"
                   (+orgbrain--format-count status)
                   (let ((reason (+orgbrain--truthy
                                  (+orgbrain--dig knowledge "reason"))))
                     (if reason (format "  (%s)" reason) "")))
           (let ((proposal (+orgbrain--truthy
                            (+orgbrain--dig knowledge "proposal"))))
             (when proposal (format "  proposal:      %s" proposal)))
           (+orgbrain--format-planned
            (+orgbrain--truthy (+orgbrain--dig knowledge "planned")))
           (when (equal status "pending_confirmation")
             ;; Not the 64-hex confirmation string: yanking that by hand is
             ;; the tax this client exists to remove.  `+orgbrain/approve'
             ;; sends the three structured fields verbatim instead.
             "  approve:       M-x +orgbrain/approve (gy) -- nothing is admitted until you do")))))

(defun +orgbrain--format-conversation (result)
  "Return the conversation block of RESULT, or nil when it is single-turn.

Capture, knowledge, answer and delivery succeed or fail independently, so
all four are printed.  The case that most needs it is a failed
composition: capture and the proposal are decided and journaled before
COMPOSE runs, so an answer reading `I could not compose an answer' can sit
above a perfectly good, verified memory outcome."
  (let ((receipt (+orgbrain--conversation-receipt result)))
    (when (consp receipt)
      (string-join
       (delq nil
             (append
              (list
               "-- conversation --"
               (format "id:              %s"
                       (+orgbrain--format-count
                        (+orgbrain--dig receipt "conversation_id")))
               (format "capture:         %s"
                       (+orgbrain--capture-summary
                        (+orgbrain--dig receipt "capture"))))
              (+orgbrain--format-knowledge
               (or (+orgbrain--truthy (+orgbrain--dig receipt "knowledge"))
                   (+orgbrain--truthy (+orgbrain--dig result "memory_receipt"))))
              (list
               (format "answer:          %s%s"
                       (+orgbrain--format-count
                        (+orgbrain--dig receipt "answer" "status"))
                       (let ((why (+orgbrain--truthy
                                   (+orgbrain--dig result "answer_receipt"
                                                   "answer_failure"))))
                         (if why (format "  (%s)" why) "")))
               (format "delivery:        %s"
                       (+orgbrain--format-count
                        (+orgbrain--dig receipt "delivery" "status")))
               (let ((user (+orgbrain--truthy
                            (+orgbrain--dig receipt "user_turn_id")))
                     (assistant (+orgbrain--truthy
                                 (+orgbrain--dig receipt "assistant_turn_id"))))
                 (when (or user assistant)
                   ;; Printed so `+orgbrain/set-reply-target' has something
                   ;; visible to name, and so a reply target can be checked
                   ;; against the transcript rather than taken on trust.
                   (format "turns:           user %s  assistant %s"
                           (+orgbrain--format-count user)
                           (+orgbrain--format-count assistant)))))))
       "\n"))))

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
                 (+orgbrain--format-conversation result)
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
    (define-key map (kbd "C-c C-n") #'+orgbrain/new-conversation)
    (define-key map (kbd "C-c C-y") #'+orgbrain/approve)
    (define-key map (kbd "C-c C-r") #'+orgbrain/set-reply-target)
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
  ;; An answer is one long paragraph, so soft-wrap it and make the motion
  ;; keys agree with what is on screen -- see `+orgbrain-input-mode'.
  (visual-line-mode 1)
  ;; `global-so-long-mode' would strip font-lock from a single long answer.
  (when (boundp 'so-long-threshold)
    (set (make-local-variable 'so-long-threshold) nil)))

(defvar +orgbrain-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+orgbrain/send)
    (define-key map (kbd "C-c C-p") #'+orgbrain/switch-project)
    (define-key map (kbd "C-c C-n") #'+orgbrain/new-conversation)
    (define-key map (kbd "C-c C-y") #'+orgbrain/approve)
    (define-key map (kbd "C-c C-r") #'+orgbrain/set-reply-target)
    map)
  "Keymap for `+orgbrain-input-mode'.")

(define-derived-mode +orgbrain-input-mode text-mode "OrgBrain-Input"
  "Mode for the editable OrgBrain input buffer.

`visual-line-mode' because a brief is prose, not code.  A pasted brief is
one logical line that wraps over twenty screen lines, and Evil\='s `j\=' and
`k\=' are linewise -- so on that buffer they leap the whole paragraph and
land at its start or end, which is what makes the pane feel like it has no
motion at all.  `init-git-ui.el\='s review buffers already bind `j\='/`k\=' to
the visual variants for exactly this reason; this module simply had not
followed the convention.

With the mode on, Emacs\=' own `C-a\='/`C-e\='/`C-k\=' become visual too, which
is the half of the request that is not vim: the pane should behave like any
other Emacs buffer you type prose into."
  :interactive nil
  :group 'orgbrain
  (setq truncate-lines nil)
  (visual-line-mode 1))

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

(defun +orgbrain--short-id (id)
  "Return the first eight characters of ID, enough to match against a receipt."
  (if (and (stringp id) (> (length id) 8)) (substring id 0 8) (or id "")))

(defun +orgbrain--header-line (kind)
  "Return the header line for the KIND pane, `output' or `input'.
Each pane is labelled, because the two are otherwise indistinguishable
at a glance -- issue #6's own sketch labels them OUTPUT and INPUT.  Only
the input pane carries `mode\=': the mode decides what a send does, and a
send is issued from there, so showing it on the transcript said nothing."
  (concat
   (pcase kind ('output "OUTPUT") ('input "INPUT"))
   (format "  |  project: %s" (or +orgbrain--project "unscoped"))
   ;; The transcript pane carries the conversation identity and whether the
   ;; daemon is retaining anything; the input pane carries what a send will
   ;; do with it.  Splitting them keeps either header readable.
   (when (eq kind 'output) (+orgbrain--conversation-note))
   (when (eq kind 'input)
     (concat
      (format "  |  mode: %s" (+orgbrain--mode-label +orgbrain--mode))
      (when +orgbrain--capture-discussion "  |  +capture")
      (when +orgbrain--reply-target
        (format "  |  reply->%s" (+orgbrain--short-id +orgbrain--reply-target)))
      (when +orgbrain--candidate "  |  proposal pending")))
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
  ;; Probed once per workspace, next to the project list and for the same
  ;; reason: the feature may not be on the daemon, and finding out at send
  ;; time costs whatever brief was just typed.
  (when (eq +orgbrain--conversation-support 'unknown)
    (+orgbrain--probe-conversation-support))
  (+orgbrain--ensure-conversation)
  (+orgbrain--display-workspace)
  (+orgbrain--refresh-header)
  (message "orgbrain: %s, mode %s, %s.  TAB cycles mode, RET sends"
           (or +orgbrain--project "unscoped")
           (+orgbrain--mode-label +orgbrain--mode)
           (if (eq +orgbrain--conversation-support 'yes)
               (format "conversation %s" +orgbrain--conversation)
             "single-turn (this daemon has no conversational memory)")))

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
    ;; The daemon binds a conversation ID to owner + transport + project, so
    ;; carrying one across a switch makes the next turn a different scope
    ;; under the same ID -- confusing history, and cross-scope rejections.
    ;; Starting a fresh one is the honest half of the choice: refusing the
    ;; switch would trap the workspace in whichever project it happened to
    ;; open in.
    (let ((previous +orgbrain--conversation))
      (+orgbrain--ensure-conversation)
      (+orgbrain--refresh-header)
      (message "orgbrain project: %s (project list from %s)%s"
               (or +orgbrain--project "unscoped") source
               (if (equal previous +orgbrain--conversation)
                   ""
                 (format "; new conversation %s" +orgbrain--conversation))))))

(defconst +orgbrain--slug-rule
  "A project slug is one path segment: no slash, no leading or trailing space."
  "Mirror of `orgbrain'\''s own rule (`project/knowledge.el'\''s `_slug').
Checked here so a typo costs no round trip and no `project_slug_invalid'.")

(defun +orgbrain--check-slug (slug)
  "Signal a `user-error' unless SLUG is a usable project slug."
  (let ((clean (or slug "")))
    (when (string-empty-p (string-trim clean))
      (user-error "orgbrain: a project needs a slug.  %s" +orgbrain--slug-rule))
    (unless (equal clean (string-trim clean))
      (user-error "orgbrain: %s" +orgbrain--slug-rule))
    (when (string-match-p "/" clean)
      (user-error "orgbrain: %s" +orgbrain--slug-rule))
    clean))

(defun +orgbrain/new-project ()
  "Create a project on the daemon and switch the workspace to it.

`orgbrain project new' is not one of `+orgbrain-modes'\'' request modes on
purpose: creating a project happens once, and a destructive verb should
not sit on the TAB cycle where a stray keystroke reaches it.

The summary is not decoration.  `project_page_ops' captures a page *and*
writes one entity-scoped fact built from it, so until you remember
anything else that sentence is the only thing the project knows, and the
only thing an ask can cite.

This is a GBrain write, so it is refused while a request is outstanding:
`assert_service_idle' refuses rather than queues, and what a refusal
costs is whatever the owner just typed."
  (interactive)
  (when +orgbrain--pending
    (user-error "orgbrain: %s in flight; a write is refused while a job runs"
                (plist-get +orgbrain--pending :label)))
  (let* ((slug (+orgbrain--check-slug
                (read-string "New project slug (one segment, lowercase): ")))
         (title (string-trim (read-string (format "Title for %s: " slug) slug)))
         (summary (string-trim
                   (read-string "One-sentence summary (becomes the first fact): "))))
    (when (string-empty-p title)
      (user-error "orgbrain: a project needs a title"))
    (when (string-empty-p summary)
      (unless (yes-or-no-p
               "No summary means the project's only fact is its title.  Continue? ")
        (user-error "orgbrain: cancelled")))
    (+orgbrain--set-status 'working)
    (message "orgbrain: creating %s..." slug)
    (+orgbrain--cli
     (append (list "project" "new" slug "--title" title)
             (unless (string-empty-p summary) (list "--summary" summary))
             (list "--json"))
     nil
     (lambda (stdout problem)
       (if problem
           (progn (+orgbrain--set-status 'error)
                  (message "orgbrain: %s" problem))
         ;; Switch scope only once the daemon has confirmed the write, so a
         ;; failed create cannot leave the workspace pointed at a project
         ;; that does not exist.
         (setq +orgbrain--project slug
               +orgbrain--projects-source 'server
               +orgbrain--exchanges nil
               +orgbrain--exchanges-project nil
               +orgbrain--exchange-index nil)
         (+orgbrain--set-status 'idle)
         (+orgbrain--append
          (format "=== project new  projects/%s  ===\n> %s\n\n%s\n\n"
                  slug title (string-trim (or stdout ""))))
         (message "orgbrain: %s created and selected.  TAB to remember, then seed it"
                  slug))))))

(defun +orgbrain--absorb-conversation (record)
  "Update the workspace from the conversation receipt in job RECORD.

Three things are learned from an answer and from nowhere else: whether the
daemon actually retained the turn, which turn IDs this exchange got, and
whether a knowledge proposal is now waiting for approval."
  (let* ((result (+orgbrain--dig record "result"))
         (receipt (+orgbrain--conversation-receipt result))
         (capture (+orgbrain--dig receipt "capture"))
         (knowledge (or (+orgbrain--truthy (+orgbrain--dig receipt "knowledge"))
                        (+orgbrain--truthy
                         (+orgbrain--dig result "memory_receipt")))))
    (when (consp receipt)
      (setq +orgbrain--capture-state
            (if (equal (+orgbrain--dig capture "status") "disabled")
                'disabled
              'on))
      ;; Consumed, not sticky: a reply target names one earlier turn for one
      ;; send, and a stale one would silently re-aim the next brief.
      (setq +orgbrain--reply-target nil)
      (setq +orgbrain--candidate
            (cond
             ((equal (+orgbrain--dig knowledge "status") "pending_confirmation")
              (list :id (+orgbrain--dig knowledge "candidate_id")
                    :hash (+orgbrain--dig knowledge "candidate_hash")
                    :version (or (+orgbrain--truthy
                                  (+orgbrain--dig knowledge "candidate_version"))
                                 1)
                    :proposal (+orgbrain--truthy
                               (+orgbrain--dig knowledge "proposal"))
                    :planned (+orgbrain--truthy
                              (+orgbrain--dig knowledge "planned"))
                    ;; The conversation and entity the proposal was made in,
                    ;; not whatever the workspace is pointing at by the time
                    ;; approval happens: a different conversation is rejected
                    ;; as `confirmation_scope_mismatch'.
                    :conversation (or (+orgbrain--truthy
                                       (+orgbrain--dig receipt "conversation_id"))
                                      +orgbrain--conversation)
                    :entity (+orgbrain--strip-project-prefix
                             (+orgbrain--truthy
                              (+orgbrain--dig record "request" "entity")))))
             ((and (equal (+orgbrain--dig knowledge "candidate_id")
                          (plist-get +orgbrain--candidate :id))
                   (or (equal (+orgbrain--dig knowledge "status") "verified")
                       (equal (+orgbrain--dig knowledge "reason") "owner_rejected")))
              nil)
             (t +orgbrain--candidate))))))

(defun +orgbrain--propose-warning (record)
  "Return the line saying a `propose' send proposed nothing, or nil.

The mirror in `+orgbrain--remember-supplied' decides whether to add a
prefix; this decides nothing and only reports what came back.  That is the
half that cannot drift: when the daemon\='s classifier moves again -- it
has moved once already -- the mirror will be wrong and this will still be
right.

`unchanged' is the daemon saying it read the turn as a read, not a
preservation request.  Inside the receipt it is one undifferentiated line
among nine, which is no way to learn that the mode the owner deliberately
selected did not do its job.  `clarification' is not warned about: the
daemon puts its own question in the answer text, where it is already
visible."
  (let* ((result (+orgbrain--dig record "result"))
         (receipt (+orgbrain--conversation-receipt result))
         (status (+orgbrain--dig
                  (or (+orgbrain--truthy (+orgbrain--dig receipt "knowledge"))
                      (+orgbrain--truthy (+orgbrain--dig result "memory_receipt")))
                  "status")))
    (cond
     ((not (consp receipt))
      (format "propose: single-turn response without a conversation receipt; knowledge status: %s. Inspect the outcome before resubmitting." (or status "unknown")))
     ((equal status "unchanged")
      "propose: the daemon did not read this as a preservation request, so nothing was proposed.  It needs the turn to supply what is preserved -- a statement to remember, or `this'/`that' with a reply target armed (`gr').")
     (t nil))))

(defun +orgbrain--note-unsupported-conversation (problem)
  "Downgrade conversation support when PROBLEM is the daemon rejecting the flag.
The `--help' probe can be out of date -- the host moved, or the daemon was
rolled back mid-session -- and argparse\\='s `unrecognized arguments' is the
only signal that happens.  Caught here so the next send degrades to a
single-turn ask instead of failing the same way forever."
  (when (and (stringp problem)
             (string-match-p "unrecognized arguments" problem)
             (string-match-p "--conversation-id\\|--confirm-candidate\\|--reply-to-turn-id\\|--capture-discussion"
                             problem))
    (setq +orgbrain--conversation-support 'no)
    (message "orgbrain: this daemon has no conversational memory; falling back to single-turn asks")
    t))

(defun +orgbrain--finish-send (label text stdout problem &optional expect-proposal)
  "Render the reply to a LABEL request of TEXT, or report PROBLEM.
STDOUT is the raw CLI output when the call succeeded.  LABEL rather than a
mode symbol: approvals are not one of `+orgbrain-modes' -- they are not on
the TAB cycle by design -- but they land in the transcript the same way.
With EXPECT-PROPOSAL non-nil, a reply that proposed nothing is called out
rather than left as one line inside the receipt block."
  (+orgbrain--stop-tick)
  (setq +orgbrain--pending nil
        +orgbrain--exchanges nil
        +orgbrain--exchanges-project nil
        +orgbrain--exchange-index nil)
  (let* ((parsed (+orgbrain--parse-json stdout))
         (record (and (consp parsed)
                      (or (null problem)
                          (and (+orgbrain--dig parsed "id")
                               (member (+orgbrain--dig parsed "state")
                                       '("failed" "succeeded"))))
                      parsed)))
    (cond
     ((and problem (null record))
      (+orgbrain--set-status 'error)
      (unless (+orgbrain--note-unsupported-conversation problem)
        (message "orgbrain: %s" problem)))
     ((null record)
      (+orgbrain--set-status 'error)
      (message "orgbrain: could not parse the %s receipt; see %s"
               label (+orgbrain--buffer-name 'output))
      (+orgbrain--append (format "=== %s  unparseable receipt ===\n%s\n\n"
                                 label (string-trim (or stdout "")))))
     (t
      ;; A fresh receipt echoes no request, so keep what was actually sent.
      (let ((exchange (plist-put (+orgbrain--record-exchange record) :sent text))
            (warning (and expect-proposal (+orgbrain--propose-warning record)))
            (failed (or problem (equal (+orgbrain--dig record "state") "failed"))))
        (+orgbrain--absorb-conversation record)
        (+orgbrain--set-status (if failed 'error 'idle))
        (+orgbrain--append (+orgbrain--format-exchange exchange label))
        (when warning (+orgbrain--append (concat "!! " warning "\n\n")))
        ;; Clear the brief only once the transcript holds it, so a failed
        ;; send never costs the owner the thought they typed.
        (unless failed (+orgbrain--set-input ""))
        (message "orgbrain %s: %s in %s ms%s" label
                 (if failed "failed; receipt shown, input kept" "done")
                 (+orgbrain--format-count
                  (+orgbrain--dig record "result" "answer_receipt"
                                  "latency_ms"))
                 (cond
                  (+orgbrain--candidate
                   ".  A proposal is waiting: `gy' approves it, `gN' rejects it")
                  ;; Said in the echo area as well as the transcript: the
                  ;; whole failure mode is that it goes unnoticed.
                  (warning (concat ".  " warning))
                  (t ""))))))))

(defun +orgbrain--assert-idle ()
  "Signal a `user-error' when a request is genuinely still outstanding.
A flag can outlive the request it describes -- a reloaded module, or a
build that left it set after a signal -- and then the callback that would
clear it never runs, so obeying it refuses every later send forever.  A
flag is judged stale when it records a process that has died, or records
no `:process' key at all (the shape older state has).  A flag that records
nil is left alone: a transport that returns no process cannot be
second-guessed, and `+orgbrain/reset' is the hatch."
  (when +orgbrain--pending
    (if (and (plist-member +orgbrain--pending :process)
             (let ((proc (plist-get +orgbrain--pending :process)))
               (or (null proc) (process-live-p proc))))
        (user-error "orgbrain: %s still in flight; send is disabled until it returns (`M-x +orgbrain/reset' if it is not)"
                    (plist-get +orgbrain--pending :label))
      (setq +orgbrain--pending nil))))

(defun +orgbrain--dispatch (label text request wait &optional expect-proposal)
  "Send REQUEST, rendering the reply under LABEL and echoing WAIT while it runs.
TEXT is what the owner sent, kept because a fresh receipt echoes no
request.  Shared by `+orgbrain/send' and by the approval commands, which
are deliberately not modes: a destructive verb should not sit on the TAB
cycle where a stray keystroke reaches it.  EXPECT-PROPOSAL is passed
through to `+orgbrain--finish-send'."
  (setq +orgbrain--pending
        (list :label label :started (float-time) :process nil))
  (+orgbrain--set-status 'working)
  (+orgbrain--start-tick)
  (message "orgbrain %s: sent to %s, waiting (%s)" label
           (if (eq +orgbrain-transport #'+orgbrain--transport-local)
               "this host"
             +orgbrain-ssh-host)
           wait)
  ;; If the dispatch signals, `+orgbrain--pending' must not survive it:
  ;; a stuck pending refuses every later send until Emacs restarts.
  (condition-case signalled
      (let ((proc (+orgbrain--cli
                   (plist-get request :args)
                   (plist-get request :stdin)
                   (lambda (stdout problem)
                     (+orgbrain--finish-send label text stdout problem
                                             expect-proposal)))))
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
     (signal (car signalled) (cdr signalled)))))

(defun +orgbrain/send ()
  "Send the input buffer through the current request mode.
Refuses while another request is in flight: the daemon rejects a write
while any job is running, and losing a thought is worse than waiting."
  (interactive)
  (+orgbrain--assert-idle)
  (let ((text (+orgbrain--input-text))
        (mode +orgbrain--mode))
    (when (string-empty-p text)
      (user-error "orgbrain: the input buffer is empty"))
    ;; `recall' is not a conversation verb -- its parser has no
    ;; `--conversation-id' -- so it neither starts nor needs one.
    (unless (eq mode 'recall) (+orgbrain--ensure-conversation))
    (+orgbrain--dispatch (+orgbrain--mode-label mode) text
                         (+orgbrain--build-request mode text +orgbrain--project)
                         (if (eq mode 'consult)
                             "a consulted ask takes ~5 min"
                           "an ask takes 19-25s")
                         (eq mode 'propose))))

(defun +orgbrain/new-conversation ()
  "Start a fresh conversation, so later turns do not cohere with earlier ones.
The conversation ID is what makes turns cohere on the daemon, which makes
both extremes wrong: reusing one forever glues unrelated work together,
and a new one per send is the single-turn behaviour this replaces."
  (interactive)
  (setq +orgbrain--conversation (+orgbrain--new-conversation-id +orgbrain--project)
        +orgbrain--conversation-project +orgbrain--project
        +orgbrain--capture-state 'unknown
        +orgbrain--reply-target nil
        +orgbrain--candidate nil)
  (+orgbrain--refresh-header)
  (message "orgbrain: new conversation %s" +orgbrain--conversation))

(defun +orgbrain/toggle-capture ()
  "Toggle `--capture-discussion' on the next sends.
With retention off on the daemon an ordinary question preserves nothing.
This asks for the turn to be kept anyway.  The daemon still needs a
dedicated conversation source configured, and refuses the send with
`conversation_source_required' when it has none."
  (interactive)
  (setq +orgbrain--capture-discussion (not +orgbrain--capture-discussion))
  (+orgbrain--refresh-header)
  (message "orgbrain: capture-discussion %s"
           (if +orgbrain--capture-discussion "on" "off")))

(defun +orgbrain/set-reply-target (&optional user-turn)
  "Aim the next send at the exchange currently replayed by `M-p'/`M-n'.

This is what lets `propose' say `Remember this' about a specific earlier
exchange.  The daemon explicitly refuses to guess: `Remember this' with no
resolved target returns one clarification and commits nothing.  So this
refuses too, rather than falling back to the newest exchange -- emulating
the guess is the behaviour the server is designed to prevent.

An exchange holds two turns.  The default target is the assistant\='s, which
is what \"remember this\" about a replayed answer normally means; with a
prefix argument, USER-TURN, it is the question instead.  Stated rather than
inferred, and echoed on the way out, because which turn is preserved is
the whole content of the effect."
  (interactive "P")
  (let ((exchange (and +orgbrain--exchange-index
                       (nth +orgbrain--exchange-index +orgbrain--exchanges))))
    (unless exchange
      (user-error "orgbrain: replay an exchange with `M-p' first; a reply target is never guessed"))
    (let ((turn (if user-turn
                    (plist-get exchange :user-turn)
                  (or (plist-get exchange :assistant-turn)
                      (plist-get exchange :user-turn)))))
      (unless turn
        (user-error "orgbrain: that exchange has no captured turn to reply to (it predates conversational memory, or capture was off)"))
      (unless (equal (plist-get exchange :conversation) +orgbrain--conversation)
        (user-error "orgbrain: that turn belongs to conversation %s, not %s; `gn' starts a new one but cannot move a turn between them"
                    (or (plist-get exchange :conversation) "none")
                    (or +orgbrain--conversation "none")))
      (setq +orgbrain--reply-target turn)
      (+orgbrain--refresh-header)
      (message "orgbrain: next send replies to the %s turn %s"
               (if (equal turn (plist-get exchange :user-turn)) "user" "assistant")
               (+orgbrain--short-id turn)))))

(defun +orgbrain--describe-candidate (candidate)
  "Return the text shown before approving CANDIDATE."
  (concat (format "Proposal: %s\n" (or (plist-get candidate :proposal) "(none stated)"))
          (let ((planned (+orgbrain--format-planned (plist-get candidate :planned))))
            (if planned (concat "Planned:\n" planned "\n") ""))
          (format "Candidate %s v%s in conversation %s"
                  (plist-get candidate :id)
                  (plist-get candidate :version)
                  (plist-get candidate :conversation))))

(defun +orgbrain--decide-candidate (admit)
  "Approve the pending proposal when ADMIT is non-nil, otherwise reject it.
The proposal statement and its planned operations are shown first: seeing
exactly what is about to be admitted is the entire point of the gate."
  (+orgbrain--assert-idle)
  (let ((candidate +orgbrain--candidate))
    (unless candidate
      (user-error "orgbrain: no proposal is waiting"))
    (unless (and (plist-get candidate :id) (plist-get candidate :hash))
      (user-error "orgbrain: the proposal receipt carried no candidate id and hash"))
    (+orgbrain--append (format "=== proposal ===\n%s\n\n"
                               (+orgbrain--describe-candidate candidate)))
    (unless (yes-or-no-p (format "%s this proposal: %s? "
                                 (if admit "Approve" "Reject")
                                 (or (plist-get candidate :proposal)
                                     (plist-get candidate :id))))
      (user-error "orgbrain: left the proposal pending"))
    (let ((request (+orgbrain--build-confirm candidate admit)))
      ;; Retain the exact candidate until a receipt verifies its disposition.
      ;; A failed submission or lost reply is not an acknowledgement. Server
      ;; confirmation rechecks and replay guards remain authoritative.
      ;; The body, not the last argument: `nth 1' is the `ask' positional,
      ;; which is the confirmation text the daemon actually matched.
      (+orgbrain--dispatch (if admit "approve" "reject")
                           (nth 1 (plist-get request :args))
                           request
                           "an approval is an ordinary ask, 19-25s"))))

(defun +orgbrain/approve ()
  "Approve the knowledge proposal from the last answer.
Sends the candidate ID, hash, and version structurally, so the owner never
yanks the 64-hex confirmation string out of the transcript by hand.  That
is the bandwidth argument, not a nicety: the daemon deliberately refuses
every shortcut around those three fields, so the typing can only be
automated, never simplified away."
  (interactive)
  (+orgbrain--decide-candidate t))

(defun +orgbrain/reject-proposal ()
  "Reject the knowledge proposal from the last answer, admitting nothing."
  (interactive)
  (+orgbrain--decide-candidate nil))

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
        +orgbrain--exchange-index nil
        ;; An armed reply target and a pending proposal both describe a
        ;; conversation the reset is walking away from.  Re-probing is the
        ;; point of the hatch when the daemon is what changed.
        +orgbrain--reply-target nil
        +orgbrain--candidate nil
        +orgbrain--capture-state 'unknown
        +orgbrain--conversation-support 'unknown)
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

(defun +orgbrain--replay-would-discard-p ()
  "Non-nil when replaying would throw away unsent input.

Replay overwrites the input buffer.  Everything else in this module goes
out of its way not to cost the owner a thought -- a failed send keeps the
brief, a successful one clears it only once the transcript holds it -- and
replay was the exception.  Text already put there by a replay is not a
thought, so walking the dialogue does not nag."
  (let ((text (+orgbrain--input-text)))
    (and (not (string-empty-p text))
         (let ((current (and +orgbrain--exchange-index
                            (nth +orgbrain--exchange-index +orgbrain--exchanges))))
           (not (equal text (string-trim (or (plist-get current :sent) ""))))))))

(defun +orgbrain--confirm-replay ()
  "Signal a `user-error' unless replay may overwrite the input buffer."
  (when (+orgbrain--replay-would-discard-p)
    (unless (yes-or-no-p "Replace the unsent brief with this exchange? ")
      (user-error "orgbrain: kept the brief"))))

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
  (+orgbrain--confirm-replay)
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
  (+orgbrain--confirm-replay)
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
    (evil-ex-define-cmd "orgbrain-reset" #'+orgbrain/reset)
    (evil-ex-define-cmd "orgbrain-new-project" #'+orgbrain/new-project)
    (evil-ex-define-cmd "orgbrain-new-conversation" #'+orgbrain/new-conversation)
    (evil-ex-define-cmd "orgbrain-approve" #'+orgbrain/approve)
    (evil-ex-define-cmd "orgbrain-reject" #'+orgbrain/reject-proposal)
    (evil-ex-define-cmd "orgbrain-reply-to" #'+orgbrain/set-reply-target)
    (evil-ex-define-cmd "orgbrain-capture" #'+orgbrain/toggle-capture))
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +orgbrain-mode-map
      ;; Visual-line motion, matching `init-git-ui.el'. `gj'/`gk' keep the
      ;; logical-line motions rather than losing them.
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line
      "gj" #'evil-next-line
      "gk" #'evil-previous-line
      "H" #'evil-beginning-of-line
      "L" #'evil-end-of-line
      (kbd "RET") #'+orgbrain/send
      (kbd "<up>") #'+orgbrain/previous-exchange
      (kbd "<down>") #'+orgbrain/next-exchange
      (kbd "M-p") #'+orgbrain/previous-exchange
      (kbd "M-n") #'+orgbrain/next-exchange
      "gp" #'+orgbrain/switch-project
      "gP" #'+orgbrain/new-project
      ;; The conversation verbs.  Approval is `gy'/`gN' rather than a mode:
      ;; TAB must not be able to reach a verb that admits knowledge.
      "gn" #'+orgbrain/new-conversation
      "gy" #'+orgbrain/approve
      "gN" #'+orgbrain/reject-proposal
      "gr" #'+orgbrain/set-reply-target
      "gc" #'+orgbrain/toggle-capture
      "q" #'+orgbrain/quit
      (kbd "C-h") #'windmove-left
      (kbd "C-l") #'windmove-right
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up)
    (evil-define-key 'normal +orgbrain-input-mode-map
      (kbd "TAB") #'+orgbrain/cycle-mode
      (kbd "<tab>") #'+orgbrain/cycle-mode
      ;; Visual-line motion, matching `init-git-ui.el'. `gj'/`gk' keep the
      ;; logical-line motions rather than losing them.
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line
      "gj" #'evil-next-line
      "gk" #'evil-previous-line
      "H" #'evil-beginning-of-line
      "L" #'evil-end-of-line
      (kbd "RET") #'+orgbrain/send
      ;; Arrows move point here.  In every other buffer in this config they
      ;; do, and this is the pane you type into -- an arrow that replaced the
      ;; buffer contents was the one destructive key on the board.  The
      ;; dialogue walk moves to `M-p'/`M-n', which is what Emacs uses for
      ;; input history everywhere else.
      (kbd "<up>") #'evil-previous-visual-line
      (kbd "<down>") #'evil-next-visual-line
      (kbd "M-p") #'+orgbrain/previous-exchange
      (kbd "M-n") #'+orgbrain/next-exchange
      "gp" #'+orgbrain/switch-project
      "gP" #'+orgbrain/new-project
      ;; The conversation verbs.  Approval is `gy'/`gN' rather than a mode:
      ;; TAB must not be able to reach a verb that admits knowledge.
      "gn" #'+orgbrain/new-conversation
      "gy" #'+orgbrain/approve
      "gN" #'+orgbrain/reject-proposal
      "gr" #'+orgbrain/set-reply-target
      "gc" #'+orgbrain/toggle-capture
      "q" #'+orgbrain/quit
      (kbd "C-h") #'windmove-left
      (kbd "C-l") #'windmove-right
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up)
    ;; The input buffer is edited in insert state, where an unbound C-j
    ;; would insert a newline instead of moving to the window below.
    (evil-define-key 'insert +orgbrain-input-mode-map
      (kbd "M-p") #'+orgbrain/previous-exchange
      (kbd "M-n") #'+orgbrain/next-exchange
      (kbd "C-h") #'windmove-left
      (kbd "C-l") #'windmove-right
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state '+orgbrain-mode 'normal)
    (evil-set-initial-state '+orgbrain-input-mode 'normal)))

(provide 'init-orgbrain)

;;; init-orgbrain.el ends here
