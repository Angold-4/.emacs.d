;;; init-tradeoffs-trace.el --- tradeoffs-trace: programmed Pi review pipeline -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ango Wang

;;; Commentary:
;;
;; Emacs front end for tradeoffs-trace (docs/tradeoffs-trace.md).  The
;; conductor is a detached Node daemon; Emacs only starts runs, renders the
;; run directory, and writes owner commands into the run's inbox.  Everything
;; shown here is rebuilt from `~/.tradeoffs-trace/<run>/' (`tt state'), so it
;; survives closing buffers or restarting Emacs.
;;
;; Keys extend the Pilish `C-c m' prefix:
;;
;;   C-c m r   in a plan (Org) buffer: validate, snapshot, start a run, open
;;             its workspace
;;   C-c m s   focus or rebuild a run's workspace (trace, status, input);
;;             offers to resume a run whose conductor is not running
;;   C-c m d   the run's decision view
;;
;; The conductor always comes from the *installed runner*
;; (`<root>/runner/current/tradeoffs-trace', see `tt runner install <sha>'),
;; never from the repository being edited, so a run that works on
;; tradeoffs-trace itself does not execute the code under review.
;;
;; The workspace polls the run directory every `+tt-refresh-interval'
;; seconds; the trace renders the most recently active agent's stream file.
;; An agent with no activity for `+tt-idle-minutes' while the phase is not
;; terminal is flagged IDLE in the status buffer.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'tab-bar)

(defgroup +tt nil
  "tradeoffs-trace front end."
  :group 'tools)

(defcustom +tt-root (expand-file-name "~/.tradeoffs-trace/")
  "Directory holding tradeoffs-trace runs and installed runners."
  :type 'directory)

(defcustom +tt-runner nil
  "Package directory of the runner to launch.
Nil means `<+tt-root>/runner/current/tradeoffs-trace'."
  :type '(choice (const nil) directory))

(defcustom +tt-node "node"
  "Node executable used to run the tradeoffs-trace CLI."
  :type 'string)

(defcustom +tt-refresh-interval 2
  "Seconds between workspace refreshes."
  :type 'number)

(defcustom +tt-idle-minutes 5
  "Minutes without agent activity after which a running phase is flagged idle."
  :type 'number)

(defconst +tt--terminal-phases '("DONE" "BLOCKED" "AWAITING_OWNER")
  "Phase states that need no further execution without the owner.")

(defvar-local +tt--run-dir nil
  "Run directory shown by this tradeoffs-trace buffer.")

(defvar-local +tt--trace-agent nil
  "Agent id pinned in a trace buffer, or nil to follow the active agent.")

;;;; CLI

(defun +tt--runner-dir ()
  "Return the installed runner's package directory, or signal an error."
  (let ((dir (or +tt-runner
                 (expand-file-name "runner/current/tradeoffs-trace" +tt-root))))
    (unless (file-exists-p (expand-file-name "src/cli.ts" dir))
      (user-error "No tradeoffs-trace runner at %s; run `tt runner install <sha>'" dir))
    dir))

(defun +tt--cli (&rest args)
  "Run the tradeoffs-trace CLI with ARGS; return its stdout, trimmed."
  (let ((cli (expand-file-name "src/cli.ts" (+tt--runner-dir))))
    (with-temp-buffer
      (let ((status (apply #'call-process +tt-node nil t nil cli
                           (append args (list "--root" (directory-file-name +tt-root))))))
        (unless (eq status 0)
          (error "tt %s failed: %s" (string-join args " ") (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun +tt--state (run-dir)
  "Return the parsed `tt state' of RUN-DIR as nested alists."
  (json-parse-string (+tt--cli "state" run-dir)
                     :object-type 'alist :array-type 'list
                     :null-object nil :false-object :false))

(defun +tt--get (alist &rest keys)
  "Follow KEYS (symbols) through nested ALIST."
  (let ((v alist))
    (dolist (k keys v)
      (setq v (and (listp v) (alist-get k v))))))

;;;; Plan parsing and validation

(defun +tt--git (dir &rest args)
  "Run git ARGS in DIR; return trimmed stdout or nil."
  (with-temp-buffer
    (when (eq 0 (apply #'call-process "git" nil t nil "-C" dir args))
      (string-trim (buffer-string)))))

(defun +tt--keyword (name)
  "Return the value of #+NAME in the current buffer, or nil."
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (k) (when (string= (org-element-property :key k) name)
                  (org-element-property :value k)))
    nil t))

(defun +tt--phase-body (hl)
  "Return the body text of headline HL without its property drawer."
  (let ((beg (org-element-property :contents-begin hl))
        (end (org-element-property :contents-end hl)))
    (if (not beg) ""
      (replace-regexp-in-string
       "^[ \t]*:PROPERTIES:\\(?:.\\|\n\\)*?:END:[ \t]*\n?" ""
       (buffer-substring-no-properties beg end)))))

(defun +tt--goal (body)
  "Return the \"Goal:\" text of phase BODY, joined across continuation lines.
The goal ends at a blank line, a list item or the \"Acceptance:\" line."
  (let ((lines (split-string body "\n")) (goal nil) (in nil))
    (catch 'done
      (dolist (l lines)
        (let ((tl (string-trim l)))
          (cond
           ((string-match "\\`Goal:[ \t]*\\(.*\\)" tl) (setq in t goal (list (match-string 1 tl))))
           ((and in (or (string-empty-p tl) (string-prefix-p "-" tl) (string-prefix-p "Acceptance:" tl)))
            (throw 'done nil))
           (in (push tl goal))))))
    (when goal (string-trim (string-join (nreverse goal) " ")))))

(defun +tt--parse-phase (hl)
  "Parse phase headline HL into (PHASE-ALIST . ERRORS)."
  (let* ((line (line-number-at-pos (org-element-property :begin hl)))
         (id (org-element-property :ID hl))
         (checks (org-element-property :CHECKS hl))
         (boundaries (org-element-property :BOUNDARIES hl))
         (reserved (org-element-property :RESERVED hl))
         (provisional (member "provisional" (org-element-property :tags hl)))
         (body (+tt--phase-body hl))
         (goal (+tt--goal body))
         (acceptance
          (when (string-match "^[ \t]*Acceptance:[ \t]*\n\\(\\(?:[ \t]*- .*\n?\\)+\\)" body)
            (mapcar (lambda (l) (string-trim (replace-regexp-in-string "^[ \t]*- " "" l)))
                    (split-string (match-string 1 body) "\n" t "[ \t]+"))))
         (errors nil))
    (unless provisional
      (unless id (push (cons line "phase has no :ID: property") errors))
      (unless checks (push (cons line "phase has no :CHECKS: property") errors))
      (unless goal (push (cons line "phase has no \"Goal:\" line") errors))
      (unless acceptance (push (cons line "phase has no \"Acceptance:\" list") errors)))
    (cons `((id . ,(or id (format "line-%d" line)))
            (goal . ,(or goal ""))
            (acceptance . ,(vconcat acceptance))
            (checks . ,(vconcat (and checks (list checks))))
            (boundaries . ,(vconcat (and boundaries (split-string boundaries))))
            (reserved . ,(vconcat (and reserved (split-string reserved ";" t "[ \t]+"))))
            (provisional . ,(if provisional t :false)))
          (nreverse errors))))

(defun +tt-parse-plan ()
  "Parse the current Org plan buffer.
Return a plist (:plan ALIST :errors ((LINE . MESSAGE) ...))."
  (let* ((file (or buffer-file-name default-directory))
         (dir (file-name-directory (expand-file-name file)))
         (repo (or (+tt--keyword "TT_REPO")
                   (+tt--git dir "rev-parse" "--show-toplevel")))
         (branch (or (+tt--keyword "TT_BRANCH")
                     (and repo (+tt--git repo "symbolic-ref" "--short" "HEAD"))))
         (title (or (+tt--keyword "TITLE") (file-name-base file)))
         (global-checks (+tt--keyword "TT_CHECKS"))
         (errors nil) (phases nil) (ids nil))
    (unless repo (push (cons 1 "not inside a git repository and no #+TT_REPO") errors))
    (unless branch (push (cons 1 "cannot determine the integration branch; set #+TT_BRANCH") errors))
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (hl)
        (when (= (org-element-property :level hl) 1)
          (pcase-let ((`(,phase . ,errs) (+tt--parse-phase hl)))
            (let ((id (alist-get 'id phase)))
              (when (member id ids)
                (push (cons (line-number-at-pos (org-element-property :begin hl))
                            (format "duplicate phase ID %s" id))
                      errors))
              (push id ids))
            (setq errors (append errs errors))
            (push phase phases)))))
    (unless phases (push (cons 1 "plan has no phase headlines") errors))
    (list :plan `((title . ,title)
                  (repo . ,(or repo ""))
                  (integrationBranch . ,(or branch ""))
                  (checks . ,(vconcat (and global-checks (list global-checks))))
                  (phases . ,(vconcat (nreverse phases))))
          :errors (sort errors (lambda (a b) (< (car a) (car b)))))))

(defun +tt--show-plan-errors (file errors)
  "Show ERRORS for FILE in *tt-plan-errors* with jump-to-line."
  (with-current-buffer (get-buffer-create "*tt-plan-errors*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "tradeoffs-trace: %d plan error(s); no run started\n\n" (length errors)))
      (dolist (e errors)
        (insert (format "%s:%d: %s\n" file (car e) (cdr e)))))
    (compilation-mode)
    (display-buffer (current-buffer))))

;;;; Runs

(defun +tt--runs ()
  "Return run directories under `+tt-root', most recently active first."
  (let ((dirs (seq-filter
               (lambda (d) (file-exists-p (expand-file-name "meta.json" d)))
               (directory-files +tt-root t "\\`[^.]" t))))
    (sort dirs (lambda (a b)
                 (time-less-p (+tt--activity b) (+tt--activity a))))))

(defun +tt--activity (run-dir)
  "Return the last modification time of RUN-DIR's control log."
  (let ((f (expand-file-name "events.jsonl" run-dir)))
    (if (file-exists-p f) (file-attribute-modification-time (file-attributes f)) 0)))

(defun +tt--run-plan-path (run-dir)
  "Return the plan file recorded for RUN-DIR by Emacs, or nil."
  (let ((f (expand-file-name "emacs.json" run-dir)))
    (when (file-exists-p f)
      (alist-get 'planPath (json-read-file f)))))

(defun +tt--label (run-dir)
  "Completion label for RUN-DIR."
  (let* ((meta (ignore-errors (json-read-file (expand-file-name "meta.json" run-dir)))))
    (format "%s  %s" (file-name-nondirectory (directory-file-name run-dir))
            (or (alist-get 'title meta) ""))))

(defun +tt--resolve-run ()
  "Resolve the run a `C-c m' command means (design §1.4)."
  (or +tt--run-dir
      (let* ((plan buffer-file-name)
             (mine (and plan (seq-filter (lambda (d) (equal (+tt--run-plan-path d) plan))
                                         (+tt--runs)))))
        (cond
         ((= (length mine) 1) (car mine))
         ((> (length mine) 1)
          (let ((alist (mapcar (lambda (d) (cons (+tt--label d) d)) mine)))
            (cdr (assoc (completing-read "Run of this plan: " alist nil t) alist))))
         (t (let* ((runs (+tt--runs))
                   (alist (mapcar (lambda (d) (cons (+tt--label d) d)) runs)))
              (unless runs (user-error "No tradeoffs-trace runs under %s" +tt-root))
              (cdr (assoc (completing-read "tradeoffs-trace run: " alist nil t) alist))))))))

;;;###autoload
(defun +tt-run ()
  "Validate the plan in this buffer, start a run, and open its workspace."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not an Org plan buffer"))
  (let* ((parsed (+tt-parse-plan))
         (errors (plist-get parsed :errors))
         (file buffer-file-name))
    (if errors
        (+tt--show-plan-errors (or file (buffer-name)) errors)
      (let ((existing (and file (seq-find (lambda (d) (equal (+tt--run-plan-path d) file))
                                          (+tt--runs)))))
        (if (and existing
                 (not (member (+tt--phase-name (ignore-errors (+tt--state existing)))
                              +tt--terminal-phases))
                 (y-or-n-p "This plan has an active run; focus it instead of starting a new one? "))
            (+tt--workspace existing)
          (let* ((json-file (make-temp-file "tt-plan-" nil ".json"
                                            (json-encode (plist-get parsed :plan))))
                 (run-id (car (last (split-string (+tt--cli "start" json-file) "\n" t))))
                 (run-dir (expand-file-name run-id +tt-root)))
            (delete-file json-file)
            (with-temp-file (expand-file-name "emacs.json" run-dir)
              (insert (json-encode `((planPath . ,file)))))
            (write-region nil nil (expand-file-name "plan/v1.org" run-dir) nil 'silent)
            (message "tradeoffs-trace: started run %s" run-id)
            (+tt--workspace run-dir)))))))

(defun +tt--phase-name (state)
  "Return the phase state name in STATE."
  (+tt--get state 'state 'phase 'phase))

;;;; Workspace

(defun +tt--buffer (kind run-dir)
  "Return the KIND buffer for RUN-DIR, creating it if needed."
  (let* ((id (file-name-nondirectory (directory-file-name run-dir)))
         (buf (get-buffer-create (format "*tt-%s: %s*" kind id))))
    (with-current-buffer buf
      (pcase kind
        ("trace" (unless (derived-mode-p '+tt-trace-mode) (+tt-trace-mode)))
        ("status" (unless (derived-mode-p '+tt-status-mode) (+tt-status-mode)))
        ("input" (unless (derived-mode-p '+tt-input-mode) (+tt-input-mode))))
      (setq +tt--run-dir run-dir))
    buf))

(defun +tt--workspace (run-dir)
  "Open (or rebuild) the workspace tab for RUN-DIR."
  (let* ((id (file-name-nondirectory (directory-file-name run-dir)))
         (tab (format "tt:%s" id)))
    (if (tab-bar--tab-index-by-name tab)
        (tab-bar-select-tab-by-name tab)
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab))
    (delete-other-windows)
    (let* ((trace (+tt--buffer "trace" run-dir))
           (status (+tt--buffer "status" run-dir))
           (input (+tt--buffer "input" run-dir))
           (top (selected-window))
           (bottom (split-window top (- (max 6 (/ (window-total-height top) 5))) 'below)))
      (set-window-buffer top trace)
      (set-window-buffer (split-window top nil 'right) status)
      (set-window-buffer bottom input)
      (+tt--refresh-all)
      (select-window bottom))
    (+tt--ensure-timer)))

(defvar +tt--timer nil)

(defun +tt--ensure-timer ()
  "Start the workspace refresh timer."
  (unless (timerp +tt--timer)
    (setq +tt--timer (run-with-timer +tt-refresh-interval +tt-refresh-interval #'+tt--refresh-all))))

(defun +tt--refresh-all ()
  "Refresh every visible tradeoffs-trace buffer; stop the timer if none."
  (let ((any nil))
    (dolist (win (window-list-1 nil 'nomini t))
      (with-current-buffer (window-buffer win)
        (when +tt--run-dir
          (setq any t)
          (ignore-errors
            (cond ((derived-mode-p '+tt-trace-mode) (+tt--render-trace win))
                  ((derived-mode-p '+tt-status-mode) (+tt--render-status)))))))
    (unless any
      (when (timerp +tt--timer) (cancel-timer +tt--timer))
      (setq +tt--timer nil))))

;;;;; Trace

(defun +tt--stream-files (run-dir)
  "Agent stream files of RUN-DIR, most recently written first."
  (let ((dir (expand-file-name "stream" run-dir)))
    (when (file-directory-p dir)
      (sort (directory-files dir t "\\.jsonl\\'")
            (lambda (a b) (time-less-p (file-attribute-modification-time (file-attributes b))
                                       (file-attribute-modification-time (file-attributes a))))))))

(defun +tt--render-stream (file)
  "Render agent stream FILE as readable text: messages, tools folded."
  (let ((out nil) (text ""))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (let* ((rec (ignore-errors (json-parse-string line :object-type 'alist :null-object nil)))
               (ev (alist-get 'event rec))
               (type (alist-get 'type ev)))
          (pcase type
            ("agent_start" (push (format "\n[%s · %s]\n" (alist-get 'agentId rec)
                                         (substring (or (alist-get 'ts rec) "") 11 16))
                                 out))
            ("message_update"
             (let ((ame (alist-get 'assistantMessageEvent ev)))
               (when (equal (alist-get 'type ame) "text_delta")
                 (setq text (concat text (alist-get 'delta ame))))))
            ("message_end" (unless (string-empty-p text) (push (concat text "\n") out) (setq text "")))
            ("tool_execution_start"
             (unless (string-empty-p text) (push (concat text "\n") out) (setq text ""))
             (push (format "▸ tool: %s %s\n" (alist-get 'toolName ev)
                           (truncate-string-to-width
                            (replace-regexp-in-string "\n" " " (format "%s" (or (alist-get 'args ev) "")))
                            90 nil nil "…"))
                   out))
            ("agent_settled" (push "· settled\n" out))))))
    (concat (apply #'concat (nreverse out)) text)))

(defun +tt--render-trace (&optional win)
  "Re-render the trace buffer in WIN from the followed agent's stream file."
  (let* ((files (+tt--stream-files +tt--run-dir))
         (file (if +tt--trace-agent
                   (seq-find (lambda (f) (string-prefix-p +tt--trace-agent (file-name-nondirectory f))) files)
                 (car files))))
    (when file
      (let ((at-end (and win (>= (window-point win) (1- (point-max)))))
            (inhibit-read-only t))
        (erase-buffer)
        (insert (+tt--render-stream file))
        (when (and win at-end) (set-window-point win (point-max)))))))

(defun +tt-trace-pick-agent ()
  "Choose which agent of this run the trace follows."
  (interactive)
  (let* ((names (mapcar (lambda (f) (file-name-base f)) (+tt--stream-files +tt--run-dir)))
         (pick (completing-read "Follow agent (empty = active): " names)))
    (setq +tt--trace-agent (unless (string-empty-p pick) pick))
    (+tt--render-trace (get-buffer-window))))

(defvar-keymap +tt-trace-mode-map
  :parent special-mode-map
  "a" #'+tt-trace-pick-agent
  "g" #'+tt--refresh-all)

(define-derived-mode +tt-trace-mode special-mode "tt-trace"
  "Live trace of a tradeoffs-trace agent (read-only).")

;;;;; Status

(defun +tt--idle-minutes (run-dir)
  "Minutes since any agent of RUN-DIR produced an event."
  (let ((f (car (+tt--stream-files run-dir))))
    (if (not f) 0
      (/ (float-time (time-subtract nil (file-attribute-modification-time (file-attributes f)))) 60))))

(defun +tt--render-status ()
  "Render the status buffer from `tt state'."
  (let* ((s (+tt--state +tt--run-dir))
         (phase (+tt--get s 'state 'phase))
         (name (alist-get 'phase phase))
         (alive (eq (alist-get 'conductorAlive s) t))
         (reviews (alist-get 'reviews phase))
         (decisions (alist-get 'decisions phase))
         (findings (seq-filter (lambda (f) (equal (alist-get 'status f) "open")) (alist-get 'findings phase)))
         (requests (seq-filter (lambda (r) (equal (alist-get 'status r) "open")) (alist-get 'ownerRequests phase)))
         (idle (+tt--idle-minutes +tt--run-dir))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s\n" (+tt--get s 'meta 'title))
            (format "run %s   conductor %s   run %s\n\n"
                    (file-name-nondirectory (directory-file-name +tt--run-dir))
                    (if alive "running" "STOPPED") (+tt--get s 'state 'run))
            (format "%s  %s\n" (alist-get 'phaseId phase) (propertize (or name "?") 'face 'bold))
            (format "   attempt %s · repair rounds %s/%s\n"
                    (+tt--get phase 'attempt 'n) (alist-get 'repairRoundsUsed phase)
                    (alist-get 'repairRoundsGranted phase)))
    (when-let* ((c (+tt--get phase 'candidate 'sha))) (insert (format "   candidate %s\n" (substring c 0 7))))
    (when-let* ((ch (alist-get 'checks phase)))
      (insert (format "   checks %s\n" (if (eq (alist-get 'passed ch) t) "✓" "✗"))))
    (when-let* ((pr (alist-get 'probe phase)))
      (insert (format "   probe %s\n" (if (eq (alist-get 'passed pr) t) "✓" "✗"))))
    (insert (format "   reviews %s\n"
                    (mapconcat (lambda (w) (format "%s %s" w (if (alist-get 'review (alist-get w reviews)) "✓" "⧗")))
                               '(M A B) "  ")))
    (insert (format "   decisions %d · open findings %d\n" (length decisions) (length findings)))
    (when-let* ((why (alist-get 'blockedReason phase))) (insert (format "   blocked: %s\n" why)))
    (when (and alive (not (member name +tt--terminal-phases)) (> idle +tt-idle-minutes))
      (insert (propertize (format "\n   IDLE: no agent activity for %.0f min\n" idle) 'face 'warning)))
    (unless (or alive (member name '("DONE")))
      (insert (propertize "\n   conductor not running — M-x +tt-resume\n" 'face 'warning)))
    (when requests
      (insert (propertize (format "\n⚑ needs you: %d   (C-c m d)\n" (length requests)) 'face 'error)))))

(defvar-keymap +tt-status-mode-map
  :parent special-mode-map
  "d" #'+tt-decisions
  "g" #'+tt--refresh-all)

(define-derived-mode +tt-status-mode special-mode "tt-status"
  "Status of a tradeoffs-trace run.")

;;;;; Input

(defun +tt--write-command (run-dir command)
  "Write owner COMMAND (an alist) into RUN-DIR's inbox; return its id."
  (let* ((id (format "cmd-%s-%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))
         (inbox (expand-file-name "inbox" run-dir)))
    (make-directory inbox t)
    (with-temp-file (expand-file-name (concat id ".json") inbox)
      (insert (json-encode (cons (cons 'commandId id) command))))
    id))

(defun +tt-input-send ()
  "Queue the input buffer's text as an owner note for the next worker attempt."
  (interactive)
  (let ((text (string-trim (buffer-string))))
    (when (string-empty-p text) (user-error "Nothing to send"))
    (let ((s (+tt--state +tt--run-dir)))
      (+tt--write-command
       +tt--run-dir
       `((type . "note") (text . ,text)
         (binding . ((runId . ,(+tt--get s 'state 'phase 'runId))
                     (phaseId . ,(+tt--get s 'state 'phase 'phaseId)))))))
    (erase-buffer)
    (message "tradeoffs-trace: note queued for the next worker attempt")))

(defvar-keymap +tt-input-mode-map
  "C-c C-c" #'+tt-input-send)

(define-derived-mode +tt-input-mode text-mode "tt-input"
  "Owner input for a tradeoffs-trace run.  \\<+tt-input-mode-map>\\[+tt-input-send] sends.")

;;;; Show, resume

;;;###autoload
(defun +tt-show ()
  "Focus or rebuild the workspace of the run this buffer means."
  (interactive)
  (let* ((run (+tt--resolve-run))
         (s (ignore-errors (+tt--state run))))
    (when (and s (not (eq (alist-get 'conductorAlive s) t))
               (not (member (+tt--phase-name s) '("DONE" "BLOCKED")))
               (y-or-n-p "The conductor for this run is not running; resume it? "))
      (+tt--cli "resume" run))
    (+tt--workspace run)))

(defun +tt-resume ()
  "Relaunch the conductor of the current run."
  (interactive)
  (let ((run (+tt--resolve-run)))
    (+tt--cli "resume" run)
    (message "tradeoffs-trace: conductor relaunched for %s" (file-name-nondirectory run))))

;;;; Decision view

(defun +tt--binding (s record)
  "The design §7.1 binding tuple for RECORD in state S."
  (let ((phase (+tt--get s 'state 'phase)))
    `((runId . ,(alist-get 'runId phase))
      (phaseId . ,(alist-get 'phaseId phase))
      (candidateSha . ,(+tt--get phase 'candidate 'sha))
      (contractVersion . ,(+tt--get phase 'contract 'contractVersion))
      (recordId . ,(alist-get 'id record))
      (recordVersion . ,(alist-get 'version record)))))

(defun +tt--ballots-for (phase decision-id)
  "Ballots in PHASE on DECISION-ID."
  (seq-filter (lambda (b) (equal (alist-get 'decisionId b) decision-id)) (alist-get 'ballots phase)))

(defun +tt--decision-entry (keyword d phase)
  "Insert decision D under KEYWORD with its plain-language fields."
  (let ((start (point)))
    (insert (format "* %s %s    :%s:\n" keyword (alist-get 'choice d) (alist-get 'phaseId phase)))
    (insert (format "%s · %s · %s\n\n" (alist-get 'class d) (alist-get 'source d) (alist-get 'id d)))
    (insert (format "Why it matters: %s\n\n" (alist-get 'whyItMatters d)))
    (dolist (a (alist-get 'alternatives d))
      (insert (format "  - %s — %s\n" (alist-get 'option a) (alist-get 'consequence a))))
    (when-let* ((r (alist-get 'recommendation d)))
      (insert (format "\nRecommendation: %s. %s\n" (alist-get 'choice r) (alist-get 'reason r))))
    (let ((ballots (+tt--ballots-for phase (alist-get 'id d))))
      (insert "\n** Details\n")
      (insert (format "   - decision %s v%s\n" (alist-get 'id d) (alist-get 'version d)))
      (dolist (b ballots)
        (insert (format "   - %s %s: %s\n" (alist-get 'reviewer b) (alist-get 'vote b) (alist-get 'rationale b)))))
    (insert "\n")
    (put-text-property start (point) '+tt-record (cons 'decision d))))

(defun +tt--dissent-p (phase d)
  "Non-nil when some ballot on D rejected."
  (seq-some (lambda (b) (equal (alist-get 'vote b) "reject")) (+tt--ballots-for phase (alist-get 'id d))))

(defun +tt--render-decisions (s)
  "Insert the decision view for state S (design §10.1 sections)."
  (let* ((phase (+tt--get s 'state 'phase))
         (requests (seq-filter (lambda (r) (equal (alist-get 'status r) "open")) (alist-get 'ownerRequests phase)))
         (findings (seq-filter (lambda (f) (equal (alist-get 'status f) "open")) (alist-get 'findings phase)))
         (corrections (alist-get 'corrections phase))
         (decisions (alist-get 'decisions phase))
         (details (seq-filter (lambda (d) (equal (alist-get 'class d) "detail")) decisions))
         (others (seq-remove (lambda (d) (equal (alist-get 'class d) "detail")) decisions)))
    (insert (format "#+TITLE: decisions — %s\n\n" (+tt--get s 'meta 'title)))
    (insert (format "Needs you (%d)\n\n" (length requests)))
    (dolist (r requests)
      (let ((start (point)))
        (insert (format "* NEEDS-YOU %s\n" (or (alist-get 'question r) (alist-get 'origin r))))
        (let ((i 0))
          (dolist (o (alist-get 'options r))
            (setq i (1+ i))
            (insert (format "  %d. %s\n" i (alist-get 'label o)))))
        (insert "\n")
        (put-text-property start (point) '+tt-record (cons 'request r))))
    (insert (format "Open findings (%d)\n\n" (length findings)))
    (dolist (f findings)
      (let ((start (point)))
        (insert (format "* FINDING %s — raised by %s · %s\n  %s\n\n"
                        (alist-get 'kind f) (alist-get 'raisedBy f) (alist-get 'severity f)
                        (alist-get 'evidence f)))
        (put-text-property start (point) '+tt-record (cons 'finding f))))
    (insert (format "Corrections (%d)\n\n" (length corrections)))
    (dolist (c corrections)
      (insert (format "* CORRECTION %s — %s\n  %s\n\n" (alist-get 'id c) (alist-get 'status c)
                      (alist-get 'text c))))
    (let ((dissent (seq-filter (lambda (d) (+tt--dissent-p phase d)) others)))
      (insert (format "Accepted with dissent / pending vote (%d)\n\n" (length dissent)))
      (dolist (d dissent) (+tt--decision-entry "DISSENT" d phase))
      (insert (format "Decisions (%d)\n\n" (- (length others) (length dissent))))
      (dolist (d (seq-remove (lambda (d) (+tt--dissent-p phase d)) others))
        (+tt--decision-entry "DECISION" d phase)))
    (insert (format "For sampling (%d detail)\n\n" (length details)))
    (dolist (d details) (+tt--decision-entry "SAMPLE" d phase))))

(defvar-local +tt--decision-state nil
  "The `tt state' the decision view was rendered from (for bindings).")

;;;###autoload
(defun +tt-decisions ()
  "Open the decision view of the run this buffer means."
  (interactive)
  (let* ((run (+tt--resolve-run))
         (s (+tt--state run))
         (buf (get-buffer-create (format "*tt-decisions: %s*" (file-name-nondirectory (directory-file-name run))))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (+tt--render-decisions s))
      (+tt-decisions-mode)
      (setq +tt--run-dir run +tt--decision-state s)
      (org-overview)
      (org-cycle-hide-drawers 'all)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun +tt--record-at-point ()
  "Return (KIND . RECORD) at point, or signal."
  (or (get-text-property (point) '+tt-record) (user-error "No record at point")))

(defun +tt--queue (type &rest fields)
  "Queue an owner command TYPE for the record at point with FIELDS."
  (pcase-let ((`(,kind . ,rec) (+tt--record-at-point)))
    (let ((id (+tt--write-command
               +tt--run-dir
               `((type . ,type) (recordKind . ,(symbol-name kind))
                 (binding . ,(+tt--binding +tt--decision-state rec)) ,@fields))))
      (message "tradeoffs-trace: queued %s (%s); refresh with g" type id))))

(defun +tt-decision-revise ()
  "Revise the decision or finding at point: describe the correction."
  (interactive)
  (+tt--queue "revise" (cons 'text (read-string "Correction (what should change): "))
              (cons 'changesContract (if (y-or-n-p "Does this change the contract? ") t :false))))

(defun +tt-decision-override ()
  "Override the delegated decision at point."
  (interactive)
  (+tt--queue "override" (cons 'vote (completing-read "Override: " '("approve" "reject") nil t))))

(defun +tt-decision-accept-finding ()
  "Accept the finding at point, with a scope note."
  (interactive)
  (+tt--queue "accept-finding" (cons 'scope (read-string "Scope of the accepted risk: "))))

(defun +tt-decision-resolve (n)
  "Resolve the owner request at point with option N (or write one with 0)."
  (interactive "p")
  (pcase-let ((`(,_ . ,rec) (+tt--record-at-point)))
    (let ((opt (nth (1- n) (alist-get 'options rec))))
      (+tt--queue "resolve" (cons 'option (or (alist-get 'id opt) (read-string "Your option: ")))))))

(defun +tt-decision-miss ()
  "Mark the sampled item at point as \"should have been surfaced\"."
  (interactive)
  (+tt--queue "miss"))

(defun +tt-decision-unneeded ()
  "Mark the owner request at point as \"did not need me\"."
  (interactive)
  (+tt--queue "unneeded"))

(defvar-keymap +tt-decisions-mode-map
  "r" #'+tt-decision-revise
  "o" #'+tt-decision-override
  "x" #'+tt-decision-accept-finding
  "s" #'+tt-decision-miss
  "u" #'+tt-decision-unneeded
  "w" (lambda () (interactive) (+tt-decision-resolve 0))
  "1" (lambda () (interactive) (+tt-decision-resolve 1))
  "2" (lambda () (interactive) (+tt-decision-resolve 2))
  "3" (lambda () (interactive) (+tt-decision-resolve 3))
  "g" #'+tt-decisions
  "TAB" #'org-cycle)

(define-derived-mode +tt-decisions-mode org-mode "tt-decisions"
  "Read-only decision view of a tradeoffs-trace run; act with keys."
  (setq buffer-read-only t))

;;;; Keys

(keymap-global-set "C-c m r" #'+tt-run)
(keymap-global-set "C-c m s" #'+tt-show)
(keymap-global-set "C-c m d" #'+tt-decisions)

(provide 'init-tradeoffs-trace)
;;; init-tradeoffs-trace.el ends here
