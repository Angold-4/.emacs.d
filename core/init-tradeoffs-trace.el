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
;;   C-c m d   the run's decision view (read-only; intervene via the input box)
;;   C-c m l   every run: open (RET), stop (k), resume (R)
;;   C-c m p   a program (several plans / phases as one graph): RET opens a
;;             phase's run, k stops, R resumes
;;
;; The conductor always comes from the *installed runner*
;; (`<root>/runner/current/tradeoffs-trace', see `tt runner install <sha>'),
;; never from the repository being edited, so a run that works on
;; tradeoffs-trace itself does not execute the code under review.
;;
;; The workspace polls the run directory every `+tt-refresh-interval'
;; seconds.  The trace appends one line per tool call of the most recently
;; active agent, with the files each call changed; the status shows the
;; pipeline with stage times, where the active agent's time goes, each
;; reviewer's outcome and why the phase did or did not accept (all computed
;; by `tt state', see tradeoffs-trace/src/view.ts).  The conductor itself
;; nudges, then ends, an agent that stalls mid-turn; the status flags it.

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

(defun +tt--plan-references (dir)
  "Absolute paths of the documents this plan cites that exist on disk.
From #+TT_REFS (space separated) and from every *.md name in the plan
text that resolves relative to DIR (the plan's directory) or to ~.  The
conductor copies them into the run and tells every agent where they are,
so no agent searches the file system for them."
  (let ((found nil)
        (self (and buffer-file-name (expand-file-name buffer-file-name))))
    (dolist (name (split-string (or (+tt--keyword "TT_REFS") "") "[ ,]+" t))
      (push name found))
    (save-excursion
      (goto-char (point-min))
      ;; Markdown only: plans name sibling .org plans for ordering, not as references.
      (while (re-search-forward "\\(?:~/\\|/\\)?[[:alnum:]_./-]*[[:alnum:]_-]\\.md\\_>" nil t)
        (push (match-string-no-properties 0) found)))
    (let ((out nil))
      (dolist (name (nreverse found))
        (let ((abs (expand-file-name name (if (string-prefix-p "~" name) "~" dir))))
          (when (and (file-regular-p abs)
                     (not (equal abs self))
                     (not (member abs out)))
            (push abs out))))
      (nreverse out))))

(defun +tt--plan-deadlines ()
  "Per-plan time limits from #+TT_SH_MINUTES, #+TT_CHECK_MINUTES and
#+TT_ATTEMPT_MINUTES, as the conductor's deadline fields in ms (or nil).
For repositories whose builds and suites outlast the defaults (3, 5, 45)."
  (let ((ms (lambda (kw) (let ((v (+tt--keyword kw)))
                           (and v (string-match-p "\\`[0-9]+\\'" v) (* 60000 (string-to-number v))))))
        (out nil))
    (when-let* ((v (funcall ms "TT_SH_MINUTES"))) (push (cons 'shCommandMs v) out))
    (when-let* ((v (funcall ms "TT_CHECK_MINUTES"))) (push (cons 'checkMs v) out) (push (cons 'probeMs v) out))
    (when-let* ((v (funcall ms "TT_ATTEMPT_MINUTES"))) (push (cons 'workerAttemptMs v) out))
    (nreverse out)))

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
                  (phases . ,(vconcat (nreverse phases)))
                  ,@(let ((d (+tt--plan-deadlines)))
                      (and d `((deadlines . ,d))))
                  ,@(let ((r (+tt--plan-references dir)))
                      (and r `((references . ,(vconcat r))))))
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
  "Validate the plan in this buffer, start a run, and open its workspace.
A program file (#+TT_PROGRAM) or a plan with several phases starts a
program instead: one run per phase, in dependency order (see `+tt-program')."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not an Org plan buffer"))
  (if (or (+tt--keyword "TT_PROGRAM")
          (> (length (alist-get 'phases (plist-get (+tt-parse-plan) :plan))) 1))
      (+tt-program-start)
    (+tt--run-single)))

(defun +tt--run-single ()
  "Start a single-phase run from the plan in this buffer."
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

;;;; Programs (phase 4)

;; A program runs several plans, and plans with several phases, as one
;; dependency graph: every phase is an ordinary run (same review loop), a
;; phase starts only when what it depends on is DONE, and independent phases
;; run in parallel.  By default each phase publishes to its own branch cut
;; from its dependencies' branches ("<TT_BRANCH>--<id>"), so every phase is
;; its own stacked PR.  A program file:
;;
;;   #+TITLE: plan 13
;;   #+TT_PROGRAM: 4                  max phases in parallel
;;   #+TT_BRANCHES: stack             or "shared": all publish to TT_BRANCH
;;   * 13a
;;     :PROPERTIES:
;;     :PLAN:   13a_shared_markets.org
;;     :END:
;;   * 13c
;;     :PROPERTIES:
;;     :PLAN:   13c_vendor_pyth.org
;;     :AFTER:  13b
;;     :END:

(defun +tt--plan-of-file (file)
  "Parse the plan FILE; return (PLAN . ERRORS), errors prefixed with FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (setq buffer-file-name file default-directory (file-name-directory file))
    (delay-mode-hooks (org-mode))
    (let ((parsed (+tt-parse-plan)))
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil)
      (cons (plist-get parsed :plan)
            (mapcar (lambda (e) (cons (car e) (format "%s:%d: %s" (file-name-nondirectory file) (car e) (cdr e))))
                    (plist-get parsed :errors))))))

(defun +tt-parse-program ()
  "Parse the current buffer as a program.
Return a plist (:program ALIST :errors ((LINE . MESSAGE) ...)).  A plan
buffer that is not a program becomes a program of one entry, so a plan with
several phases runs them in order."
  (let* ((file (or buffer-file-name default-directory))
         (dir (file-name-directory (expand-file-name file)))
         (title (or (+tt--keyword "TITLE") (file-name-base file)))
         (max (let ((v (+tt--keyword "TT_PROGRAM"))) (if (and v (string-match-p "\\`[0-9]+\\'" v)) (string-to-number v) 1)))
         (branches (or (+tt--keyword "TT_BRANCHES") "stack"))
         ;; TT_*_MINUTES in the program file: the default for every entry
         ;; whose plan does not set its own.
         (defaults (+tt--plan-deadlines))
         (entries nil) (errors nil))
    (if (not (+tt--keyword "TT_PROGRAM"))
        (let ((parsed (+tt-parse-plan)))
          (setq errors (plist-get parsed :errors))
          (push `((id . ,(file-name-base file)) (after . []) (plan . ,(plist-get parsed :plan))) entries))
      (org-element-map (org-element-parse-buffer 'headline) 'headline
        (lambda (hl)
          (when (= (org-element-property :level hl) 1)
            (let* ((line (line-number-at-pos (org-element-property :begin hl)))
                   (id (or (org-element-property :ID hl) (org-element-property :raw-value hl)))
                   (plan-file (org-element-property :PLAN hl))
                   (after (split-string (or (org-element-property :AFTER hl) "") "[ ,]+" t)))
              (cond
               ((not plan-file) (push (cons line (format "entry %s has no :PLAN:" id)) errors))
               ((not (file-exists-p (expand-file-name plan-file dir)))
                (push (cons line (format "entry %s: plan file %s not found" id plan-file)) errors))
               (t
                (pcase-let ((`(,plan . ,errs) (+tt--plan-of-file (expand-file-name plan-file dir))))
                  (dolist (e errs) (push (cons line (cdr e)) errors))
                  (when (and defaults (not (assq 'deadlines plan)))
                    (setq plan (append plan `((deadlines . ,defaults)))))
                  (push `((id . ,id) (after . ,(vconcat after)) (plan . ,plan)) entries)))))))))
    (unless entries (push (cons 1 "program has no entries") errors))
    (list :program `((title . ,title) (maxParallel . ,max) (branches . ,branches)
                     (entries . ,(vconcat (nreverse entries))))
          :errors (sort errors (lambda (a b) (< (car a) (car b)))))))

(defvar-local +tt--program-dir nil "Program directory shown by this buffer.")

(defun +tt-program-start ()
  "Validate the program (or multi-phase plan) in this buffer and start it."
  (interactive)
  (let* ((parsed (+tt-parse-program))
         (errors (plist-get parsed :errors)))
    (if errors
        (+tt--show-plan-errors (or buffer-file-name (buffer-name)) errors)
      (let* ((json-file (make-temp-file "tt-program-" nil ".json" (json-encode (plist-get parsed :program))))
             (id (car (last (split-string (+tt--cli "program" "start" json-file) "\n" t)))))
        (delete-file json-file)
        (message "tradeoffs-trace: started program %s" id)
        (+tt-program (expand-file-name (concat "programs/" id) +tt-root))))))

(defun +tt--program-state (dir)
  "Parsed `tt program state' of DIR."
  (json-parse-string (+tt--cli "program" "state" dir) :object-type 'alist :array-type 'list
                     :null-object nil :false-object :false))

(defun +tt--render-program ()
  "Render the program buffer from `tt program state'."
  (let* ((s (+tt--program-state +tt--program-dir))
         (nodes (alist-get 'nodes (alist-get 'state s)))
         (inhibit-read-only t)
         (pt (point)))
    (erase-buffer)
    (dolist (line (alist-get 'lines s))
      (let ((start (point)))
        (insert line "\n")
        ;; RET on a node's line opens its run.
        (when (string-match "\\`[^ ]+ \\([^ ]+\\)" line)
          (let ((run (alist-get 'runId (alist-get (intern (match-string 1 line)) nodes))))
            (when run (put-text-property start (point) '+tt-run-id run))))))
    (goto-char (min pt (point-max)))))

(defun +tt-program-open-node ()
  "Open the workspace of the node's run at point."
  (interactive)
  (let ((run (get-text-property (point) '+tt-run-id)))
    (unless run (user-error "No run on this line"))
    (+tt--workspace (expand-file-name run +tt-root))))

(defun +tt-program-stop ()
  "Stop this program: its scheduler and every running node."
  (interactive)
  (when (y-or-n-p "Stop this program and its running phases? ")
    (message "%s" (+tt--cli "program" "stop" +tt--program-dir))
    (+tt--render-program)))

(defun +tt-program-resume ()
  "Restart this program's scheduler."
  (interactive)
  (message "%s" (+tt--cli "program" "resume" +tt--program-dir))
  (+tt--render-program))

(defvar-keymap +tt-program-mode-map
  :parent special-mode-map
  "RET" #'+tt-program-open-node
  "k" #'+tt-program-stop
  "R" #'+tt-program-resume
  "g" #'+tt--refresh-all)

(define-derived-mode +tt-program-mode special-mode "tt-program"
  "A tradeoffs-trace program.  \\<+tt-program-mode-map>\\[+tt-program-open-node] opens a phase's run, \\[+tt-program-stop] stops, \\[+tt-program-resume] resumes."
  (visual-line-mode 1)
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +tt-program-mode-map
      (kbd "RET") #'+tt-program-open-node "k" #'+tt-program-stop "R" #'+tt-program-resume "g" #'+tt--refresh-all)))

;;;###autoload
(defun +tt-program (&optional dir)
  "Show the program in DIR, or choose one."
  (interactive)
  (let* ((dir (or dir
                  (let ((ids (directory-files (expand-file-name "programs" +tt-root) nil "\\`[^.]")))
                    (unless ids (user-error "No programs under %s" +tt-root))
                    (expand-file-name (concat "programs/" (completing-read "Program: " (reverse ids) nil t)) +tt-root))))
         (buf (get-buffer-create (format "*tt-program: %s*" (file-name-nondirectory (directory-file-name dir))))))
    (with-current-buffer buf
      (+tt-program-mode)
      (setq +tt--program-dir dir +tt--run-dir dir)
      (+tt--render-program))
    (pop-to-buffer buf)
    (+tt--ensure-timer)))

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
                  ((derived-mode-p '+tt-status-mode) (+tt--render-status))
                  ((derived-mode-p '+tt-input-mode) (+tt--render-input-header))
                  ((derived-mode-p '+tt-program-mode) (+tt--render-program)))))))
    (unless any
      (when (timerp +tt--timer) (cancel-timer +tt--timer))
      (setq +tt--timer nil))))

;;;;; Trace

;; The trace is rendered incrementally: only bytes appended to the followed
;; agent's stream file since the last refresh are read, and the large,
;; frequent `message_update' records are skipped without being parsed.  Each
;; tool call becomes one line when it finishes; the call still running is
;; shown in the header line with its elapsed time.

(defvar-local +tt--trace-file nil "Stream file the trace buffer renders.")
(defvar-local +tt--trace-offset 0 "Bytes of `+tt--trace-file' already rendered.")
(defvar-local +tt--trace-partial "" "Trailing incomplete line from the last read.")
(defvar-local +tt--trace-open nil "Hash of running tool calls: id -> (TS NAME ARG).")

(defun +tt--stream-files (run-dir)
  "Agent stream files of RUN-DIR, most recently written first."
  (let ((dir (expand-file-name "stream" run-dir)))
    (when (file-directory-p dir)
      (sort (directory-files dir t "\\.jsonl\\'")
            (lambda (a b) (time-less-p (file-attribute-modification-time (file-attributes b))
                                       (file-attribute-modification-time (file-attributes a))))))))

(defun +tt--hms (ts)
  "Local HH:MM:SS of ISO timestamp TS."
  (or (ignore-errors (format-time-string "%H:%M:%S" (date-to-time ts))) "--:--:--"))

(defun +tt--secs-between (from to)
  "Seconds from ISO timestamp FROM to TO (nil TO means now)."
  (or (ignore-errors
        (- (float-time (if to (date-to-time to) nil)) (float-time (date-to-time from))))
      0))

(defun +tt--dur (secs)
  "Short human duration for SECS."
  (let ((s (max 0 (round secs))))
    (cond ((< s 60) (format "%ds" s))
          ((< s 3600) (format "%dm%02ds" (/ s 60) (% s 60)))
          (t (format "%dh%02dm" (/ s 3600) (/ (% s 3600) 60))))))

(defun +tt--one-line (text width)
  "TEXT on one line, truncated to WIDTH."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[\n\t ]+" " " (or text ""))) width nil nil "…"))

(defun +tt--tool-verb (name arg)
  "Short rendering of tool NAME with ARG: `$ cmd' for sh, else `name arg'."
  (let ((polling (and (member name '("sh" "bash"))
                      (string-match-p "\\bsleep\\b\\|\\btail -f\\b" (or arg "")))))
    (concat (if (member name '("sh" "bash")) "$ " (concat name " "))
            (+tt--one-line arg 300)
            (if polling "  (polling)" ""))))

(defun +tt--tool-arg (args)
  "The one argument that identifies a tool call in ARGS."
  (let ((v (or (alist-get 'command args) (alist-get 'path args) (alist-get 'pattern args) "")))
    (if (stringp v) v (format "%s" v))))

(defun +tt--result-tail (ev)
  "Last non-empty output line of a tool_execution_end event EV."
  (let* ((content (alist-get 'content (alist-get 'result ev)))
         (text (and (listp content) (alist-get 'text (car content)))))
    (when (stringp text)
      (let ((lines (seq-remove #'string-blank-p (split-string text "\n"))))
        (when lines (+tt--one-line (car (last lines)) 60))))))

(defun +tt--trace-line (rec)
  "Rendered text for stream record REC, or nil to show nothing."
  (let* ((ev (alist-get 'event rec))
         (type (alist-get 'type ev))
         (ts (alist-get 'ts rec)))
    (pcase type
      ("agent_start" (format "── %s · %s ──\n" (alist-get 'agentId rec) (+tt--hms ts)))
      ("message_end"
       (let* ((msg (alist-get 'message ev))
              (texts (and (equal (alist-get 'role msg) "assistant")
                          (seq-keep (lambda (c) (and (equal (alist-get 'type c) "text") (alist-get 'text c)))
                                    (alist-get 'content msg)))))
         (when (and texts (not (string-blank-p (string-join texts " "))))
           (format "%s » %s\n" (+tt--hms ts) (+tt--one-line (string-join texts " ") 400)))))
      ("tool_execution_start"
       (puthash (alist-get 'toolCallId ev)
                (list ts (alist-get 'toolName ev) (+tt--tool-arg (alist-get 'args ev)))
                +tt--trace-open)
       nil)
      ("tool_execution_end"
       (let ((start (gethash (alist-get 'toolCallId ev) +tt--trace-open)))
         (remhash (alist-get 'toolCallId ev) +tt--trace-open)
         (when start
           (let* ((err (eq (alist-get 'isError ev) t))
                  (code (+tt--get ev 'result 'details 'exitCode))
                  (tail (+tt--result-tail ev)))
             (format "%s %s %s %s%s\n"
                     (+tt--hms (nth 0 start))
                     (+tt--tool-verb (nth 1 start) (nth 2 start))
                     (if err (propertize (format "✗%s" (if (numberp code) code "")) 'face 'error) "✓")
                     (+tt--dur (+tt--secs-between (nth 0 start) ts))
                     (if (and tail (member (nth 1 start) '("sh" "bash"))) (concat " · " tail) ""))))))
      ("tt_file_changes"
       (mapconcat (lambda (f)
                    (format "           %s %s\n" (alist-get 'path f)
                            (propertize (format "+%d −%d" (alist-get 'added f) (alist-get 'removed f))
                                        'face 'shadow)))
                  (alist-get 'files ev) ""))
      ("agent_settled" "· settled\n"))))

(defun +tt--trace-header ()
  "Header line: the followed agent and the tool call still running, if any."
  (let ((running nil))
    (when +tt--trace-open
      (maphash (lambda (_ v) (push v running)) +tt--trace-open))
    (concat (if +tt--trace-file (file-name-base +tt--trace-file) "no agent yet")
            (if +tt--trace-agent "  (pinned; a picks)" "  (following the active agent; a pins)")
            (mapconcat (lambda (v)
                         (format "   ⧗ %s %s" (+tt--tool-verb (nth 1 v) (nth 2 v))
                                 (+tt--dur (+tt--secs-between (nth 0 v) nil))))
                       running ""))))

(defun +tt--render-trace (&optional win)
  "Append whatever the followed agent's stream gained since the last refresh."
  (let* ((files (+tt--stream-files +tt--run-dir))
         (file (if +tt--trace-agent
                   (seq-find (lambda (f) (string-prefix-p +tt--trace-agent (file-name-nondirectory f))) files)
                 (car files)))
         (inhibit-read-only t))
    (unless (equal file +tt--trace-file)
      (erase-buffer)
      (setq +tt--trace-file file +tt--trace-offset 0 +tt--trace-partial ""
            +tt--trace-open (make-hash-table :test 'equal)))
    (when file
      (let* ((size (file-attribute-size (file-attributes file)))
             (from +tt--trace-offset)   ; buffer-local: read it here, not in the temp buffer
             (at-end (or (not win) (>= (window-point win) (1- (point-max)))))
             (chunk (when (> size from)
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally file nil from size)
                        (decode-coding-string (buffer-string) 'utf-8)))))
        (when chunk
          (setq +tt--trace-offset size)
          (let* ((lines (split-string (concat +tt--trace-partial chunk) "\n"))
                 (out nil))
            (setq +tt--trace-partial (car (last lines)))
            (dolist (line (butlast lines))
              (unless (or (string-empty-p line) (string-search "\"message_update\"" line))
                (when-let* ((rec (ignore-errors
                                   (json-parse-string line :object-type 'alist :array-type 'list
                                                      :null-object nil :false-object :false)))
                            (text (+tt--trace-line rec)))
                  (push text out))))
            (when out
              (save-excursion
                (goto-char (point-max))
                (insert (apply #'concat (nreverse out)))))))
        (setq header-line-format (+tt--trace-header))
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
  "Live trace of a tradeoffs-trace agent (read-only)."
  (visual-line-mode 1))

;;;;; Status

(defun +tt--owner-input-state-label (state reason)
  "Human label for a recorded owner-input STATE, with REASON when present."
  (pcase state
    ("delivered" "delivered")
    ("noted" "noted")
    ("correction-started" "correction started")
    ("delivery-uncertain" (format "delivery uncertain%s" (if reason (format " (%s)" reason) "")))
    ("refused" (format "refused: %s" (or reason "not accepted")))
    (_ (or state "sent"))))

(defun +tt--not-picked-up-p (at)
  "True when ISO time AT is more than 30 s in the past."
  (ignore-errors
    (> (- (float-time) (float-time (date-to-time at))) 30)))

(defun +tt--render-owner-inputs (s)
  "Insert the Owner input section (design §7.4/§9.3) from state S, if any.
Recorded effects come from the conductor (`ownerInputs'); anything still
sitting in the inbox (`pendingOwnerInputs') is shown as sent, or `not
picked up' once 30 s have passed.  Nothing is inferred beyond that."
  (let* ((recorded (or (alist-get 'ownerInputs s) nil))
         (pending (or (alist-get 'pendingOwnerInputs s) nil))
         (entries (append
                   (mapcar (lambda (r)
                             (cons (+tt--owner-input-state-label (alist-get 'state r) (alist-get 'reason r)) r))
                           recorded)
                   (mapcar (lambda (r)
                             (let ((at (alist-get 'at r)))
                               (cons (if (and at (+tt--not-picked-up-p at)) "not picked up" "sent") r)))
                           pending))))
    (when entries
      (insert (format "\nOwner input (%d)\n" (length entries)))
      (dolist (e entries)
        (let* ((r (cdr e))
               (text (or (alist-get 'text r) ""))
               (kind (alist-get 'kind r)))
          (insert (format "  - %s — %s%s\n"
                          (truncate-string-to-width text 70 nil nil "…")
                          (car e)
                          (if kind (format " (%s)" kind) ""))))))))

(defun +tt--render-input-header ()
  "Recompute the *tt-input* header line from the current run state."
  (setq header-line-format
        (condition-case err
            (+tt--input-header (+tt--state +tt--run-dir))
          (error (format "Cannot deliver input: %s" (error-message-string err))))))

(defun +tt--status-row (label value &optional face)
  "Insert one status row: LABEL padded, then VALUE (in FACE)."
  (when (and value (not (string-empty-p value)))
    (let ((start (point)))
      (insert (propertize (format "%-10s" label) 'face 'shadow)
              (if face (propertize value 'face face) value) "\n")
      ;; A row longer than the window wraps under its value, not its label.
      (put-text-property start (point) 'wrap-prefix (make-string 10 ?\s)))))

(defun +tt--render-status-from (s run-dir)
  "Insert the status of RUN-DIR from `tt state' S (plan 3b layout)."
  (let* ((phase (+tt--get s 'state 'phase))
         (name (alist-get 'phase phase))
         (alive (eq (alist-get 'conductorAlive s) t))
         (v (alist-get 'view s))
         (attention (alist-get 'attention v)))
    (insert (propertize (or (+tt--get s 'meta 'title) "") 'face 'bold) "\n")
    (insert (propertize
             (format "run %s · %s · %s\n\n"
                     (file-name-nondirectory (directory-file-name run-dir))
                     (if alive "conductor running" "conductor stopped")
                     (alist-get 'elapsed v))
             'face 'shadow))
    (+tt--status-row "phase"
                     (format "%s · %s · round %s · attempt %s · repairs %s/%s"
                             (alist-get 'phaseId phase) name (alist-get 'round v)
                             (+tt--get phase 'attempt 'n) (alist-get 'repairRoundsUsed phase)
                             (alist-get 'repairRoundsGranted phase)))
    (+tt--status-row "pipeline" (alist-get 'pipeline v))
    (+tt--status-row "time" (alist-get 'time v))
    (+tt--status-row "gates" (alist-get 'gates v))
    (+tt--status-row "previous" (alist-get 'previousRound v) 'shadow)
    (+tt--status-row "reviews" (alist-get 'reviewLine v))
    (+tt--status-row "verdict" (alist-get 'verdict v)
                     (if (equal name "DONE") 'success 'warning))
    (+tt--status-row "records"
                     (format "%d decisions%s%s · %d open findings%s"
                             (alist-get 'liveDecisions v)
                             (let ((f (alist-get 'failedDecisions v))) (if (> f 0) (format " (%d failed)" f) ""))
                             (let ((f (or (alist-get 'flaggedDecisions v) 0))) (if (> f 0) (format " · %d flagged for you" f) ""))
                             (alist-get 'openFindings v)
                             (let ((b (alist-get 'boundaryFilesChanged v)))
                               (if (> b 0) (format " · boundary files changed: %d (reviewers classify)" b) ""))))
    (when-let* ((why (alist-get 'blockedReason phase)))
      (+tt--status-row "blocked" why 'error))
    (+tt--render-owner-inputs s)
    (when attention
      (insert "\n" (propertize (format "⚑ %s%s" attention
                                       (cond ((equal attention "needs you")
                                              " — type a correction in the input box (C-c m d to read the decisions)")
                                             ((equal attention "conductor stopped") " — M-x +tt-resume")
                                             (t "")))
                               'face 'error)
              "\n"))))

(defun +tt--render-status ()
  "Render the status buffer from `tt state'."
  (let ((s (+tt--state +tt--run-dir))
        (inhibit-read-only t))
    (erase-buffer)
    (+tt--render-status-from s +tt--run-dir)))

(defvar-keymap +tt-status-mode-map
  :parent special-mode-map
  "d" #'+tt-decisions
  "g" #'+tt--refresh-all)

(define-derived-mode +tt-status-mode special-mode "tt-status"
  "Status of a tradeoffs-trace run."
  (visual-line-mode 1))

;;;;; Input

(defun +tt--write-command (run-dir command)
  "Write owner COMMAND (an alist) into RUN-DIR's inbox; return its id.
The document is written to a temporary name and renamed into place, so a
conductor poll can never observe a truncated or half-written file (which it
would otherwise reject permanently as malformed)."
  (let* ((id (format "cmd-%s-%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))
         (inbox (expand-file-name "inbox" run-dir))
         (final (expand-file-name (concat id ".json") inbox))
         (tmp (expand-file-name (concat id ".json.tmp") inbox))
         (payload (json-encode (cons (cons 'commandId id) command))))
    (make-directory inbox t)
    (write-region payload nil tmp nil 'silent)
    (rename-file tmp final t)
    id))

(defun +tt--input-header (s)
  "One line stating what sending the input will do now, from state S.
Design §7.4/§7.5: a steer while a worker runs, a note otherwise, a
correction while the phase is AWAITING_OWNER, a refusal after DONE/BLOCKED
— or that the conductor cannot deliver input at all."
  (let* ((phase (+tt--get s 'state 'phase))
         (name (alist-get 'phase phase))
         (attempt (+tt--get phase 'attempt 'n))
         (alive (eq (alist-get 'conductorAlive s) t))
         (requests (seq-filter (lambda (r) (equal (alist-get 'status r) "open"))
                               (alist-get 'ownerRequests phase))))
    (cond
     ((equal name "DONE")
      "Sending is refused: the run is DONE.")
     ((equal name "BLOCKED")
      (let ((why (alist-get 'blockedReason phase)))
        (format "Sending is refused: the phase is BLOCKED%s."
                (if why (format " (%s)" why) ""))))
     ((not alive)
      "Cannot deliver input: no conductor is running for this run (resume it to deliver).")
     ((equal name "AWAITING_OWNER")
      (format "Sending corrects the phase: resolves %d open owner request(s), grants 3 repair rounds, starts a repair attempt with your text verbatim."
              (length requests)))
     ((member name '("IMPLEMENTING" "FREEZING"))
      (format "Sending steers worker attempt %s now (at most once; C-c C-c or RET)." attempt))
     (t
      (format "Sending notes the next worker attempt (phase %s)." (or name "?"))))))

(defun +tt-input-send ()
  "Queue the input buffer's text as the owner input its phase calls for.
Design §7.4/§7.5: a steer to a running worker, a correction while the
phase is AWAITING_OWNER, a note otherwise; refused after DONE or BLOCKED,
with the reason shown here."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (s (ignore-errors (+tt--state +tt--run-dir)))
         (phase (and s (+tt--get s 'state 'phase)))
         (name (and phase (alist-get 'phase phase))))
    (when (string-empty-p text) (user-error "Nothing to send"))
    (unless s (user-error "Cannot read this run's state"))
    (cond
     ((equal name "DONE")
      (user-error "Refused: the run is DONE; it accepts no further owner input"))
     ((equal name "BLOCKED")
      (let ((why (alist-get 'blockedReason phase)))
        (user-error "Refused: the phase is BLOCKED%s" (if why (format " (%s)" why) ""))))
     (t
      (let* ((binding `((runId . ,(+tt--get phase 'runId)) (phaseId . ,(+tt--get phase 'phaseId))))
             (kind (cond ((equal name "AWAITING_OWNER") "correction")
                         ((member name '("IMPLEMENTING" "FREEZING")) "steer")
                         (t "note")))
             (id (+tt--write-command
                  +tt--run-dir
                  `((type . ,kind) (text . ,text) (binding . ,binding)))))
        (erase-buffer)
        (message "tradeoffs-trace: %s queued in the inbox (%s); see Owner input in the status buffer" kind id))))))

(defun +tt-input-ret ()
  "RET in the input buffer: send in Evil normal state, else insert a newline."
  (interactive)
  (if (eq (and (boundp 'evil-state) (symbol-value 'evil-state)) 'insert)
      (newline)
    (+tt-input-send)))

(defvar-keymap +tt-input-mode-map
  "C-c C-c" #'+tt-input-send
  "RET" #'+tt-input-ret)

(define-derived-mode +tt-input-mode text-mode "tt-input"
  "Owner input for a tradeoffs-trace run.  \\<+tt-input-mode-map>\\[+tt-input-send] sends."
  (visual-line-mode 1)
  ;; In Evil normal state RET must send; in insert state it must insert a
  ;; newline (the mode-map RET binding covers emacs/insert, this covers
  ;; normal). Guarded so loading this file never requires Evil.
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +tt-input-mode-map (kbd "RET") #'+tt-input-send)))

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

;; Read-only (plan 3b): only decisions, each a self-contained block, for the
;; current round; earlier rounds collapse to one line each.  The owner
;; intervenes only through the input box.  Every state label comes from the
;; tally (`decisionStatuses' in `tt state'), never from individual ballots.

(defun +tt--decision-label (status d phase)
  "Heading label for decision D with tally STATUS in PHASE."
  (let ((dissent (seq-some (lambda (b) (and (equal (alist-get 'decisionId b) (alist-get 'id d))
                                            (equal (alist-get 'vote b) "reject")))
                           (alist-get 'ballots phase))))
    (concat
     (pcase (alist-get 'status status)
       ("passed" (if dissent "ACCEPTED with dissent" "ACCEPTED"))
       ("failed" (format "REJECTED (%s)" (or (alist-get 'reason status) "vote failed")))
       ("suspended" "SUSPENDED")
       ("owner" "NEEDS YOU")
       ("detail" "DETAIL")
       (_ "PENDING"))
     ;; A reserved decision: voted like any other, flagged for the owner.
     (if (eq (alist-get 'flagged status) t) " ⚑ FLAGGED" ""))))

(defun +tt--decision-block (d status phase)
  "Insert decision D (tally STATUS) as one self-contained Org entry."
  (let* ((choice (or (alist-get 'choice d) ""))
         (short (truncate-string-to-width (+tt--one-line choice 200) 80 nil nil "…"))
         (source (alist-get 'source d))
         (seen (alist-get 'alsoSeenBy d))
         (rec (alist-get 'recommendation d))
         (chosen (and rec (alist-get 'choice rec)))
         (ballots (seq-filter (lambda (b) (equal (alist-get 'decisionId b) (alist-get 'id d)))
                              (alist-get 'ballots phase))))
    (insert (format "* %s  %s\n" (+tt--decision-label status d phase) short))
    (unless (equal short (+tt--one-line choice 200)) (insert (format "  %s\n" choice)))
    (insert (format "  raised by %s%s\n"
                    (if (equal source "reviewer-discovered")
                        (format "reviewer %s (discovered)"
                                (or (alist-get 'discoveredBy d)
                                    (and (string-match "-\\([MAB]\\)-[0-9]+\\'" (alist-get 'id d))
                                         (match-string 1 (alist-get 'id d)))
                                    "?"))
                      "the worker")
                    (if seen (format "; also seen by %s" (string-join seen ", ")) "")))
    (insert (format "  Why it matters: %s\n" (alist-get 'whyItMatters d)))
    (dolist (a (alist-get 'alternatives d))
      (let ((opt (alist-get 'option a)))
        (insert (format "  %s %s — %s\n" (if (and chosen (string-prefix-p (downcase opt) (downcase chosen))) "●" "○")
                        opt (alist-get 'consequence a)))))
    (when rec
      (insert (format "  Recommendation: %s. %s\n" (alist-get 'choice rec) (or (alist-get 'reason rec) ""))))
    (dolist (b ballots)
      (insert (format "  %s %s — %s\n" (alist-get 'reviewer b) (alist-get 'vote b)
                      (+tt--one-line (alist-get 'rationale b) 160))))
    (insert "\n")))

(defun +tt--finding-location (f)
  "The file part of finding F's evidence, or \"(general)\"."
  (let ((ev (or (alist-get 'evidence f) "")))
    (if (string-match "\\([[:alnum:]_./-]+\\.[[:alnum:]]+\\)\\(:[0-9]+\\)?" ev)
        (match-string 1 ev)
      "(general)")))

(defun +tt--render-findings (findings)
  "Insert FINDINGS grouped by location, evidence folded under each."
  (let ((groups nil))
    (dolist (f findings)
      (let ((loc (+tt--finding-location f)))
        (setf (alist-get loc groups nil nil #'equal) (append (alist-get loc groups nil nil #'equal) (list f)))))
    (dolist (g (nreverse groups))
      (insert (format "* %s\n" (car g)))
      (dolist (f (cdr g))
        (let* ((ev (or (alist-get 'evidence f) ""))
               (first (car (split-string ev "\\. " t))))
          (insert (format "** %s %s — %s%s\n" (upcase (or (alist-get 'severity f) ""))
                          (+tt--one-line first 90)
                          (alist-get 'raisedBy f)
                          (let ((also (alist-get 'alsoRaisedBy f)))
                            (if also (format "; also raised by %s" (string-join also ", ")) ""))))
          (insert (format "   %s\n" ev)))))
    (insert "\n")))

(defun +tt--render-decisions (s)
  "Insert the decision view for state S."
  (let* ((phase (+tt--get s 'state 'phase))
         (v (alist-get 'view s))
         (statuses (alist-get 'decisionStatuses s))
         (candidate (+tt--get phase 'candidate 'sha))
         (current (seq-filter (lambda (d)
                                (and (not (equal (alist-get 'source d) "trigger"))
                                     (not (equal (alist-get 'status (alist-get (intern (alist-get 'id d)) statuses))
                                                 "superseded"))))
                              (alist-get 'decisions phase)))
         (findings (seq-filter (lambda (f) (equal (alist-get 'status f) "open")) (alist-get 'findings phase)))
         (addressing (alist-get 'addressing v))
         (needs (alist-get 'needsYou v)))
    (insert (format "#+TITLE: decisions — %s\n" (+tt--get s 'meta 'title)))
    (insert (format "Round %s · candidate %s · attempt %s%s\n"
                    (alist-get 'round v) (if candidate (substring candidate 0 7) "none yet")
                    (+tt--get phase 'attempt 'n)
                    (if addressing (format " — addressing: %s" (string-join addressing "; ")) "")))
    (insert (format "Reviews: %s\n" (alist-get 'reviewLine v)))
    (when (alist-get 'verdict v) (insert (format "Verdict: %s\n" (alist-get 'verdict v))))
    (when (and needs (> needs 0))
      (insert (format "⚑ %d owner request(s) open — type a correction in the input box\n" needs)))
    (insert "\n")
    (if current
        (dolist (d current)
          (+tt--decision-block d (alist-get (intern (alist-get 'id d)) statuses) phase))
      (insert "No decisions on this candidate yet.\n\n"))
    (when findings
      (insert (format "Findings (%d open)\n" (length findings)))
      (+tt--render-findings findings))
    (let ((rounds (alist-get 'rounds v)))
      (when rounds
        (insert "Earlier rounds\n")
        (dolist (r rounds)
          (insert (format "* Round %s · %s · %s\n" (alist-get 'round r)
                          (substring (alist-get 'candidateSha r) 0 7) (alist-get 'outcome r))))))))

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
      (setq +tt--run-dir run)
      (org-content 1)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun +tt-decisions-refresh ()
  "Re-render this decision view, keeping point roughly in place."
  (interactive)
  (let ((pt (point)) (run +tt--run-dir))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (+tt--render-decisions (+tt--state run)))
    (setq +tt--run-dir run)
    (org-content 1)
    (goto-char (min pt (point-max)))))

(defvar-keymap +tt-decisions-mode-map
  "g" #'+tt-decisions-refresh
  "TAB" #'org-cycle
  "q" #'quit-window)

(define-derived-mode +tt-decisions-mode org-mode "tt-decisions"
  "Read-only decision view of a tradeoffs-trace run.
\\<+tt-decisions-mode-map>\\[+tt-decisions-refresh] refreshes, \\[org-cycle] folds, \\[quit-window] quits.
To intervene, type into the run's input box."
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +tt-decisions-mode-map
      "g" #'+tt-decisions-refresh (kbd "TAB") #'org-cycle "q" #'quit-window)))

;;;; Runs list and mode line

;; Plan 3b: one place to see every run (`C-c m l'), and a display-only
;; mode-line indicator while any run is active.  Both come from one
;; `tt list --json' call, never one `tt state' per run.

(defun +tt--list ()
  "Parsed `tt list --json'."
  (json-parse-string (+tt--cli "list" "--json") :object-type 'alist :array-type 'list
                     :null-object nil :false-object :false))

(defun +tt--attention-face (row)
  "Face for ROW's attention, or nil."
  (when (alist-get 'attention row) 'error))

(defun +tt--runs-entries ()
  "Tabulated-list entries for every run."
  (mapcar (lambda (r)
            (let ((face (+tt--attention-face r)))
              (list (alist-get 'runDir r)
                    (vector (alist-get 'id r)
                            (if (eq (alist-get 'alive r) t) "●" "○")
                            (alist-get 'stage r)
                            (alist-get 'stageElapsed r)
                            (alist-get 'reviews r)
                            (if (alist-get 'attention r)
                                (propertize (concat "⚑ " (alist-get 'attention r)) 'face face)
                              "")
                            (alist-get 'title r)))))
          (+tt--list)))

(defun +tt-runs-open ()
  "Open the workspace of the run at point."
  (interactive)
  (when-let* ((run (tabulated-list-get-id))) (+tt--workspace run)))

(defun +tt-runs-stop ()
  "Stop the conductor of the run at point (`tt stop')."
  (interactive)
  (when-let* ((run (tabulated-list-get-id)))
    (when (y-or-n-p (format "Stop run %s? " (file-name-nondirectory run)))
      (message "%s" (+tt--cli "stop" run))
      (tabulated-list-revert))))

(defun +tt-runs-resume ()
  "Relaunch the conductor of the run at point (`tt resume')."
  (interactive)
  (when-let* ((run (tabulated-list-get-id)))
    (+tt--cli "resume" run)
    (message "tradeoffs-trace: conductor relaunched for %s" (file-name-nondirectory run))))

(defvar-keymap +tt-runs-mode-map
  :parent tabulated-list-mode-map
  "RET" #'+tt-runs-open
  "k" #'+tt-runs-stop
  "R" #'+tt-runs-resume)

(define-derived-mode +tt-runs-mode tabulated-list-mode "tt-runs"
  "Every tradeoffs-trace run.  \\<+tt-runs-mode-map>\\[+tt-runs-open] opens, \\[+tt-runs-stop] stops, \\[+tt-runs-resume] resumes, g refreshes."
  (setq tabulated-list-format [("run" 9 t) ("" 1 nil) ("stage" 10 t) ("for" 7 nil)
                               ("reviews" 26 nil) ("attention" 20 t) ("title" 0 t)]
        tabulated-list-entries #'+tt--runs-entries)
  (tabulated-list-init-header)
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +tt-runs-mode-map
      (kbd "RET") #'+tt-runs-open "k" #'+tt-runs-stop "R" #'+tt-runs-resume "g" #'tabulated-list-revert)))

;;;###autoload
(defun +tt-runs ()
  "List every tradeoffs-trace run."
  (interactive)
  (let ((buf (get-buffer-create "*tt-runs*")))
    (with-current-buffer buf
      (+tt-runs-mode)
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defvar +tt--mode-line-string ""
  "The mode-line indicator text (display only).")
(put '+tt--mode-line-string 'risky-local-variable t)

(defvar +tt--mode-line-timer nil)

(defun +tt--live-run-p (run-dir)
  "Non-nil when RUN-DIR's conductor process is alive (no Node call)."
  (let* ((f (expand-file-name "conductor.pid" run-dir))
         (pid (and (file-exists-p f)
                   (string-to-number (with-temp-buffer (insert-file-contents f) (buffer-string))))))
    (and pid (> pid 0) (process-attributes pid) t)))

(defun +tt--mode-line-update ()
  "Refresh the mode-line indicator from `tt list' when any run is live."
  (setq +tt--mode-line-string
        (if (not (seq-some #'+tt--live-run-p (ignore-errors (+tt--runs))))
            ""
          (let ((rows (seq-filter (lambda (r) (or (eq (alist-get 'alive r) t) (equal (alist-get 'attention r) "needs you")))
                                  (ignore-errors (+tt--list)))))
            (if (null rows) ""
              (concat " ["
                      (mapconcat
                       (lambda (r)
                         (propertize (format "tt:%s %s %s %s" (substring (alist-get 'id r) 0 4)
                                             (alist-get 'stage r) (alist-get 'stageElapsed r)
                                             (replace-regexp-in-string " +" "" (alist-get 'reviews r)))
                                     'face (+tt--attention-face r)))
                       rows " | ")
                      "]")))))
  (force-mode-line-update t))

(defun +tt--ensure-mode-line ()
  "Install the display-only mode-line indicator and its timer."
  (unless (memq '+tt--mode-line-string global-mode-string)
    (setq global-mode-string (append global-mode-string '(+tt--mode-line-string))))
  (unless (timerp +tt--mode-line-timer)
    (setq +tt--mode-line-timer (run-with-timer 1 10 #'+tt--mode-line-update))))

;;;; Keys

(keymap-global-set "C-c m r" #'+tt-run)
(keymap-global-set "C-c m s" #'+tt-show)
(keymap-global-set "C-c m d" #'+tt-decisions)
(keymap-global-set "C-c m l" #'+tt-runs)
(keymap-global-set "C-c m p" #'+tt-program)
(+tt--ensure-mode-line)

(provide 'init-tradeoffs-trace)
;;; init-tradeoffs-trace.el ends here
