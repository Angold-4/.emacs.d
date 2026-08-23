;;; git-review-baseline.el --- Measure Magit baseline timings -*- lexical-binding: t -*-

;;; Commentary:
;; Test-local helpers for Phase 0 baseline measurements.
;;
;; Guarantees while measuring:
;; - Temporary process/URL advice is always removed via `unwind-protect'.
;; - A hard network guard is active for both prewarm and timed samples.
;; - Prewarm is never timed and never contributes to process statistics.
;; - Nested process APIs (e.g. process-file -> call-process) record once.
;; - Timings are prewarmed, then sampled repeatedly; reports use median and
;;   min/max range so later phases can apply a 20% regression threshold.
;; - Fixture-owned Magit buffers are killed and the prior window
;;   configuration is restored before a temporary repository is deleted.
;;
;; Interactive entry points:
;;   M-x git-review-baseline-run-manual RET
;;   M-x git-review-baseline-run-on-large-fixture RET

;;; Code:

(require 'cl-lib)
(require 'git-review-fixtures)

(defconst git-review-baseline-sample-count 5
  "Number of timed samples collected for each warm metric after prewarm.")

(defvar git-review-baseline--process-log nil
  "List of (TIME PROGRAM ARGS) recorded during instrumentation.")

(defvar git-review-baseline--blocked-attempts nil
  "List of (KIND . DETAIL) for attempts stopped by the network guard.")

(defvar git-review-baseline--guard-active nil
  "Non-nil while the hard network guard advice is installed.")

(defvar git-review-baseline--recording-p nil
  "Non-nil while child processes should be appended to the process log.
Prewarm runs with the guard active but with recording disabled.")

(defvar git-review-baseline--record-depth 0
  "Nesting depth for process API advice; only depth 0 records an entry.")

(defconst git-review-baseline--network-argv-markers
  '("api.github.com" "http://" "https://")
  "Argv substrings that always count as intentional remote access.")

(defconst git-review-baseline--network-programs
  '("curl" "wget" "gh")
  "Program basenames treated as network clients when invoked.")

(defconst git-review-baseline--git-network-subcommands
  '("fetch" "pull" "push" "clone" "ls-remote")
  "Git subcommands that always contact a remote.")

(defconst git-review-baseline--git-remote-mutating-subcommands
  '("update" "prune" "set-url" "add" "remove" "rename")
  "git-remote subcommands treated as intentional remote mutation.")

(defconst git-review-baseline--git-globals-with-arg
  '("-c" "-C" "--git-dir" "--work-tree" "--namespace"
    "--config-env" "--super-prefix")
  "Git global options that consume the following argv token.")

(defun git-review-baseline--median (numbers)
  "Return the median of NUMBERS (a non-empty list of numbers)."
  (let* ((sorted (sort (copy-sequence numbers) #'<))
         (n (length sorted))
         (mid (/ n 2)))
    (if (cl-oddp n)
        (nth mid sorted)
      (/ (+ (nth (1- mid) sorted) (nth mid sorted)) 2.0))))

(defun git-review-baseline--stats (samples)
  "Return plist describing SAMPLE seconds: :samples :median :min :max :count."
  (list :samples (append samples nil)
        :count (length samples)
        :median (git-review-baseline--median samples)
        :min (apply #'min samples)
        :max (apply #'max samples)))

(defun git-review-baseline--git-tokens-after-globals (args)
  "Strip Magit/Git global options from ARGS; return remaining tokens.
Handles prefixes such as `--no-pager`, `-c key=value`, and `-C /path'."
  (let ((tokens (mapcar (lambda (a) (format "%s" a)) args)))
    (catch 'done
      (while tokens
        (let ((tok (car tokens)))
          (cond
           ((member tok git-review-baseline--git-globals-with-arg)
            (setq tokens (nthcdr 2 tokens)))
           ((or (string-prefix-p "--" tok)
                ;; Glued form: -ccore.foo=bar
                (string-match-p "\\`-c." tok)
                (string-prefix-p "-" tok))
            (setq tokens (cdr tokens)))
           (t
            (throw 'done tokens)))))
      nil)))

(defun git-review-baseline--git-subcommand (args)
  "Return the Git subcommand in ARGS after skipping global options."
  (car (git-review-baseline--git-tokens-after-globals args)))

(defun git-review-baseline--git-remote-mutating-p (args)
  "Return non-nil when ARGS is a mutating `git remote ...' invocation."
  (let* ((tokens (git-review-baseline--git-tokens-after-globals args))
         (sub (car tokens))
         (action (cadr tokens)))
    (and (equal sub "remote")
         (member action git-review-baseline--git-remote-mutating-subcommands))))

(defun git-review-baseline--argv-network-p (program args)
  "Return non-nil if PROGRAM/ARGS looks like intentional remote access."
  (let* ((base (file-name-nondirectory (or program "")))
         (arg-strings (mapcar (lambda (a) (format "%s" a)) args))
         (git-sub (and (string= base "git")
                       (git-review-baseline--git-subcommand arg-strings))))
    (or (member base git-review-baseline--network-programs)
        (cl-some
         (lambda (tok)
           (cl-some (lambda (m)
                      (string-match-p (regexp-quote m) tok))
                    git-review-baseline--network-argv-markers))
         arg-strings)
        (and (string= base "git")
             (or (member git-sub git-review-baseline--git-network-subcommands)
                 (git-review-baseline--git-remote-mutating-p arg-strings))))))

(defun git-review-baseline--block-network (kind detail)
  "Record and signal a blocked network attempt of KIND with DETAIL."
  (push (cons kind detail) git-review-baseline--blocked-attempts)
  (error "git-review-baseline: blocked network attempt (%s): %S"
         kind detail))

(defun git-review-baseline--guard-process (program args)
  "Block PROGRAM ARGS when the hard network guard is active."
  (when (and git-review-baseline--guard-active
             (git-review-baseline--argv-network-p program args))
    (git-review-baseline--block-network 'process (cons program args))))

(defun git-review-baseline--record-process (program args)
  "Append PROGRAM and ARGS to `git-review-baseline--process-log'."
  (push (list (float-time) program (append args nil))
        git-review-baseline--process-log))

(defun git-review-baseline--invoke-process (program args continue)
  "Guard PROGRAM ARGS, record at most once for nested APIs, then CONTINUE.
`process-file' often calls `call-process' internally; only the outermost
advised entry is logged when `git-review-baseline--recording-p' is non-nil."
  (git-review-baseline--guard-process program args)
  (if (and git-review-baseline--recording-p
           (zerop git-review-baseline--record-depth))
      (progn
        (git-review-baseline--record-process program args)
        (let ((git-review-baseline--record-depth 1))
          (funcall continue)))
    (funcall continue)))

(defun git-review-baseline--wrap-call-process (orig program &rest rest)
  "Advice around `call-process' that guards and records PROGRAM."
  (let ((args (nthcdr 3 rest)))
    (git-review-baseline--invoke-process
     program args
     (lambda () (apply orig program rest)))))

(defun git-review-baseline--wrap-process-file (orig program &rest rest)
  "Advice around `process-file' that guards and records PROGRAM."
  ;; Signature: (PROGRAM &optional INFILE BUFFER DISPLAY &rest ARGS)
  (let ((args (nthcdr 3 rest)))
    (git-review-baseline--invoke-process
     program args
     (lambda () (apply orig program rest)))))

(defun git-review-baseline--wrap-call-process-region (orig start end program &rest rest)
  "Advice around `call-process-region' that guards and records PROGRAM."
  ;; Signature: (START END PROGRAM &optional DELETE BUFFER DISPLAY &rest ARGS)
  (let ((args (nthcdr 3 rest)))
    (git-review-baseline--invoke-process
     program args
     (lambda () (apply orig start end program rest)))))

(defun git-review-baseline--wrap-start-process (orig name buffer program &rest args)
  "Advice around `start-process' that guards and records PROGRAM."
  (git-review-baseline--invoke-process
   program args
   (lambda () (apply orig name buffer program args))))

(defun git-review-baseline--wrap-make-process (orig &rest spec)
  "Advice around `make-process' that guards and records :command."
  (let* ((command (plist-get spec :command))
         (program (car command))
         (args (cdr command)))
    (if program
        (git-review-baseline--invoke-process
         program args
         (lambda () (apply orig spec)))
      (apply orig spec))))

(defun git-review-baseline--wrap-url-retrieve (orig url &rest rest)
  "Advice that blocks `url-retrieve' while the network guard is active."
  (if git-review-baseline--guard-active
      (git-review-baseline--block-network 'url-retrieve url)
    (apply orig url rest)))

(defun git-review-baseline--wrap-url-retrieve-synchronously (orig url &rest rest)
  "Advice that blocks `url-retrieve-synchronously' under the network guard."
  (if git-review-baseline--guard-active
      (git-review-baseline--block-network 'url-retrieve-synchronously url)
    (apply orig url rest)))

(defun git-review-baseline--wrap-open-network-stream (orig &rest args)
  "Advice that blocks `open-network-stream' under the network guard."
  (if git-review-baseline--guard-active
      (git-review-baseline--block-network 'open-network-stream args)
    (apply orig args)))

(defun git-review-baseline--wrap-make-network-process (orig &rest spec)
  "Advice that blocks `make-network-process' under the network guard."
  (if git-review-baseline--guard-active
      (git-review-baseline--block-network 'make-network-process spec)
    (apply orig spec)))

(defun git-review-baseline--advice-pairs ()
  "Return ((SYMBOL . FUNCTION) ...) for all temporary baseline advice."
  (list (cons 'call-process #'git-review-baseline--wrap-call-process)
        (cons 'process-file #'git-review-baseline--wrap-process-file)
        (cons 'call-process-region #'git-review-baseline--wrap-call-process-region)
        (cons 'start-process #'git-review-baseline--wrap-start-process)
        (cons 'make-process #'git-review-baseline--wrap-make-process)
        (cons 'url-retrieve #'git-review-baseline--wrap-url-retrieve)
        (cons 'url-retrieve-synchronously
              #'git-review-baseline--wrap-url-retrieve-synchronously)
        (cons 'open-network-stream
              #'git-review-baseline--wrap-open-network-stream)
        (cons 'make-network-process
              #'git-review-baseline--wrap-make-network-process)))

(defun git-review-baseline--advice-installed-p ()
  "Return non-nil if any baseline measurement advice is still installed."
  (cl-some (lambda (pair)
             (advice-member-p (cdr pair) (car pair)))
           (git-review-baseline--advice-pairs)))

(defun git-review-baseline--with-instrumentation (fn)
  "Call FN with a hard network guard installed.
Always removes advice afterward.  Return (RESULT . PROCESS-LOG).
FN controls whether processes are recorded via
`git-review-baseline--recording-p' (nil during prewarm)."
  (setq git-review-baseline--process-log nil
        git-review-baseline--blocked-attempts nil
        git-review-baseline--guard-active t
        git-review-baseline--recording-p nil
        git-review-baseline--record-depth 0)
  (dolist (pair (git-review-baseline--advice-pairs))
    (advice-add (car pair) :around (cdr pair)))
  (unwind-protect
      (let ((result (funcall fn)))
        (cons result (nreverse git-review-baseline--process-log)))
    (setq git-review-baseline--guard-active nil
          git-review-baseline--recording-p nil
          git-review-baseline--record-depth 0)
    (dolist (pair (git-review-baseline--advice-pairs))
      (advice-remove (car pair) (cdr pair)))
    (setq git-review-baseline--process-log nil)))

(defun git-review-baseline--classify-program (program)
  "Return a short label for PROGRAM: git, delta, difft, or other."
  (let ((base (file-name-nondirectory (or program ""))))
    (cond
     ((string= base "git") 'git)
     ((string-match-p "\\`delta\\'" base) 'delta)
     ((string-match-p "\\`difft" base) 'difft)
     (t 'other))))

(defun git-review-baseline--format-argv (program args)
  "Format PROGRAM and ARGS as a single shell-like string."
  (format "%s %s"
          program
          (mapconcat (lambda (a) (format "%s" a)) args " ")))

(defun git-review-baseline--summarize-processes (log)
  "Return a plist summarizing process kinds in LOG.
Any `other' processes include their full program/argv in :other-processes."
  (let ((counts '((git . 0) (delta . 0) (difft . 0) (other . 0)))
        (samples nil)
        (others nil))
    (dolist (entry log)
      (let* ((program (nth 1 entry))
             (args (nth 2 entry))
             (kind (git-review-baseline--classify-program program))
             (formatted (git-review-baseline--format-argv program args)))
        (setf (alist-get kind counts) (1+ (alist-get kind counts)))
        (when (eq kind 'other)
          (push formatted others))
        (when (< (length samples) 12)
          (push formatted samples))))
    (list :counts counts
          :samples (nreverse samples)
          :other-processes (nreverse others)
          :blocked-attempts (append git-review-baseline--blocked-attempts nil))))

(defun git-review-baseline--time-form (form)
  "Evaluate FORM and return elapsed seconds as a float."
  (let* ((start (current-time))
         (_value (funcall form)))
    (float-time (time-subtract (current-time) start))))

(defun git-review-baseline--status-buffer ()
  "Return the current Magit status buffer, or nil."
  (magit-get-mode-buffer 'magit-status-mode))

(defun git-review-baseline--ensure-status (root)
  "Ensure a Magit status buffer exists for ROOT and return it."
  (let ((default-directory (file-name-as-directory (expand-file-name root))))
    (magit-status-setup-buffer default-directory)
    (or (git-review-baseline--status-buffer)
        (error "Failed to open Magit status for %s" root))))

(defun git-review-baseline--refresh-status ()
  "Refresh the current Magit status buffer."
  (with-current-buffer (git-review-baseline--status-buffer)
    (magit-refresh)))

(defun git-review-baseline--open-diff ()
  "Open a unified working-tree Magit diff."
  (magit-diff-working-tree nil)
  t)

(defun git-review-baseline--measure-metric (prewarm-fn sample-fn &optional samples)
  "Prewarm with PREWARM-FN, then time SAMPLE-FN SAMPLES times.
The hard network guard is active for both prewarm and samples.  Prewarm
is excluded from timings and process statistics.  Return
plist (:stats STATS :processes SUMMARY)."
  (let ((n (or samples git-review-baseline-sample-count)))
    (pcase-let*
        ((`(,times . ,log)
          (git-review-baseline--with-instrumentation
           (lambda ()
             ;; Guarded prewarm: no timing, no process recording.
             (let ((git-review-baseline--recording-p nil))
               (funcall prewarm-fn))
             ;; Guarded timed samples with process recording.
             (let ((git-review-baseline--recording-p t)
                   (acc nil))
               (dotimes (_ n)
                 (push (git-review-baseline--time-form sample-fn) acc))
               (nreverse acc)))))
         (stats (git-review-baseline--stats times))
         (procs (git-review-baseline--summarize-processes log)))
      (list :stats stats :processes procs))))

(defun git-review-baseline--root-prefix (root)
  "Return an expanded directory prefix for ROOT suitable for string tests."
  (file-name-as-directory (expand-file-name root)))

(defun git-review-baseline--buffer-under-root-p (root)
  "Return non-nil when the current buffer's default-directory is under ROOT."
  (let ((prefix (git-review-baseline--root-prefix root))
        (dd default-directory))
    (and (stringp dd)
         (string-prefix-p prefix (file-name-as-directory (expand-file-name dd))))))

(defun git-review-baseline--magit-buffers-for-root (root)
  "Return Magit buffers whose default-directory lives under ROOT."
  (cl-loop for buf in (buffer-list)
           when (with-current-buffer buf
                  (and (derived-mode-p 'magit-mode)
                       (git-review-baseline--buffer-under-root-p root)))
           collect buf))

(defun git-review-baseline-cleanup-repo-buffers (root)
  "Kill Magit buffers belonging to ROOT and return the killed buffer names.
Safe to call when ROOT has already been deleted: matching uses the buffer
local `default-directory' string prefix, not a live filesystem check."
  (let ((killed nil))
    (dolist (buf (git-review-baseline--magit-buffers-for-root root))
      (push (buffer-name buf) killed)
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (nreverse killed)))

(defun git-review-baseline-measure-repo (root &optional samples)
  "Measure Magit status/diff/refresh timings in ROOT.

Each metric is prewarmed once under the hard network guard (untimed, not
logged), then sampled SAMPLES times (default
`git-review-baseline-sample-count').  Reported values are median/min/max.

Window counts require a live interactive frame; batch Emacs leaves those
fields nil rather than inventing numbers.

Local Git read operations are allowed; fetch/pull/push/clone/URL/network
streams and Magit-prefixed `git remote update' error."
  (require 'magit)
  (let* ((default-directory (file-name-as-directory (expand-file-name root)))
         (n (or samples git-review-baseline-sample-count))
         (display-p (display-graphic-p))
         (tty-windows-p (and (not noninteractive) (not display-p)))
         (can-count-windows (and (not noninteractive)
                                 (frame-live-p (selected-frame))))
         (windows-before (when can-count-windows (length (window-list))))
         status-result
         refresh-result
         diff-result
         windows-after-status
         windows-after-diff)
    ;; Status: prewarm create, then time warm re-entry via setup-buffer.
    (setq status-result
          (git-review-baseline--measure-metric
           (lambda () (git-review-baseline--ensure-status root))
           (lambda ()
             (git-review-baseline--ensure-status root)
             t)
           n))
    (setq windows-after-status (when can-count-windows (length (window-list))))

    ;; Refresh: status already exists; prewarm one refresh, then sample.
    (setq refresh-result
          (git-review-baseline--measure-metric
           (lambda ()
             (git-review-baseline--ensure-status root)
             (git-review-baseline--refresh-status))
           #'git-review-baseline--refresh-status
           n))

    ;; Diff: prewarm open, then sample re-open of working-tree diff.
    (setq diff-result
          (git-review-baseline--measure-metric
           (lambda ()
             (git-review-baseline--ensure-status root)
             (git-review-baseline--open-diff))
           #'git-review-baseline--open-diff
           n))
    (setq windows-after-diff (when can-count-windows (length (window-list))))

    (list :root root
          :interactive (not noninteractive)
          :graphic display-p
          :tty tty-windows-p
          :sample-count n
          :prewarmed t
          :windows-before windows-before
          :windows-after-status windows-after-status
          :windows-after-diff windows-after-diff
          :status (plist-get status-result :stats)
          :refresh (plist-get refresh-result :stats)
          :diff (plist-get diff-result :stats)
          :status-processes (plist-get status-result :processes)
          :refresh-processes (plist-get refresh-result :processes)
          :diff-processes (plist-get diff-result :processes)
          :network-guard 'hard
          :network-blocked-attempts
          (append (plist-get (plist-get status-result :processes)
                             :blocked-attempts)
                  (plist-get (plist-get refresh-result :processes)
                             :blocked-attempts)
                  (plist-get (plist-get diff-result :processes)
                             :blocked-attempts)))))

(defun git-review-baseline--format-stats (label stats)
  "Format STATS plist for LABEL as a single report line."
  (format "- warm %s: median %.3fs (min %.3fs, max %.3fs, n=%d)"
          label
          (plist-get stats :median)
          (plist-get stats :min)
          (plist-get stats :max)
          (plist-get stats :count)))

(defun git-review-baseline--format-process-summary (label summary)
  "Format process SUMMARY under LABEL, including any other argv details."
  (let* ((c (plist-get summary :counts))
         (others (plist-get summary :other-processes))
         (line (format "- processes during %s: git=%s delta=%s difft=%s other=%s"
                       label
                       (alist-get 'git c)
                       (alist-get 'delta c)
                       (alist-get 'difft c)
                       (alist-get 'other c))))
    (if others
        (concat line
                "\n"
                (mapconcat (lambda (cmd)
                             (format "  - other argv: `%s`" cmd))
                           others
                           "\n"))
      line)))

(defun git-review-baseline--format-report (plist)
  "Format measurement PLIST as text for docs/git-baseline.md."
  (cl-labels
      ((fmt-win (v)
         (if v (format "%s" v) "PENDING OWNER MANUAL RUN")))
    (string-join
     (list
      (format "- repository: `%s`" (plist-get plist :root))
      (format "- interactive: %s" (plist-get plist :interactive))
      (format "- graphic-frame: %s" (plist-get plist :graphic))
      (format "- prewarmed: %s" (plist-get plist :prewarmed))
      (format "- samples per metric: %s" (plist-get plist :sample-count))
      (format "- network guard: %s" (plist-get plist :network-guard))
      (format "- windows before Magit status: %s"
              (fmt-win (plist-get plist :windows-before)))
      (format "- windows after Magit status: %s"
              (fmt-win (plist-get plist :windows-after-status)))
      (format "- windows after unified diff: %s"
              (fmt-win (plist-get plist :windows-after-diff)))
      (git-review-baseline--format-stats
       "Magit status" (plist-get plist :status))
      (git-review-baseline--format-stats
       "`magit-refresh`" (plist-get plist :refresh))
      (git-review-baseline--format-stats
       "unified working-tree diff" (plist-get plist :diff))
      (git-review-baseline--format-process-summary
       "status samples" (plist-get plist :status-processes))
      (git-review-baseline--format-process-summary
       "refresh samples" (plist-get plist :refresh-processes))
      (git-review-baseline--format-process-summary
       "diff samples" (plist-get plist :diff-processes))
      (format "- network attempts blocked during measurement: %d"
              (length (plist-get plist :network-blocked-attempts))))
     "\n")))

;;;###autoload
(defun git-review-baseline-run-manual (&optional root)
  "Measure Magit baseline timings and print a pasteable report.
When ROOT is nil, use `default-directory'.  Intended for an interactive
graphical or tmux Emacs session owned by the human operator."
  (interactive)
  (let* ((dir (or root default-directory))
         (report (git-review-baseline-measure-repo dir))
         (text (git-review-baseline--format-report report)))
    (with-current-buffer (get-buffer-create "*git-review-baseline*")
      (erase-buffer)
      (insert "# Git Review Baseline Measurement\n\n")
      (insert text)
      (insert "\n")
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message "Baseline report written to *git-review-baseline*")
    report))

;;;###autoload
(defun git-review-baseline-run-on-large-fixture ()
  "Build a temporary large fixture, measure it, then delete it cleanly.
Restores the previous window configuration and kills fixture-owned Magit
buffers before removing the temporary directory."
  (interactive)
  (let* ((parent nil)
         (root nil)
         (wconf (and (not noninteractive)
                     (current-window-configuration))))
    (unwind-protect
        (progn
          (setq root (git-review-fixtures-make-temp-root "large-baseline"))
          (setq parent (file-name-directory (directory-file-name root)))
          (git-review-fixtures--init-repo root)
          (git-review-fixtures-create-large root 100)
          (git-review-baseline-run-manual root))
      (when root
        (git-review-baseline-cleanup-repo-buffers root))
      (when wconf
        (set-window-configuration wconf))
      (when (and parent (file-directory-p parent))
        (ignore-errors (delete-directory parent t))))))

(provide 'git-review-baseline)

;;; git-review-baseline.el ends here
