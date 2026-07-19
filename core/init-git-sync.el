;;; init-git-sync.el --- Shared mirror and explicit synchronization -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: One bare mirror per canonical repository; explicit async sync

;;; Commentary:
;; Phase 4 ownership for shared mirrors and explicit synchronization.
;;
;; Design:
;; - Cache root is `+git-store-registry-directory' (tests bind a temp dir).
;; - Mirror path: <cache>/<sha256(repository-id)>/mirror.git
;; - Sync prepares/fetches into candidate.git asynchronously, then publishes
;;   with a publish-intent + backup restore protocol so state-write failure
;;   never leaves a newer mirror visible under an older generation.
;; - Exclusive sync.lock.d directory lock with owner nonce.
;;   A clean Emacs exit releases locks owned by that process.  A later
;;   session may atomically reclaim a lock whose same-host PID is dead;
;;   live and malformed locks are never stolen.
;; - Per-run attempt IDs; sentinels/callbacks ignore stale attempts.
;; - Concurrency slots tracked explicitly (queued does not own a slot).
;; - Forge adapter returns a cancellable handle with success and error paths.
;;   Interception uses persistent around-advice so async chains stay covered.
;; - Async Git uses `make-process' with GIT_TERMINAL_PROMPT=0 (user Git
;;   configuration is preserved).
;; - Mutating publish recovery runs only under lock ownership; readers fall
;;   back to mirror.git.old while a live publication holds the lock.
;; - Private dependency: forge--pull (internal generic; isolated + tested).
;;
;; Ownership chain: init-git -> init-git-store -> init-git-sync -> init-git-ui

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'init-git-store)

(declare-function forge--pull "forge-commands" (repo &optional callback since))
(declare-function forge-get-repository "forge-repo")
(declare-function magit-refresh "magit-mode")
(declare-function magit-toplevel "magit-git")
(declare-function +git-review-target-p "init-git-ui" (obj))
(declare-function +git-review-target-repository-id "init-git-ui" (target))
(declare-function +git-review-target-context-id "init-git-ui" (target))
(declare-function +git-pr-repository-id "init-git-pr" (pr))

;; ---------------------------------------------------------------------------
;; Customization
;; ---------------------------------------------------------------------------

(defcustom +git-sync-active-repositories nil
  "Allowlist of canonical repository IDs for `+git/sync-all'.
An empty list means synchronize nothing (never \"all registered\")."
  :type '(repeat string)
  :group 'magit)

(defcustom +git-sync-max-concurrent 2
  "Maximum simultaneous repository sync jobs for sync-all."
  :type 'integer
  :group 'magit)

(defcustom +git-sync-stale-lock-seconds 1800
  "Age threshold for explicit `+git-sync-clear-stale-lock' maintenance.
Ordinary acquisition may reclaim a well-formed same-host lock immediately
when its owner PID is dead.  Live or unknown locks are never cleared."
  :type 'integer
  :group 'magit)

(defcustom +git-sync-stale-after-seconds 86400
  "Seconds after last success after which idle sync data is considered stale.
Zero disables age-based staleness."
  :type 'integer
  :group 'magit)

(defvar +git-sync--git-fetch-count 0
  "Number of remote `git fetch' processes started by sync.
Reset around instrumented tests.")

(defvar +git-sync--forge-pull-count 0
  "Number of Forge pull invocations started by sync.")

(defvar +git-sync--waiting-refresh-count 0
  "Number of waiting-buffer refresh calls after sync completion.")

(defvar +git-sync--attempt-counter 0
  "Monotonic counter for per-run attempt IDs.")

(defvar +git-sync-forge-pull-function #'+git-sync--forge-pull-default
  "Adapter called as (FN REPOSITORY-ID ON-COMPLETE) -> HANDLE.
ON-COMPLETE receives a status symbol (or (failed MSG)):
  ok | unavailable | untracked | unsupported | skipped | failed | cancelled
HANDLE is a plist with at least :cancel, a function of no args that
requests cancellation.  ON-COMPLETE must be invoked exactly once.
Tests may bind a stub.  Default isolates private `forge--pull'.")

;; ---------------------------------------------------------------------------
;; Job registry and state machine
;; ---------------------------------------------------------------------------

(defconst +git-sync--active-states
  '(queued preparing-mirror fetching-mirror pulling-forge
    fetching-checks publishing)
  "Job states that mean synchronization is in progress.")

(cl-defstruct (+git-sync-job
               (:constructor +git-sync-job--create)
               (:copier nil))
  "One per-repository asynchronous synchronization job."
  repository-id
  state                 ; idle | queued | ... | succeeded | failed | cancelled
  attempt-id            ; unique per run; sentinels must match
  started-at            ; float-time or nil
  finished-at
  process               ; Emacs process owned by this attempt, or nil
  cancel-flag
  previous-generation
  candidate-generation
  waiting-buffers       ; list of live buffers
  waiting-callbacks     ; list of functions
  last-success          ; float-time
  last-error            ; sanitized string
  forge-status          ; current | stale | unavailable | failed | skipped
  stale-p
  lock-held-p
  lock-nonce            ; unique owner token for the directory lock
  slot-held-p           ; non-nil when this job owns a concurrency slot
  persist-failure-p     ; nil when failure must not rewrite sync-state
  forge-request         ; cancellable handle plist from Forge adapter
  candidate-dir
  backup-dir            ; mirror.git.old retained until state publish succeeds
  remote-url            ; may contain credentials; never logged raw
  seed-root
  provider              ; github | gitlab | generic
  pr-refs-status)       ; ok | unavailable

(defvar +git-sync--jobs (make-hash-table :test #'equal)
  "Live map: repository-id -> `+git-sync-job'.")

(defvar +git-sync--global-active 0
  "Number of jobs currently past queued (holding work slots).")

(defvar +git-sync--global-waiters nil
  "FIFO of repository-ids waiting for a concurrency slot.")

(defun +git-sync-reset-jobs ()
  "Clear in-memory sync jobs.  Intended for tests."
  (maphash
   (lambda (_id job)
     (when-let ((proc (+git-sync-job-process job)))
       (when (process-live-p proc)
         (ignore-errors (delete-process proc)))))
   +git-sync--jobs)
  (clrhash +git-sync--jobs)
  (setq +git-sync--global-active 0
        +git-sync--global-waiters nil)
  nil)

(defun +git-sync--get-job (repository-id)
  "Return the live job for REPOSITORY-ID, creating an idle one if needed."
  (or (gethash repository-id +git-sync--jobs)
      (let ((job (+git-sync-job--create
                  :repository-id repository-id
                  :state 'idle
                  :attempt-id 0
                  :started-at nil
                  :finished-at nil
                  :process nil
                  :cancel-flag nil
                  :previous-generation 0
                  :candidate-generation 0
                  :waiting-buffers nil
                  :waiting-callbacks nil
                  :last-success nil
                  :last-error nil
                  :forge-status 'unavailable
                  :stale-p nil
                  :lock-held-p nil
                  :lock-nonce nil
                  :slot-held-p nil
                  :persist-failure-p t
                  :forge-request nil
                  :candidate-dir nil
                  :backup-dir nil
                  :remote-url nil
                  :seed-root nil
                  :provider 'generic
                  :pr-refs-status 'unavailable)))
        (puthash repository-id job +git-sync--jobs)
        job)))

(defun +git-sync--next-attempt-id ()
  "Return a fresh attempt ID for a sync run."
  (cl-incf +git-sync--attempt-counter))

(defun +git-sync--job-current-p (job attempt-id &optional proc)
  "Return non-nil when JOB still owns ATTEMPT-ID (and PROC when given)."
  (and job
       (eql (+git-sync-job-attempt-id job) attempt-id)
       (memq (+git-sync-job-state job) +git-sync--active-states)
       (or (null proc)
           (eq (+git-sync-job-process job) proc))))

;; ---------------------------------------------------------------------------
;; Pure path helpers
;; ---------------------------------------------------------------------------

(defun +git-sync--repository-hash (repository-id)
  "Return SHA-256 hex digest of the exact REPOSITORY-ID string."
  (secure-hash 'sha256 repository-id))

(defun +git-sync-repository-cache-directory (repository-id)
  "Return the cache directory for REPOSITORY-ID under the registry root."
  (expand-file-name (+git-sync--repository-hash repository-id)
                    (file-name-as-directory +git-store-registry-directory)))

(defun +git-sync-mirror-directory (repository-id)
  "Return the published bare mirror path for REPOSITORY-ID."
  (expand-file-name "mirror.git"
                    (+git-sync-repository-cache-directory repository-id)))

(defun +git-sync-state-file (repository-id)
  "Return the sync-state.json path for REPOSITORY-ID."
  (expand-file-name "sync-state.json"
                    (+git-sync-repository-cache-directory repository-id)))

(defun +git-sync-lock-file (repository-id)
  "Return the sync lock directory path for REPOSITORY-ID.
The lock is a directory (`sync.lock.d') created atomically."
  (expand-file-name "sync.lock.d"
                    (+git-sync-repository-cache-directory repository-id)))

(defun +git-sync-lock-owner-file (repository-id)
  "Return the owner metadata file inside the lock directory."
  (expand-file-name "owner.json" (+git-sync-lock-file repository-id)))

(defun +git-sync-publish-intent-file (repository-id)
  "Return the publish-intent.json path for REPOSITORY-ID."
  (expand-file-name "publish-intent.json"
                    (+git-sync-repository-cache-directory repository-id)))

(defun +git-sync-mirror-backup-directory (repository-id)
  "Return the temporary backup path used during publication."
  (concat (+git-sync-mirror-directory repository-id) ".old"))

(defun +git-sync-candidate-directory (repository-id)
  "Return the candidate bare mirror path used during sync."
  (expand-file-name "candidate.git"
                    (+git-sync-repository-cache-directory repository-id)))

(defun +git-sync--ensure-cache-dir (repository-id)
  "Create the repository cache directory for REPOSITORY-ID."
  (make-directory (+git-sync-repository-cache-directory repository-id) t))

;; ---------------------------------------------------------------------------
;; URL / message sanitization
;; ---------------------------------------------------------------------------

(defun +git-sync--sanitize-url (url)
  "Return URL with userinfo stripped for diagnostics."
  (cond
   ((or (not (stringp url)) (string-empty-p url)) url)
   ((string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\)://[^/@]+@" url)
    (replace-regexp-in-string
     "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\)://[^/@]+@"
     "\\1://"
     url))
   ((string-match "\\`[^@]+@\\([^:]+\\):" url)
    (concat (match-string 1 url)
            ":"
            (substring url (match-end 0))))
   (t url)))

(defun +git-sync--sanitize-error (err)
  "Return a compact sanitized error string from ERR."
  (let* ((raw (cond
               ((stringp err) err)
               ((and (consp err) (stringp (cadr err))) (cadr err))
               (t (error-message-string err))))
         (clean (replace-regexp-in-string
                 "https://[^/@\n ]+@" "https://"
                 (replace-regexp-in-string
                  "http://[^/@\n ]+@" "http://"
                  (or raw "unknown error")))))
    (if (> (length clean) 200)
        (concat (substring clean 0 197) "...")
      clean)))

;; ---------------------------------------------------------------------------
;; Provider refspecs
;; ---------------------------------------------------------------------------

(defun +git-sync-detect-provider (repository-id)
  "Return provider symbol for REPOSITORY-ID: github, gitlab, or generic."
  (cond
   ((or (not (stringp repository-id))
        (string-prefix-p "local:" repository-id))
    'generic)
   ((string-match-p "\\`\\(?:[^/]*\\.\\)?github\\.com/" repository-id)
    'github)
   ((string-match-p "\\`\\(?:[^/]*\\.\\)?gitlab\\." repository-id)
    'gitlab)
   ((string-match-p "github\\.com/" repository-id) 'github)
   ((string-match-p "gitlab\\." repository-id) 'gitlab)
   (t 'generic)))

(defun +git-sync-provider-refspecs (repository-id)
  "Return (REFSPECS . PR-STATUS) for REPOSITORY-ID.
REFSPECS is a list of fetch refspec strings.  PR-STATUS is `ok' when
provider PR/MR refs are included, otherwise `unavailable'."
  (let* ((provider (+git-sync-detect-provider repository-id))
         (base '("+refs/heads/*:refs/heads/*"
                 "+refs/tags/*:refs/tags/*")))
    (pcase provider
      ('github
       (cons (append base
                     '("+refs/pull/*/head:refs/pull/*/head"
                       "+refs/pull/*/merge:refs/pull/*/merge"))
             'ok))
      ('gitlab
       (cons (append base
                     '("+refs/merge-requests/*/head:refs/merge-requests/*/head"))
             'ok))
      (_
       (cons base 'unavailable)))))

;; ---------------------------------------------------------------------------
;; Sync-state persistence (atomic JSON)
;; ---------------------------------------------------------------------------

(defun +git-sync--alist-get (key alist)
  "Return value for KEY in ALIST (string or symbol keys)."
  (or (cdr (assoc key alist))
      (cdr (assq (intern key) alist))))

(defun +git-sync--default-state ()
  "Return a fresh sync-state alist.
Always allocate a new list: callers may mutate fields with `setf'."
  (list (cons "version" 1)
        (cons "generation" 0)
        (cons "last-success" nil)
        (cons "last-attempt" nil)
        (cons "last-error" nil)
        (cons "forge-status" "unavailable")
        (cons "provider" "generic")
        (cons "pr-refs-status" "unavailable")
        (cons "mirror-format" 1)))

(defun +git-sync-load-state (repository-id)
  "Load sync-state.json for REPOSITORY-ID.
Malformed content returns a usable default with `:malformed' metadata
and never evaluates Lisp."
  (let ((file (+git-sync-state-file repository-id)))
    (if (not (file-readable-p file))
        (cons (+git-sync--default-state) nil)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (json-false :false)
                   (data (json-read))
                   (version (+git-sync--alist-get "version" data))
                   (gen (+git-sync--alist-get "generation" data)))
              (unless (and (numberp version) (numberp gen))
                (error "Malformed sync-state: bad version/generation"))
              (cons data nil)))
        (error
         (cons (let ((st (+git-sync--default-state)))
                 (setf (alist-get "last-error" st nil nil #'equal)
                       (+git-sync--sanitize-error err))
                 st)
               'malformed))))))

(defun +git-sync-save-state (repository-id state)
  "Atomically write STATE for REPOSITORY-ID.
Uses a same-directory temporary file and rename.  On failure, the
previous valid file is preserved and an error is signaled."
  (+git-sync--ensure-cache-dir repository-id)
  (let* ((file (+git-sync-state-file repository-id))
         (dir (file-name-directory file))
         (tmp (make-temp-file
               (expand-file-name ".git-sync-state-" dir) nil ".tmp")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((json-encoding-pretty-print t)
                  (json-encoding-default-indentation "  "))
              (insert (json-encode state))
              (insert "\n")))
          (rename-file tmp file t)
          (setq tmp nil)
          file)
      (when (and tmp (file-exists-p tmp))
        (ignore-errors (delete-file tmp))))))

(defun +git-sync--state-generation (state)
  "Return generation number from STATE alist."
  (or (+git-sync--alist-get "generation" state) 0))

;; ---------------------------------------------------------------------------
;; Filesystem lock (atomic directory + owner nonce)
;; ---------------------------------------------------------------------------

(defun +git-sync--new-lock-nonce ()
  "Return a unique lock owner nonce."
  (format "%s-%s-%s-%s"
          (emacs-pid)
          (system-name)
          (float-time)
          (random most-positive-fixnum)))

(defun +git-sync--lock-payload (nonce)
  "Return sanitized lock owner metadata alist for NONCE."
  `(("pid" . ,(emacs-pid))
    ("host" . ,(system-name))
    ("timestamp" . ,(float-time))
    ("nonce" . ,nonce)))

(defun +git-sync--read-lock (repository-id)
  "Return lock alist for REPOSITORY-ID, or nil / `unknown'."
  (+git-sync--read-lock-at (+git-sync-lock-file repository-id)))

(defun +git-sync--pid-alive-p (pid)
  "Return non-nil when PID appears alive on this host."
  (and (integerp pid)
       (> pid 0)
       (file-exists-p (format "/proc/%d" pid))))

(defun +git-sync--lock-stale-p (lock)
  "Return non-nil when LOCK may be removed under the stale policy."
  (cond
   ((eq lock 'unknown) nil)
   ((not (listp lock)) nil)
   (t
    (let* ((pid (+git-sync--alist-get "pid" lock))
           (host (+git-sync--alist-get "host" lock))
           (ts (+git-sync--alist-get "timestamp" lock))
           (same-host (equal host (system-name)))
           (age (and (numberp ts) (- (float-time) ts))))
      (and same-host
           (integerp pid)
           (not (+git-sync--pid-alive-p pid))
           (numberp age)
           (> age +git-sync-stale-lock-seconds))))))

(defun +git-sync--lock-dead-owner-p (lock)
  "Return non-nil when LOCK has a confirmed dead same-host owner."
  (and (listp lock)
       (let ((pid (+git-sync--alist-get "pid" lock))
             (host (+git-sync--alist-get "host" lock)))
         (and (equal host (system-name))
              (integerp pid)
              (> pid 0)
              (not (+git-sync--pid-alive-p pid))))))

(defun +git-sync--remove-lock-dir (repository-id)
  "Remove the lock directory for REPOSITORY-ID if present."
  (let ((dir (+git-sync-lock-file repository-id)))
    (when (file-directory-p dir)
      (ignore-errors (delete-directory dir t)))
    (when (file-exists-p dir)
      (ignore-errors (delete-file dir)))))

(defun +git-sync--read-lock-at (dir)
  "Return lock alist from owner.json inside DIR, or nil / `unknown'."
  (let ((file (expand-file-name "owner.json" dir)))
    (cond
     ((not (file-directory-p dir)) nil)
     ((not (file-readable-p file)) 'unknown)
     (t
      (condition-case _
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'string)
                  (json-false :false))
              (json-read)))
        (error 'unknown))))))

(defun +git-sync--lock-same-owner-p (a b)
  "Return non-nil when lock alists A and B share pid/host/nonce."
  (and (listp a) (listp b)
       (equal (+git-sync--alist-get "nonce" a)
              (+git-sync--alist-get "nonce" b))
       (equal (+git-sync--alist-get "pid" a)
              (+git-sync--alist-get "pid" b))
       (equal (+git-sync--alist-get "host" a)
              (+git-sync--alist-get "host" b))))

(defun +git-sync-clear-stale-lock (repository-id)
  "Explicitly remove a stale sync lock for REPOSITORY-ID.
Ordinary acquisition already reclaims confirmed dead-owner locks without
an age delay.  This maintenance command additionally enforces
`+git-sync-stale-lock-seconds'.  Return non-nil when a stale lock was
removed.  Live and unknown locks are always refused."
  (interactive
   (list (+git-sync--resolve-repository-id)))
  (+git-sync--reclaim-dead-lock repository-id t))

(defun +git-sync--reclaim-dead-lock (repository-id &optional require-age)
  "Atomically reclaim REPOSITORY-ID's confirmed dead-owner lock.
When REQUIRE-AGE is non-nil, also apply
`+git-sync-stale-lock-seconds'.  The lock directory is first renamed to
a unique quarantine path, so a concurrent process can safely acquire the
canonical lock without being deleted or displaced by this cleanup."
  (let* ((dir (+git-sync-lock-file repository-id))
         (observed (+git-sync--read-lock repository-id))
         (eligible (if require-age
                       (+git-sync--lock-stale-p observed)
                     (+git-sync--lock-dead-owner-p observed)))
         (quarantine
          (expand-file-name
           (format ".sync.lock.dead.%s.%s"
                   (emacs-pid) (+git-sync--new-lock-nonce))
           (+git-sync-repository-cache-directory repository-id))))
    (when (and eligible (file-directory-p dir))
      (condition-case nil
          (progn
            (rename-file dir quarantine)
            (let ((moved (+git-sync--read-lock-at quarantine)))
              (if (and (+git-sync--lock-same-owner-p observed moved)
                       (+git-sync--lock-dead-owner-p moved)
                       (or (not require-age)
                           (+git-sync--lock-stale-p moved)))
                  (progn
                    (ignore-errors (delete-directory quarantine t))
                    t)
                ;; Owner metadata changed unexpectedly.  Restore only when
                ;; no racer has acquired the canonical lock.
                (unless (file-exists-p dir)
                  (ignore-errors (rename-file quarantine dir)))
                nil)))
        (file-error nil)))))

(defun +git-sync-try-acquire-lock (repository-id &optional nonce)
  "Attempt to acquire the sync lock for REPOSITORY-ID.
Return `acquired' or `busy'.  NONCE when provided is written as the
owner token; otherwise a new nonce is generated.  On `acquired', return
(`acquired' . NONCE).

Never steals a live or unknown lock.  A well-formed same-host lock whose
PID is confirmed dead is atomically quarantined before acquisition.
Builds a complete temporary lock directory (with owner.json) then
atomically renames it into place."
  (+git-sync--ensure-cache-dir repository-id)
  (+git-sync--reclaim-dead-lock repository-id)
  (let* ((dir (+git-sync-lock-file repository-id))
         (nonce (or nonce (+git-sync--new-lock-nonce)))
         (existing (+git-sync--read-lock repository-id))
         (cache (+git-sync-repository-cache-directory repository-id))
         (staging nil))
    (cond
     ((and (null existing) (not (file-exists-p dir)))
      (setq staging
            (expand-file-name
             (format ".sync.lock.staging.%s.%s" (emacs-pid) nonce)
             cache))
      (condition-case _
          (progn
            (when (file-exists-p staging)
              (ignore-errors (delete-directory staging t)))
            (make-directory staging)
            (with-temp-file (expand-file-name "owner.json" staging)
              (insert (json-encode (+git-sync--lock-payload nonce)))
              (insert "\n"))
            (rename-file staging dir)
            (setq staging nil)
            (cons 'acquired nonce))
        (file-already-exists
         (when staging (ignore-errors (delete-directory staging t)))
         (cons 'busy nil))
        (error
         (when staging (ignore-errors (delete-directory staging t)))
         (cons 'busy nil))))
     (t (cons 'busy nil)))))

(defun +git-sync--lock-live-p (repository-id)
  "Return non-nil when a sync lock should block mutating recovery.
Unknown and non-stale locks are treated as live.  Stale locks are not."
  (let ((dir (+git-sync-lock-file repository-id))
        (lock (+git-sync--read-lock repository-id)))
    (cond
     ((not (file-exists-p dir)) nil)
     ((eq lock 'unknown) t)
     ((null lock) (file-directory-p dir))
     ((+git-sync--lock-stale-p lock) nil)
     (t t))))

(defun +git-sync--publish-incomplete-p (repository-id)
  "Return non-nil when publish-intent claims a generation not yet in state."
  (let ((intent-file (+git-sync-publish-intent-file repository-id)))
    (and (file-readable-p intent-file)
         (let* ((intent
                 (condition-case _
                     (with-temp-buffer
                       (insert-file-contents intent-file)
                       (goto-char (point-min))
                       (let ((json-object-type 'alist)
                             (json-key-type 'string)
                             (json-false :false))
                         (json-read)))
                   (error nil)))
                (intent-gen
                 (and intent (+git-sync--alist-get "generation" intent)))
                (state-gen
                 (+git-sync--state-generation
                  (car (+git-sync-load-state repository-id)))))
           (not (and (numberp intent-gen) (eql intent-gen state-gen)))))))

(defun +git-sync-acquire-lock (repository-id)
  "Acquire lock for REPOSITORY-ID.
Return the owner nonce on success, or nil when the lock is busy
(including when it belongs to a live or unknown owner).  A confirmed
same-host dead-owner lock is reclaimed by the underlying acquisition."
  (let* ((result (+git-sync-try-acquire-lock repository-id))
         (status (car result))
         (nonce (cdr result)))
    (and (eq status 'acquired) nonce)))

(defun +git-sync-release-lock (repository-id &optional nonce)
  "Release an owned sync lock for REPOSITORY-ID.
When NONCE is non-nil, release only if the on-disk owner matches."
  (let ((lock (+git-sync--read-lock repository-id)))
    (when (and (listp lock)
               (equal (+git-sync--alist-get "pid" lock) (emacs-pid))
               (equal (+git-sync--alist-get "host" lock) (system-name))
               (or (null nonce)
                   (equal (+git-sync--alist-get "nonce" lock) nonce)))
      (+git-sync--remove-lock-dir repository-id))))

;; ---------------------------------------------------------------------------
;; Git helpers (argument vectors only)
;; ---------------------------------------------------------------------------

(defun +git-sync--call-git (dir &rest args)
  "Run git ARGS with DIR as the working tree / git-dir context.
Returns (exit-code . stdout).  Sets GIT_TERMINAL_PROMPT=0 only.
Does not disable the user's global/system Git configuration."
  (with-temp-buffer
    (let* ((default-directory
            (file-name-as-directory (expand-file-name (or dir default-directory))))
           (process-environment (+git-sync--git-process-environment))
           (exit (apply #'call-process "git" nil t nil args)))
      (cons exit (buffer-string)))))

(defun +git-sync--git-ok (dir &rest args)
  "Run git ARGS in DIR; signal on non-zero exit."
  (let* ((result (apply #'+git-sync--call-git dir args))
         (exit (car result))
         (out (string-trim-right (cdr result))))
    (unless (eq exit 0)
      (error "git %s failed (exit %s): %s"
             (mapconcat #'identity args " ") exit
             (+git-sync--sanitize-error out)))
    out))

(defun +git-sync--mirror-exists-p (repository-id)
  "Return non-nil when a readable published mirror exists.
Runs crash recovery and accepts mirror.git.old while an incomplete
publish is still being repaired."
  (let ((dir (+git-sync-published-mirror-directory repository-id)))
    (and (file-directory-p dir)
         (file-exists-p (expand-file-name "HEAD" dir)))))

(defun +git-sync--delete-dir (dir)
  "Delete DIR recursively when it exists."
  (when (and dir (file-exists-p dir))
    (ignore-errors (delete-directory dir t))))

(defun +git-sync--choose-seed-root (repository-id)
  "Return an available local context root for REPOSITORY-ID, or nil."
  (when-let ((ctx (car (+git-store-list-live-contexts repository-id))))
    (+git-store-local-context-root ctx)))

(defvar +git-sync-test-remote-override nil
  "Alist of (REPOSITORY-ID . FETCH-URL) for offline tests.
When set, `+git-sync--choose-remote-url' prefers these URLs so hosted
canonical identities can still fetch from a local bare fixture.")

(defun +git-sync--choose-remote-url (repository-id)
  "Return a fetch URL from a live context for REPOSITORY-ID, or nil."
  (or (cdr (assoc repository-id +git-sync-test-remote-override))
      (cl-dolist (ctx (+git-store-list-live-contexts repository-id))
        (when-let ((url (+git-store-local-context-remote-url ctx)))
          (cl-return url)))))

(defun +git-sync-ensure-mirror (repository-id &optional seed-root)
  "Explicitly create a bare published mirror for REPOSITORY-ID.
SEED-ROOT when non-nil seeds via local object transfer only (no network).
Never uses Git alternates.  Ordinary registry/open paths must not call this."
  (if (+git-sync--mirror-exists-p repository-id)
      (+git-sync-published-mirror-directory repository-id)
    (+git-sync--ensure-cache-dir repository-id)
    (let* ((mirror (+git-sync-mirror-directory repository-id))
           (seed (or seed-root (+git-sync--choose-seed-root repository-id))))
      (cond
       ((and seed (file-directory-p seed))
        ;; Local bare clone without --shared/--reference: no alternates.
        (+git-sync--git-ok seed "clone" "--bare" seed mirror))
       (t
        (+git-sync--git-ok default-directory "init" "--bare" mirror)))
      ;; Point mirror remote at the configured fetch URL when known.
      (when-let ((url (+git-sync--choose-remote-url repository-id)))
        (ignore-errors
          (+git-sync--git-ok mirror "remote" "remove" "origin"))
        (+git-sync--git-ok mirror "remote" "add" "origin" url))
      (let ((repo (+git-store-get-repository repository-id)))
        (when repo
          (setf (+git-store-repository-mirror-directory repo) mirror)))
      mirror)))

(defun +git-sync--git-process-environment ()
  "Return `process-environment' for sync Git children.
Only forces GIT_TERMINAL_PROMPT=0 so credential helpers, insteadOf,
proxy/SSL, and SSH settings from the user's Git configuration remain
available.  Tests that need isolation bind GIT_CONFIG_* themselves."
  (cons "GIT_TERMINAL_PROMPT=0" process-environment))

(defun +git-sync--start-git-process (job name args sentinel)
  "Start an async git process for JOB with ARGS and SENTINEL.
Binds GIT_TERMINAL_PROMPT=0.  Returns the process."
  (let* ((buf (generate-new-buffer (format " *%s*" name)))
         (process-environment (+git-sync--git-process-environment))
         (attempt (+git-sync-job-attempt-id job))
         (proc
          (make-process
           :name name
           :buffer buf
           :command (cons "git" args)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (p msg)
             (funcall sentinel job attempt p msg)))))
    (set-process-query-on-exit-flag proc nil)
    (setf (+git-sync-job-process job) proc)
    proc))

(defun +git-sync--restore-mirror-from-backup (mirror backup)
  "Try to restore BACKUP over MIRROR.
Return non-nil only when BACKUP was successfully renamed to MIRROR.
On failure, leave BACKUP in place (never delete it)."
  (cond
   ((not (file-directory-p backup)) nil)
   ((file-directory-p mirror)
    ;; Move the incomplete published mirror aside, then restore backup.
    (let ((broken (concat mirror ".broken"))
          (ok nil))
      (condition-case _
          (progn
            (+git-sync--delete-dir broken)
            (rename-file mirror broken)
            (rename-file backup mirror)
            (setq ok t)
            (+git-sync--delete-dir broken))
        (error
         ;; Prefer keeping the backup as the last known good generation.
         (when (and (not (file-directory-p backup))
                    (file-directory-p mirror))
           (ignore-errors (rename-file mirror backup)))
         (when (and (not (file-directory-p mirror))
                    (file-directory-p broken))
           (ignore-errors (rename-file broken mirror)))
         (setq ok nil)))
      ok))
   (t
    (condition-case _
        (progn (rename-file backup mirror) t)
      (error nil)))))

(defun +git-sync--recover-incomplete-publish (repository-id)
  "Apply crash-recovery rules for an interrupted publication.
Caller must own the repository sync lock.  If publish-intent exists and
sync-state generation matches the intent, drop leftover backup/intent.
Otherwise restore mirror.git.old over mirror.git.  Backup and intent
are retained unless restoration definitely succeeds."
  (let* ((intent-file (+git-sync-publish-intent-file repository-id))
         (backup (+git-sync-mirror-backup-directory repository-id))
         (mirror (+git-sync-mirror-directory repository-id)))
    (when (file-readable-p intent-file)
      (let* ((intent
              (condition-case _
                  (with-temp-buffer
                    (insert-file-contents intent-file)
                    (goto-char (point-min))
                    (let ((json-object-type 'alist)
                          (json-key-type 'string)
                          (json-false :false))
                      (json-read)))
                (error nil)))
             (intent-gen (and intent (+git-sync--alist-get "generation" intent)))
             (state-gen (+git-sync--state-generation
                         (car (+git-sync-load-state repository-id)))))
        (cond
         ((and (numberp intent-gen) (eql intent-gen state-gen))
          ;; State already published; leftover backup/intent only.
          (+git-sync--delete-dir backup)
          (ignore-errors (delete-file intent-file)))
         (t
          ;; Incomplete publish: restore previous mirror when present.
          (if (file-directory-p backup)
              (when (+git-sync--restore-mirror-from-backup mirror backup)
                (ignore-errors (delete-file intent-file)))
            ;; No backup to restore; keep intent for a later attempt.
            nil)))))))

(defun +git-sync--maybe-recover-incomplete-publish (repository-id)
  "Mutating recovery only when no live publication owns the lock.
Acquires the lock briefly for the mutation.  No-ops while another
process (or an in-flight publisher) holds the lock."
  (when (and (+git-sync--publish-incomplete-p repository-id)
             (not (+git-sync--lock-live-p repository-id)))
    (let ((nonce (+git-sync-acquire-lock repository-id)))
      (when nonce
        (unwind-protect
            (+git-sync--recover-incomplete-publish repository-id)
          (+git-sync-release-lock repository-id nonce))))))

(defun +git-sync-published-mirror-directory (repository-id)
  "Return a readable published mirror path for REPOSITORY-ID.
When no live lock is held, attempt crash recovery under the lock.
While a live publication owns the lock (or recovery could not finish),
prefer mirror.git.old so readers never roll back an in-flight publish."
  (+git-sync--maybe-recover-incomplete-publish repository-id)
  (let ((mirror (+git-sync-mirror-directory repository-id))
        (backup (+git-sync-mirror-backup-directory repository-id))
        (incomplete (+git-sync--publish-incomplete-p repository-id)))
    (cond
     ((and incomplete
           (file-directory-p backup)
           (file-exists-p (expand-file-name "HEAD" backup)))
      backup)
     ((and (file-directory-p mirror)
           (file-exists-p (expand-file-name "HEAD" mirror)))
      mirror)
     ((and (file-directory-p backup)
           (file-exists-p (expand-file-name "HEAD" backup)))
      backup)
     (t mirror))))

(defun +git-sync--prepare-candidate-args (job)
  "Return git argv (no program) to create JOB's candidate bare repo."
  (let* ((repository-id (+git-sync-job-repository-id job))
         (candidate (+git-sync-candidate-directory repository-id))
         (mirror (+git-sync-published-mirror-directory repository-id))
         (seed (or (+git-sync-job-seed-root job)
                   (+git-sync--choose-seed-root repository-id))))
    (+git-sync--delete-dir candidate)
    (setf (+git-sync-job-candidate-dir job) candidate)
    (cond
     ((+git-sync--mirror-exists-p repository-id)
      (list "clone" "--bare" mirror candidate))
     ((and seed (file-directory-p seed))
      (list "clone" "--bare" seed candidate))
     (t
      (list "init" "--bare" candidate)))))

(defun +git-sync--configure-candidate-remote (job)
  "Configure origin on JOB's candidate from its remote URL."
  (let ((candidate (+git-sync-job-candidate-dir job))
        (url (or (+git-sync-job-remote-url job)
                 (+git-sync--choose-remote-url
                  (+git-sync-job-repository-id job)))))
    (when url
      (setf (+git-sync-job-remote-url job) url)
      (ignore-errors
        (+git-sync--git-ok candidate "remote" "remove" "origin"))
      (+git-sync--git-ok candidate "remote" "add" "origin" url))))

(defun +git-sync--prepare-sentinel (job attempt proc _msg)
  "Handle completion of JOB's asynchronous candidate preparation."
  (when (memq (process-status proc) '(exit signal))
    (let* ((exit (process-exit-status proc))
           (buf (process-buffer proc))
           (out (and (buffer-live-p buf)
                     (with-current-buffer buf (buffer-string)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (+git-sync--job-current-p job attempt proc)
        (setf (+git-sync-job-process job) nil)
        (cond
         ((+git-sync-job-cancel-flag job)
          (+git-sync--finish job 'cancelled "cancelled"))
         ((not (eq exit 0))
          (+git-sync--finish
           job 'failed
           (+git-sync--sanitize-error
            (or out (format "git prepare exit %s" exit)))))
         (t
          (condition-case err
              (progn
                (+git-sync--configure-candidate-remote job)
                (cond
                 ((+git-sync-job-cancel-flag job)
                  (+git-sync--finish job 'cancelled "cancelled"))
                 ((not (+git-sync-job-remote-url job))
                  (+git-sync--finish
                   job 'failed "no usable remote for synchronization"))
                 (t
                  (setf (+git-sync-job-state job) 'fetching-mirror)
                  (+git-sync--start-fetch job))))
            (error
             (+git-sync--finish
              job 'failed (+git-sync--sanitize-error err))))))))))

(defun +git-sync--start-prepare (job)
  "Start asynchronous candidate preparation for JOB."
  (let* ((args (+git-sync--prepare-candidate-args job))
         (name (format "git-sync-prepare-%s"
                       (+git-sync--repository-hash
                        (+git-sync-job-repository-id job)))))
    (+git-sync--start-git-process
     job name args #'+git-sync--prepare-sentinel)))

(defun +git-sync--write-publish-intent (repository-id generation)
  "Write publish-intent.json for REPOSITORY-ID claiming GENERATION."
  (let ((file (+git-sync-publish-intent-file repository-id))
        (dir (file-name-directory
              (+git-sync-publish-intent-file repository-id)))
        (tmp nil))
    (make-directory (file-name-directory file) t)
    (setq tmp (make-temp-file
               (expand-file-name ".git-sync-intent-" dir) nil ".tmp"))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (json-encode
                     `(("generation" . ,generation)
                       ("timestamp" . ,(float-time))))
                   "\n"))
          (rename-file tmp file t)
          (setq tmp nil))
      (when (and tmp (file-exists-p tmp))
        (ignore-errors (delete-file tmp))))))

(defun +git-sync--publish-candidate (job)
  "Swap JOB's candidate into mirror.git, retaining backup until confirmed.
Does not delete the backup; caller must confirm via state publication
then call `+git-sync--confirm-publish', or restore via
`+git-sync--rollback-publish'."
  (let* ((repository-id (+git-sync-job-repository-id job))
         (candidate (+git-sync-job-candidate-dir job))
         (mirror (+git-sync-mirror-directory repository-id))
         (backup (+git-sync-mirror-backup-directory repository-id)))
    (unless (and candidate (file-directory-p candidate))
      (error "Missing candidate mirror"))
    (+git-sync--delete-dir backup)
    (when (file-directory-p mirror)
      (rename-file mirror backup))
    (condition-case err
        (progn
          (rename-file candidate mirror)
          (setf (+git-sync-job-candidate-dir job) nil)
          (setf (+git-sync-job-backup-dir job)
                (and (file-directory-p backup) backup))
          mirror)
      (error
       (when (and (not (file-directory-p mirror))
                  (file-directory-p backup))
         (ignore-errors (rename-file backup mirror)))
       (setf (+git-sync-job-backup-dir job) nil)
       (signal (car err) (cdr err))))))

(defun +git-sync--confirm-publish (job)
  "Drop backup and publish-intent after successful state publication."
  (let ((repository-id (+git-sync-job-repository-id job)))
    (when-let ((backup (+git-sync-job-backup-dir job)))
      (+git-sync--delete-dir backup)
      (setf (+git-sync-job-backup-dir job) nil))
    (ignore-errors
      (delete-file (+git-sync-publish-intent-file repository-id)))))

(defun +git-sync--rollback-publish (job)
  "Restore the previous mirror after a failed state publication.
Retain backup and publish-intent unless restoration definitely succeeds."
  (let* ((repository-id (+git-sync-job-repository-id job))
         (mirror (+git-sync-mirror-directory repository-id))
         (backup (or (+git-sync-job-backup-dir job)
                     (+git-sync-mirror-backup-directory repository-id)))
         (intent (+git-sync-publish-intent-file repository-id)))
    (if (and (file-directory-p backup)
             (+git-sync--restore-mirror-from-backup mirror backup))
        (progn
          (setf (+git-sync-job-backup-dir job) nil)
          (ignore-errors (delete-file intent)))
      ;; Leave backup/intent for crash recovery / offline readers.
      nil)))

(defun +git-sync--cleanup-candidate (job)
  "Delete JOB's candidate directory if present."
  (when-let ((dir (+git-sync-job-candidate-dir job)))
    (+git-sync--delete-dir dir)
    (setf (+git-sync-job-candidate-dir job) nil)))

(defun +git-sync--cleanup-on-emacs-exit ()
  "Release processes, candidates, and locks owned by this Emacs.
Published mirrors, Forge data, PR metadata, and synchronization state
remain on disk for the next Emacs session.  This hook performs no
network operations."
  (maphash
   (lambda (repository-id job)
     (condition-case nil
         (progn
           (setf (+git-sync-job-cancel-flag job) t)
           (when-let ((proc (+git-sync-job-process job)))
             (when (process-live-p proc)
               (ignore-errors (delete-process proc))))
           (when-let ((request (+git-sync-job-forge-request job)))
             (when-let ((cancel (plist-get request :cancel)))
               (ignore-errors (funcall cancel))))
           (+git-sync--cleanup-candidate job)
           (when (+git-sync-job-lock-held-p job)
             (+git-sync-release-lock
              repository-id (+git-sync-job-lock-nonce job))
             (setf (+git-sync-job-lock-held-p job) nil)
             (setf (+git-sync-job-lock-nonce job) nil)))
       (error nil)))
   +git-sync--jobs))

(add-hook 'kill-emacs-hook #'+git-sync--cleanup-on-emacs-exit)

;; ---------------------------------------------------------------------------
;; Async fetch via make-process
;; ---------------------------------------------------------------------------

(defun +git-sync--fetch-argv (job)
  "Return the git fetch argument vector for JOB (no program name)."
  (let* ((repository-id (+git-sync-job-repository-id job))
         (pair (+git-sync-provider-refspecs repository-id))
         (refspecs (car pair))
         (pr-status (cdr pair))
         (candidate (+git-sync-job-candidate-dir job)))
    (setf (+git-sync-job-provider job)
          (+git-sync-detect-provider repository-id))
    (setf (+git-sync-job-pr-refs-status job) pr-status)
    (append (list "-C" candidate "fetch" "--prune" "origin")
            refspecs)))

(defun +git-sync--start-fetch (job)
  "Start an asynchronous git fetch for JOB into its candidate."
  (let* ((argv (+git-sync--fetch-argv job))
         (name (format "git-sync-fetch-%s"
                       (+git-sync--repository-hash
                        (+git-sync-job-repository-id job)))))
    (cl-incf +git-sync--git-fetch-count)
    (+git-sync--start-git-process
     job name argv #'+git-sync--fetch-sentinel)))

(defun +git-sync--fetch-sentinel (job attempt proc _msg)
  "Handle completion of JOB's fetch process PROC for ATTEMPT."
  (when (memq (process-status proc) '(exit signal))
    (let* ((exit (process-exit-status proc))
           (buf (process-buffer proc))
           (out (and (buffer-live-p buf)
                     (with-current-buffer buf (buffer-string)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (+git-sync--job-current-p job attempt proc)
        (setf (+git-sync-job-process job) nil)
        (cond
         ((+git-sync-job-cancel-flag job)
          (+git-sync--finish job 'cancelled "cancelled"))
         ((eq exit 0)
          (+git-sync--enter-pulling-forge job))
         (t
          (+git-sync--finish
           job 'failed
           (+git-sync--sanitize-error
            (or out (format "git fetch exit %s" exit))))))))))

;; ---------------------------------------------------------------------------
;; Forge adapter (cancellable handle; success + error completion)
;; ---------------------------------------------------------------------------

(defvar +git-sync--forge-current-ctx nil
  "Dynamically rebound forge intercept context for the active request chain.")

(defvar +git-sync--forge-advice-installed nil
  "Non-nil when persistent Forge HTTP advice is installed.")

(defvar +git-sync-url-retrieve-function nil
  "Optional `url-retrieve' replacement used by Forge intercept advice.
Nil means call the advised original.  Tests bind a stub so delayed
callbacks still pass through the intercept.")

(defvar +git-sync-ghub-request-function nil
  "Optional `ghub-request' replacement used by Forge intercept advice.
Nil means call the advised original.  Tests bind a stub for delayed
GitLab-style errorbacks.")

(cl-defstruct (+git-sync-forge-ctx
               (:constructor +git-sync--make-forge-ctx)
               (:copier nil))
  cancelled finish (url-buffers nil) (done nil))

(defun +git-sync--forge-handle (cancel-fn)
  "Return a Forge request HANDLE plist wrapping CANCEL-FN."
  (list :cancel cancel-fn))

(defun +git-sync--kill-url-retrieve-buffer (buf)
  "Abort an in-flight `url-retrieve' BUF and kill it."
  (when (buffer-live-p buf)
    (when-let ((proc (get-buffer-process buf)))
      (ignore-errors (set-process-sentinel proc #'ignore))
      (ignore-errors (delete-process proc)))
    (ignore-errors (kill-buffer buf))))

(defun +git-sync--forge-ctx-finish (ctx status)
  "Complete CTX once with STATUS and drop tracked URL buffers."
  (unless (+git-sync-forge-ctx-done ctx)
    (setf (+git-sync-forge-ctx-done ctx) t)
    (dolist (buf (+git-sync-forge-ctx-url-buffers ctx))
      (+git-sync--kill-url-retrieve-buffer buf))
    (setf (+git-sync-forge-ctx-url-buffers ctx) nil)
    (funcall (+git-sync-forge-ctx-finish ctx) status)))

(defun +git-sync--forge-wrap-callback (ctx callback)
  "Return CALLBACK rebound to CTX for the duration of its body.
Ensures chained async Forge/Ghub requests still see the intercept.
Callback errors complete CTX with `(failed MESSAGE)' so the sync job
cannot remain stuck in pulling-forge."
  (if (not (functionp callback))
      callback
    (lambda (&rest args)
      (let ((+git-sync--forge-current-ctx ctx))
        (unless (+git-sync-forge-ctx-cancelled ctx)
          (condition-case err
              (apply callback args)
            (error
             (+git-sync--forge-ctx-finish
              ctx
              (list 'failed
                    (+git-sync--sanitize-error err))))))))))

(defun +git-sync--advice-url-retrieve
    (orig url callback &optional cbargs silent inhibit-cookies)
  "Around advice: track/cancel URL buffers while a forge ctx is active."
  (let* ((ctx +git-sync--forge-current-ctx)
         (impl (or +git-sync-url-retrieve-function orig)))
    (if (null ctx)
        (if inhibit-cookies
            (funcall impl url callback cbargs silent inhibit-cookies)
          (funcall impl url callback cbargs silent))
      (let* ((wrapped (+git-sync--forge-wrap-callback ctx callback))
             (buf (if inhibit-cookies
                      (funcall impl url wrapped cbargs silent inhibit-cookies)
                    (funcall impl url wrapped cbargs silent))))
        (when (buffer-live-p buf)
          (push buf (+git-sync-forge-ctx-url-buffers ctx)))
        buf))))

(defun +git-sync--advice-ghub-request
    (orig method resource &optional params &rest keys)
  "Around advice: inject errorbacks for the active forge ctx."
  (let ((ctx +git-sync--forge-current-ctx)
        (impl (or +git-sync-ghub-request-function orig)))
    (if (null ctx)
        (apply impl method resource params keys)
      (let ((callback (plist-get keys :callback))
            (errorback (plist-get keys :errorback)))
        (setq keys (copy-sequence keys))
        (when callback
          (setq keys
                (plist-put keys :callback
                           (+git-sync--forge-wrap-callback ctx callback))))
        (when (and callback (or (null errorback) (eq errorback t)))
          (setq keys
                (plist-put
                 keys :errorback
                 (lambda (&rest e)
                   (let ((+git-sync--forge-current-ctx ctx))
                     (unless (+git-sync-forge-ctx-cancelled ctx)
                       (+git-sync--forge-ctx-finish
                        ctx
                        (list 'failed
                              (+git-sync--sanitize-error
                               (or (car e) "forge http failed"))))))))))
        (apply impl method resource params keys)))))

(defun +git-sync--advice-forge-query (orig &rest args)
  "Around advice: inject errorbacks/callback completion for forge GraphQL."
  (let ((ctx +git-sync--forge-current-ctx))
    (if (null ctx)
        (apply orig args)
      (let* ((plist (copy-sequence (nthcdr 3 args)))
             (cb (plist-get plist :callback)))
        (unless (plist-member plist :errorback)
          (setq plist
                (plist-put
                 plist :errorback
                 (lambda (&rest e)
                   (let ((+git-sync--forge-current-ctx ctx))
                     (unless (+git-sync-forge-ctx-cancelled ctx)
                       (+git-sync--forge-ctx-finish
                        ctx
                        (list 'failed
                              (+git-sync--sanitize-error
                               (or (car e) "forge query failed"))))))))))
        (when cb
          (setq plist
                (plist-put
                 plist :callback
                 (lambda (&rest cb-args)
                   (let ((+git-sync--forge-current-ctx ctx))
                     (unless (+git-sync-forge-ctx-cancelled ctx)
                       (condition-case err
                           (progn
                             (apply cb cb-args)
                             ;; Selective repos skip the outer forge--pull
                             ;; callback; finish only after store succeeds.
                             (+git-sync--forge-ctx-finish ctx 'ok))
                         (error
                          (+git-sync--forge-ctx-finish
                           ctx
                           (list 'failed
                                 (+git-sync--sanitize-error err)))))))))))
        (apply orig (append (cl-subseq args 0 3) plist))))))

(defun +git-sync--advice-forge-msg (orig repo echo done format &rest args)
  "Around advice: complete selective/GitLab pulls on store-done."
  (let ((ctx +git-sync--forge-current-ctx)
        (ret (condition-case _
                 (apply orig repo echo done format args)
               (error nil))))
    (when (and ctx
               done
               (stringp format)
               (string-match-p "Storing REPO" format)
               (not (+git-sync-forge-ctx-cancelled ctx)))
      (+git-sync--forge-ctx-finish ctx 'ok))
    ret))

(defun +git-sync--ensure-forge-advice ()
  "Install persistent around-advice used by the default Forge adapter."
  (require 'url)
  (unless (advice-member-p #'+git-sync--advice-url-retrieve 'url-retrieve)
    (advice-add 'url-retrieve :around #'+git-sync--advice-url-retrieve))
  (when (and (fboundp 'ghub-request)
             (not (advice-member-p #'+git-sync--advice-ghub-request
                                   'ghub-request)))
    (advice-add 'ghub-request :around #'+git-sync--advice-ghub-request))
  (when (and (fboundp 'forge--query)
             (not (advice-member-p #'+git-sync--advice-forge-query
                                   'forge--query)))
    (advice-add 'forge--query :around #'+git-sync--advice-forge-query))
  (when (and (fboundp 'forge--msg)
             (not (advice-member-p #'+git-sync--advice-forge-msg
                                   'forge--msg)))
    (advice-add 'forge--msg :around #'+git-sync--advice-forge-msg))
  (setq +git-sync--forge-advice-installed t))

(defun +git-sync--forge-pull-default (repository-id on-complete)
  "Default Forge adapter using private `forge--pull'.
Returns a cancellable handle.  ON-COMPLETE is invoked exactly once.

Loads Forge on demand when installed.  Installs persistent around-advice
so interception remains active for the full asynchronous request chain
(including delayed GitLab pages, selective store completion, and late
URL callbacks).  Cancel kills tracked `url-retrieve' processes.

Private dependency: `forge--pull' (internal generic)."
  (cl-incf +git-sync--forge-pull-count)
  (let* ((ctx (+git-sync--make-forge-ctx
               :finish on-complete))
         (cancel
          (lambda ()
            (setf (+git-sync-forge-ctx-cancelled ctx) t)
            (dolist (buf (+git-sync-forge-ctx-url-buffers ctx))
              (+git-sync--kill-url-retrieve-buffer buf))
            (setf (+git-sync-forge-ctx-url-buffers ctx) nil)
            (+git-sync--forge-ctx-finish ctx 'cancelled))))
    (ignore-errors (require 'forge nil t))
    (+git-sync--ensure-forge-advice)
    (cond
     ((not (featurep 'forge))
      (+git-sync--forge-ctx-finish ctx 'unavailable)
      (+git-sync--forge-handle cancel))
     ((not (fboundp 'forge--pull))
      (+git-sync--forge-ctx-finish ctx 'unavailable)
      (+git-sync--forge-handle cancel))
     ((not (fboundp 'forge-get-repository))
      (+git-sync--forge-ctx-finish ctx 'unavailable)
      (+git-sync--forge-handle cancel))
     (t
      (let* ((root (+git-sync--choose-seed-root repository-id))
             (repo (and root
                        (ignore-errors
                          (let ((default-directory
                                 (file-name-as-directory root)))
                            (or (forge-get-repository :tracked?)
                                (forge-get-repository
                                 root nil :tracked?)))))))
        (cond
         ((null repo)
          (+git-sync--forge-ctx-finish ctx 'untracked)
          (+git-sync--forge-handle cancel))
         (t
          (+git-sync--ensure-forge-advice)
          (condition-case err
              (let ((+git-sync--forge-current-ctx ctx))
                (forge--pull
                 repo
                 (+git-sync--forge-wrap-callback
                  ctx
                  (lambda ()
                    (+git-sync--forge-ctx-finish ctx 'ok)))))
            (error
             (+git-sync--forge-ctx-finish
              ctx
              (list 'failed (+git-sync--sanitize-error err)))))
          (+git-sync--forge-handle cancel))))))))

(defun +git-sync--enter-pulling-forge (job)
  "Advance JOB into the Forge pull step."
  (if (+git-sync-job-cancel-flag job)
      (+git-sync--finish job 'cancelled "cancelled")
    (setf (+git-sync-job-state job) 'pulling-forge)
    (let* ((repository-id (+git-sync-job-repository-id job))
           (attempt (+git-sync-job-attempt-id job))
           (handle
            (funcall
             +git-sync-forge-pull-function
             repository-id
             (lambda (status)
               (+git-sync--forge-callback job attempt status)))))
      (setf (+git-sync-job-forge-request job) handle)
      ;; Immediate completion (unavailable/untracked) already finished
      ;; via callback; clear handle when no longer active.
      (unless (+git-sync--job-active-p job)
        (setf (+git-sync-job-forge-request job) nil)))))

(defun +git-sync--forge-callback (job attempt status)
  "Handle Forge adapter STATUS for JOB ATTEMPT."
  (when (and (eql (+git-sync-job-attempt-id job) attempt)
             (eq (+git-sync-job-state job) 'pulling-forge))
    (setf (+git-sync-job-forge-request job) nil)
    (cond
     ((or (+git-sync-job-cancel-flag job) (eq status 'cancelled))
      (+git-sync--finish job 'cancelled "cancelled"))
     (t
      (pcase status
        ('ok
         (setf (+git-sync-job-forge-status job) 'current)
         (+git-sync--enter-fetching-checks job))
        ((or 'unavailable 'untracked 'unsupported 'skipped)
         (setf (+git-sync-job-forge-status job) status)
         (+git-sync--enter-fetching-checks job))
        ((pred consp)
         (setf (+git-sync-job-forge-status job) 'failed)
         (+git-sync--finish
          job 'failed
          (format "forge: %s"
                  (+git-sync--sanitize-error
                   (if (stringp (cadr status)) (cadr status) status)))))
        ('failed
         (setf (+git-sync-job-forge-status job) 'failed)
         (+git-sync--finish job 'failed "forge pull failed"))
        (_
         (setf (+git-sync-job-forge-status job) 'failed)
         (+git-sync--finish
          job 'failed
          (format "forge: unexpected status %s" status))))))))

(defun +git-sync--enter-fetching-checks (job)
  "Phase 4 checks extension point: no-op, then publish."
  (if (+git-sync-job-cancel-flag job)
      (+git-sync--finish job 'cancelled "cancelled")
    (setf (+git-sync-job-state job) 'fetching-checks)
    ;; No GitHub checks adapter in Phase 4.
    (+git-sync--enter-publishing job)))

(defun +git-sync--enter-publishing (job)
  "Atomically publish JOB's candidate and sync-state generation.
Writes publish-intent, swaps candidate while retaining backup, then
writes sync-state.  On state failure, restores the previous mirror."
  (if (+git-sync-job-cancel-flag job)
      (+git-sync--finish job 'cancelled "cancelled")
    (setf (+git-sync-job-state job) 'publishing)
    (let* ((repository-id (+git-sync-job-repository-id job))
           (prev (+git-sync-job-previous-generation job))
           (next (1+ prev))
           (now (float-time))
           (state `(("version" . 1)
                    ("generation" . ,next)
                    ("last-success" . ,now)
                    ("last-attempt" . ,now)
                    ("last-error" . nil)
                    ("forge-status" .
                     ,(symbol-name (+git-sync-job-forge-status job)))
                    ("provider" .
                     ,(symbol-name (+git-sync-job-provider job)))
                    ("pr-refs-status" .
                     ,(symbol-name (+git-sync-job-pr-refs-status job)))
                    ("mirror-format" . 1)))
           (swapped nil))
      (condition-case err
          (progn
            (+git-sync--write-publish-intent repository-id next)
            (+git-sync--publish-candidate job)
            (setq swapped t)
            (+git-sync-save-state repository-id state)
            (+git-sync--confirm-publish job)
            (setf (+git-sync-job-candidate-generation job) next)
            (let ((repo (+git-store-get-repository repository-id)))
              (when repo
                (setf (+git-store-repository-mirror-directory repo)
                      (+git-sync-mirror-directory repository-id))
                ;; Do not overwrite Phase 3 context cache-generation.
                (setf (+git-store-repository-last-success repo) now)
                (setf (+git-store-repository-last-error repo) nil)))
            (+git-sync--finish job 'succeeded nil))
        (error
         (when swapped
           (+git-sync--rollback-publish job))
         ;; Do not delete intent here: rollback retains it when restore fails.
         (+git-sync--finish
          job 'failed
          (+git-sync--sanitize-error err)))))))

;; ---------------------------------------------------------------------------
;; Finish / refresh / cancel
;; ---------------------------------------------------------------------------

(defun +git-sync--finish (job final-state &optional error-message)
  "Complete JOB in FINAL-STATE, releasing lock/slot and refreshing waiters."
  (unless (memq (+git-sync-job-state job) '(succeeded failed cancelled))
    (let ((repository-id (+git-sync-job-repository-id job))
          (held-slot (+git-sync-job-slot-held-p job))
          (held-lock (+git-sync-job-lock-held-p job))
          (nonce (+git-sync-job-lock-nonce job))
          (persist (+git-sync-job-persist-failure-p job)))
      (unwind-protect
          (progn
            (+git-sync--cleanup-candidate job)
            (when held-lock
              (+git-sync-release-lock repository-id nonce)
              (setf (+git-sync-job-lock-held-p job) nil)
              (setf (+git-sync-job-lock-nonce job) nil))
            (setf (+git-sync-job-process job) nil)
            (setf (+git-sync-job-forge-request job) nil)
            (setf (+git-sync-job-state job) final-state)
            (setf (+git-sync-job-finished-at job) (float-time))
            (when error-message
              (setf (+git-sync-job-last-error job)
                    (+git-sync--sanitize-error error-message)))
            (pcase final-state
              ('succeeded
               (setf (+git-sync-job-last-success job)
                     (+git-sync-job-finished-at job))
               (setf (+git-sync-job-stale-p job) nil)
               (setf (+git-sync-job-last-error job) nil))
              ((or 'failed 'cancelled)
               (setf (+git-sync-job-stale-p job) t)
               ;; Never rewrite sync-state when we do not own the lock
               ;; (e.g. \"already syncing elsewhere\").
               (when (and persist held-lock)
                 (let* ((pair (+git-sync-load-state repository-id))
                        (state (car pair))
                        (now (float-time)))
                   (setf (alist-get "last-attempt" state nil nil #'equal) now)
                   (setf (alist-get "last-error" state nil nil #'equal)
                         (+git-sync-job-last-error job))
                   (when (eq final-state 'failed)
                     (setf (alist-get "forge-status" state nil nil #'equal)
                           (symbol-name
                            (or (+git-sync-job-forge-status job) 'failed))))
                   (ignore-errors
                     (+git-sync-save-state repository-id state))))))
            (+git-sync--refresh-waiting job)
            (let ((msg
                   (pcase final-state
                     ('succeeded
                      (format "Synced %s (generation %s, forge: %s)"
                              repository-id
                              (+git-sync-job-candidate-generation job)
                              (+git-sync-job-forge-status job)))
                     ('cancelled
                      (format "Sync cancelled: %s" repository-id))
                     (_
                      (format "Sync failed: %s (%s)"
                              repository-id
                              (or (+git-sync-job-last-error job)
                                  "error"))))))
              (message "%s" msg)))
        (when held-slot
          (setf (+git-sync-job-slot-held-p job) nil)
          (setq +git-sync--global-active
                (max 0 (1- +git-sync--global-active)))
          (+git-sync--pump-global-queue)))))
  job)

(defun +git-sync--refresh-waiting (job)
  "Refresh each live waiting buffer for JOB at most once."
  (let ((bufs (cl-remove-if-not #'buffer-live-p
                                (+git-sync-job-waiting-buffers job)))
        (cbs (copy-sequence (+git-sync-job-waiting-callbacks job)))
        (selected (selected-window))
        (wconf (current-window-configuration)))
    (setf (+git-sync-job-waiting-buffers job) nil)
    (setf (+git-sync-job-waiting-callbacks job) nil)
    (dolist (buf bufs)
      (when (buffer-live-p buf)
        (cl-incf +git-sync--waiting-refresh-count)
        (with-selected-window
            (or (get-buffer-window buf t) selected)
          (with-current-buffer buf
            (condition-case _
                (cond
                 ;; PR workspaces must rebuild from Forge cache + mirror,
                 ;; not merely redraw the stale in-memory model.
                 ((and (derived-mode-p '+git-pr-mode)
                       (fboundp '+git-pr-refresh))
                  (+git-pr-refresh))
                 ((and (bound-and-true-p +git-pr--model)
                       (fboundp '+git-pr--refresh-dispatch))
                  (+git-pr--refresh-dispatch))
                 ((and (bound-and-true-p +git-review-target)
                       (fboundp '+git-review-refresh))
                 (+git-review-refresh))
                 ((derived-mode-p 'magit-mode)
                  (when (fboundp 'magit-refresh)
                    (magit-refresh)))
                 (t nil))
              (error nil))))))
    (dolist (cb cbs)
      (ignore-errors (funcall cb job)))
    ;; Preserve window layout / selection when practical.
    (ignore-errors (set-window-configuration wconf))
    (when (window-live-p selected)
      (select-window selected))))

(defun +git-sync-cancel (repository-id)
  "Cancel the active sync job for REPOSITORY-ID when present.
Stops only the process/Forge request owned by that job.  Queued jobs
are removed from the waiter queue without touching concurrency slots.
During Forge pull, the lock is retained until the adapter completes."
  (interactive
   (list (+git-sync--resolve-repository-id)))
  (let ((job (gethash repository-id +git-sync--jobs)))
    (unless job
      (user-error "No sync job for %s" repository-id))
    (unless (memq (+git-sync-job-state job) +git-sync--active-states)
      (user-error "Sync not active for %s" repository-id))
    (setf (+git-sync-job-cancel-flag job) t)
    (cond
     ;; Drop from the global waiter queue when still queued.
     ((eq (+git-sync-job-state job) 'queued)
      (setq +git-sync--global-waiters
            (delq repository-id +git-sync--global-waiters))
      (+git-sync--finish job 'cancelled "cancelled"))
     (t
      (when-let ((proc (+git-sync-job-process job)))
        (when (process-live-p proc)
          (ignore-errors (interrupt-process proc))
          (run-at-time
           0.5 nil
           (lambda ()
             (when (and (process-live-p proc)
                        (+git-sync-job-cancel-flag job)
                        (eq (+git-sync-job-process job) proc))
               (ignore-errors (delete-process proc)))))))
      (if-let ((req (+git-sync-job-forge-request job)))
          ;; Adapter must call ON-COMPLETE; keep lock until then.
          (when-let ((cancel (plist-get req :cancel)))
            (ignore-errors (funcall cancel)))
        ;; Between async steps with no live process/request.
        (unless (+git-sync-job-process job)
          (when (memq (+git-sync-job-state job)
                      '(preparing-mirror fetching-mirror fetching-checks
                        publishing))
            (+git-sync--finish job 'cancelled "cancelled"))))))
    job))

;; ---------------------------------------------------------------------------
;; Start / coalesce / concurrency
;; ---------------------------------------------------------------------------

(defun +git-sync--job-active-p (job)
  "Return non-nil when JOB is mid-pipeline."
  (memq (+git-sync-job-state job) +git-sync--active-states))

(defun +git-sync--merge-waiters (job buffer callback)
  "Add BUFFER and CALLBACK to JOB waiters without duplicates."
  (when (and buffer (buffer-live-p buffer))
    (unless (memq buffer (+git-sync-job-waiting-buffers job))
      (setf (+git-sync-job-waiting-buffers job)
            (cons buffer (+git-sync-job-waiting-buffers job)))))
  (when callback
    (unless (memq callback (+git-sync-job-waiting-callbacks job))
      (setf (+git-sync-job-waiting-callbacks job)
            (cons callback (+git-sync-job-waiting-callbacks job))))))

(defun +git-sync--begin-pipeline (job)
  "Run the sync pipeline for JOB after a slot and lock are held."
  (setf (+git-sync-job-cancel-flag job) nil)
  (setf (+git-sync-job-started-at job) (float-time))
  (setf (+git-sync-job-finished-at job) nil)
  (setf (+git-sync-job-last-error job) nil)
  (setf (+git-sync-job-backup-dir job) nil)
  (setf (+git-sync-job-forge-request job) nil)
  (let* ((repository-id (+git-sync-job-repository-id job))
         (pair (+git-sync-load-state repository-id))
         (state (car pair))
         (gen (+git-sync--state-generation state)))
    (+git-sync--recover-incomplete-publish repository-id)
    (setf (+git-sync-job-previous-generation job) gen)
    (setf (+git-sync-job-candidate-generation job) (1+ gen))
    (setf (+git-sync-job-last-success job)
          (+git-sync--alist-get "last-success" state))
    (condition-case err
        (progn
          (setf (+git-sync-job-state job) 'preparing-mirror)
          (+git-sync--start-prepare job))
      (error
       (+git-sync--finish job 'failed (+git-sync--sanitize-error err))))))

(defun +git-sync--try-start-job (job)
  "Acquire lock and begin JOB, or finish as failed if locked elsewhere.
Does not persist sync-state when the lock is owned by another process."
  (let ((repository-id (+git-sync-job-repository-id job)))
    (let ((nonce (+git-sync-acquire-lock repository-id)))
      (if (not nonce)
          (progn
            (setf (+git-sync-job-stale-p job) t)
            (setf (+git-sync-job-persist-failure-p job) nil)
            (+git-sync--finish
             job 'failed "already syncing elsewhere"))
        (setf (+git-sync-job-lock-held-p job) t)
        (setf (+git-sync-job-lock-nonce job) nonce)
        (setf (+git-sync-job-persist-failure-p job) t)
        (+git-sync--begin-pipeline job)))))

(defun +git-sync--pump-global-queue ()
  "Start queued jobs up to `+git-sync-max-concurrent'."
  (while (and +git-sync--global-waiters
              (< +git-sync--global-active +git-sync-max-concurrent))
    (let* ((id (pop +git-sync--global-waiters))
           (job (gethash id +git-sync--jobs)))
      (when (and job (eq (+git-sync-job-state job) 'queued))
        (cl-incf +git-sync--global-active)
        (setf (+git-sync-job-slot-held-p job) t)
        (+git-sync--try-start-job job)))))

(defun +git-sync-start (repository-id &optional buffer callback)
  "Start or join an explicit sync for REPOSITORY-ID.
BUFFER when live is refreshed once on completion.  CALLBACK is called
with the job on completion.  Duplicate in-process requests coalesce.
Returns the job object.  Never blocks the UI thread on network I/O
or local mirror cloning."
  (unless (and (stringp repository-id) (not (string-empty-p repository-id)))
    (user-error "No canonical repository to synchronize"))
  (let ((job (+git-sync--get-job repository-id)))
    (if (+git-sync--job-active-p job)
        (progn
          (+git-sync--merge-waiters job buffer callback)
          job)
      ;; Fresh start.
      (setf (+git-sync-job-state job) 'queued)
      (setf (+git-sync-job-attempt-id job) (+git-sync--next-attempt-id))
      (setf (+git-sync-job-cancel-flag job) nil)
      (setf (+git-sync-job-process job) nil)
      (setf (+git-sync-job-forge-request job) nil)
      (setf (+git-sync-job-waiting-buffers job) nil)
      (setf (+git-sync-job-waiting-callbacks job) nil)
      (setf (+git-sync-job-slot-held-p job) nil)
      (setf (+git-sync-job-lock-held-p job) nil)
      (setf (+git-sync-job-lock-nonce job) nil)
      (setf (+git-sync-job-persist-failure-p job) t)
      (setf (+git-sync-job-remote-url job)
            (+git-sync--choose-remote-url repository-id))
      (setf (+git-sync-job-seed-root job)
            (+git-sync--choose-seed-root repository-id))
      (unless (or (+git-sync-job-remote-url job)
                  (+git-sync-job-seed-root job))
        (setf (+git-sync-job-state job) 'idle)
        (user-error "No usable remote or local context for %s" repository-id))
      (+git-sync--merge-waiters job buffer callback)
      (let ((repo (+git-store-get-repository repository-id)))
        (when repo
          (setf (+git-store-repository-sync-process repo) job)))
      (if (< +git-sync--global-active +git-sync-max-concurrent)
          (progn
            (cl-incf +git-sync--global-active)
            (setf (+git-sync-job-slot-held-p job) t)
            (+git-sync--try-start-job job))
        (setq +git-sync--global-waiters
              (append +git-sync--global-waiters (list repository-id))))
      job)))

(defun +git-sync-wait (repository-id &optional timeout)
  "Block until the sync job for REPOSITORY-ID leaves an active state.
TIMEOUT defaults to 30 seconds.  Intended for tests."
  (let* ((timeout (or timeout 30.0))
         (deadline (+ (float-time) timeout))
         (job (+git-sync--get-job repository-id)))
    (while (and (+git-sync--job-active-p job)
                (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (setq job (+git-sync--get-job repository-id)))
    job))

;; ---------------------------------------------------------------------------
;; Status / inspection
;; ---------------------------------------------------------------------------

(defun +git-sync-status (repository-id)
  "Return a read-only status plist for REPOSITORY-ID.
May recover a crashed publication only when no live lock is held."
  (let* ((job (gethash repository-id +git-sync--jobs))
         (pair (+git-sync-load-state repository-id))
         (state (car pair))
         (malformed (cdr pair))
         (gen (+git-sync--state-generation state))
         (forge (or (and job (+git-sync-job-forge-status job))
                    (intern (or (+git-sync--alist-get "forge-status" state)
                                "unavailable"))))
         (last-success (or (and job (+git-sync-job-last-success job))
                           (+git-sync--alist-get "last-success" state)))
         (err (or (and job (+git-sync-job-last-error job))
                  (+git-sync--alist-get "last-error" state)))
         (job-state (if job (+git-sync-job-state job) 'idle))
         (age-stale
          (and (numberp last-success)
               (numberp +git-sync-stale-after-seconds)
               (> +git-sync-stale-after-seconds 0)
               (> (- (float-time) last-success)
                  +git-sync-stale-after-seconds)))
         (stale (or (and job (+git-sync-job-stale-p job))
                    (memq forge '(failed stale))
                    age-stale
                    (and malformed t)
                    (memq job-state '(failed cancelled)))))
    (list :repository-id repository-id
          :state job-state
          :generation gen
          :last-success last-success
          :stale stale
          :mirror (+git-sync-published-mirror-directory repository-id)
          :mirror-exists (+git-sync--mirror-exists-p repository-id)
          :forge forge
          :error err
          :malformed malformed
          :provider (or (and job (+git-sync-job-provider job))
                        (intern (or (+git-sync--alist-get "provider" state)
                                    "generic")))
          :pr-refs (or (and job (+git-sync-job-pr-refs-status job))
                       (intern (or (+git-sync--alist-get "pr-refs-status" state)
                                   "unavailable"))))))

(defun +git-sync-describe (repository-id)
  "Return an ASCII-safe compact status string for REPOSITORY-ID."
  (let* ((st (+git-sync-status repository-id))
         (ls (plist-get st :last-success))
         (ls-str (if (numberp ls)
                     (format-time-string "%Y-%m-%d %H:%M:%S" ls)
                   "-"))
         (err (plist-get st :error)))
    (format
     (concat "repo: %s\n"
             "state: %s\n"
             "generation: %s\n"
             "last success: %s\n"
             "stale: %s\n"
             "mirror: %s\n"
             "forge: %s\n"
             "error: %s\n")
     (plist-get st :repository-id)
     (plist-get st :state)
     (plist-get st :generation)
     ls-str
     (if (plist-get st :stale) "yes" "no")
     (plist-get st :mirror)
     (plist-get st :forge)
     (or err "-"))))

;; ---------------------------------------------------------------------------
;; Explicit mirror -> context import (Phase 5 facing)
;; ---------------------------------------------------------------------------

(defun +git-sync--validate-import-ref (local-ref)
  "Signal `user-error' unless LOCAL-REF is a safe refs/git-review/ ref."
  (unless (and (stringp local-ref)
               (string-prefix-p "refs/git-review/" local-ref)
               (not (string-match-p "\\.\\." local-ref))
               (not (string-suffix-p "/" local-ref))
               (not (string-match-p "[\n\r]" local-ref)))
    (user-error "Import destination must be under refs/git-review/: %s"
                local-ref))
  (let* ((result (+git-sync--call-git
                  default-directory
                  "check-ref-format" "--allow-onelevel" local-ref))
         (exit (car result)))
    (unless (eq exit 0)
      (user-error "Invalid import ref name: %s" local-ref)))
  local-ref)

(defun +git-sync-import-ref (repository-id context-id ref &optional local-ref)
  "Import REF from the published mirror into CONTEXT-ID.
Writes a namespaced local ref under refs/git-review/ only.
LOCAL-REF when provided must also be under refs/git-review/ and pass
`git check-ref-format'.  Performs no network access.  Never checks out,
merges, or modifies the worktree.  Rejects a context belonging to
another repository."
  (let* ((ctx (+git-store-get-context context-id))
         (local-ref (or local-ref
                        (format "refs/git-review/%s"
                                (replace-regexp-in-string
                                 "\\`refs/" "" ref))))
         (mirror (+git-sync-published-mirror-directory repository-id))
         (root nil))
    (+git-sync--validate-import-ref local-ref)
    (unless ctx
      (user-error "Unknown local context %s" context-id))
    (unless (equal (+git-store-local-context-repository-id ctx)
                   repository-id)
      (user-error "Context %s does not belong to %s"
                  context-id repository-id))
    (unless (+git-store-context-eligible-p ctx repository-id)
      (user-error "Context %s is not eligible" context-id))
    (unless (+git-sync--mirror-exists-p repository-id)
      (user-error "No published mirror for %s" repository-id))
    (setq root (+git-store-local-context-root ctx))
    (let ((oid (+git-sync--git-ok mirror "rev-parse" "--verify" ref)))
      (+git-sync--git-ok
       root "fetch" "--no-tags" mirror
       (format "%s:%s" ref local-ref))
      (list :oid (string-trim oid)
            :local-ref local-ref
            :context-id context-id
            :repository-id repository-id))))

;; ---------------------------------------------------------------------------
;; Repository resolution for commands
;; ---------------------------------------------------------------------------

(defun +git-sync--resolve-repository-id (&optional root)
  "Resolve the canonical repository ID for the current review or ROOT.
Uses `+git-review-target' struct accessors when present; never
`plist-get' on the target object."
  (or
   (and (bound-and-true-p +git-review-target)
        (let ((target +git-review-target))
          (cond
           ((and (fboundp '+git-review-target-p)
                 (+git-review-target-p target))
            (or (+git-review-target-repository-id target)
                (and (+git-review-target-context-id target)
                     (when-let ((ctx (+git-store-get-context
                                      (+git-review-target-context-id
                                       target))))
                       (+git-store-local-context-repository-id ctx)))))
           ((and (consp target) (keywordp (car target)))
            (or (plist-get target :repository-id)
                (and (plist-get target :context-id)
                     (when-let ((ctx (+git-store-get-context
                                      (plist-get target :context-id))))
                       (+git-store-local-context-repository-id ctx))))))))
   (and (bound-and-true-p +git-pr--model)
        (fboundp '+git-pr-repository-id)
        (+git-pr-repository-id +git-pr--model))
   (let* ((root (or root
                    (and (fboundp 'magit-toplevel) (magit-toplevel))
                    (and (fboundp 'projectile-project-root)
                         (ignore-errors (projectile-project-root)))
                    default-directory))
          (ctx (and root
                    (file-directory-p root)
                    (ignore-errors (+git-store-context-for-root root)))))
     (and ctx (+git-store-local-context-repository-id ctx)))))

;; ---------------------------------------------------------------------------
;; User commands
;; ---------------------------------------------------------------------------

(defun +git/sync (&optional repository-id)
  "Synchronize the current review repository (explicit network).
Does not change windows, selected buffer, or Evil state."
  (interactive)
  (let* ((id (or repository-id (+git-sync--resolve-repository-id)))
         (buf (current-buffer)))
    (unless id
      (user-error "No canonical repository to synchronize"))
    (let ((job (+git-sync-start id buf)))
      (message "Sync %s: %s" id (+git-sync-job-state job))
      job)))

(defun +git/sync-all ()
  "Synchronize allowlisted repositories with bounded concurrency.
An empty `+git-sync-active-repositories' synchronizes nothing."
  (interactive)
  (cond
   ((null +git-sync-active-repositories)
    (message "Sync-all: allowlist empty; nothing to synchronize")
    nil)
   (t
    (let ((ids (cl-remove-if-not
                (lambda (id)
                  (and (stringp id)
                       (+git-store-get-repository id)))
                +git-sync-active-repositories)))
      (if (null ids)
          (progn
            (message "Sync-all: no allowlisted registered repositories")
            nil)
        (dolist (id ids)
          (+git-sync-start id))
        (message "Sync-all: queued %d repositories (max concurrent %d)"
                 (length ids) +git-sync-max-concurrent)
        ids)))))

(defun +git/sync-status (&optional repository-id)
  "Show compact synchronization status for the current repository."
  (interactive)
  (let ((id (or repository-id (+git-sync--resolve-repository-id))))
    (unless id
      (user-error "No canonical repository"))
    (let ((text (+git-sync-describe id)))
      (message "%s" (replace-regexp-in-string "\n" " | " text))
      (when (called-interactively-p 'interactive)
        (with-current-buffer (get-buffer-create "*Git Sync Status*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert text)
            (goto-char (point-min))
            (view-mode 1))
          (display-buffer (current-buffer))))
      text)))

(defun +git/sync-cancel (&optional repository-id)
  "Cancel synchronization for the current repository."
  (interactive)
  (let ((id (or repository-id (+git-sync--resolve-repository-id))))
    (unless id
      (user-error "No canonical repository"))
    (+git-sync-cancel id)))

(provide 'init-git-sync)

;;; init-git-sync.el ends here
