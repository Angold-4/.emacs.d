;;; git-review-sync-test.el --- Phase 4 mirror/sync integration tests -*- lexical-binding: t -*-

;;; Commentary:
;; Explicit synchronization, shared bare mirrors, locking, generation
;; publication, Forge adapter stubs, and local import.  Uses only local
;; bare remotes and stubs; never contacts a real network endpoint.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'git-review-fixtures)
(require 'git-review-baseline)
(require 'init-git-store)
(require 'init-git-sync)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun git-review-sync--set-fetch-url (root url &optional remote)
  "Configure ROOT's REMOTE (default origin) fetch URL to URL."
  (let ((remote (or remote "origin")))
    (ignore-errors
      (git-review-fixtures--git-ok root "remote" "remove" remote))
    (git-review-fixtures--git-ok root "remote" "add" remote url)))

(defun git-review-sync--forge-stub (status &optional delay)
  "Return a Forge adapter that completes with STATUS.
Optional DELAY seconds defers completion.  Returns a cancellable handle."
  (lambda (_id on-complete)
    (cl-incf +git-sync--forge-pull-count)
    (let* ((done nil)
           (finish
            (lambda (st)
              (unless done
                (setq done t)
                (funcall on-complete st))))
           (cancel (lambda () (funcall finish 'cancelled))))
      (if delay
          (run-at-time delay nil (lambda () (funcall finish status)))
        (funcall finish status))
      (list :cancel cancel))))

(defun git-review-sync--with-dirs (fn)
  "Call FN with disposable registry/cache directory; reset sync jobs."
  (let* ((reg (make-temp-file "git-review-sync-reg " t))
         (+git-store-registry-directory reg)
         (+git-sync-test-remote-override nil)
         (+git-sync-active-repositories nil)
         (+git-sync-max-concurrent 2)
         (+git-sync-forge-pull-function
          (git-review-sync--forge-stub 'unavailable))
         (+git-sync--git-fetch-count 0)
         (+git-sync--forge-pull-count 0)
         (+git-sync--waiting-refresh-count 0)
         (+git-sync--attempt-counter 0))
    (unwind-protect
        (progn
          (+git-store-reset-registry)
          (+git-sync-reset-jobs)
          (funcall fn reg))
      (ignore-errors (+git-sync-reset-jobs))
      (ignore-errors (+git-store-reset-registry))
      (ignore-errors (delete-directory reg t)))))

(defun git-review-sync--make-upstream (prefix)
  "Create a bare upstream with a commit, tag, and branch; return (BARE . OID)."
  (let* ((parent (make-temp-file (format "git-review sync-%s " prefix) t))
         (work (expand-file-name "upstream work" parent))
         (bare (expand-file-name "upstream.git" parent)))
    (git-review-fixtures--init-repo work)
    (git-review-fixtures--write-file work "README.md" "v1\n")
    (git-review-fixtures--git-ok work "add" "-A")
    (git-review-fixtures--git-ok work "commit" "-m" "initial")
    (git-review-fixtures--git-ok work "tag" "v1.0")
    (git-review-fixtures--git-ok work "branch" "feature")
    (git-review-fixtures--git-ok work "clone" "--bare" work bare)
    (let ((oid (string-trim
                (git-review-fixtures--git-ok work "rev-parse" "HEAD"))))
      (cons bare oid))))

(defun git-review-sync--clone-with-identity (bare url prefix)
  "Clone BARE to a worktree, rewrite fetch URL to URL, return root."
  (let* ((parent (make-temp-file (format "git-review sync-%s " prefix) t))
         (dst (expand-file-name (format "%s repo" prefix) parent)))
    (git-review-fixtures--git-ok bare "clone" bare dst)
    (git-review-fixtures--git-ok dst "config" "user.name"
                                 git-review-fixtures-test-name)
    (git-review-fixtures--git-ok dst "config" "user.email"
                                 git-review-fixtures-test-email)
    (git-review-fixtures--git-ok dst "config" "commit.gpgsign" "false")
    (ignore-errors
      (git-review-fixtures--git-ok dst "remote" "remove" "origin"))
    (git-review-fixtures--git-ok dst "remote" "add" "origin" url)
    dst))

(defun git-review-sync--cleanup-root (root)
  "Delete ROOT's temporary parent directory."
  (when (and root (file-directory-p root))
    (ignore-errors
      (delete-directory (file-name-directory (directory-file-name root)) t))))

(defun git-review-sync--register (root url &optional bare-for-fetch)
  "Register ROOT with hosted URL; optionally override fetch to BARE-FOR-FETCH."
  (let ((ctx (+git-store-register-root root)))
    (when bare-for-fetch
      (let ((id (+git-store-local-context-repository-id ctx)))
        (setq +git-sync-test-remote-override
              (cons (cons id bare-for-fetch)
                    +git-sync-test-remote-override))))
    ctx))

(defun git-review-sync--wait-success (repository-id &optional timeout)
  "Wait for sync of REPOSITORY-ID and assert success."
  (let ((job (+git-sync-wait repository-id timeout)))
    (should (eq (+git-sync-job-state job) 'succeeded))
    job))

;; ---------------------------------------------------------------------------
;; Paths / identity
;; ---------------------------------------------------------------------------

(ert-deftest git-review-sync-stable-sha256-paths ()
  "Mirror paths hash the full canonical id; hosts do not collide."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((a "github.com/org/dragon")
            (b "gitlab.com/org/dragon")
            (c "github.com/org/other")
            (ha (+git-sync--repository-hash a))
            (hb (+git-sync--repository-hash b))
            (hc (+git-sync--repository-hash c)))
       (should (= (length ha) 64))
       (should-not (equal ha hb))
       (should-not (equal ha hc))
       (should (equal (+git-sync-mirror-directory a)
                      (expand-file-name
                       (format "%s/mirror.git" ha)
                       +git-store-registry-directory)))
       (should (string-suffix-p "sync-state.json"
                                (+git-sync-state-file a)))
       (should (string-suffix-p "sync.lock.d"
                                (+git-sync-lock-file a)))))))

(ert-deftest git-review-sync-same-origin-one-mirror ()
  "Same-origin clones share one mirror; different repos do not."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "share"))
            (bare (car up))
            (url "git@github.com:org/sync-share.git")
            (a (git-review-sync--clone-with-identity bare url "a"))
            (b (git-review-sync--clone-with-identity bare url "b"))
            (other-url "git@github.com:org/other.git")
            (c (git-review-sync--clone-with-identity bare other-url "c")))
       (unwind-protect
           (let* ((ctx-a (git-review-sync--register a url bare))
                  (ctx-b (git-review-sync--register b url bare))
                  (ctx-c (git-review-sync--register c other-url bare))
                  (id-a (+git-store-local-context-repository-id ctx-a))
                  (id-b (+git-store-local-context-repository-id ctx-b))
                  (id-c (+git-store-local-context-repository-id ctx-c)))
             (should (equal id-a id-b))
             (should-not (equal id-a id-c))
             (should (equal (+git-sync-mirror-directory id-a)
                            (+git-sync-mirror-directory id-b)))
             (should-not (equal (+git-sync-mirror-directory id-a)
                                (+git-sync-mirror-directory id-c))))
         (git-review-sync--cleanup-root a)
         (git-review-sync--cleanup-root b)
         (git-review-sync--cleanup-root c)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-ordinary-ops-do-not-create-mirror ()
  "Registry/open/tree/diff/refresh never create a mirror."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (git-review-fixtures-with-repo
      "no-mirror"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (git-review-sync--set-fetch-url
         root "git@github.com:org/no-mirror.git")
        (let* ((ctx (+git-store-register-root root))
               (id (+git-store-local-context-repository-id ctx))
               (mirror (+git-sync-mirror-directory id)))
          (should-not (file-directory-p mirror))
          (let ((default-directory (file-name-as-directory root)))
            (require 'magit)
            (let ((buf (+git-review-open-worktree)))
              (should (buffer-live-p buf))
              (should-not (file-directory-p mirror))
              (with-current-buffer buf
                (+git-review-refresh)
                (should-not (file-directory-p mirror))
                (let ((tree (+git-review-open-changes-tree)))
                  (should (buffer-live-p tree))
                  (should-not (file-directory-p mirror))
                  (kill-buffer tree)))
              (kill-buffer buf)))
          (should-not (file-directory-p mirror))
          (git-review-baseline-cleanup-repo-buffers root)))))))

;; ---------------------------------------------------------------------------
;; Mirror seed / fetch
;; ---------------------------------------------------------------------------

(ert-deftest git-review-sync-bare-seed-local-only ()
  "Explicit mirror seed uses local Git only; no network."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "seed"))
            (bare (car up))
            (oid (cdr up))
            (url "git@github.com:org/seed.git")
            (work (git-review-sync--clone-with-identity bare url "seed-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (mirror (+git-sync-ensure-mirror id work)))
             (should (file-directory-p mirror))
             (should (file-exists-p (expand-file-name "HEAD" mirror)))
             (should (equal
                      (string-trim
                       (+git-sync--git-ok mirror "rev-parse" "HEAD"))
                      oid))
             ;; No alternates file: cache deletion must not break clones.
             (should-not
              (file-exists-p
               (expand-file-name "objects/info/alternates" mirror)))
             (should (file-directory-p work)))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-fetch-branches-and-tags ()
  "Warm sync against a local bare fixture fetches branches and tags."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "fetch"))
            (bare (car up))
            (url "git@github.com:org/fetch.git")
            (work (git-review-sync--clone-with-identity bare url "fetch-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (setq +git-sync--git-fetch-count 0
                   +git-sync--forge-pull-count 0)
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should (= +git-sync--git-fetch-count 1))
             (should (= +git-sync--forge-pull-count 1))
             (let ((mirror (+git-sync-mirror-directory id)))
               (should (file-directory-p mirror))
               (should (string-match-p
                        "refs/heads/main"
                        (+git-sync--git-ok mirror "show-ref")))
               (should (string-match-p
                        "refs/heads/feature"
                        (+git-sync--git-ok mirror "show-ref")))
               (should (string-match-p
                        "refs/tags/v1.0"
                        (+git-sync--git-ok mirror "show-ref"))))
             (should (= (plist-get (+git-sync-status id) :generation) 1)))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-provider-refspecs ()
  "GitHub/GitLab include PR/MR refs; generic reports unavailable."
  (let* ((gh (+git-sync-provider-refspecs "github.com/org/x"))
         (gl (+git-sync-provider-refspecs "gitlab.com/group/x"))
         (gen (+git-sync-provider-refspecs "git.example.com/org/x"))
         (local (+git-sync-provider-refspecs "local:/tmp/x")))
    (should (eq (cdr gh) 'ok))
    (should (cl-find "+refs/pull/*/head:refs/pull/*/head" (car gh)
                     :test #'equal))
    (should (cl-find "+refs/pull/*/merge:refs/pull/*/merge" (car gh)
                     :test #'equal))
    (should (eq (cdr gl) 'ok))
    (should (cl-find
             "+refs/merge-requests/*/head:refs/merge-requests/*/head"
             (car gl) :test #'equal))
    (should (eq (cdr gen) 'unavailable))
    (should (eq (cdr local) 'unavailable))
    (should (cl-find "+refs/heads/*:refs/heads/*" (car gen) :test #'equal))
    (should-not (cl-find-if (lambda (s) (string-match-p "pull" s))
                            (car gen)))))

;; ---------------------------------------------------------------------------
;; Coalescing / locking
;; ---------------------------------------------------------------------------

(ert-deftest git-review-sync-in-process-coalesce-one-fetch ()
  "Duplicate in-process requests join one job and issue one fetch."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "coal"))
            (bare (car up))
            (url "git@github.com:org/coal.git")
            (work (git-review-sync--clone-with-identity bare url "coal-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             ;; Delay forge so both requests join before completion.
             (setq +git-sync-forge-pull-function
                   (git-review-sync--forge-stub 'ok 0.05))
             (setq +git-sync--git-fetch-count 0
                   +git-sync--forge-pull-count 0)
             (let* ((cb1-count 0)
                    (cb2-count 0)
                    (cb1 (lambda (_job) (cl-incf cb1-count)))
                    (cb2 (lambda (_job) (cl-incf cb2-count)))
                    (j1 (+git-sync-start id nil cb1))
                    (j2 (+git-sync-start id nil cb2)))
               (should (eq j1 j2))
               (git-review-sync--wait-success id 10)
               (should (= +git-sync--git-fetch-count 1))
               (should (= +git-sync--forge-pull-count 1))
               (should (= cb1-count 1))
               (should (= cb2-count 1))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-two-contexts-one-canonical-job ()
  "Two same-origin contexts concurrently request one canonical sync."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "twoctx"))
            (bare (car up))
            (url "git@github.com:org/twoctx.git")
            (a (git-review-sync--clone-with-identity bare url "ta"))
            (b (git-review-sync--clone-with-identity bare url "tb")))
       (unwind-protect
           (let* ((ctx-a (git-review-sync--register a url bare))
                  (ctx-b (git-review-sync--register b url bare))
                  (id (+git-store-local-context-repository-id ctx-a)))
             (should (equal id (+git-store-local-context-repository-id ctx-b)))
             (setq +git-sync--git-fetch-count 0)
             (let ((j1 (+git-sync-start id))
                   (j2 (+git-sync-start id)))
               (should (eq j1 j2))
               (git-review-sync--wait-success id)
               (should (= +git-sync--git-fetch-count 1))))
         (git-review-sync--cleanup-root a)
         (git-review-sync--cleanup-root b)
         (git-review-sync--cleanup-root bare))))))

(defun git-review-sync--write-lock (repository-id pid &optional timestamp)
  "Create an exclusive lock directory for REPOSITORY-ID owned by PID."
  (+git-sync--ensure-cache-dir repository-id)
  (let* ((dir (+git-sync-lock-file repository-id))
         (nonce (format "test-nonce-%s" pid)))
    (when (file-directory-p dir)
      (delete-directory dir t))
    (make-directory dir)
    (with-temp-file (+git-sync-lock-owner-file repository-id)
      (insert (json-encode
               `(("pid" . ,pid)
                 ("host" . ,(system-name))
                 ("timestamp" . ,(or timestamp (float-time)))
                 ("nonce" . ,nonce)))
             "\n"))
    nonce))

(ert-deftest git-review-sync-cross-process-lock-respected ()
  "A live foreign lock blocks fetch; a dead-owner lock is recovered."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "lock"))
            (bare (car up))
            (url "git@github.com:org/lock.git")
            (work (git-review-sync--clone-with-identity bare url "lock-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (lock (+git-sync-lock-file id)))
             (+git-sync--ensure-cache-dir id)
             ;; Live lock owned by this PID: must not steal.
             (git-review-sync--write-lock id (emacs-pid))
             (setq +git-sync--git-fetch-count 0)
             (let ((job (+git-sync-start id))
                   (state-before
                    (car (+git-sync-load-state id))))
               (+git-sync-wait id 5)
               (should (eq (+git-sync-job-state job) 'failed))
               (should (string-match-p "elsewhere"
                                       (+git-sync-job-last-error job)))
               (should (= +git-sync--git-fetch-count 0))
               ;; Must not rewrite sync-state without owning the lock.
               (should (equal (car (+git-sync-load-state id))
                              state-before)))
             ;; Confirmed dead owner: acquisition reclaims it automatically.
             (let ((+git-sync-stale-lock-seconds 0))
               (git-review-sync--write-lock id 2147483646
                                            (- (float-time) 10))
               (let ((acq (+git-sync-try-acquire-lock id)))
                 (should (eq (car acq) 'acquired))
                 (+git-sync-release-lock id (cdr acq))))
             ;; Unknown lock content: never steal / never auto-clear.
             (make-directory lock t)
             (with-temp-file (+git-sync-lock-owner-file id)
               (insert "not-json-lock-content"))
             (should (eq (car (+git-sync-try-acquire-lock id)) 'busy))
             (should-not (+git-sync-clear-stale-lock id)))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-lock-released-on-outcomes ()
  "Lock is released after success, git failure, forge failure, cancel."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "rel"))
            (bare (car up))
            (url "git@github.com:org/rel.git")
            (work (git-review-sync--clone-with-identity bare url "rel-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (lock (+git-sync-lock-file id)))
             ;; Success.
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should-not (file-directory-p lock))
             ;; Git failure: point override at missing path.
             (setq +git-sync-test-remote-override
                   (list (cons id "/tmp/git-review-missing-remote-xyz.git")))
             (+git-sync-reset-jobs)
             (+git-sync-start id)
             (+git-sync-wait id 10)
             (should-not (file-directory-p lock))
             ;; Restore remote; forge failure.
             (setq +git-sync-test-remote-override (list (cons id bare)))
             (setq +git-sync-forge-pull-function
                   (git-review-sync--forge-stub 'failed))
             (+git-sync-reset-jobs)
             (+git-sync-start id)
             (+git-sync-wait id 10)
             (should-not (file-directory-p lock))
             (should (eq (plist-get (+git-sync-status id) :forge) 'failed))
             ;; Cancel during delayed forge.
             (setq +git-sync-forge-pull-function
                   (git-review-sync--forge-stub 'ok 0.2))
             (+git-sync-reset-jobs)
             (+git-sync-start id)
             (let ((deadline (+ (float-time) 2)))
               (while (and (< (float-time) deadline)
                           (not (eq (+git-sync-job-state
                                     (+git-sync--get-job id))
                                    'pulling-forge)))
                 (accept-process-output nil 0.02)))
             (+git-sync-cancel id)
             (+git-sync-wait id 5)
             (should-not (file-directory-p lock))
             (should (eq (+git-sync-job-state (+git-sync--get-job id))
                         'cancelled)))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-clean-exit-releases-owned-state ()
  "A normal Emacs exit leaves durable data but no process-owned lock."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((id "github.com/org/clean-exit")
            (+git-sync--jobs (make-hash-table :test #'equal))
            (job (+git-sync--get-job id))
            (nonce (+git-sync-acquire-lock id))
            (candidate (+git-sync-candidate-directory id))
            (mirror (+git-sync-mirror-directory id))
            (state-file (+git-sync-state-file id)))
       (should nonce)
       (make-directory candidate t)
       (make-directory mirror t)
       (with-temp-file state-file
         (insert "{\"generation\":1}\n"))
       (setf (+git-sync-job-candidate-dir job) candidate)
       (setf (+git-sync-job-lock-held-p job) t)
       (setf (+git-sync-job-lock-nonce job) nonce)
       (+git-sync--cleanup-on-emacs-exit)
       (should-not (file-directory-p (+git-sync-lock-file id)))
       (should-not (file-directory-p candidate))
       (should (file-directory-p mirror))
       (should (file-readable-p state-file))))))

;; ---------------------------------------------------------------------------
;; Generation / failure semantics
;; ---------------------------------------------------------------------------

(ert-deftest git-review-sync-success-publishes-generation ()
  "Complete pipeline atomically publishes generation 1 then 2."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "gen"))
            (bare (car up))
            (url "git@github.com:org/gen.git")
            (work (git-review-sync--clone-with-identity bare url "gen-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should (= (plist-get (+git-sync-status id) :generation) 1))
             (+git-sync-reset-jobs)
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should (= (plist-get (+git-sync-status id) :generation) 2))
             (should-not
              (file-directory-p (+git-sync-candidate-directory id))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-git-failure-preserves-generation ()
  "Git failure keeps the previous published mirror and generation."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "gfail"))
            (bare (car up))
            (oid (cdr up))
            (url "git@github.com:org/gfail.git")
            (work (git-review-sync--clone-with-identity bare url "gfail-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (mirror (+git-sync-mirror-directory id)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should (= (plist-get (+git-sync-status id) :generation) 1))
             (setq +git-sync-test-remote-override
                   (list (cons id "/tmp/git-review-no-such-remote.git")))
             (+git-sync-reset-jobs)
             (+git-sync-start id)
             (let ((job (+git-sync-wait id 10)))
               (should (eq (+git-sync-job-state job) 'failed))
               (should (= (plist-get (+git-sync-status id) :generation) 1))
               (should (equal
                        (string-trim
                         (+git-sync--git-ok mirror "rev-parse" "HEAD"))
                        oid))
               (should-not
                (file-directory-p (+git-sync-candidate-directory id)))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-forge-failure-preserves-generation ()
  "Forge failure after Git success preserves published generation."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "ffail"))
            (bare (car up))
            (url "git@github.com:org/ffail.git")
            (work (git-review-sync--clone-with-identity bare url "ffail-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (setq +git-sync-forge-pull-function
                   (git-review-sync--forge-stub 'failed))
             (+git-sync-reset-jobs)
             (+git-sync-start id)
             (let ((job (+git-sync-wait id 10)))
               (should (eq (+git-sync-job-state job) 'failed))
               (should (= (plist-get (+git-sync-status id) :generation) 1))
               (should (eq (plist-get (+git-sync-status id) :forge) 'failed))
               (should (plist-get (+git-sync-status id) :stale))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-cancel-preserves-generation ()
  "Cancellation preserves prior generation and removes candidate."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "can"))
            (bare (car up))
            (url "git@github.com:org/can.git")
            (work (git-review-sync--clone-with-identity bare url "can-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (setq +git-sync-forge-pull-function
                   (git-review-sync--forge-stub 'ok 0.25))
             (+git-sync-reset-jobs)
             (+git-sync-start id)
             (let ((deadline (+ (float-time) 3)))
               (while (and (< (float-time) deadline)
                           (not (eq (+git-sync-job-state
                                     (+git-sync--get-job id))
                                    'pulling-forge)))
                 (accept-process-output nil 0.02)))
             (+git-sync-cancel id)
             (+git-sync-wait id 5)
             (should (eq (+git-sync-job-state (+git-sync--get-job id))
                         'cancelled))
             (should (= (plist-get (+git-sync-status id) :generation) 1))
             (should-not
              (file-directory-p (+git-sync-candidate-directory id))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-atomic-state-rename-failure ()
  "Failed atomic state rename preserves the previous valid file."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((id "github.com/org/atomic")
            (file (+git-sync-state-file id)))
       (+git-sync--ensure-cache-dir id)
       (+git-sync-save-state
        id
        `(("version" . 1)
          ("generation" . 3)
          ("last-success" . 1.0)
          ("last-attempt" . 1.0)
          ("last-error" . nil)
          ("forge-status" . "current")
          ("provider" . "github")
          ("pr-refs-status" . "ok")
          ("mirror-format" . 1)))
       (should (= (+git-sync--state-generation
                   (car (+git-sync-load-state id)))
                  3))
       (cl-letf (((symbol-function 'rename-file)
                  (lambda (&rest _)
                    (error "simulated rename failure"))))
         (should-error
          (+git-sync-save-state
           id
           `(("version" . 1)
             ("generation" . 99)
             ("last-success" . 2.0)
             ("last-attempt" . 2.0)
             ("last-error" . nil)
             ("forge-status" . "current")
             ("provider" . "github")
             ("pr-refs-status" . "ok")
             ("mirror-format" . 1)))))
       (should (file-readable-p file))
       (should (= (+git-sync--state-generation
                   (car (+git-sync-load-state id)))
                  3))))))

(ert-deftest git-review-sync-malformed-state-safe ()
  "Malformed sync-state degrades without Lisp evaluation."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((id "github.com/org/malformed")
            (file (+git-sync-state-file id)))
       (+git-sync--ensure-cache-dir id)
       (with-temp-file file
         (insert "(+ 1 2) #evil\n{\"not\": \"closed\""))
       (let* ((pair (+git-sync-load-state id))
              (state (car pair))
              (flag (cdr pair)))
         (should (eq flag 'malformed))
         (should (= (+git-sync--state-generation state) 0))
         (should (stringp (+git-sync--alist-get "last-error" state))))))))

;; ---------------------------------------------------------------------------
;; Forge adapter
;; ---------------------------------------------------------------------------

(ert-deftest git-review-sync-forge-adapter-paths ()
  "Forge adapter success/unavailable/untracked/delayed/error paths."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "forge"))
            (bare (car up))
            (url "git@github.com:org/forge.git")
            (work (git-review-sync--clone-with-identity bare url "forge-w"))
            (saw nil))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             ;; Unavailable (default) -> Git-only success.
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should (memq (plist-get (+git-sync-status id) :forge)
                           '(unavailable untracked skipped unsupported)))
             ;; Delayed ok callback.
             (setq +git-sync-forge-pull-function
                   (git-review-sync--forge-stub 'ok 0.05))
             (+git-sync-reset-jobs)
             (setq +git-sync--forge-pull-count 0)
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should (eq (plist-get (+git-sync-status id) :forge) 'current))
             (should (= +git-sync--forge-pull-count 1))
             ;; Untracked still Git-only success.
             (setq +git-sync-forge-pull-function
                   (git-review-sync--forge-stub 'untracked))
             (+git-sync-reset-jobs)
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should (eq (plist-get (+git-sync-status id) :forge) 'untracked))
             ;; Error path fails the job.
             (setq +git-sync-forge-pull-function
                   (lambda (_id on-complete)
                     (cl-incf +git-sync--forge-pull-count)
                     (funcall on-complete (list 'failed "auth exploded"))
                     (list :cancel #'ignore)))
             (+git-sync-reset-jobs)
             (setq saw (+git-sync-start id))
             (+git-sync-wait id 10)
             (should (eq (+git-sync-job-state saw) 'failed))
             (should (string-match-p "auth"
                                     (+git-sync-job-last-error saw))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-forge-callback-prevents-second-fetch ()
  "Forge completion callback yields exactly one git fetch per sync."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "onecb"))
            (bare (car up))
            (url "git@github.com:org/onecb.git")
            (work (git-review-sync--clone-with-identity bare url "onecb-w"))
            (callback-passed nil))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (setq +git-sync-forge-pull-function
                   (lambda (_id on-complete)
                     (setq callback-passed (functionp on-complete))
                     (cl-incf +git-sync--forge-pull-count)
                     (funcall on-complete 'ok)
                     (list :cancel #'ignore)))
             (setq +git-sync--git-fetch-count 0
                   +git-sync--forge-pull-count 0)
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should callback-passed)
             (should (= +git-sync--git-fetch-count 1))
             (should (= +git-sync--forge-pull-count 1)))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

;; ---------------------------------------------------------------------------
;; Waiting buffers / bindings / sync-all / import
;; ---------------------------------------------------------------------------

(ert-deftest git-review-sync-waiting-buffers-refresh-once ()
  "Live waiting buffers refresh once; dead buffers are ignored."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "wait"))
            (bare (car up))
            (url "git@github.com:org/wait.git")
            (work (git-review-sync--clone-with-identity bare url "wait-w"))
            (dead (get-buffer-create " *git-sync-dead*"))
            (live nil)
            (wconf nil)
            (wcount nil))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (kill-buffer dead)
             (let ((default-directory (file-name-as-directory work)))
               (require 'magit)
               (setq live (+git-review-open-worktree))
               (setq wconf (current-window-configuration)
                     wcount (length (window-list)))
               (setq +git-sync--waiting-refresh-count 0
                     +git-sync-forge-pull-function
                     (git-review-sync--forge-stub 'unavailable 0.05))
               (let ((selected-before (selected-window)))
                 (+git-sync-start id live)
                 (+git-sync-start id dead)
                 (git-review-sync--wait-success id)
                 (should (= +git-sync--waiting-refresh-count 1))
                 (should (= (length (window-list)) wcount))
                 (should (eq (selected-window) selected-before))
                 (should (window-configuration-p wconf)))
               (kill-buffer live)))
         (git-review-baseline-cleanup-repo-buffers work)
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-bindings-and-gr-local ()
  "`@', C-c g f/F exist; gr remains local-only."
  (should (commandp #'+git/sync))
  (should (commandp #'+git/sync-all))
  (should (commandp #'+git-review-refresh))
  (should (fboundp #'+git-dispatch))
  (should (keymapp +git-review-buffer-mode-map))
  ;; gr must not call sync.
  (git-review-sync--with-dirs
   (lambda (_reg)
     (git-review-fixtures-with-repo
      "gr-local"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (git-review-sync--set-fetch-url
         root "git@github.com:org/gr-local.git")
        (+git-store-register-root root)
        (let ((default-directory (file-name-as-directory root))
              (starts 0))
          (cl-letf (((symbol-function '+git-sync-start)
                     (lambda (&rest _)
                       (cl-incf starts)
                       (error "sync must not run"))))
            (require 'magit)
            (let ((buf (+git-review-open-worktree)))
              (with-current-buffer buf
                (+git-review-refresh))
              (kill-buffer buf)))
          (should (= starts 0))
          (git-review-baseline-cleanup-repo-buffers root)))))))

(ert-deftest git-review-sync-all-allowlist-and-concurrency ()
  "Empty allowlist does nothing; allowlist + concurrency bound work."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (setq +git-sync-active-repositories nil)
     (should (null (+git/sync-all)))
     (let* ((up (git-review-sync--make-upstream "all"))
            (bare (car up))
            (url-a "git@github.com:org/alla.git")
            (url-b "git@github.com:org/allb.git")
            (url-c "git@github.com:org/allc.git")
            (a (git-review-sync--clone-with-identity bare url-a "alla"))
            (b (git-review-sync--clone-with-identity bare url-b "allb"))
            (c (git-review-sync--clone-with-identity bare url-c "allc"))
            (max-seen 0)
            (gate-open nil))
       (unwind-protect
           (let* ((id-a (+git-store-local-context-repository-id
                         (git-review-sync--register a url-a bare)))
                  (id-b (+git-store-local-context-repository-id
                         (git-review-sync--register b url-b bare)))
                  (id-c (+git-store-local-context-repository-id
                         (git-review-sync--register c url-c bare))))
             ;; Only A and B allowlisted; C registered but excluded.
             (setq +git-sync-active-repositories (list id-a id-b)
                   +git-sync-max-concurrent 1
                   +git-sync-forge-pull-function
                   (lambda (_id on-complete)
                     (cl-incf +git-sync--forge-pull-count)
                     (setq max-seen
                           (max max-seen +git-sync--global-active))
                     (let ((timer nil))
                       (setq timer
                             (run-at-time
                              0.05 0.05
                              (lambda ()
                                (when gate-open
                                  (cancel-timer timer)
                                  (funcall on-complete 'unavailable))))))
                     (list :cancel
                           (lambda ()
                             (funcall on-complete 'cancelled)))))
             (setq +git-sync--git-fetch-count 0)
             (+git/sync-all)
             ;; Let first job reach forge; second should be queued.
             (let ((deadline (+ (float-time) 5)))
               (while (and (< (float-time) deadline)
                           (< +git-sync--global-active 1))
                 (accept-process-output nil 0.02)))
             (should (<= +git-sync--global-active 1))
             (setq gate-open t)
             (+git-sync-wait id-a 15)
             (+git-sync-wait id-b 15)
             (should (<= max-seen 1))
             (should (eq (+git-sync-job-state (+git-sync--get-job id-a))
                         'succeeded))
             (should (eq (+git-sync-job-state (+git-sync--get-job id-b))
                         'succeeded))
             (should (eq (+git-sync-job-state (+git-sync--get-job id-c))
                         'idle))
             (should (= +git-sync--git-fetch-count 2)))
         (git-review-sync--cleanup-root a)
         (git-review-sync--cleanup-root b)
         (git-review-sync--cleanup-root c)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-import-ref-local-namespaced ()
  "Mirror-to-context import is local, namespaced, and rejects foreigners."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "imp"))
            (bare (car up))
            (oid (cdr up))
            (url "git@github.com:org/imp.git")
            (other-url "git@github.com:org/other-imp.git")
            (work (git-review-sync--clone-with-identity bare url "imp-w"))
            (other (git-review-sync--clone-with-identity
                    bare other-url "imp-o")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (ctx-o (git-review-sync--register other other-url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (id-o (+git-store-local-context-repository-id ctx-o)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (let* ((before-branch
                     (string-trim
                      (git-review-fixtures--git-ok
                       work "symbolic-ref" "--short" "HEAD")))
                    (info
                     (+git-sync-import-ref
                      id
                      (+git-store-local-context-context-id ctx)
                      "refs/heads/main")))
               (should (equal (plist-get info :oid) oid))
               (should (string-prefix-p "refs/git-review/"
                                        (plist-get info :local-ref)))
               (should
                (equal
                 (string-trim
                  (git-review-fixtures--git-ok
                   work "rev-parse" (plist-get info :local-ref)))
                 oid))
               (should (equal
                        (string-trim
                         (git-review-fixtures--git-ok
                          work "symbolic-ref" "--short" "HEAD"))
                        before-branch))
               ;; Source is the local published mirror directory.
               (should (file-directory-p
                        (+git-sync-mirror-directory id))))
             (should-error
              (+git-sync-import-ref
               id
               (+git-store-local-context-context-id ctx-o)
               "refs/heads/main")
              :type 'user-error)
             (should-not (equal id id-o)))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root other)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-offline-open-after-sync ()
  "After fixture sync, open and gr succeed under the hard network guard."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "off"))
            (bare (car up))
            (url "git@github.com:org/off.git")
            (work (git-review-sync--clone-with-identity bare url "off-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (git-review-baseline--with-instrumentation
              (lambda ()
                (let ((default-directory (file-name-as-directory work)))
                  (require 'magit)
                  (let ((buf (+git-review-open-worktree)))
                    (should (buffer-live-p buf))
                    (with-current-buffer buf
                      (+git-review-refresh))
                    (kill-buffer buf)))
                (should (null git-review-baseline--blocked-attempts)))))
         (git-review-baseline-cleanup-repo-buffers work)
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-failure-does-not-modify-clone ()
  "Failure/cancellation never modifies clone branch, remotes, or user refs."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "intact"))
            (bare (car up))
            (url "git@github.com:org/intact.git")
            (work (git-review-sync--clone-with-identity bare url "intact-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (branch-before
                   (string-trim
                    (git-review-fixtures--git-ok
                     work "symbolic-ref" "--short" "HEAD")))
                  (remote-before
                   (string-trim
                    (git-review-fixtures--git-ok
                     work "remote" "get-url" "origin")))
                  (head-before
                   (string-trim
                    (git-review-fixtures--git-ok work "rev-parse" "HEAD")))
                  (refs-before
                   (string-trim
                    (git-review-fixtures--git-ok work "show-ref"))))
             ;; Force git failure.
             (setq +git-sync-test-remote-override
                   (list (cons id "/tmp/git-review-intact-missing.git")))
             (+git-sync-start id)
             (+git-sync-wait id 10)
             (should (equal
                      (string-trim
                       (git-review-fixtures--git-ok
                        work "symbolic-ref" "--short" "HEAD"))
                      branch-before))
             (should (equal
                      (string-trim
                       (git-review-fixtures--git-ok
                        work "remote" "get-url" "origin"))
                      remote-before))
             (should (equal
                      (string-trim
                       (git-review-fixtures--git-ok work "rev-parse" "HEAD"))
                      head-before))
             (should (equal
                      (string-trim
                       (git-review-fixtures--git-ok work "show-ref"))
                      refs-before)))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-url-sanitization ()
  "Diagnostics strip credentials from URLs."
  (should (equal (+git-sync--sanitize-url
                  "https://user:secret@github.com/org/x.git")
                 "https://github.com/org/x.git"))
  (should (string-match-p "secret"
                          "https://user:secret@github.com/x"))
  (should-not
   (string-match-p
    "secret"
    (+git-sync--sanitize-error
     "failed https://user:secret@github.com/org/x.git boom"))))

(ert-deftest git-review-sync-large-tree-warm-after-phase4 ()
  "100-file Changes Tree warm median stays bounded after Phase 4 load."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (git-review-fixtures-with-repo
      "sync-large-tree"
      (lambda (root)
        (git-review-fixtures-create-large root 100)
        (git-review-sync--set-fetch-url
         root "git@github.com:org/sync-large.git")
        (+git-store-register-root root)
        (require 'magit)
        (let* ((default-directory (file-name-as-directory root))
               (target (+git-review-target-for-worktree root))
               (samples nil))
          ;; Prewarm.
          (+git-changes-tree-setup-buffer target)
          (dotimes (_ 5)
            (let ((t0 (float-time)))
              (with-current-buffer
                  (+git-changes-tree-setup-buffer target)
                (magit-refresh))
              (push (- (float-time) t0) samples)))
          (setq samples (nreverse samples))
          (let* ((sorted (sort (copy-sequence samples) #'<))
                 (median (nth (/ (length sorted) 2) sorted)))
            (should (< median 2.0))
            (message
             "phase4 large-tree warm: median=%.3fs samples=%S"
             median samples))
          (git-review-baseline-cleanup-repo-buffers root)))))))

(ert-deftest git-review-sync-state-fail-after-swap-restores-mirror ()
  "State publication failure after candidate swap restores old mirror OID."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "pubfail"))
            (bare (car up))
            (oid1 (cdr up))
            (url "git@github.com:org/pubfail.git")
            (work (git-review-sync--clone-with-identity bare url "pubfail-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (mirror (+git-sync-mirror-directory id)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should (= (plist-get (+git-sync-status id) :generation) 1))
             (should (equal (string-trim
                             (+git-sync--git-ok mirror "rev-parse" "HEAD"))
                            oid1))
             (let* ((tmp (make-temp-file "git-review pubfail-up " t))
                    (up-work (expand-file-name "w" tmp)))
               (git-review-fixtures--git-ok bare "clone" bare up-work)
               (git-review-fixtures--git-ok
                up-work "config" "user.name" git-review-fixtures-test-name)
               (git-review-fixtures--git-ok
                up-work "config" "user.email" git-review-fixtures-test-email)
               (git-review-fixtures--write-file up-work "README.md" "v2\n")
               (git-review-fixtures--git-ok up-work "add" "-A")
               (git-review-fixtures--git-ok up-work "commit" "-m" "v2")
               (git-review-fixtures--git-ok up-work "push" bare "main")
               (ignore-errors (delete-directory tmp t)))
             (let ((oid2 (string-trim
                          (+git-sync--git-ok bare "rev-parse" "HEAD"))))
               (should-not (equal oid1 oid2))
               (+git-sync-reset-jobs)
               (cl-letf (((symbol-function '+git-sync-save-state)
                          (lambda (&rest _)
                            (error "simulated state publish failure"))))
                 (+git-sync-start id)
                 (let ((job (+git-sync-wait id 15)))
                   (should (eq (+git-sync-job-state job) 'failed))
                   (should (= (plist-get (+git-sync-status id) :generation) 1))
                   (should (equal (string-trim
                                   (+git-sync--git-ok mirror "rev-parse" "HEAD"))
                                  oid1))
                   (should-not (file-directory-p
                                (+git-sync-mirror-backup-directory id)))
                   (should-not (file-exists-p
                                (+git-sync-publish-intent-file id)))))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-two-process-lock-race ()
  "Two batch Emacs processes racing acquire exactly one lock."
  (git-review-sync--with-dirs
   (lambda (reg)
     (let* ((id "github.com/org/race-lock")
            (core (expand-file-name "core" user-emacs-directory))
            (out-a (make-temp-file "git-review-lock-a-"))
            (out-b (make-temp-file "git-review-lock-b-"))
            (gate (make-temp-file "git-review-lock-gate-"))
            (script-a (make-temp-file "git-review-lock-sa-" nil ".el"))
            (script-b (make-temp-file "git-review-lock-sb-" nil ".el")))
       (unwind-protect
           (progn
             (+git-sync--ensure-cache-dir id)
             (dolist (pair (list (cons script-a out-a) (cons script-b out-b)))
               (with-temp-file (car pair)
                 (prin1
                  `(progn
                     (setq user-emacs-directory ,user-emacs-directory)
                     (add-to-list 'load-path ,core)
                     (require 'init-git-store)
                     (require 'init-git-sync)
                     (setq +git-store-registry-directory ,reg)
                     (while (not (file-exists-p ,gate))
                       (sleep-for 0.01))
                     (let ((result (+git-sync-try-acquire-lock ,id)))
                       (with-temp-file ,(cdr pair)
                         (prin1 (car result) (current-buffer))
                         (insert "\n")
                         (when (eq (car result) 'acquired)
                           (prin1 (cdr result) (current-buffer))
                           (insert "\n")
                           (sleep-for 0.4)
                           (+git-sync-release-lock ,id (cdr result))))))
                  (current-buffer))))
             (let ((proc-a (start-process "lock-race-a" nil
                                          "emacs" "--batch" "-Q" "-l" script-a))
                   (proc-b (start-process "lock-race-b" nil
                                          "emacs" "--batch" "-Q" "-l" script-b)))
               (sleep-for 0.15)
               (with-temp-file gate (insert "go\n"))
               (let ((deadline (+ (float-time) 10)))
                 (while (and (< (float-time) deadline)
                             (or (process-live-p proc-a)
                                 (process-live-p proc-b)))
                   (accept-process-output nil 0.05)))
               (let* ((ra (with-temp-buffer
                            (insert-file-contents out-a) (buffer-string)))
                      (rb (with-temp-buffer
                            (insert-file-contents out-b) (buffer-string)))
                      (statuses (list (car (read-from-string ra))
                                      (car (read-from-string rb)))))
                 (should (equal (sort (mapcar #'symbol-name statuses) #'string<)
                                '("acquired" "busy"))))))
         (dolist (f (list out-a out-b gate script-a script-b))
           (ignore-errors (delete-file f)))
         (+git-sync--remove-lock-dir id))))))

(ert-deftest git-review-sync-cancel-queued-preserves-slot ()
  "Cancelling a queued job does not decrement the concurrency slot."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "qcancel"))
            (bare (car up))
            (url-a "git@github.com:org/qcancel-a.git")
            (url-b "git@github.com:org/qcancel-b.git")
            (a (git-review-sync--clone-with-identity bare url-a "qca"))
            (b (git-review-sync--clone-with-identity bare url-b "qcb"))
            (gate-open nil)
            (timer nil))
       (unwind-protect
           (let* ((id-a (+git-store-local-context-repository-id
                         (git-review-sync--register a url-a bare)))
                  (id-b (+git-store-local-context-repository-id
                         (git-review-sync--register b url-b bare))))
             (setq +git-sync-max-concurrent 1
                   +git-sync-forge-pull-function
                   (lambda (_id on-complete)
                     (cl-incf +git-sync--forge-pull-count)
                     (setq timer
                           (run-at-time
                            0.05 0.05
                            (lambda ()
                              (when gate-open
                                (cancel-timer timer)
                                (funcall on-complete 'unavailable)))))
                     (list :cancel
                           (lambda ()
                             (when timer (cancel-timer timer))
                             (funcall on-complete 'cancelled)))))
             (+git-sync-start id-a)
             (let ((deadline (+ (float-time) 8)))
               (while (and (< (float-time) deadline)
                           (not (eq (+git-sync-job-state
                                     (+git-sync--get-job id-a))
                                    'pulling-forge)))
                 (accept-process-output nil 0.02)))
             (should (eq (+git-sync-job-state (+git-sync--get-job id-a))
                         'pulling-forge))
             (should (= +git-sync--global-active 1))
             (+git-sync-start id-b)
             (should (eq (+git-sync-job-state (+git-sync--get-job id-b))
                         'queued))
             (should (= +git-sync--global-active 1))
             (+git-sync-cancel id-b)
             (should (eq (+git-sync-job-state (+git-sync--get-job id-b))
                         'cancelled))
             (should (= +git-sync--global-active 1))
             (setq gate-open t)
             (+git-sync-wait id-a 10)
             (should (= +git-sync--global-active 0)))
         (setq gate-open t)
         (when timer (ignore-errors (cancel-timer timer)))
         (git-review-sync--cleanup-root a)
         (git-review-sync--cleanup-root b)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-stale-callback-ignored-after-restart ()
  "Delayed Forge callback from a cancelled attempt cannot finish a new run."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "stale-cb"))
            (bare (car up))
            (url "git@github.com:org/stale-cb.git")
            (work (git-review-sync--clone-with-identity bare url "stale-w"))
            (old-complete nil)
            (old-cancel nil))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (setq +git-sync-forge-pull-function
                   (lambda (_id on-complete)
                     (cl-incf +git-sync--forge-pull-count)
                     (setq old-complete on-complete)
                     (list :cancel
                           (lambda ()
                             (setq old-cancel t)))))
             (+git-sync-start id)
             (let ((deadline (+ (float-time) 5)))
               (while (and (< (float-time) deadline)
                           (not (eq (+git-sync-job-state
                                     (+git-sync--get-job id))
                                    'pulling-forge)))
                 (accept-process-output nil 0.02)))
             (let ((attempt1 (+git-sync-job-attempt-id
                              (+git-sync--get-job id))))
               (+git-sync-cancel id)
               (should old-cancel)
               (funcall old-complete 'cancelled)
               (+git-sync-wait id 5)
               (should (eq (+git-sync-job-state (+git-sync--get-job id))
                           'cancelled))
               (setq +git-sync-forge-pull-function
                     (git-review-sync--forge-stub 'unavailable))
               (+git-sync-reset-jobs)
               (git-review-sync--register work url bare)
               (+git-sync-start id)
               (git-review-sync--wait-success id)
               (funcall old-complete 'ok)
               (should (= (plist-get (+git-sync-status id) :generation) 1))
               (should (eq (+git-sync-job-state (+git-sync--get-job id))
                           'succeeded))
               (should-not
                (eql attempt1
                     (+git-sync-job-attempt-id (+git-sync--get-job id))))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-forge-async-error-and-cancel ()
  "Async Forge error fails the job; cancel waits for adapter completion."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "ferr"))
            (bare (car up))
            (url "git@github.com:org/ferr.git")
            (work (git-review-sync--clone-with-identity bare url "ferr-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (setq +git-sync-forge-pull-function
                   (lambda (_id on-complete)
                     (cl-incf +git-sync--forge-pull-count)
                     (run-at-time
                      0.05 nil
                      (lambda ()
                        (funcall on-complete (list 'failed "api 401"))))
                     (list :cancel #'ignore)))
             (+git-sync-start id)
             (let ((job (+git-sync-wait id 10)))
               (should (eq (+git-sync-job-state job) 'failed))
               (should (string-match-p "401" (+git-sync-job-last-error job)))
               (should-not (file-directory-p (+git-sync-lock-file id))))
             (+git-sync-reset-jobs)
             (let ((released nil))
               (setq +git-sync-forge-pull-function
                     (lambda (_id on-complete)
                       (cl-incf +git-sync--forge-pull-count)
                       (list :cancel
                             (lambda ()
                               (run-at-time
                                0.1 nil
                                (lambda ()
                                  (setq released t)
                                  (funcall on-complete 'cancelled)))))))
               (+git-sync-start id)
               (let ((deadline (+ (float-time) 5)))
                 (while (and (< (float-time) deadline)
                             (not (eq (+git-sync-job-state
                                       (+git-sync--get-job id))
                                      'pulling-forge)))
                   (accept-process-output nil 0.02)))
               (should (file-directory-p (+git-sync-lock-file id)))
               (+git-sync-cancel id)
               (should (file-directory-p (+git-sync-lock-file id)))
               (+git-sync-wait id 5)
               (should released)
               (should-not (file-directory-p (+git-sync-lock-file id)))
               (should (eq (+git-sync-job-state (+git-sync--get-job id))
                           'cancelled))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-import-rejects-non-namespaced-refs ()
  "Import rejects refs/heads/*, refs/tags/*, and malformed destinations."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "imprej"))
            (bare (car up))
            (url "git@github.com:org/imprej.git")
            (work (git-review-sync--clone-with-identity bare url "imprej-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (cid (+git-store-local-context-context-id ctx)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (should-error
              (+git-sync-import-ref
               id cid "refs/heads/main"
               "refs/heads/phase4-created-user-branch")
              :type 'user-error)
             (should-error
              (+git-sync-import-ref
               id cid "refs/heads/main" "refs/tags/evil")
              :type 'user-error)
             (should-error
              (+git-sync-import-ref
               id cid "refs/heads/main" "refs/git-review/bad..name")
              :type 'user-error)
             (should
              (not (eq 0 (car (git-review-fixtures--call-git
                               work "show-ref" "--verify" "--quiet"
                               "refs/heads/phase4-created-user-branch"))))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-prepare-is-asynchronous ()
  "`+git-sync-start' returns before candidate preparation finishes."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "asyncprep"))
            (bare (car up))
            (url "git@github.com:org/asyncprep.git")
            (work (git-review-sync--clone-with-identity bare url "aprep-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx))
                  (job (+git-sync-start id)))
             (should (memq (+git-sync-job-state job)
                           '(queued preparing-mirror fetching-mirror
                             pulling-forge publishing succeeded)))
             (should (+git-sync-job-attempt-id job))
             (git-review-sync--wait-success id))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-idle-success-not-unconditionally-stale ()
  "After restart, a successful idle sync is not stale solely for being idle."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((up (git-review-sync--make-upstream "nostale"))
            (bare (car up))
            (url "git@github.com:org/nostale.git")
            (work (git-review-sync--clone-with-identity bare url "nostale-w")))
       (unwind-protect
           (let* ((ctx (git-review-sync--register work url bare))
                  (id (+git-store-local-context-repository-id ctx)))
             (+git-sync-start id)
             (git-review-sync--wait-success id)
             (+git-sync-reset-jobs)
             (let ((+git-sync-stale-after-seconds 86400)
                   (st (+git-sync-status id)))
               (should (eq (plist-get st :state) 'idle))
               (should-not (plist-get st :stale))
               (should (= (plist-get st :generation) 1))))
         (git-review-sync--cleanup-root work)
         (git-review-sync--cleanup-root bare))))))

(ert-deftest git-review-sync-recovery-rename-failure-retains-backup ()
  "Recovery retains backup and intent when restore rename fails."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((id "github.com/org/recfail")
            (mirror (+git-sync-mirror-directory id))
            (backup (+git-sync-mirror-backup-directory id))
            (intent (+git-sync-publish-intent-file id))
            (orig-rename (symbol-function 'rename-file)))
       (+git-sync--ensure-cache-dir id)
       (make-directory backup t)
       (with-temp-file (expand-file-name "HEAD" backup)
         (insert "ref: refs/heads/main\n"))
       (make-directory mirror t)
       (with-temp-file (expand-file-name "HEAD" mirror)
         (insert "ref: refs/heads/broken\n"))
       (+git-sync-save-state
        id
        '(("version" . 1)
          ("generation" . 1)
          ("last-success" . 1.0)
          ("forge-status" . "unavailable")
          ("provider" . "github")
          ("pr-refs-status" . "unavailable")
          ("mirror-format" . 1)))
       (with-temp-file intent
         (insert (json-encode '(("generation" . 2) ("timestamp" . 2.0)))
                 "\n"))
       (cl-letf (((symbol-function 'rename-file)
                  (lambda (file newname &optional ok-if-already-exists)
                    (if (file-equal-p file backup)
                        (error "simulated restore rename failure")
                      (funcall orig-rename file newname
                               ok-if-already-exists)))))
         (+git-sync--recover-incomplete-publish id)
         (should (file-directory-p backup))
         (should (file-exists-p (expand-file-name "HEAD" backup)))
         (should (file-exists-p intent))
         ;; Must not wipe both generations.
         (should (or (file-directory-p mirror)
                     (file-directory-p backup)))
         (should (equal (+git-sync-published-mirror-directory id)
                        backup)))))))

(ert-deftest git-review-sync-status-recovers-incomplete-publish ()
  "Status recovers an incomplete publish without starting a new sync."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((id "github.com/org/status-rec")
            (mirror (+git-sync-mirror-directory id))
            (backup (+git-sync-mirror-backup-directory id))
            (intent (+git-sync-publish-intent-file id)))
       (+git-sync--ensure-cache-dir id)
       (make-directory backup t)
       (with-temp-file (expand-file-name "HEAD" backup)
         (insert "ref: refs/heads/main\n"))
       (+git-sync-save-state
        id
        '(("version" . 1)
          ("generation" . 1)
          ("last-success" . 1.0)
          ("forge-status" . "unavailable")
          ("provider" . "github")
          ("pr-refs-status" . "unavailable")
          ("mirror-format" . 1)))
       (with-temp-file intent
         (insert (json-encode '(("generation" . 2) ("timestamp" . 2.0)))
                 "\n"))
       (should-not (file-directory-p mirror))
       (let ((st (+git-sync-status id)))
         (should (file-directory-p mirror))
         (should-not (file-directory-p backup))
         (should-not (file-exists-p intent))
         (should (equal (plist-get st :mirror) mirror))
         (should (plist-get st :mirror-exists))
         (should (= (plist-get st :generation) 1)))))))

(ert-deftest git-review-sync-preserves-user-git-config ()
  "Production sync helpers do not null the user's Git configuration."
  (let* ((tmpdir (make-temp-file "git-sync-cfg " t))
         (global (expand-file-name "global.gitconfig" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file global
            (insert "[credential]\n\thelper = store\n"))
          (let ((process-environment
                 (append
                  (list (concat "GIT_CONFIG_GLOBAL=" global))
                  (cl-remove-if
                   (lambda (e)
                     (string-match-p "\\`GIT_CONFIG_\\(GLOBAL\\|SYSTEM\\)=" e))
                   process-environment))))
            (should
             (eq 0 (car (+git-sync--call-git
                         tmpdir "config" "--global"
                         "--get" "credential.helper"))))
            (let ((env (+git-sync--git-process-environment)))
              (should (equal (car env) "GIT_TERMINAL_PROMPT=0"))
              (should-not
               (cl-find-if
                (lambda (e)
                  (string-match-p "GIT_CONFIG_GLOBAL=/dev/null" e))
                env))
              (should-not
               (cl-find-if
                (lambda (e)
                  (string-match-p "GIT_CONFIG_SYSTEM=/dev/null" e))
                env)))))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest git-review-sync-default-forge-cancel-kills-url ()
  "Default Forge adapter cancels a URL started during forge--pull."
  (let* ((had-forge (featurep 'forge))
         (callback-retained nil)
         (completed nil)
         (got nil)
         (url-buf nil)
         (handle nil)
         (+git-sync-url-retrieve-function
          (lambda (_url _callback &optional _cbargs _silent _inhibit)
            (setq url-buf
                  (generate-new-buffer " *git-sync-url-hang*"))
            (let ((proc (start-process "git-sync-url-hang" url-buf
                                      "sleep" "60")))
              (set-process-query-on-exit-flag proc nil)
              url-buf))))
    (unwind-protect
        (progn
          (unless (memq 'forge features) (push 'forge features))
          (cl-letf
              (((symbol-function '+git-sync--choose-seed-root)
                (lambda (_) default-directory))
               ((symbol-function 'forge-get-repository)
                (lambda (&rest _) 'fake-repo))
               ((symbol-function 'forge--pull)
                (lambda (_repo &optional callback _since)
                  (url-retrieve
                   "http://127.0.0.1:1/hang"
                   (lambda (&rest _)
                     (setq callback-retained t)
                     (when callback (funcall callback)))))))
            (+git-sync--ensure-forge-advice)
            (setq handle
                  (+git-sync--forge-pull-default
                   "github.com/org/forge-cancel"
                   (lambda (st)
                     (setq completed t)
                     (setq got st))))
            (should (buffer-live-p url-buf))
            (should (process-live-p (get-buffer-process url-buf)))
            (funcall (plist-get handle :cancel))
            (should completed)
            (should (eq got 'cancelled))
            (should-not
             (and (buffer-live-p url-buf)
                  (get-buffer-process url-buf)
                  (process-live-p (get-buffer-process url-buf))))
            (should-not callback-retained)))
      (unless had-forge
        (setq features (delq 'forge features)))
      (when (buffer-live-p url-buf)
        (ignore-errors (kill-buffer url-buf))))))

(ert-deftest git-review-sync-default-forge-late-url-cancel ()
  "Cancel kills a URL started after forge--pull returns (chained async)."
  (let* ((had-forge (featurep 'forge))
         (callback-retained nil)
         (completed nil)
         (got nil)
         (url-buf nil)
         (handle nil)
         (+git-sync-url-retrieve-function
          (lambda (url callback &optional _cbargs _silent _inhibit)
            (cond
             ((string-match-p "/first" url)
              (run-at-time
               0.05 nil
               (lambda () (funcall callback nil)))
              (generate-new-buffer " *git-sync-url-first*"))
             (t
              (setq url-buf
                    (generate-new-buffer " *git-sync-url-late*"))
              (let ((proc (start-process "git-sync-url-late" url-buf
                                        "sleep" "60")))
                (set-process-query-on-exit-flag proc nil)
                url-buf))))))
    (unwind-protect
        (progn
          (unless (memq 'forge features) (push 'forge features))
          (cl-letf
              (((symbol-function '+git-sync--choose-seed-root)
                (lambda (_) default-directory))
               ((symbol-function 'forge-get-repository)
                (lambda (&rest _) 'fake-repo))
               ((symbol-function 'forge--pull)
                (lambda (_repo &optional _callback _since)
                  (url-retrieve
                   "http://127.0.0.1:1/first"
                   (lambda (&rest _)
                     (url-retrieve
                      "http://127.0.0.1:1/hang"
                      (lambda (&rest _)
                        (setq callback-retained t))))))))
            (+git-sync--ensure-forge-advice)
            (setq handle
                  (+git-sync--forge-pull-default
                   "github.com/org/forge-late"
                   (lambda (st)
                     (setq completed t)
                     (setq got st))))
            (let ((deadline (+ (float-time) 3)))
              (while (and (< (float-time) deadline)
                          (not (and url-buf (buffer-live-p url-buf))))
                (accept-process-output nil 0.02)))
            (should (buffer-live-p url-buf))
            (should (process-live-p (get-buffer-process url-buf)))
            (funcall (plist-get handle :cancel))
            (should completed)
            (should (eq got 'cancelled))
            (should-not
             (and (buffer-live-p url-buf)
                  (get-buffer-process url-buf)
                  (process-live-p (get-buffer-process url-buf))))
            (should-not callback-retained)))
      (unless had-forge
        (setq features (delq 'forge features)))
      (when (buffer-live-p url-buf)
        (ignore-errors (kill-buffer url-buf))))))

(ert-deftest git-review-sync-default-forge-async-selective-and-gitlab ()
  "Delayed selective store and delayed GitLab error complete the job."
  (let* ((had-forge (featurep 'forge))
         (got nil)
         (+git-sync-url-retrieve-function
          (lambda (_url callback &optional _cbargs _silent _inhibit)
            (run-at-time 0.05 nil (lambda () (funcall callback nil)))
            (generate-new-buffer " *git-sync-url-sel*")))
         (+git-sync-ghub-request-function
          (lambda (_method _resource &optional _params &rest keys)
            (let ((eb (plist-get keys :errorback)))
              (run-at-time
               0.05 nil
               (lambda ()
                 (when (functionp eb)
                   (funcall eb "gitlab api 401"))))))))
    (unwind-protect
        (progn
          (unless (memq 'forge features) (push 'forge features))
          (unless (fboundp 'forge--msg)
            (fset 'forge--msg (lambda (&rest _))))
          (unless (fboundp 'ghub-request)
            (fset 'ghub-request (lambda (&rest _))))
          (+git-sync--ensure-forge-advice)
          ;; Async selective: Storing REPO after forge--pull returns.
          (cl-letf
              (((symbol-function '+git-sync--choose-seed-root)
                (lambda (_) default-directory))
               ((symbol-function 'forge-get-repository)
                (lambda (&rest _) 'fake-repo))
               ((symbol-function 'forge--pull)
                (lambda (_repo &optional _callback _since)
                  (url-retrieve
                   "http://127.0.0.1:1/sel"
                   (lambda (&rest _)
                     (forge--msg 'fake-repo t t "Storing REPO"))))))
            (setq got nil)
            (+git-sync--forge-pull-default
             "github.com/org/async-sel"
             (lambda (st) (setq got st)))
            (let ((deadline (+ (float-time) 3)))
              (while (and (< (float-time) deadline) (null got))
                (accept-process-output nil 0.02)))
            (should (eq got 'ok)))
          ;; Async GitLab error after pull returns via chained request.
          (cl-letf
              (((symbol-function '+git-sync--choose-seed-root)
                (lambda (_) default-directory))
               ((symbol-function 'forge-get-repository)
                (lambda (&rest _) 'fake-repo))
               ((symbol-function 'forge--pull)
                (lambda (_repo &optional _callback _since)
                  (url-retrieve
                   "http://127.0.0.1:1/gl"
                   (lambda (&rest _)
                     (ghub-request "GET" "/projects/x" nil
                                   :callback #'ignore
                                   :errorback t))))))
            (setq got nil)
            (+git-sync--forge-pull-default
             "gitlab.com/org/async-gl"
             (lambda (st) (setq got st)))
            (let ((deadline (+ (float-time) 3)))
              (while (and (< (float-time) deadline) (null got))
                (accept-process-output nil 0.02)))
            (should (consp got))
            (should (eq (car got) 'failed))
            (should (string-match-p "401" (cadr got)))))
      (unless had-forge
        (setq features (delq 'forge features))))))

(ert-deftest git-review-sync-live-lock-status-does-not-rollback-publish ()
  "Status during a live mid-publish lock must not restore the old mirror."
  (git-review-sync--with-dirs
   (lambda (_reg)
     (let* ((id "github.com/org/live-pub")
            (mirror (+git-sync-mirror-directory id))
            (backup (+git-sync-mirror-backup-directory id))
            (intent (+git-sync-publish-intent-file id))
            (nonce nil))
       (+git-sync--ensure-cache-dir id)
       ;; Last successful generation lives in backup during the swap window.
       (make-directory backup t)
       (with-temp-file (expand-file-name "HEAD" backup)
         (insert "ref: refs/heads/old\n"))
       (make-directory mirror t)
       (with-temp-file (expand-file-name "HEAD" mirror)
         (insert "ref: refs/heads/new\n"))
       (+git-sync-save-state
        id
        '(("version" . 1)
          ("generation" . 1)
          ("last-success" . 1.0)
          ("forge-status" . "unavailable")
          ("provider" . "github")
          ("pr-refs-status" . "unavailable")
          ("mirror-format" . 1)))
       (with-temp-file intent
         (insert (json-encode '(("generation" . 2) ("timestamp" . 2.0)))
                 "\n"))
       (setq nonce (+git-sync-acquire-lock id))
       (should nonce)
       (unwind-protect
           (progn
             ;; Reader observes status while publisher still holds the lock.
             (let ((st (+git-sync-status id)))
               (should (equal (plist-get st :mirror) backup))
               (should (= (plist-get st :generation) 1)))
             (should (file-exists-p intent))
             (should (file-directory-p backup))
             (should (file-directory-p mirror))
             (should (string-match-p
                      "new"
                      (with-temp-buffer
                        (insert-file-contents
                         (expand-file-name "HEAD" mirror))
                        (buffer-string))))
             ;; Publisher finishes: write generation against the new mirror.
             (+git-sync-save-state
              id
              '(("version" . 1)
                ("generation" . 2)
                ("last-success" . 2.0)
                ("forge-status" . "unavailable")
                ("provider" . "github")
                ("pr-refs-status" . "unavailable")
                ("mirror-format" . 1)))
             (ignore-errors (delete-file intent))
             (+git-sync--delete-dir backup))
         (+git-sync-release-lock id nonce))
       (let ((st (+git-sync-status id)))
         (should (= (plist-get st :generation) 2))
         (should (equal (plist-get st :mirror) mirror))
         (should (string-match-p
                  "new"
                  (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "HEAD" mirror))
                    (buffer-string))))
         (should-not (file-directory-p backup))
         (should-not (file-exists-p intent)))))))

(ert-deftest git-review-sync-default-forge-async-callback-error ()
  "Delayed Forge storage callback errors complete the job as failed."
  (let* ((had-forge (featurep 'forge))
         (got nil)
         (+git-sync-url-retrieve-function
          (lambda (_url callback &optional _cbargs _silent _inhibit)
            (run-at-time
             0.05 nil
             (lambda () (funcall callback nil)))
            (generate-new-buffer " *git-sync-url-cb-err*"))))
    (unwind-protect
        (progn
          (unless (memq 'forge features) (push 'forge features))
          (+git-sync--ensure-forge-advice)
          (cl-letf
              (((symbol-function '+git-sync--choose-seed-root)
                (lambda (_) default-directory))
               ((symbol-function 'forge-get-repository)
                (lambda (&rest _) 'fake-repo))
               ((symbol-function 'forge--pull)
                (lambda (_repo &optional _callback _since)
                  (url-retrieve
                   "http://127.0.0.1:1/cb-err"
                   (lambda (&rest _)
                     (error "forge sqlite store boom"))))))
            (setq got nil)
            (+git-sync--forge-pull-default
             "github.com/org/cb-err"
             (lambda (st) (setq got st)))
            (let ((deadline (+ (float-time) 3)))
              (while (and (< (float-time) deadline) (null got))
                (accept-process-output nil 0.02)))
            (should (consp got))
            (should (eq (car got) 'failed))
            (should (string-match-p "sqlite store boom"
                                    (format "%s" (cadr got))))))
      (unless had-forge
        (setq features (delq 'forge features))))))

(ert-deftest git-review-sync-dead-lock-auto-recovered ()
  "A dead-owner lock is reclaimed safely across racing processes."
  (git-review-sync--with-dirs
   (lambda (reg)
     (let* ((id "github.com/org/stale-manual")
            (core (expand-file-name "core" user-emacs-directory))
            (out-a (make-temp-file "git-review-stale-a-"))
            (out-b (make-temp-file "git-review-stale-b-"))
            (gate (make-temp-file "git-review-stale-gate-"))
            (script-a (make-temp-file "git-review-stale-sa-" nil ".el"))
            (script-b (make-temp-file "git-review-stale-sb-" nil ".el"))
            (dead-nonce "dead-nonce-no-auto-clear")
            (lock-dir (+git-sync-lock-file id)))
       (unwind-protect
           (progn
             (+git-sync--ensure-cache-dir id)
             (make-directory lock-dir t)
             (with-temp-file (+git-sync-lock-owner-file id)
               (insert
                (json-encode
                 `(("pid" . 2147483646)
                   ("host" . ,(system-name))
                   ("timestamp" . 1.0)
                   ("nonce" . ,dead-nonce))))
               (insert "\n"))
             ;; One-process restart transparently reclaims a dead lock.
             (let ((+git-sync-stale-lock-seconds 0))
               (let ((acq (+git-sync-try-acquire-lock id)))
                 (should (eq (car acq) 'acquired))
                 (+git-sync-release-lock id (cdr acq))))
             ;; Replant the dead lock.  Of two racing processes, exactly one
             ;; may acquire it; the other must observe that live owner.
             (make-directory lock-dir t)
             (with-temp-file (+git-sync-lock-owner-file id)
               (insert
                (json-encode
                 `(("pid" . 2147483646)
                   ("host" . ,(system-name))
                   ("timestamp" . 1.0)
                   ("nonce" . ,dead-nonce))))
               (insert "\n"))
             (dolist (pair (list (cons script-a out-a)
                                 (cons script-b out-b)))
               (with-temp-file (car pair)
                 (prin1
                  `(progn
                     (setq user-emacs-directory ,user-emacs-directory)
                     (add-to-list 'load-path ,core)
                     (require 'init-git-store)
                     (require 'init-git-sync)
                     (setq +git-store-registry-directory ,reg)
                     (setq +git-sync-stale-lock-seconds 0)
                     (while (not (file-exists-p ,gate))
                       (sleep-for 0.01))
                     (let ((result (+git-sync-try-acquire-lock ,id)))
                       (with-temp-file ,(cdr pair)
                         (prin1 (car result) (current-buffer))
                         (insert "\n"))))
                  (current-buffer))))
             (let ((proc-a (start-process "stale-busy-a" nil
                                          "emacs" "--batch" "-Q" "-l" script-a))
                   (proc-b (start-process "stale-busy-b" nil
                                          "emacs" "--batch" "-Q" "-l" script-b)))
               (sleep-for 0.15)
               (with-temp-file gate (insert "go\n"))
               (let ((deadline (+ (float-time) 10)))
                 (while (and (< (float-time) deadline)
                             (or (process-live-p proc-a)
                                 (process-live-p proc-b)))
                   (accept-process-output nil 0.05)))
               (let* ((sa (car (read-from-string
                                (with-temp-buffer
                                  (insert-file-contents out-a)
                                  (buffer-string)))))
                      (sb (car (read-from-string
                                (with-temp-buffer
                                  (insert-file-contents out-b)
                                  (buffer-string))))))
                 (should (equal (sort (list sa sb)
                                      (lambda (a b)
                                        (string< (symbol-name a)
                                                 (symbol-name b))))
                                '(acquired busy)))))
             ;; Both child processes have exited.  Their remaining acquired
             ;; lock is dead and the next session can reclaim it immediately.
             (let ((+git-sync-stale-lock-seconds 0))
               (let ((acq (+git-sync-try-acquire-lock id)))
                 (should (eq (car acq) 'acquired))
                 (+git-sync-release-lock id (cdr acq)))))
         (mapc (lambda (f) (ignore-errors (delete-file f)))
               (list out-a out-b gate script-a script-b))
         (+git-sync--remove-lock-dir id))))))

(provide 'git-review-sync-test)

;;; git-review-sync-test.el ends here
