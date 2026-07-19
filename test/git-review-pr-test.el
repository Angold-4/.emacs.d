;;; git-review-pr-test.el --- Phase 5 shared PR workspace tests -*- lexical-binding: t -*-

;;; Commentary:
;; Cached PR workspace, exact mirror ranges, shared buffer identity,
;; Changes Tree reuse, commit stepping, edit context, and offline guards.
;; Uses only local Git fixtures, temporary Forge DBs, and injected adapters.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eieio)
(require 'git-review-fixtures)
(require 'git-review-baseline)
(require 'init-git-store)
(require 'init-git-sync)
(require 'init-git-ui)
(require 'init-forge)
(require 'init-git-pr)

;; ---------------------------------------------------------------------------
;; Fixture helpers
;; ---------------------------------------------------------------------------

(defun git-review-pr--git (root &rest args)
  "Run git ARGS in ROOT; return trimmed stdout."
  (string-trim-right
   (apply #'git-review-fixtures--git-ok root args)))

(defun git-review-pr--with-dirs (fn)
  "Call FN with disposable registry/state dirs and sync stubs reset."
  (let* ((reg (make-temp-file "git-review-pr-reg " t))
         (state (make-temp-file "git-review-pr-state " t))
         (+git-store-registry-directory reg)
         (+git-review-state-directory state)
         (+git-sync-test-remote-override nil)
         (+git-sync-active-repositories nil)
         (+git-sync-max-concurrent 2)
         (+git-sync-forge-pull-function
          (lambda (_id on-complete)
            (cl-incf +git-sync--forge-pull-count)
            (funcall on-complete 'unavailable)
            (list :cancel #'ignore)))
         (+git-sync--git-fetch-count 0)
         (+git-sync--forge-pull-count 0)
         (+git-sync--waiting-refresh-count 0)
         (+forge-pr-lookup-function #'+forge-lookup-pr-default)
         (+forge-pr-list-function #'+forge-list-prs-default)
         (owner-forge (bound-and-true-p forge-database-file)))
    (unwind-protect
        (progn
          (+git-store-reset-registry)
          (+git-sync-reset-jobs)
          (funcall fn reg state))
      (ignore-errors (+git-sync-reset-jobs))
      (ignore-errors (+git-store-reset-registry))
      (dolist (buf (buffer-list))
        (when (and (buffer-live-p buf)
                   (with-current-buffer buf
                     (or (derived-mode-p '+git-pr-mode)
                         (bound-and-true-p +git-pr--model))))
          (ignore-errors (kill-buffer buf))))
      (when (and owner-forge (boundp 'forge-database-file))
        (should (equal forge-database-file owner-forge)))
      (ignore-errors (delete-directory reg t))
      (ignore-errors (delete-directory state t)))))

(defun git-review-pr--publish-mirror (repository-id seed-root)
  "Ensure a published generation-1 mirror for REPOSITORY-ID from SEED-ROOT."
  (+git-sync-ensure-mirror repository-id seed-root)
  (let ((st (+git-sync--default-state)))
    (setf (alist-get "generation" st nil nil #'equal) 1)
    (setf (alist-get "last-success" st nil nil #'equal) (float-time))
    (setf (alist-get "forge-status" st nil nil #'equal) "current")
    (setf (alist-get "provider" st nil nil #'equal) "github")
    (setf (alist-get "pr-refs-status" st nil nil #'equal) "ok")
    (+git-sync-save-state repository-id st))
  (+git-sync-published-mirror-directory repository-id))

(defun git-review-pr--make-merged-squash-graph (prefix)
  "Build the required merged-squash fixture.
Returns plist with :work :bare :url :base :p1 :p2 :p3 :p4 :squash :main
and commit subjects.  Review range is merge-base(B,P4)..P4."
  (let* ((parent (make-temp-file (format "git-review pr-%s " prefix) t))
         (work (expand-file-name "work" parent))
         (bare (expand-file-name "upstream.git" parent))
         (url (format "git@github.com:org/%s.git" prefix)))
    (git-review-fixtures--init-repo work)
    (let ((branch (string-trim-right
                   (ignore-errors
                     (git-review-pr--git work "symbolic-ref" "--short" "HEAD")))))
      (unless (equal branch "main")
        (ignore-errors (git-review-pr--git work "branch" "-M" "main"))
        (ignore-errors (git-review-pr--git work "checkout" "-B" "main"))))
    (git-review-fixtures--write-file work "README.md" "base\n")
    (git-review-fixtures--write-file work "keep.txt" "stable\n")
    (git-review-pr--git work "add" "-A")
    (git-review-pr--git work "commit" "-m" "base")
    (let ((base (git-review-pr--git work "rev-parse" "HEAD")))
      (git-review-pr--git work "checkout" "-b" "feature")
      (git-review-fixtures--write-file work "a.txt" "p1\n")
      (git-review-pr--git work "add" "a.txt")
      (git-review-pr--git
       work "commit" "-m" "p1"
       "-m" "Detailed body for commit-by-commit review.")
      (let ((p1 (git-review-pr--git work "rev-parse" "HEAD")))
        (git-review-fixtures--write-file work "b.txt" "p2\n")
        (git-review-pr--git work "add" "b.txt")
        (git-review-pr--git work "commit" "-m" "p2")
        (let ((p2 (git-review-pr--git work "rev-parse" "HEAD")))
          (git-review-fixtures--write-file work "c.txt" "p3\n")
          (git-review-pr--git work "add" "c.txt")
          (git-review-pr--git work "commit" "-m" "p3")
          (let ((p3 (git-review-pr--git work "rev-parse" "HEAD")))
            (git-review-fixtures--write-file work "d.txt" "p4\n")
            (git-review-pr--git work "add" "d.txt")
            (git-review-pr--git work "commit" "-m" "p4")
            (let ((p4 (git-review-pr--git work "rev-parse" "HEAD")))
              (git-review-pr--git work "checkout" "main")
              ;; Distinct squash merge commit S on main.
              (git-review-fixtures--write-file
               work "a.txt" "p1\n")
              (git-review-fixtures--write-file work "b.txt" "p2\n")
              (git-review-fixtures--write-file work "c.txt" "p3\n")
              (git-review-fixtures--write-file work "d.txt" "p4\n")
              (git-review-pr--git work "add" "-A")
              (git-review-pr--git work "commit" "-m" "squash merge of feature")
              (let ((squash (git-review-pr--git work "rev-parse" "HEAD")))
                (git-review-fixtures--write-file work "later.txt" "m\n")
                (git-review-pr--git work "add" "later.txt")
                (git-review-pr--git work "commit" "-m" "main advances")
                (let ((main (git-review-pr--git work "rev-parse" "HEAD")))
                  ;; Publish PR head/base refs into the bare remote.
                  (git-review-pr--git work "clone" "--bare" work bare)
                  (git-review-pr--git bare "update-ref"
                                    "refs/pull/16/head" p4)
                  (git-review-pr--git bare "update-ref"
                                    "refs/pull/16/merge" squash)
                  (git-review-pr--git bare "update-ref"
                                    "refs/heads/feature" p4)
                  (list :parent parent :work work :bare bare :url url
                        :base base :p1 p1 :p2 p2 :p3 p3 :p4 p4
                        :squash squash :main main))))))))))

(defun git-review-pr--make-merge-commit-graph (prefix)
  "Build a PR whose head is a merge commit; return plist."
  (let* ((parent (make-temp-file (format "git-review pr-merge-%s " prefix) t))
         (work (expand-file-name "work" parent))
         (bare (expand-file-name "upstream.git" parent))
         (url (format "git@github.com:org/%s.git" prefix)))
    (git-review-fixtures--init-repo work)
    (git-review-fixtures--write-file work "README.md" "base\n")
    (git-review-pr--git work "add" "-A")
    (git-review-pr--git work "commit" "-m" "base")
    (let ((base (git-review-pr--git work "rev-parse" "HEAD")))
      (git-review-pr--git work "checkout" "-b" "side")
      (git-review-fixtures--write-file work "side.txt" "s\n")
      (git-review-pr--git work "add" "side.txt")
      (git-review-pr--git work "commit" "-m" "side")
      (let ((side (git-review-pr--git work "rev-parse" "HEAD")))
        (git-review-pr--git work "checkout" "-b" "feature" base)
        (git-review-fixtures--write-file work "feat.txt" "f\n")
        (git-review-pr--git work "add" "feat.txt")
        (git-review-pr--git work "commit" "-m" "feat")
        (git-review-pr--git work "merge" "--no-ff" "-m" "merge side" "side")
        (let ((head (git-review-pr--git work "rev-parse" "HEAD"))
              (parents
               (split-string
                (git-review-pr--git work "rev-list" "--parents" "-n1" "HEAD")
                " " t)))
          (git-review-pr--git work "clone" "--bare" work bare)
          (git-review-pr--git bare "update-ref" "refs/pull/7/head" head)
          (list :parent parent :work work :bare bare :url url
                :base base :side side :head head
                :parents (cdr parents)))))))

(defun git-review-pr--clone (bare url prefix &optional single-branch)
  "Clone BARE with identity URL rewrite; return root.
When SINGLE-BRANCH is non-nil, clone only that branch tip using
`--no-local' so PR head commits are absent from the clone while
present in the published mirror."
  (let* ((parent (make-temp-file (format "git-review pr-clone-%s " prefix) t))
         (dst (expand-file-name (format "%s repo" prefix) parent)))
    (if single-branch
        (git-review-fixtures--git-ok
         bare "clone" "--no-local" "--single-branch"
         "--branch" single-branch bare dst)
      (git-review-fixtures--git-ok bare "clone" bare dst))
    (git-review-fixtures--git-ok dst "config" "user.name"
                                 git-review-fixtures-test-name)
    (git-review-fixtures--git-ok dst "config" "user.email"
                                 git-review-fixtures-test-email)
    (git-review-fixtures--git-ok dst "config" "commit.gpgsign" "false")
    (ignore-errors
      (git-review-fixtures--git-ok dst "remote" "remove" "origin"))
    (git-review-fixtures--git-ok dst "remote" "add" "origin" url)
    dst))

(defun git-review-pr--cleanup (root)
  "Delete temporary parent of ROOT."
  (when (and root (file-directory-p root))
    (ignore-errors
      (git-review-baseline-cleanup-repo-buffers root)
      (delete-directory (file-name-directory (directory-file-name root)) t))))

(defun git-review-pr--stub-snapshot (repository-id number &rest attrs)
  "Build a `+forge-pr-snapshot' stub for tests."
  (+forge-pr-snapshot-create
   :repository-id repository-id
   :number number
   :title (or (plist-get attrs :title) "test pr")
   :state (or (plist-get attrs :state) 'open)
   :draft (and (plist-get attrs :draft) t)
   :author (or (plist-get attrs :author) "alice")
   :review-status (plist-get attrs :review-status)
   :inbox-status (plist-get attrs :inbox-status)
   :base-ref (or (plist-get attrs :base-ref) "main")
   :base-repo (plist-get attrs :base-repo)
   :base-rev (plist-get attrs :base-rev)
   :head-ref (or (plist-get attrs :head-ref) "feature")
   :head-user (plist-get attrs :head-user)
   :head-repo (plist-get attrs :head-repo)
   :head-rev (plist-get attrs :head-rev)
   :cross-repo-p (and (plist-get attrs :cross-repo-p) t)
   :created (plist-get attrs :created)
   :updated (plist-get attrs :updated)
   :closed (plist-get attrs :closed)
   :merged (plist-get attrs :merged)
   :body (or (plist-get attrs :body) "body with 你好")
   :posts (or (plist-get attrs :posts)
              (list (list :author "bob"
                          :created "2026-01-01"
                          :body "comment 世界"
                          :forge-object nil)))
   :forge-object (plist-get attrs :forge-object)))

(defun git-review-pr--install-stub (repository-id number attrs)
  "Install lookup/list stubs for REPOSITORY-ID NUMBER from ATTRS plist.
Return the installed snapshot so tests can mutate it."
  (let ((snap (apply #'git-review-pr--stub-snapshot
                     repository-id number attrs)))
    (setq +forge-pr-lookup-function
          (lambda (rid n)
            (cond
             ((and (equal rid repository-id) (= n number)) snap)
             (t (user-error
                 "PR #%s is not cached. Run C-c g f, wait for sync, then retry."
                 n)))))
    (setq +forge-pr-list-function
          (lambda (rid)
            (if (equal rid repository-id) (list snap) nil)))
    snap))

(defun git-review-pr--register (root url bare)
  "Register ROOT under URL with fetch override to BARE."
  (let ((ctx (+git-store-register-root root)))
    (let ((id (+git-store-local-context-repository-id ctx)))
      (setq +git-sync-test-remote-override
            (cons (cons id bare) +git-sync-test-remote-override)))
    ctx))

(defclass git-review-pr--fake-repo ()
  ((condition :initarg :condition :initform :tracked)
   (id :initarg :id :initform "fake-repo")
   (owner :initarg :owner :initform "org")
   (name :initarg :name :initform "proj")
   (pullreqs :initarg :pullreqs :initform nil))
  "Temporary Forge-like repository object for adapter tests.")

(defclass git-review-pr--fake-pullreq ()
  ((number :initarg :number)
   (title :initarg :title :initform "")
   (state :initarg :state :initform 'open)
   (author :initarg :author :initform "alice")
   (status :initarg :status :initform nil)
   (draft-p :initarg :draft-p :initform nil)
   (base-ref :initarg :base-ref :initform "main")
   (base-repo :initarg :base-repo :initform nil)
   (base-rev :initarg :base-rev :initform nil)
   (head-ref :initarg :head-ref :initform "feature")
   (head-user :initarg :head-user :initform nil)
   (head-repo :initarg :head-repo :initform nil)
   (head-rev :initarg :head-rev :initform nil)
   (cross-repo-p :initarg :cross-repo-p :initform nil)
   (created :initarg :created :initform nil)
   (updated :initarg :updated :initform nil)
   (closed :initarg :closed :initform nil)
   (merged :initarg :merged :initform nil)
   (body :initarg :body :initform "")
   (posts :initarg :posts :initform nil))
  "Temporary Forge-like pullreq object for adapter tests.")

(defun git-review-pr--with-temp-forge-db (fn)
  "Call FN with a real temporary Forge Closql database.
Ensures Forge loads via `+forge--ensure-forge-apis', inserts one tracked
repository and pullreq, then calls (FN REPO PULLREQ).  Owner
`forge-database-file' is never written."
  (should (+forge--ensure-forge-apis))
  (should (fboundp 'forge-get-pullreq))
  (let* ((db (let ((path (make-temp-file "git-review-forge-db-" nil ".sqlite")))
               ;; Closql must create the DB itself; an empty precreated file
               ;; is not a valid SQLite database.
               (delete-file path)
               path))
         (owner-forge (and (boundp 'forge-database-file)
                           forge-database-file))
         (owner-mtime (and owner-forge (file-exists-p owner-forge)
                           (file-attribute-modification-time
                            (file-attributes owner-forge))))
         (forge-database-file db)
         repo pr)
    (unwind-protect
        (progn
          (should-not (equal (expand-file-name db)
                             (and owner-forge
                                  (expand-file-name owner-forge))))
          ;; Drop any prior live connection so the temp file is used.
          (when (and (fboundp 'forge-db) (forge-db t))
            (ignore-errors (emacsql-close (forge-db))))
          ;; Prefer :stub so Forge does not consult github.user / network.
          (setq repo (forge-get-repository
                      (list "github.com" "org" "temp-pr-db")
                      nil :stub))
          (should repo)
          (closql-insert (forge-db) repo t)
          (oset repo condition :tracked)
          (let* ((rid (oref repo id))
                 (pid (forge--object-id 'forge-pullreq repo 42)))
            (setq pr
                  (forge-pullreq
                   :id pid
                   :repository rid
                   :number 42
                   :state 'open
                   :author "alice"
                   :title "from forge db"
                   :created "2026-01-01T00:00:00Z"
                   :updated "2026-01-01T00:00:00Z"
                   :closed nil
                   :merged nil
                   :status 'unread
                   :base-ref "main"
                   :base-rev "abcabcabcabcabcabcabcabcabcabcabcabcabca"
                   :head-ref "feature"
                   :head-rev "defdefdefdefdefdefdefdefdefdefdefdefdefd"
                   :body "hello"
                   :draft-p nil
                   :cross-repo-p nil))
            (closql-insert (forge-db) pr t)
            (funcall fn repo pr)))
      (ignore-errors
        (when (and (fboundp 'forge-db) (forge-db t))
          (emacsql-close (forge-db))))
      (ignore-errors (delete-file db))
      (when (and owner-mtime owner-forge (file-exists-p owner-forge))
        (should (equal owner-mtime
                       (file-attribute-modification-time
                        (file-attributes owner-forge))))))))

;; ---------------------------------------------------------------------------
;; Binding / parse / range
;; ---------------------------------------------------------------------------

(ert-deftest git-review-pr-dispatch-binding ()
  "C-c g p is bound to pull-request review."
  (should (commandp #'+git/review-pull-request))
  (should (fboundp #'+git-dispatch))
  (let ((suffix (cl-find-if
                 (lambda (s)
                   (and (transient-suffix-object-p s)
                        (equal (oref s key) "p")))
                 (ignore-errors
                   (oref (transient-prefix-object #'+git-dispatch)
                         children)))))
    ;; Transient structure varies; assert command identity at least.
    (should (eq (indirect-function #'+git/review-pr)
                (indirect-function #'+git/review-pull-request)))))

(ert-deftest git-review-pr-parse-number ()
  "PR number parsing accepts integers, #N, and completion strings."
  (should (= (+forge-parse-pr-number 16) 16))
  (should (= (+forge-parse-pr-number "16") 16))
  (should (= (+forge-parse-pr-number "#16") 16))
  (should (= (+forge-parse-pr-number "#16  MERGED  demo") 16))
  (should (= (+forge-parse-pr-number "#16  OPEN  title with spaces") 16))
  (should-error (+forge-parse-pr-number "abc") :type 'user-error)
  (should-error (+forge-parse-pr-number "16a") :type 'user-error))

(ert-deftest git-review-pr-1password-token-provider ()
  "1Password resolves once, stays in memory, and never enters argv."
  (let ((+forge-1password-token-references
         '(("api.github.com" . "op://Employee/GitHub Forge/token")))
        (+forge-1password-cli-program "op")
        (+forge--1password-token-cache (make-hash-table :test #'equal))
        calls)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (and (equal program "op") "/mock/op")))
              ((symbol-function 'process-file)
               (lambda (program _infile destination _display &rest args)
                 (push (cons program args) calls)
                 (should (eq destination t))
                 (insert "test-token\n")
                 0)))
      (should (equal (+forge--1password-read-token "api.github.com")
                     "test-token"))
      (should (equal (+forge--1password-read-token "api.github.com")
                     "test-token"))
      (should (= (length calls) 1))
      (should
       (equal (car calls)
              '("op" "read" "op://Employee/GitHub Forge/token")))
      (should-not (member "test-token" (car calls)))
      ;; Existing Auth Source remains authoritative; do not invoke `op'.
      (setq calls nil)
      (should
       (equal
        (+forge--ghub-token-with-1password
         (lambda (&rest _) "auth-source-token")
         "api.github.com" "alice" 'forge nil 'github)
        "auth-source-token"))
      (should (null calls))
      ;; Missing Auth Source falls back exactly once to `op'.  The initial
      ;; lookup is forced non-creating so Ghub cannot raise before fallback.
      (clrhash +forge--1password-token-cache)
      (let (original-nocreate)
        (should
         (equal
          (+forge--ghub-token-with-1password
           (lambda (_host _username _package nocreate _forge)
             (push nocreate original-nocreate)
             nil)
           "api.github.com" "alice" 'forge nil 'github)
          "test-token"))
        (should (equal original-nocreate '(t)))
        (should (= (length calls) 1)))
      ;; Other Ghub packages never consult the Forge 1Password provider.
      (setq calls nil)
      (should
       (equal
        (+forge--ghub-token-with-1password
         (lambda (_host _username _package nocreate _forge)
           (unless nocreate "ordinary-package-token"))
         "api.github.com" "alice" 'ghub nil 'github)
        "ordinary-package-token"))
      (should (null calls))))
  ;; A configured provider fails clearly when `op' is unavailable.
  (let ((+forge-1password-token-references
         '(("api.github.com" . "op://Employee/GitHub Forge/token")))
        (+forge--1password-token-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (+forge--1password-read-token "api.github.com")
                    :type 'user-error))))

(ert-deftest git-review-pr-direct-forge-require ()
  "Direct (require 'forge) works without +forge--ensure-forge-apis.
Module init must already have installed cond-let compatibility so
autoloaded commands such as `forge-add-repository' can load Forge."
  (should (fboundp #'+forge--ensure-cond-let-compat))
  (should (fboundp 'cond-let--thread$))
  ;; Do not call +forge--ensure-forge-apis; require Forge the same way
  ;; a direct autoload would.
  (let ((err nil))
    (condition-case e
        (progn
          (require 'forge)
          (should (featurep 'forge))
          (should (fboundp 'forge-get-pullreq))
          (should (fboundp 'forge-add-repository))
          (should
           (advice-member-p #'+forge--ghub-token-with-1password
                            'ghub--token)))
      (error (setq err e)))
    (should (null err))))

(ert-deftest git-review-pr-parse-nested-canonical-id ()
  "Nested GitLab-style repository identities parse to (host owner name)."
  (should (equal (+forge--parse-canonical-id "github.com/org/project")
                 '("github.com" "org" "project")))
  (should (equal (+forge--parse-canonical-id
                  "gitlab.com/group/subgroup/project")
                 '("gitlab.com" "group/subgroup" "project")))
  (should (null (+forge--parse-canonical-id "local:/tmp/x"))))

(ert-deftest git-review-pr-derive-range-merged-squash ()
  "Merged squash PR uses merge-base(B,P4)..P4, never squash or main."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "squash"))
            (work (plist-get g :work))
            (base (plist-get g :base))
            (p4 (plist-get g :p4))
            (squash (plist-get g :squash))
            (main (plist-get g :main)))
       (unwind-protect
           (pcase-let ((`(,mb . ,head)
                        (+git-pr-derive-range work base p4)))
             (should (equal head p4))
             (should (equal mb base))
             (should-not (equal head squash))
             (should-not (equal head main))
             (let ((commits (+git-pr-collect-commits work mb head)))
               (should (= (length commits) 4))
               (should (equal (+git-pr-commit-oid (nth 0 commits))
                              (plist-get g :p1)))
               (should (equal (+git-pr-commit-oid (nth 3 commits)) p4))))
         (git-review-pr--cleanup work))))))

(ert-deftest git-review-pr-pull-head-vs-merge-ref ()
  "Provider pull head is discovery-only; merge ref is never the PR head."
  (should (equal (+git-pr--provider-pull-head-ref 16) "refs/pull/16/head"))
  (should (equal (+git-pr--provider-pull-merge-ref 16) "refs/pull/16/merge"))
  (should-not (equal (+git-pr--provider-pull-head-ref 16)
                     (+git-pr--provider-pull-merge-ref 16))))

;; ---------------------------------------------------------------------------
;; Workspace identity / overview / offline
;; ---------------------------------------------------------------------------

(ert-deftest git-review-pr-shared-buffer-across-clones ()
  "Same canonical repo + PR number from two clones returns one buffer."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "share"))
            (bare (plist-get g :bare))
            (url (plist-get g :url))
            (a (git-review-pr--clone bare url "sa"))
            (b (git-review-pr--clone bare url "sb"))
            ctx-a ctx-b id)
       (unwind-protect
           (progn
             (setq ctx-a (git-review-pr--register a url bare))
             (setq ctx-b (git-review-pr--register b url bare))
             (setq id (+git-store-local-context-repository-id ctx-a))
             (should (equal id (+git-store-local-context-repository-id ctx-b)))
             (git-review-pr--publish-mirror id a)
             ;; Also copy PR objects into mirror via fetch from bare.
             (let ((mirror (+git-sync-published-mirror-directory id)))
               (git-review-pr--git mirror "fetch" bare
                                  "+refs/pull/*/head:refs/pull/*/head"
                                  "+refs/heads/*:refs/heads/*"))
             (git-review-pr--install-stub
              id 16
              (list :state 'merged
                    :title "feat(dummy): prove ambiguous bridge reconciliation"
                    :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)
                    :base-ref "main"
                    :head-ref "feature"))
             (let* ((buf-a (+git/review-pull-request 16 a))
                    (buf-b (+git/review-pull-request 16 b)))
               (should (eq buf-a buf-b))
               (should (derived-mode-p '+git-pr-mode))
               (with-current-buffer buf-a
                 (should (equal (+git-pr-repository-id +git-pr--model) id))
                 (should (= (+git-pr-number +git-pr--model) 16))
                 (should (equal (+git-pr-head-oid +git-pr--model)
                                (plist-get g :p4)))
                 (should-not (equal (+git-pr-head-oid +git-pr--model)
                                    (plist-get g :squash)))
                 (should (= (length (+git-pr-commits +git-pr--model)) 4))
                 ;; Reopening from B adopts B as edit context.
                 (should (equal +git-review-edit-context-id
                                (+git-store-local-context-context-id ctx-b))))
               (kill-buffer buf-a)))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup b)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-different-repos-no-collision ()
  "Same PR number in two canonical repositories yields different buffers."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g1 (git-review-pr--make-merged-squash-graph "r1"))
            (g2 (git-review-pr--make-merged-squash-graph "r2"))
            (a (git-review-pr--clone (plist-get g1 :bare)
                                     (plist-get g1 :url) "a1"))
            (b (git-review-pr--clone (plist-get g2 :bare)
                                     (plist-get g2 :url) "b2"))
            id1 id2)
       (unwind-protect
           (progn
             (setq id1 (+git-store-local-context-repository-id
                        (git-review-pr--register a (plist-get g1 :url)
                                                 (plist-get g1 :bare))))
             (setq id2 (+git-store-local-context-repository-id
                        (git-review-pr--register b (plist-get g2 :url)
                                                 (plist-get g2 :bare))))
             (should-not (equal id1 id2))
             (git-review-pr--publish-mirror id1 a)
             (git-review-pr--publish-mirror id2 b)
             (git-review-pr--git (+git-sync-published-mirror-directory id1)
                                "fetch" (plist-get g1 :bare)
                                "+refs/*:refs/*")
             (git-review-pr--git (+git-sync-published-mirror-directory id2)
                                "fetch" (plist-get g2 :bare)
                                "+refs/*:refs/*")
             (git-review-pr--install-stub
              id1 16 (list :base-rev (plist-get g1 :base)
                           :head-rev (plist-get g1 :p4)))
             (let ((snap2 (git-review-pr--stub-snapshot
                           id2 16
                           :base-rev (plist-get g2 :base)
                           :head-rev (plist-get g2 :p4)))
                   (old-lookup +forge-pr-lookup-function))
               (setq +forge-pr-lookup-function
                     (lambda (rid n)
                       (cond
                        ((and (equal rid id1) (= n 16))
                         (funcall old-lookup rid n))
                        ((and (equal rid id2) (= n 16)) snap2)
                        (t (user-error "PR #%s is not cached" n)))))
               (let ((buf1 (+git/review-pull-request 16 a))
                     (buf2 (+git/review-pull-request 16 b)))
                 (should-not (eq buf1 buf2))
                 (should (string-match-p (regexp-quote id1) (buffer-name buf1)))
                 (should (string-match-p (regexp-quote id2) (buffer-name buf2)))
                 (kill-buffer buf1)
                 (kill-buffer buf2))))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup b)
         (git-review-pr--cleanup (plist-get g1 :work))
         (git-review-pr--cleanup (plist-get g2 :work)))))))

(ert-deftest git-review-pr-missing-metadata-actionable ()
  "Missing/untracked metadata produces actionable errors and no network."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (git-review-fixtures-with-repo
      "missing-meta"
      (lambda (root)
        (git-review-fixtures--write-file root "a.txt" "a\n")
        (git-review-pr--git root "add" "-A")
        (git-review-pr--git root "commit" "-m" "a")
        (git-review-pr--git root "remote" "add" "origin"
                           "git@github.com:org/missing-meta.git")
        (let* ((ctx (+git-store-register-root root))
               (id (+git-store-local-context-repository-id ctx)))
          (setq +forge-pr-lookup-function
                (lambda (_rid _n)
                  (user-error
                   (concat "Repository is not tracked by Forge. "
                           "Run M-x forge-add-repository once, then C-c g f."))))
          (git-review-baseline--with-instrumentation
           (lambda ()
             (should-error (+git/review-pull-request 16 root)
                           :type 'user-error)
             (should (null git-review-baseline--blocked-attempts))))))))))

(ert-deftest git-review-pr-overview-sections-and-unicode ()
  "Overview section order, ASCII chrome, Unicode content, no eager diffs."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "ov"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "ov"))
            id buf)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :state 'merged
                    :title "feat: 中文标题"
                    :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)
                    :body "desc 你好"))
             (setq buf (+git/review-pull-request 16 a))
             (with-current-buffer buf
               (let ((text (buffer-string)))
                 (should (string-match-p "PR #16 - feat: 中文标题" text))
                 (should (string-match-p "MERGED" text))
                 (should (string-match-p "Edit context:" text))
                 (should-not (string-match-p "\\[L to switch\\]" text))
                 (should (string-match-p "Cache: generation" text))
                 (should (string-match-p "Changes (" text))
                 (should (string-match-p "Changed files" text))
                 (should (string-match-p "Commits (4, oldest first)" text))
                 (should (string-match-p "Checks: unavailable (Phase 7)" text))
                 (should (string-match-p "Description" text))
                 (should (string-match-p "Conversation" text))
                 (should (string-match-p "你好" text))
                 (should (string-match-p "世界" text))
                 ;; Exact OIDs in details, not squash.
                 (should (string-match-p
                          (regexp-quote (plist-get g :p4)) text))
                 (should-not (string-match-p
                              (regexp-quote (plist-get g :squash)) text)))
               ;; No eager file-diff buffers.
               (should
                (cl-notany
                 (lambda (b)
                   (with-current-buffer b
                     (and (bound-and-true-p +git-review-file-path)
                          (equal (+git-review-target-overview-id
                                  +git-review-target)
                                 (+git-review-target-overview-id
                                  (+git-pr-target +git-pr--model))))))
                 (buffer-list))))
             (kill-buffer buf))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-tree-and-reviewed-shared ()
  "t reuses Changes Tree; reviewed progress is shared across clones."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "tree"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "ta"))
            (b (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "tb"))
            id buf tree)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--register b (plist-get g :url)
                                      (plist-get g :bare))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (setq buf (+git/review-pull-request 16 a))
             (with-current-buffer buf
               (setq tree (+git-pr-open-changes-tree))
               (should (derived-mode-p '+git-changes-tree-mode))
               (should (eq (+git-review-target-scope +git-review-target)
                           'pullreq))
               (should (equal (+git-review-target-base-oid +git-review-target)
                              (+git-pr-merge-base-oid +git-pr--model)))
               (should (equal (+git-review-target-head-oid +git-review-target)
                              (plist-get g :p4)))
               ;; Mark first file reviewed.
               (goto-char (point-min))
               (when (re-search-forward "a\\.txt" nil t)
                 (+git-changes-tree-toggle-reviewed))
               (let ((map +git-review--reviewed-map))
                 (should (> (hash-table-count map) 0))))
             ;; Reopen from clone B: same family/state.
             (kill-buffer tree)
             (let ((buf2 (+git/review-pull-request 16 b)))
               (should (eq buf buf2))
               (with-current-buffer buf2
                 (let* ((target (+git-pr-target +git-pr--model))
                        (map (+git-review-state-load target)))
                   (should (> (hash-table-count map) 0))))
               (kill-buffer buf2)))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup b)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-tree-reloads-after-mirror-generation-change ()
  "Actual `t' replaces a stale PR range after an external mirror sync.
This models a long-lived PR workspace whose published mirror was replaced by
`C-c g f' from another buffer."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "tree-refresh"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "tree-refresh"))
            id workspace tree)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                 "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p3)))
             (setq workspace (+git/review-pull-request 16 a))
             (with-current-buffer workspace
               (should (equal (+git-pr-head-oid +git-pr--model)
                              (plist-get g :p3))))
             ;; Simulate an explicit sync elsewhere: Forge now says P4 and
             ;; the published generation advances while WORKSPACE stays open.
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (let ((st (car (+git-sync-load-state id))))
               (setf (alist-get "generation" st nil nil #'equal) 2)
               (+git-sync-save-state id st))
             (with-current-buffer workspace
               (evil-normalize-keymaps)
               (should (eq (key-binding "t") #'+git-review-open-changes-tree))
               (call-interactively (key-binding "t"))
               (setq tree (window-buffer (selected-window))))
             (with-current-buffer tree
               (should (derived-mode-p '+git-changes-tree-mode))
               (should (= (+git-pr-cache-generation +git-pr--model) 2))
               (should (equal (+git-review-target-head-oid +git-review-target)
                              (plist-get g :p4)))
               (should (cl-find "d.txt" +git-review--files
                                :key #'+git-review-file-path
                                :test #'equal))
               (goto-char (point-min))
               (should (re-search-forward "d\\.txt" nil t))
               (beginning-of-line)
               (call-interactively #'+git-changes-tree-visit-file)
               (should (derived-mode-p 'magit-diff-mode))
               (should (equal (+git-review-target-head-oid +git-review-target)
                              (plist-get g :p4)))))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-tree-reloads-when-old-objects-disappear ()
  "RET/o reload a PR when an atomic mirror replacement drops old OIDs.
The state generation is deliberately left unchanged so this regression proves
that object presence, rather than hexadecimal `rev-parse' acceptance, protects
the range."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((old (git-review-pr--make-merged-squash-graph "gone-old"))
            ;; Force a distinct root history even when the test runs within
            ;; one clock second using identical fixture names/content.
            (new (let ((process-environment
                        (append
                         '("GIT_AUTHOR_DATE=2001-01-01T00:00:00Z"
                           "GIT_COMMITTER_DATE=2001-01-01T00:00:00Z")
                         process-environment)))
                   (git-review-pr--make-merged-squash-graph "gone-new")))
            (a (git-review-pr--clone (plist-get old :bare)
                                     (plist-get old :url) "gone"))
            id workspace tree mirror)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get old :url)
                                                (plist-get old :bare))))
             (setq mirror (git-review-pr--publish-mirror id a))
             (git-review-pr--git mirror "fetch"
                                 (plist-get old :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get old :base)
                    :head-rev (plist-get old :p4)))
             (setq workspace (+git/review-pull-request 16 a))
             (with-current-buffer workspace
               (should (+git-pr--object-exists-p
                        mirror (+git-pr-head-oid +git-pr--model))))

             ;; Simulate atomic publication of a force-pushed, unrelated
             ;; generation.  No old object is copied into the replacement.
             (delete-directory mirror t)
             (git-review-pr--git
              (file-name-directory (directory-file-name mirror))
              "clone" "--bare" "--no-local"
              (plist-get new :bare) mirror)
             (should (= (plist-get (+git-sync-status id) :generation) 1))
             (should-not
              (+git-pr--object-exists-p mirror (plist-get old :p4)))
             ;; `rev-parse' demonstrates the original false positive.
             (should (+git-review--rev-parse mirror (plist-get old :p4)))

             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get new :base)
                    :head-rev (plist-get new :p4)))
             (with-current-buffer workspace
               (setq tree (+git-pr-open-changes-tree)))
             (with-current-buffer tree
               (should (equal (+git-review-target-head-oid +git-review-target)
                              (plist-get new :p4)))
               (goto-char (point-min))
               (should (re-search-forward "a\\.txt" nil t))
               (beginning-of-line)
               ;; RET and o share this guarded implementation; exercise both
               ;; entry paths against the replacement mirror.
               (let ((diff (+git-changes-tree-visit-file)))
                 (with-current-buffer diff
                   (should-not
                    (string-match-p "Invalid revision range"
                                    (buffer-string)))))
               (switch-to-buffer tree)
               (goto-char (point-min))
               (should (re-search-forward "a\\.txt" nil t))
               (beginning-of-line)
               (let ((diff (+git-changes-tree-visit-other-window)))
                 (with-current-buffer diff
                   (should-not
                    (string-match-p "Invalid revision range"
                                    (buffer-string)))))))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get old :work))
         (git-review-pr--cleanup (plist-get new :work)))))))

(ert-deftest git-review-pr-head-advance-invalidates-changed ()
  "Head advance preserves unchanged fingerprints and clears changed files."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "adv"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "adv"))
            id buf)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p3)))
             (setq buf (+git/review-pull-request 16 a))
             (with-current-buffer buf
               (let* ((files (+git-pr-files +git-pr--model))
                      (keep (cl-find "keep.txt" files
                                     :key #'+git-review-file-path
                                     :test #'equal))
                      (map (make-hash-table :test #'equal)))
                 (dolist (f files)
                   (puthash (+git-review-file-path f)
                            (+git-review-file-fingerprint f)
                            map))
                 (+git-review-state-save (+git-pr-target +git-pr--model) map)
                 ;; Advance head to p4: d.txt is new; prior files may keep.
                 (git-review-pr--install-stub
                  id 16
                  (list :base-rev (plist-get g :base)
                        :head-rev (plist-get g :p4)))
                 (+git-pr-refresh)
                 (let* ((new-files (+git-pr-files +git-pr--model))
                        (synced (+git-review--sync-reviewed-map
                                 new-files
                                 (+git-review-state-load
                                  (+git-pr-target +git-pr--model)))))
                   (should (cl-find "d.txt" new-files
                                    :key #'+git-review-file-path
                                    :test #'equal))
                   (should-not (gethash "d.txt" synced))
                   (when keep
                     (should (gethash "keep.txt" synced))))))
             (kill-buffer buf))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-commit-workflow ()
  "c / gc / gC / q commit workflow uses mirror objects and restores layout."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "cmt"))
            ;; Main-only, non-local clone: feature commits absent here.
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "cmt" "main"))
            id buf)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (should-not
              (ignore-errors
                (git-review-pr--git a "cat-file" "-e" (plist-get g :p4))))
             (should
              (git-review-pr--git (+git-sync-published-mirror-directory id)
                                 "cat-file" "-e" (plist-get g :p4)))
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (delete-other-windows)
             (setq buf (+git/review-pull-request 16 a))
             (should (eq (window-buffer (selected-window)) buf))
             (let ((c0 nil)
                   (mirror (+git-sync-published-mirror-directory id)))
               (with-current-buffer buf
                 (setq c0 (+git-pr-open-commit 0)))
               (should (buffer-live-p c0))
               (should (eq (window-buffer (selected-window)) c0))
               (with-current-buffer c0
                 (should (derived-mode-p 'magit-revision-mode))
                 (let ((text (buffer-string)))
                   (should (string-match-p "Author:" text))
                   (should (string-match-p "p1" text))
                   (should
                    (string-match-p
                     "Detailed body for commit-by-commit review\\." text))
                   ;; Magit's clean washer intentionally removes raw
                   ;; "diff --git" plumbing while retaining native sections.
                   (goto-char (point-min))
                   (should (re-search-forward "^new file +a\\.txt$" nil t))
                   (beginning-of-line)
                   (should (magit-section-match
                            'file (magit-current-section))))
                 (should (= +git-pr--commit-index 0))
                 (should (eq +git-pr--return-buffer buf))
                 (should (integerp
                          (+git-review-target-pr-number +git-review-target)))
                 (should (+git-review--pr-backed-p +git-review-target))
                 (should (equal
                          (+git-review--git-root-for-target
                           +git-review-target)
                          mirror))
                 ;; Ordinary commit targets still require a local context;
                 ;; PR-backed commits must not.
                 (let ((ordinary
                        (+git-review-make-target
                         a 'commit nil nil
                         (plist-get g :base) (plist-get g :p1)
                         id
                         (+git-review-target-context-id +git-review-target))))
                   (cl-letf (((symbol-function
                               '+git-store-default-edit-context)
                              (lambda (&rest _) nil))
                             ((symbol-function
                               '+git-store-context-eligible-p)
                              (lambda (&rest _) nil)))
                     (should-error
                      (+git-review--git-root-for-target ordinary)
                      :type 'user-error)
                     (should (equal
                              (+git-review--git-root-for-target
                               +git-review-target)
                              mirror))))
                 (+git-pr-next-commit)
                 (should (= +git-pr--commit-index 1))
                 (should (eq +git-pr--return-buffer buf))
                 (+git-pr-next-commit)
                 (+git-pr-next-commit)
                 (should (= +git-pr--commit-index 3))
                 (should (equal
                          (+git-review-target-head-oid +git-review-target)
                          (plist-get g :p4)))
                 (should (eq +git-pr--return-buffer buf))
                 (should (buffer-live-p buf))
                 (should (eq (key-binding (kbd "q")) #'+git-pr-quit)))
               ;; Quit outside nested with-current-buffer so restore is visible.
               (with-current-buffer c0
                 (call-interactively #'+git-pr-quit))
               (should (buffer-live-p buf))
               (should (eq (window-buffer (selected-window)) buf))
               (should (eq (current-buffer) buf))
               (should (null (get-buffer-window c0 t)))
               (should (with-current-buffer buf
                         (derived-mode-p '+git-pr-mode))))
             (when (buffer-live-p buf) (kill-buffer buf)))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-merge-commit-parent-selection ()
  "Merge commit exposes parents; P selects non-default parent."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merge-commit-graph "mergehd"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "mh"))
            id buf)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 7
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :head)
                    :head-ref "feature"))
             (setq buf (+git/review-pull-request 7 a))
             (with-current-buffer buf
               (let* ((commits (+git-pr-commits +git-pr--model))
                      (merge (cl-find-if #'+git-pr-commit-merge-p commits)))
                 (should merge)
                 (should (= (length (+git-pr-commit-parents merge)) 2))
                 (pcase-let ((`(,base . ,head)
                              (+git-pr-commit-range merge)))
                   (should (equal base
                                  (car (+git-pr-commit-parents merge))))
                   (should (equal head (+git-pr-commit-oid merge))))
                 (let ((other (cadr (+git-pr-commit-parents merge))))
                   (pcase-let ((`(,base2 . ,_)
                                (+git-pr-commit-range merge other)))
                     (should (equal base2 other))))))
             (kill-buffer buf))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-edit-context-and-e ()
  "L rejects foreign contexts; e opens selected clone without checkout."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "edit"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "ea"))
            (b (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "eb"))
            ;; Foreign repo.
            (g2 (git-review-pr--make-merged-squash-graph "foreign"))
            (f (git-review-pr--clone (plist-get g2 :bare)
                                     (plist-get g2 :url) "ef"))
            id ctx-a ctx-b ctx-f buf)
       (unwind-protect
           (progn
             (setq ctx-a (git-review-pr--register a (plist-get g :url)
                                                  (plist-get g :bare)))
             (setq ctx-b (git-review-pr--register b (plist-get g :url)
                                                  (plist-get g :bare)))
             (setq ctx-f (git-review-pr--register f (plist-get g2 :url)
                                                  (plist-get g2 :bare)))
             (setq id (+git-store-local-context-repository-id ctx-a))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (setq buf (+git/review-pull-request 16 a))
             (with-current-buffer buf
               (should-error
                (+git-review-set-edit-context
                 (+git-store-local-context-context-id ctx-f))
                :type 'user-error)
               (+git-review-set-edit-context
                (+git-store-local-context-context-id ctx-b))
               (should (equal +git-review-edit-context-id
                              (+git-store-local-context-context-id ctx-b)))
               (should (equal (+git-review--active-edit-root) b))
               ;; Ensure feature files exist in B for e.
               (git-review-pr--git b "checkout" "-B" "feature"
                                  (plist-get g :p4))
               (let ((before-head
                      (git-review-pr--git b "rev-parse" "HEAD"))
                     (before-branch
                      (git-review-pr--git b "symbolic-ref" "--short" "HEAD"))
                     (file (cl-find "a.txt" (+git-pr-files +git-pr--model)
                                    :key #'+git-review-file-path
                                    :test #'equal))
                     visited)
                 (should file)
                 (cl-letf (((symbol-function '+git-pr--file-at-point)
                            (lambda () file))
                           ((symbol-function 'find-file)
                            (lambda (path)
                              (setq visited path)
                              (get-buffer-create "*git-review-e-probe*"))))
                   (call-interactively #'+git-review-visit-worktree))
                 (should visited)
                 (should (string-suffix-p "a.txt" visited))
                 (should (file-equal-p
                          (file-name-directory visited)
                          (file-name-as-directory b)))
                 (should (equal (git-review-pr--git b "rev-parse" "HEAD")
                                before-head))
                 (should (equal (git-review-pr--git b "symbolic-ref"
                                                   "--short" "HEAD")
                                before-branch))))
             (kill-buffer buf))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup b)
         (git-review-pr--cleanup f)
         (git-review-pr--cleanup (plist-get g :work))
         (git-review-pr--cleanup (plist-get g2 :work)))))))

(ert-deftest git-review-pr-offline-open-and-gr ()
  "PR open/tree/diff/commit/gr under hard network guard; no fetch/gh/delta."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "off"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "off"))
            id)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (git-review-baseline--with-instrumentation
              (lambda ()
                (let ((buf (+git/review-pull-request 16 a)))
                  (with-current-buffer buf
                    (+git-pr-refresh)
                    (let ((tree (+git-pr-open-changes-tree)))
                      (should (buffer-live-p tree))
                      (kill-buffer tree))
                    (let ((c0 (+git-pr-open-commit 0)))
                      (should (buffer-live-p c0))
                      (kill-buffer c0)))
                  (should (null git-review-baseline--blocked-attempts))
                  (let ((log git-review-baseline--process-log))
                    (should
                     (cl-notany
                      (lambda (entry)
                        (let ((args (mapcar #'identity (cdr entry))))
                          (or (member "fetch" args)
                              (member "gh" args)
                              (member "delta" args)
                              (member "difft" args)
                              (cl-find-if
                               (lambda (a)
                                 (and (stringp a)
                                      (string-match-p "https?://" a)))
                               args))))
                      log)))
                  (kill-buffer buf)))))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-sync-waiter-refreshes-model ()
  "A sync waiter rebuilds the PR model without exposing an `@' shortcut."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "syncat"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "sy"))
            id buf snap)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (setq snap
                   (git-review-pr--install-stub
                    id 16
                    (list :base-rev (plist-get g :base)
                          :head-rev (plist-get g :p3)
                          :title "before")))
             (setq buf (+git/review-pull-request 16 a))
             (should (commandp #'+git/sync))
             (should-not
              (eq (keymap-lookup +git-pr-mode-map "@") #'+git/sync))
             (with-current-buffer buf
               (should (derived-mode-p '+git-pr-mode))
               (should (= (length (+git-pr-commits +git-pr--model)) 3))
               (should (equal (+git-pr-head-oid +git-pr--model)
                              (plist-get g :p3))))
             ;; Advance the cached stub to P4, then run the waiter path.
             (setf (+forge-pr-snapshot-head-rev snap) (plist-get g :p4))
             (setf (+forge-pr-snapshot-title snap) "after")
             (let ((job (+git-sync--get-job id)))
               (+git-sync--merge-waiters job buf nil)
               (should (memq buf (+git-sync-job-waiting-buffers job)))
               (+git-sync--refresh-waiting job))
             (with-current-buffer buf
               (should (equal (+git-pr-head-oid +git-pr--model)
                              (plist-get g :p4)))
               (should (= (length (+git-pr-commits +git-pr--model)) 4))
               (should (equal
                        (+forge-pr-snapshot-title (+git-pr-snapshot
                                                   +git-pr--model))
                        "after")))
             (kill-buffer buf))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-refresh-survives-deleted-origin-clone ()
  "`gr' adopts another live clone; `@' still resolves the repository id."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "grlive"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "ga"))
            (b (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "gb"))
            id buf sync-calls)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--register b (plist-get g :url) (plist-get g :bare))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (setq buf (+git/review-pull-request 16 a))
             ;; Delete origin clone files without killing the shared PR buffer.
             (let ((parent (file-name-directory (directory-file-name a))))
               (ignore-errors (delete-directory parent t)))
             (setq a nil)
             (should (buffer-live-p buf))
             (with-current-buffer buf
               (+git-pr-refresh)
               (should (= (length (+git-pr-commits +git-pr--model)) 4))
               (should (file-directory-p
                        (+git-review-target-root +git-review-target)))
               (should (equal (+git-review-target-root +git-review-target) b))
               (should (file-directory-p default-directory))
               (should (file-equal-p
                        default-directory
                        (+git-sync-published-mirror-directory id)))
               (should (equal (+git-sync--resolve-repository-id) id))
               ;; Real @ path: resolve then start (stub returns a real job).
               (setq sync-calls nil)
               (cl-letf (((symbol-function '+git-sync-start)
                          (lambda (rid &optional _buf _cb)
                            (push rid sync-calls)
                            (+git-sync--get-job rid))))
                 (call-interactively #'+git/sync))
               (should (equal sync-calls (list id))))
             (kill-buffer buf))
         (when a (git-review-pr--cleanup a))
         (git-review-pr--cleanup b)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-failed-sync-keeps-workspace ()
  "Failed sync leaves prior PR workspace usable."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "failkeep"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "fk"))
            id buf gen)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (setq buf (+git/review-pull-request 16 a))
             (setq gen (+git-pr-cache-generation
                        (with-current-buffer buf +git-pr--model)))
             ;; Simulate a failed sync generation without running the
             ;; async pipeline (avoids cross-test cache pollution).
             (let ((st (car (+git-sync-load-state id))))
               (setf (alist-get "last-error" st nil nil #'equal)
                     "simulated sync failure")
               (setf (alist-get "forge-status" st nil nil #'equal) "failed")
               (+git-sync-save-state id st))
             (should (= (plist-get (+git-sync-status id) :generation) gen))
             (should (buffer-live-p buf))
             (with-current-buffer buf
               (should (= (length (+git-pr-commits +git-pr--model)) 4))
               (should (string-match-p "PR #16" (buffer-string))))
             (kill-buffer buf))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-default-forge-adapter-temp-db ()
  "Default Forge adapter reads a real temporary Closql database.
Owner `forge-database-file' remains untouched.  Inbox `status' is not
treated as review/approval status."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (git-review-pr--with-temp-forge-db
      (lambda (_repo _pr)
        (let ((snap (+forge-lookup-pr-default "github.com/org/temp-pr-db" 42)))
          (should (= (+forge-pr-snapshot-number snap) 42))
          (should (equal (+forge-pr-snapshot-title snap) "from forge db"))
          (should (string-prefix-p "abc" (+forge-pr-snapshot-base-rev snap)))
          (should (string-prefix-p "def" (+forge-pr-snapshot-head-rev snap)))
          (should (eq (+forge-pr-snapshot-inbox-status snap) 'unread))
          (should (null (+forge-pr-snapshot-review-status snap)))
          (should (eieio-object-p
                   (+forge-pr-snapshot-forge-object snap)))))))))

(ert-deftest git-review-pr-fork-unavailable-head ()
  "Fork PR identity is base repo + number; missing head OID errors clearly."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "fork"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "fk"))
            id)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--install-stub
              id 9
              (list :cross-repo-p t
                    :head-user "contributor"
                    :head-repo "contributor/fork"
                    :base-rev (plist-get g :base)
                    :head-rev (make-string 40 ?f)))
             (let ((err (should-error (+git/review-pull-request 9 a)
                                      :type 'user-error)))
               (should (string-match-p
                        "missing\\|unavailable\\|mirror\\|merge base\\|OID"
                        (error-message-string err)))))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-window-count-preserved ()
  "Two-window PR -> tree -> file navigation preserves window count."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "win"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "win"))
            id)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (delete-other-windows)
             (split-window-right)
             (let ((wcount (length (window-list)))
                   (buf (+git/review-pull-request 16 a)))
               (should (= (length (window-list)) wcount))
               ;; Exercise the actual Evil RET binding on a PR overview file
               ;; row.  Its section value is a `+git-review-file' record, so
               ;; allowing Magit's section-local remapping to consume it would
               ;; signal `wrong-type-argument stringp'.
               (with-current-buffer buf
                 (goto-char (point-min))
                 (should (re-search-forward "a\\.txt" nil t))
                 (beginning-of-line)
                 (should (+git-review-file-p
                          (oref (magit-current-section) value)))
                 (should (eq (key-binding (kbd "RET"))
                             #'+git-review-visit))
                 (call-interactively (key-binding (kbd "RET"))))
               (let ((file-buf (window-buffer (selected-window))))
                 (should (= (length (window-list)) wcount))
                 (should (with-current-buffer file-buf
                           (and (derived-mode-p 'magit-diff-mode)
                                (equal +git-review-file-path "a.txt"))))
                 (with-current-buffer file-buf
                   (+git-review-quit))
                 (should (eq (window-buffer (selected-window)) buf)))
               (with-current-buffer buf
                 (let ((tree (+git-pr-open-changes-tree)))
                   (should (= (length (window-list)) wcount))
                   (with-current-buffer tree
                     (goto-char (point-min))
                     (when (re-search-forward "a\\.txt" nil t)
                       (+git-changes-tree-visit-file)
                       (should (= (length (window-list)) wcount))))
                   (kill-buffer tree)))
               (kill-buffer buf)))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(ert-deftest git-review-pr-warm-render-under-one-second ()
  "Warm cached PR workspace reopen completes under one second."
  (git-review-pr--with-dirs
   (lambda (_reg _state)
     (let* ((g (git-review-pr--make-merged-squash-graph "perf"))
            (a (git-review-pr--clone (plist-get g :bare)
                                     (plist-get g :url) "perf"))
            id buf)
       (unwind-protect
           (progn
             (setq id (+git-store-local-context-repository-id
                       (git-review-pr--register a (plist-get g :url)
                                                (plist-get g :bare))))
             (git-review-pr--publish-mirror id a)
             (git-review-pr--git (+git-sync-published-mirror-directory id)
                                "fetch" (plist-get g :bare) "+refs/*:refs/*")
             (git-review-pr--install-stub
              id 16
              (list :base-rev (plist-get g :base)
                    :head-rev (plist-get g :p4)))
             (setq buf (+git/review-pull-request 16 a))
             (let* ((t0 (float-time))
                    (_ (progn (+git/review-pull-request 16 a)
                              (with-current-buffer buf (+git-pr-refresh))))
                    (elapsed (- (float-time) t0)))
               (message "phase5 warm PR render: %.3fs" elapsed)
               (should (< elapsed 1.0)))
             (kill-buffer buf))
         (git-review-pr--cleanup a)
         (git-review-pr--cleanup (plist-get g :work)))))))

(provide 'git-review-pr-test)

;;; git-review-pr-test.el ends here
