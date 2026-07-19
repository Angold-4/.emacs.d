;;; git-review-integration-test.el --- Phase 3 registry/context integration -*- lexical-binding: t -*-

;;; Commentary:
;; Temporary clones, linked worktrees, URL normalization, registry
;; persistence, legacy state migration, and edit-context selection.
;; Never contacts a remote; fake fetch URLs configure same-origin identity.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'git-review-fixtures)
(require 'git-review-baseline)

(defun git-review-phase3--with-dirs (fn)
  "Call FN with disposable state and registry directories bound."
  (let* ((state (make-temp-file "git-review-state " t))
         (reg (make-temp-file "git-review-registry " t))
         (+git-review-state-directory state)
         (+git-store-registry-directory reg))
    (unwind-protect
        (progn
          (+git-store-reset-registry)
          (funcall fn state reg))
      (ignore-errors (delete-directory state t))
      (ignore-errors (delete-directory reg t))
      (ignore-errors (+git-store-reset-registry)))))

(defun git-review-phase3--set-fetch-url (root url &optional remote)
  "Configure ROOT's REMOTE (default origin) fetch URL to URL."
  (let ((remote (or remote "origin")))
    (ignore-errors
      (git-review-fixtures--git-ok root "remote" "remove" remote))
    (git-review-fixtures--git-ok root "remote" "add" remote url)))

(defun git-review-phase3--clone-same-origin (src url prefix)
  "Create an independent clone of SRC configured with fetch URL.
Uses local `git clone' with file transport then rewrites the fetch URL
to URL so both clones share a hosted canonical identity offline."
  (let* ((parent (make-temp-file (format "git-review %s " prefix) t))
         (dst (expand-file-name (format "%s repo" prefix) parent)))
    (git-review-fixtures--git-ok src "clone" src dst)
    (git-review-fixtures--git-ok dst "config" "user.name"
                                 git-review-fixtures-test-name)
    (git-review-fixtures--git-ok dst "config" "user.email"
                                 git-review-fixtures-test-email)
    (git-review-fixtures--git-ok dst "config" "commit.gpgsign" "false")
    (git-review-phase3--set-fetch-url dst url)
    dst))

(defun git-review-phase3--cleanup-root (root)
  "Delete ROOT's temporary parent directory when present."
  (when (and root (file-directory-p root))
    (ignore-errors
      (delete-directory (file-name-directory (directory-file-name root)) t))))

;; ---------------------------------------------------------------------------
;; URL normalization
;; ---------------------------------------------------------------------------

(ert-deftest git-review-phase3-url-normalize-transport-equivalence ()
  "SSH SCP, ssh://, HTTPS, and git:// normalize to one canonical id."
  (let ((expected "github.com/org/dragon"))
    (should (equal (+git-store-normalize-remote-url
                    "git@github.com:org/dragon.git")
                   expected))
    (should (equal (+git-store-normalize-remote-url
                    "ssh://git@github.com/org/dragon.git")
                   expected))
    (should (equal (+git-store-normalize-remote-url
                    "https://github.com/org/dragon.git")
                   expected))
    (should (equal (+git-store-normalize-remote-url
                    "git://github.com/org/dragon.git")
                   expected))
    (should (equal (+git-store-normalize-remote-url
                    "https://user@github.com/org/dragon.git")
                   expected))))

(ert-deftest git-review-phase3-url-normalize-host-port-path ()
  "Different hosts, nested groups, and meaningful ports stay distinct."
  (should (equal (+git-store-normalize-remote-url
                  "https://github.com/org/dragon.git")
                 "github.com/org/dragon"))
  (should (equal (+git-store-normalize-remote-url
                  "https://gitlab.com/org/dragon.git")
                 "gitlab.com/org/dragon"))
  (should-not (equal (+git-store-normalize-remote-url
                      "https://github.com/org/dragon.git")
                     (+git-store-normalize-remote-url
                      "https://gitlab.com/org/dragon.git")))
  (should (equal (+git-store-normalize-remote-url
                  "https://gitlab.com/group/sub/dragon.git")
                 "gitlab.com/group/sub/dragon"))
  (should (equal (+git-store-normalize-remote-url
                  "ssh://git@gitlab.example:2222/group/dragon.git")
                 "gitlab.example:2222/group/dragon"))
  ;; Default ports are omitted.
  (should (equal (+git-store-normalize-remote-url
                  "https://github.com:443/org/dragon.git")
                 "github.com/org/dragon"))
  (should (equal (+git-store-normalize-remote-url
                  "ssh://git@github.com:22/org/dragon.git")
                 "github.com/org/dragon")))

(ert-deftest git-review-phase3-url-normalize-local-and-malformed ()
  "Malformed URLs and local/file remotes degrade safely."
  (should (null (+git-store-normalize-remote-url "")))
  (should (null (+git-store-normalize-remote-url "not a url")))
  (should (null (+git-store-normalize-remote-url "https://github.com/")))
  (should (null (+git-store-normalize-remote-url "https://github.com/onlyone")))
  ;; Relative remotes require an explicit base directory.
  (should (null (+git-store-normalize-remote-url "../origin.git")))
  (let ((local (+git-store-normalize-remote-url "/tmp/some/repo.git")))
    (should (string-prefix-p "local:" local)))
  (let ((file (+git-store-normalize-remote-url "file:///tmp/some/repo.git")))
    (should (string-prefix-p "local:" file)))
  (should-not (equal (+git-store-normalize-remote-url
                      "https://github.com/org/dragon.git")
                     (+git-store-normalize-remote-url
                      "/tmp/org/dragon.git"))))

(ert-deftest git-review-phase3-relative-local-remotes-resolve-against-repo-root ()
  "Relative remotes expand against the repository root, never ambient cwd."
  (let* ((base-a "/tmp/cross-a/work")
         (base-b "/tmp/cross-b/work")
         (id-a (+git-store-normalize-remote-url "../origin.git" base-a))
         (id-b (+git-store-normalize-remote-url "../origin.git" base-b))
         (shared-near "/tmp/shared-parent/near")
         (shared-far "/tmp/shared-parent/deep/far")
         (id-near (+git-store-normalize-remote-url
                   "../shared.git" shared-near))
         (id-far (+git-store-normalize-remote-url
                  "../../shared.git" shared-far)))
    ;; Identical relative strings resolving to different abs paths stay separate.
    (should (equal id-a "local:/tmp/cross-a/origin.git"))
    (should (equal id-b "local:/tmp/cross-b/origin.git"))
    (should-not (equal id-a id-b))
    ;; Different relative strings resolving to the same absolute path share id.
    (should (equal id-near "local:/tmp/shared-parent/shared.git"))
    (should (equal id-far id-near))
    ;; Ambient default-directory must not change results when base is set.
    (let ((default-directory "/home/ango"))
      (should (equal (+git-store-normalize-remote-url "../origin.git" base-a)
                     id-a))
      (should (equal (+git-store-normalize-remote-url "../origin.git" base-b)
                     id-b)))
    (let ((default-directory "/tmp"))
      (should (equal (+git-store-normalize-remote-url "../origin.git" base-a)
                     id-a)))))

(ert-deftest git-review-phase3-relative-remotes-discovery-ignores-ambient-cwd ()
  "Discovery with identical relative remotes keeps unrelated repos distinct."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let* ((parent-a (make-temp-file "git-review cross-a " t))
            (parent-b (make-temp-file "git-review cross-b " t))
            (work-a (expand-file-name "cross-a repo" parent-a))
            (work-b (expand-file-name "cross-b repo" parent-b))
            (origin-a (expand-file-name "origin.git" parent-a))
            (origin-b (expand-file-name "origin.git" parent-b)))
       (unwind-protect
           (progn
             (git-review-fixtures--init-repo work-a)
             (git-review-fixtures--write-file work-a "a.txt" "a\n")
             (git-review-fixtures--git-ok work-a "add" "-A")
             (git-review-fixtures--git-ok work-a "commit" "-m" "a")
             (git-review-fixtures--git-ok work-a "clone" "--bare" work-a origin-a)
             (git-review-phase3--set-fetch-url work-a "../origin.git")
             (git-review-fixtures--init-repo work-b)
             (git-review-fixtures--write-file work-b "b.txt" "b\n")
             (git-review-fixtures--git-ok work-b "add" "-A")
             (git-review-fixtures--git-ok work-b "commit" "-m" "b")
             (git-review-fixtures--git-ok work-b "clone" "--bare" work-b origin-b)
             (git-review-phase3--set-fetch-url work-b "../origin.git")
             (let ((default-directory "/home/ango"))
               (let* ((ctx-a (+git-store-register-root work-a))
                      (ctx-b (+git-store-register-root work-b))
                      (id-a (+git-store-local-context-repository-id ctx-a))
                      (id-b (+git-store-local-context-repository-id ctx-b)))
                 (should (equal id-a
                                (format "local:%s"
                                        (+git-store--normalize-path origin-a))))
                 (should (equal id-b
                                (format "local:%s"
                                        (+git-store--normalize-path origin-b))))
                 (should-not (equal id-a id-b))
                 (should-not (file-equal-p origin-a origin-b)))))
         (ignore-errors (delete-directory parent-a t))
         (ignore-errors (delete-directory parent-b t)))))))

(ert-deftest git-review-phase3-remote-alias-rename-preserves-identity ()
  "Renaming a remote alias without changing its fetch URL keeps identity."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (git-review-fixtures-with-repo
      "rename-remote"
      (lambda (root)
        (git-review-fixtures--write-file root "a.txt" "a\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "base")
        (git-review-phase3--set-fetch-url
         root "git@github.com:org/dragon.git" "origin")
        (let* ((ctx1 (+git-store-register-root root))
               (id1 (+git-store-local-context-repository-id ctx1)))
          (should (equal id1 "github.com/org/dragon"))
          (git-review-fixtures--git-ok root "remote" "rename" "origin" "upstream")
          (let* ((ctx2 (+git-store-register-root root))
                 (id2 (+git-store-local-context-repository-id ctx2)))
            (should (equal id1 id2))
            (should (equal (+git-store-local-context-context-id ctx1)
                           (+git-store-local-context-context-id ctx2)))
            (should (equal (+git-store-local-context-remote-name ctx2)
                           "upstream")))))))))

;; ---------------------------------------------------------------------------
;; Independent clones and linked worktrees
;; ---------------------------------------------------------------------------

(ert-deftest git-review-phase3-same-origin-clones-share-repo-not-context ()
  "Two same-origin clones share one canonical record but stay separate locally."
  (require 'magit)
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "clone-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "shared.el" "v1\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "base")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/dragon.git" "clone-b"))
             ;; Distinct local changes.
             (git-review-fixtures--write-file root-a "shared.el" "a-only\n")
             (git-review-fixtures--write-file root-b "shared.el" "b-only\n")
             (git-review-fixtures--git-ok root-b "checkout" "-b" "feature")
             (let* ((+git-store--git-process-count 0)
                    (ctx-a (+git-store-register-root root-a))
                    (ctx-b (+git-store-register-root root-b))
                    (repo-id (+git-store-local-context-repository-id ctx-a))
                    (repo (+git-store-get-repository repo-id))
                    (wt-a (+git-review-target-for-worktree root-a))
                    (wt-b (+git-review-target-for-worktree root-b))
                    (st-a (+git-review-target-for-staged root-a))
                    (st-b (+git-review-target-for-staged root-b)))
               (should (equal repo-id "github.com/org/dragon"))
               (should (equal repo-id
                              (+git-store-local-context-repository-id ctx-b)))
               (should (eq repo (+git-store-get-repository repo-id)))
               (should (= 2 (length (+git-store-repository-local-contexts repo))))
               (should-not (equal (+git-store-local-context-context-id ctx-a)
                                  (+git-store-local-context-context-id ctx-b)))
               (should-not (equal (+git-store-local-context-git-dir ctx-a)
                                  (+git-store-local-context-git-dir ctx-b)))
               (should (equal (+git-store-local-context-current-branch ctx-a)
                              "main"))
               (should (equal (+git-store-local-context-current-branch ctx-b)
                              "feature"))
               (should-not (equal (+git-review-target-family-id wt-a)
                                  (+git-review-target-family-id wt-b)))
               (should-not (equal (+git-review-target-family-id st-a)
                                  (+git-review-target-family-id st-b)))
               (should-not (equal (+git-review-target-overview-id wt-a)
                                  (+git-review-target-overview-id wt-b)))
               (let ((default-directory (file-name-as-directory root-a)))
                 (unwind-protect
                     (git-review-baseline--with-instrumentation
                      (lambda ()
                        (let* ((oa (+git-review--open-overview wt-a))
                               (ob (progn
                                     (setq default-directory
                                           (file-name-as-directory root-b))
                                     (+git-review--open-overview wt-b))))
                          (should (not (eq oa ob)))
                          (should (buffer-live-p oa))
                          (should (buffer-live-p ob)))))
                   (git-review-baseline-cleanup-repo-buffers root-a)
                   (git-review-baseline-cleanup-repo-buffers root-b)))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

(ert-deftest git-review-phase3-linked-worktrees-share-common-dir ()
  "Linked worktrees share git-common-dir but keep separate local state."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (git-review-fixtures-with-repo
      "worktree-main"
      (lambda (root)
        (git-review-fixtures--write-file root "a.txt" "base\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "base")
        (git-review-phase3--set-fetch-url
         root "git@github.com:org/dragon.git")
        (let* ((wt-parent (make-temp-file "git-review worktree " t))
               (wt-root (expand-file-name "linked repo" wt-parent)))
          (unwind-protect
              (progn
                (git-review-fixtures--git-ok
                 root "worktree" "add" "-b" "review" wt-root)
                (git-review-fixtures--write-file root "a.txt" "main-change\n")
                (git-review-fixtures--write-file wt-root "a.txt" "review-change\n")
                (let* ((ctx-main (+git-store-register-root root))
                       (ctx-wt (+git-store-register-root wt-root)))
                  (should (equal
                           (+git-store-local-context-repository-id ctx-main)
                           (+git-store-local-context-repository-id ctx-wt)))
                  (should (equal
                           (+git-store-local-context-git-common-dir ctx-main)
                           (+git-store-local-context-git-common-dir ctx-wt)))
                  (should-not (equal
                               (+git-store-local-context-git-dir ctx-main)
                               (+git-store-local-context-git-dir ctx-wt)))
                  (should-not (equal
                               (+git-store-local-context-root ctx-main)
                               (+git-store-local-context-root ctx-wt)))
                  (should-not (equal
                               (+git-store-local-context-context-id ctx-main)
                               (+git-store-local-context-context-id ctx-wt)))
                  (should (equal
                           (+git-store-local-context-current-branch ctx-main)
                           "main"))
                  (should (equal
                           (+git-store-local-context-current-branch ctx-wt)
                           "review"))
                  (let ((wt-a (+git-review-target-for-worktree root))
                        (wt-b (+git-review-target-for-worktree wt-root)))
                    (should-not (equal (+git-review-target-family-id wt-a)
                                       (+git-review-target-family-id wt-b))))))
            (ignore-errors
              (git-review-fixtures--git-ok root "worktree" "remove" "--force"
                                           wt-root))
            (ignore-errors (delete-directory wt-parent t)))))))))

;; ---------------------------------------------------------------------------
;; Commit identity sharing and collision rules
;; ---------------------------------------------------------------------------

(ert-deftest git-review-phase3-commit-shared-across-same-origin ()
  "Same commit from two same-origin clones shares immutable identity/buffers."
  (require 'magit)
  (git-review-phase3--with-dirs
   (lambda (state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "commit-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "one\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c1")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/dragon.git" "commit-b"))
             (let* ((head (string-trim-right
                           (git-review-fixtures--git-ok
                            root-a "rev-parse" "HEAD")))
                    (ta (+git-review-target-for-commit head root-a))
                    (tb (+git-review-target-for-commit head root-b)))
               (should (equal (+git-review-target-repository-id ta)
                              (+git-review-target-repository-id tb)))
               (should (equal (+git-review-target-family-id ta)
                              (+git-review-target-family-id tb)))
               (should (equal (+git-review-target-overview-id ta)
                              (+git-review-target-overview-id tb)))
               (should-not (equal (+git-review-target-context-id ta)
                                  (+git-review-target-context-id tb)))
               (let ((default-directory (file-name-as-directory root-a)))
                 (unwind-protect
                     (git-review-baseline--with-instrumentation
                      (lambda ()
                        (let* ((oa (+git-review--open-overview ta))
                               (ob (progn
                                     (setq default-directory
                                           (file-name-as-directory root-b))
                                     (+git-review--open-overview tb))))
                          (should (eq oa ob))
                          (should (equal
                                   (buffer-local-value
                                    '+git-review-edit-context-id ob)
                                   (+git-review-target-context-id tb))))))
                   (git-review-baseline-cleanup-repo-buffers root-a)
                   (git-review-baseline-cleanup-repo-buffers root-b)))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

(ert-deftest git-review-phase3-commit-oid-different-repos-do-not-collide ()
  "Same commit OID under different canonical repos must not collide."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "oid-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "same\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c1")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             ;; Independent repo with identical content (possibly same OID)
             ;; but a different remote identity.
             (setq root-b (git-review-fixtures-make-temp-root "oid-b"))
             (git-review-fixtures--init-repo root-b)
             (git-review-fixtures--write-file root-b "f.el" "same\n")
             (git-review-fixtures--git-ok root-b "add" "-A")
             (git-review-fixtures--git-ok root-b "commit" "-m" "c1")
             (git-review-phase3--set-fetch-url
              root-b "https://gitlab.com/org/dragon.git")
             (let* ((ha (string-trim-right
                         (git-review-fixtures--git-ok root-a "rev-parse" "HEAD")))
                    (hb (string-trim-right
                         (git-review-fixtures--git-ok root-b "rev-parse" "HEAD")))
                    (ta (+git-review-target-for-commit ha root-a))
                    (tb (+git-review-target-for-commit hb root-b)))
               (should-not (equal (+git-review-target-repository-id ta)
                                  (+git-review-target-repository-id tb)))
               (should-not (equal (+git-review-target-family-id ta)
                                  (+git-review-target-family-id tb)))
               (should-not (equal (+git-review-target-overview-id ta)
                                  (+git-review-target-overview-id tb)))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

;; ---------------------------------------------------------------------------
;; Reviewed state isolation and migration
;; ---------------------------------------------------------------------------

(ert-deftest git-review-phase3-worktree-state-isolated-across-contexts ()
  "Worktree/staged reviewed state never leaks between contexts."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "state-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "base\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "base")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/dragon.git" "state-b"))
             (git-review-fixtures--write-file root-a "f.el" "a\n")
             (git-review-fixtures--write-file root-b "f.el" "b\n")
             (let* ((ta (+git-review-target-for-worktree root-a))
                    (tb (+git-review-target-for-worktree root-b))
                    (fa (car (+git-review-collect-files ta)))
                    (map (make-hash-table :test #'equal)))
               (puthash (+git-review-file-path fa)
                        (+git-review-file-fingerprint fa)
                        map)
               (+git-review-state-save ta map)
               (let ((loaded-a (+git-review-state-load ta))
                     (loaded-b (+git-review-state-load tb)))
                 (should (= 1 (hash-table-count loaded-a)))
                 (should (= 0 (hash-table-count loaded-b))))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

(ert-deftest git-review-phase3-commit-state-reusable-across-contexts ()
  "Commit reviewed state can be reused across same-origin contexts."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "cstate-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "v1\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c0")
             (git-review-fixtures--write-file root-a "f.el" "v2\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c1")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/dragon.git" "cstate-b"))
             (let* ((head (string-trim-right
                           (git-review-fixtures--git-ok
                            root-a "rev-parse" "HEAD")))
                    (ta (+git-review-target-for-commit head root-a))
                    (tb (+git-review-target-for-commit head root-b))
                    (files (+git-review-collect-files ta))
                    (file (car files))
                    (map (make-hash-table :test #'equal)))
               (should (equal (+git-review-target-family-id ta)
                              (+git-review-target-family-id tb)))
               (puthash (+git-review-file-path file)
                        (+git-review-file-fingerprint file)
                        map)
               (+git-review-state-save ta map)
               (let ((loaded (+git-review-state-load tb)))
                 (should (+git-review--file-reviewed-p file loaded)))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

(ert-deftest git-review-phase3-legacy-state-migrates-once ()
  "Legacy Phase 2 state migrates once without deleting the legacy file."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (git-review-fixtures-with-repo
      "legacy"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (let* ((target (+git-review-target-for-worktree root))
               (legacy-family
                (+git-review--legacy-family-id
                 root 'worktree "HEAD" nil
                 (+git-review-target-base-oid target)))
               (legacy-file
                (+git-review--state-file-for-family legacy-family))
               (new-file (+git-review--state-file target))
               (files (+git-review-collect-files target))
               (file (cl-find "README.md" files
                              :key #'+git-review-file-path :test #'equal))
               (map (make-hash-table :test #'equal)))
          (should file)
          (should-not (equal legacy-family
                             (+git-review-target-family-id target)))
          ;; Write a Phase 2 legacy file directly.
          (+git-review--ensure-state-dir)
          (puthash "README.md" (+git-review-file-fingerprint file) map)
          (with-temp-file legacy-file
            (prin1 (list (cons 'version 1)
                         (cons 'family-id legacy-family)
                         (cons 'entries
                               (list (cons "README.md"
                                           (+git-review-file-fingerprint file)))))
                   (current-buffer)))
          (should (file-exists-p legacy-file))
          (should-not (file-exists-p new-file))
          (let ((loaded (+git-review-state-load target)))
            (should (+git-review--file-reviewed-p file loaded))
            (should (file-exists-p new-file))
            (should (file-exists-p legacy-file)))
          ;; Second load uses the new file; legacy remains.
          (let ((loaded2 (+git-review-state-load target)))
            (should (+git-review--file-reviewed-p file loaded2))
            (should (file-exists-p legacy-file)))))))))

;; ---------------------------------------------------------------------------
;; Registry persistence
;; ---------------------------------------------------------------------------

(ert-deftest git-review-phase3-registry-atomic-and-malformed ()
  "Registry save is atomic; malformed persistence yields an empty registry."
  (git-review-phase3--with-dirs
   (lambda (_state reg)
     (git-review-fixtures-with-repo
      "registry"
      (lambda (root)
        (git-review-fixtures--write-file root "a.txt" "a\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "base")
        (git-review-phase3--set-fetch-url
         root "https://github.com/org/dragon.git")
        (let* ((ctx (+git-store-register-root root))
               (file (+git-store--registry-file))
               (gen (+git-store-repository-cache-generation
                     (+git-store-get-repository
                      (+git-store-local-context-repository-id ctx)))))
          (should (file-exists-p file))
          (should (>= gen 1))
          ;; Simulate rename failure: leave a valid prior generation.
          (let ((prior (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))))
            (cl-letf (((symbol-function 'rename-file)
                       (lambda (&rest _)
                         (error "simulated rename failure"))))
              (setq +git-store--dirty t)
              (should-error (+git-store-save-registry) :type 'error))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) prior))))
          ;; Malformed JSON -> empty usable registry, never eval Lisp.
          (with-temp-file file
            (insert "{not valid json (lambda () (error \"EVALED\"))}\n")
            (insert ";; (+ 1 2)\n"))
          (+git-store-load-registry)
          (should (= 0 (hash-table-count +git-store--repositories)))
          ;; Lisp forms must not have been evaluated.
          (should (null (get 'git-review-phase3-eval-probe 'evaled)))))))))

;; ---------------------------------------------------------------------------
;; Context deletion and L / e behavior
;; ---------------------------------------------------------------------------

(ert-deftest git-review-phase3-deleted-context-fallback-and-L ()
  "Deleted contexts degrade; L retargets shareable commits only."
  (require 'magit)
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "del-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "x\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c1")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/dragon.git" "del-b"))
             (let* ((head (string-trim-right
                           (git-review-fixtures--git-ok
                            root-a "rev-parse" "HEAD")))
                    (ctx-a (+git-store-register-root root-a))
                    (ctx-b (+git-store-register-root root-b))
                    (ta (+git-review-target-for-commit head root-a))
                    (wt (+git-review-target-for-worktree root-a)))
               ;; L rejects worktree retargeting.
               (let ((default-directory (file-name-as-directory root-a)))
                 (unwind-protect
                     (git-review-baseline--with-instrumentation
                      (lambda ()
                        (let ((ov (+git-review--open-overview ta)))
                          (with-current-buffer ov
                            (should-error
                             (+git-review-set-edit-context
                              (+git-store-local-context-context-id ctx-b)
                              wt)
                             :type 'user-error)
                            ;; L succeeds on shareable commit.
                            (+git-review-set-edit-context
                             (+git-store-local-context-context-id ctx-b)
                             ta)
                            (should (equal +git-review-edit-context-id
                                           (+git-store-local-context-context-id
                                            ctx-b)))
                            ;; e opens the chosen context file.
                            (setq +git-review-file-path "f.el")
                            (let ((before (current-buffer)))
                              (+git-review-visit-worktree)
                              (should (file-equal-p
                                       (expand-file-name "f.el" root-b)
                                       buffer-file-name))
                              (+git-review-quit)
                              (should (eq (current-buffer) before))))
                          ;; Delete clone A; B remains selectable and refreshable.
                          (git-review-phase3--cleanup-root root-a)
                          (setq root-a nil)
                          (+git-store-refresh-context-availability
                           (+git-store-local-context-repository-id ctx-a))
                          (with-current-buffer ov
                            (let ((root (+git-review--active-edit-root ta)))
                              (should (file-equal-p root root-b)))
                            ;; gr must work from the surviving clone.
                            (+git-review-refresh)
                            (should (file-directory-p default-directory))
                            (should (file-equal-p default-directory root-b))))))
                   (git-review-baseline-cleanup-repo-buffers root-b)
                   (when root-a
                     (git-review-baseline-cleanup-repo-buffers root-a))))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

(ert-deftest git-review-phase3-offline-public-ops ()
  "Public register/open/tree/diff/refresh/L stay under the network guard."
  (require 'magit)
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (git-review-fixtures-with-repo
      "offline3"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (git-review-phase3--set-fetch-url
         root "https://github.com/org/dragon.git")
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (git-review-baseline--with-instrumentation
               (lambda ()
                 (let* ((ctx (+git-store-register-root root))
                        (head (string-trim-right
                               (git-review-fixtures--git-ok
                                root "rev-parse" "HEAD")))
                        (wt (+git-review-target-for-worktree root))
                        (cm (+git-review-target-for-commit head root))
                        (ov (+git-review--open-overview wt))
                        (tree (+git-changes-tree-setup-buffer wt))
                        (file (car (with-current-buffer tree
                                     +git-review--files)))
                        (diff (+git-review-open-file-diff wt file))
                        (cov (+git-review--open-overview cm)))
                   (should ctx)
                   (should (buffer-live-p ov))
                   (should (buffer-live-p tree))
                   (should (buffer-live-p diff))
                   (with-current-buffer tree
                     (+git-review-refresh)
                     (should-error
                      (+git-review-select-edit-context
                       (+git-review-target-context-id wt))
                      :type 'user-error))
                   (with-current-buffer cov
                     (+git-review-select-edit-context
                      (+git-review-target-context-id cm)))
                   (should (null git-review-baseline--blocked-attempts)))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase3-register-caches-identity ()
  "Repeated registration reuses the record without bumping on no-op."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (git-review-fixtures-with-repo
      "cache"
      (lambda (root)
        (git-review-fixtures--write-file root "a.txt" "a\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "base")
        (git-review-phase3--set-fetch-url
         root "https://github.com/org/dragon.git")
        (let* ((+git-store--git-process-count 0)
               (ctx1 (+git-store-register-root root))
               (procs1 +git-store--git-process-count)
               (gen1 (+git-store-repository-cache-generation
                      (+git-store-get-repository
                       (+git-store-local-context-repository-id ctx1))))
               (+git-store--git-process-count 0)
               (ctx2 (+git-store-register-root root))
               (procs2 +git-store--git-process-count)
               (gen2 (+git-store-repository-cache-generation
                      (+git-store-get-repository
                       (+git-store-local-context-repository-id ctx2)))))
          (should (eq ctx1 ctx2))
          (should (equal gen1 gen2))
          ;; Refresh discovery still runs Git, but generation stays put.
          (should (> procs1 0))
          (should (> procs2 0))
          (message "phase3 register procs: first=%d refresh=%d gen=%d"
                   procs1 procs2 gen1)))))))

;; ---------------------------------------------------------------------------
;; P1 regression coverage
;; ---------------------------------------------------------------------------

(ert-deftest git-review-phase3-reopen-file-diff-adopts-edit-context ()
  "Reopening a shared commit file diff from clone B makes e open B."
  (require 'magit)
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "reuse-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "shared\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c1")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/dragon.git" "reuse-b"))
             ;; Distinct worktree content so a wrong-clone visit is obvious.
             (git-review-fixtures--write-file root-a "f.el" "from-a\n")
             (git-review-fixtures--write-file root-b "f.el" "from-b\n")
             (let* ((head (string-trim-right
                           (git-review-fixtures--git-ok
                            root-a "rev-parse" "HEAD")))
                    (ta (+git-review-target-for-commit head root-a))
                    (tb (+git-review-target-for-commit head root-b))
                    (files (+git-review-collect-files ta))
                    (file (cl-find "f.el" files
                                   :key #'+git-review-file-path
                                   :test #'equal)))
               (should file)
               (let ((default-directory (file-name-as-directory root-a)))
                 (unwind-protect
                     (git-review-baseline--with-instrumentation
                      (lambda ()
                        (let* ((diff-a (+git-review-open-file-diff ta file))
                               (diff-b (+git-review-open-file-diff tb file)))
                          (should (eq diff-a diff-b))
                          (with-current-buffer diff-b
                            (should (equal
                                     +git-review-edit-context-id
                                     (+git-review-target-context-id tb)))
                            (should (file-equal-p default-directory root-b))
                            (let ((before (current-buffer)))
                              (+git-review-visit-worktree)
                              (should (file-equal-p
                                       (expand-file-name "f.el" root-b)
                                       buffer-file-name))
                              (should (equal (string-trim
                                              (buffer-substring-no-properties
                                               (point-min) (point-max)))
                                             "from-b"))
                              (+git-review-quit)
                              (should (eq (current-buffer) before)))))))
                   (git-review-baseline-cleanup-repo-buffers root-a)
                   (git-review-baseline-cleanup-repo-buffers root-b)))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

(ert-deftest git-review-phase3-deleted-origin-refresh-uses-survivor ()
  "After deleting the originating clone, gr works on overview/tree/file-diff."
  (require 'magit)
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "gr-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "v1\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c0")
             (git-review-fixtures--write-file root-a "f.el" "v2\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c1")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/dragon.git" "gr-b"))
             (let* ((head (string-trim-right
                           (git-review-fixtures--git-ok
                            root-a "rev-parse" "HEAD")))
                    (ta (+git-review-target-for-commit head root-a))
                    (_ctx-b (+git-store-register-root root-b))
                    (files (+git-review-collect-files ta))
                    (file (cl-find "f.el" files
                                   :key #'+git-review-file-path
                                   :test #'equal)))
               (should file)
               (let ((default-directory (file-name-as-directory root-a)))
                 (unwind-protect
                     (git-review-baseline--with-instrumentation
                      (lambda ()
                        (let* ((ov (+git-review--open-overview ta))
                               (tree (+git-changes-tree-setup-buffer ta))
                               (diff (+git-review-open-file-diff ta file)))
                          ;; Switch edit context to B before deleting A.
                          (dolist (buf (list ov tree diff))
                            (with-current-buffer buf
                              (+git-review-set-edit-context
                               (+git-review-target-context-id
                                (+git-review-target-for-commit head root-b))
                               ta)))
                          (git-review-phase3--cleanup-root root-a)
                          (setq root-a nil)
                          (+git-store-refresh-context-availability
                           (+git-review-target-repository-id ta))
                          (with-current-buffer ov
                            (+git-review-refresh)
                            (should (file-equal-p default-directory root-b)))
                          (with-current-buffer tree
                            (+git-review-refresh)
                            (should (file-equal-p default-directory root-b))
                            (should +git-review--files))
                          (with-current-buffer diff
                            (+git-review-refresh)
                            (should (file-equal-p default-directory root-b))))))
                   (git-review-baseline-cleanup-repo-buffers root-b)
                   (when root-a
                     (git-review-baseline-cleanup-repo-buffers root-a))))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

(ert-deftest git-review-phase3-registry-roundtrip-across-session ()
  "Valid registry survives save -> reset memory -> lazy load."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (git-review-fixtures-with-repo
      "roundtrip"
      (lambda (root)
        (git-review-fixtures--write-file root "a.txt" "a\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "base")
        (git-review-phase3--set-fetch-url
         root "https://github.com/org/dragon.git")
        (let* ((ctx (+git-store-register-root root))
               (repo-id (+git-store-local-context-repository-id ctx))
               (ctx-id (+git-store-local-context-context-id ctx))
               (file (+git-store--registry-file)))
          (should (file-exists-p file))
          (should (equal repo-id "github.com/org/dragon"))
          ;; Simulate Emacs restart: clear memory, allow reload from disk.
          (+git-store-simulate-new-session)
          (should (= 0 (hash-table-count +git-store--repositories)))
          (should (null +git-store--registry-loaded))
          ;; Public access must lazy-load.
          (let ((loaded (+git-store-get-context ctx-id)))
            (should loaded)
            (should +git-store--registry-loaded)
            (should (equal (+git-store-local-context-repository-id loaded)
                           repo-id))
            (should (equal (+git-store-local-context-root loaded)
                           (+git-store--normalize-path root)))
            (should (+git-store-get-repository repo-id)))))))))

(ert-deftest git-review-phase3-ui-open-refreshes-changed-remote ()
  "Reopening a review rediscovers a changed remote/branch/HEAD/upstream."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (git-review-fixtures-with-repo
      "refresh-ui"
      (lambda (root)
        (git-review-fixtures--write-file root "a.txt" "a\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "base")
        (git-review-phase3--set-fetch-url
         root "https://github.com/org/dragon.git")
        (let* ((ctx1 (+git-store-context-for-root root))
               (id1 (+git-store-local-context-repository-id ctx1))
               (gen1 (+git-store-repository-cache-generation
                      (+git-store-get-repository id1)))
               (head1 (+git-store-local-context-head ctx1)))
          (should (equal id1 "github.com/org/dragon"))
          ;; Change remote URL to another hosted identity.
          (git-review-phase3--set-fetch-url
           root "https://gitlab.com/org/dragon.git")
          ;; Advance HEAD on a new branch with upstream metadata absent.
          (git-review-fixtures--git-ok root "checkout" "-b" "feature")
          (git-review-fixtures--write-file root "a.txt" "b\n")
          (git-review-fixtures--git-ok root "add" "-A")
          (git-review-fixtures--git-ok root "commit" "-m" "next")
          ;; UI entry point refreshes via context-for-root.
          (let* ((wt (+git-review-target-for-worktree root))
                 (ctx2 (+git-store-get-context
                        (+git-review-target-context-id wt)))
                 (id2 (+git-store-local-context-repository-id ctx2))
                 (gen2 (+git-store-repository-cache-generation
                        (+git-store-get-repository id2))))
            (should (equal id2 "gitlab.com/org/dragon"))
            (should-not (equal id1 id2))
            (should (equal (+git-store-local-context-current-branch ctx2)
                           "feature"))
            (should-not (equal head1 (+git-store-local-context-head ctx2)))
            (should (> gen2 0))
            ;; No-op rediscovery preserves generation.
            (let* ((ctx3 (+git-store-context-for-root root))
                   (gen3 (+git-store-repository-cache-generation
                          (+git-store-get-repository
                           (+git-store-local-context-repository-id ctx3)))))
              (should (equal gen2 gen3))
              (should (eq ctx2 ctx3))))))))))

(ert-deftest git-review-phase3-staged-and-branch-state-isolated ()
  "Staged and branch reviewed state never leak across same-origin contexts."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "iso-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "base\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "base")
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/dragon.git")
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/dragon.git" "iso-b"))
             ;; Staged change only in A.
             (git-review-fixtures--write-file root-a "f.el" "staged-a\n")
             (git-review-fixtures--git-ok root-a "add" "--" "f.el")
             ;; Branch tip only in B.
             (git-review-fixtures--git-ok root-b "checkout" "-b" "feature")
             (git-review-fixtures--write-file root-b "f.el" "branch-b\n")
             (git-review-fixtures--git-ok root-b "add" "-A")
             (git-review-fixtures--git-ok root-b "commit" "-m" "feature")
             (let* ((st-a (+git-review-target-for-staged root-a))
                    (st-b (+git-review-target-for-staged root-b))
                    (br-a (+git-review-target-for-branch "main" "main" root-a))
                    (br-b (+git-review-target-for-branch "main" "feature" root-b))
                    (fa (car (+git-review-collect-files st-a)))
                    (map (make-hash-table :test #'equal)))
               (should-not (equal (+git-review-target-family-id st-a)
                                  (+git-review-target-family-id st-b)))
               (should-not (equal (+git-review-target-family-id br-a)
                                  (+git-review-target-family-id br-b)))
               (when fa
                 (puthash (+git-review-file-path fa)
                          (+git-review-file-fingerprint fa)
                          map)
                 (+git-review-state-save st-a map)
                 (should (= 1 (hash-table-count
                               (+git-review-state-load st-a))))
                 (should (= 0 (hash-table-count
                               (+git-review-state-load st-b)))))
               (let* ((files-b (+git-review-collect-files br-b))
                      (fb (cl-find "f.el" files-b
                                   :key #'+git-review-file-path
                                   :test #'equal))
                      (bmap (make-hash-table :test #'equal)))
                 (should fb)
                 (puthash "f.el" (+git-review-file-fingerprint fb) bmap)
                 (+git-review-state-save br-b bmap)
                 (should (= 1 (hash-table-count
                               (+git-review-state-load br-b))))
                 (should (= 0 (hash-table-count
                               (+git-review-state-load br-a)))))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

;;; ---------------------------------------------------------------------------
;;; Cross-repository eligibility (same OID, different canonical repos)
;;; ---------------------------------------------------------------------------

(ert-deftest git-review-phase3-cross-repo-same-oid-rejects-foreign-context ()
  "Repo-B targets must never select a repo-A context, even with the same OID."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (let (root-a root-b)
       (unwind-protect
           (progn
             (setq root-a (git-review-fixtures-make-temp-root "cross-a"))
             (git-review-fixtures--init-repo root-a)
             (git-review-fixtures--write-file root-a "f.el" "same\n")
             (git-review-fixtures--git-ok root-a "add" "-A")
             (git-review-fixtures--git-ok root-a "commit" "-m" "c1")
             ;; Clone so both trees contain the identical commit OID, then
             ;; re-home remotes to distinct canonical repositories (forks).
             (setq root-b
                   (git-review-phase3--clone-same-origin
                    root-a "https://github.com/org/two.git" "cross-b"))
             (git-review-phase3--set-fetch-url
              root-a "https://github.com/org/one.git")
             (git-review-phase3--set-fetch-url
              root-b "https://github.com/org/two.git")
             (let* ((head (string-trim-right
                           (git-review-fixtures--git-ok
                            root-a "rev-parse" "HEAD")))
                    (ctx-a (+git-store-register-root root-a))
                    (ctx-b (+git-store-register-root root-b))
                    (repo-a (+git-store-local-context-repository-id ctx-a))
                    (repo-b (+git-store-local-context-repository-id ctx-b))
                    (ta (+git-review-target-for-commit head root-a))
                    (tb (+git-review-target-for-commit head root-b))
                    (oids (+git-review--target-required-oids tb)))
               (should (equal head
                              (string-trim-right
                               (git-review-fixtures--git-ok
                                root-b "rev-parse" "HEAD"))))
               (should (equal repo-a "github.com/org/one"))
               (should (equal repo-b "github.com/org/two"))
               (should-not (equal repo-a repo-b))
               (should (+git-store-context-eligible-p ctx-a repo-a oids))
               (should (+git-store-context-eligible-p ctx-b repo-b oids))
               (should-not (+git-store-context-eligible-p ctx-a repo-b oids))
               (should-not (+git-store-context-eligible-p ctx-b repo-a oids))
               ;; While current in a repo-A review buffer, resolving the
               ;; operational root for a repo-B target must select B.
               (with-temp-buffer
                 (setq-local +git-review-target ta)
                 (setq-local +git-review-edit-context-id
                             (+git-store-local-context-context-id ctx-a))
                 (let ((selected (+git-review--git-root-for-target tb)))
                   (should (file-equal-p selected root-b))
                   (should-not (file-equal-p selected root-a))))
               ;; Reopening adoption for B must not accept A's context.
               (with-temp-buffer
                 (setq-local +git-review-target tb)
                 (setq-local +git-review-edit-context-id
                             (+git-store-local-context-context-id ctx-a))
                 (+git-review--adopt-reopening-context tb)
                 (should (equal +git-review-edit-context-id
                                (+git-store-local-context-context-id ctx-b)))
                 (should (file-equal-p default-directory root-b)))
               ;; L / set-edit-context rejects cross-repo selection.
               (with-temp-buffer
                 (setq-local +git-review-target tb)
                 (should-error
                  (+git-review-set-edit-context
                   (+git-store-local-context-context-id ctx-a) tb)
                  :type 'user-error))))
         (git-review-phase3--cleanup-root root-a)
         (git-review-phase3--cleanup-root root-b))))))

(ert-deftest git-review-phase3-rehomed-context-not-used-by-old-repo-target ()
  "After a remote re-home, an old repo-A target must not use the now-repo-B context."
  (git-review-phase3--with-dirs
   (lambda (_state _reg)
     (git-review-fixtures-with-repo
      "rehome"
      (lambda (root)
        (git-review-fixtures--write-file root "f.el" "v1\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "c1")
        (git-review-phase3--set-fetch-url
         root "https://github.com/org/one.git")
        (let* ((head (string-trim-right
                      (git-review-fixtures--git-ok root "rev-parse" "HEAD")))
               (ctx1 (+git-store-register-root root))
               (ta (+git-review-target-for-commit head root))
               (ctx-id (+git-store-local-context-context-id ctx1))
               (oids (+git-review--target-required-oids ta)))
          (should (equal (+git-review-target-repository-id ta)
                         "github.com/org/one"))
          (should (equal (+git-store-local-context-repository-id ctx1)
                         "github.com/org/one"))
          ;; Re-home the checkout's remote to a different canonical repo.
          (git-review-phase3--set-fetch-url
           root "https://github.com/org/two.git")
          (let ((ctx2 (+git-store-register-root root)))
            (should (equal (+git-store-local-context-context-id ctx2) ctx-id))
            (should (equal (+git-store-local-context-repository-id ctx2)
                           "github.com/org/two"))
            ;; Immutable target still names the old repository.
            (should (equal (+git-review-target-repository-id ta)
                           "github.com/org/one"))
            (should-not (+git-store-context-eligible-p
                         ctx2
                         (+git-review-target-repository-id ta)
                         oids))
            (with-temp-buffer
              (setq-local +git-review-target ta)
              (setq-local +git-review-edit-context-id ctx-id)
              (should-error (+git-review--git-root-for-target ta)
                            :type 'user-error))
            (with-temp-buffer
              (setq-local +git-review-target ta)
              (should-error (+git-review--adopt-reopening-context ta)
                            :type 'user-error)))))))))

(provide 'git-review-integration-test)

;;; git-review-integration-test.el ends here
