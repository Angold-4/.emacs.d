;;; git-review-phase2.el --- Phase 2 local review and Changes Tree tests -*- lexical-binding: t -*-

;;; Commentary:
;; Offline ERT coverage for Phase 2 review targets, machine-readable
;; file models, Changes Tree, persistence, per-file diffs, immutable
;; guards, and bounded-process 100-file performance.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-review-fixtures)
(require 'git-review-baseline)

(defun git-review-phase2--with-state-dir (fn)
  "Call FN with review-state and registry dirs bound to temp directories."
  (let* ((dir (make-temp-file "git-review-state " t))
         (reg (make-temp-file "git-review-registry " t))
         (+git-review-state-directory dir)
         (+git-store-registry-directory reg))
    (unwind-protect
        (progn
          (+git-store-reset-registry)
          (funcall fn dir))
      (ignore-errors (delete-directory dir t))
      (ignore-errors (delete-directory reg t))
      (ignore-errors (+git-store-reset-registry)))))

(defun git-review-phase2--file-by-path (files path)
  "Return the `+git-review-file' in FILES whose path equals PATH."
  (cl-find path files :key #'+git-review-file-path :test #'equal))

(defun git-review-phase2--goto-tree-file (path)
  "Move point to the Changes Tree file section for PATH."
  (goto-char (point-min))
  (unless (re-search-forward (regexp-quote (file-name-nondirectory path)) nil t)
    (error "Tree file %s not found" path))
  (beginning-of-line)
  (should (magit-section-match 'file (magit-current-section))))

(defun git-review-phase2--diff-file-buffers ()
  "Return live Magit file-diff buffers with `+git-review-file-path' set."
  (cl-remove-if-not
   (lambda (buf)
     (with-current-buffer buf
       (and (derived-mode-p 'magit-diff-mode)
            +git-review-file-path)))
   (buffer-list)))

(ert-deftest git-review-phase2-review-targets-and-identities ()
  "Worktree/staged/commit/branch targets expose stable identities and ranges."
  (git-review-fixtures-with-repo
   "targets"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let* ((wt (+git-review-target-for-worktree root))
            (st (+git-review-target-for-staged root))
            (head (string-trim-right
                   (git-review-fixtures--git-ok root "rev-parse" "HEAD")))
            (cm (+git-review-target-for-commit head root))
            ;; Create a second commit/branch for merge-base review.
            (_ (progn
                 (git-review-fixtures--write-file root "branch.txt" "b\n")
                 (git-review-fixtures--git-ok root "add" "-A")
                 (git-review-fixtures--git-ok root "commit" "-m" "second")
                 (git-review-fixtures--git-ok root "branch" "feature")
                 (git-review-fixtures--git-ok root "reset" "--hard" "HEAD~1")))
            (br (+git-review-target-for-branch "main" "feature" root)))
       (should (eq (+git-review-target-scope wt) 'worktree))
       (should (eq (+git-review-target-mutable-p wt) t))
       (should (string-prefix-p "git-review:worktree:" (+git-review-target-family-id wt)))
       (should (equal (+git-review-target-family-id wt)
                      (+git-review-target-family-id
                       (+git-review-target-for-worktree root))))
       (should (stringp (+git-review-target-repository-id wt)))
       (should (stringp (+git-review-target-context-id wt)))
       (should (eq (+git-review-target-scope st) 'staged))
       (should (eq (+git-review-target-mutable-p st) t))
       (should (eq (+git-review-target-scope cm) 'commit))
       (should (null (+git-review-target-mutable-p cm)))
       (should (equal (+git-review-target-head-oid cm) head))
       (should (eq (+git-review-target-scope br) 'branch))
       (should (null (+git-review-target-mutable-p br)))
       (should (string-match-p
                (format "git-review:branch:%s:%s:main:feature\\'"
                        (regexp-quote (+git-review-target-repository-id br))
                        (regexp-quote (+git-review-target-context-id br)))
                (+git-review-target-family-id br)))
       (should (string-match-p
                (format "git-review:branch:%s:%s:%s:%s:overview\\'"
                        (regexp-quote (+git-review-target-repository-id br))
                        (regexp-quote (+git-review-target-context-id br))
                        (regexp-quote (+git-review-target-base-oid br))
                        (regexp-quote (+git-review-target-head-oid br)))
                (+git-review-target-overview-id br)))
       (should-not (equal (+git-review-target-family-id br)
                          (+git-review-target-overview-id br)))
       (pcase-let ((`(,range ,typearg) (+git-review-target-range-args br)))
         (should (null typearg))
         (should (string-match-p "\\.\\." range)))
       ;; Root commit uses empty tree.
       (let* ((root-oid
               (string-trim-right
                (git-review-fixtures--git-ok root "rev-list" "--max-parents=0" "HEAD")))
              (root-target (+git-review-target-for-commit root-oid root)))
         (should (equal (+git-review-target-base-oid root-target)
                        +git-review-empty-tree)))))))

(ert-deftest git-review-phase2-buffer-reuse ()
  "Reopening overview/tree/file diff reuses the same buffer.
After a commit, the reused worktree overview and file-diff update Magit
range state to the new HEAD instead of keeping the old object."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "reuse"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (git-review-baseline--with-instrumentation
               (lambda ()
                 (let* ((target1 (+git-review-target-for-worktree root))
                        (old-head (+git-review-target-base-oid target1))
                        (o1 (+git-review--open-overview target1))
                        (t1 (+git-changes-tree-setup-buffer target1))
                        (file (git-review-phase2--file-by-path
                               (with-current-buffer t1 +git-review--files)
                               "README.md"))
                        (f1 (+git-review-open-file-diff target1 file)))
                   (should (equal (buffer-local-value 'magit-buffer-range o1)
                                  old-head))
                   ;; Commit the staged/worktree mix so HEAD advances.
                   (git-review-fixtures--git-ok root "add" "-A")
                   (git-review-fixtures--git-ok root "commit" "-m" "advance")
                   (git-review-fixtures--write-file
                    root "README.md" "# after commit\nchanged-again\n")
                   (let* ((target2 (+git-review-target-for-worktree root))
                          (new-head (+git-review-target-base-oid target2))
                          (o2 (+git-review--open-overview target2))
                          (files2 (+git-review-collect-files target2))
                          (file2 (git-review-phase2--file-by-path
                                  files2 "README.md"))
                          (f2 (+git-review-open-file-diff target2 file2)))
                     (should (not (equal old-head new-head)))
                     (should (eq o1 o2))
                     (should (eq f1 f2))
                     (should (equal (buffer-local-value 'magit-buffer-range o2)
                                    new-head))
                     (should (equal (buffer-local-value 'magit-buffer-range f2)
                                    new-head))
                     (should (equal
                              (+git-review-target-base-oid
                               (buffer-local-value '+git-review-target o2))
                              new-head))))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-collect-files-cases ()
  "NUL-safe collection covers nested/space/unicode/binary/copy/submodule."
  (git-review-fixtures-with-repo
   "collect"
   (lambda (root)
     (git-review-fixtures-create-phase2 root)
     (let* ((+git-review--git-process-count 0)
            (target (+git-review-target-for-worktree root))
            (files (+git-review-collect-files target))
            (paths (mapcar #'+git-review-file-path files)))
       (should (< +git-review--git-process-count
                  +git-review--max-git-processes-per-refresh))
       (should (git-review-phase2--file-by-path files "src/nested/app.el"))
       (should (git-review-phase2--file-by-path files "docs/review test.el"))
       (should (git-review-phase2--file-by-path files "docs/ユニコード.el"))
       (should (git-review-phase2--file-by-path files "new file.txt"))
       (let ((bin (git-review-phase2--file-by-path files "assets/logo.bin")))
         (should bin)
         (should (eq (+git-review-file-status bin) 'binary))
         (should (eq (+git-review-file-additions bin) 'binary)))
       (should (cl-some (lambda (f)
                          (eq (+git-review-file-status f) 'untracked))
                        files))
       (let ((copy (git-review-phase2--file-by-path files "copy-dest.el")))
         (should copy)
         (should (eq (+git-review-file-status copy) 'copied))
         (should (equal (+git-review-file-old-path copy) "copy-source.el"))
         (should (numberp (+git-review-file-additions copy)))
         (should (numberp (+git-review-file-deletions copy))))
       ;; Empty directory is omitted.
       (should-not (cl-find "empty-dir" paths :test #'string-prefix-p))
       ;; Submodule/gitlink when present.
       (let ((sub (git-review-phase2--file-by-path files "vendor/lib")))
         (should sub)
         (should (eq (+git-review-file-status sub) 'submodule)))))))

(ert-deftest git-review-phase2-tree-rollups-and-collapse ()
  "Tree builds nested folders with aggregate counts and review rollups.
Directory sections store complete relative paths so nested SPC works,
including repeated basenames under different parents."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "rollups"
      (lambda (root)
        (git-review-fixtures-create-phase2 root)
        ;; Repeated folder basename under different parents (uncommitted).
        (git-review-fixtures--write-file root "a/api/one.el" "one-changed\n")
        (git-review-fixtures--write-file root "b/api/two.el" "two-changed\n")
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (let* ((target (+git-review-target-for-worktree root))
                     (buf (+git-changes-tree-setup-buffer target)))
                (with-current-buffer buf
                  (should (derived-mode-p '+git-changes-tree-mode))
                  (should (string-match-p "src/" (buffer-string)))
                  ;; SPC on top-level src/ uses full path value.
                  (goto-char (point-min))
                  (re-search-forward "src/")
                  (beginning-of-line)
                  (let ((sec (magit-current-section)))
                    (should (magit-section-match 'directory sec))
                    (should (equal (oref sec value) "src"))
                    (+git-changes-tree-toggle-reviewed)
                    (should (>= (hash-table-count +git-review--reviewed-map) 1)))
                  ;; Expand and SPC nested directory by complete path.
                  (magit-section-show (magit-current-section))
                  (re-search-forward "nested/")
                  (beginning-of-line)
                  (let ((nsec (magit-current-section)))
                    (should (equal (oref nsec value) "src/nested"))
                    (+git-changes-tree-toggle-reviewed))
                  ;; Repeated basename api/ under a/ and b/.
                  (goto-char (point-min))
                  (re-search-forward "a/")
                  (beginning-of-line)
                  (magit-section-show (magit-current-section))
                  (re-search-forward "api/")
                  (beginning-of-line)
                  (should (equal (oref (magit-current-section) value) "a/api"))
                  (goto-char (point-min))
                  (re-search-forward "b/")
                  (beginning-of-line)
                  (magit-section-show (magit-current-section))
                  (re-search-forward "api/")
                  (beginning-of-line)
                  (should (equal (oref (magit-current-section) value) "b/api"))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-reviewed-toggle-persistence ()
  "SPC toggles persistence without mutating Git status or files."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (state-dir)
     (git-review-fixtures-with-repo
      "toggle"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (let ((default-directory (file-name-as-directory root))
              (before (git-review-fixtures-porcelain root "-uall")))
          (unwind-protect
              (let* ((target (+git-review-target-for-worktree root))
                     (buf (+git-changes-tree-setup-buffer target)))
                (with-current-buffer buf
                  (git-review-phase2--goto-tree-file "README.md")
                  (+git-changes-tree-toggle-reviewed)
                  (let ((file (git-review-phase2--file-by-path
                               +git-review--files "README.md")))
                    (should (+git-review--file-reviewed-p
                             file +git-review--reviewed-map)))
                  (should (file-exists-p (+git-review--state-file target)))
                  ;; Toggle again clears.
                  (+git-changes-tree-toggle-reviewed)
                  (let ((file (git-review-phase2--file-by-path
                               +git-review--files "README.md")))
                    (should-not (+git-review--file-reviewed-p
                                 file +git-review--reviewed-map))))
                (should (equal before
                               (git-review-fixtures-porcelain root "-uall")))
                (should (file-directory-p state-dir)))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-atomic-and-malformed-state ()
  "Atomic writes preserve prior state on failed rename; malformed falls back."
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "state"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (let* ((target (+git-review-target-for-worktree root))
               (map (make-hash-table :test #'equal))
               (file (+git-review--state-file target)))
          (puthash "README.md" "fp-one" map)
          (+git-review-state-save target map)
          (should (file-readable-p file))
          (let ((prev (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))))
            ;; Failed final rename must leave the previous generation intact.
            (cl-letf (((symbol-function 'rename-file)
                       (let ((orig (symbol-function 'rename-file))
                             (final (expand-file-name file)))
                         (lambda (from to &optional ok-if-already-exists)
                           (if (equal (expand-file-name to) final)
                               (error "simulated atomic rename failure")
                             (funcall orig from to ok-if-already-exists))))))
              (let ((map2 (make-hash-table :test #'equal)))
                (puthash "README.md" "fp-two" map2)
                (should-error (+git-review-state-save target map2)
                              :type 'error)))
            (should (equal (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))
                           prev))
            (should (equal (gethash "README.md"
                                    (+git-review-state-load target))
                           "fp-one"))
            ;; Malformed content degrades to all-unreviewed.
            (with-temp-file file
              (insert "not-a-valid-lisp-state"))
            (should (= 0 (hash-table-count
                          (+git-review-state-load target)))))))))))

(ert-deftest git-review-phase2-fingerprint-invalidation ()
  "Changing one reviewed file invalidates only that file's checkmark."
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "fingerprint"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (let* ((target (+git-review-target-for-worktree root))
               (files1 (+git-review-collect-files target))
               (readme (git-review-phase2--file-by-path files1 "README.md"))
               (util (git-review-phase2--file-by-path files1 "lib/util.el"))
               (map (make-hash-table :test #'equal)))
          (puthash "README.md" (+git-review-file-fingerprint readme) map)
          (puthash "lib/util.el" (+git-review-file-fingerprint util) map)
          (+git-review-state-save target map)
          ;; Modify only README.
          (git-review-fixtures--write-file
           root "README.md" "# fixture\nline-one\nline-two-modified\nextra\n")
          (let* ((files2 (+git-review-collect-files target))
                 (synced (+git-review--sync-reviewed-map
                          files2 (+git-review-state-load target)))
                 (readme2 (git-review-phase2--file-by-path files2 "README.md"))
                 (util2 (git-review-phase2--file-by-path files2 "lib/util.el")))
            (should-not (+git-review--file-reviewed-p readme2 synced))
            (should (+git-review--file-reviewed-p util2 synced)))))))))

(ert-deftest git-review-phase2-tree-navigation-and-file-diff ()
  "Tree RET/o/q/t, Evil normal state, and exact per-file diffs work offline."
  (require 'magit)
  (require 'evil)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "nav"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (git-review-baseline--with-instrumentation
               (lambda ()
                 (let* ((target (+git-review-target-for-worktree root))
                        (overview (+git-review--open-overview target))
                        (windows-before (length (window-list)))
                        tree)
                   (with-current-buffer overview
                     (should (eq evil-state 'normal))
                     (setq tree (+git-review-open-changes-tree target)))
                   (with-current-buffer tree
                     (should (derived-mode-p '+git-changes-tree-mode))
                     (should (eq evil-state 'normal))
                     (should (eq (key-binding "n") #'evil-search-next))
                     (should (eq (key-binding "N") #'evil-search-previous))
                     (should (= windows-before (length (window-list))))
                     (git-review-phase2--goto-tree-file "README.md")
                     (+git-changes-tree-visit-file)
                     (should (derived-mode-p 'magit-diff-mode))
                     (should (equal +git-review-file-path "README.md"))
                     (should (eq (current-buffer)
                                 (+git-review--find-file-diff-buffer
                                  target "README.md")))
                     (+git-review-quit)
                     (should (eq (current-buffer) tree))))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-file-diff-kinds ()
  "Per-file diffs cover added/modified/deleted/rename/untracked exactly."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "diff-kinds"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (let* ((target (+git-review-target-for-worktree root))
                     (files (+git-review-collect-files target))
                     (overview (+git-review--open-overview target))
                     (ov-text (with-current-buffer overview (buffer-string))))
                ;; Worktree overview includes untracked as an all-added patch.
                (should (string-match-p "untracked\\.txt" ov-text))
                (let* ((mod (git-review-phase2--file-by-path files "README.md"))
                       (add (git-review-phase2--file-by-path files "added.el"))
                       (del (git-review-phase2--file-by-path files "to-delete.el"))
                       (ren (git-review-phase2--file-by-path files "new-name.el"))
                       (unt (git-review-phase2--file-by-path files "untracked.txt")))
                  (should (eq (+git-review-file-status mod) 'modified))
                  (should (eq (+git-review-file-status add) 'added))
                  (should (eq (+git-review-file-status del) 'deleted))
                  (should (eq (+git-review-file-status ren) 'renamed))
                  (should (equal (+git-review-file-old-path ren) "old-name.el"))
                  (should (numberp (+git-review-file-additions ren)))
                  (should (numberp (+git-review-file-deletions ren)))
                  (should (eq (+git-review-file-status unt) 'untracked))
                  (dolist (spec `((,mod "README.md" nil)
                                  (,add "added.el" "new file")
                                  (,del "to-delete.el" "deleted")
                                  (,ren "new-name.el" "rename from")
                                  (,unt "untracked.txt" "new file")))
                    (pcase-let* ((`(,file ,path ,needle) spec)
                                 (buf (+git-review-open-file-diff target file))
                                 (text (with-current-buffer buf
                                         (buffer-string))))
                      (should (equal (buffer-local-value
                                      '+git-review-file-path buf)
                                     path))
                      (should (> (length text) 0))
                      (when needle
                        (should (string-match-p needle text)))
                      (when (eq file ren)
                        (should (string-match-p "rename from old-name\\.el" text))
                        (should (string-match-p "rename to new-name\\.el" text))
                        (should-not (string-match-p "new file" text)))))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-file-diff-special-kinds ()
  "Copy/binary/submodule buffers keep file sections, visit, and clear text.
Assertions cover open and post-refresh: visible labels, Magit file section,
`magit-diff--file', and `e' visit behavior."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "diff-special"
      (lambda (root)
        (git-review-fixtures-create-phase2 root)
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (let* ((target (+git-review-target-for-worktree root))
                     (files (+git-review-collect-files target))
                     (copy (git-review-phase2--file-by-path files "copy-dest.el"))
                     (bin (git-review-phase2--file-by-path files "assets/logo.bin"))
                     (sub (git-review-phase2--file-by-path files "vendor/lib")))
                (should (eq (+git-review-file-status copy) 'copied))
                (should (eq (+git-review-file-status bin) 'binary))
                (should (eq (+git-review-file-status sub) 'submodule))
                (dolist (spec
                         `((,copy "copy-dest.el"
                                  ("copied" "copy from copy-source\\.el"
                                   "copy to copy-dest\\.el")
                                  ("new file")
                                  visit)
                           (,bin "assets/logo.bin"
                                 ("BINARY" "Binary file" "assets/logo\\.bin")
                                 nil
                                 visit)
                           (,sub "vendor/lib"
                                 ("SUBMODULE\\|submodule"
                                  "Submodule/gitlink\\|Subproject"
                                  "vendor/lib")
                                 ("new file")
                                 submodule-error)))
                  (pcase-let* ((`(,file ,path ,needles ,forbidden ,visit) spec)
                               (buf (+git-review-open-file-diff target file)))
                    (dotimes (_ 2)
                      (should (equal (buffer-local-value
                                      '+git-review-file-path buf)
                                     path))
                      (with-current-buffer buf
                        (goto-char (point-min))
                        (let* ((text (buffer-string))
                               (sec (magit-diff--file-section))
                               (mf (magit-diff--file)))
                          (dolist (needle needles)
                            (should (string-match-p needle text)))
                          (dolist (bad forbidden)
                            (should-not (string-match-p bad text)))
                          (should (magit-file-section-p sec))
                          (should (equal (oref sec value) path))
                          (should (stringp mf))
                          (should (string-suffix-p path mf)))
                        (pcase visit
                          ('visit
                           (+git-review-visit-worktree)
                           (should (file-equal-p
                                    (buffer-file-name)
                                    (expand-file-name path root)))
                           (kill-buffer (current-buffer))
                           (set-buffer buf))
                          ('submodule-error
                           (should-error (+git-review-visit-worktree)
                                         :type 'user-error)))
                        (magit-refresh))))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-immutable-guards ()
  "Commit/branch review buffers cannot stage, unstage, or discard."
  (require 'magit)
  (git-review-fixtures-with-repo
   "immutable"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let* ((default-directory (file-name-as-directory root))
            (head (string-trim-right
                   (git-review-fixtures--git-ok root "rev-parse" "HEAD"))))
       (unwind-protect
           (let* ((target (+git-review-target-for-commit head root))
                  (buf (+git-review--open-overview target)))
             (with-current-buffer buf
               (should (null (+git-review-target-mutable-p +git-review-target)))
               (should-error (magit-stage) :type 'user-error)
               (should-error (magit-unstage) :type 'user-error)
               (should-error (magit-discard) :type 'user-error)))
         (git-review-baseline-cleanup-repo-buffers root))))))

(ert-deftest git-review-phase2-offline-public-commands ()
  "Public open/tree/diff/refresh commands stay offline and tool-free."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "offline"
      (lambda (root)
        (git-review-fixtures-create-small root)
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (let* ((pair (git-review-baseline--with-instrumentation
                            (lambda ()
                              (setq git-review-baseline--recording-p t)
                              (let* ((target (+git-review-target-for-worktree root))
                                     (ov (+git-review--open-overview target))
                                     (tree (+git-changes-tree-setup-buffer target))
                                     (file (git-review-phase2--file-by-path
                                            (with-current-buffer tree
                                              +git-review--files)
                                            "README.md")))
                                (+git-review-open-file-diff target file)
                                (with-current-buffer tree
                                  (+git-review-refresh))
                                (list ov tree)))))
                     (log (cdr pair))
                     (summary (git-review-baseline--summarize-processes log))
                     (counts (plist-get summary :counts)))
                (should (= 0 (alist-get 'delta counts)))
                (should (= 0 (alist-get 'difft counts)))
                (should (= 0 (length (plist-get summary :blocked-attempts)))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-large-tree-bounded-and-fast ()
  "100-file tree creates no file diffs, stays bounded, and is warm-fast."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "large-tree"
      (lambda (root)
        (git-review-fixtures-create-large root 100)
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (let* ((target (+git-review-target-for-worktree root))
                     (samples nil)
                     (process-samples nil))
                ;; Prewarm.
                (let ((+git-review--git-process-count 0))
                  (+git-changes-tree-setup-buffer target)
                  (should (null (git-review-phase2--diff-file-buffers)))
                  (should (< +git-review--git-process-count
                             +git-review--max-git-processes-per-refresh)))
                ;; Warm samples.
                (dotimes (_ 5)
                  (let* ((+git-review--git-process-count 0)
                         (start (current-time)))
                    (with-current-buffer
                        (+git-changes-tree-setup-buffer target)
                      (magit-refresh))
                    (push (float-time (time-since start)) samples)
                    (push +git-review--git-process-count process-samples)
                    (should (null (git-review-phase2--diff-file-buffers)))))
                (setq samples (nreverse samples))
                (setq process-samples (nreverse process-samples))
                (let* ((sorted (sort (copy-sequence samples) #'<))
                       (median (nth (/ (length sorted) 2) sorted))
                       (min-s (car sorted))
                       (max-s (car (last sorted)))
                       (max-procs (apply #'max process-samples)))
                  (message
                   "phase2 large-tree warm: median=%.3fs min=%.3fs max=%.3fs procs=%S"
                   median min-s max-s process-samples)
                  (should (< median 1.0))
                  (should (<= max-procs
                              +git-review--max-git-processes-per-refresh))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-branch-advance-keeps-reviewed ()
  "Advancing a symbolic branch head keeps reviewed state for unchanged files.
Persistence identity (`family-id') stays stable; immutable buffer identity
(`overview-id') changes and must not reuse the prior overview buffer."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     (git-review-fixtures-with-repo
      "branch-advance"
      (lambda (root)
        (git-review-fixtures--write-file root "keep.el" "keep-v1\n")
        (git-review-fixtures--write-file root "change.el" "change-v1\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "base")
        (git-review-fixtures--git-ok root "checkout" "-b" "feature")
        (git-review-fixtures--write-file root "keep.el" "keep-v2\n")
        (git-review-fixtures--write-file root "change.el" "change-v2\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "feature-1")
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (let* ((t1 (+git-review-target-for-branch "main" "feature" root))
                     (files1 (+git-review-collect-files t1))
                     (keep1 (git-review-phase2--file-by-path files1 "keep.el"))
                     (chg1 (git-review-phase2--file-by-path files1 "change.el"))
                     (map (make-hash-table :test #'equal))
                     (family1 (+git-review-target-family-id t1))
                     (overview1-id (+git-review-target-overview-id t1))
                     (ov1 (+git-review--open-overview t1)))
                (should keep1)
                (should chg1)
                (puthash "keep.el" (+git-review-file-fingerprint keep1) map)
                (puthash "change.el" (+git-review-file-fingerprint chg1) map)
                (+git-review-state-save t1 map)
                ;; Advance feature with only change.el modified.
                (git-review-fixtures--write-file root "change.el" "change-v3\n")
                (git-review-fixtures--git-ok root "add" "-A")
                (git-review-fixtures--git-ok root "commit" "-m" "feature-2")
                (let* ((t2 (+git-review-target-for-branch "main" "feature" root))
                       (family2 (+git-review-target-family-id t2))
                       (overview2-id (+git-review-target-overview-id t2))
                       (files2 (+git-review-collect-files t2))
                       (synced (+git-review--sync-reviewed-map
                                files2 (+git-review-state-load t2)))
                       (keep2 (git-review-phase2--file-by-path files2 "keep.el"))
                       (chg2 (git-review-phase2--file-by-path files2 "change.el"))
                       (ov2 (+git-review--open-overview t2)))
                  (should (equal family1 family2))
                  (should (not (equal overview1-id overview2-id)))
                  (should (not (equal (+git-review-target-head-oid t1)
                                      (+git-review-target-head-oid t2))))
                  (should (buffer-live-p ov1))
                  (should (not (eq ov1 ov2)))
                  (should keep2)
                  (should chg2)
                  (should (+git-review--file-reviewed-p keep2 synced))
                  (should-not (+git-review--file-reviewed-p chg2 synced))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(ert-deftest git-review-phase2-difftastic-d-only ()
  "Difftastic remains an explicit D action; d is not bound to it."
  (let ((ui (expand-file-name "core/init-git-ui.el" user-emacs-directory))
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents ui)
      (goto-char (point-min))
      (should (re-search-forward
               "(kbd \"D\") #'\\+difftastic/full" nil t))
      (goto-char (point-min))
      (should-not (re-search-forward
                   "(kbd \"d\") #'\\+difftastic/" nil t)))))

(ert-deftest git-review-phase2-faces-function-exists ()
  "Named face applicator exists for theme-safe native diffs."
  (should (fboundp '+git-review-apply-diff-faces))
  (+git-review-apply-diff-faces))

(defun git-review-phase2--show-all-sections (&optional section)
  "Recursively show SECTION (default: root) so nested headings are visible."
  (let ((section (or section magit-root-section)))
    (when section
      (magit-section-show section)
      (dolist (child (oref section children))
        (git-review-phase2--show-all-sections child)))))

(defun git-review-phase2--section-heading-line (section)
  "Return SECTION's heading line without the trailing newline."
  (string-trim-right
   (buffer-substring-no-properties
    (oref section start)
    (oref section content))))

(defun git-review-phase2--goto-dir (path)
  "Move point to the directory section whose value is PATH."
  (goto-char (point-min))
  (let ((found nil)
        (base (concat (file-name-nondirectory (directory-file-name path))
                      "/")))
    (while (and (not found)
                (re-search-forward (regexp-quote base) nil t))
      (beginning-of-line)
      (let ((sec (magit-current-section)))
        (when (and (magit-section-match 'directory sec)
                   (equal (oref sec value) path))
          (setq found sec)))
      (end-of-line))
    (unless found
      (error "Tree directory %s not found" path))
    (beginning-of-line)
    found))

(defun git-review-phase2--ascii-only-p (string)
  "Return non-nil when STRING contains only ASCII characters."
  (cl-every (lambda (c) (< c 128)) string))

(ert-deftest git-review-phase2-tree-ascii-and-indentation ()
  "Changes Tree uses ASCII chrome and depth-based 4-space indentation.
Also keeps full path section values, nested SPC scope, RET file diffs,
and Unicode filenames unchanged."
  (require 'magit)
  (git-review-phase2--with-state-dir
   (lambda (_state)
     ;; ASCII-only fixture: no Unicode path segments.
     (git-review-fixtures-with-repo
      "ascii-layout"
      (lambda (root)
        (git-review-fixtures--write-file
         root "src/api/users.ts" "export const users = 1;\n")
        (git-review-fixtures--write-file
         root "src/api/auth.ts" "export const auth = 1;\n")
        (git-review-fixtures--write-file
         root "src/ui/panel.tsx" "export const panel = 1;\n")
        (git-review-fixtures--git-ok root "add" "-A")
        (git-review-fixtures--git-ok root "commit" "-m" "base")
        (git-review-fixtures--write-file
         root "src/api/users.ts" "export const users = 2;\n")
        (git-review-fixtures--write-file
         root "src/api/auth.ts" "export const auth = 2;\n")
        (git-review-fixtures--write-file
         root "src/ui/panel.tsx" "export const panel = 2;\n")
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (let* ((target (+git-review-target-for-worktree root))
                     (buf (+git-changes-tree-setup-buffer target)))
                (with-current-buffer buf
                  (git-review-phase2--show-all-sections)
                  ;; 1. Header uses ASCII | and not middle-dot.
                  (should (stringp header-line-format))
                  (should (string-match-p "|" header-line-format))
                  (should-not (string-match-p "·" header-line-format))
                  (should (string-match-p
                           "Changes  [0-9]+/[0-9]+ reviewed  |  "
                           header-line-format))
                  (should (string-match-p
                           "Changes  [0-9]+/[0-9]+ reviewed  |  "
                           (buffer-string)))
                  (should-not (string-match-p "·" (buffer-string)))
                  ;; 2. ASCII-only fixture => entirely ASCII tree buffer.
                  (should (git-review-phase2--ascii-only-p (buffer-string)))
                  ;; 3-6. Indentation by depth; 7. full path values.
                  (let ((src (git-review-phase2--goto-dir "src")))
                    (should (equal (oref src value) "src"))
                    (should (string-prefix-p "[ ] src/"
                                             (git-review-phase2--section-heading-line
                                              src)))
                    (should (= 0 (string-match "\\`\\["
                                               (git-review-phase2--section-heading-line
                                                src)))))
                  (let ((api (git-review-phase2--goto-dir "src/api")))
                    (should (equal (oref api value) "src/api"))
                    (should (string-prefix-p
                             "    [ ] api/"
                             (git-review-phase2--section-heading-line api))))
                  (git-review-phase2--goto-tree-file "src/api/users.ts")
                  (let* ((sec (magit-current-section))
                         (line (git-review-phase2--section-heading-line sec)))
                    (should (equal (oref sec value) "src/api/users.ts"))
                    (should (string-prefix-p "        [ ] users.ts" line))
                    ;; Exact-line shape: indent + checkbox + name + status + counts.
                    (should (string-match-p
                             "\\`        \\[ \\] users\\.ts +M +\\+[0-9]+ -[0-9]+\\'"
                             line)))
                  (git-review-phase2--goto-tree-file "src/ui/panel.tsx")
                  (should (equal (oref (magit-current-section) value)
                                 "src/ui/panel.tsx"))
                  (should (string-prefix-p
                           "        [ ] panel.tsx"
                           (git-review-phase2--section-heading-line
                            (magit-current-section))))
                  ;; 8. SPC on nested directory affects only its descendants.
                  (let ((before (hash-table-count +git-review--reviewed-map))
                        (api-files '("src/api/users.ts" "src/api/auth.ts")))
                    (git-review-phase2--goto-dir "src/api")
                    (+git-changes-tree-toggle-reviewed)
                    (dolist (path api-files)
                      (should (+git-review--file-reviewed-p
                               (git-review-phase2--file-by-path
                                +git-review--files path)
                               +git-review--reviewed-map)))
                    (should-not
                     (+git-review--file-reviewed-p
                      (git-review-phase2--file-by-path
                       +git-review--files "src/ui/panel.tsx")
                      +git-review--reviewed-map))
                    (should (> (hash-table-count +git-review--reviewed-map)
                               before)))
                  ;; 9. RET from indented file opens the correct per-file diff.
                  (git-review-phase2--goto-tree-file "src/api/users.ts")
                  (let ((diff (+git-changes-tree-visit-file)))
                    (should (buffer-live-p diff))
                    (should (equal (buffer-local-value
                                    '+git-review-file-path diff)
                                   "src/api/users.ts"))
                    (should (derived-mode-p 'magit-diff-mode)))))
            (git-review-baseline-cleanup-repo-buffers root)))))
     ;; 11. Unicode filenames remain supported; UI chrome stays ASCII.
     (git-review-fixtures-with-repo
      "unicode-layout"
      (lambda (root)
        (git-review-fixtures-create-phase2 root)
        (let ((default-directory (file-name-as-directory root)))
          (unwind-protect
              (let* ((target (+git-review-target-for-worktree root))
                     (buf (+git-changes-tree-setup-buffer target))
                     (uni "docs/ユニコード.el"))
                (with-current-buffer buf
                  (git-review-phase2--show-all-sections)
                  (should (string-match-p "ユニコード\\.el" (buffer-string)))
                  (should-not (string-match-p "·" (buffer-string)))
                  (should-not (string-match-p "—" (buffer-string)))
                  (git-review-phase2--goto-tree-file uni)
                  (let* ((sec (magit-current-section))
                         (line (git-review-phase2--section-heading-line sec)))
                    (should (equal (oref sec value) uni))
                    (should (string-prefix-p "    [ ] ユニコード.el" line))
                    ;; Chrome around the Unicode name is ASCII.
                    (should (string-match-p "\\`    \\[ \\] " line))
                    (should (string-match-p "  M  " line)))))
            (git-review-baseline-cleanup-repo-buffers root))))))))

(provide 'git-review-phase2)

;;; git-review-phase2.el ends here
