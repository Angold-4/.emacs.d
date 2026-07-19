;;; git-review-fixtures.el --- Temporary Git fixtures for review tests -*- lexical-binding: t -*-

;;; Commentary:
;; Builds disposable Git repositories under temporary directories for ERT.
;; Fixtures never live in the Emacs configuration repository.  All Git
;; invocations use argument lists (call-process), never shell strings.

;;; Code:

(require 'cl-lib)

(defconst git-review-fixtures-test-name "Git Review Fixture"
  "Local committer name used inside temporary fixtures.")

(defconst git-review-fixtures-test-email "git-review-fixture@example.test"
  "Local committer email used inside temporary fixtures.")

(defun git-review-fixtures--call-git (dir &rest args)
  "Run git ARGS in DIR and return (exit-code . stdout-string).
Uses `call-process' with a separate argument list so paths with spaces
are safe.  Never builds a shell command string."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
           (process-environment
            (append
             (list "GIT_CONFIG_GLOBAL=/dev/null"
                   "GIT_CONFIG_SYSTEM=/dev/null"
                   "GIT_TERMINAL_PROMPT=0"
                   (concat "GIT_AUTHOR_NAME=" git-review-fixtures-test-name)
                   (concat "GIT_AUTHOR_EMAIL=" git-review-fixtures-test-email)
                   (concat "GIT_COMMITTER_NAME=" git-review-fixtures-test-name)
                   (concat "GIT_COMMITTER_EMAIL=" git-review-fixtures-test-email))
             process-environment))
           (exit (apply #'call-process "git" nil t nil args)))
      (cons exit (buffer-string)))))

(defun git-review-fixtures--git-ok (dir &rest args)
  "Run git ARGS in DIR and signal an error unless exit status is 0."
  (let* ((result (apply #'git-review-fixtures--call-git dir args))
         (exit (car result))
         (out (cdr result)))
    (unless (eq exit 0)
      (error "git %s failed in %s (exit %s): %s"
             (mapconcat #'identity args " ") dir exit out))
    out))

(defun git-review-fixtures--write-file (root relative contents)
  "Write CONTENTS to ROOT/RELATIVE, creating parent directories as needed."
  (let ((path (expand-file-name relative root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert contents))
    path))

(defun git-review-fixtures--init-repo (root)
  "Initialize a local Git repository at ROOT with a disposable identity."
  (make-directory root t)
  (git-review-fixtures--git-ok root "init" "-b" "main")
  (git-review-fixtures--git-ok root "config" "user.name" git-review-fixtures-test-name)
  (git-review-fixtures--git-ok root "config" "user.email" git-review-fixtures-test-email)
  ;; Keep fixtures deterministic and offline.
  (git-review-fixtures--git-ok root "config" "commit.gpgsign" "false")
  root)

(defun git-review-fixtures-make-temp-root (prefix)
  "Create a temporary directory whose path contains a space.
PREFIX is a short label included in the directory name."
  (let* ((parent (make-temp-file (format "git-review %s " prefix) t))
         (root (expand-file-name (format "%s repo" prefix) parent)))
    (make-directory root t)
    root))

(defun git-review-fixtures-with-repo (prefix fn)
  "Call FN with a temporary Git repository root named from PREFIX.
The temporary tree is deleted afterward via `unwind-protect'.
Binds `+git-store-registry-directory' to a disposable directory when the
store module is loaded so tests never touch the owner's registry."
  (let ((root (git-review-fixtures-make-temp-root prefix))
        (registry-dir nil)
        (store-bound (boundp '+git-store-registry-directory)))
    (unwind-protect
        (progn
          (git-review-fixtures--init-repo root)
          (if store-bound
              (let* ((+git-store-registry-directory
                      (setq registry-dir
                            (make-temp-file "git-review-registry " t))))
                (+git-store-reset-registry)
                (funcall fn root))
            (funcall fn root)))
      (ignore-errors
        (delete-directory (file-name-directory
                           (directory-file-name root))
                          t))
      (when (and registry-dir (file-directory-p registry-dir))
        (ignore-errors (delete-directory registry-dir t)))
      (when store-bound
        (ignore-errors (+git-store-reset-registry))))))

(defun git-review-fixtures-create-small (root)
  "Populate ROOT with a small working tree covering common change kinds.

After this function returns, ROOT contains:
- an unstaged modification of a tracked file;
- a staged modification of a tracked file;
- an untracked file;
- a staged added file;
- a deleted tracked file (staged deletion);
- a renamed tracked file (staged rename);
- nested directories under src/nested/."
  (git-review-fixtures--write-file
   root "README.md" "# fixture\nline-one\n")
  (git-review-fixtures--write-file
   root "src/nested/app.el" ";; nested app\n(defun fixture-app () t)\n")
  (git-review-fixtures--write-file
   root "lib/util.el" ";; util v1\n(defun fixture-util () 1)\n")
  (git-review-fixtures--write-file
   root "old-name.el" ";; rename me\n(defun fixture-old () t)\n")
  (git-review-fixtures--write-file
   root "to-delete.el" ";; delete me\n(defun fixture-del () t)\n")
  (git-review-fixtures--git-ok root "add" "-A")
  (git-review-fixtures--git-ok root "commit" "-m" "fixture: initial commit")

  ;; Unstaged modification of a tracked file.
  (git-review-fixtures--write-file
   root "README.md" "# fixture\nline-one\nline-two-modified\n")

  ;; Staged modification of a tracked file.
  (git-review-fixtures--write-file
   root "lib/util.el" ";; util v2 staged\n(defun fixture-util () 2)\n")
  (git-review-fixtures--git-ok root "add" "--" "lib/util.el")

  ;; Untracked file.
  (git-review-fixtures--write-file
   root "untracked.txt" "I am untracked\n")

  ;; Staged added file.
  (git-review-fixtures--write-file
   root "added.el" ";; newly added\n(defun fixture-added () t)\n")
  (git-review-fixtures--git-ok root "add" "--" "added.el")

  ;; Staged deletion.
  (git-review-fixtures--git-ok root "rm" "--" "to-delete.el")

  ;; Staged rename (keeps history when similarity is high).
  (git-review-fixtures--git-ok root "mv" "--" "old-name.el" "new-name.el")

  root)

(defun git-review-fixtures-create-large (root &optional count)
  "Populate ROOT with at least COUNT changed files (default 100).

Creates a cheap base commit of COUNT files, then modifies every file so
later phases can time status/Changes Tree/diff rendering."
  (let ((n (or count 100)))
    (dotimes (i n)
      (let ((rel (format "files/f-%03d.txt" i)))
        (git-review-fixtures--write-file
         root rel (format "base content for %s\n" rel))))
    (git-review-fixtures--git-ok root "add" "-A")
    (git-review-fixtures--git-ok root "commit" "-m" "fixture: large base")
    (dotimes (i n)
      (let ((rel (format "files/f-%03d.txt" i)))
        (git-review-fixtures--write-file
         root rel (format "changed content for %s\nline-2\n" rel))))
    root))

(defun git-review-fixtures-create-phase2 (root)
  "Populate ROOT with Phase 2 change kinds for tree/persistence tests.

After this function returns, ROOT contains:
- nested directories with spaces and Unicode path segments;
- a binary file change;
- a copied file (detectable with git -C);
- an empty directory (Git omits it from the change set);
- a submodule/gitlink entry when nested repos are supported;
- the common small-fixture change kinds."
  (git-review-fixtures--write-file
   root "README.md" "# phase2\nbase\n")
  (git-review-fixtures--write-file
   root "src/nested/app.el" ";; nested\n(defun p2-app () t)\n")
  (git-review-fixtures--write-file
   root "src/ui/panel.tsx" "export const panel = 1;\n")
  (git-review-fixtures--write-file
   root "assets/logo.bin" "BASEBIN\n")
  (git-review-fixtures--write-file
   root "copy-source.el" ";; copy me\n(defun p2-copy () t)\n")
  (git-review-fixtures--write-file
   root "docs/review test.el" ";; space name\n(defun p2-space () t)\n")
  (git-review-fixtures--write-file
   root "docs/ユニコード.el" ";; unicode\n(defun p2-uni () t)\n")
  (git-review-fixtures--git-ok root "add" "-A")
  (git-review-fixtures--git-ok root "commit" "-m" "phase2: base")

  ;; Empty directory should not appear in Git's change list.
  (make-directory (expand-file-name "empty-dir" root) t)

  ;; Nested modifications.
  (git-review-fixtures--write-file
   root "src/nested/app.el" ";; nested changed\n(defun p2-app () nil)\n")
  (git-review-fixtures--write-file
   root "src/ui/panel.tsx" "export const panel = 2;\n")
  (git-review-fixtures--write-file
   root "docs/review test.el" ";; space name changed\n(defun p2-space () 1)\n")
  (git-review-fixtures--write-file
   root "docs/ユニコード.el" ";; unicode changed\n(defun p2-uni () 1)\n")

  ;; Binary change (force binary detection via NUL byte).
  (let ((bin (expand-file-name "assets/logo.bin" root)))
    (with-temp-file bin
      (set-buffer-multibyte nil)
      (insert "BIN\0CHANGED\n")))

  ;; Copy detection: identical content under a new name, keep source.
  (git-review-fixtures--write-file
   root "copy-dest.el" ";; copy me\n(defun p2-copy () t)\n")
  (git-review-fixtures--git-ok root "add" "--" "copy-dest.el")

  ;; Untracked file with a space.
  (git-review-fixtures--write-file
   root "new file.txt" "untracked phase2\n")

  ;; Optional submodule/gitlink: create a nested repo and add it.
  (let ((sub (expand-file-name "vendor/lib" root)))
    (make-directory sub t)
    (git-review-fixtures--git-ok sub "init" "-b" "main")
    (git-review-fixtures--git-ok sub "config" "user.name"
                                 git-review-fixtures-test-name)
    (git-review-fixtures--git-ok sub "config" "user.email"
                                 git-review-fixtures-test-email)
    (git-review-fixtures--write-file sub "mod.el" ";; submodule\n")
    (git-review-fixtures--git-ok sub "add" "-A")
    (git-review-fixtures--git-ok sub "commit" "-m" "submodule base")
    ;; Register as gitlink from the parent.
    (git-review-fixtures--git-ok root "add" "--" "vendor/lib"))

  root)

(defun git-review-fixtures-porcelain (root &rest extra-args)
  "Return `git status --porcelain=v1' output for ROOT.
EXTRA-ARGS are appended after --porcelain=v1 (for example -uall)."
  (string-trim-right
   (apply #'git-review-fixtures--git-ok
          root
          (append '("status" "--porcelain=v1") extra-args))))

(defun git-review-fixtures-name-status (root)
  "Return `git diff --cached --name-status' output for ROOT."
  (string-trim-right
   (git-review-fixtures--git-ok root "diff" "--cached" "--name-status")))

(provide 'git-review-fixtures)

;;; git-review-fixtures.el ends here
