;;; git-review-doc-test.el --- Git review documentation contract -*- lexical-binding: t -*-

;;; Commentary:
;; Keep the concise guide, architecture reference, and delivery status aligned
;; with the tested Phase 0-5 implementation.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst git-review-doc-test--files
  '("README.md"
    "docs.md"
    "git.md"
    "docs/git.md"
    "docs/git-review-auth.md"
    "docs/git-baseline.md"
    "git-plan.md")
  "User-facing Git documentation files checked by this suite.")

(defun git-review-doc-test--contents (relative)
  "Return the contents of RELATIVE under `user-emacs-directory'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative user-emacs-directory))
    (buffer-string)))

(ert-deftest git-review-documentation-current-contract ()
  "Guides describe the implemented command and PR workflows, not old keys."
  (let ((guide (git-review-doc-test--contents "git.md"))
        (architecture (git-review-doc-test--contents "docs/git.md"))
        (plan (git-review-doc-test--contents "git-plan.md"))
        (all (mapconcat #'git-review-doc-test--contents
                        git-review-doc-test--files "\n")))
    (dolist (required
             '("C-c g r    HEAD versus working tree/index"
               "C-c g f    fetch the shared Git mirror and update Forge"
               "C-c g p -> PR number -> c -> RET"
               "`SPC` toggles the file or folder"
               "`gr` is intentionally different"))
      (should (string-match-p (regexp-quote required) guide)))
    (dolist (required
             '("Status: **Phases 0–5 implemented on PR #5**"
               "core/init-git-store.el"
               "PR and PR-commit object operations use the published bare mirror"
               "dead-owner restart"))
      (should (string-match-p (regexp-quote required) architecture)))
    (should (string-match-p
             (regexp-quote "The next implementation action is **Phase 6 only**")
             plan))
    (dolist (stale
             '("Implementation status: **Not started"
               "C-c g g"
               "C-c g f / @"
               "Difftastic is available through `D`"
               "| `n` | Jump to next file"
               "The next implementation action is **Phase 1 only**"))
      (should-not (string-match-p (regexp-quote stale) all)))))

(ert-deftest git-review-documentation-local-links-resolve ()
  "Every relative Markdown link in the Git documentation resolves."
  (dolist (relative git-review-doc-test--files)
    (let* ((file (expand-file-name relative user-emacs-directory))
           (base (file-name-directory file)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward
                "]([ \t]*\\([^)\n]+\\)[ \t]*)" nil t)
          (let* ((raw (match-string-no-properties 1))
                 (target (car (split-string raw "#")))
                 (target (string-trim target "<" ">")))
            (unless (or (string-empty-p target)
                        (string-match-p
                         "\\`\\(?:https?\\|mailto\\|file\\):" target))
              (should
               (file-exists-p (expand-file-name target base))))))))))

(provide 'git-review-doc-test)

;;; git-review-doc-test.el ends here
