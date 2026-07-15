;;; init-git.el --- Magit package ownership and Git entry points -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Magit package/configuration and top-level Git dispatch

;;; Commentary:
;; Phase 1 ownership for Magit lives here.  Review UI (display policy, Evil
;; review mode, visits, faces, Delta/Difftastic) lives in `init-git-ui.el'.
;;
;; Entry points:
;;   C-x g       — magit-status (direct)
;;   C-c g       — +git-dispatch Transient (local review commands only)
;;   C-x M-g     — magit-dispatch
;;
;; Review helpers:
;;   +git/review          — working-tree review
;;   +git/review-staged   — staged review
;;   +git/review-commit   — one commit
;;   +git/review-branch   — branch merge-base..head review
;;   +git/log-oneline     — compact log
;;
;; Phase 2 local review targets and the Changes Tree live in
;; `init-git-ui.el'.  These entry points only construct targets and
;; open the reusable overview buffers.

;;; Code:

;; Transient is built into Emacs 28+ / bundled with Magit.
(require 'transient nil t)

;; =============================================================================
;; Magit package and base settings (single owner)
;; =============================================================================

;; Magit depends on cond-let; pin recipe explicitly to avoid recipe lookup issues.
(straight-use-package '(cond-let :type git :host github :repo "tarsius/cond-let"))

(use-package magit
  :straight t
  :commands (magit-status magit-dispatch magit-diff-working-tree
             magit-diff-staged magit-diff-range magit-log-current)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  ;; Show diff when committing
  (setq magit-commit-show-diff t)

  ;; Don't ask before saving buffers
  (setq magit-save-repository-buffers 'dontask)

  ;; Word-level refinement only on the hunk at point ('t), not every hunk
  ;; ('all). Refining all hunks is a major cost when a status/diff buffer
  ;; has many files, and is the main reason refresh stalls past ~30 files.
  (setq magit-diff-refine-hunk t)

  ;; Start with a generous amount of context (like GitHub's default)
  (setq magit-diff-extra-context 6)

  ;; --- Initial visibility: collapse file sections ---
  (setq magit-section-initial-visibility-alist
        '((file . hide)
          (untracked . hide)))

  ;; Paint added/removed stats in the file header
  (setq magit-diff-paint-whitespace-lines 'both)

  ;; Adjust diff arguments for cleaner output
  (setq magit-diff-arguments '("--no-ext-diff"))
  (setq magit-diff-options '("--no-ext-diff"))

  ;; Show fine (word-level) diffs inline without whitespace noise
  (setq magit-diff-refine-ignore-whitespace t)

  ;; Show process buffer only on errors (less noise)
  (setq magit-process-popup-time -1)

  ;; Auto-revert tracked buffers after git operations
  (setq magit-auto-revert-mode t)

  ;; Performance: limit hunk-region highlight cost
  (setq magit-diff-highlight-hunk-region-functions
        '(magit-diff-highlight-hunk-region-dim-outside
          magit-diff-highlight-hunk-region-using-face)))

;; =============================================================================
;; Local review entry points
;; =============================================================================

(defvar +git/review-context-lines 6
  "Number of context lines to show around each change.
Use +/- in the review buffer to adjust.")

(defun +git/review ()
  "Open a reusable working-tree review overview (HEAD vs worktree/index)."
  (interactive)
  (+git-review-open-worktree))

(defun +git/review-staged ()
  "Open a reusable staged review overview (HEAD vs index)."
  (interactive)
  (+git-review-open-staged))

(defun +git/review-commit (&optional commit)
  "Open a reusable commit review overview for COMMIT.
If COMMIT is nil, prompt for one.  Root commits use Git's empty tree."
  (interactive
   (progn (require 'magit)
          (list (magit-read-branch-or-commit "Review commit"))))
  (+git-review-open-commit commit))

(defun +git/review-branch (&optional base head)
  "Open a reusable branch review for merge-base(BASE, HEAD)..HEAD.
Prompt for BASE and HEAD when omitted."
  (interactive
   (progn (require 'magit)
          (list (magit-read-branch-or-commit "Review base")
                (magit-read-branch-or-commit "Review head"))))
  (+git-review-open-branch base head))

(defun +git/log-oneline ()
  "Show a compact one-line-per-commit git log."
  (interactive)
  (require 'magit)
  (let ((default-directory (or (magit-toplevel)
                               (user-error "Not inside a Git repository"))))
    (magit-log-current '("--oneline" "--graph" "--decorate") '("-n100"))))

(defun +git/increase-context ()
  "Show more context lines around each change."
  (interactive)
  (setq +git/review-context-lines (min 50 (+ +git/review-context-lines 3)))
  (message "Context: %d lines" +git/review-context-lines)
  (+git/refresh-with-context))

(defun +git/decrease-context ()
  "Show fewer context lines around each change."
  (interactive)
  (setq +git/review-context-lines (max 0 (- +git/review-context-lines 3)))
  (message "Context: %d lines" +git/review-context-lines)
  (+git/refresh-with-context))

(defun +git/refresh-with-context ()
  "Refresh the current diff buffer with updated context lines."
  (when (derived-mode-p 'magit-diff-mode)
    (setq magit-buffer-diff-args
          (list "--stat" "--no-ext-diff"
                (format "-U%d" +git/review-context-lines)))
    (magit-refresh)))

;; =============================================================================
;; C-c g Transient dispatch (local only; never contacts a remote)
;; =============================================================================

(transient-define-prefix +git-dispatch ()
  "Git review dispatch for local Magit entry points.
Does not fetch, pull, push, or contact Forge remotes."
  [["Status"
    ("g" "status" magit-status)]
   ["Review"
    ("r" "working-tree review" +git/review)
    ("s" "staged review" +git/review-staged)
    ("c" "commit review" +git/review-commit)
    ("b" "branch review" +git/review-branch)
    ("l" "compact log" +git/log-oneline)]])

(global-set-key (kbd "C-c g") #'+git-dispatch)

;; =============================================================================
;; forge — GitHub/GitLab pull requests & issues inside magit
;; =============================================================================
;; Topic metadata remains cached in ~/.emacs.d/forge-database.sqlite.
;; Phase 1 removes on-visit fetch advice: RET opens the cached topic buffer.

(use-package forge
  :straight t
  :after magit
  :init
  ;; Skip forge's default binding injection: in current magit, the
  ;; transient slot it targets (`"o"' in magit-dispatch) has moved or
  ;; been removed.  Setting this in `:init' runs before forge loads.
  (setq forge-add-default-bindings nil))

;; =============================================================================
;; magit-todos — Surface TODO/FIXME/HACK in magit-status
;; =============================================================================

(use-package magit-todos
  :straight t
  :after magit
  :if (or (executable-find "rg") (executable-find "ag"))
  :config
  (setq magit-todos-keywords '("TODO" "FIXME" "HACK" "XXX" "NOTE")
        magit-todos-max-items 50
        magit-todos-recursive t
        magit-todos-exclude-globs '(".git/" "node_modules/" "target/" "dist/"))
  (magit-todos-mode 1))

;;; init-git.el ends here
