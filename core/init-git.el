;;; init-git.el --- Magit package ownership and Git entry points -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Magit package/configuration and top-level Git dispatch

;;; Commentary:
;; Phase 1 ownership for Magit lives here.  Review UI (display policy, Evil
;; review mode, visits, faces, Delta/Difftastic) lives in `init-git-ui.el'.
;; Synchronization lives in `init-git-sync.el'.
;;
;; Entry points:
;;   C-x g       — magit-status (direct)
;;   C-c g       — +git-dispatch Transient
;;   C-x M-g     — magit-dispatch
;;
;; Review helpers:
;;   +git/review          — working-tree review
;;   +git/review-staged   — staged review
;;   +git/review-commit   — one commit
;;   +git/review-branch   — branch merge-base..head review
;;   +git/review-pull-request — cached PR workspace (Phase 5)
;;   +git/log-oneline     — compact log
;;
;; Explicit sync (network-capable):
;;   C-c g f              — synchronize current repository
;;   C-c g F              — synchronize allowlisted repositories
;;
;; Phase 2 local review targets and the Changes Tree live in
;; `init-git-ui.el'.  Forge ownership lives in `init-forge.el'.
;; PR workspace lives in `init-git-pr.el'.

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
Use `+git/increase-context' or `+git/decrease-context' to adjust.")

(defun +git/review ()
  "Open a reusable working-tree review overview (HEAD vs worktree/index)."
  (interactive)
  (+git-review-open-worktree))

(defun +git/review-staged ()
  "Open a reusable staged review overview (HEAD vs index)."
  (interactive)
  (+git-review-open-staged))

(defun +git/review-unstaged ()
  "Open a reusable unstaged/untracked review overview (index vs worktree)."
  (interactive)
  (+git-review-open-unstaged))

(defun +git/status (&optional root)
  "Open Magit status for the active local edit context.
From a PR buffer this deliberately ignores the internal bare mirror."
  (interactive)
  (require 'magit)
  (magit-status
   (+git-review--normalize-root
    (or root (+git-review--local-entry-root)))))

(defun +git/review-commit (&optional commit root)
  "Open a reusable commit review overview for COMMIT.
If COMMIT is nil, prompt for one.  Root commits use Git's empty tree."
  (interactive)
  (require 'magit)
  (let* ((root (+git-review--normalize-root
                (or root (+git-review--local-entry-root))))
         (commit (or commit
                     (let ((default-directory
                            (file-name-as-directory root)))
                       (magit-read-branch-or-commit "Review commit")))))
    (+git-review-open-commit commit root)))

(defun +git/review-branch (&optional base head root)
  "Open a reusable branch review for merge-base(BASE, HEAD)..HEAD.
Prompt for BASE and HEAD when omitted."
  (interactive)
  (require 'magit)
  (let* ((root (+git-review--normalize-root
                (or root (+git-review--local-entry-root))))
         (default-directory (file-name-as-directory root))
         (base (or base (magit-read-branch-or-commit "Review base")))
         (head (or head (magit-read-branch-or-commit "Review head"))))
    (+git-review-open-branch base head root)))

(defun +git/log-oneline ()
  "Show a compact one-line-per-commit git log."
  (interactive)
  (require 'magit)
  (let ((default-directory
         (file-name-as-directory (+git-review--local-entry-root))))
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
  "Git review dispatch.
`g/r/s/u/c/b/p/l' are local-only.  `f/F' are explicitly network-capable."
  [["Status"
    ("g" "current PR / worktree home" +git/home)]
   ["Review (local)"
    ("r" "working-tree review" +git/review)
    ("s" "staged review" +git/review-staged)
    ("u" "unstaged + untracked review" +git/review-unstaged)
    ("c" "commit review" +git/review-commit)
    ("b" "branch review" +git/review-branch)
    ("p" "pull request review (cached)" +git/review-pull-request)
    ("l" "compact log" +git/log-oneline)]
   ["Sync (network)"
    ("f" "synchronize repository" +git/sync)
    ("F" "synchronize allowlisted" +git/sync-all)]])

(global-set-key (kbd "C-c g") #'+git-dispatch)

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
