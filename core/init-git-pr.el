;;; init-git-pr.el --- Shared pull-request review workspace -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Cached PR model, overview, commit navigation, conversation

;;; Commentary:
;; Phase 5: shared read-only PR workspace keyed by canonical repository + PR
;; number.  Forge supplies metadata; the Phase 4 published mirror supplies
;; Git objects.  Opening and `gr' are offline; `@' remains the only explicit
;; sync path.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'init-forge)
(require 'init-git-store)
(require 'init-git-sync)
(require 'init-git-ui)

;; =============================================================================
;; PR model
;; =============================================================================

(cl-defstruct (+git-pr-commit
               (:constructor +git-pr-commit-create)
               (:copier nil))
  "One PR commit record from a bounded mirror rev-list."
  oid
  parents          ; list of parent OIDs
  subject
  author
  date
  merge-p)

(cl-defstruct (+git-pr
               (:constructor +git-pr--create)
               (:copier nil))
  "Shared PR workspace model.  Never reconstructed from rendered text."
  repository-id
  number
  forge-object          ; native Forge handle when available
  snapshot              ; +forge-pr-snapshot
  state
  title
  author
  draft
  base-ref
  head-ref
  base-repo
  head-repo
  head-user
  cross-repo-p
  base-oid              ; cached Forge base tip
  head-oid              ; cached Forge head tip (never squash/merge ref)
  merge-base-oid        ; git merge-base(base, head)
  mirror                ; published bare mirror directory
  commits               ; list of +git-pr-commit, oldest first
  files                 ; list of +git-review-file
  target                ; +git-review-target (scope pullreq)
  cache-generation
  last-success
  sync-status)          ; plist from +git-sync-status

(defvar-local +git-pr--model nil
  "Buffer-local `+git-pr' for the PR workspace and its children.")

(defvar-local +git-pr--commit-index nil
  "Index into `+git-pr-commits' when visiting a PR commit review.")

(defvar-local +git-pr--selected-parent nil
  "Explicit parent OID for merge-commit diffs in PR commit buffers.")

(defvar-local +git-pr--return-buffer nil
  "Stable return target for PR commit stepping (`q' -> PR workspace).")

(defvar-local +git-pr--revision-base-oid nil
  "Base OID used by the clean PR commit revision buffer.")

(defvar-local +git-pr--revision-head-oid nil
  "Head OID used by the clean PR commit revision buffer.")

;; =============================================================================
;; Range helpers (pure; tested against fixture graphs)
;; =============================================================================

(defun +git-pr-derive-range (mirror base-oid head-oid)
  "Return (MERGE-BASE . HEAD-OID) for exact PR review in MIRROR.
Signals when objects are missing or no merge base exists."
  (unless (and (stringp base-oid) (stringp head-oid)
               (not (string-empty-p base-oid))
               (not (string-empty-p head-oid)))
    (user-error
     "Cached PR base/head OID missing. Run C-c g f, wait for sync, then retry."))
  (unless (and mirror (file-directory-p mirror))
    (user-error
     "No published mirror. Run C-c g f, wait for sync, then retry."))
  (let* ((result (+git-review--call-git mirror "merge-base" base-oid head-oid))
         (exit (car result))
         (out (string-trim-right (cdr result))))
    (unless (and (eq exit 0) (not (string-empty-p out)))
      ;; The normal success path uses one Git process.  Probe individual
      ;; objects only on failure to keep PR open/refresh inexpensive.
      (cond
       ((not (+git-pr--object-exists-p mirror base-oid))
        (user-error
         (concat "Cached base OID %s is missing from the published mirror. "
                 "Run C-c g f (provider ref may be unavailable)")
         (+git-pr--short-oid base-oid)))
       ((not (+git-pr--object-exists-p mirror head-oid))
        (user-error
         (concat "Cached head OID %s is missing from the published mirror. "
                 "Run C-c g f (fork/provider ref may be unavailable)")
         (+git-pr--short-oid head-oid)))
       (t
        (user-error "No merge base for cached base %s and head %s"
                    (+git-pr--short-oid base-oid)
                    (+git-pr--short-oid head-oid)))))
    (cons out head-oid)))

(defun +git-pr--short-oid (oid)
  "Return a short display form of OID."
  (if (and (stringp oid) (> (length oid) 7))
      (substring oid 0 7)
    (or oid "?")))

(defun +git-pr--object-exists-p (mirror oid)
  "Return non-nil when OID names an existing commit in MIRROR.
Do not use `git rev-parse' for this check: it accepts a syntactically valid
40-hex OID even when the object is absent.  `cat-file -e OID^{commit}' verifies
both object presence and the type required by a PR range."
  (and (stringp oid)
       (stringp mirror)
       (file-directory-p mirror)
       (eq 0
           (car (+git-review--call-git
                 mirror "cat-file" "-e" (concat oid "^{commit}"))))))

(defun +git-pr--provider-pull-head-ref (number)
  "Return the GitHub-style provider pull head ref for NUMBER."
  (format "refs/pull/%d/head" number))

(defun +git-pr--provider-pull-merge-ref (number)
  "Return the GitHub-style synthetic pull merge ref for NUMBER."
  (format "refs/pull/%d/merge" number))

;; =============================================================================
;; Commit list (one bounded Git command)
;; =============================================================================

(defun +git-pr-collect-commits (mirror merge-base-oid head-oid)
  "Return oldest-first `+git-pr-commit' list for MERGE-BASE-OID..HEAD-OID.
Uses one bounded `rev-list --reverse --topo-order' invocation."
  (let* ((out (string-trim-right
               (+git-review--git-ok
                mirror "rev-list" "--reverse" "--topo-order"
                "--format=%H%x00%P%x00%s%x00%an%x00%ad"
                (format "%s..%s" merge-base-oid head-oid))))
         (lines (and out (not (string-empty-p out))
                     (split-string out "\n" t)))
         commits)
    ;; rev-list --format emits a "commit <oid>" header line then the format.
    (dolist (line (or lines nil))
      (cond
       ((string-prefix-p "commit " line)
        nil)
       (t
        (let* ((parts (split-string line "\0" nil))
               (oid (nth 0 parts))
               (parents-raw (or (nth 1 parts) ""))
               (subject (or (nth 2 parts) ""))
               (author (or (nth 3 parts) ""))
               (date (or (nth 4 parts) ""))
               (parents (split-string parents-raw " " t)))
          (when (and oid (not (string-empty-p oid)))
            (push (+git-pr-commit-create
                   :oid oid
                   :parents parents
                   :subject subject
                   :author author
                   :date date
                   :merge-p (> (length parents) 1))
                  commits))))))
    (nreverse commits)))

(defun +git-pr-commit-range (commit &optional parent-oid)
  "Return (BASE-OID . HEAD-OID) for COMMIT review.
Normal commits use first-parent..commit.  Root commits use the empty
tree.  Merge commits default to first parent unless PARENT-OID is set."
  (let* ((oid (+git-pr-commit-oid commit))
         (parents (+git-pr-commit-parents commit))
         (base (cond
                (parent-oid parent-oid)
                ((null parents) +git-review-empty-tree)
                (t (car parents)))))
    (cons base oid)))

;; =============================================================================
;; Build / refresh model
;; =============================================================================

(defun +git-pr--resolve-root (&optional root)
  "Return an absolute originating root for PR open."
  (+git-review--normalize-root
   (or root
       (and (bound-and-true-p +git-review-target)
            (+git-review-target-root +git-review-target))
       (+git-review--require-root))))

(defun +git-pr--build (repository-id number root context-id snapshot)
  "Build a `+git-pr' from SNAPSHOT using the published mirror."
  (let* ((st (+git-sync-status repository-id))
         (mirror (plist-get st :mirror))
         (base-oid (+forge-pr-snapshot-base-rev snapshot))
         (head-oid (+forge-pr-snapshot-head-rev snapshot)))
    (unless (and (numberp (plist-get st :generation))
                 (> (plist-get st :generation) 0)
                 mirror
                 (file-directory-p mirror)
                 (file-exists-p (expand-file-name "HEAD" mirror)))
      (user-error
       (concat "No published mirror generation for %s. "
               "Run C-c g f, wait for sync, then retry.")
       repository-id))
    (unless (and (stringp base-oid) (stringp head-oid)
                 (not (string-empty-p base-oid))
                 (not (string-empty-p head-oid)))
      (user-error
       (concat "PR #%s is cached but base/head OIDs are missing. "
               "Run C-c g f, wait for sync, then retry.")
       number))
    (pcase-let* ((`(,merge-base . ,head)
                  (+git-pr-derive-range mirror base-oid head-oid))
                 (commits (+git-pr-collect-commits mirror merge-base head))
                 (target (+git-review-target-for-pullreq
                          repository-id number
                          merge-base head
                          root context-id
                          (+forge-pr-snapshot-base-ref snapshot)
                          (+forge-pr-snapshot-head-ref snapshot)))
                 (files (+git-review-collect-files target)))
      (+git-pr--create
       :repository-id repository-id
       :number number
       :forge-object (+forge-pr-snapshot-forge-object snapshot)
       :snapshot snapshot
       :state (+forge-pr-snapshot-state snapshot)
       :title (+forge-pr-snapshot-title snapshot)
       :author (+forge-pr-snapshot-author snapshot)
       :draft (+forge-pr-snapshot-draft snapshot)
       :base-ref (+forge-pr-snapshot-base-ref snapshot)
       :head-ref (+forge-pr-snapshot-head-ref snapshot)
       :base-repo (+forge-pr-snapshot-base-repo snapshot)
       :head-repo (+forge-pr-snapshot-head-repo snapshot)
       :head-user (+forge-pr-snapshot-head-user snapshot)
       :cross-repo-p (+forge-pr-snapshot-cross-repo-p snapshot)
       :base-oid base-oid
       :head-oid head
       :merge-base-oid merge-base
       :mirror mirror
       :commits commits
       :files files
       :target target
       :cache-generation (plist-get st :generation)
       :last-success (plist-get st :last-success)
       :sync-status st))))

(defun +git-pr--load (number &optional root)
  "Load PR NUMBER from the current repository ROOT into a `+git-pr'."
  (let* ((root (+git-pr--resolve-root root))
         (ctx (+git-store-context-for-root root))
         (repository-id (+git-store-local-context-repository-id ctx))
         (context-id (+git-store-local-context-context-id ctx))
         (snapshot (+forge-get-pr-snapshot repository-id number)))
    (+git-pr--build repository-id number root context-id snapshot)))

(defun +git-pr--model-current-p (model)
  "Return non-nil when MODEL still matches the published mirror generation.
Object checks also protect against an interrupted/recovered publication whose
replacement mirror no longer contains an old cached range."
  (let* ((repository-id (+git-pr-repository-id model))
         (status (+git-sync-status repository-id))
         (mirror (plist-get status :mirror)))
    (and (equal (+git-pr-cache-generation model)
                (plist-get status :generation))
         (stringp mirror)
         (file-directory-p mirror)
         (equal (file-truename mirror)
                (file-truename (+git-pr-mirror model)))
         ;; Validate the exact rendered range in one process.  Unlike
         ;; `rev-parse', this fails when either object is absent.
         (eq 0
             (car (+git-review--call-git
                   mirror "merge-base" "--is-ancestor"
                   (+git-pr-merge-base-oid model)
                   (+git-pr-head-oid model)))))))

(defun +git-pr--ensure-current-model (&optional refresh-view)
  "Return a PR model valid for the currently published mirror.
When the buffer-local model is stale, rebuild it from the local Forge cache
and mirror, update the target, and optionally REFRESH-VIEW.  This function
never fetches or performs network I/O."
  (let ((old (or +git-pr--model
                 (user-error "No PR model"))))
    (unless (+git-pr--model-current-p old)
      (let* ((repository-id (+git-pr-repository-id old))
             (preferred (+git-review-target-root (+git-pr-target old)))
             (root (+git-pr--resolve-root-for-repository
                    repository-id preferred))
             (new (+git-pr--load (+git-pr-number old) root)))
        (setq-local +git-pr--model new)
        (when (and (bound-and-true-p +git-review-target)
                   (eq (+git-review-target-scope +git-review-target)
                       'pullreq))
          (setq-local +git-review-target (+git-pr-target new)))
        (+git-review--adopt-reopening-context (+git-pr-target new))
        (when refresh-view
          (cond
           ((derived-mode-p '+git-pr-mode)
            (+git-pr-refresh-buffer))
           ((derived-mode-p '+git-changes-tree-mode)
            (magit-refresh))
           ((derived-mode-p 'magit-diff-mode)
            (+git-review--sync-magit-diff-state
             (+git-pr-target new)
             (and (boundp '+git-review-file-path)
                  +git-review-file-path))
            (magit-refresh))))
        (message "PR #%s updated to mirror generation %s"
                 (+git-pr-number new)
                 (+git-pr-cache-generation new))))
    +git-pr--model))

;; =============================================================================
;; Buffer identity / reuse
;; =============================================================================

(defun +git-pr--buffer-name (repository-id number)
  "Return the deterministic PR workspace buffer name."
  (format "*PR %s#%d*" repository-id number))

(defun +git-pr--find-workspace (repository-id number)
  "Return the live shared PR workspace for REPOSITORY-ID and NUMBER."
  (let ((name (+git-pr--buffer-name repository-id number)))
    (cl-find-if
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (and (derived-mode-p '+git-pr-mode)
                   (bound-and-true-p +git-pr--model)
                   (equal (+git-pr-repository-id +git-pr--model)
                          repository-id)
                   (= (+git-pr-number +git-pr--model) number)))))
     (cons (get-buffer name) (buffer-list)))))

(defun +git-pr--attach (model &optional file-path)
  "Attach MODEL to the current buffer as a PR review buffer."
  (setq-local +git-pr--model model)
  (setq-local +git-review-target (+git-pr-target model))
  (when file-path
    (setq-local +git-review-file-path file-path))
  (+git-review--adopt-reopening-context (+git-pr-target model))
  (+git-review--install-immutable-guards)
  (+git-review-buffer-mode 1)
  (+git-review--enter-normal-state)
  (+git-store-remember-shared-buffer
   (+git-pr-repository-id model)
   (current-buffer)))

;; =============================================================================
;; Overview rendering
;; =============================================================================

(defun +git-pr--state-word (model)
  "Return an ASCII state word for MODEL."
  (let ((state (or (+git-pr-state model) 'open)))
    (cond
     ((+git-pr-draft model) "DRAFT")
     ((eq state 'merged) "MERGED")
     ((memq state '(closed rejected)) "CLOSED")
     ((eq state 'open) "OPEN")
     (t (upcase (symbol-name state))))))

(defun +git-pr--review-status-word (model)
  "Return a short review/approval word; never Forge inbox status."
  (let* ((snap (+git-pr-snapshot model))
         (st (and snap (+forge-pr-snapshot-review-status snap))))
    (pcase st
      ('approved "approved")
      ('changes-requested "changes requested")
      ('commented "commented")
      ('reviewed "reviewed")
      (_ "review status unavailable"))))

(defun +git-pr--resolve-root-for-repository (repository-id &optional preferred)
  "Return a usable local root for REPOSITORY-ID.
Prefer PREFERRED when it still exists; otherwise any live registered
same-repository context.  Signals when none remain."
  (cond
   ((and (stringp preferred)
         (file-directory-p preferred))
    preferred)
   (t
    (let ((ctx (+git-store-default-edit-context repository-id nil nil)))
      (unless ctx
        (user-error
         (concat "No live local context for %s. "
                 "Open a clone of this repository, then retry.")
         repository-id))
      (+git-store-local-context-root ctx)))))

(defun +git-pr--age-string (last-success)
  "Return a compact age string for LAST-SUCCESS epoch seconds."
  (cond
   ((not (numberp last-success)) "never")
   (t
    (let ((secs (max 0 (floor (- (float-time) last-success)))))
      (cond
       ((< secs 60) (format "%ds ago" secs))
       ((< secs 3600) (format "%dm ago" (/ secs 60)))
       ((< secs 86400) (format "%dh ago" (/ secs 3600)))
       (t (format "%dd ago" (/ secs 86400))))))))

(defun +git-pr--cache-line (model)
  "Return the Cache chrome line for MODEL."
  (let* ((st (+git-pr-sync-status model))
         (gen (or (+git-pr-cache-generation model) 0))
         (forge (plist-get st :forge))
         (stale (plist-get st :stale))
         (age (+git-pr--age-string (+git-pr-last-success model))))
    (format "Cache: generation %s | synced %s | %s | forge: %s"
            gen
            age
            (if stale "stale" "current")
            (or forge "unavailable"))))

(defun +git-pr--edit-context-line ()
  "Return the Edit context chrome line."
  (let* ((ctx-id (or (and (bound-and-true-p +git-review-edit-context-id)
                          +git-review-edit-context-id)
                     (and +git-pr--model
                          (+git-review-target-context-id
                           (+git-pr-target +git-pr--model)))))
         (ctx (and ctx-id (+git-store-get-context ctx-id)))
         (root (or (and ctx (+git-store-local-context-root ctx)) "?")))
    (format "Edit context: %s" root)))

(defun +git-pr--file-rollups (files)
  "Return (COUNT ADDS DELS REVIEWED) for FILES."
  (let ((adds 0)
        (dels 0)
        (reviewed 0)
        (map (or +git-review--reviewed-map
                 (and +git-review-target
                      (+git-review-state-load +git-review-target))
                 (make-hash-table :test #'equal))))
    (dolist (f files)
      (let ((a (+git-review-file-additions f))
            (d (+git-review-file-deletions f)))
        (when (integerp a) (setq adds (+ adds a)))
        (when (integerp d) (setq dels (+ dels d)))
        (when (+git-review--file-reviewed-p f map)
          (setq reviewed (1+ reviewed)))))
    (list (length files) adds dels reviewed)))

(defun +git-pr--insert-header (model)
  "Insert the PR overview header chrome for MODEL."
  (let* ((title (or (+git-pr-title model) ""))
         (base (or (+git-pr-base-ref model) "?"))
         (head (or (+git-pr-head-ref model) "?"))
         (head-disp
          (if (+git-pr-cross-repo-p model)
              (format "%s:%s"
                      (or (+git-pr-head-user model)
                          (+git-pr-head-repo model)
                          "fork")
                      head)
            head)))
    (insert (format "PR #%d - %s\n"
                    (+git-pr-number model) title))
    (insert (format "%s | %s | %s <- %s\n"
                    (+git-pr--state-word model)
                    (+git-pr--review-status-word model)
                    base head-disp))
    (insert (format "Repository: %s\n" (+git-pr-repository-id model)))
    (insert (+git-pr--edit-context-line) "\n")
    (insert (+git-pr--cache-line model) "\n")
    (insert "\n")))

(defun +git-pr--insert-details (model)
  "Insert collapsible OID details for MODEL."
  (magit-insert-section (pr-details nil t)
    (magit-insert-heading "Details")
    (insert (format "  base OID:  %s\n" (+git-pr-base-oid model)))
    (insert (format "  head OID:  %s\n" (+git-pr-head-oid model)))
    (insert (format "  merge-base: %s\n" (+git-pr-merge-base-oid model)))
    (insert (format "  range: %s..%s\n"
                    (+git-pr--short-oid (+git-pr-merge-base-oid model))
                    (+git-pr--short-oid (+git-pr-head-oid model))))
    (when (+git-pr-cross-repo-p model)
      (insert (format "  head repo: %s\n"
                      (or (+git-pr-head-repo model) "?"))))
    (insert "\n")))

(defun +git-pr--insert-changes (model)
  "Insert Changes progress and changed-file rows for MODEL."
  (pcase-let* ((files (+git-pr-files model))
               (`(,count ,adds ,dels ,reviewed)
                (+git-pr--file-rollups files)))
    (magit-insert-section (pr-changes nil)
      (magit-insert-heading
        (format "Changes (%d files, +%d -%d) | %d/%d reviewed"
                count adds dels reviewed count))
      (magit-insert-section (pr-changed-files nil)
        (magit-insert-heading "Changed files")
        (if (null files)
            (insert "  (no changed files)\n")
          (dolist (file files)
            (magit-insert-section section
                (file (+git-review-file-path file))
              (oset section value file)
              (magit-insert-heading
                (format "  %s  %s  %s\n"
                        (+git-review--status-glyph
                         (+git-review-file-status file))
                        (+git-review-file-path file)
                        (+git-review--format-counts
                         (+git-review-file-additions file)
                         (+git-review-file-deletions file)
                         (+git-review-file-status file))))))))
      (insert "\n"))))

(defun +git-pr--insert-commits (model)
  "Insert oldest-first commit rows for MODEL."
  (let ((commits (+git-pr-commits model)))
    (magit-insert-section (pr-commits nil)
      (magit-insert-heading
        (format "Commits (%d, oldest first)" (length commits)))
      (if (null commits)
          (insert "  (no commits)\n")
        (let ((idx 0))
          (dolist (c commits)
            (magit-insert-section section (commit (+git-pr-commit-oid c))
              (oset section value (cons idx c))
              (magit-insert-heading
                (format "  %s%s %s\n"
                        (+git-pr--short-oid (+git-pr-commit-oid c))
                        (if (+git-pr-commit-merge-p c) " (merge)" "")
                        (+git-pr-commit-subject c))))
            (setq idx (1+ idx)))))
      (insert "\n"))))

(defun +git-pr--insert-checks (_model)
  "Insert Checks placeholder (Phase 7)."
  (magit-insert-section (pr-checks nil t)
    (magit-insert-heading "Checks: unavailable (Phase 7)")
    (insert "\n")))

(defun +git-pr--insert-description (model)
  "Insert Description section for MODEL."
  (let ((body (or (and (+git-pr-snapshot model)
                       (+forge-pr-snapshot-body
                        (+git-pr-snapshot model)))
                  "")))
    (magit-insert-section (pr-description nil t)
      (magit-insert-heading "Description")
      (if (string-empty-p (string-trim body))
          (insert "  (no description)\n")
        (dolist (line (split-string body "\n"))
          (insert "  " line "\n")))
      (insert "\n"))))

(defun +git-pr--insert-conversation (model)
  "Insert Conversation posts from cached Forge objects."
  (let* ((snap (+git-pr-snapshot model))
         (posts (and snap (+forge-pr-snapshot-posts snap))))
    (magit-insert-section (pr-conversation nil t)
      (magit-insert-heading
        (format "Conversation (%d)" (length (or posts nil))))
      (if (null posts)
          (insert "  (no cached conversation)\n")
        (dolist (post posts)
          (magit-insert-section section (pr-post post)
            (oset section value post)
            (magit-insert-heading
              (format "  %s  %s\n"
                      (or (plist-get post :author) "?")
                      (or (plist-get post :created) "")))
            (let ((body (or (plist-get post :body) "")))
              (dolist (line (split-string body "\n"))
                (insert "    " line "\n"))))))
      (insert "\n"))))

(defun +git-pr-refresh-buffer ()
  "Rebuild the current PR workspace buffer contents from `+git-pr--model'."
  (let ((model +git-pr--model)
        (inhibit-read-only t))
    (unless model
      (user-error "No PR model in this buffer"))
    (setq +git-review--files (+git-pr-files model))
    (setq +git-review--reviewed-map
          (+git-review--sync-reviewed-map
           (+git-pr-files model)
           (+git-review-state-load (+git-pr-target model))))
    (erase-buffer)
    (magit-insert-section (pr-workspace nil)
      (+git-pr--insert-header model)
      (+git-pr--insert-details model)
      (+git-pr--insert-changes model)
      (+git-pr--insert-commits model)
      (+git-pr--insert-checks model)
      (+git-pr--insert-description model)
      (+git-pr--insert-conversation model))
    (goto-char (point-min))
    (setq-local buffer-read-only t)))

;; Keymap must exist before `define-derived-mode' (which would otherwise
;; create an empty map that a later `defvar' cannot replace).
(defvar +git-pr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'+git-pr-visit)
    (define-key map (kbd "RET") #'+git-pr-visit)
    (define-key map (kbd "t") #'+git-pr-open-changes-tree)
    (define-key map (kbd "c") #'+git-pr-goto-first-commit)
    (define-key map (kbd "gr") #'+git-pr-refresh)
    (define-key map (kbd "e") #'+git-review-visit-worktree)
    (define-key map (kbd "q") #'+git-review-quit)
    map)
  "Keymap for `+git-pr-mode'.")

(define-derived-mode +git-pr-mode magit-mode "Git-PR"
  "Mode for the shared pull-request review workspace."
  :interactive nil
  :group 'magit
  (magit-hack-dir-local-variables)
  (setq truncate-lines t)
  (+git-review-buffer-mode 1))

(cl-defmethod magit-buffer-value (&context (major-mode +git-pr-mode))
  (list 'git-pr
        (and +git-pr--model
             (+git-pr-repository-id +git-pr--model))
        (and +git-pr--model
             (+git-pr-number +git-pr--model))))

(defun +git-pr--setup-buffer (model)
  "Create or reuse the PR workspace buffer for MODEL and render it."
  (require 'magit)
  (let* ((repository-id (+git-pr-repository-id model))
         (number (+git-pr-number model))
         (name (+git-pr--buffer-name repository-id number))
         (existing (+git-pr--find-workspace repository-id number))
         (buf (or existing (get-buffer-create name)))
         (mirror (+git-pr-mirror model)))
    (with-current-buffer buf
      (setq-local default-directory
                  (file-name-as-directory mirror))
      (when (boundp 'magit--default-directory)
        (setq-local magit--default-directory default-directory))
      (unless (derived-mode-p '+git-pr-mode)
        (+git-pr-mode))
      (+git-pr--attach model)
      (+git-pr-refresh-buffer)
      (+git-review--enter-normal-state))
    buf))

;; =============================================================================
;; Entry point
;; =============================================================================

(defun +git/review-pull-request (number &optional root)
  "Open the shared cached PR workspace for NUMBER.
ROOT defaults to the current repository.  Never fetches.  Reuses the
same buffer across same-origin clones."
  (interactive
   (let* ((root (+git-pr--resolve-root))
          (ctx (+git-store-context-for-root root))
          (id (+git-store-local-context-repository-id ctx))
          (cands (+forge-pr-completion-candidates id))
          (input (completing-read "PR number: " cands nil nil)))
     (list (+forge-parse-pr-number input) root)))
  (let* ((number (+forge-parse-pr-number number))
         (root (+git-pr--resolve-root root))
         (ctx (+git-store-context-for-root root))
         (repository-id (+git-store-local-context-repository-id ctx))
         (existing (+git-pr--find-workspace repository-id number))
         (model nil)
         (buffer nil))
    (setq model
          (condition-case err
              (+git-pr--load number root)
            (user-error (signal (car err) (cdr err)))
            (error
             ;; Keep a previous good workspace when rebuild fails.
             (if (and existing
                      (with-current-buffer existing
                        (bound-and-true-p +git-pr--model)))
                 (progn
                   (message "PR refresh failed; keeping previous workspace: %s"
                            (error-message-string err))
                   (with-current-buffer existing +git-pr--model))
               (signal (car err) (cdr err))))))
    (setq buffer (+git-pr--setup-buffer model))
    (magit-display-buffer buffer)
    (with-current-buffer buffer
      (+git-review--adopt-reopening-context (+git-pr-target model))
      (+git-review--enter-normal-state))
    buffer))

(defalias '+git/review-pr #'+git/review-pull-request)

;; =============================================================================
;; Local refresh vs explicit sync
;; =============================================================================

(defun +git-pr-refresh ()
  "Reread Forge cache, sync state, and mirror; refresh this PR workspace.
Performs no network I/O.  Prefers the originating clone when it still
exists; otherwise rebuilds through any other live same-repository
context.  Adopts that context for `e', while PR object operations remain
rooted in the mirror."
  (interactive)
  (unless (and (bound-and-true-p +git-pr--model)
               (derived-mode-p '+git-pr-mode))
    (user-error "Not in a PR workspace"))
  (let* ((old +git-pr--model)
         (repository-id (+git-pr-repository-id old))
         (preferred (+git-review-target-root (+git-pr-target old)))
         (root (+git-pr--resolve-root-for-repository
                repository-id preferred))
         (number (+git-pr-number old))
         (new (condition-case err
                 (+git-pr--load number root)
               (error
                (message "PR refresh failed; keeping previous workspace: %s"
                         (error-message-string err))
                nil))))
    (when new
      (setq-local +git-pr--model new)
      (setq-local +git-review-target (+git-pr-target new))
      (+git-review--adopt-reopening-context (+git-pr-target new))
      (+git-pr-refresh-buffer)
      (+git-review--enter-normal-state))))

(defun +git-pr--refresh-dispatch ()
  "Refresh PR workspace or fall back to `+git-review-refresh'."
  (interactive)
  (cond
   ((derived-mode-p '+git-pr-mode) (+git-pr-refresh))
   ((and (bound-and-true-p +git-pr--model)
         (bound-and-true-p +git-review-target)
         (or (eq (+git-review-target-scope +git-review-target) 'pullreq)
             (and (fboundp '+git-review--pr-backed-p)
                  (+git-review--pr-backed-p +git-review-target))))
    ;; Tree / file / commit children: rebuild from Forge + mirror.
    (let* ((model +git-pr--model)
           (repository-id (+git-pr-repository-id model))
           (preferred (+git-review-target-root (+git-pr-target model)))
           (root (ignore-errors
                   (+git-pr--resolve-root-for-repository
                    repository-id preferred)))
           (new (and root
                     (ignore-errors
                       (+git-pr--load (+git-pr-number model) root)))))
      (when new
        (setq-local +git-pr--model new)
        (when (eq (+git-review-target-scope +git-review-target) 'pullreq)
          (setq-local +git-review-target (+git-pr-target new))))
      (+git-review-refresh)))
   (t (+git-review-refresh))))

;; =============================================================================
;; Navigation: files, commits, visit
;; =============================================================================

(defun +git-pr--file-at-point ()
  "Return `+git-review-file' at point in the PR workspace, or nil."
  (let* ((section (magit-current-section))
         (val (and section (oref section value))))
    (cond
     ((and val (+git-review-file-p val)) val)
     ((and (stringp val)
           +git-pr--model)
      (cl-find val (+git-pr-files +git-pr--model)
               :key #'+git-review-file-path :test #'equal))
     (t nil))))

(defun +git-pr--commit-at-point ()
  "Return (INDEX . +git-pr-commit) at point, or nil."
  (let* ((section (magit-current-section))
         (val (and section (oref section value))))
    (cond
     ((and (consp val) (+git-pr-commit-p (cdr val))) val)
     ((and (stringp val) +git-pr--model)
      (let ((idx 0)
            found)
        (dolist (c (+git-pr-commits +git-pr--model))
          (when (equal (+git-pr-commit-oid c) val)
            (setq found (cons idx c)))
          (setq idx (1+ idx)))
        found))
     (t nil))))

(defun +git-pr-visit (&optional other-window)
  "Visit the PR section at point.
With OTHER-WINDOW non-nil, open file diffs in the reusable review
window.  Other PR section kinds retain their normal selected-window
behavior."
  (interactive "P")
  (cond
   ((magit-section-match 'pr-changes)
    (+git-review-open-changes-tree (+git-pr-target +git-pr--model)))
   ((magit-section-match 'file)
    (let ((file (+git-pr--file-at-point)))
      (unless file (user-error "No file at point"))
      (+git-review-open-file-diff
       (+git-pr-target +git-pr--model) file other-window)))
   ((magit-section-match 'commit)
    (let ((pair (+git-pr--commit-at-point)))
      (unless pair (user-error "No commit at point"))
      (+git-pr-open-commit (car pair))))
   ((magit-section-match 'pr-post)
    (magit-section-toggle (magit-current-section)))
   (t
    (magit-section-toggle (magit-current-section)))))

(defun +git-pr-open-changes-tree ()
  "Open the Changes Tree for the current locally cached PR range.
If an explicit sync replaced the published mirror since this workspace was
rendered, rebuild the model first so Magit never receives a stale range."
  (interactive)
  (let ((model (+git-pr--ensure-current-model t)))
    (let ((buf (+git-changes-tree-setup-buffer (+git-pr-target model))))
      (with-current-buffer buf
        (setq-local +git-pr--model model)
        (setq-local +git-pr--return-buffer
                    (or +git-pr--return-buffer
                        (get-buffer
                         (+git-pr--buffer-name
                          (+git-pr-repository-id model)
                          (+git-pr-number model))))))
      buf)))

(defun +git-pr-goto-first-commit ()
  "Jump to the oldest commit row, or open it when unambiguous."
  (interactive)
  (unless (derived-mode-p '+git-pr-mode)
    (user-error "Not in a PR workspace"))
  (let ((commits (+git-pr-commits +git-pr--model)))
    (unless commits
      (user-error "No commits in this PR"))
    (goto-char (point-min))
    (if-let ((section (magit-get-section
                       '((commit) (pr-commits) (pr-workspace)))))
        (progn
          (goto-char (oref section start))
          (magit-section-update-highlight))
      (+git-pr-open-commit 0))))

(defun +git-pr--capture-workspace-return (return-buf)
  "Capture return state for RETURN-BUF before opening a PR child.
Must run before the child buffer is displayed, otherwise the saved
window configuration already contains the commit/diff layout."
  (cond
   ((not (and return-buf (buffer-live-p return-buf))) nil)
   ((eq (current-buffer) return-buf)
    (+git-review--capture-return))
   ((and (bound-and-true-p +git-review--return)
         (eq (plist-get +git-review--return :buffer) return-buf)
         (window-configuration-p
          (plist-get +git-review--return :window-config)))
    ;; Stepping between commits: keep the original workspace capture.
    +git-review--return)
   (t
    (list :buffer return-buf
          :point (with-current-buffer return-buf (point))
          :window-config nil
          :selected-window nil))))

(defun +git-pr--insert-revision-diff ()
  "Insert the selected-parent diff into a clean PR revision buffer."
  (magit--insert-diff t
    "diff" "-p" "--no-prefix"
    (and (member "--stat" magit-buffer-diff-args) "--numstat")
    magit-buffer-diff-args
    +git-pr--revision-base-oid
    +git-pr--revision-head-oid
    "--"
    magit-buffer-diff-files))

(defun +git-pr-open-commit (index &optional parent-oid)
  "Open PR commit at INDEX as a clean revision buffer.
The buffer shows the commit header, complete commit message, and diff in that
order.  It reuses a stable Magit revision buffer while stepping.
PARENT-OID selects a non-default merge parent when provided.
`q' returns to the PR workspace, not through prior commits."
  (let* ((model (or +git-pr--model
                    (user-error "No PR model")))
         (commits (+git-pr-commits model))
         (commit (nth index commits))
         (return-buf
          (or (and (derived-mode-p '+git-pr-mode) (current-buffer))
              +git-pr--return-buffer
              (+git-pr--find-workspace
               (+git-pr-repository-id model)
               (+git-pr-number model))))
         ;; Capture before opening/displaying the child buffer.
         (return-state (+git-pr--capture-workspace-return return-buf)))
    (unless commit
      (user-error "No PR commit at index %s" index))
    (when (and (+git-pr-commit-merge-p commit)
               parent-oid
               (not (member parent-oid (+git-pr-commit-parents commit))))
      (user-error "Parent %s is not a parent of this merge commit"
                  (+git-pr--short-oid parent-oid)))
    (when (and (+git-pr-commit-merge-p commit)
               (> (length (+git-pr-commit-parents commit)) 1)
               (null parent-oid)
               (null +git-pr--selected-parent)
               current-prefix-arg)
      (setq parent-oid (+git-pr-select-merge-parent commit)))
    (pcase-let* ((`(,base . ,head)
                  (+git-pr-commit-range
                   commit
                   (or parent-oid +git-pr--selected-parent)))
                 (target
                  (+git-review-make-target
                   (+git-review-target-root (+git-pr-target model))
                   'commit
                   (if (equal base +git-review-empty-tree)
                       "empty-tree"
                     (concat (+git-pr--short-oid head) "^"))
                   (+git-pr--short-oid head)
                   base head
                   (+git-pr-repository-id model)
                   (+git-review-target-context-id (+git-pr-target model))
                   ;; Mark as PR-backed so Git objects resolve via mirror.
                   (+git-pr-number model)))
                 ;; Force PR commit overview identity so stepping reuses one buffer.
                 (target
                  (let ((oid (+git-pr-commit-oid commit)))
                    (setf (+git-review-target-overview-id target)
                          (format "git-review:pr-commit:%s:%s:overview"
                                  (+git-pr-repository-id model)
                                  (+git-pr-number model)))
                    (setf (+git-review-target-family-id target)
                          (format "git-review:pr-commit:%s:%s:%s"
                                  (+git-pr-repository-id model)
                                  (+git-pr-number model)
                                  oid))
                    target))
                 (mirror (+git-pr-mirror model))
                 (args (list "--stat" "--no-ext-diff"
                             (format "-U%d" +git/review-context-lines)))
                 (sections '(magit-insert-revision-headers
                             magit-insert-revision-message
                             +git-pr--insert-revision-diff))
                 (headers-format "Author: %aN <%aE>\nDate:   %ad\n")
                 (buf nil)
                 (+git-review--suppress-return-record t))
      (let ((default-directory (file-name-as-directory mirror))
            (magit-revision-sections-hook sections)
            (magit-revision-insert-related-refs nil)
            (magit-revision-headers-format headers-format)
            (+git-pr--revision-base-oid base)
            (+git-pr--revision-head-oid head))
        (setq buf (magit-revision-setup-buffer head args nil)))
      (with-current-buffer buf
        (setq-local magit-revision-sections-hook sections)
        (setq-local magit-revision-insert-related-refs nil)
        (setq-local magit-revision-headers-format headers-format)
        (setq-local +git-pr--revision-base-oid base)
        (setq-local +git-pr--revision-head-oid head)
        (+git-review--attach-target target)
        (setq-local +git-pr--model model)
        (setq-local +git-pr--commit-index index)
        (setq-local +git-pr--selected-parent
                    (or parent-oid
                        (and (+git-pr-commit-merge-p commit)
                             (car (+git-pr-commit-parents commit)))))
        (setq-local +git-pr--return-buffer return-buf)
        (when return-state
          (+git-review--set-return buf return-state))
        (+git-review--enter-normal-state))
      (magit-display-buffer buf)
      buf)))

(defun +git-pr-select-merge-parent (commit)
  "Prompt for an explicit parent of merge COMMIT; return OID."
  (let* ((parents (+git-pr-commit-parents commit))
         (choices
          (cl-loop for p in parents
                   for i from 1
                   collect (cons (format "parent %d: %s" i
                                         (+git-pr--short-oid p))
                                 p)))
         (pick (completing-read "Merge parent: "
                                (mapcar #'car choices) nil t)))
    (cdr (assoc pick choices))))

(defun +git-pr-select-parent ()
  "Choose an explicit merge parent for the current PR commit (`P')."
  (interactive)
  (unless (and (bound-and-true-p +git-pr--model)
               (integerp +git-pr--commit-index))
    (user-error "Not in a PR commit review"))
  (let* ((commit (nth +git-pr--commit-index
                      (+git-pr-commits +git-pr--model))))
    (unless (+git-pr-commit-merge-p commit)
      (user-error "Not a merge commit"))
    (let ((parent (+git-pr-select-merge-parent commit)))
      (setq-local +git-pr--selected-parent parent)
      (+git-pr-open-commit +git-pr--commit-index parent))))

(defun +git-pr-next-commit ()
  "Open the next PR commit, replacing the selected window."
  (interactive)
  (cond
   ((and (bound-and-true-p +git-pr--model)
         (integerp +git-pr--commit-index))
    (let ((next (1+ +git-pr--commit-index))
          (n (length (+git-pr-commits +git-pr--model))))
      (when (>= next n)
        (user-error "Already at the newest PR commit"))
      (+git-pr-open-commit next)))
   ((derived-mode-p '+git-pr-mode)
    (let ((pair (+git-pr--commit-at-point)))
      (if pair
          (+git-pr-open-commit (1+ (car pair)))
        (+git-pr-open-commit 0))))
   (t (+git-review-next-commit))))

(defun +git-pr-prev-commit ()
  "Open the previous PR commit, replacing the selected window."
  (interactive)
  (cond
   ((and (bound-and-true-p +git-pr--model)
         (integerp +git-pr--commit-index))
    (when (<= +git-pr--commit-index 0)
      (user-error "Already at the oldest PR commit"))
    (+git-pr-open-commit (1- +git-pr--commit-index)))
   ((derived-mode-p '+git-pr-mode)
    (let ((pair (+git-pr--commit-at-point)))
      (if (and pair (> (car pair) 0))
          (+git-pr-open-commit (1- (car pair)))
        (user-error "No previous PR commit"))))
   (t (+git-review-prev-commit))))

(defun +git-pr-quit ()
  "Return from a PR child buffer to the PR workspace when recorded.
Falls back to `+git-review-quit' for ordinary review buffers.
Always selects the workspace window after restore so `q' cannot leave
the commit/diff buffer displayed."
  (interactive)
  (cond
   ((and (bound-and-true-p +git-pr--return-buffer)
         (buffer-live-p +git-pr--return-buffer)
         (not (derived-mode-p '+git-pr-mode)))
    (let ((state +git-review--return)
          (ret +git-pr--return-buffer)
          (child (current-buffer)))
      (setq-local +git-review--return nil)
      (setq-local +git-pr--return-buffer nil)
      (when (and state
                 (window-configuration-p (plist-get state :window-config)))
        (set-window-configuration (plist-get state :window-config)))
      (when (buffer-live-p ret)
        (let ((win (or (get-buffer-window ret t)
                       (and (window-live-p (plist-get state :selected-window))
                            (plist-get state :selected-window))
                       (selected-window))))
          (set-window-buffer win ret)
          (select-window win)
          (when (integer-or-marker-p (plist-get state :point))
            (with-current-buffer ret
              (goto-char (min (max (point-min) (plist-get state :point))
                              (point-max)))))))
      (when (and (buffer-live-p child)
                 (not (eq child ret))
                 (not (get-buffer-window child t)))
        (bury-buffer child))))
   (t (+git-review-quit))))

;; =============================================================================
;; Conversation / composition (Forge native; no submit in tests)
;; =============================================================================

(defun +git-pr-reply ()
  "Open a native Forge reply composition buffer for the current PR.
Opening is local and performs no network call.  Submission remains an
explicit Forge action."
  (interactive)
  (let* ((model (or +git-pr--model (user-error "No PR model")))
         (obj (+git-pr-forge-object model)))
    (unless obj
      (user-error
       "No native Forge object available for reply in this workspace"))
    (require 'forge)
    (require 'forge-commands)
    ;; Isolate the private composition setup Forge itself uses.
    (forge--setup-post-buffer obj #'forge--submit-create-post
      "%i;new-comment" "New comment on #%i of %p")
    (when (and (boundp 'evil-mode) evil-mode
               (fboundp 'evil-insert-state))
      (evil-insert-state))))

(defun +git-pr-edit-post ()
  "Edit the conversation post at point through Forge when possible."
  (interactive)
  (let* ((section (magit-current-section))
         (val (and section (oref section value)))
         (obj (or (and (listp val) (plist-get val :forge-object))
                  (and +git-pr--model
                       (magit-section-match 'pr-description)
                       (+git-pr-forge-object +git-pr--model)))))
    (unless obj
      (user-error "No editable Forge post at point"))
    (require 'forge)
    (require 'forge-commands)
    (cond
     ((forge-pullreq-p obj)
      (forge--setup-post-buffer obj #'forge--submit-edit-post
        "%i" "Edit #%i of %p" nil
        (lambda ()
          (insert "# " (oref obj title) "\n\n")
          (insert (or (oref obj body) "")))))
     (t
      (forge--setup-post-buffer obj #'forge--submit-edit-post
        "%i;%I" "Edit comment on #%i of %p" nil
        (lambda ()
          (insert (or (oref obj body) ""))))))
    (when (and (boundp 'evil-mode) evil-mode
               (fboundp 'evil-insert-state))
      (evil-insert-state))))

;; =============================================================================
;; Key bindings (Evil; base map is defined above `+git-pr-mode')
;; =============================================================================

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +git-pr-mode-map
      (kbd "RET") #'+git-pr-visit
      "t" #'+git-pr-open-changes-tree
      "c" #'+git-pr-goto-first-commit
      "gc" #'+git-pr-next-commit
      "gC" #'+git-pr-prev-commit
      "gr" #'+git-pr-refresh
      "e" #'+git-review-visit-worktree
      "q" #'+git-review-quit)
    (evil-set-initial-state '+git-pr-mode 'normal))
  ;; PR commit / tree children: prefer PR commit stepping and return.
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +git-review-buffer-mode-map
      "gc" #'+git-pr-next-commit
      "gC" #'+git-pr-prev-commit
      "gr" #'+git-pr--refresh-dispatch
      "q" #'+git-pr-quit)))

(add-hook '+git-pr-mode-hook #'+git-review--maybe-enable)

(provide 'init-git-pr)

;;; init-git-pr.el ends here
