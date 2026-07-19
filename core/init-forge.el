;;; init-forge.el --- Forge package ownership and cached PR adapter -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Forge package/configuration and narrow cached metadata adapter

;;; Commentary:
;; Phase 5 ownership: Forge lives here so Magit (`init-git.el') and the PR
;; workspace (`init-git-pr.el') stay separate.  This module never contacts a
;; network endpoint.  It only reads Forge's local Closql database through
;; public object APIs (`forge-get-repository', `forge-get-pullreq', `oref').
;;
;; There is no handwritten Forge SQL and no second PR metadata store.
;; Tests must bind `forge-database-file' to a temporary path or inject
;; `+forge-pr-lookup-function' / `+forge-pr-list-function'.
;;
;; Forge 0.6.7 requires cond-let >= 1.1 (`and$' / `thread$' threading).  The
;; pinned cond-let 0.2 still exposes the multi-form API as `cond-let--and>'.
;; Compatibility is installed at module load, before `use-package' forge, so
;; direct autoloads (`forge-add-repository') and `(require 'forge)' succeed
;; without calling `+forge--ensure-forge-apis'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; =============================================================================
;; cond-let compatibility (must run before any Forge load)
;; =============================================================================

(defvar +forge--load-error nil
  "Last Forge load error string, or nil when load succeeded / not attempted.")

(defun +forge--ensure-cond-let-compat ()
  "Bridge pinned cond-let 0.2 so Forge 0.6.7 can macroexpand.
Forge's `and$' shorthand expects the multi-form threading API that
cond-let 1.1 places on `cond-let--and$'.  Pinned 0.2 keeps that API on
`cond-let--and>' and lacks `cond-let--thread$'.  Redefine only when the
1.1 symbols are missing; do not touch Straight pins."
  (require 'cond-let nil t)
  (unless (fboundp 'cond-let--thread$)
    ;; Promote 0.2's multi-form `and>' onto the `and$' symbol Forge expands.
    (when (fboundp 'cond-let--and>)
      (defalias 'cond-let--and$ (symbol-function 'cond-let--and>)))
    (eval
     '(defmacro cond-let--thread$ (form form2 &rest forms)
        "Bind `$' to each FORM before evaluating the next (cond-let 1.1 API)."
        (declare (indent 0) (debug t))
        `(,(if forms 'let* 'let)
          (($ ,form)
           ,@(and forms
                  (mapcar (lambda (form) `($ ,form))
                          (cons form2 (butlast forms)))))
          ,(or (car (last forms)) form2)))
     t))
  (fboundp 'cond-let--thread$))

;; Install immediately so every later Forge entry point is protected.
(+forge--ensure-cond-let-compat)

;; =============================================================================
;; Forge package (single owner)
;; =============================================================================
;; Topic metadata remains cached in forge-database.sqlite under
;; `user-emacs-directory'.  Phase 1 removed on-visit fetch advice: visiting a
;; cached Forge topic never triggers a hidden fetch.

(use-package forge
  :straight t
  :after magit
  :defer t
  :init
  ;; Re-assert compat before any deferred/autoloaded Forge load.
  (+forge--ensure-cond-let-compat)
  ;; Skip forge's default binding injection: in current magit, the
  ;; transient slot it targets (`"o"' in magit-dispatch) has moved or
  ;; been removed.  Setting this in `:init' runs before forge loads.
  (setq forge-add-default-bindings nil))

;; =============================================================================
;; Optional 1Password token provider
;; =============================================================================

(defcustom +forge-1password-token-references
  (when-let ((reference (getenv "FORGE_GITHUB_TOKEN_OP_REF")))
    `(("api.github.com" . ,reference)))
  "Alist mapping Forge API hosts to 1Password secret references.

Example:

  ((\"api.github.com\" . \"op://Employee/GitHub Forge/token\"))

Only the non-secret `op://` reference is stored in Emacs configuration.
When Ghub cannot find its PACKAGE token through ordinary Auth Source,
Emacs runs `op read REFERENCE' and caches the returned token in memory
for the rest of the Emacs session.  Set the environment variable
FORGE_GITHUB_TOKEN_OP_REF for the common Github.com case instead of
customizing this variable."
  :type '(alist :key-type string :value-type string)
  :group 'magit)

(defcustom +forge-1password-cli-program "op"
  "1Password CLI executable used to resolve Forge token references."
  :type 'string
  :group 'magit)

(defvar +forge--1password-token-cache (make-hash-table :test #'equal)
  "Session-only cache of tokens returned by 1Password.
Keys are secret references.  Values are never persisted by this module.")

(defun +forge--1password-reference (host)
  "Return the configured 1Password secret reference for HOST."
  (or (cdr (assoc-string host +forge-1password-token-references t))
      (and (string-prefix-p "api." host)
           (cdr (assoc-string
                 (string-remove-prefix "api." host)
                 +forge-1password-token-references t)))))

(defun +forge--1password-read-token (host)
  "Read HOST's Forge token from 1Password and cache it for this session.
The token is captured from `op' stdout and is never placed in argv,
messages, process logs, or a file by this module."
  (let* ((reference (+forge--1password-reference host))
         (cached (and reference
                      (gethash reference +forge--1password-token-cache))))
    (cond
     (cached cached)
     ((null reference) nil)
     ((not (string-prefix-p "op://" reference))
      (user-error "Forge 1Password reference must begin with op://"))
     ((not (executable-find +forge-1password-cli-program))
      (user-error
       "1Password CLI `%s' is unavailable; install/sign in to `op' or remove the Forge op:// reference"
       +forge-1password-cli-program))
     (t
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix)
              (coding-system-for-write 'utf-8-unix)
              (status
               (process-file +forge-1password-cli-program nil t nil
                             "read" reference)))
          (if (not (and (integerp status) (zerop status)))
              (let ((detail (string-trim (buffer-string))))
                (erase-buffer)
                (user-error "1Password could not provide the Forge token%s"
                            (if (string-empty-p detail)
                                ""
                              (format ": %s" detail))))
            (let ((token (string-trim-right (buffer-string))))
              (erase-buffer)
              (when (string-empty-p token)
                (user-error "1Password returned an empty Forge token"))
              (puthash reference token +forge--1password-token-cache)
              token))))))))

(defun +forge-1password-clear-token-cache ()
  "Forget all Forge tokens fetched from 1Password in this Emacs session."
  (interactive)
  (clrhash +forge--1password-token-cache)
  (require 'auth-source)
  (auth-source-forget-all-cached)
  (message "Forgot session-cached Forge tokens"))

(defun +forge--ghub-token-with-1password
    (original host username package &optional nocreate forge)
  "Call ORIGINAL Ghub token lookup, falling back to 1Password.
HOST, USERNAME, PACKAGE, NOCREATE, and FORGE are Ghub's private token
lookup arguments.  Ordinary Auth Source remains authoritative.  The
1Password provider runs only for PACKAGE `forge' and a configured HOST."
  (or (funcall original host username package t forge)
      (and (eq package 'forge)
           (+forge--1password-reference host)
           (+forge--1password-read-token host))
      (funcall original host username package nocreate forge)))

(defun +forge--install-1password-ghub-advice ()
  "Install the narrow Ghub token-provider adapter once."
  (when (and (fboundp 'ghub--token)
             (not (advice-member-p #'+forge--ghub-token-with-1password
                                   'ghub--token)))
    ;; Private Ghub seam, isolated here because Ghub has no public token
    ;; provider hook.  This does not alter requests or authentication when no
    ;; 1Password reference is configured.
    (advice-add 'ghub--token :around
                #'+forge--ghub-token-with-1password)))

(with-eval-after-load 'ghub
  (+forge--install-1password-ghub-advice))

;; =============================================================================
;; Cached PR snapshot
;; =============================================================================

(cl-defstruct (+forge-pr-snapshot
               (:constructor +forge-pr-snapshot-create)
               (:copier nil))
  "Plain, inspectable PR metadata snapshot for the review workspace.
Retains the underlying Forge object for native comment/edit actions."
  repository-id          ; canonical store id (github.com/org/project)
  number                 ; integer
  title
  state                  ; open|merged|closed|rejected|...
  draft                  ; boolean
  author
  review-status          ; approval/review summary when known; never inbox status
  inbox-status           ; Forge topic inbox status (unread/pending/done)
  base-ref
  base-repo
  base-rev               ; OID string or nil
  head-ref
  head-user
  head-repo
  head-rev               ; OID string or nil
  cross-repo-p
  created
  updated
  closed
  merged
  body
  posts                  ; list of post/review plists (preserve :forge-object)
  forge-object)          ; native forge-pullreq handle (or nil for stubs)

(defvar +forge-pr-lookup-function #'+forge-lookup-pr-default
  "Function used to look up a cached PR snapshot.
Called as (FN REPOSITORY-ID NUMBER).  Must return a `+forge-pr-snapshot'
or signal `user-error'.  Tests may bind a deterministic stub.")

(defvar +forge-pr-list-function #'+forge-list-prs-default
  "Function used to list cached PR snapshots for completion.
Called as (FN REPOSITORY-ID).  Must return a list of `+forge-pr-snapshot'.
Never contacts the network.")

;; ---------------------------------------------------------------------------
;; Canonical id <-> Forge host/owner/name
;; ---------------------------------------------------------------------------

(defun +forge--parse-canonical-id (repository-id)
  "Return (HOST OWNER NAME) for REPOSITORY-ID, or nil when unparsable.
Accepts store identities such as `github.com/org/project' and nested
GitLab paths such as `gitlab.com/group/subgroup/project' (OWNER may
contain `/')."
  (when (and (stringp repository-id)
             (not (string-prefix-p "local:" repository-id)))
    (let* ((parts (split-string repository-id "/" t))
           (host (car parts))
           (name (car (last parts)))
           (owner (and (> (length parts) 2)
                       (mapconcat #'identity (butlast (cdr parts)) "/"))))
      (when (and host owner name
                 (>= (length parts) 3)
                 (string-match-p "\\." host))
        (list host owner name)))))

(defun +forge--ensure-forge-apis ()
  "Ensure Forge object APIs are loadable; return non-nil on success.
Applies cond-let compatibility, then loads `forge'.  Never fetches
or writes the owner's Forge database.  On failure, records
`+forge--load-error' and returns nil."
  (or (fboundp 'forge-get-pullreq)
      (progn
        (setq +forge--load-error nil)
        (+forge--ensure-cond-let-compat)
        (require 'magit nil t)
        (condition-case err
            (progn
              (require 'forge)
              (and (fboundp 'forge-get-pullreq)
                   (progn (setq +forge--load-error nil) t)))
          (error
           (setq +forge--load-error (error-message-string err))
           nil)))))

(defun +forge--forge-repo-for-canonical (repository-id &optional demand)
  "Return the Forge repository object for REPOSITORY-ID, or nil.
DEMAND defaults to `:known?'.  Never inserts, tracks, or fetches."
  (when (+forge--ensure-forge-apis)
    (pcase-let ((`(,host ,owner ,name)
                 (+forge--parse-canonical-id repository-id)))
      (when host
        (ignore-errors
          (forge-get-repository (list host owner name) nil
                                (or demand :known?)))))))

(defun +forge--slot (obj slot &optional default)
  "Return SLOT of OBJ via `oref', or DEFAULT when unavailable.
Closql-backed Forge objects can signal `unbound-slot' for sparse rows;
never let that escape into the PR workspace."
  (condition-case _
      (if (and obj (eieio-object-p obj)
               (slot-exists-p obj slot))
          (let ((val (eieio-oref obj slot)))
            (if (eq val eieio-unbound) default val))
        default)
    (unbound-slot default)
    (error default)))

(defun +forge--post-plist (post &optional kind)
  "Return an inspectable plist for Forge POST, preserving the object.
KIND is `post' (default) or `review'."
  (list :id (+forge--slot post 'id)
        :number (+forge--slot post 'number)
        :author (+forge--slot post 'author)
        :created (+forge--slot post 'created)
        :updated (+forge--slot post 'updated)
        :body (+forge--slot post 'body "")
        :kind (or kind 'post)
        :state (+forge--slot post 'state)
        :forge-object post))

(defun +forge--review-status-from-pullreq (pullreq)
  "Return a review/approval summary for PULLREQ, never inbox status.
Uses cached `reviews' when present; returns nil when unknown."
  (let ((reviews
         (condition-case _
             (or (+forge--slot pullreq 'reviews) nil)
           (error nil))))
    (when reviews
      (let ((states
             (delq nil
                   (mapcar (lambda (r) (+forge--slot r 'state)) reviews))))
        (cond
         ((cl-find 'APPROVED states :test #'equal) 'approved)
         ((cl-find "APPROVED" states :test #'equal) 'approved)
         ((cl-find 'CHANGES_REQUESTED states :test #'equal) 'changes-requested)
         ((cl-find "CHANGES_REQUESTED" states :test #'equal) 'changes-requested)
         ((cl-find 'COMMENTED states :test #'equal) 'commented)
         ((cl-find "COMMENTED" states :test #'equal) 'commented)
         (t 'reviewed))))))

(defun +forge--conversation-entries (pullreq)
  "Return post+review plists for PULLREQ conversation rendering."
  (let* ((posts
          (condition-case _
              (mapcar #'+forge--post-plist
                      (or (+forge--slot pullreq 'posts) nil))
            (error nil)))
         (reviews
          (condition-case _
              (mapcar (lambda (r) (+forge--post-plist r 'review))
                      (or (+forge--slot pullreq 'reviews) nil))
            (error nil))))
    (append (or posts nil) (or reviews nil))))

(defun +forge--snapshot-from-pullreq (repository-id pullreq)
  "Build a `+forge-pr-snapshot' for REPOSITORY-ID from Forge PULLREQ."
  (let* ((state (+forge--slot pullreq 'state))
         (draft (+forge--slot pullreq 'draft-p)))
    (+forge-pr-snapshot-create
     :repository-id repository-id
     :number (+forge--slot pullreq 'number)
     :title (or (+forge--slot pullreq 'title) "")
     :state (or state 'open)
     :draft (and draft t)
     :author (+forge--slot pullreq 'author)
     :review-status (+forge--review-status-from-pullreq pullreq)
     :inbox-status (+forge--slot pullreq 'status)
     :base-ref (+forge--slot pullreq 'base-ref)
     :base-repo (+forge--slot pullreq 'base-repo)
     :base-rev (+forge--slot pullreq 'base-rev)
     :head-ref (+forge--slot pullreq 'head-ref)
     :head-user (+forge--slot pullreq 'head-user)
     :head-repo (+forge--slot pullreq 'head-repo)
     :head-rev (+forge--slot pullreq 'head-rev)
     :cross-repo-p (and (+forge--slot pullreq 'cross-repo-p) t)
     :created (+forge--slot pullreq 'created)
     :updated (+forge--slot pullreq 'updated)
     :closed (+forge--slot pullreq 'closed)
     :merged (+forge--slot pullreq 'merged)
     :body (or (+forge--slot pullreq 'body) "")
     :posts (+forge--conversation-entries pullreq)
     :forge-object pullreq)))

(defun +forge-lookup-pr-default (repository-id number)
  "Look up PR NUMBER for REPOSITORY-ID through Forge object APIs.
Signals actionable `user-error' when the repository is untracked or the
PR is absent from the local Forge cache.  Never fetches."
  (unless (and (integerp number) (> number 0))
    (user-error "Invalid PR number: %s" number))
  (unless (+forge--ensure-forge-apis)
    (user-error
     (concat "Forge is not available"
             (if +forge--load-error
                 (format " (%s). " +forge--load-error)
               ". ")
             "Install/load Forge, run M-x forge-add-repository once, then C-c g f.")))
  (let ((repo (+forge--forge-repo-for-canonical repository-id :known?)))
    (unless repo
      (user-error
       (concat "Repository is not tracked by Forge. "
               "Run M-x forge-add-repository once, then C-c g f.")))
    (unless (eq (+forge--slot repo 'condition) :tracked)
      (user-error
       (concat "Repository is not tracked by Forge. "
               "Run M-x forge-add-repository once, then C-c g f.")))
    (let ((pullreq (forge-get-pullreq repo number)))
      (unless pullreq
        (user-error
         "PR #%s is not cached. Run C-c g f, wait for sync, then retry."
         number))
      (+forge--snapshot-from-pullreq repository-id pullreq))))

(defun +forge-list-prs-default (repository-id)
  "Return cached PR snapshots for REPOSITORY-ID via Forge object relations.
Uses `(oref REPO pullreqs)'; never runs handwritten SQL."
  (when (+forge--ensure-forge-apis)
    (let ((repo (+forge--forge-repo-for-canonical repository-id :known?)))
      (if (not (and repo (eq (+forge--slot repo 'condition) :tracked)))
          nil
        (condition-case _
            (mapcar (lambda (pr)
                      (+forge--snapshot-from-pullreq repository-id pr))
                    (or (+forge--slot repo 'pullreqs) nil))
          (error nil))))))

(defun +forge-get-pr-snapshot (repository-id number)
  "Return a `+forge-pr-snapshot' for REPOSITORY-ID and NUMBER.
Dispatches through `+forge-pr-lookup-function'."
  (funcall +forge-pr-lookup-function repository-id number))

(defun +forge-list-pr-snapshots (repository-id)
  "Return cached PR snapshots for REPOSITORY-ID for completion."
  (funcall +forge-pr-list-function repository-id))

(defun +forge-pr-completion-candidates (repository-id)
  "Return completion strings for cached PRs in REPOSITORY-ID.
Format: \"#16  MERGED  title\".  Direct numeric input remains valid even
when a PR is absent from this list."
  (mapcar
   (lambda (snap)
     (format "#%d  %s  %s"
             (+forge-pr-snapshot-number snap)
             (upcase (symbol-name
                      (or (+forge-pr-snapshot-state snap) 'open)))
             (or (+forge-pr-snapshot-title snap) "")))
   (+forge-list-pr-snapshots repository-id)))

(defun +forge-parse-pr-number (input)
  "Parse INPUT into a positive PR number.
Accepts integers, bare digits, `#16', and completion strings such as
`#16  MERGED  title'.  Signals `user-error' on invalid input."
  (cond
   ((and (integerp input) (> input 0)) input)
   ((and (stringp input)
         (string-match "\\`[#]?\\([0-9]+\\)\\(?:\\s-\\|$\\)"
                       (string-trim input)))
    (let ((n (string-to-number (match-string 1 (string-trim input)))))
      (if (> n 0) n
        (user-error "Invalid PR number: %s" input))))
   (t (user-error "Invalid PR number: %s" input))))

(provide 'init-forge)

;;; init-forge.el ends here
