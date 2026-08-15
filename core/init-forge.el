;;; init-forge.el --- Forge package ownership and cached PR adapter -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Forge package/configuration and narrow cached metadata adapter

;;; Commentary:
;; Phase 5 ownership: Forge lives here so Magit (`init-git.el') and the PR
;; workspace (`init-git-pr.el') stay separate.  Normal workspace reads use
;; only Forge's local Closql database through public object APIs
;; (`forge-get-repository', `forge-get-pullreq', `oref').  The sole network
;; boundary is repository registration inside an explicit Phase 4 sync job.
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

;; Set this before `use-package' expansion.  Evil Collection can autoload Forge
;; while its own setup runs, which is earlier than a deferred package's
;; `:init' form on some installations.
(setq forge-add-default-bindings nil)

(use-package forge
  :straight t
  :after magit
  :defer t
  :init
  ;; Re-assert compat before any deferred/autoloaded Forge load.
  (+forge--ensure-cond-let-compat)
  ;; Reassert the setting for reloads and unusual package-loading orders.
  (setq forge-add-default-bindings nil))

;; =============================================================================
;; Native Forge credential providers
;; =============================================================================

(defcustom +forge-allow-auth-source (not (eq system-type 'darwin))
  "Whether Forge may read or create tokens through ordinary Auth Source.

The default is non-nil on Linux and WSL, where an encrypted
`~/.authinfo.gpg' is the recommended persistent credential store.  It is nil
on macOS because `+forge-use-macos-keychain' uses the system Keychain instead.
This setting is narrow: other Ghub packages keep their normal Auth Source
behavior."
  :type 'boolean
  :group 'magit)

(defcustom +forge-use-macos-keychain (eq system-type 'darwin)
  "Whether Forge may retrieve its token from macOS Keychain.

This dedicated provider reads only an Internet Password item matching the
Forge API host and Ghub account name (`USERNAME^forge').  It does not enable
ordinary Auth Source files.  The default is non-nil on macOS and nil on
Linux/WSL, where ordinary Auth Source is enabled by default."
  :type 'boolean
  :group 'magit)

(defcustom +forge-macos-keychain-label "Emacs Forge GitHub token"
  "Label used for the Forge Internet Password item in macOS Keychain."
  :type 'string
  :group 'magit)

(defvar +forge--session-token-cache (make-hash-table :test #'equal)
  "Session-only Forge tokens keyed by API host.
This is the temporary no-disk fallback on every platform.")

(defun +forge--ghub-account (username)
  "Return Ghub's Forge account name for USERNAME."
  (format "%s^forge" username))

(defun +forge--macos-keychain-token (host username)
  "Return the Forge token for HOST and USERNAME from macOS Keychain.
Return nil outside macOS, when the provider is disabled, or when no matching
Internet Password item exists."
  (when (and +forge-use-macos-keychain
             (eq system-type 'darwin)
             (stringp username)
             (not (string-empty-p username)))
    (require 'auth-source)
    (let* ((auth-sources '(macos-keychain-internet))
           (entry (car (auth-source-search
                        :max 1
                        :host host
                        :user (+forge--ghub-account username)
                        :require '(:secret))))
           (secret (and entry (plist-get entry :secret))))
      (cond
       ((functionp secret) (funcall secret))
       ((stringp secret) secret)
       (t nil)))))

(defun +forge--configured-github-username ()
  "Return the configured GitHub username, or nil.
This is a local Git configuration lookup and never contacts GitHub."
  (or (and (fboundp 'ghub--username)
           (ignore-errors (ghub--username "api.github.com" 'github)))
      (car (ignore-errors
             (process-lines "git" "config" "--get" "github.user")))))

(defun +forge-store-token-in-macos-keychain (&optional host username)
  "Store a Forge token for HOST and USERNAME in macOS Keychain.

The token is read with `read-passwd' and sent to Apple's `security' command
through standard input.  It is never placed in process arguments, a file,
minibuffer history, the kill ring, or messages by this module.  The Keychain
account is Ghub's `USERNAME^forge' identity.  An existing matching item is
updated."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "macOS Keychain is available only on macOS"))
  (unless (file-executable-p "/usr/bin/security")
    (user-error "Apple's /usr/bin/security command is unavailable"))
  (let* ((host (or host "api.github.com"))
         (username
          (or username
              (+forge--configured-github-username)
              (read-string "GitHub username: "))))
    (when (string-empty-p username)
      (user-error "GitHub username cannot be empty"))
    (let* ((account (+forge--ghub-account username))
           (token (read-passwd (format "Forge token for %s: " host))))
      (when (string-empty-p token)
        (user-error "Forge token cannot be empty"))
      (let ((output (generate-new-buffer " *forge-keychain-output*"))
            status detail)
        (unwind-protect
            (progn
              (with-temp-buffer
                ;; `security ... -w' asks for the password and confirmation.
                ;; Supply the same hidden value twice through stdin.
                (insert token "\n" token "\n")
                (setq status
                      (call-process-region
                       (point-min) (point-max)
                       "/usr/bin/security" t output nil
                       "add-internet-password"
                       "-U"
                       "-a" account
                       "-s" host
                       "-l" +forge-macos-keychain-label
                       "-w")))
              (with-current-buffer output
                (setq detail (string-trim (buffer-string))))
              (unless (and (integerp status) (zerop status))
                (user-error
                 "Could not store the Forge token in macOS Keychain%s"
                 (if (string-empty-p detail)
                     ""
                   (format ": %s" detail))))
              (require 'auth-source)
              (auth-source-forget-all-cached)
              (message
               "Stored Forge credential in macOS Keychain for %s (%s)"
               host account)
              t)
          (when (buffer-live-p output)
            (kill-buffer output))
          (when (stringp token)
            (clear-string token)))))))

(defun +forge-set-session-token (&optional host)
  "Read a Forge token for HOST into memory for this Emacs session only.

This is a temporary fallback when no persistent platform credential store is
available.  The value is not written to a file, customization, the kill ring,
minibuffer history, messages, or process arguments by this module."
  (interactive)
  (let* ((host (or host "api.github.com"))
         (token (read-passwd (format "Forge token for %s: " host))))
    (when (string-empty-p token)
      (user-error "Forge token cannot be empty"))
    (puthash host token +forge--session-token-cache)
    (message "Forge token cached in memory for %s until Emacs exits" host)
    t))

(defun +forge-clear-token-cache ()
  "Forget session and Auth Source cached Forge tokens."
  (interactive)
  (clrhash +forge--session-token-cache)
  (require 'auth-source)
  (auth-source-forget-all-cached)
  (message "Forgot session-cached Forge tokens"))

(defun +forge--ghub-token-from-native-store
    (original host username package &optional nocreate forge)
  "Provide a Forge token from the configured platform-native store.
HOST, USERNAME, PACKAGE, NOCREATE, and FORGE are Ghub's private token
lookup arguments.  Forge uses, in order, a manually supplied session token,
macOS Keychain, then ordinary Auth Source when enabled.  Other Ghub packages
retain ORIGINAL behavior."
  (if (not (eq package 'forge))
      (funcall original host username package nocreate forge)
    (or (gethash host +forge--session-token-cache)
        (+forge--macos-keychain-token host username)
        (and +forge-allow-auth-source
             (funcall original host username package nocreate forge))
        (user-error
         "No Forge token for %s; use macOS Keychain, Auth Source, or M-x +forge-set-session-token"
         host))))

(defun +forge--install-native-ghub-token-advice ()
  "Install the narrow Ghub token-provider adapter once."
  (when (and (fboundp 'ghub--token)
             (not (advice-member-p #'+forge--ghub-token-from-native-store
                                   'ghub--token)))
    ;; Private Ghub seam, isolated here because Ghub has no public token
    ;; provider hook.
    (advice-add 'ghub--token :around
                #'+forge--ghub-token-from-native-store)))

(with-eval-after-load 'ghub
  (+forge--install-native-ghub-token-advice))

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

(defvar +forge-repository-lookup-function #'forge-get-repository
  "Forge repository lookup seam used by explicit synchronization tests.")

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

(defun +forge-repository-for-explicit-sync (root)
  "Return ROOT's Forge repository, registering it during explicit sync.

This function may contact the forge when the repository is not known yet;
call it only from the explicit `C-c g f' / `C-c g F' synchronization path.
Unlike `forge-add-repository', it does not add a pull-request refspec to the
working clone and does not start a second Forge pull.  The caller owns the
single pull after this one-time repository-id lookup."
  (unless (+forge--ensure-forge-apis)
    (user-error "Forge is unavailable%s"
                (if +forge--load-error
                    (format ": %s" +forge--load-error)
                  "")))
  (let ((default-directory (file-name-as-directory root)))
    (or (funcall +forge-repository-lookup-function :tracked?)
        (when-let ((stub (funcall +forge-repository-lookup-function :stub?)))
          (funcall +forge-repository-lookup-function stub nil :insert!)))))

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
             "Install/load Forge, then run C-c g f to register and synchronize.")))
  (let ((repo (+forge--forge-repo-for-canonical repository-id :known?)))
    (unless repo
      (user-error
       (concat "Repository is not tracked by Forge. "
               "Run C-c g f to register and synchronize it.")))
    (unless (eq (+forge--slot repo 'condition) :tracked)
      (user-error
       (concat "Repository is not tracked by Forge. "
               "Run C-c g f to register and synchronize it.")))
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

(defun +forge-open-pr-snapshots (repository-id)
  "Return cached open or draft PRs in REPOSITORY-ID.
The result is newest-number-first and performs no network access."
  (sort
   (cl-remove-if-not
    (lambda (snapshot)
      (memq (+forge-pr-snapshot-state snapshot) '(open draft)))
    (+forge-list-pr-snapshots repository-id))
   (lambda (a b)
     (> (+forge-pr-snapshot-number a)
        (+forge-pr-snapshot-number b)))))

(defun +forge-prs-for-head-ref (repository-id head-ref)
  "Return cached open or draft PRs in REPOSITORY-ID for HEAD-REF.
The result is newest-number-first and performs no network access."
  (cl-remove-if-not
   (lambda (snapshot)
     (equal (+forge-pr-snapshot-head-ref snapshot) head-ref))
   (+forge-open-pr-snapshots repository-id)))

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
