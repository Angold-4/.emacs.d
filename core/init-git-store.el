;;; init-git-store.el --- Canonical repository identity and local contexts -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Repository registry, URL normalization, local-context ownership

;;; Commentary:
;; Phase 3 ownership for canonical repository identity and local contexts.
;; Transport-equivalent hosted remotes share one repository record.  Independent
;; clones and linked worktrees remain explicit local contexts with separate
;; branches, indexes, and worktrees.
;;
;; This module never fetches, opens Forge's database, creates mirrors, or
;; contacts a remote.  Persistence is thin scalar metadata only.
;;
;; Ownership chain: init-git -> init-git-store -> init-git-ui

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;; ---------------------------------------------------------------------------
;; Customization
;; ---------------------------------------------------------------------------

(defcustom +git-store-registry-directory
  (expand-file-name ".cache/git-review/repositories/" user-emacs-directory)
  "Directory for the thin Phase 3 repository registry.
Tests must bind this to a temporary directory."
  :type 'directory
  :group 'magit)

(defvar +git-store--git-process-count 0
  "Number of Git processes started by store helpers.
Reset around instrumented public commands in tests.")

;; ---------------------------------------------------------------------------
;; Structures
;; ---------------------------------------------------------------------------

(cl-defstruct (+git-store-local-context
               (:constructor +git-store-local-context--create)
               (:copier nil))
  "One local clone or linked worktree under a canonical repository."
  context-id       ; stable id including per-worktree git-dir
  repository-id    ; canonical repository identity
  root             ; normalized worktree root
  git-dir          ; absolute per-worktree Git directory
  git-common-dir   ; absolute shared Git storage directory
  current-branch   ; symbolic branch or nil (detached)
  head             ; resolved HEAD OID or nil (unborn)
  upstream         ; symbolic upstream or nil
  remote-name      ; selected fetch remote alias or nil
  remote-url       ; selected fetch URL or nil
  available-p)     ; nil when root is missing/deleted

(cl-defstruct (+git-store-repository
               (:constructor +git-store-repository--create)
               (:copier nil))
  "One live canonical repository record."
  repository-id
  forge-repository ; optional; never required; never persisted
  local-contexts   ; list of +git-store-local-context
  shared-buffers   ; list of live buffer objects (not persisted)
  cache-generation
  last-success     ; reserved local metadata
  last-error       ; reserved local metadata
  ;; Phase 4 placeholders only; behavior is not implemented here.
  mirror-directory
  sync-process
  waiting-buffers)

;; ---------------------------------------------------------------------------
;; Live registry
;; ---------------------------------------------------------------------------

(defvar +git-store--repositories (make-hash-table :test #'equal)
  "Live in-memory map: repository-id -> `+git-store-repository'.")

(defvar +git-store--contexts-by-id (make-hash-table :test #'equal)
  "Live map: context-id -> `+git-store-local-context'.")

(defvar +git-store--contexts-by-root (make-hash-table :test #'equal)
  "Live map: normalized root -> `+git-store-local-context'.")

(defvar +git-store--dirty nil
  "Non-nil when the in-memory registry needs persistence.")

(defvar +git-store--registry-loaded nil
  "Non-nil after the registry has been loaded or intentionally reset.
When nil, the next public registry access loads from disk once.")

(defun +git-store-reset-registry ()
  "Clear the live registry.  Intended for tests.
Marks the registry as loaded so tests that bind a temporary storage
root do not accidentally reload the owner's on-disk registry."
  (clrhash +git-store--repositories)
  (clrhash +git-store--contexts-by-id)
  (clrhash +git-store--contexts-by-root)
  (setq +git-store--dirty nil
        +git-store--registry-loaded t)
  nil)

(defun +git-store--ensure-registry-loaded ()
  "Load the thin registry from disk once, before public access.
No-op after a successful load or an intentional `+git-store-reset-registry'."
  (unless +git-store--registry-loaded
    (+git-store-load-registry)))

;; ---------------------------------------------------------------------------
;; Git helpers (argument lists only)
;; ---------------------------------------------------------------------------

(defun +git-store--call-git (root &rest args)
  "Run git ARGS in ROOT; return (exit-code . stdout-string)."
  (cl-incf +git-store--git-process-count)
  (with-temp-buffer
    (let* ((default-directory
            (file-name-as-directory (expand-file-name root)))
           (process-environment
            (append '("GIT_TERMINAL_PROMPT=0") process-environment))
           (exit (apply #'call-process "git" nil t nil args)))
      (cons exit (buffer-string)))))

(defun +git-store--git-ok (root &rest args)
  "Run git ARGS in ROOT and return trimmed stdout, signaling on failure."
  (let* ((result (apply #'+git-store--call-git root args))
         (exit (car result))
         (out (string-trim-right (cdr result))))
    (unless (eq exit 0)
      (error "git %s failed in %s (exit %s): %s"
             (mapconcat #'identity args " ") root exit out))
    out))

(defun +git-store--git-string (root &rest args)
  "Run git ARGS in ROOT; return trimmed stdout or nil on failure/empty."
  (let* ((result (apply #'+git-store--call-git root args))
         (exit (car result))
         (out (string-trim-right (cdr result))))
    (and (eq exit 0) (not (string-empty-p out)) out)))

(defun +git-store--normalize-path (path)
  "Return PATH as an absolute path without a trailing slash."
  (directory-file-name (expand-file-name path)))

;; ---------------------------------------------------------------------------
;; URL -> canonical identity (pure)
;; ---------------------------------------------------------------------------

(defconst +git-store--default-ports
  '(("ssh" . "22")
    ("git+ssh" . "22")
    ("ssh+git" . "22")
    ("https" . "443")
    ("http" . "80")
    ("git" . "9418"))
  "Transport -> default port.  Default ports are omitted from identities.")

(defun +git-store--strip-dot-git (path)
  "Remove one trailing `.git' from PATH when present."
  (cond
   ((string-suffix-p ".git/" path)
    (substring path 0 -5))
   ((string-suffix-p ".git" path)
    (substring path 0 -4))
   (t path)))

(defun +git-store--collapse-slashes (path)
  "Collapse redundant slashes and trim leading/trailing slashes from PATH."
  (let* ((parts (split-string path "/" t))
         (joined (mapconcat #'identity parts "/")))
    joined))

(defun +git-store--host-with-port (host port scheme)
  "Return HOST, or HOST:PORT when PORT is meaningful for SCHEME."
  (let* ((host (downcase host))
         (default (cdr (assoc scheme +git-store--default-ports))))
    (if (and port
             (not (string-empty-p port))
             (not (and default (equal port default))))
        (format "%s:%s" host port)
      host)))

(defun +git-store--resolve-local-path (path &optional base-directory)
  "Resolve PATH to an absolute filesystem path for local identity.
Relative PATH requires BASE-DIRECTORY and is expanded against that
directory only.  Never uses the caller's ambient `default-directory'
for relative remotes.  Absolute and `~/' paths resolve independently.
Return nil when a relative path has no base."
  (cond
   ((or (not (stringp path)) (string-empty-p path))
    nil)
   ((file-name-absolute-p path)
    (+git-store--normalize-path path))
   ((string-prefix-p "~/" path)
    (+git-store--normalize-path (expand-file-name path)))
   (base-directory
    (+git-store--normalize-path (expand-file-name path base-directory)))
   (t nil)))

(defun +git-store--local-identity (path &optional base-directory)
  "Return a local-only repository identity for PATH, or nil if unresolvable.
BASE-DIRECTORY is required when PATH is relative."
  (let ((absolute (+git-store--resolve-local-path path base-directory)))
    (and absolute (format "local:%s" absolute))))

(defun +git-store--parse-scp-url (url)
  "Parse SCP-style URL `user@host:path'.  Return (HOST PORT PATH) or nil."
  (when (and (not (string-match-p "://" url))
             (string-match
              "\\`\\(?:\\([^@]+\\)@\\)?\\([^:/]+\\):\\(.+\\)\\'"
              url))
    (let ((host (match-string 2 url))
          (path (match-string 3 url)))
      ;; Reject Windows-style drive paths like C:/...
      (unless (and (= (length host) 1)
                   (string-match-p "\\`[A-Za-z]\\'" host))
        (list host nil path)))))

(defun +git-store--parse-scheme-url (url)
  "Parse scheme:// URLs.  Return (SCHEME HOST PORT PATH) or nil."
  (when (string-match
         (concat "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\)://"
                 "\\(?:[^/@]+@\\)?"
                 "\\([^/:]+\\)"
                 "\\(?::\\([0-9]+\\)\\)?"
                 "\\(/.*\\)?\\'")
         url)
    (list (downcase (match-string 1 url))
          (match-string 2 url)
          (match-string 3 url)
          (or (match-string 4 url) "/"))))

(defun +git-store--local-path-url-p (url)
  "Return non-nil when URL looks like a local filesystem path remote."
  (or (file-name-absolute-p url)
      (string-prefix-p "./" url)
      (string-prefix-p "../" url)
      (string-prefix-p "~/" url)
      ;; Bare relative path: no scheme and not SCP `user@host:path' / `host:path'.
      (and (not (string-match-p "://" url))
           (not (string-match-p "\\`\\(?:[^@]+@\\)?[^:/]+:" url)))))
(defun +git-store-normalize-remote-url (url &optional base-directory)
  "Return canonical repository identity for URL, or nil when unsafe/local.

Optional BASE-DIRECTORY is the repository root used to resolve relative
local remotes.  Relative paths must not depend on ambient
`default-directory'.

Transport-equivalent hosted forms share one identity, for example:

  git@github.com:org/dragon.git
  ssh://git@github.com/org/dragon.git
  https://github.com/org/dragon.git
  git://github.com/org/dragon.git
    -> github.com/org/dragon

Rules:
- Ignore transport and user information.
- Lowercase the host.
- Remove redundant slashes and one trailing `.git'.
- Retain the complete repository path after the host.
- Retain meaningful non-default host ports.
- Local paths and `file://' remotes return a `local:' identity.
- Malformed/ambiguous URLs return nil."
  (cond
   ((or (not (stringp url)) (string-empty-p (string-trim url)))
    nil)
   (t
    (let ((url (string-trim url)))
      (cond
       ;; Absolute/relative local paths.
       ((+git-store--local-path-url-p url)
        (+git-store--local-identity url base-directory))
       ;; file:// remotes stay local.
       ((string-match-p "\\`file://" url)
        (let ((path (cond
                     ;; file:///abs/path or file://localhost/abs/path
                     ((string-match "\\`file://\\(?:localhost\\)?\\(/.*\\)\\'" url)
                      (match-string 1 url))
                     ((string-match "\\`file://\\([^/].*\\)\\'" url)
                      (match-string 1 url))
                     (t nil))))
          (if (and path (not (string-empty-p path)) (not (equal path "/")))
              (+git-store--local-identity
               (+git-store--strip-dot-git path)
               base-directory)
            nil)))
       (t
        (let* ((forge-parts
                (and (fboundp 'forge--split-forge-url)
                     (ignore-errors (forge--split-forge-url url))))
               (scheme-parts (+git-store--parse-scheme-url url))
               (scp-parts (and (not scheme-parts)
                               (+git-store--parse-scp-url url))))
          (cond
           ;; Prefer Forge's host/owner/name when available so ssh.github.com
           ;; aligns with github.com.  Ports still come from our parser.
           ((and forge-parts (= (length forge-parts) 3))
            (pcase-let* ((`(,host ,owner ,name) forge-parts)
                         (port (and scheme-parts (nth 2 scheme-parts)))
                         (scheme (or (and scheme-parts (nth 0 scheme-parts))
                                     "ssh"))
                         (host-port (+git-store--host-with-port
                                     host port scheme))
                         (path (+git-store--collapse-slashes
                                (+git-store--strip-dot-git
                                 (format "%s/%s" owner name)))))
              (and (not (string-empty-p path))
                   (format "%s/%s" host-port path))))
           (scheme-parts
            (pcase-let* ((`(,scheme ,host ,port ,raw-path) scheme-parts))
              (cond
               ((member scheme '("file"))
                (+git-store--local-identity raw-path base-directory))
               ((not (member scheme
                             '("ssh" "git+ssh" "ssh+git" "https" "http" "git")))
                nil)
               (t
                (let* ((host-port (+git-store--host-with-port
                                   host port scheme))
                       (path (+git-store--collapse-slashes
                              (+git-store--strip-dot-git
                               (string-remove-prefix "/" raw-path)))))
                  (cond
                   ((string-empty-p path) nil)
                   ((not (string-match-p "/" path)) nil) ; need owner/name
                   (t (format "%s/%s" host-port path))))))))
           (scp-parts
            (pcase-let* ((`(,host ,_port ,raw-path) scp-parts)
                         (host-port (+git-store--host-with-port
                                     host nil "ssh"))
                         (path (+git-store--collapse-slashes
                                (+git-store--strip-dot-git raw-path))))
              (cond
               ((string-empty-p path) nil)
               ((not (string-match-p "/" path)) nil)
               (t (format "%s/%s" host-port path)))))
           (t nil)))))))))

;; ---------------------------------------------------------------------------
;; Remote selection
;; ---------------------------------------------------------------------------

(defun +git-store--remote-fetch-url (root remote)
  "Return the fetch URL for REMOTE in ROOT, or nil.
Never uses a push-only URL."
  (or (+git-store--git-string root "config" "--get"
                               (format "remote.%s.url" remote))
      ;; Explicit fetch URL when remote.<name>.url is unset but fetchurl is set.
      (+git-store--git-string root "config" "--get"
                               (format "remote.%s.fetchurl" remote))))

(defun +git-store--list-remotes (root)
  "Return remotes in ROOT that have a fetch URL, lexically sorted."
  (let ((names (split-string
                (or (+git-store--git-string root "remote") "")
                "\n" t))
        remotes)
    (dolist (name names)
      (when (+git-store--remote-fetch-url root name)
        (push name remotes)))
    (sort remotes #'string<)))

(defun +git-store--upstream-remote (root branch)
  "Return the upstream remote name for BRANCH in ROOT, or nil."
  (when branch
    (let ((up (+git-store--git-string
               root "rev-parse" "--abbrev-ref"
               (format "%s@{upstream}" branch))))
      (when (and up (string-match "\\`\\([^/]+\\)/" up))
        (match-string 1 up)))))

(defun +git-store-select-remote (root &optional branch)
  "Choose the fetch remote for ROOT deterministically.
Order: upstream remote for BRANCH (or current branch), then origin,
then the lexically first remote with a fetch URL.  Returns
(REMOTE-NAME . FETCH-URL) or nil."
  (let* ((branch (or branch
                     (+git-store--git-string
                      root "symbolic-ref" "--short" "HEAD")))
         (upstream (+git-store--upstream-remote root branch))
         (remotes (+git-store--list-remotes root))
         (chosen (cond
                  ((and upstream (member upstream remotes)) upstream)
                  ((member "origin" remotes) "origin")
                  ((car remotes) (car remotes))
                  (t nil))))
    (and chosen
         (cons chosen (+git-store--remote-fetch-url root chosen)))))

;; ---------------------------------------------------------------------------
;; Context discovery
;; ---------------------------------------------------------------------------

(defun +git-store--context-id (git-dir)
  "Stable local-context identity from absolute GIT-DIR."
  (format "ctx:%s" (+git-store--normalize-path git-dir)))

(defun +git-store--discover-context (root)
  "Inspect ROOT and return a fresh `+git-store-local-context'.
Does not mutate the registry."
  (let* ((root (+git-store--normalize-path root))
         (git-dir
          (+git-store--normalize-path
           (or (+git-store--git-string root "rev-parse" "--absolute-git-dir")
               (error "Not a Git repository: %s" root))))
         (git-common-dir
          (or (+git-store--git-string
               root "rev-parse" "--path-format=absolute" "--git-common-dir")
              (+git-store--git-string root "rev-parse" "--git-common-dir")
              git-dir))
         (git-common-dir
          (+git-store--normalize-path
           (if (file-name-absolute-p git-common-dir)
               git-common-dir
             (expand-file-name git-common-dir
                               (file-name-directory git-dir)))))
         (branch (+git-store--git-string
                  root "symbolic-ref" "--short" "HEAD"))
         (head (+git-store--git-string root "rev-parse" "HEAD"))
         (upstream (and branch
                        (+git-store--git-string
                         root "rev-parse" "--abbrev-ref"
                         (format "%s@{upstream}" branch))))
         (remote-pair (+git-store-select-remote root branch))
         (remote-name (car remote-pair))
         (remote-url (cdr remote-pair))
         (repo-id (or (and remote-url
                           (+git-store-normalize-remote-url remote-url root))
                      (+git-store--local-identity git-common-dir)))
         (ctx-id (+git-store--context-id git-dir)))
    (+git-store-local-context--create
     :context-id ctx-id
     :repository-id repo-id
     :root root
     :git-dir git-dir
     :git-common-dir git-common-dir
     :current-branch branch
     :head head
     :upstream upstream
     :remote-name remote-name
     :remote-url remote-url
     :available-p (file-directory-p root))))
;; ---------------------------------------------------------------------------
;; Registry mutations
;; ---------------------------------------------------------------------------

(defun +git-store--ensure-repository (repository-id)
  "Return the live repository record for REPOSITORY-ID, creating if needed."
  (or (gethash repository-id +git-store--repositories)
      (let ((repo (+git-store-repository--create
                   :repository-id repository-id
                   :forge-repository nil
                   :local-contexts nil
                   :shared-buffers nil
                   :cache-generation 0
                   :last-success nil
                   :last-error nil
                   :mirror-directory nil
                   :sync-process nil
                   :waiting-buffers nil)))
        (puthash repository-id repo +git-store--repositories)
        repo)))

(defun +git-store--context-equal-scalars (a b)
  "Return non-nil when scalar fields of contexts A and B match."
  (and (equal (+git-store-local-context-context-id a)
              (+git-store-local-context-context-id b))
       (equal (+git-store-local-context-repository-id a)
              (+git-store-local-context-repository-id b))
       (equal (+git-store-local-context-root a)
              (+git-store-local-context-root b))
       (equal (+git-store-local-context-git-dir a)
              (+git-store-local-context-git-dir b))
       (equal (+git-store-local-context-git-common-dir a)
              (+git-store-local-context-git-common-dir b))
       (equal (+git-store-local-context-current-branch a)
              (+git-store-local-context-current-branch b))
       (equal (+git-store-local-context-head a)
              (+git-store-local-context-head b))
       (equal (+git-store-local-context-upstream a)
              (+git-store-local-context-upstream b))
       (equal (+git-store-local-context-remote-name a)
              (+git-store-local-context-remote-name b))
       (equal (+git-store-local-context-remote-url a)
              (+git-store-local-context-remote-url b))
       (eq (+git-store-local-context-available-p a)
           (+git-store-local-context-available-p b))))

(defun +git-store--index-context (ctx)
  "Index CTX in the by-id and by-root tables."
  (puthash (+git-store-local-context-context-id ctx)
           ctx +git-store--contexts-by-id)
  (puthash (+git-store-local-context-root ctx)
           ctx +git-store--contexts-by-root))

(defun +git-store--bump-generation (repo)
  "Increment REPO cache-generation and mark the registry dirty."
  (setf (+git-store-repository-cache-generation repo)
        (1+ (+git-store-repository-cache-generation repo)))
  (setq +git-store--dirty t))

(defun +git-store-register-root (root)
  "Register or refresh the local context for ROOT.
Returns the `+git-store-local-context'.  Repeated registration of the
same root updates the existing record and returns the same context id.
Canonical repository records are shared by identity.
Identical discovery results do not bump `cache-generation'."
  (+git-store--ensure-registry-loaded)
  (let* ((fresh (+git-store--discover-context root))
         (repo-id (+git-store-local-context-repository-id fresh))
         (ctx-id (+git-store-local-context-context-id fresh))
         (repo (+git-store--ensure-repository repo-id))
         (existing (gethash ctx-id +git-store--contexts-by-id)))
    (cond
     ((and existing (+git-store--context-equal-scalars existing fresh))
      ;; No state change: return the live object without bumping generation.
      existing)
     (existing
      ;; Same context id: refresh scalars in place.  Rehome when the
      ;; canonical repository identity changed (remote URL moved).
      (let ((old-repo-id (+git-store-local-context-repository-id existing)))
        (unless (equal old-repo-id repo-id)
          (let ((old-repo (+git-store-get-repository old-repo-id)))
            (when old-repo
              (setf (+git-store-repository-local-contexts old-repo)
                    (cl-remove existing
                               (+git-store-repository-local-contexts old-repo)
                               :count 1))
              (+git-store--bump-generation old-repo))))
        (setf (+git-store-local-context-repository-id existing) repo-id)
        (setf (+git-store-local-context-root existing)
              (+git-store-local-context-root fresh))
        (setf (+git-store-local-context-git-dir existing)
              (+git-store-local-context-git-dir fresh))
        (setf (+git-store-local-context-git-common-dir existing)
              (+git-store-local-context-git-common-dir fresh))
        (setf (+git-store-local-context-current-branch existing)
              (+git-store-local-context-current-branch fresh))
        (setf (+git-store-local-context-head existing)
              (+git-store-local-context-head fresh))
        (setf (+git-store-local-context-upstream existing)
              (+git-store-local-context-upstream fresh))
        (setf (+git-store-local-context-remote-name existing)
              (+git-store-local-context-remote-name fresh))
        (setf (+git-store-local-context-remote-url existing)
              (+git-store-local-context-remote-url fresh))
        (setf (+git-store-local-context-available-p existing)
              (+git-store-local-context-available-p fresh))
        (unless (memq existing (+git-store-repository-local-contexts repo))
          (setf (+git-store-repository-local-contexts repo)
                (append (+git-store-repository-local-contexts repo)
                        (list existing))))
        (+git-store--index-context existing)
        (+git-store--bump-generation repo)
        (+git-store-save-registry)
        existing))
     (t
      (setf (+git-store-repository-local-contexts repo)
            (append (+git-store-repository-local-contexts repo)
                    (list fresh)))
      (+git-store--index-context fresh)
      (+git-store--bump-generation repo)
      (+git-store-save-registry)
      fresh))))

(defun +git-store-get-repository (repository-id)
  "Return the live repository record for REPOSITORY-ID, or nil."
  (+git-store--ensure-registry-loaded)
  (gethash repository-id +git-store--repositories))

(defun +git-store-get-context (context-id)
  "Return the live local context for CONTEXT-ID, or nil."
  (+git-store--ensure-registry-loaded)
  (gethash context-id +git-store--contexts-by-id))

(defun +git-store-context-for-root (root)
  "Return the local context for ROOT, refreshing discovery on every call.
Opening/reopening a review uses this as the refresh boundary so a
changed remote, branch, HEAD, upstream, or availability is picked up.
Identical discovery results preserve `cache-generation'."
  (+git-store-register-root root))

(defun +git-store-list-contexts (&optional repository-id)
  "Return live contexts, optionally filtered by REPOSITORY-ID.
Order is deterministic: by repository-id then context-id."
  (+git-store--ensure-registry-loaded)
  (let (contexts)
    (maphash (lambda (_id ctx) (push ctx contexts))
             +git-store--contexts-by-id)
    (when repository-id
      (setq contexts
            (cl-remove-if-not
             (lambda (ctx)
               (equal (+git-store-local-context-repository-id ctx)
                      repository-id))
             contexts)))
    (cl-sort contexts
             (lambda (a b)
               (let ((ra (+git-store-local-context-repository-id a))
                     (rb (+git-store-local-context-repository-id b)))
                 (if (equal ra rb)
                     (string< (+git-store-local-context-context-id a)
                              (+git-store-local-context-context-id b))
                   (string< ra rb)))))))

(defun +git-store-list-live-contexts (repository-id)
  "Return available contexts for REPOSITORY-ID whose roots still exist."
  (cl-remove-if-not
   (lambda (ctx)
     (and (+git-store-local-context-available-p ctx)
          (file-directory-p (+git-store-local-context-root ctx))))
   (+git-store-list-contexts repository-id)))

(defun +git-store-prune-shared-buffers (repository-id)
  "Drop dead buffers from REPOSITORY-ID's shared-buffers list.
Increments cache-generation only when something was removed."
  (let ((repo (+git-store-get-repository repository-id)))
    (when repo
      (let* ((before (+git-store-repository-shared-buffers repo))
             (after (cl-remove-if-not #'buffer-live-p before)))
        (unless (eq (length before) (length after))
          (setf (+git-store-repository-shared-buffers repo) after)
          (+git-store--bump-generation repo)
          (+git-store-save-registry))
        after))))

(defun +git-store-remember-shared-buffer (repository-id buffer)
  "Record BUFFER on REPOSITORY-ID's shared-buffers list when live."
  (when (and repository-id (buffer-live-p buffer))
    (let ((repo (+git-store--ensure-repository repository-id)))
      (unless (memq buffer (+git-store-repository-shared-buffers repo))
        (setf (+git-store-repository-shared-buffers repo)
              (cons buffer
                    (+git-store-repository-shared-buffers repo)))
        ;; Shared buffers are ephemeral; do not bump or persist.
        ))))

(defun +git-store-mark-context-unavailable (context-id)
  "Mark CONTEXT-ID unavailable without removing the canonical record."
  (let ((ctx (+git-store-get-context context-id)))
    (when (and ctx (+git-store-local-context-available-p ctx))
      (setf (+git-store-local-context-available-p ctx) nil)
      (let ((repo (+git-store-get-repository
                   (+git-store-local-context-repository-id ctx))))
        (when repo
          (+git-store--bump-generation repo)
          (+git-store-save-registry)))
      ctx)))

(defun +git-store-refresh-context-availability (&optional repository-id)
  "Re-check context roots; mark missing ones unavailable.
Returns the list of contexts whose availability changed."
  (let (changed)
    (dolist (ctx (+git-store-list-contexts repository-id))
      (let* ((root (+git-store-local-context-root ctx))
             (alive (file-directory-p root))
             (was (+git-store-local-context-available-p ctx)))
        (unless (eq alive was)
          (setf (+git-store-local-context-available-p ctx) alive)
          (push ctx changed)
          (let ((repo (+git-store-get-repository
                       (+git-store-local-context-repository-id ctx))))
            (when repo
              (+git-store--bump-generation repo))))))
    (when changed
      (+git-store-save-registry))
    (nreverse changed)))

;; ---------------------------------------------------------------------------
;; Object presence (local Git only)
;; ---------------------------------------------------------------------------

(defun +git-store-context-contains-oid-p (ctx oid)
  "Return non-nil when CTX's repository contains object OID.
Uses `git cat-file -e', which exits 0 with empty stdout on success."
  (and ctx oid
       (+git-store-local-context-available-p ctx)
       (file-directory-p (+git-store-local-context-root ctx))
       (eq 0 (car (+git-store--call-git
                   (+git-store-local-context-root ctx)
                   "cat-file" "-e" oid)))))

(defun +git-store-context-eligible-p (ctx repository-id &optional oids)
  "Return non-nil when CTX may operate for REPOSITORY-ID.
Requires all of: matching repository ID, availability, an existing root,
and presence of every OID in OIDS when OIDS is non-nil.  Central
eligibility for preferred selection, reopening adoption, fallback, and L."
  (and ctx
       repository-id
       (equal (+git-store-local-context-repository-id ctx) repository-id)
       (+git-store-local-context-available-p ctx)
       (file-directory-p (+git-store-local-context-root ctx))
       (let ((oids (delq nil (copy-sequence oids))))
         (or (null oids)
             (cl-every (lambda (oid)
                         (+git-store-context-contains-oid-p ctx oid))
                       oids)))))

(defun +git-store-eligible-contexts-for-oids (repository-id oids)
  "Return live contexts for REPOSITORY-ID that contain every OID in OIDS."
  (cl-remove-if-not
   (lambda (ctx)
     (+git-store-context-eligible-p ctx repository-id oids))
   (+git-store-list-live-contexts repository-id)))

(defun +git-store-default-edit-context (repository-id &optional preferred-id oids)
  "Choose an active edit context for REPOSITORY-ID.
PREFERRED-ID is tried first when still eligible.  OIDS when non-nil
must all exist in the chosen context.  Selection is deterministic:
live contexts sorted by context-id."
  (+git-store-refresh-context-availability repository-id)
  (let* ((eligible (if oids
                       (+git-store-eligible-contexts-for-oids
                        repository-id oids)
                     (cl-remove-if-not
                      (lambda (ctx)
                        (+git-store-context-eligible-p ctx repository-id))
                      (+git-store-list-live-contexts repository-id))))
         (preferred
          (and preferred-id
               (let ((ctx (+git-store-get-context preferred-id)))
                 (and (+git-store-context-eligible-p ctx repository-id oids)
                      ctx)))))
    (or preferred (car eligible))))

;; ---------------------------------------------------------------------------
;; Persistence (thin JSON; atomic rename)
;; ---------------------------------------------------------------------------

(defun +git-store--registry-file ()
  "Return the absolute path of the thin registry JSON file."
  (expand-file-name "registry.json"
                    (file-name-as-directory +git-store-registry-directory)))

(defun +git-store--ensure-registry-dir ()
  "Create `+git-store-registry-directory' when missing."
  (make-directory +git-store-registry-directory t))

(defun +git-store--context-to-alist (ctx)
  "Serialize CTX to a JSON-friendly alist of scalars."
  `(("context-id" . ,(+git-store-local-context-context-id ctx))
    ("repository-id" . ,(+git-store-local-context-repository-id ctx))
    ("root" . ,(+git-store-local-context-root ctx))
    ("git-dir" . ,(+git-store-local-context-git-dir ctx))
    ("git-common-dir" . ,(+git-store-local-context-git-common-dir ctx))
    ("current-branch" . ,(+git-store-local-context-current-branch ctx))
    ("head" . ,(+git-store-local-context-head ctx))
    ("upstream" . ,(+git-store-local-context-upstream ctx))
    ("remote-name" . ,(+git-store-local-context-remote-name ctx))
    ("remote-url" . ,(+git-store-local-context-remote-url ctx))
    ("available" . ,(if (+git-store-local-context-available-p ctx) t :false))))

(defun +git-store--repository-to-alist (repo)
  "Serialize REPO to a JSON-friendly alist of scalars."
  `(("repository-id" . ,(+git-store-repository-repository-id repo))
    ("cache-generation" . ,(+git-store-repository-cache-generation repo))
    ("last-success" . ,(+git-store-repository-last-success repo))
    ("last-error" . ,(+git-store-repository-last-error repo))
    ("local-contexts" . ,(mapcar #'+git-store--context-to-alist
                                 (+git-store-repository-local-contexts repo)))))

(defun +git-store--registry-alist ()
  "Build the full registry alist for JSON encoding."
  (let (repos)
    (maphash (lambda (_id repo) (push repo repos))
             +git-store--repositories)
    (setq repos (cl-sort repos #'string<
                         :key #'+git-store-repository-repository-id))
    `(("version" . 1)
      ("repositories" . ,(mapcar #'+git-store--repository-to-alist repos)))))

(defun +git-store-save-registry ()
  "Atomically persist the thin registry when dirty.
Uses a same-directory temporary file plus rename.  On write/rename
failure, preserves the prior valid generation and signals."
  (when +git-store--dirty
    (+git-store--ensure-registry-dir)
    (let* ((file (+git-store--registry-file))
           (dir (file-name-directory file))
           (tmp (make-temp-file
                 (expand-file-name ".git-store-registry-" dir) nil ".tmp"))
           (payload (+git-store--registry-alist)))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (let ((json-encoding-pretty-print t)
                    (json-encoding-default-indentation "  "))
                (insert (json-encode payload))
                (insert "\n")))
            (rename-file tmp file t)
            (setq tmp nil
                  +git-store--dirty nil)
            file)
        (when (and tmp (file-exists-p tmp))
          (ignore-errors (delete-file tmp)))))))

(defun +git-store--alist-get (key alist)
  "Return the value for KEY in ALIST, trying string and symbol keys."
  (or (cdr (assoc key alist))
      (cdr (assq (intern key) alist))))

(defun +git-store--json-true-p (value)
  "Return non-nil when JSON VALUE represents true."
  (and value (not (eq value :false)) (not (eq value json-false))))

(defun +git-store--context-from-alist (alist)
  "Rebuild a local context from ALIST scalars."
  (let* ((available (+git-store--json-true-p
                     (+git-store--alist-get "available" alist)))
         (root (+git-store--alist-get "root" alist)))
    (+git-store-local-context--create
     :context-id (+git-store--alist-get "context-id" alist)
     :repository-id (+git-store--alist-get "repository-id" alist)
     :root root
     :git-dir (+git-store--alist-get "git-dir" alist)
     :git-common-dir (+git-store--alist-get "git-common-dir" alist)
     :current-branch (+git-store--alist-get "current-branch" alist)
     :head (+git-store--alist-get "head" alist)
     :upstream (+git-store--alist-get "upstream" alist)
     :remote-name (+git-store--alist-get "remote-name" alist)
     :remote-url (+git-store--alist-get "remote-url" alist)
     :available-p (and available root (file-directory-p root) t))))
(defun +git-store-load-registry ()
  "Load the thin registry from disk into memory.
Malformed persistence degrades to an empty usable registry with a
message and never evaluates Lisp."
  (+git-store-reset-registry)
  (let ((file (+git-store--registry-file)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (json-false :false)
                   (data (json-read))
                   (version (+git-store--alist-get "version" data))
                   (repos (+git-store--alist-get "repositories" data)))
              (unless (and (numberp version) (listp repos))
                (error "Malformed registry: bad version or repositories"))
              (dolist (repo-alist repos)
                (let* ((repo-id (+git-store--alist-get "repository-id"
                                                       repo-alist))
                       (gen (or (+git-store--alist-get "cache-generation"
                                                       repo-alist)
                                0))
                       (contexts-raw
                        (+git-store--alist-get "local-contexts" repo-alist))
                       (repo (+git-store-repository--create
                              :repository-id repo-id
                              :forge-repository nil
                              :local-contexts nil
                              :shared-buffers nil
                              :cache-generation gen
                              :last-success
                              (+git-store--alist-get "last-success" repo-alist)
                              :last-error
                              (+git-store--alist-get "last-error" repo-alist)
                              :mirror-directory nil
                              :sync-process nil
                              :waiting-buffers nil))
                       (contexts nil))
                  (unless (stringp repo-id)
                    (error "Malformed registry: repository-id"))
                  (dolist (ctx-alist contexts-raw)
                    (let ((ctx (+git-store--context-from-alist ctx-alist)))
                      (unless (stringp
                               (+git-store-local-context-context-id ctx))
                        (error "Malformed registry: context-id"))
                      (push ctx contexts)
                      (+git-store--index-context ctx)))
                  (setf (+git-store-repository-local-contexts repo)
                        (nreverse contexts))
                  (puthash repo-id repo +git-store--repositories)))))
        (error
         (message "Ignoring malformed Git review registry %s: %s"
                  file (error-message-string err))
         (+git-store-reset-registry)))))
  (setq +git-store--registry-loaded t
        +git-store--dirty nil)
  +git-store--repositories)

(defun +git-store-simulate-new-session ()
  "Clear live registry memory and allow the next access to reload from disk.
Intended for tests that verify persistence across a restart boundary."
  (clrhash +git-store--repositories)
  (clrhash +git-store--contexts-by-id)
  (clrhash +git-store--contexts-by-root)
  (setq +git-store--dirty nil
        +git-store--registry-loaded nil)
  nil)

(provide 'init-git-store)

;;; init-git-store.el ends here
