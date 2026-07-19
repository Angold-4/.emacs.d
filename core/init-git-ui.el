;;; init-git-ui.el --- Git review display, Evil mode, and visits -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Same-window Magit display, review minor mode, native visits

;;; Commentary:
;; Phase 1 UI contract:
;; - Default Magit/Forge navigation replaces the selected window.
;; - Explicit `o' and `|' reuse one right-hand window.
;; - `q' unwinds a per-buffer caller stack and restores layout.
;; - Generated review buffers share one Evil normal-state vocabulary.
;; - File visiting uses Magit native APIs, never rendered-text parsers.
;;
;; Phase 2 local review:
;; - Explicit `+git-review-target' values for worktree/staged/commit/branch.
;; - Machine-readable changed-file model (NUL Git parsers, never Magit text).
;; - Changes Tree (`+git-changes-tree-mode') with reviewed checkmarks.
;; - Atomic local review-state persistence under `.cache/git-review/state/'.
;; - Exact per-file unified diffs reused by target+path.
;; - Theme-safe black/red/green native Magit faces (no Delta/Difftastic).
;;
;; Phase 3 identity:
;; - Canonical repository id + local context id from `init-git-store'.
;; - Worktree/staged/branch state is context-local; commit buffers may share.
;; - `L' selects the active edit context on shareable immutable reviews.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'init-git-store)
(require 'init-git-sync)

;; =============================================================================
;; Caller / layout stack for nested review navigation
;; =============================================================================

(defvar-local +git-review--return nil
  "Return state for this review or visited buffer.
Form: (:buffer BUFFER :point POINT :window-config WINDOW-CONFIGURATION
       :selected-window WINDOW).
BUFFER is the caller buffer; WINDOW-CONFIGURATION restores the layout
as it existed immediately before this buffer was shown.")

(defvar +git-review--prefer-other-window nil
  "Non-nil while an explicit other-window visit is in progress.")

(defvar +git-review--suppress-return-record nil
  "Non-nil to skip recording return state during display.")

(defun +git-review--capture-return ()
  "Return a plist describing the current selected window and layout."
  (list :buffer (current-buffer)
        :point (point)
        :window-config (current-window-configuration)
        :selected-window (selected-window)))

(defun +git-review--set-return (buffer state)
  "Store return STATE on BUFFER when it is live and STATE is usable."
  (when (and buffer (buffer-live-p buffer) state)
    (with-current-buffer buffer
      (setq-local +git-review--return state))))

(defun +git-review--record-return-for-buffer (buffer)
  "Record the current layout as BUFFER's return target."
  (unless (or +git-review--suppress-return-record
              +git-review--prefer-other-window
              (not (buffer-live-p buffer))
              (eq (current-buffer) buffer))
    ;; Capture before entering BUFFER; current-buffer is still the caller.
    (let ((state (+git-review--capture-return)))
      (with-current-buffer buffer
        (setq-local +git-review--return state)))))

(defun +git-review--restore-return (state)
  "Restore layout and point from STATE.  Return non-nil on success."
  (when state
    (let ((conf (plist-get state :window-config))
          (buf (plist-get state :buffer))
          (pt (plist-get state :point))
          (win (plist-get state :selected-window)))
      (when (and conf (window-configuration-p conf))
        (set-window-configuration conf))
      (cond
       ((and (window-live-p win)
             (eq (window-buffer win) buf))
        (select-window win)
        (when (and (buffer-live-p buf) (integer-or-marker-p pt))
          (with-current-buffer buf
            (goto-char (min (max (point-min) pt) (point-max))))))
       ((buffer-live-p buf)
        (when (get-buffer-window buf t)
          (select-window (get-buffer-window buf t)))
        (when (and (integer-or-marker-p pt)
                   (eq (current-buffer) buf))
          (goto-char (min (max (point-min) pt) (point-max))))))
      t)))

;; =============================================================================
;; Same-window Magit display (single owner)
;; =============================================================================

(defun +git-review--process-buffer-p (buffer)
  "Return non-nil when BUFFER is a Magit process utility buffer."
  (with-current-buffer buffer
    (derived-mode-p 'magit-process-mode)))

(defun +git-review--other-window ()
  "Return the reusable right-hand review window, creating it if needed."
  (let* ((frame (selected-frame))
         (win (frame-parameter frame '+git-review-other-window)))
    (unless (and (window-live-p win)
                 (not (eq win (selected-window))))
      (setq win (split-window (selected-window) nil 'right))
      (set-frame-parameter frame '+git-review-other-window win))
    win))

(defun +git-review--display-in-other-window (buffer)
  "Display BUFFER in the reusable right-hand window and select it."
  (let ((win (+git-review--other-window)))
    (set-window-buffer win buffer)
    (select-window win)
    win))

(defun +git-display-buffer (buffer)
  "Display Magit/Forge BUFFER according to the Phase 1 window contract.

Navigation views replace the selected window.  Explicit other-window
visits reuse one right-hand window.  Magit process buffers may use a
non-selected utility window."
  (cond
   ((+git-review--process-buffer-p buffer)
    (display-buffer buffer '(display-buffer-below-selected
                             (inhibit-same-window . t)
                             (window-height . 0.3))))
   (+git-review--prefer-other-window
    (+git-review--record-return-for-buffer buffer)
    (+git-review--display-in-other-window buffer))
   (t
    (+git-review--record-return-for-buffer buffer)
    (display-buffer buffer '(display-buffer-same-window)))))

(defun +git-review--install-display ()
  "Install the single Magit display function owned by this module."
  (setq magit-display-buffer-function #'+git-display-buffer)
  ;; Own return restoration ourselves; Magit's global winconf fights nesting.
  (setq magit-pre-display-buffer-hook
        (delq 'magit-save-window-configuration
              (copy-sequence (or magit-pre-display-buffer-hook
                                 '(magit-save-window-configuration)))))
  (setq magit-bury-buffer-function #'+git-review-quit))

(with-eval-after-load 'magit
  (+git-review--install-display))

;; =============================================================================
;; Diff faces (theme-safe, native Magit only)
;; =============================================================================

(defun +git-review-apply-diff-faces ()
  "Apply Phase 2 native unified-diff faces for terminal and graphical Emacs.

Default/context uses an effectively black background with readable gray
text.  Added/removed lines use dark green/red backgrounds with near-white
foregrounds.  Refined regions are brighter.  File headings stay prominent;
hunk headings stay quieter.  Does not enable Delta or Difftastic."
  (custom-set-faces
   '(magit-diff-added
     ((t (:foreground "#e8ffe8" :background "#0a2a12" :extend t))))
   '(magit-diff-added-highlight
     ((t (:foreground "#f5fff5" :background "#0f3a18" :extend t))))
   '(magit-diff-removed
     ((t (:foreground "#ffe8e8" :background "#2a0a0a" :extend t))))
   '(magit-diff-removed-highlight
     ((t (:foreground "#fff5f5" :background "#3a0f0f" :extend t))))
   '(magit-diff-context
     ((t (:foreground "#b0b0b0" :background "#000000" :extend t))))
   '(magit-diff-context-highlight
     ((t (:foreground "#c8c8c8" :background "#0a0a0a" :extend t))))
   '(magit-diff-file-heading
     ((t (:foreground "#f0f0f0" :background "#000000" :weight bold :extend t))))
   '(magit-diff-file-heading-highlight
     ((t (:foreground "#ffffff" :background "#12121a" :weight bold :extend t))))
   '(magit-diff-hunk-heading
     ((t (:foreground "#8a8aaa" :background "#101018" :extend t))))
   '(magit-diff-hunk-heading-highlight
     ((t (:foreground "#a0a0c0" :background "#181828" :extend t))))
   '(magit-diff-lines-heading
     ((t (:foreground "#ffffff" :background "#2a2a40" :extend t))))
   '(smerge-refined-added
     ((t (:foreground "#ffffff" :background "#1a5c28" :extend t))))
   '(smerge-refined-removed
     ((t (:foreground "#ffffff" :background "#6c1a1a" :extend t))))
   '(diff-refine-added
     ((t (:foreground "#ffffff" :background "#1a5c28" :extend t))))
   '(diff-refine-removed
     ((t (:foreground "#ffffff" :background "#6c1a1a" :extend t))))
   '(magit-section-heading
     ((t (:foreground "#d0b060" :weight bold))))
   '(magit-section-highlight
     ((t (:background "#0a0a0a"))))))

(with-eval-after-load 'magit
  (+git-review-apply-diff-faces))

;; Re-apply after theme toggles so review readability is not erased.
(defun +git-review--reapply-faces-after-theme (&rest _)
  "Re-apply review diff faces after a theme change."
  (+git-review-apply-diff-faces))

;; init-themes loads before this module; re-apply after owner theme toggles.
(when (fboundp '+theme/load-dark)
  (advice-add '+theme/load-dark :after #'+git-review--reapply-faces-after-theme))
(when (fboundp '+theme/load-light)
  (advice-add '+theme/load-light :after #'+git-review--reapply-faces-after-theme))

;; Themes may load before Magit; apply once Magit is present.
(when (featurep 'magit)
  (+git-review-apply-diff-faces))

;; =============================================================================
;; Diff-hl
;; =============================================================================

(use-package diff-hl
  :straight t
  :hook ((after-init . global-diff-hl-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

;; =============================================================================
;; Section navigation helpers (Magit section APIs only)
;; =============================================================================

(defun +git-review--goto-section-type (type forward)
  "Move to the next/previous Magit section of TYPE.
FORWARD non-nil moves forward; otherwise backward.  Uses Magit section
APIs and never scans rendered text."
  (require 'magit-section)
  (let* ((start (point))
         (move (if forward #'magit-section-forward #'magit-section-backward))
         (label (symbol-name type)))
    (condition-case nil
        (progn
          (funcall move)
          (while (not (magit-section-match type (magit-current-section)))
            (funcall move))
          (recenter 3))
      (user-error
       (goto-char start)
       (user-error "No %s %s section"
                   (if forward "next" "previous")
                   label)))))

(defun +git-review-next-file ()
  "Jump to the next file section."
  (interactive)
  (+git-review--goto-section-type 'file t))

(defun +git-review-prev-file ()
  "Jump to the previous file section."
  (interactive)
  (+git-review--goto-section-type 'file nil))

(defun +git-review-next-hunk ()
  "Jump to the next hunk section."
  (interactive)
  (+git-review--goto-section-type 'hunk t))

(defun +git-review-prev-hunk ()
  "Jump to the previous hunk section."
  (interactive)
  (+git-review--goto-section-type 'hunk nil))

(defun +git-review-next-commit ()
  "Jump to the next commit section."
  (interactive)
  (+git-review--goto-section-type 'commit t))

(defun +git-review-prev-commit ()
  "Jump to the previous commit section."
  (interactive)
  (+git-review--goto-section-type 'commit nil))

;; =============================================================================
;; Native visits
;; =============================================================================

(defun +git-review--enable-return-on-buffer (buffer state)
  "Install return STATE on BUFFER and enable return mode there."
  (when (and buffer (buffer-live-p buffer) state)
    (with-current-buffer buffer
      (setq-local +git-review--return state)
      (+git-review-return-mode 1)
      ;; Writable worktree targets stay in normal state; do not force Insert.
      (when (bound-and-true-p evil-mode)
        (evil-normal-state)))))

(defun +git-review--apply-return (before state)
  "Record STATE on the buffer shown after leaving BEFORE, if any."
  (let ((after (current-buffer)))
    (when (and (not (eq after before))
               (buffer-live-p after))
      (if (with-current-buffer after
            (or (derived-mode-p 'magit-mode)
                (bound-and-true-p +git-review-buffer-mode)))
          (+git-review--set-return after state)
        (+git-review--enable-return-on-buffer after state)))
    after))

(defun +git-review--visit-thing-command ()
  "Return the Magit/Forge command remapped from `magit-visit-thing' at point.

Magit implements contextual RET via keymap remapping
\(`[remap magit-visit-thing]').  Calling `magit-visit-thing' directly
bypasses those remappings and always signals that nothing can be
visited.  Resolve the remapping first, then invoke that command."
  (or (command-remapping 'magit-visit-thing)
      'magit-visit-thing))

(defun +git-review--call-visit-thing ()
  "Invoke the remapped Magit/Forge primary visit command at point."
  (call-interactively (+git-review--visit-thing-command)))

(defun +git-review--call-visit-thing-other-window ()
  "Invoke the remapped primary visit into the reusable right-hand window.

Diff file visits do not consult `magit-display-buffer-function'; they
call `pop-to-buffer-same-window' / `switch-to-buffer-other-window'
directly.  For those, route through Magit's private
`magit-diff-visit-file--internal' (pinned Magit 1288f65) so we can
reuse `+git-review--other-window' instead of accumulating splits.
Compatibility risk: if Magit renames or changes that helper, this
branch must be updated."
  (let ((cmd (+git-review--visit-thing-command)))
    (cond
     ((and (eq cmd 'magit-diff-visit-file)
           (fboundp 'magit-diff-visit-file--internal))
      (magit-diff-visit-file--internal
       (and (bound-and-true-p magit-diff-visit-prefer-worktree)
            (memq (magit-diff--dwim) '(staged unstaged)))
       #'+git-review--display-in-other-window))
     (t
      (let ((+git-review--prefer-other-window t))
        (call-interactively cmd))))))

(defun +git-review-move-visual-lines-down (&optional n)
  "Move down N groups of eight visual lines (default one group)."
  (interactive "p")
  (evil-next-visual-line (* (or n 1) 8)))

(defun +git-review-move-visual-lines-up (&optional n)
  "Move up N groups of eight visual lines (default one group)."
  (interactive "p")
  (evil-previous-visual-line (* (or n 1) 8)))

(defun +git-review-visit ()
  "Primary visit in the selected window via Magit/Forge remappings."
  (interactive)
  (cond
   ((derived-mode-p '+git-changes-tree-mode)
    (+git-changes-tree-visit-file))
   ;; File sections in the PR overview intentionally carry a
   ;; `+git-review-file' record.  Magit's section-local remapping wins over
   ;; the PR major-mode remapping and expects a string path, so dispatch the
   ;; workspace explicitly instead of handing that record to Magit.
   ((and (derived-mode-p '+git-pr-mode)
         (fboundp '+git-pr-visit))
    (let ((state (+git-review--capture-return))
          (before (current-buffer)))
      (call-interactively #'+git-pr-visit)
      (+git-review--apply-return before state)))
   (t
    (let ((state (+git-review--capture-return))
          (before (current-buffer)))
      (+git-review--call-visit-thing)
      (+git-review--apply-return before state)))))

(defun +git-review-visit-other-window ()
  "Primary visit in the reusable right-hand window."
  (interactive)
  (cond
   ((derived-mode-p '+git-changes-tree-mode)
    (+git-changes-tree-visit-other-window))
   ((and (derived-mode-p '+git-pr-mode)
         (fboundp '+git-pr-visit))
    (let ((state (+git-review--capture-return))
          (before (current-buffer)))
      (+git-pr-visit t)
      (+git-review--apply-return before state)))
   (t
    (let ((state (+git-review--capture-return))
          (before (current-buffer)))
      (+git-review--call-visit-thing-other-window)
      (+git-review--apply-return before state)))))

(defun +git-review-quit (&optional _kill-buffer)
  "Return to the recorded caller/layout, or bury the Magit buffer.
Does not kill user source buffers."
  (interactive "P")
  (let ((state +git-review--return)
        (buf (current-buffer))
        (source-visit (and buffer-file-name
                           (not (derived-mode-p 'magit-mode)))))
    (setq-local +git-review--return nil)
    (when (bound-and-true-p +git-review-return-mode)
      (+git-review-return-mode -1))
    (cond
     (state
      (+git-review--restore-return state)
      ;; Never kill user source buffers; Magit buffers remain available.
      (when (and (not source-visit)
                 (buffer-live-p buf)
                 (not (get-buffer-window buf t)))
        (bury-buffer buf)))
     ((derived-mode-p 'magit-mode)
      (magit-mode-quit-window nil))
     (t
      (quit-window nil)))))

(defun +git-review-refresh ()
  "Refresh the current local Magit or Changes Tree buffer only.
Shareable commit buffers rebind Magit directories to the active edit
context first so a deleted originating clone does not break `gr'."
  (interactive)
  (cond
   ((derived-mode-p '+git-changes-tree-mode 'magit-mode)
    (when (bound-and-true-p +git-review-target)
      (+git-review--bind-operational-directory
       (+git-review--git-root-for-target +git-review-target)))
    (magit-refresh))
   (t (user-error "Nothing to refresh"))))

(defun +git-review-section-toggle ()
  "Toggle the Magit section at point."
  (interactive)
  (magit-section-toggle (magit-current-section)))

;; =============================================================================
;; Return mode for worktree / blob visits
;; =============================================================================

(defvar +git-review-return-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `+git-review-return-mode'.")

(define-minor-mode +git-review-return-mode
  "Temporary return binding after an explicit review visit."
  :lighter " GitRet"
  :keymap +git-review-return-mode-map)

;; =============================================================================
;; Review buffer minor mode
;; =============================================================================

(defvar +git-review-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `+git-review-buffer-mode'.")

(defun +git-review--enter-normal-state ()
  "Ensure generated review buffers start in Evil normal state."
  (when (bound-and-true-p evil-mode)
    (evil-normal-state)))

(define-minor-mode +git-review-buffer-mode
  "Consistent Evil navigation for Magit and Forge review buffers."
  :lighter " GitReview"
  :keymap +git-review-buffer-mode-map
  (when +git-review-buffer-mode
    (+git-review--enter-normal-state)))

(defun +git-review--maybe-enable ()
  "Enable `+git-review-buffer-mode' in Magit/Forge/Changes Tree/PR views."
  (when (or (derived-mode-p 'magit-mode)
            (derived-mode-p 'forge-topic-mode)
            (derived-mode-p '+git-changes-tree-mode)
            (derived-mode-p '+git-pr-mode)
            (bound-and-true-p magit-blob-mode))
    (+git-review-buffer-mode 1)))

(defun +git-review--setup-evil-keys ()
  "Bind the Phase 1 review vocabulary in Evil normal state."
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +git-review-buffer-mode-map
      "h" #'evil-backward-char
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line
      "l" #'evil-forward-char
      "H" #'evil-beginning-of-line
      "J" #'+evil/move-lines-down
      "K" #'+evil/move-lines-up
      "L" #'evil-end-of-line
      (kbd "TAB") #'+git-review-section-toggle
      (kbd "<backtab>") #'magit-section-cycle-global
      (kbd "RET") #'+git-review-visit
      "e" #'+git-review-visit-worktree
      "o" #'+git-review-visit-other-window
      "t" #'+git-review-open-changes-tree
      "gf" #'+git-review-next-file
      "gF" #'+git-review-prev-file
      "gh" #'+git-review-next-hunk
      "gH" #'+git-review-prev-hunk
      "gc" #'+git-review-next-commit
      "gC" #'+git-review-prev-commit
      "n" #'evil-search-next
      "N" #'evil-search-previous
      "gr" #'+git-review-refresh
      "q" #'+git-review-quit
      "/" #'evil-search-forward
      "?" #'evil-search-backward
      (kbd "C-h") #'windmove-left
      (kbd "C-l") #'windmove-right
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up)
    (evil-define-key 'visual +git-review-buffer-mode-map
      (kbd "C-y") #'+copy-to-system-clipboard)
    (evil-define-key 'normal +git-review-return-mode-map
      "q" #'+git-review-quit)))

(with-eval-after-load 'evil
  (+git-review--setup-evil-keys)
  ;; Generated Magit modes start in normal state (including first buffer).
  (dolist (mode '(magit-status-mode
                  magit-diff-mode
                  magit-revision-mode
                  magit-log-mode
                  magit-refs-mode
                  magit-stash-mode
                  magit-process-mode
                  +git-changes-tree-mode))
    (evil-set-initial-state mode 'normal)))

(with-eval-after-load 'forge-topic
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'forge-topic-mode 'normal)))

(dolist (hook '(magit-mode-hook
                magit-blob-mode-hook
                forge-topic-mode-hook))
  (add-hook hook #'+git-review--maybe-enable))

;; =============================================================================
;; Phase 2 — Local review targets, Changes Tree, persistence, file diffs
;; =============================================================================
;; Ownership: local review / Changes Tree / diff faces belong in this module
;; per `git-plan.md'.  Canonical identity and local contexts live in
;; `init-git-store.el' (Phase 3).

(defconst +git-review-empty-tree
  "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  "Git's well-known empty-tree object ID (SHA-1).")

(defcustom +git-review-state-directory
  (expand-file-name ".cache/git-review/state/" user-emacs-directory)
  "Directory for atomically persisted local review checkmarks.
Tests should bind this to a temporary directory."
  :type 'directory
  :group 'magit)

(defvar +git-review--git-process-count 0
  "Number of Git processes started by Phase 2 model/collection helpers.
Reset around instrumented public commands in tests.")

(defvar +git-review--max-git-processes-per-refresh 12
  "Hard upper bound for Git processes during one tree refresh.
Far below N-files so accidental per-file loops fail tests loudly.")

(defvar-local +git-review-target nil
  "Buffer-local `+git-review-target' for overview, tree, and file diffs.")

(defvar-local +git-review-file-path nil
  "Buffer-local relative path for a per-file review diff buffer.")

(defvar-local +git-review--files nil
  "Buffer-local list of `+git-review-file' records for the Changes Tree.")

(defvar-local +git-review--reviewed-map nil
  "Buffer-local hash table: path -> fingerprint for currently reviewed files.")

(defvar-local +git-review-edit-context-id nil
  "Active edit context id for shared immutable review buffers.
Separate from immutable target identity so reopen/reuse does not mutate
the reviewed commit/PR identity.")

(cl-defstruct (+git-review-target
               (:constructor +git-review-target--create)
               (:copier nil))
  "Explicit local review target.  Never reconstructed from rendered text."
  root           ; absolute repository root (originating context)
  scope          ; worktree | staged | commit | branch | pullreq
  base-ref       ; symbolic base label (or nil)
  base-oid       ; resolved base object ID
  head-ref       ; symbolic head label (or nil)
  head-oid       ; resolved head object ID (nil for mutable worktree tip)
  family-id      ; stable target-family identity (persistence)
  overview-id    ; stable overview buffer identity
  mutable-p      ; non-nil when stage/unstage/discard are allowed
  repository-id  ; canonical repository identity (Phase 3)
  context-id     ; originating local context identity (Phase 3)
  pr-number)     ; Phase 5: integer PR number when scope is pullreq

(cl-defstruct (+git-review-file
               (:constructor +git-review-file-create)
               (:copier nil))
  "One changed-file record from machine-readable Git output."
  path           ; current path
  old-path       ; previous path for rename/copy, else nil
  status         ; modified|added|deleted|renamed|copied|untracked|binary|submodule
  additions      ; integer or the symbol `binary'
  deletions      ; integer or the symbol `binary'
  old-mode
  new-mode
  old-oid
  new-oid
  fingerprint)  ; content fingerprint for review-state persistence

;; ---------------------------------------------------------------------------
;; Git process helpers (argument lists only; NUL-safe; counted)
;; ---------------------------------------------------------------------------

(defun +git-review--call-git (root &rest args)
  "Run git ARGS in ROOT; return (exit-code . stdout-string).
Increments `+git-review--git-process-count'.  Never builds a shell string."
  (cl-incf +git-review--git-process-count)
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory (expand-file-name root)))
           (process-environment
            (append '("GIT_TERMINAL_PROMPT=0") process-environment))
           (exit (apply #'call-process "git" nil t nil args)))
      (cons exit (buffer-string)))))

(defun +git-review--git-ok (root &rest args)
  "Run git ARGS in ROOT and return stdout, signaling on non-zero exit."
  (let* ((result (apply #'+git-review--call-git root args))
         (exit (car result))
         (out (cdr result)))
    (unless (eq exit 0)
      (error "git %s failed in %s (exit %s): %s"
             (mapconcat #'identity args " ") root exit out))
    out))

(defun +git-review--git-ok-allow (root allowed-exits &rest args)
  "Like `+git-review--git-ok' but tolerate exit codes in ALLOWED-EXITS."
  (let* ((result (apply #'+git-review--call-git root args))
         (exit (car result))
         (out (cdr result)))
    (unless (or (eq exit 0) (memq exit allowed-exits))
      (error "git %s failed in %s (exit %s): %s"
             (mapconcat #'identity args " ") root exit out))
    out))

(defun +git-review--git-items (root &rest args)
  "Run git ARGS in ROOT and split NUL-delimited stdout into a list."
  (let ((out (apply #'+git-review--git-ok-allow root '(1) args)))
    (split-string out "\0" t)))

(defun +git-review--rev-parse (root &rest args)
  "Return `git rev-parse' stdout for ROOT trimmed, or nil on failure."
  (let* ((result (apply #'+git-review--call-git root
                        (append '("rev-parse" "--verify") args)))
         (exit (car result))
         (out (string-trim-right (cdr result))))
    (and (eq exit 0) (not (string-empty-p out)) out)))

(defun +git-review--assert-bounded-processes (context &optional start-count)
  "Signal when Git processes since START-COUNT exceed the Phase 2 bound."
  (let* ((start (or start-count 0))
         (used (- +git-review--git-process-count start)))
    (when (> used +git-review--max-git-processes-per-refresh)
      (error "Phase 2 %s used %d Git processes (bound %d); likely N-per-file"
             context
             used
             +git-review--max-git-processes-per-refresh))))

;; ---------------------------------------------------------------------------
;; Review-target construction
;; ---------------------------------------------------------------------------

(defun +git-review--normalize-root (root)
  "Return ROOT as an absolute directory path with trailing slash removed."
  (directory-file-name (expand-file-name root)))

(defun +git-review--legacy-family-id (root scope base-ref head-ref head-oid
                                           &optional pr-number)
  "Return the Phase 2 root-based family id for legacy state migration."
  (let ((root (+git-review--normalize-root root)))
    (pcase scope
      ((or 'worktree 'staged)
       (format "git-review:%s:%s" scope root))
      ('commit
       (format "git-review:commit:%s:%s" root head-oid))
      ('branch
       (format "git-review:branch:%s:%s:%s"
               root
               (or base-ref "")
               (or head-ref "")))
      ('pullreq
       (format "git-review:pr:%s:%s" root (or pr-number 0)))
      (_ (error "Unknown review scope: %S" scope)))))

(defun +git-review--make-ids (repository-id context-id root scope
                                            base-ref head-ref
                                            base-oid head-oid
                                            &optional pr-number)
  "Return (FAMILY-ID . OVERVIEW-ID) for SCOPE under REPOSITORY-ID.

FAMILY-ID is the persistence identity:
- worktree/staged: repository-id + context-id (never shared across clones)
- branch: repository-id + context-id + symbolic base/head labels
- commit: repository-id + resolved head OID (shared across same-origin clones)
- pullreq: repository-id + PR number (shared; head advance reuses state)

OVERVIEW-ID is the immutable buffer-reuse identity:
- worktree/staged: stable so refresh reuses buffers
- branch: includes resolved base/head OIDs so advancing creates a new buffer
- commit: repository-id + resolved OIDs (shared across same-origin clones)
- pullreq: repository-id + PR number (stable across head OID changes)

ROOT is retained only for legacy Phase 2 state migration lookups."
  (let* ((repo (or repository-id
                   (format "local:%s" (+git-review--normalize-root root))))
         (ctx (or context-id
                  (format "ctx:%s" (+git-review--normalize-root root))))
         (family (pcase scope
                   ((or 'worktree 'staged)
                    (format "git-review:%s:%s:%s" scope repo ctx))
                   ('commit
                    (format "git-review:commit:%s:%s" repo head-oid))
                   ('branch
                    (format "git-review:branch:%s:%s:%s:%s"
                            repo ctx
                            (or base-ref "")
                            (or head-ref "")))
                   ('pullreq
                    (format "git-review:pr:%s:%s" repo (or pr-number 0)))
                   (_ (error "Unknown review scope: %S" scope))))
         (overview (pcase scope
                     ((or 'worktree 'staged)
                      (concat family ":overview"))
                     ('commit
                      (format "git-review:commit:%s:%s:%s:overview"
                              repo
                              (or base-oid +git-review-empty-tree)
                              head-oid))
                     ('branch
                      (format "git-review:branch:%s:%s:%s:%s:overview"
                              repo ctx base-oid head-oid))
                     ('pullreq
                      (format "git-review:pr:%s:%s:overview"
                              repo (or pr-number 0)))
                     (_ (error "Unknown review scope: %S" scope)))))
    (cons family overview)))

(defun +git-review-make-target (root scope &optional base-ref head-ref
                                     base-oid head-oid
                                     repository-id context-id
                                     pr-number)
  "Construct a `+git-review-target' for ROOT and SCOPE.
BASE-REF/HEAD-REF are display labels and persistence selectors.
BASE-OID/HEAD-OID are resolved object IDs when applicable.
REPOSITORY-ID and CONTEXT-ID come from the store when available.
PR-NUMBER is required when SCOPE is `pullreq'."
  (let* ((root (+git-review--normalize-root root))
         (mutable (and (memq scope '(worktree staged)) t))
         (ids (+git-review--make-ids repository-id context-id root scope
                                     base-ref head-ref
                                     base-oid head-oid
                                     pr-number)))
    (+git-review-target--create
     :root root
     :scope scope
     :base-ref base-ref
     :base-oid base-oid
     :head-ref head-ref
     :head-oid head-oid
     :family-id (car ids)
     :overview-id (cdr ids)
     :mutable-p mutable
     :repository-id repository-id
     :context-id context-id
     :pr-number pr-number)))

(defun +git-review--require-root ()
  "Return the Magit toplevel or signal a user error."
  (require 'magit)
  (or (magit-toplevel)
      (user-error "Not inside a Git repository")))

(defun +git-review--inside-worktree-p (root)
  "Return non-nil when ROOT is a Git worktree rather than a bare repository."
  (equal (ignore-errors
           (string-trim
            (+git-review--git-ok
             root "rev-parse" "--is-inside-work-tree")))
         "true"))

(defun +git-review--local-entry-root ()
  "Return the local worktree root for a new worktree/staged review.
PR buffers intentionally bind `default-directory' to the shared bare
mirror for object correctness.  When a local review is launched from
such a buffer, use its active edit context instead of treating the mirror
as a worktree."
  (let* ((target (and (bound-and-true-p +git-review-target)
                      +git-review-target))
         (root (if (and target (+git-review--pr-backed-p target))
                   (+git-review--active-edit-root target)
                 (+git-review--require-root))))
    (unless (+git-review--inside-worktree-p root)
      (user-error
       "Local worktree review requires a clone/worktree, not bare repository %s"
       root))
    root))

(defun +git-review--context-for-root (root)
  "Register/refresh ROOT in the store and return its local context."
  (+git-store-context-for-root root))

(defun +git-review-target-for-worktree (&optional root)
  "Build a worktree review target for ROOT (default: current repository)."
  (let ((root (+git-review--normalize-root
               (or root (+git-review--local-entry-root)))))
    (unless (+git-review--inside-worktree-p root)
      (user-error "Working-tree review cannot use bare repository %s" root))
    (let* ((ctx (+git-review--context-for-root root))
           (head (or (+git-review--rev-parse root "HEAD")
                     +git-review-empty-tree)))
      (+git-review-make-target
       root 'worktree "HEAD" nil head nil
       (+git-store-local-context-repository-id ctx)
       (+git-store-local-context-context-id ctx)))))

(defun +git-review-target-for-staged (&optional root)
  "Build a staged review target for ROOT."
  (let ((root (+git-review--normalize-root
               (or root (+git-review--local-entry-root)))))
    (unless (+git-review--inside-worktree-p root)
      (user-error "Staged review cannot use bare repository %s" root))
    (let* ((ctx (+git-review--context-for-root root))
           (head (or (+git-review--rev-parse root "HEAD")
                     +git-review-empty-tree)))
      (+git-review-make-target
       root 'staged "HEAD" "index" head nil
       (+git-store-local-context-repository-id ctx)
       (+git-store-local-context-context-id ctx)))))

(defun +git-review-target-for-commit (commit &optional root)
  "Build a commit review target for COMMIT in ROOT.
Root commits use `+git-review-empty-tree' as the base."
  (let* ((root (+git-review--normalize-root
                (or root (+git-review--require-root))))
         (ctx (+git-review--context-for-root root))
         (head-oid (or (+git-review--rev-parse root commit)
                       (user-error "Cannot resolve commit %s" commit)))
         (parent (+git-review--rev-parse root (concat head-oid "^")))
         (base-oid (or parent +git-review-empty-tree))
         (base-ref (if parent (concat commit "^") "empty-tree")))
    (+git-review-make-target
     root 'commit base-ref commit
     base-oid head-oid
     (+git-store-local-context-repository-id ctx)
     (+git-store-local-context-context-id ctx))))

(defun +git-review-target-for-branch (base head &optional root)
  "Build a branch review target for merge-base(BASE,HEAD)..HEAD."
  (let* ((root (+git-review--normalize-root
                (or root (+git-review--require-root))))
         (ctx (+git-review--context-for-root root))
         (base-oid (or (+git-review--rev-parse root base)
                       (user-error "Cannot resolve base %s" base)))
         (head-oid (or (+git-review--rev-parse root head)
                       (user-error "Cannot resolve head %s" head)))
         (merge-base (or (string-trim-right
                          (+git-review--git-ok root "merge-base"
                                               base-oid head-oid))
                         (user-error "No merge-base for %s and %s"
                                     base head))))
    (+git-review-make-target
     root 'branch base head
     merge-base head-oid
     (+git-store-local-context-repository-id ctx)
     (+git-store-local-context-context-id ctx))))

(defun +git-review-target-for-pullreq (repository-id number
                                                     merge-base-oid head-oid
                                                     &optional root
                                                     context-id
                                                     base-ref head-ref)
  "Build a read-only PR review target for REPOSITORY-ID and NUMBER.
MERGE-BASE-OID..HEAD-OID is the exact review range."
  (let* ((root (+git-review--normalize-root
                (or root (+git-review--require-root))))
         (ctx-id (or context-id
                     (+git-store-local-context-context-id
                      (+git-review--context-for-root root)))))
    (+git-review-make-target
     root 'pullreq
     (or base-ref "base")
     (or head-ref (format "PR#%s" number))
     merge-base-oid head-oid
     repository-id ctx-id
     number)))

(defun +git-review-target-range-args (target)
  "Return (RANGE TYPEARG) Git diff arguments for TARGET.
RANGE is a string or nil.  TYPEARG is \"--cached\" or nil."
  (pcase (+git-review-target-scope target)
    ('worktree (list (+git-review-target-base-oid target) nil))
    ('staged (list (+git-review-target-base-oid target) "--cached"))
    ((or 'commit 'branch 'pullreq)
     (list (format "%s..%s"
                   (+git-review-target-base-oid target)
                   (+git-review-target-head-oid target))
           nil))
    (_ (error "Unknown review scope: %S"
              (+git-review-target-scope target)))))

(defun +git-review-target-diff-args (_target)
  "Return Magit/Git diff argument list for TARGET overview/file buffers."
  (list "--no-ext-diff"
        (format "-U%d" (if (boundp '+git/review-context-lines)
                           +git/review-context-lines
                         6))
        "-M"
        "-C"
        "--find-copies-harder"))

;; ---------------------------------------------------------------------------
;; Active edit context (Phase 3 / Phase 5)
;; ---------------------------------------------------------------------------

(defun +git-review--shareable-p (&optional target)
  "Return non-nil when TARGET is a shareable immutable review (commit or PR).
PR commit children (commit scope with a PR number) are shareable too."
  (let ((target (or target
                    (and (bound-and-true-p +git-review-target)
                         +git-review-target))))
    (and target
         (or (memq (+git-review-target-scope target) '(commit pullreq))
             (+git-review--pr-backed-p target)))))
(defun +git-review--shareable-commit-p (&optional target)
  "Return non-nil when TARGET is a shareable immutable commit review.
Kept for callers that need commit-only behavior; prefer
`+git-review--shareable-p' for edit-context switching."
  (let ((target (or target
                    (and (bound-and-true-p +git-review-target)
                         +git-review-target))))
    (and target (eq (+git-review-target-scope target) 'commit))))

(defun +git-review--pr-backed-p (&optional target)
  "Return non-nil when TARGET reads Git objects from the published mirror.
True for `pullreq' scopes and for commit children that carry a PR number."
  (let ((target (or target
                    (and (bound-and-true-p +git-review-target)
                         +git-review-target))))
    (and target
         (or (eq (+git-review-target-scope target) 'pullreq)
             (and (eq (+git-review-target-scope target) 'commit)
                  (integerp (+git-review-target-pr-number target)))))))

(defun +git-review--edit-context-oids (target)
  "Return OIDs that must exist in an edit context for TARGET, or nil.
PR reviews and PR commit children read Git objects from the published
mirror, so edit-context eligibility is repository-identity only."
  (when (and (+git-review--shareable-commit-p target)
             (not (+git-review--pr-backed-p target)))
    (+git-review--target-required-oids target)))

(defun +git-review--target-required-oids (target)
  "Return object IDs that must exist in an edit context for TARGET.
Skips the well-known empty tree, which may not be materialized."
  (cl-remove-if
   (lambda (oid)
     (or (null oid)
         (equal oid +git-review-empty-tree)))
   (list (+git-review-target-base-oid target)
         (+git-review-target-head-oid target))))

(defun +git-review--published-mirror-root (repository-id)
  "Return the published bare mirror for REPOSITORY-ID, or signal."
  (let ((mirror (and (fboundp '+git-sync-published-mirror-directory)
                     (+git-sync-published-mirror-directory repository-id))))
    (unless (and mirror
                 (file-directory-p mirror)
                 (file-exists-p (expand-file-name "HEAD" mirror)))
      (user-error
       (concat "No published mirror for %s. "
               "Run C-c g f, wait for sync, then retry.")
       repository-id))
    mirror))

(defun +git-review--git-root-for-target (target)
  "Return the Git directory used for object/diff operations on TARGET.
PR reviews and PR commit children use the Phase 4 published bare mirror.
Ordinary shareable commit reviews use the active/eligible edit context.
Local worktree, staged, and branch reviews always use the originating
root."
  (cond
   ((+git-review--pr-backed-p target)
    (+git-review--published-mirror-root
     (+git-review-target-repository-id target)))
   ((+git-review--shareable-commit-p target)
    (let* ((preferred
            (or (and (bound-and-true-p +git-review-edit-context-id)
                     +git-review-edit-context-id)
                (+git-review-target-context-id target)))
           (oids (+git-review--target-required-oids target))
           (repo-id (+git-review-target-repository-id target))
           (preferred-ctx (and preferred
                               (+git-store-get-context preferred)))
           (ctx
            (cond
             ((+git-store-context-eligible-p preferred-ctx repo-id oids)
              preferred-ctx)
             (t
              (+git-store-default-edit-context repo-id preferred oids)))))
      (unless ctx
        (user-error
         "No eligible local context for this commit review"))
      (when (and (bound-and-true-p +git-review-target)
                 (equal (+git-review-target-overview-id +git-review-target)
                        (+git-review-target-overview-id target)))
        (setq-local +git-review-edit-context-id
                    (+git-store-local-context-context-id ctx)))
      (+git-store-local-context-root ctx)))
   (t (+git-review-target-root target))))

(defun +git-review--bind-operational-directory (root)
  "Bind Magit and buffer directories to absolute object-operation ROOT."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (setq-local default-directory root)
    (when (boundp 'magit--default-directory)
      (setq-local magit--default-directory root))
    root))

(defun +git-review--adopt-reopening-context (target)
  "Adopt TARGET's originating clone as this buffer's active edit context.
Only for shareable immutable reviews, and only when that context is live
for TARGET's repository.  Then derives Magit's operational directory from
TARGET: an eligible clone for ordinary commits, or the published mirror for
PR/PR-commit objects.  Immutable review identity is unchanged."
  (when (+git-review--shareable-p target)
    (let* ((ctx-id (+git-review-target-context-id target))
           (ctx (+git-store-get-context ctx-id))
           (oids (+git-review--edit-context-oids target))
           (repo-id (+git-review-target-repository-id target)))
      (cond
       ((+git-store-context-eligible-p ctx repo-id oids)
        (setq-local +git-review-edit-context-id ctx-id))
       (t
        (+git-review--ensure-edit-context target)))
      (+git-store-remember-shared-buffer
       (+git-review-target-repository-id target)
       (current-buffer))))
  (+git-review--bind-operational-directory
   (+git-review--git-root-for-target target))
  target)

(defun +git-review--identity-suffix (&optional target edit-context-id)
  "Return a compact ASCII identity suffix for TARGET.
Example: \"repo: github.com/org/dragon | context: ~/work/dragon\""
  (let* ((target (or target +git-review-target))
         (repo (and target (+git-review-target-repository-id target)))
         (ctx-id (or edit-context-id
                     (and (bound-and-true-p +git-review-edit-context-id)
                          +git-review-edit-context-id)
                     (and target (+git-review-target-context-id target))))
         (ctx (and ctx-id (+git-store-get-context ctx-id)))
         (root (or (and ctx (+git-store-local-context-root ctx))
                   (and target (+git-review-target-root target)))))
    (format "repo: %s | context: %s"
            (or repo "?")
            (or root "?"))))

(defun +git-review--ensure-edit-context (&optional target)
  "Ensure `+git-review-edit-context-id' is set for TARGET in the current buffer.
For shareable reviews, prefer an eligible live context.  For local scopes,
lock to the originating context.  PR reviews do not require commit OIDs
in the edit context."
  (when-let ((target (or target +git-review-target)))
    (let* ((repo-id (+git-review-target-repository-id target))
           (origin (+git-review-target-context-id target))
           (oids (+git-review--edit-context-oids target))
           (chosen
            (if (+git-review--shareable-p target)
                (or (+git-store-default-edit-context
                     repo-id
                     (or +git-review-edit-context-id origin)
                     oids)
                    (user-error
                     "No registered local context for this shared review"))
              (or (+git-store-get-context origin)
                  (+git-store-context-for-root
                   (+git-review-target-root target))))))
      (setq-local +git-review-edit-context-id
                  (+git-store-local-context-context-id chosen))
      chosen)))

(defun +git-review--active-edit-root (&optional target)
  "Return the absolute root of the active edit context for TARGET.
When there is no Phase 2/3 review target, fall back to Magit's toplevel
so Phase 1 Magit status/diff `e' visits keep working."
  (let ((target (or target
                    (and (bound-and-true-p +git-review-target)
                         +git-review-target))))
    (if (null target)
        (or (and (fboundp 'magit-toplevel) (magit-toplevel))
            (user-error "Not inside a Git repository"))
      (let* ((repo-id (+git-review-target-repository-id target))
             (oids (+git-review--edit-context-oids target))
             (ctx (+git-review--ensure-edit-context target)))
        (unless (+git-store-context-eligible-p ctx repo-id oids)
          ;; Originating context disappeared or was re-homed: try another
          ;; eligible context for shareable reviews, otherwise signal.
          (if (+git-review--shareable-p target)
              (let ((fallback
                     (+git-store-default-edit-context repo-id nil oids)))
                (unless fallback
                  (user-error
                   "Active edit context disappeared; no eligible local context remains"))
                (setq-local +git-review-edit-context-id
                            (+git-store-local-context-context-id fallback))
                (setq ctx fallback))
            (user-error
             "Local review context `%s' is no longer available"
             (or (+git-review-target-root target) "?"))))
        (+git-store-local-context-root ctx)))))

(defun +git-review-visit-worktree ()
  "Visit the writable worktree file for the diff or tree file at point.
Uses the active edit context root for shared immutable commit/PR reviews.
Signals a clear user error when no writable worktree target exists.
Does not force Insert state.  Generated per-file buffers may fall back to
buffer-local `+git-review-file-path' when Magit file sections are missing."
  (interactive)
  (cond
   ((derived-mode-p '+git-pr-mode)
    (let* ((file (and (fboundp '+git-pr--file-at-point)
                      (+git-pr--file-at-point)))
           (root (+git-review--active-edit-root))
           (rel (and file (+git-review-file-path file)))
           (full (and rel root (expand-file-name rel root))))
      (cond
       ((null file)
        (user-error "No file at point"))
       ((eq (+git-review-file-status file) 'deleted)
        (user-error "File `%s' was deleted in this review; no worktree target"
                    rel))
       ((eq (+git-review-file-status file) 'submodule)
        (user-error
         "Submodule/gitlink `%s' - open that repository separately"
         rel))
       ((not (and full (file-exists-p full) (not (file-directory-p full))))
        (user-error "No writable worktree file at `%s' in edit context %s"
                    (or rel "?") root))
       (t
        (let ((state (+git-review--capture-return))
              (before (current-buffer)))
          (find-file full)
          (let ((after (current-buffer)))
            (when (and (not (eq after before)) (buffer-live-p after))
              (+git-review--enable-return-on-buffer after state))))))))
   ((derived-mode-p '+git-changes-tree-mode)
    (let* ((file (+git-review--file-at-tree-point))
           (root (+git-review--active-edit-root))
           (rel (and file (+git-review-file-path file)))
           (full (and rel root (expand-file-name rel root))))
      (cond
       ((and file (eq (+git-review-file-status file) 'deleted))
        (user-error "File `%s' was deleted in this review; no worktree target"
                    rel))
       ((not (and full (file-exists-p full) (not (file-directory-p full))))
        (user-error "No writable worktree target at point"))
       (t
        (let ((state (+git-review--capture-return))
              (before (current-buffer)))
          (find-file full)
          (let ((after (current-buffer)))
            (when (and (not (eq after before)) (buffer-live-p after))
              (+git-review--enable-return-on-buffer after state))))))))
   (t
    (let* ((has-target (bound-and-true-p +git-review-target))
           (root (+git-review--active-edit-root))
           (origin-root (or (and has-target
                                 (+git-review-target-root +git-review-target))
                            root))
           (magit-file (and (fboundp 'magit-diff--file)
                            (ignore-errors (magit-diff--file))))
           (rel (or (and magit-file origin-root
                         (file-relative-name magit-file origin-root))
                    (and (bound-and-true-p +git-review-file-path)
                         +git-review-file-path)))
           (full (and rel root (expand-file-name rel root))))
      (cond
       ((eq +git-review--presented-status 'submodule)
        (user-error
         "Submodule/gitlink `%s' - open that repository separately to review its commits"
         (or rel +git-review-file-path "unknown")))
       ((and full (file-directory-p full))
        (user-error "No writable worktree file at `%s'" (or rel full)))
       ((not (and full (file-exists-p full)))
        (user-error "No writable worktree target at point"))
       (t
        (let ((state (+git-review--capture-return))
              (before (current-buffer)))
          (condition-case err
              (cond
               ((and has-target (+git-review--shareable-p))
                (find-file full))
               ((and magit-file (equal root origin-root))
                (magit-diff-visit-worktree-file nil))
               (t (find-file full)))
            (error
             (let ((msg (error-message-string err)))
               (if (string-match-p
                    "Cannot determine file\\|No file\\|does not exist\\|not exist"
                    msg)
                   (find-file full)
                 (signal (car err) (cdr err))))))
          (let ((after (current-buffer)))
            (when (and (not (eq after before)) (buffer-live-p after))
              (+git-review--enable-return-on-buffer after state))))))))))

(defun +git-review-set-edit-context (context-id &optional target)
  "Set the active edit context to CONTEXT-ID for TARGET (noninteractive).
Shareable commit/PR reviews may select any eligible live context for the
same canonical repository.  Worktree/staged/branch reviews reject
retargeting.  Returns the chosen `+git-store-local-context'."
  (let* ((target (or target +git-review-target
                     (user-error "No review target")))
         (repo-id (+git-review-target-repository-id target))
         (ctx (+git-store-get-context context-id))
         (oids (+git-review--edit-context-oids target)))
    (unless ctx
      (user-error "Unknown local context `%s'" context-id))
    (unless (+git-review--shareable-p target)
      (user-error
       "Worktree, staged, and local branch reviews are fixed to their originating context (%s)"
       (+git-review-target-root target)))
    (unless (+git-store-context-eligible-p ctx repo-id oids)
      (cond
       ((not (equal (+git-store-local-context-repository-id ctx) repo-id))
        (user-error "Context `%s' belongs to a different repository" context-id))
       ((not (and (+git-store-local-context-available-p ctx)
                  (file-directory-p (+git-store-local-context-root ctx))))
        (user-error "Local context `%s' is not available"
                    (+git-store-local-context-root ctx)))
       (t
        (user-error
         "Context `%s' does not contain a required reviewed object"
         (+git-store-local-context-root ctx)))))
    (setq-local +git-review-edit-context-id context-id)
    (+git-review--bind-operational-directory
     (+git-review--git-root-for-target target))
    (message "Edit context: %s" (+git-store-local-context-root ctx))
    ctx))

(defun +git-review-select-edit-context (&optional context-id)
  "Select the active edit context for the current shareable review (`L').
With CONTEXT-ID, select noninteractively.  Otherwise prompt among
eligible live contexts.  Preserves Phase 1 same-window and `q' return
behavior (selection does not change windows)."
  (interactive)
  (unless (bound-and-true-p +git-review-target)
    (user-error "No review target in this buffer"))
  (let* ((target +git-review-target)
         (repo-id (+git-review-target-repository-id target)))
    (unless (+git-review--shareable-p target)
      (user-error
       "Worktree, staged, and local branch reviews are fixed to their originating context (%s); L only applies to shared commit/PR reviews"
       (+git-review-target-root target)))
    (let* ((oids (+git-review--edit-context-oids target))
           (eligible (if oids
                         (+git-store-eligible-contexts-for-oids repo-id oids)
                       (+git-store-list-live-contexts repo-id)))
           (chosen-id
            (or context-id
                (progn
                  (unless eligible
                    (user-error
                     "No registered local context for this shared review"))
                  (let* ((collection
                          (mapcar
                           (lambda (ctx)
                             (cons (+git-store-local-context-root ctx)
                                   (+git-store-local-context-context-id ctx)))
                           eligible))
                         (root (completing-read
                                "Edit context: "
                                (mapcar #'car collection)
                                nil t nil nil
                                (car (car collection)))))
                    (cdr (assoc root collection)))))))
      (+git-review-set-edit-context chosen-id target))))

;; ---------------------------------------------------------------------------
;; Changed-file model (NUL parsers; bounded Git processes)
;; ---------------------------------------------------------------------------

(defun +git-review--status-from-code (code)
  "Map a Git name-status CODE character/string to a status symbol."
  (let ((c (if (stringp code) (aref code 0) code)))
    (pcase c
      (?M 'modified)
      (?A 'added)
      (?D 'deleted)
      (?R 'renamed)
      (?C 'copied)
      (?T 'modified)
      (?U 'modified)
      (?\? 'untracked)
      (_ 'modified))))

(defun +git-review--parse-name-status-items (items)
  "Parse NUL `git diff --name-status -z' ITEMS into an alist of records.
Each record is (PATH OLD-PATH STATUS-CODE)."
  (let ((i 0)
        (n (length items))
        records)
    (while (< i n)
      (let* ((status (nth i items))
             (code (and status (substring status 0 1))))
        (cond
         ((member code '("R" "C"))
          (when (>= (+ i 2) n)
            (error "Truncated rename/copy name-status at %S" status))
          (push (list (nth (+ i 2) items)  ; new path
                      (nth (+ i 1) items)  ; old path
                      code)
                records)
          (setq i (+ i 3)))
         (t
          (when (>= (+ i 1) n)
            (error "Truncated name-status at %S" status))
          (push (list (nth (+ i 1) items) nil code) records)
          (setq i (+ i 2))))))
    (nreverse records)))

(defun +git-review--parse-numstat-items (items)
  "Parse NUL `git diff --numstat -z' ITEMS into hash PATH -> (ADD . DEL).

Ordinary records are one NUL field `ADDS\\tDELS\\tPATH'.  Rename/copy
records are `ADDS\\tDELS\\t' followed by two more NUL fields (old path,
then new path).  Binary files map to (binary . binary)."
  (let ((table (make-hash-table :test #'equal))
        (i 0)
        (n (length items)))
    (while (< i n)
      (let ((item (nth i items)))
        (if (not (string-match-p "\\`[-0-9]+\t[-0-9]+\t" item))
            (setq i (1+ i))
          (let* ((parts (split-string item "\t"))
                 (adds (nth 0 parts))
                 (dels (nth 1 parts))
                 (path (nth 2 parts))
                 (counts (if (or (equal adds "-") (equal dels "-"))
                             (cons 'binary 'binary)
                           (cons (string-to-number adds)
                                 (string-to-number dels)))))
            (setq i (1+ i))
            (cond
             ((and path (not (string-empty-p path)))
              (puthash path counts table))
             (t
              (when (>= (+ i 1) n)
                (error "Truncated rename/copy numstat near %S" item))
              (let ((_old (nth i items))
                    (new (nth (1+ i) items)))
                (puthash new counts table)
                (setq i (+ i 2)))))))))
    table))

(defun +git-review--parse-raw-items (items)
  "Parse NUL `git diff --raw -z' ITEMS into hash PATH -> plist.
Plist keys: :old-mode :new-mode :old-oid :new-oid :status :old-path."
  (let ((table (make-hash-table :test #'equal))
        (i 0)
        (n (length items)))
    (while (< i n)
      (let* ((meta (nth i items)))
        (unless (and meta (string-prefix-p ":" meta))
          (error "Unexpected raw diff item: %S" meta))
        (let* ((parts (split-string (substring meta 1) " " t))
               (old-mode (nth 0 parts))
               (new-mode (nth 1 parts))
               (old-oid (nth 2 parts))
               (new-oid (nth 3 parts))
               (status (nth 4 parts))
               (code (and status (substring status 0 1))))
          (cond
           ((member code '("R" "C"))
            (let ((old-path (nth (+ i 1) items))
                  (new-path (nth (+ i 2) items)))
              (puthash new-path
                       (list :old-mode old-mode :new-mode new-mode
                             :old-oid old-oid :new-oid new-oid
                             :status code :old-path old-path)
                       table)
              (setq i (+ i 3))))
           (t
            (let ((path (nth (+ i 1) items)))
              (puthash path
                       (list :old-mode old-mode :new-mode new-mode
                             :old-oid old-oid :new-oid new-oid
                             :status code :old-path nil)
                       table)
              (setq i (+ i 2))))))))
    table))

(defun +git-review--hash-paths (root paths)
  "Return hash table PATH -> object ID for PATHS under ROOT in one process.
Uses a single `git hash-object --stdin-paths' invocation.
Paths are LF-separated (safe for spaces and Unicode; Git's
`--stdin-paths' does not accept `-z' on all Git versions)."
  (let ((table (make-hash-table :test #'equal)))
    (when paths
      (cl-incf +git-review--git-process-count)
      (with-temp-buffer
        (dolist (p paths)
          (when (string-match-p "\n" p)
            (error "Path contains newline; cannot batch-hash: %S" p))
          (insert p "\n"))
        (let* ((default-directory
                (file-name-as-directory (expand-file-name root)))
               (exit (call-process-region
                      (point-min) (point-max)
                      "git" t t nil
                      "hash-object" "--stdin-paths")))
          (unless (eq exit 0)
            (error "git hash-object --stdin-paths failed in %s: %s"
                   root (buffer-string)))
          (let ((oids (split-string (buffer-string) "\n" t)))
            (unless (= (length oids) (length paths))
              (error "hash-object returned %d ids for %d paths"
                     (length oids) (length paths)))
            (cl-loop for path in paths
                     for oid in oids
                     do (puthash path oid table))))))
    table))
(defun +git-review--zero-oid-p (oid)
  "Return non-nil when OID is missing or an all-zero placeholder."
  (or (null oid)
      (string-match-p "\\`0+\\'" oid)))

(defun +git-review--classify-file (status-code raw-meta _num adds _dels)
  "Return status symbol from STATUS-CODE, RAW-META, and ADD counts."
  (let* ((mode (or (plist-get raw-meta :new-mode)
                   (plist-get raw-meta :old-mode)))
         (binary (eq adds 'binary)))
    (cond
     ((and mode (string-prefix-p "160000" mode)) 'submodule)
     ((equal status-code "?") 'untracked)
     (binary 'binary)
     (t (+git-review--status-from-code status-code)))))

(defun +git-review-collect-files (target)
  "Return a list of `+git-review-file' for TARGET using batched Git calls.
Never parses Magit buffer text.  Asserts a bounded process count.
Shareable commit reviews collect from the operational edit-context root."
  (let* ((root (+git-review--git-root-for-target target))
         (scope (+git-review-target-scope target))
         (start-count +git-review--git-process-count)
         (range-type (+git-review-target-range-args target))
         (range (car range-type))
         (typearg (cadr range-type))
         (diff-base (append (list "diff" "--no-ext-diff" "-M" "-C"
                                  "--find-copies-harder" "-z")
                            (and typearg (list typearg))
                            (list range)))
         (ns-items (apply #'+git-review--git-items root
                          (append diff-base '("--name-status"))))
         (num-items (apply #'+git-review--git-items root
                           (append diff-base '("--numstat"))))
         (raw-items (apply #'+git-review--git-items root
                           (append diff-base '("--raw"))))
         (ns-records (+git-review--parse-name-status-items ns-items))
         (num-table (+git-review--parse-numstat-items num-items))
         (raw-table (+git-review--parse-raw-items raw-items))
         (untracked
          (when (eq scope 'worktree)
            (+git-review--git-items
             root "ls-files" "-z" "--others" "--exclude-standard")))
         (need-hash nil)
         files)
    (dolist (rec ns-records)
      (pcase-let* ((`(,path ,old-path ,code) rec)
                   (raw (gethash path raw-table))
                   (num (or (gethash path num-table)
                            (and old-path (gethash old-path num-table))))
                   (adds (car num))
                   (dels (cdr num))
                   (status (+git-review--classify-file code raw num adds dels))
                   (old-oid (plist-get raw :old-oid))
                   (new-oid (plist-get raw :new-oid))
                   (fp nil))
        (cond
         ((eq status 'untracked)
          (push path need-hash))
         ((and (eq scope 'worktree)
               (not (eq status 'deleted))
               (or (+git-review--zero-oid-p new-oid)
                   (eq status 'modified)))
          (push path need-hash))
         ((not (+git-review--zero-oid-p new-oid))
          (setq fp new-oid))
         ((not (+git-review--zero-oid-p old-oid))
          (setq fp old-oid))
         (t (setq fp (format "%s:%s" status path))))
        (push (+git-review-file-create
               :path path
               :old-path (or old-path (plist-get raw :old-path))
               :status status
               :additions (or adds 0)
               :deletions (or dels 0)
               :old-mode (plist-get raw :old-mode)
               :new-mode (plist-get raw :new-mode)
               :old-oid old-oid
               :new-oid new-oid
               :fingerprint fp)
              files)))
    (dolist (path untracked)
      (push path need-hash)
      (push (+git-review-file-create
             :path path
             :old-path nil
             :status 'untracked
             :additions 0
             :deletions 0
             :old-mode nil
             :new-mode nil
             :old-oid nil
             :new-oid nil
             :fingerprint nil)
            files))
    (setq need-hash (cl-delete-duplicates need-hash :test #'equal))
    (let ((hashes (+git-review--hash-paths root need-hash)))
      (dolist (file files)
        (unless (+git-review-file-fingerprint file)
          (setf (+git-review-file-fingerprint file)
                (or (gethash (+git-review-file-path file) hashes)
                    (format "missing:%s"
                            (+git-review-file-path file)))))))
    ;; Line counts for untracked: treat whole file as additions via wc
    ;; using one batched approach — read sizes from hash-object already
    ;; done; approximate with `git diff --numstat --no-index' would be
    ;; N processes.  Instead count lines in-process for untracked only
    ;; when the set is small enough; for large trees leave 0 and let the
    ;; file diff show the patch.  Prefer a single Python/awk-free loop:
    (dolist (file files)
      (when (eq (+git-review-file-status file) 'untracked)
        (let* ((full (expand-file-name (+git-review-file-path file) root))
               (attrs (and (file-regular-p full) (file-attributes full)))
               (size (and attrs (file-attribute-size attrs))))
          (cond
           ((null attrs)
            (setf (+git-review-file-additions file) 0))
           ((or (not size) (> size (* 2 1024 1024)))
            (setf (+git-review-file-additions file) 'binary)
            (setf (+git-review-file-deletions file) 'binary)
            (setf (+git-review-file-status file) 'binary))
           (t
            (with-temp-buffer
              (insert-file-contents full)
              (setf (+git-review-file-additions file)
                    (count-lines (point-min) (point-max)))
              (setf (+git-review-file-deletions file) 0)))))))
    (+git-review--assert-bounded-processes
     (format "collect-files (+%d)"
             (- +git-review--git-process-count start-count))
     start-count)
    (cl-sort files #'string< :key #'+git-review-file-path)))

;; ---------------------------------------------------------------------------
;; Review-state persistence (atomic; fingerprint keyed; Phase 3 migration)
;; ---------------------------------------------------------------------------

(defun +git-review--state-file-for-family (family-id)
  "Return the absolute state filename for FAMILY-ID."
  (let ((hash (secure-hash 'sha256 family-id)))
    (expand-file-name (concat hash ".eld")
                      (file-name-as-directory +git-review-state-directory))))

(defun +git-review--state-file (target)
  "Return the absolute Phase 3 state filename for TARGET."
  (+git-review--state-file-for-family
   (+git-review-target-family-id target)))

(defun +git-review--legacy-state-file (target)
  "Return the Phase 2 root-based state filename for TARGET, if distinct."
  (let* ((legacy (+git-review--legacy-family-id
                  (+git-review-target-root target)
                  (+git-review-target-scope target)
                  (+git-review-target-base-ref target)
                  (+git-review-target-head-ref target)
                  (+git-review-target-head-oid target)
                  (+git-review-target-pr-number target)))
         (current (+git-review-target-family-id target)))
    (unless (equal legacy current)
      (+git-review--state-file-for-family legacy))))

(defun +git-review--ensure-state-dir ()
  "Create `+git-review-state-directory' when missing."
  (make-directory +git-review-state-directory t))

(defun +git-review--read-state-file (file)
  "Read path->fingerprint entries from FILE into a new hash table.
Malformed state returns an empty table after a message."
  (let ((table (make-hash-table :test #'equal)))
    (when (and file (file-readable-p file))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let* ((data (read (current-buffer)))
                   (entries (cdr (assq 'entries data))))
              (unless (and (listp data) (assq 'version data) (listp entries))
                (error "Malformed review state"))
              (dolist (pair entries)
                (when (and (consp pair)
                           (stringp (car pair))
                           (stringp (cdr pair)))
                  (puthash (car pair) (cdr pair) table)))))
        (error
         (message "Ignoring malformed Git review state %s: %s"
                  file (error-message-string err))
         (clrhash table))))
    table))

(defun +git-review-state-load (target)
  "Load reviewed path->fingerprint map for TARGET.
Uses the Phase 3 canonical/context-aware state file when present.
Otherwise migrates once from the exact legacy Phase 2 root-based file
without deleting or rewriting that legacy file."
  (let* ((new-file (+git-review--state-file target))
         (legacy-file (+git-review--legacy-state-file target))
         (table nil)
         (migrated nil))
    (cond
     ((file-readable-p new-file)
      (setq table (+git-review--read-state-file new-file)))
     ((and legacy-file (file-readable-p legacy-file))
      (setq table (+git-review--read-state-file legacy-file)
            migrated t)
      ;; Write the new generation atomically; leave the legacy file intact.
      (when (> (hash-table-count table) 0)
        (+git-review-state-save target table)))
     (t
      (setq table (make-hash-table :test #'equal))))
    (when migrated
      (message "Migrated Git review state to canonical identity for %s"
               (+git-review-target-family-id target)))
    table))

(defun +git-review-state-save (target reviewed-map)
  "Atomically persist REVIEWED-MAP for TARGET.
Writes a temporary file in the same directory, then renames into place."
  (+git-review--ensure-state-dir)
  (let* ((file (+git-review--state-file target))
         (dir (file-name-directory file))
         (tmp (make-temp-file
               (expand-file-name ".git-review-state-" dir) nil ".tmp"))
         (entries nil))
    (maphash (lambda (path fp)
               (push (cons path fp) entries))
             reviewed-map)
    (setq entries (cl-sort entries #'string< :key #'car))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((print-level nil)
                  (print-length nil))
              (prin1 (list (cons 'version 1)
                           (cons 'family-id
                                 (+git-review-target-family-id target))
                           (cons 'repository-id
                                 (+git-review-target-repository-id target))
                           (cons 'context-id
                                 (+git-review-target-context-id target))
                           (cons 'entries entries))
                     (current-buffer))
              (insert "\n")))
          (rename-file tmp file t)
          (setq tmp nil))
      (when (and tmp (file-exists-p tmp))
        (ignore-errors (delete-file tmp)))))
  reviewed-map)

(defun +git-review--file-reviewed-p (file reviewed-map)
  "Return non-nil when FILE is reviewed under REVIEWED-MAP."
  (let ((fp (gethash (+git-review-file-path file) reviewed-map)))
    (and fp (equal fp (+git-review-file-fingerprint file)))))

(defun +git-review--sync-reviewed-map (files reviewed-map)
  "Drop stale paths from REVIEWED-MAP and keep fingerprint matches in FILES.
Returns a new hash table."
  (let ((next (make-hash-table :test #'equal)))
    (dolist (file files)
      (when (+git-review--file-reviewed-p file reviewed-map)
        (puthash (+git-review-file-path file)
                 (+git-review-file-fingerprint file)
                 next)))
    next))

;; ---------------------------------------------------------------------------
;; Folder tree model
;; ---------------------------------------------------------------------------

(defun +git-review--path-segments (path)
  "Split PATH into directory segments, dropping empties."
  (split-string path "/" t))

(defun +git-review--tree-insert (root-alist file)
  "Insert FILE into nested ROOT-ALIST tree keyed by path segments.
Directory nodes are plists with keys :dirs :files :name :path.
`:path' is the complete repository-relative directory path.
File nodes store the `+git-review-file' under :file."
  (let* ((parts (+git-review--path-segments (+git-review-file-path file)))
         (dirs (butlast parts))
         (name (car (last parts)))
         (node root-alist)
         (prefix ""))
    (dolist (dir dirs)
      (setq prefix (if (string-empty-p prefix)
                       dir
                     (concat prefix "/" dir)))
      (let ((child (assoc dir (plist-get node :dirs))))
        (unless child
          (setq child (cons dir (list :name dir
                                      :path prefix
                                      :dirs nil
                                      :files nil)))
          (setf (plist-get node :dirs)
                (append (plist-get node :dirs) (list child))))
        (setq node (cdr child))))
    (setf (plist-get node :files)
          (append (plist-get node :files)
                  (list (cons name (list :name name :file file)))))
    root-alist))

(defun +git-review--build-tree (files)
  "Build a nested directory alist from FILES."
  (let ((root (list :name "" :dirs nil :files nil)))
    (dolist (file files)
      (+git-review--tree-insert root file))
    root))

(defun +git-review--sum-counts (a b)
  "Add count values A and B, preserving `binary' dominance."
  (cond
   ((or (eq a 'binary) (eq b 'binary)) 'binary)
   ((and (numberp a) (numberp b)) (+ a b))
   ((numberp a) a)
   ((numberp b) b)
   (t 0)))

(defun +git-review--node-stats (node reviewed-map)
  "Return plist (:adds :dels :total :reviewed :state) for NODE."
  (let ((adds 0)
        (dels 0)
        (total 0)
        (reviewed 0))
    (dolist (pair (plist-get node :files))
      (let* ((file (plist-get (cdr pair) :file)))
        (setq adds (+git-review--sum-counts
                    adds (+git-review-file-additions file)))
        (setq dels (+git-review--sum-counts
                    dels (+git-review-file-deletions file)))
        (cl-incf total)
        (when (+git-review--file-reviewed-p file reviewed-map)
          (cl-incf reviewed))))
    (dolist (pair (plist-get node :dirs))
      (let ((st (+git-review--node-stats (cdr pair) reviewed-map)))
        (setq adds (+git-review--sum-counts adds (plist-get st :adds)))
        (setq dels (+git-review--sum-counts dels (plist-get st :dels)))
        (cl-incf total (plist-get st :total))
        (cl-incf reviewed (plist-get st :reviewed))))
    (list :adds adds
          :dels dels
          :total total
          :reviewed reviewed
          :state (cond
                  ((= total 0) 'unreviewed)
                  ((= reviewed 0) 'unreviewed)
                  ((= reviewed total) 'reviewed)
                  (t 'partial)))))

(defun +git-review--status-glyph (status)
  "Return a short status glyph for STATUS."
  (pcase status
    ('modified "M")
    ('added "A")
    ('deleted "D")
    ('renamed "R")
    ('copied "C")
    ('untracked "?")
    ('binary "BINARY")
    ('submodule "SUBMODULE")
    (_ "?")))

(defun +git-review--checkbox (state)
  "Return checkbox text for reviewed STATE."
  (pcase state
    ('reviewed "[x]")
    ('partial "[-]")
    (_ "[ ]")))

(defun +git-review--format-counts (adds dels &optional binary-status)
  "Format ADD/DEL counts for tree display."
  (cond
   ((or (eq adds 'binary) (eq dels 'binary) (eq binary-status 'binary))
    "BINARY")
   ((eq binary-status 'submodule) "SUBMODULE")
   (t (format "+%s -%s" adds dels))))

(defun +git-review--pad-to-width (string width)
  "Pad STRING with ASCII spaces to at least display WIDTH via `string-width'."
  (let ((pad (- width (string-width string))))
    (if (> pad 0)
        (concat string (make-string pad ?\s))
      string)))

(defun +git-review--tree-indent (depth)
  "Return ASCII indentation for hierarchy DEPTH (4 spaces per level)."
  (make-string (* (max 0 depth) 4) ?\s))

(defun +git-changes-tree--format-file-heading (depth state name status counts)
  "Format a Changes Tree file heading at DEPTH."
  (concat (+git-review--tree-indent depth)
          (+git-review--checkbox state)
          " "
          (+git-review--pad-to-width name 20)
          "  "
          (+git-review--pad-to-width status 6)
          "  "
          counts
          "\n"))

(defun +git-changes-tree--format-dir-heading (depth state name counts)
  "Format a Changes Tree directory heading at DEPTH."
  (concat (+git-review--tree-indent depth)
          (+git-review--checkbox state)
          " "
          (+git-review--pad-to-width name 28)
          "  "
          counts
          "\n"))

;; ---------------------------------------------------------------------------
;; Changes Tree mode (valid Magit sections; lazy — no per-file diffs)
;; ---------------------------------------------------------------------------

(define-derived-mode +git-changes-tree-mode magit-mode "Git-Tree"
  "Mode for the local Changes Tree review buffer."
  :interactive nil
  :group 'magit
  (magit-hack-dir-local-variables)
  (setq truncate-lines t)
  (+git-review-buffer-mode 1))

(cl-defmethod magit-buffer-value (&context (major-mode +git-changes-tree-mode))
  (list 'git-changes-tree
        (and +git-review-target
             (+git-review-target-overview-id +git-review-target))))

(defvar +git-changes-tree-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'+git-changes-tree-visit-file)
    map)
  "Keymap for Changes Tree file sections.")

(defvar +git-changes-tree-dir-section-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Changes Tree directory sections.")

(defun +git-changes-tree--insert-file (file reviewed-map &optional depth)
  "Insert a Magit section for FILE using REVIEWED-MAP at DEPTH.
DEPTH defaults to 0 (root).  Section value remains the full relative path."
  (let* ((depth (or depth 0))
         (reviewed (+git-review--file-reviewed-p file reviewed-map))
         (state (if reviewed 'reviewed 'unreviewed))
         (status (+git-review-file-status file))
         (counts (+git-review--format-counts
                  (+git-review-file-additions file)
                  (+git-review-file-deletions file)
                  status)))
    (magit-insert-section section (file (+git-review-file-path file))
      (oset section keymap '+git-changes-tree-file-section-map)
      (magit-insert-heading
        (+git-changes-tree--format-file-heading
         depth
         state
         (file-name-nondirectory (+git-review-file-path file))
         (+git-review--status-glyph status)
         counts)))))

(defun +git-changes-tree--insert-dir (path node reviewed-map &optional hide depth)
  "Insert directory PATH from NODE with REVIEWED-MAP at DEPTH.
PATH is the complete repository-relative directory path (section value).
The heading displays only the basename.  HIDE non-nil collapses the
directory initially.  DEPTH defaults to 0 (root).  Children are rendered
at DEPTH+1.  Children are section bodies only — no file-diff buffers or
per-file Git diffs."
  (let* ((depth (or depth 0))
         (stats (+git-review--node-stats node reviewed-map))
         (counts (+git-review--format-counts
                  (plist-get stats :adds)
                  (plist-get stats :dels)))
         (base (if (or (null path) (string-empty-p path))
                   "."
                 (file-name-nondirectory (directory-file-name path))))
         (label (concat base "/")))
    (magit-insert-section section (directory path hide)
      (oset section keymap '+git-changes-tree-dir-section-map)
      (magit-insert-heading
        (+git-changes-tree--format-dir-heading
         depth
         (plist-get stats :state)
         label
         counts))
      (dolist (pair (plist-get node :dirs))
        (let* ((child (cdr pair))
               (child-path (or (plist-get child :path)
                               (if (or (null path) (string-empty-p path))
                                   (car pair)
                                 (concat path "/" (car pair))))))
          (+git-changes-tree--insert-dir
           child-path child reviewed-map t (1+ depth))))
      (dolist (pair (plist-get node :files))
        (+git-changes-tree--insert-file
         (plist-get (cdr pair) :file) reviewed-map (1+ depth))))))

(defun +git-changes-tree-refresh-buffer ()
  "Refresh the current Changes Tree from local Git + persisted state.
Does not generate per-file diff buffers."
  (unless +git-review-target
    (user-error "Changes Tree has no review target"))
  (let* ((target +git-review-target)
         (root (+git-review--git-root-for-target target))
         (_dir (+git-review--bind-operational-directory root))
         (files (+git-review-collect-files target))
         (reviewed (+git-review--sync-reviewed-map
                    files
                    (or +git-review--reviewed-map
                        (+git-review-state-load target))))
         (tree (+git-review--build-tree files))
         (stats (+git-review--node-stats tree reviewed)))
    (setq +git-review--files files
          +git-review--reviewed-map reviewed)
    (+git-review-state-save target reviewed)
    (+git-review--ensure-edit-context target)
    (magit-set-header-line-format
     (format "Changes  %d/%d reviewed  |  %s  |  %s"
             (plist-get stats :reviewed)
             (plist-get stats :total)
             (+git-review--format-counts
              (plist-get stats :adds)
              (plist-get stats :dels))
             (+git-review--identity-suffix target)))
    (magit-insert-section (changes-tree)
      (magit-insert-heading
        (format "Changes  %d/%d reviewed  |  %s\n"
                (plist-get stats :reviewed)
                (plist-get stats :total)
                (+git-review--format-counts
                 (plist-get stats :adds)
                 (plist-get stats :dels))))
      (dolist (pair (plist-get tree :dirs))
        (let* ((child (cdr pair))
               (child-path (or (plist-get child :path) (car pair))))
          (+git-changes-tree--insert-dir child-path child reviewed t 0)))
      (dolist (pair (plist-get tree :files))
        (+git-changes-tree--insert-file
         (plist-get (cdr pair) :file) reviewed 0)))))

(defun +git-changes-tree-setup-buffer (target)
  "Create or reuse the Changes Tree buffer for TARGET and display it.
Locks Magit buffer identity to `overview-id' so advancing an immutable
branch creates a new tree while worktree/staged still reuse."
  (require 'magit)
  (let* ((existing
          (car (+git-review--find-buffers-for-target
                target
                (lambda (buf)
                  (with-current-buffer buf
                    (derived-mode-p '+git-changes-tree-mode))))))
         buf)
    (if existing
        (progn
          (setq buf existing)
          (magit-display-buffer buf)
          (with-current-buffer buf
            (setq-local +git-review-target target)
            (+git-review--adopt-reopening-context target)
            (magit-refresh)
            (+git-review--enter-normal-state)))
      (let ((default-directory
             (file-name-as-directory
              (+git-review--git-root-for-target target))))
        (setq buf
              (magit-setup-buffer #'+git-changes-tree-mode t
                (+git-review-target target)
                (+git-review--reviewed-map nil)
                (+git-review--files nil)))
        (with-current-buffer buf
          (setq-local +git-review-target target)
          (+git-review--adopt-reopening-context target)
          (+git-review--enter-normal-state))))
    buf))

;; ---------------------------------------------------------------------------
;; Overview + entry openers (buffer reuse)
;; ---------------------------------------------------------------------------

(defun +git-review--find-buffers-for-target (target &optional pred)
  "Return live buffers matching TARGET's immutable overview identity.
Optional PRED is called with each buffer and must return non-nil to keep it.
Persistence uses `family-id'; buffer reuse uses `overview-id'."
  (let ((id (+git-review-target-overview-id target))
        found)
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (bound-and-true-p +git-review-target)
                        (equal (+git-review-target-overview-id
                                +git-review-target)
                               id)
                        (or (null pred) (funcall pred buf)))))
        (push buf found)))
    (nreverse found)))

(defun +git-review--attach-target (target &optional file-path)
  "Attach TARGET (and optional FILE-PATH) to the current buffer."
  (setq-local +git-review-target target)
  (when file-path
    (setq-local +git-review-file-path file-path))
  (+git-review--adopt-reopening-context target)
  (+git-review--install-immutable-guards)
  (+git-review--maybe-install-untracked-overview target file-path)
  (+git-review-buffer-mode 1)
  (+git-review--enter-normal-state))

(defun +git-review--sync-magit-diff-state (target &optional files)
  "Update Magit buffer-local diff variables from TARGET before refresh.
FILES when non-nil replaces `magit-buffer-diff-files'."
  (pcase-let* ((`(,range ,typearg) (+git-review-target-range-args target))
               (args (+git-review-target-diff-args target))
               (dtype (pcase (+git-review-target-scope target)
                        ('staged 'staged)
                        ('worktree 'committed)
                        (_ 'committed))))
    (setq-local +git-review-target target)
    (setq-local magit-buffer-range range)
    (setq-local magit-buffer-typearg typearg)
    (setq-local magit-buffer-diff-args args)
    (setq-local magit-buffer-diff-type dtype)
    (when files
      (setq-local magit-buffer-diff-files files))
    (setq-local magit-buffer-range-hashed
                (and range (fboundp 'magit-hash-range)
                     (magit-hash-range range)))))

(defun +git-review--insert-untracked-overview ()
  "Insert valid Git-produced --no-index diffs for untracked worktree files.
Uses Magit's washer so section markers stay complete."
  (when (and (bound-and-true-p +git-review-target)
             (eq (+git-review-target-scope +git-review-target) 'worktree)
             (null +git-review-file-path))
    (let* ((root (+git-review--git-root-for-target +git-review-target))
           (paths (+git-review--git-items
                   root "ls-files" "-z" "--others" "--exclude-standard"))
           (null-device (if (eq system-type 'windows-nt) "NUL" "/dev/null"))
           (args (+git-review-target-diff-args +git-review-target)))
      (dolist (path paths)
        (let ((full (expand-file-name path root)))
          (when (and (file-exists-p full) (not (file-directory-p full)))
            (magit--insert-diff 'wash-anyway
              "diff" "--no-index" "-p" "--no-prefix"
              args "--"
              (magit-convert-filename-for-git null-device)
              (magit-convert-filename-for-git full))))))))

(defun +git-review--maybe-install-untracked-overview (target file-path)
  "Install or remove the untracked overview section hook for TARGET."
  (setq-local magit-diff-sections-hook
              (remq '+git-review--insert-untracked-overview
                    (copy-sequence (or magit-diff-sections-hook
                                       (default-value
                                        'magit-diff-sections-hook)))))
  (when (and (eq (+git-review-target-scope target) 'worktree)
             (null file-path))
    (add-hook 'magit-diff-sections-hook
              #'+git-review--insert-untracked-overview t t)))

(defun +git-review--magit-diff-locked-p (target &optional files)
  "Return non-nil when Magit should lock the buffer to its value.
Immutable commit/branch/PR reviews always lock so advancing resolved OIDs
creates a new buffer (PR overview identity itself is stable and separate).
File-specific diffs lock so they do not steal the overview buffer
(`magit-diff-buffer-file-locked' convention)."
  (or (memq (+git-review-target-scope target) '(commit branch pullreq))
      (and files t)))

(defun +git-review--open-overview (target)
  "Open or reuse the Magit unified overview for TARGET."
  (require 'magit)
  (let* ((args (+git-review-target-diff-args target))
         (range-type (+git-review-target-range-args target))
         (range (car range-type))
         (typearg (cadr range-type))
         (existing
          (car (+git-review--find-buffers-for-target
                target
                (lambda (buf)
                  (with-current-buffer buf
                    (and (derived-mode-p 'magit-diff-mode)
                         (null +git-review-file-path)))))))
         buffer)
    (if existing
        (progn
          (setq buffer existing)
          (magit-display-buffer buffer)
          (with-current-buffer buffer
            (+git-review--adopt-reopening-context target)
            (+git-review--sync-magit-diff-state target nil)
            (+git-review--maybe-install-untracked-overview target nil)
            (magit-refresh)
            (+git-review--enter-normal-state)))
      (let ((default-directory
             (file-name-as-directory
              (+git-review--git-root-for-target target))))
        (setq buffer
              (magit-diff-setup-buffer
               range typearg args nil
               (pcase (+git-review-target-scope target)
                 ('staged 'staged)
                 ('worktree 'committed)
                 (_ 'committed))
               (+git-review--magit-diff-locked-p target)))
        (with-current-buffer buffer
          (+git-review--attach-target target)
          (when (eq (+git-review-target-scope target) 'worktree)
            ;; Re-refresh so the untracked hook runs after attachment.
            (magit-refresh)))))
    buffer))

(defun +git-review-open-worktree ()
  "Open the reusable working-tree review overview."
  (interactive)
  (+git-review--open-overview (+git-review-target-for-worktree)))

(defun +git-review-open-staged ()
  "Open the reusable staged review overview."
  (interactive)
  (+git-review--open-overview (+git-review-target-for-staged)))

(defun +git-review-open-commit (commit)
  "Open the reusable commit review overview for COMMIT."
  (interactive
   (list (magit-read-branch-or-commit "Review commit")))
  (+git-review--open-overview (+git-review-target-for-commit commit)))

(defun +git-review-open-branch (base head)
  "Open the reusable branch review for BASE..HEAD via merge-base."
  (interactive
   (list (magit-read-branch-or-commit "Review base")
         (magit-read-branch-or-commit "Review head")))
  (+git-review--open-overview (+git-review-target-for-branch base head)))

(defun +git-review-open-changes-tree (&optional target)
  "Open or reuse the Changes Tree for TARGET or the current buffer target."
  (interactive)
  ;; The shared review minor mode owns `t', so PR buffers arrive here before
  ;; their major-mode binding.  Dispatch explicitly instead of opening a tree
  ;; from a possibly stale PR target after a mirror generation changes.
  (if (and (bound-and-true-p +git-pr--model)
           (fboundp '+git-pr-open-changes-tree))
      (+git-pr-open-changes-tree)
    (let ((target (or target
                      (and (bound-and-true-p +git-review-target)
                           +git-review-target)
                      (+git-review-target-for-worktree))))
      (+git-changes-tree-setup-buffer target))))

;; ---------------------------------------------------------------------------
;; Per-file unified diffs
;; ---------------------------------------------------------------------------

(defun +git-review--file-at-tree-point ()
  "Return the `+git-review-file' at point in the Changes Tree, or nil."
  (let* ((section (magit-current-section))
         (path (and section
                    (magit-section-match 'file section)
                    (oref section value))))
    (and path
         (cl-find path +git-review--files
                  :key #'+git-review-file-path
                  :test #'equal))))

(defun +git-review--find-file-diff-buffer (target path)
  "Return an existing file-diff buffer for TARGET and PATH, or nil."
  (car (+git-review--find-buffers-for-target
        target
        (lambda (buf)
          (with-current-buffer buf
            (and (derived-mode-p 'magit-diff-mode)
                 (equal +git-review-file-path path)))))))

(defvar-local +git-review--presented-status nil
  "Status symbol for specialized per-file presentation (copy/binary/submodule).")

(defvar-local +git-review--presented-old-path nil
  "Old path for rename/copy presentation in the current file-diff buffer.")

(defun +git-review--file-section-for-path (path)
  "Return the Magit file section whose value equals PATH, or nil."
  (and path magit-root-section
       (cl-find-if (lambda (sec)
                     (and (magit-file-section-p sec)
                          (equal (oref sec value) path)))
                   (oref magit-root-section children))))

(defun +git-review--rewrite-heading-status (section from-status to-status)
  "Rewrite FROM-STATUS to TO-STATUS in SECTION's heading.
Preserves Magit text properties by keeping the %-11s field width."
  (let* ((from (format "%-11s" from-status))
         (to (format "%-11s" to-status))
         (inhibit-read-only t))
    (when (= (length from) (length to))
      (save-excursion
        (goto-char (oref section start))
        (when (looking-at (regexp-quote from))
          (replace-match to t t))))))

(defun +git-review--insert-section-note (section text)
  "Insert TEXT as a child note inside SECTION without replacing it."
  (let ((inhibit-read-only t)
        (magit-insert-section--parent section)
        (magit-insert-section--current section))
    (save-excursion
      (goto-char (oref section content))
      (magit-insert-section (hunk '(git-review-note))
        (insert (propertize text 'font-lock-face 'magit-diff-context)))
      (oset section end (copy-marker (max (marker-position (oref section end))
                                          (point))
                                     t))
      (magit-section--set-section-properties section))))

(defun +git-review--fallback-body (kind path &optional old-path)
  "Return clear fallback/notice text for KIND at PATH."
  (pcase kind
    ('binary
     (format "BINARY\nBinary file `%s' changed.\nNo textual unified diff is shown.\n"
             path))
    ('submodule
     (format "SUBMODULE\nSubmodule/gitlink `%s' changed.\nThis is not a regular file diff.\nOpen the submodule repository separately to review its commits.\n"
             path))
    ('copied
     (format "copy from %s\ncopy to %s\n"
             (or old-path path) path))
    (_ (format "No textual diff available for `%s' (%s).\n" path kind))))

(defun +git-review--ensure-file-section (path)
  "Return a Magit file section for PATH, creating one only if Magit omitted it."
  (or (+git-review--file-section-for-path path)
      (let ((inhibit-read-only t)
            (kind +git-review--presented-status)
            (old +git-review--presented-old-path))
        (erase-buffer)
        (magit-insert-section (diffbuf)
          (magit-diff-insert-file-section
           path
           (and (eq kind 'copied) old)
           (pcase kind
             ('copied "copied")
             ('binary "modified")
             ('submodule "submodule")
             (_ "modified"))
           nil
           (and (eq kind 'copied)
                (format "copy from %s\ncopy to %s\n" old path))
           (format "diff --git %s %s\n" (or old path) path)
           (eq kind 'binary)
           (pcase kind
             ('binary "binary")
             ('submodule "submodule/gitlink")
             (_ nil))))
        (setq magit-root-section
              (or magit-insert-section--current magit-root-section))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (+git-review--file-section-for-path path))))

(defun +git-review--ensure-status-presentation ()
  "Annotate Magit's native file section; never replace it with bare text.
Survives `gr'/`magit-refresh' because Magit rebuilds sections first."
  (when (and +git-review--presented-status +git-review-file-path)
    (let* ((path +git-review-file-path)
           (kind +git-review--presented-status)
           (old +git-review--presented-old-path)
           (section (+git-review--ensure-file-section path))
           (case-fold-search nil))
      (when section
        ;; Collapsed empty bodies still accept child notes at `content'.
        (when (oref section hidden)
          (magit-section-show section))
        (pcase kind
          ('copied
           (+git-review--rewrite-heading-status section "new file" "copied")
           (unless (save-excursion
                     (goto-char (oref section start))
                     (re-search-forward "^copy from " (oref section end) t))
             (+git-review--insert-section-note
              section (+git-review--fallback-body kind path old))))
          ('binary
           (unless (save-excursion
                     (goto-char (oref section start))
                     (re-search-forward "Binary file `" (oref section end) t))
             (+git-review--insert-section-note
              section (+git-review--fallback-body kind path))))
          ('submodule
           (+git-review--rewrite-heading-status section "new file" "submodule")
           (+git-review--rewrite-heading-status section "modified" "submodule")
           (unless (save-excursion
                     (goto-char (oref section start))
                     (re-search-forward "Submodule/gitlink `"
                                        (oref section end) t))
             (+git-review--insert-section-note
              section (+git-review--fallback-body kind path))))
          (_ nil))
        (goto-char (oref section start))
        (set-buffer-modified-p nil)))))

(defun +git-review--install-status-presentation (status &optional old-path)
  "Install STATUS presentation that re-applies after Magit refresh."
  (setq-local +git-review--presented-status status)
  (setq-local +git-review--presented-old-path old-path)
  (add-hook 'magit-refresh-buffer-hook
            #'+git-review--ensure-status-presentation t t)
  (+git-review--ensure-status-presentation))

(defun +git-review--insert-fallback-diff (title body)
  "Insert a single valid Magit section with TITLE and BODY text."
  (magit-insert-section (file title)
    (magit-insert-heading (concat title "\n"))
    (insert (propertize body 'face 'magit-diff-context))
    (insert "\n")))

(defun +git-review--open-untracked-diff (target file)
  "Open a Git-produced --no-index diff for untracked FILE."
  (let* ((root (+git-review--git-root-for-target target))
         (path (+git-review-file-path file))
         (full (expand-file-name path root))
         (null-device (if (eq system-type 'windows-nt) "NUL" "/dev/null"))
         (files (list (magit-convert-filename-for-git null-device)
                      (magit-convert-filename-for-git full))))
    (magit-diff-setup-buffer
     nil "--no-index"
     (+git-review-target-diff-args target)
     files
     'undefined
     (+git-review--magit-diff-locked-p target files))))

(defun +git-review--file-diff-pathspecs (file)
  "Return Magit `magit-buffer-diff-files' pathspecs for FILE.
Rename/copy entries include both old and new paths so Git preserves
rename metadata instead of rendering a pure add."
  (let ((path (+git-review-file-path file))
        (old (+git-review-file-old-path file)))
    (if (and old (not (string-empty-p old)) (not (equal old path)))
        (list old path)
      (list path))))

(defun +git-review-open-file-diff (target file &optional other-window)
  "Open or reuse the exact unified diff for FILE under TARGET.
OTHER-WINDOW non-nil uses the reusable right-hand window.
Binary, submodule, and copy entries get clear visible presentation that
survives `gr'."
  (require 'magit)
  (let* ((path (+git-review-file-path file))
         (status (+git-review-file-status file))
         (old-path (+git-review-file-old-path file))
         (existing (+git-review--find-file-diff-buffer target path))
         (display (if other-window
                      (lambda (buffer)
                        (+git-review--record-return-for-buffer buffer)
                        (+git-review--display-in-other-window buffer))
                    nil))
         (pathspecs (+git-review--file-diff-pathspecs file))
         buffer)
    (cond
     (existing
      (setq buffer existing)
      (if other-window
          (funcall display buffer)
        (magit-display-buffer buffer))
      (with-current-buffer buffer
        (+git-review--adopt-reopening-context target)
        (cond
         ((eq status 'untracked)
          (setq-local +git-review-target target)
          (magit-refresh))
         ((memq status '(binary submodule))
          (+git-review--attach-target target path)
          (+git-review--install-status-presentation status old-path))
         (t
          (+git-review--sync-magit-diff-state target pathspecs)
          (magit-refresh)
          (when (eq status 'copied)
            (+git-review--install-status-presentation status old-path))))
        (+git-review--enter-normal-state)))
     (t
      ;; Only let-bind default-directory when creating a new Magit buffer.
      ;; Reusing an existing buffer while let-bound would restore the prior
      ;; root on unwind (buffer-local default-directory gotcha).
      (let ((default-directory
             (file-name-as-directory
              (+git-review--git-root-for-target target))))
        (cond
         ((eq status 'untracked)
          (let ((magit-display-buffer-function
                 (or display magit-display-buffer-function)))
            (setq buffer (+git-review--open-untracked-diff target file)))
          (with-current-buffer buffer
            (+git-review--attach-target target path)))
         ((memq status '(binary submodule))
          (let ((magit-display-buffer-function
                 (or display magit-display-buffer-function)))
            (pcase-let* ((`(,range ,typearg)
                          (+git-review-target-range-args target))
                         (args (+git-review-target-diff-args target)))
              (setq buffer
                    (magit-diff-setup-buffer
                     range typearg args pathspecs
                     (pcase (+git-review-target-scope target)
                       ('staged 'staged)
                       (_ 'committed))
                     (+git-review--magit-diff-locked-p target pathspecs)))))
          (with-current-buffer buffer
            (+git-review--attach-target target path)
            (+git-review--install-status-presentation status old-path)))
         (t
          (pcase-let* ((`(,range ,typearg)
                        (+git-review-target-range-args target))
                       (args (+git-review-target-diff-args target)))
            (let ((magit-display-buffer-function
                   (or display magit-display-buffer-function)))
              (setq buffer
                    (magit-diff-setup-buffer
                     range typearg args pathspecs
                     (pcase (+git-review-target-scope target)
                       ('staged 'staged)
                       (_ 'committed))
                     (+git-review--magit-diff-locked-p target pathspecs))))
            (with-current-buffer buffer
              (+git-review--attach-target target path)
              (when (eq status 'copied)
                (+git-review--install-status-presentation
                 status old-path)))))))))
    buffer))

(defun +git-changes-tree-visit-file (&optional other-window)
  "Visit the unified diff for the Changes Tree file at point."
  (interactive "P")
  (let* ((old-file (+git-review--file-at-tree-point))
         (path (and old-file (+git-review-file-path old-file))))
    ;; A mirror can be atomically replaced while this tree remains open.
    ;; Refresh the cached PR target before handing its range to Magit.
    (when (and path
               (bound-and-true-p +git-pr--model)
               (fboundp '+git-pr--ensure-current-model))
      (+git-pr--ensure-current-model t))
    (let ((file (and path
                     (cl-find path +git-review--files
                              :key #'+git-review-file-path
                              :test #'equal))))
      (unless file
        (user-error
         (if path
             "File %s is no longer part of the current PR range"
           "No changed file at point")
         path))
      (+git-review-open-file-diff +git-review-target file other-window))))

(defun +git-changes-tree-visit-other-window ()
  "Visit the file diff at point in the reusable right-hand window."
  (interactive)
  (+git-changes-tree-visit-file t))

;; ---------------------------------------------------------------------------
;; Reviewed toggles (never mutate Git state)
;; ---------------------------------------------------------------------------

(defun +git-review--descendant-files (section)
  "Return `+git-review-file' records under SECTION (file or directory)."
  (cond
   ((magit-section-match 'file section)
    (let* ((path (oref section value))
           (file (cl-find path +git-review--files
                          :key #'+git-review-file-path
                          :test #'equal)))
      (and file (list file))))
   ((magit-section-match 'directory section)
    (let* ((prefix (oref section value))
           (prefix (if (or (null prefix) (string-empty-p prefix))
                       ""
                     (concat prefix "/"))))
      (cl-remove-if-not
       (lambda (file)
         (or (string-empty-p prefix)
             (string-prefix-p prefix (+git-review-file-path file))))
       +git-review--files)))
   (t nil)))

(defun +git-changes-tree-toggle-reviewed ()
  "Toggle reviewed state for the file or folder at point.
Never stages, unstages, discards, or modifies Git files."
  (interactive)
  (unless (derived-mode-p '+git-changes-tree-mode)
    (user-error "Not in a Changes Tree buffer"))
  (let* ((section (magit-current-section))
         (files (+git-review--descendant-files section))
         (map (or +git-review--reviewed-map
                  (setq +git-review--reviewed-map
                        (make-hash-table :test #'equal)))))
    (unless files
      (user-error "No reviewable file or folder at point"))
    (let* ((all-reviewed
            (cl-every (lambda (f) (+git-review--file-reviewed-p f map))
                      files)))
      (dolist (file files)
        (let ((path (+git-review-file-path file))
              (fp (+git-review-file-fingerprint file)))
          (if all-reviewed
              (remhash path map)
            (puthash path fp map))))
      (setq +git-review--reviewed-map map)
      (+git-review-state-save +git-review-target map)
      (magit-refresh))))

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +git-changes-tree-mode-map
      (kbd "SPC") #'+git-changes-tree-toggle-reviewed
      "t" #'+git-review-open-changes-tree
      "o" #'+git-changes-tree-visit-other-window
      (kbd "RET") #'+git-changes-tree-visit-file)))

;; ---------------------------------------------------------------------------
;; Immutable staging guards
;; ---------------------------------------------------------------------------

(defun +git-review--immutable-target-p ()
  "Return non-nil when the current buffer's review target is immutable."
  (and (bound-and-true-p +git-review-target)
       (not (+git-review-target-mutable-p +git-review-target))))

(defun +git-review--reject-immutable-mutation (&rest _)
  "Signal when stage/unstage/discard is attempted on an immutable review."
  (when (+git-review--immutable-target-p)
    (user-error
     "Cannot stage, unstage, or discard from an immutable %s review"
     (+git-review-target-scope +git-review-target))))

(defvar +git-review--mutation-commands
  '(magit-stage
    magit-stage-file
    magit-stage-files
    magit-unstage
    magit-unstage-file
    magit-unstage-files
    magit-discard
    magit-discard-files)
  "Magit commands that mutate the index or worktree.")

(defun +git-review--install-immutable-guards ()
  "Ensure immutable-target guards are advised onto mutation commands."
  (dolist (cmd +git-review--mutation-commands)
    (when (fboundp cmd)
      (advice-add cmd :before #'+git-review--reject-immutable-mutation))))

(with-eval-after-load 'magit
  (+git-review--install-immutable-guards))

;; =============================================================================
;; Syntax Highlighting in Diff Buffers (via magit-delta)
;; =============================================================================

(use-package magit-delta
  :straight t
  :after magit
  :if (executable-find "delta")
  :custom
  (magit-delta-default-dark-theme "Nord")
  (magit-delta-default-light-theme "GitHub")
  (magit-delta-hide-plus-minus-markers t)
  :config
  (defun +git/undo-delta-face-remap (&rest _)
    "Remove magit-delta's face remapping so diff backgrounds show."
    (setq face-remapping-alist
          (cl-remove-if (lambda (entry)
                          (memq (car entry)
                                '(magit-diff-context-highlight
                                  magit-diff-added
                                  magit-diff-added-highlight
                                  magit-diff-removed
                                  magit-diff-removed-highlight)))
                        face-remapping-alist)))
  (advice-add 'magit-delta-mode :after #'+git/undo-delta-face-remap))

;; =============================================================================
;; difftastic — optional AST-aware structural diffs (Transient/M-x only)
;; =============================================================================

(use-package difftastic
  :straight t
  :after magit
  :if (executable-find "difft")
  :demand t
  :config
  (setenv "DFT_DISPLAY" "side-by-side")
  (setenv "DFT_BYTE_LIMIT"  "10485760")
  (setenv "DFT_NODE_LIMIT"  "10000000")
  (setenv "DFT_GRAPH_LIMIT" "30000000")

  (defface +difftastic/removed
    '((t (:foreground "#ff6c6b" :weight bold)))
    "Bright red for difftastic removed tokens (token-level, fg-only).")
  (defface +difftastic/added
    '((t (:foreground "#98be65" :weight bold)))
    "Bright green for difftastic added tokens (token-level, fg-only).")

  (setq difftastic-normal-colors-vector
        (vector (aref ansi-color-normal-colors-vector 0)
                '+difftastic/removed
                '+difftastic/added
                'magit-diff-file-heading
                'font-lock-comment-face
                'font-lock-string-face
                'font-lock-warning-face
                (aref ansi-color-normal-colors-vector 7))
        difftastic-bright-colors-vector
        (vector (aref ansi-color-bright-colors-vector 0)
                '+difftastic/removed
                '+difftastic/added
                'magit-diff-file-heading
                'font-lock-comment-face
                'font-lock-string-face
                'font-lock-warning-face
                (aref ansi-color-bright-colors-vector 7))
        difftastic-highlight-alist nil)

  (setq difftastic-display-buffer-function
        (lambda (buffer _requested-width)
          (let* ((existing-win
                  (cl-find-if
                   (lambda (w)
                     (and (not (eq (window-buffer w) buffer))
                          (with-current-buffer (window-buffer w)
                            (derived-mode-p 'difftastic-mode))))
                   (window-list)))
                 (existing-buf
                  (cl-find-if
                   (lambda (b)
                     (and (not (eq b buffer))
                          (with-current-buffer b
                            (derived-mode-p 'difftastic-mode))))
                   (buffer-list))))
            (if existing-win
                (progn (set-window-buffer existing-win buffer)
                       (select-window existing-win))
              (pop-to-buffer buffer))
            (when existing-buf
              (kill-buffer existing-buf)))))

  (defun +difftastic/full ()
    "Show a difftastic diff of the entire working tree."
    (interactive)
    (difftastic-magit-diff nil nil))

  (defun +difftastic/at-point ()
    "Show a difftastic diff restricted to the file at point."
    (interactive)
    (let ((file (magit-file-at-point)))
      (if file
          (difftastic-magit-diff nil (list file))
        (+difftastic/full))))

  (defun +difftastic/quit ()
    "Quit and kill the current difftastic buffer + close its window."
    (interactive)
    (let ((win (selected-window)))
      (kill-buffer (current-buffer))
      (when (and (window-live-p win)
                 (not (one-window-p)))
        (delete-window win))))

  (defun +difftastic/setup-buffer-keys ()
    "Inside a difftastic buffer, bind `q' to quit + kill."
    (when (bound-and-true-p evil-mode)
      (evil-local-set-key 'normal (kbd "q") #'+difftastic/quit))
    (local-set-key (kbd "q") #'+difftastic/quit))

  (add-hook 'difftastic-mode-hook #'+difftastic/setup-buffer-keys)

  (with-eval-after-load 'magit-diff
    (dolist (prefix '(magit-diff magit-diff-refresh))
      (transient-append-suffix prefix '(-1 -1)
        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
         ("S" "Difftastic show"        difftastic-magit-show)]))))

(provide 'init-git-ui)

;;; init-git-ui.el ends here
