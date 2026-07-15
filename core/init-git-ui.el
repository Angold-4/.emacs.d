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

;;; Code:

(require 'cl-lib)
(require 'subr-x)

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
;; Diff faces
;; =============================================================================

(with-eval-after-load 'magit
  ;; Background-only faces for added/removed/context lines.  No :foreground
  ;; on added/removed so syntax highlighting can tint code on top.
  (custom-set-faces
   '(magit-diff-added ((t (:background "#0a1a0a" :extend t))))
   '(magit-diff-added-highlight ((t (:background "#0f220f" :extend t))))
   '(magit-diff-removed ((t (:background "#1a0a0a" :extend t))))
   '(magit-diff-removed-highlight ((t (:background "#220f0f" :extend t))))
   '(magit-diff-context ((t (:foreground "#888888" :extend t))))
   '(magit-diff-context-highlight ((t (:foreground "#999999" :background "#111111" :extend t))))
   '(magit-diff-file-heading ((t (:foreground "#e0e0e0" :weight bold :extend t))))
   '(magit-diff-file-heading-highlight ((t (:foreground "#ffffff" :background "#1a1a2e" :weight bold :extend t))))
   '(magit-diff-hunk-heading ((t (:foreground "#8888bb" :background "#1a1a22" :extend t))))
   '(magit-diff-hunk-heading-highlight ((t (:foreground "#aaaadd" :background "#222233" :extend t))))
   '(magit-section-heading ((t (:foreground "#c0a040" :weight bold))))
   '(magit-section-highlight ((t (:background "#0a0a0a"))))))

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
  (let ((state (+git-review--capture-return))
        (before (current-buffer)))
    (+git-review--call-visit-thing)
    (+git-review--apply-return before state)))

(defun +git-review-visit-other-window ()
  "Primary visit in the reusable right-hand window."
  (interactive)
  (let ((state (+git-review--capture-return))
        (before (current-buffer)))
    (+git-review--call-visit-thing-other-window)
    (+git-review--apply-return before state)))
(defun +git-review-visit-worktree ()
  "Visit the writable worktree file for the diff at point.
Signals a clear user error when no writable worktree target exists.
Does not force Insert state."
  (interactive)
  (let* ((rel (and (fboundp 'magit-diff--file)
                   (ignore-errors (magit-diff--file))))
         (root (and (fboundp 'magit-toplevel) (magit-toplevel)))
         (full (and rel root (expand-file-name rel root))))
    (unless (and full (file-exists-p full) (not (file-directory-p full)))
      (user-error "No writable worktree target at point"))
    (let ((state (+git-review--capture-return))
          (before (current-buffer)))
      (condition-case err
          (magit-diff-visit-worktree-file nil)
        (error
         (let ((msg (error-message-string err)))
           (if (string-match-p
                "Cannot determine file\\|No file\\|does not exist\\|not exist"
                msg)
               (user-error "No writable worktree target at point")
             (signal (car err) (cdr err))))))
      (let ((after (current-buffer)))
        (when (and (not (eq after before)) (buffer-live-p after))
          (+git-review--enable-return-on-buffer after state))))))

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
  "Refresh the current local Magit buffer only."
  (interactive)
  (if (derived-mode-p 'magit-mode)
      (magit-refresh)
    (user-error "Nothing to refresh")))

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
  "Enable `+git-review-buffer-mode' in Magit/Forge generated views."
  (when (or (derived-mode-p 'magit-mode)
            (derived-mode-p 'forge-topic-mode)
            (bound-and-true-p magit-blob-mode))
    (+git-review-buffer-mode 1)))

(defun +git-review--setup-evil-keys ()
  "Bind the Phase 1 review vocabulary in Evil normal state."
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal +git-review-buffer-mode-map
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line
      "J" #'+git-review-move-visual-lines-down
      "K" #'+git-review-move-visual-lines-up
      (kbd "TAB") #'+git-review-section-toggle
      (kbd "<backtab>") #'magit-section-cycle-global
      (kbd "RET") #'+git-review-visit
      "e" #'+git-review-visit-worktree
      "o" #'+git-review-visit-other-window
      "|" #'+git-review-visit-other-window
      "]f" #'+git-review-next-file
      "[f" #'+git-review-prev-file
      "]h" #'+git-review-next-hunk
      "[h" #'+git-review-prev-hunk
      "]c" #'+git-review-next-commit
      "[c" #'+git-review-prev-commit
      "n" #'evil-search-next
      "N" #'evil-search-previous
      "gr" #'+git-review-refresh
      "q" #'+git-review-quit
      "+" #'+git/increase-context
      "=" #'+git/increase-context
      "-" #'+git/decrease-context
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
                  magit-process-mode))
    (evil-set-initial-state mode 'normal)))

(with-eval-after-load 'forge-topic
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'forge-topic-mode 'normal)))

(dolist (hook '(magit-mode-hook
                magit-blob-mode-hook
                forge-topic-mode-hook))
  (add-hook hook #'+git-review--maybe-enable))

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
;; difftastic — optional AST-aware structural diffs (explicit D/d only)
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

  (defun +difftastic/setup-evil-keys ()
    "Bind `D' / `d' to difftastic shortcuts in magit buffers."
    (when (bound-and-true-p evil-mode)
      (evil-local-set-key 'normal (kbd "D") #'+difftastic/full)
      (evil-local-set-key 'normal (kbd "d") #'+difftastic/at-point)))

  (defun +difftastic/setup-buffer-keys ()
    "Inside a difftastic buffer, bind `q' to quit + kill."
    (when (bound-and-true-p evil-mode)
      (evil-local-set-key 'normal (kbd "q") #'+difftastic/quit))
    (local-set-key (kbd "q") #'+difftastic/quit))

  (dolist (hook '(magit-status-mode-hook
                  magit-diff-mode-hook
                  magit-revision-mode-hook))
    (add-hook hook #'+difftastic/setup-evil-keys))

  (add-hook 'difftastic-mode-hook #'+difftastic/setup-buffer-keys)

  (with-eval-after-load 'magit-diff
    (dolist (prefix '(magit-diff magit-diff-refresh))
      (transient-append-suffix prefix '(-1 -1)
        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
         ("S" "Difftastic show"        difftastic-magit-show)]))))

;;; init-git-ui.el ends here
