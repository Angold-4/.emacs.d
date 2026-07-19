;;; git-review-phase1.el --- Phase 1 navigation and ownership tests -*- lexical-binding: t -*-

;;; Commentary:
;; Offline ERT coverage for Phase 1 buffer/window foundations.
;; Reuses Phase 0 fixtures and the hard network guard.
;; Visit tests must call +git-review-visit / other-window / worktree /
;; dispatch directly so remapping regressions cannot hide behind Magit
;; primitives.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-review-fixtures)
(require 'git-review-baseline)

(defun git-review-phase1--source-files ()
  "Return absolute paths for Phase 1 production sources under test."
  (mapcar (lambda (rel)
            (expand-file-name rel user-emacs-directory))
          '("core/init-git.el"
            "core/init-git-ui.el"
            "core/init-tools.el"
            "core/init-evil.el"
            "init.el")))

(defun git-review-phase1--file-contains-p (path regexp)
  "Return non-nil when PATH contains a match for REGEXP."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (re-search-forward regexp nil t)))

(defun git-review-phase1--goto-diff-line (root relative needle)
  "In the current Magit diff buffer, move point to NEEDLE under RELATIVE."
  (let ((file (expand-file-name relative root)))
    (goto-char (point-min))
    (unless (re-search-forward (regexp-quote relative) nil t)
      (error "File section for %s not found" relative))
    (magit-section-show (magit-current-section))
    (unless (re-search-forward (regexp-quote needle) nil t)
      (error "Diff line %S not found in %s" needle file))
    (beginning-of-line)))

(defun git-review-phase1--visit-line-number ()
  "Return the line number at point in the current buffer."
  (line-number-at-pos (point)))

(defun git-review-phase1--goto-status-commit ()
  "Move point to a commit section in the current Magit status buffer."
  (goto-char (point-min))
  (unless (re-search-forward "^[a-f0-9]\\{7,\\} " nil t)
    (error "No commit section found in status buffer"))
  (beginning-of-line)
  (should (magit-section-match 'commit (magit-current-section))))

(defun git-review-phase1--invoke-dispatch-offline ()
  "Invoke `+git-dispatch' and quit the Transient without networking."
  (require 'transient)
  (funcall-interactively #'+git-dispatch)
  (when (fboundp 'transient-quit-all)
    (transient-quit-all)))

(defun git-review-phase1--cleanup-file-buffers (root)
  "Kill file-visiting buffers under ROOT."
  (dolist (b (buffer-list))
    (when (and (buffer-file-name b)
               (string-prefix-p root (buffer-file-name b)))
      (kill-buffer b))))

(ert-deftest git-review-phase1-magit-single-owner ()
  "Magit configuration has one owner; obsolete paths are absent."
  (require 'magit)
  (let ((git (expand-file-name "core/init-git.el" user-emacs-directory))
        (ui (expand-file-name "core/init-git-ui.el" user-emacs-directory))
        (tools (expand-file-name "core/init-tools.el" user-emacs-directory)))
    (should (file-readable-p git))
    (should (file-readable-p ui))
    (should (git-review-phase1--file-contains-p git "(use-package magit"))
    (should (git-review-phase1--file-contains-p
             ui "magit-display-buffer-function #'\\+git-display-buffer"))
    (should (git-review-phase1--file-contains-p ui "define-minor-mode \\+git-review-buffer-mode"))
    (should-not (git-review-phase1--file-contains-p tools "(use-package magit"))
    (should-not (git-review-phase1--file-contains-p tools "straight-use-package '(cond-let"))
    (should-not (git-review-phase1--file-contains-p
                 git "\\+git/open-file-at-point"))
    (should-not (git-review-phase1--file-contains-p
                 ui "\\+git/open-file-at-point"))
    (should-not (git-review-phase1--file-contains-p
                 git "\\+forge/resolve-ref"))
    (should-not (git-review-phase1--file-contains-p
                 git "\\+forge/visit-pullreq-around"))
    (should-not (git-review-phase1--file-contains-p
                 git "advice-add 'forge-visit-pullreq"))
    (should-not (git-review-phase1--file-contains-p
                 git "(global-set-key (kbd \"C-c g r\")"))
    (should (fboundp '+git-display-buffer))
    (should (eq magit-display-buffer-function #'+git-display-buffer))
    (should (fboundp '+git-dispatch))
    (should (eq (key-binding (kbd "C-c g")) #'+git-dispatch))
    (should (eq (key-binding (kbd "C-x g")) #'magit-status))
    (should-not (fboundp '+git/open-file-at-point))
    (should-not (fboundp '+forge/resolve-ref))
    (should-not (fboundp '+forge/visit-pullreq-around))))

(ert-deftest git-review-phase1-review-mode-and-evil-state ()
  "Generated Magit modes enable review mode and start in Evil normal state."
  (require 'magit)
  (require 'evil)
  (git-review-fixtures-with-repo
   "review-mode"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let ((default-directory (file-name-as-directory root)))
       (unwind-protect
           (git-review-baseline--with-instrumentation
            (lambda ()
              (magit-status-setup-buffer default-directory)
              (should (bound-and-true-p +git-review-buffer-mode))
              (should (eq evil-state 'normal))
              (magit-diff-working-tree "HEAD" '("--no-ext-diff" "-U3"))
              (should (bound-and-true-p +git-review-buffer-mode))
              (should (eq evil-state 'normal))))
         (git-review-baseline-cleanup-repo-buffers root))))))

(ert-deftest git-review-phase1-evil-navigation-bindings ()
  "Review buffers use Vim motion and mnemonic, bracket-free navigation."
  (require 'evil)
  (with-temp-buffer
    (+git-review-buffer-mode 1)
    (evil-local-mode 1)
    (evil-normalize-keymaps)
    (evil-normal-state)
    (should (eq (key-binding "n") #'evil-search-next))
    (should (eq (key-binding "N") #'evil-search-previous))
    (should (eq (key-binding "h") #'evil-backward-char))
    (should (eq (key-binding "l") #'evil-forward-char))
    (should (eq (key-binding "H") #'evil-beginning-of-line))
    (should (eq (key-binding "J") #'+evil/move-lines-down))
    (should (eq (key-binding "K") #'+evil/move-lines-up))
    (should (eq (key-binding "L") #'evil-end-of-line))
    (should (eq (key-binding "gf") #'+git-review-next-file))
    (should (eq (key-binding "gF") #'+git-review-prev-file))
    (should (eq (key-binding "gh") #'+git-review-next-hunk))
    (should (eq (key-binding "gH") #'+git-review-prev-hunk))
    ;; Phase 5 specializes commit stepping when loaded; both commands retain
    ;; the same next/previous contract outside PR child buffers.
    (should (memq (key-binding "gc")
                  '(+git-review-next-commit +git-pr-next-commit)))
    (should (memq (key-binding "gC")
                  '(+git-review-prev-commit +git-pr-prev-commit)))))

(ert-deftest git-review-phase1-window-count-preserved ()
  "Source -> status -> diff preserves window count 2 -> 2 -> 2."
  (require 'magit)
  (git-review-fixtures-with-repo
   "windows"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let* ((default-directory (file-name-as-directory root))
            (source (find-file-noselect
                     (expand-file-name "README.md" root)))
            (other (get-buffer-create " *git-review-phase1-other*"))
            (winconf nil))
       (unwind-protect
           (progn
             (delete-other-windows)
             (switch-to-buffer source)
             (select-window (split-window-right))
             (switch-to-buffer other)
             (other-window 1)
             (should (= 2 (count-windows)))
             (setq winconf (current-window-configuration))
             (git-review-baseline--with-instrumentation
              (lambda ()
                (magit-status default-directory)
                (should (= 2 (count-windows)))
                (should (eq (window-buffer (next-window)) other))
                (magit-diff-working-tree "HEAD" '("--no-ext-diff" "-U3"))
                (should (= 2 (count-windows)))
                (should (eq (window-buffer (next-window)) other))
                (should (derived-mode-p 'magit-diff-mode)))))
         (when winconf (set-window-configuration winconf))
         (git-review-baseline-cleanup-repo-buffers root)
         (when (buffer-live-p other) (kill-buffer other))
         (when (buffer-live-p source) (kill-buffer source)))))))

(ert-deftest git-review-phase1-other-window-reuse ()
  "Repeated explicit other-window visits create at most one extra window."
  (require 'magit)
  (git-review-fixtures-with-repo
   "other-win"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let ((default-directory (file-name-as-directory root)))
       (unwind-protect
           (git-review-baseline--with-instrumentation
            (lambda ()
              (delete-other-windows)
              (magit-diff-working-tree "HEAD" '("--no-ext-diff" "-U6"))
              (should (= 1 (count-windows)))
              (git-review-phase1--goto-diff-line
               root "README.md" "+line-two-modified")
              (+git-review-visit-other-window)
              (should (= 2 (count-windows)))
              (let ((right (frame-parameter nil '+git-review-other-window)))
                (should (window-live-p right))
                (select-window (previous-window))
                (select-window (get-buffer-window
                                (magit-get-mode-buffer 'magit-diff-mode)
                                t))
                (git-review-phase1--goto-diff-line
                 root "README.md" "+line-two-modified")
                (+git-review-visit-other-window)
                (should (= 2 (count-windows)))
                (should (eq right
                            (frame-parameter nil '+git-review-other-window))))))
         (set-frame-parameter nil '+git-review-other-window nil)
         (git-review-baseline-cleanup-repo-buffers root)
         (delete-other-windows))))))

(ert-deftest git-review-phase1-quit-restores-chain ()
  "Repeated q unwinds callers and restores the original layout."
  (require 'magit)
  (git-review-fixtures-with-repo
   "quit-chain"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let* ((default-directory (file-name-as-directory root))
            (source (find-file-noselect
                     (expand-file-name "README.md" root)))
            (other (get-buffer-create " *git-review-phase1-quit-other*")))
       (unwind-protect
           (git-review-baseline--with-instrumentation
            (lambda ()
              (delete-other-windows)
              (switch-to-buffer source)
              (select-window (split-window-right))
              (switch-to-buffer other)
              (other-window 1)
              (should (= 2 (count-windows)))
              (magit-status default-directory)
              (should (derived-mode-p 'magit-status-mode))
              (magit-diff-working-tree "HEAD" '("--no-ext-diff" "-U3"))
              (should (derived-mode-p 'magit-diff-mode))
              (+git-review-quit)
              (should (derived-mode-p 'magit-status-mode))
              (should (= 2 (count-windows)))
              (should (eq (window-buffer (next-window)) other))
              (+git-review-quit)
              (should (eq (current-buffer) source))
              (should (= 2 (count-windows)))
              (should (eq (window-buffer (next-window)) other))))
         (git-review-baseline-cleanup-repo-buffers root)
         (when (buffer-live-p other) (kill-buffer other))
         (when (buffer-live-p source) (kill-buffer source))
         (delete-other-windows))))))

(ert-deftest git-review-phase1-visit-uses-command-remapping ()
  "RET resolves Magit remapping and never calls the placeholder body."
  (require 'magit)
  (git-review-fixtures-with-repo
   "remap-visit"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let ((default-directory (file-name-as-directory root))
           (placeholder-calls 0))
       (unwind-protect
           (git-review-baseline--with-instrumentation
            (lambda ()
              ;; Use unstaged (not HEAD...worktree range) so Magit's
              ;; remapped visit lands on the writable worktree line.
              (magit-diff-unstaged '("--no-ext-diff" "-U6"))
              (git-review-phase1--goto-diff-line
               root "README.md" "+line-two-modified")
              (should (eq (command-remapping 'magit-visit-thing)
                          'magit-diff-visit-file))
              (should (eq (+git-review--visit-thing-command)
                          'magit-diff-visit-file))
              (cl-letf (((symbol-function 'magit-visit-thing)
                         (lambda (&rest _)
                           (interactive)
                           (cl-incf placeholder-calls)
                           (user-error "placeholder must not be called"))))
                (+git-review-visit))
              (should (= placeholder-calls 0))
              (should (string-suffix-p "README.md" (or buffer-file-name "")))
              (should (looking-at "line-two-modified"))
              (should (not (derived-mode-p 'magit-diff-mode)))))
         (git-review-baseline-cleanup-repo-buffers root)
         (git-review-phase1--cleanup-file-buffers root))))))

(ert-deftest git-review-phase1-status-commit-visit-and-other-window ()
  "Status commit RET/o use remapping and reuse one right-hand window."
  (require 'magit)
  (git-review-fixtures-with-repo
   "status-commit"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let ((default-directory (file-name-as-directory root)))
       (unwind-protect
           (git-review-baseline--with-instrumentation
            (lambda ()
              (delete-other-windows)
              (magit-status default-directory)
              (git-review-phase1--goto-status-commit)
              (let ((cmd (command-remapping 'magit-visit-thing)))
                (should cmd)
                (should (not (eq cmd 'magit-visit-thing)))
                (should (eq (+git-review--visit-thing-command) cmd)))
              (cl-letf (((symbol-function 'magit-visit-thing)
                         (lambda (&rest _)
                           (interactive)
                           (user-error "placeholder must not be called"))))
                (+git-review-visit))
              (should (derived-mode-p 'magit-revision-mode))
              (should (= 1 (count-windows)))
              (+git-review-quit)
              (should (derived-mode-p 'magit-status-mode))
              (git-review-phase1--goto-status-commit)
              (cl-letf (((symbol-function 'magit-visit-thing)
                         (lambda (&rest _)
                           (interactive)
                           (user-error "placeholder must not be called"))))
                (+git-review-visit-other-window))
              (should (derived-mode-p 'magit-revision-mode))
              (should (= 2 (count-windows)))
              (let ((right (frame-parameter nil '+git-review-other-window)))
                (should (window-live-p right))
                (select-window (previous-window))
                (should (derived-mode-p 'magit-status-mode))
                (git-review-phase1--goto-status-commit)
                (+git-review-visit-other-window)
                (should (= 2 (count-windows)))
                (should (eq right
                            (frame-parameter nil '+git-review-other-window))))))
         (set-frame-parameter nil '+git-review-other-window nil)
         (git-review-baseline-cleanup-repo-buffers root)
         (delete-other-windows))))))

(ert-deftest git-review-phase1-native-line-mapping ()
  "Native review visit commands map added/removed/context/rename/delete."
  (require 'magit)
  (git-review-fixtures-with-repo
   "visit-map"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let ((default-directory (file-name-as-directory root)))
       (unwind-protect
           (git-review-baseline--with-instrumentation
            (lambda ()
              ;; Added line via remapped RET on an unstaged diff.
              (magit-diff-unstaged '("--no-ext-diff" "-U6"))
              (git-review-phase1--goto-diff-line
               root "README.md" "+line-two-modified")
              (+git-review-visit)
              (should (string-suffix-p "README.md" buffer-file-name))
              (should (looking-at "line-two-modified"))
              (should (= (git-review-phase1--visit-line-number) 3))
              (should (not (derived-mode-p 'magit-diff-mode)))
              (+git-review-quit)

              ;; Writable worktree target via e (also from HEAD...worktree).
              (magit-diff-working-tree "HEAD" '("--no-ext-diff" "-U6"))
              (git-review-phase1--goto-diff-line
               root "README.md" "+line-two-modified")
              (+git-review-visit-worktree)
              (should (string-suffix-p "README.md" buffer-file-name))
              (should (looking-at "line-two-modified"))
              (should (= (git-review-phase1--visit-line-number) 3))
              (should (eq evil-state 'normal))
              (+git-review-quit)

              ;; Context line via worktree command.
              (magit-diff-working-tree "HEAD" '("--no-ext-diff" "-U6"))
              (git-review-phase1--goto-diff-line
               root "README.md" " line-one")
              (+git-review-visit-worktree)
              (should (looking-at "line-one"))
              (+git-review-quit)

              ;; Ordinary removed line maps to the old/left revision blob.
              (magit-diff-staged nil '("--no-ext-diff" "-U6"))
              (git-review-phase1--goto-diff-line
               root "lib/util.el" "-;; util v1")
              (+git-review-visit)
              (should (looking-at ";; util v1"))
              (should (not (derived-mode-p 'magit-diff-mode)))
              (+git-review-quit)

              ;; Staged rename: e opens the writable new path.
              (magit-diff-staged nil '("--no-ext-diff" "-U6"))
              (goto-char (point-min))
              (re-search-forward "new-name\\.el" nil t)
              (magit-section-show (magit-current-section))
              (re-search-forward "^\\+.*fixture-old" nil t)
              (beginning-of-line)
              (+git-review-visit-worktree)
              (should (string-suffix-p "new-name.el" buffer-file-name))
              (+git-review-quit)

              ;; Deleted file: e errors; RET opens the revision/blob target.
              (magit-diff-staged nil '("--no-ext-diff" "-U6"))
              (goto-char (point-min))
              (re-search-forward "to-delete\\.el" nil t)
              (magit-section-show (magit-current-section))
              (re-search-forward "^-" nil t)
              (beginning-of-line)
              (should-error (+git-review-visit-worktree)
                            :type 'user-error)
              (+git-review-visit)
              (should (or (not buffer-file-name)
                          (string-match-p "to-delete" (or buffer-file-name ""))))
              (should (or (bound-and-true-p magit-blob-mode)
                          (looking-at ";; delete me")))))
         (git-review-baseline-cleanup-repo-buffers root)
         (git-review-phase1--cleanup-file-buffers root))))))

(ert-deftest git-review-phase1-offline-open-visit-refresh-dispatch ()
  "Open, visit commands, gr, and C-c g fail if they attempt networking."
  (require 'magit)
  (git-review-fixtures-with-repo
   "offline"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let ((default-directory (file-name-as-directory root)))
       (unwind-protect
           (pcase-let*
               ((`(,_result . ,log)
                 (git-review-baseline--with-instrumentation
                  (lambda ()
                    (setq git-review-baseline--recording-p t)
                    (magit-status default-directory)
                    (magit-diff-unstaged '("--no-ext-diff" "-U3"))
                    (git-review-phase1--goto-diff-line
                     root "README.md" "+line-two-modified")
                    (+git-review-visit)
                    (+git-review-quit)
                    (magit-diff-working-tree "HEAD" '("--no-ext-diff" "-U3"))
                    (git-review-phase1--goto-diff-line
                     root "README.md" "+line-two-modified")
                    (+git-review-visit-worktree)
                    (+git-review-quit)
                    (magit-status default-directory)
                    (+git-review-refresh)
                    (should (eq (key-binding (kbd "C-c g")) #'+git-dispatch))
                    (git-review-phase1--invoke-dispatch-offline)))))
             (should (null git-review-baseline--blocked-attempts))
             (should (cl-every
                      (lambda (entry)
                        (let ((prog (nth 1 entry)))
                          (not (member (file-name-nondirectory (format "%s" prog))
                                       '("gh" "curl" "wget" "delta" "difft")))))
                      log))
             (should (not (cl-some
                           (lambda (entry)
                             (git-review-baseline--argv-network-p
                              (nth 1 entry) (nth 2 entry)))
                           log))))
         (git-review-baseline-cleanup-repo-buffers root))))))

(ert-deftest git-review-phase1-other-window-diff-uses-private-helper ()
  "Diff o/| routes through magit-diff-visit-file--internal for reuse."
  (require 'magit)
  (git-review-fixtures-with-repo
   "diff-other"
   (lambda (root)
     (git-review-fixtures-create-small root)
     (let ((default-directory (file-name-as-directory root))
           (internal-calls 0))
       (unwind-protect
           (git-review-baseline--with-instrumentation
            (lambda ()
              (delete-other-windows)
              (magit-diff-working-tree "HEAD" '("--no-ext-diff" "-U6"))
              (git-review-phase1--goto-diff-line
               root "README.md" "+line-two-modified")
              (should (eq (+git-review--visit-thing-command)
                          'magit-diff-visit-file))
              (cl-letf* ((orig (symbol-function 'magit-diff-visit-file--internal))
                         ((symbol-function 'magit-diff-visit-file--internal)
                          (lambda (&rest args)
                            (cl-incf internal-calls)
                            (apply orig args)))
                         ((symbol-function 'magit-visit-thing)
                          (lambda (&rest _)
                            (interactive)
                            (user-error "placeholder must not be called"))))
                (+git-review-visit-other-window)
                (should (= internal-calls 1))
                (should (= 2 (count-windows)))
                (select-window (previous-window))
                (git-review-phase1--goto-diff-line
                 root "README.md" "+line-two-modified")
                (+git-review-visit-other-window)
                (should (= internal-calls 2))
                (should (= 2 (count-windows))))))
         (set-frame-parameter nil '+git-review-other-window nil)
         (git-review-baseline-cleanup-repo-buffers root)
         (delete-other-windows))))))

(provide 'git-review-phase1)

;;; git-review-phase1.el ends here
