;;; git-review-test.el --- ERT tests for Git review Phase 0/1 -*- lexical-binding: t -*-

;;; Commentary:
;; Phase 0 foundation tests: temporary Git fixtures and the baseline
;; measurement helper.  Phase 1 navigation/ownership tests are loaded
;; from `git-review-phase1.el'.  Measurements run under a hard network
;; guard.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-review-fixtures)
(require 'git-review-baseline)

(defun git-review-test--porcelain-lines (root)
  "Return porcelain status lines for ROOT as a list of strings."
  (let ((out (git-review-fixtures-porcelain root "-uall")))
    (if (string-empty-p out)
        nil
      (split-string out "\n" t))))

(defun git-review-test--line-matching (lines regexp)
  "Return the first element of LINES matching REGEXP, or nil."
  (cl-find-if (lambda (line) (string-match-p regexp line)) lines))

(defun git-review-test--stats-shape-p (stats)
  "Return non-nil when STATS has median/min/max/count/samples."
  (and (listp stats)
       (numberp (plist-get stats :median))
       (numberp (plist-get stats :min))
       (numberp (plist-get stats :max))
       (integerp (plist-get stats :count))
       (listp (plist-get stats :samples))
       (= (plist-get stats :count) (length (plist-get stats :samples)))
       (<= (plist-get stats :min) (plist-get stats :median))
       (<= (plist-get stats :median) (plist-get stats :max))))

(ert-deftest git-review-test-small-fixture-state ()
  "Small fixture exposes modified/staged/untracked/added/deleted/renamed/nested."
  (git-review-fixtures-with-repo
   "small"
   (lambda (root)
     ;; Path must contain a space (fixture helper guarantee).
     (should (string-match-p " " root))
     (git-review-fixtures-create-small root)

     (let* ((lines (git-review-test--porcelain-lines root))
            (cached (git-review-fixtures-name-status root)))
       ;; Nested directory still present as a tracked path.
       (should (file-directory-p (expand-file-name "src/nested" root)))
       (should (file-readable-p (expand-file-name "src/nested/app.el" root)))

       ;; Unstaged modification of tracked README.md (space in XY: " M").
       (should (git-review-test--line-matching lines "^ M README\\.md$"))

       ;; Staged modification of lib/util.el.
       (should (git-review-test--line-matching lines "^M  lib/util\\.el$"))

       ;; Untracked file.
       (should (git-review-test--line-matching lines "^\\?\\? untracked\\.txt$"))

       ;; Staged added file.
       (should (git-review-test--line-matching lines "^A  added\\.el$"))

       ;; Staged deletion.
       (should (git-review-test--line-matching lines "^D  to-delete\\.el$"))

       ;; Staged rename. Porcelain uses "R  old -> new"; name-status may
       ;; include a similarity score (R100). Fall back to D+A if needed.
       (should (or (git-review-test--line-matching
                    lines "^R[ 0-9]+old-name\\.el -> new-name\\.el$")
                   (and (git-review-test--line-matching lines "^D  old-name\\.el$")
                        (git-review-test--line-matching lines "^A  new-name\\.el$"))))

       ;; Machine-readable cached name-status confirms add/delete/rename/mod.
       (should (string-match-p "^M\tlib/util\\.el$" cached))
       (should (string-match-p "^A\tadded\\.el$" cached))
       (should (string-match-p "^D\tto-delete\\.el$" cached))
       (should (or (string-match-p "^R[0-9]+\told-name\\.el\tnew-name\\.el$" cached)
                   (and (string-match-p "^D\told-name\\.el$" cached)
                        (string-match-p "^A\tnew-name\\.el$" cached))))))))

(ert-deftest git-review-test-large-fixture-change-count ()
  "Large fixture cheaply creates at least 100 changed files."
  (git-review-fixtures-with-repo
   "large"
   (lambda (root)
     (should (string-match-p " " root))
     (git-review-fixtures-create-large root 100)
     (let* ((out (string-trim-right
                  (git-review-fixtures--git-ok
                   root "diff" "--name-only")))
            (files (if (string-empty-p out)
                       nil
                     (split-string out "\n" t))))
       (should (>= (length files) 100))
       (should (cl-every (lambda (f) (string-prefix-p "files/" f)) files))))))

(ert-deftest git-review-test-fixture-uses-local-identity ()
  "Fixtures configure a local name/email and do not need a global identity."
  (git-review-fixtures-with-repo
   "identity"
   (lambda (root)
     (git-review-fixtures--write-file root "a.txt" "a\n")
     (git-review-fixtures--git-ok root "add" "--" "a.txt")
     (git-review-fixtures--git-ok root "commit" "-m" "identity check")
     (let ((name (string-trim-right
                  (git-review-fixtures--git-ok
                   root "log" "-1" "--format=%an")))
           (email (string-trim-right
                   (git-review-fixtures--git-ok
                    root "log" "-1" "--format=%ae"))))
       (should (equal name git-review-fixtures-test-name))
       (should (equal email git-review-fixtures-test-email))))))

(ert-deftest git-review-test-call-git-handles-spaces-in-path ()
  "Git helper accepts repository paths that contain spaces."
  (git-review-fixtures-with-repo
   "space path"
   (lambda (root)
     (should (string-match-p " " root))
     (git-review-fixtures--write-file root "ok.txt" "ok\n")
     (git-review-fixtures--git-ok root "add" "--" "ok.txt")
     (git-review-fixtures--git-ok root "commit" "-m" "space path ok")
     (let ((out (string-trim-right
                 (git-review-fixtures--git-ok
                  root "rev-parse" "--is-inside-work-tree"))))
       (should (equal out "true"))))))

(ert-deftest git-review-test-fixtures-cleaned-up ()
  "Temporary fixture directories are removed after the helper returns."
  (let (parent)
    (git-review-fixtures-with-repo
     "cleanup"
     (lambda (root)
       (setq parent (file-name-directory (directory-file-name root)))
       (should (file-directory-p root))
       (git-review-fixtures--write-file root "x.txt" "x\n")))
    (should (not (file-exists-p parent)))))

(ert-deftest git-review-test-baseline-report-shape ()
  "Baseline measurement returns prewarmed median/min/max stats and a report."
  (require 'magit)
  (git-review-fixtures-with-repo
   "baseline-shape"
   (lambda (root)
     (git-review-fixtures-create-large root 20)
     (let* ((report (git-review-baseline-measure-repo root 3))
            (text (git-review-baseline--format-report report)))
       (should (eq (plist-get report :prewarmed) t))
       (should (eq (plist-get report :network-guard) 'hard))
       (should (= (plist-get report :sample-count) 3))
       (should (git-review-test--stats-shape-p (plist-get report :status)))
       (should (git-review-test--stats-shape-p (plist-get report :refresh)))
       (should (git-review-test--stats-shape-p (plist-get report :diff)))
       (should (= (plist-get (plist-get report :status) :count) 3))
       (should (string-match-p "median" text))
       (should (string-match-p "network guard: hard" text))
       (should (not (git-review-baseline--advice-installed-p)))
       (git-review-baseline-cleanup-repo-buffers root)))))

(ert-deftest git-review-test-baseline-advice-removed-on-failure ()
  "Measurement advice is removed even when the timed body signals."
  (should (not (git-review-baseline--advice-installed-p)))
  (should-error
   (git-review-baseline--with-instrumentation
    (lambda () (error "forced failure")))
   :type 'error)
  (should (not (git-review-baseline--advice-installed-p)))
  (should (null git-review-baseline--guard-active)))

(ert-deftest git-review-test-baseline-network-guard ()
  "Hard network guard blocks URL and remote Git invocations."
  (require 'url)
  ;; URL retrieve is blocked.
  (should-error
   (git-review-baseline--with-instrumentation
    (lambda ()
      (url-retrieve-synchronously "https://example.com")))
   :type 'error)
  (should (not (git-review-baseline--advice-installed-p)))
  (should (eq (caar git-review-baseline--blocked-attempts)
              'url-retrieve-synchronously))

  ;; make-network-process is blocked directly.
  (setq git-review-baseline--blocked-attempts nil)
  (should-error
   (git-review-baseline--with-instrumentation
    (lambda ()
      (make-network-process :name "git-review-baseline-test"
                            :host "example.com"
                            :service 443)))
   :type 'error)
  (should (not (git-review-baseline--advice-installed-p)))
  (should (eq (caar git-review-baseline--blocked-attempts)
              'make-network-process))

  ;; git fetch is blocked.
  (setq git-review-baseline--blocked-attempts nil)
  (should-error
   (git-review-baseline--with-instrumentation
    (lambda ()
      (call-process "git" nil nil nil "fetch" "origin")))
   :type 'error)
  (should (not (git-review-baseline--advice-installed-p)))
  (should (eq (caar git-review-baseline--blocked-attempts) 'process))

  ;; curl https is blocked.
  (setq git-review-baseline--blocked-attempts nil)
  (should-error
   (git-review-baseline--with-instrumentation
    (lambda ()
      (call-process "curl" nil nil nil "https://example.com")))
   :type 'error)
  (should (eq (caar git-review-baseline--blocked-attempts) 'process))

  ;; Plain git remote update is blocked.
  (setq git-review-baseline--blocked-attempts nil)
  (should-error
   (git-review-baseline--with-instrumentation
    (lambda ()
      (call-process "git" nil nil nil "remote" "update")))
   :type 'error)
  (should (eq (caar git-review-baseline--blocked-attempts) 'process))

  ;; Magit-style prefixed remote update is blocked.
  (setq git-review-baseline--blocked-attempts nil)
  (should-error
   (git-review-baseline--with-instrumentation
    (lambda ()
      (call-process "git" nil nil nil
                    "--no-pager" "-c" "foo=bar" "remote" "update")))
   :type 'error)
  (should (eq (caar git-review-baseline--blocked-attempts) 'process))

  ;; git -C /path remote update is blocked.
  (setq git-review-baseline--blocked-attempts nil)
  (should-error
   (git-review-baseline--with-instrumentation
    (lambda ()
      (call-process "git" nil nil nil "-C" "/tmp" "remote" "update")))
   :type 'error)
  (should (eq (caar git-review-baseline--blocked-attempts) 'process))

  ;; Local git status is allowed.
  (git-review-fixtures-with-repo
   "guard-local"
   (lambda (root)
     (git-review-fixtures--write-file root "a.txt" "a\n")
     (git-review-fixtures--git-ok root "add" "--" "a.txt")
     (git-review-fixtures--git-ok root "commit" "-m" "local")
     (let* ((default-directory (file-name-as-directory root))
            (pair (git-review-baseline--with-instrumentation
                   (lambda ()
                     (setq git-review-baseline--recording-p t)
                     (call-process "git" nil t nil "status" "--porcelain")))))
       (should (numberp (car pair)))
       (should (not (git-review-baseline--advice-installed-p)))))))

(ert-deftest git-review-test-baseline-guard-during-prewarm-and-sample ()
  "Hard guard is active for both prewarm and timed sample callbacks."
  (let ((prewarm-guard nil)
        (sample-guard nil)
        (prewarm-recording 'unset)
        (sample-recording 'unset))
    (git-review-baseline--measure-metric
     (lambda ()
       (setq prewarm-guard git-review-baseline--guard-active
             prewarm-recording git-review-baseline--recording-p))
     (lambda ()
       (setq sample-guard git-review-baseline--guard-active
             sample-recording git-review-baseline--recording-p)
       t)
     1)
    (should (eq prewarm-guard t))
    (should (eq sample-guard t))
    (should (null prewarm-recording))
    (should (eq sample-recording t))
    (should (not (git-review-baseline--advice-installed-p)))))

(ert-deftest git-review-test-baseline-no-double-record ()
  "Nested process APIs record one child command, not two."
  (pcase-let*
      ((`(,_result . ,log)
        (git-review-baseline--with-instrumentation
         (lambda ()
           (setq git-review-baseline--recording-p t)
           ;; `process-file' calls `call-process' internally on local files.
           (process-file "true" nil nil nil)))))
    (should (= 1 (length log)))
    (should (equal (file-name-nondirectory (nth 1 (car log))) "true"))
    (should (not (git-review-baseline--advice-installed-p)))))

(ert-deftest git-review-test-baseline-magit-buffer-cleanup ()
  "Fixture Magit buffers are killed before the temporary repo is deleted."
  (require 'magit)
  (let* ((root (git-review-fixtures-make-temp-root "magit-cleanup"))
         (parent (file-name-directory (directory-file-name root)))
         status-buf
         diff-buf)
    (unwind-protect
        (progn
          (git-review-fixtures--init-repo root)
          (git-review-fixtures-create-small root)
          (let ((default-directory (file-name-as-directory root)))
            (magit-status-setup-buffer default-directory)
            (setq status-buf (magit-get-mode-buffer 'magit-status-mode))
            (magit-diff-working-tree nil)
            (setq diff-buf (magit-get-mode-buffer 'magit-diff-mode)))
          (should (buffer-live-p status-buf))
          (should (buffer-live-p diff-buf))
          (let ((killed (git-review-baseline-cleanup-repo-buffers root)))
            (should (>= (length killed) 2))
            (should (not (buffer-live-p status-buf)))
            (should (not (buffer-live-p diff-buf)))
            (should (null (git-review-baseline--magit-buffers-for-root root)))))
      (git-review-baseline-cleanup-repo-buffers root)
      (ignore-errors (delete-directory parent t)))))

(ert-deftest git-review-test-baseline-repeated-warm-samples ()
  "Warm metrics collect multiple samples after an untimed prewarm."
  (require 'magit)
  (git-review-fixtures-with-repo
   "warm-samples"
   (lambda (root)
     (git-review-fixtures-create-large root 15)
     (let* ((calls 0)
            (result
             (git-review-baseline--measure-metric
              (lambda ()
                (cl-incf calls)
                (should git-review-baseline--guard-active)
                (git-review-baseline--ensure-status root))
              (lambda ()
                (cl-incf calls)
                (should git-review-baseline--guard-active)
                (git-review-baseline--refresh-status))
              4))
            (stats (plist-get result :stats)))
       ;; One prewarm + four timed samples.
       (should (= calls 5))
       (should (= (plist-get stats :count) 4))
       (should (= (length (plist-get stats :samples)) 4))
       (should (git-review-test--stats-shape-p stats))
       (should (not (git-review-baseline--advice-installed-p)))
       (git-review-baseline-cleanup-repo-buffers root)))))

;; Phase 1 foundation tests.
(require 'git-review-phase1)

;; Phase 2 local review / Changes Tree tests.
(require 'git-review-phase2)

;; Phase 3 canonical identity / local-context integration tests.
(require 'git-review-integration-test)

(provide 'git-review-test)

;;; git-review-test.el ends here
