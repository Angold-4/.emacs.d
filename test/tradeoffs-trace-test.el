;;; tradeoffs-trace-test.el --- ERT tests for init-tradeoffs-trace -*- lexical-binding: t -*-

;; Run: emacs --batch -Q -L core -L test -l test/tradeoffs-trace-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'init-tradeoffs-trace)

(defconst +tt-test--valid-plan "#+TITLE: sum validation
#+TT_REPO: /tmp/tt-ert-repo
#+TT_BRANCH: main
#+TT_CHECKS: node --test

* Phase 1: validate input
  :PROPERTIES:
  :ID:          p1
  :CHECKS:      node --test
  :BOUNDARIES:  src/api/** package.json
  :RESERVED:    public API types; persistence format
  :END:
  Goal: make sum() reject non-numbers
    without coercion.
  Acceptance:
  - existing tests pass
  - sum('a', 1) throws

* Phase 2: metrics                                              :provisional:
  later
")

(defun +tt-test--parse (text)
  "Parse TEXT as a plan buffer."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (setq buffer-file-name "/tmp/tt-ert-plan.org")
    (prog1 (+tt-parse-plan) (set-buffer-modified-p nil) (setq buffer-file-name nil))))

(ert-deftest tradeoffs-trace-plan-parse ()
  "A valid plan becomes the conductor's JSON plan shape."
  (let* ((parsed (+tt-test--parse +tt-test--valid-plan))
         (plan (plist-get parsed :plan))
         (p1 (aref (alist-get 'phases plan) 0)))
    (should (null (plist-get parsed :errors)))
    (should (equal (alist-get 'title plan) "sum validation"))
    (should (equal (alist-get 'integrationBranch plan) "main"))
    (should (equal (alist-get 'id p1) "p1"))
    (should (equal (alist-get 'goal p1) "make sum() reject non-numbers without coercion."))
    (should (equal (alist-get 'acceptance p1) ["existing tests pass" "sum('a', 1) throws"]))
    (should (equal (alist-get 'boundaries p1) ["src/api/**" "package.json"]))
    (should (equal (alist-get 'reserved p1) ["public API types" "persistence format"]))
    (should (eq (alist-get 'provisional (aref (alist-get 'phases plan) 1)) t))))

(ert-deftest tradeoffs-trace-plan-validation ()
  "Invalid plans report errors at the right lines and start no run."
  (let* ((bad "#+TITLE: bad\n#+TT_REPO: /tmp/x\n#+TT_BRANCH: main\n\n* Phase 1: no id\n  Goal: something\n\n* Phase 2: dup\n  :PROPERTIES:\n  :ID: p2\n  :CHECKS: true\n  :END:\n  Goal: g\n  Acceptance:\n  - a\n* Phase 3: dup again\n  :PROPERTIES:\n  :ID: p2\n  :CHECKS: true\n  :END:\n  Goal: g\n  Acceptance:\n  - a\n")
         (errors (plist-get (+tt-test--parse bad) :errors))
         (lines (mapcar #'car errors)))
    (should (member 5 lines))
    (should (seq-find (lambda (e) (string-match-p "no :ID:" (cdr e))) errors))
    (should (seq-find (lambda (e) (string-match-p "no \"Acceptance:\"" (cdr e))) errors))
    (should (seq-find (lambda (e) (and (= (car e) 16) (string-match-p "duplicate phase ID p2" (cdr e)))) errors))
    ;; +tt-run refuses to start: it shows the errors buffer instead.
    (let ((started nil))
      (cl-letf (((symbol-function '+tt--cli) (lambda (&rest _) (setq started t) "x")))
        (with-temp-buffer
          (insert bad) (org-mode)
          (+tt-run)))
      (should-not started)
      (with-current-buffer "*tt-plan-errors*"
        (should (string-match-p "tt-plan\\|:5: phase has no :ID:" (buffer-string)))))))

(defun +tt-test--make-run (root id &optional plan-path)
  "Create a fake run ID under ROOT, optionally recording PLAN-PATH."
  (let ((dir (expand-file-name id root)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "meta.json" dir) (insert (format "{\"title\":\"%s\"}" id)))
    (with-temp-file (expand-file-name "events.jsonl" dir) (insert "{}\n"))
    (when plan-path
      (with-temp-file (expand-file-name "emacs.json" dir) (insert (json-encode `((planPath . ,plan-path))))))
    dir))

(ert-deftest tradeoffs-trace-run-resolution ()
  "Design §1.4: buffer-local run, then the plan's run, then completing-read."
  (let* ((+tt-root (make-temp-file "tt-ert-root" t))
         (a (+tt-test--make-run +tt-root "run-a" "/tmp/plan-a.org"))
         (b (+tt-test--make-run +tt-root "run-b" "/tmp/plan-b.org"))
         (b2 (+tt-test--make-run +tt-root "run-b2" "/tmp/plan-b.org")))
    (unwind-protect
        (progn
          ;; 1. a tradeoffs-trace buffer's own run wins
          (with-temp-buffer (setq +tt--run-dir b) (should (equal (+tt--resolve-run) b)))
          ;; 2. a plan buffer with exactly one run uses it
          (with-temp-buffer (setq buffer-file-name "/tmp/plan-a.org")
                            (should (equal (+tt--resolve-run) a))
                            (setq buffer-file-name nil))
          ;; 2'. several runs of one plan: ask, offering only that plan's runs
          (let (offered)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_p coll &rest _) (setq offered (mapcar #'cdr coll)) (caar coll))))
              (with-temp-buffer (setq buffer-file-name "/tmp/plan-b.org")
                                (+tt--resolve-run)
                                (setq buffer-file-name nil)))
            (should (equal (sort offered #'string<) (sort (list b b2) #'string<))))
          ;; 3. anywhere else: all runs
          (let (offered)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_p coll &rest _) (setq offered coll) (caar coll))))
              (with-temp-buffer (+tt--resolve-run)))
            (should (= (length offered) 3))))
      (delete-directory +tt-root t))))

(defconst +tt-test--state
  '((meta (title . "sum validation"))
    (state (run . "RUN_ACTIVE")
           (phase (runId . "r1") (phaseId . "p1") (phase . "AWAITING_OWNER")
                  (candidate (sha . "7c1e0a4aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
                  (contract (contractVersion (snapshot . 1) (sectionSha256 . "9e2c")))
                  (decisions ((id . "D-p1-01") (version . 2) (class . "delegated") (source . "worker")
                              (choice . "Batch cancels per tick")
                              (whyItMatters . "fewer lock acquisitions under load")
                              (alternatives ((option . "a lock per request") (consequence . "twice the contention")))
                              (recommendation (choice . "batch") (reason . "inside the latency budget")))
                             ((id . "D-p1-02") (version . 1) (class . "detail") (source . "worker")
                              (choice . "helper renamed") (whyItMatters . "readability")
                              (alternatives ((option . "keep") (consequence . "none")))
                              (recommendation (choice . "rename") (reason . "clearer"))))
                  (ballots ((decisionId . "D-p1-01") (reviewer . "A") (vote . "reject") (rationale . "a lone cancel waits a tick"))
                           ((decisionId . "D-p1-01") (reviewer . "M") (vote . "approve") (rationale . "within budget")))
                  (findings ((id . "F-p1-02") (version . 1) (kind . "contract") (status . "open") (raisedBy . "B")
                             (severity . "blocking") (evidence . "no fill after cancel")))
                  (ownerRequests ((id . "R-1") (version . 1) (status . "open") (origin . "open_finding")
                                  (options ((id . "repair") (label . "repair (grant 3 rounds)")))))
                  (corrections))))
  "A fixture `tt state' for the decision view.")

(ert-deftest tradeoffs-trace-decision-render ()
  "The decision view has the §10.1 sections and plain-language fields, details folded."
  (with-temp-buffer
    (+tt--render-decisions +tt-test--state)
    (let ((text (buffer-string)))
      (dolist (s '("Needs you (1)" "Open findings (1)" "Corrections (0)"
                   "Accepted with dissent / pending vote (1)" "For sampling (1 detail)"))
        (should (string-match-p (regexp-quote s) text)))
      (should (string-match-p "Why it matters: fewer lock acquisitions under load" text))
      (should (string-match-p "a lock per request — twice the contention" text))
      (should (string-match-p "Recommendation: batch. inside the latency budget" text))
      (should (string-match-p "A reject: a lone cancel waits a tick" text))
      (should (string-match-p "\\*\\* Details" text)))))

(ert-deftest tradeoffs-trace-commands-bound ()
  "Every decision-view command carries the full binding tuple (design §7.1)."
  (let* ((run (make-temp-file "tt-ert-run" t)))
    (unwind-protect
        (with-temp-buffer
          (+tt--render-decisions +tt-test--state)
          (setq +tt--run-dir run +tt--decision-state +tt-test--state)
          (goto-char (point-min))
          (search-forward "DISSENT Batch cancels")
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "reject")))
            (+tt-decision-override))
          (let* ((files (directory-files (expand-file-name "inbox" run) t "\\.json\\'"))
                 (cmd (json-read-file (car files)))
                 (binding (alist-get 'binding cmd)))
            (should (= (length files) 1))
            (should (equal (alist-get 'type cmd) "override"))
            (should (equal (alist-get 'vote cmd) "reject"))
            (should (equal (alist-get 'runId binding) "r1"))
            (should (equal (alist-get 'phaseId binding) "p1"))
            (should (string-prefix-p "7c1e0a4" (alist-get 'candidateSha binding)))
            (should (equal (alist-get 'recordId binding) "D-p1-01"))
            (should (equal (alist-get 'recordVersion binding) 2))
            (should (alist-get 'contractVersion binding))))
      (delete-directory run t))))

(ert-deftest tradeoffs-trace-stream-render ()
  "The trace renders text deltas and folds tool calls to one line."
  (let ((f (make-temp-file "tt-ert-stream" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:01.377Z\",\"event\":{\"type\":\"agent_start\"}}\n"
                    "{\"agentId\":\"worker-1\",\"event\":{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"text_delta\",\"delta\":\"Moving removal \"}}}\n"
                    "{\"agentId\":\"worker-1\",\"event\":{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"text_delta\",\"delta\":\"under the lock\"}}}\n"
                    "{\"agentId\":\"worker-1\",\"event\":{\"type\":\"tool_execution_start\",\"toolName\":\"sh\",\"args\":{\"command\":\"npm test\"}}}\n"))
          (let ((out (+tt--render-stream f)))
            (should (string-match-p "\\[worker-1 · 06:52\\]" out))
            (should (string-match-p "Moving removal under the lock" out))
            (should (string-match-p "▸ tool: sh" out))))
      (delete-file f))))

(provide 'tradeoffs-trace-test)
;;; tradeoffs-trace-test.el ends here
