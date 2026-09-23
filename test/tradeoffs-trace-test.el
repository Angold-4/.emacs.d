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
           (phase (runId . "r1") (phaseId . "p1") (phase . "IMPLEMENTING")
                  (attempt (n . 2))
                  (candidate (sha . "7c1e0a4aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
                  (contract (contractVersion (snapshot . 1) (sectionSha256 . "9e2c")))
                  (decisions ((id . "D-p1-7c1e0a4a-1") (version . 2) (class . "delegated") (source . "worker")
                              (choice . "Batch cancels per tick")
                              (whyItMatters . "fewer lock acquisitions under load")
                              (alternatives ((option . "batch per tick") (consequence . "one lock per tick"))
                                            ((option . "a lock per request") (consequence . "twice the contention")))
                              (recommendation (choice . "batch per tick") (reason . "inside the latency budget")))
                             ((id . "D-p1-7c1e0a4a-disc-A-2") (version . 1) (class . "delegated")
                              (source . "reviewer-discovered") (alsoSeenBy "M")
                              (choice . "Errors are thrown, not returned") (whyItMatters . "callers must catch")
                              (alternatives ((option . "throw") (consequence . "loud")))
                              (recommendation (choice . "throw") (reason . "fail fast")))
                             ((id . "D-p1-old-1") (version . 3) (class . "delegated") (source . "worker")
                              (supersededBy . "candidate 7c1e0a4: not carried forward")
                              (choice . "an old choice") (whyItMatters . "x")
                              (alternatives ((option . "a") (consequence . "b"))))
                             ((id . "D-p1-7c1e0a4a-trigger-3") (version . 1) (class . "delegated") (source . "trigger")
                              (choice . "Diff touches boundary path src/api") (whyItMatters . "boundary")
                              (alternatives ((option . "a") (consequence . "b")))))
                  (ballots ((decisionId . "D-p1-7c1e0a4a-1") (reviewer . "A") (vote . "reject") (rationale . "a lone cancel waits a tick"))
                           ((decisionId . "D-p1-7c1e0a4a-1") (reviewer . "M") (vote . "approve") (rationale . "within budget")))
                  (findings ((id . "F-p1-M-4") (version . 1) (kind . "defect") (status . "open") (raisedBy . "M")
                             (alsoRaisedBy "B") (severity . "blocking")
                             (evidence . "src/sum.js:12 accepts NaN. It returns NaN to callers.")))
                  (ownerRequests)
                  (corrections)))
    (decisionStatuses (D-p1-7c1e0a4a-1 (status . "failed") (reason . "M veto"))
                      (D-p1-7c1e0a4a-disc-A-2 (status . "passed") (flagged . t))
                      (D-p1-old-1 (status . "superseded") (reason . "not carried forward"))
                      (D-p1-7c1e0a4a-trigger-3 (status . "pending")))
    (view (round . 2) (reviewLine . "M ✗ 1 blocking   A ✗ 1 reject   B ✓")
          (verdict . "not accepted: D-1 vetoed by M; blocking finding F-M-4 open → repair attempt 2")
          (addressing "D-1 vetoed by M" "blocking finding F-M-4 open")
          (needsYou . 0)
          (rounds ((round . 1) (candidateSha . "1234567aaaa") (outcome . "not accepted: checks failed")))))
  "A fixture `tt state' for the decision view.")

(ert-deftest tradeoffs-trace-decision-render ()
  "Plan 3b: only current-round decisions, as self-contained blocks labelled by the tally."
  (with-temp-buffer
    (+tt--render-decisions +tt-test--state)
    (let ((text (buffer-string)))
      (should (string-match-p "Round 2 · candidate 7c1e0a4 · attempt 2 — addressing: D-1 vetoed by M; blocking finding F-M-4 open" text))
      ;; the tally's result, not the ballots: M's veto rejects it although M approved nothing else
      (should (string-match-p "\\* REJECTED (M veto)  Batch cancels per tick" text))
      (should (string-match-p "\\* ACCEPTED ⚑ FLAGGED  Errors are thrown, not returned" text))
      (should (string-match-p "raised by reviewer A (discovered); also seen by M" text))
      (should (string-match-p "Why it matters: fewer lock acquisitions under load" text))
      (should (string-match-p "● batch per tick — one lock per tick" text))
      (should (string-match-p "○ a lock per request — twice the contention" text))
      (should (string-match-p "A reject — a lone cancel waits a tick" text))
      ;; superseded records and boundary triggers are not listed as decisions
      (should-not (string-match-p "an old choice" text))
      (should-not (string-match-p "Diff touches boundary" text))
      ;; findings grouped by location with also-raised-by; earlier rounds one line
      (should (string-match-p "\\* src/sum.js" text))
      (should (string-match-p "BLOCKING src/sum.js:12 accepts NaN — M; also raised by B" text))
      (should (string-match-p "\\* Round 1 · 1234567 · not accepted: checks failed" text)))))

(ert-deftest tradeoffs-trace-decision-view-read-only ()
  "The decision view has no action keys: g, TAB and q only."
  (should-not (fboundp '+tt-decision-override))
  (should-not (fboundp '+tt-decision-resolve))
  (dolist (key '("r" "o" "x" "s" "u" "w" "1" "2" "3"))
    (should-not (keymap-lookup +tt-decisions-mode-map key)))
  (should (eq (keymap-lookup +tt-decisions-mode-map "g") '+tt-decisions-refresh)))

(defun +tt-test--stream (lines)
  "A temporary stream file holding LINES."
  (let ((f (make-temp-file "tt-ert-stream" nil ".jsonl")))
    (with-temp-file f (insert (mapconcat #'identity lines "\n") "\n"))
    f))

(ert-deftest tradeoffs-trace-trace-lines ()
  "Plan 3b: one line per tool call with time, verb, result and duration; file changes under it."
  (let* ((root (make-temp-file "tt-ert-run" t))
         (dir (expand-file-name "stream" root))
         (f (progn (make-directory dir) (expand-file-name "worker-1.jsonl" dir))))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:01.000Z\",\"event\":{\"type\":\"agent_start\"}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:02.000Z\",\"event\":{\"type\":\"message_update\",\"assistantMessageEvent\":{\"type\":\"text_delta\",\"delta\":\"x\"}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:03.000Z\",\"event\":{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Now I run the narrow test.\"}]}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:04.000Z\",\"event\":{\"type\":\"tool_execution_start\",\"toolCallId\":\"t1\",\"toolName\":\"sh\",\"args\":{\"command\":\"node --test test.js\"}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:08.500Z\",\"event\":{\"type\":\"tool_execution_end\",\"toolCallId\":\"t1\",\"isError\":true,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"ok 1\\nnot ok 2 - rejects NaN\\n\"}],\"details\":{\"exitCode\":1}}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:09.000Z\",\"event\":{\"type\":\"tool_execution_start\",\"toolCallId\":\"t2\",\"toolName\":\"edit\",\"args\":{\"path\":\"sum.js\"}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:09.200Z\",\"event\":{\"type\":\"tool_execution_end\",\"toolCallId\":\"t2\",\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"ok\"}]}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:09.300Z\",\"event\":{\"type\":\"tt_file_changes\",\"toolCallId\":\"t2\",\"files\":[{\"path\":\"sum.js\",\"added\":12,\"removed\":3}]}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:10.000Z\",\"event\":{\"type\":\"tool_execution_start\",\"toolCallId\":\"t3\",\"toolName\":\"sh\",\"args\":{\"command\":\"sleep 20\"}}}\n"))
          (with-temp-buffer
            (+tt-trace-mode)
            (setq +tt--run-dir root)
            (+tt--render-trace)
            (let ((text (buffer-string)))
              (should (string-match-p "── worker-1 · " text))
              (should (string-match-p "» Now I run the narrow test." text))
              (should (string-match-p "\\$ node --test test.js ✗1 4s · not ok 2 - rejects NaN" text))
              (should (string-match-p "edit sum.js ✓ 0s" text))
              (should (string-match-p "sum.js \\+12 −3" text))
              (should-not (string-match-p "\n\n" text)))
            ;; the running call is in the header, marked as polling
            (should (string-match-p "⧗ \\$ sleep 20  (polling)" header-line-format))
            ;; incremental: a later append renders only the new part
            (with-temp-file f
              (insert-file-contents f)
              (goto-char (point-max))
              (insert "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:30.000Z\",\"event\":{\"type\":\"tool_execution_end\",\"toolCallId\":\"t3\",\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"\"}]}}}\n"))
            (+tt--render-trace)
            (should (string-match-p "\\$ sleep 20  (polling) ✓ 20s" (buffer-string)))
            (should (= 1 (how-many "edit sum.js" (point-min) (point-max))))))
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-status-rows ()
  "Plan 3b: the status shows the pipeline, the time line, outcomes and the verdict."
  (with-temp-buffer
    (+tt--render-status-from
     `((meta (title . "sum validation"))
       (conductorAlive . t)
       (ownerInputs) (pendingOwnerInputs)
       (state (run . "RUN_ACTIVE")
              (phase (phaseId . "p1") (phase . "REVIEWING") (attempt (n . 1))
                     (repairRoundsUsed . 0) (repairRoundsGranted . 3)))
       (view (elapsed . "1m02s") (round . 1)
             (pipeline . "implement 30s → freeze 1s → checks 1s → probe 0s → review 12s… (14m48s left)")
             (time . "reviewer-M: model 80% · polling 0% · full tests 0%")
             (gates . "checks ✓ · probe ✓ (reused)")
             (reviewLine . "M ✗ 2 reject · 1 blocking   A ✓   B ⧗")
             (liveDecisions . 4) (failedDecisions . 0) (flaggedDecisions . 2) (openFindings . 1) (boundaryFilesChanged . 2)))
     "/tmp/tt-ert/abcd1234")
    (let ((text (buffer-string)))
      (should (string-match-p "run abcd1234 · conductor running · 1m02s" text))
      (should (string-match-p "pipeline  implement 30s → freeze 1s" text))
      (should (string-match-p "time      reviewer-M: model 80%" text))
      (should (string-match-p "reviews   M ✗ 2 reject · 1 blocking   A ✓   B ⧗" text))
      (should (string-match-p "4 decisions · 2 flagged for you · 1 open findings" text))
      (should (string-match-p "boundary files changed: 2 (reviewers classify)" text))
      ;; empty sections are not shown
      (should-not (string-match-p "Owner input" text))
      (should-not (string-match-p "verdict" text)))))

(defun +tt-test--input-state (phase &optional alive blocked requests)
  "A minimal `tt state' for the input-header tests."
  `((conductorAlive . ,(if alive t :false))
    (ownerInputs)
    (pendingOwnerInputs)
    (state (run . "RUN_ACTIVE")
           (phase (runId . "r1") (phaseId . "p1") (phase . ,phase)
                  (attempt (n . 2))
                  (blockedReason . ,blocked)
                  (ownerRequests . ,requests)))))

(ert-deftest tradeoffs-trace-input-header ()
  "Design §7.4/§7.5: the input header states what sending will do now."
  (should (string-match-p "refused: the run is DONE"
                          (+tt--input-header (+tt-test--input-state "DONE" t))))
  (should (string-match-p "refused: the phase is BLOCKED (reviewer unavailable)"
                          (+tt--input-header (+tt-test--input-state "BLOCKED" t "reviewer unavailable"))))
  (should (string-match-p "Cannot deliver input"
                          (+tt--input-header (+tt-test--input-state "IMPLEMENTING" nil))))
  (should (string-match-p "steers worker attempt 2"
                          (+tt--input-header (+tt-test--input-state "IMPLEMENTING" t))))
  (should (string-match-p "notes the next worker attempt"
                          (+tt--input-header (+tt-test--input-state "REVIEWING" t))))
  (should (string-match-p "corrects the phase: resolves 2 open owner request"
                          (+tt--input-header
                           (+tt-test--input-state
                            "AWAITING_OWNER" t nil
                            '(((id . "OR-1") (status . "open"))
                              ((id . "OR-2") (status . "open"))))))))

(defconst +tt-test--owner-input-state
  '((meta (title . "sum validation"))
    (state (run . "RUN_ACTIVE")
           (phase (runId . "r1") (phaseId . "p1") (phase . "IMPLEMENTING")
                  (attempt (n . 1)) (repairRoundsUsed . 0) (repairRoundsGranted . 3)
                  (reviews) (decisions) (findings) (ownerRequests)))
    (ownerInputs ((id . "c1") (kind . "steer") (state . "delivered") (text . "steer this"))
                 ((id . "c2") (kind . "note") (state . "noted") (text . "note that"))
                 ((id . "c3") (kind . "correction") (state . "correction-started") (text . "correct it"))
                 ((id . "c4") (kind . "steer") (state . "delivery-uncertain") (text . "maybe")
                  (reason . "the conductor restarted"))
                 ((id . "c5") (kind . "note") (state . "refused") (text . "too late")
                  (reason . "the phase is DONE")))
    (pendingOwnerInputs ((id . "p1") (kind . "note") (text . "waiting") (at . "2000-01-01T00:00:00.000Z"))))
  "A fixture with one owner input in each recorded state, plus a stale pending one.")

(ert-deftest tradeoffs-trace-owner-input-section ()
  "The status buffer's Owner input section shows each recorded state."
  (with-temp-buffer
    (+tt--render-owner-inputs +tt-test--owner-input-state)
    (let ((text (buffer-string)))
      (should (string-match-p "Owner input (6)" text))
      (should (string-match-p "steer this — delivered (steer)" text))
      (should (string-match-p "note that — noted (note)" text))
      (should (string-match-p "correct it — correction started (correction)" text))
      (should (string-match-p "maybe — delivery uncertain (the conductor restarted) (steer)" text))
      (should (string-match-p "too late — refused: the phase is DONE (note)" text))
      (should (string-match-p "waiting — not picked up (note)" text)))))

(provide 'tradeoffs-trace-test)
;;; tradeoffs-trace-test.el ends here
