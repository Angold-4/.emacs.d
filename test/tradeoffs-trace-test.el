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

(ert-deftest tradeoffs-trace-plan-rerun-keyword ()
  "Plan 05d: #+TT_RERUN becomes the plan's rerun template and its line."
  (let* ((text (concat "#+TITLE: rerun\n"
                      "#+TT_REPO: /tmp/tt-ert-repo\n"
                      "#+TT_BRANCH: main\n"
                      "#+TT_CHECKS: node --test\n"
                      "#+TT_RERUN: node --test --test-name-pattern {name} {file}\n"
                      "\n"
                      "* Phase 1: p\n"
                      "  :PROPERTIES:\n"
                      "  :ID:          p1\n"
                      "  :CHECKS:      node --test\n"
                      "  :END:\n"
                      "  Goal: g\n"
                      "  Acceptance:\n"
                      "  - a\n"))
         (plan (plist-get (+tt-test--parse text) :plan)))
    (should (equal (alist-get 'rerun plan) "node --test --test-name-pattern {name} {file}"))
    (should (= (alist-get 'rerunLine plan) 5)))
  ;; A plan without the keyword carries neither field.
  (let ((plan (plist-get (+tt-test--parse +tt-test--valid-plan) :plan)))
    (should (null (alist-get 'rerun plan)))
    (should (null (alist-get 'rerunLine plan)))))

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

(ert-deftest tradeoffs-trace-plan-owner-checklist ()
  "Plan 01c: an `Owner checklist:' list parses beside Acceptance, not into it.
Its items are the owner's, and their lines are recorded for `tt lint'."
  (let* ((text (concat "#+TITLE: t\n#+TT_REPO: /tmp/x\n#+TT_BRANCH: main\n\n"
                       "* Phase 1: p\n  :PROPERTIES:\n  :ID: p1\n  :CHECKS: true\n  :END:\n"
                       "  Goal: g\n  Acceptance:\n  - a worker check\n  - all existing tests still pass\n"
                       "  Owner checklist:\n  - the owner records a live run\n  - the owner rules on K4\n"))
         (parsed (+tt-test--parse text))
         (p1 (aref (alist-get 'phases (plist-get parsed :plan)) 0)))
    (should (null (plist-get parsed :errors)))
    ;; The checklist is not acceptance: the worker and reviewers never see it.
    (should (equal (alist-get 'acceptance p1) ["a worker check" "all existing tests still pass"]))
    (should (equal (alist-get 'ownerChecklist p1) ["the owner records a live run" "the owner rules on K4"]))
    (should (= (length (alist-get 'acceptanceLines p1)) 2))
    (should (= (length (alist-get 'ownerChecklistLines p1)) 2))
    ;; The plan records its source file so `tt lint' can name it.
    (should (equal (alist-get 'sourceFile (plist-get parsed :plan)) "/tmp/tt-ert-plan.org"))
    ;; A plan without the list has no ownerChecklist key.
    (should-not (assq 'ownerChecklist
                      (aref (alist-get 'phases (plist-get (+tt-test--parse +tt-test--valid-plan) :plan)) 0)))))

(ert-deftest tradeoffs-trace-plan-lint-blocks-run ()
  "Plan 01c: a lint error shown by `tt lint' blocks `+tt-run'.
Emacs mirrors the one implementation by shelling out: `+tt--cli' is stubbed
the way a lint failure would behave, and no run may start."
  (let ((started nil) (linted nil))
    (cl-letf (((symbol-function '+tt--cli)
               (lambda (cmd &rest _)
                 (if (equal cmd "lint")
                     (progn (setq linted t)
                            (error "tt lint failed: PLAN.org:39: error: [p1] the owner is the actor"))
                   (setq started t)
                   "run-id"))))
      (with-temp-buffer
        (insert +tt-test--valid-plan)
        (setq buffer-file-name "/tmp/tt-ert-lint-plan.org")
        (org-mode)
        (+tt-run)
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)))
    (should linted)
    (should-not started)
    (with-current-buffer "*tt-plan-errors*"
      (should (string-match-p "PLAN\\.org:39: error" (buffer-string))))))

(ert-deftest tradeoffs-trace-status-owner-checklist ()
  "Plan 01c: the status buffer shows the owner checklist once DONE, and only
then.  It is never part of the worker's or a reviewer's acceptance."
  (let* ((plan (list (cons 'title "split")
                     (cons 'phases
                           (list (list (cons 'id "13.10") (cons 'goal "g")
                                       (cons 'acceptance nil)
                                       (cons 'ownerChecklist (list "the owner records a live run"
                                                                   "the owner rules on K4")))))))
         (state-for (lambda (name)
                      `((meta (title . "split"))
                        (conductorAlive . :false)
                        (ownerInputs) (pendingOwnerInputs)
                        (secrets (declared) (missing) (tooShort))
                        (plan . ,plan)
                        (state (run . "RUN_ACTIVE")
                               (phase (phaseId . "13.10") (phase . ,name) (attempt (n . 1))
                                      (repairRoundsUsed . 0) (repairRoundsGranted . 3)))
                        (view (elapsed . "1m") (round . 1) (pipeline . ,name)
                              (reviewLine . "M ✓   A ✓   B ✓")
                              (liveDecisions . 0) (failedDecisions . 0)
                              (flaggedDecisions . 0) (openFindings . 0)
                              (boundaryFilesChanged . 0))))))
    (with-temp-buffer
      (+tt--render-status-from (funcall state-for "DONE") "/tmp/tt-ert/abcd1234")
      (should (string-match-p "Owner checklist (2)" (buffer-string)))
      (should (string-match-p "- the owner records a live run" (buffer-string))))
    (with-temp-buffer
      (+tt--render-status-from (funcall state-for "REVIEWING") "/tmp/tt-ert/abcd1234")
      (should-not (string-match-p "Owner checklist" (buffer-string))))))

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
       (secrets (declared "FAKE_KEY" "OTHER_KEY" "TT") (missing "FAKE_KEY") (tooShort "TT"))
       (state (run . "RUN_ACTIVE")
              (phase (phaseId . "p1") (phase . "REVIEWING") (attempt (n . 1))
                     (repairRoundsUsed . 0) (repairRoundsGranted . 3)))
       (view (elapsed . "1m02s") (round . 1)
             (pipeline . "implement 30s → freeze 1s → checks 1s → probe 0s → review 12s… (14m48s left)")
             (time . "reviewer-M: model 80% · polling 0% · full tests 0%")
             (gates . "checks ✓ · probe ✓ (reused)")
             (reviewLine . "M ✗ 2 reject · 1 blocking   A ✓   B ⧗")
             (review . "T 4 (2 raw) · F 1 · B 0 · C-c m d")
             (boundaryFilesChanged . 2)))
     "/tmp/tt-ert/abcd1234")
    (let ((text (buffer-string)))
      (should (string-match-p "run abcd1234 · conductor running · 1m02s" text))
      (should (string-match-p "pipeline  implement 30s → freeze 1s" text))
      (should (string-match-p "time      reviewer-M: model 80%" text))
      (should (string-match-p "reviews   M ✗ 2 reject · 1 blocking   A ✓   B ⧗" text))
      ;; Plan 05c: the old `records … decisions' row is gone; the `review' row
      ;; is in trade-off vocabulary.
      (should (string-match-p "review    T 4 (2 raw) · F 1 · B 0 · C-c m d" text))
      (should-not (string-match-p "decisions" text))
      ;; Advisory A-5: the boundary-changed note survives as its own row.
      (should (string-match-p "boundary  files changed: 2 (reviewers classify)" text))
      ;; Plan 01a: an unset declared secret is reported by name; a set one is
      ;; not, and a value too short to mask is reported too.
      (should (string-match-p "secret    FAKE_KEY not set" text))
      (should (string-match-p "secret    TT too short to mask" text))
      (should-not (string-match-p "OTHER_KEY" text))
      ;; empty sections are not shown
      (should-not (string-match-p "Owner input" text))
      (should-not (string-match-p "verdict" text)))))

(defun +tt-test--input-state (phase &optional alive blocked requests program)
  "A minimal `tt state' for the input-header tests."
  `((conductorAlive . ,(if alive t :false))
    (program . ,(if program '((programId . "prog1") (node . "13a")) nil))
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

(defconst +tt-test--directive-state
  '((meta (title . "sum validation"))
    (conductorAlive . t)
    (ownerInputs)
    (pendingOwnerInputs)
    (state (run . "RUN_ACTIVE")
           (phase (runId . "r1") (phaseId . "p1") (phase . "REVIEWING")
                  (attempt (n . 2)) (repairRoundsUsed . 0) (repairRoundsGranted . 3)
                  (ownerRequests)
                  (ownerDirectives
                   ((id . "OD-1") (seq . 1)
                    (text . "the 14 exchange-state-machine failures are pre-existing, not yours")
                    (scope . "phase") (status . "in-force")
                    (targets "worker" "M" "A" "B")
                    ;; A has not acknowledged yet, so it renders ⧗ (the plan's
                    ;; own example): the delivery state is never inferred.
                    (deliveries (worker . "delivered") (M . "delivered")
                                (B . "delivered")))
                   ;; A program-wide ruling lives in its own ODP namespace,
                   ;; so one id names one ruling at both levels.
                   ((id . "ODP-2") (seq . 2)
                    (text . "no node may touch the vendor adapters after this ruling")
                    (scope . "program") (status . "withdrawn")
                    (targets) (deliveries))))))
  "A fixture `tt state' with one directive in force and one withdrawn.")

(ert-deftest tradeoffs-trace-owner-directives-section ()
  "Plan 01i: the status shows each directive, its scope, whether it is in
force, and the delivery state per live agent."
  (with-temp-buffer
    (+tt--render-owner-inputs +tt-test--directive-state)
    (let ((text (buffer-string)))
      (should (string-match-p "Owner directives (2)" text))
      (should (string-match-p
               (regexp-quote
                "OD-1 [this phase, in force] the 14 exchange-state-machine failures are pre-existing, not yours — worker ✓ M ✓ A ⧗ B ✓")
               text))
      (should (string-match-p
               (regexp-quote
                "ODP-2 [whole program, withdrawn] no node may touch the vendor adapters after this ruling — (no live agent; carried in every later prompt)")
               text)))))

(ert-deftest tradeoffs-trace-input-header-scope ()
  "Plan 01i (D5): the input header states the directive's scope, for a run's
box, a program node's box, and a program buffer's box."
  ;; A node of a program: `C-u' really can reach the whole program.
  (should (string-match-p "owner directive applying to this phase"
                          (+tt--input-header (+tt-test--input-state "IMPLEMENTING" t nil nil t))))
  (should (string-match-p "C-u C-c C-c: whole program"
                          (+tt--input-header (+tt-test--input-state "IMPLEMENTING" t nil nil t))))
  ;; A hand-started run has nothing program-wide to reach, and says so.
  (should (string-match-p "this run is not part of a program"
                          (+tt--input-header (+tt-test--input-state "REVIEWING" t))))
  (should-not (string-match-p "whole program"
                              (+tt--input-header (+tt-test--input-state "REVIEWING" t))))
  (should (string-match-p "program-wide owner directive for the whole program"
                          (+tt--input-header nil t))))

(ert-deftest tradeoffs-trace-input-program-wide ()
  "Plan 01i (D5): C-u C-c C-c on a program node's run sends its text as a
program-wide directive; without the prefix it applies to this phase, and a
run that is not part of a program can only apply it to its phase."
  (let ((written nil))
    ;; A node of a program: C-u really is program-wide.
    (cl-letf (((symbol-function '+tt--state) (lambda (_) (+tt-test--input-state "IMPLEMENTING" t nil nil t)))
              ((symbol-function '+tt--write-command) (lambda (_dir cmd) (setq written cmd) "id-1")))
      (with-temp-buffer
        (insert "fix the Stork link")
        (setq +tt--run-dir "/tmp/tt-ert/abcd1234")
        (+tt-input-send)
        (should (equal (alist-get 'scope written) "phase"))
        (insert "fix the Stork link")
        (+tt-input-send '(4))
        (should (equal (alist-get 'scope written) "program"))))
    ;; A hand-started run has nothing program-wide to reach: the input is
    ;; recorded for this phase, and the confirmation says so (never "whole
    ;; program", which the conductor would demote anyway).
    (cl-letf (((symbol-function '+tt--state) (lambda (_) (+tt-test--input-state "IMPLEMENTING" t)))
              ((symbol-function '+tt--write-command) (lambda (_dir cmd) (setq written cmd) "id-2")))
      (with-temp-buffer
        (insert "fix the Stork link")
        (setq +tt--run-dir "/tmp/tt-ert/abcd1234")
        (+tt-input-send '(4))
        (should (equal (alist-get 'scope written) "phase"))))))

(ert-deftest tradeoffs-trace-program-input-writes-a-program-directive ()
  "Plan 01i: the program buffer's input box records a program-wide directive
through the CLI, which appends the event and steers every running node now."
  (let ((called nil)
        (dir (make-temp-file "tt-ert-prog" t)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function '+tt--cli)
                     (lambda (&rest args) (setq called args) "ODP-1")))
            (with-temp-buffer
              (insert "no node may touch the vendor adapters")
              (setq +tt--input-program-dir dir)
              (+tt-input-send)
              (should (equal called (list "program" "directive" dir "no node may touch the vendor adapters"))))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-program-input-withdraws-a-directive ()
  "Plan 01i: the program input box's `withdraw ODP-n' goes through `tt
program withdraw' at once; trailing prose does not turn it into a new ruling,
and an unknown id is refused by that command."
  (let ((called nil)
        (dir (make-temp-file "tt-ert-prog" t)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function '+tt--cli)
                     (lambda (&rest args) (setq called args) "")))
            (with-temp-buffer
              (insert "withdraw ODP-1 because it is stale")
              (setq +tt--input-program-dir dir)
              (+tt-input-send)
              (should (equal called (list "program" "withdraw" dir "ODP-1")))))
          ;; A withdrawal that names no id is refused here, never queued as a
          ;; brand-new program-wide ruling.
          (cl-letf (((symbol-function '+tt--cli)
                     (lambda (&rest args) (setq called args) "")))
            (setq called nil)
            (with-temp-buffer
              (insert "withdraw the Stork exception")
              (setq +tt--input-program-dir dir)
              (should-error (+tt-input-send) :type 'user-error)
              (should-not called)))
          ;; …and so is a phase id, which is not a program-wide ruling.
          (cl-letf (((symbol-function '+tt--cli)
                     (lambda (&rest args) (setq called args) "")))
            (setq called nil)
            (with-temp-buffer
              (insert "withdraw OD-2")
              (setq +tt--input-program-dir dir)
              (should-error (+tt-input-send) :type 'user-error)
              (should-not called))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-program-parse ()
  "Phase 4: a program file lists plan files with their dependencies."
  (let* ((dir (make-temp-file "tt-ert-prog" t))
         (plan-a (expand-file-name "a.org" dir))
         (plan-b (expand-file-name "b.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file plan-a (insert +tt-test--valid-plan))
          (with-temp-file plan-b (insert (replace-regexp-in-string "p1" "q1" +tt-test--valid-plan)))
          (with-temp-buffer
            (insert "#+TITLE: plan 13\n#+TT_PROGRAM: 4\n#+TT_CHECK_MINUTES: 40\n#+TT_SECRETS: VENDOR_KEY OTHER_KEY\n\n* 13a\n  :PROPERTIES:\n  :PLAN: a.org\n  :END:\n* 13c\n  :PROPERTIES:\n  :PLAN: b.org\n  :AFTER: 13a\n  :END:\n")
            (setq buffer-file-name (expand-file-name "program.org" dir) default-directory dir)
            (org-mode)
            (let* ((parsed (+tt-parse-program))
                   (program (plist-get parsed :program))
                   (entries (alist-get 'entries program)))
              (set-buffer-modified-p nil) (setq buffer-file-name nil)
              (should (null (plist-get parsed :errors)))
              (should (= (alist-get 'maxParallel program) 4))
              (should (equal (alist-get 'branches program) "stack"))
              (should (equal (mapcar (lambda (e) (alist-get 'id e)) entries) '("13a" "13c")))
              (should (equal (alist-get 'after (aref entries 1)) ["13a"]))
              (should (equal (alist-get 'title (alist-get 'plan (aref entries 0))) "sum validation"))
              ;; the program's time limits reach every entry's plan
              (should (= (alist-get 'checkMs (alist-get 'deadlines (alist-get 'plan (aref entries 1)))) 2400000))
              ;; ... and so do its declared secrets (names only; plan 14's
              ;; program-level declaration used to reach no entry)
              (should (equal (alist-get 'secrets (alist-get 'plan (aref entries 0))) ["VENDOR_KEY" "OTHER_KEY"]))
              (should (equal (alist-get 'secrets (alist-get 'plan (aref entries 1))) ["VENDOR_KEY" "OTHER_KEY"]))))
          ;; A missing plan file is an error at the entry's line.
          (with-temp-buffer
            (insert "#+TITLE: bad\n#+TT_PROGRAM: 2\n* x\n  :PROPERTIES:\n  :PLAN: missing.org\n  :END:\n")
            (setq buffer-file-name (expand-file-name "bad.org" dir) default-directory dir)
            (org-mode)
            (let ((errors (plist-get (+tt-parse-program) :errors)))
              (set-buffer-modified-p nil) (setq buffer-file-name nil)
              (should (seq-find (lambda (e) (and (= (car e) 3) (string-match-p "missing.org not found" (cdr e)))) errors))))
          ;; A plan with two phases (not a program) becomes one entry: its phases run in order.
          (with-temp-buffer
            (insert (replace-regexp-in-string ":provisional:" "" +tt-test--valid-plan)
                    "  :PROPERTIES:\n  :ID: p2\n  :CHECKS: true\n  :END:\n  Goal: g\n  Acceptance:\n  - a\n")
            (setq buffer-file-name (expand-file-name "multi.org" dir) default-directory dir)
            (org-mode)
            (let ((program (plist-get (+tt-parse-program) :program)))
              (set-buffer-modified-p nil) (setq buffer-file-name nil)
              (should (= (length (alist-get 'entries program)) 1))
              (should (= (length (alist-get 'phases (alist-get 'plan (aref (alist-get 'entries program) 0)))) 2)))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-plan-deadlines ()
  "Per-plan time limits reach the JSON plan in ms; none means no field."
  (let* ((plan (plist-get (+tt-test--parse (concat "#+TT_SH_MINUTES: 15\n#+TT_CHECK_MINUTES: 30\n#+TT_ATTEMPT_MINUTES: 90\n#+TT_GATE_MINUTES: 45\n"
                                                   +tt-test--valid-plan))
                          :plan))
         (d (alist-get 'deadlines plan)))
    (should (= (alist-get 'shCommandMs d) 900000))
    (should (= (alist-get 'checkMs d) 1800000))
    (should (= (alist-get 'probeMs d) 1800000))
    (should (= (alist-get 'workerAttemptMs d) 5400000))
    ;; Plan 01f: the gate's own limit, defaulting to 30 minutes when unwritten.
    (should (= (alist-get 'gateMs d) 2700000))
    (should-not (assq 'deadlines (plist-get (+tt-test--parse +tt-test--valid-plan) :plan)))))

(ert-deftest tradeoffs-trace-plan-gate ()
  "Plan 01f: :GATE: and :GATE_CLEANUP: parse into the phase."
  (let* ((text (replace-regexp-in-string
                ":RESERVED:    public API types; persistence format\n"
                ":RESERVED:    public API types; persistence format\n  :GATE:        deploy/atlas.sh --clean --build\n  :GATE_CLEANUP: docker compose down -v\n"
                +tt-test--valid-plan))
         (plan (plist-get (+tt-test--parse text) :plan))
         (p1 (aref (alist-get 'phases plan) 0))
         (p2 (aref (alist-get 'phases plan) 1)))
    (should (equal (alist-get 'gate p1) "deploy/atlas.sh --clean --build"))
    (should (equal (alist-get 'gateCleanup p1) "docker compose down -v"))
    ;; A phase without the properties carries no gate at all: it never
    ;; enters the GATING stage and accepts exactly as before plan 01f.
    (should-not (assq 'gate p2))
    (should-not (assq 'gateCleanup p2))))

(ert-deftest tradeoffs-trace-plan-references ()
  "A plan's cited documents that exist on disk become its references."
  (let* ((dir (make-temp-file "tt-ert-refs" t))
         (ref (expand-file-name "12_ref_contract.md" dir))
         (plan-file (expand-file-name "12a.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file ref (insert "# contract\n"))
          (with-temp-buffer
            (insert (replace-regexp-in-string
                     "Goal: make sum() reject non-numbers"
                     "Goal: see `12_ref_contract.md` §1 and missing_ref.md; make sum() reject non-numbers"
                     +tt-test--valid-plan))
            (setq buffer-file-name plan-file default-directory dir)
            (org-mode)
            (let ((refs (alist-get 'references (plist-get (+tt-parse-plan) :plan))))
              (set-buffer-modified-p nil) (setq buffer-file-name nil)
              (should (equal refs (vector ref))))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-plan-secrets ()
  "Plan 01a: #+TT_SECRETS becomes the plan's secrets list — names only."
  (let ((plan (plist-get (+tt-test--parse (concat "#+TT_SECRETS: FAKE_KEY  OTHER_KEY,THIRD_KEY\n"
                                                    +tt-test--valid-plan))
                          :plan)))
    (should (equal (alist-get 'secrets plan) ["FAKE_KEY" "OTHER_KEY" "THIRD_KEY"])))
  (should-not (assq 'secrets (plist-get (+tt-test--parse +tt-test--valid-plan) :plan)))
  ;; An empty #+TT_SECRETS declares nothing.
  (should-not (assq 'secrets (plist-get (+tt-test--parse (concat "#+TT_SECRETS:\n" +tt-test--valid-plan)) :plan))))

(ert-deftest tradeoffs-trace-plan-models ()
  "#+TT_MODELS becomes the plan's models map: provider optional, model may
contain a slash, and a role named twice is recorded for `tt lint'."
  (let* ((text (concat "#+TT_MODELS: worker=deepseek/deepseek-v4.1-flash reviewer=vercel-ai-gateway:anthropic/claude-sonnet-5 evaluator=openai:gpt-x panel=bare\n"
                       +tt-test--valid-plan))
         (plan (plist-get (+tt-test--parse text) :plan))
         (models (alist-get 'models plan)))
    ;; no provider when none was written; the whole value is the model,
    ;; including the slash
    (should (equal (alist-get 'model (alist-get 'worker models)) "deepseek/deepseek-v4.1-flash"))
    (should-not (assq 'provider (alist-get 'worker models)))
    ;; provider before the FIRST colon; the rest is the model
    (should (equal (alist-get 'provider (alist-get 'reviewer models)) "vercel-ai-gateway"))
    (should (equal (alist-get 'model (alist-get 'reviewer models)) "anthropic/claude-sonnet-5"))
    (should (equal (alist-get 'model (alist-get 'evaluator models)) "gpt-x"))
    (should (equal (alist-get 'provider (alist-get 'evaluator models)) "openai"))
    (should (equal (alist-get 'model (alist-get 'panel models)) "bare"))
    ;; the keyword's own line, for `tt lint'
    (should (= (alist-get 'modelsLine plan) 1)))
  ;; a plan without the keyword is unchanged: no models field at all
  (should-not (assq 'models (plist-get (+tt-test--parse +tt-test--valid-plan) :plan)))
  ;; a role named twice: the later model wins, and the repetition is recorded
  (let* ((plan (plist-get (+tt-test--parse (concat "#+TT_MODELS: worker=a worker=b\n" +tt-test--valid-plan)) :plan)))
    (should (equal (alist-get 'modelsRepeated plan) ["worker"]))
    (should (equal (alist-get 'model (alist-get 'worker (alist-get 'models plan))) "b"))))

(ert-deftest tradeoffs-trace-program-models ()
  "A program's #+TT_MODELS is the per-role default for every entry; an
entry's own value for a role wins over it."
  (let* ((dir (make-temp-file "tt-ert-prog-models" t))
         (plan-a (expand-file-name "a.org" dir))
         (plan-b (expand-file-name "b.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file plan-a (insert +tt-test--valid-plan))
          (with-temp-file plan-b (insert (concat "#+TT_MODELS: worker=entry-w\n"
                                                 (replace-regexp-in-string "p1" "q1" +tt-test--valid-plan))))
          (with-temp-buffer
            (insert "#+TITLE: pm\n#+TT_PROGRAM: 2\n#+TT_MODELS: worker=prog-w reviewer=prog-r\n\n* 13a\n  :PROPERTIES:\n  :PLAN: a.org\n  :END:\n* 13c\n  :PROPERTIES:\n  :PLAN: b.org\n  :AFTER: 13a\n  :END:\n")
            (setq buffer-file-name (expand-file-name "program.org" dir) default-directory dir)
            (org-mode)
            (let* ((program (plist-get (+tt-parse-program) :program))
                   (entries (alist-get 'entries program))
                   (pa (alist-get 'plan (aref entries 0)))
                   (pb (alist-get 'plan (aref entries 1))))
              (set-buffer-modified-p nil) (setq buffer-file-name nil)
              ;; the program's declaration reaches an entry with none of its own
              (should (equal (alist-get 'model (alist-get 'worker (alist-get 'models pa))) "prog-w"))
              (should (equal (alist-get 'model (alist-get 'reviewer (alist-get 'models pa))) "prog-r"))
              ;; the entry's own value wins; the program fills the other role
              (should (equal (alist-get 'model (alist-get 'worker (alist-get 'models pb))) "entry-w"))
              (should (equal (alist-get 'model (alist-get 'reviewer (alist-get 'models pb))) "prog-r"))
              ;; the program object keeps its own declaration for `tt lint'
              (should (equal (alist-get 'model (alist-get 'worker (alist-get 'models program))) "prog-w"))
              ;; ... and names its own Org file, not the temporary JSON copy
              (should (equal (alist-get 'sourceFile program) (expand-file-name "program.org" dir)))
              ;; which roles came from the program is recorded, so `tt lint'
              ;; checks each declaration exactly once (the entry's own here,
              ;; the program's on the program object)
              (should (equal (alist-get 'modelsFromProgram pa) [worker reviewer]))
              (should (equal (alist-get 'modelsFromProgram pb) [reviewer])))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-plan-model-seats ()
  "#+TT_MODELS names a seat as `reviewer.M' / `panel.2', and
`panel=reviewers' records panelFrom for every panel seat; Pi's thinking
suffix stays in the model because a provider is the text before the first
`:' only when it has no `/` (finding #20)."
  (let* ((text (concat "#+TT_MODELS: worker=deepseek/deepseek-v4.1-flash reviewer.M=vercel-ai-gateway:anthropic/claude-opus-5.5 reviewer.A=deepseek/deepseek-v4.1-flash reviewer.B=vercel-ai-gateway:spacexai/grok-4.6 evaluator=vercel-ai-gateway:anthropic/claude-opus-5.5 panel=reviewers\n"
                       +tt-test--valid-plan))
         (models (alist-get 'models (plist-get (+tt-test--parse text) :plan)))
         (seats (alist-get 'reviewerSeats models)))
    (should (equal (alist-get 'model (alist-get 'worker models)) "deepseek/deepseek-v4.1-flash"))
    (should (equal (alist-get 'panelFrom models) "reviewers"))
    (should-not (assq 'panel models))
    (should (equal (alist-get 'provider (alist-get 'M seats)) "vercel-ai-gateway"))
    (should (equal (alist-get 'model (alist-get 'M seats)) "anthropic/claude-opus-5.5"))
    (should (equal (alist-get 'model (alist-get 'A seats)) "deepseek/deepseek-v4.1-flash"))
    (should (equal (alist-get 'model (alist-get 'B seats)) "spacexai/grok-4.6"))
    (should (equal (alist-get 'model (alist-get 'evaluator models)) "anthropic/claude-opus-5.5")))
  ;; Pi's --model accepts a thinking suffix; a `/` before the first `:' means
  ;; the whole value is the model, and the suffix is kept.
  (let* ((plan (plist-get (+tt-test--parse (concat "#+TT_MODELS: reviewer=openai/gpt-6-sol:high\n" +tt-test--valid-plan)) :plan))
         (reviewer (alist-get 'reviewer (alist-get 'models plan))))
    (should (equal (alist-get 'model reviewer) "openai/gpt-6-sol:high"))
    (should-not (assq 'provider reviewer)))
  (let* ((plan (plist-get (+tt-test--parse (concat "#+TT_MODELS: reviewer=vercel-ai-gateway:openai/gpt-6-sol:high\n" +tt-test--valid-plan)) :plan))
         (reviewer (alist-get 'reviewer (alist-get 'models plan))))
    (should (equal (alist-get 'provider reviewer) "vercel-ai-gateway"))
    (should (equal (alist-get 'model reviewer) "openai/gpt-6-sol:high")))
  ;; an unknown seat and a repeated seat are preserved for `tt lint'
  (let* ((plan (plist-get (+tt-test--parse (concat "#+TT_MODELS: reviewer.X=x panel.4=y reviewer.M=a reviewer.M=b\n" +tt-test--valid-plan)) :plan))
         (models (alist-get 'models plan)))
    (should (equal (alist-get 'model (alist-get 'X (alist-get 'reviewerSeats models))) "x"))
    (should (equal (alist-get 'model (alist-get (intern "4") (alist-get 'panelSeats models))) "y"))
    (should (equal (alist-get 'model (alist-get 'M (alist-get 'reviewerSeats models))) "b"))
    (should (equal (alist-get 'modelsRepeated plan) ["reviewer.M"]))))

(ert-deftest tradeoffs-trace-program-model-seats ()
  "A program's #+TT_MODELS merges seat by seat: the entry's own seat wins,
an unnamed seat still gets the program's, and the inherited keys are
recorded for `tt lint'."
  (let* ((dir (make-temp-file "tt-ert-prog-seats" t))
         (plan-a (expand-file-name "a.org" dir))
         (plan-b (expand-file-name "b.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file plan-a (insert +tt-test--valid-plan))
          (with-temp-file plan-b (insert (concat "#+TT_MODELS: worker=entry-w reviewer.A=entry-a\n"
                                                 (replace-regexp-in-string "p1" "q1" +tt-test--valid-plan))))
          (with-temp-buffer
            (insert "#+TITLE: pm\n#+TT_PROGRAM: 2\n#+TT_MODELS: worker=prog-w reviewer.M=prog-m panel=reviewers\n\n* 13a\n  :PROPERTIES:\n  :PLAN: a.org\n  :END:\n* 13c\n  :PROPERTIES:\n  :PLAN: b.org\n  :AFTER: 13a\n  :END:\n")
            (setq buffer-file-name (expand-file-name "program.org" dir) default-directory dir)
            (org-mode)
            (let* ((program (plist-get (+tt-parse-program) :program))
                   (entries (alist-get 'entries program))
                   (pa (alist-get 'plan (aref entries 0)))
                   (pb (alist-get 'plan (aref entries 1))))
              (set-buffer-modified-p nil) (setq buffer-file-name nil)
              ;; an entry with none of its own inherits the program's seat
              (should (equal (alist-get 'model (alist-get 'M (alist-get 'reviewerSeats (alist-get 'models pa)))) "prog-m"))
              (should (equal (alist-get 'panelFrom (alist-get 'models pa)) "reviewers"))
              (should (equal (alist-get 'model (alist-get 'worker (alist-get 'models pa))) "prog-w"))
              ;; the entry's own worker and reviewer.A win; the program's M stays
              (should (equal (alist-get 'model (alist-get 'worker (alist-get 'models pb))) "entry-w"))
              (should (equal (alist-get 'model (alist-get 'A (alist-get 'reviewerSeats (alist-get 'models pb)))) "entry-a"))
              (should (equal (alist-get 'model (alist-get 'M (alist-get 'reviewerSeats (alist-get 'models pb)))) "prog-m"))
              ;; the inherited keys are recorded so `tt lint' checks each once
              (should (equal (alist-get 'modelsFromProgram pb) [reviewer.M panelFrom]))
              (should (equal (alist-get 'modelsFromProgram pa) [worker reviewer.M panelFrom])))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-trace-never-shows-a-secret-value ()
  "Plan 01a: the trace masks a declared secret's value (read from Emacs's own
environment) wherever a stream file happens to hold one; the name shows."
  (let* ((root (make-temp-file "tt-ert-secret" t))
         (dir (expand-file-name "stream" root)))
    (unwind-protect
        (progn
          (make-directory dir)
          (make-directory (expand-file-name "plan" root))
          (with-temp-file (expand-file-name "plan/v1.json" root)
            (insert (json-encode '((title . "t") (secrets . ["FAKE_KEY"])))))
          (setenv "FAKE_KEY" "tt-fake-4f8a2b1c9d3e")
          (with-temp-file (expand-file-name "worker-1.jsonl" dir)
            (insert "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:03.000Z\",\"event\":{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"I used tt-fake-4f8a2b1c9d3e now.\"}]}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:04.000Z\",\"event\":{\"type\":\"tool_execution_start\",\"toolCallId\":\"t1\",\"toolName\":\"sh\",\"args\":{\"command\":\"curl -H 'Bearer tt-fake-4f8a2b1c9d3e' x\"}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:05.000Z\",\"event\":{\"type\":\"tool_execution_end\",\"toolCallId\":\"t1\",\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"Bearer tt-fake-4f8a2b1c9d3e\"}]}}}\n"))
          (with-temp-buffer
            (+tt-trace-mode)
            (setq +tt--run-dir root)
            (+tt--render-trace)
            (let ((text (buffer-string)))
              (should (string-search "» I used ***FAKE_KEY*** now." text))
              (should (string-search "$ curl -H 'Bearer ***FAKE_KEY***' x ✓" text))
              (should (string-search "· Bearer ***FAKE_KEY***" text))
              (should-not (string-search "tt-fake-4f8a2b1c9d3e" text))))
          ;; Without the declared name (or without the variable set) nothing
          ;; is masked: this is a display guard, not the conductor's redaction.
          (setenv "FAKE_KEY" nil)
          (with-temp-buffer
            (+tt-trace-mode)
            (setq +tt--run-dir root)
            (should (null (+tt--secret-values root)))))
      (setenv "FAKE_KEY" nil)
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-redact-masks-longest-first-and-skips-short-values ()
  "Plan 01a, matching secrets.ts: a value that contains another must be masked
first, and a value shorter than the conductor's own minimum is never masked."
  (let* ((root (make-temp-file "tt-ert-redact" t))
         (plan (expand-file-name "plan" root)))
    (unwind-protect
        (progn
          (make-directory plan)
          (with-temp-file (expand-file-name "v1.json" plan)
            (insert (json-encode '((title . "t") (secrets . ["A_KEY" "AB_KEY" "TT"])))))
          (setenv "A_KEY" "tt-fake")
          (setenv "AB_KEY" "tt-fake-abcd1234")
          (setenv "TT" "1")
          ;; The short value is skipped entirely: masking "1" would rewrite
          ;; every id, count and timestamp the trace renders.
          (let ((secrets (+tt--secret-values root)))
            (should (equal (mapcar #'car secrets) '("A_KEY" "AB_KEY")))
            (should (equal (+tt--redact "1 of 2" secrets) "1 of 2"))
            ;; Longest first: no suffix of AB_KEY's value may survive, and one
            ;; pass masks both.
            (should (equal (+tt--redact "a=tt-fake b=tt-fake-abcd1234" secrets)
                           "a=***A_KEY*** b=***AB_KEY***"))
            (should-not (string-match-p "abcd1234" (+tt--redact "x tt-fake-abcd1234 y" secrets))))
          ;; …and the same holds through the trace renderer, which is what a
          ;; stream file left unredacted by an older runner goes through.
          (make-directory (expand-file-name "stream" root))
          (with-temp-file (expand-file-name "worker-1.jsonl" (expand-file-name "stream" root))
            (insert "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:03.000Z\",\"event\":{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"keep 1 and tt-fake-abcd1234\"}]}}}\n"))
          (with-temp-buffer
            (+tt-trace-mode)
            (setq +tt--run-dir root)
            (+tt--render-trace)
            (let ((text (buffer-string)))
              (should (string-search "keep 1 and ***AB_KEY***" text))
              (should-not (string-search "tt-fake" text)))))
      (setenv "A_KEY" nil)
      (setenv "AB_KEY" nil)
      (setenv "TT" nil)
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-mode-line-wait ()
  "Plan 01b: the mode-line segment names the oldest waiting program node."
  (let ((fixture
         (concat "[{\"id\":\"p1\",\"title\":\"plan 13\",\"waiting\":["
                 "{\"node\":\"13f\",\"since\":\"2026-09-24T04:59:00.000Z\","
                 "\"duration\":\"1h12m\",\"reason\":\"the repair budget ran out\"}]}]")))
    (cl-letf (((symbol-function '+tt--cli) (lambda (&rest _) fixture)))
      (let ((+tt--notify-flash nil))
        (should (equal (+tt--mode-line-wait) " [⚑ 13f waiting 1h12m]"))))
    ;; No program waiting: no segment.
    (cl-letf (((symbol-function '+tt--cli) (lambda (&rest _) "[]")))
      (should (null (+tt--mode-line-wait))))))

(ert-deftest tradeoffs-trace-notification-echo ()
  "Plan 01b: each new notifications.jsonl line is shown in the echo area once."
  (let* ((file (make-temp-file "tt-ert-notify" nil ".jsonl"))
         (messages nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"id\":\"r1\",\"kind\":\"run\",\"title\":\"13f vendor\","
                    "\"node\":\"13f\",\"reason\":\"the repair budget ran out\","
                    "\"at\":\"2026-09-24T04:59:00.000Z\"}\n"))
          (let ((+tt--notifications-file file)
                (+tt--notifications-offset 0)
                (+tt--notify-flash nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (+tt--notifications-poll)
              (should (= 1 (length messages)))
              (should (string-match-p "the repair budget ran out" (car messages)))
              (should (string-match-p "13f" (car messages)))
              ;; The line was consumed: a second poll shows nothing again.
              (+tt--notifications-poll)
              (should (= 1 (length messages)))
              ;; A later append is shown too.
              (with-temp-file file
                (insert "{\"id\":\"r1\",\"kind\":\"run\",\"title\":\"13f vendor\","
                        "\"node\":\"13f\",\"reason\":\"still waiting\","
                        "\"at\":\"2026-09-24T05:59:00.000Z\"}\n"))
              (setq +tt--notifications-offset 0)
              (+tt--notifications-poll)
              (should (= 2 (length messages)))
              (should (string-match-p "still waiting" (car messages))))))
      (delete-file file))))

(ert-deftest tradeoffs-trace-notification-no-replay ()
  "Plan 01b: a line already in the file when Emacs starts is not shown."
  (let* ((file (make-temp-file "tt-ert-notify-old" nil ".jsonl"))
         (messages nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"id\":\"old\",\"kind\":\"run\",\"title\":\"old\","
                    "\"reason\":\"before Emacs started\"}\n"))
          (let ((+tt--notifications-file file)
                (+tt--notifications-offset nil)
                (+tt--notify-flash nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (+tt--notifications-poll)
              (should (null messages)))))
      (delete-file file))))

(ert-deftest tradeoffs-trace-notification-multibyte-offset ()
  "Plan 01b: a multi-byte reason leaves the offset at the file's byte end."
  (let* ((file (make-temp-file "tt-ert-notify-mb" nil ".jsonl"))
         (messages nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"id\":\"r1\",\"kind\":\"run\",\"title\":\"café — 13f\","
                    "\"node\":\"13f\",\"reason\":\"waiting…\"}\n"))
          (let ((+tt--notifications-file file)
                (+tt--notifications-offset 0)
                (+tt--notify-flash nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (+tt--notifications-poll)
              (should (= 1 (length messages)))
              ;; The offset must be a BYTE position at the file's end, or the
              ;; next poll re-reads text inside a line it already showed.
              (should (= +tt--notifications-offset (file-attribute-size (file-attributes file))))
              (+tt--notifications-poll)
              (should (= 1 (length messages))))))
      (delete-file file))))

(ert-deftest tradeoffs-trace-mode-line-wait-without-live-run ()
  "Plan 01b: a waiting node whose run conductor is gone still shows its wait."
  (let ((fixture "[{\"id\":\"p1\",\"waiting\":[{\"node\":\"13f\",\"since\":\"2026-09-24T04:59:00.000Z\",\"duration\":\"1h12m\"}]}]"))
    (cl-letf (((symbol-function '+tt--cli) (lambda (&rest _) fixture))
              ((symbol-function '+tt--runs) (lambda () (list "/nonexistent-tt-run")))
              ((symbol-function '+tt--live-run-p) (lambda (_) nil)))
      (let ((+tt--notify-flash nil))
        (+tt--mode-line-update)
        (should (string-match-p "⚑ 13f waiting 1h12m" +tt--mode-line-string))))))

(defconst +tt-test--amended-state
  '((meta (title . "sum validation"))
    (state (run . "RUN_ACTIVE")
           (phase (runId . "r1") (phaseId . "p1") (phase . "IMPLEMENTING")
                  (attempt (n . 2))
                  (candidate (sha . "7c1e0a4aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
                  (contract (contractVersion (snapshot . 2) (sectionSha256 . "9e2c")))
                  (decisions ((id . "D-p1-C1-amendment") (version . 2) (class . "reserved")
                              (source . "worker")
                              (choice . "the tests pass")
                              (whyItMatters . "the literal wording cannot be met")
                              (alternatives ((option . "it works") (consequence . "no candidate can satisfy it")))
                              (recommendation (choice . "the tests pass") (reason . "satisfiable and still meaningful"))
                              (amendment (id . "AM-p1-C1")
                                         (criterion . "it works")
                                         (proposedWording . "the tests pass")
                                         (why . "the literal wording cannot be met")
                                         (raisedBy . "worker") (status . "applied"))))
                  (ballots)
                  (findings)
                  (ownerRequests)
                  (corrections)))
    (decisionStatuses (D-p1-C1-amendment (status . "passed") (reason . "vote passed")
                                         (amendment (id . "AM-p1-C1")
                                                    (criterion . "it works")
                                                    (proposedWording . "the tests pass")
                                                    (status . "applied"))))
    (view (round . 2) (reviewLine . "M ✓   A ✓   B ✓") (needsYou . 0)))
  "Plan 01g: a `tt state' whose only record is an applied amendment.")

(ert-deftest tradeoffs-trace-amendment-decision-render ()
  "Plan 01g: the decision view shows an amendment as `⚑ AMENDED' with the old
wording → the new one, read from the tally status the conductor emits."
  (with-temp-buffer
    (+tt--render-decisions +tt-test--amended-state)
    (let ((text (buffer-string)))
      (should (string-match-p "\\* ⚑ AMENDED" text))
      (should (string-match-p (regexp-quote "it works → the tests pass") text)))))

(defconst +tt-test--amendment-input-state
  '((conductorAlive . t)
    (program . nil)
    (ownerInputs) (pendingOwnerInputs)
    (state (run . "RUN_ACTIVE")
           (phase (runId . "r1") (phaseId . "p1") (phase . "REVIEWING")
                  (attempt (n . 2))
                  (decisions ((id . "D-am") (class . "reserved")
                              (amendment (id . "AM-p1-C1") (status . "applied")
                                         (criterion . "it works")
                                         (proposedWording . "the tests pass")))))))
  "Plan 01g: a state with one applied amendment, for the input-box tests.")

(ert-deftest tradeoffs-trace-amendment-revert-input ()
  "Plan 01g: text naming an applied amendment id is sent as a correction, and
a near-miss is not."
  (should (equal (+tt--revert-amendment-id +tt-test--amendment-input-state "revert AM-p1-C1") "AM-p1-C1"))
  ;; Only the command form counts: a mention in a steer/note must not revert.
  (should-not (+tt--revert-amendment-id +tt-test--amendment-input-state "AM-p1-C1 still looks wrong"))
  (should-not (+tt--revert-amendment-id +tt-test--amendment-input-state "please revert AM-p1-C1 later"))
  ;; A longer id that merely begins with the same text is not a revert.
  (should-not (+tt--revert-amendment-id +tt-test--amendment-input-state "revert AM-p1-C10"))
  (let ((written nil))
    (cl-letf (((symbol-function '+tt--state) (lambda (_) +tt-test--amendment-input-state))
              ((symbol-function '+tt--write-command) (lambda (_dir cmd) (setq written cmd) "id-1")))
      (with-temp-buffer
        (insert "revert AM-p1-C1 because the owner disagrees")
        (setq +tt--run-dir "/tmp/tt-ert/abcd1234")
        (+tt-input-send)
        (should (equal (alist-get 'type written) "correction")))))
  ;; A mention inside a steer reaches the worker as its natural kind.
  (let ((written nil))
    (cl-letf (((symbol-function '+tt--state) (lambda (_) +tt-test--amendment-input-state))
              ((symbol-function '+tt--write-command) (lambda (_dir cmd) (setq written cmd) "id-2")))
      (with-temp-buffer
        (insert "AM-p1-C1 still looks wrong, also fix the retry loop")
        (setq +tt--run-dir "/tmp/tt-ert/abcd1234")
        (+tt-input-send)
        (should (equal (alist-get 'type written) "note"))))))

(ert-deftest tradeoffs-trace-amendment-reverted-render ()
  "Plan 01g: a reverted amendment's line points back to the restored wording,
so the view never claims the replacement is still in force (A-14)."
  (let* ((a '((id . "AM-p1-C1") (criterion . "it works")
              (proposedWording . "the tests pass") (status . "reverted")))
         (d `((amendment . ,a))))
    (should (equal (+tt--amendment-line d) "  the tests pass → it works\n")))
  (let* ((a '((id . "AM-p1-C1") (criterion . "it works")
              (proposedWording . "the tests pass") (status . "applied")))
         (d `((amendment . ,a))))
    (should (equal (+tt--amendment-line d) "  it works → the tests pass\n"))))

;;; Plan 01h: the live trade-offs panel and the cost meter

(defconst +tt-test--tradeoff-state
  '((meta (title . "sum validation"))
    (conductorAlive . :false)
    (ownerInputs) (pendingOwnerInputs)
    (secrets (declared) (missing) (tooShort))
    (plan (title . "sum") (phases . (((id . "p1") (goal . "g") (acceptance) (ownerChecklist)))))
    (state (run . "RUN_ACTIVE")
           (phase (phaseId . "p1") (phase . "REVIEWING") (attempt (n . 2))
                  (repairRoundsUsed . 1) (repairRoundsGranted . 3)
                  (candidate (sha . "7c1e0a4aaaaaaaa"))
                  (decisions ((id . "D-p1-1") (class . "delegated")
                              (choice . "Batch cancels per tick") (whyItMatters . "w")
                              (alternatives ((option . "a") (consequence . "b")))
                              (recommendation (choice . "a") (reason . "b")))
                             ((id . "D-p1-flag") (class . "reserved")
                              (choice . "Errors are thrown, not returned") (whyItMatters . "w")
                              (alternatives ((option . "a") (consequence . "b")))
                              (recommendation (choice . "a") (reason . "b"))))
                  (ballots)
                  (findings ((id . "F-p1-M-2") (severity . "advisory") (status . "open")
                             (raisedBy . "M")
                             (evidence . "src/sum.js:9 a slow path. It returns NaN to callers.")))
                  (ownerRequests)))
    (decisionStatuses (D-p1-1 (status . "failed") (reason . "M veto"))
                      (D-p1-flag (status . "passed") (flagged . t)))
    (view (elapsed . "1m02s") (round . 2)
          (loop . "▶ REVIEW 4m12s of 15m · M·A·B · deepseek-v4.1-flash")
          (models . "worker=deepseek/deepseek-v4.1-flash reviewer=vercel-ai-gateway:anthropic/claude-opus-5.5")
          (pipeline . "review 12s… (14m48s left)")
          (reviewLine . "M ✗ 1 reject   A ✓   B ✓")
          (verdict . "not accepted: D-1 vetoed by M → repair attempt 2")
          (liveDecisions . 2) (failedDecisions . 1) (flaggedDecisions . 1)
          (openFindings . 1) (boundaryFilesChanged . 0)
          (needsYou . 0)
          (tradeoffs ((kind . "flagged") (recordId . "D-p1-flag")
                      (text . "⚑ flagged: D-flag Errors are thrown, not returned — passed"))
                     ((kind . "veto") (recordId . "D-p1-1")
                      (text . "vetoed by M: D-1 Batch cancels per tick — a lone cancel waits a tick"))
                     ((kind . "advisories") (recordId . "F-p1-M-2")
                      (text . "1 advisories (1 new) — C-c m d")))
          (cost (rounds . 2) (totalMinutes . 106) (ownerWaitMinutes . 34)
                (nextRoundMinutes . 14)
                (text . "2 rounds · 106m total · implement 34m · review 30m · owner wait 34m · next round ≈ 14 min"))
          (rounds)))
  "A plan 01h `tt state': a Trade-offs panel, a cost row and the records they name.")

(defun +tt-test--without-tradeoffs ()
  "The plan 01h fixture as a run from before the stage: no tradeoffs/cost."
  (let ((s (copy-tree +tt-test--tradeoff-state)))
    (setf (alist-get 'tradeoffs (alist-get 'view s)) nil)
    (setf (alist-get 'cost (alist-get 'view s)) nil)
    s))

(ert-deftest tradeoffs-trace-status-tradeoffs-and-cost ()
  "Plan 01h: the status buffer renders the Trade-offs section directly under
 the verdict and the cost row; each trade-off line carries its record for RET."
  (with-temp-buffer
    (+tt--render-status-from +tt-test--tradeoff-state "/tmp/tt-ert/abcd1234")
    (let ((text (buffer-string)))
      (should (string-match-p "Trade-offs (3)" text))
      (should (string-match-p "⚑ flagged: D-flag Errors are thrown, not returned — passed" text))
      (should (string-match-p "vetoed by M: D-1 Batch cancels per tick — a lone cancel waits a tick" text))
      (should (string-match-p "1 advisories (1 new) — C-c m d" text))
      (should (string-match-p "cost .*2 rounds · 106m total · implement 34m · review 30m · owner wait 34m · next round ≈ 14 min" text))
      (should (< (string-match-p "verdict" text) (string-match-p "Trade-offs" text)))))
  ;; RET finds the record on the line, without printing it.
  (let (record)
    (cl-letf (((symbol-function '+tt-decisions) (lambda (&optional r) (setq record r))))
      (with-temp-buffer
        (+tt--render-status-from +tt-test--tradeoff-state "/tmp/tt-ert/abcd1234")
        (goto-char (point-min))
        (search-forward "vetoed by M: D-1")
        (goto-char (match-beginning 0))
        (+tt-open-tradeoff)
        (should (equal record "D-p1-1"))))))

(ert-deftest tradeoffs-trace-status-omits-empty-tradeoffs ()
  "Plan 01h: a run from before the stage (no `tradeoffs'/`cost' in the view)
renders as before; an empty Trade-offs section is omitted too."
  (with-temp-buffer
    (+tt--render-status-from (+tt-test--without-tradeoffs) "/tmp/tt-ert/abcd1234")
    (should-not (string-match-p "Trade-offs" (buffer-string)))
    (should-not (string-match-p "^cost" (buffer-string)))
    (should (string-match-p "verdict" (buffer-string))))
  (with-temp-buffer
    (let ((s (copy-tree +tt-test--tradeoff-state)))
      (setf (alist-get 'tradeoffs (alist-get 'view s)) nil)
      (+tt--render-status-from s "/tmp/tt-ert/abcd1234")
      (should-not (string-match-p "Trade-offs" (buffer-string))))))

(ert-deftest tradeoffs-trace-decision-view-follows-tradeoff-order ()
  "Plan 01h: the decision view keeps the Trade-offs order (flagged before a
vetoed decision here) and folds the advisories under one heading with the
count; each block carries its record so RET can land on it."
  (with-temp-buffer
    (+tt--render-decisions +tt-test--tradeoff-state)
    (let ((text (buffer-string)))
      (should (< (string-match-p "ACCEPTED ⚑ FLAGGED" text)
                 (string-match-p "REJECTED (M veto)" text)))
      (should (string-match-p "\\* Advisories (1) — accepted, not fixed" text))
      (should (string-match-p "\\*\\* ADVISORY src/sum.js:9 a slow path — M" text))
      ;; The advisories are not listed again as blocking findings.
      (should-not (string-match-p "Findings (" text)))
    (goto-char (point-min))
    (should (+tt--goto-record "D-p1-1"))
    (should (looking-at "\\* REJECTED (M veto)"))
    (goto-char (point-min))
    (should (+tt--goto-record "F-p1-M-2"))
    (should (looking-at "\\*\\* ADVISORY"))
    ;; A record the view does not carry is not an error, just not found.
    (goto-char (point-min))
    (should-not (+tt--goto-record "D-missing"))))

(ert-deftest tradeoffs-trace-decision-view-reveals-the-record ()
  "Plan 01h (M-4): `+tt-decisions' folds to level 1 before RET's jump runs,
 so a level-2 advisory must be revealed by the jump instead of staying
hidden under the Advisories heading."
  (with-temp-buffer
    (+tt--render-decisions +tt-test--tradeoff-state)
    (org-mode)
    (org-content 1)
    (goto-char (point-min))
    (should (+tt--goto-record "F-p1-M-2"))
    (should (looking-at "\\*\\* ADVISORY"))
    (should-not (get-char-property (point) 'invisible))))

(defconst +tt-test--directive-tradeoff-state
  '((meta (title . "sum validation"))
    (conductorAlive . :false)
    (ownerInputs) (pendingOwnerInputs)
    (secrets (declared) (missing) (tooShort))
    (state (run . "RUN_ACTIVE")
           (phase (phaseId . "p1") (phase . "REVIEWING") (attempt (n . 2))
                  (candidate (sha . "7c1e0a4aaaaaaaa"))
                  (decisions) (ballots) (findings) (ownerRequests)
                  (ownerDirectives ((id . "OD-1") (seq . 1)
                                    (text . "the 14 exchange-state-machine failures are pre-existing, not yours")
                                    (scope . "phase") (status . "in-force")
                                    (targets "worker" "M")
                                    (deliveries (worker . "delivered"))))))
    (decisionStatuses)
    (view (elapsed . "1m02s") (round . 2) (pipeline . "review 12s")
          (reviewLine . "M ✓   A ✓   B ✓")
          (verdict . "not accepted: the checks kept failing → repair attempt 2")
          (liveDecisions . 0) (failedDecisions . 0) (flaggedDecisions . 0)
          (openFindings . 0) (boundaryFilesChanged . 0) (needsYou . 0)
          (tradeoffs ((kind . "directive") (recordId . "OD-1")
                      (text . "directive OD-1 not yet delivered to M: the 14 exchange-state-machine failures are pre-existing, not yours")))
          (cost (text . "2 rounds · 20m total"))
          (rounds)))
  "Plan 01h: a directive Trade-offs line and the directive record it names.")

(ert-deftest tradeoffs-trace-tradeoff-directive-ret ()
  "Plan 01h (M-3): RET on a directive's Trade-offs line opens the decision
view on that directive's own record, which the view now carries."
  (with-temp-buffer
    (+tt--render-status-from +tt-test--directive-tradeoff-state "/tmp/tt-ert/abcd1234")
    (should (string-match-p "Trade-offs (1)" (buffer-string)))
    (goto-char (point-min))
    (search-forward "not yet delivered")
    (should (equal (get-text-property (match-beginning 0) '+tt-record) "OD-1")))
  (let (record)
    (cl-letf (((symbol-function '+tt-decisions) (lambda (&optional r) (setq record r))))
      (with-temp-buffer
        (+tt--render-status-from +tt-test--directive-tradeoff-state "/tmp/tt-ert/abcd1234")
        (goto-char (point-min))
        (search-forward "not yet delivered")
        (goto-char (match-beginning 0))
        (+tt-open-tradeoff)
        (should (equal record "OD-1")))))
  (with-temp-buffer
    (+tt--render-decisions +tt-test--directive-tradeoff-state)
    (org-mode)
    (org-content 1)
    (should (string-match-p "\\* Owner directives (1)" (buffer-string)))
    (goto-char (point-min))
    (should (+tt--goto-record "OD-1"))
    (should (looking-at "\\*\\* OD-1"))
    (should-not (get-char-property (point) 'invisible))))

(ert-deftest tradeoffs-trace-status-ret-without-a-record-is-inert ()
  "Plan 01h (B-5): RET on a status row that is not a Trade-offs line stays as
inert as it was before the key existed — no error, and nothing opened."
  (let ((called nil))
    (cl-letf (((symbol-function '+tt-decisions) (lambda (&optional _) (setq called t))))
      (with-temp-buffer
        (+tt--render-status-from +tt-test--tradeoff-state "/tmp/tt-ert/abcd1234")
        (goto-char (point-min))
        (search-forward "pipeline")
        (goto-char (match-beginning 0))
        (+tt-open-tradeoff)
        (should-not called)
        ;; …and on a plain row of a run from before this stage.
        (erase-buffer)
        (+tt--render-status-from (+tt-test--without-tradeoffs) "/tmp/tt-ert/abcd1234")
        (goto-char (point-min))
        (+tt-open-tradeoff)
        (should-not called)))))

;;; Plan 03b: the runtime-rendered review buffer

(defconst +tt-test--review-org
  (concat "#+TITLE: tradeoffs-trace review — cebd7fcb-01 · 33c41174\n"
          "#+CONTRACT_VERSION: v1\n"
          "\n"
          ;; Decision briefs: the owner's question is the heading, and the
          ;; original evidence is in the folded body.
          "* Needs you (1)\n"
          "** Should a vendor excluded before a weekend stay excluded when its market reopens?\n"
          "   :PROPERTIES:\n"
          "   :ID: F-M-9\n"
          "   :KIND: brief\n"
          "   :COMMAND: resolve\n"
          "   :OPTIONS: accept_risk,repair\n"
          "   :ACCEPT_OPTION: accept_risk\n"
          "   :REFUSE_OPTION: repair\n"
          "   :QUESTION: Should a vendor excluded before a weekend stay excluded when its market reopens?\n"
          "   :RECORD_VERSION: 1\n"
          "   :CANDIDATE_SHA: C1\n"
          "   :CONTRACT_VERSION: 4\n"
          "   :CONTRACT_SHA256: aaaa\n"
          "   :RUN_ID: r1\n"
          "   :PHASE_ID: p1\n"
          "   :END:\n"
          "   Today: Pyth's NVDA product reopens Sunday 20:00 ET.\n"
          "   Impact: No market stops publishing.\n"
          "   Options:\n"
          "   - Keep as is [option:accept_risk] — rejoins at once Cost: one stale quote\n"
          "   - Hold it out for 10 s [option:repair] (recommended) — rejoins after 10 s Cost: 10 s with one vendor fewer\n"
          "   Recommendation: Hold it out for 10 s — IC §5's re-entry rule\n"
          "*** Evidence\n"
          "   - [1] message: F-M-9 a vendor excluded before the weekend rejoins immediately\n"
          "   - [2] config: calendars.yaml us_equity overnight starts at 20:00\n"
          "\n"
          "* Blockers\n"
          "** B-1 the loop does not terminate\n"
          "  :PROPERTIES:\n"
          "  :ID: B-1\n"
          "  :TYPE: blocker\n"
          "  :STATE: published\n"
          "  :RAISED_BY: M\n"
          "  :IMPORTANCE: high\n"
          "  :VERDICT: none\n"
          "  :MESSAGE_VERSION: 1\n"
          "  :CANDIDATE_SHA: C1\n"
          "  :CONTRACT_VERSION: 1\n"
          "  :CONTRACT_SHA256: aaaa\n"
          "  :RUN_ID: r1\n"
          "  :PHASE_ID: p1\n"
          "  :END:\n"
          "  an empty input leaves the cursor where it started\n"
          "\n"
          "* Trade-offs\n"
          "** T-1 Batch cancels per tick\n"
          "  :PROPERTIES:\n"
          "  :ID: T-1\n"
          "  :TYPE: tradeoff\n"
          "  :STATE: published\n"
          "  :RAISED_BY: worker\n"
          "  :IMPORTANCE: high\n"
          "  :VERDICT: none\n"
          "  :MESSAGE_VERSION: 2\n"
          "  :CANDIDATE_SHA: C2\n"
          "  :CONTRACT_VERSION: 1\n"
          "  :CONTRACT_SHA256: aaaa\n"
          "  :RUN_ID: r1\n"
          "  :PHASE_ID: p1\n"
          "  :END:\n"
          "  fewer lock acquisitions under load\n"
          "\n"
          "* Findings\n"
          "** Minor (1)\n"
          "*** F-1 a slow path\n"
          "  :PROPERTIES:\n"
          "  :ID: F-1\n"
          "  :TYPE: finding\n"
          "  :STATE: raw\n"
          "  :RAISED_BY: A\n"
          "  :IMPORTANCE: low\n"
          "  :VERDICT: none\n"
          "  :MESSAGE_VERSION: 1\n"
          "  :CANDIDATE_SHA: C2\n"
          "  :CONTRACT_VERSION: 1\n"
          "  :CONTRACT_SHA256: aaaa\n"
          "  :RUN_ID: r1\n"
          "  :PHASE_ID: p1\n"
          "  :END:\n"
          "  not frozen yet\n")
  "A fixture `views/review.org' with a decision brief, a blocker, a trade-off and a raw finding.")

(defun +tt-test--review-buffer (dir)
  "Open DIR's review the real way: +tt-review on a fixture run directory.
A-17/OD-2: the buffer must be opened through +tt-review (the mode first,
then the buffer-locals), not by setting the locals after the mode."
  (make-directory (expand-file-name "views/messages" dir) t)
  (with-temp-file (expand-file-name "views/review.org" dir) (insert +tt-test--review-org))
  (dolist (id '("B-1" "T-1" "F-1"))
    (with-temp-file (expand-file-name (concat "views/messages/" id ".org") dir)
      (insert (format "* %s\n\n* Evidence\n  - src/x.ts:1\n" id))))
  (let ((+tt--run-dir dir))
    (+tt-review))
  ;; The shared refresh timer is not part of these unit tests.
  (when (timerp +tt--timer) (cancel-timer +tt--timer) (setq +tt--timer nil))
  (get-file-buffer (expand-file-name "views/review.org" dir)))

(ert-deftest tradeoffs-trace-review-real-buffer ()
  "A-17/OD-2: a buffer opened through +tt-review keeps its run dir, so RET,
A/D and the mtime refresh all work in a real, file-visiting buffer."
  (let ((dir (make-temp-file "tt-ert-review" t))
        (opened nil) (calls nil) (cli 0) (pf 0))
    (unwind-protect
        (let ((buf (+tt-test--review-buffer dir)))
          (with-current-buffer buf
            (should (equal +tt--run-dir dir))
            (should (equal +tt-review--file (expand-file-name "views/review.org" dir)))
            ;; RET opens the message's own file.
            (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f) buf)))
              (goto-char (point-min))
              (search-forward "T-1")
              (goto-char (match-beginning 0))
              (+tt-review-open-message))
            (should (equal opened (expand-file-name "views/messages/T-1.org" dir)))
            ;; A calls tt verdict with the heading's run dir and full binding.
            (cl-letf (((symbol-function '+tt--cli) (lambda (&rest args) (setq calls args) "verdict applied"))
                      ((symbol-function '+tt-review-refresh) (lambda (&optional _) nil)))
              (goto-char (point-min))
              (search-forward "B-1")
              (goto-char (match-beginning 0))
              (+tt-review-accept))
            (should (equal (nth 1 calls) dir))
            (should (equal (member "--candidate-sha" calls)
                           '("--candidate-sha" "C1" "--message-version" "1"
                             "--contract-version" "1" "--contract-sha256" "aaaa"
                             "--run-id" "r1" "--phase-id" "p1")))
            ;; The mtime refresh re-reads the changed file, with no CLI call.
            (with-temp-file (expand-file-name "views/review.org" dir)
              (insert (replace-regexp-in-string "Batch cancels per tick" "A changed title" +tt-test--review-org)))
            (cl-letf (((symbol-function '+tt--cli) (lambda (&rest _) (cl-incf cli) ""))
                      ((symbol-function 'process-file) (lambda (&rest _) (cl-incf pf) 0)))
              (+tt-review-refresh))
            (should (string-search "A changed title" (buffer-string)))
            (should (= cli 0))
            (should (= pf 0))))
      (when (timerp +tt--timer) (cancel-timer +tt--timer) (setq +tt--timer nil))
      (when-let* ((b (get-file-buffer (expand-file-name "views/review.org" dir)))) (kill-buffer b))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-review-buffer-faces-and-ret ()
  "Plan 03b: each type gets its face; RET opens the message's own file."
  (let ((dir (make-temp-file "tt-ert-review" t)))
    (unwind-protect
        (let ((buf (+tt-test--review-buffer dir)))
          (with-current-buffer buf
            (make-directory (expand-file-name "views/messages" dir) t)
            (with-temp-file (expand-file-name "views/messages/T-1.org" dir) (insert "* T-1\n"))
            (goto-char (point-min))
            (search-forward "B-1")
            (should (eq (get-text-property (match-beginning 0) 'face) '+tt-review-blocker-face))
            (goto-char (point-min))
            (search-forward "T-1")
            (should (eq (get-text-property (match-beginning 0) 'face) '+tt-review-tradeoff-face))
            (goto-char (point-min))
            (search-forward "F-1")
            (should (eq (get-text-property (match-beginning 0) 'face) '+tt-review-finding-face))
            ;; RET opens views/messages/<id>.org, not a move within the buffer.
            (let (opened)
              (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f) buf)))
                (goto-char (point-min))
                (search-forward "T-1")
                (goto-char (match-beginning 0))
                (+tt-review-open-message))
              (should (equal opened (expand-file-name "views/messages/T-1.org" dir)))))
          (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-review-verdicts-call-tt-with-the-binding ()
  "Plan 03b: A and D call `tt verdict' with the heading's full binding; D
asks for a one-line reason; a raw message is not yet frozen."
  (let ((dir (make-temp-file "tt-ert-review" t)))
    (unwind-protect
        (let ((buf (+tt-test--review-buffer dir))
              (calls nil))
          (with-current-buffer buf
            (cl-letf (((symbol-function '+tt--cli) (lambda (&rest args) (setq calls args) "queued verdict x"))
                      ((symbol-function '+tt-review-refresh) (lambda (&optional _) nil)))
              ;; A on the blocker: the full binding from the heading.
              (goto-char (point-min))
              (search-forward "B-1")
              (goto-char (match-beginning 0))
              (+tt-review-accept)
              (should (equal (nth 0 calls) "verdict"))
              (should (equal (nth 1 calls) dir))
              (should (equal (nth 2 calls) "B-1"))
              (should (equal (nth 3 calls) "accept"))
              (should (equal (member "--candidate-sha" calls)
                             '("--candidate-sha" "C1" "--message-version" "1"
                               "--contract-version" "1" "--contract-sha256" "aaaa"
                               "--run-id" "r1" "--phase-id" "p1")))
              ;; D on the trade-off asks for the reason and passes it.
              (setq calls nil)
              (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "not the trade-off the goal needed")))
                (goto-char (point-min))
                (search-forward "T-1")
                (goto-char (match-beginning 0))
                (+tt-review-refuse))
              (should (equal (nth 3 calls) "refuse"))
              (should (equal (member "--reason" calls)
                             '("--reason" "not the trade-off the goal needed"
                               "--candidate-sha" "C2" "--message-version" "2"
                               "--contract-version" "1" "--contract-sha256" "aaaa"
                               "--run-id" "r1" "--phase-id" "p1")))
              ;; A raw message cannot be settled: no CLI call, a clear reason.
              (setq calls nil)
              (goto-char (point-min))
              (search-forward "F-1")
              (goto-char (match-beginning 0))
              (let ((err (condition-case e (progn (+tt-review-accept) nil) (user-error e))))
                (should err)
                (should (string-match-p "not yet frozen" (error-message-string err))))
              (should (null calls))))
          (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-review-refresh-costs-only-reads ()
  "Plan 03b: with views present, refreshing the review and status buffers
calls neither `+tt--cli' nor `process-file' — only file reads."
  (let* ((dir (make-temp-file "tt-ert-review" t))
         (cli 0) (pf 0))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/review.org" dir) (insert +tt-test--review-org))
          (with-temp-file (expand-file-name "views/status.txt" dir) (insert "run: r1\nphase: p1 — REVIEWING\npipeline: review 12s…\n"))
          (let ((buf (+tt-test--review-buffer dir)))
            (with-current-buffer buf
              (setq +tt-review--mtime (file-attribute-modification-time
                                       (file-attributes (expand-file-name "views/review.org" dir))))
              (cl-letf (((symbol-function '+tt--cli) (lambda (&rest _) (cl-incf cli) ""))
                        ((symbol-function 'process-file) (lambda (&rest _) (cl-incf pf) 0)))
                (+tt-review-refresh)
                (+tt-review-refresh t)))
            (kill-buffer buf))
          ;; The status buffer reads views/status.txt instead of `tt state'.
          (with-temp-buffer
            (+tt-status-mode)
            (setq +tt--run-dir dir)
            (cl-letf (((symbol-function '+tt--cli) (lambda (&rest _) (cl-incf cli) ""))
                      ((symbol-function 'process-file) (lambda (&rest _) (cl-incf pf) 0)))
              (+tt--render-status))
            (should (string-match-p "pipeline: review 12s" (buffer-string))))
          (should (= cli 0))
          (should (= pf 0)))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-review-fold-layout ()
  "Plan 05c: review.org opens with every message title visible and every body
and drawer folded, whatever `org-startup-folded' the user set; TAB shows a
message's body but never its drawer; a refresh keeps the expanded messages
and point on the same one."
  (dolist (startup '(t showeverything nil))
    (let ((dir (make-temp-file "tt-ert-review" t))
          (org-startup-folded startup))
      (unwind-protect
          (let ((buf (+tt-test--review-buffer dir)))
            (with-current-buffer buf
              ;; Every message title is visible; its body and drawer are folded.
              (dolist (id '("B-1" "T-1" "F-1"))
                (goto-char (point-min))
                (search-forward id)
                (goto-char (match-beginning 0))
                (should (not (get-char-property (line-beginning-position) 'invisible)))
                (should (org-fold-folded-p (line-end-position))))
              ;; TAB on T-1 shows its body, keeps its drawer folded.
              (goto-char (point-min))
              (search-forward "T-1")
              (goto-char (match-beginning 0))
              (+tt-review-toggle)
              (goto-char (point-min))
              (search-forward "fewer lock acquisitions under load")
              (should (not (org-fold-folded-p (line-beginning-position))))
              (goto-char (point-min))
              (search-forward ":ID: T-1")
              (should (org-fold-folded-p (line-beginning-position)))
              ;; TAB again folds the body back (the drawer stays hidden).
              (goto-char (point-min))
              (search-forward "T-1")
              (goto-char (match-beginning 0))
              (+tt-review-toggle)
              (goto-char (point-min))
              (search-forward "fewer lock acquisitions under load")
              (should (org-fold-folded-p (line-beginning-position)))
              ;; Expand once more so the refresh has something to preserve.
              (goto-char (point-min))
              (search-forward "T-1")
              (goto-char (match-beginning 0))
              (+tt-review-toggle)
              ;; Refresh: T-1 stays expanded, B-1 stays folded, point on T-1.
              (with-temp-file (expand-file-name "views/review.org" dir)
                (insert (replace-regexp-in-string "Batch cancels per tick" "Batch cancels per tick now" +tt-test--review-org)))
              (goto-char (point-min))
              (search-forward "T-1")
              (goto-char (match-beginning 0))
              (+tt-review-refresh t)
              (should (string-search "Batch cancels per tick now" (buffer-string)))
              (should (equal (org-entry-get nil "ID") "T-1"))
              (goto-char (point-min))
              (search-forward "fewer lock acquisitions under load")
              (should (not (org-fold-folded-p (line-beginning-position))))
              (goto-char (point-min))
              (search-forward "B-1")
              (goto-char (match-beginning 0))
              (should (org-fold-folded-p (line-end-position))))
            (kill-buffer buf))
        (delete-directory dir t)))))

(ert-deftest tradeoffs-trace-review-brief-evidence-and-status-question ()
  "Decision briefs: the review buffer shows the brief with its body and evidence
folded, TAB unfolds the original evidence, and the status `needs you' line
shows the question, never a finding id."
  (let ((dir (make-temp-file "tt-ert-review" t)))
    (unwind-protect
        (let ((buf (+tt-test--review-buffer dir)))
          (with-current-buffer buf
            ;; The question and the decision's paragraphs are visible; only the
            ;; original evidence is folded (disc-M-172).
            (goto-char (point-min))
            (search-forward "Should a vendor excluded")
            (goto-char (match-beginning 0))
            (should (not (get-char-property (line-beginning-position) 'invisible)))
            (should-not (org-fold-folded-p (line-end-position)))
            (goto-char (point-min))
            (search-forward "Today: Pyth's")
            (should (not (org-fold-folded-p (line-beginning-position))))
            (goto-char (point-min))
            (search-forward "Impact: No market stops publishing")
            (should (not (org-fold-folded-p (line-beginning-position))))
            (goto-char (point-min))
            (search-forward "*** Evidence")
            (goto-char (match-beginning 0))
            (should (org-fold-folded-p (line-end-position)))
            ;; TAB on the brief unfolds the evidence.
            (goto-char (point-min))
            (search-forward "Should a vendor excluded")
            (goto-char (match-beginning 0))
            (+tt-review-toggle)
            (goto-char (point-min))
            (search-forward "message: F-M-9")
            (should (not (org-fold-folded-p (line-beginning-position))))
            (goto-char (point-min))
            (search-forward "config: calendars.yaml")
            (should (not (org-fold-folded-p (line-beginning-position))))
            ;; TAB again folds it back.
            (goto-char (point-min))
            (search-forward "Should a vendor excluded")
            (goto-char (match-beginning 0))
            (+tt-review-toggle)
            (goto-char (point-min))
            (search-forward "message: F-M-9")
            (should (org-fold-folded-p (line-beginning-position))))
          (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-brief-resolve-sends-the-request-command ()
  "Decision briefs: A writes the resolve command with the brief's own accept
option id and full binding, prompting for the scope note accept_risk needs;
RET chooses an option and sends the same encoding."
  (let ((dir (make-temp-file "tt-ert-review" t))
        (writes nil)
        (written nil))
    (unwind-protect
        (let ((buf (+tt-test--review-buffer dir)))
          (with-current-buffer buf
            (cl-letf (((symbol-function '+tt--write-command)
                       (lambda (run-dir command) (push command writes) (setq written (list run-dir command)) "cmd-1"))
                      ((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _)
                         (car (seq-find (lambda (c) (string-match-p "Hold it out" (car c))) collection))))
                      ((symbol-function 'read-string)
                       (lambda (prompt &rest _)
                         (if (string-match-p "Scope note" prompt)
                             "known race, accepted for one release"
                           "the rejoin is still too eager"))))
              (goto-char (point-min))
              (search-forward "Should a vendor excluded")
              (goto-char (match-beginning 0))
              (+tt-review-accept)
              (should (equal (nth 0 written) dir))
              (should (equal (alist-get 'type (nth 1 written)) "resolve"))
              (should (equal (alist-get 'option (nth 1 written)) "accept_risk"))
              (should (equal (alist-get 'note (nth 1 written)) "known race, accepted for one release"))
              (let ((binding (alist-get 'binding (nth 1 written))))
                (should (equal (alist-get 'recordId binding) "F-M-9"))
                (should (equal (alist-get 'candidateSha binding) "C1"))
                (should (equal (alist-get 'recordVersion binding) 1))
                (should (equal (alist-get 'contractVersion binding) '((snapshot . 4) (sectionSha256 . "aaaa")))))
              ;; RET prompts on the PLAIN label (finding B-35) and a refuse
              ;; option asks for the optional reason, which rides both the
              ;; resolve's note and a note command so it reaches the next
              ;; worker (OD-2 / D-B-78).
              (setq writes nil written nil)
              (goto-char (point-min))
              (search-forward "Should a vendor excluded")
              (goto-char (match-beginning 0))
              (+tt-review-open-message)
              (let* ((resolve (seq-find (lambda (c) (equal (alist-get 'type c) "resolve")) writes))
                     (note (seq-find (lambda (c) (equal (alist-get 'type c) "note")) writes)))
                (should (equal (alist-get 'option resolve) "repair"))
                (should (equal (alist-get 'note resolve) "the rejoin is still too eager"))
                (should (equal (alist-get 'recordId (alist-get 'binding resolve)) "F-M-9"))
                (should (equal (alist-get 'text note) "the rejoin is still too eager"))
                (should (equal (alist-get 'phaseId (alist-get 'binding note)) "p1")))
              ;; The collection offered is labels, not ids.
              (should-not (seq-some (lambda (c) (string-match-p "accept_risk" (car c))) (+tt-review--brief-choices))))
          (kill-buffer buf))
      (delete-directory dir t)))))

(defconst +tt-test--brief-override-org
  (concat "#+TITLE: tradeoffs-trace review\n"
          "#+CONTRACT_VERSION: v1\n"
          "\n"
          "* Needs you (1)\n"
          "** Should this choice stand? widen the re-entry band\n"
          "   :PROPERTIES:\n"
          "   :ID: D-A-80\n"
          "   :KIND: brief\n"
          "   :COMMAND: override\n"
          "   :OPTIONS: approve,reject_and_repair\n"
          "   :ACCEPT_OPTION: approve\n"
          "   :REFUSE_OPTION: reject_and_repair\n"
          "   :RECORD_VERSION: 2\n"
          "   :CANDIDATE_SHA: C1\n"
          "   :CONTRACT_VERSION: 4\n"
          "   :CONTRACT_SHA256: aaaa\n"
          "   :RUN_ID: r1\n"
          "   :PHASE_ID: p1\n"
          "   :END:\n"
          "   Impact: Whether any market stops publishing is not established.\n")
  "A fixture review with one override brief (a flagged reserved decision).")

(ert-deftest tradeoffs-trace-brief-override-sends-an-override ()
  "Decision briefs: a flagged reserved decision's brief A/D send an override
(approve/reject) command with the decision's own binding."
  (let ((dir (make-temp-file "tt-ert-review" t))
        (writes nil)
        (written nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/review.org" dir) (insert +tt-test--brief-override-org))
          (with-temp-file (expand-file-name "views/status.txt" dir) (insert "run: r1\n"))
          (let ((+tt--run-dir dir))
            (+tt-review))
          (when (timerp +tt--timer) (cancel-timer +tt--timer) (setq +tt--timer nil))
          (let ((buf (get-file-buffer (expand-file-name "views/review.org" dir))))
            (with-current-buffer buf
              (cl-letf (((symbol-function '+tt--write-command)
                         (lambda (run-dir command) (push command writes) (setq written (list run-dir command)) "cmd-2"))
                        ((symbol-function 'read-string) (lambda (&rest _) "the band stays too wide")))
                (goto-char (point-min))
                (search-forward "Should this choice stand")
                (goto-char (match-beginning 0))
                (+tt-review-accept)
                (should (equal (alist-get 'type (nth 1 written)) "override"))
                (should (equal (alist-get 'vote (nth 1 written)) "approve"))
                (should (equal (alist-get 'recordId (alist-get 'binding (nth 1 written))) "D-A-80"))
                (should (equal (alist-get 'recordVersion (alist-get 'binding (nth 1 written))) 2))
                ;; D (reject) asks for the optional reason and queues it as a
                ;; note so it reaches the next worker (OD-2 / D-B-78).
                (setq writes nil written nil)
                (goto-char (point-min))
                (search-forward "Should this choice stand")
                (goto-char (match-beginning 0))
                (+tt-review-refuse)
                (let* ((override (seq-find (lambda (c) (equal (alist-get 'type c) "override")) writes))
                       (note (seq-find (lambda (c) (equal (alist-get 'type c) "note")) writes)))
                  (should (equal (alist-get 'vote override) "reject"))
                  (should (equal (alist-get 'text note) "the band stays too wide")))))
            (kill-buffer buf)))
      (delete-directory dir t))))

(defconst +tt-test--brief-entry-org
  (concat "#+TITLE: tradeoffs-trace review\n"
          "#+CONTRACT_VERSION: v1\n"
          "\n"
          "* Needs you (1)\n"
          "** Should this stand? a held market makes no offer\n"
          "   :PROPERTIES:\n"
          "   :ID: E-1\n"
          "   :KIND: brief\n"
          "   :COMMAND: entry\n"
          "   :OPTIONS: accept,refuse\n"
          "   :ACCEPT_OPTION: accept\n"
          "   :REFUSE_OPTION: refuse\n"
          "   :END:\n"
          "   Impact: Whether any market stops publishing is not established.\n"
          "   Options:\n"
          "   - Accept it [option:accept] — the entry is settled\n"
          "   - Refuse it [option:refuse] — your reason reaches the worker\n")
  "A fixture review with one live-entry brief.")

(ert-deftest tradeoffs-trace-brief-entry-settles-via-the-entry-command ()
  "Decision briefs: a live entry's brief A/D call the entry accept/refuse
command the review view already uses."
  (let ((dir (make-temp-file "tt-ert-review" t))
        (calls nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/review.org" dir) (insert +tt-test--brief-entry-org))
          (let ((+tt--run-dir dir))
            (+tt-review))
          (when (timerp +tt--timer) (cancel-timer +tt--timer) (setq +tt--timer nil))
          (let ((buf (get-file-buffer (expand-file-name "views/review.org" dir))))
            (with-current-buffer buf
              (cl-letf (((symbol-function '+tt--cli) (lambda (&rest args) (setq calls args) "entry accept applied")))
                (goto-char (point-min))
                (search-forward "Should this stand")
                (goto-char (match-beginning 0))
                (+tt-review-accept)
                (should (equal calls (list "entry" dir "accept" "E-1")))
                (setq calls nil)
                (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "the market is held")))
                  (goto-char (point-min))
                  (search-forward "Should this stand")
                  (goto-char (match-beginning 0))
                  (+tt-review-refuse)
                  (should (equal calls (list "entry" dir "refuse" "E-1" "--reason" "the market is held"))))))
            (kill-buffer buf)))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-status-needs-you-shows-the-question ()
  "Decision briefs: the status `needs you' line shows the brief's question, not
a finding id."
  (let ((s `((state (phase (phaseId . "p1") (phase . "AWAITING_OWNER")
                        (attempt (n . 1)) (repairRoundsUsed . 0) (repairRoundsGranted . 3)))
             (conductorAlive . t)
             (meta (title . "atlas 15d"))
             (view (elapsed . "1m") (round . 1) (pipeline . "repair 1s…")
                   (reviewLine . "M ✓   A ✓   B ✓")
                   (attention . "needs you")
                   (attentionQuestion . "Should a vendor excluded before a weekend stay excluded when its market reopens?")
                   (cost (text . "1m"))))))
    (with-temp-buffer
      (+tt--render-status-from s "/tmp/does-not-matter")
      (should (string-match-p
               "Should a vendor excluded before a weekend stay excluded when its market reopens?"
               (buffer-string)))
      (should (string-match-p "needs you" (buffer-string)))
      (should-not (string-match-p "F-M-9" (buffer-string))))))

(ert-deftest tradeoffs-trace-status-blocked-shows-the-question ()
  "Decision briefs: a BLOCKED phase that has a brief shows its question too,
not only the word BLOCKED (disc-M-185)."
  (let ((s `((state (phase (phaseId . "p1") (phase . "BLOCKED")
                        (blockedReason . "reviewer unavailable")
                        (attempt (n . 1)) (repairRoundsUsed . 0) (repairRoundsGranted . 3)))
             (conductorAlive . t)
             (meta (title . "atlas 15d"))
             (view (elapsed . "1m") (round . 1) (pipeline . "review 1s…")
                   (reviewLine . "M ✓   A ✓   B ✓")
                   (attention . "BLOCKED")
                   (attentionQuestion . "Should a vendor excluded before a weekend stay excluded when its market reopens?")
                   (cost (text . "1m"))))))
    (with-temp-buffer
      (+tt--render-status-from s "/tmp/does-not-matter")
      (should (string-match-p
               "BLOCKED — Should a vendor excluded before a weekend stay excluded when its market reopens?"
               (buffer-string))))))

(defconst +tt-test--entry-review-org
  (concat "#+TITLE: tradeoffs-trace review — cebd7fcb-01 · 33c41174\n"
          "#+CONTRACT_VERSION: v1\n"
          "#+CANDIDATE: C1\n"
          "\n"
          "* Blockers\n"
          "** E-1 no priced-frame counter  [M·worker · +1 linked · src/a.rs:1-5]\n"
          "   :PROPERTIES:\n"
          "   :ID: E-1\n"
          "   :TYPE: blocker\n"
          "   :STATE: open\n"
          "   :ANCHOR: src/a.rs:1-5\n"
          "   :RAISED_BY: M,worker\n"
          "   :LINKED: B-1,T-2\n"
          "   :END:\n"
          "  the counter is never priced\n"
          "  * Linked messages\n"
          "    - B-1 no priced-frame counter — summary of B-1\n"
          "    - T-2 the fix — summary of T-2\n"
          "\n"
          "* Findings\n"
          "(none)\n"
          "\n"
          "* Trade-offs\n"
          "** E-2 tolerances raised on the fill path  [worker · src/b.rs:1-2 · ≈ E-3]\n"
          "   :PROPERTIES:\n"
          "   :ID: E-2\n"
          "   :TYPE: tradeoff\n"
          "   :STATE: open\n"
          "   :ANCHOR: src/b.rs:1-2\n"
          "   :RAISED_BY: worker\n"
          "   :LINKED: T-3\n"
          "   :HINT: E-3\n"
          "   :END:\n"
          "  tolerances raised\n"
          "\n"
          "** E-3 tolerances raised on the fill path again  [worker · src/c.rs:1-2 · ≈ E-2]\n"
          "   :PROPERTIES:\n"
          "   :ID: E-3\n"
          "   :TYPE: tradeoff\n"
          "   :STATE: open\n"
          "   :ANCHOR: src/c.rs:1-2\n"
          "   :RAISED_BY: worker\n"
          "   :LINKED: T-4\n"
          "   :HINT: E-2\n"
          "   :END:\n"
          "  tolerances raised again\n"
          "\n"
          "4 raw → 3 entries · 4 linked · 0 dropped · 0 unaccounted · unexposed 0\n")
  "A fixture `views/review.org' with entry headings and a `≈' hint.")

(defun +tt-test--entry-review-buffer (dir)
  "Open DIR's review with the entry fixture (the real +tt-review path)."
  (make-directory (expand-file-name "views/entries" dir) t)
  (with-temp-file (expand-file-name "views/review.org" dir) (insert +tt-test--entry-review-org))
  (dolist (id '("E-1" "E-2" "E-3"))
    (with-temp-file (expand-file-name (concat "views/entries/" id ".org") dir) (insert (format "* %s\n" id))))
  (let ((+tt--run-dir dir))
    (+tt-review))
  (when (timerp +tt--timer) (cancel-timer +tt--timer) (setq +tt--timer nil))
  (get-file-buffer (expand-file-name "views/review.org" dir)))

(ert-deftest tradeoffs-trace-review-entry-keys ()
  "Plan 05j: RET opens an entry's own file; s sends ENTRY_SPLIT; m sends the
owner's merge; A/D act on the entry."
  (let ((dir (make-temp-file "tt-ert-entry-review" t)) (calls nil) (opened nil))
    (unwind-protect
        (let ((buf (+tt-test--entry-review-buffer dir)))
          (with-current-buffer buf
            (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f) buf)))
              (goto-char (point-min))
              (search-forward "E-1")
              (goto-char (match-beginning 0))
              (+tt-review-open-message))
            (should (equal opened (expand-file-name "views/entries/E-1.org" dir)))
            (cl-letf (((symbol-function '+tt--cli) (lambda (&rest args) (setq calls args) "ok"))
                      ((symbol-function '+tt-review-refresh) (lambda (&optional _) nil)))
              ;; s on the entry splits its first linked message.
              (goto-char (point-min))
              (search-forward "E-1")
              (goto-char (match-beginning 0))
              (+tt-review-split)
              (should (equal calls (list "entry" dir "split" "E-1" "B-1")))
              ;; s on a linked-message bullet splits that message.
              (goto-char (point-min))
              (search-forward "T-2 the fix")
              (goto-char (match-beginning 0))
              (+tt-review-split)
              (should (equal calls (list "entry" dir "split" "E-1" "T-2")))
              ;; m merges into the ≈ neighbour.
              (goto-char (point-min))
              (search-forward "E-2 tolerances")
              (goto-char (match-beginning 0))
              (+tt-review-merge)
              (should (equal calls (list "entry" dir "merge" "E-2" "E-3")))
              ;; A and D act on the entry.
              (goto-char (point-min))
              (search-forward "E-1")
              (goto-char (match-beginning 0))
              (+tt-review-accept)
              (should (equal calls (list "entry" dir "accept" "E-1")))
              (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "not this round")))
                (goto-char (point-min))
                (search-forward "E-1")
                (goto-char (match-beginning 0))
                (+tt-review-refuse))
              (should (equal (nth 2 calls) "refuse"))
              (should (equal (member "--reason" calls) '("--reason" "not this round")))))
          (kill-buffer buf))
      (when (timerp +tt--timer) (cancel-timer +tt--timer) (setq +tt--timer nil))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-program-review-key ()
  "Plan 05j: C-c m D opens programs/<id>/views/review.org."
  (let* ((root (make-temp-file "tt-ert-program-review" t))
         (dir (expand-file-name "programs/abc" root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/review.org" dir) (insert +tt-test--entry-review-org))
          (let ((+tt-root root) (+tt--program-dir dir))
            (+tt-program-review)
            (should (equal (file-truename (buffer-file-name))
                           (file-truename (expand-file-name "views/review.org" dir))))
            (should (equal +tt--run-dir dir))
            (should (derived-mode-p '+tt-review-mode)))
          (when (timerp +tt--timer) (cancel-timer +tt--timer) (setq +tt--timer nil))
          (when-let* ((b (get-file-buffer (expand-file-name "views/review.org" dir)))) (kill-buffer b)))
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-status-file-restores-records ()
  "Plan 03b (A-1/B-6): the rendered status file keeps plan 01h's RET
binding, so a trade-off line still opens the decision view."
  (let ((dir (make-temp-file "tt-ert-status" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/status.txt" dir)
            (insert "sum validation\n"
                    "run r1 · conductor running · 1m\n\n"
                    "Trade-offs (1)\n"
                    "  - vetoed by M: D-1 Batch cancels per tick\t:RECORD:D-1\n"))
          (let (record)
            (cl-letf (((symbol-function '+tt-decisions) (lambda (&optional r) (setq record r))))
              (with-temp-buffer
                (+tt-status-mode)
                (setq +tt--run-dir dir)
                (+tt--render-status)
                (should-not (string-search ":RECORD:" (buffer-string)))
                (should (string-search "Trade-offs (1)" (buffer-string)))
                (goto-char (point-min))
                (search-forward "vetoed by M: D-1")
                (goto-char (match-beginning 0))
                (+tt-open-tradeoff)
                (should (equal record "D-1"))))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-cli-error-includes-stderr ()
  "A-13: a failing `tt' failure message carries stderr as well as stdout."
  (let ((+tt-root (make-temp-file "tt-ert-root" t))
        (+tt-runner (make-temp-file "tt-ert-runner" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" +tt-runner) t)
          (with-temp-file (expand-file-name "src/cli.ts" +tt-runner) (insert "//"))
          (cl-letf (((symbol-function 'process-file)
                     (lambda (_program _in destination _display &rest _args)
                       (insert "queued verdict v-1")
                       ;; Like the real `process-file': stderr goes to a file name.
                       (when (consp destination)
                         (should (stringp (cadr destination)))
                         (with-temp-file (cadr destination) (insert "verdict rejected: message T-1 changed v1 → v2")))
                       1)))
            (let ((err (condition-case e (+tt--cli "verdict" "/tmp/run" "T-1" "accept") (error e))))
              (should err)
              (should (string-match-p "queued verdict v-1" (error-message-string err)))
              (should (string-match-p "verdict rejected: message T-1 changed" (error-message-string err))))))
      (delete-directory +tt-root t)
      (delete-directory +tt-runner t))))

(ert-deftest tradeoffs-trace-review-verdict-stale-echo ()
  "A-13: a rejected verdict's reason reaches the echo area, live or exited."
  (dolist (reason '("verdict rejected: message B-1 changed v1 → v2 since you viewed it"
                    "verdict rejected: message B-1 is bound to candidate C1, but the phase is now at candidate C2"))
    (let ((dir (make-temp-file "tt-ert-review" t))
          (echoed nil))
      (unwind-protect
          (let ((buf (+tt-test--review-buffer dir)))
            (with-current-buffer buf
              (cl-letf (((symbol-function '+tt--cli)
                         (lambda (&rest _) (error "tt verdict failed: %s" reason)))
                        ((symbol-function '+tt-review-refresh) (lambda (&optional _) nil))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args) (push (apply #'format fmt args) echoed))))
                (goto-char (point-min))
                (search-forward "B-1")
                (goto-char (match-beginning 0))
                (+tt-review-accept)))
            (should (seq-find (lambda (m) (string-match-p (regexp-quote reason) m)) echoed)))
        (delete-directory dir t)))))

(ert-deftest tradeoffs-trace-status-file-restores-faces ()
  "A-15: the rendered status file keeps the row faces +tt--render-status-from
used: shadow `previous'/`cost' values, and a DONE `verdict' as success."
  (let ((dir (make-temp-file "tt-ert-status" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/status.txt" dir)
            (insert "sum validation\n"
                    "run r1 · conductor stopped · 8s\n\n"
                    "phase     p1 · DONE · round 1 · attempt 1 · repairs 0/3\n"
                    "previous  round 1 · c1 · not accepted\n"
                    "verdict   accepted and published\n"
                    "cost      1 round · 0m total\n"
                    "base      base fails: 1 test\n"
                    "blocked   no candidate\n"))
          (with-temp-buffer
            (+tt-status-mode)
            (setq +tt--run-dir dir)
            (+tt--render-status)
            (let ((face-at (lambda (needle)
                             (goto-char (point-min))
                             (search-forward needle)
                             (get-text-property (match-beginning 0) 'face))))
              (should (eq (funcall face-at "accepted and published") 'success))
              (should (eq (funcall face-at "round 1 · c1") 'shadow))
              (should (eq (funcall face-at "1 round · 0m total") 'shadow))
              (should (eq (funcall face-at "base fails") 'warning))
              (should (eq (funcall face-at "no candidate") 'error))
              (should (eq (funcall face-at "sum validation") 'bold))
              ;; B-16: a row's continuation wraps under its value, not column 0.
              (goto-char (point-min))
              (search-forward "1 round · 0m total")
              (should (equal (get-text-property (match-beginning 0) 'wrap-prefix) (make-string 10 ?\s)))))) 
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-review-verdict-refuses-a-partial-binding ()
  "A heading without the full binding is refused locally, never settling a
version the owner never saw (the M/B objection to the silent fallback)."
  (let ((dir (make-temp-file "tt-ert-review" t)))
    (unwind-protect
        (let ((buf (+tt-test--review-buffer dir))
              (called nil))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (search-forward "B-1")
              (goto-char (match-beginning 0))
              (org-entry-delete nil "CONTRACT_SHA256"))
            (cl-letf (((symbol-function '+tt--cli) (lambda (&rest _) (setq called t) ""))
                      ((symbol-function '+tt-review-refresh) (lambda (&optional _) nil)))
              (goto-char (point-min))
              (search-forward "B-1")
              (goto-char (match-beginning 0))
              (let ((err (condition-case e (progn (+tt-review-accept) nil) (user-error e))))
                (should err)
                (should (string-match-p "no full binding" (error-message-string err))))
              (should-not called)))
          (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-review-refuse-checks-before-asking ()
  "A-18: D on a raw message says 'not yet frozen' without asking for a reason."
  (let ((dir (make-temp-file "tt-ert-review" t))
        (asked nil) (called nil))
    (unwind-protect
        (let ((buf (+tt-test--review-buffer dir)))
          (with-current-buffer buf
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (setq asked t) "reason"))
                      ((symbol-function '+tt--cli) (lambda (&rest _) (setq called t) ""))
                      ((symbol-function '+tt-review-refresh) (lambda (&optional _) nil)))
              (goto-char (point-min))
              (search-forward "F-1")
              (goto-char (match-beginning 0))
              (let ((err (condition-case e (progn (+tt-review-refuse) nil) (user-error e))))
                (should err)
                (should (string-match-p "not yet frozen" (error-message-string err))))
              (should-not asked)
              (should-not called)))
          (kill-buffer buf))
      (delete-directory dir t))))

(provide 'tradeoffs-trace-test)
;;; tradeoffs-trace-test.el ends here

(ert-deftest tradeoffs-trace-remote-root ()
  "A remote `+tt-root' runs `tt' on that host with host-local paths."
  (let ((calls nil)
        (+tt-root "/ssh:mac:/Users/me/.tradeoffs-trace/")
        (+tt-runner "/ssh:mac:/Users/me/.tradeoffs-trace/runner/current/tradeoffs-trace"))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'process-file)
               (lambda (program _in _out _disp &rest args)
                 (push (list default-directory program args) calls)
                 (insert "ok")
                 0)))
      (should (equal (+tt--cli "state" "/ssh:mac:/Users/me/.tradeoffs-trace/abcd1234") "ok"))
      (pcase-let ((`(,dir ,program ,args) (car calls)))
        ;; It runs on the server: TRAMP's default-directory, the server's paths.
        (should (equal dir "/ssh:mac:/Users/me/.tradeoffs-trace/"))
        (should (equal program +tt-node))
        (should (equal args '("/Users/me/.tradeoffs-trace/runner/current/tradeoffs-trace/src/cli.ts"
                              "state" "/Users/me/.tradeoffs-trace/abcd1234"
                              "--root" "/Users/me/.tradeoffs-trace"))))
      ;; git asks the plan's own host too.
      (setq calls nil)
      (+tt--git "/ssh:mac:/Users/me/work/repo" "rev-parse" "HEAD")
      (should (equal (nth 2 (car calls)) '("-C" "/Users/me/work/repo" "rev-parse" "HEAD"))))))

;;; Plan 03c: the stop/continue keys, the opt-in tab bar and the program header.

(defun +tt-test--program-state (id source)
  "A minimal `tt program state' payload for the program buffer."
  `((id . ,id)
    (sourcePath . ,source)
    (state (nodes))
    (lines)))

(ert-deftest tradeoffs-trace-c-c-m-k-confirms-in-a-program-buffer ()
  "`C-c m k' asks first: `n' cancels (no `tt program stop'), RET stops."
  (should (eq (keymap-lookup nil "C-c m k") '+tt-stop))
  (should (eq (keymap-lookup nil "C-c m c") '+tt-continue))
  (let* ((dir (make-temp-file "tt-ert-prog" t))
         (calls nil)
         (buf (get-buffer-create "*tt-test-prog-k*")))
    (unwind-protect
        (cl-letf (((symbol-function '+tt--cli) (lambda (&rest args) (push args calls) "stopped"))
                  ((symbol-function '+tt--program-state)
                   (lambda (_) (+tt-test--program-state "p1" "/tmp/prog.org"))))
          (with-current-buffer buf
            (+tt-program-mode)
            (setq +tt--program-dir dir)
            ;; `n' cancels the confirmation: nothing is stopped.
            (cl-letf (((symbol-function 'read-key) (lambda () ?n)))
              (+tt-stop))
            (should-not calls)
            ;; RET confirms: `tt program stop <dir>' runs exactly once.
            (cl-letf (((symbol-function 'read-key) (lambda () ?\r)))
              (+tt-stop))
            (should (equal (car calls) (list "program" "stop" dir)))))
      (delete-directory dir t)
      (kill-buffer buf))))

(ert-deftest tradeoffs-trace-c-c-m-k-refuses-elsewhere ()
  "Outside a program or phase buffer both keys say so and call nothing."
  (let ((calls nil))
    (cl-letf (((symbol-function '+tt--cli) (lambda (&rest args) (push args calls) "")))
      (with-temp-buffer
        (should-error (+tt-stop) :type 'user-error)
        (should-error (+tt-continue) :type 'user-error)))
    (should-not calls)))

(ert-deftest tradeoffs-trace-c-c-m-k-only-in-phase-buffers ()
  "A buffer that merely carries `+tt--run-dir' (the read-only review or
decisions view) is not a phase buffer, so both keys refuse (M-8)."
  (let ((calls nil))
    (cl-letf (((symbol-function '+tt--cli) (lambda (&rest args) (push args calls) "")))
      ;; A read-only view is not one of the phase's own buffers.
      (with-temp-buffer
        (setq +tt--run-dir "/tmp/run-x")
        (should-not (+tt--phase-buffer-p))
        (should-error (+tt-stop) :type 'user-error)
        (should-error (+tt-continue) :type 'user-error))
      ;; A run's status buffer is.
      (with-temp-buffer
        (+tt-status-mode)
        (setq +tt--run-dir "/tmp/run-x")
        (should (+tt--phase-buffer-p))))
    (should-not calls)))

(ert-deftest tradeoffs-trace-c-c-m-c-continues-a-program ()
  "`C-c m c' resumes the program whose buffer point is in."
  (let* ((dir (make-temp-file "tt-ert-prog" t))
         (calls nil)
         (buf (get-buffer-create "*tt-test-prog-c*")))
    (unwind-protect
        (cl-letf (((symbol-function '+tt--cli) (lambda (&rest args) (push args calls) "resumed"))
                  ((symbol-function '+tt--program-state)
                   (lambda (_) (+tt-test--program-state "p1" "/tmp/prog.org"))))
          (with-current-buffer buf
            (+tt-program-mode)
            (setq +tt--program-dir dir)
            (+tt-continue)
            (should (equal (car calls) (list "program" "resume" dir)))))
      (delete-directory dir t)
      (kill-buffer buf))))

(ert-deftest tradeoffs-trace-tab-bar-is-opt-in ()
  "With `+tt-use-tab-bar' nil (the default) opening a run creates no tab and
leaves the owner's window layout alone (finding M-21)."
  (should-not +tt-use-tab-bar)
  (let ((tabbed nil)
        (took-frame nil)
        (+tt-use-tab-bar nil)
        (run-dir (make-temp-file "tt-ert-run" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'tab-bar-new-tab) (lambda () (setq tabbed t)))
                  ((symbol-function 'tab-bar--tab-index-by-name) (lambda (&rest _) nil))
                  ((symbol-function 'delete-other-windows) (lambda (&optional _) (setq took-frame t)))
                  ((symbol-function '+tt--refresh-all) (lambda (&rest _) nil))
                  ((symbol-function '+tt--ensure-timer) (lambda () nil)))
          (+tt--workspace run-dir)
          (should-not tabbed)
          (should-not took-frame)
          ;; The run's buffers exist and carry the run's readable id.
          (should (get-buffer (format "*tt-status: %s*" (file-name-nondirectory run-dir)))))
      (dolist (buf (buffer-list)) (when (string-prefix-p "*tt-" (buffer-name buf)) (kill-buffer buf)))
      (delete-directory run-dir t))))

(ert-deftest tradeoffs-trace-program-header-shows-id-and-source ()
  "The program buffer's header carries the program id and the source path."
  (let ((buf (get-buffer-create "*tt-test-prog-header*")))
    (unwind-protect
        (cl-letf (((symbol-function '+tt--program-state)
                   (lambda (_) (+tt-test--program-state "prog0001" "/home/me/orgw/work/atlas/indexps/14_program.org"))))
          (with-current-buffer buf
            (+tt-program-mode)
            (setq +tt--program-dir "/tmp/x")
            (+tt--render-program)
            (should (string-match-p "prog0001" header-line-format))
            (should (string-match-p "14_program\\.org" header-line-format))))
      (kill-buffer buf))))

(ert-deftest tt-cli-runs-a-real-process ()
  "`+tt--cli' itself, not stubbed: stdout is returned, and a failure reports
stdout and stderr.  A buffer as `process-file''s stderr destination broke
every call (`wrong-type-argument stringp'), and every other test stubs it."
  (let* ((root (make-temp-file "tt-root-" t))
         (runner (expand-file-name "runner" root))
         (node (expand-file-name "fake-node" root))
         (+tt-root (file-name-as-directory root))
         (+tt-runner runner)
         (+tt-node node))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" runner) t)
          (write-region "" nil (expand-file-name "src/cli.ts" runner))
          ;; A stand-in for node: `ok' succeeds, anything else fails loudly.
          (write-region "#!/bin/sh\nif [ \"$2\" = ok ]; then echo out; exit 0; fi\necho partial; echo boom >&2; exit 1\n"
                        nil node)
          (set-file-modes node #o755)
          (should (equal (+tt--cli "ok") "out"))
          (let ((err (should-error (+tt--cli "bad"))))
            (should (string-match-p "tt bad failed" (cadr err)))
            (should (string-match-p "partial" (cadr err)))
            (should (string-match-p "boom" (cadr err)))))
      (delete-directory root t))))

;;; Plan 03c: the two runtime charts in the Emacs views.

(defun +tt-test--chart-run (root id &optional readable loop tape)
  "A fake run ID under ROOT, with READABLE in program.json, LOOP its chart and
TAPE its `views/tape.txt'."
  (let ((dir (expand-file-name id root)))
    (make-directory (expand-file-name "views" dir) t)
    (with-temp-file (expand-file-name "meta.json" dir) (insert (format "{\"title\":\"%s\"}" id)))
    (with-temp-file (expand-file-name "events.jsonl" dir) (insert "{}\n"))
    (when readable
      (with-temp-file (expand-file-name "program.json" dir)
        (insert (json-encode `((readableId . ,readable))))))
    (when loop
      (with-temp-file (expand-file-name "views/loop.txt" dir) (insert loop)))
    (when tape
      (with-temp-file (expand-file-name "views/tape.txt" dir) (insert tape)))
    dir))

(defconst +tt-test--loop-tape
  (concat "05g seat models \u00b7 round 2 \u00b7 attempt 2/3 \u00b7 1h04m\n"
          "\n"
          "  \u2713  BASELINE    7m\n"
          "  \u2713  IMPLEMENT   16m\n"
          "  \u2713  FREEZE      2s\n"
          "  \u2713  CHECKS      7m\n"
          "  \u2713  PROBE       1s\n"
          "  \u25b6  REVIEW      4m12s of 15m   M\u00b7A\u00b7B \u00b7 deepseek-v4.1-flash\n"
          "     EVALUATE\n"
          "     RESOLVE\n"
          "     PUBLISH\n"
          "     DONE\n")
  "A small `views/tape.txt' shaped like `renderLoopTape'.")

(defconst +tt-test--loop-chart
  (concat "phase chart\n"
          "current state: IMPLEMENTING   (entered 1x - 5s)\n"
          "\n"
          "phase states:\n"
          "  +---------------+\n"
          "  | READY         |  not entered\n"
          "  +---------------+\n"
          "\n"
          "> +---------------+\n"
          "  | IMPLEMENTING  |  entered 1x - 5s\n"
          "  +---------------+\n"
          "\n"
          "> +---------------+\n"
          "  | RUN_ACTIVE    |  current\n"
          "  +---------------+\n")
  "A small `views/loop.txt' shaped like `renderPhaseChart'.")

(ert-deftest tradeoffs-trace-program-buffer-shows-chart ()
  "Plan 03c: the program buffer begins with `views/program.txt', then the
node list; without the file it shows a one-line notice and the node list."
  (let* ((dir (make-temp-file "tt-ert-prog" t))
         (+tt-root (file-name-as-directory dir))
         (node-line "\u25b6 13a                    running   run-a prog1-01")
         (chart "program prog1 - title\n\n+-------+\n| 13a   |  running - IMPLEMENTING\n+-------+\n")
         (state `((id . "prog1")
                  (sourcePath . "/tmp/prog.org")
                  (state (nodes (13a (runId . "run-a") (readableId . "prog1-01"))))
                  ;; An explicit list of strings: `+tt--render-program' walks it
                  ;; with `dolist', which never iterates a bare string.
                  (lines . ,(list node-line)))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/program.txt" dir) (insert chart))
          (cl-letf (((symbol-function '+tt--program-state) (lambda (_) state)))
            (with-temp-buffer
              (+tt-program-mode)
              (setq +tt--program-dir dir)
              (+tt--render-program)
              (should (string-prefix-p chart (buffer-string)))
              (should (string-match-p "\u25b6 13a" (buffer-string)))
              ;; the node list comes after the chart
              (should (< (string-match-p "program prog1" (buffer-string))
                         (string-match-p "\u25b6 13a" (buffer-string))))
              ;; RET on a node line still opens that node's run
              (let (opened)
                (cl-letf (((symbol-function '+tt--workspace) (lambda (d) (setq opened d))))
                  (goto-char (point-min))
                  (search-forward "\u25b6 13a")
                  (+tt-program-open-node))
                (should (equal opened (expand-file-name "run-a" +tt-root))))))
          ;; Without the file: one-line notice, then the node list.
          (delete-file (expand-file-name "views/program.txt" dir))
          (cl-letf (((symbol-function '+tt--program-state) (lambda (_) state)))
            (with-temp-buffer
              (+tt-program-mode)
              (setq +tt--program-dir dir)
              (+tt--render-program)
              (let ((text (buffer-string)))
                (should (string-match-p "\\`no program chart yet (views/program.txt)\n" text))
                (should (string-match-p "\u25b6 13a" text))))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-program-buffer-keeps-point-on-the-node ()
  "Plan 03c (A-3/B-1): the chart above the node list changes length between
refreshes; point must stay on the node the owner was on, not drift to a
neighbouring line (which would make RET open the wrong run)."
  (let* ((dir (make-temp-file "tt-ert-prog" t))
         (+tt-root (file-name-as-directory dir))
         (state `((id . "prog1")
                  (sourcePath . "/tmp/prog.org")
                  (state (nodes (13a (runId . "run-a") (readableId . "prog1-01"))
                                (13b (runId . "run-b") (readableId . "prog1-02"))))
                  (lines . ,(list "\u25b6 13a running run-a prog1-01"
                                  "\u25b6 13b running run-b prog1-02")))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/program.txt" dir)
            (insert "program prog1 - title\n"))
          (cl-letf (((symbol-function '+tt--program-state) (lambda (_) state)))
            (with-temp-buffer
              (+tt-program-mode)
              (setq +tt--program-dir dir)
              (+tt--render-program)
              (goto-char (point-min))
              (search-forward "13b")
              ;; The runtime appends `run:' and `reason' lines to the chart as
              ;; the program moves, so the node list shifts down.
              (with-temp-file (expand-file-name "views/program.txt" dir)
                (insert "program prog1 - title\n\nrun: r-a\nreason: waiting on you\nmore\n"))
              (+tt--render-program)
              (should (equal (get-text-property (point) '+tt-run-id) "run-b")))))
      (delete-directory dir t))))

(ert-deftest tradeoffs-trace-tape-keys-open-the-tape ()
  "Plan 05h: C-c m g opens `*tt-tape <readable-id>*' read-only, showing
`views/tape.txt', from a status buffer, a review buffer and a program node line."
  (should (eq (keymap-lookup nil "C-c m g") '+tt-tape))
  (let* ((root (make-temp-file "tt-ert-root" t))
         (dir (+tt-test--chart-run root "run-a" "prog1-01" +tt-test--loop-chart +tt-test--loop-tape))
         (+tt-root (file-name-as-directory root))
         (bufs nil))
    (unwind-protect
        (progn
          ;; From a status buffer.
          (with-temp-buffer (+tt-status-mode) (setq +tt--run-dir dir) (+tt-tape))
          (let ((b (get-buffer "*tt-tape prog1-01*")))
            (push b bufs)
            (should b)
            (with-current-buffer b
              (should (string-search "\u25b6  REVIEW" (buffer-string)))
              (should buffer-read-only)))
          ;; From a review buffer: the buffer is the tape file's own bytes.
          (with-temp-buffer (+tt-review-mode) (setq +tt--run-dir dir) (+tt-tape))
          (should (equal (with-current-buffer (get-buffer "*tt-tape prog1-01*") (buffer-string))
                         (with-temp-buffer
                           (insert-file-contents (expand-file-name "views/tape.txt" dir))
                           (buffer-string))))
          ;; From a node line in the program buffer.
          (with-temp-buffer
            (insert "\u25b6 13a running run-a prog1-01\n")
            (put-text-property (point-min) (point-max) '+tt-run-id "run-a")
            (+tt-program-mode)
            (setq +tt--program-dir root +tt--run-dir root)
            (goto-char (point-min))
            (+tt-tape))
          (should (get-buffer "*tt-tape prog1-01*")))
      (when (timerp +tt--timer) (cancel-timer +tt--timer) (setq +tt--timer nil))
      (+tt-tape--stop-timer)
      (dolist (b bufs) (when (buffer-live-p b) (kill-buffer b)))
      (dolist (b (buffer-list)) (when (string-prefix-p "*tt-tape " (buffer-name b)) (kill-buffer b)))
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-tape-refresh-keeps-point-and-faces ()
  "Plan 05h: a changed `views/tape.txt' is re-read, point stays on the same
step, and passed/head/failed rows carry their faces."
  (let* ((root (make-temp-file "tt-ert-root" t))
         (dir (+tt-test--chart-run root "run-a" "prog1-01" +tt-test--loop-chart +tt-test--loop-tape))
         (+tt-root (file-name-as-directory root))
         (buf (get-buffer-create "*tt-tape prog1-01*")))
    (unwind-protect
        (with-current-buffer buf
          (+tt-tape-mode)
          (setq +tt--run-dir dir
                +tt-tape--file (expand-file-name "views/tape.txt" dir)
                +tt-tape--loop-file (expand-file-name "views/loop.txt" dir))
          (+tt-tape-refresh t)
          ;; A passed row is shadowed, the head is highlighted.
          (goto-char (point-min))
          (search-forward "BASELINE")
          (should (eq (get-text-property (match-beginning 0) 'face) '+tt-tape-passed-face))
          (goto-char (point-min))
          (search-forward "REVIEW")
          (should (eq (get-text-property (match-beginning 0) 'face) '+tt-tape-head-face))
          ;; Point on REVIEW survives a refresh that changed the head's mark.
          (goto-char (point-min))
          (search-forward "REVIEW")
          (with-temp-file (expand-file-name "views/tape.txt" dir)
            (insert (replace-regexp-in-string "  \u25b6  REVIEW" "  \u2717  REVIEW" +tt-test--loop-tape)))
          ;; Force a different stored mtime, so the test does not depend on
          ;; the filesystem's timestamp granularity between the two writes.
          (setq +tt-tape--mtime '(0 0))
          (+tt-tape-refresh)
          (should (string-match-p "\u2717  REVIEW" (buffer-string)))
          (should (looking-at "  \u2717  REVIEW"))
          (should (eq (get-text-property (point) 'face) '+tt-tape-failed-face)))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-tape-toggle-view ()
  "Plan 05h: `f' switches the tape buffer to `views/loop.txt' and back."
  (let* ((root (make-temp-file "tt-ert-root" t))
         (dir (+tt-test--chart-run root "run-a" "prog1-01" +tt-test--loop-chart +tt-test--loop-tape))
         (+tt-root (file-name-as-directory root))
         (buf (get-buffer-create "*tt-tape prog1-01*")))
    (unwind-protect
        (with-current-buffer buf
          (+tt-tape-mode)
          (setq +tt--run-dir dir
                +tt-tape--file (expand-file-name "views/tape.txt" dir)
                +tt-tape--loop-file (expand-file-name "views/loop.txt" dir))
          (+tt-tape-refresh t)
          (should (string-match-p "05g seat models" (buffer-string)))
          (+tt-tape-toggle-view)
          (should +tt-tape--show-loop)
          (should (string-match-p "current state: IMPLEMENTING" (buffer-string)))
          (goto-char (point-min))
          (search-forward "| IMPLEMENTING")
          (should (eq (get-text-property (match-beginning 0) 'face) '+tt-chart-current-face))
          (+tt-tape-toggle-view)
          (should-not +tt-tape--show-loop)
          (should (string-match-p "05g seat models" (buffer-string))))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-tape-timer-follows-visibility ()
  "Plan 05h: the tape timer runs at `+tt-tape-refresh-interval' while a tape
buffer is visible and stops when none is."
  (let* ((root (make-temp-file "tt-ert-root" t))
         (dir (+tt-test--chart-run root "run-a" "prog1-01" +tt-test--loop-chart +tt-test--loop-tape))
         (+tt-root (file-name-as-directory root))
         (buf (get-buffer-create "*tt-tape prog1-01*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (+tt-tape-mode)
            (setq +tt--run-dir dir
                  +tt-tape--file (expand-file-name "views/tape.txt" dir)
                  +tt-tape--loop-file (expand-file-name "views/loop.txt" dir)
                  +tt-tape-refresh-interval 5))
          ;; Visible: the timer is started, at the configured interval.
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf (+tt-tape--ensure-timer))
          (should (timerp +tt--tape-timer))
          (should (= (float-time (timer--repeat-delay +tt--tape-timer)) 5.0))
          ;; Not visible: a tick refreshes nothing and stops the timer.
          (set-window-buffer (selected-window) (get-buffer-create "*tt-not-tape*"))
          (+tt-tape--tick)
          (should-not (timerp +tt--tape-timer)))
      (+tt-tape--stop-timer)
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-tape-missing-file-is-one-line ()
  "Plan 05h: a run without `views/tape.txt' opens the tape buffer with a
one-line notice and no error."
  (let* ((root (make-temp-file "tt-ert-root" t))
         (dir (+tt-test--chart-run root "run-a" "prog1-01" +tt-test--loop-chart nil))
         (+tt-root (file-name-as-directory root))
         (buf (get-buffer-create "*tt-tape prog1-01*")))
    (unwind-protect
        (with-current-buffer buf
          (+tt-tape-mode)
          (setq +tt--run-dir dir
                +tt-tape--file (expand-file-name "views/tape.txt" dir)
                +tt-tape--loop-file (expand-file-name "views/loop.txt" dir))
          (should-not (file-exists-p +tt-tape--file))
          (+tt-tape-refresh t)
          (should (string-match-p "\\`no tape yet" (buffer-string)))
          (should (= 1 (how-many "\n" (point-min) (point-max))))
          (should buffer-read-only))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest tradeoffs-trace-status-shows-tape-row-and-models-row ()
  "Plan 05h: the status buffer shows the tape's current row as `loop' (and no
`chart C-c m g' hint), and the `models' row is padded and has no colon."
  (with-temp-buffer
    (+tt--render-status-from +tt-test--tradeoff-state "/tmp/tt-ert/abcd1234")
    (let ((text (buffer-string)))
      (should (string-match-p "^loop      \u25b6 REVIEW 4m12s of 15m" text))
      (should (string-match-p "^models    worker=deepseek" text))
      (should-not (string-match-p "^chart " text))))
  (let ((dir (make-temp-file "tt-ert-status" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "views" dir) t)
          (with-temp-file (expand-file-name "views/status.txt" dir)
            (insert "sum validation\nrun r1 \u00b7 conductor running \u00b7 1m\n\nloop      \u25b6 REVIEW 4m12s of 15m\nphase     p1 \u00b7 REVIEWING\nmodels    worker=deepseek\n"))
          (with-temp-buffer
            (+tt-status-mode)
            (setq +tt--run-dir dir)
            (+tt--render-status)
            (let ((text (buffer-string)))
              (should (string-match-p "^loop      \u25b6 REVIEW 4m12s of 15m" text))
              (should (string-match-p "^models    worker=deepseek" text))
              (should-not (string-match-p "^chart " text)))))
      (delete-directory dir t))))
