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
             (liveDecisions . 4) (failedDecisions . 0) (flaggedDecisions . 2) (openFindings . 1) (boundaryFilesChanged . 2)))
     "/tmp/tt-ert/abcd1234")
    (let ((text (buffer-string)))
      (should (string-match-p "run abcd1234 · conductor running · 1m02s" text))
      (should (string-match-p "pipeline  implement 30s → freeze 1s" text))
      (should (string-match-p "time      reviewer-M: model 80%" text))
      (should (string-match-p "reviews   M ✗ 2 reject · 1 blocking   A ✓   B ⧗" text))
      (should (string-match-p "4 decisions · 2 flagged for you · 1 open findings" text))
      (should (string-match-p "boundary files changed: 2 (reviewers classify)" text))
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
          (setenv "FAKE_KEY" "sk-live-4f8a2b1c9d3e")
          (with-temp-file (expand-file-name "worker-1.jsonl" dir)
            (insert "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:03.000Z\",\"event\":{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"I used sk-live-4f8a2b1c9d3e now.\"}]}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:04.000Z\",\"event\":{\"type\":\"tool_execution_start\",\"toolCallId\":\"t1\",\"toolName\":\"sh\",\"args\":{\"command\":\"curl -H 'Bearer sk-live-4f8a2b1c9d3e' x\"}}}\n"
                    "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:05.000Z\",\"event\":{\"type\":\"tool_execution_end\",\"toolCallId\":\"t1\",\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"Bearer sk-live-4f8a2b1c9d3e\"}]}}}\n"))
          (with-temp-buffer
            (+tt-trace-mode)
            (setq +tt--run-dir root)
            (+tt--render-trace)
            (let ((text (buffer-string)))
              (should (string-search "» I used ***FAKE_KEY*** now." text))
              (should (string-search "$ curl -H 'Bearer ***FAKE_KEY***' x ✓" text))
              (should (string-search "· Bearer ***FAKE_KEY***" text))
              (should-not (string-search "sk-live-4f8a2b1c9d3e" text))))
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
          (setenv "A_KEY" "sk-live")
          (setenv "AB_KEY" "sk-live-abcd1234")
          (setenv "TT" "1")
          ;; The short value is skipped entirely: masking "1" would rewrite
          ;; every id, count and timestamp the trace renders.
          (let ((secrets (+tt--secret-values root)))
            (should (equal (mapcar #'car secrets) '("A_KEY" "AB_KEY")))
            (should (equal (+tt--redact "1 of 2" secrets) "1 of 2"))
            ;; Longest first: no suffix of AB_KEY's value may survive, and one
            ;; pass masks both.
            (should (equal (+tt--redact "a=sk-live b=sk-live-abcd1234" secrets)
                           "a=***A_KEY*** b=***AB_KEY***"))
            (should-not (string-match-p "abcd1234" (+tt--redact "x sk-live-abcd1234 y" secrets))))
          ;; …and the same holds through the trace renderer, which is what a
          ;; stream file left unredacted by an older runner goes through.
          (make-directory (expand-file-name "stream" root))
          (with-temp-file (expand-file-name "worker-1.jsonl" (expand-file-name "stream" root))
            (insert "{\"agentId\":\"worker-1\",\"ts\":\"2026-09-23T06:52:03.000Z\",\"event\":{\"type\":\"message_end\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"keep 1 and sk-live-abcd1234\"}]}}}\n"))
          (with-temp-buffer
            (+tt-trace-mode)
            (setq +tt--run-dir root)
            (+tt--render-trace)
            (let ((text (buffer-string)))
              (should (string-search "keep 1 and ***AB_KEY***" text))
              (should-not (string-search "sk-live" text)))))
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

(provide 'tradeoffs-trace-test)
;;; tradeoffs-trace-test.el ends here
