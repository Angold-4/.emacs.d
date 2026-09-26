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
            (insert "#+TITLE: plan 13\n#+TT_PROGRAM: 4\n#+TT_CHECK_MINUTES: 40\n\n* 13a\n  :PROPERTIES:\n  :PLAN: a.org\n  :END:\n* 13c\n  :PROPERTIES:\n  :PLAN: b.org\n  :AFTER: 13a\n  :END:\n")
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
              (should (= (alist-get 'checkMs (alist-get 'deadlines (alist-get 'plan (aref entries 1)))) 2400000))))
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

(provide 'tradeoffs-trace-test)
;;; tradeoffs-trace-test.el ends here
