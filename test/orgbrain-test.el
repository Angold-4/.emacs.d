;;; orgbrain-test.el --- ERT tests for the OrgBrain client -*- lexical-binding: t -*-

;;; Commentary:
;; Pure logic of `core/init-orgbrain.el': mode cycling, per-mode argument
;; construction, receipt field extraction, the history-to-exchanges
;; abstraction with its project filter, and the project-list fallback.  The
;; fixtures follow real `orgbrain history --json' records from the daemon, so
;; every key path under test is a shipped one.  Values are edited where a test
;; needs them to be — notably `request.entity', which is null on most live
;; records — so treat the paths as observed and the values as constructed.
;;
;; No test contacts the daemon: `+orgbrain-transport' is bound to a stub.
;; The window layout IS covered: batch Emacs has one frame and `split-window'
;; works there, which is enough to pin the split's shape and its idempotence.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'init-orgbrain)

;; persp-mode's own `defcustom' makes this globally special; declare it with a
;; value here so the module's dynamic `let' actually shadows the test's.  A
;; valueless `defvar' marks a variable special only inside its own file.
(defvar persp-switch-to-added-buffer t)

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defconst orgbrain-test--ask-json "
{
  \"id\": \"3aa8dc1a556c48f3b0ed4620c35bf0fa\",
  \"kind\": \"ask\",
  \"state\": \"succeeded\",
  \"request\": { \"budget_tokens\": 2000,
                 \"entity\": \"projects/orgbrain\",
                 \"text\": \"what do we know about the retrieval pipeline?\" },
  \"result\": {
    \"answer\": \"The ledger does not contain information about a retrieval pipeline.\",
    \"answer_receipt\": {
      \"citations\": [],
      \"evidence\": { \"fact_ids\": [], \"slugs\": [\"projects/orgbrain\"] },
      \"failure_class\": null,
      \"gaps\": [\"No ledger entries describe retrieval pipeline architecture.\"],
      \"gbrain_calls\": { \"gbrain_ms\": 5437.31, \"kernel\": 6, \"total\": 6, \"worker\": 0 },
      \"grounding\": \"absent\",
      \"latency_ms\": 22739,
      \"model_revision\": \"qwen3.8-27b-local\",
      \"utilisation\": { \"cited_content_entries\": 0,
                        \"content_entries\": 1,
                        \"ledger_entries\": 2,
                        \"utilisation\": 0.0 }
    },
    \"citations\": [],
    \"gaps\": [\"No ledger entries describe retrieval pipeline architecture.\"]
  },
  \"error\": null,
  \"created_at\": \"2026-08-23T16:50:02.814+00:00\",
  \"updated_at\": \"2026-08-23T16:50:25.607+00:00\"
}"
  "An `ask' job record trimmed to the fields the client renders.
The key paths are those of a real record; `request.entity' is set here
because the scoped path is what needs covering, while the live record
this was taken from carries null.")

(defconst orgbrain-test--recall-json "
{
  \"id\": \"1b17591235d049b195a173a116364bd8\",
  \"kind\": \"recall\",
  \"state\": \"succeeded\",
  \"request\": { \"budget_tokens\": 2000, \"entity\": null, \"query\": \"Wrappers\" },
  \"result\": {
    \"answer_receipt\": { \"failure_class\": null,
                         \"latency_ms\": 758,
                         \"model_revision\": \"qwen3.8-27b-local\" },
    \"memory\": {
      \"budget_tokens\": 2000,
      \"budget_used\": 1256,
      \"facts\": [ { \"entity_slug\": \"projects/wrappers\",
                    \"fact\": \"Wrappers uses PostgreSQL for the event log\",
                    \"fact_id\": \"59\" } ],
      \"search_degraded\": null,
      \"total\": 1
    }
  },
  \"error\": null,
  \"created_at\": \"2026-08-23T15:58:27.059+00:00\",
  \"updated_at\": \"2026-08-23T15:58:28.000+00:00\"
}"
  "One real `recall' job record; a recall receipt carries memory, not an answer.")

(defconst orgbrain-test--remember-json "
{
  \"id\": \"aa0000000000000000000000000000ff\",
  \"kind\": \"remember\",
  \"state\": \"succeeded\",
  \"request\": { \"entity\": \"projects/wrappers\",
                 \"fact\": \"Wrappers chose Groth16 over STARKs\",
                 \"kind\": \"fact\",
                 \"visibility\": \"world\" },
  \"result\": { \"answer\": \"Noted.\", \"answer_receipt\": { \"latency_ms\": 1200 } },
  \"error\": null,
  \"created_at\": \"2026-08-22T12:11:49.288+00:00\",
  \"updated_at\": \"2026-08-22T12:11:50.000+00:00\"
}"
  "One legacy `remember' job record, whose sent text lives in `request.fact'.")

(defconst orgbrain-test--failed-json "
{
  \"id\": \"bb0000000000000000000000000000ee\",
  \"kind\": \"ask\",
  \"state\": \"failed\",
  \"request\": { \"entity\": \"projects/orgbrain\", \"text\": \"why?\" },
  \"result\": { \"answer_receipt\": { \"failure_class\": \"compose_timeout\" } },
  \"error\": \"compose stage timed out\",
  \"created_at\": \"2026-08-24T09:00:00.000+00:00\",
  \"updated_at\": \"2026-08-24T09:00:30.000+00:00\"
}"
  "A failed job record: `state' is `failed' and `error' carries the reason.")

(defun orgbrain-test--record (json)
  "Return the parsed job record in JSON."
  (or (+orgbrain--parse-json json)
      (error "Fixture is not valid JSON")))

(defun orgbrain-test--history-json ()
  "Return a `history --json' array, newest first as the CLI returns it."
  (concat "[" orgbrain-test--ask-json "," orgbrain-test--recall-json ","
          orgbrain-test--remember-json "," orgbrain-test--failed-json "]"))

(defun orgbrain-test--transport (responses)
  "Return a transport stub answering from RESPONSES.
RESPONSES maps the first CLI argument to either a stdout string or a
`(exit . stderr)' cons describing a failure.  Every call is recorded in
`orgbrain-test--calls'."
  (lambda (args stdin callback)
    (push (list :args args :stdin stdin) orgbrain-test--calls)
    (let* ((answer (cdr (assoc (car args) responses)))
           (result (if (consp answer)
                       (list :exit (car answer) :stdout "" :stderr (cdr answer))
                     (list :exit 0 :stdout (or answer "") :stderr ""))))
      (if callback (funcall callback result) result))))

(defvar orgbrain-test--calls nil
  "Transport calls recorded by `orgbrain-test--transport', newest first.")

(defmacro orgbrain-test--with-transport (responses &rest body)
  "Evaluate BODY with the transport stubbed from RESPONSES."
  (declare (indent 1))
  `(let ((orgbrain-test--calls nil)
         (+orgbrain-transport (orgbrain-test--transport ,responses))
         (+orgbrain-history-limit 4)
         (+orgbrain-default-projects '("orgbrain")))
     ,@body))

;; ---------------------------------------------------------------------------
;; R2 — mode cycling
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-mode-cycle-wraps-in-order ()
  "TAB walks `ask' -> `remember' -> `recall' and back to `ask'."
  (should (eq (+orgbrain--next-mode 'ask) 'remember))
  (should (eq (+orgbrain--next-mode 'remember) 'recall))
  (should (eq (+orgbrain--next-mode 'recall) 'consult))
  (should (eq (+orgbrain--next-mode 'consult) 'ask)))

(ert-deftest orgbrain-mode-cycle-follows-the-mode-table ()
  "A mode added to `+orgbrain-modes' joins the cycle with no other change.

Position-independent on purpose.  This used to name `recall' as the entry
the new mode follows, so adding `consult' broke a test that was not about
`consult'.  `prompt' and `digest' are still coming (orgbrain#58), and the
property under test -- appending an entry is the whole change -- does not
depend on how long the table already is."
  (let* ((last-existing (car (last (mapcar #'car +orgbrain-modes))))
         (+orgbrain-modes (append +orgbrain-modes
                                  '((digest :label "digest"
                                            :builder +orgbrain--build-ask
                                            :hint "test entry")))))
    (should (eq (+orgbrain--next-mode last-existing) 'digest))
    (should (eq (+orgbrain--next-mode 'digest) 'ask))
    (should (equal (+orgbrain--mode-label 'digest) "digest"))))

(ert-deftest orgbrain-unknown-mode-signals ()
  "An unknown mode is an internal contract violation, not a user error."
  (should-error (+orgbrain--mode-plist 'nonexistent)))

;; ---------------------------------------------------------------------------
;; R2 — argument construction
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-ask-sends-text-on-stdin-scoped-to-the-project ()
  "`ask' pipes the brief and scopes it with `--entity projects/<slug>'."
  (let ((request (+orgbrain--build-request 'ask "long brief" "orgbrain")))
    (should (equal (plist-get request :args)
                   '("ask" "-" "--json" "--entity" "projects/orgbrain")))
    (should (equal (plist-get request :stdin) "long brief"))))

(ert-deftest orgbrain-ask-without-a-project-is-unscoped ()
  "A nil project omits `--entity' rather than sending an empty entity."
  (should (equal (plist-get (+orgbrain--build-request 'ask "brief" nil) :args)
                 '("ask" "-" "--json")))
  (should (equal (plist-get (+orgbrain--build-request 'ask "brief" "") :args)
                 '("ask" "-" "--json"))))

(ert-deftest orgbrain-remember-goes-through-ask ()
  "`remember' is an `ask' with a prefix; the `remember' verb is never called."
  (let* ((request (+orgbrain--build-request 'remember "we chose Groth16" "wrappers"))
         (args (plist-get request :args)))
    (should (equal args '("ask" "-" "--json" "--entity" "projects/wrappers")))
    (should (equal (car args) "ask"))
    (should-not (member "remember" args))
    (should (equal (plist-get request :stdin)
                   "remember that we chose Groth16"))))

(ert-deftest orgbrain-remember-matches-ask-except-for-the-prefix ()
  "The remember builder differs from ask only in the stdin prefix."
  (let ((remember (+orgbrain--build-request 'remember "x" "orgbrain"))
        (ask (+orgbrain--build-request 'ask "remember that x" "orgbrain")))
    (should (equal (plist-get remember :args) (plist-get ask :args)))
    (should (equal (plist-get remember :stdin) (plist-get ask :stdin)))))

(ert-deftest orgbrain-recall-passes-the-query-positionally ()
  "`recall' takes the query as an argument and sends no stdin."
  (let ((request (+orgbrain--build-request 'recall "Wrappers" "orgbrain")))
    (should (equal (plist-get request :args) '("recall" "Wrappers" "--json")))
    (should-not (plist-get request :stdin))))

;; ---------------------------------------------------------------------------
;; Receipt extraction and rendering
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-receipt-fields-come-from-the-real-key-paths ()
  "The rendered receipt reads the shipped paths, including `utilisation'."
  (let* ((record (orgbrain-test--record orgbrain-test--ask-json))
         (result (+orgbrain--dig record "result"))
         (receipt (+orgbrain--dig result "answer_receipt")))
    (should (equal (+orgbrain--dig receipt "grounding") "absent"))
    (should (equal (+orgbrain--dig receipt "latency_ms") 22739))
    (should (equal (+orgbrain--dig receipt "model_revision") "qwen3.8-27b-local"))
    (should (equal (+orgbrain--dig receipt "utilisation" "content_entries") 1))
    (should (equal (+orgbrain--dig receipt "gbrain_calls" "kernel") 6))
    ;; `content_entries' is NOT a top-level receipt key.
    (should-not (+orgbrain--dig receipt "content_entries"))
    (let ((text (+orgbrain--format-receipt result)))
      (should (string-match-p "grounding:       absent" text))
      (should (string-match-p "citations:       none" text))
      (should (string-match-p "content_entries: 1" text))
      (should (string-match-p "latency_ms:      22739" text))
      (should (string-match-p "model_revision:  qwen3.8-27b-local" text))
      (should (string-match-p "kernel 6" text)))))

(ert-deftest orgbrain-ask-body-shows-answer-and-gaps ()
  "An ask renders the answer plus `result.gaps'."
  (let ((text (+orgbrain--format-body
               (orgbrain-test--record orgbrain-test--ask-json))))
    (should (string-match-p "does not contain information" text))
    (should (string-match-p "-- gaps --" text))
    (should (string-match-p "retrieval pipeline architecture" text))))

(ert-deftest orgbrain-recall-body-shows-memory-instead-of-an-answer ()
  "A recall receipt has no answer, so memory totals and facts are rendered."
  (let ((text (+orgbrain--format-body
               (orgbrain-test--record orgbrain-test--recall-json))))
    (should (string-match-p "-- memory --" text))
    (should (string-match-p "total: 1" text))
    (should (string-match-p "budget_used: 1256" text))
    (should (string-match-p "search_degraded: none" text))
    (should (string-match-p "projects/wrappers" text))
    (should (string-match-p "PostgreSQL for the event log" text))))

(ert-deftest orgbrain-failed-state-is-rendered-with-its-error ()
  "A failed job shows its state and error rather than an empty answer."
  (let ((text (+orgbrain--format-body
               (orgbrain-test--record orgbrain-test--failed-json))))
    (should (string-match-p "state: failed" text))
    (should (string-match-p "compose stage timed out" text))
    (should (string-match-p "failure_class:   compose_timeout" text))))

(ert-deftest orgbrain-exchange-heading-quotes-the-sent-text ()
  "The transcript block names the mode and quotes what was sent."
  (let* ((exchange (+orgbrain--record-exchange
                    (orgbrain-test--record orgbrain-test--ask-json)))
         (text (+orgbrain--format-exchange exchange "ask")))
    (should (string-prefix-p "=== ask  projects/orgbrain" text))
    (should (string-match-p "^> what do we know" text))))

;; ---------------------------------------------------------------------------
;; JSON degradation
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-unparseable-json-returns-nil ()
  "Garbage output degrades to nil instead of signalling."
  (should-not (+orgbrain--parse-json "ssh: connect to host vienna port 2224"))
  (should-not (+orgbrain--parse-json ""))
  (should-not (+orgbrain--parse-json nil)))

(ert-deftest orgbrain-json-null-and-false-are-not-values ()
  "JSON null and false never leak into the UI as symbols."
  (should-not (+orgbrain--truthy :null))
  (should-not (+orgbrain--truthy :false))
  (should (equal (+orgbrain--truthy "absent") "absent")))

(ert-deftest orgbrain-transport-failure-signals-a-user-error ()
  "A non-zero exit becomes a `user-error' carrying the stderr diagnostic."
  (orgbrain-test--with-transport '(("history" . (255 . "ssh: Could not resolve hostname")))
    (let ((problem (should-error (+orgbrain--cli '("history" "--json"))
                                 :type 'user-error)))
      (should (string-match-p "Could not resolve hostname"
                              (error-message-string problem))))))

;; ---------------------------------------------------------------------------
;; R2b — consult mode

(ert-deftest orgbrain-consult-mode-forces-one-consult-on-the-ask-path ()
  "`consult' is `ask --consult': same verb, same stdin, one extra flag.
The kernel owns the trigger (orgbrain#69); the client only says the owner
asked for it, which is trigger 1 of three and the only one a client can
legitimately raise."
  (let ((built (+orgbrain--build-request 'consult "why is this slow" "orgbrain")))
    (should (equal (plist-get built :args)
                   '("ask" "-" "--json" "--consult" "--entity" "projects/orgbrain")))
    (should (equal (plist-get built :stdin) "why is this slow"))))

(ert-deftest orgbrain-consult-mode-is-unscoped-cleanly ()
  "No project means no --entity, and --consult still stands alone."
  (should (equal (plist-get (+orgbrain--build-request 'consult "q" nil) :args)
                 '("ask" "-" "--json" "--consult"))))

(ert-deftest orgbrain-consult-is-the-last-mode-in-the-cycle ()
  "TAB reaches the expensive mode only after the cheap ones.
A consulted ask is ~5 min and its prompt leaves the host, so it must not
sit between `ask' and `remember' where a stray TAB lands on it."
  (should (eq (car (last (mapcar #'car +orgbrain-modes))) 'consult)))

;; Elapsed clock

(ert-deftest orgbrain-header-shows-elapsed-while-a-request-is-in-flight ()
  "A five-minute send needs to look different from a stuck one."
  (let ((+orgbrain--project "orgbrain")
        (+orgbrain--mode 'consult)
        (+orgbrain--status 'working)
        (+orgbrain-ssh-host "vienna")
        (+orgbrain--projects-source 'server)
        (+orgbrain--pending (list :mode 'consult :started (- (float-time) 137))))
    (should (string-match-p "vienna working 13[0-9]s" (+orgbrain--header-line 'input)))))

(ert-deftest orgbrain-header-has-no-clock-when-idle ()
  (let ((+orgbrain--project "orgbrain")
        (+orgbrain--mode 'ask)
        (+orgbrain--status 'idle)
        (+orgbrain-ssh-host "vienna")
        (+orgbrain--projects-source 'server)
        (+orgbrain--pending nil))
    (should (string-match-p "vienna idle" (+orgbrain--header-line 'input)))
    (should-not (string-match-p "[0-9]s" (+orgbrain--header-line 'input)))))

(ert-deftest orgbrain-tick-timer-cancels-itself-when-nothing-is-pending ()
  "A lost callback must not leave a timer running for the session."
  (let ((+orgbrain--pending nil)
        (+orgbrain--tick-timer nil))
    (+orgbrain--start-tick)
    (should (timerp +orgbrain--tick-timer))
    ;; The timer body cancels itself on the first fire with nothing pending.
    (funcall (timer--function +orgbrain--tick-timer))
    (should-not +orgbrain--tick-timer)))

;; Consult provenance in the transcript
;;
;; Fixture is the real receipt of job 80d2797 on Vienna: an `ask --consult'
;; that cited one brain fact and one consult take.

(defvar orgbrain-test--consulted-receipt
  "{\"answer\": \"LayerZero Labs is the company behind LayerZero.\",
     \"citations\": [13, 17],
     \"evidence_ledger\": {\"entries\": [
        {\"n\": 13, \"origin\": \"kernel\",  \"text_source\": \"fact\", \"text\": \"Angold works on LayerZero Labs\"},
        {\"n\": 17, \"origin\": \"consult\", \"text_source\": \"take\", \"holder\": \"cursor-grok-4.6-high\", \"text\": \"omnichain messaging\"}]},
     \"answer_receipt\": {
        \"grounding\": \"grounded\", \"latency_ms\": 125170,
        \"model_revision\": \"qwen3.8-27b-local\", \"failure_class\": null,
        \"utilisation\": {\"content_entries\": 16, \"consult_entries\": 1},
        \"consults\": {\"requested\": 1, \"ran\": 1, \"refused\": 0, \"failed\": 0,
                       \"trigger\": \"owner_requested\",
                       \"persist_error\": \"No brain directory configured.\"}}}"
  "A real consulted receipt, trimmed to the fields the renderer reads.")

(ert-deftest orgbrain-receipt-reports-that-a-consult-ran ()
  "Without this the buffer gives no sign the consult fired at all."
  (let ((text (+orgbrain--format-receipt
               (+orgbrain--read-json orgbrain-test--consulted-receipt))))
    (should (string-match-p "consults:.*ran 1" text))
    (should (string-match-p "trigger owner_requested" text))))

(ert-deftest orgbrain-receipt-names-which-citation-came-from-off-the-host ()
  "A composite answer must say which half was not the brain's."
  (let ((text (+orgbrain--format-receipt
               (+orgbrain--read-json orgbrain-test--consulted-receipt))))
    (should (string-match-p "citations:.*(13 17)" text))
    (should (string-match-p "17 from consult" text))))

(ert-deftest orgbrain-receipt-warns-when-a-take-did-not-persist ()
  "A take that never reached GBrain lives for one job; silence would hide that."
  (let ((text (+orgbrain--format-receipt
               (+orgbrain--read-json orgbrain-test--consulted-receipt))))
    (should (string-match-p "NOT PERSISTED" text))))

(ert-deftest orgbrain-receipt-omits-the-consult-block-on-an-ordinary-ask ()
  "All-zero consult counters are noise on the 99% of asks that never consult."
  (let* ((json (+orgbrain--read-json orgbrain-test--consulted-receipt))
         (receipt (+orgbrain--dig json "answer_receipt")))
    (setcdr (assoc "consults" receipt)
            '(("requested" . 0) ("ran" . 0) ("refused" . 0) ("failed" . 0)))
    (let ((text (+orgbrain--format-receipt json)))
      (should-not (string-match-p "consults:" text))
      ;; the rest of the receipt still renders
      (should (string-match-p "grounding:       grounded" text)))))

;; Project creation

(ert-deftest orgbrain-slug-rule-is-checked-before-the-round-trip ()
  "The daemon refuses `projects/a/b'; catching it here costs no call."
  (should-error (+orgbrain--check-slug "a/b") :type 'user-error)
  (should-error (+orgbrain--check-slug "") :type 'user-error)
  (should-error (+orgbrain--check-slug "  ") :type 'user-error)
  (should-error (+orgbrain--check-slug " atlas") :type 'user-error)
  (should (equal (+orgbrain--check-slug "atlas") "atlas")))

(ert-deftest orgbrain-new-project-builds-the-cli-call-and-selects-on-success ()
  "Scope moves only after the daemon confirms, and the caches are dropped."
  (let* ((sent nil)
         (+orgbrain--project "orgbrain")
         (+orgbrain--pending nil)
         (+orgbrain--exchanges '(:stale))
         (+orgbrain--exchanges-project "orgbrain")
         (+orgbrain--exchange-index 3)
         (+orgbrain-transport
          (lambda (args _stdin cb)
            (setq sent args)
            (funcall cb (list :exit 0 :stdout "{\"ok\":true}" :stderr ""))
            nil)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &optional initial &rest _)
                 (cond ((string-match-p "slug" prompt) "atlas")
                       ((string-match-p "Title" prompt) "ATLAS")
                       (t "A verifiable-compute paper")))))
      (+orgbrain/new-project))
    (should (equal sent '("project" "new" "atlas" "--title" "ATLAS"
                          "--summary" "A verifiable-compute paper" "--json")))
    (should (equal +orgbrain--project "atlas"))
    (should (eq +orgbrain--projects-source 'server))
    (should-not +orgbrain--exchanges)
    (should-not +orgbrain--exchange-index)))

(ert-deftest orgbrain-new-project-keeps-the-old-scope-when-the-write-fails ()
  "A failed create must not leave the workspace pointed at nothing."
  (let ((+orgbrain--project "orgbrain")
        (+orgbrain--pending nil)
        (+orgbrain-transport
         (lambda (_args _stdin cb)
           (funcall cb (list :exit 1 :stdout "" :stderr "project_slug_invalid"))
           nil)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &optional initial &rest _)
                 (cond ((string-match-p "slug" prompt) "atlas")
                       ((string-match-p "Title" prompt) "ATLAS")
                       (t "s")))))
      (+orgbrain/new-project))
    (should (equal +orgbrain--project "orgbrain"))
    (should (eq +orgbrain--status 'error))))

(ert-deftest orgbrain-new-project-is-refused-while-a-job-is-running ()
  "`assert_service_idle' refuses a GBrain write while any job runs, and what a
refusal costs is whatever was just typed."
  (let ((+orgbrain--pending (list :mode 'ask :started (float-time) :process nil)))
    (should-error (+orgbrain/new-project) :type 'user-error)))

(ert-deftest orgbrain-new-project-is-not-a-request-mode ()
  "A once-ever destructive verb must not sit on the TAB cycle."
  (should-not (assq 'new-project +orgbrain-modes))
  (should-not (memq '+orgbrain--build-new-project
                    (mapcar (lambda (m) (plist-get (cdr m) :builder)) +orgbrain-modes))))

;; R3 — the dialogue abstraction
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-exchanges-filter-by-project-and-order-oldest-first ()
  "`+orgbrain-exchanges' keeps only the project's jobs, oldest first."
  (orgbrain-test--with-transport (list (cons "history" (orgbrain-test--history-json)))
    (let ((exchanges (+orgbrain-exchanges "orgbrain")))
      (should (= (length exchanges) 2))
      (should (equal (mapcar (lambda (e) (plist-get e :created)) exchanges)
                     '("2026-08-23T16:50:02.814+00:00"
                       "2026-08-24T09:00:00.000+00:00")))
      (dolist (exchange exchanges)
        (should (equal (plist-get exchange :entity) "projects/orgbrain"))))
    (should (= (length (+orgbrain-exchanges "wrappers")) 1))
    (should (null (+orgbrain-exchanges "no-such-project")))))

(ert-deftest orgbrain-exchanges-request-the-configured-history-limit ()
  "The history call passes `--limit' and `--json' and nothing else."
  (orgbrain-test--with-transport (list (cons "history" (orgbrain-test--history-json)))
    (+orgbrain-exchanges "orgbrain")
    (should (equal (plist-get (car orgbrain-test--calls) :args)
                   '("history" "--limit" "4" "--json")))))

(ert-deftest orgbrain-exchanges-take-sent-text-from-the-right-field ()
  "`ask' uses `request.text', `recall' uses `request.query', `remember' `fact'."
  (let ((records (list (orgbrain-test--record orgbrain-test--ask-json)
                       (orgbrain-test--record orgbrain-test--recall-json)
                       (orgbrain-test--record orgbrain-test--remember-json))))
    (should (equal (mapcar (lambda (e) (plist-get e :sent))
                           (+orgbrain--exchanges-from-records records nil))
                   '("what do we know about the retrieval pipeline?"
                     "Wrappers"
                     "Wrappers chose Groth16 over STARKs")))))

(ert-deftest orgbrain-empty-history-yields-no-exchanges ()
  "An empty history array is a normal state, not an error."
  (orgbrain-test--with-transport '(("history" . "[]"))
    (should (null (+orgbrain-exchanges "orgbrain")))))

(ert-deftest orgbrain-unreadable-history-is-a-user-error ()
  "Non-JSON history output is reported to the owner, not parsed as empty."
  (orgbrain-test--with-transport '(("history" . "command not found: orgbrain"))
    (should-error (+orgbrain-exchanges "orgbrain") :type 'user-error)))

;; ---------------------------------------------------------------------------
;; S1 — the project list and its fallback
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-project-list-prefers-the-server-verb ()
  "When `project list --json' answers, its slugs are used and marked `server'."
  (orgbrain-test--with-transport
      '(("project" . "[\"projects/alpha\", \"beta\"]")
        ("history" . "[]"))
    (let ((projects (+orgbrain-projects)))
      (should (equal (car projects) '("alpha" "beta" "orgbrain")))
      (should (eq (cdr projects) 'server)))))

(ert-deftest orgbrain-project-list-accepts-object-shapes ()
  "Object and wrapped shapes of the coming verb are parsed too."
  (should (equal (+orgbrain--parse-project-list
                  (+orgbrain--parse-json
                   "[{\"slug\": \"projects/alpha\"}, {\"name\": \"beta\"}]"))
                 '("alpha" "beta")))
  (should (equal (+orgbrain--parse-project-list
                  (+orgbrain--parse-json
                   "{\"projects\": [\"projects/alpha\"]}"))
                 '("alpha"))))

(ert-deftest orgbrain-project-list-falls-back-to-history-entities ()
  "With no `project list' verb the slugs come from history `request.entity'."
  (orgbrain-test--with-transport
      (list (cons "project" '(2 . "error: unrecognized subcommand 'list'"))
            (cons "history" (orgbrain-test--history-json)))
    (let ((projects (+orgbrain-projects)))
      (should (equal (car projects) '("orgbrain" "wrappers")))
      (should (eq (cdr projects) 'history)))))

(ert-deftest orgbrain-project-list-falls-back-to-defaults ()
  "With neither verb nor history the default projects keep the client usable."
  (orgbrain-test--with-transport
      (list (cons "project" '(2 . "error: unrecognized subcommand 'list'"))
            (cons "history" "[]"))
    (let ((projects (+orgbrain-projects)))
      (should (equal (car projects) '("orgbrain")))
      (should (eq (cdr projects) 'default)))))

(ert-deftest orgbrain-fallback-is-visible-in-the-header-line ()
  "A derived or default project set is announced, never silent."
  (should (equal (+orgbrain--projects-note 'server) ""))
  (should (string-match-p "projects: history" (+orgbrain--projects-note 'history)))
  (should (string-match-p "projects: default" (+orgbrain--projects-note 'default))))

;; ---------------------------------------------------------------------------
;; S4 — serialisation
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-send-refuses-while-a-request-is-in-flight ()
  "A second send is refused so a GBrain write is never rejected."
  (let ((+orgbrain--pending (list :mode 'ask :started 0.0)))
    (should-error (+orgbrain/send) :type 'user-error)))

;; ---------------------------------------------------------------------------
;; Transport shape
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-ssh-transport-builds-the-verified-command ()
  "The SSH transport shells out exactly as the verified probe does.
`ConnectTimeout' is part of the contract: `BatchMode' only suppresses
prompts, so without it a down tunnel freezes the synchronous calls in the
TCP connect for the system default."
  (let (command)
    (cl-letf (((symbol-function '+orgbrain--run)
               (lambda (cmd _stdin _callback) (setq command cmd))))
      (let ((+orgbrain-ssh-host "vienna")
            (+orgbrain-command "orgbrain")
            (+orgbrain-connect-timeout 8))
        (+orgbrain--transport-ssh '("ask" "-" "--json") "hi" nil)))
    (should (equal command
                   '("ssh" "-o" "BatchMode=yes" "-o" "ConnectTimeout=8"
                     "vienna" "orgbrain ask - --json")))))

(ert-deftest orgbrain-local-transport-runs-the-cli-directly ()
  "The local transport is the same call without SSH, for use on the host."
  (let (command)
    (cl-letf (((symbol-function '+orgbrain--run)
               (lambda (cmd _stdin _callback) (setq command cmd))))
      (let ((+orgbrain-command "orgbrain"))
        (+orgbrain--transport-local '("recall" "x" "--json") nil nil)))
    (should (equal command '("orgbrain" "recall" "x" "--json")))))

;; ---------------------------------------------------------------------------
;; Documentation contract
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-documentation-describes-the-bindings ()
  "The keybinding reference documents the workspace as implemented."
  (let ((docs (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "docs.md" user-emacs-directory))
                (buffer-string))))
    (dolist (required '(":orgbrain"
                        "+orgbrain/open"
                        "`TAB`"
                        "`C-c C-c`"
                        "`gp`"
                        "`C-c C-p`"
                        "remember that"
                        "+orgbrain-ssh-host"
                        "+orgbrain-connect-timeout"
                        "C-x o p"
                        "projects: truncated"
                        "(unscoped)"))
      (should (string-match-p (regexp-quote required) docs)))))


;; ---------------------------------------------------------------------------
;; Server integration: the real `project list --json' payload
;; ---------------------------------------------------------------------------

(defconst orgbrain-test--project-list-json "
{
  \"projects\": [ { \"slug\": \"projects/emacs-d\",  \"name\": \"emacs-d\",  \"title\": \"\" },
                 { \"slug\": \"projects/orgbrain\", \"name\": \"orgbrain\", \"title\": \"OrgBrain\" } ],
  \"count\": 2, \"complete\": true, \"truncated\": false,
  \"dropped\": 0, \"rejected\": null, \"limit\": 200, \"sanitizer_cap\": 24
}"
  "Verbatim output of `orgbrain project list --json' as the daemon emits it.
Generated by running the server's own `project_list' against a fake
adapter, so this is the shipped shape and not a guess.")

(ert-deftest orgbrain-parses-the-real-project-list-payload ()
  "The daemon's `project list --json' object yields bare project slugs."
  (should (equal (+orgbrain--parse-project-list
                  (+orgbrain--parse-json orgbrain-test--project-list-json))
                 '("emacs-d" "orgbrain"))))

(ert-deftest orgbrain-project-list-truncation-is-surfaced ()
  "A truncated project list is reported, not shown as though it were whole.
The sanitizer caps `list_pages' at 24, so the server reports its own
completeness; dropping that would present a short list as the full set."
  (let ((truncated (replace-regexp-in-string
                    "\"truncated\": false" "\"truncated\": true"
                    orgbrain-test--project-list-json t t)))
    (cl-letf (((symbol-function '+orgbrain--cli)
               (lambda (&rest _) truncated)))
      (should (eq (cdr (+orgbrain-projects)) 'server-truncated))))
  (cl-letf (((symbol-function '+orgbrain--cli)
             (lambda (&rest _) orgbrain-test--project-list-json)))
    (should (eq (cdr (+orgbrain-projects)) 'server)))
  (should (equal (+orgbrain--projects-note 'server-truncated)
                 "  |  projects: truncated")))

;; ---------------------------------------------------------------------------
;; Window layout
;; ---------------------------------------------------------------------------

(defun orgbrain-test--stub-transport (args stdin callback)
  "Answer ARGS from canned data so no layout test contacts the daemon.
STDIN is ignored.  CALLBACK takes a transport result plist, exactly as
`+orgbrain--run' hands it over, so async callers complete too."
  (ignore stdin)
  (let ((result (list :exit 0
                      :stdout (if (member "list" args)
                                  orgbrain-test--project-list-json
                                "[]")
                      :stderr "")))
    (if callback (progn (funcall callback result) 'stub) result)))

(defmacro orgbrain-test--with-workspace (&rest body)
  "Run BODY with a fresh workspace open and the transport stubbed."
  (declare (indent 0))
  `(let ((+orgbrain-transport #'orgbrain-test--stub-transport)
         (+orgbrain--project nil)
         (+orgbrain--mode 'ask))
     (unwind-protect
         (progn (delete-other-windows)
                (set-frame-parameter (selected-frame) '+orgbrain-input-window nil)
                ,@body)
       (dolist (kind '(output input))
         (let ((buffer (get-buffer (+orgbrain--buffer-name kind))))
           (when (buffer-live-p buffer) (kill-buffer buffer))))
       (set-frame-parameter (selected-frame) '+orgbrain-input-window nil)
       (delete-other-windows))))

(ert-deftest orgbrain-open-builds-the-two-buffer-split ()
  "`+orgbrain/open' shows output over input and leaves point in input."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (should (= (length (window-list)) 2))
    (should (equal (buffer-name (current-buffer))
                   (+orgbrain--buffer-name 'input)))
    (let ((output (get-buffer-window (+orgbrain--buffer-name 'output)))
          (input (get-buffer-window (+orgbrain--buffer-name 'input))))
      (should (window-live-p output))
      (should (window-live-p input))
      ;; Output on top: its first line is above the input window's.
      (should (< (window-top-line output) (window-top-line input))))))

(ert-deftest orgbrain-open-is-idempotent ()
  "Reopening the workspace reuses its windows instead of splitting again.
Point normally rests in the input pane, so an `open' that trusted the
selected window turned that pane into a second output pane and leaked a
window on every call."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (+orgbrain/open)
    (+orgbrain/open)
    (should (= (length (window-list)) 2))
    (should (equal (buffer-name (current-buffer))
                   (+orgbrain--buffer-name 'input)))))

(ert-deftest orgbrain-open-restores-a-deleted-input-pane ()
  "Deleting the input window and reopening rebuilds the split."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (delete-window (get-buffer-window (+orgbrain--buffer-name 'input)))
    (should (= (length (window-list)) 1))
    (+orgbrain/open)
    (should (= (length (window-list)) 2))
    (should (equal (buffer-name (current-buffer))
                   (+orgbrain--buffer-name 'input)))))


;; ---------------------------------------------------------------------------
;; Transport robustness: `+orgbrain--run' must never signal
;; ---------------------------------------------------------------------------

(defun orgbrain-test--hidden-buffers ()
  "Return the transport's hidden stdout/stderr buffers still alive."
  (seq-filter (lambda (buffer)
                (string-prefix-p " *orgbrain-" (buffer-name buffer)))
              (buffer-list)))

(ert-deftest orgbrain-run-reports-a-missing-program-instead-of-signalling ()
  "A program that cannot start is a result, not a signal.
`make-process' raises `file-missing' for an absent program.  Letting that
escape left `+orgbrain--pending' set and refused every later send until
Emacs restarted."
  (let ((result (+orgbrain--run '("orgbrain-no-such-program-xyz") "hi" nil)))
    (should (eq (plist-get result :exit) 'failed))
    (should (equal (plist-get result :stdout) ""))
    (should (string-match-p "no such file\\|not found\\|Searching for program"
                            (plist-get result :stderr)))
    (should (string-match-p "could not run" (+orgbrain--result-error result))))
  (should-not (orgbrain-test--hidden-buffers)))

(ert-deftest orgbrain-run-reports-a-closed-stdin-instead-of-signalling ()
  "A child that closes its input mid-write is reported, never signalled.
`process-send-string' raises when the pipe is gone, which is the ordinary
shape of an ssh that dies before consuming a long brief."
  (let ((result (+orgbrain--run '("sh" "-c" "exit 7")
                                (make-string 400000 ?x) nil)))
    (should (memq (plist-get result :exit) '(7 failed)))
    (should (+orgbrain--result-error result)))
  (should-not (orgbrain-test--hidden-buffers)))

(ert-deftest orgbrain-run-delivers-a-failure-to-its-callback ()
  "The async path calls back with the failure rather than losing the request."
  (let (delivered)
    (+orgbrain--run '("orgbrain-no-such-program-xyz") nil
                    (lambda (result) (setq delivered result)))
    (should (eq (plist-get delivered :exit) 'failed)))
  (should-not (orgbrain-test--hidden-buffers)))

(ert-deftest orgbrain-run-kills-its-hidden-buffers ()
  "A completed call leaves no stdout/stderr buffer and no live process."
  (let (delivered)
    (+orgbrain--run '("sh" "-c" "printf out; printf err 1>&2") nil
                    (lambda (result) (setq delivered result)))
    (let ((waited 0))
      (while (and (null delivered) (< waited 200))
        (accept-process-output nil 0.05)
        (setq waited (1+ waited))))
    (should (equal (plist-get delivered :stdout) "out"))
    (should (equal (plist-get delivered :stderr) "err")))
  (should-not (orgbrain-test--hidden-buffers))
  (should-not (seq-filter (lambda (process)
                            (string-prefix-p "orgbrain" (process-name process)))
                          (process-list))))

;; ---------------------------------------------------------------------------
;; S4 — a failed send must never wedge the client
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-send-does-not-wedge-when-the-dispatch-signals ()
  "A signalling transport clears `+orgbrain--pending' on its way out.
A stuck pending refuses every later send with `still in flight' and only a
restart of Emacs clears it, so the guard is the difference between one
lost request and an unusable client."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (+orgbrain--set-input "a brief worth not losing")
    (let ((+orgbrain-transport
           (lambda (_args _stdin _callback)
             (signal 'file-missing (list "Searching for program" "ssh")))))
      (should-error (+orgbrain/send) :type 'file-missing))
    (should-not +orgbrain--pending)
    (should (eq +orgbrain--status 'error))
    ;; And the next send is accepted rather than refused as still in flight.
    (+orgbrain--set-input "the next brief")
    (+orgbrain/send)
    (should-not +orgbrain--pending)))

(ert-deftest orgbrain-send-clears-pending-when-the-call-fails ()
  "A transport-level failure reports the error and re-enables send."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (+orgbrain--set-input "brief")
    (let ((+orgbrain-transport
           (lambda (_args _stdin callback)
             (funcall callback (list :exit 'failed :stdout ""
                                     :stderr "ssh: no such program"))
             'stub)))
      (+orgbrain/send))
    (should-not +orgbrain--pending)
    (should (eq +orgbrain--status 'error))))

(ert-deftest orgbrain-send-is-refused-while-a-real-request-is-outstanding ()
  "S4: with one request in flight the second send is a `user-error'."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (+orgbrain--set-input "the first brief")
    (let ((+orgbrain-transport (lambda (_args _stdin _callback) 'never-returns)))
      (+orgbrain/send)
      (should +orgbrain--pending)
      (should (eq +orgbrain--status 'working))
      (+orgbrain--set-input "the second brief")
      (should-error (+orgbrain/send) :type 'user-error))
    (setq +orgbrain--pending nil)))

;; ---------------------------------------------------------------------------
;; Window layout under rude buffer handling
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-reopen-after-killing-the-input-buffer-keeps-two-panes ()
  "Killing `*orgbrain-input*' and reopening must not leak a window.
The dead input buffer's window falls back to showing `*orgbrain*', and an
`open' that took any window showing the output buffer as the output pane
split the frame again and left two panes on the same transcript."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (kill-buffer (+orgbrain--buffer-name 'input))
    (+orgbrain/open)
    (should (= (length (window-list)) 2))
    (should (equal (buffer-name (current-buffer))
                   (+orgbrain--buffer-name 'input)))
    (should (= (length (get-buffer-window-list
                        (+orgbrain--buffer-name 'output) nil
                        (selected-frame)))
               1))))

(ert-deftest orgbrain-reopen-after-killing-the-output-buffer-keeps-two-panes ()
  "Killing `*orgbrain*' and reopening rebuilds the transcript pane."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (kill-buffer (+orgbrain--buffer-name 'output))
    (+orgbrain/open)
    (should (= (length (window-list)) 2))
    (should (window-live-p (get-buffer-window (+orgbrain--buffer-name 'output))))
    (should (equal (buffer-name (current-buffer))
                   (+orgbrain--buffer-name 'input)))))

(ert-deftest orgbrain-open-refuses-a-pane-too-short-to-split ()
  "A pane too short for two windows is a `user-error', not a raw `error'.
`split-window' was asked for a negative upper size once the 6-line floor
exceeded the pane, which signalled `Window ... too small for splitting'."
  (orgbrain-test--with-workspace
    (split-window) (split-window)
    (should (> (length (window-list)) 2))
    (should-error (+orgbrain/open) :type 'user-error)))

(ert-deftest orgbrain-writes-survive-a-killed-output-buffer ()
  "Appending after the owner killed the transcript must not signal."
  (orgbrain-test--with-workspace
    (+orgbrain/open)
    (kill-buffer (+orgbrain--buffer-name 'output))
    (+orgbrain--append "recovered\n")
    (should (buffer-live-p (get-buffer (+orgbrain--buffer-name 'output))))
    (kill-buffer (+orgbrain--buffer-name 'input))
    (should (equal (+orgbrain--input-text) ""))
    (+orgbrain--refresh-header)))

(ert-deftest orgbrain-switcher-can-return-to-unscoped ()
  "Selecting the unscoped candidate clears the project scope.
Most jobs in the live history carry `request.entity: null', so a
workspace that can never leave a project can never replay them."
  (let ((+orgbrain--project "orgbrain")
        (+orgbrain--projects-source 'default)
        (+orgbrain-default-projects '("orgbrain")))
    (cl-letf (((symbol-function '+orgbrain-projects)
               (lambda () (cons '("orgbrain") 'default)))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member +orgbrain--unscoped-choice collection))
                 +orgbrain--unscoped-choice)))
      (+orgbrain/switch-project))
    (should (null +orgbrain--project))
    ;; An unscoped workspace passes no --entity and walks every exchange.
    (should (null (+orgbrain--entity-args +orgbrain--project)))))

(ert-deftest orgbrain-switcher-selects-a-named-project ()
  "Selecting a slug scopes the workspace to it."
  (let ((+orgbrain--project nil)
        (+orgbrain--projects-source 'default))
    (cl-letf (((symbol-function '+orgbrain-projects)
               (lambda () (cons '("emacs-d" "orgbrain") 'server)))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "emacs-d")))
      (+orgbrain/switch-project))
    (should (equal +orgbrain--project "emacs-d"))
    (should (equal (+orgbrain--entity-args "emacs-d")
                   '("--entity" "projects/emacs-d")))))


;; ---------------------------------------------------------------------------
;; Pane labelling and the input buffer's fate after a send
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-panes-are-labelled-and-only-input-shows-the-mode ()
  "Each pane names itself, and `mode' appears only where a send is issued."
  (let ((+orgbrain--project "orgbrain")
        (+orgbrain--mode 'ask)
        (+orgbrain--status 'idle)
        (+orgbrain--projects-source 'server)
        (+orgbrain-ssh-host "vienna"))
    (let ((output (+orgbrain--header-line 'output))
          (input (+orgbrain--header-line 'input)))
      (should (string-prefix-p "OUTPUT" output))
      (should (string-prefix-p "INPUT" input))
      ;; The mode decides what a send does; the transcript issues no sends.
      (should-not (string-match-p "mode:" output))
      (should (string-match-p "mode: ask" input))
      (dolist (line (list output input))
        (should (string-match-p "project: orgbrain" line))
        (should (string-match-p "vienna idle" line)))
      ;; The two panes must not render identically -- that is what made them
      ;; indistinguishable in use.
      (should-not (equal output input)))))

(ert-deftest orgbrain-a-successful-send-clears-the-input ()
  "The brief is cleared only after the transcript holds it."
  (let ((+orgbrain--project "orgbrain")
        (+orgbrain--pending (list :mode 'ask)))
    (unwind-protect
        (progn
          (+orgbrain--set-input "what is the progress?")
          (+orgbrain--finish-send 'ask "what is the progress?"
                                  orgbrain-test--ask-json nil)
          (should (equal (+orgbrain--input-text) ""))
          ;; The transcript keeps what was sent, quoted.
          (should (string-match-p
                   (regexp-quote "> what is the progress?")
                   (with-current-buffer (+orgbrain--buffer 'output)
                     (buffer-string)))))
      (dolist (kind '(output input))
        (let ((buffer (get-buffer (+orgbrain--buffer-name kind))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest orgbrain-a-failed-send-keeps-the-input ()
  "A failure must never cost the owner the thought they typed."
  (let ((+orgbrain--project "orgbrain")
        (+orgbrain--pending (list :mode 'ask)))
    (unwind-protect
        (progn
          (+orgbrain--set-input "a long brief")
          (+orgbrain--finish-send 'ask "a long brief" nil "ssh: connect failed")
          (should (equal (+orgbrain--input-text) "a long brief"))
          ;; Unparseable output is also a failure: keep the brief.
          (+orgbrain--finish-send 'ask "a long brief" "not json at all" nil)
          (should (equal (+orgbrain--input-text) "a long brief")))
      (dolist (kind '(output input))
        (let ((buffer (get-buffer (+orgbrain--buffer-name kind))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))


;; ---------------------------------------------------------------------------
;; Recovering from a lost in-flight flag
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-a-stale-in-flight-flag-does-not-refuse-forever ()
  "A flag whose process is gone is cleared, not obeyed.
The callback that would clear it will never run, so refusing every later
send would wedge the client until Emacs restarted."
  (let ((+orgbrain--project "orgbrain")
        ;; Exactly the shape an older build left behind: no `:process' key.
        (+orgbrain--pending (list :mode 'ask :started 0))
        (sent nil))
    (unwind-protect
        (cl-letf (((symbol-function '+orgbrain--cli)
                   (lambda (&rest _) (setq sent t) nil)))
          (+orgbrain--set-input "a question")
          (+orgbrain/send)
          (should sent))
      (dolist (kind '(output input))
        (let ((buffer (get-buffer (+orgbrain--buffer-name kind))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest orgbrain-a-live-request-still-refuses-a-second-send ()
  "S4 holds: a genuinely running process blocks a concurrent send."
  (let* ((proc (start-process "orgbrain-test-sleep" nil "sleep" "30"))
         (+orgbrain--project "orgbrain")
         (+orgbrain--pending (list :mode 'ask :started 0 :process proc)))
    (unwind-protect
        (progn
          (+orgbrain--set-input "a question")
          (should-error (+orgbrain/send) :type 'user-error))
      (ignore-errors (delete-process proc))
      (dolist (kind '(output input))
        (let ((buffer (get-buffer (+orgbrain--buffer-name kind))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest orgbrain-a-dead-recorded-process-is-also-stale ()
  "A recorded process that has exited marks the flag stale."
  (let* ((proc (start-process "orgbrain-test-true" nil "true"))
         (+orgbrain--project "orgbrain")
         (sent nil))
    (while (process-live-p proc) (accept-process-output nil 0.02))
    (let ((+orgbrain--pending (list :mode 'ask :started 0 :process proc)))
      (unwind-protect
          (cl-letf (((symbol-function '+orgbrain--cli)
                     (lambda (&rest _) (setq sent t) nil)))
            (+orgbrain--set-input "a question")
            (+orgbrain/send)
            (should sent))
        (dolist (kind '(output input))
          (let ((buffer (get-buffer (+orgbrain--buffer-name kind))))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest orgbrain-reset-clears-the-request-state ()
  "`+orgbrain/reset' is the escape hatch and leaves the client sendable."
  (let ((+orgbrain--project "orgbrain")
        (+orgbrain--pending (list :mode 'ask :started 0))
        (+orgbrain--status 'working)
        (+orgbrain--exchange-index 3)
        (+orgbrain-transport #'orgbrain-test--stub-transport))
    (unwind-protect
        (progn
          (+orgbrain/reset)
          (should (null +orgbrain--pending))
          (should (eq +orgbrain--status 'idle))
          (should (null +orgbrain--exchange-index)))
      (dolist (kind '(output input))
        (let ((buffer (get-buffer (+orgbrain--buffer-name kind))))
          (when (buffer-live-p buffer) (kill-buffer buffer))))
      (set-frame-parameter (selected-frame) '+orgbrain-input-window nil)
      (delete-other-windows))))


;; ---------------------------------------------------------------------------
;; persp-mode: adding a buffer must not steal the selected window
;; ---------------------------------------------------------------------------

(ert-deftest orgbrain-persp-add-buffer-does-not-clobber-the-split ()
  "The split survives a `persp-add-buffer' that switches the selected window.
`persp-switch-to-added-buffer' defaults to t, so `persp-add-buffer' puts
the buffer it is handed into the selected window.  Building the split
calls it twice, and the second call used to overwrite the window the
transcript had just been placed in -- leaving both panes on the input
buffer and the answer appended to a buffer displayed nowhere.

This is the failure a batch `-Q' run cannot see, because persp-mode is
not loaded there.  The fake below reproduces only the switching."
  (let ((persp-switch-to-added-buffer t)
        (+orgbrain-transport #'orgbrain-test--stub-transport)
        (+orgbrain--project "orgbrain"))
    (cl-letf (((symbol-function 'persp-add-buffer)
               (lambda (buf &rest _)
                 (when persp-switch-to-added-buffer
                   (set-window-buffer (selected-window) buf))
                 buf)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-frame-parameter (selected-frame) '+orgbrain-input-window nil)
            (+orgbrain/open)
            (let ((shown (mapcar (lambda (win) (buffer-name (window-buffer win)))
                                 (window-list))))
              (should (member (+orgbrain--buffer-name 'output) shown))
              (should (member (+orgbrain--buffer-name 'input) shown))
              ;; The transcript must be visible: an invisible output pane is
              ;; how an answer silently goes nowhere.
              (should (window-live-p
                       (get-buffer-window (+orgbrain--buffer-name 'output))))))
        (dolist (kind '(output input))
          (let ((buffer (get-buffer (+orgbrain--buffer-name kind))))
            (when (buffer-live-p buffer) (kill-buffer buffer))))
        (set-frame-parameter (selected-frame) '+orgbrain-input-window nil)
        (delete-other-windows)))))

(provide 'orgbrain-test)

;;; orgbrain-test.el ends here
