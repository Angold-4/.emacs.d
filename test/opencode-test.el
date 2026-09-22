;;; opencode-test.el --- ERT tests for the OpenCode client -*- lexical-binding: t -*-

;;; Commentary:
;; Pure, server-free coverage for core/init-opencode.el: the org writer and
;; reader round-trip, the file-name identity, catalog lookup, the context
;; arithmetic, replay trimming, the per-session save timers, and the mode-line
;; bars.  Runs with `-l init.el' so the module (and its package) are loaded.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'opencode)

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defun opencode-test--message (role time provider model &rest parts)
  "Return a server message shaped like the real one.
ROLE, TIME, PROVIDER and MODEL go into `info'; PARTS are `(type text)'."
  (list (cons 'info (list (cons 'role role)
                          (cons 'time (list (cons 'created time)))
                          (cons 'providerID provider)
                          (cons 'modelID model)))
        (cons 'parts
              (cl-loop for (type text) in parts
                       collect (list (cons 'type type) (cons 'text text))))))

(defun opencode-test--temp-dir ()
  "Return a fresh temporary directory."
  (file-name-as-directory (make-temp-file "opencode-test" t)))

;; ---------------------------------------------------------------------------
;; Org writer
;; ---------------------------------------------------------------------------

(ert-deftest opencode-org-writer-shape ()
  "Sections, model, escaping, and reasoning dropout."
  (let* ((time 1758500000000)
         (user (opencode-test--message "user" time "opencode" "big-pickle"
                                       (list "text" "hello")))
         (assistant (opencode-test--message "assistant" (+ time 60000)
                                            "opencode" "big-pickle"
                                            (list "text" "* not a heading")
                                            (list "reasoning" "secret thinking")
                                            (list "tool" "bash")))
         (org (+opencode--messages-to-org (list user assistant))))
    (should (string-match-p "\\`\\* Prompt " org))
    (should (string-match-p "^\\* Response " org))
    (should (string-match-p "\\[opencode/big-pickle\\]" org))
    ;; a line starting with * is escaped so it cannot become a heading
    (should (string-match-p "^,\\* not a heading" org))
    ;; reasoning is dropped
    (should-not (string-match-p "secret thinking" org))))

(ert-deftest opencode-org-writer-model-from-top-level ()
  "The response header carries the model even when `model' is absent."
  (let ((org (+opencode--messages-to-org
              (list (opencode-test--message "assistant" 1 "vercel"
                                            "deepseek/deepseek-v4.1-flash"
                                            (list "text" "hi"))))))
    (should (string-match-p "\\[vercel/deepseek/deepseek-v4.1-flash\\]" org))))

(ert-deftest opencode-org-escape-leaves-prose-alone ()
  "Only heading/keyword lines are escaped."
  (should (equal (+opencode--org-escape "plain\ntext") "plain\ntext"))
  (should (equal (+opencode--org-escape "* x\n#+y") ",* x\n,#+y")))

;; ---------------------------------------------------------------------------
;; Writer -> reader round-trip, and file-name identity
;; ---------------------------------------------------------------------------

(ert-deftest opencode-file-round-trip ()
  "A written session reads back with the same id, title and body."
  (let* ((dir (opencode-test--temp-dir))
         (+opencode-sessions-directory dir)
         (session (list (cons 'id "ses_abcdef123456")
                        (cons 'title "2026-09-22 feat/openc")
                        (cons 'directory dir)))
         (messages (list (opencode-test--message "user" 1 "opencode" "big-pickle"
                                                 (list "text" "hello"))))
         (file (+opencode--write-session session "ses_abcdef123456" messages)))
    (unwind-protect
        (progn
          (should (equal (+opencode--file-keyword file "OPENCODE_ID") "ses_abcdef123456"))
          (should (equal (+opencode--file-keyword file "OPENCODE_TITLE")
                         "2026-09-22 feat/openc"))
          (should (string-match-p "\\`\\* Prompt " (+opencode--file-body file))))
      (delete-directory dir t))))

(ert-deftest opencode-session-file-same-id-same-path ()
  "The id, not the title, decides the file, so a save cannot fork a second one."
  (let* ((dir (opencode-test--temp-dir))
         (+opencode-sessions-directory dir))
    (unwind-protect
        (let ((a (+opencode--session-file (list (cons 'title "T")) "ses_abc123456"))
              (b (+opencode--session-file (list (cons 'title "T")) "ses_abc123456")))
          (should (equal a b))
          (should (string-match-p "123456\\.org\\'" a)))
      (delete-directory dir t))))

;; ---------------------------------------------------------------------------
;; Catalog lookup (keys are interned)
;; ---------------------------------------------------------------------------

(ert-deftest opencode-model-entry-uses-interned-key ()
  "The catalog keys models by `intern', which is how lookup must work."
  (let ((opencode-providers
         (list (list (cons 'id "p")
                     (cons 'models
                           (list (cons (intern "m/one")
                                       (list (cons 'name "M One")
                                             (cons 'limit (list (cons 'context 200000)))))))))))
    (should (equal (+opencode--model-limit "p" "m/one") 200000))
    (should (equal (alist-get 'name (+opencode--model-entry "p" "m/one")) "M One"))
    (should-not (+opencode--model-entry "p" "missing"))))

;; ---------------------------------------------------------------------------
;; Context bar
;; ---------------------------------------------------------------------------

(ert-deftest opencode-compact-number ()
  "Thousands fold to k, small numbers stay plain."
  (should (equal (+opencode--compact-number 999) "999"))
  (should (equal (+opencode--compact-number 2000) "2k"))
  (should (equal (+opencode--compact-number 171000) "171k")))

(ert-deftest opencode-output-bar-escapes-percent ()
  "The bar shows the percentage, escaped so the mode line renders one percent."
  (let ((opencode-session-agent '((name . "build")
                                  (model (providerID . "p") (modelID . "m"))))
        (opencode-session-tokens 0)
        (opencode-session-status "idle")
        (+opencode--context-used 100000)
        (+opencode--context-limit 200000))
    (cl-letf (((symbol-function 'opencode--current-model)
               (lambda () '((name . "M")))))
      (let ((bar (+opencode--output-bar)))
        (should (string-match-p "ctx 100k/200k (50%%)" bar))))))

(ert-deftest opencode-output-bar-unknown-limit-is-raw ()
  "With no known limit the bar shows raw tokens, never a wrong percentage."
  (let ((opencode-session-agent nil)
        (opencode-session-status "idle")
        (+opencode--context-used 150000)
        (+opencode--context-model nil)
        (+opencode--context-limit nil))
    (cl-letf (((symbol-function 'opencode--current-model) (lambda () nil)))
      (should (string-match-p "ctx 150k" (+opencode--output-bar))))))

;; ---------------------------------------------------------------------------
;; Replay trimming
;; ---------------------------------------------------------------------------

(ert-deftest opencode-replay-trims-and-starts-at-a-prompt ()
  "The tail is kept, and it begins at a user message, not a cut reply."
  (let* ((+opencode-replay-limit 3)
         (assistant (opencode-test--message "assistant" 1 "p" "m" (list "text" "r")))
         (user (opencode-test--message "user" 2 "p" "m" (list "text" "q")))
         (messages (list assistant assistant user assistant user assistant))
         kept)
    (+opencode--limit-replay (lambda (m) (setq kept m)) messages)
    (should (<= (length kept) 3))
    (should (equal (alist-get 'role (alist-get 'info (car kept))) "user"))))

;; ---------------------------------------------------------------------------
;; Per-session save timers
;; ---------------------------------------------------------------------------

(ert-deftest opencode-save-timers-are-per-session ()
  "Scheduling two sessions keeps two timers; replying replaces its own."
  (unwind-protect
      (progn
        (+opencode--schedule-save "a")
        (+opencode--schedule-save "b")
        (should (gethash "a" +opencode--save-timers))
        (should (gethash "b" +opencode--save-timers))
        (let ((first (gethash "a" +opencode--save-timers)))
          (+opencode--schedule-save "a")
          (should-not (eq first (gethash "a" +opencode--save-timers)))))
    (maphash (lambda (_ timer) (cancel-timer timer)) +opencode--save-timers)
    (clrhash +opencode--save-timers)))

(provide 'opencode-test)
;;; opencode-test.el ends here
