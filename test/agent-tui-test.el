;;; agent-tui-test.el --- Tests for agent TUI navigation -*- lexical-binding: t -*-

;;; Commentary:
;; Pure-function coverage for core/init-agent-tui.el: mouse-mode detection,
;; title detection, screen-cell math, and the SGR click sequence.  No PTY or
;; live vterm is started.

;;; Code:

(require 'ert)
(require 'cl-lib)

(unless (featurep 'init-agent-tui)
  (load-file (expand-file-name
              "../core/init-agent-tui.el"
              (file-name-directory (or load-file-name buffer-file-name)))))

;; =============================================================================
;; Mouse-mode detection
;; =============================================================================

(ert-deftest agent-tui-mouse-enable-detected ()
  "A DECSET enable of a mouse mode turns tracking on."
  (should (+agent-tui-mouse-mode-after-output nil "\e[?1000h"))
  (should (+agent-tui-mouse-mode-after-output nil "\e[?1002h"))
  (should (+agent-tui-mouse-mode-after-output nil "\e[?1003h")))

(ert-deftest agent-tui-mouse-combined-enable-detected ()
  "A combined SGR enable sequence turns tracking on."
  (should (+agent-tui-mouse-mode-after-output nil "\e[?1000;1002;1003;1006h")))

(ert-deftest agent-tui-mouse-disable-detected ()
  "A DECSET reset of a mouse mode turns tracking off."
  (should-not (+agent-tui-mouse-mode-after-output t "\e[?1000l"))
  (should-not (+agent-tui-mouse-mode-after-output t "\e[?1000;1002;1006l")))

(ert-deftest agent-tui-mouse-unrelated-modes-ignored ()
  "Non-mouse private modes leave tracking unchanged."
  (should (+agent-tui-mouse-mode-after-output t "\e[?25l"))
  (should (+agent-tui-mouse-mode-after-output t "\e[?2004h"))
  (should-not (+agent-tui-mouse-mode-after-output nil "\e[?25h")))

(ert-deftest agent-tui-mouse-sgr-encoding-alone-is-not-tracking ()
  "SGR extended encoding without a tracking mode does not enable clicks."
  (should-not (+agent-tui-mouse-mode-after-output nil "\e[?1006h")))

(ert-deftest agent-tui-mouse-last-wins ()
  "The most recent enable/reset in a chunk wins."
  (should-not (+agent-tui-mouse-mode-after-output nil "\e[?1000h\e[?1000l"))
  (should (+agent-tui-mouse-mode-after-output nil "\e[?1000l\e[?1000h")))

(ert-deftest agent-tui-mouse-survives-partial-chunks ()
  "Chunks without mouse modes leave the prior state alone."
  (should (+agent-tui-mouse-mode-after-output t "plain output"))
  (should-not (+agent-tui-mouse-mode-after-output nil "plain output")))

;; =============================================================================
;; Title detection
;; =============================================================================

(ert-deftest agent-tui-title-detects-agents ()
  "OSC titles naming either agent are recognised."
  (should (+agent-tui-title-matches-p "\e]0;OpenCode\a"))
  (should (+agent-tui-title-matches-p "\e]2;OpenCode\a"))
  (should (+agent-tui-title-matches-p "\e]0;\u2733 Claude Code\a"))
  (should (+agent-tui-title-matches-p "\e]0;OpenCode\e\\")))

(ert-deftest agent-tui-title-ignores-other-programs ()
  "Shell or editor titles are ignored."
  (should-not (+agent-tui-title-matches-p "\e]0;zsh\a"))
  (should-not (+agent-tui-title-matches-p "\e]0;~/Work/dragon\a"))
  (should-not (+agent-tui-title-matches-p "no title here")))

;; =============================================================================
;; Screen-cell math
;; =============================================================================

(ert-deftest agent-tui-cell-relative-to-viewport ()
  "Rows are relative to the frozen viewport's first line."
  (should (equal (+agent-tui-cell 1 1 0) '(1 . 1)))
  (should (equal (+agent-tui-cell 40 42 7) '(3 . 8))))

(ert-deftest agent-tui-cell-rejects-off-screen ()
  "A point above the viewport yields no cell."
  (should-not (+agent-tui-cell 10 9 0)))

;; =============================================================================
;; SGR click sequence
;; =============================================================================

(ert-deftest agent-tui-sgr-mouse-press-and-release ()
  "SGR press ends in M, release in m, with column before row."
  (should (equal (+agent-tui-sgr-mouse 0 5 10 t) "\e[<0;10;5M"))
  (should (equal (+agent-tui-sgr-mouse 0 5 10 nil) "\e[<0;10;5m")))

;; =============================================================================
;; Clean yanks
;; =============================================================================

(ert-deftest agent-tui-clean-yanked-text-strips-decorations ()
  "Box drawing and block characters are removed, spacing collapsed."
  (should (equal (+agent-tui-clean-yanked-text "\u2503 hello") "hello"))
  (should (equal (+agent-tui-clean-yanked-text "\u2500\u2500\u2500") ""))
  (should (equal (+agent-tui-clean-yanked-text "a    b\n\u2502 c") "a b\nc")))

(provide 'agent-tui-test)
;;; agent-tui-test.el ends here
