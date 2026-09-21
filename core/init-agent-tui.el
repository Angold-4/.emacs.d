;;; init-agent-tui.el --- Vim keys for agent TUIs in vterm -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ango Wang

;;; Commentary:
;;
;; OpenCode and Claude Code are full-screen TUIs (Bubble Tea and a custom
;; Ink-style renderer) that we run inside vterm.  Scrolling their output and
;; moving around it means reaching for PageUp/PageDown, which is awkward from
;; Evil normal state, and sending them arrow keys is worse: their arrow keys
;; drive the agent's own focus and history (OpenCode drops you into the input
;; box), not a cursor over the transcript.
;;
;; So this module treats the frozen screen like a buffer and moves the real
;; Emacs cursor over it, while leaving the agent's own keys for paging:
;;
;;   j / k    move the cursor down / up a line; at the top/bottom edge,
;;            scroll the agent's transcript so you can keep going
;;   h / l    move the cursor left / right a character
;;   J / K    send <next> / <prior>  (PageDown / PageUp) to page the agent
;;   RET      click the terminal cell under point (SGR mouse)
;;   i / a    enter insert state so real keys reach the agent
;;   p        paste the clipboard into the agent's input
;;   yy       yank the current line, stripped of TUI box drawing
;;   q        bury the buffer
;;
;; init-tools.el already freezes the selected vterm viewport in normal state,
;; so the cursor stays over a stable screen and the app cannot scroll it away.
;;
;; Both agents also have an inline (non-alternate-screen) mode, where the
;; transcript flows into the terminal scrollback instead:
;;
;;   Claude Code:  CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1 claude
;;   OpenCode:     opencode --mini
;;
;; We track whether the app took the alternate screen from its output.  When
;; it did not, this buffer is an ordinary scrollback buffer, so j/k/h/l walk
;; the whole transcript, J/K page it with evil-scroll-page-down/up, and RET
;; simply sends Return rather than trying to click a cell that may be history.
;; The launchers use the inline modes by default where they do not lose
;; features (see `+agent-tui-claude-command' / `+agent-tui-opencode-command').
;;
;; Both agents enable xterm mouse tracking once their session is live
;; (verified: OpenCode emits ?1000h/?1002h/?1003h/?1006h on startup, Claude
;; Code after the trust prompt).  We watch terminal output for those DECSET
;; modes and only synthesise a click when the app actually listens for one;
;; otherwise RET falls back to sending Return.
;;
;; A synthetic click is sent as an SGR sequence directly to the PTY
;; (`process-send-string'), bypassing libvterm, because emacs-libvterm does
;; not forward mouse events.  The cell is computed from the frozen viewport,
;; so the click lands on the row Emacs is showing.
;;
;; Auto-enabled for vterm buffers named "*opencode*" / "*claude*" (see
;; `+agent-tui-buffer-name-regexp') and for any buffer whose terminal title
;; announces one of the agents.  `+agent-tui-setup' enables it by hand in an
;; arbitrary vterm.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'seq)

(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-mode "vterm" ())
(declare-function vterm--filter "vterm" (process input))
(declare-function evil-local-set-key "evil-core" (state key def))
(declare-function evil-insert-state "evil" (&optional arg))
(declare-function evil-normal-state "evil" (&optional arg))
(declare-function evil-next-line "evil-commands" (&optional count))
(declare-function evil-previous-line "evil-commands" (&optional count))
(declare-function evil-backward-char "evil-commands" (&optional count))
(declare-function evil-forward-char "evil-commands" (&optional count))
(declare-function evil-scroll-page-down "evil-commands" (&optional count))
(declare-function evil-scroll-page-up "evil-commands" (&optional count))
(declare-function evil-delete "evil-commands" (beg end &optional type register yank-handler))
(declare-function +clipboard/get "init-core" (&optional arg))

(defvar vterm--process)
(defvar vterm--term)

;; =============================================================================
;; Detection
;; =============================================================================

(defcustom +agent-tui-buffer-name-regexp
  "\\*\\(?:opencode\\|claude\\)\\*"
  "Buffer names that should get agent TUI keybindings automatically."
  :type 'regexp
  :group 'tools)

(defcustom +agent-tui-title-regexp
  "\\(?:OpenCode\\|Claude Code\\)"
  "Terminal title substrings that identify an agent TUI.
Used when the buffer name does not match `+agent-tui-buffer-name-regexp',
for example a plain `claude' run inside a numbered terminal."
  :type 'regexp
  :group 'tools)

(defconst +agent-tui--osc-title-regexp
  (concat "\e]" "[012];" "\\([^\a\e]*\\)" "\\(?:" "\a" "\\|" "\e\\\\" "\\)")
  "Regexp matching an OSC title sequence; match 1 is the title.
Titles end with BEL or the ST sequence ESC backslash.")

(defconst +agent-tui--decset-regexp
  "\e\\[\\?\\([0-9;]*\\)\\([hl]\\)"
  "Regexp matching a DEC private mode set (`h') or reset (`l') sequence.")

(defconst +agent-tui--mouse-modes '("1000" "1002" "1003")
  "DEC private modes that make an application report mouse events.")

(defconst +agent-tui--alt-screen-modes '("1047" "1049")
  "DEC private modes that switch an application to the alternate screen.")

(defun +agent-tui-decset-state-after-output (modes current input)
  "Return the state of MODES after terminal INPUT, given CURRENT state.
MODES is a list of DEC private mode numbers.  Returns non-nil when the
most recent set (`h') or reset (`l') among them was a set."
  (let ((mode current)
        (start 0))
    (while (string-match +agent-tui--decset-regexp input start)
      ;; Capture both groups before `split-string', which runs its own
      ;; `string-match' and would clobber this match data.
      (let* ((param-string (match-string 1 input))
             (enabled (string= "h" (match-string 2 input)))
             (params (split-string param-string ";" t)))
        (when (seq-intersection params modes)
          (setq mode enabled)))
      (setq start (match-end 0)))
    mode))

(defun +agent-tui-mouse-mode-after-output (current input)
  "Return mouse-tracking state after terminal INPUT, given CURRENT state."
  (+agent-tui-decset-state-after-output +agent-tui--mouse-modes current input))

(defun +agent-tui-alt-screen-after-output (current input)
  "Return alternate-screen state after terminal INPUT, given CURRENT state."
  (+agent-tui-decset-state-after-output +agent-tui--alt-screen-modes current input))

(defun +agent-tui-title-matches-p (input)
  "Return non-nil when terminal INPUT sets an agent TUI title."
  (let ((start 0)
        found)
    (while (and (not found)
                (string-match +agent-tui--osc-title-regexp input start))
      (when (string-match-p +agent-tui-title-regexp (match-string 1 input))
        (setq found t))
      (setq start (match-end 0)))
    found))

;; =============================================================================
;; Buffer-local state
;; =============================================================================

(defvar-local +agent-tui-p nil
  "Non-nil when this vterm buffer has agent TUI keybindings.")

(defvar-local +agent-tui-mouse-tracking nil
  "Non-nil when the running application enables mouse reporting.")

(defvar-local +agent-tui-alt-screen nil
  "Non-nil when the running application owns the alternate screen.
When nil the agent renders inline, so the transcript accumulates in the
terminal scrollback and this buffer behaves like an ordinary text buffer.")

;; =============================================================================
;; Sending input
;; =============================================================================

(defun +agent-tui--live-p ()
  "Return non-nil when the current buffer is a live vterm."
  (and (derived-mode-p 'vterm-mode)
       (bound-and-true-p vterm--term)))

(defun +agent-tui--send-key (key &optional shift meta ctrl)
  "Send KEY to the agent with optional SHIFT, META and CTRL modifiers."
  (when (+agent-tui--live-p)
    (vterm-send-key key shift meta ctrl)))

(defun +agent-tui--send-string (string)
  "Send STRING directly to the terminal process."
  (when (and (+agent-tui--live-p)
             (process-live-p (bound-and-true-p vterm--process)))
    (process-send-string vterm--process string)))

(defun +agent-tui-cell (start-line point-line column)
  "Return (ROW . COL) for a screen cell, or nil when off screen.
START-LINE is the buffer line at the top of the viewport, POINT-LINE the
line under point, and COLUMN a 0-based buffer column.  ROW and COL are
1-based terminal cells."
  (let ((row (1+ (- point-line start-line)))
        (col (1+ column)))
    (when (and (>= row 1) (>= col 1))
      (cons row col))))

(defun +agent-tui-screen-cell (&optional window)
  "Return (ROW . COL), 1-based, of point in WINDOW's visible terminal.
WINDOW defaults to the selected window.  The frozen viewport kept by
init-tools.el in normal state makes row 1 the top of the terminal screen."
  (let* ((window (or window (selected-window)))
         (buffer (window-buffer window))
         (start (window-start window))
         (point (window-point window)))
    (with-current-buffer buffer
      (+agent-tui-cell (line-number-at-pos start)
                       (line-number-at-pos point)
                       (save-excursion
                         (goto-char point)
                         (current-column))))))

(defun +agent-tui-viewport-cell (&optional window)
  "Return (ROW . COL), 1-based, at the middle of WINDOW's screen.
Wheel events are sent here because the transcript, not the input box or a
sidebar at the cursor, is the surface that scrolls."
  (let* ((window (or window (selected-window)))
         (height (window-body-height window))
         (width (window-body-width window)))
    (cons (max 1 (/ height 2)) (max 1 (/ width 2)))))

(defun +agent-tui--scroll (direction &optional count)
  "Scroll the agent transcript by COUNT notches in DIRECTION.
DIRECTION is `down' or `up'.  Uses the mouse wheel when the app reports the
mouse, and a page key otherwise (for example Claude Code before the trust
prompt, which enables no mouse mode)."
  (if +agent-tui-mouse-tracking
      (let ((cell (+agent-tui-viewport-cell)))
        (when cell
          (let ((button (if (eq direction 'down) 65 64))
                (row (car cell))
                (col (cdr cell)))
            (dotimes (_ (or count 1))
              (+agent-tui--send-string (+agent-tui-sgr-mouse button row col t))))))
    (+agent-tui--send-key (if (eq direction 'down) "<next>" "<prior>"))))

(defun +agent-tui-sgr-mouse (button row col press)
  "Return the SGR mouse escape sequence for BUTTON at ROW/COL.
PRESS non-nil means a button press, nil a release.  Coordinates are
1-based screen cells, matching the SGR `CSI < Cb ; Cx ; Cy M' form."
  (format "\e[<%d;%d;%d%s" button col row (if press "M" "m")))

;; =============================================================================
;; Navigation commands
;; =============================================================================

;; j/k/h/l move the Emacs cursor instead of poking the agent.  The agent's
;; arrow keys change its own focus and history (OpenCode jumps to the input
;; box), which is not what "move the cursor" should mean.
;;
;; On the alternate screen the frozen buffer is only one page tall, so at the
;; top/bottom edge j/k scroll the agent itself (wheel, or a page key without
;; mouse support) and the cursor stays on the edge while the transcript moves
;; past it.  In inline mode the whole transcript is already in the buffer, so
;; j/k are ordinary line motions through it.

(defun +agent-tui-down (count)
  "Move the cursor COUNT lines down, scrolling the agent at the bottom edge."
  (interactive "p")
  (if (not +agent-tui-alt-screen)
      (evil-next-line (or count 1))
    (dotimes (_ (or count 1))
      (let ((line (line-number-at-pos)))
        (ignore-errors (evil-next-line 1))
        (when (= line (line-number-at-pos))
          (+agent-tui--scroll 'down 1))))))

(defun +agent-tui-up (count)
  "Move the cursor COUNT lines up, scrolling the agent at the top edge."
  (interactive "p")
  (if (not +agent-tui-alt-screen)
      (evil-previous-line (or count 1))
    (dotimes (_ (or count 1))
      (let ((line (line-number-at-pos)))
        (ignore-errors (evil-previous-line 1))
        (when (= line (line-number-at-pos))
          (+agent-tui--scroll 'up 1))))))

(defun +agent-tui-left (count)
  "Move the cursor COUNT characters left over the frozen agent screen."
  (interactive "p")
  (evil-backward-char (or count 1)))

(defun +agent-tui-right (count)
  "Move the cursor COUNT characters right over the frozen agent screen."
  (interactive "p")
  (evil-forward-char (or count 1)))

;; On the alternate screen J/K send exactly the same keys as the real
;; PageDown/PageUp, and nothing else: moving point as well would scroll the
;; view a second time on top of the agent's own scroll and make it stutter.
;; Inline buffers have real scrollback, so J/K page the buffer directly.

(defun +agent-tui-page-down (count)
  "Page the agent, or the buffer, down COUNT times."
  (interactive "p")
  (if (not +agent-tui-alt-screen)
      (evil-scroll-page-down (or count 1))
    (dotimes (_ (or count 1))
      (+agent-tui--send-key "<next>"))))

(defun +agent-tui-page-up (count)
  "Page the agent, or the buffer, up COUNT times."
  (interactive "p")
  (if (not +agent-tui-alt-screen)
      (evil-scroll-page-up (or count 1))
    (dotimes (_ (or count 1))
      (+agent-tui--send-key "<prior>"))))

(defun +agent-tui-click ()
  "Click the terminal cell under point, or send Return when that cannot work.
A click is synthesised only on the alternate screen for applications that
enabled mouse tracking, because an unsupported SGR sequence would otherwise
be typed as input, and because in inline mode the cell under point may be
scrolled-back history rather than something live to click."
  (interactive)
  (unless (+agent-tui--live-p)
    (user-error "Not a live vterm buffer"))
  (if (and +agent-tui-alt-screen +agent-tui-mouse-tracking)
      (let ((cell (+agent-tui-screen-cell)))
        (unless cell
          (user-error "Point is not on a terminal cell"))
        (let ((row (car cell))
              (col (cdr cell)))
          (+agent-tui--send-string (+agent-tui-sgr-mouse 0 row col t))
          (+agent-tui--send-string (+agent-tui-sgr-mouse 0 row col nil))
          (accept-process-output vterm--process 0.05 nil t)))
    (vterm-send-return)))

(defun +agent-tui-insert ()
  "Enter insert state so real keystrokes reach the agent."
  (interactive)
  (evil-insert-state))

(defun +agent-tui-paste ()
  "Paste the host clipboard into the agent's input, as a bracketed paste."
  (interactive)
  (let ((text (or (ignore-errors (+clipboard/get)) (current-kill 0))))
    (when (and text (not (string-empty-p text)))
      (vterm-send-string text t))))

;; =============================================================================
;; Clean yanks
;; =============================================================================

(defvar +agent-tui-box-chars-re
  (rx (any (#x2500 . #x257F)           ; Box Drawing
           (#x2580 . #x259F)           ; Block Elements
           #x25A0 #x25A1 #x25AA #x25AB ; Geometric shapes
           #x25B2 #x25B6 #x25BC #x25C0 ; Triangles
           #x25CF #x25CB               ; Circles
           #x25A3                      ; ▣
           #x2713 #x2714               ; Check marks
           #x2717 #x2718               ; Crosses
           #x2022))                    ; Bullet
  "Regexp matching TUI decorations that should not be yanked.")

(defun +agent-tui-clean-yanked-text (text)
  "Strip TUI decorations and excess whitespace from TEXT."
  (let* ((lines (split-string text "\n"))
         (cleaned
          (mapcar
           (lambda (line)
             (string-trim
              (replace-regexp-in-string
               "   +" " "
               (replace-regexp-in-string +agent-tui-box-chars-re "" line))))
           lines)))
    (string-trim
     (string-join
      (delq nil
            (cl-mapcar (lambda (raw clean)
                         (if (and (string-empty-p clean)
                                  (not (string-empty-p (string-trim raw))))
                             nil
                           clean))
                       lines cleaned))
      "\n"))))

(defun +agent-tui-kill-region-clean ()
  "Copy the active region, cleaned, to the kill ring."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((clean (+agent-tui-clean-yanked-text
                (buffer-substring-no-properties (region-beginning) (region-end)))))
    (kill-new clean)
    (when (fboundp '+clipboard/set)
      (+clipboard/set clean))
    (message "Copied %d lines (cleaned)" (length (split-string clean "\n")))
    (evil-normal-state)))

(defun +agent-tui-kill-line-clean ()
  "Copy the current line, cleaned, to the kill ring."
  (interactive)
  (let ((clean (+agent-tui-clean-yanked-text
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))))
    (kill-new clean)
    (when (fboundp '+clipboard/set)
      (+clipboard/set clean))
    (message "Copied: %s" (truncate-string-to-width clean 60))))

;; =============================================================================
;; Setup
;; =============================================================================

(defun +agent-tui-setup ()
  "Install agent TUI keybindings in the current vterm buffer."
  (interactive)
  (when (and (derived-mode-p 'vterm-mode)
             (bound-and-true-p evil-mode))
    (setq +agent-tui-p t)

    ;; Navigation: drive the agent's cursor, and move point in step so the
    ;; block cursor stays over the row the agent highlights.
    (evil-local-set-key 'normal (kbd "j") #'+agent-tui-down)
    (evil-local-set-key 'normal (kbd "k") #'+agent-tui-up)
    (evil-local-set-key 'normal (kbd "h") #'+agent-tui-left)
    (evil-local-set-key 'normal (kbd "l") #'+agent-tui-right)
    (evil-local-set-key 'normal (kbd "J") #'+agent-tui-page-down)
    (evil-local-set-key 'normal (kbd "K") #'+agent-tui-page-up)

    ;; Click the cell under point; agents that do not report the mouse get a
    ;; plain Return instead.
    (evil-local-set-key 'normal (kbd "RET") #'+agent-tui-click)
    (evil-local-set-key 'normal (kbd "<return>") #'+agent-tui-click)

    ;; i/a hand the keyboard back to the agent.
    (evil-local-set-key 'normal (kbd "i") #'+agent-tui-insert)
    (evil-local-set-key 'normal (kbd "a") #'+agent-tui-insert)

    ;; Clipboard and clean yanks.
    (evil-local-set-key 'normal (kbd "p") #'+agent-tui-paste)
    (evil-local-set-key 'normal (kbd "y y") #'+agent-tui-kill-line-clean)
    (evil-local-set-key 'visual (kbd "y") #'+agent-tui-kill-region-clean)
    (evil-local-set-key 'visual (kbd "C-y") #'+agent-tui-kill-region-clean)

    ;; Keep visual-mode motions local to the frozen screen.
    (evil-local-set-key 'visual (kbd "j") #'evil-next-line)
    (evil-local-set-key 'visual (kbd "k") #'evil-previous-line)
    (evil-local-set-key 'visual (kbd "d") #'evil-delete)
    (evil-local-set-key 'visual (kbd "x") #'evil-delete)

    (evil-local-set-key 'normal (kbd "q") #'bury-buffer)))

(defun +agent-tui-maybe-setup ()
  "Enable agent TUI keybindings when the buffer name names an agent."
  (when (and (derived-mode-p 'vterm-mode)
             (string-match-p +agent-tui-buffer-name-regexp (buffer-name)))
    (+agent-tui-setup)))

;; =============================================================================
;; Output watcher
;; =============================================================================

(defun +agent-tui-note-output (process input)
  "Track mouse support and agent titles in terminal INPUT for PROCESS.
Installed as :before advice on `vterm--filter'."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq +agent-tui-alt-screen
              (+agent-tui-alt-screen-after-output +agent-tui-alt-screen input))
        (unless (and +agent-tui-p +agent-tui-mouse-tracking)
          (setq +agent-tui-mouse-tracking
                (+agent-tui-mouse-mode-after-output
                 +agent-tui-mouse-tracking input))
          (unless +agent-tui-p
            (when (+agent-tui-title-matches-p input)
              (+agent-tui-setup))))))))

(with-eval-after-load 'vterm
  (unless (advice-member-p #'+agent-tui-note-output #'vterm--filter)
    (advice-add #'vterm--filter :before #'+agent-tui-note-output))
  (add-hook 'vterm-mode-hook #'+agent-tui-maybe-setup 80))

;; =============================================================================
;; Launchers
;; =============================================================================

(defcustom +agent-tui-opencode-command "opencode"
  "Shell command used by `+agent-tui-opencode'.
OpenCode's full TUI uses the alternate screen, which limits the buffer to
one page.  Set this to \"opencode --mini\" for the inline renderer, whose
transcript stays in the terminal scrollback."
  :type 'string
  :group 'tools)

(defcustom +agent-tui-claude-command
  "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1 claude"
  "Shell command used by `+agent-tui-claude'.
Claude Code's inline mode keeps the transcript in the terminal scrollback,
so the vterm buffer behaves like a normal Emacs buffer.  Drop the environment
variable to get the full-screen alternate-screen renderer instead."
  :type 'string
  :group 'tools)

(defun +agent-tui--open (name command)
  "Show the agent terminal NAME, starting COMMAND when it does not exist."
  (require 'vterm)
  (let ((buffer (get-buffer name)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer name))
      (with-current-buffer buffer
        (vterm-mode)
        (+agent-tui-setup)
        (vterm-send-string command)
        (vterm-send-return)))
    (pop-to-buffer buffer)))

(defun +agent-tui-opencode ()
  "Open or focus OpenCode in a dedicated vterm buffer."
  (interactive)
  (+agent-tui--open "*opencode*" +agent-tui-opencode-command))

(defun +agent-tui-claude ()
  "Open or focus Claude Code in a dedicated vterm buffer."
  (interactive)
  (+agent-tui--open "*claude*" +agent-tui-claude-command))

(global-set-key (kbd "C-c o") #'+agent-tui-opencode)
(global-set-key (kbd "C-c O") #'+agent-tui-claude)

(provide 'init-agent-tui)
;;; init-agent-tui.el ends here
