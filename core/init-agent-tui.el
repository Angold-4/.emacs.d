;;; init-agent-tui.el --- Vim keys for agent TUIs in vterm -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ango Wang

;;; Commentary:
;;
;; OpenCode and Claude Code are full-screen TUIs (Bubble Tea and a custom
;; Ink-style renderer) that we run inside vterm.  Their own cursor is moved
;; with the arrow keys and PageUp/PageDown, which is awkward from Evil normal
;; state.  This module turns those keys into the Vim vocabulary we already use
;; for buffers:
;;
;;   j / k    send <down> / <up>       and nudge Emacs point with them
;;   h / l    send <left> / <right>    and nudge Emacs point with them
;;   J / K    send <next> / <prior>    (PageDown / PageUp)
;;   RET      click the terminal cell under point (SGR mouse)
;;   i / a    enter insert state so real keys reach the agent
;;   p        paste the clipboard into the agent's input
;;   yy       yank the current line, stripped of TUI box drawing
;;   q        bury the buffer
;;
;; Both agents enable xterm mouse tracking once their session is live
;; (verified: OpenCode emits ?1000h/?1002h/?1003h/?1006h on startup, Claude
;; Code after the trust prompt).  We watch terminal output for those DECSET
;; modes and only synthesise a click when the app actually listens for one;
;; otherwise RET falls back to sending Return.
;;
;; A synthetic click is sent as an SGR sequence directly to the PTY
;; (`process-send-string'), bypassing libvterm, because emacs-libvterm does
;; not forward mouse events.  The cell is computed from the frozen viewport
;; that init-tools.el maintains in normal state, so the click lands on the
;; row Emacs is showing.
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

(defun +agent-tui-mouse-mode-after-output (current input)
  "Return mouse-tracking state after terminal INPUT, given CURRENT state.
INPUT is a raw chunk of terminal output.  Returns non-nil when the most
recent DECSET mouse mode was an enable, nil when it was a disable."
  (let ((mode current)
        (start 0))
    (while (string-match +agent-tui--decset-regexp input start)
      ;; Capture both groups before `split-string', which runs its own
      ;; `string-match' and would clobber this match data.
      (let* ((param-string (match-string 1 input))
             (enabled (string= "h" (match-string 2 input)))
             (params (split-string param-string ";" t)))
        (when (seq-intersection params +agent-tui--mouse-modes)
          (setq mode enabled)))
      (setq start (match-end 0)))
    mode))

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

(defun +agent-tui-sgr-mouse (button row col press)
  "Return the SGR mouse escape sequence for BUTTON at ROW/COL.
PRESS non-nil means a button press, nil a release.  Coordinates are
1-based screen cells, matching the SGR `CSI < Cb ; Cx ; Cy M' form."
  (format "\e[<%d;%d;%d%s" button col row (if press "M" "m")))

;; =============================================================================
;; Navigation commands
;; =============================================================================

(defun +agent-tui-down (count)
  "Send COUNT Down keys to the agent, moving point along."
  (interactive "p")
  (dotimes (_ (or count 1))
    (+agent-tui--send-key "<down>")
    (ignore-errors (forward-line 1))))

(defun +agent-tui-up (count)
  "Send COUNT Up keys to the agent, moving point along."
  (interactive "p")
  (dotimes (_ (or count 1))
    (+agent-tui--send-key "<up>")
    (ignore-errors (forward-line -1))))

(defun +agent-tui-left (count)
  "Send COUNT Left keys to the agent, moving point along."
  (interactive "p")
  (dotimes (_ (or count 1))
    (+agent-tui--send-key "<left>")
    (ignore-errors (backward-char 1))))

(defun +agent-tui-right (count)
  "Send COUNT Right keys to the agent, moving point along."
  (interactive "p")
  (dotimes (_ (or count 1))
    (+agent-tui--send-key "<right>")
    (ignore-errors (forward-char 1))))

(defun +agent-tui-page-down (count)
  "Send COUNT PageDown keys to the agent, moving point along."
  (interactive "p")
  (dotimes (_ (or count 1))
    (+agent-tui--send-key "<next>")
    (ignore-errors (forward-line (window-body-height)))))

(defun +agent-tui-page-up (count)
  "Send COUNT PageUp keys to the agent, moving point along."
  (interactive "p")
  (dotimes (_ (or count 1))
    (+agent-tui--send-key "<prior>")
    (ignore-errors (forward-line (- (window-body-height))))))

(defun +agent-tui-click ()
  "Click the terminal cell under point, or send Return when unsupported.
A click is synthesised only for applications that enabled mouse tracking,
because an unsupported SGR sequence would otherwise be typed as input."
  (interactive)
  (unless (+agent-tui--live-p)
    (user-error "Not a live vterm buffer"))
  (if +agent-tui-mouse-tracking
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
  (+agent-tui--open "*opencode*" "opencode"))

(defun +agent-tui-claude ()
  "Open or focus Claude Code in a dedicated vterm buffer."
  (interactive)
  (+agent-tui--open "*claude*" "claude"))

(global-set-key (kbd "C-c o") #'+agent-tui-opencode)
(global-set-key (kbd "C-c O") #'+agent-tui-claude)

(provide 'init-agent-tui)
;;; init-agent-tui.el ends here
