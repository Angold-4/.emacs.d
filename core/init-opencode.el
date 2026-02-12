;;; init-opencode.el --- OpenCode integration for vterm -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: OpenCode-specific vterm configuration with vim-style navigation

;;; Commentary:
;; This module configures vterm for optimal OpenCode usage.
;;
;; OpenCode is a full-screen TUI (Bubble Tea). The vterm buffer contains only
;; the currently visible screen. OpenCode's layout:
;;   - Header:  4 lines (top)
;;   - Messages: H-8 lines (middle, scrollable inside OpenCode)
;;   - Input:   4 lines (bottom)
;;
;; We use a custom freeze/unfreeze mechanism (not vterm-copy-mode, which
;; destroys keymaps) to pause terminal output in evil normal mode.
;;
;; Cursor behavior (vim-in-a-file style):
;;   - j/k move the cursor line by line within the message area
;;   - When the cursor hits the top/bottom edge of the message area,
;;     a scroll command is sent to OpenCode to reveal more content
;;   - h/l move left/right freely on the frozen screen
;;   - v enters visual mode for text selection and yanking

;;; Code:

(require 'cl-lib)
(require 'seq)

;; =============================================================================
;; Box-drawing character cleanup for yanked text
;; =============================================================================

(defvar +opencode/box-chars-re
  (rx (any (#x2500 . #x257F)           ; Box Drawing (━, ┃, ┏, ┓, ┗, ┛, ┣, ┫, ┳, ┻, ╋, ╭, ╮, etc.)
           (#x2580 . #x259F)           ; Block Elements (▀, ▁, ▄, █, ▌, ▐, ▕, ▏, etc.)
           #x25A0 #x25A1 #x25AA #x25AB ; Geometric shapes (■, □, ▪, ▫)
           #x25B2 #x25B6 #x25BC #x25C0 ; Triangles (▲, ▶, ▼, ◀)
           #x25CF #x25CB               ; Circles (●, ○)
           #x25A3                      ; ▣
           #x2713 #x2714               ; Check marks (✓, ✔)
           #x2717 #x2718               ; Crosses (✗, ✘)
           #x2022                      ; Bullet (•)
           ))
  "Regexp matching box-drawing, block, and decorative characters from OpenCode's TUI.
Covers Unicode ranges:
  U+2500-257F  Box Drawing
  U+2580-259F  Block Elements
  Plus common decorative symbols (checkmarks, bullets, geometric shapes).")

(defun +opencode/clean-yanked-text (text)
  "Strip TUI decorations from TEXT for clean pasting.
Removes box-drawing characters, block elements, decorative symbols,
and cleans up excess whitespace left behind."
  (let* ((lines (split-string text "\n"))
         (cleaned
          (mapcar
           (lambda (line)
             (let ((s line))
               ;; Remove box-drawing and decorative characters
               (setq s (replace-regexp-in-string +opencode/box-chars-re "" s))
               ;; Collapse runs of 3+ spaces into single space
               (setq s (replace-regexp-in-string "   +" " " s))
               ;; Trim both ends
               (string-trim s)))
           lines)))
    ;; Remove lines that became empty after stripping (were pure decoration)
    ;; but preserve intentional blank lines by checking if original had content
    (let* ((paired (cl-mapcar #'cons lines cleaned))
           (filtered (mapcar #'cdr
                             (seq-remove
                              (lambda (pair)
                                (and (string-empty-p (cdr pair))
                                     (not (string-empty-p (string-trim (car pair))))))
                              paired))))
      (string-trim (string-join filtered "\n")))))

(defun +opencode/yank-clean ()
  "Yank the selected region, clean it, and copy to kill ring.
Use this in visual mode to copy clean text from OpenCode."
  (interactive)
  (when (use-region-p)
    (let* ((raw (buffer-substring-no-properties (region-beginning) (region-end)))
           (clean (+opencode/clean-yanked-text raw)))
      (kill-new clean)
      (when (fboundp '+clipboard/set)
        (+clipboard/set clean))
      (message "Copied %d lines (cleaned)" (length (split-string clean "\n")))
      (evil-normal-state))))

;; =============================================================================
;; Configuration
;; =============================================================================

(defvar +opencode/header-lines 4
  "Number of lines OpenCode's header occupies at the top of the screen.")

(defvar +opencode/footer-lines 4
  "Number of lines OpenCode's input/status area occupies at the bottom.")

;; =============================================================================
;; Buffer-local state
;; =============================================================================

(defvar-local +opencode/buffer-p nil
  "Non-nil if this buffer is an OpenCode vterm buffer.")

(defvar-local +opencode/frozen-p nil
  "Non-nil if the OpenCode display is currently frozen.")

;; =============================================================================
;; Message area bounds
;; =============================================================================

(defun +opencode/msg-first-line ()
  "Return the buffer line number of the first message area line."
  (1+ +opencode/header-lines))

(defun +opencode/msg-last-line ()
  "Return the buffer line number of the last message area line."
  (let ((total-lines (count-lines (point-min) (point-max))))
    (- total-lines +opencode/footer-lines)))

(defun +opencode/goto-line (n)
  "Move point to the beginning of line N (1-indexed)."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun +opencode/current-line ()
  "Return the current line number (1-indexed)."
  (line-number-at-pos (point)))

(defun +opencode/clamp-to-msg-area ()
  "Ensure point is within the message area bounds."
  (let ((cur (+opencode/current-line))
        (first (+opencode/msg-first-line))
        (last (+opencode/msg-last-line)))
    (cond
     ((< cur first) (+opencode/goto-line first))
     ((> cur last) (+opencode/goto-line last)))))

;; =============================================================================
;; Custom freeze/unfreeze (preserves evil keybindings)
;; =============================================================================

(defun +opencode/freeze ()
  "Freeze the display: pause terminal output, keep keybindings alive."
  (when (and (not +opencode/frozen-p)
             (derived-mode-p 'vterm-mode)
             vterm--term)
    (setq +opencode/frozen-p t)
    (setq vterm-copy-mode t)
    (vterm-send-stop)))

(defun +opencode/unfreeze ()
  "Unfreeze the display: resume terminal output."
  (when (and +opencode/frozen-p
             (derived-mode-p 'vterm-mode)
             vterm--term)
    (setq +opencode/frozen-p nil)
    (setq vterm-copy-mode nil)
    (vterm-reset-cursor-point)
    (vterm-send-start)))

;; =============================================================================
;; Core: send a scroll key to OpenCode, then re-freeze
;; =============================================================================

(defun +opencode/scroll-opencode (key &optional shift meta ctrl target-line)
  "Unfreeze, send KEY to OpenCode, wait for redraw, re-freeze.
After re-freezing, move cursor to TARGET-LINE (1-indexed).
SHIFT, META, CTRL are modifier flags for `vterm-send-key'."
  (when (and +opencode/buffer-p (derived-mode-p 'vterm-mode) vterm--term)
    (let ((inhibit-redisplay t)
          (col (current-column)))
      ;; 1. Unfreeze
      (+opencode/unfreeze)
      ;; 2. Send key
      (vterm-send-key key shift meta ctrl)
      ;; 3. Wait for redraw
      (accept-process-output vterm--process 0.05 nil t)
      (when vterm--term
        (let ((inhibit-read-only t))
          (vterm--redraw vterm--term)))
      ;; 4. Re-freeze
      (+opencode/freeze)
      ;; 5. Position cursor
      (when target-line
        (+opencode/goto-line target-line)
        (move-to-column col)))))

;; =============================================================================
;; Navigation commands
;; =============================================================================

(defun +opencode/move-down (count)
  "Move cursor down COUNT lines within the message area.
When hitting the bottom edge, scroll OpenCode down and keep cursor at bottom."
  (interactive "p")
  (let ((cur (+opencode/current-line))
        (last (+opencode/msg-last-line))
        (col (current-column)))
    (dotimes (_ (or count 1))
      (setq cur (+opencode/current-line))
      (setq last (+opencode/msg-last-line))
      (if (>= cur last)
          ;; At bottom edge — scroll OpenCode down, stay at bottom
          (+opencode/scroll-opencode "e" nil t t last)
        ;; Within message area — just move cursor down
        (forward-line 1)))
    (move-to-column col)))

(defun +opencode/move-up (count)
  "Move cursor up COUNT lines within the message area.
When hitting the top edge, scroll OpenCode up and keep cursor at top."
  (interactive "p")
  (let ((cur (+opencode/current-line))
        (first (+opencode/msg-first-line))
        (col (current-column)))
    (dotimes (_ (or count 1))
      (setq cur (+opencode/current-line))
      (setq first (+opencode/msg-first-line))
      (if (<= cur first)
          ;; At top edge — scroll OpenCode up, stay at top
          (+opencode/scroll-opencode "y" nil t t first)
        ;; Within message area — just move cursor up
        (forward-line -1)))
    (move-to-column col)))

(defun +opencode/half-page-down ()
  "Scroll OpenCode down half a page."
  (interactive)
  (let ((mid (/ (+ (+opencode/msg-first-line) (+opencode/msg-last-line)) 2)))
    (+opencode/scroll-opencode "d" nil t t mid)))

(defun +opencode/half-page-up ()
  "Scroll OpenCode up half a page."
  (interactive)
  (let ((mid (/ (+ (+opencode/msg-first-line) (+opencode/msg-last-line)) 2)))
    (+opencode/scroll-opencode "u" nil t t mid)))

(defun +opencode/goto-last-message ()
  "Jump to the last (most recent) message in OpenCode."
  (interactive)
  (+opencode/scroll-opencode "g" nil t t (+opencode/msg-last-line)))

(defun +opencode/goto-first-message ()
  "Jump to the first message in OpenCode."
  (interactive)
  (+opencode/scroll-opencode "g" nil nil t (+opencode/msg-first-line)))

;; =============================================================================
;; State transitions: evil <-> freeze/unfreeze
;; =============================================================================

(defun +opencode/enter-normal-mode ()
  "When entering evil normal state, freeze and position cursor in message area."
  (when (and +opencode/buffer-p
             (derived-mode-p 'vterm-mode))
    (+opencode/freeze)
    (+opencode/clamp-to-msg-area)))

(defun +opencode/enter-insert-mode ()
  "When entering evil insert state, unfreeze."
  (when (and +opencode/buffer-p
             (derived-mode-p 'vterm-mode))
    (+opencode/unfreeze)))

;; =============================================================================
;; OpenCode Vterm Setup
;; =============================================================================

(defun +opencode/vterm-setup ()
  "Setup vterm for OpenCode with vim-style cursor-moves-through-content."
  (when (bound-and-true-p evil-mode)

    (setq +opencode/buffer-p t)

    ;; --- Window navigation (both modes) ---
    (evil-local-set-key 'normal (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'normal (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'normal (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'normal (kbd "C-k") 'windmove-up)
    (evil-local-set-key 'insert (kbd "C-h") 'windmove-left)
    (evil-local-set-key 'insert (kbd "C-l") 'windmove-right)
    (evil-local-set-key 'insert (kbd "C-j") 'windmove-down)
    (evil-local-set-key 'insert (kbd "C-k") 'windmove-up)

    ;; =====================================================================
    ;; NORMAL MODE
    ;; =====================================================================

    ;; j/k — move cursor within message area, scroll at edges
    (evil-local-set-key 'normal (kbd "j") #'+opencode/move-down)
    (evil-local-set-key 'normal (kbd "k") #'+opencode/move-up)

    ;; J/K — half page scroll
    (evil-local-set-key 'normal (kbd "J") #'+opencode/half-page-down)
    (evil-local-set-key 'normal (kbd "K") #'+opencode/half-page-up)

    ;; G / gg — jump to last / first message
    (evil-local-set-key 'normal (kbd "G") #'+opencode/goto-last-message)
    (evil-local-set-key 'normal (kbd "g g") #'+opencode/goto-first-message)

    ;; h/l — standard evil motions (work on frozen buffer, no override needed)

    ;; i / a — enter insert mode
    (evil-local-set-key 'normal (kbd "i")
      (lambda () (interactive) (evil-insert-state)))
    (evil-local-set-key 'normal (kbd "a")
      (lambda () (interactive) (evil-insert-state)))

    ;; p — paste into OpenCode input
    (evil-local-set-key 'normal (kbd "p")
      (lambda () (interactive)
        (let ((text (current-kill 0)))
          (when text
            (let ((line (+opencode/current-line))
                  (col (current-column)))
              (+opencode/unfreeze)
              (vterm-send-string text)
              (accept-process-output vterm--process 0.05 nil t)
              (+opencode/freeze)
              (+opencode/goto-line line)
              (move-to-column col))))))

    ;; yy — yank current line (cleaned)
    (evil-local-set-key 'normal (kbd "y y")
      (lambda () (interactive)
        (let* ((raw (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
               (clean (+opencode/clean-yanked-text raw)))
          (kill-new clean)
          (when (fboundp '+clipboard/set)
            (+clipboard/set clean))
          (message "Copied: %s" (truncate-string-to-width clean 60)))))

    ;; q — bury buffer
    (evil-local-set-key 'normal (kbd "q")
      (lambda () (interactive) (bury-buffer)))

    ;; =====================================================================
    ;; INSERT MODE
    ;; =====================================================================

    (evil-local-set-key 'insert (kbd "C-v")
      (lambda () (interactive)
        (let ((text (current-kill 0)))
          (when text (vterm-send-string text)))))

    (evil-local-set-key 'insert (kbd "C-c C-c")
      (lambda () (interactive)
        (vterm-send-key "c" nil nil t)))

    ;; =====================================================================
    ;; VISUAL MODE: standard motions for selection + clean yank
    ;; =====================================================================

    ;; j/k in visual mode must extend selection (not scroll OpenCode)
    (evil-local-set-key 'visual (kbd "j") #'evil-next-line)
    (evil-local-set-key 'visual (kbd "k") #'evil-previous-line)

    ;; y in visual mode — yank with box-character cleanup
    (evil-local-set-key 'visual (kbd "y") #'+opencode/yank-clean)

    ;; C-y in visual mode — copy clean text to system clipboard
    (evil-local-set-key 'visual (kbd "C-y") #'+opencode/yank-clean)

    ;; Override evil-collection-vterm's visual d/x (they try to interact
    ;; with the terminal which makes no sense on a frozen buffer)
    (evil-local-set-key 'visual (kbd "d") #'evil-delete)
    (evil-local-set-key 'visual (kbd "x") #'evil-delete)

    ;; =====================================================================
    ;; Evil state hooks
    ;; =====================================================================
    (add-hook 'evil-normal-state-entry-hook #'+opencode/enter-normal-mode nil t)
    (add-hook 'evil-insert-state-entry-hook #'+opencode/enter-insert-mode nil t)
    (add-hook 'evil-visual-state-entry-hook #'+opencode/enter-normal-mode nil t)))

;; =============================================================================
;; Auto-detect and apply
;; =============================================================================

(defun +opencode/maybe-setup ()
  "If this vterm buffer is named *opencode*, apply OpenCode keybindings."
  (when (string-match-p "\\*opencode\\*" (buffer-name))
    (+opencode/vterm-setup)))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook #'+opencode/maybe-setup 90))

;; =============================================================================
;; Toggle OpenCode
;; =============================================================================

(defun +opencode/toggle ()
  "Toggle OpenCode in a dedicated vterm buffer."
  (interactive)
  (let ((opencode-buffer-name "*opencode*"))
    (if-let ((existing-buffer (get-buffer opencode-buffer-name)))
        (switch-to-buffer existing-buffer)
      (let ((vterm-buffer (generate-new-buffer opencode-buffer-name)))
        (switch-to-buffer vterm-buffer)
        (vterm-mode)
        (vterm-send-string "opencode")
        (vterm-send-return)))))

;; =============================================================================
;; Keybinding
;; =============================================================================

(global-set-key (kbd "C-c o") '+opencode/toggle)

;;; init-opencode.el ends here
