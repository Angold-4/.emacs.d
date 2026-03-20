;;; init-core.el --- Core settings and utility macros -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Core Emacs settings, macros, and utility functions

;;; Commentary:
;; This module provides:
;; - Custom macros for cleaner configuration (add-hook!, defadvice!)
;; - Core Emacs settings that should be set early
;; - Utility functions used throughout the configuration

;;; Code:

;; =============================================================================
;; Utility Macros
;; =============================================================================

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:
  1. The hook(s) to add to.
  2. Optional properties :local, :append, and/or :depth [N].
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

Examples:
  (add-hook! after-init-hook (message \"Loaded!\"))
  (add-hook! (prog-mode-hook text-mode-hook) :append #'my-func)

\(fn HOOKS [:append :local [:depth N] :remove :call-immediately] FUNCTIONS-OR-FORMS...)"
  (declare (indent defun))
  (let* ((hook-forms (if (listp hooks) hooks (list hooks)))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p call-immediately-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))
        (:call-immediately (setq call-immediately-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil)) next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (func (list ,@func-forms))
         (dolist (hook (nreverse ',hook-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))
         ,(if call-immediately-p `(funcall func))))))


(defmacro defadvice! (symbol arglist &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

Example:
  (defadvice! +my-advice (fn &rest args)
    :around #'some-function
    (message \"Before\")
    (apply fn args)
    (message \"After\"))

\(fn SYMBOL ARGLIST &rest [WHERE PLACES...] BODY\)"
  (declare (indent defun))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))


;; =============================================================================
;; Utility Functions
;; =============================================================================

(defun +temp-buffer-p (buffer)
  "Return t if BUFFER is temporary."
  (string-match-p "^ " (buffer-name buffer)))

(defun +call-fn-with-pp-to-prin1 (fn &rest args)
  "Call FN with ARGS, mapping `pp' to `prin1' when called."
  (cl-letf (((symbol-function #'pp) #'prin1)
            ((symbol-function #'pp-to-string) #'prin1-to-string))
    (apply fn args)))

(defun +unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;; =============================================================================
;; Core Emacs Settings
;; =============================================================================

;; Silence some warnings
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq warning-minimum-level :error)

;; Better default settings
(setq
 ;; Don't create lock files
 create-lockfiles nil
 
 ;; Increase read-process-output-max (helps with LSP performance)
 read-process-output-max (* 1024 1024)  ; 1MB
 
 ;; Faster interaction with subprocesses
 process-adaptive-read-buffering nil
 
 ;; Handle long lines better (Emacs 29+)
 bidi-paragraph-direction 'left-to-right
 bidi-inhibit-bpa t
 
 ;; Don't ping random machines
 ffap-machine-p-known 'reject
 
 ;; Enable recursive minibuffers
 enable-recursive-minibuffers t
 
 ;; Selection replaces region when typing
 delete-selection-mode t)

;; Remember cursor position in files
(save-place-mode 1)

;; Remember recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Save minibuffer history
(savehist-mode 1)

;; Auto-refresh buffers when files change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; =============================================================================
;; Built-in Package Configuration
;; =============================================================================

;; [xref] Cross-reference support
(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep
        xref-history-storage 'xref-window-local-history))

;; [project] Project management (built-in)
(require 'project)

;; Add Go module support
(defun +project-find-go-module (dir)
  "Find go.mod in DIR to identify Go project root."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "Return root of go-module PROJECT."
  (cdr project))

(add-hook 'project-find-functions #'+project-find-go-module)

;; =============================================================================
;; WSL2 Browser Integration
;; =============================================================================
;; Open URLs in the Windows host browser (Chrome) from WSL2.
;; This is required for OAuth flows (org-gcal) and general browse-url usage.

(when (and (eq system-type 'gnu/linux)
           (string-match-p "microsoft\\|WSL" (shell-command-to-string "uname -r")))
  (setq browse-url-generic-program "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"
        browse-url-browser-function 'browse-url-generic))

;; =============================================================================
;; Clipboard Integration (via tmux paste buffer)
;; =============================================================================

;; Strategy: Use tmux's paste buffer as a shared clipboard between panes.
;;
;;   Emacs C-y (visual mode)  →  copies text to tmux paste buffer
;;   tmux  C-a P              →  pastes from tmux buffer into active pane
;;
;; This works reliably over SSH without requiring OSC 52 terminal support.
;; The clipboard lives on the server and is shared between all tmux panes
;; (e.g. Emacs in one pane, OpenCode in another).

(defun +copy-to-system-clipboard (beg end)
  "Copy visible region text from BEG to END to tmux paste buffer.
Filters invisible text (e.g. collapsed magit sections).
The text can then be pasted in any tmux pane with C-a P."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    ;; In buffers with invisible text, extract only visible portion
    (when (next-single-property-change beg 'invisible nil end)
      (setq text
            (let ((result "")
                  (pos beg))
              (while (< pos end)
                (let* ((next-change (or (next-single-property-change
                                        pos 'invisible nil end)
                                       end))
                       (inv (invisible-p (get-text-property pos 'invisible))))
                  (unless inv
                    (setq result (concat result
                                        (buffer-substring-no-properties
                                         pos next-change))))
                  (setq pos next-change)))
              result)))
    ;; Add to Emacs kill ring
    (kill-new text)
    ;; Also write to tmux paste buffer so C-p in tmux can paste it
    (when (getenv "TMUX")
      (let ((process-connection-type nil))
        (let ((proc (start-process "tmux-copy" nil "tmux" "load-buffer" "-")))
          (process-send-string proc text)
          (process-send-eof proc))))
    (message "Copied %d chars (C-a P in tmux to paste)" (length text))))

;;; init-core.el ends here
