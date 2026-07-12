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

;; =============================================================================
;; Large / long-line file performance
;; =============================================================================

;; The usual cause of "Emacs lags when there's lots of text" is a buffer with
;; very long lines (minified code, generated files, big JSON, logs). Emacs's
;; redisplay and font-lock get quadratically slow on long lines. `so-long'
;; detects these buffers and automatically turns off the expensive minor modes
;; (font-lock, ligatures, rainbow-delimiters, hl-todo, line numbers, ...),
;; which removes the lag. Toggle a buffer back with M-x so-long-revert.
(global-so-long-mode 1)
;; Trip so-long a bit earlier than the 10k default so heavy files are caught.
(setq so-long-threshold 5000)

;; Smoother scrolling through big buffers: don't pause to fully fontify/lay out
;; while scrolling fast (Emacs catches up when you stop).
(setq fast-but-imprecise-scrolling t)
;; Defer syntax highlighting of off-screen text until idle, so typing and
;; scrolling stay responsive in large files.
(setq jit-lock-defer-time 0)

;; Remember cursor position in files
(save-place-mode 1)

;; Remember recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Save minibuffer history
(savehist-mode 1)

;; Auto-refresh buffers when files change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil
      auto-revert-avoid-polling t)

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
;; WSL detection & host integration
;; =============================================================================

(defvar +is-wsl
  (and (eq system-type 'gnu/linux)
       (or (getenv "WSL_DISTRO_NAME")
           (getenv "WSLENV")
           (string-match-p "microsoft\\|WSL"
                           (shell-command-to-string "uname -r"))))
  "Non-nil if running inside WSL.")

(defun +clipboard/wsl-executable (basename)
  "Return a usable path for Windows program BASENAME from WSL, or nil.
Checks Emacs `exec-path'/PATH first, then standard System32 locations."
  (or (executable-find basename)
      (let ((system32 (expand-file-name basename "/mnt/c/Windows/System32/")))
        (when (file-executable-p system32) system32))
      (when (string-equal basename "powershell.exe")
        (let ((full "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"))
          (when (file-executable-p full) full)))))

;; Open URLs in the Windows host browser (Chrome) from WSL2.
(when +is-wsl
  (setq browse-url-generic-program "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"
        browse-url-browser-function 'browse-url-generic))

;; =============================================================================
;; Clipboard integration
;; =============================================================================
;;
;; C-y in evil visual mode (`+copy-to-system-clipboard') and normal yank (C-y in
;; insert) share these helpers so WSL Emacs can exchange text with the Windows
;; host clipboard (clip.exe / PowerShell), macOS (pbcopy/pbpaste), and native
;; Linux (wl-copy/xclip).  tmux paste buffer is an extra channel when in tmux.

(defun +clipboard/pipe-to (text program &rest args)
  "Pipe TEXT to PROGRAM via stdin, silently skipping if PROGRAM is missing."
  (when (and program (file-executable-p program))
    (condition-case err
        (let ((process-connection-type nil))
          (let ((proc (apply #'start-process program nil program args)))
            (process-send-string proc text)
            (process-send-eof proc)))
      (error
       (message "Clipboard copy failed (%s): %s"
                program (error-message-string err))))))

(defun +clipboard/set (text)
  "Put TEXT on the host system clipboard when a backend is available."
  (when (and text (not (string-empty-p text)))
    (let ((pipe (lambda (program &rest args)
                  (apply #'+clipboard/pipe-to text program args))))
      (cond
       ;; WSL → Windows clipboard (clip.exe preferred; PowerShell fallback)
       (+is-wsl
        (let ((clip (+clipboard/wsl-executable "clip.exe"))
              (ps (+clipboard/wsl-executable "powershell.exe")))
          (cond (clip (funcall pipe clip))
                (ps (+clipboard/pipe-to text ps "-NoProfile" "-Command"
                                        "$input = [Console]::In.ReadToEnd(); Set-Clipboard -Value $input")))))
       ((eq system-type 'darwin)
        (when (executable-find "pbcopy")
          (funcall pipe "pbcopy")))
       ((eq system-type 'gnu/linux)
        (cond ((executable-find "wl-copy")
               (funcall pipe "wl-copy"))
              ((executable-find "xclip")
               (funcall pipe "xclip" "-selection" "clipboard"))
              ((executable-find "xsel")
               (funcall pipe "xsel" "--clipboard" "--input"))))))))

(defun +clipboard/get ()
  "Return text from the host system clipboard, or nil if unavailable."
  (ignore-errors
    (let ((clip
           (cond
            ;; WSL → Windows clipboard
            (+is-wsl
             (let ((ps (+clipboard/wsl-executable "powershell.exe")))
               (when ps
                 (shell-command-to-string
                  (format "%s -NoProfile -Command \"Get-Clipboard -Raw\" 2>/dev/null"
                          (shell-quote-argument ps)))))
            ((eq system-type 'darwin)
             (when (executable-find "pbpaste")
               (shell-command-to-string "pbpaste")))
            ((and (eq system-type 'gnu/linux) (executable-find "wl-paste"))
             (shell-command-to-string "wl-paste --no-newline 2>/dev/null"))
            ((and (eq system-type 'gnu/linux) (getenv "DISPLAY"))
             (gui-get-selection 'CLIPBOARD))
            (t
             (gui-get-selection 'CLIPBOARD)))))
      (when (and clip (not (string-empty-p clip)))
        ;; Windows / PowerShell often appends a trailing CRLF.
        (string-trim-right clip "[\r\n]+"))))))

;; Terminal Emacs on WSL: make C-y (yank) read the Windows clipboard.
(when (and +is-wsl (+clipboard/wsl-executable "powershell.exe"))
  (setq interprogram-paste-function '+clipboard/get))

(defun +copy-to-system-clipboard (beg end)
  "Copy visible region text from BEG to END to the system clipboard.
Filters invisible text (e.g. collapsed magit sections).

Writes through every channel that is available:
  - Emacs kill ring                         (always)
  - Windows clipboard via clip.exe          (WSL)
  - macOS system clipboard via `pbcopy'     (darwin)
  - Linux system clipboard via `wl-copy' or
    `xclip' / `xsel'                         (native gnu/linux)
  - tmux paste buffer                       (when running inside tmux)

This is more reliable than relying on Emacs's `select-enable-clipboard'
in terminal Emacs, which goes through OSC 52 and only works if the
host terminal opts into it."
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
    ;; Emacs kill ring + host clipboard
    (kill-new text)
    (+clipboard/set text)
    ;; tmux paste buffer for cross-pane sharing (C-a P to paste elsewhere)
    (when (and (getenv "TMUX") (executable-find "tmux"))
      (+clipboard/pipe-to text "tmux" "load-buffer" "-"))
    (message "Copied %d chars" (length text))))

;;; init-core.el ends here
