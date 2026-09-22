;;; init-pilish.el --- Pi coding agent (Pilish) -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ango Wang

;;; Commentary:
;;
;; Pilish (https://github.com/dnouri/pilish) is an Emacs frontend for Pi
;; (https://pi.dev), a minimal, extensible coding agent.  Pi owns the
;; providers, models, sessions and tools; Pilish renders the conversation
;; as Markdown in one window and the prompt in an ordinary Emacs buffer in
;; another.  Emacs is only the frontend: no terminal, no PTY, no key
;; forwarding.
;;
;; Key surface is intentionally tiny: one prefix, `C-c m', with
;;
;;   C-c m c   create (start or focus) a session in this workspace
;;   C-c m m   browse every previous session (all projects)
;;   C-c m a   pick the agent's model
;;
;; Pilish's own `C-c C-*' bindings are removed so this config's global
;; `C-c' bindings apply inside its buffers; sending is RET in the input
;; buffer's normal state (type, ESC, RET), which is also how a prompt is
;; sent while the agent is busy.  The rest of Pilish is on `M-x pilish-*'.
;;
;; Chat buffer: Pilish's optional Evil integration normally puts the
;; read-only transcript in *motion* state.  We use the normal state instead
;; so h/j/k/l and w motions, H/L beginning/end of line, J/K the 8-line
;; jumps, `v'/`V' visual selection and yank work as everywhere else.  Only
;; the Pilish keys that normal state would shadow and that stay useful in a
;; read-only transcript are re-asserted: `i'/`a' focus the input, RET visits
;; a file at point, TAB folds a tool/thinking block.
;;
;; Input + output: two windows in one frame, chat above and input below,
;; each with its own point and scroll.  Pilish keeps them independently
;; scrollable: a window at the buffer end follows new output, one scrolled
;; up stays put.  The input is pinned to the lower third.
;;
;; Models go through the Vercel AI Gateway provider (`vercel-ai-gateway'):
;; export `AI_GATEWAY_API_KEY', or store the key in `~/.pi/agent/auth.json'
;; under that entry.  OpenCode's store (`~/.local/share/opencode/auth.json',
;; provider id `vercel') is a different file and is NOT read by Pi.
;;
;; The model you pick is remembered: Pi's RPC `set_model' does not persist a
;; default, so on each explicit change we write `defaultProvider' /
;; `defaultModel' into Pi's own `~/.pi/agent/settings.json'.  Pi then applies
;; it to every new session, so `C-c m a' once means the next session starts
;; on that model.
;;
;; This module is deferred, so nothing loads until a key or command is used.

;;; Code:

(require 'json)

;;;; Variables and commands we touch before Pilish loads

(defvar pilish-extra-args)
(defvar pilish-session-browser-default-scope)
(defvar pilish-input-window-display)
(defvar pilish-input-window-height)
(defvar pilish-chat-mode-map)
(defvar pilish-input-mode-map)
(defvar pilish-evil-chat-state)
(defvar pilish--chat-buffer)
(defvar pilish--process)

(declare-function pilish-evil-setup "pilish-evil")
(declare-function pilish-evil-insert-input "pilish-evil")
(declare-function pilish-evil-append-input "pilish-evil")
(declare-function pilish-visit-file "pilish-render")
(declare-function pilish-toggle-tool-section "pilish-render")
(declare-function pilish--update-state-from-response "pilish-core")
(declare-function pilish--browse-switch-session "pilish-browse")
(declare-function pilish--session-live-process-p "pilish-ui")
(declare-function pilish-open-session-file "pilish")
(declare-function evil-define-key* "evil-core")

;; Native Evil state for the chat buffer, set before `pilish-evil' loads.
(setq pilish-evil-chat-state 'normal)

;;;; Options

(defcustom +pilish-provider "vercel-ai-gateway"
  "Provider passed to the `pi' CLI as `--provider'.
Defaults to the Vercel AI Gateway.  Set to nil to let Pi choose."
  :type '(choice (const :tag "Vercel AI Gateway" "vercel-ai-gateway")
                 (const :tag "Let Pi choose" nil)
                 string)
  :group 'tools)

(defcustom +pilish-model nil
  "Model passed to the `pi' CLI as `--model'.
Nil (the default) lets Pi apply the model remembered in its settings."
  :type '(choice (const :tag "Use remembered model" nil) string)
  :group 'tools)

(defcustom +pilish-remember-model t
  "When non-nil, remember the last model picked as Pi's next default.
The provider and model are written to Pi's global
`~/.pi/agent/settings.json' (`defaultProvider' / `defaultModel'), the same
keys Pi's own TUI writes.  Pi, not Emacs, then applies it on the next
session."
  :type 'boolean
  :group 'tools)

(defun +pilish--extra-args ()
  "Return the `--provider'/`--model' args for `pilish-extra-args'."
  (append (when +pilish-provider (list "--provider" +pilish-provider))
          (when +pilish-model (list "--model" +pilish-model))))

;;;; Pi's data directory and the remembered model

(defun +pilish/agent-directory ()
  "Return Pi's data directory, honouring `PI_CODING_AGENT_DIR'."
  (file-name-as-directory
   (expand-file-name (or (getenv "PI_CODING_AGENT_DIR") "~/.pi/agent"))))

(defun +pilish--settings-file ()
  "Return Pi's global settings file."
  (expand-file-name "settings.json" (+pilish/agent-directory)))

(defun +pilish--persist-model (provider id)
  "Store PROVIDER and ID as Pi's global default model.
Read-modify-write `settings.json', preserving every other key, and
replace the file atomically with mode 0600."
  (let* ((file (+pilish--settings-file))
         (settings (and (file-readable-p file)
                        (condition-case nil
                            (json-read-file file)
                          (error nil))))
         (settings (if (consp settings) settings nil))
         (temp (make-temp-file (expand-file-name "settings-" (file-name-directory file))
                               nil ".json")))
    (setf (alist-get 'defaultProvider settings) provider)
    (setf (alist-get 'defaultModel settings) id)
    (unwind-protect
        (progn
          (with-temp-file temp
            (insert (json-encode settings))
            (insert "\n"))
          (set-file-modes temp #o600)
          (rename-file temp file t))
      (when (file-exists-p temp)
        (ignore-errors (delete-file temp))))
    (message "Pi: default model is now %s/%s" provider id)))

(defun +pilish--remember-model (response &rest _)
  "Remember the model a successful model change selected.
Runs as :after advice on `pilish--update-state-from-response'."
  (when (and +pilish-remember-model
             (eq (plist-get response :success) t))
    (let* ((command (plist-get response :command))
           (data (plist-get response :data))
           (model (if (equal command "cycle_model")
                      (plist-get data :model)
                    data)))
      (when (member command '("set_model" "cycle_model"))
        (let ((provider (plist-get model :provider))
              (id (plist-get model :id)))
          (when (and provider id)
            (ignore-errors (+pilish--persist-model provider id))))))))

(defun +pilish--install-advice ()
  "Install the two Pilish advices this module relies on."
  (unless (advice-member-p #'+pilish--remember-model
                           'pilish--update-state-from-response)
    (advice-add 'pilish--update-state-from-response
                :after #'+pilish--remember-model))
  (unless (advice-member-p #'+pilish--browse-switch-session
                           'pilish--browse-switch-session)
    (advice-add 'pilish--browse-switch-session
                :around #'+pilish--browse-switch-session)))

;;;; Open a browsed session when none is live

(defun +pilish--browse-switch-session (orig path &rest args)
  "Switch to PATH, opening it when no live session is linked.
Pilish's session browser reads its archive from disk with no live
process, but its RET guard (`pilish--browse-switch-session') requires a
live linked chat buffer and otherwise signals \"No pi session to switch
to\".  When there is no live session, open the session file directly
instead, so `C-c m m' then RET works without starting a throwaway
session first."
  (let* ((chat-buf (and (boundp 'pilish--chat-buffer) pilish--chat-buffer))
         (proc (and (buffer-live-p chat-buf)
                    (buffer-local-value 'pilish--process chat-buf))))
    (if (and (stringp path)
             (not (and (buffer-live-p chat-buf)
                       (pilish--session-live-process-p proc))))
        (pilish-open-session-file path)
      (apply orig path args))))

;;;; Remove Pilish's own prefix keys

(defvar +pilish-strip-keys
  '("C-c C-c" "C-c C-s" "C-c C-k" "C-c C-p" "C-c C-r"
    "C-c C-n" "C-c C-e" "C-c C-m" "C-c C-t" "C-c C-y")
  "Pilish in-buffer keys removed so this config's globals apply.
Send is RET in the input buffer's normal state; the commands remain on
`M-x pilish-*'.")

(defun +pilish--strip-default-keys ()
  "Unbind Pilish's `C-c C-*' keys from the chat and input keymaps."
  (dolist (map (list pilish-chat-mode-map pilish-input-mode-map))
    (dolist (key +pilish-strip-keys)
      (ignore-errors (define-key map (kbd key) nil)))))

;;;; Native Evil chat buffer

(defun +pilish--setup-evil ()
  "Make the chat buffer a native Evil buffer.
Use the configured normal state so H/L/J/K, w motions, and visual
selection/yank work as everywhere else, then re-assert the few Pilish
keys that normal state would otherwise shadow and that remain useful in a
read-only transcript: `i'/`a' focus the input, RET visits a file at point,
TAB folds a tool/thinking block."
  (when (and (featurep 'evil) (fboundp 'pilish-evil-setup))
    (setq pilish-evil-chat-state 'normal)
    (pilish-evil-setup)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal pilish-chat-mode-map
        "i" #'pilish-evil-insert-input
        "a" #'pilish-evil-append-input
        (kbd "RET") #'pilish-visit-file
        (kbd "TAB") #'pilish-toggle-tool-section
        [tab] #'pilish-toggle-tool-section))))

;;;; Pilish

(use-package pilish
  :straight t
  :commands (pilish
             pilish-session-browser
             pilish-select-model)
  :bind (("C-c m c" . pilish)
         ("C-c m m" . pilish-session-browser)
         ("C-c m a" . pilish-select-model))
  :config
  ;; Vercel AI Gateway; `C-c m m' spans every project.
  (setq pilish-extra-args (+pilish--extra-args)
        pilish-session-browser-default-scope 'all)
  ;; Chat + input stay visible together; input takes the lower third, and
  ;; each window keeps its own point and scroll.
  (setq pilish-input-window-display 'always
        pilish-input-window-height 0.3)
  (defalias 'pi 'pilish)
  (+pilish--strip-default-keys)
  (+pilish--install-advice)
  ;; `pilish-evil' loads lazily on the first session; apply, and re-apply if
  ;; this module is evaluated in a session that already loaded it.
  (with-eval-after-load 'pilish-evil
    (+pilish--setup-evil))
  (when (featurep 'pilish-evil)
    (+pilish--setup-evil)))

(provide 'init-pilish)
;;; init-pilish.el ends here
