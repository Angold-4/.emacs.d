;;; init-pilish.el --- Pi coding agent (Pilish) -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ango Wang

;;; Commentary:
;;
;; Pilish (https://github.com/dnouri/pilish) is an Emacs frontend for Pi
;; (https://pi.dev), a minimal, extensible coding agent.  Pi owns the
;; providers, models, sessions and tools; Pilish renders the conversation
;; as Markdown in one window and the prompt in an ordinary Emacs buffer in
;; another.  Emacs is only the frontend: no terminal, no PTY, no key
;; forwarding, so Evil motions, yank, search and narrowing keep working.
;;
;; Pilish launches the `pi' CLI in RPC mode as a subprocess of Emacs;
;; credentials therefore live with Pi, not in this repository.  Models are
;; served through the Vercel AI Gateway provider (`vercel-ai-gateway'):
;; either export `AI_GATEWAY_API_KEY', or store the key in Pi's own
;; credential file `~/.pi/agent/auth.json' under the `vercel-ai-gateway'
;; entry.  Note that OpenCode's store (`~/.local/share/opencode/auth.json',
;; provider id `vercel') is a different file and is NOT read by Pi.
;; `+pilish-provider' and `+pilish-model' forward `--provider'/`--model' to
;; the CLI; set either to nil to let Pi choose.
;;
;; One dedicated prefix, `C-c m', mirrors the OpenCode integration:
;;
;;   C-c m m   global session browser (all projects)
;;   C-c m c   start or focus a session in this workspace
;;   C-c m i   focus this session's input buffer
;;   C-c m o   hide/show this project's session windows
;;   C-c m M   select model
;;   C-c m a   select model (provider-aware picker)
;;   C-c m v   select thinking level
;;   C-c m n   start a new session (reset)
;;   C-c m s   export this session to HTML
;;   C-c m d   open Pi's sessions directory
;;   C-c m r   reload the pi process
;;   C-c m t   conversation tree browser
;;   C-c m q   close this session
;;
;; Pilish binds its own in-buffer keys (C-c C-c send, C-c C-s steering,
;; C-c C-k abort, C-c C-p menu, C-c C-r sessions); see its README.  This
;; module is deferred, so nothing loads until a command or key above is
;; used.

;;; Code:

;;;; Vercel AI Gateway

(defcustom +pilish-provider "vercel-ai-gateway"
  "Provider passed to the `pi' CLI as `--provider'.
Defaults to the Vercel AI Gateway.  Set to nil to let Pi choose; its
model picker still lists every authenticated provider."
  :type '(choice (const :tag "Vercel AI Gateway" "vercel-ai-gateway")
                 (const :tag "Let Pi choose" nil)
                 string)
  :group 'tools)

(defcustom +pilish-model nil
  "Model passed to the `pi' CLI as `--model'.
Nil lets Pi use its own default.  Use a model id from the selected
provider, for example \"openai/gpt-5-mini\" for the Vercel AI Gateway."
  :type '(choice (const :tag "Pi default" nil) string)
  :group 'tools)

(defun +pilish--extra-args ()
  "Return the `--provider'/`--model' args for `pilish-extra-args'."
  (append (when +pilish-provider (list "--provider" +pilish-provider))
          (when +pilish-model (list "--model" +pilish-model))))

;;;; Sessions directory

(defun +pilish/agent-directory ()
  "Return Pi's data directory, honouring `PI_CODING_AGENT_DIR'."
  (file-name-as-directory
   (expand-file-name (or (getenv "PI_CODING_AGENT_DIR") "~/.pi/agent"))))

(defun +pilish/open-sessions-directory ()
  "Open Pi's session archive directory in Dired."
  (interactive)
  (let ((dir (expand-file-name "sessions/" (+pilish/agent-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (dired dir)))

;;;; Pilish

(use-package pilish
  :straight t
  :commands (pilish
             pilish-open-input
             pilish-toggle
             pilish-new-session
             pilish-session-browser
             pilish-tree-browser
             pilish-select-model
             pilish-select-thinking
             pilish-export-html
             pilish-reload
             pilish-quit)
  :bind (("C-c m m" . pilish-session-browser)
         ("C-c m c" . pilish)
         ("C-c m i" . pilish-open-input)
         ("C-c m o" . pilish-toggle)
         ("C-c m M" . pilish-select-model)
         ("C-c m a" . pilish-select-model)
         ("C-c m v" . pilish-select-thinking)
         ("C-c m n" . pilish-new-session)
         ("C-c m s" . pilish-export-html)
         ("C-c m d" . +pilish/open-sessions-directory)
         ("C-c m r" . pilish-reload)
         ("C-c m t" . pilish-tree-browser)
         ("C-c m q" . pilish-quit))
  :config
  ;; Point Pi at the Vercel AI Gateway and let `C-c m m' span every project.
  (setq pilish-extra-args (+pilish--extra-args)
        pilish-session-browser-default-scope 'all)
  (defalias 'pi 'pilish))

(provide 'init-pilish)
;;; init-pilish.el ends here
