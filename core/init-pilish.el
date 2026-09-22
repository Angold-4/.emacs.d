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
;; One dedicated prefix, `C-c m', mirrors the OpenCode integration with a
;; deliberately small surface:
;;
;;   C-c m c   create (start or focus) a session in this workspace
;;   C-c m m   browse every previous session (all projects)
;;   C-c m a   pick the agent's model
;;
;; Everything else Pilish offers stays on its own keys (C-c C-c send,
;; C-c C-s steering, C-c C-k abort, C-c C-p menu, C-c C-r sessions) and the
;; `M-x pilish-*' commands.  This module is deferred, so nothing loads until
;; one of the keys or commands above is used.

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
  ;; Point Pi at the Vercel AI Gateway; `C-c m m' spans every project.
  (setq pilish-extra-args (+pilish--extra-args)
        pilish-session-browser-default-scope 'all)
  (defalias 'pi 'pilish))

(provide 'init-pilish)
;;; init-pilish.el ends here
