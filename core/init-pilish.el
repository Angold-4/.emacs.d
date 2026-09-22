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
;; Chat buffer: Pilish ships its own Evil integration that puts the
;; read-only transcript in *motion* state.  We override that to the normal
;; state so the chat behaves like any other buffer under this config's Evil
;; setup: h/j/k/l and w motions, H/L beginning/end of line, J/K the 8-line
;; jumps, `v'/`V' visual selection, and yank.  A few Pilish keys that normal
;; state would shadow and that still matter in a read-only transcript are
;; re-asserted: `i'/`a' focus the input, RET visits a file at point, TAB
;; folds a tool/thinking block.  (`n'/`p' message motion and `f' fork fall
;; to Evil's native n/p/f; use `M-x pilish-next-message' etc. if wanted.)
;;
;; Input + output: two windows, one frame, chat on top and input below,
;; each with its own point and scroll.  Pilish already keeps them
;; independently scrollable: a window left at the buffer end follows new
;; output, while a window you scrolled up in stays put.  We pin the layout
;; to always show both and give the input a fraction of the height, so the
;; pair reads as one unit.  No single merged buffer: the chat is read-only
;; rendered Markdown (tree-sitter, foldable tool sections), while the input
;; is editable text -- merging them would fight that model, and Emacs
;; already gives per-window scrolling.
;;
;; Models go through the Vercel AI Gateway provider (`vercel-ai-gateway'):
;; either export `AI_GATEWAY_API_KEY', or store the key in Pi's own
;; credential file `~/.pi/agent/auth.json' under that entry.  OpenCode's
;; store (`~/.local/share/opencode/auth.json', provider id `vercel') is a
;; different file and is NOT read by Pi.  `+pilish-provider' and
;; `+pilish-model' forward `--provider'/`--model' to the CLI.
;;
;; One dedicated prefix, `C-c m', a deliberately small surface:
;;
;;   C-c m c   create (start or focus) a session in this workspace
;;   C-c m m   browse every previous session (all projects)
;;   C-c m a   pick the agent's model
;;
;; This module is deferred, so nothing loads until one of the keys or
;; commands above is used.

;;; Code:

;;;; Package options we set before Pilish loads

(defvar pilish-extra-args)
(defvar pilish-session-browser-default-scope)
(defvar pilish-input-window-display)
(defvar pilish-input-window-height)
(defvar pilish-chat-mode-map)
(defvar pilish-evil-chat-state)

(declare-function pilish-evil-setup "pilish-evil")
(declare-function pilish-evil-insert-input "pilish-evil")
(declare-function pilish-evil-append-input "pilish-evil")
(declare-function pilish-visit-file "pilish-render")
(declare-function pilish-toggle-tool-section "pilish-render")
(declare-function evil-define-key* "evil-core")

;; Native Evil state for the chat buffer, set before `pilish-evil' loads.
(setq pilish-evil-chat-state 'normal)

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

;;;; Native Evil chat buffer

(defun +pilish--setup-evil ()
  "Make the chat buffer a native Evil buffer.
Use the configured normal state so H/L/J/K, w motions, and visual
selection/ yank work as everywhere else, then re-assert the few Pilish
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
  ;; `pilish-evil' loads lazily on the first session; apply, and re-apply if
  ;; this module is evaluated in a session that already loaded it.
  (with-eval-after-load 'pilish-evil
    (+pilish--setup-evil))
  (when (featurep 'pilish-evil)
    (+pilish--setup-evil)))

(provide 'init-pilish)
;;; init-pilish.el ends here
