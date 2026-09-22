;;; init-opencode.el --- OpenCode sessions as native Emacs buffers -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ango Wang

;;; Commentary:
;;
;; OpenCode runs headless (`opencode serve') and sczi/opencode.el
;; (https://codeberg.org/sczi/opencode.el) drives it from ordinary Emacs
;; buffers: a vtable session manager plus comint chat sessions with markdown,
;; diff-mode, permissions and the question tool.  Everything is real buffer
;; text, so Evil motions, visual selection, yank, search and narrowing work,
;; and the modeline shows the agent, model, variant and context usage.
;;
;;   :opencode / M-x +opencode/open / C-c o   session manager for this project
;;
;; The package and its `plz' dependencies are not on MELPA, so all four are
;; pinned here.  OpenCode itself is found via `~/.opencode/bin'.
;;
;; This replaces the old vterm-based `init-opencode.el' (never registered in
;; `+init-modules').

;;; Code:

(require 'project)

;; Keep the executable reachable before anything tries to resolve it.
(add-to-list 'exec-path (expand-file-name "~/.opencode/bin/"))

;; sczi/opencode.el needs the plz stack; none of it is on MELPA, so pin the
;; recipes before the package below resolves its dependencies.
(use-package plz
  :straight (:type git :host github :repo "alphapapa/plz.el"))

(use-package plz-media-type
  :straight (:type git :host github :repo "r0man/plz-media-type"))

(use-package plz-event-source
  :straight (:type git :host github :repo "r0man/plz-event-source"))

(use-package opencode
  :straight (:type git :host codeberg :repo "sczi/opencode.el")
  :commands (opencode opencode-connect opencode-select-project
             opencode-new-session opencode-select-open-session
             opencode-visit-last-idle opencode-add-buffer-dwim
             opencode-add-region opencode-add-file-dwim)
  :config
  (setq opencode-command (or (executable-find "opencode") "opencode"))
  ;; Start a headless server on demand when none is running.
  (setq opencode-auto-start-server t))

(defun +opencode/open ()
  "Open the OpenCode session manager for the current project."
  (interactive)
  (require 'opencode)
  (call-interactively #'opencode))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "opencode" #'+opencode/open)
  (evil-ex-define-cmd "oc" #'+opencode/open))

(global-set-key (kbd "C-c o") #'+opencode/open)

(provide 'init-opencode)
;;; init-opencode.el ends here
