;;; init-agent-shell.el --- AI agents as native Emacs buffers -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ango Wang

;;; Commentary:
;;
;; OpenCode and Claude Code speak the Agent Client Protocol, and agent-shell
;; (xenodium/agent-shell) drives ACP agents from an ordinary Emacs buffer via
;; shell-maker/comint.  The transcript is real buffer text, so Evil motions,
;; visual selection, G, yank, search and narrowing all work without a
;; terminal, an alternate screen, or any key forwarding.
;;
;;   C-c o       OpenCode    (opencode acp)
;;   C-c O       Claude Code (claude-agent-acp)
;;   M-x ashell  the default agent (OpenCode)
;;   C-c A ...   session commands: o=opencode c=claude s=switch n=new
;;               m=model i=interrupt
;;
;; Evil: sessions start in insert state for typing.  In insert state RET adds
;; a newline and C-<return> (or M-RET) sends the prompt; in normal state RET
;; sends it, and the usual motions/visual selection work over the transcript.
;;
;; Slash commands: ACP only exposes the agent's *skills* as `/commands'
;; (verify with `M-x agent-shell`'s "Available /commands" section).  OpenCode
;; advertises its skills (e.g. `/delegate`, `/review`, `/init`), but the
;; OpenCode TUI commands such as `/sessions`, `/models` or `/new` are not part
;; of ACP and cannot be typed here.  Use the agent-shell equivalents instead:
;;
;;   sessions  `C-c A s' / `M-x agent-shell-switch-buffer',
;;             `M-x agent-shell-resume-session',
;;             or `C-u C-u M-x agent-shell' to pick a shell
;;   new       `C-c A n' / `M-x agent-shell-new-shell'
;;   model     `C-c A m' / `C-c C-v' inside a session
;;   mode      `C-c C-m' inside a session
;;   stop      `C-c A i' / `C-c C-c' inside a session
;;
;; Claude Code needs the ACP bridge once:
;;   npm install -g @agentclientprotocol/claude-agent-acp
;; OpenCode and Claude Code both reuse their existing CLI logins
;; (`opencode auth login' / the `claude' CLI), so no keys live in this file.

;;; Code:

(use-package agent-shell
  :straight t
  :commands (agent-shell)
  :config
  ;; OpenCode is the day-to-day agent here.
  (setq agent-shell-preferred-agent-config 'opencode)
  ;; `opencode' is not on a fresh login shell's PATH.
  (add-to-list 'exec-path (expand-file-name "~/.opencode/bin/"))
  ;; Reuse the `claude' CLI's subscription login.
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; Evil: type in insert state, read/yank the transcript in normal state.
  (evil-set-initial-state 'agent-shell-mode 'insert)
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'insert agent-shell-mode-map (kbd "C-<return>") #'comint-send-input)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)
  ;; Diff review buffers read better in Emacs state, where y/n/p/q send directly.
  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state)))))

(defun +agent-shell/opencode ()
  "Open an OpenCode ACP session in a native Emacs buffer."
  (interactive)
  (require 'agent-shell)
  (require 'agent-shell-opencode)
  (agent-shell-opencode-start-agent))

(defun +agent-shell/claude ()
  "Open a Claude Code ACP session in a native Emacs buffer."
  (interactive)
  (require 'agent-shell)
  (require 'agent-shell-anthropic)
  (agent-shell-anthropic-start-claude-code))

(defun +agent-shell/switch ()
  "Switch between existing agent-shell buffers."
  (interactive)
  (require 'agent-shell)
  (call-interactively #'agent-shell-switch-buffer))

(defun +agent-shell/new ()
  "Start another agent-shell session."
  (interactive)
  (require 'agent-shell)
  (call-interactively #'agent-shell-new-shell))

(defun +agent-shell/model ()
  "Choose the model for the current agent-shell session."
  (interactive)
  (require 'agent-shell)
  (call-interactively #'agent-shell-set-session-model))

(defun +agent-shell/interrupt ()
  "Interrupt the current agent-shell session."
  (interactive)
  (require 'agent-shell)
  (call-interactively #'agent-shell-interrupt))

(defvar +agent-shell/command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'+agent-shell/opencode)
    (define-key map (kbd "c") #'+agent-shell/claude)
    (define-key map (kbd "s") #'+agent-shell/switch)
    (define-key map (kbd "n") #'+agent-shell/new)
    (define-key map (kbd "m") #'+agent-shell/model)
    (define-key map (kbd "i") #'+agent-shell/interrupt)
    map)
  "Prefix map for agent-shell session commands.")

;; Short names for M-x.
(defalias 'ashell 'agent-shell)
(defalias 'ashell-claude '+agent-shell/claude)

(global-set-key (kbd "C-c o") #'+agent-shell/opencode)
(global-set-key (kbd "C-c O") #'+agent-shell/claude)
(global-set-key (kbd "C-c A") +agent-shell/command-map)

(provide 'init-agent-shell)
;;; init-agent-shell.el ends here
