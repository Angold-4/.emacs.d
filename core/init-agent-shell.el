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
;;   C-c o   OpenCode    (opencode acp)
;;   C-c O   Claude Code (claude-agent-acp)
;;
;; M-x agent-shell prompts for any ACP agent found on PATH.
;;
;; Claude Code needs the ACP bridge once:
;;   npm install -g @agentclientprotocol/claude-agent-acp
;; OpenCode and Claude Code both reuse their existing CLI logins
;; (`opencode auth login` / the `claude` CLI), so no keys live in this file.

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
        (agent-shell-anthropic-make-authentication :login t)))

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

(global-set-key (kbd "C-c o") #'+agent-shell/opencode)
(global-set-key (kbd "C-c O") #'+agent-shell/claude)

(provide 'init-agent-shell)
;;; init-agent-shell.el ends here
