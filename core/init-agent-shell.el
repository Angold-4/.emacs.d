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
;;
;; Inside a session (agent-shell-mode):
;;
;;   C-c k m     switch model
;;   C-c k r     resume the most recent session in this workspace
;;   C-c k R     pick a session in this workspace to resume
;;   C-c k n     new session
;;   C-c k s     switch to another agent-shell buffer
;;   C-c k i     interrupt
;;
;; ACP scopes `session/list' to the workspace cwd (OpenCode filters by it),
;; so r/R are workspace-scoped; there is no cross-workspace list to pick from.
;;
;; The in-buffer top bar shows only what matters here: model, PR id (from the
;; git workbench's cached forge snapshots), branch, and context window left.
;; agent-shell's own SVG header and mode-line copy are disabled for this.
;;
;; Evil: sessions start in insert state.  RET inserts a newline in insert
;; state; C-<return> or M-RET sends.  Normal-state RET sends and the usual
;; motions work over the transcript.
;;
;; Slash commands: ACP only exposes the agent's *skills* (OpenCode advertises
;; `/delegate', `/review', `/init', ...), not TUI commands such as `/sessions'
;; or `/models'.  Use the `C-c k' bindings above for those.
;;
;; Claude Code needs its ACP bridge once:
;;   npm install -g @agentclientprotocol/claude-agent-acp
;; OpenCode and Claude Code both reuse their existing CLI logins
;; (`opencode auth login' / the `claude' CLI), so no keys live in this file.

;;; Code:

(require 'project)

(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-get-model-name "agent-shell" (state))
(declare-function agent-shell-set-session-model "agent-shell" (&optional on-success))
(declare-function agent-shell-switch-buffer "agent-shell" ())
(declare-function agent-shell-new-shell "agent-shell" ())
(declare-function agent-shell-interrupt "agent-shell" (&optional force))
(declare-function agent-shell--format-number-compact "agent-shell" (number))
(declare-function agent-shell--context-usage-face "agent-shell-usage" (percentage))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function +git-store-context-for-root "init-git-store" (root))
(declare-function +git-store-local-context-repository-id "init-git-store" (context))
(declare-function +git-pr--branch-name "init-git-pr" (root))
(declare-function +forge-prs-for-head-ref "init-forge" (repository-id head-ref))
(declare-function +forge-pr-snapshot-number "init-forge" (snapshot))

;; =============================================================================
;; Package
;; =============================================================================

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

  ;; We render our own top bar (see `+agent-shell/setup').
  (setq agent-shell-header-style nil)

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

;; =============================================================================
;; Top bar: model | PR | branch | context left
;; =============================================================================

(defvar-local +agent-shell--git-cache nil
  "Cons of DEFAULT-DIRECTORY and its (:branch :pr) plist.
Cached so the header line does not hit Git or forge on every redisplay.")

(defun +agent-shell--git-info (directory)
  "Return a (:branch BRANCH :pr NUMBER) plist for DIRECTORY, or nil."
  (when-let ((root (ignore-errors (magit-toplevel directory))))
    (when-let* ((context (ignore-errors (+git-store-context-for-root root)))
                (repository (+git-store-local-context-repository-id context))
                (branch (ignore-errors (+git-pr--branch-name root))))
      (list :branch branch
            :pr (when (fboundp '+forge-prs-for-head-ref)
                  (when-let ((prs (ignore-errors
                                    (+forge-prs-for-head-ref repository branch))))
                    (+forge-pr-snapshot-number (car prs))))))))

(defun +agent-shell--git-info-cached ()
  "Return `+agent-shell--git-info' for the buffer, memoised by directory."
  (let ((directory (expand-file-name default-directory)))
    (unless (and +agent-shell--git-cache
                 (equal (car +agent-shell--git-cache) directory))
      (setq +agent-shell--git-cache
            (cons directory (+agent-shell--git-info directory))))
    (cdr +agent-shell--git-cache)))

(defun +agent-shell--model ()
  "Return the current model name, or nil."
  (ignore-errors (agent-shell-get-model-name (agent-shell--state))))

(defun +agent-shell--context-left ()
  "Return a propertised \"N left (P%)\" string, or nil when unknown."
  (when-let* ((state (ignore-errors (agent-shell--state)))
              (usage (map-elt state :usage))
              (used (map-elt usage :context-used))
              (size (map-elt usage :context-size))
              ((numberp used))
              ((numberp size))
              ((> size 0)))
    (let* ((used-percent (/ (* 100.0 used) size))
           (left-percent (/ (* 100.0 (- size used)) size)))
      (propertize (format "%s left (%.0f%%)"
                          (agent-shell--format-number-compact (- size used))
                          left-percent)
                  'face (agent-shell--context-usage-face used-percent)
                  'help-echo (format "Context: %s / %s tokens used"
                                     (agent-shell--format-number-compact used)
                                     (agent-shell--format-number-compact size))))))

(defun +agent-shell/header-line ()
  "Return the agent-shell top bar: model, PR, branch, context left."
  (let* ((git-info (+agent-shell--git-info-cached))
         (parts (delq nil
                      (list (when-let ((model (+agent-shell--model)))
                              (propertize model 'face 'agent-shell-model
                                          'help-echo "Current model"))
                            (when-let ((pr (plist-get git-info :pr)))
                              (propertize (format "PR #%d" pr)
                                          'face 'agent-shell-session-title))
                            (when-let ((branch (plist-get git-info :branch)))
                              (propertize branch 'face 'agent-shell-session-directory))
                            (+agent-shell--context-left)))))
    (if parts
        (concat " " (mapconcat #'identity parts "  ·  ") " ")
      "")))

;; =============================================================================
;; Session commands (C-c k)
;; =============================================================================

(defun +agent-shell--opencode-with-strategy (strategy)
  "Start an OpenCode session using session STRATEGY."
  (let ((agent-shell-session-strategy strategy))
    (require 'agent-shell)
    (require 'agent-shell-opencode)
    (agent-shell-opencode-start-agent)))

(defun +agent-shell/resume-latest ()
  "Resume the most recent OpenCode session in this workspace."
  (interactive)
  (+agent-shell--opencode-with-strategy 'latest))

(defun +agent-shell/resume-choose ()
  "Pick an OpenCode session in this workspace to resume."
  (interactive)
  (+agent-shell--opencode-with-strategy 'prompt))

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

;; =============================================================================
;; Install
;; =============================================================================

(defun +agent-shell/setup ()
  "Install the top bar in agent-shell buffers."
  (setq-local header-line-format '(:eval (+agent-shell/header-line))))

(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-mode-hook #'+agent-shell/setup)
  ;; Our top bar replaces agent-shell's mode-line copy of the same data.
  (when (fboundp 'agent-shell--setup-modeline)
    (advice-add 'agent-shell--setup-modeline :override #'ignore))
  (define-key agent-shell-mode-map (kbd "C-c k m") #'agent-shell-set-session-model)
  (define-key agent-shell-mode-map (kbd "C-c k r") #'+agent-shell/resume-latest)
  (define-key agent-shell-mode-map (kbd "C-c k R") #'+agent-shell/resume-choose)
  (define-key agent-shell-mode-map (kbd "C-c k n") #'+agent-shell/new)
  (define-key agent-shell-mode-map (kbd "C-c k s") #'+agent-shell/switch)
  (define-key agent-shell-mode-map (kbd "C-c k i") #'+agent-shell/interrupt))

;; =============================================================================
;; Launchers
;; =============================================================================

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

(defvar +agent-shell/command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'+agent-shell/opencode)
    (define-key map (kbd "c") #'+agent-shell/claude)
    (define-key map (kbd "s") #'+agent-shell/switch)
    (define-key map (kbd "n") #'+agent-shell/new)
    (define-key map (kbd "m") #'+agent-shell/model)
    (define-key map (kbd "r") #'+agent-shell/resume-latest)
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
