;;; init-completion.el --- Completion framework -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Completion configuration with Vertico, Orderless, and Company

;;; Commentary:
;; This module configures the completion framework:
;; - Vertico: Vertical minibuffer completion UI (for C-p, C-x b, M-x, etc.)
;; - Orderless: Fuzzy/multi-component matching (type "binance ts" to find binance.ts)
;; - Marginalia: Rich annotations in completion candidates
;; - Company: In-buffer popup completion (for code, LSP, etc.)

;;; Code:

;; =============================================================================
;; Vertico (Vertical Completion UI)
;; =============================================================================

;; Vertico enhances the built-in completing-read with a vertical list.
;; Everything that uses completing-read benefits: projectile, xref, M-x, etc.
(use-package vertico
  :straight t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-count 15           ; Show 15 candidates
        vertico-cycle t            ; Cycle at top/bottom
        vertico-resize nil))       ; Fixed height (no resizing)

;; =============================================================================
;; Orderless (Fuzzy/Multi-component Matching)
;; =============================================================================

;; Orderless lets you type space-separated patterns in any order.
;; e.g., "binance ts" matches "hybrid/src/libs/exchanges/core/binance.ts"
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; =============================================================================
;; Marginalia (Rich Annotations)
;; =============================================================================

;; Marginalia adds helpful annotations next to completion candidates:
;; file sizes, docstrings, keybindings, etc.
(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode))

;; =============================================================================
;; Company Mode
;; =============================================================================

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("RET" . company-complete-selection)
              ("<return>" . company-complete-selection))
  :config
  (setq
   ;; Popup settings
   company-tooltip-align-annotations t
   company-tooltip-limit 10
   company-tooltip-minimum-width 40
   
   ;; Delay settings (responsive but not annoying)
   company-idle-delay 0.2
   company-echo-delay (if (display-graphic-p) nil 0)
   company-minimum-prefix-length 2
   
   ;; Don't require exact match
   company-require-match nil
   
   ;; Case handling
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   
   ;; Disable in some modes
   company-global-modes '(not erc-mode
                              message-mode
                              help-mode
                              gud-mode
                              eshell-mode
                              shell-mode
                              vterm-mode)
   
   ;; Backend configuration
   ;; capf = completion-at-point-functions (integrates with LSP)
   company-backends '((company-capf :with company-yasnippet)
                      (company-dabbrev-code company-keywords company-files)
                      company-dabbrev)
   
   ;; Appearance
   company-format-margin-function nil
   
   ;; Sorting
   company-transformers '(company-sort-prefer-same-case-prefix
                          company-sort-by-occurrence
                          company-sort-by-backend-importance)))

;; =============================================================================
;; YASnippet (optional snippets)
;; =============================================================================

(use-package yasnippet
  :straight t
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (setq yas-wrap-around-region t)

  ;; Suppress noisy warnings from snippet conditions that reference
  ;; uninstalled packages (e.g., js2-mode's `js2-node-type' in js-mode
  ;; snippets).  yasnippet-snippets ships js-mode/js2-mode snippets whose
  ;; #condition calls js2-mode functions; these fail in typescript buffers
  ;; because TS modes inherit js-mode snippets via the parent-mode chain.
  ;; The condition evaluator already catches the error and returns nil,
  ;; but it logs a noisy message via `yas--message' that floods the echo
  ;; area and can interfere with company popup display.
  (setq yas-verbosity 0)

  (yas-reload-all))

;; Common snippets collection
(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; =============================================================================
;; Buffer Filtering Utilities
;; =============================================================================

;; These patterns define "internal" buffers that are typically not useful
;; to switch to. Used by +buffer/switch-persp in init-tools.el
(defvar +buffer/hidden-patterns
  '("\\` "                    ; Buffers starting with space (internal)
    "\\`\\*Echo Area"         ; Echo areas
    "\\`\\*Minibuf"           ; Minibuffers  
    "\\`\\*code-conversion"   ; Encoding work buffers
    "\\`\\*Completions\\*"    ; Completion buffer
    "\\`\\*Async-native"      ; Native compilation logs
    "\\`\\*Backtrace\\*"      ; Backtrace buffer
    "\\`\\*Compile-Log\\*"    ; Compile logs
    "\\`\\*straight-"         ; Straight.el buffers
    "-comint-indirect\\*\\'") ; Comint indirect buffers
  "List of regex patterns for internal buffers.")

(defun +buffer/internal-p (buffer)
  "Return t if BUFFER is an internal Emacs buffer."
  (let ((name (if (bufferp buffer) (buffer-name buffer) buffer)))
    (cl-some (lambda (pattern) (string-match-p pattern name))
             +buffer/hidden-patterns)))

;; =============================================================================
;; IBuffer (better buffer list for C-x C-b)
;; =============================================================================

(use-package ibuffer
  :straight (:type built-in)
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Group buffers by project/type
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Files" (predicate buffer-file-name))
           ("Dired" (mode . dired-mode))
           ("Shell" (or (mode . shell-mode)
                        (mode . eshell-mode)
                        (mode . term-mode)
                        (mode . vterm-mode)))
           ("Magit" (name . "^magit"))
           ("Org" (mode . org-mode))
           ("LSP" (name . "^\\*lsp"))
           ("Help" (or (mode . help-mode)
                       (mode . helpful-mode)))
           ("Internal" (name . "^\\*")))))
  
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  
  ;; Don't show empty groups
  (setq ibuffer-show-empty-filter-groups nil)
  
  ;; Human-readable size column
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process))))

;;; init-completion.el ends here
