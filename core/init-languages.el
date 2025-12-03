;;; init-languages.el --- Language-specific settings -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Programming language modes and tree-sitter configuration

;;; Commentary:
;; This module configures:
;; - Tree-sitter for syntax highlighting (Emacs 29+)
;; - Language-specific major modes
;; - Indentation settings per language
;; - Formatters
;;
;; Note: LSP is configured separately in init-lsp.el and must be
;; started manually with C-x l

;;; Code:

;; =============================================================================
;; Tree-sitter (Emacs 29+ built-in)
;; =============================================================================

;; Emacs 29+ has built-in tree-sitter support
;; We configure it to auto-remap major modes to their -ts-mode variants

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  
  ;; Auto-install grammars when needed
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml       . ("https://github.com/ikatyang/tree-sitter-toml"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  
  ;; Function to install all grammars
  (defun +treesit/install-all-grammars ()
    "Install all tree-sitter grammars defined in `treesit-language-source-alist'."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (let ((lang (car grammar)))
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar: %s" lang)
          (treesit-install-language-grammar lang)))))
  
  ;; Auto-remap to tree-sitter modes when grammar is available
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (css-mode        . css-ts-mode)
          (go-mode         . go-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode         . js-ts-mode)
          (json-mode       . json-ts-mode)
          (python-mode     . python-ts-mode)
          (sh-mode         . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode       . yaml-ts-mode))))

;; =============================================================================
;; C/C++
;; =============================================================================

(use-package cc-mode
  :straight (:type built-in)
  :hook ((c-mode c++-mode) . (lambda ()
                               (setq c-basic-offset 2
                                     tab-width 2
                                     indent-tabs-mode nil)
                               (c-set-offset 'defun-block-intro 2)
                               (c-set-offset 'statement-block-intro 2)
                               (c-set-offset 'brace-list-intro 2)
                               (c-set-offset 'block-open 0)
                               (c-set-offset 'inline-open 0)
                               (c-set-offset 'substatement-open 0)
                               (c-set-offset 'case-label '+))))

;; =============================================================================
;; Rust
;; =============================================================================

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save nil  ; Use C-c f to format manually
        rust-format-goto-problem nil))

(use-package cargo
  :straight t
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

;; =============================================================================
;; Go
;; =============================================================================

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :hook ((go-mode go-ts-mode) . (lambda ()
                                  (setq tab-width 4
                                        indent-tabs-mode t))))

;; =============================================================================
;; Python
;; =============================================================================

;; Python uses built-in python-mode or python-ts-mode
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq python-indent-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; =============================================================================
;; JavaScript/TypeScript
;; =============================================================================

(use-package typescript-mode
  :straight t
  :mode "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2))

(use-package js
  :straight (:type built-in)
  :config
  (setq js-indent-level 2))

;; Prettier for JS/TS formatting
(use-package prettier-js
  :straight t
  :commands prettier-js)

;; =============================================================================
;; Web (HTML, CSS)
;; =============================================================================

(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1))

(use-package css-mode
  :straight (:type built-in)
  :config
  (setq css-indent-offset 2))

;; =============================================================================
;; Data Formats
;; =============================================================================

(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'")

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package json-mode
  :straight t
  :mode "\\.json\\'")

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'")

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; =============================================================================
;; Other Languages
;; =============================================================================

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :config
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t))

(use-package cmake-mode
  :straight t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package solidity-mode
  :straight t
  :mode "\\.sol\\'")

;; =============================================================================
;; Format Buffer Function
;; =============================================================================

(defun +format/buffer ()
  "Format the current buffer based on the major mode.
Uses LSP formatter if LSP is active, otherwise falls back to mode-specific formatters."
  (interactive)
  (cond
   ;; LSP is available and connected
   ((and (bound-and-true-p lsp-mode)
         (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer))
   ;; JavaScript/TypeScript with prettier
   ((derived-mode-p 'js-mode 'js-ts-mode 'typescript-mode 'typescript-ts-mode
                    'tsx-ts-mode 'web-mode 'css-mode 'css-ts-mode)
    (if (executable-find "prettier")
        (prettier-js)
      (message "prettier not found, install with: npm i -g prettier")))
   ;; Rust
   ((derived-mode-p 'rust-mode 'rust-ts-mode)
    (if (executable-find "rustfmt")
        (rust-format-buffer)
      (message "rustfmt not found, install with: rustup component add rustfmt")))
   ;; Go
   ((derived-mode-p 'go-mode 'go-ts-mode)
    (if (executable-find "gofmt")
        (gofmt)
      (message "gofmt not found")))
   ;; C/C++ with clang-format
   ((derived-mode-p 'c-mode 'c-ts-mode 'c++-mode 'c++-ts-mode)
    (if (executable-find "clang-format")
        (let ((start (point-min))
              (end (point-max)))
          (shell-command-on-region start end "clang-format" nil t))
      (message "clang-format not found")))
   ;; Python
   ((derived-mode-p 'python-mode 'python-ts-mode)
    (cond
     ((executable-find "black")
      (shell-command-on-region (point-min) (point-max) "black -q -" nil t))
     ((executable-find "autopep8")
      (shell-command-on-region (point-min) (point-max) "autopep8 -" nil t))
     (t (message "No Python formatter found (install black or autopep8)"))))
   ;; Fallback
   (t
    (message "No formatter available for %s" major-mode))))

;; Bind format to C-c f in prog modes
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") '+format/buffer)))

;; =============================================================================
;; Indent Bars (visual indentation guides)
;; =============================================================================

(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.2
        indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-zigzag nil
        indent-bars-highlight-current-depth nil
        indent-bars-pattern "|"
        indent-bars-prefer-character t))

;;; init-languages.el ends here
