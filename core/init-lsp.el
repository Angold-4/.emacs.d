;;; init-lsp.el --- LSP configuration (on-demand) -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: LSP-mode configuration with on-demand activation

;;; Commentary:
;; This module configures LSP support with lsp-mode.
;; 
;; IMPORTANT: LSP is NOT auto-enabled! Use C-x l to toggle LSP on/off.
;; This keeps Emacs lightweight when you just want to read code.
;;
;; Supported Languages:
;; - C/C++     : clangd
;; - Rust      : rust-analyzer
;; - Go        : gopls
;; - Python    : pyright or pylsp
;; - TypeScript: typescript-language-server
;; - JavaScript: typescript-language-server
;;
;; Keybindings:
;; ┌─────────────────────────────────────────────────────────────────┐
;; │ LSP Control                                                      │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ C-x l       : Toggle LSP on/off                                  │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Navigation (xref)                                                │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ M-.         : Jump to definition                                 │
;; │ M-,         : Jump back (pop marker stack)                       │
;; │ M-?         : Find references                                    │
;; │ M-/         : Find type definition                               │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Diagnostics (errors/warnings)                                    │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ (hover)     : Error shows in echo area automatically             │
;; │ C-c C-k     : Show error at point in detail buffer               │
;; │ C-c e n     : Go to next error                                   │
;; │ C-c e p     : Go to previous error                               │
;; │ C-c e l     : List all errors in buffer                          │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ Code Actions                                                     │
;; ├─────────────────────────────────────────────────────────────────┤
;; │ C-c .       : Show documentation                                 │
;; │ M-l         : Code actions (quick fix)                           │
;; │ C-c f       : Format buffer                                      │
;; └─────────────────────────────────────────────────────────────────┘

;;; Code:

;; =============================================================================
;; LSP Mode
;; =============================================================================

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c ." . lsp-describe-thing-at-point)
              ("M-l" . lsp-execute-code-action)
              ("M-/" . lsp-find-type-definition)
              ("M-?" . lsp-find-references)
              ("C-c f" . lsp-format-buffer))
  :init
  ;; Performance settings
  (setq lsp-keymap-prefix "C-c l"  ; Prefix for lsp-command-map
        lsp-idle-delay 0.5
        lsp-log-io nil             ; Disable logging for performance
        lsp-completion-provider :capf
        lsp-enable-snippet t
        
        ;; UI settings - keep it minimal
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable t
        lsp-lens-enable nil        ; Disable code lens (can be slow)
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil
        
        ;; File watching (disable for large projects)
        lsp-enable-file-watchers nil
        lsp-enable-indentation t
        lsp-enable-on-type-formatting nil
        
        ;; Keep it lightweight
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-semantic-highlighting t
        
        ;; Auto-guess project root - look for these files going UP the tree
        ;; This ensures we find Cargo.toml in parent dirs (e.g., monorepo)
        lsp-auto-guess-root t)
  
  :config
  ;; Ensure lsp doesn't auto-format on save (do it manually with C-c f)
  (setq lsp-before-save-edits nil)
  
  ;; Better project root detection for Rust workspaces/monorepos
  ;; Keeps going up until it finds Cargo.toml (workspace root)
  (defun +lsp/rust-project-root (path)
    "Find the Rust project root by looking for Cargo.toml.
For workspaces, finds the topmost Cargo.toml."
    (when-let ((root (locate-dominating-file path "Cargo.toml")))
      ;; Check if there's a parent Cargo.toml (workspace)
      (let ((parent (file-name-directory (directory-file-name root))))
        (if (and parent (file-exists-p (expand-file-name "Cargo.toml" parent)))
            (+lsp/rust-project-root parent)
          root))))
  
  ;; Register our custom root finder for Rust
  (with-eval-after-load 'lsp-rust
    (setq lsp-rust-analyzer-cargo-watch-command "clippy"
          ;; Ensure rust-analyzer indexes all workspace members
          lsp-rust-analyzer-cargo-all-targets t
          lsp-rust-analyzer-check-all-targets t
          ;; Proc macro support (for derives, etc.)
          lsp-rust-analyzer-proc-macro-enable t
          lsp-rust-analyzer-experimental-proc-attr-macros t
          ;; Import settings
          lsp-rust-analyzer-import-merge-behaviour "crate"
          lsp-rust-analyzer-import-prefix "crate"
          ;; Inlay hints - show types inline!
          lsp-rust-analyzer-inlay-hints-mode t
          lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
          lsp-rust-analyzer-display-chaining-hints t
          lsp-rust-analyzer-display-closure-return-type-hints t
          lsp-rust-analyzer-display-parameter-hints t
          lsp-rust-analyzer-display-reborrow-hints "never")
    ;; Use our custom root detection
    (advice-add 'lsp--suggest-project-root :around
                (lambda (orig-fn)
                  (or (when (derived-mode-p 'rust-mode 'rust-ts-mode)
                        (+lsp/rust-project-root default-directory))
                      (funcall orig-fn)))))
  
  ;; Enable inlay hints globally for LSP
  (setq lsp-inlay-hint-enable t)
  
  ;; Configure servers for remote editing
  (with-eval-after-load 'tramp
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "gopls")
      :major-modes '(go-mode go-ts-mode)
      :remote? t
      :server-id 'gopls-remote))
    
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "rust-analyzer")
      :major-modes '(rust-mode rust-ts-mode)
      :remote? t
      :server-id 'rust-analyzer-remote))))

;; =============================================================================
;; LSP UI (optional enhancements)
;; =============================================================================

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-c C-d" . lsp-ui-doc-show))    ; Show doc popup manually
  :config
  (setq
   ;; Sideline - DISABLED (we use echo area for diagnostics)
   lsp-ui-sideline-enable nil
   
   ;; Peek (preview definitions in popup)
   lsp-ui-peek-enable t
   lsp-ui-peek-show-directory t
   
   ;; Doc popup (the box that shows type info)
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'top
   lsp-ui-doc-delay 0.3
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-max-width 80
   lsp-ui-doc-max-height 15
   lsp-ui-doc-include-signature t))

;; =============================================================================
;; Eldoc - Show diagnostics in echo area (status bar)
;; =============================================================================

;; Eldoc shows diagnostics in the echo area when cursor is on that line
(use-package eldoc
  :straight (:type built-in)
  :config
  (setq eldoc-idle-delay 0.2
        eldoc-echo-area-use-multiline-p t
        eldoc-echo-area-prefer-doc-buffer nil))  ; Prefer echo area

;; Custom function to show LSP diagnostics in echo area
(defun +lsp/eldoc-diagnostics (callback &rest _)
  "Show LSP diagnostics in eldoc (echo area)."
  (when (bound-and-true-p lsp-mode)
    (when-let* ((diags (lsp--cur-line-diagnotics))
                (diag (car diags))
                (severity (gethash "severity" diag 1))
                (msg (gethash "message" diag)))
      (funcall callback
               (format "%s: %s"
                       (propertize
                        (pcase severity
                          (1 "Error")
                          (2 "Warning")
                          (3 "Info")
                          (_ "Hint"))
                        'face (pcase severity
                                (1 'error)
                                (2 'warning)
                                (_ 'shadow)))
                       msg)))))

;; Add our diagnostic function to eldoc
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions
                        #'+lsp/eldoc-diagnostics nil t))))

;; =============================================================================
;; Toggle LSP Command
;; =============================================================================

(defun +lsp/toggle ()
  "Toggle LSP for the current buffer.
If LSP is running, shut it down. If not, start it."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (progn
        (lsp-disconnect)
        (lsp-mode -1)
        (message "LSP disabled"))
    (if (derived-mode-p 'prog-mode)
        (progn
          (lsp)
          (message "LSP started for %s" major-mode))
      (message "LSP only works in programming modes"))))

(defun +lsp/toggle-inlay-hints ()
  "Toggle inlay hints (type annotations) on/off."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (progn
        (setq lsp-inlay-hint-enable (not lsp-inlay-hint-enable))
        (lsp-inlay-hints-mode (if lsp-inlay-hint-enable 1 -1))
        (message "Inlay hints %s" (if lsp-inlay-hint-enable "enabled" "disabled")))
    (message "LSP not active")))

;; Global binding to toggle LSP
(global-set-key (kbd "C-x l") '+lsp/toggle)
;; Toggle inlay hints
(global-set-key (kbd "C-x i") '+lsp/toggle-inlay-hints)

;; =============================================================================
;; Show Error at Point (detailed view in buffer)
;; =============================================================================

(defun +lsp/show-diagnostic-in-buffer ()
  "Show detailed diagnostic at point in a buffer below."
  (interactive)
  (let ((diags nil)
        (source "none"))
    ;; Collect diagnostics from LSP
    (when (and (bound-and-true-p lsp-mode)
               (fboundp 'lsp--cur-line-diagnotics))
      (setq diags (lsp--cur-line-diagnotics))
      (setq source "lsp"))
    ;; Fallback to flymake
    (when (and (not diags) (bound-and-true-p flymake-mode))
      (setq diags (flymake-diagnostics (point)))
      (setq source "flymake"))
    
    (if (not diags)
        (message "No diagnostic at point")
      ;; Create buffer with diagnostics
      (let ((buf (get-buffer-create "*Diagnostics*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (if (string= source "lsp")
              ;; LSP diagnostics
              (dolist (diag diags)
                (let* ((severity (gethash "severity" diag 1))
                       (message-text (gethash "message" diag "No message"))
                       (source-name (gethash "source" diag "unknown"))
                       (code (gethash "code" diag nil))
                       (range (gethash "range" diag))
                       (start (when range (gethash "start" range)))
                       (line (when start (1+ (gethash "line" start 0))))
                       (col (when start (gethash "character" start 0))))
                  (insert (propertize
                           (format "[%s] "
                                   (pcase severity
                                     (1 "ERROR")
                                     (2 "WARNING")
                                     (3 "INFO")
                                     (_ "HINT")))
                           'face (pcase severity
                                   (1 'error)
                                   (2 'warning)
                                   (_ 'shadow))))
                  (when line
                    (insert (format "Line %d, Col %d\n" line col)))
                  (insert (format "Source: %s" source-name))
                  (when code
                    (insert (format " [%s]" code)))
                  (insert "\n\n")
                  (insert message-text)
                  (insert "\n\n---\n\n")))
            ;; Flymake diagnostics
            (dolist (diag diags)
              (insert (format "%s\n\n" (flymake-diagnostic-text diag)))))
          (goto-char (point-min))
          (special-mode))  ; Read-only, q to quit
        ;; Show buffer below
        (display-buffer buf
                        '((display-buffer-below-selected)
                          (window-height . 0.3)))))))

(defun +lsp/list-all-diagnostics ()
  "List all diagnostics in the current buffer."
  (interactive)
  (cond
   ((bound-and-true-p flymake-mode)
    (flymake-show-buffer-diagnostics))
   ((bound-and-true-p lsp-mode)
    (lsp-treemacs-errors-list))
   (t
    (message "No diagnostics available"))))

;; C-c C-k = show detailed error in buffer below
(global-set-key (kbd "C-c C-k") '+lsp/show-diagnostic-in-buffer)
;; C-c e l = list all errors
(global-set-key (kbd "C-c e l") '+lsp/list-all-diagnostics)

;; =============================================================================
;; Xref (jump to definition / references)
;; =============================================================================

;; Xref is the built-in cross-reference system
;; LSP integrates with it automatically when lsp-mode is active
(use-package xref
  :straight (:type built-in)
  :config
  (setq xref-search-program 'ripgrep
        xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  
  ;; Don't prompt for TAGS - just say "not found" if no backend can find it
  (setq xref-prompt-for-identifier nil)
  
  ;; Evil-friendly: M-. and M-, work in normal mode
  (with-eval-after-load 'evil
    ;; M-. = jump to definition (already default in xref)
    ;; M-, = jump back
    (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
    (define-key evil-normal-state-map (kbd "M-,") 'xref-go-back)))

;; Make sure LSP registers itself as xref backend when active
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook
            (lambda ()
              ;; LSP adds itself to xref-backend-functions
              ;; Remove etags backend when LSP is active
              (setq-local xref-backend-functions
                          (remq 'etags--xref-backend xref-backend-functions)))))

;; =============================================================================
;; Flymake Integration (diagnostics)
;; =============================================================================

;; Use flymake for diagnostics (LSP integrates with it)
(use-package flymake
  :straight (:type built-in)
  :bind (("C-c e n" . flymake-goto-next-error)
         ("C-c e p" . flymake-goto-prev-error)
         ("C-c e l" . flymake-show-buffer-diagnostics))
  :config
  (setq flymake-no-changes-timeout 0.5)
  
  ;; Show flymake diagnostics in eldoc (echo area)
  (setq flymake-show-diagnostics-at-end-of-line nil)  ; Don't clutter lines
  
  ;; Ensure eldoc shows flymake errors
  (add-hook 'flymake-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions
                        #'flymake-eldoc-function nil t))))

;; Tell LSP to use flymake for diagnostics (not flycheck)
(with-eval-after-load 'lsp-mode
  (setq lsp-diagnostics-provider :flymake))

;; =============================================================================
;; Language Server Configurations
;; =============================================================================

;; These are the default commands lsp-mode will use.
;; Make sure these are installed and in your PATH:
;; - clangd (C/C++)
;; - rust-analyzer (Rust)
;; - gopls (Go)
;; - pyright or pylsp (Python)
;; - typescript-language-server (TypeScript/JavaScript)

;; C/C++ with clangd
;; NOTE: clangd needs compile_commands.json to understand your project!
;; Generate it with:
;;   - CMake: cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
;;   - Bear: bear -- make
;;   - Ninja: ninja -t compdb > compile_commands.json
;; 
;; For projects with compile_commands.json in build/:
;;   Create a symlink: ln -s build/compile_commands.json compile_commands.json
(with-eval-after-load 'lsp-mode
  (setq lsp-clients-clangd-args
        '("--header-insertion=never"
          "--background-index"
          "-j=4"
          ;; Query the compiler driver for system include paths
          ;; This is ESSENTIAL when compile_commands.json uses GCC but clangd is Clang
          "--query-driver=/usr/bin/c++,/usr/bin/g++,/usr/bin/gcc")))

;; Function to check C++ LSP status
(defun +lsp/check-cpp-setup ()
  "Check if C++ LSP (clangd) is properly configured."
  (interactive)
  (let* ((project-root (or (lsp-workspace-root) default-directory))
         (compile-commands (expand-file-name "compile_commands.json" project-root))
         (build-compile-commands (expand-file-name "build/compile_commands.json" project-root)))
    (message "C++ LSP Check:\n  Project root: %s\n  compile_commands.json: %s\n  build/compile_commands.json: %s\n  clangd installed: %s\n  LSP active: %s\n  Flymake active: %s\n  Diagnostics: %s"
             project-root
             (if (file-exists-p compile-commands) "Found" "NOT FOUND")
             (if (file-exists-p build-compile-commands) "Found" "NOT FOUND")
             (if (executable-find "clangd") "Yes" "NO")
             (if (bound-and-true-p lsp-mode) "Yes" "No")
             (if (bound-and-true-p flymake-mode) "Yes" "No")
             (if (bound-and-true-p flymake-mode)
                 (format "%d" (length (flymake-diagnostics)))
               "N/A"))))

;; Symlink compile_commands.json from build/ to project root (recommended)
(defun +lsp/symlink-compile-commands ()
  "Create a symlink from build/compile_commands.json to project root.
This helps clangd find the compile database more reliably."
  (interactive)
  (let* ((project-root (or (lsp-workspace-root) (projectile-project-root) default-directory))
         (build-cc (expand-file-name "build/compile_commands.json" project-root))
         (root-cc (expand-file-name "compile_commands.json" project-root)))
    (if (not (file-exists-p build-cc))
        (message "No compile_commands.json found in build/")
      (when (file-exists-p root-cc)
        (delete-file root-cc))
      (make-symbolic-link build-cc root-cc)
      (message "Created symlink: %s -> %s" root-cc build-cc))))

;; =============================================================================
;; Rust Format (using rustfmt with edition from Cargo.toml)
;; =============================================================================

(defun +rust/get-edition-from-cargo-toml ()
  "Extract the edition from Cargo.toml in the project root."
  (when-let* ((root (+lsp/rust-project-root default-directory))
              (cargo-file (expand-file-name "Cargo.toml" root)))
    (when (file-exists-p cargo-file)
      (with-temp-buffer
        (insert-file-contents cargo-file)
        (when (re-search-forward "^edition\\s-*=\\s-*[\"']\\([0-9]+\\)[\"']" nil t)
          (match-string 1))))))

(defun +rust/format-buffer ()
  "Format current Rust buffer using rustfmt with correct edition.
Reads the edition from Cargo.toml and passes it to rustfmt."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "Buffer is not visiting a file")
      ;; Save first
      (save-buffer)
      ;; Get edition from Cargo.toml
      (let* ((edition (or (+rust/get-edition-from-cargo-toml) "2021"))
             (cmd (format "rustfmt --edition %s %s"
                          edition
                          (shell-quote-argument file))))
        (message "Running: %s" cmd)
        (shell-command cmd))
      ;; Revert buffer to see changes
      (revert-buffer t t t)
      (message "Formatted with rustfmt (edition %s)"
               (or (+rust/get-edition-from-cargo-toml) "2021")))))

;; Override C-c f in rust-mode to use cargo fmt
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c f") '+rust/format-buffer))
(with-eval-after-load 'rust-ts-mode
  (when (boundp 'rust-ts-mode-map)
    (define-key rust-ts-mode-map (kbd "C-c f") '+rust/format-buffer)))

;;; init-lsp.el ends here
