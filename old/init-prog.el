(use-package xref
  :config
  (setq
   xref-search-program 'ripgrep
   xref-history-storage 'xref-window-local-history)

  (defadvice! +xref--push-marker-stack-a (&rest rest)
    :before '(find-function consult-imenu consult-ripgrep citre-jump)
    (xref-push-marker-stack (point-marker)))
  )


(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package project
  :straight (:type built-in))

;; Eglot configuration for Gopls
(defun my-eglot-go-server (_interactive)
  (if (file-remote-p default-directory)
      '("/home/awang/.gopls_wrappers.sh")
    '("gopls")))

;; Eglot configuration for Gopls
(defun my-eglot-rust-server (_interactive)
  (if (file-remote-p default-directory)
      '("/home/awang/.rust_wrappers.sh")
    '("rust-analyzer")))

;; Install prettier-js for TypeScript and JavaScript formatting
(use-package prettier-js
  :straight (prettier-js :type git :host github :repo "prettier/prettier-emacs"))

;; Define a function to format the buffer based on major mode
(defun format-buffer ()
  "Format the current buffer based on the major mode."
  (interactive)
  (cond
   ((or (eq major-mode 'js-mode) (eq major-mode 'typescript-mode))
    (prettier-js))
   ((eglot-managed-p)
    (eglot-format-buffer))
   (t
    (message "No formatter available for this mode."))))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 2       ; base indent = 2
                  tab-width 2
                  indent-tabs-mode nil)  ; use spaces, not tabs
            ;; make the line after a function-open brace indent by 2, not 4
            (c-set-offset 'defun-block-intro 2)
            ;; these keep other blocks consistent at 2
            (c-set-offset 'statement-block-intro 2)
            (c-set-offset 'brace-list-intro 2)
            (c-set-offset 'block-open 0)
            (c-set-offset 'inline-open 0)
            (c-set-offset 'substatement-open 0)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 2
                  tab-width 2
                  indent-tabs-mode nil)
            (c-set-offset 'defun-block-intro 2)
            (c-set-offset 'statement-block-intro 2)
            (c-set-offset 'brace-list-intro 2)
            (c-set-offset 'block-open 0)
            (c-set-offset 'inline-open 0)
            (c-set-offset 'substatement-open 0)))

;; [Eglot] LSP support
(use-package eglot
  :hook ((c-mode c++-mode rust-mode python-mode java-mode c-ts-mode c++-ts-mode rust-ts-mode python-ts-mode go-mode typescript-mode) . eglot-ensure)
  :custom-face (eglot-highlight-symbol-face ((t (:underline t))))
  :bind (:map eglot-mode-map
              ("C-c ." . eldoc)
              ("M-l" . eglot-code-actions)
              ("M-/" . eglot-find-typeDefinition)
              ("M-?" . xref-find-references))
  :config
  (setq eglot-events-buffer-config '(:size 0 :format full)
        eglot-autoshutdown t
        eglot-report-progress 'messages)
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd" "--compile-commands-dir=build/"))
               '((typescript-mode tsx-ts-mode typescript-ts-mode js-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . my-eglot-go-server))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . my-eglot-rust-server))

  ;; Define the filter function
  (defun +eglot-filter-hover-info (info)
    "Filter out lines starting with 'M-l:' from hover information."
    (let ((lines (split-string info "\n")))
      (string-join
       (cl-remove-if (lambda (line) (string-prefix-p "M-l:" line)) lines)
       "\n")))

  ;; Apply the filter to eglot--hover-info
  (advice-add 'eglot--hover-info :filter-return #'+eglot-filter-hover-info)

  ;; eglot has it's own strategy by default
  (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq-default eglot-workspace-configuration
                '((:pyls . (:plugins (:jedi_completion (:fuzzy t))))
                  (:rust-analyzer . (:cargo (:allFeatures t :allTargets t :features "full")
                                            :checkOnSave :json-false
                                            :completion (:termSearch (:enable t)
                                                                     :fullFunctionSignatures (:enable t))
                                            :hover (:memoryLayout (:size "both")
                                                                  :show (:traitAssocItems 5)
                                                                  :documentation (:keywords (:enable :json-false)))
                                            :inlayHints(;:bindingModeHints (:enable t)
                                                        :lifetimeElisionHints (:enable "skip_trivial" :useParameterNames t)
                                                        :closureReturnTypeHints (:enable "always")
                                                        :discriminantHints (:enable t)
                                                        :genericParameterHints (:lifetime (:enable t)))
                                            :semanticHighlighting (:operator (:specialization (:enable t))
                                                                             :punctuation (:enable t :specialization (:enable t)))
                                            :workspace (:symbol (:search (:kind "all_symbols"
                                                                                :scope "workspace_and_dependencies")))
                                            :lru (:capacity 1024)))
                  (:typescript . (:preferences (:importModuleSpecifierPreference "non-relative")))
                  (:gopls . ((staticcheck . t)
                             (matcher . "CaseSensitive")))))

  (defsubst find-value-and-succ (value lst)
    (while (and lst (not (eq (car lst) value)))
      (setq lst (cdr lst)))
    (if lst
        (car (cdr lst))
      nil))

  (defsubst set-value-and-succ (key value lst)
    (let ((key-pos (member key lst)))
      (if key-pos
          (if (cdr key-pos)
              (setcar (cdr key-pos) value)
            (error "Key found but no value (no succ element) to update"))
        (setf lst (append lst (list key value)))))
    lst)

  (defsubst toggle-boolean-json (v)
    (if (eq v :json-false)
        t
      :json-false))

  (defun +eglot-toggle-exclude-imports-for-rust-analyzer ()
    (interactive)
    (let* ((current-config (alist-get :rust-analyzer eglot-workspace-configuration))
           (references (find-value-and-succ :references current-config))
           (val (find-value-and-succ :excludeImports references)))
      (if references
          (setf references (set-value-and-succ :excludeImports (toggle-boolean-json (or val :json-false)) references))
        (setq references (list :excludeImports t)))
      (setq current-config (set-value-and-succ :references references current-config))
      (setf (alist-get :rust-analyzer eglot-workspace-configuration) current-config)
      (if (eq val :json-false)
          (message "Exclude imports")
        (message "Include imports")))
    )

  (defun +eglot-toggle-exclude-tests-for-rust-analyzer ()
    (interactive)
    (let* ((current-config (alist-get :rust-analyzer eglot-workspace-configuration))
           (references (find-value-and-succ :references current-config))
           (val (find-value-and-succ :excludeTests references)))
      (if references
          (setf references (set-value-and-succ :excludeTests (toggle-boolean-json (or val :json-false)) references))
        (setq references (list :excludeTests t)))
      (setq current-config (set-value-and-succ :references references current-config))
      (setf (alist-get :rust-analyzer eglot-workspace-configuration) current-config)
      )
    )

  ;; we call eldoc manually by C-h .
  (add-hook! eglot-managed-mode-hook
    ;; (defun +eglot-disable-eldoc-mode ()
    ;;   (when (eglot-managed-p)
    ;;     (eldoc-mode -1)))))
    (defun +eglot-configure-eldoc ()
      (when (eglot-managed-p)
        (setq-local eldoc-documentation-functions '(flymake-eldoc-function))
        (eldoc-mode 1)))))

(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c f") 'format-buffer)))


(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))


(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :bind (("C-c f ]" . flymake-goto-next-error)
         ("C-c f [" . flymake-goto-prev-error)
         ("C-c f b" . flymake-show-buffer-diagnostics))
  :config
  (setq
   flymake-diagnostic-functions nil)
  )

;; Langs
(use-package cc-mode
  :config
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+))


(use-package csv-mode
  :straight t)


(use-package rainbow-csv
  :straight (:host github :repo "emacs-vs/rainbow-csv"))


(use-package cmake-mode
  :straight t)


(use-package rmsbolt ; A compiler output viewer
  :straight t)


(use-package scala-ts-mode
  :straight t
  :config
  (add-to-list 'eglot-server-programs
               `((scala-mode scala-ts-mode)
                 . ,(alist-get 'scala-mode eglot-server-programs))))


(use-package llvm-mode
  :straight (:host github :repo "nverno/llvm-mode" :files ("*.el")))


(use-package swift-mode
  :straight t)


(use-package js
  :config
  (setq js-indent-level 2))


(use-package css-mode
  :config
  (setq css-indent-offset 2))


(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-goto-problem nil)
  )

(use-package cargo
  :straight t
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))


(use-package rust-playground
  :straight t)


(use-package go-mode
  :straight t)


(use-package haskell-mode
  :straight t
  :config
  (setq
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t))


(use-package tuareg
  :straight t)


;; (use-package verilog-mode
;;   :straight t
;;   :config
;;   (setq verilog-align-ifelse t
;;         verilog-auto-delete-trailing-whitespace t
;;         verilog-auto-inst-param-value t
;;         verilog-auto-inst-vector nil
;;         verilog-auto-lineup (quote all)
;;         verilog-auto-newline nil
;;         verilog-auto-save-policy nil
;;         verilog-auto-template-warn-unused t
;;         verilog-case-indent 4
;;         verilog-cexp-indent 4
;;         verilog-highlight-grouping-keywords t
;;         verilog-highlight-modules t
;;         verilog-indent-level 4
;;         verilog-indent-level-behavioral 4
;;         verilog-indent-level-declaration 4
;;         verilog-indent-level-module 4
;;         verilog-tab-to-comment t))


;; [yaml]
(use-package yaml-mode
  :straight t)


;; [toml]
(use-package toml-mode
  :straight t)


;; [graphviz-dot]
(use-package graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-indent-width 4))


;; [Proof General] Proof General is a generic front-end for proof assistants
;; (use-package proof-general
;;   :straight t
;;   :init
;;   (setq proof-splash-enable nil))


;; Major mode for editing web templates
(use-package web-mode
  :straight t
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.[lh]?eex\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-enable-html-entities-fontification t
   web-mode-auto-close-style 1))



;; [ts]
(use-package typescript-mode
  :straight t
  :mode ("\\.ts[x]\\'" . typescript-mode))


(use-package moonbit-mode
  :straight (:host github :repo "tonyfettes/moonbit-mode"))


;; [indent-bars] Show indent guides
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.2
        indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-zigzag nil
        indent-bars-highlight-current-depth nil
        indent-bars-pattern "|"
        indent-bars-prefer-character t)
  )
