;;; -*- lexical-binding: t -*-

(use-package paren
  :custom-face (show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.2)
  )

;; [rainbow-delimiters] Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

(use-package highlight-parentheses
  :straight t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2)
  )

;; [whitespace] Show visualize TAB, (HARD) SPC, newline
(use-package whitespace
  :hook ((prog-mode conf-mode yaml-mode) . whitespace-mode)
  :init
  :config
  ;; only show bad whitespace
  (setq whitespace-style
        '(face trailing empty indentation space-before-tab space-after-tab)))


(use-package hl-todo
  :straight t
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :inverse-video t))))
  :hook ((prog-mode conf-mode yaml-mode) . hl-todo-mode)
  :config
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")
  (defun +hl-todo-add-keywords (keys color)
    (dolist (keyword keys)
      (if-let ((item (assoc keyword hl-todo-keyword-faces)))
          (setf (cdr item) color)
        (push `(,keyword . ,color) hl-todo-keyword-faces))))
  (add-hook! enable-theme-functions :call-immediately
    (defun +hl-update-keyword-faces (&rest _)
      (+hl-todo-add-keywords '("BUG" "DEFECT" "ISSUE") (face-foreground 'error))
      (+hl-todo-add-keywords '("WORKAROUND" "HACK" "TRICK") (face-foreground 'warning))))
)

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil)
)

(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . +recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . +recenter-and-pulse-line))
  :init
  (setq pulse-delay 0.1
        pulse-iterations 2)
  (defun +pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (+pulse-momentary-line)))
  (defun +recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))
  (defun +recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse-momentary-line))
  (dolist (cmd '(recenter-top-bottom other-window switch-to-buffer ...))
    (advice-add cmd :after #'+pulse-momentary-line))
  (dolist (cmd '(pop-to-mark-command pop-global-mark goto-last-change))
    (advice-add cmd :after #'+recenter-and-pulse))
  (dolist (cmd '(symbol-overlay-basic-jump compile-goto-error))
    (advice-add cmd :after #'+recenter-and-pulse-line))
)


(use-package symbol-overlay
  :straight (:host github :repo "roife/symbol-overlay" :branch "master")
  :bind (("C-c s i" . symbol-overlay-put)
         ("C-c s n" . symbol-overlay-switch-forward)
         ("C-c s p" . symbol-overlay-switch-backward)
         ("C-c s c" . symbol-overlay-remove-all)
         :map symbol-overlay-map
         ("h" . nil) ("q" . nil) ("i" . nil)
         ("R" . symbol-overlay-query-replace)
         ("?" . symbol-overlay-map-help)
         ("c" . symbol-overlay-put)
         ("C" . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode))
  :config
  (setq symbol-overlay-temp-highlight-on-region t)
)
