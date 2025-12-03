;;; init-themes.el --- Theme configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Theme management with dark/light toggle

;;; Commentary:
;; This module provides:
;; - Dark theme: noctilux (existing)
;; - Light theme: minimal-light (clean white background, blue/yellow syntax)
;; - Easy toggle between themes with M-x +theme/toggle
;;
;; Theme Choices:
;; - Dark: noctilux - a refined dark theme inspired by LightTable
;; - Light: minimal-light - strictly white background with black text,
;;          blue/yellow syntax highlighting for readability

;;; Code:

;; =============================================================================
;; Theme Paths
;; =============================================================================

;; Add custom themes directory to load path
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/" user-emacs-directory))

;; =============================================================================
;; Theme State
;; =============================================================================

(defvar +theme/current 'dark
  "Current theme variant. Either 'dark or 'light.")

(defvar +theme/dark-theme 'noctilux
  "Theme to use for dark mode.")

(defvar +theme/light-theme 'minimal-light
  "Theme to use for light mode.")

;; =============================================================================
;; Theme Functions
;; =============================================================================

(defun +theme/load-dark ()
  "Load the dark theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme +theme/dark-theme t)
  ;; Force pure black background for OLED screens
  (custom-set-faces
   '(default ((t (:background "#000000")))))
  (setq +theme/current 'dark)
  (message "Loaded dark theme: %s" +theme/dark-theme))

(defun +theme/load-light ()
  "Load the light theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme +theme/light-theme t)
  (setq +theme/current 'light)
  (message "Loaded light theme: %s" +theme/light-theme))

(defun +theme/toggle ()
  "Toggle between dark and light themes."
  (interactive)
  (if (eq +theme/current 'dark)
      (+theme/load-light)
    (+theme/load-dark)))

;; =============================================================================
;; Syntax Highlighting Settings
;; =============================================================================

;; [paren] Show matching parentheses
(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        show-paren-delay 0.1))

;; [rainbow-delimiters] Color-code nested parentheses
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

;; [hl-todo] Highlight TODO/FIXME/etc keywords
(use-package hl-todo
  :straight t
  :hook ((prog-mode conf-mode) . hl-todo-mode)
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :inverse-video t))))
  :config
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":"))

;; [whitespace] Show problematic whitespace
(use-package whitespace
  :straight (:type built-in)
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :config
  ;; Only show problematic whitespace
  (setq whitespace-style '(face trailing empty indentation
                                space-before-tab space-after-tab)))

;; =============================================================================
;; Visual Feedback
;; =============================================================================

;; [pulse] Pulse current line on jump
(use-package pulse
  :straight (:type built-in)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :config
  (setq pulse-delay 0.08
        pulse-iterations 2)
  
  (defun +pulse/line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  
  (defun +pulse/recenter-and-pulse (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse/line))
  
  ;; Pulse on common navigation commands
  (dolist (cmd '(recenter-top-bottom other-window switch-to-buffer))
    (advice-add cmd :after #'+pulse/line))
  
  (dolist (cmd '(pop-to-mark-command pop-global-mark))
    (advice-add cmd :after #'+pulse/recenter-and-pulse)))

;; [goggles] Highlight changes on edit
(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil))

;; =============================================================================
;; Load Default Theme
;; =============================================================================

;; Load dark theme by default
(+theme/load-dark)

;;; init-themes.el ends here

