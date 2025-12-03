;;; init-ui.el --- UI configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: User interface settings, fonts, ligatures, fringes

;;; Commentary:
;; This module configures the visual appearance of Emacs:
;; - Frame settings (scroll bars, menu bar, tool bar)
;; - Font configuration
;; - Ligature support
;; - Fringe settings
;; - Cursor behavior
;; - Window dividers

;;; Code:

;; =============================================================================
;; Frame Settings
;; =============================================================================

;; Disable UI chrome (already done in early-init, but ensure it)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable tab bar (Emacs 27+)
(when (fboundp 'tab-bar-mode)
  (tab-bar-mode -1))

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Allow frame resize by pixels (smoother resizing)
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

;; =============================================================================
;; Font Configuration
;; =============================================================================

;; Set default font - Cascadia Code PL with fallbacks
;; Change this if you prefer a different font
(defvar +ui/default-font "Cascadia Code PL"
  "Default font family.")

(defvar +ui/default-font-size 130
  "Default font size (in 1/10 pt, so 130 = 13pt).")

(defun +ui/set-font ()
  "Set the default font if available."
  (when (display-graphic-p)
    (when (member +ui/default-font (font-family-list))
      (set-face-attribute 'default nil
                          :family +ui/default-font
                          :height +ui/default-font-size))))

;; Set font on startup
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (+ui/set-font))))
  (+ui/set-font))

;; =============================================================================
;; Ligature Support (Emacs 28+)
;; =============================================================================

(use-package ligature
  :straight t
  :hook ((prog-mode markdown-mode org-mode) . ligature-mode)
  :config
  ;; Enable common programming ligatures
  ;; These work well with Cascadia Code, Fira Code, JetBrains Mono, etc.
  ;; Note: Each ligature must be 2+ characters
  (ligature-set-ligatures
   '(prog-mode markdown-mode org-mode)
   '(;; Arrows
     "->" "-->" "->>" "-<" "-<<" "<-" "<--" "<<-" "<->" "<-->"
     "=>" "==>" "=>>" "=<" "=<<" "<=" "<==" "<<=" "<=>" "<==>"
     ;; Comparisons
     "==" "===" "!=" "!==" ">=" "<=" "<=>"
     ;; Logic
     "&&" "||" "!!" "::"
     ;; Other
     "..." ".." "/*" "*/" "//" "/**" "##" "###" "####"
     ":=" "::=" "=:=" "++" "--" "??" "?." ".?" "?:" 
     "<|" "|>" "<|>" "|]" "[|" "{|" "|}"
     "www" "#_" "#(" "#_(" "#:" "#!" "#?")))

;; =============================================================================
;; Performance Settings
;; =============================================================================

(setq
 ;; Reduce UI updates
 idle-update-delay 1.0
 
 ;; Inhibit fontification while receiving input (better scrolling)
 redisplay-skip-fontification-on-input t
 
 ;; Only highlight in selected window
 highlight-nonselected-windows nil
 cursor-in-non-selected-windows nil
 
 ;; Font caching (uses more memory but faster)
 inhibit-compacting-font-caches t)

;; =============================================================================
;; Mouse & Scrolling (GUI only)
;; =============================================================================

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

;; =============================================================================
;; Window Dividers
;; =============================================================================

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(add-hook 'window-setup-hook #'window-divider-mode)

;; =============================================================================
;; Fringes
;; =============================================================================

;; Reduce fringe clutter
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Custom fringe bitmaps (more subtle)
(define-fringe-bitmap 'right-curly-arrow
  [#b00110000 #b00110000 #b00000000 #b00110000
   #b00110000 #b00000000 #b00110000 #b00110000])

(define-fringe-bitmap 'left-curly-arrow
  [#b00110000 #b00110000 #b00000000 #b00110000
   #b00110000 #b00000000 #b00110000 #b00110000])

(define-fringe-bitmap 'right-arrow
  [#b00000000 #b00000000 #b00001110 #b00001110
   #b00001110 #b00000000 #b00000000 #b00000000])

(define-fringe-bitmap 'left-arrow
  [#b00000000 #b00000000 #b00000000 #b01110000
   #b01110000 #b01110000 #b00000000 #b00000000])

;; =============================================================================
;; Line Numbers
;; =============================================================================

;; Display line numbers in programming modes only
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Use relative line numbers (comment out if you prefer absolute)
;; (setq display-line-numbers-type 'relative)

;; =============================================================================
;; Mode Line
;; =============================================================================

;; Keep the mode line clean and informative
;; Consider using doom-modeline or similar for more features
(setq mode-line-compact t)

;; =============================================================================
;; Fullscreen
;; =============================================================================

;; Start in fullscreen (comment out if you prefer windowed)
;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;;; init-ui.el ends here

