;;; early-init.el --- Early initialization for Emacs 30+ -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Author: Ango Wang
;; Description: Minimal early init for fast startup

;;; Commentary:
;; This file is loaded before init.el and before the package system and GUI
;; are initialized. We use it to disable package.el and set up GC optimization.

;;; Code:

;; =============================================================================
;; Performance Optimization
;; =============================================================================

;; Increase GC threshold during startup for faster loading
;; Will be reset to a reasonable value after init
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Prevent flash of unstyled UI elements
(setq frame-inhibit-implied-resize t)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; =============================================================================
;; UI Elements (disable early to prevent flicker)
;; =============================================================================

;; Disable UI elements before they load (prevents flicker)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Don't show startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)

;; =============================================================================
;; Native Compilation (Emacs 29+)
;; =============================================================================

(when (featurep 'native-compile)
  ;; Silence native compilation warnings
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; Set native compilation cache directory
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (expand-file-name "eln-cache/" user-emacs-directory))))

;; =============================================================================
;; File Handlers (defer during startup)
;; =============================================================================

;; Temporarily disable file-name-handler-alist for faster startup
(defvar +file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist +file-name-handler-alist)))

;;; early-init.el ends here
