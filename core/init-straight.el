;;; init-straight.el --- Package management with straight.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Description: Package manager configuration

;;; Commentary:
;; This module sets up straight.el for package management with use-package
;; integration. Packages are fetched from Git repositories for reproducibility.
;;
;; To update packages: M-x straight-pull-all
;; To rebuild packages: M-x straight-rebuild-all

;;; Code:

;; =============================================================================
;; Straight.el Settings (must be set before bootstrap)
;; =============================================================================

(setq straight-check-for-modifications nil        ; Skip modification checks (faster)
      straight-vc-git-default-clone-depth 1       ; Shallow clone
      straight-use-package-by-default t)          ; All use-package calls use straight

;; Handle native compilation
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq comp-deferred-compilation-deny-list ()
        warning-suppress-log-types '((comp))))

(setq straight-disable-native-compile
      (not (and (fboundp 'native-comp-available-p)
                (native-comp-available-p))))

;; =============================================================================
;; Bootstrap Straight.el
;; =============================================================================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; =============================================================================
;; Use-package Integration
;; =============================================================================

(straight-use-package 'use-package)

;; Configure use-package
(setq use-package-always-demand (daemonp)      ; Load immediately if daemon
      use-package-always-defer (not (daemonp)) ; Defer loading if not daemon
      use-package-expand-minimally t           ; Minimize generated code
      use-package-enable-imenu-support t       ; Enable Imenu support
      use-package-compute-statistics nil)      ; Don't compute load statistics

;;; init-straight.el ends here

;;; init-straight.el ends here
