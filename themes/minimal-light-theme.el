;;; minimal-light-theme.el --- A minimal light theme -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ango Wang
;; Author: Ango Wang
;; Description: Minimal light theme with white background and clean syntax

;;; Commentary:
;; A strictly minimal light theme:
;; - Pure white background (#ffffff)
;; - Black text for maximum readability
;; - Blue for keywords and functions
;; - Yellow/gold for strings and constants
;; - Subtle highlighting for comments
;;
;; Designed for:
;; - Long coding sessions
;; - Printing code
;; - Daylight viewing

;;; Code:

(deftheme minimal-light
  "A minimal light theme with white background and clean syntax highlighting.")

(let ((class '((class color) (min-colors 89)))
      ;; Color palette
      (bg         "#ffffff")  ; Pure white background
      (bg-alt     "#f5f5f5")  ; Slightly off-white for subtle distinction
      (bg-hl      "#e8e8e8")  ; Highlight background
      (fg         "#000000")  ; Pure black foreground
      (fg-dim     "#555555")  ; Dimmed text (comments)
      (fg-alt     "#333333")  ; Slightly lighter black
      
      ;; Syntax colors
      (blue       "#0000cc")  ; Keywords, functions
      (blue-dark  "#000099")  ; Types, classes
      (yellow     "#996600")  ; Strings
      (gold       "#b58900")  ; Constants, numbers
      (green      "#006600")  ; Success, added
      (red        "#cc0000")  ; Errors, deleted
      (orange     "#cc6600")  ; Warnings
      (purple     "#6600cc")  ; Variables, special
      (cyan       "#006666")  ; Links, built-ins
      
      ;; UI colors
      (border     "#cccccc")  ; Borders
      (cursor     "#000000")  ; Cursor
      (selection  "#b4d7ff")  ; Selection
      (match      "#ffff00")  ; Search match
      (modeline   "#e0e0e0")) ; Mode line

  (custom-theme-set-faces
   'minimal-light

   ;; ==========================================================================
   ;; Basic Faces
   ;; ==========================================================================
   
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,selection :extend t))))
   `(highlight ((,class (:background ,bg-hl))))
   `(hl-line ((,class (:background ,bg-alt :extend t))))
   `(fringe ((,class (:background ,bg))))
   `(vertical-border ((,class (:foreground ,border))))
   `(minibuffer-prompt ((,class (:foreground ,blue :weight bold))))
   `(link ((,class (:foreground ,cyan :underline t))))
   `(link-visited ((,class (:foreground ,purple :underline t))))
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,orange :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))

   ;; ==========================================================================
   ;; Mode Line
   ;; ==========================================================================
   
   `(mode-line ((,class (:background ,modeline :foreground ,fg
                         :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive ((,class (:background ,bg-alt :foreground ,fg-dim
                                  :box (:line-width 1 :color ,border)))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-emphasis ((,class (:weight bold))))
   `(mode-line-highlight ((,class (:inherit highlight))))

   ;; ==========================================================================
   ;; Font Lock (Syntax Highlighting)
   ;; ==========================================================================
   
   `(font-lock-builtin-face ((,class (:foreground ,cyan))))
   `(font-lock-comment-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-dim))))
   `(font-lock-constant-face ((,class (:foreground ,gold))))
   `(font-lock-doc-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,blue :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,blue))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-number-face ((,class (:foreground ,gold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,purple))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow))))
   `(font-lock-string-face ((,class (:foreground ,yellow))))
   `(font-lock-type-face ((,class (:foreground ,blue-dark))))
   `(font-lock-variable-name-face ((,class (:foreground ,purple))))
   `(font-lock-warning-face ((,class (:foreground ,orange :weight bold))))

   ;; ==========================================================================
   ;; Search & Matching
   ;; ==========================================================================
   
   `(isearch ((,class (:background ,match :foreground ,fg :weight bold))))
   `(isearch-fail ((,class (:background ,red :foreground ,bg))))
   `(lazy-highlight ((,class (:background ,bg-hl))))
   `(match ((,class (:background ,match))))
   `(show-paren-match ((,class (:foreground ,green :weight bold :underline t))))
   `(show-paren-mismatch ((,class (:foreground ,red :weight bold :underline t))))

   ;; ==========================================================================
   ;; Line Numbers
   ;; ==========================================================================
   
   `(line-number ((,class (:foreground ,fg-dim :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,bg-alt :weight bold))))

   ;; ==========================================================================
   ;; Dired
   ;; ==========================================================================
   
   `(dired-directory ((,class (:foreground ,blue :weight bold))))
   `(dired-flagged ((,class (:foreground ,red))))
   `(dired-header ((,class (:foreground ,blue :weight bold))))
   `(dired-ignored ((,class (:foreground ,fg-dim))))
   `(dired-mark ((,class (:foreground ,green :weight bold))))
   `(dired-marked ((,class (:foreground ,green :weight bold))))
   `(dired-perm-write ((,class (:foreground ,orange))))
   `(dired-symlink ((,class (:foreground ,cyan))))
   `(dired-warning ((,class (:foreground ,orange))))

   ;; ==========================================================================
   ;; Org Mode
   ;; ==========================================================================
   
   `(org-level-1 ((,class (:foreground ,blue :weight bold :height 1.2))))
   `(org-level-2 ((,class (:foreground ,blue-dark :weight bold :height 1.1))))
   `(org-level-3 ((,class (:foreground ,purple :weight bold))))
   `(org-level-4 ((,class (:foreground ,cyan :weight bold))))
   `(org-level-5 ((,class (:foreground ,green))))
   `(org-level-6 ((,class (:foreground ,yellow))))
   `(org-level-7 ((,class (:foreground ,orange))))
   `(org-level-8 ((,class (:foreground ,fg-dim))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-headline-done ((,class (:foreground ,fg-dim))))
   `(org-link ((,class (:foreground ,cyan :underline t))))
   `(org-date ((,class (:foreground ,purple))))
   `(org-tag ((,class (:foreground ,fg-dim :weight bold))))
   `(org-block ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line ((,class (:foreground ,fg-dim :background ,bg-alt :extend t))))
   `(org-block-end-line ((,class (:foreground ,fg-dim :background ,bg-alt :extend t))))
   `(org-code ((,class (:foreground ,cyan :background ,bg-alt))))
   `(org-verbatim ((,class (:foreground ,yellow :background ,bg-alt))))
   `(org-table ((,class (:foreground ,fg))))

   ;; ==========================================================================
   ;; Magit
   ;; ==========================================================================
   
   `(magit-section-heading ((,class (:foreground ,blue :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-alt))))
   `(magit-branch-local ((,class (:foreground ,blue))))
   `(magit-branch-remote ((,class (:foreground ,green))))
   `(magit-diff-added ((,class (:foreground ,green :background "#e6ffe6"))))
   `(magit-diff-added-highlight ((,class (:foreground ,green :background "#ccffcc"))))
   `(magit-diff-removed ((,class (:foreground ,red :background "#ffe6e6"))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red :background "#ffcccc"))))
   `(magit-diff-context ((,class (:foreground ,fg-dim))))
   `(magit-diff-context-highlight ((,class (:background ,bg-alt))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-hl))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,modeline))))

   ;; ==========================================================================
   ;; Company
   ;; ==========================================================================
   
   `(company-tooltip ((,class (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,selection))))
   `(company-tooltip-common ((,class (:foreground ,blue :weight bold))))
   `(company-tooltip-annotation ((,class (:foreground ,fg-dim))))
   `(company-scrollbar-bg ((,class (:background ,bg-hl))))
   `(company-scrollbar-fg ((,class (:background ,fg-dim))))
   `(company-preview ((,class (:background ,bg-alt))))
   `(company-preview-common ((,class (:foreground ,blue :background ,bg-alt))))

   ;; ==========================================================================
   ;; Flymake
   ;; ==========================================================================
   
   `(flymake-error ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,orange)))))
   `(flymake-note ((,class (:underline (:style wave :color ,blue)))))

   ;; ==========================================================================
   ;; LSP
   ;; ==========================================================================
   
   `(lsp-face-highlight-read ((,class (:background ,bg-hl))))
   `(lsp-face-highlight-write ((,class (:background ,bg-hl :underline t))))
   `(lsp-face-highlight-textual ((,class (:background ,bg-hl))))

   ;; ==========================================================================
   ;; Rainbow Delimiters
   ;; ==========================================================================
   
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,purple))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,red))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red :weight bold))))

   ;; ==========================================================================
   ;; Treemacs
   ;; ==========================================================================
   
   `(treemacs-directory-face ((,class (:foreground ,blue))))
   `(treemacs-file-face ((,class (:foreground ,fg))))
   `(treemacs-git-modified-face ((,class (:foreground ,orange))))
   `(treemacs-git-added-face ((,class (:foreground ,green))))
   `(treemacs-git-untracked-face ((,class (:foreground ,yellow))))

   ;; ==========================================================================
   ;; Whitespace
   ;; ==========================================================================
   
   `(whitespace-trailing ((,class (:background "#ffe0e0"))))
   `(whitespace-space ((,class (:foreground ,border))))
   `(whitespace-tab ((,class (:foreground ,border))))
   `(whitespace-empty ((,class (:background "#fff0e0"))))

   ;; ==========================================================================
   ;; hl-todo
   ;; ==========================================================================
   
   `(hl-todo ((,class (:weight bold :inverse-video t))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'minimal-light)

;;; minimal-light-theme.el ends here
