;;; init-org.el --- Minimal org-mode configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Ango Wang
;; Description: Plain note-taking with org-mode.  Notes live under ~/org/
;; on Linux and ~/orgw/ on macOS.  No calendar sync, no multi-project
;; scaffolding, no ICS export — just normal org.

;;; Commentary:
;; All .org files anywhere under the platform org root feed into the agenda.
;;
;; Keybindings (see also init-evil.el for global C-c l/a/c):
;;   C-c a   Org agenda
;;   C-c c   Org capture
;;   C-c d   Set deadline   (org-mode)
;;   C-c s   Schedule       (org-mode)

;;; Code:

(defvar +org/root
  (expand-file-name (if (eq system-type 'darwin) "~/orgw/" "~/org/"))
  "Root directory for all org files on this machine.
macOS uses ~/orgw/; Linux and other platforms use ~/org/.")

;; Make sure the root exists so first-launch capture/agenda don't error.
(unless (file-directory-p +org/root)
  (make-directory +org/root t))

(use-package org
  :straight (:type built-in)
  :defer t
  :config
  ;; ── Basics ─────────────────────────────────────────────────────────
  (setq org-directory +org/root
        org-default-notes-file (expand-file-name "notes.org" +org/root)
        org-log-done 'time
        org-log-into-drawer t
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-startup-folded t
        org-startup-indented t
        org-adapt-indentation nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0)
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

  ;; ── TODO keywords ──────────────────────────────────────────────────
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@/!)" "|"
                    "DONE(d!)" "CANCELLED(c@)"))
        org-todo-keyword-faces
        '(("TODO"        . (:foreground "#ff6c6b" :weight bold))
          ("IN-PROGRESS" . (:foreground "#ECBE7B" :weight bold))
          ("WAITING"     . (:foreground "#da8548" :weight bold))
          ("DONE"        . (:foreground "#98be65" :weight bold))
          ("CANCELLED"   . (:foreground "#5B6268" :weight bold))))

  ;; ── Agenda: every .org file under +org/root (recursive) ──────────
  (setq org-agenda-files
        (when (file-directory-p +org/root)
          (directory-files-recursively +org/root "\\.org\\'"))
        org-agenda-span 'day
        org-agenda-start-on-weekday 1
        org-agenda-window-setup 'current-window
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)

  ;; ── Capture: TODO and free-form Note, both into notes.org ────────
  (setq org-capture-templates
        `(("t" "TODO" entry
           (file+headline ,org-default-notes-file "Inbox")
           "* TODO %?\n  %i" :empty-lines 1)
          ("n" "Note" entry
           (file+headline ,org-default-notes-file "Notes")
           "* %?\n  %i" :empty-lines 1)))

  ;; ── Clocking ───────────────────────────────────────────────────────
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t
        org-clock-in-resume t
        org-clock-out-when-done t
        org-clock-idle-time 15
        org-clock-out-remove-zero-time-clocks t)

  ;; ── Per-mode keys ──────────────────────────────────────────────────
  (define-key org-mode-map (kbd "C-c d") 'org-deadline)
  (define-key org-mode-map (kbd "C-c s") 'org-schedule))

;; ── Markdown export ────────────────────────────────────────────────────
;; Org ships `ox-md' (plain Markdown) but does NOT enable it in the export
;; dispatcher by default, which is why C-c C-e offered no Markdown option.
;; `ox-gfm' extends ox-md with GitHub Flavored Markdown and captures org
;; content far better: real tables, fenced code blocks WITH language tags,
;; strikethrough, and task lists. Loading it registers both backends.
;;
;;   C-c C-e m m  → plain Markdown (file)
;;   C-c C-e g g  → GitHub Flavored Markdown (file)   ← the better one
;;   C-c C-e g G  → GitHub Flavored Markdown (to buffer)
(use-package ox-gfm
  :straight t
  :after org
  :demand t)

(with-eval-after-load 'org
  (require 'ox-md)            ; built-in plain Markdown backend
  (require 'ox-gfm nil t))    ; GitHub Flavored Markdown backend

;; ── PDF export (via Tectonic + engrave-faces) ──────────────────────────
;; Org exports to PDF through LaTeX (the built-in `latex' backend). We drive
;; it with Tectonic: a self-contained XeTeX engine that auto-downloads only
;; the packages a document needs (no MacTeX), supports system fonts/Unicode,
;; and needs no shell-escape. `engrave-faces' renders source blocks in the
;; PDF using the current Emacs theme's colors.
;;
;;   C-c C-e l p  → export to PDF (and stay)
;;   C-c C-e l o  → export to PDF and open it
;;
;; First export is slower while Tectonic downloads its support bundle (cached
;; afterwards under ~/Library/Caches/Tectonic). Subsequent runs are fast.
(use-package engrave-faces
  :straight t
  :after org)

(with-eval-after-load 'ox-latex
  ;; Theme-colored code blocks in the PDF (org 9.6+). Needs engrave-faces.
  (setq org-latex-src-block-backend 'engraved)
  ;; Compile with Tectonic instead of pdflatex/xelatex. %f = .tex file,
  ;; %o = output directory. --keep-logs leaves a .log next to the PDF.
  (setq org-latex-pdf-process
        '("tectonic -X compile %f --outdir %o --keep-logs"))
  ;; Tectonic is XeTeX-based; telling org the compiler is xelatex makes it
  ;; emit XeTeX-appropriate default packages (no inputenc/fontenc clash).
  (setq org-latex-compiler "xelatex")

  ;; A dense, HTML-like document class: small 10pt body, tight 1.6cm margins
  ;; (the stock `article' wastes ~⅓ of the page on margins), a clean
  ;; sans-serif family, and HTML-style paragraphs (no first-line indent, a
  ;; blank line between paragraphs). Result: much more text per page, closer
  ;; to the HTML export's look.
  ;; Make table columns WRAP to the page width. By default org emits fixed
  ;; l/c/r columns with no width limit, so a table with long cell text runs
  ;; off the right edge and looks "cut". This advice rewrites the column spec
  ;; into equal-width ragged paragraph columns summing to \linewidth, so wide
  ;; tables wrap cleanly. Applies to every table in every file.
  (defun +org/latex-wrap-table-columns (orig table info &rest args)
    (let* ((spec (apply orig table info args))
           (ncols (length (seq-filter (lambda (c) (memq c '(?l ?c ?r)))
                                      (string-to-list spec)))))
      (if (> ncols 0)
          (let ((col (format ">{\\raggedright\\arraybackslash}p{\\dimexpr(\\linewidth - %d\\tabcolsep)/%d\\relax}"
                             (* 2 ncols) ncols)))
            (mapconcat #'identity (make-list ncols col) ""))
        spec)))
  (advice-add 'org-latex--align-string :around #'+org/latex-wrap-table-columns)

  (add-to-list 'org-latex-classes
               '("org-dense"
                 "\\documentclass[10pt]{article}
\\usepackage[margin=1.6cm]{geometry}
\\usepackage{parskip}
\\usepackage{microtype}
\\usepackage{fontspec}
%% Monospace = Cascadia Code: a true fixed-width font that DOES contain the
%% Unicode box-drawing glyphs (┌ ─ │ └ ┴) and arrows (▼ ▲ ·) used in the
%% ASCII charts. The default LaTeX mono font lacks these, so charts in
%% example/src blocks came out blank or misaligned.
%% Scale=MatchLowercase shrinks the mono font so its x-height matches the
%% body text — monospace otherwise renders visibly larger at the same pt.
\\setmonofont{Cascadia Code}[Scale=MatchLowercase]
\\renewcommand{\\familydefault}{\\sfdefault}
\\usepackage{etoolbox}
%% Grey background block behind #+begin_example blocks (the ASCII charts).
%% example blocks export to a plain `verbatim'; we wrap every verbatim in a
%% breakable grey mdframed box. (We avoid tcolorbox here because org's
%% `engraved' src-block backend already loads it, and a second load would
%% clash.) mdframed breaks cleanly across pages for tall charts.
\\usepackage[framemethod=TikZ]{mdframed}
\\definecolor{exampleblockbg}{HTML}{ECECEC}
\\newmdenv[backgroundcolor=exampleblockbg,linewidth=0pt,%
  leftmargin=0pt,rightmargin=0pt,%
  innerleftmargin=6pt,innerrightmargin=6pt,%
  innertopmargin=5pt,innerbottommargin=5pt,%
  skipabove=6pt,skipbelow=6pt]{exampleblock}
\\BeforeBeginEnvironment{verbatim}{\\begin{exampleblock}}
\\AfterEndEnvironment{verbatim}{\\end{exampleblock}}
%% Keep (sub)section headings with their content: if too little room remains
%% on the page, start the heading on a fresh page instead of stranding it.
\\usepackage{needspace}
\\pretocmd{\\subsection}{\\needspace{8\\baselineskip}}{}{}
\\pretocmd{\\subsubsection}{\\needspace{6\\baselineskip}}{}{}
%% `array' enables the >{...}p{} wrapping columns produced by the table advice
%% in init-org.el (so wide tables wrap to the page instead of overflowing).
\\usepackage{array}
%% Cleaner page breaks: never leave a single line stranded at the top or
%% bottom of a page (orphans/widows), and start each top-level section on a
%% fresh page so a big section never begins near the bottom of a page.
\\widowpenalty=10000
\\clubpenalty=10000
%% Small footer note on every page (like a margin/bottom note), e.g.
%% \"Generated from Emacs Org-mode\". The title page stays clean because
%% \\maketitle forces the `plain' page style there.
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhf{}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}
\\fancyfoot[L]{\\footnotesize\\itshape\\color{gray} Generated from Emacs Org-mode}
\\fancyfoot[R]{\\footnotesize\\thepage}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ;; \clearpage lives in the section TEMPLATE (not a global
                 ;; \pretocmd hook) so it only fires for real `* Section'
                 ;; headings — not for the \section*{Contents} that
                 ;; \tableofcontents emits internally (which would otherwise
                 ;; shove the TOC off the title page).
                 ("\\clearpage\n\\section{%s}" . "\\clearpage\n\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "org-dense"))

;; Prettier headings
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

;;; init-org.el ends here
