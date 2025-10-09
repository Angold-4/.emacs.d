(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(setq indent-bars-unspecified-fg-color "black")  ; Or any valid color like "#282c34"
;; (load-theme 'noctilux t)
;; (load-theme 'timu-spacegrey t)
;; (load-theme 'doom-nord-light t)

;; (if (not (display-graphic-p))
    ;; (set-face-background 'default "white")
    ;; (set-face-foreground 'default "black"))

;; (set-face-background 'default "black")
