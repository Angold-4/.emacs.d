(use-package persp-mode
  :straight t
  :defines (recentf-exclude)
  :commands (get-current-persp persp-contain-buffer-p persp-update-names-cache)
  :hook ((after-init . persp-mode))
  :init
  (setq persp-keymap-prefix (kbd "C-c p")
        persp-nil-name "⊥")
  :config
  (setq
   persp-autokill-buffer-on-remove 'kill-weak
   persp-reset-windows-on-nil-window-conf nil
   persp-add-buffer-on-after-change-major-mode t
   persp-set-last-persp-for-new-frames t
   persp-remove-buffers-from-nil-persp-behaviour nil
   ;; Do not auto load
   persp-auto-resume-time 0)
  (add-hook! persp-filter-save-buffers-functions
             (defun +persp-ignore-dead-or-temp-buffers (b)
               "Ignore dead or temp buffers."
               (or (not (buffer-live-p b))
                   (string-prefix-p " *" (buffer-name b)))))

  (add-hook! persp-filter-save-buffers-functions
             (defun +persp-ignore-more-temp-buffers (b)
               "Ignore more temporary buffers."
               (let ((bname (file-name-nondirectory (buffer-name b))))
                 (or (string-prefix-p ".newsrc" bname)
                     (string-prefix-p "magit" bname)
                     (string-prefix-p "COMMIT_EDITMSG" bname)
                     (string-prefix-p "Pfuture-Callback" bname)
                     (string-prefix-p "treemacs-persist" bname)
                     (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                     (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname))))))


(with-eval-after-load 'persp-mode
  (define-key persp-mode-map (kbd "C-x b") 'persp-switch-to-buffer))
