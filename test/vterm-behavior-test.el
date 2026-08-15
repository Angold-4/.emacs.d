;;; vterm-behavior-test.el --- Vterm viewport behavior tests -*- lexical-binding: t -*-

;;; Commentary:
;; Unit coverage for normal-state viewport persistence without starting a PTY.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defun vterm-test--fill-buffer ()
  "Make the current test buffer look like a normal-state vterm scrollback."
  (setq-local major-mode 'vterm-mode
              evil-mode t
              evil-state 'normal)
  (dotimes (line 200)
    (insert (format "line %03d\n" line))))

(defun vterm-test--place-view (window point-line start-line)
  "Place WINDOW's point and start at POINT-LINE and START-LINE."
  (with-selected-window window
    (goto-char (point-min))
    (forward-line point-line)
    (set-window-start
     window
     (save-excursion
       (goto-char (point-min))
       (forward-line start-line)
       (point))
     t)))

(ert-deftest vterm-normal-view-freezes-and-insert-unfreezes ()
  "Normal state freezes one view; insert state releases that snapshot."
  (let ((buffer (generate-new-buffer " *vterm-state-test*"))
        (configuration (current-window-configuration))
        (+vterm/frozen-views (make-hash-table :test #'eq)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (vterm-test--fill-buffer)
          (vterm-test--place-view (selected-window) 80 70)
          (+vterm/freeze-current-buffer-views)
          (let* ((saved (gethash (selected-window) +vterm/frozen-views))
                 (saved-point (nth 1 saved))
                 (saved-start (nth 2 saved)))
            (goto-char (point-min))
            (set-window-start (selected-window) (point-min) t)
            (+vterm/restore-frozen-views)
            (should (= (window-point (selected-window)) saved-point))
            (should (= (window-start (selected-window)) saved-start)))
          (setq-local evil-state 'insert)
          (+vterm/release-selected-window-view)
          (should (= (hash-table-count +vterm/frozen-views) 0)))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest vterm-unselected-visible-window-stays-frozen ()
  "A command in another window cannot move a visible normal-state vterm."
  (let ((buffer (generate-new-buffer " *vterm-visible-test*"))
        (other (generate-new-buffer " *vterm-visible-other*"))
        (configuration (current-window-configuration))
        (+vterm/frozen-views (make-hash-table :test #'eq))
        (+vterm/command-window nil)
        (+vterm/command-buffer nil))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (vterm-test--fill-buffer)
          (let* ((vterm-window (selected-window))
                 (other-window (split-window-right)))
            (set-window-buffer other-window other)
            (vterm-test--place-view vterm-window 80 70)
            ;; `windmove' can leave a terminal buffer in insert state.  Once
            ;; its window is unselected it must still freeze.
            (setq-local evil-state 'insert)
            (select-window other-window)
            (setq +vterm/command-window vterm-window
                  +vterm/command-buffer buffer)
            (+vterm/after-command)
            (let* ((saved (gethash vterm-window +vterm/frozen-views))
                   (saved-point (nth 1 saved))
                   (saved-start (nth 2 saved)))
              (setq +vterm/command-window other-window
                    +vterm/command-buffer other)
              ;; Model another command or package moving the visible terminal.
              (set-window-point vterm-window (point-min))
              (set-window-start vterm-window (point-min) t)
              (+vterm/after-command)
              (should (= (window-point vterm-window) saved-point))
              (should (= (window-start vterm-window) saved-start)))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest vterm-selected-navigation-advances-only-its-view ()
  "Deliberate navigation in the selected vterm updates its snapshot."
  (let ((buffer (generate-new-buffer " *vterm-navigation-test*"))
        (configuration (current-window-configuration))
        (+vterm/frozen-views (make-hash-table :test #'eq))
        (+vterm/command-window nil)
        (+vterm/command-buffer nil))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (vterm-test--fill-buffer)
          (let ((window (selected-window)))
            (vterm-test--place-view window 40 30)
            (+vterm/freeze-window-view window)
            (setq +vterm/command-window window
                  +vterm/command-buffer buffer)
            (vterm-test--place-view window 90 80)
            (+vterm/after-command)
            (let ((saved (gethash window +vterm/frozen-views)))
              (should (= (nth 1 saved) (window-point window)))
              (should (= (nth 2 saved) (window-start window))))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest vterm-redraw-restores-persistent-view-without-intermediate-paint ()
  "A redraw restores the old snapshot while redisplay remains inhibited."
  (let ((buffer (generate-new-buffer " *vterm-redraw-test*"))
        (configuration (current-window-configuration))
        (+vterm/frozen-views (make-hash-table :test #'eq))
        redraw-inhibited)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (vterm-test--fill-buffer)
          (let ((window (selected-window)))
            (vterm-test--place-view window 80 70)
            (+vterm/freeze-window-view window)
            (let* ((saved (gethash window +vterm/frozen-views))
                   (saved-point (nth 1 saved))
                   (saved-start (nth 2 saved)))
              (+vterm/advice-delayed-redraw
               (lambda (_buffer)
                 (setq redraw-inhibited inhibit-redisplay)
                 (set-window-point window (point-min))
                 (set-window-start window (point-min) t))
               buffer)
              (should redraw-inhibited)
              (should (= (window-point window) saved-point))
              (should (= (window-start window) saved-start))
              ;; Redraw must restore the old snapshot, never replace it with
              ;; the transient terminal cursor position.
              (should (= (nth 1 (gethash window +vterm/frozen-views))
                         saved-point)))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest vterm-unchanged-view-does-not-force-redisplay ()
  "Restoring an unchanged vterm view performs no window mutations."
  (let ((buffer (generate-new-buffer " *vterm-no-flash-test*"))
        (configuration (current-window-configuration))
        (+vterm/frozen-views (make-hash-table :test #'eq))
        (start-calls 0)
        (point-calls 0))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (vterm-test--fill-buffer)
          (vterm-test--place-view (selected-window) 40 30)
          (+vterm/freeze-window-view (selected-window))
          (let ((real-set-window-start (symbol-function 'set-window-start))
                (real-set-window-point (symbol-function 'set-window-point)))
            (cl-letf (((symbol-function 'set-window-start)
                       (lambda (&rest args)
                         (cl-incf start-calls)
                         (apply real-set-window-start args)))
                      ((symbol-function 'set-window-point)
                       (lambda (&rest args)
                         (cl-incf point-calls)
                         (apply real-set-window-point args))))
              (+vterm/restore-window-view (selected-window))))
          (should (= start-calls 0))
          (should (= point-calls 0)))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'vterm-behavior-test)

;;; vterm-behavior-test.el ends here
