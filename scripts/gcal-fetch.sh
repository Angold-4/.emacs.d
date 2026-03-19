#!/usr/bin/env bash
# gcal-fetch.sh — Fetch Google Calendar events in batch mode.
#
# Usage: bash ~/.emacs.d/scripts/gcal-fetch.sh
#
# Runs org-gcal-fetch in Emacs batch mode (synchronous networking).
# This avoids the segfault in Emacs 30.1.50 dev build's async code.
# OAuth must already be set up (run gcal-auth.sh first).
#
# Events are written to ~/org/gcal.org and ICS is exported to
# ~/org/calendar.ics.

set -e

echo "=== org-gcal Fetch (batch mode) ==="

emacs --batch \
  -l ~/.emacs.d/init.el \
  --eval '(progn
    (require (quote org-gcal))
    (message "Fetching calendar events...")
    (org-gcal-fetch)
    ;; Wait for async deferred chains to complete
    (let ((i 0))
      (while (< i 60)
        (accept-process-output nil 1)
        (sleep-for 0.1)
        (setq i (1+ i))))
    ;; Save all modified org buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name (buffer-modified-p))
          (save-buffer))))
    (message "Done."))'

echo "=== Done ==="
