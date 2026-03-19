#!/usr/bin/env bash
# gcal-sync.sh — Two-way sync between Google Calendar and local org files.
#
# Usage: bash ~/.emacs.d/scripts/gcal-sync.sh
#
# Runs org-gcal-sync in Emacs batch mode (synchronous networking).
# This both FETCHES events from Google Calendar AND PUSHES local
# changes (edits to existing events in gcal.org) back to Google.
#
# This avoids the segfault in Emacs 30.1.50 dev build's async code.
# OAuth must already be set up (run gcal-auth.sh first).

set -e

echo "=== org-gcal Two-Way Sync (batch mode) ==="

emacs --batch \
  -l ~/.emacs.d/init.el \
  --eval '(progn
    (require (quote org-gcal))
    (message "Starting two-way sync...")
    (org-gcal-sync)
    ;; Wait for async deferred chains to complete
    (let ((i 0))
      (while (< i 90)
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
