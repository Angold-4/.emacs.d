#!/usr/bin/env bash
# gcal-auth.sh — Complete org-gcal OAuth in batch mode (no segfault).
#
# Usage: bash ~/.emacs.d/scripts/gcal-auth.sh
#
# This runs Emacs in batch mode to do the initial Google OAuth authorization.
# Batch mode uses synchronous networking, avoiding the segfault in Emacs
# 30.1.50 dev build's async code paths.
#
# After running this:
#   1. A Google authorization URL will be displayed
#   2. Open it in your browser (copy-paste to Windows Chrome)
#   3. Log in and authorize the app
#   4. You'll be redirected to localhost:8080 (which will fail to load — that's OK)
#   5. Copy the "code" parameter from the redirect URL
#      (it looks like: http://localhost:8080/?code=4/0AXXXXXXX...&scope=...)
#   6. Paste the code value back into the terminal prompt
#
# Once complete, the OAuth token is saved and org-gcal-fetch will work
# in normal (interactive) Emacs without needing the browser flow again.

set -e

echo "=== org-gcal OAuth Authorization (batch mode) ==="
echo ""
echo "This will open the Google OAuth flow in your terminal."
echo "Follow the prompts to authorize Emacs to access your Google Calendar."
echo ""

emacs --batch \
  -l ~/.emacs.d/init.el \
  --eval '(progn
    (require (quote org-gcal))
    (message "")
    (message "Starting org-gcal-fetch...")
    (message "A Google auth URL will appear. Copy it to your browser.")
    (message "After authorizing, copy the code= value from the redirect URL.")
    (message "")
    (org-gcal-fetch)
    ;; org-gcal-fetch uses aio (async); give it time to complete all
    ;; network requests (auth token exchange + calendar fetch).
    (let ((i 0))
      (while (< i 120)
        (accept-process-output nil 1)
        (sleep-for 0.1)
        (setq i (1+ i))))
    ;; Save all modified org buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name (buffer-modified-p))
          (save-buffer))))
    (message "")
    (message "Done. Check ~/.emacs.d/oauth2-auto.plist for saved token."))'

echo ""
echo "=== Done ==="
echo ""
echo "If successful, you can now run M-x org-gcal-fetch inside Emacs."
echo "The OAuth token is stored in ~/.emacs.d/oauth2-auto.plist"
