#!/bin/bash
# Deploy org-roam-second-brain to all machines
# Run from any machine - will deploy to all reachable targets

echo "=== Pushing to GitHub ==="
git push origin main || { echo "Failed to push"; exit 1; }

echo "=== Deploying to feynman ==="
if ssh -o ConnectTimeout=5 feynman.cruver.network "cd ~/.config/emacs/.local/straight/repos/org-roam-second-brain && git pull origin main" 2>/dev/null; then
    ssh feynman.cruver.network "emacsclient --eval '(progn (straight-rebuild-package \"org-roam-second-brain\") (require '\"'\"'org-roam-second-brain))'" 2>/dev/null && echo "  Reloaded in Emacs" || echo "  (Emacs not running)"
else
    echo "  (feynman offline)"
fi

echo "=== Deploying to n8n-backend ==="
if ssh -o ConnectTimeout=5 root@galileo.cruver.network "pct exec 121 -- bash -c 'cd /root/.config/emacs/.local/straight/repos/org-roam-second-brain && git pull origin main'" 2>/dev/null; then
    ssh root@galileo.cruver.network "pct exec 121 -- bash -c 'emacsclient --eval \"(progn (straight-rebuild-package \\\"org-roam-second-brain\\\") (require '\"'\"'org-roam-second-brain))\"'" 2>/dev/null && echo "  Reloaded in Emacs" || echo "  (Emacs not running)"
else
    echo "  (n8n-backend offline)"
fi

echo "=== Deploying to joule ==="
if ssh -o ConnectTimeout=5 joule.cruver.network "cd ~/.config/emacs/.local/straight/repos/org-roam-second-brain && git pull origin main" 2>/dev/null; then
    ssh joule.cruver.network "emacsclient -f ~/emacs-server/server --eval '(progn (straight-rebuild-package \"org-roam-second-brain\") (require '\"'\"'org-roam-second-brain))'" 2>/dev/null && echo "  Reloaded in Emacs" || echo "  (Emacs not running)"
else
    echo "  (joule offline)"
fi

echo "=== Done ==="
