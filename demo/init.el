;;; init.el --- The Emacs the clips are recorded in -*- lexical-binding: t; -*-

;;; Commentary:
;; Started by the tapes as  emacs -nw -Q -l demo/init.el  after demo/serve.sh.
;; It reads the same copy of the sample vault the demo server writes
;; (demo/work/vault) and asks that server for the proposals, the way a desktop
;; Emacs beside a headless notes server does (`orsb-server-url').

;;; Code:

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name ".." here))
       (work (expand-file-name "work/" here))
       (state (expand-file-name "state-emacs/" work))
       (deps (or (getenv "ORSB_DEPS")
                 (car (file-expand-wildcards "~/.config/emacs/.local/straight/build-*/")))))
  (add-to-list 'load-path root)
  (when deps
    (dolist (dir (split-string deps ":" t))
      (dolist (sub (directory-files (expand-file-name dir) t "\\`[^.]"))
        (when (and (file-directory-p sub)
                   (not (string-match-p "org-roam-second-brain" sub)))
          (add-to-list 'load-path sub t)))))
  (unless (locate-library "org-roam")
    (require 'package)
    (package-initialize))
  (make-directory state t)
  (setq org-roam-directory (expand-file-name "vault/" work)
        org-roam-db-location (expand-file-name "org-roam.db" state)
        org-id-locations-file (expand-file-name "org-id-locations" state)
        user-emacs-directory state
        make-backup-files nil
        create-lockfiles nil
        auto-save-default nil
        inhibit-startup-screen t
        initial-scratch-message nil
        url-proxy-services nil))

(require 'org-roam)
(require 'org-roam-second-brain)

(setq orsb-server-url "http://127.0.0.1:8019/mcp")
(org-roam-db-sync)
(orsb-mode 1)
(setq default-directory org-roam-directory)
(menu-bar-mode -1)
;; The package's screens take the whole frame, and the keys go to them.
(setq display-buffer-alist '(("\\`\\*Second Brain" (display-buffer-same-window))))
(global-auto-revert-mode 1)

;;; init.el ends here
