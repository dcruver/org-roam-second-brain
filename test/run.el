;;; run.el --- Batch test runner for org-roam-second-brain -*- lexical-binding: t; -*-

;;; Commentary:
;; Usage: emacs -Q --batch -l test/run.el
;;
;; Dependencies (org-roam and what it pulls in) are found in this order:
;;   1. $ORSB_DEPS, a colon-separated list of directories whose immediate
;;      subdirectories are added to `load-path' (a straight.el build dir
;;      or an ELPA lisp dir), appended so the working copy always wins;
;;   2. `package-user-dir' when `package-initialize' finds org-roam.
;; The working copy (the parent of this file's directory) is prepended.

;;; Code:

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name ".." here))
       (deps (getenv "ORSB_DEPS")))
  (add-to-list 'load-path root)
  (add-to-list 'load-path here)
  (when (and deps (not (string-empty-p deps)))
    (dolist (dir (split-string deps ":" t))
      (dolist (sub (directory-files (expand-file-name dir) t "\\`[^.]"))
        (when (and (file-directory-p sub)
                   ;; never let an installed copy of this package shadow the tree under test
                   (not (string-match-p "org-roam-second-brain" sub)))
          (add-to-list 'load-path sub t)))))
  (unless (locate-library "org-roam")
    (require 'package)
    (package-initialize))
  (unless (locate-library "org-roam")
    (message "org-roam not found on load-path; set ORSB_DEPS to a directory of package builds")
    (kill-emacs 2))
  (require 'ert)
  ;; Test files may `require' each other's fixtures; load each once.
  (dolist (f (directory-files here t "-test\\.el\\'"))
    (unless (featurep (intern (file-name-base f)))
      (load f nil t)))
  (ert-run-tests-batch-and-exit (or (getenv "ORSB_TEST_SELECTOR") t)))

;;; run.el ends here
