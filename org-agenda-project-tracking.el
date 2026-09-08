;;; org-agenda-project-tracking.el --- Agenda views and TODO journaling for the second brain -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.2"))
;; Keywords: org-mode, roam, agenda
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:
;; Optional: org-agenda views over the project and admin notes, and a hook
;; that journals every TODO state change into the daily note with a link
;; back to the task.  Nothing here runs at load time; call
;; `orsb-agenda-setup' from your init file (it works in vanilla Emacs and
;; Doom alike).

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'org-roam)
(require 'orsb-core)

(defgroup orsb-agenda nil
  "org-roam-second-brain agenda integration."
  :group 'orsb
  :prefix "orsb-agenda-")

(defcustom orsb-agenda-directories '("project" "admin")
  "Node types (keys of `orsb-directories') whose directories feed org-agenda."
  :type '(repeat string)
  :group 'orsb-agenda)

(defcustom orsb-agenda-log-to-daily t
  "Journal TODO state changes into the daily note."
  :type 'boolean
  :group 'orsb-agenda)

(defconst orsb-agenda-custom-commands
  '(("p" "Projects Overview"
     ((tags-todo "+TODO=\"IN-PROGRESS\"" ((org-agenda-overriding-header "🔄 In Progress")))
      (tags-todo "+TODO=\"BLOCKED\"" ((org-agenda-overriding-header "⛔ Blocked")))
      (tags-todo "+TODO=\"TODO\"+PRIORITY=\"A\"" ((org-agenda-overriding-header "⚡ High Priority TODO")))
      (tags-todo "+TODO=\"TODO\"" ((org-agenda-overriding-header "📋 All TODOs")
                                   (org-agenda-sorting-strategy '(priority-down))))))
    ("b" "Blocked Items" tags-todo "+TODO=\"BLOCKED\""
     ((org-agenda-overriding-header "⛔ Blocked Tasks Across All Projects")))
    ("i" "In Progress" tags-todo "+TODO=\"IN-PROGRESS\""
     ((org-agenda-overriding-header "🔄 Currently Working On")))
    ("h" "High Priority" tags-todo "+PRIORITY=\"A\""
     ((org-agenda-overriding-header "⚡ High Priority Tasks"))))
  "The agenda views `orsb-agenda-setup' installs.")

(defun orsb-agenda-log-state-change ()
  "Journal the TODO state change at point into today's daily note."
  (when (and orsb-agenda-log-to-daily
             (bound-and-true-p org-state)
             (bound-and-true-p org-last-state)
             (not (string= org-state org-last-state))
             (buffer-file-name)
             (org-roam-file-p))
    (let* ((task-heading (substring-no-properties (org-get-heading t t t t)))
           (task-id (org-id-get-create))
           (project-title (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))) "No project"))
           (path (orsb-core-daily-file nil t)))
      (with-current-buffer (find-file-noselect path)
        (org-with-wide-buffer
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert (format "* %s %s: [[id:%s][%s]] (%s)\n"
                         (format-time-string "%H:%M") org-state task-id task-heading project-title)))
        (save-buffer))
      (message "Logged %s → %s to daily journal" org-last-state org-state))))

;;;###autoload
(defun orsb-agenda-setup ()
  "Point org-agenda at the project and admin notes, install the views,
and journal TODO changes to the daily note."
  (interactive)
  (setq org-agenda-files
        (mapcar (lambda (type) (expand-file-name (cdr (assoc type orsb-directories)) org-roam-directory))
                orsb-agenda-directories))
  (dolist (cmd orsb-agenda-custom-commands)
    (setf (alist-get (car cmd) org-agenda-custom-commands nil nil #'equal) (cdr cmd)))
  (add-hook 'org-after-todo-state-change-hook #'orsb-agenda-log-state-change))

(define-obsolete-function-alias 'my/org-roam-log-state-change-to-daily #'orsb-agenda-log-state-change "2.0")

(defun my/org-roam-change-task-state (file heading new-state)
  "Change the TODO state of HEADING in FILE to NEW-STATE (obsolete).
Use `orsb-core-set-todo' on the heading node instead."
  (declare (obsolete orsb-core-set-todo "2.0"))
  (let ((node (seq-find (lambda (n) (and (equal (org-roam-node-file n) (file-truename (expand-file-name file org-roam-directory)))
                                         (> (org-roam-node-level n) 0)
                                         (string-equal-ignore-case (org-roam-node-title n) heading)))
                        (org-roam-node-list))))
    (unless node (error "Could not find task: %s" heading))
    (orsb-core-set-todo node new-state)
    (format "Changed '%s' to %s" heading new-state)))

(provide 'org-agenda-project-tracking)
;;; org-agenda-project-tracking.el ends here
