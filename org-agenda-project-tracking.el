;;; org-agenda-project-tracking.el --- Project tracking with org-agenda and journal integration -*- lexical-binding: t; -*-
;;
;; Author: Nabu (AI Assistant)
;; Created: 2026-01-28
;;
;;; Commentary:
;; Integrates org-agenda with org-roam project notes, including automatic
;; journal logging of TODO state changes with bidirectional links.
;;
;;; Code:


;;;; Agenda Configuration
(after! org-agenda
  ;; Point agenda at org-roam projects
  (setq! org-agenda-files (list (expand-file-name "projects/" org-roam-directory)
                                (expand-file-name "admin/" org-roam-directory)))
  
  ;; Custom agenda views
  (setq! org-agenda-custom-commands
         '(("p" "Projects Overview"
            ((tags-todo "+TODO=\"IN-PROGRESS\""
                        ((org-agenda-overriding-header "🔄 In Progress")))
             (tags-todo "+TODO=\"BLOCKED\""
                        ((org-agenda-overriding-header "⛔ Blocked")))
             (tags-todo "+TODO=\"TODO\"+PRIORITY=\"A\""
                        ((org-agenda-overriding-header "⚡ High Priority TODO")))
             (tags-todo "+TODO=\"TODO\""
                        ((org-agenda-overriding-header "📋 All TODOs")
                         (org-agenda-sorting-strategy '(priority-down))))))
           
           ("b" "Blocked Items"
            tags-todo "+TODO=\"BLOCKED\""
            ((org-agenda-overriding-header "⛔ Blocked Tasks Across All Projects")))
           
           ("i" "In Progress"
            tags-todo "+TODO=\"IN-PROGRESS\""
            ((org-agenda-overriding-header "🔄 Currently Working On")))
           
           ("h" "High Priority"
            tags-todo "+PRIORITY=\"A\""
            ((org-agenda-overriding-header "⚡ High Priority Tasks"))))))

;;;; Journal Integration - Log state changes to daily notes
(defun my/org-roam-log-state-change-to-daily ()
  "Log TODO state changes to today's org-roam daily note with backlinks."
  (when (and org-state 
             org-last-state
             (not (string= org-state org-last-state)))
    (let* ((timestamp (format-time-string "%H:%M"))
           (date-str (format-time-string "%Y-%m-%d"))
           (daily-file (expand-file-name 
                        (format "daily/%s.org" date-str)
                        org-roam-directory))
           (task-heading (substring-no-properties (org-get-heading t t t t)))
           (task-id (org-id-get-create))
           (project-title (save-excursion
                           (goto-char (point-min))
                           (if (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
                               (match-string-no-properties 1)
                             "No project")))
           (entry-text (format "* %s %s: [[id:%s][%s]] (%s)\n"
                              timestamp
                              org-state
                              task-id
                              task-heading
                              project-title)))
      
      ;; Ensure daily file exists with proper structure
      (unless (file-exists-p daily-file)
        (with-temp-buffer
          (insert (format ":PROPERTIES:\n:ID: %s\n:NODE-TYPE: daily\n:END:\n#+title: %s\n#+filetags: :daily:\n\n"
                         date-str date-str))
          (write-file daily-file)))
      
      ;; Append to daily file
      (with-current-buffer (find-file-noselect daily-file)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert entry-text)
        (save-buffer)
        (message "Logged %s → %s to daily journal" org-last-state org-state)))))

(after! org
  (add-hook 'org-after-todo-state-change-hook #'my/org-roam-log-state-change-to-daily))

;;;; Programmatic State Changes (for external tools like Nabu)
(defun my/org-roam-change-task-state (file heading new-state)
  "Change the TODO state of HEADING in FILE to NEW-STATE.
Use this from emacsclient to trigger hooks properly."
  (with-current-buffer (find-file-noselect (expand-file-name file))
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\*+ \\(TODO\\|IN-PROGRESS\\|BLOCKED\\|DONE\\|CANCELLED\\) "
                                   (regexp-quote heading)) nil t)
        (progn
          (org-todo new-state)
          (save-buffer)
          (format "Changed '%s' to %s" heading new-state))
      (error "Could not find task: %s" heading))))

(provide 'org-agenda-project-tracking)
;;; org-agenda-project-tracking.el ends here
