;;; org-roam-second-brain.el --- Second Brain for org-roam -*- lexical-binding: t; -*-

;; Author: David Cruver <dcruver@users.noreply.github.com>
;; URL: https://github.com/dcruver/org-roam-second-brain
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.2"))

;;; Commentary:
;; Second Brain functionality for org-roam providing:
;; - Structured node types (person, project, idea, admin)
;; - Proactive surfacing (daily digest, stale projects, pending followups)
;; - Semantic link suggestions (via org-roam-vector-search)
;; - Interactive commands for Emacs-first workflow
;;
;; This package provides both interactive commands for direct Emacs use
;; and core functions that can be wrapped by API layers.

;;; Code:

(require 'org-roam)
(require 'org-roam-vector-search)
(require 'json)
(require 'subr-x)
(require 'cl-lib)

;;; ============================================================================
;;; CONFIGURATION (use customize interface for environment-specific settings)
;;; ============================================================================

(defgroup sb nil
  "Second Brain for org-roam."
  :group 'org-roam
  :prefix "sb/")

(defcustom sb/stale-days 5
  "Number of days without activity before a project is considered stale."
  :type 'integer
  :group 'sb)

(defcustom sb/similarity-threshold 0.6
  "Minimum similarity score for link suggestions.
Higher values (closer to 1.0) mean more similar notes only."
  :type 'float
  :group 'sb)

(defcustom sb/show-digest-on-startup t
  "Whether to show the daily digest when Emacs starts.
Set to nil if you prefer to call `sb/digest' manually."
  :type 'boolean
  :group 'sb)

(defcustom sb/proactive-suggestions nil
  "Whether to show proactive link suggestions when visiting notes.
Suggestions appear for notes that could be linked but aren't.
Disabled by default as it makes an API call on every file open."
  :type 'boolean
  :group 'sb)

(defcustom sb/directories
  '((person . "people")
    (project . "projects")
    (idea . "ideas")
    (admin . "admin")
    (blog . "blog"))
  "Subdirectories for each node type.
Alist mapping node type symbols to subdirectory names under `org-roam-directory'."
  :type '(alist :key-type symbol :value-type string)
  :group 'sb)

(defcustom sb/digest-buffer-name "*Second Brain Digest*"
  "Name of the buffer for displaying the daily digest."
  :type 'string
  :group 'sb)

(defcustom sb/suggestions-buffer-name "*Link Suggestions*"
  "Name of the buffer for displaying link suggestions."
  :type 'string
  :group 'sb)

(defcustom sb/hugo-base-dir "~/Projects/hullabalooing/blog"
  "Path to Hugo site root directory.
Used by blog publishing functions to locate content directory."
  :type 'directory
  :group 'sb)

(defcustom sb/hugo-sections
  '("signalscope" "health-tracking" "homelab" "gpu-ai"
    "second-brain" "cyberdeck" "side-projects" "writing")
  "Valid Hugo sections for blog posts.
Each section corresponds to a project area in the Hugo site."
  :type '(repeat string)
  :group 'sb)

(defcustom sb/blog-llm-function nil
  "Function to call for LLM operations in blog writing.
Should accept (PROMPT CONTENT) and return generated text.
If nil, AI functions will not be available."
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "LLM function"))
  :group 'sb)

;;; ============================================================================
;;; INTERNAL HELPERS
;;; ============================================================================

(defun sb/--ensure-directory (type)
  "Ensure subdirectory for TYPE exists, return the path."
  (let* ((subdir (cdr (assoc type sb/directories)))
         (dir (expand-file-name (or subdir "") org-roam-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun sb/--generate-id ()
  "Generate a unique ID for new notes."
  (format "%s" (time-convert nil 'integer)))

(defun sb/--create-filepath (title type)
  "Create filepath for a note with TITLE of TYPE."
  (let* ((dir (sb/--ensure-directory type))
         (id (sb/--generate-id))
         (slug (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title)))
         (filename (format "%s-%s.org" slug id)))
    (expand-file-name filename dir)))

(defun sb/--extract-properties (file)
  "Extract org properties from FILE as alist."
  (when (and file (file-exists-p file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (goto-char (point-min))
          (let ((props '()))
            (when (re-search-forward "^:PROPERTIES:" nil t)
              (while (and (forward-line 1)
                          (not (looking-at "^:END:"))
                          (not (eobp)))
                (when (looking-at "^:\\([^:]+\\):\\s-*\\(.*\\)\\s-*$")
                  (push (cons (downcase (match-string 1))
                              (match-string 2)) props))))
            props))
      (error '()))))

(defun sb/--file-modified-days-ago (file)
  "Return number of days since FILE was last modified."
  (when (and file (file-exists-p file))
    (let* ((mtime (file-attribute-modification-time (file-attributes file)))
           (now (current-time))
           (diff (time-subtract now mtime)))
      (/ (float-time diff) 86400.0))))

(defun sb/--extract-unchecked-items (file section-name)
  "Extract unchecked checkbox items from SECTION-NAME in FILE."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((items '()))
        (when (re-search-forward (format "^\\* %s$" (regexp-quote section-name)) nil t)
          (forward-line 1)
          (while (and (not (eobp))
                      (not (looking-at "^\\* ")))
            (when (looking-at "^- \\[ \\] \\(.+\\)$")
              (push (match-string 1) items))
            (forward-line 1)))
        (nreverse items)))))

(defun sb/--extract-link-names (text)
  "Extract all [[Name]] links from TEXT."
  (let ((names '())
        (start 0))
    (while (string-match "\\[\\[\\([^]]+\\)\\]\\]" text start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    (nreverse names)))

(defun sb/--node-exists-p (title)
  "Check if an org-roam node with TITLE exists."
  (org-roam-node-from-title-or-alias title))

(defun sb/--get-backlink-ids (node)
  "Get list of node IDs that link to NODE."
  (let* ((id (org-roam-node-id node))
         (backlinks (org-roam-db-query
                     [:select :distinct [source]
                      :from links
                      :where (= dest $s1)]
                     id)))
    (mapcar #'car backlinks)))

(defun sb/--node-from-file (file)
  "Get the org-roam node for FILE."
  (when file
    (let ((id (caar (org-roam-db-query
                     [:select [id] :from nodes :where (= file $s1) :limit 1]
                     file))))
      (when id (org-roam-node-from-id id)))))

(defun sb/--generate-embedding (file-path)
  "Generate embedding for FILE-PATH if org-roam-semantic is available."
  (condition-case err
      (when (fboundp 'org-roam-semantic-generate-embedding)
        (message "Generating embedding for: %s" (file-name-nondirectory file-path))
        (org-roam-semantic-generate-embedding file-path)
        ;; Save and close the buffer
        (let ((buf (find-buffer-visiting file-path)))
          (when buf
            (with-current-buffer buf
              (save-buffer)
              (kill-buffer))))
        t)
    (error
     (message "Warning: Failed to generate embeddings: %s"
              (error-message-string err))
     nil)))

;;; ============================================================================
;;; CORE FUNCTIONS (return elisp data structures)
;;; ============================================================================

(defun sb/core-create-node (type title properties &optional content)
  "Create a node of TYPE with TITLE and PROPERTIES.
PROPERTIES is an alist of additional properties to set.
CONTENT is optional body text for the note.
Returns the created node's ID and filepath."
  (let* ((filepath (sb/--create-filepath title type))
         (id (org-id-new))
         (timestamp (format-time-string "[%Y-%m-%d %a]")))

    (with-current-buffer (find-file-noselect filepath)
      (org-mode)
      (erase-buffer)
      ;; Properties drawer
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" id))
      (insert (format ":NODE-TYPE: %s\n" type))
      (dolist (prop properties)
        (when (cdr prop)
          (insert (format ":%s: %s\n" (upcase (car prop)) (cdr prop)))))
      (insert ":END:\n")
      (insert (format "#+title: %s\n\n" title))
      ;; Content
      (when content
        (insert content))
      (save-buffer)
      (kill-buffer (current-buffer)))

    ;; Sync database
    (org-roam-db-sync)

    ;; Generate embeddings
    (sb/--generate-embedding filepath)

    ;; Return result
    (list :id id :file filepath :title title :type type)))

(defun sb/core-ensure-person (name)
  "Ensure a person node exists for NAME, create minimal if not.
Returns the node ID (existing or newly created)."
  (let ((existing (sb/--node-exists-p name)))
    (if existing
        (org-roam-node-id existing)
      ;; Create minimal person node
      (let ((result (sb/core-create-node 'person name nil)))
        (plist-get result :id)))))

(defun sb/core-nodes-by-type (type)
  "Get all nodes of TYPE.
Returns list of plists with :node, :file, :title, :properties."
  (let ((results '())
        (seen-files (make-hash-table :test 'equal)))
    (dolist (node (org-roam-node-list))
      ;; Only file-level nodes (level 0)
      (when (= (org-roam-node-level node) 0)
        (let* ((file (org-roam-node-file node)))
          (unless (gethash file seen-files)
            (let* ((props (sb/--extract-properties file))
                   (node-type (cdr (assoc "node-type" props))))
              (when (and node-type (string= (downcase node-type) (symbol-name type)))
                (puthash file t seen-files)
                (push (list :node node
                            :file file
                            :title (org-roam-node-title node)
                            :properties props)
                      results)))))))
    (nreverse results)))

(defun sb/core-active-projects ()
  "Get active projects sorted by last modified.
Returns list of plists with project details."
  (let* ((projects (sb/core-nodes-by-type 'project))
         (active (seq-filter
                  (lambda (p)
                    (let ((status (cdr (assoc "status" (plist-get p :properties)))))
                      (and status (string= (downcase status) "active"))))
                  projects)))
    (mapcar
     (lambda (p)
       (let* ((file (plist-get p :file))
              (props (plist-get p :properties))
              (next-action (cdr (assoc "next-action" props)))
              (unchecked (sb/--extract-unchecked-items file "Next Actions"))
              (days-ago (sb/--file-modified-days-ago file)))
         (list :id (org-roam-node-id (plist-get p :node))
               :title (plist-get p :title)
               :file file
               :status "active"
               :next-action next-action
               :unchecked-actions unchecked
               :days-since-modified (round (or days-ago 0)))))
     active)))

(defun sb/core-stale-projects (&optional days)
  "Get projects with no activity in DAYS days (default `sb/stale-days').
Returns list of plists with project details."
  (let* ((threshold (or days sb/stale-days))
         (projects (sb/core-nodes-by-type 'project))
         (active (seq-filter
                  (lambda (p)
                    (let ((status (cdr (assoc "status" (plist-get p :properties)))))
                      (and status
                           (not (string= (downcase status) "done"))
                           (not (string= (downcase status) "someday")))))
                  projects))
         (stale (seq-filter
                 (lambda (p)
                   (let ((days-ago (sb/--file-modified-days-ago (plist-get p :file))))
                     (and days-ago (>= days-ago threshold))))
                 active)))
    (mapcar
     (lambda (p)
       (let* ((file (plist-get p :file))
              (props (plist-get p :properties)))
         (list :id (org-roam-node-id (plist-get p :node))
               :title (plist-get p :title)
               :file file
               :status (cdr (assoc "status" props))
               :next-action (cdr (assoc "next-action" props))
               :days-since-modified (round (sb/--file-modified-days-ago file)))))
     stale)))

(defun sb/core-pending-followups ()
  "Get people with unchecked follow-up items in notes that link to them.
Returns list of plists with person and followup details."
  (let* ((people (sb/core-nodes-by-type 'person))
         (with-followups
          (seq-filter
           (lambda (p)
             (let* ((node (plist-get p :node))
                    (name (plist-get p :title))
                    (backlink-ids (sb/--get-backlink-ids node))
                    (followups '()))
               ;; Check each backlinking note for unchecked items mentioning this person
               (dolist (source-id backlink-ids)
                 (when-let* ((source-node (org-roam-node-from-id source-id))
                             (source-file (org-roam-node-file source-node)))
                   (with-temp-buffer
                     (insert-file-contents source-file)
                     (goto-char (point-min))
                     (while (re-search-forward "^[ \t]*- \\[ \\] \\(.*\\)$" nil t)
                       (let ((item-text (match-string 1)))
                         (when (string-match-p (regexp-quote name) item-text)
                           (push (string-trim item-text) followups)))))))
               (> (length followups) 0)))
           people)))
    (mapcar
     (lambda (p)
       (let* ((node (plist-get p :node))
              (name (plist-get p :title))
              (backlink-ids (sb/--get-backlink-ids node))
              (followups '()))
         ;; Collect followups
         (dolist (source-id backlink-ids)
           (when-let* ((source-node (org-roam-node-from-id source-id))
                       (source-file (org-roam-node-file source-node)))
             (with-temp-buffer
               (insert-file-contents source-file)
               (goto-char (point-min))
               (while (re-search-forward "^[ \t]*- \\[ \\] \\(.*\\)$" nil t)
                 (let ((item-text (match-string 1)))
                   (when (string-match-p (regexp-quote name) item-text)
                     (push (string-trim item-text) followups)))))))
         (list :id (org-roam-node-id node)
               :name name
               :file (plist-get p :file)
               :backlink-count (length backlink-ids)
               :followups (nreverse followups)
               :followup-count (length followups))))
     with-followups)))

(defun sb/core-dangling-person-links ()
  "Find [[Name]] links in unchecked items where the person node doesn't exist.
Returns list of plists with name, item text, and source file."
  (let ((dangling '())
        (seen-names (make-hash-table :test 'equal)))
    (dolist (file (directory-files-recursively org-roam-directory "\\.org$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*- \\[ \\] \\(.*\\[\\[.+\\]\\].*\\)$" nil t)
          (let* ((item-text (match-string 1))
                 (names (sb/--extract-link-names item-text)))
            (dolist (name names)
              (unless (or (gethash name seen-names)
                          (sb/--node-exists-p name))
                (puthash name t seen-names)
                (push (list :name name
                            :item (string-trim item-text)
                            :file file)
                      dangling)))))))
    (nreverse dangling)))

(defun sb/core-unlinked-similar (file &optional threshold)
  "Find notes similar to FILE that aren't already linked.
Returns list of plists with file, title, and similarity."
  (when (fboundp 'org-roam-semantic-get-similar-data)
    (let* ((threshold (or threshold sb/similarity-threshold))
           (content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
           (similar (org-roam-semantic-get-similar-data content 20 threshold))
           ;; Get existing links from this file
           (existing-links (make-hash-table :test 'equal))
           (node (sb/--node-from-file file)))
      ;; Add self to exclude list
      (puthash file t existing-links)
      ;; Add forward links
      (when node
        (let ((links (org-roam-db-query
                      [:select [dest] :from links :where (= source $s1)]
                      (org-roam-node-id node))))
          (dolist (link links)
            (when-let ((dest-node (org-roam-node-from-id (car link))))
              (puthash (org-roam-node-file dest-node) t existing-links)))))
      ;; Add backlinks
      (when node
        (let ((backlinks (sb/--get-backlink-ids node)))
          (dolist (id backlinks)
            (when-let ((backlink-node (org-roam-node-from-id id)))
              (puthash (org-roam-node-file backlink-node) t existing-links)))))
      ;; Filter to unlinked only
      (let ((unlinked (seq-filter
                       (lambda (result)
                         (not (gethash (car result) existing-links)))
                       similar)))
        (mapcar
         (lambda (result)
           (let* ((sim-file (car result))
                  (similarity (cadr result))
                  (title (with-temp-buffer
                           (insert-file-contents sim-file)
                           (goto-char (point-min))
                           (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
                               (match-string 1)
                             (file-name-base sim-file)))))
             (list :file sim-file
                   :title title
                   :similarity similarity)))
         unlinked)))))

(defun sb/core-digest-data ()
  "Gather all data for daily digest.
Returns plist with all digest information."
  (let ((active (sb/core-active-projects))
        (followups (sb/core-pending-followups))
        (stale (sb/core-stale-projects))
        (dangling (sb/core-dangling-person-links)))
    (list :generated-at (format-time-string "%Y-%m-%d %H:%M:%S")
          :active-projects (list :total (length active)
                                 :top-3 (seq-take active 3))
          :pending-followups (list :total (length followups)
                                   :people followups)
          :stale-projects (list :total (length stale)
                                :projects stale)
          :dangling-links (list :total (length dangling)
                                :items (seq-take dangling 5)))))

;;; ============================================================================
;;; BLOG POST FUNCTIONS
;;; ============================================================================

(defun sb/--blog-template (title section)
  "Generate blog post template for TITLE in SECTION."
  (format "* Outline
- [ ] Introduction
- [ ] Main points
- [ ] Conclusion

* Draft

* Research
"))

(defun sb/core-create-blog (title section &optional slug)
  "Create a blog post node with TITLE for SECTION.
SLUG is optional; if nil, generated from title.
Returns plist with :id, :file, :title, :section."
  (let* ((slug (or slug (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title))))
         (filepath (sb/--create-filepath title 'blog))
         (id (org-id-new))
         (date (format-time-string "[%Y-%m-%d %a]"))
         (hugo-section (format "%s/posts" section)))

    (with-current-buffer (find-file-noselect filepath)
      (org-mode)
      (erase-buffer)
      ;; Properties drawer
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" id))
      (insert ":NODE-TYPE: blog\n")
      (insert (format ":EXPORT_FILE_NAME: %s\n" slug))
      (insert (format ":EXPORT_HUGO_SECTION: %s\n" hugo-section))
      (insert ":END:\n")
      (insert (format "#+title: %s\n" title))
      (insert (format "#+date: %s\n" date))
      (insert "#+hugo_draft: true\n")
      (insert "#+hugo_tags: \n")
      (insert (format "#+hugo_categories: %s\n\n"
                      (capitalize (replace-regexp-in-string "-" " " section))))
      ;; Content template
      (insert (sb/--blog-template title section))
      (save-buffer)
      (kill-buffer (current-buffer)))

    ;; Sync database
    (org-roam-db-sync)

    ;; Generate embeddings
    (sb/--generate-embedding filepath)

    ;; Return result
    (list :id id :file filepath :title title :section section :slug slug)))

(defun sb/core-blog-posts (&optional draft-filter)
  "Get all blog posts, optionally filtered by draft status.
DRAFT-FILTER can be:
  nil     - return all posts
  'draft  - return only drafts
  'published - return only published posts
Returns list of plists with post details."
  (let ((posts (sb/core-nodes-by-type 'blog)))
    (when draft-filter
      (setq posts
            (seq-filter
             (lambda (p)
               (let* ((file (plist-get p :file))
                      (is-draft (sb/--blog-is-draft file)))
                 (if (eq draft-filter 'draft)
                     is-draft
                   (not is-draft))))
             posts)))
    (mapcar
     (lambda (p)
       (let* ((file (plist-get p :file))
              (props (plist-get p :properties))
              (section (cdr (assoc "export_hugo_section" props)))
              (days-ago (sb/--file-modified-days-ago file)))
         (list :id (org-roam-node-id (plist-get p :node))
               :title (plist-get p :title)
               :file file
               :section section
               :draft (sb/--blog-is-draft file)
               :days-since-modified (round (or days-ago 0)))))
     posts)))

(defun sb/--blog-is-draft (file)
  "Check if blog FILE is marked as draft."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward "^#\\+hugo_draft:\\s-*\\(true\\|false\\)" nil t)
          (string= (match-string 1) "true")
        t))))  ; Default to draft if not specified

(defun sb/--blog-set-draft-value (file value)
  "Set draft status in blog FILE to VALUE (t or nil)."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (if (re-search-forward "^#\\+hugo_draft:\\s-*\\(true\\|false\\)" nil t)
          (replace-match (format "#+hugo_draft: %s" (if value "true" "false")))
        ;; Insert if not found
        (goto-char (point-min))
        (when (re-search-forward "^#\\+date:" nil t)
          (end-of-line)
          (insert (format "\n#+hugo_draft: %s" (if value "true" "false")))))
      (save-buffer))))

(defun sb/--blog-get-property (file property)
  "Get PROPERTY value from blog FILE."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward (format "^:%s:\\s-*\\(.+\\)$"
                                       (upcase property)) nil t)
        (string-trim (match-string 1))))))

(defun sb/--blog-validate (file)
  "Validate blog FILE has required properties for publishing.
Returns nil if valid, or a string describing the issue."
  (let ((export-file-name (sb/--blog-get-property file "EXPORT_FILE_NAME"))
        (export-section (sb/--blog-get-property file "EXPORT_HUGO_SECTION")))
    (cond
     ((not export-file-name) "Missing EXPORT_FILE_NAME property")
     ((not export-section) "Missing EXPORT_HUGO_SECTION property")
     ((not (member (car (split-string export-section "/"))
                   sb/hugo-sections))
      (format "Invalid section: %s" export-section))
     (t nil))))

;;; ============================================================================
;;; BUFFER MODE (for digest and suggestions displays)
;;; ============================================================================

(defvar sb/buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'sb/buffer-next-item)
    (define-key map (kbd "p") 'sb/buffer-prev-item)
    (define-key map (kbd "RET") 'sb/buffer-visit-item)
    (define-key map (kbd "g") 'sb/buffer-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "i") 'sb/buffer-insert-link)
    (define-key map (kbd "c") 'sb/buffer-create-person)
    (define-key map (kbd "x") 'sb/buffer-dismiss-item)
    map)
  "Keymap for `sb/buffer-mode'.")

(define-derived-mode sb/buffer-mode special-mode "SB"
  "Major mode for Second Brain buffer displays.

\\{sb/buffer-mode-map}"
  (setq-local revert-buffer-function #'sb/buffer-do-refresh)
  (setq-local truncate-lines t)
  ;; Explicitly use our keymap to override any inherited bindings
  (use-local-map sb/buffer-mode-map))

;; Evil mode compatibility
(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) sb/buffer-mode-map
    (kbd "RET") 'sb/buffer-visit-item
    (kbd "j") 'sb/buffer-next-item
    (kbd "k") 'sb/buffer-prev-item
    (kbd "gr") 'sb/buffer-refresh
    (kbd "q") 'quit-window
    (kbd "yi") 'sb/buffer-insert-link
    (kbd "c") 'sb/buffer-create-person))

(defvar-local sb/buffer-items nil
  "List of items in the current buffer for navigation.")

(defvar-local sb/buffer-type nil
  "Type of content in buffer: 'digest, 'suggestions, 'followups, 'stale.")

(defvar-local sb/buffer-source-file nil
  "Source file for suggestion buffers.")

(defun sb/buffer-next-item ()
  "Move to next item in the buffer."
  (interactive)
  (let ((next (next-single-property-change (point) 'sb-item)))
    (when next
      (goto-char next)
      (when (get-text-property (point) 'sb-item)
        (sb/--highlight-current-item)))))

(defun sb/buffer-prev-item ()
  "Move to previous item in the buffer."
  (interactive)
  (let ((prev (previous-single-property-change (point) 'sb-item)))
    (when prev
      (goto-char prev)
      (let ((start (previous-single-property-change (point) 'sb-item)))
        (when start (goto-char start)))
      (when (get-text-property (point) 'sb-item)
        (sb/--highlight-current-item)))))

(defun sb/buffer-visit-item ()
  "Visit the item at point."
  (interactive)
  ;; Search for property anywhere on current line
  (let ((item (or (get-text-property (point) 'sb-item)
                  (get-text-property (line-beginning-position) 'sb-item)
                  (save-excursion
                    (beginning-of-line)
                    (let ((eol (line-end-position)))
                      (catch 'found
                        (while (< (point) eol)
                          (when-let ((it (get-text-property (point) 'sb-item)))
                            (throw 'found it))
                          (forward-char 1))
                        nil))))))
    (if item
        (let ((file (plist-get item :file))
              (id (plist-get item :id)))
          (cond
           (file (find-file file))
           (id (org-roam-id-open id nil))
           (t (message "No file or ID for this item"))))
      (message "No item on this line"))))

(defun sb/buffer-refresh ()
  "Refresh the current buffer."
  (interactive)
  (sb/buffer-do-refresh nil nil))

(defun sb/buffer-do-refresh (_ignore-auto _noconfirm)
  "Actual refresh implementation."
  (pcase sb/buffer-type
    ('digest (sb/digest))
    ('suggestions (sb/suggest-links sb/buffer-source-file))
    ('followups (sb/followups))
    ('stale (sb/stale))
    ('dangling (sb/dangling))
    (_ (message "Unknown buffer type, can't refresh"))))

(defun sb/buffer-insert-link ()
  "Insert a link to the item at point into the kill ring."
  (interactive)
  (when-let ((item (get-text-property (point) 'sb-item)))
    (let ((id (plist-get item :id))
          (title (or (plist-get item :title) (plist-get item :name))))
      (if id
          (progn
            (kill-new (format "[[id:%s][%s]]" id title))
            (message "Copied link to %s" title))
        (message "No ID for this item")))))

(defun sb/buffer-create-person ()
  "Create a person node for the dangling link at point."
  (interactive)
  (when-let ((item (get-text-property (point) 'sb-item)))
    (let ((name (plist-get item :name)))
      (when name
        (sb/core-ensure-person name)
        (message "Created person node for %s" name)
        (sb/buffer-refresh)))))

(defun sb/buffer-dismiss-item ()
  "Dismiss the item at point (remove from view, not from data)."
  (interactive)
  (message "Item dismissed from view"))

(defun sb/--highlight-current-item ()
  "Highlight the current item."
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face 'highlight)
    (run-at-time 0.5 nil (lambda () (delete-overlay ov)))))

(defun sb/--insert-item (text item &optional indent)
  "Insert TEXT with ITEM as property, optionally with INDENT."
  (let ((start (point)))
    (when indent (insert (make-string indent ? )))
    (insert text)
    (insert "\n")
    ;; Property covers entire line including newline
    (put-text-property start (point) 'sb-item item)))

;;; ============================================================================
;;; INTERACTIVE COMMANDS
;;; ============================================================================

;;;###autoload
(defun sb/person (name &optional context)
  "Create a person node for NAME with optional CONTEXT.
When called interactively, prompts for name and context."
  (interactive "sPerson name: \nsContext (how do you know them?): ")
  (let* ((props (when (and context (not (string-empty-p context)))
                  (list (cons "context" context)
                        (cons "last-contact" (format-time-string "[%Y-%m-%d %a]")))))
         (content "* Follow-ups\n\n* Notes\n")
         (result (sb/core-create-node 'person name props content)))
    (find-file (plist-get result :file))
    (message "Created person: %s" name)))

;;;###autoload
(defun sb/project (title &optional next-action)
  "Create a project node for TITLE with optional NEXT-ACTION.
When called interactively, prompts for title and next action."
  (interactive "sProject title: \nsNext action: ")
  (let* ((props (list (cons "status" "active")
                      (when (and next-action (not (string-empty-p next-action)))
                        (cons "next-action" next-action))))
         (props (seq-filter #'identity props))
         (content (format "* Next Actions\n%s\n* Notes\n"
                          (if (and next-action (not (string-empty-p next-action)))
                              (format "- [ ] %s\n" next-action)
                            "")))
         (result (sb/core-create-node 'project title props content)))
    (find-file (plist-get result :file))
    (message "Created project: %s" title)))

;;;###autoload
(defun sb/idea (title one-liner)
  "Create an idea node for TITLE with ONE-LINER summary.
When called interactively, prompts for both."
  (interactive "sIdea title: \nsOne-liner (brief insight): ")
  (let* ((props (list (cons "one-liner" one-liner)))
         (content (format "* One-liner\n%s\n\n* Elaboration\n" one-liner))
         (result (sb/core-create-node 'idea title props content)))
    (find-file (plist-get result :file))
    (message "Created idea: %s" title)))

;;;###autoload
(defun sb/admin (title &optional due-date)
  "Create an admin task node for TITLE with optional DUE-DATE.
When called interactively, prompts for both."
  (interactive "sAdmin task: \nsDue date (YYYY-MM-DD, or empty): ")
  (let* ((props (list (cons "status" "todo")
                      (when (and due-date (not (string-empty-p due-date)))
                        (cons "due-date" (format "[%s]" due-date)))))
         (props (seq-filter #'identity props))
         (content "* Notes\n")
         (result (sb/core-create-node 'admin title props content)))
    (find-file (plist-get result :file))
    (message "Created admin task: %s" title)))

;;;###autoload
(defun sb/inbox (text)
  "Log TEXT to today's inbox, auto-creating person nodes for [[Name]] links."
  (interactive "sInbox entry: ")
  (let* ((today (format-time-string "%Y-%m-%d"))
         (daily-file (expand-file-name (concat today ".org")
                                       (expand-file-name "daily" org-roam-directory)))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (link-names (sb/--extract-link-names text))
         (created-people '()))

    ;; Auto-create person nodes
    (dolist (name link-names)
      (unless (sb/--node-exists-p name)
        (sb/core-ensure-person name)
        (push name created-people)))

    ;; Ensure daily directory and file exist
    (unless (file-exists-p (file-name-directory daily-file))
      (make-directory (file-name-directory daily-file) t))
    (unless (file-exists-p daily-file)
      (with-temp-file daily-file
        (insert (format "#+title: %s\n#+filetags: :daily:\n\n" today))))

    ;; Add to inbox
    (with-current-buffer (find-file-noselect daily-file)
      (goto-char (point-min))
      (if (re-search-forward "^\\* Inbox$" nil t)
          (progn (end-of-line) (newline))
        (goto-char (point-max))
        (unless (bolp) (newline))
        (insert "* Inbox\n"))
      (insert (format "- %s %s\n" timestamp text))
      (save-buffer))

    (when created-people
      (message "Logged to inbox. Created person nodes: %s"
               (string-join (nreverse created-people) ", ")))
    (unless created-people
      (message "Logged to inbox"))))

;;;###autoload
(defun sb/digest ()
  "Display the daily digest in a dedicated buffer."
  (interactive)
  (let ((data (sb/core-digest-data))
        (buf (get-buffer-create sb/digest-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'digest)

        ;; Header
        (insert (propertize "Second Brain Daily Digest\n" 'face 'bold))
        (insert (format "Generated: %s\n\n" (plist-get data :generated-at)))

        ;; Active Projects
        (let* ((projects (plist-get data :active-projects))
               (total (plist-get projects :total))
               (top-3 (plist-get projects :top-3)))
          (insert (propertize (format "Active Projects (%d)\n" total) 'face 'bold))
          (if (> total 0)
              (dolist (p top-3)
                (sb/--insert-item
                 (format "  %s%s"
                         (plist-get p :title)
                         (if-let ((na (plist-get p :next-action)))
                             (format " -> %s" na)
                           ""))
                 p 0))
            (insert "  No active projects\n"))
          (insert "\n"))

        ;; Pending Follow-ups
        (let* ((followups (plist-get data :pending-followups))
               (total (plist-get followups :total))
               (people (plist-get followups :people)))
          (insert (propertize (format "Pending Follow-ups (%d people)\n" total) 'face 'bold))
          (if (> total 0)
              (dolist (p (seq-take people 5))
                (sb/--insert-item
                 (format "  %s: %s"
                         (plist-get p :name)
                         (or (car (plist-get p :followups)) ""))
                 p 0))
            (insert "  No pending follow-ups\n"))
          (insert "\n"))

        ;; Dangling Links
        (let* ((dangling (plist-get data :dangling-links))
               (total (plist-get dangling :total))
               (items (plist-get dangling :items)))
          (when (> total 0)
            (insert (propertize (format "Untracked People (%d)\n" total) 'face 'bold))
            (dolist (item items)
              (sb/--insert-item
               (format "  [[%s]] - %s"
                       (plist-get item :name)
                       (truncate-string-to-width (plist-get item :item) 50))
               item 0))
            (insert "  Press 'c' to create person nodes\n\n")))

        ;; Stale Projects
        (let* ((stale (plist-get data :stale-projects))
               (total (plist-get stale :total))
               (projects (plist-get stale :projects)))
          (insert (propertize (format "Might Be Stuck (%d)\n" total) 'face 'bold))
          (if (> total 0)
              (dolist (p (seq-take projects 3))
                (sb/--insert-item
                 (format "  %s (%d days)"
                         (plist-get p :title)
                         (plist-get p :days-since-modified))
                 p 0))
            (insert "  All projects recently active\n"))
          (insert "\n"))

        ;; Footer
        (insert "\n")
        (insert "Keys: n/p=navigate, RET=open, g=refresh, q=quit\n")
        (insert "      i=copy link, c=create person, x=dismiss\n")

        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/followups ()
  "Display all pending follow-ups."
  (interactive)
  (let ((data (sb/core-pending-followups))
        (buf (get-buffer-create "*Follow-ups*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'followups)

        (insert (propertize "Pending Follow-ups\n\n" 'face 'bold))

        (if data
            (dolist (p data)
              (insert (propertize (format "%s\n" (plist-get p :name)) 'face 'bold))
              (dolist (followup (plist-get p :followups))
                (sb/--insert-item (format "  - [ ] %s" followup) p 0))
              (insert "\n"))
          (insert "No pending follow-ups.\n"))

        (insert "\nKeys: n/p=navigate, RET=open person, g=refresh, q=quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/stale ()
  "Display stale projects."
  (interactive)
  (let ((data (sb/core-stale-projects))
        (buf (get-buffer-create "*Stale Projects*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'stale)

        (insert (propertize (format "Stale Projects (>%d days)\n\n" sb/stale-days) 'face 'bold))

        (if data
            (dolist (p data)
              (sb/--insert-item
               (format "%s - %d days%s"
                       (plist-get p :title)
                       (plist-get p :days-since-modified)
                       (if-let ((na (plist-get p :next-action)))
                           (format " [%s]" na)
                         ""))
               p 0))
          (insert "No stale projects. All actively maintained.\n"))

        (insert "\nKeys: n/p=navigate, RET=open, g=refresh, q=quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/dangling ()
  "Display dangling person links (untracked people)."
  (interactive)
  (let ((data (sb/core-dangling-person-links))
        (buf (get-buffer-create "*Untracked People*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'dangling)

        (insert (propertize "Untracked People\n" 'face 'bold))
        (insert "These [[Name]] links don't have person nodes.\n\n")

        (if data
            (dolist (item data)
              (sb/--insert-item
               (format "[[%s]] in: %s"
                       (plist-get item :name)
                       (truncate-string-to-width (plist-get item :item) 40))
               item 0))
          (insert "All person links are tracked.\n"))

        (insert "\nKeys: n/p=navigate, c=create person, g=refresh, q=quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/suggest-links (&optional file)
  "Find notes similar to FILE that aren't already linked.
If FILE is nil, uses the current buffer's file."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
         (suggestions (when file (sb/core-unlinked-similar file))))
    (if suggestions
        (let ((buf (get-buffer-create sb/suggestions-buffer-name)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (sb/buffer-mode)
              (setq sb/buffer-type 'suggestions)
              (setq sb/buffer-source-file file)

              (insert (propertize "Link Suggestions\n" 'face 'bold))
              (insert (format "For: %s\n\n" (file-name-nondirectory file)))

              (dolist (s suggestions)
                (sb/--insert-item
                 (format "%.2f  %s"
                         (plist-get s :similarity)
                         (plist-get s :title))
                 s 0))

              (insert "\nKeys: n/p=navigate, RET=open, i=copy link, g=refresh, q=quit\n")
              (goto-char (point-min))))
          (display-buffer buf))
      (message "No unlinked similar notes found (or semantic search unavailable)"))))

;;;###autoload
(defun sb/weekly ()
  "Display weekly review data."
  (interactive)
  (let* ((active (sb/core-active-projects))
         (stale (sb/core-stale-projects))
         (followups (sb/core-pending-followups))
         (buf (get-buffer-create "*Weekly Review*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)

        (insert (propertize "Weekly Review\n\n" 'face 'bold))
        (insert (format "Date: %s\n\n" (format-time-string "%Y-%m-%d")))

        ;; Projects summary
        (insert (propertize "Projects\n" 'face 'bold))
        (insert (format "  Active: %d\n" (length active)))
        (insert (format "  Stale (>%d days): %d\n\n" sb/stale-days (length stale)))

        ;; People summary
        (insert (propertize "People\n" 'face 'bold))
        (insert (format "  With pending follow-ups: %d\n\n" (length followups)))

        ;; Action items
        (insert (propertize "Suggested Actions\n" 'face 'bold))
        (when stale
          (insert "  - Review stale projects (M-x sb/stale)\n"))
        (when followups
          (insert "  - Check pending follow-ups (M-x sb/followups)\n"))
        (insert "  - Run sb/suggest-links on key notes\n")

        (insert "\nPress 'g' to refresh, 'q' to quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;; ============================================================================
;;; BLOG INTERACTIVE COMMANDS
;;; ============================================================================

;;;###autoload
(defun sb/blog (title)
  "Create a new blog post with TITLE.
Prompts for Hugo section interactively."
  (interactive "sBlog post title: ")
  (let* ((section (completing-read "Hugo section: " sb/hugo-sections nil t))
         (result (sb/core-create-blog title section)))
    (find-file (plist-get result :file))
    (message "Created blog post: %s in %s" title section)))

;;;###autoload
(defun sb/blog-set-draft ()
  "Toggle draft status of the current blog post."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Not visiting a file"))
    (let ((is-draft (sb/--blog-is-draft file)))
      (sb/--blog-set-draft-value file (not is-draft))
      (revert-buffer t t)
      (message "Draft status: %s" (if is-draft "false (ready to publish)" "true (draft)")))))

;;;###autoload
(defun sb/blog-list (&optional filter)
  "List all blog posts, optionally filtered by FILTER.
FILTER can be: draft, published, or nil for all."
  (interactive
   (list (completing-read "Filter (empty for all): "
                          '("" "draft" "published") nil t)))
  (let* ((draft-filter (cond
                        ((string= filter "draft") 'draft)
                        ((string= filter "published") 'published)
                        (t nil)))
         (posts (sb/core-blog-posts draft-filter))
         (buf (get-buffer-create "*Blog Posts*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'blog-list)

        (insert (propertize (format "Blog Posts%s (%d)\n\n"
                                    (if (and filter (not (string-empty-p filter)))
                                        (format " [%s]" filter)
                                      "")
                                    (length posts))
                            'face 'bold))

        (if posts
            (dolist (p posts)
              (sb/--insert-item
               (format "[%s] %s (%s, %dd ago)"
                       (if (plist-get p :draft) "DRAFT" "LIVE")
                       (plist-get p :title)
                       (or (plist-get p :section) "?")
                       (plist-get p :days-since-modified))
               p 0))
          (insert "No blog posts found.\n"))

        (insert "\nKeys: n/p=navigate, RET=open, g=refresh, q=quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/blog-from-idea ()
  "Create a blog post from an existing idea node.
Prompts to select an idea and Hugo section."
  (interactive)
  (let* ((ideas (sb/core-nodes-by-type 'idea))
         (idea-titles (mapcar (lambda (i) (plist-get i :title)) ideas))
         (selected-title (completing-read "Idea to convert: " idea-titles nil t))
         (selected-idea (seq-find (lambda (i) (string= (plist-get i :title) selected-title)) ideas))
         (section (completing-read "Hugo section: " sb/hugo-sections nil t)))
    (if selected-idea
        (let* ((idea-file (plist-get selected-idea :file))
               (idea-content (with-temp-buffer
                               (insert-file-contents idea-file)
                               (buffer-string)))
               (result (sb/core-create-blog selected-title section)))
          ;; Open the new blog post
          (find-file (plist-get result :file))
          ;; Add link to original idea in Research section
          (goto-char (point-max))
          (when (re-search-backward "^\\* Research$" nil t)
            (forward-line 1)
            (insert (format "- [[id:%s][%s]] (original idea)\n"
                            (org-roam-node-id (plist-get selected-idea :node))
                            selected-title)))
          (save-buffer)
          (message "Created blog post from idea: %s" selected-title))
      (user-error "Idea not found"))))

;;;###autoload
(defun sb/blog-generate-outline ()
  "Generate an outline for the current blog post using AI.
Requires `sb/blog-llm-function' to be configured."
  (interactive)
  (unless sb/blog-llm-function
    (user-error "sb/blog-llm-function not configured"))
  (let* ((file (buffer-file-name))
         (title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
                    (match-string 1))))
         (prompt (format "Generate a detailed blog post outline for: \"%s\"

Create 5-7 main sections with brief descriptions. Format as:
- [ ] Section Name - brief description of what to cover

Keep it practical and focused." title))
         (result (funcall sb/blog-llm-function prompt "")))
    (when result
      ;; Find the Outline section and replace content
      (goto-char (point-min))
      (when (re-search-forward "^\\* Outline$" nil t)
        (forward-line 1)
        (let ((start (point)))
          ;; Delete until next heading or end
          (if (re-search-forward "^\\* " nil t)
              (progn (beginning-of-line) (delete-region start (point)))
            (delete-region start (point-max)))
          (insert result "\n\n")))
      (save-buffer)
      (message "Outline generated"))))

;;;###autoload
(defun sb/blog-expand-section ()
  "Expand the current outline item to prose using AI.
Place cursor on an outline item, and AI will generate content."
  (interactive)
  (unless sb/blog-llm-function
    (user-error "sb/blog-llm-function not configured"))
  (let* ((title (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
                    (match-string 1))))
         (current-line (thing-at-point 'line t))
         (section-name (when (string-match "^-\\s-*\\[.?\\]\\s-*\\(.+\\)$" current-line)
                         (match-string 1 current-line)))
         (prompt (format "Write 2-3 paragraphs for a blog post section.
Blog title: \"%s\"
Section: \"%s\"

Write in a clear, conversational technical style. Be specific and practical."
                         title (or section-name current-line)))
         (result (funcall sb/blog-llm-function prompt "")))
    (when result
      ;; Insert in Draft section
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\* Draft$" nil t)
          (goto-char (point-max))
          (if (re-search-backward "^\\* " nil t)
              (forward-line 0)
            (goto-char (point-max)))
          (insert (format "\n** %s\n\n%s\n"
                          (or section-name "Section")
                          result))))
      (save-buffer)
      (message "Section expanded"))))

;;;###autoload
(defun sb/blog-edit-tone (tone)
  "Rewrite the selected region in the specified TONE using AI.
Prompts for tone: professional, casual, technical, friendly."
  (interactive
   (list (completing-read "Tone: "
                          '("professional" "casual" "technical" "friendly" "concise")
                          nil t)))
  (unless sb/blog-llm-function
    (user-error "sb/blog-llm-function not configured"))
  (unless (use-region-p)
    (user-error "Please select a region first"))
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format "Rewrite the following text in a %s tone.
Keep the same meaning but adjust the style. Return only the rewritten text.

Text to rewrite:
%s" tone text))
         (result (funcall sb/blog-llm-function prompt "")))
    (when result
      (delete-region (region-beginning) (region-end))
      (insert result)
      (message "Text rewritten in %s tone" tone))))

;;;###autoload
(defun sb/blog-link-research ()
  "Find semantically similar notes and add links to the Research section.
Uses embeddings to find related notes that aren't already linked."
  (interactive)
  (let* ((file (buffer-file-name))
         (suggestions (sb/core-unlinked-similar file)))
    (if suggestions
        (let* ((titles (mapcar (lambda (s)
                                 (format "%.2f %s"
                                         (plist-get s :similarity)
                                         (plist-get s :title)))
                               suggestions))
               (selected (completing-read-multiple "Link notes (comma-sep): " titles)))
          (when selected
            ;; Find Research section and add links
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^\\* Research$" nil t)
                (forward-line 1)
                (dolist (sel selected)
                  (let* ((title (string-trim (substring sel 5)))  ; Remove similarity prefix
                         (match (seq-find (lambda (s) (string= (plist-get s :title) title))
                                          suggestions)))
                    (when match
                      (let ((node (sb/--node-from-file (plist-get match :file))))
                        (when node
                          (insert (format "- [[id:%s][%s]]\n"
                                          (org-roam-node-id node)
                                          title)))))))))
            (save-buffer)
            (message "Added %d research links" (length selected))))
      (message "No similar unlinked notes found"))))

;;;###autoload
(defun sb/blog-publish ()
  "Validate and export the current blog post via ox-hugo.
Sets draft to false and exports to the Hugo content directory."
  (interactive)
  (let* ((file (buffer-file-name))
         (validation-error (sb/--blog-validate file)))
    (if validation-error
        (user-error "Cannot publish: %s" validation-error)
      ;; Set draft to false
      (sb/--blog-set-draft-value file nil)
      (revert-buffer t t)
      ;; Export via ox-hugo
      (if (fboundp 'org-hugo-export-to-md)
          (progn
            (org-hugo-export-to-md)
            (message "Published! Export complete."))
        (user-error "ox-hugo not available. Install it with: M-x package-install RET ox-hugo")))))

;;; ============================================================================
;;; LIST AND SEARCH COMMANDS
;;; ============================================================================

;;;###autoload
(defun sb/projects (&optional status)
  "List all projects, optionally filtered by STATUS.
STATUS can be: active, waiting, blocked, someday, done, or nil for all."
  (interactive
   (list (completing-read "Filter by status (empty for all): "
                          '("" "active" "waiting" "blocked" "someday" "done")
                          nil t)))
  (let* ((all-projects (sb/core-nodes-by-type 'project))
         (filtered (if (and status (not (string-empty-p status)))
                       (seq-filter
                        (lambda (p)
                          (let ((s (cdr (assoc "status" (plist-get p :properties)))))
                            (and s (string= (downcase s) (downcase status)))))
                        all-projects)
                     all-projects))
         (buf (get-buffer-create "*Projects*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'projects)

        (insert (propertize (format "Projects%s (%d)\n\n"
                                    (if (and status (not (string-empty-p status)))
                                        (format " [%s]" status)
                                      "")
                                    (length filtered))
                            'face 'bold))

        (if filtered
            (dolist (p filtered)
              (let* ((props (plist-get p :properties))
                     (status (or (cdr (assoc "status" props)) "unknown"))
                     (next-action (cdr (assoc "next-action" props)))
                     (days (round (or (sb/--file-modified-days-ago (plist-get p :file)) 0))))
                (sb/--insert-item
                 (format "[%s] %s%s (%dd ago)"
                         status
                         (plist-get p :title)
                         (if next-action (format " -> %s" next-action) "")
                         days)
                 (list :id (org-roam-node-id (plist-get p :node))
                       :file (plist-get p :file)
                       :title (plist-get p :title))
                 0)))
          (insert "No projects found.\n"))

        (insert "\nKeys: n/p=navigate, RET=open, g=refresh, q=quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/people ()
  "List all people nodes."
  (interactive)
  (let* ((all-people (sb/core-nodes-by-type 'person))
         (buf (get-buffer-create "*People*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'people)

        (insert (propertize (format "People (%d)\n\n" (length all-people)) 'face 'bold))

        (if all-people
            (dolist (p all-people)
              (let* ((props (plist-get p :properties))
                     (context (cdr (assoc "context" props)))
                     (last-contact (cdr (assoc "last-contact" props))))
                (sb/--insert-item
                 (format "%s%s%s"
                         (plist-get p :title)
                         (if context (format " (%s)" context) "")
                         (if last-contact (format " [%s]" last-contact) ""))
                 (list :id (org-roam-node-id (plist-get p :node))
                       :file (plist-get p :file)
                       :name (plist-get p :title))
                 0)))
          (insert "No people found.\n"))

        (insert "\nKeys: n/p=navigate, RET=open, g=refresh, q=quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/search (query)
  "Search for nodes by title matching QUERY.
Searches across all node types."
  (interactive "sSearch for: ")
  (let* ((query-lower (downcase query))
         (all-nodes (org-roam-node-list))
         (matches (seq-filter
                   (lambda (node)
                     (and (= (org-roam-node-level node) 0)  ;; File-level only
                          (string-match-p (regexp-quote query-lower)
                                          (downcase (org-roam-node-title node)))))
                   all-nodes))
         (buf (get-buffer-create "*Search Results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'search)

        (insert (propertize (format "Search: \"%s\" (%d results)\n\n" query (length matches))
                            'face 'bold))

        (if matches
            (dolist (node matches)
              (let* ((file (org-roam-node-file node))
                     (props (sb/--extract-properties file))
                     (node-type (or (cdr (assoc "node-type" props)) "note")))
                (sb/--insert-item
                 (format "[%s] %s"
                         node-type
                         (org-roam-node-title node))
                 (list :id (org-roam-node-id node)
                       :file file
                       :title (org-roam-node-title node))
                 0)))
          (insert "No matches found.\n"))

        (insert "\nKeys: n/p=navigate, RET=open, g=refresh, q=quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/ideas ()
  "List all idea nodes."
  (interactive)
  (let* ((all-ideas (sb/core-nodes-by-type 'idea))
         (buf (get-buffer-create "*Ideas*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'ideas)

        (insert (propertize (format "Ideas (%d)\n\n" (length all-ideas)) 'face 'bold))

        (if all-ideas
            (dolist (p all-ideas)
              (let* ((props (plist-get p :properties))
                     (one-liner (cdr (assoc "one-liner" props))))
                (sb/--insert-item
                 (format "%s%s"
                         (plist-get p :title)
                         (if one-liner (format " - %s" (truncate-string-to-width one-liner 50)) ""))
                 (list :id (org-roam-node-id (plist-get p :node))
                       :file (plist-get p :file)
                       :title (plist-get p :title))
                 0)))
          (insert "No ideas found.\n"))

        (insert "\nKeys: n/p=navigate, RET=open, g=refresh, q=quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;; ============================================================================
;;; PROACTIVE FEATURES
;;; ============================================================================

(defun sb/--maybe-show-suggestions ()
  "Show link suggestions if enabled and viewing an org-roam file."
  (when (and sb/proactive-suggestions
             (derived-mode-p 'org-mode)
             (buffer-file-name)
             (org-roam-file-p))
    ;; Show in minibuffer instead of popup to be less intrusive
    (let ((suggestions (sb/core-unlinked-similar (buffer-file-name))))
      (when (and suggestions (> (length suggestions) 0))
        (message "SB: %d potential links found. Run M-x sb/suggest-links to see them."
                 (length suggestions))))))

(defun sb/--startup-digest ()
  "Show daily digest on startup if enabled."
  (when sb/show-digest-on-startup
    (run-with-idle-timer 2 nil #'sb/digest)))

;;; ============================================================================
;;; DOOM EMACS INTEGRATION
;;; ============================================================================

;; Hook into Doom's init if available
(with-eval-after-load 'doom
  (when (boundp 'doom-after-init-hook)
    (add-hook 'doom-after-init-hook #'sb/--startup-digest)))

;; Also work with vanilla Emacs
(unless (featurep 'doom)
  (add-hook 'emacs-startup-hook #'sb/--startup-digest))

;; Proactive suggestions hook
(add-hook 'org-roam-find-file-hook #'sb/--maybe-show-suggestions)

;;; ============================================================================
;;; KEY BINDINGS (C-c b prefix)
;;; ============================================================================

(defvar sb/blog-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'sb/blog)              ; Create new blog post
    (define-key map (kbd "l") 'sb/blog-list)         ; List blog posts
    (define-key map (kbd "d") 'sb/blog-set-draft)    ; Toggle draft status
    (define-key map (kbd "i") 'sb/blog-from-idea)    ; Create from idea
    (define-key map (kbd "o") 'sb/blog-generate-outline)  ; AI outline
    (define-key map (kbd "e") 'sb/blog-expand-section)    ; AI expand
    (define-key map (kbd "t") 'sb/blog-edit-tone)    ; AI tone
    (define-key map (kbd "r") 'sb/blog-link-research) ; Link research
    (define-key map (kbd "p") 'sb/blog-publish)      ; Publish
    map)
  "Keymap for blog commands under C-c b B.")

(defvar sb/command-map
  (let ((map (make-sparse-keymap)))
    ;; Create commands
    (define-key map (kbd "p") 'sb/person)
    (define-key map (kbd "P") 'sb/project)
    (define-key map (kbd "i") 'sb/idea)
    (define-key map (kbd "a") 'sb/admin)
    (define-key map (kbd "I") 'sb/inbox)
    ;; List commands (l prefix)
    (define-key map (kbd "l p") 'sb/projects)
    (define-key map (kbd "l e") 'sb/people)
    (define-key map (kbd "l i") 'sb/ideas)
    (define-key map (kbd "l b") 'sb/blog-list)  ; Also accessible via l b
    ;; Search
    (define-key map (kbd "/") 'sb/search)
    ;; Surfacing commands
    (define-key map (kbd "d") 'sb/digest)
    (define-key map (kbd "f") 'sb/followups)
    (define-key map (kbd "s") 'sb/stale)
    (define-key map (kbd "u") 'sb/dangling)
    (define-key map (kbd "L") 'sb/suggest-links)  ;; Capital L for link suggestions
    (define-key map (kbd "w") 'sb/weekly)
    ;; Blog commands (B prefix)
    (define-key map (kbd "B") sb/blog-map)
    map)
  "Keymap for Second Brain commands.")

(global-set-key (kbd "C-c b") sb/command-map)

(provide 'org-roam-second-brain)
;;; org-roam-second-brain.el ends here
