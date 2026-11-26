;;; orsb-blog.el --- Blog-writing commands for a folder of ox-hugo posts -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 3.1.1
;; Package-Requires: ((emacs "29.1") (org-roam "2.2"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:

;; Optional.  Load it to get the sb/blog-* commands under C-c b B.
;;
;; These commands work on a folder, not on a note type: set
;; `orsb-blog-directory' to the vault folder that holds the posts (and,
;; for `sb/blog-from-idea', `orsb-blog-idea-directory' to the folder that
;; holds ideas).  What a new post starts with (export properties, #+hugo_
;; lines) is best said in that folder's _schema.org; `sb/blog' also writes
;; the usual ones itself.

;;; Code:

(require 'org-roam)
(require 'orsb-core)
(require 'org-roam-second-brain)

(defcustom orsb-blog-directory nil
  "Vault folder (relative to `org-roam-directory') that holds blog posts.
nil disables the blog commands."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'sb)

(defcustom orsb-blog-idea-directory nil
  "Vault folder (relative to `org-roam-directory') whose notes `sb/blog-from-idea' offers."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'sb)

(defcustom sb/hugo-base-dir nil
  "Hugo site directory the blog commands export to; nil disables them."
  :type '(choice (const :tag "Not configured" nil) directory)
  :group 'sb)

(defcustom sb/hugo-sections nil
  "Hugo sections offered by the blog commands."
  :type '(repeat string)
  :group 'sb)

(defcustom sb/blog-llm-function nil
  "Function to call for LLM operations in blog writing.
Should accept (PROMPT CONTENT) and return generated text.
If nil, AI functions will not be available."
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "LLM function"))
  :group 'sb)

(defun orsb-blog--notes-in (dir)
  "Return the notes in the vault folder DIR, each a plist.
Keys: :node, :file, :title and :properties (an alist, keys lowercase)."
  (unless dir (user-error "Set `orsb-blog-directory' (and `orsb-blog-idea-directory') to use the blog commands"))
  (let ((root (file-name-as-directory (expand-file-name dir org-roam-directory))) out)
    (dolist (node (org-roam-node-list))
      (when (and (= (org-roam-node-level node) 0)
                 (string-prefix-p root (expand-file-name (org-roam-node-file node))))
        (push (list :node node :file (org-roam-node-file node) :title (org-roam-node-title node)
                    :properties (sb/--extract-properties (org-roam-node-file node)))
              out)))
    (nreverse out)))

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
Returns plist with :id, :file, :title, :section, :slug."
  (unless sb/hugo-base-dir
    (user-error "Set `sb/hugo-base-dir' to use the blog commands"))
  (let* ((slug (or slug (orsb-core--slug title)))
         (node (orsb-core-create-node
                title
                :dir (expand-file-name (or orsb-blog-directory (user-error "Set `orsb-blog-directory' to use the blog commands")) org-roam-directory)
                :drawer `(("STATUS" . "draft")
                          ("EXPORT_FILE_NAME" . ,slug)
                          ("EXPORT_HUGO_SECTION" . ,(format "%s/posts" section)))
                :keywords `(("date" . ,(format-time-string "[%Y-%m-%d %a]"))
                            ("hugo_base_dir" . ,sb/hugo-base-dir)
                            ("hugo_draft" . "true")
                            ("hugo_categories" . ,(capitalize (replace-regexp-in-string "-" " " section))))
                :body (sb/--blog-template title section))))
    (list :id (org-roam-node-id node) :file (org-roam-node-file node)
          :title title :section section :slug slug)))

(defun sb/core-blog-posts (&optional draft-filter)
  "Get all blog posts, optionally filtered by draft status.
DRAFT-FILTER can be:
  nil     - return all posts
  'draft  - return only drafts
  'published - return only published posts
Returns list of plists with post details."
  (let ((posts (orsb-blog--notes-in orsb-blog-directory)))
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

(defun sb/--blog-has-content (file)
  "Check if blog FILE has substantial content beyond template."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; Look for Draft section with content
      (when (re-search-forward "^\\* Draft" nil t)
        (let ((start (point)))
          (if (re-search-forward "^\\* " nil t)
              (> (- (match-beginning 0) start) 50)
            (> (- (point-max) start) 50)))))))

(defun sb/--blog-outline-progress (file)
  "Get outline progress for blog FILE as (completed . total)."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((checked 0) (unchecked 0))
        (when (re-search-forward "^\\* Outline" nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\* " nil t)
                           (match-beginning 0)
                         (point-max)))))
            (while (re-search-forward "^- \\[\\(.\\)\\]" end t)
              (if (string= (match-string 1) "X")
                  (cl-incf checked)
                (cl-incf unchecked)))))
        (cons checked (+ checked unchecked))))))

(defun sb/core-blog-digest-data ()
  "Gather blog-related data for daily digest.
Returns plist with :drafts, :unpublished, and :ideas-for-blog."
  (let* ((all-posts (sb/core-blog-posts))
         (drafts (seq-filter (lambda (p) (plist-get p :draft)) all-posts))
         (published (seq-filter (lambda (p) (not (plist-get p :draft))) all-posts))
         (ideas (orsb-blog--notes-in orsb-blog-idea-directory)))
    ;; Enrich drafts with progress info
    (setq drafts
          (mapcar (lambda (d)
                    (let* ((file (plist-get d :file))
                           (progress (sb/--blog-outline-progress file))
                           (has-content (sb/--blog-has-content file)))
                      (append d (list :progress progress
                                      :has-content has-content))))
                  drafts))
    ;; Filter ideas that could make good blog posts (have interesting titles)
    ;; Exclude ideas that already have a blog post with similar title
    (let ((blog-titles (mapcar (lambda (p) (downcase (plist-get p :title))) all-posts)))
      (setq ideas
            (seq-filter
             (lambda (idea)
               (let* ((title (plist-get idea :title))
                      (title-lower (downcase title)))
                 ;; Keep ideas that:
                 ;; 1. Don't already have a matching blog post
                 ;; 2. Have a title longer than 10 chars (meaningful)
                 (and (> (length title) 10)
                      (not (member title-lower blog-titles))
                      ;; Check for idea-like titles (opinions, insights)
                      (or (string-match-p "\\b\\(why\\|how\\|what\\|the\\)\\b" title-lower)
                          (> (length title) 30)))))
             ideas)))
    (list :drafts (list :total (length drafts)
                        :items drafts)
          :published (list :total (length published)
                           :recent (seq-take
                                    (seq-sort-by (lambda (p)
                                                   (plist-get p :days-since-modified))
                                                 #'< published)
                                    3))
          :ideas-for-blog (list :total (length ideas)
                                :items (seq-take ideas 5)))))

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
  (let* ((ideas (orsb-blog--notes-in orsb-blog-idea-directory))
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


(define-key sb/command-map (kbd "B") sb/blog-map)

(provide 'orsb-blog)
;;; orsb-blog.el ends here
