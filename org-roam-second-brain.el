;;; org-roam-second-brain.el --- Second Brain for org-roam -*- lexical-binding: t; -*-

;; Author: David Cruver <dcruver@users.noreply.github.com>
;; URL: https://github.com/dcruver/org-roam-second-brain
;; Version: 3.1.3
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
;;
;; Also includes org-roam-api.el for MCP server integration.

;;; Code:

(require 'org-roam)
(require 'org-roam-vector-search)
(require 'orsb-core)
(require 'orsb-schema)
(require 'orsb-tools)
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

(defcustom sb/digest-buffer-name "*Second Brain Digest*"
  "Name of the buffer for displaying the daily digest."
  :type 'string
  :group 'sb)

(defcustom sb/suggestions-buffer-name "*Link Suggestions*"
  "Name of the buffer for displaying link suggestions."
  :type 'string
  :group 'sb)

(defcustom sb/daily-auto-link-mode 'first-only
  "Controls when daily auto-linking runs.
- 'first-only: Only add Related Notes on first substantial entry
- 'adaptive: Update when content changes significantly
- 'always: Update on every save
- 'manual: Never auto-link, only manual via sb/daily-link"
  :type '(choice (const :tag "First entry only" first-only)
                 (const :tag "Adaptive" adaptive)
                 (const :tag "Always" always)
                 (const :tag "Manual only" manual))
  :group 'sb)

(defcustom sb/daily-link-types 'both
  "What types of notes dailies should link to.
- 'both: Link to dailies and concept notes
- 'dailies-only: Only link to other daily files
- 'concepts-only: Only link to non-daily org-roam files"
  :type '(choice (const :tag "Both" both)
                 (const :tag "Dailies only" dailies-only)
                 (const :tag "Concepts only" concepts-only))
  :group 'sb)

(defcustom sb/daily-max-concept-links 8
  "Maximum concept note links in Related Notes section."
  :type 'integer
  :group 'sb)

(defcustom sb/daily-max-daily-links 5
  "Maximum daily note links in Related Notes section."
  :type 'integer
  :group 'sb)

(defcustom sb/daily-min-content-length 200
  "Minimum content length (chars) before auto-linking triggers."
  :type 'integer
  :group 'sb)

;;; ============================================================================
;;; INTERNAL HELPERS
;;; ============================================================================

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

(defun sb/--is-daily-file-p (file)
  "Check if FILE is in the daily directory."
  (and file (string-match-p "/daily/" file)))

(defun sb/--daily-has-related-section-p ()
  "Check if current buffer has a Related Notes section."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Related Notes" nil t)))

(defun sb/--daily-remove-related-section ()
  "Remove existing Related Notes section from current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\* Related Notes" nil t)
      (let ((start (match-beginning 0)))
        (forward-line 1)
        (while (and (not (eobp))
                    (or (looking-at "^- \\[\\[")
                        (looking-at "^\\*\\*\\* ")
                        (looking-at "^$")))
          (forward-line 1))
        (delete-region start (point))))))

;;; ============================================================================
;;; CORE FUNCTIONS (return elisp data structures)
;;; ============================================================================

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

(defun sb/core-daily-related-notes (file &optional threshold)
  "Find notes related to daily FILE for auto-linking.
Returns plist with :concepts and :dailies lists, each containing
(:file :title :similarity :id) plists."
  (when (and (fboundp 'org-roam-semantic-get-similar-data)
             (sb/--is-daily-file-p file))
    (let* ((threshold (or threshold sb/similarity-threshold))
           (content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
           (similar (org-roam-semantic-get-similar-data content 20 threshold))
           (existing-links (make-hash-table :test 'equal))
           (concepts '())
           (dailies '()))
      ;; Exclude self
      (puthash file t existing-links)
      ;; Classify and filter results
      (dolist (result similar)
        (let* ((sim-file (car result))
               (similarity (cadr result))
               (is-daily (sb/--is-daily-file-p sim-file)))
          (unless (gethash sim-file existing-links)
            (let* ((title (org-roam-semantic--get-title sim-file))
                   (node-id (org-roam-semantic--get-node-id sim-file))
                   (entry (list :file sim-file
                                :title title
                                :similarity similarity
                                :id node-id)))
              (if is-daily
                  (push entry dailies)
                (push entry concepts))))))
      ;; Filter by link-types preference
      (pcase sb/daily-link-types
        ('dailies-only (setq concepts nil))
        ('concepts-only (setq dailies nil)))
      ;; Apply limits
      (list :concepts (seq-take (nreverse concepts) sb/daily-max-concept-links)
            :dailies (seq-take (nreverse dailies) sb/daily-max-daily-links)))))

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
    (define-key map (kbd "a") 'sb/proposal-approve)
    (define-key map (kbd "B") 'sb/proposal-approve-batch)
    (define-key map (kbd "r") 'sb/proposal-reject)
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
    (kbd "a") 'sb/proposal-approve
    (kbd "B") 'sb/proposal-approve-batch
    (kbd "r") 'sb/proposal-reject))

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

(defun sb/buffer-refresh ()
  "Refresh the current buffer."
  (interactive)
  (sb/buffer-do-refresh nil nil))

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
;;; COMMANDS THAT KNOW NO NOTE TYPES
;;; ============================================================================
;; Everything here asks the vault what kinds of note it has (its _schema.org
;; files, through `orsb-schema') and goes through the same tool functions the
;; MCP server uses, so a note made from the keyboard obeys the same rules as
;; one made by an agent.

(defun sb/--call (fn args)
  "Call the tool function FN with ARGS; return its data.
A refusal becomes a `user-error'; rules broken in warn mode are shown."
  (let ((orsb-tools--warnings nil))
    (condition-case err
        (prog1 (funcall fn args)
          (when orsb-tools--warnings
            (message "Note rules: %s" (string-join orsb-tools--warnings "; "))))
      (orsb-error (user-error "%s" (nth 2 err))))))

;; Proposals live with the notes server that took them, not in the vault.  An
;; Emacs that is not that server (a desktop beside a headless server) asks it.

(defcustom orsb-server-url nil
  "URL of the notes server that holds the proposals, or nil.
For example \"http://feynman:8007/mcp\".  nil means this Emacs is the server:
proposals are read from its own change directory.  When set, the approval
screen lists, shows and decides proposals on that server, which also makes
the approved change."
  :type '(choice (const nil) string)
  :group 'orsb)

(defun sb/--remote-post (body)
  "POST the JSON string BODY to `orsb-server-url'; return the reply body."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")
                                     ("Accept" . "application/json, text/event-stream")))
        (url-request-data (encode-coding-string body 'utf-8)))
    (with-current-buffer (or (url-retrieve-synchronously orsb-server-url t t 30)
                             (user-error "No answer from the notes server at %s" orsb-server-url))
      (unwind-protect
          (progn (goto-char (point-min))
                 (unless (re-search-forward "\r?\n\r?\n" nil t)
                   (user-error "Bad answer from the notes server at %s" orsb-server-url))
                 (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8))
        (kill-buffer)))))

(defun sb/--remote-call (tool args)
  "Call TOOL with ARGS on the notes server; return its data.
A refusal is signalled as `orsb-error', as the local call would."
  (let* ((parse (lambda (text) (json-parse-string text :object-type 'alist :array-type 'array
                                                  :null-object nil :false-object nil)))
         (request (json-serialize `((jsonrpc . "2.0") (id . 1) (method . "tools/call")
                                    (params . ((name . ,tool)
                                               (arguments . ,(or args (make-hash-table))))))))
         (reply (funcall parse (sb/--remote-post request)))
         (text (alist-get 'text (aref (or (alist-get 'content (alist-get 'result reply))
                                          (user-error "The notes server answered without a result"))
                                      0)))
         (envelope (funcall parse text)))
    (if (alist-get 'ok envelope)
        (alist-get 'data envelope)
      (let ((e (alist-get 'error envelope)))
        (signal 'orsb-error (list (intern (or (alist-get 'code e) "error")) (alist-get 'message e)))))))

(defun sb/--proposals-call (tool fn args)
  "Call the proposal TOOL with ARGS: on the server when one is set, else FN here."
  (if orsb-server-url (sb/--remote-call tool args) (funcall fn args)))

(defun sb/--proposals (tool fn args)
  "Like `sb/--proposals-call', with a refusal shown as a `user-error'."
  (condition-case err (sb/--proposals-call tool fn args)
    (orsb-error (user-error "%s" (nth 2 err)))))

(defun sb/--items (data key)
  "The items of the section KEY of the review DATA, as a list."
  (append (alist-get 'items (alist-get key data)) nil))

(defun sb/--type-names ()
  "The note types this vault defines; \"note\" alone when it defines none."
  (or (mapcar (lambda (e) (alist-get 'type e)) (and (orsb-schema-present-p) (orsb-schema-tree)))
      '("note")))

(defun sb/--ask-p (row)
  "Non-nil when the schema property ROW has to be asked for on creation.
Required, with no default, and not something the server fills in."
  (let ((form (downcase (string-trim (or (nth 2 row) "")))))
    (and (orsb-schema--yes (nth 1 row))
         (string-empty-p (or (nth 3 row) ""))
         (not (string= form "id"))
         (not (string-match-p "set by the server" form)))))

(defun sb/--read-property (row schema)
  "Read a value for the schema property ROW of SCHEMA from the minibuffer."
  (let* ((name (car row))
         (form (string-trim (or (nth 2 row) "")))
         (f (downcase form))
         (prompt (format "%s%s: " name (if (string-empty-p form) "" (format " (%s)" form)))))
    (cond
     ((string-prefix-p "one of " f)
      (let ((rest (string-trim (substring form 7))))
        (completing-read prompt (split-string (or (orsb-schema-get schema (upcase rest)) rest) "[ ,]+" t) nil t)))
     ((string-prefix-p "date" f)
      (format "[%s]" (org-read-date nil nil nil prompt)))
     ((string-prefix-p "link" f)
      (let ((node (org-roam-node-read nil nil nil t prompt)))
        (format "[[id:%s][%s]]" (org-roam-node-id node) (org-roam-node-title node))))
     (t (read-string prompt)))))

;;;###autoload
(defun sb/new (type title)
  "Create a note of TYPE titled TITLE and visit it.
The types offered are the ones the vault defines.  The type's schema decides
the folder, the starting sections and which properties are asked for."
  (interactive
   (let ((type (completing-read "New note of type: " (sb/--type-names) nil t)))
     (list type (read-string (format "Title of the new %s: " type)))))
  (let* ((schema (and (orsb-schema-present-p)
                      (orsb-schema-for-directory (orsb-tools--type-directory type))))
         (props (delq nil (mapcar (lambda (row)
                                    (when (sb/--ask-p row)
                                      (cons (intern (upcase (car row))) (sb/--read-property row schema))))
                                  (alist-get 'properties schema))))
         (args `((title . ,title) (node_type . ,type) (properties . ,props)))
         (data (sb/--call #'orsb-tool-create-node args)))
    (when (and (eq (alist-get 'created data) :json-false)
               (y-or-n-p (format "A note titled \"%s\" exists. Create another? " title)))
      (setq data (sb/--call #'orsb-tool-create-node (append args '((on_duplicate . "create"))))))
    (if (eq (alist-get 'created data) t)
        (find-file (expand-file-name (alist-get 'file data) org-roam-directory))
      (when-let* ((dup (car (append (alist-get 'possible_duplicates data) nil)))
                  (id (alist-get 'id dup)))
        (org-roam-id-open id nil)))))

;;;###autoload
(defun sb/inbox (text)
  "Add TEXT to today's inbox."
  (interactive "sInbox entry: ")
  (sb/--call #'orsb-tool-log-to-inbox `((text . ,text)))
  (message "Logged to inbox"))

(defun sb/--record-line (r)
  "One line for the note record R: type, title, status, next action, age."
  (format "  [%s] %s%s%s%s"
          (or (alist-get 'node_type r) "note")
          (alist-get 'title r)
          (if-let* ((st (alist-get 'status r))) (format "  (%s)" st) "")
          (if-let* ((na (alist-get 'next_action r))) (format " -> %s" na) "")
          (if-let* ((d (alist-get 'days_since_modified r))) (format "  %dd" d) "")))

(defun sb/--record-item (r)
  "The navigation item for the note record R."
  (list :id (alist-get 'id r) :title (alist-get 'title r)))

(defun sb/--show (buffer-name type title fill)
  "Fill BUFFER-NAME, a `sb/buffer-mode' buffer of TYPE headed TITLE, by FILL."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type type)
        (insert (propertize (concat title "\n\n") 'face 'bold))
        (funcall fill)
        (insert "\nKeys: n/p move, RET open, g refresh, i copy link, q quit\n")
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun sb/digest ()
  "Show what needs attention: unfinished and stale notes, due tasks, the inbox,
and changes waiting for approval."
  (interactive)
  (let* ((data (sb/--call #'orsb-tool-get-review '((limit . 200))))
         (open (sb/--items data 'open))
         (stale (sb/--items data 'stale))
         (tasks (sb/--items data 'due_tasks))
         (inbox (sb/--items data 'inbox))
         ;; an unreachable server must not break the digest
         (pending (or (ignore-errors (alist-get 'total (sb/--proposals "get_proposals" #'orsb-tool-get-proposals nil))) 0)))
    (sb/--show
     sb/digest-buffer-name 'digest
     (format "Second Brain digest, %s" (format-time-string "%Y-%m-%d %H:%M"))
     (lambda ()
       (insert (propertize (format "Unfinished (%d)\n" (length open)) 'face 'bold))
       (if open
           (dolist (r (seq-take open 12)) (sb/--insert-item (sb/--record-line r) (sb/--record-item r) 0))
         (insert (if (orsb-schema-present-p)
                     "  Nothing open.\n"
                   "  This vault has no _schema.org files, so no note type has a lifecycle.\n")))
       (insert "\n" (propertize (format "Stale, untouched for %d days or more (%d)\n" orsb-stale-days (length stale)) 'face 'bold))
       (dolist (r (seq-take stale 12)) (sb/--insert-item (sb/--record-line r) (sb/--record-item r) 0))
       (insert "\n" (propertize (format "Tasks due within 3 days (%d)\n" (length tasks)) 'face 'bold))
       (dolist (tk (seq-take tasks 12))
         (sb/--insert-item (format "  %s%s  (%s)"
                                   (or (alist-get 'text tk) (alist-get 'heading tk) "")
                                   (if (alist-get 'overdue tk) "  OVERDUE" "")
                                   (or (alist-get 'title tk) ""))
                           (list :id (or (alist-get 'heading_id tk) (alist-get 'id tk)) :title (alist-get 'title tk))
                           0))
       (insert "\n" (propertize (format "Inbox (%d open)\n" (length inbox)) 'face 'bold))
       (dolist (e (seq-take inbox 8)) (insert (format "  %s\n" (alist-get 'text e))))
       (insert "\n" (propertize (format "Waiting for approval: %d" pending) 'face 'bold)
               (if (> pending 0) "   (M-x sb/proposals)\n" "\n"))))))

;;;###autoload
(defun sb/open ()
  "List every unfinished note: any type whose schema declares a lifecycle."
  (interactive)
  (let ((open (sb/--items (sb/--call #'orsb-tool-get-review '((limit . 1000))) 'open)))
    (sb/--show "*Second Brain: unfinished*" 'open (format "Unfinished notes (%d)" (length open))
               (lambda ()
                 (dolist (r open) (sb/--insert-item (sb/--record-line r) (sb/--record-item r) 0))))))

;;;###autoload
(defun sb/stale ()
  "List unfinished notes nobody has touched for `orsb-stale-days' days."
  (interactive)
  (let ((stale (sb/--items (sb/--call #'orsb-tool-get-review '((limit . 1000))) 'stale)))
    (sb/--show "*Second Brain: stale*" 'stale
               (format "Stale notes, untouched for %d days or more (%d)" orsb-stale-days (length stale))
               (lambda ()
                 (dolist (r stale) (sb/--insert-item (sb/--record-line r) (sb/--record-item r) 0))))))

(defvar-local sb/buffer-list-type nil
  "The note type a `sb/list' buffer shows.")

;;;###autoload
(defun sb/list (type)
  "List the notes of TYPE, one of the types the vault defines."
  (interactive (list (completing-read "List notes of type: " (sb/--type-names) nil t)))
  (let ((nodes (append (alist-get 'nodes (sb/--call #'orsb-tool-list-nodes `((node_type . ,type) (limit . 1000)))) nil)))
    (sb/--show (format "*Second Brain: %s*" type) 'list (format "Notes of type %s (%d)" type (length nodes))
               (lambda ()
                 (setq sb/buffer-list-type type)
                 (dolist (r nodes) (sb/--insert-item (sb/--record-line r) (sb/--record-item r) 0))))))

;;;###autoload
(defun sb/search (query)
  "List the notes whose title contains QUERY, each with its type."
  (interactive "sSearch titles for: ")
  (let* ((q (downcase query))
         (matches (seq-filter (lambda (node)
                                (and (= (org-roam-node-level node) 0)
                                     (string-search q (downcase (org-roam-node-title node)))))
                              (org-roam-node-list))))
    (sb/--show "*Search Results*" 'search (format "Search: \"%s\" (%d results)" query (length matches))
               (lambda ()
                 (dolist (node matches)
                   (sb/--insert-item (format "  [%s] %s" (orsb-tools--node-type node nil) (org-roam-node-title node))
                                     (list :id (org-roam-node-id node) :file (org-roam-node-file node)
                                           :title (org-roam-node-title node))
                                     0))))))

;;; Proposals: changes waiting for approval

(defun sb/--proposal-at-point ()
  "The id of the proposal on this line, or nil."
  (plist-get (or (get-text-property (point) 'sb-item)
                 (get-text-property (line-beginning-position) 'sb-item))
             :proposal))

;;;###autoload
(defun sb/proposals ()
  "List the changes agents have filed for approval.
RET shows what one would change, a approves it, r rejects it."
  (interactive)
  (let ((items (append (alist-get 'proposals (sb/--proposals "get_proposals" #'orsb-tool-get-proposals '((limit . 200)))) nil)))
    (sb/--show "*Second Brain: proposals*" 'proposals (format "Waiting for approval (%d)" (length items))
               (lambda ()
                 (unless items (insert "  Nothing is waiting.\n"))
                 (dolist (p items)
                   (sb/--insert-item
                    (format "  %s  %-12s %s%s%s%s"
                            (alist-get 'id p) (alist-get 'tool p)
                            (string-join (append (alist-get 'files p) nil) ", ")
                            (if (stringp (alist-get 'batch p)) (format "  {%s}" (alist-get 'batch p)) "")
                            (if (eq (alist-get 'stale p) t) "  [note changed since]" "")
                            (let ((why (alist-get 'reason p)))
                              (if (stringp why) (format "\n      %s" why) "")))
                    (list :proposal (alist-get 'id p) :batch (alist-get 'batch p))
                    0))
                 (insert "\n  RET preview, a approve, r reject, B approve the whole {batch} of the one at point\n")))))

(defun sb/proposal-show (id)
  "Show what proposal ID would change, line by line."
  (let ((p (sb/--proposals "get_proposals" #'orsb-tool-get-proposals `((id . ,id)))))
    (with-current-buffer (get-buffer-create "*Second Brain: proposal*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Proposal %s  (%s, %s)\n" id (alist-get 'tool p) (alist-get 'status p)) 'face 'bold))
        (when (stringp (alist-get 'reason p)) (insert "Why: " (alist-get 'reason p) "\n"))
        (when-let* ((changed (append (alist-get 'changed_since p) nil)))
          (insert (propertize (format "The note changed since this was filed: %s\n" (string-join changed ", ")) 'face 'warning)))
        (seq-doseq (c (alist-get 'would_change p))
          (insert "\n" (propertize (format "%s  (%s)\n" (alist-get 'file c) (alist-get 'action c)) 'face 'bold))
          (seq-doseq (l (alist-get 'lines_removed c)) (insert (propertize (concat "- " l "\n") 'face 'diff-removed)))
          (seq-doseq (l (alist-get 'lines_added c)) (insert (propertize (concat "+ " l "\n") 'face 'diff-added))))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun sb/proposal-approve ()
  "Approve the proposal at point: apply its change."
  (interactive)
  (let ((id (or (sb/--proposal-at-point) (user-error "No proposal on this line"))))
    (when (y-or-n-p (format "Apply proposal %s? " id))
      (condition-case err
          (sb/--proposals-call "decide_proposal" #'orsb-tool-decide-proposal `((id . ,id) (decision . "approve")))
        (orsb-error
         (if (and (eq (nth 1 err) 'conflict)
                  (y-or-n-p (format "%s.  Apply it to the note as it is now? " (nth 2 err))))
             (sb/--proposals "decide_proposal" #'orsb-tool-decide-proposal `((id . ,id) (decision . "approve") (force . t)))
           (user-error "%s" (nth 2 err)))))
      (message "Applied %s" id)
      (sb/proposals))))

(defun sb/proposal-approve-batch ()
  "Approve every pending proposal filed under the batch of the one at point."
  (interactive)
  (let* ((item (or (get-text-property (point) 'sb-item) (get-text-property (line-beginning-position) 'sb-item)))
         (batch (plist-get item :batch)))
    (unless (stringp batch) (user-error "The proposal on this line is not part of a batch"))
    (let* ((n (alist-get 'total (sb/--proposals "get_proposals" #'orsb-tool-get-proposals `((batch . ,batch) (limit . 1)))))
           (run (lambda (force)
                  (sb/--proposals "decide_proposal" #'orsb-tool-decide-proposal
                                  `((batch . ,batch) (decision . "approve") ,@(when force '((force . t))))))))
      (when (y-or-n-p (format "Apply all %s proposals of batch %s? " n batch))
        (let* ((r (funcall run nil))
               (refused (append (alist-get 'refused r) nil)))
          (when (and refused (y-or-n-p (format "%d applied, %d refused (first: %s).  Apply those to the notes as they are now? "
                                               (alist-get 'decided r) (length refused) (alist-get 'message (car refused)))))
            (setq r (funcall run t)))
          (message "Batch %s: %s applied, %d refused" batch (alist-get 'decided r) (length (append (alist-get 'refused r) nil))))
        (sb/proposals)))))

(defun sb/proposal-reject (note)
  "Reject the proposal at point, keeping NOTE as the reason."
  (interactive (list (if (sb/--proposal-at-point) (read-string "Why (optional): ") "")))
  (let ((id (or (sb/--proposal-at-point) (user-error "No proposal on this line"))))
    (sb/--proposals "decide_proposal" #'orsb-tool-decide-proposal
               `((id . ,id) (decision . "reject") ,@(unless (string-empty-p note) `((note . ,note)))))
    (message "Rejected %s" id)
    (sb/proposals)))

(defun sb/buffer-visit-item ()
  "Visit the item on this line: a note, or a proposal's preview."
  (interactive)
  (let ((item (or (get-text-property (point) 'sb-item)
                  (get-text-property (line-beginning-position) 'sb-item))))
    (cond ((null item) (message "No item on this line"))
          ((plist-get item :proposal) (sb/proposal-show (plist-get item :proposal)))
          ((plist-get item :file) (find-file (plist-get item :file)))
          ((plist-get item :id) (org-roam-id-open (plist-get item :id) nil))
          (t (message "Nothing to open for this item")))))

(defun sb/buffer-do-refresh (_ignore-auto _noconfirm)
  "Redo the listing this buffer shows."
  (pcase sb/buffer-type
    ('digest (sb/digest))
    ('suggestions (sb/suggest-links sb/buffer-source-file))
    ('open (sb/open))
    ('stale (sb/stale))
    ('list (sb/list sb/buffer-list-type))
    ('proposals (sb/proposals))
    (_ (message "This listing cannot be refreshed; run its command again"))))

;;; ============================================================================
;;; INTERACTIVE COMMANDS
;;; ============================================================================

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

;;; ============================================================================
;;; LIST AND SEARCH COMMANDS
;;; ============================================================================

;;; ============================================================================
;;; DAILY AUTO-LINKING COMMANDS
;;; ============================================================================

;;;###autoload
(defun sb/daily-link (&optional file)
  "Add or update Related Notes section in daily FILE.
If FILE is nil, uses current buffer's file."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
         (data (sb/core-daily-related-notes file)))
    (unless (sb/--is-daily-file-p file)
      (user-error "Not a daily file"))
    (when (or (plist-get data :concepts) (plist-get data :dailies))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          ;; Remove existing section
          (sb/--daily-remove-related-section)
          ;; Add new section at end
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n** Related Notes\n")
          ;; Concepts first
          (when-let ((concepts (plist-get data :concepts)))
            (insert (format "*** %s\n"
                            (file-name-sans-extension
                             (file-name-nondirectory file))))
            (dolist (c concepts)
              (insert (format "- [[id:%s][%s]] (%.3f)\n"
                              (plist-get c :id)
                              (plist-get c :title)
                              (plist-get c :similarity))))
            (insert "\n"))
          ;; Then dailies
          (when-let ((dailies (plist-get data :dailies)))
            (insert "*** Related Days\n")
            (dolist (d dailies)
              (let ((date (file-name-sans-extension
                           (file-name-nondirectory (plist-get d :file)))))
                (insert (format "- [[id:%s][%s]] (%.3f)\n"
                                (plist-get d :id)
                                date
                                (plist-get d :similarity)))))
            (insert "\n")))
        (message "Linked %d concepts and %d dailies"
                 (length (plist-get data :concepts))
                 (length (plist-get data :dailies)))))))

;;;###autoload
(defun sb/daily-link-all (&optional threshold)
  "Batch process all daily files to add Related Notes.
With prefix arg, prompts for similarity THRESHOLD."
  (interactive "P")
  (let* ((threshold (if threshold
                        (read-number "Similarity threshold: " sb/similarity-threshold)
                      sb/similarity-threshold))
         (daily-dir (expand-file-name "daily" org-roam-directory))
         (files (directory-files daily-dir t "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
         (processed 0)
         (linked 0))
    (message "Processing %d daily files..." (length files))
    (dolist (file files)
      (let ((initial-size (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-size))))
        (sb/daily-link file)
        (let ((new-size (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-size))))
          (when (> new-size initial-size)
            (cl-incf linked))))
      (cl-incf processed)
      (when (zerop (mod processed 10))
        (message "Processed %d/%d..." processed (length files))))
    (message "Done: %d files processed, %d updated" processed linked)))

;;;###autoload
(defun sb/daily-connections ()
  "Show connection report for all daily files."
  (interactive)
  (let* ((daily-dir (expand-file-name "daily" org-roam-directory))
         (files (directory-files daily-dir t "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
         (connections '())
         (buf (get-buffer-create "*Daily Connections*")))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward "\\[\\[id:[^]]+\\]\\[[^]]+\\]\\]" nil t)
            (cl-incf count))
          (push (cons (file-name-sans-extension (file-name-nondirectory file)) count)
                connections))))
    (setq connections (sort connections (lambda (a b) (> (cdr a) (cdr b)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sb/buffer-mode)
        (setq sb/buffer-type 'daily-connections)
        (insert (propertize "Daily Connections Report\n\n" 'face 'bold))
        (insert (format "Total daily files: %d\n" (length files)))
        (insert (format "Files with links: %d\n\n"
                        (length (seq-filter (lambda (x) (> (cdr x) 0)) connections))))
        (dolist (conn connections)
          (insert (format "  %s: %d links\n" (car conn) (cdr conn))))
        (insert "\nKeys: q=quit\n")
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

(defun sb/--maybe-auto-link-daily ()
  "Auto-link daily file on save if configured."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (sb/--is-daily-file-p (buffer-file-name))
             (> (buffer-size) sb/daily-min-content-length)
             (not (eq sb/daily-auto-link-mode 'manual)))
    (let ((should-link
           (pcase sb/daily-auto-link-mode
             ('first-only (not (sb/--daily-has-related-section-p)))
             ('adaptive (not (sb/--daily-has-related-section-p)))
             ('always t))))
      (when should-link
        ;; Use timer to run after save completes
        (run-with-timer 0.5 nil
                        (lambda (f)
                          (when (get-file-buffer f)
                            (with-current-buffer (get-file-buffer f)
                              (sb/daily-link f)
                              (save-buffer))))
                        (buffer-file-name))))))

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

;; Daily auto-linking hook
(add-hook 'after-save-hook #'sb/--maybe-auto-link-daily)

;;; ============================================================================
;;; KEY BINDINGS (C-c b prefix)
;;; ============================================================================

(defvar sb/daily-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'sb/daily-link)        ; Link current daily
    (define-key map (kbd "L") 'sb/daily-link-all)    ; Batch link all
    (define-key map (kbd "c") 'sb/daily-connections) ; Show connections
    map)
  "Keymap for daily commands under C-c b D.")

(defvar sb/command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'sb/new)            ; a note of any type the vault defines
    (define-key map (kbd "I") 'sb/inbox)
    (define-key map (kbd "l") 'sb/list)           ; notes of one type
    (define-key map (kbd "o") 'sb/open)           ; unfinished notes
    (define-key map (kbd "s") 'sb/stale)
    (define-key map (kbd "d") 'sb/digest)
    (define-key map (kbd "A") 'sb/proposals)      ; changes waiting for approval
    (define-key map (kbd "/") 'sb/search)
    (define-key map (kbd "L") 'sb/suggest-links)
    (define-key map (kbd "D") sb/daily-map)
    map)
  "Keymap for Second Brain commands.")

(defvar orsb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") sb/command-map)
    (define-key map (kbd "C-c v s") 'org-roam-semantic-search)
    (define-key map (kbd "C-c v i") 'org-roam-semantic-insert-similar)
    (define-key map (kbd "C-c v r") 'org-roam-semantic-insert-related)
    (define-key map (kbd "C-c v c") 'org-roam-semantic-search-chunks)
    (define-key map (kbd "C-c v g") 'org-roam-semantic-generate-chunks-for-file)
    (define-key map (kbd "C-c v G") 'org-roam-semantic-generate-all-chunks)
    map)
  "Keymap of `orsb-mode': C-c b (second brain) and C-c v (semantic search).")

;;;###autoload
(define-minor-mode orsb-mode
  "Global keybindings for org-roam-second-brain (C-c b and C-c v).
Nothing is bound until this mode is enabled, so the package no longer
claims global keys at load time."
  :global t
  :keymap orsb-mode-map
  :group 'orsb)

(provide 'org-roam-second-brain)
;;; org-roam-second-brain.el ends here
