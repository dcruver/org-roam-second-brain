;;; org-roam-api.el --- Complete API for org-roam knowledge management -*- lexical-binding: t; -*-

;; Author: David Cruver <dcruver@users.noreply.github.com>
;; URL: https://github.com/dcruver/org-roam-second-brain
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.2"))

;;; Commentary:
;; Complete API for org-roam integration including:
;; - Basic note creation and search
;; - Enhanced contextual search for RAG
;; - Simplified draft management (one draft per room)
;;
;; This package provides the API functions used by org-roam-mcp.

;;; Code:

(require 'org-roam)
(require 'org-roam-vector-search)
(require 'json)
(require 'subr-x)

;;; ============================================================================
;;; HELPER FUNCTIONS
;;; ============================================================================

(defun my/api--ensure-org-roam-db ()
  "Ensure org-roam database is available."
  t) ; No automatic sync - handle selectively per function

(defun my/api--json-response (data)
  "Convert DATA to JSON string for API response."
  (json-encode data))

(defun my/api--generate-id ()
  "Generate a unique ID for new notes."
  (format "%s" (time-convert nil 'integer)))

(defun my/api--create-org-file (title id)
  "Create org file path for TITLE with ID."
  (let ((filename (format "%s-%s.org"
                         (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title))
                         id)))
    (expand-file-name filename org-roam-directory)))

(defun my/api--node-to-json (node)
  "Convert org-roam NODE to JSON-serializable format."
  (when node
    (let* ((file (org-roam-node-file node))
           (properties (my/api--extract-properties file)))
      `((id . ,(org-roam-node-id node))
        (title . ,(org-roam-node-title node))
        (file . ,file)
        (status . ,(cdr (assoc "status" properties)))
        (priority . ,(cdr (assoc "priority" properties)))
        (created . ,(format-time-string "%Y-%m-%d %H:%M:%S"
                                       (org-roam-node-file-mtime node)))))))

(defun my/api--extract-properties (file)
  "Extract org properties from FILE."
  (when file
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

(defun my/api--strip-embedding-properties (content)
  "Remove :EMBEDDING: property lines from CONTENT.
These are large float arrays that cause JSON parsing issues when
output through emacsclient."
  (when content
    (replace-regexp-in-string
     "^:EMBEDDING[^:]*:.*$" "" content)))

(defun my/api--save-buffer-no-hooks ()
  "Save buffer without triggering before-save and after-save hooks.
Avoids slow hooks like embedding generation or toc-org on large files."
  (let ((inhibit-read-only t))
    (write-region (point-min) (point-max) (buffer-file-name) nil :silent)
    (set-buffer-modified-p nil)))




(defun my/api--generate-and-save-embedding (file-path)
  "Generate embedding for FILE-PATH and save the buffer.
This is a helper function to ensure embeddings are persisted to disk."
  (condition-case err
      (when (fboundp 'org-roam-semantic-generate-embedding)
        (message "Generating embedding for: %s" (file-name-nondirectory file-path))
        (org-roam-semantic-generate-embedding file-path)
        ;; Save the buffer after embedding is stored
        (let ((buf (find-buffer-visiting file-path)))
          (when buf
            (with-current-buffer buf
              (save-buffer)
              (kill-buffer))))
        t)
    (error
     (message "Warning: Failed to generate embeddings for %s: %s"
              (file-name-nondirectory file-path)
              (error-message-string err))
     nil)))

;;; ============================================================================
;;; BASIC NOTE CREATION AND SEARCH
;;; ============================================================================

(defun my/api-create-note (title content &optional type confidence)
  "Create new org-roam note with properties and generate embeddings if org-roam-semantic is available."
  (interactive)
  (my/api--ensure-org-roam-db)

  (let* ((id (my/api--generate-id))
         (file-path (my/api--create-org-file title id)))

    ;; Create file using buffer approach to trigger hooks
    (with-current-buffer (find-file-noselect file-path)
      (org-mode)
      (erase-buffer)
      (insert (format ":PROPERTIES:\n")
              (format ":ID: %s\n" id)
              (format ":END:\n")
              (format "#+title: %s\n\n" title)
              (format "%s\n" (or content "")))
      (save-buffer)  ; This will trigger the before-save-hook
      (kill-buffer (current-buffer)))

    (org-roam-db-sync)

    ;; Generate embeddings if org-roam-semantic is available
    (my/api--generate-and-save-embedding file-path)

    (let ((node (org-roam-node-from-id id)))
      (json-encode
        `((success . t)
          (message . "Note created successfully")
          (embedding_generated . ,(and (fboundp 'org-roam-semantic-generate-embedding) t))
          (note . ,(my/api--node-to-json node)))))))

(defun my/api-search-notes (query)
  "Search org-roam notes by QUERY."
  (interactive)
  (my/api--ensure-org-roam-db)

  (let* ((query-words (split-string (downcase query) "\\s-+" t))
         (all-nodes (org-roam-node-list))
         (matching-nodes
          (seq-filter
           (lambda (node)
             (let ((title (downcase (org-roam-node-title node))))
               (seq-some
                (lambda (word)
                  (string-match-p (regexp-quote word) title))
                query-words)))
           all-nodes))
         (node-data (mapcar #'my/api--node-to-json matching-nodes)))

    (my/api--json-response
     `((success . t)
       (query . ,query)
       (total_found . ,(length matching-nodes))
       (notes . ,node-data)))))

;;; ============================================================================
;;; SEMANTIC VECTOR SEARCH (for RAG)
;;; ============================================================================

(defun my/api-semantic-search (query &optional limit cutoff)
  "Semantic vector search using org-roam-semantic.
Returns semantically similar notes with full content for RAG applications."
  (interactive)
  (my/api--ensure-org-roam-db)

  (condition-case err
      (if (fboundp 'org-roam-semantic-get-similar-data)
          (let* ((limit (or limit 10))
                 (cutoff (or cutoff 0.55))
                 (similarities (org-roam-semantic-get-similar-data query limit cutoff))
                 (enriched-results
                  (mapcar (lambda (result)
                            (let* ((file (car result))
                                   (similarity (cadr result))
                                   (node (let ((id (caar (org-roam-db-query [:select [id] :from nodes :where (= file $s1)] file))))
                                          (when id (org-roam-node-from-id id))))
                                   (full-content (my/api--strip-embedding-properties
                                                  (when file
                                                    (condition-case nil
                                                        (with-temp-buffer
                                                          (insert-file-contents file)
                                                          (buffer-string))
                                                      (error "")))))
                                   (properties (my/api--extract-properties file))
                                   (backlinks (when node (org-roam-backlinks-get node)))
                                   (forward-links (when node
                                                   (org-roam-db-query
                                                    [:select [dest] :from links :where (= source $s1)]
                                                    (org-roam-node-id node)))))

                              `((id . ,(when node (org-roam-node-id node)))
                                (title . ,(when node (org-roam-node-title node)))
                                (file . ,file)
                                (similarity_score . ,similarity)
                                (full_content . ,full-content)
                                (properties . ,properties)
                                (backlinks . ,(mapcar (lambda (backlink)
                                                       `((id . ,(org-roam-node-id (org-roam-backlink-source-node backlink)))
                                                         (title . ,(org-roam-node-title (org-roam-backlink-source-node backlink)))))
                                                     backlinks))
                                (forward_links . ,(mapcar (lambda (link-dest)
                                                           (when-let ((dest-node (org-roam-node-from-id link-dest)))
                                                             `((id . ,link-dest)
                                                               (title . ,(org-roam-node-title dest-node)))))
                                                         forward-links))
                                (created . ,(when node (format-time-string "%Y-%m-%d %H:%M:%S"
                                                                           (org-roam-node-file-mtime node)))))))
                          similarities)))

            (my/api--json-response
             `((success . t)
               (query . ,query)
               (search_type . "semantic_vector")
               (total_found . ,(length similarities))
               (similarity_cutoff . ,cutoff)
               (notes . ,enriched-results)
               (knowledge_context . ,(my/api--generate-semantic-context enriched-results query)))))

        ;; Fallback to keyword search if semantic search not available
        (my/api-contextual-search query limit))

    (error
     (my/api--json-response
      `((success . nil)
        (error . ,(format "Semantic search failed: %s. Ensure org-roam-semantic is loaded and embeddings are generated." (error-message-string err)))
        (fallback_available . t))))))

(defun my/api--generate-semantic-context (enriched-results query)
  "Generate contextual summary for semantic search results."
  (let ((total-notes (length enriched-results))
        (avg-similarity (if enriched-results
                           (/ (apply #'+ (mapcar (lambda (note)
                                                  (cdr (assoc 'similarity_score note)))
                                                enriched-results))
                              (float (length enriched-results)))
                         0))
        (connection-count (apply #'+ (mapcar (lambda (note)
                                             (+ (length (cdr (assoc 'backlinks note)))
                                                (length (cdr (assoc 'forward_links note)))))
                                           enriched-results))))
    `((query_intent . ,query)
      (search_method . "vector_embeddings")
      (total_semantic_matches . ,total-notes)
      (average_similarity . ,avg-similarity)
      (total_connections . ,connection-count)
      (embedding_model . "nomic-embed-text")
      (knowledge_density . ,(if (> total-notes 1)
                               (/ connection-count (* total-notes (1- total-notes)))
                             0)))))

;;; ============================================================================
;;; ENHANCED CONTEXTUAL SEARCH (for RAG)
;;; ============================================================================

(defun my/api-contextual-search (query &optional limit)
  "Enhanced search that returns rich context for LLM consumption."
  (interactive)
  (my/api--ensure-org-roam-db)

  (let* ((limit (or limit 10))
         (query-words (split-string (downcase query) "\\s-+" t))
         (all-nodes (org-roam-node-list))
         (matching-nodes
          (seq-filter
           (lambda (node)
             (let ((title (downcase (org-roam-node-title node)))
                   (content (when-let ((file (org-roam-node-file node)))
                             (condition-case nil
                                 (with-temp-buffer
                                   (insert-file-contents file)
                                   (downcase (buffer-string)))
                               (error "")))))
               (seq-some
                (lambda (word)
                  (or (string-match-p (regexp-quote word) title)
                      (and content (string-match-p (regexp-quote word) content))))
                query-words)))
           all-nodes))
         (limited-results (seq-take matching-nodes limit))
         (enriched-results
          (mapcar (lambda (node)
                    (my/api--enrich-node-context node query-words))
                  limited-results)))

    (my/api--json-response
     `((success . t)
       (query . ,query)
       (context_type . "enhanced_search")
       (total_found . ,(length matching-nodes))
       (returned . ,(length limited-results))
       (notes . ,enriched-results)
       (knowledge_context . ,(my/api--generate-knowledge-context enriched-results query))))))

(defun my/api--enrich-node-context (node query-words)
  "Enrich NODE with full content, connections, and relevance scoring."
  (let* ((file (org-roam-node-file node))
         (full-content (my/api--strip-embedding-properties
                        (when file
                          (condition-case nil
                              (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))
                            (error "")))))
         (backlinks (org-roam-backlinks-get node))
         (forward-links (when file
                         (org-roam-db-query
                          [:select [dest] :from links :where (= source $s1)]
                          (org-roam-node-id node))))
         (properties (my/api--extract-properties file)))

    `((id . ,(org-roam-node-id node))
      (title . ,(org-roam-node-title node))
      (file . ,file)
      (full_content . ,full-content)
      (properties . ,properties)
      (backlinks . ,(mapcar (lambda (backlink)
                             `((id . ,(org-roam-node-id (org-roam-backlink-source-node backlink)))
                               (title . ,(org-roam-node-title (org-roam-backlink-source-node backlink)))))
                           backlinks))
      (forward_links . ,(mapcar (lambda (link-dest)
                                 (when-let ((dest-node (org-roam-node-from-id link-dest)))
                                   `((id . ,link-dest)
                                     (title . ,(org-roam-node-title dest-node)))))
                               forward-links))
      (relevance_score . ,(my/api--calculate-relevance node query-words))
      (created . ,(format-time-string "%Y-%m-%d %H:%M:%S"
                                     (org-roam-node-file-mtime node))))))

(defun my/api--generate-knowledge-context (enriched-results query)
  "Generate contextual summary for LLM about the knowledge graph state."
  (let ((total-notes (length enriched-results))
        (connection-count (apply #'+ (mapcar (lambda (note)
                                              (+ (length (cdr (assoc 'backlinks note)))
                                                 (length (cdr (assoc 'forward_links note)))))
                                            enriched-results))))
    `((query_intent . ,query)
      (total_relevant_notes . ,total-notes)
      (total_connections . ,connection-count)
      (avg_connections_per_note . ,(if (> total-notes 0)
                                      (/ connection-count total-notes)
                                    0))
      (knowledge_density . ,(if (> total-notes 1)
                               (/ connection-count (* total-notes (1- total-notes)))
                             0)))))

(defun my/api--calculate-relevance (node query-words)
  "Calculate relevance score for NODE given QUERY-WORDS."
  (let* ((title (downcase (org-roam-node-title node)))
         (content (when-let ((file (org-roam-node-file node)))
                   (condition-case nil
                       (with-temp-buffer
                         (insert-file-contents file)
                         (downcase (buffer-string)))
                     (error ""))))
         (title-matches (seq-count (lambda (word)
                                    (string-match-p (regexp-quote word) title))
                                  query-words))
         (content-matches (seq-count (lambda (word)
                                      (and content (string-match-p (regexp-quote word) content)))
                                    query-words))
         (total-words (length query-words)))

    (if (> total-words 0)
        (+ (* 0.7 (/ title-matches (float total-words)))
           (* 0.3 (/ content-matches (float total-words))))
      0)))

;;; ============================================================================
;;; SIMPLIFIED DRAFT MANAGEMENT (One Draft Per Room)
;;; ============================================================================

(defun my/api--draft-file-path (room-id)
  "Get draft file path for ROOM-ID."
  (let ((clean-room-id (replace-regexp-in-string "[^a-zA-Z0-9]" "" room-id)))
    (expand-file-name (format "draft-%s.org" clean-room-id) org-roam-directory)))

(defun my/api-get-draft (room-id)
  "Get current draft for ROOM-ID, or nil if none exists."
  (my/api--ensure-org-roam-db)

  (let ((draft-file (my/api--draft-file-path room-id)))
    (if (file-exists-p draft-file)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents draft-file)
              (let (original-request current-draft created)
                (goto-char (point-min))
                (when (re-search-forward ":ORIGINAL-REQUEST: \\(.+\\)" nil t)
                  (setq original-request (match-string 1)))
                (goto-char (point-min))
                (when (re-search-forward ":CREATED: \\(.+\\)" nil t)
                  (setq created (match-string 1)))

                ;; Content extraction using working method
                (goto-char (point-min))
                (when (search-forward "* Current Draft" nil t)
                  (when (search-forward "#+begin_example" nil t)
                    (forward-line 1)
                    (let ((start (point)))
                      (when (search-forward "#+end_example" nil t)
                        (beginning-of-line)
                        (setq current-draft (buffer-substring-no-properties start (point)))
                        (setq current-draft (string-trim current-draft))))))

                (my/api--json-response
                 `((success . t)
                   (has_draft . t)
                   (room_id . ,room-id)
                   (original_request . ,original-request)
                   (current_draft . ,(or current-draft ""))
                   (created . ,created)
                   (has_content . ,(not (string-empty-p (or current-draft ""))))))))
          (error
           (my/api--json-response
            `((success . nil)
              (error . "Could not read draft file")
              (room_id . ,room-id)))))
      (my/api--json-response
       `((success . t)
         (has_draft . nil)
         (room_id . ,room-id)
         (message . "No draft exists"))))))

(defun my/api-update-draft (room-id content &optional revision-note)
  "Create or update draft for ROOM-ID with CONTENT."
  (my/api--ensure-org-roam-db)

  (let ((draft-file (my/api--draft-file-path room-id))
        (revision-note (or revision-note "Draft updated"))
        (is-new-draft (not (file-exists-p (my/api--draft-file-path room-id)))))

    (if is-new-draft
        ;; Create new draft
        (with-temp-file draft-file
          (insert ":PROPERTIES:\n")
          (insert (format ":ROOM-ID: %s\n" room-id))
          (insert (format ":ORIGINAL-REQUEST: %s\n" revision-note))
          (insert (format ":CREATED: %s\n" (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
          (insert (format ":LAST-UPDATED: %s\n" (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
          (insert ":END:\n")
          (insert "#+title: Draft\n\n")
          (insert "* Current Draft\n#+begin_example\n")
          (insert content "\n")
          (insert "#+end_example\n\n")
          (insert "* Revision History\n")
          (insert (format "** %s - Draft Created\n" (format-time-string "%H:%M:%S")))
          (insert (format "%s\n" revision-note)))

      ;; Update existing draft
      (with-temp-buffer
        (insert-file-contents draft-file)
        (goto-char (point-min))

        ;; Update LAST-UPDATED property
        (when (re-search-forward ":LAST-UPDATED: .*" nil t)
          (replace-match (format ":LAST-UPDATED: %s"
                                (format-time-string "%Y-%m-%dT%H:%M:%SZ"))))

        ;; Update current draft content
        (goto-char (point-min))
        (when (search-forward "* Current Draft" nil t)
          (when (search-forward "#+begin_example" nil t)
            (forward-line 1)
            (let ((start (point)))
              (when (search-forward "#+end_example" nil t)
                (beginning-of-line)
                (delete-region start (point))
                (insert content "\n")))))

        ;; Add revision history entry
        (goto-char (point-max))
        (insert (format "\n** %s - Revision\n" (format-time-string "%H:%M:%S")))
        (insert (format "%s\n" revision-note))

        (write-file draft-file)))

    (my/api--json-response
     `((success . t)
       (action . ,(if is-new-draft "draft_created" "draft_updated"))
       (room_id . ,room-id)
       (is_new . ,is-new-draft)
       (message . ,(if is-new-draft "Draft created successfully" "Draft updated successfully"))))))

(defun my/api-finalize-draft (room-id final-title)
  "Convert draft to permanent note and remove draft file."
  (my/api--ensure-org-roam-db)

  (let ((draft-file (my/api--draft-file-path room-id)))
    (if (file-exists-p draft-file)
        (condition-case nil
            (let (current-draft)
              ;; Extract draft content using working method
              (with-temp-buffer
                (insert-file-contents draft-file)
                (goto-char (point-min))
                (when (search-forward "* Current Draft" nil t)
                  (when (search-forward "#+begin_example" nil t)
                    (forward-line 1)
                    (let ((start (point)))
                      (when (search-forward "#+end_example" nil t)
                        (beginning-of-line)
                        (setq current-draft (buffer-substring-no-properties start (point)))
                        (setq current-draft (string-trim current-draft)))))))

              (if (string-empty-p (or current-draft ""))
                  (my/api--json-response
                   `((success . nil)
                     (error . "No draft content to save")
                     (room_id . ,room-id)))

                ;; Create final note
                (let* ((final-id (my/api--generate-id))
                       (file-path (my/api--create-org-file final-title final-id)))

                  (with-temp-file file-path
                    (insert current-draft))

                  ;; Remove draft file
                  (delete-file draft-file)

                  ;; Sync database so final note is immediately available
                  (org-roam-db-sync)

                  ;; Generate embeddings if org-roam-semantic is available
                  (my/api--generate-and-save-embedding file-path)

                  (my/api--json-response
                   `((success . t)
                     (action . "draft_finalized")
                     (room_id . ,room-id)
                     (final_note_id . ,final-id)
                     (final_note_title . ,final-title)
                     (embedding_generated . ,(and (fboundp 'org-roam-semantic-generate-embedding) t))
                     (message . ,(format "Draft saved as \"%s\"" final-title)))))))
          (error
           (my/api--json-response
            `((success . nil)
              (error . "Failed to finalize draft")
              (room_id . ,room-id)))))
      (my/api--json-response
       `((success . nil)
         (error . "No draft exists to finalize")
         (room_id . ,room-id))))))

(defun my/api-cancel-draft (room-id)
  "Delete draft file for ROOM-ID."
  (my/api--ensure-org-roam-db)

  (let ((draft-file (my/api--draft-file-path room-id)))
    (if (file-exists-p draft-file)
        (progn
          (delete-file draft-file)
          (my/api--json-response
           `((success . t)
             (action . "draft_cancelled")
             (room_id . ,room-id)
             (message . "Draft cancelled"))))
      (my/api--json-response
       `((success . t)
         (action . "no_draft_to_cancel")
         (room_id . ,room-id)
         (message . "No draft to cancel"))))))

;; Configure org-roam dailies
(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M> %?"
         :target (file+head "%<%Y-%m-%d>.org"
                           "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))))

(defun my/add-daily-entry-structured (timestamp title points next-steps tags &optional type priority)
  "Add structured entry to today's daily note. Handles both journal and TODO entries.
TYPE: 'journal' or 'todo'"
  (let* ((today (format-time-string "%Y-%m-%d"))
         (daily-file (expand-file-name (concat today ".org")
                                      (expand-file-name "daily" org-roam-directory)))
         (entry-type (or type "journal"))
         (is-todo (string= entry-type "todo")))

    (unless (file-exists-p (file-name-directory daily-file))
      (make-directory (file-name-directory daily-file) t))

    (unless (file-exists-p daily-file)
      (with-temp-file daily-file
        (insert (format "#+title: %s\n#+filetags: :daily:\n\n" today))))

    (with-current-buffer (find-file-noselect daily-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))

      ;; Insert entry header with appropriate formatting
      (if is-todo
          (if (and tags (> (length tags) 0))
              (insert (format "* TODO %s %s    %s\n"
                             timestamp title
                             (concat ":" (string-join tags ":") ":")))
            (insert (format "* TODO %s %s\n" timestamp title)))
        ;; Regular journal entry
        (if (and tags (> (length tags) 0))
            (insert (format "* %s %s    %s\n"
                           timestamp title
                           (concat ":" (string-join tags ":") ":")))
          (insert (format "* %s %s\n" timestamp title))))

      ;; Insert main points
      (when points
        (dolist (point points)
          (insert (format "- %s\n" point))))

      ;; Insert next steps
      (when next-steps
        (if is-todo
            (progn
              (insert "\n** Subtasks\n")
              (dolist (step next-steps)
                (insert (format "- [ ] %s\n" step))))
          (progn
            (insert "\n** Next Steps\n")
            (dolist (step next-steps)
              (insert (format "- [ ] %s\n" step))))))

      (insert "\n")
      (save-buffer))

    (message "Added %s entry to daily note: %s" entry-type daily-file)))

(defun my/get-daily-note-content (&optional date)
  "Get content of daily note for DATE (defaults to today)."
  (let* ((target-date (or date (format-time-string "%Y-%m-%d")))
         (daily-file (expand-file-name (concat target-date ".org")
                                      (expand-file-name org-roam-dailies-directory org-roam-directory))))
    (if (file-exists-p daily-file)
        (with-temp-buffer
          (insert-file-contents daily-file)
          (buffer-string))
      "")))

;;; ============================================================================
;;; INBOX LOGGING (Audit Trail)
;;; ============================================================================

(defun my/api-add-inbox-entry (command original-text &optional linked-note-id linked-note-title)
  "Add an inbox entry to today's daily note for audit trail.
COMMAND is the command used (e.g., 'journal', 'note', 'yt').
ORIGINAL-TEXT is the full original message including the command.
LINKED-NOTE-ID and LINKED-NOTE-TITLE are optional if a note was created."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (daily-file (expand-file-name (concat today ".org")
                                      (expand-file-name "daily" org-roam-directory)))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (link-text (if (and linked-note-id linked-note-title)
                       (format "[[id:%s][%s]]" linked-note-id linked-note-title)
                     (cond
                      ((string= command "journal") "daily entry added")
                      ((string= command "search") "search performed")
                      ((string= command "help") "help shown")
                      (t "processed")))))

    ;; Ensure daily directory exists
    (unless (file-exists-p (file-name-directory daily-file))
      (make-directory (file-name-directory daily-file) t))

    ;; Create daily file if it doesn't exist
    (unless (file-exists-p daily-file)
      (with-temp-file daily-file
        (insert (format "#+title: %s\n#+filetags: :daily:\n\n" today))))

    (with-current-buffer (find-file-noselect daily-file)
      ;; Find or create Inbox section
      (goto-char (point-min))
      (let ((inbox-pos (re-search-forward "^\\* Inbox$" nil t)))
        (if inbox-pos
            ;; Found Inbox section - go to after the heading
            (progn
              (forward-line 1)
              ;; Skip any existing entries to add at the end of Inbox section
              (while (and (not (eobp))
                         (or (looking-at "^\\*\\* ")
                             (looking-at "^   ")
                             (looking-at "^$")))
                (forward-line 1))
              ;; Back up to before the next section or end
              (when (looking-at "^\\* ")
                (forward-line -1)
                (end-of-line)
                (insert "\n")))
          ;; No Inbox section - create one at the end
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "* Inbox\n")))

      ;; Insert the inbox entry
      (insert (format "** DONE /%s → %s\n" command link-text))
      (insert "   :PROPERTIES:\n")
      (insert (format "   :CAPTURED: %s\n" timestamp))
      (insert (format "   :COMMAND: %s\n" command))
      (insert (format "   :ORIGINAL: %s\n" original-text))
      (when linked-note-id
        (insert (format "   :LINKED-ID: %s\n" linked-note-id)))
      (insert "   :END:\n")

      (save-buffer))

    (my/api--json-response
     `((success . t)
       (message . "Inbox entry added")
       (command . ,command)
       (timestamp . ,timestamp)
       (linked_note_id . ,linked-note-id)
       (linked_note_title . ,linked-note-title)))))

;;; ============================================================================
;;; STRUCTURED NODE TYPES (Phase 2)
;;; ============================================================================

(defun my/api--create-subdirectory (subdir)
  "Ensure SUBDIR exists under org-roam-directory."
  (let ((dir (expand-file-name subdir org-roam-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun my/api--create-structured-file (title id subdir)
  "Create org file path for TITLE with ID in SUBDIR."
  (let* ((dir (my/api--create-subdirectory subdir))
         (filename (format "%s-%s.org"
                          (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title))
                          id)))
    (expand-file-name filename dir)))

(defun my/api-create-person (name &optional context follow-ups notes)
  "Create a person node with structured fields.
NAME is the person's name.
CONTEXT is optional context about how you know them.
FOLLOW-UPS is an optional list of action items.
NOTES is optional additional notes."
  (my/api--ensure-org-roam-db)
  (let* ((id (my/api--generate-id))
         (file-path (my/api--create-structured-file name id "people"))
         (timestamp (format-time-string "[%Y-%m-%d %a]")))

    (with-current-buffer (find-file-noselect file-path)
      (org-mode)
      (erase-buffer)
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" id))
      (insert ":NODE-TYPE: person\n")
      (when context
        (insert (format ":CONTEXT: %s\n" context)))
      (insert (format ":LAST-CONTACT: %s\n" timestamp))
      (insert ":END:\n")
      (insert (format "#+title: %s\n\n" name))

      ;; Follow-ups section
      (insert "* Follow-ups\n")
      (if follow-ups
          (dolist (item follow-ups)
            (insert (format "- [ ] %s\n" item)))
        (insert "\n"))

      ;; Notes section
      (insert "\n* Notes\n")
      (when notes
        (insert (format "%s\n" notes)))

      (save-buffer)
      (kill-buffer (current-buffer)))

    (org-roam-db-sync)

    ;; Generate embeddings if available
    (my/api--generate-and-save-embedding file-path)

    (let ((node (org-roam-node-from-id id)))
      (json-encode
       `((success . t)
         (message . "Person note created successfully")
         (note . ((id . ,id)
                  (title . ,name)
                  (file . ,file-path)
                  (node_type . "person"))))))))

(defun my/api-create-project (title &optional status next-action notes)
  "Create a project node with structured fields.
TITLE is the project name.
STATUS is optional status (active, waiting, blocked, someday, done).
NEXT-ACTION is optional next actionable step.
NOTES is optional additional notes."
  (my/api--ensure-org-roam-db)
  (let* ((id (my/api--generate-id))
         (file-path (my/api--create-structured-file title id "projects"))
         (status (or status "active")))

    (with-current-buffer (find-file-noselect file-path)
      (org-mode)
      (erase-buffer)
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" id))
      (insert ":NODE-TYPE: project\n")
      (insert (format ":STATUS: %s\n" status))
      (when next-action
        (insert (format ":NEXT-ACTION: %s\n" next-action)))
      (insert ":END:\n")
      (insert (format "#+title: %s\n\n" title))

      ;; Next Actions section
      (insert "* Next Actions\n")
      (if next-action
          (insert (format "- [ ] %s\n" next-action))
        (insert "\n"))

      ;; Notes section
      (insert "\n* Notes\n")
      (when notes
        (insert (format "%s\n" notes)))

      (save-buffer)
      (kill-buffer (current-buffer)))

    (org-roam-db-sync)

    ;; Generate embeddings if available
    (my/api--generate-and-save-embedding file-path)

    (let ((node (org-roam-node-from-id id)))
      (json-encode
       `((success . t)
         (message . "Project note created successfully")
         (note . ((id . ,id)
                  (title . ,title)
                  (file . ,file-path)
                  (node_type . "project")
                  (status . ,status))))))))

(defun my/api-create-idea (title one-liner &optional elaboration)
  "Create an idea node with structured fields.
TITLE is the idea title.
ONE-LINER is a brief summary of the insight.
ELABORATION is optional detailed explanation."
  (my/api--ensure-org-roam-db)
  (let* ((id (my/api--generate-id))
         (file-path (my/api--create-structured-file title id "ideas")))

    (with-current-buffer (find-file-noselect file-path)
      (org-mode)
      (erase-buffer)
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" id))
      (insert ":NODE-TYPE: idea\n")
      (insert (format ":ONE-LINER: %s\n" one-liner))
      (insert ":END:\n")
      (insert (format "#+title: %s\n\n" title))

      ;; One-liner section
      (insert "* One-liner\n")
      (insert (format "%s\n" one-liner))

      ;; Elaboration section
      (insert "\n* Elaboration\n")
      (when elaboration
        (insert (format "%s\n" elaboration)))

      (save-buffer)
      (kill-buffer (current-buffer)))

    (org-roam-db-sync)

    ;; Generate embeddings if available
    (my/api--generate-and-save-embedding file-path)

    (let ((node (org-roam-node-from-id id)))
      (json-encode
       `((success . t)
         (message . "Idea note created successfully")
         (note . ((id . ,id)
                  (title . ,title)
                  (file . ,file-path)
                  (node_type . "idea"))))))))

(defun my/api-create-admin (title &optional due-date notes)
  "Create an admin task node with structured fields.
TITLE is the task title.
DUE-DATE is optional due date in YYYY-MM-DD format.
NOTES is optional additional notes."
  (my/api--ensure-org-roam-db)
  (let* ((id (my/api--generate-id))
         (file-path (my/api--create-structured-file title id "admin"))
         (due-timestamp (when due-date
                         (format "[%s]" due-date))))

    (with-current-buffer (find-file-noselect file-path)
      (org-mode)
      (erase-buffer)
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" id))
      (insert ":NODE-TYPE: admin\n")
      (when due-timestamp
        (insert (format ":DUE-DATE: %s\n" due-timestamp)))
      (insert ":STATUS: todo\n")
      (insert ":END:\n")
      (insert (format "#+title: %s\n\n" title))

      ;; Notes section
      (insert "* Notes\n")
      (when notes
        (insert (format "%s\n" notes)))

      (save-buffer)
      (kill-buffer (current-buffer)))

    (org-roam-db-sync)

    ;; Generate embeddings if available
    (my/api--generate-and-save-embedding file-path)

    (let ((node (org-roam-node-from-id id)))
      (json-encode
       `((success . t)
         (message . "Admin task created successfully")
         (note . ((id . ,id)
                  (title . ,title)
                  (file . ,file-path)
                  (node_type . "admin")
                  (due_date . ,due-date))))))))

;;; ============================================================================
;;; PROACTIVE SURFACING (Phase 3)
;;; ============================================================================

(defun my/api--get-nodes-by-type (node-type &optional file-level-only)
  "Get all org-roam nodes with NODE-TYPE property matching NODE-TYPE.
If FILE-LEVEL-ONLY is non-nil, only return one node per file (deduplicates aliases)."
  (let ((results '())
        (seen-files (make-hash-table :test 'equal)))
    (dolist (node (org-roam-node-list))
      ;; Filter to file-level nodes only (level 0) if requested
      (when (or (not file-level-only)
                (= (org-roam-node-level node) 0))
        (let* ((file (org-roam-node-file node))
               ;; Skip if we've already processed this file (handles ROAM_ALIASES)
               (already-seen (and file-level-only (gethash file seen-files))))
          (unless already-seen
            (let* ((props (my/api--extract-properties file))
                   (type (cdr (assoc "node-type" props))))
              (when (and type (string= (downcase type) (downcase node-type)))
                (when file-level-only (puthash file t seen-files))
                (push (cons node props) results)))))))
    results))

(defun my/api--extract-unchecked-items (file section-name)
  "Extract unchecked checkbox items from SECTION-NAME in FILE."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((items '()))
        (when (re-search-forward (format "^\\* %s$" section-name) nil t)
          (forward-line 1)
          (while (and (not (eobp))
                      (not (looking-at "^\\* ")))
            (when (looking-at "^- \\[ \\] \\(.+\\)$")
              (push (match-string 1) items))
            (forward-line 1)))
        (nreverse items)))))

(defun my/api--file-modified-days-ago (file)
  "Return number of days since FILE was last modified."
  (when (and file (file-exists-p file))
    (let* ((mtime (file-attribute-modification-time (file-attributes file)))
           (now (current-time))
           (diff (time-subtract now mtime)))
      (/ (float-time diff) 86400.0))))

(defun my/api-get-active-projects ()
  "Get all projects with status=active, ordered by last modified.
Returns projects with their next actions and modification dates."
  (my/api--ensure-org-roam-db)
  (let* ((project-nodes (my/api--get-nodes-by-type "project" t))
         (active-projects
          (seq-filter
           (lambda (node-props)
             (let ((status (cdr (assoc "status" (cdr node-props)))))
               (and status (string= (downcase status) "active"))))
           project-nodes))
         (enriched-projects
          (mapcar
           (lambda (node-props)
             (let* ((node (car node-props))
                    (props (cdr node-props))
                    (file (org-roam-node-file node))
                    (next-action (cdr (assoc "next-action" props)))
                    (next-actions (my/api--extract-unchecked-items file "Next Actions"))
                    (days-ago (my/api--file-modified-days-ago file)))
               `((id . ,(org-roam-node-id node))
                 (title . ,(org-roam-node-title node))
                 (file . ,file)
                 (status . "active")
                 (next_action . ,next-action)
                 (unchecked_actions . ,next-actions)
                 (days_since_modified . ,(round days-ago)))))
           active-projects))
         ;; Sort by days_since_modified ascending (most recently touched first)
         (sorted-projects
          (sort enriched-projects
                (lambda (a b)
                  (< (cdr (assoc 'days_since_modified a))
                     (cdr (assoc 'days_since_modified b)))))))
    (my/api--json-response
     `((success . t)
       (total . ,(length sorted-projects))
       (projects . ,sorted-projects)))))

(defun my/api--get-backlink-files (node)
  "Get list of files that link to NODE."
  (let* ((id (org-roam-node-id node))
         (backlinks (org-roam-db-query
                     [:select :distinct [source]
                      :from links
                      :where (= dest $s1)]
                     id)))
    (mapcar #'car backlinks)))

(defun my/api--find-unchecked-items-mentioning (file person-name)
  "Find unchecked items in FILE that mention PERSON-NAME.
Returns list of unchecked item texts."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((items '())
            (pattern (concat "^[ \t]*- \\[ \\].*\\[\\[" (regexp-quote person-name) "\\]\\]")))
        ;; Find unchecked items mentioning this person
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*- \\[ \\] \\(.*\\)$" nil t)
          (let ((item-text (match-string 1)))
            (when (string-match-p (regexp-quote person-name) item-text)
              (push (string-trim item-text) items))))
        (nreverse items)))))

(defun my/api-get-pending-followups ()
  "Get all people with unchecked follow-up items in backlinked notes.
Scans notes that link to each person for unchecked [ ] items mentioning them."
  (my/api--ensure-org-roam-db)
  (let* ((person-nodes (my/api--get-nodes-by-type "person" t))
         (people-with-followups
          (seq-filter
           (lambda (node-props)
             (let* ((node (car node-props))
                    (name (org-roam-node-title node))
                    (backlink-files (my/api--get-backlink-files node))
                    (all-followups '()))
               ;; Collect unchecked items from all backlinked files
               (dolist (file backlink-files)
                 (let ((items (my/api--find-unchecked-items-mentioning file name)))
                   (setq all-followups (append all-followups items))))
               (and all-followups (> (length all-followups) 0))))
           person-nodes))
         (enriched-people
          (mapcar
           (lambda (node-props)
             (let* ((node (car node-props))
                    (name (org-roam-node-title node))
                    (backlink-files (my/api--get-backlink-files node))
                    (all-followups '()))
               ;; Collect unchecked items from all backlinked files
               (dolist (file backlink-files)
                 (let ((items (my/api--find-unchecked-items-mentioning file name)))
                   (setq all-followups (append all-followups items))))
               `((id . ,(org-roam-node-id node))
                 (name . ,name)
                 (file . ,(org-roam-node-file node))
                 (backlink_count . ,(length backlink-files))
                 (pending_followups . ,all-followups)
                 (followup_count . ,(length all-followups)))))
           people-with-followups))
         ;; Sort by followup_count descending (most follow-ups first)
         (sorted-people
          (sort enriched-people
                (lambda (a b)
                  (> (cdr (assoc 'followup_count a))
                     (cdr (assoc 'followup_count b)))))))
    (my/api--json-response
     `((success . t)
       (total . ,(length sorted-people))
       (people . ,sorted-people)))))

(defun my/api-get-stale-projects (&optional days-threshold)
  "Get projects with no activity in DAYS-THRESHOLD days (default 5).
Returns projects that might be stuck or forgotten."
  (my/api--ensure-org-roam-db)
  (let* ((threshold (or days-threshold 5))
         (project-nodes (my/api--get-nodes-by-type "project" t))
         (active-projects
          (seq-filter
           (lambda (node-props)
             (let ((status (cdr (assoc "status" (cdr node-props)))))
               (and status
                    (not (string= (downcase status) "done"))
                    (not (string= (downcase status) "someday")))))
           project-nodes))
         (stale-projects
          (seq-filter
           (lambda (node-props)
             (let* ((node (car node-props))
                    (file (org-roam-node-file node))
                    (days-ago (my/api--file-modified-days-ago file)))
               (and days-ago (>= days-ago threshold))))
           active-projects))
         (enriched-projects
          (mapcar
           (lambda (node-props)
             (let* ((node (car node-props))
                    (props (cdr node-props))
                    (file (org-roam-node-file node))
                    (status (cdr (assoc "status" props)))
                    (next-action (cdr (assoc "next-action" props)))
                    (days-ago (my/api--file-modified-days-ago file)))
               `((id . ,(org-roam-node-id node))
                 (title . ,(org-roam-node-title node))
                 (file . ,file)
                 (status . ,status)
                 (next_action . ,next-action)
                 (days_since_modified . ,(round days-ago)))))
           stale-projects))
         ;; Sort by days_since_modified descending (most stale first)
         (sorted-projects
          (sort enriched-projects
                (lambda (a b)
                  (> (cdr (assoc 'days_since_modified a))
                     (cdr (assoc 'days_since_modified b)))))))
    (my/api--json-response
     `((success . t)
       (threshold_days . ,threshold)
       (total . ,(length sorted-projects))
       (projects . ,sorted-projects)))))

(defun my/api-get-weekly-inbox (&optional days)
  "Get inbox entries from the past DAYS days (default 7).
Returns entries grouped by day for weekly review."
  (my/api--ensure-org-roam-db)
  (let* ((num-days (or days 7))
         (daily-dir (expand-file-name "daily" org-roam-directory))
         (entries-by-day '())
         (total-entries 0))
    ;; Iterate through past N days
    (dotimes (i num-days)
      (let* ((date (format-time-string "%Y-%m-%d"
                                       (time-subtract (current-time)
                                                     (days-to-time i))))
             (daily-file (expand-file-name (concat date ".org") daily-dir))
             (day-entries '()))
        (when (file-exists-p daily-file)
          (with-temp-buffer
            (insert-file-contents daily-file)
            (goto-char (point-min))
            ;; Find Inbox section
            (when (re-search-forward "^\\* Inbox$" nil t)
              (forward-line 1)
              ;; Parse inbox entries
              (while (and (not (eobp))
                          (not (looking-at "^\\* [^I]")))
                (when (looking-at "^\\*\\* \\(DONE\\|TODO\\) /\\([^ ]+\\) → \\(.+\\)$")
                  (let ((status (match-string 1))
                        (command (match-string 2))
                        (result (match-string 3))
                        (captured nil)
                        (original nil))
                    ;; Extract properties
                    (save-excursion
                      (when (re-search-forward ":CAPTURED: \\(.+\\)" nil t)
                        (setq captured (match-string 1)))
                      (when (re-search-forward ":ORIGINAL: \\(.+\\)" nil t)
                        (setq original (match-string 1))))
                    (push `((status . ,status)
                            (command . ,command)
                            (result . ,result)
                            (captured . ,captured)
                            (original . ,original))
                          day-entries)
                    (setq total-entries (1+ total-entries))))
                (forward-line 1)))))
        (when day-entries
          (push `((date . ,date)
                  (entries . ,(nreverse day-entries))
                  (count . ,(length day-entries)))
                entries-by-day))))
    (my/api--json-response
     `((success . t)
       (days_covered . ,num-days)
       (total_entries . ,total-entries)
       (by_day . ,(nreverse entries-by-day))))))

(defun my/api-get-digest-data ()
  "Get all data needed for daily digest in a single call.
Combines active projects, pending follow-ups, and stale projects."
  (my/api--ensure-org-roam-db)
  (let* ((active-result (json-read-from-string (my/api-get-active-projects)))
         (followups-result (json-read-from-string (my/api-get-pending-followups)))
         (stale-result (json-read-from-string (my/api-get-stale-projects 5))))
    (my/api--json-response
     `((success . t)
       (generated_at . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
       (active_projects . ((total . ,(cdr (assoc 'total active-result)))
                           (top_3 . ,(seq-take (cdr (assoc 'projects active-result)) 3))))
       (pending_followups . ((total . ,(cdr (assoc 'total followups-result)))
                             (people . ,(cdr (assoc 'people followups-result)))))
       (stale_projects . ((total . ,(cdr (assoc 'total stale-result)))
                          (projects . ,(cdr (assoc 'projects stale-result)))))))))

(defun my/api--extract-link-names (text)
  "Extract all [[Name]] links from TEXT.
Returns list of names found."
  (let ((names '())
        (start 0))
    (while (string-match "\\[\\[\\([^]]+\\)\\]\\]" text start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    (nreverse names)))

(defun my/api-get-dangling-followups ()
  "Find unchecked items with [[Name]] links where the person node doesn't exist.
Scans all org files for unchecked items mentioning untracked people."
  (my/api--ensure-org-roam-db)
  (let ((dangling-items '())
        (seen-names (make-hash-table :test 'equal)))
    ;; Scan all org files in org-roam-directory
    (dolist (file (directory-files-recursively org-roam-directory "\\.org$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*- \\[ \\] \\(.*\\[\\[.+\\]\\].*\\)$" nil t)
          (let* ((item-text (match-string 1))
                 (names (my/api--extract-link-names item-text)))
            ;; Check each linked name
            (dolist (name names)
              (unless (or (gethash name seen-names)
                          (my/api--node-exists-p name))
                (puthash name t seen-names)
                (push `((name . ,name)
                        (item . ,(string-trim item-text))
                        (file . ,file))
                      dangling-items)))))))
    (my/api--json-response
     `((success . t)
       (total . ,(length dangling-items))
       (untracked_people . ,(nreverse dangling-items))))))

(defun my/api--node-exists-p (title)
  "Check if an org-roam node with TITLE exists."
  (org-roam-node-from-title-or-alias title))

(defun my/api--create-minimal-person (name)
  "Create a minimal person node for NAME if it doesn't exist.
Returns the node ID if created, nil if already exists."
  (unless (my/api--node-exists-p name)
    (let* ((slug (org-roam-node-slug (org-roam-node-create :title name)))
           (filename (format "%s-%s.org"
                            (format-time-string "%Y%m%d%H%M%S")
                            slug))
           (filepath (expand-file-name filename org-roam-directory))
           (id (org-id-new)))
      (with-temp-file filepath
        (insert (format ":PROPERTIES:\n:ID: %s\n:NODE-TYPE: person\n:END:\n#+title: %s\n"
                       id name)))
      (org-roam-db-sync)
      ;; Generate embeddings if org-roam-semantic is available
      (my/api--generate-and-save-embedding filepath)
      id)))

(defun my/api-log-to-inbox (text)
  "Log TEXT to inbox and auto-create person nodes for [[Name]] links.
Returns list of any person nodes that were created."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (daily-file (expand-file-name (concat today ".org")
                                       (expand-file-name "daily" org-roam-directory)))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (link-names (my/api--extract-link-names text))
         (created-people '()))

    ;; Auto-create person nodes for any [[Name]] links that don't exist
    (dolist (name link-names)
      (when (my/api--create-minimal-person name)
        (push name created-people)))

    ;; Ensure daily directory exists
    (unless (file-exists-p (file-name-directory daily-file))
      (make-directory (file-name-directory daily-file) t))

    ;; Create daily file if it doesn't exist
    (unless (file-exists-p daily-file)
      (with-temp-file daily-file
        (insert (format "#+title: %s\n#+filetags: :daily:\n\n" today))))

    (with-current-buffer (find-file-noselect daily-file)
      ;; Find or create Inbox section
      (goto-char (point-min))
      (let ((inbox-pos (re-search-forward "^\\* Inbox$" nil t)))
        (if inbox-pos
            (progn
              (end-of-line)
              (newline))
          ;; Create Inbox section at end
          (goto-char (point-max))
          (unless (bolp) (newline))
          (insert "* Inbox\n")))

      ;; Add the entry
      (insert (format "- %s %s\n" timestamp text))
      (save-buffer))

    (my/api--json-response
     `((success . t)
       (logged . ,text)
       (created_people . ,(nreverse created-people))))))

(provide 'org-roam-api)
;;; org-roam-api.el ends here

(defun my/api-read-note (identifier &optional section)
  "Read full content of a note by ID or path.
IDENTIFIER can be an org-roam ID or a path relative to org-roam-directory.
If SECTION is provided, return only that heading's content."
  (my/api--ensure-org-roam-db)
  (condition-case err
      (let* ((file (cond
                    ;; Check if it's a file path
                    ((and (stringp identifier)
                          (or (file-exists-p identifier)
                              (file-exists-p (expand-file-name identifier org-roam-directory))))
                     (if (file-exists-p identifier)
                         identifier
                       (expand-file-name identifier org-roam-directory)))
                    ;; Otherwise treat as org-roam ID
                    (t (when-let ((node (org-roam-node-from-id identifier)))
                         (org-roam-node-file node)))))
             (node (when file
                     (let ((id (caar (org-roam-db-query 
                                     [:select [id] :from nodes :where (= file $s1)] 
                                     file))))
                       (when id (org-roam-node-from-id id))))))
        (if (not file)
            (json-encode `((success . :json-false)
                          (error . ,(format "Note not found: %s" identifier))))
          (let* ((raw-content (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string)))
                 (content (my/api--strip-embedding-properties raw-content))
                 (title (when node (org-roam-node-title node)))
                 (properties (my/api--extract-properties file)))
            ;; If section specified, extract just that section
            (when section
              (setq content
                    (with-temp-buffer
                      (insert content)
                      (org-mode)
                      (goto-char (point-min))
                      (if (re-search-forward 
                           (format "^\\*+[ \t]+%s" (regexp-quote section)) nil t)
                          (let ((start (line-beginning-position))
                                (end (save-excursion
                                       (org-end-of-subtree t t)
                                       (point))))
                            (buffer-substring-no-properties start end))
                        (format "Section not found: %s" section)))))
            (json-encode
             `((success . t)
               (file . ,file)
               (title . ,title)
               (properties . ,properties)
               (content . ,content))))))
    (error
     (json-encode `((success . :json-false)
                   (error . ,(format "Error reading note: %s" (error-message-string err))))))))

(defun my/api-update-note (identifier content &optional section mode)
  "Update content in a note by ID or path.
IDENTIFIER can be an org-roam ID or a path relative to org-roam-directory.
CONTENT is the text to add.
SECTION is an optional heading name to target.
MODE is \"append\" (default), \"prepend\", or \"replace\"."
  (my/api--ensure-org-roam-db)
  (let ((mode (or mode "append")))
    (condition-case err
        (let* ((file (cond
                      ;; Check if it's a file path
                      ((and (stringp identifier)
                            (or (file-exists-p identifier)
                                (file-exists-p (expand-file-name identifier org-roam-directory))))
                       (if (file-exists-p identifier)
                           identifier
                         (expand-file-name identifier org-roam-directory)))
                      ;; Otherwise treat as org-roam ID
                      (t (when-let ((node (org-roam-node-from-id identifier)))
                           (org-roam-node-file node))))))
          (if (not file)
              (json-encode `((success . :json-false)
                            (error . ,(format "Note not found: %s" identifier))))
            (with-current-buffer (find-file-noselect file)
              (org-mode)
              (if section
                  ;; Target specific section
                  (progn
                    (goto-char (point-min))
                    (if (re-search-forward 
                         (format "^\\(\\*+\\)[ \t]+%s" (regexp-quote section)) nil t)
                        (let ((level (length (match-string 1))))
                          (cond
                           ((string= mode "replace")
                            ;; Replace section content (keep heading)
                            (end-of-line)
                            (let ((start (point))
                                  (end (save-excursion
                                         (org-end-of-subtree t t)
                                         (point))))
                              (delete-region start end)
                              (insert "\n" content "\n")))
                           ((string= mode "prepend")
                            ;; Insert right after heading
                            (end-of-line)
                            (insert "\n" content))
                           (t ; append (default)
                            ;; Insert at end of section, before next heading
                            (let ((end (save-excursion
                                         (org-end-of-subtree t t)
                                         (skip-chars-backward " \t\n")
                                         (point))))
                              (goto-char end)
                              (insert "\n" content)))))
                      ;; Section not found - create it
                      (goto-char (point-max))
                      (unless (bolp) (insert "\n"))
                      (insert (format "* %s\n%s\n" section content))))
                ;; No section - operate on whole file
                (cond
                 ((string= mode "replace")
                  ;; Replace everything after properties/title
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+title:.*\n+" nil t)
                    (delete-region (point) (point-max))
                    (insert content "\n")))
                 ((string= mode "prepend")
                  ;; Insert after properties/title
                  (goto-char (point-min))
                  (if (re-search-forward "^#\\+title:.*\n+" nil t)
                      (insert content "\n")
                    (goto-char (point-min))
                    (insert content "\n")))
                 (t ; append (default)
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert content "\n"))))
              (my/api--save-buffer-no-hooks)
              (let ((updated-content (my/api--strip-embedding-properties
                                      (buffer-substring-no-properties (point-min) (point-max)))))
                (json-encode
                 `((success . t)
                   (file . ,file)
                   (mode . ,mode)
                   (section . ,section)
                   (content_preview . ,(substring updated-content 0 (min 500 (length updated-content))))))))))
      (error
       (json-encode `((success . :json-false)
                     (error . ,(format "Error updating note: %s" (error-message-string err)))))))))

(defun my/api-list-notes (&optional node-type status limit sort-by)
  "List org-roam notes with optional filters.
NODE-TYPE: \"project\", \"person\", \"idea\", \"admin\", \"blog\", or nil for all.
STATUS: \"active\", \"stale\", \"done\", \"cancelled\", or nil for all.
LIMIT: Maximum number of results (default 50).
SORT-BY: \"created\", \"modified\", or \"title\" (default \"modified\")."
  (my/api--ensure-org-roam-db)
  (let* ((limit (or limit 50))
         (sort-by (or sort-by "modified"))
         (all-nodes (org-roam-node-list))
         (filtered-nodes
          (seq-filter
           (lambda (node)
             (let* ((file (org-roam-node-file node))
                    (props (my/api--extract-properties file))
                    (node-type-prop (cdr (assoc "node-type" props)))
                    (status-prop (downcase (or (cdr (assoc "status" props)) ""))))
               (and
                ;; Filter by node-type if specified
                (or (not node-type)
                    (string= (downcase (or node-type-prop "")) (downcase node-type)))
                ;; Filter by status if specified
                (or (not status)
                    (string= status-prop (downcase status))))))
           all-nodes))
         ;; Sort
         (sorted-nodes
          (sort filtered-nodes
                (lambda (a b)
                  (cond
                   ((string= sort-by "title")
                    (string< (org-roam-node-title a) (org-roam-node-title b)))
                   ((string= sort-by "created")
                    (time-less-p (org-roam-node-file-mtime b)
                                (org-roam-node-file-mtime a)))
                   (t ; modified (default) - most recent first
                    (time-less-p (org-roam-node-file-mtime b)
                                (org-roam-node-file-mtime a)))))))
         ;; Limit results
         (limited-nodes (seq-take sorted-nodes limit))
         ;; Convert to JSON format
         (results
          (mapcar
           (lambda (node)
             (let* ((file (org-roam-node-file node))
                    (props (my/api--extract-properties file)))
               `((id . ,(org-roam-node-id node))
                 (title . ,(org-roam-node-title node))
                 (file . ,file)
                 (node_type . ,(cdr (assoc "node-type" props)))
                 (status . ,(cdr (assoc "status" props)))
                 (priority . ,(cdr (assoc "priority" props)))
                 (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S"
                                                  (org-roam-node-file-mtime node))))))
           limited-nodes)))
    (json-encode
     `((success . t)
       (total_found . ,(length filtered-nodes))
       (returned . ,(length limited-nodes))
       (filters . ((node_type . ,node-type)
                   (status . ,status)
                   (sort_by . ,sort-by)
                   (limit . ,limit)))
       (notes . ,results)))))
(defun my/api-get-note-properties (identifier)
  "Get just metadata for a note without full content.
IDENTIFIER can be an org-roam ID or a path relative to org-roam-directory."
  (my/api--ensure-org-roam-db)
  (condition-case err
      (let* ((file (cond
                    ((and (stringp identifier)
                          (or (file-exists-p identifier)
                              (file-exists-p (expand-file-name identifier org-roam-directory))))
                     (if (file-exists-p identifier)
                         identifier
                       (expand-file-name identifier org-roam-directory)))
                    (t (when-let ((node (org-roam-node-from-id identifier)))
                         (org-roam-node-file node)))))
             (node (when file
                     (let ((id (caar (org-roam-db-query 
                                     [:select [id] :from nodes :where (= file $s1)] 
                                     file))))
                       (when id (org-roam-node-from-id id))))))
        (if (not file)
            (json-encode (list (cons (quote success) :json-false)
                              (cons (quote error) (format "Note not found: %s" identifier))))
          (let* ((properties (my/api--extract-properties file))
                 (title (when node (org-roam-node-title node)))
                 (tags (when node (org-roam-node-tags node)))
                 (forward-links (org-roam-db-query
                                [:select [dest] :from links :where (= source $s1)]
                                (org-roam-node-id node)))
                 (backlinks (mapcar (lambda (bl) (org-roam-node-id (org-roam-backlink-source-node bl)))
                                   (org-roam-backlinks-get node))))
            (json-encode
             (list (cons (quote success) t)
                   (cons (quote file) file)
                   (cons (quote id) (when node (org-roam-node-id node)))
                   (cons (quote title) title)
                   (cons (quote status) (cdr (assoc "status" properties)))
                   (cons (quote priority) (cdr (assoc "priority" properties)))
                   (cons (quote node_type) (cdr (assoc "node-type" properties)))
                   (cons (quote created) (cdr (assoc "created" properties)))
                   (cons (quote tags) tags)
                   (cons (quote links_to) (mapcar (function car) forward-links))
                   (cons (quote links_from) backlinks))))))
    (error
     (json-encode (list (cons (quote success) :json-false)
                       (cons (quote error) (format "Error getting properties: %s" (error-message-string err))))))))

(defun my/api-delete-note (identifier &optional archive)
  "Delete or archive a note.
IDENTIFIER can be an org-roam ID or path.
If ARCHIVE is non-nil, move to archive directory instead of deleting."
  (my/api--ensure-org-roam-db)
  (condition-case err
      (let* ((file (cond
                    ((and (stringp identifier)
                          (or (file-exists-p identifier)
                              (file-exists-p (expand-file-name identifier org-roam-directory))))
                     (if (file-exists-p identifier)
                         identifier
                       (expand-file-name identifier org-roam-directory)))
                    (t (when-let ((node (org-roam-node-from-id identifier)))
                         (org-roam-node-file node))))))
        (if (not file)
            (json-encode (list (cons (quote success) :json-false)
                              (cons (quote error) (format "Note not found: %s" identifier))))
          (let ((archive-dir (expand-file-name "archive" org-roam-directory)))
            (if archive
                (progn
                  (unless (file-exists-p archive-dir)
                    (make-directory archive-dir t))
                  (rename-file file (expand-file-name (file-name-nondirectory file) archive-dir))
                  (org-roam-db-sync)
                  (json-encode (list (cons (quote success) t)
                                    (cons (quote action) "archived")
                                    (cons (quote file) file))))
              (delete-file file)
              (org-roam-db-sync)
              (json-encode (list (cons (quote success) t)
                                (cons (quote action) "deleted")
                                (cons (quote file) file)))))))
    (error
     (json-encode (list (cons (quote success) :json-false)
                       (cons (quote error) (format "Error: %s" (error-message-string err))))))))

(defun my/api-rename-note (identifier new-title)
  "Rename a note, updating its title and filename.
IDENTIFIER can be an org-roam ID or path.
NEW-TITLE is the new title for the note."
  (my/api--ensure-org-roam-db)
  (condition-case err
      (let* ((file (cond
                    ((and (stringp identifier)
                          (or (file-exists-p identifier)
                              (file-exists-p (expand-file-name identifier org-roam-directory))))
                     (if (file-exists-p identifier)
                         identifier
                       (expand-file-name identifier org-roam-directory)))
                    (t (when-let ((node (org-roam-node-from-id identifier)))
                         (org-roam-node-file node))))))
        (if (not file)
            (json-encode (list (cons (quote success) :json-false)
                              (cons (quote error) (format "Note not found: %s" identifier))))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (when (re-search-forward "^#\\+title:.*$" nil t)
              (replace-match (format "#+title: %s" new-title)))
            (my/api--save-buffer-no-hooks))
          (let* ((node (org-roam-node-at-point))
                 (new-slug (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" new-title)))
                 (id (when node (org-roam-node-id node)))
                 (new-filename (format "%s-%s.org" new-slug (or id (format-time-string "%s"))))
                 (new-path (expand-file-name new-filename (file-name-directory file))))
            (unless (string= file new-path)
              (rename-file file new-path)
              (org-roam-db-sync))
            (json-encode (list (cons (quote success) t)
                              (cons (quote old_file) file)
                              (cons (quote new_file) new-path)
                              (cons (quote new_title) new-title))))))
    (error
     (json-encode (list (cons (quote success) :json-false)
                       (cons (quote error) (format "Error: %s" (error-message-string err))))))))

(defun my/api-manage-tags (identifier action tag)
  "Add or remove a tag from a note.
IDENTIFIER can be an org-roam ID or path.
ACTION is \"add\" or \"remove\".
TAG is the tag to add or remove (without colons)."
  (my/api--ensure-org-roam-db)
  (condition-case err
      (let* ((file (cond
                    ((and (stringp identifier)
                          (or (file-exists-p identifier)
                              (file-exists-p (expand-file-name identifier org-roam-directory))))
                     (if (file-exists-p identifier)
                         identifier
                       (expand-file-name identifier org-roam-directory)))
                    (t (when-let ((node (org-roam-node-from-id identifier)))
                         (org-roam-node-file node))))))
        (if (not file)
            (json-encode (list (cons (quote success) :json-false)
                              (cons (quote error) (format "Note not found: %s" identifier))))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (let ((filetags-line (when (re-search-forward "^#\\+filetags:\\s-*\\(.*\\)$" nil t)
                                  (match-string 1))))
              (goto-char (point-min))
              (cond
               ((string= action "add")
                (if filetags-line
                    (progn
                      (re-search-forward "^#\\+filetags:.*$" nil t)
                      (replace-match (format "#+filetags: %s:%s:" 
                                            (string-trim-right filetags-line ":") tag)))
                  (re-search-forward "^#\\+title:.*$" nil t)
                  (end-of-line)
                  (insert (format "\n#+filetags: :%s:" tag))))
               ((string= action "remove")
                (when filetags-line
                  (re-search-forward "^#\\+filetags:.*$" nil t)
                  (replace-match (format "#+filetags: %s" 
                                        (replace-regexp-in-string (format ":%s:" tag) ":" filetags-line))))))
              (my/api--save-buffer-no-hooks)
              (org-roam-db-sync)
              (json-encode (list (cons (quote success) t)
                                (cons (quote action) action)
                                (cons (quote tag) tag)
                                (cons (quote file) file)))))))
    (error
     (json-encode (list (cons (quote success) :json-false)
                       (cons (quote error) (format "Error: %s" (error-message-string err))))))))

(defun my/api-add-link (from-id to-id &optional section)
  "Add an org-roam link from one note to another.
FROM-ID is the source note ID.
TO-ID is the destination note ID.
SECTION is optional heading to add link under."
  (my/api--ensure-org-roam-db)
  (condition-case err
      (let* ((from-node (org-roam-node-from-id from-id))
             (to-node (org-roam-node-from-id to-id))
             (from-file (when from-node (org-roam-node-file from-node)))
             (to-title (when to-node (org-roam-node-title to-node))))
        (if (not (and from-file to-node))
            (json-encode (list (cons (quote success) :json-false)
                              (cons (quote error) "Source or destination note not found")))
          (with-current-buffer (find-file-noselect from-file)
            (if section
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward (format "^\\*+[ \t]+%s" (regexp-quote section)) nil t)
                      (progn
                        (org-end-of-subtree t t)
                        (skip-chars-backward " \t\n")
                        (insert (format "\n- [[id:%s][%s]]" to-id to-title)))
                    (goto-char (point-max))
                    (insert (format "\n* %s\n- [[id:%s][%s]]" section to-id to-title))))
              (goto-char (point-max))
              (insert (format "\n- [[id:%s][%s]]" to-id to-title)))
            (my/api--save-buffer-no-hooks)
            (org-roam-db-sync)
            (json-encode (list (cons (quote success) t)
                              (cons (quote from) from-id)
                              (cons (quote to) to-id)
                              (cons (quote to_title) to-title))))))
    (error
     (json-encode (list (cons (quote success) :json-false)
                       (cons (quote error) (format "Error: %s" (error-message-string err))))))))
