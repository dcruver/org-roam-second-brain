;;; org-roam-vector-search.el --- Vector embeddings and AI assistance for org-roam -*- lexical-binding: t; -*-

;; Author: David Cruver <dcruver@users.noreply.github.com>
;; URL: https://github.com/dcruver/org-roam-second-brain
;; Version: 1.4.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.2"))

;;; Commentary:
;; This package adds vector embedding support and direct AI integration to org-roam.
;; It stores embeddings as org properties and provides semantic similarity search.
;; Supports OpenAI-compatible APIs (vLLM, Infinity, etc.) for embeddings and generation.

;;; Code:

(require 'org-roam)
(require 'json)
(require 'url)
(require 'org)
(require 'ox-md)
(require 'cl-lib)
(require 'subr-x)

;;; Version

(defconst org-roam-semantic-version "1.4.0"
  "Version of the org-roam-semantic package suite.")

(defun org-roam-semantic-version ()
  "Display the version of org-roam-semantic."
  (interactive)
  (message "org-roam-semantic version %s" org-roam-semantic-version))

;;; Configuration

(defgroup org-roam-vector-search nil
  "Vector embeddings and semantic search for org-roam."
  :group 'org-roam
  :prefix "org-roam-semantic-")

(defcustom org-roam-semantic-embedding-url "http://localhost:8080"
  "Base URL for embeddings API server (OpenAI-compatible).
This should point to your embedding service. Common values:
- http://localhost:8080 (Infinity local)
- http://localhost:11434 (Ollama local)"
  :type 'string
  :group 'org-roam-vector-search)

(defcustom org-roam-semantic-generation-url "http://localhost:8000/v1"
  "Base URL for text generation API server (OpenAI-compatible).
This should point to your LLM service. Common values:
- http://localhost:8000/v1 (vLLM local)
- http://localhost:11434 (Ollama local)"
  :type 'string
  :group 'org-roam-vector-search)

(defcustom org-roam-semantic-embedding-model "nomic-ai/nomic-embed-text-v1.5"
  "Model to use for generating embeddings.
Must be available in your embedding service.
Recommended models:
- nomic-ai/nomic-embed-text-v1.5 (Infinity default)
- nomic-embed-text (Ollama)
- BAAI/bge-base-en-v1.5 (good alternative)"
  :type 'string
  :group 'org-roam-vector-search)

(defcustom org-roam-semantic-generation-model "casperhansen/llama-3.3-70b-instruct-awq"
  "Model to use for text generation and AI assistance.
Must be available in your LLM service.
Popular models:
- casperhansen/llama-3.3-70b-instruct-awq (vLLM)
- llama3.1:8b (Ollama)
- qwen2.5:7b (Ollama alternative)"
  :type 'string
  :group 'org-roam-vector-search)

(defcustom org-roam-semantic-embedding-dimensions 768
  "Number of dimensions in the embedding vectors.
This should match your embedding model:
- nomic-embed-text: 768
- all-minilm: 384
- mxbai-embed-large: 1024
Change this only if you switch embedding models."
  :type 'integer
  :group 'org-roam-vector-search)

;; Chunking is now always enabled for optimal semantic search performance

(defcustom org-roam-semantic-min-chunk-size 100
  "Minimum word count for a section to get its own embedding.
Sections smaller than this will not have embeddings generated."
  :type 'integer
  :group 'org-roam-vector-search)

(defcustom org-roam-semantic-max-chunk-size 1000
  "Maximum word count for a single chunk.
Sections longer than this will be split into smaller chunks."
  :type 'integer
  :group 'org-roam-vector-search)

(defcustom org-roam-semantic-similarity-cutoff 0.55
  "Similarity cutoff threshold for related notes.
Notes with similarity below this threshold will be excluded from results.
Higher values (closer to 1.0) mean more similar notes only."
  :type 'float
  :group 'org-roam-vector-search)

;;; Utility Functions

(defun org-roam-semantic-get-similar-data (query-text &optional limit cutoff)
  "Get similarity data programmatically.
Returns list of (file similarity position heading-text) tuples.
Searches chunks within files when chunking is enabled.
If CUTOFF is provided, filters results to only include similarities above that threshold."
  (let ((limit (or limit 10))
        (cutoff (or cutoff 0.0))
        (similarities '()))
    ;; Generate embedding for query
    (let ((query-embedding (org-roam-ai-generate-embedding query-text)))
      (if query-embedding
          (progn
            ;; Compare with all notes that have embeddings
            (dolist (file (org-roam-list-files))
              ;; Search chunks within file
              (let ((all-embeddings (org-roam-semantic--get-all-embeddings file)))
                (dolist (chunk all-embeddings)
                  (let* ((position (nth 0 chunk))
                         (heading-text (nth 1 chunk))
                         (embedding (nth 2 chunk))
                         (similarity (org-roam-semantic--cosine-similarity query-embedding embedding)))
                    (when (and similarity (>= similarity cutoff))
                      (push (list file similarity position heading-text) similarities))))))
            ;; Sort by similarity and take top results
            (setq similarities (sort similarities (lambda (a b) (> (cadr a) (cadr b)))))
            (if (> (length similarities) limit)
                (butlast similarities (- (length similarities) limit))
              similarities))
        (message "Failed to generate embedding for query")
        nil))))

(defun org-roam-semantic--normalize-text (text)
  "Normalize text for embedding by removing extra whitespace and formatting."
  (when text
    (let ((normalized (replace-regexp-in-string "[ \t\n\r]+" " " text)))
      (string-trim normalized))))

(defun org-roam-semantic--get-content (file)
  "Extract content including title, properly skipping all front matter."
  (with-temp-buffer
    (insert-file-contents file)

    (let (title content)
      ;; Extract title
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
        (setq title (match-string 1)))

      ;; Skip to after properties drawer
      (goto-char (point-min))
      (when (re-search-forward "^:END:" nil t)
        (forward-line 1))

      ;; Skip ALL keyword lines (#+title:, #+filetags:, etc.)
      (while (and (not (eobp))
                  (looking-at "^#\\+[a-zA-Z_-]+:"))
        (forward-line 1))

      ;; Skip blank lines
      (while (and (not (eobp))
                  (looking-at "^\\s-*$"))
        (forward-line 1))

      ;; Get actual content
      (setq content (buffer-substring-no-properties (point) (point-max)))

      (org-roam-semantic--normalize-text
       (if title
           (concat title ". " content)
         content)))))

(defun org-roam-semantic--vector-magnitude (vector)
  "Calculate the magnitude of a vector."
  (sqrt (apply '+ (mapcar (lambda (x) (* x x)) vector))))

(defun org-roam-semantic--cosine-similarity (vec1 vec2)
  "Calculate cosine similarity between two vectors."
  (condition-case err
      (when (and vec1 vec2
                 (listp vec1) (listp vec2)
                 (= (length vec1) (length vec2))
                 (> (length vec1) 0))
        (let* ((dot-product 0.0)
               (mag1-sq 0.0)
               (mag2-sq 0.0)
               (valid-count 0))
          ;; Calculate dot product and magnitudes in one pass
          (dotimes (i (length vec1))
            (let ((v1 (nth i vec1))
                  (v2 (nth i vec2)))
              (when (and (numberp v1) (numberp v2))
                (setq dot-product (+ dot-product (* v1 v2)))
                (setq mag1-sq (+ mag1-sq (* v1 v1)))
                (setq mag2-sq (+ mag2-sq (* v2 v2)))
                (setq valid-count (1+ valid-count)))))
          ;; Only calculate if we have valid numbers
          (when (> valid-count 0)
            (let ((mag1 (sqrt mag1-sq))
                  (mag2 (sqrt mag2-sq)))
              (if (or (zerop mag1) (zerop mag2))
                  0.0
                (/ dot-product (* mag1 mag2)))))))
    (error
     (message "Error in cosine similarity calculation: %s" err)
     nil)))

;;; Chunking Functions

(defun org-roam-semantic--count-words (text)
  "Count words in TEXT."
  (length (split-string (org-roam-semantic--normalize-text text) "\\s-+" t)))

(defun org-roam-semantic--generate-chunk-id ()
  "Generate a unique ID for a chunk."
  (org-id-new))

(defun org-roam-semantic--parse-chunks (file)
  "Parse FILE and return list of chunks with metadata.
Returns list of (position heading-text content word-count level)."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((chunks '())
          (file-title nil))

      ;; Get file title for file-level chunk
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
        (setq file-title (match-string 1)))

      ;; Use org-mode outline navigation instead of org-map-entries
      (goto-char (point-min))
      (message "Debug Parse: Starting manual heading scan...")
      (let ((heading-count 0))
        (while (re-search-forward "^\\*+ " nil t)
          (save-excursion
            (beginning-of-line)
            (when (org-at-heading-p)
              (cl-incf heading-count)
              (let* ((heading-pos (point))
                     (heading-components (org-heading-components))
                     (heading-text (nth 4 heading-components))
                     (level (nth 0 heading-components))
                     (content-start (progn
                                      (forward-line 1)
                                      (point)))
                     (content-end (save-excursion
                                    (if (outline-next-heading)
                                        (point)
                                      (point-max))))
                     (content (buffer-substring-no-properties content-start content-end))
                     (full-content (concat heading-text ". " content))
                     (word-count (org-roam-semantic--count-words full-content)))

                ;; Debug: Show all sections found
                (message "Debug Parse: Found section '%s' at pos %d with %d words (level %d)"
                         heading-text heading-pos word-count level)

                ;; Include all chunks - mark those below threshold differently
                (if (>= word-count org-roam-semantic-min-chunk-size)
                    (progn
                      (message "Debug Parse: INCLUDING '%s' (%d words >= %d minimum)"
                               heading-text word-count org-roam-semantic-min-chunk-size)
                      (push (list heading-pos heading-text full-content word-count level :embedding) chunks))
                  (progn
                    (message "Debug Parse: INCLUDING (ID-only) '%s' (%d words < %d minimum)"
                             heading-text word-count org-roam-semantic-min-chunk-size)
                    (push (list heading-pos heading-text full-content word-count level :id-only) chunks)))))))
        (message "Debug Parse: Manual scan completed, found %d headings" heading-count))



      (nreverse chunks))))

(defun org-roam-semantic--get-chunk-content (file position)
  "Get the content for a chunk at POSITION in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char position)
    (if (= position (point-min))
        ;; File-level chunk
        (org-roam-semantic--get-content file)
      ;; Section-level chunk
      (let* ((level (save-excursion
                      (when (looking-at "^\\(\\*+\\)")
                        (length (match-string 1)))))
             (heading-text (save-excursion
                             (when (looking-at "^\\*+\\s-+\\(.+\\)$")
                               (match-string 1))))
             (content-start (progn (forward-line 1) (point)))
             (content-end (progn
                            (if (re-search-forward (format "^\\*\\{1,%d\\}\\s-" level) nil t)
                                (match-beginning 0)
                              (point-max))))
             (content (buffer-substring-no-properties content-start content-end)))
        (concat heading-text ". " content)))))

;;; OpenAI-Compatible API Functions

(defun org-roam-ai-generate-embedding (text)
  "Call OpenAI-compatible embeddings API synchronously.
Works with Infinity, vLLM, and other OpenAI-compatible embedding services."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string
                           (json-encode `((model . ,org-roam-semantic-embedding-model)
                                        (input . ,text)))
                           'utf-8))
        (url (concat org-roam-semantic-embedding-url "/embeddings")))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (let* ((json-response (decode-coding-string
                                (buffer-substring (point) (point-max)) 'utf-8))
                 (data (json-read-from-string json-response))
                 ;; OpenAI format: {"data": [{"embedding": [...]}]}
                 (data-array (cdr (assoc 'data data)))
                 (first-result (if (vectorp data-array) (aref data-array 0) (car data-array)))
                 (embedding (cdr (assoc 'embedding first-result))))
            (kill-buffer (current-buffer))
            ;; Convert vector to list if necessary
            (if (vectorp embedding)
                (append embedding nil)
              embedding)))
      (error
       (message "Error calling embeddings API: %s" err)
       nil))))

(defun org-roam-ai-generate-text (prompt &optional system-prompt)
  "Call OpenAI-compatible chat completions API synchronously.
Works with vLLM, OpenAI, and other compatible services."
  (let* ((messages (if system-prompt
                       `[((role . "system") (content . ,system-prompt))
                         ((role . "user") (content . ,prompt))]
                     `[((role . "user") (content . ,prompt))]))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-encode `((model . ,org-roam-semantic-generation-model)
                                         (messages . ,messages)
                                         (stream . :json-false)))
                            'utf-8))
         (url (concat org-roam-semantic-generation-url "/chat/completions")))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (let* ((json-response (decode-coding-string
                                (buffer-substring (point) (point-max)) 'utf-8))
                 (data (json-read-from-string json-response))
                 ;; OpenAI format: {"choices": [{"message": {"content": "..."}}]}
                 (choices (cdr (assoc 'choices data)))
                 (first-choice (if (vectorp choices) (aref choices 0) (car choices)))
                 (message-obj (cdr (assoc 'message first-choice)))
                 (content (cdr (assoc 'content message-obj))))
            (kill-buffer (current-buffer))
            content))
      (error
       (message "Error calling chat API: %s" err)
       nil))))

;;; Embedding Storage and Retrieval
(defun org-roam-semantic--store-embedding (file embedding &optional identifier)
  "Store EMBEDDING vector in FILE at location specified by IDENTIFIER.
If IDENTIFIER is nil, stores at file level.
If IDENTIFIER is a string, finds heading by that text.
If IDENTIFIER is a number, treats it as a position.
Does NOT call `save-buffer` (so it is safe in save hooks)."
  (when embedding
    (with-current-buffer (find-file-noselect file)
      (require 'org)
      (save-excursion
        (org-with-wide-buffer
          (if identifier
              ;; Store at heading level - find heading by identifier
              (progn
                (cond
                 ;; String identifier - search by heading text
                 ((stringp identifier)
                  (goto-char (point-min))
                  (unless (re-search-forward (format "^\\*+\\s-+%s\\s-*$" (regexp-quote identifier)) nil t)
                    (error "Cannot find heading with text: %s" identifier))
                  (beginning-of-line))
                 ;; Numeric identifier - go to position
                 ((numberp identifier)
                  (goto-char identifier)
                  (beginning-of-line)
                  (unless (looking-at "^\\*")
                    (error "Position %d is not at a heading" identifier)))
                 (t
                  (error "Invalid identifier type: %s" identifier)))
                ;; Ensure heading has an ID
                (unless (org-entry-get (point) "ID")
                  (org-entry-put (point) "ID" (org-roam-semantic--generate-chunk-id)))
                ;; Store embedding
                (org-entry-put (point) "EMBEDDING"
                               (mapconcat (lambda (x) (format "%.6f" x)) embedding " "))
                ;; Mark buffer as modified to ensure save hooks trigger
                (set-buffer-modified-p t))
            ;; Store at file level
            (progn
              (goto-char (point-min))
              ;; Ensure a file-level property drawer exists
              (unless (org-get-property-block) (org-insert-property-drawer))
              ;; Replace the property value at the file level
              (org-entry-put (point) "EMBEDDING"
                             (mapconcat (lambda (x) (format "%.6f" x)) embedding " "))
              ;; Mark buffer as modified to ensure save hooks trigger
              (set-buffer-modified-p t))))
      ;; IMPORTANT: do NOT call (save-buffer) here
      ))))

(defun org-roam-semantic--ensure-heading-id (file heading-text)
  "Ensure HEADING-TEXT in FILE has an ID property, even without embedding.
This allows short sections to get IDs for future expansion."
  (with-current-buffer (find-file-noselect file)
    (require 'org)
    (save-excursion
      (org-with-wide-buffer
        (goto-char (point-min))
        (when (re-search-forward (format "^\\*+\\s-+%s\\s-*$" (regexp-quote heading-text)) nil t)
          (beginning-of-line)
          ;; Only add ID if one doesn't exist
          (unless (org-entry-get (point) "ID")
            (org-entry-put (point) "ID" (org-roam-semantic--generate-chunk-id))
            (message "Added ID to short section: %s" heading-text)
            ;; Mark buffer as modified
            (set-buffer-modified-p t)))))))

(defun org-roam-semantic--get-embedding (file &optional position)
  "Retrieve embedding vector from FILE at POSITION.
If POSITION is nil, gets file-level embedding.
If POSITION is specified, gets embedding from heading at that position."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (or position (point-min)))
    (when (re-search-forward "^[ \t]*:EMBEDDING:[ \t]*\\(.*\\)$"
                             (if position
                                 (save-excursion
                                   (forward-line 10) ; Look within property drawer
                                   (point))
                               nil) t)
      (let ((embedding-str (match-string 1)))
        (when (and embedding-str (not (string-empty-p embedding-str)))
          (condition-case err
              (mapcar 'string-to-number (split-string embedding-str))
            (error
             (message "Error parsing embedding in %s: %s" (file-name-nondirectory file) err)
             nil)))))))

(defun org-roam-semantic--get-all-embeddings (file)
  "Retrieve all embeddings from FILE.
Returns list of (position heading-text embedding) tuples."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((embeddings '())
          (file-title nil))

      ;; Get file title
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
        (setq file-title (match-string 1)))

      ;; Check for file-level embedding
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:EMBEDDING:[ \t]*\\(.*\\)$" nil t)
        (let ((embedding-str (match-string 1)))
          (when (and embedding-str (not (string-empty-p embedding-str)))
            (condition-case err
                (let ((embedding (mapcar 'string-to-number (split-string embedding-str))))
                  (push (list (point-min) (or file-title "File") embedding) embeddings))
              (error
               (message "Error parsing file-level embedding: %s" err))))))

      ;; Find all heading-level embeddings
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\)\\s-+\\(.+\\)$" nil t)
        (let* ((heading-pos (match-beginning 0))
               (heading-text (match-string 2))
               (property-end (save-excursion
                               (forward-line 1)
                               (when (looking-at "^[ \t]*:PROPERTIES:")
                                 (re-search-forward "^[ \t]*:END:" nil t)
                                 (point)))))
          (when property-end
            (save-excursion
              (goto-char heading-pos)
              (when (re-search-forward "^[ \t]*:EMBEDDING:[ \t]*\\(.*\\)$" property-end t)
                (let ((embedding-str (match-string 1)))
                  (when (and embedding-str (not (string-empty-p embedding-str)))
                    (condition-case err
                        (let ((embedding (mapcar 'string-to-number (split-string embedding-str))))
                          (push (list heading-pos heading-text embedding) embeddings))
                      (error
                       (message "Error parsing embedding for %s: %s" heading-text err))))))))))

      (nreverse embeddings))))

(defun org-roam-semantic--has-embedding-p (file)
  "Check if note already has chunk embeddings."
  (not (null (org-roam-semantic--get-all-embeddings file))))

;;; Main Embedding Functions

(defun org-roam-semantic--generate-embedding (text)
  "Generate embedding for text synchronously."
  (org-roam-ai-generate-embedding text))

;;;###autoload
(defun org-roam-semantic-generate-embedding (file)
  "Generate and store chunk embeddings for a single note."
  (interactive (list (buffer-file-name)))
  (unless file
    (error "No file associated with current buffer"))
  (org-roam-semantic-generate-chunks-for-file file))

;;;###autoload
(defun org-roam-semantic-generate-chunks-for-file (file)
  "Generate embeddings for all chunks in FILE."
  (interactive (list (buffer-file-name)))
  (unless file
    (error "No file associated with current buffer"))
  (let* ((chunks (org-roam-semantic--parse-chunks file))
         (total (length chunks))
         (processed 0)
         (skipped 0))
    (message "Generating embeddings for %d chunks in %s..." total (file-name-nondirectory file))

    (dolist (chunk chunks)
      (let* ((position (nth 0 chunk))
             (heading-text (nth 1 chunk))
             (content (nth 2 chunk))
             (word-count (nth 3 chunk))
             (level (nth 4 chunk))
             (chunk-type (nth 5 chunk))
             (existing-embedding (org-roam-semantic--get-embedding file position)))

        (message "Debug: Chunk '%s' at position %d with %d words (type: %s)" heading-text position word-count chunk-type)

        (cond
         ;; ID-only chunks - just ensure they have an ID
         ((eq chunk-type :id-only)
          (org-roam-semantic--ensure-heading-id file heading-text)
          (cl-incf processed)
          (message "Added ID to short section: %s [%d/%d]" heading-text (+ processed skipped) total))

         ;; Embedding chunks - check if already has embedding
         (existing-embedding
          (cl-incf skipped)
          (message "Skipping %s (already has embedding) [%d/%d]" heading-text (+ processed skipped) total))

         ;; Embedding chunks without embeddings - generate them
         (t
          (cl-incf processed)
          (message "Processing %s [%d/%d]..." heading-text (+ processed skipped) total)
          (condition-case err
              (let ((embedding (org-roam-ai-generate-embedding content)))
                (if embedding
                    (progn
                      (org-roam-semantic--store-embedding file embedding heading-text)
                      (message "Successfully stored embedding for %s" heading-text))
                  (message "Failed to generate embedding for %s" heading-text)))
            (error
             (message "Error processing %s: %s" heading-text (error-message-string err))))))))

    (message "Chunk embedding generation complete for %s: %d processed, %d skipped"
             (file-name-nondirectory file) processed skipped)))

;;;###autoload
(defun org-roam-semantic-generate-all-embeddings ()
  "Generate chunk embeddings for all org-roam notes that don't have them."
  (interactive)
  (org-roam-semantic-generate-all-chunks))

;;;###autoload
(defun org-roam-semantic-generate-all-chunks ()
  "Generate chunk embeddings for all org-roam notes."
  (interactive)

  (let* ((files (org-roam-list-files))
         (total-files (length files))
         (file-count 0)
         (total-chunks 0)
         (processed-chunks 0)
         (skipped-chunks 0))

    (message "Starting chunk embedding generation for %d files..." total-files)

    (dolist (file files)
      (cl-incf file-count)
      (message "Processing file %d/%d: %s" file-count total-files (file-name-nondirectory file))

      (let* ((chunks (org-roam-semantic--parse-chunks file))
             (file-chunk-count (length chunks)))
        (cl-incf total-chunks file-chunk-count)

        (dolist (chunk chunks)
          (let* ((position (nth 0 chunk))
                 (heading-text (nth 1 chunk))
                 (content (nth 2 chunk))
                 (word-count (nth 3 chunk))
                 (level (nth 4 chunk))
                 (chunk-type (nth 5 chunk))
                 (existing-embedding (org-roam-semantic--get-embedding file position)))

            (cond
             ;; ID-only chunks - just ensure they have an ID
             ((eq chunk-type :id-only)
              (org-roam-semantic--ensure-heading-id file heading-text)
              (cl-incf processed-chunks)
              (message "  Added ID to short section: %s [%d/%d]" heading-text (+ processed-chunks skipped-chunks) total-chunks))

             ;; Embedding chunks - check if already has embedding
             (existing-embedding
              (cl-incf skipped-chunks)
              (message "  Skipping %s (already has embedding) [%d/%d]" heading-text (+ processed-chunks skipped-chunks) total-chunks))

             ;; Embedding chunks without embeddings - generate them
             (t
              (cl-incf processed-chunks)
              (message "  Processing %s [%d/%d]..." heading-text (+ processed-chunks skipped-chunks) total-chunks)
              (let ((embedding (org-roam-ai-generate-embedding content)))
                (if embedding
                    (progn
                      (org-roam-semantic--store-embedding file embedding heading-text)
                      (message "  Successfully stored embedding for %s" heading-text))
                  (message "  Failed to generate embedding for %s" heading-text)))))))))

    (message "Chunk embedding generation complete: %d files processed, %d chunks total, %d processed, %d skipped"
             total-files total-chunks processed-chunks skipped-chunks)))

;;; Vector Search Functions

(defun org-roam-semantic--get-title (file)
  "Extract the title from an org-roam note file."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
            (string-trim (match-string 1))
          (file-name-sans-extension (file-name-nondirectory file))))
    (error (file-name-sans-extension (file-name-nondirectory file)))))

(defun org-roam-semantic--get-node-id (file)
  "Extract the node ID from an org-roam note file."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^:ID:\\s-*\\(.+\\)$" nil t)
          (string-trim (match-string 1))))
    (error nil)))

;;;###autoload
(defun org-roam-semantic-find-similar (query-text &optional limit chunk-level)
  "Find notes similar to the query text and display in a results buffer with clickable links.
If CHUNK-LEVEL is non-nil, searches chunks instead of whole files."
  (interactive "sSearch for concept: ")
  (let ((similarities (org-roam-semantic-get-similar-data query-text (or limit 10) chunk-level)))
    (if similarities
        (with-current-buffer (get-buffer-create (if chunk-level "*Similar Chunks*" "*Similar Notes*"))
          (erase-buffer)
          (org-mode) ; Enable org-mode for clickable links
          (insert (format "* Similar %s for: %s\n\n"
                         (if chunk-level "chunks" "notes") query-text))
          (insert "Click links to open notes, or copy the org-roam links below:\n\n")

          (dolist (result similarities)
            (let* ((file (car result))
                   (similarity (cadr result))
                   (position (when chunk-level (nth 2 result)))
                   (heading-text (when chunk-level (nth 3 result)))
                   (title (org-roam-semantic--get-title file))
                   (node-id (org-roam-semantic--get-node-id file)))

              (if chunk-level
                  ;; Chunk result
                  (progn
                    (insert (format "** %.3f - [[file:%s::%s][%s > %s]]\n"
                                   similarity file
                                   (if (= position (point-min)) title heading-text)
                                   title heading-text))
                    (when node-id
                      (insert (format "   Org-roam link: =[[id:%s][%s]]=\n" node-id title))))
                ;; File result
                (progn
                  (insert (format "** %.3f - [[file:%s][%s]]\n" similarity file title))
                  (when node-id
                    (insert (format "   Org-roam link: =[[id:%s][%s]]=\n" node-id title)))))
              (insert "\n")))

          (insert "\n** Usage:\n")
          (insert "- Click file links to open notes\n")
          (when chunk-level
            (insert "- File links will jump to the specific section\n"))
          (insert "- Copy org-roam links (the =[[id:...]]= parts) to insert elsewhere\n")
          (insert "- Use C-c C-c on org-roam links to follow them\n")

          (goto-char (point-min))
          (display-buffer (current-buffer))
          (message "Found %d similar %s - click links to open"
                  (length similarities) (if chunk-level "chunks" "notes")))
      (message "No similar %s found" (if chunk-level "chunks" "notes")))))

(defun org-roam-semantic-find-and-insert (query-text &optional limit)
  "Find similar notes and insert org-roam links into the current buffer."
  (interactive "sSearch for concept: ")
  (if (not (derived-mode-p 'org-mode))
      (message "This function only works in org-mode buffers")
    (let ((similarities (org-roam-semantic-get-similar-data query-text (or limit 5))))
      (if similarities
          (progn
            (insert (format "\n** Related Notes - %s\n" query-text))
            (dolist (result similarities)
              (let* ((file (car result))
                     (similarity (cadr result))
                     (title (org-roam-semantic--get-title file))
                     (node-id (org-roam-semantic--get-node-id file)))
                (if node-id
                    (insert (format "- [[id:%s][%s]] (%.3f)\n" node-id title similarity))
                  (insert (format "- [[file:%s][%s]] (%.3f)\n" file title similarity)))))
            (insert "\n")
            (message "Inserted %d similar note links" (length similarities)))
        (message "No similar notes found")))))

;;;###autoload
(defun org-roam-semantic-search (concept)
  "Interactive search for notes by concept - displays results buffer."
  (interactive "sConcept to search for: ")
  (org-roam-semantic-find-similar concept))

;;;###autoload
(defun org-roam-semantic-search-chunks (concept)
  "Interactive search for note chunks by concept - displays results buffer."
  (interactive "sConcept to search for (chunks): ")
  (org-roam-semantic-find-similar concept t))

;;;###autoload
(defun org-roam-semantic-insert-related (concept)
  "Search for related notes and insert links at point."
  (interactive "sConcept to find related notes for: ")
  (org-roam-semantic-find-and-insert concept))

;;;###autoload
(defun org-roam-semantic-insert-similar (&optional cutoff)
  "Find notes similar to current note and insert org-roam links at point.
Uses similarity cutoff from `org-roam-semantic-similarity-cutoff' or CUTOFF if provided.
All notes above the similarity threshold will be inserted."
  (interactive "P")
  (if (not (and (derived-mode-p 'org-mode) (org-roam-file-p)))
      (message "This function only works in org-roam files")
    (let* ((current-file (buffer-file-name))
           (title (or (org-roam-get-keyword "TITLE")
                     (file-name-sans-extension (file-name-nondirectory current-file))))
           (content (org-roam-semantic--get-content current-file))
           (query-text (or content title))
           (cutoff (or cutoff org-roam-semantic-similarity-cutoff))
           (similarities (org-roam-semantic-get-similar-data query-text nil cutoff))) ; Use cutoff, no limit

      ;; Filter out the current note from results
      (setq similarities (seq-remove (lambda (result)
                                      (string= (car result) current-file))
                                    similarities))

      (if similarities
          (progn
            (insert (format "\n** Related Notes (similarity >= %.2f)\n" cutoff))
            (dolist (result similarities)
              (let* ((file (car result))
                     (similarity (cadr result))
                     (title (org-roam-semantic--get-title file))
                     (node-id (org-roam-semantic--get-node-id file)))
                (if node-id
                    (insert (format "- [[id:%s][%s]] (%.3f)\n" node-id title similarity))
                  (insert (format "- [[file:%s][%s]] (%.3f)\n" file title similarity)))))
            (insert "\n")
            (message "Inserted %d similar note links (similarity >= %.2f)" (length similarities) cutoff))
        (message "No similar notes found above similarity threshold %.2f" cutoff)))))

;;; Status and Maintenance Functions

;;;###autoload
(defun org-roam-semantic-debug-embedding (file)
  "Debug embedding for a specific file."
  (interactive (list (read-file-name "Check embedding for file: "
                                     org-roam-directory nil t)))
  (let ((embedding (org-roam-semantic--get-embedding file)))
    (if embedding
        (message "File: %s\nEmbedding: %d dimensions\nFirst few values: %s"
                 (file-name-nondirectory file)
                 (length embedding)
                 (mapconcat 'number-to-string
                           (list (nth 0 embedding) (nth 1 embedding) (nth 2 embedding)
                                 (nth 3 embedding) (nth 4 embedding)) ", "))
      (message "File: %s has no embedding" (file-name-nondirectory file)))))

;;;###autoload
(defun org-roam-semantic-status ()
  "Show status of vector embeddings in the knowledge base."
  (interactive)
  (let* ((all-files (org-roam-list-files))
         (total-notes (length all-files))
         (notes-with-embeddings 0)
         (total-chunks 0)
         (chunks-with-embeddings 0)
         (notes-without-embeddings '())
         (embedding-sizes '()))

    (dolist (file all-files)
      ;; Count chunks and their embeddings
      (let ((all-embeddings (org-roam-semantic--get-all-embeddings file)))
        (let ((chunk-count (length (org-roam-semantic--parse-chunks file))))
          (setq total-chunks (+ total-chunks chunk-count))
          (setq chunks-with-embeddings (+ chunks-with-embeddings (length all-embeddings)))
          (when (> chunk-count 0)
            (cl-incf notes-with-embeddings))
          (dolist (chunk-embedding all-embeddings)
            (push (length (nth 2 chunk-embedding)) embedding-sizes)))))

    (let ((coverage (if (> total-notes 0)
                       (/ (* 100.0 notes-with-embeddings) total-notes)
                     0))
          (chunk-coverage (if (> total-chunks 0)
                             (/ (* 100.0 chunks-with-embeddings) total-chunks)
                           0))
          (unique-sizes (seq-uniq embedding-sizes)))

      (message "Vector Search Status: %d/%d files with chunks, %d/%d total chunks (%.1f%%) have embeddings. Sizes: %s"
               notes-with-embeddings total-notes
               chunks-with-embeddings total-chunks chunk-coverage unique-sizes)
      (with-current-buffer (get-buffer-create "*Embedding Status*")
        (erase-buffer)
        (insert (format "Chunk Embedding Coverage: %d/%d files have chunks, %d/%d chunks (%.1f%%) embedded\n"
                       notes-with-embeddings total-notes
                       chunks-with-embeddings total-chunks chunk-coverage))
        (insert (format "Embedding dimensions found: %s\n\n" unique-sizes))
        (display-buffer (current-buffer))))))

;;;;;; Minimal org → Markdown for n8n (ox-md), safe for files with leading drawers

(require 'org)
(require 'ox-md)

(defun org-roam-semantic--org-md-wrap-if-needed ()
  "If buffer has no headlines or starts with a :PROPERTIES: drawer,
wrap contents under a synthetic top-level heading using #+title or filename."
  (save-excursion
    (goto-char (point-min))
    (let* ((has-heading (save-excursion (re-search-forward org-heading-regexp nil t)))
           (starts-with-drawer (looking-at-p "\\`\\s-*:PROPERTIES:\\s-*\n")))
      (when (or (not has-heading) starts-with-drawer)
        (let* ((title (or (cadr (assoc "TITLE" (org-collect-keywords '("title"))))
                          (file-name-base (or buffer-file-name "note"))))
               (content (buffer-string)))
          (erase-buffer)
          (insert "* " title "\n\n" content))))))

(defun org-roam-semantic--org-strip-property-drawers ()
  "Remove all :PROPERTIES:…:END: drawers anywhere in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((rx (rx line-start (* blank) ":PROPERTIES:" (* blank) "\n"
                  (*? anything)
                  line-start (* blank) ":END:" (* blank) "\n")))
      (while (re-search-forward rx nil t)
        (replace-match "" t t)))))

(defun org-roam-semantic--with-org-file (org-file thunk)
  "Open ORG-FILE into a temp org buffer and run THUNK there."
  (unless (and org-file (file-readable-p org-file))
    (error "File not found or unreadable: %s" org-file))
  (with-temp-buffer
    (insert-file-contents org-file)
    (let ((default-directory (file-name-directory org-file)))
      (delay-mode-hooks (org-mode)))
    (org-roam-semantic--org-md-wrap-if-needed)
    (org-roam-semantic--org-strip-property-drawers)
    (let ((org-export-use-babel nil)
          (org-confirm-babel-evaluate nil)
          (org-export-with-broken-links 'mark)
          (org-export-with-toc nil)
          (org-export-with-section-numbers nil)
          (org-export-with-author nil)
          (org-export-with-creator nil)
          (org-export-with-email nil)
          (org-export-with-date nil)
          ;; also tell Org not to export any drawers (LOGBOOK, etc.)
          (org-export-with-drawers nil))
      (funcall thunk))))

(defun org-roam-semantic--export-md-string (org-file)
  "Export ORG-FILE to Markdown and return it as a Lisp string."
  (org-roam-semantic--with-org-file
   org-file
   (lambda ()
     ;; (org-export-as BACKEND SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)
     (org-export-as 'md nil nil t '(:explicit-links t)))))

(defun org-roam-semantic--temp-md-path (org-file)
  "Deterministic-but-unique temp filename for ORG-FILE."
  (let* ((abs (expand-file-name org-file))
         (mtime (or (nth 5 (file-attributes abs)) (current-time)))
         (sig  (secure-hash 'sha1 (format "%s::%s" abs mtime))))
    (expand-file-name (format "orgmd-%s.md" sig) temporary-file-directory)))

(defun org-roam-semantic--export-md-tempfile (org-file)
  "Export ORG-FILE to a deterministic temp file and return the path."
  (let* ((out (org-roam-semantic--temp-md-path org-file))
         (md  (org-roam-semantic--export-md-string org-file)))
    (with-temp-file out (insert md))
    out))

(defun org-roam-semantic--export-md-read-delete (org-file)
  "Export ORG-FILE, read result, delete temp, return Markdown as Lisp string."
  (let* ((path (org-roam-semantic--export-md-tempfile org-file))
         (contents (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
    (ignore-errors (delete-file path))
    contents))

;;; Auto-embedding hook
(defun org-roam-semantic--update-on-save ()
  "Update embeddings for current file on save."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (org-roam-file-p))
    ;; Always use chunking for optimal semantic search
    (org-roam-semantic-generate-chunks-for-file (buffer-file-name))))

;; Add the hook
(add-hook 'before-save-hook 'org-roam-semantic--update-on-save)

;;; Key Bindings for Vector Search

(global-set-key (kbd "C-c v s") 'org-roam-semantic-search)
(global-set-key (kbd "C-c v i") 'org-roam-semantic-insert-similar)
(global-set-key (kbd "C-c v r") 'org-roam-semantic-insert-related)

;; Chunk-level search bindings
(global-set-key (kbd "C-c v c") 'org-roam-semantic-search-chunks)
(global-set-key (kbd "C-c v g") 'org-roam-semantic-generate-chunks-for-file)
(global-set-key (kbd "C-c v G") 'org-roam-semantic-generate-all-chunks)

(provide 'org-roam-vector-search)

;;; org-roam-vector-search.el ends here
