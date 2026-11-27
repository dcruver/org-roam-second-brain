;;; orsb-search.el --- Semantic search with stale detection and an index -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Don Cruver

;; Author: Don Cruver
;; Version: 3.2.0
;; Package-Requires: ((emacs "29.1") (org-roam "2.2"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Semantic search: the call to an OpenAI-compatible /embeddings endpoint,
;; the chunker that splits a note at its headings, and on top of them:
;;
;; - A cache beside the notes.  Vectors live in the SQLite file managed by
;;   `orsb-embed-cache', keyed by the org-roam :ID: of the heading they
;;   were made from.  Notes hold only text: re-embedding never rewrites a
;;   .org file, so Syncthing has nothing to push and nothing to conflict.
;;   The only note change this module still makes is giving a heading an
;;   :ID: the first time it is embedded, so its vector has a key.
;; - Stale detection.  Each row records the sha1 of the normalized chunk
;;   text it was made from.  A chunk is regenerated only when its hash
;;   changed, or when there is no row for the configured model.
;; - An index.  Query time reads an in-memory table of node id -> vector,
;;   loaded from the cache once, and org-roam's database for where each
;;   node sits.  No note is read or parsed on the query path.
;; - A queue.  Writes (MCP tools, saves) enqueue the file; a worker runs when
;;   Emacs is idle and embeds one file at a time.  Nothing on the request path
;;   or in a save hook waits for the embedding service.
;;
;; Vaults whose notes still carry :EMBEDDING: properties move over with
;; `orsb-search-migrate-properties' (copies them into the cache, touches no
;; note) and later, as a separate opt-in step,
;; `orsb-search-strip-properties' (removes them from the notes).

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-roam)
(require 'json)
(require 'url)
(require 'cl-lib)
(require 'orsb-embed-cache)
(require 'orsb-schema)
(require 'seq)
(require 'subr-x)

(defgroup orsb-search nil
  "org-roam-second-brain semantic search."
  :group 'orsb
  :prefix "orsb-search-")

(make-obsolete-variable 'orsb-search-cache-file 'orsb-embed-cache-file "2.1.0")

(defcustom orsb-search-idle-delay 2
  "Seconds of idle time before the embedding worker takes the next queued file."
  :type 'number
  :group 'orsb-search)

(defcustom orsb-search-hash-property "EMBEDDING_HASH"
  "Property that recorded which text an in-note :EMBEDDING: was computed from.
Only read by the migration from in-note vectors, and removed by
`orsb-search-strip-properties'."
  :type 'string
  :group 'orsb-search)

;;;; The embeddings endpoint

(define-obsolete-variable-alias 'org-roam-semantic-embedding-url 'orsb-embedding-url "3.2.0")
(define-obsolete-variable-alias 'org-roam-semantic-embedding-model 'orsb-embedding-model "3.2.0")
(define-obsolete-variable-alias 'org-roam-semantic-min-chunk-size 'orsb-search-min-chunk-words "3.2.0")
(define-obsolete-variable-alias 'org-roam-semantic-similarity-cutoff 'orsb-search-similarity-cutoff "3.2.0")

(defcustom orsb-embedding-url "http://localhost:11434/v1"
  "Base URL of an OpenAI-compatible embeddings service.
\"/embeddings\" is appended to it.  The default is Ollama on this machine."
  :type 'string
  :group 'orsb-search)

(defcustom orsb-embedding-model "nomic-embed-text"
  "The embedding model to ask the service for.
Changing it makes every stored vector stale for the new model."
  :type 'string
  :group 'orsb-search)

(defcustom orsb-search-min-chunk-words 100
  "Words a section needs before it gets an embedding of its own."
  :type 'integer
  :group 'orsb-search)

(defcustom orsb-search-similarity-cutoff 0.55
  "Lowest similarity `orsb-search-insert-similar' still inserts a link for."
  :type 'float
  :group 'orsb-search)

;;;; Primitives

(defun orsb-search--normalize (text)
  "Normalize text for embedding by removing extra whitespace and formatting."
  (when text
    (let ((normalized (replace-regexp-in-string "[ \t\n\r]+" " " text)))
      (string-trim normalized))))

(defun orsb-search--cosine (vec1 vec2)
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

(defun orsb-search--count-words (text)
  "Count words in TEXT."
  (length (split-string (orsb-search--normalize text) "\\s-+" t)))

(defun orsb-search--parse-chunks (file)
  "Parse FILE and return list of chunks with metadata.
Returns list of (position heading-text content word-count level)."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((chunks '()))
      (goto-char (point-min))
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
                     (word-count (orsb-search--count-words full-content)))

                ;; Include all chunks - mark those below threshold differently
                (if (>= word-count orsb-search-min-chunk-words)
                    (push (list heading-pos heading-text full-content word-count level :embedding) chunks)
                  (push (list heading-pos heading-text full-content word-count level :id-only) chunks)))))))
      (nreverse chunks))))

(defun orsb-search-embed (text)
  "Call OpenAI-compatible embeddings API synchronously.
Works with Infinity, vLLM, and other OpenAI-compatible embedding services."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string
                           (json-encode `((model . ,orsb-embedding-model)
                                        (input . ,text)))
                           'utf-8))
        (url (concat orsb-embedding-url "/embeddings")))
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

(defun orsb-search--note-text (file)
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

      (orsb-search--normalize
       (if title
           (concat title ". " content)
         content)))))

(defun orsb-search--file-title (file)
  "Extract the title from an org-roam note file."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
            (string-trim (match-string 1))
          (file-name-sans-extension (file-name-nondirectory file))))
    (error (file-name-sans-extension (file-name-nondirectory file)))))

(defun orsb-search--file-id (file)
  "Extract the node ID from an org-roam note file."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^:ID:\\s-*\\(.+\\)$" nil t)
          (string-trim (match-string 1))))
    (error nil)))

(defun orsb-search--model ()
  "The embedding model rows are written and read for."
  orsb-embedding-model)

;;;; Chunks on disk

(defun orsb-search--chunk-hash (text)
  "The stale-detection hash of chunk TEXT."
  (sha1 (or (orsb-search--normalize text) "")))

(defun orsb-search--parse-vector (string)
  "Parse a space-separated float STRING into a list, or nil."
  (condition-case nil
      (let ((v (mapcar #'string-to-number (split-string string nil t))))
        (and v v))
    (error nil)))

(defun orsb-search--drawer-id (end)
  "The :ID: in the property drawer from point to END, or nil."
  (save-excursion
    (and (re-search-forward "^[ \t]*:ID:[ \t]*\\(\\S-+\\)" end t)
         (match-string-no-properties 1))))

(defun orsb-search--read-stored (file)
  "Return the vectors FILE still carries as :EMBEDDING: properties.
A list of plists, each with :position (heading start, or 1 for the file
drawer), :heading, :id (the drawer's :ID: or nil), :embedding (a list of
floats or nil) and :hash (the :EMBEDDING_HASH: or nil).  Only the
migration and the stripper read these; search reads the cache."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((out nil)
          (hash-rx (format "^[ \t]*:%s:[ \t]*\\(\\S-+\\)" (regexp-quote orsb-search-hash-property))))
      ;; file-level drawer
      (goto-char (point-min))
      (when (looking-at "^[ \t]*:PROPERTIES:")
        (let ((end (save-excursion (re-search-forward "^[ \t]*:END:" nil t))))
          (when end
            (let ((emb (save-excursion (and (re-search-forward "^[ \t]*:EMBEDDING:[ \t]*\\(.*\\)$" end t) (match-string 1))))
                  (hash (save-excursion (and (re-search-forward hash-rx end t) (match-string 1)))))
              (when (or emb hash)
                (push (list :position 1 :heading nil
                            :id (orsb-search--drawer-id end)
                            :embedding (and emb (orsb-search--parse-vector emb))
                            :hash hash)
                      out))))))
      ;; headings
      (goto-char (point-min))
      (while (re-search-forward "^\\*+[ \t]+\\(.*\\)$" nil t)
        (let ((pos (match-beginning 0))
              (heading (match-string 1)))
          (forward-line 1)
          (when (looking-at "^[ \t]*:PROPERTIES:")
            (let ((end (save-excursion (re-search-forward "^[ \t]*:END:" nil t))))
              (when end
                (let ((emb (save-excursion (and (re-search-forward "^[ \t]*:EMBEDDING:[ \t]*\\(.*\\)$" end t) (match-string 1))))
                      (hash (save-excursion (and (re-search-forward hash-rx end t) (match-string 1)))))
                  (when (or emb hash)
                    (push (list :position pos :heading heading
                                :id (orsb-search--drawer-id end)
                                :embedding (and emb (orsb-search--parse-vector emb))
                                :hash hash)
                          out))))))))
      (nreverse out))))

(defun orsb-search--strip-drawers (text)
  "TEXT without property drawers, so drawer contents do not alter the hash.
Not anchored to a line start: the chunk parser glues the heading and its
body with \". \", so the drawer can follow the heading on one line."
  (replace-regexp-in-string
   "[ \t]*:PROPERTIES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" " " text))

(defun orsb-search--heading-ids (file positions)
  "The :ID: of the heading at each of POSITIONS in FILE (nil where none)."
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar (lambda (pos)
              (goto-char pos)
              (forward-line 1)
              (when (looking-at "^[ \t]*:PROPERTIES:")
                (let ((end (save-excursion (re-search-forward "^[ \t]*:END:" nil t))))
                  (and end (orsb-search--drawer-id end)))))
            positions)))

(defun orsb-search--current-chunks (file)
  "Chunks FILE has now: list of (POSITION HEADING TEXT HASH EMBED-P ID).
TEXT is the heading plus its body with drawers removed; it is what gets
embedded and hashed, and its word count (not the parser's, which counts
drawer lines) decides whether the chunk is embedded or only gets an id.
ID is the heading's :ID:, the chunk's key in the cache, or nil."
  (let* ((parsed (let ((inhibit-message t) (message-log-max nil))
                   (orsb-search--parse-chunks file)))
         (ids (orsb-search--heading-ids file (mapcar #'car parsed))))
    (cl-mapcar (lambda (c id)
                 (let ((text (orsb-search--strip-drawers (nth 2 c))))
                   (list (nth 0 c) (nth 1 c) text
                         (orsb-search--chunk-hash text)
                         (>= (orsb-search--count-words text)
                             orsb-search-min-chunk-words)
                         id)))
               parsed ids)))

(defun orsb-search--stale-chunks (chunks)
  "The embeddable CHUNKS whose cache row is missing or outdated."
  (let ((stored (orsb-embed-cache-hashes (delq nil (mapcar (lambda (c) (nth 5 c)) chunks))
                                         (orsb-search--model))))
    (seq-filter (lambda (c)
                  (and (nth 4 c)
                       (or (null (nth 5 c))
                           (not (equal (gethash (nth 5 c) stored) (nth 3 c))))))
                chunks)))

(defun orsb-search-file-stale-p (file)
  "Non-nil when FILE has a chunk whose embedding is missing or outdated."
  (and (orsb-search--stale-chunks (orsb-search--current-chunks file)) t))

;;;; Generation

(defvar orsb-search--inhibit-hooks nil
  "Non-nil while the worker saves a file, so the save hook does not re-queue it.")

(defun orsb-search--save (file)
  "Write FILE's buffer to disk without running save hooks.
Returns non-nil when something was written."
  (with-current-buffer (find-file-noselect file)
    (when (buffer-modified-p)
      (let ((orsb-search--inhibit-hooks t)
            (inhibit-read-only t))
        (write-region (point-min) (point-max) file nil :silent)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil)
        t))))

(defun orsb-search--ensure-ids (file chunks)
  "Give every chunk in CHUNKS of FILE that lacks an :ID: one, and save.
Works on positions from the bottom up, so earlier positions stay valid.
Skips the file when a buffer visiting it has unsaved edits (the positions
would not match); the save hook queues it again once those are saved.
Returns non-nil when the file was written."
  (let ((missing (sort (mapcar #'car (seq-remove (lambda (c) (nth 5 c)) chunks)) #'>)))
    (when missing
      (let ((buf (find-buffer-visiting file)))
        (unless (and buf (buffer-modified-p buf))
          (with-current-buffer (or buf (find-file-noselect file))
            (unless (verify-visited-file-modtime)
              (revert-buffer t t t))
            (org-with-wide-buffer
             (dolist (pos missing)
               (goto-char pos)
               (when (org-at-heading-p)
                 (unless (org-get-property-block) (org-insert-property-drawer))
                 (unless (org-entry-get (point) "ID")
                   (org-entry-put (point) "ID" (org-id-new)))))))
          (when (orsb-search--save file)
            ;; new ids are new nodes, and every later heading moved
            (ignore-errors (org-roam-db-update-file file))
            t))))))

(defun orsb-search--not-a-note-p (file)
  "Non-nil when FILE is a schema file: rules about notes, not a note.
Embedding it would give its headings ids, and so make them notes."
  (equal (file-name-nondirectory file) orsb-schema-file-name))

(defun orsb-search-generate-file (file &optional force)
  "Embed the chunks of FILE whose text changed (all of them with FORCE).
Vectors go to the cache; the note itself changes only when a heading
first needs an :ID:.  A schema file is left alone.
Returns (GENERATED . SKIPPED)."
  (let ((chunks (unless (orsb-search--not-a-note-p file)
                  (orsb-search--current-chunks file))))
    (when (orsb-search--ensure-ids file chunks)
      (setq chunks (orsb-search--current-chunks file)))
    (let* ((model (orsb-search--model))
           (todo (if force (seq-filter (lambda (c) (nth 4 c)) chunks) (orsb-search--stale-chunks chunks)))
           (generated 0)
           (skipped (- (seq-count (lambda (c) (nth 4 c)) chunks) (length todo))))
      (dolist (c todo)
        (pcase-let ((`(,_pos ,heading ,text ,hash ,_embed-p ,id) c))
          (if (null id)
              (message "orsb-search: %S in %s has no :ID: yet, not embedded" heading (file-name-nondirectory file))
            (let ((embedding (orsb-search-embed text)))
              (if embedding
                  (progn (orsb-embed-cache-put id embedding hash model)
                         (orsb-search--index-put id embedding)
                         (setq generated (1+ generated)))
                (message "orsb-search: no embedding for %S in %s" heading (file-name-nondirectory file)))))))
      (cons generated skipped))))

(defun orsb-search-generate-all (&optional force)
  "Run `orsb-search-generate-file' over every note now (FORCE: re-embed all).
Returns (GENERATED . SKIPPED) summed over the vault."
  (interactive "P")
  (let ((generated 0) (skipped 0))
    (dolist (file (org-roam-list-files))
      (condition-case err
          (let ((r (orsb-search-generate-file file force)))
            (setq generated (+ generated (car r)) skipped (+ skipped (cdr r))))
        (error (message "orsb-search: %s: %s" (file-name-nondirectory file) (error-message-string err)))))
    (when (called-interactively-p 'any)
      (message "orsb-search: %d embedded, %d already current" generated skipped))
    (cons generated skipped)))

(defun orsb-search-backfill-hashes (&optional _files)
  "Obsolete: hashes now live in the cache with each vector.
Kept so old callers do not break; writes nothing and returns 0.  To bring
in-note vectors (with or without a hash) into the cache, use
`orsb-search-migrate-properties'."
  (interactive)
  0)
(make-obsolete 'orsb-search-backfill-hashes 'orsb-search-migrate-properties "2.1.0")

;;;; Migration from in-note vectors

(defun orsb-search-migrate-properties (&optional files)
  "Copy :EMBEDDING: vectors still in FILES (default: every note) into the cache.
Reads the notes and writes nothing to them.  Each vector is stored under
its drawer's :ID: for the configured model, with its :EMBEDDING_HASH:,
or, when it has none, the hash of the chunk's current text (the same
assumption the old hash backfill made).  A row already in the cache wins,
so running this twice is harmless.  Vectors in a drawer without an :ID:
cannot be keyed and are left for the worker to re-embed.

Returns a plist: :files (notes that had vectors), :copied, :present
\(already in the cache) and :no-id."
  (interactive)
  (let ((model (orsb-search--model))
        (nfiles 0) (copied 0) (present 0) (no-id 0))
    (with-sqlite-transaction (orsb-embed-cache-db)
      (dolist (file (or files (org-roam-list-files)))
        (let ((stored (seq-filter (lambda (s) (plist-get s :embedding)) (orsb-search--read-stored file))))
          (when stored
            (setq nfiles (1+ nfiles))
            (let ((current nil))
              (dolist (s stored)
                (let ((id (plist-get s :id)))
                  (if (null id)
                      (setq no-id (1+ no-id))
                    (let ((hash (or (plist-get s :hash)
                                    (progn
                                      (unless current (setq current (orsb-search--current-chunks file)))
                                      (nth 3 (seq-find (lambda (c) (equal (nth 5 c) id)) current))))))
                      (if (orsb-embed-cache-put-new id (plist-get s :embedding) hash model)
                          (setq copied (1+ copied))
                        (setq present (1+ present))))))))))))
    (orsb-search-index-reset)
    (let ((result (list :files nfiles :copied copied :present present :no-id no-id)))
      (when (called-interactively-p 'any)
        (message "orsb-search: copied %d vectors from %d notes into %s (%d already there, %d without an :ID:)"
                 copied nfiles orsb-embed-cache-file present no-id))
      result)))

(defun orsb-search-strip-properties (&optional files dry-run)
  "Remove :EMBEDDING: and :EMBEDDING_HASH: from FILES (default: every note).
The separate, opt-in second step after `orsb-search-migrate-properties'.
A drawer's vector is removed only when the cache holds a row for that
drawer's :ID: (for any model), so nothing is dropped that the cache
lacks.  Notes open with unsaved edits are skipped.  With DRY-RUN (the
prefix argument interactively) nothing is written; the counts say what
would happen.

Returns a plist: :files (notes changed, or that would be), :removed
\(vectors), :kept (vectors with no cache row or no :ID:) and :skipped
\(notes with unsaved edits)."
  (interactive (list nil current-prefix-arg))
  (when (and (called-interactively-p 'any) (not dry-run)
             (not (yes-or-no-p (format "Remove in-note embeddings that %s already holds? "
                                       (abbreviate-file-name orsb-embed-cache-file)))))
    (user-error "Nothing changed"))
  (let ((nfiles 0) (removed 0) (kept 0) (skipped 0))
    (dolist (file (or files (org-roam-list-files)))
      (let* ((stored (orsb-search--read-stored file))
             (doomed (seq-filter (lambda (s) (and (plist-get s :id)
                                                  (orsb-embed-cache-has-p (plist-get s :id) t)))
                                 stored)))
        (setq kept (+ kept (- (length stored) (length doomed))))
        (when doomed
          (let ((buf (find-buffer-visiting file)))
            (if (and buf (buffer-modified-p buf))
                (setq skipped (1+ skipped)
                      kept (+ kept (length doomed)))
              (setq nfiles (1+ nfiles)
                    removed (+ removed (length doomed)))
              (unless dry-run
                (with-current-buffer (or buf (find-file-noselect file))
                  (unless (verify-visited-file-modtime)
                    (revert-buffer t t t))
                  (org-with-wide-buffer
                   (dolist (s doomed)
                     (let ((pos (if (eql (plist-get s :position) 1)
                                    (point-min)
                                  (org-find-property "ID" (plist-get s :id)))))
                       (when pos
                         (org-entry-delete pos "EMBEDDING")
                         (org-entry-delete pos orsb-search-hash-property))))))
                (when (orsb-search--save file)
                  (ignore-errors (org-roam-db-update-file file)))))))))
    (let ((result (list :files nfiles :removed removed :kept kept :skipped skipped)))
      (when (called-interactively-p 'any)
        (message "orsb-search: %s %d vectors from %d notes (%d kept, %d notes skipped for unsaved edits)"
                 (if dry-run "would remove" "removed") removed nfiles kept skipped))
      result)))

(defun orsb-search-prune-cache (&optional keep-other-models)
  "Drop cache rows for nodes org-roam no longer knows, and other models' rows.
With KEEP-OTHER-MODELS (prefix argument), rows from other models stay.
Returns the number of rows removed."
  (interactive "P")
  (let ((n (orsb-embed-cache-prune (mapcar #'car (org-roam-db-query [:select [id] :from nodes]))
                                   keep-other-models)))
    (orsb-search-index-reset)
    (when (called-interactively-p 'any) (message "orsb-search: removed %d cache rows" n))
    n))

;;;; Queue and worker

(defvar orsb-search--queue nil "Files waiting for the embedding worker.")
(defvar orsb-search--timer nil "The idle timer driving the worker.")

(defun orsb-search-enqueue (file)
  "Queue FILE for (re)embedding when Emacs is idle."
  (when (and file (not orsb-search--inhibit-hooks) (org-roam-file-p file)
             (not (orsb-search--not-a-note-p file)))
    (let ((file (file-truename file)))
      (unless (member file orsb-search--queue)
        (setq orsb-search--queue (append orsb-search--queue (list file))))
      (unless orsb-search--timer
        (setq orsb-search--timer
              (run-with-idle-timer orsb-search-idle-delay t #'orsb-search--work))))
    t))

(defun orsb-search--work ()
  "Embed one queued file; stop the timer when the queue is empty."
  (if-let ((file (pop orsb-search--queue)))
      (condition-case err
          (when (file-exists-p file)
            (orsb-search-generate-file file))
        (error (message "orsb-search: %s: %s" (file-name-nondirectory file) (error-message-string err))))
    (when orsb-search--timer
      (cancel-timer orsb-search--timer)
      (setq orsb-search--timer nil))))

(defun orsb-search-drain ()
  "Process the whole queue now (tests and one-off maintenance)."
  (while orsb-search--queue (orsb-search--work))
  (orsb-search--work))

(defun orsb-search--after-save ()
  "Save hook: queue the saved note instead of embedding it synchronously."
  (when (and (derived-mode-p 'org-mode) (buffer-file-name) (org-roam-file-p))
    (orsb-search-enqueue (buffer-file-name))))

(add-hook 'after-save-hook #'orsb-search--after-save)

;;;; Index

(defvar orsb-search--index (make-hash-table :test 'equal)
  "Node id -> vector for the model in `orsb-search--index-model'.")
(defvar orsb-search--index-loaded nil "Non-nil once the index holds the cache.")
(defvar orsb-search--index-model nil "Model the index was loaded for.")

(defun orsb-search--index-load ()
  "Load every vector of the configured model from the cache, once."
  (unless (and orsb-search--index-loaded (equal orsb-search--index-model (orsb-search--model)))
    (clrhash orsb-search--index)
    (orsb-embed-cache-map (lambda (id vec) (puthash id vec orsb-search--index))
                          (orsb-search--model))
    (setq orsb-search--index-loaded t
          orsb-search--index-model (orsb-search--model))))

(defun orsb-search--index-put (id vector)
  "Record ID's new VECTOR in a loaded index."
  (when (and orsb-search--index-loaded (equal orsb-search--index-model (orsb-search--model)))
    (puthash id vector orsb-search--index)))

(defun orsb-search-index-reset ()
  "Drop the in-memory index; the next query reloads it from the cache."
  (interactive)
  (clrhash orsb-search--index)
  (setq orsb-search--index-loaded nil orsb-search--index-model nil))

(defun orsb-search--nodes (&optional file)
  "Org-roam nodes as (ID FILE POS LEVEL TITLE), only FILE's when given."
  (if file
      (mapcar (lambda (r) (cons (car r) (cons file (cdr r))))
              (org-roam-db-query [:select [id pos level title] :from nodes :where (= file $s1)]
                                 (expand-file-name file)))
    (org-roam-db-query [:select [id file pos level title] :from nodes])))

(defun orsb-search--node-chunk (node)
  "NODE as (POSITION HEADING VECTOR), or nil when it has no vector.
POSITION 1 and HEADING nil mark the file-level node."
  (when-let ((vec (gethash (nth 0 node) orsb-search--index)))
    (if (zerop (nth 3 node))
        (list 1 nil vec)
      (list (nth 2 node) (nth 4 node) vec))))

(defun orsb-search--file-chunks (file)
  "(POSITION HEADING VECTOR) triples for FILE's nodes that have a vector."
  (orsb-search--index-load)
  (delq nil (mapcar #'orsb-search--node-chunk (orsb-search--nodes file))))

;;;; Query

(defun orsb-search-similar-vector (qv &optional limit cutoff exclude)
  "Chunks most similar to vector QV, best first.
At most LIMIT hits, each with score >= CUTOFF.  EXCLUDE is a list of
files whose chunks are skipped.  Each hit is (FILE SCORE POSITION HEADING)."
  (orsb-search--index-load)
  (let ((limit (or limit 10))
        (cutoff (or cutoff 0.0))
        (skip (mapcar #'file-truename exclude))
        (hits nil))
    (dolist (node (orsb-search--nodes))
      (when-let ((c (orsb-search--node-chunk node)))
        (unless (and skip (member (file-truename (nth 1 node)) skip))
          (let ((score (orsb-search--cosine qv (nth 2 c))))
            (when (and score (>= score cutoff))
              (push (list (nth 1 node) score (nth 0 c) (nth 1 c)) hits))))))
    (seq-take (sort hits (lambda (a b) (> (cadr a) (cadr b)))) limit)))

(defun orsb-search-similar (query &optional limit cutoff)
  "Chunks most similar to QUERY, best first, at most LIMIT with score >= CUTOFF.
Each hit is (FILE SCORE POSITION HEADING).
Signals an error when the embedding service is unreachable."
  (let ((qv (orsb-search-embed query)))
    (unless qv (error "Embedding service returned nothing for the query"))
    (orsb-search-similar-vector qv limit cutoff)))

(defun orsb-search-centroid (file &optional start end)
  "The mean of FILE's stored chunk vectors, or nil when it has none.
With START and END, only chunks whose position lies in [START, END) count."
  (let ((vecs (delq nil (mapcar (lambda (c)
                                  (when (or (null start)
                                            (and (>= (nth 0 c) start) (< (nth 0 c) end)))
                                    (nth 2 c)))
                                (orsb-search--file-chunks file)))))
    (when vecs
      (let* ((n (float (length vecs)))
             (sum (make-vector (length (car vecs)) 0.0)))
        (dolist (v vecs)
          (seq-do-indexed (lambda (x i) (aset sum i (+ (aref sum i) x))) v))
        (mapcar (lambda (x) (/ x n)) sum)))))

(defun orsb-search-chunk-info (file position &optional max-chars query)
  "What the chunk at POSITION in FILE says: (:heading :id :snippet).
POSITION 1 is the file-level chunk; any position on a heading's line
means that heading.  The snippet is the chunk's text
with drawers and planning removed, at most MAX-CHARS (default 240); when
QUERY's first word occurs in it, the snippet is taken around it."
  (let ((max-chars (or max-chars 240)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (min position (point-max)))
      ;; positions come from org-roam's database, which may point inside
      ;; the heading line rather than at its start
      (beginning-of-line)
      (let (heading id text)
        (if (looking-at "^\\*+[ \t]+\\(.*\\)$")
            (progn
              (setq heading (string-trim
                             (replace-regexp-in-string
                              "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" ""
                              (match-string-no-properties 1))))
              (forward-line 1))
          (goto-char (point-min)))
        (let* ((start (point))
               (end (save-excursion (if (re-search-forward "^\\*+[ \t]" nil t) (match-beginning 0) (point-max))))
               (raw (buffer-substring-no-properties start end)))
          (when (string-match "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" raw)
            (setq id (match-string 1 raw)))
          (setq text (string-trim
                      (replace-regexp-in-string
                       "[ \t\n]+" " "
                       (replace-regexp-in-string
                        "^[ \t]*\\(?:#\\+[A-Za-z_]+:.*\\|\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):.*\\)$" ""
                        (replace-regexp-in-string
                         "^[ \t]*:\\([A-Za-z_]+\\):[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*$" "" raw))))))
        (when (and query (> (length text) max-chars))
          (let* ((word (car (split-string (downcase query) "[^[:alnum:]]+" t)))
                 (at (and word (string-search word (downcase text)))))
            (when (and at (> at (/ max-chars 3)))
              (setq text (concat "…" (substring text (max 0 (- at (/ max-chars 3)))))))))
        (list :heading heading :id id
              :snippet (if (> (length text) max-chars)
                           (concat (substring text 0 max-chars) "…")
                         text))))))

(defun orsb-search-index-stats ()
  "Return (FILES-INDEXED . CHUNKS-INDEXED) for the loaded index."
  (orsb-search--index-load)
  (let ((files (make-hash-table :test 'equal)) (chunks 0))
    (dolist (node (orsb-search--nodes))
      (when (gethash (nth 0 node) orsb-search--index)
        (puthash (nth 1 node) t files)
        (setq chunks (1+ chunks))))
    (cons (hash-table-count files) chunks)))

;;;; Commands (C-c v, bound by `orsb-mode')

(defun orsb-search--similar-or-nil (query-text &optional limit cutoff)
  "`orsb-search-similar' for QUERY-TEXT with LIMIT and CUTOFF.
Returns nil, with a message, when the embedding service does not answer."
  (condition-case err
      (orsb-search-similar query-text (or limit 10) (if (numberp cutoff) cutoff 0.0))
    (error (message "orsb-search: %s" (error-message-string err)) nil)))

(defun orsb-search--insert-links (hits)
  "Insert one link line per file in HITS, best first."
  (let (seen)
    (dolist (hit hits)
      (let ((file (car hit)))
        (unless (member file seen)
          (push file seen)
          (let ((title (orsb-search--file-title file))
                (id (orsb-search--file-id file)))
            (insert (if id
                        (format "- [[id:%s][%s]] (%.3f)\n" id title (cadr hit))
                      (format "- [[file:%s][%s]] (%.3f)\n" file title (cadr hit))))))))
    (length seen)))

;;;###autoload
(defun orsb-search-notes (query)
  "Show the sections closest in meaning to QUERY, each a link to its note."
  (interactive "sSearch by meaning: ")
  (let ((hits (orsb-search--similar-or-nil query 10)))
    (if (null hits)
        (message "Nothing similar found")
      (with-current-buffer (get-buffer-create "*Similar Notes*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert (format "* Similar to: %s\n\n" query))
          (dolist (hit hits)
            (pcase-let ((`(,file ,score ,_pos ,heading) hit))
              (let ((title (orsb-search--file-title file))
                    (id (orsb-search--file-id file)))
                (insert (format "** %.3f  %s%s\n" score
                                (if id (format "[[id:%s][%s]]" id title) (format "[[file:%s][%s]]" file title))
                                (if (and heading (not (equal heading title))) (concat " > " heading) "")))))))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun orsb-search-insert-related (query)
  "Insert links to the notes closest in meaning to QUERY at point."
  (interactive "sFind notes related to: ")
  (unless (derived-mode-p 'org-mode) (user-error "Not an org buffer"))
  (let ((hits (orsb-search--similar-or-nil query 5)))
    (if (null hits)
        (message "Nothing similar found")
      (insert (format "\n** Related Notes - %s\n" query))
      (message "Inserted %d links" (orsb-search--insert-links hits))
      (insert "\n"))))

;;;###autoload
(defun orsb-search-insert-similar (&optional cutoff)
  "Insert links to the notes similar to the current one at point.
CUTOFF (the prefix argument) or `orsb-search-similarity-cutoff' is the
lowest similarity still inserted."
  (interactive "P")
  (unless (and (derived-mode-p 'org-mode) (buffer-file-name) (org-roam-file-p))
    (user-error "Not an org-roam note"))
  (let* ((file (buffer-file-name))
         (cutoff (if (numberp cutoff) cutoff orsb-search-similarity-cutoff))
         (hits (seq-remove (lambda (hit) (file-equal-p (car hit) file))
                           (orsb-search--similar-or-nil (orsb-search--note-text file) 50 cutoff))))
    (if (null hits)
        (message "No notes at similarity %.2f or above" cutoff)
      (insert (format "\n** Related Notes (similarity >= %.2f)\n" cutoff))
      (message "Inserted %d links" (orsb-search--insert-links hits))
      (insert "\n"))))

;;;###autoload
(defun orsb-search-generate-buffer ()
  "Embed the sections of the current note whose text changed, now."
  (interactive)
  (unless (buffer-file-name) (user-error "This buffer visits no file"))
  (let ((r (orsb-search-generate-file (buffer-file-name))))
    (message "orsb-search: %d embedded, %d already current" (car r) (cdr r))))

(provide 'orsb-search)
;;; orsb-search.el ends here
