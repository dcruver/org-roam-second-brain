;;; orsb-embed-cache.el --- SQLite store for note embeddings -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 2.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:

;; Embedding vectors live in one SQLite file beside the notes instead of
;; inside them.  By default that is
;; ~/.cache/org-roam-second-brain/embeddings.db, which is outside any
;; Syncthing folder: re-embedding a note no longer rewrites the .org file,
;; so nothing ripples to other devices and nothing turns into a conflict
;; copy.
;;
;; One row per org-roam node and embedding model:
;;
;;   node_id       the :ID: of the heading (or file) the vector was made from
;;   model         the embedding model that made it
;;   content_hash  sha1 of the normalized chunk text it was made from
;;   dims          vector length
;;   vector        the floats, packed as little-endian float32
;;   updated_at    when it was written (ISO 8601, UTC)
;;
;; Because rows are keyed by model too, a model switch can re-embed into
;; new rows while the old ones keep serving until `orsb-embed-cache-prune'
;; removes them.  The file is a cache: delete it and the idle worker in
;; `orsb-search' rebuilds it from the notes.
;;
;; Uses the SQLite support built into Emacs 29 and later; no new package.

;;; Code:

(require 'subr-x)
(require 'sqlite)
(require 'seq)

(defvar org-roam-semantic-embedding-model)

(defgroup orsb-embed-cache nil
  "org-roam-second-brain embedding cache."
  :group 'orsb
  :prefix "orsb-embed-cache-")

(defcustom orsb-embed-cache-file
  (expand-file-name "org-roam-second-brain/embeddings.db"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "SQLite file that holds note embeddings.
Keep it outside any synced folder: it belongs to one machine and can be
rebuilt from the notes at any time."
  :type 'file
  :group 'orsb-embed-cache)

(defconst orsb-embed-cache-schema-version 1
  "Schema version this code reads and writes.")

(defvar orsb-embed-cache--db nil "Open SQLite handle, or nil.")
(defvar orsb-embed-cache--db-file nil "File `orsb-embed-cache--db' is open on.")

;;;; Connection

(defun orsb-embed-cache-close ()
  "Close the cache database if it is open."
  (when orsb-embed-cache--db
    (ignore-errors (sqlite-close orsb-embed-cache--db)))
  (setq orsb-embed-cache--db nil
        orsb-embed-cache--db-file nil))

(defun orsb-embed-cache--init (db)
  "Create the schema in DB if needed and check its version."
  (sqlite-pragma db "busy_timeout = 5000")
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS embeddings (
  node_id      TEXT    NOT NULL,
  model        TEXT    NOT NULL,
  content_hash TEXT    NOT NULL,
  dims         INTEGER NOT NULL,
  vector       BLOB    NOT NULL,
  updated_at   TEXT    NOT NULL,
  PRIMARY KEY (node_id, model))")
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)")
  (sqlite-execute db "INSERT OR IGNORE INTO meta (key, value) VALUES ('schema_version', ?)"
                  (list (number-to-string orsb-embed-cache-schema-version)))
  (let ((v (caar (sqlite-select db "SELECT value FROM meta WHERE key = 'schema_version'"))))
    (unless (equal v (number-to-string orsb-embed-cache-schema-version))
      (error "orsb-embed-cache: %s has schema version %s, this code knows %d"
             orsb-embed-cache--db-file v orsb-embed-cache-schema-version))))

(defun orsb-embed-cache-db ()
  "The open cache database, opening (and creating) it on first use."
  (unless (sqlite-available-p)
    (error "orsb-embed-cache: this Emacs has no SQLite support (Emacs 29+ built with sqlite3 is needed)"))
  (let ((file (expand-file-name orsb-embed-cache-file)))
    (unless (and orsb-embed-cache--db (equal file orsb-embed-cache--db-file))
      (orsb-embed-cache-close)
      (make-directory (file-name-directory file) t)
      (let ((db (sqlite-open file)))
        (setq orsb-embed-cache--db db
              orsb-embed-cache--db-file file)
        (condition-case err
            (orsb-embed-cache--init db)
          (error (orsb-embed-cache-close)
                 (signal (car err) (cdr err))))))
    orsb-embed-cache--db))

(defun orsb-embed-cache--model (model)
  "MODEL, or the configured embedding model when MODEL is nil."
  (or model
      (and (boundp 'org-roam-semantic-embedding-model) org-roam-semantic-embedding-model)
      "default"))

;;;; Packing

(defun orsb-embed-cache--float32-bits (x)
  "The IEEE 754 single-precision bit pattern of number X, as an integer.
Values below the float32 normal range are stored as (signed) zero."
  (let ((x (float x)))
    (when (or (isnan x) (= (abs x) 1.0e+INF))
      (error "orsb-embed-cache: cannot store non-finite value %s" x))
    (if (= x 0.0)
        0
      (let* ((sign (if (< x 0) 1 0))
             (fe (frexp (abs x)))       ; mantissa in [0.5, 1)
             (e (+ (cdr fe) 126))
             (m (round (* (- (* 2 (car fe)) 1.0) 8388608.0))))
        (when (= m 8388608) (setq m 0 e (1+ e)))
        (cond ((<= e 0) (ash sign 31))
              ((>= e 255) (error "orsb-embed-cache: %s is out of float32 range" x))
              (t (logior (ash sign 31) (ash e 23) m)))))))

(defun orsb-embed-cache-pack (vector)
  "Pack VECTOR (a list or vector of numbers) as little-endian float32s.
The result is a unibyte string marked so SQLite stores it as a BLOB."
  (let* ((n (length vector))
         (s (make-string (* 4 n) 0))
         (i 0))
    (seq-doseq (x vector)
      (let ((bits (orsb-embed-cache--float32-bits x)))
        (aset s i (logand bits 255))
        (aset s (+ i 1) (logand (ash bits -8) 255))
        (aset s (+ i 2) (logand (ash bits -16) 255))
        (aset s (+ i 3) (logand (ash bits -24) 255))
        (setq i (+ i 4))))
    (propertize (string-to-unibyte s) 'coding-system 'binary)))

(defun orsb-embed-cache-unpack (blob)
  "The list of floats packed in BLOB by `orsb-embed-cache-pack'."
  (let ((out nil)
        (i (- (length blob) 4)))
    (while (>= i 0)
      (let* ((bits (logior (aref blob i)
                           (ash (aref blob (+ i 1)) 8)
                           (ash (aref blob (+ i 2)) 16)
                           (ash (aref blob (+ i 3)) 24)))
             (e (logand (ash bits -23) 255))
             (m (logand bits #x7fffff))
             (sign (if (zerop (ash bits -31)) 1.0 -1.0)))
        (push (cond ((= e 0) (* sign 0.0))
                    ((= e 255) (* sign 1.0e+INF))
                    (t (* sign (ldexp (+ 1.0 (/ m 8388608.0)) (- e 127)))))
              out))
      (setq i (- i 4)))
    out))

(defun orsb-embed-cache--now ()
  "The current UTC time as ISO 8601."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

;;;; Reads

(defun orsb-embed-cache-get (node-id &optional model)
  "The row for NODE-ID and MODEL as a plist, or nil.
Keys: :hash, :dims, :vector (a list of floats) and :updated."
  (when-let ((row (car (sqlite-select (orsb-embed-cache-db)
                                      "SELECT content_hash, dims, vector, updated_at FROM embeddings WHERE node_id = ? AND model = ?"
                                      (list node-id (orsb-embed-cache--model model))))))
    (list :hash (nth 0 row) :dims (nth 1 row)
          :vector (orsb-embed-cache-unpack (nth 2 row))
          :updated (nth 3 row))))

(defun orsb-embed-cache-hashes (node-ids &optional model)
  "Hash table of NODE-IDS that have a row for MODEL -> their content hash.
Reads no vectors, so it is cheap enough for stale checks."
  (let ((table (make-hash-table :test 'equal))
        (model (orsb-embed-cache--model model))
        (ids (delete-dups (delq nil (copy-sequence node-ids)))))
    (while ids
      (let ((batch (seq-take ids 500)))
        (setq ids (nthcdr 500 ids))
        (dolist (row (sqlite-select (orsb-embed-cache-db)
                                    (format "SELECT node_id, content_hash FROM embeddings WHERE model = ? AND node_id IN (%s)"
                                            (mapconcat (lambda (_) "?") batch ","))
                                    (cons model batch)))
          (puthash (car row) (cadr row) table))))
    table))

(defun orsb-embed-cache-has-p (node-id &optional any-model)
  "Non-nil when NODE-ID has a row for the configured model.
With ANY-MODEL, a row for any model counts."
  (if any-model
      (sqlite-select (orsb-embed-cache-db)
                     "SELECT 1 FROM embeddings WHERE node_id = ? LIMIT 1" (list node-id))
    (sqlite-select (orsb-embed-cache-db)
                   "SELECT 1 FROM embeddings WHERE node_id = ? AND model = ? LIMIT 1"
                   (list node-id (orsb-embed-cache--model nil)))))

(defun orsb-embed-cache-map (fn &optional model)
  "Call FN with NODE-ID and VECTOR for every row of MODEL."
  (let ((set (sqlite-select (orsb-embed-cache-db)
                            "SELECT node_id, vector FROM embeddings WHERE model = ?"
                            (list (orsb-embed-cache--model model)) 'set)))
    (unwind-protect
        (while (sqlite-more-p set)
          (when-let ((row (sqlite-next set)))
            (funcall fn (car row) (orsb-embed-cache-unpack (cadr row)))))
      (sqlite-finalize set))))

(defun orsb-embed-cache-count (&optional model)
  "Number of rows for MODEL (every model when MODEL is t)."
  (caar (if (eq model t)
            (sqlite-select (orsb-embed-cache-db) "SELECT count(*) FROM embeddings")
          (sqlite-select (orsb-embed-cache-db) "SELECT count(*) FROM embeddings WHERE model = ?"
                         (list (orsb-embed-cache--model model))))))

;;;; Writes

(defun orsb-embed-cache-put (node-id vector hash &optional model)
  "Store VECTOR made from text with HASH for NODE-ID and MODEL, replacing any row."
  (sqlite-execute (orsb-embed-cache-db)
                  "INSERT OR REPLACE INTO embeddings (node_id, model, content_hash, dims, vector, updated_at) VALUES (?, ?, ?, ?, ?, ?)"
                  (list node-id (orsb-embed-cache--model model) (or hash "")
                        (length vector) (orsb-embed-cache-pack vector) (orsb-embed-cache--now))))

(defun orsb-embed-cache-put-new (node-id vector hash &optional model)
  "Like `orsb-embed-cache-put', but keep an existing row.
Returns non-nil when a row was written."
  (> (sqlite-execute (orsb-embed-cache-db)
                     "INSERT OR IGNORE INTO embeddings (node_id, model, content_hash, dims, vector, updated_at) VALUES (?, ?, ?, ?, ?, ?)"
                     (list node-id (orsb-embed-cache--model model) (or hash "")
                           (length vector) (orsb-embed-cache-pack vector) (orsb-embed-cache--now)))
     0))

(defun orsb-embed-cache-delete (node-id &optional model)
  "Remove NODE-ID's row for MODEL (every model when MODEL is t)."
  (if (eq model t)
      (sqlite-execute (orsb-embed-cache-db) "DELETE FROM embeddings WHERE node_id = ?" (list node-id))
    (sqlite-execute (orsb-embed-cache-db) "DELETE FROM embeddings WHERE node_id = ? AND model = ?"
                    (list node-id (orsb-embed-cache--model model)))))

(defun orsb-embed-cache-prune (live-ids &optional keep-other-models)
  "Delete rows whose node is not in LIVE-IDS, and rows of other models.
With KEEP-OTHER-MODELS, rows made by other models are left alone.
Returns the number of rows deleted."
  (let ((live (make-hash-table :test 'equal))
        (model (orsb-embed-cache--model nil))
        (doomed nil))
    (dolist (id live-ids) (puthash id t live))
    (dolist (row (sqlite-select (orsb-embed-cache-db) "SELECT node_id, model FROM embeddings"))
      (when (or (not (gethash (car row) live))
                (and (not keep-other-models) (not (equal (cadr row) model))))
        (push row doomed)))
    (with-sqlite-transaction (orsb-embed-cache-db)
      (dolist (row doomed)
        (sqlite-execute (orsb-embed-cache-db)
                        "DELETE FROM embeddings WHERE node_id = ? AND model = ?" row)))
    (length doomed)))

(provide 'orsb-embed-cache)
;;; orsb-embed-cache.el ends here
