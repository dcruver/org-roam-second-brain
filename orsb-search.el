;;; orsb-search.el --- Semantic search with stale detection and an index -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.2"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:

;; Builds on the embedding primitives in `org-roam-vector-search.el' (the
;; OpenAI-compatible call, the chunk parser, the drawer writer) and adds
;; the three things it lacked:
;;
;; - Stale detection.  Every chunk embedding is stored next to
;;   :EMBEDDING_HASH:, the sha1 of the normalized chunk text.  A chunk is
;;   regenerated only when its hash changed; an edited section no longer
;;   keeps its old vector forever.
;; - An index.  Query time reads an in-memory table keyed by file mtime
;;   instead of re-reading and re-parsing every note; the table is written
;;   to `orsb-search-cache-file' when Emacs is idle so a daemon restart does
;;   not start cold.
;; - A queue.  Writes (MCP tools, saves) enqueue the file; a worker runs when
;;   Emacs is idle and embeds one file at a time.  Nothing on the request path
;;   or in a save hook waits for the embedding service.

;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-roam-vector-search)
(require 'seq)
(require 'subr-x)

(defgroup orsb-search nil
  "org-roam-second-brain semantic search."
  :group 'orsb
  :prefix "orsb-search-")

(defcustom orsb-search-cache-file (locate-user-emacs-file "orsb-embeddings.eld")
  "File the embedding index is persisted to (nil disables persistence)."
  :type '(choice (const nil) file)
  :group 'orsb-search)

(defcustom orsb-search-idle-delay 2
  "Seconds of idle time before the embedding worker takes the next queued file."
  :type 'number
  :group 'orsb-search)

(defcustom orsb-search-hash-property "EMBEDDING_HASH"
  "Property that records which text an :EMBEDDING: was computed from."
  :type 'string
  :group 'orsb-search)

;;;; Chunks on disk

(defun orsb-search--chunk-hash (text)
  "The stale-detection hash of chunk TEXT."
  (sha1 (or (org-roam-semantic--normalize-text text) "")))

(defun orsb-search--parse-vector (string)
  "Parse a space-separated float STRING into a list, or nil."
  (condition-case nil
      (let ((v (mapcar #'string-to-number (split-string string nil t))))
        (and v v))
    (error nil)))

(defun orsb-search--read-stored (file)
  "Return the stored chunks of FILE as a list of plists.
Each has :position (heading start, or 1 for the file drawer), :heading,
:embedding (a list of floats or nil) and :hash (string or nil)."
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
                                :embedding (and emb (orsb-search--parse-vector emb))
                                :hash hash)
                          out))))))))
      (nreverse out))))

(defun orsb-search--strip-drawers (text)
  "TEXT without property drawers, so stored vectors do not alter the hash.
Not anchored to a line start: the chunk parser glues the heading and its
body with \". \", so the drawer can follow the heading on one line."
  (replace-regexp-in-string
   "[ \t]*:PROPERTIES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" " " text))

(defun orsb-search--current-chunks (file)
  "Chunks FILE has now: list of (POSITION HEADING TEXT HASH EMBED-P).
TEXT is the heading plus its body with drawers removed; it is what gets
embedded and hashed, and its word count (not the parser's, which counts
drawer lines) decides whether the chunk is embedded or only gets an id."
  (let ((inhibit-message t) (message-log-max nil))
    (mapcar (lambda (c)
              (let ((text (orsb-search--strip-drawers (nth 2 c))))
                (list (nth 0 c) (nth 1 c) text
                      (orsb-search--chunk-hash text)
                      (>= (org-roam-semantic--count-words text)
                          org-roam-semantic-min-chunk-size))))
            (org-roam-semantic--parse-chunks file))))

(defun orsb-search-file-stale-p (file)
  "Non-nil when FILE has a chunk whose embedding is missing or outdated."
  (let ((stored (orsb-search--read-stored file)))
    (seq-some (lambda (c)
                (and (nth 4 c)
                     (let ((s (seq-find (lambda (x) (equal (plist-get x :heading) (nth 1 c))) stored)))
                       (or (null s)
                           (null (plist-get s :embedding))
                           (not (equal (plist-get s :hash) (nth 3 c)))))))
              (orsb-search--current-chunks file))))

;;;; Generation

(defvar orsb-search--inhibit-hooks nil
  "Non-nil while the worker saves a file, so the save hook does not re-queue it.")

(defun orsb-search--write (file heading embedding hash)
  "Store EMBEDDING and HASH for HEADING in FILE without saving."
  (org-roam-semantic--store-embedding file embedding heading)
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote heading)) nil t)
       (org-entry-put (point) orsb-search-hash-property hash)))))

(defun orsb-search--save (file)
  "Write FILE's buffer to disk without running save hooks."
  (with-current-buffer (find-file-noselect file)
    (when (buffer-modified-p)
      (let ((orsb-search--inhibit-hooks t)
            (inhibit-read-only t))
        (write-region (point-min) (point-max) file nil :silent)
        (set-buffer-modified-p nil)))))

(defun orsb-search-generate-file (file &optional force)
  "Embed the chunks of FILE whose text changed (all of them with FORCE).
Short chunks only get an :ID:.  Returns (GENERATED . SKIPPED)."
  (let* ((stored (orsb-search--read-stored file))
         (generated 0) (skipped 0))
    (dolist (c (orsb-search--current-chunks file))
      (pcase-let ((`(,_pos ,heading ,text ,hash ,embed-p) c))
        (if (not embed-p)
            (org-roam-semantic--ensure-heading-id file heading)
          (let ((s (seq-find (lambda (x) (equal (plist-get x :heading) heading)) stored)))
            (if (and (not force) s (plist-get s :embedding) (equal (plist-get s :hash) hash))
                (setq skipped (1+ skipped))
              (let ((embedding (org-roam-ai-generate-embedding text)))
                (if embedding
                    (progn (orsb-search--write file heading embedding hash)
                           (setq generated (1+ generated)))
                  (message "orsb-search: no embedding for %S in %s" heading (file-name-nondirectory file)))))))))
    (orsb-search--save file)
    (when (> generated 0) (orsb-search--index-invalidate file))
    (cons generated skipped)))

(defun orsb-search-backfill-hashes (&optional files)
  "Give every hash-less stored embedding in FILES (default: all notes) a hash.
Assumes the stored vector matches the current text, which is the right
call once for a vault that predates hashes; afterwards `stale' works.
Returns the number of hashes written."
  (interactive)
  (let ((n 0))
    (dolist (file (or files (org-roam-list-files)))
      (let ((stored (orsb-search--read-stored file)))
        (when (seq-some (lambda (s) (and (plist-get s :embedding) (null (plist-get s :hash)))) stored)
          (let ((current (orsb-search--current-chunks file)))
            (dolist (s stored)
              (when (and (plist-get s :embedding) (null (plist-get s :hash)) (plist-get s :heading))
                (when-let ((c (seq-find (lambda (x) (equal (nth 1 x) (plist-get s :heading))) current)))
                  (with-current-buffer (find-file-noselect file)
                    (org-with-wide-buffer
                     (goto-char (point-min))
                     (when (re-search-forward (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote (nth 1 c))) nil t)
                       (org-entry-put (point) orsb-search-hash-property (nth 3 c))
                       (setq n (1+ n))))))))
            (orsb-search--save file)
            (orsb-search--index-invalidate file)))))
    (when (called-interactively-p 'any) (message "orsb-search: wrote %d hashes" n))
    n))

;;;; Queue and worker

(defvar orsb-search--queue nil "Files waiting for the embedding worker.")
(defvar orsb-search--timer nil "The idle timer driving the worker.")

(defun orsb-search-enqueue (file)
  "Queue FILE for (re)embedding when Emacs is idle."
  (when (and file (not orsb-search--inhibit-hooks) (org-roam-file-p file))
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

;; The legacy hook embedded synchronously on every save (and did so before
;; the save, so a fresh file was not on disk yet); replace it.
(remove-hook 'before-save-hook 'org-roam-semantic--update-on-save)
(add-hook 'after-save-hook #'orsb-search--after-save)

;;;; Index

(defvar orsb-search--index (make-hash-table :test 'equal)
  "File -> (MTIME . CHUNKS), CHUNKS a list of (POSITION HEADING VECTOR).")
(defvar orsb-search--index-dirty nil)
(defvar orsb-search--index-loaded nil)
(defvar orsb-search--save-timer nil)

(defun orsb-search--mtime (file)
  "FILE's modification time as a float."
  (float-time (file-attribute-modification-time (file-attributes file))))

(defun orsb-search--index-invalidate (file)
  "Forget FILE's cached chunks."
  (remhash (file-truename file) orsb-search--index)
  (setq orsb-search--index-dirty t))

(defun orsb-search--file-chunks (file)
  "Cached (POSITION HEADING VECTOR) triples for FILE, re-read when it changed."
  (orsb-search--index-load)
  (let* ((file (file-truename file))
         (mtime (orsb-search--mtime file))
         (cached (gethash file orsb-search--index)))
    (if (and cached (= (car cached) mtime))
        (cdr cached)
      (let ((chunks (delq nil (mapcar (lambda (s)
                                        (when (plist-get s :embedding)
                                          (list (plist-get s :position) (plist-get s :heading) (plist-get s :embedding))))
                                      (orsb-search--read-stored file)))))
        (puthash file (cons mtime chunks) orsb-search--index)
        (setq orsb-search--index-dirty t)
        (orsb-search--index-schedule-save)
        chunks))))

(defun orsb-search--index-schedule-save ()
  "Write the index when Emacs has been idle a while."
  (when (and orsb-search-cache-file (not orsb-search--save-timer))
    (setq orsb-search--save-timer
          (run-with-idle-timer 10 nil
                               (lambda ()
                                 (setq orsb-search--save-timer nil)
                                 (orsb-search--index-save))))))

(defun orsb-search--index-save ()
  "Persist the index to `orsb-search-cache-file'."
  (when (and orsb-search-cache-file orsb-search--index-dirty)
    (let ((alist nil))
      (maphash (lambda (k v) (push (cons k v) alist)) orsb-search--index)
      (make-directory (file-name-directory (expand-file-name orsb-search-cache-file)) t)
      (with-temp-file orsb-search-cache-file
        (let ((print-length nil) (print-level nil))
          (prin1 alist (current-buffer))))
      (setq orsb-search--index-dirty nil))))

(defun orsb-search--index-load ()
  "Load the persisted index once; entries are validated by mtime on use."
  (unless orsb-search--index-loaded
    (setq orsb-search--index-loaded t)
    (when (and orsb-search-cache-file (file-exists-p orsb-search-cache-file))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents orsb-search-cache-file)
            (dolist (cell (read (current-buffer)))
              (puthash (car cell) (cdr cell) orsb-search--index)))
        (error (message "orsb-search: ignoring cache %s: %s" orsb-search-cache-file (error-message-string err)))))))

(defun orsb-search-index-reset ()
  "Drop the in-memory index (and the cache file)."
  (interactive)
  (clrhash orsb-search--index)
  (setq orsb-search--index-dirty nil orsb-search--index-loaded t)
  (when (and orsb-search-cache-file (file-exists-p orsb-search-cache-file))
    (delete-file orsb-search-cache-file)))

;;;; Query

(defun orsb-search-similar (query &optional limit cutoff)
  "Chunks most similar to QUERY, best first, at most LIMIT with score >= CUTOFF.
Each hit is (FILE SCORE POSITION HEADING), the shape of
`org-roam-semantic-get-similar-data', but read from the index.
Signals an error when the embedding service is unreachable."
  (let ((limit (or limit 10))
        (cutoff (or cutoff 0.0))
        (qv (org-roam-ai-generate-embedding query))
        (hits nil))
    (unless qv (error "Embedding service returned nothing for the query"))
    (dolist (file (org-roam-list-files))
      (dolist (c (orsb-search--file-chunks file))
        (let ((score (org-roam-semantic--cosine-similarity qv (nth 2 c))))
          (when (and score (>= score cutoff))
            (push (list file score (nth 0 c) (nth 1 c)) hits)))))
    (seq-take (sort hits (lambda (a b) (> (cadr a) (cadr b)))) limit)))

(defun orsb-search-index-stats ()
  "Return (FILES-INDEXED . CHUNKS-INDEXED)."
  (let ((files 0) (chunks 0))
    (maphash (lambda (_k v) (setq files (1+ files) chunks (+ chunks (length (cdr v))))) orsb-search--index)
    (cons files chunks)))

(provide 'orsb-search)
;;; orsb-search.el ends here
