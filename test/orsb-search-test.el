;;; orsb-search-test.el --- ERT tests for orsb-search -*- lexical-binding: t; -*-

;;; Commentary:
;; The embedding service is replaced by a deterministic fake (a vector
;; derived from the text), so these tests exercise hashing, stale detection,
;; the SQLite cache, the migration, the queue and the index without any
;; network.  Each test gets its own temporary cache database.

;;; Code:

(require 'ert)
(require 'org-roam)
(require 'orsb-core)
(require 'orsb-search)
(require 'orsb-tools)
(require 'orsb-core-test)   ; vault fixture

(defvar orsb-search-test--calls 0 "Fake embedding calls made.")

(defun orsb-search-test--fake-embedding (text)
  "A 4-float vector that depends on TEXT only."
  (setq orsb-search-test--calls (1+ orsb-search-test--calls))
  (let ((h (sha1 (orsb-search--normalize text))))
    (mapcar (lambda (i) (/ (float (string-to-number (substring h (* 2 i) (+ 2 (* 2 i))) 16)) 255.0))
            '(0 1 2 3))))

(defmacro orsb-search-test--with-fake (&rest body)
  "Run BODY with the embedding API faked, tiny chunks, and a temp cache db."
  (declare (indent 0))
  `(let ((orsb-search-test--calls 0)
         (orsb-search-min-chunk-words 3)
         (orsb-embedding-model "fake-model")
         (orsb-embed-cache-file (make-temp-file "orsb-cache-" nil ".db"))
         (orsb-embed-cache--db nil)
         (orsb-embed-cache--db-file nil)
         (orsb-search--index (make-hash-table :test 'equal))
         (orsb-search--index-loaded nil)
         (orsb-search--index-model nil)
         (orsb-search--queue nil)
         (orsb-search--timer nil))
     (unwind-protect
         (cl-letf (((symbol-function 'orsb-search-embed) #'orsb-search-test--fake-embedding))
           ,@body)
       (when orsb-search--timer (cancel-timer orsb-search--timer))
       (orsb-embed-cache-close)
       (ignore-errors (delete-file orsb-embed-cache-file)))))

(defconst orsb-search-test--note "\
:PROPERTIES:
:ID: s-1
:NODE-TYPE: reference
:END:
#+title: Frigate NVR

* Overview
Frigate records the camera streams on the services VLAN.

* Tiny
short
")

(defun orsb-search-test--heading-id (file heading)
  "The :ID: under HEADING in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (re-search-forward (format "^\\*+ %s\n:PROPERTIES:\n:ID: +\\(\\S-+\\)" (regexp-quote heading)))
    (match-string 1)))

;;;; The store

(ert-deftest orsb-embed-cache-pack-round-trips ()
  (let* ((v '(0.0 1.0 -1.0 0.5 -0.123456 3.0e-5 12345.678 1.0e-40))
         (back (orsb-embed-cache-unpack (orsb-embed-cache-pack v))))
    (should (= (length back) (length v)))
    (cl-mapc (lambda (a b)
               (should (<= (abs (- a b)) (max 1e-30 (* 1e-6 (abs a))))))
             (butlast v) (butlast back))
    ;; below the float32 normal range: zero
    (should (= 0.0 (car (last back))))
    (should (= 32 (length (orsb-embed-cache-pack v))))
    (should-error (orsb-embed-cache-pack (list 0.0e+NaN)))))

(ert-deftest orsb-embed-cache-rows-are-blobs-keyed-by-model ()
  (orsb-search-test--with-fake
    (orsb-embed-cache-put "n-1" '(0.25 -0.5 1.0) "h1")
    (orsb-embed-cache-put "n-1" '(1.0 0.0 0.0) "h1" "other-model")
    (should (equal (plist-get (orsb-embed-cache-get "n-1") :vector) '(0.25 -0.5 1.0)))
    (should (equal (plist-get (orsb-embed-cache-get "n-1" "other-model") :vector) '(1.0 0.0 0.0)))
    (should (= 3 (plist-get (orsb-embed-cache-get "n-1") :dims)))
    (should (equal (caar (sqlite-select (orsb-embed-cache-db) "SELECT DISTINCT typeof(vector) FROM embeddings")) "blob"))
    (should (= 2 (orsb-embed-cache-count t)))
    ;; put-new keeps what is there
    (should-not (orsb-embed-cache-put-new "n-1" '(9.0 9.0 9.0) "h2"))
    (should (equal (plist-get (orsb-embed-cache-get "n-1") :hash) "h1"))
    ;; survives closing and reopening (a daemon restart)
    (orsb-embed-cache-close)
    (should (equal (gethash "n-1" (orsb-embed-cache-hashes '("n-1" "nope"))) "h1"))
    ;; prune: unknown nodes and other models go
    (orsb-embed-cache-put "gone" '(1.0) "h")
    (should (= 2 (orsb-embed-cache-prune '("n-1"))))
    (should (= 1 (orsb-embed-cache-count t)))))

;;;; Generation

(ert-deftest orsb-search-generate-writes-cache-not-note ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        (should (orsb-search-file-stale-p file))
        (should (equal (orsb-search-generate-file file) '(1 . 0)))
        (let ((text (orsb-test--file "reference/frigate.org")))
          ;; the headings got ids (their keys), and nothing else
          (should (string-match-p "^\\* Overview\n:PROPERTIES:\n:ID: +[0-9a-f-]+\n:END:\nFrigate" text))
          (should (string-match-p "^\\* Tiny\n:PROPERTIES:\n:ID:" text))
          (should-not (string-match-p "EMBEDDING" text)))
        (let* ((id (orsb-search-test--heading-id file "Overview"))
               (row (orsb-embed-cache-get id)))
          (should row)
          (should (= 4 (plist-get row :dims)))
          (should (stringp (plist-get row :hash)))
          ;; the new heading ids reached org-roam's database
          (should (org-roam-node-from-id id)))
        (should-not (orsb-search-file-stale-p file))
        ;; unchanged: nothing regenerated, no service call, file untouched
        (let ((before orsb-search-test--calls)
              (mtime (file-attribute-modification-time (file-attributes file))))
          (should (equal (orsb-search-generate-file file) '(0 . 1)))
          (should (= before orsb-search-test--calls))
          (should (equal mtime (file-attribute-modification-time (file-attributes file)))))))))

;; A schema file states rules; embedding it would give its headings ids and
;; so turn them into notes.
(ert-deftest orsb-search-leaves-schema-files-alone ()
  (orsb-search-test--with-fake
    (let* ((dir (make-temp-file "orsb-schema-" t))
           (file (expand-file-name "_schema.org" dir))
           (text "* Type\nWork with an outcome and an end, as this vault sees it.\n"))
      (unwind-protect
          (progn
            (with-temp-file file (insert text))
            (should (equal (orsb-search-generate-file file) '(0 . 0)))
            (should (= orsb-search-test--calls 0))
            (should (equal text (with-temp-buffer (insert-file-contents file) (buffer-string)))))
        (delete-directory dir t)))))

(ert-deftest orsb-search-edit-makes-chunk-stale-then-regenerates ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        (orsb-search-generate-file file)
        (let* ((id (orsb-search-test--heading-id file "Overview"))
               (old-hash (plist-get (orsb-embed-cache-get id) :hash)))
          (orsb-core-set-body (orsb-core-resolve "s-1")
                              (format "* Overview\n:PROPERTIES:\n:ID: %s\n:END:\nFrigate now also runs object detection on the GPU.\n\n* Tiny\n:PROPERTIES:\n:ID: %s\n:END:\nshort\n"
                                      id (orsb-search-test--heading-id file "Tiny")))
          (should (orsb-search-file-stale-p file))
          (let ((text-before (orsb-test--file "reference/frigate.org")))
            (should (equal (orsb-search-generate-file file) '(1 . 0)))
            ;; re-embedding an existing heading does not touch the note
            (should (equal text-before (orsb-test--file "reference/frigate.org"))))
          (should-not (equal old-hash (plist-get (orsb-embed-cache-get id) :hash)))
          (should-not (orsb-search-file-stale-p file)))))))

(ert-deftest orsb-search-heading-with-keyword-and-tags-gets-an-id ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/todo.org" "\
:PROPERTIES:
:ID: s-4
:END:
#+title: Todo

* TODO [#A] Replace the UPS battery :hardware:
The rack UPS battery is four years old and fails its self test.

* TODO [#A] Replace the UPS battery :hardware:
Same heading text twice; each gets its own id.
")))
        (org-roam-db-sync)
        (should (equal (orsb-search-generate-file file) '(2 . 0)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((ids nil))
            (while (re-search-forward "^:ID: +\\([0-9a-f-]+\\)$" nil t) (push (match-string 1) ids))
            (should (= 2 (length (delete-dups ids))))
            (dolist (id ids) (should (orsb-embed-cache-get id)))))))))

(ert-deftest orsb-search-unsaved-buffer-is-left-alone ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-max))
          (insert "\nTyping, not saved yet.\n"))
        ;; no ids can be added under someone's unsaved edits, so nothing is keyed yet
        (should (equal (orsb-search-generate-file file) '(0 . 0)))
        (should-not (string-match-p "^:ID: +[0-9a-f]\\{8\\}-" (orsb-test--file "reference/frigate.org")))
        (should (orsb-search-file-stale-p file))))))

(ert-deftest orsb-search-model-switch-re-embeds-into-new-rows ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        (orsb-search-generate-file file)
        (let ((orsb-embedding-model "new-model"))
          (should (orsb-search-file-stale-p file))
          (should (equal (orsb-search-generate-file file) '(1 . 0)))
          (should-not (orsb-search-file-stale-p file))
          ;; both models' rows sit side by side until pruned
          (should (= 2 (orsb-embed-cache-count t)))
          (should (= 1 (orsb-search-prune-cache)))
          (should (= 1 (orsb-embed-cache-count t)))
          (should (= 1 (orsb-embed-cache-count "new-model"))))))))

;;;; Migration from in-note vectors

(defconst orsb-search-test--old-note "\
:PROPERTIES:
:ID: s-2
:END:
#+title: Old
* Section
:PROPERTIES:
:ID: 11111111-2222-3333-4444-555555555555
:EMBEDDING: 0.1 0.2 0.3 0.4
:END:
Some long enough text here.
* Hashed
:PROPERTIES:
:ID: 66666666-2222-3333-4444-555555555555
:EMBEDDING: 0.5 0.6 0.7 0.8
:EMBEDDING_HASH: 0123456789abcdef
:END:
More long enough text here.
* Keyless
:PROPERTIES:
:EMBEDDING: 0.9 0.9 0.9 0.9
:END:
A vector with no id to key it by.
")

(ert-deftest orsb-search-migrate-copies-vectors-and-leaves-notes-alone ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let* ((file (orsb-test--write "reference/old.org" orsb-search-test--old-note))
             (before (orsb-test--file "reference/old.org")))
        (org-roam-db-sync)
        (let ((r (orsb-search-migrate-properties (list file))))
          (should (= 1 (plist-get r :files)))
          (should (= 2 (plist-get r :copied)))
          (should (= 0 (plist-get r :present)))
          (should (= 1 (plist-get r :no-id))))
        ;; not one byte of the note changed
        (should (equal before (orsb-test--file "reference/old.org")))
        (let ((row (orsb-embed-cache-get "11111111-2222-3333-4444-555555555555")))
          (should (equal (mapcar (lambda (x) (/ (round (* x 1e4)) 1e4)) (plist-get row :vector)) '(0.1 0.2 0.3 0.4)))
          ;; no stored hash: the current text's hash, so the chunk is not stale
          (should (equal (plist-get row :hash)
                         (nth 3 (seq-find (lambda (c) (equal (nth 1 c) "Section")) (orsb-search--current-chunks file))))))
        ;; a stored hash is carried over as it was
        (should (equal (plist-get (orsb-embed-cache-get "66666666-2222-3333-4444-555555555555") :hash) "0123456789abcdef"))
        ;; again: harmless
        (let ((r (orsb-search-migrate-properties (list file))))
          (should (= 0 (plist-get r :copied)))
          (should (= 2 (plist-get r :present))))
        (should (= 0 orsb-search-test--calls))))))

(ert-deftest orsb-search-strip-is-separate-and-only-drops-cached-vectors ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let* ((file (orsb-test--write "reference/old.org" orsb-search-test--old-note)))
        (org-roam-db-sync)
        ;; before migrating there is nothing in the cache, so nothing goes
        (should (= 0 (plist-get (orsb-search-strip-properties (list file)) :removed)))
        (orsb-search-migrate-properties)
        ;; dry run writes nothing
        (let ((before (orsb-test--file "reference/old.org"))
              (r (orsb-search-strip-properties (list file) t)))
          (should (= 2 (plist-get r :removed)))
          (should (= 1 (plist-get r :kept)))
          (should (equal before (orsb-test--file "reference/old.org"))))
        (let ((r (orsb-search-strip-properties (list file))))
          (should (= 1 (plist-get r :files)))
          (should (= 2 (plist-get r :removed)))
          (should (= 1 (plist-get r :kept))))
        (let ((text (orsb-test--file "reference/old.org")))
          (should (string-match-p "^\\* Section\n:PROPERTIES:\n:ID: +11111111-2222-3333-4444-555555555555\n:END:\nSome" text))
          (should (string-match-p "^\\* Hashed\n:PROPERTIES:\n:ID: +66666666[^\n]*\n:END:\nMore" text))
          (should-not (string-match-p "EMBEDDING_HASH" text))
          ;; the keyless vector had no cache row: kept
          (should (string-match-p "^\\* Keyless\n:PROPERTIES:\n:EMBEDDING: 0.9" text)))
        ;; search still finds the stripped vectors, now from the cache
        (let ((hits (orsb-search-similar-vector '(0.1 0.2 0.3 0.4) 1 0.9)))
          (should (equal (nth 3 (car hits)) "Section")))))))

;;;; Worker, index, query

(ert-deftest orsb-search-queue-worker-and-after-write ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        ;; an MCP write queues the file instead of embedding inline
        (orsb-core-set-properties (orsb-core-resolve "s-1") '(("STATUS" . "active")))
        (should (member (file-truename file) orsb-search--queue))
        (should (= 0 orsb-search-test--calls))
        (orsb-search-drain)
        (should-not orsb-search--queue)
        (should-not orsb-search--timer)
        (should (= 1 orsb-search-test--calls))
        (should-not (orsb-search-file-stale-p file))))))

(ert-deftest orsb-search-index-and-query ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        (orsb-search-generate-file file)
        ;; the fake embedding is a function of the exact normalized chunk text
        (let ((hits (orsb-search-similar "Overview. Frigate records the camera streams on the services VLAN." 5 0.0)))
          (should hits)
          (should (equal (file-truename (car (car hits))) (file-truename file)))
          (should (equal (nth 3 (car hits)) "Overview"))
          (should (> (cadr (car hits)) 0.99)))
        ;; queries read no note
        (let ((reads 0))
          (cl-letf* ((orig (symbol-function 'insert-file-contents))
                     ((symbol-function 'insert-file-contents)
                      (lambda (&rest args) (setq reads (1+ reads)) (apply orig args))))
            (orsb-search-similar "camera" 5 0.0)
            (should (= reads 0))))
        (let ((stats (orsb-search-index-stats)))
          (should (equal stats '(1 . 1)))
          ;; a restart reloads the same index from the cache file
          (orsb-search-index-reset)
          (orsb-embed-cache-close)
          (should (equal (orsb-search-index-stats) stats)))
        ;; through the tool contract
        (let ((d (cdr (orsb-tools-test--call "search" 'query "camera streams" 'mode "semantic" 'cutoff 0.0))))
          (should (eq (alist-get 'ok d) t))
          (should (equal (alist-get 'id (car (alist-get 'hits (alist-get 'data d)))) "s-1")))
        (let ((n (cdr (orsb-tools-test--call "get_node" 'id "s-1" 'include_body :json-false))))
          (should (eq (alist-get 'embedding_stale (alist-get 'data n)) :json-false)))))))

(ert-deftest orsb-search-legacy-commands-use-the-cache ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        ;; the old per-file command embeds into the cache, not the note
        (orsb-search-generate-file file)
        (should-not (string-match-p "EMBEDDING" (orsb-test--file "reference/frigate.org")))
        (should-not (orsb-search-file-stale-p file))
        ;; the old query function reads the cache
        (let ((hits (orsb-search--similar-or-nil
                     "Overview. Frigate records the camera streams on the services VLAN." 3 0.5)))
          (should (equal (nth 3 (car hits)) "Overview")))
        ;; unreachable service: nil, as before, not an error
        (cl-letf (((symbol-function 'orsb-search-embed) (lambda (_) nil)))
          (should-not (orsb-search--similar-or-nil "anything")))))))

(require 'orsb-tools-test)  ; for orsb-tools-test--call

(provide 'orsb-search-test)
;;; orsb-search-test.el ends here
