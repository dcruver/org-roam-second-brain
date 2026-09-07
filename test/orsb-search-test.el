;;; orsb-search-test.el --- ERT tests for orsb-search -*- lexical-binding: t; -*-

;;; Commentary:
;; The embedding service is replaced by a deterministic fake (a vector
;; derived from the text), so these tests exercise hashing, stale detection,
;; the queue, the index and the cache file without any network.

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
  (let ((h (sha1 (org-roam-semantic--normalize-text text))))
    (mapcar (lambda (i) (/ (float (string-to-number (substring h (* 2 i) (+ 2 (* 2 i))) 16)) 255.0))
            '(0 1 2 3))))

(defmacro orsb-search-test--with-fake (&rest body)
  "Run BODY with the embedding API faked, tiny chunks, and a temp cache file."
  (declare (indent 0))
  `(let ((orsb-search-test--calls 0)
         (org-roam-semantic-min-chunk-size 3)
         (orsb-search-cache-file (make-temp-file "orsb-cache-" nil ".eld"))
         (orsb-search--index (make-hash-table :test 'equal))
         (orsb-search--index-loaded nil)
         (orsb-search--index-dirty nil)
         (orsb-search--queue nil)
         (orsb-search--timer nil))
     (unwind-protect
         (cl-letf (((symbol-function 'org-roam-ai-generate-embedding) #'orsb-search-test--fake-embedding))
           ,@body)
       (when orsb-search--timer (cancel-timer orsb-search--timer))
       (ignore-errors (delete-file orsb-search-cache-file)))))

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

(ert-deftest orsb-search-generate-writes-embedding-and-hash ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        (should (orsb-search-file-stale-p file))
        (should (equal (orsb-search-generate-file file) '(1 . 0)))
        (let ((text (orsb-test--file "reference/frigate.org")))
          (should (string-match-p "^\\* Overview\n:PROPERTIES:\n:ID: +[0-9a-f-]+\n:EMBEDDING: +[0-9. -]+\n:EMBEDDING_HASH: +[0-9a-f]+\n:END:" text))
          ;; the short heading only got an id
          (should (string-match-p "^\\* Tiny\n:PROPERTIES:\n:ID:" text))
          (should-not (string-match-p "Tiny\n:PROPERTIES:\n:ID:[^\n]*\n:EMBEDDING:" text)))
        (should-not (orsb-search-file-stale-p file))
        ;; unchanged: nothing regenerated, no service call
        (let ((before orsb-search-test--calls))
          (should (equal (orsb-search-generate-file file) '(0 . 1)))
          (should (= before orsb-search-test--calls)))))))

(ert-deftest orsb-search-edit-makes-chunk-stale-then-regenerates ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/frigate.org" orsb-search-test--note)))
        (org-roam-db-sync)
        (orsb-search-generate-file file)
        (let ((old-hash (plist-get (car (orsb-search--read-stored file)) :hash)))
          (orsb-core-set-body (orsb-core-resolve "s-1") "* Overview\nFrigate now also runs object detection on the GPU.\n\n* Tiny\nshort\n")
          (should (orsb-search-file-stale-p file))
          (should (equal (orsb-search-generate-file file) '(1 . 0)))
          (should-not (equal old-hash (plist-get (car (orsb-search--read-stored file)) :hash)))
          (should-not (orsb-search-file-stale-p file)))))))

(ert-deftest orsb-search-backfill-gives-old-embeddings-a-hash ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (let ((file (orsb-test--write "reference/old.org" "\
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
")))
        (org-roam-db-sync)
        (should (orsb-search-file-stale-p file))
        (should (= 1 (orsb-search-backfill-hashes (list file))))
        (should-not (orsb-search-file-stale-p file))
        (should (string-match-p ":EMBEDDING: 0.1 0.2 0.3 0.4\n:EMBEDDING_HASH: [0-9a-f]+" (orsb-test--file "reference/old.org")))
        (should (= 0 orsb-search-test--calls))))))

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
        ;; second query reads the index: no re-parse (stat only)
        (let ((parses 0))
          (cl-letf* ((orig (symbol-function 'orsb-search--read-stored))
                     ((symbol-function 'orsb-search--read-stored)
                      (lambda (f) (setq parses (1+ parses)) (funcall orig f))))
            (orsb-search-similar "camera" 5 0.0)
            (should (= parses 0))))
        ;; every vault file is indexed (the fixture hub carries one vector too)
        (let ((stats (orsb-search-index-stats)))
          (should (>= (car stats) 1))
          (should (>= (cdr stats) 1))
          (should (gethash (file-truename file) orsb-search--index))
          ;; the cache file round-trips
          (orsb-search--index-save)
          (should (file-exists-p orsb-search-cache-file))
          (clrhash orsb-search--index)
          (setq orsb-search--index-loaded nil)
          (orsb-search--index-load)
          (should (equal (orsb-search-index-stats) stats)))
        ;; through the tool contract
        (let ((d (cdr (orsb-tools-test--call "search" 'query "camera streams" 'mode "semantic" 'cutoff 0.0))))
          (should (eq (alist-get 'ok d) t))
          (should (equal (alist-get 'id (car (alist-get 'hits (alist-get 'data d)))) "s-1")))
        (let ((n (cdr (orsb-tools-test--call "get_node" 'id "s-1" 'include_body :json-false))))
          (should (eq (alist-get 'embedding_stale (alist-get 'data n)) :json-false)))))))

(require 'orsb-tools-test)  ; for orsb-tools-test--call

(provide 'orsb-search-test)
;;; orsb-search-test.el ends here
