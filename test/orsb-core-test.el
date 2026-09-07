;;; orsb-core-test.el --- ERT tests for orsb-core -*- lexical-binding: t; -*-

;;; Commentary:
;; Runtime tests against a throwaway vault: real files, a real org-roam db,
;; real buffer edits.  Each test gets a fresh vault via `orsb-test-with-vault'.

;;; Code:

(require 'ert)
(require 'org-roam)
(require 'orsb-core)
(require 'org-roam-api)
(require 'json)

(defconst orsb-test--project-file "\
:PROPERTIES:
:ID: 1784854105
:NODE-TYPE: project
:STATUS: active
:NEXT-ACTION: Phase 4 status curation.
:END:
#+title: Project Backlog
#+filetags: :backlog:

* Next Actions
:PROPERTIES:
:ID:       f18f192d-5722-4641-b1c4-a15cf64c6e04
:END:
- [ ] Phase 4 status curation.

* Purpose
:PROPERTIES:
:ID:       c169b8b9-d9b6-4792-bf6e-4a01f51e74d1
:EMBEDDING: 0.1 0.2 0.3
:END:
Central index of everything I want to build.
"
  "A file-level project note shaped like the real vault.")

(defconst orsb-test--drawerless-file "\
:PROPERTIES:
:ID: 2dd2b7f0-0000-4000-8000-000000000001
:END:
#+title: Drawerless
#+filetags: :misc:

Just a body.
"
  "A note whose drawer holds only the :ID: (no STATUS or NODE-TYPE yet).")

(defconst orsb-test--no-id-file "\
#+title: Not A Node

Files without an :ID: are not org-roam nodes.
"
  "A file org-roam does not index.")

(defmacro orsb-test-with-vault (&rest body)
  "Run BODY with `org-roam-directory' bound to a fresh temporary vault."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "orsb-vault-" t))
          (org-roam-directory dir)
          (org-roam-db-location (expand-file-name "org-roam.db" dir))
          (org-roam-file-extensions '("org"))
          (org-id-locations-file (expand-file-name ".org-id-locations" dir))
          (org-id-track-globally t)
          (org-id-locations nil)
          (make-backup-files nil)
          (org-roam-db-update-on-save nil))
     (unwind-protect
         (progn
           (orsb-test--write "projects/project-backlog-1784854105.org" orsb-test--project-file)
           (orsb-test--write "misc/drawerless.org" orsb-test--drawerless-file)
           (orsb-test--write "misc/no-id.org" orsb-test--no-id-file)
           (org-roam-db-sync)
           ,@body)
       (org-roam-db--close-all)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b) (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun orsb-test--write (relative content)
  "Write CONTENT to RELATIVE under `org-roam-directory'."
  (let ((path (expand-file-name relative org-roam-directory)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defun orsb-test--file (relative)
  "Return the contents of RELATIVE under `org-roam-directory'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative org-roam-directory))
    (buffer-string)))

(defun orsb-test--json (string)
  "Parse STRING as JSON into an alist with symbol keys."
  (let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol))
    (json-read-from-string string)))

;;;; resolve

(ert-deftest orsb-core-resolve-by-id-path-and-title ()
  (orsb-test-with-vault
    (should (equal (org-roam-node-id (orsb-core-resolve "1784854105")) "1784854105"))
    (should (equal (org-roam-node-id (orsb-core-resolve "projects/project-backlog-1784854105.org")) "1784854105"))
    (should (equal (org-roam-node-id (orsb-core-resolve (expand-file-name "projects/project-backlog-1784854105.org" org-roam-directory))) "1784854105"))
    (should (equal (org-roam-node-id (orsb-core-resolve "Project Backlog")) "1784854105"))
    (should (= (org-roam-node-level (orsb-core-resolve "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1")) 1))
    (should-error (orsb-core-resolve "no-such-thing") :type 'orsb-error)
    (should-error (orsb-core-resolve "") :type 'orsb-error)))

;;;; file-level properties (the bug that started this)

(ert-deftest orsb-core-file-node-properties-round-trip ()
  (orsb-test-with-vault
    (let ((node (orsb-core-resolve "1784854105")))
      (should (equal (cdr (assoc "STATUS" (orsb-core-node-properties node))) "active"))
      (should (equal (cdr (assoc "NEXT-ACTION" (orsb-core-node-properties node))) "Phase 4 status curation."))
      (should-not (assoc "ID" (orsb-core-node-properties node)))
      (orsb-core-set-properties node '(("STATUS" . "done") (next_action . "ship it")))
      (let ((text (orsb-test--file "projects/project-backlog-1784854105.org")))
        (should (string-match-p "^:STATUS: +done$" text))
        (should (string-match-p "^:NEXT_ACTION: +ship it$" text))
        (should-not (string-match-p "^#\\+STATUS:" text))
        ;; exactly one occurrence -> two pieces
        (should (= 2 (length (split-string text ":ID: 1784854105")))))
      (should (equal (orsb-core-node-property node "status") "done"))
      ;; the db saw the write without a full sync
      (should (equal (org-roam-node-title (org-roam-node-from-id "1784854105")) "Project Backlog")))))

(ert-deftest orsb-core-id-only-drawer-gains-properties ()
  (orsb-test-with-vault
    (let ((node (orsb-core-resolve "Drawerless")))
      (should-not (orsb-core-node-properties node))
      (orsb-core-set-properties node '(("STATUS" . "waiting")))
      (let ((text (orsb-test--file "misc/drawerless.org")))
        (should (string-match-p "\\`:PROPERTIES:\n:ID: +2dd2b7f0[^\n]*\n:STATUS: +waiting\n:END:\n#\\+title: Drawerless$" text)))
      (should (equal (orsb-core-node-property node "STATUS") "waiting")))))

(ert-deftest orsb-core-file-without-id-is-not-a-node ()
  (orsb-test-with-vault
    (let ((err (should-error (orsb-core-resolve "misc/no-id.org") :type 'orsb-error)))
      (should (eq (nth 1 err) 'not-found)))
    (should-error (orsb-core-resolve "Not A Node") :type 'orsb-error)))

(ert-deftest orsb-core-delete-property-with-null ()
  (orsb-test-with-vault
    (let ((node (orsb-core-resolve "1784854105")))
      (orsb-core-set-properties node '(("NEXT-ACTION" . :json-null)))
      (should-not (assoc "NEXT-ACTION" (orsb-core-node-properties node)))
      (should (string-match-p "^:STATUS: +active$" (orsb-test--file "projects/project-backlog-1784854105.org"))))))

(ert-deftest orsb-core-refuses-hidden-and-bad-keys ()
  (orsb-test-with-vault
    (let ((node (orsb-core-resolve "1784854105")))
      (should-error (orsb-core-set-properties node '(("ID" . "x"))) :type 'orsb-error)
      (should-error (orsb-core-set-properties node '(("EMBEDDING_HASH" . "x"))) :type 'orsb-error)
      (should-error (orsb-core-set-properties node '(("bad key" . "x"))) :type 'orsb-error))))

;;;; heading-level properties

(ert-deftest orsb-core-heading-node-properties-round-trip ()
  (orsb-test-with-vault
    (let ((node (orsb-core-resolve "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1")))
      (should-not (assoc "EMBEDDING" (orsb-core-node-properties node)))
      (orsb-core-set-properties node '(("OWNER" . "kai")))
      (let ((text (orsb-test--file "projects/project-backlog-1784854105.org")))
        (should (string-match-p "^:OWNER: +kai$" text))
        ;; exactly one :ID: line for that heading, drawer intact
        (should (= 2 (length (split-string text "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1"))))
        (should (string-match-p ":EMBEDDING: 0.1 0.2 0.3" text)))
      (should (equal (orsb-core-node-property node "owner") "kai"))
      ;; the file-level drawer is untouched
      (should (equal (orsb-core-node-property (orsb-core-resolve "1784854105") "STATUS") "active")))))

;;;; keywords and body

(ert-deftest orsb-core-keywords-are-separate-from-properties ()
  (orsb-test-with-vault
    (let ((node (orsb-core-resolve "1784854105")))
      (should (equal (cdr (assoc "TITLE" (orsb-core-node-keywords node))) "Project Backlog"))
      (orsb-core-set-keywords node '(("hugo_draft" . "true")))
      (should (string-match-p "^#\\+HUGO_DRAFT: true$" (orsb-test--file "projects/project-backlog-1784854105.org")))
      (should-not (assoc "HUGO_DRAFT" (orsb-core-node-properties node)))
      (should-error (orsb-core-set-keywords (orsb-core-resolve "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1") '(("X" . "y"))) :type 'orsb-error))))

(ert-deftest orsb-core-body-read-and-replace ()
  (orsb-test-with-vault
    (let ((heading (orsb-core-resolve "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1"))
          (file (orsb-core-resolve "1784854105")))
      (should (equal (string-trim (orsb-core-node-body heading)) "Central index of everything I want to build."))
      (should-not (string-match-p "EMBEDDING" (orsb-core-node-body heading)))
      (should (string-match-p "\\`\\* Next Actions" (orsb-core-node-body file)))
      (orsb-core-set-body heading "New purpose.")
      (let ((text (orsb-test--file "projects/project-backlog-1784854105.org")))
        (should (string-match-p "^\\* Purpose\n:PROPERTIES:\n:ID: +c169b8b9[^\n]*\n:EMBEDDING: 0.1 0.2 0.3\n:END:\nNew purpose.\n" text))
        (should-not (string-match-p "Central index" text))))))

;;;; through the JSON API layer (what the MCP server calls)

(ert-deftest orsb-api-update-node-changes-file-level-status ()
  (orsb-test-with-vault
    (let ((r (orsb-test--json (my/api-update-node "1784854105" nil '((STATUS . "done"))))))
      (should (eq (alist-get 'success r) t))
      (should (equal (alist-get 'STATUS (alist-get 'properties r)) "done")))
    (let ((p (orsb-test--json (my/api-get-note-properties "1784854105"))))
      (should (equal (alist-get 'status p) "done"))
      (should (equal (alist-get 'node_type p) "project"))
      (should (equal (alist-get 'NEXT-ACTION (alist-get 'properties p)) "Phase 4 status curation.")))
    (let ((n (orsb-test--json (my/api-read-node "1784854105"))))
      (should (equal (alist-get 'STATUS (alist-get 'properties n)) "done"))
      (should (equal (alist-get 'TITLE (alist-get 'keywords n)) "Project Backlog"))
      (should (= (alist-get 'level n) 0)))))

(ert-deftest orsb-api-update-node-unknown-id-fails-cleanly ()
  (orsb-test-with-vault
    (let ((r (orsb-test--json (my/api-update-node "nope" nil '((STATUS . "done"))))))
      (should (eq (alist-get 'success r) :json-false))
      (should (string-match-p "nope" (alist-get 'error r))))))

(ert-deftest orsb-api-update-note-append-and-guard ()
  (orsb-test-with-vault
    (let ((r (orsb-test--json (my/api-update-note "1784854105" "- [ ] new item" "Next Actions" "append" nil))))
      (should (eq (alist-get 'success r) t)))
    (should (string-match-p "- \\[ \\] Phase 4 status curation.\n- \\[ \\] new item" (orsb-test--file "projects/project-backlog-1784854105.org")))
    ;; a whole-file replace that drops headings is refused without force
    (let ((r (orsb-test--json (my/api-update-note "1784854105" "gone" nil "replace" nil))))
      (should (eq (alist-get 'success r) :json-false))
      (should (string-match-p "Purpose" (orsb-test--file "projects/project-backlog-1784854105.org"))))))

(provide 'orsb-core-test)
;;; orsb-core-test.el ends here
