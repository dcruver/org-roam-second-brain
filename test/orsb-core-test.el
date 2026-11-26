;;; orsb-core-test.el --- ERT tests for orsb-core -*- lexical-binding: t; -*-

;;; Commentary:
;; Runtime tests against a throwaway vault: real files, a real org-roam db,
;; real buffer edits.  Each test gets a fresh vault via `orsb-test-with-vault'.

;;; Code:

(require 'ert)
(require 'org-roam)
(require 'orsb-core)
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
          (orsb-change-directory (expand-file-name ".orsb-changes" dir))
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

;;;; creation, sections, delete, inbox, follow-ups, blog (the former my/api-* logic)

(ert-deftest orsb-core-create-node-knows-no-types ()
  (orsb-test-with-vault
    ;; folder, properties, keyword lines, tags and body are all the caller's
    (let* ((n (orsb-core-create-node "Rebuild the shed"
                                     :dir (expand-file-name "work/" org-roam-directory)
                                     :drawer '(("STATUS" . "active") ("NEXT-ACTION" . "Buy lumber"))
                                     :keywords '(("date" . "[2026-11-25]"))
                                     :tags '("home")
                                     :body "* Plan\nNeeds a roof."))
           (text (orsb-test--file (file-relative-name (org-roam-node-file n) org-roam-directory))))
      (should (string-prefix-p (expand-file-name "work/" org-roam-directory) (org-roam-node-file n)))
      (should (equal (orsb-core-node-property n "NEXT-ACTION") "Buy lumber"))
      (should (equal (org-roam-node-tags n) '("home")))
      (should (string-match-p "\\`:PROPERTIES:\n:ID: [^\n]+\n:STATUS: active\n:NEXT-ACTION: Buy lumber\n:END:\n" text))
      (should (string-match-p "^#\\+filetags: :home:\n#\\+date: \\[2026-11-25\\]\n\n\\* Plan\nNeeds a roof\\." text))
      (should-not (string-match-p "NODE-TYPE" text)))
    ;; nothing but a title: a bare note in the vault's top folder
    (let ((n (orsb-core-create-node "Loose thought")))
      (should (equal (file-name-directory (org-roam-node-file n))
                     (file-name-as-directory (expand-file-name org-roam-directory)))))
    (should-error (orsb-core-create-node " ") :type 'orsb-error)))

(ert-deftest orsb-core-headings-sections-and-guarded-replace ()
  (orsb-test-with-vault
    (let* ((hub (orsb-core-resolve "1784854105"))
           (h (orsb-core-add-heading hub "Materials" "- lumber" '(("OWNER" . "don")) 2)))
      (should (= (org-roam-node-level h) 2))
      (should (equal (orsb-core-node-property h "OWNER") "don"))
      (should (equal (string-trim (orsb-core-node-body h)) "- lumber"))
      (orsb-core-update-section hub "Next Actions" "- [ ] appended")
      (orsb-core-update-section hub "Next Actions" "- [ ] first" "prepend")
      (should (string-match-p "\\`- \\[ \\] first\n- \\[ \\] Phase 4.*\n- \\[ \\] appended" (orsb-core-section-body hub "Next Actions")))
      (orsb-core-update-section hub "Brand New" "fresh" "replace")
      (should (equal (string-trim (orsb-core-section-body hub "Brand New")) "fresh"))
      ;; whole-file replace: refused when headings vanish, allowed with force
      (should-error (orsb-core-replace-file-body hub "gone") :type 'orsb-error)
      (should (string-match-p "Purpose" (orsb-test--file "projects/project-backlog-1784854105.org")))
      ;; headings kept but the note shrinks a lot: still refused without force
      (should-error (orsb-core-replace-file-body hub "* Purpose\n* Next Actions\n* Materials\n* Brand New\nrewritten\n") :type 'orsb-error)
      (orsb-core-replace-file-body hub "* Purpose\n* Next Actions\n* Materials\n* Brand New\nrewritten\n" t)
      (let ((text (orsb-test--file "projects/project-backlog-1784854105.org")))
        (should (string-match-p "rewritten" text))
        (should (string-match-p "\\`:PROPERTIES:\n:ID: 1784854105" text))
        (should (string-match-p "^#\\+title: Project Backlog$" text)))
      (should (seq-some (lambda (f) (string-match-p "\\.bak-" f))
                        (directory-files (expand-file-name "projects" org-roam-directory))))
      ;; with the backup switched off a replace leaves no further .bak file
      (let* ((dir (expand-file-name "projects" org-roam-directory))
             (count (lambda () (length (seq-filter (lambda (f) (string-match-p "\\.bak-" f)) (directory-files dir)))))
             (before (funcall count))
             (orsb-replace-backup nil))
        (sleep-for 1.1)
        (orsb-core-replace-file-body (orsb-core-resolve "1784854105") "* Purpose\n* Next Actions\n* Materials\n* Brand New\nagain\n" t)
        (should (string-match-p "again" (orsb-test--file "projects/project-backlog-1784854105.org")))
        (should (= before (funcall count)))))))

(ert-deftest orsb-core-delete-archive-and-link ()
  (orsb-test-with-vault
    (let* ((hub (orsb-core-resolve "1784854105"))
           (p (orsb-core-create-node "project" "Temp")))
      (orsb-core-link hub p "Next Actions")
      (should (string-match-p (concat "\\[\\[id:" (org-roam-node-id p) "\\]") (orsb-core-section-body hub "Next Actions")))
      (should (= 1 (orsb-core-unlink hub (org-roam-node-id p))))
      ;; delete a heading subtree
      (orsb-core-delete-node (orsb-core-resolve "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1"))
      (should-not (string-match-p "Purpose" (orsb-test--file "projects/project-backlog-1784854105.org")))
      (should-error (orsb-core-resolve "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1") :type 'orsb-error)
      ;; archive then delete a file
      (let ((target (orsb-core-delete-node p t)))
        (should (string-prefix-p (expand-file-name "archive/" org-roam-directory) target))
        (should (equal (org-roam-node-file (orsb-core-resolve (org-roam-node-id p))) target))
        (orsb-core-delete-node (orsb-core-resolve (org-roam-node-id p)))
        (should-not (file-exists-p target))
        (should-error (orsb-core-resolve (org-roam-node-id p)) :type 'orsb-error)))))

(ert-deftest orsb-core-daily-and-inbox ()
  (orsb-test-with-vault
    (let ((orsb-daily-directory "daily"))
      (orsb-core-add-daily-entry "Kai did a thing" '("one" "two") '("next") '("kai"))
      (let ((text (orsb-core-daily-content)))
        (should (string-match-p "^\\* [0-9][0-9]:[0-9][0-9] Kai did a thing    :kai:$" text))
        (should (string-match-p "- one\n- two\n\n\\*\\* Next Steps\n- \\[ \\] next" text)))
      ;; the daily note is a node
      (should (orsb-core-resolve (format-time-string "%Y-%m-%d")))
      ;; the inbox line keeps a [[Name]] as written and links the note; no note is made for the name
      (should-not (orsb-core-log-to-inbox "call [[Grace Hopper]] about the compiler" (orsb-core-resolve "1784854105")))
      (should-error (orsb-core-resolve "Grace Hopper") :type 'orsb-error)
      (should (string-match-p "^\\* Inbox\n- \\[[^]]+\\] call \\[\\[Grace Hopper\\]\\] about the compiler → \\[\\[id:1784854105\\]\\[Project Backlog\\]\\]$"
                              (orsb-core-daily-content)))
      ;; entries come back without the bullet dash
      (should (equal (cdr (car (orsb-core-inbox-entries 1)))
                     (list (string-remove-prefix "- " (car (last (split-string (orsb-core-daily-content) "\n" t))))))))))


;;;; change journal

(ert-deftest orsb-core-change-rolls-back-on-error ()
  (orsb-test-with-vault
    (let* ((file "projects/project-backlog-1784854105.org")
           (before (orsb-test--file file)))
      ;; a write followed by a failure: the file is put back, nothing journaled
      (should-error (orsb-core-call-with-change
                     "test" "half done" nil
                     (lambda ()
                       (orsb-core-append-body (orsb-core-resolve "1784854105") "half done")
                       (error "boom"))))
      (should (equal (orsb-test--file file) before))
      (should (null (orsb-core-changes)))
      ;; a nested call joins the outer change: one entry for both writes
      (let ((r (orsb-core-call-with-change
                "test" "two writes" nil
                (lambda ()
                  (orsb-core-append-body (orsb-core-resolve "1784854105") "one")
                  (orsb-core-call-with-change "inner" "x" nil
                                              (lambda () (orsb-core-append-body (orsb-core-resolve "1784854105") "two")))
                  'done))))
        (should (eq (car r) 'done))
        (should (= (length (orsb-core-changes)) 1))
        (orsb-core-restore (cdr r))
        (should (equal (orsb-test--file file) before))))))

;;;; an empty section keeps to itself

(ert-deftest orsb-core-empty-section-followed-by-a-heading ()
  "An empty section's body ends where the next heading starts.
Until 2.2.2 it was taken to be the next section: an append landed there and a
replace deleted it."
  (orsb-test-with-vault
    (orsb-test--write "misc/three.org" ":PROPERTIES:\n:ID: three\n:END:\n#+title: Three\n\n* A\nkeep A\n* B\n* C\nkeep C\n** C child\ndeep\n* D")
    (org-roam-db-sync)
    (let ((n (orsb-core-resolve "three")))
      (should (equal (orsb-core-section-body n "B") ""))
      (orsb-core-update-section n "B" "- entry" "append" t)
      (should (string-match-p "\\* B\n- entry\n\\* C\nkeep C\n\\*\\* C child\ndeep\n\\* D" (orsb-test--file "misc/three.org")))
      (orsb-core-update-section n "B" "wiped" "replace" t)
      (should (string-match-p "\\* A\nkeep A\n\\* B\nwiped\n\\* C\nkeep C\n\\*\\* C child\ndeep\n\\* D" (orsb-test--file "misc/three.org")))
      (orsb-core-update-section n "B" "first" "prepend" t)
      (should (string-match-p "\\* B\nfirst\nwiped\n\\* C\nkeep C" (orsb-test--file "misc/three.org")))
      ;; the last heading of the file, with no newline after it
      (orsb-core-update-section n "D" "tail" "append" t)
      (should (string-match-p "\\* D\ntail\n\\'" (orsb-test--file "misc/three.org")))
      (should (equal (string-trim (orsb-core-section-body n "C")) "keep C\n** C child\ndeep")))))

(provide 'orsb-core-test)
;;; orsb-core-test.el ends here
