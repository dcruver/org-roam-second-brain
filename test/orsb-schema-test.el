;;; orsb-schema-test.el --- ERT tests for orsb-schema -*- lexical-binding: t; -*-

;;; Commentary:
;; Plain files in a throwaway folder: the schema reader needs no org-roam.

;;; Code:

(require 'ert)
(require 'orsb-schema)

(defconst orsb-schema-test--root-file "\
#+title: Schema: every note

* Type
:PROPERTIES:
:TYPE: note
:WRITE: open
:EXCLUDE: archive
:NOTES_HERE: no
:END:
Rules every note follows.

* Properties
| property | required | form                               |
|----------+----------+------------------------------------|
| ID       | yes      | id                                 |
| PARENT   | no       | link: the note this one belongs to |
")

(defconst orsb-schema-test--project-file "\
* Type
:PROPERTIES:
:TYPE: project
:NOTES_HERE: yes
:LIFECYCLE: STATUS
:LIFECYCLE_VALUES: active waiting done
:LIFECYCLE_FINISHED: done
:END:

* Properties
| property    | required | form                    |
|-------------+----------+-------------------------|
| STATUS      | yes      | one of LIFECYCLE_VALUES |
| NEXT-ACTION | yes      | one sentence            |

* Sections
| section       | required | rule    |
|---------------+----------+---------|
| Current state | yes      | replace |
| Log           | yes      | append  |

* Template
** Current state
As of %DATE%.
** Log
")

(defconst orsb-schema-test--blog-file "\
* Type
:PROPERTIES:
:TYPE: blog
:LIFECYCLE_VALUES: idea draft published
:END:

* Properties
| property    | required | form                        |
|-------------+----------+-----------------------------|
| NEXT-ACTION | remove   |                             |
| IDEA        | no       | link to a note of type idea |

* Sections
| section       | required | rule |
|---------------+----------+------|
| Current state | remove   |      |
| Log           | remove   |      |

* Template
")

(defconst orsb-schema-test--idea-file "\
* Type
:PROPERTIES:
:TYPE: idea
:END:

* Properties
| property    | required | form |
|-------------+----------+------|
| NEXT-ACTION | no       | text |

* Sections
| section       | required | rule |
|---------------+----------+------|
| Current state | remove   |      |
| Log           | remove   |      |
")

(defun orsb-schema-test--write (root rel content)
  "Write CONTENT to REL under ROOT, creating folders."
  (let ((file (expand-file-name rel root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert content))
    file))

(defun orsb-schema-test--note (id &rest props)
  "Return the text of a note with ID, the PROPS plist as its drawer, and two sections."
  (concat ":PROPERTIES:\n:ID: " id "\n"
          (let (out) (while props (push (format ":%s: %s\n" (pop props) (pop props)) out))
               (apply #'concat (nreverse out)))
          ":END:\n#+title: " id "\n\n* Current state\nx\n* Log :tag:\ny\n"))

(defmacro orsb-schema-test--with-vault (&rest body)
  "Run BODY with `root' bound to a vault holding the test schema files."
  (declare (indent 0))
  `(let ((root (file-name-as-directory (make-temp-file "orsb-schema-" t))))
     (unwind-protect
         (progn
           (orsb-schema-test--write root "_schema.org" orsb-schema-test--root-file)
           (orsb-schema-test--write root "projects/_schema.org" orsb-schema-test--project-file)
           (orsb-schema-test--write root "projects/blog/_schema.org" orsb-schema-test--blog-file)
           (orsb-schema-test--write root "projects/ideas/_schema.org" orsb-schema-test--idea-file)
           ,@body)
       (delete-directory root t))))

(ert-deftest orsb-schema-parses-the-four-parts ()
  (let ((s (orsb-schema-parse-string orsb-schema-test--project-file)))
    (should (equal (cdr (assoc "TYPE" (alist-get 'type s))) "project"))
    (should (equal (alist-get 'properties s)
                   '(("STATUS" "yes" "one of LIFECYCLE_VALUES") ("NEXT-ACTION" "yes" "one sentence"))))
    (should (equal (mapcar #'car (alist-get 'sections s)) '("Current state" "Log")))
    (should (equal (alist-get 'template s) "* Current state\nAs of %DATE%.\n* Log")))
  ;; no Template heading: no template key, so a parent's template survives a merge
  (should-not (assq 'template (orsb-schema-parse-string orsb-schema-test--idea-file)))
  ;; an empty Template heading: an empty template, which overrides
  (should (equal (alist-get 'template (orsb-schema-parse-string orsb-schema-test--blog-file)) "")))

(ert-deftest orsb-schema-new-body-puts-the-text-where-the-template-says ()
  (let ((plain '((template . "* Current state\nAs of %DATE%.\n* Log")))
        (placed '((template . "* Draft\n%BODY%\n* Research"))))
    (should (string-match-p "\\`Lead\\.\n\n\\* Current state\nAs of [0-9-]+\\.\n\\* Log\\'" (orsb-schema-new-body plain "T" "Lead.\n")))
    (should (equal (orsb-schema-new-body placed "T" "The post.") "* Draft\nThe post.\n* Research"))
    (should (equal (orsb-schema-new-body placed "T" nil) "* Draft\n\n* Research"))
    (should (equal (orsb-schema-new-body nil "T" "Just text") "Just text"))
    (should-not (orsb-schema-new-body nil "T" nil))))

(ert-deftest orsb-schema-a-folder-inherits-and-overrides ()
  (orsb-schema-test--with-vault
    (let ((project (orsb-schema-for-directory (expand-file-name "projects" root) root))
          (blog (orsb-schema-for-directory (expand-file-name "projects/blog" root) root)))
      ;; inherited from the root
      (should (equal (orsb-schema-get project "WRITE") "open"))
      (should (equal (mapcar #'car (alist-get 'properties project)) '("ID" "PARENT" "STATUS" "NEXT-ACTION")))
      (should (equal (alist-get 'types project) '("note" "project")))
      ;; blog: a key replaced, a row removed, a row added, sections and template cleared
      (should (equal (orsb-schema-get blog "LIFECYCLE_VALUES") "idea draft published"))
      (should (equal (orsb-schema-get blog "LIFECYCLE") "STATUS"))
      (should (equal (mapcar #'car (alist-get 'properties blog)) '("ID" "PARENT" "STATUS" "IDEA")))
      (should-not (alist-get 'sections blog))
      (should (equal (alist-get 'template blog) ""))
      (should (equal (alist-get 'types blog) '("note" "project" "blog"))))))

(ert-deftest orsb-schema-a-folder-without-a-file-is-grouping ()
  (orsb-schema-test--with-vault
    (make-directory (expand-file-name "projects/2026" root) t)
    (let ((s (orsb-schema-for-directory (expand-file-name "projects/2026" root) root)))
      (should (equal (orsb-schema-get s "TYPE") "project"))
      (should (equal (alist-get 'type-directory s) "projects"))
      (should-not (alist-get 'own-file s)))))

(ert-deftest orsb-schema-tree-lists-the-types ()
  (orsb-schema-test--with-vault
    (orsb-schema-test--write root "archive/_schema.org" "* Type\n:PROPERTIES:\n:TYPE: ghost\n:END:\n")
    (let ((tree (orsb-schema-tree root)))
      (should (equal (mapcar (lambda (e) (alist-get 'type e)) tree) '("note" "project" "blog" "idea")))
      (should (equal (alist-get 'parent (nth 2 tree)) "project"))
      (should (equal (alist-get 'directory (nth 2 tree)) "projects/blog"))
      (should (equal (alist-get 'lifecycle (nth 1 tree)) "STATUS")))
    (should (equal (orsb-schema-type-directory "idea" root)
                   (file-name-as-directory (expand-file-name "projects/ideas" root))))))

(ert-deftest orsb-schema-check-reports-what-departs ()
  (orsb-schema-test--with-vault
    ;; fine
    (orsb-schema-test--write root "projects/good.org"
                             (orsb-schema-test--note "p-good" "STATUS" "Active" "NEXT-ACTION" "ship it"))
    ;; wrong status, no next action, a missing section
    (orsb-schema-test--write root "projects/bad.org"
                             ":PROPERTIES:\n:ID: p-bad\n:STATUS: paused\n:END:\n#+title: bad\n\n* Log\n")
    ;; sits in the root, where no note may be; has no ID
    (orsb-schema-test--write root "stray.org" "#+title: stray\n")
    ;; a folder nothing gives a type to
    (orsb-schema-test--write root "misc/thing.org" (orsb-schema-test--note "m-thing"))
    ;; grouping folder under projects: same rules as projects
    (orsb-schema-test--write root "projects/2026/sub.org"
                             (orsb-schema-test--note "p-sub" "STATUS" "done" "NEXT-ACTION" "none"))
    ;; an idea, and blog posts linking to it, to a project, and to nothing
    (orsb-schema-test--write root "projects/ideas/seed.org" (orsb-schema-test--note "i-seed" "STATUS" "waiting"))
    (orsb-schema-test--write root "projects/blog/ok.org"
                             (orsb-schema-test--note "b-ok" "STATUS" "draft" "IDEA" "[[id:i-seed][Seed]]"))
    (orsb-schema-test--write root "projects/blog/wrong.org"
                             (orsb-schema-test--note "b-wrong" "STATUS" "draft" "IDEA" "[[id:p-good][Good]]"))
    (orsb-schema-test--write root "projects/blog/dangling.org"
                             (orsb-schema-test--note "b-dangling" "STATUS" "active" "IDEA" "nowhere"))
    ;; two notes with one id: org-roam would only ever see one of them
    (orsb-schema-test--write root "projects/twin-a.org" (orsb-schema-test--note "twin" "STATUS" "done" "NEXT-ACTION" "x"))
    (orsb-schema-test--write root "projects/twin-b.org" (orsb-schema-test--note "twin" "STATUS" "done" "NEXT-ACTION" "x"))
    ;; never looked at
    (orsb-schema-test--write root "archive/old.org" "#+title: old\n")
    (orsb-schema-test--write root "projects/x.sync-conflict-1.org" "#+title: conflict\n")
    (let* ((report (orsb-schema-check root))
           (of (lambda (file)
                 (mapcar (lambda (v) (cons (alist-get 'rule v) (alist-get 'detail v)))
                         (seq-filter (lambda (v) (equal (alist-get 'file v) file))
                                     (alist-get 'violations report))))))
      (should (= (alist-get 'notes report) 11))
      (should (equal (funcall of "projects/twin-a.org") '(("duplicate-id" . "twin (also in projects/twin-b.org)"))))
      (should (equal (mapcar #'car (funcall of "projects/twin-b.org")) '("duplicate-id")))
      (should-not (funcall of "projects/good.org"))
      (should-not (funcall of "projects/2026/sub.org"))
      (should-not (funcall of "projects/ideas/seed.org"))
      (should-not (funcall of "projects/blog/ok.org"))
      (should (equal (mapcar #'car (funcall of "projects/bad.org"))
                     '("bad-value" "missing-property" "missing-section")))
      (should (string-match-p "paused" (cdr (car (funcall of "projects/bad.org")))))
      (should (equal (funcall of "stray.org")
                     '(("misplaced" . "no note may sit directly in this folder") ("missing-property" . "ID"))))
      (should (equal (mapcar #'car (funcall of "misc/thing.org")) '("untyped-folder")))
      (should (equal (funcall of "projects/blog/wrong.org") '(("bad-value" . "IDEA: links to a project, not a idea"))))
      (should (equal (mapcar #'car (funcall of "projects/blog/dangling.org")) '("bad-value" "bad-value")))
      (should (= (alist-get 'with_violations report) 7))
      (should (equal (cdr (assoc "blog" (alist-get 'by_type report))) 3))
      (should (equal (cdr (assoc "bad-value" (alist-get 'by_rule report))) 4)))))

(ert-deftest orsb-schema-a-vault-without-schema-files-has-no-rules ()
  (let ((root (file-name-as-directory (make-temp-file "orsb-schema-" t))))
    (unwind-protect
        (progn
          (orsb-schema-test--write root "a.org" "#+title: a\n")
          (should-not (orsb-schema-present-p root))
          (should-not (orsb-schema-tree root))
          (let ((report (orsb-schema-check root)))
            (should (= (alist-get 'notes report) 1))
            (should (= (alist-get 'with_violations report) 0))))
      (delete-directory root t))))

(provide 'orsb-schema-test)
;;; orsb-schema-test.el ends here
