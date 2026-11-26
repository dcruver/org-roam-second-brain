;;; orsb-schema.el --- Note types and their rules, read from the vault -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 3.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:

;; A vault describes its own note types.  This module holds no type of its
;; own: everything comes from files named `orsb-schema-file-name'
;; (_schema.org) inside the vault.
;;
;; - A folder is a type when its _schema.org sets TYPE.  A folder without
;;   such a file is grouping only and has the type of the folder above.
;; - The schema of a folder is the vault root's file, then each _schema.org
;;   on the way down, merged in that order: a lower file's entry replaces
;;   the same entry from above, new entries are added, and a table row whose
;;   second column is "remove" deletes an inherited row.
;; - So a note's type is where it lives.  Folders say what a note IS A KIND
;;   OF.  What a note has or belongs to is a property whose form is a link.
;;
;; A _schema.org has up to four top-level headings, all optional:
;;
;;   * Type        a property drawer (TYPE, WRITE, NOTES_HERE, EXCLUDE,
;;                 LIFECYCLE, ... any key), then free text
;;   * Properties  a table: property | required | form
;;   * Sections    a table: section | required | rule
;;   * Template    the body a new note of this type starts with
;;
;; The file has no org-roam :ID:, so it is not a note.
;;
;; `orsb-schema-check' reads every note and reports where the vault departs
;; from its own rules.  It reads files only; it needs no org-roam database
;; and changes nothing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup orsb-schema nil
  "Note types and rules read from the vault."
  :group 'org-roam)

(defcustom orsb-schema-file-name "_schema.org"
  "Name of the file that gives a folder its type and rules."
  :type 'string
  :group 'orsb-schema)

(defcustom orsb-schema-ignore-regexp "\\.sync-conflict-\\|\\.bak\\(?:-\\|\\'\\)\\|\\`\\.#"
  "Note files whose name matches this are not checked."
  :type 'regexp
  :group 'orsb-schema)

(defvar org-roam-directory)

(defun orsb-schema--root (&optional root)
  "Return ROOT, or the org-roam vault, as a directory name."
  (file-name-as-directory
   (expand-file-name (or root
                         (and (boundp 'org-roam-directory) org-roam-directory)
                         (error "No vault: pass ROOT or set `org-roam-directory'")))))

;;;; Reading one file

(defun orsb-schema--sections (text)
  "Split TEXT into its top-level headings: an alist of (lowercase title . body)."
  (let ((case-fold-search nil) (pos 0) starts out)
    (while (string-match "^\\* +\\(.*\\)$" text pos)
      (push (list (match-beginning 0) (match-end 0) (string-trim (match-string 1 text))) starts)
      (setq pos (match-end 0)))
    (setq starts (nreverse starts))
    (while starts
      (let* ((cur (pop starts))
             (end (if starts (car (car starts)) (length text))))
        (push (cons (downcase (nth 2 cur)) (substring text (nth 1 cur) end)) out)))
    (nreverse out)))

(defun orsb-schema--drawer (body)
  "Return the property drawer of BODY as an alist of (KEY . value), keys upcased."
  (when (string-match "^[ \t]*:PROPERTIES:[ \t]*\n\\(\\(?:.*\n\\)*?\\)[ \t]*:END:" body)
    (let ((drawer (match-string 1 body)) (pos 0) out)
      (while (string-match "^[ \t]*:\\([^: \t\n]+\\):[ \t]*\\(.*\\)$" drawer pos)
        (push (cons (upcase (match-string 1 drawer)) (string-trim (match-string 2 drawer))) out)
        (setq pos (match-end 0)))
      (nreverse out))))

(defun orsb-schema--table (body)
  "Return the rows of the first table in BODY, without rules and the header row.
Each row is a list of trimmed cell strings."
  (let (rows started done)
    (dolist (line (split-string body "\n"))
      (unless done
        (cond ((string-match-p "\\`[ \t]*|" line)
               (setq started t)
               (unless (string-match-p "\\`[ \t]*|[-+|]*\\'" line)
                 (let ((cells (split-string (string-trim line) "|")))
                   ;; a line "| a | b |" splits to ("" " a " " b " "")
                   (push (mapcar #'string-trim (butlast (cdr cells))) rows))))
              (started (setq done t)))))
    (cdr (nreverse rows))))

(defun orsb-schema--template (body)
  "Return BODY as a note body: its headings promoted one level."
  (string-trim (replace-regexp-in-string "^\\*\\(\\*+ \\)" "\\1" body)))

(defun orsb-schema-parse-string (text)
  "Parse the contents TEXT of a schema file.
Return an alist with keys `type' (drawer alist), `properties' and `sections'
\(lists of rows) and, only when the file has a Template heading, `template'."
  (let ((sections (orsb-schema--sections text)) out)
    (when-let* ((b (cdr (assoc "type" sections))))
      (push (cons 'type (orsb-schema--drawer b)) out))
    (when-let* ((b (cdr (assoc "properties" sections))))
      (push (cons 'properties (orsb-schema--table b)) out))
    (when-let* ((b (cdr (assoc "sections" sections))))
      (push (cons 'sections (orsb-schema--table b)) out))
    (when-let* ((cell (assoc "template" sections)))
      (push (cons 'template (orsb-schema--template (cdr cell))) out))
    (nreverse out)))

(defun orsb-schema-parse-file (file)
  "Parse the schema FILE; nil when it does not exist."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (orsb-schema-parse-string (buffer-string)))))

;;;; Merging

(defun orsb-schema--merge-alist (parent child)
  "Return PARENT with each key of CHILD replaced or added."
  (let ((out (copy-sequence parent)))
    (dolist (cell child)
      (if-let* ((old (assoc (car cell) out)))
          (setq out (mapcar (lambda (c) (if (eq c old) cell c)) out))
        (setq out (append out (list cell)))))
    out))

(defun orsb-schema--row= (a b)
  "Non-nil when rows A and B name the same thing (first column, case ignored)."
  (string= (downcase (car a)) (downcase (car b))))

(defun orsb-schema--merge-rows (parent child)
  "Return the rows of PARENT overridden by CHILD.
A CHILD row replaces the PARENT row with the same first column, in place; a
new one is appended; one whose second column is \"remove\" deletes it."
  (let ((out (copy-sequence parent)))
    (dolist (row child)
      (let ((remove (string= (downcase (or (nth 1 row) "")) "remove"))
            (old (seq-find (lambda (r) (orsb-schema--row= r row)) out)))
        (cond (remove (setq out (seq-remove (lambda (r) (orsb-schema--row= r row)) out)))
              (old (setq out (mapcar (lambda (r) (if (eq r old) row r)) out)))
              (t (setq out (append out (list row)))))))
    out))

(defun orsb-schema-merge (parent child)
  "Return schema PARENT with schema CHILD's overrides and additions applied."
  (let ((out (list (cons 'type (orsb-schema--merge-alist (alist-get 'type parent) (alist-get 'type child)))
                   (cons 'properties (orsb-schema--merge-rows (alist-get 'properties parent)
                                                              (alist-get 'properties child)))
                   (cons 'sections (orsb-schema--merge-rows (alist-get 'sections parent)
                                                            (alist-get 'sections child))))))
    (if-let* ((tpl (or (assq 'template child) (assq 'template parent))))
        (append out (list (cons 'template (cdr tpl))))
      out)))

;;;; The schema of a folder

(defvar orsb-schema--cache nil
  "While non-nil, a hash table of directory -> resolved schema for one run.")

(defmacro orsb-schema--with-cache (&rest body)
  "Run BODY reading each schema file at most once."
  (declare (indent 0))
  `(let ((orsb-schema--cache (or orsb-schema--cache (make-hash-table :test #'equal))))
     ,@body))

(defun orsb-schema-get (schema key)
  "Return the value of the Type drawer KEY in SCHEMA."
  (cdr (assoc key (alist-get 'type schema))))

(defun orsb-schema--relative (dir root)
  "Return DIR relative to ROOT without a trailing slash; \"\" for ROOT itself."
  (directory-file-name (file-relative-name (file-name-as-directory dir) root)))

(defun orsb-schema--rel (dir root)
  "Like `orsb-schema--relative', but \"\" instead of \".\" for the root."
  (let ((r (orsb-schema--relative dir root)))
    (if (string= r ".") "" r)))

(defun orsb-schema-for-directory (dir &optional root)
  "Return the resolved schema for notes in DIR of the vault ROOT.
Besides `type', `properties', `sections' and `template' the result carries
`directory' (DIR relative to the vault), `type-directory' (the folder that
set TYPE), `types' (the chain of types from the root down) and `own-file'
\(non-nil when DIR has its own schema file)."
  (let* ((root (orsb-schema--root root))
         (dir (file-name-as-directory (expand-file-name dir)))
         (rel (orsb-schema--rel dir root)))
    (or (and orsb-schema--cache (gethash dir orsb-schema--cache))
        (let* ((own (orsb-schema-parse-file (expand-file-name orsb-schema-file-name dir)))
               (parent (unless (string= rel "")
                         (orsb-schema-for-directory (file-name-directory (directory-file-name dir)) root)))
               (merged (orsb-schema-merge parent own))
               (own-type (cdr (assoc "TYPE" (alist-get 'type own))))
               (types (let ((chain (alist-get 'types parent)))
                        (if (and own-type (not (equal own-type (car (last chain)))))
                            (append chain (list own-type))
                          chain)))
               (schema (append merged
                               (list (cons 'directory rel)
                                     (cons 'type-directory (if own-type rel (alist-get 'type-directory parent)))
                                     (cons 'types types)
                                     (cons 'own-file (and own t))))))
          (when orsb-schema--cache (puthash dir schema orsb-schema--cache))
          schema))))

(defun orsb-schema-for-file (file &optional root)
  "Return the resolved schema that applies to the note FILE."
  (orsb-schema-for-directory (file-name-directory (expand-file-name file)) root))

(defun orsb-schema-present-p (&optional root)
  "Non-nil when the vault ROOT has a schema file at its top."
  (file-readable-p (expand-file-name orsb-schema-file-name (orsb-schema--root root))))

;;;; Walking the vault

(defun orsb-schema--excluded (schema)
  "Return the folders SCHEMA's EXCLUDE names, as paths relative to the vault."
  (let ((base (alist-get 'directory schema)))
    (mapcar (lambda (name) (if (string= base "") name (concat base "/" name)))
            (split-string (or (orsb-schema-get schema "EXCLUDE") "") "[ ,]+" t))))

(defun orsb-schema--walk (root fn)
  "Call FN with each folder of the vault ROOT that is not hidden or excluded.
EXCLUDE is read from each folder's own schema file and names its subfolders."
  (let ((root (orsb-schema--root root)))
    (cl-labels
        ((visit (dir)
           (funcall fn dir)
           (let* ((own (orsb-schema-parse-file (expand-file-name orsb-schema-file-name dir)))
                  (skip (split-string (or (cdr (assoc "EXCLUDE" (alist-get 'type own))) "") "[ ,]+" t)))
             (dolist (sub (directory-files dir t "\\`[^.]"))
               (when (and (file-directory-p sub)
                          (not (member (file-name-nondirectory sub) skip)))
                 (visit (file-name-as-directory sub)))))))
      (visit root))))

(defun orsb-schema-tree (&optional root)
  "Return the types the vault ROOT defines, in folder order.
Each is an alist: type, directory, parent (type or nil), write, lifecycle."
  (let ((root (orsb-schema--root root)) out)
    (orsb-schema--with-cache
      (orsb-schema--walk
       root
       (lambda (dir)
         (let ((schema (orsb-schema-for-directory dir root)))
           (when (and (alist-get 'own-file schema)
                      (equal (alist-get 'type-directory schema) (alist-get 'directory schema)))
             (let ((chain (alist-get 'types schema)))
               (push `((type . ,(orsb-schema-get schema "TYPE"))
                       (directory . ,(alist-get 'directory schema))
                       (parent . ,(car (last (butlast chain))))
                       (write . ,(or (orsb-schema-get schema "WRITE") "open"))
                       (lifecycle . ,(orsb-schema-get schema "LIFECYCLE")))
                     out)))))))
    (nreverse out)))

(defun orsb-schema-type-directory (type &optional root)
  "Return the absolute folder of TYPE in the vault ROOT, or nil."
  (let ((root (orsb-schema--root root)))
    (when-let* ((entry (seq-find (lambda (e) (equal (alist-get 'type e) type)) (orsb-schema-tree root))))
      (file-name-as-directory (expand-file-name (alist-get 'directory entry) root)))))

;;;; Reading a note for the check

(defun orsb-schema--note-facts (file)
  "Return what the check needs from the note FILE.
An alist: `properties' (file-level drawer, keys upcased), `headings'
\(top-level heading titles, lowercase, without tags) and `ids' (every :ID:)."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((text (buffer-string))
           (case-fold-search t)
           (props (when (string-match "\\`\\(?:[ \t]*\n\\)*[ \t]*:PROPERTIES:[ \t]*\n\\(\\(?:.*\n\\)*?\\)[ \t]*:END:" text)
                    (orsb-schema--drawer (match-string 0 text))))
           (pos 0) headings ids)
      (let ((case-fold-search nil))
        (while (string-match "^\\* +\\(.*\\)$" text pos)
          (push (downcase (string-trim (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" ""
                                                                 (match-string 1 text))))
                headings)
          (setq pos (match-end 0))))
      (setq pos 0)
      (while (string-match "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" text pos)
        (push (match-string 1 text) ids)
        (setq pos (match-end 0)))
      `((properties . ,props) (headings . ,(nreverse headings)) (ids . ,(nreverse ids))))))

(defun orsb-schema--note-files (root)
  "Return every note file of the vault ROOT that the check covers."
  (let (out)
    (orsb-schema--walk
     root
     (lambda (dir)
       (dolist (f (directory-files dir t "\\.org\\'"))
         (let ((name (file-name-nondirectory f)))
           (unless (or (string= name orsb-schema-file-name)
                       (string-match-p orsb-schema-ignore-regexp name)
                       (file-directory-p f))
             (push f out))))))
    (nreverse out)))

;;;; Forms

(defun orsb-schema--link-id (value)
  "Return the note id VALUE points to: an [[id:...]] link, id:..., or a bare id."
  (cond ((string-match "\\[\\[id:\\([^]]+\\)\\]" value) (match-string 1 value))
        ((string-match "\\`id:\\(\\S-+\\)\\'" value) (match-string 1 value))
        ((string-match-p "\\`\\S-+\\'" value) value)))

(defun orsb-schema--form-error (form value schema ids root)
  "Return why VALUE does not fit FORM, or nil when it does.
SCHEMA resolves \"one of KEY\"; IDS maps a note id to its file, for links
in the vault ROOT.  A form this function does not know accepts any value."
  (let ((f (downcase (string-trim (or form "")))))
    (cond
     ((string-prefix-p "one of " f)
      (let* ((rest (string-trim (substring (string-trim form) 7)))
             (allowed (split-string (or (orsb-schema-get schema (upcase rest)) rest) "[ ,]+" t)))
        (unless (member (downcase value) (mapcar #'downcase allowed))
          (format "\"%s\" is not one of: %s" value (string-join allowed ", ")))))
     ((string= f "four digits")
      (unless (string-match-p "\\`[0-9]\\{4\\}\\'" value) (format "\"%s\" is not four digits" value)))
     ((string= f "url")
      (unless (string-match-p "\\`https?://" value) (format "\"%s\" is not a URL" value)))
     ((string-prefix-p "date" f)
      (unless (string-match-p "\\`[[<]?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" value)
        (format "\"%s\" is not a date" value)))
     ((string-prefix-p "link" f)
      (let* ((id (orsb-schema--link-id value))
             (target (and id (gethash id ids)))
             (want (and (string-match "\\btype +\\([^ .,]+\\)" f) (match-string 1 f))))
        (cond ((not target) (format "\"%s\" does not link to a note in the vault" value))
              ((and want
                    (not (member want (mapcar #'downcase
                                              (alist-get 'types (orsb-schema-for-file target root))))))
               (format "links to a %s, not a %s"
                       (or (car (last (alist-get 'types (orsb-schema-for-file target root)))) "note")
                       want))))))))

;;;; The check

(defun orsb-schema--yes (cell)
  "Non-nil when table CELL says yes."
  (member (downcase (or cell "")) '("yes" "y" "true" "required")))

(defun orsb-schema--check-note (file facts schema ids root)
  "Return the violations of note FILE with FACTS against SCHEMA.
IDS maps note ids to files in the vault ROOT.  Each is (RULE . DETAIL)."
  (let ((props (alist-get 'properties facts))
        (headings (alist-get 'headings facts))
        out)
    (when (and (string= (downcase (or (orsb-schema-get schema "NOTES_HERE") "yes")) "no")
               ;; NOTES_HERE speaks for the folder that says it, not for the folders below
               (let ((dir (file-name-directory file)))
                 (cdr (assoc "NOTES_HERE"
                             (alist-get 'type (orsb-schema-parse-file
                                               (expand-file-name orsb-schema-file-name dir)))))))
      (push (cons "misplaced" "no note may sit directly in this folder") out))
    (when (and (string= (or (alist-get 'type-directory schema) "") "")
               (not (string= (alist-get 'directory schema) "")))
      (push (cons "untyped-folder" "this folder has no type: no schema file here or above it sets TYPE") out))
    (dolist (row (alist-get 'properties schema))
      (let* ((key (upcase (car row)))
             (value (cdr (assoc key props))))
        (cond ((or (null value) (string-empty-p value))
               (when (orsb-schema--yes (nth 1 row))
                 (push (cons "missing-property" key) out)))
              (t (when-let* ((why (orsb-schema--form-error (nth 2 row) value schema ids root)))
                   (push (cons "bad-value" (format "%s: %s" key why)) out))))))
    (dolist (row (alist-get 'sections schema))
      (when (and (orsb-schema--yes (nth 1 row))
                 (not (member (downcase (car row)) headings)))
        (push (cons "missing-section" (car row)) out)))
    (nreverse out)))

(defun orsb-schema-check (&optional root)
  "Check every note of the vault ROOT against the vault's own schema files.
Return an alist: `notes' (count), `with_violations', `by_rule' and `by_type'
\(alists of counts), `types' (`orsb-schema-tree') and `violations', a list of
alists with file, type, rule and detail.  Nothing is changed."
  (let* ((root (orsb-schema--root root))
         (ids (make-hash-table :test #'equal))
         (by-rule nil) (by-type nil) (bad 0) violations facts-of)
    (orsb-schema--with-cache
      (let ((files (orsb-schema--note-files root)))
        (dolist (f files)
          (let ((facts (orsb-schema--note-facts f)))
            (push (cons f facts) facts-of)
            (dolist (id (alist-get 'ids facts)) (puthash id f ids))))
        (dolist (cell (nreverse facts-of))
          (let* ((f (car cell))
                 (schema (orsb-schema-for-file f root))
                 (type (or (orsb-schema-get schema "TYPE") "note"))
                 (found (orsb-schema--check-note f (cdr cell) schema ids root)))
            (cl-incf (alist-get type by-type 0 nil #'equal))
            (when found (cl-incf bad))
            (dolist (v found)
              (cl-incf (alist-get (car v) by-rule 0 nil #'equal))
              (push `((file . ,(file-relative-name f root)) (type . ,type)
                      (rule . ,(car v)) (detail . ,(cdr v)))
                    violations))))
        `((notes . ,(length files))
          (with_violations . ,bad)
          (by_rule . ,(sort by-rule (lambda (a b) (> (cdr a) (cdr b)))))
          (by_type . ,(sort by-type (lambda (a b) (> (cdr a) (cdr b)))))
          (types . ,(orsb-schema-tree root))
          (violations . ,(nreverse violations)))))))

(provide 'orsb-schema)
;;; orsb-schema.el ends here
