;;; orsb-core.el --- Core domain logic for org-roam-second-brain -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1") (org-roam "2.2"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:

;; The single implementation of every second-brain capability.  Functions
;; here take and return Emacs Lisp data (nodes, alists, strings) and signal
;; `orsb-error' on failure.  They know nothing about JSON, MCP, keybindings
;; or interactive prompts: `org-roam-second-brain.el' (human UI) and
;; `org-roam-mcp-http.el' (MCP server) are thin adapters over this file.
;;
;; The property model: a node's metadata is its :PROPERTIES: drawer, the
;; file-level drawer for a file node (level 0) and the heading drawer for a
;; heading node.  It is read and written only through `org-entry-get' and
;; `org-entry-put' with point at the node, which is what Org itself does, so
;; a file node's :STATUS: is finally reachable.  #+KEYWORD: lines are a
;; separate, explicit thing (`orsb-core-node-keywords').

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-roam)
(require 'subr-x)
(require 'seq)

;;;; Errors

(define-error 'orsb-error "org-roam-second-brain error")

(defun orsb-error (code message &rest args)
  "Signal an `orsb-error' with CODE and formatted MESSAGE.
CODE is a symbol such as `not-found', `invalid-argument', `refused'
or `unavailable'; adapters map it to their own error vocabulary."
  (signal 'orsb-error (list code (apply #'format message args))))

;;;; Customization

(defgroup orsb nil
  "org-roam-second-brain."
  :group 'org-roam
  :prefix "orsb-")

(defcustom orsb-hidden-properties '("ID" "EMBEDDING")
  "Property keys (or prefixes) never returned to callers.
A key is hidden when it equals an entry or starts with an entry followed by
an underscore, so \"EMBEDDING\" hides EMBEDDING, EMBEDDING_HASH, ..."
  :type '(repeat string)
  :group 'orsb)

;;;; Compatibility

(defun orsb-core-string= (a b)
  "Case-insensitive string equality (Emacs 28 lacks `string-equal-ignore-case')."
  (eq t (compare-strings a nil nil b nil nil t)))

;;;; Resolving

(defun orsb-core--file-node (file)
  "Return the level-0 org-roam node for FILE, or nil."
  (when-let ((id (caar (org-roam-db-query
                        [:select [id] :from nodes
                         :where (and (= file $s1) (= level 0))]
                        file))))
    (org-roam-node-from-id id)))

(defun orsb-core--first-node-in-file (file)
  "Return any node for FILE, preferring the file-level one."
  (or (orsb-core--file-node file)
      (when-let ((id (caar (org-roam-db-query
                            [:select [id] :from nodes :where (= file $s1)
                             :order-by [(asc level)] :limit 1]
                            file))))
        (org-roam-node-from-id id))))

(defun orsb-core-resolve (identifier)
  "Return the org-roam node IDENTIFIER names, signalling `not-found' otherwise.
IDENTIFIER is tried as an org-roam id, then as an absolute path or a path
relative to `org-roam-directory', then as an exact title or alias."
  (unless (and (stringp identifier) (not (string-empty-p (string-trim identifier))))
    (orsb-error 'invalid-argument "id must be a non-empty string"))
  (let ((identifier (string-trim identifier)))
    (or (org-roam-node-from-id identifier)
        (let ((path (cond ((file-name-absolute-p identifier) identifier)
                          (t (expand-file-name identifier org-roam-directory)))))
          (when (file-exists-p path)
            (orsb-core--first-node-in-file (file-truename path))))
        (org-roam-node-from-title-or-alias identifier)
        (orsb-error 'not-found "No note or node matches %S" identifier))))

;;;; Buffer access

(defmacro orsb-core--with-node (node &rest body)
  "Run BODY in NODE's file buffer with point at the node, widened.
The buffer is left unsaved; call `orsb-core--after-write' when BODY changed it."
  (declare (indent 1) (debug t))
  (let ((n (make-symbol "node")))
    `(let ((,n ,node))
       (with-current-buffer (find-file-noselect (org-roam-node-file ,n))
         (unless (derived-mode-p 'org-mode) (org-mode))
         (org-with-wide-buffer
          (goto-char (if (= (org-roam-node-level ,n) 0)
                         (point-min)
                       (orsb-core--heading-point ,n)))
          ,@body)))))

(defun orsb-core--heading-point (node)
  "Return the position of NODE's heading in the current buffer.
Prefers the :ID: line so a stale db position cannot send us elsewhere."
  (let ((id (org-roam-node-id node)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id)) nil t)
          (progn (org-back-to-heading t) (point))
        (let ((pos (org-roam-node-point node)))
          (if (and pos (<= pos (point-max)))
              (progn (goto-char pos) (org-back-to-heading t) (point))
            (orsb-error 'not-found "Node %s is not in %s any more" id (buffer-file-name))))))))

(defun orsb-core--save-quietly ()
  "Write the current buffer to disk without running save hooks.
The hooks (embedding generation, toc-org) are slow and unsafe on the MCP
request path; the embedding refresh is queued separately."
  (let ((inhibit-read-only t))
    (write-region (point-min) (point-max) (buffer-file-name) nil :silent)
    ;; Record the new modtime, or the next edit asks about supersession
    ;; (a prompt that kills a batch/daemon session).
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)))

(defun orsb-core--after-write (file)
  "Persist FILE's buffer and bring the org-roam db up to date for it.
Every mutation ends here."
  (with-current-buffer (find-file-noselect file)
    (orsb-core--save-quietly))
  (org-roam-db-update-file file)
  ;; Embeddings are refreshed by the idle worker, never on the request path.
  (when (fboundp 'orsb-search-enqueue)
    (orsb-search-enqueue file)))

(defun orsb-core-db-refresh (&rest files)
  "Bring the org-roam db up to date for FILES only (no full scan).
A file that no longer exists is cleared from the db; nil entries are
ignored.  A full `org-roam-db-sync' walks every note and freezes Emacs for
seconds; on the MCP request path this is the replacement."
  (dolist (file files)
    (when file
      (let ((file (file-truename file)))
        (if (file-exists-p file)
            (org-roam-db-update-file file)
          (org-roam-db-clear-file file))))))

;;;; Properties

(defun orsb-core--hidden-key-p (key)
  "Non-nil when KEY should not be exposed to callers."
  (let ((k (upcase key)))
    (seq-some (lambda (h) (or (string= k h) (string-prefix-p (concat h "_") k)))
              orsb-hidden-properties)))

(defun orsb-core--normalize-key (key)
  "Return KEY as an upper-cased property name string."
  (let ((k (if (symbolp key) (symbol-name key) key)))
    (unless (and (stringp k) (string-match-p "\\`[A-Za-z0-9_-]+\\'" k))
      (orsb-error 'invalid-argument "Invalid property key %S" key))
    (upcase k)))

(defun orsb-core-node-properties (node)
  "Return NODE's drawer properties as an alist of (KEY . VALUE) strings.
Keys are upper-cased; hidden keys (see `orsb-hidden-properties') are omitted.
Inherited and special Org properties are not included."
  (orsb-core--with-node node
    (let (props)
      (dolist (cell (org-entry-properties nil 'standard))
        (let ((key (car cell)))
          (unless (or (orsb-core--hidden-key-p key)
                      (string-match-p "\\`\\(CATEGORY\\|BLOCKED\\|ITEM\\|FILE\\|TODO\\|PRIORITY\\|TAGS\\|ALLTAGS\\|TIMESTAMP\\|TIMESTAMP_IA\\|CLOCKSUM\\|CLOCKSUM_T\\|CLOSED\\|DEADLINE\\|SCHEDULED\\)\\'" key))
            (push (cons (upcase key) (string-trim (cdr cell))) props))))
      (nreverse props))))

(defun orsb-core-node-property (node key)
  "Return NODE's drawer property KEY (any case) as a string, or nil."
  (orsb-core--with-node node
    (org-entry-get nil (orsb-core--normalize-key key))))

(defun orsb-core-set-properties (node alist)
  "Write ALIST of (KEY . VALUE) into NODE's drawer and update the db.
Keys are upper-cased.  A VALUE of nil, :json-null or the empty string
deletes the property.  Values are written as strings.  Returns the node's
properties after the write."
  (unless (listp alist)
    (orsb-error 'invalid-argument "properties must be an object of key/value pairs"))
  (dolist (cell alist)
    (when (orsb-core--hidden-key-p (orsb-core--normalize-key (car cell)))
      (orsb-error 'refused "Property %s is managed by the system and cannot be set" (car cell))))
  (orsb-core--with-node node
    (dolist (cell alist)
      (let ((key (orsb-core--normalize-key (car cell)))
            (val (cdr cell)))
        (if (or (null val) (eq val :json-null) (and (stringp val) (string-empty-p val)))
            (org-entry-delete nil key)
          (org-entry-put nil key (if (stringp val) val (format "%s" val)))))))
  (orsb-core--after-write (org-roam-node-file node))
  (orsb-core-node-properties node))

;;;; Keywords (#+KEY: lines, file nodes only)

(defun orsb-core-node-keywords (node)
  "Return NODE's file's #+KEYWORD: lines as an alist of (KEY . VALUE).
Keys are upper-cased.  Empty for heading nodes."
  (when (= (org-roam-node-level node) 0)
    (orsb-core--with-node node
      (let (kws)
        (goto-char (point-min))
        (while (re-search-forward "^#\\+\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)$" nil t)
          (push (cons (upcase (match-string 1)) (string-trim (match-string 2))) kws))
        (nreverse kws)))))

(defun orsb-core-set-keywords (node alist)
  "Set #+KEYWORD: lines from ALIST on NODE's file (file nodes only).
A nil or empty value removes the line.  New lines go after #+title:."
  (unless (= (org-roam-node-level node) 0)
    (orsb-error 'invalid-argument "keywords can only be set on a file-level node"))
  (orsb-core--with-node node
    (dolist (cell alist)
      (let* ((key (orsb-core--normalize-key (car cell)))
             (val (cdr cell))
             (rx (format "^#\\+%s:.*$" (regexp-quote key))))
        (goto-char (point-min))
        (let ((found (let ((case-fold-search t)) (re-search-forward rx nil t))))
          (cond
           ((or (null val) (and (stringp val) (string-empty-p val)))
            (when found (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))))
           (found (replace-match (format "#+%s: %s" key val) t t))
           (t (goto-char (point-min))
              (if (let ((case-fold-search t)) (re-search-forward "^#\\+title:.*$" nil t))
                  (progn (end-of-line) (insert (format "\n#+%s: %s" key val)))
                (org-with-wide-buffer
                 (goto-char (point-min))
                 (when (looking-at "^[ \t]*:PROPERTIES:")
                   (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                   (forward-line 1))
                 (insert (format "#+%s: %s\n" key val))))))))))
  (orsb-core--after-write (org-roam-node-file node))
  (orsb-core-node-keywords node))

;;;; Body

(defun orsb-core--strip-hidden-property-lines (text)
  "Remove drawer lines for hidden properties from TEXT."
  (replace-regexp-in-string "^[ \t]*:EMBEDDING[^:]*:.*\n?" "" text))

(defun orsb-core--body-bounds ()
  "Return (START . END) of the body of the node at point.
For a file node the body starts after the file-level drawer and the
#+KEYWORD: block and runs to the end of the buffer."
  (save-excursion
    (if (org-before-first-heading-p)
        (progn
          (goto-char (point-min))
          (when (looking-at "^[ \t]*:PROPERTIES:")
            (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (forward-line 1))
          (while (looking-at "^#\\+") (forward-line 1))
          (while (and (not (eobp)) (looking-at "^[ \t]*$")) (forward-line 1))
          (cons (point) (point-max)))
      (org-back-to-heading t)
      (end-of-line)
      (forward-line 1)
      (when (looking-at "^[ \t]*:PROPERTIES:")
        (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
          (forward-line 1)))
      (cons (point) (save-excursion (org-end-of-subtree t t) (point))))))

(defun orsb-core-node-body (node)
  "Return NODE's body text (after its drawer/keywords), hidden lines stripped."
  (orsb-core--with-node node
    (let ((b (orsb-core--body-bounds)))
      (if (>= (car b) (cdr b))
          ""
        (orsb-core--strip-hidden-property-lines
         (buffer-substring-no-properties (car b) (cdr b)))))))

(defun orsb-core-set-body (node text)
  "Replace NODE's body with TEXT, keeping its heading, drawer and keywords."
  (unless (stringp text) (orsb-error 'invalid-argument "text must be a string"))
  (orsb-core--with-node node
    (let ((b (orsb-core--body-bounds)))
      (delete-region (car b) (cdr b))
      (goto-char (car b))
      (insert text)
      (unless (string-suffix-p "\n" text) (insert "\n"))))
  (orsb-core--after-write (org-roam-node-file node))
  t)

(defun orsb-core-append-body (node text &optional prepend)
  "Append TEXT to NODE's body (or prepend with PREPEND), keeping everything else."
  (unless (stringp text) (orsb-error 'invalid-argument "text must be a string"))
  (orsb-core--with-node node
    (let ((b (orsb-core--body-bounds)))
      (if prepend
          (progn (goto-char (car b))
                 (insert text)
                 (unless (string-suffix-p "\n" text) (insert "\n")))
        (orsb-core--insert-at-body-end b text))))
  (orsb-core--after-write (org-roam-node-file node))
  t)

(defun orsb-core--insert-at-body-end (bounds text)
  "Insert TEXT after the last non-blank line of the body BOUNDS.
Blank lines that separated the body from the next heading stay after it."
  (goto-char (cdr bounds))
  (skip-chars-backward " \t\n" (car bounds))
  (if (= (point) (car bounds))
      (goto-char (car bounds))
    (end-of-line)
    ;; reuse the newline that ended the last line instead of adding one
    (if (eq (char-after) ?\n) (forward-char 1) (insert "\n")))
  (insert text)
  (unless (string-suffix-p "\n" text) (insert "\n")))

(defun orsb-core-section-body (node section)
  "Return the body of the heading titled SECTION inside NODE's subtree/file.
Matching is case-insensitive on the heading text (TODO keywords and tags
ignored).  Signals `not-found' when there is no such heading."
  (orsb-core--with-node node
    (let ((end (if (org-before-first-heading-p) (point-max)
                 (save-excursion (org-end-of-subtree t t) (point))))
          (found nil))
      (while (and (not found) (re-search-forward org-heading-regexp end t))
        (when (orsb-core-string= (string-trim (org-get-heading t t t t)) (string-trim section))
          (setq found (point))))
      (unless found
        (orsb-error 'not-found "No heading %S under %s" section (org-roam-node-title node)))
      (goto-char found)
      (let ((b (orsb-core--body-bounds)))
        (if (>= (car b) (cdr b)) ""
          (orsb-core--strip-hidden-property-lines
           (buffer-substring-no-properties (car b) (cdr b))))))))

;;;; Title, tags, TODO

(defun orsb-core-set-title (node title)
  "Set NODE's title: the #+title: keyword for a file node, the headline otherwise."
  (unless (and (stringp title) (not (string-blank-p title)))
    (orsb-error 'invalid-argument "title must be a non-empty string"))
  (if (= (org-roam-node-level node) 0)
      (orsb-core-set-keywords node `(("TITLE" . ,title)))
    (orsb-core--with-node node
      (org-edit-headline title))
    (orsb-core--after-write (org-roam-node-file node)))
  t)

(defun orsb-core-node-local-tags (node)
  "Return NODE's own tags (filetags for a file node, heading tags otherwise)."
  (orsb-core--with-node node
    (if (org-before-first-heading-p)
        (let ((kw (cdr (assoc "FILETAGS" (orsb-core-node-keywords node)))))
          (when kw (split-string kw ":" t "[ \t]+")))
      (org-get-tags nil t))))

(defun orsb-core-set-tags (node tags)
  "Replace NODE's own tags with TAGS (a list of strings)."
  (dolist (tag tags)
    (unless (and (stringp tag) (string-match-p "\\`[[:alnum:]_@#%-]+\\'" tag))
      (orsb-error 'invalid-argument "Invalid tag %S" tag)))
  (if (= (org-roam-node-level node) 0)
      (orsb-core-set-keywords node `(("FILETAGS" . ,(if tags (concat ":" (string-join tags ":") ":") nil))))
    (orsb-core--with-node node
      (org-set-tags tags))
    (orsb-core--after-write (org-roam-node-file node)))
  t)

(defun orsb-core-set-todo (node keyword)
  "Set the TODO KEYWORD on heading NODE; nil or \"\" clears it."
  (when (= (org-roam-node-level node) 0)
    (orsb-error 'invalid-argument "todo can only be set on a heading node, not a file"))
  (orsb-core--with-node node
    (let ((org-inhibit-logging t)
          (org-todo-log-states nil))
      (org-todo (if (or (null keyword) (string-blank-p keyword)) 'none (upcase keyword)))))
  (orsb-core--after-write (org-roam-node-file node))
  t)

;;;; Links

(defun orsb-core-unlink (node target-id)
  "Remove lines in NODE's body that link to TARGET-ID.  Return the count."
  (let ((count 0))
    (orsb-core--with-node node
      (let* ((b (orsb-core--body-bounds))
             (end (copy-marker (cdr b)))
             (rx (concat "^.*\\[\\[id:" (regexp-quote target-id) "\\].*\n?")))
        (goto-char (car b))
        (while (re-search-forward rx end t)
          (replace-match "")
          (setq count (1+ count)))))
    (when (> count 0)
      (orsb-core--after-write (org-roam-node-file node)))
    count))

;;;; Records

(defun orsb-core-node-record (node &optional include-body)
  "Return an alist describing NODE for adapters to serialize.
With INCLUDE-BODY, include the body text."
  (let* ((file (org-roam-node-file node))
         (level (org-roam-node-level node))
         (props (orsb-core-node-properties node))
         (mtime (org-roam-node-file-mtime node)))
    (append
     `((id . ,(org-roam-node-id node))
       (title . ,(org-roam-node-title node))
       (file . ,file)
       (level . ,level)
       (node_type . ,(cdr (assoc "NODE-TYPE" props)))
       (status . ,(cdr (assoc "STATUS" props)))
       (todo . ,(org-roam-node-todo node))
       (tags . ,(vconcat (org-roam-node-tags node)))
       (properties . ,(or props :empty-object))
       (keywords . ,(or (orsb-core-node-keywords node) :empty-object))
       (modified . ,(when mtime (format-time-string "%Y-%m-%dT%H:%M:%S%z" mtime))))
     (when include-body
       `((body . ,(orsb-core-node-body node)))))))

;;;; Vault layout

(defcustom orsb-directories
  '(("project" . "projects") ("person" . "people") ("idea" . "ideas")
    ("admin" . "admin") ("blog" . "blog") ("reference" . "reference")
    ("howto" . "howto") ("note" . ""))
  "Node types and the vault subdirectory each is created in (\"\" = root)."
  :type '(alist :key-type string :value-type string)
  :group 'orsb)

(defcustom orsb-daily-directory "daily"
  "Vault subdirectory of the daily notes (one YYYY-MM-DD.org per day)."
  :type 'string
  :group 'orsb)

(defcustom orsb-archive-directory "archive"
  "Vault subdirectory archived notes are moved to."
  :type 'string
  :group 'orsb)

(defcustom orsb-inbox-heading "Inbox"
  "Heading in the daily note that collects inbox entries."
  :type 'string
  :group 'orsb)

(defcustom orsb-hugo-base-dir nil
  "Root of the Hugo site blog nodes export to (nil: blog creation is refused)."
  :type '(choice (const nil) directory)
  :group 'orsb)

(defcustom orsb-hugo-sections nil
  "Hugo sections offered for blog nodes (nil: any)."
  :type '(repeat string)
  :group 'orsb)

(defun orsb-core--vault-file (relative)
  "RELATIVE resolved under `org-roam-directory'."
  (expand-file-name relative org-roam-directory))

(defun orsb-core--slug (title)
  "A filename-safe slug of TITLE."
  (let ((s (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" title))))
    (string-trim s "-+" "-+")))

(defun orsb-core--new-file-path (title type)
  "Path for a new TYPE note titled TITLE (directory created as needed)."
  (let* ((dir (cdr (assoc type orsb-directories)))
         (dir (if (and dir (not (string-empty-p dir))) (orsb-core--vault-file dir) org-roam-directory))
         (stamp (format-time-string "%Y%m%d%H%M%S"))
         (path (expand-file-name (format "%s-%s.org" (orsb-core--slug title) stamp) dir)))
    (make-directory dir t)
    (while (file-exists-p path)
      (setq path (expand-file-name (format "%s-%s-%d.org" (orsb-core--slug title) stamp (random 1000)) dir)))
    path))

(defun orsb-core--write-new-file (path drawer keywords body)
  "Create PATH with DRAWER (alist), KEYWORDS (alist) and BODY text.
Returns the file-level node."
  (with-temp-file path
    (insert ":PROPERTIES:\n")
    (dolist (cell drawer)
      (when (cdr cell) (insert (format ":%s: %s\n" (car cell) (cdr cell)))))
    (insert ":END:\n")
    (dolist (cell keywords)
      (when (cdr cell) (insert (format "#+%s: %s\n" (car cell) (cdr cell)))))
    (insert "\n")
    (when body (insert body) (unless (string-suffix-p "\n" body) (insert "\n"))))
  (org-roam-db-update-file path)
  (when (fboundp 'orsb-search-enqueue) (orsb-search-enqueue path))
  (or (orsb-core--file-node path)
      (orsb-error 'internal "Created %s but org-roam did not index it" path)))

;;;; Creation

(defun orsb-core-create-node (type title &rest args)
  "Create a TYPE note titled TITLE and return its node.
ARGS is a plist: :body, :status, :next-action, :context, :follow-ups (list),
:one-liner, :due-date, :hugo-section, :tags (list), :properties (alist).
Each type gets its conventional drawer keys and skeleton headings."
  (unless (assoc type orsb-directories)
    (orsb-error 'invalid-argument "node_type must be one of %s"
                (string-join (mapcar #'car orsb-directories) ", ")))
  (unless (and (stringp title) (not (string-blank-p title)))
    (orsb-error 'invalid-argument "title must be a non-empty string"))
  (let* ((id (org-id-new))
         (body (plist-get args :body))
         (status (plist-get args :status))
         (tags (plist-get args :tags))
         (path (orsb-core--new-file-path title type))
         (drawer `(("ID" . ,id) ("NODE-TYPE" . ,type)))
         (keywords `(("title" . ,title)
                     ("filetags" . ,(and tags (concat ":" (string-join tags ":") ":")))))
         (text nil))
    (pcase type
      ("project"
       (setq drawer (append drawer `(("STATUS" . ,(or status "active"))
                                     ("NEXT-ACTION" . ,(plist-get args :next-action)))))
       (setq text (concat "* Next Actions\n"
                          (if (plist-get args :next-action) (format "- [ ] %s\n" (plist-get args :next-action)) "")
                          "\n* Notes\n" (or body ""))))
      ("person"
       (setq drawer (append drawer `(("CONTEXT" . ,(plist-get args :context))
                                     ("LAST-CONTACT" . ,(format-time-string "[%Y-%m-%d %a]")))))
       (setq text (concat "* Follow-ups\n"
                          (mapconcat (lambda (f) (format "- [ ] %s\n" f)) (plist-get args :follow-ups) "")
                          "\n* Notes\n" (or body ""))))
      ("idea"
       (let ((one (or (plist-get args :one-liner)
                      (and body (car (split-string body "\n" t)))
                      title)))
         (setq drawer (append drawer `(("ONE-LINER" . ,one) ("STATUS" . ,status))))
         (setq text (concat "* One-liner\n" one "\n\n* Elaboration\n" (or body "")))))
      ("admin"
       (setq drawer (append drawer `(("DUE-DATE" . ,(and (plist-get args :due-date) (format "[%s]" (plist-get args :due-date))))
                                     ("STATUS" . ,(or status "active")))))
       (setq text (concat "* Notes\n" (or body ""))))
      ("blog"
       (unless orsb-hugo-base-dir
         (orsb-error 'refused "Blog notes need `orsb-hugo-base-dir' to be set on the server"))
       (let* ((section (or (plist-get args :hugo-section) (car orsb-hugo-sections) "posts"))
              (slug (orsb-core--slug title)))
         (when (and orsb-hugo-sections (not (member section orsb-hugo-sections)))
           (orsb-error 'invalid-argument "hugo_section must be one of %s" (string-join orsb-hugo-sections ", ")))
         (setq drawer (append drawer `(("STATUS" . ,(or status "draft"))
                                       ("EXPORT_FILE_NAME" . ,slug)
                                       ("EXPORT_HUGO_SECTION" . ,(format "%s/posts" section)))))
         (setq keywords (append keywords
                                `(("date" . ,(format-time-string "[%Y-%m-%d %a]"))
                                  ("hugo_base_dir" . ,orsb-hugo-base-dir)
                                  ("hugo_draft" . "true")
                                  ("hugo_tags" . ,(and tags (string-join tags " ")))
                                  ("hugo_categories" . ,(capitalize (replace-regexp-in-string "-" " " section))))))
         (setq text (concat "* Draft\n\n" (or body "")))))
      (_ (setq drawer (append drawer `(("STATUS" . ,status))))
         (setq text body)))
    (let ((node (orsb-core--write-new-file path drawer keywords text)))
      (when-let ((props (plist-get args :properties)))
        (orsb-core-set-properties node props))
      (orsb-core-resolve id))))

(defun orsb-core-add-heading (node heading &optional body properties level)
  "Append HEADING (with a fresh :ID:) to NODE's file and return the new node.
BODY and PROPERTIES (alist) are optional; LEVEL defaults to 1."
  (unless (and (stringp heading) (not (string-blank-p heading)))
    (orsb-error 'invalid-argument "heading must be a non-empty string"))
  (let ((id (org-id-new))
        (file (org-roam-node-file node)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert (make-string (or level 1) ?*) " " heading "\n:PROPERTIES:\n:ID: " id "\n")
       (dolist (cell properties)
         (insert (format ":%s: %s\n" (orsb-core--normalize-key (car cell)) (cdr cell))))
       (insert ":END:\n")
       (when (and body (not (string-blank-p body)))
         (insert body)
         (unless (string-suffix-p "\n" body) (insert "\n")))))
    (orsb-core--after-write file)
    (orsb-core-resolve id)))

;;;; Section and file edits

(defun orsb-core--goto-section (section &optional create)
  "Move point to the heading titled SECTION in the current buffer.
With CREATE, append a level-1 heading when it is missing.  Return non-nil
when found or created."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (re-search-forward org-heading-regexp nil t))
      (when (orsb-core-string= (string-trim (org-get-heading t t t t)) (string-trim section))
        (setq found (line-beginning-position))))
    (cond
     (found (goto-char found) t)
     (create (goto-char (point-max))
             (unless (bolp) (insert "\n"))
             (insert "* " section "\n")
             (forward-line -1)
             t)
     (t nil))))

(defun orsb-core-update-section (node section content &optional mode)
  "Edit the body of heading SECTION in NODE's file with CONTENT.
MODE is \"append\" (default), \"prepend\" or \"replace\".  The heading is
created when missing."
  (let ((mode (or mode "append")))
    (unless (member mode '("append" "prepend" "replace"))
      (orsb-error 'invalid-argument "mode must be append, prepend or replace"))
    (with-current-buffer (find-file-noselect (org-roam-node-file node))
      (org-with-wide-buffer
       (orsb-core--goto-section section t)
       (let ((b (orsb-core--body-bounds)))
         (pcase mode
           ("replace" (delete-region (car b) (cdr b))
                      (goto-char (car b))
                      (insert content)
                      (unless (string-suffix-p "\n" content) (insert "\n")))
           ("prepend" (goto-char (car b))
                      (insert content)
                      (unless (string-suffix-p "\n" content) (insert "\n")))
           (_ (orsb-core--insert-at-body-end b content))))))
    (orsb-core--after-write (org-roam-node-file node))
    t))

(defun orsb-core--top-level-headings (text)
  "Titles of the level-1 headings in TEXT."
  (let (out (pos 0))
    (while (string-match "^\\* +\\(.*?\\)[ \t]*$" text pos)
      (push (match-string 1 text) out)
      (setq pos (match-end 0)))
    (nreverse out)))

(defun orsb-core-replace-file-body (node content &optional force)
  "Replace everything after NODE's drawer and keywords with CONTENT.
Refused (`refused') when CONTENT would drop a level-1 heading or shrink a
note of more than 200 characters below half its size, unless FORCE.  A
timestamped .bak sibling is written first."
  (let ((file (org-roam-node-file node)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let* ((b (orsb-core--body-bounds))
              (old (buffer-substring-no-properties (car b) (cdr b)))
              (dropped (seq-difference (orsb-core--top-level-headings old)
                                       (orsb-core--top-level-headings content)
                                       #'string=))
              (shrunk (and (> (length old) 200) (< (length content) (* 0.5 (length old))))))
         (when (and (not force) (or dropped shrunk))
           (orsb-error 'refused "whole-note replace would %s; re-read the note and resend the full body, or pass force:true"
                       (if dropped (format "drop heading(s): %s" (string-join dropped ", "))
                         (format "shrink the note from %d to %d characters" (length old) (length content)))))
         (copy-file file (format "%s.bak-%s" file (format-time-string "%s")) t)
         (delete-region (car b) (cdr b))
         (goto-char (car b))
         (insert content)
         (unless (string-suffix-p "\n" content) (insert "\n")))))
    (orsb-core--after-write file)
    t))

;;;; Delete, archive, link

(defun orsb-core-delete-node (node &optional archive)
  "Delete NODE: a heading subtree, or a whole note (moved to the archive
directory with ARCHIVE).  Returns the resulting file path, or nil."
  (let ((file (org-roam-node-file node)))
    (if (> (org-roam-node-level node) 0)
        (progn
          (orsb-core--with-node node
            (delete-region (point) (save-excursion (org-end-of-subtree t t) (point))))
          (orsb-core--after-write file)
          file)
      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (if archive
          (let* ((dir (orsb-core--vault-file orsb-archive-directory))
                 (target (expand-file-name (file-name-nondirectory file) dir)))
            (make-directory dir t)
            (rename-file file target t)
            (orsb-core-db-refresh file target)
            target)
        (delete-file file)
        (orsb-core-db-refresh file)
        nil))))

(defun orsb-core-link (node target &optional section)
  "Add a bullet linking to TARGET in NODE's file, under SECTION if given."
  (let ((file (org-roam-node-file node))
        (link (format "- [[id:%s][%s]]" (org-roam-node-id target) (org-roam-node-title target))))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (if section
           (progn (orsb-core--goto-section section t)
                  (orsb-core--insert-at-body-end (orsb-core--body-bounds) link))
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert link "\n"))))
    (orsb-core--after-write file)
    t))

;;;; Daily notes and inbox

(defun orsb-core-daily-file (&optional date create)
  "Path of the daily note for DATE (YYYY-MM-DD, default today).
With CREATE, make the file (with an :ID:) when it does not exist."
  (let* ((date (or date (format-time-string "%Y-%m-%d")))
         (dir (orsb-core--vault-file orsb-daily-directory))
         (path (expand-file-name (concat date ".org") dir)))
    (when (and create (not (file-exists-p path)))
      (make-directory dir t)
      (with-temp-file path
        (insert (format ":PROPERTIES:\n:ID: %s\n:NODE-TYPE: daily\n:END:\n#+title: %s\n#+filetags: :daily:\n\n"
                        (org-id-new) date)))
      (org-roam-db-update-file path))
    path))

(defun orsb-core-daily-content (&optional date)
  "Text of the daily note for DATE, or \"\" when there is none."
  (let ((path (orsb-core-daily-file date)))
    (if (file-exists-p path)
        (with-temp-buffer (insert-file-contents path) (buffer-string))
      "")))

(defun orsb-core-add-daily-entry (title points &optional next-steps tags timestamp todo)
  "Append a timestamped heading to today's daily note.
POINTS and NEXT-STEPS are lists of strings, TAGS a list of tag strings.
With TODO the heading is a TODO item and NEXT-STEPS become Subtasks."
  (let ((path (orsb-core-daily-file nil t))
        (stamp (or timestamp (format-time-string "%H:%M"))))
    (with-current-buffer (find-file-noselect path)
      (org-with-wide-buffer
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert (format "* %s%s %s%s\n" (if todo "TODO " "") stamp title
                       (if tags (concat "    :" (string-join tags ":") ":") "")))
       (dolist (p points) (insert (format "- %s\n" p)))
       (when next-steps
         (insert (if todo "\n** Subtasks\n" "\n** Next Steps\n"))
         (dolist (s next-steps) (insert (format "- [ ] %s\n" s))))
       (insert "\n")))
    (orsb-core--after-write path)
    path))

(defun orsb-core--link-names (text)
  "Names in [[Name]] links in TEXT (id: links excluded)."
  (let (out (pos 0))
    (while (string-match "\\[\\[\\([^]:[]+\\)\\]\\]" text pos)
      (push (match-string 1 text) out)
      (setq pos (match-end 0)))
    (nreverse out)))

(defun orsb-core--ensure-person (name)
  "Create a minimal person note for NAME unless a node with that title exists.
Returns the new node, or nil."
  (unless (org-roam-node-from-title-or-alias name)
    (orsb-core-create-node "person" name)))

(defun orsb-core-log-to-inbox (text &optional linked-node)
  "Append TEXT as a timestamped bullet under the inbox heading of today's note.
LINKED-NODE, when given, is appended as an id link.  [[Name]] links in
TEXT get a person note created.  Returns the names of people created."
  (let ((path (orsb-core-daily-file nil t))
        (created nil))
    (dolist (name (orsb-core--link-names text))
      (when (orsb-core--ensure-person name) (push name created)))
    (with-current-buffer (find-file-noselect path)
      (org-with-wide-buffer
       (orsb-core--goto-section orsb-inbox-heading t)
       (orsb-core--insert-at-body-end
        (orsb-core--body-bounds)
        (format "- %s %s%s" (format-time-string "[%Y-%m-%d %a %H:%M]") text
                (if linked-node
                    (format " → [[id:%s][%s]]" (org-roam-node-id linked-node) (org-roam-node-title linked-node))
                  "")))))
    (orsb-core--after-write path)
    (nreverse created)))

(defun orsb-core-inbox-entries (&optional days)
  "Inbox bullets of the last DAYS days (default 7): list of (DATE . LINES)."
  (let (out)
    (dotimes (i (or days 7))
      (let* ((date (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time i))))
             (path (orsb-core-daily-file date)))
        (when (file-exists-p path)
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (when (re-search-forward (format "^\\* %s[ \t]*$" (regexp-quote orsb-inbox-heading)) nil t)
              (let ((lines nil) (end (save-excursion (or (and (re-search-forward "^\\* " nil t) (match-beginning 0)) (point-max)))))
                (forward-line 1)
                (while (< (point) end)
                  (cond ((looking-at "^[ \t]*- \\(.*\\)$") (push (match-string 1) lines))
                        ((looking-at "^\\*\\* \\(?:DONE\\|TODO\\) \\(.*\\)$") (push (match-string 1) lines)))
                  (forward-line 1))
                (when lines (push (cons date (nreverse lines)) out))))))))
    (nreverse out)))

;;;; Follow-ups

(defun orsb-core--unchecked-items (file &optional mentioning)
  "Unchecked checkbox lines in FILE, optionally only those MENTIONING a string."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (items)
        (while (re-search-forward "^[ \t]*- \\[ \\] \\(.*\\)$" nil t)
          (let ((item (string-trim (match-string 1))))
            (when (or (null mentioning) (string-match-p (regexp-quote mentioning) item))
              (push item items))))
        (nreverse items)))))

(defun orsb-core-followups ()
  "People with unchecked follow-ups: list of plists
\(:node :followups) where followups are unchecked items mentioning the
person, from the person's own note and from notes linking to it."
  (let (out)
    (dolist (person (seq-filter (lambda (n) (and (= (org-roam-node-level n) 0)
                                                 (equal "person" (cdr (assoc "NODE-TYPE" (org-roam-node-properties n))))))
                                (org-roam-node-list)))
      (let* ((name (org-roam-node-title person))
             (files (seq-uniq
                     (cons (org-roam-node-file person)
                           (mapcar (lambda (bl) (org-roam-node-file (org-roam-backlink-source-node bl)))
                                   (org-roam-backlinks-get person)))))
             (items (apply #'append
                           (orsb-core--unchecked-items (org-roam-node-file person))
                           (mapcar (lambda (f) (unless (equal f (org-roam-node-file person))
                                                 (orsb-core--unchecked-items f name)))
                                   files))))
        (when items (push (list :node person :followups (seq-uniq items)) out))))
    (sort out (lambda (a b) (> (length (plist-get a :followups)) (length (plist-get b :followups)))))))

(defun orsb-core-dangling-followups ()
  "Unchecked items with a [[Name]] link to a title no node has.
Returns a list of plists (:name :item :file)."
  (let (out (seen (make-hash-table :test 'equal)))
    (dolist (file (org-roam-list-files))
      (dolist (item (orsb-core--unchecked-items file))
        (dolist (name (orsb-core--link-names item))
          (unless (or (gethash name seen) (org-roam-node-from-title-or-alias name))
            (puthash name t seen)
            (push (list :name name :item item :file file) out)))))
    (nreverse out)))

;;;; Blog

(defun orsb-core-blog-node-p (node)
  "Whether NODE is a blog post: NODE-TYPE, blog directory, or Hugo keywords."
  (let ((type (cdr (assoc "NODE-TYPE" (org-roam-node-properties node))))
        (dir (cdr (assoc "blog" orsb-directories))))
    (cond
     (type (equal type "blog"))
     ((and dir (not (string-empty-p dir))
           (string-prefix-p (file-name-as-directory (orsb-core--vault-file dir)) (org-roam-node-file node)))
      t)
     (t (seq-some (lambda (kw) (string-prefix-p "HUGO" (car kw))) (orsb-core-node-keywords node))))))

(defun orsb-core--blog-outline (node)
  "Return (HEADINGS-WITH-TEXT . HEADINGS) for blog NODE."
  (let ((total 0) (filled 0))
    (with-temp-buffer
      (insert-file-contents (org-roam-node-file node))
      (goto-char (point-min))
      (while (re-search-forward "^\\*+[ \t]+.+$" nil t)
        (setq total (1+ total))
        (let ((start (point))
              (end (save-excursion (or (and (re-search-forward "^\\*+[ \t]" nil t) (match-beginning 0)) (point-max)))))
          (when (string-match-p "[[:alnum:]]"
                                (replace-regexp-in-string ":PROPERTIES:\\(?:.\\|\n\\)*?:END:" ""
                                                          (buffer-substring-no-properties start end)))
            (setq filled (1+ filled))))))
    (cons filled total)))

(defun orsb-core-blog-status ()
  "Blog overview as a plist: :drafts, :published (recent first), :ideas."
  (let* ((nodes (seq-filter (lambda (n) (= (org-roam-node-level n) 0)) (org-roam-node-list)))
         (posts (seq-filter #'orsb-core-blog-node-p nodes))
         (draft-p (lambda (n)
                    (let ((status (cdr (assoc "STATUS" (org-roam-node-properties n))))
                          (kw (cdr (assoc "HUGO_DRAFT" (orsb-core-node-keywords n)))))
                      (cond (status (not (equal status "published")))
                            (kw (equal kw "true"))
                            (t t)))))
         (drafts (seq-filter draft-p posts))
         (published (seq-remove draft-p posts))
         (titles (mapcar (lambda (n) (downcase (org-roam-node-title n))) posts))
         (ideas (seq-filter (lambda (n)
                              (and (equal "idea" (cdr (assoc "NODE-TYPE" (org-roam-node-properties n))))
                                   (not (member (downcase (org-roam-node-title n)) titles))))
                            nodes)))
    (list :drafts (mapcar (lambda (n) (list :node n :outline (orsb-core--blog-outline n))) drafts)
          :published (seq-take (seq-sort-by (lambda (n) (or (org-roam-node-file-mtime n) 0))
                                            (lambda (a b) (time-less-p b a)) published)
                               10)
          :ideas (seq-take ideas 10))))

(provide 'orsb-core)
;;; orsb-core.el ends here
