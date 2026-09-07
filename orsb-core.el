;;; orsb-core.el --- Core domain logic for org-roam-second-brain -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.2"))
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
    (set-buffer-modified-p nil)))

(defun orsb-core--after-write (file)
  "Persist FILE's buffer and bring the org-roam db up to date for it.
Every mutation ends here."
  (with-current-buffer (find-file-noselect file)
    (orsb-core--save-quietly))
  (org-roam-db-update-file file))

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
        (goto-char (cdr b))
        (unless (or (bobp) (eq (char-before) ?\n)) (insert "\n"))
        (insert text)
        (unless (string-suffix-p "\n" text) (insert "\n")))))
  (orsb-core--after-write (org-roam-node-file node))
  t)

(defun orsb-core-section-body (node section)
  "Return the body of the heading titled SECTION inside NODE's subtree/file.
Matching is case-insensitive on the heading text (TODO keywords and tags
ignored).  Signals `not-found' when there is no such heading."
  (orsb-core--with-node node
    (let ((end (if (org-before-first-heading-p) (point-max)
                 (save-excursion (org-end-of-subtree t t) (point))))
          (found nil))
      (while (and (not found) (re-search-forward org-heading-regexp end t))
        (when (string-equal-ignore-case (string-trim (org-get-heading t t t t)) (string-trim section))
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

(provide 'orsb-core)
;;; orsb-core.el ends here
