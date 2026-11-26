;;; orsb-core.el --- Core domain logic for org-roam-second-brain -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 3.1.3
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

(defun orsb-error-hint (code hint message &rest args)
  "Like `orsb-error' with CODE and MESSAGE, plus HINT.
HINT says what the caller should do next.
Adapters show HINT instead of their generic per-code hint."
  (signal 'orsb-error (list code (apply #'format message args) hint)))

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

(defun orsb-core--title-candidates (title)
  "Nodes whose title or alias is TITLE (case-insensitive), as (ID . FILE) pairs."
  (seq-uniq
   (mapcar (lambda (row) (cons (car row) (cadr row)))
           (append
            (org-roam-db-query [:select [id file] :from nodes :where (= title $s1) :collate NOCASE] title)
            (org-roam-db-query [:select [nodes:id nodes:file] :from aliases
                                :inner-join nodes :on (= aliases:node-id nodes:id)
                                :where (= aliases:alias $s1) :collate NOCASE]
                               title)))))

(defun orsb-core--from-title (title)
  "The node titled (or aliased) TITLE, nil when none.
Two or more matches signal `invalid-argument' listing their ids, so the
caller can pick one instead of hitting an internal error."
  (let ((exact (condition-case nil (org-roam-node-from-title-or-alias title) (error :ambiguous))))
    (cond
     ((org-roam-node-p exact) exact)
     (t (let ((cands (orsb-core--title-candidates title)))
          (cond
           ((null cands) nil)
           ((and (= (length cands) 1) (not (eq exact :ambiguous))) (org-roam-node-from-id (caar cands)))
           (t (orsb-error-hint
               'invalid-argument "Pass one of these ids as id instead of the title."
               "%d notes are titled %S: %s" (length cands) title
               (mapconcat (lambda (c) (format "%s (%s)" (car c)
                                              (file-relative-name (cdr c) org-roam-directory)))
                          cands "; ")))))))))

(defun orsb-core--date-node (identifier)
  "The daily note IDENTIFIER names (\"today\", \"yesterday\", YYYY-MM-DD), or nil.
Signals `not-found' when the day has no daily note."
  (let ((date (cond ((string= (downcase identifier) "today") (format-time-string "%Y-%m-%d"))
                    ((string= (downcase identifier) "yesterday")
                     (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1))))
                    ((string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" identifier) identifier))))
    (when date
      (let ((path (orsb-core-daily-file date)))
        (or (and (file-exists-p path) (orsb-core--first-node-in-file (file-truename path)))
            (orsb-error-hint 'not-found "add_daily_entry or log_to_inbox creates the day's note."
                             "No daily note for %s" date))))))

(defun orsb-core-resolve (identifier)
  "Return the org-roam node IDENTIFIER names, signalling `not-found' otherwise.
IDENTIFIER is tried as an org-roam id, then as an absolute path or a path
relative to `org-roam-directory', then as an exact title or alias, then as
a day (\"today\", \"yesterday\" or YYYY-MM-DD) naming a daily note.  A
title shared by several nodes signals `invalid-argument' with their ids."
  (unless (and (stringp identifier) (not (string-empty-p (string-trim identifier))))
    (orsb-error 'invalid-argument "id must be a non-empty string"))
  (let ((identifier (string-trim identifier)))
    (or (org-roam-node-from-id identifier)
        (let ((path (cond ((file-name-absolute-p identifier) identifier)
                          (t (expand-file-name identifier org-roam-directory)))))
          (when (and (file-exists-p path) (not (file-directory-p path)))
            (orsb-core--first-node-in-file (file-truename path))))
        (orsb-core--from-title identifier)
        (orsb-core--date-node identifier)
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

;;;; Change journal: backups, dry runs, undo
;;
;; Every write ends in `orsb-core--after-write' (buffer edits) or one of the
;; `orsb-core--fs-*' helpers (whole-file operations).  Inside
;; `orsb-core-call-with-change' both first record the file's on-disk content,
;; so a change can be undone (`orsb-core-restore') or, when it fails half way,
;; rolled back automatically.  With `orsb-core-dry-run' bound, edits stay in
;; the buffers and nothing touches the disk; the caller gets a preview and
;; every buffer is reverted afterwards.

(defcustom orsb-change-directory nil
  "Directory of the change journal (one file per change).
nil means `orsb-changes/' under `user-emacs-directory'.  It is kept out of
the vault so org-roam never indexes it."
  :type '(choice (const nil) directory)
  :group 'orsb)

(defcustom orsb-change-keep 200
  "How many changes the journal keeps; older ones are pruned."
  :type 'integer
  :group 'orsb)

(defvar orsb-core--change nil
  "The change being recorded, a plist (:files ALIST :ops LIST), or nil.
:files maps each touched file (absolute) to its content before the change,
nil for a file that did not exist.  :ops lists planned whole-file operations
for previews.")

(defvar orsb-core-dry-run nil
  "Non-nil while a change is previewed: buffers change, the disk does not.")

(defun orsb-core--disk-content (file)
  "FILE's content on disk as a string, or nil when it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (buffer-string))))

(defun orsb-core--snapshot (file)
  "Remember FILE's on-disk content in the current change (first touch only)."
  (when (and orsb-core--change file)
    (let ((file (expand-file-name file)))
      (unless (assoc file (plist-get orsb-core--change :files))
        (setq orsb-core--change
              (plist-put orsb-core--change :files
                         (cons (cons file (orsb-core--disk-content file))
                               (plist-get orsb-core--change :files))))))))

(defun orsb-core--plan (op &rest args)
  "Record whole-file operation OP with ARGS for the preview."
  (when orsb-core--change
    (setq orsb-core--change
          (plist-put orsb-core--change :ops
                     (append (plist-get orsb-core--change :ops) (list (cons op args)))))))

(defun orsb-core--after-write (file)
  "Persist FILE's buffer and bring the org-roam db up to date for it.
Every buffer mutation ends here.  Inside a recorded change the old content
is kept first; in a dry run the edit stays in the buffer, unsaved."
  (orsb-core--snapshot file)
  (unless orsb-core-dry-run
    (with-current-buffer (find-file-noselect file)
      (orsb-core--save-quietly))
    (org-roam-db-update-file file)
    ;; Embeddings are refreshed by the idle worker, never on the request path.
    (when (fboundp 'orsb-search-enqueue)
      (orsb-search-enqueue file))))

(defun orsb-core--forget-buffer (file)
  "Kill FILE's buffer without saving, if it has one."
  (when-let ((buf (find-buffer-visiting file)))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf)))

(defun orsb-core--fs-delete (file)
  "Delete FILE as part of the current change."
  (orsb-core--snapshot file)
  (orsb-core--plan 'delete file)
  (unless orsb-core-dry-run
    (orsb-core--forget-buffer file)
    (delete-file file)
    (orsb-core-db-refresh file)))

(defun orsb-core--fs-rename (from to)
  "Move file FROM to TO as part of the current change (TO must not exist)."
  (orsb-core--snapshot from)
  (orsb-core--snapshot to)
  (orsb-core--plan 'move from to)
  (unless orsb-core-dry-run
    (orsb-core--forget-buffer from)
    (make-directory (file-name-directory to) t)
    (rename-file from to)
    (orsb-core-db-refresh from to)))

(defun orsb-core--fs-write (file content)
  "Write CONTENT to FILE (nil CONTENT deletes it) as part of the current change."
  (orsb-core--snapshot file)
  (orsb-core--plan (if content 'write 'delete) file)
  (unless orsb-core-dry-run
    (orsb-core--forget-buffer file)
    (if content
        (progn (make-directory (file-name-directory file) t)
               (let ((coding-system-for-write 'utf-8))
                 (write-region content nil file nil :silent)))
      (when (file-exists-p file) (delete-file file)))
    (orsb-core-db-refresh file)
    (when (and content (fboundp 'orsb-search-enqueue))
      (orsb-search-enqueue file))))

(defun orsb-core--change-dir ()
  "The journal directory, created on demand."
  (let ((dir (file-name-as-directory
              (expand-file-name (or orsb-change-directory (locate-user-emacs-file "orsb-changes/"))))))
    (make-directory dir t)
    dir))

(defun orsb-core--hash (content)
  "Hash of file CONTENT (nil for a missing file)."
  (and content (sha1 content)))

(defun orsb-core--rel (file)
  "FILE relative to the vault when it is inside it."
  (if (and file (string-prefix-p (file-truename org-roam-directory) (file-truename file)))
      (file-relative-name (file-truename file) (file-truename org-roam-directory))
    file))

(defun orsb-core--line-diff (old new)
  "Lines removed from OLD and added in NEW, as (REMOVED . ADDED).
At most 15 of each."
  (let* ((ol (split-string (or old "") "\n"))
         (nl (split-string (or new "") "\n"))
         (count (lambda (lines)
                  (let ((h (make-hash-table :test 'equal)))
                    (dolist (l lines) (puthash l (1+ (gethash l h 0)) h))
                    h)))
         (oh (funcall count ol)) (nh (funcall count nl))
         removed added)
    (dolist (l ol) (if (> (gethash l nh 0) 0) (puthash l (1- (gethash l nh)) nh)
                     (unless (string-blank-p l) (push l removed))))
    (dolist (l nl) (if (> (gethash l oh 0) 0) (puthash l (1- (gethash l oh)) oh)
                     (unless (string-blank-p l) (push l added))))
    (cons (seq-take (nreverse removed) 15) (seq-take (nreverse added) 15))))

(defun orsb-core--preview ()
  "Describe the change being recorded (dry run) as a list of alists."
  (let ((ops (plist-get orsb-core--change :ops)) out)
    (dolist (cell (reverse (plist-get orsb-core--change :files)))
      (let* ((file (car cell)) (old (cdr cell))
             (op (seq-find (lambda (o) (member file (cdr o))) ops))
             (buf (find-buffer-visiting file))
             (new (cond ((and op (eq (car op) 'delete)) nil)
                        ((and op (eq (car op) 'move)) (if (equal file (nth 1 op)) nil old))
                        ((and buf (buffer-modified-p buf)) (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))
                        (t old)))
             (action (cond ((and op (eq (car op) 'move)) (if (equal file (nth 1 op)) "move_from" "move_to"))
                           ((and op (eq (car op) 'delete)) "delete")
                           ((null old) "create")
                           (t "edit"))))
        (unless (and (equal action "edit") (equal old new))
          (let ((d (orsb-core--line-diff old (if (equal action "move_to") (cdr (assoc (nth 1 op) (plist-get orsb-core--change :files))) new))))
            (push `((file . ,(orsb-core--rel file)) (action . ,action)
                    (chars_before . ,(length (or old "")))
                    (chars_after . ,(length (or new (and (equal action "move_to") (cdr (assoc (nth 1 op) (plist-get orsb-core--change :files)))) "")))
                    ,@(unless (member action '("move_from" "move_to"))
                        `((lines_removed . ,(vconcat (car d))) (lines_added . ,(vconcat (cdr d))))))
                  out)))))
    (nreverse out)))

(defun orsb-core--revert-touched ()
  "Undo unsaved dry-run edits in every buffer the current change touched."
  (dolist (cell (plist-get orsb-core--change :files))
    (when-let ((buf (find-buffer-visiting (car cell))))
      (if (cdr cell)
          (with-current-buffer buf
            (let ((inhibit-read-only t) (pt (point)))
              (erase-buffer)
              (insert (cdr cell))
              (goto-char (min pt (point-max)))
              (set-buffer-modified-p nil)))
        (orsb-core--forget-buffer (car cell))))))

(defun orsb-core--put-back (files)
  "Write each (FILE . CONTENT) in FILES back to disk; nil CONTENT deletes FILE."
  (dolist (cell files)
    (let ((file (car cell)) (content (cdr cell)))
      (orsb-core--forget-buffer file)
      (if content
          (progn (make-directory (file-name-directory file) t)
                 (let ((coding-system-for-write 'utf-8))
                   (write-region content nil file nil :silent)))
        (when (file-exists-p file) (delete-file file)))))
  ;; clear before update so an id that moved between files never collides
  (dolist (cell files) (unless (cdr cell) (ignore-errors (org-roam-db-clear-file (car cell)))))
  (dolist (cell files) (when (cdr cell) (ignore-errors (org-roam-db-clear-file (car cell)))))
  (dolist (cell files) (when (cdr cell) (org-roam-db-update-file (car cell)))))

(defun orsb-core--journal-write (tool summary)
  "Store the current change under a new id and return the id.
Return nil if nothing changed."
  (let ((files (seq-remove (lambda (c) (equal (cdr c) (orsb-core--disk-content (car c))))
                           (plist-get orsb-core--change :files))))
    (when files
      (let* ((id (format "%s-%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))
             (entry (list :id id :time (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                          :tool tool :summary summary :vault (expand-file-name org-roam-directory)
                          :files (mapcar (lambda (c) (list (car c) (cdr c) (orsb-core--hash (orsb-core--disk-content (car c)))))
                                         files)))
             (dir (orsb-core--change-dir)))
        (let ((coding-system-for-write 'utf-8))
          (with-temp-file (expand-file-name (concat id ".eld") dir)
            (let ((print-length nil) (print-level nil) (print-escape-newlines nil))
              (prin1 entry (current-buffer)))))
        (let ((all (directory-files dir t "\\.eld\\'")))
          (when (> (length all) orsb-change-keep)
            (mapc #'delete-file (seq-take all (- (length all) orsb-change-keep)))))
        id))))

(defun orsb-core-call-with-change (tool summary dry-run fn)
  "Run FN as one recorded change named TOOL with SUMMARY.
Returns (RESULT . CHANGE): CHANGE is the new change id (nil when nothing
was written) or, with DRY-RUN, a preview of what would change.  When FN
signals, the files it already wrote are put back before the error goes on.
A call nested inside another change joins it."
  (if orsb-core--change
      (cons (funcall fn) nil)
    (let ((orsb-core--change (list :files nil :ops nil))
          (orsb-core-dry-run dry-run)
          (done nil))
      (unwind-protect
          (let ((result (funcall fn)))
            (setq done t)
            (cons result (if dry-run (orsb-core--preview) (orsb-core--journal-write tool summary))))
        (cond (dry-run (orsb-core--revert-touched))
              ((not done)
               (orsb-core--put-back (seq-remove (lambda (c) (equal (cdr c) (orsb-core--disk-content (car c))))
                                                (plist-get orsb-core--change :files)))))))))

(defun orsb-core--journal-read (id)
  "The journal entry for change ID, or signal `not-found'."
  (let ((file (expand-file-name (concat id ".eld") (orsb-core--change-dir))))
    (unless (and (string-match-p "\\`[0-9T]+-[0-9a-f]+\\'" id) (file-exists-p file))
      (orsb-error-hint 'not-found "Call restore without change_id to list recent changes."
                       "No change %S in the journal" id))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
      (read (current-buffer)))))

(defun orsb-core-changes (&optional limit)
  "The LIMIT (default 10) most recent changes, newest first, as plists."
  (let* ((dir (orsb-core--change-dir))
         (files (seq-take (reverse (directory-files dir t "\\.eld\\'")) (or limit 10))))
    (mapcar (lambda (f)
              (let ((e (with-temp-buffer
                         (let ((coding-system-for-read 'utf-8)) (insert-file-contents f))
                         (read (current-buffer)))))
                (list :id (plist-get e :id) :time (plist-get e :time) :tool (plist-get e :tool)
                      :summary (plist-get e :summary) :restored (plist-get e :restored)
                      :files (mapcar (lambda (x) (orsb-core--rel (car x))) (plist-get e :files)))))
            files)))

(defun orsb-core-restore (id &optional force dry-run)
  "Undo change ID: put every file it touched back as it was before.
Refused when a file changed again since, unless FORCE.  The restore is
itself a change, so it can be undone too.  Returns (FILES . CHANGE)
where FILES is a list of (FILE . ACTION)."
  (let* ((entry (orsb-core--journal-read id))
         (files (plist-get entry :files))
         (conflicts (seq-filter (lambda (f) (not (equal (orsb-core--hash (orsb-core--disk-content (nth 0 f))) (nth 2 f))))
                                files)))
    (when (and conflicts (not force))
      (orsb-error-hint 'refused "Pass force:true to restore anyway (later edits to these files are lost), or restore the later changes first."
                       "Changed again since %s: %s" id
                       (mapconcat (lambda (f) (orsb-core--rel (car f))) conflicts ", ")))
    (let ((r (orsb-core-call-with-change
              "restore" (format "restore %s" id) dry-run
              (lambda ()
                (let ((back (mapcar (lambda (f) (cons (nth 0 f) (nth 1 f))) files))
                      (actions nil))
                  (dolist (f files)
                    (orsb-core--snapshot (nth 0 f))
                    (orsb-core--plan (if (nth 1 f) 'write 'delete) (nth 0 f))
                    (push (cons (nth 0 f) (cond ((null (nth 1 f)) "deleted")
                                                ((file-exists-p (nth 0 f)) "restored")
                                                (t "recreated")))
                          actions))
                  ;; all files first, then the db, so an id that moved between
                  ;; files is never indexed twice
                  (unless orsb-core-dry-run (orsb-core--put-back back))
                  (nreverse actions))))))
      (unless dry-run
        (let ((file (expand-file-name (concat id ".eld") (orsb-core--change-dir))))
          (let ((coding-system-for-write 'utf-8))
            (with-temp-file file
              (let ((print-length nil) (print-level nil))
                (prin1 (plist-put entry :restored (cdr r)) (current-buffer)))))))
      r)))

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
      ;; The end is taken from the heading itself.  Taking it after stepping
      ;; onto the next line was wrong for a heading with an empty body: that
      ;; line is the NEXT heading, so the bounds became the next section's,
      ;; and an append landed in it and a replace deleted it.
      (let ((end (save-excursion (org-end-of-subtree t t) (point))))
        (end-of-line)
        (if (eobp)
            ;; a heading that ends the buffer without a newline
            (progn (insert "\n") (setq end (point)))
          (forward-line 1))
        (when (and (< (point) end) (looking-at "^[ \t]*:PROPERTIES:"))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
            (forward-line 1)))
        (cons (min (point) end) end)))))

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
    (let* ((start (point))
           (end (if (org-before-first-heading-p) (point-max)
                  (save-excursion (org-end-of-subtree t t) (point))))
           (found nil))
      (while (and (not found) (re-search-forward org-heading-regexp end t))
        (when (orsb-core-string= (string-trim (org-get-heading t t t t)) (string-trim section))
          (setq found (point))))
      (unless found
        (orsb-error-hint 'not-found (orsb-core--sections-hint start end)
                         "No heading %S under %s" section (org-roam-node-title node)))
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
  "Remove NODE's links to TARGET-ID and return how many there were.
A line that holds nothing but the link (a bare bullet) is removed; in any
other line only the link goes, replaced by its description, so the
surrounding text survives."
  (let ((count 0))
    (orsb-core--with-node node
      (let* ((b (orsb-core--body-bounds))
             (end (copy-marker (cdr b)))
             (link-rx (concat "\\[\\[id:" (regexp-quote target-id) "\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]")))
        (goto-char (car b))
        (while (re-search-forward link-rx end t)
          (setq count (1+ count))
          (let ((desc (or (match-string 1) ""))
                (mb (match-beginning 0)) (me (match-end 0)))
            (if (save-excursion
                  (goto-char mb)
                  (and (looking-back "^[ \t]*\\(?:[-+*][ \t]+\\)?" (line-beginning-position))
                       (progn (goto-char me) (looking-at "[ \t]*$"))))
                (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
              (delete-region mb me)
              (goto-char mb)
              (insert desc))))))
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
       (status . ,(cdr (assoc "STATUS" props)))
       (todo . ,(org-roam-node-todo node))
       (tags . ,(vconcat (org-roam-node-tags node)))
       (properties . ,(or props (make-hash-table)))
       (keywords . ,(or (orsb-core-node-keywords node) (make-hash-table)))
       (modified . ,(when mtime (format-time-string "%Y-%m-%dT%H:%M:%S%z" mtime))))
     (when include-body
       `((body . ,(orsb-core-node-body node)))))))

;;;; Vault layout

(defcustom orsb-daily-directory "daily"
  "Vault subdirectory of the daily notes (one YYYY-MM-DD.org per day)."
  :type 'string
  :group 'orsb)

(defcustom orsb-archive-directory "archive"
  "Vault subdirectory archived notes are moved to."
  :type 'string
  :group 'orsb)

(defcustom orsb-replace-backup t
  "Non-nil writes a timestamped .bak copy before a whole-note replace.
The copy sits beside the note.  Every write is also journaled and can
be undone with `restore', so a site where a writer replaces notes often
(a generated note rewritten every night) can set this to nil to keep
the vault free of .bak files."
  :type 'boolean
  :group 'orsb)

(defcustom orsb-inbox-heading "Inbox"
  "Heading in the daily note that collects inbox entries."
  :type 'string
  :group 'orsb)

(defun orsb-core--vault-file (relative)
  "RELATIVE resolved under `org-roam-directory'."
  (expand-file-name relative org-roam-directory))

(defun orsb-core--slug (title)
  "A filename-safe slug of TITLE."
  (let ((s (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" title))))
    (string-trim s "-+" "-+")))

(defun orsb-core--new-file-path (title &optional dir)
  "Path for a new note titled TITLE in DIR (the vault's top folder by default).
DIR is created as needed."
  (let* ((dir (file-name-as-directory (expand-file-name (or dir org-roam-directory))))
         (stamp (format-time-string "%Y%m%d%H%M%S"))
         (path (expand-file-name (format "%s-%s.org" (orsb-core--slug title) stamp) dir)))
    (make-directory dir t)
    (while (file-exists-p path)
      (setq path (expand-file-name (format "%s-%s-%d.org" (orsb-core--slug title) stamp (random 1000)) dir)))
    path))

(defun orsb-core--write-new-file (path drawer keywords body)
  "Create PATH with DRAWER (alist), KEYWORDS (alist) and BODY text.
Returns the file-level node."
  (orsb-core--snapshot path)
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

(defun orsb-core-create-node (title &rest args)
  "Create a note titled TITLE and return its node.
ARGS is a plist: :dir (folder, default the vault's top folder), :drawer
\(alist of properties, in order; an ID is added), :keywords (alist of extra
#+KEY lines), :tags (list) and :body.  What a note of some type needs is the
caller's business: this function knows no note types."
  (unless (and (stringp title) (not (string-blank-p title)))
    (orsb-error 'invalid-argument "title must be a non-empty string"))
  (let* ((id (org-id-new))
         (tags (plist-get args :tags))
         (path (orsb-core--new-file-path title (plist-get args :dir))))
    (orsb-core--write-new-file
     path
     (cons (cons "ID" id) (seq-remove (lambda (c) (equal (car c) "ID")) (plist-get args :drawer)))
     (append `(("title" . ,title)
               ("filetags" . ,(and tags (concat ":" (string-join tags ":") ":"))))
             (plist-get args :keywords))
     (plist-get args :body))
    (orsb-core-resolve id)))

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

(defun orsb-core--heading-titles (&optional start end)
  "Titles of the headings between START and END (default: whole buffer)."
  (save-excursion
    (goto-char (or start (point-min)))
    (let (out)
      (while (re-search-forward org-heading-regexp (or end (point-max)) t)
        (push (string-trim (org-get-heading t t t t)) out))
      (nreverse out))))

(defun orsb-core--sections-hint (&optional start end)
  "A hint naming the headings between START and END (default: the node at point).
Point must be at a node in an org buffer."
  (let* ((start (or start (if (org-before-first-heading-p) (point-min) (point))))
         (end (or end (if (org-before-first-heading-p) (point-max)
                        (save-excursion (org-end-of-subtree t t) (point)))))
         (titles (orsb-core--heading-titles start end)))
    (if titles
        (format "Headings here: %s. Use one of these exactly, or call get_node to see the outline."
                (mapconcat (lambda (h) (format "%S" h)) (seq-take (seq-uniq titles) 25) ", "))
      "This note has no headings; leave section out to edit the body.")))

(defun orsb-core-update-section (node section content &optional mode no-create)
  "Edit the body of heading SECTION in NODE's file with CONTENT.
MODE is \"append\" (default), \"prepend\" or \"replace\".  The heading is
created when missing, unless NO-CREATE, which signals `not-found' naming
the headings that do exist."
  (let ((mode (or mode "append")))
    (unless (member mode '("append" "prepend" "replace"))
      (orsb-error 'invalid-argument "mode must be append, prepend or replace"))
    (with-current-buffer (find-file-noselect (org-roam-node-file node))
      (unless (derived-mode-p 'org-mode) (org-mode))
      (org-with-wide-buffer
       (unless (orsb-core--goto-section section (not no-create))
         (orsb-error-hint 'not-found (concat (orsb-core--sections-hint (point-min) (point-max))
                                             " To add it as a new heading, pass create_section:true.")
                          "No heading %S in %s" section (org-roam-node-title node)))
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
timestamped .bak sibling is written first when `orsb-replace-backup' is set."
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
         (when orsb-replace-backup
           (copy-file file (format "%s.bak-%s" file (format-time-string "%s")) t))
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
      (if archive
          (let* ((dir (orsb-core--vault-file orsb-archive-directory))
                 (target (expand-file-name (file-name-nondirectory file) dir)))
            (when (string-prefix-p (file-name-as-directory (file-truename dir)) (file-truename file))
              (orsb-error 'refused "%s is already archived; pass archive:false to delete it" (orsb-core--rel file)))
            (while (file-exists-p target)
              (setq target (expand-file-name (format "%s-%d.%s" (file-name-base file) (random 10000)
                                                     (file-name-extension file))
                                             dir)))
            (orsb-core--fs-rename file target)
            target)
        (orsb-core--fs-delete file)
        nil))))

(defun orsb-core-links-to-p (node target-id)
  "Whether NODE links to TARGET-ID.
A file node counts its whole file, a heading its subtree."
  (orsb-core--with-node node
    (let ((end (if (org-before-first-heading-p) (point-max)
                 (save-excursion (org-end-of-subtree t t) (point)))))
      (and (search-forward (format "[[id:%s]" target-id) end t) t))))

(defun orsb-core-link (node target &optional section)
  "Add a bullet linking to TARGET in NODE's file, under SECTION if given.
Returns `added', or `exists' (writing nothing) when NODE already links there."
  (if (orsb-core-links-to-p node (org-roam-node-id target))
      'exists
    (orsb-core--link-1 node target section)
    'added))

(defun orsb-core--link-1 (node target section)
  "Insert the link bullet to TARGET in NODE's file (under SECTION if given)."
  (let ((file (org-roam-node-file node))
        (link (format "- [[id:%s][%s]]" (org-roam-node-id target) (org-roam-node-title target))))
    (with-current-buffer (find-file-noselect file)
      (unless (derived-mode-p 'org-mode) (org-mode))
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
      (orsb-core--snapshot path)
      (make-directory dir t)
      (with-temp-file path
        (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n#+filetags: :daily:\n\n"
                        (org-id-new) date)))
      (org-roam-db-update-file path))
    path))

(defun orsb-core-daily-content (&optional date)
  "Text of the daily note for DATE, or \"\" when there is none."
  (let ((path (orsb-core-daily-file date)))
    (if (file-exists-p path)
        (with-temp-buffer (insert-file-contents path) (buffer-string))
      "")))

(defun orsb-core-add-daily-entry (title points &optional next-steps tags timestamp todo date)
  "Append a timestamped heading to the daily note of DATE (default today).
POINTS and NEXT-STEPS are lists of strings, TAGS a list of tag strings.
With TODO the heading is a TODO item and NEXT-STEPS become Subtasks."
  (when (and date (not (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date)))
    (orsb-error 'invalid-argument "date must be YYYY-MM-DD (got %S)" date))
  (let ((path (orsb-core-daily-file date t))
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

(defun orsb-core-log-to-inbox (text &optional linked-node)
  "Append TEXT as a timestamped bullet under the inbox heading of today's note.
LINKED-NODE, when given, is appended as an id link."
  (let ((path (orsb-core-daily-file nil t)))
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
    nil))

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

;;;; Outline

(defun orsb-core-outline (node)
  "The headings inside NODE (its file, or its subtree), as plists
\(:title :level :id :todo), in document order.  A heading node's own
heading is not included."
  (orsb-core--with-node node
    (let* ((file-p (org-before-first-heading-p))
           (end (if file-p (point-max) (save-excursion (org-end-of-subtree t t) (point))))
           (out nil))
      (unless file-p (end-of-line))
      (while (re-search-forward org-heading-regexp end t)
        (save-excursion
          (beginning-of-line)
          (push (list :title (string-trim (org-get-heading t t t t))
                      :level (org-current-level)
                      :id (org-entry-get nil "ID")
                      :todo (org-get-todo-state))
                out)))
      (nreverse out))))

;;;; Planning (SCHEDULED / DEADLINE)

(defun orsb-core--parse-date (date)
  "DATE (YYYY-MM-DD, optionally with HH:MM) as (TIME . HAS-CLOCK).
Signal `invalid-argument' otherwise."
  (unless (and (stringp date)
               (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\(?:[ T]\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\'"
                             (string-trim date)))
    (orsb-error 'invalid-argument "dates are YYYY-MM-DD or \"YYYY-MM-DD HH:MM\" (got %S)" date))
  (let* ((d (string-trim date))
         (_ (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\(?:[ T]\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\'" d))
         (hh (match-string 4 d)) (mm (match-string 5 d)))
    (cons (encode-time (list 0 (if mm (string-to-number mm) 0) (if hh (string-to-number hh) 0)
                             (string-to-number (match-string 3 d)) (string-to-number (match-string 2 d))
                             (string-to-number (match-string 1 d)) nil -1 nil))
          (and hh t))))

(defun orsb-core--org-timestamp (date)
  "An active Org timestamp for DATE (see `orsb-core--parse-date')."
  (let ((p (orsb-core--parse-date date)))
    (let ((system-time-locale "C"))
      (format-time-string (if (cdr p) "<%Y-%m-%d %a %H:%M>" "<%Y-%m-%d %a>") (car p)))))

(defun orsb-core-set-planning (node what date)
  "Set WHAT (`scheduled' or `deadline') on heading NODE to DATE.
A nil or empty DATE removes it."
  (when (= (org-roam-node-level node) 0)
    (orsb-error-hint 'invalid-argument
                     "Schedule a task heading inside the note, or set a DUE-DATE property on the note."
                     "%s applies to headings (tasks), not to a whole note" what))
  (let ((stamp (and date (not (string-blank-p date)) (orsb-core--org-timestamp date))))
    (orsb-core--with-node node
      (let ((org-log-reschedule nil) (org-log-redeadline nil)
            (org-read-date-force-compatible-dates nil))
        (if stamp
            (org-add-planning-info what stamp)
          (org-add-planning-info nil nil what)))))
  (orsb-core--after-write (org-roam-node-file node))
  t)

(defun orsb-core-node-planning (node)
  "Heading NODE's (:scheduled S :deadline D) as YYYY-MM-DD[ HH:MM] strings, or nil."
  (unless (= (org-roam-node-level node) 0)
    (orsb-core--with-node node
      (let ((sch (org-entry-get nil "SCHEDULED")) (dl (org-entry-get nil "DEADLINE")))
        (when (or sch dl)
          (list :scheduled (and sch (orsb-core--stamp-date sch))
                :deadline (and dl (orsb-core--stamp-date dl))))))))

(defun orsb-core--stamp-date (stamp)
  "YYYY-MM-DD (and HH:MM when present) from Org timestamp STAMP."
  (when (and stamp (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\(?:[^0-9>]*\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\)?" stamp))
    (if (match-string 2 stamp) (concat (match-string 1 stamp) " " (match-string 2 stamp)) (match-string 1 stamp))))

;;;; Aliases

(defun orsb-core-node-aliases (node)
  "NODE's ROAM_ALIASES as a list of strings."
  (let ((raw (orsb-core-node-property node "ROAM_ALIASES")))
    (and raw (split-string-and-unquote raw))))

(defun orsb-core-set-aliases (node add remove)
  "Add the strings in ADD to NODE's aliases and drop those in REMOVE.
Return the aliases."
  (let* ((current (orsb-core-node-aliases node))
         (new (seq-uniq (append (seq-remove (lambda (a) (seq-some (lambda (r) (orsb-core-string= a r)) remove)) current)
                                (seq-remove #'string-blank-p add)))))
    (orsb-core--with-node node
      (if new
          (org-entry-put nil "ROAM_ALIASES" (combine-and-quote-strings new))
        (org-entry-delete nil "ROAM_ALIASES")))
    (orsb-core--after-write (org-roam-node-file node))
    new))

;;;; Backlinks

(defun orsb-core-backlinks (node &optional subtree)
  "Nodes in other files that link to NODE, as (ID TITLE FILE) lists.
With SUBTREE, links to any node inside NODE (its file, or its subtree for
a heading) count too."
  (let* ((file (org-roam-node-file node))
         (ids (if subtree
                  (if (= (org-roam-node-level node) 0)
                      (mapcar #'car (org-roam-db-query [:select [id] :from nodes :where (= file $s1)] file))
                    (cons (org-roam-node-id node)
                          (delq nil (mapcar (lambda (h) (plist-get h :id)) (orsb-core-outline node)))))
                (list (org-roam-node-id node))))
         (rows (apply #'append
                      (mapcar (lambda (id)
                                (org-roam-db-query [:select :distinct [nodes:id nodes:title nodes:file] :from links
                                                    :inner-join nodes :on (= links:source nodes:id)
                                                    :where (and (= links:dest $s1) (= links:type "id"))]
                                                   id))
                              ids))))
    (seq-uniq (seq-remove (lambda (r) (equal (nth 2 r) file)) rows)
              (lambda (a b) (equal (car a) (car b))))))

;;;; Rename propagation

(defun orsb-core-propagate-rename (node old-title new-title)
  "Rewrite links to NODE whose text is OLD-TITLE so they read NEW-TITLE.
Covers [[id:ID][OLD-TITLE]] descriptions in the files linking to NODE and
plain [[OLD-TITLE]] title links anywhere in the vault.  Returns the list
of files changed."
  (let ((id (org-roam-node-id node))
        (changed nil))
    (unless (or (string-blank-p old-title) (orsb-core-string= old-title new-title))
      (let* ((desc-rx (concat "\\[\\[id:" (regexp-quote id) "\\]\\[" (regexp-quote old-title) "\\]\\]"))
            (name-rx (concat "\\[\\[" (regexp-quote old-title) "\\]\\]"))
            (candidates (seq-uniq
                         (append (mapcar #'car (org-roam-db-query
                                                [:select :distinct [nodes:file] :from links
                                                 :inner-join nodes :on (= links:source nodes:id)
                                                 :where (= links:dest $s1)]
                                                id))
                                 (seq-filter (lambda (f)
                                               (with-temp-buffer
                                                 (insert-file-contents f)
                                                 (let ((case-fold-search t)) (re-search-forward name-rx nil t))))
                                             (org-roam-list-files))))))
        (dolist (file candidates)
          (with-current-buffer (find-file-noselect file)
            (let ((hit nil) (case-fold-search t))
              (org-with-wide-buffer
               (goto-char (point-min))
               (while (re-search-forward desc-rx nil t)
                 (replace-match (format "[[id:%s][%s]]" id new-title) t t) (setq hit t))
               (goto-char (point-min))
               (while (re-search-forward name-rx nil t)
                 (replace-match (format "[[%s]]" new-title) t t) (setq hit t)))
              (when hit
                (orsb-core--after-write file)
                (push file changed)))))))
    (nreverse changed)))

;;;; Headings under a parent

(defun orsb-core--goto-anchor (target &optional section)
  "Move point to where children of TARGET go; return the anchor level.
The anchor is the heading titled SECTION inside TARGET when given (a
`not-found' error names the headings that exist), else TARGET's own
heading (level N), or the file (level 0).  Must run in TARGET's buffer."
  (goto-char (if (= (org-roam-node-level target) 0) (point-min) (orsb-core--heading-point target)))
  (let* ((file-p (= (org-roam-node-level target) 0))
         (start (point))
         (end (if file-p (point-max) (save-excursion (org-end-of-subtree t t) (point)))))
    (if section
        (let ((found nil))
          (unless file-p (end-of-line))
          (while (and (not found) (re-search-forward org-heading-regexp end t))
            (when (orsb-core-string= (string-trim (org-get-heading t t t t)) (string-trim section))
              (setq found (line-beginning-position))))
          (unless found
            (orsb-error-hint 'not-found (orsb-core--sections-hint start end)
                             "No heading %S in %s" section (org-roam-node-title target)))
          (goto-char found)
          (org-current-level))
      (if file-p 0 (org-current-level)))))

(defun orsb-core--shift-levels (text delta)
  "TEXT with every heading moved DELTA levels (never above level 1)."
  (if (= delta 0) text
    (replace-regexp-in-string
     "^\\(\\*+\\)\\([ \t]\\)"
     (lambda (m)
       (let ((n (length (match-string 1 m))))
         (concat (make-string (max 1 (+ n delta)) ?*) (match-string 2 m))))
     text t t)))

(defun orsb-core--insert-under-anchor (anchor-level text)
  "Insert subtree TEXT as the last child of the anchor at point.
TEXT's top heading is level 1."
  (let ((text (orsb-core--shift-levels text anchor-level)))
    (if (= anchor-level 0)
        (goto-char (point-max))
      (org-end-of-subtree t t))
    (unless (bolp) (insert "\n"))
    (unless (or (bobp) (save-excursion (forward-line -1) (looking-at "^[ \t]*$")))
      (insert "\n"))
    (let ((start (point)))
      (insert text)
      (unless (string-suffix-p "\n" text) (insert "\n"))
      start)))

(defun orsb-core-insert-heading (parent heading &rest args)
  "Add HEADING with a fresh :ID: as the last child of PARENT.
Return the new node.
ARGS is a plist: :section (a heading inside PARENT to nest under instead),
:body, :properties (alist), :todo, :tags (list), :scheduled, :deadline."
  (unless (and (stringp heading) (not (string-blank-p heading)))
    (orsb-error 'invalid-argument "title must be a non-empty string"))
  (let* ((id (org-id-new))
         (file (org-roam-node-file parent))
         (todo (plist-get args :todo))
         (tags (plist-get args :tags))
         (body (plist-get args :body))
         (planning (string-join
                    (delq nil (list (when-let ((d (plist-get args :scheduled))) (unless (string-blank-p d) (concat "SCHEDULED: " (orsb-core--org-timestamp d))))
                                    (when-let ((d (plist-get args :deadline))) (unless (string-blank-p d) (concat "DEADLINE: " (orsb-core--org-timestamp d))))))
                    " "))
         (text (concat "* " (if (and todo (not (string-blank-p todo))) (concat (upcase todo) " ") "")
                       heading
                       (if tags (concat " :" (string-join tags ":") ":") "")
                       "\n"
                       (if (string-empty-p planning) "" (concat planning "\n"))
                       ":PROPERTIES:\n:ID: " id "\n"
                       (mapconcat (lambda (c) (format ":%s: %s\n" (orsb-core--normalize-key (car c)) (cdr c)))
                                  (plist-get args :properties) "")
                       ":END:\n"
                       (if (and body (not (string-blank-p body)))
                           (concat body (if (string-suffix-p "\n" body) "" "\n"))
                         ""))))
    (dolist (tag tags)
      (unless (and (stringp tag) (string-match-p "\\`[[:alnum:]_@#%-]+\\'" tag))
        (orsb-error 'invalid-argument "Invalid tag %S" tag)))
    (with-current-buffer (find-file-noselect file)
      (unless (derived-mode-p 'org-mode) (org-mode))
      (org-with-wide-buffer
       (orsb-core--insert-under-anchor (orsb-core--goto-anchor parent (plist-get args :section)) text)))
    (orsb-core--after-write file)
    (if orsb-core-dry-run
        id
      (orsb-core-resolve id))))

;;;; Checkboxes

(defconst orsb-core--checkbox-rx
  "^\\(?:[ \t]*[-+]\\|[ \t]+\\*\\|[ \t]*[0-9]+[.)]\\)[ \t]+\\[\\([ xX-]\\)\\][ \t]+\\(.*?\\)[ \t]*$"
  "A plain-list item with a checkbox: group 1 the mark, group 2 the text.")

(defun orsb-core--checkboxes-in (start end)
  "Checkbox items between START and END in the current buffer, as plists
\(:index :checked :text :pos :heading), index counted from the start of the buffer."
  (save-excursion
    (let ((index 0) (out nil))
      (goto-char (point-min))
      (while (re-search-forward orsb-core--checkbox-rx nil t)
        (setq index (1+ index))
        (let ((pos (match-beginning 0)))
          (when (and (>= pos start) (< pos end))
            (push (list :index index
                        :checked (not (equal (match-string 1) " "))
                        :text (match-string-no-properties 2)
                        :pos pos
                        :heading (save-excursion
                                   (goto-char pos)
                                   (unless (org-before-first-heading-p)
                                     (org-back-to-heading t)
                                     (string-trim (org-get-heading t t t t)))))
                  out))))
      (nreverse out))))

(defun orsb-core-checkboxes (node &optional section)
  "Checkbox items in NODE (file or subtree), or in its heading SECTION."
  (orsb-core--with-node node
    (let* ((level (orsb-core--goto-anchor node section))
           (start (point))
           (end (if (= level 0) (point-max) (save-excursion (org-end-of-subtree t t) (point)))))
      (orsb-core--checkboxes-in start end))))

(defun orsb-core-set-checkbox (node &rest args)
  "Tick (or untick) one checkbox in NODE and return its plist with :was.
ARGS is a plist: :item (text to match, case-insensitive; an exact match
wins over a partial one), :index (position in the whole file, as
`orsb-core-tasks' reports it), :checked (default t), :section (limit the
text match to that heading).  Several matches signal `invalid-argument'
listing them."
  (let* ((item (plist-get args :item))
         (index (plist-get args :index))
         (checked (if (plist-member args :checked) (plist-get args :checked) t))
         (target nil))
    (unless (or item index)
      (orsb-error 'invalid-argument "give item (the checkbox text) or index"))
    (orsb-core--with-node node
      (let* ((boxes (if index
                        (let ((all (orsb-core--checkboxes-in (point-min) (point-max))))
                          (or (seq-filter (lambda (b) (= (plist-get b :index) index)) all)
                              (orsb-error-hint 'not-found "get_tasks lists each checkbox with its index."
                                               "No checkbox number %d in %s (it has %d)" index
                                               (orsb-core--rel (org-roam-node-file node)) (length all))))
                      (orsb-core-checkboxes node (plist-get args :section))))
             (matches (if index boxes
                        (let* ((needle (downcase (string-trim item)))
                               (exact (seq-filter (lambda (b) (string= (downcase (plist-get b :text)) needle)) boxes)))
                          (or exact (seq-filter (lambda (b) (string-search needle (downcase (plist-get b :text)))) boxes))))))
        (cond
         ((null matches)
          (orsb-error-hint 'not-found
                           (if boxes
                               (format "Checkboxes here: %s"
                                       (mapconcat (lambda (b) (format "%d %S" (plist-get b :index) (plist-get b :text)))
                                                  (seq-take boxes 12) "; "))
                             "This note has no checkboxes.")
                           "No checkbox matching %S in %s" item (org-roam-node-title node)))
         ((> (length matches) 1)
          (orsb-error-hint 'invalid-argument "Pass index (the number before each item) to pick one."
                           "%d checkboxes match %S: %s" (length matches) item
                           (mapconcat (lambda (b) (format "%d %S" (plist-get b :index) (plist-get b :text)))
                                      (seq-take matches 12) "; "))))
        (setq target (car matches))
        (goto-char (plist-get target :pos))
        (re-search-forward "\\[\\([ xX-]\\)\\]" (line-end-position))
        (replace-match (if checked "X" " ") t t nil 1)
        (org-update-checkbox-count-maybe 'all)))
    (if (eq (and checked t) (plist-get target :checked))
        ;; already in that state: nothing to save
        (orsb-core--with-node node (set-buffer-modified-p nil))
      (orsb-core--after-write (org-roam-node-file node)))
    (append (list :was (plist-get target :checked)) (plist-put (copy-sequence target) :checked (and checked t)))))

;;;; Tasks

(defun orsb-core--todo-sets (&optional text)
  "The (OPEN . DONE) TODO keyword lists for a file whose content is TEXT.
#+TODO:, #+SEQ_TODO: and #+TYP_TODO: lines win over `org-todo-keywords'."
  (let ((seqs nil))
    (when text
      (let ((pos 0))
        (while (string-match "^#\\+\\(?:SEQ_\\|TYP_\\)?TODO:[ \t]*\\(.*\\)$" text pos)
          (let ((words (match-string 1 text)))
            (setq pos (match-end 0))
            (push (split-string words "[ \t]+" t) seqs)))))
    (unless seqs
      (setq seqs (mapcar (lambda (s) (if (memq (car-safe s) '(sequence type)) (cdr s) s))
                         (if (stringp (car org-todo-keywords)) (list org-todo-keywords) org-todo-keywords))))
    (let (open done)
      (dolist (sq seqs)
        (let* ((words (mapcar (lambda (w) (replace-regexp-in-string "(.*)\\'" "" w)) sq))
               (bar (seq-position words "|")))
          (if bar
              (progn (setq open (append open (seq-take words bar)))
                     (setq done (append done (seq-drop words (1+ bar)))))
            (setq open (append open (butlast words)))
            (setq done (append done (last words))))))
      (cons (seq-uniq (delete "" open)) (seq-uniq (delete "" done))))))

(defun orsb-core--file-tasks (file)
  "TODO headings and checkboxes in FILE, as plists, in document order."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((sets (orsb-core--todo-sets (buffer-string)))
           (open (car sets)) (done (cdr sets))
           (kw-rx (concat "\\`\\(" (regexp-opt (append open done)) "\\)\\(?:[ \t]+\\|\\'\\)"))
           (file-id (save-excursion
                      (goto-char (point-min))
                      (when (looking-at "^[ \t]*:PROPERTIES:")
                        (let ((end (save-excursion (re-search-forward "^[ \t]*:END:" nil t))))
                          (and (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" end t) (match-string 1))))))
           (title (save-excursion (goto-char (point-min))
                                  (and (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" nil t) (string-trim (match-string 1)))))
           (heading nil) (heading-id nil) (sched nil) (dline nil) (prio nil)
           (index 0) (line 0) (out nil))
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (1+ line))
        (cond
         ((looking-at "^\\(\\*+\\)[ \t]+\\(.*?\\)[ \t]*$")
          (let* ((raw (match-string 2))
                 (kw (and (string-match kw-rx raw) (match-string 1 raw)))
                 (rest (if kw (substring raw (match-end 0)) raw))
                 (p (and (string-match "\\`\\[#\\([A-Z0-9]\\)\\][ \t]*" rest) (match-string 1 rest)))
                 (rest (if p (substring rest (match-end 0)) rest))
                 (text (string-trim (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" rest))))
            (setq heading text heading-id nil sched nil dline nil prio p)
            ;; planning line and drawer right below the heading
            (save-excursion
              (forward-line 1)
              (when (looking-at "^[ \t]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):")
                (let ((l (buffer-substring (line-beginning-position) (line-end-position))))
                  (when (string-match "SCHEDULED:[ \t]*\\(<[^>]+>\\)" l) (setq sched (orsb-core--stamp-date (match-string 1 l))))
                  (when (string-match "DEADLINE:[ \t]*\\(<[^>]+>\\)" l) (setq dline (orsb-core--stamp-date (match-string 1 l)))))
                (forward-line 1))
              (when (looking-at "^[ \t]*:PROPERTIES:")
                (let ((end (save-excursion (re-search-forward "^[ \t]*:END:" nil t))))
                  (when (and end (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" end t))
                    (setq heading-id (match-string 1))))))
            (when kw
              (push (list :kind "todo" :text text :state kw :done (and (member kw done) t)
                          :file file :file_id file-id :title title :heading text :heading_id heading-id
                          :scheduled sched :deadline dline :priority p :line line)
                    out))))
         ((looking-at orsb-core--checkbox-rx)
          (setq index (1+ index))
          (push (list :kind "checkbox" :text (match-string-no-properties 2)
                      :state (if (equal (match-string 1) " ") "[ ]" (format "[%s]" (match-string 1)))
                      :done (not (equal (match-string 1) " "))
                      :file file :file_id file-id :title title :heading heading :heading_id heading-id
                      :index index :scheduled sched :deadline dline :priority prio :line line)
                out)))
        (forward-line 1))
      (nreverse out))))

(defun orsb-core-tasks (&optional files)
  "Tasks (TODO headings and checkboxes) in FILES.
FILES defaults to every note outside the archive."
  (let ((archive (file-name-as-directory (file-truename (orsb-core--vault-file orsb-archive-directory)))))
    (apply #'append
           (mapcar (lambda (f) (condition-case nil (orsb-core--file-tasks f) (error nil)))
                   (or files
                       (seq-remove (lambda (f) (string-prefix-p archive (file-truename f)))
                                   (org-roam-list-files)))))))

;;;; Moving and merging

(defun orsb-core--subtree-text ()
  "Text and level of the subtree at point (point at its heading)."
  (let ((start (point))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (list (buffer-substring-no-properties start end) (org-current-level) start end)))

(defun orsb-core-move-node (node target &optional section)
  "Move heading NODE (with everything under it) to be the last child of TARGET.
TARGET is a note or a heading; SECTION names a heading inside TARGET to
go under instead.  The heading keeps its :ID:, so links still resolve."
  (when (= (org-roam-node-level node) 0)
    (orsb-error-hint 'invalid-argument "To fold a whole note into another, use merge_nodes."
                     "id must be a heading; %s is a whole note" (org-roam-node-title node)))
  (when (equal (org-roam-node-id node) (org-roam-node-id target))
    (orsb-error 'invalid-argument "Cannot move a heading under itself"))
  (let ((src (org-roam-node-file node))
        (dst (org-roam-node-file target))
        text)
    ;; refuse a target inside the moved subtree
    (when (and (equal src dst) (> (org-roam-node-level target) 0))
      (orsb-core--with-node node
        (let ((end (save-excursion (org-end-of-subtree t t) (point)))
              (tpos (orsb-core--heading-point target)))
          (when (and (> tpos (point)) (< tpos end))
            (orsb-error 'invalid-argument "Cannot move a heading under its own child")))))
    ;; cut first (and save), so the id is never in two files at once
    (orsb-core--with-node node
      (pcase-let ((`(,txt ,level ,start ,end) (orsb-core--subtree-text)))
        (setq text (orsb-core--shift-levels txt (- 1 level)))
        (delete-region start end)))
    (orsb-core--after-write src)
    (with-current-buffer (find-file-noselect dst)
      (unless (derived-mode-p 'org-mode) (org-mode))
      (org-with-wide-buffer
       (orsb-core--insert-under-anchor (orsb-core--goto-anchor target section) text)))
    (orsb-core--after-write dst)
    t))

(defun orsb-core-promote-node (node &optional dir leave-link)
  "Turn heading NODE into a note of its own in the folder DIR.
DIR defaults to the vault's top folder.
The new note keeps the heading's :ID: and properties, so links still
resolve; with LEAVE-LINK a bullet linking to it stays where the heading
was.  Returns the new file's path."
  (when (= (org-roam-node-level node) 0)
    (orsb-error 'invalid-argument "%s is already a note" (org-roam-node-title node)))
  (let ((src (org-roam-node-file node))
        title id props tags body)
    (orsb-core--with-node node
      (setq title (string-trim (org-get-heading t t t t))
            id (org-entry-get nil "ID")
            tags (org-get-tags nil t)
            props (seq-remove (lambda (c) (or (member (car c) '("ID" "CATEGORY" "BLOCKED" "ITEM" "FILE" "TODO" "PRIORITY" "TAGS" "ALLTAGS"
                                                                "TIMESTAMP" "TIMESTAMP_IA" "CLOCKSUM" "CLOCKSUM_T" "CLOSED" "DEADLINE" "SCHEDULED"))
                                              (string-prefix-p "EMBEDDING" (car c))))
                              (org-entry-properties nil 'standard)))
      (let ((sch (org-entry-get nil "SCHEDULED")) (dl (org-entry-get nil "DEADLINE")))
        ;; planning has no place on a whole note: a deadline becomes DUE-DATE,
        ;; a scheduled date a line of text, so neither is lost
        (when (and dl (not (assoc "DUE-DATE" props)))
          (setq props (append props `(("DUE-DATE" . ,(orsb-core--stamp-date dl))))))
        (when sch (setq sch (format "Scheduled: [%s]\n" (orsb-core--stamp-date sch))))
        (pcase-let* ((`(,_txt ,level ,start ,end) (orsb-core--subtree-text))
                     (from (save-excursion (org-end-of-meta-data t) (point))))
          (setq body (concat (or sch "")
                             (orsb-core--shift-levels (buffer-substring-no-properties (min from end) end) (- level))))
          (delete-region start end)
          (when leave-link
            (goto-char start)
            (insert (format "- [[id:%s][%s]]\n" id title))))))
    (orsb-core--after-write src)
    (let ((path (orsb-core--new-file-path title dir)))
      (if orsb-core-dry-run
          (progn (orsb-core--snapshot path)
                 (orsb-core--plan 'write path))
        (orsb-core--write-new-file
         path
         (cons (cons "ID" id) props)
         `(("title" . ,title) ("filetags" . ,(and tags (concat ":" (string-join tags ":") ":"))))
         (string-trim-left body "\n+")))
      path)))

(defun orsb-core-merge-node (source target &optional section)
  "Fold note SOURCE into TARGET as a heading, then remove SOURCE's file.
The heading carries SOURCE's title, :ID:, properties and tags, so every
link to SOURCE now lands on the merged section; nothing has to be
rewritten.  SECTION names a heading inside TARGET to go under.  Returns
the number of links from other notes that still resolve."
  (unless (= (org-roam-node-level source) 0)
    (orsb-error-hint 'invalid-argument "To move a heading, use move_node."
                     "id must be a whole note; %s is a heading" (org-roam-node-title source)))
  (when (equal (org-roam-node-file source) (org-roam-node-file target))
    (orsb-error 'invalid-argument "Cannot merge a note into itself"))
  (let* ((src (org-roam-node-file source))
         (dst (org-roam-node-file target))
         (backlinks (length (orsb-core-backlinks source)))
         (title (org-roam-node-title source))
         (tags (orsb-core-node-local-tags source))
         (props (orsb-core--with-node source
                  (seq-remove (lambda (c) (or (member (car c) '("ID" "CATEGORY" "BLOCKED" "ITEM" "FILE" "TODO" "PRIORITY" "TAGS" "ALLTAGS"
                                                                "TIMESTAMP" "TIMESTAMP_IA" "CLOCKSUM" "CLOCKSUM_T" "CLOSED" "DEADLINE" "SCHEDULED"))
                                                (string-prefix-p "EMBEDDING" (car c))))
                              (org-entry-properties nil 'standard))))
         (body (orsb-core-node-body source))
         (text (concat "* " title (if tags (concat " :" (string-join tags ":") ":") "") "\n"
                       ":PROPERTIES:\n:ID: " (org-roam-node-id source) "\n"
                       (mapconcat (lambda (c) (format ":%s: %s\n" (car c) (cdr c))) props "")
                       ":END:\n"
                       (orsb-core--shift-levels (string-trim-left body "\n+") 1))))
    ;; remove the source first, so the id is never indexed twice
    (orsb-core--fs-delete src)
    (with-current-buffer (find-file-noselect dst)
      (unless (derived-mode-p 'org-mode) (org-mode))
      (org-with-wide-buffer
       (orsb-core--insert-under-anchor (orsb-core--goto-anchor target section) text)))
    (orsb-core--after-write dst)
    backlinks))

;;;; Vault health

(defun orsb-core-health ()
  "Vault health as a plist: :orphans (file nodes with no id links in or out,
daily notes and the archive excluded), :broken (id links to missing nodes,
as (SOURCE-FILE . DEST-ID)), :duplicates (titles held by several notes,
as (TITLE . IDS))."
  (let* ((rows (org-roam-db-query [:select [id file level title] :from nodes]))
         (node-file (make-hash-table :test 'equal))
         (has-link (make-hash-table :test 'equal))
         (broken nil) (titles (make-hash-table :test 'equal))
         (archive (file-name-as-directory (file-truename (orsb-core--vault-file orsb-archive-directory))))
         (daily (file-name-as-directory (file-truename (orsb-core--vault-file orsb-daily-directory)))))
    (dolist (r rows) (puthash (nth 0 r) (nth 1 r) node-file))
    (dolist (l (org-roam-db-query [:select [source dest] :from links :where (= type "id")]))
      (let ((sf (gethash (car l) node-file)) (df (gethash (cadr l) node-file)))
        (cond ((null df) (when sf (push (cons sf (cadr l)) broken)))
              ((and sf (not (equal sf df))) (puthash sf t has-link) (puthash df t has-link)))))
    (dolist (r rows)
      (when (and (= (nth 2 r) 0) (not (string-prefix-p archive (file-truename (nth 1 r)))))
        (puthash (downcase (or (nth 3 r) "")) (cons (nth 0 r) (gethash (downcase (or (nth 3 r) "")) titles)) titles)))
    (list :orphans (seq-filter (lambda (r) (and (= (nth 2 r) 0)
                                                (not (gethash (nth 1 r) has-link))
                                                (not (string-prefix-p archive (file-truename (nth 1 r))))
                                                (not (string-prefix-p daily (file-truename (nth 1 r))))))
                               rows)
          :broken (nreverse broken)
          :duplicates (let (out) (maphash (lambda (k v) (when (> (length v) 1) (push (cons k (reverse v)) out))) titles) out))))

;;;; Inbox entries with refs

(defun orsb-core--inbox-lines (date)
  "Inbox bullets of DATE's daily note, or nil.
Each is a plist (:ref :date :text :resolved :pos)."
  (let ((path (orsb-core-daily-file date)))
    (when (file-exists-p path)
      (with-current-buffer (find-file-noselect path)
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward (format "^\\* %s[ \t]*$" (regexp-quote orsb-inbox-heading)) nil t)
           (let ((end (save-excursion (or (and (re-search-forward "^\\* " nil t) (match-beginning 0)) (point-max))))
                 (n 0) (out nil))
             (forward-line 1)
             (while (re-search-forward "^[ \t]*- \\(\\[\\([ xX]\\)\\] \\)?\\(.*\\)$" end t)
               (setq n (1+ n))
               (push (list :ref (format "%s:%d" date n) :date date
                           :text (match-string-no-properties 3)
                           :resolved (and (match-string 2) (not (equal (match-string 2) " ")))
                           :pos (match-beginning 0))
                     out))
             (nreverse out))))))))

(defun orsb-core-inbox (&optional days include-resolved)
  "Inbox entries of the last DAYS days (default 7), as plists with :ref.
Resolved entries are left out unless INCLUDE-RESOLVED."
  (let (out)
    (dotimes (i (or days 7))
      (let ((date (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time i)))))
        (push (seq-filter (lambda (e) (or include-resolved (not (plist-get e :resolved))))
                          (orsb-core--inbox-lines date))
              out)))
    (apply #'append out)))

(defun orsb-core-resolve-inbox (ref &optional target section)
  "Mark inbox entry REF (DATE:N) done; with TARGET, also copy it there and link it.
The entry is ticked ([X]) rather than removed, so other refs stay valid.
SECTION is a heading in TARGET for the copied bullet.  Returns the entry plist."
  (unless (and (stringp ref) (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\):\\([0-9]+\\)\\'" ref))
    (orsb-error-hint 'invalid-argument "Refs look like 2026-11-19:3; get_review lists open inbox entries with theirs."
                     "Not an inbox ref: %S" ref))
  (let* ((date (match-string 1 ref))
         (entry (seq-find (lambda (e) (equal (plist-get e :ref) ref)) (orsb-core--inbox-lines date)))
         (path (orsb-core-daily-file date)))
    (unless entry
      (orsb-error-hint 'not-found "get_review lists open inbox entries with their refs."
                       "No inbox entry %s" ref))
    (when (plist-get entry :resolved)
      (orsb-error 'refused "Inbox entry %s is already resolved" ref))
    (when target
      (let ((text (string-trim (replace-regexp-in-string "\\`\\[[0-9]\\{4\\}-[^]]*\\][ \t]*" "" (plist-get entry :text)))))
        (if section
            (orsb-core-update-section target section (concat "- " text) "append" t)
          (orsb-core-append-body target (concat "- " text)))))
    (with-current-buffer (find-file-noselect path)
      (org-with-wide-buffer
       (goto-char (plist-get entry :pos))
       (when (re-search-forward "^\\([ \t]*- \\)\\(\\[ \\] \\)?" (line-end-position) t)
         (replace-match "\\1[X] " t nil))
       (when target
         (end-of-line)
         (insert (format " → filed to [[id:%s][%s]]" (org-roam-node-id target) (org-roam-node-title target))))))
    (orsb-core--after-write path)
    entry))

(provide 'orsb-core)
;;; orsb-core.el ends here
