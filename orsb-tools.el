;;; orsb-tools.el --- The 2.0 MCP tool contract -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.2"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:

;; Eighteen orthogonal MCP tools with one identifier parameter (`id': an
;; org-roam id, a vault path, or an exact title), one response envelope
;;
;;   {"ok":true,"data":{...}}
;;   {"ok":false,"error":{"code":"not_found|invalid_argument|refused|unavailable|internal",
;;                        "message":"...","hint":"..."}}   (+ MCP isError)
;;
;; and one status vocabulary (`orsb-status-values').  Everything is
;; implemented on `orsb-core'.  The pre-2.0 tool names are registered as
;; thin argument mappers over these tools, marked deprecated, while
;; `orsb-mcp-legacy-tools' is non-nil; they answer in the old
;; {"success": ...} shape.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-roam)
(require 'orsb-core)
;; Semantic search, stale detection and the embedding queue. Optional: the
;; tools degrade to "unavailable" errors when it is not loadable.
(require 'orsb-search nil t)

(declare-function orsb-search-similar "orsb-search")
(declare-function orsb-search-file-stale-p "orsb-search")
(declare-function orsb-search-generate-file "orsb-search")
(declare-function orsb-search-backfill-hashes "orsb-search")
(declare-function orsb-search-enqueue "orsb-search")
(declare-function org-roam-mcp-http--register-tool "org-roam-mcp-http")
(defvar org-roam-mcp-http--tools)

;;;; Customization

(defvaralias 'orsb-node-types 'orsb-directories)

(defcustom orsb-status-values '("active" "waiting" "blocked" "someday" "done" "cancelled")
  "Allowed values of the STATUS property.  `stale' is computed, never stored."
  :type '(repeat string)
  :group 'orsb)

(defcustom orsb-blog-status-values '("idea" "stub" "draft" "published")
  "Allowed STATUS values for blog nodes, whose lifecycle differs from projects'."
  :type '(repeat string)
  :group 'orsb)

(defcustom orsb-status-aliases
  '(("completed" . "done") ("finished" . "done") ("closed" . "done")
    ("shelved" . "someday") ("paused" . "waiting") ("on-hold" . "waiting")
    ("superseded" . "cancelled") ("dropped" . "cancelled")
    ("idea" . "someday") ("todo" . "active") ("in-progress" . "active")
    ("draft" . "active") ("published" . "done") ("stub" . "someday"))
  "Legacy STATUS spellings and the canonical value each maps to."
  :type '(alist :key-type string :value-type string)
  :group 'orsb)

(defcustom orsb-stale-days 5
  "Days without a change after which an unfinished node counts as stale."
  :type 'integer
  :group 'orsb)

(defcustom orsb-mcp-legacy-tools t
  "When non-nil the pre-2.0 tool names stay registered, marked deprecated."
  :type 'boolean
  :group 'orsb)

(defcustom orsb-known-property-keys
  '("NODE-TYPE" "STATUS" "NEXT-ACTION" "CONTEXT" "LAST-CONTACT" "DUE-DATE" "ONE-LINER" "PRIORITY" "CREATED")
  "Property keys with a conventional meaning, reported by get_schema."
  :type '(repeat string)
  :group 'orsb)

(defconst orsb-tools-version "2.0.0")

;;;; Argument helpers

(defun orsb-arg (args key &optional default)
  "Return KEY from the JSON-decoded ARGS alist, or DEFAULT.
JSON null and false both count as absent for scalars."
  (let ((v (alist-get key args)))
    (if (or (null v) (eq v :json-null) (eq v :json-false)) default v)))

(defun orsb-arg-bool (args key)
  "Return t when KEY in ARGS is JSON true."
  (let ((v (alist-get key args)))
    (and v (not (eq v :json-false)) (not (eq v :json-null)) (not (equal v "false")))))

(defun orsb-arg-list (args key)
  "Return KEY in ARGS as a list of strings (accepts array, CSV string, or nil)."
  (let ((v (alist-get key args)))
    (cond ((or (null v) (eq v :json-null)) nil)
          ((vectorp v) (mapcar (lambda (x) (format "%s" x)) v))
          ((listp v) (mapcar (lambda (x) (format "%s" x)) v))
          ((stringp v) (split-string v "[,;\n]" t "[ \t]+"))
          (t (list (format "%s" v))))))

(defun orsb-arg-int (args key default)
  "Return KEY in ARGS as an integer, or DEFAULT."
  (let ((v (orsb-arg args key)))
    (cond ((integerp v) v)
          ((numberp v) (round v))
          ((and (stringp v) (string-match-p "\\`[0-9]+\\'" v)) (string-to-number v))
          (t default))))

(defun orsb-arg-props (args key)
  "Return KEY in ARGS as a property alist with string keys."
  (let ((v (alist-get key args)))
    (cond ((or (null v) (eq v :json-null)) nil)
          ((hash-table-p v) (let (out) (maphash (lambda (k val) (push (cons (format "%s" k) val) out)) v) (nreverse out)))
          ((listp v) (mapcar (lambda (c) (cons (format "%s" (car c)) (cdr c))) v))
          (t (orsb-error 'invalid-argument "%s must be an object" key)))))

(defun orsb-require (args &rest keys)
  "Signal `invalid-argument' unless every key in KEYS is present in ARGS."
  (dolist (k keys)
    (when (null (orsb-arg args k))
      (orsb-error 'invalid-argument "Missing required argument: %s" k))))

(defun orsb--vector (list)
  "LIST as a JSON array (empty list encodes as [] not null)."
  (vconcat list))

(defun orsb--object (alist)
  "ALIST as a JSON object (empty alist encodes as {} not null)."
  (or alist :empty-object))

;;;; Records

(defun orsb-tools--relative (file)
  "FILE relative to `org-roam-directory'."
  (if (and file (string-prefix-p (file-truename org-roam-directory) (file-truename file)))
      (file-relative-name (file-truename file) (file-truename org-roam-directory))
    file))

(defun orsb-tools--days-since (mtime)
  "Whole days since MTIME."
  (if mtime (/ (- (float-time) (float-time mtime)) 86400.0) 0))

(defun orsb-tools--stale-p (status days)
  "Whether a node with STATUS untouched for DAYS counts as stale."
  (and (member (or status "active") '("active" "waiting" "blocked"))
       (>= days orsb-stale-days)))

(defun orsb-tools--light-record (node)
  "Cheap record built from the db only (no file read)."
  (let* ((props (org-roam-node-properties node))
         (status (cdr (assoc "STATUS" props)))
         (days (orsb-tools--days-since (org-roam-node-file-mtime node))))
    `((id . ,(org-roam-node-id node))
      (title . ,(org-roam-node-title node))
      (file . ,(orsb-tools--relative (org-roam-node-file node)))
      (level . ,(org-roam-node-level node))
      (node_type . ,(cdr (assoc "NODE-TYPE" props)))
      (status . ,status)
      (todo . ,(org-roam-node-todo node))
      (tags . ,(orsb--vector (org-roam-node-tags node)))
      (modified . ,(let ((m (org-roam-node-file-mtime node)))
                     (when m (format-time-string "%Y-%m-%dT%H:%M:%S%z" m))))
      (days_since_modified . ,(round days))
      (stale . ,(if (orsb-tools--stale-p status days) t :json-false)))))

(defun orsb-tools--links (node)
  "Return (links_to . links_from) for NODE as lists of {id,title}."
  (let ((to (mapcar (lambda (row)
                      (let ((n (org-roam-node-from-id (car row))))
                        `((id . ,(car row)) (title . ,(and n (org-roam-node-title n))))))
                    ;; A file node owns the links under every heading in its
                    ;; file; a heading node only its own.
                    (if (= (org-roam-node-level node) 0)
                        (org-roam-db-query [:select :distinct [links:dest] :from links
                                            :inner-join nodes :on (= links:source nodes:id)
                                            :where (and (= nodes:file $s1) (= links:type "id"))]
                                           (org-roam-node-file node))
                      (org-roam-db-query [:select :distinct [dest] :from links
                                          :where (and (= source $s1) (= type "id"))]
                                         (org-roam-node-id node)))))
        (from (mapcar (lambda (bl)
                        (let ((n (org-roam-backlink-source-node bl)))
                          `((id . ,(org-roam-node-id n)) (title . ,(org-roam-node-title n)))))
                      (org-roam-backlinks-get node))))
    (cons to from)))

(defun orsb-tools--full-record (node &optional include-body)
  "Complete record for get_node."
  (let* ((rec (orsb-core-node-record node include-body))
         (light (orsb-tools--light-record node))
         (links (orsb-tools--links node)))
    (append
     `((id . ,(alist-get 'id rec))
       (title . ,(alist-get 'title rec))
       (file . ,(orsb-tools--relative (alist-get 'file rec)))
       (level . ,(alist-get 'level rec))
       (node_type . ,(alist-get 'node_type rec))
       (status . ,(alist-get 'status rec))
       (todo . ,(alist-get 'todo rec))
       (tags . ,(alist-get 'tags rec))
       (properties . ,(alist-get 'properties rec))
       (keywords . ,(alist-get 'keywords rec))
       (modified . ,(alist-get 'modified rec))
       (days_since_modified . ,(alist-get 'days_since_modified light))
       (stale . ,(alist-get 'stale light))
       (embedding_stale . ,(if (and (fboundp 'orsb-search-file-stale-p)
                                    (ignore-errors (orsb-search-file-stale-p (org-roam-node-file node))))
                               t :json-false))
       (links_to . ,(orsb--vector (car links)))
       (links_from . ,(orsb--vector (cdr links))))
     (when include-body `((body . ,(alist-get 'body rec)))))))

(defun orsb-tools--snippet (node &optional length)
  "First LENGTH characters of NODE's body, whitespace collapsed."
  (let* ((body (condition-case nil (orsb-core-node-body node) (error "")))
         ;; drop sub-heading drawers and #+ lines, keep heading text
         (clean (replace-regexp-in-string
                 "^[ \t]*\\(:[A-Za-z0-9_-]+:.*\\|#\\+.*\\)$" ""
                 (replace-regexp-in-string "^\\*+[ \t]+" "" body)))
         (flat (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " clean)))
         (n (or length 240)))
    (if (> (length flat) n) (concat (substring flat 0 n) "…") flat)))

;;;; Status vocabulary

(defun orsb-tools--status-values-for (node-or-type)
  "The status vocabulary that applies to NODE-OR-TYPE (a node or a type string)."
  (if (if (stringp node-or-type) (equal node-or-type "blog") (and node-or-type (orsb-core-blog-node-p node-or-type)))
      orsb-blog-status-values
    orsb-status-values))

(defun orsb-tools--canonical-status (value)
  "Return the canonical status for VALUE, or nil when it is not recognized."
  (let ((v (downcase (string-trim (or value "")))))
    (cond ((member v orsb-status-values) v)
          ((assoc v orsb-status-aliases) (cdr (assoc v orsb-status-aliases)))
          (t nil))))

(defun orsb-tools--check-status (value &optional node-or-type)
  "Return VALUE if it is an allowed status for NODE-OR-TYPE, else signal."
  (let* ((v (downcase (string-trim value)))
         (allowed (orsb-tools--status-values-for node-or-type)))
    (if (member v allowed) v
      (orsb-error 'invalid-argument "status must be one of %s (got %S)"
                  (string-join allowed ", ") value))))

;;;; Tool implementations (each takes the decoded args alist, returns data)

(defun orsb-tool-search (args)
  "search: title | contextual | semantic."
  (orsb-require args 'query)
  (let* ((query (orsb-arg args 'query))
         (mode (or (orsb-arg args 'mode) "semantic"))
         (type (orsb-arg args 'node_type))
         (limit (min 25 (orsb-arg-int args 'limit 10)))
         (cutoff (let ((c (orsb-arg args 'cutoff))) (if (numberp c) c 0.5)))
         (hits nil))
    (pcase mode
      ("title"
       (dolist (row (org-roam-db-query [:select [id] :from nodes
                                        :where (like title $s1) :order-by [(asc level) (asc title)]]
                                       (concat "%" query "%")))
         (when-let ((n (org-roam-node-from-id (car row))))
           (push (cons n nil) hits))))
      ("contextual"
       (let ((words (split-string (downcase query) "[ \t]+" t)))
         (dolist (file (org-roam-list-files))
           (when-let* ((text (condition-case nil
                                 (with-temp-buffer (insert-file-contents file) (downcase (buffer-string)))
                               (error nil)))
                       (score (seq-count (lambda (w) (string-search w text)) words))
                       (_ (> score 0))
                       (n (orsb-core--first-node-in-file file)))
             (push (cons n (/ (float score) (length words))) hits)))))
      ("semantic"
       (unless (fboundp 'orsb-search-similar)
         (orsb-error 'unavailable "Semantic search is not loaded (orsb-search)"))
       (let ((seen (make-hash-table :test 'equal)))
         (dolist (r (condition-case err
                        (orsb-search-similar query (* 3 limit) cutoff)
                      (error (orsb-error 'unavailable "Semantic search failed: %s" (error-message-string err)))))
           (let ((file (car r)) (score (cadr r)))
             (unless (gethash file seen)
               (when-let ((n (orsb-core--first-node-in-file file)))
                 (puthash file t seen)
                 (push (cons n score) hits)))))))
      (_ (orsb-error 'invalid-argument "mode must be title, contextual or semantic")))
    (setq hits (nreverse hits))
    (when type
      (setq hits (seq-filter (lambda (h) (equal (cdr (assoc "NODE-TYPE" (org-roam-node-properties (car h)))) type)) hits)))
    (when (member mode '("contextual" "semantic"))
      (setq hits (seq-sort-by #'cdr #'> hits)))
    (setq hits (seq-take hits limit))
    `((query . ,query) (mode . ,mode) (total . ,(length hits))
      (hits . ,(orsb--vector
                (mapcar (lambda (h)
                          (append (orsb-tools--light-record (car h))
                                  `((score . ,(cdr h)) (snippet . ,(orsb-tools--snippet (car h))))))
                        hits))))))

(defun orsb-tool-get-node (args)
  "get_node."
  (orsb-require args 'id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (section (orsb-arg args 'section))
         (include-body (not (eq (alist-get 'include_body args) :json-false)))
         (rec (orsb-tools--full-record node (and include-body (not section)))))
    (if section
        (append rec `((section . ,section) (body . ,(orsb-core-section-body node section))))
      rec)))

(defun orsb-tools--list (args)
  "Shared filter/sort for list_nodes and get_projects; returns node records."
  (let* ((type (orsb-arg args 'node_type))
         (status (orsb-arg args 'status))
         (stale (alist-get 'stale args))
         (tags (orsb-arg-list args 'tags))
         (limit (orsb-arg-int args 'limit 50))
         (sort-by (or (orsb-arg args 'sort_by) "modified"))
         (nodes (seq-filter (lambda (n) (= (org-roam-node-level n) 0)) (org-roam-node-list)))
         (records (mapcar #'orsb-tools--light-record nodes)))
    (when type (setq records (seq-filter (lambda (r) (equal (alist-get 'node_type r) type)) records)))
    (when status
      (let ((s (or (orsb-tools--canonical-status status) status)))
        (setq records (seq-filter (lambda (r) (equal (orsb-tools--canonical-status (alist-get 'status r)) s)) records))))
    (when (and stale (not (eq stale :json-null)))
      (let ((want (not (eq stale :json-false))))
        (setq records (seq-filter (lambda (r) (eq (eq (alist-get 'stale r) t) want)) records))))
    (when tags
      (setq records (seq-filter (lambda (r) (seq-every-p (lambda (tg) (seq-contains-p (alist-get 'tags r) tg)) tags)) records)))
    (setq records
          (pcase sort-by
            ("title" (seq-sort-by (lambda (r) (downcase (or (alist-get 'title r) ""))) #'string< records))
            ("created" (seq-sort-by (lambda (r) (or (alist-get 'modified r) "")) #'string< records))
            (_ (seq-sort-by (lambda (r) (or (alist-get 'modified r) "")) #'string> records))))
    (seq-take records limit)))

(defun orsb-tool-list-nodes (args)
  "list_nodes."
  (let ((records (orsb-tools--list args)))
    `((total . ,(length records)) (nodes . ,(orsb--vector records)))))

(defun orsb-tool-get-projects (args)
  "get_projects: project nodes, optional status / stale filter."
  (let* ((orsb-stale-days (orsb-arg-int args 'days_threshold orsb-stale-days))
         (records (orsb-tools--list (append `((node_type . "project") (limit . 500)) args))))
    `((total . ,(length records)) (stale_days . ,orsb-stale-days)
      (projects . ,(orsb--vector
                    (mapcar (lambda (r)
                              (let ((n (org-roam-node-from-id (alist-get 'id r))))
                                (append r `((next_action . ,(cdr (assoc "NEXT-ACTION" (org-roam-node-properties n))))))))
                            records))))))

(defun orsb-tool-create-node (args)
  "create_node, typed."
  (orsb-require args 'node_type 'title)
  (let* ((type (orsb-arg args 'node_type))
         (status (when-let ((s (orsb-arg args 'status))) (orsb-tools--check-status s type)))
         (node (orsb-core-create-node type (orsb-arg args 'title)
                                      :body (orsb-arg args 'body)
                                      :status status
                                      :next-action (orsb-arg args 'next_action)
                                      :context (orsb-arg args 'context)
                                      :follow-ups (orsb-arg-list args 'follow_ups)
                                      :one-liner (orsb-arg args 'one_liner)
                                      :due-date (orsb-arg args 'due_date)
                                      :hugo-section (orsb-arg args 'hugo_section)
                                      :tags (orsb-arg-list args 'tags)
                                      :properties (orsb-arg-props args 'properties))))
    (orsb-tools--full-record node nil)))

(defun orsb-tool-add-heading (args)
  "add_heading."
  (orsb-require args 'id 'heading)
  (let* ((parent (orsb-core-resolve (orsb-arg args 'id)))
         (todo (orsb-arg args 'todo))
         (heading (if todo (concat (upcase todo) " " (orsb-arg args 'heading)) (orsb-arg args 'heading)))
         (node (orsb-core-add-heading parent heading (orsb-arg args 'body)
                                      (orsb-arg-props args 'properties)
                                      (orsb-arg-int args 'level 1))))
    (orsb-tools--full-record node nil)))

(defun orsb-tool-set-node (args)
  "set_node: title / status / todo / properties / tags / keywords."
  (orsb-require args 'id)
  (let ((node (orsb-core-resolve (orsb-arg args 'id)))
        (changed nil))
    (when-let ((title (orsb-arg args 'title)))
      (orsb-core-set-title node title) (push "title" changed)
      (setq node (orsb-core-resolve (org-roam-node-id node))))
    (when-let ((status (orsb-arg args 'status)))
      (orsb-core-set-properties node `(("STATUS" . ,(orsb-tools--check-status status node)))) (push "status" changed))
    (when (assq 'todo args)
      (orsb-core-set-todo node (orsb-arg args 'todo)) (push "todo" changed))
    (when-let ((props (orsb-arg-props args 'properties)))
      (orsb-core-set-properties node props) (push "properties" changed))
    (when (assq 'tags args)
      (orsb-core-set-tags node (orsb-arg-list args 'tags)) (push "tags" changed))
    (let ((add (orsb-arg-list args 'tags_add)) (remove (orsb-arg-list args 'tags_remove)))
      (when (or add remove)
        (let ((current (orsb-core-node-local-tags node)))
          (orsb-core-set-tags node (seq-uniq (append (seq-remove (lambda (tg) (member tg remove)) current) add))))
        (push "tags" changed)))
    (when-let ((kws (orsb-arg-props args 'keywords)))
      (orsb-core-set-keywords node kws) (push "keywords" changed))
    (unless changed (orsb-error 'invalid-argument "Nothing to change: give title, status, todo, properties, tags, tags_add, tags_remove or keywords"))
    (append `((changed . ,(orsb--vector (nreverse changed))))
            (orsb-tools--full-record (orsb-core-resolve (org-roam-node-id node)) nil))))

(defun orsb-tool-update-body (args)
  "update_body: append | prepend | replace, optionally within a section."
  (orsb-require args 'id 'content)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (content (orsb-arg args 'content))
         (section (orsb-arg args 'section))
         (mode (or (orsb-arg args 'mode) "append"))
         (force (orsb-arg-bool args 'force)))
    (unless (member mode '("append" "prepend" "replace"))
      (orsb-error 'invalid-argument "mode must be append, prepend or replace"))
    (cond
     (section (orsb-core-update-section node section content mode))
     ((equal mode "replace")
      (if (= (org-roam-node-level node) 0)
          (orsb-core-replace-file-body node content force)
        (orsb-core-set-body node content)))
     (t (orsb-core-append-body node content (equal mode "prepend"))))
    `((id . ,(org-roam-node-id node)) (file . ,(orsb-tools--relative (org-roam-node-file node)))
      (mode . ,mode) (section . ,section))))

(defun orsb-tool-delete-node (args)
  "delete_node: a file (optionally archived) or a heading subtree.
Accepts the pre-2.0 `node_id' / `identifier' spellings too."
  (let* ((id (or (orsb-arg args 'id) (orsb-arg args 'node_id) (orsb-arg args 'identifier)
                 (orsb-error 'invalid-argument "Missing required argument: id")))
         (node (orsb-core-resolve id))
         (archive (orsb-arg-bool args 'archive))
         (level (org-roam-node-level node))
         (result (orsb-core-delete-node node archive)))
    `((id . ,(org-roam-node-id node)) (level . ,level)
      (action . ,(cond ((> level 0) "deleted") (archive "archived") (t "deleted")))
      (file . ,(orsb-tools--relative (or result (org-roam-node-file node)))))))

(defun orsb-tool-link-nodes (args)
  "link_nodes: add or remove an [[id:...]] link from id to target_id."
  (orsb-require args 'id 'target_id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (target (orsb-core-resolve (orsb-arg args 'target_id)))
         (action (or (orsb-arg args 'action) "add")))
    (pcase action
      ("add" (orsb-core-link node target (orsb-arg args 'section))
             `((action . "added") (id . ,(org-roam-node-id node)) (target_id . ,(org-roam-node-id target))
               (target_title . ,(org-roam-node-title target))))
      ("remove" `((action . "removed") (id . ,(org-roam-node-id node)) (target_id . ,(org-roam-node-id target))
                  (removed . ,(orsb-core-unlink node (org-roam-node-id target)))))
      (_ (orsb-error 'invalid-argument "action must be add or remove")))))

(defun orsb-tool-add-daily-entry (args)
  "add_daily_entry."
  (orsb-require args 'title 'points)
  (let ((path (orsb-core-add-daily-entry (orsb-arg args 'title)
                                         (orsb-arg-list args 'points)
                                         (orsb-arg-list args 'next_steps)
                                         (orsb-arg-list args 'tags)
                                         (orsb-arg args 'timestamp)
                                         (equal (orsb-arg args 'type) "todo"))))
    `((file . ,(orsb-tools--relative path)) (title . ,(orsb-arg args 'title)))))

(defun orsb-tool-get-daily (args)
  "get_daily {date}."
  `((date . ,(or (orsb-arg args 'date) (format-time-string "%Y-%m-%d")))
    (content . ,(orsb-core-daily-content (orsb-arg args 'date)))))

(defun orsb-tool-log-to-inbox (args)
  "log_to_inbox {text, linked_id, category}."
  (orsb-require args 'text)
  (let* ((linked (when-let ((l (orsb-arg args 'linked_id))) (orsb-core-resolve l)))
         (category (orsb-arg args 'category))
         (text (if category (format "[%s] %s" category (orsb-arg args 'text)) (orsb-arg args 'text)))
         (created (orsb-core-log-to-inbox text linked)))
    `((logged . t) (linked_id . ,(and linked (org-roam-node-id linked)))
      (created_people . ,(orsb--vector created)))))

(defun orsb-tool-get-followups (args)
  "get_followups {dangling}."
  (if (orsb-arg-bool args 'dangling)
      (let ((items (orsb-core-dangling-followups)))
        `((total . ,(length items))
          (untracked_people . ,(orsb--vector
                                (mapcar (lambda (p) `((name . ,(plist-get p :name)) (item . ,(plist-get p :item))
                                                      (file . ,(orsb-tools--relative (plist-get p :file)))))
                                        items)))))
    (let ((people (orsb-core-followups)))
      `((total . ,(length people))
        (people . ,(orsb--vector
                    (mapcar (lambda (p)
                              (append (orsb-tools--light-record (plist-get p :node))
                                      `((pending_followups . ,(orsb--vector (plist-get p :followups)))
                                        (followup_count . ,(length (plist-get p :followups))))))
                            people)))))))

(defun orsb-tool-get-digest (args)
  "get_digest {days}: projects, follow-ups and the recent inbox in one call."
  (let* ((projects (orsb-tool-get-projects '((status . "active"))))
         (stale (orsb-tool-get-projects '((stale . t))))
         (followups (orsb-tool-get-followups nil))
         (dangling (orsb-tool-get-followups '((dangling . t))))
         (inbox (orsb-core-inbox-entries (orsb-arg-int args 'days 7))))
    `((generated_at . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
      (active_projects . ,projects)
      (stale_projects . ,stale)
      (pending_followups . ,followups)
      (dangling_followups . ,dangling)
      (inbox . ,(orsb--vector (mapcar (lambda (d) `((date . ,(car d)) (entries . ,(orsb--vector (cdr d))))) inbox))))))

(defun orsb-tool-get-blog-status (_args)
  "get_blog_status."
  (let ((s (orsb-core-blog-status)))
    `((drafts . ((total . ,(length (plist-get s :drafts)))
                 (items . ,(orsb--vector
                            (mapcar (lambda (d)
                                      (append (orsb-tools--light-record (plist-get d :node))
                                              `((sections_written . ,(car (plist-get d :outline)))
                                                (sections . ,(cdr (plist-get d :outline))))))
                                    (plist-get s :drafts))))))
      (published . ((total . ,(length (plist-get s :published)))
                    (recent . ,(orsb--vector (mapcar #'orsb-tools--light-record (plist-get s :published))))))
      (ideas_for_blog . ((total . ,(length (plist-get s :ideas)))
                         (items . ,(orsb--vector (mapcar #'orsb-tools--light-record (plist-get s :ideas)))))))))

(defun orsb-tool-sync (args)
  "sync {id, embeddings, full, wait}.
With id: update that note's db row now (and its embeddings if asked).
Otherwise a full scan is queued for idle time so the request path stays
responsive; wait=true runs it synchronously instead.  full=true forces a
rebuild (the only remedy when the db and the files disagree)."
  (let ((id (orsb-arg args 'id))
        (embeddings (orsb-arg-bool args 'embeddings))
        (full (orsb-arg-bool args 'full))
        (wait (orsb-arg-bool args 'wait)))
    (when (and embeddings (not (fboundp 'orsb-search-generate-file)))
      (orsb-error 'unavailable "Embedding generation is not loaded (orsb-search)"))
    (cond
     (id (let* ((node (orsb-core-resolve id))
                (file (org-roam-node-file node)))
           (org-roam-db-update-file file)
           (let ((done (when embeddings (orsb-search-generate-file file full))))
             `((synced . ,(orsb-tools--relative file))
               (embeddings . ,(if embeddings t :json-false))
               (generated . ,(or (car done) 0))
               (skipped . ,(or (cdr done) 0))))))
     (t
      (let ((work (lambda ()
                    (if full (org-roam-db-sync 'force) (org-roam-db-sync))
                    (when embeddings
                      ;; hashes first so unchanged notes are not re-embedded,
                      ;; then queue every note for the idle worker
                      (orsb-search-backfill-hashes)
                      (dolist (f (org-roam-list-files)) (orsb-search-enqueue f))))))
        (if wait
            (progn (funcall work)
                   `((synced . "db") (full . ,(if full t :json-false)) (embeddings . ,(if embeddings t :json-false))))
          (run-with-idle-timer 1 nil work)
          `((queued . t) (full . ,(if full t :json-false)) (embeddings . ,(if embeddings t :json-false))
            (what . "database scan (and embeddings if asked) runs when Emacs is idle; pass wait=true to block"))))))))

(defun orsb-tool-get-schema (_args)
  "get_schema: the vocabulary this server enforces."
  `((version . ,orsb-tools-version)
    (id_forms . ["org-roam id" "path relative to the vault, or absolute" "exact title or alias"])
    (node_types . ,(orsb--vector (mapcar (lambda (c) `((type . ,(car c)) (directory . ,(cdr c)))) orsb-directories)))
    (status_values . ,(orsb--vector orsb-status-values))
    (blog_status_values . ,(orsb--vector orsb-blog-status-values))
    (status_aliases . ,(orsb--object orsb-status-aliases))
    (stale_days . ,orsb-stale-days)
    (property_keys . ,(orsb--vector orsb-known-property-keys))
    (daily_directory . ,orsb-daily-directory)
    (inbox_heading . ,orsb-inbox-heading)
    (hugo_sections . ,(orsb--vector orsb-hugo-sections))
    (blog_enabled . ,(if orsb-hugo-base-dir t :json-false))
    (legacy_tools . ,(if orsb-mcp-legacy-tools t :json-false))
    (envelope . "{ok:true,data} | {ok:false,error:{code,message,hint}} with isError")))

;;;; Envelope

(defun orsb-tools--hint (code)
  "A hint for error CODE."
  (pcase code
    ('not-found "Use search (mode title) to find the id, or pass a vault-relative path.")
    ('invalid-argument "Call get_schema for the accepted values.")
    ('refused "The operation was blocked by a safety rule; read the message.")
    ('unavailable "A required module is not loaded on the server.")
    (_ "Unexpected server error; retrying will not help.")))

(defun orsb-tools--clean-args (args)
  "ARGS as an alist, treating a non-object (e.g. []) as no arguments."
  (if (and (listp args) (or (null args) (consp (car args)))) args nil))

(defun orsb-tools--run (fn args)
  "Run FN on ARGS; return (:ok DATA) or (:error CODE MESSAGE)."
  (condition-case err
      (list :ok (funcall fn (orsb-tools--clean-args args)))
    (orsb-error (list :error (nth 1 err) (nth 2 err)))
    (error (list :error 'internal (error-message-string err)))))

(defun orsb-tools--envelope (fn args)
  "Run FN on ARGS and return the 2.0 JSON envelope."
  (let ((r (orsb-tools--run fn args)))
    (if (eq (car r) :ok)
        (json-encode `((ok . t) (data . ,(orsb--object (nth 1 r)))))
      (json-encode `((ok . :json-false)
                     (error . ((code . ,(replace-regexp-in-string "-" "_" (symbol-name (nth 1 r))))
                               (message . ,(nth 2 r))
                               (hint . ,(orsb-tools--hint (nth 1 r))))))))))

;;;; Registration

(defconst orsb-tools--id-schema
  '(id . ((type . "string") (description . "Node id, vault-relative path, or exact title")))
  "The shared id parameter.")

(defconst orsb-tools--definitions
  `(("search" orsb-tool-search
     "Search the notes. mode=title (substring of titles, fast), contextual (all query words in the text), semantic (embedding similarity, default). Returns hits with id, title, type, status, score and a snippet; call get_node on an id for the full note."
     ("query")
     ((query . ((type . "string") (description . "Search text")))
      (mode . ((type . "string") (enum . ("title" "contextual" "semantic")) (default . "semantic")))
      (node_type . ((type . "string") (description . "Only nodes of this NODE-TYPE")))
      (limit . ((type . "integer") (default . 10) (description . "Max hits (<= 25)")))
      (cutoff . ((type . "number") (default . 0.5) (description . "Minimum similarity for semantic mode")))))
    ("get_node" orsb-tool-get-node
     "Read one node (file or heading): title, type, status, todo, tags, properties (its :PROPERTIES: drawer), keywords (#+KEY: lines of the file), links, and the body. section=Heading returns only that heading's body."
     ("id")
     (,orsb-tools--id-schema
      (section . ((type . "string") (description . "Return only this heading's body")))
      (include_body . ((type . "boolean") (default . t)))))
    ("list_nodes" orsb-tool-list-nodes
     "List file-level nodes with optional filters. stale=true selects unfinished nodes untouched for stale_days. Cheap: reads the database only."
     ()
     ((node_type . ((type . "string")))
      (status . ((type . "string") (description . "A status value (see get_schema)")))
      (stale . ((type . "boolean")))
      (tags . ((type . "array") (items . ((type . "string"))) (description . "All of these tags")))
      (limit . ((type . "integer") (default . 50)))
      (sort_by . ((type . "string") (enum . ("modified" "title" "created")) (default . "modified")))))
    ("create_node" orsb-tool-create-node
     "Create a typed note (project, person, idea, admin, blog, note, reference, howto) in its conventional directory with the conventional properties, and return it. Extra properties, tags and a status may be given."
     ("node_type" "title")
     ((node_type . ((type . "string") (enum . ("project" "person" "idea" "admin" "blog" "note" "reference" "howto"))))
      (title . ((type . "string")))
      (body . ((type . "string") (description . "Initial body text")))
      (status . ((type . "string")))
      (next_action . ((type . "string") (description . "project: NEXT-ACTION")))
      (due_date . ((type . "string") (description . "admin: YYYY-MM-DD")))
      (one_liner . ((type . "string") (description . "idea: one-line summary")))
      (context . ((type . "string") (description . "person: how you know them")))
      (follow_ups . ((type . "array") (items . ((type . "string"))) (description . "person: follow-up items")))
      (hugo_section . ((type . "string") (description . "blog: Hugo section")))
      (properties . ((type . "object") (additionalProperties . ((type . "string")))))
      (tags . ((type . "array") (items . ((type . "string")))))))
    ("add_heading" orsb-tool-add-heading
     "Add a heading node (with its own id) under a note. Returns the new node."
     ("id" "heading")
     (,orsb-tools--id-schema
      (heading . ((type . "string") (description . "Heading text, without stars")))
      (body . ((type . "string")))
      (properties . ((type . "object") (additionalProperties . ((type . "string")))))
      (level . ((type . "integer") (default . 1)))
      (todo . ((type . "string") (description . "TODO keyword to start with, e.g. TODO")))))
    ("set_node" orsb-tool-set-node
     "Change a node's metadata without touching its body: title, status (validated, see get_schema), todo keyword (headings only), properties (merged into the :PROPERTIES: drawer; null deletes; file-level drawer for a file node), tags / tags_add / tags_remove, and keywords (#+KEY: lines, file nodes only)."
     ("id")
     (,orsb-tools--id-schema
      (title . ((type . "string")))
      (status . ((type . "string")))
      (todo . ((type . "string") (description . "TODO keyword; empty clears")))
      (properties . ((type . "object") (additionalProperties . ((type . ("string" "null"))))))
      (tags . ((type . "array") (items . ((type . "string"))) (description . "Replace all own tags")))
      (tags_add . ((type . "array") (items . ((type . "string")))))
      (tags_remove . ((type . "array") (items . ((type . "string")))))
      (keywords . ((type . "object") (additionalProperties . ((type . "string")))))))
    ("update_body" orsb-tool-update-body
     "Edit a node's text: append (default), prepend, or replace; section=Heading targets one heading of the file (created if missing). A whole-file replace that would drop headings or shrink the note a lot is refused unless force=true; a .bak is written first."
     ("id" "content")
     (,orsb-tools--id-schema
      (content . ((type . "string")))
      (section . ((type . "string")))
      (mode . ((type . "string") (enum . ("append" "prepend" "replace")) (default . "append")))
      (force . ((type . "boolean") (default . :json-false)))))
    ("delete_node" orsb-tool-delete-node
     "Delete a heading subtree, or a whole note (archive=true moves the file to the archive directory instead)."
     ("id")
     (,orsb-tools--id-schema
      (archive . ((type . "boolean") (default . :json-false)))))
    ("link_nodes" orsb-tool-link-nodes
     "Add (default) or remove an [[id:...]] link from node id to target_id; section says which heading the bullet goes under."
     ("id" "target_id")
     (,orsb-tools--id-schema
      (target_id . ((type . "string")))
      (action . ((type . "string") (enum . ("add" "remove")) (default . "add")))
      (section . ((type . "string")))))
    ("add_daily_entry" orsb-tool-add-daily-entry
     "Add a structured entry (title, bullet points, optional next steps and tags) to today's daily note. type=todo makes it a TODO heading."
     ("title" "points")
     ((title . ((type . "string")))
      (points . ((type . "array") (items . ((type . "string")))))
      (next_steps . ((type . "array") (items . ((type . "string")))))
      (tags . ((type . "array") (items . ((type . "string")))))
      (timestamp . ((type . "string") (description . "HH:MM, default now")))
      (type . ((type . "string") (enum . ("journal" "todo")) (default . "journal")))))
    ("get_daily" orsb-tool-get-daily
     "The raw text of a daily note (default today)."
     ()
     ((date . ((type . "string") (description . "YYYY-MM-DD")))))
    ("log_to_inbox" orsb-tool-log-to-inbox
     "Append one line to today's inbox for the human to file, optionally linked to a node. [[Name]] links create a person note when none exists."
     ("text")
     ((text . ((type . "string")))
      (linked_id . ((type . "string") (description . "Node the entry is about")))
      (category . ((type . "string") (description . "Free label prefixed in brackets, e.g. task, idea, followup")))))
    ("get_digest" orsb-tool-get-digest
     "Everything a daily review needs: active and stale projects, pending and dangling follow-ups, and the inbox of the last days."
     ()
     ((days . ((type . "integer") (default . 7)))))
    ("get_projects" orsb-tool-get-projects
     "Project nodes with status, next action and staleness. Filter with status or stale=true; days_threshold overrides the stale cutoff."
     ()
     ((status . ((type . "string")))
      (stale . ((type . "boolean")))
      (days_threshold . ((type . "integer")))))
    ("get_followups" orsb-tool-get-followups
     "People with unchecked follow-ups (default) or, with dangling=true, [[Name]] links that have no person note."
     ()
     ((dangling . ((type . "boolean") (default . :json-false)))))
    ("get_blog_status" orsb-tool-get-blog-status
     "Blog overview: drafts with how many sections have text, recent published posts, ideas that could become posts."
     () ())
    ("sync" orsb-tool-sync
     "Bring the database (and optionally embeddings) up to date: id for one note; otherwise a scan queued for idle time (wait=true blocks; full=true forces a rebuild)."
     ()
     ((id . ((type . "string")))
      (embeddings . ((type . "boolean") (default . :json-false)))
      (full . ((type . "boolean") (default . :json-false)))
      (wait . ((type . "boolean") (default . :json-false)))))
    ("get_schema" orsb-tool-get-schema
     "The vocabulary this server enforces: node types and directories, status values and aliases, stale threshold, conventional property keys, Hugo sections, id forms. Call this instead of guessing."
     () ()))
  "The 2.0 contract: (NAME FUNCTION DESCRIPTION REQUIRED SCHEMA).")

(defun orsb-tools-function (name)
  "The implementation function of 2.0 tool NAME."
  (or (nth 1 (assoc name orsb-tools--definitions))
      (error "No such tool: %s" name)))

;;;; Legacy names (pre-2.0): argument mappers, old response shape

(defun orsb-legacy--data (name args)
  "Run 2.0 tool NAME on ARGS; return its data or signal the error."
  (let ((r (orsb-tools--run (orsb-tools-function name) args)))
    (if (eq (car r) :ok) (nth 1 r)
      (orsb-error (nth 1 r) "%s" (nth 2 r)))))

(defun orsb-legacy--ok (&rest fields)
  "A legacy success alist with FIELDS (an alist) merged in."
  (append '((success . t)) (car fields)))

(defun orsb-legacy--wrap (fn)
  "Wrap legacy mapper FN so it answers in the {\"success\":...} shape."
  (lambda (args)
    (condition-case err
        (json-encode (funcall fn (orsb-tools--clean-args args)))
      (orsb-error (json-encode `((success . :json-false) (error . ,(nth 2 err)))))
      (error (json-encode `((success . :json-false) (error . ,(error-message-string err))))))))

(defun orsb-legacy--id (args &rest keys)
  "The first of KEYS present in ARGS (the old identifier spellings)."
  (seq-some (lambda (k) (orsb-arg args k)) keys))

(defun orsb-legacy--search (mode)
  "Mapper for the old search tools in MODE."
  (lambda (args)
    (let ((d (orsb-legacy--data "search" (append `((mode . ,mode)) args))))
      (orsb-legacy--ok
       `((query . ,(alist-get 'query d)) (total_found . ,(alist-get 'total d))
         (notes . ,(orsb--vector
                    (mapcar (lambda (h) `((id . ,(alist-get 'id h)) (title . ,(alist-get 'title h))
                                          (file . ,(alist-get 'file h)) (node_type . ,(alist-get 'node_type h))
                                          (status . ,(alist-get 'status h))
                                          (similarity_score . ,(alist-get 'score h))
                                          (snippet . ,(alist-get 'snippet h))))
                            (alist-get 'hits d)))))))))

(defun orsb-legacy--create (type)
  "Mapper for the old create_* tools of TYPE."
  (lambda (args)
    (let* ((title (or (orsb-arg args 'title) (orsb-arg args 'name)))
           (body (or (orsb-arg args 'notes) (orsb-arg args 'body) (orsb-arg args 'elaboration)))
           (d (orsb-legacy--data "create_node"
                                 `((node_type . ,type) (title . ,title) (body . ,body)
                                   (status . ,(orsb-arg args 'status))
                                   (next_action . ,(orsb-arg args 'next_action))
                                   (context . ,(orsb-arg args 'context))
                                   (follow_ups . ,(alist-get 'follow_ups args))
                                   (one_liner . ,(orsb-arg args 'one_liner))
                                   (due_date . ,(orsb-arg args 'due_date))
                                   (hugo_section . ,(orsb-arg args 'section))
                                   (tags . ,(alist-get 'tags args))))))
      (orsb-legacy--ok
       `((message . ,(format "%s note created" type))
         (note_id . ,(alist-get 'id d)) (id . ,(alist-get 'id d))
         (file . ,(alist-get 'file d)) (title . ,(alist-get 'title d))
         (node . ,(orsb--object d)))))))

(defconst orsb-tools--legacy
  `(("search_notes" "search" ,(orsb-legacy--search "title"))
    ("contextual_search" "search" ,(orsb-legacy--search "contextual"))
    ("semantic_search" "search" ,(orsb-legacy--search "semantic"))
    ("read_note" "get_node"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "get_node" `((id . ,(orsb-legacy--id args 'identifier 'id)) (section . ,(orsb-arg args 'section))))))
          (orsb-legacy--ok `((file . ,(alist-get 'file d)) (id . ,(alist-get 'id d)) (title . ,(alist-get 'title d))
                             (properties . ,(alist-get 'properties d)) (content . ,(alist-get 'body d)))))))
    ("read_node" "get_node"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "get_node" `((id . ,(orsb-legacy--id args 'node_id 'id))))))
          (orsb-legacy--ok `((node_id . ,(alist-get 'id d)) (title . ,(alist-get 'title d)) (file . ,(alist-get 'file d))
                             (level . ,(alist-get 'level d)) (text . ,(alist-get 'body d))
                             (properties . ,(alist-get 'properties d)) (keywords . ,(alist-get 'keywords d)))))))
    ("get_note_properties" "get_node"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "get_node" `((id . ,(orsb-legacy--id args 'identifier 'id)) (include_body . :json-false)))))
          (orsb-legacy--ok `((file . ,(alist-get 'file d)) (id . ,(alist-get 'id d)) (title . ,(alist-get 'title d))
                             (status . ,(alist-get 'status d)) (node_type . ,(alist-get 'node_type d))
                             (properties . ,(alist-get 'properties d)) (tags . ,(alist-get 'tags d))
                             (links_to . ,(orsb--vector (mapcar (lambda (l) (alist-get 'id l)) (alist-get 'links_to d))))
                             (links_from . ,(orsb--vector (mapcar (lambda (l) (alist-get 'id l)) (alist-get 'links_from d)))))))))
    ("list_notes" "list_nodes"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "list_nodes" (if (equal (orsb-arg args 'status) "stale")
                                                     (append '((stale . t)) (assq-delete-all 'status (copy-alist args)))
                                                   args))))
          (orsb-legacy--ok `((total . ,(alist-get 'total d)) (notes . ,(alist-get 'nodes d)))))))
    ("get_active_projects" "get_projects"
     ,(lambda (_args)
        (let ((d (orsb-legacy--data "get_projects" '((status . "active")))))
          (orsb-legacy--ok `((total . ,(alist-get 'total d)) (projects . ,(alist-get 'projects d)))))))
    ("get_stale_projects" "get_projects"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "get_projects" `((stale . t) (days_threshold . ,(alist-get 'days_threshold args))))))
          (orsb-legacy--ok `((total . ,(alist-get 'total d)) (projects . ,(alist-get 'projects d)))))))
    ("create_note" "create_node" ,(orsb-legacy--create "note"))
    ("create_project" "create_node" ,(orsb-legacy--create "project"))
    ("create_person" "create_node" ,(orsb-legacy--create "person"))
    ("create_idea" "create_node" ,(orsb-legacy--create "idea"))
    ("create_admin" "create_node" ,(orsb-legacy--create "admin"))
    ("create_blog_post" "create_node" ,(orsb-legacy--create "blog"))
    ("add_node" "add_heading"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "add_heading" `((id . ,(orsb-legacy--id args 'note_id 'id)) (heading . ,(orsb-arg args 'heading))
                                                    (body . ,(orsb-arg args 'text)) (properties . ,(alist-get 'properties args))
                                                    (level . ,(alist-get 'level args))))))
          (orsb-legacy--ok `((node_id . ,(alist-get 'id d)) (file . ,(alist-get 'file d)) (heading . ,(alist-get 'title d)) (level . ,(alist-get 'level d)))))))
    ("update_node" "set_node / update_body"
     ,(lambda (args)
        (let ((id (orsb-legacy--id args 'node_id 'id))
              (text (or (orsb-arg args 'text) (orsb-arg args 'content)))
              (section (orsb-arg args 'section))
              (props (alist-get 'properties args)))
          (when props (orsb-legacy--data "set_node" `((id . ,id) (properties . ,props))))
          (when text (orsb-legacy--data "update_body" `((id . ,id) (content . ,text) (section . ,section) (mode . "replace"))))
          (let ((d (orsb-legacy--data "get_node" `((id . ,id) (include_body . :json-false)))))
            (orsb-legacy--ok `((node_id . ,(alist-get 'id d)) (file . ,(alist-get 'file d)) (properties . ,(alist-get 'properties d))))))))
    ("update_note" "update_body"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "update_body" `((id . ,(orsb-legacy--id args 'identifier 'id)) (content . ,(orsb-arg args 'content))
                                                    (section . ,(orsb-arg args 'section)) (mode . ,(or (orsb-arg args 'mode) "append"))
                                                    (force . ,(alist-get 'force args))))))
          (orsb-legacy--ok `((file . ,(alist-get 'file d)) (mode . ,(alist-get 'mode d)) (section . ,(alist-get 'section d)))))))
    ("manage_tags" "set_node"
     ,(lambda (args)
        (let* ((action (orsb-arg args 'action))
               (key (if (equal action "remove") 'tags_remove 'tags_add))
               (d (orsb-legacy--data "set_node" `((id . ,(orsb-legacy--id args 'identifier 'id)) (,key . ,(vector (orsb-arg args 'tag)))))))
          (orsb-legacy--ok `((file . ,(alist-get 'file d)) (action . ,action) (tag . ,(orsb-arg args 'tag)) (tags . ,(alist-get 'tags d)))))))
    ("rename_note" "set_node"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "set_node" `((id . ,(orsb-legacy--id args 'identifier 'id)) (title . ,(orsb-arg args 'new_title))))))
          (orsb-legacy--ok `((file . ,(alist-get 'file d)) (new_title . ,(alist-get 'title d)))))))
    ("change_task_state" "set_node"
     ,(lambda (args)
        ;; old contract: file + heading text; find the heading node in that file
        (let* ((file (orsb-arg args 'file))
               (heading (orsb-arg args 'heading))
               (node (or (seq-find (lambda (n) (and (equal (org-roam-node-file n) (expand-file-name file org-roam-directory))
                                                    (> (org-roam-node-level n) 0)
                                                    (orsb-core-string= (org-roam-node-title n) heading)))
                                   (org-roam-node-list))
                         (orsb-error 'not-found "No heading %S in %s" heading file)))
               (d (orsb-legacy--data "set_node" `((id . ,(org-roam-node-id node)) (todo . ,(orsb-arg args 'new_state))))))
          (orsb-legacy--ok `((message . ,(format "Changed %S to %s" heading (alist-get 'todo d))))))))
    ("delete_note" "delete_node"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "delete_node" `((id . ,(orsb-legacy--id args 'identifier 'id)) (archive . ,(alist-get 'archive args))))))
          (orsb-legacy--ok `((action . ,(alist-get 'action d)) (file . ,(alist-get 'file d)))))))
    ("delete_node" "delete_node"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "delete_node" `((id . ,(orsb-legacy--id args 'node_id 'id))))))
          (orsb-legacy--ok `((node_id . ,(alist-get 'id d)) (file . ,(alist-get 'file d)))))))
    ("add_link" "link_nodes"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "link_nodes" `((id . ,(orsb-arg args 'from_id)) (target_id . ,(orsb-arg args 'to_id)) (section . ,(orsb-arg args 'section))))))
          (orsb-legacy--ok `((from . ,(alist-get 'id d)) (to . ,(alist-get 'target_id d)) (to_title . ,(alist-get 'target_title d)))))))
    ("add_daily_entry" "add_daily_entry"
     ,(lambda (args)
        (orsb-legacy--data "add_daily_entry" args)
        (orsb-legacy--ok `((message . ,(format "Added journal entry: %s" (orsb-arg args 'title)))))))
    ("get_daily_content" "get_daily"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "get_daily" args)))
          (orsb-legacy--ok `((content . ,(alist-get 'content d)))))))
    ("log_to_inbox" "log_to_inbox"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "log_to_inbox" `((text . ,(orsb-arg args 'text))))))
          (orsb-legacy--ok `((logged . ,(orsb-arg args 'text)) (created_people . ,(alist-get 'created_people d)))))))
    ("add_inbox_entry" "log_to_inbox"
     ,(lambda (args)
        (orsb-legacy--data "log_to_inbox" `((text . ,(orsb-arg args 'original_text)) (category . ,(orsb-arg args 'command))
                                            (linked_id . ,(orsb-arg args 'linked_note_id))))
        (orsb-legacy--ok `((message . "Inbox entry added") (command . ,(orsb-arg args 'command))))))
    ("get_digest_data" "get_digest"
     ,(lambda (_args)
        (let ((d (orsb-legacy--data "get_digest" nil)))
          (orsb-legacy--ok `((generated_at . ,(alist-get 'generated_at d))
                             (active_projects . ,(alist-get 'active_projects d))
                             (pending_followups . ,(alist-get 'pending_followups d))
                             (stale_projects . ,(alist-get 'stale_projects d)))))))
    ("get_weekly_inbox" "get_digest"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "get_digest" `((days . ,(alist-get 'days args))))))
          (orsb-legacy--ok `((by_day . ,(alist-get 'inbox d)))))))
    ("get_pending_followups" "get_followups"
     ,(lambda (_args) (orsb-legacy--ok (orsb-legacy--data "get_followups" nil))))
    ("get_dangling_followups" "get_followups"
     ,(lambda (_args) (orsb-legacy--ok (orsb-legacy--data "get_followups" '((dangling . t))))))
    ("blog_status" "get_blog_status"
     ,(lambda (_args) (orsb-legacy--ok (orsb-legacy--data "get_blog_status" nil))))
    ("sync_database" "sync"
     ,(lambda (_args) (orsb-legacy--data "sync" '((wait . t))) (orsb-legacy--ok '((message . "Database synced")))))
    ("generate_note_embedding" "sync"
     ,(lambda (args)
        (let ((node (orsb-core-resolve (or (orsb-arg args 'file_path) (orsb-arg args 'id)))))
          (orsb-legacy--data "sync" `((id . ,(org-roam-node-id node)) (embeddings . t)))
          (orsb-legacy--ok `((message . ,(format "Embedding generated for %s" (org-roam-node-file node))))))))
    ("generate_embeddings" "sync"
     ,(lambda (_args) (orsb-legacy--data "sync" '((embeddings . t))) (orsb-legacy--ok '((message . "Batch embedding generation queued"))))))
  "Legacy tools: (OLD-NAME REPLACEMENT MAPPER).  MAPPER takes the decoded
args and returns the legacy response alist, calling the 2.0 tools.")

(defun orsb-tools-legacy-name-p (name)
  "Whether NAME is registered as a legacy (pre-2.0) tool, i.e. answers in the
old {\"success\": ...} shape."
  (and (assoc name orsb-tools--legacy)
       (not (assoc name orsb-tools--definitions))
       t))

(defun orsb-tools-register ()
  "Register the 2.0 tools, and the legacy names when `orsb-mcp-legacy-tools'."
  (clrhash org-roam-mcp-http--tools)
  (dolist (def orsb-tools--definitions)
    (pcase-let ((`(,name ,fn ,desc ,required ,schema) def))
      (org-roam-mcp-http--register-tool
       name desc
       (lambda (args) (orsb-tools--envelope fn args))
       (mapcar (lambda (p) (cons (car p) 'string)) schema)
       required
       schema)))
  (when orsb-mcp-legacy-tools
    (dolist (entry orsb-tools--legacy)
      (pcase-let ((`(,old ,new ,mapper) entry))
        ;; A name the 2.0 contract also uses (add_daily_entry, log_to_inbox,
        ;; delete_node) keeps its 2.0 implementation, which accepts the old
        ;; arguments as well.
        (unless (assoc old orsb-tools--definitions)
          (org-roam-mcp-http--register-tool
           old (format "[deprecated → %s] Old name kept for compatibility; see MIGRATION.md." new)
           (orsb-legacy--wrap mapper)
           nil nil nil)))))
  (message "orsb-tools: %d tools registered (%s legacy names)"
           (hash-table-count org-roam-mcp-http--tools)
           (if orsb-mcp-legacy-tools "including" "without")))

;;;; Status migration

(defun orsb-migrate-status (&optional apply)
  "Report (and with APPLY, rewrite) STATUS values that are not canonical.
Interactively, shows the report; with a prefix argument, applies it.
Also moves a #+STATUS: keyword into the drawer.  Unrecognized free-text
values are listed and left alone; blog nodes keep their own lifecycle."
  (interactive "P")
  (let ((plan nil) (unknown nil) (blog 0))
    (dolist (node (seq-filter (lambda (n) (= (org-roam-node-level n) 0)) (org-roam-node-list)))
      (let* ((props (org-roam-node-properties node))
             (drawer (cdr (assoc "STATUS" props)))
             (kw (cdr (assoc "STATUS" (orsb-core-node-keywords node))))
             (raw (or drawer kw)))
        (when raw
          (if (orsb-core-blog-node-p node)
              (setq blog (1+ blog))
            (let ((canon (orsb-tools--canonical-status raw)))
              (cond
               ((null canon) (push (cons (org-roam-node-title node) raw) unknown))
               ((or kw (not (equal raw canon)))
                (push (list node raw canon (and kw t)) plan))))))))
    (when apply
      (dolist (p plan)
        (pcase-let ((`(,node ,_raw ,canon ,from-kw) p))
          (orsb-core-set-properties node `(("STATUS" . ,canon)))
          (when from-kw (orsb-core-set-keywords node '(("STATUS" . nil)))))))
    (let ((report
           (concat (format "%s %d status value(s):\n" (if apply "Rewrote" "Would rewrite") (length plan))
                   (mapconcat (lambda (p) (format "  %s: %S -> %S%s" (org-roam-node-title (car p)) (nth 1 p) (nth 2 p)
                                                  (if (nth 3 p) " (from #+STATUS:)" "")))
                              (reverse plan) "\n")
                   (format "\n%d unrecognized value(s) left alone:\n" (length unknown))
                   (mapconcat (lambda (u) (format "  %s: %S" (car u) (cdr u))) (reverse unknown) "\n")
                   (format "\n%d blog note(s) keep their own lifecycle (%s)." blog (string-join orsb-blog-status-values "/")))))
      (if (called-interactively-p 'any)
          (with-current-buffer (get-buffer-create "*orsb status migration*")
            (erase-buffer) (insert report) (display-buffer (current-buffer)))
        report))))

(provide 'orsb-tools)
;;; orsb-tools.el ends here
