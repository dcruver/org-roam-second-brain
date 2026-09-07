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
;; and one status vocabulary (`orsb-status-values').  Property edits go
;; through `orsb-core'; a few capabilities still delegate to the legacy
;; `my/api-*' implementations until those move into the core (2.0 plan,
;; phase 5).  The 35 legacy tool names stay registered, marked deprecated,
;; while `orsb-mcp-legacy-tools' is non-nil.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-roam)
(require 'orsb-core)

(declare-function org-roam-mcp-http--register-tool "org-roam-mcp-http")
(declare-function org-roam-mcp-tool-fn "org-roam-mcp-http")
(declare-function org-roam-mcp-tool-description "org-roam-mcp-http")
(declare-function org-roam-mcp-http--set-tool-description "org-roam-mcp-http")
(declare-function org-roam-semantic-get-similar-data "org-roam-vector-search")
(declare-function org-roam-semantic-generate-all-embeddings "org-roam-vector-search")
(declare-function my/api--generate-and-save-embedding "org-roam-api")
(defvar org-roam-mcp-http--tools)

;;;; Customization

(defcustom orsb-node-types
  '(("project" . "projects") ("person" . "people") ("idea" . "ideas")
    ("admin" . "admin") ("blog" . "blog") ("reference" . "reference")
    ("howto" . "howto") ("note" . ""))
  "Node types and the vault subdirectory each lives in (\"\" = vault root)."
  :type '(alist :key-type string :value-type string)
  :group 'orsb)

(defcustom orsb-status-values '("active" "waiting" "blocked" "someday" "done" "cancelled")
  "Allowed values of the STATUS property.  `stale' is computed, never stored."
  :type '(repeat string)
  :group 'orsb)

(defcustom orsb-blog-status-values '("idea" "draft" "published")
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

(defcustom orsb-hugo-sections nil
  "Hugo sections offered for blog nodes; falls back to `sb/hugo-sections'."
  :type '(repeat string)
  :group 'orsb)

(defcustom orsb-known-property-keys
  '("NODE-TYPE" "STATUS" "NEXT-ACTION" "CONTEXT" "LAST-CONTACT" "DUE" "PRIORITY" "CREATED")
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
    (and v (not (eq v :json-false)) (not (eq v :json-null)))))

(defun orsb-arg-list (args key)
  "Return KEY in ARGS as a list of strings (accepts array, CSV string, or nil)."
  (let ((v (alist-get key args)))
    (cond ((or (null v) (eq v :json-null)) nil)
          ((vectorp v) (mapcar (lambda (x) (format "%s" x)) v))
          ((listp v) (mapcar (lambda (x) (format "%s" x)) v))
          ((stringp v) (split-string v "[,;]" t "[ \t]+"))
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

;;;; Legacy bridge (until the my/api-* logic moves into orsb-core)

(defvar orsb-tools--legacy-fns (make-hash-table :test 'equal)
  "Legacy tool name -> function, captured before deprecation/removal.")

(defun orsb-tools--legacy (name args)
  "Call legacy tool NAME with ARGS and return its decoded result.
A legacy {\"success\":false,...} reply is re-signalled as `orsb-error'."
  (let ((fn (gethash name orsb-tools--legacy-fns)))
    (unless fn (orsb-error 'unavailable "Legacy implementation %s is not loaded" name))
    (let* ((raw (funcall fn args))
           (json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol)
           (parsed (condition-case nil (json-read-from-string raw) (error nil))))
      (cond
       ((null parsed) (orsb-error 'internal "%s returned no data" name))
       ((and (assq 'success parsed) (memq (alist-get 'success parsed) '(nil :json-false)))
        (let ((msg (or (alist-get 'error parsed) "failed")))
          (orsb-error (if (string-match-p "not found" (format "%s" msg)) 'not-found 'internal)
                      "%s" msg)))
       (t parsed)))))

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
       (links_to . ,(orsb--vector (car links)))
       (links_from . ,(orsb--vector (cdr links))))
     (when include-body `((body . ,(alist-get 'body rec)))))))

(defun orsb-tools--snippet (node &optional length)
  "First LENGTH characters of NODE's body, whitespace collapsed."
  (let* ((body (condition-case nil (orsb-core-node-body node) (error "")))
         (flat (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " body)))
         (n (or length 240)))
    (if (> (length flat) n) (concat (substring flat 0 n) "…") flat)))

;;;; Status vocabulary

(defun orsb-tools--canonical-status (value)
  "Return the canonical status for VALUE, or nil when it is not recognized."
  (let ((v (downcase (string-trim (or value "")))))
    (cond ((member v orsb-status-values) v)
          ((assoc v orsb-status-aliases) (cdr (assoc v orsb-status-aliases)))
          (t nil))))

(defun orsb-tools--blog-node-p (node-or-type)
  "Whether NODE-OR-TYPE (a node or a NODE-TYPE string) is a blog node."
  (equal "blog" (if (stringp node-or-type) node-or-type
                  (cdr (assoc "NODE-TYPE" (org-roam-node-properties node-or-type))))))

(defun orsb-tools--status-values-for (node-or-type)
  "The status vocabulary that applies to NODE-OR-TYPE."
  (if (orsb-tools--blog-node-p node-or-type) orsb-blog-status-values orsb-status-values))

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
       (unless (fboundp 'org-roam-semantic-get-similar-data)
         (orsb-error 'unavailable "Semantic search is not loaded (org-roam-vector-search)"))
       (let ((seen (make-hash-table :test 'equal)))
         (dolist (r (org-roam-semantic-get-similar-data query (* 3 limit) cutoff))
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

(defun orsb-tools--new-node-id (result)
  "Extract the created node's id from a legacy create result."
  (or (alist-get 'id (alist-get 'note result))
      (alist-get 'note_id result)
      (alist-get 'node_id result)
      (alist-get 'id result)
      (when-let ((file (or (alist-get 'file result) (alist-get 'file (alist-get 'note result)))))
        (org-roam-db-update-file file)
        (when-let ((n (orsb-core--first-node-in-file file))) (org-roam-node-id n)))))

(defun orsb-tool-create-node (args)
  "create_node, typed."
  (orsb-require args 'node_type 'title)
  (let* ((type (orsb-arg args 'node_type))
         (title (orsb-arg args 'title))
         (body (orsb-arg args 'body))
         (status (when-let ((s (orsb-arg args 'status))) (orsb-tools--check-status s type)))
         (result
          (pcase type
            ("project" (orsb-tools--legacy "create_project"
                                           `((title . ,title) (notes . ,(or body ""))
                                             (status . ,(or status "active"))
                                             (next_action . ,(orsb-arg args 'next_action)))))
            ("person" (orsb-tools--legacy "create_person"
                                          `((name . ,title) (context . ,(orsb-arg args 'context))
                                            (follow_ups . ,(let ((f (orsb-arg-list args 'follow_ups))) (and f (string-join f "\n"))))
                                            (notes . ,body))))
            ("idea" (orsb-tools--legacy "create_idea"
                                        `((title . ,title) (one_liner . ,(or (orsb-arg args 'one_liner) (and body (car (split-string body "\n" t))) title))
                                          (elaboration . ,body))))
            ("admin" (orsb-tools--legacy "create_admin"
                                         `((title . ,title) (due_date . ,(orsb-arg args 'due_date)) (notes . ,body))))
            ("blog" (orsb-tools--legacy "create_blog_post"
                                        `((title . ,title) (section . ,(or (orsb-arg args 'hugo_section) "homelab"))
                                          (body . ,(or body "")) (tags . ,(string-join (orsb-arg-list args 'tags) ",")))))
            ("note" (orsb-tools--legacy "create_note" `((title . ,title))))
            (_ (orsb-error 'invalid-argument "node_type must be one of %s"
                           (string-join (mapcar #'car orsb-node-types) ", ")))))
         (id (orsb-tools--new-node-id result)))
    (unless id (orsb-error 'internal "Note was created but its id could not be determined"))
    (let ((node (orsb-core-resolve id)))
      (when (and (equal type "note") body) (orsb-core-set-body node body))
      (when-let ((props (orsb-arg-props args 'properties))) (orsb-core-set-properties node props))
      (when (and status (not (equal type "project"))) (orsb-core-set-properties node `(("STATUS" . ,status))))
      (when (and (equal type "note") (not (cdr (assoc "NODE-TYPE" (orsb-core-node-properties node)))))
        (orsb-core-set-properties node '(("NODE-TYPE" . "note"))))
      (when-let ((tags (orsb-arg-list args 'tags)))
        (unless (equal type "blog") (orsb-core-set-tags node tags)))
      (orsb-tools--full-record (orsb-core-resolve id) nil))))

(defun orsb-tool-add-heading (args)
  "add_heading."
  (orsb-require args 'id 'heading)
  (let* ((parent (orsb-core-resolve (orsb-arg args 'id)))
         (todo (orsb-arg args 'todo))
         (heading (if todo (concat (upcase todo) " " (orsb-arg args 'heading)) (orsb-arg args 'heading)))
         (result (orsb-tools--legacy "add_node"
                                     `((note_id . ,(org-roam-node-id parent))
                                       (heading . ,heading)
                                       (text . ,(or (orsb-arg args 'body) ""))
                                       (properties . ,(orsb-arg-props args 'properties))
                                       (level . ,(orsb-arg-int args 'level 1)))))
         (id (orsb-tools--new-node-id result)))
    (unless id (orsb-error 'internal "Heading was added but its id could not be determined"))
    (orsb-tools--full-record (orsb-core-resolve id) nil)))

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
     ((or section (= (org-roam-node-level node) 0))
      ;; Legacy update_note carries the destructive-replace guard and the .bak.
      (orsb-tools--legacy "update_note"
                          `((identifier . ,(org-roam-node-id node)) (content . ,content)
                            (section . ,section) (mode . ,mode) (force . ,(if force t :json-false)))))
     ((equal mode "replace") (orsb-core-set-body node content))
     (t (orsb-core-append-body node content (equal mode "prepend"))))
    `((id . ,(org-roam-node-id node)) (file . ,(orsb-tools--relative (org-roam-node-file node)))
      (mode . ,mode) (section . ,section))))

(defun orsb-tool-delete-node (args)
  "delete_node: a file (optionally archived) or a heading subtree."
  (orsb-require args 'id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (archive (orsb-arg-bool args 'archive))
         (result (if (= (org-roam-node-level node) 0)
                     ;; nil, not :json-false: the legacy function treats any non-nil as "archive"
                     (orsb-tools--legacy "delete_note" `((identifier . ,(org-roam-node-file node)) (archive . ,(and archive t))))
                   (orsb-tools--legacy "delete_node" `((node_id . ,(org-roam-node-id node)))))))
    `((id . ,(org-roam-node-id node)) (level . ,(org-roam-node-level node))
      (action . ,(or (alist-get 'action result) "deleted"))
      (file . ,(if (and archive (= (org-roam-node-level node) 0))
                   (concat "archive/" (file-name-nondirectory (org-roam-node-file node)))
                 (orsb-tools--relative (org-roam-node-file node)))))))

(defun orsb-tool-link-nodes (args)
  "link_nodes: add or remove an [[id:...]] link from id to target_id."
  (orsb-require args 'id 'target_id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (target (orsb-core-resolve (orsb-arg args 'target_id)))
         (action (or (orsb-arg args 'action) "add")))
    (pcase action
      ("add" (orsb-tools--legacy "add_link" `((from_id . ,(org-roam-node-id node)) (to_id . ,(org-roam-node-id target))
                                              (section . ,(orsb-arg args 'section))))
             `((action . "added") (id . ,(org-roam-node-id node)) (target_id . ,(org-roam-node-id target))
               (target_title . ,(org-roam-node-title target))))
      ("remove" `((action . "removed") (id . ,(org-roam-node-id node)) (target_id . ,(org-roam-node-id target))
                  (removed . ,(orsb-core-unlink node (org-roam-node-id target)))))
      (_ (orsb-error 'invalid-argument "action must be add or remove")))))

(defun orsb-tool-add-daily-entry (args)
  "add_daily_entry (unchanged contract)."
  (orsb-require args 'title 'points)
  (let ((r (orsb-tools--legacy "add_daily_entry" args)))
    `((file . ,(orsb-tools--relative (alist-get 'file r))) (title . ,(orsb-arg args 'title)))))

(defun orsb-tool-get-daily (args)
  "get_daily {date}."
  (let ((r (orsb-tools--legacy "get_daily_content" `((date . ,(orsb-arg args 'date))))))
    `((date . ,(or (orsb-arg args 'date) (format-time-string "%Y-%m-%d")))
      (content . ,(or (alist-get 'content r) "")))))

(defun orsb-tool-log-to-inbox (args)
  "log_to_inbox {text, linked_id, category}."
  (orsb-require args 'text)
  (let* ((linked (orsb-arg args 'linked_id))
         (r (if linked
                (let ((n (orsb-core-resolve linked)))
                  (orsb-tools--legacy "add_inbox_entry"
                                      `((command . ,(or (orsb-arg args 'category) "note"))
                                        (original_text . ,(orsb-arg args 'text))
                                        (linked_note_id . ,(org-roam-node-id n))
                                        (linked_note_title . ,(org-roam-node-title n)))))
              (orsb-tools--legacy "log_to_inbox" `((text . ,(orsb-arg args 'text)))))))
    `((logged . t) (linked_id . ,linked)
      (created_people . ,(orsb--vector (alist-get 'created_people r))))))

(defun orsb-tool-get-digest (args)
  "get_digest {days}: the daily digest plus the inbox for the last DAYS days."
  (let ((digest (orsb-tools--legacy "get_digest_data" nil))
        (inbox (orsb-tools--legacy "get_weekly_inbox" `((days . ,(orsb-arg-int args 'days 7))))))
    `((digest . ,digest) (inbox . ,inbox))))

(defun orsb-tool-get-followups (args)
  "get_followups {dangling}."
  (if (orsb-arg-bool args 'dangling)
      (orsb-tools--legacy "get_dangling_followups" nil)
    (orsb-tools--legacy "get_pending_followups" nil)))

(defun orsb-tool-get-blog-status (_args)
  "get_blog_status."
  (orsb-tools--legacy "blog_status" nil))

(defun orsb-tool-sync (args)
  "sync {id, embeddings, full}."
  (let ((id (orsb-arg args 'id))
        (embeddings (orsb-arg-bool args 'embeddings))
        (full (orsb-arg-bool args 'full)))
    (cond
     (id (let ((node (orsb-core-resolve id)))
           (org-roam-db-update-file (org-roam-node-file node))
           (when embeddings
             (unless (fboundp 'my/api--generate-and-save-embedding)
               (orsb-error 'unavailable "Embedding generation is not loaded"))
             (my/api--generate-and-save-embedding (org-roam-node-file node)))
           `((synced . ,(orsb-tools--relative (org-roam-node-file node))) (embeddings . ,(if embeddings t :json-false)))))
     (full (if embeddings
               (progn
                 (unless (fboundp 'org-roam-semantic-generate-all-embeddings)
                   (orsb-error 'unavailable "Embedding generation is not loaded"))
                 (run-with-idle-timer 1 nil #'org-roam-semantic-generate-all-embeddings)
                 '((queued . t) (what . "embeddings for every note; runs when Emacs is idle")))
             (org-roam-db-sync)
             '((synced . "db") (full . t))))
     (t (org-roam-db-sync) '((synced . "db"))))))

(defun orsb-tool-get-schema (_args)
  "get_schema: the vocabulary this server enforces."
  `((version . ,orsb-tools-version)
    (id_forms . ["org-roam id" "path relative to the vault, or absolute" "exact title or alias"])
    (node_types . ,(orsb--vector (mapcar (lambda (c) `((type . ,(car c)) (directory . ,(cdr c)))) orsb-node-types)))
    (status_values . ,(orsb--vector orsb-status-values))
    (blog_status_values . ,(orsb--vector orsb-blog-status-values))
    (status_aliases . ,(orsb--object orsb-status-aliases))
    (stale_days . ,orsb-stale-days)
    (property_keys . ,(orsb--vector orsb-known-property-keys))
    (hugo_sections . ,(orsb--vector (or orsb-hugo-sections (and (boundp 'sb/hugo-sections) (symbol-value 'sb/hugo-sections)))))
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

(defun orsb-tools--envelope (fn args)
  "Run FN on ARGS and return the 2.0 JSON envelope."
  (condition-case err
      (json-encode `((ok . t) (data . ,(orsb--object (funcall fn args)))))
    (orsb-error
     (let ((code (nth 1 err)) (msg (nth 2 err)))
       (json-encode `((ok . :json-false)
                      (error . ((code . ,(replace-regexp-in-string "-" "_" (symbol-name code)))
                                (message . ,msg)
                                (hint . ,(orsb-tools--hint code))))))))
    (error
     (json-encode `((ok . :json-false)
                    (error . ((code . "internal")
                              (message . ,(error-message-string err))
                              (hint . ,(orsb-tools--hint 'internal)))))))))

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
     "Create a typed note (project, person, idea, admin, blog, note) in its conventional directory with the conventional properties, and return it. Extra properties, tags and a status may be given."
     ("node_type" "title")
     ((node_type . ((type . "string") (enum . ("project" "person" "idea" "admin" "blog" "note"))))
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
     "Delete a heading subtree, or a whole note (archive=true moves the file to archive/ instead)."
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
     "Add a structured entry (title, bullet points, optional next steps and tags) to today's daily note."
     ("title" "points")
     ((title . ((type . "string")))
      (points . ((type . "array") (items . ((type . "string")))))
      (next_steps . ((type . "array") (items . ((type . "string")))))
      (tags . ((type . "array") (items . ((type . "string")))))
      (timestamp . ((type . "string") (description . "HH:MM, default now")))))
    ("get_daily" orsb-tool-get-daily
     "The raw text of a daily note (default today)."
     ()
     ((date . ((type . "string") (description . "YYYY-MM-DD")))))
    ("log_to_inbox" orsb-tool-log-to-inbox
     "Append one line to today's inbox for the human to file, optionally linked to a node."
     ("text")
     ((text . ((type . "string")))
      (linked_id . ((type . "string") (description . "Node the entry is about")))
      (category . ((type . "string") (description . "Free label, e.g. task, idea, followup")))))
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
     "Blog overview: drafts with outline progress, recent posts, ideas that could become posts."
     () ())
    ("sync" orsb-tool-sync
     "Bring the database (and optionally embeddings) up to date: id for one note, full=true for everything. full+embeddings is queued for idle time."
     ()
     ((id . ((type . "string")))
      (embeddings . ((type . "boolean") (default . :json-false)))
      (full . ((type . "boolean") (default . :json-false)))))
    ("get_schema" orsb-tool-get-schema
     "The vocabulary this server enforces: node types and directories, status values and aliases, stale threshold, conventional property keys, Hugo sections, id forms. Call this instead of guessing."
     () ()))
  "The 2.0 contract: (NAME FUNCTION DESCRIPTION REQUIRED SCHEMA).")

(defconst orsb-tools--legacy-map
  '(("search_notes" . "search") ("semantic_search" . "search") ("contextual_search" . "search")
    ("read_note" . "get_node") ("read_node" . "get_node") ("get_note_properties" . "get_node")
    ("list_notes" . "list_nodes") ("get_active_projects" . "get_projects") ("get_stale_projects" . "get_projects")
    ("create_note" . "create_node") ("create_project" . "create_node") ("create_person" . "create_node")
    ("create_idea" . "create_node") ("create_admin" . "create_node") ("create_blog_post" . "create_node")
    ("add_node" . "add_heading") ("update_node" . "set_node / update_body") ("manage_tags" . "set_node")
    ("rename_note" . "set_node") ("change_task_state" . "set_node") ("update_note" . "update_body")
    ("delete_note" . "delete_node") ("delete_node" . "delete_node") ("add_link" . "link_nodes")
    ("get_daily_content" . "get_daily") ("add_inbox_entry" . "log_to_inbox")
    ("get_digest_data" . "get_digest") ("get_weekly_inbox" . "get_digest")
    ("get_pending_followups" . "get_followups") ("get_dangling_followups" . "get_followups")
    ("blog_status" . "get_blog_status") ("sync_database" . "sync")
    ("generate_embeddings" . "sync") ("generate_note_embedding" . "sync"))
  "Legacy tool name -> replacement, for the deprecation notice.")

(defun orsb-tools-register ()
  "Register the 2.0 tools and mark (or drop) the legacy ones.
Call after `org-roam-mcp-http--register-all-tools'."
  ;; Keep every legacy implementation reachable for the bridge: some names
  ;; (add_daily_entry, log_to_inbox, delete_node) are reused by the 2.0
  ;; contract and get overwritten below, others are removed when
  ;; `orsb-mcp-legacy-tools' is nil. First capture wins, so a second
  ;; registration pass never replaces a legacy function with a 2.0 one.
  (maphash (lambda (name tool)
             (unless (gethash name orsb-tools--legacy-fns)
               (puthash name (org-roam-mcp-tool-fn tool) orsb-tools--legacy-fns)))
           org-roam-mcp-http--tools)
  (dolist (def orsb-tools--definitions)
    (pcase-let ((`(,name ,fn ,desc ,required ,schema) def))
      (org-roam-mcp-http--register-tool
       name desc
       (lambda (args) (orsb-tools--envelope fn args))
       (mapcar (lambda (p) (cons (car p) 'string)) schema)
       required
       schema)))
  (dolist (pair orsb-tools--legacy-map)
    (when-let ((tool (gethash (car pair) org-roam-mcp-http--tools)))
      (if orsb-mcp-legacy-tools
          (unless (string-prefix-p "[deprecated" (org-roam-mcp-tool-description tool))
            (org-roam-mcp-http--set-tool-description
             tool (format "[deprecated → %s] %s" (cdr pair) (org-roam-mcp-tool-description tool))))
        (remhash (car pair) org-roam-mcp-http--tools))))
  (message "orsb-tools: %d tools registered (%s legacy names)"
           (hash-table-count org-roam-mcp-http--tools)
           (if orsb-mcp-legacy-tools "including" "without")))

;;;; Status migration

(defun orsb-migrate-status (&optional apply)
  "Report (and with APPLY, rewrite) STATUS values that are not canonical.
Interactively, shows the report; with a prefix argument, applies it.
Also moves a #+STATUS: keyword into the drawer.  Unrecognized free-text
values are listed and left alone."
  (interactive "P")
  (let ((plan nil) (unknown nil) (blog 0))
    (dolist (node (seq-filter (lambda (n) (= (org-roam-node-level n) 0)) (org-roam-node-list)))
      (let* ((props (org-roam-node-properties node))
             (drawer (cdr (assoc "STATUS" props)))
             (kw (cdr (assoc "STATUS" (orsb-core-node-keywords node))))
             (raw (or drawer kw)))
        (when raw
          (if (orsb-tools--blog-node-p node)
              ;; Blog nodes have their own lifecycle (idea/draft/published);
              ;; never fold it into the project vocabulary.
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
