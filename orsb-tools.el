;;; orsb-tools.el --- The 2.0 MCP tool contract -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 2.1.0
;; Package-Requires: ((emacs "29.1") (org-roam "2.2"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:

;; Nineteen MCP tools sized for small local models, with one identifier
;; parameter (`id': an org-roam id, a vault path, an exact title, or a day
;; for a daily note), one response envelope
;;
;;   {"ok":true,"data":{...}}
;;   {"ok":false,"error":{"code":"not_found|invalid_argument|refused|unavailable|internal",
;;                        "message":"...","hint":"..."}}   (+ MCP isError)
;;
;; and one status vocabulary (`orsb-status-values').  Every write is
;; recorded as a change with an id that `restore' undoes, and most writes
;; take dry_run to preview.  Everything is implemented on `orsb-core'.
;;
;; `orsb-mcp-tool-profile' picks the set: `small' (the nineteen) or `full'
;; (adds sync and get_blog_status).  The pre-2.0 names, and the 2.0 tools
;; folded into others in 2.1, are registered only while
;; `orsb-mcp-legacy-tools' is non-nil, marked deprecated; the pre-2.0 ones
;; answer in the old {"success": ...} shape.

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
(declare-function orsb-search-similar-vector "orsb-search")
(declare-function orsb-search-centroid "orsb-search")
(declare-function orsb-search-chunk-info "orsb-search")
(declare-function org-roam-ai-generate-embedding "org-roam-ai")
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

(defcustom orsb-mcp-legacy-tools nil
  "When non-nil the pre-2.0 tool names stay registered, marked deprecated.
So do the 2.0 tools that 2.1 folded into others (add_heading, get_daily,
get_projects, get_followups, get_digest).  Off by default: every extra
tool costs a small model context and a chance to pick the wrong one."
  :type 'boolean
  :group 'orsb)

(defcustom orsb-mcp-tool-profile 'small
  "Which tools the MCP server offers.
`small' is the nineteen-tool set meant for small local models; `full'
adds sync and get_blog_status."
  :type '(choice (const small) (const full))
  :group 'orsb)

(defcustom orsb-mcp-instructions
  "This server keeps an org-roam second brain (plain-text notes that link to each other).
- Find before you create: search, then get_node. create_node refuses a title that already exists and lists it.
- Every write returns change_id; restore with it undoes the change. Writes take dry_run:true to preview.
- Errors carry a hint saying what to do next; follow it instead of retrying the same call.
- get_review shows what needs attention; get_tasks lists open tasks; set_checkbox and set_node close them.
- Unsure of an allowed value? Call get_schema."
  "Text sent to MCP clients at initialize, as guidance for the model.\nnil sends none."
  :type '(choice (const :tag "None" nil) string)
  :group 'orsb)

(defcustom orsb-known-property-keys
  '("NODE-TYPE" "STATUS" "NEXT-ACTION" "CONTEXT" "LAST-CONTACT" "DUE-DATE" "ONE-LINER" "PRIORITY" "CREATED")
  "Property keys with a conventional meaning, reported by get_schema."
  :type '(repeat string)
  :group 'orsb)

(defconst orsb-tools-version "2.1.0")

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
  (or alist (make-hash-table)))

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
      (stale . ,(if (orsb-tools--stale-p status days) t :json-false))
      ,@(when-let ((next (cdr (assoc "NEXT-ACTION" props)))) `((next_action . ,next))))))

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

;;;; Shared helpers

(defun orsb-tools--change (args tool summary fn)
  "Run FN as one recorded change named TOOL with SUMMARY; return its data.
FN returns the data alist.  With dry_run in ARGS nothing is written and
the data gains would_change (a preview per file); otherwise it gains
change_id, the handle `restore' takes."
  (let* ((dry (orsb-arg-bool args 'dry_run))
         (r (orsb-core-call-with-change tool summary dry fn)))
    (append (car r)
            (if dry
                `((dry_run . t) (would_change . ,(orsb--vector (cdr r))))
              `((change_id . ,(cdr r)))))))

(defun orsb-tools--page (items args default-limit)
  "ITEMS cut by offset and limit from ARGS; return (PAGE . FIELDS).
FIELDS are total, returned and, when more remain, next_offset."
  (let* ((offset (max 0 (orsb-arg-int args 'offset 0)))
         (limit (max 1 (orsb-arg-int args 'limit default-limit)))
         (page (seq-take (seq-drop items offset) limit))
         (total (length items)))
    (cons page
          (append `((total . ,total) (returned . ,(length page)))
                  (when (< (+ offset (length page)) total)
                    `((next_offset . ,(+ offset (length page)))))))))

(defun orsb-tools--ref (node)
  "A short {id,title,file} for NODE."
  `((id . ,(org-roam-node-id node)) (title . ,(org-roam-node-title node))
    (file . ,(orsb-tools--relative (org-roam-node-file node)))))

(defun orsb-tools--heading-text (raw)
  "Heading line RAW without TODO keyword, priority cookie or tags."
  (and raw
       (let* ((s (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" raw))
              (s (replace-regexp-in-string "\\`\\(?:[A-Z]+[ \t]+\\)?\\(?:\\[#[A-Z0-9]\\][ \t]*\\)?" "" s)))
         (string-trim (if (string-empty-p (string-trim s)) raw s)))))

(defun orsb-tools--date-arg (args key)
  "KEY in ARGS as YYYY-MM-DD (\"today\", \"tomorrow\" and +N days accepted), or nil."
  (when-let ((v (orsb-arg args key)))
    (let ((v (downcase (string-trim v))))
      (cond ((equal v "today") (format-time-string "%Y-%m-%d"))
            ((equal v "tomorrow") (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 1))))
            ((string-match "\\`\\+?\\([0-9]+\\)d?\\'" v)
             (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time (string-to-number (match-string 1 v))))))
            ((string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" v) v)
            (t (orsb-error 'invalid-argument "%s must be YYYY-MM-DD, today, tomorrow or +N (got %S)" key v))))))

;;;; Tool implementations (each takes the decoded args alist, returns data)

(defun orsb-tools--text-section (text pos)
  "The heading above POS in TEXT (cleaned), or nil."
  (let ((start nil) (p 0))
    (while (and (string-match "^\\*+[ \t]+\\(.*\\)$" text p) (< (match-beginning 0) pos))
      (setq start (match-string 1 text) p (match-end 0)))
    (orsb-tools--heading-text start)))

(defun orsb-tools--context-snippet (text pos &optional n)
  "About N (default 240) characters of TEXT around POS.\nDrawers and keyword lines are removed."
  (let* ((n (or n 240))
         (from (max 0 (- pos (/ n 3))))
         (to (min (length text) (+ from n (* 2 n))))
         (raw (substring text from to))
         (clean (replace-regexp-in-string
                 "^[ \t]*\\(:[A-Za-z0-9_-]+:.*\\|#\\+.*\\)$" ""
                 (replace-regexp-in-string "^\\*+[ \t]+" "" raw)))
         (flat (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " clean))))
    (concat (if (> from 0) "…" "")
            (if (> (length flat) n) (concat (substring flat 0 n) "…") flat))))

(defun orsb-tool-search (args)
  "search: title | contextual | semantic."
  (orsb-require args 'query)
  (let* ((query (orsb-arg args 'query))
         (mode (or (orsb-arg args 'mode) "semantic"))
         (type (orsb-arg args 'node_type))
         (limit (min 25 (orsb-arg-int args 'limit 10)))
         (cutoff (let ((c (orsb-arg args 'cutoff))) (if (numberp c) c 0.5)))
         (hits nil))
    ;; each hit: (NODE SCORE EXTRA) where EXTRA is an alist (section, snippet...)
    (pcase mode
      ("title"
       (let ((seen (make-hash-table :test 'equal)))
         (dolist (row (append (org-roam-db-query [:select [id] :from nodes
                                                  :where (like title $s1) :order-by [(asc level) (asc title)]]
                                                 (concat "%" query "%"))
                              (org-roam-db-query [:select [node-id] :from aliases :where (like alias $s1)]
                                                 (concat "%" query "%"))))
           (unless (gethash (car row) seen)
             (puthash (car row) t seen)
             (when-let ((n (org-roam-node-from-id (car row))))
               (push (list n nil nil) hits))))))
      ("contextual"
       (let ((words (split-string (downcase query) "[^[:alnum:]_-]+" t)))
         (unless words (orsb-error 'invalid-argument "query has no words to look for"))
         (dolist (file (org-roam-list-files))
           (when-let* ((text (condition-case nil
                                 (with-temp-buffer (insert-file-contents file) (buffer-string))
                               (error nil)))
                       (low (downcase text))
                       (found (delq nil (mapcar (lambda (w) (string-search w low)) words)))
                       (n (orsb-core--first-node-in-file file)))
             (let* ((phrase (string-search (downcase query) low))
                    (pos (or phrase (apply #'min found))))
               (push (list n (/ (float (length found)) (length words))
                           `((section . ,(orsb-tools--text-section text pos))
                             (snippet . ,(orsb-tools--context-snippet text pos))))
                     hits))))))
      ("semantic"
       (unless (fboundp 'orsb-search-similar)
         (orsb-error 'unavailable "Semantic search is not loaded (orsb-search)"))
       (let ((seen (make-hash-table :test 'equal)))
         (dolist (r (condition-case err
                        (orsb-search-similar query (* 4 limit) cutoff)
                      (error (orsb-error-hint 'unavailable "Use mode \"contextual\" (word match) until the embedding service is back."
                                              "Semantic search failed: %s" (error-message-string err)))))
           (pcase-let ((`(,file ,score ,pos ,_heading) r))
             (unless (gethash file seen)
               (when-let ((n (orsb-core--first-node-in-file file)))
                 (puthash file t seen)
                 (let ((info (ignore-errors (orsb-search-chunk-info file pos 240 query))))
                   (push (list n score
                               `((section . ,(plist-get info :heading))
                                 (section_id . ,(plist-get info :id))
                                 (snippet . ,(plist-get info :snippet))))
                         hits))))))))
      (_ (orsb-error 'invalid-argument "mode must be title, contextual or semantic")))
    (setq hits (nreverse hits))
    (when type
      (setq hits (seq-filter (lambda (h) (equal (cdr (assoc "NODE-TYPE" (org-roam-node-properties (car h)))) type)) hits)))
    (if (member mode '("contextual" "semantic"))
        (setq hits (seq-sort-by #'cadr #'> hits))
      ;; title: an exact match first, then shorter (closer) titles
      (setq hits (seq-sort-by (lambda (h) (let ((title (downcase (or (org-roam-node-title (car h)) ""))))
                                            (if (equal title (downcase query)) -1 (length title))))
                              #'< hits)))
    (setq hits (seq-take hits limit))
    `((query . ,query) (mode . ,mode) (total . ,(length hits))
      (hits . ,(orsb--vector
                (mapcar (lambda (h)
                          (let ((extra (nth 2 h)))
                            (append (orsb-tools--light-record (car h))
                                    `((score . ,(nth 1 h)))
                                    (seq-filter #'cdr (assq-delete-all 'snippet (copy-alist extra)))
                                    `((snippet . ,(or (alist-get 'snippet extra) (orsb-tools--snippet (car h))))))))
                        hits))))))

(defun orsb-tool-get-node (args)
  "get_node."
  (orsb-require args 'id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (section (orsb-arg args 'section))
         (include-body (not (eq (alist-get 'include_body args) :json-false)))
         (max-chars (orsb-arg-int args 'max_chars 6000))
         (rec (orsb-tools--full-record node nil))
         (outline (orsb-core-outline node))
         (planning (orsb-core-node-planning node))
         (aliases (orsb-core-node-aliases node))
         (body (cond (section (orsb-core-section-body node section))
                     (include-body (orsb-core-node-body node)))))
    (append rec
            (when aliases `((aliases . ,(orsb--vector aliases))))
            (when planning `((scheduled . ,(plist-get planning :scheduled)) (deadline . ,(plist-get planning :deadline))))
            `((sections . ,(orsb--vector
                            (mapcar (lambda (h) `((title . ,(plist-get h :title)) (level . ,(plist-get h :level))
                                                  ,@(when (plist-get h :id) `((id . ,(plist-get h :id))))
                                                  ,@(when (plist-get h :todo) `((todo . ,(plist-get h :todo))))))
                                    outline))))
            (when section `((section . ,section)))
            (when body
              (if (> (length body) max-chars)
                  `((body . ,(substring body 0 max-chars))
                    (truncated . t) (body_chars . ,(length body))
                    (hint . "The body was cut; pass section (one of sections) or a larger max_chars."))
                `((body . ,body)))))))

(defun orsb-tools--created-key (record node)
  "Sort key for created order: CREATED property, the filename stamp, else modified."
  (or (when-let ((c (cdr (assoc "CREATED" (org-roam-node-properties node)))))
        (and (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" c)
             (concat (match-string 1 c) (match-string 2 c) (match-string 3 c))))
      (let ((f (file-name-base (org-roam-node-file node))))
        (and (string-match "\\([0-9]\\{8\\}\\)[0-9]\\{0,6\\}" f) (match-string 1 f)))
      (replace-regexp-in-string "[^0-9]" "" (substring (or (alist-get 'modified record) "00000000") 0 10))))

(defun orsb-tools--list-records (args)
  "Filtered and sorted light records of file nodes for list_nodes (all, unpaged)."
  (let* ((type (orsb-arg args 'node_type))
         (status (orsb-arg args 'status))
         (stale (alist-get 'stale args))
         (tags (orsb-arg-list args 'tags))
         (sort-by (or (orsb-arg args 'sort_by) "modified"))
         (nodes (seq-filter (lambda (n) (= (org-roam-node-level n) 0)) (org-roam-node-list)))
         (health (when (or (orsb-arg-bool args 'orphan) (orsb-arg-bool args 'broken_links)) (orsb-core-health))))
    (when (orsb-arg-bool args 'orphan)
      (let ((ids (mapcar #'car (plist-get health :orphans))))
        (setq nodes (seq-filter (lambda (n) (member (org-roam-node-id n) ids)) nodes))))
    (when (orsb-arg-bool args 'broken_links)
      (let ((files (mapcar #'car (plist-get health :broken))))
        (setq nodes (seq-filter (lambda (n) (member (org-roam-node-file n) files)) nodes))))
    (when-let ((to (orsb-arg args 'linked_to)))
      (let* ((target (orsb-core-resolve to))
             (files (mapcar (lambda (r) (nth 2 r)) (orsb-core-backlinks target t))))
        (setq nodes (seq-filter (lambda (n) (member (org-roam-node-file n) files)) nodes))))
    (let ((records (mapcar (lambda (n) (cons n (orsb-tools--light-record n))) nodes)))
      (when type (setq records (seq-filter (lambda (r) (equal (alist-get 'node_type (cdr r)) type)) records)))
      (when status
        (let ((s (or (orsb-tools--canonical-status status) status)))
          (setq records (seq-filter (lambda (r) (equal (orsb-tools--canonical-status (alist-get 'status (cdr r))) s)) records))))
      (when (and stale (not (eq stale :json-null)))
        (let ((want (not (eq stale :json-false))))
          (setq records (seq-filter (lambda (r) (eq (eq (alist-get 'stale (cdr r)) t) want)) records))))
      (when tags
        (setq records (seq-filter (lambda (r) (seq-every-p (lambda (tg) (seq-contains-p (alist-get 'tags (cdr r)) tg)) tags)) records)))
      (mapcar #'cdr
              (pcase sort-by
                ("title" (seq-sort-by (lambda (r) (downcase (or (alist-get 'title (cdr r)) ""))) #'string< records))
                ("created" (seq-sort-by (lambda (r) (orsb-tools--created-key (cdr r) (car r))) #'string> records))
                ("modified" (seq-sort-by (lambda (r) (or (alist-get 'modified (cdr r)) "")) #'string> records))
                (_ (orsb-error 'invalid-argument "sort_by must be modified, created or title")))))))

(defun orsb-tools--list (args)
  "Shared filter/sort for list_nodes and the deprecated get_projects (first LIMIT)."
  (seq-take (orsb-tools--list-records args) (orsb-arg-int args 'limit 50)))

(defun orsb-tool-list-nodes (args)
  "list_nodes."
  (let ((p (orsb-tools--page (orsb-tools--list-records args) args 50)))
    (append (cdr p) `((nodes . ,(orsb--vector (car p)))))))

(defun orsb-tool-get-projects (args)
  "get_projects (deprecated): project nodes, optional status / stale filter."
  (let* ((orsb-stale-days (orsb-arg-int args 'days_threshold orsb-stale-days))
         (records (orsb-tools--list (append `((node_type . "project") (limit . 500)) args))))
    `((total . ,(length records)) (stale_days . ,orsb-stale-days)
      (projects . ,(orsb--vector
                    (mapcar (lambda (r) (if (assq 'next_action r) r (append r '((next_action))))) records))))))

(defun orsb-tool-find-related (args)
  "find_related: notes close in meaning to a note (or to some text)."
  (unless (fboundp 'orsb-search-similar-vector)
    (orsb-error 'unavailable "Semantic search is not loaded (orsb-search)"))
  (let* ((id (orsb-arg args 'id))
         (text (orsb-arg args 'text))
         (limit (min 25 (orsb-arg-int args 'limit 5)))
         (cutoff (let ((c (orsb-arg args 'cutoff))) (if (numberp c) c 0.5)))
         (node (and id (orsb-core-resolve id)))
         (_ (unless (or node text) (orsb-error 'invalid-argument "give id (a note) or text")))
         (file (and node (org-roam-node-file node)))
         (qv (condition-case err
                 (or (and node
                          (if (= (org-roam-node-level node) 0)
                              (orsb-search-centroid file)
                            (orsb-core--with-node node
                              (orsb-search-centroid file (point) (save-excursion (org-end-of-subtree t t) (point))))))
                     (org-roam-ai-generate-embedding
                      (or text (let ((b (concat (org-roam-node-title node) "\n" (orsb-core-node-body node))))
                                 (substring b 0 (min 4000 (length b))))))
                     (error "no vector"))
               (error (orsb-error-hint 'unavailable "The note has no stored embeddings and the embedding service did not answer; sync with embeddings:true, or use search."
                                       "Could not get a vector to compare: %s" (error-message-string err)))))
         (linked (when node
                   (let ((links (orsb-tools--links node)))
                     (seq-uniq (delq nil (append (mapcar (lambda (l) (when-let ((n (org-roam-node-from-id (alist-get 'id l)))) (org-roam-node-file n)))
                                                         (car links))
                                                 (mapcar (lambda (r) (nth 2 r)) (orsb-core-backlinks node t))))))))
         (seen (make-hash-table :test 'equal))
         (hits nil))
    (dolist (r (orsb-search-similar-vector qv (* 4 limit) cutoff (and file (list file))))
      (pcase-let ((`(,f ,score ,pos ,_h) r))
        (unless (or (gethash f seen) (>= (length hits) limit))
          (puthash f t seen)
          (when-let ((n (orsb-core--first-node-in-file f)))
            (let ((info (ignore-errors (orsb-search-chunk-info f pos 200))))
              (push (append (orsb-tools--ref n)
                            `((node_type . ,(cdr (assoc "NODE-TYPE" (org-roam-node-properties n))))
                              (score . ,score)
                              (linked . ,(if (member f linked) t :json-false))
                              ,@(when (plist-get info :heading) `((section . ,(plist-get info :heading))))
                              ,@(when (plist-get info :id) `((section_id . ,(plist-get info :id))))
                              (snippet . ,(plist-get info :snippet))))
                    hits))))))
    `(,@(when node `((id . ,(org-roam-node-id node)) (title . ,(org-roam-node-title node))))
      (total . ,(length hits))
      (related . ,(orsb--vector (nreverse hits)))
      ,@(when node '((hint . "linked=false marks notes this one is not yet connected to; link_nodes connects them."))))))

(defcustom orsb-duplicate-similarity nil
  "When a number, create_node also flags notes whose meaning is this close.
Compared with cosine similarity of the title (0 to 1, e.g. 0.9) against
stored embeddings.  Costs a call to the embedding service per create; nil
checks titles and aliases only."
  :type '(choice (const :tag "Titles and aliases only" nil) number)
  :group 'orsb)

(defun orsb-tools--duplicates (title parent)
  "Notes (or, with PARENT, headings under it) that look like TITLE, as refs."
  (if parent
      (seq-filter #'identity
                  (mapcar (lambda (h)
                            (when (orsb-core-string= (plist-get h :title) title)
                              `((title . ,(plist-get h :title)) (id . ,(plist-get h :id)) (why . "same heading under this parent"))))
                          (orsb-core-outline parent)))
    (let* ((exact (mapcar (lambda (c) (when-let ((n (org-roam-node-from-id (car c))))
                                        (append (orsb-tools--ref n) '((why . "same title or alias")))))
                          (orsb-core--title-candidates title)))
           (similar (when (and (numberp orsb-duplicate-similarity) (fboundp 'orsb-search-similar))
                      (condition-case nil
                          (mapcar (lambda (r) (when-let ((n (orsb-core--first-node-in-file (car r))))
                                                (append (orsb-tools--ref n) `((why . "similar meaning") (score . ,(cadr r))))))
                                  (orsb-search-similar title 3 orsb-duplicate-similarity))
                        (error nil)))))
      (seq-uniq (delq nil (append exact similar)) (lambda (a b) (equal (alist-get 'id a) (alist-get 'id b)))))))

(defun orsb-tool-create-node (args)
  "create_node: a typed note, or with parent_id a heading inside a note."
  (orsb-require args 'title)
  (let* ((title (string-trim (orsb-arg args 'title)))
         (parent (when-let ((p (orsb-arg args 'parent_id))) (orsb-core-resolve p)))
         (type (or (orsb-arg args 'node_type) (unless parent "note")))
         (on-dup (or (orsb-arg args 'on_duplicate) "ask"))
         (dups (unless (equal on-dup "create") (orsb-tools--duplicates title parent))))
    (unless (member on-dup '("ask" "create" "use_existing"))
      (orsb-error 'invalid-argument "on_duplicate must be ask, create or use_existing"))
    (unless parent
      (dolist (k '(todo scheduled deadline section))
        (when (orsb-arg args k)
          (orsb-error-hint 'invalid-argument "Pass parent_id to create a task heading inside a note."
                           "%s applies to a heading, not to a new note" k))))
    (cond
     ((and dups (equal on-dup "ask"))
      `((created . :json-false)
        (possible_duplicates . ,(orsb--vector dups))
        (hint . "Nothing was created. Use one of these ids, or call again with on_duplicate \"use_existing\" (first match) or \"create\" (make it anyway).")))
     ((and dups (equal on-dup "use_existing") (alist-get 'id (car dups)))
      (append '((created . :json-false))
              (orsb-tools--full-record (orsb-core-resolve (alist-get 'id (car dups))) nil)))
     (t
      (let ((status (when-let ((s (orsb-arg args 'status))) (orsb-tools--check-status s type))))
        (orsb-tools--change
         args "create_node" (format "create %s" title)
         (lambda ()
           (let ((node
                  (if parent
                      (orsb-core-insert-heading parent title
                                                :section (orsb-arg args 'section)
                                                :body (orsb-arg args 'body)
                                                :properties (append (orsb-arg-props args 'properties)
                                                                    (when status `(("STATUS" . ,status))))
                                                :todo (orsb-arg args 'todo)
                                                :tags (orsb-arg-list args 'tags)
                                                :scheduled (orsb-arg args 'scheduled)
                                                :deadline (orsb-arg args 'deadline))
                    (orsb-core-create-node type title
                                           :body (orsb-arg args 'body)
                                           :status status
                                           :next-action (orsb-arg args 'next_action)
                                           :context (orsb-arg args 'context)
                                           :follow-ups (orsb-arg-list args 'follow_ups)
                                           :one-liner (orsb-arg args 'one_liner)
                                           :due-date (orsb-arg args 'due_date)
                                           :hugo-section (orsb-arg args 'hugo_section)
                                           :tags (orsb-arg-list args 'tags)
                                           :properties (orsb-arg-props args 'properties)))))
             (append '((created . t)) (orsb-tools--full-record node nil))))))))))

(defun orsb-tool-add-heading (args)
  "add_heading (deprecated: create_node with parent_id)."
  (orsb-require args 'id 'heading)
  (let* ((parent (orsb-core-resolve (orsb-arg args 'id)))
         (todo (orsb-arg args 'todo))
         (heading (if todo (concat (upcase todo) " " (orsb-arg args 'heading)) (orsb-arg args 'heading)))
         (node (orsb-core-add-heading parent heading (orsb-arg args 'body)
                                      (orsb-arg-props args 'properties)
                                      (orsb-arg-int args 'level 1))))
    (orsb-tools--full-record node nil)))

(defun orsb-tools--child-heading (node heading)
  "The heading titled HEADING inside NODE, as a node.\nThe heading gets an :ID: first if it has none."
  (let ((file (org-roam-node-file node)) id)
    (orsb-core--with-node node
      (orsb-core--goto-anchor node heading)
      (setq id (org-entry-get nil "ID"))
      (unless id
        (setq id (org-id-new))
        (org-entry-put nil "ID" id)))
    (when (buffer-modified-p (find-buffer-visiting file))
      (orsb-core--after-write file))
    (if orsb-core-dry-run
        (orsb-error-hint 'invalid-argument "Run without dry_run once (it only adds an :ID: to the heading), or address the heading by an id."
                         "Heading %S has no id yet, so a dry run cannot follow it" heading)
      (orsb-core-resolve id))))

(defun orsb-tool-set-node (args)
  "set_node: title, status, todo, planning, properties, tags, aliases, keywords."
  (orsb-require args 'id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id))))
    (orsb-tools--change
     args "set_node" (format "set %s" (org-roam-node-title node))
     (lambda ()
       (let ((changed nil) (links-updated nil))
         (when-let ((heading (orsb-arg args 'heading)))
           (setq node (orsb-tools--child-heading node heading)))
         (when-let ((title (orsb-arg args 'title)))
           (let ((old (org-roam-node-title node)))
             (orsb-core-set-title node title) (push "title" changed)
             (unless orsb-core-dry-run (setq node (orsb-core-resolve (org-roam-node-id node))))
             (unless (eq (alist-get 'update_links args) :json-false)
               (setq links-updated (orsb-core-propagate-rename node old title)))))
         (when-let ((status (orsb-arg args 'status)))
           (orsb-core-set-properties node `(("STATUS" . ,(orsb-tools--check-status status node)))) (push "status" changed))
         (when (assq 'todo args)
           (orsb-core-set-todo node (orsb-arg args 'todo)) (push "todo" changed))
         (dolist (what '(scheduled deadline))
           (when (assq what args)
             (orsb-core-set-planning node what (orsb-arg args what)) (push (symbol-name what) changed)))
         (when-let ((props (orsb-arg-props args 'properties)))
           (orsb-core-set-properties node props) (push "properties" changed))
         (when (assq 'tags args)
           (orsb-core-set-tags node (orsb-arg-list args 'tags)) (push "tags" changed))
         (let ((add (orsb-arg-list args 'tags_add)) (remove (orsb-arg-list args 'tags_remove)))
           (when (or add remove)
             (let ((current (orsb-core-node-local-tags node)))
               (orsb-core-set-tags node (seq-uniq (append (seq-remove (lambda (tg) (member tg remove)) current) add))))
             (push "tags" changed)))
         (let ((add (orsb-arg-list args 'aliases_add)) (remove (orsb-arg-list args 'aliases_remove)))
           (when (or add remove)
             (orsb-core-set-aliases node add remove) (push "aliases" changed)))
         (when-let ((kws (orsb-arg-props args 'keywords)))
           (orsb-core-set-keywords node kws) (push "keywords" changed))
         (unless changed
           (orsb-error 'invalid-argument "Nothing to change: give title, status, todo, scheduled, deadline, properties, tags, tags_add, tags_remove, aliases_add, aliases_remove or keywords"))
         (append `((changed . ,(orsb--vector (nreverse changed))))
                 (when links-updated
                   `((links_updated . ,(orsb--vector (mapcar #'orsb-tools--relative links-updated)))))
                 (if orsb-core-dry-run
                     (orsb-tools--ref node)
                   (orsb-tools--full-record (orsb-core-resolve (org-roam-node-id node)) nil))))))))

(defun orsb-tool-update-body (args)
  "update_body: append | prepend | replace, optionally within a section."
  (orsb-require args 'id 'content)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (content (orsb-arg args 'content))
         (section (orsb-arg args 'section))
         (create (orsb-arg-bool args 'create_section))
         (mode (or (orsb-arg args 'mode) "append"))
         (force (orsb-arg-bool args 'force)))
    (unless (member mode '("append" "prepend" "replace"))
      (orsb-error 'invalid-argument "mode must be append, prepend or replace"))
    (cl-flet ((size () (length (or (if section
                                       (condition-case nil (orsb-core-section-body node section) (orsb-error nil))
                                     (orsb-core-node-body node))
                                   ""))))
      (let ((before (size)))
        (orsb-tools--change
         args "update_body" (format "%s %s%s" mode (org-roam-node-title node) (if section (format " / %s" section) ""))
         (lambda ()
           (cond
            (section (orsb-core-update-section node section content mode (not create)))
            ((equal mode "replace")
             (if (= (org-roam-node-level node) 0)
                 (orsb-core-replace-file-body node content force)
               (orsb-core-set-body node content)))
            (t (orsb-core-append-body node content (equal mode "prepend"))))
           `((id . ,(org-roam-node-id node)) (file . ,(orsb-tools--relative (org-roam-node-file node)))
             (mode . ,mode) (section . ,section)
             (chars_before . ,before) (chars_after . ,(size)))))))))

(defun orsb-tool-set-checkbox (args)
  "set_checkbox: tick or untick one checklist item."
  (orsb-require args 'id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (checked (not (eq (alist-get 'checked args) :json-false))))
    (orsb-tools--change
     args "set_checkbox" (format "checkbox in %s" (org-roam-node-title node))
     (lambda ()
       (let* ((r (orsb-core-set-checkbox node
                                         :item (orsb-arg args 'item)
                                         :index (orsb-arg-int args 'index nil)
                                         :section (orsb-arg args 'section)
                                         :checked checked))
              (boxes (orsb-core-checkboxes node)))
         `((id . ,(org-roam-node-id node)) (item . ,(plist-get r :text)) (index . ,(plist-get r :index))
           (heading . ,(plist-get r :heading))
           (checked . ,(if checked t :json-false))
           (changed . ,(if (eq (and (plist-get r :was) t) checked) :json-false t))
           (progress . ,(format "%d/%d" (seq-count (lambda (b) (plist-get b :checked)) boxes) (length boxes)))))))))

(defun orsb-tools--task-json (task)
  "TASK plist as a JSON alist, empty fields left out."
  (seq-filter #'cdr
              `((kind . ,(plist-get task :kind)) (text . ,(plist-get task :text))
                (state . ,(plist-get task :state))
                (done . ,(if (plist-get task :done) t :json-false))
                (id . ,(plist-get task :file_id)) (title . ,(plist-get task :title))
                (file . ,(orsb-tools--relative (plist-get task :file)))
                (heading . ,(plist-get task :heading)) (heading_id . ,(plist-get task :heading_id))
                (index . ,(plist-get task :index))
                (scheduled . ,(plist-get task :scheduled)) (deadline . ,(plist-get task :deadline))
                (priority . ,(plist-get task :priority)))))

(defun orsb-tool-get-tasks (args)
  "get_tasks: TODO headings and checkboxes across the vault."
  (let* ((state (or (orsb-arg args 'state) "open"))
         (kind (or (orsb-arg args 'kind) "all"))
         (node (when-let ((i (orsb-arg args 'id))) (orsb-core-resolve i)))
         (person (when-let ((p (orsb-arg args 'person)))
                   (or (ignore-errors (orsb-core-resolve p)) p)))
         (due (orsb-tools--date-arg args 'due_before))
         (query (when-let ((q (orsb-arg args 'query))) (downcase q)))
         (range (when (and node (> (org-roam-node-level node) 0))
                  (orsb-core--with-node node
                    (cons (line-number-at-pos)
                          (save-excursion (org-end-of-subtree t t) (line-number-at-pos))))))
         (tasks (orsb-core-tasks (when node (list (org-roam-node-file node))))))
    (unless (member kind '("all" "todo" "checkbox"))
      (orsb-error 'invalid-argument "kind must be all, todo or checkbox"))
    (setq tasks
          (seq-filter
           (lambda (tk)
             (and (pcase state
                    ("open" (not (plist-get tk :done)))
                    ("done" (plist-get tk :done))
                    ("all" t)
                    (_ (equal (upcase state) (plist-get tk :state))))
                  (or (equal kind "all") (equal kind (plist-get tk :kind)))
                  (or (null range) (and (>= (plist-get tk :line) (car range)) (< (plist-get tk :line) (cdr range))))
                  (or (null person)
                      (let ((text (plist-get tk :text)))
                        (if (org-roam-node-p person)
                            (or (string-search (org-roam-node-id person) text)
                                (string-search (downcase (org-roam-node-title person)) (downcase text))
                                (equal (plist-get tk :file_id) (org-roam-node-id person)))
                          (string-search (downcase person) (downcase text)))))
                  (or (null due)
                      (let ((d (or (plist-get tk :deadline) (plist-get tk :scheduled))))
                        (and d (not (string> (substring d 0 10) due)))))
                  (or (null query)
                      (string-search query (downcase (concat (plist-get tk :text) " " (or (plist-get tk :heading) "")))))))
           tasks))
    ;; dated first, earliest first; undated keep document order
    (setq tasks (seq-sort-by (lambda (tk) (let ((d (or (plist-get tk :deadline) (plist-get tk :scheduled)))) (if d (substring d 0 10) "9999")))
                             #'string< tasks))
    (let ((p (orsb-tools--page tasks args 30)))
      (append (cdr p)
              `((tasks . ,(orsb--vector (mapcar #'orsb-tools--task-json (car p))))
                (how_to_complete . "checkbox: set_checkbox with id and index. todo: set_node with id=heading_id (or id and heading) and todo=\"DONE\"."))))))

(defun orsb-tool-delete-node (args)
  "delete_node: a heading subtree, or a whole note (archived by default).
Accepts the pre-2.0 `node_id' / `identifier' spellings too."
  (let* ((id (or (orsb-arg args 'id) (orsb-arg args 'node_id) (orsb-arg args 'identifier)
                 (orsb-error 'invalid-argument "Missing required argument: id")))
         (node (orsb-core-resolve id))
         (archive (let ((v (alist-get 'archive args)))
                    ;; by default a note is archived, or deleted when already in the archive
                    (if (null v)
                        (not (string-prefix-p (file-name-as-directory (file-truename (orsb-core--vault-file orsb-archive-directory)))
                                              (file-truename (org-roam-node-file node))))
                      (orsb-arg-bool args 'archive))))
         (level (org-roam-node-level node))
         (backlinks (orsb-core-backlinks node t)))
    (orsb-tools--change
     args "delete_node" (format "%s %s" (if (and archive (= level 0)) "archive" "delete") (org-roam-node-title node))
     (lambda ()
       (let ((result (orsb-core-delete-node node (and archive (= level 0)))))
         `((id . ,(org-roam-node-id node)) (title . ,(org-roam-node-title node)) (level . ,level)
           (action . ,(cond ((> level 0) "deleted") (archive "archived") (t "deleted")))
           (file . ,(orsb-tools--relative (or result (org-roam-node-file node))))
           (backlinks . ,(orsb--vector (mapcar (lambda (r) `((id . ,(nth 0 r)) (title . ,(nth 1 r)) (file . ,(orsb-tools--relative (nth 2 r))))) backlinks)))
           ,@(when (and backlinks (not (and archive (= level 0))))
               '((hint . "These notes still link to the deleted node; link_nodes action \"remove\" cleans each, or restore undoes the delete.")))))))))

(defun orsb-tool-link-nodes (args)
  "link_nodes: add or remove an [[id:...]] link from id to target_id."
  (orsb-require args 'id 'target_id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (target (orsb-core-resolve (orsb-arg args 'target_id)))
         (action (or (orsb-arg args 'action) "add"))
         (both (orsb-arg-bool args 'both_ways)))
    (unless (member action '("add" "remove"))
      (orsb-error 'invalid-argument "action must be add or remove"))
    (when (equal (org-roam-node-id node) (org-roam-node-id target))
      (orsb-error 'invalid-argument "A note cannot link to itself"))
    (orsb-tools--change
     args "link_nodes" (format "%s link %s -> %s" action (org-roam-node-title node) (org-roam-node-title target))
     (lambda ()
       (append
        `((id . ,(org-roam-node-id node)) (target_id . ,(org-roam-node-id target))
          (target_title . ,(org-roam-node-title target)))
        (if (equal action "add")
            (let ((r (orsb-core-link node target (orsb-arg args 'section)))
                  (back (when both (orsb-core-link target node nil))))
              `((action . ,(symbol-name r))
                ,@(when both `((back_link . ,(symbol-name back))))))
          (let ((n (orsb-core-unlink node (org-roam-node-id target)))
                (back (when both (orsb-core-unlink target (org-roam-node-id node)))))
            `((action . "removed") (removed . ,n) ,@(when both `((back_removed . ,back)))))))))))

(defun orsb-tool-move-node (args)
  "move_node: refile a heading under another note or heading, or make it a note."
  (orsb-require args 'id)
  (let* ((node (orsb-core-resolve (orsb-arg args 'id)))
         (to-note (orsb-arg-bool args 'to_note))
         (target (when-let ((tg (orsb-arg args 'target_id))) (orsb-core-resolve tg))))
    (unless (or to-note target)
      (orsb-error 'invalid-argument "give target_id (where it goes) or to_note:true (make it a note of its own)"))
    (orsb-tools--change
     args "move_node" (format "move %s" (org-roam-node-title node))
     (lambda ()
       (if to-note
           (let ((path (orsb-core-promote-node node (orsb-arg args 'node_type)
                                               (not (eq (alist-get 'leave_link args) :json-false)))))
             `((id . ,(org-roam-node-id node)) (title . ,(org-roam-node-title node))
               (action . "promoted") (file . ,(orsb-tools--relative path))))
         (orsb-core-move-node node target (orsb-arg args 'section))
         `((id . ,(org-roam-node-id node)) (title . ,(org-roam-node-title node))
           (action . "moved") (target_id . ,(org-roam-node-id target))
           (file . ,(orsb-tools--relative (org-roam-node-file target)))))))))

(defun orsb-tool-merge-nodes (args)
  "merge_nodes: fold one note into another."
  (orsb-require args 'id 'target_id)
  (let* ((source (orsb-core-resolve (orsb-arg args 'id)))
         (target (orsb-core-resolve (orsb-arg args 'target_id))))
    (orsb-tools--change
     args "merge_nodes" (format "merge %s into %s" (org-roam-node-title source) (org-roam-node-title target))
     (lambda ()
       (let ((kept (orsb-core-merge-node source target (orsb-arg args 'section))))
         `((id . ,(org-roam-node-id source)) (title . ,(org-roam-node-title source))
           (target_id . ,(org-roam-node-id target)) (target_title . ,(org-roam-node-title target))
           (removed_file . ,(orsb-tools--relative (org-roam-node-file source)))
           (links_kept . ,kept)))))))

(defun orsb-tool-add-daily-entry (args)
  "add_daily_entry."
  (orsb-require args 'title 'points)
  (let ((date (orsb-tools--date-arg args 'date)))
    (orsb-tools--change
     args "add_daily_entry" (format "daily entry %s" (orsb-arg args 'title))
     (lambda ()
       (let ((path (orsb-core-add-daily-entry (orsb-arg args 'title)
                                              (orsb-arg-list args 'points)
                                              (orsb-arg-list args 'next_steps)
                                              (orsb-arg-list args 'tags)
                                              (orsb-arg args 'timestamp)
                                              (equal (orsb-arg args 'type) "todo")
                                              date)))
         `((file . ,(orsb-tools--relative path)) (title . ,(orsb-arg args 'title))
           (date . ,(or date (format-time-string "%Y-%m-%d")))))))))

(defun orsb-tool-get-daily (args)
  "get_daily {date} (deprecated: get_node with id \"today\" or a date)."
  `((date . ,(or (orsb-arg args 'date) (format-time-string "%Y-%m-%d")))
    (content . ,(orsb-core-daily-content (orsb-arg args 'date)))))

(defun orsb-tool-log-to-inbox (args)
  "log_to_inbox {text, linked_id, category}."
  (orsb-require args 'text)
  (let* ((linked (when-let ((l (orsb-arg args 'linked_id))) (orsb-core-resolve l)))
         (category (orsb-arg args 'category))
         (text (if category (format "[%s] %s" category (orsb-arg args 'text)) (orsb-arg args 'text))))
    (orsb-tools--change
     args "log_to_inbox" (format "inbox: %s" (truncate-string-to-width text 60))
     (lambda ()
       (let* ((created (orsb-core-log-to-inbox text linked))
              (today (format-time-string "%Y-%m-%d"))
              (entries (orsb-core--inbox-lines today)))
         `((logged . t)
           (ref . ,(plist-get (car (last entries)) :ref))
           (linked_id . ,(and linked (org-roam-node-id linked)))
           (created_people . ,(orsb--vector created))))))))

(defun orsb-tool-resolve-inbox (args)
  "resolve_inbox {ref, target_id, section}."
  (orsb-require args 'ref)
  (let ((target (when-let ((tg (orsb-arg args 'target_id))) (orsb-core-resolve tg))))
    (orsb-tools--change
     args "resolve_inbox" (format "resolve inbox %s" (orsb-arg args 'ref))
     (lambda ()
       (let ((e (orsb-core-resolve-inbox (orsb-arg args 'ref) target (orsb-arg args 'section))))
         `((ref . ,(plist-get e :ref)) (text . ,(plist-get e :text)) (resolved . t)
           ,@(when target `((filed_to . ,(orsb-tools--ref target))))))))))

(defun orsb-tool-get-followups (args)
  "get_followups {dangling} (deprecated: get_review)."
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
  "get_digest {days} (deprecated: get_review)."
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

(defun orsb-tool-get-review (args)
  "get_review: what needs attention, each list capped, with totals."
  (let* ((n (orsb-arg-int args 'limit 10))
         (days (orsb-arg-int args 'days 7))
         (soon (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 3))))
         (today (format-time-string "%Y-%m-%d"))
         (files (seq-filter (lambda (nd) (= (org-roam-node-level nd) 0)) (org-roam-node-list)))
         (records (mapcar #'orsb-tools--light-record files))
         (projects (seq-filter (lambda (r) (equal (alist-get 'node_type r) "project")) records))
         (active (seq-sort-by (lambda (r) (or (alist-get 'modified r) "")) #'string>
                              (seq-filter (lambda (r) (equal (orsb-tools--canonical-status (alist-get 'status r)) "active")) projects)))
         (stale (seq-filter (lambda (r) (eq (alist-get 'stale r) t)) projects))
         (tasks (seq-filter (lambda (tk) (and (not (plist-get tk :done))
                                              (let ((d (or (plist-get tk :deadline) (plist-get tk :scheduled))))
                                                (and d (not (string> (substring d 0 10) soon))))))
                            (orsb-core-tasks)))
         (tasks (seq-sort-by (lambda (tk) (substring (or (plist-get tk :deadline) (plist-get tk :scheduled)) 0 10)) #'string< tasks))
         (people (orsb-core-followups))
         (inbox (orsb-core-inbox days))
         (health (orsb-core-health))
         (dangling (orsb-core-dangling-followups))
         (stale-emb (when (and (orsb-arg-bool args 'check_embeddings) (fboundp 'orsb-search-file-stale-p))
                      (seq-filter (lambda (f) (ignore-errors (orsb-search-file-stale-p f))) (org-roam-list-files)))))
    (cl-flet ((section (items fn)
                `((total . ,(length items)) (items . ,(orsb--vector (mapcar fn (seq-take items n))))))
              (short (r) (seq-filter #'cdr `((id . ,(alist-get 'id r)) (title . ,(alist-get 'title r))
                                              (status . ,(alist-get 'status r))
                                              (next_action . ,(alist-get 'next_action r))
                                              (days_since_modified . ,(alist-get 'days_since_modified r))))))
      `((generated_at . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
        (active_projects . ,(section active #'short))
        (stale_projects . ,(section stale #'short))
        (due_tasks . ,(section tasks (lambda (tk) (append (orsb-tools--task-json tk)
                                                          (let ((d (or (plist-get tk :deadline) (plist-get tk :scheduled))))
                                                            (when (string< (substring d 0 10) today) '((overdue . t))))))))
        (followups . ,(section people (lambda (p) `((id . ,(org-roam-node-id (plist-get p :node)))
                                                    (title . ,(org-roam-node-title (plist-get p :node)))
                                                    (items . ,(orsb--vector (plist-get p :followups)))))))
        (inbox . ,(section inbox (lambda (e) `((ref . ,(plist-get e :ref)) (text . ,(plist-get e :text))))))
        (health
         . ((orphans . ,(section (plist-get health :orphans) (lambda (r) `((id . ,(nth 0 r)) (title . ,(nth 3 r)) (file . ,(orsb-tools--relative (nth 1 r)))))))
            (broken_links . ,(section (plist-get health :broken) (lambda (b) `((file . ,(orsb-tools--relative (car b))) (missing_id . ,(cdr b))))))
            (duplicate_titles . ,(section (plist-get health :duplicates) (lambda (d) `((title . ,(car d)) (ids . ,(orsb--vector (cdr d)))))))
            (dangling_name_links . ,(section dangling (lambda (p) `((name . ,(plist-get p :name)) (file . ,(orsb-tools--relative (plist-get p :file)))))))
            ,@(when stale-emb `((stale_embeddings . ,(section stale-emb (lambda (f) (orsb-tools--relative f))))))))
        (hint . "Open inbox entries close with resolve_inbox ref; tasks with set_checkbox or set_node; orphans with link_nodes.")))))

(defun orsb-tool-restore (args)
  "restore: undo a recorded change, or list recent ones."
  (let ((id (orsb-arg args 'change_id)))
    (if (null id)
        (let ((changes (orsb-core-changes (orsb-arg-int args 'limit 10))))
          `((changes . ,(orsb--vector
                         (mapcar (lambda (c)
                                   (seq-filter #'cdr
                                               `((change_id . ,(plist-get c :id)) (time . ,(plist-get c :time))
                                                 (tool . ,(plist-get c :tool)) (summary . ,(plist-get c :summary))
                                                 (files . ,(orsb--vector (plist-get c :files)))
                                                 (restored_by . ,(plist-get c :restored)))))
                                 changes)))
            (hint . "Pass a change_id to undo that change; restores can be undone the same way.")))
      (let* ((dry (orsb-arg-bool args 'dry_run))
             (r (orsb-core-restore id (orsb-arg-bool args 'force) dry)))
        `((restored . ,id)
          (files . ,(orsb--vector (mapcar (lambda (f) `((file . ,(orsb-tools--relative (car f))) (action . ,(cdr f)))) (car r))))
          ,@(if dry `((dry_run . t) (would_change . ,(orsb--vector (cdr r))))
              `((change_id . ,(cdr r)))))))))

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
  (let ((todo (orsb-core--todo-sets)))
    `((version . ,orsb-tools-version)
      (tool_profile . ,(symbol-name orsb-mcp-tool-profile))
      (id_forms . ["org-roam id" "path relative to the vault, or absolute" "exact title or alias"
                   "\"today\", \"yesterday\" or YYYY-MM-DD for a daily note"])
      (node_types . ,(orsb--vector (mapcar (lambda (c) `((type . ,(car c)) (directory . ,(cdr c)))) orsb-directories)))
      (status_values . ,(orsb--vector orsb-status-values))
      (blog_status_values . ,(orsb--vector orsb-blog-status-values))
      (status_aliases . ,(orsb--object orsb-status-aliases))
      (todo_keywords . ((open . ,(orsb--vector (car todo))) (done . ,(orsb--vector (cdr todo)))))
      (tags_in_use . ,(orsb--vector
                       (mapcar #'car (seq-take (org-roam-db-query [:select [tag (funcall count tag)] :from tags
                                                                   :group-by tag :order-by [(desc (funcall count tag))]])
                                               50))))
      (stale_days . ,orsb-stale-days)
      (property_keys . ,(orsb--vector orsb-known-property-keys))
      (daily_directory . ,orsb-daily-directory)
      (inbox_heading . ,orsb-inbox-heading)
      (hugo_sections . ,(orsb--vector orsb-hugo-sections))
      (blog_enabled . ,(if orsb-hugo-base-dir t :json-false))
      (legacy_tools . ,(if orsb-mcp-legacy-tools t :json-false))
      (envelope . "{ok:true,data} | {ok:false,error:{code,message,hint}} with isError")
      (undo . "Every write returns change_id; restore with that id puts the files back. dry_run:true previews a write without making it."))))

;;;; Envelope

(defun orsb-tools--hint (code)
  "A generic hint for error CODE, used when the error carries none."
  (pcase code
    ('not-found "Use search (mode title) to find the id, or pass a vault-relative path.")
    ('invalid-argument "Call get_schema for the accepted values.")
    ('refused "The operation was blocked by a safety rule; read the message.")
    ('unavailable "A required module or service is not available on the server.")
    (_ "Unexpected server error; retrying will not help.")))

(defun orsb-tools--clean-args (args)
  "ARGS as an alist, treating a non-object (e.g. []) as no arguments."
  (if (and (listp args) (or (null args) (consp (car args)))) args nil))

(defun orsb-tools--run (fn args)
  "Run FN on ARGS; return (:ok DATA) or (:error CODE MESSAGE HINT)."
  (condition-case err
      (list :ok (funcall fn (orsb-tools--clean-args args)))
    (orsb-error (list :error (nth 1 err) (nth 2 err) (nth 3 err)))
    (error (list :error 'internal (error-message-string err) nil))))

(defun orsb-tools--envelope (fn args)
  "Run FN on ARGS and return the JSON envelope."
  (let ((r (orsb-tools--run fn args)))
    (if (eq (car r) :ok)
        (json-encode `((ok . t) (data . ,(orsb--object (nth 1 r)))))
      (json-encode `((ok . :json-false)
                     (error . ((code . ,(replace-regexp-in-string "-" "_" (symbol-name (nth 1 r))))
                               (message . ,(nth 2 r))
                               (hint . ,(or (nth 3 r) (orsb-tools--hint (nth 1 r)))))))))))

;;;; Registration

(defconst orsb-tools--id-schema
  '(id . ((type . "string") (description . "Note id, vault path, exact title, or today/YYYY-MM-DD")))
  "The shared id parameter.")

(defconst orsb-tools--dry-run-schema
  '(dry_run . ((type . "boolean") (default . :json-false) (description . "Show what would change; write nothing")))
  "The shared dry_run parameter.")

(defconst orsb-tools--string-list '((type . "array") (items . ((type . "string")))))

(defconst orsb-tools--definitions
  `(("search" orsb-tool-search
     "Find notes. mode: semantic (by meaning, default), contextual (word match), title (title or alias contains the query). Each hit has id, title, score, the matching section and a snippet; get_node reads the whole note."
     ("query")
     ((query . ((type . "string")))
      (mode . ((type . "string") (enum . ("semantic" "contextual" "title")) (default . "semantic")))
      (node_type . ((type . "string") (description . "Only notes of this type")))
      (limit . ((type . "integer") (default . 10) (description . "At most 25")))
      (cutoff . ((type . "number") (default . 0.5) (description . "Minimum similarity, semantic mode")))))
    ("get_node" orsb-tool-get-node
     "Read one note or heading: metadata, links, the outline of its sections, and the body (cut at max_chars; pass section to read one heading)."
     ("id")
     (,orsb-tools--id-schema
      (section . ((type . "string") (description . "Return only this heading's body")))
      (include_body . ((type . "boolean") (default . t)))
      (max_chars . ((type . "integer") (default . 6000)))))
    ("list_nodes" orsb-tool-list-nodes
     "List notes with filters. stale=true: unfinished and untouched for a while. orphan=true: no links in or out. broken_links=true: has a link to a missing note. linked_to: notes linking to that id. Pages with limit and offset."
     ()
     ((node_type . ((type . "string")))
      (status . ((type . "string") (description . "A status value (see get_schema)")))
      (stale . ((type . "boolean")))
      (orphan . ((type . "boolean")))
      (broken_links . ((type . "boolean")))
      (linked_to . ((type . "string") (description . "Only notes that link to this id")))
      (tags . (,@orsb-tools--string-list (description . "All of these tags")))
      (sort_by . ((type . "string") (enum . ("modified" "created" "title")) (default . "modified")))
      (limit . ((type . "integer") (default . 50)))
      (offset . ((type . "integer") (default . 0)))))
    ("find_related" orsb-tool-find-related
     "Notes close in meaning to a note (id) or to some text. Each result says whether it is already linked (linked=false is a link worth adding) and which section matched."
     ()
     (,orsb-tools--id-schema
      (text . ((type . "string") (description . "Compare with this text instead of a note")))
      (limit . ((type . "integer") (default . 5)))
      (cutoff . ((type . "number") (default . 0.5)))))
    ("create_node" orsb-tool-create-node
     "Create a note of a type, or with parent_id a heading (task, section) inside a note. If a note with the same title exists nothing is created and possible_duplicates is returned; on_duplicate decides."
     ("title")
     ((title . ((type . "string")))
      (node_type . ((type . "string") (enum . ("note" "project" "person" "idea" "admin" "blog" "reference" "howto")) (default . "note")))
      (parent_id . ((type . "string") (description . "Create a heading inside this note instead of a new note")))
      (section . ((type . "string") (description . "With parent_id: nest under this heading")))
      (body . ((type . "string")))
      (todo . ((type . "string") (description . "With parent_id: TODO keyword, e.g. TODO")))
      (scheduled . ((type . "string") (description . "With parent_id: YYYY-MM-DD")))
      (deadline . ((type . "string") (description . "With parent_id: YYYY-MM-DD")))
      (status . ((type . "string")))
      (next_action . ((type . "string") (description . "project: next action")))
      (due_date . ((type . "string") (description . "admin: YYYY-MM-DD")))
      (one_liner . ((type . "string") (description . "idea: one-line summary")))
      (context . ((type . "string") (description . "person: how you know them")))
      (follow_ups . (,@orsb-tools--string-list (description . "person: follow-up items")))
      (hugo_section . ((type . "string") (description . "blog: Hugo section")))
      (tags . ,orsb-tools--string-list)
      (properties . ((type . "object") (additionalProperties . ((type . "string")))))
      (on_duplicate . ((type . "string") (enum . ("ask" "use_existing" "create")) (default . "ask")))))
    ("set_node" orsb-tool-set-node
     "Change a note's or heading's metadata, not its text: title (links to it are updated), status, todo, scheduled, deadline, properties (null deletes one), tags, aliases, keywords. heading picks a heading inside the note by its title."
     ("id")
     (,orsb-tools--id-schema
      (heading . ((type . "string") (description . "Act on this heading inside the note")))
      (title . ((type . "string")))
      (update_links . ((type . "boolean") (default . t) (description . "On rename, update link text elsewhere")))
      (status . ((type . "string")))
      (todo . ((type . "string") (description . "TODO keyword, e.g. DONE; empty clears")))
      (scheduled . ((type . "string") (description . "YYYY-MM-DD; empty clears")))
      (deadline . ((type . "string") (description . "YYYY-MM-DD; empty clears")))
      (properties . ((type . "object") (additionalProperties . ((type . ("string" "null"))))))
      (tags . (,@orsb-tools--string-list (description . "Replace all own tags")))
      (tags_add . ,orsb-tools--string-list)
      (tags_remove . ,orsb-tools--string-list)
      (aliases_add . ,orsb-tools--string-list)
      (aliases_remove . ,orsb-tools--string-list)
      (keywords . ((type . "object") (additionalProperties . ((type . "string")))))
      ,orsb-tools--dry-run-schema))
    ("update_body" orsb-tool-update-body
     "Edit a note's text: append (default), prepend or replace. section targets one heading; a missing section is an error listing the real ones unless create_section=true. Big whole-note replaces need force=true."
     ("id" "content")
     (,orsb-tools--id-schema
      (content . ((type . "string")))
      (section . ((type . "string")))
      (create_section . ((type . "boolean") (default . :json-false)))
      (mode . ((type . "string") (enum . ("append" "prepend" "replace")) (default . "append")))
      (force . ((type . "boolean") (default . :json-false)))
      ,orsb-tools--dry-run-schema))
    ("set_checkbox" orsb-tool-set-checkbox
     "Tick (or untick) one checklist item in a note, by its text or by the index get_tasks gives."
     ("id")
     (,orsb-tools--id-schema
      (item . ((type . "string") (description . "Text of the item (a unique part is enough)")))
      (index . ((type . "integer") (description . "Checkbox number from get_tasks")))
      (section . ((type . "string") (description . "Only look under this heading")))
      (checked . ((type . "boolean") (default . t)))
      ,orsb-tools--dry-run-schema))
    ("get_tasks" orsb-tool-get-tasks
     "Open tasks across all notes: TODO headings and unchecked checkboxes, dated ones first. Filter by state, kind, note (id), person, due_before or query."
     ()
     ((state . ((type . "string") (default . "open") (description . "open, done, all, or a keyword like WAITING")))
      (kind . ((type . "string") (enum . ("all" "todo" "checkbox")) (default . "all")))
      ,orsb-tools--id-schema
      (person . ((type . "string") (description . "Tasks mentioning this person (name or id)")))
      (due_before . ((type . "string") (description . "YYYY-MM-DD, today, tomorrow or +N days")))
      (query . ((type . "string") (description . "Text the task contains")))
      (limit . ((type . "integer") (default . 30)))
      (offset . ((type . "integer") (default . 0)))))
    ("link_nodes" orsb-tool-link-nodes
     "Add (default) or remove a link from note id to target_id. Adding an existing link changes nothing. both_ways links back too."
     ("id" "target_id")
     (,orsb-tools--id-schema
      (target_id . ((type . "string")))
      (action . ((type . "string") (enum . ("add" "remove")) (default . "add")))
      (section . ((type . "string") (description . "Heading the link goes under")))
      (both_ways . ((type . "boolean") (default . :json-false)))
      ,orsb-tools--dry-run-schema))
    ("move_node" orsb-tool-move-node
     "Move a heading (with everything under it) into another note or heading, or with to_note=true make it a note of its own. Links to it keep working."
     ("id")
     (,orsb-tools--id-schema
      (target_id . ((type . "string") (description . "Note or heading to move it under")))
      (section . ((type . "string") (description . "Heading inside the target to move it under")))
      (to_note . ((type . "boolean") (default . :json-false)))
      (node_type . ((type . "string") (description . "With to_note: type of the new note")))
      (leave_link . ((type . "boolean") (default . t) (description . "With to_note: leave a link where it was")))
      ,orsb-tools--dry-run-schema))
    ("merge_nodes" orsb-tool-merge-nodes
     "Merge note id into target_id: it becomes a section of the target and its file is removed. Links to it keep working."
     ("id" "target_id")
     (,orsb-tools--id-schema
      (target_id . ((type . "string")))
      (section . ((type . "string") (description . "Heading inside the target to put it under")))
      ,orsb-tools--dry-run-schema))
    ("delete_node" orsb-tool-delete-node
     "Delete a heading, or a whole note (moved to the archive by default; archive=false deletes the file; a note already archived is deleted). Returns the notes that linked to it."
     ("id")
     (,orsb-tools--id-schema
      (archive . ((type . "boolean") (default . t)))
      ,orsb-tools--dry-run-schema))
    ("add_daily_entry" orsb-tool-add-daily-entry
     "Add an entry (title, bullet points, optional next steps and tags) to a daily note, today by default. type=todo makes it a TODO heading."
     ("title" "points")
     ((title . ((type . "string")))
      (points . ,orsb-tools--string-list)
      (next_steps . ,orsb-tools--string-list)
      (tags . ,orsb-tools--string-list)
      (date . ((type . "string") (description . "YYYY-MM-DD, default today")))
      (timestamp . ((type . "string") (description . "HH:MM, default now")))
      (type . ((type . "string") (enum . ("journal" "todo")) (default . "journal")))))
    ("log_to_inbox" orsb-tool-log-to-inbox
     "Add one line to today's inbox for later filing, optionally linked to a note. Returns its ref for resolve_inbox. [[Name]] creates a person note when none exists."
     ("text")
     ((text . ((type . "string")))
      (linked_id . ((type . "string") (description . "Note the entry is about")))
      (category . ((type . "string") (description . "Label shown in brackets, e.g. task, idea")))))
    ("resolve_inbox" orsb-tool-resolve-inbox
     "Mark an inbox entry done by its ref (get_review lists them). With target_id the text is also copied into that note."
     ("ref")
     ((ref . ((type . "string") (description . "Like 2026-11-19:3")))
      (target_id . ((type . "string") (description . "Note to file the entry into")))
      (section . ((type . "string") (description . "Heading in the target")))
      ,orsb-tools--dry-run-schema))
    ("get_review" orsb-tool-get-review
     "What needs attention, in one call: active and stale projects, tasks due within 3 days, follow-ups, open inbox entries with refs, and vault health (orphans, broken links, duplicate titles)."
     ()
     ((limit . ((type . "integer") (default . 10) (description . "Items per list")))
      (days . ((type . "integer") (default . 7) (description . "Inbox days to include")))
      (check_embeddings . ((type . "boolean") (default . :json-false) (description . "Also list notes with outdated embeddings (slower)")))))
    ("restore" orsb-tool-restore
     "Undo a change: every write returns a change_id, and restore with it puts the files back. Without change_id, lists recent changes."
     ()
     ((change_id . ((type . "string")))
      (force . ((type . "boolean") (default . :json-false) (description . "Restore even if the files changed again since")))
      (limit . ((type . "integer") (default . 10)))
      ,orsb-tools--dry-run-schema))
    ("get_schema" orsb-tool-get-schema
     "The vocabulary this server enforces: note types, status values, TODO keywords, tags in use, id forms. Call this instead of guessing."
     () ()))
  "The small-model profile: (NAME FUNCTION DESCRIPTION REQUIRED SCHEMA).")

(defconst orsb-tools--full-extras
  `(("get_blog_status" orsb-tool-get-blog-status
     "Blog overview: drafts with how many sections have text, recent published posts, ideas that could become posts."
     () ())
    ("sync" orsb-tool-sync
     "Bring the database (and optionally embeddings) up to date: id for one note; otherwise a scan queued for idle time (wait=true blocks; full=true forces a rebuild)."
     ()
     ((id . ((type . "string")))
      (embeddings . ((type . "boolean") (default . :json-false)))
      (full . ((type . "boolean") (default . :json-false)))
      (wait . ((type . "boolean") (default . :json-false))))))
  "Tools added when `orsb-mcp-tool-profile' is `full'.")

(defconst orsb-tools--deprecated
  `(("add_heading" orsb-tool-add-heading "create_node with parent_id"
     ("id" "heading")
     (,orsb-tools--id-schema
      (heading . ((type . "string")))
      (body . ((type . "string")))
      (properties . ((type . "object")))
      (level . ((type . "integer") (default . 1)))
      (todo . ((type . "string")))))
    ("get_daily" orsb-tool-get-daily "get_node with id \"today\" or a date"
     () ((date . ((type . "string")))))
    ("get_projects" orsb-tool-get-projects "list_nodes with node_type project"
     () ((status . ((type . "string"))) (stale . ((type . "boolean"))) (days_threshold . ((type . "integer")))))
    ("get_followups" orsb-tool-get-followups "get_review"
     () ((dangling . ((type . "boolean")))))
    ("get_digest" orsb-tool-get-digest "get_review"
     () ((days . ((type . "integer"))))))
  "2.0 tools folded into others in 2.1.
Each is (NAME FUNCTION REPLACEMENT REQUIRED SCHEMA).  They are
registered, marked deprecated, while `orsb-mcp-legacy-tools' is non-nil.")

(defun orsb-tools--active-definitions ()
  "The tool definitions the current `orsb-mcp-tool-profile' registers."
  (append orsb-tools--definitions
          (when (eq orsb-mcp-tool-profile 'full) orsb-tools--full-extras)))

(defun orsb-tools-function (name)
  "The implementation function of tool NAME (current, full-profile or deprecated)."
  (or (nth 1 (assoc name orsb-tools--definitions))
      (nth 1 (assoc name orsb-tools--full-extras))
      (nth 1 (assoc name orsb-tools--deprecated))
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
  "Mapper for the old search tools in MODE.
The old semantic_search returned each note's full text; callers that fed
that to an LLM (blog_draft) get a cleaned `excerpt' of up to 1500
characters instead of the 240-character snippet."
  (lambda (args)
    (let ((d (orsb-legacy--data "search" (append `((mode . ,mode)) args))))
      (orsb-legacy--ok
       `((query . ,(alist-get 'query d)) (total_found . ,(alist-get 'total d))
         (notes . ,(orsb--vector
                    (mapcar (lambda (h)
                              (let ((node (ignore-errors (org-roam-node-from-id (alist-get 'id h)))))
                                `((id . ,(alist-get 'id h)) (title . ,(alist-get 'title h))
                                  (file . ,(alist-get 'file h)) (node_type . ,(alist-get 'node_type h))
                                  (status . ,(alist-get 'status h))
                                  (similarity_score . ,(alist-get 'score h))
                                  (snippet . ,(alist-get 'snippet h))
                                  (excerpt . ,(if node (orsb-tools--snippet node 1500) (alist-get 'snippet h))))))
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
          (when text (orsb-legacy--data "update_body" `((id . ,id) (content . ,text) (section . ,section) (mode . "replace")
                                                        (create_section . t))))
          (let ((d (orsb-legacy--data "get_node" `((id . ,id) (include_body . :json-false)))))
            (orsb-legacy--ok `((node_id . ,(alist-get 'id d)) (file . ,(alist-get 'file d)) (properties . ,(alist-get 'properties d))))))))
    ("update_note" "update_body"
     ,(lambda (args)
        (let ((d (orsb-legacy--data "update_body" `((id . ,(orsb-legacy--id args 'identifier 'id)) (content . ,(orsb-arg args 'content))
                                                    (section . ,(orsb-arg args 'section)) (mode . ,(or (orsb-arg args 'mode) "append"))
                                                    (create_section . t) (force . ,(alist-get 'force args))))))
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
       (not (assoc name orsb-tools--full-extras))
       (not (assoc name orsb-tools--deprecated))
       t))

(defun orsb-tools--register-def (name fn desc required schema)
  "Register tool NAME running FN with DESC, REQUIRED args and SCHEMA."
  (org-roam-mcp-http--register-tool
   name desc
   (lambda (args) (orsb-tools--envelope fn args))
   (mapcar (lambda (p) (cons (car p) 'string)) schema)
   required
   schema))

(defun orsb-tools-register ()
  "Register the tools of `orsb-mcp-tool-profile'.
With `orsb-mcp-legacy-tools', the deprecated names are registered too."
  (clrhash org-roam-mcp-http--tools)
  (dolist (def (orsb-tools--active-definitions))
    (apply #'orsb-tools--register-def def))
  (when orsb-mcp-legacy-tools
    (dolist (def orsb-tools--deprecated)
      (pcase-let ((`(,name ,fn ,replacement ,required ,schema) def))
        (unless (gethash name org-roam-mcp-http--tools)
          (orsb-tools--register-def name fn (format "[deprecated → %s] Kept for compatibility; see MIGRATION.md." replacement)
                                    required schema))))
    (dolist (entry orsb-tools--legacy)
      (pcase-let ((`(,old ,new ,mapper) entry))
        ;; A name the current set also uses (add_daily_entry, log_to_inbox,
        ;; delete_node) keeps the current implementation, which accepts
        ;; the old arguments as well.
        (unless (gethash old org-roam-mcp-http--tools)
          (org-roam-mcp-http--register-tool
           old (format "[deprecated → %s] Old name kept for compatibility; see MIGRATION.md." new)
           (orsb-legacy--wrap mapper)
           nil nil nil)))))
  (message "orsb-tools: %d tools registered (%s profile, %s deprecated names)"
           (hash-table-count org-roam-mcp-http--tools)
           orsb-mcp-tool-profile
           (if orsb-mcp-legacy-tools "with" "without")))

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
