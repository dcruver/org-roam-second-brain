;;; orsb-tools.el --- The 2.0 MCP tool contract -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 3.1.2
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
;; and note types read from the vault (`orsb-schema').  Every write is
;; recorded as a change with an id that `restore' undoes, and most writes
;; take dry_run to preview.  Everything is implemented on `orsb-core'.
;;
;; There is one tool set, `orsb-tools--definitions' (nineteen tools).  The
;; pre-2.0 names, the 2.0 tools folded into others in 2.1 and the tool
;; profiles were removed in 2.2; MIGRATION.md maps each old name to its
;; replacement.  The whole graph, for a program that draws it, is the plain
;; HTTP endpoint GET /graph (`orsb-tools-graph'), not a tool.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-roam)
(require 'orsb-core)
(require 'orsb-schema)
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
(declare-function orsb-search-enqueue "orsb-search")
(declare-function org-roam-mcp-http--register-tool "org-roam-mcp-http")
(defvar org-roam-mcp-http--tools)

;;;; Customization

(defcustom orsb-stale-days 5
  "Days without a change after which an unfinished node counts as stale."
  :type 'integer
  :group 'orsb)

(defcustom orsb-mcp-instructions
  "This server keeps an org-roam second brain (plain-text notes that link to each other).
- Find before you create: search, then get_node. create_node refuses a title that already exists and lists it.
- Every write returns change_id; restore with it undoes the change. Writes take dry_run:true to preview.
- Errors carry a hint saying what to do next; follow it instead of retrying the same call.
- get_review shows what needs attention; get_tasks lists open tasks; set_checkbox and set_node close them.
- A note's type is the folder it is in. get_schema lists the types; get_schema with type shows what one requires: properties, sections, allowed status values.
- When you add text to a note, name the section. A section may only grow (append) or be rewritten whole (replace); the type's schema says which.
- If a write answers with warnings, it was made but broke a rule of the note's type: say which.
- If a write is refused because the change needs approval, repeat the same call with propose:true and a reason. That writes nothing: say it is waiting for approval, never that it was done.
- Approving is the owner's decision. Do not offer to approve a proposal yourself, and call decide_proposal only when the user names the proposal and tells you to approve or reject it.
- Before creating a note for something the user speaks of as existing (\"my X project\"), search for it by its short name; create it only when the user asks for something new."
  "Text sent to MCP clients at initialize, as guidance for the model.\nnil sends none."
  :type '(choice (const :tag "None" nil) string)
  :group 'orsb)

(defconst orsb-tools-version "3.1.2")

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

;;;; The vault's own rules (orsb-schema)

(defvar orsb-tools--warnings nil
  "Rules a write broke while the vault only warns; reported with the result.")

(defun orsb-tools--id-file (id)
  "Return the file of the note with ID, or nil."
  (when-let* ((node (org-roam-node-from-id id)))
    (org-roam-node-file node)))

(defun orsb-tools--guard (problems)
  "Apply PROBLEMS, a list of strings, each in the mode its note's type asks for.
A problem from a strict type refuses the write; one from a warning type is
recorded for the result; one from a type that switched its rules off is dropped."
  (let ((strict (seq-filter (lambda (p) (eq (orsb-schema-problem-mode p) 'strict)) problems))
        (warned (seq-filter (lambda (p) (eq (orsb-schema-problem-mode p) 'warn)) problems)))
    (when strict
      (orsb-error-hint 'schema-violation
                       "get_schema with type shows this note type's rules. A change that needs approval can be filed by repeating the call with propose:true."
                       "%s" (mapconcat #'substring-no-properties strict "; ")))
    (when warned
      (setq orsb-tools--warnings
            (append orsb-tools--warnings (mapcar #'substring-no-properties warned))))))

(defun orsb-tools--schema-on-p ()
  "Non-nil when the vault has schema files.
Whether a broken rule refuses a write, warns or is ignored is each type's
own choice (ENFORCE), applied by `orsb-tools--guard'."
  (orsb-schema-present-p))

(defun orsb-tools--top-section (node section)
  "Return the top-level heading a body edit of NODE lands in.
SECTION is the tool's section argument; a heading node edits its own
top-level heading."
  (or section
      (when (> (org-roam-node-level node) 0)
        (or (car (org-roam-node-olp node)) (org-roam-node-title node)))))

(defun orsb-tools--node-type (node props)
  "Return the type of NODE: the type of the folder it lives in.
PROPS is unused; a note's properties do not say what it is."
  (ignore props)
  (if (orsb-tools--schema-on-p)
      (or (orsb-schema-get (orsb-schema-for-file (org-roam-node-file node)) "TYPE") "note")
    "note"))

(defun orsb-tools--type-directory (type)
  "Return the folder notes of TYPE are created in, or signal.
The vault's schema files define the types.  A vault without any has one
type, \"note\", kept in its top folder."
  (cond ((not (orsb-schema-present-p))
         (if (equal type "note")
             (file-name-as-directory (expand-file-name org-roam-directory))
           (orsb-error-hint 'invalid-argument
                            "Note types come from _schema.org files in the vault; this vault has none."
                            "No such note type: %s (this vault has only \"note\")" type)))
        ((orsb-schema-type-directory type))
        (t (orsb-error-hint 'invalid-argument
                            "get_schema lists the note types this vault defines."
                            "No such note type: %s (this vault has %s)" type
                            (mapconcat (lambda (e) (alist-get 'type e)) (orsb-schema-tree) ", ")))))

(defun orsb-tools--lifecycle-key (file)
  "Return the property that holds the status of the note FILE."
  (or (and (orsb-tools--schema-on-p)
           (orsb-schema-get (orsb-schema-for-file file) "LIFECYCLE"))
      "STATUS"))

;;;; Records

(defun orsb-tools--relative (file)
  "FILE relative to `org-roam-directory'."
  (if (and file (string-prefix-p (file-truename org-roam-directory) (file-truename file)))
      (file-relative-name (file-truename file) (file-truename org-roam-directory))
    file))

(defun orsb-tools--days-since (mtime)
  "Whole days since MTIME."
  (if mtime (/ (- (float-time) (float-time mtime)) 86400.0) 0))

(defun orsb-tools--open-p (node status)
  "Whether NODE, with lifecycle value STATUS, is unfinished work.
True when its type declares a lifecycle and STATUS is not one of the values
that type calls finished.  A type without a lifecycle is never open."
  (when (orsb-tools--schema-on-p)
    (let ((schema (orsb-schema-for-file (org-roam-node-file node))))
      (and (orsb-schema-get schema "LIFECYCLE")
           (not (orsb-schema-finished-p schema status))))))

(defun orsb-tools--stale-p (node status days)
  "Whether NODE, with lifecycle value STATUS, untouched for DAYS, is stale."
  (and (>= days orsb-stale-days) (orsb-tools--open-p node status)))

(defun orsb-tools--light-record (node)
  "Cheap record built from the db only (no file read)."
  (let* ((props (org-roam-node-properties node))
         (status (cdr (assoc (orsb-tools--lifecycle-key (org-roam-node-file node)) props)))
         (days (orsb-tools--days-since (org-roam-node-file-mtime node))))
    `((id . ,(org-roam-node-id node))
      (title . ,(org-roam-node-title node))
      (file . ,(orsb-tools--relative (org-roam-node-file node)))
      (level . ,(org-roam-node-level node))
      (node_type . ,(orsb-tools--node-type node props))
      (status . ,status)
      (todo . ,(org-roam-node-todo node))
      (tags . ,(orsb--vector (org-roam-node-tags node)))
      (modified . ,(let ((m (org-roam-node-file-mtime node)))
                     (when m (format-time-string "%Y-%m-%dT%H:%M:%S%z" m))))
      (days_since_modified . ,(round days))
      (stale . ,(if (orsb-tools--stale-p node status days) t :json-false))
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
       (node_type . ,(alist-get 'node_type light))
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
               (push (list n nil nil) hits))))
         ;; Nothing contains the query as written.  A model often adds a word
         ;; ("Fence repair project" for the note "Fence repair") and, finding
         ;; nothing, makes a duplicate.  Fall back to the notes whose title
         ;; holds most of the query's words, best first.
         (unless hits
           (let* ((words (seq-uniq (split-string (downcase query) "[^[:alnum:]]+" t)))
                  (need (max 1 (ceiling (* 0.6 (length words)))))
                  scored)
             (dolist (row (org-roam-db-query [:select [id title] :from nodes :where (= level 0)]))
               (let* ((title-words (split-string (downcase (or (cadr row) "")) "[^[:alnum:]]+" t))
                      (found (seq-count (lambda (w) (member w title-words)) words)))
                 (when (>= found need) (push (cons found (car row)) scored))))
             (dolist (cell (sort scored (lambda (x y) (< (car x) (car y)))))
               (when-let ((n (org-roam-node-from-id (cdr cell))))
                 (push (list n (/ (float (car cell)) (length words)) '((match . "words"))) hits)))
             (setq hits (nreverse hits))))))
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
      (setq hits (seq-filter (lambda (h) (equal (orsb-tools--node-type (car h) (org-roam-node-properties (car h))) type)) hits)))
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

;;;; The whole graph

(defun orsb-tools-graph ()
  "Every node and every id link, for drawing the graph (GET /graph).
Nodes carry id, title, vault-relative file, level, pos (character
position in the file) and tags; links are [source dest] pairs.  Reads
the database only; no bodies."
  (let ((tags (make-hash-table :test 'equal)))
    (dolist (row (org-roam-db-query [:select [node-id tag] :from tags]))
      (when (and (stringp (nth 1 row)) (not (string-empty-p (nth 1 row))))
        (push (nth 1 row) (gethash (car row) tags))))
    `((nodes . ,(orsb--vector
                 (mapcar (lambda (row)
                           (let ((id (car row)))
                             `((id . ,id)
                               (title . ,(or (nth 4 row) ""))
                               (file . ,(orsb-tools--relative (nth 1 row)))
                               (level . ,(nth 2 row))
                               (pos . ,(nth 3 row))
                               (tags . ,(orsb--vector (nreverse (gethash id tags)))))))
                         (org-roam-db-query [:select [id file level pos title] :from nodes]))))
      (links . ,(orsb--vector
                 (mapcar (lambda (row) (vector (car row) (nth 1 row)))
                         (org-roam-db-query [:select :distinct [source dest] :from links
                                             :where (= type "id")])))))))

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
        (let ((s (downcase status)))
          (setq records (seq-filter (lambda (r) (equal (downcase (or (alist-get 'status (cdr r)) "")) s)) records))))
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
  "Shared filter/sort for list_nodes (first LIMIT)."
  (seq-take (orsb-tools--list-records args) (orsb-arg-int args 'limit 50)))

(defun orsb-tool-list-nodes (args)
  "list_nodes."
  (let ((p (orsb-tools--page (orsb-tools--list-records args) args 50)))
    (append (cdr p) `((nodes . ,(orsb--vector (car p)))))))

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
                            `((node_type . ,(orsb-tools--node-type n (org-roam-node-properties n)))
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

(defconst orsb-tools--create-reserved
  '(title node_type parent_id on_duplicate body status tags properties keywords dry_run propose reason
    section todo scheduled deadline)
  "Arguments of create_node that are not a note property.")

(defun orsb-tools--create-by-schema (args type title)
  "Create a TYPE note titled TITLE the way the vault's schema files say.
The folder, the default and required properties, the keyword lines and the
starting body all come from the type's merged schema.  Any other argument
in ARGS that names one of the type's properties (next_action for
NEXT-ACTION) sets that property."
  (let* ((dir (orsb-tools--type-directory type))
         (schema (if (orsb-schema-present-p) (orsb-schema-for-directory dir) nil))
         (known (mapcar (lambda (r) (upcase (car r))) (alist-get 'properties schema)))
         (lifecycle (or (orsb-schema-get schema "LIFECYCLE") "STATUS"))
         (props (append (orsb-schema-default-properties schema title)
                        (orsb-schema-server-properties schema))))
    (dolist (cell args)
      (let ((key (upcase (replace-regexp-in-string "_" "-" (symbol-name (car cell))))))
        (when (and (not (memq (car cell) orsb-tools--create-reserved))
                   (member key known)
                   (orsb-arg args (car cell)))
          (setf (alist-get key props nil nil #'equal) (format "%s" (orsb-arg args (car cell)))))))
    (when-let* ((st (orsb-arg args 'status)))
      (setf (alist-get lifecycle props nil nil #'equal) st))
    (dolist (cell (orsb-arg-props args 'properties))
      (setf (alist-get (upcase (car cell)) props nil nil #'equal) (format "%s" (cdr cell))))
    (when (orsb-tools--schema-on-p)
      (orsb-tools--guard (orsb-schema-create-problems type props #'orsb-tools--id-file)))
    (orsb-tools--change
     args "create_node" (format "create %s" title)
     (lambda ()
       (let ((node (orsb-core-create-node
                    title
                    :dir dir
                    ;; schema order first, then whatever else was given
                    :drawer (append (seq-filter #'identity (mapcar (lambda (k) (assoc k props)) known))
                                    (seq-remove (lambda (c) (member (car c) known)) (reverse props)))
                    ;; the type's keyword lines, then the caller's, which win
                    :keywords (let ((given (orsb-arg-props args 'keywords)))
                                (append (seq-remove (lambda (c) (assoc-string (car c) given t))
                                                    (orsb-schema-new-keywords schema title nil (orsb-arg-list args 'tags)))
                                        (mapcar (lambda (c) (cons (car c) (format "%s" (cdr c)))) given)))
                    :tags (orsb-arg-list args 'tags)
                    :body (orsb-schema-new-body schema title (orsb-arg args 'body)))))
         (append '((created . t)) (orsb-tools--full-record node nil)))))))

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
     ((not parent)
      (orsb-tools--create-by-schema args type title))
     (t
      (when (orsb-tools--schema-on-p)
        (orsb-tools--guard (orsb-schema-body-problems (org-roam-node-file parent)
                                                      ;; a heading added straight under a note is a new
                                                      ;; top-level section of its own
                                                      (or (orsb-tools--top-section parent (orsb-arg args 'section)) title)
                                                      "append")))
      (orsb-tools--change
       args "create_node" (format "create %s" title)
       (lambda ()
         (let ((node (orsb-core-insert-heading parent title
                                               :section (orsb-arg args 'section)
                                               :body (orsb-arg args 'body)
                                               :properties (append (orsb-arg-props args 'properties)
                                                                   (when-let* ((st (orsb-arg args 'status)))
                                                                     `(("STATUS" . ,st))))
                                               :todo (orsb-arg args 'todo)
                                               :tags (orsb-arg-list args 'tags)
                                               :scheduled (orsb-arg args 'scheduled)
                                               :deadline (orsb-arg args 'deadline))))
           (append '((created . t)) (orsb-tools--full-record node nil)))))))))

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
    (when (and (orsb-tools--schema-on-p) (not (orsb-arg args 'heading)))
      (let ((file (org-roam-node-file node)))
        (orsb-tools--guard
         (if (= (org-roam-node-level node) 0)
             (orsb-schema-property-problems
              file
              (append (when-let* ((st (orsb-arg args 'status)))
                        (list (cons (orsb-tools--lifecycle-key file) st)))
                      (orsb-arg-props args 'properties))
              #'orsb-tools--id-file)
           ;; a heading's properties are its own; only the type's write policy applies
           (orsb-schema-property-problems file nil #'orsb-tools--id-file)))))
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
           ;; the vault's schema names the property and its values; the guard above checked them
           (orsb-core-set-properties
            node `((,(if (= (org-roam-node-level node) 0) (orsb-tools--lifecycle-key (org-roam-node-file node)) "STATUS")
                    . ,status)))
           (push "status" changed))
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
    (when (orsb-tools--schema-on-p)
      (orsb-tools--guard (orsb-schema-body-problems (org-roam-node-file node)
                                                    (orsb-tools--top-section node section) mode)))
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
         (mention (when-let ((p (orsb-arg args 'mentions)))
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
                  (or (null mention)
                      (let ((text (plist-get tk :text)))
                        (if (org-roam-node-p mention)
                            (or (string-search (org-roam-node-id mention) text)
                                (string-search (downcase (org-roam-node-title mention)) (downcase text))
                                (equal (plist-get tk :file_id) (org-roam-node-id mention)))
                          (string-search (downcase mention) (downcase text)))))
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
    (when (orsb-tools--schema-on-p)
      (orsb-tools--guard
       (if (= level 0)
           (orsb-schema-operation-problems (org-roam-node-file node) "delete")
         (orsb-schema-body-problems (org-roam-node-file node) (orsb-tools--top-section node nil) "replace"))))
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
    (when (orsb-tools--schema-on-p)
      (orsb-tools--guard
       (append (if (= (org-roam-node-level node) 0)
                   (orsb-schema-operation-problems (org-roam-node-file node) "move")
                 (orsb-schema-body-problems (org-roam-node-file node) (orsb-tools--top-section node nil) "replace"))
               (when target
                 (orsb-schema-body-problems (org-roam-node-file target)
                                            (orsb-tools--top-section target (orsb-arg args 'section)) "append")))))
    (orsb-tools--change
     args "move_node" (format "move %s" (org-roam-node-title node))
     (lambda ()
       (if to-note
           (let ((path (orsb-core-promote-node node (orsb-tools--type-directory (or (orsb-arg args 'node_type) "note"))
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
    (when (orsb-tools--schema-on-p)
      (orsb-tools--guard
       (append (orsb-schema-operation-problems (org-roam-node-file source) "merge")
               (orsb-schema-body-problems (org-roam-node-file target)
                                          (orsb-tools--top-section target (orsb-arg args 'section)) "append"))))
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

(defun orsb-tool-log-to-inbox (args)
  "log_to_inbox {text, linked_id, category}."
  (orsb-require args 'text)
  (let* ((linked (when-let ((l (orsb-arg args 'linked_id))) (orsb-core-resolve l)))
         (category (orsb-arg args 'category))
         (text (if category (format "[%s] %s" category (orsb-arg args 'text)) (orsb-arg args 'text))))
    (orsb-tools--change
     args "log_to_inbox" (format "inbox: %s" (truncate-string-to-width text 60))
     (lambda ()
       (let* ((today (progn (orsb-core-log-to-inbox text linked) (format-time-string "%Y-%m-%d")))
              (entries (orsb-core--inbox-lines today)))
         `((logged . t)
           (ref . ,(plist-get (car (last entries)) :ref))
           (linked_id . ,(and linked (org-roam-node-id linked)))))))))

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

(defun orsb-tool-get-review (args)
  "get_review: what needs attention, each list capped, with totals."
  (let* ((n (orsb-arg-int args 'limit 10))
         (days (orsb-arg-int args 'days 7))
         (soon (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 3))))
         (today (format-time-string "%Y-%m-%d"))
         (files (seq-filter (lambda (nd) (= (org-roam-node-level nd) 0)) (org-roam-node-list)))
         (records (mapcar #'orsb-tools--light-record files))
         ;; unfinished notes of every type that has a lifecycle, newest first
         (open (seq-sort-by (lambda (r) (or (alist-get 'modified r) "")) #'string>
                            (seq-filter #'identity
                                        (cl-mapcar (lambda (nd r) (and (orsb-tools--open-p nd (alist-get 'status r)) r))
                                                   files records))))
         (stale (seq-filter (lambda (r) (eq (alist-get 'stale r) t)) open))
         (tasks (seq-filter (lambda (tk) (and (not (plist-get tk :done))
                                              (let ((d (or (plist-get tk :deadline) (plist-get tk :scheduled))))
                                                (and d (not (string> (substring d 0 10) soon))))))
                            (orsb-core-tasks)))
         (tasks (seq-sort-by (lambda (tk) (substring (or (plist-get tk :deadline) (plist-get tk :scheduled)) 0 10)) #'string< tasks))
         (inbox (orsb-core-inbox days))
         (health (orsb-core-health))
         (stale-emb (when (and (orsb-arg-bool args 'check_embeddings) (fboundp 'orsb-search-file-stale-p))
                      (seq-filter (lambda (f) (ignore-errors (orsb-search-file-stale-p f))) (org-roam-list-files)))))
    (cl-flet ((section (items fn)
                `((total . ,(length items)) (items . ,(orsb--vector (mapcar fn (seq-take items n))))))
              (short (r) (seq-filter #'cdr `((id . ,(alist-get 'id r)) (title . ,(alist-get 'title r))
                                              (node_type . ,(alist-get 'node_type r))
                                              (status . ,(alist-get 'status r))
                                              (next_action . ,(alist-get 'next_action r))
                                              (days_since_modified . ,(alist-get 'days_since_modified r))))))
      `((generated_at . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
        (open . ,(section open #'short))
        (stale . ,(section stale #'short))
        (due_tasks . ,(section tasks (lambda (tk) (append (orsb-tools--task-json tk)
                                                          (let ((d (or (plist-get tk :deadline) (plist-get tk :scheduled))))
                                                            (when (string< (substring d 0 10) today) '((overdue . t))))))))
        (inbox . ,(section inbox (lambda (e) `((ref . ,(plist-get e :ref)) (text . ,(plist-get e :text))))))
        (health
         . ((orphans . ,(section (plist-get health :orphans) (lambda (r) `((id . ,(nth 0 r)) (title . ,(nth 3 r)) (file . ,(orsb-tools--relative (nth 1 r)))))))
            (broken_links . ,(section (plist-get health :broken) (lambda (b) `((file . ,(orsb-tools--relative (car b))) (missing_id . ,(cdr b))))))
            (duplicate_titles . ,(section (plist-get health :duplicates) (lambda (d) `((title . ,(car d)) (ids . ,(orsb--vector (cdr d)))))))
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

(defun orsb-tools--type-json (entry)
  "ENTRY of `orsb-schema-tree' as a JSON object.
A nil value encodes as null: `json-encode' writes nil that way, and would
write the keyword :json-null as the string \"json-null\"."
  (copy-alist entry))

(defun orsb-tools--rows-json (rows names)
  "Schema table ROWS as a JSON array of objects keyed by NAMES."
  (orsb--vector
   (mapcar (lambda (row)
             (let ((i -1))
               (mapcar (lambda (name) (cons name (or (nth (cl-incf i) row) ""))) names)))
           rows)))

(defun orsb-tools--type-schema (type)
  "The merged rules of note TYPE, read from the vault's schema files."
  (let* ((dir (or (orsb-schema-type-directory type)
                  (orsb-error-hint 'not-found "get_schema without type lists the types this vault defines."
                                   "No such note type: %s" type)))
         (schema (orsb-schema-for-directory dir)))
    `((type . ,type)
      (directory . ,(alist-get 'directory schema))
      (is_a . ,(orsb--vector (alist-get 'types schema)))
      (settings . ,(orsb--object (mapcar (lambda (c) (cons (intern (car c)) (cdr c)))
                                         (alist-get 'type schema))))
      (properties . ,(orsb-tools--rows-json (alist-get 'properties schema) '(property required form)))
      (sections . ,(orsb-tools--rows-json (alist-get 'sections schema) '(section required rule)))
      (template . ,(alist-get 'template schema)))))

(defun orsb-tool-check-schema (args)
  "check_schema: where the notes depart from the vault's own schema files."
  (let* ((report (orsb-schema-check))
         (rule (orsb-arg args 'rule))
         (type (orsb-arg args 'type))
         (under (orsb-arg args 'path))
         (limit (max 1 (min 500 (orsb-arg-int args 'limit 50))))
         (offset (max 0 (orsb-arg-int args 'offset 0)))
         (all (seq-filter (lambda (v)
                            (and (or (not rule) (equal rule (alist-get 'rule v)))
                                 (or (not type) (equal type (alist-get 'type v)))
                                 (or (not under) (string-prefix-p under (alist-get 'file v)))))
                          (alist-get 'violations report)))
         (counts (lambda (alist) (orsb--object (mapcar (lambda (c) (cons (intern (car c)) (cdr c))) alist)))))
    `((schema_files . ,(if (orsb-schema-present-p) t :json-false))
      (notes . ,(alist-get 'notes report))
      (with_violations . ,(alist-get 'with_violations report))
      (by_rule . ,(funcall counts (alist-get 'by_rule report)))
      (by_type . ,(funcall counts (alist-get 'by_type report)))
      (matching . ,(length all))
      (offset . ,offset)
      (violations . ,(orsb--vector (seq-take (seq-drop all offset) limit))))))

(defun orsb-tools--file-id (file)
  "The file-level :ID: of FILE (the one before its first heading), or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((end (save-excursion (if (re-search-forward "^\\*+ " nil t) (line-beginning-position) (point-max)))))
      (when (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" end t)
        (match-string 1)))))

(defun orsb-tool-reassign-id (args)
  "reassign_id: give a note that shares its id with another note an id of its own."
  (orsb-require args 'file)
  (let* ((rel (orsb-arg args 'file))
         (root (expand-file-name org-roam-directory))
         (file (orsb-core--vault-file rel)))
    (unless (and (string-suffix-p ".org" file) (file-in-directory-p file root) (file-regular-p file))
      (orsb-error-hint 'not-found "check_schema with rule duplicate-id lists the files." "No note at %s" rel))
    (let* ((heading-id (orsb-arg args 'id))
           (id (or heading-id (orsb-tools--file-id file) (orsb-error 'refused "%s has no id of its own" rel)))
           (others (seq-filter (lambda (f) (and (not (file-equal-p f file))
                                                (member id (alist-get 'ids (orsb-schema--note-facts f)))))
                               (orsb-schema--note-files root)))
           (indexed (ignore-errors (org-roam-node-file (org-roam-node-from-id id)))))
      (unless (member id (alist-get 'ids (orsb-schema--note-facts file)))
        (orsb-error 'not-found "%s has no :ID: %s" rel id))
      (unless others
        (orsb-error 'refused "No other note has the id %s, so %s keeps it" id rel))
      (when (and indexed (file-equal-p indexed file) (not (orsb-arg-bool args 'force)))
        (orsb-error-hint 'refused
                         "Links and search use this copy. Reassign the other note instead, or pass force:true to move the id away from this one."
                         "%s is the note the index holds under %s; the hidden one is %s"
                         rel id (mapconcat (lambda (f) (file-relative-name f root)) others ", ")))
      (orsb-tools--change
       args "reassign_id" (format "new id for %s" rel)
       (lambda ()
         (let ((new (org-id-uuid)))
           (with-current-buffer (find-file-noselect file)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward (format "^[ \t]*:ID:[ \t]+\\(%s\\)[ \t]*$" (regexp-quote id)))
               (replace-match new t t nil 1)))
           (orsb-core--after-write file)
           `((file . ,rel) (old_id . ,id) (new_id . ,new)
             (shared_with . ,(orsb--vector (mapcar (lambda (f) (file-relative-name f root)) others))))))))))

(defun orsb-tool-get-schema (args)
  "get_schema: the vocabulary this server enforces, or with type one type's rules."
  (if-let* ((type (orsb-arg args 'type)))
      (orsb-tools--type-schema type)
    (orsb-tools--get-schema-1)))

(defun orsb-tools--get-schema-1 ()
  "The whole vocabulary: the types the vault defines, TODO keywords, tags in use."
  (let ((todo (orsb-core--todo-sets)))
    `((version . ,orsb-tools-version)
      (id_forms . ["org-roam id" "path relative to the vault, or absolute" "exact title or alias"
                   "\"today\", \"yesterday\" or YYYY-MM-DD for a daily note"])
      (todo_keywords . ((open . ,(orsb--vector (car todo))) (done . ,(orsb--vector (cdr todo)))))
      (tags_in_use . ,(orsb--vector
                       (mapcar #'car (seq-take (org-roam-db-query [:select [tag (funcall count tag)] :from tags
                                                                   :group-by tag :order-by [(desc (funcall count tag))]])
                                               50))))
      (stale_days . ,orsb-stale-days)
      (daily_directory . ,orsb-daily-directory)
      (inbox_heading . ,orsb-inbox-heading)
      (schema_files . ,(if (orsb-schema-present-p) t :json-false))
      (enforce . ,(symbol-name (orsb-schema-mode)))
      (types . ,(orsb--vector (mapcar #'orsb-tools--type-json (orsb-schema-tree))))
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

;;;; Proposals: a write, stored until someone approves it

(defvar orsb-tools--definitions)

(defvar orsb-tools--current-tool nil
  "Name of the tool being run, for filing a proposal.")

(defun orsb-tools--proposal-dir ()
  "The folder proposals are kept in, created on demand."
  (let ((dir (expand-file-name "proposals/" (orsb-core--change-dir))))
    (make-directory dir t)
    dir))

(defun orsb-tools--proposal-file (id)
  "The file of proposal ID."
  (unless (and (stringp id) (string-match-p "\\`[0-9A-Za-z-]+\\'" id))
    (orsb-error 'invalid-argument "Not a proposal id: %s" id))
  (expand-file-name (concat id ".json") (orsb-tools--proposal-dir)))

(defun orsb-tools--proposal-read (id)
  "Return proposal ID as an alist, or signal not-found."
  (let ((file (orsb-tools--proposal-file id)))
    (unless (file-readable-p file)
      (orsb-error-hint 'not-found "get_proposals lists them." "No such proposal: %s" id))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol) (json-false :json-false))
        (json-read-from-string (buffer-string))))))

(defun orsb-tools--proposal-write (proposal)
  "Store PROPOSAL, an alist with an id."
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file (orsb-tools--proposal-file (alist-get 'id proposal))
      (insert (json-encode proposal)))))

(defun orsb-tools--proposal-stale (proposal)
  "Return the files of PROPOSAL that changed since it was made, as a list."
  (let (out)
    (seq-doseq (f (alist-get 'files proposal))
      (let* ((rel (alist-get 'file f))
             (was (alist-get 'hash f))
             (was (if (member was '(nil :json-null :json-false "json-null")) nil was))
             (now (orsb-core--hash (orsb-core--disk-content (orsb-core--vault-file rel)))))
        (unless (equal was now) (push rel out))))
    (nreverse out)))

(defun orsb-tools--proposal-summary (proposal)
  "The short form of PROPOSAL for a list."
  (let ((stale (orsb-tools--proposal-stale proposal))
        (pending (equal (alist-get 'status proposal) "pending")))
    `((id . ,(alist-get 'id proposal))
      (status . ,(alist-get 'status proposal))
      (tool . ,(alist-get 'tool proposal))
      (reason . ,(alist-get 'reason proposal))
      ,@(when-let* ((b (alist-get 'batch proposal))) `((batch . ,b)))
      (created . ,(alist-get 'created proposal))
      (files . ,(orsb--vector (mapcar (lambda (f) (alist-get 'file f)) (append (alist-get 'files proposal) nil))))
      ,@(when pending `((stale . ,(if stale t :json-false)))))))

(defun orsb-tools--propose (tool fn args)
  "File the call of TOOL (function FN) with ARGS as a proposal; write nothing.
The call is run as a dry run with the \"needs approval\" rules lifted, so
its preview is exactly what approving it would do."
  (unless (seq-find (lambda (def) (and (equal (car def) tool) (assq 'dry_run (nth 4 def)))) orsb-tools--definitions)
    (orsb-error 'invalid-argument "%s changes nothing, so there is nothing to propose" tool))
  (let* ((call (seq-remove (lambda (c) (memq (car c) '(propose reason batch dry_run))) args))
         (orsb-schema-approved t)
         (data (funcall fn (append call '((dry_run . t)))))
         (preview (append (alist-get 'would_change data) nil))
         (id (format "%s-%04x" (format-time-string "%Y%m%dT%H%M%S") (random 65536)))
         (proposal
          `((id . ,id)
            (status . "pending")
            (tool . ,tool)
            (arguments . ,(orsb--object call))
            (reason . ,(orsb-arg args 'reason))
            ,@(when-let* ((b (orsb-arg args 'batch))) `((batch . ,b)))
            (created . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
            (files . ,(orsb--vector
                       (mapcar (lambda (entry)
                                 (let ((rel (alist-get 'file entry)))
                                   `((file . ,rel)
                                     (hash . ,(orsb-core--hash (orsb-core--disk-content (orsb-core--vault-file rel)))))))
                               preview)))
            (would_change . ,(orsb--vector preview)))))
    (unless preview
      (orsb-error 'invalid-argument "This call would change nothing, so there is nothing to propose"))
    (orsb-tools--proposal-write proposal)
    `((proposed . t) (proposal_id . ,id) (status . "pending")
      (would_change . ,(orsb--vector preview))
      (hint . "Nothing was written. decide_proposal with this id and decision \"approve\" applies it; \"reject\" drops it."))))

(defun orsb-tool-get-proposals (args)
  "get_proposals: the changes waiting for approval, or one of them in full."
  (if-let* ((id (orsb-arg args 'id)))
      (let ((p (orsb-tools--proposal-read id)))
        (append (orsb-tools--proposal-summary p)
                `((arguments . ,(orsb--object (alist-get 'arguments p)))
                  (would_change . ,(alist-get 'would_change p))
                  ,@(when-let* ((stale (and (equal (alist-get 'status p) "pending") (orsb-tools--proposal-stale p))))
                      `((changed_since . ,(orsb--vector stale))))
                  ,@(when-let* ((c (alist-get 'change_id p))) `((change_id . ,c)))
                  ,@(when-let* ((n (alist-get 'note p))) `((note . ,n)))
                  ,@(when-let* ((d (alist-get 'decided p))) `((decided . ,d))))))
    (let* ((want (or (orsb-arg args 'status) "pending"))
           (limit (max 1 (min 200 (orsb-arg-int args 'limit 50))))
           (all (mapcar (lambda (f) (orsb-tools--proposal-read (file-name-base f)))
                        (directory-files (orsb-tools--proposal-dir) nil "\\.json\\'")))
           (batch (orsb-arg args 'batch))
           (kept (seq-filter (lambda (p) (and (or (equal want "all") (equal (alist-get 'status p) want))
                                              (or (null batch) (equal (alist-get 'batch p) batch))))
                             all))
           ;; ids start with their timestamp: oldest first, the order to decide them in
           (kept (sort kept (lambda (a b) (string< (alist-get 'id a) (alist-get 'id b))))))
      `((status . ,want)
        (total . ,(length kept))
        (proposals . ,(orsb--vector (mapcar #'orsb-tools--proposal-summary (seq-take kept limit))))))))

(defun orsb-tool-decide-proposal (args)
  "decide_proposal: approve (apply) or reject one stored change, or a whole batch."
  (orsb-require args 'decision)
  (cond
   ((orsb-arg args 'id) (orsb-tools--decide-one args))
   ((orsb-arg args 'batch)
    ;; every pending proposal of the batch, oldest first; one refusal does not stop the rest
    (let* ((batch (orsb-arg args 'batch))
           (ids (sort (mapcar (lambda (p) (alist-get 'id p))
                              (seq-filter (lambda (p) (and (equal (alist-get 'status p) "pending")
                                                           (equal (alist-get 'batch p) batch)))
                                          (mapcar (lambda (f) (orsb-tools--proposal-read (file-name-base f)))
                                                  (directory-files (orsb-tools--proposal-dir) nil "\\.json\\'"))))
                      #'string<))
           (done 0) refused)
      (unless ids (orsb-error-hint 'not-found "get_proposals lists them." "No pending proposal in batch %s" batch))
      (dolist (id ids)
        (condition-case err
            (progn (orsb-tools--decide-one (cons (cons 'id id) (assq-delete-all 'batch (copy-alist args))))
                   (cl-incf done))
          (orsb-error (push `((id . ,id) (code . ,(string-replace "-" "_" (symbol-name (nth 1 err)))) (message . ,(nth 2 err))) refused))))
      `((batch . ,batch) (decision . ,(orsb-arg args 'decision)) (decided . ,done)
        (refused . ,(orsb--vector (nreverse refused))))))
   (t (orsb-error 'invalid-argument "Give id (one proposal) or batch (every pending proposal filed under that label)"))))

(defun orsb-tools--decide-one (args)
  "Approve or reject the proposal ARGS names by id."
  (let* ((p (orsb-tools--proposal-read (orsb-arg args 'id)))
         (decision (orsb-arg args 'decision))
         (def (seq-find (lambda (d) (equal (car d) (alist-get 'tool p))) orsb-tools--definitions))
         (stamp `((decided . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                  ,@(when-let* ((n (orsb-arg args 'note))) `((note . ,n))))))
    (unless (member decision '("approve" "reject"))
      (orsb-error 'invalid-argument "decision must be approve or reject"))
    (unless (equal (alist-get 'status p) "pending")
      (orsb-error 'refused "Proposal %s was already %s" (alist-get 'id p) (alist-get 'status p)))
    (if (equal decision "reject")
        (progn
          (orsb-tools--proposal-write (append `((status . "rejected")) stamp (assq-delete-all 'status (copy-alist p))))
          `((id . ,(alist-get 'id p)) (status . "rejected")))
      (when-let* ((stale (and (not (orsb-arg-bool args 'force)) (orsb-tools--proposal-stale p))))
        (orsb-error-hint 'conflict
                         "get_proposals with this id shows what it would do; force:true applies it to the note as it is now, or reject it and propose again."
                         "The note changed since this was proposed: %s" (string-join stale ", ")))
      (unless def (orsb-error 'internal "Proposal %s names an unknown tool: %s" (alist-get 'id p) (alist-get 'tool p)))
      (let* ((orsb-schema-approved t)
             (call (seq-remove (lambda (c) (memq (car c) '(propose reason dry_run)))
                               (append (alist-get 'arguments p) nil)))
             (data (funcall (nth 1 def) call)))
        (orsb-tools--proposal-write
         (append `((status . "approved") (change_id . ,(alist-get 'change_id data)))
                 stamp (assq-delete-all 'status (copy-alist p))))
        `((id . ,(alist-get 'id p)) (status . "approved")
          (change_id . ,(alist-get 'change_id data))
          (result . ,(orsb--object data)))))))

(defun orsb-tools--run (fn args)
  "Run FN on ARGS; return (:ok DATA) or (:error CODE MESSAGE HINT).
With propose in ARGS the call is filed as a proposal instead of run."
  (condition-case err
      (let ((clean (orsb-tools--clean-args args)))
        (list :ok (if (orsb-arg-bool clean 'propose)
                      (orsb-tools--propose orsb-tools--current-tool fn clean)
                    (funcall fn clean))))
    (orsb-error (list :error (nth 1 err) (nth 2 err) (nth 3 err)))
    (error (list :error 'internal (error-message-string err) nil))))

(defun orsb-tools--envelope (fn args &optional tool)
  "Run FN on ARGS and return the JSON envelope.  TOOL is the tool's name."
  (let* ((orsb-tools--warnings nil)
         (orsb-tools--current-tool tool)
         (r (orsb-tools--run fn args)))
    (if (eq (car r) :ok)
        (json-encode `((ok . t)
                       (data . ,(orsb--object
                                 (if (and orsb-tools--warnings (listp (nth 1 r)))
                                     (append (nth 1 r) `((warnings . ,(orsb--vector orsb-tools--warnings))))
                                   (nth 1 r))))))
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
     "Create a note of a type, or with parent_id a heading (task, section) inside a note. The type decides the folder, the starting sections and which properties are required (get_schema with type shows them). If a note with the same title exists nothing is created and possible_duplicates is returned; on_duplicate decides."
     ("title")
     ((title . ((type . "string")))
      (node_type . ((type . "string") (default . "note") (description . "A note type; get_schema lists the ones this vault defines")))
      (parent_id . ((type . "string") (description . "Create a heading inside this note instead of a new note")))
      (section . ((type . "string") (description . "With parent_id: nest under this heading")))
      (body . ((type . "string")))
      (todo . ((type . "string") (description . "With parent_id: TODO keyword, e.g. TODO")))
      (scheduled . ((type . "string") (description . "With parent_id: YYYY-MM-DD")))
      (deadline . ((type . "string") (description . "With parent_id: YYYY-MM-DD")))
      (status . ((type . "string") (description . "The type's lifecycle value, when it has one")))
      (tags . ,orsb-tools--string-list)
      (properties . ((type . "object") (additionalProperties . ((type . "string")))
                     (description . "The note's properties; get_schema with type lists the ones its type requires")))
      (keywords . ((type . "object") (additionalProperties . ((type . "string")))
                   (description . "Extra #+KEYWORD lines for the new note, beside the ones its type supplies")))
      (on_duplicate . ((type . "string") (enum . ("ask" "use_existing" "create")) (default . "ask")))
      ,orsb-tools--dry-run-schema))
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
     "Open tasks across all notes: TODO headings and unchecked checkboxes, dated ones first. Filter by state, kind, note (id), mentions, due_before or query."
     ()
     ((state . ((type . "string") (default . "open") (description . "open, done, all, or a keyword like WAITING")))
      (kind . ((type . "string") (enum . ("all" "todo" "checkbox")) (default . "all")))
      ,orsb-tools--id-schema
      (mentions . ((type . "string") (description . "Tasks that mention this name or link to this note id")))
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
     "Add one line to today's inbox for later filing, optionally linked to a note. Returns its ref for resolve_inbox."
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
     "What needs attention, in one call: open (unfinished notes of every type that has a lifecycle) and the stale ones among them, tasks due within 3 days, open inbox entries with refs, and vault health (orphans, broken links, duplicate titles)."
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
    ("reassign_id" orsb-tool-reassign-id
     "Give a note a new id when it shares its id with another note (check_schema rule duplicate-id). Only one note per id is visible to search and links; this makes the hidden one visible. Refuses the copy the index holds, since links point at it."
     ("file")
     ((file . ((type . "string") (description . "Vault path of the hidden note, as check_schema prints it")))
      (id . ((type . "string") (description . "When the shared id sits on a heading inside the note: that id. Omitted, the note's own id")))
      (force . ((type . "boolean") (default . :json-false) (description . "Reassign even the copy the index holds")))
      ,orsb-tools--dry-run-schema))
    ("get_schema" orsb-tool-get-schema
     "The vocabulary this server enforces: note types (read from the vault's _schema.org files), status values, TODO keywords, tags in use, id forms. With type: that type's required properties, sections, write policy and template. Call this instead of guessing."
     ()
     ((type . ((type . "string") (description . "A note type; returns its merged rules")))))
    ("get_proposals" orsb-tool-get-proposals
     "Changes filed for approval (any write tool called with propose:true). Lists the pending ones, oldest first, each with the files it touches and whether the note changed since. With id: one proposal in full, with its line-by-line preview."
     ()
     ((id . ((type . "string") (description . "One proposal, in full")))
      (status . ((type . "string") (enum . ("pending" "approved" "rejected" "all")) (default . "pending")))
      (batch . ((type . "string") (description . "Only proposals filed under this batch label")))
      (limit . ((type . "integer") (default . 50)))))
    ("decide_proposal" orsb-tool-decide-proposal
     "Approve or reject a proposal (id), or every pending proposal of a batch (batch). Approve applies the stored change and returns its change_id (restore undoes it). If the note changed since the proposal was made it is refused unless force:true; in a batch the others are still decided and the refused ones are listed."
     ("decision")
     ((id . ((type . "string") (description . "One proposal")))
      (batch . ((type . "string") (description . "Instead of id: every pending proposal filed under this batch label")))
      (decision . ((type . "string") (enum . ("approve" "reject"))))
      (note . ((type . "string") (description . "Kept with the decision")))
      (force . ((type . "boolean") (default . :json-false) (description . "Apply even though the note changed since")))))
    ("check_schema" orsb-tool-check-schema
     "Where the notes depart from the vault's own rules: a note in a folder that may not hold notes, an id shared by two notes (the index only sees one of them), a missing required property or section, a value outside its allowed set, a link to the wrong kind of note. Returns counts by rule and by type, and the violations (paged). Reads only; changes nothing."
     ()
     ((rule . ((type . "string") (description . "Only this rule: misplaced, untyped-folder, duplicate-id, missing-property, bad-value, missing-section")))
      (type . ((type . "string") (description . "Only notes of this type")))
      (path . ((type . "string") (description . "Only notes under this folder, e.g. projects/")))
      (limit . ((type . "integer") (default . 50)))
      (offset . ((type . "integer") (default . 0))))))
  "The tools: (NAME FUNCTION DESCRIPTION REQUIRED SCHEMA).")

(defun orsb-tools--register-def (name fn desc required schema)
  "Register tool NAME running FN with DESC, REQUIRED args and SCHEMA."
  (let ((schema (if (assq 'dry_run schema)
                    ;; every tool that writes can also be filed for approval
                    (append schema
                            '((propose . ((type . "boolean") (default . :json-false)
                                          (description . "File this change for approval instead of making it; returns proposal_id")))
                              (reason . ((type . "string") (description . "With propose: why, shown to whoever decides")))
                              (batch . ((type . "string") (description . "With propose: a label shared by proposals that can be decided together")))))
                  schema)))
    (org-roam-mcp-http--register-tool
     name desc
     (lambda (args) (orsb-tools--envelope fn args name))
     (mapcar (lambda (p) (cons (car p) 'string)) schema)
     required
     schema)))

(defun orsb-tools-register ()
  "Register the tools (`orsb-tools--definitions')."
  (clrhash org-roam-mcp-http--tools)
  (dolist (def orsb-tools--definitions)
    (apply #'orsb-tools--register-def def))
  (message "orsb-tools: %d tools registered" (hash-table-count org-roam-mcp-http--tools)))

(provide 'orsb-tools)
;;; orsb-tools.el ends here
