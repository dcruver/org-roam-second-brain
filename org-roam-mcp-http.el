;;; org-roam-mcp-http.el --- HTTP JSON-RPC server for org-roam MCP -*- lexical-binding: t; -*-

;; Author: Don Cruver / Nabu
;; Description: Pure elisp HTTP endpoint for org-roam MCP tools.
;;   Eliminates the Python -> shell -> emacsclient escaping pipeline.
;;   All my/api-* functions are called directly within Emacs.

;;; Commentary:
;; Start with (org-roam-mcp-http-start PORT)
;; Stop with (org-roam-mcp-http-stop)
;; Test: curl -s -X POST http://localhost:8007/ -H "Content-Type: application/json" \
;;         -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"search_notes","arguments":{"query":"test"}}}'

;;; Code:

(require 'json)
(require 'cl-lib)

(defvar org-roam-mcp-http--port 8007
  "Port for the org-roam MCP HTTP server.")

(defvar org-roam-mcp-http--server-proc nil
  "The httpd server process.")

;; ---------------------------------------------------------------------------
;; Tool registry: maps tool name -> (:fn FUNC :args-spec ((NAME . TYPE) ...))
;; ---------------------------------------------------------------------------

(defvar org-roam-mcp-http--tools (make-hash-table :test 'equal)
  "Hash table of registered MCP tools.")

(cl-defstruct (org-roam-mcp-tool (:constructor org-roam-mcp-tool-create))
  "An MCP tool registration."
  name description fn args-spec required-args schema)

(defun org-roam-mcp-http--register-tool (name description fn args-spec required-args schema)
  "Register a tool NAME with DESCRIPTION, FN, ARGS-SPEC, REQUIRED-ARGS, and SCHEMA."
  (puthash name (org-roam-mcp-tool-create
                 :name name
                 :description description
                 :fn fn
                 :args-spec args-spec
                 :required-args required-args
                 :schema schema)
           org-roam-mcp-http--tools))

;; ---------------------------------------------------------------------------
;; Tool definitions
;; ---------------------------------------------------------------------------

(defun org-roam-mcp-http--register-all-tools ()
  "Register all org-roam MCP tools."

  ;; --- Search ---
  (org-roam-mcp-http--register-tool
   "search_notes"
   "Search org-roam notes by text query."
   (lambda (args)
     (let ((query (alist-get 'query args)))
       (my/api-search-notes query)))
   '((query . string))
   '("query")
   '((query . ((type . "string") (description . "Search query")))))

  (org-roam-mcp-http--register-tool
   "semantic_search"
   "Semantic vector search using embeddings. Finds conceptually related notes."
   (lambda (args)
     (let ((query (alist-get 'query args))
           (limit (or (alist-get 'limit args) 10))
           (cutoff (or (alist-get 'cutoff args) 0.55)))
       (my/api-semantic-search query limit cutoff)))
   '((query . string) (limit . integer) (cutoff . number))
   '("query")
   '((query . ((type . "string") (description . "Search query")))
     (limit . ((type . "integer") (description . "Max results") (default . 10)))
     (cutoff . ((type . "number") (description . "Similarity threshold 0.0-1.0") (default . 0.55)))))

  (org-roam-mcp-http--register-tool
   "contextual_search"
   "Contextual search with enriched results."
   (lambda (args)
     (let ((query (alist-get 'query args))
           (limit (or (alist-get 'limit args) 10)))
       (my/api-contextual-search query limit)))
   '((query . string) (limit . integer))
   '("query")
   '((query . ((type . "string") (description . "Search query")))
     (limit . ((type . "integer") (description . "Max results") (default . 10)))))

  ;; --- Read/Write ---
  (org-roam-mcp-http--register-tool
   "read_note"
   "Read full content of a note by org-roam ID or path."
   (lambda (args)
     (let ((identifier (alist-get 'identifier args))
           (section (alist-get 'section args)))
       (if section
           (my/api-read-note identifier section)
         (my/api-read-note identifier))))
   '((identifier . string) (section . string))
   '("identifier")
   '((identifier . ((type . "string") (description . "Org-roam node ID or path")))
     (section . ((type . "string") (description . "Optional heading name to return only that section")))))

  (org-roam-mcp-http--register-tool
   "update_note"
   "Update content in a note by appending, prepending, or replacing. Can target specific sections."
   (lambda (args)
     (let ((identifier (alist-get 'identifier args))
           (content (alist-get 'content args))
           (section (alist-get 'section args))
           (mode (or (alist-get 'mode args) "append")))
       (if section
           (my/api-update-note identifier content section mode)
         (my/api-update-note identifier content nil mode))))
   '((identifier . string) (content . string) (section . string) (mode . string))
   '("identifier" "content")
   '((identifier . ((type . "string") (description . "Org-roam node ID or path")))
     (content . ((type . "string") (description . "Content to add to the note")))
     (section . ((type . "string") (description . "Optional heading to target (creates if not found)")))
     (mode . ((type . "string") (description . "append, prepend, or replace") (default . "append")))))

  (org-roam-mcp-http--register-tool
   "create_note"
   "Create a new org-roam note."
   (lambda (args)
     (let ((title (alist-get 'title args))
           (content (alist-get 'content args))
           (type (or (alist-get 'type args) "reference"))
           (confidence (or (alist-get 'confidence args) "medium")))
       (my/api-create-note title content type confidence)))
   '((title . string) (content . string) (type . string) (confidence . string))
   '("title" "content")
   '((title . ((type . "string") (description . "Title for the new note")))
     (content . ((type . "string") (description . "Content for the new note")))
     (type . ((type . "string") (description . "Note type (reference, video, concept)") (default . "reference")))
     (confidence . ((type . "string") (description . "Confidence level (high, medium, low)") (default . "medium")))))

  ;; --- Task Management ---
  (org-roam-mcp-http--register-tool
   "change_task_state"
   "Change the TODO state of a task. Triggers org-mode hooks for proper logging."
   (lambda (args)
     (let ((file (alist-get 'file args))
           (heading (alist-get 'heading args))
           (new_state (alist-get 'new_state args)))
       (condition-case err
           (progn
             (my/org-roam-change-task-state file heading new_state)
             (json-encode '((success . t) (message . "Task state changed"))))
         (error (json-encode `((success . :json-false) (error . ,(error-message-string err))))))))
   '((file . string) (heading . string) (new_state . string))
   '("file" "heading" "new_state")
   '((file . ((type . "string") (description . "Path to the org file")))
     (heading . ((type . "string") (description . "Heading text to find")))
     (new_state . ((type . "string") (description . "New TODO state (TODO, DONE, etc.)")))))

  (org-roam-mcp-http--register-tool
   "list_notes"
   "List org-roam notes with optional filters by type and status."
   (lambda (args)
     (let ((node-type (alist-get 'node_type args))
           (status (alist-get 'status args))
           (limit (or (alist-get 'limit args) 50))
           (sort-by (or (alist-get 'sort_by args) "modified")))
       (my/api-list-notes node-type status limit sort-by)))
   '((node_type . string) (status . string) (limit . integer) (sort_by . string))
   '()
   '((node_type . ((type . "string") (description . "Filter by type: project, person, idea, admin, blog")))
     (status . ((type . "string") (description . "Filter by status: active, stale, done, cancelled")))
     (limit . ((type . "integer") (description . "Max results") (default . 50)))
     (sort_by . ((type . "string") (description . "Sort: modified, created, title") (default . "modified")))))

  (org-roam-mcp-http--register-tool
   "get_note_properties"
   "Get properties of a note by ID or path."
   (lambda (args)
     (let ((identifier (alist-get 'identifier args)))
       (my/api-get-note-properties identifier)))
   '((identifier . string))
   '("identifier")
   '((identifier . ((type . "string") (description . "Org-roam node ID or path")))))

  ;; --- Structured Creation ---
  (org-roam-mcp-http--register-tool
   "create_project"
   "Create a project node. Always include substantive context in the notes field — an empty project with just a title is useless. Capture the why, what was discussed, key details, and next steps."
   (lambda (args)
     (let ((title (alist-get 'title args))
           (status (or (alist-get 'status args) "active"))
           (next-action (alist-get 'next_action args))
           (notes (alist-get 'notes args)))
       (my/api-create-project title status next-action notes)))
   '((title . string) (status . string) (next_action . string) (notes . string))
   '("title" "notes")
   '((title . ((type . "string") (description . "Project name")))
     (status . ((type . "string") (description . "Status: active, waiting, blocked, someday, done") (default . "active")))
     (next_action . ((type . "string") (description . "Next actionable step")))
     (notes . ((type . "string") (description . "Main content and context — conversations, research, decisions, rationale. Always include relevant context.")))))

  (org-roam-mcp-http--register-tool
   "create_person"
   "Create a person node for tracking interactions and follow-ups."
   (lambda (args)
     (let ((name (alist-get 'name args))
           (context (alist-get 'context args))
           (follow-ups (alist-get 'follow_ups args))
           (notes (alist-get 'notes args)))
       (my/api-create-person name context follow-ups notes)))
   '((name . string) (context . string) (follow_ups . string) (notes . string))
   '("name")
   '((name . ((type . "string") (description . "Person's name")))
     (context . ((type . "string") (description . "How you know them / relationship context")))
     (follow_ups . ((type . "string") (description . "Pending follow-up items")))
     (notes . ((type . "string") (description . "Additional notes")))))

  (org-roam-mcp-http--register-tool
   "create_idea"
   "Create an idea node. Always include a meaningful one_liner and elaboration with full context."
   (lambda (args)
     (let ((title (alist-get 'title args))
           (one-liner (alist-get 'one_liner args))
           (elaboration (alist-get 'elaboration args)))
       (my/api-create-idea title one-liner elaboration)))
   '((title . string) (one_liner . string) (elaboration . string))
   '("title" "one_liner")
   '((title . ((type . "string") (description . "Idea title")))
     (one_liner . ((type . "string") (description . "Brief summary of the insight")))
     (elaboration . ((type . "string") (description . "Detailed explanation and reasoning")))))

  (org-roam-mcp-http--register-tool
   "create_admin"
   "Create an admin task node for tracking tasks with due dates."
   (lambda (args)
     (let ((title (alist-get 'title args))
           (due-date (alist-get 'due_date args))
           (notes (alist-get 'notes args)))
       (my/api-create-admin title due-date notes)))
   '((title . string) (due_date . string) (notes . string))
   '("title")
   '((title . ((type . "string") (description . "Task title")))
     (due_date . ((type . "string") (description . "Due date YYYY-MM-DD")))
     (notes . ((type . "string") (description . "Additional notes")))))

  ;; --- Daily/Inbox ---
  (org-roam-mcp-http--register-tool
   "add_daily_entry"
   "Add structured entry to daily note. Points should contain substantive content — the details, context, and reasoning worth remembering."
   (lambda (args)
     (let* ((timestamp (or (alist-get 'timestamp args) (format-time-string "%H:%M")))
            (title (alist-get 'title args))
            (points (or (alist-get 'points args) []))
            (next-steps (or (alist-get 'next_steps args) []))
            (tags (or (alist-get 'tags args) []))
            ;; Convert JSON arrays (vectors) to lists
            (points-list (if (vectorp points) (append points nil) points))
            (steps-list (if (vectorp next-steps) (append next-steps nil) next-steps))
            (tags-list (if (vectorp tags) (append tags nil) tags)))
       (condition-case err
           (progn
             (my/add-daily-entry-structured timestamp title points-list steps-list tags-list)
             (json-encode `((success . t) (message . ,(format "Added journal entry: %s" title)))))
         (error (json-encode `((success . :json-false) (error . ,(error-message-string err))))))))
   '((timestamp . string) (title . string) (points . array) (next_steps . array) (tags . array))
   '("title" "points")
   '((timestamp . ((type . "string") (description . "HH:MM format")))
     (title . ((type . "string") (description . "Entry title")))
     (points . ((type . "array") (description . "Main content points") (items . ((type . "string")))))
     (next_steps . ((type . "array") (description . "Action items") (items . ((type . "string")))))
     (tags . ((type . "array") (description . "Tags") (items . ((type . "string")))))))

  (org-roam-mcp-http--register-tool
   "get_daily_content"
   "Get content of daily note for a given date."
   (lambda (args)
     (let ((date (alist-get 'date args)))
       (condition-case err
           (let ((content (if date
                              (my/get-daily-note-content date)
                            (my/get-daily-note-content))))
             (json-encode `((success . t) (content . ,content))))
         (error (json-encode `((success . :json-false) (error . ,(error-message-string err))))))))
   '((date . string))
   '()
   '((date . ((type . "string") (description . "Date in YYYY-MM-DD format (defaults to today)")))))

  (org-roam-mcp-http--register-tool
   "add_inbox_entry"
   "Add an entry to the org-roam inbox."
   (lambda (args)
     (let ((command (alist-get 'command args))
           (original-text (alist-get 'original_text args))
           (linked-note-id (alist-get 'linked_note_id args))
           (linked-note-title (alist-get 'linked_note_title args)))
       (my/api-add-inbox-entry command original-text linked-note-id linked-note-title)))
   '((command . string) (original_text . string) (linked_note_id . string) (linked_note_title . string))
   '("command" "original_text")
   '((command . ((type . "string") (description . "Inbox command/category")))
     (original_text . ((type . "string") (description . "Content to add")))
     (linked_note_id . ((type . "string") (description . "Optional linked note ID")))
     (linked_note_title . ((type . "string") (description . "Optional linked note title")))))

  (org-roam-mcp-http--register-tool
   "log_to_inbox"
   "Quick log to inbox."
   (lambda (args)
     (let ((text (alist-get 'text args)))
       (my/api-log-to-inbox text)))
   '((text . string))
   '("text")
   '((text . ((type . "string") (description . "Text to log")))))

  ;; --- Surfacing ---
  (org-roam-mcp-http--register-tool
   "get_active_projects"
   "Get all active projects with their next actions, ordered by last modified."
   (lambda (_args) (my/api-get-active-projects))
   '() '() '())

  (org-roam-mcp-http--register-tool
   "get_stale_projects"
   "Get projects with no activity in X days."
   (lambda (args)
     (let ((days (or (alist-get 'days_threshold args) 5)))
       (my/api-get-stale-projects days)))
   '((days_threshold . integer))
   '()
   '((days_threshold . ((type . "integer") (description . "Days of inactivity threshold") (default . 5)))))

  (org-roam-mcp-http--register-tool
   "get_pending_followups"
   "Get all people with pending follow-up items."
   (lambda (_args) (my/api-get-pending-followups))
   '() '() '())

  (org-roam-mcp-http--register-tool
   "get_dangling_followups"
   "Get follow-up items that reference people without person notes."
   (lambda (_args) (my/api-get-dangling-followups))
   '() '() '())

  (org-roam-mcp-http--register-tool
   "get_weekly_inbox"
   "Get inbox entries from the last N days."
   (lambda (args)
     (let ((days (or (alist-get 'days args) 7)))
       (my/api-get-weekly-inbox days)))
   '((days . integer))
   '()
   '((days . ((type . "integer") (description . "Number of days to look back") (default . 7)))))

  (org-roam-mcp-http--register-tool
   "get_digest_data"
   "Get comprehensive digest data for daily review."
   (lambda (_args) (my/api-get-digest-data))
   '() '() '())

  ;; --- Maintenance ---
  (org-roam-mcp-http--register-tool
   "delete_note"
   "Delete or archive a note."
   (lambda (args)
     (let ((identifier (alist-get 'identifier args))
           (archive (alist-get 'archive args)))
       (my/api-delete-note identifier archive)))
   '((identifier . string) (archive . boolean))
   '("identifier")
   '((identifier . ((type . "string") (description . "Org-roam node ID or path")))
     (archive . ((type . "boolean") (description . "If true, move to trash instead of deleting")))))

  (org-roam-mcp-http--register-tool
   "rename_note"
   "Rename a note's title."
   (lambda (args)
     (let ((identifier (alist-get 'identifier args))
           (new-title (alist-get 'new_title args)))
       (my/api-rename-note identifier new-title)))
   '((identifier . string) (new_title . string))
   '("identifier" "new_title")
   '((identifier . ((type . "string") (description . "Org-roam node ID or path")))
     (new_title . ((type . "string") (description . "New title for the note")))))

  (org-roam-mcp-http--register-tool
   "manage_tags"
   "Add or remove tags on a note."
   (lambda (args)
     (let ((identifier (alist-get 'identifier args))
           (action (alist-get 'action args))
           (tag (alist-get 'tag args)))
       (my/api-manage-tags identifier action tag)))
   '((identifier . string) (action . string) (tag . string))
   '("identifier" "action" "tag")
   '((identifier . ((type . "string") (description . "Org-roam node ID or path")))
     (action . ((type . "string") (description . "add or remove")))
     (tag . ((type . "string") (description . "Tag name")))))

  (org-roam-mcp-http--register-tool
   "add_link"
   "Add a link from one note to another."
   (lambda (args)
     (let ((from-id (alist-get 'from_id args))
           (to-id (alist-get 'to_id args))
           (section (alist-get 'section args)))
       (my/api-add-link from-id to-id section)))
   '((from_id . string) (to_id . string) (section . string))
   '("from_id" "to_id")
   '((from_id . ((type . "string") (description . "Source note ID")))
     (to_id . ((type . "string") (description . "Target note ID")))
     (section . ((type . "string") (description . "Section in source note to add link")))))

  ;; --- Embeddings ---
  (org-roam-mcp-http--register-tool
   "generate_note_embedding"
   "Generate embedding for a single note."
   (lambda (args)
     (let ((file-path (alist-get 'file_path args)))
       (condition-case err
           (progn
             (my/api--generate-and-save-embedding file-path)
             (json-encode `((success . t) (message . ,(format "Embedding generated for %s" file-path)))))
         (error (json-encode `((success . :json-false) (error . ,(error-message-string err))))))))
   '((file_path . string))
   '("file_path")
   '((file_path . ((type . "string") (description . "Absolute path to the org file")))))

  (org-roam-mcp-http--register-tool
   "generate_embeddings"
   "Generate embeddings for all notes (batch operation). WARNING: slow, processes all 1500+ notes."
   (lambda (_args)
     (condition-case err
         (progn
           (org-roam-semantic-generate-embeddings)
           (json-encode '((success . t) (message . "Batch embedding generation started"))))
       (error (json-encode `((success . :json-false) (error . ,(error-message-string err)))))))
   '() '() '())

  (org-roam-mcp-http--register-tool
   "sync_database"
   "Sync the org-roam database."
   (lambda (_args)
     (condition-case err
         (progn
           (org-roam-db-sync)
           (json-encode '((success . t) (message . "Database synced"))))
       (error (json-encode `((success . :json-false) (error . ,(error-message-string err)))))))
   '() '() '())

  (message "org-roam-mcp-http: registered %d tools" (hash-table-count org-roam-mcp-http--tools)))

;; ---------------------------------------------------------------------------
;; JSON-RPC dispatch
;; ---------------------------------------------------------------------------

(defun org-roam-mcp-http--handle-tools-call (params)
  "Handle a tools/call JSON-RPC method with PARAMS."
  (let* ((tool-name (alist-get 'name params))
         (arguments (or (alist-get 'arguments params) '()))
         (tool (gethash tool-name org-roam-mcp-http--tools)))
    (if (not tool)
        (json-encode `((error . ((code . -32601)
                                 (message . ,(format "Unknown tool: %s" tool-name))))))
      ;; Check required args
      (let ((missing (cl-remove-if
                      (lambda (req) (alist-get (intern req) arguments))
                      (org-roam-mcp-tool-required-args tool))))
        (if missing
            (json-encode `((error . ((code . -32602)
                                     (message . ,(format "Missing required arguments: %s"
                                                         (string-join missing ", ")))))))
          ;; Call the tool function
          (condition-case err
              (let ((result (funcall (org-roam-mcp-tool-fn tool) arguments)))
                ;; result is a JSON string from my/api-* functions
                ;; We need to parse it and re-encode in the response envelope
                (let ((parsed (condition-case nil
                                  (json-read-from-string result)
                                (error result))))
                  (json-encode `((result . ((content . [((type . "text")
                                                         (text . ,result))])
                                            ,@(when (and (listp parsed)
                                                         (alist-get 'note parsed))
                                                `((note . ,(alist-get 'note parsed))))))))))
            (error
             (json-encode `((error . ((code . -32603)
                                      (message . ,(format "Tool error: %s" (error-message-string err))))))))))))))

(defun org-roam-mcp-http--handle-tools-list (_params)
  "Handle a tools/list JSON-RPC method. Return all registered tool schemas."
  (let ((tools-list '()))
    (maphash
     (lambda (name tool)
       (let ((properties '())
             (schema (org-roam-mcp-tool-schema tool)))
         ;; Build properties from schema
         (dolist (prop schema)
           (push (cons (car prop) (cdr prop)) properties))
         (push `((name . ,name)
                 (description . ,(org-roam-mcp-tool-description tool))
                 (inputSchema . ((type . "object")
                                 (properties . ,(nreverse properties))
                                 (required . ,(vconcat (org-roam-mcp-tool-required-args tool))))))
               tools-list)))
     org-roam-mcp-http--tools)
    (json-encode `((result . ((tools . ,(vconcat (nreverse tools-list)))))))))

(defun org-roam-mcp-http--dispatch (body)
  "Dispatch a JSON-RPC request from BODY string. Return JSON response string."
  (condition-case err
      (let* ((request (json-read-from-string body))
             (id (alist-get 'id request))
             (method (alist-get 'method request))
             (params (or (alist-get 'params request) '()))
             (response
              (cond
               ((equal method "tools/call")
                (org-roam-mcp-http--handle-tools-call params))
               ((equal method "tools/list")
                (org-roam-mcp-http--handle-tools-list params))
               (t
                (json-encode `((error . ((code . -32601)
                                         (message . ,(format "Unknown method: %s" method))))))))))
        ;; Wrap with jsonrpc and id
        (let* ((parsed-response (json-read-from-string response))
               (final `((jsonrpc . "2.0")
                        (id . ,id)
                        ,@(if (alist-get 'error parsed-response)
                              `((error . ,(alist-get 'error parsed-response)))
                            `((result . ,(alist-get 'result parsed-response)))))))
          (json-encode final)))
    (error
     (json-encode `((jsonrpc . "2.0")
                    (id . :null)
                    (error . ((code . -32700)
                              (message . ,(format "Parse error: %s" (error-message-string err))))))))))

;; ---------------------------------------------------------------------------
;; HTTP handler using simple-httpd
;; ---------------------------------------------------------------------------

(defvar org-roam-mcp-http--server nil
  "The TCP server process.")

(defun org-roam-mcp-http--make-http-response (status content-type body)
  "Create an HTTP response string with STATUS, CONTENT-TYPE, and BODY."
  (format "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nAccess-Control-Allow-Origin: *\r\nAccess-Control-Allow-Methods: POST, GET, OPTIONS\r\nAccess-Control-Allow-Headers: Content-Type\r\nConnection: close\r\n\r\n%s"
          status content-type (string-bytes body) body))

(defun org-roam-mcp-http--handle-connection (proc data)
  "Handle incoming HTTP connection on PROC with DATA.
Accumulates data across multiple filter calls to handle chunked delivery."
  (let ((buf (or (process-get proc :buffer) "")))
    (setq buf (concat buf data))
    (process-put proc :buffer buf)
    ;; Find header/body boundary (handle both \r\n\r\n and \n\n)
    (let ((header-end (or (string-match "\r\n\r\n" buf)
                          (string-match "\n\n" buf))))
      (when header-end
        (let* ((separator (if (string-match "\r\n\r\n" buf) 4 2))
               (headers (substring buf 0 header-end))
               (body-start (+ header-end separator))
               (body (substring buf body-start))
               (cl-val (when (string-match "[Cc]ontent-[Ll]ength: *\\([0-9]+\\)" headers)
                         (string-to-number (match-string 1 headers))))
               (body-complete (or (null cl-val) (>= (length body) cl-val)))
               (first-nl (or (string-match "\r?\n" headers) (length headers)))
               (method-line (substring headers 0 first-nl)))
          ;; Wait for full body before processing
          (when body-complete
            (when cl-val
              (setq body (substring body 0 (min (length body) cl-val))))
            (cond
             ((string-prefix-p "OPTIONS" method-line)
              (process-send-string proc
                (org-roam-mcp-http--make-http-response "204 No Content" "text/plain" ""))
              (delete-process proc))
             ((string-prefix-p "GET" method-line)
              (process-send-string proc
                (org-roam-mcp-http--make-http-response "200 OK" "text/plain" "OK"))
              (delete-process proc))
             ((string-prefix-p "POST" method-line)
              ;; Defer tool execution via timer so the event loop stays free
              ;; for any synchronous HTTP calls (e.g., embedding generation)
              (let ((saved-proc proc)
                    (saved-body body))
                (run-at-time 0 nil
                  (lambda ()
                    (let ((response (condition-case err
                                        (org-roam-mcp-http--dispatch saved-body)
                                      (error (json-encode
                                              `((jsonrpc . "2.0") (id . :null)
                                                (error . ((message . ,(error-message-string err))))))))))
                      (when (process-live-p saved-proc)
                        (process-send-string saved-proc
                          (org-roam-mcp-http--make-http-response "200 OK" "application/json" response))
                        (delete-process saved-proc)))))))
             (t
              (process-send-string proc
                (org-roam-mcp-http--make-http-response "405 Method Not Allowed" "text/plain" ""))
              (delete-process proc)))))))))

(defun org-roam-mcp-http--sentinel (proc event)
  "Handle PROC connection EVENT."
  (when (string-match "\\(closed\\|connection broken\\|deleted\\)" event)
    (process-put proc :buffer nil)))

;;;###autoload
(defun org-roam-mcp-http-start (&optional port)
  "Start the org-roam MCP HTTP server on PORT (default 8007)."
  (interactive)
  (let ((port (or port org-roam-mcp-http--port)))
    ;; Stop existing server if running
    (when org-roam-mcp-http--server
      (org-roam-mcp-http-stop))
    ;; Register tools
    (org-roam-mcp-http--register-all-tools)
    ;; Start TCP server
    (setq org-roam-mcp-http--server
          (make-network-process
           :name "org-roam-mcp-http"
           :server t
           :host "0.0.0.0"
           :service port
           :family 'ipv4
           :filter #'org-roam-mcp-http--handle-connection
           :sentinel #'org-roam-mcp-http--sentinel
           :coding 'utf-8))
    (message "org-roam-mcp-http: started on port %d with %d tools"
             port (hash-table-count org-roam-mcp-http--tools))))

;;;###autoload
(defun org-roam-mcp-http-stop ()
  "Stop the org-roam MCP HTTP server."
  (interactive)
  (when org-roam-mcp-http--server
    (delete-process org-roam-mcp-http--server)
    (setq org-roam-mcp-http--server nil)
    (message "org-roam-mcp-http: stopped")))

(provide 'org-roam-mcp-http)
;;; org-roam-mcp-http.el ends here
