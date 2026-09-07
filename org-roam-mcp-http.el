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
(require 'org-id)
;; The tool implementations. Previously left to the user's init file, which
;; made every tool fail with a void-function error when it was forgotten.
(require 'org-roam-api)
;; The 2.0 tool contract, registered on top of the legacy tools.
(require 'orsb-tools)
;; Doom-only module (after!/setq!); only loadable where those macros exist.
;; change_task_state reports a void-function error if it is absent.
(when (fboundp 'after!)
  (require 'org-agenda-project-tracking nil t))

;; The port lives in `orsb-mcp-port' (transport section below);
;; `org-roam-mcp-http--port' is kept as an alias for old init files.

(defvar org-roam-mcp-http--session-id nil
  "Session id of the request being handled (request-scoped, dynamically bound).
Set by `initialize' to the new session so the response carries it.")

(defvar org-roam-mcp-http--protocol-version "2025-03-26"
  "MCP protocol version this server implements.")

(defvar org-roam-mcp-http--server-info
  '((name . "org-roam-mcp") (version . "1.0.0"))
  "Server info returned in MCP initialize response.")

(defun org-roam-mcp-http--generate-session-id ()
  "Generate a new MCP session ID."
  (org-id-uuid))

;; ---------------------------------------------------------------------------
;; Tool registry: maps tool name -> (:fn FUNC :args-spec ((NAME . TYPE) ...))
;; ---------------------------------------------------------------------------

(defvar org-roam-mcp-http--tools (make-hash-table :test 'equal)
  "Hash table of registered MCP tools.")

(cl-defstruct (org-roam-mcp-tool (:constructor org-roam-mcp-tool-create))
  "An MCP tool registration."
  name description fn args-spec required-args schema)

(defun org-roam-mcp-http--set-tool-description (tool description)
  "Replace TOOL's DESCRIPTION (used by orsb-tools to mark deprecations)."
  (setf (org-roam-mcp-tool-description tool) description))

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
   "Search org-roam notes by text query (title match only, not body content)."
   (lambda (args)
     (let ((query (alist-get 'query args))
           (node-type (alist-get 'node_type args)))
       (my/api-search-notes query node-type)))
   '((query . string) (node_type . string))
   '("query")
   '((query . ((type . "string") (description . "Search query")))
     (node_type . ((type . "string") (description . "Optional: restrict to one type — project, admin, person, blog, reference, idea, telos, daily, howto. See org-roam-schema note (id 1777502556) for what each means.")))))

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

  ;; --- Note operations (file-level) ---
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
   "create_note"
   "Create a new org-roam note file. Returns note_id for use with add_node."
   (lambda (args)
     (let ((title (alist-get 'title args))
           (properties (alist-get 'properties args)))
       (my/api-create-note title properties)))
   '((title . string) (properties . object))
   '("title")
   '((title . ((type . "string") (description . "Title for the new note")))
     (properties . ((type . "object")
                    (description . "Optional #+KEYWORD: value pairs for the file header")
                    (additionalProperties . ((type . "string")))))))

  ;; --- Node operations (heading-level) ---
  (org-roam-mcp-http--register-tool
   "read_node"
   "Read a node by its org-roam ID. Returns text (body) and properties. For file-level nodes (level 0), properties are #+KEYWORD: pairs. For heading-level nodes, properties are :PROPERTIES: drawer entries."
   (lambda (args)
     (let ((node-id (alist-get 'node_id args)))
       (my/api-read-node node-id)))
   '((node_id . string))
   '("node_id")
   '((node_id . ((type . "string") (description . "Org-roam node ID")))))

  (org-roam-mcp-http--register-tool
   "add_node"
   "Add a new heading-level node to an existing note. The node receives a generated org-roam :ID: in its :PROPERTIES: drawer."
   (lambda (args)
     (let ((note-id (alist-get 'note_id args))
           (heading (alist-get 'heading args))
           (text (or (alist-get 'text args) ""))
           (properties (alist-get 'properties args))
           (level (or (alist-get 'level args) 1)))
       (my/api-add-node note-id heading text properties level)))
   '((note_id . string) (heading . string) (text . string) (properties . object) (level . integer))
   '("note_id" "heading")
   '((note_id . ((type . "string") (description . "Org-roam ID of the parent note")))
     (heading . ((type . "string") (description . "Heading title text (after the stars)")))
     (text . ((type . "string") (description . "Body content for the node")))
     (properties . ((type . "object")
                    (description . "Drawer key-value pairs stored in :PROPERTIES: (excluding :ID:, which is auto-generated)")
                    (additionalProperties . ((type . "string")))))
     (level . ((type . "integer") (description . "Heading level (default 1)") (default . 1)))))

  (org-roam-mcp-http--register-tool
   "update_node"
   "Update a node's body text and/or properties by its org-roam ID. Omit text or properties to leave them unchanged. properties writes the node's :PROPERTIES: drawer (file-level drawer for a file node, heading drawer for a heading node); keys are upper-cased, e.g. {\"STATUS\":\"done\"}. With section, content replaces just that heading's body (update_note replace semantics)."
   (lambda (args)
     (let ((node-id (alist-get 'node_id args))
           (text (or (alist-get 'text args) (alist-get 'content args)))
           (section (alist-get 'section args))
           (properties (alist-get 'properties args)))
       (if (and section text)
           ;; blog_revise's shape: replace one section's body, keep the rest.
           (progn
             (when properties (my/api-update-node node-id nil properties))
             (my/api-update-note node-id text section "replace" nil))
         (my/api-update-node node-id text properties))))
   '((node_id . string) (text . string) (content . string) (section . string) (properties . object))
   '("node_id")
   '((node_id . ((type . "string") (description . "Org-roam node ID")))
     (text . ((type . "string") (description . "New body content; omit to leave unchanged")))
     (content . ((type . "string") (description . "Alias of text")))
     (section . ((type . "string") (description . "Heading whose body text/content replaces; omit to replace the whole node body")))
     (properties . ((type . "object")
                    (description . "Key-value pairs to update; file-level: #+KEYWORD: lines; heading-level: :PROPERTIES: drawer")
                    (additionalProperties . ((type . "string")))))))

  (org-roam-mcp-http--register-tool
   "delete_node"
   "Delete a heading-level node and its subtree by org-roam ID. Cannot delete file-level nodes; use delete_note for that."
   (lambda (args)
     (let ((node-id (alist-get 'node_id args)))
       (my/api-delete-node node-id)))
   '((node_id . string))
   '("node_id")
   '((node_id . ((type . "string") (description . "Org-roam node ID to delete")))))

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
   '((node_type . ((type . "string") (description . "Filter by type: project, admin, person, blog, reference, idea, telos, daily, howto. See org-roam-schema note (id 1777502556) for what each means.")))
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

  ;; --- Blog (used by the homelab agent's blog_draft / blog_drafts tools) ---
  (org-roam-mcp-http--register-tool
   "create_blog_post"
   "Create a blog post note (node type blog, ox-hugo header, hugo_draft true) in SECTION with BODY as the draft text. Returns id, file and slug."
   (lambda (args)
     (let ((title (alist-get 'title args))
           (section (alist-get 'section args))
           (body (or (alist-get 'body args) ""))
           (tags (or (alist-get 'tags args) "")))
       (condition-case err
           (let* ((res (sb/core-create-blog title section))
                  (file (plist-get res :file)))
             (with-current-buffer (find-file-noselect file)
               (goto-char (point-min))
               (when (and (stringp tags) (> (length tags) 0)
                          (re-search-forward "^#\\+hugo_tags: *$" nil t))
                 (replace-match (format "#+hugo_tags: %s" tags)))
               (goto-char (point-min))
               (if (re-search-forward "^\\* Draft\\s-*$" nil t)
                   (progn (end-of-line) (insert "\n\n" body "\n"))
                 (goto-char (point-max))
                 (insert "\n* Draft\n\n" body "\n"))
               (save-buffer)
               (kill-buffer (current-buffer)))
             (org-roam-db-sync)
             (json-encode `((success . t) (id . ,(plist-get res :id)) (file . ,file)
                            (title . ,title) (slug . ,(plist-get res :slug)) (section . ,section))))
         (error (json-encode `((success . :json-false) (error . ,(error-message-string err))))))))
   '((title . string) (section . string) (body . string) (tags . string))
   '("title" "section" "body")
   '((title . ((type . "string") (description . "Post title")))
     (section . ((type . "string") (description . "Hugo section: signalscope, health-tracking, homelab, gpu-ai, second-brain, cyberdeck, side-projects, writing")))
     (body . ((type . "string") (description . "Org-mode body of the draft (headings as * / **)")))
     (tags . ((type . "string") (description . "Optional comma-separated hugo tags")))))

  (org-roam-mcp-http--register-tool
   "blog_status"
   "Blog overview: drafts with outline progress, recently published posts, and idea notes that could become posts."
   (lambda (_args)
     (condition-case err
         (let* ((d (sb/core-blog-digest-data))
                (draft->alist
                 (lambda (p)
                   (let ((pr (plist-get p :progress)))
                     `((id . ,(plist-get p :id)) (title . ,(plist-get p :title))
                       (file . ,(plist-get p :file))
                       (days_since_modified . ,(plist-get p :days-since-modified))
                       (progress . ,(if (consp pr) (format "%s/%s" (car pr) (cdr pr)) pr))
                       (has_content . ,(if (plist-get p :has-content) t :json-false))))))
                (post->alist
                 (lambda (p) `((id . ,(plist-get p :id)) (title . ,(plist-get p :title))
                               (days_since_modified . ,(plist-get p :days-since-modified)))))
                (idea->alist
                 (lambda (n) (cond ((and (recordp n) (eq (aref n 0) 'org-roam-node))
                                    `((id . ,(org-roam-node-id n)) (title . ,(org-roam-node-title n))))
                                   ((listp n)
                                    `((id . ,(plist-get n :id)) (title . ,(plist-get n :title))))
                                   (t `((title . ,(format "%s" n))))))))
           (json-encode `((success . t)
                          (drafts . ,(vconcat (mapcar draft->alist (plist-get (plist-get d :drafts) :items))))
                          (published_recent . ,(vconcat (mapcar post->alist (plist-get (plist-get d :published) :recent))))
                          (ideas . ,(vconcat (mapcar idea->alist (plist-get (plist-get d :ideas-for-blog) :items)))))))
       (error (json-encode `((success . :json-false) (error . ,(error-message-string err)))))))
   '() '() '())

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
   "update_note"
   "Update content in a note by appending, prepending, or replacing. Can target a section by heading. A whole-file replace is guarded: it is refused if it would drop existing top-level headings or drastically shrink the note (pass force:true to override), so prefer append/prepend or section-targeted edits. Use update_node to change properties."
   (lambda (args)
     (let ((identifier (alist-get 'identifier args))
           (content (alist-get 'content args))
           (section (alist-get 'section args))
           (mode (or (alist-get 'mode args) "append"))
           (force (let ((f (alist-get 'force args))) (and f (not (eq f :json-false))))))
       (my/api-update-note identifier content section mode force)))
   '((identifier . string) (content . string) (section . string) (mode . string) (force . boolean))
   '("identifier" "content")
   '((identifier . ((type . "string") (description . "Org-roam node ID or path relative to the vault")))
     (content . ((type . "string") (description . "Content to add or the replacement text")))
     (section . ((type . "string") (description . "Optional heading to target (created if not found)")))
     (mode . ((type . "string") (enum . ("append" "prepend" "replace")) (description . "append, prepend, or replace") (default . "append")))
     (force . ((type . "boolean") (description . "Override the destructive whole-file replace guard. Leave unset; only use for a deliberate full rewrite.") (default . :json-false)))))

  (org-roam-mcp-http--register-tool
   "generate_embeddings"
   "Generate embeddings for all notes (batch operation). WARNING: slow, processes all 1500+ notes."
   (lambda (_args)
     (condition-case err
         (progn
           (org-roam-semantic-generate-all-embeddings)
           (json-encode '((success . t) (message . "Batch embedding generation finished"))))
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

  (message "org-roam-mcp-http: registered %d legacy tools" (hash-table-count org-roam-mcp-http--tools))
  ;; The 2.0 contract (orsb-tools.el) registers on top and marks or removes
  ;; the legacy names according to `orsb-mcp-legacy-tools'.
  (orsb-tools-register))

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
          ;; Call the tool function, abandoning it after orsb-mcp-tool-timeout
          ;; seconds (effective only where the tool yields to the event loop).
          (condition-case err
              (let* ((started (float-time))
                     (result (with-timeout (orsb-mcp-tool-timeout :timeout)
                               (funcall (org-roam-mcp-tool-fn tool) arguments))))
                (when (eq result :timeout)
                  (orsb-mcp--log "tool %s timed out after %ss" tool-name orsb-mcp-tool-timeout)
                  (setq result
                        (if (gethash tool-name orsb-tools--legacy-fns)
                            ;; legacy shape
                            (json-encode `((success . :json-false)
                                           (error . ,(format "Tool %s timed out after %s seconds" tool-name orsb-mcp-tool-timeout))))
                          (json-encode `((ok . :json-false)
                                         (error . ((code . "timeout")
                                                   (message . ,(format "Tool %s timed out after %s seconds" tool-name orsb-mcp-tool-timeout))
                                                   (hint . "The server is busy or the operation is slow; try again or narrow the request."))))))))
                (orsb-mcp--log "tool %s %.0f ms%s" tool-name (* 1000 (- (float-time) started))
                               (if (string-prefix-p "{\"ok\":false" result) " (error)" ""))
                ;; result is a JSON string. 2.0 tools return the
                ;; {"ok":...} envelope and get MCP isError on failure;
                ;; legacy my/api-* results are passed through unchanged
                ;; (with the historical `note' hoist).
                (let* ((is-error (string-prefix-p "{\"ok\":false" result))
                       (parsed (condition-case nil
                                   (json-read-from-string result)
                                 (error result))))
                  (json-encode `((result . ((content . [((type . "text")
                                                         (text . ,result))])
                                            ,@(when is-error '((isError . t)))
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
                                 ;; Empty alist json-encodes as null; use a hash-table
                                 ;; so zero-arg tools emit "properties": {} (MCP spec).
                                 (properties . ,(if properties
                                                    (nreverse properties)
                                                  (make-hash-table)))
                                 (required . ,(vconcat (org-roam-mcp-tool-required-args tool))))))
               tools-list)))
     org-roam-mcp-http--tools)
    (json-encode `((result . ((tools . ,(vconcat (nreverse tools-list)))))))))

;; ---------------------------------------------------------------------------
;; MCP protocol handlers
;; ---------------------------------------------------------------------------

(defgroup orsb-mcp nil
  "org-roam-second-brain MCP server transport."
  :group 'orsb
  :prefix "orsb-mcp-")

(defcustom orsb-mcp-port 8007
  "TCP port the MCP server listens on."
  :type 'integer :group 'orsb-mcp)
(defvaralias 'org-roam-mcp-http--port 'orsb-mcp-port)

(defcustom orsb-mcp-host "127.0.0.1"
  "Address to bind.  \"0.0.0.0\" exposes the server to the network;
combine that with `orsb-mcp-auth-token' unless the network is trusted."
  :type 'string :group 'orsb-mcp)

(defcustom orsb-mcp-auth-token nil
  "When non-nil, POST and DELETE must carry `Authorization: Bearer <token>'."
  :type '(choice (const nil) string) :group 'orsb-mcp)

(defcustom orsb-mcp-tool-timeout 30
  "Seconds a single tool call may run before it is abandoned with an error.
Only effective where the tool yields to the event loop (waiting on a
process or network); pure CPU work is not interrupted."
  :type 'number :group 'orsb-mcp)

(defcustom orsb-mcp-session-max-idle (* 24 60 60)
  "Seconds of inactivity after which a session is forgotten."
  :type 'integer :group 'orsb-mcp)

(defcustom orsb-mcp-session-max 64
  "Most sessions kept at once; the least recently used is dropped first."
  :type 'integer :group 'orsb-mcp)

(defcustom orsb-mcp-log-buffer "*orsb-mcp-log*"
  "Buffer that receives one line per request (nil disables logging)."
  :type '(choice (const nil) string) :group 'orsb-mcp)

(defcustom orsb-mcp-log-max-lines 2000
  "Trim the log buffer to this many lines."
  :type 'integer :group 'orsb-mcp)

(defconst org-roam-mcp-http--supported-protocols '("2025-03-26" "2025-06-18" "2025-11-25")
  "Protocol versions we can speak; the client's choice is echoed when listed.")

;; ---------------------------------------------------------------------------
;; Logging
;; ---------------------------------------------------------------------------

(defun orsb-mcp--log (format-string &rest args)
  "Append a timestamped line to `orsb-mcp-log-buffer'."
  (when orsb-mcp-log-buffer
    (with-current-buffer (get-buffer-create orsb-mcp-log-buffer)
      (goto-char (point-max))
      (insert (format-time-string "%Y-%m-%d %H:%M:%S ") (apply #'format format-string args) "\n")
      (when (> (count-lines (point-min) (point-max)) orsb-mcp-log-max-lines)
        (goto-char (point-min))
        (forward-line (/ orsb-mcp-log-max-lines 4))
        (delete-region (point-min) (point))))))

;; ---------------------------------------------------------------------------
;; Sessions: one entry per client, expiring, capped
;; ---------------------------------------------------------------------------

(defvar orsb-mcp--sessions (make-hash-table :test 'equal)
  "Session id -> plist (:created :last-seen :client).")

(defun orsb-mcp--sessions-expire ()
  "Forget idle sessions and, if still over the cap, the least recently used."
  (let ((now (float-time)) (stale nil))
    (maphash (lambda (id s)
               (when (> (- now (plist-get s :last-seen)) orsb-mcp-session-max-idle)
                 (push id stale)))
             orsb-mcp--sessions)
    (dolist (id stale) (remhash id orsb-mcp--sessions))
    (while (> (hash-table-count orsb-mcp--sessions) orsb-mcp-session-max)
      (let (oldest oldest-seen)
        (maphash (lambda (id s)
                   (when (or (null oldest-seen) (< (plist-get s :last-seen) oldest-seen))
                     (setq oldest id oldest-seen (plist-get s :last-seen))))
                 orsb-mcp--sessions)
        (remhash oldest orsb-mcp--sessions)))))

(defun orsb-mcp--session-create (client)
  "Register a new session for CLIENT (a name string) and return its id."
  (let ((id (org-roam-mcp-http--generate-session-id))
        (now (float-time)))
    (puthash id (list :created now :last-seen now :client client) orsb-mcp--sessions)
    ;; expire after inserting so the cap counts the newcomer
    (orsb-mcp--sessions-expire)
    id))

(defun orsb-mcp--session-touch (id)
  "Mark session ID as used; return non-nil when it is known."
  (when-let ((s (and id (gethash id orsb-mcp--sessions))))
    (plist-put s :last-seen (float-time))
    t))

(defun orsb-mcp--session-delete (id)
  "Forget session ID; return non-nil when it existed."
  (when (and id (gethash id orsb-mcp--sessions))
    (remhash id orsb-mcp--sessions)
    t))

(defun org-roam-mcp-http-sessions ()
  "Return the live sessions as a list of (ID . CLIENT), newest first."
  (let (out)
    (maphash (lambda (id s) (push (cons id (plist-get s :client)) out)) orsb-mcp--sessions)
    (sort out (lambda (a b) (> (plist-get (gethash (car a) orsb-mcp--sessions) :created)
                                (plist-get (gethash (car b) orsb-mcp--sessions) :created))))))

;; ---------------------------------------------------------------------------
;; MCP protocol handlers
;; ---------------------------------------------------------------------------

(defun org-roam-mcp-http--handle-initialize (params)
  "Handle MCP initialize: open a session and return capabilities.
The new id is left in `org-roam-mcp-http--session-id' (request-scoped)
so the HTTP layer puts it in the Mcp-Session-Id response header."
  (let* ((client (or (alist-get 'name (alist-get 'clientInfo params)) "unknown"))
         (wanted (alist-get 'protocolVersion params))
         (version (if (member wanted org-roam-mcp-http--supported-protocols)
                      wanted org-roam-mcp-http--protocol-version)))
    (setq org-roam-mcp-http--session-id (orsb-mcp--session-create client))
    (orsb-mcp--log "initialize client=%s protocol=%s session=%s (%d live)"
                   client version org-roam-mcp-http--session-id (hash-table-count orsb-mcp--sessions))
    (json-encode
     `((result . ((protocolVersion . ,version)
                  (capabilities . ((tools . ((listChanged . :json-false)))))
                  (serverInfo . ,org-roam-mcp-http--server-info)))))))

(defun org-roam-mcp-http--handle-ping (_params)
  "Handle MCP ping request."
  (json-encode '((result . ()))))

(defun org-roam-mcp-http--dispatch (body)
  "Dispatch a JSON-RPC request from BODY string.
Return JSON response string, or symbol `notification' for fire-and-forget messages."
  (condition-case err
      (let* ((request (json-read-from-string body))
             (id (alist-get 'id request))
             (method (alist-get 'method request))
             (params (or (alist-get 'params request) '())))
        ;; Notifications (no id) get no JSON-RPC response
        (if (and (null id) (stringp method) (string-prefix-p "notifications/" method))
            'notification
          (let ((response
                 (cond
                  ((equal method "initialize")
                   (org-roam-mcp-http--handle-initialize params))
                  ((equal method "ping")
                   (org-roam-mcp-http--handle-ping params))
                  ((equal method "tools/call")
                   (org-roam-mcp-http--handle-tools-call params))
                  ((equal method "tools/list")
                   (org-roam-mcp-http--handle-tools-list params))
                  (t
                   (json-encode `((error . ((code . -32601)
                                            (message . ,(format "Unknown method: %s" method))))))))))
            ;; Inject jsonrpc + id after the handler's opening brace via
            ;; string concatenation. Round-tripping through json-read-from-string
            ;; collapses every nested empty object ({}) into nil/null, which
            ;; strict MCP clients reject (e.g. zero-arg tools' inputSchema.properties).
            (concat "{\"jsonrpc\":\"2.0\",\"id\":"
                    (json-encode id)
                    ","
                    (substring response 1)))))
    (error
     (json-encode `((jsonrpc . "2.0")
                    (id . :null)
                    (error . ((code . -32700)
                              (message . ,(format "Parse error: %s" (error-message-string err))))))))))

;; ---------------------------------------------------------------------------
;; HTTP: parsing and handling are pure functions; the process filter only
;; accumulates bytes and defers POST work to a timer.
;; ---------------------------------------------------------------------------

(defvar org-roam-mcp-http--server nil
  "The TCP server process.")

(defun org-roam-mcp-http--make-http-response (status content-type body &optional extra-headers)
  "Create an HTTP response string with STATUS, CONTENT-TYPE, BODY and EXTRA-HEADERS.
The request-scoped `org-roam-mcp-http--session-id', when set, is echoed
in Mcp-Session-Id."
  (let ((session-header (if org-roam-mcp-http--session-id
                            (format "Mcp-Session-Id: %s\r\n" org-roam-mcp-http--session-id)
                          "")))
    (format "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nAccess-Control-Allow-Origin: *\r\nAccess-Control-Allow-Methods: POST, DELETE, OPTIONS\r\nAccess-Control-Allow-Headers: Content-Type, Accept, Authorization, Mcp-Session-Id\r\n%s%sConnection: close\r\n\r\n%s"
            status content-type (string-bytes body) session-header (or extra-headers "") body)))

(defun org-roam-mcp-http--header (headers name)
  "Return the value of HTTP header NAME (case-insensitive) in HEADERS, or nil."
  (let ((case-fold-search t))
    (when (string-match (concat "^" (regexp-quote name) ": *\\([^\r\n]*\\)") headers)
      (string-trim (match-string 1 headers)))))

(defun org-roam-mcp-http--parse-request (buf)
  "Parse the accumulated request bytes BUF.
Return nil while the request is incomplete, else a plist
\(:method :path :headers :body).  The header/body separator is found once
and its length taken from that match, so a \\r\\n\\r\\n inside the body can
no longer confuse a request whose headers ended with \\n\\n."
  (when (string-match "\r\n\r\n\\|\n\n" buf)
    (let* ((header-end (match-beginning 0))
           (body-start (match-end 0))
           (headers (substring buf 0 header-end))
           (body (substring buf body-start))
           (cl (org-roam-mcp-http--header headers "Content-Length"))
           (cl-val (and cl (string-to-number cl))))
      (when (or (null cl-val) (>= (string-bytes body) cl-val))
        (when (and cl-val (> (string-bytes body) cl-val))
          (let ((encoded (encode-coding-string body 'utf-8)))
            (setq body (decode-coding-string (substring encoded 0 cl-val) 'utf-8))))
        (let* ((request-line (car (split-string headers "\r?\n")))
               (parts (split-string request-line " " t)))
          (list :method (upcase (or (car parts) ""))
                :path (or (cadr parts) "/")
                :headers headers
                :body body))))))

(defun org-roam-mcp-http--authorized-p (headers)
  "Whether HEADERS satisfy `orsb-mcp-auth-token' (always when it is nil)."
  (or (null orsb-mcp-auth-token)
      (equal (org-roam-mcp-http--header headers "Authorization")
             (concat "Bearer " orsb-mcp-auth-token))))

(defun org-roam-mcp-http--handle-request (req)
  "Produce the full HTTP response string for parsed request REQ.
Pure with respect to the network: safe to call from tests."
  (let* ((method (plist-get req :method))
         (headers (plist-get req :headers))
         (client-session (org-roam-mcp-http--header headers "Mcp-Session-Id"))
         ;; request-scoped: what the response header will carry
         (org-roam-mcp-http--session-id client-session))
    (cond
     ((equal method "OPTIONS")
      (org-roam-mcp-http--make-http-response "204 No Content" "text/plain" ""))
     ((equal method "GET")
      ;; MCP Streamable HTTP: GET opens an SSE stream, which we do not offer.
      (org-roam-mcp-http--make-http-response "405 Method Not Allowed" "text/plain" ""))
     ((not (org-roam-mcp-http--authorized-p headers))
      (orsb-mcp--log "%s rejected: bad or missing bearer token" method)
      (org-roam-mcp-http--make-http-response "401 Unauthorized" "text/plain" ""
                                             "WWW-Authenticate: Bearer\r\n"))
     ((equal method "DELETE")
      (if (orsb-mcp--session-delete client-session)
          (progn (orsb-mcp--log "session %s terminated by client" client-session)
                 (setq org-roam-mcp-http--session-id nil)
                 (org-roam-mcp-http--make-http-response "200 OK" "text/plain" ""))
        (org-roam-mcp-http--make-http-response "404 Not Found" "text/plain" "")))
     ((equal method "POST")
      (if (and client-session (not (orsb-mcp--session-touch client-session)))
          (progn (orsb-mcp--log "POST rejected: unknown session %s" client-session)
                 (org-roam-mcp-http--make-http-response "404 Not Found" "text/plain" ""))
        (let ((result (condition-case err
                          (org-roam-mcp-http--dispatch (plist-get req :body))
                        (error (json-encode
                                `((jsonrpc . "2.0") (id . :null)
                                  (error . ((code . -32603)
                                            (message . ,(error-message-string err))))))))))
          (if (eq result 'notification)
              (org-roam-mcp-http--make-http-response "202 Accepted" "text/plain" "")
            (org-roam-mcp-http--make-http-response "200 OK" "application/json" result)))))
     (t (org-roam-mcp-http--make-http-response "405 Method Not Allowed" "text/plain" "")))))

(defun org-roam-mcp-http--respond (proc response)
  "Send RESPONSE on PROC if the client is still connected, then close."
  (if (process-live-p proc)
      (progn (process-send-string proc response)
             (delete-process proc))
    (orsb-mcp--log "client went away before the response could be sent")))

(defun org-roam-mcp-http--handle-connection (proc data)
  "Accumulate DATA for PROC; once a request is complete, answer it.
POST work runs from a zero-delay timer so the filter returns at once."
  (let ((buf (concat (or (process-get proc :buffer) "") data)))
    (process-put proc :buffer buf)
    (when-let ((req (org-roam-mcp-http--parse-request buf)))
      (process-put proc :buffer nil)
      (if (equal (plist-get req :method) "POST")
          (run-at-time 0 nil
                       (lambda ()
                         (let ((started (float-time)))
                           (org-roam-mcp-http--respond proc (org-roam-mcp-http--handle-request req))
                           (orsb-mcp--log "POST %s %.0f ms" (plist-get req :path)
                                          (* 1000 (- (float-time) started))))))
        (org-roam-mcp-http--respond proc (org-roam-mcp-http--handle-request req))))))

(defun org-roam-mcp-http--sentinel (proc event)
  "Handle PROC connection EVENT."
  (when (string-match "\\(closed\\|connection broken\\|deleted\\)" event)
    (process-put proc :buffer nil)))

;;;###autoload
(defun org-roam-mcp-http-start (&optional port host)
  "Start the org-roam MCP HTTP server on PORT (default `orsb-mcp-port')
bound to HOST (default `orsb-mcp-host')."
  (interactive)
  (let ((port (or port orsb-mcp-port))
        (host (or host orsb-mcp-host)))
    (when org-roam-mcp-http--server
      (org-roam-mcp-http-stop))
    (org-roam-mcp-http--register-all-tools)
    (setq org-roam-mcp-http--server
          (make-network-process
           :name "org-roam-mcp-http"
           :server t
           :host host
           :service port
           :family 'ipv4
           :filter #'org-roam-mcp-http--handle-connection
           :sentinel #'org-roam-mcp-http--sentinel
           :coding 'utf-8))
    (orsb-mcp--log "server started on %s:%d with %d tools%s" host port
                   (hash-table-count org-roam-mcp-http--tools)
                   (if orsb-mcp-auth-token " (bearer auth on)" ""))
    (message "org-roam-mcp-http: started on %s:%d with %d tools"
             host port (hash-table-count org-roam-mcp-http--tools))))

;;;###autoload
(defun org-roam-mcp-http-stop ()
  "Stop the org-roam MCP HTTP server.  Sessions survive a restart."
  (interactive)
  (when org-roam-mcp-http--server
    (delete-process org-roam-mcp-http--server)
    (setq org-roam-mcp-http--server nil)
    (orsb-mcp--log "server stopped")
    (message "org-roam-mcp-http: stopped")))

(provide 'org-roam-mcp-http)
;;; org-roam-mcp-http.el ends here
