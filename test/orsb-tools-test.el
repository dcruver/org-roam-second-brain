;;; orsb-tools-test.el --- ERT tests for the 2.0 MCP contract -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives tools through `org-roam-mcp-http--handle-tools-call', i.e. the same
;; path an MCP client takes, and checks the {ok,data|error} envelope, the
;; isError flag and the shared `id' parameter.

;;; Code:

(require 'ert)
(require 'org-roam)
(require 'orsb-core)
(require 'orsb-tools)
(require 'org-roam-mcp-http)
(require 'orsb-core-test)   ; fixtures: orsb-test-with-vault, orsb-test--file, orsb-test--json

(defun orsb-tools-test--ensure-registered ()
  "Register the default tool set unless it is what the hash holds."
  (unless (and (gethash "get_schema" org-roam-mcp-http--tools)
               (not (gethash "read_note" org-roam-mcp-http--tools)))
    (org-roam-mcp-http--register-all-tools)))

(defun orsb-tools-test--call (name &rest args)
  "Call tool NAME with ARGS (a plist of symbol keys) via the JSON-RPC handler.
Return (RPC-RESPONSE . ENVELOPE) where ENVELOPE is the decoded tool text."
  (unless (gethash name org-roam-mcp-http--tools) (orsb-tools-test--ensure-registered))
  (let* ((arguments (let (out) (while args (push (cons (pop args) (pop args)) out)) (nreverse out)))
         (raw (org-roam-mcp-http--handle-tools-call `((name . ,name) (arguments . ,arguments))))
         (rpc (orsb-test--json raw))
         (text (alist-get 'text (car (alist-get 'content (alist-get 'result rpc))))))
    (cons rpc (and text (orsb-test--json text)))))

(defun orsb-tools-test--data (name &rest args)
  "Call NAME and return its data, failing the test on an error envelope."
  (let* ((r (apply #'orsb-tools-test--call name args))
         (env (cdr r)))
    (unless (eq (alist-get 'ok env) t)
      (ert-fail (format "%s %S -> %S" name args env)))
    (alist-get 'data env)))

(defun orsb-tools-test--error (name &rest args)
  "Call NAME expecting an error envelope; return the error alist."
  (let* ((r (apply #'orsb-tools-test--call name args))
         (rpc (car r)) (env (cdr r)))
    (should (eq (alist-get 'ok env) :json-false))
    (should (eq (alist-get 'isError (alist-get 'result rpc)) t))
    (alist-get 'error env)))

;;;; schema and registration

(ert-deftest orsb-tools-schema-and-the-one-tool-set ()
  (orsb-test-with-vault
    (let ((schema (orsb-tools-test--data "get_schema")))
      (should (equal (alist-get 'stale_days schema) orsb-stale-days))
      (should (equal (alist-get 'enforce schema) "off"))
      ;; nothing of any particular vault is built in
      (dolist (gone '(node_types status_values blog_status_values status_aliases property_keys hugo_sections blog_enabled))
        (should-not (assq gone schema))))
    ;; exactly these twenty-three: no old names, no profiles
    (orsb-tools-test--ensure-registered)
    (let ((names '("search" "get_node" "list_nodes" "find_related" "create_node" "set_node" "update_body"
                   "set_checkbox" "get_tasks" "link_nodes" "move_node" "merge_nodes" "delete_node"
                   "add_daily_entry" "log_to_inbox" "resolve_inbox" "get_review" "restore" "get_schema"
                   "check_schema" "get_proposals" "decide_proposal" "reassign_id")))
      (dolist (name names) (should (gethash name org-roam-mcp-http--tools)))
      (should (= (hash-table-count org-roam-mcp-http--tools) (length names))))
    (dolist (gone '("read_note" "read_node" "semantic_search" "create_blog_post" "blog_status" "update_node"
                    "add_heading" "get_projects" "get_digest" "get_blog_status" "sync" "get_graph"))
      (should-not (gethash gone org-roam-mcp-http--tools)))))

;;;; get_node / set_node

(ert-deftest orsb-tools-get-node-by-id-path-title-and-section ()
  (orsb-test-with-vault
    (let ((by-id (orsb-tools-test--data "get_node" 'id "1784854105"))
          (by-title (orsb-tools-test--data "get_node" 'id "Project Backlog"))
          (by-path (orsb-tools-test--data "get_node" 'id "projects/project-backlog-1784854105.org")))
      (should (equal (alist-get 'id by-title) "1784854105"))
      (should (equal (alist-get 'id by-path) "1784854105"))
      (should (equal (alist-get 'status by-id) "active"))
      (should (equal (alist-get 'node_type by-id) "note"))
      (should (equal (alist-get 'NEXT-ACTION (alist-get 'properties by-id)) "Phase 4 status curation."))
      (should (equal (alist-get 'TITLE (alist-get 'keywords by-id)) "Project Backlog"))
      (should (equal (alist-get 'file by-id) "projects/project-backlog-1784854105.org"))
      (should (string-match-p "\\* Next Actions" (alist-get 'body by-id)))
      (should (equal (alist-get 'tags by-id) '("backlog"))))
    (let ((sec (orsb-tools-test--data "get_node" 'id "1784854105" 'section "purpose")))
      (should (equal (alist-get 'section sec) "purpose"))
      (should (string-match-p "Central index" (alist-get 'body sec)))
      (should-not (string-match-p "EMBEDDING" (alist-get 'body sec))))
    (let ((err (orsb-tools-test--error "get_node" 'id "1784854105" 'section "Nope")))
      (should (equal (alist-get 'code err) "not_found")))))

(ert-deftest orsb-tools-set-node-status-written ()
  (orsb-test-with-vault
    ;; without schema files a status is just a property: written as given, any value
    (let ((d (orsb-tools-test--data "set_node" 'id "Project Backlog" 'status "waiting")))
      (should (equal (alist-get 'status d) "waiting"))
      (should (equal (alist-get 'changed d) '("status"))))
    (should (string-match-p "^:STATUS: +waiting$" (orsb-test--file "projects/project-backlog-1784854105.org")))
    (orsb-tools-test--data "set_node" 'id "1784854105" 'status "shelved")
    (let ((err (orsb-tools-test--error "set_node" 'id "1784854105")))
      (should (equal (alist-get 'code err) "invalid_argument")))
    (let ((err (orsb-tools-test--error "set_node" 'id "1784854105" 'todo "DONE")))
      (should (equal (alist-get 'code err) "invalid_argument")))))

(ert-deftest orsb-tools-set-node-title-tags-properties-keywords ()
  (orsb-test-with-vault
    (let ((d (orsb-tools-test--data "set_node" 'id "1784854105"
                                    'title "Backlog v2" 'tags ["backlog" "hub"]
                                    'properties '((NEXT-ACTION . :json-null) (OWNER . "don"))
                                    'keywords '((hugo_draft . "true")))))
      (should (equal (alist-get 'title d) "Backlog v2"))
      (should (equal (alist-get 'tags d) '("backlog" "hub")))
      (should (equal (alist-get 'OWNER (alist-get 'properties d)) "don"))
      (should-not (alist-get 'NEXT-ACTION (alist-get 'properties d)))
      (should (equal (alist-get 'HUGO_DRAFT (alist-get 'keywords d)) "true")))
    (should (equal (org-roam-node-title (org-roam-node-from-id "1784854105")) "Backlog v2"))
    (let ((d (orsb-tools-test--data "set_node" 'id "1784854105" 'tags_add ["kai"] 'tags_remove ["hub"])))
      (should (equal (alist-get 'tags d) '("backlog" "kai"))))
    ;; heading node: todo + local tags
    (let ((d (orsb-tools-test--data "set_node" 'id "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1" 'todo "TODO" 'tags ["urgent"])))
      (should (equal (alist-get 'todo d) "TODO"))
      (should (member "urgent" (alist-get 'tags d))))
    (should (string-match-p "^\\* TODO Purpose +:urgent:$" (orsb-test--file "projects/project-backlog-1784854105.org")))))

;;;; body

(ert-deftest orsb-tools-update-body-modes ()
  (orsb-test-with-vault
    (orsb-tools-test--data "update_body" 'id "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1" 'content "Appended.")
    (orsb-tools-test--data "update_body" 'id "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1" 'content "Prepended." 'mode "prepend")
    (let ((body (alist-get 'body (orsb-tools-test--data "get_node" 'id "c169b8b9-d9b6-4792-bf6e-4a01f51e74d1"))))
      (should (string-match-p "\\`Prepended\\.\nCentral index.*\nAppended\\.\n\\'" body)))
    (orsb-tools-test--data "update_body" 'id "1784854105" 'section "Next Actions" 'content "- [ ] via section")
    (should (string-match-p "curation\\.\n- \\[ \\] via section" (orsb-test--file "projects/project-backlog-1784854105.org")))
    ;; guarded whole-file replace
    (let ((err (orsb-tools-test--error "update_body" 'id "1784854105" 'content "gone" 'mode "replace")))
      (should (member (alist-get 'code err) '("internal" "refused")))
      (should (string-match-p "Purpose" (orsb-test--file "projects/project-backlog-1784854105.org"))))))

;;;; listing and search

(ert-deftest orsb-tools-list-nodes ()
  (orsb-test-with-vault
    (let ((all (orsb-tools-test--data "list_nodes")))
      (should (= (alist-get 'total all) 2))
      (should (member "Project Backlog" (mapcar (lambda (n) (alist-get 'title n)) (alist-get 'nodes all)))))
    ;; no schema files: every note is a "note", and none has a lifecycle to go stale in
    (let ((notes (orsb-tools-test--data "list_nodes" 'node_type "note")))
      (should (= (alist-get 'total notes) 2))
      (should (eq (alist-get 'stale (car (alist-get 'nodes notes))) :json-false)))
    (should (= (alist-get 'total (orsb-tools-test--data "list_nodes" 'node_type "project")) 0))
    (should (= (alist-get 'total (orsb-tools-test--data "list_nodes" 'status "ACTIVE")) 1))
    (should (= (alist-get 'total (orsb-tools-test--data "list_nodes" 'status "done")) 0))
    (should (= (alist-get 'total (orsb-tools-test--data "list_nodes" 'tags ["backlog"])) 1))
    (let ((p (orsb-tools-test--data "list_nodes" 'tags ["backlog"])))
      (should (equal (alist-get 'next_action (car (alist-get 'nodes p))) "Phase 4 status curation.")))
    (let ((page (orsb-tools-test--data "list_nodes" 'limit 1)))
      (should (= (alist-get 'returned page) 1))
      (should (= (alist-get 'next_offset page) 1)))))

(ert-deftest orsb-tools-search-title-and-contextual ()
  (orsb-test-with-vault
    (let ((t1 (orsb-tools-test--data "search" 'query "backlog" 'mode "title")))
      (should (= (alist-get 'total t1) 1))
      (should (equal (alist-get 'id (car (alist-get 'hits t1))) "1784854105")))
    (let ((c (orsb-tools-test--data "search" 'query "central index" 'mode "contextual")))
      (should (>= (alist-get 'total c) 1))
      ;; the snippet is taken around the match
      (should (string-match-p "Central index" (alist-get 'snippet (car (alist-get 'hits c))))))
    (should (= (alist-get 'total (orsb-tools-test--data "search" 'query "zzzz-nothing" 'mode "title")) 0))
    (should (equal (alist-get 'code (orsb-tools-test--error "search" 'query "x" 'mode "magic")) "invalid_argument"))))

;;;; create / heading / links / delete

(ert-deftest orsb-tools-create-heading-link-delete ()
  (orsb-test-with-vault
    ;; a vault without schema files has one type, "note", kept in its top folder
    (let* ((created (orsb-tools-test--data "create_node" 'title "Rebuild the shed" 'body "Needs a roof."
                                           'status "active" 'properties '((NEXT-ACTION . "Buy lumber")) 'tags ["home"]))
           (id (alist-get 'id created)))
      (should (equal (alist-get 'node_type created) "note"))
      (should (equal (alist-get 'status created) "active"))
      (should (equal (alist-get 'NEXT-ACTION (alist-get 'properties created)) "Buy lumber"))
      (should-not (string-match-p "/" (alist-get 'file created)))
      (should-not (alist-get 'NODE-TYPE (alist-get 'properties created)))
      (should (member "home" (alist-get 'tags created)))
      (let ((h (orsb-tools-test--data "create_node" 'parent_id id 'title "Materials" 'body "- lumber" 'todo "TODO")))
        (should (= (alist-get 'level h) 1))
        (should (equal (alist-get 'todo h) "TODO"))
        (should (equal (alist-get 'title h) "Materials")))
      (let ((l (orsb-tools-test--data "link_nodes" 'id "1784854105" 'target_id id 'section "Next Actions")))
        (should (equal (alist-get 'action l) "added")))
      (should (string-match-p (concat "\\[\\[id:" id "\\]") (orsb-test--file "projects/project-backlog-1784854105.org")))
      (should (member id (mapcar (lambda (x) (alist-get 'id x))
                                 (alist-get 'links_to (orsb-tools-test--data "get_node" 'id "1784854105" 'include_body :json-false)))))
      (let ((u (orsb-tools-test--data "link_nodes" 'id "1784854105" 'target_id id 'action "remove")))
        (should (= (alist-get 'removed u) 1)))
      (should-not (string-match-p (concat "\\[\\[id:" id "\\]") (orsb-test--file "projects/project-backlog-1784854105.org")))
      (let ((d (orsb-tools-test--data "delete_node" 'id id 'archive t)))
        (should (equal (alist-get 'action d) "archived"))
        (should (string-prefix-p "archive/" (alist-get 'file d))))
      ;; archived notes stay inside the vault, so the node still resolves
      (should (string-prefix-p "archive/" (alist-get 'file (orsb-tools-test--data "get_node" 'id id 'include_body :json-false))))
      (orsb-tools-test--data "delete_node" 'id id)
      (should (equal (alist-get 'code (orsb-tools-test--error "get_node" 'id id)) "not_found")))
    (should (equal (alist-get 'code (orsb-tools-test--error "create_node" 'node_type "widget" 'title "x")) "invalid_argument"))))

;;;; daily / inbox

(ert-deftest orsb-tools-daily-inbox ()
  (orsb-test-with-vault
    (let ((org-roam-dailies-directory "daily/"))
      (orsb-tools-test--data "add_daily_entry" 'title "Kai did a thing" 'points ["one" "two"] 'tags ["kai"])
      (let ((d (orsb-tools-test--data "get_node" 'id "today")))
        (should (string-match-p "Kai did a thing" (alist-get 'body d)))
        (should (string-match-p "- one" (alist-get 'body d))))
      (orsb-tools-test--data "log_to_inbox" 'text "file this" 'linked_id "1784854105")
      (should (string-match-p "file this" (alist-get 'body (orsb-tools-test--data "get_node" 'id "today")))))))

;;;; missing args: JSON-RPC -32602 before dispatch

(ert-deftest orsb-tools-missing-required-arg-is-rpc-error ()
  (orsb-test-with-vault
    (orsb-tools-test--ensure-registered)
    (let ((rpc (orsb-test--json (org-roam-mcp-http--handle-tools-call '((name . "get_node") (arguments . ()))))))
      (should (= (alist-get 'code (alist-get 'error rpc)) -32602)))))

;;;; title search forgives an extra word

(ert-deftest orsb-tools-title-search-falls-back-to-the-words ()
  (orsb-test-with-vault
    ;; as written: the substring match, unchanged
    (let ((hits (alist-get 'hits (orsb-tools-test--data "search" 'query "Backlog" 'mode "title"))))
      (should (equal (mapcar (lambda (h) (alist-get 'title h)) hits) '("Project Backlog"))))
    ;; an extra word no title contains: the note that holds most of the words
    (let ((hits (alist-get 'hits (orsb-tools-test--data "search" 'query "Project Backlog note" 'mode "title"))))
      (should (equal (alist-get 'title (car hits)) "Project Backlog")))
    ;; too little in common is still nothing
    (should-not (alist-get 'hits (orsb-tools-test--data "search" 'query "Backlog of unrelated garden chores" 'mode "title")))))

;;;; schema files

(ert-deftest orsb-tools-schema-files-drive-get-schema-and-check ()
  (orsb-test-with-vault
    ;; no schema files: nothing to report
    (let ((schema (orsb-tools-test--data "get_schema")))
      (should (eq (alist-get 'schema_files schema) :json-false))
      (should-not (append (alist-get 'types schema) nil)))
    (let ((dir (file-name-as-directory org-roam-directory)))
      (with-temp-file (expand-file-name "_schema.org" dir)
        (insert "* Type\n:PROPERTIES:\n:TYPE: note\n:END:\n\n* Properties\n"
                "| property | required | form |\n|-\n| ID | yes | id |\n"))
      (make-directory (expand-file-name "projects" dir) t)
      (with-temp-file (expand-file-name "projects/_schema.org" dir)
        (insert "* Type\n:PROPERTIES:\n:TYPE: project\n:WRITE: open\n:LIFECYCLE: STATUS\n:END:\n\n"
                "* Properties\n| property | required | form |\n|-\n| OWNER | yes | text |\n\n"
                "* Sections\n| section | required | rule |\n|-\n| Log | yes | append |\n")))
    (let* ((schema (orsb-tools-test--data "get_schema"))
           (types (append (alist-get 'types schema) nil)))
      (should (eq (alist-get 'schema_files schema) t))
      (should (equal (mapcar (lambda (e) (alist-get 'type e)) types) '("note" "project")))
      (should (equal (alist-get 'parent (cadr types)) "note"))
      ;; the root type has no parent: null on the wire, not a string
      (should-not (alist-get 'parent (car types))))
    (should (string-search "\"parent\":null"
                           (alist-get 'text (car (alist-get 'content (alist-get 'result (car (orsb-tools-test--call "get_schema"))))))))
    (let ((project (orsb-tools-test--data "get_schema" 'type "project")))
      (should (equal (append (alist-get 'is_a project) nil) '("note" "project")))
      (should (equal (alist-get 'LIFECYCLE (alist-get 'settings project)) "STATUS"))
      (should (equal (mapcar (lambda (r) (alist-get 'property r)) (append (alist-get 'properties project) nil))
                     '("ID" "OWNER")))
      (should (equal (alist-get 'rule (car (append (alist-get 'sections project) nil))) "append")))
    (should (equal (alist-get 'code (orsb-tools-test--error "get_schema" 'type "ghost")) "not_found"))
    (let ((report (orsb-tools-test--data "check_schema" 'rule "missing-property" 'path "projects/")))
      (should (eq (alist-get 'schema_files report) t))
      (should (> (alist-get 'notes report) 0))
      (should (= (alist-get 'matching report) (length (alist-get 'violations report))))
      (seq-doseq (v (alist-get 'violations report))
        (should (equal (alist-get 'rule v) "missing-property"))
        (should (string-prefix-p "projects/" (alist-get 'file v)))))))

;;;; the vault's rules on writes

(defun orsb-tools-test--schema-vault (enforce)
  "Give the test vault schema files, with ENFORCE as the root's mode."
  (orsb-test--write "_schema.org"
                    (concat "* Type\n:PROPERTIES:\n:TYPE: note\n:NOTES_HERE: no\n:ENFORCE: " enforce "\n:END:\n\n"
                            "* Properties\n| property | required | form |\n|-\n| ID | yes | id |\n"
                            "| CREATED | no | date, set by the server |\n"))
  (orsb-test--write "projects/_schema.org"
                    (concat "* Type\n:PROPERTIES:\n:TYPE: project\n:NOTES_HERE: yes\n:LIFECYCLE: STATUS\n"
                            ":LIFECYCLE_VALUES: active waiting done\n:LIFECYCLE_FINISHED: done\n:END:\n\n"
                            "* Properties\n| property | required | form |\n|-\n"
                            "| STATUS | yes | one of LIFECYCLE_VALUES |\n| NEXT-ACTION | yes | one sentence |\n\n"
                            "* Sections\n| section | required | rule |\n|-\n"
                            "| Current state | yes | replace |\n| Log | yes | append |\n| Design | no | propose |\n\n"
                            "* Template\n** Current state\nAs of %DATE%.\n** Log\n** Design\n"))
  (orsb-test--write "projects/posts/_schema.org"
                    (concat "* Type\n:PROPERTIES:\n:TYPE: post\n:LIFECYCLE_VALUES: draft published\n"
                            ":LIFECYCLE_FINISHED: published\n:END:\n\n"
                            "* Properties\n| property | required | form | default |\n|-\n"
                            "| STATUS | yes | one of LIFECYCLE_VALUES | draft |\n"
                            "| NEXT-ACTION | remove | | |\n"
                            "| EXPORT_FILE_NAME | yes | text | %SLUG% |\n\n"
                            "* Sections\n| section | required | rule |\n|-\n"
                            "| Current state | remove | |\n| Log | remove | |\n| Design | remove | |\n\n"
                            "* Keywords\n| keyword | value |\n|-\n| date | [%DATE%] |\n| hugo_draft | true |\n| hugo_tags | %TAGS% |\n\n"
                            "* Template\n** Draft\n"))
  (orsb-test--write "telos/_schema.org"
                    "* Type\n:PROPERTIES:\n:TYPE: telos\n:NOTES_HERE: yes\n:WRITE: propose\n:NEVER: move merge delete\n:ENFORCE: strict\n:END:\n")
  (orsb-test--write "telos/mission.org" ":PROPERTIES:\n:ID: telos-mission\n:END:\n#+title: Mission\n\nTo make things.\n")
  (org-roam-db-sync))

(ert-deftest orsb-tools-a-note-is-created-the-way-its-schema-says ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    (let* ((made (orsb-tools-test--data "create_node" 'title "Rack the server" 'node_type "project"
                                        'status "active" 'next_action "Buy rails" 'body "Lead text."))
           (file (alist-get 'file made))
           (text (orsb-test--file file)))
      (should (eq (alist-get 'created made) t))
      (should (string-prefix-p "projects/" file))
      (should (equal (alist-get 'node_type made) "project"))
      (should (string-match-p "^:STATUS: active$" text))
      (should (string-match-p "^:NEXT-ACTION: Buy rails$" text))
      (should (string-match-p "^:CREATED: \\[[0-9-]+\\]$" text))
      (should-not (string-match-p "NODE-TYPE" text))
      (should (string-match-p "Lead text\\.\n\n\\* Current state\nAs of [0-9-]+\\.\n\\* Log\n\\* Design" text)))
    ;; a required property is missing; a value is outside the set; the type does not exist; the root holds no notes
    (should (string-match-p "NEXT-ACTION is required"
                            (alist-get 'message (orsb-tools-test--error "create_node" 'title "No plan" 'node_type "project"
                                                                        'status "active"))))
    (should (equal (alist-get 'code (orsb-tools-test--error "create_node" 'title "Odd" 'node_type "project"
                                                            'status "paused" 'next_action "x"))
                   "schema_violation"))
    (should (string-match-p "No such note type: ghost"
                            (alist-get 'message (orsb-tools-test--error "create_node" 'title "X" 'node_type "ghost"))))
    (should (string-match-p "top folder"
                            (alist-get 'message (orsb-tools-test--error "create_node" 'title "Loose" 'node_type "note"))))))

(ert-deftest orsb-tools-sections-and-policies-are-enforced ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    (let ((id (alist-get 'id (orsb-tools-test--data "create_node" 'title "Fan curve" 'node_type "project"
                                                    'status "active" 'next_action "Measure"))))
      ;; Log only grows
      (orsb-tools-test--data "update_body" 'id id 'section "Log" 'content "- 2026-11-25 measured")
      (should (string-match-p "append-only"
                              (alist-get 'message (orsb-tools-test--error "update_body" 'id id 'section "Log"
                                                                          'mode "replace" 'content "gone"))))
      ;; Current state is rewritten whole
      (orsb-tools-test--data "update_body" 'id id 'section "Current state" 'mode "replace" 'content "As of now: done.")
      (should (string-match-p "rewritten whole"
                              (alist-get 'message (orsb-tools-test--error "update_body" 'id id 'section "Current state"
                                                                          'content "and also"))))
      ;; Design needs a proposal; the whole note needs a section
      (should (string-match-p "approved proposal"
                              (alist-get 'message (orsb-tools-test--error "update_body" 'id id 'section "Design"
                                                                          'content "we decided"))))
      (should (string-match-p "name a section"
                              (alist-get 'message (orsb-tools-test--error "update_body" 'id id 'content "stray"))))
      ;; a section the schema does not name is free
      (orsb-tools-test--data "update_body" 'id id 'section "Scratch" 'create_section t 'content "anything")
      ;; status: inside the set or refused
      (orsb-tools-test--data "set_node" 'id id 'status "waiting")
      (should (string-match-p "not one of"
                              (alist-get 'message (orsb-tools-test--error "set_node" 'id id 'status "paused"))))
      (should (string-match-p "NEXT-ACTION is required"
                              (alist-get 'message (orsb-tools-test--error "set_node" 'id id
                                                                          'properties '((NEXT-ACTION . "")))))))
    ;; telos: nothing changes it directly
    (should (string-match-p "approved proposal"
                            (alist-get 'message (orsb-tools-test--error "update_body" 'id "telos-mission" 'content "more"))))
    (should (string-match-p "never deleted"
                            (alist-get 'message (orsb-tools-test--error "delete_node" 'id "telos-mission"))))
    (should (string-match-p "To make things" (orsb-test--file "telos/mission.org")))))

(ert-deftest orsb-tools-warn-mode-writes-and-says-so ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "warn")
    (let ((made (orsb-tools-test--data "create_node" 'title "Half a plan" 'node_type "project" 'status "active")))
      (should (eq (alist-get 'created made) t))
      (should (equal (alist-get 'warnings made) '("NEXT-ACTION is required on a project note")))
      (let ((r (orsb-tools-test--data "update_body" 'id (alist-get 'id made) 'section "Log" 'mode "replace" 'content "x")))
        (should (string-match-p "append-only" (car (alist-get 'warnings r))))))
    ;; a type may be strict in a vault that only warns: telos is, here
    (should (string-match-p "approved proposal"
                            (alist-get 'message (orsb-tools-test--error "update_body" 'id "telos-mission" 'content "more"))))
    (should (string-match-p "never deleted" (alist-get 'message (orsb-tools-test--error "delete_node" 'id "telos-mission"))))
    (should (string-match-p "To make things" (orsb-test--file "telos/mission.org")))
    ;; and it can still be changed the approved way
    (let ((pid (alist-get 'proposal_id (orsb-tools-test--data "update_body" 'id "telos-mission" 'content "And to teach." 'propose t))))
      (orsb-tools-test--data "decide_proposal" 'id pid 'decision "approve")
      (should (string-match-p "And to teach\\." (orsb-test--file "telos/mission.org"))))
    ;; a write that breaks nothing carries no warnings
    (let ((ok (orsb-tools-test--data "create_node" 'title "Whole plan" 'node_type "project"
                                     'status "active" 'next_action "Go")))
      (should-not (alist-get 'warnings ok)))))

;;;; defaults, keyword lines, the review

(ert-deftest orsb-tools-a-type-can-supply-defaults-and-keywords ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    (should (equal (alist-get 'enforce (orsb-tools-test--data "get_schema")) "strict"))
    (let* ((made (orsb-tools-test--data "create_node" 'title "Racking Feynman!" 'node_type "post" 'tags ["homelab"]))
           (text (orsb-test--file (alist-get 'file made))))
      (should (string-prefix-p "projects/posts/" (alist-get 'file made)))
      (should (equal (alist-get 'node_type made) "post"))
      (should (equal (alist-get 'status made) "draft"))
      (should (string-match-p "^:EXPORT_FILE_NAME: racking-feynman$" text))
      (should (string-match-p "^#\\+date: \\[[0-9-]+\\]\n#\\+hugo_draft: true$" text))
      ;; the note's tags fill %TAGS%
      (should (string-match-p "^#\\+hugo_tags: homelab$" text))
      (should (string-match-p "^\\* Draft$" text))
      (should-not (string-match-p "Current state" text))
      ;; keyword lines given at creation join the type's, and win over them; a line with nothing to say is left out
      (let ((text2 (orsb-test--file (alist-get 'file (orsb-tools-test--data
                                                      "create_node" 'title "Second post" 'node_type "post"
                                                      'keywords '((hugo_categories . "Homelab") (hugo_draft . "false")))))))
        (should (string-match-p "^#\\+hugo_categories: Homelab$" text2))
        (should (string-match-p "^#\\+hugo_draft: false$" text2))
        (should-not (string-match-p "hugo_draft: true" text2))
        (should-not (string-match-p "hugo_tags" text2)))
      ;; its own lifecycle, not the project's
      (orsb-tools-test--data "set_node" 'id (alist-get 'id made) 'status "published")
      (should (string-match-p "not one of"
                              (alist-get 'message (orsb-tools-test--error "set_node" 'id (alist-get 'id made) 'status "active")))))))

(ert-deftest orsb-tools-review-lists-unfinished-notes-of-any-lifecycle-type ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    (let ((post (alist-get 'id (orsb-tools-test--data "create_node" 'title "A post" 'node_type "post")))
          (plan (alist-get 'id (orsb-tools-test--data "create_node" 'title "A plan" 'node_type "project"
                                                      'status "active" 'next_action "Start"))))
      (cl-flet ((open-titles () (mapcar (lambda (r) (alist-get 'title r))
                                        (alist-get 'items (alist-get 'open (orsb-tools-test--data "get_review" 'limit 50))))))
        (should (member "A post" (open-titles)))
        (should (member "A plan" (open-titles)))
        ;; a telos note has no lifecycle: never in the list
        (should-not (member "Mission" (open-titles)))
        (orsb-tools-test--data "set_node" 'id post 'status "published")
        (orsb-tools-test--data "set_node" 'id plan 'status "done")
        (should-not (member "A post" (open-titles)))
        (should-not (member "A plan" (open-titles))))
      (let ((review (orsb-tools-test--data "get_review")))
        (dolist (gone '(active_projects stale_projects followups))
          (should-not (assq gone review)))))
    ;; a heading promoted to a note lands in its type's folder
    (let* ((plan (alist-get 'id (orsb-tools-test--data "create_node" 'title "Host plan" 'node_type "project"
                                                       'status "active" 'next_action "x")))
           (logged (alist-get 'id (orsb-tools-test--data "create_node" 'parent_id plan 'section "Log" 'title "Entry"
                                                         'body "Happened.")))
           (h (alist-get 'id (orsb-tools-test--data "create_node" 'parent_id plan 'title "Spin-off" 'body "Its own thing."))))
      ;; taking an entry out of an append-only section is an edit of that section
      (should (string-match-p "append-only"
                              (alist-get 'message (orsb-tools-test--error "move_node" 'id logged 'to_note t 'node_type "post"))))
      ;; a section the schema does not rule is free to leave
      (let ((moved (orsb-tools-test--data "move_node" 'id h 'to_note t 'node_type "post" 'leave_link :json-false)))
        (should (string-prefix-p "projects/posts/" (alist-get 'file moved)))))))

;;;; proposals

(ert-deftest orsb-tools-a-proposal-waits-then-applies ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    (let* ((id (alist-get 'id (orsb-tools-test--data "create_node" 'title "Duct" 'node_type "project"
                                                     'status "active" 'next_action "Print it")))
           (file (alist-get 'file (orsb-tools-test--data "get_node" 'id id)))
           (before (orsb-test--file file))
           ;; refused directly, and the refusal says how to file it
           (refusal (orsb-tools-test--error "update_body" 'id id 'section "Design" 'content "Two fans, pushing."))
           (filed (orsb-tools-test--data "update_body" 'id id 'section "Design" 'content "Two fans, pushing."
                                         'propose t 'reason "Decided on 2026-11-25"))
           (pid (alist-get 'proposal_id filed)))
      (should (string-match-p "propose:true" (alist-get 'hint refusal)))
      (should (eq (alist-get 'proposed filed) t))
      (should (member "Two fans, pushing." (alist-get 'lines_added (car (alist-get 'would_change filed)))))
      ;; nothing was written
      (should (equal (orsb-test--file file) before))
      (let* ((list (orsb-tools-test--data "get_proposals"))
             (one (car (alist-get 'proposals list))))
        (should (= (alist-get 'total list) 1))
        (should (equal (alist-get 'id one) pid))
        (should (equal (alist-get 'reason one) "Decided on 2026-11-25"))
        (should (equal (alist-get 'files one) (list file)))
        (should (eq (alist-get 'stale one) :json-false)))
      (should (equal (alist-get 'section (alist-get 'arguments (orsb-tools-test--data "get_proposals" 'id pid))) "Design"))
      (let ((done (orsb-tools-test--data "decide_proposal" 'id pid 'decision "approve" 'note "ok")))
        (should (equal (alist-get 'status done) "approved"))
        (should (stringp (alist-get 'change_id done))))
      (should (string-match-p "\\* Design\nTwo fans, pushing\\." (orsb-test--file file)))
      (should (= (alist-get 'total (orsb-tools-test--data "get_proposals")) 0))
      (should (equal (alist-get 'status (orsb-tools-test--data "get_proposals" 'id pid)) "approved"))
      (should (equal (alist-get 'code (orsb-tools-test--error "decide_proposal" 'id pid 'decision "approve")) "refused")))
    ;; a note that only changes by proposal
    (let ((pid (alist-get 'proposal_id (orsb-tools-test--data "update_body" 'id "telos-mission"
                                                              'content "And to teach." 'propose t))))
      (orsb-tools-test--data "decide_proposal" 'id pid 'decision "approve")
      (should (string-match-p "And to teach\\." (orsb-test--file "telos/mission.org"))))))

;;;; batches of proposals, and the note hidden behind a shared id

(ert-deftest orsb-tools-a-batch-is-decided-together ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    (let* ((mk (lambda (title) (alist-get 'id (orsb-tools-test--data "create_node" 'title title 'node_type "project"
                                                                     'status "active" 'next_action "Go"))))
           (a (funcall mk "Alpha")) (b (funcall mk "Beta")) (c (funcall mk "Gamma"))
           (file (lambda (id) (alist-get 'file (orsb-tools-test--data "get_node" 'id id)))))
      (dolist (id (list a b))
        (orsb-tools-test--data "update_body" 'id id 'section "Design" 'content "Batched line." 'propose t 'batch "cleanup-1"))
      (orsb-tools-test--data "update_body" 'id c 'section "Design" 'content "Alone." 'propose t)
      (let ((listed (orsb-tools-test--data "get_proposals" 'batch "cleanup-1")))
        (should (= (alist-get 'total listed) 2))
        (should (equal (alist-get 'batch (car (alist-get 'proposals listed))) "cleanup-1")))
      ;; one of the two notes moves on: the batch still decides the other and names the refusal
      (orsb-tools-test--data "update_body" 'id b 'section "Log" 'content "- moved on")
      (let ((r (orsb-tools-test--data "decide_proposal" 'batch "cleanup-1" 'decision "approve")))
        (should (= (alist-get 'decided r) 1))
        (should (equal (alist-get 'code (car (alist-get 'refused r))) "conflict")))
      (should (string-match-p "Batched line" (orsb-test--file (funcall file a))))
      (should-not (string-match-p "Batched line" (orsb-test--file (funcall file b))))
      (let ((r (orsb-tools-test--data "decide_proposal" 'batch "cleanup-1" 'decision "approve" 'force t)))
        (should (= (alist-get 'decided r) 1)))
      (should (string-match-p "Batched line" (orsb-test--file (funcall file b))))
      ;; the proposal outside the batch is untouched
      (should (= (alist-get 'total (orsb-tools-test--data "get_proposals")) 1))
      (should-not (string-match-p "Alone" (orsb-test--file (funcall file c))))
      (should (equal (alist-get 'code (orsb-tools-test--error "decide_proposal" 'batch "cleanup-1" 'decision "approve")) "not_found"))
      (should (equal (alist-get 'code (orsb-tools-test--error "decide_proposal" 'decision "approve")) "invalid_argument")))))

(ert-deftest orsb-tools-a-hidden-note-gets-its-own-id ()
  (orsb-test-with-vault
    (dolist (n '("one" "two"))
      (with-temp-file (expand-file-name (concat "shared-" n ".org") org-roam-directory)
        (insert ":PROPERTIES:\n:ID: 1700000000\n:END:\n#+title: Shared " n "\n\n* Part\n:PROPERTIES:\n:ID: part-" n "\n:END:\nText.\n")))
    (org-roam-db-sync)
    (let* ((seen (file-name-nondirectory (org-roam-node-file (org-roam-node-from-id "1700000000"))))
           (hidden (if (equal seen "shared-one.org") "shared-two.org" "shared-one.org")))
      (should (= 2 (alist-get 'duplicate-id (alist-get 'by_rule (orsb-tools-test--data "check_schema")))))
      ;; the copy links point at keeps its id
      (should (equal (alist-get 'code (orsb-tools-test--error "reassign_id" 'file seen)) "refused"))
      (let ((preview (orsb-tools-test--data "reassign_id" 'file hidden 'dry_run t)))
        (should (eq (alist-get 'dry_run preview) t))
        (should (string-match-p ":ID: 1700000000" (orsb-test--file hidden))))
      (let* ((done (orsb-tools-test--data "reassign_id" 'file hidden))
             (new (alist-get 'new_id done)))
        (should (equal (alist-get 'old_id done) "1700000000"))
        (should (string-match-p (concat ":ID: " (regexp-quote new)) (orsb-test--file hidden)))
        ;; only the note's own id changed, and both notes are now in the index
        (should (string-match-p ":ID: part-" (orsb-test--file hidden)))
        (should (equal (file-name-nondirectory (org-roam-node-file (org-roam-node-from-id new))) hidden))
        (should (equal (file-name-nondirectory (org-roam-node-file (org-roam-node-from-id "1700000000"))) seen))
        (should-not (alist-get 'duplicate-id (alist-get 'by_rule (orsb-tools-test--data "check_schema"))))
        (should (equal (alist-get 'code (orsb-tools-test--error "reassign_id" 'file hidden)) "refused"))
        (orsb-tools-test--data "restore" 'change_id (alist-get 'change_id done))
        (should (string-match-p ":ID: 1700000000" (orsb-test--file hidden)))))))

(ert-deftest orsb-tools-a-shared-heading-id-and-ids-in-examples ()
  (orsb-test-with-vault
    (dolist (n '("one" "two"))
      (with-temp-file (expand-file-name (concat "h-" n ".org") org-roam-directory)
        (insert ":PROPERTIES:\n:ID: note-" n "\n:END:\n#+title: H " n "\n\n* Part\n:PROPERTIES:\n:ID: same-part\n:END:\nText.\n"
                "\n#+begin_example\n* Not a section\n:PROPERTIES:\n:ID: <uuid>\n:END:\n#+end_example\n")))
    (org-roam-db-sync)
    ;; the ids inside the example blocks are text, not ids: only the heading id is shared
    (let ((v (alist-get 'violations (orsb-tools-test--data "check_schema" 'rule "duplicate-id"))))
      (should (= (length v) 2))
      (should (string-match-p "same-part" (alist-get 'detail (car v)))))
    (let* ((seen (file-name-nondirectory (org-roam-node-file (org-roam-node-from-id "same-part"))))
           (hidden (if (equal seen "h-one.org") "h-two.org" "h-one.org")))
      (should (equal (alist-get 'code (orsb-tools-test--error "reassign_id" 'file seen 'id "same-part")) "refused"))
      (should (equal (alist-get 'code (orsb-tools-test--error "reassign_id" 'file hidden 'id "nope")) "not_found"))
      (let ((done (orsb-tools-test--data "reassign_id" 'file hidden 'id "same-part")))
        (should (string-match-p (concat "\\* Part\n:PROPERTIES:\n:ID: " (regexp-quote (alist-get 'new_id done)))
                                (orsb-test--file hidden)))
        ;; the note's own id and the example text are untouched
        (should (string-match-p ":ID: note-" (orsb-test--file hidden)))
        (should (string-match-p ":ID: <uuid>" (orsb-test--file hidden))))
      (should-not (alist-get 'duplicate-id (alist-get 'by_rule (orsb-tools-test--data "check_schema")))))))

(ert-deftest orsb-tools-a-note-moves-to-another-folder ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "warn")
    (make-directory (expand-file-name "projects/sub" org-roam-directory) t)
    (with-temp-file (expand-file-name "loose.org" org-roam-directory)
      (insert ":PROPERTIES:\n:ID: loose-1\n:END:\n#+title: Loose\n\n* Part\n:PROPERTIES:\n:ID: loose-part\n:END:\nText.\n"))
    (org-roam-db-sync)
    (should (equal (alist-get 'code (orsb-tools-test--error "move_node" 'id "loose-1" 'folder "nowhere")) "not_found"))
    (should (equal (alist-get 'code (orsb-tools-test--error "move_node" 'id "loose-part" 'folder "projects")) "invalid_argument"))
    (let ((preview (orsb-tools-test--data "move_node" 'id "loose-1" 'folder "projects/sub" 'dry_run t)))
      (should (eq (alist-get 'dry_run preview) t))
      (should (file-exists-p (expand-file-name "loose.org" org-roam-directory))))
    (let ((done (orsb-tools-test--data "move_node" 'id "loose-1" 'folder "projects/sub")))
      (should (equal (alist-get 'file done) "projects/sub/loose.org"))
      (should (equal (alist-get 'node_type done) "project"))
      (should-not (file-exists-p (expand-file-name "loose.org" org-roam-directory)))
      ;; found by the same id, at its new place and as its new type
      (let ((n (orsb-tools-test--data "get_node" 'id "loose-1")))
        (should (equal (alist-get 'file n) "projects/sub/loose.org"))
        (should (equal (alist-get 'node_type n) "project")))
      (should (equal (alist-get 'file (orsb-tools-test--data "get_node" 'id "loose-part")) "projects/sub/loose.org"))
      (should (equal (alist-get 'code (orsb-tools-test--error "move_node" 'id "loose-1" 'folder "projects/sub")) "invalid_argument"))
      (orsb-tools-test--data "restore" 'change_id (alist-get 'change_id done))
      (should (file-exists-p (expand-file-name "loose.org" org-roam-directory))))
    ;; a type that is never moved stays where it is
    (should (equal (alist-get 'code (orsb-tools-test--error "move_node" 'id "telos-mission" 'folder "projects")) "schema_violation"))))

(ert-deftest orsb-tools-a-proposal-goes-stale-or-is-rejected ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    (let* ((id (alist-get 'id (orsb-tools-test--data "create_node" 'title "Rack" 'node_type "project"
                                                     'status "active" 'next_action "Rails")))
           (file (alist-get 'file (orsb-tools-test--data "get_node" 'id id)))
           (first (alist-get 'proposal_id (orsb-tools-test--data "update_body" 'id id 'section "Design"
                                                                 'content "Four posts." 'propose t)))
           (second (alist-get 'proposal_id (orsb-tools-test--data "update_body" 'id id 'section "Design"
                                                                  'content "Casters." 'propose t))))
      ;; the note moves on
      (orsb-tools-test--data "update_body" 'id id 'section "Log" 'content "- racked")
      (should (eq (alist-get 'stale (car (alist-get 'proposals (orsb-tools-test--data "get_proposals")))) t))
      (should (equal (alist-get 'changed_since (orsb-tools-test--data "get_proposals" 'id first)) (list file)))
      (should (equal (alist-get 'code (orsb-tools-test--error "decide_proposal" 'id first 'decision "approve")) "conflict"))
      (should-not (string-match-p "Four posts" (orsb-test--file file)))
      ;; forced, it applies to the note as it is now
      (orsb-tools-test--data "decide_proposal" 'id first 'decision "approve" 'force t)
      (should (string-match-p "Four posts\\." (orsb-test--file file)))
      (should (string-match-p "- racked" (orsb-test--file file)))
      ;; rejected: kept on record, never applied
      (should (equal (alist-get 'status (orsb-tools-test--data "decide_proposal" 'id second 'decision "reject" 'note "no"))
                     "rejected"))
      (should-not (string-match-p "Casters" (orsb-test--file file)))
      (should (= (alist-get 'total (orsb-tools-test--data "get_proposals" 'status "rejected")) 1)))
    ;; proposing does not get round the other rules, and a read has nothing to propose
    (let ((id (alist-get 'id (orsb-tools-test--data "create_node" 'title "Other" 'node_type "project"
                                                    'status "active" 'next_action "x"))))
      (should (string-match-p "append-only"
                              (alist-get 'message (orsb-tools-test--error "update_body" 'id id 'section "Log" 'mode "replace"
                                                                          'content "wiped" 'propose t)))))
    (should (equal (alist-get 'code (orsb-tools-test--error "get_schema" 'propose t)) "invalid_argument"))
    (should (equal (alist-get 'code (orsb-tools-test--error "get_proposals" 'id "nope-1")) "not_found"))))

(provide 'orsb-tools-test)
;;; orsb-tools-test.el ends here
