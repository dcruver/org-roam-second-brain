;;; orsb-tools-test.el --- ERT tests for the 2.0 MCP contract -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives tools through `org-roam-mcp-http--handle-tools-call', i.e. the same
;; path an MCP client takes, and checks the {ok,data|error} envelope, the
;; isError flag, the shared `id' parameter, and the legacy deprecation.

;;; Code:

(require 'ert)
(require 'org-roam)
(require 'orsb-core)
(require 'orsb-tools)
(require 'org-roam-mcp-http)
(require 'orsb-core-test)   ; fixtures: orsb-test-with-vault, orsb-test--file, orsb-test--json

(defun orsb-tools-test--ensure-registered ()
  "Register the tool set once (the hash is global)."
  (unless (gethash "get_schema" org-roam-mcp-http--tools)
    (org-roam-mcp-http--register-all-tools)))

(defun orsb-tools-test--call (name &rest args)
  "Call tool NAME with ARGS (a plist of symbol keys) via the JSON-RPC handler.
Return (RPC-RESPONSE . ENVELOPE) where ENVELOPE is the decoded tool text."
  (orsb-tools-test--ensure-registered)
  (let* ((arguments (let (out) (while args (push (cons (pop args) (pop args)) out)) (nreverse out)))
         (raw (org-roam-mcp-http--handle-tools-call `((name . ,name) (arguments . ,arguments))))
         (rpc (orsb-test--json raw))
         (text (alist-get 'text (car (alist-get 'content (alist-get 'result rpc))))))
    (cons rpc (and text (orsb-test--json text)))))

(defun orsb-tools-test--data (name &rest args)
  "Call NAME and return its data, failing the test on an error envelope."
  (let* ((r (apply #'orsb-tools-test--call name args))
         (env (cdr r)))
    (should (eq (alist-get 'ok env) t))
    (alist-get 'data env)))

(defun orsb-tools-test--error (name &rest args)
  "Call NAME expecting an error envelope; return the error alist."
  (let* ((r (apply #'orsb-tools-test--call name args))
         (rpc (car r)) (env (cdr r)))
    (should (eq (alist-get 'ok env) :json-false))
    (should (eq (alist-get 'isError (alist-get 'result rpc)) t))
    (alist-get 'error env)))

;;;; schema and registration

(ert-deftest orsb-tools-schema-and-legacy-marking ()
  (orsb-test-with-vault
    (let ((schema (orsb-tools-test--data "get_schema")))
      (should (member "waiting" (alist-get 'status_values schema)))
      (should (equal (alist-get 'stale_days schema) orsb-stale-days))
      (should (equal (cdr (assq 'completed (alist-get 'status_aliases schema))) "done")))
    (dolist (name '("search" "get_node" "list_nodes" "create_node" "add_heading" "set_node" "update_body"
                    "delete_node" "link_nodes" "add_daily_entry" "get_daily" "log_to_inbox" "get_digest"
                    "get_projects" "get_followups" "get_blog_status" "sync" "get_schema"))
      (should (gethash name org-roam-mcp-http--tools)))
    (should (string-prefix-p "[deprecated → get_node]"
                             (org-roam-mcp-tool-description (gethash "read_node" org-roam-mcp-http--tools))))
    (should (string-prefix-p "[deprecated → set_node / update_body]"
                             (org-roam-mcp-tool-description (gethash "update_node" org-roam-mcp-http--tools))))))

;;;; get_node / set_node

(ert-deftest orsb-tools-get-node-by-id-path-title-and-section ()
  (orsb-test-with-vault
    (let ((by-id (orsb-tools-test--data "get_node" 'id "1784854105"))
          (by-title (orsb-tools-test--data "get_node" 'id "Project Backlog"))
          (by-path (orsb-tools-test--data "get_node" 'id "projects/project-backlog-1784854105.org")))
      (should (equal (alist-get 'id by-title) "1784854105"))
      (should (equal (alist-get 'id by-path) "1784854105"))
      (should (equal (alist-get 'status by-id) "active"))
      (should (equal (alist-get 'node_type by-id) "project"))
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

(ert-deftest orsb-tools-set-node-status-validated-and-written ()
  (orsb-test-with-vault
    (let ((d (orsb-tools-test--data "set_node" 'id "Project Backlog" 'status "WAITING")))
      (should (equal (alist-get 'status d) "waiting"))
      (should (equal (alist-get 'changed d) '("status"))))
    (should (string-match-p "^:STATUS: +waiting$" (orsb-test--file "projects/project-backlog-1784854105.org")))
    (let ((err (orsb-tools-test--error "set_node" 'id "1784854105" 'status "shelved")))
      (should (equal (alist-get 'code err) "invalid_argument"))
      (should (string-match-p "someday" (alist-get 'message err))))
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

(ert-deftest orsb-tools-list-and-projects ()
  (orsb-test-with-vault
    (let ((all (orsb-tools-test--data "list_nodes")))
      (should (= (alist-get 'total all) 2))
      (should (member "Project Backlog" (mapcar (lambda (n) (alist-get 'title n)) (alist-get 'nodes all)))))
    (let ((projects (orsb-tools-test--data "list_nodes" 'node_type "project")))
      (should (= (alist-get 'total projects) 1))
      (should (eq (alist-get 'stale (car (alist-get 'nodes projects))) :json-false)))
    (should (= (alist-get 'total (orsb-tools-test--data "list_nodes" 'status "done")) 0))
    (should (= (alist-get 'total (orsb-tools-test--data "list_nodes" 'tags ["backlog"])) 1))
    (let ((p (orsb-tools-test--data "get_projects")))
      (should (equal (alist-get 'next_action (car (alist-get 'projects p))) "Phase 4 status curation.")))
    (let ((stale (orsb-tools-test--data "get_projects" 'stale t 'days_threshold 0)))
      (should (= (alist-get 'total stale) 1)))))

(ert-deftest orsb-tools-search-title-and-contextual ()
  (orsb-test-with-vault
    (let ((t1 (orsb-tools-test--data "search" 'query "backlog" 'mode "title")))
      (should (= (alist-get 'total t1) 1))
      (should (equal (alist-get 'id (car (alist-get 'hits t1))) "1784854105")))
    (let ((c (orsb-tools-test--data "search" 'query "central index" 'mode "contextual")))
      (should (>= (alist-get 'total c) 1))
      (should (string-match-p "Next Actions" (alist-get 'snippet (car (alist-get 'hits c))))))
    (should (= (alist-get 'total (orsb-tools-test--data "search" 'query "zzzz-nothing" 'mode "title")) 0))
    (should (equal (alist-get 'code (orsb-tools-test--error "search" 'query "x" 'mode "magic")) "invalid_argument"))))

;;;; create / heading / links / delete

(ert-deftest orsb-tools-create-heading-link-delete ()
  (orsb-test-with-vault
    (let* ((created (orsb-tools-test--data "create_node" 'node_type "project" 'title "Rebuild the shed"
                                           'body "Needs a roof." 'next_action "Buy lumber" 'tags ["home"]))
           (id (alist-get 'id created)))
      (should (equal (alist-get 'node_type created) "project"))
      (should (equal (alist-get 'status created) "active"))
      (should (equal (alist-get 'NEXT-ACTION (alist-get 'properties created)) "Buy lumber"))
      (should (string-prefix-p "projects/" (alist-get 'file created)))
      (should (member "home" (alist-get 'tags created)))
      (let ((h (orsb-tools-test--data "add_heading" 'id id 'heading "Materials" 'body "- lumber" 'todo "TODO")))
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

;;;; daily / inbox / sync

(ert-deftest orsb-tools-daily-inbox-sync ()
  (orsb-test-with-vault
    (let ((org-roam-dailies-directory "daily/"))
      (orsb-tools-test--data "add_daily_entry" 'title "Kai did a thing" 'points ["one" "two"] 'tags ["kai"])
      (let ((d (orsb-tools-test--data "get_daily")))
        (should (string-match-p "Kai did a thing" (alist-get 'content d)))
        (should (string-match-p "- one" (alist-get 'content d))))
      (orsb-tools-test--data "log_to_inbox" 'text "file this" 'linked_id "1784854105")
      (should (string-match-p "file this" (alist-get 'content (orsb-tools-test--data "get_daily")))))
    (should (equal (alist-get 'synced (orsb-tools-test--data "sync" 'id "1784854105")) "projects/project-backlog-1784854105.org"))
    (should (eq (alist-get 'queued (orsb-tools-test--data "sync")) t))
    (should (equal (alist-get 'synced (orsb-tools-test--data "sync" 'wait t 'full t)) "db"))))

;;;; missing args: JSON-RPC -32602 before dispatch

(ert-deftest orsb-tools-missing-required-arg-is-rpc-error ()
  (orsb-test-with-vault
    (orsb-tools-test--ensure-registered)
    (let ((rpc (orsb-test--json (org-roam-mcp-http--handle-tools-call '((name . "get_node") (arguments . ()))))))
      (should (= (alist-get 'code (alist-get 'error rpc)) -32602)))))

;;;; status migration

(ert-deftest orsb-tools-migrate-status-report-and-apply ()
  (orsb-test-with-vault
    (orsb-test--write "projects/old-1.org" ":PROPERTIES:\n:ID: m-1\n:NODE-TYPE: project\n:STATUS: completed\n:END:\n#+title: Old One\n")
    (orsb-test--write "projects/old-2.org" ":PROPERTIES:\n:ID: m-2\n:NODE-TYPE: project\n:END:\n#+title: Old Two\n#+STATUS: shelved\n")
    (orsb-test--write "projects/old-3.org" ":PROPERTIES:\n:ID: m-3\n:NODE-TYPE: project\n:STATUS: pass1-verified\n:END:\n#+title: Old Three\n")
    (orsb-test--write "blog/post.org" ":PROPERTIES:\n:ID: b-1\n:NODE-TYPE: blog\n:STATUS: published\n:END:\n#+title: A Post\n")
    (org-roam-db-sync)
    (let ((report (orsb-migrate-status)))
      (should (string-match-p "Would rewrite 2" report))
      (should (string-match-p "Old One: \"completed\" -> \"done\"" report))
      (should (string-match-p "Old Two: \"shelved\" -> \"someday\" (from #\\+STATUS:)" report))
      (should (string-match-p "1 unrecognized" report))
      (should (string-match-p "1 blog note(s) keep their own lifecycle" report)))
    (orsb-migrate-status t)
    (should (string-match-p "^:STATUS: +done$" (orsb-test--file "projects/old-1.org")))
    (let ((two (orsb-test--file "projects/old-2.org")))
      (should (string-match-p "^:STATUS: +someday$" two))
      (should-not (string-match-p "^#\\+STATUS:" two)))
    (should (string-match-p "pass1-verified" (orsb-test--file "projects/old-3.org")))
    (should (string-match-p "^:STATUS: +published$" (orsb-test--file "blog/post.org")))
    ;; blog nodes validate against their own vocabulary
    (should (equal (alist-get 'status (orsb-tools-test--data "set_node" 'id "b-1" 'status "draft")) "draft"))
    (should (equal (alist-get 'code (orsb-tools-test--error "set_node" 'id "b-1" 'status "done")) "invalid_argument"))
    (should (equal (alist-get 'code (orsb-tools-test--error "set_node" 'id "m-1" 'status "published")) "invalid_argument"))))

(provide 'orsb-tools-test)
;;; orsb-tools-test.el ends here
