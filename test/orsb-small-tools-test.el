;;; orsb-small-tools-test.el --- ERT tests for the 2.1 small-model tools -*- lexical-binding: t; -*-

;;; Commentary:
;; The tools added or reshaped in 2.1: tasks and checkboxes, dry runs and
;; restore, duplicate checks, rename propagation, move / promote / merge,
;; inbox refs, the review, find_related and the hints on errors.  Driven
;; through the JSON-RPC handler like orsb-tools-test.el.

;;; Code:

(require 'ert)
(require 'org-roam)
(require 'orsb-core)
(require 'orsb-search)
(require 'orsb-tools)
(require 'org-roam-mcp-http)
(require 'orsb-core-test)
(require 'orsb-tools-test)
(require 'orsb-search-test)

(defalias 'orsb-st--data #'orsb-tools-test--data)
(defalias 'orsb-st--error #'orsb-tools-test--error)

(defconst orsb-st--tasks-file "\
:PROPERTIES:
:ID: t-1
:NODE-TYPE: project
:STATUS: active
:END:
#+title: Garden
#+TODO: TODO WAITING | DONE CANCELLED

* Beds
** TODO Build the raised bed
DEADLINE: <2020-01-01 Wed>
- [ ] buy cedar boards
- [ ] buy screws
- [X] measure the space
** WAITING Soil delivery from [[id:p-1][Sam Lee]]
SCHEDULED: <2099-05-01 Fri>
** DONE Pick a spot
* Notes
- [ ] ask Sam about compost
- [ ] buy screws for the gate
")

(defconst orsb-st--person-file "\
:PROPERTIES:
:ID: p-1
:NODE-TYPE: person
:END:
#+title: Sam Lee

Neighbor; has a truck.
")

(defmacro orsb-st--with-garden (&rest body)
  "BODY in a vault that also holds the garden and person notes."
  (declare (indent 0))
  `(orsb-test-with-vault
     (orsb-test--write "projects/garden.org" orsb-st--tasks-file)
     (orsb-test--write "people/sam-lee.org" orsb-st--person-file)
     (org-roam-db-sync)
     (orsb-tools-test--ensure-registered)
     ,@body))

(defun orsb-st--texts (data key)
  "The text of each item in DATA's KEY list."
  (mapcar (lambda (x) (alist-get 'text x)) (alist-get key data)))

;;;; tasks and checkboxes

(ert-deftest orsb-st-get-tasks-filters ()
  (orsb-st--with-garden
    (let* ((all (orsb-st--data "get_tasks"))
           (texts (orsb-st--texts all 'tasks)))
      ;; open TODO headings (custom keywords) and unchecked boxes; not DONE, not [X]
      (should (member "Build the raised bed" texts))
      (should (seq-find (lambda (x) (string-prefix-p "Soil delivery" x)) texts))
      (should (member "buy cedar boards" texts))
      (should-not (member "Pick a spot" texts))
      (should-not (member "measure the space" texts))
      ;; dated tasks first, overdue at the top
      (should (equal (car texts) "Build the raised bed"))
      ;; checkboxes under a heading inherit its deadline and carry an index
      (let ((boards (seq-find (lambda (x) (equal (alist-get 'text x) "buy cedar boards")) (alist-get 'tasks all))))
        (should (equal (alist-get 'deadline boards) "2020-01-01"))
        (should (equal (alist-get 'id boards) "t-1"))
        (should (integerp (alist-get 'index boards)))))
    (should (equal (orsb-st--texts (orsb-st--data "get_tasks" 'kind "todo" 'due_before "today") 'tasks)
                   '("Build the raised bed")))
    (should (equal (orsb-st--texts (orsb-st--data "get_tasks" 'state "WAITING") 'tasks)
                   '("Soil delivery from [[id:p-1][Sam Lee]]")))
    ;; by person: the link (by id) or the full name; a bare first name is not enough
    (should (= (length (alist-get 'tasks (orsb-st--data "get_tasks" 'mentions "Sam Lee"))) 1))
    (should (= (length (alist-get 'tasks (orsb-st--data "get_tasks" 'mentions "Sam"))) 2))
    (should (= (length (alist-get 'tasks (orsb-st--data "get_tasks" 'query "screws"))) 2))
    (should (member "Pick a spot" (orsb-st--texts (orsb-st--data "get_tasks" 'state "done") 'tasks)))
    (let ((page (orsb-st--data "get_tasks" 'limit 2)))
      (should (= (alist-get 'returned page) 2))
      (should (= (alist-get 'next_offset page) 2)))))

(ert-deftest orsb-st-set-checkbox-and-close-todo ()
  (orsb-st--with-garden
    ;; ambiguous text: an error that lists the candidates and says how to pick
    (let ((err (orsb-st--error "set_checkbox" 'id "t-1" 'item "screws")))
      (should (equal (alist-get 'code err) "invalid_argument"))
      (should (string-match-p "buy screws for the gate" (alist-get 'message err)))
      (should (string-match-p "index" (alist-get 'hint err))))
    ;; an exact match wins over partial ones
    (let ((r (orsb-st--data "set_checkbox" 'id "t-1" 'item "buy screws")))
      (should (equal (alist-get 'item r) "buy screws"))
      (should (eq (alist-get 'changed r) t))
      (should (equal (alist-get 'progress r) "2/5"))
      (should (alist-get 'change_id r)))
    (should (string-match-p "- \\[X\\] buy screws$" (orsb-test--file "projects/garden.org")))
    ;; by index, as get_tasks reports it
    (let* ((gate (seq-find (lambda (x) (equal (alist-get 'text x) "buy screws for the gate"))
                           (alist-get 'tasks (orsb-st--data "get_tasks"))))
           (r (orsb-st--data "set_checkbox" 'id "t-1" 'index (alist-get 'index gate))))
      (should (equal (alist-get 'item r) "buy screws for the gate")))
    ;; a missing item lists what is there
    (let ((err (orsb-st--error "set_checkbox" 'id "t-1" 'item "plant tomatoes")))
      (should (equal (alist-get 'code err) "not_found"))
      (should (string-match-p "cedar" (alist-get 'hint err))))
    ;; untick
    (orsb-st--data "set_checkbox" 'id "t-1" 'item "buy screws" 'checked :json-false)
    (should (string-match-p "- \\[ \\] buy screws$" (orsb-test--file "projects/garden.org")))
    ;; close a TODO heading that has no :ID: yet, by its title
    (let ((r (orsb-st--data "set_node" 'id "t-1" 'heading "Build the raised bed" 'todo "DONE")))
      (should (equal (alist-get 'todo r) "DONE")))
    (should (string-match-p "\\*\\* DONE Build the raised bed" (orsb-test--file "projects/garden.org")))
    (should-not (member "Build the raised bed" (orsb-st--texts (orsb-st--data "get_tasks" 'kind "todo") 'tasks)))))

;;;; dry run and restore

(ert-deftest orsb-st-dry-run-change-id-and-restore ()
  (orsb-st--with-garden
    (let* ((file "projects/project-backlog-1784854105.org")
           (before (orsb-test--file file))
           (dry (orsb-st--data "update_body" 'id "1784854105" 'section "Purpose" 'content "dry text" 'dry_run t)))
      (should (eq (alist-get 'dry_run dry) t))
      (should-not (alist-get 'change_id dry))
      (should (equal (orsb-test--file file) before))
      (let ((preview (car (alist-get 'would_change dry))))
        (should (equal (alist-get 'action preview) "edit"))
        (should (member "dry text" (alist-get 'lines_added preview))))
      ;; nothing lingers in the buffer either
      (should-not (string-match-p "dry text" (alist-get 'body (orsb-st--data "get_node" 'id "1784854105"))))
      ;; the real write
      (let* ((w (orsb-st--data "update_body" 'id "1784854105" 'section "Purpose" 'content "real text"))
             (cid (alist-get 'change_id w)))
        (should (stringp cid))
        (should (< (alist-get 'chars_before w) (alist-get 'chars_after w)))
        (should (string-match-p "real text" (orsb-test--file file)))
        ;; listed
        (should (equal (alist-get 'change_id (car (alist-get 'changes (orsb-st--data "restore")))) cid))
        ;; a restore dry run changes nothing
        (orsb-st--data "restore" 'change_id cid 'dry_run t)
        (should (string-match-p "real text" (orsb-test--file file)))
        ;; restore
        (let ((r (orsb-st--data "restore" 'change_id cid)))
          (should (equal (orsb-test--file file) before))
          ;; and the restore itself can be undone
          (orsb-st--data "restore" 'change_id (alist-get 'change_id r))
          (should (string-match-p "real text" (orsb-test--file file))))
        ;; restoring the first change again now conflicts with the newer state
        (orsb-st--data "update_body" 'id "1784854105" 'content "later edit")
        (let ((err (orsb-st--error "restore" 'change_id cid)))
          (should (equal (alist-get 'code err) "refused"))
          (should (string-match-p "force" (alist-get 'hint err))))
        (orsb-st--data "restore" 'change_id cid 'force t)
        (should (equal (orsb-test--file file) before))))
    (should (equal (alist-get 'code (orsb-st--error "restore" 'change_id "nope")) "not_found"))))

(ert-deftest orsb-st-update-body-missing-section ()
  (orsb-st--with-garden
    (let ((err (orsb-st--error "update_body" 'id "1784854105" 'section "Ideas" 'content "x")))
      (should (equal (alist-get 'code err) "not_found"))
      (should (string-match-p "Next Actions" (alist-get 'hint err)))
      (should (string-match-p "create_section" (alist-get 'hint err))))
    (orsb-st--data "update_body" 'id "1784854105" 'section "Ideas" 'content "x" 'create_section t)
    (should (string-match-p "^\\* Ideas" (orsb-test--file "projects/project-backlog-1784854105.org")))))

;;;; create, set, rename

(ert-deftest orsb-st-create-node-duplicates-and-headings ()
  (orsb-st--with-garden
    (let ((r (orsb-st--data "create_node" 'title "garden")))
      (should (eq (alist-get 'created r) :json-false))
      (should (equal (alist-get 'id (car (alist-get 'possible_duplicates r))) "t-1")))
    (should-not (file-exists-p (expand-file-name "notes" org-roam-directory)))
    (should (equal (alist-get 'id (orsb-st--data "create_node" 'title "Garden" 'on_duplicate "use_existing")) "t-1"))
    (let ((r (orsb-st--data "create_node" 'title "Garden" 'on_duplicate "create")))
      (should (eq (alist-get 'created r) t))
      (should-not (equal (alist-get 'id r) "t-1")))
    ;; a nested task heading with planning
    (let ((h (orsb-st--data "create_node" 'parent_id "t-1" 'section "Beds" 'title "Order mulch"
                            'todo "TODO" 'deadline "2099-06-01")))
      (should (= (alist-get 'level h) 2))
      (should (equal (alist-get 'todo h) "TODO")))
    (should (string-match-p "\\*\\* TODO Order mulch\nDEADLINE: <2099-06-01 [A-Za-z]+>"
                            (orsb-test--file "projects/garden.org")))
    ;; same heading under the same parent is a duplicate too
    (should (eq (alist-get 'created (orsb-st--data "create_node" 'parent_id "t-1" 'title "Order mulch")) :json-false))
    ;; planning on a new note is refused with a hint
    (should (string-match-p "parent_id" (alist-get 'hint (orsb-st--error "create_node" 'title "New" 'deadline "2099-01-01"))))))

(ert-deftest orsb-st-set-node-rename-aliases-planning ()
  (orsb-st--with-garden
    ;; rename rewrites [[id:..][Old]] link text elsewhere
    (let ((r (orsb-st--data "set_node" 'id "p-1" 'title "Sam Leeds")))
      (should (member "projects/garden.org" (alist-get 'links_updated r))))
    (should (string-match-p "\\[\\[id:p-1\\]\\[Sam Leeds\\]\\]" (orsb-test--file "projects/garden.org")))
    ;; aliases, then resolvable by alias
    (orsb-st--data "set_node" 'id "p-1" 'aliases_add ["Sammy" "S. Lee"])
    (should (equal (alist-get 'id (orsb-st--data "get_node" 'id "Sammy")) "p-1"))
    (orsb-st--data "set_node" 'id "p-1" 'aliases_remove ["Sammy"])
    (should (equal (alist-get 'aliases (orsb-st--data "get_node" 'id "p-1")) '("S. Lee")))
    ;; schedule a heading; a whole note gets a hint instead
    (let ((h (orsb-st--data "create_node" 'parent_id "t-1" 'title "Water")))
      (orsb-st--data "set_node" 'id (alist-get 'id h) 'scheduled "2099-07-04")
      (should (equal (alist-get 'scheduled (orsb-st--data "get_node" 'id (alist-get 'id h))) "2099-07-04"))
      (orsb-st--data "set_node" 'id (alist-get 'id h) 'scheduled "")
      (should-not (alist-get 'scheduled (orsb-st--data "get_node" 'id (alist-get 'id h)))))
    (should (string-match-p "DUE-DATE" (alist-get 'hint (orsb-st--error "set_node" 'id "t-1" 'deadline "2099-01-01"))))))

;;;; links

(ert-deftest orsb-st-link-nodes-idempotent-both-ways-precise-remove ()
  (orsb-st--with-garden
    (should (equal (alist-get 'action (orsb-st--data "link_nodes" 'id "t-1" 'target_id "1784854105" 'both_ways t)) "added"))
    (let ((again (orsb-st--data "link_nodes" 'id "t-1" 'target_id "1784854105")))
      (should (equal (alist-get 'action again) "exists"))
      (should-not (alist-get 'change_id again)))
    (should (string-match-p "\\[\\[id:t-1\\]" (orsb-test--file "projects/project-backlog-1784854105.org")))
    ;; a link inside a sentence: the words stay, only the link goes
    (orsb-st--data "link_nodes" 'id "t-1" 'target_id "p-1" 'action "remove")
    (let ((text (orsb-test--file "projects/garden.org")))
      (should (string-match-p "Soil delivery from Sam Lee$" text))
      (should-not (string-match-p "id:p-1" text)))))

;;;; move, promote, merge, delete

(ert-deftest orsb-st-move-and-promote ()
  (orsb-st--with-garden
    (let* ((h (orsb-st--data "create_node" 'parent_id "t-1" 'section "Notes" 'title "Compost plan" 'body "Three bins."))
           (hid (alist-get 'id h))
           (garden (orsb-test--file "projects/garden.org")))
      ;; dry run: nothing moves
      (orsb-st--data "move_node" 'id hid 'target_id "1784854105" 'section "Purpose" 'dry_run t)
      (should (equal (orsb-test--file "projects/garden.org") garden))
      ;; move under Purpose: level follows the new parent
      (let ((m (orsb-st--data "move_node" 'id hid 'target_id "1784854105" 'section "Purpose")))
        (should (equal (alist-get 'action m) "moved"))
        (should-not (string-match-p "Compost plan" (orsb-test--file "projects/garden.org")))
        (should (string-match-p "\\*\\* Compost plan" (orsb-test--file "projects/project-backlog-1784854105.org")))
        (should (equal (alist-get 'file (orsb-st--data "get_node" 'id hid)) "projects/project-backlog-1784854105.org"))
        ;; restore puts it back
        (orsb-st--data "restore" 'change_id (alist-get 'change_id m))
        (should (equal (orsb-test--file "projects/garden.org") garden)))
      ;; promote to a note of its own, keeping the id
      (let ((p (orsb-st--data "move_node" 'id hid 'to_note t)))
        (should (equal (alist-get 'action p) "promoted"))
        (let ((n (orsb-st--data "get_node" 'id hid)))
          (should (= (alist-get 'level n) 0))
          (should (equal (alist-get 'node_type n) "note"))
          (should (string-match-p "Three bins" (alist-get 'body n))))
        ;; the new file holds the heading's body once, with one :ID:
        (let ((text (orsb-test--file (alist-get 'file p))))
          (should (= (with-temp-buffer (insert text) (count-matches ":ID:" (point-min) (point-max))) 1)))
        ;; a link was left where it was
        (should (string-match-p (concat "- \\[\\[id:" (regexp-quote hid) "\\]\\[Compost plan\\]\\]")
                                (orsb-test--file "projects/garden.org")))))
    (should (string-match-p "merge_nodes" (alist-get 'hint (orsb-st--error "move_node" 'id "t-1" 'target_id "p-1"))))))

(ert-deftest orsb-st-merge-keeps-links-and-restores ()
  (orsb-st--with-garden
    (let ((garden (orsb-test--file "projects/garden.org"))
          (person (orsb-test--file "people/sam-lee.org")))
      ;; garden links to p-1; merge the person note into the backlog project
      (let ((m (orsb-st--data "merge_nodes" 'id "p-1" 'target_id "1784854105")))
        (should (= (alist-get 'links_kept m) 1))
        (should-not (file-exists-p (expand-file-name "people/sam-lee.org" org-roam-directory)))
        ;; the old id now names a heading in the target, so the link still resolves
        (let ((n (orsb-st--data "get_node" 'id "p-1")))
          (should (= (alist-get 'level n) 1))
          (should (equal (alist-get 'file n) "projects/project-backlog-1784854105.org"))
          (should (string-match-p "has a truck" (alist-get 'body n))))
        (orsb-st--data "restore" 'change_id (alist-get 'change_id m))
        (should (equal (orsb-test--file "people/sam-lee.org") person))
        (should (equal (orsb-test--file "projects/garden.org") garden))
        (should (= (alist-get 'level (orsb-st--data "get_node" 'id "p-1")) 0))
        (should-not (string-match-p "has a truck" (orsb-test--file "projects/project-backlog-1784854105.org")))))))

(ert-deftest orsb-st-delete-archives-by-default ()
  (orsb-st--with-garden
    (let ((d (orsb-st--data "delete_node" 'id "p-1")))
      (should (equal (alist-get 'action d) "archived"))
      (should (equal (alist-get 'id (car (alist-get 'backlinks d))) "t-1"))
      (orsb-st--data "restore" 'change_id (alist-get 'change_id d))
      (should (file-exists-p (expand-file-name "people/sam-lee.org" org-roam-directory)))
      (should (equal (alist-get 'file (orsb-st--data "get_node" 'id "p-1")) "people/sam-lee.org")))))

;;;; inbox and review

(ert-deftest orsb-st-inbox-refs-resolve-and-review ()
  (orsb-st--with-garden
    (let* ((a (orsb-st--data "log_to_inbox" 'text "call the plumber"))
           (b (orsb-st--data "log_to_inbox" 'text "mulch prices")))
      (should (string-match-p "\\`[0-9-]+:1\\'" (alist-get 'ref a)))
      (should (string-match-p ":2\\'" (alist-get 'ref b)))
      (let ((inbox (alist-get 'inbox (orsb-st--data "get_review"))))
        (should (= (alist-get 'total inbox) 2)))
      ;; file the second into the garden note
      (let ((r (orsb-st--data "resolve_inbox" 'ref (alist-get 'ref b) 'target_id "t-1" 'section "Notes")))
        (should (eq (alist-get 'resolved r) t)))
      (should (string-match-p "- mulch prices" (orsb-test--file "projects/garden.org")))
      (let ((inbox (alist-get 'inbox (orsb-st--data "get_review"))))
        (should (= (alist-get 'total inbox) 1))
        (should (equal (alist-get 'ref (car (alist-get 'items inbox))) (alist-get 'ref a))))
      ;; resolving twice is refused
      (should (equal (alist-get 'code (orsb-st--error "resolve_inbox" 'ref (alist-get 'ref b))) "refused")))))

(ert-deftest orsb-st-review-health-and-list-filters ()
  (orsb-st--with-garden
    (orsb-test--write "notes/lonely.org" ":PROPERTIES:\n:ID: o-1\n:END:\n#+title: Lonely\n\nNo links.\n")
    (orsb-test--write "notes/broken.org" ":PROPERTIES:\n:ID: b-9\n:END:\n#+title: Broken\n\nSee [[id:missing-1][gone]].\n")
    (orsb-test--write "notes/lonely-2.org" ":PROPERTIES:\n:ID: o-2\n:END:\n#+title: Lonely\n\nSame title.\n")
    (org-roam-db-sync)
    (let* ((review (orsb-st--data "get_review"))
           (health (alist-get 'health review)))
      (should (member "o-1" (mapcar (lambda (x) (alist-get 'id x)) (alist-get 'items (alist-get 'orphans health)))))
      (should (equal (alist-get 'missing_id (car (alist-get 'items (alist-get 'broken_links health)))) "missing-1"))
      (should (equal (alist-get 'title (car (alist-get 'items (alist-get 'duplicate_titles health)))) "lonely"))
      ;; the overdue raised bed is due
      (should (member "Build the raised bed" (orsb-st--texts (alist-get 'due_tasks review) 'items))))
    (should (member "o-1" (mapcar (lambda (n) (alist-get 'id n)) (alist-get 'nodes (orsb-st--data "list_nodes" 'orphan t)))))
    (should (equal (mapcar (lambda (n) (alist-get 'id n)) (alist-get 'nodes (orsb-st--data "list_nodes" 'broken_links t))) '("b-9")))
    (should (equal (mapcar (lambda (n) (alist-get 'id n)) (alist-get 'nodes (orsb-st--data "list_nodes" 'linked_to "p-1"))) '("t-1")))
    ;; an ambiguous title is an error naming both ids
    (let ((err (orsb-st--error "get_node" 'id "Lonely")))
      (should (equal (alist-get 'code err) "invalid_argument"))
      (should (string-match-p "o-1" (alist-get 'message err)))
      (should (string-match-p "o-2" (alist-get 'message err))))))

;;;; get_node

(ert-deftest orsb-st-get-node-outline-dates-and-truncation ()
  (orsb-st--with-garden
    (let ((n (orsb-st--data "get_node" 'id "t-1" 'max_chars 40)))
      (should (equal (mapcar (lambda (s) (alist-get 'title s)) (alist-get 'sections n))
                     '("Beds" "Build the raised bed" "Soil delivery from [[id:p-1][Sam Lee]]" "Pick a spot" "Notes")))
      (should (eq (alist-get 'truncated n) t))
      (should (= (length (alist-get 'body n)) 40))
      (should (string-match-p "section" (alist-get 'hint n))))
    (orsb-st--data "add_daily_entry" 'title "Older day" 'points ["x"] 'date "2026-01-02")
    (should (string-match-p "Older day" (alist-get 'body (orsb-st--data "get_node" 'id "2026-01-02"))))
    (should (string-match-p "add_daily_entry" (alist-get 'hint (orsb-st--error "get_node" 'id "2001-01-01"))))))

;;;; semantic: sections, snippets, find_related

(defconst orsb-st--vec-a "1.0 0.0 0.0 0.0")
(defconst orsb-st--vec-b "0.9 0.1 0.0 0.0")
(defconst orsb-st--vec-c "0.0 0.0 1.0 0.0")

(ert-deftest orsb-st-find-related-and-semantic-sections ()
  (orsb-test-with-vault
    (orsb-search-test--with-fake
      (orsb-test--write "notes/frigate.org"
                        (format ":PROPERTIES:\n:ID: v-1\n:END:\n#+title: Frigate\n\n* Storage\n:PROPERTIES:\n:ID: v-1s\n:EMBEDDING: %s\n:END:\nRecordings go to the NAS share.\n" orsb-st--vec-a))
      (orsb-test--write "notes/nas.org"
                        (format ":PROPERTIES:\n:ID: v-2\n:END:\n#+title: NAS\n\n* Shares\n:PROPERTIES:\n:ID: v-2s\n:EMBEDDING: %s\n:END:\nThe camera share holds the recordings.\n" orsb-st--vec-b))
      (orsb-test--write "notes/bread.org"
                        (format ":PROPERTIES:\n:ID: v-3\n:EMBEDDING: %s\n:END:\n#+title: Bread\n\nSourdough starter notes.\n" orsb-st--vec-c))
      (org-roam-db-sync)
      ;; vectors written the old way reach search through the migration
      (orsb-search-migrate-properties)
      (orsb-tools-test--ensure-registered)
      ;; by note: the centroid of its stored vectors, itself excluded
      (let* ((r (orsb-st--data "find_related" 'id "v-1" 'cutoff 0.5))
             (hit (car (alist-get 'related r))))
        (should (= (alist-get 'total r) 1))
        (should (equal (alist-get 'id hit) "v-2"))
        (should (eq (alist-get 'linked hit) :json-false))
        (should (equal (alist-get 'section hit) "Shares"))
        (should (string-match-p "camera share" (alist-get 'snippet hit))))
      ;; once linked, it says so
      (orsb-st--data "link_nodes" 'id "v-1" 'target_id "v-2")
      (should (eq (alist-get 'linked (car (alist-get 'related (orsb-st--data "find_related" 'id "v-1")))) t))
      ;; semantic search reports the matching section and its id
      (cl-letf (((symbol-function 'org-roam-ai-generate-embedding) (lambda (_) '(1.0 0.0 0.0 0.0))))
        (let ((hit (car (alist-get 'hits (orsb-st--data "search" 'query "where do recordings go")))))
          (should (equal (alist-get 'id hit) "v-1"))
          (should (equal (alist-get 'section hit) "Storage"))
          (should (equal (alist-get 'section_id hit) "v-1s"))
          (should (string-match-p "NAS share" (alist-get 'snippet hit))))))))

;;;; server guidance

(ert-deftest orsb-st-initialize-sends-instructions ()
  (let* ((rpc (orsb-test--json (org-roam-mcp-http--handle-initialize '((protocolVersion . "2025-03-26")))))
         (text (alist-get 'instructions (alist-get 'result rpc))))
    (should (stringp text))
    (should (string-match-p "restore" text)))
  (let ((orsb-mcp-instructions nil))
    (should-not (assq 'instructions (alist-get 'result (orsb-test--json (org-roam-mcp-http--handle-initialize nil)))))))

(provide 'orsb-small-tools-test)
;;; orsb-small-tools-test.el ends here
