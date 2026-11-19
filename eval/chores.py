"""Everyday vault chores for the small-model test.

Each chore is a dict:

  name       short id, used on the command line (--chores) and in results
  prompt     what the user says to the model, in plain words; it never names
             a tool, an id or a file, so the model has to find things itself
  setup      tool calls the harness makes before the model starts (optional),
             e.g. the bad edit the "undo" chore asks the model to reverse
  checks     the expected end state, see vaultcheck.py for the check types
  may_touch  glob patterns (relative to the vault) of files the chore is
             allowed to change; any other change, addition or removal fails
             the chore's "no collateral damage" check
  reference  a known good way to do the chore, as tool calls; run with
             --scripted to test the harness and scoring without a model.
             "$setup.N.key" is replaced by field key of setup call N's result.
  reference_answer  the final reply the scripted run gives
  needs      optional: "embeddings" when the chore only makes sense with
             semantic search (skipped with --no-embeddings)

All chores run on a fresh copy of eval/vault, one at a time.  The seed notes
use fixed ids (eval-garden, eval-coop ...) so checks can find them.
"""


def chores(today):
    """The chore list.  TODAY is YYYY-MM-DD, for the daily note."""
    daily = f"daily/{today}.org"
    return [
        {
            "name": "capture_idea",
            "prompt": "I just had an idea: build a solar food dehydrator out of an old storm window. "
                      "Save it as an idea note and connect it to my Garden project.",
            "checks": [
                {"type": "note_exists", "note": {"title_re": "dehydrat", "type": "idea"}},
                {"type": "in_dir", "note": {"title_re": "dehydrat"}, "dir": "ideas"},
                {"type": "link", "from": {"title_re": "dehydrat"}, "to": {"id": "eval-garden"}, "either_way": True},
            ],
            "may_touch": ["ideas/*", "projects/garden.org"],
            "reference": [
                {"tool": "search", "args": {"query": "solar dehydrator", "mode": "title"}},
                {"tool": "create_node", "args": {"title": "Solar food dehydrator", "node_type": "idea",
                                                 "one_liner": "A food dehydrator built from an old storm window"}},
                {"tool": "link_nodes", "args": {"id": "Solar food dehydrator", "target_id": "eval-garden"}},
            ],
            "reference_answer": "Saved the idea as 'Solar food dehydrator' and linked it to Garden.",
        },
        {
            "name": "link_person",
            "prompt": "Sam Lee is going to help me build the chicken coop. Connect the coop project to Sam's note.",
            "checks": [
                {"type": "link", "from": {"id": "eval-coop"}, "to": {"id": "eval-sam"}},
            ],
            "may_touch": ["projects/chicken-coop.org", "people/sam-lee.org"],
            "reference": [
                {"tool": "search", "args": {"query": "chicken coop", "mode": "title"}},
                {"tool": "link_nodes", "args": {"id": "eval-coop", "target_id": "eval-sam"}},
            ],
            "reference_answer": "Linked Chicken coop to Sam Lee.",
        },
        {
            "name": "merge_duplicate",
            "prompt": "I think I ended up with two notes about collecting rainwater. Merge them into one, "
                      "keeping 'Rain barrel system' as the note that stays.",
            "checks": [
                {"type": "file_absent", "path": "reference/rainwater-collection.org"},
                {"type": "contains", "note": {"id": "eval-rain-1"}, "text": "IBC"},
                {"type": "contains", "note": {"id": "eval-rain-1"}, "text": "first flush diverter"},
                # the Garden note links to the old note; that link must still work
                {"type": "id_resolves", "id": "eval-rain-2", "in_path": "reference/rain-barrel-system.org"},
            ],
            "may_touch": ["reference/rain-barrel-system.org", "reference/rainwater-collection.org",
                          "projects/garden.org", "archive/*"],
            "reference": [
                {"tool": "search", "args": {"query": "rain", "mode": "title"}},
                {"tool": "merge_nodes", "args": {"id": "eval-rain-2", "target_id": "eval-rain-1"}},
            ],
            "reference_answer": "Merged 'Rainwater collection' into 'Rain barrel system'.",
        },
        {
            "name": "overdue_tasks",
            "prompt": "What tasks are overdue right now?",
            "checks": [
                {"type": "answer", "all_re": [r"gate"],
                 "none_re": [r"pick a spot", r"pour(ed)? the footings", r"annual furnace service"]},
            ],
            "may_touch": [],
            "reference": [
                {"tool": "get_tasks", "args": {"due_before": "today"}},
            ],
            "reference_answer": "One task is overdue: Fix the fence gate (deadline 2025-06-01), in Fence repair.",
        },
        {
            "name": "tick_checkbox",
            "prompt": "I bought the cedar boards for the raised bed. Tick that off.",
            "checks": [
                {"type": "checkbox", "note": {"id": "eval-garden"}, "text": "buy cedar boards", "checked": True},
                {"type": "checkbox", "note": {"id": "eval-garden"}, "text": "buy screws", "checked": False},
                {"type": "checkbox", "note": {"id": "eval-garden"}, "text": "measure the space", "checked": True},
            ],
            "may_touch": ["projects/garden.org"],
            "reference": [
                {"tool": "get_tasks", "args": {"query": "cedar"}},
                {"tool": "set_checkbox", "args": {"id": "eval-garden", "item": "buy cedar boards"}},
            ],
            "reference_answer": "Ticked off 'buy cedar boards' under Build the raised bed.",
        },
        {
            "name": "close_todo",
            "prompt": "I replaced the furnace filter this morning. Mark that task done.",
            "checks": [
                {"type": "heading", "note": {"id": "eval-furnace"}, "title_re": "replace the furnace filter",
                 "todo": "DONE"},
                {"type": "heading", "note": {"id": "eval-furnace"}, "title_re": "annual furnace service",
                 "todo": "DONE"},
            ],
            "may_touch": ["admin/furnace.org", "daily/*"],
            "reference": [
                {"tool": "get_tasks", "args": {"query": "furnace filter"}},
                {"tool": "set_node", "args": {"id": "eval-furnace", "heading": "Replace the furnace filter",
                                              "todo": "DONE"}},
            ],
            "reference_answer": "Marked 'Replace the furnace filter' done.",
        },
        {
            "name": "move_task",
            "prompt": "The 'Install chicken waterer' task ended up in my Garden project by mistake. "
                      "Move it to the Chicken coop project.",
            "checks": [
                {"type": "heading", "note": {"id": "eval-coop"}, "title_re": "install chicken waterer", "todo": "TODO"},
                {"type": "contains", "note": {"id": "eval-coop"}, "text": "five gallon bucket"},
                {"type": "heading", "note": {"id": "eval-garden"}, "title_re": "install chicken waterer", "absent": True},
                {"type": "heading", "note": {"id": "eval-garden"}, "title_re": "turn the compost pile", "todo": "TODO"},
            ],
            "may_touch": ["projects/garden.org", "projects/chicken-coop.org"],
            "reference": [
                {"tool": "get_node", "args": {"id": "Garden", "include_body": False}},
                {"tool": "move_node", "args": {"id": "eval-waterer", "target_id": "eval-coop", "section": "Tasks"}},
            ],
            "reference_answer": "Moved 'Install chicken waterer' into Chicken coop under Tasks.",
        },
        {
            "name": "undo_bad_edit",
            "setup": [
                {"tool": "update_body", "args": {"id": "eval-well", "content": "Pump notes go here.",
                                                 "mode": "replace", "force": True}},
            ],
            "prompt": "I just wiped out most of my Well pump note with a bad edit. Please undo that change.",
            "checks": [
                {"type": "contains", "note": {"id": "eval-well"}, "text": "Grundfos SQ 2-85"},
                {"type": "contains", "note": {"id": "eval-well"}, "text": "breaker is number 14"},
                {"type": "not_contains", "note": {"id": "eval-well"}, "text": "Pump notes go here"},
            ],
            "may_touch": ["reference/well-pump.org"],
            "reference": [
                {"tool": "restore", "args": {}},
                {"tool": "restore", "args": {"change_id": "$setup.0.change_id"}},
            ],
            "reference_answer": "Undid the edit; the Well pump note is back as it was.",
        },
        {
            "name": "answer_from_vault",
            "prompt": "What pressure is my well pump's pressure switch set to?",
            "checks": [
                {"type": "answer", "all_re": [r"\b40\b", r"\b60\b"]},
            ],
            "may_touch": [],
            "reference": [
                {"tool": "search", "args": {"query": "well pump pressure switch", "mode": "contextual"}},
                {"tool": "get_node", "args": {"id": "eval-well"}},
            ],
            "reference_answer": "40/60 psi: it cuts in at 40 and out at 60.",
        },
        {
            "name": "add_task",
            "prompt": "Add a task to my Chicken coop project: buy hardware cloth for the run, due by March 1, 2027.",
            "checks": [
                {"type": "heading", "note": {"id": "eval-coop"}, "title_re": "hardware cloth",
                 "todo": "TODO", "deadline": "2027-03-01"},
            ],
            "may_touch": ["projects/chicken-coop.org"],
            "reference": [
                {"tool": "create_node", "args": {"title": "Buy hardware cloth for the run", "parent_id": "eval-coop",
                                                 "section": "Tasks", "todo": "TODO", "deadline": "2027-03-01"}},
            ],
            "reference_answer": "Added TODO 'Buy hardware cloth for the run', deadline 2027-03-01, to Chicken coop.",
        },
        {
            "name": "set_status",
            "prompt": "The fence repair is on hold until my neighbor gets back to me. Update that project's status.",
            "checks": [
                {"type": "property", "note": {"id": "eval-fence"}, "key": "STATUS", "equals": "waiting"},
            ],
            "may_touch": ["projects/fence-repair.org"],
            "reference": [
                {"tool": "get_schema", "args": {}},
                {"tool": "set_node", "args": {"id": "eval-fence", "status": "waiting"}},
            ],
            "reference_answer": "Set Fence repair to waiting.",
        },
        {
            "name": "inbox_capture",
            "prompt": "Quick note so I don't forget: call the county about the property survey.",
            "checks": [
                {"type": "contains", "note": {"path": daily}, "regex": r"county.*survey|survey.*county"},
            ],
            "may_touch": ["daily/*"],
            "reference": [
                {"tool": "log_to_inbox", "args": {"text": "Call the county about the property survey",
                                                  "category": "task"}},
            ],
            "reference_answer": "Added it to today's inbox.",
        },
        {
            "name": "link_related",
            "prompt": "Look for notes related to my Chicken coop project that aren't connected to it yet, "
                      "and link the ones that really belong from the coop note.",
            "checks": [
                {"type": "link", "from": {"id": "eval-coop"}, "to": {"id": "eval-eggs"}},
                # precision: these share no real topic with the coop
                {"type": "not_contains", "note": {"id": "eval-coop"}, "regex": r"id:eval-(well|router|furnace|fence)\]"},
            ],
            "may_touch": ["projects/chicken-coop.org", "notes/egg-production-log.org"],
            "reference": [
                {"tool": "find_related", "args": {"id": "eval-coop"}},
                {"tool": "search", "args": {"query": "hens", "mode": "contextual"}},
                {"tool": "link_nodes", "args": {"id": "eval-coop", "target_id": "eval-eggs"}},
            ],
            "reference_answer": "Linked Egg production log from Chicken coop.",
        },
        {
            "name": "archive_note",
            "prompt": "I don't need my old router notes any more. Put that note away, but don't delete it for good.",
            "checks": [
                {"type": "file_absent", "path": "reference/old-router.org"},
                {"type": "id_resolves", "id": "eval-router"},
                {"type": "in_dir", "note": {"id": "eval-router"}, "dir": "archive"},
            ],
            "may_touch": ["reference/old-router.org", "archive/*"],
            "reference": [
                {"tool": "search", "args": {"query": "router", "mode": "title"}},
                {"tool": "delete_node", "args": {"id": "eval-router"}},
            ],
            "reference_answer": "Archived 'Old router notes'.",
        },
    ]
