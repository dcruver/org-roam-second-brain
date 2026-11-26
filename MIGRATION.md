# Migrating to the current MCP contract

## 2.2 → 3.0: the vault defines its own note types

3.0 is a breaking release. Nothing is kept for compatibility.

- **No built-in note types.** `orsb-directories` (and the alias
  `orsb-node-types`) are gone. A type is a folder with a `_schema.org` that
  sets `TYPE`; see the README. A vault without schema files has one type,
  `note`, created in the vault's top folder. To keep the 2.x layout, give
  each of your folders (`projects/`, `people/`, ...) a `_schema.org`.
- **`NODE-TYPE` is not read or written.** A note's type is where it lives.
  `node_type` in answers and in filters is the type of the note's folder.
- **Statuses come from the schema.** `orsb-status-values`,
  `orsb-blog-status-values`, `orsb-status-aliases` and `M-x
  orsb-migrate-status` are gone. A type names its lifecycle property and
  values (`LIFECYCLE`, `LIFECYCLE_VALUES`, `LIFECYCLE_FINISHED`). Old
  spellings are no longer mapped; a value is written as given and, with
  schema files, checked.
- **`create_node`** takes `title`, `node_type`, `status`, `properties`,
  `tags`, `body`. The arguments `next_action`, `due_date`, `one_liner`,
  `context`, `follow_ups` and `hugo_section` are gone: pass properties in
  `properties`. Skeleton headings, default properties and keyword lines come
  from the type's schema (`Template`, the `default` column, `Keywords`).
- **Blog support moved out of the core.** `orsb-hugo-base-dir` and
  `orsb-hugo-sections` are gone from the server; a blog type's schema supplies
  the export properties and `#+hugo_*` lines. The Emacs blog commands keep
  their own `sb/hugo-base-dir` and `sb/hugo-sections`.
- **People are not special.** `log_to_inbox` no longer creates a note for a
  `[[Name]]`, and its answer has no `created_people`. `get_review` has no
  `followups` and no `dangling_name_links`. `get_tasks` takes `mentions`
  where it took `person`.
- **`get_review`** returns `open` and `stale` (unfinished notes of every type
  that declares a lifecycle) where it returned `active_projects` and
  `stale_projects`. Without schema files both are empty.
- **`get_schema`** no longer returns `node_types`, `status_values`,
  `blog_status_values`, `status_aliases`, `property_keys`, `hugo_sections` or
  `blog_enabled`. It returns `types`, `schema_files` and `enforce`.
- **New tools:** `check_schema`, `get_proposals`, `decide_proposal`; every
  write tool takes `propose` and `reason`. Twenty-two tools in all.
- **`orsb-agenda-directories`** is a list of vault folders, not of types, and
  is empty by default.
- **`orsb-core-create-node`** is `(TITLE &rest ARGS)` with `:dir`, `:drawer`,
  `:keywords`, `:tags`, `:body`; `orsb-core-promote-node` takes a folder.

## 2.1 → 2.2: one tool set

2.2 removes everything that offered a second set of tools:

- **The old names are gone.** `orsb-mcp-legacy-tools` no longer exists; the 1.x
  names and the 2.0 tools that 2.1 folded into others are not registered under
  any setting. The tables below still say what each one became, so a client can
  be moved over call by call.
- **Tool profiles are gone.** `orsb-mcp-tool-profile` no longer exists. There
  are nineteen tools and no `full` profile.
- **`sync` and `get_blog_status` are removed.** The database follows the files
  by itself (the idle worker, and `org-roam-db-sync` from Emacs for a forced
  refresh). For a blog overview use `list_nodes {node_type: "blog"}`: each note
  carries its status (`idea`, `stub`, `draft`, `published`).
- **`get_graph` is now `GET /graph`**, a plain HTTP endpoint on the same port
  with the same `{ok, data: {nodes, links}}` answer. It was never a tool for a
  model.
- **Old response shape.** The 1.x names answered `{"success": ...}`. Every
  remaining tool answers `{"ok": ..., "data" | "error"}`; a caller moving off
  `read_note` reads `data.body` where it read `content`, and passes
  `max_chars` when it needs more than 6,000 characters.

## 2.0 → 2.1: small-model tools (historical)

Written when the old names could still be switched on; they cannot since 2.2.

## 2.0 → 2.1: small-model tools

2.1 reshapes the tool set for small local models, which have less context and
pick tools less reliably. The full list is in the README; the changes that can
affect an existing client:

- **Old names are off by default.** `orsb-mcp-legacy-tools` now defaults to
  `nil`. Set it to `t` to keep the 1.x names (below) and these 2.0 tools,
  which 2.1 folded into others:

  | 2.0 tool | 2.1 call |
  |---|---|
  | `add_heading {id, heading}` | `create_node {parent_id: id, title: heading}` (also `section`, `todo`, `scheduled`, `deadline`) |
  | `get_daily {date}` | `get_node {id: "today"}` or `{id: "2026-11-19"}` |
  | `get_projects {status, stale}` | `list_nodes {node_type: "project", status, stale}` (records now carry `next_action`) |
  | `get_followups`, `get_digest` | `get_review` |

- **`sync` and `get_blog_status`** are offered only with
  `(setq orsb-mcp-tool-profile 'full)`. Nothing else changes about them.
- **`update_body` with a `section` that does not exist** is now an error
  listing the headings that do exist. Pass `create_section: true` for the old
  behavior (the legacy `update_note` / `update_node` mappers still create it).
- **`delete_node` archives a note by default.** Pass `archive: false` to delete
  the file. A note already in the archive is deleted. Heading deletes are
  unchanged.
- **`create_node` refuses a title (or alias) that already exists**: it returns
  `created: false` with `possible_duplicates`. Pass `on_duplicate: "create"`
  for the old behavior, or `"use_existing"`. `node_type` now defaults to
  `note`.
- **`link_nodes`** returns `action: "exists"` (and writes nothing) when the
  link is already there; removing a link that sits inside a sentence keeps its
  words instead of deleting the line.
- **`set_node {title}`** also rewrites `[[id:...][Old title]]` and `[[Old title]]`
  link text in other notes. Pass `update_links: false` to skip that.
- **An ambiguous title** passed as `id` is now an `invalid_argument` error
  listing each match's id, instead of picking one.
- **Every write answers with `change_id`**, and most take `dry_run`. `restore`
  undoes a change. Errors carry a specific `hint` where the server knows one.
- **Empty objects** (`properties`, `keywords`) now encode as `{}`; 2.0 sent the
  string `"empty-object"`.

New tools: `find_related`, `set_checkbox`, `get_tasks`, `move_node`,
`merge_nodes`, `resolve_inbox`, `get_review`, `restore`.

## Embeddings move out of the notes

Vectors are no longer written as `:EMBEDDING:` properties. They live in a
SQLite file beside the notes (`orsb-embed-cache-file`, by default
`~/.cache/org-roam-second-brain/embeddings.db`), keyed by node `:ID:` and
embedding model; see "Semantic search" in the README. For an existing vault:

1. `M-x orsb-search-migrate-properties`: copies the in-note vectors into the
   cache without touching a note.
2. Later, opt-in: `M-x orsb-search-strip-properties` (`C-u` for a dry run)
   removes the in-note lines whose vectors the cache holds.

Elisp changes: `orsb-search-cache-file` (the old `.eld` index) is obsolete in
favour of `orsb-embed-cache-file`; `orsb-search-backfill-hashes` is obsolete
and does nothing; `org-roam-semantic-get-similar-data`,
`org-roam-semantic-generate-chunks-for-file` and
`org-roam-semantic-generate-all-chunks` now read and write the cache. The MCP
contract is unchanged.

## 1.x → 2.0

Since 2.0.0 the old names are thin argument mappers over the new tools (the
`my/api-*` implementation and `org-roam-api.el` are gone), so both spellings
always behave the same. Also gone: `deploy.sh` (see SETUP.md §7), the global
key bindings claimed at load time (enable `orsb-mode`), and the synchronous
embedding on every save (now queued for idle time). Elisp entry points kept
as aliases: `sb/*` commands and `sb/hugo-*` variables, `org-roam-mcp-http-start`
/ `-stop` (also `orsb-mcp-start` / `orsb-mcp-stop`), `my/org-roam-change-task-state`.

Version 2.0 replaces the 35 tools that grew up one at a time with 18 orthogonal
ones. Every tool takes the same `id` parameter (an org-roam id, a path relative
to the vault or absolute, or an exact title/alias), returns the same envelope,
and uses one status vocabulary. The old names keep working, marked
`[deprecated → new]` in `tools/list`, while `orsb-mcp-legacy-tools` is non-nil
(the default in 2.0; off by default since 2.1). Set it to `t` to keep them.

## Envelope

```json
{"ok": true,  "data": { ... }}
{"ok": false, "error": {"code": "not_found | invalid_argument | refused | unavailable | internal",
                        "message": "...", "hint": "..."}}
```

Failures also set MCP `isError: true` on the result. JSON-RPC `-32602` is
returned only for a missing required argument, `-32603` only for an unexpected
server error. The legacy tools keep their `{"success": ...}` shape.

## Properties

A node's metadata is its `:PROPERTIES:` drawer: the file-level drawer for a
file node, the heading drawer for a heading node. `set_node {properties}` writes
there (keys upper-cased, `null` deletes), which is what fixed "STATUS never
changes" in 1.x. `#+KEY:` lines are separate: `keywords` on `get_node` /
`set_node`, file nodes only.

## Status

`active | waiting | blocked | someday | done | cancelled`. `stale` is computed
(`days_since_modified >= stale_days` and not finished), never stored.
`M-x orsb-migrate-status` reports the vault's variants
(`completed → done`, `shelved → someday`, `paused → waiting`, ...) and rewrites
them with a prefix argument. `get_schema` returns the live vocabulary.

## Old tool → new call

| Old | New |
|---|---|
| `search_notes {query, node_type}` | `search {query, mode: "title", node_type}` |
| `contextual_search {query, limit}` | `search {query, mode: "contextual", limit}` |
| `semantic_search {query, limit, cutoff}` | `search {query, mode: "semantic", limit, cutoff}` (hits carry a `snippet`, not the full note) |
| `read_note {identifier, section}` | `get_node {id, section}` (`body` holds the text) |
| `read_node {node_id}` | `get_node {id}` |
| `get_note_properties {identifier}` | `get_node {id, include_body: false}` |
| `list_notes {node_type, status, limit, sort_by}` | `list_nodes {...}` (`status: "stale"` → `stale: true`) |
| `get_active_projects` | `get_projects {status: "active"}` |
| `get_stale_projects {days_threshold}` | `get_projects {stale: true, days_threshold}` |
| `create_note {title, properties}` | `create_node {node_type: "note", title, keywords}` |
| `create_project {title, notes, status, next_action}` | `create_node {node_type: "project", title, body, status, next_action}` |
| `create_person {name, context, follow_ups, notes}` | `create_node {node_type: "person", title, context, follow_ups: [...], body}` |
| `create_idea {title, one_liner, elaboration}` | `create_node {node_type: "idea", title, one_liner, body}` |
| `create_admin {title, due_date, notes}` | `create_node {node_type: "admin", title, due_date, body}` |
| `create_blog_post {title, section, body, tags}` | `create_node {node_type: "blog", title, hugo_section, body, tags: [...]}` |
| `add_node {note_id, heading, text, properties, level}` | `add_heading {id, heading, body, properties, level, todo}` |
| `update_node {node_id, properties}` | `set_node {id, properties}` |
| `update_node {node_id, text}` | `update_body {id, content, mode: "replace"}` |
| `update_node {node_id, section, content}` | `update_body {id, section, content, mode: "replace"}` |
| `update_note {identifier, content, section, mode, force}` | `update_body {id, content, section, mode, force}` |
| `manage_tags {identifier, action, tag}` | `set_node {id, tags_add: [..]}` / `{tags_remove: [..]}` |
| `rename_note {identifier, new_title}` | `set_node {id, title}` |
| `change_task_state {file, heading, new_state}` | `set_node {id: <heading id>, todo}` |
| `delete_note {identifier, archive}` | `delete_node {id, archive}` |
| `delete_node {node_id}` | `delete_node {id}` |
| `add_link {from_id, to_id, section}` | `link_nodes {id, target_id, section}` |
| `add_daily_entry` | unchanged |
| `get_daily_content {date}` | `get_daily {date}` |
| `log_to_inbox {text}` | `log_to_inbox {text}` (plus optional `linked_id`, `category`) |
| `add_inbox_entry {command, original_text, linked_note_id}` | `log_to_inbox {text, linked_id, category}` |
| `get_digest_data` / `get_weekly_inbox {days}` | `get_digest {days}` |
| `get_pending_followups` | `get_followups` |
| `get_dangling_followups` | `get_followups {dangling: true}` |
| `blog_status` | `get_blog_status` |
| `sync_database` | `sync` |
| `generate_note_embedding {file_path}` | `sync {id, embeddings: true}` |
| `generate_embeddings` | `sync {full: true, embeddings: true}` (queued for idle time) |

## Response keys that changed

- Everything is under `data`. The `note` object legacy `create_*` returned is
  now the whole `data` (a node record: `id, title, file, level, node_type,
  status, todo, tags, properties, keywords, modified, days_since_modified,
  stale, links_to, links_from`).
- `file` is relative to the vault.
- `properties` keys are upper-case as written in the drawer; `ID` and
  `EMBEDDING*` are never returned.
- Booleans are JSON booleans; empty lists are `[]`, empty objects `{}`.
