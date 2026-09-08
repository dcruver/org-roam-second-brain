# Migrating to the 2.0 MCP contract

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
(the default for one release). Set it to `nil` to drop them.

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
