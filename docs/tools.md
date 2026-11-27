# The MCP server and its tools

```elisp
(setq orsb-mcp-host "127.0.0.1"    ; "0.0.0.0" to serve your LAN
      orsb-mcp-port 8007
      orsb-mcp-auth-token nil)     ; set a string to require Authorization: Bearer
(orsb-mcp-start)
```

Point an MCP client at `http://host:8007/mcp` (Streamable HTTP, POST only; no
SSE stream). The server keeps one session per client, times out a tool after
`orsb-mcp-tool-timeout` seconds, and logs every request to `*orsb-mcp-log*`.

Try it:

```sh
curl -s -X POST http://localhost:8007/mcp -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"get_schema","arguments":{}}}'
```

## Connecting an assistant

Any client that speaks MCP over Streamable HTTP works. With Claude Code:

```sh
claude mcp add --transport http notes http://localhost:8007/mcp
# with a token set:
claude mcp add --transport http notes http://localhost:8007/mcp --header "Authorization: Bearer change-me"
```

For other clients, the URL is `http://<host>:8007/mcp` and the header is
`Authorization: Bearer <token>` when `orsb-mcp-auth-token` is set.

## Tools

Twenty-three tools, one set. A small model gets few, clear choices, and every
error says what to do next.

| Tool | Purpose |
|---|---|
| `search` | semantic (by meaning), contextual (word match) or title search; each hit names the matching section and shows a snippet from it |
| `get_node` | one note or heading: metadata, links, an outline of its sections, and the body (cut at `max_chars`; read one `section` at a time) |
| `list_nodes` | notes filtered by type, status, staleness, tags, `orphan`, `broken_links` or `linked_to`; pages with `limit` and `offset` |
| `find_related` | notes close in meaning to a note or a text, each marked `linked` or not, so missing links stand out |
| `create_node` | a typed note, or with `parent_id` a heading (task, section) inside one, with `todo`, `scheduled`, `deadline`; refuses an existing title and lists it (`on_duplicate`) |
| `set_node` | title (link text elsewhere follows), status, todo, scheduled, deadline, properties, tags, aliases, keywords; `heading` picks a heading by title |
| `update_body` | append / prepend / replace, per section or whole note (guarded); a missing section is an error listing the real ones |
| `set_checkbox` | tick or untick one checklist item, by its text or index |
| `get_tasks` | open TODO headings and checkboxes across the vault, dated first; filter by state, note, mentions, due date, text |
| `link_nodes` | add (idempotent) or remove a link; `both_ways`; removing a link inside a sentence keeps the words |
| `move_node` | refile a heading under another note or heading, or make it a note of its own; links keep working; with `folder`, move a whole note to another folder (and so to another type) |
| `merge_nodes` | fold one note into another as a section; links to it keep working |
| `delete_node` | delete a heading, or archive (default) / delete a note; returns the notes that linked to it |
| `add_daily_entry` | a structured entry in today's (or a given day's) daily note |
| `log_to_inbox`, `resolve_inbox` | add a line to today's inbox and get its ref; later mark it done, optionally filing it into a note |
| `get_review` | one call for what needs attention: unfinished notes of every type with a lifecycle and the stale ones among them, due tasks, open inbox entries, vault health |
| `restore` | undo any write by its `change_id`, or list recent changes |
| `get_schema` | the vocabulary the server enforces: the types the vault defines, TODO keywords, tags in use; with `type`, that type's rules |
| `check_schema` | where existing notes depart from the vault's rules (read-only) |
| `get_proposals`, `decide_proposal` | changes filed for approval: list or show them, approve or reject |
| `reassign_id` | give a note that shares its id with another note an id of its own, so both are visible |

Every tool takes `id` as an org-roam id, a vault-relative path, an exact
title or alias, or `today` / `yesterday` / `YYYY-MM-DD` for a daily note. An
ambiguous title is an error listing the candidates. Answers are
`{"ok":true,"data":{...}}` or `{"ok":false,"error":{"code","message","hint"}}`
(with MCP `isError`). A type's lifecycle value (`status`) is checked against
the values its `_schema.org` allows.

Every write is recorded: the answer carries a `change_id`, and `restore`
with it puts every file the write touched back as it was (a restore is
itself recorded, so it can be undone too). Most writes also take
`dry_run: true`, which answers with what would change and writes nothing.
If a write fails halfway, the files it already changed are put back. The
journal lives in `orsb-change-directory` (default `orsb-changes/` in your
Emacs directory) and keeps the last `orsb-change-keep` (200) changes.

At `initialize` the server sends `orsb-mcp-instructions`, a few lines of
guidance for the model (search before creating, undo with restore, follow
the hints). Set it to your own text, or `nil` to send none.

Options:

```elisp
(setq orsb-duplicate-similarity 0.9)  ; create_node also flags notes close in meaning
                                      ; (one embedding call per create)
```

## The graph, for drawing

`GET /graph` on the same port answers `{"ok":true,"data":{"nodes":[...],"links":[...]}}`:
every node (id, title, vault-relative file, level, pos, tags) and every id link
as a `[source, dest]` pair, straight from the database, no bodies. It is a plain
HTTP endpoint, not an MCP tool, because it is for a program that draws the graph,
not for a model. It honours `orsb-mcp-auth-token` like the rest.

## Coming from an earlier version

[MIGRATION.md](https://github.com/dcruver/org-roam-second-brain/blob/main/MIGRATION.md)
maps the tool names and arguments of 1.x and 2.x onto these.
