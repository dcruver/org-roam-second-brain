# org-roam-second-brain

A second brain on top of [org-roam](https://www.orgroam.com/): typed notes
(projects, people, ideas, admin, blog posts) with conventions an agent can
rely on, semantic search over your vault, a daily digest, and an **MCP server**
so an LLM assistant uses the same notes you do. Everything is Emacs Lisp; the
only external service is an OpenAI-compatible embeddings endpoint (optional).

Version 2.1 has one core and one contract, sized for small local models:

| Module | What it is |
|---|---|
| `orsb-core.el` | Every capability as a plain function: resolve a note by id, path or title; read and write the `:PROPERTIES:` drawer; create typed notes; edit sections with a destructive-replace guard; daily notes, inbox, follow-ups, blog status. |
| `orsb-tools.el` | The 19-tool MCP contract on top of the core: one `id` parameter, one `{ok, data | error}` envelope with a hint on every error, one status vocabulary, and an undo journal behind every write. Older tool names are available behind `orsb-mcp-legacy-tools` (see `MIGRATION.md`). |
| `org-roam-mcp-http.el` | The MCP server (Streamable HTTP, POST): per-client sessions, optional bearer token, tool timeout, request log. |
| `orsb-embed-cache.el` | The embedding store: one SQLite file beside the notes (not in them), one row per node and model. |
| `orsb-search.el` | Semantic search: stale detection by content hash, an in-memory index loaded from the cache, an idle-time worker that re-embeds what changed, and the migration from in-note vectors. |
| `org-roam-vector-search.el` | The embedding primitives and the interactive search commands. |
| `org-roam-second-brain.el` | The human side: `orsb-mode` (`C-c b` and `C-c v`), capture commands, the daily digest, blog helpers. |
| `org-agenda-project-tracking.el` | Optional agenda views and TODO journaling (`orsb-agenda-setup`). |

## Install

Doom Emacs (`packages.el` and `config.el`):

```elisp
(package! org-roam-second-brain
  :recipe (:host github :repo "dcruver/org-roam-second-brain"))

(use-package! org-roam-second-brain
  :after org-roam
  :config
  (orsb-mode 1))
```

straight.el:

```elisp
(straight-use-package
 '(org-roam-second-brain :type git :host github :repo "dcruver/org-roam-second-brain"))
(require 'org-roam-second-brain)
(orsb-mode 1)
```

Requires Emacs 29.1+ (builtin SQLite) and org-roam 2.2+.

## Configure

Everything environment-specific is a defcustom in the `orsb` group:

```elisp
(setq org-roam-directory "~/org-roam")
;; vault layout (defaults shown)
(setq orsb-directories '(("project" . "projects") ("person" . "people") ("idea" . "ideas")
                         ("admin" . "admin") ("blog" . "blog") ("reference" . "reference")
                         ("howto" . "howto") ("note" . "")))
(setq orsb-daily-directory "daily" orsb-archive-directory "archive")
;; status vocabulary and staleness
(setq orsb-status-values '("active" "waiting" "blocked" "someday" "done" "cancelled")
      orsb-stale-days 5)
;; blog posts (ox-hugo); nil refuses blog creation
(setq orsb-hugo-base-dir "~/Projects/my-site/" orsb-hugo-sections '("homelab" "writing"))
;; semantic search (any OpenAI-compatible /embeddings endpoint, e.g. Ollama or Infinity)
(setq org-roam-semantic-embedding-url "http://localhost:11434/v1"
      org-roam-semantic-embedding-model "nomic-embed-text")
```

## The MCP server

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

### Tools

Nineteen tools by default (`orsb-mcp-tool-profile` is `small`). A small model
gets fewer, clearer choices, and every error says what to do next.

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
| `get_tasks` | open TODO headings and checkboxes across the vault, dated first; filter by state, note, person, due date, text |
| `link_nodes` | add (idempotent) or remove a link; `both_ways`; removing a link inside a sentence keeps the words |
| `move_node` | refile a heading under another note or heading, or make it a note of its own; links keep working |
| `merge_nodes` | fold one note into another as a section; links to it keep working |
| `delete_node` | delete a heading, or archive (default) / delete a note; returns the notes that linked to it |
| `add_daily_entry` | a structured entry in today's (or a given day's) daily note |
| `log_to_inbox`, `resolve_inbox` | add a line to today's inbox and get its ref; later mark it done, optionally filing it into a note |
| `get_review` | one call for what needs attention: projects, due tasks, follow-ups, open inbox entries, vault health |
| `restore` | undo any write by its `change_id`, or list recent changes |
| `get_schema` | the vocabulary the server enforces: types, statuses, TODO keywords, tags in use |

Every tool takes `id` as an org-roam id, a vault-relative path, an exact
title or alias, or `today` / `yesterday` / `YYYY-MM-DD` for a daily note. An
ambiguous title is an error listing the candidates. Answers are
`{"ok":true,"data":{...}}` or `{"ok":false,"error":{"code","message","hint"}}`
(with MCP `isError`). Status is validated: `active | waiting | blocked |
someday | done | cancelled` for notes, `idea | stub | draft | published` for
blog posts. `M-x orsb-migrate-status` normalizes older spellings in a vault.

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
(setq orsb-mcp-tool-profile 'full)    ; also offer sync, get_blog_status and get_graph
                                      ; (get_graph: every node and id link, for drawing the graph)
(setq orsb-mcp-legacy-tools t)        ; also offer the 1.x and 2.0 names, marked deprecated
(setq orsb-duplicate-similarity 0.9)  ; create_node also flags notes close in meaning
                                      ; (one embedding call per create)
```

### The graph, for drawing

`GET /graph` on the same port answers `{"ok":true,"data":{"nodes":[...],"links":[...]}}`:
every node (id, title, vault-relative file, level, pos, tags) and every id link
as a `[source, dest]` pair, straight from the database, no bodies. It is a plain
HTTP endpoint, not an MCP tool, because it is for a program that draws the graph,
not for a model. It honours `orsb-mcp-auth-token` like the rest.

## Semantic search

Embeddings live in one SQLite file beside the notes, not in them:
`orsb-embed-cache-file`, by default
`~/.cache/org-roam-second-brain/embeddings.db`. Keep it outside any synced
folder. Each row is one org-roam node (keyed by its `:ID:`) and one embedding
model, with the hash of the text it was made from:

```
node_id       org-roam ID of the heading (or file)
model         embedding model that made it
content_hash  sha1 of the normalized chunk text
dims          vector length
vector        little-endian float32s, packed as a blob
updated_at    when it was embedded (ISO 8601, UTC)
```

Saving a note queues it; an idle-time worker re-embeds only the chunks whose
text changed, and writes nothing to the note except an `:ID:` the first time
a heading is embedded (its key). Queries read an in-memory index loaded from
the cache. Changing `org-roam-semantic-embedding-model` makes every chunk
stale for the new model; old rows keep serving the old model until
`M-x orsb-search-prune-cache` removes them (along with rows for nodes that no
longer exist). The file is a cache: delete it and `sync {"embeddings": true}`
or `M-x orsb-search-generate-all` rebuilds it.

### Moving a vault off in-note embeddings

Vaults from before 2.1 carry `:EMBEDDING:` and `:EMBEDDING_HASH:` lines in
their notes. Two commands, run on the machine that runs the server:

1. `M-x orsb-search-migrate-properties` copies those vectors into the cache.
   It reads the notes and writes nothing to them; running it twice is
   harmless. Vectors in a drawer without an `:ID:` are left for the worker to
   re-embed.
2. When you are happy with search, `M-x orsb-search-strip-properties` removes
   the lines from the notes (`C-u` first for a dry run that only counts). It
   removes a vector only when the cache holds a row for that node, and skips
   notes with unsaved edits. This rewrites each affected note once, so let
   Syncthing settle before editing elsewhere.

Until step 2, other devices still see the old lines; nothing reads them.

## Develop

```sh
make test     # ERT against a throwaway vault (ORSB_DEPS points at your org-roam build)
make compile  # byte-compile everything, warnings shown
make lint     # warnings are errors for the orsb-* modules
make reload   # reload into a running Emacs server and restart the MCP server
make eval     # give a local model everyday vault chores and score them (eval/README.md)
```

CI runs the same on Emacs 29 and 30. See `SETUP.md` for a full
walkthrough and `MIGRATION.md` for the 1.x → 2.0 → 2.1 tool mapping.
